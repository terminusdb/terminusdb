:- module(database,[
              query_context_transaction_objects/2,
              run_transactions/3,
              run_transactions/4,
              retry_transaction/2,
              with_transaction/3,
              with_transaction/4,
              graph_inserts_deletes/3
          ]).

/** <module> Implementation of database graph management
 *
 * This module helps other modules with the representation of databases and
 * their associated graphs by bundling them as objects with some convenience
 * operators and accessors.
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- use_module(core(transaction/descriptor)).
:- use_module(core(transaction/validate)).
:- use_module(core(util)).
:- use_module(core(util/utils)).
:- use_module(core(triple), [xrdf_added/4, xrdf_deleted/4]).
:- use_module(core(plugins)).
:- use_module(core(document/migration)).

:- use_module(config(terminus_config), [max_transaction_retries/1]).

:- use_module(library(lists)).
:- use_module(library(prolog_stack)).
:- use_module(library(plunit)).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).
:- use_module(library(yall)).
:- use_module(library(random)).
:- use_module(library(terminus_store)).
:- use_module(library(aggregate)).

descriptor_database_name(Descriptor, 'terminusdb:///system/data/_system') :-
    system_descriptor{} = Descriptor,
    !.
% NOTE: Do these two make any sense?
descriptor_database_name(Descriptor, ID) :-
    id_descriptor{ instance : ID } = Descriptor.
descriptor_database_name(Descriptor, Label) :-
    label_descriptor{ instance : Label } = Descriptor.
descriptor_database_name(Descriptor, Name) :-
    database_descriptor{ organization_name : _,
                         database_name : Name } = Descriptor,
    !.
descriptor_database_name(Descriptor, Name) :-
    repository_descriptor{ repository_name : _,
                           database_descriptor : DB_Descriptor} = Descriptor,
    !,
    descriptor_database_name(DB_Descriptor, Name).
descriptor_database_name(Descriptor, Name) :-
    branch_descriptor{ branch_name : _,
                       repository_descriptor : Repo_Descriptor } = Descriptor,
    !,
    descriptor_database_name(Repo_Descriptor, Name).

transaction_database_name(Transaction, Name) :-
    descriptor_database_name(Transaction.descriptor, Name).

same_hierarchy(Transaction1, Transaction2) :-
    transaction_database_name(Transaction1,Name),
    transaction_database_name(Transaction2,Name).

partition_by_root(Transaction_Objects, Partitions) :-
    partition_by_root_(Transaction_Objects, [], Partitions).

partition_by_root_([], Partitions, Partitions).
partition_by_root_([Transaction|Tail], Partitions_So_Far, Final) :-
    select(Partition, Partitions_So_Far, [Transaction|Partition], Partitions),
    Partition = [Partition_Transaction|_],
    \+ memberchk(Transaction, Partition),
    same_hierarchy(Transaction, Partition_Transaction),
    !,
    partition_by_root_(Tail,Partitions,Final).
partition_by_root_([Transaction|Tail], Partitions_So_Far, Final) :-
    % A new partition
    partition_by_root_(Tail, [[Transaction]|Partitions_So_Far], Final).

/**
 * multi_transaction(Query_Context) is semidet.
 *
 * Is true if we more than one database - this makes rollback consistency impossible.
 */
multi_transaction(Query_Context) :-
    Transaction_Objects = Query_Context.transaction_objects,
    partition_by_root(Transaction_Objects, Partitions),
    length(Partitions, N),
    N \= 1.


read_write_obj_already_committed(RW_Obj) :-
    read_write_obj{ descriptor: _Descriptor,
                    write: Layer_Builder } :< RW_Obj,
    nonvar(Layer_Builder),
    builder_committed(Layer_Builder).

already_committed(Transaction_Object) :-
    Instance_Objects = Transaction_Object.instance_objects,
    exists(read_write_obj_already_committed,Instance_Objects).
already_committed(Transaction_Object) :-
    Schema_Objects = Transaction_Object.schema_objects,
    exists(read_write_obj_already_committed,Schema_Objects).
already_committed(Transaction_Object) :-
    Inference_Objects = Transaction_Object.inference_objects,
    exists(read_write_obj_already_committed,Inference_Objects).

/*
 * partial_commits(Query_Context) is semidet
 *
 * Is true if we are a partial commit. i.e. bad.
 */
partial_commits(Query_Context) :-
    exists(already_committed, Query_Context.transaction_objects).

slot_size(2).
slot_coefficient(0.5).
slot_time(0.1).

/*
 * compute_backoff(Count, Time) is det.
 *
 * Computes the number of seconds to back off from a transaction
 */
compute_backoff(Count, Time) :-
    slot_size(Slot),
    slot_coefficient(C),
    Slots is floor(C * (Slot ** Count)) + 1,
    random(0,Slots,This_Slot),
    slot_time(Slot_Time),
    Time is This_Slot * Slot_Time.

reset_read_write_obj(Read_Write_Obj, Map, New_Map) :-
    nb_set_dict(read, Read_Write_Obj, _),
    nb_set_dict(write, Read_Write_Obj, _),
    nb_set_dict(backlinks, Read_Write_Obj, []),
    nb_set_dict(triple_update, Read_Write_Obj, false),
    Descriptor = (Read_Write_Obj.descriptor),
    (   get_dict(commit_type, Descriptor, _)
    ->  nb_set_dict(commit_type, Descriptor, _)
    ;   true),

    open_read_write_obj(Read_Write_Obj.descriptor,Read_Write_Obj, Map, New_Map).

reset_transaction_object_graph_descriptors(Transaction_Object, Map, New_Map) :-
    transaction_object{
        instance_objects : Instance_Objects,
        schema_objects : Schema_Objects,
        inference_objects : Inference_Objects
    } :< Transaction_Object,

    (   get_dict(parent,Transaction_Object,Parent_Transaction)
    ->  reset_transaction_object_graph_descriptors(Parent_Transaction, Map, Map1)
    ;   Map1 = Map),

    mapm(reset_read_write_obj, Instance_Objects, Map1, Map2),
    mapm(reset_read_write_obj, Schema_Objects, Map2, Map3),
    mapm(reset_read_write_obj, Inference_Objects, Map3, New_Map).

/**
 *
 */
reset_transaction_objects_graph_descriptors(Transaction_Objects) :-
    mapm(reset_transaction_object_graph_descriptors, Transaction_Objects, [], _).

/**
 * reset_query_context(Query_Context) is semidet.
 *
 * Attempts to re-open a query context.
 */
reset_query_context(Query_Context) :-
    reset_transaction_objects_graph_descriptors(
        Query_Context.transaction_objects
    ).

/**
 * retry_transaction(Query_Context, Recount_Number) is nondet.
 *
 * Retry transaction sets up some number of choice points for
 * retrying a transaction together with a back-off strategy.
 *
 * If it is impossible to back out of a transaction (partial commits
 * in the case of multi-database transactions), we instead through an error and
 * complain.
 *
 * WARNING: This is a side-effecting operation
 */
retry_transaction(Query_Context, Transaction_Retry_Count) :-
    max_transaction_retries(Max_Transaction_Retries),

    between(0, Max_Transaction_Retries, Transaction_Retry_Count),

    % If we are > 0, we have actually failed at least once.
    (   Transaction_Retry_Count > 0
    ->  (   multi_transaction(Query_Context),
            partial_commits(Query_Context)
        ->  throw(error(multi_transaction_error("We are in a multi transaction which has partially commited"),context(retry_transaction/1,Query_Context)))
        ;   true),
        % This is side-effecting!!!
        reset_query_context(Query_Context),
        compute_backoff(Transaction_Retry_Count,BackOff),
        sleep(BackOff)
    ;   true).


pre_transaction_tabling :-
    true.

post_transaction_tabling :-
    abolish_private_tables.

/**
 * with_transaction(+Query_Context, +Body, -Meta_Data) is semidet.
 *
 * Performs a transaction after Body is run.
 *
 * The body is assumed semidet.
 */
:- meta_predicate with_transaction(?,0,?).
with_transaction(Query_Context,Body,Meta_Data) :-
    with_transaction(Query_Context,Body,Meta_Data, []).

:- meta_predicate with_transaction(?,0,?,+).
with_transaction(Query_Context,
                 Body,
                 Meta_Data,
                 Options) :-
    setup_call_cleanup(
        pre_transaction_tabling,
        with_transaction_(Query_Context,Body,Meta_Data,Options),
        post_transaction_tabling % Do some cleanup of schema compilation etc.
    ).

:- meta_predicate with_transaction(?,0,?,+).
with_transaction_(Query_Context,
                  Body,
                  Meta_Data,
                  Options) :-
    retry_transaction(Query_Context, Transaction_Retry_Count),
    (   catch(call(Body),
              fail_transaction,
              Fail_Transaction=true)
    ->  Fail_Transaction = false,
        query_context_transaction_objects(Query_Context, Transactions),
        run_transactions(Transactions,(Query_Context.all_witnesses),Meta_Data0,Options),
        !, % No going back now!
        Meta_Data = (Meta_Data0.put(_{transaction_retry_count : Transaction_Retry_Count}))
    ;   !,
        fail).
with_transaction_(_,
                  _,
                  _,
                  _) :-
    throw(error(transaction_retry_exceeded, _)).

:- use_module(core(util/test_utils)).
:- use_module(library(http/json)).
/*
 * run_transactions(Transaction, All_Witnesses, Meta_Data) is det.
 *
 * Run all transactions and throw errors with witnesses.
 */
run_transactions(Transactions, All_Witnesses, Meta_Data) :-
    run_transactions(Transactions, All_Witnesses, Meta_Data,[]).

/*

Options include
* inside_migration: whether we ourselves are a migration transaction
* require_migration: whether we should throw an error if no migration can be established
* allow_destructive_migration: whether we should allow strengthening migrations

 */
run_transactions(Transactions, All_Witnesses, Meta_Data, Options) :-
    transaction_objects_to_validation_objects(Transactions, Validations),
    (   option(inside_migration(true), Options)
    ->  Validations=Validations0,
        Inference_Metadata=_{}
    ;   infer_migrations_for_commit(Validations,Validations0,Inference_Metadata,Options)
    ),
    validate_validation_objects(Validations0, All_Witnesses, Witnesses),

    (   Witnesses = []
    ->  true
    ;   throw(error(schema_check_failure(Witnesses),_))),

    findall(Witness,
            pre_commit_hook(Validations0, Witness),
            Hook_Witnesses),
    (   Hook_Witnesses = []
    ->  true
    ;   throw(error(schema_check_failure(Hook_Witnesses),_))),

    commit_validation_objects(Validations0, Committed),
    collect_validations_metadata(Validations0, Validation_Meta_Data),
    collect_commit_metadata(Committed, Commit_Meta_Data),
    put_dict(Validation_Meta_Data, Commit_Meta_Data, Meta_Data0),
    put_dict(Inference_Metadata, Meta_Data0, Meta_Data),
    ignore(forall(post_commit_hook(Validations0, Meta_Data), true)).

no_schema_changes_for_validation(Validation) :-
    member(Schema_Object,(Validation.schema_objects)),
    (Schema_Object.changed) = false.

no_schema_changes(Validations) :-
    forall(
        member(Validation, Validations),
        no_schema_changes_for_validation(Validation)
    ).

infer_migrations_for_commit(Validations,Validations,_{},_Options) :-
    no_schema_changes(Validations),
    !.
infer_migrations_for_commit(Validations0,Validations1,Meta_Data,Options) :-
    (   option(require_migration(true), Options)
    ->  do_or_die(
            (   option(allow_destructive_migration(true), Options)
            ->  infer_arbitrary_migration(Validations0, Validations1, Meta_Data)
            ;   infer_weakening_migration(Validations0, Validations1, Meta_Data)),
            error(no_inferrable_migration, _))
    ;   option(allow_destructive_migration(true), Options)
    ->  infer_arbitrary_migration(Validations0, Validations1, Meta_Data)
    ;   infer_weakening_migration(Validations0, Validations1, Meta_Data)
    ->  true
    ;   Validations0 = Validations1,
        Meta_Data = _{}
    ).

/* Note: This should not exist */
graph_inserts_deletes(Graph, I, D) :-
    graph_validation_obj{ changed: Value } :< Graph,
    (   ground(Value),
        Value = true
    ;   var(Value)),
    !,
    layer_addition_count(Graph.read, I),
    layer_removal_count(Graph.read, D).
graph_inserts_deletes(_Graph, 0, 0).

validation_inserts_deletes(Validation, Inserts, Deletes) :-
    % only count if we are in a branch or terminus commit
    validation_object{
        descriptor : Descriptor,
        instance_objects : Instance_Objects,
        schema_objects : Schema_Objects,
        inference_objects : Inference_Objects
    } :< Validation,
    (   Descriptor = branch_descriptor{ branch_name : _,
                                        repository_descriptor : _}
    ->  true
    ;   Descriptor = system_descriptor{}),
    !,
    foldl([Graph,(I0,D0),(I1,D1)]>>(
              graph_inserts_deletes(Graph, I, D),
              I1 is I0 + I,
              D1 is D0 + D
          ),
          Instance_Objects, (0,0), (Insert_0, Delete_0)),
    foldl([Graph,(I0,D0),(I1,D1)]>>(
              graph_inserts_deletes(Graph, I, D),
              I1 is I0 + I,
              D1 is D0 + D
          ),
          Inference_Objects, (Insert_0,Delete_0), (Insert_1, Delete_1)),
    foldl([Graph,(I0,D0),(I1,D1)]>>(
              graph_inserts_deletes(Graph, I, D),
              I1 is I0 + I,
              D1 is D0 + D
          ),
          Schema_Objects, (Insert_1,Delete_1), (Inserts, Deletes)).
validation_inserts_deletes(_Validation, 0, 0).

/*
 * collect_validations_metadata(Validations, Meta_Data) is det.
 */
collect_validations_metadata(Validations, Meta_Data) :-
    foldl([Validation,R0,R1]>>(
              validation_inserts_deletes(Validation, Inserts, Deletes),
              get_dict(inserts,R0,Current_Inserts),
              New_Inserts is Current_Inserts + Inserts,
              get_dict(deletes,R0,Current_Deletes),
              New_Deletes is Current_Deletes + Deletes,
              put_dict(meta_data{inserts : New_Inserts,
                                 deletes : New_Deletes
                                }, R0, R1)
          ),
          Validations,
          meta_data{
              inserts : 0,
              deletes : 0
          },
          Meta_Data).

collect_commit_metadata(Validations, Meta_Data) :-
    convlist({Validations}/[Validation, Descriptor-Data_Version]>>(
                validation_data_version(Validation, Validations, Data_Version),
                get_dict(descriptor, Validation, Descriptor)
             ),
             Validations,
             Pairs),
    Meta_Data = meta_data{data_versions : Pairs}.

/*
 * query_context_transaction_objects(+Query_Object,Transaction_Objects) is det.
 *
 * Marshall commit info into the transaction object and run it.
 */
query_context_transaction_objects(Query_Context,Transaction_Objects) :-
    maplist({Query_Context}/[Transaction_Object,New_Transaction_Object]>>(
                (   branch_descriptor{} :< Transaction_Object.descriptor
                ->  get_dict(commit_info,Query_Context,Commit_Info),
                    put_dict(_{commit_info : Commit_Info},
                             Transaction_Object,
                             New_Transaction_Object)
                ;   New_Transaction_Object = Transaction_Object)
            ),
            Query_Context.transaction_objects,
            Transaction_Objects).

:- begin_tests(database_transactions).
:- use_module(core(util/test_utils)).
:- use_module(core(triple)).
:- use_module(core(api)).
:- use_module(core(query)).
:- use_module(library(terminus_store)).

test(test_transaction_partition, [
         setup((setup_temp_store(State),
                create_db_without_schema(admin,testdb1),
                create_db_without_schema(admin,testdb2))),
         cleanup(teardown_temp_store(State))
     ])
:-

    make_branch_descriptor(admin,testdb1,Desc1),
    make_branch_descriptor(admin,testdb2,Desc2),
    open_descriptor(Desc1, Trans1),
    open_descriptor(Desc2, Trans2),
    Transactions = [Trans1,Trans2],
    multi_transaction(query_context{transaction_objects : Transactions}).


test(partial_transaction_commit, [
         setup((setup_temp_store(State),
                create_db_without_schema(admin,testdb1),
                create_db_without_schema(admin,testdb2))),
         cleanup(teardown_temp_store(State))
     ])
:-

    make_branch_descriptor(admin,testdb1,Desc1),
    make_branch_descriptor(admin,testdb2,Desc2),
    create_context(Desc1, Context1),

    open_descriptor(Desc2, Pre_Transaction2),

    Transaction2 = Pre_Transaction2.put(_{commit_info : commit_info{ author : test,
                                                                     message : test}
                                         }),


    Transaction1_List = Context1.transaction_objects,

    Transactions = [Transaction2|Transaction1_List],

    Context = Context1.put(_{ transaction_objects : Transactions,
                              commit_info : commit_info{ author : test,
                                                         message : test}
                            }),

    ask(Context,
        insert(a, b, c)),

    query_context_transaction_objects(Context,Transaction_Objects),
    run_transactions(Transaction_Objects, true, _),

    partial_commits(Context).

:- end_tests(database_transactions).
