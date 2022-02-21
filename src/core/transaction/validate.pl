:- module(validate, [
              transaction_objects_to_validation_objects/2,
              validation_objects_to_transaction_objects/2,
              commit_validation_object/2,
              commit_validation_objects/2,
              commit_commit_validation_object/4,
              validate_validation_objects/2,
              validate_validation_objects/3,
              read_write_obj_to_graph_validation_obj/4,
              validation_object_changed/1,
              validation_object_has_layer/1
          ]).

/** <module> Validation
 *
 * Implements schema and instance validation
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- use_module(descriptor).
:- use_module(database).
:- use_module(layer_entity).
:- use_module(repo_entity).
:- use_module(ref_entity).

:- reexport(core(util/syntax)).
:- use_module(core(util)).
:- use_module(core(triple)).
:- use_module(core(query)).
:- use_module(core(document), [
                  refute_validation_objects/2,
                  delete_document/2
              ]).

:- use_module(library(semweb/turtle)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(sort)).
:- use_module(library(plunit)).

:- use_module(library(terminus_store)).

/*
 * graph_validation_obj { descriptor: graph_descriptor, read: layer, changed: bool }
 *
 * validation_object{ descriptor: collection_descriptor,
 *                    <parent>: transaction_obj % we still keep it a transaction obj and transform it later
 *                    instance_objects: list(graph_validation_obj)
 *                    schema_objects: list(graph_validation_obj)
 *                    inference_objects: list(graph_validation_obj)
 */

read_write_obj_to_graph_validation_obj(Read_Write_Obj, Graph_Validation_Obj, Map, Map) :-
    memberchk(Read_Write_Obj=Graph_Validation_Obj, Map),
    !.
read_write_obj_to_graph_validation_obj(Read_Write_Obj, Graph_Validation_Obj, Map, [Read_Write_Obj=Graph_Validation_Obj|Map]) :-
    Read_Write_Obj = read_write_obj{ descriptor: Descriptor,
                                     read: Layer,
                                     write: Layer_Builder },
    Graph_Validation_Obj = graph_validation_obj{ descriptor: Descriptor,
                                                 read: New_Layer,
                                                 changed: Changed },

    (   var(Layer_Builder)
    ->  New_Layer = Layer,
        Changed = false
    %   NOTE: This seems wasteful - we should ignore this layer not commit it if it is empty.
    ;   nb_commit(Layer_Builder, New_Layer),
        graph_inserts_deletes(Graph_Validation_Obj, N, M),
        \+ (N = 0, M = 0)
    ->  Changed = true
    ;   Changed = false,
        New_Layer = Layer ).

graph_validation_obj_to_read_write_obj(Graph_Validation_Obj, Read_Write_Obj, Map, Map) :-
    memberchk(Graph_Validation_Obj=Read_Write_Obj, Map),
    !.
graph_validation_obj_to_read_write_obj(Graph_Validation_Obj, Read_Write_Obj, Map, [Graph_Validation_Obj=Read_Write_Obj|Map]) :-
    Graph_Validation_Obj = graph_validation_obj{ descriptor: Descriptor,
                                                 read: Layer,
                                                 changed: _Changed },
    Read_Write_Obj = read_write_obj{ descriptor: Descriptor,
                                     read: Layer,
                                     write: _Layer_Builder }.

transaction_object_to_validation_object(Transaction_Object, Validation_Object, Map, New_Map) :-
    transaction_object{descriptor: Descriptor,
                       instance_objects: Instance_Objects,
                       schema_objects: Schema_Objects,
                       inference_objects: Inference_Objects} :< Transaction_Object,
    Intermediate_Validation_Object = validation_object{
                                         descriptor: Descriptor,
                                         instance_objects: Validation_Instance_Objects,
                                         schema_objects: Validation_Schema_Objects,
                                         inference_objects: Validation_Inference_Objects
                                     },
    (   Parent = Transaction_Object.get(parent)
    ->  Intermediate_Validation_Object_1 = Intermediate_Validation_Object.put(parent, Parent)
    ;   Intermediate_Validation_Object_1 = Intermediate_Validation_Object
    ),
    mapm([Object,Validation_Object]>>read_write_obj_to_graph_validation_obj(Object, Validation_Object),
         Instance_Objects,
         Validation_Instance_Objects,
         Map,
         Map_2),
    mapm([Object,Validation_Object]>>read_write_obj_to_graph_validation_obj(Object, Validation_Object),
         Schema_Objects,
         Validation_Schema_Objects,
         Map_2,
         Map_3),
    mapm([Object,Validation_Object]>>read_write_obj_to_graph_validation_obj(Object, Validation_Object),
         Inference_Objects,
         Validation_Inference_Objects,
         Map_3,
         New_Map),
    (   Commit_Info = Transaction_Object.get(commit_info)
    ->  Validation_Object = Intermediate_Validation_Object_1.put(commit_info, Commit_Info)
    ;   Validation_Object = Intermediate_Validation_Object_1).

validation_object_to_transaction_object(Validation_Object, Transaction_Object, Map, New_Map) :-
    validation_object{
        descriptor: Descriptor,
        instance_objects: Validation_Instance_Objects,
        schema_objects: Validation_Schema_Objects,
        inference_objects: Validation_Inference_Objects
    } :< Validation_Object,
    Intermediate_Transaction_Object = transaction_object{descriptor: Descriptor,
                                                         instance_objects: Instance_Objects,
                                                         schema_objects: Schema_Objects,
                                                         inference_objects: Inference_Objects},
    (   Parent = Validation_Object.get(parent)
    ->  Intermediate_Transaction_Object_1 = Intermediate_Transaction_Object.put(parent, Parent)
    ;   Intermediate_Transaction_Object_1 = Intermediate_Transaction_Object
    ),
    mapm([Validation_Obj,Read_Write_Obj]>>graph_validation_obj_to_read_write_obj(Validation_Obj, Read_Write_Obj),
         Validation_Instance_Objects,
         Instance_Objects,
         Map,
         Map_2),
    mapm([Validation_Obj,Read_Write_Obj]>>graph_validation_obj_to_read_write_obj(Validation_Obj, Read_Write_Obj),
         Validation_Schema_Objects,
         Schema_Objects,
         Map_2,
         Map_3),
    mapm([Validation_Obj,Read_Write_Obj]>>graph_validation_obj_to_read_write_obj(Validation_Obj, Read_Write_Obj),
         Validation_Inference_Objects,
         Inference_Objects,
         Map_3,
         New_Map),
    (   Commit_Info = Validation_Object.get(commit_info)
    ->  Transaction_Object = Intermediate_Transaction_Object_1.put(commit_info, Commit_Info)
    ;   Transaction_Object = Intermediate_Transaction_Object_1).

commit_validation_object(Validation_Object, []) :-
    validation_object{
        descriptor : Descriptor,
        instance_objects : Instance_Objects,
        schema_objects : Schema_Objects,
        inference_objects : Inference_Objects
    } :< Validation_Object,
    commit_descriptor{
    } :< Descriptor,

    append([Instance_Objects,Schema_Objects,Inference_Objects], Objects),
    (   exists(validation_object_changed, Objects)
    ->  throw(error(immutable_graph_update(Descriptor),_))
    ;   true).
commit_validation_object(Validation_Object, []) :-
    validation_object{
        descriptor: Descriptor,
        instance_objects: [Instance_Object],
        schema_objects: [Schema_Object],
        inference_objects: []
    } :< Validation_Object,
    system_descriptor{
    } = Descriptor,
    !,
    system_instance_name(Instance_Label),
    system_schema_name(Schema_Label),
    storage(Store),
    (   validation_object_changed(Instance_Object)
    ->  safe_open_named_graph(Store, Instance_Label, Instance_Graph),
        nb_set_head(Instance_Graph, Instance_Object.read)
    ;   true),
    (   validation_object_changed(Schema_Object)
    ->  safe_open_named_graph(Store, Schema_Label, Schema_Graph),
        nb_set_head(Schema_Graph, Schema_Object.read)
    ;   true).
commit_validation_object(Validation_Object, []) :-
    validation_object{
        descriptor: Descriptor,
        instance_objects: [Instance_Object],
        schema_objects : [Schema_Object]
    } :< Validation_Object,
    id_descriptor{
    } :< Descriptor,
    !,

    (   validation_object_changed(Instance_Object)
    ->  throw(error(immutable_graph_update(Descriptor),_))
    ;   true),
    (   validation_object_changed(Schema_Object)
    ->  throw(error(immutable_graph_update(Descriptor),_))
    ;   true).
commit_validation_object(Validation_Object, []) :-
    validation_object{
        descriptor: Descriptor,
        instance_objects: [Instance_Object],
        schema_objects: [Schema_Object]
    } :< Validation_Object,
    label_descriptor{
        instance: Instance_Label,
        schema: Schema_Label
    } :< Descriptor,
    !,
    % super simple case, we just need to set head
    % That is, assuming anything changed
    (   validation_object_changed(Instance_Object)
    ->  storage(Store),
        safe_open_named_graph(Store, Instance_Label, Instance_Graph),
        nb_set_head(Instance_Graph, Instance_Object.read)
    ;   true),
    (   validation_object_changed(Schema_Object)
    ->  storage(Store),
        safe_open_named_graph(Store, Schema_Label, Schema_Graph),
        nb_set_head(Schema_Graph, Schema_Object.read)
    ;   true).
commit_validation_object(Validation_Object, []) :-
    validation_object{
        descriptor: Descriptor,
        instance_objects: [Instance_Object],
        schema_objects: [Schema_Object]
    } :< Validation_Object,
    label_descriptor{
        instance: Instance_Label,
        variety: _Variety
    } :< Descriptor,
    !,
    do_or_die(\+ validation_object_changed(Schema_Object),
              error(immutable_schema_changed, _)),
    % super simple case, we just need to set head
    % That is, assuming anything changed
    (   validation_object_changed(Instance_Object)
    ->  storage(Store),
        safe_open_named_graph(Store, Instance_Label, Instance_Graph),
        nb_set_head(Instance_Graph, Instance_Object.read)
    ;   true).
commit_validation_object(Validation_Object, []) :-
    validation_object{
        descriptor: Descriptor,
        instance_objects: [Instance_Object]
    } :< Validation_Object,
    database_descriptor{
        organization_name: Organization_Name,
        database_name: Database_Name
    } = Descriptor,
    !,
    % super simple case, we just need to set head
    % That is, we don't need to check the schema
    (   validation_object_changed(Instance_Object)
    ->  storage(Store),
        % The label is the same as the same as the database_name
        % therefore we can just open it
        organization_database_name(Organization_Name, Database_Name, Composite),
        safe_open_named_graph(Store, Composite, Graph),
        nb_set_head(Graph, Instance_Object.read)
    ;   true).
commit_validation_object(Validation_Object, [Parent_Transaction]) :-
    validation_object{
        descriptor: Descriptor,
        instance_objects: [Instance_Object],
        parent: Parent_Transaction
    } :< Validation_Object,
    repository_descriptor{
        database_descriptor: _Repository_Descriptor,
        repository_name: Repo_Name
    } = Descriptor,
    !,
    layer_to_id(Instance_Object.read, Layer_ID),
    (   validation_object_changed(Instance_Object)
    ->  update_repository_head(Parent_Transaction, Repo_Name, Layer_ID)
    ;   true).
commit_validation_object(Validation_Object, [Parent_Transaction]) :-
    validation_object{
        parent : Parent_Transaction,
        descriptor: Descriptor,
        instance_objects: [Instance_Object],
        schema_objects: [Schema_Object]
    } :< Validation_Object,

    branch_descriptor{branch_name : Branch_Name} :< Descriptor,
    !,

    (   exists(validation_object_changed, [Instance_Object, Schema_Object])
    ->  (   branch_head_commit(Parent_Transaction, Branch_Name, Old_Commit_Uri),
            commit_type(Parent_Transaction, Old_Commit_Uri, Commit_Type),
            Commit_Type = 'http://terminusdb.com/schema/ref#InitialCommit'
        ->  replace_initial_commit_on_branch(Parent_Transaction,
                                             (Validation_Object.commit_info),
                                             Branch_Name,
                                             Instance_Object,
                                             Schema_Object)
        ;   insert_commit_object_on_branch(Parent_Transaction,
                                           Validation_Object.commit_info,
                                           Branch_Name,
                                           _Commit_Id,
                                           Commit_Uri),
            attach_schema_instance_to_commit(Parent_Transaction, Commit_Uri, Schema_Object, Instance_Object))

    ;   true).

attach_schema_instance_to_commit(Parent_Transaction, Commit_Uri, Schema_Object, Instance_Object) :-
    % instance graph may not exist, so check for that
    (   var(Instance_Object.read)
    ->  true
    ;   layer_to_id(Instance_Object.read, Instance_Layer_Id),
        insert_layer_object(Parent_Transaction, Instance_Layer_Id, Instance_Layer_Uri),
        attach_layer_to_commit(Parent_Transaction, Commit_Uri, instance, Instance_Layer_Uri)),

    layer_to_id(Schema_Object.read, Schema_Layer_Id),
    insert_layer_object(Parent_Transaction, Schema_Layer_Id, Schema_Layer_Uri),
    attach_layer_to_commit(Parent_Transaction, Commit_Uri, schema, Schema_Layer_Uri).

replace_initial_commit_on_branch(Parent_Transaction, Commit_Info, Branch_Name, Instance_Object, Schema_Object) :-
    % delete_document(Parent_Transaction, Commit_Uri),
    insert_base_commit_object(Parent_Transaction, Commit_Info, _, New_Commit_Uri),
    Schema_Layer = (Schema_Object.read),
    (   parent(Schema_Layer, _)
    ->  squash(Schema_Layer, Final_Schema_Layer),
        Final_Schema_Object = (Schema_Object.put(read, Final_Schema_Layer))
    ;   Final_Schema_Object = Schema_Object),

    attach_schema_instance_to_commit(Parent_Transaction, New_Commit_Uri, Final_Schema_Object, Instance_Object),

    branch_name_uri(Parent_Transaction, Branch_Name, Branch_Uri),
    reset_branch_head(Parent_Transaction, Branch_Uri, New_Commit_Uri).

commit_commit_validation_object(Commit_Validation_Object, [Parent_Transaction], New_Commit_Id, New_Commit_Uri) :-
    validation_object{
        parent : Parent_Transaction,
        descriptor: Descriptor,
        instance_objects: [Instance_Object],
        schema_objects: [Schema_Object]
    } :< Commit_Validation_Object,

    commit_descriptor{commit_id : Commit_Id} :< Descriptor,
    !,
    commit_id_uri(Parent_Transaction, Commit_Id, Commit_Uri),

    (   exists(validation_object_changed, [Instance_Object, Schema_Object])
    ->  insert_child_commit_object(Parent_Transaction,
                                   Commit_Uri,
                                   (Commit_Validation_Object.commit_info),
                                   New_Commit_Id,
                                   New_Commit_Uri),

        (   var(Instance_Object.read)
        ->  true
        ;   layer_to_id(Instance_Object.read, Instance_Layer_Id),
            insert_layer_object(Parent_Transaction, Instance_Layer_Id, Instance_Layer_Uri),
            attach_layer_to_commit(Parent_Transaction, Commit_Uri, instance, Instance_Layer_Uri)),

        layer_to_id(Schema_Object.read, Schema_Layer_Id),
        insert_layer_object(Parent_Transaction, Schema_Layer_Id, Schema_Layer_Uri),
        attach_layer_to_commit(Parent_Transaction, Commit_Uri, schema, Schema_Layer_Uri)
    ;   true).

validation_object_changed(Validation_Object) :-
    Validation_Object.changed = true.

validation_object_has_layer(Validation_Object) :-
    ground(Validation_Object.read).

descriptor_type_order_list([commit_descriptor, branch_descriptor, repository_descriptor, database_descriptor, label_descriptor, system_descriptor]).

:- table descriptor_type_order/3.
descriptor_type_order(Operator, Left, Right) :-
    descriptor_type_order_list(Order),
    once(nth0(Left_N, Order, Left)),
    once(nth0(Right_N, Order, Right)),
    compare(Operator, Left_N, Right_N).

descriptor_order(Operator, Left, Right) :-
    Left_Label{} :< Left,
    Right_Label{} :< Right,
    descriptor_type_order(Descriptor_Op, Left_Label, Right_Label),
    (   Descriptor_Op = (=)
    ->  compare(Operator, Left, Right)
    ;   Operator = Descriptor_Op).

transaction_validation_order((=), transaction_object, transaction_object).
transaction_validation_order((<), transaction_object, validation_object).
transaction_validation_order((>), validation_object, transaction_object).
transaction_validation_order((=), validation_object, validation_object).

commit_order(Op, Left, Right) :-
    descriptor_order(Descriptor_Op, Left.descriptor, Right.descriptor),
    (   Descriptor_Op = (=)
    ->  Left_Label{} :< Left,
        Right_Label{} :< Right,
        transaction_validation_order(Op, Left_Label, Right_Label)
    ;   Op = Descriptor_Op).

commit_validation_objects_([], []) :- !.
commit_validation_objects_([Object|Objects], Committed) :-
    transaction_object{} :< Object,
    !,
    Descriptor = (Object.descriptor),
    (   member(Validation_Object,Objects),
        validation_object{} :< Validation_Object,
        (Validation_Object.descriptor) = Descriptor
    ->  commit_validation_objects_(Objects, Committed)
    ;   transaction_objects_to_validation_objects([Object], Validation_Objects),

        validate_validation_objects(Validation_Objects, Witnesses),
        (   Witnesses \= []
        ->  Descriptor = (Object.descriptor),
            throw(error(meta_schema_validation_error(Descriptor,Witnesses),_))
        ;   true),
        append(Validation_Objects, Objects, New_Objects),
        predsort(commit_order, New_Objects, Sorted_Objects),
        commit_validation_objects_(Sorted_Objects, Committed)).
commit_validation_objects_([Object|Objects], [Object|Committed]) :-
    % we know it is a validation object
    commit_validation_object(Object, Transaction_Objects),
    append(Transaction_Objects, Objects, Unsorted_Objects),
    predsort(commit_order,Unsorted_Objects, Sorted_Objects),
    commit_validation_objects_(Sorted_Objects, Committed).

commit_validation_objects(Unsorted_Objects, Committed) :-
    % NOTE: We need to check to make sure we do not simultaneously
    % modify a parent and child of the same transaction object
    % - this could cause commit to fail when we attempt to make the
    % neccessary changes to the parent transaction object required
    % of a commit (adding commit information and repo change info for
    % instance).
    predsort(commit_order,Unsorted_Objects, Sorted_Objects),
    commit_validation_objects_(Sorted_Objects, Committed),
    maplist({Committed}/[O]>>(
                should_retain_layers_for_descriptor(O.descriptor)
            ->  layers_for_validation(O, Committed, Layers),
                retain_descriptor_layers(O.descriptor, Layers)
            ;   true
            ),
            Sorted_Objects),
    log_commits(Sorted_Objects).

layers_for_validation(Validation, Committed, Layers) :-
    _{
        instance_objects: [Instance_RWO],
        schema_objects: [Schema_RWO]
    } :< Validation,
    Instance_Layer = (Instance_RWO.read),
    Schema_Layer = (Schema_RWO.read),
    (   var(Instance_Layer)
    ->  Our_Layers = [Schema_Layer]
    ;   Our_Layers = [Instance_Layer, Schema_Layer]),

    (   get_dict(parent, Validation, Parent)
    ->  append(Our_Layers, Remainder, Layers),
        Descriptor = (Parent.Descriptor),
        include({Descriptor}/[P]>>(
                    Descriptor == (P.Descriptor)
                ),
                Committed,
                [Parent_Validation]),
        layers_for_validation(Parent_Validation, Committed, Remainder)
    ;   Layers = Our_Layers).

log_commits(_) :-
    % Skip logging work if debug log is not enabled
    \+ debug_log_enabled,
    !,
    true.
log_commits([]).
log_commits([First|Rest]) :-
    Descriptor = (First.descriptor),
    (   resolve_absolute_string_descriptor(S, Descriptor)
    ->  format(string(Message), "commit descriptor: ~w", [S])
    ;   Message = "commit unprintable descriptor"),
    descriptor_to_loggable(Descriptor, Loggable),
    json_log_debug(_{
                       message: Message,
                       descriptorAction: commit,
                       descriptor: Loggable
                   }),

    log_commits(Rest).

validate_validation_objects(Validation_Objects, Witnesses) :-
    validate_validation_objects(Validation_Objects, true, Witnesses).

validate_validation_objects_(true, Validation_Objects, Witnesses) :-
    findall(Witness,
            refute_validation_objects(Validation_Objects, Witness),
            Witnesses).
validate_validation_objects_(false, Validation_Objects, Witnesses) :-
    (   refute_validation_objects(Validation_Objects, Witness)
    ->  Witnesses = [Witness]
    ;   Witnesses = []).

validate_validation_objects(Validation_Objects, All_Witnesses, Witnesses) :-
    validate_validation_objects_(All_Witnesses, Validation_Objects, Witnesses).

transaction_objects_to_validation_objects(Transaction_Objects, Validation_Objects) :-
    mapm(transaction_object_to_validation_object, Transaction_Objects, Validation_Objects, [], _Map).

validation_objects_to_transaction_objects(Validation_Objects, Transaction_Objects) :-
    mapm(validation_object_to_transaction_object, Validation_Objects, Transaction_Objects, [], _Map).


:- begin_tests(inserts).
:- use_module(core(util/test_utils)).
:- use_module(core(api)).
:- use_module(core(transaction)).
:- use_module(library(terminus_store)).

test(commit_two_transactions_on_empty, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ])
:-
    create_db_without_schema("admin", 'Boo'),
    resolve_absolute_string_descriptor("admin/Boo", Branch_Descriptor),

    create_context(Branch_Descriptor, _{author : me, message: "hello"}, Context1),
    create_context(Branch_Descriptor, _{author : me, message: "hello"}, Context2),

    ask(Context1,
        insert(a,b,c)),
    query_context_transaction_objects(Context1, Transactions1),

    ask(Context2,
        insert(e,f,g)),
    query_context_transaction_objects(Context2, Transactions2),

    run_transactions(Transactions1,(Context1.all_witnesses),_),

    retry_transaction(Context2, _),

    ask(Context2,
        insert(e,f,g)),

    run_transactions(Transactions2,(Context2.all_witnesses),_),

    !,

    findall(t(X,Y,Z),
            ask(Branch_Descriptor,
                t(X, Y, Z)),
            Triples),

    Triples = [t(a,b,c),
               t(e,f,g)].

test(commit_two_transactions_on_existing, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ])
:-
    create_db_without_schema("admin", 'Boo'),
    resolve_absolute_string_descriptor("admin/Boo", Branch_Descriptor),


    create_context(Branch_Descriptor, _{author : me, message: "hello"}, Context),
    with_transaction(
        Context,
        ask(Context,
            insert(asdf,fdsa,baz)),
        _),

    create_context(Branch_Descriptor, _{author : me, message: "hello"}, Context1),
    create_context(Branch_Descriptor, _{author : me, message: "hello"}, Context2),

    ask(Context1,
        insert(a,b,c)),
    query_context_transaction_objects(Context1, Transactions1),

    ask(Context2,
        insert(e,f,g)),
    query_context_transaction_objects(Context2, Transactions2),

    run_transactions(Transactions1,(Context1.all_witnesses),_),

    retry_transaction(Context2, _),

    ask(Context2,
        insert(e,f,g)),

    run_transactions(Transactions2,(Context2.all_witnesses),_),
    !,

    findall(t(X,Y,Z),
            ask(Branch_Descriptor,
                t(X, Y, Z)),
            Triples),

    Triples = [t(asdf,fdsa,baz),
               t(a,b,c),
               t(e,f,g)].

test(insert_on_branch_descriptor, [
         setup(setup_temp_store(State)),
         all( t(X, Y, Z) == [t(asdf,fdsa,baz)]),
         cleanup(teardown_temp_store(State))
     ])
:-
    create_db_without_schema("admin", 'Boo'),
    % Insert
    DB_Descriptor = database_descriptor{ organization_name: "admin",
                                         database_name : "Boo" },
    Repo_Descriptor = repository_descriptor{ database_descriptor : DB_Descriptor,
                                             repository_name : "local" },
    Branch_Descriptor = branch_descriptor{ repository_descriptor: Repo_Descriptor,
                                           branch_name: "main" },
    open_descriptor(Branch_Descriptor, Transaction),
    Transaction2 = Transaction.put(commit_info, commit_info{ author : "Me", message: "chill"}),
    ask(Transaction2,
        insert(asdf,fdsa,baz)),

    transaction_objects_to_validation_objects([Transaction2], Validation),
    validate_validation_objects(Validation,Witnesses),
    Witnesses = [],
    commit_validation_objects(Validation, _),

    ask(Branch_Descriptor,
        t(X, Y, Z)).

test(insert_on_label_descriptor, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ])
:-
    triple_store(Store),
    safe_create_named_graph(Store, testdb_instance, _Graph1),
    safe_create_named_graph(Store, testdb_schema, _Graph2),

    Descriptor = label_descriptor{ instance: "testdb_instance",
                                   schema: "testdb_schema",
                                   variety: system_descriptor },
    create_context(Descriptor, _{author: "", message: ""}, Context),

    once(ask(Context,insert(foo,bar,baz))),
    Transactions = (Context.transaction_objects),
    transaction_objects_to_validation_objects(Transactions, Validation),
    commit_validation_objects(Validation, _),

    once(ask(Descriptor, t(foo,bar,baz))).

test(insert_schema_system_descriptor, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ])
:-
    create_context(system_descriptor{}, Context),

    with_transaction(Context,
                     once(ask(Context, insert(foo,bar,baz,schema))),
                     Meta_Data),

    Meta_Data.inserts = 1,

    once(ask(system_descriptor{},
             t(foo,bar,baz,schema))).


test(double_insert, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ])
:-
    create_context(system_descriptor{}, Context),

    with_transaction(Context,
                     once(ask(Context, insert(foo,bar,baz,schema))),
                     Meta_Data),


    open_descriptor(system_descriptor{}, Trans),
    [Schema] = Trans.schema_objects,
    layer_to_id(Schema.read, ID1),

    Meta_Data.inserts = 1,
    create_context(system_descriptor{}, Context2),
    with_transaction(Context2,
                     once(ask(Context2, insert(foo,bar,baz,schema))),
                     _),
    open_descriptor(system_descriptor{}, Trans2),
    [Schema2] = Trans2.schema_objects,
    layer_to_id(Schema2.read, ID2),
    ID1 = ID2.


:- end_tests(inserts).

:- begin_tests(instance_validation).

:- use_module(core(util/test_utils)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).
:- use_module(core(account)).
:- use_module(core(document)).

test(cardinality_error,
     [setup((setup_temp_store(State),
             create_db_with_test_schema('admin','test'))),
      cleanup(teardown_temp_store(State)),
      error(unexpected_array_value(["Dublin","Dubhlinn"],'http://www.w3.org/2001/XMLSchema#string'),_)])
:-

    resolve_absolute_string_descriptor("admin/test", Master_Descriptor),

    create_context(Master_Descriptor, commit_info{author:"test",message:"commit a"}, Master_Context1),

    Object = _{'@type': "City",
               '@id' : "City/Dublin",
               'name': ["Dublin",
                        "Dubhlinn"
                        ]
              },


    with_transaction(
        Master_Context1,
        insert_document(Master_Context1, Object, ID),
        _),

    writeq(ID).

test(cardinality_min_error,
     [setup((setup_temp_store(State),
             create_db_with_test_schema('admin','test'))),
      cleanup(teardown_temp_store(State)),
      error(unexpected_array_value(["Duke","Doug"],'http://www.w3.org/2001/XMLSchema#string'),_)])
:-

    resolve_absolute_string_descriptor("admin/test", Master_Descriptor),

    create_context(Master_Descriptor, commit_info{author:"test",message:"commit a"}, Master_Context1),

    Master_Context2 = (Master_Context1.put(_{ all_witnesses : true })),
    % Check to see that we get the restriction on personal name via the
    % property subsumption hierarch *AND* the class subsumption hierarchy

    Object = _{'@type': "Person",
               '@id' : "Person/Duke",
               'name': ["Duke",
                        "Doug"]
              },

    with_transaction(
        Master_Context2,
        insert_document(Master_Context2,Object,ID),
        _),

    writeq(ID),

    once((member(Witness0, Witnesses),
          Witness0.'@type' = 'vio:InstanceCardinalityRestrictionViolation',
          Witness0.'vio:predicate'.'@value' = 'http://example.com/schema/worldOntology#name',
          '2' = Witness0.'vio:cardinality'.'@value'
         )),
    once((member(Witness1, Witnesses),
          Witness1.'@type' = 'vio:InstanceCardinalityRestrictionViolation',
          Witness1.'vio:predicate'.'@value' = 'http://example.com/schema/worldOntology#address',
          '0' = Witness1.'vio:cardinality'.'@value'
         )).


:- end_tests(instance_validation).

:- begin_tests(query_without_commit).

:- use_module(core(util/test_utils)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).
:- use_module(core(account)).
:- use_module(core(document)).

test(query_empty_database,
     [setup((setup_temp_store(State),
             create_db_without_schema('admin','test'))),
      cleanup(teardown_temp_store(State))]) :-
    resolve_absolute_string_descriptor("admin/test", Descriptor),
    create_context(Descriptor, commit_info{}, Context),

    with_transaction(Context,
                     \+ ask(Context,
                            t(_,_,_)),
                     _).

:- end_tests(query_without_commit).
