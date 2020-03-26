:- module(database,[
              query_context_transaction_objects/2,
              run_transaction/1,
              run_transactions/1,
              retry_transaction/1,
              with_transaction/2
          ]).

/** <module> Implementation of database graph management
 *
 * This module helps other modules with the representation of databases and
 * their associated graphs by bundling them as objects with some convenience
 * operators and accessors.
 *
 * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of TerminusDB.                                     *
 *                                                                       *
 *  TerminusDB is free software: you can redistribute it and/or modify   *
 *  it under the terms of the GNU General Public License as published by *
 *  the Free Software Foundation, under version 3 of the License.        *
 *                                                                       *
 *                                                                       *
 *  TerminusDB is distributed in the hope that it will be useful,        *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
 *  GNU General Public License for more details.                         *
 *                                                                       *
 *  You should have received a copy of the GNU General Public License    *
 *  along with TerminusDB.  If not, see <https://www.gnu.org/licenses/>. *
 *                                                                       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- use_module(core(transaction/descriptor)).
:- use_module(core(transaction/validate)).
:- use_module(core(util)).

:- use_module(library(prolog_stack)).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).

:- use_module(library(terminus_store)).


descriptor_database_name(Descriptor, 'terminus:///terminus') :-
    terminus_descriptor{} = Descriptor,
    !.
descriptor_database_name(Descriptor, ID) :-
    id_descriptor{ id : ID } = Descriptor.
descriptor_database_name(Descriptor, Label) :-
    label_descriptor{ label : Label } = Descriptor.
descriptor_database_name(Descriptor, Name) :-
    database_descriptor{ database_name : Name } = Descriptor,
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
    RW_Obj = read_write_obj{ descriptor: _Descriptor,
                             read: _Layer,
                             write: Layer_Builder },
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

slot_size(4).
slot_coefficient(0.25).
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

/**
 * retry_transaction(Query_Context) is nondet.
 *
 * Retry transaction sets up some number of choice points for
 * retrying a transaction together with a back-off strategy.
 *
 * If it is impossible to back out of a transaction (partial commits
 * in the case of multi-database transactions), we instead through an error and 
 * complain.
 */
retry_transaction(Query_Context) :-
    config:max_transaction_retries(Max_Transaction_Retries),

    between(0, Max_Transaction_Retries, Transaction_Retry_Count),

    % If we are > 0, we have actually failed at least once.
    (   Transaction_Retry_Count > 0
    ->  (   once((   multi_transaction(Query_Context)
                 ;   partial_commits(Query_Context)))
        ->  throw(error(multi_transaction_error("We are in a multi transaction which has partially commited"),context(retry_transaction/1,Query_Context)))
        ;   true),

        compute_backoff(Transaction_Retry_Count,BackOff),
        sleep(BackOff)
    ;   true).

/*
 * with_transaction(Query_Context) is det.
 */
:- meta_predicate with_transaction(?,0).
with_transaction(Query_Context,
                 Body) :-
    retry_transaction(Query_Context),
    call(Body),
    query_context_transaction_objects(Query_Context, Transactions),
    run_transactions(Transactions).

/*
 * run_transaction(Transaction) is det.
 *
 * Run transaction and throw errors with witnesses.
 *
 */
run_transaction(Transaction) :-
    transaction_objects_to_validation_objects([Transaction], Validations),
    commit_validation_objects(Validations).

/*
 * run_transactions(Transaction) is det.
 *
 * Run all transactions and throw errors with witnesses.
 *
 */
run_transactions(Transactions) :-
    transaction_objects_to_validation_objects(Transactions, Validations),
    commit_validation_objects(Validations).


/*
 * query_context_transaction_objects(+Query_Object,Transaction_Objects) is det.
 *
 * Marshall commit info into the transaction object and run it.
 */
query_context_transaction_objects(Query_Context,Transaction_Objects) :-
    maplist({Query_Context}/[Transaction_Object,New_Transaction_Object]>>(
                get_dict(commit_info,Query_Context,Commit_Info),
                put_dict(_{commit_info : Commit_Info},
                         Transaction_Object,
                         New_Transaction_Object)
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
                user_database_name(admin,testdb1,Name1),
                create_db(Name1, "http://localhost/testdb1"),
                user_database_name(admin,testdb2,Name2),
                create_db(Name2, "http://localhost/testdb2"))),
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
                user_database_name(admin,testdb1,Name1),
                create_db(Name1, "http://localhost/testdb1"),
                user_database_name(admin,testdb2,Name2),
                create_db(Name2, "http://localhost/testdb2"))),
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
        insert(doc:a, doc:b, doc:c)),

    query_context_transaction_objects(Context,Transaction_Objects),
    run_transactions(Transaction_Objects),

    partial_commits(Context).

:- end_tests(database_transactions).
