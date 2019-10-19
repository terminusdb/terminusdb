:- module(triplestore, [
              destroy_graph/2,
              check_graph_exists/2,
              make_empty_graph/2,
              sync_backing_store/0,
              sync_database/1,
              sync_graph/2,
              xrdf/5,
              insert/5,
              delete/5,
              update/6,
              with_transaction/3
          ]).

:- use_module(library(terminus_store)).
:- use_module(library(file_utils)).
:- use_module(library(journaling)).
:- use_module(library(utils)).
:- use_module(library(schema), [cleanup_schema_module/1]).
:- use_module(library(prefixes)).
:- use_module(library(types)).
% feeling very circular :(
:- use_module(library(database)).

:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(apply_macros)).

/** <module> Triplestore
 * 
 * This module contains the database management predicates responsible 
 * for creating collections, graphs and syncing from journals.
 * 
 * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of TerminusDB.                                     *
 *                                                                       *
 *  TerminusDB is free software: you can redistribute it and/or modify   *
 *  it under the terms of the GNU General Public License as published by *
 *  the Free Software Foundation, under version 3 of the License.       *
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

/** 
 * destroy_graph(+DBID,+GID:graph_identifier) is det. 
 * 
 * Completely remove a graph from disk.
 * 
 * currently this is a noop - we can worry about collection 
 * later.
 */ 
destroy_graph(_DBID,_GID) :-
    true.


/**
 * check_graph_exists(+Database_ID,+G:graph_identifier) is semidet.
 * 
 * checks to see is the graph id in the current graph list
 */
check_graph_exists(DB,G):-
    dbid_graphid_obj(DB,G,_),
    !.
 
/** 
 * checkpoint(+Collection_Id,+Database_Id:graph_identifier) is det.
 * 
 * Create a new graph checkpoint from our current dynamic triple state
 * and sync.
 * 
 * UUU Should this ever be called? - should it be automatic?
 */
checkpoint(_DB_ID,Graph_ID) :-
    throw(error("Unimplemented")).

/* 
 * storage(-Storage_Obj) is det. 
 * 
 * Global variable holding the current storage associated with the server. 
 * This is set by sync_storage/0.
 */ 
:- dynamic storage/1.

/* 
 * sync_storage is semidet. 
 * 
 * Global variable holding the current storage associated with the server. 
 * This is set by sync_storage/0.
 */ 
sync_storage :- 
    with_mutex(
        sync_storage,
        (
            db_path(Path),
            open_directory_store(Path,Storage),
            retractall(storage(_)),
            assertz(storage(Storage))
        )
    ).

/* 
 * sync_graph(+DBN,+GID) is det. 
 * 
 * Sync the graph into dbid_graphid_obj/3 or throw an error.
 * 
 * The Mutex is not needed when called from sync_database 
 * but adding in case others call sync_graph/2 directly.
 */ 
sync_graph(DBN,G) :-
    atomic_list_concat(['sync_', DBN,'_',G], Mutex),
    with_mutex(
        Mutex,
        (   open_named_graph(Storage, G, Gobj),
            retractall(dbid_graphid_obj(DBN, G, _)),
            assertz(dbid_graphid_obj(DBN, G, Gobj))
        ->  true
        ;   retractall(dbid_graphid_obj(DBN, G, _)),
            format(atom(Msg),
                   'Could not sync graph ~q for database ~q',
                   [DBN,G]),
            throw(graph_sync_error(_{'terminus:status' : 'terminus:failure',
                                     'terminus:message' : Msg,
                                     'terminus:broken_database' : DBN,
                                     'terminus:broken_graph' : G
                                    }))
        )
    ).

/* 
 * dbid_graphid_obj(+DBID,+GraphID,-Obj).
 * 
 * Maintains a table of the current named_graph objects in terminus-store 
 * associated with the given database and graph. 
 */
:- dynamic dbid_graphid_obj/3.

/* 
 * sync_database(Database) is det.
 * 
 * Maintains a table of the current named_graph objects in terminus-store 
 * associated with the given database and graph. 
 */
sync_database(Database) :-
    % I believe we want to be able to set the database predicate for this database
    % without someone racing us.
    database_name(Database,DBN),
    atomic_list_concat(['sync_', DBN], Mutex),
    with_mutex(
        Mutex,
        (
            (   
                storage(Storage),
                
                database_instance(Database, Instances),
                database_inference(Database, Inferences),
                database_schema(Database, Schemas),
                
                append([Instances,Inferences,Schemas], Graphs),
                
            ->  forall(
                    member(G, Graphs),
                    sync_graph(DBN,G)
                )
            
            ;   format(atom(Msg),
                       'Could not load database or associated graphs for database ~q',
                       [DBN]),                            
                throw(graph_sync_error(_{'terminus:status' : 'terminus:failure',
                                         'terminus:message' : Msg,
                                         'terminus:broken_database' : DBN
                                        }))
            )
        )
    ).

sync_terminus :-
    storage(Storage),
    terminus_database(Terminus), 
    sync_database(Terminus).

sync_databases :-
    database_record_list(Database_Names),
    forall(
        member(DBN,Database_Names),
        (
            make_database_from_database_name(DBN,Database),
            sync_database(Database)
        )
    ).

/** 
 * sync_backing_store is det.  
 * 
 */ 
sync_backing_store :-    
    /* do something here */
    sync_storage,
    sync_terminus,
    sync_databases.

/** 
 * make_empty_graph(+DB_ID,+Graph_ID) is det.
 * 
 * Create a new empty graph.
 * 
 * UUU
 */
make_empty_graph(DB_ID,Graph_ID) :-
    atomic_list_concat(['sync_',DB_ID,'_',Graph_ID], Mutex),
    with_mutex(
        Mutex,
        (   
            store(Store),
            create_database(Store,Graph_ID,Graph_Obj),
            assert(dbid_graphid_obj(DB_ID,Graph_ID,Graph_Obj))
        )
    ).


/** 
 * import_graph(+File,+DB_ID,+Graph_ID) is det.
 * 
 * This predicate imports a given File as the latest checkpoint of Database_Name
 * 
 * File will be in either ntriples, turtle or hdt format. 
 * 
 * UUU - do some transactional stuff here as with schema.
 */
import_graph(File, DB_ID, Graph_ID) :-
    false.

object_storage(O,S) :-
    % easier to to go this way...
    (   nonvar(O)
    ->  (   O = literal(L),
            nonvar(L)
        ->  (   L = lang(Lang,String),
            ->  format(string(S), '"~q"@~s', [String,Lang])
            ;   L = type(Type,String),
                format(string(S), '"~q"^^"~q"', [String,Type]))
        ;   true)
    ;   true
    ).

storage_object(S,O) :- 
    (   S = value(V)
    ->  (   re_matchsub('^"(.*)"\\^\\^"(.*)"', V, M, []),
            O = literal(type(M.2, M.1))
        ->  re_matchsub('^"(.*)"@(.*)', V, M, []),
            O = literal(lang(M.2, M.1))
        ;   throw(error('Bad stored value ~q')))
    ;   S = O).

canonical_representation(S, S) :-
    is_string(S), 
canonical_representation(literal(L), value(Rep)) :-
    (   L = lang(Lang,String),
        format(string(Rep), '"~q"@"~q"', [String,Lang])
    ->  L = type(Type,String),
        format(string(Rep), '"~q"^^"~q"', [String,Type])
    ).

/** 
 * insert(+DB:atom,+G:graph_identifier,+Builder,+X,+Y,+Z) is det.
 * 
 * Insert triple into transaction layer
 */
insert(DB,G,X,Y,Z) :-
    (   xrdf(DB,[G],X,Y,Z)
    ->  true
    ;   object_storage(Z,S),
        get_write_builder(DB,G,Builder),
        nb_add_triple(Builder, X, Y, S),
        storage_object(S,Z),
    ).

/** 
 * delete(+DB,+G,+Builder,+X,+Y,+Z) is det.
 * 
 * Delete quad from transaction predicates.
 */
delete(DB,G,X,Y,Z) :-
    (   xrdf(DB,[G],X,Y,Z)
    ->  object_storage(Z,S),
        get_write_builder(DB,G,Builder),
        nb_remove_triple(Builder,X,Y,S),
        storage_object(S,Z)
    ;   true).

new_triple(_,Y,Z,subject(X2),X2,Y,Z).
new_triple(X,_,Z,predicate(Y2),X,Y2,Z).
new_triple(X,Y,_,object(Z2),X,Y,Z2).

/** 
 * update(+DB,+G,+X,+Y,+Z,+G,+Action) is det.
 * 
 * Update transaction graphs
 */ 
update(DB,G,X,Y,Z,Action) :-
    delete(DB,G,X,Y,Z),
    new_triple(X,Y,Z,Action,X1,Y1,Z1),
    insert(DB,G,X1,Y1,Z1).

/** 
 * xrdf(+Collection_Id,+Database_Ids:list,?Subject,?Predicate,?Object) is nondet.
 * 
 * The basic predicate implementing the the RDF database.
 * This layer has the transaction updates included.
 *
 * Database is either an atom or a list of atoms, referring to the name(s) of the graph(s).
 */
xrdf(Database,Gs,X,Y,Z) :-
    assertion(is_list(Gs)), % take out for production? This gets called a *lot*
    member(G,Gs),
    get_read_layer(Database,G,Layer),
    xrdf_db(Layer,X,Y,Z).

/* 
 * xrdf_db(Layer,X,Y,Z) is nondet.
 * 
 * Marshalls types appropriately for terminus-store
 */ 
xrfd_db(Layer,X,Y,Z)
    object_storage(Z,S),
    triple(Layer,X,Y,S),
    storage_object(S,Z).

/* 
 * open_update_transaction(Transaction_Records:list) is det. 
 * 
 * Opens a new database Update which can be used during the transaction. 
 */ 
open_pre_transaction([]).
open_pre_transaction([transaction_record{
                          pre_database : Pre,
                          write_graphs : Write_Graphs,
                          update_database : Update,
                          post_database : _}
                      | Rest]) :-
    open_read_transaction(Pre, Intermediate),
    open_write_transaction(Intermediate, Write_Graphs, Update),
    open_pre_transaction(Rest).

/* 
 * open_post_transaction(Transction_Records:list) is det. 
 *
 * Opens a final database to test before pushing head. 
 */
open_post_transaction([]).
open_post_transaction([transaction_record{
                           pre_database : _,
                           update_database : Update,
                           write_graphs : _,
                           post_database : Post}                       
                       |Rest]) :-
    commit_write_transaction(Update,Post),
    open_post_transaction(Rest).

/* 
 * commit_transaction(Transaction_Records:list) is det. 
 * 
 * Commits all altered graphs to head. 
 */
commit_transaction([]).
commit_transaction([transaction_record{ pre_database: _,
                                        write_graphs: WGs,
                                        update_database :_,
                                        post_database: PDB }|Rest]) :-
    maplist({PDB}/[G]>>(
                store(Store),
                open_named_graph(Store,G,G_Obj),
                get_read_layer(PDB,G,Layer)
                nb_set_head(DG_Obj, Layer),
            ), WGs),
    commit_transaction(Rest).
                  

/** 
 * with_transaction(+Options,:Query_Update,:Post) is semidet.
 * 
 * Succeeds if Update and tests Post resulting in 
 * member(witnesses(Witnesses), Options)
 * 
 * Options is a list which contains any of:
 * 
 *  * transaction_records([transaction_record{ pre_database : Pre_Database_1, 
 *                                             write_graphs : [Graph_Id_1_1... Graph_Id_1_k], 
 *                                             update_database : Update_Database_1, 
 *                                             post_database : Post_Database_1 }, 
 *                         ...
 *                         transaction_record{ pre_database : Pre_Database_n, 
 *                                             write_graphs : [Graph_Id_n_1 ... Graph_Id_n_j], 
 *                                             update_database : Update_Database_n, 
 *                                             post_database : Post_Database_n }])
 * 
 *  Specifying each of the databases which exist in the pre, update and post phases.
 *  Each database object binds in turn the layers and builders of the transaction 
 *  in its associated read_obj / write_obj fields.
 * 
 *  * witnesses(Witnesses)
 *  
 *  Which gives a list of the witnesses of failure. 
 * 
 */
with_transaction(Options,Query_Update,Post) :-
    (   memberchk(transaction_records(Records), Options)
    ->  true
    ;   format(string(MSG), "No transaction records in options: ~q", [Options]),
        throw(http_repy(not_acceptable(_{'terminus:status' : 'terminus:error',
                                         'terminus:message' : MSG})))),

    % get read layer and layer builders for transaction
    (   open_pre_transaction(Records)
    ->  format(string(MSG), "Unable to process pre transaction records: ~q", [Records]),
        throw(http_repy(not_acceptable(_{'terminus:status' : 'terminus:error',
                                         'terminus:message' : MSG})))),
    
    (   call(Update)
    ->  true
    ;   format(string(MSG), "Unable to perform the update requested: ~q", [Update]),
        throw(http_repy(not_acceptable(_{'terminus:status' : 'terminus:error',
                                         'terminus:message' : MSG})))),
    
    (   open_post_transaction(Records)
    ->  true
    ;   format(string(MSG), "Unable to process post transaction records: ~q", [Records]),
        throw(http_repy(not_acceptable(_{'terminus:status' : 'terminus:error',
                                         'terminus:message' : MSG})))),
        
    (   call(Post),
    ->  (   memberchk(witnesses([]),Options),
        ->  push_transaction(Records)
        ;   true)
    ;   format(string(MSG), "Unable to run post_condition: ~q", [Post]),
        throw(http_repy(not_acceptable(_{'terminus:status' : 'terminus:error',
                                         'terminus:message' : MSG})))).
