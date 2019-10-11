:- module(triplestore, [
              destroy_graph/2,
              check_graph_exists/2,
              make_empty_graph/2,
              sync_from_journals/0,
              sync_database/1,
              sync_graph/2,
              xrdf/5,
              insert/5,
              delete/5,
              update/6,
              commit/2,
              rollback/2,
              graph_checkpoint/3,
              current_checkpoint_directory/3,
              last_checkpoint_number/2,
              with_output_graph/2,
              ttl_to_hdt/2,
              with_transaction/2,
              checkpoint/2,
              canonicalise_subject/2,
              canonicalise_predicate/2,
              canonicalise_object/2,
              triples_canonical/2
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
 * sync_from_journals is det.  
 * 
 */ 
sync_from_journals :-    
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

/** 
 * insert(+DB:atom,+G:graph_identifier,+Builder,+X,+Y,+Z) is det.
 * 
 * Insert triple into transaction layer
 */
insert(DB,G,Builder,X,Y,Z) :-
    (   xrdf(DB,[G],X,Y,Z)
    ->  true
    ;   nb_add_triple(Builder, X, Y, Z)
    ).

/** 
 * delete(+DB,+G,+Builder,+X,+Y,+Z) is det.
 * 
 * Delete quad from transaction predicates.
 */
delete(DB,G,Builder,X,Y,Z) :-
    (   xrdf(DB,[G],X,Y,Z)
    ->  nb_remove_triple(Builder,X,Y,Z)
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
xrdf(C,Gs,X,Y,Z) :-
    assertion(is_list(Gs)), % take out for production? This gets called a *lot*
    member(G,Gs),
    dbid_graphid_obj(C,G,Obj),
    head(Obj,Layer),
    triple(Layer,X,Y,Z).

/* 
 * bind_read_graph_layers(+Spec_List:list) is det. 
 * 
 * Spec_List is a skeleton DBN-GN-GL in which DBN and GN are bound 
 * and GL is supplied. 
 * 
 * We produce a GL binding to the current read layer of head.
 */ 
bind_read_graph_layers([]).
bind_read_graph_layers([DBN-GN-GL|Rest]) :-
    dbid_graphid_obj(DBN,GN,Graph_Obj),
    head(Graph_Obj,GL),
    !,
    bind_read_graph_layers(Rest).
bind_read_graph_layers([DBN-GN-_|Rest]) :-
    format(string(MSG), 'Unable to bind a read layer for DB: ~q and Graph ~q', [DBN,GN]),
    throw(http_reply(resource_error(_{'terminus:status' : 'terminus:error',
                                      'terminus:message' : MSG}))).
bind_read_graph_layers([Spec|Rest]) :-
    format(string(MSG), 'Unable to interpret read graph specification: ~q', [Spec]),
    throw(http_reply(resource_error(_{'terminus:status' : 'terminus:error',
                                      'terminus:message' : MSG}))).

/* 
 * bind_write_graph_layer_builders(+Spec_List:list) is det. 
 * 
 * Spec_List is a skeleton DBN-GN-WGLB in which DBN and GN are bound 
 * and WGLB is supplied. In fact we ignore DBN and GN, but will 
 * use them at commit time to know which associated layer builder we mean.
 * 
 */ 
bind_write_graph_layer_builders([]).
bind_write_graph_layer_builders([_DBN-_GN-WGLB-_WL|Rest]) :-
    store(Store),
    open_write(Store,WGLB),
    !,
    bind_write_graph_layer_builders(Rest).
bind_write_graph_layer_builders([DBN-GN-_-_|Rest]) :-
    format(string(MSG), 'Unable to bind a write layer for DB: ~q and Graph ~q', [DBN,GN]),
    throw(http_reply(resource_error(_{'terminus:status' : 'terminus:error',
                                      'terminus:message' : MSG}))).
bind_write_graph_layer_builders([Spec|Rest]) :-
    format(string(MSG), 'Unable to interpret write graph specification: ~q', [Spec]),
    throw(http_reply(resource_error(_{'terminus:status' : 'terminus:error',
                                      'terminus:message' : MSG}))).


/* 
 * commit_write_graph_layer_builders(+Spec_List:list) is det. 
 * 
 * Spec_List is a skeleton DBN-GN-WGLB-WL in which DBN and GN and WGLB are bound 
 * and WL is supplied. 
 * 
 */ 
commit_write_graph_layer_builders([]).
commit_write_graph_layer_builders([_DBN-_GN-WGLB-WL|Rest]) :-
    nb_commit(WGLB,WL),
    !,
    commit_write_graph_layer_builders(Rest).
commit_write_graph_layer_builders([DBN-GN-_-_|Rest]) :-
    format(string(MSG), 'Unable to commit a write layer builder for DB: ~q and Graph ~q', [DBN,GN]),
    throw(http_reply(resource_error(_{'terminus:status' : 'terminus:error',
                                      'terminus:message' : MSG}))).
commit_write_graph_layer_builders([Spec|Rest]) :-
    format(string(MSG), 'Unable to interpret write graph commit specification: ~q', [Spec]),
    throw(http_reply(resource_error(_{'terminus:status' : 'terminus:error',
                                      'terminus:message' : MSG}))).


/* 
 * push_write_graph_layers(+Spec_List:list) is det. 
 * 
 * Spec_List is a skeleton DBN-GN-WGLB-WL in which DBN and GN and WGLB are bound 
 * and WL is supplied. 
 * 
 */ 
push_write_graph_layers([]).
push_write_graph_layers([_DBN-_GN-WGLB-WL|Rest]) :-
    nb_set_head(WGLB,WL),
    !,
    push_write_graph_layers(Rest).
push_write_graph_layers([DBN-GN-_-_|Rest]) :-
    format(string(MSG), 'Unable to push a write layer builder for DB: ~q and Graph ~q', [DBN,GN]),
    throw(http_reply(resource_error(_{'terminus:status' : 'terminus:error',
                                      'terminus:message' : MSG}))).
push_write_graph_layers([Spec|Rest]) :-
    format(string(MSG), 'Unable to interpret write graph push specification: ~q', [Spec]),
    throw(http_reply(resource_error(_{'terminus:status' : 'terminus:error',
                                      'terminus:message' : MSG}))).


/** 
 * hoare_transaction(+Options,:Pre,:Update,:Post) is semidet.
 * 
 * If Succeeds if Pre, Executes Update and tests Posts resulting in 
 * member(witnesses(Witnesses), Options)
 * 
 * Options is a list which contains any of:
 * 
 *  * read_graphs([DB_ID-Graph_ID0-RL0,
 *                 DB_ID-Graph_ID1-RL1,
 *                 ....
 *                 DB_ID-Graph_IDN-RLN])
 * 
 *  Specifying each of the writeable graphs which is in the transaction
 *  and binding the associated write layer-builder.
 *
 * 
 *  * write_graphs([DB_ID-Graph_ID0-WLB0-WL0,
 *                  DB_ID-Graph_ID1-WLB1-WL1, 
 *                  ...
 *                  DB_ID-Graph_IDN-WLBN-WLN
 *                 ])
 * 
 *  Specifying each of the writeable graphs which is in the transaction
 *  and binding the associated write layer-builder.
 * 
 *  * witnesses(Witnesses)
 *  
 *  Which gives a list of the witnesses of failure. 
 * 
 */
hoare_transaction(Options,Pre,Update,Post) :-
    (   memberchk(read_graphs(Read_Graph_Objs),Options)
    ->  true
    ;   Read_Graph_Objs=[]),

    bind_read_graph_layers(Read_Graph_Objs),
    
    (   memberchk(write_graphs(Write_Graph_Objs),Options)        
    ->  true
    ;   Write_Graph_Objs = []),

    bind_write_graph_layer_builders(Write_Graph_Objs),

    (   memberchk(witnesses(_), Options)
    ->  Transaction_Options = Options
    ;   Transaction_Options = [witnesses([])|Options]
    ),
    
    call(Pre),
    
    (   call(Update)
    ->  commit_write_graph_layer_builders(Write_Graph_Objs)
    ;   format(string(MSG), "Unable to perform the update requested: ~q", [Update]),
        throw(http_repy(not_acceptable(_{'terminus:status' : 'terminus:error',
                                         'terminus:message' : MSG})))),
    
    (   call(Post),
    ->  (   memberchk(witnesses([]),Options),
        ->  push_write_graph_layers(Write_Graph_Objs)
        ;   true)
    ;   format(string(MSG), "Unable to run post_condition: ~q", [Post]),
        throw(http_repy(not_acceptable(_{'terminus:status' : 'terminus:error',
                                         'terminus:message' : MSG})))).
