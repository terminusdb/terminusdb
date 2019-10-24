:- module(triplestore, [
              destroy_graph/2,
              check_graph_exists/2,
              make_empty_graph/2,
              sync_backing_store/0,
              safe_create_named_graph/3,
              safe_open_named_graph/3,
              xrdf/5,
              xrdf_db/4,
              insert/5,
              delete/5,
              update/6,
              storage/1
          ]).

:- use_module(library(terminus_store)).
:- reexport(library(terminus_store),
            except([create_named_graph/3,
                    open_named_graph/3])).

:- use_module(library(file_utils)).
:- use_module(library(utils)).
:- use_module(library(schema), [cleanup_schema_module/1]).
:- use_module(library(prefixes)).
:- use_module(library(types)).
% feeling very circular :(
:- use_module(library(database)).
:- use_module(library(literals)).

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
destroy_graph(_DBID,GID) :-
    db_path(Path),
    www_form_encode(GID,Safe_GID),
    interpolate([Path,Safe_GID,'.label'],Label),
    delete_file(Label).

/**
 * check_graph_exists(+Database_ID,+G:graph_identifier) is semidet.
 * 
 * checks to see is the graph id in the current graph list
 */
check_graph_exists(_DB,G):-
    storage(Store),
    safe_open_named_graph(Store,G,_).
 
/** 
 * checkpoint(+Collection_Id,+Database_Id:graph_identifier) is det.
 * 
 * Create a new graph checkpoint from our current dynamic triple state
 * and sync.
 * 
 * UUU Should this ever be called? - should it be automatic?
 */
checkpoint(_DB_ID,_Graph_ID) :-
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
        (   storage(Storage),
            safe_open_named_graph(Storage, G, Gobj),
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

/** 
 * sync_backing_store is det.  
 * 
 */ 
sync_backing_store :-    
    /* do something here */
    sync_storage.

/* 
 * safe_create_named_graph(+Store,+Graph_ID,-Graph_Obj) is det. 
 *
 * create the named graph, encoded for file-name safety
 */
safe_create_named_graph(Store,Graph_ID,Graph_Obj) :-
    www_form_encode(Graph_ID,Safe_Graph_ID),
    create_named_graph(Store,Safe_Graph_ID,Graph_Obj).

/* 
 * safe_open_named_graph(+Store,+Graph_ID,-Graph_Obj) is det. 
 *
 * open the named graph, encoded for file-name safety
 */
safe_open_named_graph(Store, Graph_ID, Graph_Obj) :-
    www_form_encode(Graph_ID,Safe_Graph_ID),
    open_named_graph(Store,Safe_Graph_ID,Graph_Obj).

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
            storage(Store),
            safe_create_named_graph(Store,Graph_ID,Graph_Obj),
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
import_graph(_File, _DB_ID, _Graph_ID) :-
    throw(error(_{'terminus:status' : 'terminus:error',
                  'terminus:message' : "import_graph/3 is unimplemented"})).

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
        nb_add_triple(Builder, X, Y, S)
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
        nb_remove_triple(Builder,X,Y,S)
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

/* 
 * xrdf_added(+DB:database,+G:graph_identifier,+X,+Y,+Z) is nondet. 
 * 
 * Query exactly the current layer (and no deeper) for added triples.
 */ 
xrdf_added(DB,G,X,Y,Z) :-
    get_read_layer(DB,G,L),
    pre_convert_node(X,A),
    pre_convert_node(Y,B),
    object_storage(Z,S),
    triple_addition(L,A,B,S),
    post_convert_node(A,X),
    post_convert_node(B,Y),
    storage_object(S,Z).

/* 
 * xrdf_deleted(+DB:database,+G:graph_identifier,+X,+Y,+Z) is nondet. 
 * 
 * Query exactly the current layer (and no deeper) for deleted triples.
 */ 
xrdf_deleted(DB,G,X,Y,Z) :-
    get_read_layer(DB,G,L),
    pre_convert_node(X,A),
    pre_convert_node(Y,B),
    object_storage(Z,S),
    triple_removal(L,A,B,S),
    post_convert_node(A,X),
    post_convert_node(B,Y),
    storage_object(S,Z).

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
    maybe_open_read_transaction(Database,DBR),
    get_read_layer(DBR,G,Layer),
    xrdf_db(Layer,X,Y,Z).


pre_convert_node(X,A) :-
    (   nonvar(X)
    ->  atom_string(X,A)
    ;   true).

post_convert_node(A,X) :-
    (   nonvar(X)
    ->  (   atom(X)
        ->  string_to_atom(A,X)
        ;   X = A)
    ;   string_to_atom(A,X)).

/* 
 * xrdf_db(Layer,X,Y,Z) is nondet.
 * 
 * Marshalls types appropriately for terminus-store
 */ 
xrdf_db(Layer,X,Y,Z) :-
    pre_convert_node(X,A),
    pre_convert_node(Y,B),
    object_storage(Z,S),
    triple(Layer,A,B,S),
    post_convert_node(A,X),
    post_convert_node(B,Y),
    storage_object(S,Z).

