:- module(triplestore, [
              destroy_graph/2,
              sync_backing_store/0,
              safe_create_named_graph/3,
              safe_open_named_graph/3,
              xrdf/4,
              xrdf_db/4,
              xrdf_deleted/4,
              xrdf_added/4,
              insert/4,
              delete/4,
              update/5,
              storage/1
          ]).

:- reexport(library(terminus_store),
            except([create_named_graph/3,
                    open_named_graph/3])).

:- use_module(file_utils).
:- use_module(utils).
:- use_module(schema, [cleanup_schema_module/1]).
:- use_module(types).
% feeling very circular :(
:- use_module(database).
:- use_module(literals).
:- use_module(descriptor).

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
    terminus_store:create_named_graph(Store,Safe_Graph_ID,Graph_Obj).

/*
 * safe_open_named_graph(+Store,+Graph_ID,-Graph_Obj) is det.
 *
 * open the named graph, encoded for file-name safety
 */
safe_open_named_graph(Store, Graph_ID, Graph_Obj) :-
    www_form_encode(Graph_ID,Safe_Graph_ID),
    terminus_store:open_named_graph(Store,Safe_Graph_ID,Graph_Obj).


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
 * insert(+G:read_write_obj,+X,+Y,+Z) is det.
 *
 * Insert triple into transaction layer
 */
insert(G,X,Y,Z) :-
    object_storage(Z,S),
    read_write_obj_builder(G, Builder),
    ignore(nb_add_triple(Builder, X, Y, S)).

/**
 * delete(+G,+Builder,+X,+Y,+Z) is det.
 *
 * Delete quad from transaction predicates.
 */
delete(Gs,X,Y,Z) :-
    forall(
        member(G,Gs),
        (   object_storage(Z,S),
            read_write_obj_builder(G, Builder),
            ignore(nb_remove_triple(Builder, X, Y, S)))).

new_triple(_,Y,Z,subject(X2),X2,Y,Z).
new_triple(X,_,Z,predicate(Y2),X,Y2,Z).
new_triple(X,Y,_,object(Z2),X,Y,Z2).

/**
 * update(+DB,+G,+X,+Y,+Z,+G,+Action) is det.
 *
 * Update transaction graphs
 */
update(G,X,Y,Z,Action) :-
    delete([G],X,Y,Z),
    new_triple(X,Y,Z,Action,X1,Y1,Z1),
    insert(G,X1,Y1,Z1).

/*
 * xrdf_added(+Gs:list(read_write_obj),+X,+Y,+Z) is nondet.
 *
 * Query exactly the current layer (and no deeper) for added triples.
 */
xrdf_added(Gs,X,Y,Z) :-
    member(G,Gs),
    read_write_obj_reader(G, Layer),
    pre_convert_node(X,A),
    pre_convert_node(Y,B),
    object_storage(Z,S),
    triple_addition(Layer,A,B,S),
    post_convert_node(A,X),
    post_convert_node(B,Y),
    storage_object(S,Z).

/*
 * xrdf_deleted(+Gs:list(read_write_obj),+X,+Y,+Z) is nondet.
 *
 * Query exactly the current layer (and no deeper) for deleted triples.
 */
xrdf_deleted(Gs,X,Y,Z) :-
    member(G,Gs),
    read_write_obj_reader(G, Layer),
    pre_convert_node(X,A),
    pre_convert_node(Y,B),
    object_storage(Z,S),
    triple_removal(Layer,A,B,S),
    post_convert_node(A,X),
    post_convert_node(B,Y),
    storage_object(S,Z).

/**
 * xrdf(?Subject,?Predicate,?Object) is nondet.
 *
 * The basic predicate implementing the the RDF database.
 * This layer has the transaction updates included.
 *
 * WARNING: Collection is now unused!
 */
xrdf(Gs,X,Y,Z) :-
    assertion(is_list(Gs)), % take out for production? This gets called a *lot*
    member(G,Gs),
    read_write_obj_reader(G, Layer),
    xrdf_db(Layer,X,Y,Z).


pre_convert_node(X,A) :-
    (   nonvar(X)
    ->  atom_string(X,A)
    ;   true).

post_convert_node(A,X) :-
    (   nonvar(X)
    ->  (   atom(X)
        ->  atom_string(X,A)
        ;   X = A)
    ;   atom_string(X,A)).

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
