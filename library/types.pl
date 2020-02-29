:- module(types, [
              is_literal/1,
              is_uri/1,
              is_id/1,
              is_bnode/1,
              is_prefixed_uri/1,
              is_uri_or_id/1,
              is_object/1,
              is_object_or_id/1,
              is_graph_identifier/1,
              is_prefix_db/1,
              is_database_identifier/1,
              is_empty_graph_name/1,
              is_database/1,
              is_read_obj/1,
              is_write_obj/1
          ]).

/** <module> Types
 *
 * This module implements pervasive types which can be used for type
 * checking. Works particularly well in conjunction with Mavis.
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



/*************************************************************************

This file deliberately has no dependencies - please do not introduce them.

**************************************************************************/

:- use_module(library(apply)).
:- use_module(library(apply_macros)).

/**
 * is_literal(+X) is semidet.
 *
 */
is_literal(Data@Lang) :-
    atom(Lang),
    atom(Data).
is_literal(_Data^^Type) :-
    % this should probably have the full xsd build out.
    atom(Type).

:- multifile error:has_type/2.
error:has_type(literal,X) :-
    is_literal(X).

/**
 * is_uri(+X) is semidet.
 *
 */
is_uri(X) :-
    atom(X).
is_uri(X) :-
    is_prefixed_uri(X).

error:has_type(uri,X) :-
    is_uri(X).

/**
 * is_prefixed_uri(+X) is semidet.
 *
 **/
is_prefixed_uri(X):-
    nonvar(X),
    functor(X,(:),2).

error:has_type(prefixed_uri,X) :-
    is_prefixed_uri(X).

/**
 * is_id(+X) is semidet.
 *
 **/
is_id(id(Y)) :-
    (   atom(Y)
    ->  true
    ;   number(Y)).

error:has_type(id,X) :-
    is_id(X).


/**
 * is_uri_or_id(+X) is semidet.
 *
 **/
is_uri_or_id(X) :-
    is_uri(X).
is_uri_or_id(id(Y)) :-
    (   atom(Y)
    ->  true
    ;   number(Y)).

error:has_type(uri_or_id,X) :-
    is_uri_or_id(X).

/**
 * is_object(+X) is semidet.
 *
 **/
is_object(X) :-
    (   is_prefixed_uri(X) % DDD maybe not anymore...
    ->  true
    ;   is_literal(X)
    ->  true
    ;   is_uri(X)).


/**
 * is_object_or_id(+X) is semidet.
 *
 **/
is_object_or_id(X) :-
    (   is_object(X)
    ->  true
    ;   is_id(X)).


/**
 * is_bnode(+X) is semidet.
 */
is_bnode(X) :-
    atom_prefix(X,'_:').

/**
 * is_number_compat(X) is semidet.
 *
 * Compatibility with number type. Needs to not supply bindings as it
 * is not a constraint but a test of current groundness.
 *
 * Name borrowed from Ciao.
 **/
is_number_compat(X) :-
    (   var(X)
    ->  true
    ;   number(X)).


/*
 * is_graph_identifier(X) is semidet.
 *
 * Type of a graph identifier object
 */
is_graph_identifier(GID) :-
    atom(GID).

error:has_type(graph_identifier, X) :-
    is_graph_identifier(X).

/*
 * is_prefix_db(X) is semidet.
 *
 * Tests to see if we have a valid prefix database.
 */
is_prefix_db([]).
is_prefix_db([A=B|Tail]) :-
    atom(A),
    atom(B),
    is_prefix_db(Tail).

error:has_type(prefix_db, X) :-
    is_prefix_db(X).


/*
 * is_database_identifier(X) is semidet.
 *
 * Type of a database identifier
 */
is_database_identifier(X) :-
    atom(X).

error:has_type(database_identifier, X) :-
    is_database_identifier(X).

/**
 * is_empty_graph_name(+Database_Id:graph_identifier) is semidet.
 *
 * Sometimes we want to designate that there is no graph
 * we can do this with none or false. JSON converts to @(false)
 * for a null object.
 **/
is_empty_graph_name(Database_Id):-
    member(Database_Id, [false, @(false), none]),
    !.

/*
 * is_database(Database) is semidet.
 *
 */
is_database(DB) :-
    is_dict(DB),
    DB.get(name) = Name,
    DB.get(instance) = Instance,
    (   DB.get(inference) = Inference
    ->  true
    ;   Inference = []),
    DB.get(schema) = Schema,
    (   DB.get(error_instance) = Error_Instance
    ->  true
    ;   Error_Instance = []),
    (   DB.get(error_schema) = Error_Schema
    ->  true
    ;   Error_Schema = []),
    DB.get(write_transaction) =  _WT,
    DB.get(read_transaction) = _RT,

    atom(Name),
    maplist(is_graph_identifier,Instance),
    maplist(is_graph_identifier,Inference),
    maplist(is_graph_identifier,Schema),
    maplist(is_graph_identifier,Error_Instance),
    maplist(is_graph_identifier,Error_Schema),
    maplist(is_write_obj,RT),
    maplist(is_read_obj,RT).

error:has_type(database, X) :-
    is_database(X).

is_write_obj(write_obj{ dbid: N,
                        graphid: G,
                        builder: _B }) :-
    atom(N),
    atom(G).

error:has_type(write_obj, X) :-
    is_write_obj(X).

is_read_obj(read_obj{ dbid: N,
                      graphid: G,
                      layer: _B }) :-
    atom(N),
    atom(G).

error:has_type(read_obj, X) :-
    is_read_obj(X).
