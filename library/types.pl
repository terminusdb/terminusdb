:- module(types, [
              is_uri/1,
              is_id/1,
              is_prefixed_uri/1,
              is_uri_or_id/1,
              is_object/1,
              is_object_or_id/1,
              is_graph_identifier/1,
              is_prefix_db/1,
              is_collection_identifier/1,
              is_empty_graph_name/1,
              is_graph/1
          ]).

/** <module> Types
 * 
 * This module implements pervasive types which can be used for type 
 * checking.
 *
 * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of RegulumDB.                                      *
 *                                                                       *
 *  RegulumDB is free software: you can redistribute it and/or modify    *
 *  it under the terms of the GNU General Public License as published by *
 *  the Free Software Foundation, either version 3 of the License, or    *
 *  (at your option) any later version.                                  *
 *                                                                       *
 *  RegulumDB is distributed in the hope that it will be useful,         *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
 *  GNU General Public License for more details.                         *
 *                                                                       *
 *  You should have received a copy of the GNU General Public License    *
 *  along with RegulumDB.  If not, see <https://www.gnu.org/licenses/>.  *
 *                                                                       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/** 
 * is_uri(+X) is semidet.
 * 
 * Mavis type checking.
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
    (   is_prefixed_uri(X)
    ->  true        
    ;   rdf_is_literal(X)
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
is_graph_identifier(GIDs) :-
    maplist(atom,GIDs).

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
 * is_collection_identifier(X) is semidet.
 * 
 * Type of a collection identifier
 */ 
is_collection_identifier(X) :-
    atom(X).

error:has_type(collection_identifier, X) :-
    is_collection_identifier(X).

/**
 * is_empty_graph_name(+Graph_Id:graph_identifier) is semidet.
 * 
 * Sometimes we want to designate that there is no graph
 * we can do this with none or false. JSON converts to @(false) 
 * for a null object.
 **/
is_empty_graph_name(Graph_Id):-
    member(Graph_Id, [false, @(false), none]),
    !.

error:has_type(rdf_object, Rdf_Object):-
    is_rdf_object(Rdf_Object).

/* 
 * is_graph(Graph) is semidet.
 * 
 * 
 */
is_graph(graph(Collection,Instance,Inference,Schema,Error_Instance,Error_Schema)) :-
    is_graph_identifier(Collection),
    is_graph_identifier(Instance),
    is_graph_identifier(Inference),
    is_graph_identifier(Schema),
    is_graph_identifier(Error_Instance),
    is_graph_identifier(Error_Schema).

error:has_type(graph, X) :-
    is_graph(X).

