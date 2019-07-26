:- module(prefixes, [
              initialise_prefix_db/0,
              initialise_prefix_db/1,
              update_collection_prefixes/2,
              set_collection_prefixes/2,
              delete_collection_prefixes/1,
              get_collection_prefixes/2
          ]).

/** <module> Prefixes
 * 
 * Prefix management utilities which assign a default prefix set with a 
 * given collection. 
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

% Use prolog persistency for prefix management.
% We could use the DB itself but this gets a bit too metacircular!
:- use_module(library(persistency)).

% Set up the database style
:- persistent prefix(collection:atom,prefix:atom,uri:atom).

:- use_module(library(file_utils)).
:- use_module(library(utils)).

/* 
 * default_prefixes(+C:uri,-P:atom,-U:uri) is det. 
 */
% per database shorthands
default_prefixes(C,doc,U) :-
    interpolate([C,'/',main], U).
default_prefixes(C,scm,U) :-
    interpolate([C,'/',schema], U).
% internal
default_prefixes(_,dcog,'https://datachemist.net/ontology/dcog#').
default_prefixes(_,dcogbox,'https://datachemist.net/ontology/dcogbox#').
default_prefixes(_,xdd,'https://datachemist.net/ontology/xdd#').
default_prefixes(_,rvo,'https://datachemist.net/ontology/rvo#').
% common
default_prefixes(_,xsd,'http://www.w3.org/2001/XMLSchema#').
default_prefixes(_,rdf,'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
default_prefixes(_,rdfs,'http://www.w3.org/2000/01/rdf-schema#').
default_prefixes(_,owl,'http://www.w3.org/2002/07/owl#').
default_prefixes(_,ex,'http://example.org/').

/*
 * initialise_prefix_db(+Collection) is det.
 * 
 * Set up default prefixes for a new collection. 
 * The semantics of this is quite different from initialise_prefix_db/2, 
 * because it doesn't cause a re-attache of the database. Perhaps we 
 * should use a different name?
 */
initialise_prefix_db(C) :-
    forall(
        default_prefixes(C,P,U),
        assert_prefix(C,P,U)
    ).

/* 
 * initialise_prefix_db is det.
 * 
 * Set up the prefix database. 
 */
initialise_prefix_db :- 
    once(file_search_path(regulum_home,BasePath)),
    interpolate([BasePath,'/',storage,'/','prefix.db'],File),
    (   \+ exists_file(File)
        % create the file
    ->  touch(File),
        db_attach(File, []),
        % Add default prefixes to all collections
        % (prefixes are collection dependent)
        collections(Collections),
        forall(
            member(C,Collections),
            initialise_prefix_db(C)
        )
        % Attach to the already existing file.
    ;   db_attach(File, [])
    ).

/* 
 * update_collection_prefixes(Prefixes:dict, Collection:atom) is det.
 */
update_collection_prefixes(Prefixes, Collection_Id) :-
    delete_collection_prefixes(Collection_Id),
    set_collection_prefixes(Prefixes, Collection_Id).

/* 
 * set_collection_prefixes(Prefixes:dict, Collection:atom) is det.
 */
set_collection_prefixes(Prefixes,Collection_Id) :-
    dict_pairs(Prefixes, _, Pairs),
    set_collection_pairs(Pairs, Collection_Id).

/* 
 * set_collection_pairs(Prefixes:list(pair(atom,atom)), Collection:atom) is det.
 * 
 * Use list of pairs to represent prefixes. 
 */
set_collection_pairs([], _).
set_collection_pairs([(Prefix-Uri)|Tail], Collection_Id) :-
    (   prefix(Collection_Id, Prefix, Uri)
    ->  true
    ;   assert_prefix(Collection_Id, Prefix, Uri)
    ),
    set_collection_pairs(Tail, Collection_Id).

/* 
 * delete_collection_pairs(Collection:atom) is det.
 * 
 * Delete all prefixes associated with a collection. 
 */
delete_collection_prefixes(Collection_Id):-
    retractall_prefix(Collection_Id,_,_).

/* 
 * get_collection_prefixes(Prefixes:dict,Collection:atom) is det.
 * 
 * Return a dictionary of prefixes for the current collection. 
 */
get_collection_prefixes(Prefixes, Collection_Id) :-
    setof(Prefix=Uri,
          prefix(Collection_Id, Prefix, Uri),
          Prefix_List),
    dict_pairs(Prefixes, _, Prefix_List).
