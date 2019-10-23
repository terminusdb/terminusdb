:- module(prefixes, [
              initialise_prefix_db/0,
              initialise_prefix_db/1,
              update_collection_prefixes/2,
              set_collection_prefixes/2,
              delete_collection_prefixes/1,
              get_collection_prefixes/2,
              get_collection_prefix_pairs/2,
              get_collection_prefix_list/2,
              prefix_list_to_rapper_args/2,
              initialise_contexts/0,
              woql_context/1,
              get_collection_jsonld_context/2,
              get_global_jsonld_context/1
          ]).

/** <module> Prefixes
 * 
 * Prefix management utilities which assign a default prefix set with a 
 * given collection. 
 * 
 * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of TerminusDB.                                      *
 *                                                                       *
 *  TerminusDB is free software: you can redistribute it and/or modify    *
 *  it under the terms of the GNU General Public License as published by *
 *  the Free Software Foundation, under version 3 of the License.        *
 *                                                                       *
 *                                                                       *
 *  TerminusDB is distributed in the hope that it will be useful,         *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
 *  GNU General Public License for more details.                         *
 *                                                                       *
 *  You should have received a copy of the GNU General Public License    *
 *  along with TerminusDB.  If not, see <https://www.gnu.org/licenses/>.  *
 *                                                                       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

% Use prolog persistency for prefix management.
% We could use the DB itself but this gets a bit too metacircular!
:- use_module(library(persistency)).

% Set up the database style
:- persistent prefix(collection:atom,prefix:atom,uri:atom).

:- use_module(library(file_utils)).
:- use_module(library(utils)).

% JSON manipulation
:- use_module(library(http/json)).

:- use_module(library(jsonld)).

:- use_module(library(database), [
                  database_name_list/1,
                  terminus_database_name/1
              ]).

% efficiency
:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(apply_macros)).

/* 
 * default_prefixes(+C:uri,-P:atom,-U:uri) is det. 
 */
% per database shorthands
default_prefixes(C,doc,U) :-
    interpolate([C,'/',document,'/'], U).
default_prefixes(C,scm,U) :-
    interpolate([C,'/',schema,'#'], U).
% internal
default_prefixes(_,Pre,URI) :-
    global_prefixes(Pre,URI).

/*
 * initialise_prefix_db(+Collection) is det.
 * 
 * Set up default prefixes for a new collection. 
 * The semantics of this is quite different from initialise_prefix_db/2, 
 * because it doesn't cause a re-attache of the database. Perhaps we 
 * should use a different name?
 */
initialise_prefix_db(C) :-
    retractall_prefix(C,_,_),
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
    once(file_search_path(terminus_home,BasePath)),
    interpolate([BasePath,'/',storage,'/','prefix.db'],File),
    (   \+ exists_file(File)
        % create the file
    ->  touch(File),
        db_attach(File, []),
        terminus_database_name(Terminus_Name),
        initialise_prefix_db(Terminus_Name), 
        % Add default prefixes to all collections
        % (prefixes are collection dependent)
        database_name_list(Names),
        forall(
            member(N,Names),
            initialise_prefix_db(N)
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
 * get_collection_prefixes(Collection:atom,Prefixes:dict) is det.
 * 
 * Return a dictionary of prefixes for the current collection. 
 */
get_collection_prefixes(Collection_Id,Prefixes) :-
    get_collection_prefix_pairs(Collection_Id,Prefix_List),
    dict_pairs(Prefixes, _, Prefix_List).

/* 
 * get_collection_prefix_pairs(Collection:atom,Prefixes:dict) is det.
 * 
 * return a list of pairs of prefixes.
 */
get_collection_prefix_pairs(Collection,List) :-
    findall(Prefix-Uri,
            prefix(Collection, Prefix, Uri),
            List).

/* 
 * get_collection_prefix_list(Collection:atom,Prefixes:dict) is det.
 * 
 * return a list of pairs of prefixes.
 */
get_collection_prefix_list(Collection,List) :-
    get_collection_prefix_pairs(Collection,Pairs),
    maplist([A-B,A=B]>>true, Pairs, List).

/* 
 * prefix_list_to_rapper_args(Collection:atom,Prefixes:dict) is det.
 * 
 * return a list of arguments for rapper.
 */
prefix_list_to_rapper_args([],[]).
prefix_list_to_rapper_args([P=U|Rest],['-f',Arg|Arg_Rest]) :-
    interpolate(['xmlns:',P,'="',U,'"'],Arg),
    prefix_list_to_rapper_args(Rest,Arg_Rest).

:- dynamic woql_context/1.

initialise_contexts :-
    terminus_schema_path(Path),
    interpolate([Path,'woql-context.jsonld'],File),
    setup_call_cleanup(
        open(File,read,Out),
        (   retractall(woql_context(_)),
            json_read_dict(Out, Doc),
            get_dict_default('@context',Doc, Ctx,_{}),
            expand_context(Ctx, Exp),
            assertz(woql_context(Exp))
        ),
        close(Out)    
    ).


/* 
 * get_collection_jsonld_context(Collection,Ctx) is det. 
 * 
 * Get a JSON-LD dictonary holding the context for this db.
 */
get_collection_jsonld_context(Collection, Ctx) :-
    get_collection_prefix_list(Collection,Ctx_Obj),
    sort(Ctx_Obj,Ctx_Sorted),
    term_jsonld(Ctx_Sorted, Ctx).

/* 
 * get_global_jsonld_context(Collection,Ctx) is det. 
 * 
 * Get a global JSON-LD dictonary holding the context
 * for generic documents.
 */
get_global_jsonld_context(Ctx) :-
    findall(Key-Value,
            global_prefixes(Key,Value),
            Pairs),
    % This is sweeping a bug under the carpet
    % KLUDGE
    sort(Pairs,Sorted),
    dict_create(Ctx,_,Sorted).

