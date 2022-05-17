:- module(global_prefixes, [
              global_prefixes/2,
              global_prefix_expand/2,
              global_prefix_expand_safe/2,
              literal_expand/2,
              default_prefixes/1,
              prefix_list/2
          ]).

/** <module> Global Prefixes
 **/

:- reexport(core(util/syntax)).

:- use_module(library(apply)).

% internal
global_prefixes(sys,'http://terminusdb.com/schema/sys#').
global_prefixes(xdd,'http://terminusdb.com/schema/xdd#').
global_prefixes(vio,'http://terminusdb.com/schema/vio#').
global_prefixes(woql,'http://terminusdb.com/schema/woql#').
global_prefixes(xsd,'http://www.w3.org/2001/XMLSchema#').
global_prefixes(rdf,'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
global_prefixes(rdfs,'http://www.w3.org/2000/01/rdf-schema#').
global_prefixes(owl,'http://www.w3.org/2002/07/owl#').
global_prefixes(api,'http://terminusdb.com/schema/api#').
global_prefixes(json,'http://terminusdb.com/schema/json#').

/*
 * TODO: Table? Use a dictionary for global_prefixes?
 */
default_prefixes(Defaults) :-
    findall(Prefix-URI,
            global_prefixes(Prefix,URI),
            Data),
    dict_create(Defaults, _, Data).

/*
 * global_prefix_expand(+X:prefixed_uri, -URI:uri) is det.
 */
global_prefix_expand(Prefix:X, URI) :-
    global_prefixes(Prefix,Base),
    atomic_list_concat([Base,X],URI).

global_prefix_expand_safe(Prefix:X, URI) :-
    !,
    global_prefix_expand(Prefix:X, URI).
global_prefix_expand_safe(URI, URI).

literal_expand(D^^T, D^^E) :-
    nonvar(T),
    !,
    global_prefix_expand(T,E).
literal_expand(D@L, D@L).

/*
 * Used for goal expansion magic
 */
prefix_list(List, Output) :-
    maplist(global_prefix_expand, List, Output).


