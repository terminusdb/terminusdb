:- module(global_prefixes, [
              global_prefixes/2,
              global_prefix_expand/2,
              literal_expand/2,
              default_prefixes/1
          ]).

/** <module> Global Prefixes
 *
 * Global prefix management utilities used for goal expansion
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

:- reexport(core(util/syntax)).

% internal
global_prefixes(xdd,'http://terminusdb.com/schema/xdd#').
global_prefixes(vio,'http://terminusdb.com/schema/vio#').
global_prefixes(woql,'http://terminusdb.com/schema/woql#').
global_prefixes(terminus,'http://terminusdb.com/schema/terminus#').
global_prefixes(ref,'http://terminusdb.com/schema/ref#').
global_prefixes(layer,'http://terminusdb.com/schema/layer#').
global_prefixes(repo,'http://terminusdb.com/schema/repository#').
global_prefixes(xsd,'http://www.w3.org/2001/XMLSchema#').
global_prefixes(rdf,'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
global_prefixes(rdfs,'http://www.w3.org/2000/01/rdf-schema#').
global_prefixes(owl,'http://www.w3.org/2002/07/owl#').

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

literal_expand(D^^T, D^^E) :-
    nonvar(T),
    !,
    global_prefix_expand(T,E).
literal_expand(D@L, D@L).
