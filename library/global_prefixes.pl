:- module(global_prefixes, [
              global_prefix_expand/2,
              literal_expand/2
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

% internal
global_prefixes(tcs,'http://terminusdb.com/schema/tcs#').
global_prefixes(tbs,'http://terminusdb.com/schema/tbs#').
global_prefixes(xdd,'http://terminusdb.com/schema/xdd#').
global_prefixes(vio,'http://terminusdb.com/schema/vio#').
global_prefixes(terminus,'http://terminusdb.com/schema/terminus#').
% common
global_prefixes(xsd,'http://www.w3.org/2001/XMLSchema#').
global_prefixes(rdf,'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
global_prefixes(rdfs,'http://www.w3.org/2000/01/rdf-schema#').
global_prefixes(owl,'http://www.w3.org/2002/07/owl#').

/* 
 * global_prefix_expand(+X:prefixed_uri, -URI:uri) is det.
 */
global_prefix_expand(Prefix:X, URI) :-
    global_prefixes(Prefix,Base),
    atomic_list_concat([Base,X],URI).

literal_expand(literal(type(T,D)), literal(type(E,D))) :-
    nonvar(T),
    !,
    global_prefix_expand(T,E).
literal_expand(literal(lang(L,D)), literal(lang(L,D))).
