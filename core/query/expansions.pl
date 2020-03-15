:- module(expansions, []).

/** <module> Expansions
 *
 * Goal expansions required by some modules.
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

:- use_module(global_prefixes).

:- reexport(core(util/syntax)).

:- multifile user:goal_expansion/2.
:- dynamic user:goal_expansion/2.

% xrdf/5
user:goal_expansion(xrdf(G,A,Y,Z),xrdf(G,X,Y,Z)) :-
    nonvar(A),
    global_prefix_expand(A,X).
user:goal_expansion(xrdf(G,X,B,Z),xrdf(G,X,Y,Z)) :-
    nonvar(B),
    global_prefix_expand(B,Y).
user:goal_expansion(xrdf(G,X,Y,C),xrdf(G,X,Y,Z)) :-
    nonvar(C),
    C \= _@_,
    C \= _^^_,
    global_prefix_expand(C,Z).
user:goal_expansion(xrdf(G,X,Y,L),xrdf(G,X,Y,Object)) :-
    nonvar(L),
    once(L = _@_ ; L = _^^_),
    literal_expand(L,Object).

% delete/5
user:goal_expansion(delete(G,A,Y,Z),delete(G,X,Y,Z)) :-
    nonvar(A),
    global_prefix_expand(A,X).
user:goal_expansion(delete(G,X,B,Z),delete(G,X,Y,Z)) :-
    nonvar(B),
    global_prefix_expand(B,Y).
user:goal_expansion(delete(G,X,Y,C),delete(G,X,Y,Z)) :-
    nonvar(C),
    C \= _@_,
    C \= _^^_,
    global_prefix_expand(C,Z).
user:goal_expansion(delete(G,X,Y,L),delete(G,X,Y,Object)) :-
    nonvar(L),
    once(L = _@_ ; L = _^^_),
    literal_expand(L,Object).

% insert/5
user:goal_expansion(insert(G,A,Y,Z),insert(G,X,Y,Z)) :-
    nonvar(A),
    global_prefix_expand(A,X).
user:goal_expansion(insert(G,X,B,Z),insert(G,X,Y,Z)) :-
    nonvar(B),
    global_prefix_expand(B,Y).
user:goal_expansion(insert(G,X,Y,C),insert(G,X,Y,Z)) :-
    nonvar(C),
    C \= _@_,
    C \= _^^_,
    global_prefix_expand(C,Z).
user:goal_expansion(insert(G,X,Y,L),insert(G,X,Y,Object)) :-
    nonvar(L),
    once(L = _@_ ; L = _^^_),
    literal_expand(L,Object).

% update/6
user:goal_expansion(update(G,A,Y,Z,Act),update(G,X,Y,Z,Act)) :-
    nonvar(A),
    global_prefix_expand(A,X).
user:goal_expansion(update(G,X,B,Z,Act),update(G,X,Y,Z,Act)) :-
    nonvar(B),
    global_prefix_expand(B,Y).
user:goal_expansion(update(G,X,Y,C,Act),update(G,X,Y,Z,Act)) :-
    nonvar(C),
    C \= _@_,
    C \= _^^_,
    global_prefix_expand(C,Z).
user:goal_expansion(update(G,X,Y,L,Act),update(G,X,Y,Object,Act)) :-
    nonvar(L),
    once(L = _@_ ; L = _^^_),
    literal_expand(L,Object).
