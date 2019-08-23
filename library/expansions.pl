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
 *  the Free Software Foundation, either version 3 of the License, or    *
 *  (at your option) any later version.                                  *
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

:- use_module(library(prefixes)).

% xrdf/5
user:goal_expansion(xrdf(DB,G,A,Y,Z),xrdf(DB,G,X,Y,Z)) :-
    \+ var(A),
    global_prefix_expand(A,X).
user:goal_expansion(xrdf(DB,G,X,B,Z),xrdf(DB,G,X,Y,Z)) :-
    \+ var(B),
    global_prefix_expand(B,Y).
user:goal_expansion(xrdf(DB,G,X,Y,C),xrdf(DB,G,X,Y,Z)) :-
    \+ var(C),
    \+ C = literal(_),
    global_prefix_expand(C,Z).
user:goal_expansion(xrdf(DB,G,X,Y,literal(L)),xrdf(DB,G,X,Y,Object)) :-
    \+ var(L),
    literal_expand(literal(L),Object).

% delete/5
user:goal_expansion(delete(DB,G,A,Y,Z),delete(DB,G,X,Y,Z)) :-
    \+ var(A),
    global_prefix_expand(A,X).
user:goal_expansion(delete(DB,G,X,B,Z),delete(DB,G,X,Y,Z)) :-
    \+ var(B),
    global_prefix_expand(B,Y).
user:goal_expansion(delete(DB,G,X,Y,C),delete(DB,G,X,Y,Z)) :-
    \+ var(C),
    \+ C = literal(_),                
    global_prefix_expand(C,Z).
user:goal_expansion(delete(DB,G,X,Y,literal(L)),delete(DB,G,X,Y,Object)) :-
    \+ var(L),
    literal_expand(literal(L),Object).

% insert/5
user:goal_expansion(insert(DB,G,A,Y,Z),insert(DB,G,X,Y,Z)) :-
    \+ var(A),
    global_prefix_expand(A,X).
user:goal_expansion(insert(DB,G,X,B,Z),insert(DB,G,X,Y,Z)) :-
    \+ var(B),
    global_prefix_expand(B,Y).
user:goal_expansion(insert(DB,G,X,Y,C),insert(DB,G,X,Y,Z)) :-
    \+ var(C),
    \+ C = literal(_),        
    global_prefix_expand(C,Z).
user:goal_expansion(insert(DB,G,X,Y,literal(L)),insert(DB,G,X,Y,Object)) :-
    \+ var(L),
    literal_expand(literal(L),Object).

% update/6
user:goal_expansion(update(DB,G,A,Y,Z,Act),update(DB,G,X,Y,Z,Act)) :-
    \+ var(A),
    global_prefix_expand(A,X).
user:goal_expansion(update(DB,G,X,B,Z,Act),update(DB,G,X,Y,Z,Act)) :-
    \+ var(B),
    global_prefix_expand(B,Y).
user:goal_expansion(update(DB,G,X,Y,C,Act),update(DB,G,X,Y,Z,Act)) :-
    \+ var(C),
    \+ C = literal(_),
    global_prefix_expand(C,Z).
user:goal_expansion(update(DB,G,X,Y,literal(L),Act),update(DB,G,X,Y,Object,Act)) :-
    \+ var(L),
    literal_expand(literal(L),Object).

