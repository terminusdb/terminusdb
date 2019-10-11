:- module(skolemise,[
              skolemise/3
          ]).


/** <module> Skolemise
 * 
 * RDF blank nodes create complications for canonical labelling. 
 * This library takes an RDF graph and obtains a canonical labelling. 
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

:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(apply_macros)).
:- use_module(library(triplestore)).

/* 
 * blank_node(+URI) is det.
 */ 
is_blank_node(URI) :-
    atom(URI),
    re_match('^_:.*',URI).

/* 
subject_blank_node(X,DBI,GraphI) :-
    xrdf(DBI,[GraphI],X,_P,_Y),
    is_blank_node(X).

prediate_blank_node(P,DBI,GraphI) :- 
    xrdf(DBI,[GraphI],_X,P,_Y),
    is_blank_node(P).
   */

object_blank_node_triple(t(X,P,Y),DBI,GraphI) :-
    xrdf(DBI,[GraphI],X,P,Y),
    is_blank_node(Y).

object_blank_node_triples(Ts,DBI,GraphI) :-
    findall(T,object_blank_node_triple(T,DBI,GraphI), Ts).

entry_triples(Entry,DBI,GraphI) :-
    object_blank_node_triples(Ts,DBI,GraphI),
    include([t(X,_,_)]>>(\+ is_blank_node(X)), Ts, Entry).

/* 
blank_node(URI,DBI,GraphI) :-
    xrdf(DBI,[GraphI],URI,_P,_Y),
    is_blank_node(URI).
blank_node(URI,DBI,GraphI) :-
    xrdf(DBI,GraphI,_X,URI,_Y),
    is_blank_node(URI).
blank_node(URI,DBI,GraphI) :-
    xrdf(DBI,GraphI,_X,_P,URI),
    is_blank_node(URI).
*/

lookup(Y,Name,[Y-Name|_Rest]) :-
    !.
lookup(Y,Name,[_|Rest]) :-
    lookup(Y,Name,Rest).

bind(Y,Name,[],[Y-Name]).
bind(Y,Name,[Y-_|Rest],[Y-Name|Rest]) :-
    !.
bind(Y,Name,[X-XN|Rest],[X-XN|New]) :-
    bind(Y,Name,Rest,New).


name_of(X,Name,Naming) :-
    is_blank_node(X),
    !,
    (   lookup(X,Name,Naming)
    ->  true
    ;   Name = 'empty').
name_of(X,X,_Naming) :-
    atom(X).
    
new_name(X,P,Y,Naming,New) :-
    name_of(X,XN,Naming),
    name_of(Y,YN,Naming),
    atomic_list_concat([XN,'|',P,'|',YN],Compound),
    md5_hash(Compound,New,[]).
    
name([], [], Naming1, Naming1).
name([t(X,P,Y)|Rest], [t(X,P,Y)|Open], Naming1, Naming3) :-
    is_blank_node(Y), 
    !,
    % '|' is not a legal IRI character
    new_name(X,P,Y,Naming1,New),
    bind(Y,New,Naming1,Naming2),
    name(Rest, Open, Naming2, Naming3).
name([_|Rest], Open, Naming1, Naming2) :-
    % \+ is_blank_node(Y),
    name(Rest, Open, Naming1, Naming2).

/* Naming gives us the SEEN set */
step(Start,Open,Naming1,Naming2,DBI,GraphI) :-
    
    findall(t(Start,P,Y),
            xrdf(DBI, GraphI, Start, P, Y),
            Triples),
    
    name(Triples,OpenTriples,Naming1,Naming2),
    maplist([t(_,_,C),C]>>true,OpenTriples,Open).

traverse([],Naming,Naming,_DBI,_GraphI).
traverse(Starts,Naming1,NamingN,DBI,GraphI) :-

    foldl({DBI,GraphI}/[Start,Open_In-Naming_In,Open_Out-Naming_Out]>>
          (
              step(Start,Open,Naming_In,Naming_Out, DBI, GraphI),
              % diff list? 
              append(Open,Open_In,Open_Out)
          ),
          Starts,[]-Naming1,OpenList-Naming2),

    sort(OpenList,Open),
    traverse(Open,Naming2,NamingN,DBI,GraphI).



/* 
 * skolemise(+DBI,+GraphI,-Naming) is det.
 * 
 * Finds a skolemisation for the given graph represented 
 * as an associative list of namings from blank node to 
 * Skolem name.
 */
skolemise(DBI,GraphI,Naming) :-
    object_blank_node_triples(Triples,DBI,GraphI),
    name(Triples,OpenTriples,[],Naming1),

    %format('Naming: ~q~n',[Naming1]),

    %format('OpenTriples: ~q~n',[OpenTriples]),

    sort(OpenTriples,SortedTriples),
    maplist([t(_,_,C),C]>>true,SortedTriples,Open),

    %format('Open: ~q~n',[Open]),
    
    traverse(Open,Naming1,Naming,DBI,GraphI).
