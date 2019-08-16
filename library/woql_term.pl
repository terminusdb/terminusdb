:- module(woql_term,[
              wf/1,
              op(600, xfx, <:),
              op(600, xfx, as),
              op(1050, xfx, =>),
              op(2, xfx, @),
              op(2, xfx, ^^)
          ]).

/** <module> WOQL Term 
 * 
 * Term syntax checking for WOQL.
 *  
 * TODO: We would like to use the metainterpretation method to guess most
 * likely syntax error causes. 
 * 
 * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of TerminusDB.                                      *
 *                                                                       *
 *  TerminusDB is free software: you can redistribute it and/or modify    *
 *  it under the terms of the GNU General Public License as published by *
 *  the Free Software Foundation, either version 3 of the License, or    *
 *  (at your option) any later version.                                  *
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

:- op(2, xfx, :).
:- op(2, xfx, :=).
:- op(600, xfx, <:).
:- op(600, xfx, as).
:- op(1050, xfx, =>).
:- op(2, xfx, @).
:- op(2, xfx, ^^).

/*
 * Variables
 * 
 * V := v(atom)
 */
ovar(v(I)) :-
    atom(I).

/*
 * Rigid Identifiers 
 *
 * R := R / atom | atom
 */ 
rigid_identifier(ID / Suffix) :-
    !,
    rigid_identifier(ID),
    atom(Suffix).
rigid_identifier(ID) :-
    atom(ID).
    
/*
 * Identifiers 
 *
 * I := R | V
 */ 
identifier(ID) :-
    rigid_identifier(ID).
identifier(ID) :-
    ovar(ID).

/* 
 * Class 
 * 
 * C := I
 * 
 * Currently no class expressions
 */ 
class(C) :-
    identifier(C).

/*
 * Objects  
 * 
 * O := I:C | I
 */ 
obj(ID:Class) :-
    identifier(ID),
    class(Class).
obj(ID) :-
    \+ ID = _:_,
    identifier(ID).
obj(ID) :-
    is_dict(ID).
    
/* 
 * Graph 
 * 
 * G := I
 */
graph_term(G) :-
    identifier(G).

/*
 * Literals
 *  
 * L := l(I,string)
 */ 
lit(l(Type,S)) :-
    identifier(Type),
    identifier(S).
lit(S@L) :-
    identifier(S),
    atom(L).
lit(S^^T) :-
    identifier(S),
    identifier(T).

/*
 * OL := O | L
 */ 
obj_or_lit(X) :-
    obj(X).
obj_or_lit(X) :-
    lit(X).

/* 
 * R := obj
 */ 
relationship(R) :-
    obj(R).

/* 
 * AL := 
 *
 */
arg_list([A|Rest]) :-
    obj_or_lit(A),
    arg_list(Rest).
arg_list([]).

/* 
 * 
 * 
 */
value(V) :-
    atom(V).
value(V) :-
    string(V).
value(V) :-
    integer(V).
value(V) :-
    ovar(V).
value(V) :-
    number(V).

/* 
 * Triple Pattern 
 */
triple_pattern(t(X,P,R)) :-
    obj(X),
    obj(P),
    obj_or_lit(R).
triple_pattern(t(X,P,R,G)) :-
    obj(X),
    obj(P),
    obj_or_lit(R),
    graph_term(G).

/*
 * Conjunctive Patterns
 * 
 * K := t(X,R,Y) | q(X,R,Y,G) | K,K | L op L | like(A,B,N) | true
 * op in =, <, >, :, <<
 */
con_pattern(A = B) :-
    value(A),
    value(B).
con_pattern(A < B) :-
    value(A),
    value(B).
con_pattern(A > B) :-
    value(A),
    value(B).
con_pattern(A : B) :-
    obj(A : B).
con_pattern(A << B) :-
    class(A),
    class(B).
con_pattern(like(A,B,F)) :-
    value(A),
    value(B),
    value(F).
con_pattern(p(A,Args)) :-
    atom(A),
    arg_list(Args).
con_pattern(t(X,P,R)) :-
    triple_pattern(t(X,P,R)).
con_pattern(t(X,P,R,G)) :-
    triple_pattern(t(X,P,R,G)).
con_pattern(r(X,P,R)) :-
    obj(X),
    relationship(P),
    obj_or_lit(R).
con_pattern(r(X,P,R,G)) :-
    obj(X),
    relationship(P),
    obj_or_lit(R),
    graph_term(G).
con_pattern(opt(T)) :-
    con_pattern(T).
con_pattern((P,Q)) :-
    con_pattern(P),
    con_pattern(Q).
con_pattern(true).

/*
 * Patterns 
 *
 * P := K | P;P | P,P
 * 
 */
pattern(P) :-
    con_pattern(P).
pattern((P;Q)) :-
    pattern(P),
    pattern(Q).
pattern((P,Q)) :-
    pattern(P),
    pattern(Q).
pattern(not(P)) :-
    pattern(P).

/*
 * variable list
 *  
 * VL := [] | [V|VL]
 */
var_list([]).
var_list([V|VL]) :-
    ovar(V),
    var_list(VL).

/*
 * Select
 *
 * S := select(VL,P)
 */ 
select(VL,P) :-
    var_list(VL),
    pattern(P).

/*
 * all 
 * 
 * A := all(P)
 */
all(P) :-
    %var_list(VL),
    pattern(P).

/*
 * any
 * 
 * E := any(VL,P)
 */
any(VL,P) :-
    var_list(VL),
    pattern(P).

/* 
 * get_spec
 *
 */
get_spec([]).
get_spec([H as V|T]) :-
    atom(H),
    ovar(V),
    get_spec(T).

csv_option([]).

csv_options([]).
csv_options([H|T]) :-
    csv_option(H),
    csv_options(T).

/* 
 * csv file spec.
 */
csv_spec(file(Path,Options)) :-
    atom(Path),
    csv_options(Options).
csv_spec(file(Path)) :-
    atom(Path).

/* 
 * source
 * 
 * S := id | file(id,type)
 */
source(Term) :-
    rigid_identifier(Term).
source(file(Id,Type)) :-
    atom(Id),
    atom(Type).

output_pattern((P,Q)) :-
    output_pattern(P),
    output_pattern(Q).
output_pattern(insert(A,B,Z)) :-
    obj(A),
    obj(B),
    obj_or_lit(Z).
output_pattern(insert(G,A,B,Z)) :-
    obj(A),
    obj(B), 
    obj_or_lit(Z),
    graph_term(G).
output_pattern(delete(A,B,Z)) :-
    obj(A),
    obj(B),
    obj_or_lit(Z).
output_pattern(delete(G,A,B,Z)) :-
    obj(A),
    obj(B), 
    obj_or_lit(Z),
    graph_term(G).
output_pattern(delete_object(URI)) :-
    obj(URI).
output_pattern(update_object(URI,_Doc)) :-
    obj(URI).


/* This entire show needs to be updated to use the 
 * metainterpretation method. 
 */ 

/*
 * Well formed term
 * 
 * WF := S | A | E | let(atom,P,WF)
 */ 
wf(any(VL,P)) :-
    any(VL,P).
wf(all(S)) :-
    all(S).
wf(select(S,T)) :-
    select(S,T).
wf(from(P,S)) :-
    wf(P),
    source(S).
wf(where(S)) :-
    pattern(S).
wf(let(P,Args,Def,S)) :-
    atom(P),
    var_list(Args),
    pattern(Def),
    wf(S).
wf(from(G,S)) :-
    identifier(G),
    wf(S).
wf(depth(N,S)) :-
    integer(N),
    wf(S).
wf(prefixes(PS,S)) :- 
    exclude([P=U]>>(atom(P),atom(U)),PS,[]),
    wf(S).
wf(start(N,S)) :-
    integer(N),
    wf(S).
wf(limit(N,S)) :-
    integer(N),
    wf(S).
wf(get(Spec,CSV)) :-
    get_spec(Spec),
    csv_spec(CSV).
wf(put(Spec,Query,CSV)) :-
    get_spec(Spec),
    pattern(Query),
    csv_spec(CSV).
wf((A,B)) :-
    wf(A),
    wf(B).
wf((A => B)) :-
    wf(A),
    % need more restrictive B
    output_pattern(B).
wf(typecast(A,B,_L,Z)) :-
    obj_or_lit(A),
    rigid_identifier(B),
    % something for _L    
    obj_or_lit(Z).
wf(into(G,S)) :-
    graph_term(G),
    wf(S).
wf(hash(G,HL,v(_V))) :-
    % something for HL
    is_list(HL),
    % nothing for _V?
    graph_term(G).
wf(concat(L,A)) :-
    % Something for L
    is_list(L),
    identifier(A).
wf(trim(S,A)) :-
    identifier(S),
    identifier(A).
wf(not(Q)) :-
    wf(Q).
wf(format(A,B,C)) :-
    obj_or_lit(A),
    atom(B),
    is_list(C).
wf(true).
