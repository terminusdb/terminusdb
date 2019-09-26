:- module(json_woql,[
              json_woql/2,
              json_woql/3
          ]).

/** <module> WOQL JSON-LD syntax
 * 
 * This file contains the code for conversion of JSON-LD to the WOQL 
 * AST.
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

:- use_module(library(utils)).
:- use_module(library(jsonld)).
 
json_woql(JSON,WOQL) :-
    json_woql(JSON,_{},WOQL).

json_woql(JSON,Ctx,WOQL) :-
    expand(JSON,Ctx,JSON_Ex),
    json_to_woql_ast(JSON_Ex,WOQL).

/* 
 * json_to_woql_ast(+JSON:dict,-AST:any) is det. 
 * 
 * Translate a JSON-LD WOQL statement into the AST. 
 */ 
json_to_woql_ast(JSON,WOQL) :-
    is_dict(JSON),
    !,
    (   _{'http://terminusdb.com/woql#select' : Clauses } :< JSON
    ->  snoc(Vargs,Sub_Query,Clauses),
        maplist([V1,V2]>>(json_to_woql_ast(V1,V2)),Vargs,WOQL_Args),
        json_to_woql_ast(Sub_Query,Sub_WOQL),
        WOQL = select(WOQL_Args,Sub_WOQL)
    ;   _{'http://terminusdb.com/woql#and' : Qs} :< JSON
    ->  maplist(json_to_woql_ast,Qs,W_Qs),
        xfy_list(',',WOQL,W_Qs)
    ;   _{'http://terminusdb.com/woql#or' : Qs} :< JSON
    ->  maplist(json_to_woql_ast,Qs,W_Qs),
        xfy_list(';',WOQL,W_Qs)
    ;   _{'http://terminusdb.com/woql#from' : [ Graph, Query ] } :< JSON
    ->  json_to_woql_ast(Graph,WG),
        json_to_woql_ast(Query,WQ),
        WOQL = from(WG,WQ)
    ;   _{'http://terminusdb.com/woql#quad' : [ Graph, Subject, Predicate, Object] } :< JSON
    ->  json_to_woql_ast(Graph,WG),
        json_to_woql_ast(Subject,WQA),
        json_to_woql_ast(Predicate,WQB),
        json_to_woql_ast(Object,WQC),
        WOQL = t(WG,WQA,WQB,WQC)
    ;   _{'http://terminusdb.com/woql#triple' : [ Subject, Predicate, Object] } :< JSON
    ->  json_to_woql_ast(Subject,WQA),
        json_to_woql_ast(Predicate,WQB),
        json_to_woql_ast(Object,WQC),
        WOQL = t(WQA,WQB,WQC)
    ;   _{'http://terminusdb.com/woql#sub' : [ Class_A, Class_B ] } :< JSON
    ->  json_to_woql_ast(Class_A,WQA),
        json_to_woql_ast(Class_B,WQB),
        WOQL = '<<'(WQA,WQB)
    ;   _{'http://terminusdb.com/woql#eq' : [ A, B ] } :< JSON
    ->  json_to_woql_ast(A,WQA),
        json_to_woql_ast(B,WQB),
        WOQL = '='(WQA,WQB)
    ;   _{'http://terminusdb.com/woql#update' : [ Doc ] } :< JSON
    ->  WOQL = update(Doc)
    ;   _{'http://terminusdb.com/woql#delete' : [ Doc ] } :< JSON
    ->  WOQL = delete(Doc)
    ;   _{'http://terminusdb.com/woql#-triple' : [ Subject, Predicate, Object ] } :< JSON
    ->  json_to_woql_ast(Subject,WQA),
        json_to_woql_ast(Predicate,WQB),
        json_to_woql_ast(Object,WQC),
        WOQL = delete(WQA,WQB,WQC)
    ;   _{'http://terminusdb.com/woql#+triple' : [ Subject, Predicate, Object ] } :< JSON
    ->  json_to_woql_ast(Subject,WQA),
        json_to_woql_ast(Predicate,WQB),
        json_to_woql_ast(Object,WQC),
        WOQL = insert(WQA,WQB,WQC)
    ;   _{'http://terminusdb.com/woql#-quad' : [ Graph, Subject, Predicate, Object ] } :< JSON
    ->  json_to_woql_ast(Graph,WG),
        json_to_woql_ast(Subject,WQA),
        json_to_woql_ast(Predicate,WQB),
        json_to_woql_ast(Object,WQC),
        WOQL = delete(WG,WQA,WQB,WQC)
    ;   _{'http://terminusdb.com/woql#+quad' : [ Graph, Subject, Predicate, Object ] } :< JSON
    ->  json_to_woql_ast(Graph,WG),
        json_to_woql_ast(Subject,WQA),
        json_to_woql_ast(Predicate,WQB),
        json_to_woql_ast(Object,WQC),
        WOQL = insert(WG,WQA,WQB,WQC)
    ;   _{'http://terminusdb.com/woql#modifies' : [ Q, U ] } :< JSON
    ->  json_to_woql_ast(Q,WQ),
        json_to_woql_ast(U,WU),
        WOQL = '=>'(WQ,WU)
    ;   _{'http://terminusdb.com/woql#eval' : [ A, X ] } :< JSON
    ->  is_json_var(X),
        json_to_woql_arith(A, Arith),
        WOQL = 'is'(v(X),Arith)
    ;   _{'http://terminusdb.com/woql#isa' : [ X, A ] } :< JSON
    ->  is_json_var(X),
        json_to_woql_arith(A, Arith),
        WOQL = ':'(v(X),Arith)
    ;   _{'http://terminusdb.com/woql#like' : [ A, B, F ] } :< JSON
    ->  json_to_woql_ast(A,WA),
        json_to_woql_ast(B,WB),
        json_to_woql_ast(F,WF),
        WOQL = like(WA,WB,WF)
    ;   _{'http://terminusdb.com/woql#less' : [ A, B ] } :< JSON
    ->  json_to_woql_arith(A,WA),
        json_to_woql_arith(B,WB),
        WOQL = '<'(WA,WB)
    ;   _{'http://terminusdb.com/woql#greater' : [ A, B ] } :< JSON
    ->  json_to_woql_arith(A,WA),
        json_to_woql_arith(B,WB),
        WOQL = '>'(WA,WB)
    ;   _{'http://terminusdb.com/woql#opt' : [ Q ] } :< JSON
    ->  json_to_woql_ast(Q,WQ),
        WOQL = 'opt'(WQ)
    ;   _{'http://terminusdb.com/woql#hash' : [ Base, Q, Hash ] } :< JSON
    ->  json_to_woql_ast(Base,WBase),
        json_to_woql_ast(Q,WQ),
        json_to_woql_ast(Hash,WHash),        
        WOQL = hash(WBase,WQ,WHash)
    ;   _{'http://terminusdb.com/woql#start' : [ N, Q ] } :< JSON
    ->  json_to_woql_arith(N,WN),
        json_to_woql_ast(Q,WQ),
        WOQL = start(WN, WQ)
    ;   _{'http://terminusdb.com/woql#limit' : [ N, Q ] } :< JSON
    ->  json_to_woql_arith(N,WN),
        json_to_woql_ast(Q,WQ),
        WOQL = limit(WN, WQ)
    ;   _{'http://terminusdb.com/woql#not' : [ Q ] } :< JSON
    ->  json_to_woql_ast(Q,WQ),
        WOQL = not(WQ)
    ;   _{'@value' : V, '@type' : T } :< JSON
    ->  WOQL = '^^'(V,T)
    ;   _{'@value' : V, '@lang' : L } :< JSON
    ->  WOQL = '@'(V,L)
    ;   _{'@id' : ID } :< JSON
    ->  json_to_woql_ast(ID,WOQL)
    ;   throw(http_reply(not_found(_{'@type' : 'vio:WOQLSyntaxError',
                                     'terminus:message' :'Un-parsable Query',
                                     'vio:query' : JSON})))
    ).
json_to_woql_ast(JSON,WOQL) :-
    coerce_atom(JSON,A),
    !,    
    (   is_json_var(A)
    ->  WOQL = v(A)
    ;   A = WOQL
    ).
json_to_woql_ast(JSON,_) :-
    format(atom(Msg), 'Un-parsable Query: ~q', [JSON]),
    throw(http_reply(not_found(_{'terminus:message' : Msg,
                                 'vio:query' : JSON,
                                 'terminus:status' : 'terminus:failure'}))).

is_json_var(A) :-
    atom_concat('http://terminusdb.com/woql/variable/',_,A).

json_to_woql_arith(JSON,WOQL) :-
    is_dict(JSON),
    !,
    (   _{'http://terminusdb.com/woql#plus' : Args } :< JSON
    ->  maplist(json_to_woql_arith,Args,WOQL_Args),
        xfy_list('+', WOQL, WOQL_Args)
    ;   _{'http://terminusdb.com/woql#minus' : Args } :< JSON
    ->  maplist(json_to_woql_arith,Args,WOQL_Args),
        xfy_list('-', WOQL, WOQL_Args)
    ;   _{'http://terminusdb.com/woql#times' : Args } :< JSON
    ->  maplist(json_to_woql_arith,Args,WOQL_Args),
        xfy_list('*', WOQL, WOQL_Args)
    ;   _{'http://terminusdb.com/woql#divide' : Args } :< JSON
    ->  maplist(json_to_woql_arith,Args,WOQL_Args),
        xfy_list('/', WOQL, WOQL_Args)
    ;   is_json_var(JSON),
        WOQL = v(JSON)
    ;   throw(http_reply(not_found('Unknown Syntax:'-JSON)))
    ).
json_to_woql_arith(JSON,JSON) :-
    number(JSON).

    

