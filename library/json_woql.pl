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

:- use_module(utils).
:- use_module(jsonld).

:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(apply_macros)).

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
    (   _{'http://terminusdb.com/woql#comment' : _} :< JSON
    ->  WOQL = true
    ;   _{'http://terminusdb.com/woql#select' : Clauses } :< JSON
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
    ;   _{'http://terminusdb.com/woql#into' : [ Graph, Query ] } :< JSON
    ->  json_to_woql_ast(Graph,WG),
        json_to_woql_ast(Query,WQ),
        WOQL = into(WG,WQ)
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
    ;   _{'http://terminusdb.com/woql#substring' : [String, Before, Length, After, Substring] } :< JSON
    ->  json_to_woql_ast(String, WString),
        json_to_woql_ast(Before, WBefore),
        json_to_woql_ast(Length, WLength),
        json_to_woql_ast(After, WAfter),
        json_to_woql_ast(Substring, WSubstring),
        WOQL = sub_string(WString,WBefore,WLength,WAfter,WSubstring)
    ;   _{'http://terminusdb.com/woql#update' : [ Doc ] } :< JSON
    ->  WOQL = update_object(Doc)
    ;   _{'http://terminusdb.com/woql#delete' : [ Doc ] } :< JSON
    ->  (   _{'@id' : ID} :< Doc
        ->   WOQL = delete_object(ID)
        ;   throw(http_reply(not_found(_{'@type' : 'vio:WOQLSyntaxError',
                                         'terminus:message' :'No ID specified in deleted object',
                                         'vio:query' : JSON})))
        )
    ;   _{'http://terminusdb.com/woql#with' : [ Graph, Path, Query] } :< JSON
    ->  json_to_woql_ast(Graph,WGraph),
        json_to_woql_ast(Path,WPath),
        json_to_woql_ast(Query,WQuery),
        WOQL = with(WGraph,WPath,WQuery)
    ;   _{'http://terminusdb.com/woql#add_triple' : [ Subject, Predicate, Object ] } :< JSON
    ->  json_to_woql_ast(Subject,WQA),
        json_to_woql_ast(Predicate,WQB),
        json_to_woql_ast(Object,WQC),
        WOQL = insert(WQA,WQB,WQC)
    ;   _{'http://terminusdb.com/woql#add_quad' : [ Subject, Predicate, Object, Graph ] } :< JSON
    ->  json_to_woql_ast(Graph,WG),
        json_to_woql_ast(Subject,WQA),
        json_to_woql_ast(Predicate,WQB),
        json_to_woql_ast(Object,WQC),
        WOQL = insert(WG,WQA,WQB,WQC)
    ;   _{'http://terminusdb.com/woql#delete_triple' : [ Subject, Predicate, Object ] } :< JSON
    ->  json_to_woql_ast(Subject,WQA),
        json_to_woql_ast(Predicate,WQB),
        json_to_woql_ast(Object,WQC),
        WOQL = delete(WQA,WQB,WQC)
    ;   _{'http://terminusdb.com/woql#delete_quad' : [ Subject, Predicate, Object, Graph ] } :< JSON
    ->  json_to_woql_ast(Graph,WG),
        json_to_woql_ast(Subject,WQA),
        json_to_woql_ast(Predicate,WQB),
        json_to_woql_ast(Object,WQC),
        WOQL = delete(WG,WQA,WQB,WQC)
    ;   _{'http://terminusdb.com/woql#when' : [ Q, U ] } :< JSON
    ->  json_to_woql_ast(Q,WQ),
        json_to_woql_ast(U,WU),
        WOQL = '=>'(WQ,WU)
    ;   _{'http://terminusdb.com/woql#trim' : [ A, T ] } :< JSON
    ->  json_to_woql_ast(A,WA),
        json_to_woql_ast(T,WT),
        WOQL = trim(WA,WT)
    ;   _{'http://terminusdb.com/woql#eval' : [ A, X ] } :< JSON
    ->  json_to_woql_arith(X, V),
        json_to_woql_arith(A, Arith),
        WOQL = 'is'(V,Arith)
    ;   _{'http://terminusdb.com/woql#isa' : [ X, A ] } :< JSON
    ->  json_to_woql_arith(X, V),
        json_to_woql_arith(A, Arith),
        WOQL = ':'(V,Arith)
    ;   _{'http://terminusdb.com/woql#like' : [ A, B, F ] } :< JSON
    ->  json_to_woql_ast(A,WA),
        json_to_woql_ast(B,WB),
        json_to_woql_ast(F,WF),
        WOQL = like(WA,WB,WF)
    ;   _{'http://terminusdb.com/woql#less' : [ A, B ] } :< JSON
    ->  json_to_woql_ast(A,WA),
        json_to_woql_ast(B,WB),
        WOQL = '<'(WA,WB)
    ;   _{'http://terminusdb.com/woql#greater' : [ A, B ] } :< JSON
    ->  json_to_woql_ast(A,WA),
        json_to_woql_ast(B,WB),
        WOQL = '>'(WA,WB)
    ;   _{'http://terminusdb.com/woql#opt' : [ Q ] } :< JSON
    ->  json_to_woql_ast(Q,WQ),
        WOQL = 'opt'(WQ)
    ;   _{'http://terminusdb.com/woql#get' : [ Header, File ] } :< JSON
    ->  maplist(json_to_woql_ast,Header,WHeader),
        json_to_woql_ast(File,WFile),
        WOQL = get(WHeader,WFile)
    ;   _{'http://terminusdb.com/woql#put' : [ Header, Query, File ] } :< JSON
    ->  maplist(json_to_woql_ast,Header,WHeader),
        json_to_woql_ast(Query,WQuery),
        json_to_woql_ast(File,WFile),
        WOQL = put(WHeader,WQuery,WFile)
    ;   _{'http://terminusdb.com/woql#remote' : [ File ] } :< JSON
    ->  WOQL = remote(File)
    ;   _{'http://terminusdb.com/woql#remote' : [ File, Dict] } :< JSON
    ->  WOQL = remote(File,Dict)
    ;   _{'http://terminusdb.com/woql#file' : [ File ] } :< JSON
    ->  WOQL = file(File)
    ;   _{'http://terminusdb.com/woql#file' : [ File, Dict] } :< JSON
    ->  WOQL = file(File,Dict)
    ;   _{'http://terminusdb.com/woql#post' : [ File ] } :< JSON
    ->  WOQL = post(File)
    ;   _{'http://terminusdb.com/woql#post' : [ File, Dict] } :< JSON
    ->  WOQL = post(File,Dict)
    ;   _{'http://terminusdb.com/woql#unique' : [ Base, Q, Hash] } :< JSON
    ->  json_to_woql_ast(Base,WBase),
        json_to_woql_ast(Q,WQ),
        json_to_woql_ast(Hash,WHash),
        WOQL = hash(WBase,WQ,WHash)
    ;   _{'http://terminusdb.com/woql#idgen' : [ Base, Q, Val ] } :< JSON
    ->  json_to_woql_ast(Base,WBase),
        json_to_woql_ast(Q,WQ),
        json_to_woql_ast(Val,WVal),
        WOQL = idgen(WBase,WQ,WVal)
    ;   _{'http://terminusdb.com/woql#idgen' : [ Base, Q, Code, Val] } :< JSON,
        _{'@id' : 'http://terminusdb.com/woql#concat'} :< Code
    ->  json_to_woql_ast(Base,WBase),
        json_to_woql_ast(Q,WQ),
        json_to_woql_ast(Val,WVal),
        WOQL = idgen(WBase,WQ,WVal)
    ;   _{'http://terminusdb.com/woql#idgen' : [ Base, Q, Code, Val] } :< JSON,
        _{'@id' : 'http://terminusdb.com/woql#hash'} :< Code
    ->  json_to_woql_ast(Base,WBase),
        json_to_woql_ast(Q,WQ),
        json_to_woql_ast(Val,WVal),
        WOQL = hash(WBase,WQ,WVal)
    ;   _{'http://terminusdb.com/woql#upper' : [ S, V ] } :< JSON
    ->  json_to_woql_ast(S,WS),
        json_to_woql_ast(V,WV),
        WOQL = upper(WS,WV)
    ;   _{'http://terminusdb.com/woql#lower' : [ S, V ] } :< JSON
    ->  json_to_woql_ast(S,WS),
        json_to_woql_ast(V,WV),
        WOQL = lower(WS,WV)
    ;   _{'http://terminusdb.com/woql#pad' : [ S, C, N, V ] } :< JSON
    ->  json_to_woql_ast(S,WS),
        json_to_woql_ast(C,WC),
        json_to_woql_ast(N,WN),
        json_to_woql_ast(V,WV),
        WOQL = pad(WS,WC,WN,WV)
    ;   _{'http://terminusdb.com/woql#split' : [ S, P, L ] } :< JSON
    ->  json_to_woql_ast(S,WS),
        json_to_woql_ast(P,WP),
        json_to_woql_ast(L,WL),
        WOQL = split(WS,WP,WL)
    ;   _{'http://terminusdb.com/woql#member' : [ S, L ] } :< JSON
    ->  json_to_woql_ast(S,WS),
        json_to_woql_ast(L,WL),
        WOQL = member(WS,WL)
    ;   _{'http://terminusdb.com/woql#concat' : [ List, Value ] } :< JSON
    ->  json_to_woql_ast(List,WList),
        json_to_woql_ast(Value,WValue),
        WOQL = concat(WList,WValue)
    ;   _{'http://terminusdb.com/woql#join' : [ List, Sep, Value ] } :< JSON
    ->  json_to_woql_ast(List,WList),
        json_to_woql_ast(Sep,WSep),
        json_to_woql_ast(Value,WValue),
        WOQL = join(WList,WSep,WValue)
    ;   _{'http://terminusdb.com/woql#sum' : [ List, Value ] } :< JSON
    ->  json_to_woql_ast(List,WList),
        json_to_woql_ast(Value,WValue),
        WOQL = sum(WList,WValue)
    ;   _{'http://terminusdb.com/woql#start' : [ N, Q ] } :< JSON
    ->  json_to_woql_arith(N,WN),
        json_to_woql_ast(Q,WQ),
        WOQL = start(WN, WQ)
    ;   _{'http://terminusdb.com/woql#limit' : [ N, Q ] } :< JSON
    ->  json_to_woql_arith(N,WN),
        json_to_woql_ast(Q,WQ),
        WOQL = limit(WN, WQ)
    ;   _{'http://terminusdb.com/woql#re' : [ Pat, String, List ] } :< JSON
    ->  json_to_woql_ast(Pat,WPat),
        json_to_woql_ast(String,WString),
        json_to_woql_ast(List,WList),
        WOQL = re(WPat, WString, WList)
    ;   _{'http://terminusdb.com/woql#order_by' : [ Templates, Query ] } :< JSON
    ->  maplist([V1,V2]>>(json_to_woql_ast(V1,V2)), Templates, WTemplates),
        json_to_woql_ast(Query,WQuery),
        WOQL = order_by(WTemplates,WQuery)
    ;   _{'http://terminusdb.com/woql#asc' : Vars } :< JSON
    ->  maplist([V1,V2]>>(json_to_woql_ast(V1,V2)),Vars,WVars),
        WOQL = asc(WVars)
    ;   _{'http://terminusdb.com/woql#desc' : Vars } :< JSON
    ->  maplist([V1,V2]>>(json_to_woql_ast(V1,V2)),Vars,WVars),
        WOQL = desc(WVars)
    ;   _{'http://terminusdb.com/woql#group_by' : [Spec,Obj,Query,Collector]} :< JSON
    ->  json_to_woql_ast(Spec,WSpec),
        json_to_woql_ast(Obj,WObj),
        json_to_woql_ast(Query,WQuery),
        json_to_woql_ast(Collector,WCollector),
        WOQL = group_by(WSpec,WObj,WQuery,WCollector)
    ;   _{'http://terminusdb.com/woql#length' : [A,B]} :< JSON
    ->  json_to_woql_ast(A,WA),
        json_to_woql_ast(B,WB),
        WOQL = length(WA,WB)
    ;   _{'http://terminusdb.com/woql#typecast' : [Val,Type,Var]} :< JSON
    ->  json_to_woql_ast(Val,WVal),
        json_to_woql_ast(Type,WType),
        json_to_woql_ast(Var,WVar),
        WOQL = typecast(WVal,WType,[],WVar)
    ;   _{'http://terminusdb.com/woql#list' : Elements } :< JSON
    ->  maplist([V1,V2]>>(json_to_woql_ast(V1,V2)), Elements, WOQL)
    ;   _{'http://terminusdb.com/woql#not' : [ Q ] } :< JSON
    ->  json_to_woql_ast(Q,WQ),
        WOQL = not(WQ)
    ;   _{'http://terminusdb.com/woql#as' : [ S, V ] } :< JSON
    ->  (   _{'@value' : WS} :< S
        ->  atom_string(WA,WS),
            json_to_woql_ast(V,WV),
            WOQL = as(WA,WV)
        ;   throw(http_reply(not_found(_{'@type' : 'vio:WOQLSyntaxError',
                                         'terminus:message' :'No source column specified',
                                         'vio:query' : JSON}))))
    ;   _{'http://terminusdb.com/woql#as' : [ S, V, T ] } :< JSON
    ->  (   _{'@value' : WS} :< S
        ->  atom_string(WA,WS),
            json_to_woql_ast(V,WV),
            json_to_woql_ast(T,WT),
            WOQL = as(WA,WV,WT)
        ;   throw(http_reply(not_found(_{'@type' : 'vio:WOQLSyntaxError',
                                         'terminus:message' :'No source column specified',
                                         'vio:query' : JSON}))))
    ;   _{'http://terminusdb.com/woql#as' : [ V ] } :< JSON
    ->  json_to_woql_ast(V,WOQL)
    ;   _{'http://terminusdb.com/woql#true' : [] } :< JSON
    ->  WOQL = true
    ;   _{'@value' : V, '@type' : T } :< JSON
    ->  atom_string(TE,T),
        WOQL = '^^'(V,TE)
    ;   _{'@value' : V, '@language' : L } :< JSON
    ->  atom_string(LE,L),
        WOQL = '@'(V,LE)
    ;   _{'http://terminusdb.com/woql#value' : V, '@type' : T } :< JSON
    ->  json_to_woql_ast(V,VE),
        atom_string(TE,T),
        WOQL = '^^'(VE,TE)
    ;   _{'http://terminusdb.com/woql#value' : V, '@language' : L } :< JSON
    ->  json_to_woql_ast(V,VE),
        atom_string(LE,L),
        WOQL = '@'(VE,LE)
    ;   _{'@id' : ID } :< JSON
    ->  json_to_woql_ast(ID,WOQL)
    ;   true = JSON
    ->  WOQL = true
    ;   throw(http_reply(not_found(_{'@type' : 'vio:WOQLSyntaxError',
                                     'terminus:message' :'Un-parsable Query',
                                     'vio:query' : JSON})))
    ).
json_to_woql_ast(JSON,WOQL) :-
    number(JSON),
    !,
    WOQL = '^^'(JSON,'http://www.w3.org/2001/XMLSchema#decimal').
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
    sub_atom(A, 0, _, _, 'http://terminusdb.com/woql/variable/').

json_to_woql_arith(JSON,WOQL) :-
    is_dict(JSON),
    !,
    (   _{'http://terminusdb.com/woql#plus' : Args } :< JSON
    ->  maplist(json_to_woql_arith,Args,WOQL_Args),
        yfx_list('+', WOQL, WOQL_Args)
    ;   _{'http://terminusdb.com/woql#minus' : Args } :< JSON
    ->  maplist(json_to_woql_arith,Args,WOQL_Args),
        yfx_list('-', WOQL, WOQL_Args)
    ;   _{'http://terminusdb.com/woql#times' : Args } :< JSON
    ->  maplist(json_to_woql_arith,Args,WOQL_Args),
        yfx_list('*', WOQL, WOQL_Args)
    ;   _{'http://terminusdb.com/woql#divide' : Args } :< JSON
    ->  maplist(json_to_woql_arith,Args,WOQL_Args),
        yfx_list('/', WOQL, WOQL_Args)
    ;   _{'http://terminusdb.com/woql#div' : Args } :< JSON
    ->  maplist(json_to_woql_arith,Args,WOQL_Args),
        yfx_list('div', WOQL, WOQL_Args)
    ;   _{'http://terminusdb.com/woql#exp' : Args } :< JSON
    ->  maplist(json_to_woql_arith,Args,WOQL_Args),
        yfx_list('**', WOQL, WOQL_Args)
    ;   _{'http://terminusdb.com/woql#floor' : [X] } :< JSON
    ->  json_to_woql_arith(X,WOQL_X),
        WOQL=floor(WOQL_X)
    ;   _{'@id': Var } :< JSON
    ->  coerce_atom(Var,A),
        is_json_var(A),
        WOQL = v(A)
    ;   number(JSON)
    ->  WOQL = JSON
    ;   throw(http_reply(not_found(_{'terminus:message' : 'Unknown Syntax',
                                     'vio:query' : JSON,
                                     'terminus:status' : 'terminus:failure'})))
    ).
json_to_woql_arith(JSON,JSON) :-
    number(JSON).
