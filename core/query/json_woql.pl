:- module(json_woql,[
              woql_context/1,
              initialise_woql_contexts/0,
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

:- use_module(jsonld).

:- use_module(core(util)).
:- reexport(core(util/syntax)).

:- use_module(library(http/json)).

:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(apply_macros)).

:- dynamic woql_context/1.
initialise_woql_contexts :-
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

json_woql(JSON,WOQL) :-
    json_woql(JSON,_{},WOQL).

json_woql(JSON,Ctx,WOQL) :-
    expand(JSON,Ctx,JSON_Ex),
    json_to_woql_ast(JSON_Ex,WOQL).

/*
 * deindex_list(Key,List,List) is det.
 *
 * Remove the indexing from a list so that it is by order.
 */
deindex_list(Key,List,Flatten) :-
    maplist({Key}/[Elt,Sub_Elt]>>(
                get_dict(Key, Elt, Sub_Elt)
            ), List, Flatten).

/*
 * json_to_woql_ast(+JSON:dict,-AST:any) is det.
 *
 * Translate a JSON-LD WOQL statement into the AST.
 */
json_to_woql_ast(JSON,WOQL) :-
    is_dict(JSON),
    !,
    (   _{'@type' : 'http://terminusdb.com/schema/woql#Comment'} :< JSON
    ->  WOQL = true
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Select',
          'http://terminusdb.com/schema/woql#variable_list' : Indexed_Variables,
          'http://terminusdb.com/schema/woql#query' : Sub_Query } :< JSON
    ->  deindex_list('http://terminusdb.com/schema/woql#variable_name',
                     Indexed_Variables,
                     Variables),
        maplist([V,v(V)]>>true, Variables, WOQL_Args),
        json_to_woql_ast(Sub_Query,Sub_WOQL),
        WOQL = select(WOQL_Args,Sub_WOQL)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#And',
          'http://terminusdb.com/schema/woql#query_list' : Indexed_Query_List} :< JSON
    ->  deindex_list('http://terminusdb.com/schema/woql#query',
                     Indexed_Query_List, Query_List),
        maplist(json_to_woql_ast, Query_List, WOQL_Queries),
        xfy_list(',',WOQL,WOQL_Queries)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Or',
          'http://terminusdb.com/schema/woql#query_list' : Indexed_Query_List} :< JSON
    ->  deindex_list('http://terminusdb.com/schema/woql#query',
                     Indexed_Query_List, Query_List),
        maplist(json_to_woql_ast,Query_List,WOQL_Queries),
        xfy_list(';',WOQL,WOQL_Queries)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Using',
          'http://terminusdb.com/schema/woql#collection' : Collection,
          'http://terminusdb.com/schema/woql#query' : Query } :< JSON
    ->  json_to_woql_ast(Collection,WC),
        json_to_woql_ast(Query,WQ),
        WOQL = using(WC,WQ)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#From',
          'http://terminusdb.com/schema/woql#graph_filter' : Graph_Filter,
          'http://terminusdb.com/schema/woql#query' : Query } :< JSON
    ->  json_to_woql_ast(Graph_Filter,WG),
        json_to_woql_ast(Query,WQ),
        WOQL = from(WG,WQ)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Into',
          'http://terminusdb.com/schema/woql#graph' : Graph,
          'http://terminusdb.com/schema/woql#query' : Query } :< JSON
    ->  json_to_woql_ast(Graph,WG),
        json_to_woql_ast(Query,WQ),
        WOQL = into(WG,WQ)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Quad',
          'http://terminusdb.com/schema/woql#subject' : Subject,
          'http://terminusdb.com/schema/woql#predicate' : Predicate,
          'http://terminusdb.com/schema/woql#object' : Object,
          'http://terminusdb.com/schema/woql#graph_filter' : Graph
         } :< JSON
    ->  json_to_woql_ast(Subject,WQA),
        json_to_woql_ast(Predicate,WQB),
        json_to_woql_ast(Object,WQC),
        json_to_woql_ast(Graph,WG),
        WOQL = t(WQA,WQB,WQC,WG)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Triple',
          'http://terminusdb.com/schema/woql#subject' : Subject,
          'http://terminusdb.com/schema/woql#predicate' : Predicate,
          'http://terminusdb.com/schema/woql#object' : Object
         } :< JSON
    ->  json_to_woql_ast(Subject,WQA),
        json_to_woql_ast(Predicate,WQB),
        json_to_woql_ast(Object,WQC),
        WOQL = t(WQA,WQB,WQC)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Subsumption',
          'http://terminusdb.com/schema/woql#child' : Class_A,
          'http://terminusdb.com/schema/woql#parent' : Class_B
         } :< JSON
    ->  json_to_woql_ast(Class_A,WQA),
        json_to_woql_ast(Class_B,WQB),
        WOQL = '<<'(WQA,WQB)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Equals',
          'http://terminusdb.com/schema/woql#left': A,
          'http://terminusdb.com/schema/woql#right' : B
         } :< JSON
    ->  json_to_woql_ast(A,WQA),
        json_to_woql_ast(B,WQB),
        WOQL = '='(WQA,WQB)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Substring',
          'http://terminusdb.com/schema/woql#string' : String,
          'http://terminusdb.com/schema/woql#before' : Before,
          'http://terminusdb.com/schema/woql#length' : Length,
          'http://terminusdb.com/schema/woql#after' After,
          'http://terminusdb.com/schema/woql#substring' : Substring
         } :< JSON
    ->  json_to_woql_ast(String, WString),
        json_to_woql_ast(Before, WBefore),
        json_to_woql_ast(Length, WLength),
        json_to_woql_ast(After, WAfter),
        json_to_woql_ast(Substring, WSubstring),
        WOQL = sub_string(WString,WBefore,WLength,WAfter,WSubstring)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Update',
          'http://terminusdb.com/schema/woql#document' : Doc
         } :< JSON
    ->  WOQL = update_object(Doc)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Delete',
          'http://terminusdb.com/schema/woql#document' : Doc
         } :< JSON
    ->  (   _{'@id' : ID} :< Doc
        ->  WOQL = delete_object(ID)
        ;   throw(http_reply(not_found(_{'@type' : 'vio:WOQLSyntaxError',
                                         'terminus:message' :'No ID specified in deleted object',
                                         'vio:query' : JSON})))
        )
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#AddTriple',
          'http://terminusdb.com/schema/woql#subject' : Subject,
          'http://terminusdb.com/schema/woql#predicate' : Predicate,
          'http://terminusdb.com/schema/woql#object' : Object
         } :< JSON
    ->  json_to_woql_ast(Subject,WQA),
        json_to_woql_ast(Predicate,WQB),
        json_to_woql_ast(Object,WQC),
        WOQL = insert(WQA,WQB,WQC)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#AddQuad',
          'http://terminusdb.com/schema/woql#subject' : Subject,
          'http://terminusdb.com/schema/woql#predicate' : Predicate,
          'http://terminusdb.com/schema/woql#object' : Object,
          'http://terminusdb.com/schema/woql#graph' : Graph
         } :< JSON
    ->  json_to_woql_ast(Graph,WG),
        json_to_woql_ast(Subject,WQA),
        json_to_woql_ast(Predicate,WQB),
        json_to_woql_ast(Object,WQC),
        WOQL = insert(WG,WQA,WQB,WQC)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#DeleteTriple',
          'http://terminusdb.com/schema/woql#subject' : Subject,
          'http://terminusdb.com/schema/woql#predicate' : Predicate,
          'http://terminusdb.com/schema/woql#object' : Object
         } :< JSON
    ->  json_to_woql_ast(Subject,WQA),
        json_to_woql_ast(Predicate,WQB),
        json_to_woql_ast(Object,WQC),
        WOQL = delete(WQA,WQB,WQC)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#DeleteQuad',
          'http://terminusdb.com/schema/woql#subject' : Subject,
          'http://terminusdb.com/schema/woql#predicate' : Predicate,
          'http://terminusdb.com/schema/woql#object' : Object,
          'http://terminusdb.com/schema/woql#graph' : Graph
         } :< JSON
    ->  json_to_woql_ast(Graph,WG),
        json_to_woql_ast(Subject,WQA),
        json_to_woql_ast(Predicate,WQB),
        json_to_woql_ast(Object,WQC),
        WOQL = delete(WG,WQA,WQB,WQC)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#When',
          'http://terminusdb.com/schema/woql#query' : Q,
          'http://terminusdb.com/schema/woql#consequent' : U
         } :< JSON
    ->  json_to_woql_ast(Q,WQ),
        json_to_woql_ast(U,WU),
        WOQL = '=>'(WQ,WU)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Trim',
          'http://terminusdb.com/schema/woql#untrimmed' :  A,
          'http://terminusdb.com/schema/woql#trimmed' : T
         } :< JSON
    ->  json_to_woql_ast(A,WA),
        json_to_woql_ast(T,WT),
        WOQL = trim(WA,WT)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Eval',
          'http://terminusdb.com/schema/woql#expression' : A,
          'http://terminusdb.com/schema/woql#result' : X
         } :< JSON
    ->  json_to_woql_arith(X, V),
        json_to_woql_arith(A, Arith),
        WOQL = 'is'(V,Arith)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#IsA',
          'http://terminusdb.com/schema/woql#element' : X,
          'http://terminusdb.com/schema/woql#of_type' : T
         } :< JSON
    ->  json_to_woql_ast(X, V),
        json_to_woql_ast(T, Class),
        WOQL = isa(V,Class)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Like',
          'http://terminusdb.com/schema/woql#left' :  A,
          'http://terminusdb.com/schema/woql#right' : B,
          'http://terminusdb.com/schema/woql#like_similarity' : F
         } :< JSON
    ->  json_to_woql_ast(A,WA),
        json_to_woql_ast(B,WB),
        json_to_woql_ast(F,WF),
        WOQL = like(WA,WB,WF)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Less',
          'http://terminusdb.com/schema/woql#left' : A,
          'http://terminusdb.com/schema/woql#right' : B
         } :< JSON
    ->  json_to_woql_ast(A,WA),
        json_to_woql_ast(B,WB),
        WOQL = '<'(WA,WB)
    ;   _{'@type' : 'http://terminusdb.com/woql#Greater',
          'http://terminusdb.com/schema/woql#left' : A,
          'http://terminusdb.com/schema/woql#right' : B
         } :< JSON
    ->  json_to_woql_ast(A,WA),
        json_to_woql_ast(B,WB),
        WOQL = '>'(WA,WB)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Optional',
          'http://terminusdb.com/schema/woql#query' : Q
         } :< JSON
    ->  json_to_woql_ast(Q,WQ),
        WOQL = 'opt'(WQ)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Get',
          'http://terminusdb.com/schema/woql#as_vars' : Header,
          'http://terminusdb.com/schema/woql#query_resource' : Resource
         } :< JSON
    ->  deindex_list(Header, Deindexed_Header),
        maplist(json_to_woql_ast,Deindexed_Header,WHeader),
        json_to_woql_ast(Resource,WResource),
        WOQL = get(WHeader,WResource)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Put',
          'http://terminusdb.com/schema/woql#as_vars' : Header,
          'http://terminusdb.com/schema/woql#query' : Query,
          'http://terminusdb.com/schema/woql#resource' : Resource
         } :< JSON
    ->  deindex_list(Header, Deindexed_Header),
        maplist(json_to_woql_ast,Deindexed_Header,WHeader),
        json_to_woql_ast(Query,WQuery),
        json_to_woql_ast(Resource,WResource),
        WOQL = put(WHeader,WQuery,WResource)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#RemoteResource',
          'http://terminusdb.com/schema/woql#remote_uri' : URI
         } :< JSON
    ->  WOQL = remote(URI,JSON)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#FileResource',
          'http://terminusdb.com/schema/woql#file' : File
         } :< JSON
    ->  WOQL = file(File,JSON)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#PostResource' : Post,
          'http://terminusdb.com/schema/woql#file' : File
         } :< JSON
    ->  WOQL = post(File,JSON)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Unique',
          'http://terminusdb.com/schema/woql#base' : Base,
          'http://terminusdb.com/schema/woql#key_list' : Key,
          'http://terminusdb.com/schema/woql#uri' : URI
         } :< JSON
    ->  json_to_woql_ast(Base,WBase),
        json_to_woql_ast(Key,WKey),
        json_to_woql_ast(URI,WURI),
        WOQL = hash(WBase,WKey,WURI)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#IDGenerator',
          'http://terminusdb.com/schema/woql#base' : Base,
          'http://terminusdb.com/schema/woql#key_list' : Key,
          'http://terminusdb.com/schema/woql#uri' : URI
         } :< JSON
    ->  json_to_woql_ast(Base,WBase),
        json_to_woql_ast(Key,WKey),
        json_to_woql_ast(URI,WURI),
        WOQL = idgen(WBase,WKey,WURI)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Upper',
          'http://terminusdb.com/schema/woql#left' :  S,
          'http://terminusdb.com/schema/woql#right' : V
         } :< JSON
    ->  json_to_woql_ast(S,WS),
        json_to_woql_ast(V,WV),
        WOQL = upper(WS,WV)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Lower',
          'http://terminusdb.com/schema/woql#left' : S,
          'http://terminusdb.com/schema/woql#right' : V
         } :< JSON
    ->  json_to_woql_ast(S,WS),
        json_to_woql_ast(V,WV),
        WOQL = lower(WS,WV)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Pad',
          'http://terminusdb.com/schema/woql#pad_string' : S,
          'http://terminusdb.com/schema/woql#pad_char' : C,
          'http://terminusdb.com/schema/woql#pad_times' : N,
          'http://terminusdb.com/schema/woql#pad_result' : V
         } :< JSON
    ->  json_to_woql_ast(S,WS),
        json_to_woql_ast(C,WC),
        json_to_woql_ast(N,WN),
        json_to_woql_ast(V,WV),
        WOQL = pad(WS,WC,WN,WV)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Split',
          'http://terminusdb.com/schema/woql#split_string' : S,
          'http://terminusdb.com/schema/woql#pattern' : P,
          'http://terminusdb.com/schema/woql#split_list' : L
         } :< JSON
    ->  json_to_woql_ast(S,WS),
        json_to_woql_ast(P,WP),
        json_to_woql_ast(L,WL),
        WOQL = split(WS,WP,WL)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Member',
          'http://terminusdb.com/schema/woql#member' : S,
          'http://terminusdb.com/schema/woql#member_list' : L
         } :< JSON
    ->  json_to_woql_ast(S,WS),
        json_to_woql_ast(L,WL),
        WOQL = member(WS,WL)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Concatenate',
          'http://terminusdb.com/schema/woql#concat_list' :  List,
          'http://terminusdb.com/schema/woql#concatenated' : Value
         } :< JSON
    ->  json_to_woql_ast(List,WList),
        json_to_woql_ast(Value,WValue),
        WOQL = concat(WList,WValue)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Join',
          'http://terminusdb.com/schema/woql#join_list' List,
          'http://terminusdb.com/schema/woql#join_separator' : Sep,
          Value ] } :< JSON
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
