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
 *
 * TODO:
 * Currently we are assuming the list is in order and that the indexes
 * are for ordering during database retrieval. This is not great, it should
 * instead order according to the key using predsort/3 first.
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
    ->  deindex_list('http://terminusdb.com/schema/woql#variable',
                     Indexed_Variables,
                     Variables),
        maplist([V_ID,V]>>json_to_woql_ast(V_ID,V),
                Variables, WOQL_Args),
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
          'http://terminusdb.com/schema/woql#after' : After,
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
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Greater',
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
    ->  (   _{'@type' : 'http://terminusdb.com/schema/woql#IndexedAsVars',
              'http://terminusdb.com/schema/woql#indexed_as_var' : Indexed
             } :< Header
        ->  deindex_list('http://terminusdb.com/schema/woql#var',
                         Indexed,
                         Deindexed_Header)
        ;   _{'@type' : 'http://terminusdb.com/schema/woql#NamedAsVars',
              'http://terminusdb.com/schema/woql#named_as_var' : Named
             } :< Header
        ->  deindex_list('http://terminusdb.com/schema/woql#var',
                         Named,
                         Deindexed_Header)
        ),
        maplist(json_to_woql_ast, Deindexed_Header, WHeader),
        json_to_woql_ast(Resource,WResource),
        WOQL = get(WHeader,WResource)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#NamedAsVar',
          'http://terminusdb.com/schema/woql#var' : Var,
          'http://terminusdb.com/schema/woql#identifier' : Identifier
         } :< JSON
    ->  json_to_woql_ast(Var, WOQL_Var),
        (   get_dict('http://terminusdb.com/schema/woql#var_type',
                     JSON,
                     Type)
        ->  WOQL = as(Identifier, v(WOQL_Var), Type)
        ;   WOQL = as(Identifier, v(WOQL_Var)))
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#IndexedAsVar',
          'http://terminusdb.com/schema/woql#var' : Var
         } :< JSON
    ->  json_to_woql_ast(Var, WOQL)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Put',
          'http://terminusdb.com/schema/woql#as_vars' : Header,
          'http://terminusdb.com/schema/woql#query' : Query,
          'http://terminusdb.com/schema/woql#resource' : Resource
         } :< JSON
    ->  maplist(json_to_woql_ast,Header,WHeader),
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
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#PostResource',
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
          'http://terminusdb.com/schema/woql#join_list' : List,
          'http://terminusdb.com/schema/woql#join_separator' : Sep,
          'http://terminusdb.com/schema/woql#join' : Value
         } :< JSON
    ->  json_to_woql_ast(List,WList),
        json_to_woql_ast(Sep,WSep),
        json_to_woql_ast(Value,WValue),
        WOQL = join(WList,WSep,WValue)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Sum',
          'http://terminusdb.com/schema/woql#sum_list' : List,
          'http://terminusdb.com/schema/woql#sum' : Value
         } :< JSON
    ->  json_to_woql_ast(List,WList),
        json_to_woql_ast(Value,WValue),
        WOQL = sum(WList,WValue)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Start',
          'http://terminusdb.com/schema/woql#start' : N,
          'http://terminusdb.com/schema/woql#query' : Q
         } :< JSON
    ->  json_to_woql_arith(N,WN),
        json_to_woql_ast(Q,WQ),
        WOQL = start(WN, WQ)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Limit' :
          'http://terminusdb.com/schema/woql#limit' :  N,
          'http://terminusdb.com/schema/woql#query' : Q
         } :< JSON
    ->  json_to_woql_arith(N,WN),
        json_to_woql_ast(Q,WQ),
        WOQL = limit(WN, WQ)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Regexp',
          'http://terminusdb.com/schema/woql#pattern' : Pat,
          'http://terminusdb.com/schema/woql#regexp_string' : String,
          'http://terminusdb.com/schema/woql#regexp_list' : List
         } :< JSON
    ->  json_to_woql_ast(Pat,WPat),
        json_to_woql_ast(String,WString),
        json_to_woql_ast(List,WList),
        WOQL = re(WPat, WString, WList)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#OrderBy',
          'http://terminusdb.com/schema/woql#variable_list' : Templates,
          'http://terminusdb.com/schema/woql#ascending' : Bool,
          'http://terminusdb.com/schema/woql#query' : Query
         } :< JSON
    ->  maplist([V1,V2]>>(json_to_woql_ast(V1,V2)), Templates, WTemplates),
        json_to_woql_ast(Query,WQuery),
        json_to_woql_ast(Bool,Boolean^^_),
        (   Boolean = true
        ->  Order = asc
        ;   Order = desc),
        WOQL = order_by(WTemplates,WQuery,Order)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#GroupBy',
          'http://terminusdb.com/schema/woql#variable_list' : Spec,
          'http://terminusdb.com/schema/woql#group_var' : Obj,
          'http://terminusdb.com/schema/woql#query' : Query,
          'http://terminusdb.com/schema/woql#grouped' : Collector
         } :< JSON
    ->  json_to_woql_ast(Spec,WSpec),
        json_to_woql_ast(Obj,WObj),
        json_to_woql_ast(Query,WQuery),
        json_to_woql_ast(Collector,WCollector),
        WOQL = group_by(WSpec,WObj,WQuery,WCollector)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Length' :
          'http://terminusdb.com/schema/woql#length_list' : A,
          'http://terminusdb.com/schema/woql#length' : B
         } :< JSON
    ->  json_to_woql_ast(A,WA),
        json_to_woql_ast(B,WB),
        WOQL = length(WA,WB)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Typecast',
          'http://terminusdb.com/schema/woql#typecast_value' : Val,
          'http://terminusdb.com/schema/woql#typecast_type' : Type,
          'http://terminusdb.com/schema/woql#typecast_result' : Var
         } :< JSON
    ->  json_to_woql_ast(Val,WVal),
        json_to_woql_ast(Type,WType),
        json_to_woql_ast(Var,WVar),
        WOQL = typecast(WVal,WType,[],WVar)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Not',
          'http://terminusdb.com/schema/woql#query' : Q
         } :< JSON
    ->  json_to_woql_ast(Q,WQ),
        WOQL = not(WQ)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#True'} :< JSON
    ->  WOQL = true
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Variable',
          'http://terminusdb.com/schema/woql#variable_name' : Name} :< JSON
    ->  coerce_atom(Name, Atom_Name),
        WOQL = v(Atom_Name)
    ;   _{'@value' : V, '@type' : T } :< JSON
    ->  atom_string(TE,T),
        WOQL = '^^'(V,TE)
    ;   _{'@value' : V, '@language' : L } :< JSON
    ->  atom_string(LE,L),
        WOQL = '@'(V,LE)
    ;   _{'http://terminusdb.com/schema/woql#value' : V, '@type' : T } :< JSON
    ->  json_to_woql_ast(V,VE),
        atom_string(TE,T),
        WOQL = '^^'(VE,TE)
    ;   _{'http://terminusdb.com/schema/woql#value' : V, '@language' : L } :< JSON
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
    coerce_atom(JSON,WOQL),
    !.
json_to_woql_ast(JSON,_) :-
    format(atom(Msg), 'Un-parsable Query: ~q', [JSON]),
    throw(http_reply(not_found(_{'terminus:message' : Msg,
                                 'vio:query' : JSON,
                                 'terminus:status' : 'terminus:failure'}))).

is_json_var(A) :-
    sub_atom(A, 0, _, _, 'http://terminusdb.com/schema/woql/variable/').

json_to_woql_arith(JSON,WOQL) :-
    is_dict(JSON),
    !,
    (   _{'@type' : 'http://terminusdb.com/schema/woql#Plus',
          'http://terminusdb.com/schema/woql#first' : First,
          'http://terminusdb.com/schema/woql#second' : Second} :< JSON
    ->  json_to_woql_arith(First, WOQL_First),
        json_to_woql_arith(Second, WOQL_Second),
        WOQL = '+'(WOQL_First,WOQL_Second)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Minus',
          'http://terminusdb.com/schema/woql#first' : First,
          'http://terminusdb.com/schema/woql#second' : Second} :< JSON
    ->  json_to_woql_arith(First, WOQL_First),
        json_to_woql_arith(Second, WOQL_Second),
        WOQL = '-'(WOQL_First,WOQL_Second)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Times',
          'http://terminusdb.com/schema/woql#first' : First,
          'http://terminusdb.com/schema/woql#second' : Second} :< JSON
    ->  json_to_woql_arith(First, WOQL_First),
        json_to_woql_arith(Second, WOQL_Second),
        WOQL = '*'(WOQL_First,WOQL_Second)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Divide',
          'http://terminusdb.com/schema/woql#first' : First,
          'http://terminusdb.com/schema/woql#second' : Second} :< JSON
    ->  json_to_woql_arith(First, WOQL_First),
        json_to_woql_arith(Second, WOQL_Second),
        WOQL = '/'(WOQL_First,WOQL_Second)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Div',
          'http://terminusdb.com/schema/woql#first' : First,
          'http://terminusdb.com/schema/woql#second' : Second} :< JSON
    ->  json_to_woql_arith(First, WOQL_First),
        json_to_woql_arith(Second, WOQL_Second),
        WOQL = 'div'(WOQL_First,WOQL_Second)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Exp',
          'http://terminusdb.com/schema/woql#first' : First,
          'http://terminusdb.com/schema/woql#second' : Second} :< JSON
    ->  json_to_woql_arith(First, WOQL_First),
        json_to_woql_arith(Second, WOQL_Second),
        WOQL = 'exp'(WOQL_First,WOQL_Second)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Floor',
          'http://terminusdb.com/schema/woql#argument' : Argument} :< JSON
    ->  json_to_woql_arith(Argument,WOQL_Argument),
        WOQL=floor(WOQL_Argument)
    ;   number(JSON)
    ->  WOQL = JSON
    ;   throw(http_reply(not_found(_{'terminus:message' : 'Unknown Syntax',
                                     'vio:query' : JSON,
                                     'terminus:status' : 'terminus:failure'})))
    ).
json_to_woql_arith(JSON,JSON) :-
    number(JSON).
