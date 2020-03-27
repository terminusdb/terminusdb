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
    json_to_woql_ast(JSON_Ex,WOQL,[]).

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
json_to_woql_ast(JSON,WOQL,Path) :-
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
        maplist({Path}/[V_ID,V]>>(
                    json_to_woql_ast(V_ID,V,
                                     ['http://terminusdb.com/schema/woql#variable',
                                      'http://terminusdb.com/schema/woql#variable_list'
                                      |Path])
                ),
                Variables, WOQL_Args),
        json_to_woql_ast(Sub_Query,Sub_WOQL,['http://terminusdb.com/schema/woql#query'
                                             |Path]),
        WOQL = select(WOQL_Args,Sub_WOQL)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#And',
          'http://terminusdb.com/schema/woql#query_list' : Indexed_Query_List} :< JSON
    ->  deindex_list('http://terminusdb.com/schema/woql#query',
                     Indexed_Query_List, Query_List),
        maplist({Path}/[Q,W]>>(
                    json_to_woql_ast(Q,W,['http://terminusdb.com/schema/woql#query',
                                          'http://terminusdb.com/schema/woql#query_list'
                                          |Path])
                ), Query_List, WOQL_Queries),
        xfy_list(',',WOQL,WOQL_Queries)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Or',
          'http://terminusdb.com/schema/woql#query_list' : Indexed_Query_List} :< JSON
    ->  deindex_list('http://terminusdb.com/schema/woql#query',
                     Indexed_Query_List, Query_List),
        maplist({Path}/[Q,W]>>(
                    json_to_woql_ast(Q,W,['http://terminusdb.com/schema/woql#query',
                                          'http://terminusdb.com/schema/woql#query_list'
                                         |Path])
                ), Query_List, WOQL_Queries),
        xfy_list(';',WOQL,WOQL_Queries)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Using',
          'http://terminusdb.com/schema/woql#collection' : Collection,
          'http://terminusdb.com/schema/woql#query' : Query } :< JSON
    ->  json_to_woql_ast(Collection,WC,['http://terminusdb.com/schema/woql#collection'
                                        |Path]),
        json_to_woql_ast(Query,WQ,['http://terminusdb.com/schema/woql#query'
                                   |Path]),
        WOQL = using(WC,WQ)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#From',
          'http://terminusdb.com/schema/woql#graph_filter' : Graph_Filter,
          'http://terminusdb.com/schema/woql#query' : Query } :< JSON
    ->  json_to_woql_ast(Graph_Filter,WG,['http://terminusdb.com/schema/woql#graph_filter'
                                          |Path]),
        json_to_woql_ast(Query,WQ,['http://terminusdb.com/schema/woql#query'
                                   |Path]),
        WOQL = from(WG,WQ)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Into',
          'http://terminusdb.com/schema/woql#graph' : Graph,
          'http://terminusdb.com/schema/woql#query' : Query } :< JSON
    ->  json_to_woql_ast(Graph,WG,['http://terminusdb.com/schema/woql#graph'
                                   |Path]),
        json_to_woql_ast(Query,WQ,['http://terminusdb.com/schema/woql#query'
                                   |Path]),
        WOQL = into(WG,WQ)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Quad',
          'http://terminusdb.com/schema/woql#subject' : Subject,
          'http://terminusdb.com/schema/woql#predicate' : Predicate,
          'http://terminusdb.com/schema/woql#object' : Object,
          'http://terminusdb.com/schema/woql#graph_filter' : Graph
         } :< JSON
    ->  json_to_woql_ast(Subject,WQA,['http://terminusdb.com/schema/woql#subject'
                                      |Path]),
        json_to_woql_ast(Predicate,WQB,['http://terminusdb.com/schema/woql#predicate'
                                        |Path]),
        json_to_woql_ast(Object,WQC,['http://terminusdb.com/schema/woql#object'
                                     |Path]),
        json_to_woql_ast(Graph,WG,['http://terminusdb.com/schema/woql#graph'
                                   |Path]),
        WOQL = t(WQA,WQB,WQC,WG)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Triple',
          'http://terminusdb.com/schema/woql#subject' : Subject,
          'http://terminusdb.com/schema/woql#predicate' : Predicate,
          'http://terminusdb.com/schema/woql#object' : Object
         } :< JSON
    ->  json_to_woql_ast(Subject,WQA,['http://terminusdb.com/schema/woql#subject'
                                      |Path]),
        json_to_woql_ast(Predicate,WQB,['http://terminusdb.com/schema/woql#predicate'
                                        |Path]),
        json_to_woql_ast(Object,WQC,['http://terminusdb.com/schema/woql#object'
                                     |Path]),
        WOQL = t(WQA,WQB,WQC)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Subsumption',
          'http://terminusdb.com/schema/woql#child' : Class_A,
          'http://terminusdb.com/schema/woql#parent' : Class_B
         } :< JSON
    ->  json_to_woql_ast(Class_A,WQA,['http://terminusdb.com/schema/woql#child'
                                      |Path]),
        json_to_woql_ast(Class_B,WQB,['http://terminusdb.com/schema/woql#parent'
                                      |Path]),
        WOQL = '<<'(WQA,WQB)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Equals',
          'http://terminusdb.com/schema/woql#left': A,
          'http://terminusdb.com/schema/woql#right' : B
         } :< JSON
    ->  json_to_woql_ast(A,WQA,['http://terminusdb.com/schema/woql#left'
                                |Path]),
        json_to_woql_ast(B,WQB,['http://terminusdb.com/schema/woql#right'
                                |Path]),
        WOQL = '='(WQA,WQB)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Substring',
          'http://terminusdb.com/schema/woql#string' : String,
          'http://terminusdb.com/schema/woql#before' : Before,
          'http://terminusdb.com/schema/woql#length' : Length,
          'http://terminusdb.com/schema/woql#after' : After,
          'http://terminusdb.com/schema/woql#substring' : Substring
         } :< JSON
    ->  json_to_woql_ast(String, WString,['http://terminusdb.com/schema/woql#string'
                                          |Path]),
        json_to_woql_ast(Before, WBefore,['http://terminusdb.com/schema/woql#before'
                                          |Path]),
        json_to_woql_ast(Length, WLength,['http://terminusdb.com/schema/woql#length'
                                          |Path]),
        json_to_woql_ast(After, WAfter,['http://terminusdb.com/schema/woql#after'
                                        |Path]),
        json_to_woql_ast(Substring, WSubstring,['http://terminusdb.com/schema/woql#substring'
                                                |Path]),
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
    ->  json_to_woql_ast(Subject,WQA,['http://terminusdb.com/schema/woql#subject'
                                      |Path]),
        json_to_woql_ast(Predicate,WQB,['http://terminusdb.com/schema/woql#predicate'
                                        |Path]),
        json_to_woql_ast(Object,WQC,['http://terminusdb.com/schema/woql#object'
                                     |Path]),
        WOQL = insert(WQA,WQB,WQC)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#AddQuad',
          'http://terminusdb.com/schema/woql#subject' : Subject,
          'http://terminusdb.com/schema/woql#predicate' : Predicate,
          'http://terminusdb.com/schema/woql#object' : Object,
          'http://terminusdb.com/schema/woql#graph' : Graph
         } :< JSON
    ->  json_to_woql_ast(Subject,WQA,['http://terminusdb.com/schema/woql#subject'
                                      |Path]),
        json_to_woql_ast(Predicate,WQB,['http://terminusdb.com/schema/woql#predicate'
                                        |Path]),
        json_to_woql_ast(Object,WQC,['http://terminusdb.com/schema/woql#object'
                                     |Path]),
        json_to_woql_ast(Graph,WG,['http://terminusdb.com/schema/woql#graph'
                                   |Path]),
        WOQL = insert(WG,WQA,WQB,WQC)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#DeleteTriple',
          'http://terminusdb.com/schema/woql#subject' : Subject,
          'http://terminusdb.com/schema/woql#predicate' : Predicate,
          'http://terminusdb.com/schema/woql#object' : Object
         } :< JSON
    ->  json_to_woql_ast(Subject,WQA,['http://terminusdb.com/schema/woql#subject'
                                      |Path]),
        json_to_woql_ast(Predicate,WQB,['http://terminusdb.com/schema/woql#predicate'
                                        |Path]),
        json_to_woql_ast(Object,WQC,['http://terminusdb.com/schema/woql#object'
                                     |Path]),
        WOQL = delete(WQA,WQB,WQC)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#DeleteQuad',
          'http://terminusdb.com/schema/woql#subject' : Subject,
          'http://terminusdb.com/schema/woql#predicate' : Predicate,
          'http://terminusdb.com/schema/woql#object' : Object,
          'http://terminusdb.com/schema/woql#graph' : Graph
         } :< JSON
    ->  json_to_woql_ast(Subject,WQA,['http://terminusdb.com/schema/woql#subject'
                                      |Path]),
        json_to_woql_ast(Predicate,WQB,['http://terminusdb.com/schema/woql#predicate'
                                        |Path]),
        json_to_woql_ast(Object,WQC,['http://terminusdb.com/schema/woql#object'
                                     |Path]),
        json_to_woql_ast(Graph,WG,['http://terminusdb.com/schema/woql#graph'
                                     |Path]),
        WOQL = delete(WG,WQA,WQB,WQC)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#When',
          'http://terminusdb.com/schema/woql#query' : Q,
          'http://terminusdb.com/schema/woql#consequent' : U
         } :< JSON
    ->  json_to_woql_ast(Q,WQ,['http://terminusdb.com/schema/woql#query'
                               |Path]),
        json_to_woql_ast(U,WU,['http://terminusdb.com/schema/woql#consequent'
                               |Path]),
        WOQL = '=>'(WQ,WU)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Trim',
          'http://terminusdb.com/schema/woql#untrimmed' :  A,
          'http://terminusdb.com/schema/woql#trimmed' : T
         } :< JSON
    ->  json_to_woql_ast(A,WA,['http://terminusdb.com/schema/woql#untrimmed'
                               |Path]),
        json_to_woql_ast(T,WT,['http://terminusdb.com/schema/woql#trimmed'
                               |Path]),
        WOQL = trim(WA,WT)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Eval',
          'http://terminusdb.com/schema/woql#expression' : A,
          'http://terminusdb.com/schema/woql#result' : X
         } :< JSON
    ->  json_to_woql_arith(X, V, ['http://terminusdb.com/schema/woql#result'
                                  |Path]),
        json_to_woql_arith(A, Arith, ['http://terminusdb.com/schema/woql#expression'
                                      |Path]),
        WOQL = 'is'(V,Arith)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#IsA',
          'http://terminusdb.com/schema/woql#element' : X,
          'http://terminusdb.com/schema/woql#of_type' : T
         } :< JSON
    ->  json_to_woql_ast(X, V, ['http://terminusdb.com/schema/woql#element'
                                |Path]),
        json_to_woql_ast(T, Class,['http://terminusdb.com/schema/woql#of_type'
                                   |Path]),
        WOQL = isa(V,Class)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Like',
          'http://terminusdb.com/schema/woql#left' :  A,
          'http://terminusdb.com/schema/woql#right' : B,
          'http://terminusdb.com/schema/woql#like_similarity' : F
         } :< JSON
    ->  json_to_woql_ast(A,WA,['http://terminusdb.com/schema/woql#left'
                               |Path]),
        json_to_woql_ast(B,WB,['http://terminusdb.com/schema/woql#right'
                               |Path]),
        json_to_woql_ast(F,WF,['http://terminusdb.com/schema/woql#like_similarity'
                               |Path]),
        WOQL = like(WA,WB,WF)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Less',
          'http://terminusdb.com/schema/woql#left' : A,
          'http://terminusdb.com/schema/woql#right' : B
         } :< JSON
    ->  json_to_woql_ast(A,WA,['http://terminusdb.com/schema/woql#left'
                               |Path]),
        json_to_woql_ast(B,WB,['http://terminusdb.com/schema/woql#right'
                               |Path]),
        WOQL = '<'(WA,WB)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Greater',
          'http://terminusdb.com/schema/woql#left' : A,
          'http://terminusdb.com/schema/woql#right' : B
         } :< JSON
    ->  json_to_woql_ast(A,WA,['http://terminusdb.com/schema/woql#left'
                               |Path]),
        json_to_woql_ast(B,WB,['http://terminusdb.com/schema/woql#right'
                               |Path]),
        WOQL = '>'(WA,WB)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Optional',
          'http://terminusdb.com/schema/woql#query' : Q
         } :< JSON
    ->  json_to_woql_ast(Q,WQ,['http://terminusdb.com/schema/woql#query'
                               |Path]),
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
        maplist({Path}/[Q,W]>>(% probably need an index...
                    json_to_woql_ast(Q,W,['http://terminusdb.com/schema/woql#var',
                                          'http://terminusdb.com/schema/woql#named_as_var'
                                          |Path])
                ), Deindexed_Header, WHeader),
        json_to_woql_ast(Resource,WResource, ['http://terminusdb.com/schema/woql#query_resource'
                                              |Path]),
        WOQL = get(WHeader,WResource)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#NamedAsVar',
          'http://terminusdb.com/schema/woql#var' : Var,
          'http://terminusdb.com/schema/woql#identifier' : Identifier
         } :< JSON
    ->  json_to_woql_ast(Var, WOQL_Var, ['http://terminusdb.com/schema/woql#var'
                                         |Path]),
        (   get_dict('http://terminusdb.com/schema/woql#var_type',
                     JSON,
                     Type)
        ->  WOQL = as(Identifier, v(WOQL_Var), Type)
        ;   WOQL = as(Identifier, v(WOQL_Var)))
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#IndexedAsVar',
          'http://terminusdb.com/schema/woql#var' : Var
         } :< JSON
    ->  json_to_woql_ast(Var, WOQL,['http://terminusdb.com/schema/woql#var'
                                    |Path])
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Put',
          'http://terminusdb.com/schema/woql#as_vars' : Header,
          'http://terminusdb.com/schema/woql#query' : Query,
          'http://terminusdb.com/schema/woql#resource' : Resource
         } :< JSON
    ->  maplist({Path}/[Q,W]>>(
                    json_to_woql_ast(Q,W,['http://terminusdb.com/schema/woql#as_vars'
                                          |Path])
                ),Header,WHeader),
        json_to_woql_ast(Query,WQuery,['http://terminusdb.com/schema/woql#query'
                                       |Path]),
        json_to_woql_ast(Resource,WResource,['http://terminusdb.com/schema/woql#resource'
                                             |Path]),
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
    ->  json_to_woql_ast(Base,WBase,['http://terminusdb.com/schema/woql#base'
                                     |Path]),
        json_to_woql_ast(Key,WKey,['http://terminusdb.com/schema/woql#key_list'
                                   |Path]),
        json_to_woql_ast(URI,WURI,['http://terminusdb.com/schema/woql#uri'
                                   |Path]),
        WOQL = hash(WBase,WKey,WURI)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#IDGenerator',
          'http://terminusdb.com/schema/woql#base' : Base,
          'http://terminusdb.com/schema/woql#key_list' : Key,
          'http://terminusdb.com/schema/woql#uri' : URI
         } :< JSON
    ->  json_to_woql_ast(Base,WBase,['http://terminusdb.com/schema/woql#base'
                                     |Path]),
        json_to_woql_ast(Key,WKey,['http://terminusdb.com/schema/woql#key_list'
                                   |Path]),
        json_to_woql_ast(URI,WURI,['http://terminusdb.com/schema/woql#uri'
                                   |Path]),
        WOQL = idgen(WBase,WKey,WURI)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Upper',
          'http://terminusdb.com/schema/woql#left' :  S,
          'http://terminusdb.com/schema/woql#right' : V
         } :< JSON
    ->  json_to_woql_ast(S,WS,['http://terminusdb.com/schema/woql#left'
                               |Path]),
        json_to_woql_ast(V,WV,['http://terminusdb.com/schema/woql#right'
                               |Path]),
        WOQL = upper(WS,WV)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Lower',
          'http://terminusdb.com/schema/woql#left' : S,
          'http://terminusdb.com/schema/woql#right' : V
         } :< JSON
    ->  json_to_woql_ast(S,WS,['http://terminusdb.com/schema/woql#left'
                               |Path]),
        json_to_woql_ast(V,WV,['http://terminusdb.com/schema/woql#right'
                               |Path]),
        WOQL = lower(WS,WV)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Pad',
          'http://terminusdb.com/schema/woql#pad_string' : S,
          'http://terminusdb.com/schema/woql#pad_char' : C,
          'http://terminusdb.com/schema/woql#pad_times' : N,
          'http://terminusdb.com/schema/woql#pad_result' : V
         } :< JSON
    ->  json_to_woql_ast(S,WS,['http://terminusdb.com/schema/woql#pad_string'
                               |Path]),
        json_to_woql_ast(C,WC,['http://terminusdb.com/schema/woql#pad_char'
                               |Path]),
        json_to_woql_ast(N,WN,['http://terminusdb.com/schema/woql#pad_times'
                               |Path]),
        json_to_woql_ast(V,WV,['http://terminusdb.com/schema/woql#pad_result'
                               |Path]),
        WOQL = pad(WS,WC,WN,WV)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Split',
          'http://terminusdb.com/schema/woql#split_string' : S,
          'http://terminusdb.com/schema/woql#pattern' : P,
          'http://terminusdb.com/schema/woql#split_list' : L
         } :< JSON
    ->  json_to_woql_ast(S,WS,['http://terminusdb.com/schema/woql#split_string'
                               |Path]),
        json_to_woql_ast(P,WP,['http://terminusdb.com/schema/woql#pattern'
                               |Path]),
        json_to_woql_ast(L,WL,['http://terminusdb.com/schema/woql#split_list'
                               |Path]),
        WOQL = split(WS,WP,WL)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Member',
          'http://terminusdb.com/schema/woql#member' : S,
          'http://terminusdb.com/schema/woql#member_list' : L
         } :< JSON
    ->  json_to_woql_ast(S,WS,['http://terminusdb.com/schema/woql#member'
                               |Path]),
        json_to_woql_ast(L,WL,['http://terminusdb.com/schema/woql#member_list'
                               |Path]),
        WOQL = member(WS,WL)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Concatenate',
          'http://terminusdb.com/schema/woql#concat_list' :  List,
          'http://terminusdb.com/schema/woql#concatenated' : Value
         } :< JSON
    ->  json_to_woql_ast(List,WList,['http://terminusdb.com/schema/woql#concat_list'
                                     |Path]),
        json_to_woql_ast(Value,WValue,['http://terminusdb.com/schema/woql#concatenated'
                                       |Path]),
        WOQL = concat(WList,WValue)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Join',
          'http://terminusdb.com/schema/woql#join_list' : List,
          'http://terminusdb.com/schema/woql#join_separator' : Sep,
          'http://terminusdb.com/schema/woql#join' : Value
         } :< JSON
    ->  json_to_woql_ast(List,WList,['http://terminusdb.com/schema/woql#join_list'
                                     |Path]),
        json_to_woql_ast(Sep,WSep,['http://terminusdb.com/schema/woql#join_separator'
                                   |Path]),
        json_to_woql_ast(Value,WValue,['http://terminusdb.com/schema/woql#join'
                                       |Path]),
        WOQL = join(WList,WSep,WValue)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Sum',
          'http://terminusdb.com/schema/woql#sum_list' : List,
          'http://terminusdb.com/schema/woql#sum' : Value
         } :< JSON
    ->  json_to_woql_ast(List,WList,['http://terminusdb.com/schema/woql#sum_list'
                                     |Path]),
        json_to_woql_ast(Value,WValue,['http://terminusdb.com/schema/woql#sum'
                                       |Path]),
        WOQL = sum(WList,WValue)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Start',
          'http://terminusdb.com/schema/woql#start' : N,
          'http://terminusdb.com/schema/woql#query' : Q
         } :< JSON
    ->  json_to_woql_arith(N,WN,['http://terminusdb.com/schema/woql#start'
                                 |Path]),
        json_to_woql_ast(Q,WQ,['http://terminusdb.com/schema/woql#query'
                               |Path]),
        WOQL = start(WN, WQ)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Limit' :
          'http://terminusdb.com/schema/woql#limit' :  N,
          'http://terminusdb.com/schema/woql#query' : Q
         } :< JSON
    ->  json_to_woql_arith(N,WN,['http://terminusdb.com/schema/woql#limit'
                                 |Path]),
        json_to_woql_ast(Q,WQ,['http://terminusdb.com/schema/woql#query'
                               |Path]),
        WOQL = limit(WN, WQ)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Regexp',
          'http://terminusdb.com/schema/woql#pattern' : Pat,
          'http://terminusdb.com/schema/woql#regexp_string' : String,
          'http://terminusdb.com/schema/woql#regexp_list' : List
         } :< JSON
    ->  json_to_woql_ast(Pat,WPat,['http://terminusdb.com/schema/woql#pattern'
                                   |Path]),
        json_to_woql_ast(String,WString,['http://terminusdb.com/schema/woql#regexp_string'
                                         |Path]),
        json_to_woql_ast(List,WList,['http://terminusdb.com/schema/woql#regexp_list'
                                     |Path]),
        WOQL = re(WPat, WString, WList)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#OrderBy',
          'http://terminusdb.com/schema/woql#variable_list' : Templates,
          'http://terminusdb.com/schema/woql#ascending' : Bool,
          'http://terminusdb.com/schema/woql#query' : Query
         } :< JSON
    ->  maplist({Path}/[V1,V2]>>(
                    json_to_woql_ast(V1,V2,['http://terminusdb.com/schema/woql#variable_list'
                                            |Path])
                ), Templates, WTemplates),
        json_to_woql_ast(Query,WQuery,['http://terminusdb.com/schema/woql#query'
                                       |Path]),
        json_to_woql_ast(Bool,Boolean^^_,['http://terminusdb.com/schema/woql#ascending'
                                          |Path]),
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
    ->  json_to_woql_ast(Spec,WSpec,['http://terminusdb.com/schema/woql#variable_list'
                                     |Path]),
        json_to_woql_ast(Obj,WObj,['http://terminusdb.com/schema/woql#group_var'
                                   |Path]),
        json_to_woql_ast(Query,WQuery,['http://terminusdb.com/schema/woql#query'
                                       |Path]),
        json_to_woql_ast(Collector,WCollector,['http://terminusdb.com/schema/woql#grouped'
                                               |Path]),
        WOQL = group_by(WSpec,WObj,WQuery,WCollector)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Length' :
          'http://terminusdb.com/schema/woql#length_list' : A,
          'http://terminusdb.com/schema/woql#length' : B
         } :< JSON
    ->  json_to_woql_ast(A,WA,['http://terminusdb.com/schema/woql#length_list'
                               |Path]),
        json_to_woql_ast(B,WB,['http://terminusdb.com/schema/woql#length'
                               |Path]),
        WOQL = length(WA,WB)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Typecast',
          'http://terminusdb.com/schema/woql#typecast_value' : Val,
          'http://terminusdb.com/schema/woql#typecast_type' : Type,
          'http://terminusdb.com/schema/woql#typecast_result' : Var
         } :< JSON
    ->  json_to_woql_ast(Val,WVal,['http://terminusdb.com/schema/woql#typecast_value'
                                   |Path]),
        json_to_woql_ast(Type,WType,['http://terminusdb.com/schema/woql#typecast_type'
                                     |Path]),
        json_to_woql_ast(Var,WVar,['http://terminusdb.com/schema/woql#typecast_result'
                                   |Path]),
        WOQL = typecast(WVal,WType,[],WVar)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Not',
          'http://terminusdb.com/schema/woql#query' : Q
         } :< JSON
    ->  json_to_woql_ast(Q,WQ,['http://terminusdb.com/schema/woql#typecast_result'
                               |Path]),
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
    ->  json_to_woql_ast(V,VE,['http://terminusdb.com/schema/woql#value'
                               |Path]),
        atom_string(TE,T),
        WOQL = '^^'(VE,TE)
    ;   _{'http://terminusdb.com/schema/woql#value' : V, '@language' : L } :< JSON
    ->  json_to_woql_ast(V,VE,['http://terminusdb.com/schema/woql#value'
                               |Path]),
        atom_string(LE,L),
        WOQL = '@'(VE,LE)
    ;   _{'@id' : ID } :< JSON
    ->  json_to_woql_ast(ID,WOQL,['@id'
                                  |Path])
    ;   true = JSON
    ->  WOQL = true
    ;   throw(http_reply(not_found(_{'@type' : 'vio:WOQLSyntaxError',
                                     'terminus:message' :'Un-parsable Query',
                                     'vio:query' : JSON})))
    ).
json_to_woql_ast(JSON,WOQL,_Path) :-
    number(JSON),
    !,
    WOQL = '^^'(JSON,'http://www.w3.org/2001/XMLSchema#decimal').
json_to_woql_ast(JSON,WOQL,_Path) :-
    coerce_atom(JSON,WOQL),
    !.
json_to_woql_ast(JSON,_,Path) :-
    format(atom(Msg), 'Un-parsable Query: ~q', [JSON]),
    throw(http_reply(not_found(_{'terminus:message' : Msg,
                                 'vio:query' : JSON,
                                 'vio:path' : Path,
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
