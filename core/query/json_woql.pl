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
    ->  json_to_woql_ast(Indexed_Variables,WOQL_Args,
                         ['http://terminusdb.com/schema/woql#variable_list'
                          |Path]),
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
        (   WC = Collection_String^^_
        ->  true
        ;   reverse(Path,Director),
            throw(http_reply(not_found(_{'@type' : 'vio:WOQLSyntaxError',
                                               'terminus:message' :'Poorly formed collection descriptor',
                                               'vio:path' : Director,
                                               'vio:query' : JSON})))
        ),
        WOQL = using(Collection_String,WQ)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#From',
          'http://terminusdb.com/schema/woql#graph_filter' : Graph_Filter,
          'http://terminusdb.com/schema/woql#query' : Query } :< JSON
    ->  json_to_woql_ast(Graph_Filter,WG,['http://terminusdb.com/schema/woql#graph_filter'
                                          |Path]),
        json_to_woql_ast(Query,WQ,['http://terminusdb.com/schema/woql#query'
                                   |Path]),
        (   WG = Graph_String^^_
        ->  true
        ;   reverse(Path,Director),
            throw(http_reply(not_found(_{'@type' : 'vio:WOQLSyntaxError',
                                         'terminus:message' :'Poorly formed graph descriptor',
                                         'vio:path' : Director,
                                         'vio:query' : JSON})))),
        WOQL = from(Graph_String,WQ)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Into',
          'http://terminusdb.com/schema/woql#graph' : Graph,
          'http://terminusdb.com/schema/woql#query' : Query } :< JSON
    ->  json_to_woql_ast(Graph,WG,['http://terminusdb.com/schema/woql#graph'
                                   |Path]),
        json_to_woql_ast(Query,WQ,['http://terminusdb.com/schema/woql#query'
                                   |Path]),
        (   WG = Graph_String^^_
        ->  true
        ;   reverse(Path,Director),
            throw(http_reply(not_found(_{'@type' : 'vio:WOQLSyntaxError',
                                         'terminus:message' :'Poorly formed graph descriptor',
                                         'vio:path' : Director,
                                         'vio:query' : JSON})))),
        WOQL = into(Graph_String,WQ)
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
        (   WG = Graph_String^^_
        ->  true
        ;   reverse(Path,Director),
            throw(http_reply(not_found(_{'@type' : 'vio:WOQLSyntaxError',
                                         'terminus:message' :'Poorly formed graph descriptor',
                                         'vio:path' : Director,
                                         'vio:query' : JSON})))),
        WOQL = t(WQA,WQB,WQC,Graph_String)
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
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#ReadObject',
          'http://terminusdb.com/schema/woql#document_uri' : Doc_ID,
          'http://terminusdb.com/schema/woql#document' : Doc
         } :< JSON
    ->  json_to_woql_ast(Doc_ID, WID, ['http://terminusdb.com/schema/woql#document_id'
                                       |Path]),
        json_to_woql_ast(Doc, WDoc, ['http://terminusdb.com/schema/woql#document'
                                     |Path]),
        WOQL = read_object(WID,WDoc)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#UpdateObject',
          'http://terminusdb.com/schema/woql#document' : Doc
         } :< JSON
    ->  (   _{'@id' : _ID} :< Doc
        ->  WOQL = update_object(Doc)
        ;   throw(http_reply(not_found(_{'@type' : 'vio:WOQLSyntaxError',
                                         'terminus:message' :'No ID specified in updated object',
                                         'vio:query' : JSON})))
        )
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#DeleteObject',
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
        (   WG = Graph_String^^_
        ->  true
        ;   reverse(Path,Director),
            throw(http_reply(not_found(_{'@type' : 'vio:WOQLSyntaxError',
                                         'terminus:message' :'Poorly formed graph descriptor',
                                         'vio:path' : Director,
                                         'vio:query' : JSON})))),
        WOQL = insert(WQA,WQB,WQC,Graph_String)
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
        (   WG = Graph_String^^_
        ->  true
        ;   reverse(Path,Director),
            throw(http_reply(not_found(_{'@type' : 'vio:WOQLSyntaxError',
                                         'terminus:message' :'Poorly formed graph descriptor',
                                         'vio:path' : Director,
                                         'vio:query' : JSON})))),
        WOQL = delete(WQA,WQB,WQC,Graph_String)
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
          'http://terminusdb.com/schema/woql#as_vars' : AsVars,
          'http://terminusdb.com/schema/woql#query_resource' : Resource
         } :< JSON
    ->  json_to_woql_ast(AsVars,WAsVars,['http://terminusdb.com/schema/woql#as_vars'
                                         |Path]),
        json_to_woql_ast(Resource,WResource, ['http://terminusdb.com/schema/woql#query_resource'
                                              |Path]),
        WOQL = get(WAsVars,WResource)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#NamedAsVar',
          'http://terminusdb.com/schema/woql#variable_name' : Var_Name,
          'http://terminusdb.com/schema/woql#identifier' : Identifier
         } :< JSON
    ->  json_to_woql_ast(Var_Name, Var_Literal, ['http://terminusdb.com/schema/woql#var'
                                                 |Path]),
        json_to_woql_ast(Identifier, WIdentifier, ['http://terminusdb.com/schema/woql#identifier'
                                                   |Path]),
        (   Var_Literal = Var_String^^_,
            atom_string(Var_ID,Var_String),
            WOQL_Var = v(Var_ID),
            WIdentifier = ID_String^^_,
            atom_string(ID, ID_String)
        ->  true
        ;   reverse(Path, Director),
            throw(http_reply(error(_{'@type' : 'vio:WOQLSyntaxError',
                                     'terminus:message' :'Poorly constructed var',
                                     'vio:path' : Director,
                                     'vio:query' : JSON})))),
        (   get_dict('http://terminusdb.com/schema/woql#var_type',
                     JSON,
                     Type),
            json_to_woql_ast(Type, WType, ['http://terminusdb.com/schema/woql#var_type'
                                           |Path])
        ->  WOQL = as(ID, WOQL_Var, WType)
        ;   WOQL = as(ID, WOQL_Var))
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
    ->  json_to_woql_ast(Header,WHeader,['http://terminusdb.com/schema/woql#as_vars'
                                         |Path]),
        json_to_woql_ast(Query,WQuery,['http://terminusdb.com/schema/woql#query'
                                       |Path]),
        json_to_woql_ast(Resource,WResource,['http://terminusdb.com/schema/woql#resource'
                                             |Path]),
        WOQL = put(WHeader,WQuery,WResource)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#RemoteResource',
          'http://terminusdb.com/schema/woql#remote_uri' : URI
         } :< JSON
    ->  json_to_woql_ast(URI,WURI,['http://terminusdb.com/schema/woql#remote_uri'
                                   |Path]),
        WURI = URI_String^^_,
        WOQL = remote(URI_String,JSON)
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
          'http://terminusdb.com/schema/woql#split_pattern' : P,
          'http://terminusdb.com/schema/woql#split_list' : L
         } :< JSON
    ->  json_to_woql_ast(S,WS,['http://terminusdb.com/schema/woql#split_string'
                               |Path]),
        json_to_woql_ast(P,WP,['http://terminusdb.com/schema/woql#split_pattern'
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
    ->  json_to_woql_ast(N,WN,['http://terminusdb.com/schema/woql#start'
                                 |Path]),
        json_to_woql_ast(Q,WQ,['http://terminusdb.com/schema/woql#query'
                               |Path]),
        (   WN = Num^^_
        ->  true
        ;   reverse(Path,Director),
            throw(http_reply(not_found(_{'@type' : 'vio:WOQLSyntaxError',
                                   'terminus:message' :'Poorly formed limit',
                                   'vio:path' : Director,
                                   'vio:query' : JSON})))),
        WOQL = start(Num, WQ)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Limit',
          'http://terminusdb.com/schema/woql#limit' :  N,
          'http://terminusdb.com/schema/woql#query' : Q
         } :< JSON
    ->  json_to_woql_ast(N,WN,['http://terminusdb.com/schema/woql#limit'
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
          'http://terminusdb.com/schema/woql#variable_ordering' : Templates,
          'http://terminusdb.com/schema/woql#query' : Query
         } :< JSON
    ->  json_to_woql_ast(Templates,WTemplates,['http://terminusdb.com/schema/woql#variable_list'
                                               |Path]),
        json_to_woql_ast(Query,WQuery,['http://terminusdb.com/schema/woql#query'
                                       |Path]),
        WOQL = order_by(WTemplates,WQuery)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#VariableOrdering',
          'http://terminusdb.com/schema/woql#variable' : Var
         } :< JSON
    ->  (   get_dict('http://terminusdb.com/schema/woql#ascending', JSON, Bool)
        ->  json_to_woql_ast(Bool,Boolean^^_,['http://terminusdb.com/schema/woql#ascending'
                                              |Path])
        ;   Boolean = true),
        json_to_woql_ast(Var,WVar,['http://terminusdb.com/schema/woql#ascending'
                                   |Path]),
        (   Boolean = true
        ->  Order = asc
        ;   Order = desc),
        WOQL =.. [Order,WVar]
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#GroupBy',
          'http://terminusdb.com/schema/woql#group_by' : Spec,
          'http://terminusdb.com/schema/woql#group_template' : Obj,
          'http://terminusdb.com/schema/woql#query' : Query,
          'http://terminusdb.com/schema/woql#grouped' : Collector
         } :< JSON
    ->  json_to_woql_ast(Spec,WSpec,['http://terminusdb.com/schema/woql#group_by'
                                     |Path]),
        json_to_woql_ast(Obj,WObj,['http://terminusdb.com/schema/woql#group_template'
                                   |Path]),
        json_to_woql_ast(Query,WQuery,['http://terminusdb.com/schema/woql#query'
                                       |Path]),
        json_to_woql_ast(Collector,WCollector,['http://terminusdb.com/schema/woql#grouped'
                                               |Path]),
        WOQL = group_by(WSpec,WObj,WQuery,WCollector)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Length',
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
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Dot',
          'http://terminusdb.com/schema/woql#dictionary' : Dictionary,
          'http://terminusdb.com/schema/woql#dictionary_key' : Key,
          'http://terminusdb.com/schema/woql#dictionary_value' : Value
         } :< JSON
    ->  json_to_woql_ast(Dictionary, WDictionary, ['http://terminusdb.com/schema/woql#dictionary_key'
                                                   |Path]),
        json_to_woql_ast(Key, WKey, ['http://terminusdb.com/schema/woql#dictionary_key'
                                     |Path]),
        json_to_woql_ast(Value,WValue,['http://terminusdb.com/schema/woql#dictionary_value'
                                       |Path]),
        WOQL = dot(WDictionary,WKey,WValue)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Path',
          'http://terminusdb.com/schema/woql#subject' : Subject,
          'http://terminusdb.com/schema/woql#path_pattern' : Pattern,
          'http://terminusdb.com/schema/woql#object' : Object,
          'http://terminusdb.com/schema/woql#path' : Edge_Path
         } :< JSON
    ->  json_to_woql_ast(Subject,WSubject,['http://terminusdb.com/schema/woql#subject'
                                           |Path]),
        json_to_woql_path_pattern(Pattern,WPattern,['http://terminusdb.com/schema/woql#path_pattern'
                                                    |Path]),
        json_to_woql_ast(Object,WObject,['http://terminusdb.com/schema/woql#object'
                                         |Path]),
        json_to_woql_ast(Edge_Path,WEdge_Path,['http://terminusdb.com/schema/woql#path'
                                               |Path]),
        WOQL = path(WSubject,WPattern,WObject,WEdge_Path)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#True'} :< JSON
    ->  WOQL = true
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Array',
          'http://terminusdb.com/schema/woql#array_element' : List} :< JSON
        % Unbox values
    ->  json_to_woql_ast(List, WOQL, ['http://terminusdb.com/schema/woql#array_element'
                                      |Path])
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#ArrayElement',
          'http://terminusdb.com/schema/woql#variable_name' : Value} :< JSON
        % Unbox values
    ->  json_to_woql_ast(Value, Result, ['http://terminusdb.com/schema/woql#variable_name'
                                         |Path]),
        Result = String_or_Atom_Name^^_,
        coerce_atom(String_or_Atom_Name, Atom_Name),
        WOQL = v(Atom_Name)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#ArrayElement',
          'http://terminusdb.com/schema/woql#node' : Node} :< JSON
        % Unbox values
    ->  json_to_woql_ast(Node, WOQL, ['http://terminusdb.com/schema/woql#node'
                                      |Path])
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#ArrayElement',
          'http://terminusdb.com/schema/woql#datatype' : DT} :< JSON
        % Unbox values
    ->  json_to_woql_ast(DT, WOQL, ['http://terminusdb.com/schema/woql#datatype'
                                    |Path])
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#VariableListElement',
          'http://terminusdb.com/schema/woql#variable_name' : Value} :< JSON
        % Unbox values
    ->  json_to_woql_ast(Value, Result, ['http://terminusdb.com/schema/woql#variable_name'
                                         |Path]),
        Result = String_or_Atom_Name^^_,
        coerce_atom(String_or_Atom_Name, Atom_Name),
        WOQL = v(Atom_Name)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#IndexedAsVars',
          'http://terminusdb.com/schema/woql#indexed_as_var' : List} :< JSON
        % Unbox values
    ->  json_to_woql_ast(List, WOQL, ['http://terminusdb.com/schema/woql#indexed_as_var'
                                      |Path])
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#AsVars',
          'http://terminusdb.com/schema/woql#named_as_var' : List} :< JSON
        % Unbox values
    ->  json_to_woql_ast(List, WOQL, ['http://terminusdb.com/schema/woql#named_as_var'
                                       |Path])

    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Datatype',
          'http://terminusdb.com/schema/woql#datatype': Value} :< JSON
        % Unbox values
    ->  json_to_woql_ast(Value, WOQL, ['http://terminusdb.com/schema/woql#datatype'
                                       |Path])
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Node',
          'http://terminusdb.com/schema/woql#node': Node} :< JSON
        % Unbox values
    ->  json_to_woql_ast(Node, WOQL, ['http://terminusdb.com/schema/woql#node'
                                      |Path])
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Variable',
          'http://terminusdb.com/schema/woql#variable_name' : Name} :< JSON
    ->  json_to_woql_ast(Name, Result, ['http://terminusdb.com/schema/woql#variable_name'
                                        |Path]),
        Result = String_or_Atom_Name^^_,
        coerce_atom(String_or_Atom_Name, Atom_Name),
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
    ;   reverse(Path,Director),
        throw(http_reply(not_found(_{'@type' : 'vio:WOQLSyntaxError',
                                     'terminus:message' :'Un-parsable Query',
                                     'vio:path' : Director,
                                     'vio:query' : JSON})))
    ).
json_to_woql_ast(JSON,WOQL,Path) :-
    is_list(JSON),
    !,
    predsort(woql_index_sort, JSON, JSON_Sorted),
    length(JSON, Len),
    End is Len - 1,
    numlist(0,End,Indexes),
    maplist({Path}/[Q,I,W]>>(
                json_to_woql_ast(Q,W,[I|Path])
            ), JSON_Sorted, Indexes, WOQL).
json_to_woql_ast(JSON,WOQL,_Path) :-
    number(JSON),
    !,
    WOQL = '^^'(JSON,'http://www.w3.org/2001/XMLSchema#decimal').
json_to_woql_ast(JSON,WOQL,_Path) :-
    coerce_atom(JSON,WOQL),
    !.
json_to_woql_ast(JSON,_,Path) :-
    format(atom(Msg), 'Un-parsable Query: ~q', [JSON]),
    reverse(Path, Director),
    throw(http_reply(not_found(_{'terminus:message' : Msg,
                                 'vio:query' : JSON,
                                 'vio:path' : Director,
                                 'terminus:status' : 'terminus:failure'}))).

json_to_woql_path_pattern(JSON,Pattern,Path) :-
    is_dict(JSON),
    !,
    (   _{'@type' : 'http://terminusdb.com/schema/woql#PathPredicate',
          'http://terminusdb.com/schema/woql#path_predicate' : Node} :< JSON
    ->   json_to_woql_ast(Node,WNode,['http://terminusdb.com/schema/woql#path_predicate'
                                      |Path]),
        Pattern = p(WNode)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#PathSequence',
          'http://terminusdb.com/schema/woql#path_first' : First,
          'http://terminusdb.com/schema/woql#path_second' : Second} :< JSON
    ->  json_to_woql_path_pattern(First,PFirst,['http://terminusdb.com/schema/woql#path_first'
                                                |Path]),
        json_to_woql_path_pattern(Second,PSecond,['http://terminusdb.com/schema/woql#path_second'
                                                  |Path]),
        Pattern = (PFirst,PSecond)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#PathOr',
          'http://terminusdb.com/schema/woql#path_left' : Left,
          'http://terminusdb.com/schema/woql#path_right' : Right} :< JSON
    ->  json_to_woql_path_pattern(Left,PLeft,['http://terminusdb.com/schema/woql#path_left'
                                              |Path]),
        json_to_woql_path_pattern(Right,PRight,['http://terminusdb.com/schema/woql#path_right'
                                                |Path]),
        Pattern = (PLeft;PRight)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#PathPlus',
          'http://terminusdb.com/schema/woql#path_pattern' : SubPattern} :< JSON
    ->  json_to_woql_path_pattern(SubPattern,PSubPattern,
                                  ['http://terminusdb.com/schema/woql#path_left'
                                   |Path]),
        Pattern = plus(PSubPattern)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#PathTimes',
          'http://terminusdb.com/schema/woql#path_pattern' : SubPattern,
          'http://terminusdb.com/schema/woql#path_minimum' : N,
          'http://terminusdb.com/schema/woql#path_maximum' : M
         } :< JSON
    ->  json_to_woql_path_pattern(SubPattern,PSubPattern,
                                  ['http://terminusdb.com/schema/woql#path_left'
                                   |Path]),
        json_to_woql_ast(N,WN,
                         ['http://terminusdb.com/schema/woql#path_minimum'
                          |Path]),
        json_to_woql_ast(M,WM,
                         ['http://terminusdb.com/schema/woql#path_maximum'
                          |Path]),
        (   WM = M_int ^^ _,
            WN = N_int ^^ _
        ->  Pattern = times(PSubPattern,N_int,M_int)
        ;   reverse(Path,Director),
            throw(http_reply(not_found(_{'@type' : 'vio:WOQLSyntaxError',
                                         'terminus:message' :'Un-parsable Query due to max/min',
                                         'vio:path' : Director,
                                         'vio:query' : JSON}))))
    ;   reverse(Path,Director),
        throw(http_reply(not_found(_{'@type' : 'vio:WOQLSyntaxError',
                                     'terminus:message' :'Un-parsable Query',
                                     'vio:path' : Director,
                                     'vio:query' : JSON})))).

is_json_var(A) :-
    sub_atom(A, 0, _, _, 'http://terminusdb.com/schema/woql/variable/').

woql_index_sort(Comp,Left,Right) :-
    (   is_dict(Left),
        is_dict(Right),
        get_dict('http://terminusdb.com/schema/woql#index', Left, Left_Lit),
        get_dict('@value', Left_Lit, I),
        get_dict('http://terminusdb.com/schema/woql#index', Right, Right_Lit),
        get_dict('@value', Right_Lit, J)
    ->  compare(Comp, I, J)
    ;   compare(Comp, Left, Right)).

json_to_woql_arith(JSON,WOQL,Path) :-
    is_dict(JSON),
    !,
    (   _{'@type' : 'http://terminusdb.com/schema/woql#Plus',
          'http://terminusdb.com/schema/woql#first' : First,
          'http://terminusdb.com/schema/woql#second' : Second} :< JSON
    ->  json_to_woql_arith(First, WOQL_First,
                           ['http://terminusdb.com/schema/woql#first'
                           |Path]),
        json_to_woql_arith(Second, WOQL_Second,
                           ['http://terminusdb.com/schema/woql#second'
                           |Path]),
        WOQL = '+'(WOQL_First,WOQL_Second)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Minus',
          'http://terminusdb.com/schema/woql#first' : First,
          'http://terminusdb.com/schema/woql#second' : Second} :< JSON
    ->  json_to_woql_arith(First, WOQL_First,
                           ['http://terminusdb.com/schema/woql#first'
                            |Path]),
        json_to_woql_arith(Second, WOQL_Second,
                           ['http://terminusdb.com/schema/woql#second'
                            |Path]),
        WOQL = '-'(WOQL_First,WOQL_Second)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Times',
          'http://terminusdb.com/schema/woql#first' : First,
          'http://terminusdb.com/schema/woql#second' : Second} :< JSON
    ->  json_to_woql_arith(First, WOQL_First,
                           ['http://terminusdb.com/schema/woql#first'
                            |Path]),
        json_to_woql_arith(Second, WOQL_Second,
                           ['http://terminusdb.com/schema/woql#second'
                            |Path]),
        WOQL = '*'(WOQL_First,WOQL_Second)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Divide',
          'http://terminusdb.com/schema/woql#first' : First,
          'http://terminusdb.com/schema/woql#second' : Second} :< JSON
    ->  json_to_woql_arith(First, WOQL_First,
                           ['http://terminusdb.com/schema/woql#first'
                            |Path]),
        json_to_woql_arith(Second, WOQL_Second,
                           ['http://terminusdb.com/schema/woql#second'
                            |Path]),
        WOQL = '/'(WOQL_First,WOQL_Second)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Div',
          'http://terminusdb.com/schema/woql#first' : First,
          'http://terminusdb.com/schema/woql#second' : Second} :< JSON
    ->  json_to_woql_arith(First, WOQL_First,
                           ['http://terminusdb.com/schema/woql#first'
                            |Path]),
        json_to_woql_arith(Second, WOQL_Second,
                           ['http://terminusdb.com/schema/woql#second'
                            |Path]),
        WOQL = 'div'(WOQL_First,WOQL_Second)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Exp',
          'http://terminusdb.com/schema/woql#first' : First,
          'http://terminusdb.com/schema/woql#second' : Second} :< JSON
    ->  json_to_woql_arith(First, WOQL_First,
                           ['http://terminusdb.com/schema/woql#first'
                            |Path]),
        json_to_woql_arith(Second, WOQL_Second,
                           ['http://terminusdb.com/schema/woql#second'
                            |Path]),
        WOQL = '**'(WOQL_First,WOQL_Second)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Floor',
          'http://terminusdb.com/schema/woql#argument' : Argument} :< JSON
    ->  json_to_woql_arith(Argument,WOQL_Argument,
                           ['http://terminusdb.com/schema/woql#argument'
                            |Path]),
        WOQL=floor(WOQL_Argument)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Datatype',
          'http://terminusdb.com/schema/woql#datatype': Value} :< JSON
        % Unbox values
    ->  json_to_woql_arith(Value, WOQL, ['http://terminusdb.com/schema/woql#value'
                                         |Path])
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Node',
          'http://terminusdb.com/schema/woql#node': Node} :< JSON
        % Unbox values
    ->  json_to_woql_arith(Node, WOQL, ['http://terminusdb.com/schema/woql#node'
                                        |Path])
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#Variable',
          'http://terminusdb.com/schema/woql#variable_name' : Name} :< JSON
    ->  json_to_woql_arith(Name, Result, ['http://terminusdb.com/schema/woql#variable_name'
                                          |Path]),
        Result = String_or_Atom_Name^^_,
        coerce_atom(String_or_Atom_Name, Atom_Name),
        WOQL = v(Atom_Name)
    ;   _{'@value' : V, '@type' : T } :< JSON
    ->  atom_string(TE,T),
        WOQL = '^^'(V,TE)
    ;   reverse(Path, Director),
        throw(http_reply(not_found(_{'terminus:message' : 'Unknown Syntax',
                                     'vio:query' : JSON,
                                     'vio:path' : Director,
                                     'terminus:status' : 'terminus:failure'})))
    ).
json_to_woql_arith(JSON,_,Path) :-
    reverse(Path, Director),
    throw(http_reply(not_found(_{'terminus:message' : 'Unknown Syntax',
                                 'vio:query' : JSON,
                                 'vio:path' : Director,
                                 'terminus:status' : 'terminus:failure'}))).
