:- module(json_woql,[
              woql_context/1,
              initialise_woql_contexts/0,
              json_woql/2,
              json_woql/3,
              json_woql_path_element_error_message/4
          ]).

/** <module> WOQL JSON-LD syntax
 *
 * This file contains the code for conversion of JSON-LD to the WOQL
 * AST.
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- use_module(jsonld).

:- use_module(library(sort)).
:- use_module(core(triple)).
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
            ), List, Flatten),
    !.
deindex_list(_Key,List,List). % maybe the list is already flattened?

/*
 * json_to_woql_ast(+JSON:dict,-AST:any) is det.
 *
 * Translate a JSON-LD WOQL statement into the AST.
 */
json_to_woql_ast(JSON,WOQL,Path) :-
    is_dict(JSON),
    !,
    (   _{'@value' : V, '@type' : T } :< JSON
    ->  atom_string(TE,T),
        % NOTE: TODO: Marshall to appropriate datatype when input is not a string
        once(typecast(V^^'http://www.w3.org/2001/XMLSchema#string',
                      TE, [], Val)),
        WOQL = Val
    ;   _{'@type' : Type} :< JSON,
        json_type_to_woql_ast(Type,JSON,WOQL,Path)
    ->  true
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
    ;   throw(error(woql_syntax_error(JSON,Path,JSON), _))
    ).
json_to_woql_ast(JSON,WOQL,Path) :-
    is_list(JSON),
    !,
    predsort(woql_index_sort, JSON, JSON_Sorted),
    length(JSON, Len),
    End is Len - 1,
    (   End = -1
    ->  Indexes = []
    ;   numlist(0,End,Indexes)),
    maplist({Path}/[Q,I,W]>>(
                json_to_woql_ast(Q,W,[I|Path])
            ), JSON_Sorted, Indexes, WOQL).
json_to_woql_ast(JSON,WOQL,_Path) :-
    number(JSON),
    !,
    WOQL = '^^'(JSON,'http://www.w3.org/2001/XMLSchema#decimal').
json_to_woql_ast(JSON,WOQL,_Path) :-
    atom(JSON),
    memberchk(JSON,[true,false]),
    !,
    WOQL = '^^'(JSON,'http://www.w3.org/2001/XMLSchema#boolean').
json_to_woql_ast(JSON,WOQL,_Path) :-
    coerce_atom(JSON,WOQL),
    !.
json_to_woql_ast(JSON,_,Path) :-
    reverse(Path, Director),
    throw(error(unparsable_query(JSON,Director), _)).


% Convert specific types to the WOQL AST, should be expanded to
% make the predicate json_to_woql_ast less verbose and big
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Comment',_,WOQL,_) :-
    WOQL = true.
json_type_to_woql_ast('http://terminusdb.com/schema/woql#TripleCount',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#resource' : Resource,
      'http://terminusdb.com/schema/woql#triple_count' : Count
     } :< JSON,
    json_to_woql_ast(Resource,WOQL_Resource,
                     ['http://terminusdb.com/schema/woql#resource'
                      |Path]),
    json_to_woql_ast(Count,WOQL_Count,
                     ['http://terminusdb.com/schema/woql#triple_count'
                      |Path]),
    do_or_die(
        (WOQL_Resource = Resource_String^^_),
        error(woql_syntax_error(JSON,
                                ['http://terminusdb.com/schema/woql#resource'|Path],
                                Resource), _)),
    WOQL = triple_count(Resource_String,WOQL_Count).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Size',JSON,WOQL,Path) :-
    _{  'http://terminusdb.com/schema/woql#resource' : Resource,
        'http://terminusdb.com/schema/woql#size' : Size
    } :< JSON,
    json_to_woql_ast(Resource,WOQL_Resource,
                     ['http://terminusdb.com/schema/woql#resource'
                      |Path]),
    json_to_woql_ast(Size,WOQL_Size,
                     ['http://terminusdb.com/schema/woql#size'
                      |Path]),
    do_or_die(
        (WOQL_Resource = Resource_String^^_),
        error(woql_syntax_error(JSON,
                                ['http://terminusdb.com/schema/woql#resource'|Path],
                                Resource), _)),
    WOQL = size(Resource_String,WOQL_Size).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Select',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#variable_list' : Indexed_Variables,
      'http://terminusdb.com/schema/woql#query' : Sub_Query } :< JSON,
    json_to_woql_ast(Indexed_Variables,WOQL_Args,
                     ['http://terminusdb.com/schema/woql#variable_list'
                      |Path]),
    json_to_woql_ast(Sub_Query,Sub_WOQL,['http://terminusdb.com/schema/woql#query'
                                         |Path]),
    WOQL = select(WOQL_Args,Sub_WOQL).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Distinct',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#variable_list' : Indexed_Variables,
      'http://terminusdb.com/schema/woql#query' : Sub_Query } :< JSON,
    json_to_woql_ast(Indexed_Variables,WOQL_Args,
                     ['http://terminusdb.com/schema/woql#variable_list'
                      |Path]),
    json_to_woql_ast(Sub_Query,Sub_WOQL,['http://terminusdb.com/schema/woql#query'
                                         |Path]),
    WOQL = distinct(WOQL_Args,Sub_WOQL).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#And',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#query_list' : Indexed_Query_List} :< JSON,
    deindex_list('http://terminusdb.com/schema/woql#query',
                 Indexed_Query_List, Query_List),
    maplist({Path}/[Q,W]>>(
                json_to_woql_ast(Q,W,['http://terminusdb.com/schema/woql#query',
                                      'http://terminusdb.com/schema/woql#query_list'
                                      |Path])
            ), Query_List, WOQL_Queries),
    xfy_list(',',WOQL,WOQL_Queries).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Or',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#query_list' : Indexed_Query_List} :< JSON,
    deindex_list('http://terminusdb.com/schema/woql#query',
                 Indexed_Query_List, Query_List),
    maplist({Path}/[Q,W]>>(
                json_to_woql_ast(Q,W,['http://terminusdb.com/schema/woql#query',
                                      'http://terminusdb.com/schema/woql#query_list'
                                      |Path])
            ), Query_List, WOQL_Queries),
    xfy_list(';',WOQL,WOQL_Queries).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Using',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#collection' : Collection,
      'http://terminusdb.com/schema/woql#query' : Query } :< JSON,
    json_to_woql_ast(Collection,WC,['http://terminusdb.com/schema/woql#collection'
                                    |Path]),
    json_to_woql_ast(Query,WQ,['http://terminusdb.com/schema/woql#query'
                               |Path]),
    do_or_die(
        (WC = Collection_String^^_),
        error(woql_syntax_error(JSON,
                                ['http://terminusdb.com/schema/woql#collection'|Path],
                                Collection), _)),
    WOQL = using(Collection_String,WQ).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#From',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#graph_filter' : Graph_Filter,
      'http://terminusdb.com/schema/woql#query' : Query } :< JSON,
    json_to_woql_ast(Graph_Filter,WG,['http://terminusdb.com/schema/woql#graph_filter'
                                      |Path]),
    json_to_woql_ast(Query,WQ,['http://terminusdb.com/schema/woql#query'
                               |Path]),
    do_or_die(
        (WG = Graph_String^^_),
        error(woql_syntax_error(JSON,
                                ['http://terminusdb.com/schema/woql#graph_filter'|Path],
                                Graph_Filter), _)),
    WOQL = from(Graph_String,WQ).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Into',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#graph' : Graph,
      'http://terminusdb.com/schema/woql#query' : Query } :< JSON,
    json_to_woql_ast(Graph,WG,['http://terminusdb.com/schema/woql#graph'
                               |Path]),
    json_to_woql_ast(Query,WQ,['http://terminusdb.com/schema/woql#query'
                               |Path]),
    do_or_die(
        (WG = Graph_String^^_),
        error(woql_syntax_error(JSON,
                                ['http://terminusdb.com/schema/woql#graph'|Path],
                                Graph), _)),
    WOQL = into(Graph_String,WQ).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Quad',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#subject' : Subject,
      'http://terminusdb.com/schema/woql#predicate' : Predicate,
      'http://terminusdb.com/schema/woql#object' : Object,
      'http://terminusdb.com/schema/woql#graph_filter' : Graph
     } :< JSON,
    json_to_woql_ast(Subject,WQA,['http://terminusdb.com/schema/woql#subject'
                                  |Path]),
    json_to_woql_ast(Predicate,WQB,['http://terminusdb.com/schema/woql#predicate'
                                    |Path]),
    json_to_woql_ast(Object,WQC,['http://terminusdb.com/schema/woql#object'
                                 |Path]),
    json_to_woql_ast(Graph,WG,['http://terminusdb.com/schema/woql#graph'
                               |Path]),

    do_or_die(
        (WG = Graph_String^^_),
        error(woql_syntax_error(JSON,
                                ['http://terminusdb.com/schema/woql#graph_filter'|Path],
                                Graph), _)),

    WOQL = t(WQA,WQB,WQC,Graph_String).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Triple',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#subject' : Subject,
      'http://terminusdb.com/schema/woql#predicate' : Predicate,
      'http://terminusdb.com/schema/woql#object' : Object
     } :< JSON,
    json_to_woql_ast(Subject,WQA,['http://terminusdb.com/schema/woql#subject'
                                  |Path]),
    json_to_woql_ast(Predicate,WQB,['http://terminusdb.com/schema/woql#predicate'
                                    |Path]),
    json_to_woql_ast(Object,WQC,['http://terminusdb.com/schema/woql#object'
                                 |Path]),
    WOQL = t(WQA,WQB,WQC).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Subsumption',JSON,WOQL,Path) :-
    _{ 'http://terminusdb.com/schema/woql#child' : Class_A,
       'http://terminusdb.com/schema/woql#parent' : Class_B
     } :< JSON,
    json_to_woql_ast(Class_A,WQA,['http://terminusdb.com/schema/woql#child'
                                  |Path]),
    json_to_woql_ast(Class_B,WQB,['http://terminusdb.com/schema/woql#parent'
                                  |Path]),
    WOQL = '<<'(WQA,WQB).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Equals',JSON,WOQL,Path) :-
    _{ 'http://terminusdb.com/schema/woql#left': A,
       'http://terminusdb.com/schema/woql#right' : B
     } :< JSON,
    json_to_woql_ast(A,WQA,['http://terminusdb.com/schema/woql#left'
                            |Path]),
    json_to_woql_ast(B,WQB,['http://terminusdb.com/schema/woql#right'
                            |Path]),
    WOQL = '='(WQA,WQB).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Substring',JSON,WOQL,Path) :-
    _{  'http://terminusdb.com/schema/woql#string' : String,
        'http://terminusdb.com/schema/woql#before' : Before,
        'http://terminusdb.com/schema/woql#length' : Length,
        'http://terminusdb.com/schema/woql#after' : After,
        'http://terminusdb.com/schema/woql#substring' : Substring
     } :< JSON,
    json_to_woql_ast(String, WString,['http://terminusdb.com/schema/woql#string'
                                      |Path]),
    json_to_woql_ast(Before, WBefore,['http://terminusdb.com/schema/woql#before'
                                      |Path]),
    json_to_woql_ast(Length, WLength,['http://terminusdb.com/schema/woql#length'
                                      |Path]),
    json_to_woql_ast(After, WAfter,['http://terminusdb.com/schema/woql#after'
                                    |Path]),
    json_to_woql_ast(Substring, WSubstring,['http://terminusdb.com/schema/woql#substring'
                                            |Path]),
    WOQL = sub_string(WString,WBefore,WLength,WAfter,WSubstring).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#ReadObject',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#document_uri' : Doc_ID,
      'http://terminusdb.com/schema/woql#document' : Doc
     } :< JSON,
    json_to_woql_ast(Doc_ID, WID, ['http://terminusdb.com/schema/woql#document_id'
                                   |Path]),
    json_to_woql_ast(Doc, WDoc, ['http://terminusdb.com/schema/woql#document'
                                 |Path]),
    WOQL = read_object(WID,WDoc).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#UpdateObject',JSON,WOQL,_) :-
    _{'http://terminusdb.com/schema/woql#document' : Doc
     } :< JSON,
    /*
    do_or_die(
        (_{'@id' : _ID} :< Doc),
        error(woql_syntax_error(JSON,
                                ['@id'|Path],
                                Doc), _)),
    */
    WOQL = update_object(Doc).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#DeleteObject',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#document_uri' : Doc
     } :< JSON,
    do_or_die(
        (   _{'@id' : ID} :< Doc
        ->  true
        ;   json_to_woql_ast(Doc,ID,['http://terminusdb.com/schema/woql#document_uri'|Path])),
        error(woql_syntax_error(JSON,
                                ['http://terminusdb.com/schema/woql#document_uri'|Path],
                                Doc), _)),
    WOQL = delete_object(ID).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#AddTriple',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#subject' : Subject,
      'http://terminusdb.com/schema/woql#predicate' : Predicate,
      'http://terminusdb.com/schema/woql#object' : Object
     } :< JSON,
    json_to_woql_ast(Subject,WQA,['http://terminusdb.com/schema/woql#subject'
                                  |Path]),
    json_to_woql_ast(Predicate,WQB,['http://terminusdb.com/schema/woql#predicate'
                                    |Path]),
    json_to_woql_ast(Object,WQC,['http://terminusdb.com/schema/woql#object'
                                 |Path]),
    WOQL = insert(WQA,WQB,WQC).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#AddedTriple',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#subject' : Subject,
      'http://terminusdb.com/schema/woql#predicate' : Predicate,
      'http://terminusdb.com/schema/woql#object' : Object
     } :< JSON,
    json_to_woql_ast(Subject,WQA,['http://terminusdb.com/schema/woql#subject'
                                  |Path]),
    json_to_woql_ast(Predicate,WQB,['http://terminusdb.com/schema/woql#predicate'
                                    |Path]),
    json_to_woql_ast(Object,WQC,['http://terminusdb.com/schema/woql#object'
                                 |Path]),
    WOQL = addition(WQA,WQB,WQC).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#RemovedTriple',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#subject' : Subject,
      'http://terminusdb.com/schema/woql#predicate' : Predicate,
      'http://terminusdb.com/schema/woql#object' : Object
     } :< JSON,
    json_to_woql_ast(Subject,WQA,['http://terminusdb.com/schema/woql#subject'
                                  |Path]),
    json_to_woql_ast(Predicate,WQB,['http://terminusdb.com/schema/woql#predicate'
                                    |Path]),
    json_to_woql_ast(Object,WQC,['http://terminusdb.com/schema/woql#object'
                                 |Path]),
    WOQL = removal(WQA,WQB,WQC).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#AddQuad',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#subject' : Subject,
      'http://terminusdb.com/schema/woql#predicate' : Predicate,
      'http://terminusdb.com/schema/woql#object' : Object,
      'http://terminusdb.com/schema/woql#graph' : Graph
     } :< JSON,
    json_to_woql_ast(Subject,WQA,['http://terminusdb.com/schema/woql#subject'
                                  |Path]),
    json_to_woql_ast(Predicate,WQB,['http://terminusdb.com/schema/woql#predicate'
                                    |Path]),
    json_to_woql_ast(Object,WQC,['http://terminusdb.com/schema/woql#object'
                                 |Path]),
    json_to_woql_ast(Graph,WG,['http://terminusdb.com/schema/woql#graph'
                               |Path]),
    do_or_die(
        (WG = Graph_String^^_),
        error(woql_syntax_error(JSON,
                                ['http://terminusdb.com/schema/woql#graph'|Path],
                                Graph), _)),
    WOQL = insert(WQA,WQB,WQC,Graph_String).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#AddedQuad',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#subject' : Subject,
      'http://terminusdb.com/schema/woql#predicate' : Predicate,
      'http://terminusdb.com/schema/woql#object' : Object,
      'http://terminusdb.com/schema/woql#graph_filter' : Graph
     } :< JSON,
    json_to_woql_ast(Subject,WQA,['http://terminusdb.com/schema/woql#subject'
                                  |Path]),
    json_to_woql_ast(Predicate,WQB,['http://terminusdb.com/schema/woql#predicate'
                                    |Path]),
    json_to_woql_ast(Object,WQC,['http://terminusdb.com/schema/woql#object'
                                 |Path]),
    json_to_woql_ast(Graph,WG,['http://terminusdb.com/schema/woql#graph_filter'
                               |Path]),
    do_or_die(
        (WG = Graph_String^^_),
        error(woql_syntax_error(JSON,
                                ['http://terminusdb.com/schema/woql#graph_filter'|Path],
                                Graph), _)),
    WOQL = addition(WQA,WQB,WQC,Graph_String).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#RemovedQuad',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#subject' : Subject,
      'http://terminusdb.com/schema/woql#predicate' : Predicate,
      'http://terminusdb.com/schema/woql#object' : Object,
      'http://terminusdb.com/schema/woql#graph_filter' : Graph
     } :< JSON,
    json_to_woql_ast(Subject,WQA,['http://terminusdb.com/schema/woql#subject'
                                  |Path]),
    json_to_woql_ast(Predicate,WQB,['http://terminusdb.com/schema/woql#predicate'
                                    |Path]),
    json_to_woql_ast(Object,WQC,['http://terminusdb.com/schema/woql#object'
                                 |Path]),
    json_to_woql_ast(Graph,WG,['http://terminusdb.com/schema/woql#graph_filter'
                               |Path]),
    do_or_die(
        (WG = Graph_String^^_),
        error(woql_syntax_error(JSON,
                                ['http://terminusdb.com/schema/woql#graph_filter'|Path],
                                Graph), _)),
    WOQL = removal(WQA,WQB,WQC,Graph_String).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#DeleteTriple',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#subject' : Subject,
      'http://terminusdb.com/schema/woql#predicate' : Predicate,
      'http://terminusdb.com/schema/woql#object' : Object
     } :< JSON,
    json_to_woql_ast(Subject,WQA,['http://terminusdb.com/schema/woql#subject'
                                  |Path]),
    json_to_woql_ast(Predicate,WQB,['http://terminusdb.com/schema/woql#predicate'
                                    |Path]),
    json_to_woql_ast(Object,WQC,['http://terminusdb.com/schema/woql#object'
                                 |Path]),
    WOQL = delete(WQA,WQB,WQC).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#DeleteQuad',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#subject' : Subject,
      'http://terminusdb.com/schema/woql#predicate' : Predicate,
      'http://terminusdb.com/schema/woql#object' : Object,
      'http://terminusdb.com/schema/woql#graph' : Graph
     } :< JSON,
    json_to_woql_ast(Subject,WQA,['http://terminusdb.com/schema/woql#subject'
                                  |Path]),
    json_to_woql_ast(Predicate,WQB,['http://terminusdb.com/schema/woql#predicate'
                                    |Path]),
    json_to_woql_ast(Object,WQC,['http://terminusdb.com/schema/woql#object'
                                 |Path]),
    json_to_woql_ast(Graph,WG,['http://terminusdb.com/schema/woql#graph'
                               |Path]),
    do_or_die(
        (WG = Graph_String^^_),
        error(woql_syntax_error(JSON,
                                ['http://terminusdb.com/schema/woql#graph'|Path],
                                Graph), _)),
    WOQL = delete(WQA,WQB,WQC,Graph_String).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#When',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#query' : Q,
      'http://terminusdb.com/schema/woql#consequent' : U
     } :< JSON,
    json_to_woql_ast(Q,WQ,['http://terminusdb.com/schema/woql#query'
                           |Path]),
    json_to_woql_ast(U,WU,['http://terminusdb.com/schema/woql#consequent'
                           |Path]),
    WOQL = when(WQ,WU).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Trim',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#untrimmed' :  A,
      'http://terminusdb.com/schema/woql#trimmed' : T
     } :< JSON,
    json_to_woql_ast(A,WA,['http://terminusdb.com/schema/woql#untrimmed'
                           |Path]),
    json_to_woql_ast(T,WT,['http://terminusdb.com/schema/woql#trimmed'
                           |Path]),
    WOQL = trim(WA,WT).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Eval',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#expression' : A,
      'http://terminusdb.com/schema/woql#result' : X
     } :< JSON,
    json_to_woql_arith(X, V, ['http://terminusdb.com/schema/woql#result'
                              |Path]),
    json_to_woql_arith(A, Arith, ['http://terminusdb.com/schema/woql#expression'
                                  |Path]),
    WOQL = 'is'(V,Arith).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#IsA',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#element' : X,
      'http://terminusdb.com/schema/woql#of_type' : T
     } :< JSON,
    json_to_woql_ast(X, V, ['http://terminusdb.com/schema/woql#element'
                            |Path]),
    json_to_woql_ast(T, Class,['http://terminusdb.com/schema/woql#of_type'
                               |Path]),
    WOQL = isa(V,Class).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Like',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#left' :  A,
      'http://terminusdb.com/schema/woql#right' : B,
      'http://terminusdb.com/schema/woql#like_similarity' : F
     } :< JSON,
    json_to_woql_ast(A,WA,['http://terminusdb.com/schema/woql#left'
                           |Path]),
    json_to_woql_ast(B,WB,['http://terminusdb.com/schema/woql#right'
                           |Path]),
    json_to_woql_ast(F,WF,['http://terminusdb.com/schema/woql#like_similarity'
                           |Path]),
    WOQL = like(WA,WB,WF).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Less',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#left' : A,
      'http://terminusdb.com/schema/woql#right' : B
     } :< JSON,
    json_to_woql_ast(A,WA,['http://terminusdb.com/schema/woql#left'
                           |Path]),
    json_to_woql_ast(B,WB,['http://terminusdb.com/schema/woql#right'
                           |Path]),
    WOQL = '<'(WA,WB).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Greater',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#left' : A,
      'http://terminusdb.com/schema/woql#right' : B
     } :< JSON,
    json_to_woql_ast(A,WA,['http://terminusdb.com/schema/woql#left'
                           |Path]),
    json_to_woql_ast(B,WB,['http://terminusdb.com/schema/woql#right'
                           |Path]),
    WOQL = '>'(WA,WB).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Optional',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#query' : Q
     } :< JSON,
    json_to_woql_ast(Q,WQ,['http://terminusdb.com/schema/woql#query'
                           |Path]),
    WOQL = 'opt'(WQ).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Get',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#as_vars' : AsVars,
      'http://terminusdb.com/schema/woql#query_resource' : Resource
     } :< JSON,
    json_to_woql_ast(AsVars,WAsVars,['http://terminusdb.com/schema/woql#as_vars'
                                     |Path]),
    json_to_woql_ast(Resource,WResource, ['http://terminusdb.com/schema/woql#query_resource'
                                          |Path]),
    WOQL = get(WAsVars,WResource).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#With',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#graph' : Graph,
      'http://terminusdb.com/schema/woql#query_resource' : Resource,
      'http://terminusdb.com/schema/woql#query' : Query
     } :< JSON,
    json_to_woql_ast(Graph,WGraph, ['http://terminusdb.com/schema/woql#graph'
                                    |Path]),
    json_to_woql_ast(Resource,WResource, ['http://terminusdb.com/schema/woql#query_resource'
                                          |Path]),
    json_to_woql_ast(Query,WQuery,['http://terminusdb.com/schema/woql#query'
                                   |Path]),
    do_or_die(
        (WGraph = Graph_String^^_),
        error(woql_syntax_error(JSON,
                                ['http://terminusdb.com/schema/woql#graph'|Path],
                                Graph), _)),

    WOQL = with(Graph_String,WResource,WQuery).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#NamedAsVar',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#variable_name' : Var_Name,
      'http://terminusdb.com/schema/woql#identifier' : Identifier
     } :< JSON,
    json_to_woql_ast(_{'@type' : 'http://terminusdb.com/schema/woql#Variable',
                       'http://terminusdb.com/schema/woql#variable_name' : Var_Name},
                     WOQL_Var, Path),
    json_to_woql_ast(Identifier, WIdentifier, ['http://terminusdb.com/schema/woql#identifier'
                                               |Path]),
    do_or_die(
        (WIdentifier = ID_String^^_,
         atom_string(ID,ID_String)),
        error(woql_syntax_error(JSON,
                                ['http://terminusdb.com/schema/woql#identifier'|Path],
                                Identifier), _)),
    (   get_dict('http://terminusdb.com/schema/woql#var_type',
                 JSON,
                 Type),
        json_to_woql_ast(Type, WType, ['http://terminusdb.com/schema/woql#var_type'
                                       |Path]),
        atom_string(Type_Atom,WType)
    ->  WOQL = as(ID, WOQL_Var, Type_Atom)
    ;   WOQL = as(ID, WOQL_Var)).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#IndexedAsVar',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#variable_name' : Var_Name
     } :< JSON,
    json_to_woql_ast(
        _{'@type' : 'http://terminusdb.com/schema/woql#Variable',
          'http://terminusdb.com/schema/woql#variable_name' : Var_Name},
        WOQL,Path).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Put',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#as_vars' : Header,
      'http://terminusdb.com/schema/woql#query' : Query,
      'http://terminusdb.com/schema/woql#query_resource' : Resource
     } :< JSON,
    json_to_woql_ast(Header,WHeader,['http://terminusdb.com/schema/woql#as_vars'
                                     |Path]),
    json_to_woql_ast(Query,WQuery,['http://terminusdb.com/schema/woql#query'
                                   |Path]),
    json_to_woql_ast(Resource,WResource,['http://terminusdb.com/schema/woql#query_resource'
                                         |Path]),
    WOQL = put(WHeader,WQuery,WResource).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#RemoteResource',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#remote_uri' : URI
     } :< JSON,
    json_to_woql_ast(URI,WURI,['http://terminusdb.com/schema/woql#remote_uri'
                               |Path]),
    do_or_die(
        (WURI = URI_String^^_),
        error(woql_syntax_error(JSON,
                                ['http://terminusdb.com/schema/woql#remote_uri'
                                 |Path],
                                WURI), _)),

    (   _{'http://terminusdb.com/schema/woql#format' : Format} :< JSON
    ->  json_to_woql_ast(Format,WFormat,
                         ['http://terminusdb.com/schema/woql#format'
                          |Path])
    ;   WFormat = _{}),

    WOQL = remote(URI_String,WFormat).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#FileResource',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#file' : File
     } :< JSON,
    json_to_woql_ast(File,WFile,['http://terminusdb.com/schema/woql#file'
                                 |Path]),
    do_or_die(
        (WFile = File_String^^_),
        error(woql_syntax_error(JSON,
                                ['http://terminusdb.com/schema/woql#file'|Path],
                                WFile), _)),
    (   _{'http://terminusdb.com/schema/woql#format' : Format} :< JSON
    ->  json_to_woql_ast(Format,WFormat,
                         ['http://terminusdb.com/schema/woql#format'
                          |Path])
    ;   WFormat = _{}),

    WOQL = file(File_String,WFormat).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#PostResource',JSON,WOQL,Path) :-
    _{
        'http://terminusdb.com/schema/woql#file' : File
    } :< JSON,
    json_to_woql_ast(File, WFile, ['http://terminusdb.com/schema/woql#file'
                                   |Path]),
    do_or_die(
        (WFile = File_String^^_),
        error(woql_syntax_error(JSON,
                                ['http://terminusdb.com/schema/woql#file'|Path],
                                WFile), _)),
    (   _{'http://terminusdb.com/schema/woql#format' : Format} :< JSON
    ->  json_to_woql_ast(Format,WFormat,
                         ['http://terminusdb.com/schema/woql#format'
                          |Path])
    ;   WFormat = _{}),

    WOQL = post(File_String,WFormat).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Format',JSON,WOQL,_) :-
    json_extract_format(JSON,WOQL).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Unique',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#base' : Base,
      'http://terminusdb.com/schema/woql#key_list' : Key,
      'http://terminusdb.com/schema/woql#uri' : URI
     } :< JSON,
    json_to_woql_ast(Base,WBase,['http://terminusdb.com/schema/woql#base'
                                 |Path]),
    json_to_woql_ast(Key,WKey,['http://terminusdb.com/schema/woql#key_list'
                               |Path]),
    json_to_woql_ast(URI,WURI,['http://terminusdb.com/schema/woql#uri'
                               |Path]),
    WOQL = hash(WBase,WKey,WURI).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#IDGenerator',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#base' : Base,
      'http://terminusdb.com/schema/woql#key_list' : Key,
      'http://terminusdb.com/schema/woql#uri' : URI
     } :< JSON,
    json_to_woql_ast(Base,WBase,['http://terminusdb.com/schema/woql#base'
                                 |Path]),
    json_to_woql_ast(Key,WKey,['http://terminusdb.com/schema/woql#key_list'
                               |Path]),
    json_to_woql_ast(URI,WURI,['http://terminusdb.com/schema/woql#uri'
                               |Path]),
    WOQL = idgen(WBase,WKey,WURI).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Upper',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#left' :  S,
      'http://terminusdb.com/schema/woql#right' : V
     } :< JSON,
    json_to_woql_ast(S,WS,['http://terminusdb.com/schema/woql#left'
                           |Path]),
    json_to_woql_ast(V,WV,['http://terminusdb.com/schema/woql#right'
                           |Path]),
    WOQL = upper(WS,WV).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Lower',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#left' : S,
      'http://terminusdb.com/schema/woql#right' : V
     } :< JSON,
    json_to_woql_ast(S,WS,['http://terminusdb.com/schema/woql#left'
                           |Path]),
    json_to_woql_ast(V,WV,['http://terminusdb.com/schema/woql#right'
                           |Path]),
    WOQL = lower(WS,WV).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Pad',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#pad_string' : S,
      'http://terminusdb.com/schema/woql#pad_char' : C,
      'http://terminusdb.com/schema/woql#pad_times' : N,
      'http://terminusdb.com/schema/woql#pad_result' : V
     } :< JSON,
    json_to_woql_ast(S,WS,['http://terminusdb.com/schema/woql#pad_string'
                           |Path]),
    json_to_woql_ast(C,WC,['http://terminusdb.com/schema/woql#pad_char'
                           |Path]),
    json_to_woql_ast(N,WN,['http://terminusdb.com/schema/woql#pad_times'
                           |Path]),
    json_to_woql_ast(V,WV,['http://terminusdb.com/schema/woql#pad_result'
                           |Path]),
    WOQL = pad(WS,WC,WN,WV).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Split',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#split_string' : S,
      'http://terminusdb.com/schema/woql#split_pattern' : P,
      'http://terminusdb.com/schema/woql#split_list' : L
     } :< JSON,
    json_to_woql_ast(S,WS,['http://terminusdb.com/schema/woql#split_string'
                           |Path]),
    json_to_woql_ast(P,WP,['http://terminusdb.com/schema/woql#split_pattern'
                           |Path]),
    json_to_woql_ast(L,WL,['http://terminusdb.com/schema/woql#split_list'
                           |Path]),
    WOQL = split(WS,WP,WL).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Member',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#member' : S,
      'http://terminusdb.com/schema/woql#member_list' : L
     } :< JSON,
    json_to_woql_ast(S,WS,['http://terminusdb.com/schema/woql#member'
                           |Path]),
    json_to_woql_ast(L,WL,['http://terminusdb.com/schema/woql#member_list'
                           |Path]),
    WOQL = member(WS,WL).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Concatenate',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#concat_list' :  List,
      'http://terminusdb.com/schema/woql#concatenated' : Value
     } :< JSON,
    json_to_woql_ast(List,WList,['http://terminusdb.com/schema/woql#concat_list'
                                 |Path]),
    json_to_woql_ast(Value,WValue,['http://terminusdb.com/schema/woql#concatenated'
                                   |Path]),
    WOQL = concat(WList,WValue).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Join',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#join_list' : List,
      'http://terminusdb.com/schema/woql#join_separator' : Sep,
      'http://terminusdb.com/schema/woql#join' : Value
     } :< JSON,
    json_to_woql_ast(List,WList,['http://terminusdb.com/schema/woql#join_list'
                                 |Path]),
    json_to_woql_ast(Sep,WSep,['http://terminusdb.com/schema/woql#join_separator'
                               |Path]),
    json_to_woql_ast(Value,WValue,['http://terminusdb.com/schema/woql#join'
                                   |Path]),
    WOQL = join(WList,WSep,WValue).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Sum',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#sum_list' : List,
      'http://terminusdb.com/schema/woql#sum' : Value
     } :< JSON,
    json_to_woql_ast(List,WList,['http://terminusdb.com/schema/woql#sum_list'
                                 |Path]),
    json_to_woql_ast(Value,WValue,['http://terminusdb.com/schema/woql#sum'
                                   |Path]),
    WOQL = sum(WList,WValue).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Count',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#query' : Query,
      'http://terminusdb.com/schema/woql#count' : Count
     } :< JSON,
    json_to_woql_ast(Query,WQuery,['http://terminusdb.com/schema/woql#query'
                                  |Path]),
    json_to_woql_ast(Count,WCount,['http://terminusdb.com/schema/woql#count'
                                   |Path]),
    WOQL = count(WQuery,WCount).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Start',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#start' : N,
      'http://terminusdb.com/schema/woql#query' : Q
     } :< JSON,
    json_to_woql_ast(N,WN,['http://terminusdb.com/schema/woql#start'
                           |Path]),
    json_to_woql_ast(Q,WQ,['http://terminusdb.com/schema/woql#query'
                           |Path]),
    do_or_die(
        (WN = Num^^_),
        error(woql_syntax_error(JSON,
                                ['http://terminusdb.com/schema/woql#start'|Path],
                                N), _)),
    WOQL = start(Num, WQ).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Limit',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#limit' :  N,
      'http://terminusdb.com/schema/woql#query' : Q
     } :< JSON,
    json_to_woql_ast(N,WN,['http://terminusdb.com/schema/woql#limit'
                           |Path]),
    json_to_woql_ast(Q,WQ,['http://terminusdb.com/schema/woql#query'
                           |Path]),
    WOQL = limit(WN, WQ).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Regexp',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#pattern' : Pat,
      'http://terminusdb.com/schema/woql#regexp_string' : String,
      'http://terminusdb.com/schema/woql#regexp_list' : List
     } :< JSON,
    json_to_woql_ast(Pat,WPat,['http://terminusdb.com/schema/woql#pattern'
                               |Path]),
    json_to_woql_ast(String,WString,['http://terminusdb.com/schema/woql#regexp_string'
                                     |Path]),
    json_to_woql_ast(List,WList,['http://terminusdb.com/schema/woql#regexp_list'
                                 |Path]),
    WOQL = re(WPat, WString, WList).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#OrderBy',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#variable_ordering' : Templates,
      'http://terminusdb.com/schema/woql#query' : Query
     } :< JSON,
    json_to_woql_ast(Templates,WTemplates,['http://terminusdb.com/schema/woql#variable_list'
                                           |Path]),
    json_to_woql_ast(Query,WQuery,['http://terminusdb.com/schema/woql#query'
                                   |Path]),
    WOQL = order_by(WTemplates,WQuery).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#VariableOrdering',JSON,WOQL,Path) :-
    _{  'http://terminusdb.com/schema/woql#variable' : Var
     } :< JSON,
    (   get_dict('http://terminusdb.com/schema/woql#ascending', JSON, Bool)
    ->  json_to_woql_ast(Bool,Boolean^^_,['http://terminusdb.com/schema/woql#ascending'
                                          |Path])
    ;   Boolean = true),
    json_to_woql_ast(Var,WVar,['http://terminusdb.com/schema/woql#ascending'
                               |Path]),
    (   Boolean = true
    ->  Order = asc
    ;   Order = desc),
    WOQL =.. [Order,WVar].
json_type_to_woql_ast('http://terminusdb.com/schema/woql#GroupBy',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#group_by' : Spec,
      'http://terminusdb.com/schema/woql#group_template' : Obj,
      'http://terminusdb.com/schema/woql#query' : Query,
      'http://terminusdb.com/schema/woql#grouped' : Collector
     } :< JSON,
    json_to_woql_ast(Spec,WSpec,['http://terminusdb.com/schema/woql#group_by'
                                 |Path]),
    json_to_woql_ast(Obj,WObj,['http://terminusdb.com/schema/woql#group_template'
                               |Path]),
    json_to_woql_ast(Query,WQuery,['http://terminusdb.com/schema/woql#query'
                                   |Path]),
    json_to_woql_ast(Collector,WCollector,['http://terminusdb.com/schema/woql#grouped'
                                           |Path]),
    WOQL = group_by(WSpec,WObj,WQuery,WCollector).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Length',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#length_list' : A,
      'http://terminusdb.com/schema/woql#length' : B
     } :< JSON,
    json_to_woql_ast(A,WA,['http://terminusdb.com/schema/woql#length_list'
                           |Path]),
    json_to_woql_ast(B,WB,['http://terminusdb.com/schema/woql#length'
                           |Path]),
    WOQL = length(WA,WB).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Typecast',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#typecast_value' : Val,
      'http://terminusdb.com/schema/woql#typecast_type' : Type,
      'http://terminusdb.com/schema/woql#typecast_result' : Var
     } :< JSON,
    json_to_woql_ast(Val,WVal,['http://terminusdb.com/schema/woql#typecast_value'
                               |Path]),
    json_to_woql_ast(Type,WType,['http://terminusdb.com/schema/woql#typecast_type'
                                 |Path]),
    json_to_woql_ast(Var,WVar,['http://terminusdb.com/schema/woql#typecast_result'
                               |Path]),
    WOQL = typecast(WVal,WType,[],WVar).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Not',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#query' : Q
     } :< JSON,
    json_to_woql_ast(Q,WQ,['http://terminusdb.com/schema/woql#query'
                           |Path]),
    WOQL = not(WQ).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Once',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#query' : Q
     } :< JSON,
    json_to_woql_ast(Q,WQ,['http://terminusdb.com/schema/woql#query'
                           |Path]),
    WOQL = once(WQ).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Immediately',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#query' : Q
     } :< JSON,
    json_to_woql_ast(Q,WQ,['http://terminusdb.com/schema/woql#query'
                           |Path]),
    WOQL = immediately(WQ).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Dot',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#dictionary' : Dictionary,
      'http://terminusdb.com/schema/woql#dictionary_key' : Key,
      'http://terminusdb.com/schema/woql#dictionary_value' : Value
     } :< JSON,
    json_to_woql_ast(Dictionary, WDictionary, ['http://terminusdb.com/schema/woql#dictionary_key'
                                               |Path]),
    json_to_woql_ast(Key, WKey, ['http://terminusdb.com/schema/woql#dictionary_key'
                                 |Path]),
    json_to_woql_ast(Value,WValue,['http://terminusdb.com/schema/woql#dictionary_value'
                                   |Path]),
    WOQL = dot(WDictionary,WKey,WValue).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Path',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#subject' : Subject,
      'http://terminusdb.com/schema/woql#path_pattern' : Pattern,
      'http://terminusdb.com/schema/woql#object' : Object,
      'http://terminusdb.com/schema/woql#path' : Edge_Path
     } :< JSON,
    json_to_woql_ast(Subject,WSubject,['http://terminusdb.com/schema/woql#subject'
                                       |Path]),
    json_to_woql_path_pattern(Pattern,WPattern,['http://terminusdb.com/schema/woql#path_pattern'
                                                |Path]),
    json_to_woql_ast(Object,WObject,['http://terminusdb.com/schema/woql#object'
                                     |Path]),
    json_to_woql_ast(Edge_Path,WEdge_Path,['http://terminusdb.com/schema/woql#path'
                                           |Path]),
    WOQL = path(WSubject,WPattern,WObject,WEdge_Path).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#True',_,WOQL,_) :-
    WOQL = true.
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Array',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#array_element' : List} :< JSON,
    % Unbox values
    json_to_woql_ast(List, WOQL, ['http://terminusdb.com/schema/woql#array_element'
                                  |Path]).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#ArrayElement',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#variable_name' : Value} :< JSON,
    % Unbox values
    json_to_woql_ast(Value, Result, ['http://terminusdb.com/schema/woql#variable_name'
                                     |Path]),
    Result = String_or_Atom_Name^^_,
    coerce_atom(String_or_Atom_Name, Atom_Name),
    WOQL = v(Atom_Name).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#ArrayElement',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#node' : Node} :< JSON,
    % Unbox values
    json_to_woql_ast(Node, WOQL, ['http://terminusdb.com/schema/woql#node'
                                  |Path]).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#ArrayElement',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#datatype' : DT} :< JSON,
    % Unbox values
    json_to_woql_ast(DT, WOQL, ['http://terminusdb.com/schema/woql#datatype'
                                |Path]).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#VariableListElement',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#variable_name' : Value} :< JSON,
    % Unbox values
    json_to_woql_ast(Value, Result, ['http://terminusdb.com/schema/woql#variable_name'
                                     |Path]),
    Result = String_or_Atom_Name^^_,
    coerce_atom(String_or_Atom_Name, Atom_Name),
    WOQL = v(Atom_Name).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#IndexedAsVars',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#indexed_as_var' : List} :< JSON,
    % Unbox values
    json_to_woql_ast(List, WOQL, ['http://terminusdb.com/schema/woql#indexed_as_var'
                                  |Path]).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#AsVars',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#named_as_var' : List} :< JSON,
    % Unbox values
    json_to_woql_ast(List, WOQL, ['http://terminusdb.com/schema/woql#named_as_var'
                                  |Path]).

json_type_to_woql_ast('http://terminusdb.com/schema/woql#Datatype',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#datatype': Value} :< JSON,
    % Unbox values
    json_to_woql_ast(Value, WOQL, ['http://terminusdb.com/schema/woql#datatype'
                                   |Path]).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Node',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#node': Node} :< JSON,
    % Unbox values
    json_to_woql_ast(Node, WOQL, ['http://terminusdb.com/schema/woql#node'
                                  |Path]).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#Variable',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#variable_name' : Name} :< JSON,
    json_to_woql_ast(Name, Result, ['http://terminusdb.com/schema/woql#variable_name'
                                    |Path]),
    Result = String_or_Atom_Name^^_,
    coerce_atom(String_or_Atom_Name, Atom_Name),
    WOQL = v(Atom_Name).
json_type_to_woql_ast('http://terminusdb.com/schema/woql#TypeOf',JSON,WOQL,Path) :-
    _{'http://terminusdb.com/schema/woql#value' : Value,
      'http://terminusdb.com/schema/woql#type' : Type
     } :< JSON,
    json_to_woql_ast(Value,WOQL_Value,
                     ['http://terminusdb.com/schema/woql#value'
                      |Path]),
    json_to_woql_ast(Type,WOQL_Type,
                     ['http://terminusdb.com/schema/woql#type'
                      |Path]),

    WOQL = typeof(WOQL_Value,WOQL_Type).

json_to_woql_path_pattern(JSON,Pattern,Path) :-
    is_dict(JSON),
    !,
    (   _{'@type' : 'http://terminusdb.com/schema/woql#PathPredicate',
          'http://terminusdb.com/schema/woql#path_predicate' : Node} :< JSON
    ->   json_to_woql_ast(Node,WNode,['http://terminusdb.com/schema/woql#path_predicate'
                                      |Path]),
         Pattern = p(WNode)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#InvertedPathPredicate',
          'http://terminusdb.com/schema/woql#path_predicate' : Node} :< JSON
    ->   json_to_woql_ast(Node,WNode,['http://terminusdb.com/schema/woql#path_predicate'
                                      |Path]),
         Pattern = n(WNode)
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
                                      ['http://terminusdb.com/schema/woql#path_pattern'
                                       |Path]),
            Pattern = plus(PSubPattern)
    ;   _{'@type' : 'http://terminusdb.com/schema/woql#PathStar',
          'http://terminusdb.com/schema/woql#path_pattern' : SubPattern} :< JSON
        ->  json_to_woql_path_pattern(SubPattern,PSubPattern,
                                      ['http://terminusdb.com/schema/woql#path_pattern'
                                       |Path]),
            Pattern = star(PSubPattern)
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
            do_or_die(
                (WN = N_int ^^ _),
                error(woql_syntax_error(JSON,
                                        ['http://terminusdb.com/schema/woql#path_minimum'|Path],
                                        N), _)),

            do_or_die(
                (WM = M_int ^^ _),
                error(woql_syntax_error(JSON,
                                        ['http://terminusdb.com/schema/woql#path_maximum'|Path],
                                        M), _)),
            Pattern = times(PSubPattern,N_int,M_int)
    ;   throw(error(woql_syntax_error(JSON,Path,JSON), _))
    ).


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
        once(typecast(V^^'http://www.w3.org/2001/XMLSchema#string',
                      TE, [], Val)),
        WOQL = Val
    ;   throw(error(woql_syntax_error(JSON,Path,JSON), _))
    ).
json_to_woql_arith(JSON,_,Path) :-
    throw(error(woql_syntax_error(JSON,Path,JSON), _)).


json_extract_format(JSON,WOQL) :-
    (   _{'http://terminusdb.com/schema/woql#format_type' : Format_Type}
        :< JSON
    ->  json_to_woql_ast(Format_Type,WFormat_Type,
                         ['http://terminusdb.com/schema/woql#format_type'
                          |Path]),
        do_or_die(
            (WFormat_Type = Format_Type_String^^_),
            error(woql_syntax_error(JSON,
                                    ['http://terminusdb.com/schema/woql#format_type'|Path],
                                    WFile), _))
    ;   Format_Type_String = "csv"),

    (   _{'http://terminusdb.com/schema/woql#format_header' : Format_Header}
        :< JSON
    ->  json_to_woql_ast(Format_Header,WFormat_Header,
                         ['http://terminusdb.com/schema/woql#format_type'
                          |Path]),
        do_or_die(
            (WFormat_Header = Format_Header_Bool^^_),
            error(woql_syntax_error(
                      JSON,
                      ['http://terminusdb.com/schema/woql#format_header'|Path],
                      WFile),
                      _))
    ;   Format_Header_Bool = true),

    WOQL = _{ 'format_type' : Format_Type_String ,
              'format_header' : Format_Header_Bool }.


json_woql_path_element_error_message(_JSON,Path,Element,Message) :-
    (   Path = [Head|_Path],
        woql_element_error_message(Head,Element,Message)
    ->  true
    ;   format(string(Message),'Not well formed WOQL JSON-LD', [])).

woql_element_error_message(
    'http://terminusdb.com/schema/woql#resource',
    Element,
    Message) :-
    format(string(Message),'Poorly formed resource descriptor: ~q',Element).
woql_element_error_message(
    'http://terminusdb.com/schema/woql#collection',
    Element,
    Message) :-
    format(string(Message),'Poorly formed collection descriptor: ~q',Element).
woql_element_error_message(
    'http://terminusdb.com/schema/woql#collection',
    Element,
    Message) :-
    format(string(Message),'Poorly formed collection descriptor: ~q',Element).
woql_element_error_message(
    'http://terminusdb.com/schema/woql#graph_filter',
    Element,
    Message) :-
    format(string(Message),'Poorly formed graph filter: ~q',Element).
woql_element_error_message(
    'http://terminusdb.com/schema/woql#graph',
    Element,
    Message) :-
    format(string(Message),'Poorly formed graph resource: ~q',Element).
woql_element_error_message(
    '@id',
    Element,
    Message) :-
    format(string(Message),'Document has no id: ~q',Element).
woql_element_error_message(
    'http://terminusdb.com/schema/woql#identifier',
    Element,
    Message) :-
    format(string(Message),'Not a well formed variable identifier: ~q',Element).
woql_element_error_message(
    'http://terminusdb.com/schema/woql#start',
    Element,
    Message) :-
    format(string(Message),'Poorly formed start: ~q, should be a positive integer',Element).
woql_element_error_message(
    'http://terminusdb.com/schema/woql#path_minimum',
    Element,
    Message) :-
    format(string(Message),'Poorly formed path min: ~q, should be a positive integer',Element).
woql_element_error_message(
    'http://terminusdb.com/schema/woql#path_maximum',
    Element,
    Message) :-
    format(string(Message),'Poorly formed path max: ~q, should be a positive integer',Element).
woql_element_error_message(
    'http://terminusdb.com/schema/woql#result',
    Element,
    Message) :-
    format(string(Message),'Not a well formed arithmetic result: ~q',Element).
woql_element_error_message(
    'http://terminusdb.com/schema/woql#expression',
    Element,
    Message) :-
    format(string(Message),'Not a well formed arithmetic expression: ~q',Element).

:- begin_tests(woql_jsonld).

test(not_a_query, []) :-
    JSON = _{'@type' : "And",
             query_list :
             [_{'@type' : "QueryListElement",
                index : _{'@type' : "xsd:integer",
                          '@value' : 0},
                query : _{'@type' : "Frob"}}]},

    woql_context(Prefixes),
    catch(
        json_woql(JSON,Prefixes,_WOQL),
        error(woql_syntax_error(Q,P,E), _),
        json_woql_path_element_error_message(Q,P,E,Message)),
    Message = "Not well formed WOQL JSON-LD".

test(post_marshalling, []) :-
    JSON = _{'@type' : "Get",
             as_vars : [_{'@type': "NamedAsVar",
                          identifier:
                          _{'@type': "xsd:string",
                            '@value': "Start station"},
                          variable_name:
                          _{'@type': "xsd:string",
                            '@value': "Start_Station"}}
                       ],
             query_resource:
             _{'@type': "PostResource",
               file: _{'@type': "xsd:string",
                       '@value': "bike_csv"}}},

    woql_context(Prefixes),
    json_woql(JSON,Prefixes,WOQL),
    WOQL = get(['Start station'as v('Start_Station')],
               post("bike_csv",_)).



test(var_simplification, []) :-
    JSON = _{'@type' : "Get",
             as_vars : [_{'@type': "NamedAsVar",
                          identifier: "Start station",
                          variable_name: "Start_Station"}
                       ],
             query_resource:
             _{'@type': "PostResource",
               file: "bike_csv"}},
    * json_write_dict(current_output,JSON,[]),
    woql_context(Prefixes),
    json_woql(JSON,Prefixes,WOQL),
    WOQL = get(['Start station'as v('Start_Station')],
               post("bike_csv",_)).


test(id_simplification, []) :-

    JSON_Atom = '{
  "@type": "IDGenerator",
  "base": {
      "@type": "Node",
      "node": "Journey"
   },
  "key_list": {
    "@type": "Array",
    "array_element": [
      {
        "@type": "ArrayElement",
        "datatype": {
          "@type": "xsd:string",
          "@value": "test"
        },
        "index": 0
      }
    ]
  },
  "uri": {
    "@type": "Variable",
    "variable_name": "Journey_ID"
  }
}',
    atom_json_dict(JSON_Atom, JSON, []),
    woql_context(Prefixes),
    json_woql(JSON,Prefixes,WOQL),
    WOQL = idgen('http://terminusdb.com/schema/woql#Journey',
                 ["test"^^'http://www.w3.org/2001/XMLSchema#string'],
                 v('Journey_ID')).

test(id_simplification_2, []) :-

    JSON_Atom = '{
  "@type": "IDGenerator",
  "base": {
      "@type": "Node",
      "node": "Journey"
   },
  "key_list": {
    "@type": "Array",
    "array_element": [
       {"@type": "Datatype",
        "datatype": {
          "@type": "xsd:string",
          "@value": "test"
        }
       }
    ]
  },
  "uri": {
    "@type": "Variable",
    "variable_name": "Journey_ID"
  }
}',
    atom_json_dict(JSON_Atom, JSON, []),
    woql_context(Prefixes),
    json_woql(JSON,Prefixes,WOQL),
    WOQL = idgen('http://terminusdb.com/schema/woql#Journey',
                 ["test"^^'http://www.w3.org/2001/XMLSchema#string'],
                 v('Journey_ID')).
:- end_tests(woql_jsonld).

:- begin_tests(jsonld_ast).

test(anySimpleType, [blocked('Not yet implemented')]) :-

    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "Variable",
                           "variable_name": "X"},
                 "right": { "@type": "Datatype",
                            "datatype": { "@value": "Something",
                                          "@type": "xsd:anySimpleType"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    woql_context(Prefixes),
    json_woql(JSON, Prefixes, _WOQL).

test(string, []) :-

    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "Variable",
                           "variable_name": "X"},
                 "right": { "@type": "Datatype",
                            "datatype": { "@value": "Something",
                                          "@type": "xsd:string"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    woql_context(Prefixes),
    json_woql(JSON, Prefixes, WOQL),
    WOQL = (v('X')="Something"^^'http://www.w3.org/2001/XMLSchema#string').

test(boolean_true, []) :-

    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "Variable",
                           "variable_name": "X"},
                 "right": { "@type": "Datatype",
                            "datatype": { "@value": "true",
                                          "@type": "xsd:boolean"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    woql_context(Prefixes),
    json_woql(JSON, Prefixes, WOQL),
    WOQL = (v('X')=true^^'http://www.w3.org/2001/XMLSchema#boolean').

test(boolean_true_concrete, []) :-

    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "Variable",
                           "variable_name": "X"},
                 "right": { "@type": "Datatype",
                            "datatype": { "@value": true,
                                          "@type": "xsd:boolean"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    woql_context(Prefixes),
    json_woql(JSON, Prefixes, WOQL),
    WOQL = (v('X')=true^^'http://www.w3.org/2001/XMLSchema#boolean').

test(boolean_true_bare, []) :-

    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "Variable",
                           "variable_name": "X"},
                 "right": { "@type": "Datatype",
                            "datatype": true}}',
    atom_json_dict(JSON_Atom, JSON, []),
    woql_context(Prefixes),
    json_woql(JSON, Prefixes, WOQL),
    WOQL = (v('X')=true^^'http://www.w3.org/2001/XMLSchema#boolean').


test(decimal_bare, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "Variable",
                           "variable_name": "X"},
                 "right": { "@type": "Datatype",
                            "datatype": 1.3}}',
    atom_json_dict(JSON_Atom, JSON, []),
    woql_context(Prefixes),
    json_woql(JSON, Prefixes, WOQL),
    WOQL = (v('X')=1.3^^'http://www.w3.org/2001/XMLSchema#decimal').

test(decimal_typed, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "Variable",
                           "variable_name": "X"},
                 "right": { "@type": "Datatype",
                            "datatype": { "@value": 1.3,
                                          "@type": "xsd:decimal"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    woql_context(Prefixes),
    json_woql(JSON, Prefixes, WOQL),
    WOQL = (v('X')=1.3^^'http://www.w3.org/2001/XMLSchema#decimal').

test(decimal, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "Variable",
                           "variable_name": "X"},
                 "right": { "@type": "Datatype",
                            "datatype": { "@value": "1.3",
                                          "@type": "xsd:decimal"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    woql_context(Prefixes),
    json_woql(JSON, Prefixes, WOQL),
    WOQL = (v('X')=1.3^^'http://www.w3.org/2001/XMLSchema#decimal').

test(date, [blocked('Marshalls correctly but not correctly treated in db')]) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "Variable",
                           "variable_name": "X"},
                 "right": { "@type": "Datatype",
                            "datatype": { "@value": "1066-09-18",
                                          "@type": "xsd:date"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    woql_context(Prefixes),
    json_woql(JSON, Prefixes, WOQL),
    WOQL = (v('X')=date(1066,9,18)^^'http://www.w3.org/2001/XMLSchema#date').

test(time, [blocked('unimplemented')]) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "Variable",
                           "variable_name": "X"},
                 "right": { "@type": "Datatype",
                            "datatype": { "@value": "15:29:44",
                                          "@type": "xsd:time"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    woql_context(Prefixes),
    json_woql(JSON, Prefixes, WOQL),
    WOQL = (v('X')=time(15,29,44,_,_)^^'http://www.w3.org/2001/XMLSchema#time').

test(dateTime, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "Variable",
                           "variable_name": "X"},
                 "right": { "@type": "Datatype",
                            "datatype": { "@value": "2004-04-12T13:20:00",
                                          "@type": "xsd:dateTime"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    woql_context(Prefixes),
    json_woql(JSON, Prefixes, WOQL),
    WOQL = (v('X')=date(2004,04,12,13,20,0,0,-,-)^^'http://www.w3.org/2001/XMLSchema#dateTime').

test(dateTimeStamp, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "Variable",
                           "variable_name": "X"},
                 "right": { "@type": "Datatype",
                            "datatype": { "@value": "2004-04-12T13:20:00-05:00",
                                          "@type": "xsd:dateTimeStamp"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    woql_context(Prefixes),
    json_woql(JSON, Prefixes, WOQL),
    WOQL = (v('X')=date(2004,4,12,13,20,0,-18000,-,-)^^'http://www.w3.org/2001/XMLSchema#dateTimeStamp').

test(byte, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "Variable",
                           "variable_name": "X"},
                 "right": { "@type": "Datatype",
                            "datatype": { "@value": 120,
                                          "@type": "xsd:byte"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    woql_context(Prefixes),
    json_woql(JSON, Prefixes, WOQL),
    WOQL = (v('X')=120^^'http://www.w3.org/2001/XMLSchema#byte').

test(short, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "Variable",
                           "variable_name": "X"},
                 "right": { "@type": "Datatype",
                            "datatype": { "@value": 120,
                                          "@type": "xsd:short"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    woql_context(Prefixes),
    json_woql(JSON, Prefixes, WOQL),
    WOQL = (v('X')=120^^'http://www.w3.org/2001/XMLSchema#short').

test(integer, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "Variable",
                           "variable_name": "X"},
                 "right": { "@type": "Datatype",
                            "datatype": { "@value": 120,
                                          "@type": "xsd:integer"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    woql_context(Prefixes),
    json_woql(JSON, Prefixes, WOQL),
    WOQL = (v('X')=120^^'http://www.w3.org/2001/XMLSchema#integer').

test(long, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "Variable",
                           "variable_name": "X"},
                 "right": { "@type": "Datatype",
                            "datatype": { "@value": 120,
                                          "@type": "xsd:long"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    woql_context(Prefixes),
    json_woql(JSON, Prefixes, WOQL),
    WOQL = (v('X')=120^^'http://www.w3.org/2001/XMLSchema#long').

test(unsignedByte, [blocked('unimplemented')]) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "Variable",
                           "variable_name": "X"},
                 "right": { "@type": "Datatype",
                            "datatype": { "@value": 120,
                                          "@type": "xsd:unsignedByte"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    woql_context(Prefixes),
    json_woql(JSON, Prefixes, WOQL),
    WOQL = (v('X')=120^^'http://www.w3.org/2001/XMLSchema#unsignedByte').

test(unsignedShort, [blocked('unimplemented')]) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "Variable",
                           "variable_name": "X"},
                 "right": { "@type": "Datatype",
                            "datatype": { "@value": 120,
                                          "@type": "xsd:unsignedShort"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    woql_context(Prefixes),
    json_woql(JSON, Prefixes, WOQL),
    WOQL = (v('X')=120^^'http://www.w3.org/2001/XMLSchema#unsignedShort').

test(unsignedInt, [blocked('unimplemented')]) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "Variable",
                           "variable_name": "X"},
                 "right": { "@type": "Datatype",
                            "datatype": { "@value": 120,
                                          "@type": "xsd:unsignedInt"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    woql_context(Prefixes),
    json_woql(JSON, Prefixes, WOQL),
    WOQL = (v('X')=120^^'http://www.w3.org/2001/XMLSchema#unsignedInt').

test(unsignedLong, [blocked('unimplemented')]) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "Variable",
                           "variable_name": "X"},
                 "right": { "@type": "Datatype",
                            "datatype": { "@value": 120,
                                          "@type": "xsd:unsignedLong"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    woql_context(Prefixes),
    json_woql(JSON, Prefixes, WOQL),
    WOQL = (v('X')=120^^'http://www.w3.org/2001/XMLSchema#unsignedLong').

test(positiveInteger, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "Variable",
                           "variable_name": "X"},
                 "right": { "@type": "Datatype",
                            "datatype": { "@value": 120,
                                          "@type": "xsd:positiveInteger"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    woql_context(Prefixes),
    json_woql(JSON, Prefixes, WOQL),
    WOQL = (v('X')=120^^'http://www.w3.org/2001/XMLSchema#positiveInteger').

test(nonNegativeInteger, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "Variable",
                           "variable_name": "X"},
                 "right": { "@type": "Datatype",
                            "datatype": { "@value": 120,
                                          "@type": "xsd:nonNegativeInteger"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    woql_context(Prefixes),
    json_woql(JSON, Prefixes, WOQL),
    WOQL = (v('X')=120^^'http://www.w3.org/2001/XMLSchema#nonNegativeInteger').

test(negativeInteger, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "Variable",
                           "variable_name": "X"},
                 "right": { "@type": "Datatype",
                            "datatype": { "@value": -120,
                                          "@type": "xsd:negativeInteger"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    woql_context(Prefixes),
    json_woql(JSON, Prefixes, WOQL),
    WOQL = (v('X')=(-120)^^'http://www.w3.org/2001/XMLSchema#negativeInteger').

test(nonPositiveInteger, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "Variable",
                           "variable_name": "X"},
                 "right": { "@type": "Datatype",
                            "datatype": { "@value": -120,
                                          "@type": "xsd:nonPositiveInteger"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    woql_context(Prefixes),
    json_woql(JSON, Prefixes, WOQL),
    WOQL = (v('X')=(-120)^^'http://www.w3.org/2001/XMLSchema#nonPositiveInteger').

test(hexBinary, [blocked('unimplemented')]) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "Variable",
                           "variable_name": "X"},
                 "right": { "@type": "Datatype",
                            "datatype": { "@value": "a03bc23",
                                          "@type": "xsd:hexBinary"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    woql_context(Prefixes),
    json_woql(JSON, Prefixes, WOQL),
    WOQL = (v('X')="a03bc23"^^'http://www.w3.org/2001/XMLSchema#hexBinary').

test(base64binary, [blocked('unimplemented')]) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "Variable",
                           "variable_name": "X"},
                 "right": { "@type": "Datatype",
                            "datatype": { "@value": "YXNkZg==",
                                          "@type": "xsd:base64Binary"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    woql_context(Prefixes),
    json_woql(JSON, Prefixes, WOQL),
    WOQL = (v('X')="YXNkZg=="^^'http://www.w3.org/2001/XMLSchema#base64Binary').

:- end_tests(jsonld_ast).
