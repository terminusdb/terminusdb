:- module(json_woql,[
              woql_context/1,
              initialise_woql_contexts/0,
              json_woql/2,
              json_woql_path_element_error_message/4,
              json_value_cast_type/3
          ]).

/** <module> WOQL JSON-LD syntax
 *
 * This file contains the code for conversion of JSON-LD to the WOQL
 * AST.
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- use_module(jsonld).

:- use_module(library(sort)).
:- use_module(core(api)).
:- use_module(core(triple)).
:- use_module(core(util)).
:- use_module(global_prefixes).
:- reexport(core(util/syntax)).

:- use_module(library(http/json)).

:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(apply_macros)).

:- use_module(library(lists)).

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
    once(json_to_woql_ast(JSON,WOQL,[])).

prefix_split(Node, Term) :-
    (   uri_has_protocol(Node)
    ->  atom_string(Term,Node)
    ;   uri_has_prefix(Node)
    ->  split_atom(Node,':',[Prefix,Suffix]),
        atom_string(Prefix_Atom,Prefix),
        atom_string(Suffix_Atom,Suffix),
        Term = Prefix_Atom:Suffix_Atom
    ;   atom_string(Term,Node)).

json_value_cast_type(V,Type,WOQL) :-
    default_prefixes(Default_Prefixes),
    prefix_expand(Type, Default_Prefixes, TE),
    (   string(V)
    ->  typecast(V^^xsd:string,
                 TE, [], Val)
    ;   atom(V),
        atom_string(V,String)
    ->  typecast(String^^xsd:string,
                 TE, [], Val)
    ;   integer(V)
    ->  % Keep integers as integers for operations like limit/offset
        typecast(V^^xsd:decimal,
                 TE, [], Val)
    ;   float(V)
    ->  % Convert floats using their natural string representation
        % Don't use 20-digit precision as that exposes float imprecision
        % The float's own precision is already determined by JSON parsing
        format(string(Num_String), '~w', [V]),
        typecast(Num_String^^xsd:string,
                 TE, [], Val)
    ;   rational(V),
        \+ integer(V)
    ->  % Handle rationals - preserve exact precision for WOQL arithmetic
        % These are clean rationals from arithmetic operations
        Val = V^^TE
    ;   member(V,[false,true])
    ->  typecast(V^^xsd:boolean,
                 TE, [], Val)
    ;   throw(error(null_unsupported, V))
    ),
    WOQL = Val.

json_data_to_woql_ast(JSON,WOQL) :-
    is_dict(JSON),
    !,
    (   _{'@value' : V, '@type' : T } :< JSON
    ->  json_value_cast_type(V,T,WOQL)
    ;   _{'@value' : V, '@language' : L } :< JSON
    ->  atom_string(LE,L),
        WOQL = '@'(V,LE)
    ;   true = JSON
    ->  WOQL = true
    ).
json_data_to_woql_ast(JSON,WOQL) :-
    atom(JSON),
    !,
    member(JSON,[true,false]),
    WOQL = JSON^^xsd:boolean.
json_data_to_woql_ast(JSON,WOQL) :-
    string(JSON),
    !,
    WOQL = JSON^^xsd:string.
json_data_to_woql_ast(JSON,WOQL) :-
    integer(JSON),
    !,
    % Keep integers as integers for operations like limit/offset
    WOQL = JSON^^xsd:decimal.
json_data_to_woql_ast(JSON,WOQL) :-
    float(JSON),
    !,
    % Convert floats using their natural string representation
    % Don't use 20-digit precision as that exposes float imprecision  
    % JSON floats already have appropriate precision from parsing
    format(string(Num_String), '~w', [JSON]),
    typecast(Num_String^^xsd:string,
             'http://www.w3.org/2001/XMLSchema#decimal',
             [], WOQL).
json_data_to_woql_ast(JSON,WOQL) :-
    rational(JSON),
    \+ integer(JSON),
    !,
    % Handle rationals - preserve exact precision from arithmetic operations
    % These rationals come from WOQL arithmetic (rdiv, +, -, *) and maintain full precision
    WOQL = JSON^^'http://www.w3.org/2001/XMLSchema#decimal'.

json_list_value_to_woql_ast(JSON,WOQL,Path) :-
    json_list_value_to_woql_ast(JSON,0,WOQL,Path).

json_list_value_to_woql_ast([],_,[],_).
json_list_value_to_woql_ast([JSON|J_Rest],N,[WOQL|W_Rest],Path) :-
    json_value_to_woql_ast(JSON,WOQL,[N|Path]),
    M is N+1,
    json_list_value_to_woql_ast(J_Rest,M,W_Rest,Path).

json_value_to_woql_ast(JSON,WOQL,Path) :-
    is_dict(JSON),
    !,
    (   _{'@type' : Pre_Type} :< JSON
    ->  atom_string(Type,Pre_Type)
    ;   Type = 'Value'
    ),
    json_value_to_woql_ast(Type,JSON,WOQL,Path).
json_value_to_woql_ast(JSON,WOQL,Path) :-
    is_list(JSON),
    !,
    json_list_value_to_woql_ast(JSON,WOQL,Path).
json_value_to_woql_ast(JSON,_,Path) :-
    reverse(Path, Director),
    throw(error(woql_syntax_error(JSON,Director,JSON), _)).

json_value_to_woql_ast('True',_,WOQL,_) :-
    !,
    WOQL = true.
json_value_to_woql_ast(Type,JSON,WOQL,Path) :-
    memberchk(Type, ['Value', 'NodeValue', 'DataValue']),
    (   _{node: Node} :< JSON
    ->  prefix_split(Node, WOQL)
    ;   _{variable: Var} :< JSON
    ->  json_data_to_woql_ast(Var,Var_String^^_),
        atom_string(Var_Atom, Var_String),
        WOQL = v(Var_Atom)
    ;   _{data: Data} :< JSON
    ->  json_data_to_woql_ast(Data,WOQL)
    ;   _{list: List} :< JSON
    ->  json_list_value_to_woql_ast(List,WOQL,Path)
    ;   _{dictionary: Dictionary} :< JSON
    ->  dictionary_template_to_woql_ast(Dictionary,WOQL,Path)
    ).

/*
 * json_to_woql_ast(+JSON:dict,-AST:any) is det.
 *
 * Translate a JSON-LD WOQL statement into the AST.
 */
json_to_woql_ast(JSON,WOQL,Path) :-
    is_dict(JSON),
    _{'@type' : Pre_Type} :< JSON,
    atom_string(Type,Pre_Type),
    json_type_to_woql_ast(Type,JSON,WOQL,Path).
json_to_woql_ast(JSON,WOQL,Path) :-
    is_list(JSON),
    json_list_to_woql_ast(JSON,WOQL,Path).
json_to_woql_ast(JSON,_,Path) :-
    reverse(Path, Director),
    throw(error(woql_syntax_error(JSON,Director,JSON), _)).


json_list_to_woql_ast(JSON,WOQL,Path) :-
    json_list_to_woql_ast(JSON,0,WOQL,Path).

json_list_to_woql_ast([],_,[],_).
json_list_to_woql_ast([JSON|J_Rest],N,[WOQL|W_Rest],Path) :-
    json_to_woql_ast(JSON,WOQL,[N|Path]),
    M is N+1,
    json_list_to_woql_ast(J_Rest,M,W_Rest,Path).

json_list_to_woql_path_pattern(JSON,WOQL,Path) :-
    json_list_to_woql_path_pattern(JSON,0,WOQL,Path).

json_list_to_woql_path_pattern([],_,[],_).
json_list_to_woql_path_pattern([JSON|J_Rest],N,[WOQL|W_Rest],Path) :-
    json_to_woql_path_pattern(JSON,WOQL,[N|Path]),
    M is N+1,
    json_list_to_woql_path_pattern(J_Rest,M,W_Rest,Path).

variable_list([],[]).
variable_list([X|List],[v(VA)|V_List]) :-
    (   is_dict(X)
    ->  get_dict('@value', X, V),
        atom_string(VA,V)
    ;   atom_string(VA,X)),
    variable_list(List,V_List).

% Convert specific types to the WOQL AST, should be expanded to
% make the predicate json_to_woql_ast less verbose and big
json_type_to_woql_ast('TripleCount',JSON,WOQL,Path) :-
    _{resource : Resource,
      count : Count
     } :< JSON,
    json_data_to_woql_ast(Resource,Resource_String^^_),
    atom_string(Resource_Atom, Resource_String),
    json_value_to_woql_ast(Count,WOQL_Count,
                           [count
                            |Path]),
    WOQL = triple_count(Resource_Atom,WOQL_Count).
json_type_to_woql_ast('Size',JSON,WOQL,Path) :-
    _{  resource : Resource,
        size : Size
    } :< JSON,
    json_data_to_woql_ast(Resource,Resource_String^^_),
    atom_string(WResource,Resource_String),
    json_value_to_woql_ast(Size,WOQL_Size,
                           [size
                            |Path]),
    WOQL = size(WResource,WOQL_Size).
json_type_to_woql_ast('Select',JSON,WOQL,Path) :-
    _{variables : Variables,
      query : Sub_Query } :< JSON,
    variable_list(Variables,WOQL_Args),
    json_to_woql_ast(Sub_Query,Sub_WOQL,[query
                                         |Path]),
    WOQL = select(WOQL_Args,Sub_WOQL).
json_type_to_woql_ast('Distinct',JSON,WOQL,Path) :-
    _{variables : Variables,
      query : Sub_Query } :< JSON,
    variable_list(Variables,WOQL_Args),
    json_to_woql_ast(Sub_Query,Sub_WOQL,[query
                                         |Path]),
    WOQL = distinct(WOQL_Args,Sub_WOQL).
json_type_to_woql_ast('Pin',JSON,WOQL,Path) :-
    _{query : Sub_Query } :< JSON,
    json_to_woql_ast(Sub_Query,Sub_WOQL,[query
                                         |Path]),
    WOQL = pin(Sub_WOQL).
json_type_to_woql_ast('And',JSON,WOQL,Path) :-
    _{and : Query_List} :< JSON,
    index_list(Query_List, Indexes),
    maplist({Path}/[Q,I,W]>>(
                json_to_woql_ast(Q,W,[and,I
                                      |Path])
            ), Query_List, Indexes, WOQL_Queries),
    xfy_list(',',WOQL,WOQL_Queries).
json_type_to_woql_ast('Or',JSON,WOQL,Path) :-
    _{or : Query_List} :< JSON,
    index_list(Query_List, Indexes),
    maplist({Path}/[Q,I,W]>>(
                json_to_woql_ast(Q,W,[or,I
                                      |Path])
            ), Query_List, Indexes, WOQL_Queries),
    xfy_list(';',WOQL,WOQL_Queries).
json_type_to_woql_ast('Using',JSON,WOQL,Path) :-
    _{collection : Collection,
      query : Query } :< JSON,
    json_data_to_woql_ast(Collection,WC),
    do_or_die(
        (WC = Collection_String^^_, atom_string(WC_Atom, Collection_String)),
        error(woql_syntax_error(JSON,
                                [collection|Path],
                                Collection), _)),
    json_to_woql_ast(Query,WQ,[query
                               |Path]),
    WOQL = using(WC_Atom,WQ).
json_type_to_woql_ast('From',JSON,WOQL,Path) :-
    _{graph : Graph,
      query : Query } :< JSON,
    json_data_to_woql_ast(Graph,WG),
    json_to_woql_ast(Query,WQ,[query
                               |Path]),
    do_or_die(
        (WG = Graph_String^^_),
        error(woql_syntax_error(JSON,
                                [graph|Path],
                                Graph), _)),
    WOQL = from(Graph_String,WQ).
json_type_to_woql_ast('Into',JSON,WOQL,Path) :-
    _{graph : Graph,
      query : Query } :< JSON,
    json_data_to_woql_ast(Graph,WG),
    json_to_woql_ast(Query,WQ,[query
                               |Path]),
    do_or_die(
        (WG = Graph_String^^_),
        error(woql_syntax_error(JSON,
                                [graph|Path],
                                Graph), _)),
    WOQL = into(Graph_String,WQ).
json_type_to_woql_ast('Triple',JSON,WOQL,Path) :-
    _{subject : Subject,
      predicate : Predicate,
      object : Object,
      graph : Graph
     } :< JSON,
    json_value_to_woql_ast(Subject,WQA,[subject
                                        |Path]),
    json_value_to_woql_ast(Predicate,WQB,[predicate
                                          |Path]),
    json_value_to_woql_ast(Object,WQC,[object
                                       |Path]),
    json_data_to_woql_ast(Graph,WG),

    do_or_die(
        (WG = Graph_String^^_),
        error(woql_syntax_error(JSON,
                                [graph|Path],
                                Graph), _)),

    WOQL = t(WQA,WQB,WQC,Graph_String).
json_type_to_woql_ast('Triple',JSON,WOQL,Path) :-
    _{subject : Subject,
      predicate : Predicate,
      object : Object
     } :< JSON,
    json_value_to_woql_ast(Subject,WQA,[subject
                                        |Path]),
    json_value_to_woql_ast(Predicate,WQB,[predicate
                                          |Path]),
    json_value_to_woql_ast(Object,WQC,[object
                                       |Path]),
    WOQL = t(WQA,WQB,WQC).
json_type_to_woql_ast('Subsumption',JSON,WOQL,Path) :-
    _{ child : Class_A,
       parent : Class_B
     } :< JSON,
    json_value_to_woql_ast(Class_A,WQA,[child
                                        |Path]),
    json_value_to_woql_ast(Class_B,WQB,[parent
                                        |Path]),
    WOQL = '<<'(WQA,WQB).
json_type_to_woql_ast('Equals',JSON,WOQL,Path) :-
    _{ left: A,
       right : B
     } :< JSON,
    json_value_to_woql_ast(A,WQA,[left
                                  |Path]),
    json_value_to_woql_ast(B,WQB,[right
                                  |Path]),
    WOQL = '='(WQA,WQB).
json_type_to_woql_ast('Substring',JSON,WOQL,Path) :-
    _{  string : String,
        before : Before,
        length : Length,
        after : After,
        substring : Substring
     } :< JSON,
    json_value_to_woql_ast(String, WString,[string
                                            |Path]),
    json_value_to_woql_ast(Before, WBefore,[before
                                            |Path]),
    json_value_to_woql_ast(Length, WLength,[length
                                            |Path]),
    json_value_to_woql_ast(After, WAfter,[after
                                          |Path]),
    json_value_to_woql_ast(Substring, WSubstring,[substring
                                                  |Path]),
    WOQL = sub_string(WString,WBefore,WLength,WAfter,WSubstring).
json_type_to_woql_ast('ReadDocument',JSON,WOQL,Path) :-
    _{identifier : Doc_ID,
      document : Doc
     } :< JSON,
    json_value_to_woql_ast(Doc_ID, WID, [identifier
                                         |Path]),
    json_value_to_woql_ast(Doc, WDoc, [document
                                       |Path]),
    WOQL = get_document(WID,WDoc).
json_type_to_woql_ast('UpdateDocument',JSON,WOQL,Path) :-
    _{document : Doc,
      identifier : Doc_ID
     } :< JSON,
    !,
    json_value_to_woql_ast(Doc, WDoc, [document
                                       |Path]),
    json_value_to_woql_ast(Doc_ID, WDoc_ID, [identifier
                                             |Path]),
    WOQL = replace_document(WDoc,WDoc_ID).
json_type_to_woql_ast('UpdateDocument',JSON,WOQL,Path) :-
    _{document : Doc
     } :< JSON,
    json_value_to_woql_ast(Doc, WDoc, [document
                                       |Path]),
    WOQL = replace_document(WDoc).
json_type_to_woql_ast('InsertDocument',JSON,WOQL,Path) :-
    _{document : Doc,
      identifier : Doc_ID
     } :< JSON,
    !,
    json_value_to_woql_ast(Doc, WDoc, [document
                                       |Path]),
    json_value_to_woql_ast(Doc_ID, WDoc_ID, [identifier
                                             |Path]),
    WOQL = insert_document(WDoc,WDoc_ID).
json_type_to_woql_ast('InsertDocument',JSON,WOQL,Path) :-
    _{document : Doc
     } :< JSON,
    json_value_to_woql_ast(Doc, WDoc, [document
                                       |Path]),
    WOQL = insert_document(WDoc).
json_type_to_woql_ast('DeleteDocument',JSON,WOQL,Path) :-
    _{identifier : Doc_ID
     } :< JSON,
    json_value_to_woql_ast(Doc_ID, WDoc_ID, [identifier|Path]),
    WOQL = delete_document(WDoc_ID).
json_type_to_woql_ast('ReadObject',JSON,WOQL,Path) :-
    _{identifier : Doc_ID,
      document : Doc
     } :< JSON,
    json_value_to_woql_ast(Doc_ID, WID, [identifier
                                         |Path]),
    json_value_to_woql_ast(Doc, WDoc, [document
                                       |Path]),
    WOQL = get_document(WID,WDoc).
json_type_to_woql_ast('UpdateObject',JSON,WOQL,Path) :-
    _{document : Doc
     } :< JSON,
    json_value_to_woql_ast(Doc, WDoc, [document
                                       |Path]),
    WOQL = update_document(WDoc).
json_type_to_woql_ast('DeleteObject',JSON,WOQL,Path) :-
    _{identifier : Doc_ID
     } :< JSON,
    json_value_to_woql_ast(Doc_ID, WDoc_ID, [identifier|Path]),
    WOQL = delete_document(WDoc_ID).
json_type_to_woql_ast('AddTriple',JSON,WOQL,Path) :-
    _{subject : Subject,
      predicate : Predicate,
      object : Object,
      graph : Graph
     } :< JSON,
    json_value_to_woql_ast(Subject,WQA,[subject
                                        |Path]),
    json_value_to_woql_ast(Predicate,WQB,[predicate
                                          |Path]),
    json_value_to_woql_ast(Object,WQC,[object
                                       |Path]),
    json_data_to_woql_ast(Graph,WG),
    do_or_die(
        (WG = Graph_String^^_),
        error(woql_syntax_error(JSON,
                                [graph|Path],
                                Graph), _)),
    WOQL = insert(WQA,WQB,WQC,Graph_String).
json_type_to_woql_ast('AddTriple',JSON,WOQL,Path) :-
    _{subject : Subject,
      predicate : Predicate,
      object : Object
     } :< JSON,
    json_value_to_woql_ast(Subject,WQA,[subject
                                        |Path]),
    json_value_to_woql_ast(Predicate,WQB,[predicate
                                          |Path]),
    json_value_to_woql_ast(Object,WQC,[object
                                       |Path]),
    WOQL = insert(WQA,WQB,WQC).
json_type_to_woql_ast('AddedTriple',JSON,WOQL,Path) :-
    _{subject : Subject,
      predicate : Predicate,
      object : Object,
      graph : Graph
     } :< JSON,
    json_value_to_woql_ast(Subject,WQA,[subject
                                        |Path]),
    json_value_to_woql_ast(Predicate,WQB,[predicate
                                          |Path]),
    json_value_to_woql_ast(Object,WQC,[object
                                       |Path]),
    json_data_to_woql_ast(Graph,WG),
    do_or_die(
        (WG = Graph_String^^_),
        error(woql_syntax_error(JSON,
                                [graph|Path],
                                Graph), _)),
    WOQL = addition(WQA,WQB,WQC,Graph_String).
json_type_to_woql_ast('AddedTriple',JSON,WOQL,Path) :-
    _{subject : Subject,
      predicate : Predicate,
      object : Object
     } :< JSON,
    json_value_to_woql_ast(Subject,WQA,[subject
                                        |Path]),
    json_value_to_woql_ast(Predicate,WQB,[predicate
                                          |Path]),
    json_value_to_woql_ast(Object,WQC,[object
                                       |Path]),
    WOQL = addition(WQA,WQB,WQC).
json_type_to_woql_ast('DeletedTriple',JSON,WOQL,Path) :-
    _{subject : Subject,
      predicate : Predicate,
      object : Object,
      graph : Graph
     } :< JSON,
    json_value_to_woql_ast(Subject,WQA,[subject
                                        |Path]),
    json_value_to_woql_ast(Predicate,WQB,[predicate
                                          |Path]),
    json_value_to_woql_ast(Object,WQC,[object
                                       |Path]),
    json_data_to_woql_ast(Graph,WG),
    do_or_die(
        (WG = Graph_String^^_),
        error(woql_syntax_error(JSON,
                                [graph|Path],
                                Graph), _)),
    WOQL = removal(WQA,WQB,WQC,Graph_String).
json_type_to_woql_ast('DeletedTriple',JSON,WOQL,Path) :-
    _{subject : Subject,
      predicate : Predicate,
      object : Object
     } :< JSON,
    json_value_to_woql_ast(Subject,WQA,[subject
                                        |Path]),
    json_value_to_woql_ast(Predicate,WQB,[predicate
                                          |Path]),
    json_value_to_woql_ast(Object,WQC,[object
                                       |Path]),
    WOQL = removal(WQA,WQB,WQC).
json_type_to_woql_ast('AddQuad',JSON,WOQL,Path) :-
    _{subject : Subject,
      predicate : Predicate,
      object : Object,
      graph : Graph
     } :< JSON,
    json_value_to_woql_ast(Subject,WQA,[subject
                                        |Path]),
    json_value_to_woql_ast(Predicate,WQB,[predicate
                                          |Path]),
    json_value_to_woql_ast(Object,WQC,[object
                                       |Path]),
    json_data_to_woql_ast(Graph,WG),
    do_or_die(
        (WG = Graph_String^^_),
        error(woql_syntax_error(JSON,
                                [graph|Path],
                                Graph), _)),
    WOQL = insert(WQA,WQB,WQC,Graph_String).
json_type_to_woql_ast('AddedQuad',JSON,WOQL,Path) :-
    _{subject : Subject,
      predicate : Predicate,
      object : Object,
      graph : Graph
     } :< JSON,
    json_value_to_woql_ast(Subject,WQA,[subject
                                        |Path]),
    json_value_to_woql_ast(Predicate,WQB,[predicate
                                          |Path]),
    json_value_to_woql_ast(Object,WQC,[object
                                       |Path]),
    json_data_to_woql_ast(Graph,WG),
    do_or_die(
        (WG = Graph_String^^_),
        error(woql_syntax_error(JSON,
                                [graph|Path],
                                Graph), _)),
    WOQL = addition(WQA,WQB,WQC,Graph_String).
json_type_to_woql_ast('DeletedQuad',JSON,WOQL,Path) :-
    _{subject : Subject,
      predicate : Predicate,
      object : Object,
      graph : Graph
     } :< JSON,
    json_value_to_woql_ast(Subject,WQA,[subject
                                        |Path]),
    json_value_to_woql_ast(Predicate,WQB,[predicate
                                          |Path]),
    json_value_to_woql_ast(Object,WQC,[object
                                       |Path]),
    json_data_to_woql_ast(Graph,WG),
    do_or_die(
        (WG = Graph_String^^_),
        error(woql_syntax_error(JSON,
                                [graph|Path],
                                Graph), _)),
    WOQL = removal(WQA,WQB,WQC,Graph_String).
json_type_to_woql_ast('DeleteTriple',JSON,WOQL,Path) :-
    _{subject : Subject,
      predicate : Predicate,
      object : Object,
      graph : Graph
     } :< JSON,
    json_value_to_woql_ast(Subject,WQA,[subject
                                        |Path]),
    json_value_to_woql_ast(Predicate,WQB,[predicate
                                          |Path]),
    json_value_to_woql_ast(Object,WQC,[object
                                       |Path]),
    json_data_to_woql_ast(Graph,WG),
    do_or_die(
        (WG = Graph_String^^_),
        error(woql_syntax_error(JSON,
                                [graph|Path],
                                Graph), _)),
    WOQL = delete(WQA,WQB,WQC,Graph_String).
json_type_to_woql_ast('DeleteTriple',JSON,WOQL,Path) :-
    _{subject : Subject,
      predicate : Predicate,
      object : Object
     } :< JSON,
    json_value_to_woql_ast(Subject,WQA,[subject
                                        |Path]),
    json_value_to_woql_ast(Predicate,WQB,[predicate
                                          |Path]),
    json_value_to_woql_ast(Object,WQC,[object
                                       |Path]),
    WOQL = delete(WQA,WQB,WQC).
json_type_to_woql_ast('When',JSON,WOQL,Path) :-
    _{query : Q,
      consequent : U
     } :< JSON,
    json_to_woql_ast(Q,WQ,[query
                           |Path]),
    json_to_woql_ast(U,WU,[consequent
                           |Path]),
    WOQL = when(WQ,WU).
json_type_to_woql_ast('Trim',JSON,WOQL,Path) :-
    _{untrimmed :  A,
      trimmed : T
     } :< JSON,
    json_value_to_woql_ast(A,WA,[untrimmed
                                 |Path]),
    json_value_to_woql_ast(T,WT,[trimmed
                                 |Path]),
    WOQL = trim(WA,WT).
json_type_to_woql_ast('Eval',JSON,WOQL,Path) :-
    _{expression : A,
      result : X
     } :< JSON,
    json_to_woql_arith(A, Arith, [expression
                                  |Path]),
    json_to_woql_arith(X, V, [result
                              |Path]),
    WOQL = is(V,Arith).
json_type_to_woql_ast('IsA',JSON,WOQL,Path) :-
    _{element : X,
      type : T
     } :< JSON,
    json_value_to_woql_ast(X, V, [element
                                  |Path]),
    json_value_to_woql_ast(T, Class,[type
                                     |Path]),
    WOQL = isa(V,Class).
json_type_to_woql_ast('Like',JSON,WOQL,Path) :-
    _{left :  A,
      right : B,
      similarity : F
     } :< JSON,
    json_value_to_woql_ast(A,WA,[left
                                 |Path]),
    json_value_to_woql_ast(B,WB,[right
                                 |Path]),
    json_value_to_woql_ast(F,WF,[like_similarity
                                 |Path]),
    WOQL = like(WA,WB,WF).
json_type_to_woql_ast('Less',JSON,WOQL,Path) :-
    _{left : A,
      right : B
     } :< JSON,
    json_value_to_woql_ast(A,WA,[left
                                 |Path]),
    json_value_to_woql_ast(B,WB,[right
                                 |Path]),
    WOQL = '<'(WA,WB).
json_type_to_woql_ast('Greater',JSON,WOQL,Path) :-
    _{left : A,
      right : B
     } :< JSON,
    json_value_to_woql_ast(A,WA,[left
                                 |Path]),
    json_value_to_woql_ast(B,WB,[right
                                 |Path]),
    WOQL = '>'(WA,WB).
json_type_to_woql_ast('Optional',JSON,WOQL,Path) :-
    _{query : Q
     } :< JSON,
    json_to_woql_ast(Q,WQ,[query
                           |Path]),
    WOQL = opt(WQ).
json_type_to_woql_ast('Get',JSON,WOQL,Path) :-
    _{columns : AsVars,
      resource : Resource
     } :< JSON,
    json_to_woql_ast(AsVars,WAsVars,[columns
                                     |Path]),
    json_to_woql_ast(Resource,WResource,[resource
                                         |Path]),
    (   _{ has_header : Bool } :< JSON
    ->  Header = Bool
    ;   Header = true),
    WOQL = get(WAsVars,WResource,Header).
json_type_to_woql_ast('Column',JSON,WOQL,Path) :-
    _{indicator : Indicator,
      variable: Variable
     } :< JSON,
    json_to_woql_ast(Indicator,WIndicator,[indicator
                                           |Path]),
    atom_string(WVariable,Variable),
    (   _{ type: Type} :< JSON
    ->  atom_string(WType, Type),
        WOQL = as(WIndicator,v(WVariable), WType)
    ;   WOQL = as(WIndicator,v(WVariable))).
json_type_to_woql_ast('Indicator',JSON,WOQL,_Path) :-
    (   _{name: Name} :< JSON
    ->  atom_string(WOQL,Name)
    ;   _{index: N} :< JSON
    ->  number_string(WOQL, N)
    ).
json_type_to_woql_ast('QueryResource',JSON,WOQL,Path) :-
    _{source: Source,
      format: Format} :< JSON,
    json_to_woql_ast(Source,WSource,[source
                                     |Path]),
    atom_string(WFormat,Format),
    (   _{options: Options} :< JSON
    ->  true
    ;   Options = _{}),
    WOQL = resource(WSource,WFormat,Options).
json_type_to_woql_ast('Source',JSON,WOQL,_Path) :-
    (   _{post: Resource} :< JSON
    ->  atom_string(R,Resource),
        WOQL = post(R)
    ;   _{url: Resource} :< JSON
    ->  atom_string(R,Resource),
        WOQL = remote(R)
    ).
json_type_to_woql_ast('LexicalKey',JSON,WOQL,Path) :-
    _{base : Base,
      key_list : Key,
      uri : URI
     } :< JSON,
    json_value_to_woql_ast(Base,WBase,[base
                                       |Path]),
    json_value_to_woql_ast(Key,WKey,[key_list
                                     |Path]),
    json_value_to_woql_ast(URI,WURI,[uri
                                     |Path]),
    WOQL = idgen(WBase,WKey,WURI).
json_type_to_woql_ast('HashKey',JSON,WOQL,Path) :-
    _{base : Base,
      key_list : Key,
      uri : URI
     } :< JSON,
    json_value_to_woql_ast(Base,WBase,[base
                                       |Path]),
    json_value_to_woql_ast(Key,WKey,[key_list
                                     |Path]),
    json_value_to_woql_ast(URI,WURI,[uri
                                     |Path]),
    WOQL = hash(WBase,WKey,WURI).
json_type_to_woql_ast('Upper',JSON,WOQL,Path) :-
    _{mixed :  S,
      upper : V
     } :< JSON,
    json_value_to_woql_ast(S,WS,[left
                                 |Path]),
    json_value_to_woql_ast(V,WV,[right
                                 |Path]),
    WOQL = upper(WS,WV).
json_type_to_woql_ast('Lower',JSON,WOQL,Path) :-
    _{mixed : S,
      lower : V
     } :< JSON,
    json_value_to_woql_ast(S,WS,[left
                                 |Path]),
    json_value_to_woql_ast(V,WV,[right
                                 |Path]),
    WOQL = lower(WS,WV).
json_type_to_woql_ast('Pad',JSON,WOQL,Path) :-
    _{string : S,
      char : C,
      times : N,
      result : V
     } :< JSON,
    json_value_to_woql_ast(S,WS,[string
                                 |Path]),
    json_value_to_woql_ast(C,WC,[char
                                 |Path]),
    json_value_to_woql_ast(N,WN,[times
                                 |Path]),
    json_value_to_woql_ast(V,WV,[result
                                 |Path]),
    WOQL = pad(WS,WC,WN,WV).
json_type_to_woql_ast('Split',JSON,WOQL,Path) :-
    _{string : S,
      pattern : P,
      list : L
     } :< JSON,
    json_value_to_woql_ast(S,WS,[string
                                 |Path]),
    json_value_to_woql_ast(P,WP,[pattern
                                 |Path]),
    json_value_to_woql_ast(L,WL,[list
                                 |Path]),
    WOQL = split(WS,WP,WL).
json_type_to_woql_ast('Member',JSON,WOQL,Path) :-
    _{member : S,
      list : L
     } :< JSON,
    json_value_to_woql_ast(S,WS,[member
                                 |Path]),
    json_value_to_woql_ast(L,WL,[list
                                 |Path]),
    WOQL = member(WS,WL).
json_type_to_woql_ast('Concatenate',JSON,WOQL,Path) :-
    _{list :  List,
      result : Value
     } :< JSON,
    json_value_to_woql_ast(List,WList,[list
                                       |Path]),
    json_value_to_woql_ast(Value,WValue,[result
                                         |Path]),
    WOQL = concat(WList,WValue).
json_type_to_woql_ast('Join',JSON,WOQL,Path) :-
    _{list : List,
      separator : Sep,
      result : Value
     } :< JSON,
    json_value_to_woql_ast(List,WList,[list
                                       |Path]),
    json_value_to_woql_ast(Sep,WSep,[separator
                                     |Path]),
    json_value_to_woql_ast(Value,WValue,[result
                                         |Path]),
    WOQL = join(WList,WSep,WValue).
json_type_to_woql_ast('Sum',JSON,WOQL,Path) :-
    _{list : List,
      result : Value
     } :< JSON,
    json_value_to_woql_ast(List,WList,[list
                                       |Path]),
    json_value_to_woql_ast(Value,WValue,[result
                                         |Path]),
    WOQL = sum(WList,WValue).
json_type_to_woql_ast('Count',JSON,WOQL,Path) :-
    _{query : Query,
      count : Count
     } :< JSON,
    json_to_woql_ast(Query,WQuery,[query
                                  |Path]),
    json_value_to_woql_ast(Count,WCount,[count
                                         |Path]),
    WOQL = count(WQuery,WCount).
json_type_to_woql_ast('Start',JSON,WOQL,Path) :-
    _{start : N,
      query : Q
     } :< JSON,
    json_data_to_woql_ast(N,WN),
    json_to_woql_ast(Q,WQ,[query
                           |Path]),
    do_or_die(
        (WN = Num^^_),
        error(woql_syntax_error(JSON,
                                [start|Path],
                                N), _)),
    WOQL = start(Num, WQ).
json_type_to_woql_ast('Limit',JSON,WOQL,Path) :-
    _{limit :  N,
      query : Q
     } :< JSON,
    json_data_to_woql_ast(N,WN),
    json_to_woql_ast(Q,WQ,[query
                           |Path]),
    do_or_die(
        (WN = Num^^_),
        error(woql_syntax_error(JSON,
                                [limit|Path],
                                N), _)),
    WOQL = limit(Num, WQ).
json_type_to_woql_ast('Regexp',JSON,WOQL,Path) :-
    _{pattern : Pat,
      string : String,
      result : List
     } :< JSON,
    json_value_to_woql_ast(Pat,WPat,[pattern
                                     |Path]),
    json_value_to_woql_ast(String,WString,[string
                                           |Path]),
    json_value_to_woql_ast(List,WList,[result
                                       |Path]),
    WOQL = re(WPat, WString, WList).
json_type_to_woql_ast('OrderBy',JSON,WOQL,Path) :-
    _{ordering : Templates,
      query : Query
     } :< JSON,
    % This should be written as a seperate predicate with the
    % OrderTemplate
    json_to_woql_ast(Templates,WTemplates,[ordering
                                           |Path]),
    json_to_woql_ast(Query,WQuery,[query
                                   |Path]),
    WOQL = order_by(WTemplates,WQuery).
json_type_to_woql_ast('OrderTemplate',JSON,WOQL,_Path) :-
    _{  variable : Var,
        order : Order
     } :< JSON,
    atom_string(V, Var),
    atom_string(Order_Atom,Order),
    (   Order_Atom = asc
    ->  WOQL = asc(v(V))
    ;   Order_Atom = desc
    ->  WOQL = desc(v(V))
    ).
json_type_to_woql_ast('GroupBy',JSON,WOQL,Path) :-
    _{group_by : Spec,
      template : Template,
      query : Query,
      grouped : Collector
     } :< JSON,
    variable_list(Spec,WSpec),
    % Backwards compat with "variable list"
    (   variable_list(Template,WObj)
    ->  true
    ;   json_value_to_woql_ast(Template,WObj,[template|Path])),
    json_to_woql_ast(Query,WQuery,[query
                                   |Path]),
    json_value_to_woql_ast(Collector,WCollector,[grouped
                                                 |Path]),
    WOQL = group_by(WSpec,WObj,WQuery,WCollector).
json_type_to_woql_ast('Length',JSON,WOQL,Path) :-
    _{list : A,
      length : B
     } :< JSON,
    json_value_to_woql_ast(A,WA,[list
                                 |Path]),
    json_value_to_woql_ast(B,WB,[length
                                 |Path]),
    WOQL = length(WA,WB).
json_type_to_woql_ast('Typecast',JSON,WOQL,Path) :-
    _{value : Val,
      type : Type,
      result : Var
     } :< JSON,
    json_value_to_woql_ast(Val,WVal,[value
                                     |Path]),
    json_value_to_woql_ast(Type,WType,[type
                                       |Path]),
    json_value_to_woql_ast(Var,WVar,[result
                                     |Path]),
    WOQL = typecast(WVal,WType,WVar).
json_type_to_woql_ast('Not',JSON,WOQL,Path) :-
    _{query : Q
     } :< JSON,
    json_to_woql_ast(Q,WQ,[query
                           |Path]),
    WOQL = not(WQ).
json_type_to_woql_ast('Once',JSON,WOQL,Path) :-
    _{query : Q
     } :< JSON,
    json_to_woql_ast(Q,WQ,[query
                           |Path]),
    WOQL = once(WQ).
json_type_to_woql_ast('Immediately',JSON,WOQL,Path) :-
    _{query : Q
     } :< JSON,
    json_to_woql_ast(Q,WQ,[query
                           |Path]),
    WOQL = immediately(WQ).
json_type_to_woql_ast('Dot',JSON,WOQL,Path) :-
    _{document : Dictionary,
      field : Key,
      value : Value
     } :< JSON,
    json_value_to_woql_ast(Dictionary, WDictionary, [document
                                                     |Path]),
    json_value_to_woql_ast(Key, WKey, [field
                                       |Path]),
    json_value_to_woql_ast(Value,WValue,[value
                                         |Path]),
    WOQL = dot(WDictionary,WKey,WValue).
json_type_to_woql_ast('Path',JSON,WOQL,Path) :-
    _{subject : Subject,
      pattern : Pattern,
      object : Object
     } :< JSON,
    json_value_to_woql_ast(Subject,WSubject,[subject
                                             |Path]),
    json_to_woql_path_pattern(Pattern,WPattern,[pattern
                                                |Path]),
    json_value_to_woql_ast(Object,WObject,[object
                                           |Path]),

    (   _{path : Edge_Path} :< JSON
    ->  json_value_to_woql_ast(Edge_Path,WEdge_Path,[path
                                                     |Path]),
        WOQL = path(WSubject,WPattern,WObject,WEdge_Path)
    ;   WOQL = path(WSubject,WPattern,WObject)).
json_type_to_woql_ast('TypeOf',JSON,WOQL,Path) :-
    _{value : Value,
      type : Type
     } :< JSON,
    json_value_to_woql_ast(Value,WOQL_Value,
                           [value
                            |Path]),
    json_value_to_woql_ast(Type,WOQL_Type,
                           [type
                            |Path]),

    WOQL = typeof(WOQL_Value,WOQL_Type).
json_type_to_woql_ast('Call',JSON,WOQL,Path) :-
    _{name : Name,
      arguments : Arguments
     } :< JSON,
    (   is_dict(Name)
    ->  get_dict('@value', Name, Name_Value),
        atom_string(Name_Atom, Name_Value)
    ;   atom_string(Name_Atom, Name)
    ),
    json_value_to_woql_ast(Arguments,WOQL_Arguments,
                           [arguments
                            |Path]),
    WOQL = call(Name_Atom,WOQL_Arguments).

json_to_woql_path_pattern(JSON,Pattern,Path) :-
    is_dict(JSON),
    get_dict('@type', JSON, Type),
    atom_string(Type_Atom,Type),
    json_type_to_woql_path_pattern(Type_Atom,JSON,Pattern,Path).
json_to_woql_path_pattern(JSON,_Pattern,Path) :-
    throw(error(woql_syntax_error(JSON,Path,JSON), _)).

json_type_to_woql_path_pattern('PathPredicate',JSON,Pattern,_Path) :-
    (   _{predicate : Node} :< JSON
    ->  prefix_split(Node,WNode),
        Pattern = p(WNode)
    ;   Pattern = p
    ).
json_type_to_woql_path_pattern('InversePathPredicate',JSON,Pattern,_Path) :-
    (   _{predicate : Node} :< JSON
    ->  prefix_split(Node,WNode),
        Pattern = n(WNode)
    ;   Pattern = n
    ).
json_type_to_woql_path_pattern('PathSequence',JSON,Pattern,Path) :-
    _{sequence : List} :< JSON,
    json_list_to_woql_path_pattern(List,WList,[sequence|Path]),
    xfy_list(',',Pattern,WList).
json_type_to_woql_path_pattern('PathOr',JSON,Pattern,Path) :-
    _{or : List} :< JSON,
    json_list_to_woql_path_pattern(List,WList,[or|Path]),
    xfy_list(';',Pattern,WList).
json_type_to_woql_path_pattern('PathPlus',JSON,Pattern,Path) :-
    _{plus : SubPattern} :< JSON,
    json_to_woql_path_pattern(SubPattern,PSubPattern,
                              [plus
                               |Path]),
    Pattern = plus(PSubPattern).
json_type_to_woql_path_pattern('PathStar',JSON,Pattern,Path) :-
    _{star : SubPattern} :< JSON,
    json_to_woql_path_pattern(SubPattern,PSubPattern,
                              [star
                               |Path]),
    Pattern = star(PSubPattern).
json_type_to_woql_path_pattern('PathTimes',JSON,Pattern,Path) :-
    _{times : SubPattern,
      from : N,
      to : M
     } :< JSON,
    json_to_woql_path_pattern(SubPattern,PSubPattern,
                              [times
                               |Path]),
    json_data_to_woql_ast(N,WN),
    json_data_to_woql_ast(M,WM),
    do_or_die(
        (   WN = N_int_Pre ^^ _,
            (   integer(N_int_Pre)
            ->  N_int_Pre = N_int
            ;   number_string(N_int, N_int_Pre)
            )
        ),
        error(woql_syntax_error(JSON,
                                [from|Path],
                                N), _)),
    do_or_die(
        (   WM = M_int_Pre ^^ _,
            (   integer(N_int_Pre)
            ->  M_int_Pre = M_int
            ;   number_string(M_int, M_int_Pre)
            )
        ),
        error(woql_syntax_error(JSON,
                                [to|Path],
                                M), _)),
    Pattern = times(PSubPattern,N_int,M_int).

is_json_var(A) :-
    sub_atom(A, 0, _, _, 'http://terminusdb.com/schema/woql/variable/').

woql_index_sort(Comp,Left,Right) :-
    (   is_dict(Left),
        is_dict(Right),
        get_dict(index, Left, Left_Lit),
        get_dict('@value', Left_Lit, I),
        get_dict(index, Right, Right_Lit),
        get_dict('@value', Right_Lit, J)
    ->  compare(Comp, I, J)
    ;   compare(Comp, Left, Right)).

json_to_woql_arith(JSON,WOQL,Path) :-
    is_dict(JSON),
    !,
    _{'@type' : Type_String} :< JSON,
    atom_string(Type,Type_String),
    json_type_to_woql_arith(Type,JSON,WOQL,Path).
json_to_woql_arith(JSON,_,Path) :-
    throw(error(woql_syntax_error(JSON,Path,JSON), _)).

json_type_to_woql_arith('Plus',JSON,WOQL,Path) :-
    _{left : Left,
      right : Right} :< JSON,
    json_to_woql_arith(Left, WOQL_Left,
                       [left
                        |Path]),
    json_to_woql_arith(Right, WOQL_Right,
                       [right
                        |Path]),
    WOQL = '+'(WOQL_Left,WOQL_Right).
json_type_to_woql_arith('Minus',JSON,WOQL,Path) :-
    _{left : Left,
      right : Right} :< JSON,
    json_to_woql_arith(Left, WOQL_Left,
                       [left
                        |Path]),
    json_to_woql_arith(Right, WOQL_Right,
                       [right
                        |Path]),
    WOQL = '-'(WOQL_Left,WOQL_Right).
json_type_to_woql_arith('Times',JSON,WOQL,Path) :-
    _{left : Left,
      right : Right} :< JSON,
    json_to_woql_arith(Left, WOQL_Left,
                       [left
                        |Path]),
    json_to_woql_arith(Right, WOQL_Right,
                       [right
                        |Path]),
    WOQL = '*'(WOQL_Left,WOQL_Right).
json_type_to_woql_arith('Divide',JSON,WOQL,Path) :-
    _{left : Left,
      right : Right} :< JSON,
    json_to_woql_arith(Left, WOQL_Left,
                       [left
                        |Path]),
    json_to_woql_arith(Right, WOQL_Right,
                       [right
                        |Path]),
    WOQL = '/'(WOQL_Left,WOQL_Right).
json_type_to_woql_arith('Div',JSON,WOQL,Path) :-
    _{left : Left,
      right : Right} :< JSON,
    json_to_woql_arith(Left, WOQL_Left,
                       [left
                        |Path]),
    json_to_woql_arith(Right, WOQL_Right,
                       [right
                        |Path]),
    WOQL = div(WOQL_Left,WOQL_Right).
json_type_to_woql_arith('Exp',JSON,WOQL,Path) :-
    _{left : Left,
      right : Right} :< JSON,
    json_to_woql_arith(Left, WOQL_Left,
                       [left
                        |Path]),
    json_to_woql_arith(Right, WOQL_Right,
                       [right
                        |Path]),
    WOQL = '**'(WOQL_Left,WOQL_Right).
json_type_to_woql_arith('Floor',JSON,WOQL,Path) :-
    _{argument : Argument} :< JSON,
    json_to_woql_arith(Argument,WOQL_Argument,
                       [argument
                        |Path]),
    WOQL=floor(WOQL_Argument).
json_type_to_woql_arith('ArithmeticValue',JSON,WOQL,_) :-
    (   _{data : Data} :< JSON
    ->  json_data_to_woql_ast(Data,WOQL)
    ;   _{variable : Var} :< JSON
    ->   atom_string(Var_Atom, Var),
         WOQL = v(Var_Atom)
    ).

dictionary_template_to_woql_ast(Template, WOQL, Path) :-
    get_dict(data, Template, Pairs),
    index_list(Pairs,Indexes),
    maplist({Path}/[Pair, I, Key-Value]>>(
                get_dict(field, Pair, Key_String),
                get_dict(value, Pair, V),
                atom_string(Key,Key_String),
                json_value_to_woql_ast(V, WOQL_Value, [value,I|Path]),
                (   WOQL_Value = Value^^_ % unnecessary specificity
                ->  true
                ;   WOQL_Value = Value)
            ), Pairs, Indexes, NewPairs),
   !,
   dict_create(WOQL, json, NewPairs).

json_woql_path_element_error_message(_JSON,Path,Element,Message) :-
    (   Path = [Head|_Path],
        woql_element_error_message(Head,Element,Message)
    ->  true
    ;   format(string(Message),'Not well formed WOQL JSON-LD', [])).

woql_element_error_message(
    resource,
    Element,
    Message) :-
    format(string(Message),'Poorly formed resource descriptor: ~q',Element).
woql_element_error_message(
    collection,
    Element,
    Message) :-
    format(string(Message),'Poorly formed collection descriptor: ~q',Element).
woql_element_error_message(
    collection,
    Element,
    Message) :-
    format(string(Message),'Poorly formed collection descriptor: ~q',Element).
woql_element_error_message(
    graph,
    Element,
    Message) :-
    format(string(Message),'Poorly formed graph resource: ~q',Element).
woql_element_error_message(
    '@id',
    Element,
    Message) :-
    format(string(Message),'Document has no id: ~q',Element).
woql_element_error_message(
    identifier,
    Element,
    Message) :-
    format(string(Message),'Not a well formed variable identifier: ~q',Element).
woql_element_error_message(
    start,
    Element,
    Message) :-
    format(string(Message),'Poorly formed start: ~q, should be a positive integer',Element).
woql_element_error_message(
    path_minimum,
    Element,
    Message) :-
    format(string(Message),'Poorly formed path min: ~q, should be a positive integer',Element).
woql_element_error_message(
    path_maximum,
    Element,
    Message) :-
    format(string(Message),'Poorly formed path max: ~q, should be a positive integer',Element).
woql_element_error_message(
    result,
    Element,
    Message) :-
    format(string(Message),'Not a well formed arithmetic result: ~q',Element).
woql_element_error_message(
    expression,
    Element,
    Message) :-
    format(string(Message),'Not a well formed arithmetic expression: ~q',Element).

:- begin_tests(woql_jsonld).

test(size_syntax,[]) :-

    catch(
        (   Query = _{ '@type' : "http://terminusdb.com/schema/woql#Size",
                       'http://terminusdb.com/schema/woql#resource' : 1,
                       'http://terminusdb.com/schema/woql#size' : 2
                     },
            json_to_woql_ast(Query, _, [])
        ),
        E,
        once(api_error_jsonld(woql,E,JSON))
    ),

    JSON = _{'@type':'api:WoqlErrorResponse',
             'api:error': _{'@type':'vio:WOQLSyntaxError',
                            'vio:path':[],
                            'vio:query': _{'@type':"http://terminusdb.com/schema/woql#Size",
                                           'http://terminusdb.com/schema/woql#resource':1,
                                           'http://terminusdb.com/schema/woql#size':2}},
             'api:message':"Not well formed WOQL JSON-LD",
             'api:status':'api:failure'}.

test(not_a_query, []) :-
    JSON = _{'@type' : "And",
             and:
             [_{'@type' : "Frob"}]},

    catch(
        json_woql(JSON,_WOQL),
        error(woql_syntax_error(Q,P,E), _),
        json_woql_path_element_error_message(Q,P,E,Message)),

    Message = "Not well formed WOQL JSON-LD".

test(post_marshalling, []) :-
    JSON = _{'@type' : "Get",
             columns : [_{'@type': "Column",
                          indicator: _{'@type' : 'Indicator',
                                       name: "Start station"},
                          variable: "Start_Station"}
                       ],
             resource: _{'@type': "QueryResource",
                         source : _{ '@type' : 'Source',
                                     post : "bike_csv"},
                         format : "csv"}},

    json_woql(JSON,WOQL),

    WOQL = get(['Start station' as v('Start_Station')],
               resource(post(bike_csv), csv, _{}), true).

test(isa, []) :-
    JSON_Atom = '{
  "@type": "IsA",
  "element": {
    "@type": "NodeValue",
    "variable": "X"
  },
  "type": {
    "@type": "NodeValue",
    "node": "xsd:string"
  }
}',

    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON,WOQL),

    WOQL = isa(v('X'),xsd:string).

:- end_tests(woql_jsonld).

:- begin_tests(jsonld_ast).

test(anySimpleType, []) :-

    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right": { "@type": "DataValue",
                            "data": { "@value": "Something",
                                      "@type": "xsd:anySimpleType"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON, WOQL),
    WOQL = (v('X')="Something"^^'http://www.w3.org/2001/XMLSchema#anySimpleType').

test(string, []) :-

    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right": { "@type": "DataValue",
                            "data": { "@value": "Something",
                                      "@type": "xsd:string"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON, WOQL),
    WOQL = (v('X')="Something"^^'http://www.w3.org/2001/XMLSchema#string').

test(boolean_true, []) :-

    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right": { "@type": "DataValue",
                            "data": { "@value": "true",
                                      "@type": "xsd:boolean"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON, WOQL),
    WOQL = (v('X')=true^^'http://www.w3.org/2001/XMLSchema#boolean').

test(boolean_true_concrete, []) :-

    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right": { "@type": "DataValue",
                            "data": { "@value": true,
                                      "@type": "xsd:boolean"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON, WOQL),
    WOQL = (v('X')=true^^'http://www.w3.org/2001/XMLSchema#boolean').

test(boolean_true_bare, []) :-

    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right": { "@type": "DataValue",
                            "data": true}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON, WOQL),
    WOQL = (v('X')=true^^xsd:boolean).


test(decimal_bare, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right": { "@type": "DataValue",
                            "data": 1.3}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON,WOQL),
    % Decimals from JSON floats are converted via string to clean rationals: 1.3 = 13/10
    WOQL = (v('X')=Rational^^'http://www.w3.org/2001/XMLSchema#decimal'),
    rational(Rational),
    Rational =:= 13 rdiv 10.

test(decimal_typed, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right": { "@type": "DataValue",
                            "data": { "@value": 1.3,
                                      "@type": "xsd:decimal"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON,WOQL),
    % Decimals are now rationals: 1.3 = 13/10
    WOQL = (v('X')=Rational^^'http://www.w3.org/2001/XMLSchema#decimal'),
    rational(Rational),
    Rational =:= 13 rdiv 10.

test(decimal, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right": { "@type": "DataValue",
                            "data": { "@value": "1.3",
                                      "@type": "xsd:decimal"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON,WOQL),
    % Decimals are now rationals: "1.3" parses to 13/10
    WOQL = (v('X')=Rational^^'http://www.w3.org/2001/XMLSchema#decimal'),
    rational(Rational),
    Rational =:= 13 rdiv 10.

test(date, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right": { "@type": "DataValue",
                            "data": { "@value": "1066-09-18",
                                      "@type": "xsd:date"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON,WOQL),
    WOQL = (v('X')=date(1066,9,18,Z)^^'http://www.w3.org/2001/XMLSchema#date'),
    Z == 0.

test(time, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right": { "@type": "DataValue",
                            "data": { "@value": "15:29:44",
                                      "@type": "xsd:time"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON,WOQL),
    WOQL = (v('X')=time(15,29,44)^^'http://www.w3.org/2001/XMLSchema#time').

test(dateTime, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right": { "@type": "DataValue",
                            "data": { "@value": "2004-04-12T13:20:00",
                                      "@type": "xsd:dateTime"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON,WOQL),
    WOQL = (v('X')=date_time(2004,04,12,13,20,0,0)^^'http://www.w3.org/2001/XMLSchema#dateTime').

test(dateTimeStamp, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right": { "@type": "DataValue",
                            "data": { "@value": "2004-04-12T13:20:00-05:00",
                                      "@type": "xsd:dateTimeStamp"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON,WOQL),
    WOQL = (v('X')=date_time(2004,4,12,8,20,0,0)^^'http://www.w3.org/2001/XMLSchema#dateTimeStamp').

test(gyear, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right": { "@type": "DataValue",
                            "data": { "@value": "2004",
                                      "@type": "xsd:gYear"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON,WOQL),
    WOQL = (v('X')=gyear(2004,Z)^^'http://www.w3.org/2001/XMLSchema#gYear'),
    Z == 0.

test(gyear_range, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right": { "@type": "DataValue",
                            "data": { "@value": "[2004,2014]",
                                      "@type": "xdd:gYearRange"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON,WOQL),
    WOQL = (v('X')=gyear_range(gyear(2004,Z),gyear(2014,Z))^^'http://terminusdb.com/schema/xdd#gYearRange'),
    Z == 0.

test(gyear_int, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right": { "@type": "DataValue",
                            "data": { "@value": 2004,
                                      "@type": "xsd:gYear"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON,WOQL),
    WOQL = (v('X')=gyear(2004,0)^^'http://www.w3.org/2001/XMLSchema#gYear').

test(gyear_month, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right": { "@type": "DataValue",
                            "data": { "@value": "2004-12",
                                      "@type": "xsd:gYearMonth"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON,WOQL),
    WOQL = (v('X')=gyear_month(2004,12,Z)^^'http://www.w3.org/2001/XMLSchema#gYearMonth'),
    Z == 0.

test(gmonth, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right": { "@type": "DataValue",
                            "data": { "@value": "--12",
                                      "@type": "xsd:gMonth"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON,WOQL),
    WOQL = (v('X')=gmonth(12,Z)^^'http://www.w3.org/2001/XMLSchema#gMonth'),
    Z == 0.

test(gmonth_day, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right": { "@type": "DataValue",
                            "data": { "@value": "-05-24",
                                      "@type": "xsd:gMonthDay"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON,WOQL),
    WOQL = (v('X')=gmonth_day(5,24,Z)^^'http://www.w3.org/2001/XMLSchema#gMonthDay'),
    Z == 0.

test(gday, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right": { "@type": "DataValue",
                            "data": { "@value": "---24",
                                          "@type": "xsd:gDay"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON,WOQL),
    WOQL = (v('X')=gday(24,Z)^^'http://www.w3.org/2001/XMLSchema#gDay'),
    Z == 0.

test(byte, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right": { "@type": "DataValue",
                            "data": { "@value": 120,
                                          "@type": "xsd:byte"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON,WOQL),
    WOQL = (v('X')=120^^'http://www.w3.org/2001/XMLSchema#byte').

test(short, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right": { "@type": "DataValue",
                            "data": { "@value": 120,
                                          "@type": "xsd:short"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON,WOQL),
    WOQL = (v('X')=120^^'http://www.w3.org/2001/XMLSchema#short').

test(integer, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right": { "@type": "DataValue",
                            "data": { "@value": 120,
                                          "@type": "xsd:integer"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON,WOQL),
    WOQL = (v('X')=120^^'http://www.w3.org/2001/XMLSchema#integer').

test(long, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right": { "@type": "DataValue",
                            "data": { "@value": 120,
                                          "@type": "xsd:long"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON,WOQL),
    WOQL = (v('X')=120^^'http://www.w3.org/2001/XMLSchema#long').

test(unsignedByte, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right": { "@type": "DataValue",
                            "data": { "@value": 120,
                                          "@type": "xsd:unsignedByte"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON,WOQL),
    WOQL = (v('X')=120^^'http://www.w3.org/2001/XMLSchema#unsignedByte').

test(unsignedShort, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right": { "@type": "DataValue",
                            "data": { "@value": 120,
                                          "@type": "xsd:unsignedShort"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON,WOQL),
    WOQL = (v('X')=120^^'http://www.w3.org/2001/XMLSchema#unsignedShort').

test(unsignedInt, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right": { "@type": "DataValue",
                            "data": { "@value": 120,
                                          "@type": "xsd:unsignedInt"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON,WOQL),
    WOQL = (v('X')=120^^'http://www.w3.org/2001/XMLSchema#unsignedInt').

test(unsignedLong, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right": { "@type": "DataValue",
                            "data": { "@value": 120,
                                          "@type": "xsd:unsignedLong"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON,WOQL),
    WOQL = (v('X')=120^^'http://www.w3.org/2001/XMLSchema#unsignedLong').

test(positiveInteger, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right": { "@type": "DataValue",
                            "data": { "@value": 120,
                                          "@type": "xsd:positiveInteger"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON,WOQL),
    WOQL = (v('X')=120^^'http://www.w3.org/2001/XMLSchema#positiveInteger').

test(nonNegativeInteger, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right": { "@type": "DataValue",
                            "data": { "@value": 120,
                                          "@type": "xsd:nonNegativeInteger"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON,WOQL),
    WOQL = (v('X')=120^^'http://www.w3.org/2001/XMLSchema#nonNegativeInteger').

test(negativeInteger, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right": { "@type": "DataValue",
                            "data": { "@value": -120,
                                          "@type": "xsd:negativeInteger"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON,WOQL),
    WOQL = (v('X')=(-120)^^'http://www.w3.org/2001/XMLSchema#negativeInteger').

test(nonPositiveInteger, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right": { "@type": "DataValue",
                            "data": { "@value": -120,
                                      "@type": "xsd:nonPositiveInteger"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON,WOQL),
    WOQL = (v('X')=(-120)^^'http://www.w3.org/2001/XMLSchema#nonPositiveInteger').

test(hexBinary, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right":  { "@type": "DataValue",
                             "data": { "@value": "a03bc23",
                                       "@type": "xsd:hexBinary"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON,WOQL),
    WOQL = (v('X')="a03bc23"^^'http://www.w3.org/2001/XMLSchema#hexBinary').

test(base64binary, []) :-
    JSON_Atom= '{"@type": "Equals",
                 "left": { "@type": "DataValue",
                           "variable": "X"},
                 "right": { "@type": "DataValue",
                            "data": { "@value": "YXNkZg==",
                                      "@type": "xsd:base64Binary"}}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_woql(JSON,WOQL),
    WOQL = (v('X')="YXNkZg=="^^'http://www.w3.org/2001/XMLSchema#base64Binary').

test(dictionary_template_to_woql_ast, []) :-
    JSON_Atom=
    '{"@type": "Value",
      "dictionary" : {"@type": "DictionaryTemplate",
                      "data": [ { "@type" : "FieldValuePair",
                                  "field" : "@type",
                                  "value" : { "@type" : "Value",
                                              "data" : "User"}},
                                { "@type" : "FieldValuePair",
                                  "field" : "name",
                                  "value" : { "@type" : "Value",
                                              "data" : "Jim"}},
                                { "@type" : "FieldValuePair",
                                  "field" : "employed",
                                  "value" : { "@type" : "Value",
                                              "data" : true }},
                                { "@type" : "FieldValuePair",
                                  "field" : "father",
                                  "value" : { "@type" : "Value",
                                              "variable" : "Father"}}]}}',
    atom_json_dict(JSON_Atom, JSON, []),
    json_value_to_woql_ast(JSON,WOQL,[]),
    WOQL = json{'@type':"User",employed:true,father:v('Father'),name:"Jim"}.

:- end_tests(jsonld_ast).
