:- module('document/json', [
              context_triple/2,
              json_elaborate/3,
              json_triple/3,
              json_schema_triple/3,
              json_schema_elaborate/3,
              get_document/3,
              get_document/4,
              get_document_uri/2,
              get_schema_document/3,
              get_schema_document_uri/2,
              get_document_by_type/3,
              get_document_uri_by_type/3,
              get_schema_document_uri_by_type/3,
              delete_document/2,
              insert_document/3,
              replace_document/3,
              nuke_documents/1,
              insert_schema_document/2,
              delete_schema_document/2,
              replace_schema_document/2,
              replace_schema_document/3,
              nuke_schema_documents/1,
              database_context/2,
              create_graph_from_json/5,
              write_json_stream_to_builder/3,
              write_json_stream_to_schema/2,
              write_json_stream_to_instance/2,
              write_json_string_to_schema/2,
              write_json_string_to_instance/2,
              replace_json_schema/2,
              class_frame/3
          ]).

:- use_module(instance).
:- use_module(schema).

:- use_module(library(pcre)).
:- use_module(library(yall)).
:- use_module(library(apply_macros)).
:- use_module(library(terminus_store)).
:- use_module(library(http/json)).

:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).

value_type_to_json_type(X, T, X, T) :-
    number(X),
    !.
value_type_to_json_type(X, T, S, T) :-
    (   string(X)
    ;   atom(X)),
    !,
    atom_string(X, S).
value_type_to_json_type(X, T, X, T) :-
    T = 'http://www.w3.org/2001/XMLSchema#boolean',
    !.
value_type_to_json_type(X, T, S, T) :-
    typecast(X^^T, 'http://www.w3.org/2001/XMLSchema#string', [], S^^_).

json_type_to_value_type(J, T, J, T) :-
    number(J),
    !.
json_type_to_value_type(J, T, J, T) :-
    T = 'http://www.w3.org/2001/XMLSchema#boolean',
    !.
json_type_to_value_type(J, T, X, T) :-
    typecast(J^^'http://www.w3.org/2001/XMLSchema#string', T, [], X^^_).


value_type_json_type(V,T,JV,JT) :-
    ground(V),
    ground(T),
    !,
    value_type_to_json_type(V,T,JV,JT).
value_type_json_type(V,T,JV,JT) :-
    json_type_to_value_type(JV,JT,V,T).

value_json(X,O) :-
    O = json{
            '@type': "@id",
            '@id': X
        },
    string(X),
    !.
value_json(RDF_Nil,[]) :-
    global_prefix_expand(rdf:nil,RDF_Nil),
    !.
value_json(V^^T,json{ '@value' : JV, '@type' : JT}) :-
    value_type_json_type(V,T,JV,JT),
    !.
value_json(X@Y,O) :-
    O = json{
            '@lang': Y,
            '@value': X
        },
    !.
value_json(X,X) :-
    atom(X).

get_all_path_values(JSON,Path_Values) :-
    findall(Path-Value,
            get_path_value(JSON,Path,Value),
            Path_Values).

% TODO: Arrays
get_value([], []) :-
    !.
get_value(Elaborated, Value) :-
    is_dict(Elaborated),
    get_dict('@type', Elaborated, "@id"),
    !,
    get_dict('@id', Elaborated, Value).
get_value(Elaborated,Value) :-
    is_dict(Elaborated),
    get_dict('@type',Elaborated,"@container"),
    !,
    get_dict('@value',Elaborated, List),
    member(Elt,List),
    get_value(Elt,Value).
get_value(Elaborated,Value) :-
    is_dict(Elaborated),
    get_dict('@value',Elaborated,_),
    !,
    value_json(Value,Elaborated).

get_path_value(Elaborated,Path,Value) :-
    is_dict(Elaborated),
    get_dict('@type',Elaborated,_),
    !,
    dict_pairs(Elaborated,json,Pairs),
    % Remove ID if it exists
    (   select('@id'-_,Pairs,Pairs1)
    ->  true
    ;   Pairs = Pairs1),
    % remove type?
    % select('@type'-_,Pairs1,Pairs2),

    member(P-V,Pairs1),

    (   P = '@type',
        atom(V)
    ->  Path = [P],
        V = Value
    ;   get_value(V,Value)
    ->  Path = [P]
    ;   get_path_value(V,Sub_Path,Value),
        Path = [P|Sub_Path]
    ).

get_field_values(JSON,Context,Fields,Values) :-
    findall(
        Value,
        (   member(Field,Fields),
            prefix_expand_schema(Field,Context,Field_Ex),
            (   get_dict(Field_Ex,JSON,Value)
            ->  true
            ;   throw(error(missing_key(Field,JSON),_))
            )
        ),
        Values).

raw(JValue,Value) :-
    is_dict(JValue),
    !,
    get_dict('@value',JValue,Value).
raw(JValue,JValue).

idgen_lexical(Base,Values,ID) :-
    when(ground(Values),
         (   maplist(raw,Values,Raw),
             maplist(uri_encoded(path),Raw,Encoded),
             merge_separator_split(Suffix, '_', Encoded),
             format(string(ID), '~w~w', [Base,Suffix])
         )).

idgen_hash(Base,Values,ID) :-
    when(ground(Values),
         (   maplist(raw,Values,Raw),
             maplist(uri_encoded(path),Raw,Encoded),
             merge_separator_split(String, '_', Encoded),
             sha_hash(String, Octets, []),
             hash_atom(Octets, Hash),
             format(string(ID), "~w~w", [Base,Hash])
         )).

idgen_path_values_hash(Base,Path,ID) :-
    format(string(A), '~q', [Path]),
    sha_hash(A, Octets, []),
    hash_atom(Octets, Hash),
    format(string(ID), "~w~w", [Base,Hash]).

idgen_random(Base,ID) :-
    random(X),
    format(string(S), '~w', [X]),
    md5_hash(S,Hash,[]),
    format(string(ID),'~w~w',[Base,Hash]).

path_strings_([], _Prefixes, []).
path_strings_([node(X)|Path], Prefixes, [URI|Strings]) :-
    % We have *very* late binding of IDs, need to do this after its done.
    when(ground(X),
         (   compress_dict_uri(X, Prefixes, Compressed),
             (   (   uri_has_protocol(Compressed)
                 ;   uri_has_prefix(Compressed))
             ->  prefix_expand(X, Prefixes, URI)
             ;   Compressed = URI))),
    path_strings_(Path, Prefixes, Strings).
path_strings_([property(X)|Path], Prefixes, [URI|Strings]) :-
    % We have *very* late binding of IDs, need to do this after its done.
    when(ground(X),
         (   compress_schema_uri(X, Prefixes, Compressed),
             (   (   uri_has_protocol(Compressed)
                 ;   uri_has_prefix(Compressed))
             ->  prefix_expand_schema(X, Prefixes, URI)
             ;   Compressed = URI))),
    path_strings_(Path, Prefixes, Strings).

path_strings(Elts, Prefixes, Strings) :-
    reverse(Elts, Descending),
    path_strings_(Descending, Prefixes, Strings).

json_idgen(JSON,DB,Context,Path,ID_Ex) :-
    get_dict('@type',JSON,Type),
    key_descriptor(DB,Type,Descriptor),
    (   Descriptor = lexical(Base,Fields)
    ->  get_field_values(JSON, Context, Fields, Values),
        path_strings(Path, Context, Strings),
        append(Strings, Values, Full),
        idgen_lexical(Base,Full,ID)
    ;   Descriptor = hash(Base,Fields),
        path_strings(Path, Context, Strings),
        append(Strings, Values, Full),
        get_field_values(JSON, Context, Fields, Values),
        idgen_hash(Base,Full,ID)
    ;   Descriptor = value_hash(Base)
    ->  get_all_path_values(JSON,Path_Values),
        idgen_path_values_hash(Base,Path_Values,ID)
    ;   Descriptor = random(Base)
    ->  idgen_random(Base,ID)
    ),
    when(ground(ID),
         prefix_expand(ID, Context, ID_Ex)).

class_descriptor_image(unit,[]).
class_descriptor_image(class(_),json{ '@type' : "@id" }).
class_descriptor_image(optional(C),Image) :-
    (   is_base_type(C)
    ->  Image = json{ '@type' : C }
    ;   Image = json{ '@type' : "@id" }).
class_descriptor_image(tagged_union(_,_),json{ '@type' : "@id" }).
class_descriptor_image(base_class(C),json{ '@type' : C }).
class_descriptor_image(enum(C,_),json{ '@type' : C }).
class_descriptor_image(list(C),json{ '@container' : "@list",
                                     '@type' : C }).
class_descriptor_image(array(C),json{ '@container' : "@array",
                                      '@type' : C }).
class_descriptor_image(set(C),json{ '@container' : "@set",
                                    '@type' : C }).
class_descriptor_image(cardinality(C,_), json{ '@container' : "@set",
                                               '@type' : C }).

database_context(DB,Context) :-
    is_transaction(DB),
    !,
    database_schema(DB,Schema),
    once(
        (   xrdf(Schema, ID, rdf:type, sys:'Context')
        ->  id_schema_json(DB,ID,Pre_Context),
            findall(
                Key-URI,
                (   xrdf(Schema, ID, sys:prefix_pair, Prefix_Pair),
                    xrdf(Schema, Prefix_Pair, sys:prefix, Key_String^^_),
                    xrdf(Schema, Prefix_Pair, sys:url, URI^^_),
                    atom_string(Key,Key_String)
                ),
                Pairs),
            dict_pairs(Prefixes, context, Pairs),
            !,
            Context_With_ID = (Pre_Context.put(Prefixes)),
            select_dict(json{'@id' : _ }, Context_With_ID, Context)
        ;   Context = _{})
    ).
database_context(Query_Context, Context) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    database_context(TO, Context).
database_context(Askable, Context) :-
    create_context(Askable, Query_Context),
    database_context(Query_Context, Context).

predicate_map(P, Context, Prop, json{ '@id' : P }) :-
    % NOTE: This is probably wrong if it already has a prefix...
    get_dict('@schema', Context, Base),
    atomic_list_concat([Base,'(.*)'],Pat),
    re_matchsub(Pat, P, Match, []),
    !,
    get_dict(1,Match,Short),
    atom_string(Prop,Short).
predicate_map(P, _Context, P, json{}).

type_context(_DB, "@id", json{}) :- !.
type_context(_DB, Base_Type, json{}) :-
    is_base_type(Base_Type),
    !.
type_context(DB,Type,Context) :-
    database_context(DB, Database_Context),
    prefix_expand_schema(Type,Database_Context,TypeEx),
    do_or_die(is_simple_class(DB, TypeEx),
              error(type_not_found(Type), _)),
    findall(Prop - C,
          (
              class_predicate_type(DB, TypeEx, P, Desc),
              class_descriptor_image(Desc, Image),
              predicate_map(P,Database_Context,Prop, Map),
              put_dict(Map,Image,C)
          ),
          Edges),
    % eliminate duplicates
    sort(Edges,Sorted_Edges),
    catch(
        dict_create(Context,json,Sorted_Edges),
        error(duplicate_key(P),_),
        throw(error(violation_of_diamond_property(Type,P)))
    ).

json_elaborate(DB,JSON,JSON_ID) :-
    database_context(DB,Context),
    json_elaborate(DB,JSON,Context,JSON_ID).

prefix_expand_schema(Node,Context,NodeEx) :-
    (   get_dict('@schema', Context, Schema),
        put_dict(_{'@base' : Schema}, Context, New_Context)
    ->  true
    ;   Context = New_Context),
    prefix_expand(Node, New_Context, NodeEx).

property_expand_key_value(Prop,Value,DB,Context,Path,P,V) :-
    get_dict(Prop, Context, Full_Expansion),
    is_dict(Full_Expansion),
    !,
    expansion_key(Prop,Full_Expansion,P,Expansion),
    context_value_expand(DB,Context,Path,Value,Expansion,V).
property_expand_key_value(Prop,Value,DB,Context,Path,P,V) :-
    prefix_expand_schema(Prop, Context, Prop_Ex),
    Prop \= Prop_Ex,
    property_expand_key_value(Prop_Ex,Value,DB,Context,Path,P,V).

json_elaborate(DB,JSON,Context,JSON_ID) :-
    json_elaborate(DB,JSON,Context,[],JSON_ID).

json_elaborate(DB,JSON,Context,Path,JSON_ID) :-
    is_dict(JSON),
    !,
    do_or_die(get_dict('@type',JSON,Type),
              error(document_has_no_type(JSON), _)),
    prefix_expand_schema(Type,Context,TypeEx),


    do_or_die(
        type_context(DB,TypeEx,Type_Context),
        error(unknown_type_encountered(TypeEx),_)),

    put_dict(Type_Context,Context,New_Context),
    json_context_elaborate(DB,JSON,New_Context,[node(JSON_ID)|Path],Elaborated),

    (   is_subdocument(DB, TypeEx)
    ->  Id_Path = Path
    ;   Id_Path = []),

    json_assign_id(Elaborated,DB,Context,Id_Path,JSON_ID).

expansion_key(Key,Expansion,Prop,Cleaned) :-
    (   select_dict(json{'@id' : Prop}, Expansion, Cleaned)
    ->  true
    ;   Key = Prop,
        Expansion = Cleaned
    ).


value_expand_list([], _DB, _Path, _Context, _Elt_Type, []).
value_expand_list([Value|Vs], DB, Path, Context, Elt_Type, [Expanded|Exs]) :-
    (   is_enum(DB,Elt_Type)
    ->  enum_value(Elt_Type,Value,Enum_Value),
        prefix_expand_schema(Enum_Value, Context, Enum_Value_Ex),
        Prepared = json{'@id' : Enum_Value_Ex,
                        '@type' : "@id"}
    ;   is_dict(Value)
    ->  put_dict(json{'@type':Elt_Type}, Value, Prepared)
    ;   is_base_type(Elt_Type)
    ->  Prepared = json{'@value' : Value,
                        '@type': Elt_Type}
    ;   Prepared = json{'@id' : Value,
                        '@type': "@id"}),
    json_elaborate(DB, Prepared, Context, Path, Expanded),
    value_expand_list(Vs, DB, Path, Context, Elt_Type, Exs).

% Unit type expansion
context_value_expand(_,_,_Path,[],json{},[]) :-
    !.
context_value_expand(DB,Context,Path,Value,Expansion,V) :-
    get_dict('@container', Expansion, _),
    !,
    % Container type
    get_dict('@type', Expansion, Elt_Type),
    (   is_list(Value)
    ->  Value_List = Value
    ;   string(Value)
    ->  Value_List = [Value]
    ;   get_dict('@value',Value,Value_List)),
    value_expand_list(Value_List, DB, Path, Context, Elt_Type, Expanded_List),
    V = (Expansion.put(json{'@value' : Expanded_List})).
context_value_expand(DB,Context,Path,Value,Expansion,V) :-
    % A possible reference
    get_dict('@type', Expansion, "@id"),
    !,
    (   is_dict(Value)
    ->  json_elaborate(DB, Value, Context, Path, V)
    ;   prefix_expand(Value,Context,Value_Ex),
        V = json{ '@type' : "@id", '@id' : Value_Ex}
    ).
context_value_expand(_,_Context,_Path,Value,_Expansion,V) :-
    % An already expanded typed value
    is_dict(Value),
    get_dict('@value',Value,_),
    !,
    V = Value.
context_value_expand(DB,_Context,_Path,Value,Expansion,V) :-
    % An unexpanded typed value
    New_Expansion = (Expansion.put(json{'@value' : Value})),
    json_elaborate(DB,New_Expansion, V).

enum_value(Type,Value,ID) :-
    atomic_list_concat([Type, '_', Value], ID).

json_context_elaborate(DB, JSON, Context, _Path, Expanded) :-
    is_dict(JSON),
    get_dict('@type',JSON,Type),
    prefix_expand_schema(Type,Context,Type_Ex),
    is_enum(DB,Type_Ex),
    !,
    get_dict('@value',JSON,Value),
    enum_value(Type,Value,Full_ID),
    Expanded = json{ '@type' : "@id",
                     '@id' : Full_ID }.
json_context_elaborate(DB, JSON, Context, Path, Expanded) :-
    is_dict(JSON),
    !,
    dict_pairs(JSON,json,Pairs),
    findall(
        P-V,
        (   member(Prop-Value,Pairs),
            (   property_expand_key_value(Prop,Value,DB,Context,[property(Prop)|Path],P,V)
            ->  true
            ;   Prop = '@type'
            ->  P = Prop,
                prefix_expand_schema(Value, Context, V)
            ;   has_at(Prop)
            ->  P = Prop,
                V = Value
            ;   (   get_dict('@type', JSON, Type)
                ->  throw(error(unrecognized_property(Type,Prop,Value), _))
                ;   throw(error(unrecognized_untyped_property(Prop,Value), _)))
            )
        ),
        PVs),
    dict_pairs(Expanded,json,PVs).

json_assign_id(JSON,DB,Context,Path,JSON_ID) :-
    % Set up the ID
    (   get_dict('@id',JSON,ID)
    ->  prefix_expand(ID, Context, ID_Ex),
        JSON_ID = (JSON.put(json{'@id' : ID_Ex}))
    ;   get_dict('@container', JSON, _)
    ->  JSON_ID = JSON
    ;   get_dict('@value', JSON, _)
    ->  JSON_ID = JSON
    ;   json_idgen(JSON,DB,Context,Path,ID)
    ->  JSON_ID = (JSON.put(json{'@id' : ID}))
    ;   throw(error(no_id(JSON),_))
    ).

json_prefix_access(JSON,Edge,Type) :-
    global_prefix_expand(Edge,Expanded),
    get_dict(Expanded,JSON,Type).

json_type(JSON,_Context,Type) :-
    json_prefix_access(JSON,rdf:type,Type).

is_type_family(Dict) :-
    get_dict('@type',Dict,Type_Constructor),
    maybe_expand_schema_type(Type_Constructor,Expanded),
    type_family_constructor(Expanded).

type_family_parts(JSON,['Cardinality',Class,Cardinality]) :-
    get_dict('@type',JSON,"Cardinality"),
    !,
    get_dict('@class',JSON, Class),
    get_dict('@cardinality',JSON, Cardinality).
type_family_parts(JSON,[Family,Class]) :-
    get_dict('@type',JSON, Family),
    get_dict('@class',JSON, Class).

type_family_id(JSON,Path,ID) :-
    reverse(Path,Rev),
    type_family_parts(JSON,Parts),
    append(Rev,Parts,Full_Path),
    maplist(uri_encoded(path),Full_Path,Encoded),
    merge_separator_split(Merged,'_',Encoded),
    ID = Merged.

key_parts(JSON,[Type|Fields]) :-
    get_dict('@type',JSON,Type),
    get_dict('@fields',JSON,Fields),
    !.
key_parts(JSON,[Type]) :-
    get_dict('@type',JSON,Type).

key_id(JSON,Context,Path,ID) :-
    reverse(Path,Rev),
    key_parts(JSON,Parts),
    append(Rev,Parts,Full_Path),
    maplist(uri_encoded(path),Full_Path,Encoded),
    merge_separator_split(Merged,'_',Encoded),
    prefix_expand_schema(Merged,Context,ID).

property_part(JSON,Key) :-
    get_dict(Key, JSON, _),
    \+ memberchk(Key, ['@id', '@type']),
    !.

property_id(JSON,Context,Path,ID) :-
    property_part(JSON,Property),
    reverse([Property|Path],Rev),
    maplist(uri_encoded(path),Rev,Encoded),
    merge_separator_split(Merged,'_',Encoded),
    prefix_expand_schema(Merged,Context,ID).

documentation_id(Context,Path,ID) :-
    reverse(['Documentation'|Path],Full_Path),
    maplist(uri_encoded(path),Full_Path,Encoded),
    merge_separator_split(Merged,'_',Encoded),
    prefix_expand_schema(Merged,Context,ID).

maybe_expand_schema_type(Type, Expanded) :-
    (   re_match('.*:.*', Type)
    ->  Type = Expanded
    ;   global_prefix_expand(sys:Type,Expanded)
    ).

is_context(JSON) :-
    get_dict('@type', JSON, "@context").

% NOTE: We probably need the prefixes in play here...
is_type_enum(JSON) :-
    get_dict('@type', JSON, "Enum"),
    !.
is_type_enum(JSON) :-
    global_prefix_expand(sys:'Enum', Enum),
    get_dict('@type', JSON, Enum).

context_triple(JSON,Triple) :-
    context_elaborate(JSON,Elaborated),
    expand(Elaborated,JSON{
                          sys:'http://terminusdb.com/schema/sys#',
                          xsd:'http://www.w3.org/2001/XMLSchema#',
                          xdd:'http://terminusdb.com/schema/xdd#'
                      },
           Expanded),
    json_triple_(Expanded,_{},Triple).

context_keyword_value_map('@type',"@context",'@type','sys:Context').
context_keyword_value_map('@base',Value,'sys:base',json{'@type' : "xsd:string", '@value' : Value}).
context_keyword_value_map('@schema',Value,'sys:schema',json{'@type' : "xsd:string", '@value' : Value}).

context_elaborate(JSON,Elaborated) :-
    is_context(JSON),
    !,
    dict_pairs(JSON,json,Pairs),
    partition([P-_]>>(member(P, ['@type', '@base', '@schema'])),
              Pairs, Keyword_Values, Prop_Values),
    findall(
        P-V,
        (   member(Keyword-Value,Keyword_Values),
            context_keyword_value_map(Keyword,Value,P,V)
        ),
        PVs),

    findall(
        Prefix_Pair,
        (   member(Prop-Value, Prop_Values),
            idgen_hash('terminusdb://Prefix_Pair_',[json{'@value' : Prop},
                                                    json{'@value' : Value}], HashURI),
            Prefix_Pair = json{'@type' : 'sys:Prefix',
                               '@id' : HashURI,
                               'sys:prefix' : json{ '@value' : Prop,
                                                    '@type' : "xsd:string"},
                               'sys:url' : json{ '@value' : Value,
                                                 '@type' : "xsd:string"}
                              }
        ),
        Prefix_Pair_List),

    dict_pairs(Elaborated,json,['@id'-"terminusdb://context",
                                'sys:prefix_pair'-json{ '@container' : "@set",
                                                        '@type' : "sys:Prefix",
                                                        '@value' : Prefix_Pair_List }
                                |PVs]).

wrap_id(ID, json{'@type' : "@id",
                 '@id' : ID}) :-
    (   atom(ID)
    ;   string(ID)),
    !.
wrap_id(ID, ID).

wrap_text(Text, json{'@type' : XSD,
                     '@value' : String}) :-
    global_prefix_expand(xsd:string, XSD),
    (   atom(Text)
    ;   string(Text)),
    !,
    atom_string(Text,String).
wrap_text(Text, Text) :-
    is_dict(Text).

expand_match_system(Key,Term,Key_Ex) :-
    global_prefixes(sys,Prefix),
    global_prefix_expand(sys:Term,Key_Ex),
    prefix_expand(Key, _{'@base' : Prefix}, Key_Ex).

json_schema_elaborate_key(V,_,json{'@type':Value}) :-
    atom(V),
    !,
    global_prefixes(sys,Prefix),
    prefix_expand(V, _{'@base' : Prefix}, Value).
json_schema_elaborate_key(V,Context,Value) :-
    get_dict('@type', V, Candidate),
    (   expand_match_system(Candidate, 'Lexical', Type)
    ;   expand_match_system(Candidate, 'Hash', Type)
    ),
    !,
    get_dict('@fields', V, Fields),
    maplist({Context}/[Elt,Elt_ID]>>(
                prefix_expand_schema(Elt,Context,Elt_Ex),
                wrap_id(Elt_Ex,Elt_ID)
            ),
            Fields, Fields_Wrapped),
    global_prefix_expand(sys:fields,Field),
    Type_Value = json{ '@type' : Type },
    Value = (Type_Value.put(Field,
                            json{
                                '@container' : "@list",
                                '@type' : "@id",
                                '@value' : Fields_Wrapped
                            })).
json_schema_elaborate_key(V,_,json{ '@type' : Type}) :-
    get_dict('@type', V, ValueHash),
    expand_match_system(ValueHash, 'ValueHash', Type),
    !.
json_schema_elaborate_key(V,_,json{ '@type' : Type}) :-
    get_dict('@type', V, Random),
    expand_match_system(Random, 'Random', Type),
    !.

json_schema_elaborate_property_documentation(Context, Path, Dict, Out) :-
    global_prefix_expand(sys:'PropertyDocumentation',Property_Ex),
    property_id(Dict, Context, Path, Id),
    dict_pairs(Dict, json, In_Pairs),
    findall(
        Key-Value,
        (
            member(K-V, In_Pairs),
            (   K = '@id'
            ->  fail
            ;   K = '@type'
            ->  fail
            ;   is_dict(V)
            ->  prefix_expand_schema(K, Context, Key),
                Value = V
            ;   prefix_expand_schema(K, Context, Key),
                wrap_text(V,Value)
            )
        ),
        Pairs),
    dict_pairs(Out, json, ['@id'-Id,'@type'-Property_Ex|Pairs]).

json_schema_elaborate_documenation(V,Context,Path,json{'@type' : Documentation_Ex,
                                                 '@id' : ID,
                                                 '@comment' :
                                                 json{ '@type' : XSD,
                                                       '@value' : V}}) :-
    string(V),
    !,
    global_prefix_expand(xsd:string, XSD),
    global_prefix_expand(sys:'Documentation',Documentation_Ex),
    documentation_id(Context,Path,ID).
json_schema_elaborate_documentation(V,Context,Path,Result2) :-
    is_dict(V),
    !,
    global_prefix_expand(sys:'Documentation',Documentation_Ex),

    documentation_id(Context,Path,Doc_Id),
    Result = json{'@id' : Doc_Id,
                   '@type' : Documentation_Ex},
    global_prefix_expand(sys:'properties',PropertiesP),
    Result1 = (Result.put(PropertiesP, Properties)),

    (   get_dict('@comment', V, Comment_Text)
    ->  wrap_text(Comment_Text, Comment),
        global_prefix_expand(sys:'comment',CommentP),
        Result2 = Result1.put(CommentP, Comment)
    ;   Result2 = Result1
    ),

    (   get_dict('@properties',V,List)
    ->  maplist(
            json_schema_elaborate_property_documentation(Context,['PropertyDocumentation',
                                                                  'Documentation'
                                                                  |Path]),
            List, Properties_List),
        Properties  = json{
                          '@container' : "@set",
                          '@type' : "@id",
                          '@value' : Properties_List
                      }
    ;   Properties  = json{
                          '@container' : "@set",
                          '@type' : "@id",
                          '@value' : []
                      }).

json_schema_predicate_value('@id',V,Context,_,'@id',V_Ex) :-
    !,
    prefix_expand_schema(V,Context,V_Ex).
json_schema_predicate_value('@cardinality',V,_,_,P,json{'@type' : Type,
                                                        '@value' : V }) :-
    !,
    global_prefix_expand(xsd:nonNegativeInteger,Type),
    global_prefix_expand(sys:cardinality, P).
json_schema_predicate_value('@key',V,Context,Path,P,Value) :-
    !,
    global_prefix_expand(sys:key, P),
    json_schema_elaborate_key(V,Context,Elab),
    key_id(V,Context,Path,ID),
    put_dict(_{'@id' : ID}, Elab, Value).
json_schema_predicate_value('@documentation',V,Context,Path,P,Value) :-
    !,
    global_prefix_expand(sys:documentation, P),
    json_schema_elaborate_documentation(V,Context,Path,Value).
json_schema_predicate_value('@abstract',[],_,_,P,[]) :-
    !,
    global_prefix_expand(sys:abstract, P).
json_schema_predicate_value('@subdocument',[],_,_,P,[]) :-
    !,
    global_prefix_expand(sys:subdocument, P).
json_schema_predicate_value('@base',V,_,_,P,Value) :-
    !,
    global_prefix_expand(sys:base, P),
    (   is_dict(V)
    ->  Value = V
    ;   global_prefix_expand(xsd:string, XSD),
        Value = json{ '@type' : XSD,
                      '@value' : V }
    ).
json_schema_predicate_value('@type',V,_,_,'@type',Value) :-
    !,
    maybe_expand_schema_type(V,Value).
json_schema_predicate_value('@class',V,Context,_,Class,json{'@type' : "@id",
                                                            '@id' : VEx}) :-
    !,
    prefix_expand_schema(V,Context,VEx),
    global_prefix_expand(sys:class, Class).
json_schema_predicate_value(P,V,Context,Path,Prop,Value) :-
    is_dict(V),
    !,
    prefix_expand_schema(P,Context,Prop),
    json_schema_elaborate(V, Context, [P|Path], Value).
json_schema_predicate_value(P,List,Context,_,Prop,Set) :-
    is_list(List),
    !,
    prefix_expand_schema(P,Context,Prop),
    maplist({Context}/[V,Value]>>prefix_expand_schema(V,Context,Value),
            List, Value_List),
    Set = json{ '@collection' : "@set",
                '@type' : "@id",
                '@value' : Value_List }.
json_schema_predicate_value(P,V,Context,_,Prop,json{'@type' : "@id",
                                                    '@id' : VEx }) :-
    prefix_expand_schema(P,Context,Prop),
    prefix_expand_schema(V,Context,VEx).

json_schema_elaborate(JSON,Context,_,Elaborated) :-
    is_type_enum(JSON),
    !,
    get_dict('@id', JSON, ID),
    prefix_expand_schema(ID,Context,ID_Ex),
    get_dict('@type', JSON, Type),
    maybe_expand_schema_type(Type,Expanded),
    get_dict('@value', JSON, List),
    maplist({ID_Ex}/[Elt,json{'@type' : "@id",
                              '@id' : V}]>>(
                format(string(V),'~w_~w',[ID_Ex,Elt])
            ),List,New_List),
    Type_ID = json{ '@id' : ID_Ex,
                    '@type' : Expanded
                  },
    global_prefix_expand(sys:value, Sys_Value),
    Elaborated = (Type_ID.put(Sys_Value,
                              json{ '@container' : "@list",
                                    '@type' : "@id",
                                    '@value' : New_List })).
json_schema_elaborate(JSON,Context,Old_Path,Elaborated) :-
    is_dict(JSON),
    dict_pairs(JSON,json,Pre_Pairs),
    !,
    (   is_type_family(JSON)
    ->  type_family_id(JSON,Old_Path,ID),
        Pairs = ['@id'-ID|Pre_Pairs]
    ;   Pairs = Pre_Pairs,
        get_dict('@id',JSON,ID)
    ),
    Path = [ID|Old_Path],
    findall(
        Prop-Value,
        (   member(P-V,Pairs),
            json_schema_predicate_value(P,V,Context,Path,Prop,Value)
        ),
        PVs),
    dict_pairs(Elaborated,json,PVs).

json_schema_elaborate(JSON,Context,JSON_Schema) :-
    json_schema_elaborate(JSON,Context,[],JSON_Schema).

json_schema_triple(JSON,Context,Triple) :-
    json_schema_elaborate(JSON,Context,[],JSON_Schema),
    json_triple_(JSON_Schema,Context,Triple).

% Triple generator
json_triple(DB,JSON,Triple) :-
    json_elaborate(DB,JSON,Elaborated),
    database_context(DB,Context),
    json_triple_(Elaborated,Context,Triple).

json_triples(DB,JSON,Triples) :-
    findall(
        Triple,
        json_triple(DB, JSON, Triple),
        Triples).

json_triple_(JSON,_,_Triple) :-
    is_dict(JSON),
    get_dict('@value', JSON, _),
    \+ get_dict('@container', JSON, _),
    !,
    fail.
json_triple_(JSON,Context,Triple) :-
    is_dict(JSON),
    !,
    % NOTE: Need to do something with containers separately
    dict_keys(JSON,Keys),

    (   get_dict('@id', JSON, ID)
    ->  true
    ;   throw(error(no_id(JSON), _))
    ),

    member(Key, Keys),
    get_dict(Key,JSON,Value),
    (   Key = '@id'
    ->  fail
    ;   Key = '@type', % this is a leaf
        Value = "@id"
    ->  fail
    ;   Key = '@type'
    ->  global_prefix_expand(rdf:type, RDF_Type),
        Triple = t(ID,RDF_Type,Value)
    ;   Key = '@inherits'
    ->  global_prefix_expand(sys:inherits, SYS_Inherits),
        (    get_dict('@value',Value,Class)
        ->  (   is_dict(Class)
            ->  get_dict('@id', Class, Inherited),
                Triple = t(ID,SYS_Inherits,Inherited)
            ;   is_list(Class)
            ->  member(Inherited, Class),
                Triple = t(ID,SYS_Inherits,Inherited)
            ;   Triple = t(ID,SYS_Inherits,Class))
        ;   get_dict('@id', Value, Inherited)
        ->  Triple = t(ID,SYS_Inherits,Inherited))
    ;   (   Value = []
        ->  global_prefix_expand(rdf:nil,RDF_Nil),
            Triple = t(ID,Key,RDF_Nil)
        ;   get_dict('@id', Value, Value_ID)
        ->  (   json_triple_(Value, Context, Triple)
            ;   Triple = t(ID,Key,Value_ID)
            )
        ;   get_dict('@container', Value, "@list")
        ->  get_dict('@value', Value, List),
            list_id_key_context_triple(List,ID,Key,Context,Triple)
        ;   get_dict('@container', Value, "@array")
        ->  get_dict('@value', Value, Array),
            array_id_key_context_triple(Array,ID,Key,Context,Triple)
        ;   get_dict('@container', Value, "@set")
        ->  get_dict('@value', Value, Set),
            set_id_key_context_triple(Set,ID,Key,Context,Triple)
        ;   value_json(Lit,Value),
            Triple = t(ID,Key,Lit)
        )
    ).

array_id_key_context_triple(List,ID,Key,Context,Triple) :-
    array_index_id_key_context_triple(List,0,ID,Key,Context,Triple).

array_index_id_key_context_triple([H|T],Index,ID,Key,Context,Triple) :-
    get_dict('@base', Context, Base),
    atomic_list_concat([Base,'Array_'], Base_Array),
    idgen_random(Base_Array,New_ID),
    reference(H,HRef),
    global_prefix_expand(sys:'Array', SYS_Array),
    global_prefix_expand(sys:value, SYS_Value),
    global_prefix_expand(sys:index, SYS_Index),
    global_prefix_expand(xsd:nonNegativeInteger, XSD_NonNegativeInteger),
    global_prefix_expand(rdf:type, RDF_Type),
    (   Triple = t(ID, Key, New_ID)
    ;   Triple = t(New_ID, RDF_Type, SYS_Array)
    ;   Triple = t(New_ID, SYS_Value, HRef)
    ;   Triple = t(New_ID, SYS_Index, Index^^XSD_NonNegativeInteger)
    ;   Next_Index is Index + 1,
        array_index_id_key_context_triple(T,Next_Index,ID,Key,Context,Triple)
    ;   json_triple_(H,Context,Triple)
    ).

set_id_key_context_triple([H|T],ID,Key,Context,Triple) :-
    (   reference(H,HRef),
        Triple = t(ID,Key,HRef)
    ;   set_id_key_context_triple(T,ID,Key,Context,Triple)
    ;   json_triple_(H,Context,Triple)
    ).

reference(Dict,ID) :-
    get_dict('@id',Dict, ID),
    !.
reference(Elt,V) :-
    value_json(V,Elt).

list_id_key_context_triple([],ID,Key,_Context,t(ID,Key,RDF_Nil)) :-
    global_prefix_expand(rdf:nil, RDF_Nil).
list_id_key_context_triple([H|T],ID,Key,Context,Triple) :-
    get_dict('@base', Context, Base),
    atomic_list_concat([Base,'Cons_'], Base_Cons),
    idgen_random(Base_Cons,New_ID),
    (   Triple = t(ID,Key,New_ID)
    ;   global_prefix_expand(rdf:type, RDF_Type),
        global_prefix_expand(rdf:'List', RDF_List),
        Triple = t(New_ID, RDF_Type, RDF_List)
    ;   reference(H,HRef),
        global_prefix_expand(rdf:first, RDF_First),
        Triple = t(New_ID,RDF_First,HRef)
    ;   global_prefix_expand(rdf:rest, RDF_Rest),
        list_id_key_context_triple(T,New_ID,RDF_Rest,Context,Triple)
    ;   json_triple_(H,Context,Triple)
    ).

rdf_list_list(_Graph, RDF_Nil,[]) :-
    global_prefix_expand(rdf:nil,RDF_Nil),
    !.
rdf_list_list(Graph, Cons,[H|L]) :-
    xrdf(Graph, Cons, rdf:type, rdf:'List'),
    xrdf(Graph, Cons, rdf:first, H),
    xrdf(Graph, Cons, rdf:rest, Tail),
    rdf_list_list(Graph,Tail,L).

array_list(DB,Id,P,List) :-
    database_instance(DB,Instance),
    findall(
        I-V,
        (   xrdf(Instance,Id,P,ArrayElement),
            xrdf(Instance,ArrayElement,rdf:type,sys:'Array'),
            xrdf(Instance,ArrayElement,sys:value,V),
            xrdf(Instance,ArrayElement,sys:index,I^^_)
        ),
        Index_List),
    keysort(Index_List, Index_List_Sorted),
    index_list_array(Index_List_Sorted,List).

index_list_array(Index_List, List) :-
    index_list_last_array(Index_List,0,List).

index_list_last_array([], _, []) :-
    !.
index_list_last_array([I-Value|T], I, [Value|List]) :-
    !,
    J is I + 1,
    index_list_last_array(T,J,List).
index_list_last_array(Index_List, I, [null|List]) :-
    (   I > 174763
    ->  throw(error(index_on_array_too_large(I),_))
    ;   true
    ),

    J is I + 1,
    index_list_last_array(Index_List,J,List).

set_list(DB,Id,P,Set) :-
    % NOTE: This will not give an empty list.
    database_instance(DB,Instance),
    setof(V,xrdf(Instance,Id,P,V),Set),
    !.

list_type_id_predicate_value([],_,_,_,_,_,[]).
list_type_id_predicate_value([O|T],C,Id,P,DB,Prefixes,[V|L]) :-
    type_id_predicate_iri_value(C,Id,P,O,DB,Prefixes,V),
    list_type_id_predicate_value(T,C,Id,P,DB,Prefixes,L).

type_id_predicate_iri_value(enum(C,_),_,_,V,_,_,O) :-
    merge_separator_split(V, '_', [C,O]).
type_id_predicate_iri_value(list(C),Id,P,O,DB,Prefixes,L) :-
    % Probably need to treat enums...
    database_instance(DB,Instance),
    rdf_list_list(Instance,O,V),
    type_descriptor(DB,C,Desc),
    list_type_id_predicate_value(V,Desc,Id,P,DB,Prefixes,L).
type_id_predicate_iri_value(array(C),Id,P,_,DB,Prefixes,L) :-
    array_list(DB,Id,P,V),
    type_descriptor(DB,C,Desc),
    list_type_id_predicate_value(V,Desc,Id,P,DB,Prefixes,L).
type_id_predicate_iri_value(set(C),Id,P,_,DB,Prefixes,L) :-
    set_list(DB,Id,P,V),
    type_descriptor(DB,C,Desc),
    list_type_id_predicate_value(V,Desc,Id,P,DB,Prefixes,L).
type_id_predicate_iri_value(cardinality(C,_),Id,P,_,DB,Prefixes,L) :-
    set_list(DB,Id,P,V),
    type_descriptor(DB,C,Desc),
    list_type_id_predicate_value(V,Desc,Id,P,DB,Prefixes,L).
type_id_predicate_iri_value(class(_),_,_,Id,DB,Prefixes,Value) :-
    (   instance_of(DB, Id, C),
        is_subdocument(DB, C)
    ->  get_document(DB, Prefixes, true, Id, Value)
    ;   compress_dict_uri(Id, Prefixes, Value)
    ).
type_id_predicate_iri_value(tagged_union(C,_),_,_,Id,DB,Prefixes,Value) :-
    (   instance_of(DB, Id, C),
        is_subdocument(DB, C)
    ->  get_document(DB, Prefixes, true, Id, Value)
    ;   compress_dict_uri(Id, Prefixes, Value)
    ).
type_id_predicate_iri_value(optional(C),Id,P,O,DB,Prefixes,V) :-
    type_descriptor(DB,C,Desc),
    type_id_predicate_iri_value(Desc,Id,P,O,DB,Prefixes,V).
type_id_predicate_iri_value(base_class(_),_,_,X^^T,_,_,S) :-
    % NOTE: This has to treat each variety of JSON value as natively
    % as possible.
    value_type_json_type(X,T,S,_).

compress_schema_uri(IRI,Prefixes,IRI_Comp) :-
    (   get_dict('@schema',Prefixes,Schema),
        put_dict(_{'@base' : Schema}, Prefixes, Schema_Prefixes)
    ->  true
    ;   Prefixes = Schema_Prefixes),
    compress_dict_uri(IRI,Schema_Prefixes,IRI_Comp),
    !.
compress_schema_uri(IRI,_Prefixes,IRI).

get_document_uri(Query_Context, ID) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    get_document_uri(TO, ID).
get_document_uri(Desc, ID) :-
    is_descriptor(Desc),
    !,
    open_descriptor(Desc,Transaction),
    get_document_uri(Transaction, ID).
get_document_uri(DB, Uri) :-
    is_simple_class(DB, Class),
    instance_of(DB, Uri, Class).

get_document_uri_by_type(Query_Context, Type, Uri) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    get_document_uri_by_type(TO, Type, Uri).
get_document_uri_by_type(Desc, Type, Uri) :-
    is_descriptor(Desc),
    !,
    open_descriptor(Desc,Transaction),
    get_document_by_type(Transaction, Type, Uri).
get_document_uri_by_type(DB, Type, Uri) :-
    database_context(DB,Prefixes),
    (   sub_atom(Type, _, _, _, ':')
    ->  Prefixed_Type = Type
    ;   atomic_list_concat(['@schema', ':', Type], Prefixed_Type)),
    prefix_expand(Prefixed_Type,Prefixes,Type_Ex),

    is_instance2(DB, Uri, Type_Ex).

get_document_by_type(Query_Context, Type, Document) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    get_document_by_type(TO, Type, Document).
get_document_by_type(Desc, Type, Document) :-
    is_descriptor(Desc),
    !,
    open_descriptor(Desc,Transaction),
    get_document_by_type(Transaction, Type, Document).
get_document_by_type(DB, Type, Document) :-
    database_context(DB,Prefixes),
    (   sub_atom(Type, _, _, _, ':')
    ->  Prefixed_Type = Type
    ;   atomic_list_concat(['@schema', ':', Type], Prefixed_Type)),
    prefix_expand(Prefixed_Type,Prefixes,Type_Ex),

    is_instance2(DB, Document_Uri, Type_Ex),

    get_document(DB, Document_Uri, Document).

get_document(Resource, Id, Document) :-
    get_document(Resource, true, Id, Document).

get_document(Query_Context, Compress, Id, Document) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    get_document(TO, Compress, Id, Document).
get_document(Desc, Compress, Id, Document) :-
    is_descriptor(Desc),
    !,
    open_descriptor(Desc,Transaction),
    get_document(Transaction, Compress, Id, Document).
get_document(DB, Compress, Id, Document) :-
    database_context(DB,Prefixes),
    get_document(DB, Prefixes, Compress, Id, Document).

get_document(DB, Prefixes, Compress, Id, Document) :-
    database_instance(DB,Instance),

    prefix_expand(Id,Prefixes,Id_Ex),

    xrdf(Instance, Id_Ex, rdf:type, Class),
    findall(
        Prop-Value,
        (   distinct([P],xrdf(Instance,Id_Ex,P,O)),
            \+ is_built_in(P),

            once(class_predicate_type(DB,Class,P,Type)),
            type_id_predicate_iri_value(Type,Id_Ex,P,O,DB,Prefixes,Value),

            (   Compress = true
            ->  compress_schema_uri(P, Prefixes, Prop)
            ;   Prop=P)
        ),
        Data),
    !,
    (   Compress = true
    ->  compress_dict_uri(Id_Ex, Prefixes, Id_comp),
        compress_schema_uri(Class, Prefixes, Class_comp),
        dict_create(Document,json,['@id'-Id_comp,
                                   '@type'-Class_comp
                                   |Data])
    ;   dict_create(Document,json,['@id'-Id_Ex,
                                   '@type'-Class
                                   |Data])).

key_descriptor_json(lexical(_, Fields), Prefixes, json{ '@type' : "Lexical",
                                                        '@fields' : Fields_Compressed }) :-
    maplist(
        {Prefixes}/[Field,Compressed]>>compress_schema_uri(Field, Prefixes, Compressed),
        Fields,
        Fields_Compressed
    ).
key_descriptor_json(hash(_, Fields), Prefixes, json{ '@type' : "Hash",
                                                     '@fields' : Fields_Compressed }) :-
    maplist(
        {Prefixes}/[Field,Compressed]>>compress_schema_uri(Field, Prefixes, Compressed),
        Fields,
        Fields_Compressed
    ).
key_descriptor_json(value_hash(_), _, json{ '@type' : "ValueHash" }).
key_descriptor_json(random(_), _, json{ '@type' : "Random" }).

documentation_descriptor_json(documentation(Comment, Properties), Prefixes, Result) :-
    Template = json{ '@comment' : Comment},
    (   Properties = []
    ->  Result = Template
    ;   maplist({Prefixes}/[Prop-Comment,JSON]>>(
                    compress_schema_uri(Prop, Prefixes, Small),
                    dict_pairs(JSON,json,[Small-Comment])
                ),
                Properties,
                JSONs),
        Result = (Template.put('@properties', JSONs))
    ).

type_descriptor_json(unit, _Prefix, "Unit").
type_descriptor_json(class(C), Prefixes, Class_Comp) :-
    compress_schema_uri(C, Prefixes, Class_Comp).
type_descriptor_json(base_class(C), Prefixes, Class_Comp) :-
    compress_schema_uri(C, Prefixes, Class_Comp).
type_descriptor_json(optional(C), Prefixes, json{ '@type' : "Optional",
                                                  '@class' : Class_Comp }) :-
    compress_schema_uri(C, Prefixes, Class_Comp).
type_descriptor_json(set(C), Prefixes, json{ '@type' : "Set",
                                             '@class' : Class_Comp }) :-
    compress_schema_uri(C, Prefixes, Class_Comp).
type_descriptor_json(array(C), Prefixes, json{ '@type' : "Array",
                                               '@class' : Class_Comp }) :-
    compress_schema_uri(C, Prefixes, Class_Comp).
type_descriptor_json(list(C), Prefixes, json{ '@type' : "List",
                                              '@class' : Class_Comp }) :-
    compress_schema_uri(C, Prefixes, Class_Comp).
type_descriptor_json(tagged_union(C,_), Prefixes, Class_Comp) :-
    compress_schema_uri(C, Prefixes, Class_Comp).
type_descriptor_json(enum(C,_),Prefixes, Class_Comp) :-
    compress_schema_uri(C, Prefixes, Class_Comp).

schema_subject_predicate_object_key_value(_,_,_Id,P,O^^_,'@base',O) :-
    global_prefix_expand(sys:base,P),
    !.
schema_subject_predicate_object_key_value(_,_,_Id,P,_,'@subdocument',[]) :-
    global_prefix_expand(sys:subdocument,P),
    !.
schema_subject_predicate_object_key_value(DB,Prefixes,Id,P,_,'@inherits',V) :-
    global_prefix_expand(sys:inherits,P),
    database_schema(DB,Schema),
    findall(Parent,
            (   xrdf(Schema, Id, sys:inherits, O),
                compress_schema_uri(O, Prefixes, Parent)
            ),
            Parent_List),
    (   Parent_List = [V]
    ->  true
    ;   Parent_List = V),
    !.
schema_subject_predicate_object_key_value(_,_,_Id,P,O^^_,'@schema',O) :-
    global_prefix_expand(sys:schema,P),
    !.
schema_subject_predicate_object_key_value(_,_,_Id,P,_,'@abstract',[]) :-
    global_prefix_expand(sys:abstract,P),
    !.
schema_subject_predicate_object_key_value(_,_,_Id,P,O,'@class',O) :-
    global_prefix_expand(sys:class,P),
    !.
schema_subject_predicate_object_key_value(DB,_,Id,P,O,'@value',Enum_List) :-
    global_prefix_expand(sys:value,P),
    !,
    database_schema(DB,Schema),
    rdf_list_list(Schema, O, L),
    maplist({Id}/[V,Enum]>>(
                atom_concat(Id,'_',Prefix),
                atom_concat(Prefix,Enum,V)
            ), L, Enum_List).
schema_subject_predicate_object_key_value(DB,Prefixes,Id,P,_,'@key',V) :-
    global_prefix_expand(sys:key,P),
    !,
    key_descriptor(DB, Id, Key),
    key_descriptor_json(Key,Prefixes,V).
schema_subject_predicate_object_key_value(DB,Prefixes,Id,P,_,'@documentation',V) :-
    global_prefix_expand(sys:documentation,P),
    !,
    documentation_descriptor(DB, Id, Documentation_Desc),
    documentation_descriptor_json(Documentation_Desc,Prefixes,V).
schema_subject_predicate_object_key_value(DB,Prefixes,_Id,P,O,K,JSON) :-
    compress_schema_uri(P, Prefixes, K),
    type_descriptor(DB, O, Descriptor),
    type_descriptor_json(Descriptor,Prefixes,JSON).

get_schema_document_uri(Query_Context, ID) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    get_schema_document_uri(TO, ID).
get_schema_document_uri(Desc, ID) :-
    is_descriptor(Desc),
    !,
    open_descriptor(Desc,Transaction),
    get_schema_document_uri(Transaction, ID).
get_schema_document_uri(_DB, '@context').
get_schema_document_uri(DB, Uri) :-
    is_simple_class(DB, Uri).

get_schema_document(DB, '@context', Document) :-
    !,
    database_context(DB, DB_Prefixes),
    get_schema_document(DB, 'terminusdb://context', Document_1),
    Document_2 = (Document_1.put('@type', '@context')),
    Document = (Document_2.put(DB_Prefixes)).
get_schema_document(DB, Id, Document) :-
    database_context(DB, DB_Prefixes),
    default_prefixes(Defaults),
    Prefixes = (Defaults.put(DB_Prefixes)),
    id_schema_json(DB, Prefixes, Id, Document).

get_schema_document_uri_by_type(DB, Type, Uri) :-
    default_prefixes(Prefixes),
    database_schema(DB,Schema),
    (   ground(Type)
    ->  prefix_expand_schema(Type, Prefixes, Type_Ex)
    ;   Type = Type_Ex
    ),

    xrdf(Schema, Uri, rdf:type, Type_Ex).

get_schema_document_by_type(DB, Type, Document) :-
    default_prefixes(Prefixes),
    database_schema(DB,Schema),
    (   ground(Type)
    ->  prefix_expand_schema(Type, Prefixes, Type_Ex)
    ;   Type = Type_Ex
    ),

    xrdf(Schema, Id_Ex, rdf:type, Type_Ex),
    id_schema_json(DB, Prefixes, Id_Ex, Document).

id_schema_json(DB, Id, JSON) :-
    default_prefixes(Defaults),
    id_schema_json(DB, Defaults, Id, JSON).

id_schema_json(DB, Prefixes, Id, JSON) :-
    database_schema(DB,Schema),
    (   ground(Id)
    ->  prefix_expand_schema(Id, Prefixes, Id_Ex)
    ;   Id = Id_Ex
    ),

    xrdf(Schema, Id_Ex, rdf:type, Class),

    findall(
        K-V,
        (   distinct([P],xrdf(Schema,Id_Ex,P,O)),
            schema_subject_predicate_object_key_value(DB,Prefixes,Id_Ex,P,O,K,V)
        ),
        Data),
    !,
    compress_schema_uri(Id_Ex, Prefixes, Id_Compressed),
    compress_schema_uri(Class, Prefixes, Class_Compressed),
    (   atom_concat('sys:',Small_Class, Class_Compressed)
    ->  true
    ;   Small_Class = Class_Compressed),
    dict_create(JSON,json,['@id'-Id_Compressed,
                           '@type'-Small_Class
                           |Data]).

validate_created_graph(schema, Layer) :-
    Validation_Object = validation_object{
                            descriptor: fake{},
                            instance_objects: [],
                            schema_objects: [graph_validation_obj{
                                                 descriptor: fake{},
                                                 read: Layer,
                                                 changed: true
                                             }]
                        },

    (   refute_schema(Validation_Object, Witness)
    ->  throw(error(schema_validation_error(Witness), _))
    ;   true).
validate_created_graph(instance(Transaction), Layer) :-
    Validation_Object = validation_object{
                            descriptor: fake{},
                            schema_objects: (Transaction.schema_objects),
                            instance_objects: [graph_validation_obj{
                                                 descriptor: fake{},
                                                 read: Layer,
                                                 changed: true
                                             }]
                        },

    (   refute_instance(Validation_Object, Witness)
    ->  throw(error(instance_validation_error(Witness), _))
    ;   true).

%%
% create_graph_from_json(+Store,+Graph_ID,+JSON_Stream,+Type:graph_type,-Layer) is det.
%
% Type := instance | schema(Database)
%
create_graph_from_json(Store, Graph_ID, JSON_Stream, Type, Layer) :-
    safe_create_named_graph(Store,Graph_ID,Graph_Obj),
    open_write(Store, Builder),

    write_json_stream_to_builder(JSON_Stream, Builder, Type),
    % commit this builder to a temporary layer to perform a diff.
    nb_commit(Builder,Layer),
    validate_created_graph(Type, Layer),
    nb_set_head(Graph_Obj, Layer).

replace_json_schema(Transaction, Stream) :-
    is_transaction(Transaction),
    !,
    database_schema(Transaction, [Schema]),
    delete_all(Schema),
    read_write_obj_builder(Schema, RWO),
    write_json_stream_to_builder(Stream, RWO, schema).
replace_json_schema(Query_Context, Stream) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    replace_json_schema(TO, Stream).


write_json_stream_to_builder(JSON_Stream, Builder, schema) :-
    !,
    json_read_dict(JSON_Stream, Context, [default_tag(json),end_of_file(eof)]),

    (   Context = eof
    ;   is_dict(Context),
        \+ get_dict('@type', Context, "@context")
    ->  throw(error(no_context_found_in_schema,_))
    ;   true
    ),

    forall(
        context_triple(Context,t(S,P,O)),
        (
            object_storage(O,OS),
            nb_add_triple(Builder, S, P, OS)
        )
    ),

    default_prefixes(Prefixes),
    put_dict(Context,Prefixes,Expanded_Context),

    forall(
        json_read_dict_stream(JSON_Stream, Dict),
        (
            forall(
                json_schema_triple(Dict,Expanded_Context,t(S,P,O)),
                (
                    object_storage(O,OS),
                    nb_add_triple(Builder, S, P, OS)
                )
            )
        )
    ).
write_json_stream_to_builder(JSON_Stream, Builder, instance(DB)) :-
    forall(
        json_read_dict_stream(JSON_Stream, Dict),
        (
            forall(
                json_triple(DB,Dict,t(S,P,O)),
                (
                    object_storage(O,OS),
                    nb_add_triple(Builder, S, P, OS)
                )
            )
        )
    ).

write_json_stream_to_schema(Transaction, Stream) :-
    transaction_object{} :< Transaction,
    !,
    [RWO] = (Transaction.schema_objects),
    read_write_obj_builder(RWO, Builder),

    write_json_stream_to_builder(Stream, Builder, schema).

write_json_stream_to_schema(Context, Stream) :-
    query_context{transaction_objects: [Transaction]} :< Context,
    write_json_stream_to_schema(Transaction, Stream).

write_json_stream_to_instance(Transaction, Stream) :-
    transaction_object{} :< Transaction,
    !,
    [RWO] = (Transaction.instance_objects),
    read_write_obj_builder(RWO, Builder),

    write_json_stream_to_builder(Stream, Builder, schema(Transaction)).

write_json_stream_to_instance(Context, Stream) :-
    query_context{transaction_objects: [Transaction]} :< Context,
    write_json_stream_to_instance(Transaction, Stream).

write_json_string_to_schema(Context, String) :-
    open_string(String, Stream),
    write_json_stream_to_schema(Context, Stream).

write_json_string_to_instance(Context, String) :-
    open_string(String, Stream),
    write_json_stream_to_instance(Context, Stream).

json_to_database_type(D^^T, _) :-
    is_list(D),
    !,
    throw(error(not_a_valid_datatype(D,T),_)).
json_to_database_type(D^^T, OC) :-
    (   string(D)
    ;   atom(D)),
    !,
    typecast(D^^'http://www.w3.org/2001/XMLSchema#string', T, [], OC).
json_to_database_type(D^^T, OC) :-
    number(D),
    !,
    typecast(D^^'http://www.w3.org/2001/XMLSchema#decimal', T, [], OC).
json_to_database_type(O, O).

%% Document insert / delete / update

run_delete_document(Desc, Commit, ID) :-
    create_context(Desc,Commit,Context),
    with_transaction(
        Context,
        delete_document(Context, ID),
        _).

delete_subdocument(DB, V) :-
    (   atom(V),
        instance_of(DB, V, C)
    ->  (   is_subdocument(DB, C)
        ->  key_descriptor(DB, C, Descriptor),
            (   memberchk(Descriptor,[lexical(_,_),hash(_,_),random(_)])
            ->  delete_document(DB, V)
            ;   true)
        ;   is_list_type(C)
        ->  delete_document(DB, V)
        ;   true
        )
    ;   true).

delete_document(DB, Id) :-
    is_transaction(DB),
    !,
    database_context(DB,Prefixes),
    database_instance(DB,Instance),
    prefix_expand(Id,Prefixes,Id_Ex),
    (   xrdf(Instance, Id_Ex, rdf:type, _)
    ->  true
    ;   throw(error(document_does_not_exist(Id),_))
    ),
    forall(
        xquad(Instance, G, Id_Ex, P, V),
        (   delete(G, Id_Ex, P, V, _),
            delete_subdocument(DB,V)
        )
    ).
delete_document(Query_Context, Id) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    delete_document(TO, Id).

nuke_documents(Transaction) :-
    is_transaction(Transaction),
    !,
    database_instance(Transaction, [Instance]),
    delete_all(Instance).
nuke_documents(Query_Context) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    nuke_documents(TO).

nuke_schema_documents(Transaction) :-
    is_transaction(Transaction),
    !,
    database_schema(Transaction, Schema),
    forall(
        (   xrdf(Schema, Id, rdf:type, Type),
            is_system_class(Type)),
        delete_schema_document(Transaction, Id)
    ).
nuke_schema_documents(Query_Context) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    nuke_documents(TO).

check_existing_document_status(Transaction, Document, Status) :-
    get_dict('@type', Document, Type),
    get_dict('@id', Document, Id),
    database_instance(Transaction, Instance),
    (   key_descriptor(Transaction, Type, value_hash(_))
    ->  (   xrdf(Instance, Id, _, _)
        ->  Status = equivalent
        ;   Status = not_present)
    ;   (   xrdf(Instance, Id, _, _)
        ->  Status = present
        ;   Status = not_present)
    ).

insert_document(Transaction, Document, ID) :-
    is_transaction(Transaction),
    !,
    json_elaborate(Transaction, Document, Elaborated),
    (   get_dict('@id', Elaborated, ID)
    ->  true
    ;   throw(error(no_id_in_schema_document(Elaborated), _))
    ),

    check_existing_document_status(Transaction, Elaborated, Status),
    (   Status = not_present
    ->  insert_document_expanded(Transaction, Elaborated, ID)
    ;   Status = equivalent
    ->  true
    ;   Status = present
    ->  throw(error(can_not_insert_existing_object_with_id(ID), _))
    ).
insert_document(Query_Context, Document, ID) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    insert_document(TO, Document, ID).

insert_document_expanded(Transaction, Elaborated, ID) :-
    get_dict('@id', Elaborated, ID),
    database_instance(Transaction, [Instance]),
    database_context(Transaction, Context),
    % insert
    forall(
        json_triple_(Elaborated, Context, t(S,P,O)),
        (   json_to_database_type(O,OC),
            insert(Instance, S, P, OC, _))
    ).

run_insert_document(Desc, Commit, Document, ID) :-
    create_context(Desc,Commit,Context),
    with_transaction(
        Context,
        insert_document(Context, Document, ID),
        _).

replace_document(DB, Document) :-
    replace_document(DB, Document, _).

replace_document(Transaction, Document, Id) :-
    is_transaction(Transaction),
    !,
    json_elaborate(Transaction, Document, Elaborated),
    get_dict('@id', Elaborated, Id),
    delete_document(Transaction, Id),
    insert_document_expanded(Transaction, Elaborated, Id).
replace_document(Query_Context, Document, Id) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    replace_document(TO, Document, Id).

run_replace_document(Desc, Commit, Document, Id) :-
    create_context(Desc,Commit,Context),
    with_transaction(
        Context,
        replace_document(Context, Document, Id),
        _).

class_frame(Validation_Object, Class, Frame) :-
    database_context(Validation_Object, DB_Prefixes),
    default_prefixes(Default_Prefixes),
    Prefixes = (Default_Prefixes.put(DB_Prefixes)),
    prefix_expand_schema(Class, Prefixes, Class_Ex),
    findall(
        Predicate_Comp-Type,
        (   class_predicate_type(Validation_Object, Class_Ex, Predicate, Type_Desc),
            type_descriptor_json(Type_Desc, Prefixes, Type),
            compress_schema_uri(Predicate, Prefixes, Predicate_Comp)
        ),
        Pairs),
    sort(Pairs, Sorted_Pairs),
    catch(
        dict_create(Frame,json,Sorted_Pairs),
        error(duplicate_key(Predicate),_),
        throw(error(violation_of_diamond_property(Class,Predicate),_))
    ).

insert_schema_document(Transaction, Document) :-
    is_transaction(Transaction),
    !,

    (   get_dict('@id', Document, Id)
    ->  true
    ;   throw(error(no_id_in_schema_document(Document), _))
    ),
    database_schema(Transaction, Schema),

    database_context(Transaction, Context),
    prefix_expand_schema(Id,Context,Id_Ex),
    do_or_die(
        \+ xrdf(Schema, Id_Ex, _, _),
        error(can_not_insert_existing_object_with_id(Id), _)),

    insert_schema_document_unsafe(Transaction, Document).
insert_schema_document(Query_Context, Document) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    insert_schema_document(TO, Document).

insert_schema_document_unsafe(Transaction, Document) :-
    is_transaction(Transaction),
    !,
    % Is this a context? If so do something else.
    database_context(Transaction, Context),
    database_schema(Transaction, [Schema]),

    default_prefixes(Prefixes),
    put_dict(Context,Prefixes,Expanded_Context),
    forall(
        json_schema_triple(Document, Expanded_Context, t(S,P,O)),
        insert(Schema, S, P, O, _)
    ).
insert_schema_document_unsafe(Query_Context, Document) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    insert_schema_document_unsafe(TO, Document).

% NOTE: This leaves garbage! We need a way to collect the leaves which
% link to array elements or lists.
delete_schema_document(Transaction, Id) :-
    is_transaction(Transaction),
    !,
    database_context(Transaction, Context),
    database_schema(Transaction, [Schema]),

    get_schema_document(Transaction, Id, Document),

    default_prefixes(Prefixes),
    put_dict(Context,Prefixes,Expanded_Context),

    forall(
        (   json_schema_triple(Document, Expanded_Context, t(S,P,O)),
            xrdf([Schema], S, P, O)),
        delete(Schema, S, P, O, _)
    ).
delete_schema_document(Query_Context, Id) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    delete_schema_document(TO, Id).

replace_schema_document(DB, Document) :-
    replace_schema_document(DB, Document, _Id).

replace_schema_document(Transaction, Document, Id) :-
    is_transaction(Transaction),
    !,
    (   get_dict('@id', Document, Id)
    ->  true
    ;   throw(error(no_id_in_schema_document(Document)))
    ),

    delete_schema_document(Transaction, Id),
    insert_schema_document_unsafe(Transaction, Document).
replace_schema_document(Query_Context, Document, Id) :-
    is_query_context(Query_Context),
    !,
    query_default_collection(Query_Context, TO),
    replace_schema_document(TO, Document, Id).



:- begin_tests(json_stream).
:- use_module(core(util)).
:- use_module(library(terminus_store)).
:- use_module(core(query), [ask/2]).

test(write_json_stream_to_builder, [
         setup(
             (   open_memory_store(Store),
                 open_write(Store,Builder)
             )
         )
     ]) :-

    open_string('
{ "@type" : "@context",
  "@base" : "terminusdb://system/data/",
  "@schema" : "http://terminusdb.com/system/schema#",
  "type" : "http://terminusdb.com/type#" }

{ "@id" : "User",
  "@type" : "Class",
  "key_hash" : "type:string",
  "capability" : { "@type" : "Set",
                   "@class" : "Capability" } }',Stream),

    write_json_stream_to_builder(Stream, Builder,schema),
    nb_commit(Builder,Layer),

    findall(
        t(X,Y,Z),
        triple(Layer,X,Y,Z),
        Triples),

    Triples = [
        t("http://terminusdb.com/system/schema#User",
          "http://terminusdb.com/system/schema#capability",
          node("http://terminusdb.com/system/schema#User_capability_Set_Capability")),
        t("http://terminusdb.com/system/schema#User",
          "http://terminusdb.com/system/schema#key_hash",
          node("http://terminusdb.com/type#string")),
        t("http://terminusdb.com/system/schema#User",
          "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
          node("http://terminusdb.com/schema/sys#Class")),
        t("http://terminusdb.com/system/schema#User_capability_Set_Capability",
          "http://terminusdb.com/schema/sys#class",
          node("http://terminusdb.com/system/schema#Capability")),
        t("http://terminusdb.com/system/schema#User_capability_Set_Capability",
          "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
          node("http://terminusdb.com/schema/sys#Set")),
        t("terminusdb://Prefix_Pair_93538b446fef31f7eef2e4d45f7addf0aa1d4ad5",
          "http://terminusdb.com/schema/sys#prefix",
          value("\"type\"^^'http://www.w3.org/2001/XMLSchema#string'")),
        t("terminusdb://Prefix_Pair_93538b446fef31f7eef2e4d45f7addf0aa1d4ad5",
          "http://terminusdb.com/schema/sys#url",
          value("\"http://terminusdb.com/type#\"^^'http://www.w3.org/2001/XMLSchema#string'")),
        t("terminusdb://Prefix_Pair_93538b446fef31f7eef2e4d45f7addf0aa1d4ad5",
          "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
          node("http://terminusdb.com/schema/sys#Prefix")),
        t("terminusdb://context",
          "http://terminusdb.com/schema/sys#base",
          value("\"terminusdb://system/data/\"^^'http://www.w3.org/2001/XMLSchema#string'")),
        t("terminusdb://context",
          "http://terminusdb.com/schema/sys#prefix_pair",
          node("terminusdb://Prefix_Pair_93538b446fef31f7eef2e4d45f7addf0aa1d4ad5")),
        t("terminusdb://context",
          "http://terminusdb.com/schema/sys#schema",
          value("\"http://terminusdb.com/system/schema#\"^^'http://www.w3.org/2001/XMLSchema#string'")),
        t("terminusdb://context",
          "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
          node("http://terminusdb.com/schema/sys#Context"))
    ].

:- end_tests(json_stream).

:- begin_tests(json).

:- use_module(core(util/test_utils)).

schema1('
{ "@type" : "@context",
  "@base" : "http://i/",
  "@schema" : "http://s/" }

{ "@id" : "Person",
  "@type" : "Class",
  "name" : "xsd:string",
  "birthdate" : "xsd:date",
  "friends" : { "@type" : "Set",
                "@class" : "Person" } }

{ "@id" : "Employee",
  "@type" : "Class",
  "@inherits" : "Person",
  "staff_number" : "xsd:string",
  "boss" : { "@type" : "Optional",
                 "@class" : "Employee" },
  "tasks" : { "@type" : "List",
              "@class" : "Task" } }

{ "@id" : "Task",
  "@type" : "Class",
  "name" : "xsd:string" }

{ "@id" : "Criminal",
  "@type" : "Class",
  "@inherits" : "Person",
  "aliases" : { "@type" : "List",
                "@class" : "xsd:string" } }
').

write_schema1(Desc) :-
    create_context(Desc,commit{
                            author : "me",
                            message : "none"},
                   Context),

    schema1(Schema1),

    % Schema
    with_transaction(
        Context,
        write_json_string_to_schema(Context, Schema1),
        _Meta).

test(get_field_values, []) :-
    Expanded = json{'@type':'http://s/Person',
                    'http://s/birthdate':
                    json{'@type':'http://www.w3.org/2001/XMLSchema#date',
                         '@value':"1979-12-28"},
                    'http://s/name':json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                                         '@value':"jane"}},
    get_field_values(Expanded, context{'@base':"http://i/",
                                       '@schema':"http://s/",
                                       '@type':'http://terminusdb.com/schema/sys#Context'},
                     [name, birthdate],
                     [ json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                            '@value':"jane"},
                       json{'@type':'http://www.w3.org/2001/XMLSchema#date',
                            '@value':"1979-12-28"}
                     ]).

test(create_database_context,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema1(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-
    open_descriptor(Desc, DB),
    type_context(DB,'Employee',Context),

    Context = json{ birthdate:json{ '@id':'http://s/birthdate',
		                            '@type':'http://www.w3.org/2001/XMLSchema#date'
		                          },
                    boss:json{'@id':'http://s/boss','@type':"@id"},
                    friends:json{'@container':"@set",
                                 '@id':'http://s/friends',
                                 '@type':'http://s/Person'},
                    name:json{ '@id':'http://s/name',
		                       '@type':'http://www.w3.org/2001/XMLSchema#string'
	                         },
                    staff_number:json{ '@id':'http://s/staff_number',
			                           '@type':'http://www.w3.org/2001/XMLSchema#string'
		                             },
                    tasks:json{'@container':"@list",'@id':'http://s/tasks','@type':'http://s/Task'}
                  }.

test(elaborate,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema1(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    Document = json{
                   '@id' : gavin,
                   '@type' : 'Criminal',
                   name : "gavin",
                   birthdate : "1977-05-24",
                   aliases : ["gavino", "gosha"]
               },

    open_descriptor(Desc, DB),

    json_elaborate(DB, Document, Elaborated),

    Elaborated = json{ '@id':'http://i/gavin',
                       '@type':'http://s/Criminal',
                       'http://s/aliases':
                       _{ '@container':"@list",
			              '@type':'http://www.w3.org/2001/XMLSchema#string',
			              '@value':[ json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
					                       '@value':"gavino"
					                     },
				                     json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
					                       '@value':"gosha"
					                     }
				                   ]
			            },
                       'http://s/birthdate':json{ '@type':'http://www.w3.org/2001/XMLSchema#date',
				                                  '@value':"1977-05-24"
			                                    },
                       'http://s/name':json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
			                                 '@value':"gavin"
			                               }
                     }.

test(id_expand,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema1(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    Document = json{
                   '@id' : gavin,
                   '@type' : 'Employee',
                   name : "gavin",
                   staff_number : "13",
                   birthdate : "1977-05-24",
                   boss : json{
                              '@id' : jane,
                              '@type' : 'Employee',
                              name : "jane",
                              staff_number : "12",
                              birthdate : "1979-12-28"
                          }
               },

    open_descriptor(Desc, DB),
    json_elaborate(DB, Document, Elaborated),

    Elaborated =
    json{
        '@id':'http://i/gavin',
        '@type':'http://s/Employee',
        'http://s/birthdate':json{ '@type':'http://www.w3.org/2001/XMLSchema#date',
				                   '@value':"1977-05-24"
			                     },
        'http://s/boss':json{ '@id':'http://i/jane',
			                  '@type':'http://s/Employee',
			                  'http://s/birthdate':json{ '@type':'http://www.w3.org/2001/XMLSchema#date',
						                                 '@value':"1979-12-28"
						                               },
			                  'http://s/name':json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
						                            '@value':"jane"
						                          },
			                  'http://s/staff_number':json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
							                                '@value':"12"
							                              }
			                },
        'http://s/name':json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
			                  '@value':"gavin"
			                },
        'http://s/staff_number':json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
				                      '@value':"13"
				                    }
    }.

test(triple_convert,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema1(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    Document = json{
                   '@id' : gavin,
                   '@type' : 'Employee',
                   name : "gavin",
                   staff_number : "13",
                   birthdate : "1977-05-24",
                   boss : json{
                              '@id' : jane,
                              '@type' : 'Employee',
                              name : "jane",
                              staff_number : "12",
                              birthdate : "1979-12-28"
                          }
               },

    open_descriptor(Desc, DB),
    json_triples(DB, Document, Triples),

    sort(Triples, Sorted),

    Sorted = [
        t('http://i/gavin',
          'http://s/birthdate',
          date(1977,5,24,0)^^'http://www.w3.org/2001/XMLSchema#date'),
        t('http://i/gavin','http://s/boss','http://i/jane'),
        t('http://i/gavin',
          'http://s/name',
          "gavin"^^'http://www.w3.org/2001/XMLSchema#string'),
        t('http://i/gavin',
          'http://s/staff_number',
          "13"^^'http://www.w3.org/2001/XMLSchema#string'),
        t('http://i/gavin',
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          'http://s/Employee'),
        t('http://i/jane',
          'http://s/birthdate',
          date(1979,12,28,0)^^'http://www.w3.org/2001/XMLSchema#date'),
        t('http://i/jane',
          'http://s/name',
          "jane"^^'http://www.w3.org/2001/XMLSchema#string'),
        t('http://i/jane',
          'http://s/staff_number',
          "12"^^'http://www.w3.org/2001/XMLSchema#string'),
        t('http://i/jane',
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          'http://s/Employee')
    ].

test(extract_json,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema1(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    Document = json{
                   '@id' : gavin,
                   '@type' : 'Employee',
                   name : "gavin",
                   staff_number : "13",
                   birthdate : "1977-05-24",
                   tasks : [],
                   boss : json{
                              '@id' : jane,
                              '@type' : 'Employee',
                              name : "jane",
                              tasks : [],
                              staff_number : "12",
                              birthdate : "1979-12-28"
                          }
               },

    run_insert_document(Desc, commit_info{ author: "Luke Skywalker",
                                           message: "foo" },
                        Document, Id),

    open_descriptor(Desc, DB),
    !, % NOTE: why does rolling back over this go mental?

    get_document(DB,Id,JSON1),

    !,
    JSON1 = json{'@id':gavin,
                 '@type':'Employee',
                 birthdate:"1977-05-24",
                 boss:jane,
                 tasks: [],
                 name:"gavin",
                 staff_number:"13"},

    get_document(DB,jane,JSON2),
    !,
    JSON2 = json{ '@id':jane,
                  '@type':'Employee',
                  birthdate:"1979-12-28",
                  tasks: [],
                  name:"jane",
                  staff_number:"12"
                }.

test(double_insert_document,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema1(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(
             can_not_insert_existing_object_with_id('http://i/jane')
         )
     ]) :-

    Document = json{
                   '@id' : gavin,
                   '@type' : 'Employee',
                   name : "gavin",
                   staff_number : "13",
                   birthdate : "1977-05-24",
                   tasks : [],
                   boss : json{
                              '@id' : jane,
                              '@type' : 'Employee',
                              name : "jane",
                              tasks : [],
                              staff_number : "12",
                              birthdate : "1979-12-28"
                          }
               },

    run_insert_document(Desc, commit_info{ author: "Luke Skywalker",
                                           message: "foo" },
                        Document, _Id1),

    Jane_Again =
    json{
        '@id' : jane,
        '@type' : 'Employee',
        name : "jane",
        tasks : [],
        staff_number : "12",
        birthdate : "1979-12-28"
    },
    run_insert_document(Desc, commit_info{ author: "Luke Skywalker",
                                           message: "bar" },
                        Jane_Again, _Id2).

test(get_value,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema1(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@id':jane,
                '@type':'Employee',
                birthdate:"1979-12-28",
                name:"jane",
                staff_number:"12"},

    open_descriptor(Desc, DB),
    json_elaborate(DB,JSON,Elaborated),

    get_all_path_values(Elaborated,Values),

    Values = [['@type']-'http://s/Employee',
              ['http://s/birthdate']-(date(1979,12,28,0)^^'http://www.w3.org/2001/XMLSchema#date'),
              ['http://s/name']-("jane"^^'http://www.w3.org/2001/XMLSchema#string'),
              ['http://s/staff_number']-("12"^^'http://www.w3.org/2001/XMLSchema#string')].

schema2('
{ "@type" : "@context",
  "@base" : "http://i/",
  "@schema" : "http://s/" }

{ "@id" : "Person",
  "@type" : "Class",
  "@base" : "Person_",
  "@key" : { "@type" : "Lexical",
             "@fields" : [ "name", "birthdate" ] },
  "name" : "xsd:string",
  "birthdate" : "xsd:date",
  "friends" : { "@type" : "Set",
                "@class" : "Person" } }

{ "@id" : "Employee",
  "@type" : "Class",
  "@inherits" : "Person",
  "@base" : "Employee_",
  "@key" : { "@type" : "Hash",
             "@fields" : [ "name", "birthdate" ] },
  "staff_number" : "xsd:string",
  "boss" : { "@type" : "Optional",
             "@class" : "Employee" },
  "tasks" : { "@type" : "List",
              "@class" : "Task" } }

{ "@id" : "Task",
  "@type" : "Class",
  "@key" : { "@type" : "ValueHash" },
  "name" : "xsd:string" }

{ "@id" : "Criminal",
  "@type" : "Class",
  "@inherits" : "Person",
  "aliases" : { "@type" : "List",
                "@class" : "xsd:string" } }

{ "@id" : "Event",
  "@type" : "Class",
  "@key" : { "@type" : "Random" },
  "action" : "xsd:string",
  "timestamp" : "xsd:dateTime" }

{ "@id" : "Book",
  "@type" : "Class",
  "@key" : { "@type" : "Lexical",
             "@fields" : ["name"] },
  "name" : "xsd:string" }

{ "@id" : "BookClub",
  "@type" : "Class",
  "@base" : "BookClub_",
  "@key" : { "@type" : "Lexical",
             "@fields" : ["name"] },
  "name" : "xsd:string",
  "people" : { "@type" : "Set",
               "@class" : "Person" },
  "book_list" : { "@type" : "Array",
                  "@class" : "Book" } }

{ "@id" : "Colour",
  "@type" : "Enum",
  "@value" : [ "red", "blue", "green" ] }

{ "@id" : "Dog",
  "@type" : "Class",
  "@base" : "Dog_",
  "@key" : { "@type" : "Lexical",
             "@fields" : [ "name" ] },
  "name" : "xsd:string",
  "hair_colour" : "Colour" }

{ "@id" : "BinaryTree",
  "@type" : "TaggedUnion",
  "@base" : "binary_tree_",
  "@key" : { "@type" : "ValueHash" },
  "leaf" : "sys:Unit",
  "node" : "Node" }

{ "@id" : "Node",
  "@type" : "Class",
  "@key" : { "@type" : "ValueHash" },
  "value" : "xsd:integer",
  "left" : "BinaryTree",
  "right" : "BinaryTree" }

{ "@id" : "Human",
  "@type" : "Class",
  "mother" : "Human",
  "father" : "Human" }

').

write_schema2(Desc) :-
    create_context(Desc,commit{
                            author : "me",
                            message : "none"},
                   Context),

    schema2(Schema1),

    % Schema
    with_transaction(
        Context,
        write_json_string_to_schema(Context, Schema1),
        _Meta).

test(schema_key_elaboration1, []) :-
    Doc = json{'@id':"Capability",
               '@key':json{'@type':"ValueHash"},
               '@type':"Class",
               role:json{'@class':"Role",
                         '@type':"Set"},
               scope:"Resource"},

    default_prefixes(Prefixes),
    Context = (Prefixes.put('@schema', 'https://s/')),

    json_schema_elaborate(Doc, Context, Elaborate),

    Elaborate =
    json{ '@id':'https://s/Capability',
          '@type':'http://terminusdb.com/schema/sys#Class',
          'http://terminusdb.com/schema/sys#key':
          json{ '@id':'https://s/Capability_ValueHash',
				'@type':'http://terminusdb.com/schema/sys#ValueHash'
			  },
          'https://s/role':
          json{ '@id':'https://s/Capability_role_Set_Role',
			    '@type':'http://terminusdb.com/schema/sys#Set',
			    'http://terminusdb.com/schema/sys#class':
                json{ '@id':'https://s/Role',
					  '@type':"@id"
					}
			  },
          'https://s/scope':
          json{'@id':'https://s/Resource',
               '@type':"@id"}
    }.

test(schema_lexical_key_elaboration, []) :-
    Doc = json{ '@id' : "Person",
                '@type' : "Class",
                '@base' : "Person_",
                '@key' : json{ '@type' : "Lexical",
                               '@fields' : [ "name", "birthdate" ] },
                'name' : "xsd:string",
                'birthdate' : "xsd:date",
                'friends' : json{ '@type' : "Set",
                                  '@class' : "Person" } },

    default_prefixes(Prefixes),
    Context = (Prefixes.put('@schema', 'https://s/')),

    json_schema_elaborate(Doc, Context, Elaborate),

    Elaborate =
    json{ '@id':'https://s/Person',
          '@type':'http://terminusdb.com/schema/sys#Class',
          'http://terminusdb.com/schema/sys#base':
          json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
				'@value':"Person_"
			  },
          'http://terminusdb.com/schema/sys#key':
          json{ '@id':'https://s/Person_Lexical_name_birthdate',
				'@type':'http://terminusdb.com/schema/sys#Lexical',
                'http://terminusdb.com/schema/sys#fields':
                json{ '@container':"@list",
					  '@type':"@id",
					  '@value': [ json{ '@id':'https://s/name',
									    '@type':"@id"
									  },
								  json{ '@id':'https://s/birthdate',
									    '@type':"@id"
									  }
							    ]
					}
			  },
          'https://s/birthdate':
          json{ '@id':'http://www.w3.org/2001/XMLSchema#date',
				'@type':"@id"
			  },
          'https://s/friends':
          json{ '@id':'https://s/Person_friends_Set_Person',
				'@type':'http://terminusdb.com/schema/sys#Set',
				'http://terminusdb.com/schema/sys#class':
                json{ '@id':'https://s/Person',
					  '@type':"@id"
					}
			  },
          'https://s/name':
          json{ '@id':'http://www.w3.org/2001/XMLSchema#string',
			    '@type':"@id"
			  }
        }.

test(idgen_lexical,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema2(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-
    JSON = json{'@type':'Person',
                birthdate:"1979-12-28",
                name:"jane"
               },

    open_descriptor(Desc, DB),
    json_elaborate(DB, JSON, Elaborated),

    Elaborated =
    json{'@id':'http://i/Person_jane_1979-12-28',
         '@type':'http://s/Person',
         'http://s/birthdate':json{'@type':'http://www.w3.org/2001/XMLSchema#date',
                                   '@value':"1979-12-28"},
         'http://s/name':json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                              '@value':"jane"}
        }.

test(idgen_hash,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema2(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@type':'Employee',
                birthdate:"1979-12-28",
                name:"jane",
                staff_number:"13"
               },

    open_descriptor(Desc, DB),
    json_elaborate(DB, JSON, Elaborated),

    Elaborated =
    json{
        '@id':'http://i/Employee_3ef31cdd168b76d42f61e49114c8886c61640bca',
        '@type':'http://s/Employee',
        'http://s/birthdate':json{ '@type':'http://www.w3.org/2001/XMLSchema#date',
				                   '@value':"1979-12-28"
			                     },
        'http://s/name':json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
			                  '@value':"jane"
			                },
        'http://s/staff_number':json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
				                      '@value':"13"
				                    }
    }.

test(idgen_value_hash,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema2(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@type':'Task',
                name:"Groceries"},

    open_descriptor(Desc, DB),
    json_elaborate(DB,JSON, Elaborated),
    Elaborated =
    json{ '@id':'http://i/Task_0525416b27d3c2cadc4a9efdfb9a3aa620966c91',
          '@type':'http://s/Task',
          'http://s/name':json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
			                    '@value':"Groceries"
			                  }
        }.

test(idgen_random,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema2(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@type': 'Event',
                action: "click click",
                timestamp: "2021-05-20T20:33:00.000Z"
               },

    open_descriptor(Desc, DB),
    json_elaborate(DB, JSON, Elaborated),

    Elaborated =
    json{ '@id':Id,
          '@type':'http://s/Event',
          'http://s/action':json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
			                      '@value':"click click"
			                    },
          'http://s/timestamp':json{ '@type':'http://www.w3.org/2001/XMLSchema#dateTime',
				                     '@value':"2021-05-20T20:33:00.000Z"
			                       }
        },

    atom_concat('http://i/Event_',_,Id).

test(type_family_id, []) :-

    type_family_id(json{'@type':"Cardinality",
                        '@cardinality':3,
                        '@class':'Person'},
                   [friend_of, 'Person'], 'Person_friend_of_Cardinality_Person_3').

test(schema_elaborate, []) :-

    Schema = json{ '@type' : 'Class',
                   '@id' : 'Person',
                   'name' : 'xsd:string',
                   'age' : json{ '@type' : 'Optional',
                                 '@class' : 'xsd:decimal' },
                   'friend_of' : json{ '@type' : 'Cardinality',
                                       '@class' : 'Person',
                                       '@cardinality' : 3 }
                 },

    default_prefixes(Prefixes),
    Context = (Prefixes.put('@schema', 'https://s#')),

    json_schema_elaborate(Schema, Context, Elaborated),

    Elaborated =
    json{ '@id':'https://s#Person',
          '@type':'http://terminusdb.com/schema/sys#Class',
          'https://s#age':
          json{ '@id':'https://s#Person_age_Optional_xsd%3Adecimal',
			    '@type':'http://terminusdb.com/schema/sys#Optional',
			    'http://terminusdb.com/schema/sys#class':
                json{ '@id':'http://www.w3.org/2001/XMLSchema#decimal',
					  '@type':"@id"
					}
			  },
          'https://s#friend_of':
          json{ '@id':'https://s#Person_friend_of_Cardinality_Person',
				'@type':'http://terminusdb.com/schema/sys#Cardinality',
				'http://terminusdb.com/schema/sys#cardinality':
                json{ '@type':'http://www.w3.org/2001/XMLSchema#nonNegativeInteger',
					  '@value':3
					},
				'http://terminusdb.com/schema/sys#class':
                json{ '@id':'https://s#Person',
					  '@type':"@id"
					}
			  },
          'https://s#name':
          json{ '@id':'http://www.w3.org/2001/XMLSchema#string',
			    '@type':"@id"
			  }
        },

    findall(Triple,
            json_schema_triple(Schema, Context, Triple),
            Triples),

    sort(Triples,Sorted),

    Sorted = [
        t('https://s#Person',
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          'http://terminusdb.com/schema/sys#Class'),
        t('https://s#Person',
          'https://s#age',
          'https://s#Person_age_Optional_xsd%3Adecimal'),
        t('https://s#Person',
          'https://s#friend_of',
          'https://s#Person_friend_of_Cardinality_Person'),
        t('https://s#Person',
          'https://s#name',
          'http://www.w3.org/2001/XMLSchema#string'),
        t('https://s#Person_age_Optional_xsd%3Adecimal',
          'http://terminusdb.com/schema/sys#class',
          'http://www.w3.org/2001/XMLSchema#decimal'),
        t('https://s#Person_age_Optional_xsd%3Adecimal',
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          'http://terminusdb.com/schema/sys#Optional'),
        t('https://s#Person_friend_of_Cardinality_Person',
          'http://terminusdb.com/schema/sys#cardinality',
          3^^'http://www.w3.org/2001/XMLSchema#nonNegativeInteger'),
        t('https://s#Person_friend_of_Cardinality_Person',
          'http://terminusdb.com/schema/sys#class',
          'https://s#Person'),
        t('https://s#Person_friend_of_Cardinality_Person',
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          'http://terminusdb.com/schema/sys#Cardinality')
    ].

test(list_id_key_context_triple, []) :-
    findall(Triple,
            list_id_key_context_triple(
                [json{'@id':"task_a4963868aa3ad8365a4b164a7f206ffc",
                      '@type':task,
                      name:json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                                '@value':"Get Groceries"}},
                 json{'@id':"task_f9e4104c952e71025a1d68218d88bab1",
                      '@type':task,
                      name:json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                                '@value':"Take out rubbish"}}],
                elt,
                p,
                _{'@base' : ''},
                Triple),
            Triples),

    Triples = [
        t(elt,p,Cons1),
        t(Cons1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/1999/02/22-rdf-syntax-ns#List'),
        t(Cons1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',"task_a4963868aa3ad8365a4b164a7f206ffc"),
        t(Cons1,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',Cons2),
        t(Cons2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/1999/02/22-rdf-syntax-ns#List'),
        t(Cons2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',"task_f9e4104c952e71025a1d68218d88bab1"),
        t(Cons2,'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',_Nil),
        t("task_f9e4104c952e71025a1d68218d88bab1",'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',task),
        t("task_f9e4104c952e71025a1d68218d88bab1",name,"Take out rubbish"^^'http://www.w3.org/2001/XMLSchema#string'),
        t("task_a4963868aa3ad8365a4b164a7f206ffc",'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',task),
        t("task_a4963868aa3ad8365a4b164a7f206ffc",name,"Get Groceries"^^'http://www.w3.org/2001/XMLSchema#string')
    ].

test(array_id_key_triple, []) :-
    findall(Triple,
            array_id_key_context_triple(
                [json{'@id':"task_a4963868aa3ad8365a4b164a7f206ffc",
                      '@type':task,
                      name:json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                                '@value':"Get Groceries"}},
                 json{'@id':"task_f9e4104c952e71025a1d68218d88bab1",
                      '@type':task,
                      name:json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                                '@value':"Take out rubbish"}}],
                elt,
                p,
                _{'@base' : ''},
                Triple),
            Triples),

    Triples = [
        t(elt,p,Array0),
        t(Array0,
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          'http://terminusdb.com/schema/sys#Array'),
        t(Array0,
          'http://terminusdb.com/schema/sys#value',
          "task_a4963868aa3ad8365a4b164a7f206ffc"),
        t(Array0,
          'http://terminusdb.com/schema/sys#index',
          0^^'http://www.w3.org/2001/XMLSchema#nonNegativeInteger'),
        t(elt,p,Array1),
        t(Array1,
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          'http://terminusdb.com/schema/sys#Array'),
        t(Array1,
          'http://terminusdb.com/schema/sys#value',
          "task_f9e4104c952e71025a1d68218d88bab1"),
        t(Array1,
          'http://terminusdb.com/schema/sys#index',
          1^^'http://www.w3.org/2001/XMLSchema#nonNegativeInteger'),
        t("task_f9e4104c952e71025a1d68218d88bab1",
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          task),
        t("task_f9e4104c952e71025a1d68218d88bab1",
          name,
          "Take out rubbish"^^'http://www.w3.org/2001/XMLSchema#string'),
        t("task_a4963868aa3ad8365a4b164a7f206ffc",
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          task),
        t("task_a4963868aa3ad8365a4b164a7f206ffc",
          name,
          "Get Groceries"^^'http://www.w3.org/2001/XMLSchema#string')
    ].

test(set_id_key_context_triple, []) :-
    findall(Triple,
            set_id_key_context_triple(
                [json{'@id':"task_a4963868aa3ad8365a4b164a7f206ffc",
                      '@type':task,
                      name:json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                                '@value':"Get Groceries"}},
                 json{'@id':"task_f9e4104c952e71025a1d68218d88bab1",
                      '@type':task,
                      name:json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                                '@value':"Take out rubbish"}}],
                elt,
                p,
                _{},
                Triple),
            Triples),

    Triples = [
        t(elt,p,"task_a4963868aa3ad8365a4b164a7f206ffc"),
        t(elt,p,"task_f9e4104c952e71025a1d68218d88bab1"),
        t("task_f9e4104c952e71025a1d68218d88bab1",
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          task),
        t("task_f9e4104c952e71025a1d68218d88bab1",
          name,
          "Take out rubbish"^^'http://www.w3.org/2001/XMLSchema#string'),
        t("task_a4963868aa3ad8365a4b164a7f206ffc",
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          task),
        t("task_a4963868aa3ad8365a4b164a7f206ffc",
          name,
          "Get Groceries"^^'http://www.w3.org/2001/XMLSchema#string')
    ].


test(list_elaborate,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema2(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@type':'Employee',
                name: "Gavin",
                birthdate: "1977-05-24",
                staff_number: "12",
                tasks : [
                    json{ name : "Get Groceries" },
                    json{ name : "Take out rubbish" }
                ]
               },

    open_descriptor(Desc, DB),
    json_elaborate(DB, JSON, Elaborated),

    Elaborated =
    json{ '@id':'http://i/Employee_2ec148d64a1bd9ec4759064e013604bcc46358fe',
          '@type':'http://s/Employee',
          'http://s/birthdate':json{ '@type':'http://www.w3.org/2001/XMLSchema#date',
				                     '@value':"1977-05-24"
			                       },
          'http://s/name':json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
			                    '@value':"Gavin"
			                  },
          'http://s/staff_number':json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
				                        '@value':"12"
				                      },
          'http://s/tasks':
          _{ '@container':"@list",
			 '@type':'http://s/Task',
			 '@value':[ json{ '@id':'http://i/Task_3f11cdcf8dfc89eb56ee2f970c6d9f0108515676',
					          '@type':'http://s/Task',
					          'http://s/name':
                              json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
								    '@value':"Get Groceries"
							      }
					        },
				        json{ '@id':'http://i/Task_86a03131d587c39afa75620b12692f1c998faec0',
					          '@type':'http://s/Task',
					          'http://s/name':
                              json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
								    '@value':"Take out rubbish"
							      }
					        }
				      ]
		   }
        },

    json_triples(DB, JSON, Triples),

    Triples =
    [ t('http://i/Employee_2ec148d64a1bd9ec4759064e013604bcc46358fe',
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        'http://s/Employee'),
      t('http://i/Employee_2ec148d64a1bd9ec4759064e013604bcc46358fe',
        'http://s/birthdate',
        date(1977,5,24,0)^^'http://www.w3.org/2001/XMLSchema#date'),
      t('http://i/Employee_2ec148d64a1bd9ec4759064e013604bcc46358fe',
        'http://s/name',
        "Gavin"^^'http://www.w3.org/2001/XMLSchema#string'),
      t('http://i/Employee_2ec148d64a1bd9ec4759064e013604bcc46358fe',
        'http://s/staff_number',
        "12"^^'http://www.w3.org/2001/XMLSchema#string'),
      t('http://i/Employee_2ec148d64a1bd9ec4759064e013604bcc46358fe',
        'http://s/tasks',
        Cons0),
      t(Cons0,
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#List'),
      t(Cons0,
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',
        'http://i/Task_3f11cdcf8dfc89eb56ee2f970c6d9f0108515676'),
      t(Cons0,
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',
        Cons1),
      t(Cons1,
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#List'),
      t(Cons1,
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#first',
        'http://i/Task_86a03131d587c39afa75620b12692f1c998faec0'),
      t(Cons1,
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest',
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#nil'),
      t('http://i/Task_86a03131d587c39afa75620b12692f1c998faec0',
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        'http://s/Task'),
      t('http://i/Task_86a03131d587c39afa75620b12692f1c998faec0',
        'http://s/name',
        "Take out rubbish"^^'http://www.w3.org/2001/XMLSchema#string'),
      t('http://i/Task_3f11cdcf8dfc89eb56ee2f970c6d9f0108515676',
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        'http://s/Task'),
      t('http://i/Task_3f11cdcf8dfc89eb56ee2f970c6d9f0108515676',
        'http://s/name',
        "Get Groceries"^^'http://www.w3.org/2001/XMLSchema#string')
    ],

    run_insert_document(Desc, commit_object{ author : "me", message : "boo"}, JSON, Id),

    open_descriptor(Desc, New_DB),
    get_document(New_DB, Id, Fresh_JSON),

    Fresh_JSON =
    json{ '@id':'Employee_2ec148d64a1bd9ec4759064e013604bcc46358fe',
          '@type':'Employee',
          birthdate:"1977-05-24",
          name:"Gavin",
          staff_number:"12",
          tasks:[ 'Task_3f11cdcf8dfc89eb56ee2f970c6d9f0108515676',
	              'Task_86a03131d587c39afa75620b12692f1c998faec0'
	            ]
        }.

test(array_elaborate,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema2(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@type':'BookClub',
                name: "Marxist book club",
                book_list : [
                    json{ name : "Das Kapital" },
                    json{ name : "Der Ursprung des Christentums" }
                ]
               },

    open_descriptor(Desc, DB),
    json_elaborate(DB,JSON,Elaborated),

    Elaborated = json{ '@id':'http://i/BookClub_Marxist%20book%20club',
                       '@type':'http://s/BookClub',
                       'http://s/book_list':
                       _{ '@container':"@array",
			              '@type':'http://s/Book',
			              '@value':[ json{ '@id':'http://i/Book_Das%20Kapital',
					                       '@type':'http://s/Book',
					                       'http://s/name':
                                           json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
								                 '@value':"Das Kapital"
								               }
					                     },
					                 json{ '@id':'http://i/Book_Der%20Ursprung%20des%20Christentums',
					                       '@type':'http://s/Book',
					                       'http://s/name':
                                           json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
								                 '@value':"Der Ursprung des Christentums"
								               }
					                     }
				                   ]
			            },
                       'http://s/name':json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
			                                 '@value':"Marxist book club"
			                               }
                     },

    json_triples(DB, JSON, Triples),

    Triples = [
        t('http://i/BookClub_Marxist%20book%20club',
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          'http://s/BookClub'),
        t('http://i/BookClub_Marxist%20book%20club',
          'http://s/book_list',
          Array0),
        t(Array0,
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          'http://terminusdb.com/schema/sys#Array'),
        t(Array0,
          'http://terminusdb.com/schema/sys#value',
          'http://i/Book_Das%20Kapital'),
        t(Array0,
          'http://terminusdb.com/schema/sys#index',
          0^^'http://www.w3.org/2001/XMLSchema#nonNegativeInteger'),
        t('http://i/BookClub_Marxist%20book%20club',
          'http://s/book_list',
          Array1),
        t(Array1,
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          'http://terminusdb.com/schema/sys#Array'),
        t(Array1,
          'http://terminusdb.com/schema/sys#value',
          'http://i/Book_Der%20Ursprung%20des%20Christentums'),
        t(Array1,
          'http://terminusdb.com/schema/sys#index',
          1^^'http://www.w3.org/2001/XMLSchema#nonNegativeInteger'),
        t('http://i/Book_Der%20Ursprung%20des%20Christentums',
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          'http://s/Book'),
        t('http://i/Book_Der%20Ursprung%20des%20Christentums',
          'http://s/name',
          "Der Ursprung des Christentums"^^'http://www.w3.org/2001/XMLSchema#string'),
        t('http://i/Book_Das%20Kapital',
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          'http://s/Book'),
        t('http://i/Book_Das%20Kapital',
          'http://s/name',
          "Das Kapital"^^'http://www.w3.org/2001/XMLSchema#string'),
        t('http://i/BookClub_Marxist%20book%20club',
          'http://s/name',
          "Marxist book club"^^'http://www.w3.org/2001/XMLSchema#string')
    ],

    run_insert_document(Desc, commit_object{ author : "me", message : "boo"}, JSON, Id),

    open_descriptor(Desc, New_DB),
    get_document(New_DB, Id, Recovered),

    Recovered = json{ '@id':'BookClub_Marxist%20book%20club',
                      '@type':'BookClub',
                      book_list:[ 'Book_Das%20Kapital',
		                          'Book_Der%20Ursprung%20des%20Christentums'
		                        ],
                      name:"Marxist book club"
                    }.
test(set_elaborate,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema2(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@type':'BookClub',
                name: "Marxist book club",
                people: [
                    json{'@type' : 'Person',
                         name : "jim",
                         birthdate: "1982-05-03"
                        },
                    json{'@type':'Person',
                         birthdate:"1979-12-28",
                         name:"jane"
                        }],
                book_list : []
               },

    open_descriptor(Desc, DB),
    json_elaborate(DB, JSON, Elaborated),

    Elaborated =
    json{ '@id':'http://i/BookClub_Marxist%20book%20club',
          '@type':'http://s/BookClub',
          'http://s/book_list':
          _{ '@container':"@array",
			 '@type':'http://s/Book',
			 '@value':[]
		   },
          'http://s/name':
          json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
			    '@value':"Marxist book club"
			  },
          'http://s/people':
          _{ '@container':"@set",
			 '@type':'http://s/Person',
			 '@value':[ json{ '@id':'http://i/Person_jim_1982-05-03',
					          '@type':'http://s/Person',
					          'http://s/birthdate':
                              json{ '@type':'http://www.w3.org/2001/XMLSchema#date',
								    '@value':"1982-05-03"
								  },
					          'http://s/name':
                              json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
								    '@value':"jim"
								  }
					        },
				        json{ '@id':'http://i/Person_jane_1979-12-28',
					          '@type':'http://s/Person',
					          'http://s/birthdate':
                              json{ '@type':'http://www.w3.org/2001/XMLSchema#date',
								    '@value':"1979-12-28"
								  },
					          'http://s/name':
                              json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
								    '@value':"jane"
								  }
					        }
				      ]
		   }
        },

    json_triples(DB, JSON, Triples),

    Triples = [
        t('http://i/BookClub_Marxist%20book%20club',
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          'http://s/BookClub'),
        t('http://i/BookClub_Marxist%20book%20club',
          'http://s/name',
          "Marxist book club"^^'http://www.w3.org/2001/XMLSchema#string'),
        t('http://i/BookClub_Marxist%20book%20club',
          'http://s/people',
          'http://i/Person_jim_1982-05-03'),
        t('http://i/BookClub_Marxist%20book%20club',
          'http://s/people',
          'http://i/Person_jane_1979-12-28'),
        t('http://i/Person_jane_1979-12-28',
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          'http://s/Person'),
        t('http://i/Person_jane_1979-12-28',
          'http://s/birthdate',
          date(1979,12,28,0)^^'http://www.w3.org/2001/XMLSchema#date'),
        t('http://i/Person_jane_1979-12-28',
          'http://s/name',
          "jane"^^'http://www.w3.org/2001/XMLSchema#string'),
        t('http://i/Person_jim_1982-05-03',
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          'http://s/Person'),
        t('http://i/Person_jim_1982-05-03',
          'http://s/birthdate',
          date(1982,5,3,0)^^'http://www.w3.org/2001/XMLSchema#date'),
        t('http://i/Person_jim_1982-05-03',
          'http://s/name',
          "jim"^^'http://www.w3.org/2001/XMLSchema#string')
    ],

    run_insert_document(Desc, commit_object{ author : "me", message : "boo"}, JSON, Id),

    open_descriptor(Desc, New_DB),
    get_document(New_DB, Id, Book_Club),

    Book_Club = json{ '@id':'BookClub_Marxist%20book%20club',
                      '@type':'BookClub',
                      name:"Marxist book club",
                      people:['Person_jane_1979-12-28','Person_jim_1982-05-03']
                    }.

test(set_elaborate_id,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema2(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@type':'BookClub',
                name: "Marxist book club",
                people: [ "Person_jim_1982-05-03", "Person_jane_1979-12-28"],
                book_list : []
               },

    open_descriptor(Desc, DB),
    json_elaborate(DB, JSON, Elaborated),
    Elaborated =
    json{'@id':'http://i/BookClub_Marxist%20book%20club',
         '@type':'http://s/BookClub',
         'http://s/book_list':_1506{'@container':"@array",
                                    '@type':'http://s/Book',
                                    '@value':[]},
         'http://s/name':json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                              '@value':"Marxist book club"},
         'http://s/people':_1390{'@container':"@set",
                                 '@type':'http://s/Person',
                                 '@value':[json{'@id':'http://i/Person_jim_1982-05-03',
                                                '@type':"@id"},
                                           json{'@id':'http://i/Person_jane_1979-12-28',
                                                '@type':"@id"}]}}.

test(elaborate_id,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema2(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@type':'Human',
                '@id' : "Cleatus",
                'mother' : "MaryJane",
                'father' : "BobbyJoe" },

    open_descriptor(Desc, DB),
    json_elaborate(DB, JSON, Elaborated),
    Elaborated =
    json{'@id':'http://i/Cleatus',
         '@type':'http://s/Human',
         'http://s/father':json{'@id':'http://i/BobbyJoe',
                                '@type':"@id"},
         'http://s/mother':json{'@id':'http://i/MaryJane',
                                '@type':"@id"}}.

test(empty_list,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema2(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@type':'Employee',
                name: "Gavin",
                birthdate: "1977-05-24",
                staff_number: "12",
                tasks : []
               },

    run_insert_document(Desc, commit_object{ author : "me", message : "boo"}, JSON, Id),

    open_descriptor(Desc, DB),
    get_document(DB, Id, Employee_JSON),

    Employee_JSON = json{'@id':_,
                         '@type':'Employee',
                         birthdate:"1977-05-24",
                         name:"Gavin",
                         staff_number:"12",
                         tasks:[]}.

test(enum_elaborate,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema2(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    open_descriptor(Desc, DB),

    type_context(DB,'Dog',TypeContext),

    TypeContext = json{ hair_colour:json{'@id':'http://s/hair_colour',
                                         '@type':'http://s/Colour'},
                        name:json{ '@id':'http://s/name',
		                           '@type':'http://www.w3.org/2001/XMLSchema#string'
	                             }
                      },

    JSON = json{'@type':'Dog',
                name: "Ralph",
                hair_colour: "blue"
               },

    json_elaborate(DB, JSON, Elaborated),

    Elaborated = json{ '@id':'http://i/Dog_Ralph',
                       '@type':'http://s/Dog',
                       'http://s/hair_colour':json{'@id':'http://s/Colour_blue',
                                                   '@type':"@id"},
                       'http://s/name':json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
			                                 '@value':"Ralph"
			                               }
                     },

    run_insert_document(Desc, commit_info{ author: "Luke Skywalker",
                                           message: "foo" },
                        JSON, Id),

    open_descriptor(Desc, New_DB),
    get_document(New_DB,Id, Dog_JSON),

    Dog_JSON = json{'@id':'Dog_Ralph',
                    '@type':'Dog',
                    hair_colour:blue,
                    name:"Ralph"}.


test(elaborate_tagged_union,[]) :-

    Binary_Tree = json{ '@type' : 'TaggedUnion',
                        '@id' : 'BinaryTree',
                        '@base' : "BinaryTree_",
                        '@key' : json{ '@type' : 'ValueHash' },
                        leaf : 'sys:Unit',
                        node : 'Node'
                      },

    Node = json{ '@type' : 'Class',
                 '@id' : 'Node',
                 '@base' : "Node_",
                 '@key' : json{ '@type' : 'ValueHash' },
                 value : 'xsd:integer',
                 left : "BinaryTree",
                 right : "BinaryTree"
               },

    default_prefixes(Prefixes),
    Context = (Prefixes.put('@schema', 'https://s/')),

    json_schema_elaborate(Binary_Tree, Context, BT_Elaborated),

    BT_Elaborated =
    json{ '@id':'https://s/BinaryTree',
          '@type':'http://terminusdb.com/schema/sys#TaggedUnion',
          'http://terminusdb.com/schema/sys#base':
          json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
				'@value':"BinaryTree_"
			  },
          'http://terminusdb.com/schema/sys#key':
          json{ '@id':'https://s/BinaryTree_ValueHash',
				'@type':'http://terminusdb.com/schema/sys#ValueHash'
			  },
          'https://s/leaf':json{ '@id':'http://terminusdb.com/schema/sys#Unit',
			                     '@type':"@id"
			                   },
          'https://s/node':json{'@id':'https://s/Node','@type':"@id"}
        },

    json_schema_elaborate(Node, Context, Node_Elaborated),

    Node_Elaborated =
    json{ '@id':'https://s/Node',
          '@type':'http://terminusdb.com/schema/sys#Class',
          'http://terminusdb.com/schema/sys#base':
          json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
				'@value':"Node_"
			  },
          'http://terminusdb.com/schema/sys#key':
          json{ '@id':'https://s/Node_ValueHash',
				'@type':'http://terminusdb.com/schema/sys#ValueHash'
			  },
          'https://s/left':json{'@id':'https://s/BinaryTree','@type':"@id"},
          'https://s/right':json{'@id':'https://s/BinaryTree','@type':"@id"},
          'https://s/value':json{ '@id':'http://www.w3.org/2001/XMLSchema#integer',
			                      '@type':"@id"
			                    }
        }.

test(binary_tree_context,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema2(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    open_descriptor(Desc, DB),
    type_context(DB,'BinaryTree', Binary_Context),

    Binary_Context = json{ leaf:json{'@id':'http://s/leaf'},
                           node:json{'@id':'http://s/node','@type':"@id"}
                         },
    type_context(DB,'Node', Node_Context),
    Node_Context = json{ left:json{'@id':'http://s/left','@type':"@id"},
                         right:json{'@id':'http://s/right','@type':"@id"},
                         value:json{ '@id':'http://s/value',
		                             '@type':'http://www.w3.org/2001/XMLSchema#integer'
		                           }
                       }.

test(binary_tree_elaborate,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema2(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@type':'BinaryTree',
                node: json{'@type':'Node',
                           value: 1,
                           left: json{'@type':'BinaryTree',
                                      node: json{'@type':'Node',
                                                 value: 0,
                                                 left: json{'@type':'BinaryTree',
                                                            leaf : []},
                                                 right: json{'@type':'BinaryTree',
                                                             leaf : []}}},
                           right: json{'@type':'BinaryTree',
                                      node: json{'@type':'Node',
                                                 value: 2,
                                                 left: json{'@type':'BinaryTree',
                                                            leaf : []},
                                                 right: json{'@type':'BinaryTree',
                                                             leaf : []}}}}},

    open_descriptor(Desc, DB),
    json_elaborate(DB,JSON,Elaborated),

    Elaborated =
    json{ '@id':'http://i/binary_tree_27afb2c9dc1067ffab4d12f5f1170cf8a419361d',
          '@type':'http://s/BinaryTree',
          'http://s/node':
          json{ '@id':'http://i/Node_9e1c81332e7435825b35bd9d9b9936d1c99057fd',
			    '@type':'http://s/Node',
			    'http://s/left':
                json{ '@id':'http://i/binary_tree_93723c4a3745a147119da53da17d9d6ecc037eda',
					  '@type':'http://s/BinaryTree',
					  'http://s/node':
                      json{ '@id':'http://i/Node_a9cb582dfc264bce6c8bc765f4a934b3522380b0',
							'@type':'http://s/Node',
							'http://s/left':
                            json{ '@id':'http://i/binary_tree_6e745bace66ec54f2b359be3c60b472969a448a2',
								  '@type':'http://s/BinaryTree',
								  'http://s/leaf':[]
								},
							'http://s/right':
                            json{ '@id':'http://i/binary_tree_6e745bace66ec54f2b359be3c60b472969a448a2',
								  '@type':'http://s/BinaryTree',
								  'http://s/leaf':[]
								},
							'http://s/value':
                            json{ '@type':'http://www.w3.org/2001/XMLSchema#integer',
								  '@value':0
								}
						  }
					},
			    'http://s/right':
                json{ '@id':'http://i/binary_tree_0d302510badd3c2d41d33f018da75638d59f0253',
					  '@type':'http://s/BinaryTree',
					  'http://s/node':
                      json{ '@id':'http://i/Node_d8bb6d9fcf734d417696881e96ecb38b02588df7',
							'@type':'http://s/Node',
							'http://s/left':
                            json{ '@id':'http://i/binary_tree_6e745bace66ec54f2b359be3c60b472969a448a2',
								  '@type':'http://s/BinaryTree',
								  'http://s/leaf':[]
								},
							'http://s/right':
                            json{ '@id':'http://i/binary_tree_6e745bace66ec54f2b359be3c60b472969a448a2',
								  '@type':'http://s/BinaryTree',
								  'http://s/leaf':[]
								},
							'http://s/value':
                            json{ '@type':'http://www.w3.org/2001/XMLSchema#integer',
								  '@value':2
								}
						  }
					},
			    'http://s/value':
                json{ '@type':'http://www.w3.org/2001/XMLSchema#integer',
					  '@value':1
					}
			  }
        },

    run_insert_document(Desc, commit_object{ author : "me", message : "boo"}, JSON, Id),

    open_descriptor(Desc, New_DB),
    get_document(New_DB, Id, Fresh_JSON),

    Fresh_JSON =
    json{ '@id':binary_tree_27afb2c9dc1067ffab4d12f5f1170cf8a419361d,
          '@type':'BinaryTree',
          node:'Node_9e1c81332e7435825b35bd9d9b9936d1c99057fd'
        }.

test(insert_get_delete,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema2(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@type':'Dog',
                name: "Ralph",
                hair_colour: "blue"
               },

    run_insert_document(Desc, commit_object{ author : "me", message : "boo"}, JSON, Id),

    get_document(Desc, Id, _),

    run_delete_document(Desc, commit_object{ author : "me", message : "boo"}, Id),

    \+ get_document(Desc, Id, _).


test(document_update,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema2(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@type':'Dog',
                name: "Ralph",
                hair_colour: "blue"
               },

    run_insert_document(Desc, commit_object{ author : "me", message : "boo"}, JSON, Id),

    New_JSON = json{'@type':'Dog',
                    '@id' : Id,
                    name: "Ralph",
                    hair_colour: "green"
                   },

    run_replace_document(Desc, commit_object{ author : "me", message : "boo"}, New_JSON, Id),

    get_document(Desc, Id, Updated_JSON),

    Updated_JSON = json{'@id':'Dog_Ralph',
                        '@type':'Dog',
                        hair_colour:green,
                        name:"Ralph"}.


test(auto_id_update,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema2(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@type':'Dog',
                name: "Ralph",
                hair_colour: "blue"
               },

    run_insert_document(Desc, commit_object{ author : "me", message : "boo"}, JSON, _Id),

    New_JSON = json{'@type':'Dog',
                    name: "Ralph",
                    hair_colour: "green"
                   },

    run_replace_document(Desc, commit_object{ author : "me", message : "boo"}, New_JSON, Same_Id),

    get_document(Desc, Same_Id, Updated_JSON),

    Updated_JSON = json{'@id':'Dog_Ralph',
                        '@type':'Dog',
                        hair_colour:green,
                        name:"Ralph"}.

test(partial_document_elaborate,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema2(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@id' : 'Dog_Henry',
                '@type':'Dog',
                hair_colour: "blue"
               },

    open_descriptor(Desc, DB),
    json_elaborate(DB,JSON,JSON_ID),

    JSON_ID = json{ '@id':'http://i/Dog_Henry',
                    '@type':'http://s/Dog',
                    'http://s/hair_colour':json{'@id':'http://s/Colour_blue',
                                                '@type':"@id"}
                  }.

test(partial_document_elaborate_list,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema2(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@id' : 'BookClub_Murder%20Mysteries',
                '@type': 'BookClub',
                name : "Murder Mysteries",
                book_list: [ json{ name : "And Then There Were None" },
                             json{ name : "In Cold Blood" }
                           ]
               },

    open_descriptor(Desc, DB),
    json_elaborate(DB,JSON,JSON_ID),

    JSON_ID = json{ '@id':'http://i/BookClub_Murder%20Mysteries',
                    '@type':'http://s/BookClub',
                    'http://s/book_list':
                    _{ '@container':"@array",
			           '@type':'http://s/Book',
			           '@value':
                       [ json{ '@id':'http://i/Book_And%20Then%20There%20Were%20None',
					           '@type':'http://s/Book',
					           'http://s/name':
                               json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
								     '@value':"And Then There Were None"
								   }
					         },
					     json{ '@id':'http://i/Book_In%20Cold%20Blood',
					           '@type':'http://s/Book',
					           'http://s/name':
                               json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
								     '@value':"In Cold Blood"
								   }
					         }
				       ]
			         },
                    'http://s/name':
                    json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
			              '@value':"Murder Mysteries"
			            }
                  }.

test(partial_document_elaborate_list_without_required,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema2(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = json{'@id' : 'BookClub_Murder%20Mysteries',
                '@type': 'BookClub',
                book_list: [ json{ name : "And Then There Were None" },
                             json{ name : "In Cold Blood" }
                           ]
               },

    open_descriptor(Desc, DB),
    json_elaborate(DB,JSON,JSON_ID),

    JSON_ID = json{ '@id':'http://i/BookClub_Murder%20Mysteries',
                    '@type':'http://s/BookClub',
                    'http://s/book_list':
                    _{ '@container':"@array",
			           '@type':'http://s/Book',
			           '@value':
                       [ json{ '@id':'http://i/Book_And%20Then%20There%20Were%20None',
					           '@type':'http://s/Book',
					           'http://s/name':
                               json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
								     '@value':"And Then There Were None"
								   }
					         },
					     json{ '@id':'http://i/Book_In%20Cold%20Blood',
					           '@type':'http://s/Book',
					           'http://s/name':
                               json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
								     '@value':"In Cold Blood"
								   }
					         }
				       ]
			         }
                  }.

test(optional_missing,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema2(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(
             schema_check_failure(
                 [witness{'@type':instance_not_cardinality_one,
                          instance:_,
                          class:'http://www.w3.org/2001/XMLSchema#dateTime',
                          predicate:'http://s/timestamp'}
                 ])
         )
     ]) :-

    JSON = json{ '@type' : "Event",
                 action : "test" },

    run_insert_document(Desc, commit_object{ author : "me",
                                             message : "boo"}, JSON, _Id).


test(extract_schema_person,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema2(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    open_descriptor(Desc, DB),
    get_schema_document(DB, 'Person', JSON),

    JSON = json{'@base':"Person_",
                '@id':'Person',
                '@key':json{'@fields':[name,birthdate],
                            '@type':"Lexical"},
                '@type':'Class',
                birthdate:'xsd:date',
                friends:json{'@class':'Person','@type':"Set"},
                name:'xsd:string'}.

test(extract_schema_employee,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema2(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    open_descriptor(Desc, DB),
    get_schema_document(DB, 'Employee', JSON),
    JSON = json{'@base':"Employee_",
                '@id':'Employee',
                '@inherits':'Person',
                '@key':json{'@fields':[name,birthdate],
                            '@type':"Hash"},
                '@type':'Class',
                boss:json{'@class':'Employee','@type':"Optional"},
                staff_number:'xsd:string',
                tasks:json{'@class':'Task',
                           '@type':"List"}}.

test(extract_schema_colour,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema2(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    open_descriptor(Desc, DB),
    get_schema_document(DB, 'Colour', JSON),
    JSON = json{'@id':'Colour',
                '@type':'Enum',
                '@value':[red,blue,green]}.

test(extract_schema_binary_tree,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema2(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    open_descriptor(Desc, DB),
    get_schema_document(DB, 'BinaryTree', JSON),

    JSON = json{'@base':"binary_tree_",
                '@id':'BinaryTree',
                '@key':json{'@type':"ValueHash"},
                '@type':'TaggedUnion',
                leaf:"Unit",
                node:'Node'}.

test(insert_schema_object,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema2(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    Document =
    _{ '@id' : "Squash",
       '@type' : "Class",
       '@key' : _{ '@type' : "Lexical",
                   '@fields' : ["genus", "species"] },
       genus : "xsd:string",
       species : "xsd:string",
       name : "xsd:string",
       colour : "xsd:string",
       shape : "xsd:string"
     },

    open_descriptor(Desc, DB),
    create_context(DB, _{ author : "me", message : "Have you tried bitcoin?" }, Context),
    with_transaction(
        Context,
        insert_schema_document(Context, Document),
        _
    ),

    open_descriptor(Desc, DB2),
    get_schema_document(DB2, 'Squash', JSON),
    JSON = json{'@id':'Squash',
                '@key':json{'@fields':[genus,species],
                            '@type':"Lexical"},
                '@type':'Class',
                colour:'xsd:string',
                genus:'xsd:string',
                name:'xsd:string',
                shape:'xsd:string',
                species:'xsd:string'}.

test(delete_schema_document,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema2(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    Document =
    _{ '@id' : "Squash",
       '@type' : "Class",
       '@key' : _{ '@type' : "Lexical",
                   '@fields' : ["genus", "species"] },
       genus : "xsd:string",
       species : "xsd:string",
       name : "xsd:string",
       colour : "xsd:string",
       shape : "xsd:string"
     },

    open_descriptor(Desc, DB),
    create_context(DB, _{ author : "me", message : "Have you tried bitcoin?" }, Context),
    with_transaction(
        Context,
        insert_schema_document(Context, Document),
        _
    ),

    open_descriptor(Desc, DB2),
    create_context(DB2,
                   _{ author : "me",
                      message : "Have you tried bitcoin?"
                    },
                   Context2),

    with_transaction(
        Context2,
        delete_schema_document(Context2, 'Squash'),
        _
    ),

    open_descriptor(Desc, DB3),
    \+ get_schema_document(DB3, 'Squash', _).

test(replace_schema_document,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema2(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    Document =
    _{ '@id' : "Squash",
       '@type' : "Class",
       '@key' : _{ '@type' : "Lexical",
                   '@fields' : ["genus", "species"] },
       genus : "xsd:string",
       species : "xsd:string",
       name : "xsd:string",
       colour : "xsd:string",
       shape : "xsd:string"
     },

    open_descriptor(Desc, DB),
    create_context(DB, _{ author : "me", message : "Have you tried bitcoin?" }, Context),
    with_transaction(
        Context,
        insert_schema_document(Context, Document),
        _
    ),

    open_descriptor(Desc, DB2),
    create_context(DB2,
                   _{ author : "me",
                      message : "Have you tried bitcoin?"
                    },
                   Context2),

    New_Document =
    _{ '@id' : "Squash",
       '@type' : "Class",
       '@key' : _{ '@type' : "Hash",
                   '@fields' : ["genus", "species"] },
       genus : "xsd:string",
       species : "xsd:string",
       name : "xsd:string",
       colour : "xsd:string",
       shape : "xsd:string",
       is_a_pumpkin : "xsd:boolean"
     },

    with_transaction(
        Context2,
        replace_schema_document(Context2, New_Document),
        _
    ),

    open_descriptor(Desc, DB3),
    get_schema_document(DB3, 'Squash', JSON),

    JSON =
    json{
        '@id':'Squash',
        '@key':json{'@fields':[genus,species],'@type':"Hash"},
        '@type':'Class',
        colour:'xsd:string',
        genus:'xsd:string',
        is_a_pumpkin:'xsd:boolean',
        name:'xsd:string',
        shape:'xsd:string',
        species:'xsd:string'}.

test(double_insert_schema,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema2(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(
             can_not_insert_existing_object_with_id("Squash")
         )
     ]) :-

    Document =
    _{ '@id' : "Squash",
       '@type' : "Class",
       '@key' : _{ '@type' : "Lexical",
                   '@fields' : ["genus", "species"] },
       genus : "xsd:string",
       species : "xsd:string",
       name : "xsd:string",
       colour : "xsd:string",
       shape : "xsd:string"
     },

    open_descriptor(Desc, DB),
    create_context(DB, _{ author : "me", message : "Have you tried bitcoin?" }, Context),
    with_transaction(
        Context,
        insert_schema_document(Context, Document),
        _
    ),

    open_descriptor(Desc, DB2),
    create_context(DB2,
                   _{ author : "me",
                      message : "Have you tried bitcoin?"
                    },
                   Context2),

    New_Document =
    _{ '@id' : "Squash",
       '@type' : "Class",
       '@key' : _{ '@type' : "Hash",
                   '@fields' : ["genus", "species"] },
       genus : "xsd:string",
       species : "xsd:string",
       name : "xsd:string",
       colour : "xsd:string",
       shape : "xsd:string",
       is_a_pumpkin : "xsd:boolean"
     },

    with_transaction(
        Context2,
        insert_schema_document(Context2, New_Document),
        _
    ).

test(comment_elaborate,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema2(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    Document =
    _{ '@id' : "Squash",
       '@type' : "Class",
       '@key' : _{ '@type' : "Lexical",
                   '@fields' : ["genus", "species"] },
       '@documentation' :
       _{ '@comment' : "Cucurbita is a genus of herbaceous vines in the gourd family, Cucurbitaceae native to the Andes and Mesoamerica.",
          '@properties' : [
              _{ genus : "The genus of the Cucurtiba is always Cucurtiba"},
              _{ species : "There are between 13 and 30 species of Cucurtiba"},
              _{ colour: "Red, Green, Brown, Yellow, lots of things here."},
              _{ shape: "Round, Silly, or very silly!" }
          ]},
       genus : "xsd:string",
       species : "xsd:string",
       name : "xsd:string",
       colour : "xsd:string",
       shape : "xsd:string"
     },

    default_prefixes(Prefixes),
    Context = (Prefixes.put('@schema', 'https://s/')),

    json_schema_elaborate(Document, Context, Elaborated),

    Elaborated =
    json{'@id':'https://s/Squash',
         '@type':'http://terminusdb.com/schema/sys#Class',
         'http://terminusdb.com/schema/sys#documentation':
         json{'http://terminusdb.com/schema/sys#comment':
              json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                   '@value':"Cucurbita is a genus of herbaceous vines in the gourd family, Cucurbitaceae native to the Andes and Mesoamerica."},
              '@id':'https://s/Squash_Documentation',
              'http://terminusdb.com/schema/sys#properties'
              :json{'@container':"@set",
                    '@type':"@id",
                    '@value':[json{'@id':'https://s/Squash_Documentation_PropertyDocumentation_genus','@type':'http://terminusdb.com/schema/sys#PropertyDocumentation',
                                   'https://s/genus':json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                                                          '@value':"The genus of the Cucurtiba is always Cucurtiba"}},
                              json{'@id':'https://s/Squash_Documentation_PropertyDocumentation_species',
                                   '@type':'http://terminusdb.com/schema/sys#PropertyDocumentation',
                                   'https://s/species':json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                                                            '@value':"There are between 13 and 30 species of Cucurtiba"}},
                              json{'@id':'https://s/Squash_Documentation_PropertyDocumentation_colour',
                                   '@type':'http://terminusdb.com/schema/sys#PropertyDocumentation',
                                   'https://s/colour':json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                                                           '@value':"Red, Green, Brown, Yellow, lots of things here."}},
                              json{'@id':'https://s/Squash_Documentation_PropertyDocumentation_shape',
                                   '@type':'http://terminusdb.com/schema/sys#PropertyDocumentation',
                                   'https://s/shape':json{'@type':'http://www.w3.org/2001/XMLSchema#string',
                                                          '@value':"Round, Silly, or very silly!"}}
                             ]},
              '@type':'http://terminusdb.com/schema/sys#Documentation'},
         'http://terminusdb.com/schema/sys#key':
         json{'@id':'https://s/Squash_Lexical_genus_species',
              '@type':'http://terminusdb.com/schema/sys#Lexical',
              'http://terminusdb.com/schema/sys#fields':
              json{'@container':"@list",
                   '@type':"@id",
                   '@value':[json{'@id':'https://s/genus',
                                  '@type':"@id"},
                             json{'@id':'https://s/species',
                                  '@type':"@id"}]}},
         'https://s/colour':json{'@id':'http://www.w3.org/2001/XMLSchema#string',
                                 '@type':"@id"},
         'https://s/genus':json{'@id':'http://www.w3.org/2001/XMLSchema#string',
                                '@type':"@id"},
         'https://s/name':json{'@id':'http://www.w3.org/2001/XMLSchema#string',
                               '@type':"@id"},
         'https://s/shape':json{'@id':'http://www.w3.org/2001/XMLSchema#string',
                                '@type':"@id"},
         'https://s/species':json{'@id':'http://www.w3.org/2001/XMLSchema#string',
                                  '@type':"@id"}},


    open_descriptor(Desc, DB),
    create_context(DB, _{ author : "me", message : "Have you tried bitcoin?" }, Context2),
    with_transaction(
        Context2,
        insert_schema_document(Context2, Document),
        _
    ),

    open_descriptor(Desc, DB2),
    get_schema_document(DB2, 'Squash', New),

    New = json{'@documentation':
               json{'@comment':"Cucurbita is a genus of herbaceous vines in the gourd family, Cucurbitaceae native to the Andes and Mesoamerica.",
                    '@properties':[json{colour:"Red, Green, Brown, Yellow, lots of things here."},
                                   json{genus:"The genus of the Cucurtiba is always Cucurtiba"},
                                   json{shape:"Round, Silly, or very silly!"},
                                   json{species:"There are between 13 and 30 species of Cucurtiba"}]},
               '@id':'Squash',
               '@key':json{'@fields':[genus,species],
                           '@type':"Lexical"},
               '@type':'Class',
               colour:'xsd:string',
               genus:'xsd:string',
               name:'xsd:string',
               shape:'xsd:string',
               species:'xsd:string'}.


test(bad_documentation,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema2(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(
             schema_check_failure([witness{'@type':invalid_property_in_property_documentation_object,class:'http://s/Not_A_Squash',predicate:'http://s/shape',subject:'http://s/Not_A_Squash_Documentation_PropertyDocumentation_shape'}])
         )
     ]) :-

    Document =
    _{ '@id' : "Not_A_Squash",
       '@type' : "Class",
       '@documentation' :
       _{ '@comment' : "Cucurbita is a genus of herbaceous vines in the gourd family, Cucurbitaceae native to the Andes and Mesoamerica.",
          '@properties' : [
              _{ genus : "The genus of the Cucurtiba is always Cucurtiba"},
              _{ shape: "Round, Silly, or very silly!" }
          ]},
       genus : "xsd:string"
     },

    open_descriptor(Desc, DB),
    create_context(DB, _{ author : "me", message : "Have you tried bitcoin?" }, Context2),
    with_transaction(
        Context2,
        insert_schema_document(Context2, Document),
        _
    ).

:- end_tests(json).

:- begin_tests(schema_checker).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).

schema3('
{ "@type" : "@context",
  "@base" : "http://i/",
  "@schema" : "http://s/" }

{ "@id" : "Person",
  "@type" : "Class",
  "@inherits" : "Engineer" }

{ "@id" : "Employee",
  "@type" : "Class",
  "@inherits" : "Person" }

{ "@id" : "Engineer",
  "@type" : "Class",
  "@inherits" : "Employee" }
').

write_schema3(Desc) :-
    create_context(Desc,commit{
                            author : "me",
                            message : "none"},
                   Context),

    schema3(Schema1),

    % Schema
    with_transaction(
        Context,
        write_json_string_to_schema(Context, Schema1),
        _Meta).

test(check_for_cycles_bad,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(
             schema_check_failure(
                 [witness{'@type':cycle_in_class,
                          from_class:'http://s/Employee',
                          path:['http://s/Employee',
                                'http://s/Person',
                                'http://s/Engineer',
                                'http://s/Employee'],
                          to_class:'http://s/Employee'}]),
             _)
     ]) :-

    write_schema3(Desc).

schema4('
{ "@type" : "@context",
  "@base" : "http://i/",
  "@schema" : "http://s/" }

{ "@id" : "Top",
  "@type" : "Class",
  "bottom_face" : { "@type" : "Optional",
                    "@class" : "Bottom" } }

{ "@id" : "Left",
  "@type" : "Class",
  "@inherits" : "Top",
  "thing" : "xsd:string",
  "right_face" : { "@type" : "List",
                   "@class" : "Right" } }

{ "@id" : "Right",
  "@type" : "Class",
  "@inherits" : "Top",
  "thing" : "xsd:string",
  "left_face" : { "@type" : "Set",
                  "@class" : "Left" } }

{ "@id" : "Bottom",
  "@type" : "Class",
  "@inherits" : [ "Right", "Left"],
  "top_face" : { "@type" : "Array",
                 "@class" : "Top" }  }
').

write_schema4(Desc) :-
    create_context(Desc,commit{
                            author : "me",
                            message : "none"},
                   Context),

    schema4(Schema1),

    % Schema
    with_transaction(
        Context,
        write_json_string_to_schema(Context, Schema1),
        _Meta).

test(elaborate_multiple_inheritance, []) :-
    Doc = json{'@id':"Bottom",
               '@inherits':["Right", "Left"],
               '@type':"Class"},
    Ctxt = json{'@base':"http://i/", '@schema':"http://s/"},
    json_schema_elaborate(
        Doc,
        Ctxt,
        Result),

    Result = json{'@id':'http://s/Bottom',
                  '@inherits':
                  json{'@collection':"@set",
                       '@type':"@id",
                       '@value':['http://s/Right','http://s/Left']},
                  '@type':'http://terminusdb.com/schema/sys#Class'},

    findall(t(S,P,O),
            json_triple_(Result,Ctxt,t(S,P,O)),
            Triples),
    Triples = [t('http://s/Bottom',
                 'http://terminusdb.com/schema/sys#inherits',
                 'http://s/Right'),
               t('http://s/Bottom',
                 'http://terminusdb.com/schema/sys#inherits',
                 'http://s/Left'),
               t('http://s/Bottom',
                 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
                 'http://terminusdb.com/schema/sys#Class')].

test(diamond_ok,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    write_schema4(Desc),

    open_descriptor(Desc, Transaction),
    class_frame(Transaction, "Bottom", Frame),

    Frame = json{
                bottom_face:json{'@class':'Bottom',
                                 '@type':"Optional"},
                left_face:json{'@class':'Left','@type':"Set"},
                right_face:json{'@class':'Right','@type':"List"},
                thing:'xsd:string',
                top_face:json{'@class':'Top','@type':"Array"}
            }.

test(extract_bottom,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema4(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    open_descriptor(Desc, DB),
    get_schema_document(DB, 'Bottom', JSON),
    JSON = json{'@id':'Bottom',
                '@inherits':['Left','Right'],
                '@type':'Class',
                top_face:json{'@class':'Top',
                              '@type':"Array"}}.

% NOTE: We need to check diamond properties at schema creation time
schema5('
{ "@type" : "@context",
  "@base" : "http://i/",
  "@schema" : "http://s/" }

{ "@id" : "Top",
  "@type" : "Class",
  "bottom_face" : { "@type" : "Optional",
                    "@class" : "Bottom" } }

{ "@id" : "Left",
  "@type" : "Class",
  "@inherits" : "Top",
  "thing" : "xsd:string",
  "right_face" : { "@type" : "List",
                   "@class" : "Right" } }

{ "@id" : "Right",
  "@type" : "Class",
  "@inherits" : "Top",
  "thing" : "xsd:dateTime",
  "left_face" : { "@type" : "Set",
                  "@class" : "Left" } }

{ "@id" : "Bottom",
  "@type" : "Class",
  "@inherits" : [ "Right", "Left"],
  "top_face" : { "@type" : "Array",
                 "@class" : "Top" } }
').

write_schema5(Desc) :-
    create_context(Desc,commit{
                            author : "me",
                            message : "none"},
                   Context),

    schema5(Schema1),

    % Schema
    with_transaction(
        Context,
        write_json_string_to_schema(Context, Schema1),
        _Meta).

test(diamond_bad,
     [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         error(
             schema_check_failure(
                 [witness{'@type':violation_of_diamond_property,
                          class:'http://s/Bottom',
                          predicate:thing}]),_)
     ]) :-

    write_schema5(Desc).


:- end_tests(schema_checker).


:- begin_tests(woql_document).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).

test(subsumption_insert,
     [
         setup(
             (   setup_temp_store(State),
                 test_woql_label_descriptor(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON = _{'@type' : "Subsumption",
             'child' : _{ '@type' : "NodeValue",
                          'node' : "system:Organization"},
             'parent' : _{'@type' : "NodeValue",
                          'variable' : "Parent"} },

    run_insert_document(Desc, commit_object{ author : "me",
                                             message : "boo"}, JSON, Id),

    get_document(Desc, Id, JSON2),

    JSON2 = json{'@id':'Subsumption_5894f172e060e3787b8045c17099266d55e3ffb1',
                 '@type':'Subsumption',
                 child:'NodeValue_06b4d0fa679ceabdee9afa804863c88676f742f2',
                 parent:'NodeValue_7aa5900abf5ed338b866b67bbcb1983dbda01b54'}.

test(substring_insert, [
         setup(
             (   setup_temp_store(State),
                 test_woql_label_descriptor(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         ),
         blocked('Something wrong with the definition of Substring')
     ]) :-

    JSON = _{'@type' : "Substring",
             string : _{ data : _{'@type' : "xsd:string",
                                  '@value' : "Test"}},
             before : _{ data : _{'@type' : "xsd:integer",
                                  '@value' : 1}},
             length : _{ variable : "Length"},
             after : _{ data : _{'@type' : "xsd:integer",
                                 '@value' : 1}},
             substring : _{ variable : "Substring" }},

    run_insert_document(Desc, commit_object{ author : "me",
                                             message : "boo"}, JSON, Id),

    get_document(Desc, Id, JSON2),
    writeq(JSON2).

:- end_tests(woql_document).

:- begin_tests(arithmetic_document).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).

schema5('
{ "@type" : "@context",
  "@base" : "http://i/",
  "@schema" : "http://s/" }

{ "@id" : "NamedExpression",
  "@type" : "Class",
  "@key" : { "@type" : "Lexical",
             "@fields" : ["name"] },
  "name" : "xsd:string",
  "expression" : "ArithmeticExpression" }

{ "@id" : "ArithmeticExpression",
  "@type" : "Class",
  "@subdocument" : [],
  "@abstract" : [] }

{ "@id": "Plus",
  "@type" : "Class",
  "@inherits" : "ArithmeticExpression",
  "@key" : { "@type" : "ValueHash" },
  "left" : "ArithmeticExpression",
  "right" : "ArithmeticExpression" }

{ "@id" : "Value",
  "@type" : "Class",
  "@inherits" : "ArithmeticExpression",
  "@key" : { "@type" : "ValueHash" },
  "number" : "xsd:integer" }

{ "@id" : "NamedExpression2",
  "@type" : "Class",
  "@key" : { "@type" : "Random" },
  "name" : "xsd:string",
  "expression" : "ArithmeticExpression2" }

{ "@id" : "ArithmeticExpression2",
  "@type" : "Class",
  "@subdocument" : [],
  "@abstract" : [] }

{ "@id": "Plus2",
  "@type" : "Class",
  "@inherits" : "ArithmeticExpression2",
  "@key" : { "@type" : "Random" },
  "left" : "ArithmeticExpression2",
  "right" : "ArithmeticExpression2" }

{ "@id" : "Value2",
  "@type" : "Class",
  "@inherits" : "ArithmeticExpression2",
  "@key" : { "@type" : "Random" },
  "number" : "xsd:integer" }

{ "@id" : "Outer",
  "@type" : "Class",
  "@key" : { "@type" : "Lexical",
             "@fields": ["name"] },
  "name" : "xsd:string",
  "inner" : "Inner",
  "number" : "xsd:integer" }

{ "@id" : "Inner",
  "@type" : "Class",
  "@subdocument" : [],
  "@key" : { "@type" : "Random" },
  "things" : { "@type" : "List",
               "@class" : "xsd:integer" },
  "name" : "xsd:string",
  "number" : "xsd:integer" }

').

write_schema5(Desc) :-
    create_context(Desc,commit{
                            author : "me",
                            message : "none"},
                   Context),

    schema5(Schema1),

    % Schema
    with_transaction(
        Context,
        write_json_string_to_schema(Context, Schema1),
        _Meta).

test(get_value_schema, [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema5(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    open_descriptor(Desc, Trans),
    get_schema_document(Trans, 'Value', Document),

    Document = json{'@id':'Value',
                    '@inherits':'ArithmeticExpression',
                    '@key':json{'@type':"ValueHash"},
                    '@type':'Class',
                    number:'xsd:integer'}.

test(plus_doc_extract, [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema5(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON =
    json{'@type': "NamedExpression",
         name: "3+(2+1)",
         expression:
         json{'@type' : "Plus",
             left : json{'@type' : "Value",
                         number : 3},
             right : json{'@type' : "Plus",
                          left : json{'@type' : "Value",
                                      number : 2},
                          right : json{'@type' : "Value",
                                       number : 1}}
             }
        },

    run_insert_document(Desc, commit_object{ author : "me",
                                             message : "boo"}, JSON, Id),

    get_document(Desc, Id, JSON2),

    JSON2 =
    json{'@id':'NamedExpression_3+(2+1)',
         '@type':'NamedExpression',
         expression:
         json{'@id':'Plus_4fbc40cb94f715fb9f72638b71cbcdbaf9c4755e',
              '@type':'Plus',
              left:json{'@id':'Value_b8c41433b5361b38d6f1995aed6f7e10e168df73',
                        '@type':'Value',
                        number:3},
              right:json{'@id':'Plus_27ff79b26675132195d938869ddb9f3c6ecc7968',
                         '@type':'Plus',
                         left:json{'@id':'Value_61ff2bb5df7172d27b758a2bf7376552798e951d',
                                   '@type':'Value',
                                   number:2},
                         right:json{'@id':'Value_b6bb94b947749d042a0f8cfc020851388d441ea6',
                                    '@type':'Value',
                                    number:1}}},
         name:"3+(2+1)"}.


test(plus_doc_delete, [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema5(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    JSON =
    json{'@type': "NamedExpression2",
         name: "3+(2+1)",
         expression:
         json{'@type' : "Plus2",
             left : json{'@type' : "Value2",
                         number : 3},
             right : json{'@type' : "Plus2",
                          left : json{'@type' : "Value2",
                                      number : 2},
                          right : json{'@type' : "Value2",
                                       number : 1}}
             }
        },

    run_insert_document(Desc, commit_object{ author : "me",
                                             message : "boo"}, JSON, Id),



    get_document(Desc, Id, Document),

    Document =
    json{'@id':_,
         '@type':'NamedExpression2',
         expression:json{'@id':_,
                         '@type':'Plus2',
                         left:json{'@id':_,
                                   '@type':'Value2',
                                   number:3},
                         right:json{'@id':_,
                                    '@type':'Plus2',
                                    left:json{'@id':_,
                                              '@type':'Value2',
                                              number:2},
                                    right:json{'@id':_,
                                               '@type':'Value2',
                                               number:1}}},
         name:"3+(2+1)"},


    run_delete_document(Desc, commit_object{ author : "me",
                                             message : "boo"}, Id),

    \+ get_document_uri(Desc, Id).

test(subdocument_deletes_lists, [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema5(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-


    JSON =
    json{'@type': "Outer",
         name: "Outer",
         inner:
         json{'@type' : "Inner",
              name: "Inner",
              number: 10,
              things: [1,2,3]},
         number: 1},

    run_insert_document(Desc, commit_object{ author : "me",
                                             message : "boo"}, JSON, Id),
    get_document(Desc, Id, JSON2),

    JSON2 = json{'@id':'Outer_Outer',
                 '@type':'Outer',
                 inner:json{'@id':_,
                            '@type':'Inner',
                            name:"Inner",
                            number:10,
                            things:[1,2,3]},
                 name:"Outer",
                 number:1},

    run_delete_document(Desc, commit_object{ author : "me",
                                             message : "boo"}, Id),

    open_descriptor(Desc, Trans),
    database_instance(Trans, Inst),
    \+ xrdf(Inst, _, _, _).

test(subdocument_deletes_lists, [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema5(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-


    JSON =
    json{'@type': "Outer",
         name: "Outer",
         inner:
         json{'@type' : "Inner",
              name: "Inner",
              number: 10,
              things: [1,2,3]},
         number: 1},

    run_insert_document(Desc, commit_object{ author : "me",
                                             message : "boo"}, JSON, Id),
    get_document(Desc, Id, JSON2),

    JSON2 = json{'@id':'Outer_Outer',
                 '@type':'Outer',
                 inner:json{'@id':_,
                            '@type':'Inner',
                            name:"Inner",
                            number:10,
                            things:[1,2,3]},
                 name:"Outer",
                 number:1},

    run_delete_document(Desc, commit_object{ author : "me",
                                             message : "boo"}, Id),

    open_descriptor(Desc, Trans),
    database_instance(Trans, Inst),
    \+ xrdf(Inst, _, _, _).

:- end_tests(arithmetic_document).
