:- module('document/json', [
              context_triple/2,
              json_elaborate/3,
              json_triple/4,
              json_schema_triple/2,
              json_schema_triple/3,
              json_schema_elaborate/2,
              id_json/3,
              database_context/2,
              create_graph_from_json/5,
              write_json_stream_to_builder/3,
              write_json_stream_to_schema/2,
              write_json_stream_to_instance/2,
              write_json_string_to_schema/2,
              write_json_string_to_instance/2
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

value_json(X,O) :-
    O = json{
            '@type': "@id",
            '@id': X
        },
    string(X),
    !.
value_json(1^^unit,json{}) :-
    !.
value_json(X^^Y,O) :-
    O = json{
            '@type': Y,
            '@value': X
        },
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
get_value(Elaborated, _) :-
    get_dict('@type', Elaborated, "@id"),
    !,
    throw(error(no_hash_possible_over_ids(Elaborated))).
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

get_field_values(JSON,Fields,Values) :-
    findall(
        Value,
        (   member(Field,Fields),
            (   get_dict(Field,JSON,Value)
            ->  true
            ;   throw(error(missing_key(Field,JSON),_))
            )
        ),
        Values).

raw(JValue,Value) :- get_dict('@value',JValue,Value).

idgen_lexical(Base,Values,ID) :-
    maplist(raw,Values,Raw),
    maplist(uri_encoded(path),Raw,Encoded),
    merge_separator_split(Suffix, '_', Encoded),
    format(string(ID), '~w~w', [Base,Suffix]).

idgen_hash(Base,Values,ID) :-
    maplist(raw,Values,Raw),
    maplist(uri_encoded(path),Raw,Encoded),
    merge_separator_split(String, '_', Encoded),
    md5_hash(String,Hash,[]),
    format(string(ID), "~w~w", [Base,Hash]).

idgen_path_values_hash(Base,Path,ID) :-
    format(string(A), '~q', [Path]),
    md5_hash(A,Hash,[]),
    format(string(ID), "~w~w", [Base,Hash]).

idgen_random(Base,ID) :-
    random(X),
    format(string(S), '~w', [X]),
    md5_hash(S,Hash,[]),
    format(string(ID),'~w~w',[Base,Hash]).

json_idgen(DB,JSON,ID) :-
    get_dict('@type',JSON,Type),
    key_descriptor(DB,Type,Descriptor),
    (   Descriptor = lexical(Base,Fields)
    ->  get_field_values(JSON, Fields, Values),
        idgen_lexical(Base,Values,ID)
    ;   Descriptor = hash(Base,Fields),
        get_field_values(JSON, Fields, Values),
        idgen_hash(Base,Values,ID)
    ;   Descriptor = value_hash(Base)
    ->  get_all_path_values(JSON,Path_Values),
        idgen_path_values_hash(Base,Path_Values,ID)
    ;   Descriptor = random(Base)
    ->  idgen_random(Base,ID)
    ).

class_descriptor_image(class(_),json{ '@type' : "@id" }).
class_descriptor_image(optional(_),json{ '@type' : "@id" }).
class_descriptor_image(tagged_union(_),json{ '@type' : "@id" }).
class_descriptor_image(base_class(C),json{ '@type' : C }).
class_descriptor_image(enum(C),json{ '@type' : C }).
class_descriptor_image(list(C),json{ '@container' : "@list",
                                     '@type' : C }).
class_descriptor_image(array(C),json{ '@container' : "@array",
                                      '@type' : C }).
class_descriptor_image(set(C),json{ '@container' : "@set",
                                    '@type' : C }).
class_descriptor_image(cardinality(C,_), json{ '@container' : "@cardinality",
                                               '@type' : C }).

database_context(DB,Context) :-
    database_schema(DB,Schema),
    (   xrdf(Schema, ID, rdf:type, sys:'Context')
    ->  id_schema_json(DB,ID,Context)
    ;   Context = _{}).

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
    maybe_expand_type(Type,Database_Context,TypeEx),
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
    dict_create(Context,json,Edges).

json_elaborate(DB,JSON,JSON_ID) :-
    database_context(DB,Context),
    json_elaborate(DB,JSON,Context,JSON_ID).

maybe_expand_type(Type,Context,TypeEx) :-
    get_dict('@schema', Context, Schema),
    put_dict(_{'@base' : Schema}, Context, New_Context),
    prefix_expand(Type, New_Context, TypeEx).

json_elaborate(DB,JSON,Context,JSON_ID) :-
    is_dict(JSON),
    !,
    get_dict('@type',JSON,Type),
    maybe_expand_type(Type,Context,TypeEx),
    do_or_die(
        type_context(DB,TypeEx,Type_Context),
        error(unknown_type_encountered(TypeEx),_)),
    %
    put_dict(Type_Context,Context,New_Context),
    json_context_elaborate(DB,JSON,New_Context,Elaborated),
    json_jsonid(DB,Elaborated,JSON_ID).

expansion_key(Key,Expansion,Prop,Cleaned) :-
    (   select_dict(json{'@id' : Prop}, Expansion, Cleaned)
    ->  true
    ;   Key = Prop,
        Expansion = Cleaned
    ).

context_value_expand(DB,Value,Expansion,V) :-
    get_dict('@container', Expansion, _),
    !,
    % Container type
    get_dict('@type', Expansion, Elt_Type),
    (   is_list(Value)
    ->  Value_List = Value
    ;   string(Value)
    ->  Value_List = [Value]
    ;   get_dict('@value',Value,Value_List)),
    maplist({DB,Elt_Type}/[Elt_In,Elt_Out]>>(
                (   is_enum(DB,Elt_Type)
                ->  enum_value(Elt_Type,Elt_In,Elt_Out)
                ;   is_dict(Elt_In)
                ->  put_dict(Elt_In,json{'@type':Elt_Type},Elt2)
                ;   is_base_type(Elt_Type)
                ->  Elt2 = json{'@value' : Elt_In,
                                '@type': Elt_Type}
                ;   Elt2 = json{'@id' : Elt_In,
                                '@type': "@id"}),
                json_elaborate(DB,Elt2,Elt_Out)
            ), Value_List, V_List),
    V = (Expansion.put(json{'@value' : V_List})).
context_value_expand(DB,Value,Expansion,V) :-
    % A possible reference
    get_dict('@type', Expansion, "@id"),
    !,
    is_dict(Value),
    json_elaborate(DB,Value, V).
context_value_expand(_,Value,_Expansion,V) :-
    % An already expanded typed value
    is_dict(Value),
    get_dict('@value',Value,_),
    !,
    V = Value.
context_value_expand(DB,Value,Expansion,V) :-
    % An unexpanded typed value
    New_Expansion = (Expansion.put(json{'@value' : Value})),
    json_elaborate(DB,New_Expansion, V).

enum_value(Type,Value,ID) :-
    atomic_list_concat([Type, '_', Value], ID).

json_context_elaborate(DB, JSON, _Context, Expanded) :-
    is_dict(JSON),
    get_dict('@type',JSON,Type),
    is_enum(DB,Type),
    !,
    get_dict('@value',JSON,Value),
    enum_value(Type,Value,Full_ID),
    Expanded = json{ '@type' : "@id",
                     '@id' : Full_ID }.
json_context_elaborate(DB, JSON, Context, Expanded) :-
    is_dict(JSON),
    !,
    dict_pairs(JSON,json,Pairs),
    findall(
        P-V,
        (   member(Prop-Value,Pairs),
            (   get_dict(Prop, Context, Full_Expansion),
                is_dict(Full_Expansion)
            ->  expansion_key(Prop,Full_Expansion,P,Expansion),
                context_value_expand(DB,Value,Expansion,V)
            ;   Prop = '@type'
            ->  P = Prop,
                maybe_expand_type(Value,Context, V)
            ;   P = Prop,
                V = Value
            )
        ),
        PVs),
    dict_pairs(Expanded,json,PVs).

json_jsonid(DB,JSON,JSON_ID) :-
    % Set up the ID
    (   get_dict('@id',JSON,_)
    ->  JSON_ID = JSON
    ;   get_dict('@container', JSON, _)
    ->  JSON_ID = JSON
    ;   get_dict('@value', JSON, _)
    ->  JSON_ID = JSON
    ;   json_idgen(DB,JSON,ID)
    ->  JSON_ID = (JSON.put(json{'@id' : ID}))
    ;   throw(error(no_id(JSON),_))
    ).


json_prefix_access(JSON,Edge,Type) :-
    global_prefix_expand(Edge,Expanded),
    get_dict(Expanded,JSON,Type).

json_type(JSON,_Context,Type) :-
    json_prefix_access(JSON,rdf:type,Type).

json_schema_elaborate(JSON,Elaborated) :-
    json_schema_elaborate(JSON,[],Elaborated).

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
    json_triple_(Expanded,Triple).

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

expand_match_system(Key,Term,Key_Ex) :-
    global_prefixes(sys,Prefix),
    global_prefix_expand(sys:Term,Key_Ex),
    prefix_expand(Key, _{'@base' : Prefix}, Key_Ex).

json_schema_elaborate_key(V,json{'@type':Value}) :-
    atom(V),
    !,
    global_prefixes(sys,Prefix),
    prefix_expand(V, _{'@base' : Prefix}, Value).
json_schema_elaborate_key(V,Value) :-
    get_dict('@type', V, Lexical),
    expand_match_system(Lexical, 'Lexical', Type),
    !,
    get_dict('@fields', V, Fields),
    Value = json{
                '@type' : Type,
                'sys:fields' :
                json{
                    '@container' : "@list",
                    '@type' : "@id",
                    '@value' : Fields
                }
            }.
json_schema_elaborate_key(V,Value) :-
    get_dict('@type', V, Hash),
    expand_match_system(Hash, 'Hash', Type),
    !,
    get_dict('@fields', V, Fields),
    Value = json{
                '@type' : Type,
                'sys:fields' :
                json{
                    '@container' : "@list",
                    '@type' : "@id",
                    '@value' : Fields
                }
            }.
json_schema_elaborate_key(V,json{ '@type' : Type}) :-
    get_dict('@type', V, ValueHash),
    expand_match_system(ValueHash, 'ValueHash', Type),
    !.
json_schema_elaborate_key(V,json{ '@type' : Type}) :-
    get_dict('@type', V, Random),
    expand_match_system(Random, 'Random', Type),
    !.

json_schema_predicate_value('@id',V,_,'@id',V) :-
    !.
json_schema_predicate_value(P,V,_,P,json{'@type' : 'xsd:positiveInteger',
                                         '@value' : V }) :-
    global_prefix_expand(sys:cardinality, P),
    !.
json_schema_predicate_value('@key',V,_,P,Value) :-
    !,
    global_prefix_expand(sys:key, P),
    json_schema_elaborate_key(V,Value).
json_schema_predicate_value('@base',V,_,P,Value) :-
    !,
    global_prefix_expand(sys:base, P),
    (   is_dict(V)
    ->  Value = V
    ;   global_prefix_expand(xsd:string, XSD),
        Value = json{ '@type' : XSD,
                      '@value' : V }
    ).
json_schema_predicate_value('@type',V,_,'@type',Value) :-
    !,
    maybe_expand_schema_type(V,Value).
json_schema_predicate_value('@class',V,_,Class,json{'@type' : "@id",
                                                    '@id' : V}) :-
    !,
    global_prefix_expand(sys:class, Class).
json_schema_predicate_value(P,V,Path,P,Value) :-
    is_dict(V),
    !,
    json_schema_elaborate(V, [P|Path], Value).
json_schema_predicate_value(P,V,_,P,json{'@type' : "@id",
                                         '@id' : V }).

json_schema_elaborate(JSON,_,Elaborated) :-
    is_type_enum(JSON),
    !,
    %writeq(JSON),nl,
    get_dict('@id', JSON, ID),
    get_dict('@type', JSON, Type),
    maybe_expand_schema_type(Type,Expanded),
    get_dict('@value', JSON, List),
    maplist({ID}/[Elt,json{'@type' : "@id",
                           '@id' : V}]>>(
                format(string(V),'~w_~w',[ID,Elt])
            ),List,New_List),
    Elaborated = json{ '@id' : ID,
                       '@type' : Expanded,
                       'sys:value' : json{ '@container' : "@set",
                                           '@type' : "@id",
                                           '@value' : New_List } }.
json_schema_elaborate(JSON,Old_Path,Elaborated) :-
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
            json_schema_predicate_value(P,V,Path,Prop,Value)
        ),
        PVs),
    dict_pairs(Elaborated,json,PVs).

expand_schema(JSON,Prefixes,Expanded) :-
    get_dict('@schema', Prefixes, Schema),
    put_dict(_{'@base' : Schema}, Prefixes, Schema_Prefixes),
    expand(JSON,Schema_Prefixes,Expanded).

json_schema_triple(JSON,Context,Triple) :-
    json_schema_elaborate(JSON,JSON_Schema),
    expand_schema(JSON_Schema,Context,Expanded),
    json_triple_(Expanded,Triple).

json_schema_triple(JSON,Triple) :-
    json_schema_elaborate(JSON,JSON_Schema),
    json_triple_(JSON_Schema,Triple).

% Triple generator
json_triple(DB,JSON,Context,Triple) :-
    json_elaborate(DB,JSON,Elaborated),
    expand(Elaborated,Context,Expanded),
    json_triple_(Expanded,Triple).

json_triples(DB,JSON,Context,Triples) :-
    findall(
        Triple,
        json_triple(DB, JSON, Context, Triple),
        Triples).

json_triple_(JSON,_Triple) :-
    is_dict(JSON),
    get_dict('@value', JSON, _),
    \+ get_dict('@container', JSON, _),
    !,
    fail.
json_triple_(JSON,Triple) :-
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
            ->  get_dict('@id', Class, Inherited)
            ;   Inherited = Class),
            Triple = t(ID,SYS_Inherits,Inherited)
        ;   get_dict('@id', Value, Inherited)
        ->  Triple = t(ID,SYS_Inherits,Inherited))
    ;   (   get_dict('@id', Value, Value_ID)
        ->  (   json_triple_(Value, Triple)
            ;   Triple = t(ID,Key,Value_ID)
            )
        ;   get_dict('@container', Value, "@list")
        ->  get_dict('@value', Value, List),
            list_id_key_triple(List,ID,Key,Triple)
        ;   get_dict('@container', Value, "@array")
        ->  get_dict('@value', Value, Array),
            array_id_key_triple(Array,ID,Key,Triple)
        ;   get_dict('@container', Value, "@set")
        ->  get_dict('@value', Value, Set),
            set_id_key_triple(Set,ID,Key,Triple)
        ;   value_json(Lit,Value),
            Triple = t(ID,Key,Lit)
        )
    ).

array_id_key_triple(List,ID,Key,Triple) :-
    array_index_id_key_triple(List,0,ID,Key,Triple).

array_index_id_key_triple([H|T],Index,ID,Key,Triple) :-
    idgen_random('Array_',New_ID),
    reference(H,HRef),
    global_prefix_expand(sys:value, SYS_Value),
    global_prefix_expand(sys:value, SYS_Index),
    (   Triple = t(ID, Key, New_ID)
    ;   Triple = t(New_ID, SYS_Value, HRef)
    ;   Triple = t(New_ID, SYS_Index, Index)
    ;   Next_Index is Index + 1,
        array_index_id_key_triple(T,Next_Index,ID,Key,Triple)
    ;   json_triple_(H,Triple)
    ).

set_id_key_triple([H|T],ID,Key,Triple) :-
    (   reference(H,HRef),
        Triple = t(ID,Key,HRef)
    ;   set_id_key_triple(T,ID,Key,Triple)
    ;   json_triple_(H,Triple)
    ).

reference(Dict,ID) :-
    get_dict('@id',Dict, ID),
    !.
reference(Elt,V) :-
    value_json(V,Elt).

list_id_key_triple([],ID,Key,t(ID,Key,rdf:nil)).
list_id_key_triple([H|T],ID,Key,Triple) :-
    idgen_random('Cons_',New_ID),
    (   reference(H,HRef),
        member(Triple,[t(ID,Key,New_ID),
                       t(New_ID,rdf:first,HRef)])
    ;   list_id_key_triple(T,New_ID,rdf:rest,Triple)
    ;   json_triple_(H,Triple)
    ).

rdf_list_list(_Graph, RDF_Nil,[]) :-
    global_prefix_expand(rdf:nil,RDF_Nil),
    !.
rdf_list_list(Graph, Cons,[H|L]) :-
    xrdf(Graph, Cons, rdf:first, H),
    xrdf(Graph, Cons, rdf:rest, Tail),
    rdf_list_list(Graph,Tail,L).

array_list(DB,Id,P,List) :-
    database_instance(DB,Instance),
    findall(
        I-V,
        (   xrdf(Instance,Id,P,ArrayElement),
            xrdf(Instance,ArrayElement,sys:value,V),
            xrdf(Instance,ArrayElement,sys:index,I)
        ),
        Index_List),
    index_list_array(Index_List,List).

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
    ->  throw(error(index_on_array_too_large(J),_))
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
    list_type_id_predicate_iri_value(T,C,Id,P,DB,Prefixes,L).

type_id_predicate_iri_value(enum(C),_,_,V^^C,_,_,O) :-
    merge_separator_split(V, '_', [C,O]).
type_id_predicate_iri_value(list(C),Id,P,O,DB,Prefixes,L) :-
    % Probably need to treat enums...
    database_instance(DB,Instance),
    rdf_list_list(Instance,O,V),
    type_descriptor(DB,C,Desc),
    list_type_id_predicate_value(V,Desc,Id,P,DB,Prefixes,L).
type_id_predicate_iri_value(array(C),Id,P,_,DB,Prefixes,L) :-
    array_list(DB,Prefixes,Id,P,V),
    type_descriptor(DB,C,Desc),
    list_type_id_predicate_value(V,Desc,Id,P,DB,Prefixes,L).
type_id_predicate_iri_value(set(C),Id,P,_,DB,Prefixes,L) :-
    set_list(DB,Prefixes,Id,P,V),
    type_descriptor(DB,C,Desc),
    list_type_id_predicate_value(V,Desc,Id,P,DB,Prefixes,L).
type_id_predicate_iri_value(cardinality(C,_),Id,P,_,DB,_,L) :-
    set_list(DB,Prefixes,Id,P,V),
    type_descriptor(DB,C,Desc),
    list_type_id_predicate_value(V,Desc,Id,P,DB,Prefixes,L).
type_id_predicate_iri_value(class(_),_,_,Id,_,Prefixes,Id_Comp) :-
    compress_dict_uri(Id, Prefixes, Id_Comp).
type_id_predicate_iri_value(tagged_union(_),_,_,V,_,_,V).
type_id_predicate_iri_value(optional(C),Id,P,O,DB,Prefixes,V) :-
    type_descriptor(DB,C,Desc),
    type_id_predicate_iri_value(Desc,Id,P,O,DB,Prefixes,V).
type_id_predicate_iri_value(base_class(_),_,_,O,_,_,S) :-
    typecast(O,'http://www.w3.org/2001/XMLSchema#string', [], S^^_).

compress_schema_uri(IRI,Prefixes,IRI_Comp) :-
    get_dict('@schema',Prefixes,Schema),
    put_dict(_{'@base' : Schema}, Prefixes, Schema_Prefixes),
    compress_dict_uri(IRI,Schema_Prefixes,IRI_Comp).

id_json(DB, Id, JSON) :-
    database_context(DB,Prefixes),
    database_instance(DB,Instance),

    prefix_expand(Id,Prefixes,Id_Ex),
    xrdf(Instance, Id_Ex, rdf:type, Class),
    findall(
        Prop-Value,
        (   distinct([P],xrdf(Instance,Id_Ex,P,O)),
            \+ is_built_in(P),

            once(class_predicate_type(DB,Class,P,Type)),
            type_id_predicate_iri_value(Type,Id_Ex,P,O,DB,Prefixes,Value),

            compress_schema_uri(P, Prefixes, Prop)
        ),
        Data),
    !,
    compress_dict_uri(Id_Ex, Prefixes, Id_comp),
    compress_schema_uri(Class, Prefixes, Class_comp),
    dict_create(JSON,json,['@id'-Id_comp,
                           '@type'-Class_comp
                           |Data]).

key_descriptor_json(lexical(_, Fields), json{ '@type' : "Lexical",
                                              '@fields' : Fields }).
key_descriptor_json(hash(_, Fields), json{ '@type' : "Hash",
                                           '@fields' : Fields }).
key_descriptor_json(value_hash(_), json{ '@type' : "ValueHash" }).
key_descriptor_json(random(_), json{ '@type' : "Random" }).

type_descriptor_json(unit, 'Unit').
type_descriptor_json(class(C), C).
type_descriptor_json(optional(C), json{ '@type' : "Optional",
                                        '@class' : C }).
type_descriptor_json(set(C), json{ '@type' : "Set",
                                   '@class' : C }).
type_descriptor_json(array(C), json{ '@type' : "Array",
                                   '@class' : C }).
type_descriptor_json(list(C), json{ '@type' : "List",
                                    '@class' : C }).
type_descriptor_json(tagged_union(C), C).
type_descriptor_json(enum(C), C).

schema_subject_predicate_object_key_value(_,_Id,P,O^^_,'@base',O) :-
    global_prefix_expand(sys:base,P),
    !.
schema_subject_predicate_object_key_value(_,_Id,P,O^^_,'@schema',O) :-
    global_prefix_expand(sys:schema,P),
    !.
schema_subject_predicate_object_key_value(_,_Id,P,O,'@class',O) :-
    global_prefix_expand(sys:class,P),
    !.
schema_subject_predicate_object_key_value(DB,_Id,P,O,'@value',L) :-
    global_prefix_expand(sys:value,P),
    !,
    database_schema(DB,Schema),
    rdf_list_list(Schema, O, L).
schema_subject_predicate_object_key_value(DB,_Id,P,O,'@key',V) :-
    global_prefix_expand(sys:key,P),
    !,
    key_descriptor(DB, O, Key),
    key_descriptor_json(Key,V).
schema_subject_predicate_object_key_value(DB,_Id,P,O,P,JSON) :-
    type_descriptor(DB, O, Descriptor),
    type_descriptor_json(Descriptor,JSON).

id_schema_json(DB, Id, JSON) :-
    database_schema(DB,Schema),
    xrdf(Schema, Id, rdf:type, Class),

    findall(
        K-V,
        (   distinct([P],xrdf(Schema,Id,P,O)),
            schema_subject_predicate_object_key_value(DB,Id,P,O,K,V)
        ),
        Data),
    !,
    dict_create(JSON,json,['@id'-Id,
                           '@type'-Class
                           |Data]).

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
    nb_set_head(Graph_Obj, Layer).

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
    database_context(DB,Context),
    default_prefixes(Prefixes),

    put_dict(Context,Prefixes,Expanded_Context),

    forall(
        json_read_dict_stream(JSON_Stream, Dict),
        (
            forall(
                json_triple(DB,Dict,Expanded_Context,t(S,P,O)),
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


insert_document(Desc, Document, ID) :-
    create_context(Desc,commit{
                            author : "test",
                            message : "none"},
                   Context),

    with_transaction(
        Context,
        (   [TO] = (Context.transaction_objects),
            database_context(TO, Prefixes),
            % Pre process document
            json_elaborate(TO, Document, Elaborated),
            expand(Elaborated, Prefixes, Expanded),
            get_dict('@id', Expanded, ID),
            [RWO] = (TO.instance_objects),
            % insert
            forall(
                json_triple_(Expanded, t(S,P,O)),
                (   O = D^^T
                ->  typecast(D^^'http://www.w3.org/2001/XMLSchema#string',
                             T, [],
                             OC),
                    insert(RWO, S, P, OC, _)
                ;   insert(RWO, S, P, O, _)
                )
            )
        ),
        _
    ).

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

    open_string(
    '{ "@type" : "@context",
       "@base" : "http://terminusdb.com/system/schema#",
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
        t("http://terminusdb.com/system/schema#User","http://terminusdb.com/system/schema#capability",node("http://terminusdb.com/system/schema#User_capability_Set_Capability")),
        t("http://terminusdb.com/system/schema#User","http://terminusdb.com/system/schema#key_hash",node("http://terminusdb.com/type#string")),
        t("http://terminusdb.com/system/schema#User","http://www.w3.org/1999/02/22-rdf-syntax-ns#type",node("http://terminusdb.com/schema/sys#Class")),
        t("http://terminusdb.com/system/schema#User_capability_Set_Capability","http://terminusdb.com/schema/sys#class",node("http://terminusdb.com/system/schema#Capability")),
        t("http://terminusdb.com/system/schema#User_capability_Set_Capability","http://www.w3.org/1999/02/22-rdf-syntax-ns#type",node("http://terminusdb.com/schema/sys#Set")),

        t("terminusdb://Prefix_Pair_5450b0648f2f15c2864f8853747d484b","http://terminusdb.com/schema/sys#prefix",value("\"type\"^^'http://www.w3.org/2001/XMLSchema#string'")),
        t("terminusdb://Prefix_Pair_5450b0648f2f15c2864f8853747d484b","http://terminusdb.com/schema/sys#url",value("\"http://terminusdb.com/type#\"^^'http://www.w3.org/2001/XMLSchema#string'")),
        t("terminusdb://Prefix_Pair_5450b0648f2f15c2864f8853747d484b","http://www.w3.org/1999/02/22-rdf-syntax-ns#type",node("http://terminusdb.com/schema/sys#Prefix")),
        t("terminusdb://context","http://terminusdb.com/schema/sys#base",value("\"http://terminusdb.com/system/schema#\"^^'http://www.w3.org/2001/XMLSchema#string'")),
        t("terminusdb://context","http://terminusdb.com/schema/sys#prefix_pair",node("terminusdb://Prefix_Pair_5450b0648f2f15c2864f8853747d484b")),
        t("terminusdb://context","http://www.w3.org/1999/02/22-rdf-syntax-ns#type",node("http://terminusdb.com/schema/sys#Context"))
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
                "@class" : "xsd:string" } }').

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

    print_term(Elaborated,[]),


    Elaborated = json{ '@id':gavin,
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
        '@id':gavin,
        '@type':'http://s/Employee',
        'http://s/birthdate':json{ '@type':'http://www.w3.org/2001/XMLSchema#date',
				                   '@value':"1977-05-24"
			                     },
        'http://s/boss':json{ '@id':jane,
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
    database_context(DB, Context),
    json_triples(DB, Document, Context, Triples),

    sort(Triples, Sorted),

    Sorted = [
        t('http://i/gavin',
          'http://s/birthdate',
          "1977-05-24"^^'http://www.w3.org/2001/XMLSchema#date'),
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
          "1979-12-28"^^'http://www.w3.org/2001/XMLSchema#date'),
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
                   boss : json{
                              '@id' : jane,
                              '@type' : 'Employee',
                              name : "jane",
                              staff_number : "12",
                              birthdate : "1979-12-28"
                          }
               },

    insert_document(Desc, Document, ID),

    open_descriptor(Desc, DB),
    !, % NOTE: why does rolling back over this go mental?

    id_json(DB,ID,JSON1),
    !,
    JSON1 = json{'@id':gavin,
                 '@type':'Employee',
                 birthdate:"1977-05-24",
                 boss:jane,
                 name:"gavin",
                 staff_number:"13"},

    id_json(DB,jane,JSON2),
    !,
    JSON2 = json{ '@id':jane,
                  '@type':'Employee',
                  birthdate:"1979-12-28",
                  name:"jane",
                  staff_number:"12"
                }.

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
              ['http://s/birthdate']-("1979-12-28"^^'http://www.w3.org/2001/XMLSchema#date'),
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
             "@fields" : [ "name", "birthday" ] },
  "name" : "xsd:string",
  "birthday" : "xsd:date",
  "friends" : { "@type" : "Set",
                "@class" : "Person" } }

{ "@id" : "Employee",
  "@type" : "Class",
  "@base" : "Employee_",
  "@key" : { "@type" : "Hash",
             "@fields" : [ "name", "birthday" ] },
  "@inherts" : "Person",
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
  "@base" : "dog_",
  "@key" : { "@type" : "Lexical",
             "@fields" : [ "name" ] },
  "name" : "xsd:string",
  "hair_colour" : "Colour" }

{ "@id" : "BinaryTree",
  "@class" : "TaggedUnion",
  "@base" : "binary_tree_",
  "@key" : { "@type" : "ValueHash" },
  "leaf" : "Unit",
  "node" : "Node" }

{ "@id" : "Node",
  "@type" : "Class",
  "@key" : { "@type" : "ValueHash" },
  "value" : "xsd:integer",
  "left" : "BinaryTree",
  "right" : "BinaryTree" }').

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
    json_schema_elaborate(Doc, Elaborate),

    Elaborate = json{ '@id':"Capability",
                      '@type':'http://terminusdb.com/schema/sys#Class',
                      'http://terminusdb.com/schema/sys#key':json{
                                                                 '@type':'http://terminusdb.com/schema/sys#ValueHash'
						                                     },
                      role:json{ '@id':'Capability_role_Set_Role',
		                         '@type':'http://terminusdb.com/schema/sys#Set',
		                         'http://terminusdb.com/schema/sys#class':json{ '@id':"Role",
								                                                '@type':"@id"
							                                                  }
	                           },
                      scope:json{'@id':"Resource",'@type':"@id"}
                    }.

test(schema_lexical_key_elaboration, []) :-
    Doc = json{ '@id' : "Person",
                '@type' : "Class",
                '@base' : "Person_",
                '@key' : json{ '@type' : "Lexical",
                               '@fields' : [ "name", "birthday" ] },
                'name' : "xsd:string",
                'birthday' : "xsd:date",
                'friends' : json{ '@type' : "Set",
                                  '@class' : "Person" } },

    json_schema_elaborate(Doc, Elaborate),

    print_term(Elaborate, []),

    Elaborate =
    json{ '@id':"Person",
          '@type':'http://terminusdb.com/schema/sys#Class',
          birthday:json{'@id':"xsd:date",'@type':"@id"},
          friends:json{ '@id':'Person_friends_Set_Person',
		                '@type':'http://terminusdb.com/schema/sys#Set',
		                'http://terminusdb.com/schema/sys#class':
                        json{ '@id':"Person",
							  '@type':"@id"
							}
		              },
          'http://terminusdb.com/schema/sys#base':
          json{ '@type':'http://www.w3.org/2001/XMLSchema#string',
				'@value':"Person_"
			  },
          'http://terminusdb.com/schema/sys#key':
          json{ '@type':'http://terminusdb.com/schema/sys#Lexical',
				'sys:fields':json{ '@container':"@list",
								   '@type':"@id",
								   '@value':[ "name",
										      "birthday"
									        ]
								 }
			  },
          name:json{'@id':"xsd:string",'@type':"@id"}
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

    print_all_triples(DB,schema),

    json_elaborate(DB, JSON, Elaborated),

    Elaborated = json{'@id':"Person_jane_1979-12-28",
                      '@type':'Person',
                      birthdate:json{'@type':xsd:date,
                                     '@value':"1979-12-28"},
                      name:json{'@type':xsd:string,
                                '@value':"jane"}
                     }.

test(idgen_hash,
     [
         setup(
             (   delete_database,
                 create_database,

             % Schema
                 forall(schema2(A,B,C),
                        insert_triple(s(A,B,C))),

                 check_and_commit
             )
         ),
         cleanup(
             (
                 delete_database
             )
         )
     ]) :-

    JSON = json{'@type':employee,
                birthdate:"1979-12-28",
                name:"jane",
                staff_number:"13"
               },

    json_elaborate(JSON, Elaborated),

    Elaborated = json{'@id':"employee_b367edeea1a0e899b55a88edf9b27513",
                      '@type':employee,
                      birthdate:json{'@type':xsd:date,
                                     '@value':"1979-12-28"},
                      name:json{'@type':xsd:string,
                                '@value':"jane"},
                      staff_number:json{'@type':xsd:string,
                                        '@value':"13"}
                     }.

test(idgen_value_hash,
     [
         setup(
             (   delete_database,
                 create_database,

             % Schema
                 forall(schema2(A,B,C),
                        insert_triple(s(A,B,C))),

                 check_and_commit
             )
         ),
         cleanup(
             (
                 delete_database
             )
         )
     ]) :-

    JSON = json{'@type':task,
                name:"Groceries"},

    json_elaborate(JSON, Elaborated),

    Elaborated = json{'@id':"task_6e39e0ff19d797fd2073c41f8f54242b",
                      '@type':task,
                      name:json{'@type':xsd:string,
                                '@value':"Groceries"}
                     }.

test(idgen_random,
     [
         setup(
             (   delete_database,
                 create_database,

             % Schema
                 forall(schema2(A,B,C),
                        insert_triple(s(A,B,C))),

                 check_and_commit
             )
         ),
         cleanup(
             (
                 delete_database
             )
         )
     ]) :-

    JSON = json{'@type':event,
                action: "click click",
                timestamp: "2021-05-20T20:33:00.000Z"
               },

    json_elaborate(JSON, Elaborated),

    Elaborated = json{'@id':_,
                      '@type':event,
                      action:json{'@type':xsd:string,
                                  '@value':"click click"},
                      timestamp:json{'@type':xsd:dateTime,
                                     '@value':"2021-05-20T20:33:00.000Z"}
                     }.
test(type_family_id, []) :-

    type_family_id(json{'@type':'Card',
                        'sys:cardinality':3,
                        'sys:class':'Person'},
                   [friend_of, 'Person'],
                   'Person_friend_of_Card_Person_3').

test(schema_elaborate, []) :-

    Schema = json{ '@type' : 'Class',
                   '@id' : 'Person',
                   'name' : 'xsd:string',
                   'age' : json{ '@type' : 'Optional',
                                 'sys:class' : 'xsd:decimal' },
                   'friend_of' : json{ '@type' : 'Card',
                                       'sys:class' : 'Person',
                                       'sys:cardinality' : 3 }
                 },

    json_schema_elaborate(Schema, Elaborated),

    Elaborated = json{'@id':'Person',
                      '@type':'Class',
                      age:json{'@id':'Person_age_Optional_xsd%3Adecimal',
                               '@type':'Optional',
                               'sys:class':json{'@type':'@id',
                                                '@value':'xsd:decimal'}
                              },
                      friend_of:json{'@id':'Person_friend_of_Card_Person_3',
                                     '@type':'Card',
                                     'sys:cardinality':json{'@type':'xsd:positiveInteger',
                                                            '@value':3},
                                     'sys:class':json{'@type':'@id',
                                                      '@value':'Person'}},
                      name:json{'@type':'@id',
                                '@value':'xsd:string'}},

    json_triples(Elaborated, Triples),

    Triples = [
        t('Person',rdf:type,'Class'),
        t('Person_age_Optional_xsd%3Adecimal',rdf:type,'Optional'),
        t('Person_age_Optional_xsd%3Adecimal','sys:class','xsd:decimal'),
        t('Person',age,'Person_age_Optional_xsd%3Adecimal'),
        t('Person_friend_of_Card_Person_3',rdf:type,'Card'),
        t('Person_friend_of_Card_Person_3','sys:cardinality', (3^^'xsd:positiveInteger')),
        t('Person_friend_of_Card_Person_3','sys:class','Person'),
        t('Person',friend_of,'Person_friend_of_Card_Person_3'),
        t('Person',name,'xsd:string')
    ].


test(list_id_key_triple, []) :-
    findall(Triple,
            list_id_key_triple([json{'@id':"task_a4963868aa3ad8365a4b164a7f206ffc",
                                     '@type':task,
                                     name:json{'@type':xsd:string,
                                               '@value':"Get Groceries"}},
                                json{'@id':"task_f9e4104c952e71025a1d68218d88bab1",
                                     '@type':task,
                                     name:json{'@type':xsd:string,
                                               '@value':"Take out rubbish"}}],
                               elt,
                               p, Triple),
            Triples),
    Triples = [
        t(elt,p,Cons1),
        t(Cons1,rdf:first,"task_a4963868aa3ad8365a4b164a7f206ffc"),
        t(Cons1,rdf:rest,Cons2),
        t(Cons2,rdf:first,"task_f9e4104c952e71025a1d68218d88bab1"),
        t(Cons2,rdf:rest,rdf:nil),
        t("task_f9e4104c952e71025a1d68218d88bab1",rdf:type,task),
        t("task_f9e4104c952e71025a1d68218d88bab1",name,("Take out rubbish"^^xsd:string)),
        t("task_a4963868aa3ad8365a4b164a7f206ffc",rdf:type,task),
        t("task_a4963868aa3ad8365a4b164a7f206ffc",name,("Get Groceries"^^xsd:string))].

test(array_id_key_triple, []) :-
    findall(Triple,
            array_id_key_triple([json{'@id':"task_a4963868aa3ad8365a4b164a7f206ffc",
                                      '@type':task,
                                      name:json{'@type':xsd:string,
                                                '@value':"Get Groceries"}},
                                 json{'@id':"task_f9e4104c952e71025a1d68218d88bab1",
                                      '@type':task,
                                      name:json{'@type':xsd:string,
                                                '@value':"Take out rubbish"}}],
                                elt,
                                p, Triple),
            Triples),

    Triples = [
        t(elt,p,Array0),
        t(Array0,sys:value,"task_a4963868aa3ad8365a4b164a7f206ffc"),
        t(Array0,sys:index,0),
        t(elt,p,Array1),
        t(Array1,sys:value,"task_f9e4104c952e71025a1d68218d88bab1"),
        t(Array1,sys:index,1),
        t("task_f9e4104c952e71025a1d68218d88bab1",rdf:type,task),
        t("task_f9e4104c952e71025a1d68218d88bab1",name,^^("Take out rubbish",xsd:string)),
        t("task_a4963868aa3ad8365a4b164a7f206ffc",rdf:type,task),
        t("task_a4963868aa3ad8365a4b164a7f206ffc",name,^^("Get Groceries",xsd:string))
    ].

test(set_id_key_triple, []) :-
    findall(Triple,
            set_id_key_triple([json{'@id':"task_a4963868aa3ad8365a4b164a7f206ffc",
                                      '@type':task,
                                      name:json{'@type':xsd:string,
                                                '@value':"Get Groceries"}},
                                 json{'@id':"task_f9e4104c952e71025a1d68218d88bab1",
                                      '@type':task,
                                      name:json{'@type':xsd:string,
                                                '@value':"Take out rubbish"}}],
                                elt,
                                p, Triple),
            Triples),

    Triples =[
        t(elt,p,"task_a4963868aa3ad8365a4b164a7f206ffc"),
        t(elt,p,"task_f9e4104c952e71025a1d68218d88bab1"),
        t("task_f9e4104c952e71025a1d68218d88bab1",rdf:type,task),
        t("task_f9e4104c952e71025a1d68218d88bab1",name,("Take out rubbish"^^xsd:string)),
        t("task_a4963868aa3ad8365a4b164a7f206ffc",rdf:type,task),
        t("task_a4963868aa3ad8365a4b164a7f206ffc",name,("Get Groceries"^^xsd:string))
    ].

test(list_elaborate,
     [
         setup(
             (   delete_database,
                 create_database,

             % Schema
                 forall(schema2(A,B,C),
                        insert_triple(s(A,B,C))),

                 check_and_commit

             )
         ),
         cleanup(
             (
                 delete_database
             )
         )
     ]) :-

    JSON = json{'@type':employee,
                name: "Gavin",
                birthdate: "1977-05-24",
                staff_number: "12",
                tasks : [
                    json{ name : "Get Groceries" },
                    json{ name : "Take out rubbish" }
                ]
               },

    json_elaborate(JSON, Elaborated),

    Elaborated = json{
        '@id':"employee_6f1bb32f84f15c68ac7b69df05967953",
        '@type':employee,
        birthdate:json{'@type':xsd:date,
                       '@value':"1977-05-24"},
        name:json{'@type':xsd:string,
                  '@value':"Gavin"},
        staff_number:json{'@type':xsd:string,
                          '@value':"12"},
        tasks:json{'@container':'@list',
                   '@type':task,
                   '@value':[json{'@id':"task_a4963868aa3ad8365a4b164a7f206ffc",
                                  '@type':task,
                                  name:json{'@type':xsd:string,
                                            '@value':"Get Groceries"}},
                             json{'@id':"task_f9e4104c952e71025a1d68218d88bab1",
                                  '@type':task,
                                  name:json{'@type':xsd:string,
                                            '@value':"Take out rubbish"}}]}},

    json_triples(Elaborated,Triples),

    Triples =[
        t("employee_6f1bb32f84f15c68ac7b69df05967953",rdf:type,employee),
        t("employee_6f1bb32f84f15c68ac7b69df05967953",birthdate,("1977-05-24"^^xsd:date)),
        t("employee_6f1bb32f84f15c68ac7b69df05967953",name,("Gavin"^^xsd:string)),
        t("employee_6f1bb32f84f15c68ac7b69df05967953",staff_number,("12"^^xsd:string)),
        t("employee_6f1bb32f84f15c68ac7b69df05967953",tasks,Cons1),
        t(Cons1,rdf:first,"task_a4963868aa3ad8365a4b164a7f206ffc"),
        t(Cons1,rdf:rest,Cons2),
        t(Cons2,rdf:first,"task_f9e4104c952e71025a1d68218d88bab1"),
        t(Cons2,rdf:rest,rdf:nil),
        t("task_f9e4104c952e71025a1d68218d88bab1",rdf:type,task),
        t("task_f9e4104c952e71025a1d68218d88bab1",name,("Take out rubbish"^^xsd:string)),
        t("task_a4963868aa3ad8365a4b164a7f206ffc",rdf:type,task),
        t("task_a4963868aa3ad8365a4b164a7f206ffc",name,("Get Groceries"^^xsd:string))],

    forall(member(t(X,Y,Z),Triples),
           insert_triple(t(X,Y,Z))),

    check_and_commit,

    id_json("employee_6f1bb32f84f15c68ac7b69df05967953", Fresh_JSON),

    Fresh_JSON = json{'@id':"employee_6f1bb32f84f15c68ac7b69df05967953",
                      '@type':employee,
                      birthdate:"1977-05-24",
                      name:"Gavin",
                      staff_number:"12",
                      tasks:["task_a4963868aa3ad8365a4b164a7f206ffc",
                             "task_f9e4104c952e71025a1d68218d88bab1"]}.

test(array_elaborate,
     [
         setup(
             (   delete_database,
                 create_database,

             % Schema
                 forall(schema2(A,B,C),
                        insert_triple(s(A,B,C))),

                 check_and_commit
             )
         ),
         cleanup(
             (
                 delete_database
             )
         )
     ]) :-

    JSON = json{'@type':book_club,
                name: "Marxist book club",
                book_list : [
                    json{ name : "Das Kapital" },
                    json{ name : "Der Ursprung des Christentums" }
                ]
               },

    json_elaborate(JSON,Elaborated),

    Elaborated = json{'@id':"book_club_Marxist%20book%20club",
                      '@type':book_club,
                      book_list:json{'@container':'@array',
                                     '@type':book,
                                     '@value':[json{'@id':"book_Das%20Kapital",
                                                    '@type':book,
                                                    name:json{'@type':xsd:string,
                                                              '@value':"Das Kapital"}},
                                               json{'@id':"book_Der%20Ursprung%20des%20Christentums",
                                                    '@type':book,
                                                    name:json{'@type':xsd:string,
                                                              '@value':"Der Ursprung des Christentums"}}]},
                      name:json{'@type':xsd:string,
                                '@value':"Marxist book club"}},

    json_triples(JSON,Triples),

    Triples = [
        t("book_club_Marxist%20book%20club",rdf:type,book_club),
        t("book_club_Marxist%20book%20club",book_list,ArrayElement0),
        t(ArrayElement0,sys:value,"book_Das%20Kapital"),
        t(ArrayElement0,sys:index,0),
        t("book_club_Marxist%20book%20club",book_list,ArrayElement1),
        t(ArrayElement1,sys:value,"book_Der%20Ursprung%20des%20Christentums"),
        t(ArrayElement1,sys:index,1),
        t("book_Der%20Ursprung%20des%20Christentums",rdf:type,book),
        t("book_Der%20Ursprung%20des%20Christentums",name,("Der Ursprung des Christentums"^^xsd:string)),
        t("book_Das%20Kapital",rdf:type,book),
        t("book_Das%20Kapital",name,("Das Kapital"^^xsd:string)),
        t("book_club_Marxist%20book%20club",name,("Marxist book club"^^xsd:string))
    ],

    forall(member(t(X1,X2,X3),Triples),
           insert_triple(t(X1,X2,X3))),

    check_and_commit,

    id_json("book_club_Marxist%20book%20club", Recovered),

    Recovered = json{'@id':"book_club_Marxist%20book%20club",
                     '@type':book_club,
                     book_list:["book_Das%20Kapital",
                                "book_Der%20Ursprung%20des%20Christentums"],
                     name:"Marxist book club"}.

test(set_elaborate,
     [
         setup(
             (   delete_database,
                 create_database,

             % Schema
                 forall(schema2(A,B,C),
                        insert_triple(s(A,B,C))),

                 check_and_commit
             )
         ),
         cleanup(
             (
                 delete_database
             )
         )
     ]) :-

    JSON = json{'@type':book_club,
                name: "Marxist book club",
                people: [
                    json{'@type' : person,
                         name : "jim",
                         birthdate: "1982-05-03"
                        },
                    json{'@type':person,
                         birthdate:"1979-12-28",
                         name:"jane"
                        }],
                book_list : []
               },

    json_elaborate(JSON, Elaborated),

    Elaborated = json{'@id':"book_club_Marxist%20book%20club",
                      '@type':book_club,
                      book_list:json{'@container':'@array',
                                     '@type':book,
                                     '@value':[]},
                      name:json{'@type':xsd:string,
                                '@value':"Marxist book club"},
                      people:json{'@container':'@set','@type':person,
                                  '@value':[json{'@id':"person_jim_1982-05-03",
                                                 '@type':person,
                                                 birthdate:json{'@type':xsd:date,
                                                                '@value':"1982-05-03"},
                                                 name:json{'@type':xsd:string,
                                                           '@value':"jim"}},
                                            json{'@id':"person_jane_1979-12-28",
                                                 '@type':person,
                                                 birthdate:json{'@type':xsd:date,
                                                                '@value':"1979-12-28"},
                                                 name:json{'@type':xsd:string,
                                                           '@value':"jane"}}]}},

    json_triples(JSON,Triples),

    Triples = [t("book_club_Marxist%20book%20club",rdf:type,book_club),
               t("book_club_Marxist%20book%20club",name,("Marxist book club"^^xsd:string)),
               t("book_club_Marxist%20book%20club",people,"person_jim_1982-05-03"),
               t("book_club_Marxist%20book%20club",people,"person_jane_1979-12-28"),
               t("person_jane_1979-12-28",rdf:type,person),
               t("person_jane_1979-12-28",birthdate,("1979-12-28"^^xsd:date)),
               t("person_jane_1979-12-28",name,("jane"^^xsd:string)),
               t("person_jim_1982-05-03",rdf:type,person),
               t("person_jim_1982-05-03",birthdate,("1982-05-03"^^xsd:date)),
               t("person_jim_1982-05-03",name,("jim"^^xsd:string))],

    forall(member(t(X,Y,Z), Triples),
           insert_triple(t(X,Y,Z))),

    check_and_commit,

    id_json("book_club_Marxist%20book%20club", Book_Club),

    Book_Club = json{'@id':"book_club_Marxist%20book%20club",
                     '@type':book_club,
                     name:"Marxist book club",
                     people:["person_jane_1979-12-28",
                             "person_jim_1982-05-03"]}.

test(empty_list,
     [
         setup(
             (   delete_database,
                 create_database,

             % Schema
                 forall(schema2(A,B,C),
                        insert_triple(s(A,B,C))),

                 check_and_commit
             )
         ),
         cleanup(
             (
                 delete_database
             )
         )
     ]) :-

    JSON = json{'@type':employee,
                name: "Gavin",
                birthdate: "1977-05-24",
                staff_number: "12",
                tasks : []
               },

    json_triples(JSON,Triples),

    Employee = "employee_6f1bb32f84f15c68ac7b69df05967953",
    Triples =[t(Employee,rdf:type,employee),
              t(Employee,birthdate,("1977-05-24"^^xsd:date)),
              t(Employee,name,^^("Gavin",xsd:string)),
              t(Employee,staff_number,^^("12",xsd:string)),
              t(Employee,tasks,rdf:nil)],

    forall(member(t(X,Y,Z), Triples),
           insert_triple(t(X,Y,Z))),

    check_and_commit,

    id_json(Employee, Employee_JSON),

    Employee_JSON = json{'@id':Employee,
                         '@type':employee,
                         birthdate:"1977-05-24",
                         name:"Gavin",
                         staff_number:"12",
                         tasks:[]}.

test(enum_elaborate,
     [
         setup(
             (   delete_database,
                 create_database,

             % Schema
                 forall(schema2(A,B,C),
                        insert_triple(s(A,B,C))),

                 check_and_commit
             )
         ),
         cleanup(
             (
                 delete_database
             )
         )
     ]) :-

    JSON = json{'@type':dog,
                name: "Ralph",
                hair_colour: "blue"
               },

    json_elaborate(JSON, Elaborated),

    Elaborated = json{'@id':"dog_Ralph",
                      '@type':dog,
                      hair_colour:json{'@type':colour,
                                       '@value':"blue"},
                      name:json{'@type':xsd:string,
                                '@value':"Ralph"}},

    json_triples(JSON,Triples),

    Triples = [t("dog_Ralph",rdf:type,dog),
               t("dog_Ralph",hair_colour,("blue"^^colour)),
               t("dog_Ralph",name,("Ralph"^^xsd:string))],

    forall(member(t(X,Y,Z), Triples),
           insert_triple(t(X,Y,Z))),

    check_and_commit,

    id_json("dog_Ralph", Dog_JSON),

    Dog_JSON = json{'@id':"dog_Ralph",
                    '@type':dog,
                    hair_colour:"blue",
                    name:"Ralph"}.

test(elaborate_tagged_union,[]) :-

    Binary_Tree = json{ '@type' : 'TaggedUnion',
                        '@id' : binary_tree,
                        'sys:base' : binary_tree_,
                        'sys:key' : json{ '@type' : 'sys:value_hash'},
                        leaf : json{},
                        node : node
                      },

    Node = json{ '@type' : 'Class',
                 '@id' : node,
                 'sys:base' : node,
                 'sys:key' : json{ '@type' : 'sys:value_hash'},
                 value : 'xsd:integer',
                 left : binary_tree,
                 right : binary_tree
               },

    json_schema_elaborate(Binary_Tree, BT_Elaborated),

    BT_Elaborated = json{'@id':binary_tree,
                         '@type':'TaggedUnion',
                         leaf:json{},
                         node:json{'@type':'@id',
                                   '@value':node},
                         'sys:base':json{'@type':'@id',
                                         '@value':binary_tree_},
                         'sys:key':json{'@type':'sys:value_hash'}},

    json_schema_elaborate(Node, Node_Elaborated),

    Node_Elaborated = json{'@id':node,'@type':'Class',
                           left:json{'@type':'@id',
                                     '@value':binary_tree},
                           right:json{'@type':'@id',
                                      '@value':binary_tree},
                           'sys:base':json{'@type':'@id',
                                           '@value':node},
                           'sys:key':json{'@type':'sys:value_hash'},
                           value:json{'@type':'@id',
                                      '@value':'xsd:integer'}}.

test(binary_tree_context,
     [
         setup(
             (   delete_database,
                 create_database,

             % Schema
                 forall(schema2(A,B,C),
                        insert_triple(s(A,B,C))),

                 check_and_commit
             )
         ),
         cleanup(
             (
                 delete_database
             )
         )
     ]) :-

    type_context(binary_tree, Binary_Context),
    Binary_Context = json{node:json{'@type':'@id'}},

    type_context(node, Node_Context),
    Node_Context = json{left:json{'@type':'@id'},
                        right:json{'@type':'@id'},
                        value:json{'@type':xsd:integer}}.


test(binary_tree_elaborate,
     [
         setup(
             (   delete_database,
                 create_database,

             % Schema
                 forall(schema2(A,B,C),
                        insert_triple(s(A,B,C))),

                 check_and_commit
             )
         ),
         cleanup(
             (
                 delete_database
             )
         )
     ]) :-

    JSON = json{'@type':binary_tree,
                node: json{'@type':node,
                           value: 1,
                           left: json{'@type':binary_tree,
                                      node: json{'@type':node,
                                                 value: 0,
                                                 left: json{'@type':binary_tree,
                                                            leaf : json{}},
                                                 right: json{'@type':binary_tree,
                                                             leaf : json{}}}},
                           right: json{'@type':binary_tree,
                                      node: json{'@type':node,
                                                 value: 2,
                                                 left: json{'@type':binary_tree,
                                                            leaf : json{}},
                                                 right: json{'@type':binary_tree,
                                                             leaf : json{}}}}}},

    json_elaborate(JSON,Elaborated),

    Elaborated = json{'@id':"binary_tree_6162f820aed5255e723ac6c8204a93a7",
                      '@type':binary_tree,
                      node:json{'@id':"node_key_5eae43f6b0c9f168746c13c1618de64c",
                                '@type':node,
                                left:json{'@id':"binary_tree_54311eeaadeb74170bc7d798c0f26981",
                                          '@type':binary_tree,
                                          node:json{'@id':"node_key_32d40d13f5c3cad3aceed61ea04fd424",
                                                    '@type':node,
                                                    left:json{'@id':"binary_tree_30cdfe5207fa97225ab97f77c4739df5",
                                                              '@type':binary_tree,
                                                              leaf:json{}},
                                                    right:json{'@id':"binary_tree_30cdfe5207fa97225ab97f77c4739df5",
                                                               '@type':binary_tree,
                                                               leaf:json{}},
                                                    value:json{'@type':xsd:integer,'@value':0}}},
                                right:json{'@id':"binary_tree_d64802d34dc699934655986166394d6a",
                                           '@type':binary_tree,
                                           node:json{'@id':"node_key_f521087200a9df87f3f1f0e3f994301a",
                                                     '@type':node,
                                                     left:json{'@id':"binary_tree_30cdfe5207fa97225ab97f77c4739df5",
                                                               '@type':binary_tree,
                                                               leaf:json{}},
                                                     right:json{'@id':"binary_tree_30cdfe5207fa97225ab97f77c4739df5",
                                                                '@type':binary_tree,
                                                                leaf:json{}},
                                                     value:json{'@type':xsd:integer,
                                                                '@value':2}}},
                                value:json{'@type':xsd:integer,
                                           '@value':1}}},

    json_triples(JSON,Triples),

    Triples = [
        t("binary_tree_6162f820aed5255e723ac6c8204a93a7",rdf:type,binary_tree),
        t("node_key_5eae43f6b0c9f168746c13c1618de64c",rdf:type,node),
        t("binary_tree_54311eeaadeb74170bc7d798c0f26981",rdf:type,binary_tree),
        t("node_key_32d40d13f5c3cad3aceed61ea04fd424",rdf:type,node),
        t("binary_tree_30cdfe5207fa97225ab97f77c4739df5",rdf:type,binary_tree),
        t("binary_tree_30cdfe5207fa97225ab97f77c4739df5",leaf,^^(1,unit)),
        t("node_key_32d40d13f5c3cad3aceed61ea04fd424",left,"binary_tree_30cdfe5207fa97225ab97f77c4739df5"),
        t("binary_tree_30cdfe5207fa97225ab97f77c4739df5",rdf:type,binary_tree),
        t("binary_tree_30cdfe5207fa97225ab97f77c4739df5",leaf,^^(1,unit)),
        t("node_key_32d40d13f5c3cad3aceed61ea04fd424",right,"binary_tree_30cdfe5207fa97225ab97f77c4739df5"),
        t("node_key_32d40d13f5c3cad3aceed61ea04fd424",value,^^(0,xsd:integer)),
        t("binary_tree_54311eeaadeb74170bc7d798c0f26981",node,"node_key_32d40d13f5c3cad3aceed61ea04fd424"),
        t("node_key_5eae43f6b0c9f168746c13c1618de64c",left,"binary_tree_54311eeaadeb74170bc7d798c0f26981"),
        t("binary_tree_d64802d34dc699934655986166394d6a",rdf:type,binary_tree),
        t("node_key_f521087200a9df87f3f1f0e3f994301a",rdf:type,node),
        t("binary_tree_30cdfe5207fa97225ab97f77c4739df5",rdf:type,binary_tree),
        t("binary_tree_30cdfe5207fa97225ab97f77c4739df5",leaf,^^(1,unit)),
        t("node_key_f521087200a9df87f3f1f0e3f994301a",left,"binary_tree_30cdfe5207fa97225ab97f77c4739df5"),
        t("binary_tree_30cdfe5207fa97225ab97f77c4739df5",rdf:type,binary_tree),
        t("binary_tree_30cdfe5207fa97225ab97f77c4739df5",leaf,^^(1,unit)),
        t("node_key_f521087200a9df87f3f1f0e3f994301a",right,"binary_tree_30cdfe5207fa97225ab97f77c4739df5"),
        t("node_key_f521087200a9df87f3f1f0e3f994301a",value,^^(2,xsd:integer)),
        t("binary_tree_d64802d34dc699934655986166394d6a",node,"node_key_f521087200a9df87f3f1f0e3f994301a"),
        t("node_key_5eae43f6b0c9f168746c13c1618de64c",right,"binary_tree_d64802d34dc699934655986166394d6a"),
        t("node_key_5eae43f6b0c9f168746c13c1618de64c",value,^^(1,xsd:integer)),
        t("binary_tree_6162f820aed5255e723ac6c8204a93a7",node,"node_key_5eae43f6b0c9f168746c13c1618de64c")
    ],


    forall(member(t(X,Y,Z),Triples),
           insert_triple(t(X,Y,Z))),

    check_and_commit,

    id_json("binary_tree_6162f820aed5255e723ac6c8204a93a7", Fresh_JSON),
    Fresh_JSON = json{'@id':"binary_tree_6162f820aed5255e723ac6c8204a93a7",
                      '@type':binary_tree,
                      node:"node_key_5eae43f6b0c9f168746c13c1618de64c"
                      }.


:- end_tests(json).
