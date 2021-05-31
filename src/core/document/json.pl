:- module('document/json', [
              context_triple/2,
              json_elaborate/3,
              json_triple/4,
              json_schema_triple/2,
              json_schema_triple/3,
              json_schema_elaborate/2,
              id_json/3,
              database_context/2
          ]).

:- use_module(instance).
:- use_module(schema).

:- use_module(library(pcre)).
:- use_module(library(yall)).
:- use_module(library(apply_macros)).

:- use_module(core(util), [merge_separator_split/3,do_or_die/2]).
:- use_module(core(query), [expand/3,global_prefix_expand/2,prefix_expand/3]).
:- use_module(core(triple), [xrdf/4,database_instance/2,database_schema/2]).

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

idgen(JSON,ID) :-
    get_dict('@type',JSON,Type),
    key_descriptor(Type,Descriptor),
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
    once(xrdf(Schema, ID, rdf:type, sys:'Context')),
    !,
    id_schema_json(DB,ID,Context).

predicate_map(P, Context, Prop, json{ '@id' : P }) :-
    % NOTE: This is probably wrong if it already has a prefix...
    get_dict('@schema', Context, Base),
    atomic_list_concat([Base,'(.*)'],Pat),
    re_matchsub(Pat, P, Match, []),
    !,
    get_dict(1,Match,Short),
    atom_string(Prop,Short).
predicate_map(P, _Context, P, json{}).

type_context(DB,Type,Context) :-
    database_context(DB, Database_Context),
    atom_string(Type_Atom,Type),
    prefix_expand(Type_Atom,Database_Context,TypeEx),
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
    json_jsonid(Elaborated,JSON_ID).

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
            ;   P = Prop,
                V = Value
            )
        ),
        PVs),
    dict_pairs(Expanded,json,PVs).

json_jsonid(JSON,JSON_ID) :-
    % Set up the ID
    (   get_dict('@id',JSON,_)
    ->  JSON_ID = JSON
    ;   get_dict('@container', JSON, _)
    ->  JSON_ID = JSON
    ;   get_dict('@value', JSON, _)
    ->  JSON_ID = JSON
    ;   idgen(JSON,ID)
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

json_schema_predicate_value('@id',V,_,'@id',V) :-
    !.
json_schema_predicate_value(P,V,_,P,json{'@type' : 'xsd:positiveInteger',
                                  '@value' : V }) :-
    global_prefix_expand(sys:cardinality, P),
    !.
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


json_schema_triple(JSON,Context,Triple) :-
    json_schema_elaborate(JSON,JSON_Schema),
    get_dict('@schema', Context, Schema),
    put_dict(_{'@base' : Schema}, Context, Schema_Context),
    expand(JSON_Schema,Schema_Context,Expanded),
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

type_id_predicate_iri_value(enum(C),_,_,V^^C,_,V).
type_id_predicate_iri_value(list(_),_,_,O,DB,V) :-
    database_instance(DB,Instance),
    rdf_list_list(Instance,O,V).
type_id_predicate_iri_value(array(_),Id,P,_,DB,V) :-
    array_list(DB,Id,P,V).
type_id_predicate_iri_value(set(_),Id,P,_,DB,V) :-
    set_list(DB,Id,P,V).
type_id_predicate_iri_value(cardinality(_,_),Id,P,_,DB,V) :-
    set_list(DB,Id,P,V).
type_id_predicate_iri_value(class(_),_,_,V,_,V).
type_id_predicate_iri_value(tagged_union(_),_,_,V,_,V).
type_id_predicate_iri_value(optional(_),_,_,V,_,V).
type_id_predicate_iri_value(base_class(_),_,_,O,_,V) :-
    value_json(O, I),
    (   is_dict(I)
    ->  get_dict('@value', I, V)
    ;   I = V
    ).

id_json(DB, Id, JSON) :-
    database_instance(DB,Instance),
    xrdf(Instance, Id, rdf:type, Class),

    findall(
        P-V,
        (   distinct([P],xrdf(Instance,Id,P,O)),
            \+ is_built_in(P),

            once(class_predicate_type(DB,Class,P,Type)),
            type_id_predicate_iri_value(Type,Id,P,O,DB,V)
        ),
        Data),
    !,
    dict_create(JSON,json,['@id'-Id,
                           '@type'-Class
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



:- begin_tests(json_conv).

:- use_module(core(util/test_utils)).

context1(json{ '@type' : "Context",
               '@base' : "i/",
               '@schema' : "s/" }).

schema1(json{ '@id' : "Person",
              '@type' : "Class",
              'name' : "xsd:string",
              'birthdate' : "xsd:date",
              'friends' : json{ '@type' : "Set",
                                '@class' : "Person" } }).
schema1(json{ '@id' : "Employee",
              '@type' : "Class",
              '@inherits' : "Person",
              'staff_number' : "xsd:string",
              'boss' : json{ '@type' : "Optional",
                             '@class' : "Employee" },
              'tasks' : json{ '@type' : "List",
                              '@class' : "Task" } }).
schema1(json{ '@id' : "Task",
              '@type' : "Class",
              'name' : "xsd:string" }).
schema1(json{ '@id' : "Criminal",
              '@type' : "Class",
              '@inherits' : "Person",
              'aliases' : json{ '@type' : "List",
                                '@class' : "xsd:string" } }).

write_schema1(Desc) :-
    create_context(Desc,commit{
                            author : "me",
                            message : "none"},
                   Context),

    % Schema
    with_transaction(
        Context,
        forall(
            (   (   context1(Ctx),
                    context_triple(Ctx,t(A,B,C))
                ;   schema1(Doc),
                    json_schema_triple(Doc,t(A,B,C)))
            ),
            ask(Context,
                insert(A,B,C))),
        _Meta
    ).

test(create_database_context,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema(admin,test),
                 resolve_absolute_string_descriptor('admin/test',Desc),
                 write_schema1(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    open_descriptor(Desc, DB),
    type_context(DB,employee,Context),

    Context = json{birthdate:json{'@type':"xsd:date"},
                   boss:json{'@type':"@id"},
                   friends:json{'@container':"@set",
                                '@type':"person"},
                   name:json{'@type':"xsd:string"},
                   staff_number:json{'@type':"xsd:string"},
                   tasks:json{'@container':"@list",
                              '@type':"task"}}.

test(elaborate,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema(admin,test),
                 resolve_absolute_string_descriptor('admin/test',Desc),
                 write_schema1(Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    Document = json{
                   '@id' : gavin,
                   '@type' : criminal,
                   name : "gavin",
                   birthdate : "1977-05-24",
                   aliases : ["gavino", "gosha"]
               },

    open_descriptor(Desc,DB),
    json_elaborate(DB, Document, Elaborated),

    Elaborated = json{'@id':"gavin",
                      '@type':"criminal",
                      aliases:json{'@container':'@list',
                                   '@type':"xsd:string",
                                   '@value':[json{'@type':"xsd:string",
                                                  '@value':"gavino"},
                                             json{'@type':"xsd:string",
                                                  '@value':"gosha"}]},
                      birthdate:json{'@type':"xsd:date",
                                     '@value':"1977-05-24"},
                      name:json{'@type':"xsd:string",
                                '@value':"gavin"}}.

test(id_expand,
     [
         setup(
             (   delete_database,
                 create_database,

             % Schema
                 forall(schema1(A,B,C),
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

    Document = json{
                   '@id' : gavin,
                   '@type' : employee,
                   name : "gavin",
                   staff_number : "13",
                   birthdate : "1977-05-24",
                   boss : json{
                              '@id' : jane,
                              '@type' : employee,
                              name : "jane",
                              staff_number : "12",
                              birthdate : "1979-12-28"
                          }
               },

    json_elaborate(Document, Elaborated),

    Elaborated = json{'@id':gavin,
                      '@type':employee,
                      birthdate:json{'@type':xsd:date,
                                     '@value':"1977-05-24"},
                      boss:json{'@id':jane,
                                '@type':employee,
                                birthdate:json{'@type':xsd:date,
                                               '@value':"1979-12-28"},
                                name:json{'@type':xsd:string,'@value':"jane"},
                                staff_number:json{'@type':xsd:string,
                                                  '@value':"12"}
                               },
                      name:json{'@type':xsd:string,
                                '@value':"gavin"},
                      staff_number:json{'@type':xsd:string,
                                        '@value':"13"}}.


test(id_free,
     [
         setup(
             (   delete_database,
                 create_database,

             % Schema
                 forall(schema1(A,B,C),
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

    Document = json{
                   '@id' : gavin,
                   '@type' : employee,
                   name : "gavin",
                   staff_number : "13",
                   birthdate : "1977-05-24",
                   boss : json{
                              '@id' : jane,
                              '@type' : employee,
                              name : "jane",
                              staff_number : "12",
                              birthdate : "1979-12-28"
                          }
               },

    json_elaborate(Document, Elaborated),

    Elaborated = json{'@id':gavin,
                      '@type':employee,
                      birthdate:json{'@type':xsd:date,
                                     '@value':"1977-05-24"},
                      boss:json{'@id':jane,
                                '@type':employee,
                                birthdate:json{'@type':xsd:date,
                                               '@value':"1979-12-28"},
                                name:json{'@type':xsd:string,'@value':"jane"},
                                staff_number:json{'@type':xsd:string,
                                                  '@value':"12"}
                               },
                      name:json{'@type':xsd:string,
                                '@value':"gavin"},
                      staff_number:json{'@type':xsd:string,
                                        '@value':"13"}}.


test(triple_convert,
     [
         setup(
             (   delete_database,
                 create_database,

             % Schema
                 forall(schema1(A,B,C),
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

    Document = json{
                   '@id' : gavin,
                   '@type' : employee,
                   name : "gavin",
                   staff_number : "13",
                   birthdate : "1977-05-24",
                   boss : json{
                              '@id' : jane,
                              '@type' : employee,
                              name : "jane",
                              staff_number : "12",
                              birthdate : "1979-12-28"
                          }
               },

    json_triples(Document, Triples),

    Triples = [
        t(gavin,rdf:type,employee),
        t(gavin,birthdate,"1977-05-24"^^xsd:date),
        t(jane,rdf:type,employee),
        t(jane,birthdate,"1979-12-28"^^xsd:date),
        t(jane,name,"jane"^^xsd:string),
        t(jane,staff_number,"12"^^xsd:string),
        t(gavin,boss,jane),
        t(gavin,name,"gavin"^^xsd:string),
        t(gavin,staff_number,"13"^^xsd:string)
    ].

test(extract_json,
     [
         setup(
             (   setup_temp_store(State),
                 create_db_without_schema(admin,test),

             % Schema
                 forall(schema1(A,B,C),
                        insert_triple(s(A,B,C))),

                 check_and_commit,

             % Document
                 Document = json{
                                '@id' : gavin,
                                '@type' : employee,
                                name : "gavin",
                                staff_number : "13",
                                birthdate : "1977-05-24",
                                boss : json{
                                           '@id' : jane,
                                           '@type' : employee,
                                           name : "jane",
                                           staff_number : "12",
                                           birthdate : "1979-12-28"
                                       }
                            },

                 forall(json_triple(Document,t(X,P,Y)),
                        insert_triple(t(X,P,Y)))

             )
         ),
         cleanup(
             (
                 teardown_temp_store(State)
             )
         )
     ]) :-

    id_json(gavin,JSON1),

    JSON1 = json{'@id':gavin,
                 '@type':employee,
                 birthdate:"1977-05-24",
                 boss:jane,
                 name:"gavin",
                 staff_number:"13"
                },

    id_json(jane,JSON2),

    JSON2 = json{'@id':jane,
                 '@type':employee,
                 birthdate:"1979-12-28",
                 name:"jane",
                 staff_number:"12"}.

test(get_value,
     [
         setup(
             (   delete_database,
                 create_database,

             % Schema
                 forall(schema1(A,B,C),
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

    JSON = json{'@id':jane,
                '@type':employee,
                birthdate:"1979-12-28",
                name:"jane",
                staff_number:"12"},

    json_elaborate(JSON,Elaborated),

    get_all_path_values(Elaborated,Values),

    Values = [['@type']-employee,
              [birthdate]- ("1979-12-28"^^xsd:date),
              [name]- ("jane"^^xsd:string),
              [staff_number]- ("12"^^xsd:string)].


schema2(person, rdf:type, 'Class').
schema2(person, sys:key, person_key).
schema2(person, sys:base, person_^^xsd:string).
schema2(person_key, rdf:type, sys:lexical).
schema2(person_key, sys:fields, person_key_list1).
schema2(person_key_list1, rdf:first, name).
schema2(person_key_list1, rdf:rest, person_key_list2).
schema2(person_key_list2, rdf:first, birthdate).
schema2(person_key_list2, rdf:rest, rdf:nil).
schema2(person, name, xsd:string).
schema2(person, birthdate, xsd:date).
schema2(person, friends, set_person).
schema2(set_person, rdf:type, 'Set').
schema2(set_person, sys:class, person).
schema2(employee, rdf:type, 'Class').
schema2(employee, sys:base, employee_^^xsd:string).
schema2(employee, sys:key, employee_key).
schema2(employee_key, rdf:type, sys:hash).
schema2(employee_key, sys:fields, employee_key_list1).
schema2(employee_key_list1, rdf:first, name).
schema2(employee_key_list1, rdf:rest, employee_key_list2).
schema2(employee_key_list2, rdf:first, birthdate).
schema2(employee_key_list2, rdf:rest, rdf:nil).
schema2(employee, rdfs:subClassOf, person).
schema2(employee, staff_number, xsd:string).
schema2(employee, boss, optional_employee).
schema2(optional_employee, rdf:type, 'Optional').
schema2(optional_employee, sys:class, employee).
schema2(employee, tasks, list_task).
schema2(list_task, rdf:type, 'List').
schema2(list_task, sys:class, task).
schema2(task, rdf:type, 'Class').
schema2(task, sys:base, task_^^xsd:string).
schema2(task, name, xsd:string).
schema2(task, sys:key, task_key).
schema2(task_key, rdf:type, sys:value_hash).
schema2(criminal, rdf:type, 'Class').
schema2(criminal, sys:base, criminal_^^xsd:string).
schema2(criminal, rdfs:subClassOf, person).
schema2(criminal, aliases, list_string).
schema2(list_string, rdf:type, 'List').
schema2(list_string, sys:class, xsd:string).
schema2(event, rdf:type, 'Class').
schema2(event, sys:base, event_^^xsd:string).
schema2(event, action, xsd:string).
schema2(event, timestamp, xsd:dateTime).
schema2(event, sys:key, event_key).
schema2(event_key, rdf:type, sys:random).
schema2(book, rdf:type, 'Class').
schema2(book, sys:base, book_^^xsd:string).
schema2(book, name, xsd:string).
schema2(book, sys:key, book_key).
schema2(book_key, rdf:type, sys:lexical).
schema2(book_key, sys:fields, book_key_fields).
schema2(book_key_fields, rdf:first, name).
schema2(book_key_fields, rdf:rest, rdf:nil).
schema2(book_club, rdf:type, 'Class').
schema2(book_club, sys:base, book_club_^^xsd:string).
schema2(book_club, sys:key, book_club_key).
schema2(book_club_key, rdf:type, sys:lexical).
schema2(book_club_key, sys:fields, book_club_key_list1).
schema2(book_club_key_list1, rdf:first, name).
schema2(book_club_key_list1, rdf:rest, rdf:nil).
schema2(book_club, name, xsd:string).
schema2(book_club, people, book_club_set_people).
schema2(book_club_set_people, rdf:type, 'Set').
schema2(book_club_set_people, sys:class, person).
schema2(book_club, book_list, book_club_array).
schema2(book_club_array, rdf:type, 'Array').
schema2(book_club_array, sys:class, book).
schema2(colour, rdf:type, 'Enum').
schema2(colour, sys:value, colour_list0).
schema2(colour_list0, rdf:first, red).
schema2(colour_list0, rdf:rest, colour_list1).
schema2(colour_list1, rdf:first, blue).
schema2(colour_list1, rdf:rest, colour_list2).
schema2(colour_list2, rdf:first, green).
schema2(colour_list2, rdf:rest, rdf:nil).
schema2(dog, rdf:type, 'Class').
schema2(dog, sys:base, dog_^^xsd:string).
schema2(dog, sys:key, dog_key).
schema2(dog_key, rdf:type, sys:lexical).
schema2(dog_key, sys:fields, dog_key_list1).
schema2(dog_key_list1, rdf:first, name).
schema2(dog_key_list1, rdf:rest, rdf:nil).
schema2(dog, name, xsd:string).
schema2(dog, hair_colour, colour).
schema2(binary_tree, rdf:type, 'TaggedUnion').
schema2(binary_tree, sys:base, binary_tree_^^xsd:string).
schema2(binary_tree, sys:key, binary_tree_key).
schema2(binary_tree_key, rdf:type, sys:value_hash).
schema2(binary_tree, leaf, 'Unit').
schema2(binary_tree, node, node).
schema2(node, rdf:type, 'Class').
schema2(node, sys:base, node_key_^^xsd:string).
schema2(node, sys:key, node_key).
schema2(node_key, rdf:type, sys:value_hash).
schema2(node, value, xsd:integer).
schema2(node, left, binary_tree).
schema2(node, right, binary_tree).

test(idgen_lexical,
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

    JSON = json{'@type':person,
                birthdate:"1979-12-28",
                name:"jane"
               },

    json_elaborate(JSON, Elaborated),

    Elaborated = json{'@id':"person_jane_1979-12-28",
                      '@type':person,
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


:- end_tests(json_conv).
