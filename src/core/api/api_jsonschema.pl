:- module(api_jsonschema, [
              tdb_class_to_json_schema/3,
              api_class_jsonschema/5
          ]).
/** <module> Class to JSON Schema conversion
 *
 * Port of the Zebra RecordForm class->JSON Schema generator
 * (buildJsonSchemaDefinition.ts) to the server side. Consumes the
 * normalized class frame produced by class_frame/4 and emits a
 * JSON Schema (draft-07) suitable for validating documents of the
 * class and for driving generated user interfaces.
 *
 * Subdocuments are inlined to depth 2, after which a
 * {"$ref":"#/definitions/<Class>"} reference is emitted and the
 * referenced class schema is generated under 'definitions'. Enum
 * references resolve through 'definitions'.EnumtraitValues like in
 * Zebra.
 *
 * Constructs that JSON Schema cannot express are recorded under
 * 'x-terminusdb' extensions; anything that cannot be represented
 * faithfully degrades to a permissive schema plus a limitations
 * note rather than a silently wrong schema.
 */

:- use_module(core(util)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(pairs)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(account)).
:- use_module(core(document)).
:- use_module(core(document/json), [compress_schema_uri/3, compress_schema_uri/4]).
:- use_module(core(document/schema), [schema_is_abstract/2, schema_concrete_subclass/3]).
:- use_module(core(triple)).

/**
 * api_class_jsonschema(+System_DB, +Auth, +Path, +Class_Spec, -JSON) is det.
 *
 * Class_Spec is uri(Class_Name) or all. For all, produces a single
 * JSON Schema document with one entry per class under 'definitions'.
 */
api_class_jsonschema(System_DB, Auth, Path, Class_Spec, JSON) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_absolute_path(Path),_)),
    do_or_die(
        askable_context(Descriptor, System_DB, Auth, Context),
        error(unresolvable_collection(Descriptor),_)),
    assert_read_access(Context),
    (   Class_Spec = uri(Class)
    ->  tdb_class_to_json_schema(Context, Class, JSON)
    ;   all_class_jsonschema(Context, JSON)
    ).

/**
 * tdb_class_to_json_schema(+Askable, +Class, -JSON) is det.
 *
 * Askable is anything class_frame/4 accepts (transaction,
 * query_context or descriptor). Class is the compressed or full
 * class name.
 */
tdb_class_to_json_schema(Askable, Class, JSON) :-
    askable_to_transaction(Askable, TO),
    database_schema(TO, Schema),
    database_prefixes(TO, DB_Prefixes),
    default_prefixes(Default_Prefixes),
    Prefixes = Default_Prefixes.put(DB_Prefixes),
    prefix_expand_schema(Class, Prefixes, Class_Ex),
    class_frame(TO, Class_Ex, Frame),
    class_frame_to_json_schema(TO, Schema, Prefixes, 0, Frame, Class_Ex, JSON0),
    resolve_definitions(TO, Schema, Prefixes, JSON0, JSON1),
    JSON = JSON1.put('$schema', "https://json-schema.org/draft-07/schema#").

all_class_jsonschema(Query_Context, JSON) :-
    query_default_collection(Query_Context, TO),
    database_schema(TO, Schema),
    database_prefixes(TO, DB_Prefixes),
    default_prefixes(Default_Prefixes),
    Prefixes = Default_Prefixes.put(DB_Prefixes),
    all_class_frames(TO, Frames),
    dict_pairs(Frames, _, Pairs),
    findall(
        Class_Comp-Sub,
        (   member(Class_Comp-Frame, Pairs),
            Class_Comp \= '@context',
            prefix_expand_schema(Class_Comp, Prefixes, Class_Ex),
            class_frame_to_json_schema(TO, Schema, Prefixes, 0, Frame, Class_Ex, Sub)
        ),
        Definition_Pairs),
    dict_pairs(Definitions0, json, Definition_Pairs),
    JSON0 = json{
                '$schema' : "https://json-schema.org/draft-07/schema#",
                definitions : Definitions0
            },
    resolve_definitions(TO, Schema, Prefixes, JSON0, JSON).

% askable_to_transaction(+Askable, -Transaction) is det.
askable_to_transaction(Askable, TO) :-
    (   (   is_transaction(Askable)
        ;   is_validation_object(Askable))
    ->  TO = Askable
    ;   is_query_context(Askable)
    ->  query_default_collection(Askable, TO)
    ;   is_descriptor(Askable)
    ->  open_descriptor(Askable, TO)
    ).

% class_frame_to_json_schema(+TO, +Schema, +Prefixes, +Depth, +Frame, +Class_Ex, -JSON) is det.
% Depth bounds subdocument inlining (references beyond depth 2).
class_frame_to_json_schema(_TO, _Schema, _Prefixes, _Depth, Frame, _Class_Ex, JSON) :-
    Frame.get('@type') == 'Enum',
    !,
    enum_value_schema(Frame.'@values', JSON).
class_frame_to_json_schema(TO, Schema, Prefixes, Depth, Frame, Class_Ex, JSON) :-
    class_object_to_json_schema(TO, Schema, Prefixes, Depth, Frame, Class_Ex, JSON).

% enum_value_schema(+Values, -JSON) is det.
% Zebra EnumtraitValues shape: oneOf of single-value enums so each
% choice carries a title for generated UIs.
enum_value_schema(Values, JSON) :-
    findall(json{type:"string", enum:[Value], title:Title},
            (   member(Value, Values),
                atom_string(Value, Title)
            ),
            Alternatives),
    JSON = json{type:"string",
                title:"Select value from enum trait",
                oneOf:Alternatives}.

class_object_to_json_schema(TO, Schema, Prefixes, Depth, Frame, Class_Ex, JSON) :-
    dict_pairs(Frame, _, Pairs),
    compress_schema_uri(Class_Ex, Prefixes, Class_Comp, [compress_ids(true)]),
    (   get_dict('@oneOf', Frame, OneOf)
    ->  (   is_list(OneOf)
        ->  OneOf_List = OneOf
        ;   OneOf_List = [OneOf]),
        oneof_groups(TO, Schema, Prefixes, Depth, OneOf_List, Union_Pairs, OneOf_Groups),
        pairs_keys(Union_Pairs, Union_Props)
    ;   Union_Pairs = [],
        Union_Props = [],
        OneOf_Groups = _),
    property_documentation(Frame, Doc_Properties),
    findall(Prop-Prop_Schema,
            (   member(Prop-Value, Pairs),
                \+ has_at(Prop),
                property_to_json_schema(TO, Schema, Prefixes, Depth, Value, Prop_Schema0),
                property_title(Doc_Properties, Prop, Prop_Schema0, Prop_Schema)
            ),
            Plain_Pairs),
    append(Plain_Pairs, Union_Pairs, Property_Pairs0),
    sort(Property_Pairs0, Property_Pairs),
    dict_pairs(Properties0, json, Property_Pairs),
    findall(Req,
            (   member(Req-Value, Pairs),
                \+ has_at(Req),
                \+ memberchk(Req, Union_Props),
                is_required_property(Value)
            ),
            Required0),
    % @type and @id are always permitted as keys and may carry any
    % value; they are optional so generated documents need not set
    % them explicitly. The acceptable @type choices and the @id
    % pattern are preserved under 'x-terminusdb'.
    put_dict('@type', Properties0, json{type:"string"}, Properties1),
    put_dict('@id', Properties1, json{type:"string"}, Properties),
    sort(Required0, Required),
    JSON0 = json{
                type : "object",
                properties : Properties,
                required : Required,
                additionalProperties : false
            },
    % A single @oneOf group is a plain oneOf; several groups mean at
    % least one member of each satisfied group, i.e. an anyOf of
    % per-group oneOfs (the Zebra deriveOneOfProperties mapping).
    (   var(OneOf_Groups)
    ->  JSON1 = JSON0
    ;   OneOf_Groups = [Single_Group]
    ->  JSON1 = JSON0.put(oneOf, Single_Group)
    ;   findall(json{oneOf:Group}, member(Group, OneOf_Groups), AnyOf),
        JSON1 = JSON0.put(anyOf, AnyOf)
    ),
    class_extensions(Frame, Schema, Prefixes, Class_Ex, Class_Comp, JSON1, JSON2),
    documentation_fields(Frame, JSON2, JSON).

% class_extensions(+Frame, +Schema, +Prefixes, +Class_Ex, +Class_Comp, +JSON0, -JSON) is det.
% Copies class-level TerminusDB constructs into 'x-terminusdb' and
% notes the constructs JSON Schema cannot express.
class_extensions(Frame, Schema, Prefixes, Class_Ex, Class_Comp, JSON0, JSON) :-
    findall(Key-Value,
            class_extension_key(Frame, Key, Value),
            Ext_Pairs0),
    (   schema_is_abstract(Schema, Class_Ex)
    ->  Candidates = []
    ;   atom_string(Class_Comp, Class_Str),
        Candidates = [Class_Str]),
    findall(Sub_Str,
            (   schema_concrete_subclass(Schema, Class_Ex, Sub_Ex),
                compress_schema_uri(Sub_Ex, Prefixes, Sub_Comp, [compress_ids(true)]),
                atom_string(Sub_Comp, Sub_Str)
            ),
            Subs),
    append(Candidates, Subs, Choices0),
    (   Choices0 = []
    ->  Ext_Pairs1 = Ext_Pairs0
    ;   sort(Choices0, Choices),
        Ext_Pairs1 = [type_choices-Choices|Ext_Pairs0]),
    atom_string(Class_Comp, Pattern_Class),
    atomic_list_concat(
        ["^", Pattern_Class, "/[A-Za-zÀ-῿Ⰰ-�0-9\\-_.!?%()]+$"],
        '', ID_Pattern),
    Ext_Pairs2 = [id_pattern-ID_Pattern|Ext_Pairs1],
    (   get_dict('@key', Frame, _)
    ->  Ext_Pairs = [limitations-["class keys are not expressible as JSON Schema; see 'x-terminusdb'.key"]|Ext_Pairs2]
    ;   Ext_Pairs = Ext_Pairs2),
    dict_pairs(Ext_Dict, json, Ext_Pairs),
    JSON = JSON0.put('x-terminusdb', Ext_Dict).

class_extension_key(Frame, key, Key) :-
    get_dict('@key', Frame, Key).
class_extension_key(Frame, abstract, true) :-
    get_dict('@abstract', Frame, _).
class_extension_key(Frame, subdocument, true) :-
    get_dict('@subdocument', Frame, _).
class_extension_key(Frame, inherits, Inherits) :-
    get_dict('@inherits', Frame, Inherits).
class_extension_key(Frame, unfoldable, true) :-
    get_dict('@unfoldable', Frame, _).
class_extension_key(Frame, shared, true) :-
    get_dict('@shared', Frame, _).
class_extension_key(Frame, metadata, Metadata) :-
    get_dict('@metadata', Frame, Metadata).

% is_required_property(+FrameValue) is semidet.
% TerminusDB requires every non-Optional property to be present in
% the document; Optional is the only excluded wrapper.
is_required_property(Value) :-
    (   is_dict(Value)
    ->  (   get_dict('@type', Value, Type)
        ->  normalize_system_type(Type, Type_Bare),
            Type_Bare \= 'Optional'
        ;   true)
    ;   true).

% property_to_json_schema(+TO, +Schema, +Prefixes, +Depth, +FrameValue, -JSON) is det.
property_to_json_schema(TO, Schema, Prefixes, Depth, Value, JSON) :-
    (   is_dict(Value)
    ->  wrapper_to_json_schema(TO, Schema, Prefixes, Depth, Value, JSON0),
        unfold_extension(Value, JSON0, JSON)
    ;   is_list(Value)
    ->  maplist(property_to_json_schema(TO, Schema, Prefixes, Depth), Value, Alts),
        (   Alts = [Single]
        ->  JSON = Single
        ;   JSON = json{anyOf:Alts}
        )
    ;   scalar_type_schema(Value, JSON)
    ).

% wrapper_to_json_schema(+TO, +Schema, +Prefixes, +Depth, +Dict, -JSON) is det.
wrapper_to_json_schema(TO, Schema, Prefixes, Depth, Value, JSON) :-
    (   get_dict('@type', Value, Type)
    ->  normalize_system_type(Type, Type_Bare),
        system_type_schema(Type_Bare, TO, Schema, Prefixes, Depth, Value, JSON)
    ;   get_dict('@subdocument', Value, []),
        get_dict('@class', Value, Class_Comp)
    ->  subdocument_schema(TO, Schema, Prefixes, Depth, Class_Comp, JSON)
    ;   JSON = json{'x-terminusdb':json{limitations:
                                        ["unrecognized property shape"]}}
    ).

% unfold_extension(+FrameValue, +JSON0, -JSON) is det.
% Field-level @unfold survives as an x-terminusdb extension.
unfold_extension(Value, JSON0, JSON) :-
    (   get_dict('@unfold', Value, true)
    ->  (   get_dict('x-terminusdb', JSON0, Ext0)
        ->  Ext = Ext0.put(unfold, true)
        ;   Ext = json{unfold:true}),
        JSON = JSON0.put('x-terminusdb', Ext)
    ;   JSON = JSON0).

% normalize_system_type(+Type0, -Type) is det.
% Frame wrappers arrive bare ('Set') or prefixed ('sys:Set').
normalize_system_type(Type0, Type) :-
    atom_string(Type0, Type_Str),
    (   sub_string(Type_Str, 0, _, Rest, "sys:")
    ->  sub_string(Type_Str, 4, Rest, 0, Type_Str1),
        atom_string(Type, Type_Str1)
    ;   Type = Type0).

system_type_schema('Optional', TO, Schema, Prefixes, Depth, Value, JSON) :-
    !,
    property_to_json_schema(TO, Schema, Prefixes, Depth, Value.'@class', JSON).
system_type_schema('Set', TO, Schema, Prefixes, Depth, Value, JSON) :-
    !,
    property_to_json_schema(TO, Schema, Prefixes, Depth, Value.'@class', Items),
    JSON0 = json{type:"array", uniqueItems:true, items:Items},
    cardinality_bounds(Value, JSON0, JSON).
system_type_schema('List', TO, Schema, Prefixes, Depth, Value, JSON) :-
    !,
    property_to_json_schema(TO, Schema, Prefixes, Depth, Value.'@class', Items),
    JSON = json{type:"array", items:Items}.
system_type_schema('Array', TO, Schema, Prefixes, Depth, Value, JSON) :-
    !,
    property_to_json_schema(TO, Schema, Prefixes, Depth, Value.'@class', Items),
    (   get_dict('@dimensions', Value, Dim),
        nonvar(Dim)
    ->  true
    ;   Dim = 1),
    array_wrap(Dim, Items, Wrapped),
    JSON = json{type:"array",
                items:Wrapped,
                'x-terminusdb':json{array_dimensions:Dim}}.
system_type_schema('Cardinality', TO, Schema, Prefixes, Depth, Value, JSON) :-
    !,
    property_to_json_schema(TO, Schema, Prefixes, Depth, Value.'@class', Items),
    JSON0 = json{type:"array", items:Items},
    cardinality_bounds(Value, JSON0, JSON).
system_type_schema('Enum', _TO, _Schema, _Prefixes, _Depth, Value, JSON) :-
    !,
    % Zebra linkedEnum: reference the enum's EnumtraitValues entry
    atom_string(Value.'@id', Enum_Str),
    atomic_list_concat(["#/definitions/EnumtraitValues/", Enum_Str], Ref),
    JSON = json{'$ref':Ref}.
system_type_schema('Foreign', _TO, _Schema, _Prefixes, _Depth, Value, JSON) :-
    !,
    atom_string(Value.'@class', Foreign_Str),
    JSON = json{type:"string",
                'x-terminusdb':json{foreign:Foreign_Str}}.
system_type_schema(_Type, TO, Schema, Prefixes, Depth, Value, JSON) :-
    % Any other dict carrying @class is a link or embedded
    % subdocument; a dict without @class is an unknown shape.
    (   get_dict('@subdocument', Value, []),
        get_dict('@class', Value, Class_Comp)
    ->  subdocument_schema(TO, Schema, Prefixes, Depth, Class_Comp, JSON)
    ;   get_dict('@class', Value, Class_Comp)
    ->  link_schema(Class_Comp, JSON)
    ;   JSON = json{'x-terminusdb':json{limitations:
                                        ["unrecognized property shape"]}}
    ).

% array_wrap(+Dim, +Items, -Wrapped) is det.
% An N-dimensional array is N levels of nested arrays.
array_wrap(Dim, Items, Wrapped) :-
    (   Dim =< 1
    ->  Wrapped = Items
    ;   Dim1 is Dim - 1,
        array_wrap(Dim1, Items, Inner),
        Wrapped = json{type:"array", items:Inner}).

cardinality_bounds(Value, JSON0, JSON) :-
    (   get_dict('@min', Value, Min),
        nonvar(Min)
    ->  JSON1 = JSON0.put(minItems, Min)
    ;   JSON1 = JSON0),
    (   get_dict('@max', Value, Max),
        nonvar(Max)
    ->  JSON = JSON1.put(maxItems, Max)
    ;   JSON = JSON1).

% link_schema(+Class_Comp, -JSON) is det.
% Zebra linkedType: linked documents appear as their @id string.
link_schema(Class_Comp, JSON) :-
    atom_string(Class_Comp, Class_Str),
    JSON = json{type:"string",
                'x-terminusdb':json{link:Class_Str}}.

% subdocument_schema(+TO, +Schema, +Prefixes, +Depth, +Class_Comp, -JSON) is det.
% Subdocuments inline until depth 2; beyond that a definition
% reference is emitted (the definition is materialized by
% resolve_definitions/5 at the top level).
subdocument_schema(TO, Schema, Prefixes, Depth, Class_Comp, JSON) :-
    (   Depth >= 2
    ->  atom_string(Class_Comp, Class_Str),
        atomic_list_concat(["#/definitions/", Class_Str], Ref),
        JSON = json{'$ref':Ref}
    ;   prefix_expand_schema(Class_Comp, Prefixes, Class_Ex),
        class_frame(TO, Class_Ex, Frame),
        Depth1 is Depth + 1,
        class_frame_to_json_schema(TO, Schema, Prefixes, Depth1, Frame, Class_Ex, JSON)
    ).

% oneof_groups(+TO, +Schema, +Prefixes, +Depth, +OneOf, -Pairs, -Groups) is det.
% @oneOf is a set of disjoint property maps; each map is one group
% whose members are mutually exclusive alternatives (mirroring the
% Zebra mapping). Alternatives also join the property table so
% additionalProperties stays false.
oneof_groups(TO, Schema, Prefixes, Depth, OneOf, Pairs, Groups) :-
    findall(Prop-Inner,
            (   member(Map, OneOf),
                dict_pairs(Map, _, Map_Pairs),
                member(Prop-Value, Map_Pairs),
                property_to_json_schema(TO, Schema, Prefixes, Depth, Value, Inner)
            ),
            Pairs0),
    sort(Pairs0, Pairs),
    findall(Alts,
            (   member(Map, OneOf),
                findall(Alt,
                        (   dict_pairs(Map, _, Map_Pairs),
                            member(Prop-_Value, Map_Pairs),
                            memberchk(Prop-Inner, Pairs),
                            dict_pairs(Props, json, [Prop-Inner]),
                            Alt = json{properties:Props, required:[Prop]}
                        ),
                        Alts)
            ),
            Groups).

% property_documentation(+Frame, -Doc_Properties) is det.
% Per-property labels live under @documentation.@properties; absent
% or undocumented properties yield an empty map.
property_documentation(Frame, Doc_Properties) :-
    (   get_dict('@documentation', Frame, Docs),
        (   is_list(Docs)
        ->  Docs = [Doc|_]
        ;   Doc = Docs),
        is_dict(Doc),
        get_dict('@properties', Doc, Doc_Properties)
    ->  true
    ;   Doc_Properties = json{}).

% property_title(+Doc_Properties, +Prop, +JSON0, -JSON) is det.
property_title(Doc_Properties, Prop, JSON0, JSON) :-
    (   get_dict(Prop, Doc_Properties, Doc),
        is_dict(Doc),
        get_dict('@label', Doc, Label)
    ->  JSON = JSON0.put(title, Label)
    ;   JSON = JSON0).

% scalar_type_schema(+Type_Comp, -JSON) is det.
% Compressed scalar/base type names to JSON Schema primitives.
scalar_type_schema(Type_Comp, JSON) :-
    atom_string(Type_Comp, Type_Str),
    (   memberchk(Type_Str, ["sys:Unit","Unit"])
    ->  JSON = json{'x-terminusdb':json{unit:true}}
    ;   sub_string(Type_Str, 0, _, _, "sys:JSON")
    ->  JSON = json{}
    ;   xsd_json_schema(Type_Comp, Type_Str, JSON)
    ).

xsd_json_schema(_, "xsd:string", json{type:"string"}) :- !.
xsd_json_schema(_, "xsd:normalizedString", json{type:"string"}) :- !.
xsd_json_schema(_, "xsd:token", json{type:"string"}) :- !.
xsd_json_schema(_, "xsd:language", json{type:"string"}) :- !.
xsd_json_schema(_, "xsd:Name", json{type:"string"}) :- !.
xsd_json_schema(_, "xsd:NCName", json{type:"string"}) :- !.
xsd_json_schema(_, "xsd:NMTOKEN", json{type:"string"}) :- !.
xsd_json_schema(_, "xsd:boolean", json{type:"boolean"}) :- !.
xsd_json_schema(_, "xsd:decimal", json{type:"number"}) :- !.
xsd_json_schema(_, "xsd:double", json{type:"number"}) :- !.
xsd_json_schema(_, "xsd:float", json{type:"number"}) :- !.
xsd_json_schema(_, "xsd:integer", json{type:"integer"}) :- !.
xsd_json_schema(_, "xsd:long", json{type:"integer"}) :- !.
xsd_json_schema(_, "xsd:int", json{type:"integer"}) :- !.
xsd_json_schema(_, "xsd:short", json{type:"integer"}) :- !.
xsd_json_schema(_, "xsd:byte", json{type:"integer"}) :- !.
xsd_json_schema(_, "xsd:nonNegativeInteger", json{type:"integer", minimum:0}) :- !.
xsd_json_schema(_, "xsd:nonPositiveInteger", json{type:"integer", maximum:0}) :- !.
xsd_json_schema(_, "xsd:positiveInteger", json{type:"integer", minimum:1}) :- !.
xsd_json_schema(_, "xsd:negativeInteger", json{type:"integer", maximum:0}) :- !.
xsd_json_schema(_, "xsd:unsignedLong", json{type:"integer", minimum:0}) :- !.
xsd_json_schema(_, "xsd:unsignedInt", json{type:"integer", minimum:0}) :- !.
xsd_json_schema(_, "xsd:unsignedShort", json{type:"integer", minimum:0}) :- !.
xsd_json_schema(_, "xsd:unsignedByte", json{type:"integer", minimum:0, maximum:255}) :- !.
xsd_json_schema(_, "xsd:date", json{type:"string", format:"date"}) :- !.
xsd_json_schema(_, "xsd:dateTime", json{type:"string", format:"date-time"}) :- !.
xsd_json_schema(_, "xsd:dateTimeStamp", json{type:"string", format:"date-time"}) :- !.
xsd_json_schema(_, "xsd:time", json{type:"string", format:"time"}) :- !.
xsd_json_schema(_, "xsd:duration", json{type:"string",
                                       'x-terminusdb':json{xsd_type:"xsd:duration"}}) :- !.
xsd_json_schema(_, "xsd:gYear", json{type:"string",
                                    'x-terminusdb':json{xsd_type:"xsd:gYear"}}) :- !.
xsd_json_schema(_, "xsd:gMonth", json{type:"string",
                                     'x-terminusdb':json{xsd_type:"xsd:gMonth"}}) :- !.
xsd_json_schema(_, "xsd:gDay", json{type:"string",
                                   'x-terminusdb':json{xsd_type:"xsd:gDay"}}) :- !.
xsd_json_schema(_, "xsd:gYearMonth", json{type:"string",
                                         'x-terminusdb':json{xsd_type:"xsd:gYearMonth"}}) :- !.
xsd_json_schema(_, "xsd:gMonthDay", json{type:"string",
                                        'x-terminusdb':json{xsd_type:"xsd:gMonthDay"}}) :- !.
xsd_json_schema(_, "xsd:anyURI", json{type:"string", format:"uri"}) :- !.
xsd_json_schema(_, "xsd:base64Binary", json{type:"string", contentEncoding:"base64"}) :- !.
xsd_json_schema(_, "xsd:hexBinary", json{type:"string", contentEncoding:"hex"}) :- !.
xsd_json_schema(_, "xsd:anySimpleType", json{}) :- !.
xsd_json_schema(_, "xsd:anyType", json{}) :- !.
% Anything not xsd-prefixed is a class link or an unknown type.
xsd_json_schema(Type_Comp, Type_Str, JSON) :-
    (   sub_string(Type_Str, 0, _, _, "xsd:")
    ->  JSON = json{'x-terminusdb':json{xsd_type:Type_Str,
                                        limitations:["unmapped xsd type"]}}
    ;   link_schema(Type_Comp, JSON)
    ).

% documentation_fields(+Frame, +JSON0, -JSON) is det.
% First @documentation entry supplies title/description.
documentation_fields(Frame, JSON0, JSON) :-
    (   get_dict('@documentation', Frame, Docs),
        (   is_list(Docs)
        ->  Docs = [Doc|_]
        ;   Doc = Docs),
        is_dict(Doc)
    ->  (   get_dict('@label', Doc, Label)
        ->  JSON1 = JSON0.put(title, Label)
        ;   JSON1 = JSON0),
        (   get_dict('@comment', Doc, Comment)
        ->  JSON = JSON1.put(description, Comment)
        ;   JSON = JSON1)
    ;   JSON = JSON0).

/* Definition resolution: every emitted {"$ref":"#/definitions/..."}
 * is materialized under 'definitions'. Enum references live in
 * 'definitions'.EnumtraitValues, class references at top level.
 * Generation is a fixpoint loop since a generated definition can
 * itself emit new references; each name is generated at most once.
 */

% resolve_definitions(+TO, +Schema, +Prefixes, +JSON0, -JSON) is det.
resolve_definitions(TO, Schema, Prefixes, JSON0, JSON) :-
    (   get_dict(definitions, JSON0, Defs0)
    ->  true
    ;   Defs0 = json{}),
    json_refs(JSON0, Refs),
    gen_definitions(TO, Schema, Prefixes, Refs, Defs0, Defs),
    (   dict_pairs(Defs, _, [])
    ->  (   get_dict(definitions, JSON0, _)
        ->  JSON = JSON0.put(definitions, Defs0)
        ;   JSON = JSON0)
    ;   JSON = JSON0.put(definitions, Defs)).

gen_definitions(_TO, _Schema, _Prefixes, [], Defs, Defs).
gen_definitions(TO, Schema, Prefixes, [Ref|Rest], Defs0, Defs) :-
    (   definition_target(Ref, Kind, Name),
        \+ definition_present(Defs0, Kind, Name)
    ->  make_definition(TO, Schema, Prefixes, Kind, Name, Def),
        put_definition(Kind, Def, Defs0, Defs1),
        json_refs(Def, More),
        append(Rest, More, Work)
    ;   Work = Rest,
        Defs1 = Defs0),
    gen_definitions(TO, Schema, Prefixes, Work, Defs1, Defs).

% definition_target(+Ref, -Kind, -Name) is semidet.
% Kind is class or enumtrait.
definition_target(Ref, enumtrait(Name), Name) :-
    atom_string(Ref, Ref_Str),
    sub_string(Ref_Str, 0, _, Rest, "#/definitions/EnumtraitValues/"),
    sub_string(Ref_Str, _, Rest, 0, Name).
definition_target(Ref, class(Name), Name) :-
    atom_string(Ref, Ref_Str),
    sub_string(Ref_Str, 0, _, Rest, "#/definitions/"),
    sub_string(Ref_Str, _, Rest, 0, Name),
    \+ sub_string(Name, _, _, _, "/").

definition_present(Defs, class(Name), _) :-
    atom_string(Name_Atom, Name),
    get_dict(Name_Atom, Defs, _),
    !.
definition_present(Defs, enumtrait(Name), _) :-
    get_dict('EnumtraitValues', Defs, Traits),
    atom_string(Name_Atom, Name),
    get_dict(Name_Atom, Traits, _),
    !.

put_definition(enumtrait(Name), Def, Defs0, Defs) :-
    (   get_dict('EnumtraitValues', Defs0, Traits0)
    ->  true
    ;   Traits0 = json{}),
    atom_string(Name_Atom, Name),
    put_dict(Name_Atom, Traits0, Def, Traits),
    Defs = Defs0.put('EnumtraitValues', Traits).
put_definition(class(Name), Def, Defs0, Defs) :-
    atom_string(Name_Atom, Name),
    put_dict(Name_Atom, Defs0, Def, Defs).

make_definition(TO, Schema, Prefixes, class(Name), _Name, Def) :-
    prefix_expand_schema(Name, Prefixes, Class_Ex),
    class_frame(TO, Class_Ex, Frame),
    class_frame_to_json_schema(TO, Schema, Prefixes, 0, Frame, Class_Ex, Def).
make_definition(TO, _Schema, Prefixes, enumtrait(Name), _Name, Def) :-
    prefix_expand_schema(Name, Prefixes, Class_Ex),
    class_frame(TO, Class_Ex, Frame),
    enum_value_schema(Frame.'@values', Def).

% json_refs(+Term, -Refs) is det.
% Collects every "$ref" string in a nested JSON structure.
json_refs(Term, Refs) :-
    findall(Ref, json_ref(Term, Ref), Refs).

json_ref(Term, Ref) :-
    is_dict(Term),
    dict_pairs(Term, _, Pairs),
    member(Key-Value, Pairs),
    (   Key == '$ref'
    ->  Value = Ref
    ;   json_ref(Value, Ref)).
json_ref(Term, Ref) :-
    is_list(Term),
    member(Elem, Term),
    json_ref(Elem, Ref).

:- begin_tests(jsonschema).
:- use_module(core(util/test_utils)).

jsonschema_test_schema('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context",
  "xsd" : "http://www.w3.org/2001/XMLSchema#" }

{ "@id" : "JsAddress",
  "@type" : "Class",
  "@key" : {"@type" : "Lexical", "@fields" : ["street", "postcode"]},
  "@documentation" : { "@comment" : "An address",
                       "@label" : "Address",
                       "@properties" : {"street" : {"@label" : "Street"}}},
  "street" : "xsd:string",
  "postcode" : {"@type" : "Optional", "@class" : "xsd:string"},
  "aliases" : {"@type" : "Set", "@class" : "xsd:string"},
  "history" : {"@type" : "List", "@class" : "xsd:string"},
  "lucky" : {"@type" : "Cardinality", "@class" : "xsd:integer", "@min_cardinality" : 1, "@max_cardinality" : 3} }

{ "@id" : "JsColor",
  "@type" : "Enum",
  "@value" : ["red", "green", "blue"] }

{ "@id" : "JsLine",
  "@type" : "Class",
  "@subdocument" : [],
  "@key" : {"@type" : "ValueHash"},
  "color" : "JsColor",
  "length" : "xsd:decimal" }

{ "@id" : "JsCountry",
  "@type" : "Class",
  "@abstract" : [],
  "name" : "xsd:string" }

{ "@id" : "JsEUCountry",
  "@type" : "Class",
  "@inherits" : ["JsCountry"],
  "eu_zone" : "xsd:boolean" }

{ "@id" : "JsExtDoc",
  "@type" : "Foreign" }

{ "@id" : "JsPerson",
  "@type" : "Class",
  "name" : "xsd:string",
  "best_color" : "JsColor",
  "home" : "JsLine",
  "address" : "JsAddress",
  "residence" : {"@type" : "Optional", "@class" : "JsAddress"},
  "country" : "JsCountry",
  "friend" : "JsExtDoc" }

{ "@id" : "JsUnion",
  "@type" : "TaggedUnion",
  "left" : "xsd:string",
  "right" : "xsd:integer" }
').

setup_jsonschema_desc(Desc) :-
    test_document_label_descriptor(jsonschema, Desc),
    write_schema(jsonschema_test_schema, Desc).

test(simple_class,
     [setup((setup_temp_store(State),
             setup_jsonschema_desc(Desc))),
      cleanup(teardown_temp_store(State))]) :-
    tdb_class_to_json_schema(Desc, 'JsAddress', JSON),
    assertion(JSON.'$schema' == "https://json-schema.org/draft-07/schema#"),
    assertion(JSON.type == "object"),
    assertion(JSON.title == "Address"),
    assertion(JSON.description == "An address"),
    assertion(JSON.additionalProperties == false),
    % mandatory property with documentation-derived title
    assertion(JSON.properties.street ==
              json{type:"string", title:"Street"}),
    % optional property -> schema present, not required
    assertion(JSON.properties.postcode == json{type:"string"}),
    % set -> unique array
    assertion(JSON.properties.aliases ==
              json{type:"array", uniqueItems:true,
                   items:json{type:"string"}}),
    % list -> array preserving order
    assertion(JSON.properties.history ==
              json{type:"array", items:json{type:"string"}}),
    % cardinality -> array with bounds
    assertion(JSON.properties.lucky ==
              json{type:"array", minItems:1, maxItems:3,
                   items:json{type:"integer"}}),
    % required: every non-Optional property; @type and @id are optional
    assertion(JSON.required == [aliases,history,lucky,street]),
    % @type and @id accept arbitrary string values
    assertion(JSON.properties.'@type' == json{type:"string"}),
    assertion(JSON.properties.'@id' == json{type:"string"}),
    % the constrained forms survive under x-terminusdb
    Type_Choices = JSON.'x-terminusdb'.type_choices,
    assertion(memberchk("JsAddress", Type_Choices)),
    ID_Pattern = JSON.'x-terminusdb'.id_pattern,
    assertion(sub_string(ID_Pattern, 0, _, _, "^JsAddress/")),
    % key recorded as extension with a fidelity limitation
    Key = JSON.'x-terminusdb'.key,
    Limitations = JSON.'x-terminusdb'.limitations,
    assertion(get_dict('@type', Key, "Lexical")),
    assertion(memberchk(_, Limitations)).

test(enum_class,
     [setup((setup_temp_store(State),
             setup_jsonschema_desc(Desc))),
      cleanup(teardown_temp_store(State))]) :-
    tdb_class_to_json_schema(Desc, 'JsColor', JSON),
    % Zebra enum shape: oneOf of titled single-value choices
    assertion(JSON.type == "string"),
    OneOf = JSON.oneOf,
    assertion(length(OneOf, 3)),
    assertion(( member(Alt, OneOf),
                get_dict(enum, Alt, [red]),
                get_dict(title, Alt, "red") )).

test(enum_property_and_links,
     [setup((setup_temp_store(State),
             setup_jsonschema_desc(Desc))),
      cleanup(teardown_temp_store(State))]) :-
    tdb_class_to_json_schema(Desc, 'JsPerson', JSON),
    % enum property -> $ref into definitions.EnumtraitValues (Zebra)
    assertion(JSON.properties.best_color ==
              json{'$ref':'#/definitions/EnumtraitValues/JsColor'}),
    Defs = JSON.definitions,
    Color_Def = Defs.'EnumtraitValues'.'JsColor',
    assertion(Color_Def.type == "string"),
    Color_OneOf = Color_Def.oneOf,
    assertion(length(Color_OneOf, 3)),
    % ordinary class link -> string with link extension
    assertion(JSON.properties.address.type == "string"),
    assertion(JSON.properties.address.'x-terminusdb'.link == "JsAddress"),
    % foreign link -> string with foreign extension
    assertion(JSON.properties.friend.type == "string"),
    assertion(JSON.properties.friend.'x-terminusdb'.foreign == "JsExtDoc"),
    % optional link -> present but not required
    Required = JSON.required,
    assertion(\+ memberchk(residence, Required)),
    assertion(JSON.properties.residence.type == "string"),
    % abstract class link -> concrete subclass reference
    assertion(JSON.properties.country.type == "string"),
    assertion(JSON.properties.country.'x-terminusdb'.link == "JsEUCountry"),
    % mandatory props required
    assertion(memberchk(name, Required)),
    assertion(memberchk(country, Required)),
    assertion(memberchk(home, Required)).

test(subdocument_property,
     [setup((setup_temp_store(State),
             setup_jsonschema_desc(Desc))),
      cleanup(teardown_temp_store(State))]) :-
    tdb_class_to_json_schema(Desc, 'JsPerson', JSON),
    Home = JSON.properties.home,
    % embedded subdocument schema inlines the class object at depth 1
    assertion(Home.type == "object"),
    Color_Ref = Home.properties.color,
    assertion(Color_Ref == json{'$ref':'#/definitions/EnumtraitValues/JsColor'}),
    assertion(Home.'x-terminusdb'.subdocument == true).

test(inherited_class,
     [setup((setup_temp_store(State),
             setup_jsonschema_desc(Desc))),
      cleanup(teardown_temp_store(State))]) :-
    tdb_class_to_json_schema(Desc, 'JsEUCountry', JSON),
    % inherited properties merged into the frame
    assertion(JSON.properties.name == json{type:"string"}),
    assertion(JSON.properties.eu_zone == json{type:"boolean"}),
    Inherits = JSON.'x-terminusdb'.inherits,
    assertion(memberchk('JsCountry', Inherits)),
    % concrete class: @type choices carry just itself
    assertion(JSON.'x-terminusdb'.type_choices == ["JsEUCountry"]).

test(abstract_class_type_choices,
     [setup((setup_temp_store(State),
             setup_jsonschema_desc(Desc))),
      cleanup(teardown_temp_store(State))]) :-
    tdb_class_to_json_schema(Desc, 'JsCountry', JSON),
    assertion(JSON.'x-terminusdb'.abstract == true),
    % abstract class: acceptable @type values are its subclasses
    assertion(JSON.'x-terminusdb'.type_choices == ["JsEUCountry"]).

test(tagged_union,
     [setup((setup_temp_store(State),
             setup_jsonschema_desc(Desc))),
      cleanup(teardown_temp_store(State))]) :-
    tdb_class_to_json_schema(Desc, 'JsUnion', JSON),
    % union alternatives appear as properties plus a oneOf
    assertion(JSON.properties.left == json{type:"string"}),
    assertion(JSON.properties.right == json{type:"integer"}),
    OneOf = JSON.oneOf,
    assertion(( member(Alt_L, OneOf), get_dict(required, Alt_L, [left]) )),
    assertion(( member(Alt_R, OneOf), get_dict(required, Alt_R, [right]) )),
    % union props are alternatives, not required keys
    Required = JSON.required,
    assertion(Required == []).

test(class_oneof_multiple_groups,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(jsonschema_oneof, Desc),
             write_schema(jsonschema_oneof_schema, Desc))),
      cleanup(teardown_temp_store(State))]) :-
    tdb_class_to_json_schema(Desc, 'JsPet', JSON),
    % several @oneOf groups: anyOf of per-group oneOfs, so a document
    % may carry one member of each group
    AnyOf = JSON.anyOf,
    assertion(length(AnyOf, 2)),
    assertion(( member(G1, AnyOf),
                get_dict(oneOf, G1, Alts1),
                member(Alt, Alts1),
                get_dict(required, Alt, [cat]) )),
    assertion(( member(G2, AnyOf),
                get_dict(oneOf, G2, Alts2),
                member(Alt, Alts2),
                get_dict(required, Alt, [employers]) )),
    % union alternatives still join the property table
    assertion(JSON.properties.cat == json{type:"string"}),
    assertion(JSON.properties.employers == json{type:"integer", minimum:1}).

jsonschema_oneof_schema('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context",
  "xsd" : "http://www.w3.org/2001/XMLSchema#" }

{ "@id" : "JsPet",
  "@type" : "Class",
  "name" : "xsd:string",
  "@oneOf" : [ { "cat" : "xsd:string", "dog" : "xsd:string" },
               { "employers" : "xsd:positiveInteger",
                 "unemployed" : "xsd:string" } ] }
').

test(choice_subdocuments,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(jsonschema_choice, Desc),
             write_schema(jsonschema_choice_schema, Desc))),
      cleanup(teardown_temp_store(State))]) :-
    tdb_class_to_json_schema(Desc, 'JsHouse', JSON),
    % a property over an abstract class with several concrete
    % subdocument subclasses is an anyOf over the alternatives
    Pet = JSON.properties.pet,
    AnyOf = Pet.anyOf,
    assertion(length(AnyOf, 2)),
    assertion(( member(Alt, AnyOf), get_dict(properties, Alt, Props),
                get_dict(lives, Props, _) )),
    assertion(( member(Alt2, AnyOf), get_dict(properties, Alt2, Props2),
                get_dict(barks, Props2, _) )).

jsonschema_choice_schema('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context",
  "xsd" : "http://www.w3.org/2001/XMLSchema#" }

{ "@id" : "JsCritter",
  "@type" : "Class",
  "@abstract" : [],
  "@subdocument" : [],
  "@key" : {"@type" : "ValueHash"},
  "petname" : "xsd:string" }

{ "@id" : "JsCat",
  "@type" : "Class",
  "@inherits" : ["JsCritter"],
  "lives" : "xsd:integer" }

{ "@id" : "JsDog",
  "@type" : "Class",
  "@inherits" : ["JsCritter"],
  "barks" : "xsd:boolean" }

{ "@id" : "JsHouse",
  "@type" : "Class",
  "@key" : {"@type" : "Random"},
  "pet" : "JsCritter" }
').

test(subdocument_depth_reference,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(jsonschema_rec, Desc),
             write_schema(jsonschema_recursive_schema, Desc))),
      cleanup(teardown_temp_store(State))]) :-
    once(tdb_class_to_json_schema(Desc, 'JsNode', JSON)),
    % depth 1 and 2 inline; depth 3 becomes a definitions reference
    Next1 = JSON.properties.next,
    assertion(Next1.type == "object"),
    Next2 = Next1.properties.next,
    assertion(Next2.type == "object"),
    assertion(Next2.properties.next ==
              json{'$ref':'#/definitions/JsNode'}),
    % the referenced class is materialized under definitions
    Def = JSON.definitions.'JsNode',
    assertion(Def.type == "object").

jsonschema_recursive_schema('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context",
  "xsd" : "http://www.w3.org/2001/XMLSchema#" }

{ "@id" : "JsNode",
  "@type" : "Class",
  "@subdocument" : [],
  "@key" : {"@type" : "ValueHash"},
  "label" : "xsd:string",
  "next" : {"@type" : "Optional", "@class" : "JsNode"} }
').

test(unknown_xsd_type,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(jsonschema_odd, Desc),
             write_schema(jsonschema_unknown_xsd_schema, Desc))),
      cleanup(teardown_temp_store(State))]) :-
    tdb_class_to_json_schema(Desc, 'JsOdd', JSON),
    % unmapped xsd type degrades to a permissive schema with a note
    assertion(JSON.properties.odd.'x-terminusdb'.xsd_type == "xsd:QName"),
    Odd_Limitations = JSON.properties.odd.'x-terminusdb'.limitations,
    assertion(memberchk("unmapped xsd type", Odd_Limitations)).

jsonschema_unknown_xsd_schema('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context",
  "xsd" : "http://www.w3.org/2001/XMLSchema#" }

{ "@id" : "JsOdd",
  "@type" : "Class",
  "odd" : "xsd:QName" }
').

:- end_tests(jsonschema).
