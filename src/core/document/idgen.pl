:- module('document/idgen',
          []).

:- use_module(core(util)).
:- use_module(core(triple)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(schema).
:- use_module(json).


%% format: document_id(<component>, <parent_id>, <id>)
%%
%% parent_id: prefix(Base, Schema)
%%          | document_id
%%
%% component: random(Type_Name, Id_Fragment)
%%            | lexical(Type_Name, +Fields, -Suffix, -Id_Fragment)
%%            | hash(Type_Name, +Fields, -Suffix, -Id_Fragment)
%%            | valuehash(Type_Name, +Fields, -Suffix, -Id_Fragment)
%%            | array_index(Index, Inner_Component, Id_Fragment)
%%            | list_index(Index, Inner_Component, Id_Fragment)
%%            | set_property(Property, Inner_Component, Id_Fragment)
%%            | optional_property(Property, Inner_Component, Id_Fragment)
%%            | property(Property, Inner_Component, Id_Fragment)
%% http://asdfasdf/documents/Foo/asdfasdf/stuff/3/Bar/asdfasdfa
%% property(stuff, Inner_Component, _),
%% property(stuff, array_index(3, random(Bar, _), _), _)
%% Fields point at a data type or collection of data types.
%% It's possible that this happens in a nested way.
%%
%% field: property(Property_Name, Field_Type, Value, Value_String)
%%      | optional_property(Property_Name, Field_Type, Value, Value_String)
%%      | list_property(Property_Name, Field_Type, Values, Value_String)
%%      | set_property(Property_Name, Field_Type, Values, Value_String)
%%      | array_property(Property_Name, Field_Type, Values, Value_String)
%%
%% Hypothetical future thing, but currently field_type is just an atom for the datatype:
%% field_type: data_type(Type)
%%           | nested_field(field)

%% Given a type, we should be able to generate a stub document_id structure

document_id_component_for_type(Schema, Prefixes, Type, Type_Component) :-
    schema_key_descriptor(Schema, Prefixes, Type, Descriptor),
    key_descriptor_to_id_component(Schema, Type, Descriptor, Type_Component).

document_id_for_type(Transaction, Type, document_id(Type_Component, Prefixes, _Id)) :-
    do_or_die(\+ is_subdocument(Transaction, Type),
              error(unexpected_subdocument(Type))),

    database_schema(Transaction, Schema),
    database_schema_prefixes(Schema, Context),
    Prefixes = prefix(Base_Prefix, Schema_Prefix),
    Base_Prefix = (Context.'@base'),
    Schema_Prefix = (Context.'@schema'),

    document_id_component_for_type(Schema, Prefixes, Type, Type_Component).

key_descriptor_to_id_component(_Schema, Type, base(_), base(Type, _)) :- !.
key_descriptor_to_id_component(_Schema, Type, random(_), random(Type, _)) :- !.
key_descriptor_to_id_component(Schema, Type, lexical(_, Fields), lexical(Type, Component_Fields, _, _)) :-
    maplist(key_descriptor_field_to_component_field(Schema, Type), Fields, Component_Fields).

key_descriptor_field_to_component_field(Schema, Type, Field, Component_Field) :-
    schema_class_predicate_conjunctive_type(Schema, Type, Field, Property_Type),

    key_descriptor_field_to_component_field_(Property_Type, Schema, Field, Component_Field).

key_descriptor_field_to_component_field_(base_class(Base_Type), _Schema, Property, property(Property, Base_Type, _, _)).
key_descriptor_field_to_component_field_(optional(Type), _Schema, Property, optional_property(Property, Type, _, _)).
key_descriptor_field_to_component_field_(set(Type), _Schema, Property, set_property(Property, Type, _, _)).
key_descriptor_field_to_component_field_(list(Type), _Schema, Property, list_property(Property, Type, _, _)).
key_descriptor_field_to_component_field_(array(Type), _Schema, Property, array_property(Property, Type, _, _)).

document_prefix_parent_id(Schema, Prefixes) :-
    database_schema_prefixes(Schema, Context),
    Prefixes = prefix(Base_Prefix, Schema_Prefix),
    Base_Prefix = (Context.'@base'),
    Schema_Prefix = (Context.'@schema').

%% Given a document and a type, it should be possible to generate a list of document_id structures
document_ids_for_document(Schema, Document, Document_Ids) :-
    do_or_die(\+ schema_is_subdocument(Schema, Type),
              error(unexpected_subdocument(Type))),

    document_prefix_parent_id(Schema, Prefixes),

    document_ids_for_document(Schema, Prefixes, Document, Document_Ids, _, _).



document_ids_for_document(Schema, Parent_Document_Id, Document, Document_Ids, _, _) :-
    % If this is not a subdocument, ensure that parent document id is the prefix parent
    get_dict('@id', Document, _),
    Type = (Document.'@type'),
    \+ schema_is_subdocument(Schema, Type),
    \+ prefix(_,_) = Parent_Document_Id,
    !,
    document_prefix_parent_id(Schema, Prefixes),
    document_ids_for_document(Schema, Prefixes, Document, Document_Ids, _, _).
document_ids_for_document(Schema, Parent_Document_Id, Document, Document_Ids, _, _) :-
    get_dict('@id', Document, Id),
    !,
    Type = (Document.'@type'),
    database_schema_prefixes(Schema, Prefixes),
    document_id_component_for_type(Schema, Prefixes, Type, Id_Component),
    fill_document_id_component(Id_Component, Document),
    Document_Id = document_id(Id_Component, Parent_Document_Id, Id),

    dict_pairs(Document, _, Pairs),
    findall(Inner_Document_Ids,
            (   member(Property-Value, Pairs),
                \+ memberchk(Property, ['@id', '@type']),

                Rest = property(Property, Inner_Component, _),

                document_ids_for_document(Schema, Document_Id, Value, Inner_Document_Ids, Inner_Component, Rest)),
            Inner_Document_Ids_Lists),
    append([[Document_Id]|Inner_Document_Ids_Lists], Document_Ids).
document_ids_for_document(Transaction, Parent_Document_Id, Document, Document_Ids, Cur, Rest) :-
    get_dict('@container', Document, "@set"),
    !,
    get_dict('@value', Document, Values),

    maplist({Transaction, Parent_Document_Id, Cur, Rest}/[Value,Inner_Document_Ids]>>(
                document_ids_for_document(Transaction, Parent_Document_Id, Value, Inner_Document_Ids, Cur, Rest)),
             Values,
             Inner_Document_Ids),
    append(Inner_Document_Ids, Document_Ids).
document_ids_for_document(Transaction, Parent_Document_Id, Document, Document_Ids, Cur, Rest) :-
    get_dict('@container', Document, Container_Type),
    !,
    get_dict('@value', Document, Values),

    index_list(Values, Indexes),
    maplist({Transaction, Parent_Document_Id, Container_Type, Cur, Rest}/[Value,Index,Inner_Document_Ids]>>(
                copy_term(Cur-Rest, Template_Cur-Template_Rest),
                (   Container_Type = "@array"
                ->  Template_Cur = array_index(Index, Inner_Component, _)
                ;   Container_Type = "@list"
                ->  Template_Cur = list_index(Index, Inner_Component, _)
                ;   throw(error(unknown_container_type(Container_Type), _))),

                document_ids_for_document(Transaction, Parent_Document_Id, Value, Inner_Document_Ids, Inner_Component, Template_Rest)),
            Values,
            Indexes,
            Inner_Document_Ids_List),
    append(Inner_Document_Ids_List, Document_Ids).
document_ids_for_document(_Transaction, _Parent_Document_Id, _Document, []).

fill_document_id_component(base(_, _), _Document).
fill_document_id_component(random(_, _), _Document).
fill_document_id_component(lexical(_, Fields, _, _), Document) :-
    fill_fields(Fields, Document).
fill_document_id_component(hash(_, Fields, _, _), Document) :-
    fill_fields(Fields, Document).

fill_fields([], _Document).
fill_fields([Field|Fields], Document) :-
    fill_field(Field, Document),
    fill_fields(Fields, Document).

fill_field(property(Property, Field_Type, Value, _), Document) :-
    get_dict(Property, Document, Value_Container),
    get_dict('@value', Value_Container, Untyped_Value),
    normalize_json_value(Untyped_Value, Field_Type, Value).
fill_field(optional_property(Property, Field_Type, Value, _), Document) :-
    (   get_dict(Property, Document, Value_Container)
    ->  get_dict('@value', Value_Container, Untyped_Value),
        normalize_json_value(Untyped_Value, Field_Type, Inner_Value),
        Value = some(Inner_Value)
    ;   Value = none).
fill_field(set_property(Property, Field_Type, Values, _), Document) :-
    fill_field_with_container(Property, Field_Type, Values, Document).
fill_field(list_property(Property, Field_Type, Values, _), Document) :-
    fill_field_with_container(Property, Field_Type, Values, Document).
fill_field(array_property(Property, Field_Type, Values, _), Document) :-
    fill_field_with_container(Property, Field_Type, Values, Document).

fill_field_with_container(Property, Field_Type, Values, Document) :-
        get_dict(Property, Document, Container),
    get_dict('@value', Container, Untyped_Values),

    maplist({Field_Type}/[Value_Container, Value]>>(
                get_dict('@value', Value_Container, Untyped_Value),
                normalize_json_value(Untyped_Value, Field_Type, Value)),
            Untyped_Values,
            Values).

untyped_typecast(V, Type, Val, Val_Type) :-
    (   string(V)
    ->  typecast(V^^xsd:string,
                 Type, [], Val^^Val_Type)
    ;   atom(V),
        atom_string(V,String)
    ->  typecast(String^^xsd:string,
                 Type, [], Val^^Val_Type)
    ;   number(V)
    ->  typecast(V^^xsd:decimal,
                 Type, [], Val^^Val_Type)).

normalize_json_value(V, Type, Val) :-
    global_prefix_expand_safe(Type, TE),

    untyped_typecast(V, TE, Casted, Casted_Type),
    typecast(Casted^^Casted_Type, xsd:string, [], Val^^_).


%% if parent_component is not yet ground, we resolve that first.
resolve_document_id(Id) :-
    var(Id),
    !,
    throw(error(document_id_not_bound(Id), _)).
resolve_document_id(document_id(Component, Parent_Id, Id)) :-
    \+ ground(Parent_Id),
    !,
    resolve_document_id(Parent_Id),
    resolve_document_id(document_id(Component, Parent_Id, Id)).
resolve_document_id(document_id(Component, Parent_Id, Id)) :-
    schema_prefix_from_document_id(Parent_Id, Schema_Prefix),
    resolve_document_id_component(Component, Schema_Prefix),
    combine_fragments(Parent_Id, Component, Id).
resolve_document_id(prefix(_,_)) :-
    % we got here cause prefix was not ground, which is an error
    throw(error(prefix_unknown_while_generating_id, _)).

resolve_document_id_propertied_component(Schema_Prefix, Property, Inner_Component, Id_Fragment) :-
    \+ ground(Inner_Component),
    !,
    resolve_document_id_component(Inner_Component, Schema_Prefix),
    resolve_document_id_propertied_component(Schema_Prefix, Property, Inner_Component, Id_Fragment).
resolve_document_id_propertied_component(Schema_Prefix, Property, Inner_Component, Id_Fragment) :-
    component_fragment(Inner_Component, Inner_Fragment),
    (   string_concat(Schema_Prefix, Contracted_Property, Property)
    ->  true
    ;   Contracted_Property = Property),
    uri_encoded_string(segment, Contracted_Property, Encoded_Property),
    format(string(Id_Fragment), "~s/~s", [Encoded_Property, Inner_Fragment]).

resolve_document_id_indexed_component(Schema_Prefix, Index, Inner_Component, Id_Fragment) :-
    \+ ground(Inner_Component),
    !,
    resolve_document_id_component(Inner_Component, Schema_Prefix),
    resolve_document_id_indexed_component(Schema_Prefix, Index, Inner_Component, Id_Fragment).
resolve_document_id_indexed_component(_Schema_Prefix, Index, Inner_Component, Id_Fragment) :-
    component_fragment(Inner_Component, Inner_Fragment),
    format(string(Id_Fragment), "~d/~s", [Index, Inner_Fragment]).

resolve_document_id_component(property(Property, Inner_Component, Id_Fragment), Schema_Prefix) :-
    resolve_document_id_propertied_component(Schema_Prefix, Property, Inner_Component, Id_Fragment).
resolve_document_id_component(optional_property(Property, Inner_Component, Id_Fragment), Schema_Prefix) :-
    resolve_document_id_propertied_component(Schema_Prefix, Property, Inner_Component, Id_Fragment).
resolve_document_id_component(set_property(Property, Inner_Component, Id_Fragment), Schema_Prefix) :-
    resolve_document_id_propertied_component(Schema_Prefix, Property, Inner_Component, Id_Fragment).
resolve_document_id_component(array_index(Index, Inner_Component, Id_Fragment), Schema_Prefix) :-
    resolve_document_id_indexed_component(Schema_Prefix, Index, Inner_Component, Id_Fragment).
resolve_document_id_component(list_index(Index, Inner_Component, Id_Fragment), Schema_Prefix) :-
    resolve_document_id_indexed_component(Schema_Prefix, Index, Inner_Component, Id_Fragment).
resolve_document_id_component(base(Base, Id_Fragment), Schema_Prefix) :-
    do_or_die(ground(Id_Fragment),
              error(base_not_ground(Base), _)),

    (   string_concat(Schema_Prefix, Contracted_Base, Base)
    ->  true
    ;   Contracted_Base = Base),

    do_or_die(string_concat(Contracted_Base, _, Id_Fragment),
              error(id_fragment_does_not_start_with_base(Base, Id_Fragment))).
resolve_document_id_component(random(Base, Id_Fragment), Schema_Prefix) :-
    random(X),
    format(string(S), '~w', [X]),
    crypto_data_hash(S, Hash, [algorithm(sha256)]),
    (   string_concat(Schema_Prefix, Contracted_Base, Base)
    ->  true
    ;   Contracted_Base = Base),
    uri_encoded_string(segment, Contracted_Base, Encoded_Base),
    format(string(Id_Fragment),'~w/~w',[Encoded_Base, Hash]).
resolve_document_id_component(lexical(Base, Fields, Suffix, Id_Fragment), Schema_Prefix) :-
    resolve_document_fields(Fields, Outputs),
    merge_separator_split(Suffix, "+", Outputs),
    (   string_concat(Schema_Prefix, Contracted_Base, Base)
    ->  true
    ;   Contracted_Base = Base),
    uri_encoded_string(segment, Contracted_Base, Encoded_Base),
    format(string(Id_Fragment),'~w/~w',[Encoded_Base, Suffix]).
resolve_document_id_component(hash(Base, Fields, Suffix, Id_Fragment), Schema_Prefix) :-
    resolve_document_fields(Fields, Outputs),
    merge_separator_split(Suffix, "+", Outputs),
    format(string(S), '~w', [Suffix]),
    crypto_data_hash(S, Hash, [algorithm(sha256)]),
    (   string_concat(Schema_Prefix, Contracted_Base, Base)
    ->  true
    ;   Contracted_Base = Base),
    uri_encoded_string(segment, Contracted_Base, Encoded_Base),
    format(string(Id_Fragment),'~w/~w',[Encoded_Base, Hash]).

resolve_document_fields([], []).
resolve_document_fields([Field|Fields], [Output|Outputs]) :-
    resolve_document_field(Field),
    field_value_string(Field, Output),
    resolve_document_fields(Fields, Outputs).

resolve_document_field(Field) :-
    field_value(Field, Value),
    do_or_die(ground(Value),
              error(field_is_not_ground_while_resolving_id(Field), _)),
    resolve_document_field_(Field).
resolve_document_field_(property(_, _, Value, Value_String)) :-
    uri_encoded_string(segment, Value, Value_String).
resolve_document_field_(optional_property(_, _, none, "+none+")) :- !.
resolve_document_field_(optional_property(_, _, some(Value), Value_String)) :-
    uri_encoded_string(segment, Value, Value_String).
resolve_document_field_(list_property(_, _, Values, Value)) :-
    maplist(uri_encoded_string(segment), Values, Encoded_Values),
    merge_separator_split(Value, "++", Encoded_Values).
resolve_document_field_(set_property(_, _, Values, Value)) :-
    sort(Values, Values_Sorted),
    maplist(uri_encoded_string(segment), Values_Sorted, Encoded_Values),
    merge_separator_split(Value, "++", Encoded_Values).
resolve_document_field_(array_property(_, _, Values, Value)) :-
    maplist(uri_encoded_string(segment), Values, Encoded_Values),
    merge_separator_split(Value, "++", Encoded_Values).


component_fragment(base(_, Fragment), Fragment).
component_fragment(random(_, Fragment), Fragment).
component_fragment(lexical(_, _, _, Fragment), Fragment).
component_fragment(hash(_, _, _, Fragment), Fragment).
component_fragment(valuehash(_, _, _, Fragment), Fragment).
component_fragment(array_index(_, _, Fragment), Fragment).
component_fragment(list_index(_, _, Fragment), Fragment).
component_fragment(property(_, _, Fragment), Fragment).

field_value(property(_, _, Value, _), Value).
field_value(optional_property(_, _, Value, _), Value).
field_value(list_property(_, _, Value, _), Value).
field_value(set_property(_, _, Value, _), Value).
field_value(array_property(_, _, Value, _), Value).

field_value_string(property(_, _, _, Value_String), Value_String).
field_value_string(optional_property(_, _, _, Value_String), Value_String).
field_value_string(list_property(_, _, _, Value_String), Value_String).
field_value_string(set_property(_, _, _, Value_String), Value_String).
field_value_string(array_property(_, _, _, Value_String), Value_String).

combine_fragments(prefix(Base, _Schema), Component, Id) :-
    component_fragment(Component, Fragment),
    format(string(Id), "~w~w", [Base, Fragment]).
combine_fragments(document_id(_, _, Parent_Id), Component, Id) :-
    component_fragment(Component, Fragment),
    format(string(Id), "~w/~w", [Parent_Id, Fragment]).

schema_prefix_from_type(Type, Prefix) :-
    split_string(Type, "#", "", Elements),
    \+ length(Elements, 1),
    !,
    do_or_die(length(Elements, 2),
              error(type_iri_contains_multiple_hashes(Type))),
    Elements = [Prefix_, _],
    string_concat(Prefix_, "#", Prefix).
schema_prefix_from_type(Type, Type).

schema_prefix_from_component(random(Type, _), Prefix) :-
    schema_prefix_from_type(Type, Prefix).
schema_prefix_from_component(lexical(Type, _), Prefix) :-
    schema_prefix_from_type(Type, Prefix).
schema_prefix_from_component(hash(Type, _), Prefix) :-
    schema_prefix_from_type(Type, Prefix).
schema_prefix_from_component(valuehash(Type, _), Prefix) :-
    schema_prefix_from_type(Type, Prefix).
schema_prefix_from_component(array_index(_, Inner), Prefix) :-
    schema_prefix_from_component(Inner, Prefix).
schema_prefix_from_component(list_index(_, Inner), Prefix) :-
    schema_prefix_from_component(Inner, Prefix).
schema_prefix_from_component(property(_, Inner), Prefix) :-
    schema_prefix_from_component(Inner, Prefix).

schema_prefix_from_document_id(prefix(_Base, Schema), Schema).
schema_prefix_from_document_id(document_id(Component, _, _), Prefix) :-
    schema_prefix_from_component(Component, Prefix).

