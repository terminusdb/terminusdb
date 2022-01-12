:- module('document/idgen',
          []).

:- use_module(core(util)).

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
%%
%% Fields point at a data type or collection of data types.
%% It's possible that this happens in a nested way.
%%
%% field: property(Property_Name, Field_Type, Value, Value_String)
%%      | optional_property(Property_Name, Field_Type, Value, Value_String)
%%      | list_property(Property_Name, Field_Type, Values, Value_String)
%%      | set_property(Property_Name, Field_Type, Values, Value_String)
%%      | array_property(Property_Name, Field_Type, Values, Value_String)
%%
%% field_type: data_type(Type)
%%           | nested_field(field)

%% Given a type, we should be able to generate a stub document_id structure
document_id_for_type(_Transaction, _Type, _Document_Id) :-
    true.

%% Given a document and a type, it should be possible to generate a list of document_id structures
document_ids_for_document(_Transaction, _Type, _Document, _Document_Ids) :-
    true.


%% if parent_component is not yet ground, we resolve that first.
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

