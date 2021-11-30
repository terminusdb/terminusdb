:- module('document/idgen',
          []).

%% format: document_id(<component>, <parent_id>, <id>)
%%
%% parent_id: prefix(...)
%%          | document_id
%%
%% component: random(Type_Name, Id_Fragment)
%%            | lexical(Type_Name, +Fields, -Suffix, -Id_Fragment)
%%            | hash(Type_Name, +Fields, -Suffix, -Id_Fragment)
%%            | valuehash(Type_Name, +Fields, -Suffix, -Id_Fragment)
%%            | array_index(Index, Inner_Component, Id_Fragment)
%%            | list_index(Index, Inner_Component, Id_Fragment)
%%            | property(Property, Inner_Component, Id_Fragment)
%%
%% field: field(Type, Property, String_Val)
%%        | child_field(field, Type, Property, String_Val)

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
    resolve_document_id_component(Component),
    combine_fragments(Parent_Id, Component, Id).
resolve_document_id(prefix(_)) :-
    % we got here cause prefix was not ground, which is an error
    throw(error(prefix_unknown_while_generating_id, _)).

resolve_document_id_propertied_component(Property, Inner_Component, Id_Fragment) :-
    \+ ground(Inner_Component),
    !,
    resolve_document_id_component(Inner_Component),
    resolve_document_id_propertied_component(Property, Inner_Component, Id_Fragment).
resolve_document_id_propertied_component(Property, Inner_Component, Id_Fragment) :-
    component_fragment(Inner_Component, Inner_Fragment),
    format(string(Id_Fragment), "~s/~s", [Property, Inner_Fragment]).

resolve_document_id_indexed_component(Index, Inner_Component, Id_Fragment) :-
    \+ ground(Inner_Component),
    !,
    resolve_document_id_component(Inner_Component),
    resolve_document_id_indexed_component(Index, Inner_Component, Id_Fragment).
resolve_document_id_indexed_component(Index, Inner_Component, Id_Fragment) :-
    component_fragment(Inner_Component, Inner_Fragment),
    format(string(Id_Fragment), "~d/~s", [Index, Inner_Fragment]).

resolve_document_id_component(property(Property, Inner_Component, Id_Fragment)) :-
    resolve_document_id_propertied_component(Property, Inner_Component, Id_Fragment).
resolve_document_id_component(array_index(Index, Inner_Component, Id_Fragment)) :-
    resolve_document_id_indexed_component(Index, Inner_Component, Id_Fragment).
resolve_document_id_component(list_index(Index, Inner_Component, Id_Fragment)) :-
    resolve_document_id_indexed_component(Index, Inner_Component, Id_Fragment).
resolve_document_id_component(random(Base, Id_Fragment)) :-
    random(X),
    format(string(S), '~w', [X]),
    crypto_data_hash(S, Hash, [algorithm(sha256)]),
    format(string(Id_Fragment),'~w/~w',[Base, Hash]).

component_fragment(random(_, Fragment), Fragment).
component_fragment(lexical(_, _, _, Fragment), Fragment).
component_fragment(hash(_, _, _, Fragment), Fragment).
component_fragment(valuehash(_, _, _, Fragment), Fragment).
component_fragment(array_index(_, _, Fragment), Fragment).
component_fragment(list_index(_, _, Fragment), Fragment).
component_fragment(property(_, _, Fragment), Fragment).

combine_fragments(prefix(P), Component, Id) :-
    component_fragment(Component, Fragment),
    format(string(Id), "~w~w", [P, Fragment]).
combine_fragments(document_id(_, _, Parent_Id), Component, Id) :-
    component_fragment(Component, Fragment),
    format(string(Id), "~w/~w", [Parent_Id, Fragment]).
