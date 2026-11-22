:- module(rebase_merge, [
    layer_touches_collections/1,
    collection_predicate/1,
    merge_document/6,
    strip_nested_ids/2
]).

:- use_module(core(document/schema)).
:- use_module(core(document/json), [prefix_expand_schema/3]).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(yall)).
:- use_module(library(terminus_store)).

collection_predicate('http://www.w3.org/1999/02/22-rdf-syntax-ns#first').
collection_predicate('http://www.w3.org/1999/02/22-rdf-syntax-ns#rest').
collection_predicate('http://terminusdb.com/schema/sys#value').
collection_predicate('http://terminusdb.com/schema/sys#index').
collection_predicate('http://terminusdb.com/schema/sys#index2').
collection_predicate('http://terminusdb.com/schema/sys#index3').
collection_predicate('http://terminusdb.com/schema/sys#index4').

% layer_touches_collections(+Layer) is semidet.
%
% True when a commit instance layer adds or removes any List cons cell or
% Array cell triple. Cells receive fresh random ids on every document
% rewrite, so replaying such a commit as a raw triple delta cannot line up
% with the target branch.
layer_touches_collections(Layer) :-
    (   triple_addition(Layer, _, Predicate, _)
    ;   triple_removal(Layer, _, Predicate, _)
    ),
    atom_string(Collection_Predicate, Predicate),
    collection_predicate(Collection_Predicate),
    !.

% merge_document(+Schema, +Prefixes, +Base, +Ours, +Theirs, -Merged)
%
% Three-way merge of one document as returned by get_document/3. Base, Ours
% and Theirs are document dicts or the atom 'missing'. Merged is a document
% dict (nested @id fields stripped so ids regenerate on write, top-level
% @id kept) or 'missing'. Throws
% error(rebase_merge_conflict(Document, Path, Base, Ours, Theirs), _).
merge_document(Schema, Prefixes, Base0, Ours0, Theirs0, Merged) :-
    % Cell and subdocument ids regenerate on every write, so equality and
    % conflict checks compare content, not ids.
    strip_nested_ids(Base0, Base),
    strip_nested_ids(Ours0, Ours),
    strip_nested_ids(Theirs0, Theirs),
    top_id(Ours, OursId),
    top_id(Theirs, TheirsId),
    top_id(Base, BaseId),
    (   nonvar(OursId) -> TopId = OursId
    ;   nonvar(TheirsId) -> TopId = TheirsId
    ;   TopId = BaseId
    ),
    catch(
        merge_value(Schema, Prefixes, unknown, Base, Ours, Theirs, [], Result0),
        error(rebase_merge_conflict(_, Path, B, O, T), _),
        (   (   top_id(Base, Id) -> true
            ;   top_id(Ours, Id) -> true
            ;   top_id(Theirs, Id) -> true
            ;   Id = unknown ),
            throw(error(rebase_merge_conflict(Id, Path, B, O, T), _)))),
    (   Result0 == missing
    ->  Merged = missing
    ;   is_dict(Result0), nonvar(TopId)
    ->  put_dict('@id', Result0, TopId, Merged)
    ;   Merged = Result0).

top_id(Doc, Id) :- is_dict(Doc), get_dict('@id', Doc, Id), !.
top_id(_, _).

% merge_value(+Schema, +Prefixes, +Type, +Base, +Ours, +Theirs, +Path, -Result)
merge_value(Schema, Prefixes, Type, Base, Ours, Theirs, Path, Result) :-
    (   Ours == Theirs
    ->  Result = Ours
    ;   Base == Ours
    ->  Result = Theirs
    ;   Base == Theirs
    ->  Result = Ours
    ;   merge_value_structured(Schema, Prefixes, Type, Base, Ours, Theirs, Path, Result)).

merge_value_structured(Schema, Prefixes, Type0, Base, Ours, Theirs, Path, Result) :-
    (   container_type(Type0)
    ->  unwrap_optional(Type0, Type),
        (   is_list(Base), is_list(Ours), is_list(Theirs)
        ->  (   Type =.. [set|_] -> merge_set(Schema, Prefixes, Type, Base, Ours, Theirs, Path, Result)
            ;   merge_list(Schema, Prefixes, Type, Base, Ours, Theirs, Path, Result))
        ;   conflict(Path, Base, Ours, Theirs))
    ;   is_dict(Base), is_dict(Ours), is_dict(Theirs)
    ->  merge_document_keys(Schema, Prefixes, Base, Ours, Theirs, Path, Result)
    ;   conflict(Path, Base, Ours, Theirs)).

unwrap_optional(optional(Type), Unwrapped) :- !, unwrap_optional(Type, Unwrapped).
unwrap_optional(Type, Type).

container_type(optional(Type)) :- !, container_type(Type).
container_type(Type) :-
    nonvar(Type),
    (   Type =.. [list|_]
    ;   Type =.. [array|_]
    ;   Type =.. [set|_]
    ;   Type =.. [table|_]).

conflict(Path, Base, Ours, Theirs) :-
    throw(error(rebase_merge_conflict(unknown, Path, Base, Ours, Theirs), _)).

merge_document_keys(Schema, Prefixes, Base, Ours, Theirs, Path, Result) :-
    dict_pairs(Ours, _, OursPairs),
    dict_pairs(Theirs, _, TheirsPairs),
    dict_pairs(Base, _, BasePairs),
    append([OursPairs, TheirsPairs, BasePairs], Pairs0),
    keys_of_pairs(Pairs0, [], Keys),
    merge_keys_(Schema, Prefixes, Base, Ours, Theirs, Keys, Path, ResultPairs),
    dict_pairs(Result, json, ResultPairs).

keys_of_pairs([], Acc, Acc).
keys_of_pairs([Key-_|Rest], Acc, Out) :-
    (   memberchk(Key, Acc)
    ->  keys_of_pairs(Rest, Acc, Out)
    ;   keys_of_pairs(Rest, [Key|Acc], Out)).

merge_keys_(_, _, _, _, _, [], _, []) :- !.
merge_keys_(Schema, Prefixes, Base, Ours, Theirs, [Key|Keys], Path, Pairs) :-
    (   Key == '@id'
    ->  Pairs = Pairs0
    ;   Key == '@type'
    ->  (   get_dict('@type', Ours, T1), get_dict('@type', Theirs, T2), T1 == T2
        ->  true
        ;   conflict(['@type'|Path], Base, Ours, Theirs)),
        Pairs = ['@type'-T1|Pairs0]
    ;   sub_atom(Key, 0, 1, _, '@')
    ->  (   get_dict(Key, Ours, OV), get_dict(Key, Theirs, TV), OV == TV
        ->  Pairs = [Key-OV|Pairs0]
        ;   Pairs = Pairs0)
    ;   property_type(Schema, Prefixes, Ours, Key, Type),
        value_of(Key, Base, BV),
        value_of(Key, Ours, OV),
        value_of(Key, Theirs, TV),
        (   OV == missing, TV == missing
        ->  Pairs = Pairs0
        ;   merge_value(Schema, Prefixes, Type, BV, OV, TV, [Key|Path], RV),
            (   RV == missing
            ->  Pairs = Pairs0
            ;   Pairs = [Key-RV|Pairs0]))),
    merge_keys_(Schema, Prefixes, Base, Ours, Theirs, Keys, Path, Pairs0).

value_of(Key, Doc, Value) :-
    is_dict(Doc), !,
    (   get_dict(Key, Doc, Value) -> true ; Value = missing ).
value_of(_, _, missing).

property_type(Schema, Prefixes, Doc, Key, Type) :-
    is_dict(Doc),
    get_dict('@type', Doc, Class),
    catch(prefix_expand_schema(Class, Prefixes, ClassEx), _, fail),
    catch(prefix_expand_schema(Key, Prefixes, KeyEx), _, fail),
    catch(schema_class_predicate_type(Schema, ClassEx, KeyEx, Type), _, Type = unknown),
    !.
property_type(_, _, _, _, unknown).

% strip_nested_ids(+Doc, -Result): keep the top-level @id, strip all nested @ids.
strip_nested_ids(Doc, Result) :-
    is_dict(Doc), !,
    dict_pairs(Doc, Tag, Pairs),
    maplist(strip_nested_pair_top, Pairs, Stripped),
    dict_pairs(Result, Tag, Stripped).
strip_nested_ids(Value, New) :-
    is_list(Value), !, maplist(strip_nested_ids, Value, New).
strip_nested_ids(Value, Value).

strip_nested_pair_top('@id'-Value, ('@id'-Value)) :- !.
strip_nested_pair_top(Key-Value, Key-New) :- strip_ids_deep(Value, New).

strip_ids_deep(Value, New) :-
    is_dict(Value), !,
    dict_pairs(Value, Tag, Pairs),
    exclude(id_pair, Pairs, Kept),
    maplist(strip_ids_deep_pair, Kept, Stripped),
    dict_pairs(New, Tag, Stripped).

strip_ids_deep(Value, New) :-
    is_list(Value), !, maplist(strip_ids_deep, Value, New).
strip_ids_deep(Value, Value).

id_pair('@id'-_).

strip_ids_deep_pair(Key-Value, Key-New) :- strip_ids_deep(Value, New).

% element_key(+Schema, +Prefixes, +Element, -Key)
%
% Stable identity of a container element across branch rewrites: the schema
% key fields for subdocuments, the value itself for literals.
element_key(Schema, Prefixes, Element, Key) :-
    is_dict(Element),
    get_dict('@type', Element, Class),
    catch(prefix_expand_schema(Class, Prefixes, ClassEx), _, fail),
    catch(schema_key_descriptor(Schema, Prefixes, ClassEx, Descriptor), _, Descriptor = unknown),
    (   Descriptor = lexical(_, Fields)
    ->  fields_key(Prefixes, Fields, Element, Key)
    ;   Descriptor = hash(_, Fields)
    ->  fields_key(Prefixes, Fields, Element, Key)
    ;   Key = Element),
    !.
element_key(_, _, Element, Element).

element_assoc(Schema, Prefixes, Elements, Assoc) :-
    maplist(element_key(Schema, Prefixes), Elements, Keys),
    pairs_keys_values(Assoc, Keys, Elements).

fields_key(Prefixes, Fields, Element, Key) :-
    maplist(field_value(Prefixes, Element), Fields, Values),
    Key = fields(Values).

% Schema key fields are expanded predicate URIs; documents carry compressed
% keys, so the @schema prefix comes off first.
field_value(Prefixes, Element, Field, Value) :-
    (   get_dict(Field, Element, Value)
    ->  true
    ;   schema_local_key(Prefixes, Field, Local),
        get_dict(Local, Element, Value)
    ->  true
    ;   Value = missing ).

schema_local_key(Prefixes, Field, Local) :-
    get_dict('@schema', Prefixes, SchemaPrefix),
    SchemaPrefix \== '',
    atom_string(SchemaPrefixA, SchemaPrefix),
    atom_concat(SchemaPrefixA, Local, Field),
    !.

% merge_list(+Schema, +Prefixes, +Type, +Base, +Ours, +Theirs, +Path, -Result)
%
% Ordered three-way merge of List/Array elements by stable identity.
merge_list(Schema, Prefixes, Type, Base, Ours, Theirs, Path, Result) :-
    element_keys(Schema, Prefixes, Base, KeysB),
    element_keys(Schema, Prefixes, Ours, KeysO),
    element_keys(Schema, Prefixes, Theirs, KeysT),
    (   KeysO == KeysB
    ->  merge_list_theirs(Schema, Prefixes, Type, Base, Ours, Theirs, Path, Result)
    ;   KeysT == KeysB
    ->  merge_list_ours(Schema, Prefixes, Type, Base, Ours, Theirs, Path, Result)
    ;   KeysO == KeysT
    ->  merge_list_both(Schema, Prefixes, Type, Base, Ours, Theirs, Path, Result)
    ;   conflict(Path, Base, Ours, Theirs)).

element_keys(Schema, Prefixes, Elements, Keys) :-
    maplist(element_key(Schema, Prefixes), Elements, Keys).

% ours kept base's membership and order: theirs drives structure, ours'
% content edits merge into shared elements.
merge_list_theirs(Schema, Prefixes, _Type, Base, Ours, Theirs, Path, Result) :-
    element_assoc(Schema, Prefixes, Base, AssocB),
    element_assoc(Schema, Prefixes, Ours, AssocO),
    assoc_to_list(AssocB, BasePairs),
    assoc_to_list(AssocO, OursPairs),
    element_keys(Schema, Prefixes, Theirs, KeysT),
    % ours may not modify an element that theirs removed
    forall((member(K-B, BasePairs),
            member(K-O, OursPairs),
            O \== B,
            \+ memberchk(K, KeysT)),
           conflict(Path, B, O, missing)),
    maplist(merge_list_theirs_elem(Schema, Prefixes, AssocB, AssocO, Path), Theirs, Result).

merge_list_theirs_elem(Schema, Prefixes, AssocB, AssocO, Path, Element, Merged) :-
    element_key(Schema, Prefixes, Element, Key),
    (   memberchk(Key-B, AssocB),
        memberchk(Key-O, AssocO)
    ->  merge_value(Schema, Prefixes, unknown, B, O, Element, Path, Merged)
    ;   Merged = Element).

% theirs kept base's membership and order: ours drives structure.
merge_list_ours(Schema, Prefixes, _Type, Base, Ours, Theirs, Path, Result) :-
    element_assoc(Schema, Prefixes, Base, AssocB),
    element_assoc(Schema, Prefixes, Theirs, AssocT),
    assoc_to_list(AssocB, BasePairs),
    assoc_to_list(AssocT, TheirsPairs),
    element_keys(Schema, Prefixes, Ours, KeysO),
    forall((member(K-B, BasePairs),
            member(K-T, TheirsPairs),
            T \== B,
            \+ memberchk(K, KeysO)),
           conflict(Path, B, missing, T)),
    maplist(merge_list_ours_elem(Schema, Prefixes, AssocB, AssocT, Path), Ours, Result).

merge_list_ours_elem(Schema, Prefixes, AssocB, AssocT, Path, Element, Merged) :-
    element_key(Schema, Prefixes, Element, Key),
    (   memberchk(Key-B, AssocB),
        memberchk(Key-T, AssocT)
    ->  merge_value(Schema, Prefixes, unknown, B, Element, T, Path, Merged)
    ;   Merged = Element).

merge_list_both(Schema, Prefixes, Type, Base, Ours, Theirs, Path, Result) :-
    element_assoc(Schema, Prefixes, Base, AssocB),
    maplist(merge_list_both_elem(Schema, Prefixes, Type, AssocB, Path), Ours, Theirs, Result).

merge_list_both_elem(Schema, Prefixes, _Type, AssocB, Path, OursElt, TheirsElt, Merged) :-
    element_key(Schema, Prefixes, OursElt, Key),
    (   memberchk(Key-B, AssocB)
    ->  merge_value(Schema, Prefixes, unknown, B, OursElt, TheirsElt, Path, Merged)
    ;   merge_value(Schema, Prefixes, unknown, missing, OursElt, TheirsElt, Path, Merged)).

assoc_to_list(Assoc, Pairs) :-
    (   is_dict(Assoc) -> dict_pairs(Assoc, _, Pairs) ; Pairs = Assoc ).

% merge_set(+Schema, +Prefixes, +Type, +Base, +Ours, +Theirs, +Path, -Result)
%
% Unordered three-way merge of Set elements by stable identity.
merge_set(Schema, Prefixes, Type, Base, Ours, Theirs, Path, Result) :-
    element_keys(Schema, Prefixes, Base, KeysB),
    element_keys(Schema, Prefixes, Ours, KeysO),
    element_keys(Schema, Prefixes, Theirs, KeysT),
    sort(KeysB, SortedB), sort(KeysO, SortedO), sort(KeysT, SortedT),
    (   SortedO == SortedB
    ->  merge_list_theirs(Schema, Prefixes, Type, Base, Ours, Theirs, Path, Result)
    ;   SortedT == SortedB
    ->  merge_list_ours(Schema, Prefixes, Type, Base, Ours, Theirs, Path, Result)
    ;   merge_set_changed(Schema, Prefixes, Type, Base, Ours, Theirs, Path, Result)).

merge_set_changed(Schema, Prefixes, _Type, Base, Ours, Theirs, Path, Result) :-
    element_assoc(Schema, Prefixes, Base, AssocB),
    element_assoc(Schema, Prefixes, Ours, AssocO),
    element_assoc(Schema, Prefixes, Theirs, AssocT),
    assoc_to_list(AssocB, BasePairs),
    assoc_to_list(AssocT, TheirsPairs),
    maplist(merge_set_ours_elem(Schema, Prefixes, AssocB, AssocT, Path), Ours, KeptWithMarkers),
    exclude(==(removed), KeptWithMarkers, KeptOurs),
    % ours removed an element that theirs modified: conflict; unchanged, the
    % removal stands and the element must not come back through theirs.
    forall((member(Key-T, TheirsPairs),
            memberchk(Key-B, BasePairs),
            \+ memberchk(Key-_, AssocO),
            T \== B),
           conflict(Path, B, missing, T)),
    assoc_keys(AssocT, KeysT),
    exclude(memberchk_in_assoc(AssocO), KeysT, NotOurs),
    exclude(memberchk_in_assoc(AssocB), NotOurs, OnlyTheirsKeys),
    maplist(theirs_key_elem(AssocT), OnlyTheirsKeys, ExtraTheirs),
    append(KeptOurs, ExtraTheirs, Result).

memberchk_in_assoc(Assoc, Key) :- memberchk(Key-_, Assoc).

assoc_keys(Assoc, Keys) :-
    assoc_to_list(Assoc, Pairs),
    maplist([K-_, K]>>true, Pairs, Keys).

theirs_key_elem(Assoc, Key, Elt) :- memberchk(Key-Elt, Assoc).

merge_set_ours_elem(Schema, Prefixes, AssocB, AssocT, Path, OursElt, Merged) :-
    element_key(Schema, Prefixes, OursElt, Key),
    (   memberchk(Key-B, AssocB)
    ->  (   memberchk(Key-T, AssocT)
        ->  merge_value(Schema, Prefixes, unknown, B, OursElt, T, Path, Merged)
        ;   OursElt == B
        ->  Merged = removed
        ;   conflict(Path, B, OursElt, missing))
    ;   (   memberchk(Key-T, AssocT)
        ->  (   OursElt == T
            ->  Merged = OursElt
            ;   conflict(Path, missing, OursElt, T))
        ;   Merged = OursElt)).
