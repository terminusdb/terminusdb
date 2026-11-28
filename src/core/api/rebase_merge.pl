:- module(rebase_merge, [
    layer_touches_collections/1,
    collection_predicate/1,
    merge_document/6,
    merge_document_with_resolution/8,
    normalize_document/4
]).

:- use_module(core(document/schema)).
:- use_module(core(document/json), [prefix_expand_schema/3]).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(yall)).
:- use_module(library(terminus_store)).

% collection_predicate(+Predicate) is semidet.
%
% Predicates that link a document to its List cons cells or Array cells.
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

% merge_document(+Schema, +Prefixes, +Base, +Ours, +Theirs, -Merged) is det.
%
% Strict three-way merge of one document as returned by get_document/3.
merge_document(Schema, Prefixes, Base, Ours, Theirs, Merged) :-
    merge_document_with_resolution(Schema, Prefixes, Base, Ours, Theirs, error, Merged, _).

% merge_document_with_resolution(+Schema, +Prefixes, +Base, +Ours, +Theirs, +Winner, -Merged, -Events) is det.
%
% Base, Ours and Theirs are document dicts or the atom 'missing'. Winner is
% error, ours, theirs or merge. Merged is a document dict (nested @id
% fields stripped so ids regenerate on write, top-level @id kept) or
% 'missing'. Events carry resolution(Path, Winner) for each place where a
% caller-selected resolution decided the outcome. Throws
% error(rebase_merge_conflict(Document, Path, Base, Ours, Theirs), _).
merge_document_with_resolution(Schema, Prefixes, Base0, Ours0, Theirs0, Winner, Merged, Events) :-
    normalize_document(Schema, Prefixes, Base0, Base),
    normalize_document(Schema, Prefixes, Ours0, Ours),
    normalize_document(Schema, Prefixes, Theirs0, Theirs),
    top_id(Ours, OursId),
    top_id(Theirs, TheirsId),
    top_id(Base, BaseId),
    (   nonvar(OursId) -> TopId = OursId
    ;   nonvar(TheirsId) -> TopId = TheirsId
    ;   TopId = BaseId
    ),
    catch(
        merge_value(Schema, Prefixes, unknown, Base, Ours, Theirs, Winner, [], Result0, Events),
        error(rebase_merge_conflict(_, Path, B, O, T), _),
        resolve_merge_conflict(Winner, Base, Ours, Theirs, Path, B, O, T, Result0, Events)),
    (   Result0 == missing
    ->  Merged = missing
    ;   is_dict(Result0), nonvar(TopId)
    ->  put_dict('@id', Result0, TopId, Merged)
    ;   Merged = Result0).

resolve_merge_conflict(Winner, _Base, Ours, Theirs, _Path, _B, _O, _T, Resolved, [resolution([], Winner)]) :-
    memberchk(Winner, [ours, theirs]),
    !,
    (   Winner == ours
    ->  Resolved = Ours
    ;   Resolved = Theirs).
resolve_merge_conflict(_Winner, Base, Ours, Theirs, Path, B, O, T, _Result, _Events) :-
    (   top_id(Base, Id) -> true
    ;   top_id(Ours, Id) -> true
    ;   top_id(Theirs, Id) -> true
    ;   Id = unknown ),
    throw(error(rebase_merge_conflict(Id, Path, B, O, T), _)).

top_id(Doc, Id) :- is_dict(Doc), get_dict('@id', Doc, Id), !.
top_id(_, _).

% merge_value(+Schema, +Prefixes, +Type, +Base, +Ours, +Theirs, +Winner, +Path, -Result, -Events) is det.
merge_value(Schema, Prefixes, Type, Base, Ours, Theirs, Winner, Path, Result, Events) :-
    (   Ours == Theirs
    ->  Result = Ours,
        Events = []
    ;   Base == Ours
    ->  Result = Theirs,
        Events = []
    ;   Base == Theirs
    ->  Result = Ours,
        Events = []
    ;   merge_value_structured(Schema, Prefixes, Type, Base, Ours, Theirs, Winner, Path, Result, Events)).

merge_value_structured(Schema, Prefixes, Type0, Base, Ours, Theirs, Winner, Path, Result, Events) :-
    (   container_type(Type0)
    ->  unwrap_optional(Type0, Type),
        container_value(Type, Base, Base_Value),
        container_value(Type, Ours, Ours_Value),
        container_value(Type, Theirs, Theirs_Value),
        (   is_list(Base_Value), is_list(Ours_Value), is_list(Theirs_Value)
        ->  (   Type =.. [set|_] -> merge_set(Schema, Prefixes, Type, Base_Value, Ours_Value, Theirs_Value, Winner, Path, Result, Events)
            ;   merge_list(Schema, Prefixes, Type, Base_Value, Ours_Value, Theirs_Value, Winner, Path, Result, Events))
        ;   conflict(Path, Base, Ours, Theirs))
    ;   is_dict(Base), is_dict(Ours), is_dict(Theirs)
    ->  merge_document_keys(Schema, Prefixes, Base, Ours, Theirs, Winner, Path, Result, Events)
    ;   conflict(Path, Base, Ours, Theirs)).

unwrap_optional(optional(Type), Unwrapped) :- !, unwrap_optional(Type, Unwrapped).
unwrap_optional(Type, Type).

container_value(Type, missing, []) :-
    Type =.. [set|_],
    !.
container_value(_, Value, Value).

container_type(optional(Type)) :- !, container_type(Type).
container_type(Type) :-
    nonvar(Type),
    (   Type =.. [list|_]
    ;   Type =.. [array|_]
    ;   Type =.. [set|_]
    ;   Type =.. [table|_]).

conflict(Path, Base, Ours, Theirs) :-
    throw(error(rebase_merge_conflict(unknown, Path, Base, Ours, Theirs), _)).

merge_document_keys(Schema, Prefixes, Base, Ours, Theirs, Winner, Path, Result, Events) :-
    dict_pairs(Ours, _, OursPairs),
    dict_pairs(Theirs, _, TheirsPairs),
    dict_pairs(Base, _, BasePairs),
    append([OursPairs, TheirsPairs, BasePairs], Pairs0),
    keys_of_pairs(Pairs0, [], Keys),
    merge_keys_(Schema, Prefixes, Base, Ours, Theirs, Winner, Keys, Path, ResultPairs, Events),
    dict_pairs(Result, json, ResultPairs).

keys_of_pairs([], Acc, Acc).
keys_of_pairs([Key-_|Rest], Acc, Out) :-
    (   memberchk(Key, Acc)
    ->  keys_of_pairs(Rest, Acc, Out)
    ;   keys_of_pairs(Rest, [Key|Acc], Out)).

merge_keys_(_, _, _, _, _, _, [], _, [], []) :- !.
merge_keys_(Schema, Prefixes, Base, Ours, Theirs, Winner, [Key|Keys], Path, Pairs, Events) :-
    (   Key == '@id'
    ->  Pairs = Pairs0,
        Field_Events = []
    ;   Key == '@type'
    ->  (   get_dict('@type', Ours, T1), get_dict('@type', Theirs, T2), T1 == T2
        ->  true
        ;   conflict(['@type'|Path], Base, Ours, Theirs)),
        Pairs = ['@type'-T1|Pairs0],
        Field_Events = []
    ;   property_type(Schema, Prefixes, Ours, Key, Type),
        value_of(Key, Base, BV),
        value_of(Key, Ours, OV),
        value_of(Key, Theirs, TV),
        (   OV == missing, TV == missing
        ->  Pairs = Pairs0,
            Field_Events = []
        ;   merge_value(Schema, Prefixes, Type, BV, OV, TV, Winner, [Key|Path], RV, Field_Events),
            (   RV == missing
            ->  Pairs = Pairs0
            ;   Pairs = [Key-RV|Pairs0]))),
    merge_keys_(Schema, Prefixes, Base, Ours, Theirs, Winner, Keys, Path, Pairs0, Rest_Events),
    append(Field_Events, Rest_Events, Events).

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

% normalize_document(+Schema, +Prefixes, +Doc, -Result) is det.
%
% Canonical form for equality, conflict checks and the write-back: embedded
% non-subdocument frames (@unfoldable, @unfold) collapse to their @id
% reference, and subdocument @ids — which regenerate on every write — are
% stripped. Linked frame content is not merged here; its edits ride the
% commit's raw delta like any non-collection document.
normalize_document(Schema, Prefixes, Doc0, Doc) :-
    collapse_linked_frames(Schema, Prefixes, Doc0, Doc1),
    strip_nested_ids(Schema, Prefixes, Doc1, Doc).

% collapse_linked_frames(+Schema, +Prefixes, +Doc, -Result) is det.
%
% The document itself is never collapsed, only frames below it.
collapse_linked_frames(Schema, Prefixes, Doc, Result) :-
    is_dict(Doc),
    !,
    dict_pairs(Doc, Tag, Pairs),
    maplist(collapse_linked_pair(Schema, Prefixes), Pairs, NewPairs),
    dict_pairs(Result, Tag, NewPairs).
collapse_linked_frames(_, _, Value, Value).

collapse_linked_pair(Schema, Prefixes, Key-Value, Key-Result) :-
    collapse_linked_value(Schema, Prefixes, Value, Result).

collapse_linked_value(Schema, Prefixes, Value, Result) :-
    is_dict(Value),
    !,
    (   linked_frame_id(Schema, Prefixes, Value, Id)
    ->  Result = Id
    ;   collapse_linked_frames(Schema, Prefixes, Value, Result)).
collapse_linked_value(Schema, Prefixes, Value, Result) :-
    is_list(Value), !,
    maplist(collapse_linked_value(Schema, Prefixes), Value, Result).
collapse_linked_value(_, _, Value, Value).

% linked_frame_id(+Schema, +Prefixes, +Dict, -Id) is semidet.
%
% An embedded frame is a reference when its class is a plain document class:
% subdocuments embed their content and must survive to the content merge.
linked_frame_id(Schema, Prefixes, Dict, Id) :-
    get_dict('@type', Dict, Class),
    get_dict('@id', Dict, Id),
    catch(prefix_expand_schema(Class, Prefixes, ClassEx), _, fail),
    catch((is_schema_simple_class(Schema, ClassEx),
           \+ schema_is_subdocument(Schema, ClassEx)), _, fail),
    !.

% strip_nested_ids(+Schema, +Prefixes, +Doc, -Result) is det.
%
% Keep the top-level @id, strip nested subdocument ids and preserve JSON data.
strip_nested_ids(Schema, Prefixes, Doc, Result) :-
    is_dict(Doc), !,
    dict_pairs(Doc, Tag, Pairs),
    maplist(strip_nested_pair_top(Schema, Prefixes), Pairs, Stripped),
    dict_pairs(Result, Tag, Stripped).
strip_nested_ids(Schema, Prefixes, Value, New) :-
    is_list(Value), !, maplist(strip_nested_ids(Schema, Prefixes), Value, New).
strip_nested_ids(_, _, Value, Value).

strip_nested_pair_top(_, _, '@id'-Value, ('@id'-Value)) :- !.
strip_nested_pair_top(Schema, Prefixes, Key-Value, Key-New) :-
    strip_ids_deep(Schema, Prefixes, Value, New).

strip_ids_deep(Schema, Prefixes, Value, New) :-
    is_dict(Value), !,
    dict_pairs(Value, Tag, Pairs),
    (   subdocument_frame(Schema, Prefixes, Value)
    ->  exclude(id_pair, Pairs, Kept)
    ;   Kept = Pairs),
    maplist(strip_ids_deep_pair(Schema, Prefixes), Kept, Stripped),
    dict_pairs(New, Tag, Stripped).

strip_ids_deep(Schema, Prefixes, Value, New) :-
    is_list(Value), !, maplist(strip_ids_deep(Schema, Prefixes), Value, New).
strip_ids_deep(_, _, Value, Value).

subdocument_frame(Schema, Prefixes, Dict) :-
    get_dict('@type', Dict, Class),
    catch(prefix_expand_schema(Class, Prefixes, ClassEx), _, fail),
    catch(schema_is_subdocument(Schema, ClassEx), _, fail),
    !.

id_pair('@id'-_).

strip_ids_deep_pair(Schema, Prefixes, Key-Value, Key-New) :-
    strip_ids_deep(Schema, Prefixes, Value, New).

% element_key(+Schema, +Prefixes, +Element, -Key) is det.
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

% merge_list(+Schema, +Prefixes, +Type, +Base, +Ours, +Theirs, +Winner, +Path, -Result, -Events) is det.
%
% Ordered three-way merge of List/Array elements by stable identity. merge
% does not concatenate ordered containers because order has no canonical
% resolution.
merge_list(Schema, Prefixes, Type, Base, Ours, Theirs, Winner, Path, Result, Events) :-
    element_keys(Schema, Prefixes, Base, KeysB),
    element_keys(Schema, Prefixes, Ours, KeysO),
    element_keys(Schema, Prefixes, Theirs, KeysT),
    (   KeysO == KeysB
    ->  merge_list_theirs(Schema, Prefixes, Type, Base, Ours, Theirs, Winner, Path, Result, Events)
    ;   KeysT == KeysB
    ->  merge_list_ours(Schema, Prefixes, Type, Base, Ours, Theirs, Winner, Path, Result, Events)
    ;   KeysO == KeysT
    ->  merge_list_both(Schema, Prefixes, Type, Base, Ours, Theirs, Winner, Path, Result, Events)
    ;   conflict(Path, Base, Ours, Theirs)).

element_keys(Schema, Prefixes, Elements, Keys) :-
    maplist(element_key(Schema, Prefixes), Elements, Keys).

% ours kept base's membership and order: theirs drives structure, ours'
% content edits merge into shared elements.
merge_list_theirs(Schema, Prefixes, _Type, Base, Ours, Theirs, Winner, Path, Result, Events) :-
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
    maplist(merge_list_theirs_elem(Schema, Prefixes, AssocB, AssocO, Winner, Path), Theirs, Result, Event_Lists),
    append(Event_Lists, Events).

merge_list_theirs_elem(Schema, Prefixes, AssocB, AssocO, Winner, Path, Element, Merged, Events) :-
    element_key(Schema, Prefixes, Element, Key),
    (   memberchk(Key-B, AssocB),
        memberchk(Key-O, AssocO)
    ->  merge_value(Schema, Prefixes, unknown, B, O, Element, Winner, Path, Merged, Events)
    ;   Merged = Element,
        Events = []).

% theirs kept base's membership and order: ours drives structure.
merge_list_ours(Schema, Prefixes, _Type, Base, Ours, Theirs, Winner, Path, Result, Events) :-
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
    maplist(merge_list_ours_elem(Schema, Prefixes, AssocB, AssocT, Winner, Path), Ours, Result, Event_Lists),
    append(Event_Lists, Events).

merge_list_ours_elem(Schema, Prefixes, AssocB, AssocT, Winner, Path, Element, Merged, Events) :-
    element_key(Schema, Prefixes, Element, Key),
    (   memberchk(Key-B, AssocB),
        memberchk(Key-T, AssocT)
    ->  merge_value(Schema, Prefixes, unknown, B, Element, T, Winner, Path, Merged, Events)
    ;   Merged = Element,
        Events = []).

merge_list_both(Schema, Prefixes, Type, Base, Ours, Theirs, Winner, Path, Result, Events) :-
    element_assoc(Schema, Prefixes, Base, AssocB),
    maplist(merge_list_both_elem(Schema, Prefixes, Type, AssocB, Winner, Path), Ours, Theirs, Result, Event_Lists),
    append(Event_Lists, Events).

merge_list_both_elem(Schema, Prefixes, _Type, AssocB, Winner, Path, OursElt, TheirsElt, Merged, Events) :-
    element_key(Schema, Prefixes, OursElt, Key),
    (   memberchk(Key-B, AssocB)
    ->  merge_value(Schema, Prefixes, unknown, B, OursElt, TheirsElt, Winner, Path, Merged, Events)
    ;   merge_value(Schema, Prefixes, unknown, missing, OursElt, TheirsElt, Winner, Path, Merged, Events)).

assoc_to_list(Assoc, Pairs) :-
    (   is_dict(Assoc) -> dict_pairs(Assoc, _, Pairs) ; Pairs = Assoc ).

% merge_set(+Schema, +Prefixes, +Type, +Base, +Ours, +Theirs, +Winner, +Path, -Result, -Events) is det.
%
% Unordered three-way merge of Set elements by stable identity. Divergent
% membership changes union additions and removals only under an explicit
% merge resolution.
merge_set(Schema, Prefixes, Type, Base, Ours, Theirs, Winner, Path, Result, Events) :-
    element_keys(Schema, Prefixes, Base, KeysB),
    element_keys(Schema, Prefixes, Ours, KeysO),
    element_keys(Schema, Prefixes, Theirs, KeysT),
    sort(KeysB, SortedB), sort(KeysO, SortedO), sort(KeysT, SortedT),
    (   SortedO == SortedB
    ->  merge_list_theirs(Schema, Prefixes, Type, Base, Ours, Theirs, Winner, Path, Result, Events)
    ;   SortedT == SortedB
    ->  merge_list_ours(Schema, Prefixes, Type, Base, Ours, Theirs, Winner, Path, Result, Events)
    ;   SortedO == SortedT
    ->  merge_set_changed(Schema, Prefixes, Type, Base, Ours, Theirs, Winner, Path, Result, Events)
    ;   Winner == merge
    ->  merge_set_changed(Schema, Prefixes, Type, Base, Ours, Theirs, Winner, Path, Result, Rest_Events),
        Events = [resolution(Path, merge)|Rest_Events]
    ;   conflict(Path, Base, Ours, Theirs)).

merge_set_changed(Schema, Prefixes, _Type, Base, Ours, Theirs, Winner, Path, Result, Events) :-
    element_assoc(Schema, Prefixes, Base, AssocB),
    element_assoc(Schema, Prefixes, Ours, AssocO),
    element_assoc(Schema, Prefixes, Theirs, AssocT),
    assoc_to_list(AssocB, BasePairs),
    assoc_to_list(AssocT, TheirsPairs),
    maplist(merge_set_ours_elem(Schema, Prefixes, AssocB, AssocT, Winner, Path), Ours, KeptWithMarkers, Event_Lists),
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
    append(KeptOurs, ExtraTheirs, Result),
    append(Event_Lists, Events).

memberchk_in_assoc(Assoc, Key) :- memberchk(Key-_, Assoc).

assoc_keys(Assoc, Keys) :-
    assoc_to_list(Assoc, Pairs),
    maplist([K-_, K]>>true, Pairs, Keys).

theirs_key_elem(Assoc, Key, Elt) :- memberchk(Key-Elt, Assoc).

merge_set_ours_elem(Schema, Prefixes, AssocB, AssocT, Winner, Path, OursElt, Merged, Events) :-
    element_key(Schema, Prefixes, OursElt, Key),
    (   memberchk(Key-B, AssocB)
    ->  (   memberchk(Key-T, AssocT)
        ->  merge_value(Schema, Prefixes, unknown, B, OursElt, T, Winner, Path, Merged, Events)
        ;   OursElt == B
        ->  Merged = removed,
            Events = []
        ;   conflict(Path, B, OursElt, missing))
    ;   (   memberchk(Key-T, AssocT)
        ->  (   OursElt == T
            ->  Merged = OursElt,
                Events = []
            ;   conflict(Path, missing, OursElt, T))
        ;   Merged = OursElt,
            Events = [])).
