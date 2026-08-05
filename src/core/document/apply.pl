:- module('document/apply',
          [
              apply_diff/4,
              apply_diff_ids_captures/7
          ]).

:- use_module(core(util)).
:- use_module(core('util/tables')).

:- use_module(library(apply)).
:- use_module(library(assoc)).
:- use_module(library(dicts)).
:- use_module(library(lists)).
:- use_module(library(when)).
:- use_module(library(yall)).
:- use_module(library(plunit)).
:- use_module(library(option)).

:- use_module(core(document/patch)).
:- use_module(core(document/json)).
:- use_module(core(document/normalize)).
:- use_module(core(transaction/database)).

% apply_diff(+Context, +Diff, Conflict ,Options)
%
% Apply a diff to a commit.
%
% [matches_final_state(true)] (default is false)
apply_diff(Context, Diff, Conflict, Options) :-
    get_dict('@delete', Diff, Delete_Candidate),
    !,
    (   is_dict(Delete_Candidate)
    ->  do_or_die(get_dict('@id', Delete_Candidate, Delete_ID),
                  error(missing_field('@id', Delete_Candidate), _))
    ;   (   string(Delete_Candidate)
        ;   atom(Delete_Candidate)
        )
    ->  Delete_Candidate = Delete_ID
    ;   throw(error(missing_field('@id', Delete_Candidate), _))
    ),
    catch(
        (   delete_document(Context, Delete_ID),
            Conflict = null
        ),
        error(document_not_found(ID), _),
        (   option(match_final_state(true), Options)
        ->  Conflict = null
        ;   Conflict = json{ '@op' : 'DeleteConflict',
                             '@id_does_not_exists' : ID
                           }
        )
    ).
apply_diff(Context, Diff, Conflict, Options) :-
    get_dict('@insert', Diff, Insert),
    !,
    catch(
        (   insert_document(Context, Insert, _Inserted_Id),
            Conflict = null
        ),
        error(can_not_insert_existing_object_with_id(Id), _),
        (   option(match_final_state(true), Options),
            query_context_transaction_objects(Context, [Transaction]),
            normalize_document(Transaction, Insert, Normalized_Insert),
            get_document(Context,Id,Document),
            Document = Normalized_Insert
        ->  Conflict = null
        ;   Conflict = json{ '@op' : 'InsertConflict',
                             '@id_already_exists' : Id }
        )
    ).
apply_diff(Context, Diff, Conflict, Options) :-
    do_or_die(
        get_dict('@id', Diff, ID),
        error(missing_field('@id', Diff), _)
    ),
    get_document(Context, ID, JSON_In),
    simple_patch(Diff,JSON_In,Result,Options),
    (   Result = success(JSON_Out)
    ->  replace_document(Context, JSON_Out, _),
        Conflict = null
    ;   Result = conflict(Conflict_Prototype),
        put_dict(_{ '@id' : ID }, Conflict_Prototype, Conflict)
    ).


apply_diff_ids_captures(Context, Diff, Conflict, Ids, Options, Captures_In, Captures_Out) :-
    get_dict('@delete', Diff, Delete_Candidate),
    !,
    (   is_dict(Delete_Candidate)
    ->  do_or_die(get_dict('@id', Delete_Candidate, Delete_ID),
                  error(missing_field('@id', Delete_Candidate), _))
    ;   (string(Delete_Candidate) ; atom(Delete_Candidate))
    ->  Delete_Candidate = Delete_ID
    ;   throw(error(missing_field('@id', Delete_Candidate), _))
    ),
    catch(
        (   delete_document(Context, Delete_ID),
            Conflict = null,
            Ids = [Delete_ID],
            Captures_Out = Captures_In
        ),
        error(document_not_found(ID), _),
        (   option(match_final_state(true), Options)
        ->  Conflict = null,
            Ids = [],
            Captures_Out = Captures_In
        ;   Conflict = json{ '@op' : 'DeleteConflict',
                             '@id_does_not_exists' : ID },
            Ids = [],
            Captures_Out = Captures_In
        )
    ).
apply_diff_ids_captures(Context, Diff, Conflict, Ids, Options, Captures_In, Captures_Out) :-
    get_dict('@insert', Diff, Insert),
    !,
    catch(
        (   insert_document(Context, Insert, false, Captures_In, Inserted_Ids, _Dependencies, Captures_Out),
            Inserted_Ids = [Inserted_Id|_],
            Conflict = null,
            Ids = [Inserted_Id]
        ),
        error(can_not_insert_existing_object_with_id(Id), _),
        (   option(match_final_state(true), Options),
            query_context_transaction_objects(Context, [Transaction]),
            normalize_document(Transaction, Insert, Normalized_Insert),
            get_document(Context, Id, Document),
            Document = Normalized_Insert
        ->  Conflict = null,
            Ids = [],
            Captures_Out = Captures_In
        ;   Conflict = json{ '@op' : 'InsertConflict',
                             '@id_already_exists' : Id },
            Ids = [],
            Captures_Out = Captures_In
        )
    ).
apply_diff_ids_captures(Context, Diff, Conflict, Ids, Options, Captures_In, Captures_Out) :-
    do_or_die(
        get_dict('@id', Diff, ID),
        error(missing_field('@id', Diff), _)
    ),
    get_dict(prefixes, Context, Prefixes),
    normalize_diff(Prefixes, Diff, Normalized_Diff),
    resolve_refs_in_diff(Captures_In, Normalized_Diff, Resolved_Diff),
    get_document(Context, ID, JSON_In),
    put_dict(options{prefixes:Prefixes}, Options, Options_With_Prefixes),
    simple_patch(Resolved_Diff,JSON_In,Result,Options_With_Prefixes),
    (   Result = success(JSON_Out)
    ->  replace_document(Context, JSON_Out, false, false, Captures_In, Ids, _Dependencies, Captures_Out),
        Conflict = null
    ;   Result = conflict(Conflict_Prototype),
        Ids = [],
        Captures_Out = Captures_In,
        put_dict(_{ '@id' : ID }, Conflict_Prototype, Conflict)
    ).

normalize_diff(Prefixes, Diff, Normalized),
is_dict(Diff) =>
    dict_pairs(Diff, _, Pairs),
    maplist(normalize_pairs(Prefixes), Pairs, New_Pairs),
    dict_create(Normalized, json, New_Pairs).
normalize_diff(Prefixes, Diff, Normalized),
is_list(Diff) =>
    maplist(normalize_diff(Prefixes),Diff,Normalized).
normalize_diff(_, Diff, Normalized) =>
    Diff = Normalized.

normalize_pairs(Prefixes,Key-Value, Result),
Key = '@type' =>
    Result = New_Key-New_Value,
    New_Key = '@type',
    compress_schema_uri(Value, Prefixes, New_Value, [compress_ids(true)]).
normalize_pairs(Prefixes,Key-Value, Result),
Key = '@id' =>
    Result = New_Key-New_Value,
    New_Key = '@id',
    compress_dict_uri(Value, Prefixes, New_Value, [compress_ids(true)]).
normalize_pairs(Prefixes,Key-Value, Result) =>
    Result = New_Key-New_Value,
    compress_schema_uri(Key, Prefixes, New_Key, [compress_ids(true)]),
    normalize_diff(Prefixes, Value, New_Value).

%% resolve_refs_in_diff(+Captures, +Diff, -Resolved) is det.
%
% Walks a diff structure and replaces any {"@ref":"name"} dict with the
% actual document ID string stored in the captures assoc. This allows
% field-level patches (SwapValue) to use @ref in @before/@after values
% when the target is a document reference field.
%
resolve_refs_in_diff(Captures, Diff, Resolved),
is_dict(Diff),
get_dict('@ref', Diff, Ref),
\+ get_dict('@op', Diff, _) =>
    (   get_assoc(Ref, Captures, Id)
    ->  Resolved = Id
    ;   throw(error(not_all_captures_found([Ref]), _))
    ).
resolve_refs_in_diff(Captures, Diff, Resolved),
is_dict(Diff) =>
    dict_pairs(Diff, _, Pairs),
    maplist(resolve_refs_pairs(Captures), Pairs, New_Pairs),
    dict_create(Resolved, json, New_Pairs).
resolve_refs_in_diff(Captures, Diff, Resolved),
is_list(Diff) =>
    maplist(resolve_refs_in_diff(Captures), Diff, Resolved).
resolve_refs_in_diff(_, Diff, Resolved) =>
    Diff = Resolved.

resolve_refs_pairs(Captures, Key-Value, Key-New_Value) :-
    resolve_refs_in_diff(Captures, Value, New_Value).

:- begin_tests(resolve_refs_in_diff).

test(basic_ref_resolution, []) :-
    empty_assoc(Empty),
    put_assoc("PersonB", Empty, "http://somewhere.for.now/document/Person/Bob", Captures),
    Diff = json{friend:json{'@ref':"PersonB"}},
    resolve_refs_in_diff(Captures, Diff, Resolved),
    get_dict(friend, Resolved, Friend),
    atom_string(Friend, "http://somewhere.for.now/document/Person/Bob").

test(nested_ref_in_swapvalue, []) :-
    empty_assoc(Empty),
    put_assoc("PersonA", Empty, "http://somewhere.for.now/document/Person/Alice", C1),
    put_assoc("PersonB", C1, "http://somewhere.for.now/document/Person/Bob", Captures),
    Diff = json{'@op':"SwapValue",
                '@before':json{'@ref':"PersonB"},
                '@after':json{'@ref':"PersonA"}},
    resolve_refs_in_diff(Captures, Diff, Resolved),
    get_dict('@before', Resolved, Before),
    get_dict('@after', Resolved, After),
    atom_string(Before, "http://somewhere.for.now/document/Person/Bob"),
    atom_string(After, "http://somewhere.for.now/document/Person/Alice").

test(no_refs_passthrough, []) :-
    empty_assoc(Captures),
    Diff = json{name:"Alice", age:30},
    resolve_refs_in_diff(Captures, Diff, Resolved),
    Resolved = Diff.

test(ref_in_list, []) :-
    empty_assoc(Empty),
    put_assoc("PersonA", Empty, "http://somewhere.for.now/document/Person/Alice", Captures),
    Diff = [json{'@ref':"PersonA"}, "plain"],
    resolve_refs_in_diff(Captures, Diff, Resolved),
    nth0(0, Resolved, First),
    atom_string(First, "http://somewhere.for.now/document/Person/Alice"),
    nth0(1, Resolved, Second),
    atom_string(Second, "plain").

test(missing_capture_throws, []) :-
    empty_assoc(Captures),
    Diff = json{'@ref':"NonExistent"},
    catch(resolve_refs_in_diff(Captures, Diff, _), Error, true),
    nonvar(Error),
    Error = error(not_all_captures_found(["NonExistent"]), _).

test(op_dict_not_treated_as_ref, []) :-
    empty_assoc(Empty),
    put_assoc("PersonB", Empty, "http://somewhere.for.now/document/Person/Bob", Captures),
    Diff = json{'@op':"SwapValue",
                '@before':json{'@ref':"PersonB"},
                '@after':"new_value"},
    resolve_refs_in_diff(Captures, Diff, Resolved),
    get_dict('@op', Resolved, "SwapValue"),
    get_dict('@before', Resolved, Before),
    atom_string(Before, "http://somewhere.for.now/document/Person/Bob"),
    get_dict('@after', Resolved, "new_value").

:- end_tests(resolve_refs_in_diff).

/*
Schematic of application
            apply
            (a-b)
     main
---a-------c-[d]
    \
     \_____b
      dev

*/
