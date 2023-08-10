:- module('document/apply',
          [
              apply_diff/4,
              apply_diff_ids_captures/7
          ]).

:- use_module(core(util)).
:- use_module(core('util/tables')).

:- use_module(library(apply)).
:- use_module(library(dicts)).
:- use_module(library(lists)).
:- use_module(library(when)).
:- use_module(library(yall)).
:- use_module(library(plunit)).
:- use_module(library(option)).

:- use_module(core(document/patch)).
:- use_module(core(document/json)).
:- use_module(core(query/jsonld)).

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
            get_document(Context,Id,Document),
            Document = Insert
        ->  Conflict = null
        ;   Conflict = json{ '@op' : 'InsertConflict',
                             '@id_already_exists' : Id }
        )
    ).
apply_diff(Context, Diff, Conflict, Options) :-
    do_or_die(
        get_dict('@id', Diff, Id_Short),
        error(missing_field('@id', Diff), _)
    ),
    get_dict(prefixes,Context,Prefixes),
    prefix_expand(Id_Short,Prefixes,Id),
    get_document(Context, Id, JSON_Pre),
    put_dict(_{ '@id' : Id}, JSON_Pre, JSON_In),
    simple_patch(Diff,JSON_In,Result,Options),
    (   Result = success(JSON_Out)
    ->  replace_document(Context, JSON_Out, _),
        Conflict = null
    ;   Result = conflict(Conflict_Prototype),
        put_dict(_{ '@id' : Id }, Conflict_Prototype, Conflict)
    ).


apply_diff_ids_captures(Context, Diff, Conflict, Ids, Options, Captures_In, Captures_Out) :-
    do_or_die(
        get_dict('@id', Diff, ID),
        error(missing_field('@id', Diff), _)
    ),
    get_dict(prefixes, Context, Prefixes),
    normalize_diff(Prefixes, Diff, Normalized_Diff),
    get_document(Context, ID, JSON_In),
    simple_patch(Normalized_Diff,JSON_In,Result,Options),
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
