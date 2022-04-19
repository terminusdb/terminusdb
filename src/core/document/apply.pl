:- module('document/apply',
          [
              apply_diff/4
          ]).

:- use_module(core(util)).
:- use_module(core('util/tables')).

:- use_module(library(apply)).
:- use_module(library(dicts)).
:- use_module(library(lists)).
:- use_module(library(when)).
:- use_module(library(yall)).
:- use_module(library(plunit)).

:- use_module(core(document/patch)).
:- use_module(core(document/json)).

% apply_diff(+Context, +Diff, Conflict ,Options)
%
% Apply a diff to a commit.
%
% [matches_final_state(true)] (default is false)
apply_diff(Context, Diff, null, _Options) :-
    get_dict('@delete', Diff, Delete_ID),
    delete_document(Context, Delete_ID).
apply_diff(Context, Diff, null, _Options) :-
    get_dict('@insert', Diff, Insert),
    insert_document(Context, Insert, _Inserted_Id).
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
    ;   Result = success(Conflict)
    ).






/*

     main    (a-b)
---a-------c  [o]
    \
     \_____b
      dev

*/
