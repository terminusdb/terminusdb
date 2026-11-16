:- use_module(library(prolog_xref)).
:- use_module(library(lists)).

:- dynamic banned_failed/1.
:- dynamic banned_predicate/2.    % "name/arity", Why — .lint_config.pl
:- dynamic banned_grandfathered/1. % File with existing violations; shrinks over time
:- dynamic ignore_file/1.          % loaded from .lint_config.pl at runtime

load_rules :-
    catch(consult('./.lint_config.pl'), _, true).

% A call violates when it resolves to a banned predicate provided by
% library(json) or library(http/json). Qualified calls are checked by
% their module; unqualified calls by which file the import resolves to,
% so a bare json_read_dict imported from json_preserve stays legal.
violates(_File, Module:Inner, PredName) :-
    banned_module(Module),
    !,
    functor(Inner, Name, Arity),
    format(string(PredName), "~w/~w", [Name, Arity]),
    banned_predicate(PredName, _).
violates(File, Goal, PredName) :-
    functor(Goal, Name, Arity),
    format(string(Bare), "~w/~w", [Name, Arity]),
    banned_predicate(Bare, _),
    (   xref_defined(File, Goal, imported(Source)),
        banned_source_file(Source)
    ->  PredName = Bare
    ).

banned_module(json).
banned_module(http_json).

banned_source_file(Source) :-
    file_base_name(Source, Base),
    member(Base, ['json.pl', 'json']),
    \+ sub_atom(Source, _, _, _, '/terminusdb/').

grandfathered(File) :-
    atom_string(File, String),
    banned_grandfathered(String).

skipped(Path) :-
    atom_string(Path, String),
    catch(ignore_file(String), _, fail).

check_banned_file(File) :-
    xref_source(File),
    forall((xref_called(File, Goal, _, _, Line),
            violates(File, Goal, PredName),
            \+ grandfathered(File)),
        (   format("ERROR: ~s:~d| Banned call to ~s — use json_preserve~n",
                   [File, Line, PredName]),
            assertz(banned_failed(true))
        )).

check_banned_path(Path) :-
    skipped(Path),
    !.
check_banned_path(File) :-
    file_name_extension(_, pl, File),
    !,
    check_banned_file(File).
check_banned_path(Dir) :-
    exists_directory(Dir),
    !,
    directory_files(Dir, Entries),
    forall((member(E, Entries), E \= '.', E \= '..',
            atomic_list_concat([Dir, '/', E], P)),
           check_banned_path(P)).
check_banned_path(_).

banned_calls :-
    load_rules,
    check_banned_path('.'),
    (   banned_failed(true) -> halt(1) ; halt(0) ).
