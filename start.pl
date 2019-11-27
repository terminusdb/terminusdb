#!/usr/bin/env swipl

/*
 *  This file is part of TerminusDB.
 *
 *  TerminusDB is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, under version 3 of the License, or
 *  (at your option) any later version.
 *
 *  TerminusDB is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with TerminusDB.  If not, see <https://www.gnu.org/licenses/>.
 *
 */

:- initialization(main).

%!  version_string(+Int:integer, -Str:string) is det
%
%   convert an integer form version number as supplied by
%   the =|version|= flag of =|current_prolog_flag/2|=
%   to a string representation
%
version_string(Int, Str) :-
    Major is floor( Int / 10_000 ) rem 100,
    Minor is floor( Int / 100 ) rem 100,
    Patch is Int rem 100,
    format(string(Str), '~w.~w.~w', [Major, Minor, Patch]).

:- multifile prolog:message//1.

prolog:message(error(version_error(Correct, Was), _)) -->
      { version_string(Correct, C),
        version_string(Was, W)
      },
      [ 'Run TerminusDB using SWI-Prolog version ~w, you are on ~w'-[C, W],
        nl].

needs_version(80110).
needs_version(80003).

must_be_proper_version :-
    needs_version(Correct),
    (   current_prolog_flag(version, Correct)
    ->   true
    ;
        current_prolog_flag(version, V),
        print_message(error,
                      error(version_error(Correct, V), terminus_db_startup)),
        halt
   ).

:- initialization must_be_proper_version.


:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.

:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(terminus_home, Dir)).

add_library_path :-
    user:file_search_path(terminus_home, Dir),
    atom_concat(Dir,'/library',Library),
    asserta(user:file_search_path(library, Library)).

:- add_library_path.

add_config_path :-
    user:file_search_path(terminus_home, Dir),
    atom_concat(Dir,'/config',Config),
    asserta(user:file_search_path(config, Config)).

:- add_config_path.

add_test_path :-
    user:file_search_path(terminus_home, Dir),
    atom_concat(Dir,'/test',Config),
    asserta(user:file_search_path(test, Config)).

:- add_test_path.

initialise_server_settings :-
    file_search_path(terminus_home, BasePath),
    !,
    atom_concat(BasePath, '/config/config.pl', Settings_Path),
    (   exists_file(Settings_Path)
    ->  true
    ;   format("CRITICAL ERROR: Server can't be started because the configuration is missing~n~nRun: ~s/utils/db_util first~n", BasePath),
        halt(10)
    ).

:- initialise_server_settings.

:- use_module(library(api)).
:- use_module(library(server)).
:- use_module(library(upgrade_db)).
:- use_module(library(prefixes)).
% We only need this if we are interactive...
:- use_module(library(sdk)).
:- use_module(test(tests)).

main(Argv) :-
    maybe_upgrade,
    initialise_prefix_db,
    initialise_contexts,
    server(Argv),
    (   Argv == [test]
    ->  run_tests
    ;   true
    ).
