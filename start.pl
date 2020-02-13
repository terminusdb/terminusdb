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
      {
        maplist(version_string, Correct, VersionStrings),
        atomic_list_concat(VersionStrings, " or ", AcceptedVersions),
        version_string(Was, W)
      },
      [ 'Run TerminusDB using SWI-Prolog version ~w, you are on ~w'-[AcceptedVersions, W],
        nl].

needs_version(80111).
needs_version(80110).
needs_version(80003).

must_be_proper_version :-
    current_prolog_flag(version, RunningVersion),
    (    needs_version(RunningVersion)
    ->   true
    ;
        findall(Version, needs_version(Version), SupportedVersions),
        print_message(error,
                      error(version_error(SupportedVersions, RunningVersion), terminus_db_startup)),
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

add_plugin_path :-
    user:file_search_path(terminus_home, Dir),
    atom_concat(Dir,'/plugins',Config),
    asserta(user:file_search_path(plugins, Config)).

:- add_plugin_path.

initialise_server_settings :-
    file_search_path(terminus_home, BasePath),
    !,
    atom_concat(BasePath, '/config/config.pl', Settings_Path),
    (   exists_file(Settings_Path)
    ->  true
    ;   print_message(error, server_missing_config(BasePath)),
        halt(10)
    ).

initialise_log_settings :-
    file_search_path(terminus_home, BasePath),
    !,
    (   getenv('TERMINUS_LOG_PATH', Log_Path)
    ->  true
    ;   atom_concat(BasePath,'/storage/httpd.log', Log_Path)),

    set_setting(http:logfile, Log_Path).

:-multifile prolog:message//1.

prolog:message(server_missing_config(BasePath)) -->
    [
    'CRITICAL ERROR: Server can\'t be started because the configuration is missing',
    nl,
    nl,
    'Run: ~s/utils/db_util first'-[BasePath],
    nl
    ].


:- initialise_server_settings.

:- use_module(library(api)).
:- use_module(library(server)).
:- use_module(library(upgrade_db)).
:- use_module(library(prefixes)).
% We only need this if we are interactive...
:- use_module(library(sdk)).
:- use_module(test(tests)).
:- use_module(library(http/http_log)).
% Plugins
%:- use_module(plugins(registry)).

main(Argv) :-
    get_time(Now),
    format_time(string(StrTime), '%A, %b %d, %H:%M:%S %Z', Now),
    http_log('terminus-server started at ~w (utime ~w) args ~w~n',
             [StrTime, Now, Argv]),
    %maybe_upgrade,
    initialise_prefix_db,
    debug(terminus(main), 'prefix_db initialized', []),
    initialise_contexts,
    debug(terminus(main), 'initialise_contexts completed', []),
    initialise_log_settings,
    debug(terminus(main), 'initialise_log_settings completed', []),
    server(Argv),
    (   Argv == [test]
    ->  run_tests
    ;   true
    ).
