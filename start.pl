#!/usr/bin/env swipl

/* 
 *  This file is part of TerminusDB.
 *
 *  TerminusDB is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
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
    ;   atom_concat(BasePath, '/config/config-example.pl',
                    Example_Settings_Path),
        copy_file(Example_Settings_Path,Settings_Path)
    ).

:- initialise_server_settings.

:- use_module(library(api)).
:- use_module(library(server)).
:- use_module(library(upgrade_db)).
:- use_module(library(prefixes)).
% We only need this if we are interactive...
:- use_module(library(sdk)).
% Might be sensible to trigger tests with an Argv switch
% :- use_module(test(api_test)).

main(Argv) :-
    maybe_upgrade,
    initialise_prefix_db,
    server(Argv).
 
