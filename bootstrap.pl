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

:- [load_paths].

:- reexport(core(util/syntax)).

:- use_module(server(routes)).
:- use_module(server(main)).
:- use_module(cli(main)).
:- use_module(core(query)).
:- use_module(core(api)).

:- use_module(library(http/http_log)).

:- set_test_options([run(manual)]).

main :-
    initialise_woql_contexts,
    bootstrap_files,
    qsave_program(terminusdb, [
                      foreign(save),
                      undefined(error),
                      toplevel(cli_toplevel),
                      autoload(true),
                      stand_alone(true)
                  ]).
