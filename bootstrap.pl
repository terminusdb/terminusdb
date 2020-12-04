#!/usr/bin/env swipl

/*
 *
 */

:- set_test_options([run(manual), load(always)]).

:- [load_paths].

:- reexport(core(util/syntax)).

:- use_module(server(routes)).
:- use_module(server(main)).
:- use_module(cli(main)).
:- use_module(core(query)).
:- use_module(core(api)).

:- use_module(library(http/http_log)).

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
