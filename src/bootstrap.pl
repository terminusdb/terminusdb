#!/usr/bin/env swipl

/*
 *
 */

:- [load_paths].

:- reexport(core(util/syntax)).

:- use_module(core(util)).

:- set_test_options([run(manual), load(always), concurrent(true)]).

:- use_module(server(routes)).
:- use_module(server(main)).
:- use_module(cli(main)).
:- use_module(core(query)).
:- use_module(core(api)).

:- use_module(library(http/http_log)).
:- use_module(library(qsave)).


main :-
    initialise_woql_contexts,
    bootstrap_files,
    bootstrap_config_files,
    qsave_program(terminusdb, [
                      foreign(save),
                      undefined(error),
                      toplevel(cli_toplevel),
                      autoload(false),
                      stand_alone(true)
                  ]).
