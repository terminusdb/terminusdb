#!/usr/bin/env swipl

/*
 *
 */

:- [load_paths].
:- reexport(core(util/syntax)).
:- use_foreign_library(foreign(librust)).
:- if(is_enterprise).
:- use_module(enterprise(init_enterprise)).
:- endif.

:- use_module(core(util)).
:- use_module(library(plunit)).

:- set_test_options([run(manual), load(always)]).

:- use_module(server(routes)).
:- use_module(server(main)).
:- use_module(cli(main)).
:- use_module(core(query)).
:- use_module(core(api)).
:- use_module(config(terminus_config)).

:- use_module(library(qsave)).

main :-
    initialize_flags,
    bootstrap_files,
    bootstrap_config_files,
    qsave_program(terminusdb, [
                      foreign(save),
                      undefined(error),
                      toplevel(cli_toplevel),
                      autoload(false),
                      stand_alone(true)
                  ]).
