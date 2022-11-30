#!/usr/bin/env swipl

/*
 *
 */

:- [load_paths].
:- reexport(core(util/syntax)).
:- use_foreign_library(foreign(librust)).

:- use_module(config(terminus_config)).
:- if(is_enterprise).
:- use_module(enterprise(init_enterprise)).
:- endif.

:- use_module(core(util)).
:- use_module(library(plunit)).

:- set_test_options([run(manual), load(always), concurrent(true)]).

:- use_module(server(routes)).
:- use_module(server(main)).
:- use_module(cli(main)).
:- use_module(core(query)).
:- use_module(core(api)).
:- use_module(config(terminus_config)).

:- use_module(library(qsave)).

% Ignore warnings from `qsave.pl` that occur on macOS. When the next
% SWI-Prolog is released, it should have the following PR, which would allow us
% to remove this `user:message_hook`.
% <https://github.com/SWI-Prolog/swipl/pull/22>
user:message_hook(qsave(strip_failed(_)), warning, _).

main :-
    initialize_flags,
    bootstrap_files,
    bootstrap_config_files,
    qsave_program(terminusdb, [
                      foreign(save),
                      undefined(error),
                      toplevel(cli_toplevel),
                      autoload(true),
                      stand_alone(true)
                  ]).
