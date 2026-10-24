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
:- use_module(core(util/plunit_json_reporter)).

:- set_test_options([run(manual), load(always), concurrent(true)]).

:- use_module(server(routes)).
:- use_module(core(plugin_api)).
:- use_module(core(plugin_raw_api)).
:- use_module(server(main)).
:- use_module(cli(main)).
:- use_module(core(query)).
:- use_module(core(api)).
:- use_module(config(terminus_config)).

:- use_module(library(qsave)).

% Load shared message hooks for error handling
:- [bootstrap_hooks].

main :-
    initialize_flags,
    bootstrap_files,
    bootstrap_config_files,
    % no_save: librust loads from a known on-disk path instead of being
    % extracted from the state on every launch (~2s saved per invocation on
    % Linux, plus AMFI kills on macOS for the unsigned extracted copy).
    % file_search_path(foreign) carries a candidate for every install
    % location this Makefile produces (see load_paths:add_rust_path/0).
    qsave_program(terminusdb, [
                      foreign(no_save),
                      undefined(error),
                      toplevel(cli_toplevel),
                      autoload(true),  % Include all autoloaded modules for snap compatibility
                      stand_alone(true)
                  ]).
