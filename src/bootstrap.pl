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

% Catch low level HTTP-related warnings (e.g., malformed cookies) to prevent server crashes
% in the compiled binary. These warnings are logged but don't halt execution.
% This allows the server to gracefully handle invalid cookies like:
%   Cookie: react-resizable-panels:layout=[15,85]; path=/
user:message_hook(http(Term), warning, Lines) :-
    % Log the warning to stderr so it's visible in server output
    format(user_error, '~N[WARNING] HTTP: ~w~n', [Term]),
    % Print the detailed message lines
    (   Lines \= []
    ->  print_message_lines(user_error, '', Lines)
    ;   true
    ),
    % Return true to prevent the warning from crashing the server
    true.

% Catch general syntax errors in HTTP headers to prevent crashes
user:message_hook(syntax_error(Term), warning, Lines) :-
    % Check if this is an HTTP-related syntax error by examining the error term
    (   functor(Term, http, _)
    ;   functor(Term, cookie, _)
    ;   functor(Term, header, _)
    ),
    % Log the warning
    format(user_error, '~N[WARNING] HTTP Syntax Error: ~w~n', [Term]),
    (   Lines \= []
    ->  print_message_lines(user_error, '', Lines)
    ;   true
    ),
    % Return true to prevent crash
    true.

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
