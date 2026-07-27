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

:- use_module(library(main)).
:- initialization(main).

:- use_module(library(settings)).

initialise_signals :-
    (   current_prolog_flag(unix, true)
    ->  on_signal(hup, _, shutdown_signal),
        on_signal(term, _, shutdown_signal),
        on_signal(int, _, shutdown_signal)
    ;   true).

:- initialise_signals.


initialise_log_settings :-
    get_time(Time),
    asserta(http_log:log_stream(user_error, Time)).

:-multifile prolog:message//1.

prolog:message(server_missing_config(BasePath)) -->
    [
    'CRITICAL ERROR: Server can\'t be started because the configuration is missing',
    nl,
    nl,
    'Run: ~s/utils/db_init first'-[BasePath],
    nl
    ].

% Catch HTTP-related warnings (e.g., malformed cookies) to prevent server crashes
% These warnings are logged but don't halt execution.
% This allows the server to gracefully handle invalid cookies like:
%   Cookie: react-resizable-panels:layout=[15,85]; path=/
user:message_hook(http(Term), warning, Lines) :-
    % Use centralized logging from json_log module with fallback
    catch(
        json_log:log_http_warning('HTTP', Term, Lines),
        _Error,
        % Fallback if json_log not loaded yet (early in boot process)
        format(user_error, '~N[WARNING] HTTP: ~w~n', [Term])
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
    % Use centralized logging with fallback
    catch(
        json_log:log_http_warning('HTTP Syntax Error', Term, Lines),
        _Error,
        format(user_error, '~N[WARNING] HTTP Syntax Error: ~w~n', [Term])
    ),
    % Return true to prevent crash
    true.

:- use_module(server(routes)).
:- use_module(server(main)).

:- use_module(library(plunit)).
:- use_module(core(util/plunit_json_reporter)).

% Plugins
%:- use_module(plugins(registry)).

:- use_module(core(query/json_woql),[initialise_woql_contexts/0]).
:- use_module(core(api), [initialize_flags/0, bootstrap_files/0]).
:- use_module(config(terminus_config)).

:- set_test_options([run(manual),concurrent(true)]).

:- use_module(cli(main)).
:- use_module(library(debug)).

shutdown_signal(_Signal) :-
    thread_send_message(main, stop).

main(_Argv) :-
    initialize_flags,
    initialise_log_settings,
    bootstrap_config_files,
    bootstrap_files,
    debug(terminus(main), 'initialise_woql_contexts completed', []),
    debug(terminus(main), 'initialise_log_settings completed', []),
    cli_toplevel.
