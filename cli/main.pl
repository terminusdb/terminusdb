:- module(cli, [cli_toplevel/0]).

/** <module> HTTP server module
 *
 * This module implements the database server. It is primarily composed
 * of a number of RESTful APIs which exchange information in JSON format
 * over HTTP. This is intended as a mechanism for interprocess
 * communication via *API* and not as a fully fledged high performance
 * server.
 *
 **/

:- use_module(core(query/json_woql),[initialise_woql_contexts/0]).

cli_toplevel :-
    current_prolog_flag(argv, Argv),
    initialise_woql_contexts,
    get_time(Now),
    format_time(string(StrTime), '%A, %b %d, %H:%M:%S %Z', Now),
    http_log('terminusdb-server started at ~w (utime ~w) args ~w~n',
             [StrTime, Now, Argv]),
    run(Argv).

read_loop :-
    read(_),
    read_loop.

run([test]) :-
    !,
    run_tests,
    halt.
run([serve|Args]) :-
    !,
    terminus_server([server]),
    % Maybe read stdin here?
    (   member(interactive, Args)
    ->  prolog
    ;   read_loop).
run([ls|_Databases]) :-
    !,
    % show beautiful tree diagram with databases and branches
    true.
run(Args) :-
    process_cli(Args).

initialise_hup :-
    (   current_prolog_flag(unix, true)
    ->  on_signal(hup, _, hup)
    ;   true).

:- initialise_hup.

initialise_log_settings :-
    (   getenv('TERMINUSDB_LOG_PATH', Log_Path)
    ->  set_setting(http:logfile, Log_Path)
    ;   get_time(Time),
        asserta(http_log:log_stream(user_error, Time))).

:- multifile prolog:message//1.

prolog:message(server_missing_config(BasePath)) -->
    [
    'CRITICAL ERROR: Server can\'t be started because the configuration is missing',
    nl,
    nl,
    'Run: ~s/utils/db_init first'-[BasePath],
    nl
    ].

process_cli(_) :-
    format("~nUsage: terminusdb [OPTION]~n",[]),
    format("~n~n",[]),
    format("serve\t\t\trun the terminusdb server~n",[]),
    format("list\t\t\tlist databases~n",[]),
    format("query [QUERY]\trun query~n",[]),
    format("test\t\t\trun tests~n",[]),
    halt.

