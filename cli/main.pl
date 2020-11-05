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
:- use_module(core(api)).
:- use_module(core(triple)).

cli_toplevel :-
    current_prolog_flag(argv, Argv),
    initialise_woql_contexts,
    initialise_log_settings,
    run(Argv).

run([test]) :-
    !,
    run_tests,
    halt.
run([serve|Args]) :-
    !,
    (   member(interactive, Args)
    ->  terminus_server([serve|Args], false),
        prolog
    ;   terminus_server([serve|Args], true)).
% run([describe|Args]) :- true.
run([list|Databases]) :-
    !,
    super_user_authority(Auth),
    list_databases(system_descriptor{}, Auth, Databases, Database_Objects),
    pretty_print_databases(Database_Objects).
run([optimize|_Databases]) :-
    !,
    %optimize_stuff_here
    true.
% run([push|_Databases])
% run([pull|_Databases])
% run([query|_Query])
%% run([query|Args]) :-
%%     (   Args = ['--js',Query]
%%     ->  run_js_code(Query, JSON)
%%     ;   Args = ['--python',Query]
%%     ->  run_python_code(Query, JOSN)
%%     ;   Args = [Query]
%%     ->  run_prolog(Query)
%%     ).
run(_) :-
    help_screen.

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

help_screen :-
    format("~nUsage: terminusdb [OPTION]~n",[]),
    format("~n~n",[]),
    format("serve\t\t\trun the terminusdb server~n",[]),
    format("list [Databases]\t\tlist databases~n",[]),
    format("query [QUERY]\t\trun query~n",[]),
    format("test\t\t\trun tests~n",[]).

