:- module(cli, [cli_toplevel/0, run/1]).

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
:- use_module(library(http/json)).
:- use_module(core(query)).

cli_toplevel :-
    current_prolog_flag(argv, Argv),
    initialise_woql_contexts,
    initialise_log_settings,
    run(Argv).


key_value_args_default(_,Default,[],Default).
key_value_args_default(Key,Value,[Key,Value|_Args],_Default) :-
    !.
key_value_args_default(Key,Value,[_,_|Args],Default) :-
    key_value_args_default(Key,Value,Args,Default).

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
run([optimize|Databases]) :-
    !,
    super_user_authority(Auth),
    forall(member(Path, Databases),
           (   api_optimize(system_descriptor{}, Auth, Path),
               format(current_output, "~N~s optimized~n", [Path])
           )).
run([branch,create,Path|Args]) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),
    key_value_args_default('--origin', Origin_Base, Args, false),
    (   Origin_Base = false
    ->  Origin_Option = none
    ;   Origin_Option = some(Origin_Base)),
    branch_create(System_DB, Auth, Path, Origin_Option, _Branch_Uri).
run([db,create,DB_Path|Args]) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),
    (   re_matchsub('([^/]*)/([^/]*)', DB_Path, Match, [])
    ->  Organization = (Match.1),
        DB = (Match.2)
    ;   DB = DB_Path,
        key_value_args_default('--organization', Organization, Args, admin)
    ),
    key_value_args_default('--label', Label, Args, ''),
    key_value_args_default('--comment', Comment, Args, 'command line update'),
    key_value_args_default('--public', Public, Args, false),
    key_value_args_default('--schema', Schema, Args, true),
    key_value_args_default('--prefixes', Prefixes_Atom, Args, '{}'),
    atom_json_dict(Prefixes_Atom, Prefixes, []),
    create_db(System_DB, Auth, Organization, DB, Label, Comment, Public, Schema, Prefixes).
run([db,delete,DB_Path]) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),
    (   re_matchsub('([^/]*)/([^/]*)', DB_Path, Match, [])
    ->  Organization = (Match.1),
        DB = (Match.2)
    ;   DB = DB_Path,
        key_value_args_default('--organization', Organization, Args, admin)
    ),
    key_value_args_default('--force', Force_Delete, Args, false),
    delete_db(System_DB, Auth, Organization, DB, Force_Delete).

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
    format("serve~` t~40|run the terminusdb server~n",[]),
    format("list [Databases]~` t~40|list databases~n",[]),
    format("optimize [Databases]~` t~40|optimize the given databases~n",[]),
    format("query [Query]~` t~40|run query~n",[]),
    format("test~` t~40|run tests~n",[]).

