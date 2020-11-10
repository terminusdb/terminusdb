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
    % Better error handling here...
    catch(
        run(Argv),
        Exception,
        format(current_output, "~NError: ~q~n~n", [Exception])).

key_value_args(Key,Value,[Key,Value|_Args]) :-
    !.
key_value_args(Key,Value,[_,_|Args]) :-
    key_value_args(Key,Value,Args).

% Key-value arguments
key_value_args_default(_,Default,[],Default).
key_value_args_default(Key,Value,[Key,Value|_Args],_Default) :-
    !.
key_value_args_default(Key,Value,[_,_|Args],Default) :-
    key_value_args_default(Key,Value,Args,Default).

% For boolean switches
switch_args_boolean(_,[],false).
switch_args_boolean(Key,[Key|_Args],true) :-
    !.
switch_args_boolean(Key,[_|Args],Default) :-
    switch_args_boolean(Key,Args,Default).

not(true,false).
not(false,true).

run([test]) :-
    !,
    run_tests,
    halt.
run([serve|Args]) :-
    !,
    (   member('--interactive', Args)
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
run([branch,delete,Path]) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),
    branch_delete(System_DB, Auth, Path).
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
    switch_args_boolean('--public', Args, Public),
    switch_args_boolean('--no-schema', Args, No_Schema),
    not(No_Schema,Schema),
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
run([store,init|Args]) :-
    !,
    (   key_value_args('--key',Key,Args)
    ->  true
    ;   format(current_output, "You must supply an administrator key to initialize the database!~n",[]),
        fail),
    key_value_args_default('--server',Server, Args, '127.0.0.1'),
    key_value_args_default('--port',Port_Atom, Args, '6363'),
    atom_number(Port_Atom, Port),
    key_value_args_default('--protocol',Protocol, Args, 'https'),

    format(atom(SERVER_URL), '~s://~s:~d', [Protocol, Server, Port]),

    initialize_database(SERVER_URL, Key),
    format('Successfully initialised database!!!~n').
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
    format("~` t~10|--interactive~` t~40|run server with interactive REPL~n",[]),
    format("list [Databases]~` t~40|list databases~n",[]),
    format("optimize [Databases]~` t~40|optimize the given databases~n",[]),
    %format("query [Query]~` t~40|run query~n",[]),
    format("db delete [Database]~` t~40|delete a database~n",[]),
    format("db create [Database]~` t~40|create a database~n",[]),
    format("~` t~10|--label [Label]~` t~40|database label~n",[]),
    format("~` t~10|--comment [Comment]~` t~40|database comment~n",[]),
    format("~` t~10|--public~` t~40|Ensure database is public~n",[]),
    format("~` t~10|--no-schema~` t~40|Exclude schema from database~n",[]),
    format("~` t~10|--prefixes [Prefixes]~` t~40|a JSON of prefixes~n",[]),
    format("branch create [Branch]~` t~40|create a branch~n",[]),
    format("~` t~10|--origin [Ref]~` t~40|ref origin of the new branch~n",[]),
    format("branch delete [Branch]~` t~40|delete a branch~n",[]),
    format("store init~` t~40|initialize a database~n",[]),
    format('~` t~10|--key [key]~` t~40|admin login key~n',[]),
    format('~` t~10|--server [server]~` t~40|server address~n',[]),
    format('~` t~10|--port [port]~` t~40|server port~n',[]),
    format('~` t~10|--protocol [protocol]~` t~40|http or https~n',[]),
    format('~` t~10|--autologin~` t~40|whether to login immediately~n',[]),
    (   prolog_flag(optimise, false)
    ->  format("test~` t~40|run tests~n",[])
    ;   true).

