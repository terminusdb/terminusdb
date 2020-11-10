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
    initialise_log_settings,

    % Better error handling here...
    catch(
        (   set_prolog_flag(verbose, true),
            run(Argv),
            set_prolog_flag(verbose, false)
        ),
        Exception,
        format(current_output, "~NError: ~q~n~n", [Exception])).

key_value_args(Key,Value,[Key,Value|_Args]) :-
    !.
key_value_args(Key,Value,[_,_|Args]) :-
    key_value_args(Key,Value,Args).

% Key-value arguments
key_value_args_default(_,Default,[],Default).
key_value_args_default(_,Default,['--'|_],Default).
key_value_args_default(Key,Value,[Key,Value|_Args],_Default) :-
    !.
key_value_args_default(Key,Value,[_|Args],Default) :-
    key_value_args_default(Key,Value,Args,Default).

% For boolean switches
switch_args_boolean(_,[],false).
switch_args_boolean(Key,[Key|_Args],true) :-
    !.
switch_args_boolean(Key,[_|Args],Bool) :-
    switch_args_boolean(Key,Args,Bool).

non_switch_args(['--'|Rest], Rest).
non_switch_args([Switch,_Next|Rest], Result) :-
    re_match('^--',Switch),
    !,
    non_switch_args(Rest, Result).
non_switch_args(Result, Result).

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
    !,
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),
    key_value_args_default('--origin', Origin_Base, Args, false),
    (   Origin_Base = false
    ->  Origin_Option = none
    ;   Origin_Option = some(Origin_Base)),
    branch_create(System_DB, Auth, Path, Origin_Option, _Branch_Uri).
run([branch,delete,Path]) :-
    !,
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),
    branch_delete(System_DB, Auth, Path).
run([db,create,DB_Path|Args]) :-
    !,
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
    key_value_args_default('--data-prefix', Data_Prefix, Args, 'terminusdb:///data/'),
    key_value_args_default('--schema-prefix', Schema_Prefix, Args, 'terminusdb:///schema#'),
    key_value_args_default('--prefixes', Prefixes_Atom, Args, '{}'),
    atom_json_dict(Prefixes_Atom, Prefixes, []),
    put_dict(Prefixes, _{doc : Data_Prefix, scm : Schema_Prefix}, Merged),
    create_db(System_DB, Auth, Organization, DB, Label, Comment, Public, Schema, Merged).
run([db,delete,DB_Path]) :-
    !,
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
    initialize_database(Key),
    format('Successfully initialised database!!!~n').
%run([csv,list])
%run([csv,delete,Path,Name])
run([csv,load,Path|Args]) :-
    !,
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),
    key_value_args_default('--commit-message',Message, Args, "loaded from terminusdb CLI"),
    key_value_args_default('--commit-author',Author, Args, "admin"),
    non_switch_args(Args,Files),
    maplist([File,File_Name=File]>>file_base_name(File, File_Name),
            Files,
            File_Map),
    csv_load(System_DB, Auth, Path, _{ message : Message,
                                       author : Author},
             File_Map, _{}),
    format(current_output,'Successfully loaded CSVs~n',[]).
run([csv,update,Path|Args]) :-
    !,
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),
    key_value_args_default('--message',Msg, Args, "loaded from terminusdb CLI"),
    key_value_args_default('--author',Author, Args, "admin"),
    non_switch_args(Args,Files),
    maplist([File,File_Name=File]>>file_base_name(File, File_Name),
            Files,
            File_Map),
    csv_update(System_DB, Auth, Path, _{ message : Msg,
                                         author : Author},
               File_Map, _{}),
    format(current_output,'Successfully updated CSVs~n',[]).
run([csv,dump,Path,File|Args]) :-
    !,
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),
    csv_dump(System_DB, Auth, Path, File, Temp, []),
    key_value_args_default('--output', Output, Args, File),
    copy_file(Temp, Output),
    format(current_output,'Successfully dumped CSV to ~s~n',[Output]).
% turtle
% run([push|_Databases])
% run([pull|_Databases])
% run([clone|_Databases])
run([query,Database,Query]) :-
    !,
    resolve_absolute_string_descriptor(Database,Descriptor),
    create_context(Descriptor,commit_info{ author : "cli",
                                           message : "testing"}, Context),
    woql_context(Prefixes),
    context_extend_prefixes(Context,Prefixes,Context0),
    read_term_from_atom(Query, AST, []),
    query_response:run_context_ast_jsonld_response(Context0, AST, Response),
    Final_Prefixes = (Context0.prefixes),
    pretty_print_query_response(Response,Final_Prefixes,String),
    write(String).
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
    format("~` t~10|--data-prefix~` t~40|default data prefix~n", []),
    format("~` t~10|--schema-prefix~` t~40|default schema prefix~n", []),
    format("branch create [Branch]~` t~40|create a branch~n",[]),
    format("~` t~10|--origin [Ref]~` t~40|ref origin of the new branch~n",[]),
    format("branch delete [Branch]~` t~40|delete a branch~n",[]),
    format("store init~` t~40|initialize a database~n",[]),
    format('~` t~10|--key [key]~` t~40|admin login key~n',[]),
    format('~` t~10|--server [server]~` t~40|server address~n',[]),
    format('~` t~10|--port [port]~` t~40|server port~n',[]),
    format('~` t~10|--protocol [protocol]~` t~40|http or https~n',[]),
    format('~` t~10|--autologin~` t~40|whether to login immediately~n',[]),
    format('csv dump [Database] [CSV]~` t~40|dump a named csv~n', []),
    format('csv load [Database] [CSV]~` t~40|load or append a csv~n', []),
    format('~` t~10|--message [message]~` t~40|commit message~n',[]),
    format('~` t~10|--author [author]~` t~40|commit author~n',[]),
    format('csv update [Database] [CSV]~` t~40|update a csv~n', []),
    format('~` t~10|--message [message]~` t~40|commit message~n',[]),
    format('~` t~10|--author [author]~` t~40|commit author~n',[]),
    format("test~` t~40|run tests~n",[]).

