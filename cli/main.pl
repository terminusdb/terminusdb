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
:- use_module(library(optparse)).

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

not(true,false).
not(false,true).

% commands
opt_spec(test,'terminusdb test OPTIONS',
         [[opt(help),
           type(boolean),
           shortflags([h]),
           longflags([help]),
           default(false),
           help('print help for `test` command')],
          [opt(test),
           type(term),
           default([]),
           help('run tests')]]).
opt_spec(serve,'terminusdb serve OPTIONS',
         [[opt(help),
           type(boolean),
           shortflags([h]),
           longflags([help]),
           default(false),
           help('print help for `serve` command')],
          [opt(interactive),
           type(boolean),
           shortflags([i]),
           longflags([interactive]),
           default(false),
           help('run server in interactive mode')]]).
opt_spec(list,'terminusdb list OPTIONS',
         [[opt(help),
           type(boolean),
           shortflags([h]),
           longflags([help]),
           default(false),
           help('print help for the `list` command')]]).
opt_spec(optimize,'terminusdb optimize OPTIONS',
         [[opt(help),
           type(boolean),
           shortflags([h]),
           longflags([help]),
           default(false),
           help('print help for the `optimize` command')]]).
opt_spec(query,'terminusdb query QUERY OPTIONS',
         [[opt(help),
           type(boolean),
           shortflags([h]),
           longflags([help]),
           default(false),
           help('print help for the `query` command')],
          [opt(message),
           type(atom),
           longflags([message]),
           shortflags([m]),
           default('cli load'),
           help('message to associate with the commit')],
          [opt(author),
           type(atom),
           longflags([author]),
           shortflags([a]),
           default('admin'),
           help('author to place on the commit')]]).
% subcommands
opt_spec(branch,create,'terminusdb branch create BRANCH_SPEC OPTIONS',
         [[opt(help),
           type(boolean),
           longflags([help]),
           shortflags([h]),
           default(false),
           help('print help for the `branch create` sub command')],
          [opt(origin),
           type(term),
           shortflags([o]),
           longflags([origin]),
           default(false),
           help('the origin branch to use')]]).
opt_spec(branch,delete,'terminusdb branch delete BRANCH_SPEC OPTIONS',
         [[opt(help),
           type(boolean),
           longflags([help]),
           shortflags([h]),
           default(false),
           help('print help for the `branch delete` sub command')]]).
opt_spec(db,create,'terminusdb db create DATABASE_SPEC OPTIONS',
         [[opt(help),
           type(boolean),
           longflags([help]),
           shortflags([h]),
           default(false),
           help('print help for the `db create` sub command')],
          [opt(organization),
           type(term),
           longflags([organization]),
           shortflags([o]),
           default(admin),
           help('organizational owner of the database')],
          [opt(label),
           type(atom),
           longflags([label]),
           shortflags([l]),
           default(''),
           help('label to use for this database')],
          [opt(comment),
           type(atom),
           longflags([comment]),
           shortflags([c]),
           default(''),
           help('long description of this database')],
          [opt(public),
           type(boolean),
           longflags([public]),
           shortflags([p]),
           default(false),
           help('whether this database is to be public')],
          [opt(schema),
           type(boolean),
           longflags([schema]),
           default(true),
           help('whether to use a schema')],
          [opt(data_prefix),
           type(atom),
           longflags(['data-prefix']),
           shortflags([d]),
           default('terminusdb:///data/'),
           help('uri prefix to use for data')],
          [opt(schema_prefix),
           type(atom),
           longflags(['schema-prefix']),
           shortflags([s]),
           default('terminusdb:///schema#'),
           help('uri prefix to use for schema')],
          [opt(prefixes),
           type(atom),
           longflags(['prefixes']),
           shortflags([x]),
           default('{}'),
           help('additional defined prefixes in JSON')]]).
opt_spec(db,delete,'terminusdb db delete DATABASE_SPEC OPTIONS',
         [[opt(help),
           type(boolean),
           longflags([help]),
           shortflags([h]),
           default(false),
           help('print help for the `db delete` sub command')],
          [opt(organization),
           type(term),
           longflags([organization]),
           shortflags([o]),
           default(admin),
           help('organizational owner of the database')],
          [opt(force),
           type(boolean),
           longflags([force]),
           shortflags([f]),
           default(false),
           help('force the deletion of the database (unsafe)')]]).
opt_spec(store,init,'terminusdb store init OPTIONS',
         [[opt(help),
           type(boolean),
           longflags([help]),
           shortflags([h]),
           default(false),
           help('print help for the `store init` sub command')],
          [opt(key),
           type(atom),
           longflags([key]),
           shortflags([k]),
           default(root),
           help('key to use for admin login')]]).
opt_spec(csv,load,'terminusdb csv load DB_SPEC FILES OPTIONS',
         [[opt(help),
           type(boolean),
           longflags([help]),
           shortflags([h]),
           default(false),
           help('print help for the `csv load` sub command')],
          [opt(message),
           type(atom),
           longflags([message]),
           shortflags([m]),
           default('cli load'),
           help('message to associate with the commit')],
          [opt(author),
           type(atom),
           longflags([author]),
           shortflags([a]),
           default('admin'),
           help('author to place on the commit')]]).
opt_spec(csv,update,'terminusdb csv update DB_SPEC FILES OPTIONS',
         [[opt(help),
           type(boolean),
           longflags([help]),
           shortflags([h]),
           default(false),
           help('print help for the `csv update` sub command')],
          [opt(message),
           type(atom),
           longflags([message]),
           shortflags([m]),
           default('cli load'),
           help('message to associate with the commit')],
          [opt(author),
           type(atom),
           longflags([author]),
           shortflags([a]),
           default('admin'),
           help('author to place on the commit')]]).
opt_spec(csv,dump,'terminusdb csv dump DB_SPEC FILES OPTIONS',
         [[opt(help),
           type(boolean),
           longflags([help]),
           shortflags([h]),
           default(false),
           help('print help for the `csv dump` sub command')],
          [opt(output),
           type(atom),
           longflags([output]),
           shortflags([o]),
           default('_'),
           help('file name to use for csv output')]]).

command(Command) :-
    opt_spec(Command,_,_).
command(Command) :-
    opt_spec(Command,_,_,_).

command_subcommand(Command,Subcommand) :-
    opt_spec(Command,Subcommand,_,_).

run([Command|Rest]) :-
    opt_spec(Command,Command_String,Spec),
    opt_parse(Spec,Rest,Opts,Positional),
    (   member(help(true), Opts)
    ->  opt_help(Spec,Help),
        format(current_output,"~s~n",[Command_String]),
        format(current_output,Help,[])
    ;   run_command(Command,Positional,Opts)).
run([Command,Subcommand|Rest]) :-
    opt_spec(Command,Subcommand,Command_String,Spec),
    opt_parse(Spec,Rest,Opts,Positional),
    (   member(help(true), Opts)
    ->  opt_help(Spec,Help),
        format(current_output,"~s~n",[Command_String]),
        format(current_output,Help,[])
    ;   run_command(Command,Subcommand,Positional,Opts)).
run([Command|_Rest]) :-
    setof(Subcommand, command_subcommand(Command,Subcommand), Subcommands),
    format(current_output, "terminusdb ~s [subcommand]~n~twhere subcommand is one of: ~q~n", [Command, Subcommands]),
    format(current_output, "type: terminusdb ~s [subcommand] --help for more details~n", [Command]).
run(_) :-
    findall(Command, command(Command), Commands),
    format(current_output, "terminusdb [command]~n~twhere command is one of: ~q~n", [Commands]),
    format(current_output, "type: terminusdb [command] --help for more details~n", []).

% Commands
run_command(test,_Positional,Opts) :-
    (   member(test([]),Opts)
    ->  run_tests
    ;   member(test(Test), Opts),
        run_tests(Test)).
run_command(serve,_Positional,Opts) :-
    (   member(interactive(true),Opts)
    ->  terminus_server([serve|Opts], false),
        prolog
    ;   terminus_server([serve|Opts], true)).
run_command(list,Databases,_Opts) :-
    super_user_authority(Auth),
    list_databases(system_descriptor{}, Auth, Databases, Database_Objects),
    pretty_print_databases(Database_Objects).
run_command(optimize,Databases,_Opts) :-
    super_user_authority(Auth),
    forall(member(Path, Databases),
           (   api_optimize(system_descriptor{}, Auth, Path),
               format(current_output, "~N~s optimized~n", [Path])
           )).
run_command(query,[Database,Query],_Opts) :-
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
run_command(Command,_Args,_Opts) :-
    opt_spec(Command,Command_String,Spec),
    opt_help(Spec,Help),
    format(current_output,"~s~n",[Command_String]),
    format(current_output,Help,[]).

% Subcommands
run_command(branch,create,[Path],Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),

    member(origin(Origin_Base), Opts),
    (   Origin_Base = false
    ->  Origin_Option = none
    ;   Origin_Option = some(Origin_Base)),
    branch_create(System_DB, Auth, Path, Origin_Option, _Branch_Uri),
    format(current_output, "~N~s branch created~n", [Path]).
run_command(branch,delete,[Path],_Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),
    branch_delete(System_DB, Auth, Path),
    format(current_output, "~N~s branch deleted~n", [Path]).
run_command(db,create,[DB_Path],Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),

    (   re_matchsub('([^/]*)/([^/]*)', DB_Path, Match, [])
    ->  Organization = (Match.1),
        DB = (Match.2)
    ;   DB = DB_Path,
        member(organization(Organization),Opts)
    ),
    member(label(Label), Opts),
    member(comment(Comment), Opts),
    member(public(Public), Opts),
    member(schema(Schema), Opts),
    member(data_prefix(Data_Prefix), Opts),
    member(schema_prefix(Schema_Prefix), Opts),
    member(prefixes(Prefixes_Atom), Opts),
    atom_json_dict(Prefixes_Atom, Prefixes, []),
    put_dict(Prefixes, _{doc : Data_Prefix, scm : Schema_Prefix}, Merged),
    create_db(System_DB, Auth, Organization, DB, Label, Comment, Public, Schema, Merged).
run_command(db,delete,[DB_Path],Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),

    (   re_matchsub('([^/]*)/([^/]*)', DB_Path, Match, [])
    ->  Organization = (Match.1),
        DB = (Match.2)
    ;   DB = DB_Path,
        member(organization(Organization), Opts)
    ),
    member(force(Force_Delete), Opts),
    delete_db(System_DB, Auth, Organization, DB, Force_Delete).
run_command(store,init, _, Opts) :-
    (   member(key(Key), Opts)
    ->  true
    ;   format(current_output, "You must supply an administrator key to initialize the database!~n",[]),
        fail),
    initialize_database(Key),
    format('Successfully initialised database!!!~n').
%run_command(csv,list,_,_,_)
%run_command(csv,delete,Path,Name,_)
run_command(csv,load,[Path|Files],Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),
    member(message(Message), Opts),
    member(author(Author), Opts),
    maplist([File,File_Name=File]>>file_base_name(File, File_Name),
            Files,
            File_Map),
    csv_load(System_DB, Auth, Path, _{ message : Message,
                                       author : Author},
             File_Map, _{}),
    format(current_output,'Successfully loaded CSVs: ~s~n',[Files]).
run_command(csv,update,[Path|Files],Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),
    member(message(Message), Opts),
    member(author(Author), Opts),
    maplist([File,File_Name=File]>>file_base_name(File, File_Name),
            Files,
            File_Map),
    csv_update(System_DB, Auth, Path, _{ message : Message,
                                         author : Author},
               File_Map, _{}),
    format(current_output,'Successfully updated CSVs: ~s~n',[Files]).
run_command(csv,dump,[Path,File],Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),
    csv_dump(System_DB, Auth, Path, File, Temp, []),
    member(output(Output), Opts),
    ignore(Output = File),
    copy_file(Temp, Output),
    format(current_output,'Successfully dumped CSV to ~s~n',[Output]).
% turtle
% run_command(push,_Databases])
% run_command(pull,_Databases])
% run_command(clone,_Databases])
run_command(Command,Sub_Command,_Args,_Opts) :-
    opt_spec(Command,Sub_Command,Command_String,Spec),
    opt_help(Spec,Help),
    format(current_output,"~s~n",[Command_String]),
    format(current_output,Help,[]).

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
    'Run: `terminusdb store init` first'-[BasePath],
    nl
    ].
