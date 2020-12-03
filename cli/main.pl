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
:- use_module(core(util), [do_or_die/2]).

cli_toplevel :-
    current_prolog_flag(argv, Argv),
    initialise_log_settings,
    % Better error handling here...
    catch_with_backtrace(
        (   set_prolog_flag(verbose, true),
            run(Argv),
            set_prolog_flag(verbose, false),
            halt(0)
        ),
        Exception,
        (   Exception = error(Error,Ctx)
        ->  print_prolog_backtrace(user_error, Ctx),
            format(user_error, "~NError: ~q~n~n", [Error]),
            halt(1)
        ;   format(user_error, "~NError: ~q~n~n", [Exception]),
            halt(1)
        )).

not(true,false).
not(false,true).

% commands
opt_spec(help,'terminusdb help',
         'Display help regarding terminusdb.',
         [[opt(markdown),
           type(boolean),
           shortflags([m]),
           longflags([markdown]),
           default(false),
           help('generate help as markdown')]]).
opt_spec(test,'terminusdb test OPTIONS',
         'Run internal TerminusDB tests.',
         [[opt(help),
           type(boolean),
           shortflags([h]),
           longflags([help]),
           default(false),
           help('print help for `test` command')],
          [opt(test),
           type(term),
           shortflags([t]),
           longflags([test]),
           default([]),
           help('Run a specific test')]]).
opt_spec(serve,'terminusdb serve OPTIONS',
         'Run the TerminusDB server.',
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
         'List databases.',
         [[opt(help),
           type(boolean),
           shortflags([h]),
           longflags([help]),
           default(false),
           help('print help for the `list` command')]]).
opt_spec(optimize,'terminusdb optimize OPTIONS',
         'Optimize a database (including _system and _meta).',
         [[opt(help),
           type(boolean),
           shortflags([h]),
           longflags([help]),
           default(false),
           help('print help for the `optimize` command')]]).
opt_spec(query,'terminusdb query QUERY OPTIONS',
         'Query a database.',
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
         'Create a branch.',
         [[opt(help),
           type(boolean),
           longflags([help]),
           shortflags([h]),
           default(false),
           help('print help for the `branch create` sub command')],
          [opt(origin),
           type(atom),
           shortflags([o]),
           longflags([origin]),
           default(false),
           help('the origin branch to use')]]).
opt_spec(branch,delete,'terminusdb branch delete BRANCH_SPEC OPTIONS',
         'Delete a branch.',
         [[opt(help),
           type(boolean),
           longflags([help]),
           shortflags([h]),
           default(false),
           help('print help for the `branch delete` sub command')]]).
opt_spec(db,create,'terminusdb db create DATABASE_SPEC OPTIONS',
         'Create a database.',
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
           shortflags([k]),
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
         'Delete a database.',
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
         'Initialize a store for TerminusDB.',
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
           help('key to use for admin login')],
          [opt(force),
           type(boolean),
           longflags([force]),
           shortflags([f]),
           default(false),
           help('force the creation of a new store even when one already exists')]]).
opt_spec(csv,list,'terminusdb csv list DB_SPEC',
         'List CSVs in the given DB.',
         [[opt(help),
           type(boolean),
           longflags([help]),
           shortflags([h]),
           default(false),
           help('print help for the `csv load` sub command')]]).
opt_spec(csv,delete,'terminusdb csv delete DB_SPEC FILE OPTIONS',
         'Delete a CSV file from the given database.',
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
opt_spec(csv,load,'terminusdb csv load DB_SPEC FILES OPTIONS',
         'Load a CSV file (appends new lines if already existing).',
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
         'Update a CSV file (equivalent to delete / load but with a minimal delta).',
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
         'Dump a CSV file from the database.',
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
    opt_spec(Command,_,_,_).
command(Command) :-
    opt_spec(Command,_,_,_,_).

command_subcommand(Command,Subcommand) :-
    opt_spec(Command,Subcommand,_,_,_).

run([Command|Rest]) :-
    opt_spec(Command,_,_,Spec),
    opt_parse(Spec,Rest,Opts,Positional),
    (   member(help(true), Opts)
    ->  terminusdb_help(Command,Opts)
    ;   run_command(Command,Positional,Opts)).
run([Command,Subcommand|Rest]) :-
    opt_spec(Command,Subcommand,_,_,Spec),
    opt_parse(Spec,Rest,Opts,Positional),
    (   member(help(true), Opts)
    ->  terminusdb_help(Command,Subcommand,Opts)
    ;   run_command(Command,Subcommand,Positional,Opts)).
run([Command|_Rest]) :-
    setof(Subcommand, command_subcommand(Command,Subcommand), Subcommands),
    format(current_output, "terminusdb ~s [subcommand]~n~twhere subcommand is one of: ~q~n", [Command, Subcommands]),
    format(current_output, "type: terminusdb ~s [subcommand] --help for more details~n", [Command]).
run(_) :-
    setof(Command, command(Command), Commands),
    format(current_output, "terminusdb [command]~n~twhere command is one of: ~q~n", [Commands]),
    format(current_output, "type: terminusdb [command] --help for more details~n", []).

% Commands
run_command(help,_Positional,Opts) :-
    terminusdb_help(Opts).
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
    api_report_errors(
        optimize,
        forall(member(Path, Databases),
               (   api_optimize(system_descriptor{}, Auth, Path),
                   format(current_output, "~N~s optimized~n", [Path])
               ))).
run_command(query,[Database,Query],Opts) :-
    resolve_absolute_string_descriptor(Database,Descriptor),
    member(author(Author), Opts),
    member(author(Message), Opts),
    create_context(Descriptor,commit_info{ author : Author,
                                           message : Message}, Context),
    api_report_errors(
        woql,
        (   woql_context(Prefixes),
            context_extend_prefixes(Context,Prefixes,Context0),
            read_term_from_atom(Query, AST, []),
            query_response:run_context_ast_jsonld_response(Context0, AST, Response),
            get_dict(prefixes,Context0, Final_Prefixes),
            pretty_print_query_response(Response,Final_Prefixes,String),
            write(String)
        )).
run_command(Command,_Args, Opts) :-
    terminusdb_help(Command,Opts).

% Subcommands
run_command(branch,create,[Path],Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),

    member(origin(Origin_Base), Opts),
    (   Origin_Base = false
    ->  Origin_Option = none
    ;   Origin_Option = some(Origin_Base)),
    api_report_errors(
        branch,
        branch_create(System_DB, Auth, Path, Origin_Option, _Branch_Uri)),
    format(current_output, "~N~s branch created~n", [Path]).
run_command(branch,delete,[Path],_Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),
    api_report_errors(
        branch,
        branch_delete(System_DB, Auth, Path)),
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
    api_report_errors(
        create_db,
        create_db(System_DB, Auth, Organization, DB, Label, Comment, Public, Schema, Merged)),
    format(current_output,"Database ~s/~s created~n",[Organization,DB]).
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
    api_report_errors(
        delete_db,
        delete_db(System_DB, Auth, Organization, DB, Force_Delete)),
    format(current_output,"Database ~s/~s deleted~n",[Organization,DB]).
run_command(store,init, _, Opts) :-
    (   member(key(Key), Opts)
    ->  true
    ;   format(current_output, "You must supply an administrator key to initialize the database!~n",[]),
        fail),
    member(force(Force), Opts),
    api_report_errors(
        store_init,
        initialize_database(Key,Force)),
    format('Successfully initialised database!!!~n').
run_command(csv,delete,[Path,Name],Opts) :-
    super_user_authority(Auth),
    member(message(Message), Opts),
    member(author(Author), Opts),
    create_context(system_descriptor{}, System_DB),
    api_report_errors(
        csv,
        csv_delete(System_DB, Auth, Path, _{ message : Message,
                                             author : Author},
                   Name, _{})),
    format(current_output,'Successfully delete CSV: ~q~n',[Name]).
run_command(csv,list,[Path],_Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),
    api_report_errors(
        csv,
        csv_list(System_DB, Auth, Path, Names,_{})),
    forall(
        member(Name, Names),
        format(current_output,'~w~n',[Name])).
run_command(csv,load,[Path|Files],Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),
    member(message(Message), Opts),
    member(author(Author), Opts),
    maplist([File,File_Name=File]>>file_base_name(File, File_Name),
            Files,
            File_Map),
    api_report_errors(
        csv,
        csv_load(System_DB, Auth, Path, _{ message : Message,
                                           author : Author},
                 File_Map, _{})),
    format(current_output,'Successfully loaded CSVs: ~q~n',[Files]).
run_command(csv,update,[Path|Files],Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),
    member(message(Message), Opts),
    member(author(Author), Opts),
    maplist([File,File_Name=File]>>file_base_name(File, File_Name),
            Files,
            File_Map),
    api_report_errors(
        csv,
        csv_update(System_DB, Auth, Path, _{ message : Message,
                                             author : Author},
                   File_Map, _{})),
    format(current_output,'Successfully updated CSVs: ~q~n',[Files]).
run_command(csv,dump,[Path,File],Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),
    api_report_errors(
        csv,
        csv_dump(System_DB, Auth, Path, File, Temp, [])),
    member(output(Output), Opts),
    ignore(Output = File),
    copy_file(Temp, Output),
    format(current_output,'Successfully dumped CSV to ~s~n',[Output]).
% turtle
% run_command(push,_Databases])
% run_command(pull,_Databases])
% run_command(clone,_Databases])
run_command(Command,Subcommand,_Args,_Opts) :-
    format_help(Command,Subcommand).

:- meta_predicate api_report_errors(?,0).
api_report_errors(API,Goal) :-
    catch_with_backtrace(
        Goal,
        Error,
        do_or_die(api_error_cli(API,Error),
                  Error)
    ).

api_error_cli(API, Error) :-
    api_error_jsonld(API,Error,JSON),
    json_cli_code(JSON,Status),
    Msg = (JSON.'api:message'),
    format(user_error,"Error: ~s~n",[Msg]),
    halt(Status).

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

%% Generate help
terminusdb_help(Opts) :-
    forall(
        opt_spec(Command,_,_,_),
        terminusdb_help(Command,Opts)
    ),
    forall(
        command_subcommand(Command,Subcommand),
        terminusdb_help(Command,Subcommand,Opts)
    ).

terminusdb_help(Command,Opts) :-
    (   member(markdown(true),Opts)
    ->  format_help_markdown(Command)
    ;   format_help(Command)
    ).

terminusdb_help(Command,Subcommand,Opts) :-
    (   member(markdown(true),Opts)
    ->  format_help_markdown(Command,Subcommand)
    ;   format_help(Command,Subcommand)
    ).

format_help(Command) :-
    opt_spec(Command,Command_String,Help_String,Spec),
    opt_help(Spec,Help),
    format(current_output,"~n~s~n~n",[Command_String]),
    format(current_output,"~s~n~n",[Help_String]),
    format(current_output,Help,[]).

format_help(Command,Subcommand) :-
    opt_spec(Command,Subcommand,Command_String,Help_String,Spec),
    opt_help(Spec,Help),
    format(current_output,"~n~s~n~n",[Command_String]),
    format(current_output,"~s~n~n",[Help_String]),
    format(current_output,Help,[]).

format_help_markdown(Command) :-
    format(current_output,'### ~s~n~n',[Command]),
    opt_spec(Command,Command_String,Help_String,OptSpec),
    embellish_flags(OptSpec,OptSpec0),
    format(current_output,'`~s`~n~n',[Command_String]),
    format(current_output,'~s~n~n',[Help_String]),
    forall(
        member(Option,OptSpec0),
        format_help_markdown_opt(Option)
    ).

format_help_markdown(Command,Subcommand) :-
    format(current_output,'### ~s ~s~n~n',[Command,Subcommand]),
    opt_spec(Command,Subcommand,Command_String,Help_String,OptSpec),
    embellish_flags(OptSpec,OptSpec0),
    format(current_output,'`~s`~n~n',[Command_String]),
    format(current_output,'~s~n~n',[Help_String]),
    forall(
        member(Option,OptSpec0),
        format_help_markdown_opt(Option)
    ).

embellish_flags(OptsSpec0,OptsSpec2) :-
    maplist(optparse:embellish_flag(short), OptsSpec0, OptsSpec1),
    maplist(optparse:embellish_flag(long), OptsSpec1, OptsSpec2).

format_help_markdown_opt(Opt) :-

    memberchk(shortflags(SFlags0),Opt),
    memberchk(longflags(LFlags0),Opt),

    atomic_list_concat(['`',SFlags0,'`'],SFlags),


    maplist([LFlag_In,LFlag_Out]>>
            atomic_list_concat(['`',LFlag_In,'`'],LFlag_Out),
            LFlags0,LFlags1
           ),
    utils:intersperse(', ',LFlags1,LFlags2),
    atomic_list_concat(LFlags2, LFlags),

    memberchk(help(Help), Opt),

    format(current_output, '  * ~s, ~s=[value]:~n', [SFlags,LFlags]),
    format(current_output, '  ~s~n~n', [Help]).

