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
:- use_module(server(main), [terminus_server/2]).
:- use_module(library(http/json)).
:- use_module(core(query)).
:- use_module(core(transaction), [open_descriptor/2]).
:- use_module(library(optparse)).
:- use_module(core(util),
              [do_or_die/2, token_authorization/2,
               basic_authorization/3, intersperse/3,
               with_memory_file/1, with_memory_file_stream/3]).
:- use_module(library(prolog_stack), [print_prolog_backtrace/2]).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(readutil)).
:- use_module(library(yall)).
:- use_module(library(pcre)).
:- use_module(library(url)).
:- use_module(library(option)).
:- use_module(library(plunit), [run_tests/0, run_tests/1]).
:- use_module(library(settings)).

:- use_module(config(terminus_config), [terminusdb_version/1, check_all_env_vars/0]).

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
        (   Exception = error(io_error(write,user_output),_)
        ->  halt(0)
        ;   Exception = error(Error,context(prolog_stack(Stack),_)),
            print_prolog_backtrace(user_error, Stack)
        ->  format(user_error, "~NError: ~q~n~n", [Error]),
            halt(1)
        ;   format(user_error, "~NError: ~q~n~n", [Exception]),
            halt(1)
        )).

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
           help('Print help for `serve` command')],
          [opt(interactive),
           type(boolean),
           shortflags([i]),
           longflags([interactive]),
           default(false),
           help('Run server in interactive mode')],
          [opt(memory),
           type(atom),
           shortflags([m]),
           longflags([memory]),
           meta(password),
           default('_'),
           help('Run server in-memory, without a persistent store. Takes a password as an optional argument. The in-memory store will be initialized with an admin account with the given password. If absent, the admin account will have \'root\' as a password.')]]).
opt_spec(list,'terminusdb list OPTIONS',
         'List databases.',
         [[opt(help),
           type(boolean),
           shortflags([h]),
           longflags([help]),
           default(false),
           help('print help for the `list` command')],
          [opt(json),
           type(boolean),
           shortflags([j]),
           longflags([json]),
           default(false),
           help('Return a JSON as the result of the `list` command')]]).
opt_spec(optimize,'terminusdb optimize OPTIONS',
         'Optimize a database (including _system and _meta).',
         [[opt(help),
           type(boolean),
           shortflags([h]),
           longflags([help]),
           default(false),
           help('print help for the `optimize` command')]]).
opt_spec(query,'terminusdb query DB_SPEC QUERY OPTIONS',
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
opt_spec(push,'terminusdb push DB_SPEC',
         'Push a branch.',
         [[opt(help),
           type(boolean),
           shortflags([h]),
           longflags([help]),
           default(false),
           help('print help for the `push` command')],
          [opt(branch),
           type(atom),
           shortflags([b]),
           longflags([branch]),
           default(main),
           help('set the origin branch for push')],
          [opt(remote_branch),
           type(atom),
           shortflags([e]),
           longflags(['remote-branch']),
           default('_'),
           help('set the branch on the remote for push')],
          [opt(remote),
           type(atom),
           shortflags([r]),
           longflags([remote]),
           default(origin),
           help('the name of the remote to use')],
          [opt(prefixes),
           type(boolean),
           shortflags([x]),
           longflags([prefixes]),
           default(false),
           help('send prefixes for database')],
          [opt(token),
           type(atom),
           shortflags([t]),
           longflags([token]),
           default('_'),
           help('machine access token')],
          [opt(user),
           type(atom),
           shortflags([u]),
           longflags([user]),
           default('_'),
           help('the user on the remote')],
          [opt(password),
           type(atom),
           shortflags([p]),
           longflags([password]),
           default('_'),
           help('the password on the remote')]]).
opt_spec(clone,'terminusdb clone URI <DB_SPEC>',
         'Clone a database (into DB_SPEC).',
         [[opt(help),
           type(boolean),
           shortflags([h]),
           longflags([help]),
           default(false),
           help('print help for the `clone` command')],
          [opt(token),
           type(atom),
           shortflags([t]),
           longflags([token]),
           default('_'),
           help('machine access token')],
          [opt(user),
           type(atom),
           shortflags([u]),
           longflags([user]),
           default('_'),
           help('the user on the remote')],
          [opt(password),
           type(atom),
           shortflags([p]),
           longflags([password]),
           default('_'),
           help('the password on the remote')],
          [opt(organization),
           type(term),
           longflags([organization]),
           shortflags([o]),
           default(admin),
           help('organizational owner of the cloned database')],
          [opt(label),
           type(atom),
           longflags([label]),
           shortflags([l]),
           default('_'),
           help('label to use for this database')],
          [opt(comment),
           type(atom),
           longflags([comment]),
           shortflags([c]),
           default(''),
           help('long description of the cloned database')],
          [opt(public),
           type(boolean),
           longflags([public]),
           shortflags([b]),
           default(false),
           help('whether the cloned database is to be public')]]).
opt_spec(pull,'terminusdb pull BRANCH_SPEC',
         'Pull a branch from a database.',
         [[opt(help),
           type(boolean),
           shortflags([h]),
           longflags([help]),
           default(false),
           help('print help for the `pull` command')],
          [opt(remote_branch),
           type(atom),
           shortflags([e]),
           longflags(['remote-branch']),
           default('_'),
           help('set the branch on the remote for push')],
          [opt(remote),
           type(atom),
           shortflags([r]),
           longflags([remote]),
           default(origin),
           help('the name of the remote to use')],
          [opt(token),
           type(atom),
           shortflags([t]),
           longflags([token]),
           default('_'),
           help('machine access token')],
          [opt(user),
           type(atom),
           shortflags([u]),
           longflags([user]),
           default('_'),
           help('the user on the remote')],
          [opt(password),
           type(atom),
           shortflags([p]),
           longflags([password]),
           default('_'),
           help('the password on the remote')]]).
opt_spec(fetch,'terminusdb fetch BRANCH_SPEC',
         'fetch data from a remote.',
         [[opt(help),
           type(boolean),
           shortflags([h]),
           longflags([help]),
           default(false),
           help('print help for the `fetch` command')],
          [opt(remote),
           type(atom),
           shortflags([r]),
           longflags([remote]),
           default(origin),
           help('the name of the remote to use')],
          [opt(token),
           type(atom),
           shortflags([t]),
           longflags([token]),
           default('_'),
           help('machine access token')],
          [opt(user),
           type(atom),
           shortflags([u]),
           longflags([user]),
           default('_'),
           help('the user on the remote')],
          [opt(password),
           type(atom),
           shortflags([p]),
           longflags([password]),
           default('_'),
           help('the password on the remote')]]).
opt_spec(rebase,'terminusdb rebase TO_DATABASE_SPEC FROM_DATABASE_SPEC OPTIONS',
         'Rebase a database with commits from FROM_DATABASE_SPEC into TO_DATABASE_SPEC.',
         [[opt(help),
           type(boolean),
           longflags([help]),
           shortflags([h]),
           default(false),
           help('print help for the `rebase` command')],
          [opt(author),
           type(atom),
           longflags([author]),
           shortflags([a]),
           default(admin),
           help('The author of the rebase')]
         ]).
opt_spec(rollup,'terminusdb rollup DATABASE_SPEC OPTIONS',
         'Creates an optimisation layer for queries on the given commit.',
         [[opt(help),
           type(boolean),
           longflags([help]),
           shortflags([h]),
           default(false),
           help('print help for the `rollup` command')]
         ]).
opt_spec(bundle,'terminusdb bundle DATABASE_SPEC OPTIONS',
         'Create a pack for a given DATABASE_SPEC that can then be reconsistuted with `terminusdb unpack`.',
         [[opt(help),
           type(boolean),
           longflags([help]),
           shortflags([h]),
           default(false),
           help('print help for the `bundle` command')],
          [opt(output),
           type(atom),
           longflags([output]),
           shortflags([o]),
           default('_'),
           help('file name to use for pack output file (defaults to descriptor based name).')]
         ]).
opt_spec(unbundle,'terminusdb unbundle DATABASE_SPEC FILE OPTIONS',
         'Unbundle a bundle file.',
         [[opt(help),
           type(boolean),
           longflags([help]),
           shortflags([h]),
           default(false),
           help('print help for the `unbundle` command')]
         ]).
opt_spec(diff,'terminusdb diff [Path] OPTIONS',
         'Create a diff between two JSONs, a JSON and a commit (path required),
or two commits (path required).',
         [[opt(help),
           type(boolean),
           longflags([help]),
           shortflags([h]),
           default(false),
           help('print help for the `diff` command')],
          [opt(before),
           type(atom),
           longflags([before]),
           shortflags([b]),
           default('_'),
           help('JSON document which is the *before*')],
          [opt(after),
           type(atom),
           longflags([after]),
           shortflags([a]),
           default('_'),
           help('JSON document which is the *after*')],
          [opt(keep),
           type(atom),
           longflags([keep]),
           shortflags([k]),
           default('{"@id" : true, "_id" : true}'),
           help('Skeleton of the document to retain as context')],
          [opt(docid),
           type(atom),
           longflags([docid]),
           shortflags([d]),
           default('_'),
           help('document id to use for comparisons')],
          [opt(before_commit),
           type(atom),
           longflags([before_commit,'before-commit']),
           shortflags([p]),
           default('_'),
           help('Commit of the *before* document(s)')],
          [opt(after_commit),
           type(atom),
           longflags([after_commit,'after-commit']),
           shortflags([s]),
           default('_'),
           help('Commit of the *after* document(s)')]
         ]).
opt_spec(log,'terminusdb log DB_SPEC',
         'Get the log for a branch given by DB_SPEC.',
         [[opt(help),
           type(boolean),
           longflags([help]),
           shortflags([h]),
           default(false),
           help('print help for the `log` command')]]).

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
opt_spec(doc,insert,'terminusdb doc insert DATABASE_SPEC OPTIONS',
         'Insert documents.',
         [[opt(help),
           type(boolean),
           longflags([help]),
           shortflags([h]),
           default(false),
           help('print help for the `doc insert` sub command')],
          [opt(message),
           type(atom),
           longflags([message]),
           shortflags([m]),
           default('cli: document insert'),
           help('message to associate with the commit')],
          [opt(author),
           type(atom),
           longflags([author]),
           shortflags([a]),
           default(admin),
           help('author to place on the commit')],
          [opt(graph_type),
           type(atom),
           longflags([graph_type,'graph-type']),
           shortflags([g]),
           default(instance),
           help('graph type (instance or schema)')],
          [opt(data),
           type(atom),
           longflags([data]),
           shortflags([d]),
           default('_'),
           help('document data')],
          [opt(full_replace),
           type(boolean),
           longflags([full_replace,'full-replace']),
           shortflags([f]),
           default(false),
           help('delete all previous documents and substitute these')]]).
opt_spec(doc,delete,'terminusdb doc delete DATABASE_SPEC OPTIONS',
         'Delete documents.',
         [[opt(help),
           type(boolean),
           longflags([help]),
           shortflags([h]),
           default(false),
           help('print help for the `doc delete` sub command')],
          [opt(message),
           type(atom),
           longflags([message]),
           shortflags([m]),
           default('cli: document delete'),
           help('message to associate with the commit')],
          [opt(author),
           type(atom),
           longflags([author]),
           shortflags([a]),
           default(admin),
           help('author to place on the commit')],
          [opt(graph_type),
           type(atom),
           longflags([graph_type,'graph-type']),
           shortflags([g]),
           default(instance),
           help('graph type (instance or schema)')],
          [opt(id),
           type(atom),
           longflags([id]),
           shortflags([i]),
           default('_'),
           help('document id to delete')],
          [opt(data),
           type(atom),
           longflags([data]),
           shortflags([d]),
           default('_'),
           help('document data')],
          [opt(nuke),
           type(boolean),
           longflags([nuke]),
           shortflags([n]),
           default(false),
           help('nuke all documents')]]).
opt_spec(doc,replace,'terminusdb doc replace DATABASE_SPEC OPTIONS',
         'Replace documents.',
         [[opt(help),
           type(boolean),
           longflags([help]),
           shortflags([h]),
           default(false),
           help('print help for the `doc replace` sub command')],
          [opt(message),
           type(atom),
           longflags([message]),
           shortflags([m]),
           default('cli: document replace'),
           help('message to associate with the commit')],
          [opt(author),
           type(atom),
           longflags([author]),
           shortflags([a]),
           default(admin),
           help('author to place on the commit')],
          [opt(graph_type),
           type(atom),
           longflags([graph_type,'graph-type']),
           shortflags([g]),
           default(instance),
           help('graph type (instance or schema)')],
          [opt(data),
           type(atom),
           longflags([data]),
           shortflags([d]),
           default('_'),
           help('document data')],
          [opt(create),
           type(atom),
           longflags([create]),
           shortflags([c]),
           default(false),
           help('create document if it does not exist')]]).
opt_spec(doc,get,'terminusdb doc get DATABASE_SPEC OPTIONS',
         'Query documents.',
         [[opt(help),
           type(boolean),
           longflags([help]),
           shortflags([h]),
           default(false),
           help('print help for the `doc get` sub command')],
          [opt(graph_type),
           type(atom),
           longflags([graph_type]),
           shortflags([g]),
           default(instance),
           help('graph type (instance or schema)')],
          [opt(skip),
           type(atom),
           longflags([skip]),
           shortflags([s]),
           default('0'),
           help('number of documents to skip')],
          [opt(count),
           type(atom),
           longflags([count]),
           shortflags([c]),
           default(unlimited),
           help('number of documents to return')],
          [opt(minimized),
           type(boolean),
           longflags([minimized]),
           shortflags([m]),
           default(true),
           help('return minimized prefixes')],
          [opt(as_list),
           type(boolean),
           longflags([as_list, 'as-list']),
           shortflags([l]),
           default(false),
           help('return results as a JSON list (as opposed to JSON-lines)')],
          [opt(unfold),
           type(boolean),
           longflags([unfold]),
           shortflags([u]),
           default(true),
           help('include subdocuments, or only subdocument ids')],
          [opt(id),
           type(atom),
           longflags([id]),
           shortflags([i]),
           default('_'),
           help('id of document to retrieve')],
          [opt(type),
           type(atom),
           longflags([type]),
           shortflags([t]),
           default('_'),
           help('type of document to retrieve')],
          [opt(compress_ids),
           type(boolean),
           longflags([compress_ids, 'compress-ids']),
           shortflags([z]),
           default(true),
           help('return compressed / minimized ids using default prefixes')],
          [opt(query),
           type(atom),
           longflags([query]),
           shortflags([q]),
           default('_'),
           help('document query search template')]]).
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
opt_spec(triples,dump,'terminusdb triples dump GRAPH_SPEC',
         'Dump an RDF string.',
         [[opt(help),
           type(boolean),
           longflags([help]),
           shortflags([h]),
           default(false),
           help('print help for the `triples dump` sub command')],
          [opt(format),
           type(atom),
           longflags([format]),
           shortflags([f]),
           default(turtle),
           help('format of RDF (can be one of: [turtle])')]]).
opt_spec(triples,update,'terminusdb triples update GRAPH_SPEC FILE',
         'Update from an RDF file (replaces current content).',
         [[opt(help),
           type(boolean),
           longflags([help]),
           shortflags([h]),
           default(false),
           help('print help for the `triples update` sub command')],
          [opt(message),
           type(atom),
           longflags([message]),
           shortflags([m]),
           default('cli: triples update'),
           help('message to associate with the commit')],
          [opt(author),
           type(atom),
           longflags([author]),
           shortflags([a]),
           default(admin),
           help('author to place on the commit')],
          [opt(format),
           type(atom),
           longflags([format]),
           shortflags([f]),
           default(turtle),
           help('format of RDF (can be one of: [turtle])')]]).
opt_spec(triples,load,'terminusdb triples load GRAPH_SPEC FILE',
         'Load triples from RDF file (Appending new).',
         [[opt(help),
           type(boolean),
           longflags([help]),
           shortflags([h]),
           default(false),
           help('print help for the `triples load` sub command')],
          [opt(message),
           type(atom),
           longflags([message]),
           shortflags([m]),
           default('cli: triples load'),
           help('message to associate with the commit')],
          [opt(author),
           type(atom),
           longflags([author]),
           shortflags([a]),
           default(admin),
           help('author to place on the commit')],
          [opt(format),
           type(atom),
           longflags([format]),
           shortflags([f]),
           default(turtle),
           help('format of RDF (can be one of: [turtle])')]]).
opt_spec(remote,add,'terminusdb remote add DATABASE_SPEC REMOTE_NAME REMOTE_LOCATION OPTIONS',
         'Add a remote.',
         [[opt(help),
           type(boolean),
           longflags([help]),
           shortflags([h]),
           default(false),
           help('print help for the `remote add` sub command')]]).
opt_spec(remote,remove,'terminusdb remote delete DATABASE_SPEC REMOTE_NAME OPTIONS',
         'Remove a remote.',
         [[opt(help),
           type(boolean),
           longflags([help]),
           shortflags([h]),
           default(false),
           help('print help for the `remote remove` sub command')]]).
opt_spec(remote,'set-url','terminusdb remote set-url DATABASE_SPEC REMOTE_NAME REMOTE_LOCATION OPTIONS',
         'Set the URL of a remote.',
         [[opt(help),
           type(boolean),
           longflags([help]),
           shortflags([h]),
           default(false),
           help('print help for the `remote set-url` sub command')]]).
opt_spec(remote,'get-url','terminusdb remote get-url DATABASE_SPEC REMOTE_NAME OPTIONS',
         'Get the URL of a remote.',
         [[opt(help),
           type(boolean),
           longflags([help]),
           shortflags([h]),
           default(false),
           help('print help for the `remote get-url` sub command')],
          [opt(remote),
           type(string),
           shortflags([r]),
           longflags([remote]),
           default("origin"),
           help('the name of the remote to use')]]).
opt_spec(remote,list,'terminusdb remote list DATABASE_SPEC OPTIONS',
         'List remotes.',
         [[opt(help),
           type(boolean),
           longflags([help]),
           shortflags([h]),
           default(false),
           help('print help for the `remote list` sub command')]]).


command(Command) :-
    opt_spec(Command,_,_,_).
command(Command) :-
    opt_spec(Command,_,_,_,_).

command_subcommand(Command,Subcommand) :-
    opt_spec(Command,Subcommand,_,_,_).

run(Argv) :-
    % Check env vars to report errors as soon as possible.
    check_all_env_vars,
    (   (   Argv = [Cmd|_],
            member(Cmd, ['--version', help, store, test])
        ;   open_descriptor(system_descriptor{}, _))
    ->  run_(Argv)
    ;   format(user_error,"Unable to find system database.~nTry one of:~n 1. Initialising the database with the command 'terminusdb store init'~n 2. Setting the variable TERMINUSDB_SERVER_DB_PATH to the correct location of the store~n 3. Launching the executable from a directory which already has a store.~n", []),
        halt(1)).

run_([Command|Rest]) :-
    opt_spec(Command,_,_,Spec),
    opt_parse(Spec,Rest,Opts,Positional),
    (   option(help(true), Opts)
    ->  terminusdb_help(Command,Opts)
    ;   run_command(Command,Positional,Opts)).
run_([Command,Subcommand|Rest]) :-
    opt_spec(Command,Subcommand,_,_,Spec),
    opt_parse(Spec,Rest,Opts,Positional),
    (   option(help(true), Opts)
    ->  terminusdb_help(Command,Subcommand,Opts)
    ;   run_command(Command,Subcommand,Positional,Opts)).
run_([Command|_Rest]) :-
    setof(Subcommand, command_subcommand(Command,Subcommand), Subcommands),
    format(current_output, "terminusdb ~s [subcommand]~n~twhere subcommand is one of: ~q~n", [Command, Subcommands]),
    format(current_output, "type: terminusdb ~s [subcommand] --help for more details~n", [Command]).
run_(['--version'|_]) :-
    terminusdb_version(TerminusDB_Version),
    current_prolog_flag(terminusdb_git_hash, Git_Hash),
    current_prolog_flag(terminus_store_prolog_version, TerminusDB_Store_Version),
    format(user_output, "TerminusDB v~s (~s)~n", [TerminusDB_Version, Git_Hash]),
    format(user_output, "terminusdb-store v~s~n", [TerminusDB_Store_Version]).
run_(_) :-
    setof(Command, command(Command), Commands),
    format(current_output, "terminusdb [command]~n~twhere command is one of: ~q~n", [Commands]),
    format(current_output, "type: terminusdb [command] --help for more details~n", []).

% Commands
run_command(help,_Positional,Opts) :-
    terminusdb_help(Opts).
run_command(test,_Positional,Opts) :-
    (   (   option(test([]),Opts)
        ->  run_tests
        ;   option(test(Test), Opts),
            run_tests(Test))
    ->  halt(0)
    ;   halt(1)).
run_command(serve,_Positional,Opts) :-
    (   option(interactive(true), Opts)
    ->  terminus_server([serve|Opts], false),
        prolog
    ;   terminus_server([serve|Opts], true)).
run_command(list,Databases,Opts) :-
    super_user_authority(Auth),
    (   Databases = []
    ->  list_databases(system_descriptor{}, Auth, Database_Objects)
    ;   list_existing_databases(Databases, Database_Objects)
    ),
    (   option(json(true), Opts)
    ->  json_write_dict(current_output, Database_Objects)
    ;   pretty_print_databases(Database_Objects)
    ).
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
    option(author(Author), Opts),
    option(message(Message), Opts),
    create_context(Descriptor,commit_info{ author : Author,
                                           message : Message}, Context),
    api_report_errors(
        woql,
        (   woql_context(Prefixes),
            context_extend_prefixes(Context,Prefixes,Context0),
            read_query_term_from_atom(Query,AST),
            query_response:run_context_ast_jsonld_response(Context0, AST, no_data_version, _, Response),
            get_dict(prefixes, Context0, Context_Prefixes),
            default_prefixes(Defaults),
            put_dict(Defaults, Context_Prefixes, Final_Prefixes),
            pretty_print_query_response(Response,Final_Prefixes,String),
            format(current_output,'~s',[String])
        )).
run_command(push,[Path],Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),

    option(remote(Remote_Name_Atom), Opts),
    atom_string(Remote_Name_Atom,Remote_Name),

    option(branch(Branch), Opts),
    option(remote_branch(Remote_Branch), Opts),
    (   var(Remote_Branch)
    ->  Branch = Remote_Branch
    ;   true),

    create_authorization(Opts,Authorization),

    format(current_output, "Pushing to: ~s~n", [Remote_Name]),
    api_report_errors(
        push,
        push(System_DB, Auth, Path, Remote_Name, Remote_Branch, Opts, authorized_push(Authorization), Result)),
    (   Result = same(Commit_Id)
    ->  format(current_output, "Did not push. Both are on commit: ~s~n", [Commit_Id])
    ;   Result = new(Commit_Id)
    ->  format(current_output, "Pushed the latest commit: ~s~n", [Commit_Id])
    ;   throw(error(unexpected_result(push, Result), _))
    ).
run_command(clone,[Remote_URL|DB_Path_List],Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),

    (   DB_Path_List = [],
        re_matchsub('^.*/([^/]*)$', Remote_URL, Match, [])
    % Get the DB name from the URI and organization from switches
    ->  DB = (Match.1),
        option(organization(Organization),Opts)
    ;   DB_Path_List = [DB_Path],
        re_matchsub('([^/]*)/([^/]*)', DB_Path, Match, [])
    % Get the DB and organization name from the argument
    ->  Organization = (Match.1),
        DB = (Match.2)
    % Get the DB from argument and organization from switches
    ;   DB_Path_List = [DB_Path],
        DB = DB_Path,
        option(organization(Organization),Opts)
    ),

    option(label(Label), Opts),
    (   var(Label)
    ->  Label = DB
    ;   true),
    option(comment(Comment), Opts),
    option(public(Public), Opts),

    create_authorization(Opts,Authorization),

    format(current_output, "Cloning: origin~n", []),
    api_report_errors(
        clone,
        clone(System_DB, Auth, Organization, DB, Label, Comment, Public, Remote_URL,
              authorized_fetch(Authorization), _Meta_Data)),
    format(current_output, "Database created: ~s/~s~n", [Organization, DB]).
run_command(pull,[Path],Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),
    option(remote(Remote_Name_Atom), Opts),
    atom_string(Remote_Name_Atom,Remote_Name),
    option(remote_branch(Remote_Branch), Opts),

    create_authorization(Opts,Authorization),

    format(current_output, "Pulling from: ~s~n", [Remote_Name]),
    api_report_errors(
        pull,
        pull(System_DB, Auth, Path, Remote_Name, Remote_Branch,
             authorized_fetch(Authorization), Result)),
    do_or_die(
        status{ 'api:pull_status': Pull_Status,
                'api:fetch_status': Head_Updated } :< Result,
        error(unexpected_result(pull, Result), _)),
    (   Pull_Status = "api:pull_unchanged"
    ->  format(current_output, "Pull unchanged.", [])
    ;   Pull_Status = "api:pull_fast_forwarded"
    ->  format(current_output, "Fast-forward pull.", [])
    ;   format(current_output, "Pull status: ~w.", [Pull_Status])
    ),
    (   Head_Updated = true
    ->  format(current_output, " Branch head updated.", [])
    ;   Head_Updated = false
    ->  format(current_output, " Branch head not updated.", [])
    ;   format(current_output, " Branch head update status: ~w.", [Head_Updated])
    ).
run_command(fetch,[Path],Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),

    api_report_errors(
        fetch,
        do_or_die(
            (   resolve_absolute_string_descriptor(Path,Descriptor),
                _{ branch_name : _} :< Descriptor
            ),
            error(invalid_absolute_path(Path),_))
    ),

    option(remote(Remote_Name_Atom), Opts),
    atom_string(Remote_Name_Atom, Remote_Name),
    % FIXME NOTE: This is very awkward and brittle.
    atomic_list_concat([Path,'/',Remote_Name,'/_commits'], Remote_Path),

    create_authorization(Opts,Authorization),

    api_report_errors(
        fetch,
        remote_fetch(System_DB, Auth, Remote_Path, authorized_fetch(Authorization),
                     New_Head_Layer_Id, Head_Has_Updated)
    ),
    format(current_output, "~N~s fetch: ~q with ~q~n",
           [Path, New_Head_Layer_Id, Head_Has_Updated]).
run_command(rebase,[Path,From_Path],Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),
    option(author(Author),Opts),
    api_report_errors(
        rebase,
        (   Strategy_Map = [],
            rebase_on_branch(System_DB, Auth, Path, From_Path, Author, Strategy_Map, Common_Commit_ID_Option, Forwarded_Commits, Reports))),
    format(current_output, "~nRebased from: ~q to ~q~n", [From_Path, Path]),
    format(current_output, "Forwarded_Commits: ~q~n", [Forwarded_Commits]),
    (   Common_Commit_ID_Option = some(Common_Commit_ID)
    ->  format(current_output, "From common commit: ~q~n", [Common_Commit_ID])
    ;   true),
    format(current_output, "Reports: ", []),
    json_write(current_output,Reports),
    format(current_output, "~n", []).
run_command(rollup,[Path],_Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),

    api_report_errors(
        rollup,
        api_rollup(System_DB, Auth, Path, [], _Status_List)),

    format(current_output, "~nRollup performed~n", []).
run_command(bundle,[Path], Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),

    option(output(Filename), Opts),
    (   var(Filename)
    ->  www_form_encode(Path,Base),
        atomic_list_concat([Base,".bundle"],Filename)
    ;   true),

    api_report_errors(
        bundle,
        bundle(System_DB, Auth, Path, Payload, [])),

    (   var(Payload)
    ->  format(current_output, "~nNo data to be bundled~n", [])
    ;   format(current_output, "~nBundle operation performed~n", []),
        format(current_output, "~nExporting as ~s~n", [Filename]),
        open(Filename, write, Stream),
        format(Stream, "~s", [Payload])
    ).
run_command(unbundle,[Path, Filename], _Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),

    catch(
        (   read_file_to_string(Filename, Payload, []),

            api_report_errors(
                unbundle,
                unbundle(System_DB, Auth, Path, Payload)),

            format(current_output, "~nUnbundle successful~n", [])),
        E,
        (   E = error(existence_error(source_sink, File), _)
        ->  format(current_output, "~nFile ~s does not exist", [File])
        ;   throw(E))).
run_command(diff, Args, Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),

    option(before(Before_Atom), Opts),
    option(after(After_Atom), Opts),
    option(keep(Keep_Atom), Opts),
    option(docid(DocId), Opts),
    option(before_commit(Before_Commit), Opts),
    option(after_commit(After_Commit), Opts),

    api_report_errors(
        diff,
        (   \+ var(Before_Atom), \+ var(After_Atom)
        ->  atom_json_dict(Before_Atom, Before, [default_tag(json)]),
            atom_json_dict(After_Atom, After, [default_tag(json)]),
            atom_json_dict(Keep_Atom, Keep, [default_tag(json)]),
            api_diff(System_DB, Auth, Before, After, Keep, Patch)
        ;   \+ var(DocId), \+ var(Before_Commit), \+ var(After_Commit),
            [Path] = Args
        ->  atom_json_dict(Keep_Atom, Keep, [default_tag(json)]),
            api_diff_id(System_DB, Auth, Path, Before_Commit,
                        After_Commit, DocId, Keep, Patch)
        ;   \+ var(After_Commit), \+ var(Before_Commit),
            [Path] = Args
        ->  atom_json_dict(Keep_Atom, Keep, [default_tag(json)]),
            api_diff_all_documents(System_DB, Auth, Path,
                                   Before_Commit, After_Commit,
                                   Keep, Patch)
        ;   \+ var(DocId), \+ var(After_Atom), \+ var(Before_Commit)
        ->  atom_json_dict(After_Atom, After, [default_tag(json)]),
            atom_json_dict(Keep_Atom, Keep, [default_tag(json)]),
            api_diff_id_document(System_DB, Auth, Path,
                                 Before_Commit, After,
                                 DocId, Keep, Patch)
        )
    ),
    json_write_dict(user_output, Patch, [width(0)]),
    nl.
run_command(log,[Path], _Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),
    api_log(System_DB, Auth, Path, Log),
    format_log(current_output,Log).
run_command(Command,_Args, Opts) :-
    terminusdb_help(Command,Opts).

% Subcommands
run_command(branch,create,[Path],Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),

    option(origin(Origin_Base), Opts),
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
        option(organization(Organization),Opts)
    ),
    option(label(Label), Opts),
    option(comment(Comment), Opts),
    option(public(Public), Opts),
    option(schema(Schema), Opts),
    option(data_prefix(Data_Prefix), Opts),
    option(schema_prefix(Schema_Prefix), Opts),
    option(prefixes(Prefixes_Atom), Opts),
    atom_json_dict(Prefixes_Atom, Prefixes, []),
    put_dict(Prefixes, _{'@base' : Data_Prefix, '@schema' : Schema_Prefix}, Merged),
    api_report_errors(
        create_db,
        create_db(System_DB, Auth, Organization, DB, Label, Comment, Schema, Public, Merged)),
    format(current_output, "Database created: ~s/~s~n", [Organization, DB]).
run_command(db,delete,[DB_Path],Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),

    (   re_matchsub('([^/]*)/([^/]*)', DB_Path, Match, [])
    ->  Organization = (Match.1),
        DB = (Match.2)
    ;   DB = DB_Path,
        option(organization(Organization), Opts)
    ),
    option(force(Force_Delete), Opts),
    api_report_errors(
        delete_db,
        delete_db(System_DB, Auth, Organization, DB, Force_Delete)),
    format(current_output, "Database deleted: ~s/~s~n", [Organization, DB]).
run_command(doc,insert,[Path], Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),
    option(author(Author), Opts),
    option(message(Message), Opts),
    option(graph_type(Graph_Type), Opts),
    option(data(Data), Opts),
    option(full_replace(Full), Opts),

    api_report_errors(
        insert_documents,
        (   (   var(Data)
            ->  with_memory_file(cli:doc_insert_memory_file(System_DB, Auth, Path, Graph_Type, Author, Message, Full, Ids))
            ;   open_string(Data, Stream),
                doc_insert_stream(System_DB, Auth, Path, Graph_Type, Author, Message, Full, Ids, Stream)
            ),
            length(Ids, Number_Inserted),
            format("Document(s) inserted: ~d~n", [Number_Inserted])
        )
    ).
run_command(doc,delete, [Path], Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),
    option(author(Author), Opts),
    option(message(Message), Opts),
    option(graph_type(Graph_Type), Opts),
    option(id(Id), Opts),
    option(nuke(Nuke), Opts),
    option(data(Data), Opts),

    api_report_errors(
        delete_documents,
        (   Nuke = true
        ->  api_nuke_documents(System_DB, Auth, Path, Graph_Type, Author, Message, no_data_version, _),
            format("Documents nuked~n", [])
        ;   ground(Id)
        ->  api_delete_document(System_DB, Auth, Path, Graph_Type, Author, Message, Id, no_data_version, _),
            format("Document deleted: ~s~n", [Id])
        ;   (   var(Data)
            ->  with_memory_file(doc_delete_memory_file(System_DB, Auth, Path, Graph_Type, Author, Message))
            ;   open_string(Data, Stream),
                api_delete_documents(System_DB, Auth, Path, Graph_Type, Author, Message, Stream, no_data_version, _)
            ),
            format("Documents deleted~n", [Graph_Type])
        )
    ).
run_command(doc,replace, [Path], Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),
    option(author(Author), Opts),
    option(message(Message), Opts),
    option(graph_type(Graph_Type), Opts),
    option(create(Create), Opts),
    option(data(Data), Opts),

    api_report_errors(
        replace_documents,
        (   (   var(Data)
            ->  with_memory_file(doc_replace_memory_file(System_DB, Auth, Path, Graph_Type, Author, Message, Create, Ids))
            ;   open_string(Data, Stream),
                api_replace_documents(System_DB, Auth, Path, Graph_Type, Author, Message, Stream,
                                      Create, no_data_version, _, Ids)
            ),
            length(Ids, Number_Inserted),
            format("Document(s) replaced: ~d~n", [Number_Inserted]),
            json_write_dict(user_output, Ids, [width(0)]),
            format(user_output, "~n", [])
        )
    ).
run_command(doc,get, [Path], Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),
    option(graph_type(Graph_Type), Opts),
    option(skip(S), Opts),
    option(count(N), Opts),
    option(minimized(Minimized), Opts),
    option(as_list(As_List), Opts),
    option(unfold(Unfold), Opts),
    option(id(Id), Opts),
    option(type(Type), Opts),
    option(compress_ids(Compress_Ids), Opts),
    option(query(Query), Opts),

    (   N = unlimited
    ->  Count = unlimited
    ;   atom_number(N,Count)
    ),
    atom_number(S,Skip),

    (   Minimized = true
    ->  JSON_Options = [width(0)]
    ;   JSON_Options = []),

    api_report_errors(
        get_documents,
        api_read_document_selector(
            System_DB, Auth, Path, Graph_Type, Skip, Count,
            As_List, Unfold, Id, Type, Compress_Ids, Query,
            JSON_Options,
            no_data_version, _Actual_Data_Version,
            [L]>>(ignore((L=true,format('[')))))
    ).
run_command(store,init, _, Opts) :-
    (   option(key(Key), Opts)
    ->  true
    ;   format(current_output, "You must supply an administrator key to initialize the database!~n",[]),
        fail),
    option(force(Force), Opts),
    api_report_errors(
        store_init,
        initialize_database(Key,Force)),
    format('Successfully initialised database!!!~n').
run_command(remote,add,[Path,Remote_Name,URL],_Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),
    api_report_errors(
        remote,
        add_remote(System_DB, Auth, Path, Remote_Name, URL)),
    format(current_output,'Successfully added remote ~s with url ~s~n',[Remote_Name,URL]).
run_command(remote,remove,[Path,Remote_Name],_Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),
    api_report_errors(
        remote,
        remove_remote(System_DB, Auth, Path, Remote_Name)),
    format(current_output,'Successfully added remote ~s~n',[Remote_Name]).
run_command(remote,'set-url',[Path,Remote_Name,URL],_Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),
    api_report_errors(
        remote,
        update_remote(System_DB, Auth, Path, Remote_Name, URL)),
    format(current_output,'Successfully set remote url to ~s~n',[URL]).
run_command(remote,'get-url',[Path|Remote_Name_List],Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),

    (   Remote_Name_List = []
    ->  option(remote(Remote_Name), Opts)
    ;   Remote_Name_List = [Remote_Name]),

    api_report_errors(
        remote,
        show_remote(System_DB, Auth, Path, Remote_Name, URL)),
    format(current_output,'Remote ~s associated with url ~s~n',[Remote_Name,URL]).
run_command(remote,list,[Path],_Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),
    api_report_errors(
        remote,
        list_remotes(System_DB, Auth, Path, Remote_Names)),
    format(current_output,'Remotes: ~q~n',[Remote_Names]).
run_command(triples,dump,[Path],Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),
    option(format(Format_Atom), Opts),
    atom_string(Format_Atom,Format),
    api_report_errors(
        triples,
        graph_dump(System_DB, Auth, Path, Format, String)),
    format(current_output,'~s',[String]).
run_command(triples,update,[Path,File],Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),
    option(message(Message), Opts),
    option(author(Author), Opts),
    option(format(Format_Atom), Opts),
    atom_string(Format_Atom,Format),
    open(File,read,Stream),
    read_string(Stream, _, TTL),
    close(Stream),
    api_report_errors(
        triples,
        graph_update(System_DB, Auth, Path, _{ message : Message,
                                               author : Author},
                     Format,TTL)),
    format(current_output,'~nSuccessfully updated triples from ~q~n',[File]).
run_command(triples,load,[Path,File],Opts) :-
    super_user_authority(Auth),
    create_context(system_descriptor{}, System_DB),
    option(message(Message), Opts),
    option(author(Author), Opts),
    option(format(Format_Atom), Opts),
    atom_string(Format_Atom,Format),
    open(File,read,Stream),
    read_string(Stream, _, TTL),
    close(Stream),
    api_report_errors(
        triples,
        graph_insert(System_DB, Auth, Path, _{ message : Message,
                                               author : Author},
                     Format,TTL)),
    format(current_output,'~nSuccessfully inserted triples from ~q~n',[File]).
run_command(Command,Subcommand,_Args,_Opts) :-
    format_help(Command,Subcommand).

doc_replace_memory_file(System_DB, Auth, Path, Graph_Type, Author, Message, Create, Ids, Mem_File) :-
    % Copy stdin to a memory file.
    with_memory_file_stream(Mem_File, write, copy_stream_data(user_input)),
    % Read the memory file to insert documents.
    with_memory_file_stream(Mem_File, read, doc_replace_stream(System_DB, Auth, Path, Graph_Type, Author, Message, Create, Ids)).

doc_replace_stream(System_DB, Auth, Path, Graph_Type, Author, Message, Create, Ids, Stream) :-
    api_replace_documents(System_DB, Auth, Path, Graph_Type, Author, Message, Stream,
                          Create, no_data_version, _, Ids).

doc_delete_memory_file(System_DB, Auth, Path, Graph_Type, Author, Message, Mem_File) :-
    % Copy stdin to a memory file.
    with_memory_file_stream(Mem_File, write, copy_stream_data(user_input)),
    % Read the memory file to insert documents.
    with_memory_file_stream(Mem_File, read, doc_delete_stream(System_DB, Auth, Path, Graph_Type, Author, Message)).

doc_delete_stream(System_DB, Auth, Path, Graph_Type, Author, Message, Stream) :-
    api_delete_documents(System_DB, Auth, Path, Graph_Type, Author, Message, Stream,
                         no_data_version, _).

doc_insert_memory_file(System_DB, Auth, Path, Graph_Type, Author, Message, Full_Replace, Ids, Mem_File) :-
    % Copy stdin to a memory file.
    with_memory_file_stream(Mem_File, write, copy_stream_data(user_input)),
    % Read the memory file to insert documents.
    with_memory_file_stream(Mem_File, read, doc_insert_stream(System_DB, Auth, Path, Graph_Type, Author, Message, Full_Replace, Ids)).

doc_insert_stream(System_DB, Auth, Path, Graph_Type, Author, Message, Full_Replace, Ids, Stream) :-
    api_insert_documents(
        System_DB, Auth, Path, Graph_Type, Author, Message, Full_Replace, Stream,
        no_data_version, _New_Data_Version, Ids).

create_authorization(Opts,Authorization) :-
    option(token(Token), Opts),
    (   var(Token)
    ->  option(user(User), Opts),
        (   var(User)
        ->  prompt(_,'Username: '),
            read_string(user_input, ['\n'], [], _, User)
        ;   true),

        option(password(Password), Opts),
        (   var(Password)
        ->  prompt(_,'Password: '),
            read_string(user_input, ['\n'], [], _, Password)
        ;   true),

        basic_authorization(User,Password,Authorization)
    ;   token_authorization(Token,Authorization)
    ).

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
    json_write_dict(user_error,JSON, []),
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
    (   option(markdown(true),Opts)
    ->  format_help_markdown(Command)
    ;   format_help(Command)
    ).

terminusdb_help(Command,Subcommand,Opts) :-
    (   option(markdown(true),Opts)
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
    intersperse(', ',LFlags1,LFlags2),
    atomic_list_concat(LFlags2, LFlags),

    memberchk(help(Help), Opt),

    format(current_output, '  * ~s, ~s=[value]:~n', [SFlags,LFlags]),
    format(current_output, '  ~s~n~n', [Help]).

bind_vars([],_).
bind_vars([Name=Var|Tail],AST) :-
    Var = v(Name),
    bind_vars(Tail,AST).

read_query_term_from_atom(Query, AST) :-
    read_term_from_atom(Query, AST, [variable_names(Names)]),
    bind_vars(Names,AST).

fetch_payload(Payload, _, none, some(Payload)).
