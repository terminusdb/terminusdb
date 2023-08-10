:- module(api_init, [
              bootstrap_files/0,
              initialize_flags/0,
              initialize_database/2,
              initialize_database_with_store/2,
              index_template/1,
              world_ontology_json/1,
              graphiql_template/1,
              update_system_graphs/0
          ]).

:- use_module(core(triple)).
:- use_module(core(util)).
:- use_module(core(document)).
:- use_module(core(query), [expand/2, default_prefixes/1, create_context/3]).
:- use_module(core(transaction), [open_descriptor/2]).
:- use_module(core(account), [generate_password_hash/2]).

:- use_module(config(terminus_config)).

:- use_module(library(semweb/turtle)).
:- use_module(library(terminus_store)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(yall)).
:- use_module(library(plunit)).
:- use_module(library(filesex)).
:- use_module(library(git), [git_hash/2]).

/**
 * initialize_flags is det.
 *
 * Initialize flags shared by all main predicates.
 */
initialize_flags :-
    (   catch(git_hash(Git_Hash, []), _, false)
    ->  true
    ;   getenv('TERMINUSDB_GIT_HASH', Git_Hash)
    ->  true
    ;   Git_Hash = null
    ),
    set_prolog_flag(terminusdb_git_hash, Git_Hash).

/**
 * create_graph_from_turtle(DB:database, Graph_ID:graph_identifier, Turtle:string) is det.
 *
 * Reads in Turtle String and writes initial database.
 */
create_graph_from_turtle(Store, Graph_ID, TTL_Path) :-
    safe_create_named_graph(Store,Graph_ID,Graph_Obj),
    open_write(Store, Builder),

    % write to a temporary builder.
    rdf_process_turtle(
        TTL_Path,
        {Builder}/
        [Triples,_Resource]>>(
            forall(member(T, Triples),
                   (   normalise_triple(T, rdf(X,P,Y)),
                       object_storage(Y,S),
                       nb_add_triple(Builder, X, P, S)))),
        []),
    % commit this builder to a temporary layer to perform a diff.
    nb_commit(Builder,Layer),
    nb_set_head(Graph_Obj, Layer).

:- dynamic template_system_instance/1.
:- dynamic system_schema/1.
:- dynamic repo_schema/1.
:- dynamic layer_schema/1.
:- dynamic ref_schema/1.
:- dynamic woql_schema/1.
:- dynamic index_template/1.
:- dynamic world_ontology_json/1.
:- dynamic graphiql_template/1.
bootstrap_files :-
    template_system_instance_json(InstancePath),
    file_to_predicate(InstancePath, template_system_instance),
    system_schema_json(SchemaPath),
    file_to_predicate(SchemaPath, system_schema),
    repository_schema_json(RepoPath),
    file_to_predicate(RepoPath, repo_schema),
    ref_schema_json(RefSchemaPath),
    file_to_predicate(RefSchemaPath, ref_schema),
    woql_schema_json(WOQLSchemaPath),
    file_to_predicate(WOQLSchemaPath, woql_schema),
    index_template_path(IndexTemplatePath),
    file_to_predicate(IndexTemplatePath, index_template),
    world_ontology_json_path(OntJsonPath),
    file_to_predicate(OntJsonPath, world_ontology_json),
    graphiql_template_path(GraphIQLTemplatePath),
    file_to_predicate(GraphIQLTemplatePath, graphiql_template).

template_system_instance_json(Path) :-
    once(expand_file_search_path(ontology('system_instance_template.json'), Path)).

system_schema_json(Path) :-
    once(expand_file_search_path(ontology('system_schema.json'), Path)).

repository_schema_json(Path) :-
    once(expand_file_search_path(ontology('repository.json'), Path)).

ref_schema_json(Path) :-
    once(expand_file_search_path(ontology('ref.json'), Path)).

woql_schema_json(Path) :-
    once(expand_file_search_path(ontology('woql.json'), Path)).

index_template_path(Path) :-
    once(expand_file_search_path(config('index.tpl'), Path)).

world_ontology_json_path(Path) :-
    once(expand_file_search_path(test('worldOnt.json'), Path)).

graphiql_template_path(Path) :-
    once(expand_file_search_path(server('templates/graphiql.tpl'), Path)).

config_path(Path) :-
    once(expand_file_search_path(config('terminus_config.pl'), Path)).

initialize_database(Key,Force) :-
    db_path(DB_Path),
    initialize_database_with_path(Key, DB_Path, Force).

/*
 * initialize_database_with_path(Key,DB_Path,Force) is det+error.
 *
 * initialize the database unless it already exists or Force is false.
 */
initialize_database_with_path(_, DB_Path, false) :-
    storage_version_path(DB_Path, Version),
    exists_file(Version),
    throw(error(storage_already_exists(DB_Path),_)).
initialize_database_with_path(Key, DB_Path, _) :-
    make_directory_path(DB_Path),
    delete_directory_contents(DB_Path),
    set_db_version(DB_Path),
    open_archive_store(DB_Path, Store),
    initialize_database_with_store(Key, Store).

initialize_schema_graph(Simple_Graph_Name, Store, Graph_Name, Graph_String, Force, Layer) :-
    (   Force = true
    ->  ignore(safe_delete_named_graph(Store, Graph_Name))
    ;   safe_named_graph_exists(Store, Graph_Name)
    ->  throw(error(schema_graph_already_exists(Simple_Graph_Name), _))
    ;   true),

    open_string(Graph_String, Graph_Stream),
    create_graph_from_json(Store, Graph_Name, Graph_Stream, schema, Layer).

initialize_system_schema(Store, Force, Layer) :-
    system_schema_name(Schema_Name),
    system_schema(System_Schema_String),
    initialize_schema_graph(system, Store, Schema_Name, System_Schema_String, Force, Layer).

initialize_ref_schema(Store, Force) :-
    ref_ontology(Ref_Name),
    ref_schema(Ref_Schema_String),
    initialize_schema_graph(ref, Store, Ref_Name, Ref_Schema_String, Force, _).

initialize_repo_schema(Store, Force) :-
    repository_ontology(Repo_Name),
    repo_schema(Repo_Schema_String),
    initialize_schema_graph(repo, Store, Repo_Name, Repo_Schema_String, Force, _).

initialize_woql_schema(Store, Force) :-
    woql_ontology(WOQL_Name),
    woql_schema(WOQL_Schema_String),
    initialize_schema_graph(woql, Store, WOQL_Name, WOQL_Schema_String, Force, _).

initialize_system_instance(Store, Schema_Layer, Key, Force) :-
    system_instance_name(Instance_Name),
    (   Force = true
    ->  safe_delete_named_graph(Store, Instance_Name)
    ;   safe_named_graph_exists(Store, Instance_Name)
    ->  throw(error(instance_graph_already_exists(system), _))
    ;   true),

    Descriptor = layer_descriptor{ schema: Schema_Layer, variety: system_descriptor},
    open_descriptor(Descriptor, Transaction_Object),

    template_system_instance(Template_Instance_String),
    generate_password_hash(Key,Hash),
    format(string(Instance_String), Template_Instance_String, [Hash]),
    open_string(Instance_String, Instance_Stream),
    create_graph_from_json(Store,Instance_Name,Instance_Stream,
                           instance(Transaction_Object),_).

initialize_database_with_store(Key, Store) :-
    initialize_database_with_store(Key, Store, false).
initialize_database_with_store(Key, Store, Force) :-
    initialize_system_schema(Store, Force, System_Schema),
    initialize_ref_schema(Store, Force),
    initialize_repo_schema(Store, Force),
    initialize_woql_schema(Store, Force),

    initialize_system_instance(Store, System_Schema, Key, Force).

current_repository_version("v1.0.1").
current_ref_version("v1.0.1").

has_no_store :-
    catch(
        (   triple_store(_),
            fail),
        error(no_database_store_version, _),
        true
    ).

current_schema_version(repo_schema, Version) :-
    current_repository_version(Version).
current_schema_version(ref_schema, Version) :-
    current_ref_version(Version).

update_system_graph(Label, Path, Predicate, Initialization) :-
    Descriptor = label_descriptor{
                     schema:Label,
                     variety:repository_descriptor
                 },

    open_descriptor(Descriptor, Transaction_Object),
    Commit_Info = commit_info{author:"test",message:"test"},
    create_context(Transaction_Object, Commit_Info, Context),

    (   database_context_object(Context, Obj),
        get_dict('@metadata', Obj, Metadata),
        get_dict('schema_version', Metadata, Version),
        current_schema_version(Predicate, Version)
    % already current
    ->  true
    % needs an upgrade
    ;   json_log_notice_formatted("Upgrading ~s",[Label]),
        file_to_predicate(Path, Predicate),
        triple_store(Store),
        call(Initialization, Store, true),
        % remove anything already pinned
        abolish_all_tables
    ).

update_repository_graph :-
    repository_ontology(Repo_Label),
    api_init:repository_schema_json(Repo_Path),
    update_system_graph(Repo_Label,
                        Repo_Path,
                        repo_schema,
                        api_init:initialize_repo_schema).

update_commit_graph :-
    ref_ontology(Ref_Label),
    api_init:ref_schema_json(Ref_Path),
    update_system_graph(Ref_Label,
                        Ref_Path,
                        ref_schema,
                        api_init:initialize_ref_schema).

update_system_graphs :-
    (   has_no_store
    ->  true
    ;   update_repository_graph,
        update_commit_graph).

% FIXME! These tests should go into `src/config/terminus_config.pl`, but I
% couldn't run them when they were there.
:- begin_tests(env_vars).

test("TERMINUSDB_INSECURE_USER_HEADER_ENABLED is not set",
     [ setup(clear_check_insecure_user_header_enabled),
       cleanup(clear_check_insecure_user_header_enabled),
       true(Enabled = false)
     ]) :-
    check_insecure_user_header_enabled(Enabled).

test("TERMINUSDB_INSECURE_USER_HEADER_ENABLED=true",
     [ setup((
           clear_check_insecure_user_header_enabled,
           setenv('TERMINUSDB_INSECURE_USER_HEADER_ENABLED', true)
       )),
       cleanup((
           clear_check_insecure_user_header_enabled,
           unsetenv('TERMINUSDB_INSECURE_USER_HEADER_ENABLED'))
       ),
       true(Enabled = true)
     ]) :-
    check_insecure_user_header_enabled(Enabled).

test("TERMINUSDB_INSECURE_USER_HEADER_ENABLED has a bad value",
     [ setup((
           clear_check_insecure_user_header_enabled,
           setenv('TERMINUSDB_INSECURE_USER_HEADER_ENABLED', 42)
       )),
       cleanup((
           clear_check_insecure_user_header_enabled,
           unsetenv('TERMINUSDB_INSECURE_USER_HEADER_ENABLED')
       )),
       throws(error(bad_env_var_value('TERMINUSDB_INSECURE_USER_HEADER_ENABLED', '42'), _))
     ]) :-
    check_insecure_user_header_enabled(_).

test("TERMINUSDB_INSECURE_USER_HEADER is not set",
     [ setup((
           clear_check_insecure_user_header_enabled,
           clear_insecure_user_header_key,
           setenv('TERMINUSDB_INSECURE_USER_HEADER_ENABLED', true)
       )),
       cleanup((
           clear_check_insecure_user_header_enabled,
           clear_insecure_user_header_key,
           unsetenv('TERMINUSDB_INSECURE_USER_HEADER_ENABLED')
       )),
       throws(error(missing_env_var('TERMINUSDB_INSECURE_USER_HEADER'), _))
     ]) :-
    insecure_user_header_key(_).

test("TERMINUSDB_INSECURE_USER_HEADER is missing",
     [ setup((
           clear_check_insecure_user_header_enabled,
           clear_insecure_user_header_key,
           setenv('TERMINUSDB_INSECURE_USER_HEADER_ENABLED', true)
       )),
       cleanup((
           clear_check_insecure_user_header_enabled,
           clear_insecure_user_header_key,
           unsetenv('TERMINUSDB_INSECURE_USER_HEADER_ENABLED')
       )),
       throws(error(missing_env_var('TERMINUSDB_INSECURE_USER_HEADER'), _))
     ]) :-
    insecure_user_header_key(_).

test("TERMINUSDB_INSECURE_USER_HEADER has a bad value",
     [ setup((
           clear_check_insecure_user_header_enabled,
           clear_insecure_user_header_key,
           setenv('TERMINUSDB_INSECURE_USER_HEADER_ENABLED', true),
           setenv('TERMINUSDB_INSECURE_USER_HEADER', '')
       )),
       cleanup((
           clear_check_insecure_user_header_enabled,
           clear_insecure_user_header_key,
           unsetenv('TERMINUSDB_INSECURE_USER_HEADER_ENABLED'),
           unsetenv('TERMINUSDB_INSECURE_USER_HEADER')
       )),
       throws(error(bad_env_var_value('TERMINUSDB_INSECURE_USER_HEADER', ''), _))
     ]) :-
    insecure_user_header_key(_).

test("TERMINUSDB_INSECURE_USER_HEADER=TerminusDB-5",
     [ setup((
           clear_check_insecure_user_header_enabled,
           clear_insecure_user_header_key,
           setenv('TERMINUSDB_INSECURE_USER_HEADER_ENABLED', true),
           setenv('TERMINUSDB_INSECURE_USER_HEADER', 'TerminusDB-5')
       )),
       cleanup((
           clear_check_insecure_user_header_enabled,
           clear_insecure_user_header_key,
           unsetenv('TERMINUSDB_INSECURE_USER_HEADER_ENABLED'),
           unsetenv('TERMINUSDB_INSECURE_USER_HEADER')
       )),
       true(Header_Key = terminusdb_5)
     ]) :-
    insecure_user_header_key(Header_Key).

test("TERMINUSDB_SERVER_DB_PATH is not set", [true(DB_Path = Expected_Path)]) :-
    config:default_database_path(DB_Path),
    working_directory(CWD, CWD),
    directory_file_path(CWD, "storage/db", Expected_Path).

test("TERMINUSDB_SERVER_DB_PATH is empty",
     [ setup(setenv('TERMINUSDB_SERVER_DB_PATH', '')),
       cleanup(unsetenv('TERMINUSDB_SERVER_DB_PATH')),
       true(DB_Path = Expected_Path)
     ]) :-
    config:default_database_path(DB_Path),
    working_directory(CWD, CWD),
    directory_file_path(CWD, "storage/db", Expected_Path).

test("TERMINUSDB_SERVER_DB_PATH=../relative/path",
     [ setup(setenv('TERMINUSDB_SERVER_DB_PATH', "../relative/path")),
       cleanup(unsetenv('TERMINUSDB_SERVER_DB_PATH')),
       true(DB_Path = Expected_Path)
     ]) :-
    config:default_database_path(DB_Path),
    working_directory(CWD, CWD),
    directory_file_path(CWD, "..", Rel_Parent_Dir),
    absolute_file_name(Rel_Parent_Dir, Parent_Dir),
    directory_file_path(Parent_Dir, "relative/path", Expected_Path).

test("TERMINUSDB_SERVER_DB_PATH=/absolute/path",
     [ setup(setenv('TERMINUSDB_SERVER_DB_PATH', "/absolute/path")),
       cleanup(unsetenv('TERMINUSDB_SERVER_DB_PATH')),
       true(DB_Path = '/absolute/path')
     ]) :-
    config:default_database_path(DB_Path).

:- end_tests(env_vars).
