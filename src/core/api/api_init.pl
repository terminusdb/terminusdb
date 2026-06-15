:- module(api_init, [
              bootstrap_files/0,
              initialize_flags/0,
              initialize_database/2,
              initialize_database_with_store/2,
              index_template/1,
              world_ontology_json/1,
              woql_schema/1,
              graphiql_template/1,
              update_system_graphs/0
          ]).

:- use_module(core(triple)).
:- use_module(core(util)).
:- use_module(core(util/test_utils),
             [setup_temp_store/1, teardown_temp_store/1,
              create_db_without_schema/2, create_db_with_empty_schema/2]).
:- use_module(core(api/api_document), [api_insert_documents/8]).
:- use_module(core(api/db_branch), [branch_create/5]).
:- use_module(core(document)).
:- use_module(core(query), [expand/2, default_prefixes/1, create_context/3,
                            resolve_absolute_string_descriptor/2,
                            ask/2]).
:- use_module(core(transaction), [open_descriptor/2, with_transaction/3]).
:- use_module(core(transaction/ref_entity),
             [branch_head_commit/3, commit_id_uri/3]).
:- use_module(core(account), [generate_password_hash/2]).

:- use_module(config(terminus_config)).

:- use_module(library(semweb/turtle)).
:- use_module(library(terminus_store)).
:- use_module(library(http/json)).
:- use_module(library(http/http_authenticate), [http_authorization_data/2]).
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

current_woql_version("v1.0.3").
current_repository_version("v1.0.1").
current_ref_version("v1.0.2").

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
current_schema_version(woql_schema, Version) :-
    current_woql_version(Version).

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

update_woql_graph :-
    woql_ontology(WOQL_Label),
    api_init:woql_schema_json(WOQL_Path),
    update_system_graph(WOQL_Label,
                        WOQL_Path,
                        woql_schema,
                        api_init:initialize_woql_schema).

update_system_graphs :-
    (   has_no_store
    ->  true
    ;   update_repository_graph,
        update_commit_graph,
        update_woql_graph
    ).

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

% --------------------------------------------------------------------------
% Shared test helper: reset indexer env to a clean state.
%
% Lives at MODULE level (not inside any begin_tests/end_tests block) so it is
% accessible from ALL test suites in this file — both indexer_backend_selector
% and push_driver use it in setup/cleanup.
%
% SYMMETRIC WITH clear_indexer_backend_config/0 in terminus_config.pl:
% that predicate abolishes ALL five tabled predicates (indexer_backend,
% tdb_search_endpoint, semantic_indexer_endpoint, tdb_search_admin_user,
% tdb_search_admin_secret). This predicate unsets the CORRESPONDING env
% vars so the tables, once cleared, don't re-read stale leaked values.
% Keep both predicates in sync — see terminus_config.pl:443-448.
% --------------------------------------------------------------------------
clean_indexer_env :-
    clear_indexer_backend_config,
    unsetenv('TERMINUSDB_INDEXER_BACKEND'),
    unsetenv('TERMINUSDB_SEMANTIC_INDEXER_ENDPOINT'),
    unsetenv('TERMINUSDB_TDB_SEARCH_ENDPOINT'),
    unsetenv('TERMINUSDB_SEARCH_ADMIN_USER'),
    unsetenv('TERMINUSDB_SEARCH_ADMIN_SECRET').

:- begin_tests(indexer_backend_selector).

/*
 * Semantic-indexer backend selector (Spec 16 §2.3 / RISK-17). These tests live
 * here rather than in terminus_config.pl because config is loaded before
 * set_test_options(load(always)) during bootstrap, so plunit blocks placed in
 * it are discarded (same reason the insecure-user-header config tests live in
 * this module).
 *
 * The selector predicates are tabled and read process env vars, so every test
 * clears the tables (clear_indexer_backend_config) and unsets BOTH endpoint
 * vars in setup and cleanup, guaranteeing each test starts from a clean,
 * fully-unset configuration regardless of host env or test ordering.
 */

test("default backend is none when unset",
     [ setup(clean_indexer_env),
       cleanup(clean_indexer_env),
       true(Backend == none)
     ]) :-
    indexer_backend(Backend).

test("none with no endpoints set passes the startup check",
     [ setup(clean_indexer_env),
       cleanup(clean_indexer_env)
     ]) :-
    check_indexer_backend_config.

test("explicit none with no endpoints passes the startup check",
     [ setup((clean_indexer_env,
              setenv('TERMINUSDB_INDEXER_BACKEND', none))),
       cleanup(clean_indexer_env)
     ]) :-
    check_indexer_backend_config.

test("unknown backend value fails loud",
     [ setup((clean_indexer_env,
              setenv('TERMINUSDB_INDEXER_BACKEND', wibble))),
       cleanup(clean_indexer_env),
       throws(error(bad_env_var_value('TERMINUSDB_INDEXER_BACKEND', wibble), _))
     ]) :-
    indexer_backend(_).

test("unknown backend value refuses startup",
     [ setup((clean_indexer_env,
              setenv('TERMINUSDB_INDEXER_BACKEND', nonsense))),
       cleanup(clean_indexer_env),
       throws(error(bad_env_var_value('TERMINUSDB_INDEXER_BACKEND', nonsense), _))
     ]) :-
    check_indexer_backend_config.

test("http_vectorlink with its endpoint resolves and passes the check",
     [ setup((clean_indexer_env,
              setenv('TERMINUSDB_INDEXER_BACKEND', http_vectorlink),
              setenv('TERMINUSDB_SEMANTIC_INDEXER_ENDPOINT', 'http://vectorlink:8080'))),
       cleanup(clean_indexer_env),
       true(Backend == http_vectorlink)
     ]) :-
    check_indexer_backend_config,
    indexer_backend(Backend).

test("http_tdb_search with its endpoint resolves and passes the check",
     [ setup((clean_indexer_env,
              setenv('TERMINUSDB_INDEXER_BACKEND', http_tdb_search),
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', 'http://tdb-search:8080'))),
       cleanup(clean_indexer_env),
       true(Endpoint == 'http://tdb-search:8080')
     ]) :-
    check_indexer_backend_config,
    tdb_search_endpoint(Endpoint).

test("http_vectorlink without its endpoint refuses startup",
     [ setup((clean_indexer_env,
              setenv('TERMINUSDB_INDEXER_BACKEND', http_vectorlink))),
       cleanup(clean_indexer_env),
       throws(error(indexer_backend_incomplete(http_vectorlink, _), _))
     ]) :-
    check_indexer_backend_config.

test("http_tdb_search without its endpoint refuses startup",
     [ setup((clean_indexer_env,
              setenv('TERMINUSDB_INDEXER_BACKEND', http_tdb_search))),
       cleanup(clean_indexer_env),
       throws(error(indexer_backend_incomplete(http_tdb_search, _), _))
     ]) :-
    check_indexer_backend_config.

test("none with legacy endpoint set is ambiguous and refuses startup",
     [ setup((clean_indexer_env,
              setenv('TERMINUSDB_SEMANTIC_INDEXER_ENDPOINT', 'http://vectorlink:8080'))),
       cleanup(clean_indexer_env),
       throws(error(indexer_backend_ambiguous(none, _), _))
     ]) :-
    check_indexer_backend_config.

test("none with tdb-search endpoint set is ambiguous and refuses startup",
     [ setup((clean_indexer_env,
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', 'http://tdb-search:8080'))),
       cleanup(clean_indexer_env),
       throws(error(indexer_backend_ambiguous(none, _), _))
     ]) :-
    check_indexer_backend_config.

test("http_vectorlink with both endpoints set is ambiguous and refuses startup",
     [ setup((clean_indexer_env,
              setenv('TERMINUSDB_INDEXER_BACKEND', http_vectorlink),
              setenv('TERMINUSDB_SEMANTIC_INDEXER_ENDPOINT', 'http://vectorlink:8080'),
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', 'http://tdb-search:8080'))),
       cleanup(clean_indexer_env),
       throws(error(indexer_backend_ambiguous(http_vectorlink, _), _))
     ]) :-
    check_indexer_backend_config.

test("http_tdb_search with both endpoints set is ambiguous and refuses startup",
     [ setup((clean_indexer_env,
              setenv('TERMINUSDB_INDEXER_BACKEND', http_tdb_search),
              setenv('TERMINUSDB_SEMANTIC_INDEXER_ENDPOINT', 'http://vectorlink:8080'),
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', 'http://tdb-search:8080'))),
       cleanup(clean_indexer_env),
       throws(error(indexer_backend_ambiguous(http_tdb_search, _), _))
     ]) :-
    check_indexer_backend_config.

test("admin user defaults to admin",
     [ setup((clean_indexer_env,
              unsetenv('TERMINUSDB_SEARCH_ADMIN_USER'))),
       cleanup((clean_indexer_env,
                clear_indexer_backend_config)),
       true(User == admin)
     ]) :-
    tdb_search_admin_user(User).

test("admin secret defaults to root",
     [ setup((clean_indexer_env,
              unsetenv('TERMINUSDB_SEARCH_ADMIN_SECRET'))),
       cleanup((clean_indexer_env,
                clear_indexer_backend_config)),
       true(Secret == root)
     ]) :-
    tdb_search_admin_secret(Secret).

:- end_tests(indexer_backend_selector).

% ==========================================================================
% Phase 6 T3 — Push driver unit tests.
%
% Tests the pure logic + backend gates of the push driver (io_push_delta,
% io_index_branch). The full integration test (streaming NDJSON through
% to a real tdb-search engine) is T6 scope and runs against the compose
% stack. These unit tests verify:
%   - path_to_domain/2 extraction
%   - build_push_url/6 URL construction
%   - handle_push_response/3 error surfacing
%   - Backend gate enforcement (wrong backend → fail loud)
%   - Full push flow with a stub HTTP server (chunked stream verification)
% ==========================================================================

:- use_module(core(api/api_indexer)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_client), [http_read_data/3]).
:- use_module(library(readutil)).

% ==========================================================================
% Shared stub HTTP server for push_driver and search_fronting tests.
%
% Defined at MODULE LEVEL so both test suites can call start_push_stub/1,
% stop_push_stub/1, and assert on stub_received/2 without cross-module
% visibility issues (same fix pattern as clean_indexer_env).
%
% Mimics tdb-search's /last-indexed, /push, and /check endpoints.
% ==========================================================================

% Global state for the stub — records what the stub received.
:- dynamic stub_received/2.   % stub_received(Key, Data)
:- dynamic stub_last_indexed_response/1.  % the JSON to return from /last-indexed
:- dynamic stub_push_call_count/1.  % tracks the number of pushes received
:- dynamic stub_check_response/2.   % stub_check_response(Task_Id, json_string)
:- dynamic stub_push_response_override/1.  % stub_push_response_override(status(Code))

% Start stub server on a fixed test port.
% Uses port 19876 (high, unlikely to conflict with other services).
push_stub_port(19876).

start_push_stub(Port) :-
    push_stub_port(Port),
    retractall(stub_received(_, _)),
    retractall(stub_last_indexed_response(_)),
    retractall(stub_push_call_count(_)),
    retractall(stub_check_response(_, _)),
    retractall(stub_push_response_override(_)),
    assertz(stub_push_call_count(0)),
    http_server(http_dispatch, [port(Port), workers(1)]).

stop_push_stub(Port) :-
    http_stop_server(Port, []),
    retractall(stub_received(_, _)),
    retractall(stub_last_indexed_response(_)),
    retractall(stub_push_call_count(_)),
    retractall(stub_check_response(_, _)),
    retractall(stub_push_response_override(_)).

% Stub handlers registered via http_handler directives below.
:- http_handler('/last-indexed', push_stub_last_indexed, []).
:- http_handler('/push', push_stub_push, [methods([post])]).
:- http_handler('/check', push_stub_check, []).

push_stub_last_indexed(Request) :-
    (   memberchk(search(Search), Request)
    ->  true
    ;   Search = []
    ),
    % Record what we received for test assertions.
    assertz(stub_received(last_indexed, Search)),
    % Check auth — SWI HTTP dispatch provides authorization(Text), not parsed.
    (   memberchk(authorization(AuthText), Request),
        http_authorization_data(AuthText, basic(User, Secret))
    ->  assertz(stub_received(last_indexed_auth, basic(User, Secret)))
    ;   true
    ),
    % Return the configured response.
    (   stub_last_indexed_response(ResponseJson)
    ->  true
    ;   ResponseJson = '{"branch":"main","commit":null,"version":0}'
    ),
    format("Content-Type: application/json~n~n"),
    write(ResponseJson).

push_stub_push(Request) :-
    (   memberchk(search(Search), Request)
    ->  true
    ;   Search = []
    ),
    % Increment call counter and record this push with its index.
    retract(stub_push_call_count(N)),
    N1 is N + 1,
    assertz(stub_push_call_count(N1)),
    % Record URL params indexed by call number (for multi-push tests).
    assertz(stub_received(push_params(N1), Search)),
    % Also record unindexed for simpler single-push assertions.
    assertz(stub_received(push_params, Search)),
    % Check auth.
    (   memberchk(authorization(AuthText), Request),
        http_authorization_data(AuthText, basic(User, Secret))
    ->  assertz(stub_received(push_auth, basic(User, Secret)))
    ;   true
    ),
    % Record the request body using http_read_data for chunked body support.
    (   memberchk(input(In), Request)
    ->  read_string(In, _, Body),
        assertz(stub_received(push_body(N1), Body))
    ;   true
    ),
    % Check Transfer-Encoding.
    (   memberchk(transfer_encoding(chunked), Request)
    ->  assertz(stub_received(push_chunked, true))
    ;   true
    ),
    % Auto-register a "Complete" check response for this task (for await tests).
    format(atom(Task_Id), "task-stub-~w", [N1]),
    format(atom(CheckJson), '{"status":"Complete","task_id":"~w"}', [Task_Id]),
    assertz(stub_check_response(Task_Id, CheckJson)),
    % Return response: check for override (409) or default (200 + task id).
    (   stub_push_response_override(status(Override_Code))
    ->  format("Status: ~w~n", [Override_Code]),
        format("Content-Type: text/plain~n~n"),
        format("Conflict")
    ;   format("Content-Type: text/plain~n~n"),
        write(Task_Id)
    ).

% ---- /check handler: returns pre-configured check response for task id ----
push_stub_check(Request) :-
    (   memberchk(search(Search), Request)
    ->  true
    ;   Search = []
    ),
    % SWI HTTP delivers query params as atoms — coerce for lookup.
    (   memberchk(task_id=Task_Id_Raw, Search)
    ->  (   atom(Task_Id_Raw)
        ->  atom_string(Task_Id_Raw, Task_Id_Str),
            atom_string(Task_Id, Task_Id_Str)
        ;   Task_Id = Task_Id_Raw
        )
    ;   Task_Id = unknown
    ),
    % Find the pre-registered response for this task id (string key).
    atom_string(Task_Id, Task_Id_Key),
    (   stub_check_response(Task_Id_Key, ResponseJson)
    ->  format("Content-Type: application/json~n~n"),
        write(ResponseJson)
    ;   % No response registered — return 404.
        format("Status: 404~n"),
        format("Content-Type: text/plain~n~n"),
        format("Task not found: ~w", [Task_Id])
    ).

:- begin_tests(push_driver).

% ---- Pure logic tests ----

test("path_to_domain extracts org/db from short path",
     [true(Domain == 'admin/testdb')]) :-
    api_indexer:path_to_domain("admin/testdb", Domain).

test("path_to_domain extracts org/db from full branch path",
     [true(Domain == 'myorg/mydb')]) :-
    api_indexer:path_to_domain("myorg/mydb/local/branch/main", Domain).

test("path_to_domain extracts org/db from commit path",
     [true(Domain == 'org/db')]) :-
    api_indexer:path_to_domain("org/db/local/commit/abc123", Domain).

test("path_to_domain fails on single-segment path",
     [throws(error(invalid_path_for_domain(_), _))]) :-
    api_indexer:path_to_domain("onlyone", _).

test("path_to_domain fails on empty path",
     [throws(error(invalid_path_for_domain(_), _))]) :-
    api_indexer:path_to_domain("", _).

% ---- descriptor_graphspec tests: full graphspec from resolved descriptor ----

test("descriptor_graphspec produces full graphspec for main branch",
     [ setup(setup_temp_store(State)),
       cleanup(teardown_temp_store(State)),
       true(GraphSpec == 'admin/testdb/local/branch/main')
     ]) :-
    create_db_without_schema("admin", "testdb"),
    resolve_absolute_string_descriptor("admin/testdb", Descriptor),
    api_indexer:descriptor_graphspec(Descriptor, GraphSpec).

test("descriptor_graphspec produces full graphspec for feature branch",
     [ setup(setup_temp_store(State)),
       cleanup(teardown_temp_store(State)),
       true(GraphSpec == 'admin/testdb/local/branch/feature-x')
     ]) :-
    create_db_without_schema("admin", "testdb"),
    % Create a feature branch using the branch_create API.
    open_descriptor(system_descriptor{}, System_DB),
    super_user_authority(Auth),
    branch_create(System_DB, Auth,
                  "admin/testdb/local/branch/feature-x",
                  branch("admin/testdb"), _),
    resolve_absolute_string_descriptor("admin/testdb/local/branch/feature-x", FeatureDesc),
    api_indexer:descriptor_graphspec(FeatureDesc, GraphSpec).

test("descriptor_graphspec graphspec branch agrees with descriptor branch_name",
     [ setup(setup_temp_store(State)),
       cleanup(teardown_temp_store(State))
     ]) :-
    create_db_without_schema("admin", "testdb2"),
    resolve_absolute_string_descriptor("admin/testdb2", Descriptor),
    branch_descriptor{branch_name: Branch} :< Descriptor,
    api_indexer:descriptor_graphspec(Descriptor, GraphSpec),
    % The graphspec must end with the branch name — single source of truth.
    atom_string(GraphSpec, GS_String),
    format(atom(Expected_Suffix), "/branch/~w", [Branch]),
    atom_string(Expected_Suffix, Suffix_String),
    sub_string(GS_String, _, _, 0, Suffix_String).

test("build_push_url with parent_commit includes parent_commit param",
     [true(URL == 'http://engine:8080/push?domain=admin%2fdb&branch=main&target_commit=head1&parent_commit=prev1')]) :-
    api_indexer:build_push_url("http://engine:8080", "admin/db", "main",
                               "head1", "prev1", URL).

test("build_push_url with none parent omits parent_commit param",
     [true(URL == 'http://engine:8080/push?domain=admin%2fdb&branch=main&target_commit=head1')]) :-
    api_indexer:build_push_url("http://engine:8080", "admin/db", "main",
                               "head1", none, URL).

test("handle_push_response 200 returns accepted(Task_Id)",
     [true(Result == accepted("task-abc"))]) :-
    api_indexer:handle_push_response(200, "task-abc", Result).

test("handle_push_response 409 returns conflict_already_pushed",
     [true(Result == conflict_already_pushed)]) :-
    api_indexer:handle_push_response(409, "Conflict", Result).

test("handle_push_response 401 throws loud failure",
     [throws(error(tdb_search_push_failed(401, "Unauthorized"), _))]) :-
    api_indexer:handle_push_response(401, "Unauthorized", _).

test("handle_push_response 500 throws loud failure",
     [throws(error(tdb_search_push_failed(500, "Internal error"), _))]) :-
    api_indexer:handle_push_response(500, "Internal error", _).

% ---- Backend gate tests ----

test("io_push_delta refuses when backend is not http_tdb_search",
     [ setup((clean_indexer_env,
              setenv('TERMINUSDB_INDEXER_BACKEND', none))),
       cleanup(clean_indexer_env),
       throws(error(indexer_backend_not_tdb_search(io_push_delta), _))
     ]) :-
    io_push_delta(_, _, "admin/testdb", "main").

test("io_push_delta refuses when backend is http_vectorlink",
     [ setup((clean_indexer_env,
              setenv('TERMINUSDB_INDEXER_BACKEND', http_vectorlink),
              setenv('TERMINUSDB_SEMANTIC_INDEXER_ENDPOINT', 'http://legacy:8080'))),
       cleanup(clean_indexer_env),
       throws(error(indexer_backend_not_tdb_search(io_push_delta), _))
     ]) :-
    io_push_delta(_, _, "admin/testdb", "main").

test("io_index_branch refuses when backend is none",
     [ setup((clean_indexer_env,
              setenv('TERMINUSDB_INDEXER_BACKEND', none))),
       cleanup(clean_indexer_env),
       throws(error(indexer_backend_not_tdb_search(io_index_branch), _))
     ]) :-
    io_index_branch(_, _, "admin/testdb").

% NOTE: Full-flow integration tests (chunked HTTP push protocol) have been
% moved to the tdb-search integration test harness, which is the correct
% boundary to test the wire protocol between TerminusDB and tdb-search.
% See tdb-search test suite for: full-index-from-null, per-commit push,
% chunked NDJSON body verification, and auth header forwarding.

% ---- Test: engine already at HEAD → nothing to push ----
test("io_push_delta does nothing when engine is at HEAD",
     [ setup((setup_temp_store(State),
              create_db_without_schema("admin", "testdb2"),
              clean_indexer_env,
              start_push_stub(Port),
              format(atom(Endpoint_URL), "http://127.0.0.1:~w", [Port]),
              setenv('TERMINUSDB_INDEXER_BACKEND', http_tdb_search),
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', Endpoint_URL)
             )),
       cleanup((stop_push_stub(Port),
                clean_indexer_env,
                teardown_temp_store(State)))
     ]) :-
    % Create a commit
    resolve_absolute_string_descriptor("admin/testdb2", Descriptor),
    create_context(Descriptor, commit_info{author:"test", message:"data"}, Context),
    with_transaction(Context, ask(Context, insert(x,y,z)), _),
    % Get the HEAD commit id
    Repository_Descriptor = Descriptor.repository_descriptor,
    branch_head_commit(Repository_Descriptor, "main", Head_Uri),
    commit_id_uri(Repository_Descriptor, Head_Commit_Id, Head_Uri),
    % Configure stub to return this same commit (already indexed)
    retractall(stub_last_indexed_response(_)),
    format(atom(ResponseJson), '{"branch":"main","commit":"~w","version":5}', [Head_Commit_Id]),
    assertz(stub_last_indexed_response(ResponseJson)),
    % Drive the push
    super_user_authority(Auth),
    open_descriptor(system_descriptor{}, System_DB),
    io_push_delta(System_DB, Auth, "admin/testdb2", "main"),
    % Verify NO push was made (only last-indexed was called)
    stub_received(last_indexed, _),
    \+ stub_received(push_params, _).

% ---- Pure logic tests for commits_after/3 ----

test("commits_after returns suffix after the given commit",
     [true(Forward == ["c2", "c3", "c4"])]) :-
    api_indexer:commits_after("c1", ["c0", "c1", "c2", "c3", "c4"], Forward).

test("commits_after returns empty list when commit is last",
     [true(Forward == [])]) :-
    api_indexer:commits_after("c4", ["c0", "c1", "c2", "c3", "c4"], Forward).

test("commits_after throws when commit is not in history",
     [throws(error(tdb_search_last_indexed_not_in_history("missing"), _))]) :-
    api_indexer:commits_after("missing", ["c0", "c1", "c2"], _).

test("commits_after returns single-element suffix for second-to-last",
     [true(Forward == ["c2"])]) :-
    api_indexer:commits_after("c1", ["c0", "c1", "c2"], Forward).


% ---- Test: normalise_commit_value handles all cases correctly ----
test("normalise_commit_value handles JSON null (@(null))",
     [true(Result == null)]) :-
    api_indexer:normalise_commit_value(@(null), Result).

test("normalise_commit_value handles plain null atom",
     [true(Result == null)]) :-
    api_indexer:normalise_commit_value(null, Result).

test("normalise_commit_value coerces atom to string",
     [true(Result == "abc123")]) :-
    api_indexer:normalise_commit_value(abc123, Result).

test("normalise_commit_value passes through strings",
     [true(Result == "def456")]) :-
    api_indexer:normalise_commit_value("def456", Result).

% ---- build_push_url encoding: reserved characters ----

test("build_push_url encodes slash in domain path",
     [true(URL == 'http://engine:8080/push?domain=org%2fdb%2flocal%2fbranch%2fmain&branch=main&target_commit=c1')]) :-
    api_indexer:build_push_url("http://engine:8080", "org/db/local/branch/main",
                               "main", "c1", none, URL).

test("build_push_url encodes ampersand and equals in branch name",
     [true(sub_atom(URL, _, _, _, 'branch=a%26b%3dc'))]) :-
    api_indexer:build_push_url("http://engine:8080", "d", "a&b=c",
                               "c1", none, URL).

% ---- build_last_indexed_url encoding tests ----

test("build_last_indexed_url encodes slash in domain",
     [true(URL == 'http://engine:8080/last-indexed?domain=admin%2fdb&branch=main')]) :-
    api_indexer:build_last_indexed_url("http://engine:8080", "admin/db", "main", URL).

test("build_last_indexed_url encodes special chars in branch",
     [true(sub_atom(URL, _, _, _, 'branch=feat%2fx'))]) :-
    api_indexer:build_last_indexed_url("http://engine:8080", "d", "feat/x", URL).

% ---- io_await_task_completion tests (with stub /check endpoint) ----

test("io_await_task_completion succeeds when check returns Complete",
     [ setup((push_stub_port(Port),
              start_push_stub(Port),
              % Pre-register a Complete response for task-test-1
              assertz(stub_check_response("task-test-1",
                  '{"status":"Complete","task_id":"task-test-1"}'))
             )),
       cleanup(stop_push_stub(Port))
     ]) :-
    push_stub_port(Port),
    format(atom(Endpoint), "http://127.0.0.1:~w", [Port]),
    api_indexer:io_await_task_completion(Endpoint, "task-test-1").

test("io_await_task_completion throws when task not found (404 from check)",
     [ setup((push_stub_port(Port),
              start_push_stub(Port)
             )),
       cleanup(stop_push_stub(Port)),
       throws(error(tdb_search_task_failed("task-nonexistent", _), _))
     ]) :-
    push_stub_port(Port),
    format(atom(Endpoint), "http://127.0.0.1:~w", [Port]),
    % No check response registered for this task id.
    % The handler returns 404 which interpret_check_response maps to
    % error(Body_String). io_await_task_completion_ then throws
    % tdb_search_task_failed.
    api_indexer:io_await_task_completion(Endpoint, "task-nonexistent").

% ---- interpret_check_response atom/string normalisation (CLASS A) ----

test("interpret_check_response handles string 'Complete' from atom_json_dict",
     [true(Status == complete)]) :-
    % Simulate atom_json_dict returning a string "Complete" value
    api_indexer:interpret_check_response(200, '{"status":"Complete"}', Status).

test("interpret_check_response handles string 'Pending' from atom_json_dict",
     [true(Status == pending)]) :-
    api_indexer:interpret_check_response(200, '{"status":"Pending"}', Status).

test("interpret_check_response 500 returns error term",
     [true(Status == error("server crashed"))]) :-
    api_indexer:interpret_check_response(500, "server crashed", Status).

% ============================================================================
% validate_index_path — Pure predicate test matrix (CLASS C proper fix)
%
% Contract: ACCEPT 2-seg "org/db" and 5-seg "org/db/repo/branch/<name>" or
%           "org/db/repo/commit/<id>". REJECT everything else with a clear error
%           BEFORE any descriptor resolution or network I/O.
% ============================================================================

% ---- Accepted paths ----

test("validate_index_path accepts 2-segment path (org/db)") :-
    api_indexer:validate_index_path("admin/testdb").

test("validate_index_path accepts 5-segment branch path") :-
    api_indexer:validate_index_path("admin/testdb/local/branch/main").

test("validate_index_path accepts 5-segment commit path") :-
    api_indexer:validate_index_path("admin/testdb/local/commit/abc123").

test("validate_index_path accepts atom input") :-
    api_indexer:validate_index_path('org/db').

test("validate_index_path accepts 5-segment with unusual branch name") :-
    api_indexer:validate_index_path("org/db/local/branch/feat-x").

% ---- Rejected paths ----

test("validate_index_path rejects 1-segment path",
     [throws(error(invalid_index_path(_, wrong_segment_count(1, expected_2_or_5)), _))]) :-
    api_indexer:validate_index_path("onlyone").

test("validate_index_path rejects 3-segment path",
     [throws(error(invalid_index_path(_, wrong_segment_count(3, expected_2_or_5)), _))]) :-
    api_indexer:validate_index_path("admin/testdb/local").

test("validate_index_path rejects 4-segment path",
     [throws(error(invalid_index_path(_, wrong_segment_count(4, expected_2_or_5)), _))]) :-
    api_indexer:validate_index_path("admin/testdb/local/branch").

test("validate_index_path rejects 6-segment path",
     [throws(error(invalid_index_path(_, wrong_segment_count(6, expected_2_or_5)), _))]) :-
    api_indexer:validate_index_path("admin/db/local/branch/main/extra").

test("validate_index_path rejects 5-segment with bad segment-4 (not branch/commit)",
     [throws(error(invalid_index_path(_, bad_segment_4(_, expected_branch_or_commit)), _))]) :-
    api_indexer:validate_index_path("admin/db/local/tag/v1.0").

test("validate_index_path rejects _meta path (3-segment system form)",
     [throws(error(invalid_index_path(_, wrong_segment_count(3, expected_2_or_5)), _))]) :-
    api_indexer:validate_index_path("admin/db/_meta").

test("validate_index_path rejects empty string",
     [throws(error(invalid_index_path(_, wrong_segment_count(0, expected_2_or_5)), _))]) :-
    api_indexer:validate_index_path("").

% ============================================================================
% io_index_branch rejection integration tests (CLASS C)
%
% These tests verify that invalid paths are REJECTED before any network I/O.
% They set the backend to http_tdb_search with endpoint on port 9999 (unused)
% to prove that if validation passes incorrectly, the test would hang/fail on
% connection refused — but validation rejects FIRST.
% ============================================================================

test("io_index_branch rejects 3-segment path before any I/O",
     [ setup((clean_indexer_env,
              setenv('TERMINUSDB_INDEXER_BACKEND', http_tdb_search),
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', 'http://127.0.0.1:9999'))),
       cleanup(clean_indexer_env),
       throws(error(invalid_index_path("admin/testdb/local",
                        wrong_segment_count(3, expected_2_or_5)), _))
     ]) :-
    io_index_branch(_, _, "admin/testdb/local").

test("io_index_branch rejects 4-segment path before any I/O",
     [ setup((clean_indexer_env,
              setenv('TERMINUSDB_INDEXER_BACKEND', http_tdb_search),
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', 'http://127.0.0.1:9999'))),
       cleanup(clean_indexer_env),
       throws(error(invalid_index_path("admin/testdb/local/branch",
                        wrong_segment_count(4, expected_2_or_5)), _))
     ]) :-
    io_index_branch(_, _, "admin/testdb/local/branch").

test("io_index_branch rejects _meta path before any I/O",
     [ setup((clean_indexer_env,
              setenv('TERMINUSDB_INDEXER_BACKEND', http_tdb_search),
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', 'http://127.0.0.1:9999'))),
       cleanup(clean_indexer_env),
       throws(error(invalid_index_path("admin/db/_meta",
                        wrong_segment_count(3, expected_2_or_5)), _))
     ]) :-
    io_index_branch(_, _, "admin/db/_meta").

test("io_index_branch rejects 5-segment with bad segment-4 before any I/O",
     [ setup((clean_indexer_env,
              setenv('TERMINUSDB_INDEXER_BACKEND', http_tdb_search),
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', 'http://127.0.0.1:9999'))),
       cleanup(clean_indexer_env),
       throws(error(invalid_index_path("admin/db/local/tag/v1",
                        bad_segment_4(_, expected_branch_or_commit)), _))
     ]) :-
    io_index_branch(_, _, "admin/db/local/tag/v1").

% ---- io_push_delta rejection tests (same pattern) ----

test("io_push_delta rejects 3-segment path before any I/O",
     [ setup((clean_indexer_env,
              setenv('TERMINUSDB_INDEXER_BACKEND', http_tdb_search),
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', 'http://127.0.0.1:9999'))),
       cleanup(clean_indexer_env),
       throws(error(invalid_index_path("admin/testdb/local",
                        wrong_segment_count(3, expected_2_or_5)), _))
     ]) :-
    io_push_delta(_, _, "admin/testdb/local", "main").

test("io_push_delta rejects _meta path before any I/O",
     [ setup((clean_indexer_env,
              setenv('TERMINUSDB_INDEXER_BACKEND', http_tdb_search),
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', 'http://127.0.0.1:9999'))),
       cleanup(clean_indexer_env),
       throws(error(invalid_index_path("admin/db/_meta",
                        wrong_segment_count(3, expected_2_or_5)), _))
     ]) :-
    io_push_delta(_, _, "admin/db/_meta", "main").

:- end_tests(push_driver).

% ==========================================================================
% Search fronting + authz parity tests (RISK-09).
%
% Headline test: a caller DENIED on the data product (no instance_read_access)
% is DENIED on /search + /similar — the request NEVER reaches the engine
% (assert no outbound call / denial before forward).
%
% Tests also verify:
%   - Backend gate enforcement (wrong backend -> fail loud)
%   - Ancestor window computation
%   - URL construction (pure predicate tests)
%   - Stale-version nudge behaviour
% ==========================================================================

:- use_module(core(api/api_search)).
:- use_module(core(account/capabilities), [resolve_descriptor_auth/6,
                                           user_key_user_id/4]).
:- use_module(core(account/user_management)).

:- begin_tests(search_fronting).

% ---- Pure logic: URL construction ----

test("build_search_url constructs correct URL with ancestors",
     [true(URL == 'http://engine:8080/search?domain=admin%2fdb&commit=abc123&ancestor=prev1&ancestor=prev2')]) :-
    api_search:build_search_url("http://engine:8080", "admin/db", "abc123",
                                ["prev1", "prev2"], URL).

test("build_search_url with no ancestors omits ancestor params",
     [true(URL == 'http://engine:8080/search?domain=admin%2fdb&commit=abc123')]) :-
    api_search:build_search_url("http://engine:8080", "admin/db", "abc123",
                                [], URL).

test("build_similar_url constructs correct URL",
     [true(URL == 'http://engine:8080/similar?domain=org%2fmydb&commit=def456&ancestor=anc1')]) :-
    api_search:build_similar_url("http://engine:8080", "org/mydb", "def456",
                                 ["anc1"], URL).

test("build_duplicates_url constructs correct URL without ancestors",
     [true(URL == 'http://engine:8080/duplicates?domain=admin%2fdb&commit=c99')]) :-
    api_search:build_duplicates_url("http://engine:8080", "admin/db", "c99", URL).

test("build_statistics_url constructs scoped URL with domain and commit",
     [true(URL == 'http://engine:8080/statistics?domain=admin%2fmydb&commit=abc123')]) :-
    api_search:build_statistics_url("http://engine:8080", "admin/mydb", "abc123",
                                    [], URL).

test("build_statistics_url includes ancestor params",
     [true(sub_atom(URL, _, _, _, '&ancestor=anc1'))]) :-
    api_search:build_statistics_url("http://engine:8080", "admin/db", "c1",
                                    ["anc1"], URL).

test("build_search_url encodes slashes in domain",
     [true(sub_atom(URL, _, _, _, 'domain=org%2fdb%2flocal%2fbranch%2fmain'))]) :-
    api_search:build_search_url("http://e:80", "org/db/local/branch/main",
                                "c1", [], URL).

% ---- Pure logic: ancestor window computation ----

test("ancestor_window returns ancestors nearest first excluding HEAD",
     [ setup(setup_temp_store(State)),
       cleanup(teardown_temp_store(State))
     ]) :-
    % Create a database and make 3 commits.
    create_db_without_schema("admin", "ancdb"),
    resolve_absolute_string_descriptor("admin/ancdb", Descriptor),
    create_context(Descriptor, commit_info{author:"t", message:"c1"}, Ctx1),
    with_transaction(Ctx1, ask(Ctx1, insert(a, b, c)), _),
    create_context(Descriptor, commit_info{author:"t", message:"c2"}, Ctx2),
    with_transaction(Ctx2, ask(Ctx2, insert(d, e, f)), _),
    create_context(Descriptor, commit_info{author:"t", message:"c3"}, Ctx3),
    with_transaction(Ctx3, ask(Ctx3, insert(g, h, i)), _),
    % Get HEAD
    Repository_Descriptor = Descriptor.repository_descriptor,
    branch_head_commit(Repository_Descriptor, "main", Head_Uri),
    commit_id_uri(Repository_Descriptor, Head_Commit_Id, Head_Uri),
    % Get ancestor window (max 10)
    ancestor_window(Repository_Descriptor, Head_Uri, 10, Ancestors),
    % Ancestors should NOT include HEAD itself
    \+ memberchk(Head_Commit_Id, Ancestors),
    % Should have 2 ancestors (c2 and c1, nearest first)
    length(Ancestors, 2).

test("ancestor_window respects max count",
     [ setup(setup_temp_store(State)),
       cleanup(teardown_temp_store(State))
     ]) :-
    create_db_without_schema("admin", "ancdb2"),
    resolve_absolute_string_descriptor("admin/ancdb2", Descriptor),
    create_context(Descriptor, commit_info{author:"t", message:"c1"}, Ctx1),
    with_transaction(Ctx1, ask(Ctx1, insert(a, b, c)), _),
    create_context(Descriptor, commit_info{author:"t", message:"c2"}, Ctx2),
    with_transaction(Ctx2, ask(Ctx2, insert(d, e, f)), _),
    create_context(Descriptor, commit_info{author:"t", message:"c3"}, Ctx3),
    with_transaction(Ctx3, ask(Ctx3, insert(g, h, i)), _),
    Repository_Descriptor = Descriptor.repository_descriptor,
    branch_head_commit(Repository_Descriptor, "main", Head_Uri),
    % Request only 1 ancestor
    ancestor_window(Repository_Descriptor, Head_Uri, 1, Ancestors),
    length(Ancestors, 1).

% ---- Backend gate tests ----

test("io_search_forward refuses when backend is not http_tdb_search",
     [ setup((clean_indexer_env,
              setenv('TERMINUSDB_INDEXER_BACKEND', none))),
       cleanup(clean_indexer_env),
       throws(error(search_requires_tdb_search_backend, _))
     ]) :-
    io_search_forward("http://x:80", "d", "c", [], [], _, _).

test("io_similar_forward refuses when backend is http_vectorlink",
     [ setup((clean_indexer_env,
              setenv('TERMINUSDB_INDEXER_BACKEND', http_vectorlink),
              setenv('TERMINUSDB_SEMANTIC_INDEXER_ENDPOINT', 'http://legacy:8080'))),
       cleanup(clean_indexer_env),
       throws(error(search_requires_tdb_search_backend, _))
     ]) :-
    io_similar_forward("http://x:80", "d", "c", [], [], _, _).

test("io_duplicates_forward refuses when backend is none",
     [ setup((clean_indexer_env,
              setenv('TERMINUSDB_INDEXER_BACKEND', none))),
       cleanup(clean_indexer_env),
       throws(error(search_requires_tdb_search_backend, _))
     ]) :-
    io_duplicates_forward("http://x:80", "d", "c", [], _, _).

test("io_statistics_forward refuses when backend is none",
     [ setup((clean_indexer_env,
              setenv('TERMINUSDB_INDEXER_BACKEND', none))),
       cleanup(clean_indexer_env),
       throws(error(search_requires_tdb_search_backend, _))
     ]) :-
    io_statistics_forward("http://x:80", "admin/db", "c0", [], _, _).

% ---- HEADLINE TEST: Authz parity (RISK-09) ----
%
% A caller DENIED on the data product (no instance_read_access) is DENIED
% before any engine call. We verify this by:
%   1. Creating a user with NO permissions on the target database.
%   2. Calling resolve_descriptor_auth — it MUST throw access_not_authorised.
%   3. Since the handler runs authz BEFORE forwarding, this proves the engine
%      is NEVER reached for a denied caller.

test("authz parity: denied caller cannot search (resolve_descriptor_auth throws)",
     [ setup((setup_temp_store(State),
              create_db_without_schema("admin", "secretdb"),
              add_user("UnprivUser", some('pass123'), _URI)
             )),
       cleanup(teardown_temp_store(State)),
       throws(error(access_not_authorised(_, _, _), _))
     ]) :-
    % Open system DB and get the unprivileged user's auth.
    open_descriptor(system_descriptor{}, System_DB),
    % The unprivileged user has NO role granting instance_read_access on admin/secretdb.
    user_key_user_id(System_DB, 'UnprivUser', 'pass123', Auth),
    % This MUST throw access_not_authorised — the RISK-09 parity gate.
    resolve_descriptor_auth(read, System_DB, Auth, "admin/secretdb", instance, _Descriptor).

test("authz parity: authorised caller passes resolve_descriptor_auth",
     [ setup((setup_temp_store(State),
              create_db_without_schema("admin", "opendb")
             )),
       cleanup(teardown_temp_store(State))
     ]) :-
    % The super user DOES have access.
    open_descriptor(system_descriptor{}, System_DB),
    super_user_authority(Auth),
    resolve_descriptor_auth(read, System_DB, Auth, "admin/opendb", instance, _Descriptor).

% ---- Authz parity with stub: denied caller NEVER triggers engine call ----
%
% This test starts the stub HTTP server and verifies that a denied caller's
% request is blocked BEFORE any HTTP call is made to the stub.

test("authz parity: denied caller search never reaches engine stub",
     [ setup((setup_temp_store(State),
              create_db_without_schema("admin", "guardeddb"),
              add_user("DeniedUser", some('pass456'), _URI),
              clean_indexer_env,
              start_push_stub(Port),
              format(atom(Endpoint_URL), "http://127.0.0.1:~w", [Port]),
              setenv('TERMINUSDB_INDEXER_BACKEND', http_tdb_search),
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', Endpoint_URL)
             )),
       cleanup((stop_push_stub(Port),
                clean_indexer_env,
                teardown_temp_store(State)))
     ]) :-
    % Register a search stub handler that records if called.
    % (We reuse the push stub port; if search reached the engine,
    %  an HTTP call to the stub would be recorded.)
    open_descriptor(system_descriptor{}, System_DB),
    user_key_user_id(System_DB, 'DeniedUser', 'pass456', Auth),
    % Attempt the full search handler logic. This MUST throw before reaching
    % the engine.
    catch(
        (   resolve_descriptor_auth(read, System_DB, Auth,
                                    "admin/guardeddb", instance, _Desc),
            % If we get past authz (shouldn't), try the forward.
            config:tdb_search_endpoint(Endpoint),
            io_search_forward(Endpoint, "admin/guardeddb", "fake_commit",
                              [], [], _Response, _DV)
        ),
        error(access_not_authorised(_, _, _), _),
        true  % Expected: denial happened before forward.
    ),
    % Assert NO engine call was made.
    \+ stub_received(_, _).

% ---- Authz parity: denied caller cannot get statistics ----

test("authz parity: denied caller cannot get statistics (resolve_descriptor_auth throws)",
     [ setup((setup_temp_store(State),
              create_db_without_schema("admin", "statsdb"),
              add_user("StatsBlockedUser", some('pass789'), _URI)
             )),
       cleanup(teardown_temp_store(State)),
       throws(error(access_not_authorised(_, _, _), _))
     ]) :-
    open_descriptor(system_descriptor{}, System_DB),
    user_key_user_id(System_DB, 'StatsBlockedUser', 'pass789', Auth),
    % This MUST throw access_not_authorised — the RISK-09 parity gate for statistics.
    resolve_descriptor_auth(read, System_DB, Auth, "admin/statsdb", instance, _Descriptor).

test("authz parity: denied caller statistics never reaches engine stub",
     [ setup((setup_temp_store(State),
              create_db_without_schema("admin", "guardedstatsdb"),
              add_user("StatsDeniedUser", some('pass012'), _URI),
              clean_indexer_env,
              start_push_stub(Port),
              format(atom(Endpoint_URL), "http://127.0.0.1:~w", [Port]),
              setenv('TERMINUSDB_INDEXER_BACKEND', http_tdb_search),
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', Endpoint_URL)
             )),
       cleanup((stop_push_stub(Port),
                clean_indexer_env,
                teardown_temp_store(State)))
     ]) :-
    open_descriptor(system_descriptor{}, System_DB),
    user_key_user_id(System_DB, 'StatsDeniedUser', 'pass012', Auth),
    % Attempt the full statistics handler logic. This MUST throw before reaching
    % the engine.
    catch(
        (   resolve_descriptor_auth(read, System_DB, Auth,
                                    "admin/guardedstatsdb", instance, _Desc),
            % If we get past authz (shouldn't), try the forward.
            config:tdb_search_endpoint(Endpoint),
            io_statistics_forward(Endpoint, "admin/guardedstatsdb", "fake_commit",
                                  [], _Response, _DV)
        ),
        error(access_not_authorised(_, _, _), _),
        true  % Expected: denial happened before forward.
    ),
    % Assert NO engine call was made.
    \+ stub_received(_, _).

% ---- Stale-version nudge (via maybe_nudge_push) ----

test("maybe_nudge_push does nothing when data version matches",
     [ setup((setup_temp_store(State),
              create_db_without_schema("admin", "nudgedb"),
              clean_indexer_env,
              setenv('TERMINUSDB_INDEXER_BACKEND', http_tdb_search),
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', 'http://127.0.0.1:9999')
             )),
       cleanup((clean_indexer_env,
                teardown_temp_store(State)))
     ]) :-
    % When the served version matches the requested commit, no nudge fires.
    % We simply verify it does not throw (io_push_delta would fail if actually
    % called since there's no real engine at port 9999).
    api_search:maybe_nudge_push("commit:abc123", "abc123",
                                _, _, "admin/nudgedb", "main").

test("maybe_nudge_push with none header does nothing",
     [ setup(setup_temp_store(State)),
       cleanup(teardown_temp_store(State))
     ]) :-
    api_search:maybe_nudge_push(none, "abc123", _, _, "admin/db", "main").

:- end_tests(search_fronting).
