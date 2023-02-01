:- module(config,[
              terminusdb_version/1,
              bootstrap_config_files/0,
              server/1,
              server_name/1,
              server_port/1,
              worker_amount/1,
              max_transaction_retries/1,
              db_path/1,
              jwt_jwks_endpoint/1,
              jwt_enabled/0,
              jwt_subject_claim_name/1,
              registry_path/1,
              tmp_path/1,
              server_worker_options/1,
              http_options/1,
              ignore_ref_and_repo_schema/0,
              file_upload_storage_path/1,
              log_level/1,
              set_log_level/1,
              clear_log_level/0,
              log_format/1,
              set_log_format/1,
              clear_log_format/0,
              insecure_user_header_key/1,
              check_all_env_vars/0,
              is_enterprise/0,
              check_insecure_user_header_enabled/1,
              clear_check_insecure_user_header_enabled/0,
              clear_insecure_user_header_key/0,
              pinned_databases/1,
              plugin_path/1,
              dashboard_enabled/0,
              parallelize_enabled/0,
              grpc_label_endpoint/1,
              crypto_password_cost/1
          ]).

:- use_module(library(pcre)).

:- use_module(core(util)).
:- use_module(core(query)).

:- use_module(library(apply)).
:- use_module(library(yall)).

/* [[[cog import cog; cog.out(f"terminusdb_version('{CURRENT_REPO_VERSION}').") ]]] */
terminusdb_version('11.0.0').
/* [[[end]]] */

bootstrap_config_files :-
    initialize_system_ssl_certs.

initialize_system_ssl_certs :-
    (   getenv('TERMINUSDB_SYSTEM_SSL_CERTS', Value)
    ->  set_prolog_flag(system_cacert_filename, Value)
    ;   true).

server_protocol(Value) :-
    Value = http.

:- table server_name/1 as shared.
server_name(Value) :-
    (   getenv('TERMINUSDB_SERVER_NAME', Value)
    ->  true
    ;   random_string(Value)).

server_port(Value) :-
    getenv_default_number('TERMINUSDB_SERVER_PORT', 6363, Value).

worker_amount(Value) :-
    current_prolog_flag(cpu_count,Integer),
    getenv_default_number('TERMINUSDB_SERVER_WORKERS', Integer, Value).

:- table max_transaction_retries/1 as shared.
max_transaction_retries(Value) :-
    (   getenv('TERMINUSDB_SERVER_MAX_TRANSACTION_RETRIES', Atom_Value)
    ->  atom_number(Atom_Value, Value)
    ;   worker_amount(Num_Workers),
        Value is Num_Workers * 2).

% This is defined only for testing. Use db_path/1 since it is tabled.
default_database_path(Path) :-
    getenv_default('TERMINUSDB_SERVER_DB_PATH', './storage/db', Value),
    absolute_file_name(Value, Path).

/**
 * db_path(-Path) is det.
 *
 * Database storage path.
 *
 */
:- table db_path/1 as shared.
db_path(Path) :-
    default_database_path(Path).

dashboard_enabled :-
    getenv_default('TERMINUSDB_ENABLE_DASHBOARD', true, Value),
    Value = true.

plugin_path(Path) :-
    getenv_default('TERMINUSDB_PLUGINS_PATH', './storage/plugins', Value),
    absolute_file_name(Value, Path).

jwt_enabled_env_var :-
    getenv_default('TERMINUSDB_JWT_ENABLED', false, true).

% jwt_enabled is used for conditional compilation of jwt_io, but want to use the
% value passed to TERMINUSDB_JWT_ENABLED at compile time, which may be different
% from runtime. Therefore, we save the TERMINUSDB_JWT_ENABLED value for runtime
% again using conditional compilation here.
:- if(jwt_enabled_env_var).
jwt_enabled :-
    true.
:- else.
jwt_enabled :-
    false.
:- endif.

jwt_jwks_endpoint(Endpoint) :-
    getenv('TERMINUSDB_SERVER_JWKS_ENDPOINT', Value),
    % Ignore an empty value in the environment variable.
    (   Value = ''
    ->  false
    ;   Endpoint = Value).

jwt_subject_claim_name(Name) :-
    getenv_default('TERMINUSDB_JWT_AGENT_NAME_PROPERTY', 'preferred_username', Name).

registry_path(Value) :-
    once(expand_file_search_path(plugins('registry.pl'), Path)),
    getenv_default('TERMINUSDB_SERVER_REGISTRY_PATH', Path, Value).

tmp_path(Value) :-
    getenv_default('TERMINUSDB_SERVER_TMP_PATH', '/tmp', Value).

:- table file_upload_storage_path/1 as shared.
file_upload_storage_path(Path) :-
    getenv('TERMINUSDB_FILE_STORAGE_PATH', Path).

server(Server) :-
    server_protocol(Protocol),
    server_port(Port),
    atomic_list_concat([Protocol,'://localhost',':',Port],Server).

server_worker_options([]).

http_options([]).

:- table ignore_ref_and_repo_schema/0 as shared.
ignore_ref_and_repo_schema :-
    getenv('TERMINUSDB_IGNORE_REF_AND_REPO_SCHEMA', true).

:- set_prolog_flag(stack_limit, 8_589_934_592).

% Turn off mavis
:- set_prolog_flag(optimise, true).

:- dynamic log_level_override/1.

:- table log_level_env/1 as shared.
log_level_env(Log_Level) :-
    getenv('TERMINUSDB_LOG_LEVEL', Log_Level_Lower),
    upcase_atom(Log_Level_Lower, Log_Level),
    memberchk(Log_Level, ['ERROR', 'WARNING', 'NOTICE', 'INFO', 'DEBUG']),
    !.
log_level_env('INFO').

log_level(Log_Level) :-
    log_level_override(Found_Log_Level),
    !,
    Log_Level = Found_Log_Level.
log_level(Log_Level) :-
    log_level_env(Found_Log_Level),
    Log_Level = Found_Log_Level.

set_log_level(Log_Level) :-
    memberchk(Log_Level, ['ERROR', 'WARNING', 'NOTICE', 'INFO', 'DEBUG']),
    clear_log_level,
    asserta(log_level_override(Log_Level)).

clear_log_level :-
    retractall(log_level_override(_)).

:- table log_format_env/1 as shared.
log_format_env(Log_Format) :-
    getenv('TERMINUSDB_LOG_FORMAT', Log_Format),
    memberchk(Log_Format, [text, json]),
    !.
log_format_env(text).

:- dynamic log_format_override/1.

log_format(Log_Format) :-
    log_format_override(Found_Log_Format),
    !,
    Log_Format = Found_Log_Format.
log_format(Log_Format) :-
    log_format_env(Found_Log_Format),
    !,
    Log_Format = Found_Log_Format.

set_log_format(Log_Format) :-
    memberchk(Log_Format, ['text', 'json']),
    clear_log_format,
    asserta(log_format_override(Log_Format)).

clear_log_format :-
    retractall(log_format_override(_)).

:- dynamic check_insecure_user_header_enabled_/1.

/* Retract the dynamic predicate for testing. */
clear_check_insecure_user_header_enabled :-
    retractall(check_insecure_user_header_enabled_(_)).

/**
 * check_insecure_user_header_enabled(-Enabled) is semidet.
 *
 * Look up the env var for enabling the insecure user header.
 */
check_insecure_user_header_enabled(Enabled) :-
    check_insecure_user_header_enabled_(Enabled),
    !.
check_insecure_user_header_enabled(Enabled) :-
    Env_Var = 'TERMINUSDB_INSECURE_USER_HEADER_ENABLED',
    getenv_default(Env_Var, false, Enabled),
    die_if(\+ memberchk(Enabled, [false, true]),
           error(bad_env_var_value(Env_Var, Enabled), _)),
    assertz(check_insecure_user_header_enabled_(Enabled)).

:- dynamic insecure_user_header_key_/1.

/* Retract the dynamic predicate for testing. */
clear_insecure_user_header_key :-
    retractall(insecure_user_header_key_(_)).

/**
 * insecure_user_header_key(-Header_Key) is semidet.
 *
 * Check if the insecure user header is enabled, look up the env var for the
 * insecure user header, and convert it to a key for checking an HTTP request.
 */
insecure_user_header_key(Header_Key) :-
    insecure_user_header_key_(Header_Key),
    !.
insecure_user_header_key(Header_Key) :-
    check_insecure_user_header_enabled(true),
    Env_Var = 'TERMINUSDB_INSECURE_USER_HEADER',
    do_or_die(getenv(Env_Var, Value),
              error(missing_env_var(Env_Var), _)),
    die_if(\+ re_match("[A-Za-z0-9-]+", Value),
           error(bad_env_var_value(Env_Var, Value), _)),
    string_lower(Value, Lower_String),
    re_replace("-"/g, "_", Lower_String, Lower_String_No_Dashes),
    atom_string(Header_Key, Lower_String_No_Dashes),
    assertz(insecure_user_header_key_(Header_Key)).

/**
 * check_all_env_vars is det.
 *
 * Load and check all env vars.
 *
 * This should be done at initialization, so that the error-checking is
 * performed as soon as possible and the the user is notified of any errors.
 *
 * Note that nothing should go wrong if this is not called. All of the
 * predicates referenced here can be called at any time. This is only done to
 * improve the user experience.
 */
check_all_env_vars :-
    ignore(insecure_user_header_key(_)).

is_enterprise :-
    current_prolog_flag(terminusdb_enterprise, true).

parse_pinned_databases(Pinned_Env, Pinned) :-
    merge_separator_split(Pinned_Env, ',', Pinned_Atoms),
    maplist([Atom, Descriptor]>>(
                do_or_die(resolve_absolute_string_descriptor(Atom, Descriptor),
                          error(invalid_descriptor(Atom), _))
            ),

            Pinned_Atoms,
            Pinned).

:- table pinned_databases/1.
pinned_databases(Pinned) :-
    getenv('TERMINUSDB_PINNED_DATABASES', Pinned_Env),
    !,
    parse_pinned_databases(Pinned_Env, Pinned).
pinned_databases([]).

:- table parallelize_enabled.
parallelize_enabled :-
    getenv_default('TERMINUSDB_PARALLELIZE_ENABLED', true, true).

:- table grpc_label_endpoint/1.
grpc_label_endpoint(Endpoint) :-
    getenv('TERMINUSDB_GRPC_LABEL_ENDPOINT', Endpoint).

crypto_password_cost(10).
