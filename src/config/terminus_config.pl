:- module(config,[
              version/1,
              bootstrap_config_files/0,
              server/1,
              server_name/1,
              server_port/1,
              worker_amount/1,
              max_transaction_retries/1,
              index_template/1,
              default_database_path/1,
              jwt_jwks_endpoint/1,
              jwt_enabled/0,
              registry_path/1,
              pack_dir/1,
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
              clear_log_format/0
          ]).

:- use_module(core(util)).

version('10.0.10').

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
    getenv_default_number('TERMINUSDB_SERVER_WORKERS', 8, Value).

:- table max_transaction_retries/1 as shared.
max_transaction_retries(Value) :-
    (   getenv('TERMINUSDB_SERVER_MAX_TRANSACTION_RETRIES', Atom_Value)
    ->  atom_number(Atom_Value, Value)
    ;   worker_amount(Num_Workers),
        Value is Num_Workers * 2).

:- dynamic index_template_/1.

index_template(Value) :-
    index_template_(Value),
    !.
index_template(Value) :-
    once(expand_file_search_path(config('index.tpl'), Template_Path)),
    open(Template_Path, read, Template_Stream),
    read_string(Template_Stream, _, Value),
    assertz(index_template_(Value)),
    close(Template_Stream).

default_database_path(Value) :-
    getenv_default('TERMINUSDB_SERVER_DB_PATH', './storage/db', Value).

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

pack_dir(Value) :-
    getenv('TERMINUSDB_SERVER_PACK_DIR', Value).

registry_path(Value) :-
    once(expand_file_search_path(plugins('registry.pl'), Path)),
    getenv_default('TERMINUSDB_SERVER_REGISTRY_PATH', Path, Value).

tmp_path(Value) :-
    user:file_search_path(terminus_home, Dir),
    atom_concat(Dir,'/tmp',TmpPathRelative),
    getenv_default('TERMINUSDB_SERVER_TMP_PATH', TmpPathRelative, Value).

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
