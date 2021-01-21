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
              jwt_public_key_path/1,
              jwt_public_key_id/1,
              jwt_enabled/0,
              registry_path/1,
              console_base_url/1,
              ssl_cert/1,
              ssl_cert_key/1,
              pack_dir/1,
              https_enabled/0,
              autologin_enabled/0,
              tmp_path/1,
              server_worker_options/1,
              http_options/1,
              ignore_ref_and_repo_schema/0
          ]).

:- use_module(core(util)).

version('4.1.0').

:- dynamic https_cert/1.
:- dynamic https_certkey/1.
bootstrap_config_files :-
    expand_file_search_path(terminus_home('config/localhost.crt'), DefaultCert),
    getenv_default('TERMINUSDB_SSL_CERT', DefaultCert, Cert),
    expand_file_search_path(terminus_home('config/localhost.key'), DefaultKey),
    getenv_default('TERMINUSDB_SSL_CERT_KEY', DefaultKey, Key),
    file_to_predicate(Cert, https_cert),
    file_to_predicate(Key, https_certkey).

server_protocol(Value) :-
    (   https_enabled
    ->  Value = https
    ;   Value = http).

server_name(Value) :-
    getenv_default('TERMINUSDB_SERVER_NAME', '127.0.0.1', Value).

server_port(Value) :-
    getenv_default_number('TERMINUSDB_SERVER_PORT', 6363, Value).

worker_amount(Value) :-
    getenv_default_number('TERMINUSDB_SERVER_WORKERS', 8, Value).

max_transaction_retries(Value) :-
    getenv_default_number('TERMINUSDB_SERVER_MAX_TRANSACTION_RETRIES', 3, Value).

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

:- dynamic worker_js_/1.

worker_js(Value) :-
    worker_js_(Value),
    !.
worker_js(Value) :-
    once(expand_file_search_path(config('worker.js'), Template_Path)),
    open(Template_Path, read, Template_Stream),
    read_string(Template_Stream, _, Value),
    assertz(worker_js_(Value)),
    close(Template_Stream).


default_database_path(Value) :-
    getenv_default('TERMINUSDB_SERVER_DB_PATH', './storage/db', Value).

jwt_enabled :-
    getenv_default('TERMINUSDB_JWT_ENABLED', false, Value),
    Value = true.

jwt_public_key_path(Value) :-
    getenv_default('TERMINUSDB_SERVER_JWT_PUBLIC_KEY_PATH', '', Value).

jwt_public_key_id(Value) :-
    getenv_default('TERMINUSDB_SERVER_JWT_PUBLIC_KEY_ID', '', Value).

console_base_url(Value) :-
    getenv_default('TERMINUSDB_CONSOLE_BASE_URL', 'https://dl.bintray.com/terminusdb/terminusdb/dev', Value).

https_enabled :-
    getenv_default('TERMINUSDB_HTTPS_ENABLED', 'true', Value),
    Value = 'true'.

autologin_enabled :-
    getenv_default('TERMINUSDB_AUTOLOGIN_ENABLED', 'false', Value),
    Value = 'true'.

ssl_cert(Filename) :-
    (   getenv('TERMINUSDB_SSL_CERT', Filename)
    ->  true
    ;   https_cert(Value),
        open_string(Value, Stream),
        tmp_file_stream(text, File, TmpStream),
        copy_stream_data(Stream, TmpStream),
        close(TmpStream),
        close(Stream),
        Filename = File).

ssl_cert_key(Filename) :-
    (   getenv('TERMINUSDB_SSL_CERT_KEY', Filename)
    ->  true
    ;   https_certkey(Value),
        open_string(Value, Stream),
        tmp_file_stream(text, File, TmpStream),
        copy_stream_data(Stream, TmpStream),
        close(TmpStream),
        close(Stream),
        Filename = File).

pack_dir(Value) :-
    getenv('TERMINUSDB_SERVER_PACK_DIR', Value).

registry_path(Value) :-
    once(expand_file_search_path(plugins('registry.pl'), Path)),
    getenv_default('TERMINUSDB_SERVER_REGISTRY_PATH', Path, Value).

tmp_path(Value) :-
    user:file_search_path(terminus_home, Dir),
    atom_concat(Dir,'/tmp',TmpPathRelative),
    getenv_default('TERMINUSDB_SERVER_TMP_PATH', TmpPathRelative, Value).

server(Server) :-
    server_protocol(Protocol),
    server_name(Name),
    server_port(Port),
    atomic_list_concat([Protocol,'://',Name,':',Port],Server).

server_worker_options([]).

http_options([]).

:- table ignore_ref_and_repo_schema/0.
ignore_ref_and_repo_schema :-
    getenv('TERMINUSDB_IGNORE_REF_AND_REPO_SCHEMA', true).

:- set_prolog_flag(stack_limit, 8_589_934_592).

% Turn off mavis
:- set_prolog_flag(optimise, true).
