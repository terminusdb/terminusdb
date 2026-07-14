:- module(plugin_api, [
    register_route/3,
    cors_handler/3,
    cors_handler/4,
    authenticate/3,
    write_cors_headers/1,
    api_report_errors/3,
    resolve_descriptor_auth/6,
    descriptor_to_path/2,
    validate_db_path/1,
    encode_query_value/2,
    document_changes/5,
    schema_types_with_metadata/2,
    raw_out_stream/2,
    stream_ndjson/2,
    plugin_env/2,
    plugin_env_default/3,
    plugin_consume_env/2,
    plugin_consume_env_default/3,
    abolish_plugin_env/1,
    abolish_plugin_env_default/2,
    indexer_notify/3,
    indexer_set_config/2,
    indexer_progress/3,
    indexer_abort_domain/1,
    indexer_available/0
]).

:- use_module(core(plugin_api/http)).
:- use_module(core(plugin_api/path)).
:- use_module(core(plugin_api/encode)).
:- use_module(core(plugin_api/delta)).
:- use_module(core(plugin_api/stream)).
:- use_module(core(plugin_api/config)).
:- use_module(core(plugin_api/indexer)).

:- reexport(core(plugin_api/http)).
:- reexport(core(plugin_api/path)).
:- reexport(core(plugin_api/encode)).
:- reexport(core(plugin_api/delta)).
:- reexport(core(plugin_api/stream)).
:- reexport(core(plugin_api/config)).
:- reexport(core(plugin_api/indexer)).
