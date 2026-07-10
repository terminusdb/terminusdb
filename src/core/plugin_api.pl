:- module(plugin_api, [
    register_route/3,
    cors_handler/3,
    cors_handler/4,
    authenticate/3,
    descriptor_to_path/2,
    validate_db_path/1,
    encode_query_value/2,
    document_changes/5,
    schema_types_with_metadata/2,
    raw_out_stream/2,
    stream_ndjson/2
]).

:- use_module(core(plugin_api/http)).
:- use_module(core(plugin_api/path)).
:- use_module(core(plugin_api/encode)).
:- use_module(core(plugin_api/delta)).
:- use_module(core(plugin_api/stream)).

:- reexport(core(plugin_api/http)).
:- reexport(core(plugin_api/path)).
:- reexport(core(plugin_api/encode)).
:- reexport(core(plugin_api/delta)).
:- reexport(core(plugin_api/stream)).
