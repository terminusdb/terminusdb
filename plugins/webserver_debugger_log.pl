:- module(webserver_actions, []).

:- use_module(core(plugin_api)).

:- multifile appserver_hooks:appserver_stream/3.

%% appserver_hooks:appserver_stream(+Method, +Path, +Handler) is det.
%
%  Register the `/api/v1/ext/debug-log` NDJSON stream endpoint. Every JSON log
%  entry produced by the server is broadcast by Prolog once to the `actions`
%  channel and Rust multiplexes it to all connected clients.
appserver_hooks:appserver_stream(get, '/api/v1/ext/debug-log', webserver_actions:actions_handler).

%% actions_handler(+Request, +StreamId, -Response) is det.
%
%  Authenticate the listener, require meta_read_access on the system,
%  then subscribe this connection to the `actions` broadcast channel.
%  Log entries are pushed by `appserver_broadcast_send/2` and forwarded
%  to every subscriber by Rust, so the stream flushes continuously.
%
%  Errors (authentication_incorrect, access_not_authorised, etc.) are
%  thrown and mapped to HTTP responses by the worker pool's
%  handle_plugin_stream_request catch wrapper.
actions_handler(Request, StreamId, Response) :-
    open_descriptor(system_descriptor{}, System_DB),
    plugin_api:authenticate_from_request(Request, System_DB, Auth),
    check_descriptor_auth(System_DB, system_descriptor{},
                          '@schema':'Action/meta_read_access', Auth),
    '$appserver':appserver_broadcast_subscribe(actions, StreamId, 0),
    Response = _{
        status: 200,
        headers: _{'Content-Type': 'application/x-ndjson'},
        body: stream
    }.
