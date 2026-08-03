:- module(webserver_debugger_log, []).

:- use_module(core(plugin_api)).
:- use_module(core(account), [is_super_user/1]).

:- multifile appserver_hooks:appserver_stream/3.

%% appserver_hooks:appserver_stream(+Method, +Path, +Handler) is det.
%
%  Register the `/api/v1/ext/debug-log` NDJSON stream endpoint. Every JSON log
%  entry produced by the server is broadcast by Prolog once to the `actions`
%  channel and Rust multiplexes it to all connected clients.
appserver_hooks:appserver_stream(get, '/api/v1/ext/debug-log', webserver_debugger_log:debug_log_handler).

%% debug_log_handler(+Request, +StreamId, -Response) is det.
%
%  Authenticate the listener and require super_user access. The debug
%  log stream exposes all server log entries, which may contain sensitive
%  information, so access is restricted to the admin user only.
%
%  Log entries are pushed by `appserver_broadcast_send/2` and forwarded
%  to every subscriber by Rust, so the stream flushes continuously.
%
%  Errors (authentication_incorrect, requires_super_user, etc.) are
%  thrown and mapped to HTTP responses by the worker pool's
%  handle_plugin_stream_request catch wrapper.
debug_log_handler(Request, StreamId, Response) :-
    open_descriptor(system_descriptor{}, System_DB),
    plugin_api:authenticate_from_request(Request, System_DB, Auth),
    do_or_die(is_super_user(Auth),
              error(requires_super_user, _)),
    '$appserver':appserver_broadcast_subscribe(actions, StreamId, 0),
    Response = _{
        status: 200,
        headers: _{'Content-Type': 'application/x-ndjson'},
        body: stream
    }.
