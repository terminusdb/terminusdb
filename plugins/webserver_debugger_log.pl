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
%  Subscribe this connection to the `actions` broadcast channel. Log entries
%  are pushed by `appserver_broadcast_send/2` and forwarded to every
%  subscriber by Rust, so the stream flushes continuously as the server acts.
actions_handler(_Request, StreamId, Response) :-
    '$appserver':appserver_broadcast_subscribe(actions, StreamId, 0),
    Response = _{
        status: 200,
        headers: _{'Content-Type': 'application/x-ndjson'},
        body: stream
    }.
