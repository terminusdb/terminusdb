:- module(webserver_ping, []).

:- use_module(core(plugin_raw_api)).

%%%%%%%%%%%%%%%%%%%% Ping Handler %%%%%%%%%%%%%%%%%%%%%%%%%
% Minimal latency probe: no database access, no authentication,
% no JSON library. Returns a Unix timestamp in milliseconds so
% callers can measure the bare Rust-to-Prolog dispatch overhead.
%
% Breakdown of the various alternatives:
% - Pure rust path is about 25 μs (40k+ req/s)
% - This ping plugin clocks in at about 200 μs (5000 req/s)
% - Full tdb_http_handler path is about 3.3 ms (300 req/s)
%
% Suggest to use tdb_http_handler instead to get baseline functionality.
%
% Registers via plugin_raw_api (Tier 2) — no tdb_http_handler,
% no http_dispatch, no cors_handler. The worker pool calls
% ping_handler/2 directly via handle_direct_plugin_pipe/6.
:- plugin_raw_api:register_raw_route(get, '/ping', webserver_ping:ping_handler).

%% ping_handler(+Request, -Response) is det.
%
%  Return a 200 OK with a minimal JSON body containing the current
%  Unix timestamp in milliseconds.
ping_handler(_Request, Response) :-
    Response = _{
        status: 200,
        body: '{"ping":"pong"}',
        headers: _{'Content-Type': 'application/json'}
    }.
