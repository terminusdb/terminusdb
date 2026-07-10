:- module(plugin_api_http, [
    register_route/3,
    cors_handler/3,
    cors_handler/4,
    authenticate/3,
    write_cors_headers/1,
    api_report_errors/3,
    resolve_descriptor_auth/6
]).

:- use_module(server(routes)).
:- use_module(server(routes/tdb_http_handler)).
:- use_module(core(account/capabilities), [resolve_descriptor_auth/6]).

:- reexport(server(routes), [cors_handler/3, cors_handler/4, authenticate/3,
                             write_cors_headers/1, api_report_errors/3]).
:- reexport(core(account/capabilities), [resolve_descriptor_auth/6]).

%% register_route(+Path, +Handler, +Options) is det.
%
%  Drop-in wrapper around tdb_http_handler/3 that registers a route
%  with both the SWI-Prolog HTTP dispatcher and the Rust webserver.
%  Plugins call this instead of reaching into server(routes) directly.
%
%  Handler is the same closure that http_handler/3 accepts, typically
%  cors_handler(Method, Goal, ExtraOptions).
register_route(Path, Handler, Options) :-
    tdb_http_handler:tdb_http_handler(Path, Handler, Options).
