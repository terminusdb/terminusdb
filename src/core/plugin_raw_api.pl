:- module(plugin_raw_api, [
    register_raw_route/3,
    register_raw_route/4,
    register_raw_stream/3
]).

:- use_module(core(appserver_hooks)).

%% register_raw_route(+Method, +Path, +Handler) is det.
%
%  Register a raw route with the Rust webserver via appserver_hooks.
%  No CORS handling, no authentication, no error formatting.
%  The handler is called directly with (Request, Response) arity.
%
%  Tier 2 — unsupported. This surface may change between minor releases.
register_raw_route(Method, Path, Handler) :-
    assertz(appserver_hooks:appserver_route(Method, Path, Handler, false)).

%% register_raw_route(+Method, +Path, +Handler, +Binary) is det.
%
%  Same as register_raw_route/3 but allows specifying whether the
%  response is binary (true/false).
register_raw_route(Method, Path, Handler, Binary) :-
    assertz(appserver_hooks:appserver_route(Method, Path, Handler, Binary)).

%% register_raw_stream(+Method, +Path, +Handler) is det.
%
%  Register a streaming route with the Rust webserver.
%  The handler is called with (Request, Stream) arity.
%
%  Tier 2 — unsupported.
register_raw_stream(Method, Path, Handler) :-
    assertz(appserver_hooks:appserver_stream(Method, Path, Handler)).
