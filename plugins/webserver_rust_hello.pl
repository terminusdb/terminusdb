:- module(webserver_rust_hello, []).

%% This plugin demonstrates loading a Rust foreign predicate plugin
%% and exposing it through an HTTP endpoint. The Rust shared object
%% (libhello_plugin.dylib / libhello_plugin.so) is loaded automatically
%% by load_foreign_plugins/2 during load_plugins/0. It registers
%% hello_rust/1 in the '$rustnative' module. This Prolog plugin calls
%% that predicate and returns the result as JSON.

:- use_module(library(http/http_json)).

%% Register route via tdb_http_handler — same mechanism as webserver_hello.pl.
:- tdb_http_handler:tdb_http_handler('/api/v1/ext/rust-hello',
                routes:cors_handler(Method, webserver_rust_hello:rust_hello_handler,
                                    [skip_authentication(true)]),
                [method(Method),
                 methods([options,get])]).

%% rust_hello_handler(+Method, +Request, +System_DB, +Auth) is det.
%
%  Calls the Rust foreign predicate hello_rust/1 and returns the
%  greeting as JSON. The predicate is registered in the '$rustnative'
%  module by the Rust plugin's install() entry point.
rust_hello_handler(_Method, _Request, _System_DB, _Auth) :-
    '$rustnative':hello_rust(Greeting),
    reply_json(_{message: Greeting}).
