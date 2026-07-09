:- module(server, [terminus_server/2]).

:- multifile enterprise_product_name/1.

/** <module> HTTP server module
 *
 * This module implements the database server. It is primarily composed
 * of a number of RESTful APIs which exchange information in JSON format
 * over HTTP. This is intended as a mechanism for interprocess
 * communication via *API* and not as a fully fledged high performance
 * server.
 *
 **/

:- use_module(core(triple)).
:- use_module(core(util/utils)).
:- use_module(core(api)).
:- use_module(core(document/parallel_elaboration), [
                  start_elaboration_workers/1,
                  stop_elaboration_workers/0
              ]).
:- use_module(core(plugins)).

% Load the Rust appserver and its built-in route modules. These are now core
% server components rather than optional plugins.
:- use_module(server(srv)).

% configuration predicates
:- use_module(config(terminus_config),[jwt_enabled/0,
                                       jwt_jwks_endpoint/1,
                                       server/1,
                                       server_port/1,
                                       server_enabled/0,
                                       log_format/1,
                                       worker_amount/1,
                                       is_enterprise/0,
                                       terminusdb_version/1,
                                       set_memory_mode/0]).

% Sockets
:- use_module(library(socket)).
:- use_module(library(ssl)).

% http server
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_ssl_plugin)).
% html_write no longer needed after busy_loading simplified to CGI-style 503
:- use_module(library(aggregate)).

:- use_module(library(option)).

% JWT IO library
:- if(jwt_enabled).

% Load the library only if JWT is enabled
:- use_module(library(jwt_io)).

% Set up JWKS only if we have an endpoint
load_jwt_conditionally :-
    (   jwt_jwks_endpoint(Endpoint)
    ->  jwt_io:setup_jwks(Endpoint)
    ;   true).

:- else.

% Otherwise, do nothing
load_jwt_conditionally :-
    true.

:- endif.


terminus_server(Argv,Wait) :-
    server(Server),
    server_port(Port),
    worker_amount(Workers),
    load_jwt_conditionally,
    % initialize the global store as an in-memory store if the memory flag is set.
    % This must happen before start_elaboration_workers/1, because worker threads
    % call triple_store/1 and would otherwise fall back to the file-based
    % default_triple_store, which fails when no disk store has been initialized.
    (   option(memory(Memory_Password),Argv),
        ground(Memory_Password)
    ->  memory_triple_store(Store),
        global_triple_store(Store),
        (   Memory_Password = ''
        ->  Password = root
        ;   Password = Memory_Password),
        initialize_database_with_store(Password, Store),
        set_memory_mode
    ;   true),
    start_elaboration_workers(Workers),
    server_backend(Backend),
    start_server_backend(Backend, Port, Workers),

    (   triple_store(_Store), % ensure triple store has been set up by retrieving it once
        (   Backend == swipl
        ->  http_delete_handler(id(busy_loading))
        ;   true
        ),
        welcome_banner(Server,Argv),
        run_post_server_startup_hooks(Backend, Port),
        (   Wait = true
        ->  wait_for_backend(Backend, Port)
        ;   true
        ),
        stop_elaboration_workers,
        '$change_window':change_window_assert_empty
    ).

%% server_backend(-Backend) is det.
%%
%  Read TERMINUSDB_SERVER_BACKEND (default `rust`).
%
%  The Rust webserver is the default backend for both the community and
%  enterprise builds. The traditional SWI-Prolog HTTP server remains
%  available as an opt-in via TERMINUSDB_SERVER_BACKEND=swipl.
server_backend(Backend) :-
    (   getenv('TERMINUSDB_SERVER_BACKEND', BackendEnv)
    ->  atom_string(BackendEnv, BackendAtom),
        downcase_atom(BackendAtom, Backend)
    ;   Backend = rust
    ).

%% start_server_backend(+Backend, +Port, +Workers) is det.
%%
%  Start the selected HTTP server backend on Port.
start_server_backend(swipl, Port, Workers) :-
    (   server_enabled
    ->  HTTPOptions = [port(Port), workers(Workers), silent(true)],
        foreach(pre_server_startup_hook(Port),true),
        catch(http_server(http_dispatch, HTTPOptions),
              E,
              (
                  writeq(E),
                  format(user_error, "Error: Port ~d is already in use.", [Port]),
                  halt(98) % EADDRINUSE
              )),
        http_handler(root(.), busy_loading,
                     [ priority(1000),
                       hide_children(true),
                       id(busy_loading),
                       time_limit(infinite),
                       prefix
                     ])
    ;   format(user_error, "Main SWI-Prolog HTTP server disabled (TERMINUSDB_SERVER_PORT=false).~n", []),
        true
    ).
start_server_backend(rust, Port, _Workers) :-
    (   server_enabled
    ->  srv:start_server(Port)
    ;   format(user_error, "Main Rust HTTP server disabled (TERMINUSDB_SERVER_PORT=false).~n", []),
        true
    ).

%% run_post_server_startup_hooks(+Backend, +Port) is det.
%%
%  Run the SWI-Prolog post-startup hooks only when the SWI-Prolog backend is
%  active. The Rust backend performs its own startup in start_server_backend.
run_post_server_startup_hooks(swipl, Port) :-
    foreach(post_server_startup_hook(Port), true).
run_post_server_startup_hooks(rust, _Port) :-
    true.

%% wait_for_backend(+Backend, +Port) is det.
%%
%  Block until the selected backend terminates. For the Rust backend there is no
%  worker thread to join, so we block on a message queue.
wait_for_backend(swipl, Port) :-
    (   server_enabled
    ->  http_current_worker(Port,ThreadID),
        thread_join(ThreadID, _Status)
    ;   thread_get_message(_)
    ).
wait_for_backend(rust, _Port) :-
    thread_get_message(_).


busy_loading(_) :-
    format('Status: 503 Service Unavailable~n'),
    format('Content-type: text/html~n~n'),
    format('<html><body><h1>Still loading</h1><p>TerminusDB is preparing to serve requests</p></body></html>~n').


print_welcome_banner(Version, ProductName, Argv, _, _, Server) :-
    log_format(json),
    !,
    format(user_error, '{"message": "Welcome to ~s server! You can view your server in a browser at ~s",\c
                          "version": "~s", "args": "~w", "severity": "INFO"}~n',
          [ProductName, Server, Version, Argv]).
print_welcome_banner(Version, ProductName, Argv, StrTime, Now, Server) :-
    format(user_error,'~N% TerminusDB server started at ~w (utime ~w) args ~w~n',
           [StrTime, Now, Argv]),
    format(user_error,'% Welcome to ~s, version ~s!~n',[ProductName, Version]),
    format(user_error,'% You can view your server in a browser at \'~s\'~n~n',[Server]).

welcome_banner(Server,Argv) :-
    % Test utils currently reads this so watch out if you change it!
    get_time(Now),
    terminusdb_version(Version),
    format_time(string(StrTime), '%A, %b %d, %H:%M:%S %Z', Now, posix),
    (   enterprise_product_name(ProductName)
    ->  true
    ;   is_enterprise
    ->  ProductName = "TerminusDB Enterprise"
    ;   ProductName = "TerminusDB"
    ),
    print_welcome_banner(Version, ProductName, Argv, StrTime, Now, Server).

:- begin_tests(server_backend_selection).

:- dynamic saved_backend/1.

% TERMINUSDB_SERVER_BACKEND may already be set in the environment that
% launches the test process (e.g. the test server script exports it). Save
% the current value in setup and restore it in cleanup so the tests are
% hermetic regardless of the surrounding environment.

setup :-
    (   getenv('TERMINUSDB_SERVER_BACKEND', Current)
    ->  assertz(saved_backend(Current))
    ;   assertz(saved_backend(none))
    ),
    unsetenv('TERMINUSDB_SERVER_BACKEND').

cleanup :-
    (   retract(saved_backend(none))
    ->  unsetenv('TERMINUSDB_SERVER_BACKEND')
    ;   retract(saved_backend(Saved))
    ->  setenv('TERMINUSDB_SERVER_BACKEND', Saved)
    ;   true
    ).

test(default_is_rust, [setup(setup), cleanup(cleanup)]) :-
    server_backend(Backend),
    Backend == rust.

test(explicit_rust, [setup(setup), cleanup(cleanup)]) :-
    setenv('TERMINUSDB_SERVER_BACKEND', 'rust'),
    server_backend(Backend),
    Backend == rust.

test(explicit_swipl, [setup(setup), cleanup(cleanup)]) :-
    setenv('TERMINUSDB_SERVER_BACKEND', 'swipl'),
    server_backend(Backend),
    Backend == swipl.

test(case_insensitive_rust, [setup(setup), cleanup(cleanup)]) :-
    setenv('TERMINUSDB_SERVER_BACKEND', 'RUST'),
    server_backend(Backend),
    Backend == rust.

test(case_insensitive_swipl, [setup(setup), cleanup(cleanup)]) :-
    setenv('TERMINUSDB_SERVER_BACKEND', 'Swipl'),
    server_backend(Backend),
    Backend == swipl.

:- end_tests(server_backend_selection).
