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
:- use_module(core(util), [json_log_error_formatted/2,
                          json_log_warning_formatted/2]).
:- use_module(core(api)).
:- use_module(core(document/parallel_elaboration), [
                  start_elaboration_workers/1,
                  stop_elaboration_workers/0
              ]).
:- use_module(core(plugins)).

% Load the Rust appserver and its built-in route modules. These are now core
% server components rather than optional plugins.
:- use_module(server(srv)).
:- use_module(server(readiness), [register_components/0,
                                  publish_readiness/0,
                                  mark_hooks_done/0]).

:- multifile plugins:readiness_component/1.

% The system store participates in startup readiness: triple_store/1
% opens it lazily after the socket accepts, so requests are held until
% it is usable. Memory mode sets the global store before bind; the same
% mark-ready point covers both modes.
plugins:readiness_component(store).

% configuration predicates
:- use_module(config(terminus_config),[jwt_enabled/0,
                                       jwt_jwks_endpoint/1,
                                       jwt_key_source_configured/0,
                                       jwt_runtime_enabled/0,
                                       oidc_issuer_url/1,
                                       check_jwt_scopes_claim_safety/0,
                                       check_jwt_subject_claim_safety/0,
                                       check_jwt_config_safety/0,
                                       server_port/1,
                                       listen_address/1,
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
:- use_module(library(lists), [member/2]).
:- use_module(library(time), [call_with_time_limit/2]).
:- use_module(library(plunit)).
:- use_module(library(debug), [assertion/1]).

:- use_module(library(option)).

% JWT setup using Rust foreign predicates (registered in $rustnative module)
:- if(jwt_enabled).

load_jwt_conditionally :-
    (   jwt_runtime_enabled
    ->  (   jwt_jwks_endpoint(Endpoint)
        ->  ignore(catch('$rustnative':jwt_setup_jwks(Endpoint), E,
                  (   json_log_error_formatted('JWT JWKS setup failed: ~w', [E]),
                      true)))
        ;   oidc_issuer_url(IssuerUrl)
        ->  ignore(catch('$rustnative':jwt_setup_oidc(IssuerUrl), E,
                  (   json_log_error_formatted('JWT OIDC setup failed: ~w', [E]),
                      true)))
        ;   true  % No JWKS or OIDC configured — JWT auth will fail at decode time
        ),
        check_jwt_scopes_claim_safety,
        check_jwt_subject_claim_safety,
        check_jwt_config_safety
    ;   jwt_key_source_configured
    ->  json_log_warning_formatted(
            'A JWT key source is configured (TERMINUSDB_SERVER_JWKS_ENDPOINT or TERMINUSDB_OIDC_ISSUER_URL) but JWT is disabled. Set TERMINUSDB_JWT_ENABLED=true to enable JWT authentication.',
            [])
    ;   true
    ).

:- else.

% Otherwise, do nothing
load_jwt_conditionally :-
    true.

:- endif.


terminus_server(Argv,Wait) :-
    server_port(Port),
    listen_address(ListenAddress),
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
    % Mark every declared readiness component pending and publish the
    % not-ready state before the socket accepts a single request.
    register_components,
    publish_readiness,
    start_server_backend(Backend, Port, Workers),

    % Hooks run first so detached init (e.g. vectorlink) overlaps the
    % store open, which stays synchronous and aborts on failure exactly
    % as before. await_all_components then blocks until every component
    % resolves and rethrows the first failure, preserving the old
    % abort-on-init-error semantics for detached init.
    (   format(string(ListenSpec), '~w:~w', [ListenAddress, Port]),
        welcome_banner(ListenSpec,Argv),
        run_post_server_startup_hooks(Backend, Port),
        catch(triple_store(_Store), % ensure triple store has been set up by retrieving it once
              Store_Error,
              (   readiness:readiness_failed(store, Store_Error),
                  throw(Store_Error))),
        readiness:readiness_ready(store),
        (   Backend == swipl
        ->  http_delete_handler(id(busy_loading))
        ;   true
        ),
        readiness:await_all_components(Failed),
        (   Failed = [failed(_Component, Init_Error)|_]
        ->  throw(Init_Error)
        ;   true),
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

%% start_server_backend(+Backend, +ListenAddress, +Port, +Workers) is det.
%%
%  Start the selected HTTP server backend on Port.
start_server_backend(swipl, ListenAddress, Port, Workers) :-
    (   server_enabled
    ->  HTTPOptions = [port(ListenAddress:Port), workers(Workers), silent(true)],
        foreach(pre_server_startup_hook(Port),true),
        catch(http_server(http_dispatch, HTTPOptions),
              E,
              (   (   E = error(socket_error(eaddrinuse,_),_)
                  ->  format(user_error, "Error: ~w:~d is already in use.~n", [ListenAddress, Port])
                  ;   format(user_error, "Error starting server: ~w~n", [E])),
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
start_server_backend(rust, _ListenAddress, Port, _Workers) :-
    % The Rust webserver FFI binds unconditionally; TERMINUSDB_LISTEN_ADDRESS
    % is honored only by the swipl backend.
    (   server_enabled
    ->  srv:start_server(Port)
    ;   format(user_error, "Main Rust HTTP server disabled (TERMINUSDB_SERVER_PORT=false).~n", []),
        true
    ).

%% run_post_server_startup_hooks(+Backend, +Port) is det.
%%
%  Run post-startup hooks for both backends. These hooks wire up
%  plugin configuration (e.g. indexer_set_config for tdb-search).
run_post_server_startup_hooks(_Backend, Port) :-
    foreach(post_server_startup_hook(Port), true),
    mark_hooks_done.

%% wait_for_backend(+Backend, +Port) is det.
%%
%  Block until a shutdown signal arrives, then stop the HTTP layer for the
%  swipl backend. For the Rust backend there is no HTTP layer to stop, so we
%  return once the stop message lands.
wait_for_backend(Backend, Port) :-
    (   current_prolog_flag(unix, true)
    ->  % Register at serve time: library(main) installs
        % prolog_main:interrupt after the load-time handlers, and
        % saved states lose load-time on_signal/3 registrations.
        on_signal(int,  _, server_shutdown_signal),
        on_signal(term, _, server_shutdown_signal),
        on_signal(hup,  _, server_shutdown_signal)
    ;   true),
    thread_get_message(stop),
    nl(user_error),
    shutdown_message('Shutting down'),
    (   Backend == swipl,
        server_enabled
    ->  (   catch(call_with_time_limit(5, http_stop_server(Port, [])),
                  _, fail)
        ->  shutdown_message('HTTP server stopped')
        ;   shutdown_message('HTTP server stop timed out'))
    ;   true
    ).


server_shutdown_signal(_Signal) :-
    thread_send_message(main, stop).

shutdown_message(Message) :-
    (   log_format(json)
    ->  format(user_error, '{"message": "~s", "severity": "INFO"}~n', [Message])
    ;   format(user_error, '% ~s~n', [Message])).

busy_loading(_) :-
    format('Status: 503 Service Unavailable~n'),
    format('Content-type: text/html~n~n'),
    format('<html><body><h1>Still loading</h1><p>TerminusDB is preparing to serve requests</p></body></html>~n').


print_welcome_banner(Version, ProductName, Argv, Iso, ListenSpec) :-
    log_format(json),
    !,
    format(user_error, '{"message": "~s ~s listening on ~s",\c
                          "version": "~s", "args": "~w", "timestamp": "~s", "severity": "INFO"}~n',
          [ProductName, Version, ListenSpec, Version, Argv, Iso]).
print_welcome_banner(Version, ProductName, Argv, Iso, ListenSpec) :-
    welcome_banner_lines(Version, ProductName, Argv, Iso, ListenSpec, Lines),
    forall(member(Line, [''|Lines]), format(user_error, '~s~n', [Line])),
    nl(user_error).

welcome_banner_lines(Version, ProductName, Argv, Iso, ListenSpec,
                     [Line1, Line2, Line3]) :-
    format(string(Line1), '% ~s ~s started ~s', [ProductName, Version, Iso]),
    format(string(Line2), '% Args: ~w', [Argv]),
    format(string(Line3), '% Listening on ~s', [ListenSpec]).

welcome_banner(ListenSpec,Argv) :-
    % Test utils currently reads this so watch out if you change it!
    get_time(Now),
    stamp_date_time(Now, Date, 'UTC'),
    format_time(string(Iso), '%Y-%m-%dT%H:%M:%SZ', Date),
    terminusdb_version(Version),
    (   enterprise_product_name(ProductName)
    ->  true
    ;   is_enterprise
    ->  ProductName = "TerminusDB Enterprise"
    ;   ProductName = "TerminusDB"
    ),
    print_welcome_banner(Version, ProductName, Argv, Iso, ListenSpec).

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

:- begin_tests(welcome_banner).

test(boring_parseable_lines) :-
    welcome_banner_lines('12.0.8', "TerminusDB", [serve,memory()],
                         "2026-10-16T06:12:11Z", 'localhost:6480', Lines),
    Lines = [Line1, Line2, Line3],
    assertion(Line1 == "% TerminusDB 12.0.8 started 2026-10-16T06:12:11Z"),
    assertion(Line2 == "% Args: [serve,memory()]"),
    assertion(Line3 == "% Listening on localhost:6480").

test(listening_line_shows_bind_address) :-
    welcome_banner_lines('12.0.8', "TerminusDB", [serve],
                         "2026-10-16T06:12:11Z", '0.0.0.0:6363', Lines),
    Lines = [_, _, Line3],
    assertion(Line3 == "% Listening on 0.0.0.0:6363").

:- end_tests(welcome_banner).
