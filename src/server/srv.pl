:- module(srv, [
              start_server/1
          ]).

:- use_module(core(plugins)).
:- use_module(core(appserver_hooks)).

%% start_server(+Port) is det.
%
%  Start the Rust webserver on Port. Static paths are normalized first, then
%  appserver_start is called. This is used by the main server when
%  TERMINUSDB_SERVER_BACKEND=rust.
start_server(Port) :-
    appserver_hooks:normalize_static_paths,
    '$appserver':appserver_start(Port).


:- use_module(library(http/http_open)).
:- use_module(library(json)).
:- use_module(core(util/test_utils)).

:- begin_tests(srv, [concurrent(false)]).

test_port(6359).

rust_url(Path, URL) :-
    test_port(Port),
    format(atom(Base), 'http://127.0.0.1:~d', [Port]),
    atomic_concat(Base, Path, URL).

setup_test_server(State, Server) :-
    test_port(Port),
    format(atom(PortAtom), '~d', [Port]),
    setup_temp_server(State, Server,
                      [env_vars(['TERMINUSDB_WEBSERVER_PORT'=PortAtom])]).

test(health_endpoint_returns_ok, [
         setup(setup_test_server(State, _Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    rust_url('/api/v1/health', URL),
    http_open(URL, Stream, [status_code(Status), timeout(5)]),
    Status = 200,
    json_read(Stream, JSON),
    close(Stream),
    JSON = json([status=ok]).

test(static_app_serves_index, [
         setup(setup_test_server(State, _Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    rust_url('/app/alpha/', URL),
    http_open(URL, Stream, [status_code(Status), timeout(5)]),
    Status = 200,
    read_string(Stream, 1000000, Body),
    close(Stream),
    sub_string(Body, _, _, _, '<html lang="en">'),
    !.

test(root_redirects_to_alpha, [
         setup(setup_test_server(State, _Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    rust_url('/', URL),
    http_open(URL, Stream,
              [status_code(Status), timeout(5),
               redirect(false),
               header(location, Location)]),
    Status = 307,
    sub_string(Location, _, _, _, '/app/alpha'),
    close(Stream).

test(info_plugin_endpoint, [
         setup(setup_test_server(State, _Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    rust_url('/api/info', URL),
    http_open(URL, Stream, [status_code(Status), timeout(5)]),
    Status = 200,
    json_read(Stream, JSON),
    close(Stream),
    JSON = json([('@type'='api:InfoResponse'),
                 ('api:status'='api:success'),
                 ('api:info'=json([('authority'='terminusdb://system/data/User/anonymous')|_]))|_]).

test(ok_plugin_endpoint, [
         setup(setup_test_server(State, _Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    rust_url('/api/ok', URL),
    http_open(URL, Stream, [status_code(Status), timeout(5)]),
    Status = 200,
    read_string(Stream, 1000, Body),
    close(Stream),
    Body = "".

test(hello_wildcard_plugin_endpoint, [
         setup(setup_test_server(State, _Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    rust_url('/api/v1/ext/hello/Prolog', URL),
    http_open(URL, Stream, [status_code(Status), timeout(5)]),
    Status = 200,
    json_read(Stream, JSON),
    close(Stream),
    JSON = json([greeting='Hello, Prolog!'|_]).

test(hello_echo_plugin_stream, [
         setup(setup_test_server(State, _Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    rust_url('/hello-echo', URL),
    format(string(Body), '{"hello":"world"}~n{"foo":"bar"}~n', []),
    http_open(URL, Stream,
              [status_code(Status),
               timeout(5),
               post(string(Body)),
               request_header('Content-Type'='application/x-ndjson')]),
    Status = 200,
    read_string(Stream, 1000000, Response),
    close(Stream),
    sub_string(Response, _, _, _, '{"hello":"world"}'),
    sub_string(Response, _, _, _, '{"foo":"bar"}'),
    !.

test(actions_plugin_stream_smoke, [
         setup(setup_test_server(State, _Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    rust_url('/api/v1/ext/actions', URL),
    http_open(URL, Stream,
              [status_code(Status),
               timeout(5),
               header(content_type, ContentType)]),
    Status = 200,
    ContentType = 'application/x-ndjson',
    close(Stream).

:- end_tests(srv).
