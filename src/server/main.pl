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
:- use_module(core(plugins)).

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
    format(string(ListenSpec), '~w:~w', [ListenAddress, Port]),
    HTTPOptions = [port(ListenAddress:Port), workers(Workers), silent(true)],
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
                 ]),
    % initialize the global store as an in-memory store if the memory flag is set
    (   option(memory(Memory_Password),Argv),
        ground(Memory_Password)
    ->  memory_triple_store(Store),
        (   Memory_Password = ''
        ->  Password = root
        ;   Password = Memory_Password),
        initialize_database_with_store(Password, Store),
        global_triple_store(Store),
        set_memory_mode
    ;   true),

    (   triple_store(_Store), % ensure triple store has been set up by retrieving it once
        http_delete_handler(id(busy_loading)),
        load_jwt_conditionally,
        welcome_banner(ListenSpec,Argv),
        foreach(post_server_startup_hook(Port),true),
        (   Wait = true
        ->  (   current_prolog_flag(unix, true)
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
            (   catch(call_with_time_limit(5, http_stop_server(Port, [])),
                      _, fail)
            ->  shutdown_message('HTTP server stopped')
            ;   shutdown_message('HTTP server stop timed out'))
        ;   true
        )
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
