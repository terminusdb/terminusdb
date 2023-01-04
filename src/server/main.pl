:- module(server, [terminus_server/2]).

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
:- use_module(core(plugins)).

% configuration predicates
:- use_module(config(terminus_config),[jwt_enabled/0,
                                       jwt_jwks_endpoint/1,
                                       server/1,
                                       server_port/1,
                                       log_format/1,
                                       worker_amount/1,
                                       is_enterprise/0,
                                       terminusdb_version/1]).

% Sockets
:- use_module(library(socket)).
:- use_module(library(ssl)).

% http server
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/html_write)).
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
    add_dashboard_path,
    load_jwt_conditionally,
    HTTPOptions = [port(Port), workers(Workers)],
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
                 ]),
    % initialize the global store as an in-memory store if the memory flag is set
    (   option(memory(Memory_Password),Argv),
        ground(Memory_Password)
    ->  memory_triple_store(Store),
        (   Memory_Password = ''
        ->  Password = root
        ;   Password = Memory_Password),
        initialize_database_with_store(Password, Store),
        global_triple_store(Store)
    ;   true),

    (   triple_store(_Store), % ensure triple store has been set up by retrieving it once
        http_delete_handler(id(busy_loading)),
        welcome_banner(Server,Argv),
        foreach(post_server_startup_hook(Port),true),
        (   Wait = true
        ->  http_current_worker(Port,ThreadID),
            thread_join(ThreadID, _Status)
        ;   true
        )
    ).


% See https://github.com/terminusdb/terminusdb-server/issues/91
%  TODO replace this with a proper page
%
busy_loading(_) :-
    reply_html_page(
        title('Still Loading'),
        \loading_page).

loading_page -->
    html([
        h1('Still loading'),
        p('TerminusDB is still synchronizing backing store')
    ]).


print_welcome_banner(Version, Enterprise, Argv, _, _, Server) :-
    log_format(json),
    !,
    format(user_error, '{"message": "Welcome to TerminusDB ~s! You can view your server in a browser at ~s",\c
                          "version": "~s", "args": "~w", "severity": "INFO"}~n',
          [Enterprise, Server, Version, Argv]).
print_welcome_banner(Version, Enterprise, Argv, StrTime, Now, Server) :-
    format(user_error,'~N% TerminusDB server started at ~w (utime ~w) args ~w~n',
           [StrTime, Now, Argv]),
    format(user_error,'% Welcome to TerminusDB\'s~w terminusdb-server, version ~s!~n',[Enterprise, Version]),
    format(user_error,'% You can view your server in a browser at \'~s\'~n~n',[Server]).

welcome_banner(Server,Argv) :-
    % Test utils currently reads this so watch out if you change it!
    get_time(Now),
    terminusdb_version(Version),
    format_time(string(StrTime), '%A, %b %d, %H:%M:%S %Z', Now, posix),
    (   is_enterprise
    ->  Enterprise = ' Enterprise'
    ;   Enterprise = ''),
    print_welcome_banner(Version, Enterprise, Argv, StrTime, Now, Server).
