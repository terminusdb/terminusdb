:- module(server, [server/1]).

/** <module> HTTP server module

This module implements the database server. It is primarily composed of a number of restful APIs which exchange information in JSON format over HTTP.

*/ 

% configuration predicates
:- use_module(config(config),[]).

% http server
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(triplestore).

server(_Argv) :-
    config:server_port(Port),
    config:server_workers(Workers),
    config:server_worker_options(_Settings),
	config:http_options(HTTPOptions),
	http_server(http_dispatch,
		        [ port(Port),
		          workers(Workers)
		          | HTTPOptions
		        ]),
	setup_call_cleanup(
	    http_handler(root(.), busy_loading,
			 [ priority(1000),
			   hide_children(true),
			   id(busy_loading),
			   prefix
			 ]),
        sync_from_journals,
	    http_delete_handler(id(busy_loading))).
