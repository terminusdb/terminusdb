:- module(cli, [process_cli/1]).

/** <module> HTTP server module
 *
 * This module implements the database server. It is primarily composed
 * of a number of RESTful APIs which exchange information in JSON format
 * over HTTP. This is intended as a mechanism for interprocess
 * communication via *API* and not as a fully fledged high performance
 * server.
 *
 **/

process_cli(_) :-
    format("~nUsage: terminusdb [OPTION]~n",[]),
    format("~n~n",[]),
    format("serve\t\t\trun the terminusdb server~n",[]),
    format("list\t\t\tlist databases~n",[]),
    format("query [QUERY]\trun query~n",[]),
    format("test\t\t\trun tests~n",[]),
    halt.

