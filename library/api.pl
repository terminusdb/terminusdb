:- module(api,[]).

% http libraries
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_header)).
:- use_module(library(http/json)). 
:- use_module(library(http/json_convert)).

% woql libraries
:- use_module(library(woql_compile)).

% Set base location
http:location(api, '/api', []).

:- http_handler('/', regulum_reply, []). 
:- http_handler(api(.), api_reply, []).
:- http_handler(api(woql), api_woql, []). 

/** 
 * api_reply(+Request:http_request) is det.
 */ 
regulum_reply(_) :- 
    reply_html_page(
		[ title(['RegulumDB'])
		], 
		[ h2('Welcome to RegulumDB - the most powerful an flexible linked-data DB in the world!'), 
		  p('We hope you will enjoy reaing our extensive documentation so you can know why you should not be here.')
		]).


/** 
 * api_reply(+Request:http_request) is det.
 */ 
api_reply(_) :- 
    reply_html_page(
		[ title(['Regulum API top-level'])
		], 
		[ h2('This is the root directory for the RegulumDB API.'), 
		  p('Please read the documention included with RegulumDB in order to interact with the graph.')
		]).

/** 
 * api_woql(+Request:http_request) is det.
 */ 
api_woql(Request) :-
    http_parameters(Request, [], [form_data(Data)]),
    
    get_key(query,Query,Data),
    http_log_stream(Log),
    run_query(Query, JSON),
    * format(Log,'Query: ~q~nResults in: ~q~n',[Query,JSON]),
    format('Content-type: application/json~n~n'),
    current_output(Out),
	json_write(Out,JSON).

