:- module(api,[]).

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

% Set base location
http:location(api, '/api', []).

:- http_handler('/', regulus_reply, []). 
:- http_handler(api(.), api_reply, []). 

/** 
 * api_reply(+Request:http_request) is det.
 */ 
regulus_reply(_) :- 
    reply_html_page(
		[ title(['RegulusDB'])
		], 
		[ h2('Welcome to RegulusDB - the most powerful an flexible linked-data DB in the world!'), 
		  p('We hope you will enjoy reaing our extensive documentation so you can know why you should not be here.')
		]).


/** 
 * api_reply(+Request:http_request) is det.
 */ 
api_reply(_) :- 
    reply_html_page(
		[ title(['Regulus API top-level'])
		], 
		[ h2('This is the root directory for the RegulusDB API.'), 
		  p('Please read the documention included with terminus in order to interact with the graph.')
		]).
