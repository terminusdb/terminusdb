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

:- http_handler(api(.), api_reply, []). 

/** 
 * terminus_reply(+Request:http_request) is det.
 */ 
api_reply(_) :- 
    reply_html_page(cliopatria(default), 
		    [ title(['Dqs API top-level'])
		    ], 
		    [ h2('This is the root directory for the terminus API.'), 
		      p('Please read the documention included with terminus in order to interact with the graph')
		    ]).
