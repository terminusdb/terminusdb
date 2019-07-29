:- module(api,[]).

/** <module> HTTP API
 * 
 * The Regulum DB API interface.
 * 
 * A RESTful endpoint inventory for weilding the full capabilities of the 
 * regulumDB. 
 *
 * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of RegulumDB.                                      *
 *                                                                       *
 *  RegulumDB is free software: you can redistribute it and/or modify    *
 *  it under the terms of the GNU General Public License as published by *
 *  the Free Software Foundation, either version 3 of the License, or    *
 *  (at your option) any later version.                                  *
 *                                                                       *
 *  RegulumDB is distributed in the hope that it will be useful,         *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
 *  GNU General Public License for more details.                         *
 *                                                                       *
 *  You should have received a copy of the GNU General Public License    *
 *  along with RegulumDB.  If not, see <https://www.gnu.org/licenses/>.  *
 *                                                                       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

% http libraries
:- use_module(library(http/http_log)).
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

% Load capabilities library
:- use_module(library(capabilities)).

% woql libraries
:- use_module(library(woql_compile)).

% Default utils
:- use_module(library(utils)).

%% Set base location
% We may want to allow this as a setting...
http:location(root, '/', []).

:- http_handler(root(.), connect_handler, []). 
:- http_handler(root(DB), db_handler(Method,DB),
                [method(Method),
                 methods([post,delete])]).
:- http_handler(root(DB/schema), schema_handler(Method,DB), 
                [method(Method),
                 methods([get,post])]).
:- http_handler(root(DB/document/DocID), document_handler(Method,DB,DocID),
                [method(Method),
                 methods([get,post,delete])]). 
:- http_handler(root(DB/woql), woql_handler(Method,DB),
                [method(Method),
                 methods([get,post,delete])]). 
:- http_handler(root(DB/search), search_handler(Method,DB),
                [method(Method),
                 methods([get,post,delete])]). 

/** 
 * connect_handler(Request:http_request) is det.
 */
connect_handler(Request) :-
    http_parameters(Request, [], [form_data(Data)]),

    get_key(key, Data, Connection_Key),
    key_auth(Connection_Key,Auth),
    
    format('Content-type: application/json~n~n'),
    current_output(Out),
	json_write_dict(Out,Auth).


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
    
    get_key(query,Data,Query),
    http_log_stream(Log),
    run_query(Query, JSON),
    * format(Log,'Query: ~q~nResults in: ~q~n',[Query,JSON]),
    format('Content-type: application/json~n~n'),
    current_output(Out),
	json_write(Out,JSON).

