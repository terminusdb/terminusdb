:- module(api,[]).

/** <module> HTTP API
 * 
 * The Terminus DB API interface.
 * 
 * A RESTful endpoint inventory for weilding the full capabilities of the 
 * terminusDB. 
 *
 * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of TerminusDB.                                     *
 *                                                                       *
 *  TerminusDB is free software: you can redistribute it and/or modify   *
 *  it under the terms of the GNU General Public License as published by *
 *  the Free Software Foundation, either version 3 of the License, or    *
 *  (at your option) any later version.                                  *
 *                                                                       *
 *  TerminusDB is distributed in the hope that it will be useful,        *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
 *  GNU General Public License for more details.                         *
 *                                                                       *
 *  You should have received a copy of the GNU General Public License    *
 *  along with TerminusDB.  If not, see <https://www.gnu.org/licenses/>. *
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

% Database utils
:- use_module(library(database_utils)).

% Graph construction utils
:- use_module(library(collection)).

% Frame and entity processing
:- use_module(library(frame)).

% JSON manipulation
:- use_module(library(json_ld)).

%% Set base location
% We may want to allow this as a setting...
http:location(root, '/', []).

%%%%%%%%%%%%% API Paths %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- http_handler(root(.), connect_handler, [methods([get,post])]). 
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

%%%%%%%%%%%%%%%%%%%% Access Rights %%%%%%%%%%%%%%%%%%%%%%%%%

/* 
 * verify_capability(+Request:http_request,+Capability) is det. 
 * 
 * This should either be true or throw an http_reply message. 
 */
% verify_capability(Request,Capability) :-
%     request_key_hash(Request,Key)

%     (   key_user(Key_Hash, User)
%     ->  true
%     ;   throw(http_reply(authorise('Not a valid key')))),

%     (   user_action(User,Capability)
%     ->  true
%     ;   throw(http_reply(method_not_allowed(Capability)))).

request_key(Request,Key) :-
    http_parameters(Request, [], [form_data(Data)]),

    (   memberchk('terminus:user_key'=Key, Data)
    ->  true
    ;   throw(http_reply(authorise('No key supplied')))).

/* 
 * authenticate(+Data,+Auth_Obj) is det. 
 * 
 * This should either bind the Auth_Obj or throw an http_status_reply/4 message. 
 */
authenticate(Request, Auth) :-
    request_key(Request,Key),
    (   key_auth(Key, Auth)
    ->  true
    ;   throw(http_reply(authorise('Not a valid key')))).

verify_access(Auth, Action, Scope) :-
    (   auth_action_scope(Auth, Action, Scope)
    ->  true
    ;   format(atom(M),'Call was: ~q', [verify_access(Auth, Action, Scope)]),
        throw(http_reply(method_not_allowed(M,Scope)))).

connection_authorised_user(Request, User) :-
    request_key(Request,Key),
    (   key_user(Key, User_ID)
    ->  (   get_user(User_ID, User)
        ->  true
        ;   throw(http_reply(method_not_allowed('Bad user object', User_ID))))
    ;   throw(http_reply(authorise('Not a valid key')))).

%%%%%%%%%%%%%%%%%%%% Connection Handlers %%%%%%%%%%%%%%%%%%%%%%%%%

/** 
 * connect_handler(Request:http_request) is det.
 */
connect_handler(Request) :-
    connection_authorised_user(Request,User),

    config:server_name(SURI),
    write_cors_headers(SURI),
    format('Content-type: application/json~n~n'),
    current_output(Out),
	json_write_dict(Out,User).

/** 
 * db_handler(Request:http_request,Method:atom,DB:atom) is det.
 */
db_handler(post,DB,Request) :-
    /* POST: Create database */
    authenticate(Request, Auth),
    
    verify_access(Auth,terminus/create_database,terminus/server),

    try_get_param(document,Request,Doc),

    try_db_uri(DB,DB_URI),
    try_create_db(DB_URI,Doc),

    config:server_name(SURI),
    write_cors_headers(SURI),
    format('Content-type: application/json~n~n'),
    
    current_output(Out),
	json_write_dict(Out,_{'terminus:status' : 'terminus:success'}).
db_handler(delete,DB,Request) :-
    /* DELETE: Delete database */
    authenticate(Request, Auth),
    
    verify_access(Auth,terminus/delete_database,terminus/server),
    
    try_db_uri(DB,DB_URI),
    try_delete_db(DB_URI),

    config:server_name(SURI),
    write_cors_headers(SURI),
    format('Content-type: application/json~n~n'),
    current_output(Out),
	json_write_dict(Out,_{'terminus:status' : 'terminus:success'}).
    
/** 
 * woql_handler(+Request:http_request) is det.
 */ 
woql_handler(Request) :-
    authenticate(Request, Auth),

    verify_access(Auth,terminus/woql_select,terminus/server),

    try_get_param(query,Request,Query),

    * http_log_stream(Log),
    run_query(Query, JSON),
    * format(Log,'Query: ~q~nResults in: ~q~n',[Query,JSON]),

    config:server_name(SURI),
    write_cors_headers(SURI),
    format('Content-type: application/json~n~n'),
    current_output(Out),
	json_write_dict(Out,JSON).

/** 
 * document_handler(+Mode, +DB, +Doc_ID, +Request:http_request) is det.
 */ 
document_handler(get, DB, Doc_ID, Request) :-
    /* Read Document */
    authenticate(Request, Auth),

    % We should make it so we can pun documents and IDs

    try_db_uri(DB,DB_URI),

    % check access rights
    verify_access(Auth,terminus/get_document,DB_URI),

    try_db_graph(DB_URI,Graph),

    try_doc_uri(DB_URI,Doc_ID,Doc_URI),

    try_get_document(Doc_URI,Graph,JSON),

    write_cors_headers(DB_URI),
    format('Content-type: application/json~n~n'),

    current_output(Out),
    json_write_dict(Out,JSON).
document_handler(post, DB, Doc_ID, Request) :-
    /* Update Document */
    authenticate(Request, Auth),
    % We should make it so we can pun documents and IDs

    try_db_uri(DB,DB_URI),

    % check access rights
    verify_access(Auth,terminus/create_document,DB_URI),

    try_db_graph(DB_URI, Graph),
    
    try_get_param(document,Request,Doc),
        
    try_update_document(Doc_ID,Doc,Graph),

    write_cors_headers(DB_URI),
    format('Content-type: application/json~n~n'),
    current_output(Out),
	json_write_dict(Out,_{'terminus:status' : 'terminus:success'}).
document_handler(delete, DB, Doc_ID, Request) :-
    /* Delete Document */
    authenticate(Request, Auth),
    % We should make it so we can pun documents and IDs

    try_db_uri(DB,DB_URI),
    
    % check access rights
    verify_access(Auth,terminus/delete_document,DB_URI),

    try_db_graph(DB_URI,Graph),

    try_doc_uri(DB_URI,Doc_ID,Doc_URI),

    try_delete_document(Doc_URI,Graph),

    write_cors_headers(DB_URI),
    format('Content-type: application/json~n~n'),
    current_output(Out),
	json_write_dict(Out,_{'terminus:status' : 'terminus:success'}).

/* 
 * try_get_document(ID, Graph) is det.
 * 
 * Actually has determinism: det + error
 * 
 * Gets document associated with ID
 */
try_get_document(ID,Graph,Object) :-
    (   entity_jsonld(ID,Graph,Object)
    ->  true
    ;   format(atom(MSG), 'Document resource ~s can not be found', [ID]),
        throw(http_reply(not_found(ID,MSG)))).

/* 
 * try_delete_document(ID, Graph) is det.
 * 
 * Actually has determinism: det + error
 * 
 * Deletes the object associated with ID, and throws an 
 * http error otherwise.
 */
try_delete_document(Doc_ID, Graph) :-
    (   delete_object(Doc_ID,Graph)
    ->  true
    ;   format(atom(MSG), 'Document resource ~s could not be deleted', [Doc_ID]),
        throw(http_reply(not_found(Doc_ID,MSG)))).

/* 
 * try_update_document(ID, Doc, Graph) is det.
 * 
 * Actually has determinism: det + error
 * 
 * Updates the object associated with ID, and throws an 
 * http error otherwise.
 */
try_update_document(Doc_ID, Doc_In, Graph) :-
    % if there is no id, we'll use the requested one.
    (   jsonld_id(Doc_In,Doc_ID_Match)
    ->  true
    %   This is wrong - we need to have the base path here as well. 
    ;   put_dict(Doc_ID,'@id',Doc_In,Doc)),
    
    (   Doc_ID_Match = Doc_ID
    ->  true
    ;   format(atom(MSG),'Unable to match object ids ~q and ~q', [Doc_ID, Doc_ID_Match]),
        throw(http_reply(not_found(Doc_ID,MSG)))),
        
    (   update_object(Doc, Graph)    
    ->  true
    ;   throw(http_reply(not_found(Doc_ID,'Unable to get object at Doc_ID')))).

/* 
 * try_db_uri(DB,DB_URI) is det. 
 * 
 * Die if we can't form a document uri. 
 */
try_db_uri(DB,DB_URI) :- 
    (   config:server_name(Server_Name),
        interpolate([Server_Name,'/',DB],DB_URI)
    ->  true
    ;   throw(http_reply(not_found(DB,'Database resource can not be found')))).

/* 
 * try_doc_uri(DB,Doc,Doc_URI) is det. 
 * 
 * Die if we can't form a document uri. 
 */
try_doc_uri(DB_URI,Doc_ID,Doc_URI) :- 
    (   interpolate([DB_URI,'/',document, '/',Doc_ID],Doc_URI)
    ->  true
    ;   format(atom(MSG), 'Document resource ~s can not be constructed in ~s', [DB_URI,Doc_ID]),
        throw(http_reply(not_found(Doc_ID,MSG)))).
    
/* 
 * try_db_graph(+DB:uri,-Graph:graph) is det. 
 * 
 * Die if we can't form a graph
 */
try_db_graph(DB_URI,Graph) :-
    (   make_collection_graph(DB_URI,Graph)
    ->  true
    ;   format(atom(MSG), 'Resource ~s can not be found', [DB_URI]),
        throw(http_reply(not_found(DB_URI,MSG)))).

/* 
 * try_get_param(Key,Request:request,Value) is det.
 * 
 */
try_get_param(Key,Request,Value) :-
    http_parameters(Request, [], [form_data(Data)]),
    
    (   memberchk(Key=Value,Data)
    ->  true
    ;   format(atom(MSG), 'Parameter resource ~s can not be found in ~s', [Key,Data]),
        throw(http_reply(not_found(Data,MSG)))).

/* 
 * try_create_db(DB_URI,Object) is det.
 * 
 */
try_create_db(DB_URI,Doc) :-
    (   create_db(DB_URI)
    ->  true
    ;   format(atom(MSG), 'Database ~s could not be created', [DB_URI]),
        throw(http_reply(not_found(DB_URI,MSG)))),

    (   add_database_resource(DB_URI,Doc)
    ->  true
    ;   format(atom(MSG), 'Database metadata could not be created: ~s', [DB_URI]),
        throw(http_reply(not_found(DB_URI,MSG)))).


/* 
 * try_create_db(DB_URI,Object) is det.
 * 
 */
try_delete_db(DB_URI) :-
    (   delete_db(DB_URI)
    ->  true
    ;   format(atom(MSG), 'Database ~s could not be destroyed', [DB_URI]),
        throw(http_reply(not_found(DB_URI,MSG)))).



    
