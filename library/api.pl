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
 *  the Free Software Foundation, under version 3 of the License.        *
 *                                                                       *
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
:- use_module(library(http/http_cors)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).

% multipart
:- use_module(library(http/http_multipart_plugin)).

% Authentication library is only half used.
% and Auth is custom, not actually "Basic"
% Results should be cached!
:- use_module(library(http/http_authenticate)).

% Load capabilities library
:- use_module(capabilities).

% woql libraries
:- use_module(woql_compile).

% Default utils
:- use_module(utils).

% Database utils
:- use_module(database_utils).

% Database construction utils
:- use_module(database).

% Frame and document processing
:- use_module(frame).

% JSON manipulation
:- use_module(jsonld).

% JSON Queries
:- use_module(json_woql).

% File processing
:- use_module(file_utils, [terminus_path/1]).

% Validation
:- use_module(validate).

% Dumping turtle
:- use_module(turtle_utils).

% Time travel
:- use_module(time_travel).

:- use_module(prefixes).
:- use_module(sdk).

%%%%%%%%%%%%% API Paths %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Set base location
% We may want to allow this as a setting...
:- multifile http:location/3.
:- dynamic http:location/3.
http:location(root, '/', []).

:- http_handler(root(.), cors_catch(connect_handler(Method)),
                [method(Method),
                 methods([options,get])]).
:- http_handler(root(console), cors_catch(console_handler(Method)),
                [method(Method),
                 methods([options,get])]).
:- http_handler(root(message), cors_catch(message_handler(Method)),
                [method(Method),
                 methods([options,get,post])]).
% Deprecated!
:- http_handler(root(dashboard), cors_catch(console_handler(Method)),
                [method(Method),
                 methods([options,get])]).
:- http_handler(root(DB), cors_catch(db_handler(Method,DB)),
                [method(Method),
                 methods([options,post,delete])]).
:- http_handler(root(DB/schema), cors_catch(schema_handler(Method,DB)),
                [method(Method),
                 time_limit(infinite),
                 methods([options,get,post])]).
:- http_handler(root(DB/frame), cors_catch(frame_handler(Method,DB)),
                [method(Method),
                 methods([options,get])]).
:- http_handler(root(DB/document/DocID), cors_catch(document_handler(Method,DB,DocID)),
                [method(Method),
                 methods([options,get,post,delete])]).
:- http_handler(root(DB/woql), cors_catch(woql_handler(Method,DB)),
                [method(Method),
                 time_limit(infinite),
                 methods([options,get,post,delete])]).
:- http_handler(root(DB/search), cors_catch(search_handler(Method,DB)),
                [method(Method),
                 methods([options,get,post,delete])]).
:- http_handler(root(DB/metadata), cors_catch(metadata_handler(Method,DB)),
                [method(Method),
                 methods([options,get])]).

%%%%%%%%%%%%%%%%%%%% JSON Reply Hackery %%%%%%%%%%%%%%%%%%%%%%

% We want to use cors whenever we're throwing an error.
:- set_setting(http:cors, [*]).

% Evil mechanism for catching, putting CORS headers and re-throwing.
:- meta_predicate cors_catch(1,?).
cors_catch(Goal,Request) :-
    catch(call(Goal, Request),
          E,
          (   cors_enable,
              http_log('~N[Error] ~q~n',[E]),
              customise_error(E)
          )
         ),
    !.
cors_catch(_,_Request) :-
    % Probably should extract the path from Request
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:message' : _{'@type' : 'xsd:string',
                                        '@value' : 'Resource not found'}},
               [status(400)]).

customise_error(syntax_error(M)) :-
    format(atom(OM), '~q', [M]),
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:witnesses' : [_{'@type' : 'vio:ViolationWithDatatypeObject',
                                           'vio:literal' : OM}]},
               [status(400)]).
customise_error(error(syntax_error(M),_)) :-
    format(atom(OM), '~q', [M]),
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:witnesses' : [_{'@type' : 'vio:ViolationWithDatatypeObject',
                                           'vio:literal' : OM}]},
               [status(400)]).
customise_error(error(syntax_error(M))) :-
    format(atom(OM), '~q', [M]),
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:witnesses' : [_{'@type' : 'vio:ViolationWithDatatypeObject',
                                           'vio:literal' : OM}]},
               [status(400)]).
customise_error(error(type_error(T,O),C)) :-
    format(atom(M),'Type error for ~q which should be ~q with context ~q', [O,T,C]),
    format(atom(OA), '~q', [O]),
    format(atom(TA), '~q', [T]),
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:witnesses' : [_{'@type' : 'vio:ViolationWithDatatypeObject',
                                           'vio:message' : M,
                                           'vio:type' : TA,
                                           'vio:literal' : OA}]},
               [status(400)]).
customise_error(graph_sync_error(JSON)) :-
    reply_json(JSON,[status(500)]).
%customise_error((method_not_allowed(
customise_error(http_reply(method_not_allowed(JSON))) :-
    reply_json(JSON,[status(405)]).
customise_error(http_reply(not_found(JSON))) :-
    reply_json(JSON,[status(404)]).
customise_error(http_reply(authorize(JSON))) :-
    reply_json(JSON,[status(401)]).
customise_error(http_reply(not_acceptable(JSON))) :-
    reply_json(JSON,[status(406)]).
customise_error(time_limit_exceeded) :-
    reply_json(_{'terminus:status' : 'teriminus:error',
                 'terminus:message' : 'Connection timed out'
               },
               [status(408)]).
customise_error(error(E)) :-
    format(atom(EM),'Error: ~q', [E]),
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:message' : EM},
               [status(500)]).
customise_error(error(E, CTX)) :-
    format(atom(EM),'Error: ~q in CTX ~q', [E, CTX]),
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:message' : EM},
               [status(500)]).
customise_error(E) :-
    throw(E).

%%%%%%%%%%%%%%%%%%%% Access Rights %%%%%%%%%%%%%%%%%%%%%%%%%

/*
 *  fetch_authorization_data(+Request, -KS) is semi-determinate.
 *
 *  Fetches the HTTP Basic Authorization data
 */
fetch_authorization_data(Request, KS) :-
    (   memberchk(authorization(Text), Request),
        http_authorization_data(Text, basic(_User, Key)),
        coerce_literal_string(Key, KS))
    -> true
    ;  throw(http_reply(method_not_allowed(_{'terminus:status' : 'terminus:failure',
                                             'terminus:message' : "No HTTP BASIC key supplied",
                                             'terminus:object' : 'fetch_authorization_data'}))).

/*
 * authenticate(+Data,+Auth_Obj) is det.
 *
 * This should either bind the Auth_Obj or throw an http_status_reply/4 message.
 */
authenticate(Request, DB, Auth) :-
    fetch_authorization_data(Request, KS),
    (   key_auth(KS, DB, Auth)
    ->  true
    ;   throw(http_reply(authorize(_{'terminus:status' : 'terminus:failure',
                                     'terminus:message' : 'Not a valid key'})))).

verify_access(Auth, DB, Action, Scope) :-
    (   auth_action_scope(Auth, DB, Action, Scope)
    ->  true
    ;   format(atom(M),'Call was: ~q', [verify_access(Auth, Action, Scope)]),
        throw(http_reply(method_not_allowed(_{'terminus:status' : 'terminus:failure',
                                              'terminus:message' : M,
                                              'terminus:object' : 'verify_access'})))).

connection_authorised_user(Request, User, SURI, DB) :-
    fetch_authorization_data(Request, KS),
    (   key_user(KS, DB, User_ID)
    ->  (   authenticate(Request, DB, Auth),
            verify_access(Auth,DB,terminus/get_document,SURI),
            get_user(User_ID, User)
        ->  true
        ;   throw(http_reply(method_not_allowed(_{'terminus:status' : 'terminus:failure',
                                                  'terminus:message' : 'Bad user object',
                                                  'terminus:object' : User_ID}))))
    ;   throw(http_reply(authorize(_{'terminus:status' : 'terminus:failure',
                                     'terminus:message' : 'Not a valid key',
                                     'terminus:object' : KS})))).

%%%%%%%%%%%%%%%%%%%% Response Predicates %%%%%%%%%%%%%%%%%%%%%%%%%

/*
 * reply_with_witnesses(+Resource_URI,+Witnesses) is det.
 *

 */
reply_with_witnesses(Resource_URI, DB, Witnesses) :-
    write_cors_headers(Resource_URI, DB),

    (   Witnesses = []
    ->  reply_json(_{'terminus:status' : 'terminus:success'})
    ;   reply_json(_{'terminus:status' : 'terminus:failure',
                     'terminus:witnesses' : Witnesses},
                   [status(406)])
    ).


%%%%%%%%%%%%%%%%%%%% Connection Handlers %%%%%%%%%%%%%%%%%%%%%%%%%

/**
 * connect_handler(+Method,+Request:http_request) is det.
 */
connect_handler(options,_Request) :-
    config:public_server_url(SURI),
    terminus_database_name(Collection),
    connect(Collection,DB),
    write_cors_headers(SURI, DB),
    format('~n').
connect_handler(get,Request) :-
    config:public_server_url(SURI),
    terminus_database_name(Collection),
    connect(Collection,DB),
    connection_authorised_user(Request,User, SURI, DB),
    write_cors_headers(SURI, DB),
    reply_json(User).

/*
 * console_handler(+Method,+Request) is det.
 */
console_handler(options,_Request) :-
    config:public_server_url(SURI),
    terminus_database_name(Collection),
    connect(Collection,DB),
    write_cors_headers(SURI, DB),
    format('~n').
console_handler(get,_Request) :-
    terminus_path(Path),
    interpolate([Path,'/config/index.html'], Index_Path),
    read_file_to_string(Index_Path, String, []),
    config:public_server_url(SURI),
    terminus_database_name(Collection),
    connect(Collection,DB),
    write_cors_headers(SURI, DB),
    format('~n'),
    write(String).

/*
 * message_handler(+Method,+Request) is det.
 */
message_handler(options,_Request) :-
    config:public_server_url(SURI),
    terminus_database_name(Collection),
    connect(Collection,DB),
    write_cors_headers(SURI, DB),
    format('~n').
message_handler(get,Request) :-
    try_get_param('terminus:message',Request,Message),

    with_output_to(
        string(Payload),
        json_write(current_output, Message, [])
    ),

    http_log('~N[Message] ~s~n',[Payload]),
    reply_json(_{'terminus:status' : 'terminus:success'}).
message_handler(post,R) :-
    add_payload_to_request(R,Request), % this should be automatic.
    try_get_param('terminus:message',Request,Message),

    with_output_to(
        string(Payload),
        json_write(current_output, Message, [])
    ),

    http_log('~N[Message] ~s~n',[Payload]),
    reply_json(_{'terminus:status' : 'terminus:success'}).

/**
 * db_handler(Request:http_request,Method:atom,DB:atom) is det.
 */
db_handler(options,_DB,_Request) :-
    % database may not exist - use server for CORS
    config:public_server_url(SURI),
    terminus_database_name(Collection),
    connect(Collection,DB),
    write_cors_headers(SURI,DB),
    format('~n').
db_handler(post,DB,R) :-
    add_payload_to_request(R,Request), % this should be automatic.
    terminus_database_name(Collection),
    connect(Collection,DBC),
    /* POST: Create database */
    authenticate(Request, DBC, Auth),
    config:public_server_url(Server),
    verify_access(Auth,DBC,terminus/create_database,Server),
    try_get_param('terminus:document',Request,Doc),
    try_db_uri(DB,DB_URI),
    try_create_db(DB,DB_URI,Doc),
    write_cors_headers(Server, DBC),
    reply_json(_{'terminus:status' : 'terminus:success'}).
db_handler(delete,DB,Request) :-
    /* DELETE: Delete database */
    terminus_database_name(Collection),
    connect(Collection,DBC),
    authenticate(Request, DBC, Auth),

    config:public_server_url(Server),

    verify_access(Auth, DBC, terminus/delete_database,Server),

    try_db_uri(DB,DB_URI),
    try_delete_db(DB_URI),

    write_cors_headers(Server, DBC),

    reply_json(_{'terminus:status' : 'terminus:success'}).

% ! woql_handler(+Method:atom, +DB:database, +Request:http_request) is
% det
%
%  @TBD  somebody who knows this code please fill this in
%
woql_handler(options,_DB,_Request) :-
    config:public_server_url(SURI),
    terminus_database_name(Collection),
    connect(Collection,DBC),
    write_cors_headers(SURI, DBC),
    format('~n').
woql_handler(get,DB,Request) :-
    % Should test for read-only query here.
    terminus_database_name(Collection),
    connect(Collection,DBC),
    % Actually we need to pull the query from the request, process it
    % and get a list of necessary capabilities to check.
    authenticate(Request, DBC, Auth),
    try_db_uri(DB,DB_URI),
    verify_access(Auth,DBC,terminus/woql_select,DB_URI),
    try_get_param('terminus:query',Request,Atom_Query),  % TODO - we make an atom here?
    atom_json_dict(Atom_Query, Query, []),

    http_log('~N[Query] ~s~n',[Atom_Query]),

    connect(DB_URI,New_Ctx),
    run_query(Query,New_Ctx,JSON),

    config:public_server_url(SURI),
    write_cors_headers(SURI, DBC),
    reply_json(JSON).
woql_handler(post,DB,R) :-
    add_payload_to_request(R,Request),

    terminus_database_name(Collection),
    connect(Collection,DBC),
    % Actually we need to pull the query from the request, process it
    % and get a list of necessary capabilities to check.
    authenticate(Request, DBC, Auth),

    try_db_uri(DB,DB_URI),

    verify_access(Auth,DBC,terminus/woql_select,DB_URI),

    try_get_param('terminus:query',Request,Atom_Query),
    atom_json_dict(Atom_Query, Query, []),

    http_log('~N[Query] ~s~n',[Atom_Query]),

    connect(DB_URI,New_Ctx),

    collect_posted_files(Request,Files),
    Ctx = [files=Files|New_Ctx],

    (   run_query(Query,Ctx,JSON)
    ->  true
    ;   JSON = _{bindings : []}),

    config:public_server_url(SURI),
    write_cors_headers(SURI, DBC),
    reply_json_dict(JSON).

%!  document_handler(+Mode, +DB, +Doc_ID, +Request:http_request) is det
%
%
document_handler(options,DB,_Doc_ID,_Request) :-
    terminus_database_name(Collection),
    connect(Collection,DBC),
    try_db_uri(DB,DB_URI),
    write_cors_headers(DB_URI, DBC),
    format('~n').
document_handler(get, DB, Doc_ID, Request) :-
    terminus_database_name(Collection),
    connect(Collection,DBC),
    /* Read Document */
    authenticate(Request, DBC, Auth),

    % We should make it so we can pun documents and IDs

    try_db_uri(DB,DB_URI),

    % check access rights
    verify_access(Auth,DBC,terminus/get_document,DB_URI),

    try_db_graph(DB_URI,Database),

    try_doc_uri(DB_URI,Doc_ID,Doc_URI),

    % This feels a bit ugly... but perhaps not
    (   get_param('terminus:encoding',Request,'terminus:frame')
    ->  try_get_filled_frame(Doc_URI,Database,JSON),
        %http_log('Writing Frame JSON-LD:', []),
        %json_write_dict(Log,JSON),
        true
    ;   try_get_document(Doc_URI,Database,JSON)
    ),
    write_cors_headers(DB_URI, DBC),
    reply_json_dict(JSON).
document_handler(post, DB, Doc_ID, R) :-
    add_payload_to_request(R,Request),

    terminus_database_name(Collection),
    connect(Collection,DBC),
    /* Update Document */
    authenticate(Request, DBC, Auth),

    try_db_uri(DB,DB_URI),

    % check access rights
    verify_access(Auth,DBC,terminus/create_document,DB_URI),

    try_db_graph(DB_URI, Database),

    try_get_param('terminus:document',Request,Doc),

    % very hacky!
    interpolate(['doc:',Doc_ID],Doc_URI),

    try_update_document(DBC, Doc_URI,Doc,Database,Witnesses),

    reply_with_witnesses(DB_URI,DBC,Witnesses).
document_handler(delete, DB, Doc_ID, Request) :-
    terminus_database_name(Collection),
    connect(Collection,DBC),
    /* Delete Document */
    authenticate(Request, DBC, Auth),
    % We should make it so we can pun documents and IDs

    try_db_uri(DB,DB_URI),

    % check access rights
    verify_access(Auth,DBC,terminus/delete_document,DB_URI),

    try_db_graph(DB_URI,Database),

    % very hacky!
    interpolate(['doc:',Doc_ID],Doc_URI),

    try_delete_document(Doc_URI,Database,Witnesses),

    reply_with_witnesses(DB_URI,DBC,Witnesses).

/**
 * frame_handler(+Mode, +DB, +Class_ID, +Request:http_request) is det.
 *
 * Establishes frame responses
 */
frame_handler(options,DB,_Request) :-
    terminus_database_name(Collection),
    connect(Collection,DBC),
    try_db_uri(DB,DB_URI),
    write_cors_headers(DB_URI, DBC),
    format('~n'). % send headers
frame_handler(get, DB, Request) :-
    terminus_database_name(Collection),
    connect(Collection,DBC),
    /* Read Document */
    authenticate(Request, DBC, Auth),

    % We should make it so we can pun documents and IDs

    try_db_uri(DB,DB_URI),

    % check access rights
    verify_access(Auth,DBC,terminus/class_frame,DB_URI),

    try_db_graph(DB_URI,Database),

    try_get_param('terminus:class',Request,Class_URI),

    try_class_frame(Class_URI,Database,Frame),

    config:public_server_url(SURI),
    write_cors_headers(SURI, DBC),
    reply_json(Frame).

/*
 * schema_handler(Mode,DB,Request) is det.
 *
 * Get or update a schema.
 */
schema_handler(options,DB,_Request) :-
    terminus_database_name(Collection),
    connect(Collection,DBC),
    try_db_uri(DB,DB_URI),
    write_cors_headers(DB_URI, DBC),
    format('~n'). % send headers
schema_handler(get,DB,Request) :-
    terminus_database_name(Collection),
    connect(Collection,DBC),
    /* Read Document */
    authenticate(Request, DBC, Auth),

    % We should make it so we can pun documents and IDs

    try_db_uri(DB,DB_URI),

    % check access rights
    verify_access(Auth,DBC,terminus/get_schema,DB_URI),

    % Let's do a default schema if we can't find one.
    catch(
        try_get_param('terminus:schema',Request,Name),
        _,
        interpolate([DB_URI,'/schema'],Name)
    ),

    try_dump_schema(DB_URI, DBC, Name, Request).
schema_handler(post,DB,R) :- % should this be put?
    add_payload_to_request(R,Request), % this should be automatic.
    terminus_database_name(Collection),
    connect(Collection,DBC),

    /* Read Document */
    authenticate(Request, DBC, Auth),

    % We should make it so we can pun documents and IDs
    try_db_uri(DB,DB_URI),

    % check access rights
    verify_access(Auth,DBC,terminus/update_schema,DB_URI),

    try_get_param('terminus:schema',Request,Name),
    try_get_param('terminus:turtle',Request,TTL),

    try_update_schema(DB_URI,Name,TTL,Witnesses),

    reply_with_witnesses(DB_URI,DBC,Witnesses).

/* */
metadata_handler(options,DB,_Request) :-
    % database may not exist - use server for CORS
    try_db_uri(DB,DB_URI),
    terminus_database_name(Collection),
    connect(Collection,DBC),
    write_cors_headers(DB_URI, DBC),
    format('~n'). % send headers
metadata_handler(get,DB,Request) :-
    terminus_database_name(Collection),
    connect(Collection,DBC),
    /* Read Document */
    authenticate(Request, DBC, Auth),

    % We should make it so we can pun documents and IDs
    try_db_uri(DB,DB_URI),

    % check access rights
    verify_access(Auth,DBC,terminus/update_schema,DB_URI),

    try_get_metadata(DB_URI,JSON),

    reply_json(JSON).

/********************************************************
 * Determinising predicates used in handlers            *
 *                                                      *
 * It's not fun to fail, so don't!                      *
 ********************************************************/


/*
 * try_get_document(ID, Database, Object) is det.
 *
 * Actually has determinism: det + error
 *
 * Gets document (JSON-LD) associated with ID
 */
try_get_document(ID,Database,Object) :-
    (   document_jsonld(ID,Database,Object)
    ->  true
    ;   format(atom(MSG), 'Document resource ~s can not be found', [ID]),
        throw(http_reply(not_found(_{'terminus:message' : MSG,
                                     'terminus:object' : ID,
                                     'terminus:status' : 'terminus:failure'})))).

/*
 * try_get_document(ID, Database) is det.
 *
 * Actually has determinism: det + error
 *
 * Gets document as filled frame (JSON-LD) associated with ID
 */
try_get_filled_frame(ID,Database,Object) :-
    (   document_filled_class_frame_jsonld(ID,_{},Database,Object)
    ->  true
    ;   format(atom(MSG), 'Document resource ~s can not be found', [ID]),
        throw(http_reply(not_found(_{'terminus:status' : 'terminus:failure',
                                     'terminus:message' : MSG,
                                     'terminus:object' : ID})))).

/*
 * try_delete_document(+ID, +Database, -Witnesses) is det.
 *
 * Actually has determinism: det + error
 *
 * Deletes the object associated with ID, and throws an
 * http error otherwise.
 */
try_delete_document(Pre_Doc_ID, Database, Witnesses) :-
    (   database_name(Database,Collection),
        get_collection_jsonld_context(Collection,Ctx)
    ->  prefix_expand(Pre_Doc_ID,Ctx,Doc_ID)
    ;   format(atom(MSG), 'Document resource ~s could not be expanded', [Pre_Doc_ID]),
        throw(http_reply(not_found(_{'terminus:status' : 'terminus_failure',
                                     'terminus:message' : MSG,
                                     'terminus:object' : Pre_Doc_ID})))),

    (   object_instance_graph(Doc_ID, Database, Document_Graph)
    ->  true
    ;   terminus_database(Terminus_DB),
        default_instance_graph(Terminus_DB, Database, Document_Graph)
    ),

    (   document_transaction(Database, Transaction_DB, Document_Graph,
                             frame:delete_object(Doc_ID,Transaction_DB),
                             Witnesses)
    ->  true
    ;   format(atom(MSG), 'Document resource ~s could not be deleted', [Doc_ID]),
        throw(http_reply(not_found(_{'terminus:status' : 'terminus_failure',
                                     'terminus:message' : MSG,
                                     'terminus:object' : Doc_ID})))).

/*
 * try_update_document(ID, Doc, Database) is det.
 *
 * Actually has determinism: det + error
 *
 * Updates the object associated with ID, and throws an
 * http error otherwise.
 */
try_update_document(Terminus_DB,Doc_ID, Doc_In, Database, Witnesses) :-
    % if there is no id, we'll use the requested one.
    (   jsonld_id(Doc_In,Doc_ID_Match)
    ->  Doc_In = Doc
    %   This is wrong - we need to have the base path here as well.
    ;   put_dict(Doc_ID,'@id',Doc_In,Doc)),

    (   database_name(Database,Collection),
        get_collection_jsonld_context(Collection,Ctx),
        get_key_document('@id',Ctx,Doc,Doc_ID_Match)
    ->  true
    ;   format(atom(MSG),'Unable to match object ids ~q and ~q', [Doc_ID, Doc_ID_Match]),
        throw(http_reply(not_found(_{'terminus:message' : MSG,
                                     'terminus:status' : 'terminus:failure'})))),

    (   object_instance_graph(Doc, Database, Document_Graph)
    ->  true
    ;   default_instance_graph(Terminus_DB, Database, Document_Graph)
    ),

    (   document_transaction(Database, Transaction_DB, Document_Graph,
                             frame:update_object(Doc,Transaction_DB), Witnesses)
    ->  true
    ;   format(atom(MSG),'Unable to update object at Doc_ID: ~q', [Doc_ID]),
        throw(http_reply(not_found(_{'terminus:message' : MSG,
                                     'terminus:status' : 'terminus:failure'})))).

/*
 * try_db_uri(DB,DB_URI) is det.
 *
 * Die if we can't form a document uri.
 */
try_db_uri(DB,DB_URI) :-
    (   config:public_server_url(Server_Name),
        interpolate([Server_Name,'/',DB],DB_URI)
    ->  true
    ;   throw(http_reply(not_found(_{'terminus:message' : 'Database resource can not be found',
                                     'terminus:status' : 'terminus:failure',
                                     'terminus:object' : DB})))).

/*
 * try_doc_uri(DB,Doc,Doc_URI) is det.
 *
 * Die if we can't form a document uri.
 */
try_doc_uri(DB_URI,Doc_ID,Doc_URI) :-
    uri_encoded(path,Doc_ID,Doc_ID_Safe),
    (   interpolate([DB_URI,'/',document, '/',Doc_ID_Safe],Doc_URI)
    ->  true
    ;   format(atom(MSG), 'Document resource ~s can not be constructed in ~s', [DB_URI,Doc_ID]),
        throw(http_reply(not_found(_{'terminus:message' : MSG,
                                     'terminus:status' : 'terminus:failure',
                                     'terminus:object' : DB_URI})))).

/*
 * try_db_graph(+DB:uri,-Database:database) is det.
 *
 * Die if we can't form a graph
 */
try_db_graph(DB_URI,Database) :-
    (   make_database_from_database_name(DB_URI,DBM)
    ->  open_read_transaction(DBM,Database) % let's at least get reads...
    ;   format(atom(MSG), 'Resource ~s can not be found', [DB_URI]),
        throw(http_reply(not_found(_{'terminus:message' : MSG,
                                     'terminus:status' : 'terminus:failure',
                                     'terminus:object' : DB_URI})))).

/*
 * try_get_param(Key,Request:request,Value) is det.
 *
 * Get a parameter from the request independent of request variety.
 */
try_get_param(Key,Request,Value) :-
    % GET or POST (but not application/json)
    memberchk(method(post), Request),
    memberchk(multipart(Parts), Request),
    !,
    (   memberchk(Key=Encoded_Value, Parts)
    ->  uri_encoded(query_value, Value, Encoded_Value)
    ;   format(atom(MSG), 'Parameter resource ~q can not be found in ~q', [Key,Parts]),
        throw(http_reply(not_found(_{'terminus:status' : 'terminus:failure',
                                     'terminus:message' : MSG})))).
try_get_param(Key,Request,Value) :-
    % GET or POST (but not application/json)
    memberchk(method(Method), Request),
    (   memberchk(Method, [get,delete])
    ;   Method = post,
        \+ memberchk(content_type('application/json'), Request)),

    http_parameters(Request, [], [form_data(Data)]),

    (   memberchk(Key=Value,Data)
    ->  true
    ;   format(atom(MSG), 'Parameter resource ~q can not be found in ~q', [Key,Data]),
        throw(http_reply(not_found(_{'terminus:status' : 'terminus:failure',
                                     'terminus:message' : MSG})))),
    !.
try_get_param(Key,Request,Value) :-
    % POST with JSON package
    memberchk(method(post), Request),
    memberchk(content_type('application/json'), Request),

    (   memberchk(payload(Document), Request)
    ->  true
    ;   format(atom(MSG), 'No JSON payload resource ~q for POST ~q', [Key,Request]),
        throw(http_reply(not_found(_{'terminus:status' : 'terminus:failure',
                                     'terminus:message' : MSG})))),

    (   get_key_document(Key, Document, Value)
    ->  true
    ;   format(atom(MSG), 'Parameter resource ~q can not be found in ~q', [Key,Document]),
        throw(http_reply(not_found(_{'terminus:status' : 'terminus:failure',
                                     'terminus:message' : MSG})))),
    !.
try_get_param(Key,Request,_Value) :-
    % OTHER with method
    memberchk(method(Method), Request),
    !,

    format(atom(MSG), 'Method ~q has no parameter key transport for key ~q', [Key,Method]),
    throw(http_reply(not_found(_{'terminus:message' : MSG,
                                 'terminus:status' : 'terminus:failure',
                                 'terminus:object' : Key}))).
try_get_param(Key,_Request,_Value) :-
    % Catch all.
    format(atom(MSG), 'Request has no parameter key transport for key ~q', [Key]),
    throw(http_reply(not_found(_{'terminus:status' : 'terminus:failure',
                                 'terminus:message' : MSG}))).

/*
 * get_param_default(Key,Request:request,Value,Default) is semidet.
 *
 * We can fail with this one, so you better do your own checking.
 */
get_param(Key,Request,Value) :-
    % GET or POST (but not application/json)
    memberchk(method(Method), Request),
    memberchk(Method, [get,delete,post]),
    % The agent is not sending a JSON request type
    \+ memberchk(content_type('application/json'), Request),
    !,

    http_parameters(Request, [], [form_data(Data)]),
    memberchk(Key=Value,Data).
get_param(Key,Request,Value) :-
    % POST with JSON package
    memberchk(method(post), Request),
    memberchk(content_type('application/json'), Request),

    memberchk(payload(Document), Request),
    get_dict(Key, Document, Value).


/*
 * try_create_db(DB,DB_URI,Object) is det.
 *
 * Try to create a database and associate resources
 */
try_create_db(DB,DB_URI,Doc) :-
    % Try to create the database resource first.
    with_mutex(
        DB_URI,
        (       % create the collection if it doesn't exist
            (   database_exists(DB_URI)
            ->  throw(http_reply(method_not_allowed(_{'terminus:status' : 'terminus:failure',
                                                      'terminus:message' : 'Database already exists',
                                                      'terminus:method' : 'terminus:create_database'})))
            ;   true),

            (   add_database_resource(DB,DB_URI,Doc)
            ->  true
            ;   format(atom(MSG), 'You managed to half-create a database we can not delete\n You should look for your local terminus wizard to manually delete it: ~s', [DB_URI]),
                throw(http_reply(not_found(_{'terminus:message' : MSG,
                                             'terminus:status' : 'terminus:failure'})))),

            (   terminus_database_name(Collection),
                connect(Collection,Terminus_DB),
                create_db(Terminus_DB, DB_URI)
            ->  true
            ;   format(atom(MSG), 'Database ~s could not be created', [DB_URI]),
                throw(http_reply(not_found(_{'terminus:message' : MSG,
                                             'terminus:status' : 'terminus:failure'})))),

            (   post_create_db(DB_URI)
            ->  true
            ;   format(atom(MSG), 'Unable to perform post-creation updates: ~s', [DB_URI]),
                throw(http_reply(not_found(_{'terminus:message' : MSG,
                                             'terminus:status' : 'terminus:failure'}))))

        )).

/*
 * try_delete_db(DB_URI) is det.
 *
 * Attempt to delete a database given its URI
 */
try_delete_db(DB_URI) :-
    with_mutex(
        DB_URI,
        (   (   delete_db(DB_URI)
            ->  true
            ;   format(atom(MSG), 'Database ~s could not be destroyed', [DB_URI]),
                throw(http_reply(not_found(_{'terminus:message' : MSG,
                                             'terminus:status' : 'terminus:failure'})))),

            (   delete_database_resource(DB_URI)
            ->  true
            ;   format(atom(MSG), 'Database ~s resource records could not be removed', [DB_URI]),
                throw(http_reply(not_found(_{'terminus:message' : MSG,
                                             'terminus:status' : 'terminus:failure'}))))
        )).

/*
 * try_atom_json(Atom,JSON) is det.
 */
try_atom_json(Atom,JSON) :-
    (   atom_json_dict(Atom, JSON, [])
    ->  true
    ;   format(atom(MSG), 'Malformed JSON Object', []),
        % Give a better error code etc. This is silly.
        throw(http_reply(not_found(_{'terminus:status' : 'terminus:failure',
                                     'terminus:message' : MSG,
                                     'terminus:object' : Atom})))).

/*
 * add_payload_to_request(Request:request,JSON:json) is det.
 *
 * Updates request with JSON-LD payload in payload(Document).
 * This should really be done automatically at request time
 * using the endpoint wrappers so we don't forget to do it.
 */
add_payload_to_request(Request,[multipart(Parts)|Request]) :-
    memberchk(method(post), Request),
    memberchk(content_type(ContentType), Request),
    http_parse_header_value(
        content_type, ContentType,
        media(multipart/'form-data', _)
    ),
    !,

    http_read_data(Request, Parts, [on_filename(save_post_file)]).
add_payload_to_request(Request,[payload(Document)|Request]) :-
    member(method(post), Request),
    member(content_type('application/json'), Request),
    !,
    http_read_data(Request, Document, [json_object(dict)]).
add_payload_to_request(Request,Request).

/*
 * save_post_file(In,File_Spec,Options) is det.
 *
 * Saves a temporary octet stream to a file. Used for multipart
 * file passing via POST.
 */
save_post_file(In, file(Filename, File), Options) :-
    option(filename(Filename), Options),
    setup_call_cleanup(
        tmp_file_stream(octet, File, Out),
        copy_stream_data(In, Out),
        close(Out)
    ).

/*
 * Make a collection of all posted files for
 * use in a Context via WOQL's get/3.
 */
collect_posted_files(Request,Files) :-
    memberchk(multipart(Parts), Request),
    !,
    include([_Token=file(_Name,_Storage)]>>true,Parts,Files).
collect_posted_files(_Request,[]).

/*
 * try_class_frame(Class,Database,Frame) is det.
 */
try_class_frame(Class,Database,Frame) :-
    (   class_frame_jsonld(Class,Database,Frame)
    ->  true
    ;   format(atom(MSG), 'Class Frame could not be json-ld encoded for class ~s', [Class]),
        % Give a better error code etc. This is silly.
        throw(http_reply(not_found(_{ 'terminus:message' : MSG,
                                      'terminus:status' : 'terminus:failure',
                                      'terminus:class' : Class})))).

/*
 * try_dump_schema(DB_URI, Request) is det.
 *
 * Write schema to current stream
 */
try_dump_schema(DB_URI, DB, Name, Request) :-
    with_mutex(
        DB_URI,
        (
            try_get_param('terminus:encoding', Request, Encoding),
            (   coerce_literal_string(Encoding, ES),
                atom_string('terminus:turtle',ES)
            ->  with_output_to(
                    string(String),
                    (   current_output(Stream),
                        graph_to_turtle(DB_URI, Name, Stream)
                    )
                ),
                config:public_server_url(SURI),
                write_cors_headers(SURI, DB),
                reply_json(String)
            ;   format(atom(MSG), 'Unimplemented encoding ~s', [Encoding]),
                % Give a better error code etc. This is silly.
                throw(http_reply(method_not_allowed(_{'terminus:message' : MSG,
                                                      'terminus:object' : DB_URI,
                                                      'terminus:status' : 'terminus:failure'})))
            )
        )
    ).

/*
 * try_update_schema(+DB_URI,+Schema_Name,+TTL,-Witnesses) is det.
 *
 */
try_update_schema(DB_URI,Schema_Name,TTL,Witnesses) :-
    coerce_literal_string(Schema_Name, Schema_String),
    atom_string(Schema_Atom, Schema_String),
    coerce_literal_string(TTL, TTLS),
    make_database_from_database_name(DB_URI, Database),
    setup_call_cleanup(
        open_string(TTLS, TTLStream),
        turtle_schema_transaction(Database, Schema_Atom, TTLStream, Witnesses),
        close(TTLStream)
    ).

/*
 * try_get_metadata(+DB_URI,+Name,+TTL,-Witnesses) is det.
 *
 *
 */
try_get_metadata(DB_URI,JSON) :-
    (   db_size(DB_URI,Size)
    ->  true
    ;   Size = 0),

    (   db_modified_datetime(DB_URI,Modified_DT)
    ->  true
    ;   Modified_DT = "1970-01-01T00:00"
    ),

    (   db_created_datetime(DB_URI,Created_DT)
    ->  true
    ;   Created_DT = "1970-01-01T00:00"
    ),

    get_collection_jsonld_context(DB_URI,Ctx),

    JSON = _{'@context' : Ctx,
             '@type' : 'terminus:DatabaseMetadata',
             'terminus:database_modified_time' : _{'@value' : Modified_DT,
                                                   '@type' : 'xsd:dateTime'},
             'terminus:database_created_time' : _{'@value' : Created_DT,
                                                  '@type' : 'xsd:dateTime'},
             'terminus:database_size' : _{'@value' : Size,
                                          '@type' : 'xsd:nonNegativeInteger'}}.
