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
:- use_module(library(http/http_cors)). 
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

% Database construction utils
:- use_module(library(database)).

% Frame and document processing
:- use_module(library(frame)).

% JSON manipulation
:- use_module(library(jsonld)).

% JSON Queries
:- use_module(library(json_woql)).

% File processing - especially for turtle
:- use_module(library(file_utils), [checkpoint_to_turtle/3]).

% Validation
:- use_module(library(validate)).

% Checkpointing...
:- use_module(library(triplestore), [checkpoint/2]).

%%%%%%%%%%%%% API Paths %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Set base location
% We may want to allow this as a setting...
http:location(root, '/', []).

:- http_handler(root(.), cors_catch(connect_handler(Method)),
                [method(Method),
                 methods([get,post])]). 
:- http_handler(root(DB), cors_catch(db_handler(Method,DB)),
                [method(Method),
                 methods([options,post,delete])]).
:- http_handler(root(DB/schema), cors_catch(schema_handler(Method,DB)), 
                [method(Method),
                 methods([options,get,post])]).
:- http_handler(root(DB/frame), cors_catch(frame_handler(Method,DB)), 
                [method(Method),
                 methods([options,get])]).
:- http_handler(root(DB/document/DocID), cors_catch(document_handler(Method,DB,DocID)),
                [method(Method),
                 methods([options,get,post,delete])]). 
:- http_handler(root(DB/woql), cors_catch(woql_handler(Method,DB)),
                [method(Method),
                 methods([options,get,post,delete])]). 
:- http_handler(root(DB/search), cors_catch(search_handler(Method,DB)),
                [method(Method),
                 methods([options,get,post,delete])]). 

%%%%%%%%%%%%%%%%%%%% JSON Reply Hackery %%%%%%%%%%%%%%%%%%%%%%

% We want to use cors whenever we're throwing an error.
:- set_setting(http:cors, [*]).

% Evil mechanism for catching, putting CORS headers and re-throwing.
:- meta_predicate cors_catch(0,?).
cors_catch(M:Goal,Request) :-
    Goal =.. [Handler|Args],
    append(Args,[Request],New_Args),
    NewGoal =.. [Handler|New_Args],
    catch(call(M:NewGoal),
          E,
          (   cors_enable,
              throw(E)
          )
         ).


%%%%%%%%%%%%%%%%%%%% Access Rights %%%%%%%%%%%%%%%%%%%%%%%%%

/* 
 * authenticate(+Data,+Auth_Obj) is det. 
 * 
 * This should either bind the Auth_Obj or throw an http_status_reply/4 message. 
 */
authenticate(Request, Auth) :-
    try_get_param('terminus:user_key',Request,Key),
    
    (   key_auth(Key, Auth)
    ->  true
    ;   throw(http_reply(authorise(_{'terminus:status' : 'terminus:failure',
                                     'terminus:message' : 'Not a valid key',
                                     'terminus:object' : Key})))).

verify_access(Auth, Action, Scope) :-
    * http_log_stream(Log),
    * format(Log,'Goal: ~q~n',[auth_action_scope(Auth, Action, Scope)]),
    (   auth_action_scope(Auth, Action, Scope)
    ->  true
    ;   format(atom(M),'Call was: ~q', [verify_access(Auth, Action, Scope)]),
        throw(http_reply(method_not_allowed(_{'terminus:status' : 'terminus:failure',
                                              'terminus:message' : M,
                                              'terminus:object' : 'verify_access'})))).

connection_authorised_user(Request, User) :-
    try_get_param('terminus:user_key',Request,Key),
    coerce_literal_string(Key, KS),
    
    (   key_user(KS, User_ID)
    ->  (   get_user(User_ID, User)
        ->  true
        ;   throw(http_reply(method_not_allowed(_{'terminus:status' : 'terminus:failure',
                                                  'terminus:message' : 'Bad user object',
                                                  'terminus:object' : User_ID}))))
    ;   throw(http_reply(authorise(_{'terminus:status' : 'terminus:failure',
                                     'terminus:message' : 'Not a valid key',
                                     'terminus:object' : KS})))).

%%%%%%%%%%%%%%%%%%%% Response Predicates %%%%%%%%%%%%%%%%%%%%%%%%%

/* 
 * reply_with_witnesses(+Resource_URI,+Witnesses) is det. 
 * 
 * 
 */
reply_with_witnesses(Resource_URI, Witnesses) :-
    write_cors_headers(Resource_URI),

    (   Witnesses = []
    ->  reply_json(_{'terminus:status' : 'terminus:success'})
    ;   reply_json(_{'terminus:status' : 'terminus:failure',
                     'terminus:witnesses' : Witnesses},
                   [code(406)])
    ).


%%%%%%%%%%%%%%%%%%%% Connection Handlers %%%%%%%%%%%%%%%%%%%%%%%%%

/** 
 * connect_handler(Request:http_request) is det.
 */
connect_handler(options,_Request) :-
    config:server(SURI),
    write_cors_headers(SURI),
    format('~n').
connect_handler(get,Request) :-
    connection_authorised_user(Request,User),

    config:server(SURI),
    write_cors_headers(SURI),
    reply_json(User).


/** 
 * db_handler(Request:http_request,Method:atom,DB:atom) is det.
 */
db_handler(options,_DB,_Request) :-
    % database may not exist - use server for CORS
    config:server(SURI),
    write_cors_headers(SURI),
    format('~n').
db_handler(post,DB,R) :-
    add_payload_to_request(R,Request), % this should be automatic.

    http_log_stream(Log),
    format(Log,'About to authenticate~n',[]),

    /* POST: Create database */
    authenticate(Request, Auth),

    format(Log,'Authenticaticated~n',[]),

    config:server(Server),
    
    verify_access(Auth,terminus/create_database,Server),

    format(Log,'Access verified~n',[]),
    
    try_get_param('terminus:document',Request,Doc),

    format(Log,'Doc ~q~n',[Doc]),

    try_db_uri(DB,DB_URI),
    try_create_db(DB,DB_URI,Doc),

    format(Log,'Database Constructed ~q~n',[DB_URI]),
        
    config:server(SURI),
    write_cors_headers(SURI),
    reply_json(_{'terminus:status' : 'terminus:success'}).
db_handler(delete,DB,Request) :-
    /* DELETE: Delete database */
    authenticate(Request, Auth),

    config:server(Server),
    
    verify_access(Auth,terminus/delete_database,Server),
    
    try_db_uri(DB,DB_URI),
    try_delete_db(DB_URI),

    config:server(SURI),
    write_cors_headers(SURI),
    
    reply_json(_{'terminus:status' : 'terminus:success'}).
    
/** 
 * woql_handler(+Request:http_request) is det.
 */
woql_handler(options,_DB,_Request) :-
    config:server(SURI),
    write_cors_headers(SURI),
    format('~n').
woql_handler(get,DB,Request) :-
    % Actually we need to pull the query from the request, process it
    % and get a list of necessary capabilities to check.
    authenticate(Request, Auth),    

    try_db_uri(DB,DB_URI),

    verify_access(Auth,terminus/woql_select,DB_URI),

    try_get_param('terminus:query',Request,Atom_Query),
    atom_json_dict(Atom_Query, Query, []),
   
    * http_log_stream(Log),
    
    run_query(Query, JSON),

    * format(Log,'Query: ~q~nResults in: ~q~n',[Query,JSON]),

    config:server(SURI),
    write_cors_headers(SURI),
    reply_json(JSON).

/** 
 * document_handler(+Mode, +DB, +Doc_ID, +Request:http_request) is det.
 */
document_handler(options,DB,_Doc_ID,_Request) :-
    try_db_uri(DB,DB_URI),
    write_cors_headers(DB_URI),
    format('~n').
document_handler(get, DB, Doc_ID, Request) :-
    /* Read Document */
    authenticate(Request, Auth),

    % We should make it so we can pun documents and IDs

    try_db_uri(DB,DB_URI),

    % check access rights
    verify_access(Auth,terminus/get_document,DB_URI),

    try_db_graph(DB_URI,Database),

    try_doc_uri(DB_URI,Doc_ID,Doc_URI),

    % This feels a bit ugly... but perhaps not
    (   get_param('terminus:encoding',Request,'terminus:frame')
    ->  try_get_filled_frame(Doc_URI,Database,JSON),        
        %http_log_stream(Log),
        %format(Log, 'Writing Frame JSON-LD:', []),
        %json_write_dict(Log,JSON),
        true
    ;   try_get_document(Doc_URI,Database,JSON)),

    write_cors_headers(DB_URI),

    reply_json(JSON).

document_handler(post, DB, Doc_ID, R) :-
    add_payload_to_request(R,Request),
    
    /* Update Document */
    authenticate(Request, Auth),

    try_db_uri(DB,DB_URI),

    % check access rights
    verify_access(Auth,terminus/create_document,DB_URI),

    try_db_graph(DB_URI, Database),
    
    try_get_param('terminus:document',Request,Doc),

    % very hacky!
    interpolate(['doc:',Doc_ID],Doc_URI),

    try_update_document(Doc_URI,Doc,Database,Witnesses),

    reply_with_witnesses(DB_URI,Witnesses).
document_handler(delete, DB, Doc_ID, Request) :-
    /* Delete Document */
    authenticate(Request, Auth),
    % We should make it so we can pun documents and IDs

    try_db_uri(DB,DB_URI),
    
    % check access rights
    verify_access(Auth,terminus/delete_document,DB_URI),

    try_db_graph(DB_URI,Database),

    % very hacky!
    interpolate(['doc:',Doc_ID],Doc_URI),

    try_delete_document(Doc_URI,Database,Witnesses),

    reply_with_witnesses(DB_URI,Witnesses).

/** 
 * frame_handler(+Mode, +DB, +Class_ID, +Request:http_request) is det.
 * 
 * Establishes frame responses 
 */
frame_handler(options,DB,_Request) :-
    try_db_uri(DB,DB_URI),
    write_cors_headers(DB_URI),
    format('~n'). % send headers
frame_handler(get, DB, Request) :-
    /* Read Document */
    authenticate(Request, Auth),

    % We should make it so we can pun documents and IDs

    try_db_uri(DB,DB_URI),

    % check access rights
    verify_access(Auth,terminus/class_frame,DB_URI),

    try_db_graph(DB_URI,Database),

    try_get_param('terminus:class',Request,Class_URI),

    try_class_frame(Class_URI,Database,Frame),

    config:server(SURI),
    write_cors_headers(SURI),
    reply_json(Frame).

/* 
 * schema_handler(Mode,DB,Request) is det. 
 * 
 * Get or update a schema.
 */ 
schema_handler(options,DB,_Request) :-
    try_db_uri(DB,DB_URI),
    write_cors_headers(DB_URI),
    format('~n'). % send headers
schema_handler(get,DB,Request) :-
    /* Read Document */
    authenticate(Request, Auth),

    % We should make it so we can pun documents and IDs

    try_db_uri(DB,DB_URI),

    % check access rights
    verify_access(Auth,terminus/get_schema,DB_URI),

    % Let's do a default schema if we can't find one.
    catch(
        try_get_param('terminus:schema',Request,Name),
        _,
        interpolate([DB_URI,'/schema'],Name)
    ),
    
    try_dump_schema(DB_URI, Name, Request).
schema_handler(post,DB,R) :- % should this be put?
    add_payload_to_request(R,Request), % this should be automatic.
    
    /* Read Document */
    authenticate(Request, Auth),

    % We should make it so we can pun documents and IDs
    try_db_uri(DB,DB_URI),

    % check access rights
    verify_access(Auth,terminus/update_schema,DB_URI),

    try_get_param('terminus:schema',Request,Name),
    try_get_param('terminus:turtle',Request,TTL),
    
    try_update_schema(DB_URI,Name,TTL,Witnesses),
    
    reply_with_witnesses(DB_URI,Witnesses).

    
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
try_delete_document(Doc_ID, Database, Witnesses) :-
    (   object_instance_graph(Doc_ID, Database, Document_Graph)
    ->  true
    ;   default_instance_graph(Database, Document_Graph)
    ),
    
    (   document_transaction(Database, Document_Graph,
                             frame:delete_object(Doc_ID,Database), Witnesses)
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
try_update_document(Doc_ID, Doc_In, Database, Witnesses) :-
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
    ;   default_instance_graph(Database, Document_Graph)
    ),
    
    % We probably need to be able to pass the document graph as a parameter
    (   document_transaction(Database, Document_Graph, frame:update_object(Doc,Database), Witnesses)
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
    (   config:server(Server_Name),
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
    (   interpolate([DB_URI,'/',document, '/',Doc_ID],Doc_URI)
    ->  true
    ;   format(atom(MSG), 'Document resource ~s can not be constructed in ~s', [DB_URI,Doc_ID]),
        throw(http_reply(not_found(_{'terminus:message' : MSG,
                                     'terminus:status' : 'terminus:failure',
                                     'terminus:object' : DB_URI})))).
    
/* 
 * try_db_graph(+DB:uri,-Database:database is det. 
 * 
 * Die if we can't form a graph
 */
try_db_graph(DB_URI,Database) :-
    (   make_database_from_database_name(DB_URI,Database)
    ->  true
    ;   format(atom(MSG), 'Resource ~s can not be found', [DB_URI]),
        throw(http_reply(not_found(_{'terminus:message' : MSG,
                                     'terminus:status' : 'terminus:failure',
                                     'terminus:object' : DB_URI})))).

/* 
 * try_get_param(Key,Request:request,Value) is det.
 * 
 */
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
        (   (   add_database_resource(DB,DB_URI,Doc)
            ->  true
            ;   format(atom(MSG), 'You managed to half-create a database we can not delete\n You should look for your local terminus wizard to manually delete it: ~s', [DB_URI]),
                throw(http_reply(not_found(_{'terminus:message' : MSG,
                                             'terminus:status' : 'terminus:failure'})))),

            (   http_log_stream(Log),
                format(Log,'~n~q~n',[create_db(DB_URI)]),
                create_db(DB_URI)
            ->  true
            ;   format(atom(MSG), 'Database ~s could not be created', [DB_URI]),
                throw(http_reply(not_found(_{'terminus:message' : MSG,
                                             'terminus:status' : 'terminus:failure'}))))
            
        )).

/* 
 * try_create_db(DB_URI,Object) is det.
 * 
 */
try_delete_db(DB_URI) :-
    with_mutex(
        DB_URI, 
        (   (   delete_database_resource(DB_URI)
            ->  true
            ;   format(atom(MSG), 'Database ~s resource records could not be removed', [DB_URI]),
                throw(http_reply(not_found(_{'terminus:message' : MSG,
                                             'terminus:status' : 'terminus:failure'})))),

            (   delete_db(DB_URI)
            ->  true
            ;   format(atom(MSG), 'Database ~s could not be destroyed', [DB_URI]),
                throw(http_reply(not_found(_{'terminus:message' : MSG,
                                             'terminus:status' : 'terminus:failure'})))
            )
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
add_payload_to_request(Request,[payload(Document)|Request]) :-
    member(method(post), Request),
    member(content_type('application/json'), Request),
    !,
    
    http_read_data(Request, Document, [json_object(dict)]).
add_payload_to_request(Request,Request).

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
try_dump_schema(DB_URI, Name, Request) :-
    with_mutex(
        DB_URI,
        (
            try_get_param('terminus:encoding', Request, Encoding),
            (   coerce_literal_string(Encoding, ES),
                atom_string('terminus:turtle',ES)
            ->  checkpoint(DB_URI, Name),
                checkpoint_to_turtle(DB_URI, Name, TTL_File),
                read_file_to_string(TTL_File, String, []),
                delete_file(TTL_File),
                config:server(SURI),
                write_cors_headers(SURI),
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
 * try_update_schema(+DB_URI,+Name,+TTL,-Witnesses) is det.
 *
 */
try_update_schema(DB_URI,Name,TTL,Witnesses) :-
    coerce_literal_string(Name, NS),
    atom_string(NA, NS),
    coerce_literal_string(TTL, TTLS),
    make_database_from_database_name(DB_URI, Database),
    setup_call_cleanup(
        open_string(TTLS, TTLStream),
        schema_transaction(Database, NA, TTLStream, Witnesses),
        close(TTLStream)
    ).     
