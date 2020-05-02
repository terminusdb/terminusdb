:- module(routes,[]).

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

%% TODO: this module should really only need things from core/api and maybe core/account.

:- reexport(core(util/syntax)).
:- use_module(core(util)).
:- use_module(core(triple)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(core(account)).

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

% Conditional loading of the JWT IO library...
% TODO: There must be a cleaner way to do this
:- (   config:jwt_public_key_path(JWTPubKeyPath),
       JWTPubKeyPath = ''
   ->  true
   ;   use_module(library(jwt_io))).

% Suppress warnings
:- dynamic jwt_decode/3.

%%%%%%%%%%%%% API Paths %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Set base location
% We may want to allow this as a setting...
:- multifile http:location/3.
:- dynamic http:location/3.
http:location(root, '/', []).

%%%%%%%%%%%%%%%%%%%% Connection Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(.), cors_catch(connect_handler(Method)),
                [method(Method),
                 methods([options,get])]).

/**
 * connect_handler(+Method,+Request:http_request) is det.
 */
/* NOTE: Need to return list of databases and access rights */
connect_handler(options,_Request) :-
    % TODO: What should this be?
    % Do a search for each config:public_server_url
    % once you know.
    config:public_url(SURI),
    open_descriptor(terminus_descriptor{}, DB),
    write_cors_headers(SURI, DB),
    format('~n').
connect_handler(get,Request) :-
    config:public_url(Server_URI),
    connection_authorised_user(Request, User_ID),
    open_descriptor(terminus_descriptor{}, DB),
    user_object(DB, User_ID, User_Obj),
    write_cors_headers(Server_URI, DB),
    reply_json(User_Obj).

:- begin_tests(connect_handler).
:- use_module(core(util/test_utils)).

test(connection_authorised_user_http_basic, [
     ]) :-
    config:server(Server),
    admin_pass(Key),
    http_get(Server, _, [authorization(basic(admin, Key))]).

/*
 * Test assumes that  setenv("TERMINUS_SERVER_JWT_PUBLIC_KEY_PATH", "test/public_key_test.key.pub") setenv("TERMINUS_SERVER_JWT_PUBLIC_KEY_ID", "testkey") are set
 */
test(connection_authorised_user_jwt, [
         condition(getenv("TERMINUS_SERVER_JWT_PUBLIC_KEY_PATH", _))
     ]) :-
    config:server(Server),
    Bearer = 'eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6InRlc3RrZXkifQ.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaHR0cHM6Ly90ZXJtaW51c2RiLmNvbS9uaWNrbmFtZSI6ImFkbWluIiwiaWF0IjoxNTE2MjM5MDIyfQ.ZHqEzJViUkP41NuyWGY97uyzrXvBsuOvOjIz00VgP9H3NHfnbO_h51nqbjt3UqBeKJ7U0wGUMTePuhXCGsAPoI9rLRSK9NzlFKGde-wTs4lAhDpp6rGhmVzVcJAYtJg8RbTGlJ78SFK6SSTpi2sXOMgVeu8fZwGnnp7ZJjP1mtJdEreDEwlZYqgy21BltmuzQ08qC70R-jRFHY2IeVBarcbqJgxjjb3BrNA5fByMD4ESOBVJlmCg8PzaI4hEdW-lSsQK8XWWYTnndB8IFdD3GYIwMovsT9dVZ4m3HrGGywGSP7TxDquvvK9ollA2JV2tLMsbk_Nqo-s7fhBbH9xjsA',
    http_get(Server, _, [authorization(bearer(Bearer))]).

test(connection_result_dbs, [])
:-
    config:server(Server),
    admin_pass(Key),
    http_get(Server, Result, [json_object(dict),authorization(basic(admin, Key))]),

    * json_write_dict(current_output, Result, []),

    _{ '@id' : "doc:admin",
       '@type':"terminus:User"
     } :< Result.

:- end_tests(connect_handler).


%%%%%%%%%%%%%%%%%%%% Console Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(console), cors_catch(console_handler(Method)),
                [method(Method),
                 methods([options,get])]).

/*
 * console_handler(+Method,+Request) is det.
 */
console_handler(options,_Request) :-
    config:public_url(SURI),
    open_descriptor(terminus_descriptor{}, DB),
    write_cors_headers(SURI, DB),
    format('~n').
console_handler(get,_Request) :-
    config:index_path(Index_Path),
    read_file_to_string(Index_Path, String, []),
    config:public_url(SURI),
    open_descriptor(terminus_descriptor{}, DB),
    write_cors_headers(SURI, DB),
    format('~n'),
    write(String).

%%%%%%%%%%%%%%%%%%%% Message Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(message), cors_catch(message_handler(Method)),
                [method(Method),
                 methods([options,get,post])]).

:- begin_tests(console_route).

test(console_route) :-
    config:public_url(SURI),
    format(string(ConsoleURL), "~s/console", [SURI]),
    http_get(ConsoleURL, _, []).

:- end_tests(console_route).

/*
 * message_handler(+Method,+Request) is det.
 */
message_handler(options,_Request) :-
    config:public_url(SURI),
    open_descriptor(terminus_descriptor{}, DB),
    write_cors_headers(SURI, DB),
    format('~n').
message_handler(get,Request) :-
    try_get_param('terminus:message',Request,Message),

    with_output_to(
        string(Payload),
        json_write(current_output, Message, [])
    ),

    http_log('~N[Message] ~s~n',[Payload]),

    config:public_url(SURI),
    open_descriptor(terminus_descriptor{}, DB),
    write_cors_headers(SURI, DB),

    reply_json(_{'terminus:status' : 'terminus:success'}).
message_handler(post,R) :-
    add_payload_to_request(R,Request), % this should be automatic.
    try_get_param('terminus:message',Request,Message),

    with_output_to(
        string(Payload),
        json_write(current_output, Message, [])
    ),

    http_log('~N[Message] ~s~n',[Payload]),

    config:public_url(SURI),
    open_descriptor(terminus_descriptor{}, DB),
    write_cors_headers(SURI, DB),

    reply_json(_{'terminus:status' : 'terminus:success'}).

%%%%%%%%%%%%%%%%%%%% Database Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(db/Account/DB), cors_catch(db_handler(Method, Account, DB)),
                [method(Method),
                 methods([options,post,delete])]).

/**
 * db_handler(Method:atom,DB:atom,Request:http_request) is det.
 */
db_handler(options,_Account,_DB,_Request) :-
    % database may not exist - use server for CORS
    config:public_url(SURI),
    open_descriptor(terminus_descriptor{}, DB),
    write_cors_headers(SURI,DB),
    format('~n').
db_handler(post,Account,DB,R) :-
    add_payload_to_request(R,Request), % this should be automatic.
    open_descriptor(terminus_descriptor{}, Terminus_DB),
    /* POST: Create database */
    authenticate(Terminus_DB, Request, Auth),
    config:public_url(Server),
    assert_auth_action_scope(Terminus_DB, Auth, terminus:create_database, Server),

    get_payload(Database_Document,Request),
    (   _{ comment : Comment,
           label : Label,
           base_uri : Base_Uri } :< Database_Document
    ->  true
    ;   throw(error(bad_api_document('Bad API document')))),

    user_database_name(Account, DB, DB_Name),

    try_create_db(DB_Name, Label, Comment, Base_Uri),

    write_cors_headers(Server, Terminus_DB),
    reply_json(_{'terminus:status' : 'terminus:success'}).
db_handler(delete,Account,DB,Request) :-
    /* DELETE: Delete database */
    open_descriptor(terminus_descriptor{}, Terminus_DB),
    authenticate(Terminus_DB, Request, Auth),

    config:public_url(Server),

    user_database_name(Account, DB, DB_Name),
    assert_auth_action_scope(Terminus_DB, Auth, terminus:delete_database, DB_Name),

    try_delete_db(DB_Name),

    write_cors_headers(Server, Terminus_DB),
    reply_json(_{'terminus:status' : 'terminus:success'}).


:- begin_tests(db_endpoint).

:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(db_create, [
         setup((user_database_name('TERMINUS_QA', 'TEST_DB', DB),
                (   database_exists(DB)
                ->  delete_db(DB)
                ;   true))),
         cleanup((user_database_name('TERMINUS_QA', 'TEST_DB', DB),
                  delete_db(DB)))
     ]) :-
    config:server(Server),
    atomic_list_concat([Server, '/db/TERMINUS_QA/TEST_DB'], URI),
    Doc = _{ base_uri : "https://terminushub.com/document",
             comment : "A quality assurance test",
             label : "A label"
           },
    admin_pass(Key),
    http_post(URI, json(Doc),
              In, [json_object(dict),
                   authorization(basic(admin, Key))]),
    _{'terminus:status' : "terminus:success"} = In.


test(db_delete, [
         setup((user_database_name('TERMINUS_QA', 'TEST_DB', DB),
                create_db(DB,'test','a test',"https://terminushub.com/")))
     ]) :-
    config:server(Server),
    atomic_list_concat([Server, '/db/TERMINUS_QA/TEST_DB'], URI),
    admin_pass(Key),
    http_delete(URI, Delete_In, [json_object(dict),
                                 authorization(basic(admin, Key))]),

    _{'terminus:status' : "terminus:success"} = Delete_In.

test(db_auth_test, [
         setup((user_database_name('TERMINUS_QA', 'TEST_DB', DB),
                (   database_exists(DB)
                ->  delete_db(DB)
                ;   true))),
         cleanup((user_database_name('TERMINUS_QA', 'TEST_DB', DB),
                  delete_db(DB)))
     ]) :-

    config:server(Server),
    atomic_list_concat([Server, '/db/TERMINUS_QA/TEST_DB'], URI),
    Doc = _{ base_uri : "https://terminushub.com/document",
             comment : "A quality assurance test",
             label : "A label"
           },
    admin_pass(Key),
    http_post(URI, json(Doc),
              In, [json_object(dict),
                   authorization(basic(admin, Key))]),
    _{'terminus:status' : "terminus:success"} = In,

    user_object(terminus_descriptor{}, doc:admin, User_Obj),
    Access = User_Obj.'terminus:authority'.'terminus:access',
    Resources = Access.'terminus:authority_scope',
    once(
        (   memberchk(Database,Resources),
            _{ '@value' :  "TERMINUS_QA|TEST_DB" } :< Database.'terminus:resource_name')).

:- end_tests(db_endpoint).


%%%%%%%%%%%%%%%%%%%% Triples Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(triples/Path), cors_catch(triples_handler(Method,Path)),
                [method(Method),
                 prefix,
                 time_limit(infinite),
                 methods([options,get,post])]).

/*
 * triples_handler(Mode,DB,Request) is det.
 *
 * Get or update a schema.
 */
triples_handler(options,Path,_Request) :-
    open_descriptor(terminus_descriptor{}, Terminus),
    resolve_absolute_string_descriptor_and_graph(Path, Descriptor, _Graph),
    write_descriptor_cors(Descriptor,Terminus),
    format('~n'). % send headers
triples_handler(get,Path,Request) :-
    open_descriptor(terminus_descriptor{}, Terminus),
    /* Read Document */
    authenticate(Terminus,Request,Auth),

    resolve_absolute_string_descriptor_and_graph(Path, Descriptor, Graph),
    create_context(Descriptor, Pre_Context),

    merge_dictionaries(
        query_context{
            terminus: Terminus,
            authorization : Auth,
            filter: Graph
        }, Pre_Context, Context),

    assert_read_access(Context),

    dump_turtle_graph(Context, Graph.type, Graph.name, String),

    write_descriptor_cors(Descriptor,Terminus),
    reply_json(String).
triples_handler(post,Path,R) :- % should this be put?
    add_payload_to_request(R,Request), % this should be automatic.
    open_descriptor(terminus_descriptor{}, Terminus),
    /* Read Document */
    authenticate(Terminus,Request,Auth),
    resolve_absolute_string_descriptor_and_graph(Path, Descriptor, Graph),

    get_payload(Triples_Document,Request),
    (   _{ turtle : TTL,
           commit_info : Commit_Info } :< Triples_Document
    ->  true
    ;   throw(error(bad_api_document('Bad API document')))),

    create_context(Descriptor, Pre_Context),

    merge_dictionaries(
        query_context{
            commit_info : Commit_Info,
            terminus: Terminus,
            authorization : Auth,
            write_graph : Graph
        }, Pre_Context, Context),

    assert_write_access(Context),

    % check access rights
    % assert_auth_action_scope(Terminus,Auth,terminus:update_schema,DB_Name),
    update_turtle_graph(Context,Graph.type,Graph.name,TTL),

    write_descriptor_cors(Descriptor,Terminus),
    reply_json(_{'terminus:status' : "terminus:success"}).


:- begin_tests(triples_endpoint).

:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(triples_update, [
         setup((user_database_name('TERMINUS_QA', 'TEST_DB', DB),
                (   database_exists(DB)
                ->  delete_db(DB)
                ;   true),
                create_db(DB,'test','a test','http://hub.terminusdb.com/TERMINUS_QA/TEST_DB/document'))),
         cleanup((user_database_name('TERMINUS_QA', 'TEST_DB', DB),
                  delete_db(DB)))

     ])
:-
    % We actually have to create the graph before we can post to it!
    % First make the schema graph
    make_branch_descriptor('TERMINUS_QA', 'TEST_DB', Branch_Descriptor),
    create_graph(Branch_Descriptor,
                 commit_info{ author : "test",
                              message: "Generated by automated testing"},
                 schema,
                 "main",
                 Transaction_Metadata),
    * json_write_dict(current_output,Transaction_Metadata, []),

    terminus_path(Path),
    interpolate([Path, '/terminus-schema/terminus_schema.owl.ttl'], TTL_File),
    read_file_to_string(TTL_File, TTL, []),
    config:server(Server),
    atomic_list_concat([Server, '/triples/TERMINUS_QA/TEST_DB/local/branch/master/schema/main'], URI),
    admin_pass(Key),
    http_post(URI, json(_{commit_info : _{ author : "Test",
                                           message : "testing" },
                          turtle : TTL}),
              _In, [json_object(dict),
                   authorization(basic(admin, Key)),
                   reply_header(Fields)]),
    * writeq(Fields),

    findall(A-B-C,
            ask(Branch_Descriptor,
                t(A, B, C, "schema/*")),
            Triples),
    memberchk('http://terminusdb.com/schema/terminus'-(rdf:type)-(owl:'Ontology'), Triples).


test(triples_get, [])
:-

    config:server(Server),
    atomic_list_concat([Server, '/triples/terminus/schema/main'], URI),
    admin_pass(Key),
    http_get(URI, In, [json_object(dict),
                       authorization(basic(admin, Key))]),
    string(In).

:- end_tests(triples_endpoint).

%%%%%%%%%%%%%%%%%%%% Frame Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(frame/Path), cors_catch(frame_handler(Method,Path)),
                [method(Method),
                 prefix,
                 methods([options,post])]).

/**
 * frame_handler(+Mode, +DB, +Class_ID, +Request:http_request) is det.
 *
 * Establishes frame responses
 */
frame_handler(options,_Path,_Request) :-
    config:public_url(SURI),
    open_descriptor(terminus_descriptor{}, Terminus),
    write_cors_headers(SURI, Terminus),
    format('~n').
frame_handler(post, Path, R) :-
    add_payload_to_request(R,Request), % this should be automatic.
    open_descriptor(terminus_descriptor{}, Terminus),
    /* Read Document */
    authenticate(Terminus, Request, Auth),
    resolve_absolute_string_descriptor(Path, Descriptor),
    create_context(Descriptor, Context0),
    merge_dictionaries(
        query_context{
            authorization: Auth
        },
        Context0,Database),

    assert_read_access(Database),
    get_payload(Doc,Request),

    (   get_dict(class,Doc,Class_URI)
    ->  try_class_frame(Class_URI,Database,Frame)
    ;   get_dict(instance,Doc,Instance_URI)
    ->  try_filled_frame(Instance_URI,Database,Frame)
    ),

    write_descriptor_cors(Descriptor,Terminus),
    reply_json(Frame).

:- begin_tests(frame_endpoint).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(get_frame, [])
:-
    config:server(Server),
    atomic_list_concat([Server, '/frame/terminus'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{ class : "terminus:Agent"
                    }),
              JSON, [json_object(dict),
                     authorization(basic(admin, Key))]),
    _{'@type':"terminus:Frame"} :< JSON.


test(get_filled_frame, [])
:-
    config:server(Server),
    atomic_list_concat([Server, '/frame/terminus'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{ instance : "doc:admin"
                    }),
              JSON, [json_object(dict),
                     authorization(basic(admin, Key))]),
    _{'@type':"terminus:FilledFrame"} :< JSON.

:- end_tests(frame_endpoint).

%%%%%%%%%%%%%%%%%%%% WOQL Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
%
:- http_handler(root(woql), cors_catch(woql_handler(Method)),
                [method(Method),
                 time_limit(infinite),
                 methods([options,post])]).
:- http_handler(root(woql/Path), cors_catch(woql_handler(Method,Path)),
                [method(Method),
                 prefix,
                 time_limit(infinite),
                 methods([options,post])]).

/**
 * woql_handler(+Method:atom, +Request:http_request) is det.
 *
 * Open WOQL with no defined database
 *
 * NOTE: This is not obtaining appropriate cors response data
 * from terminus database on spartacus.
 */
woql_handler(options, _Request) :-
    config:public_url(SURI),
    open_descriptor(terminus_descriptor{}, Terminus),
    write_cors_headers(SURI, Terminus),
    format('~n').
woql_handler(post, R) :-
    add_payload_to_request(R,Request),
    http_log('~N[Request] ~q~n', [Request]),
    open_descriptor(terminus_descriptor{}, Terminus),
    authenticate(Terminus, Request, Auth_ID),
    % No descriptor to work with until the query sets one up
    empty_context(Context),

    woql_run_context(Request, Terminus, Auth_ID, Context, JSON),

    config:public_url(SURI),
    write_cors_headers(SURI, Terminus),
    reply_json_dict(JSON).

woql_handler(options, _Path, _Request) :-
    config:public_url(SURI),
    open_descriptor(terminus_descriptor{}, Terminus),
    write_cors_headers(SURI, Terminus),
    format('~n').

woql_handler(post, Path, R) :-
    add_payload_to_request(R,Request),
    open_descriptor(terminus_descriptor{}, Terminus),
    authenticate(Terminus, Request, Auth_ID),
    % No descriptor to work with until the query sets one up
    resolve_absolute_string_descriptor(Path, Descriptor),
    create_context(Descriptor, Context),

    woql_run_context(Request, Terminus, Auth_ID, Context, JSON),

    write_descriptor_cors(Descriptor,Terminus),
    reply_json_dict(JSON).

woql_run_context(Request, Terminus, Auth_ID, Context, JSON) :-

    try_get_param('query',Request,Query),

    (   get_param('commit_info', Request, Commit_Info)
    ->  true
    ;   Commit_Info = _{}
    ),

    woql_context(Prefixes),

    context_overriding_prefixes(Context,Prefixes,Context0),

    collect_posted_files(Request,Files),

    merge_dictionaries(
        query_context{
            commit_info : Commit_Info,
            files : Files,
            terminus: Terminus,
            update_guard : _Guard,
            authorization : Auth_ID
        }, Context0, Final_Context),

    http_log('~N[Prefixes] ~q~n', [Context0.prefixes]),

    json_woql(Query, Context0.prefixes, AST),

    run_context_ast_jsonld_response(Final_Context,AST,JSON).


% woql_handler Unit Tests

:- begin_tests(woql_endpoint).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(no_db, [])
:-

    Query =
    _{'@type' : "Using",
      collection : _{'@type' : "xsd:string",
                     '@value' : "terminus"},
      query :
      _{'@type' : "Select",    %   { "select" : [ v1, v2, v3, Query ] }
        variable_list : [
            _{'@type' : "VariableListElement",
              index : _{'@type' : "xsd:integer",
                        '@value' : 0},
              variable_name : _{ '@type' : "xsd:string",
                                 '@value' : "Class"}},
            _{'@type' : "VariableListElement",
              index : _{'@type' : "xsd:integer",
                        '@value' : 1},
              variable_name : _{ '@type' : "xsd:string",
                                 '@value' : "Label"}},
            _{'@type' : "VariableListElement",
              index : _{'@type' : "xsd:integer",
                        '@value' : 2},
              variable_name : _{ '@type' : "xsd:string",
                                 '@value' : "Comment"}},
            _{'@type' : 'VariableListElement',
              index : _{'@type' : "xsd:integer",
                        '@value' : 3},
              variable_name : _{ '@type' : "xsd:string",
                                 '@value' : "Abstract"}}
        ],
        query : _{'@type' : 'And',
                  query_list : [
                      _{'@type' : 'QueryListElement',
                        index : _{'@type' : "xsd:integer",
                                  '@value' : 0},
                        query : _{'@type' : 'Quad',
                                  subject : _{'@type' : "Variable",
                                              variable_name : _{ '@type' : "xsd:string",
                                                                 '@value' : "Class"}},
                                  predicate : "rdf:type",
                                  object : "owl:Class",
                                  graph_filter : _{'@type' : "xsd:string",
                                                   '@value' : "schema/*"}}},
                      _{'@type' : 'QueryListElement',
                        index : _{'@type' : "xsd:integer",
                                  '@value' : 1},
                        query :_{'@type' : 'Not',
                                 query : _{'@type' : 'Quad',
                                           subject : _{'@type' : "Variable",
                                                      variable_name : _{ '@type' : "xsd:string",
                                                                         '@value' : "Class"}},
                                           predicate : "tcs:tag",
                                           object : "tcs:abstract",
                                           graph_filter : _{'@type' : "xsd:string",
                                                            '@value' : "schema/*"}}}},
                      _{'@type' : 'QueryListElement',
                        index : _{'@type' : "xsd:integer",
                                  '@value' : 2},
                        query : _{'@type' : 'Optional',
                                  query : _{'@type' : 'Quad',
                                            subject : _{'@type' : "Variable",
                                                       variable_name : _{ '@type' : "xsd:string",
                                                                          '@value' : "Class"}},
                                            predicate : "rdfs:label",
                                            object : _{'@type' : "Variable",
                                                       variable_name : _{ '@type' : "xsd:string",
                                                                          '@value' : "Label"}},
                                            graph_filter : _{'@type' : "xsd:string",
                                                             '@value' : "schema/*"}}}},
                      _{'@type' : 'QueryListElement',
                        index : _{'@type' : "xsd:integer",
                                  '@value' : 3},
                        query : _{'@type' : 'Optional',
                                  query : _{'@type' : 'Quad',
                                            subject : _{'@type' : "Variable",
                                                       variable_name : _{ '@type' : "xsd:string",
                                                                          '@value' : "Class"}},
                                            predicate : "rdfs:comment",
                                            object : _{'@type' : "Variable",
                                                       variable_name : _{ '@type' : "xsd:string",
                                                                          '@value' : "Comment"}},
                                            graph_filter : _{'@type' : "xsd:string",
                                                             '@value' : "schema/*"}}}},
                      _{'@type' : 'QueryListElement',
                        index : _{'@type' : "xsd:integer",
                                  '@value' : 4},
                        query : _{'@type' : 'Optional',
                                  query : _{'@type' : 'Quad',
                                            subject : _{'@type' : "Variable",
                                                        variable_name : _{ '@type' : "xsd:string",
                                                                           '@value' : "Class"}},
                                            predicate : "tcs:tag",
                                            object : _{'@type' : "Variable",
                                                       variable_name : _{ '@type' : "xsd:string",
                                                                          '@value' : "Abstract"}},
                                            graph_filter : _{'@type' : "xsd:string",
                                                             '@value' : "schema/*"}}}}]}}},

    config:server(Server),
    atomic_list_concat([Server, '/woql'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{'query' : Query}),
              JSON,
              [json_object(dict),authorization(basic(admin,Key))]),

    % extra debugging...
    % nl,
    * json_write_dict(current_output,JSON,[]),
    _{'bindings' : _L} :< JSON.

test(indexed_get, [])
:-
    Query =
    _{'@type' : 'Get',
      as_vars : [
          _{'@type' : 'IndexedAsVar',
            index : _{'@type' : "xsd:integer",
                      '@value' : 0},
            variable_name : _{ '@type' : "xsd:string",
                               '@value' : "First"}},
          _{'@type' : 'IndexedAsVar',
            index : _{'@type' : "xsd:integer",
                      '@value' : 1},
            variable_name : _{ '@type' : "xsd:string",
                               '@value' : "Second"}}],
      query_resource :
      _{'@type' : 'RemoteResource',
        remote_uri : _{ '@type' : "xsd:anyURI",
                        '@value' : "https://terminusdb.com/t/data/bike_tutorial.csv"}}},


    config:server(Server),
    atomic_list_concat([Server, '/woql'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{query : Query}),
              JSON,
              [json_object(dict),authorization(basic(admin,Key))]),

    (   _{'bindings' : _} :< JSON
    ->  true
    ;   fail).

test(named_get, [])
:-

    Query =
    _{'@type' : 'Get',
      as_vars : [
          _{'@type' : 'NamedAsVar',
            var_type : _{ '@type' : "Node",
                          node : "xsd:integer"},
            identifier : _{ '@type' : "xsd:string",
                            '@value' : "Duration"},
            variable_name : _{ '@type' : "xsd:string",
                               '@value' : "Duration"}},
          _{'@type' : 'NamedAsVar',
            identifier : _{ '@type' : "xsd:string",
                            '@value' : "Bike number"},
            variable_name : _{ '@type' : "xsd:string",
                               '@value' : "Bike_Number"}}
      ],
      query_resource :
      _{'@type' : 'RemoteResource',
        remote_uri : _{ '@type' : "xsd:anyURI",
                        '@value' : "https://terminusdb.com/t/data/bike_tutorial.csv"}}},

    config:server(Server),
    atomic_list_concat([Server, '/woql'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{query : Query}),
              JSON,
              [json_object(dict),authorization(basic(admin,Key))]),

    [First|_] = JSON.bindings,
    _{'Bike_Number':_{'@type':"http://www.w3.org/2001/XMLSchema#string",
                      '@value':"W21477"},
      'Duration':_{'@type':"http://www.w3.org/2001/XMLSchema#integer",
                   '@value':790}} :< First.

test(branch_db, [
         setup((config:server(Server),
                user_database_name(admin,test, Name),
                (   database_exists(Name)
                ->  delete_db(Name)
                ;   true),
                create_db(Name, 'test','a test', 'http://terminushub.com/admin/test/document'))),
         cleanup((user_database_name(admin,test, Name),
                  delete_db(Name)))
     ])
:-
    atomic_list_concat([Server, '/woql/admin/test'], URI),

    % TODO: We need branches to pull in the correct 'doc:' prefix.
    Query0 =
    _{'@context' : _{ doc: "http://terminushub.com/admin/test/document/"},
      '@type' : "AddTriple",
      subject : "doc:test_subject",
      predicate : "doc:test_predicate",
      object : "doc:test_object"
     },
    Commit = commit_info{ author : 'The Gavinator',
                          message : 'Peace and goodwill' },

    admin_pass(Key),
    http_post(URI,
              json(_{'query' : Query0,
                     'commit_info' : Commit }),
              JSON0,
              [json_object(dict),authorization(basic(admin,Key))]),

    _{bindings : [_{}], inserts: 1, deletes : 0} :< JSON0,

    % Now query the insert...
    Query1 =
    _{'@type' : "Triple",
      subject : _{'@type' : "Variable",
                  variable_name : _{ '@type' : "xsd:string",
                                     '@value' : "Subject"}},
      predicate : _{'@type' : "Variable",
                    variable_name : _{ '@type' : "xsd:string",
                                       '@value' : "Predicate"}},
      object : _{'@type' : "Variable",
                 variable_name : _{ '@type' : "xsd:string",
                                    '@value' : "Object"}}},

    http_post(URI,
              json(_{query : Query1}),
              JSON1,
              [json_object(dict),authorization(basic(admin,Key))]),

    * json_write_dict(current_output,JSON1,[]),

    (   _{'bindings' : L} :< JSON1
    ->  L = [_{'Object':"http://terminushub.com/admin/test/document/test_object",
               'Predicate':"http://terminushub.com/admin/test/document/test_predicate",
               'Subject':"http://terminushub.com/admin/test/document/test_subject"}]
    ).

test(update_object, [
         setup((config:server(Server),
                user_database_name(admin,test, Name),
                (   database_exists(Name)
                ->  delete_db(Name)
                ;   true),
                create_db(Name, 'test','a test','http://terminushub.com/admin/test/document'))),
         cleanup((user_database_name(admin,test, Name),
                  delete_db(Name)))
     ])
:-
    config:server(Server),
    atomic_list_concat([Server, '/woql/admin/test'], URI),

    % First make a schema against which we can have an object
    make_branch_descriptor("admin","test",Branch_Descriptor),

    create_graph(Branch_Descriptor,
                 commit_info{ author : "test",
                              message: "Generated by automated testing"},
                 schema,
                 "main",
                 _Transaction_Metadata2),

    terminus_path(Path),
    interpolate([Path, '/terminus-schema/terminus_schema.owl.ttl'], TTL_File),
    read_file_to_string(TTL_File, TTL, []),
    create_context(Branch_Descriptor, Database0),
    Database = Database0.put(commit_info, commit_info{
                                              author : "Steve",
                                              message : "Yeah I did it"
                                          }),
    update_turtle_graph(Database,schema,"main",TTL),

    % TODO: We need branches to pull in the correct 'doc:' prefix.
    Query0 =
    _{'@context' : _{ doc: "http://terminusdb.com/admin/test/document/",
                      scm: "http://terminusdb.com/schema/terminus#"},
      '@type' : "UpdateObject",
      document : _{ '@type' : "scm:Database",
                    '@id' : 'doc:my_database',
                    'scm:resource_name' : _{'@type' : "xsd:string",
                                            '@value' : "Steve"},
                    'scm:database_state' : _{'@id' : 'scm:finalized'}}
     },

    Commit = commit_info{ author : 'The Gavinator',
                          message : 'Peace and goodwill' },

    admin_pass(Key),
    http_post(URI,
              json(_{query : Query0,
                     commit_info : Commit}),
              JSON0,
              [json_object(dict),authorization(basic(admin,Key))]),

    * json_write_dict(current_output,JSON0,[]),

    Query1 =
    _{'@type' : "Triple",
      subject : _{'@type' : "Variable",
                  variable_name : _{ '@type' : "xsd:string",
                                     '@value' : "Subject"}},
      predicate : _{'@type' : "Variable",
                    variable_name : _{ '@type' : "xsd:string",
                                       '@value' : "Predicate"}},
      object : _{'@type' : "Variable",
                 variable_name : _{ '@type' : "xsd:string",
                                    '@value' : "Object"}}},

    http_post(URI,
              json(_{query : Query1}),
              JSON1,
              [json_object(dict),authorization(basic(admin,Key))]),

    Expected = [
        _{'Object':_{'@type':"http://www.w3.org/2001/XMLSchema#string",
                     '@value':"Steve"},
          'Predicate':"http://terminusdb.com/schema/terminus#resource_name",
          'Subject':"http://terminusdb.com/admin/test/document/my_database"},
        _{'Object':"http://terminusdb.com/schema/terminus#finalized",
          'Predicate':"http://terminusdb.com/schema/terminus#database_state",
          'Subject':"http://terminusdb.com/admin/test/document/my_database"},
        _{'Object':"http://terminusdb.com/schema/terminus#Database",
          'Predicate':"http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
          'Subject':"http://terminusdb.com/admin/test/document/my_database"}],
    Bindings = JSON1.bindings,
    union(Expected, Bindings, Union),
    intersection(Expected, Bindings, Intersection),
    subtract(Union, Intersection, []).


test(delete_object, [
         setup((config:server(Server),
                user_database_name(admin,test, Name),
                (   database_exists(Name)
                ->  delete_db(Name)
                ;   true),
                create_db(Name,'test','a test', 'http://terminushub.com/admin/test/document'))),
         cleanup((user_database_name(admin,test, Name),
                  delete_db(Name)))
     ])
:-

    make_branch_descriptor("admin","test",Branch_Descriptor),

    % First make a schema graph

    create_graph(Branch_Descriptor,
                 commit_info{ author : "test",
                              message: "Generated by automated testing"},
                 schema,
                 "main",
                 _Transaction_Metadata2),

    % Create the schema
    terminus_path(Path),
    interpolate([Path, '/terminus-schema/terminus_schema.owl.ttl'], TTL_File),
    read_file_to_string(TTL_File, TTL, []),
    create_context(Branch_Descriptor, Database0),
    Database = Database0.put(commit_info, commit_info{
                                              author : "Steve",
                                              message : "Yeah I did it"
                                          }),
    update_turtle_graph(Database,schema,"main",TTL),

    % Create the object
    Doc = _{ '@type' : "scm:Database",
             '@id' : 'doc:my_database',
             'scm:resource_name' :
             _{'@type' : "xsd:string",
               '@value' : "Steve"},
             'scm:database_state' :
             _{'@id' : 'scm:finalized'}},

    create_context(Branch_Descriptor, Pre_Database1),
    Pre_Database2 = Pre_Database1.put(commit_info, commit_info{
                                                       author : "Author",
                                                       message : "Message"}),
    Prefixes = _{ doc: "http://terminusdb.com/admin/test/document/",
                  scm: "http://terminusdb.com/schema/terminus#"},
    context_overriding_prefixes(Pre_Database2,Prefixes,Database1),
    with_transaction(Database1,
                     ask(Database1,
                         update_object(Doc)),
                     _Meta_Data),

    % Delete the document.
    Query1 =
    _{'@context' : _{ doc: "http://terminusdb.com/admin/test/document/",
                      scm: "http://terminusdb.com/schema/terminus#"},
      '@type' : "DeleteObject",
      document : 'doc:my_database'},

    Commit_Info = commit_info{
                      author : "Steve",
                      message : "Deleted baby"
                  },

    admin_pass(Key),
    atomic_list_concat([Server, '/woql/admin/test'], URI),
    http_post(URI,
              json(_{ query : Query1,
                      commit_info : Commit_Info}),
              JSON1,
              [json_object(dict),authorization(basic(admin,Key))]),

    * json_write_dict(current_output, JSON1, []),

    % Does it still exist?
    \+ ask(Branch_Descriptor,
           t('http://terminusdb.com/admin/test/document/my_database', _, _)).


test(get_object, [])
:-
    Query0 =
    _{'@type' : "ReadObject",
      document_uri : 'doc:admin',
      document : _{'@type' : "Variable",
                   variable_name : _{ '@type' : "xsd:string",
                                      '@value' : "Document"}}},

    config:server(Server),
    admin_pass(Key),
    atomic_list_concat([Server, '/woql/terminus'], URI),
    http_post(URI,
              json(_{query : Query0}),
              JSON0,
              [json_object(dict),authorization(basic(admin,Key))]),
    [Result] = JSON0.bindings,

    _{'@id':"doc:admin",
      '@type':"terminus:User",
      'terminus:agent_key_hash':_,
      'terminus:agent_name': _,
      'terminus:authority': _}
    :< Result.'Document'.

:- end_tests(woql_endpoint).

%%%%%%%%%%%%%%%%%%%% Clone Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(clone), cors_catch(clone_handler(Method)),
                [method(Method),
                 prefix,
                 methods([options,get])]).

clone_handler(_Method,_Request) :-
    throw(error('Not implemented')).

%%%%%%%%%%%%%%%%%%%% Fetch Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(fetch/Path), cors_catch(fetch_handler(Method,Path)),
                [method(Method),
                 prefix,
                 methods([options,post])]).

fetch_handler(_Method,_Path,_Request) :-
    throw(error('Not implemented')).


%%%%%%%%%%%%%%%%%%%% Rebase Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(rebase/Path), cors_catch(rebase_handler(Method,Path)),
                [method(Method),
                 prefix,
                 methods([options,post])]).

rebase_handler(_Method,_Path,_Request) :-
    throw(error('Not implemented')).

%%%%%%%%%%%%%%%%%%%% Push Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(push/Path), cors_catch(push_handler(Method,Path)),
                [method(Method),
                 methods([options,post])]).

push_handler(_Method,_Path,_Request) :-
    throw(error('Not implemented')).


%%%%%%%%%%%%%%%%%%%% Branch Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(branch/Path), cors_catch(branch_handler(Method,Path)),
                [method(Method),
                 prefix,
                 methods([options,post])]).

branch_handler(options,Path,_Request) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    write_descriptor_cors(Descriptor, terminus_descriptor{}),
    nl.
branch_handler(post,Path,R) :-
    resolve_absolute_string_descriptor(Path, Branch_Descriptor),
    branch_descriptor{
        repository_descriptor: Destination_Descriptor,
        branch_name: Branch_Name
    } :< Branch_Descriptor,
    
    add_payload_to_request(R,Request),
    get_payload(Document, Request),

    (   get_dict(origin, Document, Origin_Path)
    ->  resolve_absolute_string_descriptor(Origin_Path, Origin_Descriptor)
    ;   Origin_Descriptor = empty),

    (   get_dict(base_uri, Document, Base_Uri)
    ->  Options = [base_uri(Base_Uri)]
    ;   Options = []),

    branch_create(Destination_Descriptor, Origin_Descriptor, Branch_Name, Options, _Branch_Uri),

    write_descriptor_cors(Branch_Descriptor, terminus_descriptor{}),
    reply_json(_{'terminus:status' : "terminus:success"}).

:- begin_tests(branch_endpoint).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(create_empty_branch, [
         setup((config:server(Server),
                user_database_name(admin,test, Name),
                (   database_exists(Name)
                ->  delete_db(Name)
                ;   true),
                create_db(Name, 'test','a test','http://terminushub.com/admin/test/document'))),
         cleanup((user_database_name(admin,test, Name),
                  delete_db(Name)))
     ])
:-
    config:server(Server),
    atomic_list_concat([Server, '/branch/admin/test/local/branch/foo'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{base_uri: 'http://terminushub.com/admin/test/foodocument'}),
              JSON,
              [json_object(dict),authorization(basic(admin,Key))]),
    * json_write_dict(current_output, JSON, []),

    resolve_absolute_string_descriptor("admin/test/local/_commits", Repository_Descriptor),

    has_branch(Repository_Descriptor, "foo"),
    branch_base_uri(Repository_Descriptor, "foo", "http://terminushub.com/admin/test/foodocument").

test(create_empty_branch_without_base_uri_errors, [
         setup((config:server(Server),
                user_database_name(admin,test, Name),
                (   database_exists(Name)
                ->  delete_db(Name)
                ;   true),
                create_db(Name, 'test','a test','http://terminushub.com/admin/test/document'))),
         cleanup((user_database_name(admin,test, Name),
                  delete_db(Name)))
     ])
:-
    config:server(Server),
    atomic_list_concat([Server, '/branch/admin/test/local/branch/foo'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{}),
              JSON,
              [json_object(dict),authorization(basic(admin,Key)),
               status_code(Status_Code)]),
    Status_Code = 400,
    * json_write_dict(current_output, JSON, []),

    resolve_absolute_string_descriptor("admin/test/local/_commits", Repository_Descriptor),

    \+ has_branch(Repository_Descriptor, "foo").

test(create_branch_from_local_without_base_uri, [
         setup((config:server(Server),
                user_database_name(admin,test, Name),
                (   database_exists(Name)
                ->  delete_db(Name)
                ;   true),
                create_db(Name, 'test','a test','http://terminushub.com/admin/test/document'))),
         cleanup((user_database_name(admin,test, Name),
                  delete_db(Name)))
     ])
:-
    config:server(Server),
    atomic_list_concat([Server, '/branch/admin/test/local/branch/foo'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{origin:'/admin/test/local/branch/master'}),
              JSON,
              [json_object(dict),authorization(basic(admin,Key))]),
    * json_write_dict(current_output, JSON, []),

    resolve_absolute_string_descriptor("admin/test/local/_commits", Repository_Descriptor),

    has_branch(Repository_Descriptor, "foo"),
    branch_base_uri(Repository_Descriptor, "foo", "http://terminushub.com/admin/test/document").

test(create_branch_from_local_with_base_uri, [
         setup((config:server(Server),
                user_database_name(admin,test, Name),
                (   database_exists(Name)
                ->  delete_db(Name)
                ;   true),
                create_db(Name, 'test','a test','http://terminushub.com/admin/test/document'))),
         cleanup((user_database_name(admin,test, Name),
                  delete_db(Name)))
     ])
:-
    config:server(Server),
    atomic_list_concat([Server, '/branch/admin/test/local/branch/foo'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{origin:'/admin/test/local/branch/master',
                     base_uri:'http://terminushub.com/admin/test/foodocument'}),
              JSON,
              [json_object(dict),authorization(basic(admin,Key))]),
    * json_write_dict(current_output, JSON, []),

    resolve_absolute_string_descriptor("admin/test/local/_commits", Repository_Descriptor),

    has_branch(Repository_Descriptor, "foo"),
    branch_base_uri(Repository_Descriptor, "foo", "http://terminushub.com/admin/test/foodocument").

test(create_branch_that_already_exists_error, [
         setup((config:server(Server),
                user_database_name(admin,test, Name),
                (   database_exists(Name)
                ->  delete_db(Name)
                ;   true),
                create_db(Name, 'test','a test','http://terminushub.com/admin/test/document'))),
         cleanup((user_database_name(admin,test, Name),
                  delete_db(Name)))
     ])
:-
    config:server(Server),
    atomic_list_concat([Server, '/branch/admin/test/local/branch/master'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{origin:'/admin/test/local/branch/master',
                     base_uri:'http://terminushub.com/admin/test/foodocument'}),
              JSON,
              [json_object(dict),
               authorization(basic(admin,Key)),
               status_code(Status_Code)]),
    Status_Code = 400,
    * json_write_dict(current_output, JSON, []).

test(create_branch_from_nonexisting_origin_error, [
         setup((config:server(Server),
                user_database_name(admin,test, Name),
                (   database_exists(Name)
                ->  delete_db(Name)
                ;   true),
                create_db(Name, 'test','a test','http://terminushub.com/admin/test/document'))),
         cleanup((user_database_name(admin,test, Name),
                  delete_db(Name)))
     ])
:-
    config:server(Server),
    atomic_list_concat([Server, '/branch/admin/test/local/branch/foo'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{origin:'/admin/test/local/branch/bar',
                     base_uri:'http://terminushub.com/admin/test/foodocument'}),
              JSON,
              [json_object(dict),
               authorization(basic(admin,Key)),
               status_code(Status_Code)]),
    Status_Code = 400,
    * json_write_dict(current_output, JSON, []),

    resolve_absolute_string_descriptor("admin/test/local/_commits", Repository_Descriptor),

    \+ has_branch(Repository_Descriptor, "foo").

test(create_branch_from_commit_graph_error, [
         setup((config:server(Server),
                user_database_name(admin,test, Name),
                (   database_exists(Name)
                ->  delete_db(Name)
                ;   true),
                create_db(Name, 'test','a test','http://terminushub.com/admin/test/document'))),
         cleanup((user_database_name(admin,test, Name),
                  delete_db(Name)))
     ])
:-
    config:server(Server),
    atomic_list_concat([Server, '/branch/admin/test/local/branch/foo'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{origin:'/admin/test/local/_commits',
                     base_uri:'http://terminushub.com/admin/test/foodocument'}),
              JSON,
              [json_object(dict),
               authorization(basic(admin,Key)),
               status_code(Status_Code)]),
    Status_Code = 400,
    * json_write_dict(current_output, JSON, []),

    resolve_absolute_string_descriptor("admin/test/local/_commits", Repository_Descriptor),

    \+ has_branch(Repository_Descriptor, "foo").
   
:- end_tests(branch_endpoint).



%%%%%%%%%%%%%%%%%%%% Create/Delete Graph Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(graph/Path), cors_catch(graph_handler(Method,Path)),
                [method(Method),
                 prefix,
                 methods([options,post,delete])]).

graph_handler(options, _Path, _Request) :-
    config:public_url(SURI),
    open_descriptor(terminus_descriptor{}, Terminus),
    write_cors_headers(SURI, Terminus),
    format('~n').
graph_handler(post, Path, R) :-
    add_payload_to_request(R,Request),
    open_descriptor(terminus_descriptor{}, Terminus),
    authenticate(Terminus, Request, _Auth_ID),
    % No descriptor to work with until the query sets one up
    resolve_absolute_string_descriptor_and_graph(Path, Descriptor, Graph),

    get_payload(Document, Request),

    (   _{ commit_info : Commit_Info } :< Document
    ->  true
    ;   Commit_Info = _{} % Probably need to error here...
    ),

    create_graph(Descriptor,
                 Commit_Info,
                 Graph.type,
                 Graph.name,
                 _Transaction_Metadata2),

    write_descriptor_cors(Descriptor, Terminus),
    reply_json(_{'terminus:status' : "terminus:success"}).
graph_handler(delete, Path, R) :-
    add_payload_to_request(R,Request),
    open_descriptor(terminus_descriptor{}, Terminus),
    authenticate(Terminus, Request, _Auth_ID),
    % No descriptor to work with until the query sets one up
    resolve_absolute_string_descriptor_and_graph(Path, Descriptor, Graph),
    get_payload(Document, Request),

    (   _{ commit_info : Commit_Info } :< Document
    ->  true
    ;   Commit_Info = _{} % Probably need to error here...
    ),

    delete_graph(Descriptor,
                 Commit_Info,
                 Graph.type,
                 Graph.name,
                 _Transaction_Metadata2),

    write_cors_headers(Descriptor, Terminus),
    reply_json(_{'terminus:status' : "terminus:success"}).


:- begin_tests(graph_endpoint).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(create_graph, [
         setup((config:server(Server),
                user_database_name(admin,test, Name),
                (   database_exists(Name)
                ->  delete_db(Name)
                ;   true),
                create_db(Name, 'test','a test','http://terminushub.com/admin/test/document'))),
         cleanup((user_database_name(admin,test, Name),
                  delete_db(Name)))
     ])
:-
    Commit = commit_info{ author : 'The Graphinator',
                          message : 'Edges here there and everywhere' },

    config:server(Server),
    atomic_list_concat([Server, '/graph/admin/test/local/branch/master/instance/naim'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{commit_info : Commit}),
              JSON,
              [json_object(dict),authorization(basic(admin,Key))]),
    * json_write_dict(current_output, JSON, []),

    make_branch_descriptor("admin","test",Branch_Descriptor),
    open_descriptor(Branch_Descriptor, Transaction),
    Instance_Objects = Transaction.instance_objects,
    exists([Obj]>>(
               get_dict(descriptor, Obj, Desc),
               get_dict(name, Desc, "naim")
           ), Instance_Objects).

:- end_tests(graph_endpoint).

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
    cors_enable,
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
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:message' : 'Connection timed out'
               },
               [status(408)]).
customise_error(error(access_not_authorised(Auth,Action,Scope))) :-
    format(string(Msg), "Access to ~q is not authorised with action ~q and auth ~q",
           [Auth,Action,Scope]),
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:message' : Msg},
               [status(403)]).
customise_error(error(schema_check_failure(Witnesses))) :-
    reply_json(Witnesses,
               [status(405)]).
customise_error(error(branch_creation_base_uri_not_specified)) :-
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:message' : 'branch has no specified base uri'},
               [status(400)]).
customise_error(error(branch_already_exists(Branch_Name))) :-
    format(string(Msg), "branch ~w already exists", [Branch_Name]),
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:message' : Msg},
               [status(400)]).
customise_error(error(origin_branch_does_not_exist(Branch_Name))) :-
    format(string(Msg), "origin branch ~w does not exist", [Branch_Name]),
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:message' : Msg},
               [status(400)]).
customise_error(error(origin_commit_does_not_exist(Commit_Id))) :-
    format(string(Msg), "origin commit ~w does not exist", [Commit_Id]),
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:message' : Msg},
               [status(400)]).
customise_error(error(origin_cannot_be_branched(Descriptor))) :-
    format(string(Msg), "origin ~w cannot be branched", [Descriptor]),
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:message' : Msg},
               [status(400)]).
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
fetch_authorization_data(Request, Username, KS) :-
    memberchk(authorization(Text), Request),
    http_authorization_data(Text, basic(Username, Key)),
    coerce_literal_string(Key, KS).

/*
 *  fetch_jwt_data(+Request, -Username) is semi-determinate.
 *
 *  Fetches the HTTP JWT data
 */
fetch_jwt_data(Request, Username) :-
    memberchk(authorization(Text), Request),
    pattern_string_split(" ", Text, ["Bearer", Token]),
    atom_string(TokenAtom, Token),
    jwt_decode(TokenAtom, Payload, []),
    atom_json_dict(Payload, PayloadDict, []),
    UsernameString = PayloadDict.get('https://terminusdb.com/nickname'),
    atom_string(Username, UsernameString).

/*
 * authenticate(+Database, +Request, -Auth_Obj) is det.
 *
 * This should either bind the Auth_Obj or throw an http_status_reply/4 message.
 */
authenticate(DB, Request, Auth) :-
    fetch_authorization_data(Request, Username, KS),
    !,
    (   user_key_auth(DB, Username, KS, Auth)
    ->  true
    ;   throw(http_reply(authorize(_{'terminus:status' : 'terminus:failure',
                                     'terminus:message' : 'Not a valid key'})))).
authenticate(DB, Request, Auth) :-
    % Try JWT if no http keys
    fetch_jwt_data(Request, Username),
    !,
    (   username_auth(DB, Username, Auth)
    ->  true
    ;   throw(http_reply(authorize(_{'terminus:status' : 'terminus:failure',
                                     'terminus:message' : 'Not a valid key'})))).
authenticate(_, _, _) :-
    throw(http_reply(method_not_allowed(_{'terminus:status' : 'terminus:failure',
                                          'terminus:message' : "No authentication supplied",
                                          'terminus:object' : 'authenticate'}))).

connection_authorised_user(Request, User_ID) :-
    open_descriptor(terminus_descriptor{}, DB),
    fetch_authorization_data(Request, Username, KS),
    !,
    (   user_key_user_id(DB, Username, KS, User_ID)
    ->  true
    ;   throw(http_reply(authorize(_{'terminus:status' : 'terminus:failure',
                                     'terminus:message' : 'Not a valid key',
                                     'terminus:object' : KS})))).
connection_authorised_user(Request, User_ID) :-
    open_descriptor(terminus_descriptor{}, DB),
    fetch_jwt_data(Request, Username),
    username_user_id(DB, Username, User_ID).

check_descriptor_auth(terminus_descriptor{},Action,Auth,Terminus) :-
    assert_auth_action_scope(Terminus,Auth,Action,"terminus").
check_descriptor_auth(database_descriptor{ database_name : Name }, Action, Auth, Terminus) :-
    assert_auth_action_scope(Terminus,Auth,Action,Name).
check_descriptor_auth(repository_descriptor{ database_descriptor : DB,
                                             repository_name : _ }, Action, Auth, Terminus) :-
    check_descriptor_auth(DB, Action, Auth, Terminus).
check_descriptor_auth(branch_descriptor{ repository_descriptor : Repo,
                                         branch_name : _ }, Action, Auth, Terminus) :-
    check_descriptor_auth(Repo, Action, Auth, Terminus).
check_descriptor_auth(commit_descriptor{ repository_descriptor : Repo,
                                         commit_id : _ }, Action, Auth, Terminus) :-
    check_descriptor_auth(Repo, Action, Auth, Terminus).

write_descriptor_cors(terminus_descriptor{},Terminus) :-
    write_cors_headers("terminus",Terminus).
write_descriptor_cors(database_descriptor{ database_name : Name },Terminus) :-
    write_cors_headers(Name,Terminus).
write_descriptor_cors(repository_descriptor{ database_descriptor : DB,
                                             repository_name : _ }, Terminus) :-
    write_descriptor_cors(DB, Terminus).
write_descriptor_cors(branch_descriptor{ repository_descriptor : Repo,
                                         branch_name : _ }, Terminus) :-
    write_descriptor_cors(Repo, Terminus).
write_descriptor_cors(commit_descriptor{ repository_descriptor : Repo,
                                         commit_id : _ }, Terminus) :-
    write_descriptor_cors(Repo, Terminus).

check_capabilities(_Transaction_Object, _Active) :-
    % we need to resolve all graphs and graph_filters and
    % see if they are accessible
    true.


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
 * try_delete_document(+ID, +Database, -Meta_Data) is det.
 *
 * Actually has determinism: det + error
 *
 * Deletes the object associated with ID, and throws an
 * http error otherwise.
 */
try_delete_document(Pre_Doc_ID, Database, Meta_Data) :-
    (   collection_descriptor_prefixes(Database.descriptor, Ctx)
    ->  prefix_expand(Pre_Doc_ID,Ctx,Doc_ID)
    ;   format(atom(MSG), 'Document resource ~s could not be expanded', [Pre_Doc_ID]),
        throw(http_reply(not_found(_{'terminus:status' : 'terminus_failure',
                                     'terminus:message' : MSG,
                                     'terminus:object' : Pre_Doc_ID})))),

    (   with_transaction(Database,
                         ask(Database,
                             delete_object(Doc_ID)),
                         Meta_Data)
    ->  true
    ;   format(atom(MSG), 'Document resource ~s could not be deleted', [Doc_ID]),
        throw(http_reply(not_found(_{'terminus:status' : 'terminus_failure',
                                     'terminus:message' : MSG,
                                     'terminus:object' : Doc_ID})))).

/*
 * try_update_document(ID, Doc, Database, Meta_Data) is det.
 *
 * Actually has determinism: det + error
 *
 * Updates the object associated with ID, and throws an
 * http error otherwise.
 */
try_update_document(Doc_ID, Doc_In, Database, Meta_Data) :-
    % if there is no id, we'll use the requested one.
    (   jsonld_id(Doc_In,Doc_ID_Match)
    ->  Doc_In = Doc
    %   This is wrong - we need to have the base path here as well.
    ;   put_dict(Doc_ID,'@id',Doc_In,Doc)),

    (   get_key_document('@id',Database.prefixes,Doc,Doc_ID_Match)
    ->  true
    ;   format(atom(MSG),'Unable to match object ids ~q and ~q', [Doc_ID, Doc_ID_Match]),
        throw(http_reply(not_found(_{'terminus:message' : MSG,
                                     'terminus:status' : 'terminus:failure'})))),

    (   with_transaction(Database,
                         ask(Database,
                             update_object(Doc)),
                         Meta_Data)
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
    (   config:public_url(Server_Name),
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
    (   resolve_query_resource(DB_URI, Descriptor)
    ->  open_descriptor(Descriptor,Database)
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
    http_log('[Test] ~q', [Request]),

    (   memberchk(payload(Document), Request)
    ->  true
    ;   format(atom(MSG), 'No JSON payload resource ~q for POST ~q', [Key,Request]),
        throw(http_reply(not_found(_{'terminus:status' : 'terminus:failure',
                                     'terminus:message' : MSG})))),

    (   Value = Document.get(Key)
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
    memberchk(method(post), Request),
    memberchk(multipart(Parts), Request),
    !,
    memberchk(Key=Encoded_Value, Parts),
    uri_encoded(query_value, Value, Encoded_Value).
get_param(Key,Request,Value) :-
    % GET or POST (but not application/json)
    memberchk(method(Method), Request),
    (   memberchk(Method, [get,delete])
    ;   Method = post,
        \+ memberchk(content_type('application/json'), Request)),

    http_parameters(Request, [], [form_data(Data)]),
    memberchk(Key=Value,Data),
    !.
get_param(Key,Request,Value) :-
    % POST with JSON package
    memberchk(method(post), Request),
    memberchk(content_type('application/json'), Request),
    memberchk(payload(Document), Request),
    Value = Document.get(Key).
%%% cut goes to method(post) above
%     !.
% get_param(Key,Request,_Value) :-
%     % OTHER with method
%     memberchk(method(Method), Request),
%     !,
%     format(atom(MSG), 'Method ~q has no parameter key transport for key ~q', [Method,Key]),
%     throw(http_reply(not_found(_{'terminus:message' : MSG,
%                                  'terminus:status' : 'terminus:failure',
%                                  'terminus:object' : Key}))).
% get_param(Key,_Request,_Value) :-
%     % Catch all.
%     format(atom(MSG), 'Request has no parameter key transport for key ~q', [Key]),
%     throw(http_reply(not_found(_{'terminus:status' : 'terminus:failure',
%                                  'terminus:message' : MSG}))).


/*
 * try_create_db(DB,Label,Comment,Base_URI) is det.
 *
 * Try to create a database and associate resources
 */
try_create_db(DB,Label,Comment,Base_Uri) :-
    % create the collection if it doesn't exist
    (   database_exists(DB)
    ->  throw(http_reply(method_not_allowed(_{'terminus:status' : 'terminus:failure',
                                              'terminus:message' : 'Database already exists',
                                              'terminus:method' : 'terminus:create_database'})))
    ;   true),

    (   create_db(DB, Label, Comment, Base_Uri)
    ->  true
    ;   format(atom(MSG), 'Database ~s could not be created', [DB]),
        throw(http_reply(not_found(_{'terminus:message' : MSG,
                                     'terminus:status' : 'terminus:failure'})))).


/*
 * try_delete_db(DB_URI) is det.
 *
 * Attempt to delete a database given its URI
 */
try_delete_db(DB) :-
    (   delete_db(DB)
    ->  true
    ;   format(atom(MSG), 'Database ~s could not be destroyed', [DB]),
        throw(http_reply(not_found(_{'terminus:message' : MSG,
                                     'terminus:status' : 'terminus:failure'})))).

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

get_payload(Payload,Request) :-
    memberchk(payload(Payload),Request).

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
    prefix_expand(Class, Database.prefixes, ClassEx),
    (   class_frame_jsonld(Database,ClassEx,Frame)
    ->  true
    ;   format(atom(MSG), 'Class Frame could not be json-ld encoded for class ~s', [Class]),
        % Give a better error code etc. This is silly.
        throw(http_reply(not_found(_{ 'terminus:message' : MSG,
                                      'terminus:status' : 'terminus:failure',
                                      'terminus:class' : Class})))).

/*
 * try_class_frame(Class,Database,Frame) is det.
 */
try_filled_frame(Instance,Database,Frame) :-
    prefix_expand(Instance, Database.prefixes, InstanceEx),
    (   filled_frame_jsonld(Database,InstanceEx,Frame)
    ->  true
    ;   format(atom(MSG), 'Filled Class Frame could not be json-ld encoded for instance ~s', [Instance]),
        % Give a better error code etc. This is silly.
        throw(http_reply(not_found(_{ 'terminus:message' : MSG,
                                      'terminus:status' : 'terminus:failure',
                                      'terminus:instance' : Instance})))).

