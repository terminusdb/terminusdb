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
    config:server(SURI),
    open_descriptor(terminus_descriptor{}, DB),
    write_cors_headers(SURI, DB),
    format('~n').
connect_handler(get,Request) :-
    config:server(Server_URI),
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
 * Test assumes that  setenv("TERMINUSDB_SERVER_JWT_PUBLIC_KEY_PATH", "test/public_key_test.key.pub")
 * setenv("TERMINUSDB_SERVER_JWT_PUBLIC_KEY_ID", "testkey") are set
 */
test(connection_authorised_user_jwt, [
         condition(getenv("TERMINUSDB_SERVER_JWT_PUBLIC_KEY_PATH", _))
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
:- http_handler(root(console/_Path), cors_catch(console_handler(Method)),
                [method(Method),
                 methods([options,get])]).

/*
 * console_handler(+Method,+Request) is det.
 */
console_handler(options,_Request) :-
    config:server(SURI),
    open_descriptor(terminus_descriptor{}, DB),
    write_cors_headers(SURI, DB),
    format('~n').
console_handler(get,_Request) :-
    config:index_path(Index_Path),
    read_file_to_string(Index_Path, String, []),
    config:server(SURI),
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
    config:server(SURI),
    format(string(ConsoleURL), "~s/console/", [SURI]),
    http_get(ConsoleURL, _, []).

:- end_tests(console_route).

/*
 * message_handler(+Method,+Request) is det.
 */
message_handler(options,_Request) :-
    config:server(SURI),
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

    config:server(SURI),
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

    config:server(SURI),
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
    config:server(SURI),
    open_descriptor(terminus_descriptor{}, DB),
    write_cors_headers(SURI,DB),
    format('~n').
db_handler(post,Account,DB,R) :-
    add_payload_to_request(R,Request), % this should be automatic.
    open_descriptor(terminus_descriptor{}, Terminus_DB),
    /* POST: Create database */
    authenticate(Terminus_DB, Request, Auth),

    assert_auth_action_scope(Terminus_DB, Auth, terminus:create_database, "terminus"),

    get_payload(Database_Document,Request),
    do_or_die(
        (_{ comment : Comment,
            label : Label } :< Database_Document),
        error(bad_api_document(Database_Document))),

    do_or_die(
        (_{ prefixes : Prefixes } :< Database_Document,
         _{ doc : _Doc, scm : _Scm} :< Prefixes),
        error(under_specified_prefixes(Database_Document))),

    user_database_name(Account, DB, DB_Name),

    try_create_db(DB_Name, Label, Comment, Prefixes),

    config:server(Server),
    write_cors_headers(Server, Terminus_DB),
    reply_json(_{'terminus:status' : 'terminus:success'}).
db_handler(delete,Account,DB,Request) :-
    /* DELETE: Delete database */
    open_descriptor(terminus_descriptor{}, Terminus_DB),
    authenticate(Terminus_DB, Request, Auth),

    user_database_name(Account, DB, DB_Name),
    assert_auth_action_scope(Terminus_DB, Auth, terminus:delete_database, DB_Name),

    try_delete_db(DB_Name),

    config:server(Server),
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
         cleanup(delete_db(DB))
     ]) :-
    config:server(Server),
    atomic_list_concat([Server, '/db/TERMINUS_QA/TEST_DB'], URI),
    Doc = _{ prefixes : _{ doc : "https://terminushub.com/document",
                           scm : "https://terminushub.com/schema"},
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
                create_db_without_schema(DB,'test','a test')))
     ]) :-
    config:server(Server),
    atomic_list_concat([Server, '/db/TERMINUS_QA/TEST_DB'], URI),
    admin_pass(Key),
    http_delete(URI, Delete_In, [json_object(dict),
                                 authorization(basic(admin, Key))]),

    _{'terminus:status' : "terminus:success"} = Delete_In.

test(db_auth_test, [
         setup((add_user('TERMINUS_QA','user@example.com','password',User_ID),
                user_database_name('TERMINUS_QA', 'TEST_DB', DB),
                (   database_exists(DB)
                ->  delete_db(DB)
                ;   true))),
         cleanup((user_database_name('TERMINUS_QA', 'TEST_DB', DB),
                  (   database_exists(DB)
                  ->  delete_db(DB)
                  ;   true),
                  delete_user(User_ID)))
     ]) :-

    config:server(Server),
    atomic_list_concat([Server, '/db/TERMINUS_QA/TEST_DB'], URI),
    Doc = _{ prefixes : _{ doc : "https://terminushub.com/document",
                           scm : "https://terminushub.com/schema"},
             comment : "A quality assurance test",
             label : "A label"
           },

    http_post(URI, json(Doc),
              In, [json_object(dict),
                   authorization(basic('TERMINUS_QA', "password"))]),
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
            filter: type_name_filter{ type: Graph.type,
                                      names: [Graph.name] }
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
    ;   throw(error(bad_api_document(Triples_Document)))),

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
                create_db_without_schema(DB,'test','a test'))),
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


test(triples_post_get, [
         setup((user_database_name('admin', 'Jumanji', DB),
                (   database_exists(DB)
                ->  delete_db(DB)
                ;   true),
                create_db_without_schema(DB,'test','a test'))),
         cleanup((user_database_name('admin', 'Jumanji', DB),
                  delete_db(DB)))
     ])
:-

    resolve_absolute_string_descriptor('admin/Jumanji',Branch_Descriptor),

    create_graph(Branch_Descriptor,
                 commit_info{ author : "test",
                              message: "Generated by automated testing"},
                 schema,
                 "main",
                 _Transaction_Metadata),

    TTL = "
@prefix layer: <http://terminusdb.com/schema/layer#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

layer:LayerIdRestriction a owl:Restriction.",

    config:server(Server),
    atomic_list_concat([Server, '/triples/admin/Jumanji/local/branch/master/schema/main'], URI),
    admin_pass(Key),

    http_post(URI, json(_{commit_info : _{ author : "Test",
                                           message : "testing" },
                          turtle : TTL}),
              _In, [json_object(dict),
                   authorization(basic(admin, Key))]),

    http_get(URI, Result, [json_object(dict),
                           authorization(basic(admin, Key))]),

    once(sub_string(Result, _Before, _Length, _After,
                    "layer:LayerIdRestriction\n  a owl:Restriction")).

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
    config:server(SURI),
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
    config:server(SURI),
    open_descriptor(terminus_descriptor{}, Terminus),
    write_cors_headers(SURI, Terminus),
    format('~n').
woql_handler(post, R) :-
    add_payload_to_request(R,Request),
    open_descriptor(terminus_descriptor{}, Terminus),
    authenticate(Terminus, Request, Auth_ID),
    % No descriptor to work with until the query sets one up
    empty_context(Context),

    woql_run_context(Request, Terminus, Auth_ID, Context, JSON),

    config:server(SURI),
    write_cors_headers(SURI, Terminus),
    reply_json_dict(JSON).

woql_handler(options, _Path, _Request) :-
    config:server(SURI),
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

    context_extend_prefixes(Context,Prefixes,Context0),

    collect_posted_files(Request,Files),

    merge_dictionaries(
        query_context{
            commit_info : Commit_Info,
            files : Files,
            terminus: Terminus,
            update_guard : _Guard,
            authorization : Auth_ID
        }, Context0, Final_Context),

    * http_log('~N[Request] ~q~n', [Request]),

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
                create_db_without_schema(Name, 'test','a test'))),
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
                create_db_without_schema(Name, 'test','a test'))),
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
                create_db_without_schema(Name,'test','a test'))),
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
    context_extend_prefixes(Pre_Database2,Prefixes,Database1),
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
:- http_handler(root(clone/Account/DB), cors_catch(clone_handler(Method, Account, DB)),
                [method(Method),
                 methods([options,post])]).

clone_handler(options, _, _, _Request) :-
    config:server(SURI),
    open_descriptor(terminus_descriptor{}, Terminus),
    write_cors_headers(SURI, Terminus),
    format('~n').
clone_handler(post, Account, DB, R) :-
    add_payload_to_request(R,Request), % this should be automatic.

    open_descriptor(terminus_descriptor{}, Terminus_DB),
    authenticate(Terminus_DB, Request, Auth),

    assert_auth_action_scope(Terminus_DB, Auth, terminus:create_database, "terminus"),

    request_remote_authorization(Request, Authorization),
    get_payload(Database_Document,Request),

    do_or_die(
        (_{ comment : Comment,
            label : Label } :< Database_Document),
        error(bad_api_document(Database_Document))),

    do_or_die(
        (_{ remote_url : Remote_URL } :< Database_Document),
        error(no_remote_specified(Database_Document))),

    clone(Account,DB,Label,Comment,Remote_URL,authorized_fetch(Authorization),_Meta_Data),

    resolve_absolute_descriptor([Account,DB], Descriptor),

    write_descriptor_cors(Descriptor, Terminus_DB),
    reply_json_dict(
        _{'terminus:status' : 'terminus:success'}).

%%%%%%%%%%%%%%%%%%%% Fetch Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(fetch/Path), cors_catch(fetch_handler(Method,Path)),
                [method(Method),
                 prefix,
                 methods([options,post])]).

fetch_handler(options, _Path, _Request) :-
    config:server(SURI),
    open_descriptor(terminus_descriptor{}, Terminus),
    write_cors_headers(SURI, Terminus),
    format('~n').
fetch_handler(post,Path,Request) :-
    request_remote_authorization(Request, Authorization),
    % Calls pack on remote
    resolve_absolute_string_descriptor(Path,Repository_Descriptor),

    remote_fetch(Repository_Descriptor, authorized_fetch(Authorization),
                 New_Head_Layer_Id, Head_Has_Updated),

    reply_json_dict(
            _{'terminus:status' : 'terminus:success',
              'head_has_changed' : Head_Has_Updated,
              'head' : New_Head_Layer_Id}).

remote_pack_url(URL, Pack_URL) :-
    pattern_string_split('/', URL, [Protocol,Blank,Server|Rest]),
    merge_separator_split(Pack_URL,'/',[Protocol,Blank,Server,"pack"|Rest]).

authorized_fetch(Authorization, URL, Repository_Head_Option, Payload_Option) :-
    (   some(Repository_Head) = Repository_Head_Option
    ->  Document = _{ repository_head: Repository_Head }
    ;   Document = _{}),

    remote_pack_url(URL,Pack_URL),

    http_post(Pack_URL,
              json(Document),
              Payload,
              [request_header('Authorization'=Authorization),
               status_code(Status)]),

    (   Status = 200
    ->  Payload_Option = some(Payload)
    ;   Status = 204
    ->  Payload_Option = none
    ;   throw(error(remote_connection_error(Payload)))).


%%%%%%%%%%%%%%%%%%%% Rebase Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(rebase/Path), cors_catch(rebase_handler(Method,Path)),
                [method(Method),
                 prefix,
                 methods([options,post])]).


rebase_handler(options, _Path, _Request) :-
    config:server(SURI),
    open_descriptor(terminus_descriptor{}, Terminus),
    write_cors_headers(SURI, Terminus),
    format('~n').
rebase_handler(post, Path, R) :-
    add_payload_to_request(R,Request),
    open_descriptor(terminus_descriptor{}, Terminus),
    authenticate(Terminus, Request, Auth_ID),
    % No descriptor to work with until the query sets one up

    resolve_absolute_string_descriptor(Path, Our_Descriptor),
    check_descriptor_auth(Terminus, Our_Descriptor, terminus:rebase, Auth_ID),

    get_payload(Document, Request),
    (   get_dict(rebase_from, Document, Their_Path)
    ->  resolve_absolute_string_descriptor(Their_Path, Their_Descriptor)
    ;   throw(error(rebase_from_missing))),
    check_descriptor_auth(Terminus, Their_Descriptor, terminus:instance_read_access, Auth_ID),
    check_descriptor_auth(Terminus, Their_Descriptor, terminus:schema_read_access, Auth_ID),

    (   get_dict(author, Document, Author)
    ->  true
    ;   throw(error(rebase_author_missing))),

    Strategy_Map = [],
    rebase_on_branch(Our_Descriptor,Their_Descriptor, Author, Auth_ID, Strategy_Map, Common_Commit_ID_Option, Forwarded_Commits, Reports),

    Incomplete_Reply = _{ 'terminus:status' : "terminus:success",
                          forwarded_commits : Forwarded_Commits,
                          reports: Reports
                        },
    (   Common_Commit_ID_Option = some(Common_Commit_ID)
    ->  Reply = (Incomplete_Reply.put(common_commit_id, Common_Commit_ID))
    ;   Reply = Incomplete_Reply),
    
    reply_json_dict(Reply).

:- begin_tests(rebase_endpoint).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(rebase_divergent_history, [
         setup((config:server(Server),
                user_database_name('TERMINUSQA',foo, Name),
                (   database_exists(Name)
                ->  delete_db(Name)
                ;   true),
                create_db_without_schema(Name, 'test','a test'))),
         cleanup((user_database_name('TERMINUSQA',foo, Name),
                  delete_db(Name)))
     ])
:-

    resolve_absolute_string_descriptor("TERMINUSQA/foo", Master_Descriptor),
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit a"}, Master_Context1),
    with_transaction(Master_Context1,
                     ask(Master_Context1,
                         insert(a,b,c)),
                    _),

    branch_create(Master_Descriptor.repository_descriptor, Master_Descriptor, "second", _),
    resolve_absolute_string_descriptor("TERMINUSQA/foo/local/branch/second", Second_Descriptor),

    create_context(Second_Descriptor, commit_info{author:"test",message:"commit b"}, Second_Context1),
    with_transaction(Second_Context1,
                     ask(Second_Context1,
                         (   insert(d,e,f),
                             delete(a,b,c))),
                     _),

    create_context(Second_Descriptor, commit_info{author:"test",message:"commit c"}, Second_Context2),
    with_transaction(Second_Context2,
                     ask(Second_Context2,
                         insert(g,h,i)),
                     _),

    % we're also doing a commit on the original branch, to create a divergent history
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit d"}, Master_Context2),
    with_transaction(Master_Context2,
                     ask(Master_Context2,
                         insert(j,k,l)),
                     _),

    config:server(Server),
    atomic_list_concat([Server, '/rebase/TERMINUSQA/foo'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{rebase_from: 'TERMINUSQA/foo/local/branch/second',
                     author : "Gavsky"}),
              JSON,
              [json_object(dict),authorization(basic(admin,Key))]),

    * json_write_dict(current_output, JSON, []),

    _{
        forwarded_commits : [_Thing, _Another_Thing ],
        common_commit_id : _Common_Something,
        reports: _Reports,
        'terminus:status' : "terminus:success"
    } :< JSON,

    Repository_Descriptor = Master_Descriptor.repository_descriptor,
    branch_head_commit(Repository_Descriptor, "master", Commit_Uri),
    commit_uri_to_history_commit_ids(Repository_Descriptor, Commit_Uri, [Commit_A, Commit_B, Commit_C, Commit_D]),

    commit_id_to_metadata(Repository_Descriptor, Commit_A, "test", "commit a", _),
    commit_id_to_metadata(Repository_Descriptor, Commit_B, "test", "commit b", _),
    commit_id_to_metadata(Repository_Descriptor, Commit_C, "test", "commit c", _),
    commit_id_to_metadata(Repository_Descriptor, Commit_D, "Gavsky", "commit d", _).
:- end_tests(rebase_endpoint).

%%%%%%%%%%%%%%%%%%%% Pack Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(pack/Path), cors_catch(pack_handler(Method,Path)),
                [method(Method),
                 methods([options,post])]).

pack_handler(post,Path,R) :-
    open_descriptor(terminus_descriptor{}, Terminus),
    authenticate(Terminus, R, Auth_ID),

    atomic_list_concat([Path, '/local/_commits'], Repository_Path),
    resolve_absolute_string_descriptor(Repository_Path,
                                       Repository_Descriptor),

    do_or_die(
        (repository_descriptor{} :< Repository_Descriptor),
        error(not_a_repository_descriptor(Repository_Descriptor))),

    do_or_die(create_context(Repository_Descriptor, Pre_Context),
              error(unknown_repository_descriptor(Repository_Descriptor))),

    merge_dictionaries(
        query_context{
            authorization : Auth_ID,
            terminus : Terminus
        }, Pre_Context, Context),

    assert_read_access(Context),

    add_payload_to_request(R,Request),
    get_payload(Document,Request),

    (   _{ repository_head : Layer_ID } :< Document
    ->  Repo_Head_Option = some(Layer_ID)
    ;   _{} :< Document
    ->  Repo_Head_Option = none
    ;   throw(error(bad_api_document(Document)))),

    repository_context__previous_head_option__payload(
        Context, Repo_Head_Option, Payload_Option),

    (   Payload_Option = some(Payload)
    ->  throw(http_reply(bytes('application/octets',Payload)))
    ;   throw(http_reply(bytes('application/octets',"No content"),[status(204)]))).

% Currently just sending binary around...
:- begin_tests(pack_endpoint).
:- use_module(core(util/test_utils)).
%:- use_module(core(transaction)).
%:- use_module(core(api)).
:- use_module(library(http/http_open)).
:- use_module(library(terminus_store)).

test(pack_stuff, [
         % blocked('Blocked due to build problems - missing new store?'),
         setup((user_database_name('_a_test_user_', foo, DB_Name),
                (   database_exists(DB_Name)
                ->  delete_db(DB_Name)
                ;   true),
                (   agent_name_exists(terminus_descriptor{}, '_a_test_user_')
                ->  agent_name_uri(terminus_descriptor{}, '_a_test_user_', Old_User_ID),
                    delete_user(Old_User_ID)
                ;   true),
                add_user('_a_test_user_','user@example.com','password',User_ID),
                create_db_without_schema(DB_Name,'foo','a test'),
                make_user_own_database('_a_test_user_',DB_Name)
               )),
         cleanup((delete_db(DB_Name),
                  delete_user(User_ID)))
     ]) :-

    resolve_absolute_string_descriptor('_a_test_user_/foo', Descriptor),

    % First commit
    create_context(Descriptor, commit_info{author:"user",message:"commit a"}, Master_Context1),
    with_transaction(Master_Context1,
                     ask(Master_Context1,
                         insert(a,b,c)),
                     _),

    % Second commit
    create_context(Descriptor, commit_info{author:"user",message:"commit b"}, Master_Context2),
    % Before updating, grab the repository head layer_ID
    [Branch_Transaction] = (Master_Context2.transaction_objects),
    Repository = (Branch_Transaction.parent),
    repository_head_layerid(Repository,Repository_Head_Layer_ID),

    with_transaction(Master_Context2,
                     ask(Master_Context2,
                         (   insert(d,e,f),
                             delete(a,b,c))),
                     _),


    config:server(Server),
    atomic_list_concat([Server, '/pack/_a_test_user_/foo'], URI),

    Document = _{ repository_head : Repository_Head_Layer_ID },
    http_post(URI,
              json(Document),
              Data,
              [authorization(basic('_a_test_user_','password'))]),

    payload_repository_head_and_pack(Data, Head, Pack),

    % Check pack validity
    create_context(Descriptor, commit_info{author:"user",message:"commit b"}, New_Head_Context),
    % grab the repository head layer_ID and new graph layer_ID
    [New_Head_Transaction] = (New_Head_Context.transaction_objects),
    New_Head_Repository = (New_Head_Transaction.parent),
    repository_head_layerid(New_Head_Repository,New_Repository_Head_Layer_Id),
    [Instance_Graph] = (New_Head_Transaction.instance_objects),
    Layer = (Instance_Graph.read),
    layer_to_id(Layer,Layer_Id),

    pack_layerids_and_parents(Pack,Layerids_and_Parents),

    memberchk(New_Repository_Head_Layer_Id-_,Layerids_and_Parents),
    memberchk(Layer_Id-_,Layerids_and_Parents),
    memberchk(Head-_,Layerids_and_Parents),

    Head = New_Repository_Head_Layer_Id.


test(pack_nothing, [
         % blocked('causing travis to die'),
         setup((user_database_name('_a_test_user_', foo, DB_Name),
                (   database_exists(DB_Name)
                ->  delete_db(DB_Name)
                ;   true),
                (   agent_name_exists(terminus_descriptor{}, '_a_test_user_')
                ->  agent_name_uri(terminus_descriptor{}, '_a_test_user_', Old_User_ID),
                    delete_user(Old_User_ID)
                ;   true),
                add_user('_a_test_user_','user@example.com','password',User_ID),
                create_db_without_schema(DB_Name,'foo','a test'),
                make_user_own_database('_a_test_user_',DB_Name)
               )),
         cleanup((delete_db(DB_Name),
                  delete_user(User_ID)))
     ]) :-

    resolve_absolute_string_descriptor('_a_test_user_/foo', Descriptor),
    Repository_Descriptor = (Descriptor.repository_descriptor),
    open_descriptor(Repository_Descriptor, Repo_Transaction),
    repository_head_layerid(Repo_Transaction, Repository_Head_Layer_ID),

    Document = _{ repository_head : Repository_Head_Layer_ID },
    config:server(Server),
    atomic_list_concat([Server, '/pack/_a_test_user_/foo'], URI),
    http_post(URI,
              json(Document),
              _Data,
              [authorization(basic('_a_test_user_','password')),status_code(Status)]),
    Status = 204.

:- end_tests(pack_endpoint).

%%%%%%%%%%%%%%%%%%%% Unpack Handlers %%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(unpack/Path), cors_catch(unpack_handler(Method,Path)),
                [method(Method),
                 methods([options,post])]).

unpack_handler(options, _Path, _Request) :-
    config:server(SURI),
    open_descriptor(terminus_descriptor{}, Terminus),
    write_cors_headers(SURI, Terminus),
    format('~n').
unpack_handler(post, Path, R) :-
    add_payload_to_request(R,Request),

    open_descriptor(terminus_descriptor{}, Terminus),
    authenticate(Terminus, R, Auth_ID),

    string_concat(Path, "/local/_commits", Full_Path),
    do_or_die(
        (   resolve_absolute_string_descriptor(Full_Path,Repository_Descriptor),
            (repository_descriptor{} :< Repository_Descriptor)),
        reply_json(_{'@type' : "vio:UnpackPathInvalid",
                           'terminus:status' : "terminus:failure",
                           'terminus:message' : "The path to the database to unpack to was invalid"
                    },
                   400)),

    check_descriptor_auth(Terminus, Repository_Descriptor,
                          terminus:commit_write_access,
                          Auth_ID),

    get_payload(Payload, Request),

    catch(
        (   unpack(Repository_Descriptor, Payload),
            Json_Reply = _{'terminus:status' : "terminus:success"},
            Status = 200
        ),
        E,
        (   E = error(Inner_E)
        ->  (   Inner_E = not_a_linear_history_in_unpack(_History)
            ->  Json_Reply = _{'@type' : "vio:NotALinearHistory",
                               'terminus:status' : "terminus:failure",
                               'terminus:message' : "Not a linear history"
                              },
                Status = 400
            ;   Inner_E = unknown_layer_reference(Layer_Id)
            ->  Json_Reply = _{'@type' : "vio:UnknownLayerReferenceInPack",
                               'terminus:status' : "terminus:failure",
                               'terminus:message' : "A layer in the pack has an unknown parent",
                               'layer_id' : _{'@type': "xsd:string",
                                              '@value' : Layer_Id}
                              },
                Status = 400
            ;   Inner_E = database_not_found(_)
            ->  Json_Reply = _{'@type' : "vio:UnpackDestinationDatabaseNotFound",
                               'terminus:status' : "terminus:failure",
                               'terminus:message' : "The database to unpack to has not be found"
                              },
                Status = 400
            ;   throw(E))
        ;   throw(E))
    ),

    write_descriptor_cors(Repository_Descriptor, terminus_descriptor{}),
    reply_json(Json_Reply,
               [status(Status)]).

:- begin_tests(unpack_endpoint).
:- end_tests(unpack_endpoint).

%%%%%%%%%%%%%%%%%%%% Push Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(push/Path), cors_catch(push_handler(Method,Path)),
                [method(Method),
                 prefix,
                 methods([options,post])]).

% NOTE: We do this everytime - it should be handled automagically.
push_handler(options, _Path, _Request) :-
    config:server(SURI),
    open_descriptor(terminus_descriptor{}, Terminus),
    write_cors_headers(SURI, Terminus),
    format('~n').
push_handler(post,Path,R) :-
    add_payload_to_request(R,Request),
    open_descriptor(terminus_descriptor{}, Terminus),
    authenticate(Terminus, Request, Auth_ID),

    resolve_absolute_string_descriptor(Path,Branch_Descriptor),

    do_or_die(
        (branch_descriptor{} :< Branch_Descriptor),
        error(push_requires_branch_descriptor(Branch_Descriptor))),

    get_payload(Document, Request),

    do_or_die(
        _{ remote : Remote_Name,
           remote_branch : Remote_Branch } :< Document,
        error(push_has_no_remote(Document))),

    do_or_die(
        request_remote_authorization(Request, Authorization),
        error(no_remote_authorization)),

    push(Branch_Descriptor,Remote_Name,Remote_Branch,Auth_ID,
         authorized_push(Authorization),Result),

    (   Result = none
    ->  write_descriptor_cors(Branch_Descriptor, terminus_descriptor{}),
        reply_json(_{'terminus:status' : "terminus:success"})
    ;   Result = some(Head_ID)
    ->  reply_json(_{'terminus:status' : "terminus:success",
                     'head' : Head_ID})
    ;   throw(error(internal_server_error))).

remote_unpack_url(URL, Pack_URL) :-
    pattern_string_split('/', URL, [Protocol,Blank,Server|Rest]),
    merge_separator_split(Pack_URL,'/',[Protocol,Blank,Server,"unpack"|Rest]).

% NOTE: What do we do with the remote branch? How do we send it?
authorized_push(Authorization, Remote_URL, Payload) :-
    remote_unpack_url(Remote_URL, Unpack_URL),

    catch(http_post(Unpack_URL,
                    bytes('application/octets',Payload),
                    Result,
                    [request_header('Authorization'=Authorization),
                     status_code(Status_Code)]),
          E,
          throw(error(communication_failure(E)))),

    (   200 = Status_Code
    ->  true
    ;   400 = Status_Code,
        Result :< _{'@type': Vio_Type}
    ->  (   Vio_Type = "vio:NotALinearHistory"
        ->  throw(error(history_diverged))
        ;   Vio_Type = "vio:UnpackDestinationDatabaseNotFound"
        ->  throw(error(remote_unknown))
        ;   throw(error(unknown_status_code(Status_Code, Result)))
        )
    ;   403 = Status_Code
    ->  throw(error(authorization_failure(Result)))
    ;   throw(error(unknown_status_code))
    ).

%%%%%%%%%%%%%%%%%%%% Pull Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(pull/Path), cors_catch(pull_handler(Method,Path)),
                [method(Method),
                 prefix,
                 methods([options,post])]).

% NOTE: We do this everytime - it should be handled automagically.
pull_handler(options, _Path, _Request) :-
    config:server(SURI),
    open_descriptor(terminus_descriptor{}, Terminus),
    write_cors_headers(SURI, Terminus),
    format('~n').
pull_handler(post,Path,R) :-
    add_payload_to_request(R,Request),
    open_descriptor(terminus_descriptor{}, Terminus),
    authenticate(Terminus, Request, Local_Auth),

    resolve_absolute_string_descriptor(Path,Branch_Descriptor),

    get_payload(Document, Request),
    % Can't we just ask for the default remote?
    do_or_die(
        _{ remote : Remote_Name,
           remote_branch : Remote_Branch_Name
         } :< Document,
        error(pull_has_no_remote(Document))),

    do_or_die(
        request_remote_authorization(Request, Remote_Auth),
        error(no_remote_authorization)),

    catch(
        pull(Branch_Descriptor, Local_Auth, Remote_Name, Remote_Branch_Name,
             authorized_fetch(Remote_Auth),
             Result),
        E,
        (   E = error(Inner_E)
        ->  (   Inner_E = not_a_valid_local_branch(_)
            ->  throw(reply_json(_{'terminus:status' : "terminus:failure",
                                   'terminus:message' : "Not a valid local branch"},
                                 400))
            ;   Inner_E = not_a_valid_remote_branch(_)
            ->  throw(reply_json(_{'terminus:status' : "terminus:failure",
                                   'terminus:message' : "Not a valid remote branch"},
                                 400))
            ;   throw(E))
        ;   throw(E))
    ),

    write_descriptor_cors(Branch_Descriptor, terminus_descriptor{}),
    reply_json(_{'terminus:status' : "terminus:success",
                 'report' : Result}).

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

    branch_create(Destination_Descriptor, Origin_Descriptor, Branch_Name, _Branch_Uri),

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
                create_db_without_schema(Name, 'test','a test'))),
         cleanup((user_database_name(admin,test, Name),
                  delete_db(Name)))
     ])
:-
    config:server(Server),
    atomic_list_concat([Server, '/branch/admin/test/local/branch/foo'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{prefixes : _{ doc : "https://terminushub.com/document",
                                   scm : "https://terminushub.com/schema"}
                    }),
              JSON,
              [json_object(dict),authorization(basic(admin,Key))]),
    * json_write_dict(current_output, JSON, []),

    resolve_absolute_string_descriptor("admin/test/local/_commits", Repository_Descriptor),

    has_branch(Repository_Descriptor, "foo").

test(create_branch_from_local_without_prefixes, [
         setup((config:server(Server),
                user_database_name(admin,test, Name),
                (   database_exists(Name)
                ->  delete_db(Name)
                ;   true),
                create_db_without_schema(Name, 'test','a test'))),
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

    has_branch(Repository_Descriptor, "foo").

test(create_branch_from_local_with_prefixes, [
         setup((config:server(Server),
                user_database_name(admin,test, Name),
                (   database_exists(Name)
                ->  delete_db(Name)
                ;   true),
                create_db_without_schema(Name, 'test','a test'))),
         cleanup((user_database_name(admin,test, Name),
                  delete_db(Name)))
     ])
:-
    config:server(Server),
    atomic_list_concat([Server, '/branch/admin/test/local/branch/foo'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{origin:'/admin/test/local/branch/master',
                     prefixes : _{ doc : "https://terminushub.com/document",
                                   scm : "https://terminushub.com/schema"}
                    }),
              JSON,
              [json_object(dict),authorization(basic(admin,Key))]),
    * json_write_dict(current_output, JSON, []),

    resolve_absolute_string_descriptor("admin/test/local/_commits", Repository_Descriptor),

    has_branch(Repository_Descriptor, "foo").

test(create_branch_that_already_exists_error, [
         setup((config:server(Server),
                user_database_name(admin,test, Name),
                (   database_exists(Name)
                ->  delete_db(Name)
                ;   true),
                create_db_without_schema(Name, 'test','a test'))),
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
                create_db_without_schema(Name, 'test','a test'))),
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
                create_db_without_schema(Name, 'test','a test'))),
         cleanup((user_database_name(admin,test, Name),
                  delete_db(Name)))
     ])
:-
    config:server(Server),
    atomic_list_concat([Server, '/branch/admin/test/local/branch/foo'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{origin:'/admin/test/local/_commits',
                     prefixes : _{ doc : "https://terminushub.com/document",
                                   scm : "https://terminushub.com/schema"}}),
              JSON,
              [json_object(dict),
               authorization(basic(admin,Key)),
               status_code(Status_Code)]),
    Status_Code = 400,
    * json_write_dict(current_output, JSON, []),

    resolve_absolute_string_descriptor("admin/test/local/_commits", Repository_Descriptor),

    \+ has_branch(Repository_Descriptor, "foo").

:- end_tests(branch_endpoint).

%%%%%%%%%%%%%%%%%%%% Prefix Handlers %%%%%%%%%%%%%%%%%%%%%%%%%

:- http_handler(root(prefixes/Path), cors_catch(prefix_handler(Method,Path)),
                [method(Method),
                 prefix,
                 methods([options,post])]).

prefix_handler(options, _Path, _R) :-

    throw(error(not_implemented)).

%%%%%%%%%%%%%%%%%%%% Create/Delete Graph Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(graph/Path), cors_catch(graph_handler(Method,Path)),
                [method(Method),
                 prefix,
                 methods([options,post,delete])]).

graph_handler(options, _Path, _Request) :-
    config:server(SURI),
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
                user_database_name(admin,"test", Name),
                (   database_exists(Name)
                ->  delete_db(Name)
                ;   true),
                create_db_without_schema(Name, 'test','a test'))),
         cleanup(delete_db(Name))
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


test(delete_graph, [
         setup((config:server(Server),
                user_database_name(admin,"test", Name),
                (   database_exists(Name)
                ->  delete_db(Name)
                ;   true),
                create_db_without_schema(Name, 'test','a test'),
                resolve_absolute_string_descriptor('admin/test',Branch_Descriptor),
                create_graph(Branch_Descriptor,
                             commit_info{ author : "test",
                                          message: "Generated by automated testing"},
                             schema,
                             "main",
                             _Transaction_Metadata))),
         cleanup(delete_db(Name))
     ])
:-

    config:server(Server),
    atomic_list_concat([Server, '/graph/admin/test/local/branch/master/schema/main'], URI),
    admin_pass(Key),
    Commit = commit_info{ author : 'Jeebuz', message : 'Hello my children' },
    http_get(URI,
             _JSON,
             [method(delete),
              post(json(_{commit_info : Commit})),
              json_object(dict),
              authorization(basic(admin,Key))]).

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
              customise_exception(E)
          )
         ),
    !.
cors_catch(_,_Request) :-
    cors_enable,
    % Probably should extract the path from Request
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:message' :
                 _{'@type' : 'xsd:string',
                   '@value' : 'Unexpected failure in request handler'}},
               [status(500)]).

customise_exception(reply_json(M,Status)) :-
    reply_json(M,
               [status(Status)]).
customise_exception(reply_json(M)) :-
    customise_exception(reply_json(M,200)).
customise_exception(syntax_error(M)) :-
    format(atom(OM), '~q', [M]),
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:witnesses' : [_{'@type' : 'vio:ViolationWithDatatypeObject',
                                           'vio:literal' : OM}]},
               [status(400)]).
customise_exception(error(syntax_error(M),_)) :-
    format(atom(OM), '~q', [M]),
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:witnesses' : [_{'@type' : 'vio:ViolationWithDatatypeObject',
                                           'vio:literal' : OM}]},
               [status(400)]).
customise_exception(error(woql_syntax_error(JSON,Path,Element))) :-
    json_woql_path_element_error_message(JSON,Path,Element,Message),
    reverse(Path,Director),
    reply_json(_{'@type' : 'vio:WOQLSyntaxError',
                 'terminus:message' : Message,
                 'vio:path' : Director,
                 'vio:query' : JSON},
               [status(400)]).
customise_exception(error(syntax_error(M))) :-
    format(atom(OM), '~q', [M]),
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:witnesses' : [_{'@type' : 'vio:ViolationWithDatatypeObject',
                                           'vio:literal' : OM}]},
               [status(400)]).
customise_exception(error(type_error(T,O),C)) :-
    format(atom(M),'Type error for ~q which should be ~q with context ~q', [O,T,C]),
    format(atom(OA), '~q', [O]),
    format(atom(TA), '~q', [T]),
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:witnesses' : [_{'@type' : 'vio:ViolationWithDatatypeObject',
                                           'vio:message' : M,
                                           'vio:type' : TA,
                                           'vio:literal' : OA}]},
               [status(400)]).
customise_exception(graph_sync_error(JSON)) :-
    reply_json(JSON,[status(500)]).
%customise_exception((method_not_allowed(
customise_exception(http_reply(method_not_allowed(JSON))) :-
    reply_json(JSON,[status(405)]).
customise_exception(http_reply(not_found(JSON))) :-
    reply_json(JSON,[status(404)]).
customise_exception(http_reply(authorize(JSON))) :-
    reply_json(JSON,[status(401)]).
customise_exception(http_reply(not_acceptable(JSON))) :-
    reply_json(JSON,[status(406)]).
customise_exception(time_limit_exceeded) :-
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:message' : 'Connection timed out'
               },
               [status(408)]).
customise_exception(error(unqualified_resource_id(Doc_ID))) :-
    format(atom(MSG), 'Document resource ~s could not be expanded', [Doc_ID]),
    reply_json(_{'terminus:status' : 'terminus_failure',
                 'terminus:message' : MSG,
                 'terminus:object' : Doc_ID},
               [status(400)]).
customise_exception(error(unknown_deletion_error(Doc_ID))) :-
    format(atom(MSG), 'unqualfied deletion error for id ~s', [Doc_ID]),
    reply_json(_{'terminus:status' : 'terminus_failure',
                 'terminus:message' : MSG,
                 'terminus:object' : Doc_ID},
               [status(400)]).
customise_exception(error(access_not_authorised(Auth,Action,Scope))) :-
    format(string(Msg), "Access to ~q is not authorised with action ~q and auth ~q",
           [Scope,Action,Auth]),
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:message' : Msg},
               [status(403)]).
customise_exception(error(schema_check_failure(Witnesses))) :-
    reply_json(Witnesses,
               [status(405)]).
customise_exception(error(database_not_found(DB))) :-
    format(atom(MSG), 'Database ~s could not be destroyed', [DB]),
    reply_json(_{'terminus:message' : MSG,
                 'terminus:status' : 'terminus:failure'},
               [status(400)]).
customise_exception(error(database_does_not_exist(DB))) :-
    format(atom(M), 'Database does not exist with the name ~q', [DB]),
    reply_json(_{'terminus:message' : M,
                 'terminus:status' : 'terminus:failure'},
               [status(400)]).
customise_exception(error(database_files_do_not_exist(DB))) :-
    format(atom(M), 'Database fields do not exist for database with the name ~q', [DB]),
    reply_json(_{'terminus:message' : M,
                 'terminus:status' : 'terminus:failure'},
               [status(400)]).
customise_exception(error(bad_api_document(Document))) :-
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:document' : Document},
               [status(400)]).
customise_exception(error(database_already_exists(DB))) :-
    format(atom(MSG), 'Database ~s already exists', [DB]),
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:object' : DB,
                 'terminus:message' : MSG,
                 'terminus:method' : 'terminus:create_database'},
               [status(409)]).
customise_exception(error(database_could_not_be_created(DB))) :-
    format(atom(MSG), 'Database ~s could not be created', [DB]),
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:message' : MSG,
                 'terminus:method' : 'terminus:create_database'},
               [status(409)]).
customise_exception(error(could_not_create_class_frame(Class))) :-
    format(atom(MSG), 'Class Frame could not be generated for class ~s', [Class]),
    reply_json(_{ 'terminus:message' : MSG,
                  'terminus:status' : 'terminus:failure',
                  'terminus:class' : Class},
               [status(400)]).
customise_exception(error(could_not_create_filled_class_frame(Instance))) :-
    format(atom(MSG), 'Class Frame could not be generated for instance ~s', [Instance]),
    reply_json(_{ 'terminus:message' : MSG,
                  'terminus:status' : 'terminus:failure',
                  'terminus:instance' : Instance},
               [status(400)]).
customise_exception(error(maformed_json(Atom))) :-
    format(atom(MSG), 'Malformed JSON Object ~q', [MSG]),
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:message' : MSG,
                 'terminus:object' : Atom},
               [status(400)]).
customise_exception(error(no_document_for_key(Key))) :-
    format(atom(MSG), 'No document in request for key ~q', [Key]),
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:message' : MSG,
                 'terminus:key' : Key},
               [status(400)]).
customise_exception(error(no_parameter_key_in_document(Key,Document))) :-
    format(atom(MSG), 'No parameter key ~q for method ~q', [Key,Document]),
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:message' : MSG,
                 'terminus:key' : Key,
                 'terminus:object' : Document},
               [status(400)]).
customise_exception(error(no_parameter_key_form_method(Key,Method))) :-
    format(atom(MSG), 'No parameter key ~q for method ~q', [Key,Method]),
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:message' : MSG,
                 'terminus:object' : Key},
               [status(400)]).
customise_exception(error(no_parameter_key(Key))) :-
    format(atom(MSG), 'No parameter key ~q', [Key]),
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:message' : MSG,
                 'terminus:object' : Key},
               [status(400)]).
customise_exception(error(branch_already_exists(Branch_Name))) :-
    format(string(Msg), "branch ~w already exists", [Branch_Name]),
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:message' : Msg},
               [status(400)]).
customise_exception(error(origin_branch_does_not_exist(Branch_Name))) :-
    format(string(Msg), "origin branch ~w does not exist", [Branch_Name]),
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:message' : Msg},
               [status(400)]).
customise_exception(error(origin_commit_does_not_exist(Commit_Id))) :-
    format(string(Msg), "origin commit ~w does not exist", [Commit_Id]),
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:message' : Msg},
               [status(400)]).
customise_exception(error(origin_cannot_be_branched(Descriptor))) :-
    format(string(Msg), "origin ~w cannot be branched", [Descriptor]),
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:message' : Msg},
               [status(400)]).
customise_exception(error(E)) :-
    format(atom(EM),'Error: ~q', [E]),
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:message' : EM},
               [status(500)]).
customise_exception(error(E, CTX)) :-
    http_log('~N[Exception] ~q~n',[error(E,CTX)]),
    format(atom(EM),'Error: ~q in CTX ~q', [E, CTX]),
    reply_json(_{'terminus:status' : 'terminus:failure',
                 'terminus:message' : EM},
               [status(500)]).
customise_exception(http_reply(Obj)) :-
    throw(http_reply(Obj)).
customise_exception(E) :-
    http_log('~N[Exception] ~q~n',[E]),
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

%%%%%%%%%%%%%%%%%%%% Response Predicates %%%%%%%%%%%%%%%%%%%%%%%%%

/********************************************************
 * Determinising predicates used in handlers            *
 *                                                      *
 ********************************************************/

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
    ;   throw(error(no_parameter_key_in_document(Key,Parts)))).
try_get_param(Key,Request,Value) :-
    % GET or POST (but not application/json)
    memberchk(method(Method), Request),
    (   memberchk(Method, [get,delete])
    ;   Method = post,
        \+ memberchk(content_type('application/json'), Request)),

    http_parameters(Request, [], [form_data(Data)]),

    (   memberchk(Key=Value,Data)
    <>  throw(error(no_parameter_key_in_document(Key,Data)))),
    !.
try_get_param(Key,Request,Value) :-
    % POST with JSON package
    memberchk(method(post), Request),
    memberchk(content_type('application/json'), Request),

    (   memberchk(payload(Document), Request)
        <>  throw(error(no_document_for_key(Key)))),

    (   get_dict(Key,Document,Value)
        <>  throw(error(no_parameter_key_in_document(Key,Document)))),
    !.
try_get_param(Key,Request,_Value) :-
    % OTHER with method
    memberchk(method(Method), Request),
    !,
    throw(error(no_parameter_key_for_method(Key,Method))).
try_get_param(Key,_Request,_Value) :-
    % Catch all.
    throw(error(no_parameter_key(Key))).

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


/*
 * try_atom_json(Atom,JSON) is det.
 */
try_atom_json(Atom,JSON) :-
    atom_json_dict(Atom, JSON, [])
    <> throw(error(malformed_json(Atom))).

/*
 * add_payload_to_request(Request:request,JSON:json) is det.
 *
 * Updates request with JSON-LD payload in payload(Document).
 * This should really be done automatically at request time
 * using the endpoint wrappers so we don't forget to do it.
 */
add_payload_to_request(Request,[multipart(Parts)|Request]) :-
    memberchk(content_type(ContentType), Request),
    http_parse_header_value(
        content_type, ContentType,
        media(multipart/'form-data', _)
    ),
    http_log('~Nmulti-part form?~n', []),
    !,
    http_read_data(Request, Parts, [on_filename(save_post_file)]).
add_payload_to_request(Request,[payload(Document)|Request]) :-
    memberchk(content_type('application/json'), Request),
    !,
    http_read_data(Request, Document, [json_object(dict)]).
add_payload_to_request(Request,[payload(Document)|Request]) :-
    memberchk(content_type(_Some_Other_Type), Request),
    !,
    http_read_data(Request, Document, []).
add_payload_to_request(Request,Request).

get_payload(Payload,Request) :-
    memberchk(payload(Payload),Request).

/*
 * request_remote_authorization(Request, Authorization) is det.
 */
request_remote_authorization(Request, Token) :-
    memberchk(authorization_remote(Token),Request).

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
    class_frame_jsonld(Database,ClassEx,Frame)
    <>  throw(error(could_not_create_class_frame(Class))).

/*
 * try_class_frame(Class,Database,Frame) is det.
 */
try_filled_frame(Instance,Database,Frame) :-
    prefix_expand(Instance, Database.prefixes, InstanceEx),
    filled_frame_jsonld(Database,InstanceEx,Frame)
    <> throw(could_not_create_filled_class_frame(Instance)).
