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
:- if(\+((config:jwt_public_key_path(Path), Path = ''))).
:- use_module(library(jwt_io)).
:- endif.

%%%%%%%%%%%%% API Paths %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Set base location
% We may want to allow this as a setting...
:- multifile http:location/3.
:- dynamic http:location/3.
http:location(root, '/', []).
http:location(api, '/api', []).

%%%%%%%%%%%%%%%%%%%% Connection Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(.), cors_handler(Method, connect_handler),
                [method(Method),
                 methods([options,get])]).

/**
 * connect_handler(+Method,+Request:http_request) is det.
 */
/* NOTE: Need to return list of databases and access rights */
connect_handler(get, Request, System_DB, Auth) :-
    user_object(System_DB, Auth, User_Obj),
    User_Obj2 = (User_Obj.put('system:user_key_hash', "")),

    write_cors_headers(Request),
    reply_json(User_Obj2).

:- begin_tests(jwt_auth, [
                   condition(getenv("TERMINUSDB_SERVER_JWT_PUBLIC_KEY_ID", testkey))
               ]
              ).

:- use_module(core(util/test_utils)).
/*
 * Tests assume that  setenv("TERMINUSDB_SERVER_JWT_PUBLIC_KEY_PATH", "test/public_key_test.key.pub")
 * setenv("TERMINUSDB_SERVER_JWT_PUBLIC_KEY_ID", "testkey") are set
 */
test(connection_authorized_user_jwt, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    atomic_list_concat([Server, '/api'], URL),
    Bearer = 'eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6InRlc3RrZXkifQ.eyJodHRwOi8vdGVybWludXNkYi5jb20vc2NoZW1hL3N5c3RlbSNhZ2VudF9uYW1lIjoiYWRtaW4iLCJodHRwOi8vdGVybWludXNkYi5jb20vc2NoZW1hL3N5c3RlbSN1c2VyX2lkZW50aWZpZXIiOiJhZG1pbkB1c2VyLmNvbSIsImlzcyI6Imh0dHBzOi8vdGVybWludXNodWIuZXUuYXV0aDAuY29tLyIsInN1YiI6ImF1dGgwfDVlZmVmY2NkYmIzMzEzMDAxMzlkNTAzNyIsImF1ZCI6WyJodHRwczovL3Rlcm1pbnVzaHViL3JlZ2lzdGVyVXNlciIsImh0dHBzOi8vdGVybWludXNodWIuZXUuYXV0aDAuY29tL3VzZXJpbmZvIl0sImlhdCI6MTU5Mzc2OTE4MywiYXpwIjoiTUpKbmRHcDB6VWRNN28zUE9UUVBtUkpJbVkyaG8wYWkiLCJzY29wZSI6Im9wZW5pZCBwcm9maWxlIGVtYWlsIn0.hxJphuKyWLbTLTgFq37tHQvNaxDwWxeOyDVbEemYoWDhBbSbjcjP034jJ0PhupYqtdadZV4un4j9QkJeYDLNtZLD7q4tErNK5bDw9gM1z9335BSu9htjLOZEF2_DqJYLGdbazWA3MAGkg6corOCXDVyBZpToekvylsGAMztkZaeAIgnsJlHxIIMMLQHrHNCBRPC1U6ZJQc7WZdgB-gefVlVQco0w8_Q0Z28DeshD9y3XChTMeTAAT-URwmz61RB6aUFMXpr4tTtYwyXGsWdu46LuDNxOV070eTybthDpDjyYSDsn-i4YbHvDGN5kUen9pw6b47CkSdhsSSjVQLsNkA',
    http_get(URL, _, [authorization(bearer(Bearer))]).

test(connection_unauthorized_user_jwt, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    atomic_list_concat([Server, '/api'], URL),

    % mangled the payload so it should not validate
    Bearer = 'eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6InRlc3RrZXkifQ.eyJodHRwOi8vdGVybWludXNkYi5jb20vc2NoZW1hL3N5c3RlbSNhZ2VudF9uYW1lIjoiYWRtaW4iLCJodHRwOi8vdGVybWludXNkYi5jb20vc2NoZW1hL3N5c3RlbSN1c2VyX2lkZW50aWZpZXIiOiJhZG1pbkB1c2VyLmNvbSIsImlzcyI6Imh0dHBzOi8vdGVybWludXNodW0000000000aDAuY29tLyIsInN1YiI6ImF1dGgwfDVlZmVmY2NkYmIzMzEzMDAxMzlkNTAzNyIsImF1ZCI6WyJodHRwczovL3Rlcm1pbnVzaHViL3JlZ2lzdGVyVXNlciIsImh0dHBzOi8vdGVybWludXNodWIuZXUuYXV0aDAuY29tL3VzZXJpbmZvIl0sImlhdCI6MTU5Mzc2OTE4MywiYXpwIjoiTUpKbmRHcDB6VWRNN28zUE9UUVBtUkpJbVkyaG8wYWkiLCJzY29wZSI6Im9wZW5pZCBwcm9maWxlIGVtYWlsIn0.hxJphuKyWLbTLTgFq37tHQvNaxDwWxeOyDVbEemYoWDhBbSbjcjP034jJ0PhupYqtdadZV4un4j9QkJeYDLNtZLD7q4tErNK5bDw9gM1z9335BSu9htjLOZEF2_DqJYLGdbazWA3MAGkg6corOCXDVyBZpToekvylsGAMztkZaeAIgnsJlHxIIMMLQHrHNCBRPC1U6ZJQc7WZdgB-gefVlVQco0w8_Q0Z28DeshD9y3XChTMeTAAT-URwmz61RB6aUFMXpr4tTtYwyXGsWdu46LuDNxOV070eTybthDpDjyYSDsn-i4YbHvDGN5kUen9pw6b47CkSdhsSSjVQLsNkA',
    http_get(URL, _, [authorization(bearer(Bearer)), status_code(Status)]),

    Status = 401.

:- end_tests(jwt_auth).

:- begin_tests(connect_handler).
:- use_module(core(util/test_utils)).

test(connection_authorised_user_http_basic, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    admin_pass(Key),
    atomic_list_concat([Server, '/api'], URL),

    http_get(URL, _, [authorization(basic(admin, Key))]).


test(connection_result_dbs, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    admin_pass(Key),
    atomic_list_concat([Server, '/api'], URL),

    http_get(URL, Result, [json_object(dict),authorization(basic(admin, Key))]),

    * json_write_dict(current_output, Result, []),

    _{ '@id' : "doc:admin",
       '@type':"system:User"
     } :< Result.

:- end_tests(connect_handler).

%%%%%%%%%%%%%%%%%%%% Message Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(message), cors_handler(Method, message_handler),
                [method(Method),
                 methods([options,get,post])]).

/*
 * message_handler(+Method,+Request) is det.
 */
message_handler(_Method, Request, _System_DB, _Auth) :-
    try_get_param('api:message',Request,Message),

    with_output_to(
        string(Payload),
        json_write(current_output, Message, [])
    ),

    http_log('~N[Message] ~s~n',[Payload]),

    write_cors_headers(Request),

    reply_json(_{'api:status' : 'api:success'}).

%%%%%%%%%%%%%%%%%%%% Database Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(db/Account/DB), cors_handler(Method, db_handler(Account, DB)),
                [method(Method),
                 methods([options,post,delete])]).

/**
 * db_handler(Method:atom,DB:atom,Request:http_request) is det.
 */
db_handler(post, Organization, DB, Request, System_DB, Auth) :-
    /* POST: Create database */
    get_payload(Database_Document,Request),
    do_or_die(
        (_{ comment : Comment,
            label : Label } :< Database_Document),
        error(bad_api_document(Database_Document,[comment,label]),_)),

    (   _{ prefixes : Input_Prefixes } :< Database_Document
    ->  (   _{ doc : Doc} :< Input_Prefixes
        ->  true
        ;   Doc = "terminusdb:///data/"),
        (   _{ scm : Scm} :< Input_Prefixes
        ->  true
        ;   Scm = "terminusdb:///schema#"),
        Prefixes = Input_Prefixes.put(_{ doc : Doc,
                                         scm : Scm })
    ;   Prefixes = _{ doc : "terminusdb:///data/",
                      scm : "terminusdb:///schema#" }),

    (   _{ public : Public } :< Database_Document
    ->  true
    ;   Public = false),

    (   _{ schema : Schema } :< Database_Document
    ->  true
    ;   Schema = false),

    catch_with_backtrace(
        (   create_db(System_DB, Auth, Organization, DB, Label, Comment, Public, Schema, Prefixes),
            cors_reply_json(Request, _{'@type' : 'api:DbCreateResponse',
                                       'api:status' : 'api:success'})),

        Error,

        do_or_die(create_db_error_handler(Error, Request),
                  Error)).
db_handler(delete,Organization,DB,Request, System_DB, Auth) :-
    /* DELETE: Delete database */
    catch_with_backtrace(
        (   delete_db(System_DB, Auth, Organization, DB),
            cors_reply_json(Request, _{'@type' : 'api:DbDeleteResponse',
                                       'api:status' : 'api:success'})),

        Error,

        do_or_die(delete_db_error_handler(Error, Request),
                  Error)).

create_db_error_handler(error(unknown_organization(Organization_Name),_), Request) :-
    format(string(Msg), "Organization ~s does not exist.", [Organization_Name]),
    cors_reply_json(Request,
                    _{'@type' : 'api:DbCreateErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{'@type' : 'api:UnknownOrganization',
                                      'api:organization_name' : Organization_Name},
                      'api:message' : Msg},
                    [status(400)]).
create_db_error_handler(error(database_already_exists(Organization_Name, Database_Name),_), Request) :-
    cors_reply_json(Request,
                    _{'@type' : 'api:DbCreateErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{'@type' : 'api:DatabaseAlreadyExists',
                                      'api:database_name' : Database_Name,
                                      'api:organization_name' : Organization_Name},
                      'api:message' : 'Database already exists.'},
                    [status(400)]).
create_db_error_handler(error(database_in_inconsistent_state,_), Request) :-
    cors_reply_json(Request,
                    _{'@type' : 'api:DbCreateErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{'@type' : 'api:DatabaseInInconsistentState'},

                      'api:message' : 'Database is in an inconsistent state. Partial creation has taken place, but server could not finalize the database.'},
                    [status(500)]).

delete_db_error_handler(error(unknown_organization(Organization_Name),_), Request) :-
    format(string(Msg), "Organization ~s does not exist.", [Organization_Name]),
    cors_reply_json(Request,
                    _{'@type' : 'api:DbDeleteErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{'@type' : 'api:UnknownOrganization',
                                      'api:organization_name' : Organization_Name},
                      'api:message' : Msg},
                    [status(400)]).
delete_db_error_handler(error(database_does_not_exist(Organization,Database), _), Request) :-
    format(string(Msg), "Database ~s/~s does not exist.", [Organization, Database]),
    cors_reply_json(Request,
                    _{'@type' : 'api:DbDeleteErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{'@type' : 'api:DatabaseDoesNotExist',
                                      'api:database_name' : Database,
                                      'api:organization_name' : Organization},
                      'api:message' : Msg},
                    [status(400)]).
delete_db_error_handler(error(database_not_finalized(Organization,Database), _), Request) :-
    format(string(Msg), "Database ~s/~s is not in a deletable state.", [Organization, Database]),
    cors_reply_json(Request,
                    _{'@type' : 'api:DbDeleteErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{'@type' : 'api:DatabaseNotFinalized',
                                      'api:database_name' : Database,
                                      'api:organization_name' : Organization},

                      'api:message' : Msg},
                    [status(400)]).
delete_db_error_handler(error(database_files_do_not_exist(Organization,Database), _), Request) :-
    format(string(Msg), "Database files for ~s/~s were missing unexpectedly.", [Organization, Database]),
    cors_reply_json(Request,
                    _{'@type' : 'api:DbDeleteErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{'@type' : 'api:DatabaseFilesDoNotExist',
                                      'api:database_name' : Database,
                                      'api:organization_name' : Organization},
                      'api:message' : Msg},
                    [status(500)]).

:- begin_tests(db_endpoint).

:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(db_create, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    atomic_list_concat([Server, '/api/db/admin/TEST_DB'], URI),
    Doc = _{ prefixes : _{ doc : "https://terminushub.com/document",
                           scm : "https://terminushub.com/schema"},
             comment : "A quality assurance test",
             label : "A label"
           },
    admin_pass(Key),
    http_post(URI, json(Doc),
              In, [json_object(dict),
                   authorization(basic(admin, Key))]),
    _{'api:status' : "api:success"} :< In.

test(db_create_bad_api_document, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    atomic_list_concat([Server, '/api/db/admin/TEST_DB'], URI),
    Doc = _{ label : "A label" },
    admin_pass(Key),
    http_post(URI, json(Doc),
              In, [json_object(dict),
                   status_code(Code),
                   authorization(basic(admin, Key))]),
    400 = Code,
    _{'@type' : "api:BadAPIDocumentErrorResponse",
      'api:error' : Error} :< In,
    _{'@type' : "api:RequiredFieldsMissing"} :< Error.

test(db_create_existing_errors, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    create_db_without_schema("admin", "TEST_DB"),
    atomic_list_concat([Server, '/api/db/admin/TEST_DB'], URI),
    Doc = _{ prefixes : _{ doc : "https://terminushub.com/document",
                           scm : "https://terminushub.com/schema"},
             comment : "A quality assurance test",
             label : "A label"
           },
    admin_pass(Key),
    http_post(URI, json(Doc),
              Result, [json_object(dict),
                       authorization(basic(admin, Key)),
                       status_code(Status)]),
    Status = 400,
    _{'api:status' : "api:failure"} :< Result.

test(db_create_in_unknown_organization_errors, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    atomic_list_concat([Server, '/api/db/THIS_ORG_DOES_NOT_EXIST/TEST_DB'], URI),
    Doc = _{ prefixes : _{ doc : "https://terminushub.com/document",
                           scm : "https://terminushub.com/schema"},
             comment : "A quality assurance test",
             label : "A label"
           },
    admin_pass(Key),
    http_post(URI, json(Doc),
              Result, [json_object(dict),
                       authorization(basic(admin, Key)),
                       status_code(Status)]),
    Status = 400,
    _{'api:status' : "api:failure"} :< Result.

test(db_create_unauthenticated_errors, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    atomic_list_concat([Server, '/api/db/admin/TEST_DB'], URI),
    Doc = _{ prefixes : _{ doc : "https://terminushub.com/document",
                           scm : "https://terminushub.com/schema"},
             comment : "A quality assurance test",
             label : "A label"
           },
    http_post(URI, json(Doc),
              Result, [json_object(dict),
                       authorization(basic(admin, "THIS_IS_NOT_THE_CORRECT_PASSWORD")),
                       status_code(Status)]),
    Status = 401,
    _{'api:status' : "api:failure"} :< Result.

test(db_create_unauthorized_errors, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    add_user("TERMINUSQA",'user1@example.com','a comment', some('password'),_User_ID),
    atomic_list_concat([Server, '/api/db/admin/TEST_DB'], URI),
    Doc = _{ prefixes : _{ doc : "https://terminushub.com/document",
                           scm : "https://terminushub.com/schema"},
             comment : "A quality assurance test",
             label : "A label"
           },
    http_post(URI, json(Doc),
              Result, [json_object(dict),
                       authorization(basic("TERMINUSQA", "password")),
                       status_code(Status)]),
    Status = 403,
    _{'api:status' : "api:failure"} :< Result.

test(db_delete, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    create_db_without_schema("admin", "TEST_DB"),
    atomic_list_concat([Server, '/api/db/admin/TEST_DB'], URI),
    admin_pass(Key),
    http_delete(URI, Delete_In, [json_object(dict),
                                 authorization(basic(admin, Key))]),

    _{'api:status' : "api:success"} :< Delete_In.

test(db_delete_unknown_organization_errors, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    atomic_list_concat([Server, '/api/db/THIS_ORG_DOES_NOT_EXIST/TEST_DB'], URI),
    admin_pass(Key),
    http_delete(URI,
                Result,
                [json_object(dict),
                 authorization(basic(admin, Key)),
                 status_code(Status)]),

    Status = 400,

    % TODO this test is actually equivalent to the one below.
    % We need to differentiate these errors better, but I don't want to validate the exact error message.
    % We need codes!
    _{'api:status' : "api:failure"} :< Result.

test(db_delete_nonexistent_errors, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    atomic_list_concat([Server, '/api/db/admin/TEST_DB'], URI),
    admin_pass(Key),
    http_delete(URI,
                Result,
                [json_object(dict),
                 authorization(basic(admin, Key)),
                 status_code(Status)]),

    Status = 400,

    _{'api:status' : "api:failure"} :< Result.


test(db_auth_test, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    add_user('TERMINUS_QA','user@example.com','comment', some('password'),_User_ID),

    atomic_list_concat([Server, '/api/db/TERMINUS_QA/TEST_DB'], URI),
    Doc = _{ prefixes : _{ doc : "https://terminushub.com/document",
                           scm : "https://terminushub.com/schema"},
             comment : "A quality assurance test",
             label : "A label"
           },

    http_post(URI, json(Doc),
              In, [json_object(dict),
                   authorization(basic('TERMINUS_QA', "password"))]),
    _{'api:status' : "api:success"} :< In.

:- end_tests(db_endpoint).


%%%%%%%%%%%%%%%%%%%% Triples Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(triples/Path), cors_handler(Method, triples_handler(Path)),
                [method(Method),
                 prefix,
                 time_limit(infinite),
                 methods([options,get,post,put])]).

/*
 * triples_handler(Mode,DB,Request) is det.
 *
 * Get or update a schema.
 */
triples_handler(get,Path,Request, System_DB, Auth) :-
    (   get_param('format', Request, Format)
    ->  true
    ;   Format = "turtle"
    ),
    catch_with_backtrace(
        (   graph_dump(System_DB, Auth, Path, Format, String),
            cors_reply_json(Request, String)),
        Error,
        do_or_die(triples_error_handler(Error, Request),
                  Error)).
triples_handler(post,Path,Request, System_DB, Auth) :-
    get_payload(Triples_Document,Request),
    do_or_die(_{ turtle : TTL,
                 commit_info : Commit_Info } :< Triples_Document,
              error(bad_api_document(Triples_Document,[turtle,commit_info]),_)),

    catch_with_backtrace(
        (   graph_update(System_DB, Auth, Path, Commit_Info, "turtle", TTL),
            cors_reply_json(Request, _{'@type' : 'api:TriplesUpdateResponse',
                                       'api:status' : "api:success"})),
        Error,
        do_or_die(triples_error_handler(Error, Request),
                  Error)).
triples_handler(put,Path,Request, System_DB, Auth) :-
    get_payload(Triples_Document,Request),
    do_or_die(_{ turtle : TTL,
                 commit_info : Commit_Info } :< Triples_Document,
              error(bad_api_document(Triples_Document,[turtle,commit_info]),_)),

    catch_with_backtrace(
        (   graph_insert(System_DB, Auth, Path, Commit_Info, "turtle", TTL),
            cors_reply_json(Request, _{'@type' : 'api:TriplesInsertResponse',
                                       'api:status' : "api:success"})),
        Error,
        do_or_die(triples_error_handler(Error, Request),
                  Error)).

triples_error_handler(error(unknown_format(Format), _), Request) :-
    format(string(Msg), "Unrecognized format: ~q", [Format]),
    cors_reply_json(Request,
                    _{'@type' : 'api:TriplesErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{'@type' : 'api:TriplesUnknownFormat',
                                      'api:format' : Format},
                      'api:message' : Msg},
                    [status(400)]).
triples_error_handler(error(invalid_graph_descriptor(Path), _), Request) :-
    format(string(Msg), "Invalid graph descriptor: ~q", [Path]),
    cors_reply_json(Request,
                    _{'@type' : 'api:TriplesErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{'@type' : 'api:BadAbsoluteGraphDescriptor',
                                      'api:absolute_graph_descriptor' : Path},
                      'api:message' : Msg},
                    [status(400)]).
triples_error_handler(error(unknown_graph(Graph_Descriptor), _), Request) :-
    resolve_absolute_string_graph_descriptor(Path, Graph_Descriptor),
    format(string(Msg), "Invalid graph descriptor (this graph may not exist): ~q", [Graph_Descriptor]),
    cors_reply_json(Request,
                    _{'@type' : 'api:TriplesErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{'@type' : 'api:UnresolvableAbsoluteGraphDescriptor',
                                      'api:absolute_graph_descriptor' : Path},
                      'api:message' : Msg},
                    [status(400)]).
triples_error_handler(error(schema_check_failure([Witness|_]), _), Request) :-
    format(string(Msg), "Schema did not validate after this update", []),
    cors_reply_json(Request,
                    _{'@type' : 'api:TriplesErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{'@type' : 'api:SchemaValidationError',
                                      'api:witness' : Witness},
                      'api:message' : Msg},
                    [status(400)]).


:- begin_tests(triples_endpoint).

:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(triples_update, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    create_db_without_schema(admin, 'TEST_DB'),

    % We actually have to create the graph before we can post to it!
    % First make the schema graph
    make_branch_descriptor(admin, 'TEST_DB', Branch_Descriptor),
    super_user_authority(Auth),
    create_graph(system_descriptor{},
                 Auth,
                 "admin/TEST_DB/local/branch/main/schema/main",
                 commit_info{ author : "test",
                              message: "Generated by automated testing"},
                 Transaction_Metadata),
    * json_write_dict(current_output,Transaction_Metadata, []),

    terminus_path(Path),
    interpolate([Path, '/terminus-schema/system_schema.owl.ttl'], TTL_File),
    read_file_to_string(TTL_File, TTL, []),
    atomic_list_concat([Server, '/api/triples/admin/TEST_DB/local/branch/main/schema/main'], URI),
    admin_pass(Key),
    http_post(URI, json(_{commit_info : _{ author : "Test",
                                           message : "testing" },
                          turtle : TTL}),
              _In, [json_object(dict),
                    authorization(basic(admin, Key)),
                    reply_header(_Fields)]),

    findall(A-B-C,
            ask(Branch_Descriptor,
                t(A, B, C, "schema/*")),
            Triples),
    memberchk('http://terminusdb.com/schema/system'-(rdf:type)-(owl:'Ontology'), Triples).


test(triples_get, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    atomic_list_concat([Server, '/api/triples/_system/schema/main'], URI),
    admin_pass(Key),
    http_get(URI, In, [json_object(dict),
                       authorization(basic(admin, Key))]),
    string(In).


test(triples_post_get, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    create_db_without_schema("admin", "Jumanji"),

    super_user_authority(Auth),
    create_graph(system_descriptor{},
                 Auth,
                 "admin/Jumanji/local/branch/main/schema/main",
                 commit_info{ author : "test",
                              message: "Generated by automated testing"},
                 _Transaction_Metadata),

    TTL = "
@prefix layer: <http://terminusdb.com/schema/layer#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

layer:LayerIdRestriction a owl:Restriction.",

    atomic_list_concat([Server, '/api/triples/admin/Jumanji/local/branch/main/schema/main'], URI),
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



test(triples_put_two, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    create_db_without_schema("admin", "Jumanji"),

    super_user_authority(Auth),
    create_graph(system_descriptor{},
                 Auth,
                 "admin/Jumanji/local/branch/main/schema/main",
                 commit_info{ author : "test",
                              message: "Generated by automated testing"},
                 _Transaction_Metadata),

    TTL = "
@prefix layer: <http://terminusdb.com/schema/layer#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

layer:LayerIdRestriction a owl:Restriction.",

    atomic_list_concat([Server, '/api/triples/admin/Jumanji/local/branch/main/schema/main'], URI),
    admin_pass(Key),


    http_put(URI, json(_{commit_info : _{ author : "Test",
                                           message : "testing" },
                         turtle : TTL}),
             _Result1, [json_object(dict),
                        authorization(basic(admin, Key))]),

    TTL2 = "
@prefix layer: <http://terminusdb.com/schema/layer#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

layer:LayerIdRestriction2 a owl:Restriction.",

    http_put(URI, json(_{commit_info : _{ author : "Test",
                                          message : "testing" },
                         turtle : TTL2}),
             _Result2, [json_object(dict),
                        authorization(basic(admin, Key))]),

    http_get(URI, Result, [json_object(dict),
                           authorization(basic(admin, Key))]),

    once(sub_string(Result, _, _, _,
                    "layer:LayerIdRestriction\n  a owl:Restriction")),

    once(sub_string(Result, _, _, _,
                    "layer:LayerIdRestriction2\n  a owl:Restriction")).


test(get_invalid_descriptor, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    atomic_list_concat([Server, '/api/triples/nonsense'], URI),
    admin_pass(Key),

    http_get(URI, In, [json_object(dict),
                        authorization(basic(admin, Key)),
                        status_code(Code)]),
    _{'api:message':_Msg,
      'api:status':"api:failure"} :< In,
    Code = 400.


test(get_bad_descriptor, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    atomic_list_concat([Server, '/api/triples/admin/fdsa'], URI),
    admin_pass(Key),

    http_get(URI, In, [json_object(dict),
                        authorization(basic(admin, Key)),
                        status_code(Code)]),
    _{'api:message':_,
      'api:status':"api:failure"} :< In,
    Code = 400.

:- end_tests(triples_endpoint).

%%%%%%%%%%%%%%%%%%%% Frame Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(frame/Path), cors_handler(Method, frame_handler(Path)),
                [method(Method),
                 prefix,
                 methods([options,post])]).

/**
 * frame_handler(+Mode, +DB, +Class_ID, +Request:http_request) is det.
 *
 * Establishes frame responses
 */
frame_handler(post, Path, Request, System_DB, Auth) :-
    get_payload(Doc,Request),

    (   get_dict(class,Doc,Class_URI)
    ->  catch_with_backtrace(
            api_class_frame(System_DB, Auth, Path, Class_URI, Frame),
            E,
            do_or_die(frame_error_handler(E, Request),
                      E))
    ;   get_dict(instance,Doc,Instance_URI)
    ->  catch_with_backtrace(
            api_filled_frame(System_DB, Auth, Path, Instance_URI, Frame),
            E,
            do_or_die(frame_error_handler(E, Request),
                      E))
    ),

    write_cors_headers(Request),
    reply_json(Frame).

frame_error_handler(error(instance_uri_has_unknown_prefix(K),_), Request) :-
    format(string(Msg), "Instance uri has unknown prefix: ~q", [K]),
    term_string(K, Key),
    cors_reply_json(Request,
                    _{'@type' : 'api:FrameErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:InstanceUriHasUnknownPrefix',
                                       'api:instance_uri' : Key},
                      'api:message' : Msg
                     },
                    [status(400)]).
frame_error_handler(error(class_uri_has_unknown_prefix(K),_), Request) :-
    format(string(Msg), "Class uri has unknown prefix: ~q", [K]),
    term_string(K, Key),
    cors_reply_json(Request,
                    _{'@type' : 'api:FrameErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:ClassUriHasUnknownPrefix',
                                       'api:class_uri' : Key},
                      'api:message' : Msg
                     },
                    [status(400)]).
frame_error_handler(error(could_not_create_class_frame(Class),_), Request) :-
    format(string(Msg), "Could not create class frame for class: ~q", [Class]),
    term_string(Class, Class_String),
    cors_reply_json(Request,
                    _{'@type' : 'api:FrameErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:CouldNotCreateClassFrame',
                                       'api:class_uri' : Class_String},
                      'api:message' : Msg
                     },
                    [status(404)]).
frame_error_handler(error(could_not_create_filled_class_frame(Instance),_), Request) :-
    format(string(Msg), "Could not create filled class frame for instance: ~q", [Instance]),
    term_string(Instance, Instance_String),
    cors_reply_json(Request,
                    _{'@type' : 'api:FrameErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:CouldNotCreateFilledClassFrame',
                                       'api:instance_uri' : Instance_String},
                      'api:message' : Msg
                     },
                    [status(404)]).
frame_error_handler(error(invalid_absolute_path(Path),_), Request) :-
    format(string(Msg), "The following absolute resource descriptor string is invalid: ~q", [Path]),
    cors_reply_json(Request,
                    _{'@type' : 'api:FrameErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:BadAbsoluteDescriptor',
                                       'api:absolute_descriptor' : Path},
                      'api:message' : Msg
                     },
                    [status(404)]).
frame_error_handler(error(unresolvable_collection(Descriptor),_), Request) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The following descriptor could not be resolved to a resource: ~q", [Path]),
    cors_reply_json(Request,
                    _{'@type' : 'api:FrameErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:UnresolvableAbsoluteDescriptor',
                                       'api:absolute_descriptor' : Path},
                      'api:message' : Msg
                     },
                    [status(404)]).

:- begin_tests(frame_endpoint).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(get_frame, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    atomic_list_concat([Server, '/api/frame/_system'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{ class : "system:Agent"
                    }),
              JSON, [json_object(dict),
                     authorization(basic(admin, Key))]),
    _{'@type':"system:Frame"} :< JSON.


test(get_filled_frame, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    atomic_list_concat([Server, '/api/frame/_system'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{ instance : "doc:admin"
                    }),
              JSON, [json_object(dict),
                     authorization(basic(admin, Key))]),
    _{'@type':"system:FilledFrame"} :< JSON.


test(bad_path_filled_frame, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    atomic_list_concat([Server, '/api/frame/garbage'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{ instance : "doc:admin"
                    }),
              JSON, [json_object(dict),
                     authorization(basic(admin, Key)),
                     status_code(Status)]),
    \+ Status = 200,
    JSON.'api:error'.'@type' = "api:BadAbsoluteDescriptor".


test(unresolvable_path_filled_frame, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    atomic_list_concat([Server, '/api/frame/believable/garbage'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{ instance : "doc:admin"
                    }),
              JSON, [json_object(dict),
                     authorization(basic(admin, Key)),
                     status_code(Status)]),

    \+ Status = 200,
    JSON.'api:error'.'@type' = "api:UnresolvableAbsoluteDescriptor".

:- end_tests(frame_endpoint).

%%%%%%%%%%%%%%%%%%%% WOQL Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
%
:- http_handler(api(woql), cors_handler(Method, woql_handler),
                [method(Method),
                 time_limit(infinite),
                 methods([options,post])]).
:- http_handler(api(woql/Path), cors_handler(Method, woql_handler(Path)),
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
woql_handler(post, Request, System_DB, Auth) :-
    woql_handler_helper(Request, System_DB, Auth, none).

woql_handler(post, Path, Request, System_DB, Auth) :-
    woql_handler_helper(Request, System_DB, Auth, some(Path)).

woql_handler_helper(Request, System_DB, Auth, Path_Option) :-
    try_get_param('query',Request,Query),

    (   get_param('commit_info', Request, Commit_Info)
    ->  true
    ;   Commit_Info = _{}
    ),
    collect_posted_files(Request,Files),

    (   get_param('all_witnesses', Request, All_Witnesses)
    ->  true
    ;   All_Witnesses = false),

    catch_with_backtrace(
        (   woql_query_json(System_DB, Auth, Path_Option, Query, Commit_Info, Files, All_Witnesses, JSON),
            write_cors_headers(Request),
            reply_json_dict(JSON)
        ),
        E,
        do_or_die(woql_error_handler(E, Request),
                  E)).

woql_error_handler(error(invalid_absolute_path(Path),_), Request) :-
    format(string(Msg), "The following absolute resource descriptor string is invalid: ~q", [Path]),
    cors_reply_json(Request,
                    _{'@type' : 'api:WoqlErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:BadAbsoluteDescriptor',
                                       'api:absolute_descriptor' : Path},
                      'api:message' : Msg
                     },
                    [status(404)]).
woql_error_handler(error(not_a_valid_descriptor(Descriptor), _), Request) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The source path ~q is not a valid descriptor for branching", [Path]),
    cors_reply_json(Request,
                    _{'@type' : "api:WoqlErrorResponse",
                      'api:status' : "api:failure",
                      'api:message' : Msg,
                      'api:error' : _{ '@type' : "api:NotASourceBranchDescriptorError",
                                       'api:absolute_descriptor' : Path}
                     },
                    [status(400)]).
woql_error_handler(error(unresolvable_collection(Descriptor),_), Request) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The following descriptor could not be resolved to a resource: ~q", [Path]),
    cors_reply_json(Request,
                    _{'@type' : 'api:WoqlErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:UnresolvableAbsoluteDescriptor',
                                       'api:absolute_descriptor' : Path},
                      'api:message' : Msg
                     },
                    [status(404)]).
woql_error_handler(error(woql_syntax_error(badly_formed_ast(Term)),_), Request) :-
    term_string(Term,String),
    format(string(Msg), "Badly formed ast after compilation with term: ~q", [Term]),
    cors_reply_json(Request,
                    _{'@type' : 'api:WoqlErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:WOQLSyntaxError',
                                       'api:error_term' : String},
                      'api:message' : Msg
                     },
                    [status(404)]).
woql_error_handler(error(woql_syntax_error(Term),_), Request) :-
    term_string(Term,String),
    format(string(Msg), "The : ~q", [String]),
    cors_reply_json(Request,
                    _{'@type' : 'api:WoqlErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:WOQLSyntaxError',
                                       'api:error_term' : String},
                      'api:message' : Msg
                     },
                    [status(404)]).
woql_error_handler(error(schema_check_failure(Witnesses),_), Request) :-
    cors_reply_json(Request,
                    Witnesses,
                    [status(405)]).
woql_error_handler(error(woql_instantiation_error(Vars),_), Request) :-
    format(string(Msg), "The following variables were unbound but must be bound: ~q", [Vars]),
    cors_reply_json(Request,
                    _{'@type' : 'api:WoqlErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:WOQLModeError',
                                       'api:error_vars' : Vars},
                      'api:message' : Msg
                     },
                    [status(404)]).
woql_error_handler(error(unresolvable_absolute_descriptor(Descriptor), _), Request) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The WOQL query referenced an invalid absolute path for descriptor ~q", [Path]),
    cors_reply_json(Request,
                    _{'@type' : "api:WoqlErrorResponse",
                      'api:status' : "api:failure",
                      'api:message' : Msg,
                      'api:error' : _{ '@type' : "api:UnresolvableAbsoluteDescriptor",
                                       'api:absolute_descriptor' : Path}
                     },
                    [status(400)]).



% woql_handler Unit Tests

:- begin_tests(woql_endpoint).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(db_not_there, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    atomic_list_concat([Server, '/api/woql/admin/blagblagblagblagblag'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{'query' : ""}),
              JSON,
              [status_code(Code), json_object(dict),authorization(basic(admin,Key))]),
    Code = 404,
    _{'@type' : "api:WoqlErrorResponse",
      'api:error' :
      _{'@type' : "api:UnresolvableAbsoluteDescriptor",
        'api:absolute_descriptor': "admin/blagblagblagblagblag/local/branch/main"}} :< JSON.

test(no_db, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    Query =
    _{'@type' : "Using",
      collection : _{'@type' : "xsd:string",
                     '@value' : "_system"},
      query :
      _{'@type' : "Select",
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
                                           predicate : "system:tag",
                                           object : "system:abstract",
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
                                            predicate : "system:tag",
                                            object : _{'@type' : "Variable",
                                                       variable_name : _{ '@type' : "xsd:string",
                                                                          '@value' : "Abstract"}},
                                            graph_filter : _{'@type' : "xsd:string",
                                                             '@value' : "schema/*"}}}}]}}},

    atomic_list_concat([Server, '/api/woql'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{'query' : Query}),
              JSON,
              [json_object(dict),authorization(basic(admin,Key))]),

    % extra debugging...
    % nl,
    * json_write_dict(current_output,JSON,[]),
    _{'bindings' : _L} :< JSON.

test(indexed_get, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
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

    atomic_list_concat([Server, '/api/woql'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{query : Query}),
              JSON,
              [json_object(dict),authorization(basic(admin,Key))]),

    (   _{'bindings' : _} :< JSON
    ->  true
    ;   fail).

test(named_get, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
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

    atomic_list_concat([Server, '/api/woql'], URI),
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
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    create_db_without_schema(admin,test),
    atomic_list_concat([Server, '/api/woql/admin/test'], URI),

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
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    create_db_without_schema(admin,test),
    atomic_list_concat([Server, '/api/woql/admin/test'], URI),

    % First make a schema against which we can have an object

    super_user_authority(Auth),
    create_graph(system_descriptor{},
                 Auth,
                 "admin/test/local/branch/main/schema/main",
                 commit_info{ author : "test",
                              message: "Generated by automated testing"},
                 _Transaction_Metadata2),

    terminus_path(Path),
    interpolate([Path, '/terminus-schema/system_schema.owl.ttl'], TTL_File),
    read_file_to_string(TTL_File, TTL, []),

    Graph = "admin/test/local/branch/main/schema/main",
    super_user_authority(Auth),
    graph_update(system_descriptor{}, Auth, Graph,
                 commit_info{
                     author : "Steve",
                     message : "Yeah I did it"},
                 "turtle", TTL),

    % TODO: We need branches to pull in the correct 'doc:' prefix.
    Query0 =
    _{'@context' : _{ doc: "http://terminusdb.com/admin/test/document/",
                      scm: "http://terminusdb.com/schema/system#"},
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

    Expected = [ _{'Object': _{'@type':"http://www.w3.org/2001/XMLSchema#string",
                               '@value':"Steve"},
                   'Predicate':"http://terminusdb.com/schema/system#resource_name",
                   'Subject':"http://terminusdb.com/admin/test/document/my_database"},
                 _{'Object':"http://terminusdb.com/schema/system#finalized",
                   'Predicate':"http://terminusdb.com/schema/system#database_state",
                   'Subject':"http://terminusdb.com/admin/test/document/my_database"},
                 _{'Object':"http://terminusdb.com/schema/system#Database",
                   'Predicate':"http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
                   'Subject':"http://terminusdb.com/admin/test/document/my_database"}],
    forall( member(Elt, Expected),
            member(Elt, (JSON1.bindings))
          ).


test(delete_object, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    create_db_without_schema(admin,test),

    make_branch_descriptor("admin","test",Branch_Descriptor),

    % First make a schema graph
    super_user_authority(Auth),
    create_graph(system_descriptor{},
                 Auth,
                 "admin/test/local/branch/main/schema/main",
                 commit_info{ author : "test",
                              message: "Generated by automated testing"},
                 _Transaction_Metadata2),

    % Create the schema
    terminus_path(Path),
    interpolate([Path, '/terminus-schema/system_schema.owl.ttl'], TTL_File),
    read_file_to_string(TTL_File, TTL, []),

    Graph = "admin/test/local/branch/main/schema/main",
    super_user_authority(Auth),
    graph_update(system_descriptor{}, Auth, Graph,
                 commit_info{
                     author : "Steve",
                     message : "Yeah I did it"},
                 "turtle", TTL),

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
                  scm: "http://terminusdb.com/schema/system#"},
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
      document_uri : 'doc:my_database'},

    Commit_Info = commit_info{
                      author : "Steve",
                      message : "Deleted baby"
                  },

    admin_pass(Key),
    atomic_list_concat([Server, '/api/woql/admin/test'], URI),
    http_post(URI,
              json(_{ query : Query1,
                      commit_info : Commit_Info}),
              JSON1,
              [json_object(dict),authorization(basic(admin,Key))]),

    * json_write_dict(current_output, JSON1, []),

    % Does it still exist?
    \+ ask(Branch_Descriptor,
           t('http://terminusdb.com/admin/test/document/my_database', _, _)).


test(get_object, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    Query0 =
    _{'@type' : "ReadObject",
      document_uri : _{ '@type' : "woql:Node",
                        'woql:node' : "doc:admin"},
      document : _{'@type' : "Variable",
                   variable_name : _{ '@type' : "xsd:string",
                                      '@value' : "Document"}}},

    admin_pass(Key),
    atomic_list_concat([Server, '/api/woql/_system'], URI),
    http_post(URI,
              json(_{query : Query0}),
              JSON0,
              [json_object(dict),authorization(basic(admin,Key))]),
    [Result] = (JSON0.bindings),

    _{'@id':"doc:admin",
      '@type':"system:User",
      'system:user_key_hash':_,
      'system:agent_name': _,
      'system:role': _}
    :< Result.'Document'.

:- end_tests(woql_endpoint).

%%%%%%%%%%%%%%%%%%%% Clone Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(clone/Organization/DB), cors_handler(Method, clone_handler(Organization, DB)),
                [method(Method),
                 methods([options,post])]).

clone_handler(post, Organization, DB, Request, System_DB, Auth) :-

    do_or_die(
        (get_payload(Database_Document,Request),
         _{ comment : Comment,
            label : Label,
            remote_url: Remote_URL} :< Database_Document),
        error(bad_api_document(Database_Document,[comment,label,remote_url]),_)),

    (   _{ public : Public } :< Database_Document
    ->  true
    ;   Public = false),

    catch_with_backtrace(
        (   do_or_die(
                request_remote_authorization(Request, Authorization),
                error(no_remote_authorization,_)),

            clone(System_DB, Auth, Organization,DB,Label,Comment,Public,Remote_URL,authorized_fetch(Authorization),_Meta_Data),
            write_cors_headers(Request),
            reply_json_dict(
                _{'@type' : 'api:CloneResponse',
                  'api:status' : 'api:success'})
        ),
        E,
        do_or_die(clone_error_handler(E,Request),
                  E)).

clone_error_handler(error(no_remote_authorization,_),Request) :-
    format(string(Msg), "No remote authorization supplied", []),
    cors_reply_json(Request,
                    _{'@type' : 'api:CloneErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:AuthorizationError'},
                      'api:message' : Msg
                     },
                    [status(401)]).
clone_error_handler(error(remote_connection_error(Payload),_),Request) :-
    format(string(Msg), "There was a failure to clone from the remote: ~q", [Payload]),
    cors_reply_json(Request,
                    _{'@type' : 'api:CloneErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:RemoteConnectionError'},
                      'api:message' : Msg
                     },
                    [status(404)]).


:- begin_tests(clone_endpoint).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(clone_local, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    add_user("TERMINUSQA1",'user1@example.com','a comment', some('password1'),_User_ID1),
    add_user("TERMINUSQA2",'user2@example.com','a comment', some('password2'),_User_ID2),
    create_db_without_schema("TERMINUSQA1", "foo"),
    resolve_absolute_string_descriptor("TERMINUSQA1/foo", Foo_Descriptor),
    create_context(Foo_Descriptor, commit_info{author:"test",message:"test"}, Foo_Context),
    with_transaction(Foo_Context,
                     ask(Foo_Context,
                         insert(a,b,c)),
                     _),

    atomic_list_concat([Server, '/api/clone/TERMINUSQA2/bar'], URL),
    atomic_list_concat([Server, '/TERMINUSQA1/foo'], Remote_URL),
    base64("TERMINUSQA1:password1", Base64_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Auth]),
    http_post(URL,
              json(_{comment: "hai hello",
                     label: "bar",
                     remote_url: Remote_URL}),

              JSON,
              [json_object(dict),authorization(basic('TERMINUSQA2','password2')),
               request_header('Authorization-Remote'=Authorization_Remote)]),

    * json_write_dict(current_output, JSON, []),

    _{
        'api:status' : "api:success"
    } :< JSON,

    resolve_absolute_string_descriptor("TERMINUSQA2/bar", Bar_Descriptor),
    once(ask(Bar_Descriptor,
             t(a,b,c))),

    true.

test(clone_remote, [
         setup(
             (   setup_temp_unattached_server(State_1,Store_1,Server_1),
                 setup_temp_unattached_server(State_2,Store_2,Server_2))),
         cleanup(
             (
                 teardown_temp_unattached_server(State_1),
                 teardown_temp_unattached_server(State_2)))
     ])
:-
    with_triple_store(
        Store_1,
        (   add_user("TERMINUSQA1",'user1@example.com','a comment', some('password1'),_User_ID1),
            create_public_db_without_schema("TERMINUSQA1", "foo"),
            resolve_absolute_string_descriptor("TERMINUSQA1/foo", Foo_Descriptor),
            create_context(Foo_Descriptor, commit_info{author:"test",message:"test"}, Foo_Context),
            with_transaction(Foo_Context,
                             ask(Foo_Context,
                                 insert(a,b,c)),
                             _)
        )
    ),

    with_triple_store(
        Store_2,
        (   add_user("TERMINUSQA2",'user2@example.com','a comment', some('password2'),_User_ID2)
        )
    ),

    atomic_list_concat([Server_2, '/api/clone/TERMINUSQA2/bar'], URL),
    atomic_list_concat([Server_1, '/TERMINUSQA1/foo'], Remote_URL),
    base64("TERMINUSQA1:password1", Base64_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Auth]),
    http_post(URL,
              json(_{comment: "hai hello",
                     label: "bar",
                     remote_url: Remote_URL}),

              JSON,
              [json_object(dict),authorization(basic('TERMINUSQA2','password2')),
               request_header('Authorization-Remote'=Authorization_Remote)]),

    * json_write_dict(current_output, JSON, []),

    _{
        'api:status' : "api:success"
    } :< JSON,

    with_triple_store(
        Store_2,
        (   resolve_absolute_string_descriptor("TERMINUSQA2/bar", Bar_Descriptor),
            once(ask(Bar_Descriptor,
                     t(a,b,c)))
        )
    ).


:- end_tests(clone_endpoint).

%%%%%%%%%%%%%%%%%%%% Fetch Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(fetch/Path), cors_handler(Method, fetch_handler(Path)),
                [method(Method),
                 prefix,
                 methods([options,post])]).

fetch_handler(post,Path,Request, System_DB, Auth) :-
    catch_with_backtrace(
        (   do_or_die(
                request_remote_authorization(Request, Authorization),
                error(no_remote_authorization_for_fetch,_)),

            remote_fetch(System_DB, Auth, Path, authorized_fetch(Authorization),
                         New_Head_Layer_Id, Head_Has_Updated),
            write_cors_headers(Request),
            reply_json_dict(
                _{'@type' : 'api:FetchRequest',
                  'api:status' : 'api:success',
                  'api:head_has_changed' : Head_Has_Updated,
                  'api:head' : New_Head_Layer_Id})),
        E,
        do_or_die(fetch_error_handler(E,Request),
                  E)).

fetch_error_handler(error(no_remote_authorization,_),Request) :-
    format(string(Msg), "No remote authorization supplied", []),
    cors_reply_json(Request,
                    _{'@type' : 'api:FetchErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:AuthorizationError'},
                      'api:message' : Msg
                     },
                    [status(401)]).
fetch_error_handler(error(invalid_absolute_path(Path),_), Request) :-
    format(string(Msg), "The following absolute resource descriptor string is invalid: ~q", [Path]),
    cors_reply_json(Request,
                    _{'@type' : 'api:FetchErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:BadAbsoluteDescriptor',
                                       'api:absolute_descriptor' : Path},
                      'api:message' : Msg
                     },
                    [status(404)]).
fetch_error_handler(error(unresolvable_collection(Descriptor),_), Request) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The following descriptor (which should be a repository) could not be resolved to a resource: ~q", [Path]),
    cors_reply_json(Request,
                    _{'@type' : 'api:FetchErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:UnresolvableAbsoluteDescriptor',
                                       'api:absolute_descriptor' : Path},
                      'api:message' : Msg
                     },
                    [status(404)]).

remote_pack_url(URL, Pack_URL) :-
    pattern_string_split('/', URL, [Protocol,Blank,Server|Rest]),
    merge_separator_split(Pack_URL,'/',[Protocol,Blank,Server,"api","pack"|Rest]).

is_local_https(URL) :-
    re_match('^https://127.0.0.1', URL, []).

authorized_fetch(Authorization, URL, Repository_Head_Option, Payload_Option) :-
    (   some(Repository_Head) = Repository_Head_Option
    ->  Document = _{ repository_head: Repository_Head }
    ;   Document = _{}),

    (   is_local_https(URL)
    ->  Additional_Options = [cert_verify_hook(cert_accept_any)]
    ;   Additional_Options = []),

    remote_pack_url(URL,Pack_URL),

    http_post(Pack_URL,
              json(Document),
              Payload,
              [request_header('Authorization'=Authorization),
               json_object(dict),
               status_code(Status)
              |Additional_Options]),

    (   Status = 200
    ->  Payload_Option = some(Payload)
    ;   Status = 204
    ->  Payload_Option = none
    ;   throw(error(remote_connection_error(Payload),_))).

:- begin_tests(fetch_endpoint).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(fetch_first_time, [
         setup(
             (   setup_temp_unattached_server(State_1,Store_1,Server_1),
                 setup_temp_unattached_server(State_2,Store_2,Server_2))),
         cleanup(
             (
                 teardown_temp_unattached_server(State_1),
                 teardown_temp_unattached_server(State_2)))
     ])
:-

    with_triple_store(
        Store_1,
        (
            add_user("TERMINUSQA1",'user1@example.com','a comment', some('password1'),_User_ID1),
            create_public_db_without_schema("TERMINUSQA1", "foo"),
            resolve_absolute_string_descriptor("TERMINUSQA1/foo", Foo_Descriptor),
            create_context(Foo_Descriptor, commit_info{author:"test",message:"test"}, Foo_Context),
            with_transaction(Foo_Context,
                             ask(Foo_Context,
                                 insert(a,b,c)),
                             _),
            get_dict(repository_descriptor, Foo_Descriptor, Foo_Repository_Desc),
            get_dict(database_descriptor, Foo_Repository_Desc, Foo_Database_Desc),
            repository_head(Foo_Database_Desc,"local",Head)
        )
    ),

    with_triple_store(
        Store_2,
        (
            add_user("TERMINUSQA2",'user2@example.com','a comment', some('password2'),_User_ID2),
            create_public_db_without_schema("TERMINUSQA2", "bar"),
            resolve_absolute_string_descriptor("TERMINUSQA2/bar", Bar_Descriptor),
            get_dict(repository_descriptor, Bar_Descriptor, Bar_Repository_Desc),
            get_dict(database_descriptor, Bar_Repository_Desc, Bar_Database_Desc),
            create_context(Bar_Database_Desc, commit_info{author:"test",message:"test"}, Bar_Database_Context),
            atomic_list_concat([Server_1, '/TERMINUSQA1/foo'], Remote_URL),
            with_transaction(
                Bar_Database_Context,
                insert_remote_repository(Bar_Database_Context, "origin", Remote_URL, _),
                _)
        )
    ),
    atomic_list_concat([Server_2, '/api/fetch/TERMINUSQA2/bar/origin/_commits'], URL),
    base64("TERMINUSQA1:password1", Base64_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Auth]),
    http_post(URL,
              json(_{}),
              JSON,
              [json_object(dict),authorization(basic('TERMINUSQA2','password2')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(200)]),

    _{ '@type' : "api:FetchRequest",
       'api:head_has_changed' : true,
       'api:head': Head,
       'api:status' : "api:success"} :< JSON,

    with_triple_store(
        Store_2,
        (   resolve_absolute_string_descriptor("TERMINUSQA2/bar/origin", Bar_Descriptor_Origin),
            once(ask(Bar_Descriptor_Origin,
                     t(a,b,c))),
            repository_head(Bar_Database_Desc, "origin", Head)
        )
    ).

test(fetch_second_time_no_change, [
         setup(
             (   setup_temp_unattached_server(State_1,Store_1,Server_1),
                 setup_temp_unattached_server(State_2,Store_2,Server_2))),
         cleanup(
             (
                 teardown_temp_unattached_server(State_1),
                 teardown_temp_unattached_server(State_2)))
     ])
:-

    with_triple_store(
        Store_1,
        (
            add_user("TERMINUSQA1",'user1@example.com','a comment', some('password1'),_User_ID1),
            create_public_db_without_schema("TERMINUSQA1", "foo"),
            resolve_absolute_string_descriptor("TERMINUSQA1/foo", Foo_Descriptor),
            create_context(Foo_Descriptor, commit_info{author:"test",message:"test"}, Foo_Context),
            with_transaction(Foo_Context,
                             ask(Foo_Context,
                                 insert(a,b,c)),
                             _),
            get_dict(repository_descriptor, Foo_Descriptor, Foo_Repository_Desc),
            get_dict(database_descriptor, Foo_Repository_Desc, Foo_Database_Desc),
            repository_head(Foo_Database_Desc,"local",Head)
        )
    ),

    with_triple_store(
        Store_2,
        (
            add_user("TERMINUSQA2",'user2@example.com','a comment', some('password2'),_User_ID2),
            create_public_db_without_schema("TERMINUSQA2", "bar"),
            resolve_absolute_string_descriptor("TERMINUSQA2/bar", Bar_Descriptor),
            get_dict(repository_descriptor, Bar_Descriptor, Bar_Repository_Desc),
            get_dict(database_descriptor, Bar_Repository_Desc, Bar_Database_Desc),
            create_context(Bar_Database_Desc, commit_info{author:"test",message:"test"}, Bar_Database_Context),
            atomic_list_concat([Server_1, '/TERMINUSQA1/foo'], Remote_URL),
            with_transaction(
                Bar_Database_Context,
                insert_remote_repository(Bar_Database_Context, "origin", Remote_URL, _),
                _)
        )
    ),
    atomic_list_concat([Server_2, '/api/fetch/TERMINUSQA2/bar/origin/_commits'], URL),
    base64("TERMINUSQA1:password1", Base64_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Auth]),
    http_post(URL,
              json(_{}),
              JSON1,
              [json_object(dict),authorization(basic('TERMINUSQA2','password2')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(200)]),


    _{ '@type' : "api:FetchRequest",
       'api:head_has_changed' : true,
       'api:head': Head,
       'api:status' : "api:success"} :< JSON1,

    http_post(URL,
              json(_{}),
              JSON2,
              [json_object(dict),authorization(basic('TERMINUSQA2','password2')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(200)]),

    _{ '@type' : "api:FetchRequest",
       'api:head_has_changed' : false,
       'api:head': Head,
       'api:status' : "api:success"} :< JSON2,


    with_triple_store(
        Store_2,
        (
            repository_head(Bar_Database_Desc, "origin", Head)
        )
    ).

test(fetch_second_time_with_change, [
         setup(
             (   setup_temp_unattached_server(State_1,Store_1,Server_1),
                 setup_temp_unattached_server(State_2,Store_2,Server_2))),
         cleanup(
             (
                 teardown_temp_unattached_server(State_1),
                 teardown_temp_unattached_server(State_2)))
     ])
:-

    with_triple_store(
        Store_1,
        (
            add_user("TERMINUSQA1",'user1@example.com','a comment', some('password1'),_User_ID1),
            create_public_db_without_schema("TERMINUSQA1", "foo"),
            resolve_absolute_string_descriptor("TERMINUSQA1/foo", Foo_Descriptor),
            create_context(Foo_Descriptor, commit_info{author:"test",message:"test"}, Foo_Context),
            with_transaction(Foo_Context,
                             ask(Foo_Context,
                                 insert(a,b,c)),
                             _),
            get_dict(repository_descriptor, Foo_Descriptor, Foo_Repository_Desc),
            get_dict(database_descriptor, Foo_Repository_Desc, Foo_Database_Desc),
            repository_head(Foo_Database_Desc,"local",Head)
        )
    ),

    with_triple_store(
        Store_2,
        (
            add_user("TERMINUSQA2",'user2@example.com','a comment', some('password2'),_User_ID2),
            create_public_db_without_schema("TERMINUSQA2", "bar"),
            resolve_absolute_string_descriptor("TERMINUSQA2/bar", Bar_Descriptor),
            get_dict(repository_descriptor, Bar_Descriptor, Bar_Repository_Desc),
            get_dict(database_descriptor, Bar_Repository_Desc, Bar_Database_Desc),
            create_context(Bar_Database_Desc, commit_info{author:"test",message:"test"}, Bar_Database_Context),
            atomic_list_concat([Server_1, '/TERMINUSQA1/foo'], Remote_URL),
            with_transaction(
                Bar_Database_Context,
                insert_remote_repository(Bar_Database_Context, "origin", Remote_URL, _),
                _)
        )
    ),
    atomic_list_concat([Server_2, '/api/fetch/TERMINUSQA2/bar/origin/_commits'], URL),
    base64("TERMINUSQA1:password1", Base64_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Auth]),
    http_post(URL,
              json(_{}),
              JSON1,
              [json_object(dict),authorization(basic('TERMINUSQA2','password2')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(200)]),


    _{ '@type' : "api:FetchRequest",
       'api:head_has_changed' : true,
       'api:head': Head,
       'api:status' : "api:success"} :< JSON1,

    with_triple_store(
        Store_1,
        (
            create_context(Foo_Descriptor, commit_info{author:"test",message:"test"}, Foo_Context_Extra1),
            with_transaction(Foo_Context_Extra1,
                             ask(Foo_Context_Extra1,
                                 insert(d,e,f)),
                             _),
            create_context(Foo_Descriptor, commit_info{author:"test",message:"test"}, Foo_Context_Extra2),
            with_transaction(Foo_Context_Extra2,
                             ask(Foo_Context_Extra2,
                                 insert(g,h,i)),
                             _),

            repository_head(Foo_Database_Desc,"local",New_Head)
        )),
            

    http_post(URL,
              json(_{}),
              JSON2,
              [json_object(dict),authorization(basic('TERMINUSQA2','password2')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(200)]),

    _{ '@type' : "api:FetchRequest",
       'api:head_has_changed' : true,
       'api:head': New_Head,
       'api:status' : "api:success"} :< JSON2,


    with_triple_store(
        Store_2,
        (
            repository_head(Bar_Database_Desc, "origin", New_Head)
        )
    ).

:- end_tests(fetch_endpoint).


%%%%%%%%%%%%%%%%%%%% Rebase Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(rebase/Path), cors_handler(Method, rebase_handler(Path)),
                [method(Method),
                 prefix,
                 time_limit(infinite),
                 methods([options,post])]).


rebase_handler(post, Path, Request, System_DB, Auth) :-
    do_or_die(
        (   get_payload(Document, Request),
            _{ author : Author,
               rebase_from : Their_Path } :< Document
        ),
        error(bad_api_document(Document,[author,rebase_from]),_)),

    catch_with_backtrace(
        (   Strategy_Map = [],
            rebase_on_branch(System_DB, Auth, Path, Their_Path, Author, Strategy_Map, Common_Commit_ID_Option, Forwarded_Commits, Reports),

            Incomplete_Reply = _{ '@type' : "api:RebaseResponse",
                                  'api:status' : "api:success",
                                  'api:forwarded_commits' : Forwarded_Commits,
                                  'api:rebase_report': Reports
                                },
            (   Common_Commit_ID_Option = some(Common_Commit_ID)
            ->  Reply = (Incomplete_Reply.put('api:common_commit_id', Common_Commit_ID))
            ;   Reply = Incomplete_Reply),
            cors_reply_json(Request, Reply, [status(200)])),
        E,
        do_or_die(rebase_error_handler(E,Request),
                  E)
    ).

rebase_error_handler(error(invalid_target_absolute_path(Path),_), Request) :-
    format(string(Msg), "The following rebase target absolute resource descriptor string is invalid: ~q", [Path]),
    cors_reply_json(Request,
                    _{'@type' : 'api:RebaseErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:BadAbsoluteTargetDescriptor',
                                       'api:absolute_descriptor' : Path},
                      'api:message' : Msg
                     },
                    [status(400)]).
rebase_error_handler(error(invalid_source_absolute_path(Path),_), Request) :-
    format(string(Msg), "The following rebase source absolute resource descriptor string is invalid: ~q", [Path]),
    cors_reply_json(Request,
                    _{'@type' : 'api:RebaseErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:BadAbsoluteSourceDescriptor',
                                       'api:absolute_descriptor' : Path},
                      'api:message' : Msg
                     },
                    [status(400)]).
rebase_error_handler(error(rebase_requires_target_branch(Descriptor),_), Request) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The following rebase target absolute resource descriptor does not describe a branch: ~q", [Path]),
    cors_reply_json(Request,
                    _{'@type' : 'api:RebaseErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:NotATargetBranchDescriptorError',
                                       'api:absolute_descriptor' : Path},
                      'api:message' : Msg
                     },
                    [status(400)]).
rebase_error_handler(error(rebase_requires_source_branch(Descriptor),_), Request) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The following rebase source absolute resource descriptor does not describe a branch: ~q", [Path]),
    cors_reply_json(Request,
                    _{'@type' : 'api:RebaseErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:NotASourceBranchDescriptorError',
                                       'api:absolute_descriptor' : Path},
                      'api:message' : Msg
                     },
                    [status(400)]).
rebase_error_handler(error(unresolvable_target_descriptor(Descriptor),_), Request) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The following target descriptor could not be resolved to a branch: ~q", [Path]),
    cors_reply_json(Request,
                    _{'@type' : 'api:RebaseErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:UnresolvableTargetAbsoluteDescriptor',
                                       'api:absolute_descriptor' : Path},
                      'api:message' : Msg
                     },
                    [status(404)]).
rebase_error_handler(error(unresolvable_source_descriptor(Descriptor),_), Request) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The following source descriptor could not be resolved to a branch: ~q", [Path]),
    cors_reply_json(Request,
                    _{'@type' : 'api:RebaseErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:UnresolvableSourceAbsoluteDescriptor',
                                       'api:absolute_descriptor' : Path},
                      'api:message' : Msg
                     },
                    [status(404)]).
rebase_error_handler(error(rebase_commit_application_failed(continue_on_valid_commit(Their_Commit_Id)),_), Request) :-
    format(string(Msg), "While rebasing, commit ~q applied cleanly, but the 'continue' strategy was specified, indicating this should have errored", [Their_Commit_Id]),
    cors_reply_json(Request,
                    _{'@type' : 'api:RebaseErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:RebaseContinueOnValidCommit',
                                       'api:their_commit' : Their_Commit_Id},
                      'api:message' : Msg
                     },
                    [status(400)]).
rebase_error_handler(error(rebase_commit_application_failed(fixup_on_valid_commit(Their_Commit_Id)),_), Request) :-
    format(string(Msg), "While rebasing, commit ~q applied cleanly, but the 'fixup' strategy was specified, indicating this should have errored", [Their_Commit_Id]),
    cors_reply_json(Request,
                    _{'@type' : 'api:RebaseErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:RebaseFixupOnValidCommit',
                                       'api:their_commit' : Their_Commit_Id},
                      'api:message' : Msg
                     },
                    [status(400)]).
rebase_error_handler(error(rebase_commit_application_failed(schema_validation_error(Their_Commit_Id, Fixup_Witnesses)),_), Request) :-
    format(string(Msg), "Rebase failed on commit ~q due to schema validation errors", [Their_Commit_Id]),
    cors_reply_json(Request,
                    _{'@type' : 'api:RebaseErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:RebaseSchemaValidationError',
                                       'api:their_commit' : Their_Commit_Id,
                                       'api:witness' : Fixup_Witnesses},
                      'api:message' : Msg
                     },
                    [status(400)]).
rebase_error_handler(error(rebase_commit_application_failed(fixup_error(Their_Commit_Id, Fixup_Witnesses)),_), Request) :-
    format(string(Msg), "Rebase failed on commit ~q due to fixup error: ~q", [Their_Commit_Id,Fixup_Witnesses]),
    cors_reply_json(Request,
                    _{'@type' : 'api:RebaseErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:RebaseFixupError',
                                       'api:their_commit' : Their_Commit_Id,
                                       'api:witness' : Fixup_Witnesses},
                      'api:message' : Msg
                     },
                    [status(400)]).

:- begin_tests(rebase_endpoint).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(rebase_divergent_history, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    add_user("TERMINUSQA",'user@example.com','a comment', some('password'),User_ID),
    create_db_without_schema("TERMINUSQA", "foo"),

    Master_Path = "TERMINUSQA/foo",
    resolve_absolute_string_descriptor(Master_Path, Master_Descriptor),
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit a"}, Master_Context1),
    with_transaction(Master_Context1,
                     ask(Master_Context1,
                         insert(a,b,c)),
                    _),

    Second_Path = "TERMINUSQA/foo/local/branch/second",
    branch_create(system_descriptor{}, User_ID, Second_Path, some(Master_Path), _),
    resolve_absolute_string_descriptor(Second_Path, Second_Descriptor),

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

    atomic_list_concat([Server, '/api/rebase/TERMINUSQA/foo'], URI),
    http_post(URI,
              json(_{rebase_from: 'TERMINUSQA/foo/local/branch/second',
                     author : "Gavsky"}),
              JSON,
              [json_object(dict),authorization(basic('TERMINUSQA','password'))]),

    * json_write_dict(current_output, JSON, []),

    _{  '@type' : "api:RebaseResponse",
        'api:forwarded_commits' : [_Thing, _Another_Thing ],
        'api:common_commit_id' : _Common_Something,
        'api:rebase_report' : _Reports,
        'api:status' : "api:success"
    } :< JSON,

    Repository_Descriptor = Master_Descriptor.repository_descriptor,
    branch_head_commit(Repository_Descriptor, "main", Commit_Uri),
    commit_uri_to_history_commit_ids(Repository_Descriptor, Commit_Uri, [Commit_A, Commit_B, Commit_C, Commit_D]),

    commit_id_to_metadata(Repository_Descriptor, Commit_A, "test", "commit a", _),
    commit_id_to_metadata(Repository_Descriptor, Commit_B, "test", "commit b", _),
    commit_id_to_metadata(Repository_Descriptor, Commit_C, "test", "commit c", _),
    commit_id_to_metadata(Repository_Descriptor, Commit_D, "Gavsky", "commit d", _).
:- end_tests(rebase_endpoint).

%%%%%%%%%%%%%%%%%%%% Pack Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(pack/Path), cors_handler(Method, pack_handler(Path)),
                [method(Method),
                 methods([options,post])]).

pack_handler(post,Path,Request, System_DB, Auth) :-
    get_payload(Document,Request),

    (   _{ repository_head : Layer_ID } :< Document
    ->  Repo_Head_Option = some(Layer_ID)
    ;   Repo_Head_Option = none),

    catch_with_backtrace(
        (   pack(System_DB, Auth,
                 Path, Repo_Head_Option, Payload_Option),

            (   Payload_Option = some(Payload)
            ->  throw(http_reply(bytes('application/octets',Payload)))
            ;   throw(http_reply(bytes('application/octets',"No content"),[status(204)])))),
        E,
        do_or_die(pack_error_handler(E,Request),
                  E)).

pack_error_handler(error(invalid_absolute_path(Path),_), Request) :-
    format(string(Msg), "The following absolute resource descriptor string is invalid: ~q", [Path]),
    cors_reply_json(Request,
                    _{'@type' : 'api:PackErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:BadAbsoluteDescriptor',
                                       'api:absolute_descriptor' : Path},
                      'api:message' : Msg
                     },
                    [status(400)]).
pack_error_handler(error(unresolvable_collection(Descriptor),_), Request) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The following descriptor (which should be a repository) could not be resolved to a resource: ~q", [Path]),
    cors_reply_json(Request,
                    _{'@type' : 'api:PackErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:UnresolvableAbsoluteDescriptor',
                                       'api:absolute_descriptor' : Path},
                      'api:message' : Msg
                     },
                    [status(404)]).
pack_error_handler(error(not_a_repository_descriptor(Descriptor),_), Request) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The following descriptor is not a repository descriptor: ~q", [Path]),
    cors_reply_json(Request,
                    _{'@type' : 'api:PackErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:NotARepositoryDescriptorError',
                                       'api:absolute_descriptor' : Path},
                      'api:message' : Msg
                     },
                    [status(404)]).

% Currently just sending binary around...
:- begin_tests(pack_endpoint).
:- use_module(core(util/test_utils)).
%:- use_module(core(transaction)).
%:- use_module(core(api)).
:- use_module(library(http/http_open)).
:- use_module(library(terminus_store)).

test(pack_stuff, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    add_user('_a_test_user_','user@example.com','a comment', some('password'),_User_ID),
    create_db_without_schema('_a_test_user_',foo),

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


    atomic_list_concat([Server, '/api/pack/_a_test_user_/foo'], URI),

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
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    add_user('_a_test_user_','user@example.com','a comment', some('password'),_User_ID),
    create_db_without_schema('_a_test_user_','foo'),

    resolve_absolute_string_descriptor('_a_test_user_/foo', Descriptor),
    Repository_Descriptor = (Descriptor.repository_descriptor),
    open_descriptor(Repository_Descriptor, Repo_Transaction),
    repository_head_layerid(Repo_Transaction, Repository_Head_Layer_ID),

    Document = _{ repository_head : Repository_Head_Layer_ID },
    atomic_list_concat([Server, '/api/pack/_a_test_user_/foo'], URI),
    http_post(URI,
              json(Document),
              _Data,
              [authorization(basic('_a_test_user_','password')),status_code(Status)]),
    Status = 204.

:- end_tests(pack_endpoint).

%%%%%%%%%%%%%%%%%%%% Unpack Handlers %%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(unpack/Path), cors_handler(Method, unpack_handler(Path)),
                [method(Method),
                 methods([options,post])]).

unpack_handler(post, Path, Request, System_DB, Auth) :-
    get_payload(Payload, Request),

    catch_with_backtrace(
        (   unpack(System_DB, Auth, Path, Payload),
            cors_reply_json(Request,
                            _{'@type' : 'api:UnpackResponse',
                              'api:status' : "api:success"},
                            [status(200)])
        ),
        E,
        do_or_die(
            unpack_error_handler(E, Request),
            E)).

unpack_error_handler(error(not_a_linear_history_in_unpack(_History),_), Request) :-
    cors_reply_json(Request,
                    _{'@type' : "api:UnpackErrorResponse",
                      'api:status' : "api:failure",
                      'api:error' : _{'@type' : "api:NotALinearHistory"},
                      'api:message' : "Not a linear history"
                     },
                    [status(400)]).
unpack_error_handler(error(unknown_layer_reference(Layer_Id),_), Request) :-
    cors_reply_json(Request,
                    _{'@type' : "api:UnpackErrorResponse",
                      'api:status' : "api:failure",
                      'api:message' : "A layer in the pack has an unknown parent",
                      'api:error' : _{ '@type' : "api:UnknownLayerReference",
                                       'api:layer_reference' : Layer_Id}},
                    [status(400)]).
unpack_error_handler(error(unresolvable_absolute_descriptor(Descriptor), _), Request) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The database to unpack to has not been found at absolute path ~q", [Path]),
    cors_reply_json(Request,
                    _{'@type' : "api:UnpackErrorResponse",
                      'api:status' : "api:failure",
                      'api:message' : Msg,
                      'api:error' : _{ '@type' : "api:UnresolvableAbsoluteDescriptor",
                                       'api:absolute_descriptor' : Path}
                     },
                    [status(400)]).
unpack_error_handler(error(not_a_repository_descriptor(Descriptor),_), Request) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The following descriptor is not a repository descriptor: ~q", [Path]),
    cors_reply_json(Request,
                    _{'@type' : 'api:UnpackErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:NotARepositoryDescriptorError',
                                       'api:absolute_descriptor' : Path},
                      'api:message' : Msg
                     },
                    [status(404)]).
unpack_error_handler(error(invalid_absolute_path(Path),_), Request) :-
    format(string(Msg), "The following absolute resource descriptor string is invalid: ~q", [Path]),
    cors_reply_json(Request,
                    _{'@type' : 'api:UnpackErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:BadAbsoluteDescriptor',
                                       'api:absolute_descriptor' : Path},
                      'api:message' : Msg
                     },
                    [status(404)]).

:- begin_tests(unpack_endpoint).


:- end_tests(unpack_endpoint).

%%%%%%%%%%%%%%%%%%%% Push Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(push/Path), cors_handler(Method, push_handler(Path)),
                [method(Method),
                 prefix,
                 methods([options,post])]).

push_handler(post,Path,Request, System_DB, Auth) :-
    do_or_die(
        (  get_payload(Document, Request),
           _{ remote : Remote_Name,
              remote_branch : Remote_Branch } :< Document),
        error(bad_api_document(Document,[remote,remote_branch]),_)),

    do_or_die(
        request_remote_authorization(Request, Authorization),
        error(no_remote_authorization)),

    catch_with_backtrace(
        (   push(System_DB, Auth, Path, Remote_Name, Remote_Branch,
                 authorized_push(Authorization),Result),
            (   Result = same(Head_ID)
            ->  Head_Updated = false
            ;   Result = new(Head_ID)
            ->  Head_Updated = true
            ;   throw(error(internal_server_error,_))),

            Response =  _{'@type' : "api:PushResponse",
                          'api:repo_head_updated' : Head_Updated,
                          'api:repo_head' : Head_ID,
                          'api:status' : "api:success"},

            cors_reply_json(Request,
                            Response,
                            [status(200)])),
        E,
        do_or_die(push_error_handler(E,Request),
                  E)).

push_error_handler(error(invalid_absolute_path(Path),_), Request) :-
    format(string(Msg), "The following absolute resource descriptor string is invalid: ~q", [Path]),
    cors_reply_json(Request,
                    _{'@type' : 'api:PushErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:BadAbsoluteDescriptor',
                                       'api:absolute_descriptor' : Path},
                      'api:message' : Msg
                     },
                    [status(400)]).
push_error_handler(error(push_requires_branch(Descriptor),_), Request) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The following absolute resource descriptor string does not specify a branch: ~q", [Path]),
    cors_reply_json(Request,
                    _{'@type' : 'api:PushErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:NotABranchDescriptorError',
                                       'api:absolute_descriptor' : Path},
                      'api:message' : Msg
                     },
                    [status(400)]).
push_error_handler(error(unresolvable_absolute_descriptor(Descriptor), _), Request) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The branch described by the path ~q does not exist", [Path]),
    cors_reply_json(Request,
                    _{'@type' : "api:PushErrorResponse",
                      'api:status' : "api:failure",
                      'api:message' : Msg,
                      'api:error' : _{ '@type' : "api:UnresolvableAbsoluteDescriptor",
                                       'api:absolute_descriptor' : Path}
                     },
                    [status(404)]).
push_error_handler(error(remote_authorization_failure(Reason), _), Request) :-
    (   get_dict('api:message', Reason, Inner_Msg)
    ->  format(string(Msg), "Remote authorization failed for reason:", [Inner_Msg])
    ;   format(string(Msg), "Remote authorization failed with malformed response", [])),
    cors_reply_json(Request,
                    _{'@type' : "api:PushErrorResponse",
                      'api:status' : "api:failure",
                      'api:message' : Msg,
                      'api:error' : _{ '@type' : "api:RemoteAuthorizationFailure",
                                       'api:response' : Reason}
                     },
                    [status(401)]).
push_error_handler(error(remote_unpack_failed(history_diverged),_), Request) :-
    format(string(Msg), "The unpacking of layers on the remote was not possible as the history was divergent", []),
    cors_reply_json(Request,
                    _{'@type' : 'api:PushErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : "api:HistoryDivergedError"},
                      'api:message' : Msg
                     },
                    [status(400)]).
push_error_handler(error(remote_unpack_failed(communication_failure(Reason)),_), Request) :-
    format(string(Msg), "The unpacking of layers failed on the remote due to a communication error: ~q", [Reason]),
    cors_reply_json(Request,
                    _{'@type' : 'api:PushErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : "api:CommunicationFailure"},
                      'api:message' : Msg
                     },
                    [status(400)]).
push_error_handler(error(remote_unpack_failed(authorization_failure(Reason)),_), Request) :-
    format(string(Msg), "The unpacking of layers failed on the remote due to an authorization failure: ~q", [Reason]),
    cors_reply_json(Request,
                    _{'@type' : 'api:PushErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : "api:AuthorizationFailure"},
                      'api:message' : Msg
                     },
                    [status(400)]).
push_error_handler(error(remote_unpack_failed(remote_unknown),_), Request) :-
    format(string(Msg), "The remote requested was not known", []),
    cors_reply_json(Request,
                    _{'@type' : 'api:PushErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : "api:RemoteUnknown"},
                      'api:message' : Msg
                     },
                    [status(400)]).


remote_unpack_url(URL, Pack_URL) :-
    pattern_string_split('/', URL, [Protocol,Blank,Server|Rest]),
    merge_separator_split(Pack_URL,'/',[Protocol,Blank,Server,"api","unpack"|Rest]).

% NOTE: What do we do with the remote branch? How do we send it?
authorized_push(Authorization, Remote_URL, Payload) :-
    (   is_local_https(Remote_URL)
    ->  Additional_Options = [cert_verify_hook(cert_accept_any)]
    ;   Additional_Options = []),

    remote_unpack_url(Remote_URL, Unpack_URL),

    catch(http_post(Unpack_URL,
                    bytes('application/octets',Payload),
                    Result,
                    [request_header('Authorization'=Authorization),
                     json_object(dict),
                     status_code(Status_Code)
                     |Additional_Options]),
          E,
          throw(error(communication_failure(E),_))),

    (   200 = Status_Code
    ->  true
    ;   400 = Status_Code,
        _{'@type': "api:UnpackErrorResponse", 'api:error' : Error} :< Result
    ->  (   _{'@type' : "api:NotALinearHistory"} :< Error
        ->  throw(error(history_diverged,_))
        ;   _{'@type' : "api:UnpackDestinationDatabaseNotFound"} :< Error
        ->  throw(error(remote_unknown,_))
        ;   throw(error(unknown_status_code(Status_Code, Result),_))
        )
    ;   403 = Status_Code
    ->  throw(error(remote_authorization_failure(Result),_))
    ;   throw(error(unknown_status_code,_))
    ).

:- begin_tests(push_endpoint, []).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(push_empty_to_empty_does_nothing_succesfully,
     [
         setup(
             (   setup_temp_unattached_server(State_Origin,Store_Origin,Server_Origin),
                 setup_temp_unattached_server(State_Destination,Store_Destination,Server_Destination),
                 setup_cloned_situation(Store_Origin, Server_Origin, Store_Destination, Server_Destination)
             )),
         cleanup(
             (
                 teardown_temp_unattached_server(State_Origin),
                 teardown_temp_unattached_server(State_Destination)))
     ]) :-
    resolve_absolute_string_descriptor("RosaLuxemburg/bar", Origin_Branch_Descriptor),
    Origin_Database_Descriptor = (Origin_Branch_Descriptor.repository_descriptor.database_descriptor),
    resolve_absolute_string_descriptor("KarlKautsky/foo", Destination_Branch_Descriptor),
    Destination_Database_Descriptor = (Destination_Branch_Descriptor.repository_descriptor.database_descriptor),

    with_triple_store(
        Store_Origin,
        repository_head(Origin_Database_Descriptor, "origin", Head)),
    with_triple_store(
        Store_Destination,
        repository_head(Destination_Database_Descriptor, "local", Head)),

    atomic_list_concat([Server_Origin, '/api/push/RosaLuxemburg/bar'], Push_URL),
    base64("KarlKautsky:password_destination", Base64_Destination_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Destination_Auth]),
    http_post(Push_URL,
              json(_{
                       remote: "origin",
                       remote_branch: "main"
                   }),
              JSON,
              [json_object(dict),authorization(basic('RosaLuxemburg','password_origin')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(Status_Code)]),

    * json_write_dict(current_output, JSON, []),

    Status_Code = 200,
    \+ get_dict('api:head', JSON, _),

    _{'@type':"api:PushResponse",
      'api:repo_head_updated': false,
      'api:repo_head': Head,
      'api:status':"api:success"} :< JSON.



test(push_nonempty_to_empty_advances_remote_head,
     [
         setup(
             (   setup_temp_unattached_server(State_Origin,Store_Origin,Server_Origin),
                 setup_temp_unattached_server(State_Destination,Store_Destination,Server_Destination),
                 setup_cloned_situation(Store_Origin, Server_Origin, Store_Destination, Server_Destination)
             )),
         cleanup(
             (
                 teardown_temp_unattached_server(State_Origin),
                 teardown_temp_unattached_server(State_Destination)))
     ]) :-
    resolve_absolute_string_descriptor("RosaLuxemburg/bar", Origin_Branch_Descriptor),
    Origin_Database_Descriptor = (Origin_Branch_Descriptor.repository_descriptor.database_descriptor),
    resolve_absolute_string_descriptor("KarlKautsky/foo", Destination_Branch_Descriptor),
    Destination_Database_Descriptor = (Destination_Branch_Descriptor.repository_descriptor.database_descriptor),

    with_triple_store(
        Store_Origin,
        repository_head(Origin_Database_Descriptor, "origin", Head)),
    with_triple_store(
        Store_Destination,
        repository_head(Destination_Database_Descriptor, "local", Head)),
    atomic_list_concat([Server_Origin, '/api/push/RosaLuxemburg/bar'], Push_URL),
    base64("KarlKautsky:password_destination", Base64_Destination_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Destination_Auth]),
    http_post(Push_URL,
              json(_{
                       remote: "origin",
                       remote_branch: "main"
                   }),
              JSON,
              [json_object(dict),authorization(basic('RosaLuxemburg','password_origin')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(Status_Code)]),

    * json_write_dict(current_output, JSON, []),

    Status_Code = 200,

    _{'@type':"api:PushResponse", % todo this should actually not be that
      'api:repo_head_updated': false,
      'api:repo_head': Head,
      'api:status':"api:success"} :< JSON.

test(push_nonempty_to_same_nonempty_keeps_remote_head_unchanged,
     [
         setup(
             (   setup_temp_unattached_server(State_Origin,Store_Origin,Server_Origin),
                 setup_temp_unattached_server(State_Destination,Store_Destination,Server_Destination),
                 setup_cloned_nonempty_situation(Store_Origin, Server_Origin, Store_Destination, Server_Destination)
             )),
         cleanup(
             (
                 teardown_temp_unattached_server(State_Origin),
                 teardown_temp_unattached_server(State_Destination)))
     ]) :-
    resolve_absolute_string_descriptor("RosaLuxemburg/bar", Origin_Branch_Descriptor),
    Origin_Database_Descriptor = (Origin_Branch_Descriptor.repository_descriptor.database_descriptor),
    resolve_absolute_string_descriptor("KarlKautsky/foo", Destination_Branch_Descriptor),
    Destination_Database_Descriptor = (Destination_Branch_Descriptor.repository_descriptor.database_descriptor),

    with_triple_store(
        Store_Origin,
        repository_head(Origin_Database_Descriptor, "origin", Head)),
    with_triple_store(
        Store_Destination,
        repository_head(Destination_Database_Descriptor, "local", Head)),

    atomic_list_concat([Server_Origin, '/api/push/RosaLuxemburg/bar'], Push_URL),
    base64("KarlKautsky:password_destination", Base64_Destination_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Destination_Auth]),
    http_post(Push_URL,
              json(_{
                       remote: "origin",
                       remote_branch: "main"
                   }),
              JSON,
              [json_object(dict),authorization(basic('RosaLuxemburg','password_origin')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(Status_Code)]),

    * json_write_dict(current_output, JSON, []),

    Status_Code = 200,

    _{'@type':"api:PushResponse", % todo this should actually not be that
      'api:repo_head_updated': false,
      'api:repo_head': Head,
      'api:status':"api:success"} :< JSON,

    with_triple_store(
        Store_Origin,
        repository_head(Origin_Database_Descriptor, "origin", Head)),
    with_triple_store(
        Store_Destination,
        repository_head(Destination_Database_Descriptor, "local", Head)).

test(push_nonempty_to_earlier_nonempty_advances_remote_head,
     [
         setup(
             (   setup_temp_unattached_server(State_Origin,Store_Origin,Server_Origin),
                 setup_temp_unattached_server(State_Destination,Store_Destination,Server_Destination),
                 setup_cloned_nonempty_situation(Store_Origin, Server_Origin, Store_Destination, Server_Destination)
             )),
         cleanup(
             (
                 teardown_temp_unattached_server(State_Origin),
                 teardown_temp_unattached_server(State_Destination)))
     ]) :-
    resolve_absolute_string_descriptor("RosaLuxemburg/bar", Origin_Branch_Descriptor),
    with_triple_store(
        Store_Origin,
        (   create_context(Origin_Branch_Descriptor, commit_info{author:"Rosa",message:"hello"}, Origin_Context_1),
            with_transaction(Origin_Context_1,
                             ask(Origin_Context_1,
                                 insert(g,h,i)),
                             _),
            create_context(Origin_Branch_Descriptor, commit_info{author:"Rosa",message:"hello again"}, Origin_Context_2),
            with_transaction(Origin_Context_2,
                             ask(Origin_Context_2,
                                 insert(j,k,l)),
                             _)
        )
    ),

    atomic_list_concat([Server_Origin, '/api/push/RosaLuxemburg/bar'], Push_URL),
    base64("KarlKautsky:password_destination", Base64_Destination_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Destination_Auth]),
    http_post(Push_URL,
              json(_{
                       remote: "origin",
                       remote_branch: "main"
                   }),
              JSON,
              [json_object(dict),authorization(basic('RosaLuxemburg','password_origin')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(Status_Code)]),

    * json_write_dict(current_output, JSON, []),

    Status_Code = 200,

    _{'@type':"api:PushResponse",
      'api:repo_head_updated': true,
      'api:repo_head': Head,
      'api:status':"api:success"} :< JSON,

    Origin_Database_Descriptor = (Origin_Branch_Descriptor.repository_descriptor.database_descriptor),
    resolve_absolute_string_descriptor("KarlKautsky/foo", Destination_Branch_Descriptor),
    Destination_Database_Descriptor = (Destination_Branch_Descriptor.repository_descriptor.database_descriptor),

    with_triple_store(
        Store_Origin,
        repository_head(Origin_Database_Descriptor, "origin", Head)),
    with_triple_store(
        Store_Destination,
        repository_head(Destination_Database_Descriptor, "local", Head)).

:- end_tests(push_endpoint).

%%%%%%%%%%%%%%%%%%%% Pull Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(pull/Path), cors_handler(Method, pull_handler(Path)),
                [method(Method),
                 prefix,
                 methods([options,post])]).

pull_handler(post,Path,Request, System_DB, Local_Auth) :-
    % Can't we just ask for the default remote?
    do_or_die(
        (   get_payload(Document, Request),
            _{ remote : Remote_Name,
               remote_branch : Remote_Branch_Name
             } :< Document),
        error(bad_api_document(Document, [remote, remote_branch]),_)),

    do_or_die(
        request_remote_authorization(Request, Remote_Auth),
        error(no_remote_authorization)),

    catch_with_backtrace(
        (   pull(System_DB, Local_Auth, Path, Remote_Name, Remote_Branch_Name,
                 authorized_fetch(Remote_Auth),
                 Result),
            JSON_Base = _{'@type' : 'api:PullResponse',
                          'api:status' : "api:success"},
            put_dict(Result,JSON_Base,JSON_Response),
            cors_reply_json(Request,
                            JSON_Response,
                            [status(200)])),
        E,
        do_or_die(
            pull_error_handler(E,Request),
            E)).

pull_error_handler(error(not_a_valid_local_branch(Descriptor), _), Request) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The local branch described by the path ~q does not exist", [Path]),
    cors_reply_json(Request,
                    _{'@type' : "api:PullErrorResponse",
                      'api:status' : "api:failure",
                      'api:message' : Msg,
                      'api:fetch_status' : false,
                      'api:error' : _{ '@type' : "api:UnresolvableAbsoluteDescriptor",
                                       'api:absolute_descriptor' : Path}
                     },
                    [status(400)]).
pull_error_handler(error(not_a_valid_remote_branch(Descriptor), _), Request) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The remote branch described by the path ~q does not exist", [Path]),
    cors_reply_json(Request,
                    _{'@type' : "api:PullErrorResponse",
                      'api:status' : "api:failure",
                      'api:message' : Msg,
                      'api:fetch_status' : false,
                      'api:error' : _{ '@type' : "api:UnresolvableAbsoluteDescriptor",
                                       'api:absolute_descriptor' : Path}
                     },
                    [status(400)]).
pull_error_handler(error(pull_divergent_history(Common_Commit,Head_Has_Updated), _), Request) :-
    format(string(Msg), "History diverges from commit ~q", [Common_Commit]),
    cors_reply_json(Request,
                    _{'@type' : "api:PullErrorResponse",
                      'api:status' : "api:failure",
                      'api:message' : Msg,
                      'api:fetch_status' : Head_Has_Updated,
                      'api:error' : _{ '@type' : 'api:HistoryDivergedError',
                                       'api:common_commit' : Common_Commit
                                     }},
                    [status(400)]).
pull_error_handler(error(pull_no_common_history(Head_Has_Updated), _), Request) :-
    format(string(Msg), "There is no common history between branches", []),
    cors_reply_json(Request,
                    _{'@type' : "api:PullErrorResponse",
                      'api:status' : "api:failure",
                      'api:message' : Msg,
                      'api:fetch_status' : Head_Has_Updated,
                      'api:error' : _{ '@type' : 'api:NoCommonHistoryError'
                                     }},
                    [status(400)]).

:- begin_tests(pull_endpoint, []).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(pull_from_empty_to_empty,
     [
         setup(
             (   setup_temp_unattached_server(State_Local,Store_Local,Server_Local),
                 setup_temp_unattached_server(State_Remote,Store_Remote,Server_Remote),
                 setup_cloned_situation(Store_Local, Server_Local, Store_Remote, Server_Remote)
             )),
         cleanup(
             (
                 teardown_temp_unattached_server(State_Local),
                 teardown_temp_unattached_server(State_Remote)))
     ]) :-
    resolve_absolute_string_descriptor("RosaLuxemburg/bar", Local_Branch_Descriptor),
    Local_Database_Descriptor = (Local_Branch_Descriptor.repository_descriptor.database_descriptor),
    resolve_absolute_string_descriptor("KarlKautsky/foo", Remote_Branch_Descriptor),
    Remote_Database_Descriptor = (Remote_Branch_Descriptor.repository_descriptor.database_descriptor),

    with_triple_store(
        Store_Local,
        repository_head(Local_Database_Descriptor, "origin", Head)),
    with_triple_store(
        Store_Remote,
        repository_head(Remote_Database_Descriptor, "local", Head)),

    atomic_list_concat([Server_Local, '/api/pull/RosaLuxemburg/bar'], Pull_URL),
    base64("KarlKautsky:password_destination", Base64_Remote_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Remote_Auth]),
    http_post(Pull_URL,
              json(_{
                       remote: "origin",
                       remote_branch: "main"
                   }),
              JSON,
              [json_object(dict),authorization(basic('RosaLuxemburg','password_origin')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(Status_Code)]),

    * json_write_dict(current_output, JSON, []),

    Status_Code = 200,
    _{'@type' : "api:PullResponse",
      'api:pull_status' : "api:pull_unchanged",
      'api:fetch_status' : false,
      'api:status':"api:success"} :< JSON.

test(pull_from_something_to_empty,
     [
         setup(
             (   setup_temp_unattached_server(State_Local,Store_Local,Server_Local),
                 setup_temp_unattached_server(State_Remote,Store_Remote,Server_Remote),
                 setup_cloned_situation(Store_Local, Server_Local, Store_Remote, Server_Remote)
             )),
         cleanup(
             (
                 teardown_temp_unattached_server(State_Local),
                 teardown_temp_unattached_server(State_Remote)))
     ]) :-
    resolve_absolute_string_descriptor("RosaLuxemburg/bar", Local_Branch_Descriptor),
    Local_Repository_Descriptor = (Local_Branch_Descriptor.repository_descriptor),
    Local_Database_Descriptor = (Local_Repository_Descriptor.database_descriptor),
    resolve_absolute_string_descriptor("KarlKautsky/foo", Remote_Branch_Descriptor),
    Remote_Repository_Descriptor = (Remote_Branch_Descriptor.repository_descriptor),
    Remote_Database_Descriptor = (Remote_Repository_Descriptor.database_descriptor),

    with_triple_store(
        Store_Remote,
        (   create_context(Remote_Branch_Descriptor, commit_info{author:"KarlKautsky", message:"Boo!"}, Remote_Context_1),
            with_transaction(
                Remote_Context_1,
                ask(Remote_Context_1,
                    insert(a,b,c)),
                _),
            repository_head(Remote_Database_Descriptor, "local", Head),
            branch_head_commit(Remote_Repository_Descriptor, "main", Remote_Commit_Uri),
            commit_id_uri(Remote_Repository_Descriptor, Commit_Id, Remote_Commit_Uri)
        )
    ),

    atomic_list_concat([Server_Local, '/api/pull/RosaLuxemburg/bar'], Pull_URL),
    base64("KarlKautsky:password_destination", Base64_Remote_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Remote_Auth]),
    http_post(Pull_URL,
              json(_{
                       remote: "origin",
                       remote_branch: "main"
                   }),
              JSON,
              [json_object(dict),authorization(basic('RosaLuxemburg','password_origin')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(Status_Code)]),

    * json_write_dict(current_output, JSON, []),

    Status_Code = 200,
    _{'@type' : "api:PullResponse",
      'api:pull_status' : "api:pull_fast_forwarded",
      'api:fetch_status' : true,
      'api:status':"api:success"} :< JSON,

    with_triple_store(
        Store_Local,
        (
            repository_head(Local_Database_Descriptor, "origin", Head),
            branch_head_commit(Local_Repository_Descriptor, "main", Local_Commit_Uri),
            commit_id_uri(Local_Repository_Descriptor, Commit_Id, Local_Commit_Uri)
        )
    ).

test(pull_from_something_to_something,
     [
         setup(
             (   setup_temp_unattached_server(State_Local,Store_Local,Server_Local),
                 setup_temp_unattached_server(State_Remote,Store_Remote,Server_Remote),
                 setup_cloned_nonempty_situation(Store_Local, Server_Local, Store_Remote, Server_Remote)
             )),
         cleanup(
             (
                 teardown_temp_unattached_server(State_Local),
                 teardown_temp_unattached_server(State_Remote)))
     ]) :-
    resolve_absolute_string_descriptor("RosaLuxemburg/bar", Local_Branch_Descriptor),
    Local_Repository_Descriptor = (Local_Branch_Descriptor.repository_descriptor),
    Local_Database_Descriptor = (Local_Repository_Descriptor.database_descriptor),
    resolve_absolute_string_descriptor("KarlKautsky/foo", Remote_Branch_Descriptor),
    Remote_Repository_Descriptor = (Remote_Branch_Descriptor.repository_descriptor),
    Remote_Database_Descriptor = (Remote_Repository_Descriptor.database_descriptor),

    with_triple_store(
        Store_Local,
        branch_head_commit(Local_Repository_Descriptor, "main", Local_Orginal_Commit_Uri)
    ),

    with_triple_store(
        Store_Remote,
        (   create_context(Remote_Branch_Descriptor, commit_info{author:"KarlKautsky", message:"Boo!"}, Remote_Context_1),
            with_transaction(
                Remote_Context_1,
                ask(Remote_Context_1,
                    insert(e,f,g)),
                _),
            repository_head(Remote_Database_Descriptor, "local", Head),
            branch_head_commit(Remote_Repository_Descriptor, "main", Remote_Commit_Uri),
            commit_id_uri(Remote_Repository_Descriptor, Commit_Id, Remote_Commit_Uri)
        )
    ),

    atomic_list_concat([Server_Local, '/api/pull/RosaLuxemburg/bar'], Pull_URL),
    base64("KarlKautsky:password_destination", Base64_Remote_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Remote_Auth]),
    http_post(Pull_URL,
              json(_{
                       remote: "origin",
                       remote_branch: "main"
                   }),
              JSON,
              [json_object(dict),authorization(basic('RosaLuxemburg','password_origin')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(Status_Code)]),

    * json_write_dict(current_output, JSON, []),

    Status_Code = 200,
    _{'@type' : "api:PullResponse",
      'api:pull_status' : "api:pull_fast_forwarded",
      'api:fetch_status' : true,
      'api:status':"api:success"} :< JSON,

    with_triple_store(
        Store_Local,
        (
            repository_head(Local_Database_Descriptor, "origin", Head),
            branch_head_commit(Local_Repository_Descriptor, "main", Local_Commit_Uri),
            \+ Local_Orginal_Commit_Uri = Local_Commit_Uri,
            commit_id_uri(Local_Repository_Descriptor, Commit_Id, Local_Commit_Uri)
        )
    ).

test(pull_from_something_to_something_equal,
     [
         setup(
             (   setup_temp_unattached_server(State_Local,Store_Local,Server_Local),
                 setup_temp_unattached_server(State_Remote,Store_Remote,Server_Remote),
                 setup_cloned_nonempty_situation(Store_Local, Server_Local, Store_Remote, Server_Remote)
             )),
         cleanup(
             (
                 teardown_temp_unattached_server(State_Local),
                 teardown_temp_unattached_server(State_Remote)))
     ]) :-
    resolve_absolute_string_descriptor("RosaLuxemburg/bar", Local_Branch_Descriptor),
    Local_Repository_Descriptor = (Local_Branch_Descriptor.repository_descriptor),
    Local_Database_Descriptor = (Local_Repository_Descriptor.database_descriptor),

    with_triple_store(
        Store_Local,
        (
            repository_head(Local_Database_Descriptor, "origin", Head),
            branch_head_commit(Local_Repository_Descriptor, "main", Local_Commit_Uri)
        )
    ),

    atomic_list_concat([Server_Local, '/api/pull/RosaLuxemburg/bar'], Pull_URL),
    base64("KarlKautsky:password_destination", Base64_Remote_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Remote_Auth]),
    http_post(Pull_URL,
              json(_{
                       remote: "origin",
                       remote_branch: "main"
                   }),
              JSON,
              [json_object(dict),authorization(basic('RosaLuxemburg','password_origin')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(Status_Code)]),

    * json_write_dict(current_output, JSON, []),

    Status_Code = 200,
    _{'@type' : "api:PullResponse",
      'api:pull_status' : "api:pull_unchanged",
      'api:fetch_status' : false,
      'api:status':"api:success"} :< JSON,

    with_triple_store(
        Store_Local,
        (
            repository_head(Local_Database_Descriptor, "origin", Head),
            branch_head_commit(Local_Repository_Descriptor, "main", Local_Commit_Uri)
        )
    ).

test(pull_from_something_to_something_equal_other_branch,
     [
         setup(
             (   setup_temp_unattached_server(State_Local,Store_Local,Server_Local),
                 setup_temp_unattached_server(State_Remote,Store_Remote,Server_Remote),
                 setup_cloned_nonempty_situation(Store_Local, Server_Local, Store_Remote, Server_Remote)
             )),
         cleanup(
             (
                 teardown_temp_unattached_server(State_Local),
                 teardown_temp_unattached_server(State_Remote)))
     ]) :-
    resolve_absolute_string_descriptor("RosaLuxemburg/bar", Local_Branch_Descriptor),
    Local_Repository_Descriptor = (Local_Branch_Descriptor.repository_descriptor),
    Local_Database_Descriptor = (Local_Repository_Descriptor.database_descriptor),
    resolve_absolute_string_descriptor("KarlKautsky/foo", Remote_Branch_Descriptor),
    Remote_Repository_Descriptor = (Remote_Branch_Descriptor.repository_descriptor),
    Remote_Database_Descriptor = (Remote_Repository_Descriptor.database_descriptor),

    with_triple_store(
        Store_Remote,
        (
            agent_name_uri(system_descriptor{}, "KarlKautsky", User_Uri),
            branch_create(system_descriptor{}, User_Uri, "KarlKautsky/foo/local/branch/other", some("KarlKautsky/foo/local/branch/main"), _),
            repository_head(Remote_Database_Descriptor, "local", Head)
        )
    ),

    with_triple_store(
        Store_Local,
        (
            branch_head_commit(Local_Repository_Descriptor, "main", Local_Commit_Uri)
        )
    ),

    atomic_list_concat([Server_Local, '/api/pull/RosaLuxemburg/bar'], Pull_URL),
    base64("KarlKautsky:password_destination", Base64_Remote_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Remote_Auth]),
    http_post(Pull_URL,
              json(_{
                       remote: "origin",
                       remote_branch: "main"
                   }),
              JSON,
              [json_object(dict),authorization(basic('RosaLuxemburg','password_origin')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(Status_Code)]),

    * json_write_dict(current_output, JSON, []),

    Status_Code = 200,
    _{'@type' : "api:PullResponse",
      'api:pull_status' : "api:pull_unchanged",
      'api:fetch_status' : true,
      'api:status':"api:success"} :< JSON,

    with_triple_store(
        Store_Local,
        (
            repository_head(Local_Database_Descriptor, "origin", Head),
            branch_head_commit(Local_Repository_Descriptor, "main", Local_Commit_Uri)
        )
    ).


:- end_tests(pull_endpoint).

%%%%%%%%%%%%%%%%%%%% Branch Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(branch/Path), cors_handler(Method, branch_handler(Path)),
                [method(Method),
                 prefix,
                 methods([options,post])]).

branch_handler(post, Path, Request, System_DB, Auth) :-
    do_or_die(
        get_payload(Document, Request),
        error(bad_api_document(Document, []),_)),

    (   get_dict(origin, Document, Origin_Path)
    ->  Origin_Option = some(Origin_Path)
    ;   Origin_Option = none),

    % DUBIOUS are we even doing authentication here?
    catch_with_backtrace(
        (   branch_create(System_DB, Auth, Path, Origin_Option, _Branch_Uri),
            cors_reply_json(Request,
                            _{'@type' : 'api:BranchResponse',
                              'api:status' : "api:success"})),
        E,
        do_or_die(branch_error_handler(E, Request),
                  E)).

branch_error_handler(error(invalid_target_absolute_path(Path),_), Request) :-
    format(string(Msg), "The following branch target absolute resource descriptor string is invalid: ~q", [Path]),
    cors_reply_json(Request,
                    _{'@type' : 'api:BranchErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:BadAbsoluteTargetDescriptor',
                                       'api:absolute_descriptor' : Path},
                      'api:message' : Msg
                     },
                    [status(404)]).
branch_error_handler(error(invalid_source_absolute_path(Path),_), Request) :-
    format(string(Msg), "The following branch source absolute resource descriptor string is invalid: ~q", [Path]),
    cors_reply_json(Request,
                    _{'@type' : 'api:BranchErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:BadAbsoluteSourceDescriptor',
                                       'api:absolute_descriptor' : Path},
                      'api:message' : Msg
                     },
                    [status(404)]).
branch_error_handler(error(target_not_a_branch_descriptor(Descriptor), _), Request) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The target ~q is not a branch descriptor", [Path]),
    cors_reply_json(Request,
                    _{'@type' : "api:BranchErrorResponse",
                      'api:status' : "api:failure",
                      'api:message' : Msg,
                      'api:error' : _{ '@type' : "api:NotATargetBranchDescriptorError",
                                       'api:absolute_descriptor' : Path}
                     },
                    [status(400)]).
branch_error_handler(error(source_not_a_valid_descriptor(Descriptor), _), Request) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The source path ~q is not a valid descriptor for branching", [Path]),
    cors_reply_json(Request,
                    _{'@type' : "api:BranchErrorResponse",
                      'api:status' : "api:failure",
                      'api:message' : Msg,
                      'api:error' : _{ '@type' : "api:NotASourceBranchDescriptorError",
                                       'api:absolute_descriptor' : Path}
                     },
                    [status(400)]).
branch_error_handler(error(source_database_does_not_exist(Org,DB), _), Request) :-
    format(string(Msg), "The source database '~s/~s' does not exist", [Org, DB]),
    cors_reply_json(Request,
                    _{'@type' : "api:BranchErrorResponse",
                      'api:status' : "api:failure",
                      'api:message' : Msg,
                      'api:error' : _{ '@type' : "api:DatabaseDoesNotExist",
                                       'api:database_name' : DB,
                                       'api:organization_name' : Org}
                     },
                    [status(400)]).
branch_error_handler(error(repository_is_not_local(Descriptor), _), Request) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "Attempt to branch from remote repository", [Path]),
    cors_reply_json(Request,
                    _{'@type' : "api:BranchErrorResponse",
                      'api:status' : "api:failure",
                      'api:message' : Msg,
                      'api:error' : _{ '@type' : "api:NotLocalRepositoryError",
                                       'api:absolute_descriptor' : Path}
                     },
                    [status(400)]).
branch_error_handler(error(branch_already_exists(Branch_Name), _), Request) :-
    format(string(Msg), "Branch ~s already exists", [Branch_Name]),
    cors_reply_json(Request,
                    _{'@type' : "api:BranchErrorResponse",
                      'api:status' : "api:failure",
                      'api:message' : Msg,
                      'api:error' : _{ '@type' : "api:BranchExistsError",
                                       'api:branch_name' : Branch_Name}
                     },
                    [status(400)]).
branch_error_handler(error(origin_cannot_be_branched(Origin_Descriptor), _), Request) :-
    resolve_absolute_string_descriptor(Path, Origin_Descriptor),
    format(string(Msg), "Origin is not a branchable path ~q", [Path]),
    cors_reply_json(Request,
                    _{'@type' : "api:BranchErrorResponse",
                      'api:status' : "api:failure",
                      'api:message' : Msg,
                      'api:error' : _{ '@type' : "api:NotBranchableError",
                                       'api:absolute_descriptor' : Path}
                     },
                    [status(400)]).


:- begin_tests(branch_endpoint).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(create_empty_branch, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    create_db_without_schema("admin", "test"),
    atomic_list_concat([Server, '/api/branch/admin/test/local/branch/foo'], URI),
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
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    create_db_without_schema("admin", "test"),
    atomic_list_concat([Server, '/api/branch/admin/test/local/branch/foo'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{origin:'/admin/test/local/branch/main'}),
              JSON,
              [json_object(dict),authorization(basic(admin,Key))]),
    * json_write_dict(current_output, JSON, []),

    resolve_absolute_string_descriptor("admin/test/local/_commits", Repository_Descriptor),

    has_branch(Repository_Descriptor, "foo").

test(create_branch_from_local_with_prefixes, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    create_db_without_schema("admin", "test"),
    atomic_list_concat([Server, '/api/branch/admin/test/local/branch/foo'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{origin:'/admin/test/local/branch/main',
                     prefixes : _{ doc : "https://terminushub.com/document",
                                   scm : "https://terminushub.com/schema"}
                    }),
              JSON,
              [json_object(dict),authorization(basic(admin,Key))]),
    * json_write_dict(current_output, JSON, []),

    resolve_absolute_string_descriptor("admin/test/local/_commits", Repository_Descriptor),

    has_branch(Repository_Descriptor, "foo").

test(create_branch_that_already_exists_error, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    create_db_without_schema("admin", "test"),
    atomic_list_concat([Server, '/api/branch/admin/test/local/branch/main'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{origin:'/admin/test/local/branch/main',
                     base_uri:'http://terminushub.com/admin/test/foodocument'}),
              JSON,
              [json_object(dict),
               authorization(basic(admin,Key)),
               status_code(Status_Code)]),
    Status_Code = 400,
    * json_write_dict(current_output, JSON, []).

test(create_branch_from_nonexisting_origin_error, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    create_db_without_schema("admin", "test"),
    atomic_list_concat([Server, '/api/branch/admin/test/local/branch/foo'], URI),
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
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    create_db_without_schema("admin", "test"),
    atomic_list_concat([Server, '/api/branch/admin/test/local/branch/foo'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{origin:'admin/test/local/_commits',
                     prefixes : _{ doc : "https://terminushub.com/document",
                                   scm : "https://terminushub.com/schema"}}),
              JSON,
              [json_object(dict),
               authorization(basic(admin,Key)),
               status_code(Status_Code)]),

    * json_write_dict(current_output, JSON, []),
    Status_Code = 400,

    resolve_absolute_string_descriptor("admin/test/local/_commits", Repository_Descriptor),

    \+ has_branch(Repository_Descriptor, "foo").

:- end_tests(branch_endpoint).

%%%%%%%%%%%%%%%%%%%% Prefix Handlers %%%%%%%%%%%%%%%%%%%%%%%%%

:- http_handler(api(prefixes/Path), cors_handler(Method, prefix_handler(Path)),
                [method(Method),
                 prefix,
                 methods([options,post])]).

prefix_handler(post, _Path, _Request, _System_DB, _Auth) :-
    throw(error(not_implemented)).

%%%%%%%%%%%%%%%%%%%% Create/Delete Graph Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(graph/Path), cors_handler(Method, graph_handler(Path)),
                [method(Method),
                 prefix,
                 methods([options,post,delete])]).

graph_handler(post, Path, Request, System_Db, Auth) :-
    get_payload(Document, Request),

    do_or_die(_{ commit_info : Commit_Info } :< Document,
              error(bad_api_document(Document, [commit_info]), _)),

    catch_with_backtrace(
        (create_graph(System_Db, Auth,
                      Path,
                      Commit_Info,
                      _Transaction_Metadata2),
         cors_reply_json(Request,
                         _{'@type' : "api:CreateGraphResponse",
                           'api:status' : "api:success"},
                         [status(200)])),
        E,
        do_or_die(graph_error_handler(E, 'api:CreateGraphErrorResponse', Request),
                  E)).
graph_handler(delete, Path, Request, System_DB, Auth) :-
    do_or_die((   get_payload(Document, Request),
                  _{ commit_info : Commit_Info } :< Document),
              error(bad_api_document(Document, [commit_info]), _)),

    catch_with_backtrace(
        (delete_graph(System_DB,Auth,
                      Path,
                      Commit_Info,
                      _Transaction_Metadata2),
         cors_reply_json(Request,
                         _{'@type' : "api:DeleteGraphResponse",
                           'api:status' : "api:success"},
                         [status(200)])),
        E,
        do_or_die(graph_error_handler(E, 'api:DeleteGraphErrorResponse', Request),
                  E)).

graph_error_handler(error(invalid_absolute_graph_descriptor(Path),_), Type, Request) :-
    format(string(Msg), "The following absolute graph descriptor string is invalid: ~q", [Path]),
    cors_reply_json(Request,
                    _{'@type' : Type,
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:BadAbsoluteGraphDescriptor',
                                       'api:absolute_graph_descriptor' : Path},
                      'api:message' : Msg
                     },
                    [status(404)]).
graph_error_handler(error(bad_graph_type(Graph_Type),_), Type, Request) :-
    format(string(Msg), "Not a valid graph type: ~q", [Graph_Type]),
    cors_reply_json(Request,
                    _{'@type' : Type,
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:BadGraphType',
                                       'api:graph_type' : Graph_Type},
                      'api:message' : Msg
                     },
                    [status(404)]).
graph_error_handler(error(not_a_branch_descriptor(Descriptor),_), Type, Request) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The path ~s is not a branch descriptor", [Path]),
    cors_reply_json(Request,
                    _{'@type' : Type,
                      'api:status' : "api:failure",
                      'api:message' : Msg,
                      'api:error' : _{ '@type' : "api:NotABranchDescriptorError",
                                       'api:absolute_descriptor' : Path}
                     },
                    [status(400)]).
graph_error_handler(error(unresolvable_absolute_descriptor(Descriptor), _), Type, Request) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The path ~q can not be resolve to a resource", [Path]),
    cors_reply_json(Request,
                    _{'@type' : Type,
                      'api:status' : "api:failure",
                      'api:message' : Msg,
                      'api:error' : _{ '@type' : "api:UnresolvableAbsoluteDescriptor",
                                       'api:absolute_descriptor' : Path}
                     },
                    [status(400)]).
graph_error_handler(error(branch_does_not_exist(Descriptor), _), Type, Request) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The branch does not exist for ~q", [Path]),
    cors_reply_json(Request,
                    _{'@type' : Type,
                      'api:status' : "api:failure",
                      'api:message' : Msg,
                      'api:error' : _{ '@type' : "api:UnresolvableAbsoluteDescriptor",
                                       'api:absolute_descriptor' : Path}
                     },
                    [status(400)]).
graph_error_handler(error(graph_already_exists(Descriptor,Graph_Name), _), Type, Request) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    format(string(Msg), "The branch ~q already has a graph named ~q", [Path, Graph_Name]),
    cors_reply_json(Request,
                    _{'@type' : Type,
                      'api:status' : "api:failure",
                      'api:message' : Msg,
                      'api:error' : _{ '@type' : "api:BranchAlreadyExists",
                                       'api:graph_name' : Graph_Name,
                                       'api:absolute_descriptor' : Path}
                     },
                    [status(400)]).

:- begin_tests(graph_endpoint).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(create_graph, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    create_db_without_schema("admin", "test"),
    Commit = commit_info{ author : 'The Graphinator',
                          message : 'Edges here there and everywhere' },

    atomic_list_concat([Server, '/api/graph/admin/test/local/branch/main/instance/naim'], URI),
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
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    create_db_without_schema("admin", "test"),
    super_user_authority(Auth),
    create_graph(system_descriptor{},
                 Auth,
                 "admin/test/local/branch/main/schema/main",
                 commit_info{ author : "test",
                              message: "Generated by automated testing"},
                 _Transaction_Metadata),

    atomic_list_concat([Server, '/api/graph/admin/test/local/branch/main/schema/main'], URI),
    admin_pass(Key),
    Commit = commit_info{ author : 'Jeebuz', message : 'Hello my children' },
    http_get(URI,
             _JSON,
             [method(delete),
              post(json(_{commit_info : Commit})),
              json_object(dict),
              authorization(basic(admin,Key))]).

:- end_tests(graph_endpoint).

%%%%%%%%%%%%%%%%%%%% User handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(user), cors_handler(Method, user_handler),
                [method(Method),
                 prefix,
                 methods([options,post,delete])]).
:- http_handler(api(user/Name), cors_handler(Method, user_handler(Name)),
                [method(Method),
                 prefix,
                 methods([options,post,delete])]).

user_handler(post, Name, Request, System_DB, Auth) :-
    get_payload(Document, Request),

    catch_with_backtrace(
        (   update_user_transaction(System_DB, Auth, Name, Document),
            cors_reply_json(Request,
                            _{'@type' : "api:UpdateUserResponse",
                              'api:status' : "api:success"})),
        E,
        do_or_die(user_update_error_handler(E,Request),
                  E)).
user_handler(delete, Name, Request, System_DB, Auth) :-
    catch_with_backtrace(
        (   delete_user_transaction(System_DB, Auth, Name),
            cors_reply_json(Request,
                            _{'@type' : "api:DeleteUserResponse",
                              'api:status' : "api:success"})),
        E,
        do_or_die(user_delete_error_handler(E,Request),
                  E)).

user_handler(post, Request, System_DB, Auth) :-
    get_payload(Document, Request),

    do_or_die(_{ agent_name : Agent_Name } :< Document,
              error(malformed_update_user_document(Document))
             ),

    catch_with_backtrace(
        (   update_user_transaction(System_DB, Auth, Agent_Name, Document),
            cors_reply_json(Request,
                            _{'@type' : "api:UpdateUserResponse",
                              'api:status' : "api:success"})),
        E,
        do_or_die(update_user_error_handler(E,Request),
                  E)).
user_handler(delete, Request, System_DB, Auth) :-
    get_payload(Document, Request),

    do_or_die(_{ agent_name : Agent_Name },
              error(malformed_user_deletion_document(Document))
             ),

    catch_with_backtrace(
        (   delete_user_transaction(System_DB, Auth, Agent_Name),
            cors_reply_json(Request,
                            _{'@type' : "api:DeleteUserResponse",
                              'api:status' : "api:success"})),
        E,
        do_or_die(user_delete_error_handler(E,Request),
                  E)).

user_update_error_handler(error(user_update_failed_without_error(Name,Document),_),Request) :-
    atom_json_dict(Atom, Document,[]),
    format(string(Msg), "Update to user ~q failed without an error while updating with document ~q", [Name, Atom]),
    cors_reply_json(Request,
                    _{'@type' : "api:UserUpdateErrorResponse",
                      'api:status' : "api:failure",
                      'api:message' : Msg,
                      'api:error' : _{ '@type' : "api:UserUpdateFailedWithoutError",
                                       'api:user_name' : Name}
                     },
                    [status(500)]).
user_update_error_handler(error(malformed_add_user_document(Document),_),Request) :-
    atom_json_dict(Atom, Document,[]),
    format(string(Msg), "An update to a user which does not already exist was attempted with the incomplete document ~q", [Atom]),
    cors_reply_json(Request,
                    _{'@type' : "api:UserUpdateErrorResponse",
                      'api:status' : "api:failure",
                      'api:message' : Msg,
                      'api:error' : _{ '@type' : "api:MalformedAddUserDocument"}
                     },
                    [status(400)]).

user_delete_error_handler(error(user_delete_failed_without_error(Name),_),Request) :-
    format(string(Msg), "Delete of user ~q failed without an error", [Name]),
    cors_reply_json(Request,
                    _{'@type' : "api:UserDeleteErrorResponse",
                      'api:status' : "api:failure",
                      'api:message' : Msg,
                      'api:error' : _{ '@type' : "api:UserDeleteFailedWithoutError",
                                       'api:user_name' : Name}
                     },
                    [status(500)]).



%%%%%%%%%%%%%%%%%%%% Organization handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(organization), cors_handler(Method, organization_handler),
                [method(Method),
                 prefix,
                 methods([options,post,delete])]).
:- http_handler(api(organization/Name), cors_handler(Method, organization_handler(Name)),
                [method(Method),
                 prefix,
                 methods([options,post,delete])]).

organization_handler(post, Request, System_DB, Auth) :-
    get_payload(Document, Request),

    do_or_die(_{ organization_name : Org,
                 user_name : User } :< Document,
              error(bad_api_document(Document, [organization_name, user_name]))
             ),

    catch_with_backtrace(
        (   add_user_organization_transaction(System_DB, Auth, User, Org),
            cors_reply_json(Request,
                            _{'@type' : "api:AddOrganizationResponse",
                              'api:status' : "api:success"})),
        E,
        do_or_die(add_organization_error_handler(E, Request),
                  E)).
organization_handler(delete, Request, System_DB, Auth) :-
    get_payload(Document, Request),

    do_or_die(_{ organization_name : Name },
              error(malformed_organization_deletion_document(Document))
             ),

    catch_with_backtrace(
        (   delete_organization_transaction(System_DB, Auth, Name),
            cors_reply_json(Request,
                            _{'@type' : "api:DeleteOrganizationResponse",
                              'api:status' : "api:success"})),
        E,
        do_or_die(delete_organization_error_handler(E, Request),
                  E)).

organization_handler(post, Name, Request, System_DB, Auth) :-
    get_payload(Document, Request),

    do_or_die(_{ organization_name : New_Name } :< Document,
              error(bad_api_document(Document,[organization_name]), _)),

    catch_with_backtrace(
        (   update_organization_transaction(System_DB, Auth, Name, New_Name),
            cors_reply_json(Request,
                            _{'@type' : "api:DeleteOrganizationResponse",
                              'api:status' : "api:success"})),
        E,
        do_or_die(update_organization_error_handler(E, Request),
                  E)).
organization_handler(delete, Name, Request, System_DB, Auth) :-
    catch_with_backtrace(
        (   delete_organization_transaction(System_DB, Auth, Name),
            cors_reply_json(Request,
                            _{'@type' : "api:DeleteOrganizationResponse",
                              'api:status' : "api:success"})),
        E,
        do_or_die(delete_organization_error_handler(E, Request),
                  E)).

add_organization_error_handler(error(organization_already_exists(Name),_), Request) :-
    format(string(Msg), "The organization ~q already exists", [Name]),
    cors_reply_json(Request,
                    _{'@type' : "api:AddOrganizationErrorResponse",
                      'api:status' : "api:failure",
                      'api:message' : Msg,
                      'api:error' : _{ '@type' : "api:OrganizationAlreadyExists",
                                       'api:organization_name' : Name}
                     },
                    [status(400)]).
add_organization_error_handler(error(organization_creation_requires_superuser,_), Request) :-
    format(string(Msg), "Organization creation requires super user authority", []),
    cors_reply_json(Request,
                    _{'@type' : "api:AddOrganizationErrorResponse",
                      'api:status' : "api:failure",
                      'api:message' : Msg,
                      'api:error' : _{ '@type' : "api:RequiresSuperuserAuthority"}
                     },
                    [status(401)]).

update_organization_error_handler(error(organization_update_requires_superuser,_), Request) :-
    format(string(Msg), "Organization update requires super user authority", []),
    cors_reply_json(Request,
                    _{'@type' : "api:UpdateOrganizationErrorResponse",
                      'api:status' : "api:failure",
                      'api:message' : Msg,
                      'api:error' : _{ '@type' : "api:RequiresSuperuserAuthority"}
                     },
                    [status(401)]).

delete_organization_error_handler(error(delete_organization_requires_superuser,_), Request) :-
    format(string(Msg), "Organization deletion requires super user authority", []),
    cors_reply_json(Request,
                    _{'@type' : "api:DeleteOrganizationErrorResponse",
                      'api:status' : "api:failure",
                      'api:message' : Msg,
                      'api:error' : _{ '@type' : "api:RequiresSuperuserAuthority"}
                     },
                    [status(401)]).

%%%%%%%%%%%%%%%%%%%% Role handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(update_role), cors_handler(Method, update_role_handler),
                [method(Method),
                 prefix,
                 methods([options,post])]).

update_role_handler(post, Request, System_DB, Auth) :-
    get_payload(Document, Request),

    do_or_die(_{ agent_names : Agents,
                 organization_name : Organization,
                 actions : Actions
               } :< Document,
              error(bad_api_document(Document, [agent_names, organization_name, actions]), _)),

    (   _{ database_name : Database_Name } :< Document
    ->  Database_Name_Option = some(Database_Name)
    ;   Database_Name_Option = none),

    catch_with_backtrace(
        (   update_role_transaction(System_DB, Auth, Agents, Organization, Database_Name_Option, Actions),
            cors_reply_json(Request,
                            _{'@type' : "api:UpdateRoleResponse",
                              'api:status' : "api:success"})),
        E,
        do_or_die(update_role_error_handler(E, Request),
                  E)).

update_role_error_handler(error(no_manage_capability(Organization,Resource_Name), _), Request) :-
    format(string(Msg), "The organization ~q has no manage capability over the resource ~q", [Organization, Resource_Name]),
    cors_reply_json(Request,
                    _{'@type' : "api:UpdateRoleErrorResponse",
                      'api:status' : "api:failure",
                      'api:message' : Msg,
                      'api:error' : _{ '@type' : "api:NoManageCapability",
                                       'api:organization_name' : Organization,
                                       'api:resource_name' : Resource_Name}
                     },
                    [status(401)]).


%%%%%%%%%%%%%%%%%%%% Role handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(role), cors_handler(Method, role_handler),
                [method(Method),
                 prefix,
                 methods([options,post])]).

role_handler(post, Request, System_DB, Auth) :-
    get_payload(Document, Request),

    catch_with_backtrace(
        (   get_role(System_DB, Auth, Document, Response),
            cors_reply_json(Request,
                            Response)),
        E,
        do_or_die(woql_error_handler(E, Request),
                  E)).

%%%%%%%%%%%%%%%%%%%% Console Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(.), cors_handler(Method, console_handler),
                [method(Method),
                 methods([options,get])]).
:- http_handler(root(db), cors_handler(Method, console_handler),
                [method(Method),
                 prefix,
                 methods([options,get])]).
:- http_handler(root(home), cors_handler(Method, console_handler),
                [method(Method),
                 prefix,
                 methods([options,get])]).

/*
 * console_handler(+Method,+Request) is det.
 */
console_handler(get,Request, _System_DB, _Auth) :-
    config:index_path(Index_Path),
    write_cors_headers(Request),
    throw(http_reply(file('text/html', Index_Path))).

:- begin_tests(console_route).
:- use_module(core(util/test_utils)).

test(console_route, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    format(string(ConsoleURL), "~s/", [Server]),
    http_get(ConsoleURL, _, [request_header('Origin'=Server)]).

test(console_route_empty, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    format(string(ConsoleURL), "~s", [Server]),
    http_get(ConsoleURL, _, [request_header('Origin'=Server)]).

test(console_route_db, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    format(string(ConsoleURL), "~s/db/gavin/baseball", [Server]),
    http_get(ConsoleURL, _, [request_header('Origin'=Server)]).

test(console_route_home, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    format(string(ConsoleURL), "~s/home/somewhere", [Server]),
    http_get(ConsoleURL, _, [request_header('Origin'=Server)]).

:- end_tests(console_route).

%%%%%%%%%%%%%%%%%%%% Reply Hackery %%%%%%%%%%%%%%%%%%%%%%
:- meta_predicate cors_handler(+,2,?).
cors_handler(options, _Goal, Request) :-
    !,
    write_cors_headers(Request),
    format('~n').
cors_handler(Method, Goal, R) :-
    (   memberchk(Method, [post, put, delete])
    ->  add_payload_to_request(R,Request)
    ;   Request = R),

    open_descriptor(system_descriptor{}, System_Database),
    catch((   authenticate(System_Database, Request, Auth),
              cors_catch(Method, Goal, Request, System_Database, Auth)),

          error(authentication_incorrect(Reason),_),

          (   write_cors_headers(Request),
              http_log("~NAuthentication Incorrect for reason: ~q~n", [Reason]),
              reply_json(_{'@type' : 'api:ErrorResponse',
                           'api:status' : 'api:failure',
                           'api:error' : _{'@type' : 'api:IncorrectAuthenticationError'},
                           'api:message' : 'Incorrect authentication information'
                          },
                         [status(401)]))),
    !.
cors_handler(_Method, Goal, R) :-
    write_cors_headers(R),
    format(string(Msg), "Failed to run the API endpoint goal ~q", Goal),
    reply_json(_{'@type' : 'api:ErrorResponse',
                 'api:status' : 'api:failure',
                 'api:error' : _{'@type' : 'api:APIEndpointFailed'},
                 'api:message' : Msg
                },
               [status(500)]).

% Evil mechanism for catching, putting CORS headers and re-throwing.
:- meta_predicate cors_catch(+,3,?,?,?).
cors_catch(Method, Goal, Request, System_Database, Auth) :-
    strip_module(Goal, Module, PlainGoal),
    PlainGoal =.. [Head|Args],
    NewArgs = [Method|Args],
    NewPlainGoal =.. [Head|NewArgs],
    NewGoal = Module:NewPlainGoal,
    catch(call(NewGoal, Request, System_Database, Auth),
          E,
          (
              write_cors_headers(Request),
              customise_exception(E)
          )
         ),
    !.
cors_catch(_,Request) :-
    write_cors_headers(Request),
    % Probably should extract the path from Request
    reply_json(_{'api:status' : 'api:failure',
                 'api:message' :
                 _{'@type' : 'xsd:string',
                   '@value' : 'Unexpected failure in request handler'}},
               [status(500)]).

customise_exception(reply_json(M,Status)) :-
    reply_json(M,
               [status(Status)]).
customise_exception(reply_json(M)) :-
    customise_exception(reply_json(M,200)).
customise_exception(error(access_not_authorised(Auth,Action,Scope))) :-
    format(string(Msg), "Access to ~q is not authorised with action ~q and auth ~q",
           [Scope,Action,Auth]),
    term_string(Auth, Auth_String),
    term_string(Action, Action_String),
    term_string(Scope, Scope_String),
    reply_json(_{'api:status' : 'api:failure',
                 'api:message' : Msg,
                 'auth' : Auth_String,
                 'action' : Action_String,
                 'scope' : Scope_String
                },
               [status(403)]).
customise_exception(error(bad_api_document(Document,Expected),_)) :-
    format(string(Msg), "The input API document was missing required fields: ~q", [Expected]),
    reply_json(_{'@type' : 'api:BadAPIDocumentErrorResponse',
                 'api:status' : 'api:failure',
                 'api:error' : _{'@type' : 'api:RequiredFieldsMissing',
                                 'api:expected' : Expected,
                                 'api:document' : Document},
                 'api:message' : Msg},
               [status(400)]).

%% everything below this comment is dubious for this case predicate. a lot of these cases should be handled internally by their respective route handlers.
customise_exception(syntax_error(M)) :-
    format(atom(OM), '~q', [M]),
    reply_json(_{'api:status' : 'api:failure',
                 'system:witnesses' : [_{'@type' : 'vio:ViolationWithDatatypeObject',
                                           'vio:literal' : OM}]},
               [status(400)]).
customise_exception(error(syntax_error(M),_)) :-
    format(atom(OM), '~q', [M]),
    reply_json(_{'api:status' : 'api:failure',
                 'system:witnesses' : [_{'@type' : 'vio:ViolationWithDatatypeObject',
                                           'vio:literal' : OM}]},
               [status(400)]).
customise_exception(error(woql_syntax_error(JSON,Path,Element),_)) :-
    json_woql_path_element_error_message(JSON,Path,Element,Message),
    reverse(Path,Director),
    reply_json(_{'@type' : 'vio:WOQLSyntaxError',
                 'api:message' : Message,
                 'vio:path' : Director,
                 'vio:query' : JSON},
               [status(400)]).
customise_exception(error(syntax_error(M))) :-
    format(atom(OM), '~q', [M]),
    reply_json(_{'api:status' : 'api:failure',
                 'system:witnesses' : [_{'@type' : 'vio:ViolationWithDatatypeObject',
                                           'vio:literal' : OM}]},
               [status(400)]).
customise_exception(error(type_error(T,O),C)) :-
    format(atom(M),'Type error for ~q which should be ~q with context ~q', [O,T,C]),
    format(atom(OA), '~q', [O]),
    format(atom(TA), '~q', [T]),
    reply_json(_{'api:status' : 'api:failure',
                 'system:witnesses' : [_{'@type' : 'vio:ViolationWithDatatypeObject',
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
    reply_json(_{'api:status' : 'api:failure',
                 'api:message' : 'Connection timed out'
               },
               [status(408)]).
customise_exception(error(unqualified_resource_id(Doc_ID))) :-
    format(atom(MSG), 'Document resource ~s could not be expanded', [Doc_ID]),
    reply_json(_{'api:status' : 'terminus_failure',
                 'api:message' : MSG,
                 'system:object' : Doc_ID},
               [status(400)]).
customise_exception(error(unknown_deletion_error(Doc_ID))) :-
    format(atom(MSG), 'unqualfied deletion error for id ~s', [Doc_ID]),
    reply_json(_{'api:status' : 'terminus_failure',
                 'api:message' : MSG,
                 'system:object' : Doc_ID},
               [status(400)]).
customise_exception(error(schema_check_failure(Witnesses))) :-
    reply_json(Witnesses,
               [status(405)]).
customise_exception(error(database_not_found(DB))) :-
    format(atom(MSG), 'Database ~s could not be destroyed', [DB]),
    reply_json(_{'api:message' : MSG,
                 'api:status' : 'api:failure'},
               [status(400)]).
customise_exception(error(database_does_not_exist(DB))) :-
    format(atom(M), 'Database does not exist with the name ~q', [DB]),
    reply_json(_{'api:message' : M,
                 'api:status' : 'api:failure'},
               [status(400)]).
customise_exception(error(database_files_do_not_exist(DB))) :-
    format(atom(M), 'Database fields do not exist for database with the name ~q', [DB]),
    reply_json(_{'api:message' : M,
                 'api:status' : 'api:failure'},
               [status(400)]).
customise_exception(error(database_already_exists(DB))) :-
    format(atom(MSG), 'Database ~s already exists', [DB]),
    reply_json(_{'api:status' : 'api:failure',
                 'system:object' : DB,
                 'api:message' : MSG,
                 'system:method' : 'system:create_database'},
               [status(409)]).
customise_exception(error(database_could_not_be_created(DB))) :-
    format(atom(MSG), 'Database ~s could not be created', [DB]),
    reply_json(_{'api:status' : 'api:failure',
                 'api:message' : MSG,
                 'system:method' : 'system:create_database'},
               [status(409)]).
customise_exception(error(could_not_create_class_frame(Class))) :-
    format(atom(MSG), 'Class Frame could not be generated for class ~s', [Class]),
    reply_json(_{ 'api:message' : MSG,
                  'api:status' : 'api:failure',
                  'system:class' : Class},
               [status(400)]).
customise_exception(error(could_not_create_filled_class_frame(Instance))) :-
    format(atom(MSG), 'Class Frame could not be generated for instance ~s', [Instance]),
    reply_json(_{ 'api:message' : MSG,
                  'api:status' : 'api:failure',
                  'system:instance' : Instance},
               [status(400)]).
customise_exception(error(maformed_json(Atom))) :-
    format(atom(MSG), 'Malformed JSON Object ~q', [MSG]),
    reply_json(_{'api:status' : 'api:failure',
                 'api:message' : MSG,
                 'system:object' : Atom},
               [status(400)]).
customise_exception(error(no_document_for_key(Key))) :-
    format(atom(MSG), 'No document in request for key ~q', [Key]),
    reply_json(_{'api:status' : 'api:failure',
                 'api:message' : MSG,
                 'system:key' : Key},
               [status(400)]).
customise_exception(error(no_parameter_key_in_document(Key,Document))) :-
    format(atom(MSG), 'No parameter key ~q for method ~q', [Key,Document]),
    reply_json(_{'api:status' : 'api:failure',
                 'api:message' : MSG,
                 'system:key' : Key,
                 'system:object' : Document},
               [status(400)]).
customise_exception(error(no_parameter_key_form_method(Key,Method))) :-
    format(atom(MSG), 'No parameter key ~q for method ~q', [Key,Method]),
    reply_json(_{'api:status' : 'api:failure',
                 'api:message' : MSG,
                 'system:object' : Key},
               [status(400)]).
customise_exception(error(no_parameter_key(Key))) :-
    format(atom(MSG), 'No parameter key ~q', [Key]),
    reply_json(_{'api:status' : 'api:failure',
                 'api:message' : MSG,
                 'system:object' : Key},
               [status(400)]).
customise_exception(error(branch_already_exists(Branch_Name))) :-
    format(string(Msg), "branch ~w already exists", [Branch_Name]),
    reply_json(_{'api:status' : 'api:failure',
                 'api:message' : Msg},
               [status(400)]).
customise_exception(error(origin_branch_does_not_exist(Branch_Name))) :-
    format(string(Msg), "origin branch ~w does not exist", [Branch_Name]),
    reply_json(_{'api:status' : 'api:failure',
                 'api:message' : Msg},
               [status(400)]).
customise_exception(error(origin_commit_does_not_exist(Commit_Id))) :-
    format(string(Msg), "origin commit ~w does not exist", [Commit_Id]),
    reply_json(_{'api:status' : 'api:failure',
                 'api:message' : Msg},
               [status(400)]).
customise_exception(error(origin_cannot_be_branched(Descriptor))) :-
    format(string(Msg), "origin ~w cannot be branched", [Descriptor]),
    reply_json(_{'api:status' : 'api:failure',
                 'api:message' : Msg},
               [status(400)]).
customise_exception(error(E)) :-
    format(atom(EM),'Error: ~q', [E]),
    reply_json(_{'api:status' : 'api:failure',
                 'api:message' : EM},
               [status(500)]).
customise_exception(error(E, CTX)) :-
    http_log('~N[Exception] ~q~n',[error(E,CTX)]),
    format(atom(EM),'Error: ~q in CTX ~q', [E, CTX]),
    reply_json(_{'api:status' : 'api:failure',
                 'api:message' : EM},
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

:- if(\+((config:jwt_public_key_path(Path), Path = ''))).
/*
 *  fetch_jwt_data(+Request, -Username) is semi-determinate.
 *
 *  Fetches the HTTP JWT data
 */
fetch_jwt_data(Token, Username) :-
    atom_string(TokenAtom, Token),

    do_or_die(jwt_decode(TokenAtom, Payload, []),
              error(authentication_incorrect(jwt_decode_failed(TokenAtom)), _)),

    do_or_die(
        (   atom_json_dict(Payload, PayloadDict, []),
            % replace with dict key get (or whatever it is called)
            get_dict('http://terminusdb.com/schema/system#agent_name', PayloadDict, UsernameString),
            atom_string(Username, UsernameString)),
        error(malformed_jwt_payload(Payload))).
:- else.
fetch_jwt_data(_Token, _Username) :-
    throw(error(authentication_incorrect(jwt_authentication_requested_but_no_key_configured),_)).
:- endif.


/*
 * authenticate(+Database, +Request, -Auth_Obj) is det.
 *
 * This should either bind the Auth_Obj or throw an http_status_reply/4 message.
 */
authenticate(System_Askable, Request, Auth) :-
    fetch_authorization_data(Request, Username, KS),
    !,
    do_or_die(user_key_user_id(System_Askable, Username, KS, Auth),
              error(authentication_incorrect(basic_auth(Username)),_)).
authenticate(System_Askable, Request, Auth) :-
    memberchk(authorization(Text), Request),
    pattern_string_split(" ", Text, ["Bearer", Token]),
    !,
    % Try JWT if no http keys
    fetch_jwt_data(Token, Username),
    do_or_die(username_auth(System_Askable, Username, Auth),
              error(authentication_incorrect(jwt_no_user_with_name(Username)),_)).
authenticate(_, _, doc:anonymous).

/*
 * write_cors_headers(Request) is det.
 *
 * Writes cors headers associated with Resource_URI
 */
write_cors_headers(Request) :-
    (   memberchk(origin(Origin), Request)
    ->  current_output(Out),
        format(Out,'Access-Control-Allow-Methods: GET, POST, PUT, DELETE, OPTIONS\n',[]),
        format(Out,'Access-Control-Allow-Credentials: true\n',[]),
        format(Out,'Access-Control-Max-Age: 1728000\n',[]),
        format(Out,'Access-Control-Allow-Headers: Authorization, Authorization-Remote, Accept, Accept-Encoding, Accept-Language, Host, Origin, Referer, Content-Type, Content-Length, Content-Range, Content-Disposition, Content-Description\n',[]),
        format(Out,'Access-Control-Allow-Origin: ~s~n',[Origin])
    ;   true).

cors_reply_json(Request, JSON) :-
    write_cors_headers(Request),
    reply_json(JSON).

cors_reply_json(Request, JSON, Options) :-
    write_cors_headers(Request),
    reply_json(JSON, Options).

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
    memberchk(method(Method), Request),
    memberchk(Method, [post,put]),
    memberchk(multipart(Form_Data), Request),
    !,
    member(mime(Mime_Header,Encoded_Value,_),Form_Data),
    memberchk(name(Key), Mime_Header),
    (   memberchk(type('application/json'),Mime_Header)
    ->  atom_json_dict(Encoded_Value,Value,[])
    ;   uri_encoded(query_value, Value, Encoded_Value)).
try_get_param(Key,Request,Value) :-
    % GET or POST (but not application/json)
    memberchk(method(Method), Request),
    (   memberchk(Method, [get,delete])
    ;   memberchk(Method, [post,put]),
        \+ memberchk(content_type('application/json'), Request)),

    http_parameters(Request, [], [form_data(Data)]),

    (   memberchk(Key=Value,Data)
    <>  throw(error(no_parameter_key_in_document(Key,Data)))),
    !.
try_get_param(Key,Request,Value) :-
    % POST with JSON package
    memberchk(method(Method), Request),
    memberchk(Method, [post,put]),

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
    memberchk(method(Method), Request),
    memberchk(Method, [post,put]),

    memberchk(multipart(Form_Data), Request),
    !,
    memberchk(mime(Mime_Header,Encoded_Value,_),Form_Data),
    memberchk(name(Key), Mime_Header),
    (   memberchk(type('application/json'),Mime_Header)
    ->  atom_json_dict(Encoded_Value,Value,[])
    ;   uri_encoded(query_value, Value, Encoded_Value)).
get_param(Key,Request,Value) :-
    % GET or POST (but not application/json)
    memberchk(method(Method), Request),
    (   memberchk(Method, [get,delete])
    ;   memberchk(Method, [post,put]),
        \+ memberchk(content_type('application/json'), Request)),

    http_parameters(Request, [], [form_data(Data)]),
    memberchk(Key=Value,Data),
    !.
get_param(Key,Request,Value) :-
    % POST with JSON package
    memberchk(method(Method), Request),
    memberchk(Method, [post,put]),

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
add_payload_to_request(Request,[multipart(Form_Data)|Request]) :-
    memberchk(content_type(ContentType), Request),
    http_parse_header_value(
        content_type, ContentType,
        media(multipart/'form-data', _)
    ),
    !,
    http_read_data(Request, Form_Data, [on_filename(save_post_file),form_data(mime)]).
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
    convlist([mime(Mime_Header,Data,_),Filename=Temp_Filename]>>(
                 memberchk(filename(Filename),Mime_Header),
                 \+ memberchk(type('application/json'),Mime_Header),
                 setup_call_cleanup(
                     tmp_file_stream(octet, Temp_Filename, Out),
                     write(Out, Data),
                     close(Out)
                 )
             ),Parts,Files).
collect_posted_files(_Request,[]).
