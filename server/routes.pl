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

%%%%%%%%%%%%%%%%%%%% Connection Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(.), cors_handler(Method, connect_handler),
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
/*
 * Tests assume that  setenv("TERMINUSDB_SERVER_JWT_PUBLIC_KEY_PATH", "test/public_key_test.key.pub")
 * setenv("TERMINUSDB_SERVER_JWT_PUBLIC_KEY_ID", "testkey") are set
 */
test(connection_authorized_user_jwt, [
     ]) :-
    config:server(Server),
    Bearer = 'eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6InRlc3RrZXkifQ.eyJodHRwOi8vdGVybWludXNkYi5jb20vc2NoZW1hL3N5c3RlbSNhZ2VudF9uYW1lIjoiYWRtaW4iLCJodHRwOi8vdGVybWludXNkYi5jb20vc2NoZW1hL3N5c3RlbSN1c2VyX2lkZW50aWZpZXIiOiJhZG1pbkB1c2VyLmNvbSIsImlzcyI6Imh0dHBzOi8vdGVybWludXNodWIuZXUuYXV0aDAuY29tLyIsInN1YiI6ImF1dGgwfDVlZmVmY2NkYmIzMzEzMDAxMzlkNTAzNyIsImF1ZCI6WyJodHRwczovL3Rlcm1pbnVzaHViL3JlZ2lzdGVyVXNlciIsImh0dHBzOi8vdGVybWludXNodWIuZXUuYXV0aDAuY29tL3VzZXJpbmZvIl0sImlhdCI6MTU5Mzc2OTE4MywiYXpwIjoiTUpKbmRHcDB6VWRNN28zUE9UUVBtUkpJbVkyaG8wYWkiLCJzY29wZSI6Im9wZW5pZCBwcm9maWxlIGVtYWlsIn0.hxJphuKyWLbTLTgFq37tHQvNaxDwWxeOyDVbEemYoWDhBbSbjcjP034jJ0PhupYqtdadZV4un4j9QkJeYDLNtZLD7q4tErNK5bDw9gM1z9335BSu9htjLOZEF2_DqJYLGdbazWA3MAGkg6corOCXDVyBZpToekvylsGAMztkZaeAIgnsJlHxIIMMLQHrHNCBRPC1U6ZJQc7WZdgB-gefVlVQco0w8_Q0Z28DeshD9y3XChTMeTAAT-URwmz61RB6aUFMXpr4tTtYwyXGsWdu46LuDNxOV070eTybthDpDjyYSDsn-i4YbHvDGN5kUen9pw6b47CkSdhsSSjVQLsNkA',
    http_get(Server, _, [authorization(bearer(Bearer))]).

test(connection_unauthorized_user_jwt, [
     ]) :-
    config:server(Server),
    % mangled the payload so it should not validate
    Bearer = 'eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6InRlc3RrZXkifQ.eyJodHRwOi8vdGVybWludXNkYi5jb20vc2NoZW1hL3N5c3RlbSNhZ2VudF9uYW1lIjoiYWRtaW4iLCJodHRwOi8vdGVybWludXNkYi5jb20vc2NoZW1hL3N5c3RlbSN1c2VyX2lkZW50aWZpZXIiOiJhZG1pbkB1c2VyLmNvbSIsImlzcyI6Imh0dHBzOi8vdGVybWludXNodW0000000000aDAuY29tLyIsInN1YiI6ImF1dGgwfDVlZmVmY2NkYmIzMzEzMDAxMzlkNTAzNyIsImF1ZCI6WyJodHRwczovL3Rlcm1pbnVzaHViL3JlZ2lzdGVyVXNlciIsImh0dHBzOi8vdGVybWludXNodWIuZXUuYXV0aDAuY29tL3VzZXJpbmZvIl0sImlhdCI6MTU5Mzc2OTE4MywiYXpwIjoiTUpKbmRHcDB6VWRNN28zUE9UUVBtUkpJbVkyaG8wYWkiLCJzY29wZSI6Im9wZW5pZCBwcm9maWxlIGVtYWlsIn0.hxJphuKyWLbTLTgFq37tHQvNaxDwWxeOyDVbEemYoWDhBbSbjcjP034jJ0PhupYqtdadZV4un4j9QkJeYDLNtZLD7q4tErNK5bDw9gM1z9335BSu9htjLOZEF2_DqJYLGdbazWA3MAGkg6corOCXDVyBZpToekvylsGAMztkZaeAIgnsJlHxIIMMLQHrHNCBRPC1U6ZJQc7WZdgB-gefVlVQco0w8_Q0Z28DeshD9y3XChTMeTAAT-URwmz61RB6aUFMXpr4tTtYwyXGsWdu46LuDNxOV070eTybthDpDjyYSDsn-i4YbHvDGN5kUen9pw6b47CkSdhsSSjVQLsNkA',
    http_get(Server, _, [authorization(bearer(Bearer)), status_code(Status)]),

    Status = 401.

:- end_tests(jwt_auth).

:- begin_tests(connect_handler).
:- use_module(core(util/test_utils)).

test(connection_authorised_user_http_basic, [
     ]) :-
    config:server(Server),
    admin_pass(Key),
    http_get(Server, _, [authorization(basic(admin, Key))]).


test(connection_result_dbs, [])
:-
    config:server(Server),
    admin_pass(Key),
    http_get(Server, Result, [json_object(dict),authorization(basic(admin, Key))]),

    * json_write_dict(current_output, Result, []),

    _{ '@id' : "doc:admin",
       '@type':"system:User"
     } :< Result.

:- end_tests(connect_handler).


%%%%%%%%%%%%%%%%%%%% Console Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(console/_Path), cors_handler(Method, console_handler),
                [method(Method),
                 methods([options,get])]).

/*
 * console_handler(+Method,+Request) is det.
 */
console_handler(get,Request, _System_DB, _Auth) :-
    config:index_path(Index_Path),
    write_cors_headers(Request),
    throw(http_reply(file('text/html', Index_Path))).

:- begin_tests(console_route).

test(console_route) :-
    config:server(SURI),
    format(string(ConsoleURL), "~s/console/", [SURI]),
    http_get(ConsoleURL, _, [request_header('Origin'=SURI)]).

:- end_tests(console_route).

%%%%%%%%%%%%%%%%%%%% Message Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(message), cors_handler(Method, message_handler),
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
:- http_handler(root(db/Account/DB), cors_handler(Method, db_handler(Account, DB)),
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
                      'api:error' : _{'@type' : 'api:DatabaseDoesNotExists',
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
         setup(((   database_exists('admin', 'TEST_DB')
                ->  force_delete_db('admin', 'TEST_DB')
                ;   true))),
         cleanup(force_delete_db('admin', 'TEST_DB'))
     ]) :-
    config:server(Server),
    atomic_list_concat([Server, '/db/admin/TEST_DB'], URI),
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
         setup(((   database_exists('admin', 'TEST_DB')
                ->  force_delete_db('admin', 'TEST_DB')
                ;   true)))
     ]) :-
    config:server(Server),
    atomic_list_concat([Server, '/db/admin/TEST_DB'], URI),
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
         setup(((   database_exists('admin', 'TEST_DB')
                ->  force_delete_db('admin', 'TEST_DB')
                ;   true),
                create_db_without_schema("admin", "TEST_DB")
               )),
         cleanup(force_delete_db('admin', 'TEST_DB'))
     ]) :-
    config:server(Server),
    atomic_list_concat([Server, '/db/admin/TEST_DB'], URI),
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
     ]) :-
    config:server(Server),
    atomic_list_concat([Server, '/db/THIS_ORG_DOES_NOT_EXIST/TEST_DB'], URI),
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
     ]) :-
    config:server(Server),
    atomic_list_concat([Server, '/db/admin/TEST_DB'], URI),
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
         setup(add_user("TERMINUSQA",'user1@example.com','a comment', some('password'),_User_ID)),
         cleanup(delete_user_and_organization("TERMINUSQA"))
     ]) :-
    config:server(Server),
    atomic_list_concat([Server, '/db/admin/TEST_DB'], URI),
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
         setup(((   database_exists('admin', 'TEST_DB')
                ->  force_delete_db('admin', 'TEST_DB')
                ;   true),
                create_db_without_schema("admin", "TEST_DB")))
     ]) :-
    config:server(Server),
    atomic_list_concat([Server, '/db/admin/TEST_DB'], URI),
    admin_pass(Key),
    http_delete(URI, Delete_In, [json_object(dict),
                                 authorization(basic(admin, Key))]),

    _{'api:status' : "api:success"} :< Delete_In.

test(db_delete_unknown_organization_errors, [
     ]) :-
    config:server(Server),
    atomic_list_concat([Server, '/db/THIS_ORG_DOES_NOT_EXIST/TEST_DB'], URI),
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
     ]) :-
    config:server(Server),
    atomic_list_concat([Server, '/db/admin/TEST_DB'], URI),
    admin_pass(Key),
    http_delete(URI,
                Result,
                [json_object(dict),
                 authorization(basic(admin, Key)),
                 status_code(Status)]),

    Status = 400,

    _{'api:status' : "api:failure"} :< Result.


test(db_auth_test, [
         setup(((   organization_name_exists(system_descriptor{}, 'TERMINUS_QA')
                ->  delete_organization('TERMINUS_QA')
                ;   true
                ),
                (   agent_name_exists(system_descriptor{}, 'TERMINUS_QA')
                ->  delete_user('TERMINUS_QA')
                ;   add_user('TERMINUS_QA','user@example.com','comment', some('password'),_User_ID)
                ),
                (   database_exists('TERMINUS_QA', 'TEST_DB')
                ->  force_delete_db('TERMINUS_QA', 'TEST_DB')
                ;   true))),
         cleanup(((   database_exists('TERMINUS_QA', 'TEST_DB')
                  ->  force_delete_db('TERMINUS_QA', 'TEST_DB')
                  ;   true),
                  delete_user_and_organization('TERMINUS_QA')))
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
    _{'api:status' : "api:success"} :< In.

:- end_tests(db_endpoint).


%%%%%%%%%%%%%%%%%%%% Triples Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(triples/Path), cors_handler(Method, triples_handler(Path)),
                [method(Method),
                 prefix,
                 time_limit(infinite),
                 methods([options,get,post])]).

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
        (   graph_load(System_DB, Auth, Path, Commit_Info, "turtle", TTL),
            cors_reply_json(Request, _{'@type' : 'api:TriplesLoadResponse',
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


:- begin_tests(triples_endpoint).

:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(triples_update, [
         setup(((   database_exists(admin, 'TEST_DB')
                ->  force_delete_db(admin, 'TEST_DB')
                ;   true),
                create_db_without_schema(admin, 'TEST_DB'))),
         cleanup((force_delete_db(admin, 'TEST_DB')))

     ])
:-
    % We actually have to create the graph before we can post to it!
    % First make the schema graph
    make_branch_descriptor(admin, 'TEST_DB', Branch_Descriptor),
    super_user_authority(Auth),
    create_graph(system_descriptor{},
                 Auth,
                 "admin/TEST_DB/local/branch/master/schema/main",
                 commit_info{ author : "test",
                              message: "Generated by automated testing"},
                 Transaction_Metadata),
    * json_write_dict(current_output,Transaction_Metadata, []),

    terminus_path(Path),
    interpolate([Path, '/terminus-schema/system_schema.owl.ttl'], TTL_File),
    read_file_to_string(TTL_File, TTL, []),
    config:server(Server),
    atomic_list_concat([Server, '/triples/admin/TEST_DB/local/branch/master/schema/main'], URI),
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


test(triples_get, [])
:-

    config:server(Server),
    atomic_list_concat([Server, '/triples/_system/schema/main'], URI),
    admin_pass(Key),
    http_get(URI, In, [json_object(dict),
                       authorization(basic(admin, Key))]),
    string(In).


test(triples_post_get, [
         setup(((   database_exists("admin", "Jumanji")
                ->  force_delete_db("admin", "Jumanji")
                ;   true),
                create_db_without_schema("admin", "Jumanji"))),
         cleanup(force_delete_db("admin", "Jumanji"))
     ])
:-

    super_user_authority(Auth),
    create_graph(system_descriptor{},
                 Auth,
                 "admin/Jumanji/local/branch/master/schema/main",
                 commit_info{ author : "test",
                              message: "Generated by automated testing"},
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


test(get_invalid_descriptor, [])
:-
    config:server(Server),
    atomic_list_concat([Server, '/triples/nonsense'], URI),
    admin_pass(Key),

    http_get(URI, In, [json_object(dict),
                        authorization(basic(admin, Key)),
                        status_code(Code)]),
    _{'api:message':_Msg,
      'api:status':"api:failure"} :< In,
    Code = 400.


test(get_bad_descriptor, [])
:-
    config:server(Server),
    atomic_list_concat([Server, '/triples/admin/fdsa'], URI),
    admin_pass(Key),

    http_get(URI, In, [json_object(dict),
                        authorization(basic(admin, Key)),
                        status_code(Code)]),
    _{'api:message':_,
      'api:status':"api:failure"} :< In,
    Code = 400.

:- end_tests(triples_endpoint).

%%%%%%%%%%%%%%%%%%%% Frame Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(frame/Path), cors_handler(Method, frame_handler(Path)),
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

test(get_frame, [])
:-
    config:server(Server),
    atomic_list_concat([Server, '/frame/_system'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{ class : "system:Agent"
                    }),
              JSON, [json_object(dict),
                     authorization(basic(admin, Key))]),
    _{'@type':"system:Frame"} :< JSON.


test(get_filled_frame, [])
:-
    config:server(Server),
    atomic_list_concat([Server, '/frame/_system'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{ instance : "doc:admin"
                    }),
              JSON, [json_object(dict),
                     authorization(basic(admin, Key))]),
    _{'@type':"system:FilledFrame"} :< JSON.


test(bad_path_filled_frame, [])
:-
    config:server(Server),
    atomic_list_concat([Server, '/frame/garbage'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{ instance : "doc:admin"
                    }),
              JSON, [json_object(dict),
                     authorization(basic(admin, Key)),
                     status_code(Status)]),
    \+ Status = 200,
    JSON.'api:error'.'@type' = "api:BadAbsoluteDescriptor".


test(unresolvable_path_filled_frame, [])
:-
    config:server(Server),
    atomic_list_concat([Server, '/frame/believable/garbage'], URI),
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
:- http_handler(root(woql), cors_handler(Method, woql_handler),
                [method(Method),
                 time_limit(infinite),
                 methods([options,post])]).
:- http_handler(root(woql/Path), cors_handler(Method, woql_handler(Path)),
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
    try_get_param('query',Request,Query),

    (   get_param('commit_info', Request, Commit_Info)
    ->  true
    ;   Commit_Info = _{}
    ),
    collect_posted_files(Request,Files),

    catch_with_backtrace(
        (   woql_query_json(System_DB, Auth, none, Query, Commit_Info, Files, JSON),
            write_cors_headers(Request),
            reply_json_dict(JSON)
        ),
        E,
        do_or_die(woql_error_handler(E, Request),
                  E)).

woql_handler(post, Path, Request, System_DB, Auth) :-
    try_get_param('query',Request,Query),

    (   get_param('commit_info', Request, Commit_Info)
    ->  true
    ;   Commit_Info = _{}
    ),
    collect_posted_files(Request,Files),

    catch_with_backtrace(
        (   woql_query_json(System_DB, Auth, some(Path), Query, Commit_Info, Files, JSON),
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
woql_error_handler(error(woql_syntax_error(Term),_), Request) :-
    term_string(Term,String),
    format(string(Msg), "The following descriptor could not be resolved to a resource: ~q", [String]),
    cors_reply_json(Request,
                    _{'@type' : 'api:WoqlErrorResponse',
                      'api:status' : 'api:failure',
                      'api:error' : _{ '@type' : 'api:WOQLSyntaxError',
                                       'api:error_term' : String},
                      'api:message' : Msg
                     },
                    [status(404)]).


% woql_handler Unit Tests

:- begin_tests(woql_endpoint).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(db_not_there, []) :-
    config:server(Server),
    atomic_list_concat([Server, '/woql/admin/blagblagblagblagblag'], URI),
    admin_pass(Key),
    http_post(URI,
              json(_{'query' : ""}),
              JSON,
              [status_code(Code), json_object(dict),authorization(basic(admin,Key))]),
    Code = 404,
    _{'@type' : "api:WoqlErrorResponse",
      'api:error' :
      _{'@type' : "api:UnresolvableAbsoluteDescriptor",
        'api:absolute_descriptor': "admin/blagblagblagblagblag/local/branch/master"}} :< JSON.

test(no_db, [])
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
                (   database_exists(admin,test)
                ->  force_delete_db(admin,test)
                ;   true),
                create_db_without_schema(admin,test))),
         cleanup((force_delete_db(admin,test)))
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
                (   database_exists(admin,test)
                ->  force_delete_db(admin,test)
                ;   true),
                create_db_without_schema(admin,test))),
         cleanup((force_delete_db(admin,test)))
     ])
:-
    config:server(Server),
    atomic_list_concat([Server, '/woql/admin/test'], URI),

    % First make a schema against which we can have an object

    super_user_authority(Auth),
    create_graph(system_descriptor{},
                 Auth,
                 "admin/test/local/branch/master/schema/main",
                 commit_info{ author : "test",
                              message: "Generated by automated testing"},
                 _Transaction_Metadata2),

    terminus_path(Path),
    interpolate([Path, '/terminus-schema/system_schema.owl.ttl'], TTL_File),
    read_file_to_string(TTL_File, TTL, []),

    Graph = "admin/test/local/branch/master/schema/main",
    super_user_authority(Auth),
    graph_load(system_descriptor{}, Auth, Graph,
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
         setup((config:server(Server),
                (   database_exists(admin,test)
                ->  force_delete_db(admin,test)
                ;   true),
                create_db_without_schema(admin,test))),
         cleanup((force_delete_db(admin,test)))
     ])
:-

    make_branch_descriptor("admin","test",Branch_Descriptor),

    % First make a schema graph
    super_user_authority(Auth),
    create_graph(system_descriptor{},
                 Auth,
                 "admin/test/local/branch/master/schema/main",
                 commit_info{ author : "test",
                              message: "Generated by automated testing"},
                 _Transaction_Metadata2),

    % Create the schema
    terminus_path(Path),
    interpolate([Path, '/terminus-schema/system_schema.owl.ttl'], TTL_File),
    read_file_to_string(TTL_File, TTL, []),

    Graph = "admin/test/local/branch/master/schema/main",
    super_user_authority(Auth),
    graph_load(system_descriptor{}, Auth, Graph,
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
      document_uri : _{ '@type' : "woql:Node",
                        'woql:node' : "doc:admin"},
      document : _{'@type' : "Variable",
                   variable_name : _{ '@type' : "xsd:string",
                                      '@value' : "Document"}}},

    config:server(Server),
    admin_pass(Key),
    atomic_list_concat([Server, '/woql/_system'], URI),
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
:- http_handler(root(clone/Organization/DB), cors_handler(Method, clone_handler(Organization, DB)),
                [method(Method),
                 methods([options,post])]).

clone_handler(post, Organization, DB, Request, System_DB, Auth) :-
    request_remote_authorization(Request, Authorization),
    get_payload(Database_Document,Request),

    do_or_die(
        (_{ comment : Comment,
            label : Label,
            remote_url: Remote_URL} :< Database_Document),
        error(bad_api_document(Database_Document,[comment,label,remote_url]),_)),

    (   _{ public : Public } :< Database_Document
    ->  true
    ;   Public = false),

    catch_with_backtrace(
        (   clone(System_DB, Auth, Organization,DB,Label,Comment,Public,Remote_URL,authorized_fetch(Authorization),_Meta_Data),
            write_cors_headers(Request),
            reply_json_dict(
                _{'@type' : 'api:CloneResponse',
                  'api:status' : 'api:success'})
        ),
        E,
        do_or_die(clone_error_handler(E,Request),
                  E)).

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
         setup((cleanup_user_database("TERMINUSQA1", "foo"),
                cleanup_user_database("TERMINUSQA2", "bar"),

                add_user("TERMINUSQA1",'user1@example.com','a comment', some('password1'),_User_ID1),
                add_user("TERMINUSQA2",'user2@example.com','a comment', some('password2'),_User_ID2),
                create_db_without_schema("TERMINUSQA1", "foo"))),
         cleanup((cleanup_user_database("TERMINUSQA1", "foo"),
                  cleanup_user_database("TERMINUSQA2", "bar")))
     ])
:-
    resolve_absolute_string_descriptor("TERMINUSQA1/foo", Foo_Descriptor),
    create_context(Foo_Descriptor, commit_info{author:"test",message:"test"}, Foo_Context),
    with_transaction(Foo_Context,
                     ask(Foo_Context,
                         insert(a,b,c)),
                     _),

    config:server(Server),
    atomic_list_concat([Server, '/clone/TERMINUSQA2/bar'], URL),
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

:- end_tests(clone_endpoint).

%%%%%%%%%%%%%%%%%%%% Fetch Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(fetch/Path), cors_handler(Method, fetch_handler(Path)),
                [method(Method),
                 prefix,
                 methods([options,post])]).

fetch_handler(post,Path,Request, System_DB, Auth) :-
    request_remote_authorization(Request, Authorization),

    catch_with_backtrace(
        (   remote_fetch(System_DB, Auth, Path, authorized_fetch(Authorization),
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
               json_object(dict),
               status_code(Status)]),

    (   Status = 200
    ->  Payload_Option = some(Payload)
    ;   Status = 204
    ->  Payload_Option = none
    ;   throw(error(remote_connection_error(Payload),_))).


%%%%%%%%%%%%%%%%%%%% Rebase Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(rebase/Path), cors_handler(Method, rebase_handler(Path)),
                [method(Method),
                 prefix,
                 methods([options,post])]).


rebase_handler(post, Path, Request, System_DB, Auth) :-

    get_payload(Document, Request),
    (   get_dict(rebase_from, Document, Their_Path)
    ->  true
    ;   throw(error(rebase_from_missing))),

    (   get_dict(author, Document, Author)
    ->  true
    ;   throw(error(rebase_author_missing))),

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
            reply_json_dict(Reply)),
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
         setup(((   database_exists("TERMINUSQA", "foo")
                ->  force_delete_db("TERMINUSQA", "foo")
                ;   true),
                (   agent_name_exists(system_descriptor{}, "TERMINUSQA")
                ->  delete_user("TERMINUSQA")
                ;   true),
                (   organization_name_exists(system_descriptor{}, "TERMINUSQA")
                ->  delete_organization("TERMINUSQA")
                ;   true),
                add_user("TERMINUSQA",'user@example.com','a comment', some('password'),User_ID),
                create_db_without_schema("TERMINUSQA", "foo"))),
         cleanup((force_delete_db("TERMINUSQA", "foo"),
                  delete_user_and_organization("TERMINUSQA")))
     ])
:-

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

    config:server(Server),
    atomic_list_concat([Server, '/rebase/TERMINUSQA/foo'], URI),
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
    branch_head_commit(Repository_Descriptor, "master", Commit_Uri),
    commit_uri_to_history_commit_ids(Repository_Descriptor, Commit_Uri, [Commit_A, Commit_B, Commit_C, Commit_D]),

    commit_id_to_metadata(Repository_Descriptor, Commit_A, "test", "commit a", _),
    commit_id_to_metadata(Repository_Descriptor, Commit_B, "test", "commit b", _),
    commit_id_to_metadata(Repository_Descriptor, Commit_C, "test", "commit c", _),
    commit_id_to_metadata(Repository_Descriptor, Commit_D, "Gavsky", "commit d", _).
:- end_tests(rebase_endpoint).

%%%%%%%%%%%%%%%%%%%% Pack Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(pack/Path), cors_handler(Method, pack_handler(Path)),
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
         % blocked('Blocked due to build problems - missing new store?'),
         setup(((   database_exists('_a_test_user_',foo)
                ->  force_delete_db('_a_test_user_',foo)
                ;   true),
                (   agent_name_exists(system_descriptor{}, '_a_test_user_')
                ->  delete_user('_a_test_user_')
                ;   true),
                (   organization_name_exists(system_descriptor{},'_a_test_user_')
                ->  delete_organization('_a_test_user_')
                ;   true),
                add_user('_a_test_user_','user@example.com','a comment', some('password'),_User_ID),
                create_db_without_schema('_a_test_user_',foo)
               )),
         cleanup((force_delete_db('_a_test_user_',foo),
                  delete_user_and_organization('_a_test_user_')))
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
         setup(((   database_exists('_a_test_user_','foo')
                ->  force_delete_db('_a_test_user_','foo')
                ;   true),
                (   agent_name_exists(system_descriptor{}, '_a_test_user_')
                ->  delete_user('_a_test_user_')
                ;   true),
                (   organization_name_exists(system_descriptor{},'_a_test_user_')
                ->  delete_organization('_a_test_user_')
                ;   true),
                add_user('_a_test_user_','user@example.com','a comment', some('password'),_User_ID),
                create_db_without_schema('_a_test_user_','foo')
               )),
         cleanup((force_delete_db('_a_test_user_','foo'),
                  delete_user_and_organization('_a_test_user_')))
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
:- http_handler(root(unpack/Path), cors_handler(Method, unpack_handler(Path)),
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
:- http_handler(root(push/Path), cors_handler(Method, push_handler(Path)),
                [method(Method),
                 prefix,
                 methods([options,post])]).

push_handler(post,Path,Request, System_DB, Auth) :-
    get_payload(Document, Request),

    do_or_die(
        _{ remote : Remote_Name,
           remote_branch : Remote_Branch } :< Document,
        error(bad_api_document(Document,[remote,remote_branch]),_)),

    do_or_die(
        request_remote_authorization(Request, Authorization),
        error(no_remote_authorization)),

    catch_with_backtrace(
        (   push(System_DB, Auth, Path, Remote_Name, Remote_Branch,
                 authorized_push(Authorization),Result),
            (   Result = none
            ->  Response = _{'@type' : "api:PushResponse",
                             'api:status' : "api:success"}
            ;   Result = some(Head_ID)
            ->  Response = _{'@type' : "api:PushNewHeadResponse",
                             'api:head' : Head_ID,
                             'api:status' : "api:success"}
            ;   throw(error(internal_server_error,_))),
            cors_reply_json(Request,
                            Result,
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
                     json_object(dict),
                     status_code(Status_Code)]),
          E,
          throw(error(communication_failure(E),_))),

    (   200 = Status_Code
    ->  true
    ;   400 = Status_Code,
        Result :< _{'@type': Vio_Type}
    ->  (   Vio_Type = "vio:NotALinearHistory"
        ->  throw(error(history_diverged,_))
        ;   Vio_Type = "vio:UnpackDestinationDatabaseNotFound"
        ->  throw(error(remote_unknown,_))
        ;   throw(error(unknown_status_code(Status_Code, Result),_))
        )
    ;   403 = Status_Code
    ->  throw(error(remote_authorization_failure(Result),_))
    ;   throw(error(unknown_status_code,_))
    ).

%%%%%%%%%%%%%%%%%%%% Pull Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(pull/Path), cors_handler(Method, pull_handler(Path)),
                [method(Method),
                 prefix,
                 methods([options,post])]).

pull_handler(post,Path,Request, System_DB, Local_Auth) :-
    get_payload(Document, Request),
    % Can't we just ask for the default remote?
    do_or_die(
        _{ remote : Remote_Name,
           remote_branch : Remote_Branch_Name
         } :< Document,
        error(bad_api_document(Document, [remote, remote_branch]))),

    do_or_die(
        request_remote_authorization(Request, Remote_Auth),
        error(no_remote_authorization)),

    catch_with_backtrace(
        (   pull(System_DB, Local_Auth, Path, Remote_Name, Remote_Branch_Name,
                 authorized_fetch(Remote_Auth),
                 Result),
            cors_reply_json(Request,
                            _{'@type' : 'api:PullResponse',
                              'api:status' : "api:success",
                              'api:pull_report' : Result},
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
                      'api:error' : _{ '@type' : "api:UnresolvableAbsoluteDescriptor",
                                       'api:absolute_descriptor' : Path}
                     },
                    [status(400)]).
pull_error_handler(error(divergent_history(Common_Commit), _), Request) :-
    format(string(Msg), "History diverges from commit ~q", [Common_Commit]),
    cors_reply_json(Request,
                    _{'@type' : "api:PullErrorResponse",
                      'api:status' : "api:failure",
                      'api:message' : Msg,
                      'api:error' : _{ '@type' : 'api:HistoryDivergedError',
                                       'api:common_commit' : Common_Commit
                                     }},
                    [status(400)]).
pull_error_handler(error(no_common_history, _), Request) :-
    format(string(Msg), "There is no common history between branches", []),
    cors_reply_json(Request,
                    _{'@type' : "api:PullErrorResponse",
                      'api:status' : "api:failure",
                      'api:message' : Msg,
                      'api:error' : _{ '@type' : 'api:NoCommonHistoryError'
                                     }},
                    [status(400)]).

%%%%%%%%%%%%%%%%%%%% Branch Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(branch/Path), cors_handler(Method, branch_handler(Path)),
                [method(Method),
                 prefix,
                 methods([options,post])]).

branch_handler(post, Path, Request, System_DB, Auth) :-
    get_payload(Document, Request),

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
         setup((config:server(Server),
                (   database_exists("admin", "test") % very dubious database name
                ->  force_delete_db("admin", "test")
                ;   true),
                create_db_without_schema("admin", "test"))),
         cleanup(force_delete_db("admin", "test"))
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
                (   database_exists("admin", "test") % very dubious database name
                ->  force_delete_db("admin", "test")
                ;   true),
                create_db_without_schema("admin", "test"))),
         cleanup(force_delete_db("admin", "test"))
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
                (   database_exists("admin", "test") %dubious
                ->  force_delete_db("admin", "test")
                ;   true),
                create_db_without_schema("admin", "test"))),
         cleanup(force_delete_db("admin", "test"))
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
                (   database_exists("admin", "test") % dubious
                ->  force_delete_db("admin", "test")
                ;   true),
                create_db_without_schema("admin", "test"))),
         cleanup(force_delete_db("admin", "test"))
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
                (   database_exists("admin", "test") % very dubious database name
                ->  force_delete_db("admin", "test")
                ;   true),
                create_db_without_schema("admin", "test"))),
         cleanup(force_delete_db("admin", "test"))
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
                (   database_exists("admin", "test") % very dubious database name
                ->  force_delete_db("admin", "test")
                ;   true),
                create_db_without_schema("admin", "test"))),
         cleanup(force_delete_db("admin", "test"))
     ])
:-
    config:server(Server),
    atomic_list_concat([Server, '/branch/admin/test/local/branch/foo'], URI),
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

:- http_handler(root(prefixes/Path), cors_handler(Method, prefix_handler(Path)),
                [method(Method),
                 prefix,
                 methods([options,post])]).

prefix_handler(post, _Path, _Request, _System_DB, _Auth) :-
    throw(error(not_implemented)).

%%%%%%%%%%%%%%%%%%%% Create/Delete Graph Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(graph/Path), cors_handler(Method, graph_handler(Path)),
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
    get_payload(Document, Request),

    do_or_die(_{ commit_info : Commit_Info } :< Document,
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
         setup((config:server(Server),
                (   database_exists("admin", "test") % very dubious database name
                ->  force_delete_db("admin", "test")
                ;   true),
                create_db_without_schema("admin", "test"))),
         cleanup(force_delete_db("admin", "test"))
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
                (   database_exists("admin", "test")
                ->  force_delete_db("admin", "test")
                ;   true),
                create_db_without_schema("admin", "test"),
                super_user_authority(Auth),
                create_graph(system_descriptor{},
                             Auth,
                             "admin/test/local/branch/master/schema/main",
                             commit_info{ author : "test",
                                          message: "Generated by automated testing"},
                             _Transaction_Metadata))),
         cleanup(force_delete_db("admin", "test"))
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

%%%%%%%%%%%%%%%%%%%% User handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(root(user), cors_handler(Method, user_handler),
                [method(Method),
                 prefix,
                 methods([options,post,delete])]).
:- http_handler(root(user/Name), cors_handler(Method, user_handler(Name)),
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
:- http_handler(root(organization), cors_handler(Method, organization_handler),
                [method(Method),
                 prefix,
                 methods([options,post,delete])]).
:- http_handler(root(organization/Name), cors_handler(Method, organization_handler(Name)),
                [method(Method),
                 prefix,
                 methods([options,post,delete])]).

organization_handler(post, Request, System_DB, Auth) :-
    get_payload(Document, Request),

    do_or_die(_{ organization_name : Name } :< Document,
              error(malformed_organization_document(Document))
             ),

    catch_with_backtrace(
        (   add_organization_transaction(System_DB, Auth, Name),
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
:- http_handler(root(update_role), cors_handler(Method, update_role_handler),
                [method(Method),
                 prefix,
                 methods([options,post])]).

update_role_handler(post, Request, System_DB, Auth) :-
    get_payload(Document, Request),

    do_or_die(_{ database_name : Database_Name,
                 agent_names : Agents,
                 organization_name : Organization,
                 actions : Actions
               } :< Document,
              error(bad_api_document(Document, [database_name, agent_names, organization_name, actions]), _)),

    catch_with_backtrace(
        (   update_role_transaction(System_DB, Auth, Agents, Organization, Database_Name, Actions),
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
:- http_handler(root(role), cors_handler(Method, role_handler),
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
                         [status(401)]))).

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
        format(Out,'Access-Control-Allow-Methods: GET, POST, DELETE, OPTIONS\n',[]),
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
