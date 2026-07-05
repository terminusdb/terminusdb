:- module(srv_db, []).

:- use_module(core(appserver_hooks)).
:- use_module(server(routes)).
:- use_module(core(transaction)).
:- use_module(core(util)).
:- use_module(server(routes/srv_document), [
                  build_swi_request/4,
                  capture_document_output/3,
                  parse_http_response/2
              ]).

:- use_module(library(uri)).
:- use_module(library(http/http_json)).
:- use_module(library(lists)).
:- use_module(library(yall)).
:- use_module(library(plunit)).

:- multifile appserver_hooks:appserver_route/3.

appserver_hooks:appserver_route(head, '/api/db/*path', srv_db:db_handler).
appserver_hooks:appserver_route(options, '/api/db/*path', srv_db:db_handler).
appserver_hooks:appserver_route(get, '/api/db/*path', srv_db:db_handler).
appserver_hooks:appserver_route(post, '/api/db/*path', srv_db:db_handler).
appserver_hooks:appserver_route(put, '/api/db/*path', srv_db:db_handler).
appserver_hooks:appserver_route(delete, '/api/db/*path', srv_db:db_handler).

appserver_hooks:appserver_route(head, '/api/db', srv_db:db_handler).
appserver_hooks:appserver_route(options, '/api/db', srv_db:db_handler).
appserver_hooks:appserver_route(get, '/api/db', srv_db:db_handler).

%% db_handler(+RequestDict, -ResponseDict) is det.
%%
%%  Dispatch /api/db requests on the Rust server to routes:db_handler/5 and
%%  routes:db_handler/6.  The request dict is converted into a SWI-Prolog HTTP
%%  request list, authentication is performed, the handler output is captured
%%  with a CGI stream, and the resulting HTTP response is parsed back into a
%%  dict for the Rust side.
db_handler(Request, Response) :-
    catch(
        db_handler_safe(Request, Response),
        E,
        (   json_log_error_formatted("Server db handler failed: ~q", [E]),
            Response = _{
                status: 500,
                body: _{
                    '@type': 'api:ErrorResponse',
                    'api:status': 'api:failure',
                    'api:error': _{'@type': 'api:InternalServerError'},
                    'api:message': 'Internal server error'
                },
                headers: _{'Content-Type': 'application/json'}
            }
        )
    ).

db_handler_safe(Request, Response) :-
    build_swi_request(Request, SWIRequest, BodyStream, MemoryFile),
    setup_call_cleanup(
        true,
        db_handler_core(Request, SWIRequest, Response),
        (   catch(close(BodyStream), _, true),
            catch(free_memory_file(MemoryFile), _, true)
        )
    ).

db_handler_core(Request, SWIRequest, Response) :-
    get_dict(method, Request, MethodString),
    string_lower(MethodString, MethodStr),
    atom_string(Method, MethodStr),
    get_dict(path, Request, Path),
    (   Method == options
    ->  capture_document_output(
            SWIRequest,
            (   write_cors_headers(SWIRequest),
                format('Status: 200 OK~n~n')
            ),
            Captured),
        parse_http_response(Captured, Response)
    ;   open_descriptor(system_descriptor{}, System_DB),
        catch(
            (   routes:authenticate(System_DB, SWIRequest, Auth),
                db_path_args(Path, Method, SWIRequest, System_DB, Auth, Goal),
                capture_document_output(
                    SWIRequest,
                    catch(
                        Goal,
                        E,
                        (   write_cors_headers(SWIRequest),
                            customise_exception(E)
                        )
                    ),
                    Captured),
                parse_http_response(Captured, Response)
            ),
            error(authentication_incorrect(_), _),
            (   capture_document_output(
                    SWIRequest,
                    (   write_cors_headers(SWIRequest),
                        reply_json(
                            _{
                                '@type': 'api:ErrorResponse',
                                'api:status': 'api:failure',
                                'api:error': _{'@type': 'api:IncorrectAuthenticationError'},
                                'api:message': 'Incorrect authentication information'
                            },
                            [width(0), status(401)]
                        )
                    ),
                    Captured),
                parse_http_response(Captured, Response)
            )
        )
    ).

%% db_path_args(+Path, +Method, +Request, +System_DB, +Auth, -Goal) is det.
%%
%%  Map the request path /api/db[/org[/db]] to the appropriate db_handler call.
db_path_args(Path, Method, Request, System_DB, Auth, Goal) :-
    atom_string(Path, PathString),
    split_string(PathString, "/", "", Segments),
    (   Segments = ["", "api", "db"]
    ->  !,
        Goal = routes:db_handler(Method, _, Request, System_DB, Auth)
    ;   Segments = ["", "api", "db", OrgStr]
    ->  !,
        atom_string(Org, OrgStr),
        Goal = routes:db_handler(Method, Org, '', Request, System_DB, Auth)
    ;   Segments = ["", "api", "db", OrgStr, DBStr]
    ->  !,
        atom_string(Org, OrgStr),
        atom_string(DB, DBStr),
        Goal = routes:db_handler(Method, Org, DB, Request, System_DB, Auth)
    ;   !,
        Goal = routes:db_handler(Method, _, Request, System_DB, Auth)
    ).
