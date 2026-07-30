:- module(plugin_api_http, [
    register_route/3,
    cors_handler/3,
    cors_handler/4,
    authenticate/3,
    authenticate_from_request/3,
    plugin_error_response/2,
    write_cors_headers/1,
    api_report_errors/3,
    resolve_descriptor_auth/6
]).

:- use_module(server(routes)).
:- use_module(server(routes/tdb_http_handler)).
:- use_module(core(account/capabilities), [resolve_descriptor_auth/6]).

:- reexport(server(routes), [cors_handler/3, cors_handler/4, authenticate/3,
                             write_cors_headers/1, api_report_errors/3]).
:- reexport(core(account/capabilities), [resolve_descriptor_auth/6]).

%% register_route(+Path, +Handler, +Options) is det.
%
%  Drop-in wrapper around tdb_http_handler/3 that registers a route
%  with both the SWI-Prolog HTTP dispatcher and the Rust webserver.
%  Plugins call this instead of reaching into server(routes) directly.
%
%  Handler is the same closure that http_handler/3 accepts, typically
%  cors_handler(Method, Goal, ExtraOptions).
register_route(Path, Handler, Options) :-
    tdb_http_handler:tdb_http_handler(Path, Handler, Options).

%% authenticate_from_request(+Request, +System_DB, -Auth) is det.
%
%  Extract the Authorization header from the appserver request dict and
%  authenticate the user via routes:authenticate/3. Throws on auth failure.
%
%  Request is a dict with a 'headers' key containing the HTTP headers.
%  Both 'Authorization' and 'authorization' header names are accepted.
authenticate_from_request(Request, System_DB, Auth) :-
    get_dict(headers, Request, HeadersDict),
    (   get_dict('Authorization', HeadersDict, AuthValue)
    ->  true
    ;   get_dict('authorization', HeadersDict, AuthValue)
    ->  true
    ;   throw(error(authentication_incorrect(no_authorization_header), _))
    ),
    atom_string(AuthAtom, AuthValue),
    SWIRequest = [authorization(AuthAtom), peer(ip(127,0,0,1))],
    routes:authenticate(System_DB, SWIRequest, Auth).

%% plugin_error_response(+Error, -Response) is det.
%
%  Map common Prolog error terms to HTTP response dicts. Used by the
%  worker pool's stream handler dispatch so plugins can simply throw
%  errors without implementing their own catch/response formatting.
plugin_error_response(error(authentication_incorrect(_), _), Response) :- !,
    Response = _{
        status: 401,
        body: _{
            '@type': 'api:ErrorResponse',
            'api:status': 'api:authentication_failed',
            'api:message': 'Authentication required'
        },
        headers: _{'Content-Type': 'application/json'}
    }.
plugin_error_response(error(access_not_authorised(_, _Action, _), _), Response) :- !,
    Response = _{
        status: 403,
        body: _{
            '@type': 'api:ErrorResponse',
            'api:status': 'api:forbidden',
            'api:message': 'Access denied'
        },
        headers: _{'Content-Type': 'application/json'}
    }.
plugin_error_response(error(not_a_branch_descriptor(_), _), Response) :- !,
    Response = _{
        status: 400,
        body: _{
            '@type': 'api:ErrorResponse',
            'api:status': 'api:bad_request',
            'api:message': 'Not a branch descriptor'
        },
        headers: _{'Content-Type': 'application/json'}
    }.
plugin_error_response(error(invalid_absolute_path(_), _), Response) :- !,
    Response = _{
        status: 404,
        body: _{
            '@type': 'api:ErrorResponse',
            'api:status': 'api:not_found',
            'api:message': 'Path not found'
        },
        headers: _{'Content-Type': 'application/json'}
    }.
plugin_error_response(_, Response) :-
    Response = _{
        status: 500,
        body: _{
            '@type': 'api:ErrorResponse',
            'api:status': 'api:server_error',
            'api:message': 'An internal server error occurred.'
        },
        headers: _{'Content-Type': 'application/json'}
    }.
