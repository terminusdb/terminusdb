:- module(srv_document, [
                  build_swi_request/4,
                  capture_document_output/3,
                  parse_http_response/2
              ]).

:- use_module(core(appserver_hooks)).
:- use_module(server(routes)).
:- use_module(core(transaction)).
:- use_module(core(util)).
:- use_module(core(api)).

:- use_module(library(uri)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_open)).
:- use_module(library(json)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_stream)).
:- use_module(library(base64)).
:- use_module(library(memfile)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(plunit)).
:- use_module(core(util/test_utils)).

:- multifile appserver_hooks:appserver_route/3.
:- multifile appserver_hooks:appserver_stream/3.

appserver_hooks:appserver_route(head, '/api/document/*path', srv_document:document_handler).
appserver_hooks:appserver_route(options, '/api/document/*path', srv_document:document_handler).
appserver_hooks:appserver_route(post, '/api/document/*path', srv_document:document_handler).
appserver_hooks:appserver_route(put, '/api/document/*path', srv_document:document_handler).
appserver_hooks:appserver_route(delete, '/api/document/*path', srv_document:document_handler).

appserver_hooks:appserver_stream(get, '/api/document/*path', srv_document:document_stream_handler).

%% document_handler(+RequestDict, -ResponseDict) is det.
%%
%%  Dispatch an /api/document/<org>/<db> request received by the Rust server
%%  to the existing routes:document_handler/5.  The JSON request produced by the
%%  server dispatcher is converted back into a SWI-Prolog HTTP request list,
%%  authentication is performed, the handler output is captured, and the
%%  resulting HTTP-style text is parsed into a status/body/headers response
%%  dict for the Rust side.
document_handler(Request, Response) :-
    catch(
        document_handler_safe(Request, Response),
        E,
        (   json_log_error_formatted("Server document handler failed: ~q", [E]),
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

document_handler_safe(Request, Response) :-
    build_swi_request(Request, SWIRequest, BodyStream, MemoryFile),
    setup_call_cleanup(
        true,
        document_handler_core(Request, SWIRequest, Response),
        (   catch(close(BodyStream), _, true),
            catch(free_memory_file(MemoryFile), _, true)
        )
    ).

document_handler_core(Request, SWIRequest, Response) :-
    get_dict(method, Request, MethodString),
    string_lower(MethodString, MethodStr),
    atom_string(Method, MethodStr),
    get_dict(params, Request, Params),
    (   get_dict(path, Params, Rest)
    ->  format(atom(DBPath), '/~s', [Rest])
    ;   get_dict(path, Request, FullPath),
        DBPath = FullPath
    ),
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
                capture_document_output(
                    SWIRequest,
                    catch(
                        routes:document_handler(Method, DBPath, SWIRequest, System_DB, Auth),
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

%% document_stream_handler(+RequestDict, +StreamId, -ResponseDict) is det.
%%
%%  Streaming entry point for GET /api/document.  The handler captures the full
%%  SWI-Prolog response produced by routes:document_handler/5, returns the HTTP
%%  status and headers immediately, and then spawns a detached thread that
%%  pushes the response body to the Rust server NDJSON stream.  This makes
%%  the server GET endpoint behave like the main server's chunked NDJSON
%%  document stream without changing the underlying document handler.
document_stream_handler(Request, StreamId, Response) :-
    catch(
        document_stream_handler_safe(Request, StreamId, Response),
        E,
        (   json_log_error_formatted("Server document stream handler failed: ~q", [E]),
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

document_stream_handler_safe(Request, StreamId, Response) :-
    build_swi_request(Request, SWIRequest, BodyStream, MemoryFile),
    setup_call_cleanup(
        true,
        document_stream_handler_core(Request, SWIRequest, StreamId, Response),
        (   catch(close(BodyStream), _, true),
            catch(free_memory_file(MemoryFile), _, true)
        )
    ).

document_stream_handler_core(Request, SWIRequest, StreamId, Response) :-
    get_dict(params, Request, Params),
    (   get_dict(path, Params, Rest)
    ->  format(atom(DBPath), '/~s', [Rest])
    ;   get_dict(path, Request, FullPath),
        DBPath = FullPath
    ),
    open_descriptor(system_descriptor{}, System_DB),
    catch(
        (   routes:authenticate(System_DB, SWIRequest, Auth),
            capture_document_output(
                SWIRequest,
                catch(
                    routes:document_handler(get, DBPath, SWIRequest, System_DB, Auth),
                    E,
                    (   write_cors_headers(SWIRequest),
                        customise_exception(E)
                    )
                ),
                Captured),
            parse_http_response(Captured, Response0),
            Response0 = _{status: Status, body: Body, headers: Headers},
            (   string(Body),
                ndjson_body(Body)
            ->  Response = _{status: Status, body: stream, headers: Headers},
                thread_create(
                    stream_ndjson_body(Body, StreamId),
                    _,
                    [detached(true)]
                )
            ;   Response = Response0
            )
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
    ).

ndjson_body(Body) :-
    split_string(Body, "\n", "", Lines),
    subtract(Lines, [""], NonEmpty),
    length(NonEmpty, Len),
    Len > 1.

%% stream_ndjson_body(+Body, +StreamId) is det.
%%
%%  Send the already-formatted response body to the Rust server stream as
%%  individual NDJSON lines.  Empty lines are skipped.  The stream is always
%%  closed, even if a send fails.
stream_ndjson_body(Body, StreamId) :-
    catch(
        (   open_string(Body, Stream),
            stream_ndjson_lines(Stream, StreamId),
            close(Stream)
        ),
        Error,
        (   json_log_error_formatted("Document stream send failed: ~q", [Error]),
            '$appserver':appserver_stream_close(StreamId)
        )
    ).

stream_ndjson_lines(Stream, StreamId) :-
    read_line_to_string(Stream, Line),
    (   Line == end_of_file
    ->  '$appserver':appserver_stream_close(StreamId)
    ;   (   Line \= ""
        ->  '$appserver':appserver_stream_send(StreamId, Line)
        ;   true
        ),
        stream_ndjson_lines(Stream, StreamId)
    ).

%% capture_document_output(+Request, :Goal, -Text) is det.
%%
%%  Run Goal with current_output redirected to a fresh CGI stream backed by a
%%  UTF-8 memory file.  The document handler expects a CGI stream, so we keep
%%  cgi_open.  The response is read back as raw UTF-8 bytes and decoded to a
%%  string so Unicode characters are preserved.
capture_document_output(Request, Goal, Text) :-
    new_memory_file(MemFile),
    setup_call_cleanup(
        open_memory_file(MemFile, write, OutStream, [encoding(utf8)]),
        setup_call_cleanup(
            cgi_open(OutStream, CGI, srv_document:cgi_capture_hook, [request(Request)]),
            (   set_stream(CGI, encoding(utf8)),
                with_output_to(
                    CGI,
                    call(Goal)
                )
            ),
            close(CGI)
        ),
        close(OutStream)
    ),
    setup_call_cleanup(
        open_memory_file(MemFile, read, ReadStream, [type(binary), encoding(octet)]),
        read_string(ReadStream, _, ByteString),
        close(ReadStream)
    ),
    utf8_bytes_to_string(ByteString, Text).

%% cgi_capture_hook(+Event, +CGI) is det.
%%
%%  Minimal CGI hook for the capture memory file.  The header text collected
%%  by the CGI stream is written to the underlying memory file followed by a
%%  blank line; the body is written by the CGI stream itself.
cgi_capture_hook(send_header, CGI) :-
    cgi_property(CGI, header_codes(HeadText)),
    cgi_property(CGI, client(Out)),
    format(Out, '~s', [HeadText]),
    !.
cgi_capture_hook(_, _).

%% build_swi_request(+RequestDict, -SWIRequest, -BodyStream, -MemoryFile) is det.
%%
%%  Convert the JSON request dict produced by the Rust dispatcher into a
%%  SWI-Prolog HTTP request list.  Only the fields that the document handler
%%  actually uses are populated.  The body is stored in a binary memory file so
%%  the HTTP library can read the exact UTF-8 byte sequence.
build_swi_request(Request, SWIRequest, BodyStream, MemoryFile) :-
    get_dict(method, Request, MethodString),
    string_lower(MethodString, MethodStr),
    atom_string(Method, MethodStr),
    get_dict(path, Request, PathString),
    atom_string(Path, PathString),
    (   get_dict(query, Request, QueryString),
        QueryString \= ""
    ->  uri_query_components(QueryString, Query)
    ;   Query = []
    ),
    get_dict(headers, Request, HeadersDict),
    build_swi_headers(HeadersDict, HeaderTerms),
    get_dict(body, Request, Body),
    string_to_utf8_bytes(Body, BodyByteString, BodyLen),
    new_memory_file(MemoryFile),
    open_memory_file(MemoryFile, write, WriteStream, [type(binary), encoding(octet)]),
    format(WriteStream, "~s", [BodyByteString]),
    close(WriteStream),
    open_memory_file(MemoryFile, read, BodyStream, [type(binary), encoding(octet)]),
    SWIRequest = [
        method(Method),
        path(Path),
        request_uri(Path),
        search(Query),
        content_length(BodyLen),
        input(BodyStream)
        | HeaderTerms
    ].

string_to_utf8_bytes(String, ByteString, Length) :-
    new_memory_file(MF),
    open_memory_file(MF, write, W, [encoding(utf8)]),
    format(W, '~s', [String]),
    close(W),
    open_memory_file(MF, read, R, [type(binary), encoding(octet)]),
    read_string(R, _, ByteString),
    close(R),
    free_memory_file(MF),
    string_length(ByteString, Length).

utf8_byte_length(String, Length) :-
    string_to_utf8_bytes(String, _, Length).

utf8_bytes_to_string(ByteString, String) :-
    string_codes(ByteString, Bytes),
    new_memory_file(MF),
    open_memory_file(MF, write, W, [type(binary), encoding(octet)]),
    write_bytes(W, Bytes),
    close(W),
    open_memory_file(MF, read, R, [encoding(utf8)]),
    read_string(R, _, String),
    close(R),
    free_memory_file(MF).

write_bytes(_, []).
write_bytes(Stream, [Byte | Rest]) :-
    put_byte(Stream, Byte),
    write_bytes(Stream, Rest).

build_swi_headers(HeadersDict, HeaderTerms) :-
    dict_pairs(HeadersDict, _, Pairs),
    maplist(build_swi_header, Pairs, HeaderTerms).

build_swi_header(Key-Value, Term) :-
    atom_string(KeyAtom, Key),
    downcase_atom(KeyAtom, KeyLower),
    atom_string(ValueAtom, Value),
    (   KeyLower == authorization
    ->  Term = authorization(ValueAtom)
    ;   KeyLower == 'content-type'
    ->  Term = content_type(ValueAtom)
    ;   KeyLower == origin
    ->  Term = origin(ValueAtom)
    ;   KeyLower == 'x-http-method-override'
    ->  Term = x_http_method_override(ValueAtom)
    ;   KeyLower == 'x-terminusdb-api-base'
    ->  Term = x_terminusdb_api_base(ValueAtom)
    ;   KeyLower == 'x-terminusdb-api-user'
    ->  Term = x_terminusdb_api_user(ValueAtom)
    ;   Term = header(KeyAtom, ValueAtom)
    ).

%% parse_http_response(+Text, -ResponseDict) is det.
%%
%%  Parse the SWI-Prolog HTTP response text produced by the document handler
%%  into a response dict with status, body, and headers keys.
parse_http_response(Text, Response) :-
    open_string(Text, Stream),
    read_header_lines(Stream, 200, Status, HeaderPairs),
    read_string(Stream, _, Body0),
    (   Body0 == end_of_file
    ->  Body = ""
    ;   Body = Body0
    ),
    strip_hop_headers(HeaderPairs, CleanPairs),
    dict_create(Headers, _, CleanPairs),
    Response = _{
        status: Status,
        body: Body,
        headers: Headers
    }.

read_header_lines(Stream, DefaultStatus, Status, Pairs) :-
    read_line_to_string(Stream, Line),
    (   Line == ""
    ->  Status = DefaultStatus,
        Pairs = []
    ;   Line == end_of_file
    ->  Status = DefaultStatus,
        Pairs = []
    ;   re_matchsub('^HTTP/1\\.1\\s+(?<code>\\d+)', Line, Dict, [])
    ->  atom_string(CodeAtom, Dict.code),
        atom_number(CodeAtom, NewStatus),
        read_header_lines(Stream, NewStatus, Status, Pairs)
    ;   re_matchsub('^Status:\\s*(?<code>\\d+)', Line, Dict, [])
    ->  atom_string(CodeAtom, Dict.code),
        atom_number(CodeAtom, NewStatus),
        read_header_lines(Stream, NewStatus, Status, Pairs)
    ;   parse_header_line(Line, Name, Value),
        read_header_lines(Stream, DefaultStatus, Status, Rest),
        Pairs = [Name-Value | Rest]
    ).

parse_header_line(Line, Name, Value) :-
    sub_string(Line, Before, 1, After, ':'),
    !,
    sub_string(Line, 0, Before, _, NameString),
    sub_string(Line, _, After, 0, ValueString),
    normalize_space(atom(Name), NameString),
    normalize_space(string(Value), ValueString).

strip_hop_headers([], []).
strip_hop_headers([Name-_Value | Rest], Clean) :-
    downcase_atom(Name, Lower),
    member(Lower, ['transfer-encoding', 'connection', 'upgrade', 'content-length']),
    !,
    strip_hop_headers(Rest, Clean).
strip_hop_headers([Pair | Rest], [Pair | Clean]) :-
    strip_hop_headers(Rest, Clean).


:- begin_tests(srv_document, [concurrent(false)]).

test_port(6359).

rust_url(Path, URL) :-
    test_port(Port),
    format(atom(Base), 'http://127.0.0.1:~d', [Port]),
    atomic_concat(Base, Path, URL).

main_url(BaseURL, Path, URL) :-
    atomic_concat(BaseURL, Path, URL).

setup_test_server(State-URL) :-
    test_port(Port),
    format(atom(PortAtom), '~d', [Port]),
    setup_temp_server(State, URL,
                      [env_vars(['TERMINUSDB_WEBSERVER_PORT'=PortAtom])]).

auth_header(Header) :-
    format(atom(UserPass), 'admin:root', []),
    base64(UserPass, Base64),
    format(atom(Header), 'Basic ~s', [Base64]).

test_document_schema('{"@type": "@context", "@schema": "http://s/", "@base": "http://i/", "xsd": "http://www.w3.org/2001/XMLSchema#"} {"@id": "Person", "@type": "Class", "name": "xsd:string"}').

test(srv_document_insert_and_get, [
         setup(setup_test_server(State-_)),
         cleanup(teardown_temp_server(State))
     ]) :-
    create_db_without_schema("admin", "testdb"),
    rust_url('/api/document/admin/testdb', URL),
    auth_header(AuthHeader),

    test_document_schema(Schema),
    format(atom(SchemaURL), '~s?graph_type=schema&author=admin&message=schema&full_replace=true', [URL]),
    http_open(SchemaURL, PostStream,
              [method(post),
               post(string('application/json', Schema)),
               request_header('Authorization'=AuthHeader),
               status_code(SchemaStatus),
               timeout(5)]),
    read_string(PostStream, _, SchemaReply),
    close(PostStream),
    SchemaStatus = 200,
    atom_json_dict(SchemaReply, SchemaIds, [default_tag(json)]),
    is_list(SchemaIds),

    Instance = '{"@type": "Person", "name": "Alice"}',
    format(atom(InstanceURL), '~s?author=admin&message=instance', [URL]),
    http_open(InstanceURL, PostStream2,
              [method(post),
               post(string('application/json', Instance)),
               request_header('Authorization'=AuthHeader),
               status_code(InstanceStatus),
               timeout(5)]),
    read_string(PostStream2, _, InstanceReply),
    close(PostStream2),
    InstanceStatus = 200,
    atom_json_dict(InstanceReply, InstanceIds, [default_tag(json)]),
    is_list(InstanceIds),

    format(atom(GetURL), '~s?as_list=true&graph_type=instance', [URL]),
    http_open(GetURL, GetStream,
              [method(get),
               request_header('Authorization'=AuthHeader),
               status_code(GetStatus),
               timeout(5)]),
    read_string(GetStream, _, GetReply),
    close(GetStream),
    GetStatus = 200,
    atom_json_dict(GetReply, Docs, [default_tag(json)]),
    is_list(Docs),
    Docs = [Doc],
    _{'@type': "Person", name: "Alice"} :< Doc.

test(srv_document_main_server_parity, [
         setup(setup_test_server(State-BaseURL)),
         cleanup(teardown_temp_server(State))
     ]) :-
    create_db_without_schema("admin", "paritydb"),
    auth_header(AuthHeader),
    test_document_schema(Schema),

    main_url(BaseURL, '/api/document/admin/paritydb?graph_type=schema&author=admin&message=schema&full_replace=true', SchemaURL),
    http_open(SchemaURL, SchemaStream,
              [method(post),
               post(string('application/json', Schema)),
               request_header('Authorization'=AuthHeader),
               status_code(SchemaStatus),
               timeout(5)]),
    read_string(SchemaStream, _, _),
    close(SchemaStream),
    SchemaStatus = 200,

    Instance = '{"@type": "Person", "name": "Bob"}',
    main_url(BaseURL, '/api/document/admin/paritydb?author=admin&message=instance', InstanceURL),
    http_open(InstanceURL, InstanceStream,
              [method(post),
               post(string('application/json', Instance)),
               request_header('Authorization'=AuthHeader),
               status_code(InstanceStatus),
               timeout(5)]),
    read_string(InstanceStream, _, _),
    close(InstanceStream),
    InstanceStatus = 200,

    rust_url('/api/document/admin/paritydb?as_list=true&graph_type=instance', SrvGetURL),
    http_open(SrvGetURL, GetStream,
              [method(get),
               request_header('Authorization'=AuthHeader),
               status_code(GetStatus),
               timeout(5)]),
    read_string(GetStream, _, GetReply),
    close(GetStream),
    GetStatus = 200,
    atom_json_dict(GetReply, Docs, [default_tag(json)]),
    is_list(Docs),
    Docs = [Doc],
    _{'@type': "Person", name: "Bob"} :< Doc.

:- end_tests(srv_document).
