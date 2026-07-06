:- module(request_worker_pool, [
                  init_request_worker_pool/1,
                  dispatch_request/5
              ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/json)).
:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(strings)).
:- use_module(library(memfile)).
:- use_module(core(appserver_hooks)).
:- use_module(server(routes/srv_http)).
:- use_module(server(routes/tdb_http_handler)).

%% Worker pool state
%%
%% The pool uses a message queue per worker. Requests are round-robin
%% distributed. Each worker thread runs worker_loop/0, which blocks on
%% its message queue waiting for work.

:- dynamic worker_queue/1.

%% init_request_worker_pool(+Workers) is det.
%%
%%  Spawn Workers SWI-Prolog threads, each with its own message queue.
%%  Must be called once at server startup from the main engine thread.
init_request_worker_pool(Workers) :-
    (   between(1, Workers, I),
        message_queue_create(Queue),
        assertz(worker_queue(Queue)),
        atom_concat(worker_, I, Alias),
        thread_create(worker_loop(Queue), _, [detached(true), alias(Alias)]),
        fail
    ;   true
    ).

%% dispatch_request(+RequestDict, +HandlerModule, +HandlerName,
%%                  +InputStreamId, +ResponseStreamId) is det.
%%
%%  Enqueue a request to the worker pool. The request is sent to the
%%  next available worker queue in round-robin order. The worker will:
%%    1. Drain the input stream into a memory file (octet encoding)
%%    2. Build a SWI request from the request dict + body memory file
%%    3. Call http_dispatch with the pre-built SWI request
%%    4. Write the response to the output stream and close it
dispatch_request(Request, HandlerModule, HandlerName, InputStreamId, ResponseStreamId) :-
    (   retract(worker_queue(Q))
    ->  assertz(worker_queue(Q))
    ;   json_log_error_formatted("dispatch_request: no worker queue available", []),
        throw(error(no_worker_available, _))
    ),
    thread_send_message(Q, work(Request, HandlerModule, HandlerName, InputStreamId, ResponseStreamId)).

%% worker_loop(+Queue) is det.
%%
%%  Main loop for a worker thread. Blocks waiting for work messages.
worker_loop(Queue) :-
    thread_get_message(Queue, Message),
    (   Message = work(Request, HandlerModule, HandlerName, InputStreamId, ResponseStreamId)
    ->  catch(
            (   handle_work(Request, HandlerModule, HandlerName, InputStreamId, ResponseStreamId)
            ->  true
            ;   json_log_error_formatted("Worker goal failed for ~w ~w", [HandlerModule, HandlerName]),
                catch(send_error_response(ResponseStreamId, error(goal_failed, _)), _, true)
            ),
            Error,
            (   json_log_error_formatted("Worker error: ~q", [Error]),
                catch(send_error_response(ResponseStreamId, Error), _, true)
            )
        ),
        worker_loop(Queue)
    ;   worker_loop(Queue)
    ).

%% handle_work(+Request, +HandlerModule, +HandlerName,
%%             +InputStreamId, +ResponseStreamId) is det.
%%
%%  Process a single request. Drains the input stream into a memory file
%%  (single copy, octet encoding), builds the SWI request directly, then
%%  dispatches via http_dispatch_with_expansion. The response is captured
%%  and sent to the output stream.
%%
%%  For stream handlers (HandlerName == stream_handler), the ResponseStreamId
%%  is made available so that ndjson streaming can use it.
handle_work(Request, _HandlerModule, HandlerName, InputStreamId, ResponseStreamId) :-
    drain_input_stream(InputStreamId, MemoryFile, BodyStream, BodyLen),
    setup_call_cleanup(
        true,
        (   build_swi_request_from_dict(Request, BodyStream, BodyLen, SWIRequest),
            (   HandlerName == stream_handler
            ->  handle_stream_request(Request, SWIRequest, ResponseStreamId, Response)
            ;   handle_plugin_request(SWIRequest, Response)
            ),
            send_response(ResponseStreamId, Response),
            (   get_dict(body, Response, stream),
                get_dict('_ndjson_body', Response, Body)
            ->  thread_create(
                    tdb_http_handler:stream_ndjson_body(Body, ResponseStreamId),
                    _,
                    [detached(true)]
                )
            ;   true
            )
        ),
        (   catch(close(BodyStream), _, true),
            catch(free_memory_file(MemoryFile), _, true)
        )
    ).

%% drain_input_stream(+InputStreamId, -MemoryFile, -BodyStream, -BodyLen) is det.
%%
%%  Read all chunks from the input stream and write them directly into a
%%  memory file with octet encoding. This is the single copy of the body
%%  data — no intermediate Prolog string is used. The memory file is
%%  reopened for reading so the HTTP handler can consume it.
drain_input_stream(InputStreamId, MemoryFile, BodyStream, BodyLen) :-
    new_memory_file(MemoryFile),
    open_memory_file(MemoryFile, write, WriteStream, [type(binary), encoding(octet)]),
    drain_chunks(InputStreamId, WriteStream, 0, BodyLen),
    close(WriteStream),
    open_memory_file(MemoryFile, read, BodyStream, [type(binary), encoding(octet)]).

drain_chunks(InputStreamId, WriteStream, AccLen, TotalLen) :-
    '$appserver':appserver_stream_recv(InputStreamId, Chunk),
    (   Chunk == end_of_stream
    ->  TotalLen = AccLen
    ;   format(WriteStream, '~s', [Chunk]),
        string_length(Chunk, ChunkLen),
        NewLen is AccLen + ChunkLen,
        drain_chunks(InputStreamId, WriteStream, NewLen, TotalLen)
    ).

%% build_swi_request_from_dict(+RequestDict, +BodyStream, +BodyLen, -SWIRequest) is det.
%%
%%  Build a SWI-Prolog HTTP request list directly from the request dict
%%  and the pre-filled body memory file stream. This avoids the double
%%  copy that would occur if we used build_swi_request/4 (which expects
%%  a body string and converts it to a memory file).
build_swi_request_from_dict(Request, BodyStream, BodyLen, SWIRequest) :-
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
    SWIRequest = [
        method(Method),
        path(Path),
        request_uri(Path),
        search(Query),
        content_length(BodyLen),
        input(BodyStream),
        peer(ip(127,0,0,1)),
        http_version(1-1)
        | HeaderTerms
    ].

%% handle_plugin_request(+SWIRequest, -Response) is det.
%%
%%  Dispatch the SWI request through the normal HTTP pipeline, capture
%%  the CGI output, and parse it into a response dict.
handle_plugin_request(SWIRequest, Response) :-
    catch(
        (   capture_http_output(SWIRequest,
                               tdb_http_handler:http_dispatch_with_expansion(SWIRequest),
                               Captured),
            parse_http_response(Captured, Response)
        ->  true
        ;   json_log_error_formatted("Plugin handler goal failed", []),
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
        ),
        Error,
        (   json_log_error_formatted("Plugin handler failed: ~q", [Error]),
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

%% handle_stream_request(+RequestDict, +SWIRequest, +ResponseStreamId, -Response) is det.
%%
%%  Dispatch the SWI request through the HTTP pipeline, capture the output,
%%  and if the body is NDJSON, return body: stream and spawn a detached
%%  thread to push lines to the Rust stream.
handle_stream_request(_Request, SWIRequest, _ResponseStreamId, Response) :-
    catch(
        (   capture_http_output(SWIRequest,
                               tdb_http_handler:http_dispatch_with_expansion(SWIRequest),
                               Captured),
            parse_http_response(Captured, Response0),
            Response0 = _{status: Status, body: Body, headers: Headers},
            (   string(Body),
                tdb_http_handler:ndjson_body(Body)
            ->  Response = _{status: Status, body: stream, headers: Headers, '_ndjson_body': Body}
            ;   Response = Response0
            )
        ->  true
        ;   json_log_error_formatted("Stream handler goal failed", []),
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
        ),
        Error,
        (   json_log_error_formatted("Stream handler failed: ~q", [Error]),
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

%% send_response(+ResponseStreamId, +Response) is det.
%%
%%  Serialize the response dict as JSON and send it to the output stream.
%%  When the body is 'stream', the stream is left open for the spawned
%%  NDJSON thread to write to and close.
%%
%%  For binary content types (e.g. application/octets), the body is sent
%%  as a separate raw byte message after the JSON metadata, using
%%  appserver_stream_send_raw/2 which preserves 8-bit clean data.
send_response(ResponseStreamId, Response) :-
    (   get_dict('_ndjson_body', Response, _)
    ->  select_dict(_{'_ndjson_body':_}, Response, ResponseClean)
    ;   ResponseClean = Response
    ),
    (   is_binary_response(ResponseClean)
    ->  get_dict(body, ResponseClean, Body),
        select_dict(_{body:Body}, ResponseClean, ResponseMeta),
        with_output_to(string(JsonString), json_write_dict(current_output, ResponseMeta, [as(string)])),
        '$appserver':appserver_stream_send(ResponseStreamId, JsonString),
        '$appserver':appserver_stream_send_raw(ResponseStreamId, Body),
        '$appserver':appserver_stream_close(ResponseStreamId)
    ;   with_output_to(string(JsonString), json_write_dict(current_output, ResponseClean, [as(string)])),
        '$appserver':appserver_stream_send(ResponseStreamId, JsonString),
        (   get_dict(body, ResponseClean, stream)
        ->  true
        ;   '$appserver':appserver_stream_close(ResponseStreamId)
        )
    ).

%% is_binary_response(+Response) is semidet.
%%
%%  True when the response has a binary content type that should be sent
%%  as raw bytes rather than embedded in the JSON metadata.
is_binary_response(Response) :-
    get_dict(headers, Response, Headers),
    (   get_dict('Content-Type', Headers, ContentType)
    ;   get_dict('Content-type', Headers, ContentType)
    ;   get_dict('content-type', Headers, ContentType)
    ;   get_dict('content_type', Headers, ContentType)
    ),
    downcase_atom(ContentType, LowerContentType),
    (   sub_atom(LowerContentType, _, _, _, 'application/octets')
    ;   sub_atom(LowerContentType, _, _, _, 'application/octet-stream')
    ;   sub_atom(LowerContentType, _, _, _, 'application/x-gzip')
    ),
    get_dict(body, Response, Body),
    string(Body).

%% send_error_response(+ResponseStreamId, +Error) is det.
%%
%%  Send an error response to the output stream and close it.
send_error_response(ResponseStreamId, Error) :-
    (   term_string(Error, ErrorString),
        Response = _{
            status: 500,
            body: _{
                '@type': 'api:ErrorResponse',
                'api:status': 'api:failure',
                'api:error': _{'@type': 'api:InternalServerError'},
                'api:message': ErrorString
            },
            headers: _{'Content-Type': 'application/json'}
        },
        with_output_to(string(JsonString), json_write_dict(current_output, Response, [as(string)])),
        '$appserver':appserver_stream_send(ResponseStreamId, JsonString),
        '$appserver':appserver_stream_close(ResponseStreamId)
    ->  true
    ;   '$appserver':appserver_stream_close(ResponseStreamId)
    ).
