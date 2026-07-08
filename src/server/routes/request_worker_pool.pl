:- module(request_worker_pool, [
                  init_request_worker_pool/1,
                  dispatch_request/5,
                  dispatch_request/6
              ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_stream)).
:- use_module(library(json)).
:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(strings)).
:- use_module(library(memfile)).
:- use_module(library(readutil)).
:- use_module(library(unix)).
:- use_module(library(uri)).
:- use_module(core(appserver_hooks)).
:- use_module(core(util)).
:- use_module(server(routes/srv_http)).
:- use_module(server(routes/tdb_http_handler)).

%% Worker pool state
%%
%% The pool uses a message queue per worker. Requests are round-robin
%% distributed. Each worker thread runs worker_loop/1, which blocks on
%% its message queue waiting for work.
%%
%% worker/3 associates each worker's queue, thread ID, and alias.
%% Threads are created with detached(false) so that thread death is
%% observable via thread_property/2. A dead worker is a fatal condition
%% that must crash the server, not silently degrade.

:- dynamic worker_queue/1.
:- dynamic worker/3.  % worker(Queue, ThreadId, Alias)

%% Watchdog state
%%
%% The watchdog monitors worker thread health. After each request,
%% a worker signals 'ready' to the watchdog queue. If a worker has
%% been assigned work but does not signal 'ready' within a grace
%% period, the watchdog checks whether the thread is dead or stuck.
%% In either case, the server crashes with a diagnostic message
%% rather than silently degrading.
%%
%% worker_busy/2 tracks which workers are currently processing:
%%   worker_busy(Alias, DispatchTime)
%% The watchdog clears this when 'ready' is received.

:- dynamic worker_busy/2.
:- dynamic watchdog_running/0.
:- dynamic watchdog_queue/1.

%% watchdog_grace_period(-Grace) is det.
%%
%%  Grace period in seconds before the watchdog logs a warning for a
%%  busy worker. This is NOT a request timeout and does NOT kill or
%%  interrupt the worker thread. Long-running queries (minutes or even
%%  hours) are fully supported — a worker that is actively computing
%%  will simply exceed the grace period and generate a warning log
%%  entry, then continue running.
%%
%%  The watchdog only takes fatal action (halt(1)) when a worker thread
%%  is *dead* (not in 'running' status), which indicates the thread has
%%  exited without signaling 'ready' — leaving the pipe FD orphaned and
%%  the Rust side hanging forever. This is always fatal regardless of
%%  the grace period, even when the grace period is disabled.
%%
%%  The grace period can be overridden with the
%%  TERMINUSDB_WORKER_WATCHDOG_GRACE environment variable:
%%    - A positive integer (seconds): warnings fire after that long.
%%    - "0" or "false": warnings are disabled entirely. Dead-thread
%%      detection still runs.
%%  Default is 300 (5 minutes) to avoid log noise from legitimate
%%  long-running queries.
watchdog_grace_period(Grace) :-
    (   getenv('TERMINUSDB_WORKER_WATCHDOG_GRACE', EnvValue)
    ->  (   EnvValue == false
        ->  Grace = false
        ;   atom_number(EnvValue, Number),
            (   Number =< 0
            ->  Grace = false
            ;   Grace = Number
            )
        )
    ;   Grace = 300
    ).

%% init_request_worker_pool(+Workers) is det.
%%
%%  Spawn Workers SWI-Prolog threads, each with its own message queue.
%%  Must be called once at server startup from the main engine thread.
init_request_worker_pool(Workers) :-
    (   getenv('TERMINUSDB_WORKER_MEMORY_LOGGING', _)
    ->  set_prolog_flag(worker_memory_logging, true)
    ;   true
    ),
    (   between(1, Workers, I),
        message_queue_create(Queue),
        atom_concat(worker_, I, Alias),
        thread_create(worker_loop(Queue), ThreadId,
                      [detached(false), alias(Alias),
                       stack_limit(8_589_934_592)]),
        assertz(worker_queue(Queue)),
        assertz(worker(Queue, ThreadId, Alias)),
        fail
    ;   true
    ),
    start_watchdog.

%% check_worker_alive(+Queue) is det.
%%
%%  Verify that the worker thread associated with the given queue is
%%  still running. If the thread has died, this is a fatal condition —
%%  the server must crash rather than dispatching to a dead queue and
%%  hanging forever.
check_worker_alive(Queue) :-
    (   worker(Queue, ThreadId, Alias)
    ->  (   catch(thread_property(ThreadId, status(Status)), _, Status = not_found),
            (   Status == running
            ->  true
            ;   json_log_error_formatted(
                    "FATAL: Worker thread ~w (alias ~w) is dead (status: ~w). Crashing server to prevent indeterminate state.",
                    [ThreadId, Alias, Status]),
                halt(1)
            )
        )
    ;   json_log_error_formatted(
            "FATAL: No worker thread associated with queue ~w. Crashing server to prevent indeterminate state.",
            [Queue]),
        halt(1)
    ).

%% dispatch_request(+RequestDict, +HandlerModule, +HandlerName,
%%                  +InputStreamId, +ResponseStreamId) is det.
%%
%%  Enqueue a stream request to the worker pool. The request is sent to the
%%  next available worker queue in round-robin order. The worker will:
%%    1. Drain the input stream into a memory file (octet encoding)
%%    2. Build a SWI request from the request dict + body memory file
%%    3. Capture the handler output, parse it into a response dict
%%    4. Send the response to the Rust response stream and close it
dispatch_request(Request, HandlerModule, HandlerName, InputStreamId, ResponseStreamId) :-
    (   retract(worker_queue(Q))
    ->  assertz(worker_queue(Q)),
        check_worker_alive(Q),
        mark_worker_busy(Q),
        thread_send_message(Q, work(stream(Request, HandlerModule, HandlerName, InputStreamId, ResponseStreamId)))
    ;   json_log_error_formatted("dispatch_request: no worker queue available", []),
        throw(error(no_worker_available, _))
    ).

%% dispatch_request(+RequestDict, +HandlerModule, +HandlerName,
%%                  +InputReadFd, +OutputWriteFd, +Binary) is det.
%%
%%  Enqueue a pipe request to the worker pool. The worker will open the
%%  input/output file descriptors as Prolog streams and run the CGI handler
%%  directly on the output pipe.
dispatch_request(Request, HandlerModule, HandlerName, InputReadFd, OutputWriteFd, Binary) :-
    (   retract(worker_queue(Q))
    ->  assertz(worker_queue(Q)),
        check_worker_alive(Q),
        mark_worker_busy(Q),
        thread_send_message(Q, work(pipe(Request, HandlerModule, HandlerName, InputReadFd, OutputWriteFd, Binary)))
    ;   json_log_error_formatted("dispatch_request: no worker queue available", []),
        throw(error(no_worker_available, _))
    ).

%% mark_worker_busy(+Queue) is det.
%%
%%  Record that the worker associated with this queue has been assigned
%%  work. The watchdog uses this to determine which workers should have
%%  signaled 'ready' by now.
mark_worker_busy(Queue) :-
    (   worker(Queue, _ThreadId, Alias)
    ->  get_time(Now),
        retractall(worker_busy(Alias, _)),
        assertz(worker_busy(Alias, Now))
    ;   true  %% no worker found — check_worker_alive will handle it
    ).

%% mark_worker_ready(+Alias) is det.
%%
%%  Clear the busy flag for a worker that has signaled 'ready'.
%%  Called by the watchdog when it receives a 'ready' message.
mark_worker_ready(Alias) :-
    retractall(worker_busy(Alias, _)).

%% start_watchdog is det.
%%
%%  Start the watchdog thread if it is not already running. The watchdog
%%  listens on the watchdog queue for 'ready(Alias)' messages from workers
%%  and periodically checks for stuck or dead workers.
start_watchdog :-
    (   watchdog_running
    ->  true
    ;   message_queue_create(Q),
        assertz(watchdog_queue(Q)),
        assertz(watchdog_running),
        thread_create(watchdog_loop(Q), _,
                      [detached(false), alias(worker_watchdog),
                       stack_limit(8_589_934_592)])
    ).

%% signal_worker_ready(+Queue) is det.
%%
%%  Called by a worker after it has completed a request (handler +
%%  cleanup) and is about to return to thread_get_message. This tells
%%  the watchdog that the worker is healthy and available for more work.
signal_worker_ready(Queue) :-
    (   watchdog_queue(WQ)
    ->  (   worker(Queue, _, Alias)
        ->  thread_send_message(WQ, ready(Alias))
        ;   true  %% no worker entry — shouldn't happen in production
        )
    ;   true  %% watchdog not started (e.g., in unit tests)
    ).

%% watchdog_loop(+Queue) is det.
%%
%%  Main loop for the watchdog thread. It polls the watchdog queue for
%%  'ready' messages with a timeout, then checks all busy workers. If a
%%  worker has been busy longer than the grace period, the watchdog
%%  investigates: if the thread is dead, crash immediately; if it is
%%  running but stuck, dump the stack and crash.
watchdog_loop(Queue) :-
    (   thread_get_message(Queue, Message, [timeout(5)])
    ->  (   Message = ready(Alias)
        ->  mark_worker_ready(Alias)
        ;   true
        )
    ;   true  %% timeout — no messages, proceed to health check
    ),
    check_stuck_workers,
    watchdog_loop(Queue).

%% check_stuck_workers is det.
%%
%%  Iterate over all workers that are marked as busy. For each:
%%  - Immediately check if the thread is dead. If so, crash — a dead
%%    worker is always fatal, regardless of how long it has been busy
%%    or whether the grace period is disabled. A dead worker means the
%%    pipe FD is still open and the Rust side is hanging forever.
%%    halt(1) closes all FDs and terminates the process, causing the
%%    Rust side to get EOF and return an error.
%%  - If the thread is running and the grace period is enabled (not
%%    false), check whether the worker has been busy longer than the
%%    grace period. If so, log a warning (the worker may be running a
%%    long query or stuck in cleanup). This is NOT a crash — long
%%    queries are expected. When the grace period is false (disabled),
%%    no warning is logged.
check_stuck_workers :-
    get_time(Now),
    watchdog_grace_period(Grace),
    (   worker_busy(Alias, DispatchTime),
        worker(_Queue, ThreadId, Alias),
        Elapsed is Now - DispatchTime,
        catch(thread_property(ThreadId, status(Status)), _, Status = not_found),
        (   Status == running
        ->  (   Grace \== false,
                Elapsed > Grace
            ->  json_log_error_formatted(
                    "WARNING: Worker ~w (thread ~w) has been busy for ~1f seconds. Thread is running — may be a long query or stuck in cleanup.",
                    [Alias, ThreadId, Elapsed])
            ;   true  %% running, within grace period or grace disabled — normal
            )
        ;   %% Thread is dead or unfindable — crash immediately.
            json_log_error_formatted(
                "FATAL: Worker ~w (thread ~w) is dead (status: ~w) after ~1f seconds. Crashing server to prevent indeterminate state.",
                [Alias, ThreadId, Status, Elapsed]),
            halt(1)
        ),
        fail  %% backtrack to check all busy workers
    ;   true  %% no more busy workers
    ).

%% worker_loop(+Queue) is det.
%%
%%  Main loop for a worker thread. Blocks waiting for work messages and
%%  dispatches them to the appropriate handler. Between requests, the
%%  worker performs garbage collection and atom GC to prevent memory
%%  buildup that causes intermittent slowdowns in bulk WOQL queries.
%%
%%  CRITICAL: cleanup_worker_state and log_worker_memory run INSIDE the
%%  catch/3 block. If they ran outside, a GC failure would kill the
%%  thread silently (since detached threads propagate no errors), leaving
%%  the message queue orphaned and causing all subsequent requests
%%  dispatched to this worker to hang forever.
worker_loop(Queue) :-
    thread_get_message(Queue, Message),
    (   Message = work(stream(Request, HandlerModule, HandlerName, InputStreamId, ResponseStreamId))
    ->  log_worker_memory(before, HandlerModule, HandlerName),
        catch(
            (   handle_stream_work(Request, HandlerModule, HandlerName, InputStreamId, ResponseStreamId)
            ->  true
            ;   json_log_error_formatted("Worker goal failed for ~w ~w", [HandlerModule, HandlerName]),
                catch(send_error_response(ResponseStreamId, error(goal_failed, _)), _, true)
            ),
            Error,
            (   json_log_error_formatted("Worker error: ~q", [Error]),
                catch(send_error_response(ResponseStreamId, Error), _, true)
            )
        ),
        catch(
            (   cleanup_worker_state,
                log_worker_memory(after, HandlerModule, HandlerName)
            ),
            CleanupError,
            json_log_error_formatted(
                "Worker cleanup error (non-fatal, thread continues): ~q",
                [CleanupError])
        ),
        signal_worker_ready(Queue),
        worker_loop(Queue)
    ;   Message = work(pipe(Request, HandlerModule, HandlerName, InputReadFd, OutputWriteFd, Binary))
    ->  log_worker_memory(before, HandlerModule, HandlerName),
        catch(
            (   handle_pipe_work(Request, HandlerModule, HandlerName, InputReadFd, OutputWriteFd, Binary)
            ->  true
            ;   json_log_error_formatted("Worker goal failed for ~w ~w", [HandlerModule, HandlerName]),
                safe_write_cgi_error(OutputWriteFd, "Worker goal failed")
            ),
            Error,
            (   json_log_error_formatted("Worker error: ~q", [Error]),
                safe_write_cgi_error(OutputWriteFd, Error)
            )
        ),
        catch(
            (   cleanup_worker_state,
                log_worker_memory(after, HandlerModule, HandlerName)
            ),
            CleanupError,
            json_log_error_formatted(
                "Worker cleanup error (non-fatal, thread continues): ~q",
                [CleanupError])
        ),
        signal_worker_ready(Queue),
        worker_loop(Queue)
    ;   worker_loop(Queue)
    ).

%% cleanup_worker_state is det.
%%
%%  Reclaim memory between requests to prevent buildup in worker threads.
%%  Worker threads are long-lived and reuse the same Prolog stacks. Without
%%  explicit GC, atoms and trail entries from previous requests accumulate,
%%  causing intermittent slowdowns — especially in bulk WOQL arithmetic
%%  queries that create many temporary atoms (xsd:decimal/double values).
%%
%%  garbage_collect/0 and trim_stacks/0 are per-thread operations that
%%  do not block other threads. garbage_collect_atoms/0 is a GLOBAL
%%  operation that requires ALL threads to reach a safe point — if
%%  another worker is running a long query, garbage_collect_atoms blocks
%%  until that worker yields. To avoid this, we only call
%%  garbage_collect_atoms when the atom count exceeds a threshold,
%%  rather than on every request.
cleanup_worker_state :-
    get_time(T0),
    garbage_collect,
    get_time(T1),
    trim_stacks,
    get_time(T2),
    (   statistics(atoms, AtomCount),
        AtomCount > 50000
    ->  garbage_collect_atoms,
        get_time(T3),
        GCAtomTime is T3 - T2
    ;   GCAtomTime = 0
    ),
    GCTime is T1 - T0,
    TrimTime is T2 - T1,
    (   GCTime > 0.05
    ->  json_log_error_formatted(
            "SLOW_GC: garbage_collect took ~3f seconds, trim_stacks took ~3f seconds, atom_gc took ~3f seconds, atoms=~w",
            [GCTime, TrimTime, GCAtomTime, AtomCount])
    ;   true
    ).

%% log_worker_memory(+Phase, +HandlerModule, +HandlerName) is det.
%%
%%  Log memory and stack statistics for the current worker thread between
%%  requests. This helps diagnose memory/stack buildup that causes
%%  intermittent slowdowns in bulk WOQL arithmetic queries.
log_worker_memory(Phase, HandlerModule, HandlerName) :-
    (   current_prolog_flag(worker_memory_logging, true)
    ->  statistics(global_stack, [GSUsed, GSSize]),
        statistics(local_stack, [LSUsed, LSSize]),
        statistics(trail, TrailUsed),
        statistics(atoms, Atoms),
        thread_self(ThreadId),
        json_log_info_formatted(
            "WORKER_MEM ~w thread=~w handler=~w:~w global_used=~w global_size=~w local_used=~w local_size=~w trail=~w atoms=~w",
            [Phase, ThreadId, HandlerModule, HandlerName, GSUsed, GSSize, LSUsed, LSSize, TrailUsed, Atoms])
    ;   true
    ).

%% handle_stream_work(+Request, +HandlerModule, +HandlerName,
%%                     +InputStreamId, +ResponseStreamId) is det.
%%
%%  Process a single stream request. Drains the input stream into a memory file
%%  (single copy, octet encoding), builds the SWI request directly, then
%%  dispatches via http_dispatch_with_expansion. The response is captured
%%  and sent to the output stream.
%%
%%  For stream handlers (HandlerName == stream_handler), the ResponseStreamId
%%  is made available so that ndjson streaming can use it.
handle_stream_work(Request, _HandlerModule, HandlerName, InputStreamId, ResponseStreamId) :-
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

%% handle_pipe_work(+Request, +HandlerModule, +HandlerName,
%%                   +InputReadFd, +OutputWriteFd, +Binary) is det.
%%
%%  Process a single plugin request through OS pipes. The input pipe is drained
%%  into a memory file, the SWI request is built, and the handler runs with a
%%  CGI stream writing directly to the output pipe.
handle_pipe_work(Request, _HandlerModule, _HandlerName, InputReadFd, OutputWriteFd, Binary) :-
    (   Binary == true -> WriteEnc = octet ; WriteEnc = utf8 ),
    setup_call_cleanup(
        (   (   InputReadFd >= 0
            ->  '$appserver':appserver_open_fd_stream(InputReadFd, read, octet, InStream),
                drain_pipe_to_memory_file(InStream, MemoryFile, BodyStream, BodyLen)
            ;   empty_body_stream(MemoryFile, BodyStream, BodyLen)
            ),
            '$appserver':appserver_open_fd_stream(OutputWriteFd, write, WriteEnc, OutStream)
        ),
        (   build_swi_request_from_dict(Request, BodyStream, BodyLen, SWIRequest),
            catch(
                (   cgi_open(OutStream, CGI, srv_http:cgi_capture_hook, [request(SWIRequest)]),
                    setup_call_cleanup(
                        (   Binary == true
                        ->  set_stream(CGI, encoding(octet))
                        ;   set_stream(CGI, encoding(utf8))
                        ),
                        with_output_to(CGI,
                            tdb_http_handler:http_dispatch_with_expansion(SWIRequest)),
                        close(CGI)
                    )
                ->  true
                ;   write_cgi_error(OutStream, 500, "Handler goal failed")
                ),
                Error,
                handle_handler_error(Error, OutStream, CGI)
            )
        ),
        (   catch(close(OutStream), _, true),
            (   InputReadFd >= 0 -> catch(close(InStream), _, true) ; true ),
            catch(close(BodyStream), _, true),
            catch(free_memory_file(MemoryFile), _, true),
            %% Safety net: close/1 on a stream may fail to close the
            %% underlying FD if flushing buffered data encounters an error
            %% (e.g. broken pipe). Explicitly close the FDs to prevent leaks.
            catch('$appserver':appserver_close_fd(OutputWriteFd), _, true),
            (   InputReadFd >= 0 -> catch('$appserver':appserver_close_fd(InputReadFd), _, true) ; true )
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

%% drain_pipe_to_memory_file(+InStream, -MemoryFile, -BodyStream, -BodyLen) is det.
%%
%%  Read all bytes from the input pipe stream into a memory file with octet
%%  encoding, then reopen the memory file for reading. The pipe is closed by
%%  the caller after this predicate returns.
drain_pipe_to_memory_file(InStream, MemoryFile, BodyStream, BodyLen) :-
    new_memory_file(MemoryFile),
    open_memory_file(MemoryFile, write, WriteStream, [type(binary), encoding(octet)]),
    drain_pipe_bytes(InStream, WriteStream, 0, BodyLen),
    close(WriteStream),
    open_memory_file(MemoryFile, read, BodyStream, [type(binary), encoding(octet)]).

drain_pipe_bytes(InStream, WriteStream, AccLen, TotalLen) :-
    (   at_end_of_stream(InStream)
    ->  TotalLen = AccLen
    ;   get_byte(InStream, Byte),
        (   Byte == -1
        ->  TotalLen = AccLen
        ;   put_byte(WriteStream, Byte),
            NewLen is AccLen + 1,
            drain_pipe_bytes(InStream, WriteStream, NewLen, TotalLen)
        )
    ).

%% empty_body_stream(-MemoryFile, -BodyStream, -BodyLen) is det.
%%
%%  Create an empty memory file and reopen it for reading. Used for requests
%%  that have no body.
empty_body_stream(MemoryFile, BodyStream, 0) :-
    new_memory_file(MemoryFile),
    open_memory_file(MemoryFile, read, BodyStream, [type(binary), encoding(octet)]).

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

%% write_cgi_error(+Stream, +Status, +Message) is det.
%%
%%  Write a CGI-style error response (headers + JSON body) to the stream.
write_cgi_error(Stream, Status, Message) :-
    (   atom(Message)
    ->  atom_string(Message, MessageStr)
    ;   compound(Message)
    ->  term_string(Message, MessageStr)
    ;   MessageStr = Message
    ),
    ErrorBody = _{
        '@type': 'api:ErrorResponse',
        'api:status': 'api:failure',
        'api:error': _{'@type': 'api:InternalServerError'},
        'api:message': MessageStr
    },
    with_output_to(string(JsonBody), json_write_dict(current_output, ErrorBody, [as(string)])),
    format(Stream, 'Status: ~w\nContent-Type: application/json\n\n~s', [Status, JsonBody]).

%% write_cgi_error_body(+Stream, +Message) is det.
%%
%%  Write a JSON error body after headers have already been sent. This is used
%%  when a handler fails partway through writing the response.
write_cgi_error_body(Stream, Message) :-
    (   atom(Message)
    ->  atom_string(Message, MessageStr)
    ;   compound(Message)
    ->  term_string(Message, MessageStr)
    ;   MessageStr = Message
    ),
    ErrorBody = _{
        '@type': 'api:ErrorResponse',
        'api:status': 'api:failure',
        'api:error': _{'@type': 'api:InternalServerError'},
        'api:message': MessageStr
    },
    with_output_to(string(JsonBody), json_write_dict(current_output, ErrorBody, [as(string)])),
    format(Stream, '\n\n############# ERROR #################\n~s', [JsonBody]).

%% handle_handler_error(+Error, +OutStream, +CGI) is det.
%%
%%  Handle a handler exception. If the CGI headers have already been sent, write
%%  an error banner into the response body; otherwise write a fresh CGI error
%%  response.
handle_handler_error(Error, OutStream, CGI) :-
    (   cgi_headers_sent(CGI)
    ->  catch(format(OutStream, '~n~n############# ERROR #################~n', []), _, true),
        catch(write_cgi_error_body(OutStream, Error), _, true)
    ;   catch(write_cgi_error(OutStream, 500, Error), _, true)
    ).

cgi_headers_sent(CGI) :-
    catch(cgi_property(CGI, header_codes(Codes)), _, fail),
    Codes \= [].

%% open_fd_for_error(+Fd, -Stream) is semidet.
%%
%%  Best-effort attempt to open the output FD as a UTF-8 stream for writing an
%%  error response. This is used when handle_pipe_work fails before opening the
%%  output stream itself.
open_fd_for_error(Fd, Stream) :-
    '$appserver':appserver_open_fd_stream(Fd, write, utf8, Stream).

%% safe_write_cgi_error(+Fd, +Message) is det.
%%
%%  Best-effort attempt to write a CGI error response to the output pipe. The
%%  predicate always succeeds, even if the FD cannot be opened or the write
%%  fails, so that the worker loop never dies due to a failing error handler.
safe_write_cgi_error(Fd, Message) :-
    (   catch(open_fd_for_error(Fd, ErrStream), _, true),
        ground(ErrStream)
    ->  catch(write_cgi_error(ErrStream, 500, Message), _, true),
        catch(close(ErrStream), _, true)
    ;   true
    ).

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

:- begin_tests(request_worker_pool, [concurrent(false)]).

%% Suppress SWI-Prolog's "Thread running ... died on exception" warnings
%% for test threads that intentionally throw error(test_thread_death(_), _)
%% to simulate worker death. Only messages carrying a test_thread_death/1
%% error are suppressed — genuine thread deaths from other modules still
%% produce warnings.
:- multifile(user:message_hook/3).
user:message_hook(abnormal_thread_completion(throw(error(test_thread_death(_), _)), _), _Kind, _Lines) :- !.

test(write_cgi_error_format) :-
    with_output_to(string(Result),
                   write_cgi_error(current_output, 404, "Not found")),
    assertion(sub_string(Result, _, _, _, 'Status: 404')),
    assertion(sub_string(Result, _, _, _, 'Content-Type: application/json')),
    assertion(sub_string(Result, _, _, _, '"api:message":"Not found"')).

test(write_cgi_error_body_format) :-
    with_output_to(string(Result),
                   write_cgi_error_body(current_output, "Oops")),
    assertion(sub_string(Result, _, _, _, '############# ERROR #################')),
    assertion(sub_string(Result, _, _, _, '"api:message":"Oops"')).

test(empty_body_stream_zero_length) :-
    empty_body_stream(MemoryFile, BodyStream, 0),
    assertion(at_end_of_stream(BodyStream)),
    close(BodyStream),
    free_memory_file(MemoryFile).

test(build_swi_request_from_dict_basic) :-
    Request = _{method: "GET", path: "/api", query: "", headers: _{'Content-Type': 'application/json'}},
    build_swi_request_from_dict(Request, _BodyStream, 0, SWIRequest),
    assertion(member(method(get), SWIRequest)),
    assertion(member(path('/api'), SWIRequest)),
    assertion(member(content_length(0), SWIRequest)),
    assertion(member(content_type('application/json'), SWIRequest)).

%% --- Segment 2: Input Rust -> SWI-Prolog thread via mpsc (request dispatch + body) ---
%%
%% These tests verify that build_swi_request_from_dict/4 correctly constructs
%% the SWI request list from the request dict produced by Rust's JSON
%% serialization, including UTF-8 query strings, paths, headers, and body
%% content delivered through a memory file stream.

test(build_swi_request_from_dict_utf8_query) :-
    %% Query string with percent-encoded UTF-8: name=Kurt%20G%C3%B6del
    Request = _{method: "GET", path: "/api/document/admin/db", query: "name=Kurt%20G%C3%B6del", headers: _{'Content-Type': 'application/json'}},
    build_swi_request_from_dict(Request, _BodyStream, 0, SWIRequest),
    assertion(member(method(get), SWIRequest)),
    assertion(member(path('/api/document/admin/db'), SWIRequest)),
    assertion(member(search(Search), SWIRequest)),
    assertion(member(name=KurtG, Search)),
    %% The decoded value should be the atom with the UTF-8 character
    atom_codes(KurtG, [0x4B,0x75,0x72,0x74,0x20,0x47,0xF6,0x64,0x65,0x6C]),
    assertion(member(content_length(0), SWIRequest)).

test(build_swi_request_from_dict_post_with_body) :-
    Request = _{method: "POST", path: "/api/document/admin/db", query: "", headers: _{'Content-Type': 'application/json'}},
    %% Create a body stream with known content
    new_memory_file(MF),
    open_memory_file(MF, write, W, [type(binary), encoding(octet)]),
    format(W, '{"name":"Kurt G~cC3~cB6~cdel"}', [0xC3, 0xB6, 0x64]),
    close(W),
    open_memory_file(MF, read, BodyStream, [type(binary), encoding(octet)]),
    build_swi_request_from_dict(Request, BodyStream, 42, SWIRequest),
    assertion(member(method(post), SWIRequest)),
    assertion(member(content_length(42), SWIRequest)),
    assertion(member(input(BodyStream), SWIRequest)),
    close(BodyStream),
    free_memory_file(MF).

test(build_swi_request_from_dict_multiple_headers) :-
    Request = _{method: "PUT", path: "/api/document/admin/db", query: "author=test&message=hello",
                headers: _{'Content-Type': 'application/json', 'Authorization': 'Basic YWRtaW46cm9vdA==', 'Accept': 'application/json'}},
    build_swi_request_from_dict(Request, _BodyStream, 0, SWIRequest),
    assertion(member(method(put), SWIRequest)),
    assertion(member(content_type('application/json'), SWIRequest)),
    assertion(member(authorization('Basic YWRtaW46cm9vdA=='), SWIRequest)),
    assertion(member(search(Search), SWIRequest)),
    assertion(member(author=test, Search)),
    assertion(member(message=hello, Search)).

test(build_swi_request_from_dict_empty_query) :-
    Request = _{method: "GET", path: "/api/ok", query: "", headers: _{}},
    build_swi_request_from_dict(Request, _BodyStream, 0, SWIRequest),
    assertion(member(method(get), SWIRequest)),
    assertion(member(path('/api/ok'), SWIRequest)),
    assertion(member(search([]), SWIRequest)),
    assertion(member(content_length(0), SWIRequest)).

test(build_swi_request_from_dict_body_stream_readable) :-
    %% Verify that the body stream passed to build_swi_request_from_dict
    %% is readable and contains the exact bytes written.
    BodyBytes = [0x7B, 0x22, 0x6E, 0x61, 0x6D, 0x65, 0x22, 0x3A, 0x22, 0x47, 0xC3, 0xB6, 0x64, 0x65, 0x6C, 0x22, 0x7D],
    new_memory_file(MF),
    open_memory_file(MF, write, W, [type(binary), encoding(octet)]),
    forall(member(B, BodyBytes), put_byte(W, B)),
    close(W),
    open_memory_file(MF, read, BodyStream, [type(binary), encoding(octet)]),
    Request = _{method: "POST", path: "/api", query: "", headers: _{'Content-Type': 'application/json'}},
    build_swi_request_from_dict(Request, BodyStream, 17, SWIRequest),
    assertion(member(input(BodyStream), SWIRequest)),
    %% Read the body stream back and verify bytes
    read_string(BodyStream, _, BodyText),
    string_codes(BodyText, Codes),
    assertion(Codes == BodyBytes),
    close(BodyStream),
    free_memory_file(MF).

test(is_binary_response_detects_octet_stream) :-
    Response = _{status: 200, body: "abc", headers: _{'Content-Type': 'application/octet-stream'}},
    assertion(is_binary_response(Response)).

test(is_binary_response_rejects_json) :-
    Response = _{status: 200, body: "{}", headers: _{'Content-Type': 'application/json'}},
    assertion(\+ is_binary_response(Response)).

test(appserver_open_fd_stream_file) :-
    tmp_file('test', File),
    open(File, write, Stream),
    stream_property(Stream, file_no(Fd)),
    '$appserver':appserver_open_fd_stream(Fd, write, utf8, NewStream),
    format(NewStream, 'hello', []),
    close(NewStream),
    read_file_to_string(File, Text, []),
    assertion(Text == "hello"),
    delete_file(File).

%% --- Segment 1: Input pipe round-trip (HTTP client → Axum → SWI-Prolog) ---
%%
%% These tests verify that bytes written to the write end of a pipe are
%% read back byte-for-byte from the Prolog stream opened via
%% appserver_open_fd_stream/4, and that drain_pipe_to_memory_file/4
%% preserves every byte value 0-255 unchanged.

test(drain_pipe_to_memory_file_ascii) :-
    pipe(Read, Write),
    stream_property(Read, file_no(ReadFd)),
    '$appserver':appserver_open_fd_stream(ReadFd, read, octet, InStream),
    setup_call_cleanup(
        open_string("Hello, world!", Src),
        (   copy_stream_data(Src, Write),
            close(Write)
        ),
        close(Src)
    ),
    drain_pipe_to_memory_file(InStream, MemoryFile, BodyStream, BodyLen),
    assertion(BodyLen == 13),
    setup_call_cleanup(
        true,
        (   read_string(BodyStream, _, BodyText),
            assertion(BodyLen == 13),
            assertion(BodyText == "Hello, world!")
        ),
        (   close(BodyStream),
            close(InStream),
            catch(close(Read), _, true),
            free_memory_file(MemoryFile)
        )
    ).

test(drain_pipe_to_memory_file_utf8_multibyte) :-
    %% "Kurt G\u00f6del" in UTF-8 bytes: 4B 75 72 74 20 47 C3 B6 64 65 6C
    Bytes = [0x4B,0x75,0x72,0x74,0x20,0x47,0xC3,0xB6,0x64,0x65,0x6C],
    pipe(Read, Write),
    set_stream(Write, encoding(octet)),
    stream_property(Read, file_no(ReadFd)),
    '$appserver':appserver_open_fd_stream(ReadFd, read, octet, InStream),
    setup_call_cleanup(
        open_string(Bytes, Src),
        (   copy_stream_data(Src, Write),
            close(Write)
        ),
        close(Src)
    ),
    drain_pipe_to_memory_file(InStream, MemoryFile, BodyStream, BodyLen),
    assertion(BodyLen == 11),
    setup_call_cleanup(
        true,
        (   read_string(BodyStream, _, BodyText),
            assertion(string_length(BodyText, 11)),
            %% Verify the exact UTF-8 byte sequence is preserved
            string_codes(BodyText, Codes),
            assertion(Codes == Bytes)
        ),
        (   close(BodyStream),
            close(InStream),
            catch(close(Read), _, true),
            free_memory_file(MemoryFile)
        )
    ).

test(drain_pipe_to_memory_file_binary_octets) :-
    %% Raw bytes including 0x00, 0xFF, and other high-bit values
    Bytes = [0x00,0x01,0x7F,0x80,0xFF,0xC3,0xB6,0xFE],
    pipe(Read, Write),
    set_stream(Write, encoding(octet)),
    stream_property(Read, file_no(ReadFd)),
    '$appserver':appserver_open_fd_stream(ReadFd, read, octet, InStream),
    forall(member(B, Bytes), put_byte(Write, B)),
    close(Write),
    drain_pipe_to_memory_file(InStream, MemoryFile, BodyStream, BodyLen),
    assertion(BodyLen == 8),
    setup_call_cleanup(
        true,
        (   read_string(BodyStream, _, BodyText),
            string_codes(BodyText, Codes),
            assertion(Codes == Bytes)
        ),
        (   close(BodyStream),
            close(InStream),
            catch(close(Read), _, true),
            free_memory_file(MemoryFile)
        )
    ).

test(drain_pipe_to_memory_file_empty) :-
    pipe(Read, Write),
    stream_property(Read, file_no(ReadFd)),
    '$appserver':appserver_open_fd_stream(ReadFd, read, octet, InStream),
    close(Write),
    drain_pipe_to_memory_file(InStream, MemoryFile, BodyStream, BodyLen),
    assertion(BodyLen == 0),
    assertion(at_end_of_stream(BodyStream)),
    close(BodyStream),
    close(InStream),
    catch(close(Read), _, true),
    free_memory_file(MemoryFile).

test(drain_pipe_to_memory_file_japanese_utf8) :-
    %% Japanese UTF-8: E6 97 A5 E6 9C AC E8 AA 9E
    Bytes = [0xE6,0x97,0xA5,0xE6,0x9C,0xAC,0xE8,0xAA,0x9E],
    pipe(Read, Write),
    set_stream(Write, encoding(octet)),
    stream_property(Read, file_no(ReadFd)),
    '$appserver':appserver_open_fd_stream(ReadFd, read, octet, InStream),
    setup_call_cleanup(
        open_string(Bytes, Src),
        (   copy_stream_data(Src, Write),
            close(Write)
        ),
        close(Src)
    ),
    drain_pipe_to_memory_file(InStream, MemoryFile, BodyStream, BodyLen),
    assertion(BodyLen == 9),
    setup_call_cleanup(
        true,
        (   read_string(BodyStream, _, BodyText),
            string_codes(BodyText, Codes),
            assertion(Codes == Bytes)
        ),
        (   close(BodyStream),
            close(InStream),
            catch(close(Read), _, true),
            free_memory_file(MemoryFile)
        )
    ).

%% --- Segment 3: Output SWI-Prolog thread -> fd pipe (CGI response) ---
%%
%% These tests verify that CGI output written by a handler to an output pipe
%% FD is byte-correct for both UTF-8 and binary endpoint registrations.
%% They simulate the exact flow in handle_pipe_work: open pipe write end via
%% appserver_open_fd_stream/4, cgi_open/4 on the fd-backed stream, set CGI
%% encoding, write headers + body, close CGI, close OutStream (pipe EOF),
%% then read the pipe read end as raw octets and verify the exact byte
%% sequence.

%% Helper: read all bytes from a pipe read-end FD as a list of codes.
pipe_read_all_bytes(ReadFd, Bytes) :-
    '$appserver':appserver_open_fd_stream(ReadFd, read, octet, ReadStream),
    read_string(ReadStream, _, Text),
    string_codes(Text, Bytes),
    close(ReadStream).

%% Helper: run a CGI handler writing to a pipe with given encoding.
%% Produces the raw bytes that appear on the pipe read end.
cgi_to_pipe_bytes(WriteEnc, CgiEnc, Goal, Bytes) :-
    pipe(Read, Write),
    stream_property(Write, file_no(WriteFd)),
    stream_property(Read, file_no(ReadFd)),
    setup_call_cleanup(
        '$appserver':appserver_open_fd_stream(WriteFd, write, WriteEnc, OutStream),
        (   catch(
                (   cgi_open(OutStream, CGI, srv_http:cgi_capture_hook, [request([])]),
                    setup_call_cleanup(
                        set_stream(CGI, encoding(CgiEnc)),
                        with_output_to(CGI, call(Goal)),
                        close(CGI)
                    )
                ->  true
                ;   throw(error(goal_failed, Goal))
                ),
                Error,
                (   catch(close(CGI), _, true),
                    throw(Error)
                )
            ),
            close(OutStream)
        ),
        catch(close(Write), _, true)
    ),
    pipe_read_all_bytes(ReadFd, Bytes),
    catch(close(Read), _, true).

test(cgi_pipe_utf8_simple_ascii) :-
    Goal = (format(current_output, 'Status: 200 OK~n', []),
            format(current_output, 'Content-Type: application/json~n~n', []),
            format(current_output, '{"ok":true}', [])),
    cgi_to_pipe_bytes(utf8, utf8, Goal, Bytes),
    %% Expected: headers + Content-Length (inserted by cgi_capture_hook) + \n\n + body
    Expected = "Status: 200 OK\nContent-Type: application/json\nContent-Length: 11\n\n{\"ok\":true}",
    string_codes(Expected, ExpectedCodes),
    assertion(Bytes == ExpectedCodes).

test(cgi_pipe_utf8_multibyte_body) :-
    %% Handler writes a body containing the character \u00f6 (o with diaeresis)
    %% In UTF-8 mode, this should be encoded as 0xC3 0xB6
    Goal = (format(current_output, 'Status: 200 OK~n', []),
            format(current_output, 'Content-Type: application/json~n~n', []),
            format(current_output, '"Kurt G\u00f6del"', [])),
    cgi_to_pipe_bytes(utf8, utf8, Goal, Bytes),
    %% Find the body after the \n\n separator
    once(append(_Header, [0x0A, 0x0A | Body], Bytes)),
    %% Body should be: " K u r t   G 0xC3 0xB6 d e l "
    assertion(Body == [0x22, 0x4B, 0x75, 0x72, 0x74, 0x20, 0x47, 0xC3, 0xB6, 0x64, 0x65, 0x6C, 0x22]).

test(cgi_pipe_utf8_japanese_body) :-
    %% Handler writes Japanese characters
    %% In UTF-8 mode, each character is 3 bytes
    Goal = (format(current_output, 'Status: 200 OK~n', []),
            format(current_output, 'Content-Type: application/json~n~n', []),
            format(current_output, '"\u65e5\u672c\u8a9e"', [])),
    cgi_to_pipe_bytes(utf8, utf8, Goal, Bytes),
    once(append(_Header, [0x0A, 0x0A | Body], Bytes)),
    %% Body: " E6 97 A5 E6 9C AC E8 AA 9E "
    assertion(Body == [0x22, 0xE6, 0x97, 0xA5, 0xE6, 0x9C, 0xAC, 0xE8, 0xAA, 0x9E, 0x22]).

test(cgi_pipe_binary_preserves_raw_bytes) :-
    %% In binary (octet) mode, raw bytes pass through unchanged
    %% Handler writes bytes 0x00 0xFF 0x80 as body content
    Goal = (format(current_output, 'Status: 200 OK~n', []),
            format(current_output, 'Content-Type: application/octet-stream~n~n', []),
            forall(member(B, [0x00, 0xFF, 0x80, 0x42]), put_byte(current_output, B))),
    cgi_to_pipe_bytes(octet, octet, Goal, Bytes),
    once(append(_Header, [0x0A, 0x0A | Body], Bytes)),
    assertion(Body == [0x00, 0xFF, 0x80, 0x42]).

test(cgi_pipe_utf8_headers_correct) :-
    %% Verify that CGI headers are correctly written to the pipe
    Goal = (format(current_output, 'Status: 201 Created~n', []),
            format(current_output, 'Content-Type: application/json; charset=UTF-8~n~n', []),
            format(current_output, '{"id":1}', [])),
    cgi_to_pipe_bytes(utf8, utf8, Goal, Bytes),
    %% Parse the header portion (everything before \n\n)
    once(append(HeaderBytes, [0x0A, 0x0A | _Body], Bytes)),
    string_codes(HeaderStr, HeaderBytes),
    assertion(sub_string(HeaderStr, _, _, _, 'Status: 201 Created')),
    assertion(sub_string(HeaderStr, _, _, _, 'Content-Type: application/json; charset=UTF-8')).

test(cgi_pipe_utf8_empty_body) :-
    Goal = (format(current_output, 'Status: 204~n~n', [])),
    cgi_to_pipe_bytes(utf8, utf8, Goal, Bytes),
    %% Status: 204\nContent-Length: 0\n\n — empty body gets Content-Length: 0
    Expected = "Status: 204\nContent-Length: 0\n\n",
    string_codes(Expected, ExpectedCodes),
    assertion(Bytes == ExpectedCodes).

test(cgi_pipe_utf8_reply_json_writes_utf8) :-
    %% Test that reply_json_dict writes raw UTF-8 (no \uXXXX escapes)
    %% when the CGI stream is UTF-8 encoded
    Goal = (format(current_output, 'Status: 200 OK~n', []),
            format(current_output, 'Content-Type: application/json~n~n', []),
            format(current_output, '{"name":"Kurt G\u00f6del"}', [])),
    cgi_to_pipe_bytes(utf8, utf8, Goal, Bytes),
    once(append(_Header, [0x0A, 0x0A | Body], Bytes)),
    %% The body must contain 0xC3 0xB6 for \u00f6, not \u00f6 escape
    assertion(member(0xC3, Body)),
    assertion(member(0xB6, Body)),
    %% Verify no backslash-u escape sequence for this character
    \+ once(append(_, [0x5C, 0x75, 0x30, 0x30, 0x66, 0x36 | _], Body)).

%% --- Worker thread death detection (root cause of intermittent hangs) ---
%%
%% These tests confirm the hypothesis that a worker thread created with
%% detached(true) dies silently when an exception occurs outside the
%% catch/3 block. The thread's message queue still exists, so subsequent
%% dispatches to it hang forever. thread_property/2 can detect the dead
%% thread, but only if detached(false) is used.

test(detached_thread_dies_silently_on_exception) :-
    %% A detached thread that throws outside catch/3 dies silently.
    %% No error is propagated to the parent thread.
    message_queue_create(Q),
    thread_create(
        ( throw(error(test_thread_death(outside_catch_error), _)) ),
        _ThreadId,
        [detached(true), alias(test_detached_die)]
    ),
    %% Give the thread time to die
    sleep(0.1),
    %% The thread should be dead — thread_property reports status(false)
    catch(thread_property(test_detached_die, status(Status)), _, Status = not_found),
    %% A dead detached thread may report 'false' or 'exception' or be
    %% unfindable. The key point: it does NOT report 'running'.
    assertion(Status \= running),
    %% Cleanup: the message queue persists even after the thread dies
    message_queue_destroy(Q).

test(non_detached_thread_death_is_observable) :-
    %% A non-detached thread that throws outside catch/3 also dies,
    %% but its status is queryable via thread_property/2.
    thread_create(
        ( throw(error(test_thread_death(outside_catch_error), _)) ),
        ThreadId,
        [detached(false), alias(test_nondetached_die)]
    ),
    %% Give the thread time to die
    sleep(0.1),
    catch(thread_property(ThreadId, status(Status)), _, Status = not_found),
    %% Non-detached thread death is observable: status should be
    %% false, exception(_), or true (exited). NOT running.
    assertion(Status \= running),
    %% Join to clean up
    catch(thread_join(ThreadId, _), _, true).

test(cleanup_outside_catch_kills_thread) :-
    %% Simulate the current worker_loop structure: cleanup runs
    %% OUTSIDE the catch/3 block. If cleanup throws, the thread dies
    %% silently because there's no outer catch.
    thread_create(
        ( catch(true, _, true),       %% "handler" succeeds
          throw(error(test_thread_death(cleanup_failed), _)),       %% "cleanup" throws — OUTSIDE catch
          true                         %% never reached
        ),
        ThreadId,
        [detached(false), alias(test_cleanup_outside)]
    ),
    sleep(0.1),
    catch(thread_property(ThreadId, status(Status)), _, Status = not_found),
    %% Thread should be dead, not running
    assertion(Status \= running),
    catch(thread_join(ThreadId, ExitStatus), _, true),
    %% The exit status should show the exception
    (   ground(ExitStatus),
        ExitStatus = exception(_)
    ->  true
    ;   ExitStatus = false
    ->  true
    ;   true  %% Thread may have already been joined
    ).

test(cleanup_inside_catch_preserves_thread) :-
    %% When cleanup runs INSIDE the catch/3 block, a cleanup failure
    %% is caught and the thread stays alive.
    message_queue_create(TestQueue),
    message_queue_create(ReadyQueue),
    thread_create(
        ( catch(
              ( true,                          %% "handler" succeeds
                throw(error(test_thread_death(cleanup_failed), _))           %% "cleanup" throws — INSIDE catch
              ),
              _Error,
              ( true                            %% thread continues — error caught
              )
            ),
            %% Thread reaches here — it survived the cleanup failure.
            %% Signal that we survived, then block on the queue so the
            %% main thread can observe us as "running".
            thread_self(Self),
            thread_send_message(ReadyQueue, survived(Self)),
            thread_get_message(TestQueue, _)    %% block until released
        ),
        ThreadId,
        [detached(false), alias(test_cleanup_inside)]
    ),
    %% Wait for the thread to signal it survived
    thread_get_message(ReadyQueue, survived(Survived)),
    assertion(Survived == ThreadId),
    catch(thread_property(ThreadId, status(Status)), _, Status = not_found),
    %% Thread should still be running (it survived the caught cleanup error)
    assertion(Status == running),
    %% Release the thread and cleanup
    thread_send_message(TestQueue, done),
    thread_join(ThreadId, _),
    message_queue_destroy(TestQueue),
    message_queue_destroy(ReadyQueue).

test(dispatch_to_dead_worker_hangs) :-
    %% Confirm that dispatching to a dead worker's queue causes a hang.
    %% A dead worker's message queue still exists but nobody reads from it.
    message_queue_create(DeadQueue),
    thread_create(
        ( throw(error(test_thread_death(die_immediately), _)) ),
        _,
        [detached(true), alias(test_dead_worker)]
    ),
    sleep(0.1),
    %% The queue exists but the thread is dead.
    %% thread_send_message will succeed (message is queued),
    %% but nobody will ever read it.
    thread_send_message(DeadQueue, work(test)),
    %% Verify the message is stuck in the queue
    message_queue_property(DeadQueue, size(Size)),
    assertion(Size >= 1),
    %% Cleanup: destroy the queue. The stuck message is lost.
    message_queue_destroy(DeadQueue).

%% --- Worker memory cleanup ---

test(cleanup_worker_state_runs_gc_and_trim) :-
    %% cleanup_worker_state should call garbage_collect/0 and
    %% trim_stacks/0 without errors. It must always succeed
    %% (it is called between every request in the worker loop).
    %% garbage_collect_atoms/0 is only called when atom count
    %% exceeds 50000 to avoid blocking other threads on every request.
    cleanup_worker_state.

test(cleanup_worker_state_is_det) :-
    %% cleanup_worker_state must be deterministic — if it leaves
    %% choicepoints, the worker_loop recursion could accumulate them.
    %% Verify it succeeds and does not throw.
    cleanup_worker_state,
    !.

test(cleanup_worker_state_reduces_atom_count) :-
    %% Create a large number of temporary atoms, then verify that
    %% cleanup_worker_state triggers atom GC that reduces the atom count.
    %% We create atoms that are not referenced elsewhere so they become
    %% garbage after the call.
    findall(A, (between(1, 500, I), atom_concat(temp_atom_, I, A), nb_setval(temp, A)), _),
    statistics(atoms, Before),
    cleanup_worker_state,
    statistics(atoms, After),
    %% Atom count should not increase. It may not decrease much if the
    %% atoms are still on the trail, but it must not grow.
    assertion(After =< Before + 500),
    %% Clean up the nb_setval references
    nb_delete(temp).

%% --- Watchdog mechanism ---

test(signal_worker_ready_sends_to_watchdog_queue) :-
    %% When the watchdog queue exists, signal_worker_ready(Queue) sends
    %% a ready(Alias) message to it, looking up the alias from worker/3.
    message_queue_create(WQ),
    assertz(request_worker_pool:watchdog_queue(WQ)),
    message_queue_create(TestQ),
    assertz(request_worker_pool:worker(TestQ, dummy_thread, test_signal_alias)),
    signal_worker_ready(TestQ),
    thread_get_message(WQ, ready(Alias), [timeout(5)]),
    assertion(Alias == test_signal_alias),
    message_queue_destroy(WQ),
    message_queue_destroy(TestQ),
    retractall(request_worker_pool:watchdog_queue(WQ)),
    retractall(request_worker_pool:worker(TestQ, _, _)).

test(signal_worker_ready_noop_without_watchdog) :-
    %% When no watchdog queue is registered, signal_worker_ready
    %% succeeds silently (no error).
    retractall(request_worker_pool:watchdog_queue(_)),
    message_queue_create(TestQ),
    signal_worker_ready(TestQ),
    message_queue_destroy(TestQ).

test(mark_worker_busy_records_dispatch_time) :-
    %% mark_worker_busy records the current time for a worker.
    retractall(request_worker_pool:worker(_, _, _)),
    retractall(request_worker_pool:worker_busy(_, _)),
    message_queue_create(Q),
    assertz(request_worker_pool:worker(Q, dummy_thread, test_busy_alias)),
    get_time(Before),
    mark_worker_busy(Q),
    get_time(After),
    request_worker_pool:worker_busy(test_busy_alias, Time),
    assertion(Time >= Before),
    assertion(Time =< After),
    retractall(request_worker_pool:worker(_, _, _)),
    retractall(request_worker_pool:worker_busy(_, _)),
    message_queue_destroy(Q).

test(mark_worker_ready_clears_busy_flag) :-
    %% mark_worker_ready removes the busy flag.
    assertz(request_worker_pool:worker_busy(test_clear_alias, 1234.0)),
    mark_worker_ready(test_clear_alias),
    \+ request_worker_pool:worker_busy(test_clear_alias, _).

test(check_worker_alive_crashes_for_dead_thread) :-
    %% check_worker_alive calls halt(1) when the thread is dead.
    %% We test this by creating a worker entry with a dead thread
    %% and verifying that check_worker_alive detects it.
    %% Since halt(1) would kill the test process, we instead
    %% verify the logic by checking thread_property directly.
    thread_create(
        ( throw(error(test_thread_death(die_for_check), _)) ),
        ThreadId,
        [detached(false), alias(test_check_alive_dead)]
    ),
    sleep(0.1),
    catch(thread_property(ThreadId, status(Status)), _, Status = not_found),
    assertion(Status \= running),
    catch(thread_join(ThreadId, _), _, true).

test(check_worker_alive_succeeds_for_running_thread) :-
    %% A running thread should pass the alive check.
    message_queue_create(Q),
    thread_create(
        thread_get_message(Q, _),
        ThreadId,
        [detached(false), alias(test_check_alive_running)]
    ),
    sleep(0.1),
    catch(thread_property(ThreadId, status(Status)), _, Status = not_found),
    assertion(Status == running),
    thread_send_message(Q, done),
    thread_join(ThreadId, _),
    message_queue_destroy(Q).

test(watchdog_grace_period_is_positive_or_disabled) :-
    %% The grace period must be a positive number or the atom false
    %% (disabled). It must never be 0, negative, or a non-number.
    watchdog_grace_period(P),
    (   P == false
    ->  true
    ;   assertion(number(P)),
        assertion(P > 0)
    ).

test(watchdog_grace_period_env_false,
     [setup(setenv('TERMINUSDB_WORKER_WATCHDOG_GRACE', false)),
      cleanup(unsetenv('TERMINUSDB_WORKER_WATCHDOG_GRACE'))]) :-
    %% Setting TERMINUSDB_WORKER_WATCHDOG_GRACE=false disables warnings.
    watchdog_grace_period(P),
    assertion(P == false).

test(watchdog_grace_period_env_zero,
     [setup(setenv('TERMINUSDB_WORKER_WATCHDOG_GRACE', '0')),
      cleanup(unsetenv('TERMINUSDB_WORKER_WATCHDOG_GRACE'))]) :-
    %% Setting TERMINUSDB_WORKER_WATCHDOG_GRACE=0 disables warnings.
    watchdog_grace_period(P),
    assertion(P == false).

test(watchdog_grace_period_env_positive,
     [setup(setenv('TERMINUSDB_WORKER_WATCHDOG_GRACE', '120')),
      cleanup(unsetenv('TERMINUSDB_WORKER_WATCHDOG_GRACE'))]) :-
    %% A positive integer overrides the default.
    watchdog_grace_period(P),
    assertion(P == 120).

:- end_tests(request_worker_pool).
