:- module(tdb_http_handler, [
                  tdb_http_handler/3,
                  rust_handler/2,
                  stream_handler/3
              ]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(strings)).
:- use_module(core(appserver_hooks)).
:- use_module(server(routes/srv_http)).

:- multifile appserver_hooks:appserver_route/3.
:- multifile appserver_hooks:appserver_route/4.
:- multifile appserver_hooks:appserver_stream/3.

:- meta_predicate tdb_http_handler(+, 2, +).

%% tdb_http_handler(+Path, +Handler, +Options) is det.
%%
%%  Drop-in replacement for library(http/http_dispatch):http_handler/3.
%%
%%  It registers the route with the SWI-Prolog HTTP dispatcher so that the
%%  default server backend continues to work unchanged. It also registers the
%%  route with the Rust webserver via appserver_hooks, so that when
%%  TERMINUSDB_SERVER_BACKEND=rust is selected the Rust server can serve the
%%  same route.
%%
%%  Handler is the same closure that is normally passed to http_handler/3,
%%  typically cors_handler(Method, Goal, ExtraOptions). Options is the same
%%  option list that http_handler/3 accepts, with the addition of the option
%%  `tdb_stream` for GET routes that should be streamed through the Rust
%%  streaming infrastructure (e.g. NDJSON document responses).
tdb_http_handler(Path, Handler, Options) :-
    http_dispatch:http_handler(Path, Handler, Options),
    register_rust_routes(Path, Options).

register_rust_routes(Path, Options) :-
    resolve_rust_path(Path, Options, SubPathRust, RustPath),
    (   option(tdb_binary, Options)
    ->  Binary = true
    ;   Binary = false
    ),
    member(methods(Methods), Options),
    member(Method, Methods),
    (   option(tdb_stream, Options),
        Method == get
    ->  assertz(appserver_hooks:appserver_stream(Method, RustPath, tdb_http_handler:stream_handler))
    ;   assertz(appserver_hooks:appserver_route(Method, RustPath, tdb_http_handler:rust_handler))
    ),
    fail.
register_rust_routes(_, _).

%% resolve_rust_path(+Path, +Options, -RustPath) is det.
%%
%%  Convert a SWI-Prolog http_handler path into a Rust/Axiom route string.
%%
%%  - Atoms are passed through; a `prefix` option appends /*path.
%%  - Aliases like api(db/Org/DB) are expanded using http:location and variables
%%    are converted to :name or *name captures.
%%  - Variables in non-prefix subpaths are converted to :name.
%%  - The trailing variable in a prefix subpath is converted to *name.
resolve_rust_path(Path, Options, RustPath) :-
    (   atom(Path)
    ->  resolve_atom_path(Path, Options, RustPath)
    ;   Path =.. [Alias, SubPath]
    ->  http:location(Alias, Base, _),
        subpath_to_rust(SubPath, Options, SubPathRust),
        (   string_length(SubPathRust, 0)
        ->  RustPath = Base
        ;   format(atom(RustPath), '~w/~w', [Base, SubPathRust])
        )
    ;   RustPath = Path
    ).

resolve_atom_path('/', _Options, '/') :- !.
resolve_atom_path(Path, Options, RustPath) :-
    (   option(prefix, Options)
    ->  format(atom(RustPath), '~w/*path', [Path])
    ;   RustPath = Path
    ).

subpath_to_rust(SubPath, Options, RustPath) :-
    subpath_segments(SubPath, Segments),
    split_at_first_variable(Segments, Before, Var, After),
    convert_segments(Before, BeforeRust, 1),
    (   Var == none
    ->  (   option(prefix, Options)
        ->  append(BeforeRust, ['*path'], RustSegments)
        ;   RustSegments = BeforeRust
        )
    ;   After == []
    ->  (   option(prefix, Options)
        ->  append(BeforeRust, ['*path'], RustSegments)
        ;   append(BeforeRust, [':seg1'], RustSegments)
        )
    ;   (   option(prefix, Options)
        ->  fixed_segments_between_variables([Var|After], Fixed),
            append(BeforeRust, [':seg1'], RustSegments1),
            append(RustSegments1, Fixed, RustSegments2),
            append(RustSegments2, ['*path'], RustSegments)
        ;   append(BeforeRust, ['*path'], RustSegments)
        )
    ),
    atomics_to_string(RustSegments, '/', RustPath).

%% split_at_first_variable(+Segments, -Before, -Var, -After) is det.
%%
%%  Split the segment list at the first variable. Before contains only ground
%%  atoms; Var is the first variable; After is the remaining segment list
%%  (empty if Var is the last segment). Var is unified with the atom `none`
%%  when the path contains no variables.
split_at_first_variable(Segments, Before, Var, After) :-
    append(Before, [Var|After], Segments),
    var(Var),
    !,
    \+ segment_contains_variable(Before).
split_at_first_variable(Segments, Segments, none, []).

segment_contains_variable(Segments) :-
    member(Seg, Segments),
    var(Seg),
    !.

%% fixed_segments_between_variables(+After, -Fixed) is det.
%%
%%  After is the segment list starting with the first variable. Extract the
%%  non-variable segments that appear between the first and last variable so
%%  they can be included in the Rust path before the final catch-all.
fixed_segments_between_variables(After, Fixed) :-
    After = [_|Rest],
    (   append(Middle, [_], Rest)
    ->  exclude(var, Middle, Fixed)
    ;   Fixed = []
    ).

%% subpath_segments(+SubPathTerm, -Segments) is det.
%%
%  Split a Prolog subpath term like db/Org/DB into a list of its components.
%  The / operator is right-associative, so db/Org/DB is db/(Org/DB).
subpath_segments(Term, Segments) :-
    (   compound(Term),
        Term = (Left / Right)
    ->  subpath_segments(Left, LeftSegs),
        subpath_segments(Right, RightSegs),
        append(LeftSegs, RightSegs, Segments)
    ;   Segments = [Term]
    ).

convert_segments([], [], _).
convert_segments([Seg|Segs], [RustSeg|RustSegs], N) :-
    (   var(Seg)
    ->  format(atom(RustSeg), ':seg~d', [N]),
        N2 is N + 1
    ;   atom_chars(Seg, ['.'])
    ->  RustSeg = "",
        N2 = N
    ;   RustSeg = Seg,
        N2 = N
    ),
    convert_segments(Segs, RustSegs, N2).

%% rust_handler(+RequestDict, -ResponseDict) is det.
%%
%%  Generic entry point for a Rust-registered route. It converts the request back
%%  to a SWI-Prolog HTTP request list and lets the SWI-Prolog dispatcher run the
%%  handler that was registered with http_dispatch. The CGI-style response is
%%  captured and parsed into a dict for the Rust server.
rust_handler(Request, Response) :-
    catch(
        rust_handler_safe(Request, Response),
        E,
        (   json_log_error_formatted("Rust handler failed: ~q", [E]),
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

rust_handler_safe(Request, Response) :-
    build_swi_request(Request, SWIRequest, BodyStream, MemoryFile),
    setup_call_cleanup(
        true,
        (   capture_http_output(SWIRequest,
                               tdb_http_handler:http_dispatch_with_expansion(SWIRequest),
                               Captured),
            parse_http_response(Captured, Response)
        ),
        (   catch(close(BodyStream), _, true),
            catch(free_memory_file(MemoryFile), _, true)
        )
    ).

%% http_dispatch_with_expansion(+Request) is det.
%%
%%  Run the SWI-Prolog `http:request_expansion/2` hook (used by the native
%%  backend to capture request logging/correlation IDs) and then dispatch
%%  the expanded request. The expansion must run with the CGI stream as
%%  current output, so this is called inside capture_http_output/3.
http_dispatch_with_expansion(Request) :-
    http:request_expansion(Request, Expanded),
    http_dispatch:http_dispatch(Expanded).

%% stream_handler(+RequestDict, +StreamId, -ResponseDict) is det.
%%
%%  Streaming entry point for Rust-registered routes marked with the `tdb_stream`
%%  option. It runs the SWI-Prolog handler, captures the response, and if the
%%  body is an NDJSON payload it returns `body: stream` and pushes the lines to
%%  the Rust stream in a detached thread.
stream_handler(Request, StreamId, Response) :-
    catch(
        stream_handler_safe(Request, StreamId, Response),
        E,
        (   json_log_error_formatted("Rust stream handler failed: ~q", [E]),
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

stream_handler_safe(Request, StreamId, Response) :-
    build_swi_request(Request, SWIRequest, BodyStream, MemoryFile),
    setup_call_cleanup(
        true,
        (   capture_http_output(SWIRequest,
                               tdb_http_handler:http_dispatch_with_expansion(SWIRequest),
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
        (   catch(close(BodyStream), _, true),
            catch(free_memory_file(MemoryFile), _, true)
        )
    ).

ndjson_body(Body) :-
    split_string(Body, "\n", "", Lines),
    subtract(Lines, [""], NonEmpty),
    length(NonEmpty, Len),
    Len > 1.

stream_ndjson_body(Body, StreamId) :-
    catch(
        (   open_string(Body, Stream),
            stream_ndjson_lines(Stream, StreamId),
            close(Stream)
        ),
        Error,
        (   json_log_error_formatted("Stream send failed: ~q", [Error]),
            catch('$appserver':appserver_stream_close(StreamId), _, true)
        )
    ).

stream_ndjson_lines(Stream, StreamId) :-
    read_line_to_string(Stream, Line),
    (   Line == end_of_file
    ->  catch('$appserver':appserver_stream_close(StreamId), _, true)
    ;   (   Line \= ""
        ->  (   '$appserver':appserver_stream_send(StreamId, Line)
            ->  stream_ndjson_lines(Stream, StreamId)
            ;   catch('$appserver':appserver_stream_close(StreamId), _, true)
            )
        ;   stream_ndjson_lines(Stream, StreamId)
        )
    ).

:- begin_tests(tdb_http_handler, [concurrent(false)]).

% subpath_to_rust/3

% Ground, non-prefix subpath stays as-is.
test(subpath_ground_no_prefix) :-
    subpath_to_rust(db, [], "db").

% Ground prefix subpath gets a trailing catch-all.
test(subpath_ground_prefix) :-
    subpath_to_rust(db, [prefix], "db/*path").

% A single trailing variable becomes a named capture in non-prefix mode.
test(subpath_single_variable_no_prefix) :-
    subpath_to_rust(organizations/_Name, [], "organizations/:seg1").

% A single trailing variable becomes a catch-all in prefix mode.
test(subpath_single_variable_prefix) :-
    subpath_to_rust(organizations/_Name, [prefix], "organizations/*path").

% Multiple consecutive variables collapse to a single catch-all in
% non-prefix mode (Axum route conflict avoidance).
test(subpath_multiple_variables_no_prefix) :-
    subpath_to_rust(db/_Org/_DB, [], "db/*path").

% Multiple variables keep the first capture and any fixed segments, then
% collapse the rest into a catch-all in prefix mode.
test(subpath_multiple_variables_prefix) :-
    subpath_to_rust(db/_Org/_DB, [prefix], "db/:seg1/*path").

% Fixed segments between the first and last variable are preserved in prefix mode.
test(subpath_fixed_between_variables_prefix) :-
    subpath_to_rust(organizations/_Name/users/_Role, [prefix], "organizations/:seg1/users/*path").

% The atom '.' (Prolog's empty segment) maps to an empty Rust segment.
test(subpath_dot_empty) :-
    subpath_to_rust('.', [], "").

% resolve_rust_path/3

% Absolute atom paths are passed through, optionally with a catch-all.
test(resolve_atom_no_prefix) :-
    resolve_rust_path('/api/db', [], '/api/db').

test(resolve_atom_prefix) :-
    resolve_rust_path('/api/db', [prefix], '/api/db/*path').

% Alias with a '.' subpath resolves to the base location with a trailing slash,
% matching the way SWI-Prolog registers root aliases.
test(resolve_alias_with_dot, [setup(asserta(http:location(test_api, '/test', []))),
                              cleanup(retractall(http:location(test_api, _, _)))]) :-
    resolve_rust_path(test_api('.'), [], '/test/').

% Alias with a ground subpath appends the subpath.
test(resolve_alias_ground, [setup(asserta(http:location(test_api, '/test', []))),
                            cleanup(retractall(http:location(test_api, _, _)))]) :-
    resolve_rust_path(test_api(db), [], '/test/db').

% Alias with a ground prefix subpath appends a catch-all.
test(resolve_alias_ground_prefix, [setup(asserta(http:location(test_api, '/test', []))),
                                   cleanup(retractall(http:location(test_api, _, _)))]) :-
    resolve_rust_path(test_api(db), [prefix], '/test/db/*path').

% Alias with one variable subpath becomes a named capture.
test(resolve_alias_single_variable, [setup(asserta(http:location(test_api, '/test', []))),
                                     cleanup(retractall(http:location(test_api, _, _)))]) :-
    resolve_rust_path(test_api(organizations/_Name), [], '/test/organizations/:seg1').

% Alias with multiple variable subpaths collapses to a single catch-all.
test(resolve_alias_multiple_variables, [setup(asserta(http:location(test_api, '/test', []))),
                                        cleanup(retractall(http:location(test_api, _, _)))]) :-
    resolve_rust_path(test_api(db/_Org/_DB), [], '/test/db/*path').

% Alias with multiple variable subpaths in prefix mode keeps the first capture
% and collapses the rest into a catch-all.
test(resolve_alias_multiple_variables_prefix, [setup(asserta(http:location(test_api, '/test', []))),
                                               cleanup(retractall(http:location(test_api, _, _)))]) :-
    resolve_rust_path(test_api(db/_Org/_DB), [prefix], '/test/db/:seg1/*path').

% register_rust_routes/2 asserts both a wildcard route and an exact route
% for multi-segment prefix handlers so Axum matches empty tails. The fourth
% argument is the binary flag (false by default).
test(register_routes_includes_exact_for_multi_segment_prefix, [setup(asserta(http:location(test_api, '/test', []))),
                                                             cleanup(retractall(http:location(test_api, _, _)))]) :-
    retractall(appserver_hooks:appserver_route(_, _, _, _)),
    register_rust_routes(test_api(organizations/_Name/users/_Rest), [methods([get]), prefix]),
    findall(P, appserver_hooks:appserver_route(get, P, _, _), Paths),
    sort(Paths, Sorted),
    maplist(atom_string, Sorted, SortedStrings),
    assertion(SortedStrings == ["/test/organizations/:seg1/users", "/test/organizations/:seg1/users/*path"]).

% Single-segment prefix handlers get an exact route WITH a trailing slash for
% the empty tail (e.g., /test/optimize/), so GET /test/optimize/ returns 405
% instead of falling through to the parent 404.
test(register_routes_includes_exact_slash_for_single_segment_prefix, [setup(asserta(http:location(test_api, '/test', []))),
                                                                  cleanup(retractall(http:location(test_api, _, _)))]) :-
    retractall(appserver_hooks:appserver_route(_, _, _, _)),
    register_rust_routes(test_api(branch/_Path), [methods([get, post]), prefix]),
    findall(P, appserver_hooks:appserver_route(get, P, _, _), Paths),
    sort(Paths, Sorted),
    maplist(atom_string, Sorted, SortedStrings),
    assertion(SortedStrings == ["/test/branch/", "/test/branch/*path"]).

% The tdb_binary option is propagated to the fourth argument of appserver_route/4.
test(register_routes_binary_flag, [setup(asserta(http:location(test_api, '/test', []))),
                                    cleanup(retractall(http:location(test_api, _, _)))]) :-
    retractall(appserver_hooks:appserver_route(_, _, _, _)),
    register_rust_routes(test_api(blob), [methods([get]), tdb_binary]),
    findall(P, appserver_hooks:appserver_route(get, P, _, true), Paths),
    sort(Paths, Sorted),
    maplist(atom_string, Sorted, SortedStrings),
    assertion(SortedStrings == ["/test/blob"]).

:- end_tests(tdb_http_handler).
