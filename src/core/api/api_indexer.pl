:- module(api_indexer, [
              api_start_job/3,
              api_check_job/2,
              %api_index/5,
              %api_query/5,
              api_index_jobs/8,
              io_push_delta/4,
              io_index_branch/3,
              descriptor_graphspec/2,
              io_await_task_completion/2,
              build_last_indexed_url/4,
              validate_index_path/1,
              encode_query_value/2
          ]).

:- use_module(core(document/history),[commits_changed_id/5]).
:- use_module(core(document),[get_document/3, all_class_frames/3,
                              schema_metadata_descriptor/3]).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(transaction/ref_entity),
             [branch_head_commit/3, commit_id_uri/3,
              commit_uri_to_history_commit_ids/3]).
:- use_module(core(util)).
:- use_module(core(account)).
:- use_module(library(http/json)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_stream)).
:- use_module(config(terminus_config)).
:- use_module(core(api/api_graphql)).
:- use_module(core(triple), [super_user_authority/1]).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).
:- use_module(library(yall)).
:- use_module(library(lists)).
:- use_module(library(dicts)).
:- use_module(library(url), [www_form_encode/2]).

% api_start_job(+Domain:string,+Commit:string,-Task_id, +Options) is det.
% Legacy pull trigger. Only active under the http_vectorlink backend; refuses
% loud if the selector is not http_vectorlink so the legacy and push paths can
% never both run (RISK-17 / Spec 16 §2.3).
api_start_job(Domain, Commit, Task_Id) :-
    do_or_die(config:indexer_backend(http_vectorlink),
              error(indexer_backend_not_vectorlink(api_start_job), _)),
    config:semantic_indexer_endpoint(Endpoint),
    http_get(
        [ host(Endpoint),
          path('/start'),
          search([ domain=Domain,
                   commit=Commit])],
        Task_Id,
        []).

api_check_job(Task_Id, Status) :-
    do_or_die(config:indexer_backend(http_vectorlink),
              error(indexer_backend_not_vectorlink(api_check_job), _)),
    config:semantic_indexer_endpoint(Endpoint),
    http_get(
        [ host(Endpoint),
          path('/check'),
          search([ task_id=Task_Id ])],
        Status,
        []).

embedding_type_queries(Commit_Descriptor, TypeQueries) :-
    open_descriptor(Commit_Descriptor, Transaction),
    database_schema(Transaction, Schema),
    findall(
        Type-Query-Template,
        (   xrdf(Schema, Type, sys:metadata, _),
            schema_metadata_descriptor(Schema, Type, metadata(Metadata)),
            get_dict(embedding, Metadata, Embedding),
            get_dict(query, Embedding, Query),
            (   get_dict(template, Embedding, Template)
            ->  true
            ;   Template = none)),
        TypeQueries
    ).

api_indexable(some(Previous_Commit_Id), Descriptor, Commit_Id, Type, Operation) :-
    commits_changed_id(Descriptor, Previous_Commit_Id, Commit_Id, Id,
                       options{ type: Type }),
    resolve_relative_descriptor(Descriptor,
                                ["commit", Commit_Id],
                                After_Commit_Descriptor),
    resolve_relative_descriptor(Descriptor,
                                ["commit", Previous_Commit_Id],
                                Before_Commit_Descriptor),

    (   ask(After_Commit_Descriptor,
            t(Id, rdf:type, _))
    ->  (   ask(Before_Commit_Descriptor,
                t(Id, rdf:type, _))
        ->  Operation = json{ op: 'Changed',
                              id: Id }
        ;   Operation = json{ op: 'Inserted',
                              id: Id }
        )
    ;   Operation = json{ op: 'Deleted',
                          id: Id }
    ).
api_indexable(none, Descriptor, Commit_Id, Type, Operation) :-
    resolve_relative_descriptor(Descriptor,
                                ["commit", Commit_Id],
                                Commit_Descriptor),
    ask(Commit_Descriptor, t(Id, rdf:type, Type),[compress_prefixes(false)]),
    Operation = json{ op: 'Inserted',
                      id: Id }.

/* predicate which returns the various jobs as op/string:
{ "op" : "Inserted", "id" : "Doc/1", "string" : "this is in doc 1" }
{ "op" : "Changed", "id" : "Doc/2", "string" : "this is new in doc 2" }
{ "op" : "Deleted", "id" : "Doc/3"}
*/
:- meta_predicate api_index_jobs(+, +, +, 1, +, +, +, +).
api_index_jobs(System_DB, Auth, Stream, Prelude, Path, Commit_Id, Maybe_Previous_Commit_Id, _Options) :-
    do_or_die(
        is_super_user(Auth),
        error(indexing_requires_superuser, _)
    ),
    resolve_absolute_string_descriptor(Path, Descriptor),
    resolve_relative_descriptor(Descriptor,
                                ["commit", Commit_Id],
                                Commit_Descriptor),
    embedding_type_queries(Commit_Descriptor, TypeQueries),
    maplist([Type-Query-_Template, Type-Query]>>true,
            TypeQueries,
            Queries),
    convlist([Type-Query-Template, Type-Template]>>ground(Template),
             TypeQueries,
             Templates),
    % '$handlebars':handlebars_context(Templates, Handlebars),
    open_descriptor(Commit_Descriptor, Transaction),
    all_class_frames(Transaction, Frames, [compress_ids(true),expand_abstract(true),simple(true)]),
    '$embedding':embedding_context(System_DB, Transaction, Templates, Queries, Frames, Embedding_Context),
    call(Prelude,Stream),
    forall(
        (   member(Type-_Query-_Template, TypeQueries),
            api_indexable(Maybe_Previous_Commit_Id, Descriptor, Commit_Id,
                          Type, Operation)),
        (   get_dict(op, Operation, Op),
            ignore(get_dict(id, Operation, Id)),
            '$embedding':write_op_for(Stream, System_DB, Transaction, Embedding_Context, Type, Id, Op)
        )
    ).

% ==========================================================================
% Phase 6 T3 — Push driver (Capability A)
%
% TerminusDB DRIVES incremental indexing INTO the tdb-search engine.
% Protocol: GET /last-indexed -> compute delta -> POST /push (NDJSON stream).
% Gated on indexer_backend(http_tdb_search). Uses the T1-proven chunked
% streaming mechanism (http:post_data_hook/3 + http_chunked_open/3).
% ==========================================================================

:- multifile http:post_data_hook/3.

% ---- post_data_hook for lazy NDJSON streaming (T1-proven mechanism) ----
% The data term ndjson_push(Goal) carries a goal that accepts a stream arg
% and writes NDJSON lines to it. The hook sends them chunked without
% buffering the body.
http:post_data_hook(ndjson_push(Producer), Out, _HdrExtra) :-
    raw_out_stream(Out, RawOut),
    format(RawOut, "Content-Type: application/x-ndjson\r\n", []),
    format(RawOut, "Transfer-Encoding: chunked\r\n", []),
    format(RawOut, "\r\n", []),
    flush_output(RawOut),
    setup_call_cleanup(
        http_chunked_open(RawOut, Chunked, []),
        call(Producer, Chunked),
        close(Chunked)).

% Resolve a stream pair to its output side (avoids "ambiguous operation on
% stream pair" from http_open's returned pair).
raw_out_stream(Stream, Out) :-
    (   is_stream(Stream),
        stream_pair(Stream, _In, PairOut),
        PairOut \== []
    ->  Out = PairOut
    ;   Out = Stream
    ).

% ---- HTTP Basic auth header for tdb-search calls ----
tdb_search_auth_header(authorization(basic(User, Secret))) :-
    tdb_search_admin_user(User),
    tdb_search_admin_secret(Secret).

/**
 * encode_query_value(+Value, -Encoded) is det.
 *
 * Percent-encodes a value for use as an HTTP query parameter using
 * application/x-www-form-urlencoded rules. This encodes ALL characters
 * that have structural meaning in URLs including '/' (%2f), '&' (%26),
 * '=' (%3d), '+', '?', '#', and space.
 *
 * Uses www_form_encode/2 which produces LOWERCASE hex digits. This is
 * fully RFC 3986 compliant (percent-encoding is case-insensitive per S2.1).
 *
 * Accepts both atoms and strings as input; always produces an atom.
 */
encode_query_value(Value, Encoded) :-
    (   atom(Value)
    ->  Atom = Value
    ;   atom_string(Atom, Value)
    ),
    www_form_encode(Atom, Encoded).

/**
 * build_last_indexed_url(+Endpoint, +Domain, +Branch, -URL) is det.
 *
 * Constructs the GET /last-indexed URL with properly encoded query
 * parameters. Exported for testability (verify encoding of reserved chars
 * in domain/branch strings).
 */
build_last_indexed_url(Endpoint, Domain, Branch, URL) :-
    encode_query_value(Domain, Enc_Domain),
    encode_query_value(Branch, Enc_Branch),
    format(atom(URL), "~w/last-indexed?domain=~w&branch=~w",
           [Endpoint, Enc_Domain, Enc_Branch]).

/**
 * io_get_last_indexed(+Endpoint, +Domain, +Branch, -Result) is det.
 *
 * Calls GET /last-indexed?domain=Domain&branch=Branch on the tdb-search engine.
 * Result is a dict with keys `commit` (atom or null) and `version` (integer).
 * Fails loud on any non-200 response or connection error.
 */
io_get_last_indexed(Endpoint, Domain, Branch, Result) :-
    tdb_search_auth_header(AuthHeader),
    build_last_indexed_url(Endpoint, Domain, Branch, URL),
    setup_call_cleanup(
        http_open(URL, In,
                  [ status_code(Status),
                    AuthHeader,
                    request_header('Accept' = 'application/json')
                  ]),
        ( read_string(In, _, Body_String) ),
        close(In)),
    do_or_die(
        Status =:= 200,
        error(tdb_search_last_indexed_failed(Status, Body_String), _)),
    atom_json_dict(Body_String, Result, [default_tag(json)]).

/**
 * io_stream_push(+Endpoint, +Domain, +Branch, +Target_Commit,
 *                +Parent_Commit_Or_Empty, +System_DB, +Auth, +Path,
 *                +Maybe_Previous_Commit_Id, +Commit_Id, -Result) is det.
 *
 * Opens POST /push to the engine and streams the NDJSON delta using the
 * T1-proven chunked mechanism. The op-lines are produced lazily by
 * api_index_jobs/8 writing directly to the chunked HTTP stream.
 * Parent_Commit_Or_Empty is either an atom (the parent commit hash) or
 * the atom `none` (for a full initial index — omit parent_commit param).
 *
 * Result is one of:
 *   - accepted(Task_Id): push accepted, task spawned for async indexing
 *   - conflict_already_pushed: 409, commit is already in-flight or indexed
 */
io_stream_push(Endpoint, Domain, Branch, Target_Commit,
               Parent_Commit_Or_Empty, System_DB, Auth, Path,
               Maybe_Previous_Commit_Id, Commit_Id, Result) :-
    tdb_search_auth_header(AuthHeader),
    build_push_url(Endpoint, Domain, Branch, Target_Commit,
                   Parent_Commit_Or_Empty, URL),
    Producer = [Chunked_Stream]>>(
        api_index_jobs(
            System_DB,
            Auth,
            Chunked_Stream,
            [_S]>>true,
            Path,
            Commit_Id,
            Maybe_Previous_Commit_Id,
            [])
    ),
    setup_call_cleanup(
        http_open(URL, In,
                  [ method(post),
                    post(ndjson_push(Producer)),
                    status_code(Status),
                    AuthHeader
                  ]),
        read_string(In, _, Reply_Body),
        close(In)),
    handle_push_response(Status, Reply_Body, Result).

% 200: push accepted, task spawned.
handle_push_response(200, Task_Id, accepted(Task_Id)) :- !.
% 409: commit already pushed (in-flight or indexed). NOT a hard failure.
handle_push_response(409, _Body, conflict_already_pushed) :- !.
% Any other status: fail loud.
handle_push_response(Status, Body, _) :-
    throw(error(tdb_search_push_failed(Status, Body), _)).

/**
 * io_await_task_completion(+Endpoint, +Task_Id) is det.
 *
 * Polls GET /check?task_id=Task_Id until the engine reports a TERMINAL
 * state (Complete or Error). Enforces the per-commit-tagged-before-next
 * contract.
 *
 * Poll backoff: 0.1s initial, doubling up to 2s cap. Max 60 iterations.
 */
io_await_task_completion(Endpoint, Task_Id) :-
    io_await_task_completion_(Endpoint, Task_Id, 0.1, 60).

io_await_task_completion_(_Endpoint, Task_Id, _Backoff, 0) :-
    !,
    throw(error(tdb_search_task_poll_timeout(Task_Id), _)).
io_await_task_completion_(Endpoint, Task_Id, Backoff, Retries_Left) :-
    io_check_task(Endpoint, Task_Id, Status),
    (   Status = complete
    ->  true
    ;   Status = error(ErrorMsg)
    ->  throw(error(tdb_search_task_failed(Task_Id, ErrorMsg), _))
    ;   Status = pending
    ->  sleep(Backoff),
        Next_Backoff is min(Backoff * 2, 2.0),
        Next_Retries is Retries_Left - 1,
        io_await_task_completion_(Endpoint, Task_Id, Next_Backoff, Next_Retries)
    ;   throw(error(tdb_search_task_unknown_status(Task_Id, Status), _))
    ).

/**
 * io_check_task(+Endpoint, +Task_Id, -Status) is det.
 *
 * Calls GET /check?task_id=Task_Id on the engine. Returns:
 *   - complete / pending / error(Msg)
 */
io_check_task(Endpoint, Task_Id, Status) :-
    tdb_search_auth_header(AuthHeader),
    encode_query_value(Task_Id, Enc_Task_Id),
    format(atom(URL), "~w/check?task_id=~w", [Endpoint, Enc_Task_Id]),
    setup_call_cleanup(
        http_open(URL, In,
                  [ status_code(Http_Status),
                    AuthHeader,
                    request_header('Accept' = 'application/json')
                  ]),
        read_string(In, _, Body_String),
        close(In)),
    interpret_check_response(Http_Status, Body_String, Status).

interpret_check_response(200, Body_String, Status) :-
    !,
    atom_json_dict(Body_String, Dict, [default_tag(json)]),
    get_dict(status, Dict, Status_Tag_Raw),
    % atom_json_dict may yield atoms OR strings for JSON string values
    % depending on SWI-Prolog version. Normalise to atom for comparison.
    (   atom(Status_Tag_Raw)
    ->  Status_Tag = Status_Tag_Raw
    ;   atom_string(Status_Tag, Status_Tag_Raw)
    ),
    (   Status_Tag == 'Complete'
    ->  Status = complete
    ;   Status_Tag == 'Pending'
    ->  Status = pending
    ;   throw(error(tdb_search_check_unexpected_status(Status_Tag, Body_String), _))
    ).
interpret_check_response(500, Body_String, error(Body_String)) :- !.
interpret_check_response(404, Body_String, error(Body_String)) :- !.
interpret_check_response(Other_Status, Body_String, _) :-
    throw(error(tdb_search_check_failed(Other_Status, Body_String), _)).

/**
 * io_resolve_409(+Endpoint, +Domain, +Branch, +Commit) is det.
 *
 * Called when a push returns 409. Polls /last-indexed until the commit
 * appears as indexed (handles both in-flight and already-indexed cases).
 */
io_resolve_409(Endpoint, Domain, Branch, Commit) :-
    io_poll_until_indexed(Endpoint, Domain, Branch, Commit, 0.2, 30).

io_poll_until_indexed(_Endpoint, _Domain, _Branch, _Commit, _Backoff, 0) :-
    !,
    throw(error(tdb_search_409_resolution_timeout, _)).
io_poll_until_indexed(Endpoint, Domain, Branch, Commit, Backoff, Retries) :-
    io_get_last_indexed(Endpoint, Domain, Branch, Result),
    get_dict(commit, Result, Engine_Commit_Raw),
    normalise_commit_value(Engine_Commit_Raw, Engine_Commit),
    (   Engine_Commit \== null,
        Engine_Commit == Commit
    ->  true
    ;   sleep(Backoff),
        Next_Backoff is min(Backoff * 2, 2.0),
        Next_Retries is Retries - 1,
        io_poll_until_indexed(Endpoint, Domain, Branch, Commit,
                              Next_Backoff, Next_Retries)
    ).

/**
 * io_handle_push_result(+Endpoint, +Domain, +Branch, +Commit, +Result) is det.
 *
 * Processes the result from io_stream_push:
 *   - accepted(Task_Id): await task completion via /check polling
 *   - conflict_already_pushed: resolve via /last-indexed polling (resume-409)
 */
io_handle_push_result(Endpoint, _Domain, _Branch, _Commit, accepted(Task_Id)) :-
    !,
    io_await_task_completion(Endpoint, Task_Id).
io_handle_push_result(Endpoint, Domain, Branch, Commit, conflict_already_pushed) :-
    !,
    io_resolve_409(Endpoint, Domain, Branch, Commit).

% Build the POST /push URL with query parameters.
% All values are percent-encoded via encode_query_value (www_form_encode)
% to prevent parameter injection and ensure '/' in domain paths becomes %2f.
build_push_url(Endpoint, Domain, Branch, Target_Commit, none, URL) :-
    !,
    encode_query_value(Domain, Enc_Domain),
    encode_query_value(Branch, Enc_Branch),
    encode_query_value(Target_Commit, Enc_Target),
    format(atom(URL),
           "~w/push?domain=~w&branch=~w&target_commit=~w",
           [Endpoint, Enc_Domain, Enc_Branch, Enc_Target]).
build_push_url(Endpoint, Domain, Branch, Target_Commit, Parent_Commit, URL) :-
    encode_query_value(Domain, Enc_Domain),
    encode_query_value(Branch, Enc_Branch),
    encode_query_value(Target_Commit, Enc_Target),
    encode_query_value(Parent_Commit, Enc_Parent),
    format(atom(URL),
           "~w/push?domain=~w&branch=~w&target_commit=~w&parent_commit=~w",
           [Endpoint, Enc_Domain, Enc_Branch, Enc_Target, Enc_Parent]).

/**
 * validate_index_path(+Path) is det.
 *
 * Validates that Path conforms to the push driver's input contract.
 * The driver accepts ONLY these forms:
 *
 *   - 2 segments: "org/db" (shorthand for org/db/local/branch/main)
 *   - 5 segments: "org/db/<repo>/branch/<branch>"
 *                 or "org/db/<repo>/commit/<commit>"
 *     where segment 4 MUST be exactly "branch" or "commit".
 *
 * Everything else is REJECTED with a clear error BEFORE any descriptor
 * resolution or network I/O. This prevents the Prolog resolver's convenience
 * rules (e.g., 3-seg -> branch/main) from silently accepting malformed paths.
 *
 * Throws: error(invalid_index_path(Path, Reason), _)
 */
validate_index_path(Path) :-
    (   atom(Path)
    ->  atom_string(Path, Path_String)
    ;   Path_String = Path
    ),
    pattern_string_split("/", Path_String, Segments_Unfiltered),
    exclude(=(""), Segments_Unfiltered, Segments),
    length(Segments, N),
    validate_index_segments(N, Segments, Path).

validate_index_segments(2, [_Org, _DB], _Path) :- !.
validate_index_segments(5, [_Org, _DB, _Repo, Seg4, _Name], Path) :-
    !,
    text_to_string(Seg4, Seg4_Str),
    (   Seg4_Str == "branch"
    ->  true
    ;   Seg4_Str == "commit"
    ->  true
    ;   throw(error(invalid_index_path(Path,
                        bad_segment_4(Seg4, expected_branch_or_commit)), _))
    ).
validate_index_segments(N, _Segments, Path) :-
    throw(error(invalid_index_path(Path,
                    wrong_segment_count(N, expected_2_or_5)), _)).

/**
 * io_push_delta(+System_DB, +Auth, +Path, +Branch_Name) is det.
 *
 * The push driver entrypoint. Validates path structure, then gated on
 * indexer_backend(http_tdb_search). Asks the engine for its last-indexed
 * state, resolves the branch HEAD, and pushes each commit individually.
 *
 * Fails loud on:
 *   - invalid path (not 2-or-5 segment form)
 *   - indexer_backend not http_tdb_search (wrong backend)
 *   - engine unreachable or returns non-200 on /last-indexed
 *   - engine rejects the push (4xx/5xx on /push)
 *   - wrong admin secret (401 from engine)
 */
io_push_delta(System_DB, Auth, Path, Branch_Name) :-
    % Validate path structure FIRST — before any descriptor resolution or I/O.
    validate_index_path(Path),
    do_or_die(
        indexer_backend(http_tdb_search),
        error(indexer_backend_not_tdb_search(io_push_delta), _)),
    do_or_die(
        tdb_search_endpoint(Endpoint),
        error(tdb_search_endpoint_not_configured(io_push_delta), _)),
    % Resolve the descriptor — safe now that path structure is validated.
    resolve_absolute_string_descriptor(Path, Descriptor),
    % Derive the full graphspec from the resolved descriptor.
    descriptor_graphspec(Descriptor, Domain),
    % Extract the branch name from the descriptor and verify consistency.
    do_or_die(
        branch_descriptor{branch_name: Descriptor_Branch} :< Descriptor,
        error(push_requires_branch_descriptor(Path), _)),
    do_or_die(
        Descriptor_Branch == Branch_Name,
        error(branch_name_mismatch(Branch_Name, Descriptor_Branch, Path), _)),
    % Ask the engine where it is up to.
    io_get_last_indexed(Endpoint, Domain, Branch_Name, Last_Indexed),
    get_dict(commit, Last_Indexed, Engine_Commit_Raw),
    normalise_commit_value(Engine_Commit_Raw, Engine_Commit_Or_Null),
    % Resolve the branch HEAD commit in TerminusDB.
    Repository_Descriptor = Descriptor.repository_descriptor,
    branch_head_commit(Repository_Descriptor, Branch_Name, Head_Commit_Uri),
    commit_id_uri(Repository_Descriptor, Head_Commit_Id, Head_Commit_Uri),
    % Dispatch based on engine state.
    io_push_delta_(Endpoint, Domain, Branch_Name, Head_Commit_Id,
                   Head_Commit_Uri, Engine_Commit_Or_Null,
                   Repository_Descriptor, System_DB, Auth, Path).

normalise_commit_value(@(null), null) :- !.
normalise_commit_value(null, null) :- !.
normalise_commit_value(Atom, String) :-
    atom(Atom),
    !,
    atom_string(Atom, String).
normalise_commit_value(String, String) :-
    string(String).

% Case 1: engine is already at HEAD — nothing to push.
io_push_delta_(_Endpoint, _Domain, _Branch_Name, Head_Commit_Id,
               _Head_Commit_Uri, Engine_Commit,
               _Repository_Descriptor, _System_DB, _Auth, _Path) :-
    Engine_Commit \== null,
    Engine_Commit == Head_Commit_Id,
    !.

% Case 2: engine has never indexed this branch (commit is null) — full index.
% After the push is accepted, AWAIT completion (per-commit-tagged contract).
io_push_delta_(Endpoint, Domain, Branch_Name, Head_Commit_Id,
               _Head_Commit_Uri, null,
               _Repository_Descriptor, System_DB, Auth, Path) :-
    !,
    io_stream_push(Endpoint, Domain, Branch_Name, Head_Commit_Id,
                   none, System_DB, Auth, Path,
                   none, Head_Commit_Id, Result),
    io_handle_push_result(Endpoint, Domain, Branch_Name, Head_Commit_Id, Result).

% Case 3: engine has a previous commit — per-commit incremental push.
io_push_delta_(Endpoint, Domain, Branch_Name, _Head_Commit_Id,
               Head_Commit_Uri, Engine_Commit,
               Repository_Descriptor, System_DB, Auth, Path) :-
    Engine_Commit \== null,
    commit_uri_to_history_commit_ids(Repository_Descriptor,
                                     Head_Commit_Uri,
                                     History_Oldest_First),
    commits_after(Engine_Commit, History_Oldest_First, Forward_Range),
    do_or_die(
        Forward_Range \== [],
        error(tdb_search_push_no_forward_range(Engine_Commit), _)),
    io_push_commit_chain(Endpoint, Domain, Branch_Name,
                         Engine_Commit, Forward_Range,
                         System_DB, Auth, Path).

/**
 * commits_after(+Last_Commit, +History_Oldest_First, -Forward_Range) is det.
 */
commits_after(Last_Commit, History, Forward_Range) :-
    (   append(_, [Last_Commit | Forward_Range], History)
    ->  true
    ;   throw(error(tdb_search_last_indexed_not_in_history(Last_Commit), _))
    ).

/**
 * io_push_commit_chain(+Endpoint, +Domain, +Branch, +Parent_Commit,
 *                      +Commits, +System_DB, +Auth, +Path) is det.
 *
 * Pushes each commit sequentially, oldest-first. After each push is accepted,
 * awaits task completion before proceeding to the next commit (per-commit-
 * tagged-before-next contract). Handles 409 via resume polling.
 */
io_push_commit_chain(_Endpoint, _Domain, _Branch, _Parent, [],
                     _System_DB, _Auth, _Path) :- !.
io_push_commit_chain(Endpoint, Domain, Branch, Parent_Commit,
                     [Commit | Rest], System_DB, Auth, Path) :-
    io_stream_push(Endpoint, Domain, Branch, Commit,
                   Parent_Commit, System_DB, Auth, Path,
                   some(Parent_Commit), Commit, Result),
    io_handle_push_result(Endpoint, Domain, Branch, Commit, Result),
    io_push_commit_chain(Endpoint, Domain, Branch, Commit,
                         Rest, System_DB, Auth, Path).

/**
 * descriptor_graphspec(+Descriptor, -GraphSpec) is det.
 *
 * Derives the full graphspec string from a resolved TerminusDB descriptor.
 * Uses resolve_absolute_string_descriptor/2 in reverse mode.
 */
descriptor_graphspec(Descriptor, GraphSpec) :-
    resolve_absolute_string_descriptor(GraphSpec, Descriptor).

/**
 * path_to_domain(+Path, -Domain) is det.
 *
 * DEPRECATED — retained for test backward compatibility.
 */
path_to_domain(Path, Domain) :-
    pattern_string_split("/", Path, Segments_Unfiltered),
    exclude(=(""), Segments_Unfiltered, Segments),
    do_or_die(
        Segments = [Org, DB | _],
        error(invalid_path_for_domain(Path), _)),
    format(atom(Domain), "~w/~w", [Org, DB]).

/**
 * io_index_branch(+System_DB, +Auth, +Path) is det.
 *
 * Explicit "index this branch now" entrypoint. Validates path structure,
 * then resolves descriptor and drives a push. Gated on http_tdb_search.
 *
 * The Path MUST conform to the driver input contract (2-seg or 5-seg form).
 * Validation fires BEFORE descriptor resolution or any I/O.
 */
io_index_branch(System_DB, Auth, Path) :-
    % Validate path structure FIRST — before any descriptor resolution or I/O.
    validate_index_path(Path),
    do_or_die(
        indexer_backend(http_tdb_search),
        error(indexer_backend_not_tdb_search(io_index_branch), _)),
    resolve_absolute_string_descriptor(Path, Descriptor),
    do_or_die(
        branch_descriptor{branch_name: Branch_Name} :< Descriptor,
        error(push_requires_branch_descriptor(Path), _)),
    io_push_delta(System_DB, Auth, Path, Branch_Name).
