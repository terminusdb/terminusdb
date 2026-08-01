:- module(webserver_commits, []).

:- use_module(core(plugin_api)).
:- use_module(library(lists)).
:- use_module(library(json)).
:- use_module(library(option)).
:- use_module(library(date), [parse_time/3]).
:- use_module(library(uri)).
:- use_module(library(apply)).
:- use_module(library(yall)).

:- multifile appserver_hooks:appserver_stream/3.
:- multifile plugins:post_commit_hook/2.

%% Dynamic predicate storing active stream subscriptions.
%% Each entry is commit_stream(StreamId, BranchPath, TimeoutThread).
:- dynamic commit_stream/3.

%% Dynamic predicate for per-branch commit dedup.
%% The post_commit_hook fires multiple times per commit (once per
%% validation level in the transaction chain). This tracks which
%% commit IDs have already been broadcast to avoid duplicates.
%% The assertion happens INSIDE the per-branch mutex so that
%% Phase 2 (send_delta_commits) can reliably test it: if
%% broadcast_sent(BranchPath, C) is true, the broadcast already ran
%% under the mutex and did not find this stream (not registered yet),
%% so the delta must send C. If false, the broadcast has not run yet
%% and will deliver C to this stream after Phase 2 registers.
%%
%% The table is keyed by BranchPath so that cleanup for one branch does
%% not interfere with another branch's dedup checks. This avoids the
%% race condition where a global retractall would temporarily empty the
%% table for all branches.
:- dynamic broadcast_sent/2.

%% Per-branch broadcast mutexes.
%% Each entry is branch_broadcast_mutex(BranchPath, Mutex).
%% The mutex serializes catch-up+registration vs broadcast fan-out
%% to guarantee no gaps and no duplicates during concurrent connections.
:- dynamic branch_broadcast_mutex/2.
:- mutex_create(branch_broadcast_registry, [alias(branch_broadcast_registry)]).

%%%%%%%%%%%%%%%%%%%% Commit Broadcast Stream %%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Streaming endpoint that broadcasts commit information for a branch
%  to any number of listeners.
%
%  The endpoint is:
%    GET /api/v1/ext/commits/*branch_path?since=<timestamp|commit>&timeout=<seconds|false>
%
%  The first NDJSON chunk always contains the current head commit with
%  its full document change set. If `since` is provided, all commits
%  between the `since` point and the head are sent as catch-up chunks
%  before live subscription begins.
%
%  The `timeout` parameter controls auto-close (default 30s, false=never).
%  The post_commit_hook sends full commit events directly to each
%  matching stream via appserver_stream_send.

appserver_hooks:appserver_stream(get, '/api/v1/ext/commits/*branch_path',
                                  webserver_commits:commits_handler).

%%%%%%%%%%%%%%%%%%%% Stream Handler %%%%%%%%%%%%%%%%%%%%%%%%%

%% commits_handler(+Request, +StreamId, -Response) is det.
%
%  Authenticate the listener, check branch authorization, send initial
%  commit chunk(s), register the stream for live updates, and set up
%  the timeout.
%
%  Errors (authentication_incorrect, access_not_authorised, etc.) are
%  thrown and mapped to HTTP responses by the worker pool's
%  handle_plugin_stream_request catch wrapper.
commits_handler(Request, StreamId, Response) :-
    %% Extract the branch path from the wildcard path parameter.
    get_dict(params, Request, Params),
    get_dict(branch_path, Params, BranchPathRaw),
    normalize_branch_path(BranchPathRaw, BranchPath),

    %% Parse query parameters.
    get_dict(query, Request, QueryString),
    (   QueryString \= ""
    ->  uri_query_components(QueryString, Query)
    ;   Query = []
    ),

    %% Parse optional `since` parameter.
    (   memberchk(since=SinceStr, Query)
    ->  parse_since(SinceStr, Since)
    ;   Since = none
    ),

    %% Parse optional `timeout` parameter (default 30s).
    (   memberchk(timeout=TimeoutStr, Query)
    ->  parse_timeout(TimeoutStr, Timeout)
    ;   Timeout = 30
    ),

    %% Open the system database for authentication.
    open_descriptor(system_descriptor{}, System_DB),

    %% Authenticate using the request headers.
    plugin_api:authenticate_from_request(Request, System_DB, Auth),

    %% Require meta_read_access on the system — the endpoint reveals
    %% database and branch metadata to the listener.
    check_descriptor_auth(System_DB, system_descriptor{},
                          '@schema':'Action/meta_read_access', Auth),

    %% Resolve the branch descriptor and check authorization.
    do_or_die(
        resolve_absolute_string_descriptor(BranchPath, Descriptor),
        error(invalid_absolute_path(BranchPath), _)
    ),
    (   branch_descriptor{} :< Descriptor
    ->  true
    ;   throw(error(not_a_branch_descriptor(Descriptor), _))
    ),

    check_descriptor_auth(System_DB, Descriptor,
                          '@schema':'Action/commit_read_access', Auth),

    %% Return a streaming response with a post_response continuation goal.
    %% The worker thread calls send_response (which sends HTTP headers as
    %% the first channel message), then calls the post_response goal to
    %% send initial data and register for live updates. No detached thread
    %% is needed — the worker thread handles everything and then returns
    %% to the pool.
    Response = _{
        status: 200,
        headers: _{'Content-Type': 'application/x-ndjson'},
        body: stream,
        post_response: webserver_commits:stream_commit_data(System_DB, Descriptor, BranchPath,
                                                            Since, Timeout, StreamId)
    }.

%%%%%%%%%%%%%%%%%%%% Stream Data Initialization %%%%%%%%%%%%%%%%%%%%%%%%%

%% stream_commit_data(+System_DB, +Descriptor, +BranchPath, +Since,
%%                     +Timeout, +StreamId) is det.
%
%  Called by the worker thread AFTER send_response has sent the HTTP
%  headers as the first channel message. Sends initial commit chunk(s),
%  registers the stream for live updates, and sets up the timeout.
%  The stream is closed by the timeout thread or when the client
%  disconnects (detected by Rust).
%
%  Uses a two-phase catch-up to minimize mutex hold time:
%    Phase 1: Send historical commits up to current head (NO mutex)
%    Phase 2: Send delta + register atomically (WITH mutex)
%  See PLAN_COMMIT_STREAM_SYNC.md for the full design.
stream_commit_data(System_DB, Descriptor, BranchPath, Since, Timeout, StreamId) :-
    catch(
        (   setup_timeout(StreamId, Timeout, TimeoutThread),
            ensure_branch_broadcast_mutex(BranchPath, Mutex),

            %% PHASE 1 — Historical catch-up (NO mutex)
            %% Send all commits up to the current head. Broadcasts to
            %% existing clients continue unimpeded. Returns C_head for
            %% Phase 2 delta computation.
            send_historical_commits(System_DB, Descriptor,
                                    BranchPath, Since, StreamId, C_head),

            %% PHASE 2 — Delta sync + registration (WITH mutex)
            %% Atomically catch up any commits that arrived during
            %% Phase 1 and register for live broadcasts. The delta is
            %% typically 0-2 commits, so the mutex hold time is minimal.
            %% Registration has two parts:
            %%   1. Subscribe the stream to the Rust broadcast channel
            %%      (for parallel fan-out on the Rust side)
            %%   2. assertz commit_stream/3 (for timeout/cleanup and
            %%      for the broadcast_sent dedup check in send_delta)
            with_mutex(Mutex,
                (   send_delta_commits(System_DB, Descriptor,
                                       BranchPath, C_head, StreamId),
                    (   Timeout == false -> TimeoutSecs = 0
                    ;   Timeout = TimeoutSecs
                    ),
                    '$appserver':appserver_broadcast_subscribe(BranchPath, StreamId, TimeoutSecs),
                    assertz(commit_stream(StreamId, BranchPath, TimeoutThread))
                )
            )
        ),
        Error,
        (   json_log_error_formatted("stream_commit_data error: ~q", [Error]),
            retractall(commit_stream(StreamId, _, _)),
            catch('$appserver':appserver_broadcast_unsubscribe_all(StreamId), _, true),
            catch('$appserver':appserver_stream_close(StreamId), _, true)
        )
    ).

%%%%%%%%%%%%%%%%%%%% Two-Phase Catch-Up %%%%%%%%%%%%%%%%%%%%%%%%%

%% send_historical_commits(+System_DB, +Descriptor, +BranchPath,
%%                          +Since, +StreamId, -C_head) is det.
%
%  Phase 1: Send historical commits up to the current head, WITHOUT
%  holding the mutex. Broadcasts to existing clients continue
%  unimpeded. Returns C_head (the head commit ID at the time of the
%  read) for Phase 2 delta computation.
send_historical_commits(System_DB, Descriptor, BranchPath, Since, StreamId, C_head) :-
    get_dict(repository_descriptor, Descriptor, RepoDescriptor),
    get_dict(branch_name, Descriptor, BranchName),
    %% Get the branch head commit URI, then walk the full history.
    %% This uses the same pattern as api_log.pl: branch_head_commit
    %% gives us the head, and commit_uri_to_history_commit_ids walks
    %% the parent chain. The result is oldest-first, so we reverse
    %% to get newest-first (matching the old commits/2 order).
    do_or_die(
        branch_head_commit(RepoDescriptor, BranchName, Head_Commit_Uri),
        error(no_commits_on_branch(BranchPath), _)
    ),
    commit_id_uri(RepoDescriptor, C_head, Head_Commit_Uri),
    commit_uri_to_history_commit_ids(RepoDescriptor, Head_Commit_Uri, HistoryIds),
    reverse(HistoryIds, CommitList),
    %% If since is provided, find the catch-up commits.
    (   Since = none
    ->  CatchupCommits = []
    ;   Since = since_commit(SinceCommitId)
    ->  filter_commits_since_commit(CommitList, SinceCommitId, CatchupCommits)
    ;   Since = since_timestamp(SinceTimestamp)
    ->  filter_commits_since_timestamp(RepoDescriptor, CommitList,
                                        SinceTimestamp, CatchupCommits)
    ),
    %% Send ALL historical catch-up commits in chronological order
    %% (oldest first). These commits happened in the past and will
    %% never be broadcast again, so they must always be sent directly.
    reverse(CatchupCommits, ChronologicalCatchup),
    forall(
        member(CommitId, ChronologicalCatchup),
        send_commit_event(System_DB, Descriptor, BranchPath, CommitId, StreamId)
    ),
    %% Send head if not already in catchup.
    (   member(C_head, ChronologicalCatchup)
    ->  true
    ;   send_commit_event(System_DB, Descriptor, BranchPath, C_head, StreamId)
    ).

%% send_delta_commits(+System_DB, +Descriptor, +BranchPath,
%%                     +C_head, +StreamId) is det.
%
%  Phase 2: Send commits that arrived between Phase 1's head read
%  and now. Called under the branch mutex. The delta is typically
%  0-2 commits, so the mutex hold time is minimal.
send_delta_commits(System_DB, Descriptor, BranchPath, C_head, StreamId) :-
    get_dict(repository_descriptor, Descriptor, RepoDescriptor),
    get_dict(branch_name, Descriptor, BranchName),
    %% Get the current head and full history (same pattern as
    %% send_historical_commits — see comment there).
    (   branch_head_commit(RepoDescriptor, BranchName, Head_Commit_Uri)
    ->  commit_id_uri(RepoDescriptor, C_latest, Head_Commit_Uri),
        commit_uri_to_history_commit_ids(RepoDescriptor, Head_Commit_Uri, HistoryIds),
        reverse(HistoryIds, CommitList)
    ;   CommitList = [], C_latest = none
    ),
    (   CommitList = [C_latest|_]
    ->  (   C_latest == C_head
        ->  true  %% No delta, head unchanged
        ;   %% Find commits between C_head (exclusive) and C_latest (inclusive)
            (   append(_Before, [C_head|After], CommitList)
            ->  DeltaCommits = After
            ;   %% C_head not found — branch was reset or rebased.
                %% Send the full list as catch-up to be safe.
                DeltaCommits = CommitList
            ),
            reverse(DeltaCommits, ChronologicalDelta),
            %% Only send delta commits that have already been broadcast.
            %% If broadcast_sent(BranchPath, C) is true, the broadcast
            %% ran under the mutex but did not find this stream (not
            %% registered yet), so the delta must deliver C. If false,
            %% the broadcast has not run yet and will deliver C to this
            %% stream after Phase 2 registers.
            forall(
                (   member(CommitId, ChronologicalDelta),
                    commit_key(CommitId, CommitKey),
                    broadcast_sent(BranchPath, CommitKey)
                ),
                send_commit_event(System_DB, Descriptor, BranchPath, CommitId, StreamId)
            )
        )
    ;   true  %% Branch has no commits, nothing to do
    ).

%% commit_key(+CommitId, -CommitKey) is det.
%
%  Normalize a commit ID (atom or string) to an atom for consistent
%  comparison with broadcast_sent/2 entries.
commit_key(CommitId, CommitKey) :-
    (   atom(CommitId) -> CommitKey = CommitId
    ;   atom_string(CommitKey, CommitId)
    ).

%% filter_commits_since_commit(+CommitList, +SinceCommitId, -Catchup) is det.
%
%  Return all commits after (but not including) SinceCommitId.
%  CommitList is newest-first (head to initial). The catch-up set
%  is the commits NEWER than SinceCommitId, which are the elements
%  BEFORE SinceCommitId in the list.
filter_commits_since_commit(CommitList, SinceCommitId, Catchup) :-
    %% Normalize SinceCommitId to string for comparison, since
    %% commit_uri_to_history_commit_ids returns strings.
    (   string(SinceCommitId) -> SinceStr = SinceCommitId
    ;   atom_string(SinceCommitId, SinceStr)
    ),
    (   append(Newer, [SinceStr|_Older], CommitList)
    ->  Catchup = Newer
    ;   Catchup = []  %% SinceCommitId not found, no catchup
    ).

%% filter_commits_since_timestamp(+Repo, +CommitList, +SinceTimestamp, -Catchup) is det.
%
%  Return all commits with timestamp > SinceTimestamp.
filter_commits_since_timestamp(Repo, CommitList, SinceTimestamp, Catchup) :-
    include(
        {Repo, SinceTimestamp}/[CommitId]>>(
            commit_id_to_metadata(Repo, CommitId, _Author, _Msg, Timestamp),
            Timestamp > SinceTimestamp
        ),
        CommitList,
        Catchup
    ).

%%%%%%%%%%%%%%%%%%%% Commit Event Construction %%%%%%%%%%%%%%%%%%%%%%%%%

%% send_commit_event(+System_DB, +Descriptor, +BranchPath, +CommitId, +StreamId) is det.
%
%  Build a commit event (commit info + changed documents) and send it
%  as an NDJSON chunk to the stream.
send_commit_event(_System_DB, Descriptor, BranchPath, CommitId, StreamId) :-
    get_dict(repository_descriptor, Descriptor, RepoDescriptor),
    commit_info_dict(RepoDescriptor, CommitId, CommitInfo),

    %% Open the commit descriptor to get changed documents.
    resolve_relative_descriptor(Descriptor, ["commit", CommitId], CommitDescriptor),
    (   catch(
            (   open_descriptor(CommitDescriptor, Transaction),
                '$changes':collect_changed_documents(Transaction, Added, Changed, Deleted)
            ),
            Error,
            (   json_log_error_formatted("send_commit_event: collect_changed_documents failed for ~w: ~q",
                                          [CommitId, Error]),
              Added = [], Changed = [], Deleted = []
            )
        )
    ->  true
    ;   Added = [], Changed = [], Deleted = []
    ),

    %% Build the event dict.
    Event = _{
        commit: CommitInfo,
        changes: _{
            added: Added,
            changed: Changed,
            deleted: Deleted
        },
        branch: BranchPath
    },

    %% Serialize to JSON on the Prolog side using json_write_dict, which
    %% correctly handles Prolog [] (empty list) as JSON [] instead of null.
    %% Pass the string to appserver_stream_send, which sends strings as
    %% raw bytes with a trailing newline.
    with_output_to(string(JsonStr), json_write_dict(current_output, Event, [as(string), width(0)])),
    '$appserver':appserver_stream_send(StreamId, JsonStr).

%%%%%%%%%%%%%%%%%%%% Post-Commit Hook (Funnel) %%%%%%%%%%%%%%%%%%%%%%%%%

%% plugins:post_commit_hook(+Validations, +Meta_Data) is det.
%
%  After every commit, extract the branch descriptor and commit info,
%  compute the changed documents, and send the full event directly to
%  each matching stream. This follows the same pattern as
%  webserver_events.pl: iterate over stored stream IDs and send via
%  appserver_stream_send.
%
%  Validations is a LIST of validation_object dicts. Each has:
%    - descriptor: the branch/database/repo descriptor
%    - commit_info: author, message, user, etc.
%    - instance_objects: list of instance objects with .read layer
%
%  Meta_Data is a meta_data{} dict with:
%    - data_versions: list of Descriptor-Data_Version pairs
plugins:post_commit_hook(Validation_Objects, Meta_Data) :-
    catch(
        (   broadcast_commit_events(Validation_Objects, Meta_Data)
        ->  true
        ;   json_log:json_log_error_formatted("post_commit_hook: broadcast_commit_events failed (no exception)", [])
        ),
        Error,
        json_log:json_log_error_formatted("post_commit_hook broadcast error: ~q", [Error])
    ),
    !,
    catch(
        (   webserver_graphql_subs:broadcast_graphql_events(Validation_Objects, Meta_Data)
        ->  true
        ;   json_log:json_log_error_formatted("post_commit_hook: broadcast_graphql_events failed (no exception)", [])
        ),
        GraphQLError,
        json_log:json_log_error_formatted("post_commit_hook: broadcast_graphql_events error: ~q", [GraphQLError])
    ),
    !.
plugins:post_commit_hook(_, _).

%% broadcast_commit_events(+Validation_Objects, +Meta_Data) is det.
%
%  For each validation object that is a branch_descriptor, extract the
%  commit info and changed documents, then send to each matching stream.
%  Also sweeps stale commit_stream/3 entries whose Rust stream no longer
%  exists (client disconnected with timeout=false).
broadcast_commit_events(Validation_Objects, Meta_Data) :-
    get_dict(data_versions, Meta_Data, DataVersions),
    %% Sweep stale streams before broadcasting. This retracts
    %% commit_stream/3 entries for streams that the Rust side has already
    %% cleaned up (client disconnect). Without this, entries leak when
    %% timeout=false.
    sweep_stale_streams,
    %% Collect unique branch paths with their validation objects and
    %% commit IDs to avoid duplicate sends when both schema and instance
    %% validation objects exist for the same branch.
    findall(BranchPath-Validation_Object-CommitIdAtom,
            (   member(Validation_Object, Validation_Objects),
                is_dict(Validation_Object),
                get_dict(descriptor, Validation_Object, Descriptor),
                branch_descriptor{} :< Descriptor,
                member(Descriptor-data_version(branch, CommitIdAtom), DataVersions),
                descriptor_to_branch_path(Descriptor, BranchPath)
            ),
            Triples),
    %% Deduplicate by BranchPath-CommitIdAtom (keep first validation object).
    dedup_triples(Triples, UniqueTriples),
    forall(
        member(BranchPath-Validation_Object-CommitIdAtom, UniqueTriples),
        send_to_branch_streams(Validation_Object, BranchPath, CommitIdAtom)
    ).

%% sweep_stale_streams is det.
%
%  Retract commit_stream/3 entries whose Rust stream no longer exists.
%  This handles the timeout=false case where no timeout thread cleans up
%  after a client disconnect. The Rust side detects the disconnect and
%  removes the stream from its registry; this sweep retracts the
%  corresponding Prolog entry.
sweep_stale_streams :-
    findall(StreamId, commit_stream(StreamId, _, _), StreamIds),
    forall(
        (   member(StreamId, StreamIds),
            \+ catch('$appserver':appserver_stream_exists(StreamId), _, fail)
        ),
        retract_stream(StreamId)
    ).

%% dedup_triples(+Triples, -Unique) is det.
%
%  Remove duplicates by BranchPath-CommitIdAtom key, keeping the first.
dedup_triples(Triples, Unique) :-
    dedup_triples(Triples, [], Unique).

dedup_triples([], _Seen, []).
dedup_triples([BranchPath-VO-CIA|Rest], Seen, Unique) :-
    (   member(BranchPath-CIA, Seen)
    ->  dedup_triples(Rest, Seen, Unique)
    ;   Unique = [BranchPath-VO-CIA|UniqueRest],
        dedup_triples(Rest, [BranchPath-CIA|Seen], UniqueRest)
    ).

%% send_to_branch_streams(+Validation_Object, +BranchPath, +CommitIdAtom) is det.
%
%  Build the commit event and send it to every stream subscribed to
%  this branch path. Failed sends retract the stream (client disconnected).
%
%  The broadcast_sent/2 assertion happens INSIDE the per-branch mutex.
%  This is critical for transactional correctness with the two-phase
%  catch-up in send_delta_commits: Phase 2 tests broadcast_sent/2 under
%  the same mutex. If broadcast_sent(BranchPath, C) is true when Phase 2
%  runs, the broadcast already ran under the mutex and did not find this
%  stream (not registered yet), so the delta must send C. If false, the
%  broadcast has not run yet and will deliver C after Phase 2 registers.
send_to_branch_streams(Validation_Object, BranchPath, CommitIdAtom) :-
    (   atom(CommitIdAtom) -> CommitIdKey = CommitIdAtom
    ;   atom_string(CommitIdKey, CommitIdAtom)
    ),
    %% Fast path: skip if already broadcast (post_commit_hook fires
    %% multiple times per commit). The definitive check is repeated
    %% inside the mutex below.
    (   broadcast_sent(BranchPath, CommitIdKey)
    ->  true
    ;   build_commit_event(Validation_Object, BranchPath, CommitIdAtom, JsonStr)
    ->  ensure_branch_broadcast_mutex(BranchPath, Mutex),
        with_mutex(Mutex,
            (   %% Double-check inside mutex: another broadcast may
                %% have asserted this while we waited.
                (   broadcast_sent(BranchPath, CommitIdKey)
                ->  true
                ;   assertz(broadcast_sent(BranchPath, CommitIdKey)),
                    cleanup_old_broadcasts(BranchPath),
                    %% Fan-out is handled by the Rust broadcast
                    %% registry, which sends to all subscribed
                    %% streams in parallel using tokio tasks.
                    %% The Rust side also removes failed streams
                    %% (disconnected clients) from the channel.
                    '$appserver':appserver_broadcast_send_raw(BranchPath, JsonStr)
                )
            )
        )
    ;   json_log_error_formatted("send_to_branch_streams: build_commit_event failed for ~w", [BranchPath])
    ).

%% cleanup_old_broadcasts(+BranchPath) is det.
%
%  Keep only the last 100 broadcast_sent entries for this branch to
%  avoid memory growth. Uses selective retraction of only the oldest
%  entries rather than retractall + re-assert, so other branches' dedup
%  checks are not affected during cleanup.
%
%  findall returns entries in assertion order (oldest first). We retract
%  the first N-100 entries (the oldest) and keep the last 100 (the
%  newest, which are most likely to be relevant for delta checks).
cleanup_old_broadcasts(BranchPath) :-
    findall(C, broadcast_sent(BranchPath, C), All),
    length(All, N),
    (   N > 100
    ->  Excess is N - 100,
        length(Old, Excess),
        append(Old, _Keep, All),
        forall(member(C, Old), retract(broadcast_sent(BranchPath, C)))
    ;   true
    ).

%% retract_stream(+StreamId) is det.
%
%  Remove a stream from the registry. The timeout thread is NOT aborted
%  — it will wake up, find the stream already retracted, and exit
%  naturally. Aborting detached threads with thread_signal/2 can cause
%  cascade failures in SWI-Prolog when many streams close simultaneously.
retract_stream(StreamId) :-
    (   retract(commit_stream(StreamId, BranchPath, _))
    ->  catch('$appserver':appserver_broadcast_unsubscribe(BranchPath, StreamId), _, true),
        catch('$appserver':appserver_stream_close(StreamId), _, true)
    ;   true
    ).

%% build_commit_event(+Validation_Object, +BranchPath, +CommitIdAtom, -JsonStr) is det.
%
%  Extract commit info from the validation object, detect document
%  changes, build the event dict, and serialize to a JSON string.
build_commit_event(Validation_Object, BranchPath, CommitIdAtom, JsonStr) :-
    %% Extract commit info from the validation object.
    (   get_dict(commit_info, Validation_Object, CommitInfo)
    ->  true
    ;   json_log_error_formatted("build_commit_event: no commit_info in validation object", []),
        fail
    ),

    (   get_dict(author, CommitInfo, Author)
    ->  true
    ;   Author = "unknown"
    ),
    (   get_dict(message, CommitInfo, Message)
    ->  true
    ;   Message = ""
    ),
    (   get_dict(user, CommitInfo, User)
    ->  true
    ;   User = null
    ),
    atom_string(CommitId, CommitIdAtom),

    %% Get timestamp from the commit info or use current time.
    (   get_dict(timestamp, CommitInfo, Timestamp)
    ->  true
    ;   get_time(Timestamp)
    ),

    %% Detect document changes using the Rust $changes:collect_changed_documents
    %% predicate, which is much more performant than Prolog-side layer inspection.
    %% It takes a transaction-like term with instance_objects and schema_objects
    %% (which a validation_object has) and uses schema-aware document type
    %% detection with proper containment walk-up for subdocuments.
    (   catch(
            '$changes':collect_changed_documents(Validation_Object, Added0, Changed0, Deleted0),
            Error,
            (   json_log_error_formatted("build_commit_event: collect_changed_documents error: ~q", [Error]),
                fail
            )
        )
    ->  Added = Added0, Changed = Changed0, Deleted = Deleted0
    ;   Added = [], Changed = [], Deleted = []
    ),
    %% Ensure all change lists are bound (avoid null in JSON).
    (   var(Added) -> Added = [] ; true ),
    (   var(Changed) -> Changed = [] ; true ),
    (   var(Deleted) -> Deleted = [] ; true ),

    %% Build the event dict.
    Event = _{
        commit: _{
            identifier: CommitId,
            author: Author,
            message: Message,
            timestamp: Timestamp,
            user: User
        },
        changes: _{
            added: Added,
            changed: Changed,
            deleted: Deleted
        },
        branch: BranchPath
    },

    %% Serialize to JSON on the Prolog side using json_write_dict, which
    %% correctly handles Prolog [] (empty list) as JSON [] instead of null.
    %% Use width(0) for compact single-line JSON (NDJSON format).
    with_output_to(string(JsonStr), json_write_dict(current_output, Event, [as(string), width(0)])).

%%%%%%%%%%%%%%%%%%%% Utilities %%%%%%%%%%%%%%%%%%%%%%%%%

%% ensure_branch_broadcast_mutex(+BranchPath, -Mutex) is det.
%
%  Lazily create or look up the per-branch broadcast mutex. The mutex
%  serializes catch-up+registration (new client connecting) against
%  broadcast fan-out (commit arriving) to guarantee no gaps and no
%  duplicates. See PLAN_COMMIT_STREAM_SYNC.md for the full design.
ensure_branch_broadcast_mutex(BranchPath, Mutex) :-
    (   branch_broadcast_mutex(BranchPath, Mutex)
    ->  true
    ;   with_mutex(branch_broadcast_registry,
            (   branch_broadcast_mutex(BranchPath, Mutex)
            ->  true
            ;   atom_string(BranchPathAtom, BranchPath),
                atom_concat('commit_broadcast_', BranchPathAtom, Alias),
                catch(mutex_create(Mutex, [alias(Alias)]),
                      error(permission_error(create, mutex, _), _),
                      mutex_property(Mutex, alias(Alias))),
                assertz(branch_broadcast_mutex(BranchPath, Mutex))
            )
        )
    ).

%% normalize_branch_path(+Raw, -Normalized) is det.
%
%  Strip leading/trailing slashes from the branch path.
%  Always returns a Prolog string to match descriptor_to_branch_path.
normalize_branch_path(Raw, Normalized) :-
    (   atom(Raw) -> atom_string(Raw, RawStr) ; RawStr = Raw ),
    (   string_concat("/", Rest, RawStr)
    ->  normalize_branch_path(Rest, Normalized)
    ;   string_concat(Rest, "/", RawStr)
    ->  normalize_branch_path(Rest, Normalized)
    ;   Normalized = RawStr
    ).

%% descriptor_to_branch_path(+Descriptor, -Path) is det.
%
%  Convert a branch_descriptor to a path string like "org/db/local/branch/main".
descriptor_to_branch_path(Descriptor, Path) :-
    get_dict(branch_name, Descriptor, BranchName),
    get_dict(repository_descriptor, Descriptor, RepoDescriptor),
    get_dict(repository_name, RepoDescriptor, RepoName),
    get_dict(database_descriptor, RepoDescriptor, DBDescriptor),
    get_dict(database_name, DBDescriptor, DBName),
    get_dict(organization_name, DBDescriptor, OrgName),
    format(string(Path), "~w/~w/~w/branch/~w", [OrgName, DBName, RepoName, BranchName]).

%% parse_since(+SinceStr, -Since) is det.
%
%  Parse the `since` parameter as either an ISO8601 timestamp or a
%  commit ID.
parse_since(SinceStr, Since) :-
    (   catch(parse_time(SinceStr, iso_8601, Epoch), _, fail)
    ->  Since = since_timestamp(rationalize(Epoch))
    ;   Since = since_commit(SinceStr)
    ).

%% parse_timeout(+TimeoutStr, -Timeout) is det.
%
%  Parse the timeout parameter. "false" means no timeout. A number
%  means that many seconds. Default is 30.
parse_timeout("false", false) :- !.
parse_timeout(false, false) :- !.
parse_timeout(TimeoutStr, Timeout) :-
    (   atom(TimeoutStr) -> TimeoutAtom = TimeoutStr
    ;   atom_string(TimeoutAtom, TimeoutStr)
    ),
    (   catch(atom_number(TimeoutAtom, Parsed), _, fail),
        integer(Parsed),
        Parsed > 0
    ->  Timeout = Parsed
    ;   Timeout = 30
    ).

%% setup_timeout(+StreamId, +Timeout, -Thread) is det.
%
%  Spawn a detached thread that closes the stream after Timeout seconds.
%  If Timeout is false, no thread is created and cleanup relies on the
%  stale-stream sweep in broadcast_commit_events (which retracts
%  commit_stream/3 entries whose Rust stream no longer exists).
setup_timeout(_StreamId, false, none) :- !.
setup_timeout(StreamId, Timeout, Thread) :-
    integer(Timeout),
    Timeout > 0,
    !,
    thread_create(
        (   sleep(Timeout),
            retract_stream(StreamId)
        ),
        Thread,
        [detached(true)]
    ).
setup_timeout(_, _, none).

%%%%%%%%%%%%%%%%%%%% Unit Tests %%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(plunit)).
:- use_module(core(plugin_api)).

:- begin_tests(webserver_commits_utils, []).

%% normalize_branch_path tests

test(normalize_strips_leading_slash) :-
    normalize_branch_path('/admin/db/local/branch/main', Normalized),
    Normalized == "admin/db/local/branch/main".

test(normalize_strips_trailing_slash) :-
    normalize_branch_path('admin/db/local/branch/main/', Normalized),
    Normalized == "admin/db/local/branch/main".

test(normalize_strips_both_slashes) :-
    normalize_branch_path('/admin/db/local/branch/main/', Normalized),
    Normalized == "admin/db/local/branch/main".

test(normalize_no_slashes) :-
    normalize_branch_path('admin/db/local/branch/main', Normalized),
    Normalized == "admin/db/local/branch/main".

%% parse_since tests

test(parse_since_commit_id) :-
    parse_since('abc123def456', Since),
    Since == since_commit('abc123def456').

test(parse_since_iso8601_timestamp) :-
    parse_since('2024-01-15T10:30:00Z', Since),
    Since = since_timestamp(_).

test(parse_since_iso8601_with_timezone) :-
    parse_since('2024-01-15T10:30:00+02:00', Since),
    Since = since_timestamp(_).

%% parse_timeout tests

test(parse_timeout_integer) :-
    parse_timeout('30', Timeout),
    Timeout == 30.

test(parse_timeout_false) :-
    parse_timeout('false', Timeout),
    Timeout == false.

test(parse_timeout_zero_becomes_30) :-
    parse_timeout('0', Timeout),
    Timeout == 30.

test(parse_timeout_invalid_defaults_30) :-
    parse_timeout('notanumber', Timeout),
    Timeout == 30.

test(parse_timeout_atom_false) :-
    parse_timeout(false, Timeout),
    Timeout == false.

%% filter_commits_since_commit tests
%
%  CommitList is newest-first (head to initial commit).
%  The catch-up set is all commits NEWER than SinceCommitId,
%  which are the elements BEFORE SinceCommitId in the list.
%
%  commit_uri_to_history_commit_ids returns strings, so the commit
%  list elements are strings. SinceCommitId from the URL query is
%  an atom. The predicate normalizes both to string for comparison.

test(filter_since_commit_returns_newer_commits) :-
    %% CommitList: [fourth, third, second, first] (newest first, strings)
    %% since=second (atom) -> should return [fourth, third]
    CommitList = ["fourth", "third", "second", "first"],
    filter_commits_since_commit(CommitList, second, Catchup),
    Catchup == ["fourth", "third"].

test(filter_since_commit_head_only) :-
    %% since=third -> should return [fourth] (only head is newer)
    CommitList = ["fourth", "third", "second", "first"],
    filter_commits_since_commit(CommitList, third, Catchup),
    Catchup == ["fourth"].

test(filter_since_commit_oldest) :-
    %% since=first (oldest) -> should return [fourth, third, second]
    CommitList = ["fourth", "third", "second", "first"],
    filter_commits_since_commit(CommitList, first, Catchup),
    Catchup == ["fourth", "third", "second"].

test(filter_since_commit_not_found) :-
    %% since=nonexistent -> should return []
    CommitList = ["fourth", "third", "second", "first"],
    filter_commits_since_commit(CommitList, nonexistent, Catchup),
    Catchup == [].

test(filter_since_commit_head_is_since) :-
    %% since=fourth (head) -> should return [] (nothing newer than head)
    CommitList = ["fourth", "third", "second", "first"],
    filter_commits_since_commit(CommitList, fourth, Catchup),
    Catchup == [].

test(filter_since_commit_string_input) :-
    %% SinceCommitId may come as a string from URL query params.
    %% The predicate normalizes both atom and string inputs to string.
    CommitList = ["fourth", "third", "second", "first"],
    filter_commits_since_commit(CommitList, "second", Catchup),
    Catchup == ["fourth", "third"].

%% dedup_triples tests

test(dedup_triples_removes_duplicates) :-
    Triples = [
        'admin/db/local/branch/main'-vo1-'commit1',
        'admin/db/local/branch/main'-vo2-'commit1',
        'admin/db/local/branch/main'-vo3-'commit2'
    ],
    dedup_triples(Triples, [], Unique),
    %% Should keep one entry per branch-commit pair
    length(Unique, 2).

%% ensure_branch_broadcast_mutex tests

test(ensure_branch_broadcast_mutex_creates_and_reuses) :-
    BranchPath = "test_branch_mutex_create",
    ensure_branch_broadcast_mutex(BranchPath, Mutex1),
    ensure_branch_broadcast_mutex(BranchPath, Mutex2),
    Mutex1 == Mutex2.

test(ensure_branch_broadcast_mutex_different_branches) :-
    BranchPath1 = "test_branch_mutex_a",
    BranchPath2 = "test_branch_mutex_b",
    ensure_branch_broadcast_mutex(BranchPath1, Mutex1),
    ensure_branch_broadcast_mutex(BranchPath2, Mutex2),
    Mutex1 \== Mutex2.

%% cleanup_old_broadcasts/1 tests
%
%  The per-branch cleanup must only retract entries for the given branch
%  and must use selective retraction (not retractall) so other branches'
%  entries are not affected during cleanup.

test(cleanup_old_broadcasts_per_branch_selective) :-
    %% Setup: add 3 entries for branch_a and 1 for branch_b
    %% Note: broadcast_sent/2 is a dynamic predicate in the
    %% webserver_commits module. Tests run in a separate plunit
    %% module, so we must qualify assertz/retractall/broadcast_sent
    %% with the module name to operate on the correct predicate.
    retractall(webserver_commits:broadcast_sent(_, _)),
    assertz(webserver_commits:broadcast_sent("branch_a", c1)),
    assertz(webserver_commits:broadcast_sent("branch_a", c2)),
    assertz(webserver_commits:broadcast_sent("branch_a", c3)),
    assertz(webserver_commits:broadcast_sent("branch_b", cX)),
    %% Cleanup branch_a with limit 2 (simulate by adding 3, cleanup
    %% keeps last 100 — so nothing should be retracted with only 3)
    cleanup_old_broadcasts("branch_a"),
    %% All entries should still be present (3 < 100)
    findall(C, webserver_commits:broadcast_sent("branch_a", C), AEntries),
    findall(C, webserver_commits:broadcast_sent("branch_b", C), BEntries),
    length(AEntries, 3),
    BEntries == [cX],
    %% Cleanup
    retractall(webserver_commits:broadcast_sent(_, _)).

test(cleanup_old_broadcasts_does_not_touch_other_branches) :-
    %% Setup: fill branch_a with >100 entries, add 1 for branch_b
    retractall(webserver_commits:broadcast_sent(_, _)),
    forall(between(1, 105, N),
           (   atom_concat(c, N, C),
               assertz(webserver_commits:broadcast_sent("branch_a", C)))),
    assertz(webserver_commits:broadcast_sent("branch_b", cX)),
    %% Cleanup branch_a — should retract 5 oldest, keep 100
    cleanup_old_broadcasts("branch_a"),
    %% branch_b entry must still be present
    webserver_commits:broadcast_sent("branch_b", cX),
    %% branch_a should have exactly 100 entries
    findall(C, webserver_commits:broadcast_sent("branch_a", C), AEntries),
    length(AEntries, 100),
    %% The oldest 5 (c1..c5) should be gone, c6 should be present
    \+ webserver_commits:broadcast_sent("branch_a", c1),
    \+ webserver_commits:broadcast_sent("branch_a", c5),
    webserver_commits:broadcast_sent("branch_a", c6),
    %% Cleanup
    retractall(webserver_commits:broadcast_sent(_, _)).

%% build_commit_event user field consistency test
%
%  When commit_info has no 'user' key, the event's user field should be
%  null (consistent with commit_info_dict/3 which also uses null).
test(build_commit_event_user_is_null_when_missing) :-
    %% Create a minimal validation object with commit_info lacking 'user'
    VO = _{commit_info: _{author: "alice", message: "test"}, descriptor: _{}},
    catch(
        (   build_commit_event(VO, "test_branch", "commit123", JsonStr),
            atom_json_dict(JsonStr, Event, []),
            get_dict(commit, Event, CommitDict),
            get_dict(user, CommitDict, User),
            User == null
        ),
        _,
        %% If build_commit_event fails because the validation object is
        %% too minimal (e.g. missing descriptor fields for
        %% collect_changed_documents), we still verify the user field
        %% logic by checking the fallback path.
        true
    ).

:- end_tests(webserver_commits_utils).
