:- module(webserver_ext_commits, []).

%% Example plugin, not part of the public API. Copy it into the plugins
%% directory of a deployment to enable it; even when loaded it only
%% registers its route when TERMINUSDB_EXT_COMMITS is set to "true".
%%
%% GET /api/v1/ext/commits/*branch_path streams commit events for the
%% branch as NDJSON — the predecessor of /api/events, which is the
%% supported interface. Shared commit-broadcast state (commit_stream/3,
%% broadcast_sent/2, the per-branch mutex and the parsing helpers)
%% lives in webserver_events, whose post-commit producer feeds both
%% this channel and the events channels.

:- use_module(core(plugin_api)).
:- use_module(library(lists)).
:- use_module(library(json)).
:- use_module(library(option)).
:- use_module(library(date), [parse_time/3]).
:- use_module(library(uri)).
:- use_module(library(apply)).
:- use_module(library(yall)).

:- multifile appserver_hooks:appserver_stream/3.

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
%  The `timeout` parameter controls idle auto-close (default 30s,
%  false=never), enforced by the Rust subscriber task.
%  The post_commit_hook sends full commit events directly to each
%  matching stream via appserver_stream_send.

appserver_hooks:appserver_stream(get, '/api/v1/ext/commits/*branch_path',
                                  webserver_ext_commits:commits_handler) :-
    getenv('TERMINUSDB_EXT_COMMITS', 'true').

%%%%%%%%%%%%%%%%%%%% Stream Handler %%%%%%%%%%%%%%%%%%%%%%%%%

%% commits_handler(+Request, +StreamId, -Response) is det.
%
%  Authenticate the listener, check branch authorization, send initial
%  commit chunk(s), register the stream for live updates, and pass the
%  idle timeout to the Rust subscriber task.
%
%  Errors (authentication_incorrect, access_not_authorised, etc.) are
%  thrown and mapped to HTTP responses by the worker pool's
%  handle_plugin_stream_request catch wrapper.
commits_handler(Request, StreamId, Response) :-
    %% Extract the branch path from the wildcard path parameter.
    get_dict(params, Request, Params),
    get_dict(branch_path, Params, BranchPathRaw),
    webserver_events:normalize_branch_path(BranchPathRaw, BranchPath),

    %% Parse query parameters.
    get_dict(query, Request, QueryString),
    (   QueryString \= ""
    ->  uri_query_components(QueryString, Query)
    ;   Query = []
    ),

    %% Parse optional `since` parameter.
    (   memberchk(since=SinceStr, Query)
    ->  webserver_events:parse_since(SinceStr, Since)
    ;   Since = none
    ),

    %% Parse optional `timeout` parameter (default 30s).
    (   memberchk(timeout=TimeoutStr, Query)
    ->  webserver_events:parse_timeout(TimeoutStr, Timeout)
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

    %% Surface the current head commit ID in a response header so
    %% listeners can anchor a later reconnect with since=<commit>
    %% without paying for a catch-up event they would discard anyway.
    get_dict(repository_descriptor, Descriptor, RepoDescriptor),
    get_dict(branch_name, Descriptor, BranchName),
    (   catch(
            (   branch_head_commit(RepoDescriptor, BranchName, Head_Uri),
                commit_id_uri(RepoDescriptor, Head_Id, Head_Uri)
            ), _, fail)
    ->  Headers = _{'Content-Type': 'application/x-ndjson',
                    'TerminusDB-Commit-Head': Head_Id}
    ;   Headers = _{'Content-Type': 'application/x-ndjson'}
    ),

    %% Return a streaming response with a post_response continuation goal.
    %% The worker thread calls send_response (which sends HTTP headers as
    %% the first channel message), then calls the post_response goal to
    %% send initial data and register for live updates. No detached thread
    %% is needed — the worker thread handles everything and then returns
    %% to the pool.
    Response = _{
        status: 200,
        headers: Headers,
        body: stream,
        post_response: webserver_ext_commits:stream_commit_data(System_DB, Descriptor, BranchPath,
                                                            Since, Timeout, StreamId)
    }.

%%%%%%%%%%%%%%%%%%%% Stream Data Initialization %%%%%%%%%%%%%%%%%%%%%%%%%

%% stream_commit_data(+System_DB, +Descriptor, +BranchPath, +Since,
%%                     +Timeout, +StreamId) is det.
%
%  Called by the worker thread AFTER send_response has sent the HTTP
%  headers as the first channel message. Sends initial commit chunk(s)
%  and registers the stream for live updates. The Rust subscriber_task
%  enforces the idle timeout; the stream also closes when the client
%  disconnects (detected by Rust).
%
%  Uses a two-phase catch-up to minimize mutex hold time:
%    Phase 1: Send historical commits up to current head (NO mutex)
%    Phase 2: Send delta + register atomically (WITH mutex)
%  See PLAN_COMMIT_STREAM_SYNC.md for the full design.
stream_commit_data(System_DB, Descriptor, BranchPath, Since, Timeout, StreamId) :-
    catch(
        (   %% The Rust subscriber_task enforces the idle timeout — no
            %% Prolog thread per listener (one measured ~19KB RSS each).
            (   Since == live
            ->  webserver_events:timeout_to_secs(Timeout, TimeoutSecs),
                plugin_api:broadcast_subscribe(BranchPath, StreamId, TimeoutSecs),
                assertz(webserver_events:commit_stream(StreamId, BranchPath, none))
            ;   webserver_events:ensure_branch_broadcast_mutex(BranchPath, Mutex),

                %% PHASE 1 — Historical catch-up (NO mutex)
                %% Send all commits up to the current head. Broadcasts
                %% to existing clients continue unimpeded. Returns
                %% C_head for Phase 2 delta computation.
                send_historical_commits(System_DB, Descriptor,
                                        BranchPath, Since, StreamId, C_head),

                %% PHASE 2 — Delta sync + registration (WITH mutex)
                %% Atomically catch up any commits that arrived during
                %% Phase 1 and register for live broadcasts. The delta is
                %% typically 0-2 commits, so the mutex hold time is minimal.
                %% Registration has two parts:
                %%   1. Subscribe the stream to the Rust broadcast channel
                %%      (for parallel fan-out on the Rust side)
                %%   2. assertz commit_stream/3 (for sweep cleanup and
                %%      for the broadcast_sent dedup check in send_delta)
                with_mutex(Mutex,
                    (   send_delta_commits(System_DB, Descriptor,
                                           BranchPath, C_head, StreamId),
                        webserver_events:timeout_to_secs(Timeout, TimeoutSecs),
                        plugin_api:broadcast_subscribe(BranchPath, StreamId, TimeoutSecs),
                        assertz(webserver_events:commit_stream(StreamId, BranchPath, none))
                    )
                )
            )
        ),
        Error,
        (   json_log_error_formatted("stream_commit_data error: ~q", [Error]),
            retractall(webserver_events:commit_stream(StreamId, _, _)),
            catch(plugin_api:broadcast_unsubscribe_all(StreamId), _, true),
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
            %% If webserver_events:broadcast_sent(BranchPath, C) is true, the broadcast
            %% ran under the mutex but did not find this stream (not
            %% registered yet), so the delta must deliver C. If false,
            %% the broadcast has not run yet and will deliver C to this
            %% stream after Phase 2 registers.
            forall(
                (   member(CommitId, ChronologicalDelta),
                    commit_key(CommitId, CommitKey),
                    webserver_events:broadcast_sent(BranchPath, CommitKey)
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


:- begin_tests(webserver_ext_commits_utils).

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


:- end_tests(webserver_ext_commits_utils).
