:- module(api_log, [api_log/5, api_log_streaming/4, api_log_streaming_prepare/9, api_log_stream_emit/7, format_log/3]).

:- use_module(core(util)).
:- use_module(core(document)).
:- use_module(core(account)).
:- use_module(core(query)).
:- use_module(core(transaction)).

:- use_module(library(lists)).
:- use_module(library(json)).
:- use_module(library(option)).

%% log_time_bounds(+Options, -Start_TS, -End_TS) is det.
%
%  Parses the ISO 8601 datetime window options to rational epoch bounds:
%    startDateTime - skip commits newer than this instant; the first emitted
%                    commit is the newest with timestamp =< it
%    endDateTime   - stop emitting at commits older than this instant; only
%                    commits with timestamp >= it are emitted
%  `none` marks an absent bound.
log_time_bounds(Options, Start_TS, End_TS) :-
    (   option(startDateTime(Start_ISO), Options)
    ->  do_or_die(
            catch(iso8601_to_epoch(Start_ISO, Start_TS), _, fail),
            error(invalid_log_datetime(Start_ISO), _))
    ;   Start_TS = none),
    (   option(endDateTime(End_ISO), Options)
    ->  do_or_die(
            catch(iso8601_to_epoch(End_ISO, End_TS), _, fail),
            error(invalid_log_datetime(End_ISO), _))
    ;   End_TS = none).

%% commit_timestamp(+Context, +Commit_Uri, -Timestamp) is det.
commit_timestamp(Context, Commit_Uri, Timestamp) :-
    get_document(Context, Commit_Uri, Commit_Doc),
    get_dict(timestamp, Commit_Doc, Timestamp).

%% skip_newer_than(+Context, +Commit_Uri, +Start_TS, -Anchor_Uri, -Skipped) is det.
%
%  Walks the parent chain newest-to-oldest past commits whose timestamp
%  exceeds Start_TS. Yields the first commit with timestamp =< Start_TS
%  (Anchor_Uri) and the number of skipped commits. When every commit is
%  newer, Anchor_Uri is `none` and Skipped is the whole chain length.
skip_newer_than(Context, Commit_Uri, Start_TS, Anchor_Uri, Skipped) :-
    skip_newer_than_(Context, Commit_Uri, Start_TS, 0, Anchor_Uri, Skipped).

skip_newer_than_(Context, Commit_Uri, Start_TS, Acc, Anchor_Uri, Skipped) :-
    commit_timestamp(Context, Commit_Uri, Timestamp),
    (   Timestamp > Start_TS
    ->  (   commit_uri_to_parent_uri(Context, Commit_Uri, Parent_Uri)
        ->  Acc1 is Acc + 1,
            skip_newer_than_(Context, Parent_Uri, Start_TS, Acc1, Anchor_Uri, Skipped)
        ;   Anchor_Uri = none,
            Skipped is Acc + 1)
    ;   Anchor_Uri = Commit_Uri,
        Skipped = Acc).

%% take_first(+N, +List, -Taken) is det.
%  The first N elements of List; a negative N takes all of List.
take_first(N, List, List) :-
    N < 0,
    !.
take_first(_, [], []) :- !.
take_first(0, _, []) :- !.
take_first(N, [Head|Tail], [Head|Taken]) :-
    N > 0,
    N1 is N - 1,
    take_first(N1, Tail, Taken).

%% filter_commits_since(+End_TS, +Docs, -Bounded) is det.
%  Keep commit documents whose timestamp is at least End_TS, preserving order.
filter_commits_since(_, [], []).
filter_commits_since(End_TS, [Doc|Docs], Bounded) :-
    get_dict(timestamp, Doc, Timestamp),
    (   Timestamp >= End_TS
    ->  Bounded = [Doc|Rest]
    ;   Bounded = Rest),
    filter_commits_since(End_TS, Docs, Rest).

%% commit_change_counts(+Repository_Descriptor, +Commit_Id, -Added, -Changed, -Deleted) is det.
%
%  Opens the commit transaction and uses the Rust collect_changed_documents
%  to compute document change counts in a single pass. Returns zeros when
%  the commit has no parent (initial commit), where collect_changed_documents
%  fails because there is no parent layer to diff against. Any other error
%  from resolve_relative_descriptor or open_descriptor propagates.
commit_change_counts(Repository_Descriptor, Commit_Id, Added, Changed, Deleted) :-
    resolve_relative_descriptor(Repository_Descriptor,
                                ["commit", Commit_Id],
                                Commit_Descriptor),
    open_descriptor(Commit_Descriptor, Transaction),
    (   '$changes':collect_changed_documents(Transaction, Added_List, Changed_List, Deleted_List)
    ->  length(Added_List, Added),
        length(Changed_List, Changed),
        length(Deleted_List, Deleted)
    ;   Added = 0, Changed = 0, Deleted = 0
    ).

%% maybe_add_change_counts(+Repository_Descriptor, +Commit_Doc0, -Commit_Doc, +Options) is det.
%
%  When the with_counts option is true, enriches the commit document with
%  added_count, changed_count, and deleted_count fields.
maybe_add_change_counts(Repository_Descriptor, Commit_Doc0, Commit_Doc, Options) :-
    (   option(with_counts(true), Options)
    ->  get_dict(identifier, Commit_Doc0, Commit_Id),
        commit_change_counts(Repository_Descriptor, Commit_Id, Added, Changed, Deleted),
        put_dict(_{added_count: Added, changed_count: Changed, deleted_count: Deleted},
                 Commit_Doc0, Commit_Doc)
    ;   Commit_Doc0 = Commit_Doc
    ).

descriptor_type_has_history(branch_descriptor).
descriptor_type_has_history(commit_descriptor).

descriptor_has_history(Descriptor) :-
    Type{} :< Descriptor,
    descriptor_type_has_history(Type).

loggable_commit_uri(Descriptor, Repository_Descriptor, Commit_Uri) :-
    branch_descriptor{repository_descriptor: Repository_Descriptor,
                      branch_name: Branch_Name} :< Descriptor,
    !,
    branch_head_commit(Repository_Descriptor,
                       Branch_Name,
                       Commit_Uri).
loggable_commit_uri(Descriptor, Repository_Descriptor, Commit_Uri) :-
    commit_descriptor{commit_id: Commit_Id,
                      repository_descriptor: Repository_Descriptor} :< Descriptor,
    commit_id_uri(Repository_Descriptor, Commit_Id, Commit_Uri).

api_log(System_DB, Auth, Path, Log, Options) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path,Descriptor),
        error(invalid_absolute_path(Path),_)),

    do_or_die(descriptor_has_history(Descriptor),
              error(resource_has_no_history(Descriptor), _)),

    check_descriptor_auth(System_DB, Descriptor, '@schema':'Action/meta_read_access', Auth),

    do_or_die(
        open_descriptor(Descriptor, _Branch_Transaction),
        error(unresolvable_absolute_descriptor(Descriptor),_)),

    loggable_commit_uri(Descriptor, Repository_Descriptor, Commit_Uri),

    option(start(Start), Options, 0),
    option(count(Count), Options, -1),
    log_time_bounds(Options, Start_TS, End_TS),

    %  startDateTime resolves to a commit offset by walking the chain until
    %  timestamps no longer exceed it. When every commit is newer the window
    %  is empty.
    (   Start_TS = none
    ->  Effective_Start = Start,
        Anchor_Uri = Commit_Uri
    ;   do_or_die(
            open_descriptor(Repository_Descriptor, Repo_Transaction),
            error(unresolvable_absolute_descriptor(Repository_Descriptor),_)),
        skip_newer_than(Repo_Transaction, Commit_Uri, Start_TS, Anchor_Uri, Effective_Start)),

    %  endDateTime bounds the oldest edge, so collection must be unbounded
    %  before filtering; count is applied after filtering instead.
    (   Anchor_Uri = none
    ->  History_Commit_Uris = []
    ;   (   End_TS = none
        ->  Collect_Count = Count
        ;   Collect_Count = -1),
        commit_uri_to_history_commit_uris(Repository_Descriptor, Commit_Uri, History_Commit_Uris,
                                          [start(Effective_Start), count(Collect_Count)])),

    findall(Commit_Doc,
            (   member(This_Uri, History_Commit_Uris),
                get_document(Repository_Descriptor, This_Uri, Commit_Doc0),
                (   get_dict(migration, Commit_Doc0, Migration_String)
                ->  atom_json_dict(Migration_String, Migration, [at(json)]),
                    put_dict(migration, Commit_Doc0, Migration, Commit_Doc1)
                ;   Commit_Doc0 = Commit_Doc1
                ),
                maybe_add_change_counts(Repository_Descriptor, Commit_Doc1, Commit_Doc, Options)
            ),
            Rev_Log),
    (   End_TS = none
    ->  reverse(Rev_Log, Log)
    ;   filter_commits_since(End_TS, Rev_Log, Bounded_Log),
        reverse(Bounded_Log, Newest_First),
        take_first(Count, Newest_First, Log)).

%% api_log_streaming(+System_DB, +Auth, +Path, +Options) is det.
%
%  Stream commit log as NDJSON, one commit document per line.
%  Walks the parent chain from the branch head (newest) to the root
%  (oldest), writing each commit document as it is found. True
%  incremental streaming — no list materialization or reversal.
%
%  Options:
%    start - number of initial commits to skip (default 0)
%    count - maximum commits to emit (-1 = unlimited, default -1)
%    startDateTime - ISO 8601 instant; skip commits newer than it
%    endDateTime   - ISO 8601 instant; stop at commits older than it
api_log_streaming(System_DB, Auth, Path, Options) :-
    api_log_streaming_prepare(System_DB, Auth, Path, Options,
                              _Resolved_Start, Repo_Transaction, Anchor_Uri,
                              Repository_Descriptor, Effective_Start),
    option(count(Count), Options, -1),
    option(with_counts(With_Counts), Options, false),
    api_log_stream_emit(Repo_Transaction, Anchor_Uri, Effective_Start, Count,
                        With_Counts, Repository_Descriptor, Options).

%% api_log_streaming_prepare(+System_DB, +Auth, +Path, +Options,
%%                           -Resolved_Start, -Repo_Transaction, -Anchor_Uri,
%%                           -Repository_Descriptor, -Effective_Start) is det.
%
%  Resolves the stream's anchor commit without writing anything: performs
%  descriptor and auth checks, resolves the head URI, applies startDateTime
%  to find the window's top commit, and reports the resolved start offset so
%  the caller can surface it (e.g. as a response header) before emission.
api_log_streaming_prepare(System_DB, Auth, Path, Options,
                          Resolved_Start, Repo_Transaction, Anchor_Uri,
                          Repository_Descriptor, Effective_Start) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path,Descriptor),
        error(invalid_absolute_path(Path),_)),

    do_or_die(descriptor_has_history(Descriptor),
              error(resource_has_no_history(Descriptor), _)),

    check_descriptor_auth(System_DB, Descriptor, '@schema':'Action/meta_read_access', Auth),

    do_or_die(
        open_descriptor(Descriptor, _Branch_Transaction),
        error(unresolvable_absolute_descriptor(Descriptor),_)),

    loggable_commit_uri(Descriptor, Repository_Descriptor, Commit_Uri),

    %% Open the repository descriptor once and reuse the transaction
    %% object for all commit lookups. Without this, each get_document
    %% and commit_uri_to_parent_uri call re-opens the descriptor (133+
    %% redundant open_descriptor calls per request), creating new layer
    %% references and tabled predicate entries that leak atoms.
    do_or_die(
        open_descriptor(Repository_Descriptor, Repo_Transaction),
        error(unresolvable_absolute_descriptor(Repository_Descriptor),_)),

    option(start(Start), Options, 0),
    log_time_bounds(Options, Start_TS, _End_TS),
    (   Start_TS = none
    ->  Anchor_Uri = Commit_Uri,
        Effective_Start = Start,
        Resolved_Start = Start
    ;   skip_newer_than(Repo_Transaction, Commit_Uri, Start_TS, Anchor_Uri, Skipped),
        Effective_Start = 0,
        Resolved_Start = Skipped).

%% api_log_stream_emit(+Repo_Transaction, +Anchor_Uri, +Start, +Count,
%%                     +With_Counts, +Repository_Descriptor, +Options) is det.
%
%  Writes the resolved window as NDJSON lines. An `none` anchor means every
%  commit is newer than startDateTime — nothing is emitted.
api_log_stream_emit(_, none, _, _, _, _, _) :- !.
api_log_stream_emit(Context, Anchor_Uri, Start, Count, With_Counts, Repository_Descriptor, Options) :-
    log_time_bounds(Options, _Start_TS, End_TS),
    stream_commit_history(Context, Anchor_Uri, Start, Count, With_Counts,
                          Repository_Descriptor, End_TS).

%% stream_commit_history(+Context, +Commit_Uri, +Start, +Count, +With_Counts, +Repository_Descriptor, +End_TS)
%
%  Walk the parent chain newest-to-oldest, writing each commit as a
%  JSON line to current_output with flush. No list accumulation.
%
%  Start: commits to skip before emitting (0 = start from head)
%  Count: max commits to emit (-1 = unlimited, 0 = stop immediately)
%  With_Counts: if true, enrich each commit with added_count/changed_count/deleted_count
%  End_TS: `none`, or an epoch bound — stop before commits older than it
stream_commit_history(_, _, _, 0, _, _, _) :- !.
stream_commit_history(Context, Commit_Uri, Start, Count, With_Counts, Repository_Descriptor, End_TS) :-
    (   Start > 0
    ->  (   commit_uri_to_parent_uri(Context, Commit_Uri, Parent_Uri)
        ->  NextStart is Start - 1,
            stream_commit_history(Context, Parent_Uri, NextStart, Count, With_Counts, Repository_Descriptor, End_TS)
        ;   true
        )
    ;   (   End_TS = none
        ->  Emit = true
        ;   (   commit_timestamp(Context, Commit_Uri, Timestamp),
                Timestamp >= End_TS
            ->  Emit = true
            ;   Emit = false
            )
        ),
        (   Emit = true
        ->  write_commit_json_line(Context, Commit_Uri, With_Counts, Repository_Descriptor),
            (   Count > 0
            ->  Remaining is Count - 1
            ;   Remaining = -1
            ),
            (   Remaining =:= 0
            ->  true
            ;   (   commit_uri_to_parent_uri(Context, Commit_Uri, Parent_Uri)
                ->  stream_commit_history(Context, Parent_Uri, 0, Remaining, With_Counts, Repository_Descriptor, End_TS)
                ;   true
                )
            )
        ;   true
        )
    ).

%% write_commit_json_line(+Context, +Commit_Uri, +With_Counts, +Repository_Descriptor)
%
%  Retrieve a single commit document and write it as a JSON line
%  to current_output, then flush to push data to the pipe immediately.
%  When With_Counts is true, enriches the document with change counts.
write_commit_json_line(Context, Commit_Uri, With_Counts, Repository_Descriptor) :-
    get_document(Context, Commit_Uri, Commit_Doc0),
    (   get_dict(migration, Commit_Doc0, Migration_String)
    ->  atom_json_dict(Migration_String, Migration, [at(json)]),
        put_dict(migration, Commit_Doc0, Migration, Commit_Doc1)
    ;   Commit_Doc0 = Commit_Doc1
    ),
    (   With_Counts
    ->  get_dict(identifier, Commit_Doc1, Commit_Id),
        commit_change_counts(Repository_Descriptor, Commit_Id, Added, Changed, Deleted),
        put_dict(_{added_count: Added, changed_count: Changed, deleted_count: Deleted},
                 Commit_Doc1, Commit_Doc)
    ;   Commit_Doc1 = Commit_Doc
    ),
    json_write_dict(current_output, Commit_Doc, [width(0)]),
    nl,
    flush_output.

format_log(Stream, Log, Options) :-
    forall(
        member(Commit_Doc, Log),
        (   get_dict('identifier', Commit_Doc, Id),
            format(Stream,'~s~n', [Id]),
            format(Stream,'--------------------------------~n', []),
            get_dict('timestamp', Commit_Doc, TimeStamp),
            stamp_date_time(TimeStamp, DateTime, 0),
            format_time(Stream, 'Date: %FT%T%:z', DateTime),
            format(Stream, '~n', []),
            get_dict('author', Commit_Doc, Auth),
            format(Stream,'Author: ~s~n', [Auth]),
            get_dict('message', Commit_Doc, Message),
            format(Stream,'Message: ~s~n', [Message]),
            (   option(verbose(true), Options),
                get_dict(migration, Commit_Doc, Migration)
            ->  format(Stream,'Migration:~n', []),
                json_write_dict(Stream, Migration, [step(4), width(80)]),
                format(Stream,'~n', [])
            ;   true
            ),
            format(Stream, '~n', [])
        )
    ).
