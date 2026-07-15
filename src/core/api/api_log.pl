:- module(api_log, [api_log/5, api_log_streaming/4, format_log/3]).

:- use_module(core(util)).
:- use_module(core(document)).
:- use_module(core(account)).
:- use_module(core(query)).
:- use_module(core(transaction)).

:- use_module(library(lists)).
:- use_module(library(json)).
:- use_module(library(option)).

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

    commit_uri_to_history_commit_uris(Repository_Descriptor, Commit_Uri, History_Commit_Uris, Options),

    findall(Commit_Doc,
            (   member(This_Uri, History_Commit_Uris),
                get_document(Repository_Descriptor, This_Uri, Commit_Doc0),
                (   get_dict(migration, Commit_Doc0, Migration_String)
                ->  atom_json_dict(Migration_String, Migration, [at(json)]),
                    put_dict(migration, Commit_Doc0, Migration, Commit_Doc)
                ;   Commit_Doc0 = Commit_Doc
                )
            ),
            Rev_Log),
    reverse(Rev_Log, Log).

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
api_log_streaming(System_DB, Auth, Path, Options) :-
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
    option(count(Count), Options, -1),
    stream_commit_history(Repo_Transaction, Commit_Uri, Start, Count).

%% stream_commit_history(+Context, +Commit_Uri, +Start, +Count)
%
%  Walk the parent chain newest-to-oldest, writing each commit as a
%  JSON line to current_output with flush. No list accumulation.
%
%  Start: commits to skip before emitting (0 = start from head)
%  Count: max commits to emit (-1 = unlimited, 0 = stop immediately)
stream_commit_history(_, _, _, 0) :- !.
stream_commit_history(Context, Commit_Uri, Start, Count) :-
    (   Start > 0
    ->  (   commit_uri_to_parent_uri(Context, Commit_Uri, Parent_Uri)
        ->  NextStart is Start - 1,
            stream_commit_history(Context, Parent_Uri, NextStart, Count)
        ;   true
        )
    ;  write_commit_json_line(Context, Commit_Uri),
        (   Count > 0
        ->  Remaining is Count - 1
        ;   Remaining = -1
        ),
        (   Remaining =:= 0
        ->  true
        ;   (   commit_uri_to_parent_uri(Context, Commit_Uri, Parent_Uri)
            ->  stream_commit_history(Context, Parent_Uri, 0, Remaining)
            ;   true
            )
        )
    ).

%% write_commit_json_line(+Context, +Commit_Uri)
%
%  Retrieve a single commit document and write it as a JSON line
%  to current_output, then flush to push data to the pipe immediately.
write_commit_json_line(Context, Commit_Uri) :-
    get_document(Context, Commit_Uri, Commit_Doc0),
    (   get_dict(migration, Commit_Doc0, Migration_String)
    ->  atom_json_dict(Migration_String, Migration, [at(json)]),
        put_dict(migration, Commit_Doc0, Migration, Commit_Doc)
    ;   Commit_Doc0 = Commit_Doc
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
