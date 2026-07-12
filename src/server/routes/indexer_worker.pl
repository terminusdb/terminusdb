:- module(indexer_worker, [
              indexer_process_commit_handler/2
          ]).

:- use_module(core(api/api_indexer),
             [indexer_process_commit/4, indexer_next_commit/4,
              count_indexable_documents/4]).
:- use_module(core(util)).
:- use_module(core(query), [resolve_absolute_string_descriptor/2]).
:- use_module(core(transaction/ref_entity),
             [branch_head_commit/3,
              commit_uri_to_history_commit_ids/3]).
:- use_module(library(uri)).
:- use_module(library(lists)).
:- use_module(library(yall)).
:- use_module(core(util/test_utils),
             [setup_temp_store/1, teardown_temp_store/1,
              create_db_without_schema/2]).

%% indexer_process_commit_handler(+Request, +OutStream) is det.
%
%  Pipe-dispatch handler for processing a single commit.
%  Called from handle_pipe_work when HandlerModule == indexer_worker.
%
%  Request is a SWI HTTP request dict with:
%    - path: the branch path (e.g. "admin/db/local/branch/main")
%    - query: URL query string containing branch=... and commit=...
%
%  Writes CGI headers to OutStream:
%    Status: 200
%    X-Commit-Id: <commit_id>
%    X-Next-Commit: <commit_id> or None
%    X-Commit-Count: <total commits in branch history>
%
%  Then writes NDJSON body (changed documents with embedding text)
%  by calling api_indexer:indexer_process_commit/4.
%
%  On error, writes Status: 500 and an error marker JSON line.
indexer_process_commit_handler(Request, OutStream) :-
    get_dict(path, Request, Path),
    get_dict(query, Request, QueryString),
    parse_query(QueryString, BranchName, CommitId, Mode),
    (   Mode == "next"
    ->  % CommitId is the last indexed commit — find the next one.
        (   indexer_next_commit(Path, BranchName, CommitId, NextCommitRaw)
        ->  true
        ;   NextCommitRaw = 'None'
        ),
        (   NextCommitRaw == 'None'
        ->  % Already at HEAD — nothing to process.
            commit_count_for_branch(Path, BranchName, CommitCount),
            format(OutStream, 'Status: 200\n', []),
            format(OutStream, 'X-Commit-Id: ~w\n', [CommitId]),
            format(OutStream, 'X-Next-Commit: None\n', []),
            format(OutStream, 'X-Commit-Count: ~w\n', [CommitCount]),
            format(OutStream, 'X-Document-Count: 0\n', []),
            format(OutStream, '\n', []),
            flush_output(OutStream),
            !
        ;   CommitToProcess = NextCommitRaw
        )
    ;   Mode == "first"
    ->  % Never indexed — find the first commit in the branch.
        first_commit_for_branch(Path, BranchName, CommitToProcess)
    ;   % mode=process — process the given commit directly.
        CommitToProcess = CommitId
    ),
    (   commit_count_for_branch(Path, BranchName, CommitCount)
    ->  true
    ;   CommitCount = 0
    ),
    % Compute document count using fast Rust change detection.
    % This is near-instant so we can include it in the same header block.
    (   catch(count_indexable_documents(Path, BranchName, CommitToProcess, DocCount),
              _, DocCount = 0)
    ->  true
    ;   DocCount = 0
    ),
    % Find the next commit after the one we're processing (for the chain).
    (   indexer_next_commit(Path, BranchName, CommitToProcess, NextCommit)
    ->  true
    ;   NextCommit = 'None'
    ),
    % Find the parent commit (predecessor) for the push URL.
    (   parent_commit_for_branch(Path, BranchName, CommitToProcess, ParentCommit)
    ->  true
    ;   ParentCommit = "none"
    ),
    % Write all CGI headers in a single block so read_cgi_headers parses
    % them correctly (it treats the first \n\n as end-of-headers).
    format(OutStream, 'Status: 200\n', []),
    format(OutStream, 'X-Commit-Id: ~w\n', [CommitToProcess]),
    format(OutStream, 'X-Next-Commit: ~w\n', [NextCommit]),
    format(OutStream, 'X-Commit-Count: ~w\n', [CommitCount]),
    format(OutStream, 'X-Document-Count: ~w\n', [DocCount]),
    format(OutStream, 'X-Parent-Commit: ~w\n', [ParentCommit]),
    format(OutStream, '\n', []),
    flush_output(OutStream),
    format(user_error, "[DEBUG] indexer_worker: calling indexer_process_commit for ~w commit=~w doc_count=~w~n", [Path, CommitToProcess, DocCount]),
    catch(
        (   indexer_process_commit(Path, BranchName, CommitToProcess, OutStream)
        ->  format(user_error, "[DEBUG] indexer_worker: indexer_process_commit SUCCEEDED for ~w~n", [CommitToProcess])
        ;   format(user_error, "[DEBUG] indexer_worker: indexer_process_commit FAILED for ~w~n", [CommitToProcess])
        ),
        Error,
        (   with_output_to(atom(Error_Atom),
                write_term(Error, [quoted(false)])),
            format(user_error, "[DEBUG] indexer_worker: indexer_process_commit ERROR for ~w: ~w~n", [CommitToProcess, Error_Atom]),
            Error_Json = json{op:"Error", message:Error_Atom},
            json_write_dict(OutStream, Error_Json, []),
            nl(OutStream),
            flush_output(OutStream)
        )
    ).

%% parse_query(+QueryString, -BranchName, -CommitId, -Mode) is det.
%
%  Parses URL query string to extract branch, commit, and mode parameters.
%  Mode is "next", "first", or "process" (default).
parse_query(QueryString, BranchName, CommitId, Mode) :-
    uri_query_components(QueryString, Components),
    (   member(branch=BranchRaw, Components)
    ->  text_to_string(BranchRaw, BranchName)
    ;   BranchName = "main"
    ),
    (   member(commit=CommitRaw, Components)
    ->  text_to_string(CommitRaw, CommitId)
    ;   CommitId = ""
    ),
    (   member(mode=ModeRaw, Components)
    ->  text_to_string(ModeRaw, Mode)
    ;   Mode = "process"
    ).

%% first_commit_for_branch(+Path, +BranchName, -FirstCommitId) is det.
%
%  Finds the first (oldest) commit in the branch history.
%  Used when tdb-search has never indexed this branch (commit ID is empty).
first_commit_for_branch(Path, BranchName, FirstCommitId) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    branch_descriptor{branch_name: BranchName} :< Descriptor,
    get_dict(repository_descriptor, Descriptor, Repository_Descriptor),
    branch_head_commit(Repository_Descriptor, BranchName, Head_Commit_Uri),
    commit_uri_to_history_commit_ids(Repository_Descriptor,
                                     Head_Commit_Uri,
                                     [FirstCommitId|_]).

%% commit_count_for_branch(+Path, +BranchName, -Count) is semidet.
%
%  Computes the total number of commits in the branch history
%  (from the oldest commit to HEAD). Used to set the progress total
%  so the index status API can report meaningful progress.
commit_count_for_branch(Path, BranchName, Count) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    branch_descriptor{branch_name: BranchName} :< Descriptor,
    get_dict(repository_descriptor, Descriptor, Repository_Descriptor),
    branch_head_commit(Repository_Descriptor, BranchName, Head_Commit_Uri),
    commit_uri_to_history_commit_ids(Repository_Descriptor,
                                     Head_Commit_Uri,
                                     History),
    length(History, Count).

%% parent_commit_for_branch(+Path, +BranchName, +CommitId, -ParentCommit) is det.
%
%  Finds the parent (predecessor) of CommitId in the branch history.
%  Returns 'none' if CommitId is the first (oldest) commit in the branch.
parent_commit_for_branch(Path, BranchName, CommitId, ParentCommit) :-
    resolve_absolute_string_descriptor(Path, Descriptor),
    branch_descriptor{branch_name: BranchName} :< Descriptor,
    get_dict(repository_descriptor, Descriptor, Repository_Descriptor),
    branch_head_commit(Repository_Descriptor, BranchName, Head_Commit_Uri),
    commit_uri_to_history_commit_ids(Repository_Descriptor,
                                     Head_Commit_Uri,
                                     History),
    (   append(_, [Parent, CommitId|_], History)
    ->  text_to_string(Parent, ParentCommit)
    ;   ParentCommit = "none"
    ).


:- begin_tests(indexer_worker_tests).

test(parse_query_extracts_branch_and_commit) :-
    parse_query("branch=main&commit=abc123", Branch, Commit, Mode),
    assertion(Branch == "main"),
    assertion(Commit == "abc123"),
    assertion(Mode == "process").

test(parse_query_defaults_branch_to_main) :-
    parse_query("commit=abc123", Branch, Commit, Mode),
    assertion(Branch == "main"),
    assertion(Commit == "abc123"),
    assertion(Mode == "process").

test(parse_query_empty_commit) :-
    parse_query("branch=dev", Branch, Commit, Mode),
    assertion(Branch == "dev"),
    assertion(Commit == ""),
    assertion(Mode == "process").

test(parse_query_empty_query_string) :-
    parse_query("", Branch, Commit, Mode),
    assertion(Branch == "main"),
    assertion(Commit == ""),
    assertion(Mode == "process").

test(parse_query_extracts_mode_next) :-
    parse_query("branch=main&commit=abc&mode=next", Branch, Commit, Mode),
    assertion(Branch == "main"),
    assertion(Commit == "abc"),
    assertion(Mode == "next").

test(parse_query_extracts_mode_first) :-
    parse_query("branch=main&commit=&mode=first", Branch, Commit, Mode),
    assertion(Branch == "main"),
    assertion(Commit == ""),
    assertion(Mode == "first").

test("commit_count_for_branch returns 1 for a freshly created database",
     [ setup((setup_temp_store(State),
              create_db_without_schema("admin", "countdb")
             )),
       cleanup(teardown_temp_store(State)),
       true(Count == 1)
     ]) :-
    commit_count_for_branch("admin/countdb/local/branch/main", "main", Count).

:- end_tests(indexer_worker_tests).
