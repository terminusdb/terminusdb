:- module(parallel_elaboration, [
              elaborate_insert_request/4,
              elaborate_insert_request_db/4,
              elaborate_insert_request_db_with_contracts/4,
              maybe_help_with_elaboration/0,
              start_elaboration_workers/1,
              stop_elaboration_workers/0,
              with_commit_window_guard/4,
              acquire_commit_window_guard/3,
              acquire_commit_window_guard/2,
              release_commit_window_guard/0,
              has_commit_window_guard/0,
              branch_key_from_transaction/2,
              first_commit_candidate/2,
              open_commit_window_for_branch_head/5,
              max_chunk_size/1,
              start_workers/1,
              stop_workers/0
          ]).

:- meta_predicate with_commit_window_guard(+, +, -, 0).

:- use_module(core(document)).
:- use_module(core(document/json)).
:- use_module(core(api/api_document), [pre_branch_commit_id/2]).
:- use_module(core(transaction)).
:- use_module(core(transaction/descriptor), [branch_key_from_descriptor/2]).
:- use_module(core(query)).
:- use_module(core(transaction/ref_entity)).
:- use_module(core(util)).
:- use_module(library(terminus_store)).
:- use_module(core(util/test_utils)).

:- use_module(config(terminus_config), [worker_amount/1,
                                          worker_elaboration_preference/1]).
:- use_module(core(document/commit_queue)).

:- use_module(library(apply)).
:- use_module(library(random)).
:- use_module(library(assoc)).
:- use_module(library(gensym)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(pairs)).
:- use_module(library(plunit)).
:- use_module(library(yall)).

:- dynamic request_queue/8.
% request_queue(RequestId, Descriptor, DB, PendingChunks, TotalChunks, WrapApiErrors,
%                 PreBranchCommitId, PreSchemaLayerId).

:- dynamic max_chunk_size/1.
% max_chunk_size is the maximum number of documents that may be grouped into
% one parallel elaboration chunk. A large insert request is split into chunks
% no larger than this value, and each chunk is elaborated independently
% (potentially by different worker threads). A smaller default ensures more
% chunks per request and therefore better use of multiple workers for typical
% document counts; a very large request still gets many chunks. The default
% can be overridden at module load time via TERMINUSDB_CHUNK_SIZE.
max_chunk_size(1000).

:- (   getenv('TERMINUSDB_CHUNK_SIZE', ChunkSizeString),
       catch(atom_number(ChunkSizeString, ChunkSize), _, fail),
       integer(ChunkSize),
       ChunkSize > 0
   ->  retractall(max_chunk_size(_)),
       assertz(max_chunk_size(ChunkSize))
   ;   true
   ).

:- dynamic elaboration_workers_should_stop/0.
:- dynamic elaboration_worker_thread/1.

:- mutex_create(elaboration_queue_mutex).

/**
 * elaborate_insert_request(+Descriptor, +Docs, -Elaborated, +Options) is semidet.
 *
 * Elaborate a list of documents in parallel using a work-stealing chunk queue.
 * Elaboration is performed outside the transaction window; the caller is
 * responsible for opening a transaction and inserting the returned elaborated
 * documents.
 *
 * Chunks that are eligible for the Rust fast path use Rust elaboration; other
 * chunks fall back to the Prolog elaboration path.  The decision is made per
 * chunk by the worker that processes it.
 */
elaborate_insert_request(Descriptor, Docs, Elaborated, Options) :-
    length(Docs, DocCount),
    chunk_size_for_request(DocCount, ChunkSize),
    chunks_from_documents(Docs, ChunkSize, Chunks),
    length(Chunks, TotalChunks),
    open_descriptor(Descriptor, DB),
    option(wrap_api_errors(Wrap), Options, false),
    setup_call_cleanup(
        create_request_queue(Descriptor, DB, Chunks, TotalChunks, Wrap, RequestId),
        collect_and_process(RequestId, TotalChunks, Elaborated),
        cleanup_request(RequestId)
    ).

elaborate_insert_request_db(DB, Docs, Elaborated, Options) :-
    length(Docs, DocCount),
    chunk_size_for_request(DocCount, ChunkSize),
    chunks_from_documents(Docs, ChunkSize, Chunks),
    length(Chunks, TotalChunks),
    option(wrap_api_errors(Wrap), Options, false),
    setup_call_cleanup(
        create_request_queue(none, DB, Chunks, TotalChunks, Wrap, RequestId),
        collect_and_process(RequestId, TotalChunks, Elaborated),
        cleanup_request(RequestId)
    ).

elaborate_insert_request_db_with_contracts(DB, Docs, Pairs, Options) :-
    length(Docs, DocCount),
    chunk_size_for_request(DocCount, ChunkSize),
    chunks_from_documents(Docs, ChunkSize, Chunks),
    length(Chunks, TotalChunks),
    option(wrap_api_errors(Wrap), Options, false),
    setup_call_cleanup(
        create_request_queue(none, DB, Chunks, TotalChunks, Wrap, RequestId),
        collect_and_process_pairs(RequestId, TotalChunks, Pairs),
        cleanup_request(RequestId)
    ).

chunk_size_for_request(DocCount, ChunkSize) :-
    max_chunk_size(MaxChunkSize),
    (   DocCount > MaxChunkSize
    ->  ChunkSize = MaxChunkSize
    ;   ChunkSize = DocCount
    ).

chunks_from_documents(Docs, ChunkSize, Chunks) :-
    chunks_from_documents(Docs, ChunkSize, 0, Chunks).

chunks_from_documents([], _, _, []).
chunks_from_documents(Docs, ChunkSize, Index, [chunk(Index, Chunk)|Rest]) :-
    length(Chunk, ChunkSize),
    append(Chunk, Remainder, Docs),
    !,
    NextIndex is Index + 1,
    chunks_from_documents(Remainder, ChunkSize, NextIndex, Rest).
chunks_from_documents(Remainder, _, Index, [chunk(Index, Remainder)]).

snapshot_ids(DB, PreBranchCommitId, PreSchemaLayerId) :-
    (   catch(transaction_data_version(DB, data_version(branch, PreBranchCommitId)),
              error(data_version_not_found(_), _),
              fail)
    ->  true
    ;   PreBranchCommitId = none),
    (   DB = transaction_object{schema_objects: [SchemaObject]},
        layer_to_id(SchemaObject.read, PreSchemaLayerId)
    ->  true
    ;   PreSchemaLayerId = none).

branch_key_from_transaction(Transaction, BranchKey) :-
    branch_key_from_descriptor(Transaction.descriptor, BranchKey).

create_request_queue(Descriptor, DB, Chunks, TotalChunks, Wrap, RequestId) :-
    snapshot_ids(DB, PreBranchCommitId, PreSchemaLayerId),
    create_request_queue(Descriptor, DB, Chunks, TotalChunks, Wrap,
                         PreBranchCommitId, PreSchemaLayerId, RequestId).

create_request_queue(Descriptor, DB, Chunks, TotalChunks, Wrap,
                     PreBranchCommitId, PreSchemaLayerId, RequestId) :-
    gensym(request, RequestId),
    message_queue_create(RequestId, [alias(RequestId)]),
    assertz(request_queue(RequestId, Descriptor, DB, Chunks, TotalChunks, Wrap,
                          PreBranchCommitId, PreSchemaLayerId)),
    commit_queue:wake_workers.

collect_and_process(RequestId, TotalChunks, Elaborated) :-
    collect_and_process_pairs(RequestId, TotalChunks, Pairs),
    pairs_keys(Pairs, Elaborated).

collect_and_process_pairs(RequestId, TotalChunks, Pairs) :-
    collect_and_process_(RequestId, TotalChunks, [], SortedPairs),
    sort(SortedPairs, Sorted),
    pairs_values(Sorted, ChunkList),
    append(ChunkList, Pairs).

collect_and_process_(_, 0, Pairs, Pairs) :- !.
collect_and_process_(RequestId, Remaining, Pairs, FinalPairs) :-
    (   take_chunk(RequestId, _OwnerId, DB, Wrap, chunk(Index, Docs),
                   PreBranchCommitId, PreSchemaLayerId)
    ->  (   process_chunk(DB, Wrap, Docs, PreBranchCommitId, PreSchemaLayerId, Elaborated)
        ->  Remaining1 is Remaining - 1,
            collect_and_process_(RequestId, Remaining1, [Index-Elaborated|Pairs], FinalPairs)
        ;   throw(error(parallel_elaboration_failed(chunk(Index, Docs)), _))
        )
    ;   % No local chunks left; wait for idle HTTP workers to send results.
        thread_get_message(RequestId, Message),
        handle_chunk_message(RequestId, Message, Remaining, Pairs, FinalPairs)
    ).

handle_chunk_message(RequestId, chunk_done(Index, Elaborated), Remaining, Pairs, FinalPairs) :-
    !,
    Remaining1 is Remaining - 1,
    collect_and_process_(RequestId, Remaining1, [Index-Elaborated|Pairs], FinalPairs).
handle_chunk_message(_RequestId, chunk_error(_Index, Error), _Remaining, _Pairs, _FinalPairs) :-
    throw(Error).

send_chunk_done(OwnerId, Index, Elaborated) :-
    catch(thread_send_message(OwnerId, chunk_done(Index, Elaborated)), _, true).

send_chunk_error(OwnerId, Index, Error) :-
    catch(thread_send_message(OwnerId, chunk_error(Index, Error)), _, true).

take_chunk(RequestId, OwnerId, DB, Wrap, Chunk, PreBranchCommitId, PreSchemaLayerId) :-
    with_mutex(elaboration_queue_mutex,
               (   request_queue(RequestId, _, DB0, [Chunk|Rest], Total, Wrap,
                                 PreBranchCommitId0, PreSchemaLayerId0)
               ->  OwnerId = RequestId,
                   DB = DB0,
                   PreBranchCommitId = PreBranchCommitId0,
                   PreSchemaLayerId = PreSchemaLayerId0,
                   retractall(request_queue(RequestId, _, _, _, _, _, _, _)),
                   assertz(request_queue(RequestId, _, DB0, Rest, Total, Wrap,
                                          PreBranchCommitId0, PreSchemaLayerId0))
               ;   request_queue(OwnerId, _, DB0, [Chunk|Rest], Total, Wrap,
                                 PreBranchCommitId0, PreSchemaLayerId0),
                   OwnerId \= RequestId
               ->  DB = DB0,
                   PreBranchCommitId = PreBranchCommitId0,
                   PreSchemaLayerId = PreSchemaLayerId0,
                   retractall(request_queue(OwnerId, _, _, _, _, _, _, _)),
                   assertz(request_queue(OwnerId, _, DB0, Rest, Total, Wrap,
                                          PreBranchCommitId0, PreSchemaLayerId0))
               ;   fail
               )).

rust_elaboration_available :-
    current_predicate('$doc':rust_elaborate_simple_documents/5).

process_chunk(DB, Wrap, Docs, PreBranchCommitId, PreSchemaLayerId, Elaborated) :-
    rust_elaborate_chunk(DB, Wrap, Docs, PreBranchCommitId, PreSchemaLayerId, Elaborated).

rust_elaborate_chunk(DB, _Wrap, Docs, PreBranchCommitId, PreSchemaLayerId, Pairs) :-
    rust_elaboration_available,
    '$doc':get_document_context(DB, Context),
    catch(
        '$doc':rust_elaborate_simple_documents(Context, Docs, PreBranchCommitId, PreSchemaLayerId, Pairs),
        _Error,
        fail
    ),
    !.
rust_elaborate_chunk(DB, Wrap, Docs, PreBranchCommitId, PreSchemaLayerId, Pairs) :-
    prolog_elaborate_chunk(DB, Wrap, Docs, PreBranchCommitId, PreSchemaLayerId, Pairs).

prolog_elaborate_chunk(DB, Wrap, Docs, PreBranchCommitId, PreSchemaLayerId, Pairs) :-
    maplist(elaborate_with_contract(DB, Wrap, PreBranchCommitId, PreSchemaLayerId), Docs, Pairs).

elaborate_with_contract(DB, Wrap, PreBranchCommitId, PreSchemaLayerId, Doc, Elaborated-Contract) :-
    catch(
        json_elaborate_with_contract(DB, Doc, Elaborated, Contract0),
        Error,
        (   (   Wrap = true,
                current_predicate(api_document:api_document_error_wrapper/3)
            ->  api_document:api_document_error_wrapper(Error, Doc, Wrapped),
                throw(Wrapped)
            ;   throw(Error)
            )
        )
    ),
    Contract = Contract0.put(_{
        pre_branch_commit_id: PreBranchCommitId,
        pre_schema_layer_id: PreSchemaLayerId
    }).

cleanup_request(RequestId) :-
    retractall(request_queue(RequestId, _, _, _, _, _, _, _)),
    message_queue_destroy(RequestId).

/**
 * maybe_help_with_elaboration is semidet.
 *
 * Short hook for idle HTTP workers.  Try to steal and process exactly one
 * chunk from any registered request queue, then return.  Fails when no chunk
 * is available.
 */
maybe_help_with_elaboration :-
    request_queue(RequestId, _, _, _, _, _, _, _),
    take_chunk(RequestId, OwnerId, DB, Wrap, chunk(Index, Docs),
               PreBranchCommitId, PreSchemaLayerId),
    process_chunk_with_result(DB, Wrap, Docs, PreBranchCommitId, PreSchemaLayerId,
                              OwnerId, Index).

process_chunk_with_result(DB, Wrap, Docs, PreBranchCommitId, PreSchemaLayerId,
                          OwnerId, Index) :-
    (   process_chunk_caught(DB, Wrap, Docs, PreBranchCommitId, PreSchemaLayerId, Elaborated, Error)
    ->  (   var(Error)
        ->  send_chunk_done(OwnerId, Index, Elaborated)
        ;   send_chunk_error(OwnerId, Index, Error)
        )
    ;   send_chunk_error(OwnerId, Index, unknown_error)
    ).

process_chunk_caught(DB, Wrap, Docs, PreBranchCommitId, PreSchemaLayerId, Elaborated, Error) :-
    catch(
        process_chunk(DB, Wrap, Docs, PreBranchCommitId, PreSchemaLayerId, Elaborated),
        Error,
        true
    ).

start_elaboration_workers(N) :-
    start_workers(N).

stop_elaboration_workers :-
    stop_workers.

multi_purpose_worker_loop :-
    thread_self(Me),
    thread_at_exit(commit_queue:cleanup_active_branches_for_worker(Me)),
    multi_purpose_worker_loop_body.

multi_purpose_worker_loop_body :-
    (   multi_purpose_workers_should_stop
    ->  true
    ;   (   worker_should_commit_first
        ->  (   maybe_help_with_elaboration
            ->  true
            ;   commit_queue:try_commit_work
            ->  true
            ;   idle_worker_wait
            )
        ;   (   commit_queue:try_commit_work
            ->  true
            ;   maybe_help_with_elaboration
            ->  true
            ;   idle_worker_wait
            )
        ),
        multi_purpose_worker_loop_body
    ).

% worker_should_commit_first is true when the worker is allowed to prefer
% elaboration over commit work. If a commit has been pending for more than 2
% seconds, we force commit work first to avoid starvation of queued commits.
worker_should_commit_first :-
    worker_prefers_elaboration,
    \+ commit_queue:pending_commit_stale(2.0).

worker_prefers_elaboration :-
    config:worker_elaboration_preference(P),
    random(X),
    X < P.

idle_worker_wait :-
    thread_self(Me),
    thread_get_message(Me, _, []).

:- thread_local commit_window_guard_depth/2.

% True when the commit-window guard is actually useful. The server starts
% its elaboration worker pool, so it has concurrent transactions in the same
% process that need to be serialized via the process-local change window.
% The CLI runs each command in a fresh process, so its change window is
% always empty and the guard would deadlock.
commit_window_guard_applicable :-
    multi_purpose_worker_thread(_).

with_commit_window_guard(_Transaction, _BranchKey, _CommitId, Goal) :-
    \+ commit_window_guard_applicable,
    !,
    Goal.
with_commit_window_guard(Transaction, BranchKey, CommitId, Goal) :-
    (   acquire_commit_window_guard(Transaction, BranchKey, CommitId)
    ->  (   catch(Goal, Exception,
                    (   release_all_commit_window_guards,
                        throw(Exception)
                    ))
        ->  release_all_commit_window_guards
        ;   release_all_commit_window_guards,
            fail
        )
    ;   throw(fail_transaction)
    ).

acquire_commit_window_guard(Transaction, BranchKey, CommitId) :-
    (   retract(commit_window_guard_depth(D, Info))
    ->  D1 is D + 1,
        assertz(commit_window_guard_depth(D1, Info))
    ;   open_commit_window_for_branch_head(Transaction, BranchKey, CommitId, 200, GuardId),
        reset_transaction_object_graph_descriptors(Transaction),
        assertz(commit_window_guard_depth(1, (BranchKey, CommitId, GuardId)))
    ).

acquire_commit_window_guard(BranchKey, CommitId) :-
    (   retract(commit_window_guard_depth(D, Info))
    ->  D1 is D + 1,
        assertz(commit_window_guard_depth(D1, Info))
    ;   open_commit_window_with_retry(BranchKey, CommitId, 200, GuardId),
        assertz(commit_window_guard_depth(1, (BranchKey, CommitId, GuardId)))
    ).

open_commit_window_for_branch_head(Transaction, BranchKey, CommitId, Retries, GuardId) :-
    pre_branch_commit_id(Transaction, CommitId),
    (   first_commit_candidate(Transaction, CommitId)
    ->  open_first_commit_window_with_retry(BranchKey, CommitId, Retries, GuardId)
    ;   open_commit_window_with_retry(BranchKey, CommitId, Retries, GuardId)
    ).

first_commit_candidate(_Transaction, none).

open_first_commit_window_with_retry(BranchKey, CommitId, Retries, GuardId) :-
    (   string(CommitId)
    ->  CommitIdString = CommitId
    ;   atom_string(CommitId, CommitIdString)
    ),
    (   '$change_window':open_first_commit_window(BranchKey, CommitIdString, GuardId, _CurrentCommitId)
    ->  true
    ;   Retries > 0
    ->  sleep(0.05),
        Retries1 is Retries - 1,
        open_first_commit_window_with_retry(BranchKey, CommitId, Retries1, GuardId)
    ;   fail
    ).

open_commit_window_with_retry(BranchKey, CommitId, Retries, GuardId) :-
    (   atom(CommitId)
    ->  CommitIdAtom = CommitId
    ;   atom_string(CommitIdAtom, CommitId)
    ),
    (   '$change_window':open_commit_window(BranchKey, CommitIdAtom, GuardId, _CurrentCommitId)
    ->  true
    ;   Retries > 0
    ->  sleep(0.05),
        Retries1 is Retries - 1,
        open_commit_window_with_retry(BranchKey, CommitId, Retries1, GuardId)
    ;   fail
    ).

release_commit_window_guard :-
    (   retract(commit_window_guard_depth(D, Info))
    ->  D1 is D - 1,
        (   D1 > 0
        ->  assertz(commit_window_guard_depth(D1, Info))
        ;   Info = (_, _, GuardId),
            '$change_window':close_commit_window(GuardId)
        )
    ;   true
    ).

release_all_commit_window_guards :-
    (   retract(commit_window_guard_depth(_, Info))
    ->  Info = (_, _, GuardId),
        '$change_window':close_commit_window(GuardId),
        release_all_commit_window_guards
    ;   true
    ).

has_commit_window_guard :-
    commit_window_guard_depth(D, _),
    D > 0.


% Tests
% -----

:- begin_tests(parallel_elaboration, [concurrent(true)]).

:- use_module(core(document)).
:- use_module(core(document/json)).
:- use_module(core(query)).
:- use_module(core(transaction)).

test_document_schema_string(Schema) :-
    atomics_to_string(
        [ '{"@type":"@context","@base":"terminusdb:///data/","@schema":"terminusdb:///schema#"}\n',
          '{"@id":"Person","@type":"Class","@key":{"@type":"Random"},"name":"xsd:string","age":"xsd:integer"}\n' ],
        Schema).

lexical_person_schema_string(Schema) :-
    atomics_to_string(
        [ '{"@type":"@context","@base":"terminusdb:///data/","@schema":"terminusdb:///schema#"}\n',
          '{"@id":"Person","@type":"Class","@key":{"@type":"Lexical","@fields":["name"]},"name":"xsd:string","age":"xsd:integer"}\n' ],
        Schema).

parallel_subdocument_schema(Schema) :-
    atomics_to_string(
        [ '{"@type":"@context","@base":"terminusdb:///data/","@schema":"terminusdb:///schema#"}\n',
          '{"@id":"Address","@type":"Class","@subdocument":[],"@key":{"@type":"Random"},"street":"xsd:string"}\n',
          '{"@id":"Person","@type":"Class","@key":{"@type":"Random"},"name":"xsd:string","age":"xsd:integer","address":"Address"}\n' ],
        Schema).

generate_documents(N, Docs) :-
    findall(
        _{ '@type': 'Person', name: Name, age: Age },
        (   between(1, N, I),
            format(atom(Name), 'Person~|~`0t~d~4+', [I]),
            Age is 20 + (I mod 60)
        ),
        Docs
    ).

helper_docs(Count, Docs) :-
    generate_documents(Count, Docs).

helper_subdocs(Count, Docs) :-
    findall(
        _{ '@type': 'Person', name: Name, age: Age,
           address: _{ '@type': 'Address', street: Street } },
        (   between(1, Count, I),
            format(atom(Name), 'Person~|~`0t~d~4+', [I]),
            Age is 20 + (I mod 60),
            format(atom(Street), 'Street~|~`0t~d~4+', [I])
        ),
        Docs).

test(chunk_size_for_request_caps_at_default, [setup(assertz(max_chunk_size(1000))),
                                                cleanup(retractall(max_chunk_size(_)))]) :-
    chunk_size_for_request(10000, 1000), !.

test(chunk_size_for_request_uses_doc_count_below_default, [setup(assertz(max_chunk_size(1000))),
                                                            cleanup(retractall(max_chunk_size(_)))]) :-
    chunk_size_for_request(100, 100), !.

test(simple_docs_elaborate, [
         setup((setup_temp_store(State),
                test_document_label_descriptor(Desc),
                test_document_schema_string(Schema),
                write_schema_string(Schema, Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    helper_docs(10, Docs),
    elaborate_insert_request(Desc, Docs, Elaborated, [workers(4)]),
    length(Elaborated, 10), !.

test(explicit_id_falls_back_to_prolog, [
         setup((setup_temp_store(State),
                test_document_label_descriptor(Desc),
                test_document_schema_string(Schema),
                write_schema_string(Schema, Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    helper_docs(10, Docs),
    Docs = [First|Rest],
    put_dict('@id', First, 'Person/explicit', BadFirst),
    BadDocs = [BadFirst|Rest],
    elaborate_insert_request(Desc, BadDocs, Elaborated, [workers(4)]),
    length(Elaborated, 10), !.

test(capture_falls_back_to_prolog, [
         setup((setup_temp_store(State),
                test_document_label_descriptor(Desc),
                test_document_schema_string(Schema),
                write_schema_string(Schema, Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    helper_docs(10, Docs),
    Docs = [First|Rest],
    put_dict('@capture', First, "group1", BadFirst),
    BadDocs = [BadFirst|Rest],
    elaborate_insert_request(Desc, BadDocs, Elaborated, [workers(4)]),
    length(Elaborated, 10), !.

test(subdocument_falls_back_to_prolog, [
         setup((setup_temp_store(State),
                test_document_label_descriptor(Desc),
                parallel_subdocument_schema(Schema),
                write_schema_string(Schema, Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    helper_subdocs(10, BadDocs),
    elaborate_insert_request(Desc, BadDocs, Elaborated, [workers(4)]),
    length(Elaborated, 10), !.

test(simple_random_insert_correctness, [
         setup((setup_temp_store(State),
                test_document_label_descriptor(Desc),
                test_document_schema_string(Schema),
                write_schema_string(Schema, Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    helper_docs(100, Docs),
    elaborate_insert_request(Desc, Docs, Elaborated, [workers(4)]),
    length(Elaborated, 100),
    forall(member(E, Elaborated),
           (   get_dict('@id', E, _),
               get_dict('@type', E, _)
           )),
    open_descriptor(Desc, Transaction),
    create_context(Transaction, commit_info{author: "test", message: "test"}, Context),
    with_transaction(Context,
                     (   query_default_collection(Context, TO),
                         forall(member(E, Elaborated),
                                insert_document_expanded(TO, E, _))
                     ),
                     _),
    findall(Id, get_document_uri(Context, false, Id), Ids),
    length(Ids, 100).

test(result_ordering, [
         setup((setup_temp_store(State),
                test_document_label_descriptor(Desc),
                test_document_schema_string(Schema),
                write_schema_string(Schema, Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    helper_docs(50, Docs),
    findall(Name,
            (member(D, Docs), get_dict(name, D, Name)),
            InputNames),
    elaborate_insert_request(Desc, Docs, Elaborated, [workers(4)]),
    findall(Name,
            (member(E, Elaborated),
             get_dict('terminusdb:///schema#name', E, NameObj),
             get_dict('@value', NameObj, Name)),
            OutputNames),
    InputNames = OutputNames.

test(no_duplicate_ids, [
         setup((setup_temp_store(State),
                test_document_label_descriptor(Desc),
                test_document_schema_string(Schema),
                write_schema_string(Schema, Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    helper_docs(500, Docs),
    elaborate_insert_request(Desc, Docs, Elaborated, [workers(8)]),
    findall(Id, (member(E, Elaborated), get_dict('@id', E, Id)), Ids),
    length(Ids, 500),
    sort(Ids, Sorted),
    length(Sorted, 500), !.

test(thread_safety_2_workers, [
         setup((setup_temp_store(State),
                test_document_label_descriptor(Desc),
                test_document_schema_string(Schema),
                write_schema_string(Schema, Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    helper_docs(200, Docs),
    elaborate_insert_request(Desc, Docs, Elaborated, [workers(2)]),
    length(Elaborated, 200), !.

test(thread_safety_8_workers, [
         setup((setup_temp_store(State),
                test_document_label_descriptor(Desc),
                test_document_schema_string(Schema),
                write_schema_string(Schema, Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    helper_docs(200, Docs),
    elaborate_insert_request(Desc, Docs, Elaborated, [workers(8)]),
    length(Elaborated, 200), !.

test(fallback_to_prolog_for_explicit_id, [
         setup((setup_temp_store(State),
                test_document_label_descriptor(Desc),
                test_document_schema_string(Schema),
                write_schema_string(Schema, Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    helper_docs(10, Docs),
    Docs = [First|Rest],
    put_dict('@id', First, 'Person/explicit', BadFirst),
    BadDocs = [BadFirst|Rest],
    elaborate_insert_request(Desc, BadDocs, Elaborated, [workers(2)]),
    length(Elaborated, 10),
    !.

test(empty_input, [
         setup((setup_temp_store(State),
                test_document_label_descriptor(Desc),
                test_document_schema_string(Schema),
                write_schema_string(Schema, Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    elaborate_insert_request(Desc, [], Elaborated, [workers(2)]),
    Elaborated = [], !.

test(single_worker, [
         setup((setup_temp_store(State),
                test_document_label_descriptor(Desc),
                test_document_schema_string(Schema),
                write_schema_string(Schema, Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    helper_docs(100, Docs),
    elaborate_insert_request(Desc, Docs, Elaborated, [workers(1)]),
    length(Elaborated, 100), !.

group_set_of_people_schema_string(Schema) :-
    atomics_to_string(
        [ '{"@type":"@context","@base":"terminusdb:///data/","@schema":"terminusdb:///schema#"}\n',
          '{"@id":"Person","@type":"Class","@key":{"@type":"Lexical","@fields":["name"]},"name":"xsd:string","age":"xsd:decimal","order":"xsd:integer"}\n',
          '{"@id":"Group","@type":"Class","people":{"@type":"Set","@class":"Person"}}\n' ],
        Schema).

test(set_of_people_elaborates, [
         setup((setup_temp_store(State),
                test_document_label_descriptor(Desc),
                group_set_of_people_schema_string(Schema),
                write_schema_string(Schema, Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    Kant = _{'@type':'Person', name:"Immanuel Kant", age:"79", order:"3"},
    Popper = _{'@type':'Person', name:"Karl Popper", age:"92", order:"5"},
    Godel = _{'@type':'Person', name:"Kurt Gödel", age:"71", order:"5"},
    Docs = [_{'@id':'Group/0', '@type':'Group', people:[Kant, Popper, Godel]},
            _{'@id':'Group/1', '@type':'Group', people:[]},
            _{'@id':'Group/2', '@type':'Group'}],
    open_descriptor(Desc, Transaction),
    elaborate_insert_request_db_with_contracts(Transaction, Docs, Pairs, [workers(4), wrap_api_errors(true)]),
    length(Pairs, 3),
    Pairs = [_-KantContract|_],
    get_dict(id_pairs, KantContract, KantIdPairs),
    member('terminusdb:///data/Person/Immanuel%20Kant'-normal, KantIdPairs).

elaboration_without_id(Elab, Normalized) :-
    put_dict('@id', Elab, "<id>", Normalized).

% Normalize a value to a string for comparison. Prolog elaboration
% returns atoms for IRIs and string literals; Rust elaboration returns
% strings. Both are semantically equivalent, so we compare in a common
% representation.
stringify_value(Value, String) :-
    (   atom(Value)
    ->  atom_string(Value, String)
    ;   string(Value)
    ->  String = Value
    ;   Value = String
    ).

stringify_json_value(Value, Stringified) :-
    (   is_dict(Value)
    ->  dict_pairs(Value, Tag, Pairs),
        maplist([Key-Orig, Key-Str]>>(stringify_json_value(Orig, Str)),
                Pairs, NormalizedPairs),
        dict_pairs(Stringified, Tag, NormalizedPairs)
    ;   is_list(Value)
    ->  maplist(stringify_json_value, Value, Stringified)
    ;   stringify_value(Value, Stringified)
    ).

elaboration_for_comparison(Elab, Compared) :-
    elaboration_without_id(Elab, WithoutId),
    stringify_json_value(WithoutId, Compared).

id_prefix_matches(Id, Prefix) :-
    (   atom(Id)
    ->  atom_concat(Prefix, _, Id)
    ;   sub_atom(Id, 0, _, _, Prefix)
    ).

test(rust_elaboration_matches_prolog_simple, [
         setup((setup_temp_store(State),
                test_document_label_descriptor(Desc),
                test_document_schema_string(Schema),
                write_schema_string(Schema, Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    Doc = json{'@type':'Person', name:'Alice', age:30},
    open_descriptor(Desc, DB),
    json_elaborate(DB, Doc, Prolog_Elaborated),
    (   rust_elaboration_available
    ->  '$doc':get_document_context(DB, Context),
        '$doc':rust_elaborate_simple_documents(Context, [Doc], none, none, [Rust_Elaborated-_Rust_Contract]),
        elaboration_for_comparison(Prolog_Elaborated, Prolog_Normalized),
        elaboration_for_comparison(Rust_Elaborated, Rust_Normalized),
        Prolog_Normalized = Rust_Normalized,
        get_dict('@id', Prolog_Elaborated, Prolog_Id),
        get_dict('@id', Rust_Elaborated, Rust_Id),
        id_prefix_matches(Prolog_Id, 'terminusdb:///data/Person/'),
        id_prefix_matches(Rust_Id, 'terminusdb:///data/Person/')
    ;   true
    ),
    !.

test(rust_elaboration_contract_for_simple_doc, [
         setup((setup_temp_store(State),
                test_document_label_descriptor(Desc),
                test_document_schema_string(Schema),
                write_schema_string(Schema, Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    (   rust_elaboration_available
    ->  Doc = json{'@type':'Person', name:'Alice', age:30},
        open_descriptor(Desc, DB),
        elaborate_insert_request_db_with_contracts(DB, [Doc], [Elaborated-Contract], [workers(1)]),
        is_dict(Contract, verification_contract),
        get_dict(submitted_id, Contract, none),
        get_dict(generated_id, Contract, GeneratedId),
        atom(GeneratedId),
        get_dict(id_pairs, Contract, [GeneratedId-normal]),
        get_dict(dependencies, Contract, []),
        get_dict(backlinks, Contract, []),
        is_dict(Elaborated, json),
        get_dict('@type', Elaborated, 'terminusdb:///schema#Person'),
        get_dict('terminusdb:///schema#age', Elaborated, AgeObj),
        get_dict('@type', AgeObj, 'http://www.w3.org/2001/XMLSchema#integer'),
        get_dict('@value', AgeObj, 30),
        get_dict('terminusdb:///schema#name', Elaborated, NameObj),
        get_dict('@type', NameObj, 'http://www.w3.org/2001/XMLSchema#string'),
        get_dict('@value', NameObj, 'Alice')
    ;   true
    ),
    !.

test(rust_elaboration_fails_on_missing_required_field, [
         setup((setup_temp_store(State),
                test_document_label_descriptor(Desc),
                test_document_schema_string(Schema),
                write_schema_string(Schema, Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    (   rust_elaboration_available
    ->  Doc = json{'@type':'Person', age:30},
        open_descriptor(Desc, DB),
        '$doc':get_document_context(DB, Context),
        % The Rust fast path must refuse to elaborate a document that is
        % missing a required schema property, so the Prolog inference fallback
        % can generate the standard required_field_does_not_exist_in_document
        % witness.
        \+ '$doc':rust_elaborate_simple_documents(Context, [Doc], none, none, _)
    ;   true
    ),
    !.

test(rust_vs_prolog_elaboration_performance, [
         setup((setup_temp_store(State),
                test_document_label_descriptor(Desc),
                test_document_schema_string(Schema),
                write_schema_string(Schema, Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    (   rust_elaboration_available
    ->  DocCount = 10000,
        helper_docs(DocCount, Docs),
        open_descriptor(Desc, DB),
        '$doc':get_document_context(DB, Context),
        % Warm-up to avoid cold-start effects.
        '$doc':rust_elaborate_simple_documents(Context, [json{'@type':'Person', name:'Warm', age:1}], none, none, _),
        forall(member(D, Docs), json_elaborate(DB, D, _)),
        % Prolog path timing.
        statistics(walltime, [PrologT0, _]),
        forall(member(D, Docs), json_elaborate(DB, D, _)),
        statistics(walltime, [PrologT1, _]),
        PrologMs is PrologT1 - PrologT0,
        PrologDocsPerS is (DocCount * 1000) / max(PrologMs, 1),
        % Rust path timing.
        statistics(walltime, [RustT0, _]),
        '$doc':rust_elaborate_simple_documents(Context, Docs, none, none, _),
        statistics(walltime, [RustT1, _]),
        RustMs is RustT1 - RustT0,
        RustDocsPerS is (DocCount * 1000) / max(RustMs, 1),
        assertion(RustDocsPerS > PrologDocsPerS)
    ;   true
    ),
    !.

test(parallel_replace_contract, [
         setup((setup_temp_store(State),
                test_document_label_descriptor(Desc),
                test_document_schema_string(Schema),
                write_schema_string(Schema, Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    helper_docs(10, InsertDocs),
    findall(ExplicitDoc,
            (   nth1(I, InsertDocs, D),
                format(atom(ExplicitId), 'Person/explicit~|~`0t~d~3+', [I]),
                put_dict('@id', D, ExplicitId, ExplicitDoc)
            ),
            ExplicitDocs),
    with_test_transaction(Desc, C1,
        (   query_default_collection(C1, TO1),
            elaborate_insert_request_db(TO1, ExplicitDocs, Elaborated, [workers(4)]),
            forall(member(E, Elaborated),
                   insert_document_expanded(TO1, E, _))
        ), _),
    findall(ReplaceDoc,
            (   member(E, Elaborated),
                get_dict('@id', E, Id),
                get_dict('terminusdb:///schema#name', E, NameObj),
                get_dict('@value', NameObj, OldName),
                get_dict('terminusdb:///schema#age', E, AgeObj),
                get_dict('@value', AgeObj, Age),
                atom_concat(OldName, '_replaced', NewName),
                put_dict('@id', json{'@type':'Person', name: NewName, age: Age}, Id, ReplaceDoc)
            ),
            ReplaceDocs),
    with_test_transaction(Desc, C2,
        (   query_default_collection(C2, TO2),
            elaborate_insert_request_db_with_contracts(TO2, ReplaceDocs, Pairs, [workers(4)]),
            forall(member(Doc-Contract, Pairs),
                   replace_document_expanded(TO2, Doc, Contract, false, _))
        ), _),
    findall(Name,
            (   between(1, 10, I),
                format(atom(ExplicitId), 'Person/explicit~|~`0t~d~3+', [I]),
                get_document(Desc, ExplicitId, Doc, [compress_ids(true)]),
                get_dict(name, Doc, Name)
            ),
            Names),
    length(Names, 10),
    forall(member(Name, Names), sub_atom(Name, _, _, 0, '_replaced')),
    !.

test(sync_replace_contract_comparison, [
         setup((setup_temp_store(State),
                test_document_label_descriptor(Desc),
                test_document_schema_string(Schema),
                write_schema_string(Schema, Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    helper_docs(1, [InsertDoc]),
    put_dict('@id', InsertDoc, 'Person/sync', SyncDoc),
    with_test_transaction(Desc, C1,
        (   query_default_collection(C1, TO1),
            elaborate_insert_request_db(TO1, [SyncDoc], [Elaborated], [workers(4)]),
            insert_document_expanded(TO1, Elaborated, _)
        ), _),
    get_dict('terminusdb:///schema#name', Elaborated, NameObj),
    get_dict('@value', NameObj, OldName),
    get_dict('terminusdb:///schema#age', Elaborated, AgeObj),
    get_dict('@value', AgeObj, Age),
    atom_concat(OldName, '_sync', NewName),
    ReplaceDoc = json{'@id':'Person/sync', '@type':'Person', name: NewName, age: Age},
    with_test_transaction(Desc, C2,
        (   query_default_collection(C2, TO2),
            replace_document(TO2, ReplaceDoc, false, false, _)
        ), _),
    get_document(Desc, 'terminusdb:///data/Person/sync', Doc, [compress_ids(true)]),
    get_dict(name, Doc, Name),
    sub_atom(Name, _, _, 0, '_sync'),
    !.

test(parallel_replace_contract_rejects_mismatched_id, [
         setup((setup_temp_store(State),
                test_document_label_descriptor(Desc),
                lexical_person_schema_string(Schema),
                write_schema_string(Schema, Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    InsertDoc = json{'@type':'Person', name:'Alice', age:30},
    with_test_transaction(Desc, C1,
        (   query_default_collection(C1, TO1),
            elaborate_insert_request_db(TO1, [InsertDoc], [Elaborated], [workers(4)]),
            insert_document_expanded(TO1, Elaborated, _)
        ), _),
    MismatchReplace = json{'@id':'Person/Alice', '@type':'Person', name:'Bob', age:30},
    catch(
        with_test_transaction(Desc, C2,
            (   query_default_collection(C2, TO2),
                elaborate_insert_request_db_with_contracts(TO2, [MismatchReplace], [Doc-Contract], [workers(4)]),
                replace_document_expanded(TO2, Doc, Contract, false, _)
            ), _),
        Error,
        true
    ),
    Error = error(submitted_id_does_not_match_generated_id(_,_), _),
    !.

test(parallel_replace_contract_rejects_missing_document, [
         setup((setup_temp_store(State),
                test_document_label_descriptor(Desc),
                test_document_schema_string(Schema),
                write_schema_string(Schema, Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    helper_docs(1, [InsertDoc]),
    with_test_transaction(Desc, C1,
        (   query_default_collection(C1, TO1),
            elaborate_insert_request_db(TO1, [InsertDoc], [Elaborated], [workers(4)]),
            insert_document_expanded(TO1, Elaborated, _)
        ), _),
    MissingReplace = json{'@id':'Person/missing', '@type':'Person', name:'Missing', age:99},
    with_test_transaction(Desc, C2,
        (   query_default_collection(C2, TO2),
            elaborate_insert_request_db_with_contracts(TO2, [MissingReplace], [Doc-Contract], [workers(4)]),
            catch(
                replace_document_expanded(TO2, Doc, Contract, false, _),
                Error,
                true
            ),
            Error = error(document_not_found('terminusdb:///data/Person/missing', _), _)
        ), _),
    !.

test(helper_elaboration_produces_ground_contract, [
         setup((setup_temp_store(State),
                test_document_label_descriptor(Desc),
                test_document_schema_string(Schema),
                write_schema_string(Schema, Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    helper_docs(1, [Doc]),
    open_descriptor(Desc, DB),
    snapshot_ids(DB, PreBranchCommitId, PreSchemaLayerId),
    create_request_queue(none, DB, [chunk(0, [Doc])], 1, false,
                         PreBranchCommitId, PreSchemaLayerId, RequestId),
    maybe_help_with_elaboration,
    thread_get_message(RequestId, chunk_done(0, [_Doc-Contract])),
    cleanup_request(RequestId),
    get_dict(pre_schema_layer_id, Contract, GotSchemaLayerId),
    assertion(ground(GotSchemaLayerId)),
    get_dict(pre_branch_commit_id, Contract, GotBranchCommitId),
    assertion(ground(GotBranchCommitId)).

:- end_tests(parallel_elaboration).
