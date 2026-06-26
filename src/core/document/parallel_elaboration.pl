:- module(parallel_elaboration, [
              elaborate_insert_request/4,
              elaborate_insert_request_db/4,
              maybe_help_with_elaboration/0,
              start_elaboration_workers/1,
              with_processing_token/1,
              acquire_processing_token/0,
              release_processing_token/0,
              has_processing_token/0,
              chunk_size/1
          ]).

:- meta_predicate with_processing_token(0).

:- use_module(core(document)).
:- use_module(core(document/json)).
:- use_module(core(transaction)).
:- use_module(core(util)).
:- use_module(core(util/test_utils)).

:- use_module(config(terminus_config), [worker_amount/1]).

:- use_module(library(apply)).
:- use_module(library(assoc)).
:- use_module(library(gensym)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(pairs)).
:- use_module(library(plunit)).
:- use_module(library(yall)).

:- dynamic request_queue/5.
% request_queue(RequestId, Descriptor, DB, PendingChunks, TotalChunks).

:- dynamic chunk_size/1.
chunk_size(100).

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
elaborate_insert_request(Descriptor, Docs, Elaborated, _Options) :-
    chunk_size(ChunkSize),
    chunks_from_documents(Docs, ChunkSize, Chunks),
    length(Chunks, TotalChunks),
    open_descriptor(Descriptor, DB),
    setup_call_cleanup(
        create_request_queue(Descriptor, DB, Chunks, TotalChunks, RequestId),
        collect_and_process(RequestId, TotalChunks, Elaborated),
        cleanup_request(RequestId)
    ).

elaborate_insert_request_db(DB, Docs, Elaborated, _Options) :-
    chunk_size(ChunkSize),
    chunks_from_documents(Docs, ChunkSize, Chunks),
    length(Chunks, TotalChunks),
    setup_call_cleanup(
        create_request_queue(none, DB, Chunks, TotalChunks, RequestId),
        collect_and_process(RequestId, TotalChunks, Elaborated),
        cleanup_request(RequestId)
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

create_request_queue(Descriptor, DB, Chunks, TotalChunks, RequestId) :-
    gensym(request, RequestId),
    message_queue_create(RequestId, [alias(RequestId)]),
    assertz(request_queue(RequestId, Descriptor, DB, Chunks, TotalChunks)),
    init_worker_wakeup_queue,
    wake_elaboration_workers.

wake_elaboration_workers :-
    (   queue_exists(worker_wakeup_queue)
    ->  worker_amount(Workers),
        wake_elaboration_workers_(Workers)
    ;   true
    ).

wake_elaboration_workers_(0) :- !.
wake_elaboration_workers_(N) :-
    N > 0,
    thread_send_message(worker_wakeup_queue, wake),
    N1 is N - 1,
    wake_elaboration_workers_(N1).

queue_exists(Queue) :-
    catch(message_queue_property(Queue, _), _, fail).

init_worker_wakeup_queue :-
    (   queue_exists(worker_wakeup_queue)
    ->  true
    ;   catch(message_queue_create(worker_wakeup_queue, []), _, true)
    ).

collect_and_process(RequestId, TotalChunks, Elaborated) :-
    collect_and_process_(RequestId, TotalChunks, [], Pairs),
    sort(Pairs, Sorted),
    pairs_values(Sorted, ChunkList),
    append(ChunkList, Elaborated).

collect_and_process_(_, 0, Pairs, Pairs) :- !.
collect_and_process_(RequestId, Remaining, Pairs, FinalPairs) :-
    (   take_chunk(RequestId, _OwnerId, DB, chunk(Index, Docs))
    ->  (   process_chunk(DB, Docs, Elaborated)
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
handle_chunk_message(_RequestId, chunk_error(_Index), _Remaining, _Pairs, _FinalPairs) :-
    throw(error(parallel_elaboration_failed, _)).

send_chunk_done(OwnerId, Index, Elaborated) :-
    catch(thread_send_message(OwnerId, chunk_done(Index, Elaborated)), _, true).

send_chunk_error(OwnerId, Index) :-
    catch(thread_send_message(OwnerId, chunk_error(Index)), _, true).

take_chunk(RequestId, OwnerId, DB, Chunk) :-
    with_mutex(elaboration_queue_mutex,
               (   request_queue(RequestId, _, DB0, [Chunk|Rest], Total)
               ->  OwnerId = RequestId,
                   DB = DB0,
                   retractall(request_queue(RequestId, _, _, _, _)),
                   assertz(request_queue(RequestId, _, DB0, Rest, Total))
               ;   request_queue(OwnerId, _, DB0, [Chunk|Rest], Total),
                   OwnerId \= RequestId
               ->  DB = DB0,
                   retractall(request_queue(OwnerId, _, _, _, _)),
                   assertz(request_queue(OwnerId, _, DB0, Rest, Total))
               ;   fail
               )).

rust_elaboration_available :-
    current_predicate('$doc':rust_elaborate_simple_documents/3).

rust_elaborate_chunk(DB, Docs, Elaborated) :-
    rust_elaboration_available,
    !,
    (   catch(
            (   '$doc':get_document_context(DB, Context),
                '$doc':rust_elaborate_simple_documents(Context, Docs, Elaborated)
            ),
            Error,
            (   format(user_error, 'rust chunk elaboration failed: ~q~n', [Error]),
                fail
            ))
    ->  true
    ;   prolog_elaborate_chunk(DB, Docs, Elaborated)
    ).
rust_elaborate_chunk(DB, Docs, Elaborated) :-
    prolog_elaborate_chunk(DB, Docs, Elaborated).

process_chunk(DB, Docs, Elaborated) :-
    rust_elaborate_chunk(DB, Docs, Elaborated).

prolog_elaborate_chunk(DB, Docs, Elaborated) :-
    maplist(json_elaborate(DB), Docs, Elaborated).

cleanup_request(RequestId) :-
    retractall(request_queue(RequestId, _, _, _, _)),
    message_queue_destroy(RequestId).

/**
 * maybe_help_with_elaboration is semidet.
 *
 * Short hook for idle HTTP workers.  Try to steal and process exactly one
 * chunk from any registered request queue, then return.  Fails when no chunk
 * is available.
 */
maybe_help_with_elaboration :-
    request_queue(RequestId, _, _, _, _),
    take_chunk(RequestId, OwnerId, DB, chunk(Index, Docs)),
    with_processing_token(
        (   catch(process_chunk(DB, Docs, Elaborated), Error,
                   (   format(user_error, 'help chunk ~d error: ~q~n', [Index, Error]),
                       fail
                   ))
        ->  send_chunk_done(OwnerId, Index, Elaborated)
        ;   send_chunk_error(OwnerId, Index)
        )).

start_elaboration_workers(0) :- !.
start_elaboration_workers(N) :-
    N > 0,
    TokenCount is N + 1,
    token_pool_init(TokenCount),
    init_worker_wakeup_queue,
    start_elaboration_workers_(N).

start_elaboration_workers_(0) :- !.
start_elaboration_workers_(N) :-
    N > 0,
    format(atom(Alias), 'elaboration_worker_~d', [N]),
    thread_create(elaboration_worker_loop, _, [alias(Alias)]),
    N1 is N - 1,
    start_elaboration_workers_(N1).

elaboration_worker_loop :-
    (   maybe_help_with_elaboration
    ->  true
    ;   thread_get_message(worker_wakeup_queue, wake)
    ),
    elaboration_worker_loop.

token_pool_init(N) :-
    (   catch(message_queue_property(elaboration_tokens, _), _, fail)
    ->  true
    ;   message_queue_create(elaboration_tokens, []),
        fill_tokens(N)
    ).

fill_tokens(0) :- !.
fill_tokens(N) :-
    N > 0,
    thread_send_message(elaboration_tokens, token),
    N1 is N - 1,
    fill_tokens(N1).

:- thread_local processing_token_depth/1.

with_processing_token(Goal) :-
    setup_call_cleanup(
        acquire_processing_token,
        Goal,
        release_all_processing_tokens
    ).

acquire_processing_token :-
    (   retract(processing_token_depth(D))
    ->  D1 is D + 1,
        assertz(processing_token_depth(D1))
    ;   thread_get_message(elaboration_tokens, _),
        assertz(processing_token_depth(1))
    ).

release_processing_token :-
    (   retract(processing_token_depth(D))
    ->  D1 is D - 1,
        (   D1 > 0
        ->  assertz(processing_token_depth(D1))
        ;   thread_send_message(elaboration_tokens, _)
        )
    ;   true
    ).

release_all_processing_tokens :-
    (   retract(processing_token_depth(D))
    ->  release_all_processing_tokens_(D)
    ;   true
    ).

release_all_processing_tokens_(0) :- !.
release_all_processing_tokens_(D) :-
    D > 0,
    thread_send_message(elaboration_tokens, _),
    D1 is D - 1,
    release_all_processing_tokens_(D1).

has_processing_token :-
    processing_token_depth(D),
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

elaboration_without_id(Elab, Normalized) :-
    put_dict('@id', Elab, '<id>', Normalized).

id_prefix_matches(Id, Prefix) :-
    atom_concat(Prefix, _, Id).

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
        '$doc':rust_elaborate_simple_documents(Context, [Doc], [Rust_Elaborated]),
        elaboration_without_id(Prolog_Elaborated, Prolog_Normalized),
        elaboration_without_id(Rust_Elaborated, Rust_Normalized),
        Prolog_Normalized = Rust_Normalized,
        get_dict('@id', Prolog_Elaborated, Prolog_Id),
        get_dict('@id', Rust_Elaborated, Rust_Id),
        id_prefix_matches(Prolog_Id, 'terminusdb:///data/Person/'),
        id_prefix_matches(Rust_Id, 'terminusdb:///data/Person/')
    ;   true
    ),
    !.

test(rust_elaboration_matches_prolog_list, [
         setup((setup_temp_store(State),
                test_document_label_descriptor(Desc),
                test_document_schema_string(Schema),
                write_schema_string(Schema, Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    helper_docs(50, Docs),
    open_descriptor(Desc, DB),
    maplist(json_elaborate(DB), Docs, Prolog_Elaborated),
    (   rust_elaboration_available
    ->  '$doc':get_document_context(DB, Context),
        '$doc':rust_elaborate_simple_documents(Context, Docs, Rust_Elaborated),
        maplist(elaboration_without_id, Prolog_Elaborated, Prolog_Normalized),
        maplist(elaboration_without_id, Rust_Elaborated, Rust_Normalized),
        Prolog_Normalized = Rust_Normalized,
        forall(member(E, Prolog_Elaborated),
               (   get_dict('@id', E, Id),
                   id_prefix_matches(Id, 'terminusdb:///data/Person/')
               )),
        forall(member(E, Rust_Elaborated),
               (   get_dict('@id', E, Id),
                   id_prefix_matches(Id, 'terminusdb:///data/Person/')
               ))
    ;   true
    ),
    !.

:- end_tests(parallel_elaboration).
