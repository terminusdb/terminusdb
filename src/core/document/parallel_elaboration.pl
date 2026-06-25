:- module(parallel_elaboration, [
              elaborate_insert_request/4,
              is_insert_eligible/1,
              maybe_help_with_elaboration/0,
              chunk_size/1
          ]).

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
 * Elaborate a list of simple new documents in parallel using a work-stealing
 * chunk queue.  Elaboration is performed outside the transaction window; the
 * caller is responsible for opening a transaction and inserting the returned
 * elaborated documents.
 *
 * The predicate fails if the documents are not eligible for the parallel path
 * (e.g., explicit @id, captures, subdocuments, references).  The caller should
 * then fall back to the synchronous insert path.
 */
elaborate_insert_request(Descriptor, Docs, Elaborated, Options) :-
    is_insert_eligible(Docs),
    !,
    chunk_size(ChunkSize),
    chunks_from_documents(Docs, ChunkSize, Chunks),
    length(Chunks, TotalChunks),
    gensym(request, RequestId),
    message_queue_create(RequestId, [alias(RequestId)]),
    open_descriptor(Descriptor, DB),
    assertz(request_queue(RequestId, Descriptor, DB, Chunks, TotalChunks)),
    worker_count(Options, Workers),
    Helpers is Workers - 1,
    spawn_workers(Helpers, RequestId, HelperThreads),
    worker_loop(RequestId),
    collect_results(RequestId, TotalChunks, Elaborated),
    join_threads(HelperThreads),
    cleanup_request(RequestId).

elaborate_insert_request(_, _, _, _) :-
    fail.

is_insert_eligible(Docs) :-
    is_list(Docs),
    forall(member(Doc, Docs), is_simple_doc(Doc)).

is_simple_doc(Doc) :-
    is_dict(Doc),
    get_dict('@type', Doc, Type),
    (   atom(Type)
    ;   string(Type)
    ),
    \+ get_dict('@id', Doc, _),
    \+ get_dict('@capture', Doc, _),
    dict_pairs(Doc, _, Pairs),
    forall(member(Key-Value, Pairs), is_simple_field(Key, Value)).

is_simple_field('@type', _) :- !.
is_simple_field('@id', _) :- !, fail.
is_simple_field('@capture', _) :- !, fail.
is_simple_field(_, Value) :-
    is_simple_value(Value).

is_simple_value(Value) :-
    atom(Value),
    !.
is_simple_value(Value) :-
    string(Value),
    !.
is_simple_value(Value) :-
    number(Value),
    !.
is_simple_value(Value) :-
    is_list(Value),
    !,
    forall(member(V, Value), is_simple_value(V)).
is_simple_value(_) :-
    fail.

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

worker_count(Options, Workers) :-
    (   option(workers(Workers), Options)
    ->  true
    ;   worker_amount(Workers)
    ).

spawn_workers(0, _, []) :- !.
spawn_workers(N, RequestId, [Thread|Rest]) :-
    N > 0,
    thread_create(worker_loop(RequestId), Thread, []),
    N1 is N - 1,
    spawn_workers(N1, RequestId, Rest).

join_threads(Threads) :-
    forall(member(Thread, Threads),
           (   thread_join(Thread, Status),
               (   Status == true
               ->  true
               ;   format(user_error, 'helper thread ~q exited with status ~q~n', [Thread, Status])
               )
           )).

worker_loop(RequestId) :-
    (   take_chunk(RequestId, OwnerId, DB, chunk(Index, Docs))
    ->  (   catch(process_chunk(DB, Docs, Elaborated), Error,
                   (   format(user_error, 'chunk ~d error: ~q~n', [Index, Error]),
                       fail
                   ))
        ->  thread_send_message(OwnerId, chunk_done(Index, Elaborated))
        ;   return_chunk(OwnerId, chunk(Index, Docs))
        ),
        worker_loop(RequestId)
    ;   true
    ).

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

return_chunk(OwnerId, chunk(Index, Docs)) :-
    with_mutex(elaboration_queue_mutex,
               (   request_queue(OwnerId, Descriptor, DB, Pending, Total),
                   retractall(request_queue(OwnerId, _, _, _, _)),
                   assertz(request_queue(OwnerId, Descriptor, DB, [chunk(Index, Docs)|Pending], Total))
               )).

rust_elaboration_available :-
    current_predicate('$doc':rust_elaborate_simple_documents/3).

rust_elaborate_chunk(DB, Docs, Elaborated) :-
    rust_elaboration_available,
    !,
    catch(
        (   '$doc':get_document_context(DB, Context),
            '$doc':rust_elaborate_simple_documents(Context, Docs, Elaborated)
        ),
        Error,
        (   format(user_error, 'rust elaboration failed: ~q~n', [Error]),
            fail
        )
    ).
rust_elaborate_chunk(DB, Docs, Elaborated) :-
    maplist({DB}/[Doc, Elab]>>(
                empty_assoc(Captures_In),
                json_elaborate(DB, Doc, Captures_In, Elab,
                               _Ids, _Dependencies, []-[], _Captures_Out)
            ),
            Docs,
            Elaborated).

process_chunk(DB, Docs, Elaborated) :-
    rust_elaborate_chunk(DB, Docs, Elaborated).

collect_results(RequestId, TotalChunks, Elaborated) :-
    collect_results(RequestId, TotalChunks, [], Pairs),
    sort(Pairs, Sorted),
    pairs_values(Sorted, ChunkList),
    append(ChunkList, Elaborated).

collect_results(_, 0, Pairs, Pairs) :- !.
collect_results(RequestId, N, Acc, Pairs) :-
    thread_get_message(RequestId, chunk_done(Index, ChunkElaborated)),
    N1 is N - 1,
    collect_results(RequestId, N1, [Index-ChunkElaborated|Acc], Pairs).

cleanup_request(RequestId) :-
    retractall(request_queue(RequestId, _, _, _, _)),
    message_queue_destroy(RequestId).

/**
 * maybe_help_with_elaboration is det.
 *
 * Short hook for idle HTTP workers.  Try to steal and process exactly one
 * chunk from any registered request queue, then return.
 */
maybe_help_with_elaboration :-
    (   request_queue(RequestId, _, _, _, _),
        take_chunk(RequestId, OwnerId, DB, chunk(Index, Docs))
    ->  (   catch(process_chunk(DB, Docs, Elaborated), Error,
                   (   format(user_error, 'help chunk ~d error: ~q~n', [Index, Error]),
                       fail
                   ))
        ->  thread_send_message(OwnerId, chunk_done(Index, Elaborated))
        ;   return_chunk(OwnerId, chunk(Index, Docs))
        )
    ;   true
    ).


% Tests
% -----

:- begin_tests(parallel_elaboration, [concurrent(true)]).

:- use_module(core(document)).
:- use_module(core(document/json)).
:- use_module(core(document/write_path_profile)).
:- use_module(core(query)).
:- use_module(core(transaction)).

parallel_subdocument_schema(Schema) :-
    atomics_to_string(
        [ '{"@type":"@context","@base":"terminusdb:///data/","@schema":"terminusdb:///schema#"}\n',
          '{"@id":"Address","@type":"Class","@subdocument":[],"@key":{"@type":"Random"},"street":"xsd:string"}\n',
          '{"@id":"Person","@type":"Class","@key":{"@type":"Random"},"name":"xsd:string","age":"xsd:integer","address":"Address"}\n' ],
        Schema).

test_document_schema_string(Schema) :-
    test_document_schema(Schema).

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

test(eligible_simple_docs, [
         setup((setup_temp_store(State),
                test_document_label_descriptor(Desc),
                test_document_schema_string(Schema),
                write_schema_string(Schema, Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    helper_docs(10, Docs),
    is_insert_eligible(Docs).

test(ineligible_with_id, [
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
    \+ is_insert_eligible(BadDocs).

test(ineligible_with_capture, [
         setup((setup_temp_store(State),
                test_document_label_descriptor(Desc),
                test_document_schema_string(Schema),
                write_schema_string(Schema, Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    helper_docs(10, Docs),
    Docs = [First|Rest],
    put_dict('@capture', First, 'group1', BadFirst),
    BadDocs = [BadFirst|Rest],
    \+ is_insert_eligible(BadDocs).

test(ineligible_subdocument, [
         setup((setup_temp_store(State),
                test_document_label_descriptor(Desc),
                parallel_subdocument_schema(Schema),
                write_schema_string(Schema, Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-
    helper_subdocs(10, BadDocs),
    \+ is_insert_eligible(BadDocs).

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

test(fallback_ineligible_returns_failure, [
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
    \+ elaborate_insert_request(Desc, BadDocs, _, [workers(2)]),
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
