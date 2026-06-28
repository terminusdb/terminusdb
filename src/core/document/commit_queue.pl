:- module(commit_queue, [
              ensure_branch_queue/2,
              acquire_branch_lock/1,
              acquire_branch_lock/2,
              release_branch_lock/1,
              enqueue_commit/2,
              pick_next_branch/2,
              requeue_branch/1,
              register_pending_branch/1,
              cleanup_active_branches_for_worker/1,
              try_commit_work/0,
              process_one_commit/1,
              wake_workers/0,
              invalidate_pending_after_schema_change/1,
              destroy_branch_queue/1,
              start_workers/1,
              stop_workers/0,
              maybe_help_with_commit/1,
              multi_purpose_worker_thread/1,
              multi_purpose_workers_should_stop/0,
              branch_commit_queue/2,
              branch_commit_lock/2,
              active_branch/2,
              pending_branches/1
          ]).

:- use_module(config(terminus_config), [worker_amount/1]).
:- use_module(core(util)).
:- use_module(core(triple), [triple_store/1, with_triple_store/2]).
:- use_module(library(apply)).
:- use_module(library(aggregate), [aggregate_all/3]).
:- use_module(library(lists)).
:- use_module(library(plunit)).

% Branch-specific commit queue and lock registry.
:- dynamic branch_commit_queue/2.
:- dynamic branch_commit_lock/2.

% Round-robin scheduler state.
:- dynamic pending_branches/1.
:- dynamic active_branch/2.

% Worker pool state.
:- dynamic multi_purpose_worker_thread/1.
:- dynamic multi_purpose_workers_should_stop/0.

:- mutex_create(commit_scheduler_mutex).
:- mutex_create(branch_registry_mutex).

wake_workers :-
    (   multi_purpose_worker_thread(_)
    ->  forall(
            multi_purpose_worker_thread(Thread),
            catch(thread_send_message(Thread, wake(Thread)), _, true)
        )
    ;   true
    ).

ensure_branch_queue(BranchKey, Queue) :-
    (   branch_commit_queue(BranchKey, Queue)
    ->  true
    ;   with_mutex(branch_registry_mutex,
            (   branch_commit_queue(BranchKey, Queue)
            ->  true
            ;   atom_string(BranchKeyAtom, BranchKey),
                atom_concat('commit_queue_', BranchKeyAtom, QueueAlias),
                message_queue_create(Queue, [alias(QueueAlias)]),
                assertz(branch_commit_queue(BranchKey, Queue)),
                atom_concat('commit_lock_', BranchKeyAtom, LockAlias),
                mutex_create(Mutex, [alias(LockAlias)]),
                assertz(branch_commit_lock(BranchKey, Mutex))
            ))
    ).

destroy_branch_queue(BranchKey) :-
    with_mutex(branch_registry_mutex,
        (   (   retract(branch_commit_queue(BranchKey, Queue))
            ->  catch(message_queue_destroy(Queue), _, true)
            ;   true
            ),
            (   retract(branch_commit_lock(BranchKey, Mutex))
            ->  catch(mutex_destroy(Mutex), _, true)
            ;   true
            )
        )).

acquire_branch_lock(BranchKey) :-
    branch_commit_lock(BranchKey, Mutex),
    mutex_lock(Mutex).

acquire_branch_lock(BranchKey, Options) :-
    branch_commit_lock(BranchKey, Mutex),
    (   member(timeout(0), Options)
    ->  mutex_trylock(Mutex)
    ;   mutex_lock(Mutex)
    ).

release_branch_lock(BranchKey) :-
    (   branch_commit_lock(BranchKey, Mutex)
    ->  catch(mutex_unlock(Mutex), _, true)
    ;   true
    ).

queue_empty(Queue) :-
    catch(message_queue_property(Queue, size(0)), _, fail).

enqueue_commit(BranchKey, Package) :-
    ensure_branch_queue(BranchKey, Queue),
    thread_send_message(Queue, Package),
    register_pending_branch(BranchKey),
    wake_workers.

register_pending_branch(BranchKey) :-
    with_mutex(commit_scheduler_mutex,
        (   pending_branches(Branches)
        ->  (   member(BranchKey, Branches)
            ->  true
            ;   retract(pending_branches(Branches)),
                append(Branches, [BranchKey], NewBranches),
                assertz(pending_branches(NewBranches))
            )
        ;   assertz(pending_branches([BranchKey]))
        )).

pick_next_branch(BranchKey, WorkerId) :-
    with_mutex(commit_scheduler_mutex,
        (   pending_branches(Branches),
            pick_first_non_active(Branches, BranchKey, Rest)
        ->  retract(pending_branches(Branches)),
            assertz(pending_branches(Rest)),
            assertz(active_branch(BranchKey, WorkerId))
        ;   fail
        )).

pick_first_non_active([Branch|Rest], Branch, Rest) :-
    \+ active_branch(Branch, _),
    !.
pick_first_non_active([Branch|Rest], Picked, [Branch|Remaining]) :-
    pick_first_non_active(Rest, Picked, Remaining).

requeue_branch(BranchKey) :-
    thread_self(WorkerId),
    with_mutex(commit_scheduler_mutex,
        (   (   retract(active_branch(BranchKey, WorkerId))
            ->  true
            ;   true  % tolerate stale cleanup from a different worker
            ),
            (   pending_branches(Branches)
            ->  (   member(BranchKey, Branches)
                ->  true
                ;   branch_commit_queue(BranchKey, Queue),
                    \+ queue_empty(Queue)
                ->  retract(pending_branches(Branches)),
                    append(Branches, [BranchKey], NewBranches),
                    assertz(pending_branches(NewBranches))
                ;   true  % queue is empty; do not requeue
                )
            ;   (   branch_commit_queue(BranchKey, Queue),
                    \+ queue_empty(Queue)
                ->  assertz(pending_branches([BranchKey]))
                ;   true
                )
            )
        )).

cleanup_active_branches_for_worker(WorkerId) :-
    with_mutex(commit_scheduler_mutex,
        forall(retract(active_branch(_, WorkerId)), true)).

try_commit_work :-
    thread_self(WorkerId),
    pick_next_branch(BranchKey, WorkerId),
    (   acquire_branch_lock(BranchKey, [timeout(0)])
    ->  setup_call_cleanup(
            true,
            process_one_commit(BranchKey),
            (   release_branch_lock(BranchKey),
                requeue_branch(BranchKey)
            )
        )
    ;   requeue_branch(BranchKey),
        fail
    ).

process_one_commit(BranchKey) :-
    branch_commit_queue(BranchKey, Queue),
    thread_get_message(Queue, Package, [timeout(0)]),
    catch(
        api_document:run_commit_package(Package),
        Error,
        (   get_dict(reply_queue, Package, ReplyQueue),
            get_dict(request_id, Package, RequestId),
            catch(thread_send_message(ReplyQueue,
                                     commit_result(error(Error), RequestId)),
                  error(existence_error(message_queue, _), _),
                  true)
        )
    ).

invalidate_pending_after_schema_change(BranchKey) :-
    branch_commit_queue(BranchKey, Queue),
    (   thread_get_message(Queue, Package, [timeout(0)])
    ->  catch(thread_send_message(Package.reply_queue,
                                  commit_result(reject(schema_changed), Package.request_id)),
              error(existence_error(message_queue, _), _),
              true),
        invalidate_pending_after_schema_change(BranchKey)
    ;   true
    ).

maybe_help_with_commit(Package) :-
    try_commit_work,
    Package = _.

start_workers(0) :- !.
start_workers(N) :-
    N > 0,
    stop_workers,
    retractall(multi_purpose_workers_should_stop),
    start_workers_(N).

start_workers_(0) :- !.
start_workers_(N) :-
    N > 0,
    triple_store(Store),
    format(atom(Alias), 'multi_purpose_worker_~d', [N]),
    thread_create(
        with_triple_store(Store, parallel_elaboration:multi_purpose_worker_loop),
        Thread, [alias(Alias)]),
    assertz(multi_purpose_worker_thread(Thread)),
    N1 is N - 1,
    start_workers_(N1).

stop_workers :-
    retractall(multi_purpose_workers_should_stop),
    assertz(multi_purpose_workers_should_stop),
    (   multi_purpose_worker_thread(_)
    ->  forall(
            multi_purpose_worker_thread(Thread),
            catch(thread_send_message(Thread, stop), _, true)
        )
    ;   true
    ),
    forall(
        retract(multi_purpose_worker_thread(Thread)),
        catch(thread_join(Thread, _), _, true)
    ),
    retractall(multi_purpose_worker_thread(_)).

% Unit tests for the scheduler and registry.
:- begin_tests(commit_queue).

test(ensure_branch_queue_creates_queue_and_lock) :-
    destroy_branch_queue('test_branch'),
    ensure_branch_queue('test_branch', Queue),
    branch_commit_queue('test_branch', Queue),
    branch_commit_lock('test_branch', Mutex),
    assertion(message_queue_property(Queue, alias(_))),
    assertion(message_queue_property(Queue, size(0))),
    assertion(mutex_property(Mutex, alias(_))),
    destroy_branch_queue('test_branch').

test(register_and_pick_pending_branch) :-
    with_mutex(commit_scheduler_mutex,
        (   retractall(commit_queue:pending_branches(_)),
            retractall(commit_queue:active_branch(_, _))
        )),
    register_pending_branch('b1'),
    register_pending_branch('b2'),
    thread_self(Self),
    pick_next_branch(BranchKey, Self),
    assertion(BranchKey == 'b1'),
    assertion(commit_queue:active_branch('b1', Self)),
    with_mutex(commit_scheduler_mutex,
        (   retractall(commit_queue:pending_branches(_)),
            retractall(commit_queue:active_branch(_, _))
        )).

test(pick_next_branch_skips_active_branch) :-
    with_mutex(commit_scheduler_mutex,
        (   retractall(commit_queue:pending_branches(_)),
            retractall(commit_queue:active_branch(_, _))
        )),
    register_pending_branch('b1'),
    register_pending_branch('b2'),
    thread_self(Self),
    assertz(commit_queue:active_branch('b1', other_worker)),
    pick_next_branch(BranchKey, Self),
    assertion(BranchKey == 'b2'),
    with_mutex(commit_scheduler_mutex,
        (   retractall(commit_queue:pending_branches(_)),
            retractall(commit_queue:active_branch(_, _))
        )).

test(requeue_branch_appends_when_queue_not_empty) :-
    with_mutex(commit_scheduler_mutex,
        (   retractall(commit_queue:pending_branches(_)),
            retractall(commit_queue:active_branch(_, _))
        )),
    ensure_branch_queue('b1', Queue),
    ensure_branch_queue('b2', _),
    register_pending_branch('b2'),
    thread_self(Self),
    assertz(commit_queue:active_branch('b1', Self)),
    thread_send_message(Queue, dummy),
    requeue_branch('b1'),
    commit_queue:pending_branches(Branches),
    assertion(Branches == ['b2', 'b1']),
    with_mutex(commit_scheduler_mutex,
        (   retractall(commit_queue:pending_branches(_)),
            retractall(commit_queue:active_branch(_, _))
        )),
    destroy_branch_queue('b1'),
    destroy_branch_queue('b2').

test(cleanup_active_branches_for_worker) :-
    with_mutex(commit_scheduler_mutex,
        (   retractall(commit_queue:pending_branches(_)),
            retractall(commit_queue:active_branch(_, _))
        )),
    assertz(commit_queue:active_branch('b1', worker_1)),
    assertz(commit_queue:active_branch('b2', worker_1)),
    assertz(commit_queue:active_branch('b3', worker_2)),
    cleanup_active_branches_for_worker(worker_1),
    assertion(\+ commit_queue:active_branch('b1', worker_1)),
    assertion(\+ commit_queue:active_branch('b2', worker_1)),
    assertion(commit_queue:active_branch('b3', worker_2)),
    with_mutex(commit_scheduler_mutex,
        (   retractall(commit_queue:pending_branches(_)),
            retractall(commit_queue:active_branch(_, _))
        )).

% Test that enqueue_commit places the package on the branch queue and
% registers it as pending, without requiring workers or a full commit.
test(enqueue_commit_puts_package_in_queue) :-
    cleanup_workers_and_branches,
    destroy_branch_queue('test_enqueue'),
    ensure_branch_queue('test_enqueue', Queue),
    Package = package{branch_key: 'test_enqueue'},
    enqueue_commit('test_enqueue', Package),
    assertion(message_queue_property(Queue, size(1))),
    thread_get_message(Queue, RetrievedPackage, [timeout(0)]),
    assertion(RetrievedPackage = Package),
    with_mutex(commit_scheduler_mutex,
        (   retractall(commit_queue:pending_branches(_)),
            retractall(commit_queue:active_branch(_, _))
        )),
    destroy_branch_queue('test_enqueue').

% Test that process_one_commit invokes the commit handler and delivers a
% success result back to the reply queue.
test(process_one_commit_delivers_success, [setup(asserta(api_document:commit_package_test_handler(commit_queue:success_handler))), cleanup(retractall(api_document:commit_package_test_handler(_)))]) :-
    cleanup_workers_and_branches,
    destroy_branch_queue('test_process'),
    ensure_branch_queue('test_process', Queue),
    message_queue_create(ReplyQueue, []),
    Package = commit_package{
        branch_key: 'test_process',
        all_branches: ['test_process'],
        reply_queue: ReplyQueue,
        request_id: req_1,
        test_marker: true
    },
    thread_send_message(Queue, Package),
    process_one_commit('test_process'),
    thread_get_message(ReplyQueue, commit_result(success(test_meta, [test_id]), req_1), [timeout(1)]),
    message_queue_destroy(ReplyQueue),
    destroy_branch_queue('test_process').

% Test that process_one_commit catches errors from the commit handler and
% delivers them as error results.
test(process_one_commit_delivers_error, [setup(asserta(api_document:commit_package_test_handler(commit_queue:error_handler))), cleanup(retractall(api_document:commit_package_test_handler(_)))]) :-
    cleanup_workers_and_branches,
    destroy_branch_queue('test_process_error'),
    ensure_branch_queue('test_process_error', Queue),
    message_queue_create(ReplyQueue, []),
    Package = commit_package{
        branch_key: 'test_process_error',
        all_branches: ['test_process_error'],
        reply_queue: ReplyQueue,
        request_id: req_2,
        test_marker_error: true
    },
    thread_send_message(Queue, Package),
    process_one_commit('test_process_error'),
    thread_get_message(ReplyQueue, commit_result(error(test_error), req_2), [timeout(1)]),
    message_queue_destroy(ReplyQueue),
    destroy_branch_queue('test_process_error').

% Test that try_commit_work picks up an enqueued commit and delivers the
% result to the reply queue, using the current thread as the worker.
test(try_commit_work_processes_enqueued_commit, [setup(asserta(api_document:commit_package_test_handler(commit_queue:success_handler))), cleanup(retractall(api_document:commit_package_test_handler(_)))]) :-
    cleanup_workers_and_branches,
    destroy_branch_queue('test_try'),
    ensure_branch_queue('test_try', _Queue),
    message_queue_create(ReplyQueue, []),
    Package = commit_package{
        branch_key: 'test_try',
        all_branches: ['test_try'],
        reply_queue: ReplyQueue,
        request_id: req_3,
        test_marker: true
    },
    enqueue_commit('test_try', Package),
    try_commit_work,
    thread_get_message(ReplyQueue, commit_result(success(test_meta, [test_id]), req_3), [timeout(1)]),
    message_queue_destroy(ReplyQueue),
    destroy_branch_queue('test_try').

% Test that start_workers and stop_workers create and terminate the
% worker pool correctly.
test(start_and_stop_workers) :-
    cleanup_workers_and_branches,
    start_workers(2),
    aggregate_all(count, multi_purpose_worker_thread(_), Count),
    assertion(Count == 2),
    stop_workers,
    aggregate_all(count, multi_purpose_worker_thread(_), Count2),
    assertion(Count2 == 0).

% Test that a real worker thread picks up an enqueued commit and delivers
% the result to the reply queue.
test(worker_processes_enqueued_commit, [setup(asserta(api_document:commit_package_test_handler(commit_queue:success_handler))), cleanup(retractall(api_document:commit_package_test_handler(_)))]) :-
    cleanup_workers_and_branches,
    destroy_branch_queue('test_worker'),
    ensure_branch_queue('test_worker', _Queue),
    message_queue_create(ReplyQueue, []),
    Package = commit_package{
        branch_key: 'test_worker',
        all_branches: ['test_worker'],
        reply_queue: ReplyQueue,
        request_id: req_4,
        test_marker: true
    },
    start_workers(1),
    enqueue_commit('test_worker', Package),
    thread_get_message(ReplyQueue, commit_result(success(test_meta, [test_id]), req_4), [timeout(2)]),
    stop_workers,
    message_queue_destroy(ReplyQueue),
    destroy_branch_queue('test_worker').

% Test that wake_workers can wake a worker that is blocked in
% idle_worker_wait. The worker loop should continue and the worker should
% be stoppable afterwards.
test(wake_workers_wakes_idle_worker) :-
    cleanup_workers_and_branches,
    start_workers(1),
    sleep(0.05),
    wake_workers,
    stop_workers.

% Test that the SWI-Prolog thread communication primitives used by the
% queue design continue to work: a thread can wait on its own message
% queue and be woken by thread_send_message to its thread id.
test(thread_send_message_to_thread_id_reaches_own_queue) :-
    message_queue_create(Barrier, []),
    thread_create(commit_queue:test_receive_thread(Barrier, hello), Thread, [alias(test_receiver)]),
    thread_send_message(Thread, hello),
    thread_get_message(Barrier, done, [timeout(1)]),
    thread_join(Thread, true),
    message_queue_destroy(Barrier).

% Test that thread_get_message with a short timeout returns without a
% message and does not hang indefinitely. This protects against SWI-Prolog
% changes that alter timeout semantics.
test(thread_get_message_short_timeout_returns) :-
    message_queue_create(Q, []),
    (   thread_get_message(Q, _, [timeout(0.001)])
    ->  fail
    ;   true
    ),
    message_queue_destroy(Q).

% Test that a shared message queue can be used to wake multiple workers
% from a single broadcast.
test(shared_queue_wakes_multiple_workers) :-
    message_queue_create(SharedQueue, [alias(shared_queue_test)]),
    thread_create(commit_queue:test_shared_queue_worker(SharedQueue, 1), Thread1, [alias(shared_worker_1)]),
    thread_create(commit_queue:test_shared_queue_worker(SharedQueue, 2), Thread2, [alias(shared_worker_2)]),
    sleep(0.05),
    thread_send_message(SharedQueue, wake),
    thread_send_message(SharedQueue, wake),
    thread_join(Thread1, true),
    thread_join(Thread2, true),
    message_queue_destroy(SharedQueue).

:- end_tests(commit_queue).

% Test helpers. These live in the commit_queue module so they can be
% passed as handlers to api_document:execute_commit_package/2 and invoked
% from worker threads created by tests.

cleanup_workers_and_branches :-
    stop_workers,
    with_mutex(commit_scheduler_mutex,
        (   retractall(commit_queue:pending_branches(_)),
            retractall(commit_queue:active_branch(_, _))
        )).

success_handler(P, success(test_meta, [test_id])) :-
    get_dict(test_marker, P, true).

error_handler(P, error(test_error)) :-
    get_dict(test_marker_error, P, true).

test_receive_thread(Barrier, Expected) :-
    thread_self(Me),
    thread_get_message(Me, Expected, []),
    thread_send_message(Barrier, done).

test_shared_queue_worker(Queue, _Id) :-
    thread_get_message(Queue, wake, [timeout(1)]),
    assertion(wake == wake).
