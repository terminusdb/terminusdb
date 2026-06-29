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
              process_one_commit/2,
              process_commit_batch/3,
              wake_workers/0,
              invalidate_pending_after_schema_change/1,
              destroy_branch_queue/1,
              start_workers/1,
              stop_workers/0,
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
% pending_branch_age(BranchKey, Timestamp) records when BranchKey was first
% registered as having queued commits. It is used to detect starving commits.
:- dynamic pending_branch_age/2.

% Worker pool state.
:- dynamic multi_purpose_worker_thread/1.
:- dynamic multi_purpose_workers_should_stop/0.

% Commit batch size: number of queued packages processed while holding a branch
% lock. Hardcoded to 1: benchmarks showed no meaningful throughput gain from
% larger batch sizes, so we favor smooth latency/fairness across branches over
% lock-amortization.
commit_batch_size(1).

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
        (   (   pending_branch_age(BranchKey, _)
            ->  true
            ;   get_time(Now),
                assertz(pending_branch_age(BranchKey, Now))
            ),
            (   pending_branches(Branches)
            ->  (   member(BranchKey, Branches)
                ->  true
                ;   retract(pending_branches(Branches)),
                    append(Branches, [BranchKey], NewBranches),
                    assertz(pending_branches(NewBranches))
                )
            ;   assertz(pending_branches([BranchKey]))
            )
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
                ->  Requeued = true
                ;   branch_commit_queue(BranchKey, Queue),
                    \+ queue_empty(Queue)
                ->  retract(pending_branches(Branches)),
                    append(Branches, [BranchKey], NewBranches),
                    assertz(pending_branches(NewBranches)),
                    Requeued = true
                ;   Requeued = false
                )
            ;   (   branch_commit_queue(BranchKey, Queue),
                    \+ queue_empty(Queue)
                ->  assertz(pending_branches([BranchKey])),
                    Requeued = true
                ;   Requeued = false
                )
            ),
            (   Requeued == true
            ->  (   pending_branch_age(BranchKey, _)
                ->  true
                ;   get_time(Now),
                    assertz(pending_branch_age(BranchKey, Now))
                )
            ;   retractall(pending_branch_age(BranchKey, _))
            )
        )).

% pending_commit_stale(+ThresholdSeconds) is semidet.
%
% True if any pending branch has been waiting for longer than ThresholdSeconds.
% This is used by elaboration workers to force a commit attempt before taking
% elaboration work, preventing commit starvation.
pending_commit_stale(ThresholdSeconds) :-
    pending_branch_age(_, Timestamp),
    get_time(Now),
    Now - Timestamp > ThresholdSeconds,
    !.

cleanup_active_branches_for_worker(WorkerId) :-
    with_mutex(commit_scheduler_mutex,
        forall(retract(active_branch(_, WorkerId)), true)).

try_commit_work :-
    thread_self(WorkerId),
    pick_next_branch(BranchKey, WorkerId),
    commit_batch_size(BatchSize),
    (   acquire_branch_lock(BranchKey, [timeout(0)])
    ->  setup_call_cleanup(
            true,
            % Batch size: number of queued commit packages to process while
            % holding the branch lock before releasing it and letting another
            % worker pick up any remaining work. This amortizes lock-acquisition
            % cost over multiple commits without starving other branches.
            process_commit_batch(BranchKey, BatchSize, true),
            (   release_branch_lock(BranchKey),
                requeue_branch(BranchKey)
            )
        )
    ;   requeue_branch(BranchKey),
        fail
    ).

process_one_commit(BranchKey, true) :-
    branch_commit_queue(BranchKey, Queue),
    thread_get_message(Queue, Package, [timeout(0)]),
    !,
    catch(
        api_document:run_commit_package(Package),
        Error,
        deliver_commit_error(Package, Error)
    ).
process_one_commit(_, false).

process_commit_batch(BranchKey, Max, true) :-
    Max > 0,
    branch_commit_queue(BranchKey, Queue),
    thread_get_message(Queue, Package, [timeout(0)]),
    !,
    catch(
        api_document:run_commit_package(Package),
        Error,
        deliver_commit_error(Package, Error)
    ),
    Max1 is Max - 1,
    process_commit_batch(BranchKey, Max1, _).
process_commit_batch(_, _, false).

% Send an error result back to the original requester's reply queue. The queue
% may already be gone if the requester has exited, in which case the failure is
% ignored so that the worker can continue with the next commit.
deliver_commit_error(Package, Error) :-
    get_dict(reply_queue, Package, ReplyQueue),
    get_dict(request_id, Package, RequestId),
    catch(thread_send_message(ReplyQueue,
                              commit_result(error(Error), RequestId)),
          error(existence_error(message_queue, _), _),
          true).

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
        (   catch(thread_join(Thread, _), _, true),
            cleanup_active_branches_for_worker(Thread)
        )
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

test(register_pending_branch_records_age) :-
    with_mutex(commit_scheduler_mutex,
        (   retractall(commit_queue:pending_branches(_)),
            retractall(commit_queue:pending_branch_age(_, _))
        )),
    register_pending_branch('age_b1'),
    assertion(commit_queue:pending_branch_age('age_b1', _)),
    with_mutex(commit_scheduler_mutex,
        (   retractall(commit_queue:pending_branches(_)),
            retractall(commit_queue:pending_branch_age(_, _))
        )).

test(requeue_branch_preserves_age_when_not_empty) :-
    with_mutex(commit_scheduler_mutex,
        (   retractall(commit_queue:pending_branches(_)),
            retractall(commit_queue:active_branch(_, _)),
            retractall(commit_queue:pending_branch_age(_, _))
        )),
    ensure_branch_queue('age_b1', Queue),
    register_pending_branch('age_b1'),
    commit_queue:pending_branch_age('age_b1', OriginalAge),
    thread_self(Self),
    assertz(commit_queue:active_branch('age_b1', Self)),
    thread_send_message(Queue, dummy),
    requeue_branch('age_b1'),
    commit_queue:pending_branch_age('age_b1', NewAge),
    assertion(NewAge == OriginalAge),
    with_mutex(commit_scheduler_mutex,
        (   retractall(commit_queue:pending_branches(_)),
            retractall(commit_queue:active_branch(_, _)),
            retractall(commit_queue:pending_branch_age(_, _))
        )),
    destroy_branch_queue('age_b1').

test(requeue_branch_retracts_age_when_empty) :-
    with_mutex(commit_scheduler_mutex,
        (   retractall(commit_queue:pending_branches(_)),
            retractall(commit_queue:active_branch(_, _)),
            retractall(commit_queue:pending_branch_age(_, _))
        )),
    ensure_branch_queue('age_b2', _Queue),
    register_pending_branch('age_b2'),
    thread_self(Self),
    pick_next_branch('age_b2', Self),
    assertion(commit_queue:pending_branch_age('age_b2', _)),
    % Queue is empty, so requeue_branch should not requeue and should retract age.
    requeue_branch('age_b2'),
    assertion(\+ commit_queue:pending_branch_age('age_b2', _)),
    with_mutex(commit_scheduler_mutex,
        (   retractall(commit_queue:pending_branches(_)),
            retractall(commit_queue:active_branch(_, _)),
            retractall(commit_queue:pending_branch_age(_, _))
        )),
    destroy_branch_queue('age_b2').

test(pending_commit_stale_detects_old_commit) :-
    with_mutex(commit_scheduler_mutex,
        (   retractall(commit_queue:pending_branch_age(_, _))
        )),
    get_time(Now),
    OldTime is Now - 3.0,
    assertz(commit_queue:pending_branch_age('stale_b', OldTime)),
    assertion(pending_commit_stale(2.0)),
    with_mutex(commit_scheduler_mutex,
        (   retractall(commit_queue:pending_branch_age(_, _))
        )).

test(pending_commit_stale_ignores_fresh_commit) :-
    with_mutex(commit_scheduler_mutex,
        (   retractall(commit_queue:pending_branch_age(_, _))
        )),
    get_time(Now),
    assertz(commit_queue:pending_branch_age('fresh_b', Now)),
    assertion(\+ pending_commit_stale(2.0)),
    with_mutex(commit_scheduler_mutex,
        (   retractall(commit_queue:pending_branch_age(_, _))
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
    process_one_commit('test_process', true),
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
    process_one_commit('test_process_error', true),
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

% Test that process_commit_batch processes multiple queued packages in one go.
test(process_commit_batch_processes_multiple_commits, [setup(asserta(api_document:commit_package_test_handler(commit_queue:success_handler))), cleanup(retractall(api_document:commit_package_test_handler(_)))]) :-
    cleanup_workers_and_branches,
    destroy_branch_queue('test_batch_multi'),
    ensure_branch_queue('test_batch_multi', Queue),
    message_queue_create(ReplyQueue, []),
    forall(
        between(1, 3, N),
        (   atom_concat(req_multi_, N, ReqId),
            Package = commit_package{
                branch_key: 'test_batch_multi',
                all_branches: ['test_batch_multi'],
                reply_queue: ReplyQueue,
                request_id: ReqId,
                test_marker: true
            },
            thread_send_message(Queue, Package)
        )
    ),
    process_commit_batch('test_batch_multi', 10, true),
    forall(
        between(1, 3, N),
        (   atom_concat(req_multi_, N, ReqId),
            thread_get_message(ReplyQueue, commit_result(success(test_meta, [test_id]), ReqId), [timeout(1)])
        )
    ),
    message_queue_destroy(ReplyQueue),
    destroy_branch_queue('test_batch_multi').

% Test that process_commit_batch respects the Max limit and leaves excess packages on the queue.
test(process_commit_batch_respects_max, [setup(asserta(api_document:commit_package_test_handler(commit_queue:success_handler))), cleanup(retractall(api_document:commit_package_test_handler(_)))]) :-
    cleanup_workers_and_branches,
    destroy_branch_queue('test_batch_max'),
    ensure_branch_queue('test_batch_max', Queue),
    message_queue_create(ReplyQueue, []),
    forall(
        between(1, 5, N),
        (   atom_concat(req_max_, N, ReqId),
            Package = commit_package{
                branch_key: 'test_batch_max',
                all_branches: ['test_batch_max'],
                reply_queue: ReplyQueue,
                request_id: ReqId,
                test_marker: true
            },
            thread_send_message(Queue, Package)
        )
    ),
    process_commit_batch('test_batch_max', 2, true),
    forall(
        between(1, 2, N),
        (   atom_concat(req_max_, N, ReqId),
            thread_get_message(ReplyQueue, commit_result(success(test_meta, [test_id]), ReqId), [timeout(1)])
        )
    ),
    assertion(message_queue_property(Queue, size(3))),
    message_queue_destroy(ReplyQueue),
    destroy_branch_queue('test_batch_max').

% Test that process_commit_batch returns false when the queue is empty.
test(process_commit_batch_empty_returns_false, [setup(asserta(api_document:commit_package_test_handler(commit_queue:success_handler))), cleanup(retractall(api_document:commit_package_test_handler(_)))]) :-
    cleanup_workers_and_branches,
    destroy_branch_queue('test_batch_empty'),
    ensure_branch_queue('test_batch_empty', _Queue),
    process_commit_batch('test_batch_empty', 10, false),
    destroy_branch_queue('test_batch_empty').

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
