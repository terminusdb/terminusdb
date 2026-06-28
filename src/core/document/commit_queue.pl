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
              init_worker_wakeup_queue/0,
              worker_wakeup_queue/1,
              branch_commit_queue/2,
              branch_commit_lock/2,
              active_branch/2,
              pending_branches/1
          ]).

:- use_module(config(terminus_config), [worker_amount/1]).
:- use_module(core(util)).
:- use_module(core(triple), [triple_store/1, with_triple_store/2]).
:- use_module(library(apply)).
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
:- mutex_create(worker_wakeup_queue_mutex).

:- dynamic worker_wakeup_queue_handle/1.

init_worker_wakeup_queue :-
    (   worker_wakeup_queue_handle(_)
    ->  true
    ;   with_mutex(worker_wakeup_queue_mutex,
                   (   worker_wakeup_queue_handle(_)
                   ->  true
                   ;   message_queue_create(Queue, [alias(worker_wakeup_queue)]),
                       assertz(worker_wakeup_queue_handle(Queue))
                   ))
    ).

worker_wakeup_queue(Queue) :-
    worker_wakeup_queue_handle(Queue),
    !.
worker_wakeup_queue(Queue) :-
    init_worker_wakeup_queue,
    worker_wakeup_queue_handle(Queue).

wake_workers :-
    QueueAlias = worker_wakeup_queue,
    !,
    (   multi_purpose_worker_thread(_)
    ->  forall(
            multi_purpose_worker_thread(Thread),
            catch(thread_send_message(QueueAlias, wake(Thread)), _, true)
        )
    ;   true
    ).

wake_workers.

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
    init_worker_wakeup_queue,
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
    worker_wakeup_queue(Queue),
    (   multi_purpose_worker_thread(_)
    ->  forall(
            multi_purpose_worker_thread(_Thread),
            catch(thread_send_message(Queue, wake), _, true)
        )
    ;   true
    ),
    forall(
        retract(multi_purpose_worker_thread(Thread)),
        catch(thread_join(Thread, _), _, true)
    ),
    catch(message_queue_destroy(Queue), _, true),
    retractall(worker_wakeup_queue_handle(_)),
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

:- end_tests(commit_queue).
