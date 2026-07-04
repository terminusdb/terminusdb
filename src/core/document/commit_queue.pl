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
              try_global_optimization/0,
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
              pending_branches/1,
              schedule_database_optimization/1,
              schedule_branch_optimization/1
          ]).

:- use_module(config(terminus_config), [worker_amount/1]).
:- use_module(core(util)).
:- use_module(core(util/test_utils), [setup_temp_store/1, teardown_temp_store/1]).
:- use_module(core(triple), [triple_store/1, with_triple_store/2]).
:- use_module(core(transaction/descriptor), [branch_key_from_descriptor/2]).
:- use_module(core(triple/database_utils), [organization_database_name/3]).
:- use_module(core(document/meta_commit_queue)).
:- use_module(core(api/api_optimize), [descriptor_optimize/1]).
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

% Optimization scheduling state.
% pending_database_optimization(DatabaseKey, Descriptor) and
% pending_branch_optimization(BranchKey, Descriptor) hold the optimization
% that should run when the relevant queue drains. The *_due/1 variants mark
% that a second request arrived while one was already pending, so it should
% run as soon as possible rather than being dropped.
:- dynamic pending_database_optimization/2.
:- dynamic pending_branch_optimization/2.
:- dynamic pending_database_optimization_due/1.
:- dynamic pending_branch_optimization_due/1.

% pending_global_optimization(DatabaseKey, Descriptor) and pending_global_optimization_due/1
% hold database-level optimization work for database keys that have no tracked
% branch activity (e.g. system_meta). These tasks are processed by idle commit
% workers through a global task queue.
:- dynamic pending_global_optimization/2.
:- dynamic pending_global_optimization_due/1.

% database_active_branch_count(DatabaseKey, Count) tracks how many branches of
% a given database currently have an active commit worker. Used to know when a
% database-wide _meta optimization can run safely.
:- dynamic database_active_branch_count/2.

% optimization_test_handler(Handler) can be asserted by tests to intercept
% scheduled optimizations without running real descriptor_optimize/1. Handler is
% a 2-argument predicate that will be called as call(Handler, Scope, Descriptor).
:- dynamic optimization_test_handler/1.

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
                atom_concat('commit_lock_', BranchKeyAtom, LockAlias),
                (   catch(message_queue_create(Queue, [alias(QueueAlias)]),
                          error(permission_error(create, message_queue, _), _),
                          message_queue_property(Queue, alias(QueueAlias)))
                ->  true
                ;   throw(error(unable_to_create_branch_queue(BranchKey), _))
                ),
                (   catch(mutex_create(Mutex, [alias(LockAlias)]),
                          error(permission_error(create, mutex, _), _),
                          mutex_property(Mutex, alias(LockAlias)))
                ->  true
                ;   catch(message_queue_destroy(Queue), _, true),
                    throw(error(unable_to_create_branch_lock(BranchKey), _))
                ),
                assertz(branch_commit_queue(BranchKey, Queue)),
                assertz(branch_commit_lock(BranchKey, Mutex))
            ))
    ).

destroy_branch_queue(BranchKey) :-
    with_mutex(branch_registry_mutex,
        (   (   branch_commit_queue(BranchKey, Queue)
            ->  (   catch(message_queue_destroy(Queue), _, fail)
                ->  retract(branch_commit_queue(BranchKey, _))
                ;   true
                )
            ;   true
            ),
            (   branch_commit_lock(BranchKey, Mutex)
            ->  (   catch(mutex_destroy(Mutex), _, fail)
                ->  retract(branch_commit_lock(BranchKey, _))
                ;   true
                )
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
            assertz(active_branch(BranchKey, WorkerId)),
            increment_database_active_branch_count(BranchKey)
        ;   fail
        )).

pick_first_non_active([Branch|Rest], Branch, Rest) :-
    \+ active_branch(Branch, _),
    !.
pick_first_non_active([Branch|Rest], Picked, [Branch|Remaining]) :-
    pick_first_non_active(Rest, Picked, Remaining).

requeue_branch(BranchKey) :-
    thread_self(WorkerId),
    requeue_branch(BranchKey, WorkerId).

requeue_branch(BranchKey, WorkerId) :-
    with_mutex(commit_scheduler_mutex,
        (   (   retract(active_branch(BranchKey, WorkerId))
            ->  decrement_database_active_branch_count(BranchKey, NewCount),
                ActiveRetracted = true
            ;   ActiveRetracted = false,
                NewCount = unknown
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
            ),
            (   ActiveRetracted == true
            ->  branch_key_database_key(BranchKey, DatabaseKey),
                collect_pending_optimizations(BranchKey, DatabaseKey, Requeued, NewCount, BranchOpt, DatabaseOpt)
            ;   BranchOpt = none,
                DatabaseOpt = none
            )
        )),
    run_collected_optimization(branch, BranchOpt),
    run_collected_optimization(database, DatabaseOpt).

% Database key helpers.
%
% branch_key_database_key(+BranchKey, -DatabaseKey) extracts the database lock
% key from a resolved branch key such as "admin/db/local/branch/main". The
% result must match the key produced by database_key_from_descriptor/2 so that
% active-branch counts and scheduled database optimizations use the same key.
branch_key_database_key(BranchKey, DatabaseKey) :-
    (   atom_string(AtomKey, BranchKey),
        sub_atom(AtomKey, Before, _, _, '/local/branch/')
    ->  sub_atom(AtomKey, 0, Before, _, PrefixAtom),
        (   atomic_list_concat([Org, DB], '/', PrefixAtom)
        ->  organization_database_name(Org, DB, DatabaseKey)
        ;   DatabaseKey = PrefixAtom
        )
    ;   atom_string(DatabaseKey, BranchKey)
    ).

% database_key_from_descriptor(+Descriptor, -DatabaseKey) maps any descriptor
% to the lock key of the database it belongs to.
database_key_from_descriptor(Descriptor, DatabaseKey) :-
    (   system_descriptor{} :< Descriptor
    ->  DatabaseKey = system_meta
    ;   database_descriptor{organization_name: Org, database_name: DB} :< Descriptor
    ->  organization_database_name(Org, DB, DatabaseKey)
    ;   repository_descriptor{database_descriptor: DB_Desc} :< Descriptor
    ->  database_key_from_descriptor(DB_Desc, DatabaseKey)
    ;   branch_descriptor{repository_descriptor: Repo_Desc} :< Descriptor
    ->  database_key_from_descriptor(Repo_Desc, DatabaseKey)
    ;   fail
    ).

increment_database_active_branch_count(BranchKey) :-
    branch_key_database_key(BranchKey, DatabaseKey),
    (   retract(database_active_branch_count(DatabaseKey, Count))
    ->  NewCount is Count + 1,
        assertz(database_active_branch_count(DatabaseKey, NewCount))
    ;   assertz(database_active_branch_count(DatabaseKey, 1))
    ).

decrement_database_active_branch_count(BranchKey, NewCount) :-
    branch_key_database_key(BranchKey, DatabaseKey),
    (   retract(database_active_branch_count(DatabaseKey, Count))
    ->  NewCount is Count - 1,
        (   NewCount < 0
        ->  throw(error(active_branch_count_negative(DatabaseKey), _))
        ;   assertz(database_active_branch_count(DatabaseKey, NewCount))
        )
    ;   NewCount = 0,
        assertz(database_active_branch_count(DatabaseKey, 0))
    ).

% Optimization scheduling.
%
% schedule_branch_optimization(+BranchDescriptor) and
% schedule_database_optimization(+DatabaseDescriptor) are called from the
% auto-optimize plugin. They record the requested optimization and wake workers
% so it runs when the relevant queue drains.
schedule_branch_optimization(Descriptor) :-
    branch_descriptor{} :< Descriptor,
    !,
    branch_key_from_descriptor(Descriptor, BranchKey),
    with_mutex(commit_scheduler_mutex,
        (
            (   (   pending_branch_optimization(BranchKey, _)
                ;   pending_branch_optimization_due(BranchKey)
                )
            ->  assertz(pending_branch_optimization_due(BranchKey))
            ;   true
            ),
            (   retract(pending_branch_optimization(BranchKey, _))
            ->  true
            ;   true
            ),
            assertz(pending_branch_optimization(BranchKey, Descriptor))
        )
    ),
    wake_workers.

schedule_database_optimization(Descriptor) :-
    (   system_descriptor{} :< Descriptor
    ;   database_descriptor{} :< Descriptor
    ;   repository_descriptor{} :< Descriptor
    ),
    !,
    database_key_from_descriptor(Descriptor, DatabaseKey),
    (   database_active_branch_count(DatabaseKey, _)
    ->  with_mutex(commit_scheduler_mutex,
            (
                (   (   pending_database_optimization(DatabaseKey, _)
                    ;   pending_database_optimization_due(DatabaseKey)
                    )
                ->  assertz(pending_database_optimization_due(DatabaseKey))
                ;   true
                ),
                (   retract(pending_database_optimization(DatabaseKey, _))
                ->  true
                ;   true
                ),
                assertz(pending_database_optimization(DatabaseKey, Descriptor))
            )
        ),
        wake_workers
    ;   % No tracked branch activity for this database (e.g. system). There is
        % no branch queue to drain, so schedule the work on the global task
        % queue and let idle commit workers process it.
        with_mutex(commit_scheduler_mutex,
            (
                (   (   pending_global_optimization(DatabaseKey, _)
                    ;   pending_global_optimization_due(DatabaseKey)
                    )
                ->  assertz(pending_global_optimization_due(DatabaseKey))
                ;   true
                ),
                (   retract(pending_global_optimization(DatabaseKey, _))
                ->  true
                ;   true
                ),
                assertz(pending_global_optimization(DatabaseKey, Descriptor))
            )
        ),
        wake_workers
    ).

collect_pending_optimizations(BranchKey, DatabaseKey, Requeued, NewCount, BranchOpt, DatabaseOpt) :-
    (   Requeued == false,
        (   pending_branch_optimization_due(BranchKey)
        ;   pending_branch_optimization(BranchKey, _)
        )
    ->  retractall(pending_branch_optimization_due(BranchKey)),
        (   retract(pending_branch_optimization(BranchKey, Descriptor))
        ->  BranchOpt = some(Descriptor)
        ;   BranchOpt = none
        )
    ;   BranchOpt = none
    ),
    (   NewCount == 0,
        (   pending_database_optimization_due(DatabaseKey)
        ;   pending_database_optimization(DatabaseKey, _)
        )
    ->  retractall(pending_database_optimization_due(DatabaseKey)),
        (   retract(pending_database_optimization(DatabaseKey, Descriptor))
        ->  DatabaseOpt = some(Descriptor)
        ;   DatabaseOpt = none
        )
    ;   DatabaseOpt = none
    ).

run_collected_optimization(_, none) :- !.
run_collected_optimization(branch, some(Descriptor)) :-
    optimization_test_handler(Handler),
    !,
    once(call(Handler, branch, Descriptor)).
run_collected_optimization(branch, some(Descriptor)) :-
    !,
    run_branch_optimization(Descriptor).
run_collected_optimization(database, some(Descriptor)) :-
    optimization_test_handler(Handler),
    !,
    once(call(Handler, database, Descriptor)).
run_collected_optimization(database, some(Descriptor)) :-
    !,
    run_database_optimization(Descriptor).

run_branch_optimization(Descriptor) :-
    branch_key_from_descriptor(Descriptor, BranchKey),
    database_key_from_descriptor(Descriptor, DatabaseKey),
    catch(
        with_meta_commit_lock(
            DatabaseKey,
            descriptor_optimize(Descriptor)
        ),
        Error,
        log_optimization_error(branch, BranchKey, Error)
    ).

run_database_optimization(Descriptor) :-
    database_key_from_descriptor(Descriptor, DatabaseKey),
    catch(
        with_meta_commit_lock(
            DatabaseKey,
            descriptor_optimize(Descriptor)
        ),
        Error,
        log_optimization_error(database, DatabaseKey, Error)
    ).

% try_global_optimization/0 is semidet.
%
% Called by idle commit workers. It atomically picks one pending global
% optimization (and clears its due flag), runs it under the database meta lock,
% and removes the pending fact. Succeeds when a task was processed; fails when
% the global queue is empty.
try_global_optimization :-
    with_mutex(commit_scheduler_mutex,
        (   (   pending_global_optimization_due(DatabaseKey)
            ;   pending_global_optimization(DatabaseKey, _)
            )
        ->  retractall(pending_global_optimization_due(DatabaseKey)),
            (   retract(pending_global_optimization(DatabaseKey, Descriptor))
            ->  Task = some(DatabaseKey, Descriptor)
            ;   Task = none
            )
        ;   Task = none
        )
    ),
    Task = some(DatabaseKey, Descriptor),
    !,
    run_collected_optimization(database, some(Descriptor)).

log_optimization_error(Scope, Key, Error) :-
    json_log_error_formatted("Scheduled ~w optimization for ~w failed: ~w", [Scope, Key, Error]).

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
        findall(BranchKey, active_branch(BranchKey, WorkerId), Branches)),
    forall(member(BranchKey, Branches),
           requeue_branch(BranchKey, WorkerId)).

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
        run_queued_job(Package),
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
        run_queued_job(Package),
        Error,
        deliver_commit_error(Package, Error)
    ),
    Max1 is Max - 1,
    process_commit_batch(BranchKey, Max1, _).
process_commit_batch(_, _, false).

% run_queued_job(+Package) is det.
%
% Dispatch a queued package to the correct handler based on its package_type.
% Existing commit packages do not have package_type, so they default to the
% document commit handler. New contract types (rebase, optimize) are routed to
% their own handlers so they can run under the branch lock and acquire the
% meta_commit_lock only after the branch lock is already held.
run_queued_job(Package) :-
    (   get_dict(package_type, Package, Type)
    ->  true
    ;   Type = commit
    ),
    run_queued_job_of_type(Type, Package).

run_queued_job_of_type(commit, Package) :-
    !,
    api_document:run_commit_package(Package).
run_queued_job_of_type(rebase, Package) :-
    !,
    db_rebase:run_rebase_contract(Package).
run_queued_job_of_type(optimize_when_idle, Package) :-
    !,
    get_dict(branch_key, Package, BranchKey),
    branch_commit_queue(BranchKey, Queue),
    (   % Synchronous optimize requests from HTTP handlers carry a reply queue.
        % Run them immediately: if two such optimizes are queued on the same
        % branch, the deferral logic would otherwise livelock by repeatedly
        % re-enqueueing each optimize behind the other.
        get_dict(reply_queue, Package, _)
    ->  api_optimize:run_queued_optimize(Package)
    ;   queue_empty(Queue)
    ->  api_optimize:run_queued_optimize(Package)
    ;   % More commits are pending on this branch. Defer the scheduler-initiated
        % optimization so inserts are not blocked by the (potentially slow)
        % optimize work. The branch will be requeued by the caller after the
        % current package is processed, and the optimizer will be picked up
        % again when the queue drains.
        thread_send_message(Queue, Package),
        register_pending_branch(BranchKey),
        wake_workers
    ).
run_queued_job_of_type(Type, Package) :-
    throw(error(unknown_package_type(Type, Package), _)).

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

test(ensure_branch_queue_reuses_existing_queue_when_fact_missing) :-
    destroy_branch_queue('test_branch'),
    ensure_branch_queue('test_branch', Queue1),
    with_mutex(branch_registry_mutex,
               (   retract(commit_queue:branch_commit_queue('test_branch', _)),
                   retract(commit_queue:branch_commit_lock('test_branch', _))
               )),
    ensure_branch_queue('test_branch', Queue2),
    assertion(Queue1 == Queue2),
    branch_commit_queue('test_branch', Queue2),
    branch_commit_lock('test_branch', _),
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
    cleanup_active_branch_counts,
    assertz(commit_queue:active_branch('b1', worker_1)),
    assertz(commit_queue:active_branch('b2', worker_1)),
    assertz(commit_queue:active_branch('b3', worker_2)),
    assertz(commit_queue:database_active_branch_count('b1', 1)),
    assertz(commit_queue:database_active_branch_count('b2', 1)),
    assertz(commit_queue:database_active_branch_count('b3', 1)),
    cleanup_active_branches_for_worker(worker_1),
    assertion(\+ commit_queue:active_branch('b1', worker_1)),
    assertion(\+ commit_queue:active_branch('b2', worker_1)),
    assertion(commit_queue:active_branch('b3', worker_2)),
    assertion(commit_queue:database_active_branch_count('b1', 0)),
    assertion(commit_queue:database_active_branch_count('b2', 0)),
    assertion(commit_queue:database_active_branch_count('b3', 1)),
    with_mutex(commit_scheduler_mutex,
        (   retractall(commit_queue:pending_branches(_)),
            retractall(commit_queue:active_branch(_, _))
        )),
    cleanup_active_branch_counts.

test(cleanup_active_branches_for_worker_requeues_pending_commits) :-
    BranchKey = 'admin/db/local/branch/main',
    with_mutex(commit_scheduler_mutex,
        (   retractall(commit_queue:pending_branches(_)),
            retractall(commit_queue:active_branch(_, _)),
            retractall(commit_queue:pending_branch_age(_, _))
        )),
    cleanup_active_branch_counts,
    ensure_branch_queue(BranchKey, Queue),
    thread_send_message(Queue, dummy_package),
    assertz(commit_queue:active_branch(BranchKey, worker_1)),
    organization_database_name(admin, db, DBKey),
    assertz(commit_queue:database_active_branch_count(DBKey, 1)),
    cleanup_active_branches_for_worker(worker_1),
    assertion(\+ commit_queue:active_branch(BranchKey, worker_1)),
    assertion(commit_queue:database_active_branch_count(DBKey, 0)),
    assertion(commit_queue:pending_branches([BranchKey])),
    assertion(commit_queue:pending_branch_age(BranchKey, _)),
    with_mutex(commit_scheduler_mutex,
        (   retractall(commit_queue:pending_branches(_)),
            retractall(commit_queue:active_branch(_, _)),
            retractall(commit_queue:pending_branch_age(_, _))
        )),
    cleanup_active_branch_counts,
    destroy_branch_queue(BranchKey).

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
test(start_and_stop_workers, [setup(setup_temp_store(State)), cleanup(teardown_temp_store(State))]) :-
    cleanup_workers_and_branches,
    start_workers(2),
    aggregate_all(count, multi_purpose_worker_thread(_), Count),
    assertion(Count == 2),
    stop_workers,
    aggregate_all(count, multi_purpose_worker_thread(_), Count2),
    assertion(Count2 == 0).

% Test that a real worker thread picks up an enqueued commit and delivers
% the result to the reply queue.
test(worker_processes_enqueued_commit, [setup((setup_temp_store(State),
                                              asserta(api_document:commit_package_test_handler(commit_queue:success_handler)))),
                                          cleanup((retractall(api_document:commit_package_test_handler(_)),
                                                   teardown_temp_store(State)))]) :-
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
test(wake_workers_wakes_idle_worker, [setup(setup_temp_store(State)), cleanup(teardown_temp_store(State))]) :-
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

% Scheduling-state and active-branch tracking tests.

test(branch_key_database_key_resolved_and_plain) :-
    branch_key_database_key("admin/db/local/branch/main", Key),
    organization_database_name(admin, db, ExpectedKey),
    assertion(Key == ExpectedKey),
    branch_key_database_key("plain_test", PlainKey),
    assertion(PlainKey == 'plain_test').

test(database_key_from_descriptor_maps_correctly) :-
    database_key_from_descriptor(system_descriptor{}, system_meta),
    database_key_from_descriptor(database_descriptor{organization_name: "admin", database_name: "db"}, Key),
    organization_database_name("admin", "db", Expected),
    assertion(Key == Expected).

test(active_branch_count_tracks_increments_and_decrements) :-
    cleanup_active_branch_counts,
    organization_database_name(admin, db, DBKey),
    increment_database_active_branch_count("admin/db/local/branch/main"),
    assertion(commit_queue:database_active_branch_count(DBKey, 1)),
    increment_database_active_branch_count("admin/db/local/branch/other"),
    assertion(commit_queue:database_active_branch_count(DBKey, 2)),
    decrement_database_active_branch_count("admin/db/local/branch/main", Count1),
    assertion(Count1 == 1),
    assertion(commit_queue:database_active_branch_count(DBKey, 1)),
    decrement_database_active_branch_count("admin/db/local/branch/other", Count2),
    assertion(Count2 == 0),
    assertion(commit_queue:database_active_branch_count(DBKey, 0)).

test(active_branch_count_negative_throws, [throws(error(active_branch_count_negative(DBKey), _))]) :-
    cleanup_active_branch_counts,
    organization_database_name(admin, db, DBKey),
    assertz(commit_queue:database_active_branch_count(DBKey, 0)),
    decrement_database_active_branch_count("admin/db/local/branch/main", _).

test(schedule_database_optimization_records_pending_and_due) :-
    cleanup_optimization_state,
    cleanup_active_branch_counts,
    DB_Descriptor = database_descriptor{organization_name: "admin", database_name: "db"},
    database_key_from_descriptor(DB_Descriptor, DB_Key),
    assertz(commit_queue:database_active_branch_count(DB_Key, 1)),
    schedule_database_optimization(DB_Descriptor),
    assertion(commit_queue:pending_database_optimization(DB_Key, DB_Descriptor)),
    schedule_database_optimization(DB_Descriptor),
    assertion(commit_queue:pending_database_optimization_due(DB_Key)),
    assertion(commit_queue:pending_database_optimization(DB_Key, DB_Descriptor)).

test(schedule_database_optimization_due_resets_pending) :-
    cleanup_optimization_state,
    cleanup_active_branch_counts,
    DB_Descriptor = database_descriptor{organization_name: "admin", database_name: "db"},
    database_key_from_descriptor(DB_Descriptor, DB_Key),
    assertz(commit_queue:database_active_branch_count(DB_Key, 1)),
    assertz(commit_queue:pending_database_optimization(DB_Key, old_descriptor)),
    schedule_database_optimization(DB_Descriptor),
    assertion(commit_queue:pending_database_optimization(DB_Key, DB_Descriptor)),
    assertion(commit_queue:pending_database_optimization_due(DB_Key)).

test(database_optimization_runs_when_all_branches_inactive) :-
    cleanup_optimization_state,
    cleanup_active_branch_counts,
    cleanup_workers_and_branches,
    DB_Descriptor = database_descriptor{organization_name: "admin", database_name: "db"},
    database_key_from_descriptor(DB_Descriptor, DB_Key),
    BranchKey = "admin/db/local/branch/main",
    assertz(commit_queue:optimization_test_handler(test_optimize_handler)),
    assertz(commit_queue:pending_database_optimization(DB_Key, DB_Descriptor)),
    thread_self(Self),
    assertz(commit_queue:active_branch(BranchKey, Self)),
    assertz(commit_queue:database_active_branch_count(DB_Key, 1)),
    requeue_branch(BranchKey),
    assertion(commit_queue:called_optimization(database, DB_Descriptor)),
    assertion(\+ commit_queue:pending_database_optimization(DB_Key, _)),
    assertion(\+ commit_queue:pending_database_optimization_due(DB_Key)),
    cleanup_optimization_state,
    cleanup_active_branch_counts,
    cleanup_workers_and_branches.

test(system_database_optimization_schedules_on_global_queue) :-
    cleanup_optimization_state,
    cleanup_active_branch_counts,
    assertz(commit_queue:optimization_test_handler(test_optimize_handler)),
    schedule_database_optimization(system_descriptor{}),
    assertion(commit_queue:pending_global_optimization(system_meta, system_descriptor{})),
    assertion(\+ commit_queue:pending_database_optimization(system_meta, _)),
    try_global_optimization,
    assertion(commit_queue:called_optimization(database, system_descriptor{})),
    assertion(\+ commit_queue:pending_global_optimization(system_meta, _)),
    assertion(\+ commit_queue:pending_global_optimization_due(system_meta)),
    cleanup_optimization_state,
    cleanup_active_branch_counts.

test(global_queue_optimization_due_flag_set_on_second_schedule) :-
    cleanup_optimization_state,
    cleanup_active_branch_counts,
    assertz(commit_queue:optimization_test_handler(test_optimize_handler)),
    schedule_database_optimization(system_descriptor{}),
    schedule_database_optimization(system_descriptor{}),
    assertion(commit_queue:pending_global_optimization_due(system_meta)),
    try_global_optimization,
    assertion(commit_queue:called_optimization(database, system_descriptor{})),
    assertion(\+ commit_queue:pending_global_optimization(system_meta, _)),
    assertion(\+ commit_queue:pending_global_optimization_due(system_meta)),
    cleanup_optimization_state,
    cleanup_active_branch_counts.

test(branch_optimization_runs_when_queue_empties) :-
    cleanup_optimization_state,
    cleanup_workers_and_branches,
    destroy_branch_queue('opt_branch'),
    ensure_branch_queue('opt_branch', _Queue),
    assertz(commit_queue:optimization_test_handler(test_optimize_handler)),
    assertz(commit_queue:pending_branch_optimization('opt_branch', branch_descriptor{})),
    thread_self(Self),
    assertz(commit_queue:active_branch('opt_branch', Self)),
    requeue_branch('opt_branch'),
    assertion(commit_queue:called_optimization(branch, branch_descriptor{})),
    assertion(\+ commit_queue:pending_branch_optimization('opt_branch', _)),
    assertion(\+ commit_queue:pending_branch_optimization_due('opt_branch')),
    cleanup_optimization_state,
    cleanup_workers_and_branches,
    destroy_branch_queue('opt_branch').

test(second_branch_optimization_runs_as_soon_as_branch_drains) :-
    cleanup_optimization_state,
    cleanup_workers_and_branches,
    destroy_branch_queue('opt_branch2'),
    ensure_branch_queue('opt_branch2', Queue),
    thread_send_message(Queue, dummy),
    assertz(commit_queue:optimization_test_handler(test_optimize_handler)),
    assertz(commit_queue:pending_branch_optimization('opt_branch2', branch_descriptor{})),
    assertz(commit_queue:pending_branch_optimization_due('opt_branch2')),
    thread_self(Self),
    assertz(commit_queue:active_branch('opt_branch2', Self)),
    requeue_branch('opt_branch2'),
    % The queue still has a message, so the branch is requeued and the
    % optimization must not run yet.
    assertion(\+ commit_queue:called_optimization(branch, _)),
    assertion(commit_queue:pending_branch_optimization('opt_branch2', _)),
    assertion(commit_queue:pending_branch_optimization_due('opt_branch2')),
    % Drain the message and become active again, as a worker would after
    % pick_next_branch picks up the branch from pending_branches.
    thread_get_message(Queue, dummy),
    with_mutex(commit_scheduler_mutex,
        retractall(commit_queue:pending_branches(_))),
    assertz(commit_queue:active_branch('opt_branch2', Self)),
    increment_database_active_branch_count('opt_branch2'),
    requeue_branch('opt_branch2'),
    assertion(commit_queue:called_optimization(branch, branch_descriptor{})),
    assertion(\+ commit_queue:pending_branch_optimization('opt_branch2', _)),
    assertion(\+ commit_queue:pending_branch_optimization_due('opt_branch2')),
    cleanup_optimization_state,
    cleanup_workers_and_branches,
    destroy_branch_queue('opt_branch2').

:- end_tests(commit_queue).

% Test helpers. These live in the commit_queue module so they can be
% passed as handlers to api_document:execute_commit_package/2 and invoked
% from worker threads created by tests.

% Recorded by test_optimize_handler/2 during scheduled-optimization tests.
:- dynamic called_optimization/2.

cleanup_workers_and_branches :-
    stop_workers,
    with_mutex(commit_scheduler_mutex,
        (   retractall(commit_queue:pending_branches(_)),
            retractall(commit_queue:active_branch(_, _))
        )).

cleanup_optimization_state :-
    with_mutex(commit_scheduler_mutex,
        (   retractall(commit_queue:pending_database_optimization(_, _)),
            retractall(commit_queue:pending_branch_optimization(_, _)),
            retractall(commit_queue:pending_global_optimization(_, _)),
            retractall(commit_queue:pending_database_optimization_due(_)),
            retractall(commit_queue:pending_branch_optimization_due(_)),
            retractall(commit_queue:pending_global_optimization_due(_)),
            retractall(commit_queue:optimization_test_handler(_)),
            retractall(commit_queue:called_optimization(_, _))
        )).

cleanup_active_branch_counts :-
    with_mutex(commit_scheduler_mutex,
        retractall(commit_queue:database_active_branch_count(_, _))).

test_optimize_handler(Scope, Descriptor) :-
    assertz(called_optimization(Scope, Descriptor)).

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
