:- module(meta_commit_queue, [
              with_meta_commit_lock/2,
              with_meta_commit_locks/2,
              expire_meta_commit_lock/1,
              database_descriptor_key/2,
              graph_label_to_lock_key/2,
              system_meta_lock_key/1
          ]).

/** <module> Meta commit queue
 *
 * Lightweight, per-database recursive lock used to serialize commits to the
 * shared `_meta` graph. The auto-optimizer also acquires this lock so that it
 * cannot change the `_meta` head while a branch/database state-changing
 * operation is in its commit window.
 *
 * The lock is recursive: a thread that already holds the lock may re-enter
 * without deadlocking. This is important because high-level operations such as
 * rebase hold the lock while calling run_transactions/3, which then attempts to
 * acquire the same lock for the database transaction object.
 */

:- use_module(core(util)).
:- use_module(core(triple/database_utils), [organization_database_name/3]).
:- use_module(core(triple/constants), [system_instance_name/1,
                                       system_schema_name/1,
                                       system_inference_name/1]).
:- use_module(library(lists)).
:- use_module(library(plunit)).

:- dynamic meta_commit_lock/2.
:- dynamic meta_commit_touch/2.
:- dynamic meta_commit_last_sweep/1.
:- thread_local held_meta_commit_lock/1.

:- initialization(mutex_create(meta_commit_lock_creation, [])).

:- meta_predicate with_meta_commit_lock(+, :).
:- meta_predicate with_meta_commit_locks(+, :).
:- meta_predicate with_meta_commit_locks_(+, :).

database_descriptor_key(database_descriptor{organization_name: Organization,
                                          database_name: Database},
                        Key) :-
    organization_database_name(Organization, Database, Key).

ensure_meta_commit_lock(Key) :-
    (   meta_commit_lock(Key, _)
    ->  touch_meta_commit_lock(Key)
    ;   with_mutex(meta_commit_lock_creation,
                   (   maybe_sweep_meta_commit_locks,
                       (   meta_commit_lock(Key, _)
                       ->  true
                       ;   mutex_create(Mutex, []),
                           assertz(meta_commit_lock(Key, Mutex))
                       ),
                       touch_meta_commit_lock(Key)
                   ))
    ).

%% Expiry: a loaded database keeps a per-key mutex + clause alive for the
%% process lifetime unless it is removed. Locks expire two ways:
%% delete-driven (expire_meta_commit_lock/1, called when the database is
%% deleted) and TTL-driven (the lazy sweep inside ensure_meta_commit_lock
%% evicts entries idle longer than meta_commit_lock_ttl — a database that
%% was merely loaded once does not pin its lock forever).

meta_commit_lock_ttl(Secs) :-
    (   getenv('TERMINUSDB_REGISTRY_TTL', V),
        atom_number(V, N)
    ->  Secs = N
    ;   Secs = 600
    ).

touch_meta_commit_lock(Key) :-
    get_time(Now),
    retractall(meta_commit_touch(Key, _)),
    assertz(meta_commit_touch(Key, Now)).

%% Rate-limited lazy sweep — runs at most once a minute, inside the
%% creation mutex, so it never races an in-flight ensure.
maybe_sweep_meta_commit_locks :-
    get_time(Now),
    (   meta_commit_last_sweep(Last),
        Now - Last < 60
    ->  true
    ;   retractall(meta_commit_last_sweep(_)),
        assertz(meta_commit_last_sweep(Now)),
        sweep_meta_commit_locks_at(Now)
    ).

sweep_meta_commit_locks :-
    get_time(Now),
    with_mutex(meta_commit_lock_creation, sweep_meta_commit_locks_at(Now)).

sweep_meta_commit_locks_at(Now) :-
    meta_commit_lock_ttl(TTL),
    forall(
        (   meta_commit_lock(Key, Mutex),
            meta_commit_lock_expired(Key, Now, TTL)
        ),
        evict_meta_commit_lock(Key, Mutex)
    ),
    % drop touch rows whose lock is already gone
    forall(
        (   meta_commit_touch(Key, _),
            \+ meta_commit_lock(Key, _)
        ),
        retractall(meta_commit_touch(Key, _))
    ).

meta_commit_lock_expired(Key, Now, TTL) :-
    (   meta_commit_touch(Key, Touched)
    ->  Now - Touched > TTL
    ;   true  % untracked entry — treat as stale
    ).

%% Eviction grabs the mutex first (mutex_trylock fails while a holder is
%% inside with_mutex, so held locks survive the round) and retracts while
%% still holding it — a thread that looks up the clause after the retract
%% cannot find it, and one that holds it blocks eviction until next sweep.
evict_meta_commit_lock(Key, Mutex) :-
    (   mutex_trylock(Mutex)
    ->  retractall(meta_commit_lock(Key, Mutex)),
        retractall(meta_commit_touch(Key, _)),
        mutex_unlock(Mutex),
        catch(mutex_destroy(Mutex), _, true)
    ;   true
    ).

%% expire_meta_commit_lock(+Key) is det.
%%
%%  Remove the lock for a deleted key immediately (delete-driven expiry),
%%  serialized against ensure via the creation mutex. Residual race: a
%%  thread that read the lock row just before the retract can still call
%%  mutex_lock on a destroyed mutex and get an existence_error — the
%%  request is on a deleted key and fails anyway.
expire_meta_commit_lock(Key) :-
    with_mutex(meta_commit_lock_creation,
        (   meta_commit_lock(Key, Mutex)
        ->  retractall(meta_commit_lock(Key, _)),
            retractall(meta_commit_touch(Key, _)),
            (   mutex_trylock(Mutex)
            ->  mutex_unlock(Mutex),
                catch(mutex_destroy(Mutex), _, true)
            ;   true )
        ;   retractall(meta_commit_touch(Key, _))
        )).

with_meta_commit_lock(Key, Goal) :-
    ensure_meta_commit_lock(Key),
    touch_meta_commit_lock(Key),
    (   held_meta_commit_lock(Key)
    ->  call(Goal)
    ;   (   meta_commit_lock(Key, Mutex)
        ->  true
        ;   %% a sweep can evict the entry between ensure and lookup —
            %% ensure once more, then the clause is back
            ensure_meta_commit_lock(Key),
            meta_commit_lock(Key, Mutex)
        ),
        assertz(held_meta_commit_lock(Key)),
        call_cleanup(
            with_mutex(Mutex, call(Goal)),
            retract(held_meta_commit_lock(Key))
        )
    ).

with_meta_commit_locks(Keys, Goal) :-
    with_meta_commit_locks_(Keys, Goal).

with_meta_commit_locks_([], Goal) :-
    call(Goal).
with_meta_commit_locks_([Key|Keys], Goal) :-
    ensure_meta_commit_lock(Key),
    touch_meta_commit_lock(Key),
    (   held_meta_commit_lock(Key)
    ->  with_meta_commit_locks_(Keys, Goal)
    ;   (   meta_commit_lock(Key, Mutex)
        ->  true
        ;   ensure_meta_commit_lock(Key),
            meta_commit_lock(Key, Mutex)
        ),
        assertz(held_meta_commit_lock(Key)),
        call_cleanup(
            with_mutex(Mutex, with_meta_commit_locks_(Keys, Goal)),
            retract(held_meta_commit_lock(Key))
        )
    ).

system_meta_lock_key(system_meta).

graph_label_to_lock_key(Graph_Label, Lock_Key) :-
    (   system_graph_label(Graph_Label)
    ->  system_meta_lock_key(Lock_Key)
    ;   atom(Graph_Label),
        atomic_list_concat(Parts, '|', Graph_Label),
        Parts = [Organization, DB|_]
    ->  organization_database_name(Organization, DB, Lock_Key)
    ;   Lock_Key = Graph_Label
    ).

system_graph_label(Graph_Label) :-
    (   system_instance_name(Graph_Label)
    ;   system_schema_name(Graph_Label)
    ;   system_inference_name(Graph_Label)
    ),
    !.

:- begin_tests(meta_commit_queue).

test(system_instance_label_maps_to_system_meta) :-
    system_instance_name(Graph_Label),
    graph_label_to_lock_key(Graph_Label, system_meta).

test(system_schema_label_maps_to_system_meta) :-
    system_schema_name(Graph_Label),
    graph_label_to_lock_key(Graph_Label, system_meta).

test(database_meta_label_maps_to_itself) :-
    organization_database_name(admin, test, Key),
    graph_label_to_lock_key(Key, Key).

test(repo_commits_label_maps_to_database_key) :-
    organization_database_name(admin, test, DBKey),
    atomic_list_concat([admin, test, local, '_commits'], '|', RepoLabel),
    graph_label_to_lock_key(RepoLabel, DBKey).

test(branch_instance_label_maps_to_database_key) :-
    organization_database_name(admin, test, DBKey),
    atomic_list_concat([admin, test, local, branch, main, instance], '|', BranchLabel),
    graph_label_to_lock_key(BranchLabel, DBKey).

test(lock_is_recursive_within_same_thread) :-
    Key = recursive_test_key,
    with_meta_commit_lock(
        Key,
        with_meta_commit_lock(Key, true)
    ).

%% Registry expiry: locks for databases that are idle longer than the
%% TTL get reclaimed by the lazy sweep, fresh and held locks survive.

backdate_meta_commit_touch(Key, Seconds) :-
    get_time(Now),
    Past is Now - Seconds,
    retractall(meta_commit_queue:meta_commit_touch(Key, _)),
    assertz(meta_commit_queue:meta_commit_touch(Key, Past)).

test(sweep_evicts_stale_lock, [cleanup(retractall(meta_commit_queue:meta_commit_lock(sweep_stale_key,_)))]) :-
    Key = sweep_stale_key,
    meta_commit_queue:ensure_meta_commit_lock(Key),
    meta_commit_queue:meta_commit_lock(Key, _Mutex),
    backdate_meta_commit_touch(Key, 10_000),
    meta_commit_queue:sweep_meta_commit_locks,
    \+ meta_commit_queue:meta_commit_lock(Key, _).

test(sweep_destroys_evicted_mutex, [cleanup(retractall(meta_commit_queue:meta_commit_lock(sweep_destroy_key,_)))]) :-
    Key = sweep_destroy_key,
    meta_commit_queue:ensure_meta_commit_lock(Key),
    meta_commit_queue:meta_commit_lock(Key, Mutex),
    backdate_meta_commit_touch(Key, 10_000),
    meta_commit_queue:sweep_meta_commit_locks,
    \+ meta_commit_queue:meta_commit_lock(Key, _),
    assertion(catch(mutex_property(Mutex, _), error(existence_error(mutex, _), _), true)).

test(expire_destroys_mutex, [cleanup(retractall(meta_commit_queue:meta_commit_lock(expire_destroy_key,_)))]) :-
    Key = expire_destroy_key,
    meta_commit_queue:ensure_meta_commit_lock(Key),
    meta_commit_queue:meta_commit_lock(Key, Mutex),
    meta_commit_queue:expire_meta_commit_lock(Key),
    \+ meta_commit_queue:meta_commit_lock(Key, _),
    assertion(catch(mutex_property(Mutex, _), error(existence_error(mutex, _), _), true)).

test(sweep_keeps_fresh_lock, [cleanup(retractall(meta_commit_queue:meta_commit_lock(sweep_fresh_key,_)))]) :-
    Key = sweep_fresh_key,
    meta_commit_queue:ensure_meta_commit_lock(Key),
    meta_commit_queue:sweep_meta_commit_locks,
    meta_commit_queue:meta_commit_lock(Key, _).

test(sweep_skips_currently_held_lock,
     [cleanup((retractall(meta_commit_queue:meta_commit_lock(sweep_held_key,_)),
               catch(thread_join(T, _), _, true)))]) :-
    Key = sweep_held_key,
    meta_commit_queue:ensure_meta_commit_lock(Key),
    message_queue_create(Gate),
    message_queue_create(Done),
    thread_create(
        (   meta_commit_queue:with_meta_commit_lock(Key,
                (   thread_send_message(Done, locked),
                    thread_get_message(Gate, release)))
        ),
        T,
        [detached(true)]),
    thread_get_message(Done, locked),
    backdate_meta_commit_touch(Key, 10_000),
    meta_commit_queue:sweep_meta_commit_locks,
    meta_commit_queue:meta_commit_lock(Key, _),  % held lock survives the sweep
    thread_send_message(Gate, release).

test(expire_removes_lock) :-
    Key = expire_test_key,
    meta_commit_queue:ensure_meta_commit_lock(Key),
    meta_commit_queue:meta_commit_lock(Key, _),
    meta_commit_queue:expire_meta_commit_lock(Key),
    \+ meta_commit_queue:meta_commit_lock(Key, _).

test(with_lock_refreshes_touch, [cleanup(retractall(meta_commit_queue:meta_commit_lock(touch_key,_)))]) :-
    Key = touch_key,
    meta_commit_queue:ensure_meta_commit_lock(Key),
    backdate_meta_commit_touch(Key, 10_000),
    meta_commit_queue:with_meta_commit_lock(Key, true),
    meta_commit_queue:meta_commit_touch(Key, T),
    get_time(Now),
    Now - T < 60.

:- end_tests(meta_commit_queue).
