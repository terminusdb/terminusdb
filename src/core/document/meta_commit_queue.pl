:- module(meta_commit_queue, [
              with_meta_commit_lock/2,
              with_meta_commit_locks/2,
              database_descriptor_key/2
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

:- dynamic meta_commit_lock/2.
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
    ->  true
    ;   with_mutex(meta_commit_lock_creation,
                   (   meta_commit_lock(Key, _)
                   ->  true
                   ;   mutex_create(Mutex, []),
                       assertz(meta_commit_lock(Key, Mutex))
                   ))
    ).

with_meta_commit_lock(Key, Goal) :-
    ensure_meta_commit_lock(Key),
    (   held_meta_commit_lock(Key)
    ->  call(Goal)
    ;   meta_commit_lock(Key, Mutex),
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
    (   held_meta_commit_lock(Key)
    ->  with_meta_commit_locks_(Keys, Goal)
    ;   meta_commit_lock(Key, Mutex),
        assertz(held_meta_commit_lock(Key)),
        call_cleanup(
            with_mutex(Mutex, with_meta_commit_locks_(Keys, Goal)),
            retract(held_meta_commit_lock(Key))
        )
    ).
