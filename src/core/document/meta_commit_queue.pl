:- module(meta_commit_queue, [
              with_meta_commit_lock/2,
              with_meta_commit_locks/2,
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
        meta_commit_queue:(with_meta_commit_lock(Key, true))
    ).

:- end_tests(meta_commit_queue).
