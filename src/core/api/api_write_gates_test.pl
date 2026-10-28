:- module(api_write_gates_test, []).

/**
 * Write-gate audit tests.
 *
 * Verifies that the API surface cannot open a write transaction for a
 * caller holding only read access. Two layers:
 *
 * - mutating_entry_points_deny_read_only_auth/1 calls known mutating
 *   entry points as anonymous on a public database and expects
 *   access_not_authorised.
 * - exported_api_predicates_opening_write_transactions_have_write_gate/1
 *   statically scans entry-point clauses (and the predicates they call,
 *   depth-limited) for write transactions lacking a write gate, so the
 *   read-gate-plus-write-transaction shape cannot silently return.
 */

:- begin_tests(write_gates).

:- use_module(core(util)).
:- use_module(core(util/test_utils)).
:- use_module(core(triple)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(api/api_document)).
:- use_module(core(api/api_patch)).
:- use_module(core(api/api_squash)).
:- use_module(core(api/api_merge)).
:- use_module(core(api/api_reset)).
:- use_module(core(api/api_rollup)).
:- use_module(core(api/api_remote)).
:- use_module(core(api/api_prefixes)).
:- use_module(core(api/api_migration)).
:- use_module(core(api/api_access_control)).
:- use_module(library(yall)).
:- use_module(library(apply), [foldl/4]).
:- use_module(library(lists)).
:- use_module(library(assoc)).
:- use_module(library(pairs)).

expect_denied(Name-Goal) :-
    (   catch(Goal, Ball, true)
    ->  (   nonvar(Ball), Ball = error(access_not_authorised(_,_,_),_)
        ->  true
        ;   format(user_error, '~n~w: expected access_not_authorised, got ~p~n', [Name, Ball]),
            fail)
    ;   format(user_error, '~n~w: failed without expected denial~n', [Name]),
        fail).

% Mutating API entry points must reject a caller holding only read
% access, here anonymous on a public database. Denial fires before
% arguments are consumed, so placeholders stand in for valid input.
test(mutating_entry_points_deny_read_only_auth,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_public_db_without_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Admin_Auth),
    Options = [author("gavin"), message("test."), raw_json(true)],
    open_string('[{"@id":"item/a","n":1}]', Stream1),
    api_insert_documents(SystemDB, Admin_Auth, Path, Stream1, no_data_version,
                         data_version(branch,Commit1), _Ids1, Options),
    atomic_list_concat([Path, '/local/commit/', Commit1], Reset_Ref),
    open_string('[]', Empty_Stream),
    Commit_Info = commit_info{ author : "mallory", message: "x" },
    Doc_Opts = [author("mallory"), message("x"), graph_type(instance)],
    Auth = 'User/anonymous',
    forall(
        member(Name-Goal,
               [ apply - api_apply_squash_commit(SystemDB, Auth, Path, Commit_Info, "c1", "c2", []),
                 patch - api_patch_resource(SystemDB, Auth, Path, [], Commit_Info, _I0, []),
                 insert_documents - api_insert_documents(SystemDB, Auth, Path, Empty_Stream, no_data_version, _DV1, _I1, Doc_Opts),
                 delete_documents - api_delete_documents(SystemDB, Auth, Path, Empty_Stream, no_data_version, _DV2, _I2, Doc_Opts),
                 delete_document - api_delete_document(SystemDB, Auth, Path, "item/a", no_data_version, _DV3, Doc_Opts),
                 delete_documents_by_type - api_delete_documents_by_type(SystemDB, Auth, Path, "x", no_data_version, _DV4, Doc_Opts),
                 replace_documents - api_replace_documents(SystemDB, Auth, Path, Empty_Stream, no_data_version, _DV5, _I3, Doc_Opts),
                 nuke_documents - api_nuke_documents(SystemDB, Auth, Path, no_data_version, _DV6, Doc_Opts),
                 reset - api_reset(SystemDB, Auth, Path, Reset_Ref),
                 rollup - api_rollup(SystemDB, Auth, Path, [], _Status),
                 squash - api_squash(SystemDB, Auth, Path, Commit_Info, "c1", _OC),
                 concat - api_concat(SystemDB, Auth, [], Path, _C, []),
                 migrate - api_migrate_resource(SystemDB, Auth, Path, Commit_Info, "[]", _R, []),
                 add_remote - add_remote(SystemDB, Auth, Path, "r", "http://x"),
                 remove_remote - remove_remote(SystemDB, Auth, Path, "r"),
                 update_remote - update_remote(SystemDB, Auth, Path, "r", "http://x"),
                 update_prefixes - update_prefixes(Path, SystemDB, Auth, Commit_Info, _{}),
                 add_prefix - add_prefix(Path, SystemDB, Auth, Commit_Info, p, "http://p", _Res, _RP),
                 delete_prefix - delete_prefix(Path, SystemDB, Auth, Commit_Info, p, _RP2),
                 add_user - api_add_user(SystemDB, Auth, _{name:"x"}, _UId)
               ]),
        expect_denied(Name-Goal)).

% Gate-shape scan. An entry point is any exported predicate in
% src/core/api/ modules or any locally-defined predicate in
% src/server/ modules (route handlers are not module exports). An entry
% is flagged when a write transaction is reachable through its own
% clause or through callees defined in the scanned directories, and no
% write gate appears anywhere on those reached predicates. Detection is
% order-insensitive: a callee writing before a caller's gate still
% counts as covered, so the scan can miss ordering bugs, never gate
% absence.

scanned_file(File) :-
    sub_atom(File, _, _, _, '/src/core/api/')
;   sub_atom(File, _, _, _, '/src/server/').

scanned_module(Module) :-
    module_property(Module, file(File)),
    scanned_file(File),
    % plunit unit bodies are test code, not entry points.
    \+ sub_atom(Module, 0, _, _, 'plunit_').

% Index: F/A -> modules whose file in the scanned dirs defines it.
% Built once per scan; call_target resolves through it instead of
% re-enumerating module predicates for every call-shaped subterm.
scanned_pred(M, F, A) :-
    scanned_module(M),
    M:current_predicate(F/A),
    functor(H, F, A),
    predicate_property(M:H, file(File)),
    scanned_file(File).

build_index(Index) :-
    findall(F/A-M, scanned_pred(M, F, A), Pairs),
    keysort(Pairs, Sorted),
    group_pairs_by_key(Sorted, Grouped),
    list_to_assoc(Grouped, Index).

api_exported_pred_key(M-F/A) :-
    scanned_module(M),
    module_property(M, file(File)),
    sub_atom(File, _, _, _, '/src/core/api/'),
    module_property(M, exports(Exports)),
    member(F/A, Exports).

server_entry_key(M-F/A) :-
    scanned_module(M),
    module_property(M, file(File)),
    sub_atom(File, _, _, _, '/src/server/'),
    M:current_predicate(F/A),
    functor(H, F, A),
    predicate_property(M:H, file(File)).

entry_pred_key(Key) :-
    api_exported_pred_key(Key)
;   server_entry_key(Key).

term_subterm(Term, Term).
term_subterm(Sub, Term) :-
    compound(Term),
    arg(_, Term, Arg),
    term_subterm(Sub, Arg).

unqualified(Goal, Inner) :-
    (   nonvar(Goal), Goal = (_:Inner)
    ->  true
    ;   Inner = Goal ).

gate_kind(Goal, Class) :-
    unqualified(Goal, Inner),
    compound(Inner),
    functor(Inner, Name, Arity),
    gate_kind_(Name, Arity, Inner, Class).

gate_kind_(resolve_descriptor_auth, 6, Goal, Class) :-
    arg(1, Goal, Class).
gate_kind_(assert_write_access, _, _, write).
gate_kind_(assert_read_access, _, _, read).
gate_kind_(check_descriptor_auth, 4, Goal, Class) :-
    arg(3, Goal, Action),
    action_class(Action, Class).
gate_kind_(assert_auth_action_scope, 4, Goal, Class) :-
    arg(3, Goal, Action),
    action_class(Action, Class).
gate_kind_(is_super_user, _, _, write).
gate_kind_(require_super_user, _, _, write).

action_class(Action, Class) :-
    (   compound(Action), Action = _:Name
    ->  true
    ;   Name = Action ),
    (   atom(Name), sub_atom(Name, _, _, _, read)
    ->  Class = read
    ;   Class = write ).

opens_write_transaction(Goal) :-
    unqualified(Goal, Inner),
    compound(Inner),
    functor(Inner, with_transaction, _).

% A call-shaped subterm that resolves to a predicate defined in the
% scanned directories.
call_target(Index, Call, M, F, A) :-
    compound(Call),
    unqualified(Call, Plain),
    nonvar(Plain),
    % functor/3 cannot decompose 0-arity compounds like memory().
    (   compound(Plain)
    ->  compound_name_arity(Plain, F, A)
    ;   F = Plain, A = 0 ),
    get_assoc(F/A, Index, Ms),
    member(M, Ms).

% Nodes keyed M-F/A: head terms carry fresh unbound args, so memberchk
% on M-Head would never match and the DFS would not terminate.
pred_closure(Index, M-F/A, Depth, Seen0, Seen) :-
    (   memberchk(M-F/A, Seen0)
    ->  Seen = Seen0
    ;   Depth > 0
    ->  Seen1 = [M-F/A|Seen0],
        D1 is Depth - 1,
        functor(H, F, A),
        findall(M2-F2/A2,
                (   clause(M:H, B),
                    term_subterm(Call, B),
                    call_target(Index, Call, M2, F2, A2)),
                Targets),
        foldl(pred_closure_step(Index, D1), Targets, Seen1, Seen)
    ;   Seen = [M-F/A|Seen0]).

pred_closure_step(Index, Depth, M-F/A, Seen0, Seen) :-
    pred_closure(Index, M-F/A, Depth, Seen0, Seen).

reachable_opens_write(Reachable) :-
    member(M-F/A, Reachable),
    functor(H, F, A),
    clause(M:H, B),
    term_subterm(Sub, B),
    opens_write_transaction(Sub),
    !.

reachable_has_write_gate(Reachable) :-
    member(M-F/A, Reachable),
    functor(H, F, A),
    clause(M:H, B),
    term_subterm(Sub, B),
    gate_kind(Sub, write),
    !.

ungated_write(Index, M-F/A) :-
    entry_pred_key(M-F/A),
    pred_closure(Index, M-F/A, 4, [], Reachable),
    reachable_opens_write(Reachable),
    \+ reachable_has_write_gate(Reachable).

% Entry points that reach a write transaction while carrying no gate
% anywhere in their reachable set, with the reason each is safe. A new
% entry here is a deliberate decision, not an accident.
gate_delegates(api_graphql:handle_graphql_request/8).   % mutations gated in Rust (graphql/mutation.rs check_write_auth)
gate_delegates(db_create:create_ref_layer/1).           % internal step; caller create_db checks 'create_database'
gate_delegates(db_create:create_schema/4).              % internal step; caller create_db checks 'create_database'
gate_delegates(db_create:finalize_db/1).                % internal step; caller create_db checks 'create_database'
gate_delegates(db_fast_forward:fast_forward_branch/3).  % internal helper; callers (db_pull, db_clone) gate it
gate_delegates(db_create:create_schema/3).              % internal step; caller create_db checks 'create_database'
gate_delegates(db_delete:force_delete_db/2).            % cleanup helper; callers (delete_db/5, db_clone) gate it
gate_delegates(routes:graphql_handler/5).               % delegates to handle_graphql_request; mutations gated in Rust

test(exported_api_predicates_opening_write_transactions_have_write_gate, []) :-
    build_index(Index),
    findall(M:F/A, ungated_write(Index, M-F/A), Raw),
    sort(Raw, Flagged),
    findall(M:F/A, gate_delegates(M:F/A), DRaw),
    sort(DRaw, Delegated),
    subtract(Flagged, Delegated, Violations),
    (   Violations = []
    ->  true
    ;   format(user_error, '~nAPI predicates opening write transactions without a write gate: ~p~n', [Violations]),
        fail
    ).

:- end_tests(write_gates).
