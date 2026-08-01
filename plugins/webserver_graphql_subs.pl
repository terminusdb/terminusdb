:- module(webserver_graphql_subs, []).

:- use_module(core(plugin_api)).
:- use_module(library(lists)).
:- use_module(library(json)).

%% Register the WebSocket route for GraphQL subscriptions.
%% The actual WebSocket handling is in Rust; this hook only tells Rust
%% to register the route at /api/graphql-ws/*path.
:- multifile appserver_hooks:appserver_ws/3.
appserver_hooks:appserver_ws(get, '/api/graphql-ws/*path',
                              webserver_graphql_subs:graphql_ws_handler).

%% Placeholder handler — the real work is done in Rust.
graphql_ws_handler.

%% ---------------------------------------------------------------------------
%% Dynamic predicates
%% ---------------------------------------------------------------------------

%% graphql_subscription(StreamId, CohortKey, FilterArgs, SelectionSet)
:- dynamic graphql_subscription/4.

%% graphql_cohort(CohortKey, BranchPath, RawChannel, MemberCount)
:- dynamic graphql_cohort/4.

%% graphql_broadcast_sent(BranchPath, CommitId)
%% Dedup predicate to prevent duplicate broadcasts when post_commit_hook
%% fires multiple times for the same commit.
:- dynamic graphql_broadcast_sent/2.

%% Mutex for cohort registry operations
:- mutex_create(graphql_cohort_registry, [alias(graphql_cohort_registry)]).

%% ---------------------------------------------------------------------------
%% register_subscription(+BranchPath, +QueryString, +Variables,
%%                       +OperationName, -CohortKey, -RawChannel) is det.
%%
%% Called via one-shot pipe dispatch from Rust.
%% Opens a transaction, gets frames, calls Rust FFI to parse the query,
%% then delegates to register_subscription_parsed/5 for cohort bookkeeping.
%% ---------------------------------------------------------------------------
register_subscription(BranchPath, QueryString, _Variables, _OperationName,
                      CohortKey, RawChannel) :-
    %% Validate BranchPath is a non-empty atom or string
    (   atom(BranchPath) -> BranchPathAtom = BranchPath
    ;   string(BranchPath) -> atom_string(BranchPathAtom, BranchPath)
    ;   throw(error(type_error(branch_path, BranchPath), _))
    ),
    BranchPathAtom \== '',
    %% Open transaction for this branch to get frames
    resolve_absolute_string_descriptor(BranchPathAtom, Descriptor),
    open_descriptor(Descriptor, Transaction),
    %% Get or create GraphQL context (same as api_graphql.pl)
    (   '$graphql':get_cached_graphql_context(Transaction, Graphql_Context)
    ->  true
    ;   all_class_frames(Transaction, Frames,
            [compress_ids(true), expand_abstract(true), simple(true)]),
        '$graphql':get_graphql_context(Transaction, Frames, Graphql_Context)
    ),
    %% Call Rust FFI to parse the subscription query
    '$graphql':parse_subscription_query(Graphql_Context, QueryString, Parsed),
    %% Extract parsed components from the returned dict
    get_dict(class_name, Parsed, ClassName),
    get_dict(operation, Parsed, Operation),
    get_dict(filter_canonical_json, Parsed, FilterJson),
    get_dict(selection_set_hash, Parsed, SelectionHash),
    %% Delegate to register_subscription_parsed/5
    register_subscription_parsed(BranchPathAtom, ClassName, Operation,
                                 FilterJson, SelectionHash,
                                 CohortKey, RawChannel).

%% ---------------------------------------------------------------------------
%% register_subscription_parsed(+BranchPath, +ClassName, +Operation,
%%                               +FilterJson, +SelectionHash,
%%                               -CohortKey, -RawChannel) is det.
%%
%% Cohort bookkeeping with pre-parsed components. Called by
%% register_subscription/6 after FFI parsing, or directly in tests.
%% ---------------------------------------------------------------------------
register_subscription_parsed(BranchPath, ClassName, Operation,
                             _FilterJson, SelectionHash,
                             CohortKey, RawChannel) :-
    %% Validate inputs
    (   atom(BranchPath) -> BranchPathAtom = BranchPath
    ;   string(BranchPath) -> atom_string(BranchPathAtom, BranchPath)
    ;   throw(error(type_error(branch_path, BranchPath), _))
    ),
    BranchPathAtom \== '',
    atom(ClassName), ClassName \== '',
    memberchk(Operation, [added, changed, deleted]),
    !,
    %% Construct cohort key as a compound term — no separator collision,
    %% structured unification for extraction, validated components.
    CohortKey = cohort(BranchPathAtom, ClassName, Operation, SelectionHash),
    %% Register under mutex to ensure atomic cohort creation
    with_mutex(graphql_cohort_registry,
        (   (   graphql_cohort(CohortKey, BranchPathAtom, RawChannel, Count)
            ->  NewCount is Count + 1,
                retract(graphql_cohort(CohortKey, BranchPathAtom, RawChannel, Count)),
                assertz(graphql_cohort(CohortKey, BranchPathAtom, RawChannel, NewCount))
            ;   %% Construct channel name from the canonical term representation
                term_to_atom(CohortKey, CohortKeyAtom),
                atom_concat('graphql_raw_', CohortKeyAtom, RawChannel),
                assertz(graphql_cohort(CohortKey, BranchPathAtom, RawChannel, 1))
            )
        )),
    !.
register_subscription_parsed(_, _, _, _, _, _, _) :-
    throw(error(subscription_registration_failed, _)).

%% ---------------------------------------------------------------------------
%% unregister_subscription(+CohortKey, +StreamId) is det.
%%
%% Removes the subscription, decrements cohort member count,
%% and cleans up empty cohorts.
%% ---------------------------------------------------------------------------

unregister_subscription(CohortKey, StreamId) :-
    with_mutex(graphql_cohort_registry,
        (   retract(graphql_subscription(StreamId, CohortKey, _, _))
        ->  (   retract(graphql_cohort(CohortKey, BranchPath, RawChannel, Count))
            ->  NewCount is Count - 1,
                (   NewCount > 0
                ->  assertz(graphql_cohort(CohortKey, BranchPath, RawChannel, NewCount))
                ;   true
                )
            ;   true
            )
        ;   true
        )).

%% ---------------------------------------------------------------------------
%% decrement_cohort(+CohortKey) is det.
%%
%% Decrements the member count of a cohort, cleaning up if empty.
%% ---------------------------------------------------------------------------

decrement_cohort(CohortKey) :-
    (   retract(graphql_cohort(CohortKey, BranchPath, RawChannel, Count))
    ->  NewCount is Count - 1,
        (   NewCount > 0
        ->  assertz(graphql_cohort(CohortKey, BranchPath, RawChannel, NewCount))
        ;   true
        )
    ;   true
    ).

%% ---------------------------------------------------------------------------
%% sweep_stale_graphql_streams is det.
%%
%% Retract graphql_subscription/4 entries whose Rust stream no longer exists.
%% ---------------------------------------------------------------------------

sweep_stale_graphql_streams :-
    findall(StreamId, graphql_subscription(StreamId, _, _, _), StreamIds),
    forall(
        (   member(StreamId, StreamIds),
            \+ catch('$appserver':appserver_stream_exists(StreamId), _, fail)
        ),
        (   retract(graphql_subscription(StreamId, CohortKey, _, _)),
            decrement_cohort(CohortKey)
        )
    ).

%% ---------------------------------------------------------------------------
%% broadcast_graphql_events(+Validation_Objects, +Meta_Data) is det.
%%
%% Called from post_commit_hook after every commit. Broadcasts compact
%% GraphQL subscription events to cohort channels via the Rust
%% BroadcastRegistry.
%% ---------------------------------------------------------------------------

broadcast_graphql_events(Validation_Objects, Meta_Data) :-
    %% Retract dedup predicate at the start
    retractall(graphql_broadcast_sent(_, _)),
    %% Sweep stale streams before broadcasting
    sweep_stale_graphql_streams,
    %% Collect unique branch paths with validation objects and commit IDs
    get_dict(data_versions, Meta_Data, DataVersions),
    findall(BranchPath-Validation_Object-CommitIdAtom,
            (   member(Validation_Object, Validation_Objects),
                is_dict(Validation_Object),
                get_dict(descriptor, Validation_Object, Descriptor),
                branch_descriptor{} :< Descriptor,
                member(Descriptor-data_version(branch, CommitIdAtom), DataVersions),
                webserver_commits:descriptor_to_branch_path(Descriptor, BranchPath)
            ),
            Triples),
    %% Deduplicate by BranchPath-CommitIdAtom
    webserver_commits:dedup_triples(Triples, UniqueTriples),
    forall(
        member(BranchPath-Validation_Object-CommitIdAtom, UniqueTriples),
        send_graphql_events(Validation_Object, BranchPath, CommitIdAtom)
    ).

%% send_graphql_events(+Validation_Object, +BranchPath, +CommitIdAtom) is det.
%%
%% Sends events for a single branch commit, with dedup.
send_graphql_events(Validation_Object, BranchPath, CommitIdAtom) :-
    (   atom(CommitIdAtom) -> CommitIdKey = CommitIdAtom
    ;   atom_string(CommitIdKey, CommitIdAtom)
    ),
    (   graphql_broadcast_sent(BranchPath, CommitIdKey)
    ->  true
    ;   do_broadcast_graphql_events(Validation_Object, BranchPath, CommitIdAtom),
        assertz(graphql_broadcast_sent(BranchPath, CommitIdKey))
    ).

%% do_broadcast_graphql_events(+Validation_Object, +BranchPath, +CommitIdAtom) is det.
%%
%% Collects changed documents filtered by active subscription types,
%% and broadcasts compact events to the Rust BroadcastRegistry.
do_broadcast_graphql_events(Validation_Object, BranchPath, _CommitIdAtom) :-
    (   graphql_cohort(_, BranchPath, _, _)
    ->  get_dict(descriptor, Validation_Object, Descriptor),
        open_descriptor(Descriptor, Transaction),
        database_schema(Transaction, Schema),
        database_instance(Transaction, Instance),
        forall(
            graphql_cohort(CohortKey, BranchPath, RawChannel, _),
            broadcast_cohort_events(CohortKey, RawChannel, Schema, Instance)
        )
    ;   true
    ).

%% broadcast_cohort_events(+CohortKey, +RawChannel, +Schema, +Instance) is det.
%%
%% Broadcasts events for a single cohort. Phase 4 will implement the actual
%% event resolution and broadcasting via the Rust BroadcastRegistry.
broadcast_cohort_events(_CohortKey, _RawChannel, _Schema, _Instance) :-
    true.

%% ---------------------------------------------------------------------------
%% descriptor_to_branch_path is provided by webserver_commits — use that
%% implementation instead of duplicating it here.
%% ---------------------------------------------------------------------------

%% ---------------------------------------------------------------------------
%% dedup_triples is provided by webserver_commits — use that implementation
%% instead of duplicating it here.
%% ---------------------------------------------------------------------------

%% ---------------------------------------------------------------------------
%% active_subscription_type(+BranchPath, -TypeIRI) is nondet.
%%
%% Finds all type IRIs that have active subscriptions on a branch.
%% ---------------------------------------------------------------------------

active_subscription_type(BranchPath, TypeIRI) :-
    graphql_cohort(CohortKey, BranchPath, _, _),
    cohort_class(CohortKey, ClassName),
    class_to_type_iri(BranchPath, ClassName, TypeIRI).

%% cohort_class(+CohortKey, -ClassName) is det.
%%
%% Extracts the class name from a cohort key compound term.
%% CohortKey format: cohort(BranchPath, ClassName, Operation, SelectionHash)
cohort_class(cohort(_, ClassName, _, _), ClassName).

%% class_to_type_iri(+BranchPath, +ClassName, -TypeIRI) is nondet.
%%
%% Resolves a class name to its IRI in the schema, and finds all
%% subclass IRIs on backtracking. Phase 4 will implement proper resolution.
class_to_type_iri(_BranchPath, _ClassName, _TypeIRI) :-
    fail.

%% ---------------------------------------------------------------------------
%% document_class(+BranchPath, +DocIRI, -Class) is nondet.
%%
%% Determines the class(es) of a document by looking up its rdf:type
%% in the current commit's instance layer.
%% ---------------------------------------------------------------------------

document_class(BranchPath, DocIRI, Class) :-
    resolve_absolute_string_descriptor(BranchPath, Descriptor),
    open_descriptor(Descriptor, Transaction),
    database_instance(Transaction, Instance),
    once(rdf(Instance, DocIRI, rdf:type, ClassIRI)),
    class_iri_to_name(ClassIRI, Class).

%% class_iri_to_name(+ClassIRI, -ClassName) is det.
%%
%% Converts a class IRI to its GraphQL class name. Placeholder.
class_iri_to_name(ClassIRI, ClassName) :-
    (   atom(ClassIRI)
    ->  atom_string(ClassIRI, ClassStr)
    ;   ClassIRI = ClassStr
    ),
    atom_string(ClassName, ClassStr).

%% ---------------------------------------------------------------------------
%% PLUnit tests
%% ---------------------------------------------------------------------------

:- use_module(library(plunit)).
:- begin_tests(webserver_graphql_subs, []).

test(register_and_unregister, [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    %% Use register_subscription_parsed/7 directly — no FFI needed in unit tests
    register_subscription_parsed('test/db/local/branch/main', 'Person', added,
                                 '{}', 'hash1',
                                 CohortKey, RawChannel1),
    %% CohortKey is a compound term: cohort(BranchPath, Class, Op, Hash)
    compound(CohortKey),
    CohortKey = cohort('test/db/local/branch/main', 'Person', added, 'hash1'),
    atom(RawChannel1),
    webserver_graphql_subs:graphql_cohort(CohortKey, 'test/db/local/branch/main', RawChannel1, 1),
    %% Assert a subscription for this stream
    assertz(webserver_graphql_subs:graphql_subscription(stream1, CohortKey, {}, '_id')),
    %% Second subscription to same class+operation+selection joins same cohort
    register_subscription_parsed('test/db/local/branch/main', 'Person', added,
                                 '{}', 'hash1',
                                 CohortKey2, RawChannel2),
    CohortKey == CohortKey2,
    RawChannel1 == RawChannel2,
    webserver_graphql_subs:graphql_cohort(CohortKey, 'test/db/local/branch/main', RawChannel1, 2),
    %% Assert second subscription
    assertz(webserver_graphql_subs:graphql_subscription(stream2, CohortKey, {}, '_id')),
    %% Unregister one
    unregister_subscription(CohortKey, stream1),
    webserver_graphql_subs:graphql_cohort(CohortKey, 'test/db/local/branch/main', RawChannel1, 1).

test(register_different_selection_different_cohort,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    %% Different selection set hashes produce different cohort keys
    register_subscription_parsed('test/db/local/branch/main', 'Person', added,
                                 '{}', 'hash1', CohortKey1, _),
    register_subscription_parsed('test/db/local/branch/main', 'Person', added,
                                 '{}', 'hash2', CohortKey2, _),
    CohortKey1 \== CohortKey2.

%% Security: compound term cohort key prevents separator collision attacks.
%% Even if branch paths or class names contain |, -, or other special chars,
%% the compound term structure ensures correct extraction.

test(cohort_key_no_collision_with_pipe_in_branch_path,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    %% A branch path containing | cannot collide with the term structure
    CohortKey = cohort('evil|path|injection', 'Person', added, 'abc123'),
    cohort_class(CohortKey, ClassName),
    ClassName == 'Person'.

test(cohort_key_no_collision_with_pipe_in_class_name,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    %% A class name containing | is correctly extracted
    CohortKey = cohort('test/db/local/branch/main', 'Evil|Class', added, 'abc123'),
    cohort_class(CohortKey, ClassName),
    ClassName == 'Evil|Class'.

test(cohort_key_no_collision_with_dash_in_branch_path,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    %% A branch path containing - is correctly extracted
    CohortKey = cohort('test-db/local/branch/main', 'Person', added, 'abc123'),
    cohort_class(CohortKey, ClassName),
    ClassName == 'Person'.

test(cohort_key_term_to_atom_roundtrip_is_safe) :-
    %% term_to_atom produces a canonical, escapable representation
    %% that roundtrips correctly even with special characters
    CohortKey = cohort('test/db/local/branch/main', 'Person', added, 'abc123'),
    term_to_atom(CohortKey, Atom),
    atom_to_term(Atom, CohortKey2, []),
    CohortKey == CohortKey2.

test(cohort_key_term_to_atom_roundtrip_with_special_chars) :-
    %% Adversarial input with quotes, pipes, dashes, and backslashes
    %% must survive term_to_atom → atom_to_term roundtrip intact
    CohortKey = cohort('a\'b|c-d\\e', 'Person"evil', added, 'abc123'),
    term_to_atom(CohortKey, Atom),
    atom_to_term(Atom, CohortKey2, []),
    CohortKey == CohortKey2.

test(cohort_key_distinct_components_no_collision) :-
    %% Two cohort keys with different components must never be equal,
    %% even if the string representations could be confused
    CohortKey1 = cohort('a', 'b', added, 'c'),
    CohortKey2 = cohort('a|b', added, 'c'),  % wrong arity — different term
    CohortKey1 \== CohortKey2.

test(register_rejects_empty_branch_path,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    catch(
        register_subscription_parsed('', 'Person', added,
                                      '{}', 'hash1', _, _),
        Error,
        Error = error(subscription_registration_failed, _)
    ).

test(register_rejects_non_atom_branch_path,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    catch(
        register_subscription_parsed(42, 'Person', added,
                                      '{}', 'hash1', _, _),
        Error,
        Error = error(type_error(branch_path, 42), _)
    ).

:- end_tests(webserver_graphql_subs).

%% Cleanup helper for tests
cleanup_cohorts :-
    retractall(webserver_graphql_subs:graphql_subscription(_, _, _, _)),
    retractall(webserver_graphql_subs:graphql_cohort(_, _, _, _)),
    retractall(webserver_graphql_subs:graphql_broadcast_sent(_, _)).

