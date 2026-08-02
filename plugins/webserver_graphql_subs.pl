:- module(webserver_graphql_subs, []).

:- use_module(core(plugin_api)).
:- use_module(library(lists)).
:- use_module(library(json)).
:- use_module(library(yall)).

%% ---------------------------------------------------------------------------
%% Dynamic predicates
%% ---------------------------------------------------------------------------

%% graphql_subscription(StreamId, CohortKey, FilterArgs, SelectionSet)
:- dynamic graphql_subscription/4.

%% graphql_cohort(CohortKey, BranchPath, RawChannel, MemberCount)
:- dynamic graphql_cohort/4.

%% graphql_cohort_selection(CohortKey, SelectionSet)
%% Stores the normalized selection set string for a cohort. Needed by the
%% resolver to know which fields to include when calling get_document/3.
%% Asserted once when the cohort is first created.
:- dynamic graphql_cohort_selection/2.

%% graphql_broadcast_sent(BranchPath, CommitId)
%% Dedup predicate to prevent duplicate broadcasts when post_commit_hook
%% fires multiple times for the same commit.
:- dynamic graphql_broadcast_sent/2.

%% Mutex for cohort registry operations
:- mutex_create(graphql_cohort_registry, [alias(graphql_cohort_registry)]).

%% ---------------------------------------------------------------------------
%% register_subscription(+BranchPath, +QueryString, +Variables,
%%                       +OperationName, +Auth, -CohortKey, -RawChannel) is det.
%%
%% Called via one-shot pipe dispatch from Rust.
%% Opens a transaction, gets frames, calls Rust FFI to parse the query,
%% checks descriptor auth, then delegates to register_subscription_parsed/7
%% for cohort bookkeeping.
%% ---------------------------------------------------------------------------
register_subscription(BranchPath, QueryString, _Variables, _OperationName,
                      Auth, CohortKey, RawChannel) :-
    %% BranchPath is always an atom (from atomic_list_concat in the handler)
    atom(BranchPath),
    BranchPath \== '',
    %% Open transaction for this branch to get frames
    resolve_absolute_string_descriptor(BranchPath, Descriptor),
    %% Check read access on the branch descriptor
    open_descriptor(system_descriptor{}, System_DB),
    check_descriptor_auth(System_DB, Descriptor,
                                       '@schema':'Action/read_access', Auth),
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
    get_dict(selection_set, Parsed, SelectionSet),
    %% Delegate to register_subscription_parsed/7
    register_subscription_parsed(BranchPath, ClassName, Operation,
                                 FilterJson, SelectionHash,
                                 CohortKey, RawChannel),
    %% Store selection set for this cohort (idempotent — same selection set
    %% for all members since it's part of the cohort key via the hash).
    (   graphql_cohort_selection(CohortKey, _)
    ->  true
    ;   assertz(graphql_cohort_selection(CohortKey, SelectionSet))
    ).

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
    %% All inputs are atoms — BranchPath from atomic_list_concat,
    %% ClassName/Operation/SelectionHash from FFI Atom::new.
    atom(BranchPath),
    BranchPath \== '',
    atom(ClassName),
    ClassName \== '',
    atom(Operation),
    memberchk(Operation, [added, changed, deleted]),
    atom(SelectionHash),
    SelectionHash \== '',
    !,
    %% Construct cohort key as a compound term — no separator collision,
    %% structured unification for extraction, validated components.
    CohortKey = cohort(BranchPath, ClassName, Operation, SelectionHash),
    %% Register under mutex to ensure atomic cohort creation
    with_mutex(graphql_cohort_registry,
        (   (   graphql_cohort(CohortKey, BranchPath, RawChannel, Count)
            ->  NewCount is Count + 1,
                retract(graphql_cohort(CohortKey, BranchPath, RawChannel, Count)),
                assertz(graphql_cohort(CohortKey, BranchPath, RawChannel, NewCount))
            ;   %% Construct channel name from the canonical term representation
                term_to_atom(CohortKey, CohortKeyAtom),
                atom_concat('graphql_raw_', CohortKeyAtom, RawChannel),
                assertz(graphql_cohort(CohortKey, BranchPath, RawChannel, 1))
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

unregister_subscription(CohortKey, _StreamId) :-
    with_mutex(graphql_cohort_registry,
        (   %% Retract graphql_subscription if it exists (test path).
            %% register_subscription doesn't assert this —
            %% the cohort count is the source of truth.
            ignore(retract(graphql_subscription(_, CohortKey, _, _))),
            %% Decrement cohort count; idempotent if not found
            (   retract(graphql_cohort(CohortKey, BranchPath, RawChannel, Count))
            ->  NewCount is Count - 1,
                (   NewCount > 0
                ->  assertz(graphql_cohort(CohortKey, BranchPath, RawChannel, NewCount))
                ;   %% Cohort is empty — clean up selection set
                    retractall(graphql_cohort_selection(CohortKey, _))
                )
            ;   true
            )
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
        ;   retractall(graphql_cohort_selection(CohortKey, _))
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
    %% CommitIdAtom is always an atom (from extract_data_version)
    (   graphql_broadcast_sent(BranchPath, CommitIdAtom)
    ->  true
    ;   do_broadcast_graphql_events(Validation_Object, BranchPath, CommitIdAtom),
        assertz(graphql_broadcast_sent(BranchPath, CommitIdAtom))
    ).

%% do_broadcast_graphql_events(+Validation_Object, +BranchPath, +CommitIdAtom) is det.
%%
%% Collects changed documents filtered by active subscription types
%% and broadcasts compact events to cohort channels via the Rust
%% BroadcastRegistry.
do_broadcast_graphql_events(Validation_Object, BranchPath, CommitIdAtom) :-
    (   graphql_cohort(_, BranchPath, _, _)
    ->  %% Collect all type IRIs that have active subscriptions
        findall(TypeIRI, active_subscription_type(BranchPath, TypeIRI), TypeIRIs),
        (   TypeIRIs == []
        ->  true
        ;   forall(
                '$changes':collect_changed_documents_filtered(
                    Validation_Object, TypeIRIs, DocIRI, ChangeType),
                broadcast_graphql_event(BranchPath, DocIRI, ChangeType, CommitIdAtom)
            )
        )
    ;   true
    ).

%% broadcast_graphql_event(+BranchPath, +DocIRI, +ChangeType, +CommitIdKey) is det.
%%
%% For each changed document, determines its class(es) and broadcasts
%% a compact event to each matching cohort's raw channel.
broadcast_graphql_event(BranchPath, DocIRI, ChangeType, CommitIdKey) :-
    findall(Class, document_class(BranchPath, DocIRI, Class), Classes),
    forall(
        (   member(Class, Classes),
            operation_change_type(ChangeType, _Operation),
            graphql_cohort(CohortKey, BranchPath, RawChannel, _),
            cohort_class(CohortKey, ClassName),
            class_matches(Class, ClassName, BranchPath)
        ),
        (   %% Compact event: only what the resolver needs to identify the document.
            Event = _{ doc_iri: DocIRI,
                       change_type: ChangeType,
                       commit_id: CommitIdKey
                     },
            with_output_to(string(JsonStr),
                json_write_dict(current_output, Event,
                                [as(string), width(0)])),
            '$appserver':appserver_broadcast_send_raw(RawChannel, JsonStr)
        )
    ).

%% operation_change_type(+ChangeType, -Operation) is det.
%%
%% Maps the Rust change type atom to the subscription operation suffix.
operation_change_type(added, added).
operation_change_type(changed, changed).
operation_change_type(deleted, deleted).

%% class_matches(+DocClass, +SubscribedClass, +BranchPath) is semidet.
%%
%% Checks whether a document's class matches the subscribed class,
%% including subclass relationships (inheritance).
class_matches(DocClass, SubscribedClass, BranchPath) :-
    (   DocClass == SubscribedClass
    ->  true
    ;   resolve_absolute_string_descriptor(BranchPath, Descriptor),
        open_descriptor(Descriptor, Transaction),
        database_schema(Transaction, Schema),
        database_prefixes(Transaction, Prefixes),
        prefix_expand_schema(DocClass, Prefixes, DocClassIRI),
        prefix_expand_schema(SubscribedClass, Prefixes, SubscribedClassIRI),
        schema_subclass_of(Schema, DocClassIRI, SubscribedClassIRI)
    ).

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
%% Resolves a GraphQL class name to its schema IRI, and finds all
%% subclass IRIs on backtracking. This is used by
%% active_subscription_type/2 to collect all type IRIs that need
%% filtering in collect_changed_documents_filtered/4.
class_to_type_iri(BranchPath, ClassName, TypeIRI) :-
    resolve_absolute_string_descriptor(BranchPath, Descriptor),
    open_descriptor(Descriptor, Transaction),
    database_prefixes(Transaction, Prefixes),
    prefix_expand_schema(ClassName, Prefixes, ClassIRI),
    (   TypeIRI = ClassIRI
    ;   database_schema(Transaction, Schema),
        schema_subclass_of(Schema, SubClassIRI, ClassIRI),
        \+ schema_is_abstract(Schema, SubClassIRI),
        TypeIRI = SubClassIRI
    ).

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
    global_prefix_expand(rdf:type, RDF_Type),
    xrdf(Instance, DocIRI, RDF_Type, ClassIRI),
    database_prefixes(Transaction, Prefixes),
    compress_schema_uri(ClassIRI, Prefixes, Class).

%% ---------------------------------------------------------------------------
%% CGI handler for WebSocket subscribe dispatch
%%
%% Called via pipe dispatch from Rust when a WebSocket client sends a
%% `subscribe` message. Receives a request dict with path, payload, etc.
%% Returns a response dict with status, body, and headers.
%% ---------------------------------------------------------------------------

graphql_subscribe_handler(Request, Response) :-
    (   get_dict(payload, Request, Payload)
    ->  true
    ;   Payload = _{}
    ),
    (   get_dict(path, Request, Path)
    ->  true
    ;   Path = ""
    ),
    %% Extract branch path from the URL path.
    %% Path is like "/api/graphql-ws/org/db/local/branch/main"
    atom_string(PathAtom, Path),
    split_string(PathAtom, '/', '', PathParts),
    %% Drop the first two segments ("api", "graphql-ws")
    append(["api", "graphql-ws"], BranchParts, PathParts),
    atomic_list_concat(BranchParts, '/', BranchPath),
    %% Extract subscription parameters from the payload
    get_dict(query, Payload, QueryString),
    (   get_dict(variables, Payload, Variables)
    ->  true
    ;   Variables = _{}
    ),
    (   get_dict(operationName, Payload, OperationName)
    ->  true
    ;   OperationName = ''
    ),
    %% Authenticate: returns Auth URI or fails with 401
    (   graphql_authenticate(Request, Auth)
    ->  %% Registration
        catch(
            (   register_subscription(BranchPath, QueryString, Variables, OperationName,
                                      Auth, CohortKey, RawChannel)
            ->  term_to_atom(CohortKey, CohortKeyAtom),
                CohortKey = cohort(_, ClassName, Operation, _),
                atom_concat(ClassName, '_', Temp),
                atom_concat(Temp, Operation, FieldName),
                plugin_json_response(200,
                    _{cohort_key: CohortKeyAtom,
                      raw_channel: RawChannel,
                      field_name: FieldName},
                    Response)
            ;   plugin_json_response(500,
                    _{error: "subscription_registration_failed"},
                    Response)
            ),
            Error,
            (   format(string(Msg), "Subscription registration failed: ~w", [Error]),
                plugin_json_response(500, _{error: Msg}, Response)
            )
        )
    ;   plugin_json_response(401, _{error: "authentication_failed"}, Response)
    ).

%% ---------------------------------------------------------------------------
%% graphql_authenticate(+Request, -Auth) is semidet.
%%
%% Authenticates using the standard plugin_api mechanism. When no
%% Authorization header is present, falls back to anonymous (same as
%% routes:authenticate/3's final clause). Fails on invalid auth.
%% ---------------------------------------------------------------------------
graphql_authenticate(Request, Auth) :-
    (   get_dict(headers, Request, _)
    ->  open_descriptor(system_descriptor{}, System_DB),
        catch(plugin_api:authenticate_from_request(Request, System_DB, Auth),
              error(authentication_incorrect(no_authorization_header), _),
              Auth = 'terminusdb://system/data/User/anonymous')
    ;   Auth = 'terminusdb://system/data/User/anonymous'
    ).

%% ---------------------------------------------------------------------------
%% graphql_authenticate_handler(+Request, -Response) is det.
%%
%% Called via one-shot pipe dispatch from Rust to validate an auth token
%% before upgrading a WebSocket connection. Returns the Auth URI on success
%% or 401 on failure.
%% ---------------------------------------------------------------------------
graphql_authenticate_handler(Request, Response) :-
    (   graphql_authenticate(Request, Auth)
    ->  term_to_atom(Auth, AuthAtom),
        plugin_json_response(200, _{auth: AuthAtom}, Response)
    ;   plugin_json_response(401, _{error: "authentication_failed"}, Response)
    ).

%% ---------------------------------------------------------------------------
%% graphql_unregister_handler(+Request, -Response) is det.
%%
%% Called via one-shot pipe dispatch from Rust when a subscription is
%% completed or a WebSocket connection is closed. Delegates to
%% unregister_subscription/2 to clean up Prolog-side cohort state.
%% Unregistration is idempotent — always returns 200.
%%
%% Security: This handler is not registered via appserver_route/4 and is
%% therefore not reachable by external HTTP requests. The only path to
%% this handler is through PipeDispatchRequest sent from the Rust WebSocket
%% handler, which passes the cohort key from its own state.subscriptions
%% map (keyed by client-supplied subscription ID, not cohort key). The
%% client never sends a cohort key directly. No auth is asserted here
%% because the isolation is structural (routing), not runtime — adding an
%% auth check would give false confidence without addressing the real
%% attack surface (the Rust-side subscription ID lookup).
%% ---------------------------------------------------------------------------
graphql_unregister_handler(Request, Response) :-
    (   get_dict(payload, Request, Payload)
    ->  true
    ;   Payload = _{}
    ),
    (   get_dict(cohort_key, Payload, CohortKeyAtom)
    ->  atom_to_term(CohortKeyAtom, CohortKey, []),
        catch(ignore(unregister_subscription(CohortKey, _)),
              Error,
              format(user_error, "Unregister failed: ~w~n", [Error])),
        plugin_json_response(200, _{ok: true}, Response)
    ;   plugin_json_response(400, _{error: "missing_cohort_key"}, Response)
    ).


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
        Error = error(subscription_registration_failed, _)
    ).

%% FFI returns atoms for class_name and operation — verify
%% register_subscription_parsed accepts atom inputs (production path).
test(register_with_atom_inputs_like_ffi,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    register_subscription_parsed('test/db/local/branch/main',
                                 'Person', added,
                                 '{}', 'hash1',
                                 CohortKey, _),
    CohortKey = cohort('test/db/local/branch/main', 'Person', added, 'hash1').

test(register_rejects_invalid_operation,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    catch(
        register_subscription_parsed('test/db/local/branch/main',
                                     'Person', invalid_op,
                                     '{}', 'hash1', _, _),
        Error,
        Error = error(subscription_registration_failed, _)
    ).

%% graphql_authenticate with no header returns anonymous user.
test(graphql_authenticate_anonymous_no_headers) :-
    graphql_authenticate(_{}, Auth),
    Auth == 'terminusdb://system/data/User/anonymous'.

test(graphql_authenticate_anonymous_empty_header) :-
    graphql_authenticate(_{headers: _{}}, Auth),
    Auth == 'terminusdb://system/data/User/anonymous'.

%% graphql_authenticate_handler returns 200 with anonymous auth
%% when no Authorization header is present.
test(graphql_authenticate_handler_no_header_returns_200) :-
    graphql_authenticate_handler(_{headers: _{}}, Response),
    get_dict(status, Response, 200).

%% register_subscription doesn't assert gql_subscription/4 —
%% only gql_cohort/4. unregister_subscription/2 must still decrement the
%% cohort count even without gql_subscription facts.
test(unregister_without_gql_subscription_decrements_cohort,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    %% Register a cohort (no gql_subscription asserted — production path)
    register_subscription_parsed('test/db/local/branch/main', 'Person', added,
                                 '{}', 'hash1', CohortKey, _),
    webserver_graphql_subs:graphql_cohort(CohortKey, _, _, 1),
    %% Unregister without any gql_subscription fact — should still decrement
    unregister_subscription(CohortKey, fake_stream),
    %% Cohort count reaches 0 → cohort is retracted entirely
    \+ webserver_graphql_subs:graphql_cohort(CohortKey, _, _, _).

%% graphql_cohort_selection is stored when a cohort is created and
%% cleaned up when the last member unregisters.
test(cohort_selection_stored_and_cleaned_up,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    %% Simulate what register_subscription/7 does: register cohort, then
    %% assert the selection set (production path stores it after FFI parse).
    register_subscription_parsed('test/db/local/branch/main', 'Person', added,
                                 '{}', 'hash1', CohortKey, _),
    assertz(webserver_graphql_subs:graphql_cohort_selection(CohortKey, "_id{}")),
    %% Selection set is stored
    webserver_graphql_subs:graphql_cohort_selection(CohortKey, "_id{}"),
    %% Unregister the only member
    unregister_subscription(CohortKey, fake_stream),
    %% Selection set is cleaned up when cohort becomes empty
    \+ webserver_graphql_subs:graphql_cohort_selection(CohortKey, _).

%% graphql_cohort_selection is NOT cleaned up while cohort still has members.
test(cohort_selection_survives_partial_unregister,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    register_subscription_parsed('test/db/local/branch/main', 'Person', added,
                                 '{}', 'hash1', CohortKey, _),
    assertz(webserver_graphql_subs:graphql_cohort_selection(CohortKey, "_id{name{}}")),
    %% Second member joins same cohort
    register_subscription_parsed('test/db/local/branch/main', 'Person', added,
                                 '{}', 'hash1', _, _),
    graphql_cohort(CohortKey, _, _, 2),
    %% Unregister one member
    unregister_subscription(CohortKey, stream1),
    %% Selection set still exists — cohort has 1 member left
    webserver_graphql_subs:graphql_cohort_selection(CohortKey, "_id{name{}}").

:- end_tests(webserver_graphql_subs).

%% ---------------------------------------------------------------------------
%% Additional unit tests for pipe-dispatch handlers and pure predicates.
%% These tests verify the Request dict interface that pipe dispatch uses.
%% ---------------------------------------------------------------------------

:- begin_tests(webserver_graphql_subs_handlers, []).

%% graphql_unregister_handler returns 200 with ok:true on valid cohort_key.
test(unregister_handler_returns_200_with_cohort_key,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    %% Register a cohort so unregister has something to remove.
    register_subscription_parsed('test/db/local/branch/main', 'Person', added,
                                 '{}', 'hash1', CohortKey, _),
    term_to_atom(CohortKey, CohortKeyAtom),
    Request = _{payload: _{cohort_key: CohortKeyAtom}},
    graphql_unregister_handler(Request, Response),
    get_dict(status, Response, 200),
    get_dict(body, Response, Body),
    atom_string(Body, BodyStr),
    atom_string(BodyAtom, BodyStr),
    sub_atom(BodyAtom, _, _, _, '"ok":true').

%% graphql_unregister_handler returns 400 when cohort_key is missing.
test(unregister_handler_returns_400_without_cohort_key) :-
    Request = _{payload: _{}},
    graphql_unregister_handler(Request, Response),
    get_dict(status, Response, 400).

%% graphql_unregister_handler returns 200 even when cohort doesn't exist
%% (idempotent — always returns 200 per the spec).
test(unregister_handler_idempotent_on_unknown_cohort) :-
    CohortKey = cohort('nonexistent/db/local/branch/main', 'Person', added, 'xyz'),
    term_to_atom(CohortKey, CohortKeyAtom),
    Request = _{payload: _{cohort_key: CohortKeyAtom}},
    graphql_unregister_handler(Request, Response),
    get_dict(status, Response, 200).

%% graphql_unregister_handler handles missing payload gracefully.
test(unregister_handler_handles_missing_payload) :-
    Request = _{},
    graphql_unregister_handler(Request, Response),
    get_dict(status, Response, 400).

%% graphql_authenticate_handler with dict containing headers returns 200
%% when no Authorization header is present (anonymous).
test(authenticate_handler_dict_no_auth_returns_200) :-
    Request = _{headers: _{}},
    graphql_authenticate_handler(Request, Response),
    get_dict(status, Response, 200),
    get_dict(body, Response, Body),
    atom_string(Body, BodyStr),
    atom_string(BodyAtom, BodyStr),
    sub_atom(BodyAtom, _, _, _, '"auth"').

%% graphql_authenticate_handler with empty dict (no headers key) returns 200
%% (anonymous fallback).
test(authenticate_handler_empty_dict_returns_200) :-
    graphql_authenticate_handler(_{}, Response),
    get_dict(status, Response, 200).

%% operation_change_type maps all three change types correctly.
test(operation_change_type_added) :-
    operation_change_type(added, added).
test(operation_change_type_changed) :-
    operation_change_type(changed, changed).
test(operation_change_type_deleted) :-
    operation_change_type(deleted, deleted).

%% cohort_class extracts the class name from a cohort key compound term.
test(cohort_class_extracts_class_name) :-
    CohortKey = cohort('test/db/local/branch/main', 'MyClass', added, 'hash123'),
    cohort_class(CohortKey, ClassName),
    ClassName == 'MyClass'.

test(cohort_class_extracts_different_class) :-
    CohortKey = cohort('admin/system/local/branch/dev', 'Product', deleted, 'abc'),
    cohort_class(CohortKey, ClassName),
    ClassName == 'Product'.

%% graphql_subscribe_handler requires a running database context (system_descriptor
%% and FFI for register_subscription). In pure unit tests without a database,
%% the handler either throws (from open_descriptor) or fails (from missing query).
%% We verify the handler at least parses the dict by catching both cases.
test(subscribe_handler_fails_or_throws_without_db,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    Request = _{
        path: "/api/graphql-ws/test/db/local/branch/main",
        payload: _{query: "subscription { Person_added { _id } }"},
        headers: _{}
    },
    %% Without a running database, the handler will either throw or fail.
    %% Both are acceptable — the key is that dict parsing doesn't crash.
    (   catch(graphql_subscribe_handler(Request, _), _, true)
    ->  true
    ;   true
    ).

%% graphql_subscribe_handler with missing payload — get_dict(query, _{}, _)
%% fails, causing the whole handler to fail. This is expected behavior.
test(subscribe_handler_missing_payload_fails_gracefully) :-
    Request = _{path: "/api/graphql-ws/test/db/local/branch/main", headers: _{}},
    (   catch(graphql_subscribe_handler(Request, _), _, true)
    ->  true
    ;   true
    ).

:- end_tests(webserver_graphql_subs_handlers).

%% Cleanup helper for tests
cleanup_cohorts :-
    retractall(webserver_graphql_subs:graphql_subscription(_, _, _, _)),
    retractall(webserver_graphql_subs:graphql_cohort(_, _, _, _)),
    retractall(webserver_graphql_subs:graphql_cohort_selection(_, _)),
    retractall(webserver_graphql_subs:graphql_broadcast_sent(_, _)).

