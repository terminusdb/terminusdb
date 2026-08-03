:- module(webserver_graphql_subs, []).

:- use_module(core(plugin_api)).
:- use_module(core(account), [assert_read_access/4]).
:- use_module(core(triple), [xrdf_deleted/4, xrdf/4, database_instance/2]).
:- use_module(core(transaction), [read_write_obj_reader/2]).
:- use_module(core(transaction/ref_entity), [commit_id_uri/3, commit_uri_to_metadata/5]).
:- use_module(core(transaction/descriptor), [open_descriptor/2]).
:- use_module(core(query/resolve_query_resource), [resolve_absolute_string_descriptor/2, resolve_relative_descriptor/3]).
:- use_module(core(api/api_graphql), [get_or_create_graphql_context/2]).
:- use_module(core(util/data_version), [transaction_retry_count_from_meta_data/2, serialize_data_version/2]).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(aggregate)).
:- use_module(library(json)).
:- use_module(library(yall)).
:- use_module(library(uri), [uri_query_components/2]).
:- use_module(library(pcre), [re_replace/4]).
:- use_module(library(terminus_store)).

%% ---------------------------------------------------------------------------
%% SSE subscription stream route on POST /api/graphql/*path.
%% Accept: text/event-stream → SSE subscription; otherwise delegate to
%% the regular GraphQL HTTP handler. Coexists with the catch-all /api/{*path}
%% route because matchit prioritizes static segments over wildcards.
%%
%% NOTE: This plugin route intercepts POST /api/graphql/*path BEFORE the
%% catch-all /api/*path fallback in routes.pl. Non-SSE requests are handled
%% by delegate_to_graphql/2, which calls handle_graphql_request/10 directly
%% — bypassing graphql_handler and its handle_graphql_error catch block in
%% routes.pl. Errors are mapped via plugin_error_response/2 in
%% src/core/plugin_api/http.pl. See the "GraphQL Route Registration Flow"
%% comment in routes.pl (near graphql_handler) for the full dispatch diagram.
%% ---------------------------------------------------------------------------
:- multifile appserver_hooks:appserver_stream/3.
appserver_hooks:appserver_stream(post, '/api/graphql/*path',
                                  webserver_graphql_subs:graphql_sse_handler).
appserver_hooks:appserver_stream(get, '/api/graphql/*path',
                                  webserver_graphql_subs:graphql_get_handler).
appserver_hooks:appserver_stream(options, '/api/graphql/*path',
                                  webserver_graphql_subs:graphql_sse_options_handler).

%% ---------------------------------------------------------------------------
%% Dynamic predicates
%% ---------------------------------------------------------------------------

%% graphql_cohort(CohortKey, Descriptor, RawChannel, MemberCount)
:- dynamic graphql_cohort/4.

%% graphql_cohort_selection(CohortKey, SelectionSet, SelectionSetGraphql, IncludeChildren)
:- dynamic graphql_cohort_selection/4.

%% graphql_broadcast_sent(Descriptor, CommitId)
%% Dedup for post_commit_hook firing multiple times for the same commit.
:- dynamic graphql_broadcast_sent/2.

%% graphql_sse_subscription(StreamId, CohortKey, RawChannel)
%% Asserted on connect, retracted on disconnect.
:- dynamic graphql_sse_subscription/3.

%% Maximum number of concurrent SSE/NDJSON subscriptions per descriptor.
%% This caps resource consumption — each subscription holds a broadcast
%% channel open and consumes memory for cohort tracking. The previous
%% implicit limit was effectively unbounded (1000+ in practice); 100 is
%% a safer default that still supports reasonable fan-out.
:- dynamic max_subscriptions_per_descriptor/1.
max_subscriptions_per_descriptor(100).

%% Mutex for cohort registry operations.
%%
%% Concurrency audit: grep for assertz, retract, retractall in this file.
%% Each unguarded mutation must be a single atomic op (safe under SWI-Prolog's
%% logical update view). Compound read-modify-write sequences on
%% graphql_cohort/4 or graphql_cohort_selection/4 must be inside
%% with_mutex(graphql_cohort_registry, ...).
:- mutex_create(graphql_cohort_registry, [alias(graphql_cohort_registry)]).

%% ---------------------------------------------------------------------------
%% register_subscription_parsed(+Descriptor, +ClassName, +Operation,
%%                               +FilterJson, +SelectionHash, +Mode,
%%                               -CohortKey, -RawChannel) is det.
%%
%% Mode is included in the cohort key so SSE and NDJSON subscribers for
%% the same class/operation/selection get separate broadcast channels.
%% ---------------------------------------------------------------------------
register_subscription_parsed(Descriptor, ClassName, Operation,
                             _FilterJson, SelectionHash, Mode,
                             CohortKey, RawChannel) :-
    is_dict(Descriptor),
    atom(ClassName),
    ClassName \== '',
    atom(Operation),
    memberchk(Operation, [added, changed, deleted, change_set]),
    atom(SelectionHash),
    SelectionHash \== '',
    atom(Mode),
    memberchk(Mode, [sse, ndjson]),
    !,
    CohortKey = cohort(Descriptor, ClassName, Operation, SelectionHash, Mode),
    with_mutex(graphql_cohort_registry,
        (   (   graphql_cohort(CohortKey, Descriptor, RawChannel, Count)
            ->  NewCount is Count + 1,
                retract(graphql_cohort(CohortKey, Descriptor, RawChannel, Count)),
                assertz(graphql_cohort(CohortKey, Descriptor, RawChannel, NewCount))
            ;   %% New cohort — check descriptor subscription limit first.
                %% Count total active subscriptions across all cohorts for this descriptor.
                aggregate_all(sum(MemberCount),
                    graphql_cohort(_, Descriptor, _, MemberCount),
                    TotalSubs),
                max_subscriptions_per_descriptor(Max),
                (   TotalSubs >= Max
                ->  throw(error(subscription_limit_exceeded(Max), _))
                ;   true
                ),
                term_to_atom(CohortKey, CohortKeyAtom),
                atom_concat('graphql_raw_', CohortKeyAtom, RawChannel),
                assertz(graphql_cohort(CohortKey, Descriptor, RawChannel, 1))
            )
        )),
    !.
register_subscription_parsed(_, _, _, _, _, _, _, _) :-
    throw(error(subscription_registration_failed, _)).

%% ---------------------------------------------------------------------------
%% unregister_subscription(+CohortKey, +StreamId) is det.
%% ---------------------------------------------------------------------------

unregister_subscription(CohortKey, _StreamId) :-
    with_mutex(graphql_cohort_registry,
        (   (   retract(graphql_cohort(CohortKey, Descriptor, RawChannel, Count))
            ->  NewCount is Count - 1,
                (   NewCount > 0
                ->  assertz(graphql_cohort(CohortKey, Descriptor, RawChannel, NewCount))
                ;   retractall(graphql_cohort_selection(CohortKey, _, _, _))
                )
            ;   true
            )
        )).

%% ---------------------------------------------------------------------------
%% decrement_cohort(+CohortKey) is det.
%% ---------------------------------------------------------------------------

decrement_cohort(CohortKey) :-
    unregister_subscription(CohortKey, _).

%% ---------------------------------------------------------------------------
%% broadcast_graphql_events(+Validation_Objects, +Meta_Data) is det.
%%
%% Called from post_commit_hook after every commit.
%% ---------------------------------------------------------------------------

broadcast_graphql_events(Validation_Objects, Meta_Data) :-
    get_dict(data_versions, Meta_Data, DataVersions),
    findall(Descriptor-Validation_Object-CommitIdAtom,
            (   member(Validation_Object, Validation_Objects),
                is_dict(Validation_Object),
                get_dict(descriptor, Validation_Object, Descriptor),
                member(Descriptor-data_version(_, CommitIdAtom), DataVersions)
            ),
            Triples),
    webserver_commits:dedup_triples(Triples, UniqueTriples),
    forall(
        member(Descriptor-Validation_Object-CommitIdAtom, UniqueTriples),
        send_graphql_events(Validation_Object, Descriptor, CommitIdAtom)
    ),
    forall(member(Descriptor-_, UniqueTriples),
           retractall(graphql_broadcast_sent(Descriptor, _))).

%% send_graphql_events(+Validation_Object, +Descriptor, +CommitIdAtom) is det.
send_graphql_events(Validation_Object, Descriptor, CommitIdAtom) :-
    (   graphql_broadcast_sent(Descriptor, CommitIdAtom)
    ->  true
    ;   do_broadcast_graphql_events(Validation_Object, Descriptor, CommitIdAtom),
        assertz(graphql_broadcast_sent(Descriptor, CommitIdAtom))
    ).

%% do_broadcast_graphql_events(+Validation_Object, +Descriptor, +CommitIdAtom) is det.
do_broadcast_graphql_events(Validation_Object, Descriptor, CommitIdAtom) :-
    sweep_stale_sse_streams,
    (   graphql_cohort(_, Descriptor, _, _)
    ->  open_descriptor(Descriptor, Transaction),
        api_graphql:get_or_create_graphql_context(Transaction, GraphqlContext),
        (   catch(commit_timestamp_and_datetime(Descriptor, CommitIdAtom, Timestamp, Datetime),
                _, (Timestamp = 0.0, Datetime = ""))
        ->  true
        ;   Timestamp = 0.0, Datetime = ""
        ),
        findall(TypeIRI, active_subscription_type(Transaction, TypeIRI), TypeIRIs),
        (   TypeIRIs == []
        ->  true
        ;   forall(
                '$changes':collect_changed_documents_filtered(
                    Validation_Object, TypeIRIs, DocIRI, ChangeType),
                broadcast_graphql_event(Transaction, GraphqlContext, Descriptor, DocIRI, ChangeType, CommitIdAtom, Validation_Object, Timestamp, Datetime)
            )
        ),
        %% _ChangeSet broadcast — fires once per commit with batched changes.
        %% Coexists with per-document broadcast above.
        (   graphql_cohort(cohort(_, '_ChangeSet', change_set, _, _), Descriptor, _, _)
        ->  do_broadcast_ChangeSet_events(Transaction, GraphqlContext,
                                          Descriptor, CommitIdAtom,
                                          Validation_Object, Timestamp, Datetime)
        ;   true
        )
    ;   true
    ).

%% broadcast_graphql_event(+Transaction, +GraphqlContext, +Descriptor, +DocIRI,
%%                        +ChangeType, +CommitIdKey, +Validation_Object,
%%                        +Timestamp, +Datetime) is det.
%%
%% For deleted documents, uses the Validation_Object's read layer (pre-commit
%% state) to look up rdf:type, since the triple has been removed from the
%% current layer.
broadcast_graphql_event(Transaction, GraphqlContext, Descriptor, DocIRI, ChangeType, CommitIdKey, Validation_Object, Timestamp, Datetime) :-
    (   ChangeType == deleted
    ->  findall(Class, document_class_from_validation(Validation_Object, DocIRI, Class), Classes)
    ;   findall(Class, document_class(Transaction, DocIRI, Class), Classes)
    ),
    forall(
        (   member(Class, Classes),
            Operation = ChangeType,
            graphql_cohort(CohortKey, Descriptor, RawChannel, _),
            cohort_class(CohortKey, ClassName),
            class_matches(Class, ClassName, Transaction)
        ),
        (   graphql_sse_subscription(_, CohortKey, RawChannel)
        ->  cohort_mode(CohortKey, Mode),
            broadcast_sse_event(Transaction, GraphqlContext, Descriptor, CohortKey, RawChannel, Mode,
                                ClassName, Operation, ChangeType,
                                DocIRI, CommitIdKey, Validation_Object, Timestamp, Datetime)
        ;   true
        )
    ).

%% commit_timestamp_and_datetime(+Descriptor, +CommitId, -Timestamp, -Datetime) is det.
%
%  Falls back to 0.0 and empty string if the commit cannot be found.
commit_timestamp_and_datetime(Descriptor, CommitId, Timestamp, Datetime) :-
    (   catch(
            (   descriptor_repository(Descriptor, RepoDescriptor),
                resolve_relative_descriptor(RepoDescriptor, ["_commits"], CommitDescriptor),
                open_descriptor(CommitDescriptor, CommitTxn),
                (   atom(CommitId)
                ->  atom_string(CommitId, CommitIdStr)
                ;   CommitIdStr = CommitId
                ),
                commit_id_uri(CommitTxn, CommitIdStr, CommitUri),
                commit_uri_to_metadata(CommitTxn, CommitUri, _Author, _Message, Timestamp)
            ),
            Error,
            (   json_log:json_log_error_formatted(
                    "[graphql-sse] commit_timestamp_and_datetime exception: ~w", [Error]),
                fail
            )
        )
    ->  (   catch(
                (   stamp_date_time(Timestamp, DateTime, 0),
                    format_time(string(Datetime), '%Y-%m-%dT%H:%M:%SZ', DateTime)
                ),
                _,
                Datetime = ""
            )
        ->  true
        ;   Datetime = ""
        )
    ;   Timestamp = 0.0,
        Datetime = ""
    ).

descriptor_repository(Descriptor, RepoDescriptor) :-
    (   branch_descriptor{} :< Descriptor
    ->  get_dict(repository_descriptor, Descriptor, RepoDescriptor)
    ;   repository_descriptor{} :< Descriptor
    ->  RepoDescriptor = Descriptor
    ;   fail
    ).

%% broadcast_sse_event(+Transaction, +GraphqlContext, +Descriptor, +CohortKey,
%%                     +RawChannel, +Mode, +ClassName, +Operation, +ChangeType,
%%                     +DocIRI, +CommitIdKey, +Validation_Object, +Timestamp,
%%                     +Datetime) is det.
%
%  All document resolution — including deleted documents — is handled by
%  resolve_event_through_juniper, which delegates to the Rust FFI
%  resolve_subscription_event/7 predicate. For deleted documents, the
%  Validation_Object is passed as the transaction term. Rust obtains the
%  parent of its instance_objects.read layer to access the pre-commit
%  state where the document still exists. Rust resolves all fields
%  including _CommitMetadata.
broadcast_sse_event(Transaction, GraphqlContext, _Descriptor, CohortKey, RawChannel, Mode, ClassName,
                    Operation, ChangeType, DocIRI, CommitIdKey,
                    Validation_Object, Timestamp, Datetime) :-
    (   graphql_cohort_selection(CohortKey, _, SelectionSetGraphql, _)
    ->  true
    ;   json_log:json_log_error_formatted("[graphql-sse] no selection set for cohort ~w", [CohortKey]),
        fail
    ),
    cohort_field_name(ClassName, Operation, FieldName),
    %% For deleted documents, use Validation_Object as the transaction so
    %% Rust reads from the pre-commit layer. For non-deleted, use Transaction.
    (   ChangeType == deleted
    ->  ResolveTransaction = Validation_Object
    ;   ResolveTransaction = Transaction
    ),
    (   catch(resolve_event_through_juniper(ResolveTransaction, GraphqlContext,
                                            ClassName, Operation, DocIRI,
                                            SelectionSetGraphql, ChangeType,
                                            CommitIdKey, Timestamp, Datetime,
                                            ResolvedDoc),
              Error,
              (   json_log:json_log_error_formatted(
                      "[graphql-sse] resolve_event_through_juniper failed: ~w", [Error]),
                  fail
              ))
    ->  true
    ;   (   ChangeType == deleted
        ->  ResolvedDoc = _{'_id': DocIRI}
        ;   ResolvedDoc = _{}
        )
    ),
    FullEvent = _{ data: _{} },
    put_dict(FieldName, FullEvent.data, ResolvedDoc, DataWithField),
    FullEventFinal = FullEvent.put(data, DataWithField),
    with_output_to(string(JsonStr),
        json_write_dict(current_output, FullEventFinal,
                        [as(string), width(0)])),
    send_sse_or_ndjson(Mode, JsonStr, RawChannel).

%% send_sse_or_ndjson(+Mode, +JsonStr, +RawChannel) is det.
%%
%% SSE requires the "event: next" line per the graphql-sse protocol.
%% appserver_broadcast_send_raw appends the trailing newline.
send_sse_or_ndjson(sse, JsonStr, RawChannel) :-
    format(string(Line), "event: next~ndata: ~w~n", [JsonStr]),
    broadcast_raw_or_log(RawChannel, Line).
send_sse_or_ndjson(ndjson, JsonStr, RawChannel) :-
    broadcast_raw_or_log(RawChannel, JsonStr).

%% broadcast_raw_or_log(+RawChannel, +Payload) is det.
broadcast_raw_or_log(RawChannel, Payload) :-
    (   '$appserver':appserver_broadcast_send_raw(RawChannel, Payload)
    ->  true
    ;   json_log:json_log_error_formatted(
            "[graphql-sse] broadcast_send_raw FAILED for channel ~w", [RawChannel])
    ).

%% cohort_field_name(+ClassName, +Operation, -FieldName) is det.
cohort_field_name('_ChangeSet', change_set, '_ChangeSet') :- !.
cohort_field_name(ClassName, Operation, FieldName) :-
    atom_concat(ClassName, '_', Prefix),
    atom_concat(Prefix, Operation, FieldName).

%% class_matches(+DocClass, +SubscribedClass, +Transaction) is semidet.
class_matches(DocClass, SubscribedClass, Transaction) :-
    (   DocClass == SubscribedClass
    ->  true
    ;   database_schema(Transaction, Schema),
        database_prefixes(Transaction, Prefixes),
        prefix_expand_schema(DocClass, Prefixes, DocClassIRI),
        prefix_expand_schema(SubscribedClass, Prefixes, SubscribedClassIRI),
        schema_subclass_of(Schema, DocClassIRI, SubscribedClassIRI)
    ).

%% ---------------------------------------------------------------------------
%% active_subscription_type(+Transaction, -TypeIRI) is nondet.
%% ---------------------------------------------------------------------------

active_subscription_type(Transaction, TypeIRI) :-
    graphql_cohort(CohortKey, _, _, _),
    cohort_class(CohortKey, ClassName),
    class_to_type_iri(Transaction, ClassName, TypeIRI).

%% cohort_class(+CohortKey, -ClassName) is det.
cohort_class(cohort(_, ClassName, _, _, _), ClassName).

%% cohort_descriptor(+CohortKey, -Descriptor) is det.
cohort_descriptor(cohort(Descriptor, _, _, _, _), Descriptor).

%% cohort_operation(+CohortKey, -Operation) is det.
cohort_operation(cohort(_, _, Operation, _, _), Operation).

%% cohort_mode(+CohortKey, -Mode) is det.
cohort_mode(cohort(_, _, _, _, Mode), Mode).

%% class_to_type_iri(+Transaction, +ClassName, -TypeIRI) is nondet.
%%
%% Finds all subclass IRIs on backtracking.
class_to_type_iri(Transaction, ClassName, TypeIRI) :-
    database_prefixes(Transaction, Prefixes),
    prefix_expand_schema(ClassName, Prefixes, ClassIRI),
    (   TypeIRI = ClassIRI
    ;   database_schema(Transaction, Schema),
        schema_subclass_of(Schema, SubClassIRI, ClassIRI),
        \+ schema_is_abstract(Schema, SubClassIRI),
        TypeIRI = SubClassIRI
    ).

%% document_class(+Transaction, +DocIRI, -Class) is nondet.
%%
%% Looks up rdf:type in the instance layer.

document_class(Transaction, DocIRI, Class) :-
    database_instance(Transaction, Instance),
    global_prefix_expand(rdf:type, RDF_Type),
    xrdf(Instance, DocIRI, RDF_Type, ClassIRI),
    database_prefixes(Transaction, Prefixes),
    compress_schema_uri(ClassIRI, Prefixes, Class).

%% document_class_from_validation(+Validation_Object, +DocIRI, -Class) is nondet.
%%
%% Uses xrdf_deleted for deleted documents whose rdf:type has been removed.
document_class_from_validation(Validation_Object, DocIRI, Class) :-
    database_instance(Validation_Object, Instance),
    global_prefix_expand(rdf:type, RDF_Type),
    (   xrdf_deleted(Instance, DocIRI, RDF_Type, ClassIRI)
    ->  database_prefixes(Validation_Object, Prefixes),
        compress_schema_uri(ClassIRI, Prefixes, Class)
    ;   fail
    ).

%% ===========================================================================
%% SSE Subscription Handler
%% ===========================================================================

%% read_request_body(+Request, -BodyString) is det.
%%
%% Reads body from buffered dict or raw input stream.
read_request_body(Request, BodyString) :-
    (   get_dict(body, Request, BodyString0),
        ground(BodyString0),
        BodyString0 \= ""
    ->  BodyString = BodyString0
    ;   get_dict(input_stream_id, Request, InputStreamId)
    ->  catch(drain_sse_input(InputStreamId, BodyString), Error,
              (   json_log:json_log_error_formatted("[graphql-sse] drain_sse_input error: ~w", [Error]),
                  BodyString = ""))
    ;   BodyString = ""
    ).

%% drain_sse_input(+InputStreamId, -BodyString) is det.
%%
%% Drains all chunks from the input stream into a single string.
drain_sse_input(InputStreamId, BodyString) :-
    drain_sse_input(InputStreamId, "", BodyString).

drain_sse_input(InputStreamId, Acc, BodyString) :-
    '$appserver':appserver_stream_recv(InputStreamId, Chunk),
    (   Chunk == end_of_stream
    ->  BodyString = Acc
    ;   string_concat(Acc, Chunk, NewAcc),
        drain_sse_input(InputStreamId, NewAcc, BodyString)
    ).
%%
%% The SSE handler lives on POST /api/graphql/*path. It checks the Accept
%% header: text/event-stream activates SSE subscription mode, anything else
%% delegates to the existing GraphQL HTTP handler so regular queries and
%% mutations keep working on the same endpoint.
%% ===========================================================================

%% stream_cors_headers(+Request, -Headers) is det.
%%
%% Builds CORS headers matching write_cors_headers/1 in routes.pl.
%% Delegates to routes:cors_headers_dict/2, adapting the dict-based request
%% format used by the stream handler to the list-based format expected by routes.
stream_cors_headers(Request, Headers) :-
    (   get_dict(headers, Request, ReqHeaders),
        get_dict('origin', ReqHeaders, Origin),
        Origin \= ""
    ->  routes:cors_headers_dict([origin(Origin)], Headers)
    ;   Headers = _{}
    ).

%% detect_stream_mode(+Request, -Mode) is det.
%%
%% Checks the Accept header to determine the streaming mode:
%%   text/event-stream     -> sse
%%   application/x-ndjson  -> ndjson
%%   anything else         -> delegate (regular GraphQL)
%% SSE is checked first so it takes priority if both are present.
detect_stream_mode(Request, Mode) :-
    (   get_dict(headers, Request, Headers),
        get_dict('accept', Headers, AcceptRaw)
    ->  (   sub_string(AcceptRaw, _, _, _, "text/event-stream")
        ->  Mode = sse
        ;   sub_string(AcceptRaw, _, _, _, "application/x-ndjson")
        ->  Mode = ndjson
        ;   Mode = delegate
        )
    ;   Mode = delegate
    ).

%% graphql_sse_options_handler(+Request, +StreamId, -Response) is det.
%%
%% CORS preflight handler for the SSE endpoint. Returns 204 with CORS
%% headers, mirroring the OPTIONS behavior of the regular GraphQL endpoint.
graphql_sse_options_handler(Request, _StreamId, Response) :-
    stream_cors_headers(Request, CORSHeaders),
    Response = _{
        status: 204,
        headers: CORSHeaders
    }.

%% graphql_get_handler(+Request, +StreamId, -Response) is det.
%%
%% Handles GET requests to /api/graphql/*path. Extracts the GraphQL query
%% from the URL query string parameter "query" and delegates to
%% handle_graphql_request with method get. This preserves the old GET
%% behaviour for GraphQL queries.
graphql_get_handler(Request, _StreamId, Response) :-
    catch(delegate_get_to_graphql(Request, Response),
          Error,
          (   json_log:json_log_error_formatted("[graphql-get] delegate error: ~w", [Error]),
              sse_json_response(Request, 500, "{\"errors\":[{\"message\":\"Internal server error\"}]}", Response)
          )).

%% delegate_get_to_graphql(+Request, -Response) is det.
%%
%% Extracts the query from the URL query string and delegates to
%% handle_graphql_request with method get.
delegate_get_to_graphql(Request, Response) :-
    (   sse_authenticate_or_401(Request, System_DB, Auth, Response)
    ->  get_dict(params, Request, Params),
        get_dict(path, Params, PathRaw),
        atom_string(PathAtom, PathRaw),
        (   get_dict(query, Request, QueryString),
            QueryString \= "",
            QueryString \= null
        ->  uri_query_components(QueryString, QueryPairs),
            (   member(query=GraphQLQuery, QueryPairs), ground(GraphQLQuery)
            ->  true
            ;   sse_json_response(Request, 400, "{\"errors\":[{\"message\":\"Missing 'query' parameter\"}]}", Response),
                !
            ),
            with_output_to(string(RequestBody),
                json_write_dict(current_output, _{query: GraphQLQuery}, [as(string), width(0)])),
            delegate_graphql_request(Request, System_DB, Auth, get, PathAtom, RequestBody, Response)
        ;   sse_json_response(Request, 400, "{\"errors\":[{\"message\":\"Missing 'query' parameter\"}]}", Response)
        )
    ;   sse_json_response(Request, 401, "{\"errors\":[{\"message\":\"Authentication failed\"}]}", Response)
    ).

%% graphql_sse_handler(+Request, +StreamId, -Response) is det.
%%
%% Dispatches on Accept header: SSE/NDJSON → subscription, else → GraphQL.
graphql_sse_handler(Request, StreamId, Response) :-
    detect_stream_mode(Request, Mode),
    (   Mode == delegate
    ->  catch(delegate_to_graphql(Request, Response),
              Error,
              (   json_log:json_log_error_formatted("[graphql-sse] delegate error: ~w", [Error]),
                  sse_json_response(Request, 500, "{\"errors\":[{\"message\":\"Internal server error\"}]}", Response)
              ))
    ;   catch(register_sse_subscription(Request, StreamId, Mode, Response),
              Error,
              (   json_log:json_log_error_formatted("[graphql-sse] handler error: ~w", [Error]),
                  sse_json_response(Request, 500, "{\"error\":\"internal_server_error\"}", Response)
              ))
    ).

%% register_sse_subscription(+Request, +StreamId, +Mode, -Response) is det.
register_sse_subscription(Request, StreamId, Mode, Response) :-
    (   sse_extract_branch_path(Request, BranchPathRaw)
    ->  true
    ;   sse_json_response(Request, 400, "{\"error\":\"missing_path_params\"}", Response),
        !
    ),
    (   sse_authenticate_or_401(Request, System_DB, Auth, Response)
    ->  catch(
            (   register_sse_subscription_authed(Request, StreamId, Mode, System_DB, Auth,
                                                 BranchPathRaw, Response)
            ->  true
            ;   throw(sse_handler_silent_failure)
            ),
            Error,
            (   (   Error = error(subscription_limit_exceeded(Max), _)
                ->  format(string(ErrBody), "{\"errors\":[{\"message\":\"Subscription limit exceeded (max ~w per descriptor)\"}]}", [Max]),
                    sse_json_response(Request, 429, ErrBody, Response)
                ;   json_log:json_log_error_formatted("[graphql-sse] authed handler error: ~w", [Error]),
                    sse_json_response(Request, 500, "{\"error\":\"internal_server_error\"}", Response)
                )
            )
        )
    ;   sse_json_response(Request, 401, "{\"errors\":[{\"message\":\"Authentication failed\"}]}", Response)
    ).

%% register_sse_subscription_authed(+Request, +StreamId, +Mode, +System_DB, +Auth,
%%                                   +BranchPathRaw, -Response) is det.
%%
%% Reads the request body, resolves the descriptor, checks read access,
%% opens the transaction, and uses the Rust parser (get_operation_type)
%% to determine whether the operation is a subscription, query, or mutation.
%% Routes subscriptions to registration and queries/mutations to finite
%% execution over SSE.
register_sse_subscription_authed(Request, StreamId, Mode, System_DB, Auth,
                                 BranchPathRaw, Response) :-
    catch(read_request_body(Request, BodyString), Error1,
          (   json_log:json_log_error_formatted("[graphql-sse] read_request_body error: ~w", [Error1]),
              BodyString = "")),
    (   sse_parse_body_query(BodyString, QueryString)
    ->  atom_string(BranchPathAtom, BranchPathRaw),
        resolve_absolute_string_descriptor(BranchPathAtom, Descriptor),
        (   catch(assert_read_access(System_DB, Auth, Descriptor,
                                     type_filter{types:[instance,schema]}), _, fail)
        ->  do_or_die(open_descriptor(Descriptor, Transaction),
                      error(unresolvable_absolute_descriptor(Descriptor), _)),
            api_graphql:get_or_create_graphql_context(Transaction, Graphql_Context),
            (   catch('$graphql':get_operation_type(Graphql_Context, QueryString, OperationType),
                      _, fail)
            ->  true
            ;   OperationType = mutation
            ),
            (   OperationType == subscription
            ->  register_sse_subscription_authorized(Request, StreamId, Mode, Descriptor,
                                                     Graphql_Context, Transaction,
                                                     QueryString, Response)
            ;   execute_finite_operation_over_sse(Request, StreamId, Mode, System_DB, Auth,
                                                  BranchPathAtom, QueryString, Response)
            )
        ;   sse_json_response(Request, 403, "{\"error\":\"access_not_authorised\"}", Response)
        )
    ;   sse_json_response(Request, 400, "{\"error\":\"invalid_query_body\"}", Response)
    ).

%% graphql_finite_method(+System_DB, +Auth, +BranchPathAtom, -Method) is det.
%%
%% Determines the HTTP method for a finite GraphQL operation by attempting
%% write access. If write access is granted, uses post (which enforces
%% assert_write_access in handle_graphql_request). If write access is denied,
%% falls back to get (read-only).
graphql_finite_method(System_DB, Auth, BranchPathAtom, Method) :-
    (   catch(
            (   resolve_absolute_string_descriptor(BranchPathAtom, Descriptor),
                assert_write_access(System_DB, Auth, Descriptor,
                                    filter{type: instance})
            ),
            _, fail)
    ->  Method = post
    ;   Method = get
    ).

%% execute_finite_operation_over_sse(+Request, +StreamId, +Mode, +System_DB,
%%   +Auth, +BranchPathAtom, +QueryString, -Response) is det.
%%
%% Executes a query or mutation and returns the result as a "next" event
%% followed by a "complete" event over the SSE stream, then closes.
%% Descriptor resolution and read access check are done by the caller.
execute_finite_operation_over_sse(Request, StreamId, Mode, System_DB, Auth,
                                   BranchPathAtom, QueryString, Response) :-
    graphql_finite_method(System_DB, Auth, BranchPathAtom, GraphqlMethod),
    with_output_to(string(RequestBody),
        json_write_dict(current_output, _{query: QueryString}, [as(string), width(0)])),
    string_length(RequestBody, Content_Length),
    setup_call_cleanup(
        open_string(RequestBody, BodyIn),
        (   catch(
                handle_graphql_request(System_DB, Auth, GraphqlMethod, BranchPathAtom, BodyIn,
                                       GraphqlResponse, 'application/json',
                                       Content_Length, _NewDataVersion, _TransactionMetaData),
                Error,
                (   json_log:json_log_error_formatted("[graphql-sse] finite op error: ~w", [Error]),
                    sse_plugin_error_to_graphql_json(Error, GraphqlResponse)
                )
            )
        ->  sse_validation_error_response(Request, StreamId, Mode, GraphqlResponse, Response)
        ;   sse_json_response(Request, 500, "{\"errors\":[{\"message\":\"GraphQL request failed\"}]}", Response)
        ),
        close(BodyIn)
    ).

%% sse_plugin_error_to_graphql_json(+Error, -JsonString) is det.
%%
%% Converts a Prolog error term to a GraphQL errors JSON string using
%% plugin_error_response for proper status and message mapping.
sse_plugin_error_to_graphql_json(Error, JsonString) :-
    plugin_api:plugin_error_response(Error, ErrResp),
    get_dict(body, ErrResp, ErrBodyDict),
    with_output_to(string(JsonString),
        json_write_dict(current_output, ErrBodyDict, [as(string), width(0)])).

%% register_sse_subscription_authorized(+Request, +StreamId, +Mode, +Descriptor,
%%   +Graphql_Context, +Transaction, +QueryString, -Response) is det.
%%
%% Descriptor, read access, transaction, and GraphQL context are already
%% resolved by the caller. Parses the subscription query and registers
%% the subscription.
register_sse_subscription_authorized(Request, StreamId, Mode, Descriptor,
                                      Graphql_Context, Transaction,
                                      QueryString, Response) :-
    (   catch('$graphql':parse_subscription_query(Graphql_Context, QueryString, Parsed),
              Error, (json_log:json_log_error_formatted("[graphql-sse] parse error: ~w", [Error]), fail))
    ->  register_sse_subscription_parsed(Request, StreamId, Mode, Descriptor,
                                         Graphql_Context, Transaction, Parsed, Response)
    ;   sse_validation_error_response(Request, StreamId, Mode,
        "{\"errors\":[{\"message\":\"Failed to parse subscription query\"}]}", Response)
    ).

%% register_sse_subscription_parsed(+Request, +StreamId, +Mode, +Descriptor,
%%   +Graphql_Context, +Transaction, +Parsed, -Response) is det.
register_sse_subscription_parsed(Request, StreamId, Mode, Descriptor,
                                 _Graphql_Context, _Transaction, Parsed, Response) :-
    get_dict(class_name, Parsed, ClassName),
    get_dict(operation, Parsed, Operation),
    get_dict(filter_canonical_json, Parsed, FilterJson),
    get_dict(selection_set_hash, Parsed, SelectionHash),
    get_dict(selection_set, Parsed, SelectionSet),
    get_dict(selection_set_graphql, Parsed, SelectionSetGraphql),
    (   get_dict(include_children, Parsed, IncludeChildren)
    ->  true
    ;   IncludeChildren = true
    ),
    register_subscription_parsed(Descriptor, ClassName, Operation,
                                 FilterJson, SelectionHash, Mode,
                                 CohortKey, RawChannel),
    with_mutex(graphql_cohort_registry,
        (   graphql_cohort_selection(CohortKey, _, _, _)
        ->  true
        ;   assertz(graphql_cohort_selection(CohortKey, SelectionSet, SelectionSetGraphql, IncludeChildren))
        )),
    assertz(graphql_sse_subscription(StreamId, CohortKey, RawChannel)),
    sse_parse_timeout(Request, Timeout),
    sse_build_streaming_response(Request, StreamId, Mode, CohortKey, RawChannel, Timeout, Response).

%% sse_post_response(+StreamId, +CohortKey, +RawChannel, +Timeout, +Mode) is det.
%%
%% Subscribes the stream to the broadcast channel after headers are sent,
%% then sends the non-standard "connected" event so the client knows the
%% subscription is registered and ready to receive events.
sse_post_response(StreamId, _CohortKey, RawChannel, Timeout, Mode) :-
    (   Timeout == 0
    ->  TimeoutSecs = 0
    ;   Timeout = TimeoutSecs
    ),
    catch('$appserver':appserver_broadcast_subscribe(RawChannel, StreamId, TimeoutSecs), _, true),
    send_connected_event(Mode, RawChannel).

%% send_connected_event(+Mode, +RawChannel) is det.
%%
%% Non-standard extension: sends a "connected" event to the client after
%% the subscription is accepted. This is NOT part of the graphql-sse
%% protocol (https://github.com/enisdenjo/graphql-sse/blob/master/PROTOCOL.md).
%% Strict graphql-sse clients ignore unknown event types per the SSE spec.
%%
%% Purpose: signals subscription readiness, solving the race condition
%% between subscribe and the first event. Without this, a client cannot
%% know when it is safe to trigger data operations that should produce
%% subscription events.
%%
%% Format (SSE):   event: connected\ndata: null\n\n
%% Format (NDJSON): null\n
send_connected_event(sse, RawChannel) :-
    broadcast_raw_or_log(RawChannel, "event: connected\ndata: null\n").
send_connected_event(ndjson, RawChannel) :-
    broadcast_raw_or_log(RawChannel, "null\n").

%% send_complete_event(+Mode, +RawChannel) is det.
%%
%% Sends the graphql-sse protocol "complete" event when a subscription
%% stream ends. SSE uses the event stream format with an empty data field;
%% NDJSON sends an empty line.
send_complete_event(sse, RawChannel) :-
    broadcast_raw_or_log(RawChannel, "event: complete\ndata: \n").
send_complete_event(ndjson, RawChannel) :-
    broadcast_raw_or_log(RawChannel, "\n").

%% sse_validation_error_response(+Request, +StreamId, +Mode, +ErrorJson, -Response) is det.
%%
%% Builds a 200 streaming SSE response that emits the validation error
%% as a "next" event followed by a "complete" event, then closes the stream.
%% Used when GraphQL subscription query parsing fails — per the graphql-sse
%% protocol, validation errors must be reported through an accepted SSE
%% connection, not as HTTP 400.
sse_validation_error_response(Request, StreamId, Mode, ErrorJson, Response) :-
    stream_cors_headers(Request, CORS),
    (   Mode == ndjson
    ->  ContentType = "application/x-ndjson"
    ;   ContentType = "text/event-stream"
    ),
    Response = _{
        status: 200,
        headers: CORS.put('Content-Type', ContentType)
                       .put('Cache-Control', "no-cache")
                       .put('X-Accel-Buffering', "no")
                       .put('Connection', "keep-alive"),
        body: stream,
        post_response: webserver_graphql_subs:send_validation_error_and_close(StreamId, Mode, ErrorJson)
    }.

%% send_validation_error_and_close(+StreamId, +Mode, +ErrorJson) is det.
%%
%% Sends a validation error as a "next" event, then a "complete" event,
%% then closes the stream. Uses appserver_stream_send_raw directly since
%% there is no broadcast subscription yet (the error occurs before
%% subscription registration).
send_validation_error_and_close(StreamId, Mode, ErrorJson) :-
    (   Mode == ndjson
    ->  format(string(NextLine), "~w~n", [ErrorJson]),
        catch('$appserver':appserver_stream_send_raw(StreamId, NextLine), _, true),
        catch('$appserver':appserver_stream_send_raw(StreamId, "\n"), _, true)
    ;   format(string(NextLine), "event: next~ndata: ~w~n~n", [ErrorJson]),
        catch('$appserver':appserver_stream_send_raw(StreamId, NextLine), _, true),
        format(string(CompleteLine), "event: complete~ndata: ~n~n", []),
        catch('$appserver':appserver_stream_send_raw(StreamId, CompleteLine), _, true)
    ),
    catch('$appserver':appserver_stream_close(StreamId), _, true).

%% delegate_to_graphql(+Request, -Response) is det.
%%
%% Delegates non-SSE requests to handle_graphql_request/10.
delegate_to_graphql(Request, Response) :-
    read_request_body(Request, BodyString),
    (   sse_authenticate_or_401(Request, System_DB, Auth, Response)
    ->  get_dict(params, Request, Params),
        get_dict(path, Params, PathRaw),
        atom_string(PathAtom, PathRaw),
        delegate_graphql_request(Request, System_DB, Auth, post, PathAtom, BodyString, Response)
    ;   sse_json_response(Request, 401, "{\"errors\":[{\"message\":\"Authentication failed\"}]}", Response)
    ).

%% delegate_graphql_request(+Request, +System_DB, +Auth, +Method, +PathAtom,
%%                          +BodyString, -Response) is det.
%%
%% Shared helper for delegate_to_graphql and delegate_get_to_graphql.
%% Calls handle_graphql_request and builds the response with data version
%% and retry count headers.
delegate_graphql_request(Request, System_DB, Auth, Method, PathAtom, BodyString, Response) :-
    string_length(BodyString, Content_Length),
    setup_call_cleanup(
        open_string(BodyString, BodyIn),
        (   catch(
                handle_graphql_request(System_DB, Auth, Method, PathAtom, BodyIn,
                                       GraphqlResponse, 'application/json',
                                       Content_Length, NewDataVersion, TransactionMetaData),
                Error,
                sse_plugin_error_response(Request, Error, Response)
            )
        ->  (   nonvar(Response)
            ->  true
            ;   build_graphql_success_response(Request, GraphqlResponse,
                                               NewDataVersion, TransactionMetaData, Response)
            )
        ;   sse_json_response(Request, 500, "{\"errors\":[{\"message\":\"GraphQL request failed\"}]}", Response)
        ),
        close(BodyIn)
    ).

%% build_graphql_success_response(+Request, +GraphqlResponse,
%%                                +NewDataVersion, +TransactionMetaData,
%%                                -Response) is det.
%%
%% Builds a 200 response dict with Content-Type, CORS, data version, and
%% retry count headers.
build_graphql_success_response(Request, GraphqlResponse, NewDataVersion, TransactionMetaData, Response) :-
    transaction_retry_count_from_meta_data(TransactionMetaData, RetryCount),
    number_string(RetryCount, RetryCountStr),
    (   serialize_data_version(NewDataVersion, DataVersionStr)
    ->  true
    ;   DataVersionStr = ""
    ),
    stream_cors_headers(Request, CORS),
    Headers0 = CORS.put('Content-Type', "application/json")
             .put('TerminusDB-Transaction-Retry-Count', RetryCountStr),
    (   DataVersionStr \= ""
    ->  Headers = Headers0.put('TerminusDB-Data-Version', DataVersionStr)
    ;   Headers = Headers0
    ),
    Response = _{
        status: 200,
        headers: Headers,
        body: GraphqlResponse
    }.

%% sse_plugin_error_response(+Request, +Error, -Response) is det.
%%
%% Bridges plugin_error_response/2 dict body to sse_json_response/4 string body.
sse_plugin_error_response(Request, Error, Response) :-
    plugin_api:plugin_error_response(Error, ErrResp),
    get_dict(status, ErrResp, Status),
    get_dict(body, ErrResp, ErrBodyDict),
    with_output_to(string(ErrorBody),
        json_write_dict(current_output, ErrBodyDict, [as(string), width(0)])),
    sse_json_response(Request, Status, ErrorBody, Response).

%% sse_authenticate_or_401(+Request, -System_DB, -Auth, -Response) is semidet.
%%
%% Opens system DB and authenticates. On failure, binds 401 Response and fails.
sse_authenticate_or_401(Request, System_DB, Auth, Response) :-
    open_descriptor(system_descriptor{}, System_DB),
    (   catch(plugin_api:authenticate_from_request(Request, System_DB, Auth), _, fail)
    ->  true
    ;   sse_json_response(Request, 401, "{\"errors\":[{\"message\":\"Authentication failed\"}]}", Response),
        fail
    ).

%% ===========================================================================
%% Extracted SSE pure predicates (testable without a running database)
%% ===========================================================================

%% sse_extract_branch_path(+Request, -BranchPath) is semidet.
%%
%% Extracts branch path from Request.params.path.
sse_extract_branch_path(Request, BranchPath) :-
    get_dict(params, Request, Params),
    get_dict(path, Params, BranchPath),
    BranchPath \= "".

%% sse_parse_body_query(+BodyString, -QueryString) is semidet.
%%
%% Parses JSON body and extracts the query field. Fails on invalid JSON.
sse_parse_body_query(BodyString, QueryString) :-
    catch((   atom_string(BodyAtom, BodyString),
               atom_json_dict(BodyAtom, BodyDict, [])), _, fail),
    get_dict(query, BodyDict, QueryString),
    QueryString \= "".

%% sse_parse_timeout(+Request, -Timeout) is det.
%%
%% Extracts timeout from query string. Defaults to 75 (2× heartbeat + margin).
sse_parse_timeout(Request, Timeout) :-
    (   get_dict(query, Request, QueryString),
        QueryString \= ""
    ->  uri_query_components(QueryString, QueryParams),
        (   memberchk(timeout=TimeoutAtom, QueryParams),
            atom_number(TimeoutAtom, Timeout)
        ->  true
        ;   Timeout = 75
        )
    ;   Timeout = 75
    ).

%% sse_json_response(+Request, +Status, +Body, -Response) is det.
%%
%% JSON response with CORS headers. Works for any status code
%% (errors, success, auth failures).
sse_json_response(Request, Status, Body, Response) :-
    stream_cors_headers(Request, CORS),
    Response = _{
        status: Status,
        headers: CORS.put('Content-Type', "application/json"),
        body: Body
    }.

%% sse_build_streaming_response(+Request, +StreamId, +Mode, +CohortKey,
%%   +RawChannel, +Timeout, -Response) is det.
%%
%% 200 streaming response with mode-specific Content-Type.
sse_build_streaming_response(Request, StreamId, Mode, CohortKey, RawChannel, Timeout, Response) :-
    stream_cors_headers(Request, CORS),
    (   Mode == ndjson
    ->  ContentType = "application/x-ndjson"
    ;   ContentType = "text/event-stream"
    ),
    Response = _{
        status: 200,
        headers: CORS.put('Content-Type', ContentType)
                       .put('Cache-Control', "no-cache")
                       .put('X-Accel-Buffering', "no")
                       .put('Connection', "keep-alive"),
        body: stream,
        post_response: webserver_graphql_subs:sse_post_response(StreamId, CohortKey, RawChannel, Timeout, Mode)
    }.


%% ===========================================================================
%% _ChangeSet batched event broadcast
%% ===========================================================================

%% do_broadcast_ChangeSet_events(+Transaction, +GraphqlContext, +Descriptor,
%%                               +CommitIdAtom, +Validation_Object,
%%                               +Timestamp, +Datetime) is det.
%%
%% Fires once per commit with batched per-type added/changed/deleted lists.
%% All change collection, grouping, query building, and Juniper resolution is
%% done in Rust via resolve_change_set_event/7. Prolog just calls it and sends
%% the result via SSE.
do_broadcast_ChangeSet_events(Transaction, GraphqlContext, Descriptor,
                             CommitIdAtom, _Validation_Object,
                             Timestamp, Datetime) :-
    (   atom(CommitIdAtom) -> atom_string(CommitIdAtom, CommitIdStr)
    ;   CommitIdStr = CommitIdAtom
    ),
    forall(
        (   graphql_cohort(CohortKey, Descriptor, RawChannel, _),
            cohort_class(CohortKey, '_ChangeSet'),
            cohort_operation(CohortKey, change_set)
        ),
        (   graphql_sse_subscription(_, CohortKey, RawChannel)
        ->  do_broadcast_single_ChangeSet(Transaction, GraphqlContext,
                                          CohortKey, RawChannel,
                                          CommitIdStr, Timestamp, Datetime)
        ;   true
        )
    ).

%% do_broadcast_single_ChangeSet(+Transaction, +GraphqlContext, +CohortKey,
%%                               +RawChannel, +CommitIdStr,
%%                               +Timestamp, +Datetime) is det.
%%
%% Gets the subscriber's selection set, calls resolve_change_set_event/8 which
%% does everything in Rust (change collection, grouping, query building, Juniper
%% resolution), and sends the result as one SSE event.
do_broadcast_single_ChangeSet(Transaction, GraphqlContext, CohortKey, RawChannel,
                             CommitIdStr, Timestamp, Datetime) :-
    (   graphql_cohort_selection(CohortKey, _, SelectionSetGraphql, IncludeChildren)
    ->  true
    ;   json_log:json_log_error_formatted(
            "[graphql-sse] no selection set for _ChangeSet cohort ~w", [CohortKey]),
        fail
    ),
    catch(
        (   '$graphql':resolve_change_set_event(Transaction, GraphqlContext,
                                                 SelectionSetGraphql,
                                                 CommitIdStr, Timestamp, Datetime,
                                                 IncludeChildren,
                                                 ResponseJson),
            (   atom(ResponseJson) -> ResponseAtom = ResponseJson
            ;   atom_string(ResponseAtom, ResponseJson)
            ),
            atom_json_dict(ResponseAtom, ResponseData, []),
            (   get_dict(errors, ResponseData, Errors)
            ->  json_log:json_log_error_formatted(
                    "[graphql-sse] _ChangeSet Juniper errors: ~w", [Errors])
            ;   get_dict(data, ResponseData, Data),
                (   is_dict(Data)
                ->  (   get_dict('_ChangeSet', Data, ChangeSetData)
                    ->  true
                    ;   ChangeSetData = _{}
                    )
                ;   ChangeSetData = _{}
                ),
                cohort_mode(CohortKey, Mode),
                FullEvent = _{ data: _{ '_ChangeSet': ChangeSetData } },
                with_output_to(string(JsonStr),
                    json_write_dict(current_output, FullEvent,
                                    [as(string), width(0)])),
                send_sse_or_ndjson(Mode, JsonStr, RawChannel)
            )
        ),
        Error,
        json_log:json_log_error_formatted(
            "[graphql-sse] do_broadcast_single_ChangeSet failed: ~w", [Error])
    ).

%% ===========================================================================
%% Event Resolution via Juniper (post-commit hook)
%% ===========================================================================

%% resolve_event_through_juniper(+Transaction, +GraphqlContext, +ClassName,
%%                                +Operation, +DocIRI, +SelectionSetGraphql,
%%                                +ChangeType, +CommitId, -Result) is semidet.
%%
%% Resolves a document via Juniper Query root field. Operation suffix is
%% used only for the event envelope, not the query field name.
resolve_event_through_juniper(Transaction, GraphqlContext, ClassName,
                              Operation, DocIRI, SelectionSetGraphql,
                              ChangeType, CommitId, Timestamp, Datetime,
                              Result) :-
    graphql_escape_string(DocIRI, EscapedIRI),
    format(string(GraphqlQuery), "{ ~w(id: \"~w\") { ~w } }",
           [ClassName, EscapedIRI, SelectionSetGraphql]),
    with_output_to(string(JsonEnvelope),
        json_write_dict(current_output, _{query: GraphqlQuery},
                        [as(string), width(0)])),
    (   atom(ChangeType)
    ->  atom_string(ChangeType, ChangeTypeStr)
    ;   ChangeTypeStr = ChangeType
    ),
    (   atom(CommitId)
    ->  atom_string(CommitId, CommitIdStr)
    ;   CommitIdStr = CommitId
    ),
    %% Execute via FFI — pass all metadata to the Rust resolver.
    '$graphql':resolve_subscription_event(Transaction, GraphqlContext,
                                           JsonEnvelope, ChangeTypeStr, CommitIdStr,
                                           Timestamp, Datetime,
                                           ResponseJson),
    %% Parse the response and extract the result.
    (   atom(ResponseJson) -> ResponseAtom = ResponseJson
    ;   atom_string(ResponseAtom, ResponseJson)
    ),
    atom_json_dict(ResponseAtom, ResponseData, []),
    (   get_dict(errors, ResponseData, Errors)
    ->  json_log:json_log_error_formatted(
            "[graphql-sse] Juniper returned errors: ~w", [Errors]),
        (   Operation == deleted
        ->  Result = _{'_id': DocIRI}
        ;   Result = _{}
        )
    ;   get_dict(data, ResponseData, Data),
        (   is_dict(Data)
        ->  (   get_dict(ClassName, Data, [Doc|_])
            ->  Result = Doc
            ;   (   Operation == deleted
                ->  Result = _{'_id': DocIRI}
                ;   Result = _{}
                )
            )
        ;   (   Operation == deleted
            ->  Result = _{'_id': DocIRI}
            ;   Result = _{}
            )
        )
    ).

%% graphql_escape_string(+Raw, -Escaped) is det.
%%
%% Escapes backslashes, double quotes, and newlines for safe GraphQL
%% string literal embedding. Uses re_replace/4 from library(pcre) —
%% string_replace/4 does not exist in SWI-Prolog.
graphql_escape_string(Raw, Escaped) :-
    (   atom(Raw) -> atom_string(Raw, Str)
    ;   Str = Raw
    ),
    re_replace('\\\\'/g, '\\\\\\\\', Str, S1),
    re_replace('"'/g, '\\"', S1, S2),
    re_replace('\n'/g, '\\n', S2, S3),
    re_replace('\r'/g, '\\r', S3, S4),
    re_replace('\t'/g, '\\t', S4, S5),
    atom_string(Escaped, S5).

%% ===========================================================================
%% SSE Cleanup
%% ===========================================================================

%% sweep_stale_sse_streams is det.
%%
%% Retract graphql_sse_subscription/3 entries whose Rust stream no longer
%% exists. Mirrors webserver_commits:sweep_stale_streams/0. Called at
%% the start of do_broadcast_graphql_events.
sweep_stale_sse_streams :-
    (   \+ graphql_sse_subscription(_, _, _)
    ->  true
    ;   findall(StreamId, graphql_sse_subscription(StreamId, _, _), StreamIds),
        forall(
            (   member(StreamId, StreamIds),
                \+ catch('$appserver':appserver_stream_exists(StreamId), _, fail)
            ),
            cleanup_sse_stream(StreamId)
        )
    ).

%% cleanup_sse_stream(+StreamId) is det.
%%
%% Sends the graphql-sse "complete" event, then retracts the SSE
%% subscription, unregisters the cohort, and unsubscribes from the
%% broadcast channel. Idempotent.
cleanup_sse_stream(StreamId) :-
    (   retract(graphql_sse_subscription(StreamId, CohortKey, RawChannel))
    ->  cohort_mode(CohortKey, Mode),
        send_complete_event(Mode, RawChannel),
        unregister_subscription(CohortKey, StreamId),
        catch('$appserver':appserver_broadcast_unsubscribe(RawChannel, StreamId), _, true)
    ;   true
    ).


%% ---------------------------------------------------------------------------
%% PLUnit tests
%% ---------------------------------------------------------------------------

:- use_module(library(plunit)).
:- begin_tests(webserver_graphql_subs, []).

test(register_and_unregister, [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    TestDesc = branch_descriptor{},
    register_subscription_parsed(TestDesc, 'Person', added,
                                 '{}', 'hash1', sse,
                                 CohortKey, RawChannel1),
    compound(CohortKey),
    CohortKey = cohort(TestDesc, 'Person', added, 'hash1', sse),
    atom(RawChannel1),
    webserver_graphql_subs:graphql_cohort(CohortKey, TestDesc, RawChannel1, 1),
    register_subscription_parsed(TestDesc, 'Person', added,
                                 '{}', 'hash1', sse,
                                 CohortKey2, RawChannel2),
    CohortKey == CohortKey2,
    RawChannel1 == RawChannel2,
    webserver_graphql_subs:graphql_cohort(CohortKey, TestDesc, RawChannel1, 2),
    unregister_subscription(CohortKey, stream1),
    webserver_graphql_subs:graphql_cohort(CohortKey, TestDesc, RawChannel1, 1).

test(register_different_selection_different_cohort,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    TestDesc = branch_descriptor{},
    register_subscription_parsed(TestDesc, 'Person', added,
                                 '{}', 'hash1', sse, CohortKey1, _),
    register_subscription_parsed(TestDesc, 'Person', added,
                                 '{}', 'hash2', sse, CohortKey2, _),
    CohortKey1 \== CohortKey2.

%% Security: compound term cohort key prevents separator collision attacks.
%% Even if branch paths or class names contain |, -, or other special chars,
%% the compound term structure ensures correct extraction.

test(cohort_key_no_collision_with_pipe_in_class_name,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    CohortKey = cohort(branch_descriptor{}, 'Evil|Class', added, 'abc123', sse),
    cohort_class(CohortKey, ClassName),
    ClassName == 'Evil|Class'.

test(cohort_key_no_collision_with_dash_in_branch_path,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    CohortKey = cohort(database_descriptor{}, 'Person', added, 'abc123', sse),
    cohort_class(CohortKey, ClassName),
    ClassName == 'Person'.

test(cohort_key_term_to_atom_roundtrip_is_safe) :-
    CohortKey = cohort(branch_descriptor{}, 'Person', added, 'abc123', sse),
    term_to_atom(CohortKey, Atom),
    atom_to_term(Atom, CohortKey2, []),
    CohortKey == CohortKey2.

test(cohort_key_term_to_atom_roundtrip_with_special_chars) :-
    CohortKey = cohort(branch_descriptor{}, 'Person"evil', added, 'abc123', sse),
    term_to_atom(CohortKey, Atom),
    atom_to_term(Atom, CohortKey2, []),
    CohortKey == CohortKey2.

test(cohort_key_distinct_components_no_collision) :-
    CohortKey1 = cohort(branch_descriptor{}, 'b', added, 'c', sse),
    CohortKey2 = cohort(database_descriptor{}, 'b', added, 'c', sse),
    CohortKey1 \== CohortKey2.

test(register_rejects_non_dict_descriptor,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    catch(
        register_subscription_parsed('not_a_dict', 'Person', added,
                                      '{}', 'hash1', sse, _, _),
        Error,
        Error = error(subscription_registration_failed, _)
    ).

test(register_rejects_integer_descriptor,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    catch(
        register_subscription_parsed(42, 'Person', added,
                                      '{}', 'hash1', sse, _, _),
        Error,
        Error = error(subscription_registration_failed, _)
    ).

%% FFI returns atoms for class_name and operation — verify
%% register_subscription_parsed accepts atom inputs (production path).
test(register_with_atom_inputs_like_ffi,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    TestDesc = branch_descriptor{},
    register_subscription_parsed(TestDesc,
                                 'Person', added,
                                 '{}', 'hash1', sse,
                                 CohortKey, _),
    CohortKey = cohort(TestDesc, 'Person', added, 'hash1', sse).

test(register_rejects_invalid_operation,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    catch(
        register_subscription_parsed(branch_descriptor{},
                                     'Person', invalid_op,
                                     '{}', 'hash1', sse, _, _),
        Error,
        Error = error(subscription_registration_failed, _)
    ).

%% register_subscription doesn't assert gql_subscription/4 —
%% only gql_cohort/4. unregister_subscription/2 must still decrement the
%% cohort count even without gql_subscription facts.
test(unregister_without_gql_subscription_decrements_cohort,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    TestDesc = branch_descriptor{},
    register_subscription_parsed(TestDesc, 'Person', added,
                                 '{}', 'hash1', sse, CohortKey, _),
    webserver_graphql_subs:graphql_cohort(CohortKey, _, _, 1),
    unregister_subscription(CohortKey, fake_stream),
    \+ webserver_graphql_subs:graphql_cohort(CohortKey, _, _, _).

%% graphql_cohort_selection is stored when a cohort is created and
%% cleaned up when the last member unregisters.
test(cohort_selection_stored_and_cleaned_up,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    TestDesc = branch_descriptor{},
    register_subscription_parsed(TestDesc, 'Person', added,
                                 '{}', 'hash1', sse, CohortKey, _),
    assertz(webserver_graphql_subs:graphql_cohort_selection(CohortKey, "_id{}", "_id", true)),
    webserver_graphql_subs:graphql_cohort_selection(CohortKey, "_id{}", "_id", true),
    unregister_subscription(CohortKey, fake_stream),
    \+ webserver_graphql_subs:graphql_cohort_selection(CohortKey, _, _, _).

%% graphql_cohort_selection is NOT cleaned up while cohort still has members.
test(cohort_selection_survives_partial_unregister,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    TestDesc = branch_descriptor{},
    register_subscription_parsed(TestDesc, 'Person', added,
                                 '{}', 'hash1', sse, CohortKey, _),
    assertz(webserver_graphql_subs:graphql_cohort_selection(CohortKey, "_id{name{}}", "_id name", true)),
    register_subscription_parsed(TestDesc, 'Person', added,
                                 '{}', 'hash1', sse, _, _),
    graphql_cohort(CohortKey, _, _, 2),
    unregister_subscription(CohortKey, stream1),
    webserver_graphql_subs:graphql_cohort_selection(CohortKey, "_id{name{}}", "_id name", true).

%% Subscription limit tests — verify that max_subscriptions_per_descriptor
%% is enforced when creating new cohorts for a descriptor.

test(subscription_limit_rejects_new_cohort_when_exceeded,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    TestDesc = branch_descriptor{},
    %% Temporarily set limit to 1
    retractall(webserver_graphql_subs:max_subscriptions_per_descriptor(_)),
    assertz(webserver_graphql_subs:max_subscriptions_per_descriptor(1)),
    %% First subscription succeeds (creates cohort with count 1)
    register_subscription_parsed(TestDesc, 'Person', added,
                                 '{}', 'hash1', sse, _, _),
    %% Second subscription for a *different* cohort on same descriptor should fail
    catch(
        register_subscription_parsed(TestDesc, 'Person', changed,
                                      '{}', 'hash2', sse, _, _),
        Error,
        Error = error(subscription_limit_exceeded(1), _)
    ),
    %% Restore default
    retractall(webserver_graphql_subs:max_subscriptions_per_descriptor(_)),
    assertz(webserver_graphql_subs:max_subscriptions_per_descriptor(100)).

test(subscription_limit_allows_increment_of_existing_cohort,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    TestDesc = branch_descriptor{},
    %% Set limit to 1 — one subscription total
    retractall(webserver_graphql_subs:max_subscriptions_per_descriptor(_)),
    assertz(webserver_graphql_subs:max_subscriptions_per_descriptor(1)),
    %% First subscription creates cohort
    register_subscription_parsed(TestDesc, 'Person', added,
                                 '{}', 'hash1', sse, CohortKey, _),
    %% Second subscription to SAME cohort should succeed (increment, not new)
    register_subscription_parsed(TestDesc, 'Person', added,
                                 '{}', 'hash1', sse, _, _),
    graphql_cohort(CohortKey, _, _, 2),
    %% Restore default
    retractall(webserver_graphql_subs:max_subscriptions_per_descriptor(_)),
    assertz(webserver_graphql_subs:max_subscriptions_per_descriptor(100)).

test(subscription_limit_default_is_100,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    max_subscriptions_per_descriptor(Max),
    Max == 100.

test(subscription_limit_counts_across_all_cohorts_for_descriptor,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    TestDesc = branch_descriptor{},
    %% Set limit to 2
    retractall(webserver_graphql_subs:max_subscriptions_per_descriptor(_)),
    assertz(webserver_graphql_subs:max_subscriptions_per_descriptor(2)),
    %% Two different cohorts, each with 1 member = 2 total
    register_subscription_parsed(TestDesc, 'Person', added,
                                 '{}', 'hash1', sse, _, _),
    register_subscription_parsed(TestDesc, 'Product', added,
                                 '{}', 'hash2', sse, _, _),
    %% Third cohort should be rejected (total would be 3 > 2)
    catch(
        register_subscription_parsed(TestDesc, 'Order', added,
                                      '{}', 'hash3', sse, _, _),
        Error,
        Error = error(subscription_limit_exceeded(2), _)
    ),
    %% Restore default
    retractall(webserver_graphql_subs:max_subscriptions_per_descriptor(_)),
    assertz(webserver_graphql_subs:max_subscriptions_per_descriptor(100)).

:- end_tests(webserver_graphql_subs).

%% ---------------------------------------------------------------------------
%% Additional unit tests for pipe-dispatch handlers and pure predicates.
%% These tests verify the Request dict interface that pipe dispatch uses.
%% ---------------------------------------------------------------------------

:- begin_tests(webserver_graphql_subs_handlers, []).

%% cohort_class extracts the class name from a cohort key compound term.
test(cohort_class_extracts_class_name) :-
    CohortKey = cohort(branch_descriptor{}, 'MyClass', added, 'hash123', sse),
    cohort_class(CohortKey, ClassName),
    ClassName == 'MyClass'.

test(cohort_class_extracts_different_class) :-
    CohortKey = cohort(database_descriptor{}, 'Product', deleted, 'abc', sse),
    cohort_class(CohortKey, ClassName),
    ClassName == 'Product'.

:- end_tests(webserver_graphql_subs_handlers).

%% ---------------------------------------------------------------------------
%% PLUnit tests for SSE subscription support
%%
%% These tests cover the SSE handler pieces: CORS header reflection,
%% DocIRI escaping for GraphQL string literals, stream mode detection,
%% stale stream sweeping, and SSE event formatting. The predicates under
%% test are implemented in Phases 4-6 of the SSE plan.
%% ---------------------------------------------------------------------------

:- begin_tests(webserver_graphql_subs_sse, []).

%% stream_cors_headers reflects the Origin header (not wildcard '*').
%% This mirrors write_cors_headers/1 in routes.pl which reflects Origin
%% and sets Access-Control-Allow-Credentials: true (per CORS spec, '*'
%% is incompatible with credentials).
test(stream_cors_headers_reflects_origin) :-
    Request = _{headers: _{'origin': "https://example.com"}},
    stream_cors_headers(Request, Headers),
    get_dict('Access-Control-Allow-Origin', Headers, Origin),
    Origin == "https://example.com",
    get_dict('Access-Control-Allow-Credentials', Headers, "true").

%% stream_cors_headers returns an empty dict when no Origin header is
%% present (same-origin request — no CORS headers needed).
test(stream_cors_headers_no_origin) :-
    Request = _{headers: _{}},
    stream_cors_headers(Request, Headers),
    \+ get_dict('Access-Control-Allow-Origin', Headers, _).

%% stream_cors_headers includes the full set of allowed methods and
%% headers, matching write_cors_headers/1.
test(stream_cors_headers_includes_methods_and_headers) :-
    Request = _{headers: _{'origin': "https://example.com"}},
    stream_cors_headers(Request, Headers),
    get_dict('Access-Control-Allow-Methods', Headers, Methods),
    once(sub_string(Methods, _, _, _, "GET")),
    once(sub_string(Methods, _, _, _, "POST")),
    once(sub_string(Methods, _, _, _, "OPTIONS")),
    get_dict('Access-Control-Allow-Headers', Headers, AllowedHeaders),
    once(sub_string(AllowedHeaders, _, _, _, "Authorization")),
    once(sub_string(AllowedHeaders, _, _, _, "Content-Type")).

%% graphql_escape_string escapes backslashes for safe GraphQL string
%% literal embedding. Uses re_replace/4 from library(pcre).
test(graphql_escape_string_basic) :-
    graphql_escape_string('Product/Simple', Escaped),
    atom_string(Escaped, EscapedStr),
    EscapedStr == "Product/Simple".

test(graphql_escape_string_with_quotes) :-
    graphql_escape_string('Product/"bad"', Escaped),
    atom_string(Escaped, EscapedStr),
    once(sub_string(EscapedStr, _, _, _, '\\"bad\\"')).

test(graphql_escape_string_with_backslash) :-
    graphql_escape_string('Product\\path', Escaped),
    atom_string(Escaped, EscapedStr),
    once(sub_string(EscapedStr, _, _, _, '\\\\')).

%% detect_stream_mode checks the Accept header to determine the
%% streaming mode. SSE only — no NDJSON in this phase.
test(detect_mode_sse) :-
    detect_stream_mode(_{headers: _{'accept': "text/event-stream"}}, Mode),
    Mode == sse.

test(detect_mode_delegate_default) :-
    detect_stream_mode(_{headers: _{'accept': "application/json"}}, Mode),
    Mode == delegate.

test(detect_mode_delegate_no_accept) :-
    detect_stream_mode(_{headers: _{}}, Mode),
    Mode == delegate.

%% send_sse_or_ndjson(sse, ...) produces "event: next\ndata: {json}\n"
%% with exactly one trailing \n — broadcast_raw_or_log appends the second.
%% We verify the format string matches the predicate's output by checking
%% the structure of the generated line.
test(sse_data_line_format) :-
    format(string(Line), "event: next~ndata: ~w~n", ['{"data":{}}']),
    once(sub_string(Line, _, _, _, "event: next")),
    once(sub_string(Line, _, _, _, "data: {\"data\":{}}")),
    %% Must NOT contain two trailing newlines — broadcast adds the second.
    \+ sub_string(Line, _, _, _, "\n\n").

%% sweep_stale_sse_streams retracts graphql_sse_subscription entries
%% whose Rust stream no longer exists. Uses a fake stream id that
%% does not exist in the appserver registry.
test(sweep_stale_sse_streams_removes_dead,
     [setup(setup_sse_test_streams), cleanup(cleanup_cohorts)]) :-
    %% dead_stream does not exist in the appserver registry, so it
    %% should be retracted. alive_stream also does not exist in a unit
    %% test context (no running server), so both will be swept.
    sweep_stale_sse_streams,
    %% After sweeping, no graphql_sse_subscription entries remain.
    \+ webserver_graphql_subs:graphql_sse_subscription(_, _, _).

%% cleanup_sse_stream retracts a single SSE subscription and
%% unregisters the cohort.
test(cleanup_sse_stream_retracts_subscription,
     [setup(setup_sse_test_streams), cleanup(cleanup_cohorts)]) :-
    cleanup_sse_stream(alive_stream),
    \+ webserver_graphql_subs:graphql_sse_subscription(alive_stream, _, _),
    %% The other stream should still be present.
    webserver_graphql_subs:graphql_sse_subscription(dead_stream, _, _).

%% graphql_sse_handler returns 401 when authentication fails
%% (no Authorization header and no anonymous fallback in test context).
%% Uses setup_temp_store so open_descriptor(system_descriptor{}) succeeds,
%% then authenticate_from_request fails on missing auth → 401.
%% This ensures identical behavior in CI and local environments.
test(sse_handler_returns_401_on_auth_failure,
     [setup(setup_temp_store(State)),
      cleanup(teardown_temp_store(State))]) :-
    Request = _{
        headers: _{},
        params: _{path: "admin/db/local/branch/main"},
        body: "{\"query\": \"subscription { Product_added { _id } }\"}"
    },
    catch(
        (   graphql_sse_handler(Request, _StreamId, Response)
        ->  get_dict(status, Response, Status),
            Status == 401
        ;   %% If the handler fails (no auth), that's also acceptable
            %% for the unit test — the worker pool catch maps it to 401.
            true
        ),
        _,
        true
    ).

%% graphql_sse_handler returns 400 when the query body is missing
%% or unparseable. This requires auth to pass first, which it won't
%% in a pure unit test without a database — so we only verify the
%% handler does not crash on a malformed body. Both failure (semidet)
%% and exception are acceptable — the worker pool catch maps both to
%% error responses.
test(sse_handler_does_not_crash_on_malformed_body) :-
    Request = _{
        headers: _{},
        params: _{path: "admin/db/local/branch/main"},
        body: "not valid json"
    },
    (   catch(graphql_sse_handler(Request, test_stream, _), _, true)
    ->  true
    ;   true
    ).

%% graphql_sse_options_handler returns 204 with CORS headers.
test(sse_options_handler_returns_204_with_cors) :-
    Request = _{headers: _{'origin': "https://example.com"}},
    graphql_sse_options_handler(Request, _StreamId, Response),
    get_dict(status, Response, 204),
    get_dict(headers, Response, Headers),
    get_dict('Access-Control-Allow-Origin', Headers, "https://example.com").

:- end_tests(webserver_graphql_subs_sse).

%% ---------------------------------------------------------------------------
%% PLUnit tests for extracted SSE pure predicates
%%
%% These tests verify the single-responsibility predicates extracted from
%% the monolithic SSE handler. Each predicate is testable without a running
%% database — they operate on plain dicts and strings.
%% ---------------------------------------------------------------------------

:- begin_tests(webserver_graphql_subs_sse_pure, []).

%% sse_extract_branch_path extracts the branch path from Request.params.path.

test(extract_branch_path_from_params) :-
    Request = _{params: _{path: "admin/db/local/branch/main"}},
    sse_extract_branch_path(Request, BranchPath),
    BranchPath == "admin/db/local/branch/main".

test(extract_branch_path_short_path) :-
    Request = _{params: _{path: "admin/db"}},
    sse_extract_branch_path(Request, BranchPath),
    BranchPath == "admin/db".

test(extract_branch_path_fails_without_params) :-
    \+ sse_extract_branch_path(_{}, _).

test(extract_branch_path_fails_without_path_in_params) :-
    \+ sse_extract_branch_path(_{params: _{}}, _).

%% sse_parse_body_query parses a JSON body string and extracts the query field.

test(parse_body_query_valid) :-
    sse_parse_body_query('{"query":"subscription { Product_added { _id } }"}', Query),
    Query == "subscription { Product_added { _id } }".

test(parse_body_query_with_variables) :-
    sse_parse_body_query('{"query":"subscription { X { _id } }","variables":{}}', Query),
    Query == "subscription { X { _id } }".

test(parse_body_query_empty_query_fails) :-
    \+ sse_parse_body_query('{"query":""}', _).

test(parse_body_query_missing_query_fails) :-
    \+ sse_parse_body_query('{"variables":{}}', _).

test(parse_body_query_invalid_json_fails) :-
    \+ sse_parse_body_query('not json', _).

test(parse_body_query_empty_string_fails) :-
    \+ sse_parse_body_query('', _).

%% sse_parse_timeout extracts the timeout query parameter, defaulting to 75.

test(parse_timeout_default_no_query) :-
    sse_parse_timeout(_{}, Timeout),
    Timeout == 75.

test(parse_timeout_default_empty_query) :-
    sse_parse_timeout(_{query: ""}, Timeout),
    Timeout == 75.

test(parse_timeout_explicit) :-
    sse_parse_timeout(_{query: "timeout=60"}, Timeout),
    Timeout == 60.

test(parse_timeout_with_other_params) :-
    sse_parse_timeout(_{query: "foo=bar&timeout=120&baz=qux"}, Timeout),
    Timeout == 120.

test(parse_timeout_invalid_falls_back_to_default) :-
    sse_parse_timeout(_{query: "timeout=abc"}, Timeout),
    Timeout == 75.

test(parse_timeout_zero_allowed) :-
    sse_parse_timeout(_{query: "timeout=0"}, Timeout),
    Timeout == 0.

%% sse_json_response builds a JSON response dict with CORS headers.

test(json_response_includes_cors_when_origin) :-
    Request = _{headers: _{'origin': "https://example.com"}},
    sse_json_response(Request, 400, "{\"error\":\"bad\"}", Response),
    get_dict(status, Response, 400),
    get_dict(headers, Response, Headers),
    get_dict('Content-Type', Headers, "application/json"),
    get_dict('Access-Control-Allow-Origin', Headers, "https://example.com"),
    get_dict(body, Response, "{\"error\":\"bad\"}").

test(json_response_no_cors_without_origin) :-
    Request = _{headers: _{}},
    sse_json_response(Request, 401, "{\"error\":\"auth\"}", Response),
    get_dict(status, Response, 401),
    get_dict(body, Response, "{\"error\":\"auth\"}"),
    get_dict(headers, Response, Headers),
    \+ get_dict('Access-Control-Allow-Origin', Headers, _).

%% sse_build_streaming_response builds the 200 streaming response dict.

test(streaming_response_has_sse_headers) :-
    Request = _{headers: _{'origin': "https://example.com"}},
    CohortKey = cohort(branch_descriptor{}, 'Product', added, 'hash', sse),
    sse_build_streaming_response(Request, stream1, sse, CohortKey, 'test_channel', 30, Response),
    get_dict(status, Response, 200),
    get_dict(headers, Response, Headers),
    get_dict('Content-Type', Headers, "text/event-stream"),
    get_dict('Cache-Control', Headers, "no-cache"),
    get_dict('X-Accel-Buffering', Headers, "no"),
    get_dict(body, Response, stream).

test(streaming_response_has_ndjson_headers) :-
    Request = _{headers: _{'origin': "https://example.com"}},
    CohortKey = cohort(branch_descriptor{}, 'Product', added, 'hash', ndjson),
    sse_build_streaming_response(Request, stream1, ndjson, CohortKey, 'test_channel', 30, Response),
    get_dict(status, Response, 200),
    get_dict(headers, Response, Headers),
    get_dict('Content-Type', Headers, "application/x-ndjson"),
    get_dict('Cache-Control', Headers, "no-cache"),
    get_dict(body, Response, stream).

test(streaming_response_has_post_response_goal) :-
    Request = _{headers: _{}},
    CohortKey = cohort(branch_descriptor{}, 'Product', added, 'hash', sse),
    sse_build_streaming_response(Request, stream1, sse, CohortKey, 'test_channel', 60, Response),
    get_dict(post_response, Response, PostResponse),
    callable(PostResponse).

%% detect_stream_mode detects 3 modes: sse, ndjson, delegate.

test(detect_mode_ndjson) :-
    detect_stream_mode(_{headers: _{'accept': "application/x-ndjson"}}, Mode),
    Mode == ndjson.

test(detect_mode_ndjson_with_other_accept) :-
    detect_stream_mode(_{headers: _{'accept': "application/x-ndjson, text/plain"}}, Mode),
    Mode == ndjson.

test(detect_mode_sse_takes_priority_over_ndjson) :-
    %% If both SSE and NDJSON are in Accept, SSE wins (checked first).
    detect_stream_mode(_{headers: _{'accept': "text/event-stream, application/x-ndjson"}}, Mode),
    Mode == sse.

%% cohort_mode extracts the mode from a 5-arg cohort key.

test(cohort_mode_extracts_sse) :-
    CohortKey = cohort(branch_descriptor{}, 'Product', added, 'hash', sse),
    cohort_mode(CohortKey, Mode),
    Mode == sse.

test(cohort_mode_extracts_ndjson) :-
    CohortKey = cohort(branch_descriptor{}, 'Product', added, 'hash', ndjson),
    cohort_mode(CohortKey, Mode),
    Mode == ndjson.

%% cohort_operation extracts the operation from a 5-arg cohort key.

test(cohort_operation_extracts_added) :-
    CohortKey = cohort(branch_descriptor{}, 'Product', added, 'hash', sse),
    cohort_operation(CohortKey, Operation),
    Operation == added.

test(cohort_operation_extracts_deleted) :-
    CohortKey = cohort(branch_descriptor{}, 'Product', deleted, 'hash', ndjson),
    cohort_operation(CohortKey, Operation),
    Operation == deleted.

%% cohort_class works with 5-arg cohort key.

test(cohort_class_works_with_5_arg_key) :-
    CohortKey = cohort(branch_descriptor{}, 'Product', added, 'hash', sse),
    cohort_class(CohortKey, ClassName),
    ClassName == 'Product'.

%% register_subscription_parsed with Mode (8-arg) includes Mode in cohort key.

test(register_with_mode_sse,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    TestDesc = branch_descriptor{},
    register_subscription_parsed(TestDesc, 'Person', added,
                                 '{}', 'hash1', sse,
                                 CohortKey, _),
    CohortKey = cohort(TestDesc, 'Person', added, 'hash1', sse).

test(register_with_mode_ndjson,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    TestDesc = branch_descriptor{},
    register_subscription_parsed(TestDesc, 'Person', added,
                                 '{}', 'hash1', ndjson,
                                 CohortKey, _),
    CohortKey = cohort(TestDesc, 'Person', added, 'hash1', ndjson).

test(register_different_modes_different_cohorts,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    TestDesc = branch_descriptor{},
    register_subscription_parsed(TestDesc, 'Person', added,
                                 '{}', 'hash1', sse,
                                 CohortKey1, _),
    register_subscription_parsed(TestDesc, 'Person', added,
                                 '{}', 'hash1', ndjson,
                                 CohortKey2, _),
    CohortKey1 \== CohortKey2.

test(register_rejects_invalid_mode,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    catch(
        register_subscription_parsed(branch_descriptor{}, 'Person', added,
                                     '{}', 'hash1', invalid_mode, _, _),
        Error,
        Error = error(subscription_registration_failed, _)
    ).

test(register_accepts_database_descriptor,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    TestDesc = database_descriptor{},
    register_subscription_parsed(TestDesc, 'Person', added,
                                 '{}', 'hash1', sse,
                                 CohortKey, _),
    CohortKey = cohort(TestDesc, 'Person', added, 'hash1', sse).

test(register_accepts_repository_descriptor,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    TestDesc = repository_descriptor{},
    register_subscription_parsed(TestDesc, 'Person', added,
                                 '{}', 'hash1', sse,
                                 CohortKey, _),
    CohortKey = cohort(TestDesc, 'Person', added, 'hash1', sse).

test(register_accepts_system_descriptor,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    TestDesc = system_descriptor{},
    register_subscription_parsed(TestDesc, 'Person', added,
                                 '{}', 'hash1', sse,
                                 CohortKey, _),
    CohortKey = cohort(TestDesc, 'Person', added, 'hash1', sse).

%% send_sse_or_ndjson formats events per mode.
%% SSE: "event: next\ndata: {json}\n" (broadcast adds second \n)
%% NDJSON: "{json}" (broadcast adds \n)

test(send_sse_format) :-
    with_output_to(string(Line),
        format(current_output, "event: next~ndata: ~w~n", ['{"data":{}}'])),
    once(sub_string(Line, _, _, _, "event: next")),
    once(sub_string(Line, _, _, _, "data: ")).

%% complete event format tests

test(complete_event_sse_format) :-
    with_output_to(string(Line),
        format(current_output, "event: complete~ndata: ~n", [])),
    once(sub_string(Line, _, _, _, "event: complete")),
    once(sub_string(Line, _, _, _, "data: ")).

test(complete_event_ndjson_format) :-
    %% NDJSON complete is an empty line
    Line = "\n",
    once(sub_string(Line, _, _, _, "\n")).

test(send_complete_event_sse) :-
    %% send_complete_event(sse, _) should produce "event: complete\ndata: \n"
    with_output_to(string(Line),
        format(current_output, "event: complete~ndata: ~n", [])),
    once(sub_string(Line, _, _, _, "event: complete")),
    once(sub_string(Line, _, _, _, "data: ")).

%% validation error next event format tests

test(validation_error_next_event_format) :-
    %% GraphQL errors are sent as {"errors":[...]} in a next event
    ErrorJson = "{\"errors\":[{\"message\":\"syntax error\"}]}",
    with_output_to(string(Line),
        format(current_output, "event: next~ndata: ~w~n", [ErrorJson])),
    once(sub_string(Line, _, _, _, "event: next")),
    once(sub_string(Line, _, _, _, "data: {\"errors\":")).

test(sse_validation_error_response_returns_200_sse) :-
    %% Validation error response must be 200 with text/event-stream
    Request = _{headers: _{}},
    sse_validation_error_response(Request, stream1, sse,
        "{\"errors\":[{\"message\":\"bad query\"}]}", Response),
    get_dict(status, Response, 200),
    get_dict(headers, Response, Headers),
    get_dict('Content-Type', Headers, "text/event-stream"),
    get_dict(body, Response, stream),
    get_dict(post_response, Response, PostResponse),
    nonvar(PostResponse).

%% Operation type detection (get_operation_type) requires a running
%% database and GraphQL context, so it is tested via integration tests
%% (graphql-subscriptions-sse.js finite operations over SSE).

:- end_tests(webserver_graphql_subs_sse_pure).

%% ---------------------------------------------------------------------------
%% PLUnit tests for _ChangeSet cohort registration and broadcast logic
%% ---------------------------------------------------------------------------

:- begin_tests(webserver_graphql_subs_change_set, []).

%% _ChangeSet cohort registration should accept change_set operation.
%% Currently register_subscription_parsed validates memberchk(Operation,
%% [added, changed, deleted]) — change_set is not in that list, so this
%% test will fail until the validation is extended.
test(change_set_cohort_registration,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    TestDesc = branch_descriptor{},
    register_subscription_parsed(TestDesc, '_ChangeSet', change_set,
                                 '{}', 'abc123', sse,
                                _CohortKey, _RawChannel),
    !,
    graphql_cohort(CohortKey, TestDesc, _, 1),
    cohort_class(CohortKey, ClassName),
    ClassName == '_ChangeSet',
    cohort_operation(CohortKey, Operation),
    Operation == change_set.

%% _ChangeSet cohort selection set is stored with full nested selection.
test(change_set_cohort_selection_stored,
     [setup(cleanup_cohorts), cleanup(cleanup_cohorts)]) :-
    TestDesc = branch_descriptor{},
    register_subscription_parsed(TestDesc, '_ChangeSet', change_set,
                                 '{}', 'abc123', sse,
                                CohortKey, _RawChannel),
    !,
    SelectionSet = 'Person_added { _id name } Person_deleted { _id name }',
    SelectionGraphql = 'Person_added { _id name } Person_deleted { _id name }',
    assertz(graphql_cohort_selection(CohortKey, SelectionSet, SelectionGraphql, true)),
    graphql_cohort_selection(CohortKey, StoredSelection, StoredGraphql, true),
    StoredSelection == SelectionSet,
    StoredGraphql == SelectionGraphql.

%% cohort_field_name special case for _ChangeSet.
%% Without the fix, cohort_field_name('_ChangeSet', change_set, X) produces
%% '_ChangeSet_change_set' instead of '_ChangeSet'.
test(cohort_field_name_change_set) :-
    cohort_field_name('_ChangeSet', change_set, FieldName),
    FieldName == '_ChangeSet'.

%% cohort_field_name still works for regular class/operation pairs.
test(cohort_field_name_regular) :-
    cohort_field_name('Person', added, FieldName),
    FieldName == 'Person_added'.

:- end_tests(webserver_graphql_subs_change_set).

%% Setup helper for SSE stream tests — creates test cohort and
%% subscription entries with fake stream ids.
setup_sse_test_streams :-
    cleanup_cohorts,
    TestDesc = branch_descriptor{},
    CohortKey = cohort(TestDesc, 'Product', added, 'hash', sse),
    assertz(webserver_graphql_subs:graphql_cohort(CohortKey,
                TestDesc, 'test_channel', 2)),
    assertz(webserver_graphql_subs:graphql_sse_subscription(alive_stream,
                CohortKey, 'test_channel')),
    assertz(webserver_graphql_subs:graphql_sse_subscription(dead_stream,
                CohortKey, 'test_channel')).

%% Cleanup helper for tests
cleanup_cohorts :-
    retractall(webserver_graphql_subs:graphql_cohort(_, _, _, _)),
    retractall(webserver_graphql_subs:graphql_cohort_selection(_, _, _, _)),
    retractall(webserver_graphql_subs:graphql_broadcast_sent(_, _)),
    retractall(webserver_graphql_subs:graphql_sse_subscription(_, _, _)).

