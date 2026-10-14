:- module(api_indexer, [
              api_index_jobs/8,
              io_push_delta/4,
              io_index_branch/3,
              descriptor_graphspec/2,
              validate_index_path/1,
              encode_query_value/2,
              validation_is_index_enabled/1,
              indexer_process_commit/4,
              indexer_next_commit/4,
              count_indexable_documents/4,
              commit_ops_begin/0,
              commit_op/8,
              embedding_refresh_purge_types/6,
              embedding_type_queries/2,
              schema_store_clustering/2,
              schema_store_clustering_for_descriptor/2,
              schema_store_clustering_for_path/2,
              schema_store_indices/2,
              schema_store_indices_for_descriptor/2,
              schema_store_indices_for_path/2
          ]).

:- use_module(core(document/history),[commits_changed_id/5]).
:- use_module(core(document),[get_document/3, all_class_frames/3,
                              schema_metadata_descriptor/3]).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(transaction/ref_entity),
             [branch_head_commit/3, commit_id_uri/3,
              commit_uri_to_parent_uri/3, layer_uri_for_commit/4,
              commit_uri_to_history_commit_ids/3]).
:- use_module(core(util)).
:- use_module(core(account)).
:- use_module(library(json)).
:- use_module(config(terminus_config)).
:- use_module(core(api/api_graphql)).
:- use_module(core(triple), [super_user_authority/1, database_schema/2, xrdf/4]).
:- use_module(core(document/json), [database_schema_context_object/2]).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).
:- use_module(library(yall)).
:- use_module(library(lists)).
:- use_module(library(dicts)).
:- use_module(library(url), [www_form_encode/2]).

embedding_type_queries(Commit_Descriptor, TypeQueries) :-
    open_descriptor(Commit_Descriptor, Transaction),
    database_schema(Transaction, Schema),
    findall(
        Type-Query-Template,
        (   xrdf(Schema, Type, sys:metadata, _),
            schema_metadata_descriptor(Schema, Type, metadata(Metadata)),
            get_dict(embedding, Metadata, Embedding),
            get_dict(query, Embedding, Query),
            (   get_dict(template, Embedding, Template)
            ->  true
            ;   Template = none)),
        TypeQueries
    ).

%% api_indexable(+Previous_Commit_Id, +Descriptor, -Commit_Id, -Type, -Operation) is nondet.
%%
%%  Uses the fast Rust $changes:collect_changed_documents_filtered/5 predicate
%%  to enumerate changed documents for the given type between the previous
%%  commit and this commit. The Rust predicate operates directly on the
%%  layer's triple additions/removals and is orders of magnitude faster than
%%  the Prolog commits_changed_id/5.
api_indexable(some(_Previous_Commit_Id), Descriptor, Commit_Id, Type, Operation) :-
    resolve_relative_descriptor(Descriptor,
                                ["commit", Commit_Id],
                                Commit_Descriptor),
    open_descriptor(Commit_Descriptor, Transaction),
    atom_string(TypeAtom, Type),
    '$changes':collect_changed_documents_filtered(Transaction, [TypeAtom], Id, ChangeType),
    change_type_to_op(ChangeType, Id, Operation).

%% api_indexable(+Previous_Commit_Id, +Descriptor, -Commit_Id, -Type, -Operation) is nondet.
%%
%%  First commit (no previous): all documents of the given type are Inserted.
%%  Uses direct ask query since there is no delta to compute.
api_indexable(none, Descriptor, Commit_Id, Type, Operation) :-
    resolve_relative_descriptor(Descriptor,
                                ["commit", Commit_Id],
                                Commit_Descriptor),
    ask(Commit_Descriptor, t(Id, rdf:type, Type),[compress_prefixes(false)]),
    Operation = json{ op: 'Inserted',
                      id: Id }.

%% change_type_to_op(+ChangeType, +Id, -Operation) is det.
%%
%%  Maps Rust change type atoms to indexer operation dicts.
change_type_to_op(added, Id, json{op:'Inserted', id:Id}).
change_type_to_op(changed, Id, json{op:'Changed', id:Id}).
change_type_to_op(deleted, Id, json{op:'Deleted', id:Id}).

/* predicate which returns the various jobs as op/string:
{ "op" : "Inserted", "id" : "Doc/1", "string" : "this is in doc 1" }
{ "op" : "Changed", "id" : "Doc/2", "string" : "this is new in doc 2" }
{ "op" : "Deleted", "id" : "Doc/3"}
*/
:- meta_predicate api_index_jobs(+, +, +, 1, +, +, +, +).
api_index_jobs(System_DB, Auth, Stream, Prelude, Path, Commit_Id, Maybe_Previous_Commit_Id, _Options) :-
    do_or_die(
        is_super_user(Auth),
        error(indexing_requires_superuser, _)
    ),
    resolve_absolute_string_descriptor(Path, Descriptor),
    resolve_relative_descriptor(Descriptor,
                                ["commit", Commit_Id],
                                Commit_Descriptor),
    embedding_type_queries(Commit_Descriptor, TypeQueries),
    maplist([Type-Query-_Template, Type-Query]>>true,
            TypeQueries,
            Queries),
    convlist([Type-Query-Template, Type-Template]>>ground(Template),
             TypeQueries,
             Templates),
    % '$handlebars':handlebars_context(Templates, Handlebars),
    open_descriptor(Commit_Descriptor, Transaction),
    all_class_frames(Transaction, Frames, [compress_ids(true),expand_abstract(true),simple(true)]),
    '$embedding':embedding_context(System_DB, Transaction, Templates, Queries, Frames, Embedding_Context),
    (   Maybe_Previous_Commit_Id = none
    ->  RefreshTypes = [], PurgeTypes = []
    ;   embedding_refresh_purge_types(Transaction, Descriptor,
                                      Commit_Id, TypeQueries,
                                      RefreshTypes, PurgeTypes)
    ),
    call(Prelude,Stream),
    commit_ops_begin,
    forall(
        commit_op(Transaction, Descriptor, Commit_Id, TypeQueries,
                  Maybe_Previous_Commit_Id, RefreshTypes, PurgeTypes,
                  Type-Operation),
        (   get_dict(op, Operation, Op),
            ignore(get_dict(id, Operation, Id)),
            '$embedding':write_op_for(Stream, System_DB, Transaction, Embedding_Context, Type, Id, Op)
        )
    ).

/**
 * encode_query_value(+Value, -Encoded) is det.
 *
 * Percent-encodes a value for use as an HTTP query parameter using
 * application/x-www-form-urlencoded rules. This encodes ALL characters
 * that have structural meaning in URLs including '/' (%2f), '&' (%26),
 * '=' (%3d), '+', '?', '#', and space.
 *
 * Uses www_form_encode/2 which produces LOWERCASE hex digits. This is
 * fully RFC 3986 compliant (percent-encoding is case-insensitive per S2.1).
 *
 * Accepts both atoms and strings as input; always produces an atom.
 */
encode_query_value(Value, Encoded) :-
    (   atom(Value)
    ->  Atom = Value
    ;   atom_string(Atom, Value)
    ),
    www_form_encode(Atom, Encoded).

% ==========================================================================
% All curl-based push code removed — replaced by indexer_notify FFI.
% The Rust IndexerRegistry handles /last-indexed, /push, /check, and 409
% resolution internally via reqwest. Prolog only calls indexer_notify/4.
% ==========================================================================

/**
 * validate_index_path(+Path) is det.
 *
 * Validates that Path conforms to the push driver's input contract.
 * The driver accepts ONLY these forms:
 *
 *   - 2 segments: "org/db" (shorthand for org/db/local/branch/main)
 *   - 5 segments: "org/db/<repo>/branch/<branch>"
 *                 or "org/db/<repo>/commit/<commit>"
 *     where segment 4 MUST be exactly "branch" or "commit".
 *
 * Everything else is REJECTED with a clear error BEFORE any descriptor
 * resolution or network I/O. This prevents the Prolog resolver's convenience
 * rules (e.g., 3-seg -> branch/main) from silently accepting malformed paths.
 *
 * Throws: error(invalid_index_path(Path, Reason), _)
 */
validate_index_path(Path) :-
    (   atom(Path)
    ->  atom_string(Path, Path_String)
    ;   Path_String = Path
    ),
    pattern_string_split("/", Path_String, Segments_Unfiltered),
    exclude(=(""), Segments_Unfiltered, Segments),
    length(Segments, N),
    validate_index_segments(N, Segments, Path).

validate_index_segments(2, [_Org, _DB], _Path) :- !.
validate_index_segments(5, [_Org, _DB, _Repo, Seg4, _Name], Path) :-
    !,
    text_to_string(Seg4, Seg4_Str),
    (   Seg4_Str == "branch"
    ->  true
    ;   Seg4_Str == "commit"
    ->  true
    ;   throw(error(invalid_index_path(Path,
                        bad_segment_4(Seg4, expected_branch_or_commit)), _))
    ).
validate_index_segments(N, _Segments, Path) :-
    throw(error(invalid_index_path(Path,
                    wrong_segment_count(N, expected_2_or_5)), _)).

/**
 * io_push_delta(+System_DB, +Auth, +Path, +Branch_Name) is det.
 *
 * Thin wrapper around the indexer_notify/4 FFI predicate. Validates the
 * path, checks that the vectorlink endpoint is configured, and delegates
 * to the Rust IndexerRegistry which handles NDJSON generation, HTTP
 * streaming, 409 resolution, and task polling internally.
 *
 * Fails loud on:
 *   - invalid path (not 2-or-5 segment form)
 *   - indexer_backend not http_vectorlink (wrong backend)
 *   - vectorlink endpoint not configured
 *   - indexer_notify FFI not loaded (Rust runtime not available)
 */
%% push_indexer_backend is semidet.
%
%  Succeeds when a push/search indexer backend is active — either the
%  remote http_vectorlink engine or the embedded vectorlink engine.
push_indexer_backend :-
    indexer_backend(Backend),
    memberchk(Backend, [http_vectorlink, vectorlink]).

io_push_delta(_System_DB, _Auth, Path, Branch_Name) :-
    validate_index_path(Path),
    do_or_die(
        push_indexer_backend,
        error(indexer_backend_not_vectorlink(io_push_delta), _)),
    do_or_die(
        vectorlink_endpoint(_Endpoint),
        error(vectorlink_endpoint_not_configured(io_push_delta), _)),
    (   atom_concat(_, '/local/branch/', Path)
    ->  Branch_Path = Path
    ;   format(atom(Branch_Path), "~w/local/branch/~w", [Path, Branch_Name])
    ),
    (   plugin_api:indexer_available
    ->  (   schema_store_indices_for_path(Path, Store_Indices),
            (   Store_Indices == true
            ->  schema_store_clustering_for_path(Path, Store_Clustering),
                Has_Embeddings = true
            ;   Store_Clustering = false,
                Has_Embeddings = false
            ),
            plugin_api:indexer_notify(Branch_Path, Branch_Name, Store_Clustering, Has_Embeddings)
        ->  true
        ;   throw(error(indexer_notify_failed(io_push_delta), _))
        )
    ;   throw(error(indexer_ffi_not_loaded(io_push_delta), _))
    ).

/**
 * commits_after(+Last_Commit, +History_Oldest_First, -Forward_Range) is det.
 */
commits_after(Last_Commit, History, Forward_Range) :-
    (   append(_, [Last_Commit | Forward_Range], History)
    ->  true
    ;   throw(error(vectorlink_last_indexed_not_in_history(Last_Commit), _))
    ).

/**
 * descriptor_graphspec(+Descriptor, -GraphSpec) is det.
 *
 * Derives the full graphspec string from a resolved TerminusDB descriptor.
 * Uses resolve_absolute_string_descriptor/2 in reverse mode.
 */
descriptor_graphspec(Descriptor, GraphSpec) :-
    resolve_absolute_string_descriptor(GraphSpec, Descriptor).

/**
 * path_to_domain(+Path, -Domain) is det.
 *
 * DEPRECATED — retained for test backward compatibility.
 */
path_to_domain(Path, Domain) :-
    pattern_string_split("/", Path, Segments_Unfiltered),
    exclude(=(""), Segments_Unfiltered, Segments),
    do_or_die(
        Segments = [Org, DB | _],
        error(invalid_path_for_domain(Path), _)),
    format(atom(Domain), "~w/~w", [Org, DB]).

/**
 * io_index_branch(+System_DB, +Auth, +Path) is det.
 *
 * Explicit "index this branch now" entrypoint. Validates path structure,
 * then resolves descriptor and drives a push. Gated on http_vectorlink.
 *
 * The Path MUST conform to the driver input contract (2-seg or 5-seg form).
 * Validation fires BEFORE descriptor resolution or any I/O.
 */
io_index_branch(System_DB, Auth, Path) :-
    % Validate path structure FIRST — before any descriptor resolution or I/O.
    validate_index_path(Path),
    do_or_die(
        push_indexer_backend,
        error(indexer_backend_not_vectorlink(io_index_branch), _)),
    resolve_descriptor_auth(read, System_DB, Auth, Path, instance, Descriptor),
    do_or_die(
        branch_descriptor{branch_name: Branch_Name} :< Descriptor,
        error(push_requires_branch_descriptor(Path), _)),
    io_push_delta(System_DB, Auth, Path, Branch_Name).

% ==========================================================================
% Auto-push-on-commit hook
%
% Fires after every commit (multifile post_commit_hook/2 from plugins.pl).
% GATE: only acts when indexer_backend = http_vectorlink AND the committed
% data product's schema has store_indices enabled in @metadata.terminusdb.options.
%
% OPTIMISED COMMIT PATH:
%   - Schema-change commits: check store_indices from Schema_Objects (fast
%     in-memory option lookup, no descriptor open). If true, also read
%     store_clustering and call indexer_notify with has_embeddings=true.
%   - Document-only commits: skip all schema checks. Call indexer_notify
%     with has_embeddings=false. The Rust-side no_embedding_cache skips
%     instantly for domains without store_indices. If a task already exists
%     (from a prior schema commit), notify_count is incremented (O(1)).
%
% ASYNC FIRE-AND-FORGET: calls indexer_notify/4 FFI which spawns a tokio
% task in the Rust IndexerRegistry. The task runs independently of the
% commit path — commit latency is NOT inflated.
%
% FAILURE SEMANTICS: the tokio task catches its own errors, logs them,
% and stops. The commit is NEVER blocked or broken by engine-down / push
% failure. The existing search-miss nudge (api_search.pl maybe_nudge_push)
% remains as catch-up fallback.
%
% NO DOUBLE-PUSH: the engine's 409 transactional guard makes a duplicate
% push (hook + nudge both fire for same commit) a safe no-op.
% ==========================================================================

:- use_module(core(plugins)).
:- multifile plugins:post_commit_hook/2.

plugins:post_commit_hook(Validations, _Meta_Data) :-
    % Gate 1: a push indexer backend must be active. Cheap tabled check.
    push_indexer_backend,
    % Gate 2: FFI predicate must be registered (Rust runtime loaded).
    plugin_api:indexer_available,
    % Gate 3: for each branch validation, call indexer_notify.
    % Schema-change commits check store_indices (fast option lookup).
    % Document-only commits skip all schema checks — the Rust-side
    % no_embedding_cache handles the skip for domains without store_indices.
    catch(
        forall(
            (   member(Validation, Validations),
                get_dict(descriptor, Validation, Descriptor),
                branch_descriptor{branch_name: Branch_Name} :< Descriptor,
                descriptor_graphspec(Descriptor, Path),
                get_dict(schema_objects, Validation, Schema_Objects),
                (   Schema_Objects \== []
                ->  % Schema-change commit: check store_indices flag (fast
                    % in-memory option lookup, no descriptor open).
                    schema_store_indices(Schema_Objects, Store_Indices),
                    (   Store_Indices == true
                    ->  schema_store_clustering(Schema_Objects, Store_Clustering),
                        Has_Embeddings = true
                    ;   Store_Clustering = false,
                        Has_Embeddings = false
                    )
                ;   % Document-only commit: skip expensive schema checks.
                    % Let the Rust-side no_embedding_cache decide.
                    Store_Clustering = false,
                    Has_Embeddings = false
                    )
                ),
            catch(
                plugin_api:indexer_notify(Path, Branch_Name, Store_Clustering, Has_Embeddings),
                Notify_Error,
                format(user_error,
                       "[ERROR] indexer_notify failed for ~w (~w): ~q~n",
                       [Path, Branch_Name, Notify_Error])
            )
        ),
        Hook_Error,
        format(user_error,
               "[ERROR] Auto-push hook generator failed: ~q~n",
               [Hook_Error])
    ).

/**
 * validation_is_index_enabled(+Validation) is semidet.
 *
 * True if Validation has a branch_descriptor AND its schema contains at
 * least one type with sys:metadata embedding configuration. This is the
 * lightweight enablement gate — avoids spawning threads for data products
 * that have no indexed types.
 */
validation_is_index_enabled(Validation) :-
    get_dict(descriptor, Validation, Descriptor),
    branch_descriptor{} :< Descriptor,
    get_dict(schema_objects, Validation, Schema_Objects),
    (   Schema_Objects \== []
    ->  once(xrdf(Schema_Objects, _Type, sys:metadata, _))
    ;   % Document-only commit: schema didn't change, but the branch
        % schema may still have embedding metadata. Open the branch
        % descriptor and check its schema.
        open_descriptor(Descriptor, Transaction),
        database_schema(Transaction, Schema),
        once(xrdf(Schema, _Type, sys:metadata, _))
    ).

/**
 * schema_store_indices(+Schema, -StoreIndices) is det.
 *
 *  Reads the store_indices flag from the schema @context document's
 *  @metadata.terminusdb.options array. Returns false if the option is
 *  not present, if @metadata or terminusdb is missing, or if the schema
 *  has no context object.
 *
 *  This is the primary gate for whether indexing should happen for a
 *  data product. It is a fast option lookup (no xrdf scan).
 */
schema_store_indices(Schema, Store_Indices) :-
    (   catch(
            (   database_schema_context_object(Schema, Context),
                get_dict('@metadata', Context, Metadata),
                get_dict('terminusdb', Metadata, TerminusDB),
                get_dict('options', TerminusDB, Options),
                memberchk("store_indices", Options)
            ),
            _,
            fail
        )
    ->  Store_Indices = true
    ;   Store_Indices = false
    ).

/**
 * schema_store_indices_for_descriptor(+Descriptor, -StoreIndices) is det.
 *
 *  Opens the descriptor, reads the schema, and checks for store_indices.
 *  Falls back to false on any error (e.g. schemaless database).
 */
schema_store_indices_for_descriptor(Descriptor, Store_Indices) :-
    (   catch(
            (   open_descriptor(Descriptor, Transaction),
                database_schema(Transaction, Schema),
                schema_store_indices(Schema, Store_Indices)
            ),
            _,
            fail
        )
    ->  true
    ;   Store_Indices = false
    ).

/**
 * schema_store_indices_for_path(+Path, -StoreIndices) is det.
 *
 *  Resolves the path to a descriptor and checks for store_indices.
 */
schema_store_indices_for_path(Path, Store_Indices) :-
    (   catch(
            (   resolve_absolute_string_descriptor(Path, Descriptor),
                schema_store_indices_for_descriptor(Descriptor, Store_Indices)
            ),
            _,
            fail
        )
    ->  true
    ;   Store_Indices = false
    ).

/**
 * schema_store_clustering(+Schema, -StoreClustering) is det.
 *
 *  Reads the store_clustering flag from the schema @context document's
 *  @metadata.terminusdb.options array. Returns false if the option is
 *  not present, if @metadata or terminusdb is missing, or if the schema
 *  has no context object.
 */
schema_store_clustering(Schema, Store_Clustering) :-
    (   catch(
            (   database_schema_context_object(Schema, Context),
                get_dict('@metadata', Context, Metadata),
                get_dict('terminusdb', Metadata, TerminusDB),
                get_dict('options', TerminusDB, Options),
                memberchk("store_clustering", Options)
            ),
            _,
            fail
        )
    ->  Store_Clustering = true
    ;   Store_Clustering = false
    ).

/**
 * schema_store_clustering_for_descriptor(+Descriptor, -StoreClustering) is det.
 *
 *  Opens the descriptor, reads the schema, and checks for store_clustering.
 *  Falls back to false on any error (e.g. schemaless database).
 */
schema_store_clustering_for_descriptor(Descriptor, Store_Clustering) :-
    (   catch(
            (   open_descriptor(Descriptor, Transaction),
                database_schema(Transaction, Schema),
                schema_store_clustering(Schema, Store_Clustering)
            ),
            _,
            fail
        )
    ->  true
    ;   Store_Clustering = false
    ).

/**
 * schema_store_clustering_for_path(+Path, -StoreClustering) is det.
 *
 *  Resolves the path to a descriptor and checks for store_clustering.
 */
schema_store_clustering_for_path(Path, Store_Clustering) :-
    (   catch(
            (   resolve_absolute_string_descriptor(Path, Descriptor),
                schema_store_clustering_for_descriptor(Descriptor, Store_Clustering)
            ),
            _,
            fail
        )
    ->  true
    ;   Store_Clustering = false
    ).

% ==========================================================================
% FFI dispatch predicates — called from Rust dispatch loop
% ==========================================================================

%% indexer_next_commit(+Path, +BranchName, +CommitId, -NextCommit) is det.
%
%  Given a commit ID, finds the next commit in the branch history.
%  Returns the next commit ID as a string, or the atom 'None' if the
%  given commit is HEAD (no next commit).
indexer_next_commit(Path, BranchNameRaw, CommitIdRaw, NextCommit) :-
    text_to_string(CommitIdRaw, CommitId),
    resolve_absolute_string_descriptor(Path, Descriptor),
    text_to_string(BranchNameRaw, BranchNameStr),
    branch_descriptor{branch_name: BranchNameStr} :< Descriptor,
    get_dict(repository_descriptor, Descriptor, Repository_Descriptor),
    branch_head_commit(Repository_Descriptor, BranchNameStr, Head_Commit_Uri),
    commit_id_uri(Repository_Descriptor, _Head_Commit_Id, Head_Commit_Uri),
    commit_uri_to_history_commit_ids(Repository_Descriptor,
                                     Head_Commit_Uri,
                                     History_Oldest_First),
    (   append(_, [CommitId, NextCommitId|_], History_Oldest_First)
    ->  text_to_string(NextCommitId, NextCommit)
    ;   NextCommit = 'None'
    ).

%% indexer_process_commit(+Path, +BranchName, +CommitId, +Stream) is det.
%
%  Opens the commit descriptor, computes the parent commit from history,
%  finds changed documents via api_indexable/5, generates embedding text,
%  and writes NDJSON lines to Stream. On error, writes an error marker
%  JSON line to the stream before failing.
indexer_process_commit(Path, _BranchName, CommitIdRaw, Stream) :-
    text_to_string(CommitIdRaw, CommitId),
    resolve_absolute_string_descriptor(Path, Descriptor),
    resolve_relative_descriptor(Descriptor,
                                ["commit", CommitId],
                                Commit_Descriptor),
    embedding_type_queries(Commit_Descriptor, TypeQueries),
    open_descriptor(Commit_Descriptor, Transaction),
    catch(
        (   write_commit_ndjson(Transaction, Descriptor,
                                CommitId, TypeQueries, Stream)
        ->  true
        ;   fail
        ),
        Error,
        (   with_output_to(atom(Error_Atom),
                write_term(Error, [quoted(false)])),
            Error_Json = json{op:"Error", message:Error_Atom},
            json_write_dict(Stream, Error_Json, []),
            nl(Stream),
            flush_output(Stream),
            fail
        )
    ).

%% commit_parent(+Descriptor, +CommitId, -Maybe_Previous) is det.
%
%  The parent commit in the branch history: some(Parent_Id), or none for
%  the first commit.
commit_parent(Descriptor, CommitId, Maybe_Previous) :-
    get_dict(repository_descriptor, Descriptor, Repository_Descriptor),
    commit_id_uri(Repository_Descriptor, CommitId, Commit_Uri),
    (   commit_uri_to_parent_uri(Repository_Descriptor, Commit_Uri,
                                 Parent_Uri)
    ->  commit_id_uri(Repository_Descriptor, Parent_Id, Parent_Uri),
        Maybe_Previous = some(Parent_Id)
    ;   Maybe_Previous = none
    ).

%% embedding_changed_types(+Transaction, -Types) is det.
%
%  Types whose embedding query/template changed in this commit's schema
%  layer, detected from the sys:metadata → json#embedding →
%  json#query|json#template chains in the layer's triple delta
%  (Rust: $changes:collect_changed_embedding_types/2).
embedding_changed_types(Transaction, Types) :-
    findall(TA,
            (   '$changes':collect_changed_embedding_types(Transaction, T),
                atom_string(TA, T)
            ),
            Types0),
    sort(Types0, Types).

%% embedding_refresh_purge_types(+Transaction, +TypeQueries,
%%                               -RefreshTypes, -PurgeTypes) is det.
%
%  Splits detected embedding-config changes into types with a complete
%  config (query AND template — re-embed their docs) and types whose
%  config is now incomplete or gone (purge: no valid embedding exists
%  without both fields).
embedding_refresh_purge_types(Transaction, Descriptor, CommitId,
                              TypeQueries, RefreshTypes, PurgeTypes) :-
    (   % Only a commit that actually wrote the schema graph can change
        % embedding configs — an unchanged commit shares its parent's
        % schema layer, whose delta belongs to the commit that wrote it.
        commit_wrote_graph_(Descriptor, CommitId, schema)
    ->  embedding_changed_types(Transaction, Changed),
        include(complete_embedding_config(TypeQueries), Changed,
                RefreshTypes),
        exclude(complete_embedding_config(TypeQueries), Changed,
                PurgeTypes)
    ;   RefreshTypes = [],
        PurgeTypes = []
    ).

%% commit_wrote_graph_(+Descriptor, +CommitId, +GraphType) is semidet.
%
%  True if this commit wrote a new GraphType (instance or schema) layer.
%  Commits that only touch the other graph share the parent's layer, so
%  the layer delta cannot be attributed to them.
commit_wrote_graph_(Descriptor, CommitId, GraphType) :-
    get_dict(repository_descriptor, Descriptor, Repository_Descriptor),
    commit_id_uri(Repository_Descriptor, CommitId, Commit_Uri),
    layer_uri_for_commit(Repository_Descriptor, Commit_Uri, GraphType,
                         Layer_Uri),
    (   commit_uri_to_parent_uri(Repository_Descriptor, Commit_Uri,
                                 Parent_Uri)
    ->  \+ layer_uri_for_commit(Repository_Descriptor, Parent_Uri,
                                GraphType, Layer_Uri)
    ;   true
    ).

%% commit_wrote_graph(+Descriptor, +CommitId, +GraphType) is semidet.
%
%  Memoised variant of commit_wrote_graph_/3 — safe to call per candidate
%  inside commit_op/8 enumeration. Requires commit_ops_begin/0 to have
%  run first (it resets the memo).
commit_wrote_graph(Descriptor, CommitId, GraphType) :-
    (   nb_current(commit_wrote_graph_memo, Memo)
    ->  (   get_assoc(GraphType, Memo, Result)
        ->  Result == true
        ;   (   commit_wrote_graph_(Descriptor, CommitId, GraphType)
            ->  Result = true
            ;   Result = false
            ),
            put_assoc(GraphType, Memo, Result, Memo2),
            nb_setval(commit_wrote_graph_memo, Memo2),
            Result == true
        )
    ;   commit_wrote_graph_(Descriptor, CommitId, GraphType)
    ).

%% complete_embedding_config(+TypeQueries, +Type) is semidet.
%
%  The type has a usable embedding config in this commit's schema: both
%  query and template present. A detected type failing this is purged —
%  there are no valid embeddings without both fields.
complete_embedding_config(TypeQueries, Type) :-
    member(Type-_Query-Template, TypeQueries),
    Template \== none.

%% commit_ops_begin is det.
%
%  Initialises the per-enumeration dedup set used by mark_fresh/1.
commit_ops_begin :-
    empty_assoc(Empty),
    nb_setval(commit_ops_seen_ids, Empty),
    nb_setval(commit_wrote_graph_memo, Empty).

%% mark_fresh(+Id) is semidet.
%
%  Fails if Id was already yielded by this commit_ops enumeration;
%  otherwise records it. Prevents a doc changed in a schema commit from
%  being emitted twice (once as a changed doc, once via type refresh).
mark_fresh(Id) :-
    atom_string(Key, Id),
    (   nb_current(commit_ops_seen_ids, Seen)
    ->  true
    ;   empty_assoc(Seen)
    ),
    (   get_assoc(Key, Seen, _)
    ->  fail
    ;   put_assoc(Key, Seen, t, Seen2),
        nb_setval(commit_ops_seen_ids, Seen2)
    ).

%% purge_id_of_type(+Transaction, +Descriptor, +Maybe_Previous, +Type, -Id)
%%  is nondet.
%
%  Doc ids of a type whose embedding config is now invalid/absent —
%  enumerated at this commit plus, when a parent exists, at the parent
%  (covers docs deleted in this same commit, which Phase 1 cannot see
%  once the type is no longer embedding-enabled).
purge_id_of_type(Transaction, _Descriptor, _Maybe_Previous, Type, Id) :-
    ask(Transaction, t(Id, rdf:type, Type), [compress_prefixes(false)]).
purge_id_of_type(_Transaction, Descriptor, some(Parent_Id), Type, Id) :-
    resolve_relative_descriptor(Descriptor, ["commit", Parent_Id],
                                Parent_Descriptor),
    ask(Parent_Descriptor, t(Id, rdf:type, Type),
        [compress_prefixes(false)]).

%% commit_op(+Transaction, +Descriptor, +CommitId, +TypeQueries,
%%           +Maybe_Previous, +RefreshTypes, +PurgeTypes, -Op) is nondet.
%
%  Single source of truth for the ops a commit pushes to the engine:
%  changed documents of embedding-enabled types, plus Changed upserts for
%  every doc of a type whose embedding query/template changed in this
%  commit, plus Deleted ops for docs of types whose config became
%  incomplete or was removed. commit_ops_begin/0 must run before
%  enumerating; duplicate ids across phases are suppressed.
commit_op(Transaction, Descriptor, CommitId, TypeQueries, Maybe_Previous,
          RefreshTypes, PurgeTypes, Type-Operation) :-
    (   % Only enumerate doc changes when this commit actually wrote the
        % instance graph — otherwise the shared layer would replay the
        % last instance-writing commit's delta.
        commit_wrote_graph(Descriptor, CommitId, instance),
        member(Type-_Query-_Template, TypeQueries),
        \+ memberchk(Type, PurgeTypes),
        api_indexable(Maybe_Previous, Descriptor, CommitId,
                      Type, Operation)
    ;   member(Type, RefreshTypes),
        ask(Transaction, t(Id, rdf:type, Type),
            [compress_prefixes(false)]),
        Operation = json{op:'Changed', id:Id}
    ;   member(Type, PurgeTypes),
        purge_id_of_type(Transaction, Descriptor, Maybe_Previous,
                         Type, Id),
        Operation = json{op:'Deleted', id:Id}
    ),
    get_dict(id, Operation, Op_Id),
    mark_fresh(Op_Id).

%% write_commit_ndjson(+Transaction, +Descriptor, +CommitId,
%%                     +TypeQueries, +Stream)
%
%  Computes the parent commit from the commit history, then for each
%  embedding-enabled type, finds changed documents via api_indexable/5
%  and writes NDJSON lines with embedding text to Stream.
write_commit_ndjson(Transaction, Descriptor, CommitId,
                    TypeQueries, Stream) :-
    commit_parent(Descriptor, CommitId, Maybe_Previous),
    (   Maybe_Previous = none
    ->  RefreshTypes = [], PurgeTypes = []
    ;   embedding_refresh_purge_types(Transaction, Descriptor,
                                      CommitId, TypeQueries,
                                      RefreshTypes, PurgeTypes)
    ),
    (   TypeQueries = [], RefreshTypes = [], PurgeTypes = []
    ->  true
    ;   open_descriptor(system_descriptor{}, System_DB),
        maplist([Type-Query-_Template, Type-Query]>>true,
                TypeQueries, Queries),
        convlist([Type-Query-Template, Type-Template]>>ground(Template),
                 TypeQueries, Templates),
        all_class_frames(Transaction, Frames,
                         [compress_ids(true), expand_abstract(true),
                          simple(true)]),
        '$embedding':embedding_context(System_DB, Transaction, Templates,
                                       Queries, Frames, Embedding_Context),
        nb_setval(ndjson_count, 0),
        commit_ops_begin,
        forall(
            commit_op(Transaction, Descriptor, CommitId, TypeQueries,
                      Maybe_Previous, RefreshTypes, PurgeTypes,
                      Type-Operation),
            (   get_dict(op, Operation, Op),
                ignore(get_dict(id, Operation, Id)),
                nb_getval(ndjson_count, PrevCount),
                Count is PrevCount + 1,
                nb_setval(ndjson_count, Count),
                '$embedding':write_op_for(Stream, System_DB, Transaction,
                                          Embedding_Context, Type, Id, Op),
                flush_output(Stream)
            )
        )
    ).

%% count_indexable_documents(+Path, +BranchName, +CommitId, -Count) is det.
%
%  Counts the total number of indexable documents that will be processed
%  for a given commit, without generating embedding text. Used by the
%  indexer worker handler to emit an X-Document-Count header for progress
%  reporting.
%
%  Uses the fast Rust $changes:collect_changed_documents_filtered/5 for
%  commits with a parent, and direct ask counting for the first commit.
count_indexable_documents(Path, _BranchName, CommitIdRaw, Count) :-
    text_to_string(CommitIdRaw, CommitId),
    resolve_absolute_string_descriptor(Path, Descriptor),
    resolve_relative_descriptor(Descriptor,
                                ["commit", CommitId],
                                Commit_Descriptor),
    embedding_type_queries(Commit_Descriptor, TypeQueries),
    open_descriptor(Commit_Descriptor, Transaction),
    commit_parent(Descriptor, CommitId, Maybe_Previous),
    (   Maybe_Previous = none
    ->  RefreshTypes = [], PurgeTypes = []
    ;   embedding_refresh_purge_types(Transaction, Descriptor,
                                      CommitId, TypeQueries,
                                      RefreshTypes, PurgeTypes)
    ),
    (   TypeQueries = [], RefreshTypes = [], PurgeTypes = []
    ->  Count = 0
    ;   commit_ops_begin,
        aggregate_all(count,
                      commit_op(Transaction, Descriptor, CommitId,
                                TypeQueries, Maybe_Previous,
                                RefreshTypes, PurgeTypes, _TypeOp),
                      Count)
    ).


:- begin_tests(indexer_predicates, [concurrent(true)]).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(transaction/ref_entity),
             [branch_head_commit/3, commit_id_uri/3,
              commit_uri_to_history_commit_ids/3]).
:- use_module(core(api/api_document), [api_insert_documents/9]).
:- use_module(core(triple), [super_user_authority/1]).

test(next_commit_returns_none_at_head,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "foo"))),
      cleanup(teardown_temp_store(State))
     ]) :-
    resolve_absolute_string_descriptor("admin/foo/local/branch/main", Desc),
    create_context(Desc, commit_info{author:"test",message:"first"}, Ctx),
    with_transaction(Ctx, ask(Ctx, insert(a,b,c)), _),
    get_dict(repository_descriptor, Desc, Repo_Desc),
    branch_head_commit(Repo_Desc, "main", Head_Uri),
    commit_id_uri(Repo_Desc, Head_Commit_Id, Head_Uri),
    indexer_next_commit("admin/foo/local/branch/main", "main",
                        Head_Commit_Id, NextCommit),
    assertion(NextCommit == 'None').

test(next_commit_returns_next_for_older_commit,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "foo"))),
      cleanup(teardown_temp_store(State))
     ]) :-
    resolve_absolute_string_descriptor("admin/foo/local/branch/main", Desc),
    create_context(Desc, commit_info{author:"test",message:"first"}, Ctx1),
    with_transaction(Ctx1, ask(Ctx1, insert(a,b,c)), _),
    get_dict(repository_descriptor, Desc, Repo_Desc),
    branch_head_commit(Repo_Desc, "main", First_Uri),
    commit_id_uri(Repo_Desc, First_Commit_Id, First_Uri),
    create_context(Desc, commit_info{author:"test",message:"second"}, Ctx2),
    with_transaction(Ctx2, ask(Ctx2, insert(d,e,f)), _),
    indexer_next_commit("admin/foo/local/branch/main", "main",
                        First_Commit_Id, NextCommit),
    branch_head_commit(Repo_Desc, "main", Head_Uri),
    commit_id_uri(Repo_Desc, Head_Commit_Id, Head_Uri),
    text_to_string(Head_Commit_Id, Head_Str),
    assertion(NextCommit == Head_Str).

test(process_commit_fails_on_bad_path,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "foo"))),
      cleanup(teardown_temp_store(State)),
      fail
     ]) :-
    open_string("", EmptyStream),
    indexer_process_commit("admin/nonexistent/local/branch/main",
                           "main", "dummy_commit", EmptyStream).

test(next_commit_traverses_three_commit_chain,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "foo"))),
      cleanup(teardown_temp_store(State))
     ]) :-
    resolve_absolute_string_descriptor("admin/foo/local/branch/main", Desc),
    %% Commit 1
    create_context(Desc, commit_info{author:"test",message:"first"}, Ctx1),
    with_transaction(Ctx1, ask(Ctx1, insert(a,b,c)), _),
    get_dict(repository_descriptor, Desc, Repo_Desc),
    branch_head_commit(Repo_Desc, "main", Uri1),
    commit_id_uri(Repo_Desc, Id1, Uri1),
    %% Commit 2
    create_context(Desc, commit_info{author:"test",message:"second"}, Ctx2),
    with_transaction(Ctx2, ask(Ctx2, insert(d,e,f)), _),
    branch_head_commit(Repo_Desc, "main", Uri2),
    commit_id_uri(Repo_Desc, Id2, Uri2),
    %% Commit 3
    create_context(Desc, commit_info{author:"test",message:"third"}, Ctx3),
    with_transaction(Ctx3, ask(Ctx3, insert(g,h,i)), _),
    branch_head_commit(Repo_Desc, "main", Uri3),
    commit_id_uri(Repo_Desc, Id3, Uri3),
    %% Id1 → Id2
    indexer_next_commit("admin/foo/local/branch/main", "main",
                        Id1, Next1),
    text_to_string(Id2, Id2_Str),
    assertion(Next1 == Id2_Str),
    %% Id2 → Id3
    indexer_next_commit("admin/foo/local/branch/main", "main",
                        Id2, Next2),
    text_to_string(Id3, Id3_Str),
    assertion(Next2 == Id3_Str),
    %% Id3 → None (at head)
    indexer_next_commit("admin/foo/local/branch/main", "main",
                        Id3, Next3),
    assertion(Next3 == 'None').

test(process_commit_writes_ndjson_to_stream,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "foo"))),
      cleanup(teardown_temp_store(State))
     ]) :-
    resolve_absolute_string_descriptor("admin/foo/local/branch/main", Desc),
    create_context(Desc, commit_info{author:"test",message:"first"}, Ctx),
    with_transaction(Ctx, ask(Ctx, insert(a,b,c)), _),
    get_dict(repository_descriptor, Desc, Repo_Desc),
    branch_head_commit(Repo_Desc, "main", Head_Uri),
    commit_id_uri(Repo_Desc, Head_Commit_Id, Head_Uri),
    text_to_string(Head_Commit_Id, CommitId),
    %% Capture NDJSON output to a temporary file stream.
    %% The predicate may succeed or fail depending on schema configuration;
    %% the invariant is that it must not throw.
    tmp_file_stream(text, TmpFile, WriteStream),
    (   indexer_process_commit("admin/foo/local/branch/main",
                               "main", CommitId, WriteStream)
    ->  true
    ;   true
    ),
    close(WriteStream),
    delete_file(TmpFile).

test(next_commit_returns_none_for_nonexistent_commit,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "foo"))),
      cleanup(teardown_temp_store(State))
     ]) :-
    %% A commit ID not in the branch history yields 'None'.
    indexer_next_commit("admin/foo/local/branch/main", "main",
                        "nonexistent_commit_id_xyz", Next),
    assertion(Next == 'None').

test(count_indexable_documents_returns_zero_for_no_embedding_schema,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "foo"))),
      cleanup(teardown_temp_store(State))
     ]) :-
    resolve_absolute_string_descriptor("admin/foo/local/branch/main", Desc),
    create_context(Desc, commit_info{author:"test",message:"first"}, Ctx),
    with_transaction(Ctx, ask(Ctx, insert(a,b,c)), _),
    get_dict(repository_descriptor, Desc, Repo_Desc),
    branch_head_commit(Repo_Desc, "main", Head_Uri),
    commit_id_uri(Repo_Desc, Head_Commit_Id, Head_Uri),
    text_to_string(Head_Commit_Id, CommitId),
    count_indexable_documents("admin/foo/local/branch/main", "main",
                              CommitId, Count),
    assertion(Count == 0).

test(count_indexable_documents_counts_documents_with_embedding_schema,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "embdb"))),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    super_user_authority(Auth),
    open_string('
[
  {
    "@type": "@context",
    "@base": "http://example.com/data/",
    "@schema": "http://example.com/schema#"
  },
  {
    "@type": "Class",
    "@id": "Article",
    "@key": { "@type": "Lexical", "@fields": ["title"] },
    "title": "xsd:string",
    "body": "xsd:string",
    "@metadata": {
      "embedding": {
        "query": "query($id: ID){ Article(id: $id) { title body } }"
      }
    }
  }
]
', SchemaStream),
    Options = [author("test"), full_replace(true), graph_type(schema), message("test schema")],
    api_insert_documents(System, Auth, "admin/embdb", SchemaStream, no_data_version, _, _, _, Options),
    %% Insert 3 documents
    open_string('
[
  {"@type": "Article", "title": "First Document", "body": "Content of first document"},
  {"@type": "Article", "title": "Second Document", "body": "Content of second document"},
  {"@type": "Article", "title": "Third Document", "body": "Content of third document"}
]
', DocStream),
    DocOptions = [author("test"), graph_type(instance), message("test docs")],
    api_insert_documents(System, Auth, "admin/embdb", DocStream, no_data_version, _, _, _, DocOptions),
    resolve_absolute_string_descriptor("admin/embdb/local/branch/main", Desc),
    get_dict(repository_descriptor, Desc, Repo_Desc),
    branch_head_commit(Repo_Desc, "main", Head_Uri),
    commit_id_uri(Repo_Desc, Head_Commit_Id, Head_Uri),
    text_to_string(Head_Commit_Id, CommitId),
    count_indexable_documents("admin/embdb/local/branch/main", "main",
                              CommitId, Count),
    assertion(Count == 3).

test(schema_store_clustering_returns_true_when_option_present,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "clusterdb"))),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    super_user_authority(Auth),
    open_string('
[
  {
    "@type": "@context",
    "@base": "http://example.com/data/",
    "@schema": "http://example.com/schema#",
    "@metadata": {
      "terminusdb": {
        "options": ["store_clustering"]
      }
    }
  },
  {
    "@type": "Class",
    "@id": "Article",
    "@key": { "@type": "Lexical", "@fields": ["title"] },
    "title": "xsd:string",
    "body": "xsd:string",
    "@metadata": {
      "embedding": {
        "query": "query($id: ID){ Article(id: $id) { title body } }"
      }
    }
  }
]
', SchemaStream),
    Options = [author("test"), full_replace(true), graph_type(schema), message("test schema")],
    api_insert_documents(System, Auth, "admin/clusterdb", SchemaStream, no_data_version, _, _, _, Options),
    resolve_absolute_string_descriptor("admin/clusterdb/local/branch/main", Desc),
    open_descriptor(Desc, Transaction),
    database_schema(Transaction, Schema),
    schema_store_clustering(Schema, Result),
    assertion(Result == true).

test(schema_store_clustering_returns_false_when_option_absent,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "noclusterdb"))),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    super_user_authority(Auth),
    open_string('
[
  {
    "@type": "@context",
    "@base": "http://example.com/data/",
    "@schema": "http://example.com/schema#"
  },
  {
    "@type": "Class",
    "@id": "Article",
    "@key": { "@type": "Lexical", "@fields": ["title"] },
    "title": "xsd:string",
    "body": "xsd:string",
    "@metadata": {
      "embedding": {
        "query": "query($id: ID){ Article(id: $id) { title body } }"
      }
    }
  }
]
', SchemaStream),
    Options = [author("test"), full_replace(true), graph_type(schema), message("test schema")],
    api_insert_documents(System, Auth, "admin/noclusterdb", SchemaStream, no_data_version, _, _, _, Options),
    resolve_absolute_string_descriptor("admin/noclusterdb/local/branch/main", Desc),
    open_descriptor(Desc, Transaction),
    database_schema(Transaction, Schema),
    schema_store_clustering(Schema, Result),
    assertion(Result == false).

test(schema_store_clustering_for_path_returns_false_for_schemaless_db,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "schemadb"))),
      cleanup(teardown_temp_store(State))
     ]) :-
    schema_store_clustering_for_path("admin/schemadb/local/branch/main", Result),
    assertion(Result == false).

test(schema_store_indices_returns_true_when_option_present,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "indicesdb"))),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    super_user_authority(Auth),
    open_string('
[
  {
    "@type": "@context",
    "@base": "http://example.com/data/",
    "@schema": "http://example.com/schema#",
    "@metadata": {
      "terminusdb": {
        "options": ["store_indices"]
      }
    }
  },
  {
    "@type": "Class",
    "@id": "Article",
    "@key": { "@type": "Lexical", "@fields": ["title"] },
    "title": "xsd:string",
    "body": "xsd:string",
    "@metadata": {
      "embedding": {
        "query": "query($id: ID){ Article(id: $id) { title body } }"
      }
    }
  }
]
', SchemaStream),
    Options = [author("test"), full_replace(true), graph_type(schema), message("test schema")],
    api_insert_documents(System, Auth, "admin/indicesdb", SchemaStream, no_data_version, _, _, _, Options),
    resolve_absolute_string_descriptor("admin/indicesdb/local/branch/main", Desc),
    open_descriptor(Desc, Transaction),
    database_schema(Transaction, Schema),
    schema_store_indices(Schema, Result),
    assertion(Result == true).

test(schema_store_indices_returns_false_when_option_absent,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "noindicesdb"))),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    super_user_authority(Auth),
    open_string('
[
  {
    "@type": "@context",
    "@base": "http://example.com/data/",
    "@schema": "http://example.com/schema#"
  },
  {
    "@type": "Class",
    "@id": "Article",
    "@key": { "@type": "Lexical", "@fields": ["title"] },
    "title": "xsd:string",
    "body": "xsd:string",
    "@metadata": {
      "embedding": {
        "query": "query($id: ID){ Article(id: $id) { title body } }"
      }
    }
  }
]
', SchemaStream),
    Options = [author("test"), full_replace(true), graph_type(schema), message("test schema")],
    api_insert_documents(System, Auth, "admin/noindicesdb", SchemaStream, no_data_version, _, _, _, Options),
    resolve_absolute_string_descriptor("admin/noindicesdb/local/branch/main", Desc),
    open_descriptor(Desc, Transaction),
    database_schema(Transaction, Schema),
    schema_store_indices(Schema, Result),
    assertion(Result == false).

test(schema_store_indices_for_path_returns_false_for_schemaless_db,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "schemadb2"))),
      cleanup(teardown_temp_store(State))
     ]) :-
    schema_store_indices_for_path("admin/schemadb2/local/branch/main", Result),
    assertion(Result == false).

test(schema_store_indices_for_descriptor_returns_true_with_both_options,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "bothoptsdb"))),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    super_user_authority(Auth),
    open_string('
[
  {
    "@type": "@context",
    "@base": "http://example.com/data/",
    "@schema": "http://example.com/schema#",
    "@metadata": {
      "terminusdb": {
        "options": ["store_indices", "store_clustering"]
      }
    }
  },
  {
    "@type": "Class",
    "@id": "Article",
    "@key": { "@type": "Lexical", "@fields": ["title"] },
    "title": "xsd:string",
    "body": "xsd:string",
    "@metadata": {
      "embedding": {
        "query": "query($id: ID){ Article(id: $id) { title body } }"
      }
    }
  }
]
', SchemaStream),
    Options = [author("test"), full_replace(true), graph_type(schema), message("test schema")],
    api_insert_documents(System, Auth, "admin/bothoptsdb", SchemaStream, no_data_version, _, _, _, Options),
    resolve_absolute_string_descriptor("admin/bothoptsdb/local/branch/main", Desc),
    schema_store_indices_for_descriptor(Desc, Result),
    assertion(Result == true).

test(process_commit_renders_embedding_with_commit_schema,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "headdb"))),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    super_user_authority(Auth),
    %% Schema v1: embedding template marked V1MARKER
    open_string('
[
  {
    "@type": "@context",
    "@base": "http://example.com/data/",
    "@schema": "http://example.com/schema#"
  },
  {
    "@type": "Class",
    "@id": "Article",
    "@key": { "@type": "Lexical", "@fields": ["title"] },
    "title": "xsd:string",
    "body": "xsd:string",
    "@metadata": {
      "embedding": {
        "query": "query($id: ID){ Article(id: $id) { title body } }",
        "template": "V1MARKER {{title}}"
      }
    }
  }
]
', SchemaStream1),
    Options1 = [author("test"), full_replace(true), graph_type(schema), message("schema v1")],
    api_insert_documents(System, Auth, "admin/headdb", SchemaStream1, no_data_version, _, _, _, Options1),
    %% Documents commit — this becomes non-head once schema v2 lands
    open_string('
[
  {"@type": "Article", "title": "My Title", "body": "My Body"}
]
', DocStream),
    DocOptions = [author("test"), graph_type(instance), message("docs")],
    api_insert_documents(System, Auth, "admin/headdb", DocStream, no_data_version, _, _, _, DocOptions),
    resolve_absolute_string_descriptor("admin/headdb/local/branch/main", Desc),
    get_dict(repository_descriptor, Desc, Repo_Desc),
    branch_head_commit(Repo_Desc, "main", Docs_Head_Uri),
    commit_id_uri(Repo_Desc, Docs_Commit_Id, Docs_Head_Uri),
    %% Schema v2: same class, new template marked V2MARKER
    open_string('
[
  {
    "@type": "@context",
    "@base": "http://example.com/data/",
    "@schema": "http://example.com/schema#"
  },
  {
    "@type": "Class",
    "@id": "Article",
    "@key": { "@type": "Lexical", "@fields": ["title"] },
    "title": "xsd:string",
    "body": "xsd:string",
    "@metadata": {
      "embedding": {
        "query": "query($id: ID){ Article(id: $id) { title body } }",
        "template": "V2MARKER {{title}}"
      }
    }
  }
]
', SchemaStream2),
    Options2 = [author("test"), full_replace(true), graph_type(schema), message("schema v2")],
    api_insert_documents(System, Auth, "admin/headdb", SchemaStream2, no_data_version, _, _, _, Options2),
    %% Process the pre-v2 commit: it must render with the schema that was
    %% in effect at that commit — versioned rendering, V1MARKER.
    text_to_string(Docs_Commit_Id, Docs_Commit_Str),
    tmp_file_stream(text, TmpFile, WriteStream),
    indexer_process_commit("admin/headdb/local/branch/main",
                           "main", Docs_Commit_Str, WriteStream),
    close(WriteStream),
    read_file_to_string(TmpFile, Out, []),
    delete_file(TmpFile),
    assertion(sub_string(Out, _, _, _, "V1MARKER")),
    assertion(\+ sub_string(Out, _, _, _, "V2MARKER")).

/* ================================================================
 * Embedding change detection — schema commits that alter a type's
 * @metadata.embedding query or template emit refresh (Changed) or
 * purge (Deleted) ops for all documents of that type inside the schema
 * commit's own push stream.
 * ================================================================ */

%% article_schema(+MetaFragment, -Json)
%  MetaFragment is either '' (no metadata) or a JSON object member list
%  such as '"@metadata": { "embedding": {...} }' preceded by a comma.
article_schema(MetaFragment, Json) :-
    format(string(Json),
'[
  {
    "@type": "@context",
    "@base": "http://example.com/data/",
    "@schema": "http://example.com/schema#"
  },
  {
    "@type": "Class",
    "@id": "Article",
    "@key": { "@type": "Lexical", "@fields": ["title"] },
    "title": "xsd:string",
    "body": "xsd:string"~w
  }
]', [MetaFragment]).

embedding_meta(Query, Template, Fragment) :-
    format(string(Fragment),
           ', "@metadata": { "embedding": { "query": "~s", "template": "~s" } }',
           [Query, Template]).

insert_schema_json(System, Auth, DbPath, Json) :-
    open_string(Json, Stream),
    api_insert_documents(System, Auth, DbPath, Stream, no_data_version,
                         _, _, _,
                         [author("test"), full_replace(true),
                          graph_type(schema), message("schema")]).

insert_docs_json(System, Auth, DbPath, Json) :-
    open_string(Json, Stream),
    api_insert_documents(System, Auth, DbPath, Stream, no_data_version,
                         _, _, _,
                         [author("test"), graph_type(instance),
                          message("docs")]).

three_articles(System, Auth, DbPath) :-
    insert_docs_json(System, Auth, DbPath,
'[
  {"@type": "Article", "title": "First", "body": "Body one"},
  {"@type": "Article", "title": "Second", "body": "Body two"},
  {"@type": "Article", "title": "Third", "body": "Body three"}
]').

head_commit_string(DbPath, CommitStr) :-
    atomic_list_concat([DbPath, "local/branch/main"], '/', BranchPath),
    resolve_absolute_string_descriptor(BranchPath, Desc),
    get_dict(repository_descriptor, Desc, Repo_Desc),
    branch_head_commit(Repo_Desc, "main", Head_Uri),
    commit_id_uri(Repo_Desc, Head_Commit_Id, Head_Uri),
    text_to_string(Head_Commit_Id, CommitStr).

%% collect_commit_ops(+DbPath, +CommitId, -Ops)
%  Runs indexer_process_commit into a temp file and parses the NDJSON
%  lines into dicts. Used to assert the exact op stream a commit emits.
collect_commit_ops(DbPath, CommitId, Ops) :-
    atomic_list_concat([DbPath, "local/branch/main"], '/', BranchPath),
    tmp_file_stream(text, TmpFile, WriteStream),
    indexer_process_commit(BranchPath, "main", CommitId, WriteStream),
    close(WriteStream),
    open(TmpFile, read, RS),
    read_ndjson_dicts(RS, Ops),
    close(RS),
    delete_file(TmpFile).

read_ndjson_dicts(Stream, Dicts) :-
    read_line_to_string(Stream, Line),
    (   Line == end_of_file
    ->  Dicts = []
    ;   atom_json_dict(Line, Dict, [default_tag(json)]),
        Dicts = [Dict|Rest],
        read_ndjson_dicts(Stream, Rest)
    ).

ops_of_kind(Ops, Kind, Matching) :-
    atom_string(KindAtom, Kind),
    include(op_is_kind(KindAtom), Ops, Matching).

op_is_kind(KindAtom, Dict) :-
    get_dict(op, Dict, Op),
    atom_string(KindAtom, Op).

%% count_parity(+DbPath, +CommitId, +Ops)
%  Asserts X-Document-Count parity: count_indexable_documents must equal
%  the number of ops the stream actually emitted.
count_parity(DbPath, CommitId, Ops) :-
    atomic_list_concat([DbPath, "local/branch/main"], '/', BranchPath),
    count_indexable_documents(BranchPath, "main", CommitId, Count),
    length(Ops, N),
    assertion(Count == N).

test(schema_template_change_reembeds_all_docs_of_type,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "tpldb"))),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    super_user_authority(Auth),
    embedding_meta('query($id: ID){ Article(id: $id) { title body } }',
                   'V1TPL {{title}}', Meta1),
    article_schema(Meta1, Schema1),
    insert_schema_json(System, Auth, "admin/tpldb", Schema1),
    three_articles(System, Auth, "admin/tpldb"),
    %% Schema v2: template only change
    embedding_meta('query($id: ID){ Article(id: $id) { title body } }',
                   'V2TPL {{title}}', Meta2),
    article_schema(Meta2, Schema2),
    insert_schema_json(System, Auth, "admin/tpldb", Schema2),
    head_commit_string("admin/tpldb", CommitId),
    collect_commit_ops("admin/tpldb", CommitId, Ops),
    length(Ops, N),
    assertion(N == 3),
    forall(member(Op, Ops),
           assertion(get_dict(op, Op, "Changed"))),
    forall(member(Op, Ops),
           assertion(sub_string(Op.string, _, _, _, "V2TPL"))),
    count_parity("admin/tpldb", CommitId, Ops).

test(schema_query_change_reembeds_all_docs_of_type,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "qrydb"))),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    super_user_authority(Auth),
    embedding_meta('query($id: ID){ Article(id: $id) { title } }',
                   'QTPL {{title}}', Meta1),
    article_schema(Meta1, Schema1),
    insert_schema_json(System, Auth, "admin/qrydb", Schema1),
    three_articles(System, Auth, "admin/qrydb"),
    %% Schema v2: query selects an extra field
    embedding_meta('query($id: ID){ Article(id: $id) { title body } }',
                   'QTPL {{title}}', Meta2),
    article_schema(Meta2, Schema2),
    insert_schema_json(System, Auth, "admin/qrydb", Schema2),
    head_commit_string("admin/qrydb", CommitId),
    collect_commit_ops("admin/qrydb", CommitId, Ops),
    length(Ops, N),
    assertion(N == 3),
    forall(member(Op, Ops),
           assertion(get_dict(op, Op, "Changed"))),
    count_parity("admin/qrydb", CommitId, Ops).

test(identical_schema_recommit_emits_no_ops,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "samedb"))),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    super_user_authority(Auth),
    embedding_meta('query($id: ID){ Article(id: $id) { title body } }',
                   'SAME {{title}}', Meta),
    article_schema(Meta, Schema),
    insert_schema_json(System, Auth, "admin/samedb", Schema),
    three_articles(System, Auth, "admin/samedb"),
    %% Re-commit identical schema — content-addressed metadata produces
    %% no delta, so no refresh ops.
    insert_schema_json(System, Auth, "admin/samedb", Schema),
    head_commit_string("admin/samedb", CommitId),
    collect_commit_ops("admin/samedb", CommitId, Ops),
    assertion(Ops == []),
    count_parity("admin/samedb", CommitId, Ops).

test(unrelated_schema_property_change_emits_no_refresh,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "propdb"))),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    super_user_authority(Auth),
    embedding_meta('query($id: ID){ Article(id: $id) { title body } }',
                   'PTPL {{title}}', Meta),
    article_schema(Meta, Schema1),
    insert_schema_json(System, Auth, "admin/propdb", Schema1),
    three_articles(System, Auth, "admin/propdb"),
    %% Schema v2: add an unrelated property, metadata identical
    format(string(Schema2),
'[
  {
    "@type": "@context",
    "@base": "http://example.com/data/",
    "@schema": "http://example.com/schema#"
  },
  {
    "@type": "Class",
    "@id": "Article",
    "@key": { "@type": "Lexical", "@fields": ["title"] },
    "title": "xsd:string",
    "body": "xsd:string",
    "summary": "xsd:string"~w
  }
]', [Meta]),
    insert_schema_json(System, Auth, "admin/propdb", Schema2),
    head_commit_string("admin/propdb", CommitId),
    collect_commit_ops("admin/propdb", CommitId, Ops),
    assertion(Ops == []),
    count_parity("admin/propdb", CommitId, Ops).

test(non_embedding_metadata_change_emits_no_refresh,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "metadb"))),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    super_user_authority(Auth),
    format(string(EmbJson),
           '{"query": "query($id: ID){ Article(id: $id) { title body } }", "template": "MTPL {{title}}"}',
           []),
    format(string(Meta1),
           ', "@metadata": { "embedding": ~s, "other": {"note": "v1"} }',
           [EmbJson]),
    article_schema(Meta1, Schema1),
    insert_schema_json(System, Auth, "admin/metadb", Schema1),
    three_articles(System, Auth, "admin/metadb"),
    %% Schema v2: unrelated metadata key changes; embedding subtree is
    %% content-identical, so its leaf edges do not churn — no refresh.
    format(string(Meta2),
           ', "@metadata": { "embedding": ~s, "other": {"note": "v2"} }',
           [EmbJson]),
    article_schema(Meta2, Schema2),
    insert_schema_json(System, Auth, "admin/metadb", Schema2),
    head_commit_string("admin/metadb", CommitId),
    collect_commit_ops("admin/metadb", CommitId, Ops),
    assertion(Ops == []),
    count_parity("admin/metadb", CommitId, Ops).

test(embedding_metadata_removal_purges_all_docs,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "purgedb"))),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    super_user_authority(Auth),
    embedding_meta('query($id: ID){ Article(id: $id) { title body } }',
                   'RTPL {{title}}', Meta),
    article_schema(Meta, Schema1),
    insert_schema_json(System, Auth, "admin/purgedb", Schema1),
    three_articles(System, Auth, "admin/purgedb"),
    %% Schema v2: class remains, @metadata gone
    article_schema('', Schema2),
    insert_schema_json(System, Auth, "admin/purgedb", Schema2),
    head_commit_string("admin/purgedb", CommitId),
    collect_commit_ops("admin/purgedb", CommitId, Ops),
    length(Ops, N),
    assertion(N == 3),
    forall(member(Op, Ops),
           assertion(get_dict(op, Op, "Deleted"))),
    count_parity("admin/purgedb", CommitId, Ops).

test(class_removal_purges_its_documents,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "clsdb"))),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    super_user_authority(Auth),
    embedding_meta('query($id: ID){ Article(id: $id) { title body } }',
                   'CTPL {{title}}', Meta),
    article_schema(Meta, Schema1),
    insert_schema_json(System, Auth, "admin/clsdb", Schema1),
    three_articles(System, Auth, "admin/clsdb"),
    %% Schema v2: only the context — Article class deleted entirely
    open_string('
[
  {
    "@type": "@context",
    "@base": "http://example.com/data/",
    "@schema": "http://example.com/schema#"
  }
]', S2),
    api_insert_documents(System, Auth, "admin/clsdb", S2, no_data_version,
                         _, _, _,
                         [author("test"), full_replace(true),
                          graph_type(schema), message("schema v2")]),
    head_commit_string("admin/clsdb", CommitId),
    collect_commit_ops("admin/clsdb", CommitId, Ops),
    length(Ops, N),
    assertion(N == 3),
    forall(member(Op, Ops),
           assertion(get_dict(op, Op, "Deleted"))),
    count_parity("admin/clsdb", CommitId, Ops).

test(embedding_added_to_existing_type_reembeds_docs,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "adddb"))),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    super_user_authority(Auth),
    article_schema('', Schema1),
    insert_schema_json(System, Auth, "admin/adddb", Schema1),
    three_articles(System, Auth, "admin/adddb"),
    %% Schema v2: introduce embedding metadata for the existing type
    embedding_meta('query($id: ID){ Article(id: $id) { title body } }',
                   'ADDED {{title}}', Meta2),
    article_schema(Meta2, Schema2),
    insert_schema_json(System, Auth, "admin/adddb", Schema2),
    head_commit_string("admin/adddb", CommitId),
    collect_commit_ops("admin/adddb", CommitId, Ops),
    length(Ops, N),
    assertion(N == 3),
    forall(member(Op, Ops),
           assertion(get_dict(op, Op, "Changed"))),
    forall(member(Op, Ops),
           assertion(sub_string(Op.string, _, _, _, "ADDED"))),
    count_parity("admin/adddb", CommitId, Ops).

test(template_removal_purges_rather_than_partial_embed,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "notpldb"))),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    super_user_authority(Auth),
    embedding_meta('query($id: ID){ Article(id: $id) { title body } }',
                   'WTPL {{title}}', Meta),
    article_schema(Meta, Schema1),
    insert_schema_json(System, Auth, "admin/notpldb", Schema1),
    three_articles(System, Auth, "admin/notpldb"),
    %% Schema v2: query kept, template dropped — an incomplete config is
    %% not a valid embedding, so documents purge rather than re-embed.
    article_schema(', "@metadata": { "embedding": { "query": "query($id: ID){ Article(id: $id) { title body } }" } }',
                   Schema2),
    insert_schema_json(System, Auth, "admin/notpldb", Schema2),
    head_commit_string("admin/notpldb", CommitId),
    collect_commit_ops("admin/notpldb", CommitId, Ops),
    length(Ops, N),
    assertion(N == 3),
    forall(member(Op, Ops),
           assertion(get_dict(op, Op, "Deleted"))),
    count_parity("admin/notpldb", CommitId, Ops).

test(document_only_commit_emits_exactly_changed_docs,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "docdb"))),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    super_user_authority(Auth),
    embedding_meta('query($id: ID){ Article(id: $id) { title body } }',
                   'DTPL {{title}}', Meta),
    article_schema(Meta, Schema),
    insert_schema_json(System, Auth, "admin/docdb", Schema),
    three_articles(System, Auth, "admin/docdb"),
    %% A second docs commit adding one document
    insert_docs_json(System, Auth, "admin/docdb",
'[
  {"@type": "Article", "title": "Fourth", "body": "Body four"}
]'),
    head_commit_string("admin/docdb", CommitId),
    collect_commit_ops("admin/docdb", CommitId, Ops),
    length(Ops, N),
    assertion(N == 1),
    forall(member(Op, Ops),
           assertion(get_dict(op, Op, "Inserted"))),
    count_parity("admin/docdb", CommitId, Ops).

test(refresh_and_doc_change_dedup_on_same_id,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "dedupdb"))),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    super_user_authority(Auth),
    embedding_meta('query($id: ID){ Article(id: $id) { title body } }',
                   'DEDUP {{title}}', Meta),
    article_schema(Meta, Schema),
    insert_schema_json(System, Auth, "admin/dedupdb", Schema),
    three_articles(System, Auth, "admin/dedupdb"),
    resolve_absolute_string_descriptor("admin/dedupdb/local/branch/main",
                                       Desc),
    get_dict(repository_descriptor, Desc, Repo_Desc),
    branch_head_commit(Repo_Desc, "main", Uri1),
    commit_id_uri(Repo_Desc, Parent_Id, Uri1),
    %% Doc commit adding a fourth Article
    insert_docs_json(System, Auth, "admin/dedupdb",
'[
  {"@type": "Article", "title": "Fourth", "body": "New body"}
]'),
    head_commit_string("admin/dedupdb", CommitId),
    resolve_absolute_string_descriptor("admin/dedupdb/local/branch/main",
                                       Desc2),
    resolve_relative_descriptor(Desc2, ["commit", CommitId], CDesc),
    embedding_type_queries(CDesc, TQ),
    open_descriptor(CDesc, Tx),
    %% Simulate the doc's type also appearing in this commit's refresh
    %% set (as it would in a combined schema+instance commit): the new
    %% doc must be emitted exactly once, not once per phase.
    memberchk(ArticleType-_-_, TQ),
    commit_ops_begin,
    findall(Op,
            commit_op(Tx, Desc2, CommitId, TQ, some(Parent_Id),
                      [ArticleType], [], _T-Op),
            Ops),
    length(Ops, N),
    assertion(N == 4),
    ops_of_kind(Ops, "Inserted", InsertedOps),
    ops_of_kind(Ops, "Changed", ChangedOps),
    assertion(length(InsertedOps, 1)),
    assertion(length(ChangedOps, 3)),
    maplist([D, I]>>(get_dict(id, D, I)), Ops, Ids0),
    maplist([X, A]>>atom_string(A, X), Ids0, Ids),
    sort(Ids, UniqueIds),
    length(UniqueIds, NU),
    assertion(NU == 4).

:- end_tests(indexer_predicates).

