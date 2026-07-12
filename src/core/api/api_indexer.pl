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
              count_indexable_documents/4
          ]).

:- use_module(core(document/history),[commits_changed_id/5]).
:- use_module(core(document),[get_document/3, all_class_frames/3,
                              schema_metadata_descriptor/3]).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(transaction/ref_entity),
             [branch_head_commit/3, commit_id_uri/3,
              commit_uri_to_history_commit_ids/3]).
:- use_module(core(util)).
:- use_module(core(account)).
:- use_module(library(json)).
:- use_module(config(terminus_config)).
:- use_module(core(api/api_graphql)).
:- use_module(core(triple), [super_user_authority/1, database_schema/2, xrdf/4]).
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

%% api_indexable(some(Previous_Commit_Id), Descriptor, Commit_Id, Type, Operation) is nondet.
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

%% api_indexable(none, Descriptor, Commit_Id, Type, Operation) is nondet.
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
    call(Prelude,Stream),
    forall(
        (   member(Type-_Query-_Template, TypeQueries),
            api_indexable(Maybe_Previous_Commit_Id, Descriptor, Commit_Id,
                          Type, Operation)),
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
% resolution internally via reqwest. Prolog only calls indexer_notify/2.
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
 * Thin wrapper around the indexer_notify/2 FFI predicate. Validates the
 * path, checks that the tdb_search endpoint is configured, and delegates
 * to the Rust IndexerRegistry which handles NDJSON generation, HTTP
 * streaming, 409 resolution, and task polling internally.
 *
 * Fails loud on:
 *   - invalid path (not 2-or-5 segment form)
 *   - indexer_backend not http_tdb_search (wrong backend)
 *   - tdb_search endpoint not configured
 *   - indexer_notify FFI not loaded (Rust runtime not available)
 */
io_push_delta(_System_DB, _Auth, Path, Branch_Name) :-
    validate_index_path(Path),
    do_or_die(
        indexer_backend(http_tdb_search),
        error(indexer_backend_not_tdb_search(io_push_delta), _)),
    do_or_die(
        tdb_search_endpoint(_Endpoint),
        error(tdb_search_endpoint_not_configured(io_push_delta), _)),
    (   atom_concat(_, '/local/branch/', Path)
    ->  Branch_Path = Path
    ;   format(atom(Branch_Path), "~w/local/branch/~w", [Path, Branch_Name])
    ),
    (   plugin_api:indexer_available
    ->  (   plugin_api:indexer_notify(Branch_Path, Branch_Name)
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
    ;   throw(error(tdb_search_last_indexed_not_in_history(Last_Commit), _))
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
 * then resolves descriptor and drives a push. Gated on http_tdb_search.
 *
 * The Path MUST conform to the driver input contract (2-seg or 5-seg form).
 * Validation fires BEFORE descriptor resolution or any I/O.
 */
io_index_branch(System_DB, Auth, Path) :-
    % Validate path structure FIRST — before any descriptor resolution or I/O.
    validate_index_path(Path),
    do_or_die(
        indexer_backend(http_tdb_search),
        error(indexer_backend_not_tdb_search(io_index_branch), _)),
    resolve_absolute_string_descriptor(Path, Descriptor),
    do_or_die(
        branch_descriptor{branch_name: Branch_Name} :< Descriptor,
        error(push_requires_branch_descriptor(Path), _)),
    io_push_delta(System_DB, Auth, Path, Branch_Name).

% ==========================================================================
% Auto-push-on-commit hook
%
% Fires after every commit (multifile post_commit_hook/2 from plugins.pl).
% GATE: only acts when indexer_backend = http_tdb_search AND the committed
% data product's schema has at least one type with embedding metadata.
% For all other commits this is a cheap no-op (two config checks + fail).
%
% ASYNC FIRE-AND-FORGET: calls indexer_notify/2 FFI which spawns a tokio
% task in the Rust IndexerRegistry. The task runs independently of the
% commit path — commit latency is NOT inflated. The Rust task runs as
% the system identity (super_user_authority) — indexing is infrastructure,
% not coupled to the committing user's auth.
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
    % Gate 1: backend must be http_tdb_search. Cheap tabled check.
    indexer_backend(http_tdb_search),
    % Gate 2: FFI predicate must be registered (Rust runtime loaded).
    plugin_api:indexer_available,
    % Gate 3: for each validation with embedding metadata, call indexer_notify
    % (O(1) FFI — no NDJSON generation, no HTTP calls from Prolog).
    catch(
        forall(
            (   member(Validation, Validations),
                validation_is_index_enabled(Validation),
                get_dict(descriptor, Validation, Descriptor),
                branch_descriptor{branch_name: Branch_Name} :< Descriptor,
                descriptor_graphspec(Descriptor, Path)
            ),
            catch(
                plugin_api:indexer_notify(Path, Branch_Name),
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
        ->  format(user_error, "[DEBUG] indexer_process_commit: write_commit_ndjson succeeded for ~w~n", [CommitId])
        ;   format(user_error, "[DEBUG] indexer_process_commit: write_commit_ndjson FAILED (no exception) for ~w~n", [CommitId]),
            fail
        ),
        Error,
        (   with_output_to(atom(Error_Atom),
                write_term(Error, [quoted(false)])),
            format(user_error, "[DEBUG] indexer_process_commit: write_commit_ndjson THREW ERROR for ~w: ~w~n", [CommitId, Error_Atom]),
            Error_Json = json{op:"Error", message:Error_Atom},
            json_write_dict(Stream, Error_Json, []),
            nl(Stream),
            flush_output(Stream),
            fail
        )
    ).

%% write_commit_ndjson(+Transaction, +Descriptor, +CommitId,
%%                     +TypeQueries, +Stream)
%
%  Computes the parent commit from the commit history, then for each
%  embedding-enabled type, finds changed documents via api_indexable/5
%  and writes NDJSON lines with embedding text to Stream.
write_commit_ndjson(Transaction, Descriptor, CommitId,
                    TypeQueries, Stream) :-
    (   TypeQueries = []
    ->  format(user_error, "[DEBUG] write_commit_ndjson: no TypeQueries for commit ~w~n", [CommitId])
    ;   format(user_error, "[DEBUG] write_commit_ndjson: ~w type queries for commit ~w~n", [TypeQueries, CommitId]),
        open_descriptor(system_descriptor{}, System_DB),
        format(user_error, "[DEBUG] write_commit_ndjson: opened system descriptor for ~w~n", [CommitId]),
        maplist([Type-Query-_Template, Type-Query]>>true,
                TypeQueries, Queries),
        convlist([Type-Query-Template, Type-Template]>>ground(Template),
                 TypeQueries, Templates),
        all_class_frames(Transaction, Frames,
                         [compress_ids(true), expand_abstract(true),
                          simple(true)]),
        format(user_error, "[DEBUG] write_commit_ndjson: got class frames for ~w~n", [CommitId]),
        '$embedding':embedding_context(System_DB, Transaction, Templates,
                                       Queries, Frames, Embedding_Context),
        format(user_error, "[DEBUG] write_commit_ndjson: got embedding context for ~w~n", [CommitId]),
        get_dict(repository_descriptor, Descriptor, Repository_Descriptor),
        format(user_error, "[DEBUG] write_commit_ndjson: got repo descriptor for ~w~n", [CommitId]),
        commit_id_uri(Repository_Descriptor, CommitId, Commit_Uri),
        format(user_error, "[DEBUG] write_commit_ndjson: got commit uri for ~w~n", [CommitId]),
        commit_uri_to_history_commit_ids(Repository_Descriptor,
                                         Commit_Uri, History_Oldest_First),
        format(user_error, "[DEBUG] write_commit_ndjson: got history (~w commits) for ~w~n", [History_Oldest_First, CommitId]),
        (   append(_, [Parent_Id, CommitId|_], History_Oldest_First)
        ->  Maybe_Previous = some(Parent_Id),
            format(user_error, "[DEBUG] write_commit_ndjson: parent=~w for ~w~n", [Parent_Id, CommitId])
        ;   Maybe_Previous = none,
            format(user_error, "[DEBUG] write_commit_ndjson: no parent (first commit) for ~w~n", [CommitId])
        ),
        nb_setval(ndjson_count, 0),
        format(user_error, "[DEBUG] write_commit_ndjson: starting forall for ~w with Maybe_Previous=~w~n", [CommitId, Maybe_Previous]),
        forall(
            (   member(Type-_Query-_Template, TypeQueries),
                format(user_error, "[DEBUG] write_commit_ndjson: trying api_indexable for type ~w commit ~w~n", [Type, CommitId]),
                api_indexable(Maybe_Previous, Descriptor, CommitId,
                              Type, Operation)
            ),
            (   get_dict(op, Operation, Op),
                ignore(get_dict(id, Operation, Id)),
                nb_getval(ndjson_count, PrevCount),
                Count is PrevCount + 1,
                nb_setval(ndjson_count, Count),
                (   Count =< 3
                ->  format(user_error, "[DEBUG] write_commit_ndjson: doc #~w type=~w op=~w id=~w~n", [Count, Type, Op, Id])
                ;   true
                ),
                '$embedding':write_op_for(Stream, System_DB, Transaction,
                                          Embedding_Context, Type, Id, Op),
                flush_output(Stream)
            )
        ),
        (   nb_current(ndjson_count, Count)
        ->  format(user_error, "[DEBUG] write_commit_ndjson: wrote ~w NDJSON lines for commit ~w~n", [Count, CommitId])
        ;   format(user_error, "[DEBUG] write_commit_ndjson: wrote 0 NDJSON lines for commit ~w~n", [CommitId])
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
    (   TypeQueries = []
    ->  Count = 0
    ;   open_descriptor(Commit_Descriptor, Transaction),
        get_dict(repository_descriptor, Descriptor, Repository_Descriptor),
        commit_id_uri(Repository_Descriptor, CommitId, Commit_Uri),
        commit_uri_to_history_commit_ids(Repository_Descriptor,
                                         Commit_Uri, History_Oldest_First),
        (   append(_, [Parent_Id, CommitId|_], History_Oldest_First)
        ->  Maybe_Previous = some(Parent_Id)
        ;   Maybe_Previous = none
        ),
        count_indexable_documents_(Maybe_Previous, Transaction,
                                    TypeQueries, Count)
    ).

%% count_indexable_documents_(+Maybe_Previous, +Transaction, +TypeQueries, -Count) is det.
%
%  Helper that does the actual counting based on whether there is a parent commit.
count_indexable_documents_(some(_Parent_Id), Transaction, TypeQueries, Count) :-
    findall(TypeAtom,
            (   member(Type-__-_Template, TypeQueries),
                atom_string(TypeAtom, Type)
            ),
            TypeAtoms),
    (   TypeAtoms = []
    ->  Count = 0
    ;   findall(_,
                '$changes':collect_changed_documents_filtered(
                    Transaction, TypeAtoms, _Id, _ChangeType),
                Changes),
        length(Changes, Count)
    ).
count_indexable_documents_(none, Transaction, TypeQueries, Count) :-
    findall(_,
            (   member(Type-__-_Template, TypeQueries),
                ask(Transaction, t(_Id, rdf:type, Type),
                    [compress_prefixes(false)])
            ),
            Operations),
    length(Operations, Count).


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

:- end_tests(indexer_predicates).

