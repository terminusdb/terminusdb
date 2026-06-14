:- module(api_indexer, [
              api_start_job/3,
              api_check_job/2,
              %api_index/5,
              %api_query/5,
              api_index_jobs/8,
              io_push_delta/4,
              io_index_branch/3,
              descriptor_graphspec/2
          ]).

:- use_module(core(document/history),[commits_changed_id/5]).
:- use_module(core(document),[get_document/3, all_class_frames/3]).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(transaction/ref_entity),
             [branch_head_commit/3, commit_id_uri/3,
              commit_uri_to_history_commit_ids/3]).
:- use_module(core(util)).
:- use_module(core(account)).
:- use_module(library(http/json)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_stream)).
:- use_module(config(terminus_config)).
:- use_module(core(api/api_graphql)).
:- use_module(core(triple), [super_user_authority/1]).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).
:- use_module(library(yall)).
:- use_module(library(lists)).
:- use_module(library(dicts)).

% api_start_job(+Domain:string,+Commit:string,-Task_id, +Options) is det.
% Legacy pull trigger. Only active under the http_vectorlink backend; refuses
% loud if the selector is not http_vectorlink so the legacy and push paths can
% never both run (RISK-17 / Spec 16 §2.3).
api_start_job(Domain, Commit, Task_Id) :-
    do_or_die(config:indexer_backend(http_vectorlink),
              error(indexer_backend_not_vectorlink(api_start_job), _)),
    config:semantic_indexer_endpoint(Endpoint),
    http_get(
        [ host(Endpoint),
          path('/start'),
          search([ domain=Domain,
                   commit=Commit])],
        Task_Id,
        []).

api_check_job(Task_Id, Status) :-
    do_or_die(config:indexer_backend(http_vectorlink),
              error(indexer_backend_not_vectorlink(api_check_job), _)),
    config:semantic_indexer_endpoint(Endpoint),
    http_get(
        [ host(Endpoint),
          path('/check'),
          search([ task_id=Task_Id ])],
        Status,
        []).

embedding_type_queries(Commit_Descriptor, TypeQueries) :-
    findall(
        Type-Query-Template,
        (   ask(Commit_Descriptor,
                (   t(Embedding, json:query,  Query_Point, schema),
                    opt(t(Embedding, json:template, Template_Point, schema)),
                    t(Meta, json:embedding,  Embedding, schema),
                    t(Type, sys:metadata, Meta, schema))),
            Query_Point = Query^^xsd:string,
            (   ground(Template_Point)
            ->  Template_Point = Template^^_
            ;   true)),
        TypeQueries
    ).

api_indexable(some(Previous_Commit_Id), Descriptor, Commit_Id, Type, Operation) :-
    commits_changed_id(Descriptor, Previous_Commit_Id, Commit_Id, Id,
                       options{ type: Type }),
    resolve_relative_descriptor(Descriptor,
                                ["commit", Commit_Id],
                                After_Commit_Descriptor),
    resolve_relative_descriptor(Descriptor,
                                ["commit", Previous_Commit_Id],
                                Before_Commit_Descriptor),

    (   ask(After_Commit_Descriptor,
            t(Id, rdf:type, _))
    ->  (   ask(Before_Commit_Descriptor,
                t(Id, rdf:type, _))
        ->  Operation = json{ op: 'Changed',
                              id: Id }
        ;   Operation = json{ op: 'Inserted',
                              id: Id }
        )
    ;   Operation = json{ op: 'Deleted',
                          id: Id }
    ).
api_indexable(none, Descriptor, Commit_Id, Type, Operation) :-
    resolve_relative_descriptor(Descriptor,
                                ["commit", Commit_Id],
                                Commit_Descriptor),
    ask(Commit_Descriptor, t(Id, rdf:type, Type),[compress_prefixes(false)]),
    Operation = json{ op: 'Inserted',
                      id: Id }.

/* predicate which returns the various jobs as op/string:
{ "op" : "Inserted", "id" : "Doc/1", "string" : "this is in doc 1" }
{ "op" : "Changed", "id" : "Doc/2", "string" : "this is new in doc 2" }
{ "op" : "Deleted", "id" : "Doc/3"}
*/
:- meta_predicate api_index_jobs(+, +, +, 1, +, +, +, +).
api_index_jobs(System_DB, Auth, Stream, Prelude, Path, Commit_Id, Maybe_Previous_Commit_Id, _Options) :-
    do_or_die(
        is_super_user(Auth),
        error(indexing_requires_superuser)
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
        /*
            (   member(Op, ['Inserted', 'Changed'])
            ->  (   get_dict(id, Operation, Id),
                    '$embedding':embedding_string_for(System_DB, Transaction, Embedding_Context, Type, Id, Embedding_String),
                    put_dict(_{string : Embedding_String }, Operation, Final_Operation),
                    atom_json_dict(Operation_Atom, Final_Operation, [width(0)]),
                    write(Stream, Operation_Atom),
                    nl(Stream)
                ;   throw(error(some_terrible_error, _)),
                    get_dict(id, Operation, Id),
                    format(Stream, '{ "op" : "Error", "message" : "Failed to process embedding operation for id ~s"}~n',
                           [Id])
                )
            ;   atom_json_dict(Operation_Atom, Operation, [width(0)]),
                write(Stream, Operation_Atom),
                nl(Stream)
            )
        */
        )
    ).

% ==========================================================================
% Phase 6 T3 — Push driver (Capability A)
%
% TerminusDB DRIVES incremental indexing INTO the tdb-search engine.
% Protocol: GET /last-indexed -> compute delta -> POST /push (NDJSON stream).
% Gated on indexer_backend(http_tdb_search). Uses the T1-proven chunked
% streaming mechanism (http:post_data_hook/3 + http_chunked_open/3).
% ==========================================================================

:- multifile http:post_data_hook/3.

% ---- post_data_hook for lazy NDJSON streaming (T1-proven mechanism) ----
% The data term ndjson_push(Goal) carries a goal that accepts a stream arg
% and writes NDJSON lines to it. The hook sends them chunked without
% buffering the body.
http:post_data_hook(ndjson_push(Producer), Out, _HdrExtra) :-
    raw_out_stream(Out, RawOut),
    format(RawOut, "Content-Type: application/x-ndjson\r\n", []),
    format(RawOut, "Transfer-Encoding: chunked\r\n", []),
    format(RawOut, "\r\n", []),
    flush_output(RawOut),
    setup_call_cleanup(
        http_chunked_open(RawOut, Chunked, []),
        call(Producer, Chunked),
        close(Chunked)).

% Resolve a stream pair to its output side (avoids "ambiguous operation on
% stream pair" from http_open's returned pair).
raw_out_stream(Stream, Out) :-
    (   is_stream(Stream),
        stream_pair(Stream, _In, PairOut),
        PairOut \== []
    ->  Out = PairOut
    ;   Out = Stream
    ).

% ---- HTTP Basic auth header for tdb-search calls ----
tdb_search_auth_header(authorization(basic(User, Secret))) :-
    tdb_search_admin_user(User),
    tdb_search_admin_secret(Secret).

/**
 * io_get_last_indexed(+Endpoint, +Domain, +Branch, -Result) is det.
 *
 * Calls GET /last-indexed?domain=Domain&branch=Branch on the tdb-search engine.
 * Result is a dict with keys `commit` (atom or null) and `version` (integer).
 * Fails loud on any non-200 response or connection error.
 */
io_get_last_indexed(Endpoint, Domain, Branch, Result) :-
    tdb_search_auth_header(AuthHeader),
    atomic_list_concat([Endpoint, "/last-indexed"], URL_Base),
    format(atom(URL), "~w?domain=~w&branch=~w", [URL_Base, Domain, Branch]),
    setup_call_cleanup(
        http_open(URL, In,
                  [ status_code(Status),
                    AuthHeader,
                    request_header('Accept' = 'application/json')
                  ]),
        ( read_string(In, _, Body_String) ),
        close(In)),
    do_or_die(
        Status =:= 200,
        error(tdb_search_last_indexed_failed(Status, Body_String), _)),
    atom_json_dict(Body_String, Result, [default_tag(json)]).

/**
 * io_stream_push(+Endpoint, +Domain, +Branch, +Target_Commit,
 *                +Parent_Commit_Or_Empty, +System_DB, +Auth, +Path,
 *                +Maybe_Previous_Commit_Id, +Commit_Id, -Task_Id) is det.
 *
 * Opens POST /push to the engine and streams the NDJSON delta using the
 * T1-proven chunked mechanism. The op-lines are produced lazily by
 * api_index_jobs/8 writing directly to the chunked HTTP stream.
 * Parent_Commit_Or_Empty is either an atom (the parent commit hash) or
 * the atom `none` (for a full initial index — omit parent_commit param).
 */
io_stream_push(Endpoint, Domain, Branch, Target_Commit,
               Parent_Commit_Or_Empty, System_DB, Auth, Path,
               Maybe_Previous_Commit_Id, Commit_Id, Task_Id) :-
    tdb_search_auth_header(AuthHeader),
    build_push_url(Endpoint, Domain, Branch, Target_Commit,
                   Parent_Commit_Or_Empty, URL),
    % The Producer goal is called by ndjson_push with the chunked stream.
    % It runs api_index_jobs/8 which writes op-lines to that stream directly.
    Producer = [Chunked_Stream]>>(
        api_index_jobs(
            System_DB,
            Auth,
            Chunked_Stream,
            [_S]>>true,  % no prelude — raw NDJSON body, no HTTP headers
            Path,
            Commit_Id,
            Maybe_Previous_Commit_Id,
            [])
    ),
    setup_call_cleanup(
        http_open(URL, In,
                  [ method(post),
                    post(ndjson_push(Producer)),
                    status_code(Status),
                    AuthHeader
                  ]),
        read_string(In, _, Reply_Body),
        close(In)),
    handle_push_response(Status, Reply_Body, Task_Id).

handle_push_response(200, Task_Id, Task_Id) :- !.
handle_push_response(Status, Body, _) :-
    throw(error(tdb_search_push_failed(Status, Body), _)).

% Build the POST /push URL with query parameters.
build_push_url(Endpoint, Domain, Branch, Target_Commit, none, URL) :-
    !,
    format(atom(URL),
           "~w/push?domain=~w&branch=~w&target_commit=~w",
           [Endpoint, Domain, Branch, Target_Commit]).
build_push_url(Endpoint, Domain, Branch, Target_Commit, Parent_Commit, URL) :-
    format(atom(URL),
           "~w/push?domain=~w&branch=~w&target_commit=~w&parent_commit=~w",
           [Endpoint, Domain, Branch, Target_Commit, Parent_Commit]).

/**
 * io_push_delta(+System_DB, +Auth, +Path, +Branch_Name) is det.
 *
 * The push driver entrypoint. Gated on indexer_backend(http_tdb_search).
 * Asks the engine for its last-indexed state, resolves the branch HEAD,
 * and pushes each commit individually in oldest-first order so that
 * every commit is tagged on the engine (restart-safe, resume-forward).
 *
 * Strategy:
 *   - null last (fresh index): push a single `none` diff of HEAD (full index
 *     of the current state — no intermediate history to preserve).
 *   - incremental: walk the commit chain from last (exclusive) to HEAD
 *     (inclusive), oldest-first. For each commit c_i: push the delta
 *     from c_{i-1} to c_i, tagging c_i on the engine.
 *   - already at HEAD: nothing to push.
 *
 * Fails loud on:
 *   - indexer_backend not http_tdb_search (wrong backend)
 *   - engine unreachable or returns non-200 on /last-indexed
 *   - engine rejects the push (4xx/5xx on /push)
 *   - wrong admin secret (401 from engine)
 */
io_push_delta(System_DB, Auth, Path, Branch_Name) :-
    do_or_die(
        indexer_backend(http_tdb_search),
        error(indexer_backend_not_tdb_search(io_push_delta), _)),
    do_or_die(
        tdb_search_endpoint(Endpoint),
        error(tdb_search_endpoint_not_configured(io_push_delta), _)),
    % Resolve the descriptor FIRST — this is the single source of truth.
    resolve_absolute_string_descriptor(Path, Descriptor),
    % Derive the full graphspec from the resolved descriptor. This produces
    % the canonical path form (org/db/repo/branch/<name>) that the engine
    % parses and validates via its `parse_domain` function. Using the
    % descriptor as source ensures graphspec and branch cannot disagree.
    descriptor_graphspec(Descriptor, Domain),
    % Extract the branch name from the descriptor and verify consistency
    % with the caller's Branch_Name (poka-yoke: impossible to disagree).
    do_or_die(
        branch_descriptor{branch_name: Descriptor_Branch} :< Descriptor,
        error(push_requires_branch_descriptor(Path), _)),
    do_or_die(
        Descriptor_Branch == Branch_Name,
        error(branch_name_mismatch(Branch_Name, Descriptor_Branch, Path), _)),
    % Ask the engine where it is up to.
    io_get_last_indexed(Endpoint, Domain, Branch_Name, Last_Indexed),
    get_dict(commit, Last_Indexed, Engine_Commit_Raw),
    % Normalise: atom_json_dict yields atoms for JSON strings; commit IDs
    % from TerminusDB are Prolog strings. Coerce to string for comparison.
    normalise_commit_value(Engine_Commit_Raw, Engine_Commit_Or_Null),
    % Resolve the branch HEAD commit in TerminusDB.
    Repository_Descriptor = Descriptor.repository_descriptor,
    branch_head_commit(Repository_Descriptor, Branch_Name, Head_Commit_Uri),
    commit_id_uri(Repository_Descriptor, Head_Commit_Id, Head_Commit_Uri),
    % Dispatch based on engine state.
    io_push_delta_(Endpoint, Domain, Branch_Name, Head_Commit_Id,
                   Head_Commit_Uri, Engine_Commit_Or_Null,
                   Repository_Descriptor, System_DB, Auth, Path).

% Normalise a commit value from JSON: null stays as the atom `null`;
% a JSON string (which atom_json_dict yields as an atom) is coerced to a
% Prolog string for consistent comparison with commit IDs from the triple
% store (which are Prolog strings via Commit_Id^^xsd:string).
normalise_commit_value(@(null), null) :- !.
normalise_commit_value(null, null) :- !.
normalise_commit_value(Atom, String) :-
    atom(Atom),
    !,
    atom_string(Atom, String).
normalise_commit_value(String, String) :-
    string(String).

% Case 1: engine is already at HEAD — nothing to push.
io_push_delta_(_Endpoint, _Domain, _Branch_Name, Head_Commit_Id,
               _Head_Commit_Uri, Engine_Commit,
               _Repository_Descriptor, _System_DB, _Auth, _Path) :-
    Engine_Commit \== null,
    Engine_Commit == Head_Commit_Id,
    !.  % nothing to push — already up to date

% Case 2: engine has never indexed this branch (commit is null) — full index
% of HEAD. On a fresh index there is no intermediate history to preserve;
% we push a single none-diff of head to establish the baseline.
io_push_delta_(Endpoint, Domain, Branch_Name, Head_Commit_Id,
               _Head_Commit_Uri, null,
               _Repository_Descriptor, System_DB, Auth, Path) :-
    !,
    io_stream_push(Endpoint, Domain, Branch_Name, Head_Commit_Id,
                   none, System_DB, Auth, Path,
                   none, Head_Commit_Id, _Task_Id).

% Case 3: engine has a previous commit — per-commit incremental push.
% Walk the commit chain from HEAD back to root (oldest-first after the
% history call), find where the engine left off, then push each commit
% forward individually so each is tagged on the engine.
io_push_delta_(Endpoint, Domain, Branch_Name, _Head_Commit_Id,
               Head_Commit_Uri, Engine_Commit,
               Repository_Descriptor, System_DB, Auth, Path) :-
    Engine_Commit \== null,
    % Get the full commit history oldest-first (commit_uri_to_history_commit_ids
    % returns [oldest, ..., head]).
    commit_uri_to_history_commit_ids(Repository_Descriptor,
                                     Head_Commit_Uri,
                                     History_Oldest_First),
    % Slice: find the engine's last commit in the history and take everything
    % AFTER it (the forward range the engine hasn't indexed).
    commits_after(Engine_Commit, History_Oldest_First, Forward_Range),
    do_or_die(
        Forward_Range \== [],
        error(tdb_search_push_no_forward_range(Engine_Commit), _)),
    % Push each commit in the forward range, oldest-first.
    % Each push uses the PREVIOUS commit as parent (the one before it in the
    % chain). The first commit's parent is the engine's last-indexed commit.
    io_push_commit_chain(Endpoint, Domain, Branch_Name,
                         Engine_Commit, Forward_Range,
                         System_DB, Auth, Path).

/**
 * commits_after(+Last_Commit, +History_Oldest_First, -Forward_Range) is det.
 *
 * Given the full commit history [oldest, ..., head] and the engine's
 * last-indexed commit, returns the sub-list of commits strictly AFTER
 * Last_Commit in the chain (i.e., the commits the engine hasn't indexed).
 * Fails loud if Last_Commit is not in the history (indicates the engine
 * has state that doesn't match the branch — should not happen in normal
 * operation).
 */
commits_after(Last_Commit, History, Forward_Range) :-
    (   append(_, [Last_Commit | Forward_Range], History)
    ->  true
    ;   throw(error(tdb_search_last_indexed_not_in_history(Last_Commit), _))
    ).

/**
 * io_push_commit_chain(+Endpoint, +Domain, +Branch, +Parent_Commit,
 *                      +Commits, +System_DB, +Auth, +Path) is det.
 *
 * Pushes each commit in Commits sequentially, oldest-first. Each push
 * uses Parent_Commit as the parent_commit parameter and the commit itself
 * as target_commit, with delta computed as some(Parent_Commit) → Commit.
 * After each successful push, the engine tags the target commit, so an
 * interruption at any point leaves all previously-pushed commits tagged
 * (restart-safe resume-forward).
 */
io_push_commit_chain(_Endpoint, _Domain, _Branch, _Parent, [],
                     _System_DB, _Auth, _Path) :- !.
io_push_commit_chain(Endpoint, Domain, Branch, Parent_Commit,
                     [Commit | Rest], System_DB, Auth, Path) :-
    io_stream_push(Endpoint, Domain, Branch, Commit,
                   Parent_Commit, System_DB, Auth, Path,
                   some(Parent_Commit), Commit, _Task_Id),
    io_push_commit_chain(Endpoint, Domain, Branch, Commit,
                         Rest, System_DB, Auth, Path).

/**
 * descriptor_graphspec(+Descriptor, -GraphSpec) is det.
 *
 * Derives the full graphspec string from a resolved TerminusDB descriptor.
 * Uses the built-in `resolve_absolute_string_descriptor/2` in reverse mode
 * (bound Descriptor, unbound String) which reconstructs the canonical path
 * form `org/db/repo/branch/<name>` from the descriptor's internal structure.
 *
 * This is the SINGLE SOURCE OF TRUTH for the `domain` parameter sent to
 * tdb-search: it includes the full resource path (org, db, repo, branch)
 * so the engine can validate the structure. The engine internally reduces
 * it to org/db for keying (Domain::from_resource_path) but validates the
 * full graphspec via parse_domain.
 *
 * REPLACES the old `path_to_domain/2` which string-split and truncated,
 * discarding repo and branch segments — a correctness bug on non-main
 * branches (RISK-PH6-ADDR).
 */
descriptor_graphspec(Descriptor, GraphSpec) :-
    resolve_absolute_string_descriptor(GraphSpec, Descriptor).

/**
 * path_to_domain(+Path, -Domain) is det.
 *
 * DEPRECATED — retained for test backward compatibility.
 * Extracts the org/db portion from a TerminusDB path string. The push
 * driver now uses descriptor_graphspec/2 instead, which derives the full
 * graphspec from the resolved descriptor (single source of truth).
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
 * Explicit "index this branch now" entrypoint. Resolves the branch name
 * from the Path descriptor and drives a push. Gated on http_tdb_search.
 * This is the trigger for manual/explicit re-indexing.
 */
io_index_branch(System_DB, Auth, Path) :-
    do_or_die(
        indexer_backend(http_tdb_search),
        error(indexer_backend_not_tdb_search(io_index_branch), _)),
    resolve_absolute_string_descriptor(Path, Descriptor),
    (   branch_descriptor{branch_name: Branch_Name} :< Descriptor
    ->  true
    ;   Branch_Name = "main"
    ),
    io_push_delta(System_DB, Auth, Path, Branch_Name).
