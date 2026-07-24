:- module(api_search, [
              io_search_forward/7,
              io_similar_forward/7,
              io_duplicates_forward/6,
              io_statistics_forward/6,
              io_resolve_forward/6,
              io_compare_forward/4,
              io_compare_forward/5,
              io_delete_domain/2,
              build_search_url/5,
              build_similar_url/5,
              build_duplicates_url/4,
              build_statistics_url/5,
              build_resolve_url/4,
              build_compare_url/3,
              build_compare_url/4,
              build_delete_domain_url/3,
              ancestor_window/4,
              maybe_nudge_push/6
          ]).

/** <module> Search fronting — Phase 6 T4 (Capability B, RISK-09 parity)

TerminusDB authorises every caller against its OWN capability system, then
forwards the request to the tdb-search engine. One access-control model, no
drift.

The handler (in routes.pl) calls resolve_descriptor_auth(read, ...) which
asserts instance_read_access — FAIL-CLOSED: a caller denied on the data
product gets access_not_authorised (-> 403) BEFORE any engine call. This is
the RISK-09 parity gate.

This module provides the pure URL construction + I/O forwarding predicates.
The authz gate itself lives in the route handler (routes.pl) because it
needs System_DB and Auth from the HTTP dispatch.

Gated on indexer_backend(http_tdb_search); clear error if called when
backend is none or http_vectorlink.
*/

:- use_module(config(terminus_config)).
:- use_module(core(api/api_indexer), [
    encode_query_value/2,
    io_push_delta/4,
    descriptor_graphspec/2,
    validate_index_path/1,
    schema_store_clustering_for_path/2
]).
:- use_module(core(transaction)).
:- use_module(core(transaction/ref_entity), [
    branch_head_commit/3,
    commit_id_uri/3,
    commit_uri_to_history_commit_ids/3
]).
:- use_module(core(util)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_header)).
:- use_module(library(json)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(apply)).
:- use_module(library(url), [www_form_encode/2]).
:- use_module(core(plugins)).

% ==========================================================================
% Backend gate — all search forwarding requires http_tdb_search.
% ==========================================================================

/**
 * assert_search_backend is det.
 *
 * Fails loud if the indexer backend is not http_tdb_search. Called at the
 * top of every forwarding predicate to prevent any engine communication
 * under the wrong backend.
 */
assert_search_backend :-
    do_or_die(
        indexer_backend(http_tdb_search),
        error(search_requires_tdb_search_backend, _)).

% ==========================================================================
% HTTP Basic auth header (reuses the T3 credential config).
% ==========================================================================

search_auth_header(authorization(basic(User, Secret))) :-
    plugins:tdb_search_admin_user(User),
    plugins:tdb_search_admin_secret(Secret).

% ==========================================================================
% Ancestor window computation.
% ==========================================================================

/**
 * ancestor_window(+Repository_Descriptor, +Head_Commit_Uri,
 *                 +Max_Count, -Ancestors) is det.
 *
 * Computes the nearest-first ancestor commit window for catch-up resolution.
 * Returns up to Max_Count commit IDs (nearest first, excluding HEAD itself)
 * from the branch's commit history.
 *
 * The engine uses this window to serve the nearest indexed ancestor when the
 * requested commit is not yet indexed — never a commit outside this window,
 * so it cannot serve a snapshot newer than requested.
 *
 * Uses commit_uri_to_history_commit_ids/3 which walks the full parent chain.
 * For branches with very long history, a bounded walk would be preferable;
 * the Max_Count is applied post-hoc to bound the window at the API level.
 * The full walk is acceptable because: (a) the ancestor window for search
 * catch-up is typically 10 commits; (b) long histories are unusual for active
 * branches being searched; (c) the commit chain is stored compactly.
 */
ancestor_window(Repository_Descriptor, Head_Commit_Uri, Max_Count, Ancestors) :-
    commit_uri_to_history_commit_ids(Repository_Descriptor,
                                     Head_Commit_Uri,
                                     History_Oldest_First),
    reverse(History_Oldest_First, History_Newest_First),
    % Drop the HEAD commit itself (first element after reverse) — the ancestor
    % window is EXCLUDING the requested commit.
    (   History_Newest_First = [_Head | Rest]
    ->  length_bounded_prefix(Rest, Max_Count, Ancestors)
    ;   Ancestors = []
    ).

/**
 * length_bounded_prefix(+List, +Max, -Prefix) is det.
 *
 * Takes up to Max elements from the front of List.
 */
length_bounded_prefix(List, Max, Prefix) :-
    length(List, Len),
    (   Len =< Max
    ->  Prefix = List
    ;   length(Prefix, Max),
        append(Prefix, _, List)
    ).

% ==========================================================================
% URL construction — pure predicates (no I/O, testable in isolation).
% ==========================================================================

/**
 * build_search_url(+Endpoint, +Domain, +Commit, +Ancestors, -URL) is det.
 *
 * Constructs the engine's POST /search URL with domain, commit, and ancestor
 * window as query parameters.
 */
build_search_url(Endpoint, Domain, Commit, Ancestors, URL) :-
    encode_query_value(Domain, Enc_Domain),
    encode_query_value(Commit, Enc_Commit),
    ancestor_query_params(Ancestors, Ancestor_Params),
    format(atom(URL), "~w/search?domain=~w&commit=~w~w",
           [Endpoint, Enc_Domain, Enc_Commit, Ancestor_Params]).

/**
 * build_similar_url(+Endpoint, +Domain, +Commit, +Ancestors, -URL) is det.
 *
 * Constructs the engine's /similar URL with domain, commit, and ancestor window.
 */
build_similar_url(Endpoint, Domain, Commit, Ancestors, URL) :-
    encode_query_value(Domain, Enc_Domain),
    encode_query_value(Commit, Enc_Commit),
    ancestor_query_params(Ancestors, Ancestor_Params),
    format(atom(URL), "~w/similar?domain=~w&commit=~w~w",
           [Endpoint, Enc_Domain, Enc_Commit, Ancestor_Params]).

/**
 * build_duplicates_url(+Endpoint, +Domain, +Commit, -URL) is det.
 *
 * Constructs the engine's /duplicates URL. Duplicates uses commit in the
 * query but the ancestor window is not applicable (it operates on a
 * specific committed snapshot).
 */
build_duplicates_url(Endpoint, Domain, Commit, URL) :-
    encode_query_value(Domain, Enc_Domain),
    encode_query_value(Commit, Enc_Commit),
    format(atom(URL), "~w/duplicates?domain=~w&commit=~w",
           [Endpoint, Enc_Domain, Enc_Commit]).

/**
 * build_statistics_url(+Endpoint, +Domain, +Commit, +Ancestors, -URL) is det.
 *
 * Constructs the engine's /statistics URL scoped to a single domain.
 * Includes domain, commit, and ancestor query parameters (same contract as
 * build_search_url). TerminusDB always sends domain — the engine never
 * receives global statistics from a user-facing route.
 */
build_statistics_url(Endpoint, Domain, Commit, Ancestors, URL) :-
    encode_query_value(Domain, Enc_Domain),
    encode_query_value(Commit, Enc_Commit),
    ancestor_query_params(Ancestors, Ancestor_Params),
    format(atom(URL), "~w/statistics?domain=~w&commit=~w~w",
           [Endpoint, Enc_Domain, Enc_Commit, Ancestor_Params]).

/**
 * ancestor_query_params(+Ancestors, -ParamString) is det.
 *
 * Builds the repeated &ancestor=... query parameter suffix from a list of
 * ancestor commit IDs (nearest first). Returns "" if empty.
 */
ancestor_query_params([], "") :- !.
ancestor_query_params(Ancestors, ParamString) :-
    maplist(ancestor_param_fragment, Ancestors, Fragments),
    atomic_list_concat(Fragments, ParamString).

ancestor_param_fragment(Ancestor, Fragment) :-
    encode_query_value(Ancestor, Enc),
    format(atom(Fragment), "&ancestor=~w", [Enc]).

% ==========================================================================
% I/O forwarding predicates — effectful (io* convention).
% ==========================================================================

/**
 * io_search_forward(+Endpoint, +Domain, +Commit, +Ancestors,
 *                   +Extra_Params, -Response_Body, -Data_Version_Header) is det.
 *
 * Forwards a search request to the engine's POST /search endpoint.
 * Extra_Params is a list of Key=Value pairs to add to the query string
 * (e.g., q, mode, start, count, doc_type, doc_id, snippet).
 *
 * Response_Body is the JSON string from the engine.
 * Data_Version_Header is the value of the TerminusDB-Data-Version response
 * header (or the atom `none` if absent).
 */
io_search_forward(Endpoint, Domain, Commit, Ancestors,
                  Extra_Params, Response_Body, Data_Version_Header) :-
    assert_search_backend,
    search_auth_header(AuthHeader),
    build_search_url(Endpoint, Domain, Commit, Ancestors, Base_URL),
    append_extra_params(Base_URL, Extra_Params, URL),
    io_forward_get(URL, AuthHeader, Response_Body, Data_Version_Header).

/**
 * io_similar_forward(+Endpoint, +Domain, +Commit, +Ancestors,
 *                    +Extra_Params, -Response_Body, -Data_Version_Header) is det.
 *
 * Forwards a similar request to the engine's GET /similar endpoint.
 */
io_similar_forward(Endpoint, Domain, Commit, Ancestors,
                   Extra_Params, Response_Body, Data_Version_Header) :-
    assert_search_backend,
    search_auth_header(AuthHeader),
    build_similar_url(Endpoint, Domain, Commit, Ancestors, Base_URL),
    append_extra_params(Base_URL, Extra_Params, URL),
    io_forward_get(URL, AuthHeader, Response_Body, Data_Version_Header).

/**
 * io_duplicates_forward(+Endpoint, +Domain, +Commit,
 *                       +Extra_Params, -Response_Body, -Data_Version_Header) is det.
 *
 * Forwards a duplicates request to the engine's GET /duplicates endpoint.
 */
io_duplicates_forward(Endpoint, Domain, Commit,
                      Extra_Params, Response_Body, Data_Version_Header) :-
    assert_search_backend,
    search_auth_header(AuthHeader),
    build_duplicates_url(Endpoint, Domain, Commit, Base_URL),
    append_extra_params(Base_URL, Extra_Params, URL),
    io_forward_get(URL, AuthHeader, Response_Body, Data_Version_Header).

/**
 * io_statistics_forward(+Endpoint, +Domain, +Commit, +Ancestors,
 *                       -Response_Body, -Data_Version_Header) is det.
 *
 * Forwards a statistics request to the engine's GET /statistics endpoint,
 * scoped to a single domain. Mirrors io_search_forward contract.
 */
io_statistics_forward(Endpoint, Domain, Commit, Ancestors,
                      Response_Body, Data_Version_Header) :-
    assert_search_backend,
    search_auth_header(AuthHeader),
    build_statistics_url(Endpoint, Domain, Commit, Ancestors, URL),
    io_forward_get(URL, AuthHeader, Response_Body, Data_Version_Header).

% ==========================================================================
% Compare: stateless text distance (no domain, no descriptor).
% ==========================================================================

/**
 * build_compare_url(+Endpoint, +Method, -URL) is det.
 *
 * Constructs the engine's POST /compare URL with the required method
 * query parameter. Currently only "embedding" is supported.
 */
build_compare_url(Endpoint, Method, URL) :-
    encode_query_value(Method, Enc_Method),
    format(atom(URL), "~w/compare?method=~w", [Endpoint, Enc_Method]).

/**
 * build_compare_url(+Endpoint, +Method, +Role, -URL) is det.
 *
 * Constructs the engine's POST /compare URL with method and optional role
 * query parameters. Role is a nomic task prefix selector (query, document,
 * clustering, classification).
 */
build_compare_url(Endpoint, Method, Role, URL) :-
    nonvar(Role),
    !,
    encode_query_value(Method, Enc_Method),
    encode_query_value(Role, Enc_Role),
    format(atom(URL), "~w/compare?method=~w&role=~w", [Endpoint, Enc_Method, Enc_Role]).
build_compare_url(Endpoint, Method, _Role, URL) :-
    build_compare_url(Endpoint, Method, URL).

/**
 * io_compare_forward(+Endpoint, +Method, +Body_Dict, -Response_Body) is det.
 *
 * Forwards a compare request to the engine's POST /compare endpoint.
 * Body_Dict is a dict with `source` and `target` fields (plain text strings).
 * Response_Body is the JSON string from the engine.
 *
 * Stateless: no domain, no dataset, no ANN index. The engine embeds both
 * texts and returns their cosine distance on the [0, 1] reference scale.
 *
 * Fails loud on non-2xx responses (the engine validates method and body).
 */
io_compare_forward(Endpoint, Method, Body_Dict, Response_Body) :-
    io_compare_forward(Endpoint, Method, _No_Role, Body_Dict, Response_Body).

io_compare_forward(Endpoint, Method, Role, Body_Dict, Response_Body) :-
    assert_search_backend,
    search_auth_header(AuthHeader),
    build_compare_url(Endpoint, Method, Role, URL),
    setup_call_cleanup(
        http_open(URL, In,
                  [ method(post),
                    post(json(Body_Dict)),
                    status_code(Status),
                    AuthHeader,
                    request_header('Content-Type' = 'application/json'),
                    request_header('Accept' = 'application/json')
                  ]),
        read_string(In, _, Response_Body),
        close(In)),
    handle_compare_response(Status, Response_Body, URL).

/**
 * handle_compare_response(+Status, +Body, +URL) is det.
 *
 * Succeeds on 2xx responses. Fails loud on error status, preserving
 * the engine's error body for diagnostics.
 */
handle_compare_response(Status, _Body, _URL) :-
    Status >= 200,
    Status < 300,
    !.
handle_compare_response(Status, Body, URL) :-
    throw(error(tdb_search_forward_failed(Status, Body, URL), _)).

% ==========================================================================
% Internal: HTTP GET forwarding with response header extraction.
% ==========================================================================

/**
 * io_forward_get(+URL, +AuthHeader, -Response_Body, -Data_Version_Header) is det.
 *
 * Performs a GET request to the URL with the given auth header. Extracts
 * the TerminusDB-Data-Version response header if present. Fails loud on
 * non-2xx responses with the engine's error body.
 *
 * The `header(terminusdb_data_version, DV_Raw)` option captures the
 * response header value if present; it remains unbound if absent.
 * SWI normalises header names: lowercase, hyphens to underscores.
 */
io_forward_get(URL, AuthHeader, Response_Body, Data_Version_Header) :-
    setup_call_cleanup(
        http_open(URL, In,
                  [ status_code(Status),
                    AuthHeader,
                    request_header('Accept' = 'application/json'),
                    header(terminusdb_data_version, DV_Raw)
                  ]),
        read_string(In, _, Response_Body),
        close(In)),
    normalise_data_version_header(DV_Raw, Data_Version_Header),
    handle_forward_response(Status, Response_Body, URL).

/**
 * normalise_data_version_header(+Raw, -Normalised) is det.
 *
 * Converts the raw header value (empty string if absent) to either the
 * header value or the atom `none`.
 */
normalise_data_version_header(Raw, none) :-
    (   var(Raw) ; Raw == '' ; Raw == "" ),
    !.
normalise_data_version_header(Raw, Raw).

/**
 * handle_forward_response(+Status, +Body, +URL) is det.
 *
 * Succeeds on 2xx responses. Fails loud on any error status, preserving
 * the engine's error body for diagnostics.
 */
handle_forward_response(Status, _Body, _URL) :-
    Status >= 200,
    Status < 300,
    !.
handle_forward_response(Status, Body, URL) :-
    throw(error(tdb_search_forward_failed(Status, Body, URL), _)).

% ==========================================================================
% Internal: query parameter construction.
% ==========================================================================

/**
 * append_extra_params(+Base_URL, +Params, -Full_URL) is det.
 *
 * Appends a list of Key=Value or Key=repeated(Values) pairs to a URL that
 * already contains query parameters. Handles both scalar and repeated
 * (array) params (e.g., doc_type, doc_id, ancestor).
 */
append_extra_params(Base_URL, [], Base_URL) :- !.
append_extra_params(Base_URL, Params, Full_URL) :-
    maplist(param_to_fragment, Params, Fragments),
    atomic_list_concat(Fragments, Param_String),
    atom_concat(Base_URL, Param_String, Full_URL).

param_to_fragment(Key=repeated(Values), Fragment) :-
    !,
    maplist(repeated_param_fragment(Key), Values, Frags),
    atomic_list_concat(Frags, Fragment).
param_to_fragment(Key=Value, Fragment) :-
    encode_query_value(Value, Enc_Value),
    format(atom(Fragment), "&~w=~w", [Key, Enc_Value]).

repeated_param_fragment(Key, Value, Fragment) :-
    encode_query_value(Value, Enc_Value),
    format(atom(Fragment), "&~w=~w", [Key, Enc_Value]).

% ==========================================================================
% Stale-version nudge: when the served data-version differs from the
% requested commit, trigger a background push so the next search converges.
% ==========================================================================

/**
 * maybe_nudge_push(+Data_Version_Header, +Commit, +System_DB, +Auth, +Path,
 *                  +Branch_Name) is det.
 *
 * If the engine served a different commit than requested (stale), nudge a
 * push so the next search converges. The nudge is fire-and-forget (errors
 * logged but not propagated to the search caller — the stale result is still
 * valid, just not at HEAD).
 */
maybe_nudge_push(none, _Commit, _System_DB, _Auth, _Path, _Branch) :- !.
maybe_nudge_push(Data_Version_Header, Commit, _System_DB, _Auth, Path, Branch) :-
    format(atom(Expected_DV), "commit:~w", [Commit]),
    (   Data_Version_Header == Expected_DV
    ->  true  % Served the exact commit requested — no nudge needed.
    ;   % Stale: served a different (ancestor) commit. Nudge via indexer_notify
        % (O(1) FFI call — no HTTP, no NDJSON generation from Prolog).
        % has_embeddings=false lets the Rust-side no_embedding_cache skip
        % this nudge instantly for domains without embedding metadata.
        (   plugin_api:indexer_available
        ->  catch(
                (plugin_api:indexer_notify(Path, Branch, false, false) ; true),
                Nudge_Error,
                format(user_error,
                       "[WARN] Search stale-version nudge failed for ~w: ~q~n",
                       [Path, Nudge_Error]))
        ;   true
        )
    ).

% ==========================================================================
% Resolve: entity resolution (single-domain, POST /resolve).
%
% DUAL-DOMAIN ANALYSIS (T4 PO decision):
% The /resolve endpoint's set_doc_types/set_doc_ids and target_doc_types/
% target_doc_ids are FILTERS WITHIN a single domain — the `domain` field
% is singular in the ResolveRequestBody. Cross-data-product set/target is
% NOT expressible in the current engine contract. Therefore, single-domain
% authz (resolve_descriptor_auth(read) on the ONE data product in the URL
% path) is correct and complete for the current contract.
%
% DEFERRED: Cross-data-product resolve (separate domain for set vs target)
% is NOT implemented. If a future engine contract allows two domains, a
% second resolve_descriptor_auth(read) call on the target domain is needed.
% Logged as a defer-and-log item for the PO.
% ==========================================================================

/**
 * build_resolve_url(+Endpoint, +Domain, +Commit, -URL) is det.
 *
 * Constructs the engine's POST /resolve URL. Domain and commit are passed
 * in the JSON body (not query params) for /resolve, so the URL is bare.
 * However, we still include domain and commit as query params for the
 * auth header to be consistent with the engine's routing model.
 */
build_resolve_url(Endpoint, Domain, Commit, URL) :-
    encode_query_value(Domain, Enc_Domain),
    encode_query_value(Commit, Enc_Commit),
    format(atom(URL), "~w/candidates?domain=~w&commit=~w",
           [Endpoint, Enc_Domain, Enc_Commit]).

/**
 * io_resolve_forward(+Endpoint, +Domain, +Commit, +Ancestors,
 *                    +Body_Dict, -Response_Body) is det.
 *
 * Forwards a resolve request to the engine's POST /candidates endpoint.
 * Body_Dict is the full JSON body dict from the caller (with server-derived
 * domain, commit, and ancestors injected). Returns the engine's JSON response.
 *
 * Fails loud on non-2xx responses.
 */
io_resolve_forward(Endpoint, Domain, Commit, Ancestors,
                   Body_Dict, Response_Body) :-
    assert_search_backend,
    search_auth_header(AuthHeader),
    % /candidates uses POST with JSON body — domain, commit, ancestors in body.
    format(atom(Resolve_URL), "~w/candidates", [Endpoint]),
    % Inject server-derived fields into the body (overwriting any caller attempt
    % to supply them — graphspec-from-URL invariant).
    put_dict(_{domain: Domain, commit: Commit, ancestors: Ancestors},
             Body_Dict, Forward_Body),
    setup_call_cleanup(
        http_open(Resolve_URL, In,
                  [ method(post),
                    post(json(Forward_Body)),
                    status_code(Status),
                    AuthHeader,
                    request_header('Content-Type' = 'application/json'),
                    request_header('Accept' = 'application/json')
                  ]),
        read_string(In, _, Response_Body),
        close(In)),
    handle_forward_response(Status, Response_Body, Resolve_URL).

% ==========================================================================
% DELETE /domain: drop a domain's search index on data-product deletion (T5).
%
% FAIL-LOUD, BEST-EFFORT: if the engine call fails, surface a loud error/log
% but do NOT block the TerminusDB-side deletion. The orphaned-index scenario
% is VISIBLE (logged to user_error), not silent.
% ==========================================================================

/**
 * build_delete_domain_url(+Endpoint, +Domain, -URL) is det.
 *
 * Constructs the engine's DELETE /domain?domain=<org/db> URL.
 */
build_delete_domain_url(Endpoint, Domain, URL) :-
    encode_query_value(Domain, Enc_Domain),
    format(atom(URL), "~w/domain?domain=~w", [Endpoint, Enc_Domain]).

/**
 * io_delete_domain(+Endpoint, +Domain) is det.
 *
 * Sends DELETE /domain?domain=<Domain> to the engine. Idempotent: the engine
 * returns 204 for both existing and already-removed domains. Fails loud
 * on genuine I/O errors (non-2xx and non-404).
 *
 * Called from db_delete.pl on data-product deletion. Best-effort: the caller
 * catches failures and logs them loudly without blocking the TerminusDB
 * deletion.
 */
io_delete_domain(Endpoint, Domain) :-
    assert_search_backend,
    search_auth_header(AuthHeader),
    build_delete_domain_url(Endpoint, Domain, URL),
    setup_call_cleanup(
        http_open(URL, In,
                  [ method(delete),
                    status_code(Status),
                    AuthHeader,
                    request_header('Accept' = 'application/json')
                  ]),
        read_string(In, _, Response_Body),
        close(In)),
    handle_delete_domain_response(Status, Response_Body, URL).

/**
 * handle_delete_domain_response(+Status, +Body, +URL) is det.
 *
 * Succeeds on 2xx and 404 (idempotent deletion). Fails loud on any other
 * status — the error propagates to the caller which logs it.
 */
handle_delete_domain_response(Status, _Body, _URL) :-
    Status >= 200,
    Status < 300,
    !.
handle_delete_domain_response(404, _Body, _URL) :- !.
handle_delete_domain_response(Status, Body, URL) :-
    throw(error(tdb_search_delete_domain_failed(Status, Body, URL), _)).
