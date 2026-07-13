:- module(search_resolve, [
    % Public API
    io_candidates_forward/6,
    resolve_match/4,
    resolve_run/6
]).

/** <module> search_resolve — entity resolution matching plugin

This plugin implements the entity resolution matching algorithm on the
TerminusDB server side. It calls the tdb-search /candidates endpoint to
gather raw bidirectional KNN candidate pairs, then runs the 3-threshold
matching algorithm (core 1:1, set-extra 1:M, target-extra M:1) in Prolog.

Endpoints:
  POST /api/plugin/search-resolve/<path>  — full matching with tau thresholds
  POST /api/plugin/search-candidates/<path> — raw KNN gather (proxy to tdb-search)

The /candidates endpoint is a thin proxy that forwards to tdb-search's
/candidates endpoint. The /resolve endpoint calls /candidates internally,
then applies the matching algorithm to produce the 3-partition output
(matched, set_only, target_only).
*/

:- use_module(core(plugin_api)).
:- use_module(core(util)).
:- use_module(core(account)).
:- use_module(core(account/capabilities), [resolve_descriptor_auth/6]).
:- use_module(core(transaction/ref_entity), [branch_head_commit/3, commit_id_uri/3]).
:- use_module(core(plugins)).
:- use_module(library(json)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_header)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(dicts)).
:- use_module(library(pairs)).

% Reuse tdb_search helpers for auth and endpoint discovery.
% Only ancestor_window/4, maybe_nudge_push_async/4, tdb_search_endpoint/1
% are exported by tdb_search. Other predicates are called with tdb_search:
% module prefix (SWI-Prolog allows calling non-exported predicates this way).
:- use_module(plugins(tdb_search), [
    tdb_search_endpoint/1,
    ancestor_window/4,
    maybe_nudge_push_async/4
]).

% ==========================================================================
% Route registration
% ==========================================================================

:- plugin_api:register_route(api(plugin/'search-resolve'/Path),
    plugin_api:cors_handler(Method, search_resolve:resolve_handler(Path)),
    [method(Method), prefix, methods([options,post])]).

:- plugin_api:register_route(api(plugin/'search-candidates'/Path),
    plugin_api:cors_handler(Method, search_resolve:candidates_handler(Path)),
    [method(Method), prefix, methods([options,post])]).

% ==========================================================================
% /api/plugin/search-candidates — proxy to tdb-search /candidates
% ==========================================================================

candidates_handler(post, Path, Request, System_DB, Auth) :-
    (   memberchk(search(Search), Request)
    ->  true
    ;   Search = []),
    tdb_search:search_request_body(Request, Body),
    plugin_api:api_report_errors(
        search,
        Request,
        (
            plugin_api:resolve_descriptor_auth(read, System_DB, Auth, Path, instance, Descriptor),
            do_or_die(tdb_search:tdb_search_endpoint(Endpoint),
                      error(tdb_search_endpoint_not_configured(candidates_handler), _)),
            do_or_die(
                branch_descriptor{branch_name: Branch_Name} :< Descriptor,
                error(search_requires_branch_descriptor(Path), _)),
            get_dict(repository_descriptor, Descriptor, Repository_Descriptor),
            branch_head_commit(Repository_Descriptor, Branch_Name, Head_Commit_Uri),
            commit_id_uri(Repository_Descriptor, Head_Commit_Id, Head_Commit_Uri),
            tdb_search:descriptor_domain(Descriptor, Domain),
            tdb_search:ancestor_window(Repository_Descriptor, Head_Commit_Uri, 100, Ancestors),
            tdb_search:compress_flag(Search, Compress),
            tdb_search:maybe_prefixes(Compress, Descriptor, Prefixes),
            candidates_forward_body(Body, Prefixes, Forward_Body),
            catch(
                (   io_candidates_forward(Endpoint, Domain, Head_Commit_Id, Ancestors,
                                          Forward_Body, Response_Body),
                    tdb_search:maybe_compact_response(Compress, Response_Body,
                                                       Descriptor, Final_Body),
                    plugin_api:write_cors_headers(Request),
                    format("Content-Type: application/json~n~n"),
                    write(Final_Body)
                ),
                error(tdb_search_forward_failed(404, Engine_Body, _Fail_URL), _),
                (   tdb_search:maybe_nudge_push_async(System_DB, Auth, Path, Branch_Name),
                    throw(error(search_not_indexed(Path, Engine_Body), _))
                )
            )
        )
    ).

%% io_candidates_forward(+Endpoint, +Domain, +Commit, +Ancestors,
%%                       +Body_Dict, -Response_Body) is det.
%
%  Forward a POST /candidates request to the tdb-search engine.
io_candidates_forward(Endpoint, Domain, Commit, Ancestors,
                      Body_Dict, Response_Body) :-
    tdb_search:assert_search_backend,
    tdb_search:search_auth_header(AuthHeader),
    format(atom(Candidates_URL), "~w/candidates", [Endpoint]),
    put_dict(_{domain: Domain, commit: Commit, ancestors: Ancestors},
             Body_Dict, Forward_Body),
    setup_call_cleanup(
        http_open(Candidates_URL, In,
                  [ method(post),
                    post(json(Forward_Body)),
                    status_code(Status),
                    AuthHeader,
                    request_header('Content-Type' = 'application/json'),
                    request_header('Accept' = 'application/json')
                  ]),
        read_string(In, _, Response_Body),
        close(In)),
    tdb_search:handle_forward_response(Status, Response_Body, Candidates_URL).

candidates_forward_body(Body, Prefixes, Forward_Body) :-
    findall(Key-Value,
            (   candidates_allowed_body_key(Key),
                get_dict(Key, Body, Raw_Value),
                candidates_normalize_value(Key, Raw_Value, Prefixes, Value)
            ),
            Pairs),
    dict_pairs(Forward_Body, _, Pairs).

candidates_normalize_value(Key, Raw_Ids, Prefixes, Ids) :-
    ( Key == set_doc_ids ; Key == target_doc_ids ),
    !,
    maplist({Prefixes}/[Raw, Id]>>tdb_search:normalize_doc_id(Raw, Prefixes, Id),
            Raw_Ids, Ids).
candidates_normalize_value(_Key, Value, _Prefixes, Value).

candidates_allowed_body_key(set_doc_types).
candidates_allowed_body_key(set_doc_ids).
candidates_allowed_body_key(target_doc_types).
candidates_allowed_body_key(target_doc_ids).
candidates_allowed_body_key(k).
candidates_allowed_body_key(threshold_set).
candidates_allowed_body_key(threshold_target).
candidates_allowed_body_key(include).

% ==========================================================================
% /api/plugin/search-resolve — full matching with tau thresholds
% ==========================================================================

resolve_handler(post, Path, Request, System_DB, Auth) :-
    (   memberchk(search(Search), Request)
    ->  true
    ;   Search = []),
    tdb_search:search_request_body(Request, Body),
    plugin_api:api_report_errors(
        search,
        Request,
        (
            plugin_api:resolve_descriptor_auth(read, System_DB, Auth, Path, instance, Descriptor),
            do_or_die(tdb_search:tdb_search_endpoint(Endpoint),
                      error(tdb_search_endpoint_not_configured(resolve_handler), _)),
            do_or_die(
                branch_descriptor{branch_name: Branch_Name} :< Descriptor,
                error(search_requires_branch_descriptor(Path), _)),
            get_dict(repository_descriptor, Descriptor, Repository_Descriptor),
            branch_head_commit(Repository_Descriptor, Branch_Name, Head_Commit_Uri),
            commit_id_uri(Repository_Descriptor, Head_Commit_Id, Head_Commit_Uri),
            tdb_search:descriptor_domain(Descriptor, Domain),
            tdb_search:ancestor_window(Repository_Descriptor, Head_Commit_Uri, 100, Ancestors),
            tdb_search:compress_flag(Search, Compress),
            tdb_search:maybe_prefixes(Compress, Descriptor, Prefixes),
            resolve_forward_body(Body, Prefixes, Forward_Body),
            catch(
                (   resolve_run(Endpoint, Domain, Head_Commit_Id, Ancestors,
                                Forward_Body, Response_Body),
                    tdb_search:maybe_compact_response(Compress, Response_Body,
                                                       Descriptor, Final_Body),
                    plugin_api:write_cors_headers(Request),
                    format("Content-Type: application/json~n~n"),
                    write(Final_Body)
                ),
                error(tdb_search_forward_failed(404, Engine_Body, _Fail_URL), _),
                (   tdb_search:maybe_nudge_push_async(System_DB, Auth, Path, Branch_Name),
                    throw(error(search_not_indexed(Path, Engine_Body), _))
                )
            )
        )
    ).

resolve_forward_body(Body, Prefixes, Forward_Body) :-
    findall(Key-Value,
            (   resolve_allowed_body_key(Key),
                get_dict(Key, Body, Raw_Value),
                resolve_normalize_value(Key, Raw_Value, Prefixes, Value)
            ),
            Pairs),
    dict_pairs(Forward_Body, _, Pairs).

resolve_normalize_value(Key, Raw_Ids, Prefixes, Ids) :-
    ( Key == set_doc_ids ; Key == target_doc_ids ),
    !,
    maplist({Prefixes}/[Raw, Id]>>tdb_search:normalize_doc_id(Raw, Prefixes, Id),
            Raw_Ids, Ids).
resolve_normalize_value(_Key, Value, _Prefixes, Value).

resolve_allowed_body_key(set_doc_types).
resolve_allowed_body_key(set_doc_ids).
resolve_allowed_body_key(target_doc_types).
resolve_allowed_body_key(target_doc_ids).
resolve_allowed_body_key(threshold).
resolve_allowed_body_key(tau_one_to_one).
resolve_allowed_body_key(tau_one_to_many).
resolve_allowed_body_key(tau_many_to_one).
resolve_allowed_body_key(k).

% ==========================================================================
% Matching algorithm — pure Prolog, no I/O
% ==========================================================================

%% resolve_run(+Endpoint, +Domain, +Commit, +Ancestors,
%%            +Forward_Body, -Response_Body) is det.
%
%  Calls /candidates on tdb-search, parses the response, runs the matching
%  algorithm, and produces the 3-partition JSON output.
resolve_run(Endpoint, Domain, Commit, Ancestors, Forward_Body, Response_Body) :-
    % Extract matching parameters from Forward_Body.
    get_dict(threshold, Forward_Body, Threshold),
    get_dict(tau_one_to_one, Forward_Body, TauOneToOne),
    (   get_dict(tau_one_to_many, Forward_Body, TauOneToMany)
    ->  true
    ;   TauOneToMany = none),
    (   get_dict(tau_many_to_one, Forward_Body, TauManyToOne)
    ->  true
    ;   TauManyToOne = none),
    (   get_dict(k, Forward_Body, K)
    ->  true
    ;   K = 5),

    % Build candidates request body (no include — we only need distances).
    put_dict(_{threshold_set: Threshold, threshold_target: Threshold},
             Forward_Body, Cand_Body0),
    % Remove keys not relevant for /candidates (some may be absent).
    safe_del_dict(threshold, Cand_Body0, Cand_Body1),
    safe_del_dict(tau_one_to_one, Cand_Body1, Cand_Body2),
    safe_del_dict(tau_one_to_many, Cand_Body2, Cand_Body3),
    safe_del_dict(tau_many_to_one, Cand_Body3, Cand_Body4),

    % Call tdb-search /candidates.
    io_candidates_forward(Endpoint, Domain, Commit, Ancestors,
                          Cand_Body4, Cand_Response_String),

    % Parse JSON response.
    atom_json_dict(Cand_Response_String, Cand_Result),

    % Extract directional maps.
    get_dict(set_to_target, Cand_Result, SetToTarget),
    get_dict(target_to_set, Cand_Result, TargetToSet),
    get_dict(stats, Cand_Result, Cand_Stats),
    get_dict(elapsed_ms, Cand_Stats, Elapsed_Ms),

    % Run the matching algorithm.
    Options = _{k: K, threshold: Threshold,
                tau_one_to_one: TauOneToOne,
                tau_one_to_many: TauOneToMany,
                tau_many_to_one: TauManyToOne},
    resolve_match(SetToTarget, TargetToSet, Options, Matched),

    % Build set_only and target_only (docs not in any match).
    extract_ids(Matched, set_id, Matched_Set_Ids),
    extract_ids(Matched, target_id, Matched_Target_Ids),
    dict_keys(SetToTarget, All_Set_Keys),
    dict_keys(TargetToSet, All_Target_Keys),
    sort(All_Set_Keys, All_Set_Ids),
    sort(All_Target_Keys, All_Target_Ids),
    ord_subtract(All_Set_Ids, Matched_Set_Ids, Set_Only_Ids),
    ord_subtract(All_Target_Ids, Matched_Target_Ids, Target_Only_Ids),

    length(Matched, Matched_Count),

    % Build the response JSON.
    Response = _{
        matched: Matched,
        set_only: Set_Only_Ids,
        target_only: Target_Only_Ids,
        stats: _{
            elapsed_ms: Elapsed_Ms,
            set_points: Cand_Stats.set_points,
            target_points: Cand_Stats.target_points,
            matched_count: Matched_Count
        }
    },
    atom_json_dict(Response_Body, Response, []).

%% resolve_match(+SetToTarget, +TargetToSet, +Options, -Matched) is det.
%
%  Pure matching algorithm: 3-threshold resolution producing matched pairs.
%
%  Stage 1 (Core 1:1): For each set doc S, find its best target T.
%    If T's best set doc is also S (reciprocal), and distance <= tau_one_to_one,
%    then (S, T) is a core match.
%
%  Stage 2 (Set Extra 1:M): If tau_one_to_many is enabled, for each set doc S
%    that has a core match, additional targets T' with distance <= tau_one_to_many
%    are also matched (if not already matched to another set doc as core).
%
%  Stage 3 (Target Extra M:1): If tau_many_to_one is enabled, for each target doc T
%    that has a core match, additional set docs S' with distance <= tau_many_to_one
%    are also matched (if not already matched as core).
resolve_match(SetToTarget, TargetToSet, Options, Matched) :-
    get_dict(tau_one_to_one, Options, Tau11),
    (   get_dict(tau_one_to_many, Options, Tau1M)
    ->  true
    ;   Tau1M = none),
    (   get_dict(tau_many_to_one, Options, TauM1)
    ->  true
    ;   TauM1 = none),

    % Stage 1: Core reciprocal 1:1 matches.
    findall(_{set_id: S, target_id: T, distance: D, stage: core},
            (   get_dict(S, SetToTarget, S_Neighbours),
                best_neighbour(S_Neighbours, T, D),
                D =< Tau11,
                get_dict(T, TargetToSet, T_Neighbours),
                best_neighbour(T_Neighbours, S, D2),
                D2 =< Tau11
            ),
            Core_Matched),

    % Collect core-matched IDs.
    findall(S, (member(M, Core_Matched), get_dict(set_id, M, S)), Core_Set_Ids0),
    findall(T, (member(M, Core_Matched), get_dict(target_id, M, T)), Core_Target_Ids0),
    sort(Core_Set_Ids0, Core_Set_Ids),
    sort(Core_Target_Ids0, Core_Target_Ids),

    % Stage 2: Set-extra 1:M matches.
    (   Tau1M \== none
    ->  findall(_{set_id: S, target_id: T, distance: D, stage: set_extra},
                (   get_dict(S, SetToTarget, S_Neighbours),
                    member(N, S_Neighbours),
                    get_dict(id, N, T),
                    get_dict(distance, N, D),
                    D =< Tau1M,
                    \+ memberchk(T, Core_Target_Ids),
                    \+ has_match(Core_Matched, S, T)
                ),
                Set_Extra_Matched)
    ;   Set_Extra_Matched = []),

    % Stage 3: Target-extra M:1 matches.
    (   TauM1 \== none
    ->  findall(_{set_id: S, target_id: T, distance: D, stage: target_extra},
                (   get_dict(T, TargetToSet, T_Neighbours),
                    member(N, T_Neighbours),
                    get_dict(id, N, S),
                    get_dict(distance, N, D),
                    D =< TauM1,
                    \+ memberchk(S, Core_Set_Ids),
                    \+ has_match(Core_Matched, S, T)
                ),
                Target_Extra_Matched)
    ;   Target_Extra_Matched = []),

    append([Core_Matched, Set_Extra_Matched, Target_Extra_Matched], Matched).

%% has_match(+Matched, +SetId, +TargetId) is semidet.
%  True if a match with the given set_id and target_id already exists.
has_match(Matched, S, T) :-
    member(M, Matched),
    get_dict(set_id, M, S),
    get_dict(target_id, M, T),
    !.

%% best_neighbour(+Neighbours, -BestId, -BestDistance) is semidet.
%
%  Returns the first (nearest) neighbour from a sorted candidate list.
%  Fails if the list is empty.
best_neighbour([H | _], Id, D) :-
    get_dict(id, H, Id),
    get_dict(distance, H, D),
    !.

%% extract_ids(+Matched, +Key, -Ids) is det.
%
%  Extract all values for a given key from the matched list.
%  Uses ord_union/2 which is ~20% faster than sort/2 for this case.
extract_ids(Matched, Key, Ids) :-
    atom_json_key(Key, JsonKey),
    findall(Id, (member(M, Matched), get_dict(JsonKey, M, Id)), Ids0),
    ord_union([Ids0], Ids).

% Helper to map between atom and JSON key names.
atom_json_key(set_id, set_id).
atom_json_key(target_id, target_id).

% safe_del_dict(+Key, +DictIn, -DictOut) is det.
%  Removes Key from DictIn if present, otherwise returns DictIn unchanged.
safe_del_dict(Key, DictIn, DictOut) :-
    (   del_dict(Key, DictIn, _, DictOut)
    ->  true
    ;   DictOut = DictIn
    ).

% ==========================================================================
% Unit tests
% ==========================================================================

:- use_module(core(util/test_utils),
             [setup_temp_store/1, teardown_temp_store/1,
              create_db_without_schema/2]).
:- use_module(core(account/user_management), [add_user/3]).
:- use_module(core(triple), [super_user_authority/1]).
:- use_module(core(transaction), [open_descriptor/2]).

:- begin_tests(search_resolve_matching).

test("core 1:1 reciprocal match with permissive tau", []) :-
    SetToTarget = _{
        'doc/a': [_{id: 'doc/x', distance: 0.1}, _{id: 'doc/y', distance: 0.3}],
        'doc/b': [_{id: 'doc/y', distance: 0.2}]
    },
    TargetToSet = _{
        'doc/x': [_{id: 'doc/a', distance: 0.1}],
        'doc/y': [_{id: 'doc/b', distance: 0.2}, _{id: 'doc/a', distance: 0.3}]
    },
    Options = _{k: 5, threshold: 0.5, tau_one_to_one: 0.5,
                tau_one_to_many: none, tau_many_to_one: none},
    resolve_match(SetToTarget, TargetToSet, Options, Matched),
    has_match(Matched, 'doc/a', 'doc/x'),
    has_match(Matched, 'doc/b', 'doc/y'),
    length(Matched, 2).

test("tight tau_one_to_one filters out distant reciprocal pairs", []) :-
    SetToTarget = _{
        'doc/a': [_{id: 'doc/x', distance: 0.4}],
        'doc/b': [_{id: 'doc/y', distance: 0.1}]
    },
    TargetToSet = _{
        'doc/x': [_{id: 'doc/a', distance: 0.4}],
        'doc/y': [_{id: 'doc/b', distance: 0.1}]
    },
    Options = _{k: 5, threshold: 0.5, tau_one_to_one: 0.2,
                tau_one_to_many: none, tau_many_to_one: none},
    resolve_match(SetToTarget, TargetToSet, Options, Matched),
    has_match(Matched, 'doc/b', 'doc/y'),
    \+ has_match(Matched, 'doc/a', 'doc/x'),
    length(Matched, 1).

test("tau_one_to_many adds set-extra matches beyond core", []) :-
    SetToTarget = _{
        'doc/a': [_{id: 'doc/x', distance: 0.1}, _{id: 'doc/z', distance: 0.25}]
    },
    TargetToSet = _{
        'doc/x': [_{id: 'doc/a', distance: 0.1}],
        'doc/z': [_{id: 'doc/c', distance: 0.05}]
    },
    Options = _{k: 5, threshold: 0.5, tau_one_to_one: 0.15,
                tau_one_to_many: 0.3, tau_many_to_one: none},
    resolve_match(SetToTarget, TargetToSet, Options, Matched),
    has_match(Matched, 'doc/a', 'doc/x'),
    has_match(Matched, 'doc/a', 'doc/z').

test("tau_many_to_one adds target-extra matches beyond core", []) :-
    SetToTarget = _{
        'doc/a': [_{id: 'doc/x', distance: 0.1}],
        'doc/c': [_{id: 'doc/x', distance: 0.25}]
    },
    TargetToSet = _{
        'doc/x': [_{id: 'doc/a', distance: 0.1}, _{id: 'doc/c', distance: 0.25}]
    },
    Options = _{k: 5, threshold: 0.5, tau_one_to_one: 0.15,
                tau_one_to_many: none, tau_many_to_one: 0.3},
    resolve_match(SetToTarget, TargetToSet, Options, Matched),
    has_match(Matched, 'doc/a', 'doc/x'),
    has_match(Matched, 'doc/c', 'doc/x').

test("no matches when no reciprocal pairs exist", []) :-
    SetToTarget = _{
        'doc/a': [_{id: 'doc/x', distance: 0.1}]
    },
    TargetToSet = _{
        'doc/x': [_{id: 'doc/b', distance: 0.05}]
    },
    Options = _{k: 5, threshold: 0.5, tau_one_to_one: 0.5,
                tau_one_to_many: none, tau_many_to_one: none},
    resolve_match(SetToTarget, TargetToSet, Options, Matched),
    Matched = [].

test("empty maps produce empty matches", []) :-
    SetToTarget = _{},
    TargetToSet = _{},
    Options = _{k: 5, threshold: 0.5, tau_one_to_one: 0.5,
                tau_one_to_many: none, tau_many_to_one: none},
    resolve_match(SetToTarget, TargetToSet, Options, Matched),
    Matched = [].

:- end_tests(search_resolve_matching).

% ==========================================================================
% Candidates proxy tests (stub-based)
% ==========================================================================

:- begin_tests(search_resolve_candidates_proxy).

test("io_candidates_forward calls engine /candidates and returns response",
     [ setup((setup_temp_store(State),
              create_db_without_schema("admin", "canddb"),
              tdb_search:clean_tdb_search_test_env,
              tdb_search:start_push_stub(Port),
              format(atom(Endpoint_URL), "http://127.0.0.1:~w", [Port]),
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', Endpoint_URL),
              setenv('TERMINUSDB_SEARCH_ADMIN_USER', admin),
              setenv('TERMINUSDB_SEARCH_ADMIN_SECRET', root)
             )),
       cleanup((tdb_search:stop_push_stub(Port),
                tdb_search:clean_tdb_search_test_env,
                teardown_temp_store(State)))
     ]) :-
    tdb_search:tdb_search_endpoint(Endpoint),
    io_candidates_forward(Endpoint, "admin/canddb", "c0", [],
                          _{threshold_set: 0.5, threshold_target: 0.5, k: 5},
                          Response_Body),
    atom_json_dict(Response_Body, Response),
    get_dict(set_to_target, Response, SetMap),
    get_dict("doc/set_a", SetMap, Neighbours),
    member(_{id: "doc/target_a", distance: 0.1}, Neighbours),
    tdb_search:stub_received(candidates_called, true).

test("io_candidates_forward refuses when endpoint is not configured",
     [ setup(tdb_search:clean_tdb_search_test_env),
       cleanup(tdb_search:clean_tdb_search_test_env),
       throws(error(search_requires_tdb_search_backend, _))
     ]) :-
    io_candidates_forward("http://x:80", "d", "c", [], _{}, _).

:- end_tests(search_resolve_candidates_proxy).
