:- module(tdb_search, [
    % Push driver
    io_push_delta/4,
    io_index_branch/3,
    io_auto_push_worker/2,
    validation_is_index_enabled/1,
    path_to_domain/2,
    validate_index_path/1,
    % Search fronting
    io_search_forward/7,
    io_similar_forward/7,
    io_duplicates_forward/6,
    io_statistics_forward/6,
    io_resolve_forward/6,
    io_compare_forward/4,
    io_delete_domain/2,
    ancestor_window/4,
    maybe_nudge_push/6,
    % Config
    tdb_search_endpoint/1,
    tdb_search_admin_user/1,
    tdb_search_admin_secret/1,
    clean_tdb_search_env/0
]).

/** <module> tdb-search plugin — push-based indexing + search fronting

Activates when TERMINUSDB_TDB_SEARCH_ENDPOINT is set. Provides:
- Push driver: GET /last-indexed → compute delta → POST /push (NDJSON stream)
- Auto-push-on-commit hook (fire-and-forget, never blocks commit)
- Search fronting: /api/search, /api/similar, /api/duplicates,
  /api/resolve, /api/statistics, /api/compare
- Post-delete hook: DELETE /domain on engine when data product is deleted
*/

:- use_module(core(plugin_api)).
:- use_module(core(document/history), [commits_changed_id/5]).
:- use_module(core(document), [get_document/3, all_class_frames/3,
                               schema_metadata_descriptor/3]).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(transaction/ref_entity), [branch_head_commit/3, commit_id_uri/3,
    commit_uri_to_history_commit_ids/3]).
:- use_module(core(util)).
:- use_module(core(account)).
:- use_module(core(account/capabilities), [resolve_descriptor_auth/6,
                                           user_key_user_id/4]).
:- use_module(core(triple), [super_user_authority/1, database_schema/2, xrdf/4]).
:- use_module(core(plugins)).
:- use_module(core(api/api_graphql)).
:- use_module(library(json)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_authenticate), [http_authorization_data/2]).
:- use_module(library(http/http_stream)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(readutil)).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).
:- use_module(library(yall)).
:- use_module(library(lists)).
:- use_module(library(dicts)).
:- use_module(library(url), [www_form_encode/2]).
:- use_module(library(aggregate)).
:- use_module(core(util/test_utils),
             [setup_temp_store/1, teardown_temp_store/1,
              create_db_without_schema/2, create_db_with_empty_schema/2,
              create_db_with_test_schema/2]).
:- use_module(core(api/api_document), [api_insert_documents/9]).
:- use_module(core(api/db_branch), [branch_create/5]).
:- use_module(core(api/db_delete), [delete_db/5]).
:- use_module(core(account/user_management)).
:- use_module(core(transaction/system_entity), [database_exists/2]).
:- use_module(library(http/http_client), [http_read_data/3]).

% ==========================================================================
% Config predicates — plugin-owned, using generic plugin_api env helpers
% ==========================================================================

tdb_search_endpoint(Endpoint) :-
    plugin_env('TERMINUSDB_TDB_SEARCH_ENDPOINT', Endpoint).

tdb_search_admin_user(User) :-
    plugin_consume_env_default('TERMINUSDB_SEARCH_ADMIN_USER', admin, User).

tdb_search_admin_secret(Secret) :-
    plugin_consume_env_default('TERMINUSDB_SEARCH_ADMIN_SECRET', root, Secret).

clean_tdb_search_env :-
    abolish_plugin_env('TERMINUSDB_TDB_SEARCH_ENDPOINT'),
    abolish_plugin_env_default('TERMINUSDB_SEARCH_ADMIN_USER', admin),
    abolish_plugin_env_default('TERMINUSDB_SEARCH_ADMIN_SECRET', root),
    unsetenv('TERMINUSDB_TDB_SEARCH_ENDPOINT'),
    unsetenv('TERMINUSDB_SEARCH_ADMIN_USER'),
    unsetenv('TERMINUSDB_SEARCH_ADMIN_SECRET').

% ==========================================================================
% HTTP Basic auth header for tdb-search calls
% ==========================================================================

tdb_search_auth_header(authorization(basic(User, Secret))) :-
    tdb_search_admin_user(User),
    tdb_search_admin_secret(Secret).

% ==========================================================================
% Embedding queries + api_index_jobs (moved from api_indexer.pl)
% ==========================================================================

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

% ==========================================================================
% Push driver (moved from api_indexer.pl)
% ==========================================================================

:- multifile http:post_data_hook/3.

http:post_data_hook(ndjson_push(Producer), Out, _HdrExtra) :-
    plugin_api:stream_ndjson(Out, Producer).

build_last_indexed_url(Endpoint, Domain, Branch, URL) :-
    plugin_api:encode_query_value(Domain, Enc_Domain),
    plugin_api:encode_query_value(Branch, Enc_Branch),
    format(atom(URL), "~w/last-indexed?domain=~w&branch=~w",
           [Endpoint, Enc_Domain, Enc_Branch]).

io_get_last_indexed(Endpoint, Domain, Branch, Result) :-
    tdb_search_auth_header(AuthHeader),
    build_last_indexed_url(Endpoint, Domain, Branch, URL),
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

io_stream_push(Endpoint, Domain, Branch, Target_Commit,
               Parent_Commit_Or_Empty, System_DB, Auth, Path,
               Maybe_Previous_Commit_Id, Commit_Id, Result) :-
    tdb_search_auth_header(AuthHeader),
    build_push_url(Endpoint, Domain, Branch, Target_Commit,
                   Parent_Commit_Or_Empty, URL),
    Producer = [Chunked_Stream]>>(
        api_index_jobs(
            System_DB,
            Auth,
            Chunked_Stream,
            [_S]>>true,
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
    handle_push_response(Status, Reply_Body, Result).

handle_push_response(200, Task_Id, accepted(Task_Id)) :- !.
handle_push_response(409, _Body, conflict_already_pushed) :- !.
handle_push_response(Status, Body, _) :-
    throw(error(tdb_search_push_failed(Status, Body), _)).

io_await_task_completion(Endpoint, Task_Id) :-
    io_await_task_completion_(Endpoint, Task_Id, 0.1, 60).

io_await_task_completion_(_Endpoint, Task_Id, _Backoff, 0) :-
    !,
    throw(error(tdb_search_task_poll_timeout(Task_Id), _)).
io_await_task_completion_(Endpoint, Task_Id, Backoff, Retries_Left) :-
    io_check_task(Endpoint, Task_Id, Status),
    (   Status = complete
    ->  true
    ;   Status = error(ErrorMsg)
    ->  throw(error(tdb_search_task_failed(Task_Id, ErrorMsg), _))
    ;   Status = pending
    ->  sleep(Backoff),
        Next_Backoff is min(Backoff * 2, 2.0),
        Next_Retries is Retries_Left - 1,
        io_await_task_completion_(Endpoint, Task_Id, Next_Backoff, Next_Retries)
    ;   throw(error(tdb_search_task_unknown_status(Task_Id, Status), _))
    ).

io_check_task(Endpoint, Task_Id, Status) :-
    tdb_search_auth_header(AuthHeader),
    plugin_api:encode_query_value(Task_Id, Enc_Task_Id),
    format(atom(URL), "~w/check?task_id=~w", [Endpoint, Enc_Task_Id]),
    setup_call_cleanup(
        http_open(URL, In,
                  [ status_code(Http_Status),
                    AuthHeader,
                    request_header('Accept' = 'application/json')
                  ]),
        read_string(In, _, Body_String),
        close(In)),
    interpret_check_response(Http_Status, Body_String, Status).

interpret_check_response(200, Body_String, Status) :-
    !,
    atom_json_dict(Body_String, Dict, [default_tag(json)]),
    get_dict(status, Dict, Status_Tag_Raw),
    (   atom(Status_Tag_Raw)
    ->  Status_Tag = Status_Tag_Raw
    ;   atom_string(Status_Tag, Status_Tag_Raw)
    ),
    (   Status_Tag == 'Complete'
    ->  Status = complete
    ;   Status_Tag == 'Pending'
    ->  Status = pending
    ;   throw(error(tdb_search_check_unexpected_status(Status_Tag, Body_String), _))
    ).
interpret_check_response(500, Body_String, error(Body_String)) :- !.
interpret_check_response(404, Body_String, error(Body_String)) :- !.
interpret_check_response(Other_Status, Body_String, _) :-
    throw(error(tdb_search_check_failed(Other_Status, Body_String), _)).

io_resolve_409(Endpoint, Domain, Branch, Commit) :-
    io_poll_until_indexed(Endpoint, Domain, Branch, Commit, 0.2, 30).

io_poll_until_indexed(_Endpoint, _Domain, _Branch, _Commit, _Backoff, 0) :-
    !,
    throw(error(tdb_search_409_resolution_timeout, _)).
io_poll_until_indexed(Endpoint, Domain, Branch, Commit, Backoff, Retries) :-
    io_get_last_indexed(Endpoint, Domain, Branch, Result),
    get_dict(commit, Result, Engine_Commit_Raw),
    normalise_commit_value(Engine_Commit_Raw, Engine_Commit),
    (   Engine_Commit \== null,
        Engine_Commit == Commit
    ->  true
    ;   sleep(Backoff),
        Next_Backoff is min(Backoff * 2, 2.0),
        Next_Retries is Retries - 1,
        io_poll_until_indexed(Endpoint, Domain, Branch, Commit,
                              Next_Backoff, Next_Retries)
    ).

io_handle_push_result(Endpoint, _Domain, _Branch, _Commit, accepted(Task_Id)) :-
    !,
    io_await_task_completion(Endpoint, Task_Id).
io_handle_push_result(Endpoint, Domain, Branch, Commit, conflict_already_pushed) :-
    !,
    io_resolve_409(Endpoint, Domain, Branch, Commit).

build_push_url(Endpoint, Domain, Branch, Target_Commit, none, URL) :-
    !,
    plugin_api:encode_query_value(Domain, Enc_Domain),
    plugin_api:encode_query_value(Branch, Enc_Branch),
    plugin_api:encode_query_value(Target_Commit, Enc_Target),
    format(atom(URL),
           "~w/push?domain=~w&branch=~w&target_commit=~w",
           [Endpoint, Enc_Domain, Enc_Branch, Enc_Target]).
build_push_url(Endpoint, Domain, Branch, Target_Commit, Parent_Commit, URL) :-
    plugin_api:encode_query_value(Domain, Enc_Domain),
    plugin_api:encode_query_value(Branch, Enc_Branch),
    plugin_api:encode_query_value(Target_Commit, Enc_Target),
    plugin_api:encode_query_value(Parent_Commit, Enc_Parent),
    format(atom(URL),
           "~w/push?domain=~w&branch=~w&target_commit=~w&parent_commit=~w",
           [Endpoint, Enc_Domain, Enc_Branch, Enc_Target, Enc_Parent]).

normalise_commit_value(@(null), null) :- !.
normalise_commit_value(null, null) :- !.
normalise_commit_value(Atom, String) :-
    atom(Atom),
    !,
    atom_string(Atom, String).
normalise_commit_value(String, String) :-
    string(String).

io_push_delta_(Endpoint, Domain, Branch_Name, Head_Commit_Id,
               _Head_Commit_Uri, Engine_Commit,
               _Repository_Descriptor, _System_DB, _Auth, _Path) :-
    Engine_Commit \== null,
    Engine_Commit == Head_Commit_Id,
    !.

io_push_delta_(Endpoint, Domain, Branch_Name, Head_Commit_Id,
               _Head_Commit_Uri, null,
               _Repository_Descriptor, System_DB, Auth, Path) :-
    !,
    io_stream_push(Endpoint, Domain, Branch_Name, Head_Commit_Id,
                   none, System_DB, Auth, Path,
                   none, Head_Commit_Id, Result),
    io_handle_push_result(Endpoint, Domain, Branch_Name, Head_Commit_Id, Result).

io_push_delta_(Endpoint, Domain, Branch_Name, _Head_Commit_Id,
               Head_Commit_Uri, Engine_Commit,
               Repository_Descriptor, System_DB, Auth, Path) :-
    Engine_Commit \== null,
    commit_uri_to_history_commit_ids(Repository_Descriptor,
                                     Head_Commit_Uri,
                                     History_Oldest_First),
    commits_after(Engine_Commit, History_Oldest_First, Forward_Range),
    do_or_die(
        Forward_Range \== [],
        error(tdb_search_push_no_forward_range(Engine_Commit), _)),
    io_push_commit_chain(Endpoint, Domain, Branch_Name,
                         Engine_Commit, Forward_Range,
                         System_DB, Auth, Path).

commits_after(Last_Commit, History, Forward_Range) :-
    (   append(_, [Last_Commit | Forward_Range], History)
    ->  true
    ;   throw(error(tdb_search_last_indexed_not_in_history(Last_Commit), _))
    ).

io_push_commit_chain(_Endpoint, _Domain, _Branch, _Parent, [],
                     _System_DB, _Auth, _Path) :- !.
io_push_commit_chain(Endpoint, Domain, Branch, Parent_Commit,
                     [Commit | Rest], System_DB, Auth, Path) :-
    io_stream_push(Endpoint, Domain, Branch, Commit,
                   Parent_Commit, System_DB, Auth, Path,
                   some(Parent_Commit), Commit, Result),
    io_handle_push_result(Endpoint, Domain, Branch, Commit, Result),
    io_push_commit_chain(Endpoint, Domain, Branch, Commit,
                         Rest, System_DB, Auth, Path).

descriptor_graphspec(Descriptor, GraphSpec) :-
    plugin_api:descriptor_to_path(Descriptor, GraphSpec).

path_to_domain(Path, Domain) :-
    pattern_string_split("/", Path, Segments_Unfiltered),
    exclude(=(""), Segments_Unfiltered, Segments),
    do_or_die(
        Segments = [Org, DB | _],
        error(invalid_path_for_domain(Path), _)),
    format(atom(Domain), "~w/~w", [Org, DB]).

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

io_push_delta(System_DB, Auth, Path, Branch_Name) :-
    validate_index_path(Path),
    do_or_die(
        tdb_search_endpoint(Endpoint),
        error(tdb_search_endpoint_not_configured(io_push_delta), _)),
    resolve_absolute_string_descriptor(Path, Descriptor),
    descriptor_graphspec(Descriptor, Domain),
    do_or_die(
        branch_descriptor{branch_name: Descriptor_Branch} :< Descriptor,
        error(push_requires_branch_descriptor(Path), _)),
    do_or_die(
        Descriptor_Branch == Branch_Name,
        error(branch_name_mismatch(Branch_Name, Descriptor_Branch, Path), _)),
    io_get_last_indexed(Endpoint, Domain, Branch_Name, Last_Indexed),
    get_dict(commit, Last_Indexed, Engine_Commit_Raw),
    normalise_commit_value(Engine_Commit_Raw, Engine_Commit_Or_Null),
    Repository_Descriptor = Descriptor.repository_descriptor,
    branch_head_commit(Repository_Descriptor, Branch_Name, Head_Commit_Uri),
    commit_id_uri(Repository_Descriptor, Head_Commit_Id, Head_Commit_Uri),
    io_push_delta_(Endpoint, Domain, Branch_Name, Head_Commit_Id,
                   Head_Commit_Uri, Engine_Commit_Or_Null,
                   Repository_Descriptor, System_DB, Auth, Path).

io_index_branch(System_DB, Auth, Path) :-
    validate_index_path(Path),
    do_or_die(
        tdb_search_endpoint(Endpoint),
        error(tdb_search_endpoint_not_configured(io_index_branch), _)),
    resolve_absolute_string_descriptor(Path, Descriptor),
    do_or_die(
        branch_descriptor{branch_name: Branch_Name} :< Descriptor,
        error(push_requires_branch_descriptor(Path), _)),
    io_push_delta(System_DB, Auth, Path, Branch_Name).

% ==========================================================================
% Auto-push-on-commit hook
% ==========================================================================

:- multifile plugins:post_commit_hook/2.

plugins:post_commit_hook(Validations, _Meta_Data) :-
    (   tdb_search_endpoint(_)
    ->  catch(
            forall(
                (   member(Validation, Validations),
                    validation_is_index_enabled(Validation),
                    get_dict(descriptor, Validation, Descriptor),
                    branch_descriptor{branch_name: Branch_Name} :< Descriptor,
                    descriptor_graphspec(Descriptor, Path)
                ),
                catch(
                    thread_create(
                        io_auto_push_worker(Path, Branch_Name),
                        _Thread_Id,
                        [detached(true)]
                    ),
                    Spawn_Error,
                    format(user_error,
                           "[ERROR] Auto-push thread spawn failed for ~w (~w): ~q~n",
                           [Path, Branch_Name, Spawn_Error])
                )
            ),
            Hook_Error,
            format(user_error,
                   "[ERROR] Auto-push hook generator failed: ~q~n",
                   [Hook_Error])
        )
    ;   true
    ).

validation_is_index_enabled(Validation) :-
    get_dict(descriptor, Validation, Descriptor),
    branch_descriptor{} :< Descriptor,
    get_dict(schema_objects, Validation, Schema_Objects),
    Schema_Objects \== [],
    once(xrdf(Schema_Objects, _Type, sys:metadata, _)).

io_auto_push_worker(Path, Branch_Name) :-
    catch(
        (   open_descriptor(system_descriptor{}, System_DB),
            super_user_authority(Auth),
            io_push_delta(System_DB, Auth, Path, Branch_Name)
        ),
        Error,
        catch(
            json_log_error_formatted(
                "[ERROR] Auto-push-on-commit failed for ~w (~w): ~q",
                [Path, Branch_Name, Error]),
            _Log_Error,
            format(user_error,
                   "[ERROR] Auto-push-on-commit failed for ~w (~w); error not printable~n",
                   [Path, Branch_Name])
        )
    ).

% ==========================================================================
% Search fronting (moved from api_search.pl)
% ==========================================================================

assert_search_backend :-
    do_or_die(
        tdb_search_endpoint(_),
        error(search_requires_tdb_search_backend, _)).

search_auth_header(authorization(basic(User, Secret))) :-
    tdb_search_admin_user(User),
    tdb_search_admin_secret(Secret).

ancestor_window(Repository_Descriptor, Head_Commit_Uri, Max_Count, Ancestors) :-
    commit_uri_to_history_commit_ids(Repository_Descriptor,
                                     Head_Commit_Uri,
                                     History_Oldest_First),
    reverse(History_Oldest_First, History_Newest_First),
    (   History_Newest_First = [_Head | Rest]
    ->  length_bounded_prefix(Rest, Max_Count, Ancestors)
    ;   Ancestors = []
    ).

length_bounded_prefix(List, Max, Prefix) :-
    length(List, Len),
    (   Len =< Max
    ->  Prefix = List
    ;   length(Prefix, Max),
        append(Prefix, _, List)
    ).

build_search_url(Endpoint, Domain, Commit, Ancestors, URL) :-
    plugin_api:encode_query_value(Domain, Enc_Domain),
    plugin_api:encode_query_value(Commit, Enc_Commit),
    ancestor_query_params(Ancestors, Ancestor_Params),
    format(atom(URL), "~w/search?domain=~w&commit=~w~w",
           [Endpoint, Enc_Domain, Enc_Commit, Ancestor_Params]).

build_similar_url(Endpoint, Domain, Commit, Ancestors, URL) :-
    plugin_api:encode_query_value(Domain, Enc_Domain),
    plugin_api:encode_query_value(Commit, Enc_Commit),
    ancestor_query_params(Ancestors, Ancestor_Params),
    format(atom(URL), "~w/similar?domain=~w&commit=~w~w",
           [Endpoint, Enc_Domain, Enc_Commit, Ancestor_Params]).

build_duplicates_url(Endpoint, Domain, Commit, URL) :-
    plugin_api:encode_query_value(Domain, Enc_Domain),
    plugin_api:encode_query_value(Commit, Enc_Commit),
    format(atom(URL), "~w/duplicates?domain=~w&commit=~w",
           [Endpoint, Enc_Domain, Enc_Commit]).

build_statistics_url(Endpoint, Domain, Commit, Ancestors, URL) :-
    plugin_api:encode_query_value(Domain, Enc_Domain),
    plugin_api:encode_query_value(Commit, Enc_Commit),
    ancestor_query_params(Ancestors, Ancestor_Params),
    format(atom(URL), "~w/statistics?domain=~w&commit=~w~w",
           [Endpoint, Enc_Domain, Enc_Commit, Ancestor_Params]).

ancestor_query_params([], "") :- !.
ancestor_query_params(Ancestors, ParamString) :-
    maplist(ancestor_param_fragment, Ancestors, Fragments),
    atomic_list_concat(Fragments, ParamString).

ancestor_param_fragment(Ancestor, Fragment) :-
    plugin_api:encode_query_value(Ancestor, Enc),
    format(atom(Fragment), "&ancestor=~w", [Enc]).

io_search_forward(Endpoint, Domain, Commit, Ancestors,
                  Extra_Params, Response_Body, Data_Version_Header) :-
    assert_search_backend,
    search_auth_header(AuthHeader),
    build_search_url(Endpoint, Domain, Commit, Ancestors, Base_URL),
    append_extra_params(Base_URL, Extra_Params, URL),
    io_forward_get(URL, AuthHeader, Response_Body, Data_Version_Header).

io_similar_forward(Endpoint, Domain, Commit, Ancestors,
                   Extra_Params, Response_Body, Data_Version_Header) :-
    assert_search_backend,
    search_auth_header(AuthHeader),
    build_similar_url(Endpoint, Domain, Commit, Ancestors, Base_URL),
    append_extra_params(Base_URL, Extra_Params, URL),
    io_forward_get(URL, AuthHeader, Response_Body, Data_Version_Header).

io_duplicates_forward(Endpoint, Domain, Commit,
                      Extra_Params, Response_Body, Data_Version_Header) :-
    assert_search_backend,
    search_auth_header(AuthHeader),
    build_duplicates_url(Endpoint, Domain, Commit, Base_URL),
    append_extra_params(Base_URL, Extra_Params, URL),
    io_forward_get(URL, AuthHeader, Response_Body, Data_Version_Header).

io_statistics_forward(Endpoint, Domain, Commit, Ancestors,
                      Response_Body, Data_Version_Header) :-
    assert_search_backend,
    search_auth_header(AuthHeader),
    build_statistics_url(Endpoint, Domain, Commit, Ancestors, URL),
    io_forward_get(URL, AuthHeader, Response_Body, Data_Version_Header).

build_compare_url(Endpoint, Method, URL) :-
    plugin_api:encode_query_value(Method, Enc_Method),
    format(atom(URL), "~w/compare?method=~w", [Endpoint, Enc_Method]).

io_compare_forward(Endpoint, Method, Body_Dict, Response_Body) :-
    assert_search_backend,
    search_auth_header(AuthHeader),
    build_compare_url(Endpoint, Method, URL),
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

handle_compare_response(Status, _Body, _URL) :-
    Status >= 200,
    Status < 300,
    !.
handle_compare_response(Status, Body, URL) :-
    throw(error(tdb_search_forward_failed(Status, Body, URL), _)).

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

normalise_data_version_header(Raw, none) :-
    (   var(Raw) ; Raw == '' ; Raw == "" ),
    !.
normalise_data_version_header(Raw, Raw).

handle_forward_response(Status, _Body, _URL) :-
    Status >= 200,
    Status < 300,
    !.
handle_forward_response(Status, Body, URL) :-
    throw(error(tdb_search_forward_failed(Status, Body, URL), _)).

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
    plugin_api:encode_query_value(Value, Enc_Value),
    format(atom(Fragment), "&~w=~w", [Key, Enc_Value]).

repeated_param_fragment(Key, Value, Fragment) :-
    plugin_api:encode_query_value(Value, Enc_Value),
    format(atom(Fragment), "&~w=~w", [Key, Enc_Value]).

maybe_nudge_push(none, _Commit, _System_DB, _Auth, _Path, _Branch) :- !.
maybe_nudge_push(Data_Version_Header, Commit, System_DB, Auth, Path, Branch) :-
    format(atom(Expected_DV), "commit:~w", [Commit]),
    (   Data_Version_Header == Expected_DV
    ->  true
    ;   catch(
            io_push_delta(System_DB, Auth, Path, Branch),
            Nudge_Error,
            format(user_error,
                   "[WARN] Search stale-version nudge failed for ~w: ~q~n",
                   [Path, Nudge_Error])
        )
    ).

build_resolve_url(Endpoint, Domain, Commit, URL) :-
    plugin_api:encode_query_value(Domain, Enc_Domain),
    plugin_api:encode_query_value(Commit, Enc_Commit),
    format(atom(URL), "~w/resolve?domain=~w&commit=~w",
           [Endpoint, Enc_Domain, Enc_Commit]).

io_resolve_forward(Endpoint, Domain, Commit, Ancestors,
                   Body_Dict, Response_Body) :-
    assert_search_backend,
    search_auth_header(AuthHeader),
    format(atom(Resolve_URL), "~w/resolve", [Endpoint]),
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

build_delete_domain_url(Endpoint, Domain, URL) :-
    plugin_api:encode_query_value(Domain, Enc_Domain),
    format(atom(URL), "~w/domain?domain=~w", [Endpoint, Enc_Domain]).

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

handle_delete_domain_response(Status, _Body, _URL) :-
    Status >= 200,
    Status < 300,
    !.
handle_delete_domain_response(404, _Body, _URL) :- !.
handle_delete_domain_response(Status, Body, URL) :-
    throw(error(tdb_search_delete_domain_failed(Status, Body, URL), _)).

% ==========================================================================
% Post-delete hook
% ==========================================================================

:- multifile plugins:post_delete_db_hook/2.

plugins:post_delete_db_hook(Organization, DB_Name) :-
    (   tdb_search_endpoint(Endpoint)
    ->  format(atom(Domain), "~w/~w", [Organization, DB_Name]),
        catch(io_delete_domain(Endpoint, Domain),
              Delete_Error,
              format(user_error,
                     "[ERROR] Failed to delete search domain '~w' from engine: ~q~n",
                     [Domain, Delete_Error]))
    ;   true
    ).

% ==========================================================================
% HTTP handlers (moved from routes.pl)
% ==========================================================================

search_handler(get, Path, Request, System_DB, Auth) :-
    search_handler(post, Path, Request, System_DB, Auth).
search_handler(post, Path, Request, System_DB, Auth) :-
    (   memberchk(search(Search), Request)
    ->  true
    ;   Search = []),
    search_request_body(Request, Body),
    plugin_api:api_report_errors(
        search,
        Request,
        (
            resolve_descriptor_auth(read, System_DB, Auth, Path, instance, Descriptor),
            do_or_die(tdb_search_endpoint(Endpoint),
                      error(tdb_search_endpoint_not_configured(search_handler), _)),
            do_or_die(
                branch_descriptor{branch_name: Branch_Name} :< Descriptor,
                error(search_requires_branch_descriptor(Path), _)),
            get_dict(repository_descriptor, Descriptor, Repository_Descriptor),
            branch_head_commit(Repository_Descriptor, Branch_Name, Head_Commit_Uri),
            commit_id_uri(Repository_Descriptor, Head_Commit_Id, Head_Commit_Uri),
            descriptor_graphspec(Descriptor, Domain),
            ancestor_window(Repository_Descriptor, Head_Commit_Uri, 10, Ancestors),
            search_extra_params(Search, Body, Extra_Params),
            catch(
                (   io_search_forward(Endpoint, Domain, Head_Commit_Id, Ancestors,
                                      Extra_Params, Response_Body, Data_Version_Header),
                    maybe_nudge_push(Data_Version_Header, Head_Commit_Id,
                                     System_DB, Auth, Path, Branch_Name),
                    reply_search_response(Request, Response_Body, Data_Version_Header)
                ),
                error(tdb_search_forward_failed(404, Engine_Body, _Fail_URL), _),
                (   catch(
                        io_push_delta(System_DB, Auth, Path, Branch_Name),
                        Nudge_Error,
                        format(user_error,
                               "[WARN] Search not-indexed nudge failed for ~w: ~q~n",
                               [Path, Nudge_Error])
                    ),
                    throw(error(search_not_indexed(Path, Engine_Body), _))
                )
            )
        )
    ).

similar_handler(get, Path, Request, System_DB, Auth) :-
    similar_handler(post, Path, Request, System_DB, Auth).
similar_handler(post, Path, Request, System_DB, Auth) :-
    (   memberchk(search(Search), Request)
    ->  true
    ;   Search = []),
    search_request_body(Request, Body),
    plugin_api:api_report_errors(
        search,
        Request,
        (
            resolve_descriptor_auth(read, System_DB, Auth, Path, instance, Descriptor),
            do_or_die(tdb_search_endpoint(Endpoint),
                      error(tdb_search_endpoint_not_configured(similar_handler), _)),
            do_or_die(
                branch_descriptor{branch_name: Branch_Name} :< Descriptor,
                error(search_requires_branch_descriptor(Path), _)),
            get_dict(repository_descriptor, Descriptor, Repository_Descriptor),
            branch_head_commit(Repository_Descriptor, Branch_Name, Head_Commit_Uri),
            commit_id_uri(Repository_Descriptor, Head_Commit_Id, Head_Commit_Uri),
            descriptor_graphspec(Descriptor, Domain),
            ancestor_window(Repository_Descriptor, Head_Commit_Uri, 10, Ancestors),
            similar_extra_params(Search, Body, Extra_Params),
            catch(
                (   io_similar_forward(Endpoint, Domain, Head_Commit_Id, Ancestors,
                                       Extra_Params, Response_Body, Data_Version_Header),
                    maybe_nudge_push(Data_Version_Header, Head_Commit_Id,
                                     System_DB, Auth, Path, Branch_Name),
                    reply_search_response(Request, Response_Body, Data_Version_Header)
                ),
                error(tdb_search_forward_failed(404, Engine_Body, _Fail_URL), _),
                (   catch(
                        io_push_delta(System_DB, Auth, Path, Branch_Name),
                        Nudge_Error,
                        format(user_error,
                               "[WARN] Similar not-indexed nudge failed for ~w: ~q~n",
                               [Path, Nudge_Error])
                    ),
                    throw(error(search_not_indexed(Path, Engine_Body), _))
                )
            )
        )
    ).

duplicates_handler(get, Path, Request, System_DB, Auth) :-
    (   memberchk(search(Search), Request)
    ->  true
    ;   Search = []),
    search_request_body(Request, Body),
    plugin_api:api_report_errors(
        search,
        Request,
        (
            resolve_descriptor_auth(read, System_DB, Auth, Path, instance, Descriptor),
            do_or_die(tdb_search_endpoint(Endpoint),
                      error(tdb_search_endpoint_not_configured(duplicates_handler), _)),
            do_or_die(
                branch_descriptor{branch_name: Branch_Name} :< Descriptor,
                error(search_requires_branch_descriptor(Path), _)),
            get_dict(repository_descriptor, Descriptor, Repository_Descriptor),
            branch_head_commit(Repository_Descriptor, Branch_Name, Head_Commit_Uri),
            commit_id_uri(Repository_Descriptor, Head_Commit_Id, Head_Commit_Uri),
            descriptor_graphspec(Descriptor, Domain),
            duplicates_extra_params(Search, Body, Extra_Params),
            catch(
                (   io_duplicates_forward(Endpoint, Domain, Head_Commit_Id,
                                          Extra_Params, Response_Body, Data_Version_Header),
                    reply_search_response(Request, Response_Body, Data_Version_Header)
                ),
                error(tdb_search_forward_failed(404, Engine_Body, _Fail_URL), _),
                (   catch(
                        io_push_delta(System_DB, Auth, Path, Branch_Name),
                        Nudge_Error,
                        format(user_error,
                               "[WARN] Duplicates not-indexed nudge failed for ~w: ~q~n",
                               [Path, Nudge_Error])
                    ),
                    throw(error(search_not_indexed(Path, Engine_Body), _))
                )
            )
        )
    ).

resolve_handler(post, Path, Request, System_DB, Auth) :-
    search_request_body(Request, Body),
    plugin_api:api_report_errors(
        search,
        Request,
        (
            resolve_descriptor_auth(read, System_DB, Auth, Path, instance, Descriptor),
            do_or_die(tdb_search_endpoint(Endpoint),
                      error(tdb_search_endpoint_not_configured(resolve_handler), _)),
            do_or_die(
                branch_descriptor{branch_name: Branch_Name} :< Descriptor,
                error(search_requires_branch_descriptor(Path), _)),
            get_dict(repository_descriptor, Descriptor, Repository_Descriptor),
            branch_head_commit(Repository_Descriptor, Branch_Name, Head_Commit_Uri),
            commit_id_uri(Repository_Descriptor, Head_Commit_Id, Head_Commit_Uri),
            descriptor_graphspec(Descriptor, Domain),
            ancestor_window(Repository_Descriptor, Head_Commit_Uri, 10, Ancestors),
            resolve_forward_body(Body, Forward_Body),
            catch(
                (   io_resolve_forward(Endpoint, Domain, Head_Commit_Id, Ancestors,
                                       Forward_Body, Response_Body),
                    plugin_api:write_cors_headers(Request),
                    format("Content-Type: application/json~n~n"),
                    write(Response_Body)
                ),
                error(tdb_search_forward_failed(404, Engine_Body, _Fail_URL), _),
                (   catch(
                        io_push_delta(System_DB, Auth, Path, Branch_Name),
                        Nudge_Error,
                        format(user_error,
                               "[WARN] Resolve not-indexed nudge failed for ~w: ~q~n",
                               [Path, Nudge_Error])
                    ),
                    throw(error(search_not_indexed(Path, Engine_Body), _))
                )
            )
        )
    ).

resolve_forward_body(Body, Forward_Body) :-
    findall(Key-Value,
            (   resolve_allowed_body_key(Key),
                get_dict(Key, Body, Value)
            ),
            Pairs),
    dict_pairs(Forward_Body, _, Pairs).

resolve_allowed_body_key(set_doc_types).
resolve_allowed_body_key(set_doc_ids).
resolve_allowed_body_key(target_doc_types).
resolve_allowed_body_key(target_doc_ids).
resolve_allowed_body_key(threshold).
resolve_allowed_body_key(tau_one_to_one).
resolve_allowed_body_key(tau_one_to_many).
resolve_allowed_body_key(tau_many_to_one).
resolve_allowed_body_key(k).

statistics_handler(get, Path, Request, System_DB, Auth) :-
    plugin_api:api_report_errors(
        search,
        Request,
        (
            resolve_descriptor_auth(read, System_DB, Auth, Path, instance, Descriptor),
            do_or_die(tdb_search_endpoint(Endpoint),
                      error(tdb_search_endpoint_not_configured(statistics_handler), _)),
            do_or_die(
                branch_descriptor{branch_name: Branch_Name} :< Descriptor,
                error(search_requires_branch_descriptor(Path), _)),
            get_dict(repository_descriptor, Descriptor, Repository_Descriptor),
            branch_head_commit(Repository_Descriptor, Branch_Name, Head_Commit_Uri),
            commit_id_uri(Repository_Descriptor, Head_Commit_Id, Head_Commit_Uri),
            descriptor_graphspec(Descriptor, Domain),
            ancestor_window(Repository_Descriptor, Head_Commit_Uri, 10, Ancestors),
            catch(
                (   io_statistics_forward(Endpoint, Domain, Head_Commit_Id, Ancestors,
                                          Response_Body, Data_Version_Header),
                    reply_search_response(Request, Response_Body, Data_Version_Header)
                ),
                error(tdb_search_forward_failed(404, Engine_Body, _Fail_URL), _),
                (   catch(
                        io_push_delta(System_DB, Auth, Path, Branch_Name),
                        Nudge_Error,
                        format(user_error,
                               "[WARN] Statistics not-indexed nudge failed for ~w: ~q~n",
                               [Path, Nudge_Error])
                    ),
                    throw(error(search_not_indexed(Path, Engine_Body), _))
                )
            )
        )
    ).

compare_handler(post, Request, _System_DB, Auth) :-
    do_or_die(
        Auth \== 'terminusdb://system/data/User/anonymous',
        error(authentication_incorrect(anonymous_not_allowed), _)),
    (   memberchk(search(Search), Request)
    ->  true
    ;   Search = []),
    search_request_body(Request, Body),
    plugin_api:api_report_errors(
        search,
        Request,
        (
            do_or_die(tdb_search_endpoint(Endpoint),
                      error(tdb_search_endpoint_not_configured(compare_handler), _)),
            do_or_die(
                (   memberchk(method=Method, Search),
                    Method \== ''
                ),
                error(missing_parameter(method), _)),
            do_or_die(
                (   get_dict(source, Body, Source),
                    string(Source),
                    Source \== ""
                ),
                error(missing_parameter(source), _)),
            do_or_die(
                (   get_dict(target, Body, Target),
                    string(Target),
                    Target \== ""
                ),
                error(missing_parameter(target), _)),
            Forward_Body = _{source: Source, target: Target},
            io_compare_forward(Endpoint, Method, Forward_Body, Response_Body),
            reply_compare_response(Request, Response_Body)
        )
    ).

% ---- Helper predicates ----

reply_compare_response(Request, Response_Body) :-
    plugin_api:write_cors_headers(Request),
    format("Content-Type: application/json~n~n"),
    write(Response_Body).

search_request_body(Request, Body) :-
    (   memberchk(payload(Body0), Request),
        is_dict(Body0)
    ->  Body = Body0
    ;   Body = _{}
    ).

search_scalar_present(V) :- string(V), !, V \== "".
search_scalar_present(V) :- number(V), !.
search_scalar_present(V) :- atom(V), !, V \== '', V \== null.

merged_scalar_param(Key, Body, Search, Value) :-
    (   get_dict(Key, Body, Body_Value),
        search_scalar_present(Body_Value)
    ->  Value = Body_Value
    ;   memberchk(Key=Query_Value, Search),
        Query_Value \== '',
        Value = Query_Value
    ).

merged_repeated_param(Key, Body, Search, Values) :-
    (   get_dict(Key, Body, Body_Values),
        is_list(Body_Values),
        Body_Values \== []
    ->  Values = Body_Values
    ;   findall(V, member(Key=V, Search), Query_Values),
        Query_Values \== [],
        Values = Query_Values
    ).

search_extra_params(Search, Body, Params) :-
    findall(Param,
            search_extra_param(Search, Body, Param),
            Params).

search_extra_param(Search, Body, q=Q) :-
    merged_scalar_param(q, Body, Search, Q).
search_extra_param(Search, Body, mode=Mode) :-
    merged_scalar_param(mode, Body, Search, Mode).
search_extra_param(Search, Body, start=Start) :-
    merged_scalar_param(start, Body, Search, Start).
search_extra_param(Search, Body, count=Count) :-
    merged_scalar_param(count, Body, Search, Count).
search_extra_param(Search, Body, snippet=Snippet) :-
    merged_scalar_param(snippet, Body, Search, Snippet).
search_extra_param(Search, Body, doc_type=repeated(Types)) :-
    merged_repeated_param(doc_type, Body, Search, Types).
search_extra_param(Search, Body, doc_id=repeated(Ids)) :-
    merged_repeated_param(doc_id, Body, Search, Ids).

similar_extra_params(Search, Body, Params) :-
    findall(Param,
            similar_extra_param(Search, Body, Param),
            Params).

similar_extra_param(Search, Body, id=Id) :-
    merged_scalar_param(id, Body, Search, Id).
similar_extra_param(Search, Body, start=Start) :-
    merged_scalar_param(start, Body, Search, Start).
similar_extra_param(Search, Body, count=Count) :-
    merged_scalar_param(count, Body, Search, Count).
similar_extra_param(Search, Body, snippet=Snippet) :-
    merged_scalar_param(snippet, Body, Search, Snippet).
similar_extra_param(Search, Body, doc_type=repeated(Types)) :-
    merged_repeated_param(doc_type, Body, Search, Types).
similar_extra_param(Search, Body, doc_id=repeated(Ids)) :-
    merged_repeated_param(doc_id, Body, Search, Ids).

duplicates_extra_params(Search, Body, Params) :-
    findall(Param,
            duplicates_extra_param(Search, Body, Param),
            Params).

duplicates_extra_param(Search, Body, threshold=T) :-
    merged_scalar_param(threshold, Body, Search, T).
duplicates_extra_param(Search, Body, start=Start) :-
    merged_scalar_param(start, Body, Search, Start).
duplicates_extra_param(Search, Body, count=Count) :-
    merged_scalar_param(count, Body, Search, Count).
duplicates_extra_param(Search, Body, snippet=Snippet) :-
    merged_scalar_param(snippet, Body, Search, Snippet).
duplicates_extra_param(Search, Body, doc_type=repeated(Types)) :-
    merged_repeated_param(doc_type, Body, Search, Types).
duplicates_extra_param(Search, Body, doc_id=repeated(Ids)) :-
    merged_repeated_param(doc_id, Body, Search, Ids).
duplicates_extra_param(Search, Body, target_doc_type=repeated(Types)) :-
    merged_repeated_param(target_doc_type, Body, Search, Types).
duplicates_extra_param(Search, Body, target_doc_id=repeated(Ids)) :-
    merged_repeated_param(target_doc_id, Body, Search, Ids).

reply_search_response(Request, Response_Body, Data_Version_Header) :-
    plugin_api:write_cors_headers(Request),
    (   Data_Version_Header \== none
    ->  format("TerminusDB-Data-Version: ~w~n", [Data_Version_Header])
    ;   true
    ),
    format("Content-Type: application/json~n~n"),
    write(Response_Body).

% ==========================================================================
% Route registration
% ==========================================================================

:- plugin_api:register_route(api(search/Path),
    plugin_api:cors_handler(Method, tdb_search:search_handler(Path)),
    [method(Method), prefix, methods([options,get,post])]).

:- plugin_api:register_route(api(similar/Path),
    plugin_api:cors_handler(Method, tdb_search:similar_handler(Path)),
    [method(Method), prefix, methods([options,get,post])]).

:- plugin_api:register_route(api(duplicates/Path),
    plugin_api:cors_handler(Method, tdb_search:duplicates_handler(Path)),
    [method(Method), prefix, methods([options,get])]).

:- plugin_api:register_route(api(resolve/Path),
    plugin_api:cors_handler(Method, tdb_search:resolve_handler(Path)),
    [method(Method), prefix, methods([options,post])]).

:- plugin_api:register_route(api(statistics/Path),
    plugin_api:cors_handler(Method, tdb_search:statistics_handler(Path)),
    [method(Method), prefix, methods([options,get])]).

:- plugin_api:register_route(api(compare),
    plugin_api:cors_handler(Method, tdb_search:compare_handler),
    [method(Method), methods([options,post])]).

% ==========================================================================
% Stub HTTP server for unit tests
% ==========================================================================

:- dynamic stub_received/2.
:- dynamic stub_last_indexed_response/1.
:- dynamic stub_push_call_count/1.
:- dynamic stub_check_response/2.
:- dynamic stub_push_response_override/1.

push_stub_port(19876).

start_push_stub(Port) :-
    push_stub_port(Port),
    retractall(stub_received(_, _)),
    retractall(stub_last_indexed_response(_)),
    retractall(stub_push_call_count(_)),
    retractall(stub_check_response(_, _)),
    retractall(stub_push_response_override(_)),
    assertz(stub_push_call_count(0)),
    http_server(http_dispatch, [port(Port), workers(1)]).

stop_push_stub(Port) :-
    http_stop_server(Port, []),
    retractall(stub_received(_, _)),
    retractall(stub_last_indexed_response(_)),
    retractall(stub_push_call_count(_)),
    retractall(stub_check_response(_, _)),
    retractall(stub_push_response_override(_)).

:- http_handler('/last-indexed', push_stub_last_indexed, []).
:- http_handler('/push', push_stub_push, [methods([post])]).
:- http_handler('/check', push_stub_check, []).
:- http_handler('/domain', push_stub_domain_delete, [methods([delete])]).
:- http_handler('/resolve', push_stub_resolve, [methods([post])]).

push_stub_last_indexed(Request) :-
    (   memberchk(search(Search), Request)
    ->  true
    ;   Search = []
    ),
    assertz(stub_received(last_indexed, Search)),
    (   memberchk(authorization(AuthText), Request),
        http_authorization_data(AuthText, basic(User, Secret))
    ->  assertz(stub_received(last_indexed_auth, basic(User, Secret)))
    ;   true
    ),
    (   stub_last_indexed_response(ResponseJson)
    ->  true
    ;   ResponseJson = '{"branch":"main","commit":null,"version":0}'
    ),
    format("Content-Type: application/json~n~n"),
    write(ResponseJson).

push_stub_push(Request) :-
    (   memberchk(search(Search), Request)
    ->  true
    ;   Search = []
    ),
    retract(stub_push_call_count(N)),
    N1 is N + 1,
    assertz(stub_push_call_count(N1)),
    assertz(stub_received(push_params(N1), Search)),
    assertz(stub_received(push_params, Search)),
    (   memberchk(authorization(AuthText), Request),
        http_authorization_data(AuthText, basic(User, Secret))
    ->  assertz(stub_received(push_auth, basic(User, Secret)))
    ;   true
    ),
    (   memberchk(input(In), Request)
    ->  read_string(In, _, Body),
        assertz(stub_received(push_body(N1), Body))
    ;   true
    ),
    (   memberchk(transfer_encoding(chunked), Request)
    ->  assertz(stub_received(push_chunked, true))
    ;   true
    ),
    format(atom(Task_Id), "task-stub-~w", [N1]),
    format(atom(CheckJson), '{"status":"Complete","task_id":"~w"}', [Task_Id]),
    assertz(stub_check_response(Task_Id, CheckJson)),
    (   stub_push_response_override(status(Override_Code))
    ->  format("Status: ~w~n", [Override_Code]),
        format("Content-Type: text/plain~n~n"),
        format("Conflict")
    ;   format("Content-Type: text/plain~n~n"),
        write(Task_Id)
    ).

push_stub_check(Request) :-
    (   memberchk(search(Search), Request)
    ->  true
    ;   Search = []
    ),
    (   memberchk(task_id=Task_Id_Raw, Search)
    ->  (   atom(Task_Id_Raw)
        ->  atom_string(Task_Id_Raw, Task_Id_Str),
            atom_string(Task_Id, Task_Id_Str)
        ;   Task_Id = Task_Id_Raw
        )
    ;   Task_Id = unknown
    ),
    atom_string(Task_Id, Task_Id_Key),
    (   stub_check_response(Task_Id_Key, ResponseJson)
    ->  format("Content-Type: application/json~n~n"),
        write(ResponseJson)
    ;   format("Status: 404~n"),
        format("Content-Type: text/plain~n~n"),
        format("Task not found: ~w", [Task_Id])
    ).

push_stub_domain_delete(Request) :-
    (   memberchk(search(Search), Request)
    ->  true
    ;   Search = []
    ),
    assertz(stub_received(domain_delete, Search)),
    format("Status: 204~n"),
    format("Content-Type: text/plain~n~n").

push_stub_resolve(Request) :-
    (   memberchk(input(In), Request)
    ->  read_string(In, _, Body),
        assertz(stub_received(resolve_body, Body))
    ;   true
    ),
    assertz(stub_received(resolve_called, true)),
    format("Content-Type: application/json~n~n"),
    write('{"matches":[]}').

% ==========================================================================
% clean_tdb_search_test_env — test helper to clear plugin config tables + env vars
% ==========================================================================

clean_tdb_search_test_env :-
    abolish_plugin_env('TERMINUSDB_TDB_SEARCH_ENDPOINT'),
    abolish_table_subgoals(plugin_api_config:plugin_consume_env_default('TERMINUSDB_SEARCH_ADMIN_USER', admin, _)),
    abolish_table_subgoals(plugin_api_config:plugin_consume_env_default('TERMINUSDB_SEARCH_ADMIN_SECRET', root, _)),
    (   current_predicate(config:clear_indexer_backend_config/0)
    ->  config:clear_indexer_backend_config
    ;   true
    ),
    unsetenv('TERMINUSDB_INDEXER_BACKEND'),
    unsetenv('TERMINUSDB_SEMANTIC_INDEXER_ENDPOINT'),
    unsetenv('TERMINUSDB_TDB_SEARCH_ENDPOINT'),
    unsetenv('TERMINUSDB_SEARCH_ADMIN_USER'),
    unsetenv('TERMINUSDB_SEARCH_ADMIN_SECRET').

% ==========================================================================
% Indexer backend selector tests (moved from api_init.pl)
%
% These tests verify config-layer behaviour: indexer_backend/1,
% check_indexer_backend_config/0 (both in config(terminus_config)),
% and the plugin-owned tdb_search_endpoint/1, tdb_search_admin_user/1,
% tdb_search_admin_secret/1.
%
% They cannot live in terminus_config.pl because config is loaded before
% set_test_options(load(always)) during bootstrap, so plunit blocks there
% are discarded.
% ==========================================================================

:- begin_tests(tdb_search_indexer_backend_selector).

test("default backend is none when unset",
     [ setup(clean_tdb_search_test_env),
       cleanup(clean_tdb_search_test_env),
       true(Backend == none)
     ]) :-
    config:indexer_backend(Backend).

test("none with no endpoints set passes the startup check",
     [ setup(clean_tdb_search_test_env),
       cleanup(clean_tdb_search_test_env)
     ]) :-
    config:check_indexer_backend_config.

test("explicit none with no endpoints passes the startup check",
     [ setup((clean_tdb_search_test_env,
              setenv('TERMINUSDB_INDEXER_BACKEND', none))),
       cleanup(clean_tdb_search_test_env)
     ]) :-
    config:check_indexer_backend_config.

test("unknown backend value fails loud",
     [ setup((clean_tdb_search_test_env,
              setenv('TERMINUSDB_INDEXER_BACKEND', wibble))),
       cleanup(clean_tdb_search_test_env),
       throws(error(bad_env_var_value('TERMINUSDB_INDEXER_BACKEND', wibble), _))
     ]) :-
    config:indexer_backend(_).

test("unknown backend value refuses startup",
     [ setup((clean_tdb_search_test_env,
              setenv('TERMINUSDB_INDEXER_BACKEND', nonsense))),
       cleanup(clean_tdb_search_test_env),
       throws(error(bad_env_var_value('TERMINUSDB_INDEXER_BACKEND', nonsense), _))
     ]) :-
    config:check_indexer_backend_config.

test("http_vectorlink with its endpoint resolves and passes the check",
     [ setup((clean_tdb_search_test_env,
              setenv('TERMINUSDB_INDEXER_BACKEND', http_vectorlink),
              setenv('TERMINUSDB_SEMANTIC_INDEXER_ENDPOINT', 'http://vectorlink:8080'))),
       cleanup(clean_tdb_search_test_env),
       true(Backend == http_vectorlink)
     ]) :-
    config:check_indexer_backend_config,
    config:indexer_backend(Backend).

test("http_tdb_search with its endpoint resolves and passes the check",
     [ setup((clean_tdb_search_test_env,
              setenv('TERMINUSDB_INDEXER_BACKEND', http_tdb_search),
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', 'http://tdb-search:8080'))),
       cleanup(clean_tdb_search_test_env),
       true(Endpoint == 'http://tdb-search:8080')
     ]) :-
    config:check_indexer_backend_config,
    tdb_search_endpoint(Endpoint).

test("http_vectorlink without its endpoint refuses startup",
     [ setup((clean_tdb_search_test_env,
              setenv('TERMINUSDB_INDEXER_BACKEND', http_vectorlink))),
       cleanup(clean_tdb_search_test_env),
       throws(error(indexer_backend_incomplete(http_vectorlink, _), _))
     ]) :-
    config:check_indexer_backend_config.

test("http_tdb_search without its endpoint refuses startup",
     [ setup((clean_tdb_search_test_env,
              setenv('TERMINUSDB_INDEXER_BACKEND', http_tdb_search))),
       cleanup(clean_tdb_search_test_env),
       throws(error(indexer_backend_incomplete(http_tdb_search, _), _))
     ]) :-
    config:check_indexer_backend_config.

test("none with legacy endpoint set is ambiguous and refuses startup",
     [ setup((clean_tdb_search_test_env,
              setenv('TERMINUSDB_SEMANTIC_INDEXER_ENDPOINT', 'http://vectorlink:8080'))),
       cleanup(clean_tdb_search_test_env),
       throws(error(indexer_backend_ambiguous(none, _), _))
     ]) :-
    config:check_indexer_backend_config.

test("none with tdb-search endpoint set is ambiguous and refuses startup",
     [ setup((clean_tdb_search_test_env,
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', 'http://tdb-search:8080'))),
       cleanup(clean_tdb_search_test_env),
       throws(error(indexer_backend_ambiguous(none, _), _))
     ]) :-
    config:check_indexer_backend_config.

test("http_vectorlink with both endpoints set is ambiguous and refuses startup",
     [ setup((clean_tdb_search_test_env,
              setenv('TERMINUSDB_INDEXER_BACKEND', http_vectorlink),
              setenv('TERMINUSDB_SEMANTIC_INDEXER_ENDPOINT', 'http://vectorlink:8080'),
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', 'http://tdb-search:8080'))),
       cleanup(clean_tdb_search_test_env),
       throws(error(indexer_backend_ambiguous(http_vectorlink, _), _))
     ]) :-
    config:check_indexer_backend_config.

test("http_tdb_search with both endpoints set is ambiguous and refuses startup",
     [ setup((clean_tdb_search_test_env,
              setenv('TERMINUSDB_INDEXER_BACKEND', http_tdb_search),
              setenv('TERMINUSDB_SEMANTIC_INDEXER_ENDPOINT', 'http://vectorlink:8080'),
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', 'http://tdb-search:8080'))),
       cleanup(clean_tdb_search_test_env),
       throws(error(indexer_backend_ambiguous(http_tdb_search, _), _))
     ]) :-
    config:check_indexer_backend_config.

test("admin user defaults to admin",
     [ setup((clean_tdb_search_test_env,
              unsetenv('TERMINUSDB_SEARCH_ADMIN_USER'))),
       cleanup((clean_tdb_search_test_env,
                config:clear_indexer_backend_config)),
       true(User == admin)
     ]) :-
    tdb_search_admin_user(User).

test("admin secret defaults to root",
     [ setup((clean_tdb_search_test_env,
              unsetenv('TERMINUSDB_SEARCH_ADMIN_SECRET'))),
       cleanup((clean_tdb_search_test_env,
                config:clear_indexer_backend_config)),
       true(Secret == root)
     ]) :-
    tdb_search_admin_secret(Secret).

:- end_tests(tdb_search_indexer_backend_selector).

% ==========================================================================
% Push driver unit tests
% ==========================================================================

:- begin_tests(tdb_search_push_driver).

test("path_to_domain extracts org/db from short path",
     [true(Domain == 'admin/testdb')]) :-
    tdb_search:path_to_domain("admin/testdb", Domain).

test("path_to_domain extracts org/db from full branch path",
     [true(Domain == 'myorg/mydb')]) :-
    tdb_search:path_to_domain("myorg/mydb/local/branch/main", Domain).

test("path_to_domain extracts org/db from commit path",
     [true(Domain == 'org/db')]) :-
    tdb_search:path_to_domain("org/db/local/commit/abc123", Domain).

test("path_to_domain fails on single-segment path",
     [throws(error(invalid_path_for_domain(_), _))]) :-
    tdb_search:path_to_domain("onlyone", _).

test("path_to_domain fails on empty path",
     [throws(error(invalid_path_for_domain(_), _))]) :-
    tdb_search:path_to_domain("", _).

test("descriptor_graphspec produces full graphspec for main branch",
     [ setup(setup_temp_store(State)),
       cleanup(teardown_temp_store(State))
     ]) :-
    create_db_without_schema("admin", "testdb"),
    resolve_absolute_string_descriptor("admin/testdb", Descriptor),
    tdb_search:descriptor_graphspec(Descriptor, GraphSpec).

test("descriptor_graphspec produces full graphspec for feature branch",
     [ setup(setup_temp_store(State)),
       cleanup(teardown_temp_store(State)),
       true(GraphSpec == 'admin/testdb/local/branch/feature-x')
     ]) :-
    create_db_without_schema("admin", "testdb"),
    open_descriptor(system_descriptor{}, System_DB),
    super_user_authority(Auth),
    branch_create(System_DB, Auth,
                  "admin/testdb/local/branch/feature-x",
                  branch("admin/testdb"), _),
    resolve_absolute_string_descriptor("admin/testdb/local/branch/feature-x", FeatureDesc),
    tdb_search:descriptor_graphspec(FeatureDesc, GraphSpec).

test("descriptor_graphspec graphspec branch agrees with descriptor branch_name",
     [ setup(setup_temp_store(State)),
       cleanup(teardown_temp_store(State))
     ]) :-
    create_db_without_schema("admin", "testdb2"),
    resolve_absolute_string_descriptor("admin/testdb2", Descriptor),
    branch_descriptor{branch_name: Branch} :< Descriptor,
    tdb_search:descriptor_graphspec(Descriptor, GraphSpec),
    atom_string(GraphSpec, GS_String),
    format(atom(Expected_Suffix), "/branch/~w", [Branch]),
    sub_atom(GS_String, _, _, _, Expected_Suffix).

test("build_push_url with parent_commit includes parent_commit param",
     [true(URL == 'http://engine:8080/push?domain=admin%2fdb&branch=main&target_commit=head1&parent_commit=prev1')]) :-
    tdb_search:build_push_url("http://engine:8080", "admin/db", "main",
                               "head1", "prev1", URL).

test("build_push_url with none parent omits parent_commit param",
     [true(URL == 'http://engine:8080/push?domain=admin%2fdb&branch=main&target_commit=head1')]) :-
    tdb_search:build_push_url("http://engine:8080", "admin/db", "main",
                               "head1", none, URL).

test("handle_push_response 200 returns accepted(Task_Id)",
     [true(Result == accepted("task-abc"))]) :-
    tdb_search:handle_push_response(200, "task-abc", Result).

test("handle_push_response 409 returns conflict_already_pushed",
     [true(Result == conflict_already_pushed)]) :-
    tdb_search:handle_push_response(409, "Conflict", Result).

test("handle_push_response 401 throws loud failure",
     [throws(error(tdb_search_push_failed(401, "Unauthorized"), _))]) :-
    tdb_search:handle_push_response(401, "Unauthorized", _).

test("handle_push_response 500 throws loud failure",
     [throws(error(tdb_search_push_failed(500, "Internal error"), _))]) :-
    tdb_search:handle_push_response(500, "Internal error", _).

test("io_push_delta refuses when endpoint is not configured",
     [ setup(clean_tdb_search_test_env),
       cleanup(clean_tdb_search_test_env),
       throws(error(tdb_search_endpoint_not_configured(io_push_delta), _))
     ]) :-
    io_push_delta(_, _, "admin/testdb", "main").

test("io_index_branch refuses when endpoint is not configured",
     [ setup(clean_tdb_search_test_env),
       cleanup(clean_tdb_search_test_env),
       throws(error(tdb_search_endpoint_not_configured(io_index_branch), _))
     ]) :-
    io_index_branch(_, _, "admin/testdb").

test("io_push_delta does nothing when engine is at HEAD",
     [ setup((setup_temp_store(State),
              create_db_without_schema("admin", "testdb2"),
              clean_tdb_search_test_env,
              start_push_stub(Port),
              format(atom(Endpoint_URL), "http://127.0.0.1:~w", [Port]),
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', Endpoint_URL)
             )),
       cleanup((stop_push_stub(Port),
                clean_tdb_search_test_env,
                teardown_temp_store(State)))
     ]) :-
    resolve_absolute_string_descriptor("admin/testdb2", Descriptor),
    create_context(Descriptor, commit_info{author:"test", message:"data"}, Context),
    with_transaction(Context, ask(Context, insert(x,y,z)), _),
    Repository_Descriptor = Descriptor.repository_descriptor,
    branch_head_commit(Repository_Descriptor, "main", Head_Uri),
    commit_id_uri(Repository_Descriptor, Head_Commit_Id, Head_Uri),
    retractall(tdb_search:stub_last_indexed_response(_)),
    format(atom(ResponseJson), '{"branch":"main","commit":"~w","version":5}', [Head_Commit_Id]),
    assertz(tdb_search:stub_last_indexed_response(ResponseJson)),
    super_user_authority(Auth),
    open_descriptor(system_descriptor{}, System_DB),
    io_push_delta(System_DB, Auth, "admin/testdb2", "main"),
    tdb_search:stub_received(last_indexed, _),
    \+ tdb_search:stub_received(push_params, _).

test("commits_after returns suffix after the given commit",
     [true(Forward == ["c2", "c3", "c4"])]) :-
    tdb_search:commits_after("c1", ["c0", "c1", "c2", "c3", "c4"], Forward).

test("commits_after returns empty list when commit is last",
     [true(Forward == [])]) :-
    tdb_search:commits_after("c4", ["c0", "c1", "c2", "c3", "c4"], Forward).

test("commits_after throws when commit is not in history",
     [throws(error(tdb_search_last_indexed_not_in_history("missing"), _))]) :-
    tdb_search:commits_after("missing", ["c0", "c1", "c2"], _).

test("commits_after returns single-element suffix for second-to-last",
     [true(Forward == ["c2"])]) :-
    tdb_search:commits_after("c1", ["c0", "c1", "c2"], Forward).

test("normalise_commit_value handles JSON null (@(null))",
     [true(Result == null)]) :-
    tdb_search:normalise_commit_value(@(null), Result).

test("normalise_commit_value handles plain null atom",
     [true(Result == null)]) :-
    tdb_search:normalise_commit_value(null, Result).

test("normalise_commit_value coerces atom to string",
     [true(Result == "abc123")]) :-
    tdb_search:normalise_commit_value(abc123, Result).

test("normalise_commit_value passes through strings",
     [true(Result == "def456")]) :-
    tdb_search:normalise_commit_value("def456", Result).

test("build_push_url encodes slash in domain path",
     [true(URL == 'http://engine:8080/push?domain=org%2fdb%2flocal%2fbranch%2fmain&branch=main&target_commit=c1')]) :-
    tdb_search:build_push_url("http://engine:8080", "org/db/local/branch/main",
                               "main", "c1", none, URL).

test("build_push_url encodes ampersand and equals in branch name",
     [true(sub_atom(URL, _, _, _, 'branch=a%26b%3dc'))]) :-
    tdb_search:build_push_url("http://engine:8080", "d", "a&b=c",
                               "c1", none, URL).

test("build_last_indexed_url encodes slash in domain",
     [true(URL == 'http://engine:8080/last-indexed?domain=admin%2fdb&branch=main')]) :-
    tdb_search:build_last_indexed_url("http://engine:8080", "admin/db", "main", URL).

test("build_last_indexed_url encodes special chars in branch",
     [true(sub_atom(URL, _, _, _, 'branch=feat%2fx'))]) :-
    tdb_search:build_last_indexed_url("http://engine:8080", "d", "feat/x", URL).

test("io_await_task_completion succeeds when check returns Complete",
     [ setup((push_stub_port(Port),
              start_push_stub(Port),
              assertz(tdb_search:stub_check_response("task-test-1",
                  '{"status":"Complete","task_id":"task-test-1"}'))
             )),
       cleanup(stop_push_stub(Port))
     ]) :-
    push_stub_port(Port),
    format(atom(Endpoint), "http://127.0.0.1:~w", [Port]),
    tdb_search:io_await_task_completion(Endpoint, "task-test-1").

test("io_await_task_completion throws when task not found (404 from check)",
     [ setup((push_stub_port(Port),
              start_push_stub(Port)
             )),
       cleanup(stop_push_stub(Port)),
       throws(error(tdb_search_task_failed("task-nonexistent", _), _))
     ]) :-
    push_stub_port(Port),
    format(atom(Endpoint), "http://127.0.0.1:~w", [Port]),
    tdb_search:io_await_task_completion(Endpoint, "task-nonexistent").

test("interpret_check_response handles string 'Complete' from atom_json_dict",
     [true(Status == complete)]) :-
    tdb_search:interpret_check_response(200, '{"status":"Complete"}', Status).

test("interpret_check_response handles string 'Pending' from atom_json_dict",
     [true(Status == pending)]) :-
    tdb_search:interpret_check_response(200, '{"status":"Pending"}', Status).

test("interpret_check_response 500 returns error term",
     [true(Status == error("server crashed"))]) :-
    tdb_search:interpret_check_response(500, "server crashed", Status).

test("validate_index_path accepts 2-segment path (org/db)") :-
    tdb_search:validate_index_path("admin/testdb").

test("validate_index_path accepts 5-segment branch path") :-
    tdb_search:validate_index_path("admin/testdb/local/branch/main").

test("validate_index_path accepts 5-segment commit path") :-
    tdb_search:validate_index_path("admin/testdb/local/commit/abc123").

test("validate_index_path accepts atom input") :-
    tdb_search:validate_index_path('org/db').

test("validate_index_path accepts 5-segment with unusual branch name") :-
    tdb_search:validate_index_path("org/db/local/branch/feat-x").

test("validate_index_path rejects 1-segment path",
     [throws(error(invalid_index_path(_, wrong_segment_count(1, expected_2_or_5)), _))]) :-
    tdb_search:validate_index_path("onlyone").

test("validate_index_path rejects 3-segment path",
     [throws(error(invalid_index_path(_, wrong_segment_count(3, expected_2_or_5)), _))]) :-
    tdb_search:validate_index_path("admin/testdb/local").

test("validate_index_path rejects 4-segment path",
     [throws(error(invalid_index_path(_, wrong_segment_count(4, expected_2_or_5)), _))]) :-
    tdb_search:validate_index_path("admin/testdb/local/branch").

test("validate_index_path rejects 6-segment path",
     [throws(error(invalid_index_path(_, wrong_segment_count(6, expected_2_or_5)), _))]) :-
    tdb_search:validate_index_path("admin/db/local/branch/main/extra").

test("validate_index_path rejects 5-segment with bad segment-4 (not branch/commit)",
     [throws(error(invalid_index_path(_, bad_segment_4(_, expected_branch_or_commit)), _))]) :-
    tdb_search:validate_index_path("admin/db/local/tag/v1.0").

test("validate_index_path rejects _meta path (3-segment system form)",
     [throws(error(invalid_index_path(_, wrong_segment_count(3, expected_2_or_5)), _))]) :-
    tdb_search:validate_index_path("admin/db/_meta").

test("validate_index_path rejects empty string",
     [throws(error(invalid_index_path(_, wrong_segment_count(0, expected_2_or_5)), _))]) :-
    tdb_search:validate_index_path("").

test("io_index_branch rejects 3-segment path before any I/O",
     [ setup((clean_tdb_search_test_env,
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', 'http://127.0.0.1:9999'))),
       cleanup(clean_tdb_search_test_env),
       throws(error(invalid_index_path("admin/testdb/local",
                        wrong_segment_count(3, expected_2_or_5)), _))
     ]) :-
    io_index_branch(_, _, "admin/testdb/local").

test("io_index_branch rejects 4-segment path before any I/O",
     [ setup((clean_tdb_search_test_env,
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', 'http://127.0.0.1:9999'))),
       cleanup(clean_tdb_search_test_env),
       throws(error(invalid_index_path("admin/testdb/local/branch",
                        wrong_segment_count(4, expected_2_or_5)), _))
     ]) :-
    io_index_branch(_, _, "admin/testdb/local/branch").

test("io_index_branch rejects _meta path before any I/O",
     [ setup((clean_tdb_search_test_env,
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', 'http://127.0.0.1:9999'))),
       cleanup(clean_tdb_search_test_env),
       throws(error(invalid_index_path("admin/db/_meta",
                        wrong_segment_count(3, expected_2_or_5)), _))
     ]) :-
    io_index_branch(_, _, "admin/db/_meta").

test("io_index_branch rejects 5-segment with bad segment-4 before any I/O",
     [ setup((clean_tdb_search_test_env,
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', 'http://127.0.0.1:9999'))),
       cleanup(clean_tdb_search_test_env),
       throws(error(invalid_index_path("admin/db/local/tag/v1",
                        bad_segment_4(_, expected_branch_or_commit)), _))
     ]) :-
    io_index_branch(_, _, "admin/db/local/tag/v1").

test("io_push_delta rejects 3-segment path before any I/O",
     [ setup((clean_tdb_search_test_env,
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', 'http://127.0.0.1:9999'))),
       cleanup(clean_tdb_search_test_env),
       throws(error(invalid_index_path("admin/testdb/local",
                        wrong_segment_count(3, expected_2_or_5)), _))
     ]) :-
    io_push_delta(_, _, "admin/testdb/local", "main").

test("io_push_delta rejects _meta path before any I/O",
     [ setup((clean_tdb_search_test_env,
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', 'http://127.0.0.1:9999'))),
       cleanup(clean_tdb_search_test_env),
       throws(error(invalid_index_path("admin/db/_meta",
                        wrong_segment_count(3, expected_2_or_5)), _))
     ]) :-
    io_push_delta(_, _, "admin/db/_meta", "main").

:- end_tests(tdb_search_push_driver).

% ==========================================================================
% Auto-push-on-commit hook tests
% ==========================================================================

:- begin_tests(tdb_search_auto_push_hook).

test("validation_is_index_enabled succeeds for schema with embedding metadata",
     [ setup((setup_temp_store(State),
              create_db_with_test_schema("admin", "hookdb"))),
       cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    super_user_authority(Auth),
    open_string('
[
  {
    "@type": "@context",
    "@base": "http://example.com/data/world/",
    "@schema": "http://example.com/schema/worldOntology#"
  },
  {
    "@type": "Class",
    "@id": "Animal",
    "@key": { "@type": "Lexical", "@fields": ["name"] },
    "name": "xsd:string",
    "@metadata": {
      "embedding": {
        "query": "query($id: ID){ Animal(id : $id) { name } }"
      }
    }
  }
]
', Stream),
    Options = [author("test"), full_replace(true), graph_type(schema), message("test schema")],
    api_insert_documents(System, Auth, "admin/hookdb", Stream, no_data_version, _, _, _, Options),
    resolve_absolute_string_descriptor("admin/hookdb", Descriptor),
    open_descriptor(Descriptor, Transaction),
    get_dict(schema_objects, Transaction, Schema_Objects),
    Validation = validation_object{
        descriptor: Descriptor,
        schema_objects: Schema_Objects,
        instance_objects: [],
        inference_objects: []
    },
    tdb_search:validation_is_index_enabled(Validation).

test("validation_is_index_enabled fails for schema without embedding metadata",
     [ setup((setup_temp_store(State),
              create_db_without_schema("admin", "hookdb2"))),
       cleanup(teardown_temp_store(State)),
       fail
     ]) :-
    resolve_absolute_string_descriptor("admin/hookdb2", Descriptor),
    open_descriptor(Descriptor, Transaction),
    get_dict(schema_objects, Transaction, Schema_Objects),
    Validation = validation_object{
        descriptor: Descriptor,
        schema_objects: Schema_Objects,
        instance_objects: [],
        inference_objects: []
    },
    tdb_search:validation_is_index_enabled(Validation).

test("post_commit_hook is no-op when tdb_search endpoint is not set",
     [ setup(clean_tdb_search_test_env),
       cleanup(clean_tdb_search_test_env)
     ]) :-
    \+ tdb_search:tdb_search_endpoint(_).

test("post_commit_hook spawns worker for indexed branch",
     [ setup((setup_temp_store(State),
              create_db_with_test_schema("admin", "hookdb3"),
              clean_tdb_search_test_env,
              start_push_stub(Port),
              format(atom(Endpoint_URL), "http://127.0.0.1:~w", [Port]),
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', Endpoint_URL),
              setenv('TERMINUSDB_SEARCH_ADMIN_SECRET', root)
             )),
       cleanup((stop_push_stub(Port),
                clean_tdb_search_test_env,
                teardown_temp_store(State)))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    super_user_authority(Auth),
    open_string('
[
  {
    "@type": "@context",
    "@base": "http://example.com/data/world/",
    "@schema": "http://example.com/schema/worldOntology#"
  },
  {
    "@type": "Class",
    "@id": "Animal",
    "@key": { "@type": "Lexical", "@fields": ["name"] },
    "name": "xsd:string",
    "@metadata": {
      "embedding": {
        "query": "query($id: ID){ Animal(id : $id) { name } }"
      }
    }
  }
]
', Stream),
    Options = [author("test"), full_replace(true), graph_type(schema), message("test schema")],
    api_insert_documents(System, Auth, "admin/hookdb3", Stream, no_data_version, _, _, _, Options),
    open_string('{"@type": "Animal", "name": "dog"}', Doc_Stream),
    Doc_Options = [author("test"), message("add dog")],
    api_insert_documents(System, Auth, "admin/hookdb3", Doc_Stream, no_data_version, _, _, _, Doc_Options),
    sleep(1.0),
    true.

test("post_commit_hook returns quickly even if engine is slow",
     [ setup((setup_temp_store(State),
              create_db_with_test_schema("admin", "hookdb4"),
              clean_tdb_search_test_env,
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', 'http://127.0.0.1:1'),
              setenv('TERMINUSDB_SEARCH_ADMIN_SECRET', root)
             )),
       cleanup((clean_tdb_search_test_env,
                teardown_temp_store(State)))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    super_user_authority(Auth),
    open_string('
[
  {
    "@type": "@context",
    "@base": "http://example.com/data/world/",
    "@schema": "http://example.com/schema/worldOntology#"
  },
  {
    "@type": "Class",
    "@id": "Animal",
    "@key": { "@type": "Lexical", "@fields": ["name"] },
    "name": "xsd:string",
    "@metadata": {
      "embedding": {
        "query": "query($id: ID){ Animal(id : $id) { name } }"
      }
    }
  }
]
', Stream),
    Options = [author("test"), full_replace(true), graph_type(schema), message("test schema")],
    api_insert_documents(System, Auth, "admin/hookdb4", Stream, no_data_version, _, _, _, Options),
    get_time(T0),
    open_string('{"@type": "Animal", "name": "cat"}', Doc_Stream),
    Doc_Options = [author("test"), message("add cat")],
    api_insert_documents(System, Auth, "admin/hookdb4", Doc_Stream, no_data_version, _, _, _, Doc_Options),
    get_time(T1),
    Elapsed is T1 - T0,
    Elapsed < 5.0.

test("io_auto_push_worker logs error on engine failure without raising",
     [ setup((clean_tdb_search_test_env,
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', 'http://127.0.0.1:1'),
              setenv('TERMINUSDB_SEARCH_ADMIN_SECRET', root)
             )),
       cleanup(clean_tdb_search_test_env)
     ]) :-
    tdb_search:io_auto_push_worker("admin/nonexistent", "main").

:- end_tests(tdb_search_auto_push_hook).

% ==========================================================================
% Search fronting + authz parity tests
% ==========================================================================

:- begin_tests(tdb_search_search_fronting).

test("build_search_url constructs correct URL with ancestors",
     [true(URL == 'http://engine:8080/search?domain=admin%2fdb&commit=abc123&ancestor=prev1&ancestor=prev2')]) :-
    tdb_search:build_search_url("http://engine:8080", "admin/db", "abc123",
                                ["prev1", "prev2"], URL).

test("build_search_url with no ancestors omits ancestor params",
     [true(URL == 'http://engine:8080/search?domain=admin%2fdb&commit=abc123')]) :-
    tdb_search:build_search_url("http://engine:8080", "admin/db", "abc123",
                                [], URL).

test("build_similar_url constructs correct URL",
     [true(URL == 'http://engine:8080/similar?domain=org%2fmydb&commit=def456&ancestor=anc1')]) :-
    tdb_search:build_similar_url("http://engine:8080", "org/mydb", "def456",
                                 ["anc1"], URL).

test("build_duplicates_url constructs correct URL without ancestors",
     [true(URL == 'http://engine:8080/duplicates?domain=admin%2fdb&commit=c99')]) :-
    tdb_search:build_duplicates_url("http://engine:8080", "admin/db", "c99", URL).

test("build_statistics_url constructs scoped URL with domain and commit",
     [true(URL == 'http://engine:8080/statistics?domain=admin%2fmydb&commit=abc123')]) :-
    tdb_search:build_statistics_url("http://engine:8080", "admin/mydb", "abc123",
                                    [], URL).

test("build_statistics_url includes ancestor params",
     [true(sub_atom(URL, _, _, _, '&ancestor=anc1'))]) :-
    tdb_search:build_statistics_url("http://engine:8080", "admin/db", "c1",
                                    ["anc1"], URL).

test("build_search_url encodes slashes in domain",
     [true(sub_atom(URL, _, _, _, 'domain=org%2fdb%2flocal%2fbranch%2fmain'))]) :-
    tdb_search:build_search_url("http://e:80", "org/db/local/branch/main",
                                "c1", [], URL).

test("ancestor_window returns ancestors nearest first excluding HEAD",
     [ setup(setup_temp_store(State)),
       cleanup(teardown_temp_store(State))
     ]) :-
    create_db_without_schema("admin", "ancdb"),
    resolve_absolute_string_descriptor("admin/ancdb", Descriptor),
    create_context(Descriptor, commit_info{author:"t", message:"c1"}, Ctx1),
    with_transaction(Ctx1, ask(Ctx1, insert(a, b, c)), _),
    create_context(Descriptor, commit_info{author:"t", message:"c2"}, Ctx2),
    with_transaction(Ctx2, ask(Ctx2, insert(d, e, f)), _),
    create_context(Descriptor, commit_info{author:"t", message:"c3"}, Ctx3),
    with_transaction(Ctx3, ask(Ctx3, insert(g, h, i)), _),
    Repository_Descriptor = Descriptor.repository_descriptor,
    branch_head_commit(Repository_Descriptor, "main", Head_Uri),
    commit_id_uri(Repository_Descriptor, Head_Commit_Id, Head_Uri),
    ancestor_window(Repository_Descriptor, Head_Uri, 10, Ancestors),
    \+ memberchk(Head_Commit_Id, Ancestors),
    length(Ancestors, 2).

test("ancestor_window respects max count",
     [ setup(setup_temp_store(State)),
       cleanup(teardown_temp_store(State))
     ]) :-
    create_db_without_schema("admin", "ancdb2"),
    resolve_absolute_string_descriptor("admin/ancdb2", Descriptor),
    create_context(Descriptor, commit_info{author:"t", message:"c1"}, Ctx1),
    with_transaction(Ctx1, ask(Ctx1, insert(a, b, c)), _),
    create_context(Descriptor, commit_info{author:"t", message:"c2"}, Ctx2),
    with_transaction(Ctx2, ask(Ctx2, insert(d, e, f)), _),
    create_context(Descriptor, commit_info{author:"t", message:"c3"}, Ctx3),
    with_transaction(Ctx3, ask(Ctx3, insert(g, h, i)), _),
    Repository_Descriptor = Descriptor.repository_descriptor,
    branch_head_commit(Repository_Descriptor, "main", Head_Uri),
    ancestor_window(Repository_Descriptor, Head_Uri, 1, Ancestors),
    length(Ancestors, 1).

test("io_search_forward refuses when endpoint is not configured",
     [ setup(clean_tdb_search_test_env),
       cleanup(clean_tdb_search_test_env),
       throws(error(search_requires_tdb_search_backend, _))
     ]) :-
    io_search_forward("http://x:80", "d", "c", [], [], _, _).

test("io_similar_forward refuses when endpoint is not configured",
     [ setup(clean_tdb_search_test_env),
       cleanup(clean_tdb_search_test_env),
       throws(error(search_requires_tdb_search_backend, _))
     ]) :-
    io_similar_forward("http://x:80", "d", "c", [], [], _, _).

test("io_duplicates_forward refuses when endpoint is not configured",
     [ setup(clean_tdb_search_test_env),
       cleanup(clean_tdb_search_test_env),
       throws(error(search_requires_tdb_search_backend, _))
     ]) :-
    io_duplicates_forward("http://x:80", "d", "c", [], _, _).

test("io_statistics_forward refuses when endpoint is not configured",
     [ setup(clean_tdb_search_test_env),
       cleanup(clean_tdb_search_test_env),
       throws(error(search_requires_tdb_search_backend, _))
     ]) :-
    io_statistics_forward("http://x:80", "admin/db", "c0", [], _, _).

test("authz parity: denied caller cannot search (resolve_descriptor_auth throws)",
     [ setup((setup_temp_store(State),
              create_db_without_schema("admin", "secretdb"),
              add_user("UnprivUser", some('pass123'), _URI)
             )),
       cleanup(teardown_temp_store(State)),
       throws(error(access_not_authorised(_, _, _), _))
     ]) :-
    open_descriptor(system_descriptor{}, System_DB),
    user_key_user_id(System_DB, 'UnprivUser', 'pass123', Auth),
    resolve_descriptor_auth(read, System_DB, Auth, "admin/secretdb", instance, _Descriptor).

test("authz parity: authorised caller passes resolve_descriptor_auth",
     [ setup((setup_temp_store(State),
              create_db_without_schema("admin", "opendb")
             )),
       cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System_DB),
    super_user_authority(Auth),
    resolve_descriptor_auth(read, System_DB, Auth, "admin/opendb", instance, _Descriptor).

test("authz parity: denied caller search never reaches engine stub",
     [ setup((setup_temp_store(State),
              create_db_without_schema("admin", "guardeddb"),
              add_user("DeniedUser", some('pass456'), _URI),
              clean_tdb_search_test_env,
              start_push_stub(Port),
              format(atom(Endpoint_URL), "http://127.0.0.1:~w", [Port]),
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', Endpoint_URL)
             )),
       cleanup((stop_push_stub(Port),
                clean_tdb_search_test_env,
                teardown_temp_store(State)))
     ]) :-
    open_descriptor(system_descriptor{}, System_DB),
    user_key_user_id(System_DB, 'DeniedUser', 'pass456', Auth),
    catch(
        (   resolve_descriptor_auth(read, System_DB, Auth,
                                    "admin/guardeddb", instance, _Desc),
            tdb_search:tdb_search_endpoint(Endpoint),
            io_search_forward(Endpoint, "admin/guardeddb", "fake_commit",
                              [], [], _Response, _DV)
        ),
        error(access_not_authorised(_, _, _), _),
        true
    ),
    \+ stub_received(_, _).

test("authz parity: denied caller cannot get statistics (resolve_descriptor_auth throws)",
     [ setup((setup_temp_store(State),
              create_db_without_schema("admin", "statsdb"),
              add_user("StatsBlockedUser", some('pass789'), _URI)
             )),
       cleanup(teardown_temp_store(State)),
       throws(error(access_not_authorised(_, _, _), _))
     ]) :-
    open_descriptor(system_descriptor{}, System_DB),
    user_key_user_id(System_DB, 'StatsBlockedUser', 'pass789', Auth),
    resolve_descriptor_auth(read, System_DB, Auth, "admin/statsdb", instance, _Descriptor).

test("authz parity: denied caller statistics never reaches engine stub",
     [ setup((setup_temp_store(State),
              create_db_without_schema("admin", "guardedstatsdb"),
              add_user("StatsDeniedUser", some('pass012'), _URI),
              clean_tdb_search_test_env,
              start_push_stub(Port),
              format(atom(Endpoint_URL), "http://127.0.0.1:~w", [Port]),
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', Endpoint_URL)
             )),
       cleanup((stop_push_stub(Port),
                clean_tdb_search_test_env,
                teardown_temp_store(State)))
     ]) :-
    open_descriptor(system_descriptor{}, System_DB),
    user_key_user_id(System_DB, 'StatsDeniedUser', 'pass012', Auth),
    catch(
        (   resolve_descriptor_auth(read, System_DB, Auth,
                                    "admin/guardedstatsdb", instance, _Desc),
            tdb_search:tdb_search_endpoint(Endpoint),
            io_statistics_forward(Endpoint, "admin/guardedstatsdb", "fake_commit",
                                  [], _Response, _DV)
        ),
        error(access_not_authorised(_, _, _), _),
        true
    ),
    \+ stub_received(_, _).

test("maybe_nudge_push does nothing when data version matches",
     [ setup((setup_temp_store(State),
              create_db_without_schema("admin", "nudgedb"),
              clean_tdb_search_test_env,
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', 'http://127.0.0.1:9999')
             )),
       cleanup((clean_tdb_search_test_env,
                teardown_temp_store(State)))
     ]) :-
    tdb_search:maybe_nudge_push("commit:abc123", "abc123",
                                _, _, "admin/nudgedb", "main").

test("maybe_nudge_push with none header does nothing",
     [ setup(setup_temp_store(State)),
       cleanup(teardown_temp_store(State))
     ]) :-
    tdb_search:maybe_nudge_push(none, "abc123", _, _, "admin/db", "main").

test("authz parity: denied caller cannot resolve (resolve_descriptor_auth throws)",
     [ setup((setup_temp_store(State),
              create_db_without_schema("admin", "resolvedb"),
              add_user("ResolveDeniedUser", some('pass321'), _URI)
             )),
       cleanup(teardown_temp_store(State)),
       throws(error(access_not_authorised(_, _, _), _))
     ]) :-
    open_descriptor(system_descriptor{}, System_DB),
    user_key_user_id(System_DB, 'ResolveDeniedUser', 'pass321', Auth),
    resolve_descriptor_auth(read, System_DB, Auth, "admin/resolvedb", instance, _Descriptor).

test("authz parity: authorised caller passes resolve for resolve endpoint",
     [ setup((setup_temp_store(State),
              create_db_without_schema("admin", "resolveopendb")
             )),
       cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System_DB),
    super_user_authority(Auth),
    resolve_descriptor_auth(read, System_DB, Auth, "admin/resolveopendb", instance, _Descriptor).

test("authz parity: denied caller resolve never reaches engine stub",
     [ setup((setup_temp_store(State),
              create_db_without_schema("admin", "guardedresolvedb"),
              add_user("ResolveBlockedUser", some('pass654'), _URI),
              clean_tdb_search_test_env,
              start_push_stub(Port),
              format(atom(Endpoint_URL), "http://127.0.0.1:~w", [Port]),
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', Endpoint_URL)
             )),
       cleanup((stop_push_stub(Port),
                clean_tdb_search_test_env,
                teardown_temp_store(State)))
     ]) :-
    open_descriptor(system_descriptor{}, System_DB),
    user_key_user_id(System_DB, 'ResolveBlockedUser', 'pass654', Auth),
    catch(
        (   resolve_descriptor_auth(read, System_DB, Auth,
                                    "admin/guardedresolvedb", instance, _Desc),
            tdb_search:tdb_search_endpoint(Endpoint),
            io_resolve_forward(Endpoint, "admin/guardedresolvedb", "fake_commit",
                               [], _{}, _Response)
        ),
        error(access_not_authorised(_, _, _), _),
        true
    ),
    \+ stub_received(_, _).

:- end_tests(tdb_search_search_fronting).

% ==========================================================================
% Resolve URL construction tests
% ==========================================================================

:- begin_tests(tdb_search_resolve_url_construction).

test("build_resolve_url constructs correct URL",
     [true(URL == 'http://engine:8080/resolve?domain=admin%2fdb&commit=abc123')]) :-
    tdb_search:build_resolve_url("http://engine:8080", "admin/db", "abc123", URL).

test("build_resolve_url encodes slashes in domain",
     [true(sub_atom(URL, _, _, _, 'domain=org%2fdb%2flocal%2fbranch%2fmain'))]) :-
    tdb_search:build_resolve_url("http://e:80", "org/db/local/branch/main", "c1", URL).

test("io_resolve_forward refuses when endpoint is not configured",
     [ setup(clean_tdb_search_test_env),
       cleanup(clean_tdb_search_test_env),
       throws(error(search_requires_tdb_search_backend, _))
     ]) :-
    io_resolve_forward("http://x:80", "d", "c", [], _{}, _).

:- end_tests(tdb_search_resolve_url_construction).

% ==========================================================================
% DELETE /domain trigger tests
% ==========================================================================

:- begin_tests(tdb_search_delete_domain_trigger).

test("build_delete_domain_url constructs correct URL",
     [true(URL == 'http://engine:8080/domain?domain=admin%2fmydb')]) :-
    tdb_search:build_delete_domain_url("http://engine:8080", "admin/mydb", URL).

test("io_delete_domain refuses when endpoint is not configured",
     [ setup(clean_tdb_search_test_env),
       cleanup(clean_tdb_search_test_env),
       throws(error(search_requires_tdb_search_backend, _))
     ]) :-
    tdb_search:io_delete_domain("http://x:80", "admin/mydb").

test("delete_db triggers post_delete_db_hook (stub receives DELETE)",
     [ setup((setup_temp_store(State),
              clean_tdb_search_test_env,
              start_push_stub(Port),
              format(atom(Endpoint_URL), "http://127.0.0.1:~w", [Port]),
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', Endpoint_URL),
              setenv('TERMINUSDB_SEARCH_ADMIN_USER', admin),
              setenv('TERMINUSDB_SEARCH_ADMIN_SECRET', root)
             )),
       cleanup((stop_push_stub(Port),
                clean_tdb_search_test_env,
                teardown_temp_store(State)))
     ]) :-
    create_db_without_schema("admin", "deleteme"),
    open_descriptor(system_descriptor{}, System_DB),
    super_user_authority(Auth),
    delete_db(System_DB, Auth, "admin", "deleteme", false),
    \+ database_exists("admin", "deleteme"),
    stub_received(domain_delete, _).

test("post_delete_db_hook is silent no-op when tdb_search endpoint is not set",
     [ setup((setup_temp_store(State),
              clean_tdb_search_test_env
             )),
       cleanup((clean_tdb_search_test_env,
                teardown_temp_store(State)))
     ]) :-
    ignore(plugins:post_delete_db_hook("admin", "nonexistent")).

:- end_tests(tdb_search_delete_domain_trigger).

% ==========================================================================
% Search fronting params tests (body/query parameter merge)
% ==========================================================================

:- begin_tests(tdb_search_fronting_params).

test("search q: body value overrides query value",
     [true(Params == [q="from-body"])]) :-
    tdb_search:search_extra_params([q='from-query'], _{q: "from-body"}, Params).

test("search q: query used when body absent (GET path, empty body)",
     [true(Params == [q='from-query'])]) :-
    tdb_search:search_extra_params([q='from-query'], _{}, Params).

test("search q: empty body value falls back to query",
     [true(Params == [q='from-query'])]) :-
    tdb_search:search_extra_params([q='from-query'], _{q: ""}, Params).

test("search scalar absent in both body and query is omitted",
     [true(Params == [])]) :-
    tdb_search:search_extra_params([], _{}, Params).

test("search doc_type: body list overrides query repeated values",
     [true(Params == [doc_type=repeated(["A", "B"])])]) :-
    tdb_search:search_extra_params([doc_type='X', doc_type='Y'],
                        _{doc_type: ["A", "B"]}, Params).

test("search doc_type: query repeated used when body absent",
     [true(Params == [doc_type=repeated(['X', 'Y'])])]) :-
    tdb_search:search_extra_params([doc_type='X', doc_type='Y'], _{}, Params).

test("search never forwards domain/commit/ancestor from body",
     [true(Params == [q="hi"])]) :-
    tdb_search:search_extra_params([],
                        _{q: "hi", domain: "evil/db", commit: "deadbeef",
                          ancestors: ["x"]},
                        Params).

test("search never forwards domain/commit from query",
     [true(Params == [q='hi'])]) :-
    tdb_search:search_extra_params([q='hi', domain='evil/db', commit='deadbeef'],
                        _{}, Params).

test("search ignores unknown body fields (allowlist only)",
     [true(Params == [q="hi"])]) :-
    tdb_search:search_extra_params([], _{q: "hi", wibble: "nope", '$inject': 1}, Params).

test("similar id: body value overrides query value",
     [true(Params == [id="body-id"])]) :-
    tdb_search:similar_extra_params([id='query-id'], _{id: "body-id"}, Params).

test("duplicates threshold: body value overrides query value",
     [true(Params == [threshold=0.25])]) :-
    tdb_search:duplicates_extra_params([threshold='0.9'], _{threshold: 0.25}, Params).

test("duplicates target_doc_type: body list overrides query repeated",
     [true(Params == [target_doc_type=repeated(["Buy"])])]) :-
    tdb_search:duplicates_extra_params([target_doc_type='Abt'],
                            _{target_doc_type: ["Buy"]}, Params).

:- end_tests(tdb_search_fronting_params).
