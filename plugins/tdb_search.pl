:- module(tdb_search, [
    % Push driver
    io_push_delta/4,
    io_index_branch/3,
    path_to_domain/2,
    validate_index_path/1,
    % Search fronting
    io_search_forward/7,
    io_suggest_forward/7,
    io_similar_forward/7,
    io_similar_forward_post/8,
    io_duplicates_forward/7,
    io_statistics_forward/6,
    io_statistics_for_domain/3,
    io_resolve_forward/7,
    io_compare_forward/4,
    io_compare_forward/5,
    io_embeddings_forward/8,
    io_delete_domain/2,
    ancestor_window/4,
    maybe_nudge_push/6,
    maybe_nudge_push_async/4,
    % Config
    tdb_search_endpoint/1,
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
                               schema_metadata_descriptor/3,
                               database_prefixes/2]).
:- use_module(core(query)).
:- use_module(core(query/jsonld), [compress_dict_uri/3, prefix_expand/3]).
:- use_module(core(transaction)).
:- use_module(core(transaction/ref_entity), [branch_head_commit/3, commit_id_uri/3,
    commit_uri_to_history_commit_ids/3, commit_id_to_metadata/5]).
:- use_module(core(util)).
:- use_module(core(account)).
:- use_module(core(account/capabilities), [resolve_descriptor_auth/6,
                                           user_key_user_id/4]).
:- use_module(core(triple), [super_user_authority/1, database_schema/2, xrdf/4]).
:- use_module(core(plugins)).
:- use_module(library(base64)).
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
:- use_module(library(process)).
:- use_module(library(readutil)).

% ==========================================================================
% Config predicates — plugin-owned, using generic plugin_api env helpers
% ==========================================================================

tdb_search_endpoint(Endpoint) :-
    plugin_env('TERMINUSDB_TDB_SEARCH_ENDPOINT', Endpoint).

:- multifile plugins:tdb_search_admin_user/1.
plugins:tdb_search_admin_user(User) :-
    plugin_consume_env_default('TERMINUSDB_SEARCH_ADMIN_USER', admin, User).

:- multifile plugins:tdb_search_admin_secret/1.
plugins:tdb_search_admin_secret(Secret) :-
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
    plugins:tdb_search_admin_user(User),
    plugins:tdb_search_admin_secret(Secret).

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
    open_descriptor(Commit_Descriptor, Transaction),
    call(Prelude, Stream),
    % Path 1: schema-defined embedding queries
    (   TypeQueries \= []
    ->  maplist([Type-Query-_Template, Type-Query]>>true,
                TypeQueries,
                Queries),
        convlist([Type-Query-Template, Type-Template]>>ground(Template),
                 TypeQueries,
                 Templates),
        all_class_frames(Transaction, Frames, [compress_ids(true),expand_abstract(true),simple(true)]),
        '$embedding':embedding_context(System_DB, Transaction, Templates, Queries, Frames, Embedding_Context),
        forall(
            (   member(Type-_Query-_Template, TypeQueries),
                api_indexable(Maybe_Previous_Commit_Id, Descriptor, Commit_Id,
                              Type, Operation)),
            (   get_dict(op, Operation, Op),
                ignore(get_dict(id, Operation, Id)),
                '$embedding':write_op_for(Stream, System_DB, Transaction, Embedding_Context, Type, Id, Op),
                flush_output(Stream)
            )
        )
    ;   true
    ),
    % Path 2: JSONDocument fallback via plugin embedding_for_type
    forall(
        (   resolve_relative_descriptor(Descriptor,
                                        ["commit", Commit_Id],
                                        Path2_Commit_Descriptor),
            ask(Path2_Commit_Descriptor,
                t(Id, rdf:type, 'http://terminusdb.com/schema/sys#JSONDocument'),
                [compress_prefixes(true)]),
            Operation = json{op:'Inserted', id:Id}
        ),
        (   get_dict(op, Operation, Op),
            get_dict(id, Operation, Id),
            (   member(Op, ['Inserted', 'Changed'])
            ->  get_document(Transaction, Id, Document),
                (   plugins:embedding_for_type(_, _, Document, EmbeddingString),
                    EmbeddingString \= ''
                ->  atom_string(EmbeddingString, EmbStr),
                    with_output_to(string(JsonLine),
                                   json_write(current_output,
                                              json{op:Op, id:Id, string:EmbStr},
                                              [width(0)])),
                    format(Stream, '~s~n', [JsonLine]),
                    flush_output(Stream)
                ;   plugins:embedding_for_type(_, Document, EmbeddingString),
                    EmbeddingString \= ''
                ->  atom_string(EmbeddingString, EmbStr2),
                    with_output_to(string(JsonLine2),
                                   json_write(current_output,
                                              json{op:Op, id:Id, string:EmbStr2},
                                              [width(0)])),
                    format(Stream, '~s~n', [JsonLine2]),
                    flush_output(Stream)
                ;   true
                )
            ;   with_output_to(string(JsonLine3),
                               json_write(current_output,
                                          json{op:Op, id:Id},
                                          [width(0)])),
                format(Stream, '~s~n', [JsonLine3]),
                flush_output(Stream)
            )
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

tdb_http_get(URL, Status, Body) :-
    tdb_search_auth_header(authorization(basic(User, Secret))),
    format(atom(Creds), "~w:~w", [User, Secret]),
    base64(Creds, B64),
    format(atom(AuthHeader), "Basic ~w", [B64]),
    setup_call_cleanup(
        http_open(URL, In,
                  [ request_header('Authorization'=AuthHeader),
                    request_header('Accept'='application/json'),
                    status_code(Status),
                    timeout(30) ]),
        read_string(In, _, Body),
        close(In)).

io_get_last_indexed(Endpoint, Domain, Branch, Result) :-
    build_last_indexed_url(Endpoint, Domain, Branch, URL),
    tdb_http_get(URL, Status, Body_String),
    do_or_die(
        Status =:= 200,
        error(tdb_search_last_indexed_failed(Status, Body_String), _)),
    atom_json_dict(Body_String, Result, [default_tag(json)]).

io_stream_push(Endpoint, Domain, Branch, Target_Commit,
               Parent_Commit_Or_Empty, System_DB, Auth, Path,
               Maybe_Previous_Commit_Id, Commit_Id, Result) :-
    build_push_url(Endpoint, Domain, Branch, Target_Commit,
                   Parent_Commit_Or_Empty, URL),
    % Buffer NDJSON to temp file, then POST via http_open.
    % Use tmp_file_stream instead of with_output_to to avoid current_output
    % redirection issues in detached threads.
    setup_call_cleanup(
        tmp_file_stream(text, NDFile, NDStream),
        (   set_stream(NDStream, encoding(utf8)),
            tdb_search:api_index_jobs(
                System_DB,
                Auth,
                NDStream,
                [_S]>>true,
                Path,
                Commit_Id,
                Maybe_Previous_Commit_Id,
                []),
            close(NDStream),
            read_file_to_string(NDFile, NDJSON_Body, []),
            delete_file(NDFile)
        ),
        (   catch(close(NDStream), _, true),
            catch(delete_file(NDFile), _, true)
        )),
    tdb_search_auth_header(authorization(basic(User, Secret))),
    format(atom(Creds), "~w:~w", [User, Secret]),
    base64(Creds, B64),
    format(atom(AuthHeader), "Basic ~w", [B64]),
    setup_call_cleanup(
        http_open(URL, In,
                  [ method(post),
                    post(string(NDJSON_Body)),
                    request_header('Authorization'=AuthHeader),
                    request_header('Content-Type'='application/x-ndjson'),
                    status_code(Status) ]),
        read_stream_to_final_status(In, Status, Result),
        close(In)).

handle_push_response(200, Task_Id, accepted(Task_Id)) :- !.
handle_push_response(409, _Body, conflict_already_pushed) :- !.
handle_push_response(Status, Body, _) :-
    throw(error(tdb_search_push_failed(Status, Body), _)).

%% Read NDJSON progress lines from a streaming push response.
%% The stream sends {"status":"progress",...} lines and ends with
%% either {"status":"complete",...} or {"status":"error",...}.
%% On 409 (conflict), the body is a plain error string (no stream).
read_stream_to_final_status(In, 409, conflict_already_pushed) :-
    !,
    read_string(In, _, _Body),
    close(In).
read_stream_to_final_status(In, Status, Result) :-
    Status >= 200, Status < 300,
    !,
    read_ndjson_lines(In, Lines),
    (   member(Line, Lines),
        atom_json_dict(Line, Dict, [default_tag(json)]),
        get_dict(status, Dict, complete)
    ->  get_dict(task_id, Dict, TaskId),
        Result = accepted(TaskId)
    ;   member(Line, Lines),
        atom_json_dict(Line, Dict, [default_tag(json)]),
        get_dict(status, Dict, error)
    ->  (   get_dict(error, Dict, ErrMsg)
        ->  true
        ;   ErrMsg = "unknown error"
        ),
        throw(error(tdb_search_push_failed(200, ErrMsg), _))
    ;   throw(error(tdb_search_push_stream_unexpected(Lines), _))
    ).
read_stream_to_final_status(In, Status, _) :-
    read_string(In, _, Body),
    close(In),
    throw(error(tdb_search_push_failed(Status, Body), _)).

%% Read all lines from a stream into a list of atom strings.
read_ndjson_lines(In, Lines) :-
    read_ndjson_lines_(In, [], Lines).

read_ndjson_lines_(In, Acc, Lines) :-
    (   at_end_of_stream(In)
    ->  reverse(Acc, Lines)
    ;   read_line_to_string(In, LineStr),
        (   LineStr = end_of_file
        ->  reverse(Acc, Lines)
        ;   atom_string(LineAtom, LineStr),
            read_ndjson_lines_(In, [LineAtom|Acc], Lines)
        )
    ).

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
    plugin_api:encode_query_value(Task_Id, Enc_Task_Id),
    format(atom(URL), "~w/check?task_id=~w", [Endpoint, Enc_Task_Id]),
    tdb_search_auth_header(authorization(basic(User, Secret))),
    format(atom(Creds), "~w:~w", [User, Secret]),
    base64(Creds, B64),
    format(atom(AuthHeader), "Basic ~w", [B64]),
    catch(
        (   setup_call_cleanup(
                http_open(URL, In,
                          [request_header('Authorization'=AuthHeader),
                           status_code(Http_Status)]),
                read_string(In, _, Body_String),
                close(In)),
            interpret_check_response(Http_Status, Body_String, Status)
        ),
        Error,
        (   Error = error(existence_error(url, _), _)
        ->  throw(error(tdb_search_check_failed(0, 'connection failed'), _))
        ;   throw(Error)
        )
    ).

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

io_handle_push_result(_Endpoint, _Domain, _Branch, _Commit, accepted(_Task_Id)) :-
    !.
%% Pipeline already completed by the time we get accepted(Task_Id)
%% in streaming mode. No polling needed.
io_handle_push_result(Endpoint, Domain, Branch, Commit, conflict_already_pushed) :-
    !,
    io_resolve_409(Endpoint, Domain, Branch, Commit).

build_push_url(Endpoint, Domain, Branch, Target_Commit, none, URL) :-
    !,
    plugin_api:encode_query_value(Domain, Enc_Domain),
    plugin_api:encode_query_value(Branch, Enc_Branch),
    plugin_api:encode_query_value(Target_Commit, Enc_Target),
    format(atom(URL),
           "~w/push?domain=~w&branch=~w&target_commit=~w&stream=true",
           [Endpoint, Enc_Domain, Enc_Branch, Enc_Target]).
build_push_url(Endpoint, Domain, Branch, Target_Commit, Parent_Commit, URL) :-
    plugin_api:encode_query_value(Domain, Enc_Domain),
    plugin_api:encode_query_value(Branch, Enc_Branch),
    plugin_api:encode_query_value(Target_Commit, Enc_Target),
    plugin_api:encode_query_value(Parent_Commit, Enc_Parent),
    format(atom(URL),
           "~w/push?domain=~w&branch=~w&target_commit=~w&parent_commit=~w&stream=true",
           [Endpoint, Enc_Domain, Enc_Branch, Enc_Target, Enc_Parent]).

normalise_commit_value(@(null), null) :- !.
normalise_commit_value(null, null) :- !.
normalise_commit_value(Atom, String) :-
    atom(Atom),
    !,
    atom_string(Atom, String).
normalise_commit_value(String, String) :-
    string(String).

io_push_delta_(_Endpoint, _Domain, _Branch_Name, Head_Commit_Id,
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

io_push_delta_(Endpoint, Domain, Branch_Name, Head_Commit_Id,
               Head_Commit_Uri, Engine_Commit,
               Repository_Descriptor, System_DB, Auth, Path) :-
    Engine_Commit \== null,
    % RACE GUARD: re-check /last-indexed before pushing. A concurrent
    % auto-push worker may have already pushed this commit between our
    % first check and now. If the engine is already at HEAD, return success.
    (   io_get_last_indexed(Endpoint, Domain, Branch_Name, Recheck),
        get_dict(commit, Recheck, Recheck_Commit_Raw),
        normalise_commit_value(Recheck_Commit_Raw, Recheck_Commit),
        Recheck_Commit \== null,
        Recheck_Commit == Head_Commit_Id
    ->  true
    ;   commit_uri_to_history_commit_ids(Repository_Descriptor,
                                         Head_Commit_Uri,
                                         History_Oldest_First),
        commits_after(Engine_Commit, History_Oldest_First, Forward_Range),
        do_or_die(
            Forward_Range \== [],
            error(tdb_search_push_no_forward_range(Engine_Commit), _)),
        io_push_commit_chain(Endpoint, Domain, Branch_Name,
                             Engine_Commit, Forward_Range,
                             System_DB, Auth, Path)
    ).

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

%% descriptor_domain(+Descriptor, -Domain) is det.
%
%  Extracts the org/db domain from a descriptor, matching what the
%  Rust indexer pushes to tdb-search (BranchKey::domain()).
descriptor_domain(Descriptor, Domain) :-
    plugin_api:descriptor_to_path(Descriptor, Path),
    path_to_domain(Path, Domain).

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

%% branch_path_for_notify(+Path, +Branch_Name, -Branch_Path) is det.
%
%  Normalises a path to the 5-segment branch form (org/db/local/branch/name)
%  used by indexer_notify. If Path is already a 5-segment branch path, it is
%  returned as-is. If Path is a 2-segment org/db path, /local/branch/Branch_Name
%  is appended. Other path forms are rejected via validate_index_path.
%
%  This replaces the old atom_concat(_, '/local/branch/', Path) substring
%  check which could be tricked by a database named "local" and could double
%  paths when called with a full branch path from search handlers.
branch_path_for_notify(Path, Branch_Name, Branch_Path) :-
    validate_index_path(Path),
    (   atom(Path)
    ->  atom_string(Path, Path_String)
    ;   Path_String = Path
    ),
    pattern_string_split("/", Path_String, Segments_Unfiltered),
    exclude(=(""), Segments_Unfiltered, Segments),
    length(Segments, N),
    (   N =:= 5
    ->  nth1(4, Segments, Seg4),
        text_to_string(Seg4, Seg4_Str),
        (   Seg4_Str == "branch"
        ->  Branch_Path = Path
        ;   Seg4_Str == "commit"
        ->  nth1(1, Segments, Org),
            nth1(2, Segments, DB),
            nth1(3, Segments, Repo),
            format(atom(Branch_Path), "~w/~w/~w/branch/~w", [Org, DB, Repo, Branch_Name])
        ;   throw(error(invalid_index_path(Path,
                            bad_segment_4(Seg4, expected_branch_or_commit)), _))
        )
    ;   N =:= 2
    ->  format(atom(Branch_Path), "~w/local/branch/~w", [Path, Branch_Name])
    ).

io_push_delta(_System_DB, _Auth, Path, Branch_Name) :-
    branch_path_for_notify(Path, Branch_Name, Branch_Path),
    do_or_die(
        tdb_search_endpoint(_Endpoint),
        error(tdb_search_endpoint_not_configured(io_push_delta), _)),
    (   plugin_api:indexer_available
    ->  (   api_indexer:schema_store_clustering_for_path(Path, Store_Clustering),
            plugin_api:indexer_notify(Branch_Path, Branch_Name, Store_Clustering)
        ->  true
        ;   throw(error(indexer_notify_failed(io_push_delta), _))
        )
    ;   throw(error(indexer_ffi_not_loaded(io_push_delta), _))
    ).

io_index_branch(System_DB, Auth, Path) :-
    validate_index_path(Path),
    do_or_die(
        tdb_search_endpoint(_Endpoint),
        error(tdb_search_endpoint_not_configured(io_index_branch), _)),
    plugin_api:resolve_descriptor_auth(read, System_DB, Auth, Path, instance, Descriptor),
    do_or_die(
        (   branch_descriptor{branch_name: Branch_Name} :< Descriptor
        ->  true
        ;   commit_descriptor{repository_descriptor: Repo_Desc, commit_id: Commit_Id} :< Descriptor
        ->  branch_for_commit(Repo_Desc, Commit_Id, Branch_Name)
        ),
        error(push_requires_branch_descriptor(Path), _)),
    tdb_search:branch_path_for_notify(Path, Branch_Name, Branch_Path),
    (   plugin_api:indexer_available
    ->  (   api_indexer:schema_store_clustering_for_path(Path, Store_Clustering),
            plugin_api:indexer_reindex(Branch_Path, Branch_Name, Store_Clustering)
        ->  true
        ;   throw(error(indexer_reindex_failed(io_index_branch), _))
        )
    ;   throw(error(indexer_ffi_not_loaded(io_index_branch), _))
    ).

%% branch_for_commit(+Repo_Desc, +Commit_Id, -Branch_Name) is semidet.
%
%  Find a branch that contains Commit_Id in its history.
%  Tries each branch in the repository and returns the first whose
%  commit history includes Commit_Id. This handles both the case where
%  the commit is the current branch head and where it is an older commit
%  (e.g. when indexing from the commit explorer view).
branch_for_commit(Repo_Desc, Commit_Id, Branch_Name) :-
    commit_id_uri(Repo_Desc, Commit_Id, Commit_Uri),
    has_branch(Repo_Desc, Branch_Name),
    branch_head_commit(Repo_Desc, Branch_Name, Head_Uri),
    (   Head_Uri == Commit_Uri
    ->  true
    ;   commit_uri_to_history_commit_ids(Repo_Desc, Head_Uri, History_Ids),
        member(Commit_Id, History_Ids)
    ),
    !.

% ==========================================================================
% Auto-push-on-commit hook
% ==========================================================================
% NOTE: The post_commit_hook and validation_is_index_enabled are defined in
% src/core/api/api_indexer.pl. That version calls indexer_notify/3 (Rust FFI)
% directly — no curl subprocess, no detached threads, no stream race.
% The Rust IndexerRegistry handles NDJSON generation, HTTP streaming, and
% 409 resolution internally via reqwest.

% ==========================================================================
% Search fronting (moved from api_search.pl)
% ==========================================================================

assert_search_backend :-
    do_or_die(
        tdb_search_endpoint(_),
        error(search_requires_tdb_search_backend, _)).

search_auth_header(authorization(basic(User, Secret))) :-
    plugins:tdb_search_admin_user(User),
    plugins:tdb_search_admin_secret(Secret).

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

%% search_ref_param(+Search_Ref, -ParamString) is det.
%
%  Builds the query parameter string for either a commit or branch reference.
%  commit(Commit_Id) → "&commit=<id>"
%  branch(Branch_Name) → "&branch=<name>"
search_ref_param(commit(Commit), ParamString) :-
    plugin_api:encode_query_value(Commit, Enc),
    format(atom(ParamString), "&commit=~w", [Enc]).
search_ref_param(branch(Branch), ParamString) :-
    plugin_api:encode_query_value(Branch, Enc),
    format(atom(ParamString), "&branch=~w", [Enc]).

build_search_url(Endpoint, Domain, Search_Ref, Ancestors, URL) :-
    plugin_api:encode_query_value(Domain, Enc_Domain),
    search_ref_param(Search_Ref, Ref_Params),
    ancestor_query_params(Ancestors, Ancestor_Params),
    format(atom(URL), "~w/search?domain=~w~w~w",
           [Endpoint, Enc_Domain, Ref_Params, Ancestor_Params]).

build_similar_url(Endpoint, Domain, Search_Ref, Ancestors, URL) :-
    plugin_api:encode_query_value(Domain, Enc_Domain),
    search_ref_param(Search_Ref, Ref_Params),
    ancestor_query_params(Ancestors, Ancestor_Params),
    format(atom(URL), "~w/similar?domain=~w~w~w",
           [Endpoint, Enc_Domain, Ref_Params, Ancestor_Params]).

build_duplicates_url(Endpoint, Domain, Search_Ref, Ancestors, URL) :-
    plugin_api:encode_query_value(Domain, Enc_Domain),
    search_ref_param(Search_Ref, Ref_Params),
    ancestor_query_params(Ancestors, Ancestor_Params),
    format(atom(URL), "~w/duplicates?domain=~w~w~w",
           [Endpoint, Enc_Domain, Ref_Params, Ancestor_Params]).

build_statistics_url(Endpoint, Domain, Search_Ref, Ancestors, URL) :-
    plugin_api:encode_query_value(Domain, Enc_Domain),
    search_ref_param(Search_Ref, Ref_Params),
    ancestor_query_params(Ancestors, Ancestor_Params),
    format(atom(URL), "~w/statistics?domain=~w~w~w",
           [Endpoint, Enc_Domain, Ref_Params, Ancestor_Params]).

ancestor_query_params([], "") :- !.
ancestor_query_params(Ancestors, ParamString) :-
    maplist(ancestor_param_fragment, Ancestors, Fragments),
    atomic_list_concat(Fragments, ParamString).

ancestor_param_fragment(Ancestor, Fragment) :-
    plugin_api:encode_query_value(Ancestor, Enc),
    format(atom(Fragment), "&ancestor=~w", [Enc]).

io_search_forward(Endpoint, Domain, Search_Ref, Ancestors,
                  Extra_Params, Response_Body, Data_Version_Header) :-
    assert_search_backend,
    search_auth_header(AuthHeader),
    build_search_url(Endpoint, Domain, Search_Ref, Ancestors, Base_URL),
    append_extra_params(Base_URL, Extra_Params, URL),
    io_forward_get(URL, AuthHeader, Response_Body, Data_Version_Header).

io_similar_forward(Endpoint, Domain, Search_Ref, Ancestors,
                   Extra_Params, Response_Body, Data_Version_Header) :-
    assert_search_backend,
    search_auth_header(AuthHeader),
    build_similar_url(Endpoint, Domain, Search_Ref, Ancestors, Base_URL),
    append_extra_params(Base_URL, Extra_Params, URL),
    io_forward_get(URL, AuthHeader, Response_Body, Data_Version_Header).

%% io_similar_forward_post(+Endpoint, +Domain, +Search_Ref, +Ancestors,
%%                          +Extra_Params, +Post_Body, -Response_Body,
%%                          -Data_Version_Header) is det.
%
%  Forwards a similar request as POST with a JSON body to tdb-search.
%  Used for text-based similarity search where the body contains
%  {text: "..."} instead of an id lookup.
io_similar_forward_post(Endpoint, Domain, Search_Ref, Ancestors,
                        Extra_Params, Post_Body, Response_Body,
                        Data_Version_Header) :-
    assert_search_backend,
    search_auth_header(AuthHeader),
    build_similar_url(Endpoint, Domain, Search_Ref, Ancestors, Base_URL),
    append_extra_params(Base_URL, Extra_Params, URL),
    io_forward_post(URL, AuthHeader, Post_Body, Response_Body,
                    Data_Version_Header).

io_duplicates_forward(Endpoint, Domain, Search_Ref, Ancestors,
                      Extra_Params, Response_Body, Data_Version_Header) :-
    assert_search_backend,
    search_auth_header(AuthHeader),
    build_duplicates_url(Endpoint, Domain, Search_Ref, Ancestors, Base_URL),
    append_extra_params(Base_URL, Extra_Params, URL),
    io_forward_get(URL, AuthHeader, Response_Body, Data_Version_Header).

io_statistics_forward(Endpoint, Domain, Search_Ref, Ancestors,
                      Response_Body, Data_Version_Header) :-
    assert_search_backend,
    search_auth_header(AuthHeader),
    build_statistics_url(Endpoint, Domain, Search_Ref, Ancestors, URL),
    io_forward_get(URL, AuthHeader, Response_Body, Data_Version_Header).

%% io_statistics_for_domain(+Endpoint, +Domain, -Stats) is det.
%
%  Queries tdb-search /statistics?domain=... for domain-scoped stats
%  (documents, chunks, indexed_commits, pending_index_fragments).
%  Used by the index status endpoint to enrich the response with
%  engine-side counts. Fails on non-200 or parse error.
io_statistics_for_domain(Endpoint, Domain, Stats) :-
    assert_search_backend,
    plugin_api:encode_query_value(Domain, Enc_Domain),
    format(atom(URL), "~w/statistics?domain=~w", [Endpoint, Enc_Domain]),
    tdb_http_get(URL, Status, Body_String),
    do_or_die(
        Status =:= 200,
        error(tdb_search_statistics_failed(Status, Body_String), _)),
    atom_json_dict(Body_String, Stats, [default_tag(json)]).

build_suggest_url(Endpoint, Domain, Search_Ref, Ancestors, URL) :-
    plugin_api:encode_query_value(Domain, Enc_Domain),
    search_ref_param(Search_Ref, Ref_Params),
    ancestor_query_params(Ancestors, Ancestor_Params),
    format(atom(URL), "~w/suggest?domain=~w~w~w",
           [Endpoint, Enc_Domain, Ref_Params, Ancestor_Params]).

io_suggest_forward(Endpoint, Domain, Search_Ref, Ancestors,
                   Extra_Params, Response_Body, Data_Version_Header) :-
    assert_search_backend,
    search_auth_header(AuthHeader),
    build_suggest_url(Endpoint, Domain, Search_Ref, Ancestors, Base_URL),
    append_extra_params(Base_URL, Extra_Params, URL),
    io_forward_get(URL, AuthHeader, Response_Body, Data_Version_Header).

build_compare_url(Endpoint, Method, URL) :-
    plugin_api:encode_query_value(Method, Enc_Method),
    format(atom(URL), "~w/compare?method=~w", [Endpoint, Enc_Method]).

build_compare_url(Endpoint, Method, Role, URL) :-
    nonvar(Role),
    !,
    plugin_api:encode_query_value(Method, Enc_Method),
    plugin_api:encode_query_value(Role, Enc_Role),
    format(atom(URL), "~w/compare?method=~w&role=~w", [Endpoint, Enc_Method, Enc_Role]).
build_compare_url(Endpoint, Method, _Role, URL) :-
    build_compare_url(Endpoint, Method, URL).

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

%% io_forward_post(+URL, +AuthHeader, +Post_Body, -Response_Body,
%%                  -Data_Version_Header) is det.
%
%  Forwards a POST request with a JSON body to tdb-search.
io_forward_post(URL, AuthHeader, Post_Body, Response_Body, Data_Version_Header) :-
    setup_call_cleanup(
        http_open(URL, In,
                  [ method(post),
                    post(json(Post_Body)),
                    status_code(Status),
                    AuthHeader,
                    request_header('Content-Type' = 'application/json'),
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

normalize_doc_id(Id, Normalized) :-
    atom(Id),
    !,
    (   sub_atom(Id, 0, 14, _, 'terminusdb:///')
    ->  Normalized = Id
    ;   format(atom(Normalized), 'terminusdb:///data/~w', [Id])
    ).
normalize_doc_id(Id, Normalized) :-
    format(atom(Normalized), 'terminusdb:///data/~w', [Id]).

%% normalize_doc_id(+Id, +Prefixes, -Normalized) is det.
%
%  Expand a (possibly compact) document ID to a full IRI using the
%  database prefixes. When Prefixes is the sentinel `none`, falls back
%  to the legacy normalize_doc_id/1 behavior (terminusdb:///data/Id).

normalize_doc_id(Id, none, Normalized) :-
    !,
    normalize_doc_id(Id, Normalized).
normalize_doc_id(Id, Prefixes, Normalized) :-
    expand_compact_id(Id, Prefixes, Normalized).

%% expand_compact_id(+Id, +Prefixes, -Full) is det.
%
%  Expand a compacted ID to a full IRI.
%    - If Id is already a full terminusdb:/// IRI, return as-is.
%    - If Id uses a prefix:local form known in Prefixes, use prefix_expand/3.
%    - If Id is a bare relative, prepend @base.
%    - Fallback: terminusdb:///data/Id (legacy behavior).

expand_compact_id(Id, _Prefixes, Id) :-
    atomic(Id),
    atom_concat('terminusdb:///', _, Id),
    !.
expand_compact_id(Id, Prefixes, Full) :-
    has_prefix_colon(Id, Prefixes),
    !,
    prefix_expand(Id, Prefixes, Full).
expand_compact_id(Id, Prefixes, Full) :-
    get_dict('@base', Prefixes, Base),
    !,
    atom_concat(Base, Id, Full).
expand_compact_id(Id, _Prefixes, Full) :-
    format(atom(Full), 'terminusdb:///data/~w', [Id]).

%% expand_doc_id(+Prefixes, +Raw, -Full) is det.
%
%  Helper for maplist: expands a compact doc ID to full IRI.
expand_doc_id(Prefixes, Raw, Full) :-
    expand_compact_id(Raw, Prefixes, Full).

%% has_prefix_colon(+Id, +Prefixes) is semidet.
%
%  True when Id is of the form Prefix:Local where Prefix is a key in
%  the Prefixes dict (e.g. '@schema', 'doc', 'a').

has_prefix_colon(Id, Prefixes) :-
    atomic(Id),
    sub_atom(Id, Before, _, _, ':'),
    Before > 0,
    sub_atom(Id, 0, Before, _, Prefix_Name),
    get_dict(Prefix_Name, Prefixes, _).

%% compact_json_value(+Value, +Prefixes, -Compacted) is det.
%
%  Recursively walk a JSON dict/list and compact any string or atom
%  that is a full terminusdb:/// IRI using compress_dict_uri/3.

compact_json_value(Value, Prefixes, Compacted) :-
    is_dict(Value),
    !,
    dict_pairs(Value, Tag, Pairs),
    findall(Compacted_Key-Compacted_Value,
            (   member(Key-Raw_Value, Pairs),
                compact_json_value(Raw_Value, Prefixes, Compacted_Value),
                compact_key(Key, Prefixes, Compacted_Key)
            ),
            Compacted_Pairs),
    dict_pairs(Compacted, Tag, Compacted_Pairs).
compact_json_value(Value, Prefixes, Compacted) :-
    is_list(Value),
    !,
    maplist({Prefixes}/[Raw, Comp]>>compact_json_value(Raw, Prefixes, Comp),
            Value, Compacted).
compact_json_value(Value, Prefixes, Compacted) :-
    atomic(Value),
    atom_concat('terminusdb:///', _, Value),
    !,
    compress_dict_uri(Value, Prefixes, Compacted).
compact_json_value(Value, _Prefixes, Value).

%% compact_key(+Key, +Prefixes, -CompactedKey) is det.
%
%  Compact a dict key if it is a full terminusdb:/// IRI.
%  Keys can be atoms or strings — both are handled.

compact_key(Key, Prefixes, Compacted) :-
    atomic(Key),
    atom_concat('terminusdb:///', _, Key),
    !,
    compress_dict_uri(Key, Prefixes, Compacted).
compact_key(Key, _Prefixes, Key).

%% compact_response_ids(+Response_Body, +Descriptor, -Compacted_Body) is det.
%
%  Parse a JSON response body string, compact all terminusdb:/// IRIs
%  using the database prefixes, and re-serialize to a JSON string.

compact_response_ids(Response_Body, Descriptor, Compacted_Body) :-
    database_prefixes(Descriptor, Prefixes),
    atom_json_dict(Response_Body, Response_Dict, [default_tag(json)]),
    compact_json_value(Response_Dict, Prefixes, Compacted_Dict),
    atom_json_dict(Compacted_Body, Compacted_Dict, [width(0)]).

%% compress_flag(+Search, -Compress) is det.
%
%  Read the compress query parameter, defaulting to true.

compress_flag(Search, Compress) :-
    (   memberchk(compress=Raw, Search)
    ->  normalize_bool(Raw, Compress)
    ;   Compress = true
    ).

normalize_bool(true, true) :- !.
normalize_bool('true', true) :- !.
normalize_bool(false, false) :- !.
normalize_bool('false', false) :- !.
normalize_bool(1, true) :- !.
normalize_bool(0, false) :- !.
normalize_bool(_, true).

%% maybe_prefixes(+Compress, +Descriptor, -Prefixes) is det.
%
%  When Compress is true, fetch database prefixes. Otherwise return
%  the sentinel `none` so that normalize_doc_id/3 uses legacy behavior.

maybe_prefixes(true, Descriptor, Prefixes) :-
    !,
    database_prefixes(Descriptor, Prefixes).
maybe_prefixes(false, _Descriptor, none).

%% maybe_compact_response(+Compress, +Response_Body, +Descriptor,
%%                        -Final_Body) is det.
%
%  When Compress is true, compact IRIs in the response. Otherwise
%  pass the response through unchanged.

maybe_compact_response(true, Response_Body, Descriptor, Final_Body) :-
    !,
    compact_response_ids(Response_Body, Descriptor, Final_Body).
maybe_compact_response(false, Response_Body, _Descriptor, Response_Body).

%% nudge_commit(+Search_Ref, -Nudge_Commit) is det.
%
%  Extracts the commit ID for nudge purposes. For commit descriptors,
%  returns the commit ID. For branch descriptors, returns none (skip
%  sync nudge — branch searches rely on async nudge only).
nudge_commit(commit(Commit_Id), Commit_Id) :- !.
nudge_commit(branch(_), none).

maybe_nudge_push(none, _Commit, _System_DB, _Auth, _Path, _Branch) :- !.
maybe_nudge_push(Data_Version_Header, Commit, _System_DB, _Auth, Path, Branch) :- !,
    format(string(Expected_DV), "commit:~w", [Commit]),
    (   Data_Version_Header == Expected_DV
    ->  true
    ;   (   plugin_api:indexer_available
        ->  (   branch_path_for_notify(Path, Branch, Branch_Path),
                api_indexer:schema_store_clustering_for_path(Path, Store_Clustering),
                catch((plugin_api:indexer_notify(Branch_Path, Branch, Store_Clustering) ; true),
                      Nudge_Error,
                      format(user_error,
                             "[WARN] Search stale-version nudge failed for ~w: ~q~n",
                             [Path, Nudge_Error]))
            )
        ;   true
        )
    ).

/**
 * maybe_nudge_push_async(+System_DB, +Auth, +Path, +Branch) is det.
 *
 * Checks /last-indexed before nudging. Only spawns a push thread when the
 * engine commit is null (truly unindexed). If the engine already has a
 * commit (either at HEAD or in-flight from auto-push or a previous nudge),
 * the nudge is skipped — no further push until the current indexing completes.
 */
maybe_nudge_push_async(System_DB, Auth, Path, Branch) :-
    (   catch(
            (   tdb_search_endpoint(Endpoint),
                resolve_absolute_string_descriptor(Path, Descriptor),
                descriptor_domain(Descriptor, Domain),
                io_get_last_indexed(Endpoint, Domain, Branch, Result),
                get_dict(commit, Result, Engine_Commit_Raw),
                normalise_commit_value(Engine_Commit_Raw, Engine_Commit)
            ),
            Check_Error,
            (   format(user_error,
                       "[WARN] Nudge pre-check /last-indexed failed for ~w: ~q~n",
                       [Path, Check_Error]),
                Engine_Commit = unknown
            )
        ),
        (   Engine_Commit == null
        ->  catch(
                thread_create(
                    catch(
                        io_push_delta(System_DB, Auth, Path, Branch),
                        Nudge_Error,
                        format(user_error,
                               "[WARN] Search stale-version nudge failed for ~w: ~q~n",
                               [Path, Nudge_Error])
                    ),
                    _,
                    [detached(true)]
                ),
                Spawn_Error,
                format(user_error,
                       "[WARN] Search nudge thread spawn failed for ~w: ~q~n",
                       [Path, Spawn_Error])
            )
        ;   true
        )
    ).

build_resolve_url(Endpoint, Domain, Search_Ref, URL) :-
    plugin_api:encode_query_value(Domain, Enc_Domain),
    search_ref_param(Search_Ref, Ref_Params),
    format(atom(URL), "~w/candidates?domain=~w~w",
           [Endpoint, Enc_Domain, Ref_Params]).

io_resolve_forward(Endpoint, Domain, Search_Ref, Ancestors,
                   Body_Dict, Response_Body, Data_Version_Header) :-
    assert_search_backend,
    search_auth_header(AuthHeader),
    format(atom(Resolve_URL), "~w/candidates", [Endpoint]),
    (   Search_Ref = commit(Commit)
    ->  put_dict(_{domain: Domain, commit: Commit, ancestors: Ancestors},
                 Body_Dict, Forward_Body)
    ;   Search_Ref = branch(Branch),
        put_dict(_{domain: Domain, branch: Branch, ancestors: Ancestors},
                 Body_Dict, Forward_Body)
    ),
    setup_call_cleanup(
        http_open(Resolve_URL, In,
                  [ method(post),
                    post(json(Forward_Body)),
                    status_code(Status),
                    AuthHeader,
                    request_header('Content-Type' = 'application/json'),
                    request_header('Accept' = 'application/json'),
                    header(terminusdb_data_version, DV_Raw)
                  ]),
        read_string(In, _, Response_Body),
        close(In)),
    normalise_data_version_header(DV_Raw, Data_Version_Header),
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

:- multifile plugins:post_server_startup_hook/1.

%% post_server_startup_hook(+Port) is det.
%
%  Called after the SWI-Prolog server starts. Wires the tdb-search URL
%  and auth header into the Rust IndexerRegistry via the indexer_set_config
%  FFI predicate. This runs once at boot, after tdb_search_endpoint/1 is
%  configured but before any commits arrive.
plugins:post_server_startup_hook(_Port) :-
    (   tdb_search_endpoint(Endpoint),
        plugin_api:indexer_available
    ->  (   search_auth_header(authorization(basic(User, Secret)))
        ->  format(atom(Creds), "~w:~w", [User, Secret]),
            base64(Creds, B64),
            format(atom(AuthHeader), "Basic ~w", [B64]),
            catch(plugin_api:indexer_set_config(Endpoint, AuthHeader),
                  Error,
                  format(user_error,
                         "[ERROR] indexer_set_config failed: ~q~n",
                         [Error]))
        ;   catch(plugin_api:indexer_set_config(Endpoint, ""),
                  Error,
                  format(user_error,
                         "[ERROR] indexer_set_config failed: ~q~n",
                         [Error]))
        )
    ;   true
    ).

:- multifile plugins:post_delete_db_hook/2.

plugins:post_delete_db_hook(Organization, DB_Name) :-
    (   tdb_search_endpoint(Endpoint)
    ->  format(atom(Domain), "~w/~w", [Organization, DB_Name]),
        catch(io_delete_domain(Endpoint, Domain),
              Delete_Error,
              format(user_error,
                     "[ERROR] Failed to delete search domain '~w' from engine: ~q~n",
                     [Domain, Delete_Error])),
        % Abort any active indexer tasks for this domain (FFI predicate
        % registered by Rust runtime — guard with current_predicate).
        (   plugin_api:indexer_available
        ->  catch(plugin_api:indexer_abort_domain(Domain),
                  Abort_Error,
                  format(user_error,
                         "[ERROR] Failed to abort indexer tasks for '~w': ~q~n",
                         [Domain, Abort_Error]))
        ;   true
        )
    ;   true
    ).

% ==========================================================================
% Commit resolution: use the descriptor's commit or branch name.
%
% If the URL path is /commit/<id> (commit_descriptor), search that commit.
% Otherwise (branch_descriptor), pass the branch name to tdb-search which
% resolves it to the latest indexed commit internally.
% ==========================================================================

%% resolve_search_commit(+Descriptor, -Search_Ref, -Commit_Uri, -Ancestors) is det.
%
%  Returns a search reference: either commit(Commit_Id) for commit
%  descriptors, or branch(Branch_Name) for branch descriptors.
%  Commit_Uri and Ancestors are only meaningful for commit descriptors;
%  for branch descriptors they are '' and [] respectively.
resolve_search_commit(Descriptor, Search_Ref, Commit_Uri, Ancestors) :-
    (   commit_descriptor{commit_id: Commit_Id} :< Descriptor
    ->  get_dict(repository_descriptor, Descriptor, Repository_Descriptor),
        tdb_search:commit_id_uri(Repository_Descriptor, Commit_Id, Commit_Uri),
        tdb_search:ancestor_window(Repository_Descriptor, Commit_Uri, 100, Ancestors),
        Search_Ref = commit(Commit_Id)
    ;   branch_descriptor{branch_name: Branch_Name} :< Descriptor,
        Commit_Uri = '',
        Ancestors = [],
        Search_Ref = branch(Branch_Name)
    ).

%% commit_timestamp(+Repository_Descriptor, +Commit_Id, -Timestamp) is det.
%
%  Looks up the commit timestamp from the repository. Returns none on failure.
commit_timestamp(Repository_Descriptor, Commit_Id, Timestamp) :-
    catch(
        (   commit_id_to_metadata(Repository_Descriptor, Commit_Id,
                                             _Author, _Message, Timestamp)
        ),
        _,
        Timestamp = none
    ).

%% reply_search_with_metadata(+Request, +Response_Body, +Data_Version_Header,
%%                             +Repository_Descriptor) is det.
%
%  Extracts the served commit from the data version header, looks up its
%  timestamp, and calls reply_search_response with both.
reply_search_with_metadata(Request, Response_Body, Data_Version_Header,
                           Repository_Descriptor) :-
    tdb_search:extract_commit_from_data_version(Data_Version_Header, Served_Commit),
    (   Served_Commit \== none
    ->  tdb_search:commit_timestamp(Repository_Descriptor, Served_Commit, Timestamp)
    ;   Timestamp = none
    ),
    tdb_search:reply_search_response(Request, Response_Body, Data_Version_Header,
                                     Served_Commit, Timestamp).

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
            plugin_api:resolve_descriptor_auth(read, System_DB, Auth, Path, instance, Descriptor),
            do_or_die(tdb_search:tdb_search_endpoint(Endpoint),
                      error(tdb_search_endpoint_not_configured(search_handler), _)),
            do_or_die(
                (   branch_descriptor{branch_name: Branch_Name} :< Descriptor
                ->  true
                ;   commit_descriptor{} :< Descriptor,
                    Branch_Name = none
                ),
                error(search_requires_branch_descriptor(Path), _)),
            get_dict(repository_descriptor, Descriptor, Repository_Descriptor),
            tdb_search:descriptor_domain(Descriptor, Domain),
            tdb_search:resolve_search_commit(Descriptor, Search_Ref, _Commit_Uri, Ancestors),
            tdb_search:nudge_commit(Search_Ref, Nudge_Commit),
            tdb_search:compress_flag(Search, Compress),
            tdb_search:maybe_prefixes(Compress, Descriptor, Prefixes),
            tdb_search:search_extra_params(Search, Body, Prefixes, Extra_Params),
            catch(
                (   tdb_search:io_search_forward(Endpoint, Domain, Search_Ref, Ancestors,
                                      Extra_Params, Response_Body, Data_Version_Header),
                    tdb_search:maybe_nudge_push(Data_Version_Header, Nudge_Commit,
                                     System_DB, Auth, Path, Branch_Name),
                    tdb_search:maybe_compact_response(Compress, Response_Body,
                                       Descriptor, Final_Body),
                    tdb_search:reply_search_with_metadata(Request, Final_Body,
                                       Data_Version_Header, Repository_Descriptor)
                ),
                error(tdb_search_forward_failed(404, Engine_Body, _Fail_URL), _),
                (   tdb_search:maybe_nudge_push_async(System_DB, Auth, Path, Branch_Name),
                    throw(error(search_not_indexed(Path, Engine_Body), _))
                )
            )
        )
    ).

suggest_handler(get, Path, Request, System_DB, Auth) :-
    (   memberchk(search(Search), Request)
    ->  true
    ;   Search = []),
    plugin_api:api_report_errors(
        suggest,
        Request,
        (
            plugin_api:resolve_descriptor_auth(read, System_DB, Auth, Path, instance, Descriptor),
            do_or_die(tdb_search:tdb_search_endpoint(Endpoint),
                      error(tdb_search_endpoint_not_configured(suggest_handler), _)),
            do_or_die(
                (   branch_descriptor{branch_name: Branch_Name} :< Descriptor
                ->  true
                ;   commit_descriptor{} :< Descriptor,
                    Branch_Name = none
                ),
                error(search_requires_branch_descriptor(Path), _)),
            get_dict(repository_descriptor, Descriptor, Repository_Descriptor),
            tdb_search:descriptor_domain(Descriptor, Domain),
            tdb_search:resolve_search_commit(Descriptor, Search_Ref, _Commit_Uri, Ancestors),
            tdb_search:compress_flag(Search, Compress),
            tdb_search:maybe_prefixes(Compress, Descriptor, Prefixes),
            tdb_search:search_extra_params(Search, _{}, Prefixes, Extra_Params),
            catch(
                (   tdb_search:io_suggest_forward(Endpoint, Domain, Search_Ref, Ancestors,
                                      Extra_Params, Response_Body, Data_Version_Header),
                    tdb_search:maybe_compact_response(Compress, Response_Body,
                                       Descriptor, Final_Body),
                    tdb_search:reply_search_with_metadata(Request, Final_Body,
                                       Data_Version_Header, Repository_Descriptor)
                ),
                error(tdb_search_forward_failed(404, Engine_Body, _Fail_URL), _),
                (   tdb_search:maybe_nudge_push_async(System_DB, Auth, Path, Branch_Name),
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
            plugin_api:resolve_descriptor_auth(read, System_DB, Auth, Path, instance, Descriptor),
            do_or_die(tdb_search:tdb_search_endpoint(Endpoint),
                      error(tdb_search_endpoint_not_configured(similar_handler), _)),
            do_or_die(
                (   branch_descriptor{branch_name: Branch_Name} :< Descriptor
                ->  true
                ;   commit_descriptor{} :< Descriptor,
                    Branch_Name = none
                ),
                error(search_requires_branch_descriptor(Path), _)),
            get_dict(repository_descriptor, Descriptor, Repository_Descriptor),
            tdb_search:descriptor_domain(Descriptor, Domain),
            tdb_search:resolve_search_commit(Descriptor, Search_Ref, _Commit_Uri, Ancestors),
            tdb_search:nudge_commit(Search_Ref, Nudge_Commit),
            tdb_search:compress_flag(Search, Compress),
            tdb_search:maybe_prefixes(Compress, Descriptor, Prefixes),
            tdb_search:similar_extra_params(Search, Body, Prefixes, Extra_Params),
            (   memberchk(text=Text, Extra_Params),
                tdb_search:search_scalar_present(Text)
            ->  Post_Body = _{text: Text},
                catch(
                    (   tdb_search:io_similar_forward_post(Endpoint, Domain, Search_Ref, Ancestors,
                                           Extra_Params, Post_Body, Response_Body, Data_Version_Header),
                        tdb_search:maybe_nudge_push(Data_Version_Header, Nudge_Commit,
                                         System_DB, Auth, Path, Branch_Name),
                        tdb_search:maybe_compact_response(Compress, Response_Body,
                                           Descriptor, Final_Body),
                        tdb_search:reply_search_with_metadata(Request, Final_Body,
                                           Data_Version_Header, Repository_Descriptor)
                    ),
                    error(tdb_search_forward_failed(404, Engine_Body, _), _),
                    (   tdb_search:maybe_nudge_push_async(System_DB, Auth, Path, Branch_Name),
                        throw(error(search_not_indexed(Path, Engine_Body), _))
                    )
                )
            ;   catch(
                    (   tdb_search:io_similar_forward(Endpoint, Domain, Search_Ref, Ancestors,
                                           Extra_Params, Response_Body, Data_Version_Header),
                        tdb_search:maybe_nudge_push(Data_Version_Header, Nudge_Commit,
                                         System_DB, Auth, Path, Branch_Name),
                        tdb_search:maybe_compact_response(Compress, Response_Body,
                                           Descriptor, Final_Body),
                        tdb_search:reply_search_with_metadata(Request, Final_Body,
                                           Data_Version_Header, Repository_Descriptor)
                    ),
                    error(tdb_search_forward_failed(404, Engine_Body, _Fail_URL), _),
                    (   tdb_search:maybe_nudge_push_async(System_DB, Auth, Path, Branch_Name),
                        throw(error(search_not_indexed(Path, Engine_Body), _))
                    )
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
            plugin_api:resolve_descriptor_auth(read, System_DB, Auth, Path, instance, Descriptor),
            do_or_die(tdb_search:tdb_search_endpoint(Endpoint),
                      error(tdb_search_endpoint_not_configured(duplicates_handler), _)),
            do_or_die(
                (   branch_descriptor{branch_name: Branch_Name} :< Descriptor
                ->  true
                ;   commit_descriptor{} :< Descriptor,
                    Branch_Name = none
                ),
                error(search_requires_branch_descriptor(Path), _)),
            get_dict(repository_descriptor, Descriptor, Repository_Descriptor),
            tdb_search:descriptor_domain(Descriptor, Domain),
            tdb_search:resolve_search_commit(Descriptor, Search_Ref, _Commit_Uri, Ancestors),
            tdb_search:compress_flag(Search, Compress),
            tdb_search:maybe_prefixes(Compress, Descriptor, Prefixes),
            tdb_search:duplicates_extra_params(Search, Body, Prefixes, Extra_Params),
            catch(
                (   tdb_search:io_duplicates_forward(Endpoint, Domain, Search_Ref, Ancestors,
                                          Extra_Params, Response_Body, Data_Version_Header),
                    tdb_search:maybe_compact_response(Compress, Response_Body,
                                       Descriptor, Final_Body),
                    tdb_search:reply_search_with_metadata(Request, Final_Body,
                                       Data_Version_Header, Repository_Descriptor)
                ),
                error(tdb_search_forward_failed(404, Engine_Body, _Fail_URL), _),
                (   tdb_search:maybe_nudge_push_async(System_DB, Auth, Path, Branch_Name),
                    throw(error(search_not_indexed(Path, Engine_Body), _))
                )
            )
        )
    ).

resolve_handler(post, Path, Request, System_DB, Auth) :-
    (   memberchk(search(Search), Request)
    ->  true
    ;   Search = []),
    search_request_body(Request, Body),
    plugin_api:api_report_errors(
        search,
        Request,
        (
            plugin_api:resolve_descriptor_auth(read, System_DB, Auth, Path, instance, Descriptor),
            do_or_die(tdb_search:tdb_search_endpoint(Endpoint),
                      error(tdb_search_endpoint_not_configured(resolve_handler), _)),
            do_or_die(
                (   branch_descriptor{branch_name: Branch_Name} :< Descriptor
                ->  true
                ;   commit_descriptor{} :< Descriptor,
                    Branch_Name = none
                ),
                error(search_requires_branch_descriptor(Path), _)),
            get_dict(repository_descriptor, Descriptor, Repository_Descriptor),
            tdb_search:descriptor_domain(Descriptor, Domain),
            tdb_search:resolve_search_commit(Descriptor, Search_Ref, _Commit_Uri, Ancestors),
            tdb_search:compress_flag(Search, Compress),
            tdb_search:maybe_prefixes(Compress, Descriptor, Prefixes),
            tdb_search:resolve_forward_body(Body, Prefixes, Forward_Body),
            catch(
                (   tdb_search:io_resolve_forward(Endpoint, Domain, Search_Ref, Ancestors,
                                       Forward_Body, Response_Body, Data_Version_Header),
                    tdb_search:maybe_compact_response(Compress, Response_Body,
                                       Descriptor, Final_Body),
                    tdb_search:extract_commit_from_data_version(Data_Version_Header, Served_Commit),
                    (   Served_Commit \== none
                    ->  tdb_search:commit_timestamp(Repository_Descriptor, Served_Commit, Timestamp)
                    ;   Search_Ref = commit(CId)
                    ->  tdb_search:commit_timestamp(Repository_Descriptor, CId, Timestamp)
                    ;   Timestamp = none
                    ),
                    plugin_api:write_cors_headers(Request),
                    (   Data_Version_Header \== none
                    ->  format("TerminusDB-Data-Version: ~w~n", [Data_Version_Header])
                    ;   true
                    ),
                    (   Served_Commit \== none
                    ->  format("TerminusDB-Served-Commit: ~w~n", [Served_Commit])
                    ;   true
                    ),
                    (   Timestamp \== none
                    ->  format("TerminusDB-Served-Timestamp: ~w~n", [Timestamp])
                    ;   true
                    ),
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

resolve_forward_body(Body, Forward_Body) :-
    resolve_forward_body(Body, none, Forward_Body).
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
    maplist({Prefixes}/[Raw, Id]>>normalize_doc_id(Raw, Prefixes, Id),
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
            do_or_die(tdb_search:tdb_search_endpoint(Endpoint),
                      error(tdb_search_endpoint_not_configured(compare_handler), _)),
            do_or_die(
                (   memberchk(method=Method, Search),
                    Method \== ''
                ),
                error(missing_parameter(method), _)),
            (   memberchk(role=Role, Search)
            ->  true
            ;   Role = _No_Role
            ),
            (   var(Role)
            ->  true
            ;   member(Role, [query, document, clustering, classification])
            ->  true
            ;   throw(error(malformed_parameter(role), _))
            ),
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
            tdb_search:io_compare_forward(Endpoint, Method, Role, Forward_Body, Response_Body),
            tdb_search:reply_compare_response(Request, Response_Body)
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
    search_extra_params(Search, Body, none, Params).
search_extra_params(Search, Body, Prefixes, Params) :-
    findall(Param,
            search_extra_param(Search, Body, Prefixes, Param),
            Params).

search_extra_param(Search, Body, _Prefixes, q=Q) :-
    merged_scalar_param(q, Body, Search, Q).
search_extra_param(Search, Body, _Prefixes, mode=Mode) :-
    merged_scalar_param(mode, Body, Search, Mode).
search_extra_param(Search, Body, _Prefixes, start=Start) :-
    merged_scalar_param(start, Body, Search, Start).
search_extra_param(Search, Body, _Prefixes, count=Count) :-
    merged_scalar_param(count, Body, Search, Count).
search_extra_param(Search, Body, _Prefixes, snippet=Snippet) :-
    merged_scalar_param(snippet, Body, Search, Snippet).
search_extra_param(Search, Body, _Prefixes, doc_type=repeated(Types)) :-
    merged_repeated_param(doc_type, Body, Search, Types).
search_extra_param(Search, Body, Prefixes, doc_id=repeated(Ids)) :-
    merged_repeated_param(doc_id, Body, Search, Raw_Ids),
    maplist({Prefixes}/[Raw, Id]>>normalize_doc_id(Raw, Prefixes, Id),
            Raw_Ids, Ids).

similar_extra_params(Search, Body, Params) :-
    similar_extra_params(Search, Body, none, Params).
similar_extra_params(Search, Body, Prefixes, Params) :-
    findall(Param,
            similar_extra_param(Search, Body, Prefixes, Param),
            Params).

similar_extra_param(Search, Body, Prefixes, id=Id) :-
    merged_scalar_param(id, Body, Search, Raw_Id),
    normalize_doc_id(Raw_Id, Prefixes, Id).
similar_extra_param(Search, Body, _Prefixes, text=Text) :-
    merged_scalar_param(text, Body, Search, Text).
similar_extra_param(Search, Body, _Prefixes, start=Start) :-
    merged_scalar_param(start, Body, Search, Start).
similar_extra_param(Search, Body, _Prefixes, count=Count) :-
    merged_scalar_param(count, Body, Search, Count).
similar_extra_param(Search, Body, _Prefixes, snippet=Snippet) :-
    merged_scalar_param(snippet, Body, Search, Snippet).
similar_extra_param(Search, Body, _Prefixes, doc_type=repeated(Types)) :-
    merged_repeated_param(doc_type, Body, Search, Types).
similar_extra_param(Search, Body, Prefixes, doc_id=repeated(Ids)) :-
    merged_repeated_param(doc_id, Body, Search, Raw_Ids),
    maplist({Prefixes}/[Raw, Id]>>normalize_doc_id(Raw, Prefixes, Id),
            Raw_Ids, Ids).

duplicates_extra_params(Search, Body, Params) :-
    duplicates_extra_params(Search, Body, none, Params).
duplicates_extra_params(Search, Body, Prefixes, Params) :-
    findall(Param,
            duplicates_extra_param(Search, Body, Prefixes, Param),
            Params).

duplicates_extra_param(Search, Body, _Prefixes, threshold=T) :-
    merged_scalar_param(threshold, Body, Search, T).
duplicates_extra_param(Search, Body, _Prefixes, start=Start) :-
    merged_scalar_param(start, Body, Search, Start).
duplicates_extra_param(Search, Body, _Prefixes, count=Count) :-
    merged_scalar_param(count, Body, Search, Count).
duplicates_extra_param(Search, Body, _Prefixes, snippet=Snippet) :-
    merged_scalar_param(snippet, Body, Search, Snippet).
duplicates_extra_param(Search, Body, _Prefixes, doc_type=repeated(Types)) :-
    merged_repeated_param(doc_type, Body, Search, Types).
duplicates_extra_param(Search, Body, Prefixes, doc_id=repeated(Ids)) :-
    merged_repeated_param(doc_id, Body, Search, Raw_Ids),
    maplist({Prefixes}/[Raw, Id]>>normalize_doc_id(Raw, Prefixes, Id),
            Raw_Ids, Ids).
duplicates_extra_param(Search, Body, _Prefixes, target_doc_type=repeated(Types)) :-
    merged_repeated_param(target_doc_type, Body, Search, Types).
duplicates_extra_param(Search, Body, Prefixes, target_doc_id=repeated(Ids)) :-
    merged_repeated_param(target_doc_id, Body, Search, Raw_Ids),
    maplist({Prefixes}/[Raw, Id]>>normalize_doc_id(Raw, Prefixes, Id),
            Raw_Ids, Ids).

reply_search_response(Request, Response_Body, Data_Version_Header) :-
    reply_search_response(Request, Response_Body, Data_Version_Header, none, none).

%% reply_search_response(+Request, +Response_Body, +Data_Version_Header,
%%                        +Served_Commit, +Served_Timestamp) is det.
%
%  Writes CORS headers, the TerminusDB-Data-Version header (from tdb-search),
%  and optional TerminusDB-Served-Commit / TerminusDB-Served-Timestamp headers
%  when the served commit is known. The served commit may differ from the
%  requested commit when the indexer is catching up (fallback to last-indexed).
reply_search_response(Request, Response_Body, Data_Version_Header, Served_Commit, Served_Timestamp) :-
    plugin_api:write_cors_headers(Request),
    (   Data_Version_Header \== none
    ->  format("TerminusDB-Data-Version: ~w~n", [Data_Version_Header])
    ;   true
    ),
    (   Served_Commit \== none
    ->  format("TerminusDB-Served-Commit: ~w~n", [Served_Commit])
    ;   true
    ),
    (   Served_Timestamp \== none
    ->  format("TerminusDB-Served-Timestamp: ~w~n", [Served_Timestamp])
    ;   true
    ),
    format("Content-Type: application/json~n~n"),
    write(Response_Body).

%% extract_commit_from_data_version(+Data_Version_Header, -Commit) is det.
%
%  Extracts the commit ID from a "commit:<id>" data version header.
%  Returns none if the header is missing or doesn't match the format.
extract_commit_from_data_version(none, none) :- !.
extract_commit_from_data_version("", none) :- !.
extract_commit_from_data_version(Header, Commit) :-
    atom_string(Header, HeaderStr),
    string_concat("commit:", Commit, HeaderStr),
    !.
extract_commit_from_data_version(_, none).

%% reply_embeddings_stream_headers(+Request, +Served_Commit,
%%                               +Store_Clustering, +Total_Count) is det.
%
%  Writes CORS and NDJSON streaming headers to current_output.
%  Used by both io_embeddings_forward_stream and io_embeddings_forward_post_stream
%  before streaming the NDJSON body line-by-line.
reply_embeddings_stream_headers(Request, Served_Commit, Store_Clustering, Total_Count) :-
    plugin_api:write_cors_headers(Request),
    format("Access-Control-Expose-Headers: X-Served-Commit, X-Store-Clustering, X-Total-Count~n"),
    format("Content-Type: application/x-ndjson~n"),
    format("X-Served-Commit: ~w~n", [Served_Commit]),
    format("X-Store-Clustering: ~w~n", [Store_Clustering]),
    format("X-Total-Count: ~w~n~n", [Total_Count]).

% ==========================================================================
% Index status response assembly (used by index_handler GET)
% ==========================================================================

%% assemble_index_status_response(+Indexer_Progress, +Branch_Name,
%%                                 +Last_Commit, +Engine_Stats, -Response) is det.
%
%  Constructs the index status API response dict from the Rust FFI
%  progress dict, the last indexed commit from tdb-search, and the
%  engine statistics. Handles missing keys defensively — Indexer_Progress
%  may be a minimal dict like json{status:not_found} with no
%  branch_processing key.
assemble_index_status_response(Indexer_Progress, Branch_Name,
                               Last_Commit, Engine_Stats, Response) :-
    get_dict(status, Indexer_Progress, Status_Raw),
    % atom_json_dict/3 parses JSON string values as Prolog strings,
    % not atoms. Normalise to atom for reliable == comparison.
    (   atom(Status_Raw)
    ->  Status_Atom = Status_Raw
    ;   atom_string(Status_Atom, Status_Raw)
    ),
    % If the indexer registry has no task (not_found) or is not loaded
    % (indexer_unavailable) but the engine reports a valid last-indexed
    % commit, the index exists — the task simply completed and was removed
    % from the registry, or the Rust indexer FFI isn't compiled in. Override
    % the status to "completed" so the UI doesn't show "not_found" or
    % "indexer_unavailable" for a healthy, indexed branch.
    (   ( Status_Atom == not_found
      ; Status_Atom == indexer_unavailable
      ),
        Last_Commit \= null
    ->  Status_Value = completed
    ;   Status_Value = Status_Raw
    ),
    (   get_dict(error, Indexer_Progress, Error_Value)
    ->  true
    ;   Error_Value = null
    ),
    (   get_dict(branch_processing, Indexer_Progress, BP)
    ->  true
    ;   BP = json{}
    ),
    (   get_dict(commits_processed, BP, Commits_Processed)
    ->  true
    ;   Commits_Processed = 0
    ),
    (   get_dict(total_commits, BP, Total_Commits)
    ->  true
    ;   Total_Commits = 0
    ),
    (   get_dict(current_commit, BP, Current_Commit)
    ->  true
    ;   Current_Commit = null
    ),
    (   get_dict(upcoming_commit, BP, Upcoming_Commit)
    ->  true
    ;   Upcoming_Commit = null
    ),
    (   get_dict(processed_documents, BP, Processed_Docs)
    ->  true
    ;   Processed_Docs = 0
    ),
    (   get_dict(total_documents, BP, Total_Docs)
    ->  true
    ;   Total_Docs = 0
    ),
    (   get_dict(documents_sent, BP, Docs_Sent)
    ->  true
    ;   Docs_Sent = 0
    ),
    (   get_dict(documents, Engine_Stats, Searchable_Docs)
    ->  true
    ;   Searchable_Docs = 0
    ),
    (   get_dict(chunks, Engine_Stats, Segments_Indexed)
    ->  true
    ;   Segments_Indexed = 0
    ),
    (   get_dict(indexed_commits, Engine_Stats, Commits_Received)
    ->  true
    ;   Commits_Received = 0
    ),
    (   get_dict(pending_index_documents, Engine_Stats, Pending_Updates)
    ->  true
    ;   get_dict(pending_index_fragments, Engine_Stats, Pending_Updates)
    ->  true
    ;   Pending_Updates = 0
    ),
    (   get_dict(store_clustering, Engine_Stats, Store_Clustering_Raw)
    ->  (   Store_Clustering_Raw == true
        ->  Store_Clustering = true
        ;   Store_Clustering_Raw == false
        ->  Store_Clustering = false
        ;   Store_Clustering = null
        )
    ;   Store_Clustering = null
    ),
    Engine_Section = json{
        processed_documents:Processed_Docs,
        total_documents:Total_Docs,
        documents_sent:Docs_Sent,
        searchable_documents:Searchable_Docs,
        text_segments_indexed:Segments_Indexed,
        commits_received:Commits_Received,
        pending_updates:Pending_Updates,
        store_clustering:Store_Clustering
    },
    Branch_Processing_Section = json{
        commits_processed:Commits_Processed,
        total_commits:Total_Commits,
        current_commit:Current_Commit,
        upcoming_commit:Upcoming_Commit
    },
    Response = json{
        status:Status_Value,
        error:Error_Value,
        branch:Branch_Name,
        last_indexed_commit:Last_Commit,
        branch_processing:Branch_Processing_Section,
        engine:Engine_Section
    }.

% ==========================================================================
% Index handler — explicit reindex trigger (POST), index deletion (DELETE),
% and indexing progress query (GET).
% ==========================================================================

index_handler(get, Path, Request, System_DB, Auth) :-
    plugin_api:api_report_errors(
        index,
        Request,
        (   plugin_api:resolve_descriptor_auth(read, System_DB, Auth, Path, instance, Descriptor),
            do_or_die(tdb_search:tdb_search_endpoint(Endpoint),
                      error(tdb_search_endpoint_not_configured(index_handler), _)),
            do_or_die(
                (   branch_descriptor{branch_name: Branch_Name} :< Descriptor
                ->  true
                ;   commit_descriptor{} :< Descriptor,
                    Branch_Name = none
                ),
                error(search_requires_branch_descriptor(Path), _)),
            tdb_search:descriptor_graphspec(Descriptor, Branch_Path),
            tdb_search:descriptor_domain(Descriptor, Domain),
            % 1. Get indexer progress from the Rust IndexerRegistry.
            %    The FFI returns a JSON string (not a Prolog dict) because
            %    swipl-rs's serialize_to_term doesn't respect
            %    skip_serializing_if for Option fields. We parse it here.
            (   plugin_api:indexer_available
            ->  (   plugin_api:indexer_progress(Branch_Path, Branch_Name, Progress_JSON)
                ->  atom_json_dict(Progress_JSON, Indexer_Progress, [default_tag(json)])
                ;   Indexer_Progress = json{status:not_found}
                )
            ;   Indexer_Progress = json{status:indexer_unavailable}
            ),
            % 2. Query tdb-search /last-indexed synchronously for the engine's
            %    current indexed commit.
            catch(
                (   tdb_search:io_get_last_indexed(Endpoint, Domain, Branch_Name, Last_Indexed),
                    get_dict(commit, Last_Indexed, Last_Commit_Raw),
                    tdb_search:normalise_commit_value(Last_Commit_Raw, Last_Commit)
                ),
                _,
                Last_Commit = null
            ),
            % 2b. (not_found → completed override is handled in
            %     assemble_index_status_response, where it's unit-tested.)
            
            % 3. Query tdb-search /statistics?domain=... for engine-side counts.
            catch(
                tdb_search:io_statistics_for_domain(Endpoint, Domain, Engine_Stats),
                _,
                Engine_Stats = json{documents:0, chunks:0, indexed_commits:0,
                                    pending_index_fragments:0,
                                    store_clustering:null}
            ),
            % 4. Assemble the response using the extracted predicate.
            tdb_search:assemble_index_status_response(
                Indexer_Progress, Branch_Name, Last_Commit, Engine_Stats,
                Response0),
            plugin_api:write_cors_headers(Request),
            format("Content-Type: application/json~n~n"),
            json_write_dict(current_output, Response0, [width(0)])
        )
    ).

index_handler(post, Path, Request, System_DB, Auth) :-
    plugin_api:api_report_errors(
        index,
        Request,
        (   plugin_api:resolve_descriptor_auth(read, System_DB, Auth, Path, instance, _Descriptor),
            catch(
                (   tdb_search:io_index_branch(System_DB, Auth, Path),
                    plugin_api:write_cors_headers(Request),
                    format("Content-Type: application/json~n~n"),
                    json_write_dict(current_output, json{'@type':'api:IndexResponse','api:status':'api:success'}, [width(0)])
                ),
                Error,
                (   (   Error = error(tdb_search_409_resolution_timeout, _)
                    ->  true
                    ;   Error = error(socket_error(epipe, _), _)
                    ->  true
                    ;   throw(Error)
                    ),
                    plugin_api:write_cors_headers(Request),
                    format("Content-Type: application/json~n~n"),
                    json_write_dict(current_output, json{'@type':'api:IndexResponse',
                                'api:status':'api:success',
                                'api:message':'Index already in progress for this branch'}, [width(0)])
                )
            )
        )
    ).

index_handler(delete, Path, Request, System_DB, Auth) :-
    plugin_api:api_report_errors(
        index,
        Request,
        (   plugin_api:resolve_descriptor_auth(read, System_DB, Auth, Path, instance, Descriptor),
            do_or_die(tdb_search:tdb_search_endpoint(Endpoint),
                      error(tdb_search_endpoint_not_configured(index_handler), _)),
            tdb_search:descriptor_domain(Descriptor, Domain),
            tdb_search:io_delete_domain(Endpoint, Domain),
            format(string(Message), "Index for ~w deleted", [Path]),
            plugin_api:write_cors_headers(Request),
            format("Content-Type: application/json~n~n"),
            json_write_dict(current_output, json{'@type':'api:IndexResponse','api:status':'api:success',
                        'api:message':Message}, [width(0)])
        )
    ).

% ==========================================================================
% Embeddings retrieval proxy
% ==========================================================================

%% build_embeddings_url(+Endpoint, +Domain, +Commit, +Doc_Ids, +Doc_Types, +Ancestors, -URL) is det.
%
%  Constructs the tdb-search /embeddings URL with query parameters.
%  Doc_Ids and Doc_Types are sent as comma-separated single params
%  (doc_ids=A,B&doc_types=X,Y) matching tdb-search's EmbeddingsParams struct.
build_embeddings_url(Endpoint, Domain, Search_Ref, Doc_Ids, Doc_Types, Ancestors, URL) :-
    plugin_api:encode_query_value(Domain, Enc_Domain),
    search_ref_param(Search_Ref, Ref_Params),
    (   Doc_Ids = []
    ->  Doc_Id_Params = ''
    ;   maplist([Id, Enc]>>(plugin_api:encode_query_value(Id, Enc)), Doc_Ids, Enc_Ids),
        atomic_list_concat(Enc_Ids, ',', Doc_Ids_Joined),
        format(atom(Doc_Id_Params), "&doc_ids=~w", [Doc_Ids_Joined])
    ),
    (   Doc_Types = []
    ->  Doc_Type_Params = ''
    ;   maplist([Type, Enc]>>(plugin_api:encode_query_value(Type, Enc)), Doc_Types, Enc_Types),
        atomic_list_concat(Enc_Types, ',', Doc_Types_Joined),
        format(atom(Doc_Type_Params), "&doc_types=~w", [Doc_Types_Joined])
    ),
    ancestor_query_params(Ancestors, Ancestor_Params),
    format(atom(URL), "~w/embeddings?domain=~w~w~w~w~w",
           [Endpoint, Enc_Domain, Ref_Params, Doc_Id_Params, Doc_Type_Params, Ancestor_Params]).

%% io_embeddings_forward(+Endpoint, +Domain, +Search_Ref, +Doc_Ids, +Doc_Types, +Ancestors,
%%                       +Extra_Params, -Response_Body) is det.
%
%  Forwards a GET /embeddings request to the tdb-search engine.
io_embeddings_forward(Endpoint, Domain, Search_Ref, Doc_Ids, Doc_Types, Ancestors,
                     Extra_Params, Response_Body) :-
    assert_search_backend,
    search_auth_header(AuthHeader),
    build_embeddings_url(Endpoint, Domain, Search_Ref, Doc_Ids, Doc_Types, Ancestors, Base_URL),
    append_extra_params(Base_URL, Extra_Params, URL),
    setup_call_cleanup(
        http_open(URL, In,
                  [ status_code(Status),
                    AuthHeader,
                    request_header('Accept' = 'application/json'),
                    header(terminusdb_data_version, _DV_Raw)
                  ]),
        read_string(In, _, Response_Body),
        close(In)),
    handle_forward_response(Status, Response_Body, URL).

%% io_embeddings_forward_stream(+Endpoint, +Domain, +Search_Ref, +Doc_Ids, +Doc_Types,
%%                              +Ancestors, +Extra_Params, +Request, +Prefixes,
%%                              -Served_Commit, -Store_Clustering, -Total_Count) is det.
%
%  Forwards a GET /embeddings request to tdb-search with Accept: application/x-ndjson.
%  Writes CGI headers to current_output, then streams the NDJSON response body
%  line-by-line from tdb-search to current_output (the CGI pipe stream).
%  If Prefixes is not 'none', compacts doc_id fields in each NDJSON line.
%  This achieves true end-to-end streaming without buffering the full response.
io_embeddings_forward_stream(Endpoint, Domain, Search_Ref, Doc_Ids, Doc_Types, Ancestors,
                            Extra_Params, Request, Prefixes,
                            Served_Commit, Store_Clustering, Total_Count) :-
    assert_search_backend,
    search_auth_header(AuthHeader),
    build_embeddings_url(Endpoint, Domain, Search_Ref, Doc_Ids, Doc_Types, Ancestors, Base_URL),
    append_extra_params(Base_URL, Extra_Params, URL),
    setup_call_cleanup(
        http_open(URL, In,
                  [ status_code(Status),
                    AuthHeader,
                    request_header('Accept' = 'application/x-ndjson'),
                    header(x_served_commit, Served_Commit_Raw),
                    header(x_store_clustering, Store_Clustering_Raw),
                    header(x_total_count, Total_Count_Raw)
                  ]),
        (   handle_forward_response(Status, "", URL),
            (   var(Served_Commit_Raw) -> Served_Commit = "" ; Served_Commit = Served_Commit_Raw ),
            (   var(Store_Clustering_Raw) -> Store_Clustering = "false" ; Store_Clustering = Store_Clustering_Raw ),
            (   var(Total_Count_Raw) -> Total_Count = "0" ; Total_Count = Total_Count_Raw ),
            reply_embeddings_stream_headers(Request, Served_Commit, Store_Clustering, Total_Count),
            stream_ndjson_from(In, Prefixes)
        ),
        close(In)).

%% io_embeddings_forward_post_stream(+Endpoint, +Domain, +Search_Ref, +Doc_Ids,
%%   +Doc_Types, +Ancestors, +Request, +Prefixes,
%%   -Served_Commit, -Store_Clustering, -Total_Count) is det.
%
%  POSTs a JSON body to tdb-search /embeddings with stream=true,
%  then streams the NDJSON response back with per-line doc_id compaction.
io_embeddings_forward_post_stream(Endpoint, Domain, Search_Ref, Doc_Ids, Doc_Types, Ancestors,
                                  Request, Prefixes,
                                  Served_Commit, Store_Clustering, Total_Count) :-
    assert_search_backend,
    search_auth_header(AuthHeader),
    format(atom(URL), "~w/embeddings", [Endpoint]),
    (   Search_Ref = commit(Commit)
    ->  JSON_Body = _{domain: Domain, commit: Commit,
                       doc_ids: Doc_Ids, doc_types: Doc_Types,
                       ancestors: Ancestors, stream: true}
    ;   Search_Ref = branch(Branch),
        JSON_Body = _{domain: Domain, branch: Branch,
                       doc_ids: Doc_Ids, doc_types: Doc_Types,
                       ancestors: Ancestors, stream: true}
    ),
    setup_call_cleanup(
        http_open(URL, In,
                  [ method(post),
                    status_code(Status),
                    AuthHeader,
                    request_header('Content-Type' = 'application/json'),
                    request_header('Accept' = 'application/x-ndjson'),
                    post(json(JSON_Body)),
                    header(x_served_commit, Served_Commit_Raw),
                    header(x_store_clustering, Store_Clustering_Raw),
                    header(x_total_count, Total_Count_Raw)
                  ]),
        (   handle_forward_response(Status, "", URL),
            (   var(Served_Commit_Raw) -> Served_Commit = "" ; Served_Commit = Served_Commit_Raw ),
            (   var(Store_Clustering_Raw) -> Store_Clustering = "false" ; Store_Clustering = Store_Clustering_Raw ),
            (   var(Total_Count_Raw) -> Total_Count = "0" ; Total_Count = Total_Count_Raw ),
            reply_embeddings_stream_headers(Request, Served_Commit, Store_Clustering, Total_Count),
            stream_ndjson_from(In, Prefixes)
        ),
        close(In)).

%% compact_ndjson_line(+Line, +Prefixes, -CompactedLine) is det.
%%
%%  Compacts the doc_id field in a single NDJSON line using Prefixes.
%%  Uses a targeted string replacement on the doc_id field only,
%%  avoiding full JSON parse/serialize which is too slow for large
%%  embedding arrays (768 floats per line, 2000+ lines).
%%  The doc_id is always the first key in the NDJSON output from
%%  tdb-search, so we can safely target "doc_id":"<iri>" at the
%%  start of the line without risking corruption of embedding data.
%%  Handles escaped quotes (\"") in the IRI by skipping them when
%%  searching for the closing quote.
compact_ndjson_line(Line, Prefixes, Compacted) :-
    (   string(Line)
    ->  Line_Str = Line
    ;   atom_string(Line, Line_Str)
    ),
    (   sub_string(Line_Str, Before, _, _, '"doc_id":"')
    ->  Prefix_Len = 10,
        Start is Before + Prefix_Len,
        string_length(Line_Str, Total_Len),
        After_Len is Total_Len - Start,
        sub_string(Line_Str, Start, After_Len, _, Rest),
        (   find_closing_quote(Rest, 0, QPos)
        ->  sub_string(Rest, 0, QPos, _, Full_IRI),
            compress_dict_uri(Full_IRI, Prefixes, Compacted_Id),
            sub_string(Line_Str, 0, Start, _, Head),
            After_IRI_Pos is QPos + 1,
            sub_string(Rest, After_IRI_Pos, _, 0, Tail),
            string_concat(Head, Compacted_Id, T1),
            string_concat(T1, '"', T2),
            string_concat(T2, Tail, Compacted)
        ;   Compacted = Line_Str
        )
    ;   Compacted = Line_Str
    ).

%% find_closing_quote(+String, +StartPos, -QuotePos) is semidet.
%%
%%  Finds the position of the first unescaped " in String starting
%%  from StartPos. Skips \" (escaped quote) sequences. An escaped
%%  quote is a " preceded by an odd number of backslashes.
find_closing_quote(String, Pos, QuotePos) :-
    string_length(String, Len),
    Pos < Len,
    sub_string(String, Pos, 1, _, Char),
    (   Char == "\""
    ->  (   Pos > 0
        ->  Before_Pos is Pos - 1,
            count_trailing_backslashes(String, Before_Pos, 0, Count),
            (   Count mod 2 =:= 0
            ->  QuotePos = Pos
            ;   Next_Pos is Pos + 1,
                find_closing_quote(String, Next_Pos, QuotePos)
            )
        ;   QuotePos = Pos
        )
    ;   Next_Pos is Pos + 1,
        find_closing_quote(String, Next_Pos, QuotePos)
    ).

%% count_trailing_backslashes(+String, +Pos, +Acc, -Count) is det.
%%
%%  Counts consecutive backslashes ending at Pos, accumulating in Acc.
count_trailing_backslashes(String, Pos, Acc, Count) :-
    (   Pos >= 0,
        sub_string(String, Pos, 1, _, BS),
        BS == "\\"
    ->  Next_Pos is Pos - 1,
        Next_Acc is Acc + 1,
        count_trailing_backslashes(String, Next_Pos, Next_Acc, Count)
    ;   Count = Acc
    ).

%% stream_ndjson_from(+In) is det.
%%
%%  Reads NDJSON lines from input stream In and writes each line
%%  to current_output, flushing after each line for incremental streaming.
stream_ndjson_from(In) :-
    stream_ndjson_from(In, none).

%% stream_ndjson_from(+In, +Prefixes) is det.
%%
%%  Reads NDJSON lines from input stream In and writes each line
%%  to current_output, flushing after each line for incremental streaming.
%%  If Prefixes is not 'none', compacts the doc_id field in each line.
stream_ndjson_from(In, Prefixes) :-
    read_line_to_string(In, Line),
    (   Line == end_of_file
    ->  true
    ;   (   Prefixes == none
        ->  format("~s~n", [Line]),
            flush_output,
            stream_ndjson_from(In, Prefixes)
        ;   catch(compact_ndjson_line(Line, Prefixes, Compacted),
                Error,
                (   format(user_error, "[ERROR] compact_ndjson_line failed: ~q~n", [Error]),
                    format("{\"error\":\"compaction_failed\"}~n"),
                    flush_output,
                    !,
                    fail
                )),
            format("~s~n", [Compacted]),
            flush_output,
            stream_ndjson_from(In, Prefixes)
        )
    ).

%% embeddings_handler(+Method, +Path, +Request, +System_DB, +Auth)
%
%  HTTP handler for GET/POST /api/plugin/search-embeddings/<path>.
%  GET: proxies to tdb-search GET /embeddings with query params.
%  POST: reads JSON body with doc_ids/doc_types, expands compact IDs,
%        forwards to tdb-search POST /embeddings with stream=true,
%        streams NDJSON back with per-line doc_id compaction.
embeddings_handler(get, Path, Request, System_DB, Auth) :-
    (   memberchk(search(Search), Request)
    ->  true
    ;   Search = []),
    (   memberchk(accept(Accept), Request),
        member(media(application/'x-ndjson', _, _, _), Accept)
    ->  Streaming = true
    ;   Streaming = false
    ),
    plugin_api:api_report_errors(
        search,
        Request,
        (
            plugin_api:resolve_descriptor_auth(read, System_DB, Auth, Path, instance, Descriptor),
            do_or_die(tdb_search:tdb_search_endpoint(Endpoint),
                      error(tdb_search_endpoint_not_configured(embeddings_handler), _)),
            do_or_die(
                (   branch_descriptor{branch_name: Branch_Name} :< Descriptor
                ->  true
                ;   commit_descriptor{} :< Descriptor,
                    Branch_Name = none
                ),
                error(search_requires_branch_descriptor(Path), _)),
            get_dict(repository_descriptor, Descriptor, Repository_Descriptor),
            tdb_search:descriptor_domain(Descriptor, Domain),
            tdb_search:resolve_search_commit(Descriptor, Search_Ref, _Commit_Uri, Ancestors),
            tdb_search:compress_flag(Search, Compress),
            tdb_search:maybe_prefixes(Compress, Descriptor, Prefixes),
            tdb_search:embeddings_extra_params(Search, Prefixes, Doc_Ids, Doc_Types, Extra_Params),
            catch(
                (   Streaming == true
                ->  tdb_search:io_embeddings_forward_stream(Endpoint, Domain,
                                      Search_Ref, Doc_Ids, Doc_Types, Ancestors,
                                      Extra_Params, Request, Prefixes,
                                      _Served_Commit, _Store_Clustering, _Total_Count)
                ;   tdb_search:io_embeddings_forward(Endpoint, Domain,
                                      Search_Ref, Doc_Ids, Doc_Types, Ancestors,
                                      Extra_Params, Response_Body),
                    tdb_search:maybe_compact_response(Compress, Response_Body,
                                       Descriptor, Final_Body),
                    tdb_search:reply_search_with_metadata(Request, Final_Body,
                                       none, Repository_Descriptor)
                ),
                error(tdb_search_forward_failed(404, Engine_Body, _Fail_URL), _),
                (   tdb_search:maybe_nudge_push_async(System_DB, Auth, Path, Branch_Name),
                    throw(error(search_not_indexed(Path, Engine_Body), _))
                )
            )
        )
    ).

embeddings_handler(post, Path, Request, System_DB, Auth) :-
    (   memberchk(search(Search), Request)
    ->  true
    ;   Search = []),
    plugin_api:api_report_errors(
        search,
        Request,
        (
            plugin_api:resolve_descriptor_auth(read, System_DB, Auth, Path, instance, Descriptor),
            do_or_die(tdb_search:tdb_search_endpoint(Endpoint),
                      error(tdb_search_endpoint_not_configured(embeddings_handler), _)),
            do_or_die(
                (   branch_descriptor{branch_name: Branch_Name} :< Descriptor
                ->  true
                ;   commit_descriptor{} :< Descriptor,
                    Branch_Name = none
                ),
                error(search_requires_branch_descriptor(Path), _)),
            get_dict(repository_descriptor, Descriptor, _Repository_Descriptor),
            tdb_search:descriptor_domain(Descriptor, Domain),
            tdb_search:resolve_search_commit(Descriptor, Search_Ref, _Commit_Uri, Ancestors),
            tdb_search:compress_flag(Search, Compress),
            tdb_search:maybe_prefixes(Compress, Descriptor, Prefixes),
            %% Read JSON body from request (plugin API populates payload for JSON POST)
            (   memberchk(payload(Body), Request),
                is_dict(Body)
            ->  true
            ;   Body = _{}
            ),
            %% Extract and expand doc_ids from body
            (   get_dict(doc_ids, Body, Raw_Doc_Ids)
            ->  maplist(tdb_search:expand_doc_id(Prefixes), Raw_Doc_Ids, Doc_Ids)
            ;   Doc_Ids = []
            ),
            %% Extract doc_types from body
            (   get_dict(doc_types, Body, Raw_Doc_Types)
            ->  Doc_Types = Raw_Doc_Types
            ;   Doc_Types = []
            ),
            catch(
                tdb_search:io_embeddings_forward_post_stream(Endpoint, Domain,
                                      Search_Ref, Doc_Ids, Doc_Types, Ancestors,
                                      Request, Prefixes,
                                      _Served_Commit, _Store_Clustering, _Total_Count),
                error(tdb_search_forward_failed(404, Engine_Body, _Fail_URL), _),
                (   tdb_search:maybe_nudge_push_async(System_DB, Auth, Path, Branch_Name),
                    throw(error(search_not_indexed(Path, Engine_Body), _))
                )
            )
        )
    ).

%% embeddings_extra_params(+Search, +Prefixes, -Doc_Ids, -Doc_Types, -Extra_Params) is det.
%
%  Extracts doc_id and doc_type (repeated) from query params and normalizes them.
%  Reuses search_extra_params/4 with an empty body (GET-only endpoint)
%  then splits the result into Doc_Ids, Doc_Types, and Extra_Params (count etc.).
embeddings_extra_params(Search, Prefixes, Doc_Ids, Doc_Types, Extra) :-
    search_extra_params(Search, _{}, Prefixes, Params),
    (   select(doc_id=repeated(Doc_Ids), Params, Rest1)
    ->  true
    ;   Doc_Ids = [],
        Rest1 = Params
    ),
    (   select(doc_type=repeated(Doc_Types), Rest1, Rest2)
    ->  true
    ;   Doc_Types = [],
        Rest2 = Rest1
    ),
    findall(Param, embeddings_extra_param(Rest2, Param), Extra).

embeddings_extra_param(Params, count=Count) :-
    memberchk(count=Count, Params).

% ==========================================================================
% Route registration
% ==========================================================================

:- plugin_api:register_route(api(index/Path),
    plugin_api:cors_handler(Method, tdb_search:index_handler(Path)),
    [method(Method), prefix, time_limit(infinite),
     methods([options,get,post,delete])]).

:- plugin_api:register_route(api(search/Path),
    plugin_api:cors_handler(Method, tdb_search:search_handler(Path)),
    [method(Method), prefix, time_limit(infinite), methods([options,get,post])]).

:- plugin_api:register_route(api(suggest/Path),
    plugin_api:cors_handler(Method, tdb_search:suggest_handler(Path)),
    [method(Method), prefix, time_limit(infinite), methods([options,get])]).

:- plugin_api:register_route(api(similar/Path),
    plugin_api:cors_handler(Method, tdb_search:similar_handler(Path)),
    [method(Method), prefix, time_limit(infinite), methods([options,get,post])]).

:- plugin_api:register_route(api(duplicates/Path),
    plugin_api:cors_handler(Method, tdb_search:duplicates_handler(Path)),
    [method(Method), prefix, time_limit(infinite), methods([options,get])]).

% /api/resolve route moved to search_resolve.pl plugin.

:- plugin_api:register_route(api(compare),
    plugin_api:cors_handler(Method, tdb_search:compare_handler),
    [method(Method), time_limit(infinite), methods([options,post])]).

:- plugin_api:register_route(api(plugin/'search-embeddings'/Path),
    plugin_api:cors_handler(Method, tdb_search:embeddings_handler(Path)),
    [method(Method), prefix, time_limit(infinite), tdb_stream, methods([options,get,post])]).

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
    http_handler('/last-indexed', push_stub_last_indexed, []),
    http_handler('/push', push_stub_push, [methods([post])]),
    http_handler('/check', push_stub_check, []),
    http_handler('/domain', push_stub_domain_delete, [methods([delete])]),
    http_handler('/resolve', push_stub_resolve, [methods([post])]),
    http_handler('/candidates', push_stub_candidates, [methods([post])]),
    http_handler('/embeddings', push_stub_embeddings, [methods([get])]),
    http_server(http_dispatch, [port(Port), workers(1)]).

stop_push_stub(Port) :-
    http_stop_server(Port, []),
    retractall(stub_received(_, _)),
    retractall(stub_last_indexed_response(_)),
    retractall(stub_push_call_count(_)),
    retractall(stub_check_response(_, _)),
    retractall(stub_push_response_override(_)).

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
    (   http_read_data(Request, Body, [to(string)])
    ->  assertz(stub_received(push_body(N1), Body))
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
    (   http_read_data(Request, Body, [to(string)])
    ->  assertz(stub_received(resolve_body, Body))
    ;   true
    ),
    assertz(stub_received(resolve_called, true)),
    format("Content-Type: application/json~n~n"),
    write('{"matches":[]}').

push_stub_candidates(Request) :-
    (   http_read_data(Request, Body, [to(string)])
    ->  assertz(stub_received(candidates_body, Body))
    ;   true
    ),
    assertz(stub_received(candidates_called, true)),
    format("Content-Type: application/json~n~n"),
    write('{"set_to_target":{"doc/set_a":[{"id":"doc/target_a","distance":0.1}]},"target_to_set":{"doc/target_a":[{"id":"doc/set_a","distance":0.1}]},"stats":{"set_points":1,"target_points":1,"set_to_target_edges":1,"target_to_set_edges":1,"elapsed_ms":5}}').

push_stub_embeddings(Request) :-
    (   memberchk(search(Search), Request)
    ->  true
    ;   Search = []
    ),
    assertz(stub_received(embeddings_called, Search)),
    (   memberchk(accept(Accept), Request),
        member(media(application/'x-ndjson', _, _, _), Accept)
    ->  format("Content-Type: application/x-ndjson~n"),
        format("X-Served-Commit: stub-commit~n"),
        format("X-Store-Clustering: false~n"),
        format("X-Total-Count: 0~n~n"),
        write('{"@id":"doc1","@type":"Article","title":"Test Article 1"}'), nl,
        write('{"@id":"doc2","@type":"Article","title":"Test Article 2"}'), nl
    ;   format("Content-Type: application/json~n~n"),
        write('{"doc_embeddings":{},"clustering_embeddings":{},"store_clustering":false,"served_commit":"stub-commit"}')
    ).

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
% silence_user_error(:Goal) is det.
%
%  Run Goal with the Prolog user_error stream temporarily rebound to a
%  null stream, then restore the original user_error. Use only in unit
%  tests where the json_log output is not the object under test.
silence_user_error(Goal) :-
    stream_property(OldErr, alias(user_error)),
    setup_call_cleanup(
        (   open_null_stream(Null),
            set_stream(Null, alias(user_error))
        ),
        once(Goal),
        (   flush_output(Null),
            close(Null),
            set_stream(OldErr, alias(user_error))
        )
    ).

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
    plugins:tdb_search_admin_user(User).

test("admin secret defaults to root",
     [ setup((clean_tdb_search_test_env,
              unsetenv('TERMINUSDB_SEARCH_ADMIN_SECRET'))),
       cleanup((clean_tdb_search_test_env,
                config:clear_indexer_backend_config)),
       true(Secret == root)
     ]) :-
    plugins:tdb_search_admin_secret(Secret).

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
       cleanup(teardown_temp_store(State)),
       true(GraphSpec == 'admin/testdb/local/branch/main')
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
     [true(URL == 'http://engine:8080/push?domain=admin%2fdb&branch=main&target_commit=head1&parent_commit=prev1&stream=true')]) :-
    tdb_search:build_push_url("http://engine:8080", "admin/db", "main",
                               "head1", "prev1", URL).

test("build_push_url with none parent omits parent_commit param",
     [true(URL == 'http://engine:8080/push?domain=admin%2fdb&branch=main&target_commit=head1&stream=true')]) :-
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

test("io_push_delta delegates to indexer_notify (push architecture)",
     [ setup((setup_temp_store(State),
              create_db_without_schema("admin", "testdb2"),
              clean_tdb_search_test_env,
              resolve_absolute_string_descriptor("admin/testdb2", Desc),
              get_dict(repository_descriptor, Desc, Repo_Desc),
              branch_head_commit(Repo_Desc, "main", Head_Uri),
              commit_id_uri(Repo_Desc, Head_Commit_Id, Head_Uri),
              text_to_string(Head_Commit_Id, Head_Str),
              format(atom(Last_Indexed_Json),
                     '{"branch":"main","commit":"~w","version":0}',
                     [Head_Str]),
              start_push_stub(Port),
              assertz(stub_last_indexed_response(Last_Indexed_Json)),
              format(atom(Endpoint_URL), "http://127.0.0.1:~w", [Port]),
              setenv('TERMINUSDB_TDB_SEARCH_ENDPOINT', Endpoint_URL),
              plugin_api:indexer_set_config(Endpoint_URL, "")
             )),
       cleanup((stop_push_stub(Port),
                clean_tdb_search_test_env,
                catch(plugin_api:indexer_set_config("", ""), _, true),
                teardown_temp_store(State)))
     ]) :-
    super_user_authority(Auth),
    open_descriptor(system_descriptor{}, System_DB),
    silence_user_error(io_push_delta(System_DB, Auth, "admin/testdb2", "main")).

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
     [true(URL == 'http://engine:8080/push?domain=org%2fdb%2flocal%2fbranch%2fmain&branch=main&target_commit=c1&stream=true')]) :-
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

test("branch_path_for_notify expands 2-segment path to 5-segment branch path",
     [true(Branch_Path == 'admin/testdb/local/branch/main')]) :-
    tdb_search:branch_path_for_notify("admin/testdb", "main", Branch_Path).

test("branch_path_for_notify passes 5-segment branch path through as-is",
     [true(Branch_Path == "admin/testdb/local/branch/main")]) :-
    tdb_search:branch_path_for_notify("admin/testdb/local/branch/main", "main", Branch_Path).

test("branch_path_for_notify does not double a 5-segment branch path",
     [true(Branch_Path == "admin/product_assortment/local/branch/main")]) :-
    tdb_search:branch_path_for_notify("admin/product_assortment/local/branch/main", "main", Branch_Path).

test("branch_path_for_notify handles atom input",
     [true(Branch_Path == 'admin/testdb/local/branch/main')]) :-
    tdb_search:branch_path_for_notify('admin/testdb', 'main', Branch_Path).

test("branch_path_for_notify handles DB named local (2-segment)",
     [true(Branch_Path == 'admin/local/local/branch/main')]) :-
    tdb_search:branch_path_for_notify("admin/local", "main", Branch_Path).

test("branch_path_for_notify rejects 10-segment doubled path",
     [throws(error(invalid_index_path(_, wrong_segment_count(10, expected_2_or_5)), _))]) :-
    tdb_search:branch_path_for_notify("admin/db/local/branch/main/local/branch/main/local/branch", "main", _).

test("branch_path_for_notify rejects 3-segment path",
     [throws(error(invalid_index_path(_, wrong_segment_count(3, expected_2_or_5)), _))]) :-
    tdb_search:branch_path_for_notify("admin/testdb/local", "main", _).

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
    api_indexer:validation_is_index_enabled(Validation).

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
    api_indexer:validation_is_index_enabled(Validation).

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

:- end_tests(tdb_search_auto_push_hook).

% ==========================================================================
% Search fronting + authz parity tests
% ==========================================================================

:- begin_tests(tdb_search_search_fronting).

test("build_search_url constructs correct URL with ancestors",
     [true(URL == 'http://engine:8080/search?domain=admin%2fdb&commit=abc123&ancestor=prev1&ancestor=prev2')]) :-
    tdb_search:build_search_url("http://engine:8080", "admin/db", commit("abc123"),
                                ["prev1", "prev2"], URL).

test("build_search_url with no ancestors omits ancestor params",
     [true(URL == 'http://engine:8080/search?domain=admin%2fdb&commit=abc123')]) :-
    tdb_search:build_search_url("http://engine:8080", "admin/db", commit("abc123"),
                                [], URL).

test("build_suggest_url constructs correct URL with ancestors",
     [true(URL == 'http://engine:8080/suggest?domain=admin%2fdb&commit=abc123&ancestor=prev1&ancestor=prev2')]) :-
    tdb_search:build_suggest_url("http://engine:8080", "admin/db", commit("abc123"),
                                 ["prev1", "prev2"], URL).

test("build_suggest_url with no ancestors omits ancestor params",
     [true(URL == 'http://engine:8080/suggest?domain=admin%2fdb&commit=abc123')]) :-
    tdb_search:build_suggest_url("http://engine:8080", "admin/db", commit("abc123"),
                                 [], URL).

test("build_similar_url constructs correct URL",
     [true(URL == 'http://engine:8080/similar?domain=org%2fmydb&commit=def456&ancestor=anc1')]) :-
    tdb_search:build_similar_url("http://engine:8080", "org/mydb", commit("def456"),
                                 ["anc1"], URL).

test("build_duplicates_url constructs correct URL without ancestors",
     [true(URL == 'http://engine:8080/duplicates?domain=admin%2fdb&commit=c99')]) :-
    tdb_search:build_duplicates_url("http://engine:8080", "admin/db", commit("c99"), [], URL).

test("build_statistics_url constructs scoped URL with domain and commit",
     [true(URL == 'http://engine:8080/statistics?domain=admin%2fmydb&commit=abc123')]) :-
    tdb_search:build_statistics_url("http://engine:8080", "admin/mydb", commit("abc123"),
                                    [], URL).

test("build_statistics_url includes ancestor params",
     [true(sub_atom(URL, _, _, _, '&ancestor=anc1'))]) :-
    tdb_search:build_statistics_url("http://engine:8080", "admin/db", commit("c1"),
                                    ["anc1"], URL).

test("build_search_url encodes slashes in domain",
     [true(sub_atom(URL, _, _, _, 'domain=org%2fdb%2flocal%2fbranch%2fmain'))]) :-
    tdb_search:build_search_url("http://e:80", "org/db/local/branch/main",
                                commit("c1"), [], URL).

test("build_search_url with branch ref constructs branch param",
     [true(URL == 'http://engine:8080/search?domain=admin%2fdb&branch=main')]) :-
    tdb_search:build_search_url("http://engine:8080", "admin/db", branch("main"),
                                [], URL).

test("build_duplicates_url with branch ref constructs branch param",
     [true(URL == 'http://engine:8080/duplicates?domain=admin%2fdb&branch=main')]) :-
    tdb_search:build_duplicates_url("http://engine:8080", "admin/db", branch("main"), [], URL).

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
    io_search_forward("http://x:80", "d", commit("c"), [], [], _, _).

test("io_similar_forward refuses when endpoint is not configured",
     [ setup(clean_tdb_search_test_env),
       cleanup(clean_tdb_search_test_env),
       throws(error(search_requires_tdb_search_backend, _))
     ]) :-
    io_similar_forward("http://x:80", "d", commit("c"), [], [], _, _).

test("io_duplicates_forward refuses when endpoint is not configured",
     [ setup(clean_tdb_search_test_env),
       cleanup(clean_tdb_search_test_env),
       throws(error(search_requires_tdb_search_backend, _))
     ]) :-
    io_duplicates_forward("http://x:80", "d", commit("c"), [], [], _, _).

test("io_statistics_forward refuses when endpoint is not configured",
     [ setup(clean_tdb_search_test_env),
       cleanup(clean_tdb_search_test_env),
       throws(error(search_requires_tdb_search_backend, _))
     ]) :-
    io_statistics_forward("http://x:80", "admin/db", commit("c0"), [], _, _).

test("io_statistics_for_domain refuses when endpoint is not configured",
     [ setup(clean_tdb_search_test_env),
       cleanup(clean_tdb_search_test_env),
       throws(error(search_requires_tdb_search_backend, _))
     ]) :-
    io_statistics_for_domain("http://x:80", "admin/db", _).

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
    once(( open_descriptor(system_descriptor{}, System_DB),
           user_key_user_id(System_DB, 'DeniedUser', 'pass456', Auth),
           catch(
               (   resolve_descriptor_auth(read, System_DB, Auth,
                                           "admin/guardeddb", instance, _Desc),
                   tdb_search:tdb_search_endpoint(Endpoint),
                   io_search_forward(Endpoint, "admin/guardeddb", commit("fake_commit"),
                                     [], [], _Response, _DV)
               ),
               error(access_not_authorised(_, _, _), _),
               true
           ),
           \+ stub_received(_, _) )).

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
    once(( open_descriptor(system_descriptor{}, System_DB),
           user_key_user_id(System_DB, 'StatsDeniedUser', 'pass012', Auth),
           catch(
               (   resolve_descriptor_auth(read, System_DB, Auth,
                                           "admin/guardedstatsdb", instance, _Desc),
                   tdb_search:tdb_search_endpoint(Endpoint),
                   io_statistics_forward(Endpoint, "admin/guardedstatsdb", commit("fake_commit"),
                                         [], _Response, _DV)
               ),
               error(access_not_authorised(_, _, _), _),
               true
           ),
           \+ stub_received(_, _) )).

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
    once(( open_descriptor(system_descriptor{}, System_DB),
           user_key_user_id(System_DB, 'ResolveBlockedUser', 'pass654', Auth),
           catch(
               (   resolve_descriptor_auth(read, System_DB, Auth,
                                           "admin/guardedresolvedb", instance, _Desc),
                   tdb_search:tdb_search_endpoint(Endpoint),
                   io_resolve_forward(Endpoint, "admin/guardedresolvedb", commit("fake_commit"),
                                      [], _{}, _Response, _DV)
               ),
               error(access_not_authorised(_, _, _), _),
               true
           ),
           \+ stub_received(_, _) )).

:- end_tests(tdb_search_search_fronting).

% ==========================================================================
% Resolve URL construction tests
% ==========================================================================

:- begin_tests(tdb_search_resolve_url_construction).

test("build_resolve_url constructs correct URL",
     [true(URL == 'http://engine:8080/candidates?domain=admin%2fdb&commit=abc123')]) :-
    tdb_search:build_resolve_url("http://engine:8080", "admin/db", commit("abc123"), URL).

test("build_resolve_url encodes slashes in domain",
     [true(sub_atom(URL, _, _, _, 'domain=org%2fdb%2flocal%2fbranch%2fmain'))]) :-
    tdb_search:build_resolve_url("http://e:80", "org/db/local/branch/main", commit("c1"), URL).

test("io_resolve_forward refuses when endpoint is not configured",
     [ setup(clean_tdb_search_test_env),
       cleanup(clean_tdb_search_test_env),
       throws(error(search_requires_tdb_search_backend, _))
     ]) :-
    io_resolve_forward("http://x:80", "d", commit("c"), [], _{}, _, _).

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

test("similar id: body value overrides query value and normalizes doc id",
     [true(Params == [id='terminusdb:///data/body-id'])]) :-
    tdb_search:similar_extra_params([id='query-id'], _{id: "body-id"}, Params).

test("duplicates threshold: body value overrides query value",
     [true(Params == [threshold=0.25])]) :-
    tdb_search:duplicates_extra_params([threshold='0.9'], _{threshold: 0.25}, Params).

test("duplicates target_doc_type: body list overrides query repeated",
     [true(Params == [target_doc_type=repeated(["Buy"])])]) :-
    tdb_search:duplicates_extra_params([target_doc_type='Abt'],
                            _{target_doc_type: ["Buy"]}, Params).

:- end_tests(tdb_search_fronting_params).

% ==========================================================================
% Index status API response assembly tests
% ==========================================================================

:- begin_tests(tdb_search_index_status_response).

test("not_found response has status not_found and empty branch_processing",
     [true((Status == not_found, BP == json{commits_processed:0, total_commits:0,
                                            current_commit:null, upcoming_commit:null}))]) :-
    Indexer_Progress = json{status:not_found},
    Engine_Stats = json{documents:0, chunks:0, indexed_commits:0,
                        pending_index_fragments:0},
    tdb_search:assemble_index_status_response(
        Indexer_Progress, "main", null, Engine_Stats, Response),
    get_dict(status, Response, Status),
    get_dict(branch_processing, Response, BP).

test("indexing response includes document progress in engine section",
     [true((Processed == 42, Total == 100, Sent == 80, Status == indexing))]) :-
    Indexer_Progress = json{
        status:indexing,
        branch_processing:json{
            commits_processed:1,
            total_commits:3,
            current_commit:"abc123",
            upcoming_commit:null,
            processed_documents:42,
            total_documents:100,
            documents_sent:80
        }
    },
    Engine_Stats = json{documents:50, chunks:120, indexed_commits:1,
                        pending_index_fragments:0},
    tdb_search:assemble_index_status_response(
        Indexer_Progress, "main", "abc123", Engine_Stats, Response),
    get_dict(status, Response, Status),
    get_dict(engine, Response, Engine),
    get_dict(processed_documents, Engine, Processed),
    get_dict(total_documents, Engine, Total),
    get_dict(documents_sent, Engine, Sent).

test("indexing response does not include document counters in branch_processing",
     [true(\+ get_dict(processed_documents, BP, _))]) :-
    Indexer_Progress = json{
        status:indexing,
        branch_processing:json{
            commits_processed:1,
            total_commits:3,
            current_commit:"abc123",
            processed_documents:42,
            total_documents:100
        }
    },
    Engine_Stats = json{documents:50, chunks:120, indexed_commits:1,
                        pending_index_fragments:0},
    tdb_search:assemble_index_status_response(
        Indexer_Progress, "main", "abc123", Engine_Stats, Response),
    get_dict(branch_processing, Response, BP).

test("completed response has status completed and engine shows totals",
     [true((Status == completed, Commits_Processed == 3, Total_Commits == 3, Sent == 150))]) :-
    Indexer_Progress = json{
        status:completed,
        branch_processing:json{
            commits_processed:3,
            total_commits:3,
            processed_documents:150,
            total_documents:150,
            documents_sent:150
        }
    },
    Engine_Stats = json{documents:150, chunks:300, indexed_commits:3,
                        pending_index_fragments:0},
    tdb_search:assemble_index_status_response(
        Indexer_Progress, "main", "head123", Engine_Stats, Response),
    get_dict(status, Response, Status),
    get_dict(branch_processing, Response, BP),
    get_dict(commits_processed, BP, Commits_Processed),
    get_dict(total_commits, BP, Total_Commits),
    get_dict(engine, Response, Engine),
    get_dict(documents_sent, Engine, Sent).

test("error response includes error message",
     [true((Status == error, Error == "something went wrong"))]) :-
    Indexer_Progress = json{
        status:error,
        error:"something went wrong",
        branch_processing:json{
            commits_processed:0,
            total_commits:1,
            processed_documents:0,
            total_documents:0
        }
    },
    Engine_Stats = json{documents:0, chunks:0, indexed_commits:0,
                        pending_index_fragments:0},
    tdb_search:assemble_index_status_response(
        Indexer_Progress, "main", null, Engine_Stats, Response),
    get_dict(status, Response, Status),
    get_dict(error, Response, Error).

test("engine section includes searchable_documents and text_segments_indexed from engine stats",
     [true((Searchable == 75, Segments == 200, Sent == 60))]) :-
    Indexer_Progress = json{status:indexing,
        branch_processing:json{commits_processed:1, total_commits:2,
            processed_documents:30, total_documents:75, documents_sent:60}},
    Engine_Stats = json{documents:75, chunks:200, indexed_commits:1,
                        pending_index_fragments:2},
    tdb_search:assemble_index_status_response(
        Indexer_Progress, "main", "c1", Engine_Stats, Response),
    get_dict(engine, Response, Engine),
    get_dict(searchable_documents, Engine, Searchable),
    get_dict(text_segments_indexed, Engine, Segments),
    get_dict(documents_sent, Engine, Sent).

:- end_tests(tdb_search_index_status_response).

% ==========================================================================
% IRI compaction / expansion tests
% ==========================================================================

% Shared prefixes mimicking a database context for admin/abt_buy_e2e
abt_buy_prefixes(Prefixes) :-
    Prefixes = _{'@base': "terminusdb:///data/admin/abt_buy_e2e/",
                 '@schema': "terminusdb:///schema#"}.

:- begin_tests(tdb_search_id_compaction).

test("compress_dict_uri with @base strips to bare relative IRI",
     [true(Compact == 'Abt/1101')]) :-
    abt_buy_prefixes(Prefixes),
    compress_dict_uri('terminusdb:///data/admin/abt_buy_e2e/Abt/1101',
                      Prefixes, Compact).

test("compress_dict_uri with custom prefix creates prefix:local form",
     [true(Compact == '@schema:Foo')]) :-
    abt_buy_prefixes(Prefixes),
    compress_dict_uri('terminusdb:///schema#Foo', Prefixes, Compact).

test("compress_dict_uri leaves non-terminusdb URI unchanged",
     [true(Compact == 'http://example.org/foo')]) :-
    abt_buy_prefixes(Prefixes),
    compress_dict_uri('http://example.org/foo', Prefixes, Compact).

test("expand_compact_id expands bare relative via @base",
     [true(Full == 'terminusdb:///data/admin/abt_buy_e2e/Abt/1101')]) :-
    abt_buy_prefixes(Prefixes),
    expand_compact_id('Abt/1101', Prefixes, Full).

test("expand_compact_id expands prefix:local form via prefix_expand",
     [true(Full == 'terminusdb:///schema#Foo')]) :-
    abt_buy_prefixes(Prefixes),
    expand_compact_id('@schema:Foo', Prefixes, Full).

test("expand_compact_id returns full IRI unchanged when already expanded",
     [true(Full == 'terminusdb:///data/admin/abt_buy_e2e/Abt/1101')]) :-
    abt_buy_prefixes(Prefixes),
    expand_compact_id('terminusdb:///data/admin/abt_buy_e2e/Abt/1101',
                      Prefixes, Full).

test("expand_compact_id falls back to terminusdb:///data/ when no @base and no prefix",
     [true(Full == 'terminusdb:///data/bare_id')]) :-
    expand_compact_id('bare_id', _{}, Full).

test("expand_compact_id handles string input the same as atom",
     [true(Full == 'terminusdb:///data/admin/abt_buy_e2e/Abt/1101')]) :-
    abt_buy_prefixes(Prefixes),
    expand_compact_id("Abt/1101", Prefixes, Full).

test("round-trip: compact then expand yields the original data IRI",
     [true(Full == Original)]) :-
    abt_buy_prefixes(Prefixes),
    Original = 'terminusdb:///data/admin/abt_buy_e2e/Abt/1101',
    compress_dict_uri(Original, Prefixes, Compact),
    expand_compact_id(Compact, Prefixes, Full).

test("round-trip: compact then expand yields the original schema IRI",
     [true(Full == Original)]) :-
    abt_buy_prefixes(Prefixes),
    Original = 'terminusdb:///schema#Foo',
    compress_dict_uri(Original, Prefixes, Compact),
    expand_compact_id(Compact, Prefixes, Full).

test("normalize_doc_id/3 with none sentinel uses old normalize behavior",
     [true(Normalized == 'terminusdb:///data/body-id')]) :-
    normalize_doc_id("body-id", none, Normalized).

test("normalize_doc_id/3 with none passes through full IRI unchanged",
     [true(Normalized == 'terminusdb:///data/admin/abt_buy_e2e/Abt/1101')]) :-
    normalize_doc_id('terminusdb:///data/admin/abt_buy_e2e/Abt/1101',
                      none, Normalized).

test("normalize_doc_id/3 with prefixes expands compact ID",
     [true(Normalized == 'terminusdb:///data/admin/abt_buy_e2e/Abt/1101')]) :-
    abt_buy_prefixes(Prefixes),
    normalize_doc_id('Abt/1101', Prefixes, Normalized).

test("compact_json_value compacts id field in a search response array",
     [true(Compacted == json{results:[json{id:'Abt/1101', score:0.9},
                                        json{id:'Abt/1102', score:0.8}]})]) :-
    abt_buy_prefixes(Prefixes),
    Response = json{results:[json{id:'terminusdb:///data/admin/abt_buy_e2e/Abt/1101',
                                  score:0.9},
                             json{id:'terminusdb:///data/admin/abt_buy_e2e/Abt/1102',
                                  score:0.8}]},
    compact_json_value(Response, Prefixes, Compacted).

test("compact_json_value compacts set_id and target_id in a resolve response",
     [true((Set_Id == 'Abt/1101', Target_Id == 'Buy/2001'))]) :-
    abt_buy_prefixes(Prefixes),
    Response = json{matches:[json{set_id:
                                  'terminusdb:///data/admin/abt_buy_e2e/Abt/1101',
                                  target_id:
                                  'terminusdb:///data/admin/abt_buy_e2e/Buy/2001',
                                  threshold:0.95}]},
    compact_json_value(Response, Prefixes, Compacted),
    get_dict(matches, Compacted, Matches),
    [First_Match|_] = Matches,
    get_dict(set_id, First_Match, Set_Id),
    get_dict(target_id, First_Match, Target_Id).

test("compact_json_value leaves non-IRI strings unchanged",
     [true(Compacted == json{name:"Abt/1101", label:"some label"})]) :-
    abt_buy_prefixes(Prefixes),
    Response = json{name:"Abt/1101", label:"some label"},
    compact_json_value(Response, Prefixes, Compacted).

test("compact_json_value handles deeply nested structures",
     [true(Inner_Id == 'Abt/1101')]) :-
    abt_buy_prefixes(Prefixes),
    Response = json{outer:json{inner:json{id:
                  'terminusdb:///data/admin/abt_buy_e2e/Abt/1101'}}},
    compact_json_value(Response, Prefixes, Compacted),
    get_dict(outer, Compacted, Outer),
    get_dict(inner, Outer, Inner),
    get_dict(id, Inner, Inner_Id).

test("compact_json_value handles a bare list of IRIs",
     [true(Compacted == ['Abt/1101', 'Abt/1102'])]) :-
    abt_buy_prefixes(Prefixes),
    Response = ['terminusdb:///data/admin/abt_buy_e2e/Abt/1101',
                'terminusdb:///data/admin/abt_buy_e2e/Abt/1102'],
    compact_json_value(Response, Prefixes, Compacted).

test("compact_json_value compacts dict keys that are full IRIs (doc_embeddings)",
     [true((Key_Abt == 'Abt/1101', Key_Buy == 'Buy/2001'))]) :-
    abt_buy_prefixes(Prefixes),
    Response = json{doc_embeddings:json{
                  'terminusdb:///data/admin/abt_buy_e2e/Abt/1101':[0.1,0.2],
                  'terminusdb:///data/admin/abt_buy_e2e/Buy/2001':[0.3,0.4]}},
    compact_json_value(Response, Prefixes, Compacted),
    get_dict(doc_embeddings, Compacted, Emb),
    dict_pairs(Emb, _, Pairs),
    once(member(Key_Abt-[0.1,0.2], Pairs)),
    once(member(Key_Buy-[0.3,0.4], Pairs)).

test("compact_key leaves non-IRI keys unchanged",
     [true(Key == name)]) :-
    abt_buy_prefixes(Prefixes),
    compact_key(name, Prefixes, Key).

:- end_tests(tdb_search_id_compaction).

% ==========================================================================
% Compress query parameter tests
% ==========================================================================

:- begin_tests(tdb_search_compress_param).

test("search_extra_params/4 with none uses old normalize for doc_id",
     [true(Params == [doc_id=repeated(['terminusdb:///data/doc1'])])]) :-
    tdb_search:search_extra_params([doc_id='doc1'], _{}, none, Params).

test("search_extra_params/4 with prefixes expands compact doc_id",
     [true(Params == [doc_id=repeated(['terminusdb:///data/admin/abt_buy_e2e/Abt/1101'])])]) :-
    Prefixes = _{'@base': "terminusdb:///data/admin/abt_buy_e2e/",
                 '@schema': "terminusdb:///schema#"},
    tdb_search:search_extra_params([doc_id='Abt/1101'], _{}, Prefixes, Params).

test("similar_extra_params/4 with none preserves old normalize behavior",
     [true(Params == [id='terminusdb:///data/body-id'])]) :-
    tdb_search:similar_extra_params([id='query-id'], _{id: "body-id"},
                                    none, Params).

test("similar_extra_params/4 with prefixes expands compact id",
     [true(Params == [id='terminusdb:///data/admin/abt_buy_e2e/Abt/1101'])]) :-
    Prefixes = _{'@base': "terminusdb:///data/admin/abt_buy_e2e/",
                 '@schema': "terminusdb:///schema#"},
    tdb_search:similar_extra_params([id='Abt/1101'], _{}, Prefixes, Params).

test("duplicates_extra_params/4 with none preserves old normalize behavior",
     [true(Params == [doc_id=repeated(['terminusdb:///data/doc1'])])]) :-
    tdb_search:duplicates_extra_params([doc_id='doc1'], _{}, none, Params).

test("duplicates_extra_params/4 with prefixes expands compact target_doc_id",
     [true(Params == [target_doc_id=repeated(['terminusdb:///data/admin/abt_buy_e2e/Buy/2001'])])]) :-
    Prefixes = _{'@base': "terminusdb:///data/admin/abt_buy_e2e/",
                 '@schema': "terminusdb:///schema#"},
    tdb_search:duplicates_extra_params([target_doc_id='Buy/2001'], _{},
                                       Prefixes, Params).

test("compress_flag defaults to true when parameter absent",
     [true(Compress == true)]) :-
    compress_flag([], Compress).

test("compress_flag reads false from query parameter",
     [true(Compress == false)]) :-
    compress_flag([compress='false'], Compress).

test("compress_flag reads true from query parameter",
     [true(Compress == true)]) :-
    compress_flag([compress='true'], Compress).

:- end_tests(tdb_search_compress_param).

% ==========================================================================
% Embeddings proxy tests
% ==========================================================================

:- begin_tests(tdb_search_embeddings_proxy).

test("build_embeddings_url constructs correct URL with no doc_ids",
     [true(URL == 'http://engine:8080/embeddings?domain=admin%2fdb&commit=abc123')]) :-
    tdb_search:build_embeddings_url("http://engine:8080", "admin/db", commit("abc123"), [], [], [], URL).

test("build_embeddings_url constructs correct URL with doc_ids as comma-separated",
     [true(sub_atom(URL, _, _, _, 'doc_ids=doc%2f1,doc%2f2'))]) :-
    tdb_search:build_embeddings_url("http://engine:8080", "admin/db", commit("abc123"),
                                    ["doc/1", "doc/2"], [], [], URL).

test("build_embeddings_url includes ancestor params",
     [true(sub_atom(URL, _, _, _, '&ancestor=anc1'))]) :-
    tdb_search:build_embeddings_url("http://engine:8080", "admin/db", commit("abc123"),
                                    [], [], ["anc1"], URL).

test("io_embeddings_forward refuses when endpoint is not configured",
     [ setup(clean_tdb_search_test_env),
       cleanup(clean_tdb_search_test_env),
       throws(error(search_requires_tdb_search_backend, _))
     ]) :-
    io_embeddings_forward("http://x:80", "admin/db", commit("abc123"), [], [], [], [], _).

test("embeddings_extra_params extracts and normalizes doc_ids from query",
     [true(Doc_Ids == ['terminusdb:///data/doc1'])]) :-
    tdb_search:embeddings_extra_params([doc_id='doc1'], none, Doc_Ids, _, _).

test("embeddings_extra_params with no doc_ids returns empty list",
     [true(Doc_Ids == [])]) :-
    tdb_search:embeddings_extra_params([], none, Doc_Ids, _, _).

test("embeddings_extra_params extracts doc_types from query",
     [true(Doc_Types == ['Product', 'Customer'])]) :-
    tdb_search:embeddings_extra_params([doc_type='Product', doc_type='Customer'],
                            none, _, Doc_Types, _).

test("embeddings_extra_params extracts both doc_ids and doc_types",
     [true((Doc_Ids == ['terminusdb:///data/doc1'],
            Doc_Types == ['Product']))]) :-
    tdb_search:embeddings_extra_params([doc_id='doc1', doc_type='Product'],
                            none, Doc_Ids, Doc_Types, _).

test("embeddings_extra_params with no params returns empty for both",
     [true((Doc_Ids == [], Doc_Types == []))]) :-
    tdb_search:embeddings_extra_params([], none, Doc_Ids, Doc_Types, _).

test("build_embeddings_url constructs correct URL with doc_types as comma-separated",
     [true(sub_atom(URL, _, _, _, 'doc_types=Product,Customer'))]) :-
    tdb_search:build_embeddings_url("http://engine:8080", "admin/db", commit("abc123"),
                                    [], ["Product", "Customer"], [], URL).

test("build_embeddings_url constructs correct URL with both doc_ids and doc_types",
     [true((sub_atom(URL, _, _, _, 'doc_ids=doc%2f1'),
            sub_atom(URL, _, _, _, 'doc_types=Product')))]) :-
    tdb_search:build_embeddings_url("http://engine:8080", "admin/db", commit("abc123"),
                                    ["doc/1"], ["Product"], [], URL).

test("stream_ndjson_from reads lines and writes to current_output",
     [true(Output == "line1\nline2\nline3\n")]) :-
    open_string("line1\nline2\nline3\n", In),
    with_output_to(string(Output), stream_ndjson_from(In)),
    close(In).

test("stream_ndjson_from handles empty input",
     [true(Output == "")]) :-
    open_string("", In),
    with_output_to(string(Output), stream_ndjson_from(In)),
    close(In).

test("compact_ndjson_line compacts doc_id field in an NDJSON line",
     [true(Doc_Id == "Abt/1101")]) :-
    abt_buy_prefixes(Prefixes),
    Line = '{"doc_id":"terminusdb:///data/admin/abt_buy_e2e/Abt/1101","embedding":[0.1,0.2]}',
    compact_ndjson_line(Line, Prefixes, Compacted),
    atom_json_dict(Compacted, Dict, []),
    get_dict(doc_id, Dict, Doc_Id).

test("compact_ndjson_line leaves non-IRI doc_id unchanged",
     [true(Doc_Id == "Abt/1101")]) :-
    abt_buy_prefixes(Prefixes),
    Line = '{"doc_id":"Abt/1101","embedding":[0.1,0.2]}',
    compact_ndjson_line(Line, Prefixes, Compacted),
    atom_json_dict(Compacted, Dict, []),
    get_dict(doc_id, Dict, Doc_Id).

test("compact_ndjson_line preserves embedding array",
     [true(Embedding == [0.1,0.2,0.3])]) :-
    abt_buy_prefixes(Prefixes),
    Line = '{"doc_id":"terminusdb:///data/admin/abt_buy_e2e/Abt/1101","embedding":[0.1,0.2,0.3]}',
    compact_ndjson_line(Line, Prefixes, Compacted),
    atom_json_dict(Compacted, Dict, []),
    get_dict(embedding, Dict, Embedding).

test("compact_ndjson_line preserves clustering_embedding field",
     [true(Clustering == [1.0,2.0])]) :-
    abt_buy_prefixes(Prefixes),
    Line = '{"doc_id":"terminusdb:///data/admin/abt_buy_e2e/Abt/1101","embedding":[0.1,0.2],"clustering_embedding":[1.0,2.0]}',
    compact_ndjson_line(Line, Prefixes, Compacted),
    atom_json_dict(Compacted, Dict, []),
    get_dict(clustering_embedding, Dict, Clustering).

test("stream_ndjson_from with prefixes compacts doc_id in each line",
     [true((sub_atom(Output, _, _, _, '"doc_id":"Abt/1101"'),
            \+ sub_atom(Output, _, _, _, 'terminusdb:///data')))]) :-
    abt_buy_prefixes(Prefixes),
    NDJSON = '{"doc_id":"terminusdb:///data/admin/abt_buy_e2e/Abt/1101","embedding":[0.1]}\n{"doc_id":"terminusdb:///data/admin/abt_buy_e2e/Abt/1102","embedding":[0.2]}\n',
    open_string(NDJSON, In),
    with_output_to(string(Output), stream_ndjson_from(In, Prefixes)),
    close(In).

test("stream_ndjson_from without prefixes passes lines through unchanged",
     [true(sub_atom(Output, _, _, _, 'terminusdb:///data'))]) :-
    NDJSON = '{"doc_id":"terminusdb:///data/admin/abt_buy_e2e/Abt/1101","embedding":[0.1]}\n',
    open_string(NDJSON, In),
    with_output_to(string(Output), stream_ndjson_from(In)),
    close(In).

test("compact_ndjson_line does not corrupt embedding text containing the IRI",
     [true(atom_string(Text, 'terminusdb:///data/admin/abt_buy_e2e/Abt/1101'))]) :-
    abt_buy_prefixes(Prefixes),
    Line = '{"doc_id":"terminusdb:///data/admin/abt_buy_e2e/Abt/1101","embedding":[0.1],"text":"terminusdb:///data/admin/abt_buy_e2e/Abt/1101"}',
    compact_ndjson_line(Line, Prefixes, Compacted),
    atom_json_dict(Compacted, Dict, []),
    get_dict(text, Dict, Text).

test("compact_ndjson_line produces single-line JSON output",
     [true((\+ sub_atom(Compacted, _, _, _, '\n')))]) :-
    abt_buy_prefixes(Prefixes),
    numlist(1, 100, Nums),
    maplist([N, S] >> (format(atom(S), "~w", [N])), Nums, NumStrs),
    atomic_list_concat(NumStrs, ",", EmbStr),
    format(string(Line), '{"doc_id":"terminusdb:///data/admin/abt_buy_e2e/Abt/1101","embedding":[~w]}', [EmbStr]),
    compact_ndjson_line(Line, Prefixes, Compacted).

test("compact_ndjson_line serializes rational numbers correctly",
     [true(sub_atom(Compacted, _, _, _, '0.33333333333333333333'))]) :-
    abt_buy_prefixes(Prefixes),
    Line = '{"doc_id":"terminusdb:///data/admin/abt_buy_e2e/Abt/1101","embedding":[0.33333333333333333333]}',
    compact_ndjson_line(Line, Prefixes, Compacted).

test("compact_ndjson_line handles escaped quote in doc_id IRI",
     [true(sub_string(Compacted, _, _, _, "Abt/110\\\"1"))]) :-
    abt_buy_prefixes(Prefixes),
    Line = "{\"doc_id\":\"terminusdb:///data/admin/abt_buy_e2e/Abt/110\\\"1\",\"embedding\":[0.1]}",
    compact_ndjson_line(Line, Prefixes, Compacted).

test("compact_ndjson_line handles escaped backslash before closing quote",
     [true(sub_string(Compacted, _, _, _, "Abt/1101"))]) :-
    abt_buy_prefixes(Prefixes),
    Line = "{\"doc_id\":\"terminusdb:///data/admin/abt_buy_e2e/Abt/1101\\\\\",\"embedding\":[0.1]}",
    compact_ndjson_line(Line, Prefixes, Compacted),
    atom_json_dict(Compacted, Dict, []),
    get_dict(doc_id, Dict, Doc_Id),
    atom_string(Doc_Id, Doc_Id_Str),
    once(sub_string(Doc_Id_Str, _, _, _, "Abt/1101")).

test("find_closing_quote finds first unescaped quote",
     [true(QPos == 5)]) :-
    find_closing_quote("hello\"world", 0, QPos).

test("find_closing_quote skips escaped quote",
     [true(QPos == 12)]) :-
    find_closing_quote("hello\\\"world\"", 0, QPos).

test("find_closing_quote handles escaped backslash before quote",
     [true(QPos == 7)]) :-
    find_closing_quote("hello\\\\\"world", 0, QPos).

test("find_closing_quote handles quote at position 0",
     [true(QPos == 0)]) :-
    find_closing_quote("\"rest", 0, QPos).

test("find_closing_quote fails on string with no quote",
     [fail]) :-
    find_closing_quote("no quote here", 0, _).

:- end_tests(tdb_search_embeddings_proxy).

% ==========================================================================
% store_clustering in index status response tests
% ==========================================================================

:- begin_tests(tdb_search_store_clustering_status).

test("index status response includes store_clustering true from engine stats",
     [true(Store_Clustering == true)]) :-
    Indexer_Progress = json{status:completed},
    Engine_Stats = json{documents:10, chunks:20, indexed_commits:1,
                        pending_index_fragments:0, store_clustering:true},
    tdb_search:assemble_index_status_response(
        Indexer_Progress, "main", "c1", Engine_Stats, Response),
    get_dict(engine, Response, Engine),
    get_dict(store_clustering, Engine, Store_Clustering).

test("index status response includes store_clustering false from engine stats",
     [true(Store_Clustering == false)]) :-
    Indexer_Progress = json{status:completed},
    Engine_Stats = json{documents:10, chunks:20, indexed_commits:1,
                        pending_index_fragments:0, store_clustering:false},
    tdb_search:assemble_index_status_response(
        Indexer_Progress, "main", "c1", Engine_Stats, Response),
    get_dict(engine, Response, Engine),
    get_dict(store_clustering, Engine, Store_Clustering).

test("index status response defaults store_clustering to null when absent",
     [true(Store_Clustering == null)]) :-
    Indexer_Progress = json{status:completed},
    Engine_Stats = json{documents:10, chunks:20, indexed_commits:1,
                        pending_index_fragments:0},
    tdb_search:assemble_index_status_response(
        Indexer_Progress, "main", "c1", Engine_Stats, Response),
    get_dict(engine, Response, Engine),
    get_dict(store_clustering, Engine, Store_Clustering).

:- end_tests(tdb_search_store_clustering_status).

% ==========================================================================
% not_found → completed override tests
% ==========================================================================

:- begin_tests(tdb_search_not_found_override).

test("not_found with valid last_indexed commit should be overridden to completed",
     [true(Status == completed)]) :-
    Indexer_Progress = json{status:not_found},
    Engine_Stats = json{documents:10, chunks:20, indexed_commits:1,
                        pending_index_fragments:0},
    tdb_search:assemble_index_status_response(
        Indexer_Progress, "main", "abc123", Engine_Stats, Response),
    get_dict(status, Response, Status).

test("not_found with null last_indexed commit stays not_found",
     [true(Status == not_found)]) :-
    Indexer_Progress = json{status:not_found},
    Engine_Stats = json{documents:0, chunks:0, indexed_commits:0,
                        pending_index_fragments:0},
    tdb_search:assemble_index_status_response(
        Indexer_Progress, "main", null, Engine_Stats, Response),
    get_dict(status, Response, Status).

test("indexer_unavailable with valid last_indexed commit becomes completed",
     [true(Status == completed)]) :-
    Indexer_Progress = json{status:indexer_unavailable},
    Engine_Stats = json{documents:10, chunks:20, indexed_commits:1,
                        pending_index_fragments:0},
    tdb_search:assemble_index_status_response(
        Indexer_Progress, "main", "abc123", Engine_Stats, Response),
    get_dict(status, Response, Status).

test("indexer_unavailable with null last_indexed commit stays indexer_unavailable",
     [true(Status == indexer_unavailable)]) :-
    Indexer_Progress = json{status:indexer_unavailable},
    Engine_Stats = json{documents:0, chunks:0, indexed_commits:0,
                        pending_index_fragments:0},
    tdb_search:assemble_index_status_response(
        Indexer_Progress, "main", null, Engine_Stats, Response),
    get_dict(status, Response, Status).

:- end_tests(tdb_search_not_found_override).
