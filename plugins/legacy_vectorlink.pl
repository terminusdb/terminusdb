:- module(legacy_vectorlink, [
    api_start_job/3,
    api_check_job/2,
    semantic_indexer_endpoint/1,
    clean_legacy_vectorlink_env/0
]).

/** <module> legacy_vectorlink plugin — legacy pull-based indexing (deprecated in 12.1)

Activates when TERMINUSDB_SEMANTIC_INDEXER_ENDPOINT is set. Provides:
- Legacy pull trigger: GET /start on the legacy vectorlink engine
- Legacy job status: GET /check on the legacy vectorlink engine
- HTTP handler for /api/index (streams NDJSON via api_index_jobs/8)
*/

:- use_module(core(plugin_api)).
:- use_module(library(json)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_stream)).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).
:- use_module(library(yall)).
:- use_module(library(lists)).
:- use_module(library(dicts)).

% ==========================================================================
% Config predicates — plugin-owned, using generic plugin_api env helpers
% ==========================================================================

semantic_indexer_endpoint(Endpoint) :-
    plugin_env('TERMINUSDB_SEMANTIC_INDEXER_ENDPOINT', Endpoint).

clean_legacy_vectorlink_env :-
    abolish_plugin_env('TERMINUSDB_SEMANTIC_INDEXER_ENDPOINT'),
    unsetenv('TERMINUSDB_SEMANTIC_INDEXER_ENDPOINT').

% ==========================================================================
% Legacy pull trigger (moved from api_indexer.pl)
% ==========================================================================

%% api_start_job(+Domain, +Commit, -Task_Id) is det.
%
%  Legacy pull trigger. Calls GET /start on the vectorlink engine.
%  Refuses loud if the endpoint is not configured.
api_start_job(Domain, Commit, Task_Id) :-
    do_or_die(legacy_vectorlink:semantic_indexer_endpoint(Endpoint),
              error(semantic_indexer_endpoint_not_configured(api_start_job), _)),
    http_get(
        [ host(Endpoint),
          path('/start'),
          search([ domain=Domain,
                   commit=Commit])],
        Task_Id,
        []).

%% api_check_job(+Task_Id, -Status) is det.
%
%  Legacy job status check. Calls GET /check on the vectorlink engine.
api_check_job(Task_Id, Status) :-
    do_or_die(legacy_vectorlink:semantic_indexer_endpoint(Endpoint),
              error(semantic_indexer_endpoint_not_configured(api_check_job), _)),
    http_get(
        [ host(Endpoint),
          path('/check'),
          search([ task_id=Task_Id ])],
        Status,
        []).

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
% HTTP handler (moved from routes.pl)
% ==========================================================================

index_handler(get, Path, Request, System_DB, Auth) :-
    (   memberchk(search(Search), Request)
    ->  true
    ;   Search = []),
    plugin_api:api_report_errors(
        index,
        Request,
        (
            do_or_die(legacy_vectorlink:semantic_indexer_endpoint(_),
                      error(semantic_indexer_endpoint_not_configured(index_handler), _)),
            param_value_search_required(Search, commit_id, text, Commit_Id),
            param_value_search_optional(Search, previous_commit_id, text, none, Previous_Commit_Id),
            (   Previous_Commit_Id = none
            ->  Maybe_Previous_Commit_Id = Previous_Commit_Id
            ;   Maybe_Previous_Commit_Id = some(Previous_Commit_Id)
            ),
            api_index_jobs(
                System_DB,
                Auth,
                current_output,
                [Stream]>>(
                    write(Stream,'Status: 200'),nl(Stream),
                    write(Stream,'Content-Type: application/json'),nl(Stream),
                    format("Transfer-Encoding: chunked~n"),
                    nl(Stream)),
                Path,
                Commit_Id,
                Maybe_Previous_Commit_Id,
                [])
        )
    ).

% ==========================================================================
% Route registration
% ==========================================================================
%
% Route registration for api(index/Path) is handled exclusively by
% vectorlink.pl, which implements index_handler for get, post, and delete.
% This plugin only implements index_handler(get,...); registering the route
% here would replace vectorlink.pl's registration (SWI-Prolog's
% http_handler/3 replaces handlers for the same path), breaking POST and
% DELETE endpoints.
