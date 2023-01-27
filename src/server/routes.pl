:- module(routes,[]).

/** <module> HTTP API
 *
 * The Terminus DB API interface.
 *
 * A RESTful endpoint inventory for weilding the full capabilities of the
 * terminusDB.
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

%% TODO: this module should really only need things from core/api and maybe core/account.

:- reexport(core(util/syntax)).
:- use_module(core(util)).
:- use_module(core(triple)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(core(account)).
:- use_module(core(document)).
:- use_module(core(api/api_init)).

:- use_module(config(terminus_config)).

% prolog stack print
:- use_module(library(prolog_stack), [print_prolog_backtrace/2]).

% various prolog helper libraries
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(pcre)).
:- use_module(library(option)).
:- use_module(library(yall)).
:- use_module(library(zlib)).

% unit tests
:- use_module(library(plunit)).

% http libraries
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_stream)).
:- use_module(library(url)).
:- use_module(library(uri)).

:- use_module(library(base64)).


% multipart
:- use_module(library(http/http_multipart_plugin)).
:- use_module(library(http/mimepack)).

:- use_module(library(broadcast)).

% chunked
%:- use_module(library(http/http_header)).
%:- use_module(library(http/http_stream)).

% TUS
:- use_module(library(tus)).
:- (   file_upload_storage_path(Path)
   ->  set_tus_options([tus_storage_path(Path)])
   ;   true).

% Authentication library is only half used.
% and Auth is custom, not actually "Basic"
% Results should be cached!
:- use_module(library(http/http_authenticate)).

% Conditional loading of the JWT IO library...
:- if(config:jwt_enabled).
:- use_module(library(jwt_io)).
:- endif.

:- listen(http(Term), http_request_logger(Term)).

%%%%%%%%%%%%% API Paths %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Set base location
% We may want to allow this as a setting...
:- multifile http:location/3.
:- dynamic http:location/3.
http:location(root, '/', []).
http:location(api, '/api', []).

%%%%%%%%%%%%% Fallback Path %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler('/api', reply_404_not_found, [prefix]).

reply_404_not_found(Request) :-
    member(path(Path), Request),
    format(string(Msg),'Path not found: ~w', [Path]),
    reply_json(_{'api:status' : 'api:not_found',
                 'api:path' : Path,
                 'api:message' : Msg},
               [status(404)]).

%%%%%%%%%%%%%%%%%%%% Connection Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(.), cors_handler(Method, connect_handler),
                [method(Method),
                 methods([options,get])]).

/**
 * connect_handler(+Method,+Request:http_request) is det.
 */
/* NOTE: Need to return list of databases and access rights */
connect_handler(get, Request, System_DB, Auth) :-
    findall(Database,
            user_accessible_database(System_DB, Auth, Database),
            Databases),

    write_cors_headers(Request),
    reply_json(Databases, [width(0)]).


%%%%%%%%%%%%%%%%%%%% Info Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(log/Path), cors_handler(Method, log_handler(Path)),
                [method(Method),
                 methods([options,get])]).

log_handler(get, Path, Request, System_DB, Auth) :-
    (   memberchk(search(Search), Request)
    ->  true
    ;   Search = []),
    api_report_errors(
        log,
        Request,
        (   param_value_search_optional(Search, start, integer, 0, Start),
            param_value_search_optional(Search, count, integer, -1, Count),
            Options = opts{ start: Start, count: Count},
            api_log(System_DB, Auth, Path, Log, Options),
            cors_reply_json(Request, Log))).


%%%%%%%%%%%%%%%%%%%% Info Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(info), cors_handler(Method, info_handler),
                [method(Method),
                 methods([options,get])]).

info_handler(get, Request, System_DB, Auth) :-
    api_report_errors(
        info,
        Request,
        (   info(System_DB, Auth, Info),
            cors_reply_json(Request, _{'@type' : 'api:InfoResponse',
                                       'api:info' : Info,
                                       'api:status' : 'api:success'}))).


%%%%%%%%%%%%%%%%%%%% Ping Handler %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(ok), cors_handler(Method, ok_handler),
                [method(Method),
                 methods([options,get])]).

ok_handler(_Method, _Request, _System_DB, _Auth) :-
    format('Content-type: application/octets~n', []),
    format('Status: 200 OK~n~n', []).

%%%%%%%%%%%%%%%%%%%% Database Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(db), cors_handler(Method, db_handler, [add_payload(false)]),
                [method(Method),
                 methods([options,get])]).
:- http_handler(api(db/Org/DB), cors_handler(Method, db_handler(Org, DB), [add_payload(false)]),
                [method(Method),
                 methods([options,get,head,post,put,delete])]).

db_handler(get, Request, System_DB, Auth) :-
    (   memberchk(search(Search), Request)
    ->  true
    ;   Search = []),

    api_report_errors(
        check_db,
        Request,
        (   param_value_search_optional(Search, branches, boolean, false, Branches),
            param_value_search_optional(Search, verbose, boolean, false, Verbose),
            list_databases(System_DB, Auth, Database_Objects, _{ branches : Branches,
                                                                 verbose: Verbose}),
            cors_reply_json(Request, Database_Objects)
        )
    ).

db_handler(get, Organization, DB, Request, System_DB, Auth) :-
    (   memberchk(search(Search), Request)
    ->  true
    ;   Search = []),

    api_report_errors(
        check_db,
        Request,
        (   param_value_search_optional(Search, branches, boolean, false, Branches),
            param_value_search_optional(Search, verbose, boolean, false, Verbose),
            Options = _{ branches : Branches, verbose: Verbose },
            (   list_database(System_DB, Auth, Organization, DB, Database_Object, Options)
            ->  cors_reply_json(Request, Database_Object)
            ;   cors_reply_json(Request, _{'@type' : 'api:DbListErrorResponse',
                                           'api:status' : 'api:failure',
                                           'api:message' : "Database does not exist, or you do not have permission"},
                                [status(404)])
            )
        )
    ).
db_handler(head, Organization, DB, Request, System_DB, Auth) :-
    api_report_errors(
        check_db,
        Request,
        (   db_exists_api(System_DB, Auth, Organization, DB)
        ->  cors_reply_json(Request, _{'@type' : 'api:DbExistsResponse',
                                       'api:status' : 'api:success'})
        ;   cors_reply_json(Request, _{'@type' : 'api:DbExistsErrorResponse',
                                       'api:status' : 'api:failure',
                                       'api:message' : "Database does not exist, or you do not have permission"},
                            [status(404)])
        )
    ).
db_handler(post, Organization, DB, Request, System_DB, Auth) :-
    /* POST: Create database */
    api_report_errors(
        create_db,
        Request,
        (   http_read_json_required(json_dict(JSON), Request),

            param_value_json_optional(JSON, comment, string, "", Comment),
            param_value_json_required(JSON, label, non_empty_string, Label),

            param_value_json_optional(JSON, prefixes, object, _{}, Input_Prefixes),
            Default_Prefixes = _{ '@base' : "terminusdb:///data/",
                                  '@schema' : "terminusdb:///schema#" },
            put_dict(Input_Prefixes, Default_Prefixes, Prefixes),

            param_value_json_optional(JSON, public, boolean, false, Public),
            param_value_json_optional(JSON, schema, boolean, true, Schema),

            create_db(System_DB, Auth, Organization, DB, Label, Comment, Schema, Public, Prefixes, DB_Uri),

            cors_reply_json(Request, _{'@type' : 'api:DbCreateResponse',
                                       'api:database_uri' : DB_Uri,
                                       'api:status' : 'api:success'}))).
db_handler(delete,Organization,DB,Request, System_DB, Auth) :-
    /* DELETE: Delete database */
    api_report_errors(
        delete_db,
        Request,
        (   % Deprecating request body in delete
            (   http_read_json_semidet(json_dict(JSON), Request)
            ->  true
            ;   JSON = json{}),

            (   memberchk(search(Search), Request)
            ->  true
            ;   Search = []),

            param_value_search_or_json_optional(Search, JSON, force, boolean, false, Force_Delete),
            delete_db(System_DB, Auth, Organization, DB, Force_Delete),
            cors_reply_json(Request, _{'@type' : 'api:DbDeleteResponse',
                                       'api:status' : 'api:success'}))).
db_handler(put, Organization, DB, Request, System_DB, Auth) :-
    /* PUT: Update database */
    api_report_errors(
        update_db,
        Request,
        (   http_read_json_required(json_dict(JSON), Request),
            api_db_update(System_DB, Organization, DB, Auth, commit_info{
                                                                 author : 'REST API',
                                                                 message : 'Updating Database Record'
                                                             }, JSON),
            cors_reply_json(Request, _{'@type' : 'api:DbUpdatedResponse',
                                       'api:status' : 'api:success'})
        )
    ).

:- begin_tests(db_endpoint).

:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).


test(db_create_unauthorized_errors, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    add_user("TERMINUSQA",some('password'),_User_ID),
    atomic_list_concat([Server, '/api/db/admin/TEST_DB'], URI),
    Doc = _{ prefixes : _{ doc : "https://terminushub.com/document",
                           scm : "https://terminushub.com/schema"},
             comment : "A quality assurance test",
             label : "A label"
           },
    http_post(URI, json(Doc),
              Result, [json_object(dict),
                       authorization(basic("TERMINUSQA", "password")),
                       status_code(Status)]),

    Status = 403,
    _{'api:status' : "api:forbidden"} :< Result.


test(db_force_delete_unfinalized_system_only, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    create_context(system_descriptor{}, Context),
    with_transaction(Context,
                     insert_db_object(Context, "admin", "foo", "testdb", "test db", _),
                     _),
    database_exists("admin", "foo"),

    atomic_list_concat([Server, '/api/db/admin/foo'], URI),
    admin_pass(Key),
    http_get(URI,
             Delete_In,
             [method(delete),
              post(json(_{force:true})),
              json_object(dict),
              authorization(basic(admin, Key))]),

    _{'api:status' : "api:success"} :< Delete_In,

    \+ database_exists("admin", "foo").

test(db_force_delete_unfinalized_system_and_label, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    super_user_authority(Auth),

    organization_database_name("admin","foo",Label),
    triple_store(Store),

    create_db_unfinalized(system_descriptor{},
                          Auth, "admin", "foo",
                          "dblabel", "db comment", false, true,
                         _{'@base' : "http://foo/",
                          '@schema' : "http://foo/s#"},
                         _),
    database_exists("admin", "foo"),
    safe_open_named_graph(Store, Label, _),

    atomic_list_concat([Server, '/api/db/admin/foo'], URI),
    admin_pass(Key),
    http_get(URI,
             Delete_In,
             [method(delete),
              post(json(_{force:true})),
              json_object(dict),
              authorization(basic(admin, Key))]),

    _{'api:status' : "api:success"} :< Delete_In,

    \+ database_exists("admin", "foo"),
    \+ safe_open_named_graph(Store, Label, _).

:- end_tests(db_endpoint).

%%%%%%%%%%%%%%%%%%%% Triples Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(triples/Path), cors_handler(Method, triples_handler(Path)),
                [method(Method),
                 prefix,
                 time_limit(infinite),
                 methods([options,get,post,put])]).

/*
 * triples_handler(Mode,DB,Request) is det.
 *
 * Get or update a graph with turtle.
 */
triples_handler(get,Path,Request, System_DB, Auth) :-
    (   get_param('format', Request, Format_Atom)
    ->  atom_string(Format_Atom,Format)
    ;   Format = "turtle"
    ),

    (   memberchk(accept(Accepted), Request)
    ->  true
    ;   Accepted = []),

    api_report_errors(
        triples,
        Request,
        (   graph_dump(System_DB, Auth, Path, Format, String),
            % Accept: */*
            % Somehow media(_330/_332,[],1.0,[]), passes this
            (   [media(Type/SubType,_, _, _)] = Accepted,
                var(Type),
                var(SubType)
            ->  cors_reply_json(Request, String)
            ;   memberchk(media(text/turtle,_,_,_), Accepted)
            ->  format('Content-type: text/turtle~n', []),
                format('Status: 200 OK~n~n', []),
                format(String, [])
            ;   cors_reply_json(Request, String)))).
triples_handler(post,Path,Request, System_DB, Auth) :-
    get_payload(Triples_Document,Request),
    do_or_die(_{ turtle : TTL,
                 commit_info : Commit_Info } :< Triples_Document,
              error(bad_api_document(Triples_Document,[turtle,commit_info]),_)),

    api_report_errors(
        triples,
        Request,
        (   graph_update(System_DB, Auth, Path, Commit_Info, "turtle", TTL),
            cors_reply_json(Request, _{'@type' : 'api:TriplesUpdateResponse',
                                       'api:status' : "api:success"}))).
triples_handler(put,Path,Request, System_DB, Auth) :-
    get_payload(Triples_Document,Request),
    do_or_die(_{ turtle : TTL,
                 commit_info : Commit_Info } :< Triples_Document,
              error(bad_api_document(Triples_Document,[turtle,commit_info]),_)),

    api_report_errors(
        triples,
        Request,
        (   graph_insert(System_DB, Auth, Path, Commit_Info, "turtle", TTL),
            cors_reply_json(Request, _{'@type' : 'api:TriplesInsertResponse',
                                       'api:status' : "api:success"}))).


%%%%%%%%%%%%%%%%%%%% Document Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(document/Path), cors_handler(Method, document_handler(Path), [add_payload(false)]),
                [method(Method),
                 prefix,
                 chunked,
                 time_limit(infinite),
                 methods([head,options,post,delete,get,put])]).


document_handler(head, Path, Request, System_DB, Auth) :-
    api_report_errors(
        access_documents,
        Request,
        (   (   http_read_json_semidet(json_dict(JSON), Request)
            ->  true
            ;   JSON = json{}),

            (   memberchk(search(Search), Request)
            ->  true
            ;   Search = []),


            param_value_search_or_json_optional(Search, JSON, graph_type, graph, instance, Graph_Type),
            read_data_version_header(Request, Requested_Data_Version),

            api_can_read_document(System_DB, Auth, Path, Graph_Type, Requested_Data_Version, Actual_Data_Version),
            write_data_version_header(Actual_Data_Version),
            format('Status: 200 OK~n~n', [])
        )).
document_handler(get, Path, Request, System_DB, Auth) :-
    api_report_errors(
        get_documents,
        Request,
        (   (   http_read_json_semidet(json_dict(JSON), Request)
            ->  true
            ;   JSON = json{}),

            (   memberchk(search(Search), Request)
            ->  true
            ;   Search = []),

            param_value_search_or_json_optional(Search, JSON, graph_type, graph, instance, Graph_Type),
            param_value_search_or_json_optional(Search, JSON, skip, nonnegative_integer, 0, Skip),
            param_value_search_or_json_optional(Search, JSON, count, nonnegative_integer, unlimited, Count),
            param_value_search_or_json_optional(Search, JSON, minimized, boolean, true, Minimized),
            param_value_search_or_json_optional(Search, JSON, as_list, boolean, false, As_List),
            param_value_search_or_json_optional(Search, JSON, unfold, boolean, true, Unfold),
            param_value_search_or_json_optional(Search, JSON, id, non_empty_atom, _, Id),
            param_value_search_or_json_optional(Search, JSON, type, non_empty_atom, _, Type),

            % Use new compress_ids but still support old prefixed.
            % See https://github.com/terminusdb/terminusdb/issues/802
            param_value_search_or_json_optional(Search, JSON, prefixed, boolean, true, Prefixed),
            param_value_search_or_json_optional(Search, JSON, compress_ids, boolean, Prefixed, Compress_Ids),

            param_value_json_optional(JSON, query, object, _, Query),

            read_data_version_header(Request, Requested_Data_Version),

            Config = config{
                         skip: Skip,
                         count: Count,
                         as_list: As_List,
                         compress: Compress_Ids,
                         unfold: Unfold,
                         minimized: Minimized
                     },

            api_read_document_selector(
                System_DB, Auth, Path, Graph_Type,
                Id, Type, Query, Config,
                Requested_Data_Version, Actual_Data_Version,
                cors_json_stream_write_headers_(Request, Actual_Data_Version)
            )
        )).

document_handler(post, Path, Request, System_DB, Auth) :-
    memberchk(x_http_method_override('GET'), Request), % Is this not redundant?
    !,
    document_handler(get, Path, Request, System_DB, Auth).
document_handler(post, Path, Request, System_DB, Auth) :-
    api_report_errors(
        insert_documents,
        Request,
        (   http_read_json_required(stream(Stream), Request),

            (   memberchk(search(Search), Request)
            ->  true
            ;   Search = []),

            param_value_search_author(Search, Author),
            param_value_search_message(Search, Message),
            param_value_search_graph_type(Search, Graph_Type),
            param_value_search_optional(Search, full_replace, boolean, false, Full_Replace),
            param_value_search_optional(Search, raw_json, boolean, false, Raw_JSON),

            read_data_version_header(Request, Requested_Data_Version),

            Options = options{
                          graph_type : Graph_Type,
                          author : Author,
                          message : Message,
                          full_replace : Full_Replace,
                          raw_json : Raw_JSON
                      },
            api_insert_documents(System_DB, Auth, Path, Stream, Requested_Data_Version, New_Data_Version, Ids, Options),

            write_cors_headers(Request),
            write_data_version_header(New_Data_Version),
            reply_json(Ids, [width(0)]),
            nl
        )).

document_handler(delete, Path, Request, System_DB, Auth) :-
    api_report_errors(
        delete_documents,
        Request,
        (
            (   memberchk(search(Search), Request)
            ->  true
            ;   Search = []),

            param_value_search_author(Search, Author),
            param_value_search_message(Search, Message),
            param_value_search_graph_type(Search, Graph_Type),
            param_value_search_optional(Search, nuke, boolean, false, Nuke),
            param_value_search_optional(Search, id, non_empty_atom, _, Id),

            read_data_version_header(Request, Requested_Data_Version),
            Options = options{
                          author : Author,
                          message : Message,
                          graph_type : Graph_Type
                      },

            (   Nuke = true
            ->  api_nuke_documents(System_DB, Auth, Path, Requested_Data_Version, New_Data_Version, Options)
            ;   ground(Id)
            ->  api_delete_document(System_DB, Auth, Path, Id, Requested_Data_Version, New_Data_Version, Options)
            ;   http_read_json_semidet(stream(Stream), Request)
            ->  api_delete_documents(System_DB, Auth, Path, Stream, Requested_Data_Version, New_Data_Version, _Ids, Options)
            ;   throw(error(missing_targets, _))
            ),

            write_cors_headers(Request),
            write_data_version_header(New_Data_Version),
            nl,nl
        )).

document_handler(put, Path, Request, System_DB, Auth) :-
    api_report_errors(
        replace_documents,
        Request,
        (   http_read_json_required(stream(Stream), Request),

            (   memberchk(search(Search), Request)
            ->  true
            ;   Search = []),

            param_value_search_author(Search, Author),
            param_value_search_message(Search, Message),
            param_value_search_graph_type(Search, Graph_Type),
            param_value_search_optional(Search, create, boolean, false, Create),
            param_value_search_optional(Search, raw_json, boolean, false, Raw_JSON),

            read_data_version_header(Request, Requested_Data_Version),
            Options = options{
                author : Author,
                message : Message,
                graph_type : Graph_Type,
                create : Create,
                raw_json : Raw_JSON
            },
            api_replace_documents(System_DB, Auth, Path, Stream, Requested_Data_Version, New_Data_Version, Ids, Options),

            write_cors_headers(Request),
            write_data_version_header(New_Data_Version),
            reply_json(Ids, [width(0)]),
            nl
        )).

%%%%%%%%%%%%%%%%%%%% Frame Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(schema/Path), cors_handler(Method, frame_handler(Path), [add_payload(false)]),
                [method(Method),
                 prefix,
                 methods([options,get,post])]).

/**
 * frame_handler(+Mode, +DB, +Class_ID, +Request:http_request) is det.
 *
 * Establishes frame responses
 */
frame_handler(get, Path, Request, System_DB, Auth) :-
    % TODO This possibly throws a json error, which gets reinterpreted
    % as a schema check failure for some reason. gotta fix that.
    api_report_errors(
        frame,
        Request,
        (   (   http_read_json_semidet(json_dict(JSON), Request)
            ->  true
            ;   JSON = _{}),
            (   memberchk(search(Search), Request)
            ->  true
            ;   Search = []),

            param_value_search_or_json_optional(Search, JSON, type, text, all, Class_URI),
            param_value_search_or_json_optional(Search, JSON, compress_ids, boolean, true, Compress_Ids),
            param_value_search_or_json_optional(Search, JSON, expand_abstract, boolean, true, Expand_Abstract),
            (   Class_URI = all
            ->  Class = all
            ;   Class = uri(Class_URI)
            ),

            Options =
            _{
                compress_ids: Compress_Ids,
                expand_abstract: Expand_Abstract
            },
            api_class_frame(System_DB, Auth, Path, Class, Frame, Options),
            write_cors_headers(Request),
            reply_json(Frame, [width(0)])
        )
    ).

%%%%%%%%%%%%%%%%%%%% WOQL Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
%
:- http_handler(api(woql), cors_handler(Method, woql_handler, [add_payload(false)]),
                [method(Method),
                 time_limit(infinite),
                 methods([options,post])]).
:- http_handler(api(woql/Path), cors_handler(Method, woql_handler(Path), [add_payload(false)]),
                [method(Method),
                 prefix,
                 time_limit(infinite),
                 methods([options,post])]).

/**
 * woql_handler(+Method:atom, +Request:http_request) is det.
 *
 * Open WOQL with no defined database
 *
 * NOTE: This is not obtaining appropriate cors response data
 * from terminus database on spartacus.
 */
woql_handler(post, Request, System_DB, Auth) :-
    woql_handler_helper(Request, System_DB, Auth, none).

woql_handler(post, Path, Request, System_DB, Auth) :-
    woql_handler_helper(Request, System_DB, Auth, some(Path)).

woql_handler_helper(Request, System_DB, Auth, Path_Option) :-
    api_report_errors(
        woql,
        Request,
        (   (   http_read_json_semidet(json_dict(JSON), Request)
            ->  Files = []
            ;   http_read_multipart_semidet(Request, Form_Data),
                http_read_multipart_json_semidet(Form_Data, JSON, Other_Form_Data),
                collect_multipart_files(Other_Form_Data, Files)
            ->  true
            ;   throw(error(missing_content_type("application/json or multipart/form-data"), _))
            ),

            param_value_json_required(JSON, query, object, Query),
            param_value_json_optional(JSON, commit_info, object, commit_info{}, Commit_Info),
            param_value_json_optional(JSON, all_witnesses, boolean, false, All_Witnesses),

            read_data_version_header(Request, Requested_Data_Version),

            woql_query_json(System_DB, Auth, Path_Option, json_query(Query), Commit_Info, Files, All_Witnesses, Requested_Data_Version, New_Data_Version, _Context, Response),

            write_cors_headers(Request),
            write_data_version_header(New_Data_Version),
            reply_json_dict(Response, [width(0)])
        )).


%%%%%%%%%%%%%%%%%%%% Clone Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(clone/Organization/DB), cors_handler(Method, clone_handler(Organization, DB)),
                [method(Method),
                 methods([options,post])]).

clone_handler(post, Organization, DB, Request, System_DB, Auth) :-

    do_or_die(
        (get_payload(Database_Document,Request),
         _{ comment : Comment,
            label : Label,
            remote_url: Remote_URL} :< Database_Document),
        error(bad_api_document(Database_Document,[comment,label,remote_url]),_)),

    (   _{ public : Public } :< Database_Document
    ->  true
    ;   Public = false),

    api_report_errors(
        clone,
        Request,
        (   do_or_die(
                request_remote_authorization(Request, Authorization),
                error(no_remote_authorization,_)),

            clone(System_DB, Auth, Organization,DB,Label,Comment,Public,Remote_URL,authorized_fetch(Authorization),_Meta_Data),
            write_cors_headers(Request),
            reply_json_dict(
                _{'@type' : 'api:CloneResponse',
                  'api:status' : 'api:success'}, [width(0)])
        )).

:- begin_tests(clone_endpoint).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).
:- use_module(library(base64)).

test(clone_local, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    add_user("TERMINUSQA1",some('password1'),_User_ID1),
    add_user("TERMINUSQA2",some('password2'),_User_ID2),
    create_db_without_schema("TERMINUSQA1", "foo"),
    resolve_absolute_string_descriptor("TERMINUSQA1/foo", Foo_Descriptor),
    create_context(Foo_Descriptor, commit_info{author:"test",message:"test"}, Foo_Context),
    with_transaction(Foo_Context,
                     ask(Foo_Context,
                         insert(a,b,c)),
                     _),

    atomic_list_concat([Server, '/api/clone/TERMINUSQA2/bar'], URL),
    atomic_list_concat([Server, '/TERMINUSQA1/foo'], Remote_URL),
    base64("TERMINUSQA1:password1", Base64_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Auth]),

    http_post(URL,
              json(_{comment: "hai hello",
                     label: "bar",
                     remote_url: Remote_URL}),

              JSON,
              [json_object(dict),authorization(basic('TERMINUSQA2','password2')),
               request_header('Authorization-Remote'=Authorization_Remote)]),

    * json_write_dict(current_output, JSON, []),

    _{
        'api:status' : "api:success"
    } :< JSON,

    resolve_absolute_string_descriptor("TERMINUSQA2/bar", Bar_Descriptor),
    once(ask(Bar_Descriptor,
             t(a,b,c))),

    true.

test(clone_remote, [
         setup(
             (   setup_temp_unattached_server(State_1,Store_1,Server_1),
                 setup_temp_unattached_server(State_2,Store_2,Server_2))),
         cleanup(
             (
                 teardown_temp_unattached_server(State_1),
                 teardown_temp_unattached_server(State_2)))
     ])
:-
    with_triple_store(
        Store_1,
        (   add_user("TERMINUSQA1",some('password1'),_User_ID1),
            create_public_db_without_schema("TERMINUSQA1", "foo"),
            resolve_absolute_string_descriptor("TERMINUSQA1/foo", Foo_Descriptor),
            create_context(Foo_Descriptor, commit_info{author:"test",message:"test"}, Foo_Context),
            with_transaction(Foo_Context,
                             ask(Foo_Context,
                                 insert(a,b,c)),
                             _)
        )
    ),

    with_triple_store(
        Store_2,
        (   add_user("TERMINUSQA2",some('password2'),_User_ID2)
        )
    ),

    atomic_list_concat([Server_2, '/api/clone/TERMINUSQA2/bar'], URL),
    atomic_list_concat([Server_1, '/TERMINUSQA1/foo'], Remote_URL),
    base64("TERMINUSQA1:password1", Base64_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Auth]),

    http_post(URL,
              json(_{comment: "hai hello",
                     label: "bar",
                     remote_url: Remote_URL}),

              JSON,
              [json_object(dict),authorization(basic('TERMINUSQA2','password2')),
               request_header('Authorization-Remote'=Authorization_Remote)]),

    * json_write_dict(current_output, JSON, []),

    _{
        'api:status' : "api:success"
    } :< JSON,

    with_triple_store(
        Store_2,
        (   resolve_absolute_string_descriptor("TERMINUSQA2/bar", Bar_Descriptor),
            once(ask(Bar_Descriptor,
                     t(a,b,c)))
        )
    ).


:- end_tests(clone_endpoint).

%%%%%%%%%%%%%%%%%%%% Fetch Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(fetch/Path), cors_handler(Method, fetch_handler(Path)),
                [method(Method),
                 prefix,
                 time_limit(infinite),
                 methods([options,post])]).

fetch_handler(post,Path,Request, System_DB, Auth) :-
    api_report_errors(
        fetch,
        Request,
        (   do_or_die(
                request_remote_authorization(Request, Authorization),
                error(no_remote_authorization_for_fetch,_)),

            remote_fetch(System_DB, Auth, Path, authorized_fetch(Authorization),
                         New_Head_Layer_Id, Head_Has_Updated),
            write_cors_headers(Request),
            reply_json_dict(
                _{'@type' : 'api:FetchRequest',
                  'api:status' : 'api:success',
                  'api:head_has_changed' : Head_Has_Updated,
                  'api:head' : New_Head_Layer_Id}, [width(0)]))).

:- begin_tests(fetch_endpoint).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(fetch_first_time, [
         setup(
             (   setup_temp_unattached_server(State_1,Store_1,Server_1),
                 setup_temp_unattached_server(State_2,Store_2,Server_2))),
         cleanup(
             (
                 teardown_temp_unattached_server(State_1),
                 teardown_temp_unattached_server(State_2)))
     ])
:-

    with_triple_store(
        Store_1,
        (
            add_user("TERMINUSQA1",some('password1'),_User_ID1),
            create_public_db_without_schema("TERMINUSQA1", "foo"),
            resolve_absolute_string_descriptor("TERMINUSQA1/foo", Foo_Descriptor),
            create_context(Foo_Descriptor, commit_info{author:"test",message:"test"}, Foo_Context),
            with_transaction(Foo_Context,
                             ask(Foo_Context,
                                 insert(a,b,c)),
                             _),
            get_dict(repository_descriptor, Foo_Descriptor, Foo_Repository_Desc),
            get_dict(database_descriptor, Foo_Repository_Desc, Foo_Database_Desc),
            repository_head(Foo_Database_Desc,"local",Head)
        )
    ),

    with_triple_store(
        Store_2,
        (
            add_user("TERMINUSQA2",some('password2'),_User_ID2),
            create_public_db_without_schema("TERMINUSQA2", "bar"),
            resolve_absolute_string_descriptor("TERMINUSQA2/bar", Bar_Descriptor),
            get_dict(repository_descriptor, Bar_Descriptor, Bar_Repository_Desc),
            get_dict(database_descriptor, Bar_Repository_Desc, Bar_Database_Desc),
            create_context(Bar_Database_Desc, commit_info{author:"test",message:"test"}, Bar_Database_Context),
            atomic_list_concat([Server_1, '/TERMINUSQA1/foo'], Remote_URL),
            with_transaction(
                Bar_Database_Context,
                insert_remote_repository(Bar_Database_Context, "origin", Remote_URL, _),
                _)
        )
    ),
    atomic_list_concat([Server_2, '/api/fetch/TERMINUSQA2/bar/origin/_commits'], URL),
    base64("TERMINUSQA1:password1", Base64_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Auth]),
    http_post(URL,
              json(_{}),
              JSON,
              [json_object(dict),authorization(basic('TERMINUSQA2','password2')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(200)]),

    _{ '@type' : "api:FetchRequest",
       'api:head_has_changed' : true,
       'api:head': Head,
       'api:status' : "api:success"} :< JSON,

    with_triple_store(
        Store_2,
        (   resolve_absolute_string_descriptor("TERMINUSQA2/bar/origin", Bar_Descriptor_Origin),
            once(ask(Bar_Descriptor_Origin,
                     t(a,b,c))),
            repository_head(Bar_Database_Desc, "origin", Head)
        )
    ).

test(fetch_second_time_no_change, [
         setup(
             (   setup_temp_unattached_server(State_1,Store_1,Server_1),
                 setup_temp_unattached_server(State_2,Store_2,Server_2))),
         cleanup(
             (
                 teardown_temp_unattached_server(State_1),
                 teardown_temp_unattached_server(State_2)))
     ])
:-

    with_triple_store(
        Store_1,
        (
            add_user("TERMINUSQA1",some('password1'),_User_ID1),
            create_public_db_without_schema("TERMINUSQA1", "foo"),
            resolve_absolute_string_descriptor("TERMINUSQA1/foo", Foo_Descriptor),
            create_context(Foo_Descriptor, commit_info{author:"test",message:"test"}, Foo_Context),
            with_transaction(Foo_Context,
                             ask(Foo_Context,
                                 insert(a,b,c)),
                             _),
            get_dict(repository_descriptor, Foo_Descriptor, Foo_Repository_Desc),
            get_dict(database_descriptor, Foo_Repository_Desc, Foo_Database_Desc),
            repository_head(Foo_Database_Desc,"local",Head)
        )
    ),

    with_triple_store(
        Store_2,
        (
            add_user("TERMINUSQA2",some('password2'),_User_ID2),
            create_public_db_without_schema("TERMINUSQA2", "bar"),
            resolve_absolute_string_descriptor("TERMINUSQA2/bar", Bar_Descriptor),
            get_dict(repository_descriptor, Bar_Descriptor, Bar_Repository_Desc),
            get_dict(database_descriptor, Bar_Repository_Desc, Bar_Database_Desc),
            create_context(Bar_Database_Desc, commit_info{author:"test",message:"test"}, Bar_Database_Context),
            atomic_list_concat([Server_1, '/TERMINUSQA1/foo'], Remote_URL),
            with_transaction(
                Bar_Database_Context,
                insert_remote_repository(Bar_Database_Context, "origin", Remote_URL, _),
                _)
        )
    ),
    atomic_list_concat([Server_2, '/api/fetch/TERMINUSQA2/bar/origin/_commits'], URL),
    base64("TERMINUSQA1:password1", Base64_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Auth]),
    http_post(URL,
              json(_{}),
              JSON1,
              [json_object(dict),authorization(basic('TERMINUSQA2','password2')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(200)]),


    _{ '@type' : "api:FetchRequest",
       'api:head_has_changed' : true,
       'api:head': Head,
       'api:status' : "api:success"} :< JSON1,

    http_post(URL,
              json(_{}),
              JSON2,
              [json_object(dict),authorization(basic('TERMINUSQA2','password2')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(200)]),

    _{ '@type' : "api:FetchRequest",
       'api:head_has_changed' : false,
       'api:head': Head,
       'api:status' : "api:success"} :< JSON2,


    with_triple_store(
        Store_2,
        (
            repository_head(Bar_Database_Desc, "origin", Head)
        )
    ).

test(fetch_second_time_with_change, [
         setup(
             (   setup_temp_unattached_server(State_1,Store_1,Server_1),
                 setup_temp_unattached_server(State_2,Store_2,Server_2))),
         cleanup(
             (
                 teardown_temp_unattached_server(State_1),
                 teardown_temp_unattached_server(State_2)))
     ])
:-

    with_triple_store(
        Store_1,
        (
            add_user("TERMINUSQA1",some('password1'),_User_ID1),
            create_public_db_without_schema("TERMINUSQA1", "foo"),
            resolve_absolute_string_descriptor("TERMINUSQA1/foo", Foo_Descriptor),
            create_context(Foo_Descriptor, commit_info{author:"test",message:"test"}, Foo_Context),
            with_transaction(Foo_Context,
                             ask(Foo_Context,
                                 insert(a,b,c)),
                             _),
            get_dict(repository_descriptor, Foo_Descriptor, Foo_Repository_Desc),
            get_dict(database_descriptor, Foo_Repository_Desc, Foo_Database_Desc),
            repository_head(Foo_Database_Desc,"local",Head)
        )
    ),

    with_triple_store(
        Store_2,
        (
            add_user("TERMINUSQA2",some('password2'),_User_ID2),
            create_public_db_without_schema("TERMINUSQA2", "bar"),
            resolve_absolute_string_descriptor("TERMINUSQA2/bar", Bar_Descriptor),
            get_dict(repository_descriptor, Bar_Descriptor, Bar_Repository_Desc),
            get_dict(database_descriptor, Bar_Repository_Desc, Bar_Database_Desc),
            create_context(Bar_Database_Desc, commit_info{author:"test",message:"test"}, Bar_Database_Context),
            atomic_list_concat([Server_1, '/TERMINUSQA1/foo'], Remote_URL),
            with_transaction(
                Bar_Database_Context,
                insert_remote_repository(Bar_Database_Context, "origin", Remote_URL, _),
                _)
        )
    ),
    atomic_list_concat([Server_2, '/api/fetch/TERMINUSQA2/bar/origin/_commits'], URL),
    base64("TERMINUSQA1:password1", Base64_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Auth]),
    http_post(URL,
              json(_{}),
              JSON1,
              [json_object(dict),authorization(basic('TERMINUSQA2','password2')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(200)]),


    _{ '@type' : "api:FetchRequest",
       'api:head_has_changed' : true,
       'api:head': Head,
       'api:status' : "api:success"} :< JSON1,

    with_triple_store(
        Store_1,
        (
            create_context(Foo_Descriptor, commit_info{author:"test",message:"test"}, Foo_Context_Extra1),
            with_transaction(Foo_Context_Extra1,
                             ask(Foo_Context_Extra1,
                                 insert(d,e,f)),
                             _),
            create_context(Foo_Descriptor, commit_info{author:"test",message:"test"}, Foo_Context_Extra2),
            with_transaction(Foo_Context_Extra2,
                             ask(Foo_Context_Extra2,
                                 insert(g,h,i)),
                             _),

            repository_head(Foo_Database_Desc,"local",New_Head)
        )),


    http_post(URL,
              json(_{}),
              JSON2,
              [json_object(dict),authorization(basic('TERMINUSQA2','password2')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(200)]),

    _{ '@type' : "api:FetchRequest",
       'api:head_has_changed' : true,
       'api:head': New_Head,
       'api:status' : "api:success"} :< JSON2,


    with_triple_store(
        Store_2,
        (
            repository_head(Bar_Database_Desc, "origin", New_Head)
        )
    ).

:- end_tests(fetch_endpoint).


%%%%%%%%%%%%%%%%%%%% Rebase Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(rebase/Path), cors_handler(Method, rebase_handler(Path)),
                [method(Method),
                 prefix,
                 time_limit(infinite),
                 methods([options,post])]).

rebase_handler(post, Path, Request, System_DB, Auth) :-
    check_content_type_json(Request),
    get_payload(Document,Request),
    do_or_die(
        (_{ author : Author,
            rebase_from : Their_Path } :< Document),
        error(bad_api_document(Document,[author,rebase_from]),_)),

    api_report_errors(
        rebase,
        Request,
        (   Strategy_Map = [],
            rebase_on_branch(System_DB, Auth, Path, Their_Path, Author, Strategy_Map, Common_Commit_ID_Option, Forwarded_Commits, Reports),

            Incomplete_Reply = _{ '@type' : "api:RebaseResponse",
                                  'api:status' : "api:success",
                                  'api:forwarded_commits' : Forwarded_Commits,
                                  'api:rebase_report': Reports
                                },

            (   Common_Commit_ID_Option = some(Common_Commit_ID)
            ->  put_dict(_{ 'api:common_commit_id' : Common_Commit_ID},
                         Incomplete_Reply,
                         Reply)
            ;   Reply = Incomplete_Reply),
            cors_reply_json(Request, Reply, [status(200), width(0)]))).

:- begin_tests(rebase_endpoint).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(rebase_divergent_history, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ])
:-
    add_user("TERMINUSQA",some('password'),User_ID),
    create_db_without_schema("TERMINUSQA", "foo"),

    Master_Path = "TERMINUSQA/foo",
    resolve_absolute_string_descriptor(Master_Path, Master_Descriptor),
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit a"}, Master_Context1),
    with_transaction(Master_Context1,
                     ask(Master_Context1,
                         insert(a,b,c)),
                    _),

    Second_Path = "TERMINUSQA/foo/local/branch/second",
    branch_create(system_descriptor{}, User_ID, Second_Path, branch(Master_Path), _),
    resolve_absolute_string_descriptor(Second_Path, Second_Descriptor),

    create_context(Second_Descriptor, commit_info{author:"test",message:"commit b"}, Second_Context1),
    with_transaction(Second_Context1,
                     ask(Second_Context1,
                         (   insert(d,e,f),
                             delete(a,b,c))),
                     _),

    create_context(Second_Descriptor, commit_info{author:"test",message:"commit c"}, Second_Context2),
    with_transaction(Second_Context2,
                     ask(Second_Context2,
                         insert(g,h,i)),
                     _),

    % we're also doing a commit on the original branch, to create a divergent history
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit d"}, Master_Context2),
    with_transaction(Master_Context2,
                     ask(Master_Context2,
                         insert(j,k,l)),
                     _),
    atomic_list_concat([Server, '/api/rebase/TERMINUSQA/foo'], URI),
    http_post(URI,
              json(_{rebase_from: 'TERMINUSQA/foo/local/branch/second',
                     author : "Gavsky"}),
              JSON,
              [json_object(dict),
               authorization(basic('TERMINUSQA','password')),
               status_code(_Status_Code)]),

    * json_write_dict(current_output, JSON, []),

    _{  '@type' : "api:RebaseResponse",
        'api:forwarded_commits' : [_Thing, _Another_Thing ],
        'api:common_commit_id' : _Common_Something,
        'api:rebase_report' : _Reports,
        'api:status' : "api:success"
    } :< JSON,

    Repository_Descriptor = Master_Descriptor.repository_descriptor,
    branch_head_commit(Repository_Descriptor, "main", Commit_Uri),
    commit_uri_to_history_commit_ids(Repository_Descriptor, Commit_Uri, [Commit_A, Commit_B, Commit_C, Commit_D]),

    commit_id_to_metadata(Repository_Descriptor, Commit_A, "test", "commit a", _),
    commit_id_to_metadata(Repository_Descriptor, Commit_B, "test", "commit b", _),
    commit_id_to_metadata(Repository_Descriptor, Commit_C, "test", "commit c", _),
    commit_id_to_metadata(Repository_Descriptor, Commit_D, "Gavsky", "commit d", _).
:- end_tests(rebase_endpoint).

%%%%%%%%%%%%%%%%%%%% Pack Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(pack/Path), cors_handler(Method, pack_handler(Path)),
                [method(Method),
                 time_limit(infinite),
                 chunked,
                 methods([options,post])]).

pack_handler(post,Path,Request, System_DB, Auth) :-
    get_payload(Document,Request),

    (   _{ repository_head : Layer_ID } :< Document
    ->  Repo_Head_Option = some(Layer_ID)
    ;   Repo_Head_Option = none),

    api_report_errors(
        pack,
        Request,
        pack(System_DB, Auth,
             Path, Repo_Head_Option, Payload_Option)),

    (   Payload_Option = some(Payload)
    ->  format('Content-type: application/octets~n', []),
        format('Status: 200 OK~n~n', []),
        format('~s', [Payload])
    ;   format('Content-type: application/octets~n', []),
        format('Status: 204 No Response~n~n', [])
    ).

% Currently just sending binary around...
:- begin_tests(pack_endpoint).
:- use_module(core(util/test_utils)).
%:- use_module(core(transaction)).
%:- use_module(core(api)).
:- use_module(library(http/http_open)).
:- use_module(library(terminus_store)).

test(pack_stuff, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    add_user('_a_test_user_',some('password'),_User_ID),
    create_db_without_schema('_a_test_user_',foo),

    resolve_absolute_string_descriptor('_a_test_user_/foo', Descriptor),

    % First commit
    create_context(Descriptor, commit_info{author:"user",message:"commit a"}, Master_Context1),
    with_transaction(Master_Context1,
                     ask(Master_Context1,
                         insert(a,b,c)),
                     _),

    % Second commit
    create_context(Descriptor, commit_info{author:"user",message:"commit b"}, Master_Context2),
    % Before updating, grab the repository head layer_ID
    [Branch_Transaction] = (Master_Context2.transaction_objects),
    Repository = (Branch_Transaction.parent),
    repository_head_layerid(Repository,Repository_Head_Layer_ID),

    with_transaction(Master_Context2,
                     ask(Master_Context2,
                         (   insert(d,e,f),
                             delete(a,b,c))),
                     _),


    atomic_list_concat([Server, '/api/pack/_a_test_user_/foo'], URI),

    Document = _{ repository_head : Repository_Head_Layer_ID },
    http_post(URI,
              json(Document),
              Data,
              [authorization(basic('_a_test_user_','password'))]),

    payload_repository_head_and_pack(Data, Head, Pack),

    % Check pack validity
    create_context(Descriptor, commit_info{author:"user",message:"commit b"}, New_Head_Context),
    % grab the repository head layer_ID and new graph layer_ID
    [New_Head_Transaction] = (New_Head_Context.transaction_objects),
    New_Head_Repository = (New_Head_Transaction.parent),
    repository_head_layerid(New_Head_Repository,New_Repository_Head_Layer_Id),
    [Instance_Graph] = (New_Head_Transaction.instance_objects),
    Layer = (Instance_Graph.read),
    layer_to_id(Layer,Layer_Id),

    pack_layerids_and_parents(Pack,Layerids_and_Parents),

    memberchk(New_Repository_Head_Layer_Id-_,Layerids_and_Parents),
    memberchk(Layer_Id-_,Layerids_and_Parents),
    memberchk(Head-_,Layerids_and_Parents),

    Head = New_Repository_Head_Layer_Id.


test(pack_nothing, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-
    add_user('_a_test_user_',some('password'),_User_ID),
    create_db_without_schema('_a_test_user_','foo'),

    resolve_absolute_string_descriptor('_a_test_user_/foo', Descriptor),
    Repository_Descriptor = (Descriptor.repository_descriptor),
    open_descriptor(Repository_Descriptor, Repo_Transaction),
    repository_head_layerid(Repo_Transaction, Repository_Head_Layer_ID),

    Document = _{ repository_head : Repository_Head_Layer_ID },
    atomic_list_concat([Server, '/api/pack/_a_test_user_/foo'], URI),
    http_post(URI,
              json(Document),
              _Data,
              [authorization(basic('_a_test_user_','password')),status_code(Status)]),
    Status = 204.

:- end_tests(pack_endpoint).

%%%%%%%%%%%%%%%%%%%% Unpack Handlers %%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(unpack/Path), cors_handler(Method, unpack_handler(Path)),
                [method(Method),
                 chunked,
                 time_limit(infinite),
                 methods([options,post])]).

unpack_handler(post, Path, Request, System_DB, Auth) :-

    % This really should use API versioning
    do_or_die(
        (   get_payload(Document, Request),
            (   (   is_dict(Document),
                    _{ resource_uri : Resource_Uri } :< Document,
                    Resource_Or_Payload = resource(Resource_Uri)
                )
            ->  true
            ;   Resource_Or_Payload = payload(Document)
            )
        ),
        error(bad_api_document(Document,[resource_uri]),_)),

    api_report_errors(
        unpack,
        Request,
        (   unpack(System_DB, Auth, Path, Resource_Or_Payload),
            cors_reply_json(Request,
                            _{'@type' : 'api:UnpackResponse',
                              'api:status' : "api:success"},
                            [status(200), width(0)])
        )).

%:- begin_tests(unpack_endpoint).
%:- end_tests(unpack_endpoint).

%%%%%%%%%%%%%%%%%%%% TUS Handler %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(files), tus_auth_wrapper(tus_dispatch),
                [ methods([options,head,post,patch,delete]),
                  prefix
                ]).

:- meta_predicate tus_auth_wrapper(2,?).
tus_auth_wrapper(Goal,Request) :-
    open_descriptor(system_descriptor{}, System_Database),
    catch((      authenticate(System_Database, Request, Auth),
                 www_form_encode(Auth, Domain),
                 (   memberchk(x_terminusdb_api_base(Pre_Base), Request)
                 ->  terminal_slash(Pre_Base, Base),
                     atom_concat(Base, 'api/files', Endpoint),
                     Options0 = [resumable_endpoint_base(Endpoint)]
                 ;   Options0 = []
                 ),
                 (   file_upload_storage_path(Path)
                 ->  Options = [tus_storage_path(Path)|Options0]
                 ;   Options = Options0),
                 call(Goal, [domain(Domain)|Options], Request)),
          error(authentication_incorrect(_Reason),_),
          (   reply_json(_{'@type' : 'api:ErrorResponse',
                           'api:status' : 'api:failure',
                           'api:error' : _{'@type' : 'api:IncorrectAuthenticationError'},
                           'api:message' : 'Incorrect authentication information'
                          },
                         [status(401), width(0)]))),
    !.

%%%%%%%%%%%%%%%%%%%% Push Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(push/Path), cors_handler(Method, push_handler(Path)),
                [method(Method),
                 prefix,
                 time_limit(infinite),
                 methods([options,post])]).

push_handler(post,Path,Request, System_DB, Auth) :-
    do_or_die(
        (  get_payload(Document, Request),
           _{ remote : Remote_Name,
              remote_branch : Remote_Branch } :< Document),
        error(bad_api_document(Document,[remote,remote_branch]),_)),

    do_or_die(
        request_remote_authorization(Request, Authorization),
        error(no_remote_authorization)),

    (   get_dict(push_prefixes, Document, true)
    ->  Push_Prefixes = true
    ;   Push_Prefixes = false),

    api_report_errors(
        push,
        Request,
        (   push(System_DB, Auth, Path, Remote_Name, Remote_Branch, [prefixes(Push_Prefixes)],
                 authorized_push(Authorization),Result),
            (   Result = same(Head_ID)
            ->  Head_Updated = false
            ;   Result = new(Head_ID)
            ->  Head_Updated = true
            ;   throw(error(internal_server_error,_))),

            Response =  _{'@type' : "api:PushResponse",
                          'api:repo_head_updated' : Head_Updated,
                          'api:repo_head' : Head_ID,
                          'api:status' : "api:success"},

            cors_reply_json(Request,
                            Response,
                            [status(200), width(0)]))).

:- begin_tests(push_endpoint).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).
:- use_module(library(base64)).

test(push_empty_to_empty_does_nothing_succesfully,
     [
         setup(
             (   setup_temp_unattached_server(State_Origin,Store_Origin,Server_Origin),
                 setup_temp_unattached_server(State_Destination,Store_Destination,Server_Destination),
                 setup_cloned_situation(Store_Origin, Server_Origin, Store_Destination, Server_Destination)
             )),
         cleanup(
             (
                 teardown_temp_unattached_server(State_Origin),
                 teardown_temp_unattached_server(State_Destination))),
         blocked(document_refactor)
     ]) :-
    resolve_absolute_string_descriptor("RosaLuxemburg/bar", Origin_Branch_Descriptor),
    Origin_Database_Descriptor = (Origin_Branch_Descriptor.repository_descriptor.database_descriptor),
    resolve_absolute_string_descriptor("KarlKautsky/foo", Destination_Branch_Descriptor),
    Destination_Database_Descriptor = (Destination_Branch_Descriptor.repository_descriptor.database_descriptor),

    with_triple_store(
        Store_Origin,
        repository_head(Origin_Database_Descriptor, "origin", Head)),
    with_triple_store(
        Store_Destination,
        repository_head(Destination_Database_Descriptor, "local", Head)),

    atomic_list_concat([Server_Origin, '/api/push/RosaLuxemburg/bar'], Push_URL),
    base64("KarlKautsky:password_destination", Base64_Destination_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Destination_Auth]),
    http_post(Push_URL,
              json(_{
                       remote: "origin",
                       remote_branch: "main"
                   }),
              JSON,
              [json_object(dict),authorization(basic('RosaLuxemburg','password_origin')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(Status_Code)]),

    * json_write_dict(current_output, JSON, []),

    Status_Code = 200,
    \+ get_dict('api:head', JSON, _),

    _{'@type':"api:PushResponse",
      'api:repo_head_updated': false,
      'api:repo_head': Head,
      'api:status':"api:success"} :< JSON.

test(push_empty_with_prefix_change_to_empty_changes_prefixes,
     [
         setup(
             (   setup_temp_unattached_server(State_Origin,Store_Origin,Server_Origin),
                 setup_temp_unattached_server(State_Destination,Store_Destination,Server_Destination),
                 setup_cloned_situation(Store_Origin, Server_Origin, Store_Destination, Server_Destination)
             )),
         cleanup(
             (
                 teardown_temp_unattached_server(State_Origin),
                 teardown_temp_unattached_server(State_Destination))),
         blocked(document_refactor)
     ]) :-
    resolve_absolute_string_descriptor("RosaLuxemburg/bar", Origin_Branch_Descriptor),
    resolve_absolute_string_descriptor("KarlKautsky/foo", Destination_Branch_Descriptor),

    with_triple_store(
        Store_Origin,
        (   create_context((Origin_Branch_Descriptor.repository_descriptor),
                           Origin_Repository_Context),
            with_transaction(Origin_Repository_Context,
                             update_prefixes(Origin_Repository_Context,
                                             _{doc: "http://this_is_docs/",
                                               scm: "http://this_is_scm/",
                                               foo: "http://this_is_foo/"}),
                             _))),

    atomic_list_concat([Server_Origin, '/api/push/RosaLuxemburg/bar'], Push_URL),
    base64("KarlKautsky:password_destination", Base64_Destination_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Destination_Auth]),
    http_post(Push_URL,
              json(_{
                       remote: "origin",
                       remote_branch: "main",
                       push_prefixes: true
                   }),
              JSON,
              [json_object(dict),authorization(basic('RosaLuxemburg','password_origin')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(Status_Code)]),

    * json_write_dict(current_output, JSON, []),

    Status_Code = 200,

    _{'@type':"api:PushResponse",
      'api:repo_head_updated': true,
      'api:repo_head': _,
      'api:status':"api:success"} :< JSON,

    with_triple_store(
        Store_Destination,
        (   repository_prefixes((Destination_Branch_Descriptor.repository_descriptor),
                                Prefixes),
            Prefixes = _{doc: 'http://this_is_docs/',
                         scm: 'http://this_is_scm/',
                         foo: 'http://this_is_foo/'})).

test(push_nonempty_to_empty_advances_remote_head,
     [
         setup(
             (   setup_temp_unattached_server(State_Origin,Store_Origin,Server_Origin),
                 setup_temp_unattached_server(State_Destination,Store_Destination,Server_Destination),
                 setup_cloned_situation(Store_Origin, Server_Origin, Store_Destination, Server_Destination)
             )),
         cleanup(
             (
                 teardown_temp_unattached_server(State_Origin),
                 teardown_temp_unattached_server(State_Destination))),
         blocked(document_refactor)
     ]) :-
    resolve_absolute_string_descriptor("RosaLuxemburg/bar", Origin_Branch_Descriptor),
    Origin_Database_Descriptor = (Origin_Branch_Descriptor.repository_descriptor.database_descriptor),
    resolve_absolute_string_descriptor("KarlKautsky/foo", Destination_Branch_Descriptor),
    Destination_Database_Descriptor = (Destination_Branch_Descriptor.repository_descriptor.database_descriptor),

    with_triple_store(
        Store_Origin,
        repository_head(Origin_Database_Descriptor, "origin", Head)),
    with_triple_store(
        Store_Destination,
        repository_head(Destination_Database_Descriptor, "local", Head)),
    atomic_list_concat([Server_Origin, '/api/push/RosaLuxemburg/bar'], Push_URL),
    base64("KarlKautsky:password_destination", Base64_Destination_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Destination_Auth]),
    http_post(Push_URL,
              json(_{
                       remote: "origin",
                       remote_branch: "main"
                   }),
              JSON,
              [json_object(dict),authorization(basic('RosaLuxemburg','password_origin')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(Status_Code)]),

    * json_write_dict(current_output, JSON, []),

    Status_Code = 200,

    _{'@type':"api:PushResponse", % todo this should actually not be that
      'api:repo_head_updated': false,
      'api:repo_head': Head,
      'api:status':"api:success"} :< JSON.

test(push_nonempty_to_same_nonempty_keeps_remote_head_unchanged,
     [
         setup(
             (   setup_temp_unattached_server(State_Origin,Store_Origin,Server_Origin),
                 setup_temp_unattached_server(State_Destination,Store_Destination,Server_Destination),
                 setup_cloned_nonempty_situation(Store_Origin, Server_Origin, Store_Destination, Server_Destination)
             )),
         cleanup(
             (
                 teardown_temp_unattached_server(State_Origin),
                 teardown_temp_unattached_server(State_Destination))),
         blocked(document_refactor)
     ]) :-
    resolve_absolute_string_descriptor("RosaLuxemburg/bar", Origin_Branch_Descriptor),
    Origin_Database_Descriptor = (Origin_Branch_Descriptor.repository_descriptor.database_descriptor),
    resolve_absolute_string_descriptor("KarlKautsky/foo", Destination_Branch_Descriptor),
    Destination_Database_Descriptor = (Destination_Branch_Descriptor.repository_descriptor.database_descriptor),

    with_triple_store(
        Store_Origin,
        repository_head(Origin_Database_Descriptor, "origin", Head)),
    with_triple_store(
        Store_Destination,
        repository_head(Destination_Database_Descriptor, "local", Head)),

    atomic_list_concat([Server_Origin, '/api/push/RosaLuxemburg/bar'], Push_URL),
    base64("KarlKautsky:password_destination", Base64_Destination_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Destination_Auth]),
    http_post(Push_URL,
              json(_{
                       remote: "origin",
                       remote_branch: "main"
                   }),
              JSON,
              [json_object(dict),authorization(basic('RosaLuxemburg','password_origin')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(Status_Code)]),

    * json_write_dict(current_output, JSON, []),

    Status_Code = 200,

    _{'@type':"api:PushResponse", % todo this should actually not be that
      'api:repo_head_updated': false,
      'api:repo_head': Head,
      'api:status':"api:success"} :< JSON,

    with_triple_store(
        Store_Origin,
        repository_head(Origin_Database_Descriptor, "origin", Head)),
    with_triple_store(
        Store_Destination,
        repository_head(Destination_Database_Descriptor, "local", Head)).

test(push_nonempty_to_earlier_nonempty_advances_remote_head,
     [
         setup(
             (   setup_temp_unattached_server(State_Origin,Store_Origin,Server_Origin),
                 setup_temp_unattached_server(State_Destination,Store_Destination,Server_Destination),
                 setup_cloned_nonempty_situation(Store_Origin, Server_Origin, Store_Destination, Server_Destination)
             )),
         cleanup(
             (
                 teardown_temp_unattached_server(State_Origin),
                 teardown_temp_unattached_server(State_Destination))),
         blocked(document_refactor)
     ]) :-
    resolve_absolute_string_descriptor("RosaLuxemburg/bar", Origin_Branch_Descriptor),
    with_triple_store(
        Store_Origin,
        (   create_context(Origin_Branch_Descriptor, commit_info{author:"Rosa",message:"hello"}, Origin_Context_1),
            with_transaction(Origin_Context_1,
                             ask(Origin_Context_1,
                                 insert(g,h,i)),
                             _),
            create_context(Origin_Branch_Descriptor, commit_info{author:"Rosa",message:"hello again"}, Origin_Context_2),
            with_transaction(Origin_Context_2,
                             ask(Origin_Context_2,
                                 insert(j,k,l)),
                             _)
        )
    ),

    atomic_list_concat([Server_Origin, '/api/push/RosaLuxemburg/bar'], Push_URL),
    base64("KarlKautsky:password_destination", Base64_Destination_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Destination_Auth]),
    http_post(Push_URL,
              json(_{
                       remote: "origin",
                       remote_branch: "main"
                   }),
              JSON,
              [json_object(dict),authorization(basic('RosaLuxemburg','password_origin')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(Status_Code)]),

    * json_write_dict(current_output, JSON, []),

    Status_Code = 200,

    _{'@type':"api:PushResponse",
      'api:repo_head_updated': true,
      'api:repo_head': Head,
      'api:status':"api:success"} :< JSON,

    Origin_Database_Descriptor = (Origin_Branch_Descriptor.repository_descriptor.database_descriptor),
    resolve_absolute_string_descriptor("KarlKautsky/foo", Destination_Branch_Descriptor),
    Destination_Database_Descriptor = (Destination_Branch_Descriptor.repository_descriptor.database_descriptor),

    with_triple_store(
        Store_Origin,
        repository_head(Origin_Database_Descriptor, "origin", Head)),
    with_triple_store(
        Store_Destination,
        repository_head(Destination_Database_Descriptor, "local", Head)).

:- end_tests(push_endpoint).

%%%%%%%%%%%%%%%%%%%% Pull Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(pull/Path), cors_handler(Method, pull_handler(Path)),
                [method(Method),
                 prefix,
                 time_limit(infinite),
                 methods([options,post])]).

pull_handler(post,Path,Request, System_DB, Local_Auth) :-
    % Can't we just ask for the default remote?
    do_or_die(
        (   get_payload(Document, Request),
            _{ remote : Remote_Name,
               remote_branch : Remote_Branch_Name
             } :< Document),
        error(bad_api_document(Document, [remote, remote_branch]),_)),

    do_or_die(
        request_remote_authorization(Request, Remote_Auth),
        error(no_remote_authorization)),

    api_report_errors(
        pull,
        Request,
        (   pull(System_DB, Local_Auth, Path, Remote_Name, Remote_Branch_Name,
                 authorized_fetch(Remote_Auth),
                 Result),
            JSON_Base = _{'@type' : 'api:PullResponse',
                          'api:status' : "api:success"},
            put_dict(Result,JSON_Base,JSON_Response),
            cors_reply_json(Request,
                            JSON_Response,
                            [status(200), width(0)]))).

:- begin_tests(pull_endpoint, []).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(api)).
:- use_module(library(http/http_open)).

test(pull_from_empty_to_empty,
     [
         setup(
             (   setup_temp_unattached_server(State_Local,Store_Local,Server_Local),
                 setup_temp_unattached_server(State_Remote,Store_Remote,Server_Remote),
                 setup_cloned_situation(Store_Local, Server_Local, Store_Remote, Server_Remote)
             )),
         cleanup(
             (
                 teardown_temp_unattached_server(State_Local),
                 teardown_temp_unattached_server(State_Remote))),
         blocked(document_refactor)
     ]) :-
    resolve_absolute_string_descriptor("RosaLuxemburg/bar", Local_Branch_Descriptor),
    Local_Database_Descriptor = (Local_Branch_Descriptor.repository_descriptor.database_descriptor),
    resolve_absolute_string_descriptor("KarlKautsky/foo", Remote_Branch_Descriptor),
    Remote_Database_Descriptor = (Remote_Branch_Descriptor.repository_descriptor.database_descriptor),

    with_triple_store(
        Store_Local,
        repository_head(Local_Database_Descriptor, "origin", Head)),
    with_triple_store(
        Store_Remote,
        repository_head(Remote_Database_Descriptor, "local", Head)),

    atomic_list_concat([Server_Local, '/api/pull/RosaLuxemburg/bar'], Pull_URL),
    base64("KarlKautsky:password_destination", Base64_Remote_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Remote_Auth]),
    http_post(Pull_URL,
              json(_{
                       remote: "origin",
                       remote_branch: "main"
                   }),
              JSON,
              [json_object(dict),authorization(basic('RosaLuxemburg','password_origin')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(Status_Code)]),

    * json_write_dict(current_output, JSON, []),

    Status_Code = 200,
    _{'@type' : "api:PullResponse",
      'api:pull_status' : "api:pull_unchanged",
      'api:fetch_status' : false,
      'api:status':"api:success"} :< JSON.

test(pull_from_something_to_empty,
     [
         setup(
             (   setup_temp_unattached_server(State_Local,Store_Local,Server_Local),
                 setup_temp_unattached_server(State_Remote,Store_Remote,Server_Remote),
                 setup_cloned_situation(Store_Local, Server_Local, Store_Remote, Server_Remote)
             )),
         cleanup(
             (
                 teardown_temp_unattached_server(State_Local),
                 teardown_temp_unattached_server(State_Remote))),
         blocked(document_refactor)
     ]) :-
    resolve_absolute_string_descriptor("RosaLuxemburg/bar", Local_Branch_Descriptor),
    Local_Repository_Descriptor = (Local_Branch_Descriptor.repository_descriptor),
    Local_Database_Descriptor = (Local_Repository_Descriptor.database_descriptor),
    resolve_absolute_string_descriptor("KarlKautsky/foo", Remote_Branch_Descriptor),
    Remote_Repository_Descriptor = (Remote_Branch_Descriptor.repository_descriptor),
    Remote_Database_Descriptor = (Remote_Repository_Descriptor.database_descriptor),

    with_triple_store(
        Store_Remote,
        (   create_context(Remote_Branch_Descriptor, commit_info{author:"KarlKautsky", message:"Boo!"}, Remote_Context_1),
            with_transaction(
                Remote_Context_1,
                ask(Remote_Context_1,
                    insert(a,b,c)),
                _),
            repository_head(Remote_Database_Descriptor, "local", Head),
            branch_head_commit(Remote_Repository_Descriptor, "main", Remote_Commit_Uri),
            commit_id_uri(Remote_Repository_Descriptor, Commit_Id, Remote_Commit_Uri)
        )
    ),

    atomic_list_concat([Server_Local, '/api/pull/RosaLuxemburg/bar'], Pull_URL),
    base64("KarlKautsky:password_destination", Base64_Remote_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Remote_Auth]),
    http_post(Pull_URL,
              json(_{
                       remote: "origin",
                       remote_branch: "main"
                   }),
              JSON,
              [json_object(dict),authorization(basic('RosaLuxemburg','password_origin')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(Status_Code)]),

    * json_write_dict(current_output, JSON, []),

    Status_Code = 200,
    _{'@type' : "api:PullResponse",
      'api:pull_status' : "api:pull_fast_forwarded",
      'api:fetch_status' : true,
      'api:status':"api:success"} :< JSON,

    with_triple_store(
        Store_Local,
        (
            repository_head(Local_Database_Descriptor, "origin", Head),
            branch_head_commit(Local_Repository_Descriptor, "main", Local_Commit_Uri),
            commit_id_uri(Local_Repository_Descriptor, Commit_Id, Local_Commit_Uri)
        )
    ).

test(pull_from_something_to_something,
     [
         setup(
             (   setup_temp_unattached_server(State_Local,Store_Local,Server_Local),
                 setup_temp_unattached_server(State_Remote,Store_Remote,Server_Remote),
                 setup_cloned_nonempty_situation(Store_Local, Server_Local, Store_Remote, Server_Remote)
             )),
         cleanup(
             (
                 teardown_temp_unattached_server(State_Local),
                 teardown_temp_unattached_server(State_Remote))),
         blocked(document_refactor)
     ]) :-
    resolve_absolute_string_descriptor("RosaLuxemburg/bar", Local_Branch_Descriptor),
    Local_Repository_Descriptor = (Local_Branch_Descriptor.repository_descriptor),
    Local_Database_Descriptor = (Local_Repository_Descriptor.database_descriptor),
    resolve_absolute_string_descriptor("KarlKautsky/foo", Remote_Branch_Descriptor),
    Remote_Repository_Descriptor = (Remote_Branch_Descriptor.repository_descriptor),
    Remote_Database_Descriptor = (Remote_Repository_Descriptor.database_descriptor),

    with_triple_store(
        Store_Local,
        branch_head_commit(Local_Repository_Descriptor, "main", Local_Orginal_Commit_Uri)
    ),

    with_triple_store(
        Store_Remote,
        (   create_context(Remote_Branch_Descriptor, commit_info{author:"KarlKautsky", message:"Boo!"}, Remote_Context_1),
            with_transaction(
                Remote_Context_1,
                ask(Remote_Context_1,
                    insert(e,f,g)),
                _),
            repository_head(Remote_Database_Descriptor, "local", Head),
            branch_head_commit(Remote_Repository_Descriptor, "main", Remote_Commit_Uri),
            commit_id_uri(Remote_Repository_Descriptor, Commit_Id, Remote_Commit_Uri)
        )
    ),

    atomic_list_concat([Server_Local, '/api/pull/RosaLuxemburg/bar'], Pull_URL),
    base64("KarlKautsky:password_destination", Base64_Remote_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Remote_Auth]),
    http_post(Pull_URL,
              json(_{
                       remote: "origin",
                       remote_branch: "main"
                   }),
              JSON,
              [json_object(dict),authorization(basic('RosaLuxemburg','password_origin')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(Status_Code)]),

    * json_write_dict(current_output, JSON, []),

    Status_Code = 200,
    _{'@type' : "api:PullResponse",
      'api:pull_status' : "api:pull_fast_forwarded",
      'api:fetch_status' : true,
      'api:status':"api:success"} :< JSON,

    with_triple_store(
        Store_Local,
        (
            repository_head(Local_Database_Descriptor, "origin", Head),
            branch_head_commit(Local_Repository_Descriptor, "main", Local_Commit_Uri),
            \+ Local_Orginal_Commit_Uri = Local_Commit_Uri,
            commit_id_uri(Local_Repository_Descriptor, Commit_Id, Local_Commit_Uri)
        )
    ).

test(pull_from_something_to_something_equal,
     [
         setup(
             (   setup_temp_unattached_server(State_Local,Store_Local,Server_Local),
                 setup_temp_unattached_server(State_Remote,Store_Remote,Server_Remote),
                 setup_cloned_nonempty_situation(Store_Local, Server_Local, Store_Remote, Server_Remote)
             )),
         cleanup(
             (
                 teardown_temp_unattached_server(State_Local),
                 teardown_temp_unattached_server(State_Remote))),
         blocked(document_refactor)
     ]) :-
    resolve_absolute_string_descriptor("RosaLuxemburg/bar", Local_Branch_Descriptor),
    Local_Repository_Descriptor = (Local_Branch_Descriptor.repository_descriptor),
    Local_Database_Descriptor = (Local_Repository_Descriptor.database_descriptor),

    with_triple_store(
        Store_Local,
        (
            repository_head(Local_Database_Descriptor, "origin", Head),
            branch_head_commit(Local_Repository_Descriptor, "main", Local_Commit_Uri)
        )
    ),

    atomic_list_concat([Server_Local, '/api/pull/RosaLuxemburg/bar'], Pull_URL),
    base64("KarlKautsky:password_destination", Base64_Remote_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Remote_Auth]),
    http_post(Pull_URL,
              json(_{
                       remote: "origin",
                       remote_branch: "main"
                   }),
              JSON,
              [json_object(dict),authorization(basic('RosaLuxemburg','password_origin')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(Status_Code)]),

    * json_write_dict(current_output, JSON, []),

    Status_Code = 200,
    _{'@type' : "api:PullResponse",
      'api:pull_status' : "api:pull_unchanged",
      'api:fetch_status' : false,
      'api:status':"api:success"} :< JSON,

    with_triple_store(
        Store_Local,
        (
            repository_head(Local_Database_Descriptor, "origin", Head),
            branch_head_commit(Local_Repository_Descriptor, "main", Local_Commit_Uri)
        )
    ).

test(pull_from_something_to_something_equal_other_branch,
     [
         setup(
             (   setup_temp_unattached_server(State_Local,Store_Local,Server_Local),
                 setup_temp_unattached_server(State_Remote,Store_Remote,Server_Remote),
                 setup_cloned_nonempty_situation(Store_Local, Server_Local, Store_Remote, Server_Remote)
             )),
         cleanup(
             (
                 teardown_temp_unattached_server(State_Local),
                 teardown_temp_unattached_server(State_Remote))),
         blocked(document_refactor)
     ]) :-
    resolve_absolute_string_descriptor("RosaLuxemburg/bar", Local_Branch_Descriptor),
    Local_Repository_Descriptor = (Local_Branch_Descriptor.repository_descriptor),
    Local_Database_Descriptor = (Local_Repository_Descriptor.database_descriptor),
    resolve_absolute_string_descriptor("KarlKautsky/foo", Remote_Branch_Descriptor),
    Remote_Repository_Descriptor = (Remote_Branch_Descriptor.repository_descriptor),
    Remote_Database_Descriptor = (Remote_Repository_Descriptor.database_descriptor),

    with_triple_store(
        Store_Remote,
        (
            agent_name_uri(system_descriptor{}, "KarlKautsky", User_Uri),
            branch_create(system_descriptor{}, User_Uri, "KarlKautsky/foo/local/branch/other", branch("KarlKautsky/foo/local/branch/main"), _),
            repository_head(Remote_Database_Descriptor, "local", Head)
        )
    ),

    with_triple_store(
        Store_Local,
        (
            branch_head_commit(Local_Repository_Descriptor, "main", Local_Commit_Uri)
        )
    ),

    atomic_list_concat([Server_Local, '/api/pull/RosaLuxemburg/bar'], Pull_URL),
    base64("KarlKautsky:password_destination", Base64_Remote_Auth),
    format(string(Authorization_Remote), "Basic ~s", [Base64_Remote_Auth]),
    http_post(Pull_URL,
              json(_{
                       remote: "origin",
                       remote_branch: "main"
                   }),
              JSON,
              [json_object(dict),authorization(basic('RosaLuxemburg','password_origin')),
               request_header('Authorization-Remote'=Authorization_Remote),
               status_code(Status_Code)]),

    * json_write_dict(current_output, JSON, []),

    Status_Code = 200,
    _{'@type' : "api:PullResponse",
      'api:pull_status' : "api:pull_unchanged",
      'api:fetch_status' : true,
      'api:status':"api:success"} :< JSON,

    with_triple_store(
        Store_Local,
        (
            repository_head(Local_Database_Descriptor, "origin", Head),
            branch_head_commit(Local_Repository_Descriptor, "main", Local_Commit_Uri)
        )
    ).


:- end_tests(pull_endpoint).

%%%%%%%%%%%%%%%%%%%% Branch Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(branch/Path), cors_handler(Method, branch_handler(Path)),
                [method(Method),
                 prefix,
                 methods([options,post,delete])]).

branch_handler(post, Path, Request, System_DB, Auth) :-
    do_or_die(
        get_payload(Document, Request),
        error(bad_api_document(Document, []),_)),

    (   get_dict(origin, Document, Origin_Path)
    ->  Origin_Option = branch(Origin_Path)
    ;   ignore(get_dict(prefixes, Document, Input_Prefixes)),
        ignore(get_dict(schema, Document, Schema)),
        Origin_Option = empty(Input_Prefixes, Schema)),

    api_report_errors(
        branch,
        Request,
        (   branch_create(System_DB, Auth, Path, Origin_Option, _Branch_Uri),
            cors_reply_json(Request,
                            _{'@type' : 'api:BranchResponse',
                              'api:status' : "api:success"}))).
branch_handler(delete, Path, Request, System_DB, Auth) :-
    api_report_errors(
        branch,
        Request,
        (   branch_delete(System_DB, Auth, Path),
            cors_reply_json(Request,
                            _{'@type' : 'api:BranchResponse',
                              'api:status' : "api:success"}))).

%%%%%%%%%%%%%%%%%%%% Prefix Handlers %%%%%%%%%%%%%%%%%%%%%%%%%

:- http_handler(api(prefixes/Path), cors_handler(Method, prefix_handler(Path)),
                [method(Method),
                 prefix,
                 methods([options,get])]).

% this allows the client to discover prefixes for a given resource
% from an endpoint, but also regularises the use of JSON-LD contexts
% which can then include the endpoint for context discovery
prefix_handler(get, Path, Request, System_DB, Auth) :-
    api_report_errors(
        prefix,
        Request,
        (   get_prefixes(Path, System_DB, Auth, Prefixes),
            cors_reply_json(Request,
                            Prefixes,
                            [status(200)]))).

%%%%%%%%%%%%%%%%%%%% User handlers %%%%%%%%%%%%%%%%%%%%%%%%%
%
% THIS IS A DEPRECATED HANDLER - YOU SHOULD NOT RELY ON THIS!
%
:- http_handler(api(user), cors_handler(Method, user_handler),
                [method(Method),
                 prefix,
                 methods([options,post,delete])]).
:- http_handler(api(user/Name), cors_handler(Method, user_handler(Name)),
                [method(Method),
                 prefix,
                 methods([options,post,delete])]).

user_handler(post, Name, Request, System_DB, Auth) :-
    get_payload(Document, Request),

    api_report_errors(
        user_update,
        Request,
        (   update_user_transaction(System_DB, Auth, Name, Document),
            cors_reply_json(Request,
                            _{'@type' : "api:UpdateUserResponse",
                              'api:status' : "api:success"}))).
user_handler(delete, Name, Request, System_DB, Auth) :-
    api_report_errors(
        user_delete,
        Request,
        (   delete_user_transaction(System_DB, Auth, Name),
            cors_reply_json(Request,
                            _{'@type' : "api:DeleteUserResponse",
                              'api:status' : "api:success"}))).

user_handler(post, Request, System_DB, Auth) :-
    get_payload(Document, Request),

    do_or_die(_{ agent_name : Agent_Name } :< Document,
              error(malformed_update_user_document(Document, [agent_name]))
             ),

    api_report_errors(
        user_update,
        Request,
        (   update_user_transaction(System_DB, Auth, Agent_Name, Document),
            cors_reply_json(Request,
                            _{'@type' : "api:UpdateUserResponse",
                              'api:status' : "api:success"}))).
user_handler(delete, Request, System_DB, Auth) :-
    api_report_errors(
        user_delete,
        Request,
        (   (   memberchk(payload(JSON), Request)
            ->  true
            ;   JSON = _{}),
            (   memberchk(search(Search), Request)
            ->  true
            ;   Search = []),
            param_value_search_or_json_required(Search, JSON, agent_name, text, Agent_Name),
            delete_user_transaction(System_DB, Auth, Agent_Name),
            cors_reply_json(Request,
                            _{'@type' : "api:DeleteUserResponse",
                              'api:status' : "api:success"}))).

%%%%%%%%%%%%%%%%%%%% User Organization handlers %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% THIS IS A DEPRECATED HANDLER - YOU SHOULD NOT RELY ON THIS!
%
:- http_handler(api(user_organizations), cors_handler(Method, user_organizations_handler),
                [method(Method),
                 prefix,
                 methods([options,get])]).

user_organizations_handler(get, Request, System_DB, Auth) :-
    api_report_errors(
        user_organizations,
        Request,
        (   user_organizations(System_DB, Auth, Result),
            cors_reply_json(Request, Result)
        )
    ).


%%%%%%%%%%%%%%%%%%%% Organization handlers %%%%%%%%%%%%%%%%%%%%%%%%%
%
% THIS IS A DEPRECATED HANDLER - YOU SHOULD NOT RELY ON THIS!
%
:- http_handler(api(organization), cors_handler(Method, organization_handler, [add_payload(false)]),
                [method(Method),
                 prefix,
                 methods([options,post,delete])]).
:- http_handler(api(organization/Name), cors_handler(Method, organization_handler(Name), [add_payload(false)]),
                [method(Method),
                 prefix,
                 methods([options,delete])]).

organization_handler(post, Request, System_DB, Auth) :-
    api_report_errors(
        add_organization,
        Request,
        (   http_read_json_required(json_dict(JSON), Request),

            param_value_json_required(JSON, organization_name, non_empty_string, Org),
            param_value_json_required(JSON, user_name, non_empty_string, User),

            add_user_organization_transaction(System_DB, Auth, User, Org),
            cors_reply_json(Request,
                            _{'@type' : "api:AddOrganizationResponse",
                              'api:status' : "api:success"}))).
organization_handler(delete, Request, System_DB, Auth) :-
    api_report_errors(
        delete_organization,
        Request,
        (   (   memberchk(payload(JSON), Request)
            ->  true
            ;   JSON = _{}),
            (   memberchk(search(Search), Request)
            ->  true
            ;   Search = []),
            param_value_search_or_json_required(Search, JSON, organization_name, non_empty_string, Name),

            delete_organization_transaction(System_DB, Auth, Name),
            cors_reply_json(Request,
                            _{'@type' : "api:DeleteOrganizationResponse",
                              'api:status' : "api:success"}))).

organization_handler(delete, Name, Request, System_DB, Auth) :-
    api_report_errors(
        delete_organization,
        Request,
        (   delete_organization_transaction(System_DB, Auth, Name),
            cors_reply_json(Request,
                            _{'@type' : "api:DeleteOrganizationResponse",
                              'api:status' : "api:success"}))).

%%%%%%%%%%%%%%%%%%%% Squash handler %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(squash/Path), cors_handler(Method, squash_handler(Path)),
                [method(Method),
                 prefix,
                 time_limit(infinite),
                 methods([options,post])]).

/*
 * squash_handler(Mode,Path,Request,System,Auth) is det.
 *
 * Squash commit chain to a new commit
 */
squash_handler(post, Path, Request, System_DB, Auth) :-

    do_or_die(
        (   get_payload(Document, Request),
            _{ commit_info : Commit_Info } :< Document),
        error(bad_api_document(Document, [commit_info]), _)),

    api_report_errors(
        squash,
        Request,
        (   api_squash(System_DB, Auth, Path, Commit_Info, Commit, Old_Commit)
        ->  cors_reply_json(Request, _{'@type' : 'api:SquashResponse',
                                       'api:commit' : Commit,
                                       'api:old_commit' : Old_Commit,
                                       'api:status' : "api:success"})
        ;   cors_reply_json(Request, _{'@type' : 'api:EmptySquashResponse',
                                       'api:empty_commit' : true,
                                       'api:status' : "api:success"})
        )).


%%%%%%%%%%%%%%%%%%%% Reset handler %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(reset/Path), cors_handler(Method, reset_handler(Path)),
                [method(Method),
                 prefix,
                 time_limit(infinite),
                 methods([options,post])]).

/*
 * reset_handler(Mode, Path, Request, System, Auth) is det.
 *
 * Reset a branch to a new commit.
 */
reset_handler(post, Path, Request, System_DB, Auth) :-

    do_or_die(
        (   get_payload(Document, Request),
            _{ commit_descriptor : Ref } :< Document),
        error(bad_api_document(Document, [commit_descriptor]), _)),

    api_report_errors(
        reset,
        Request,
        (   api_reset(System_DB, Auth, Path, Ref),
            cors_reply_json(Request, _{'@type' : 'api:ResetResponse',
                                       'api:status' : "api:success"}))).


%%%%%%%%%%%%%%%%%%%% Optimize handler %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(optimize/Path), cors_handler(Method, optimize_handler(Path)),
                [method(Method),
                 prefix,
                 time_limit(infinite),
                 methods([options,post])]).

/*
 * optimize_handler(Mode,DB,Request) is det.
 *
 * Optimize a resource
 */
optimize_handler(post, Path, Request, System_DB, Auth) :-
    api_report_errors(
        optimize,
        Request,
        (   api_optimize(System_DB, Auth, Path),
            cors_reply_json(Request, _{'@type' : 'api:OptimizeResponse',
                                       'api:status' : "api:success"}))).

:- begin_tests(optimize_endpoint).
:- use_module(core(util/test_utils)).
:- use_module(library(terminus_store)).

test(optimize_system, [
         setup(setup_temp_server(State, Server)),
         cleanup(teardown_temp_server(State))
     ]) :-

    create_db_without_schema("admin", "test"),
    create_db_without_schema("admin", "test2"),
    atomic_list_concat([Server, '/api/optimize/_system'], URI),

    admin_pass(Key),
    http_post(URI,
              json(_{}),
              JSON,
              [json_object(dict),authorization(basic(admin,Key))]),

    JSON = _{'@type':"api:OptimizeResponse",
             'api:status':"api:success"},

    open_descriptor(system_descriptor{}, Transaction),
    [RWO] = (Transaction.instance_objects),
    Layer = (RWO.read),
    \+ parent(Layer,_).

:- end_tests(optimize_endpoint).


%%%%%%%%%%%%%%%%%%%% Remote handler %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(remote/Path), cors_handler(Method, remote_handler(Path)),
                [method(Method),
                 prefix,
                 time_limit(infinite),
                 methods([options,post,put,get,delete])]).

remote_handler(post, Path, Request, System_DB, Auth) :-

    do_or_die(
        (   get_payload(Document, Request),
            _{ remote_name : Remote_Name,
               remote_location : URL
             } :< Document),
        error(bad_api_document(Document, [remote_name, remote_location]), _)),

    api_report_errors(
        remote,
        Request,
        (   add_remote(System_DB, Auth, Path, Remote_Name, URL),
            cors_reply_json(Request, _{'@type' : 'api:RemoteResponse',
                                       'api:status' : "api:success"}))).
remote_handler(delete, Path, Request, System_DB, Auth) :-
    api_report_errors(
        remote,
        Request,
        (   (   memberchk(payload(JSON), Request)
            ->  true
            ;   JSON = _{}),
            (   memberchk(search(Search), Request)
            ->  true
            ;   Search = []),
            format(user_error, "~n~nSearch ~q~n~n", [Search]),
            format(user_error, "~n~nJSON ~q~n~n", [JSON]),

            param_value_search_or_json_required(Search, JSON, remote_name, text, Remote_Name),
            remove_remote(System_DB, Auth, Path, Remote_Name),
            cors_reply_json(Request, _{'@type' : 'api:RemoteResponse',
                                       'api:status' : "api:success"}))).
remote_handler(put, Path, Request, System_DB, Auth) :-
    do_or_die(
        (   get_payload(Document, Request),
            _{ remote_name : Remote_Name,
               remote_location : URL
             } :< Document),
        error(bad_api_document(Document, [remote_name]), _)),

    api_report_errors(
        remote,
        Request,
        (   update_remote(System_DB, Auth, Path, Remote_Name, URL),
            cors_reply_json(Request, _{'@type' : 'api:RemoteResponse',
                                       'api:status' : "api:success"}))).
remote_handler(get, Path, Request, System_DB, Auth) :-
    api_report_errors(
        remote,
        Request,
        (   (   memberchk(payload(JSON), Request)
            ->  true
            ;   JSON = _{}
            ),
            (   memberchk(search(Search), Request)
            ->  true
            ;   Search = []),
            param_value_search_or_json_optional(Search, JSON, remote_name, text, [], Remote_Name),
            \+ Remote_Name = []
        ->  show_remote(System_DB, Auth, Path, Remote_Name, Remote_URL),
            cors_reply_json(Request, _{'@type' : 'api:RemoteResponse',
                                       'api:remote_name' : Remote_Name,
                                       'api:remote_url' : Remote_URL,
                                       'api:status' : "api:success"})
        ;   list_remotes(System_DB, Auth, Path, Remote_Names),
            cors_reply_json(Request, _{'@type' : 'api:RemoteResponse',
                                       'api:remote_names' : Remote_Names,
                                       'api:status' : "api:success"}))).


%%%%%%%%%%%%%%%%%%%% Patch handler %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(patch), cors_handler(Method, patch_handler),
                [method(Method),
                 prefix,
                 time_limit(infinite),
                 methods([options,post])]).

/*
 * patch_handler(Mode, Request, System, Auth) is det.
 *
 * Reset a branch to a new commit.
 */
patch_handler(post, Request, System_DB, Auth) :-
    do_or_die(
        (   get_payload(Document, Request),
            _{ before : Before,
               patch : Patch
             } :< Document),
        error(bad_api_document(Document, [before, patch]), _)),

    % We should take options about final state here.
    api_report_errors(
        patch,
        Request,
        (   api_patch(System_DB, Auth, Patch, Before, Result, Document),
            (   Result = success(After)
            ->  cors_reply_json(Request, After)
            ;   Result = conflict(Conflict)
            ->  cors_reply_json(Request, Conflict, [status(409)])
            )
        )
    ).

%%%%%%%%%%%%%%%%%%%% Diff handler %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(diff), cors_handler(Method, diff_handler(none{})),
                [method(Method),
                 time_limit(infinite),
                 methods([options,post])]).
:- http_handler(api(diff/Path), cors_handler(Method, diff_handler(Path)),
                [method(Method),
                 prefix,
                 time_limit(infinite),
                 methods([options,post])]).

/*
 * diff_handler(Mode, Request, System, Auth) is det.
 *
 * Reset a branch to a new commit.
 */
diff_handler(post, Path, Request, System_DB, Auth) :-
    get_payload(Document, Request),
    do_or_die((   _{ before : Before,
                     after : After
                   } :< Document,
                  Operation = document
              ;   _{ before_data_version : Before_Version
                   } :< Document,
                  (   _{ after_data_version: After_Version}
                      :< Document,
                      (   _{ document_id: Doc_ID} :< Document
                      ->  Operation = versioned_document
                      ;   Operation = all_documents)
                  ;   _{ after: After_Document,
                         document_id : Doc_ID} :< Document,
                      Operation = versioned_document_document)
              ),
              error(bad_api_document_choices(Document, [[before, after],
                                                        [before_data_version,
                                                         after_data_version],
                                                        [before_data_version,
                                                         after_data_version,
                                                         document_id],
                                                        [before_data_version,
                                                         after,
                                                         document_id]]), _)),

    % We could probably just feed the document in,
    % the default is dubious.
    (   get_dict(keep,Document,_)
    ->  Options = Document
    ;   put_dict(_{ keep : _{ '@id' : true, '_id' : true }},
                 Document, Options)
    ),

    api_report_errors(
        diff,
        Request,
        (   (   Operation = document
            ->  api_diff(System_DB, Auth, Before, After, Patch, Options)
            ;   Operation = versioned_document
            ->  api_diff_id(System_DB, Auth, Path, Before_Version,
                            After_Version, Doc_ID, Patch, Options)
            ;   Operation = all_documents
            ->  api_diff_all_documents(System_DB, Auth, Path, Before_Version, After_Version, Patch, Options)
            ;   api_diff_id_document(System_DB, Auth, Path,
                                     Before_Version, After_Document,
                                     Doc_ID, Patch, Options)
            ),
            cors_reply_json(Request, Patch)
        )
    ).

%%%%%%%%%%%%%%%%%%%% Apply handler %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(apply/Path), cors_handler(Method, apply_handler(Path)),
                [method(Method),
                 prefix,
                 time_limit(infinite),
                 methods([options,post])]).

/*
 * apply_handler(Mode, Path, Request, System, Auth) is det.
 *
 * Reset a branch to a new commit.
 */
apply_handler(post, Path, Request, System_DB, Auth) :-
    get_payload(Document, Request),
    do_or_die((   _{ before_commit: Before_Commit,
                     after_commit: After_Commit,
                     commit_info: Commit_Info
                   } :< Document
              ),
              error(bad_api_document(Document, [before_commit,after_commit,commit_info,type]))
             ),

    (   _{ match_final_state: Match_Final_State } :< Document
    ->  true
    ;   Match_Final_State = true
    ),

    (   _{ keep: Keep } :< Document
    ->  true
    ;   Keep = json{'@id':true, '@type':true}
    ),

    (   _{ type: Type } :< Document
    ->  atom_string(Type_Atom, Type)
    ;   Type_Atom = squash
    ),

    api_report_errors(
        apply,
        Request,
        catch(
            (   api_apply_squash_commit(System_DB, Auth, Path, Commit_Info,
                                        Before_Commit, After_Commit,
                                        [type(Type_Atom),
                                         match_final_state(Match_Final_State),
                                         keep(Keep)]),
                cors_reply_json(Request,
                                json{'@type' : "api:ApplyResponse",
                                     'api:status' : "api:success"})),
            error(apply_squash_witnesses(Witnesses)),
            (   cors_reply_json(Request,
                                json{'@type' : "api:ApplyError",
                                     'api:witnesses' : Witnesses,
                                     'api:status' : 'api:conflict'},
                                [status(409),serialize_unknown(true)]))
        )
    ).


%%%%%%%%%%%%%%%%%%%% Roles handler %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(roles), cors_handler(Method, roles_handler),
                [method(Method),
                 methods([options,post,put,get])]).
:- http_handler(api(roles/Name), cors_handler(Method, roles_handler(Name)),
                [method(Method),
                 methods([options,delete,get])]).

/*
 * roles_handler(Mode, Request, System, Auth) is det.
 *
 * Insert a role or update a role
 */
roles_handler(post, Request, System_DB, Auth) :-
    get_payload(Role, Request),
    do_or_die((   _{ name: Name,
                     action: _
                   } :< Role
              ),
              error(bad_api_document(Role, [name,action]))
             ),
    api_report_errors(
        role,
        Request,
        (   uri_encoded(segment, Name, Encoded_Name),
            atom_concat('Role/',Encoded_Name,Name_Id),
            put_dict(_{'@id': Name_Id}, Role, Role_With_Id),
            api_add_role(System_DB,Auth,Role_With_Id,Role_Id),
            cors_reply_json(Request,Role_Id)
        )
    ).
roles_handler(put, Request, System_DB, Auth) :-
    get_payload(Role, Request),
    do_or_die((   _{ name: Name,
                     action: _
                   } :< Role
              ),
              error(bad_api_document(Role, [name,action]))
             ),
    api_report_errors(
        role,
        Request,
        (   api_get_role_from_name(System_DB,Auth,Name,Old_Role),
            put_dict(Role,Old_Role,New_Role),
            api_update_role(System_DB,Auth,New_Role),
            cors_reply_json(Request,
                            json{'@type' : "api:RolesResponse",
                                 'api:status' : "api:success"})
        )
    ).
roles_handler(get, Request, System_DB, Auth) :-
    api_report_errors(
        role,
        Request,
        (   api_get_roles(System_DB, Auth, Roles),
            cors_reply_json(Request, Roles)
        )
    ).

/*
 * roles_handler(Mode, Path, Request, System, Auth) is det.
 *
 * Delete a role
 */
roles_handler(delete, Name, Request, System_DB, Auth) :-
    api_report_errors(
        role,
        Request,
        (   uri_encoded(segment, Name, Encoded_Name),
            atom_concat('Role/',Encoded_Name,Name_Id),
            api_delete_role(System_DB,Auth,Name_Id),
            cors_reply_json(Request,
                            json{'@type' : "api:RolesResponse",
                                 'api:status' : "api:success"})
        )
    ).
roles_handler(get, Name, Request, System_DB, Auth) :-
    api_report_errors(
        role,
        Request,
        (   api_get_role_from_name(System_DB,Auth,Name,Role),
            cors_reply_json(Request, Role)
        )
    ).

%%%%%%%%%%%%%%%%%%%% Organizations handler %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(organizations), cors_handler(Method, organizations_handler),
                [method(Method),
                 methods([options,get])]).
:- http_handler(api(organizations/Name), cors_handler(Method, organizations_handler(Name)),
                [method(Method),
                 methods([options,post,delete,get])]).
:- http_handler(api(organizations/Org/users/Rest),
                cors_handler(Method, organizations_users_handler(Org,Rest)),
                [method(Method),
                 prefix,
                 methods([options,get])]).
/*
:- http_handler(api(organizations/Org/users/User),
                cors_handler(Method, organizations_users_handler(Org,User)),
                [method(Method),
                 methods([options,get])]).
:- http_handler(api(organizations/Org/users/User/databases),
                cors_handler(Method, organizations_users_databases_handler(Org,User)),
                [method(Method),
                 methods([options,get])]).
*/

/*
 * organizations_handler(Mode, Request, System, Auth) is det.
 *
 * Get all organizations
 */
organizations_handler(get, Request, System_DB, Auth) :-
    api_report_errors(
        organization,
        Request,
        (   api_get_organizations(System_DB, Auth, Orgs),
            cors_reply_json(Request,Orgs)
        )
    ).

/*
 * organizations_handler(Mode, Name, Request, System, Auth) is det.
 *
 * Manage organization
 */
organizations_handler(post, Name, Request, System_DB, Auth) :-
    api_report_errors(
        organization,
        Request,
        (   api_add_organization(System_DB, Auth, _{ name: Name }, Id),
            cors_reply_json(Request,Id)
        )
    ).
organizations_handler(get, Name, Request, System_DB, Auth) :-
    api_report_errors(
        organization,
        Request,
        (   api_get_organization_from_name(System_DB, Auth, Name, Org),
            cors_reply_json(Request,Org)
        )
    ).
organizations_handler(delete, Name, Request, System_DB, Auth) :-
    api_report_errors(
        organization,
        Request,
        (   api_get_organization_from_name(System_DB, Auth, Name, Org),
            get_dict('@id', Org, Org_Id),
            api_delete_organization(System_DB,Auth,Org_Id),
            cors_reply_json(Request,
                            json{'@type' : "api:OrganizationResponse",
                                 'api:status' : "api:success"})
        )
    ).

/*
 * organizations_users_handler(get, Org, Path, Request, System_DB, Auth) is det.
 *
 * Get all users for an organization
 */
organizations_users_handler(get, Org, Path, Request, System_DB, Auth) :-
    api_report_errors(
        organization,
        Request,
        (   split_atom(Path, '/', Path_List),
            (   Path_List = ['']
            ->  api_get_organizations_users(System_DB, Auth, Org, Users),
                cors_reply_json(Request,Users)
            ;   Path_List = [User]
            ->  api_get_organizations_users_object(System_DB, Auth, Org, User, Obj),
                cors_reply_json(Request,Obj)
            ;   Path_List = [User,databases]
            ->  api_get_organizations_users_databases(System_DB, Auth, Org, User, Databases),
                cors_reply_json(Request,Databases)
            )
        )
    ).

%%%%%%%%%%%%%%%%%%%% Users handler %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(users), cors_handler(Method, users_handler),
                [method(Method),
                 methods([options,post,put,get])]).
:- http_handler(api(users/Name), cors_handler(Method, users_handler(Name)),
                [method(Method),
                 methods([options,delete,get])]).

/*
 * users_handler(Mode, Request, System, Auth) is det.
 *
 * Manage users
 */
users_handler(get, Request, System_DB, Auth) :-
    (   memberchk(search(Search), Request)
    ->  true
    ;   Search = []),
    param_value_search_optional(Search, capability, boolean, false, Capability),
    api_report_errors(
        user,
        Request,
        (   api_get_users(System_DB, Auth, Users,_{capability:Capability}),
            cors_reply_json(Request, Users)
        )
    ).
users_handler(post, Request, System_DB, Auth) :-
    get_payload(User, Request),
    do_or_die((   _{ name: _ } :< User
              ),
              error(bad_api_document(User, [name]))
             ),
    % should explicitly search for params...
    api_report_errors(
        user,
        Request,
        (   api_add_user(System_DB, Auth, User, Id),
            cors_reply_json(Request, Id)
        )
    ).
users_handler(put, Request, System_DB, Auth) :-
    get_payload(User, Request),
    do_or_die((   _{ name: Name,
                     password: Pass
                   } :< User
              ),
              error(bad_api_document(User, [name,password]))
             ),
    api_report_errors(
        user,
        Request,
        (   api_update_user_password(System_DB, Auth, Name, Pass),
            cors_reply_json(Request,
                            json{'@type' : "api:UsersResponse",
                                 'api:status' : "api:success"})
        )
    ).

/*
 * users_handler(Mode, Name, Request, System, Auth) is det.
 *
 * Manage Users
 */
users_handler(get, Name, Request, System_DB, Auth) :-
    (   memberchk(search(Search), Request)
    ->  true
    ;   Search = []),
    param_value_search_optional(Search, capability, boolean, false, Capability),
    api_report_errors(
        user,
        Request,
        (   api_get_user_from_name(System_DB, Auth, Name, User, _{capability:Capability}),
            cors_reply_json(Request,User)
        )
    ).
users_handler(delete, Name, Request, System_DB, Auth) :-
    api_report_errors(
        user,
        Request,
        (   api_get_user_from_name(System_DB, Auth, Name, User, _{capability:false}),
            get_dict('@id', User, User_Id),
            api_delete_user(System_DB,Auth,User_Id),
            cors_reply_json(Request,
                            json{'@type' : "api:UsersResponse",
                                 'api:status' : "api:success"})
        )
    ).

%%%%%%%%%%%%%%%%%%%% Capabilities handler %%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler(api(capabilities), cors_handler(Method, capabilities_handler),
                [method(Method),
                 methods([options,post])]).

/*
 * capabilities_handler(Mode, Request, System, Auth) is det.
 *
 * grant or revoke capabilities
 */
capabilities_handler(post, Request, System_DB, Auth) :-
    get_payload(Cap, Request),
    do_or_die((   _{ operation: Op,
                     scope: _,
                     user: _,
                     roles: _
                   } :< Cap
              ),
              error(bad_api_document(Cap, [operation,scope,user,roles]))
             ),
    api_report_errors(
        capability,
        Request,
        (
            (   get_dict(scope_type, Cap, Scope_Type_String)
            ->  atom_string(Scope_Type, Scope_Type_String),
                put_dict(_{ scope_type : Scope_Type}, Cap, Grant_Doc),
                grant_document_to_ids(System_DB, Auth, Grant_Doc, Cap_Ids)
            ;   Cap = Cap_Ids
            ),
            (   Op = "revoke"
            ->  api_revoke_capability(System_DB,Auth,Cap_Ids),
                cors_reply_json(Request,
                                json{'@type' : "api:CapabilityResponse",
                                     'api:status' : "api:success"})
            ;   Op = "grant"
            ->  api_grant_capability(System_DB,Auth,Cap_Ids),
                cors_reply_json(Request,
                                json{'@type' : "api:CapabilityResponse",
                                     'api:status' : "api:success"})
            ;   throw(error(unknown_capabilities_operation(Op)))
            )
        )
    ).

%%%%%%%%%%%%%%%%%%%% GraphQL handler %%%%%%%%%%%%%%%%%%%%%%%%%
http:location(graphql,api(graphql),[]).
:- http_handler(graphql(.), cors_handler(Method, graphql_handler("_system"), [add_payload(false),skip_authentication(true)]),
                [method(Method),
                 methods([options,get,post])]).
:- http_handler(graphql(Path), cors_handler(Method, graphql_handler(Path), [add_payload(false),skip_authentication(true)]),
                [method(Method),
                 prefix,
                 methods([options,get,post])]).

graphql_handler(Method, Path_Atom, Request, System_DB, Auth) :-
    memberchk(input(Input), Request),
    memberchk(content_type(Content_Type), Request),
    memberchk(content_length(Content_Length), Request),

    catch((      authenticate(System_DB, Request, Auth),
                 handle_graphql_request(System_DB, Auth, Method, Path_Atom, Input, Response, Content_Type, Content_Length),
                 write_cors_headers(Request),
                 write('Status: 200'),nl,
                 write('Content-Type: application/json'),nl,
                 nl,
                 write(Response)
          ),
          E,
          handle_graphql_error(E, Request)).

handle_graphql_error(error(authentication_incorrect(_Auth), _), Request) :-
    cors_reply_json(Request,
                    json{'errors': [json{message: "Authentication incorrect"}]},
                    [status(401)]).
handle_graphql_error(error(access_not_authorised(Auth, Action, Scope), _), Request) :-
    format(string(Msg), "Access to ~q is not authorised with action ~q and auth ~q",
           [Scope,Action,Auth]),
    cors_reply_json(Request,
                    json{'errors': [json{message: Msg}]},
                    [status(403)]).
handle_graphql_error(E, Request) :-
    format(string(Msg), "Unexpected error in graphql: ~q",
           [E]),
    cors_reply_json(Request,
                    json{'errors': [json{message: Msg}]},
                    [status(500)]).

%%%%%%%%%%%%%%%%%%%% GraphiQL handler %%%%%%%%%%%%%%%%%%%%%%%%%
http:location(graphiql,root(graphiql),[]).
:- http_handler(graphiql(.), cors_handler(Method, graphiql_handler("_system"), [add_payload(false)]),
                [method(Method),
                 methods([options,get,post])]).
:- http_handler(graphiql(Path), cors_handler(Method, graphiql_handler(Path), [add_payload(false)]),
                [method(Method),
                 prefix,
                 methods([options,get])]).

graphiql_handler(_Method, Path_Atom, _Request, _System_DB, _Auth) :-
    atom_string(Path_Atom, Path),
    format(string(Full_Path), "/api/graphql/~s", [Path]),
    graphiql_template(Template),
    format(string(Result), Template, [Full_Path]),
    throw(http_reply(bytes('text/html', Result))).

%%%%%%%%%%%%%%%%%%%% Dashboard Handlers %%%%%%%%%%%%%%%%%%%%%%%%%
http:location(dashboard,root(dashboard),[]).
http:location(assets,root(assets),[]).

:- http_handler(root(.), redirect_to_dashboard,
                [methods([options,get])]).
:- http_handler(dashboard(.), dashboard_handler,
                [prefix,
                 methods([options,get])]).
:- http_handler(assets(.), serve_dashboard_assets,
                [prefix,
                 methods([options,get])]).

:- meta_predicate try_dashboard(+, :).
try_dashboard(Request, Goal) :-
    (   config:dashboard_enabled
    ->  call(Goal)
    ;   memberchk(request_uri(Uri), Request),
        throw(http_reply(gone(Uri)))).

serve_dashboard_assets(Request) :-
    try_dashboard(Request, serve_files_in_directory(assets, Request)).

redirect_to_dashboard(Request) :-
    try_dashboard(Request, http_redirect(moved_temporary, dashboard(.), Request)).

dashboard_handler(Request) :-
    try_dashboard(Request, http_reply_file(dashboard('index.html'), [], Request)).

%%%%%%%%%%%%%%%%%%%% Reply Hackery %%%%%%%%%%%%%%%%%%%%%%
:- meta_predicate cors_handler(+,2,?).
:- meta_predicate cors_handler(+,2,?,+).
cors_handler(Method, Goal, Request) :-
    cors_handler(Method, Goal, [], Request).
cors_handler(options, _Goal, _Options, Request) :-
    !,
    write_cors_headers(Request),
    format('~n').
cors_handler(_Old_Method, Goal, Options, Request) :-
    select(x_http_method_override(Method), Request, New_Request),
    !,
    downcase_atom(Method,Mapped),
    cors_handler(Mapped, Goal, Options, New_Request).
cors_handler(Method, Goal, Options, R) :-
    cors_catch(
        R,
        (
            (   memberchk(Method, [post, put, delete]),
                \+ memberchk(add_payload(false), Options)
            ->  add_payload_to_request(R,Request)
            ;   Request = R),

            open_descriptor(system_descriptor{}, System_Database),
            catch((   (   option(skip_authentication(true), Options)
                      ->  true
                      ;   authenticate(System_Database, Request, Auth)),
                      call_http_handler(Method, Goal, Request, System_Database, Auth)),

                  error(authentication_incorrect(Reason),_),

                  (   write_cors_headers(Request),
                      json_log_error_formatted("~NAuthentication Incorrect for reason: ~q~n", [Reason]),
                      reply_json(_{'@type' : 'api:ErrorResponse',
                                   'api:status' : 'api:failure',
                                   'api:error' : _{'@type' : 'api:IncorrectAuthenticationError'},
                                   'api:message' : 'Incorrect authentication information'
                                  },
                                 [width(0), status(401)]))))),
    !.
cors_handler(_Method, Goal, _Options, R) :-
    write_cors_headers(R),
    format(string(Msg), "Failed to run the API endpoint goal ~q", Goal),
    reply_json(_{'@type' : 'api:ErrorResponse',
                 'api:status' : 'api:failure',
                 'api:error' : _{'@type' : 'api:APIEndpointFailed'},
                 'api:message' : Msg
                },
               [status(500), width(0)]).

% Evil mechanism for catching, putting CORS headers and re-throwing.
:- meta_predicate cors_catch(+, 0).
:- meta_predicate call_http_handler(+,3,?,?,?).
cors_catch(Request, Goal) :-
    catch(Goal,
          E,
          (
              write_cors_headers(Request),
              customise_exception(E)
          )),
    !.
cors_catch(Request, _Goal) :-
    write_cors_headers(Request),
    % Probably should extract the path from Request
    reply_json(_{'api:status' : 'api:failure',
                 'api:message' :'Unexpected failure in request handler'},
               [status(500), width(0)]).

call_http_handler(Method, Goal, Request, System_Database, Auth) :-
    strip_module(Goal, Module, PlainGoal),
    PlainGoal =.. [Head|Args],
    NewArgs = [Method|Args],
    NewPlainGoal =.. [Head|NewArgs],
    NewGoal = Module:NewPlainGoal,

    call(NewGoal, Request, System_Database, Auth).

customise_exception(reply_json(M,Status)) :-
    reply_json(M,
               [status(Status), width(0)]).
customise_exception(reply_json(M)) :-
    customise_exception(reply_json(M,200)).
customise_exception(error(E)) :-
    generic_exception_jsonld(E,JSON),
    json_http_code(JSON,Status),
    reply_json(JSON,[status(Status), width(0)]).
customise_exception(error(E,_)) :-
    generic_exception_jsonld(E,JSON),
    json_http_code(JSON,Status),
    reply_json(JSON,[status(Status), width(0)]).
customise_exception(http_reply(method_not_allowed(JSON))) :-
    reply_json(JSON,[status(405), width(0)]).
customise_exception(http_reply(not_found(JSON))) :-
    reply_json(JSON,[status(404), width(0)]).
customise_exception(http_reply(authorize(JSON))) :-
    reply_json(JSON,[status(401), width(0)]).
customise_exception(http_reply(not_acceptable(JSON))) :-
    reply_json(JSON,[status(406), width(0)]).
customise_exception(time_limit_exceeded) :-
    reply_json(_{'api:status' : 'api:failure',
                 'api:message' : 'Connection timed out'
               },
               [status(408), width(0)]).
customise_exception(error(E)) :-
    format(atom(EM),'Error: ~q', [E]),
    reply_json(_{'api:status' : 'api:server_error',
                 'api:message' : EM},
               [status(500), width(0)]).
customise_exception(error(E, CTX)) :-
    json_log_error_formatted('~N[Exception] ~q',[error(E,CTX)]),
    (   CTX = context(prolog_stack(Stack),_)
    ->  with_output_to(
            string(Ctx_String),
            print_prolog_backtrace(current_output,Stack))
    ;   format(string(Ctx_String), "~q", [CTX])),
    format(atom(EM),'Error: ~q~n~s', [E, Ctx_String]),
    reply_json(_{'api:status' : 'api:server_error',
                 'api:message' : EM},
               [status(500), width(0)]).
customise_exception(http_reply(Obj)) :-
    throw(http_reply(Obj)).
customise_exception(E) :-
    json_log_error_formatted('~N[Exception] ~q',[E]),
    throw(E).

/*
 * api_error_http_reply(API,Error,Request) is false + exception.
 *
 * Throw an API appropriate JSON-LD response message.
 *
 */
api_error_http_reply(API, Error, Request) :-
    api_error_jsonld(API,Error,JSON),
    json_http_code(JSON,Status),
    cors_reply_json(Request,JSON,[status(Status),serialize_unknown(true)]).

api_error_http_reply(API, Error, Type, Request) :-
    api_error_jsonld(API,Error,Type,JSON),
    json_http_code(JSON,Status),
    cors_reply_json(Request,JSON,[status(Status),serialize_unknown(true)]).

:- meta_predicate api_report_errors(?,?,0).
api_report_errors(API,Request,Goal) :-
    catch_with_backtrace(
        Goal,
        Error,
        do_or_die(api_error_http_reply(API,Error,Request),
                  Error)
    ).

%%%%%%%%%%%%%%%%%%%% Access Rights %%%%%%%%%%%%%%%%%%%%%%%%%

/*
 *  fetch_authorization_data(+Request, -KS) is semi-determinate.
 *
 *  Fetches the HTTP Basic Authorization data
 */
fetch_authorization_data(Request, Username, KS) :-
    memberchk(authorization(Text), Request),
    http_authorization_data(Text, basic(Username, Key)),
    coerce_literal_string(Key, KS).

:- if(config:jwt_enabled).
/*
 *  fetch_jwt_data(+Request, -Username) is semi-determinate.
 *
 *  Fetches the HTTP JWT data
 */
fetch_jwt_data(Token, Username) :-
    atom_string(TokenAtom, Token),

    do_or_die(jwt_decode(TokenAtom, Payload, []),
              error(authentication_incorrect(jwt_decode_failed(TokenAtom)), _)),

    do_or_die(
        (   atom_json_dict(Payload, PayloadDict, []),
            jwt_subject_claim_name(ClaimName),
            % replace with dict key get (or whatever it is called)
            get_dict(ClaimName, PayloadDict, UsernameString),
            atom_string(Username, UsernameString)),
        error(malformed_jwt_payload(Payload))).
:- else.
fetch_jwt_data(_Token, _Username) :-
    throw(error(authentication_incorrect(jwt_authentication_requested_but_no_key_configured),_)).
:- endif.


/*
 * authenticate(+Database, +Request, -Auth_Obj) is det.
 *
 * This should either bind the Auth_Obj or throw an http_status_reply/4 message.
 */
authenticate(System_Askable, Request, Auth) :-
    fetch_authorization_data(Request, Username, KS),
    !,

    (   user_key_user_id(System_Askable, Username, KS, Auth)
    ->  true
    ;   format(string(Message), "User '~w' failed to authenticate through basic auth", Username),
        json_log_debug(_{
                           message: Message,
                           authMethod: basic,
                           authResult: failure,
                           user: Username
                       }),

        throw(error(authentication_incorrect(basic_auth(Username)),_))),

    format(string(Message), "User '~w' authenticated through basic auth", Username),
    json_log_debug(_{
                       message: Message,
                       authMethod: basic,
                       authResult: success,
                       user: Username
                   }).
authenticate(System_Askable, Request, Auth) :-
    memberchk(authorization(Text), Request),
    pattern_string_split(" ", Text, ["Bearer", Token]),
    !,
    % Try JWT if no http keys
    fetch_jwt_data(Token, Username),
    (   username_auth(System_Askable, Username, Auth)
    ->  true
    ;   format(string(Message), "User '~w' failed to authenticate through JWT", Username),
        json_log_debug(_{
                           message: Message,
                           authMethod: jwt,
                           authResult: failure,
                           user: Username
                       }),
        throw(error(authentication_incorrect(jwt_no_user_with_name(Username)),_))),

    format(string(Message), "User '~w' authenticated through JWT", Username),
    json_log_debug(_{
                       message: Message,
                       authMethod: jwt,
                       authResult: success,
                       user: Username
                   }).
authenticate(System_Askable, Request, Auth) :-
    insecure_user_header_key(Header_Key),
    Header =.. [Header_Key, Username],
    memberchk(Header, Request),
    !,
    (   username_auth(System_Askable, Username, Auth)
    ->  true
    ;   format(string(Message), "User '~w' failed to authenticate with header '~w'", [Username, Header_Key]),
        json_log_debug(_{
                           message: Message,
                           authMethod: insecure_user_header,
                           authResult: failure,
                           user: Username
                       }),
        throw(error(authentication_incorrect(insecure_user_header_no_user_with_name(Username)),_))),
    format(string(Message), "User '~w' authenticated with header '~w'", [Username, Header_Key]),
    json_log_debug(_{
                       message: Message,
                       authMethod: insecure_user_header,
                       authResult: success,
                       user: Username
                   }).
authenticate(_, _, Auth) :-
    Auth = 'terminusdb://system/data/User/anonymous',
    json_log_debug(_{
                       message: "User 'anonymous' authenticated as no authentication information was submitted",
                       authMethod: anonymous,
                       authResult: success,
                       user: "anonymous"
                   }).

/*
 * write_cors_headers(Request) is det.
 *
 * Writes cors headers associated with Resource_URI
 */
write_cors_headers(Request) :-
    (   memberchk(origin(Origin), Request)
    ->  current_output(Out),
        format(Out,'Access-Control-Allow-Methods: GET, POST, PUT, DELETE, OPTIONS\n',[]),
        format(Out,'Access-Control-Allow-Credentials: true\n',[]),
        format(Out,'Access-Control-Max-Age: 1728000\n',[]),
        format(Out,'Access-Control-Allow-Headers: Authorization, Authorization-Remote, Accept, Accept-Encoding, Accept-Language, Host, Origin, Referer, Content-Type, Content-Length, Content-Range, Content-Disposition, Content-Description, X-HTTP-METHOD-OVERRIDE\n',[]),
        format(Out,'Access-Control-Allow-Origin: ~s~n',[Origin])
    ;   true).

cors_reply_json(Request, JSON) :-
    write_cors_headers(Request),
    reply_json(JSON, [json_object(dict), width(0)]).

cors_reply_json(Request, JSON, Options) :-
    write_cors_headers(Request),
    reply_json(JSON, [width(0), json_object(dict)|Options]).

/**
 * cors_json_stream_write_headers_(+Request, +Data_Version, +As_List) is det.
 *
 * Write CORS and JSON headers. This should only be called in the following:
 *   - cors_json_stream_end
 *   - cors_json_stream_write_dict
 */
cors_json_stream_write_headers_(Request, Data_Version, As_List) :-
    write_cors_headers(Request),
    write_data_version_header(Data_Version),
    (   As_List = true
    ->  % Write the JSON header
        format("Content-type: application/json; charset=UTF-8~n~n")
    ;   % Write the JSON stream header.
        format("Content-type: application/json; stream=true; charset=UTF-8~n~n")).


%%%%%%%%%%%%%%%%%%%% Response Predicates %%%%%%%%%%%%%%%%%%%%%%%%%

/********************************************************
 * Determinising predicates used in handlers            *
 *                                                      *
 ********************************************************/

/*
 * try_get_param(Key,Request:request,Value) is det.
 *
 * Get a parameter from the request independent of request variety.
 */
try_get_param(Key,Request,Value) :-
    % GET or POST (but not application/json)
    memberchk(method(Method), Request),
    memberchk(Method, [post,put]),
    memberchk(multipart(Form_Data), Request),
    !,
    member(mime(Mime_Header,Encoded_Value,_),Form_Data),
    memberchk(name(Key), Mime_Header),
    (   json_mime_type(Mime_Header)
    ->  atom_json_dict(Encoded_Value,Value,[])
    ;   uri_encoded(query_value, Value, Encoded_Value)).
try_get_param(Key,Request,Value) :-
    % GET or POST (but not application/json)
    memberchk(method(Method), Request),
    (   memberchk(Method, [get,delete])
    ;   memberchk(Method, [post,put]),
        \+ json_content_type(Request)),

    http_parameters(Request, [], [form_data(Data)]),

    (   memberchk(Key=Value,Data)
    <>  throw(error(no_parameter_key_in_query_parameters(Key,Data)))),
    !.
try_get_param(Key,Request,Value) :-
    % POST with JSON package
    memberchk(method(Method), Request),
    memberchk(Method, [post,put]),

    json_content_type(Request),

    (   memberchk(payload(Document), Request)
        <>  throw(error(no_document_for_key(Key)))),

    (   get_dict(Key,Document,Value)
        <>  throw(error(no_parameter_key_in_document(Key,Document)))),
    !.
try_get_param(Key,Request,_Value) :-
    % OTHER with method
    memberchk(method(Method), Request),
    !,
    throw(error(no_parameter_key_for_method(Key,Method))).
try_get_param(Key,_Request,_Value) :-
    % Catch all.
    throw(error(no_parameter_key(Key))).

json_content_type(Request) :-
    memberchk(content_type(CT), Request),
    re_match('^application/json', CT, []).

multipart_content_type(Request) :-
    memberchk(content_type(Content_Type), Request),
    http_parse_header_value(content_type, Content_Type, media(multipart/'form-data', _)).

check_content_type(Request, Expected, ContentType) :-
    do_or_die(
        memberchk(content_type(ContentType), Request),
        error(missing_content_type(Expected), _)).

check_content_type_json(Request) :-
    Expected = 'application/json',
    check_content_type(Request, Expected, ContentType),
    atom_concat('^', Expected, RE),
    do_or_die(
        re_match(RE, ContentType, []),
        error(bad_content_type(ContentType, Expected), _)).

json_mime_type(Mime) :-
    memberchk(type(CT),Mime),
    re_match('^application/json', CT, []).

check_content_length(Request) :-
    do_or_die(
        memberchk(content_length(_), Request),
        error(missing_content_length, _)).

content_encoded(Request, Encoding) :-
    memberchk(content_encoding(Encoding), Request),
    do_or_die(
        accepted_encoding(Encoding),
        error(unsupported_content_encoding(Encoding), _)).

accepted_encoding(deflate).
accepted_encoding(gzip).

/*
 * get_param_default(Key,Request:request,Value,Default) is semidet.
 *
 * We can fail with this one, so you better do your own checking.
 */
get_param(Key,Request,Value) :-
    % GET or POST (but not application/json)
    memberchk(method(Method), Request),
    memberchk(Method, [post,put]),

    memberchk(multipart(Form_Data), Request),
    !,
    memberchk(mime(Mime_Header,Encoded_Value,_),Form_Data),
    memberchk(name(Key), Mime_Header),
    (   json_mime_type(Mime_Header)
    ->  atom_json_dict(Encoded_Value,Value,[])
    ;   uri_encoded(query_value, Value, Encoded_Value)).
get_param(Key,Request,Value) :-
    % GET or POST (but not application/json)
    memberchk(method(Method), Request),
    (   memberchk(Method, [get,delete])
    ;   memberchk(Method, [post,put]),
        \+ json_content_type(Request)),

    http_parameters(Request, [], [form_data(Data)]),
    memberchk(Key=Value,Data),
    !.
get_param(Key,Request,Value) :-
    % POST with JSON package
    memberchk(method(Method), Request),
    memberchk(Method, [post,put]),

    json_content_type(Request),
    memberchk(payload(Document), Request),
    Value = Document.get(Key).

/*
 * read_json_dict(+Atom, -JSON) is det.
 * read_json_dict(+Text, -JSON) is det.
 *
 * - Read JSON from an atom or text and catch syntax errors.
 */
read_json_dict(AtomOrText, JSON) :-
    catch(
        atom_json_dict(AtomOrText, JSON, [default_tag(json)]),
        error(syntax_error(json(_Kind)), _),
        throw(error(malformed_json_payload(AtomOrText), _))
    ).

/*
 * http_read_utf8(-Output, Request) is det.
 *
 * - Read the request payload into Output.
 * - Output can be one of:
 *   - stream(Stream) to read into a stream
 *   - string(String) to read into a string
 *   - json_dict(JSON) to read into a JSON dict
 */
http_read_utf8(stream(Stream), Request) :-
    (   content_encoded(Request, Encoding)
    ->  memberchk(input(Input_Stream), Request),
        zopen(Input_Stream, Uncompressed_Stream, [format(Encoding), multi_part(false)]),
        read_string(Uncompressed_Stream, _, S1),
        open_string(S1, Stream)
    ;   http_read_data(Request, String, [to(string), input_encoding(utf8)]),
        open_string(String, Stream)
    ).
http_read_utf8(string(String), Request) :-
    http_read_utf8(stream(Stream), Request),
    read_string(Stream, _Length, String).
http_read_utf8(json_dict(JSON), Request) :-
    http_read_utf8(string(String), Request),
    memberchk(content_length(_Len), Request),
    read_json_dict(String, JSON).

/*
 * http_read_json_required(-Output, +Request) is det.
 *
 * - Throw exceptions if the minimal requirements for JSON input are not met.
 * - Read the request payload into a stream.
 */
http_read_json_required(Output, Request) :-
    check_content_type_json(Request),
    check_content_length(Request),
    http_read_utf8(Output, Request).

/*
 * http_read_json_semidet(-Output, +Request) is semidet.
 *
 * - Fail if the minimal requirements for JSON input are not met.
 * - Read the request payload into a stream.
 */
http_read_json_semidet(Output, Request) :-
    json_content_type(Request),
    memberchk(content_length(_Len), Request),
    http_read_utf8(Output, Request).

/*
 * http_read_multipart_semidet(+Request, -Form_Data) is semidet.
 *
 * - Fail if the minimal requirements for multipart/form-data input are not met.
 * - Read the request payload as multipart form data.
 */
http_read_multipart_semidet(Request, Form_Data) :-
    multipart_content_type(Request),
    http_read_data(Request, Form_Data, [on_filename(save_post_file), form_data(mime)]).

/*
 * http_read_multipart_json_semidet(+Form_Data, -JSON, -Form_Data_Out) is semidet.
 *
 * - Fail if the form data does not contain a part with type 'application/json'.
 * - Read the JSON dict from the part.
 * - Return the remaining parts.
 */
http_read_multipart_json_semidet([], _JSON, []) :-
    !,
    fail.
http_read_multipart_json_semidet([mime(Mime_Header, Value, _) | Form_Data], JSON, Form_Data) :-
    json_mime_type(Mime_Header),
    !,
    read_json_dict(Value, JSON).
http_read_multipart_json_semidet([Part | Form_Data], JSON, [Part | Form_Data_Out]) :-
    http_read_multipart_json_semidet(Form_Data, JSON, Form_Data_Out).

/*
 * add_payload_to_request(Request:request,JSON:json) is det.
 *
 * Updates request with JSON-LD payload in payload(Document).
 * This should really be done automatically at request time
 * using the endpoint wrappers so we don't forget to do it.
 */
add_payload_to_request(Request,[multipart(Form_Data)|Request]) :-
    http_read_multipart_semidet(Request, Form_Data),
    !.
add_payload_to_request(Request,[payload(Document)|Request]) :-
    http_read_json_semidet(json_dict(Document), Request),
    !.
add_payload_to_request(Request,[payload(Document)|Request]) :-
    memberchk(content_type(_Some_Other_Type), Request),
    !,
    http_read_data(Request, Document, []).
add_payload_to_request(Request,Request).

get_payload(Payload,Request) :-
    memberchk(payload(Payload),Request),
    !.
get_payload(Payload,Request) :-
    memberchk(multipart(Form_Data),Request),
    member(mime(Meta,Value,_),Form_Data),
    memberchk(name(payload), Meta),
    read_json_dict(Value, Payload).

/*
 * request_remote_authorization(Request, Authorization) is det.
 */
request_remote_authorization(Request, Token) :-
    memberchk(authorization_remote(Token),Request).

/*
 * save_post_file(In,File_Spec,Options) is det.
 *
 * Saves a temporary octet stream to a file. Used for multipart
 * file passing via POST.
 */
save_post_file(In, file(Filename, File), Options) :-
    option(filename(Filename), Options),
    setup_call_cleanup(
        tmp_file_stream(octet, File, Out),
        copy_stream_data(In, Out),
        close(Out)
    ).

/*
 * collect_multipart_files(+Parts, -Files) is det.
 *
 * Search through the form data parts to collect matching pairs of file name
 * from request and temporary file name on server.
 */
collect_multipart_files(Parts, Files) :-
    convlist(
        [mime(Mime_Header, Data, _), Filename=Temp_Filename]>>(
            memberchk(filename(Filename), Mime_Header),
            setup_call_cleanup(
                tmp_file_stream(octet, Temp_Filename, Out),
                write(Out, Data),
                close(Out)
            )
        ),
        Parts,
        Files
    ).
collect_multipart_files(_Parts, []).

/*
 * Make a collection of all posted files for
 * use in a Context via csv interface
 */
collect_posted_named_files(Request,Files) :-
    memberchk(multipart(Parts), Request),
    !,
    convlist([mime(Mime_Header,Data,_),Name=Temp_Filename]>>(
                 memberchk(filename(_),Mime_Header),
                 memberchk(name(Name),Mime_Header),
                 \+ json_mime_type(Mime_Header),
                 setup_call_cleanup(
                     tmp_file_stream(octet, Temp_Filename, Out),
                     write(Out, Data),
                     close(Out)
                 )
             ),Parts,Files).
collect_posted_named_files(_Request,[]).


%% Logging

match_http_info(method(Method), Method_Upper, _Protocol, _Host, _Port, _Url_Suffix, _Remote_Ip, _User_Agent, _Size, _Operation_Id) :-
    string_upper(Method, Method_Upper).
match_http_info(protocol(Protocol), _Method, Protocol, _Host, _Port, _Url_Suffix, _Remote_Ip, _User_Agent, _Size, _Operation_Id).
match_http_info(host(Host), _Method, _Protocol, Host, _Port, _Url_Suffix, _Remote_Ip, _User_Agent, _Size, _Operation_Id).
match_http_info(port(Port), _Method, _Protocol, _Host, Port, _Url_Suffix, _Remote_Ip, _User_Agent, _Size, _Operation_Id).
match_http_info(request_uri(Url_Suffix), _Method, _Protocol, _Host, _Port, Url_Suffix, _Remote_Ip, _User_Agent, _Size, _Operation_Id).
match_http_info(peer(Peer), _Method, _Protocol, _Host, _Port, _Url_Suffix, Remote_Ip, _User_Agent, _Size, _Operation_Id) :-
    % what about ipv6 though?
    % is there any other sort of peer possible?
    Peer = ip(N1,N2,N3,N4),
    format(string(Remote_Ip), "~w.~w.~w.~w", [N1, N2, N3, N4]).
match_http_info(user_agent(User_Agent), _Method, _Protocol, _Host, _Port, _Url_Suffix, _Remote_Ip, User_Agent, _Size, _Operation_Id).
match_http_info(content_length(Size), _Method, _Protocol, _Host, _Port, _Url_Suffix, _Remote_Ip, _User_Agent, Size_String, _Operation_Id) :-
    term_string(Size, Size_String).
match_http_info(x_operation_id(Operation_Id), _Method, _Protocol, _Host, _Port, _Url_Suffix, _Remote_Ip, _User_Agent, _Size, Operation_Id_String) :-
    atom_string(Operation_Id, Operation_Id_String).
match_http_info(_, _Method, _Protocol, _Host, _Port, _Url_Suffix, _Remote_Ip, _User_Agent, _Size, _Operation_Id).

extract_http_info_([], _Method, _Protocol, _Host, _Port, _Url_Suffix, _Remote_Ip, _User_Agent, _Size, _Operation_Id).
extract_http_info_([First|Rest], Method, Protocol, Host, Port, Url_Suffix, Remote_Ip, User_Agent, Size, Operation_Id) :-
    match_http_info(First, Method, Protocol, Host, Port, Url_Suffix, Remote_Ip, User_Agent, Size, Operation_Id),
    !,
    extract_http_info_(Rest, Method, Protocol, Host, Port, Url_Suffix, Remote_Ip, User_Agent, Size, Operation_Id).

extract_http_info(Request, Method, Url, Path, Remote_Ip, User_Agent, Size, Operation_Id) :-
    extract_http_info_(Request, Method, Protocol, Host, Port, Path, Remote_Ip, User_Agent, Size, Operation_Id),

    (   var(Port)
    ->  format(string(Url), "~w://~w~w", [Protocol, Host, Path])
    ;   format(string(Url), "~w://~w:~w~w", [Protocol, Host, Port, Path])).


get_current_id_from_stream(Id) :-
    current_output(CGI),
    cgi_property(CGI, id(Id)).

save_request(Request) :-
    get_time(Now),
    get_current_id_from_stream(Id),
    extract_http_info(Request, Method, Url, Path, Remote_Ip, User_Agent, Size, Submitted_Operation_Id),
    (   var(Submitted_Operation_Id)
    ->  Operation_Id = none
    ;   Operation_Id = some(Submitted_Operation_Id)),

    include([_-V]>>(nonvar(V)), [requestMethod-Method,
                                 requestUrl-Url,
                                 requestSize-Size,
                                 remoteIp-Remote_Ip,
                                 userAgent-User_Agent
                                ],
           Http_Pairs),
    assert(saved_request(Id, Now, Path, Operation_Id, Http_Pairs)).

:- multifile http:request_expansion/2.
http:request_expansion(Request, Request) :-
    save_request(Request).

http_request_logger(_) :-
    % Skip work if info log is not enabled
    \+ info_log_enabled,
    !,
    true.
http_request_logger(request_start(Local_Id, Request)) :-
    extract_http_info(Request, Method, _Url, Path, _Remote_Ip, _User_Agent, _Size, Submitted_Operation_Id),
    generate_request_id(Local_Id, Request_Id),

    (   var(Submitted_Operation_Id)
    ->  Operation_Id = first(Request_Id)
    ;   Operation_Id = Submitted_Operation_Id),

    format(string(Message), "Request ~w started - ~w ~w", [Request_Id, Method, Path]),

    include([_-V]>>(nonvar(V)), [method-Method,
                                 path-Path,
                                 message-Message
                                ],
            Dict_Pairs),
    dict_create(Dict, json, Dict_Pairs),
    json_log_debug(Operation_Id,
                  Request_Id,
                   Dict).

http_request_logger(request_finished(Local_Id, Code, _Status, _Cpu, Bytes)) :-
    term_string(Bytes, Bytes_String),

    saved_request(Local_Id, Start, Path, Submitted_Operation_Id, Initial_Http_Pairs),
    retract(saved_request(Local_Id, Start, Path, Submitted_Operation_Id, Initial_Http_Pairs)),
    get_time(Now),
    Latency is Now - Start,
    format(string(Latency_String), "~9fs", [Latency]),
    Http_Pairs = [
        status-Code,
        responseSize-Bytes_String,
        latency-Latency_String
        |Initial_Http_Pairs],

    dict_create(Http, json, Http_Pairs),

    generate_request_id(Local_Id, Request_Id),
    (   Submitted_Operation_Id = some(Operation_Id)
    ->  true
    ;   Operation_Id = last(Request_Id)),

    format(string(Message), "~w ~w (~w)", [Http.requestMethod, Path, Code]),
    json_log_info(Operation_Id,
                  Request_Id,
                  json{
                      httpRequest: Http,
                      message: Message
                  }).
