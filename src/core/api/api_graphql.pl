:- module(api_graphql, [handle_graphql_request/8]).

:- use_module(core(util)).
:- use_module(core(transaction)).
:- use_module(core(document)).
:- use_module(core(account)).
:- use_module(core(transaction)).
:- use_module(core(query)).

descriptor_db_uri(System_DB, Desc, Database_Uri) :-
    (   branch_descriptor{} :< Desc
    ->  get_dict(repository_descriptor, Desc, Repo),
        get_dict(database_descriptor, Repo, DB)
    ;   repository_descriptor{} :< Desc
    ->  get_dict(database_descriptor, Desc, DB)
    ;   Desc = DB
    ),
    get_dict(database_name, DB, Database_Name),
    get_dict(organization_name, DB, Organization_Name),
    organization_database_name_uri(System_DB, Organization_Name, Database_Name, Database_Uri).

maybe_show_database(System_DB, Auth, Desc, Action, DB, Maybe_DB) :-
    descriptor_db_uri(System_DB, Desc, Scope_Iri),
    (   auth_action_scope(System_DB, Auth, Action, Scope_Iri)
    ->  Maybe_DB = DB
    ;   Maybe_DB = none
    ).

handle_graphql_request(System_DB, Auth, Method, Path_Atom, Input_Stream, Response, _Content_Type, Content_Length) :-
    atom_string(Path_Atom, Path),
    json_log_info_formatted("wow ~q", [Path]),
    (   Path == ""
    ->  '$graphql':handle_system_request(Method, System_DB, Auth, Content_Length, Input_Stream, Response)
    ;   (   resolve_absolute_string_descriptor(Path, Desc)
        ->  open_descriptor(Desc, Transaction)
        ;   Desc = system_descriptor{},
            Transaction = System_DB
        ),
        (   branch_descriptor{} :< Desc
        ->  maybe_show_database(System_DB, Auth, Desc,
                                '@schema':'Action/commit_read_access',
                                (Transaction.parent),
                                Commit_DB),
            maybe_show_database(System_DB, Auth, Desc,
                                '@schema':'Action/meta_read_access',
                                (Transaction.parent.parent),
                                Meta_DB)
        ;   repository_descriptor{} :< Desc
        ->  maybe_show_database(System_DB, Auth, Desc,
                                '@schema':'Action/commit_read_access',
                                Transaction,
                                Commit_DB),
            maybe_show_database(System_DB, Auth, Desc,
                                '@schema':'Action/meta_read_access',
                                (Transaction.parent),
                                Meta_DB)
        ;   database_descriptor{} :< Desc
        ->  Commit_DB = none,
            maybe_show_database(System_DB, Auth, Desc,
                                '@schema':'Action/meta_read_access',
                                (Transaction),
                                Meta_DB)
        ;   Commit_DB = none,
            Meta_DB = none
        ),
        assert_read_access(System_DB, Auth, Desc, type_filter{types:[instance,schema]}),
        (   '$graphql':get_cached_graphql_context(Transaction, Graphql_Context)
        ->  true
        ;   all_class_frames(Transaction, Frames, [compress_ids(true),expand_abstract(true),simple(true)]),
            '$graphql':get_graphql_context(Transaction, Frames, Graphql_Context)),
        '$graphql':handle_request(Method, Graphql_Context, System_DB, Meta_DB, Commit_DB, Transaction, Auth, Content_Length, Input_Stream, Response)
    ).
