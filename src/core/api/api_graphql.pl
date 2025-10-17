:- module(api_graphql, [handle_graphql_request/8]).

:- use_module(core(util)).
:- use_module(core(transaction)).
:- use_module(core(document)).
:- use_module(core(account)).
:- use_module(core(transaction)).
:- use_module(core(query)).
:- use_module(library(pcre), [re_replace/4]).

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

% Post-process GraphQL JSON response to convert decimal strings to JSON numbers
% This ensures xsd:decimal values are returned as JSON numbers per JSON_SERIALIZATION_RULES.md
post_process_graphql_decimals(ResponseIn, ResponseOut) :-
    % Convert decimal field strings to unquoted numbers
    % Pattern: "fieldname":"0.123..." → "fieldname":0.123...
    % This applies to all decimal fields (bigfloat, decimalValue, etc.)
    re_replace('"(bigfloat|decimalValue|price|taxRate|quantity|value20digits|value15digits|calculation)":"([0-9.eE+-]+)"'/g, '"\\1":\\2', ResponseIn, Temp),
    % Continue with any other decimal-like fields
    re_replace('"([a-zA-Z_][a-zA-Z0-9_]*)":"([0-9]+\\.[0-9]+)"'/g, '"\\1":\\2', Temp, ResponseOut).

handle_graphql_request(System_DB, Auth, Method, Path_Atom, Input_Stream, Response, _Content_Type, Content_Length) :-
    atom_string(Path_Atom, Path),
    (   Path == ""
    %->  '$graphql':handle_system_request(Method, System_DB, Auth, Content_Length, Input_Stream, Response)
    ->  throw(error(no_graphql_path_given, _))
    ;   (   resolve_absolute_string_descriptor(Path, Desc)
        ->  do_or_die(open_descriptor(Desc, Transaction),
                      error(unresolvable_absolute_descriptor(Desc), _))
        ;   throw(error(invalid_absolute_path(Path), _))
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

        create_context(Transaction, commit_info{author: Author, message: Message}, C),
        catch(
            with_transaction(C,
                             (   '$graphql':handle_request(Method, Graphql_Context, System_DB, Meta_DB, Commit_DB, Transaction, Auth, Content_Length, Input_Stream, ResponseRaw, Is_Error, Author, Message),
                                 die_if(Is_Error = true,
                                        response(ResponseRaw)),
                                 (   var(Author)
                                 ->  user_name_uri(System_DB, Author, Auth)
                                 ;   true),
                                 (   var(Message)
                                 ->  Message = "Mutation through GraphQL"
                                 ;   true)
                             ),

                             _),
            response(ResponseRaw),
            json_log_info_formatted("intercepted a failing graphql, not committing", [])),
        % Post-process: convert decimal strings to JSON numbers
        post_process_graphql_decimals(ResponseRaw, Response)
    ).

