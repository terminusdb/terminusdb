:- module(db_clone, [
              clone/11
          ]).


:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).
:- use_module(db_create).
:- use_module(db_delete).
:- use_module(db_fetch).
:- use_module(db_fast_forward).
:- use_module(api_remote).

:- meta_predicate clone(+,+,+,+,+,+,+,+,+,3,-).
clone(System_DB, Auth, Account,DB,Label,Comment,Public,Remote,Source,Fetch_Predicate,Meta_Data) :-
    setup_call_catcher_cleanup(
        true,
        clone_(System_DB, Auth, Account,DB,Label,Comment,Public,Remote,Source,Fetch_Predicate,Meta_Data),
        exception(E),

        (   clone_cleanup_required(E)
        ->  force_delete_db(Account, DB)
        ;   true)).

clone_cleanup_required(remote_pack_failed(_)).
clone_cleanup_required(remote_pack_unpexected_failure(_)).
clone_cleanup_required(error(http_open_error(_), _)).
clone_cleanup_required(error(remote_connection_failure(_, _), _)).

do_clone_(Auth,Account,DB,Remote,Source, Fetch_Predicate,Meta_Data) :-
    open_descriptor(system_descriptor{}, System_DB),

    % Is this local or over the network
    remote_path(Source, Remote_Path),

    resolve_absolute_descriptor([Account,DB,"_meta"], Database_Descriptor),
    create_context(Database_Descriptor, Database_Context),
    % Add remote
    with_transaction(
        Database_Context,
        insert_remote_repository(Database_Context,
                                 Remote,
                                 Remote_Path,
                                 _Head,
                                 _Repo_URI
                                ),
        _Meta_Data),

    From_Path_List = [Account,DB,Remote,"_commits"],
    merge_separator_split(From_Path, '/', From_Path_List),

    % Fetch remote
    (   Remote_Path = db(_Remote_Team,_Remote_DB)
    ->  local_fetch(System_DB, Auth, From_Path, _New_Head, _Has_Updated)
    ;   remote_fetch(System_DB, Auth, From_Path, Fetch_Predicate, _New_Head, _Has_Updated)
    ),

    resolve_absolute_descriptor([Account,DB,"local","branch","main"], To_Branch_Descriptor),
    resolve_absolute_descriptor([Account,DB,Remote,"branch","main"], From_Branch_Descriptor),

    % Fast forward commits from master in remote to master in local
    fast_forward_branch(To_Branch_Descriptor, From_Branch_Descriptor, Applied_Commits),

    Meta_Data = _{ applied_commits : Applied_Commits }.


:- meta_predicate clone_(+,+,+,+,+,+,+,+,+,3,-).
clone_(System_DB,Auth,Account,DB,Label,Comment,Public,Remote,Source,Fetch_Predicate,Meta_Data) :-
    % Create DB
    create_db_unfinalized(System_DB, Auth, Account, DB, Label, Comment, false, Public, _{'@base' : 'http://example.com/', '@schema' : 'http://example.com#'}, Db_Uri),

    catch_with_backtrace(do_clone_(Auth,Account,DB,Remote,Source,Fetch_Predicate,Meta_Data),
                         Error,
                         true),

    (   var(Error)
    ->  finalize_db(Db_Uri)
    ;   force_delete_db(Account, DB),
        throw(Error)).


