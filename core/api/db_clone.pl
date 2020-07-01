:- module(db_clone, [
              clone/9
          ]).


:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).
:- use_module(db_create).
:- use_module(db_delete).
:- use_module(db_fetch).
:- use_module(db_fast_forward).

:- meta_predicate clone(+,+,+,+,+,+,+,3,-).
clone(System_DB, Auth, Account,DB,Label,Comment,Remote_URL,Fetch_Predicate,Meta_Data) :-
    setup_call_catcher_cleanup(
        true,
        clone_(System_DB, Auth, Account,DB,Label,Comment,Remote_URL,Fetch_Predicate,Meta_Data),
        exception(_),

        (   catch(try_delete_db(Account,DB),
                  error(database_not_found(_)),
                  true))).


:- meta_predicate clone_(+,+,+,+,+,+,+,3,-).
clone_(System_DB, Auth, Account,DB,Label,Comment,Remote_URL,Fetch_Predicate,Meta_Data) :-
    % Create DB
    create_db(System_DB, Auth, Account, DB, Label, Comment, _{}),

    resolve_absolute_descriptor([Account,DB,"_meta"], Database_Descriptor),
    create_context(Database_Descriptor, Database_Context),
    % Add remote
    with_transaction(
        Database_Context,
        insert_remote_repository(Database_Context,
                                 "origin",
                                 Remote_URL,
                                 _Head,
                                 _Repo_URI
                                ),
        _Meta_Data),

    resolve_absolute_descriptor([Account,DB,"local","_commits"], To_Descriptor),
    resolve_absolute_descriptor([Account,DB,"origin","_commits"], From_Descriptor),

    % Fetch remote
    remote_fetch(From_Descriptor, Fetch_Predicate, _New_Head, _Has_Updated),

    create_context(To_Descriptor, To_Context),
    with_transaction(
        To_Context,
        % Copy Prefixes
        copy_prefixes(From_Descriptor, To_Context),
        _),

    resolve_absolute_descriptor([Account,DB,"local","branch", "master"], To_Branch_Descriptor),
    resolve_absolute_descriptor([Account,DB,"origin","branch", "master"], From_Branch_Descriptor),

    % Fast forward commits from master in remote to master in local
    fast_forward_branch(To_Branch_Descriptor, From_Branch_Descriptor, Applied_Commits),
    Meta_Data = _{ applied_commits : Applied_Commits }.
