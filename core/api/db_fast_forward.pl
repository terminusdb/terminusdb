:- module(db_fast_forward, [
              fast_forward_branch/4
          ]).

:- use_module(core(query)).
:- use_module(core(transaction)).

fast_forward_branch(Our_Branch_Descriptor, Their_Branch_Descriptor, Common_Commit_Id, Their_Branch_Path) :-
    Our_Repo_Descriptor = Our_Branch_Descriptor.repository_descriptor,
    Their_Repo_Descriptor = Their_Branch_Descriptor.repository_descriptor,
    create_context(Our_Repo_Descriptor, Our_Repo_Context),
    create_context(Their_Repo_Descriptor, Their_Repo_Context),

    branch_head_commit(Our_Repo_Context, "master", Our_Commit_Uri),
    commit_id_uri(Our_Repo_Context, Our_Commit_Id, Our_Commit_Uri),
    branch_head_commit(Their_Repo_Context, "second", Their_Commit_Uri),
    commit_id_uri(Their_Repo_Context, Their_Commit_Id, Their_Commit_Uri),

    most_recent_common_ancestor(Our_Repo_Context, Their_Repo_Context, Our_Commit_Id, Their_Commit_Id, Common_Commit_Id, Our_Branch_Path, Their_Branch_Path),

    (   Our_Branch_Path = []
    ->  true
    % todo include more interesting things in this error
    ;   throw(error(fast_forward_divergent_history))),

    copy_commits(Their_Repo_Context, Our_Repo_Context, Their_Commit_Uri),

    branch_name_uri(Our_Repo_Context, Our_Branch_Descriptor.branch_name, Branch_Uri),
    unlink_commit_object_from_branch(Our_Repo_Context, Branch_Uri),
    link_commit_object_to_branch(Our_Repo_Context, Branch_Uri, Their_Commit_Uri).

