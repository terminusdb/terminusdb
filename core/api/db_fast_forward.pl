:- module(db_fast_forward, [
%              fast_forward_branch/4
          ]).

:- use_module(core(query)).
:- use_module(core(transaction)).

/*
fast_forward_branch(Our_Branch_Descriptor, Their_Branch_Descriptor, Common_Commit_Id, Their_Branch_Path) :-
    Our_Repo_Descriptor = Our_Branch_Descriptor.repository_descriptor,
    Their_Repo_Descriptor = Their_Branch_Descriptor.repository_descriptor,
    create_context(Our_Repo_Descriptor, Our_Repo_Context),
    create_context(Their_Repo_Descriptor, Their_Repo_Context),

    (   branch_head_commit(Our_Repo_Context, "master", Our_Commit_Uri)
    ->  commit_id_uri(Our_Repo_Context, Our_Commit_Id, Our_Commit_Uri),
        (   branch_head_commit(Their_Repo_Context, "second", Their_Commit_Uri)
        ->  commit_id_uri(Their_Repo_Context, Their_Commit_Id, Their_Commit_Uri),
            most_recent_common_ancestor(Our_Repo_Context, Their_Repo_Context, Our_Commit_Id, Their_Commit_Id, Common_Commit_Id, Our_Branch_Path, Their_Branch_Path)
        ;   throw(error(fast_forward_divergent_history)))
    ;   Our_Branch_Path = []
        
    branch_head_commit(Their_Repo_Context, "second", Their_Commit_Uri)
    ->  commit_id_uri(Their_Repo_Context, Their_Commit_Id, Their_Commit_Uri),
        most_recent_common_ancestor(Our_Repo_Context, Their_Repo_Context, Our_Commit_Id, Their_Commit_Id, Common_Commit_Id, Our_Branch_Path, Their_Branch_Path)
    ;   throw(error(fast_forward_with_empty_branch))),
        

    (   Our_Branch_Path = []
    ->  true
    % todo include more interesting things in this error
    ;   throw(error(fast_forward_divergent_history))),

    copy_commits(Their_Repo_Context, Our_Repo_Context, Their_Commit_Uri),

    branch_name_uri(Our_Repo_Context, Our_Branch_Descriptor.branch_name, Branch_Uri),
    ignore(unlink_commit_object_from_branch(Our_Repo_Context, Branch_Uri)),
    link_commit_object_to_branch(Our_Repo_Context, Branch_Uri, Their_Commit_Uri).

:- begin_tests(fast_forward_api).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).
:- use_module(core(triple)).

:- use_module(db_init).
:- use_module(db_branch).
test(fast_forward_empty_branch_from_same_repo,
     [setup((setup_temp_store(State),
             create_db('user|foo','test','a test', 'terminus://blah'))),
      cleanup(teardown_temp_store(State))
     ])
:-
    resolve_absolute_string_descriptor("user/foo", Master_Descriptor),

    % create a branch off the master branch (which should result in an empty branch)
    branch_create(Master_Descriptor.repository_descriptor, Master_Descriptor, "second", [], _),

    resolve_absolute_string_descriptor("user/foo/local/branch/second", Second_Descriptor),

    % create two commits on the new branch
    create_context(Second_Descriptor, commit_info{author:"test",message:"commit a"}, Second_Context_1),
    with_transaction(Second_Context_1,
                     ask(Second_Context_1,
                         insert(a,b,c)),
                     _),
    create_context(Second_Descriptor, commit_info{author:"test",message:"commit b"}, Second_Context_2),
    with_transaction(Second_Context_2,
                     ask(Second_Context_2,
                         insert(d,e,f)),
                     _),

    % fast forward master with second
    fast_forward_branch(Master_Descriptor, Second_Descriptor, Common_Commit_Id, Applied_Commit_Ids),

    writeq(Common_Commit_Id),nl,
    writeq(Applied_Commit_Ids),nl,
    
    
    true.

:- end_tests(fast_forward_api).

*/
