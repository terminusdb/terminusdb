:- module(db_fast_forward, [
              fast_forward_branch/3
          ]).

:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).

fast_forward_branch(Our_Branch_Descriptor, Their_Branch_Descriptor, Applied_Commit_Ids) :-
    Our_Repo_Descriptor = Our_Branch_Descriptor.repository_descriptor,
    Their_Repo_Descriptor = Their_Branch_Descriptor.repository_descriptor,
    create_context(Our_Repo_Descriptor, Our_Repo_Context),
    create_context(Their_Repo_Descriptor, Their_Repo_Context),
    with_transaction(Our_Repo_Context,
                     (
                         (   branch_head_commit(Our_Repo_Context, Our_Branch_Descriptor.branch_name, Our_Commit_Uri)
                         % our branch has a previous commit, so we have to find the common ancestor with their branch
                         % This common ancestor should be our own head, so we need to check for divergent history and error in that case.
                         ->  commit_id_uri(Our_Repo_Context, Our_Commit_Id, Our_Commit_Uri),
                             (   branch_head_commit(Their_Repo_Context, Their_Branch_Descriptor.branch_name, Their_Commit_Uri),
                                 commit_id_uri(Their_Repo_Context, Their_Commit_Id, Their_Commit_Uri),
                                 most_recent_common_ancestor(Our_Repo_Context, Their_Repo_Context, Our_Commit_Id, Their_Commit_Id, Common_Commit_Id, Our_Branch_Path, Their_Branch_Path)
                             ->  (   Our_Branch_Path = []
                                 ->  true
                                 ;   throw(error(divergent_history(Common_Commit_Id, Our_Branch_Path, Their_Branch_Path)))),

                                 Applied_Commit_Ids = Their_Branch_Path,
                                 Next = copy_commit(Their_Commit_Id)
                             ;   throw(error(no_common_history)))
                         ;   (   branch_head_commit(Their_Repo_Context, Their_Branch_Descriptor.branch_name, Their_Commit_Uri)
                             ->  commit_id_uri(Their_Repo_Context, Their_Commit_Id, Their_Commit_Uri),
                                 commit_uri_to_history_commit_ids(Their_Repo_Context, Their_Commit_Uri, Applied_Commit_Ids),
                                 Next = copy_commit(Their_Commit_Id)
                             ;   Next = nothing,
                                 Applied_Commit_Ids = [])),

                         (   Next = copy_commit(Commit_Id)
                         ->  copy_commits(Their_Repo_Context, Our_Repo_Context, Commit_Id),
                             branch_name_uri(Our_Repo_Context, Our_Branch_Descriptor.branch_name, Our_Branch_Uri),
                             ignore(unlink_commit_object_from_branch(Our_Repo_Context, Our_Branch_Uri)),
                             context_default_prefixes(Their_Repo_Context, Their_Repo_Context_Stripped),
                             commit_id_uri(Their_Repo_Context_Stripped, Commit_Id, Commit_Uri),
                             link_commit_object_to_branch(Our_Repo_Context, Our_Branch_Uri, Commit_Uri)
                         ;   true)),
                     _).

:- begin_tests(fast_forward_api).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).
:- use_module(core(triple)).

:- use_module(db_create).
:- use_module(db_branch).
test(fast_forward_empty_branch_on_empty_from_same_repo,
     [setup((setup_temp_store(State),
             create_db_without_schema('user|foo','test','a test'))),
      cleanup(teardown_temp_store(State))
     ])
:-
    resolve_absolute_string_descriptor("user/foo", Master_Descriptor),

    % create a branch off the master branch (which should result in an empty branch)
    branch_create(Master_Descriptor.repository_descriptor, Master_Descriptor, "second", _),

    resolve_absolute_string_descriptor("user/foo/local/branch/second", Second_Descriptor),

    % fast forward master with second
    fast_forward_branch(Master_Descriptor, Second_Descriptor, Applied_Commit_Ids),
    Applied_Commit_Ids == [],

    % check history
    Repo_Descriptor = (Master_Descriptor.repository_descriptor),
    \+ branch_head_commit(Repo_Descriptor, "master", _Head_Commit_Uri).

test(fast_forward_empty_branch_from_same_repo,
     [setup((setup_temp_store(State),
             create_db_without_schema('user|foo','test','a test'))),
      cleanup(teardown_temp_store(State))
     ])
:-
    resolve_absolute_string_descriptor("user/foo", Master_Descriptor),

    % create a branch off the master branch (which should result in an empty branch)
    branch_create(Master_Descriptor.repository_descriptor, Master_Descriptor, "second", _),

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
    fast_forward_branch(Master_Descriptor, Second_Descriptor, Applied_Commit_Ids),

    % check history
    Repo_Descriptor = (Master_Descriptor.repository_descriptor),
    branch_head_commit(Repo_Descriptor, "master", Head_Commit_Uri),
    commit_uri_to_history_commit_ids(Repo_Descriptor, Head_Commit_Uri, History),
    History = [Commit_A, Commit_B],
    History = Applied_Commit_Ids,

    commit_id_to_metadata(Repo_Descriptor, Commit_A, _, "commit a", _),
    commit_id_to_metadata(Repo_Descriptor, Commit_B, _, "commit b", _).

test(fast_forward_nonempty_branch_from_same_repo,
     [setup((setup_temp_store(State),
             create_db_without_schema('user|foo','test','a test'))),
      cleanup(teardown_temp_store(State))
     ])
:-
    resolve_absolute_string_descriptor("user/foo", Master_Descriptor),

    % create single commit on master branch
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit a"}, Master_Context),
    with_transaction(Master_Context,
                     ask(Master_Context,
                         insert(a,b,c)),
                     _),

    % create a branch off the master branch
    branch_create(Master_Descriptor.repository_descriptor, Master_Descriptor, "second", _),

    resolve_absolute_string_descriptor("user/foo/local/branch/second", Second_Descriptor),

    % create two commits on the new branch
    create_context(Second_Descriptor, commit_info{author:"test",message:"commit b"}, Second_Context_1),
    with_transaction(Second_Context_1,
                     ask(Second_Context_1,
                         insert(d,e,f)),
                     _),
    create_context(Second_Descriptor, commit_info{author:"test",message:"commit c"}, Second_Context_2),
    with_transaction(Second_Context_2,
                     ask(Second_Context_2,
                         insert(g,h,i)),
                     _),

    % fast forward master with second
    fast_forward_branch(Master_Descriptor, Second_Descriptor, Applied_Commit_Ids),

    % check history
    Repo_Descriptor = (Master_Descriptor.repository_descriptor),
    branch_head_commit(Repo_Descriptor, "master", Head_Commit_Uri),
    commit_uri_to_history_commit_ids(Repo_Descriptor, Head_Commit_Uri, History),
    History = [Commit_A, Commit_B, Commit_C],
    Applied_Commit_Ids = [Commit_B, Commit_C],


    commit_id_to_metadata(Repo_Descriptor, Commit_A, _, "commit a", _),
    commit_id_to_metadata(Repo_Descriptor, Commit_B, _, "commit b", _),
    commit_id_to_metadata(Repo_Descriptor, Commit_C, _, "commit c", _).

test(fast_forward_branch_with_divergent_history_from_same_repo,
     [setup((setup_temp_store(State),
             create_db_without_schema('user|foo','test','a test'))),
      cleanup(teardown_temp_store(State)),
      throws(error(divergent_history(Commit_A_Id,[Commit_B_Id],[Commit_C_Id])))
     ])
:-
    resolve_absolute_string_descriptor("user/foo", Master_Descriptor),

    % create single commit on master branch
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit a"}, Master_Context),
    with_transaction(Master_Context,
                     ask(Master_Context,
                         insert(a,b,c)),
                     _),

    % create a branch off the master branch
    branch_create(Master_Descriptor.repository_descriptor, Master_Descriptor, "second", _),

    resolve_absolute_string_descriptor("user/foo/local/branch/second", Second_Descriptor),

    % create commit on original branch
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit b"}, Master_Context_2),
    with_transaction(Master_Context_2,
                     ask(Master_Context_2,
                         insert(d,e,f)),
                     _),

    % create commit on second branch
    create_context(Second_Descriptor, commit_info{author:"test",message:"commit c"}, Second_Context),
    with_transaction(Second_Context,
                     ask(Second_Context,
                         insert(g,h,i)),
                     _),

    once(ask((Master_Descriptor.repository_descriptor),
             (   t(Commit_A_Uri, ref:commit_message, "commit a"^^xsd:string),
                 t(Commit_A_Uri, ref:commit_id, Commit_A_Id^^xsd:string)))),
    once(ask((Master_Descriptor.repository_descriptor),
             (   t(Commit_B_Uri, ref:commit_message, "commit b"^^xsd:string),
                 t(Commit_B_Uri, ref:commit_id, Commit_B_Id^^xsd:string)))),
    once(ask((Master_Descriptor.repository_descriptor),
             (   t(Commit_C_Uri, ref:commit_message, "commit c"^^xsd:string),
                 t(Commit_C_Uri, ref:commit_id, Commit_C_Id^^xsd:string)))),

    % fast forward master with second
    fast_forward_branch(Master_Descriptor, Second_Descriptor, _Applied_Commit_Ids).

test(fast_forward_branch_from_empty_branch,
     [setup((setup_temp_store(State),
             create_db_without_schema('user|foo','test','a test'))),
      cleanup(teardown_temp_store(State)),
      throws(error(no_common_history))
     ])
:-
    resolve_absolute_string_descriptor("user/foo", Master_Descriptor),

    % create single commit on master branch
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit a"}, Master_Context),
    with_transaction(Master_Context,
                     ask(Master_Context,
                         insert(a,b,c)),
                     _),

    % create a branch off the master branch
    branch_create(Master_Descriptor.repository_descriptor, empty, "second", _),

    resolve_absolute_string_descriptor("user/foo/local/branch/second", Second_Descriptor),

    % fast forward master with second
    fast_forward_branch(Master_Descriptor, Second_Descriptor, _Applied_Commit_Ids).

test(fast_forward_branch_from_unrelated_branch,
     [setup((setup_temp_store(State),
             create_db_without_schema('user|foo','test','a test'))),
      cleanup(teardown_temp_store(State)),
      throws(error(no_common_history))
     ])
:-
    resolve_absolute_string_descriptor("user/foo", Master_Descriptor),

    % create single commit on master branch
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit a"}, Master_Context),
    with_transaction(Master_Context,
                     ask(Master_Context,
                         insert(a,b,c)),
                     _),

    % create a branch off the master branch
    branch_create(Master_Descriptor.repository_descriptor, empty, "second", _),

    resolve_absolute_string_descriptor("user/foo/local/branch/second", Second_Descriptor),
    create_context(Second_Descriptor, commit_info{author:"test",message:"commit b"}, Second_Context),
    with_transaction(Second_Context,
                     ask(Second_Context,
                         insert(d,e,f)),
                     _),


    % fast forward master with second
    fast_forward_branch(Master_Descriptor, Second_Descriptor, _Applied_Commit_Ids).

test(fast_forward_empty_branch_from_empty_branch,
     [setup((setup_temp_store(State),
             create_db_without_schema('user|foo','test','a test'))),
      cleanup(teardown_temp_store(State))
     ])
:-
    resolve_absolute_string_descriptor("user/foo", Master_Descriptor),

    % create a branch off the master branch
    branch_create(Master_Descriptor.repository_descriptor, empty, "second", _),

    resolve_absolute_string_descriptor("user/foo/local/branch/second", Second_Descriptor),

    % fast forward master with second
    fast_forward_branch(Master_Descriptor, Second_Descriptor, Applied_Commit_Ids),

    Applied_Commit_Ids == [].

test(fast_forward_nonempty_branch_from_equal_branch,
     [setup((setup_temp_store(State),
             create_db_without_schema('user|foo','test','a test'))),
      cleanup(teardown_temp_store(State))
     ])
:-
    resolve_absolute_string_descriptor("user/foo", Master_Descriptor),
    create_context(Master_Descriptor, _{author:"author",message:"message"}, Master_Context),
    with_transaction(Master_Context,
                     ask(Master_Context,
                         insert(a,b,c)),
                     _),

    % create a branch off the master branch
    branch_create(Master_Descriptor.repository_descriptor, Master_Descriptor, "second", _),

    resolve_absolute_string_descriptor("user/foo/local/branch/second", Second_Descriptor),

    % fast forward master with second
    fast_forward_branch(Master_Descriptor, Second_Descriptor, Applied_Commit_Ids),

    Applied_Commit_Ids == [].

test(fast_forward_branch_from_other_repo,
     [setup((setup_temp_store(State),
             create_db_without_schema('user|foo','test','a test'),
             create_db_without_schema('user|bar','test','a test'))),
      cleanup(teardown_temp_store(State))
     ])
:-
    resolve_absolute_string_descriptor("user/foo", Foo_Descriptor),

    % create single commit on master branch
    create_context(Foo_Descriptor, commit_info{author:"test",message:"commit a"}, Foo_Context),
    with_transaction(Foo_Context,
                     ask(Foo_Context,
                         insert(a,b,c)),
                     _),

    % create a branch off the master branch
    resolve_absolute_string_descriptor("user/bar/local/branch/second", Bar_Descriptor),
    branch_create(Bar_Descriptor.repository_descriptor, Foo_Descriptor, "second", _),


    % create two commits on the new branch
    create_context(Bar_Descriptor, commit_info{author:"test",message:"commit b"}, Bar_Context_1),
    with_transaction(Bar_Context_1,
                     ask(Bar_Context_1,
                         insert(d,e,f)),
                     _),
    create_context(Bar_Descriptor, commit_info{author:"test",message:"commit c"}, Bar_Context_2),
    with_transaction(Bar_Context_2,
                     ask(Bar_Context_2,
                         insert(g,h,i)),
                     _),

    % fast forward master with second
    fast_forward_branch(Foo_Descriptor, Bar_Descriptor, Applied_Commit_Ids),

    % check history
    Repo_Descriptor = (Foo_Descriptor.repository_descriptor),
    branch_head_commit(Repo_Descriptor, "master", Head_Commit_Uri),
    commit_uri_to_history_commit_ids(Repo_Descriptor, Head_Commit_Uri, History),
    History = [Commit_A, Commit_B, Commit_C],
    Applied_Commit_Ids = [Commit_B, Commit_C],


    commit_id_to_metadata(Repo_Descriptor, Commit_A, _, "commit a", _),
    commit_id_to_metadata(Repo_Descriptor, Commit_B, _, "commit b", _),
    commit_id_to_metadata(Repo_Descriptor, Commit_C, _, "commit c", _).

:- end_tests(fast_forward_api).
