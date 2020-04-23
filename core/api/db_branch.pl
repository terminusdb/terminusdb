:- module(db_branch, [branch_create/5]).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).

branch_create_(Repository_Descriptor, empty, New_Branch_Name, Options, Branch_Uri) :-
    !,
    % easy! just create the branch object and nothing else
    (   memberchk(base_uri(Base_Uri), Options)
    ->  create_context(Repository_Descriptor, Context),
        with_transaction(Context,
                         insert_branch_object(Context,
                                              New_Branch_Name,
                                              Base_Uri,
                                              Branch_Uri),
                         _)
    ;   throw(error(branch_creation_base_uri_not_specified))).
branch_create_(Repository_Descriptor, Branch_Descriptor, New_Branch_Name, Options, Branch_Uri) :-
    branch_descriptor{} :< Branch_Descriptor,
    Repository_Descriptor = Branch_Descriptor.repository_descriptor,
    !,
    % In this case, both source and destination branch exist in the same repository.
    % A new branch is trivial to create by inserting the branch object and pointing it at the head of the other branch.
    create_context(Repository_Descriptor, Context),
    with_transaction(Context,
                     (   (   memberchk(base_uri(Base_Uri), Options)
                         ->  true
                         ;   branch_base_uri(Context, Branch_Descriptor.branch_name, Base_Uri)),
                         insert_branch_object(Context,
                                              New_Branch_Name,
                                              Base_Uri,
                                              Branch_Uri),
                         (   branch_head_commit(Context, Branch_Descriptor.branch_name, Head_Commit_Uri)
                         ->  link_commit_object_to_branch(Context, Branch_Uri, Head_Commit_Uri)
                         ;   true)),
                     _).
branch_create_(Repository_Descriptor, Branch_Descriptor, New_Branch_Name, Options, Branch_Uri) :-
    branch_descriptor{} :< Branch_Descriptor,
    Repository_Descriptor \= Branch_Descriptor.repository_descriptor,
    !,
    % in this case, the source is a branch descriptor, but it could come from anywhere.
    % We'll have to copy over the commit onformation into the destination commit graph
    throw(error(not_implemented)),

    true.
branch_create_(Repository_Descriptor, Commit_Descriptor, New_Branch_Name, Options, Branch_Uri) :-
    commit_descriptor{} :< Commit_Descriptor,
    % easy! make a new branch object and point it at the given commit
    throw(error(not_implemented)),

    true.


/* create a branch in the given repository. Origin is the thing we wish to create a branch out of. it can be any kind of branch descriptor or commit descriptor. branch name is the name of the new branch. options contain a branch default prefix thingie. */
branch_create(Repository_Descriptor, Origin_Descriptor, New_Branch_Name, Options, Branch_Uri) :-
    % ensure that we're putting this branch into a local repository
    (   repository_type(Repository_Descriptor.database_descriptor,
                        Repository_Descriptor.repository_name,
                        local)
    ->  true
    ;   throw(error(repository_is_not_local(Repository_Descriptor)))),

    branch_create_(Repository_Descriptor, Origin_Descriptor, New_Branch_Name, Options, Branch_Uri).

:- begin_tests(branch_api).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).
:- use_module(db_init).
test(create_branch_from_nothing_without_uri_produces_error,
     [setup((setup_temp_store(State),
             create_db('user|foo','test','a test', 'terminus://blah'))),
      cleanup(teardown_temp_store(State)),
      throws(error(branch_creation_base_uri_not_specified))]
    ) :-
    Repository_Descriptor = repository_descriptor{
                                database_descriptor: database_descriptor{
                                                         database_name: 'user|foo'
                                                     },
                                repository_name: "local"
                            },
    branch_create(Repository_Descriptor, empty, "moo", [], _).

test(create_branch_from_nothing,
     [setup((setup_temp_store(State),
             create_db('user|foo','test','a test', 'terminus://blah'))),
      cleanup(teardown_temp_store(State))
     ]
    ) :-
    Repository_Descriptor = repository_descriptor{
                                database_descriptor: database_descriptor{
                                                         database_name: 'user|foo'
                                                     },
                                repository_name: "local"
                            },
    branch_create(Repository_Descriptor, empty, "moo", [base_uri('http://flurps/flarg')], _),

    has_branch(Repository_Descriptor, "moo"),
    branch_base_uri(Repository_Descriptor, "moo", "http://flurps/flarg"),
    \+ branch_head_commit(Repository_Descriptor, "moo", _).

test(create_branch_from_local_empty_branch_copying_base_uri,
     [setup((setup_temp_store(State),
             create_db('user|foo','test','a test', 'terminus://blah'))),
      cleanup(teardown_temp_store(State))
     ]
    ) :-
    Repository_Descriptor = repository_descriptor{
                                database_descriptor: database_descriptor{
                                                         database_name: 'user|foo'
                                                     },
                                repository_name: "local"
                            },
    Origin_Branch_Descriptor = branch_descriptor{
                                   repository_descriptor: Repository_Descriptor,
                                   branch_name: "master"
                               },

    branch_create(Repository_Descriptor, Origin_Branch_Descriptor, "moo", [], _),

    has_branch(Repository_Descriptor, "moo"),
    branch_base_uri(Repository_Descriptor, "moo", "terminus://blah"),
    \+ branch_head_commit(Repository_Descriptor, "moo", _).

test(create_branch_from_local_empty_branch_overriding_base_uri,
     [setup((setup_temp_store(State),
             create_db('user|foo','test','a test', 'terminus://blah'))),
      cleanup(teardown_temp_store(State))
     ]
    ) :-
    Repository_Descriptor = repository_descriptor{
                                database_descriptor: database_descriptor{
                                                         database_name: 'user|foo'
                                                     },
                                repository_name: "local"
                            },
    Origin_Branch_Descriptor = branch_descriptor{
                                   repository_descriptor: Repository_Descriptor,
                                   branch_name: "master"
                               },

    branch_create(Repository_Descriptor, Origin_Branch_Descriptor, "moo", [base_uri('http://flurps/flarg')], _),

    has_branch(Repository_Descriptor, "moo"),
    branch_base_uri(Repository_Descriptor, "moo", "http://flurps/flarg"),
    \+ branch_head_commit(Repository_Descriptor, "moo", _).

test(create_branch_from_local_branch_with_commits,
     [setup((setup_temp_store(State),
             create_db('user|foo','test','a test', 'terminus://blah'))),
      cleanup(teardown_temp_store(State))
     ]
    ) :-
    Repository_Descriptor = repository_descriptor{
                                database_descriptor: database_descriptor{
                                                         database_name: 'user|foo'
                                                     },
                                repository_name: "local"
                            },
    Origin_Branch_Descriptor = branch_descriptor{
                                   repository_descriptor: Repository_Descriptor,
                                   branch_name: "master"
                               },

    create_context(Origin_Branch_Descriptor, commit_info{author:"test", message:"first commit"}, Context1),
    with_transaction(Context1,
                     once(ask(Context1, insert(foo,bar,baz))),
                     _),
    create_context(Origin_Branch_Descriptor, commit_info{author:"test", message:"second commit"}, Context2),
    with_transaction(Context2,
                     once(ask(Context2, insert(baz,bar,foo))),
                     _),

    branch_create(Repository_Descriptor, Origin_Branch_Descriptor, "moo", [base_uri('http://flurps/flarg')], _),

    has_branch(Repository_Descriptor, "moo"),
    branch_base_uri(Repository_Descriptor, "moo", "http://flurps/flarg"),
    branch_head_commit(Repository_Descriptor, "master", Commit_Uri),
    branch_head_commit(Repository_Descriptor, "moo", Commit_Uri).


:- end_tests(branch_api).
