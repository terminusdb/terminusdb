:- module(db_branch, [branch_create/4]).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).

branch_create_(Repository_Descriptor, empty, New_Branch_Name, Branch_Uri) :-
    !,
    % easy! just create the branch object and nothing else
    create_context(Repository_Descriptor, Context),
    with_transaction(Context,
                     insert_branch_object(Context,
                                          New_Branch_Name,
                                          Branch_Uri),
                     _).
branch_create_(Repository_Descriptor, Branch_Descriptor, New_Branch_Name, Branch_Uri) :-
    branch_descriptor{} :< Branch_Descriptor,
    Repository_Descriptor = Branch_Descriptor.repository_descriptor,
    !,

    (   has_branch(Repository_Descriptor, Branch_Descriptor.branch_name)
    ->  true
    ;   throw(error(origin_branch_does_not_exist(Branch_Descriptor.branch_name)))),

    % In this case, both source and destination branch exist in the same repository.
    % A new branch is trivial to create by inserting the branch object and pointing it at the head of the other branch.
    create_context(Repository_Descriptor, Context),
    with_transaction(Context,
                     (   insert_branch_object(Context,
                                              New_Branch_Name,
                                              Branch_Uri),
                         (   branch_head_commit(Context, Branch_Descriptor.branch_name, Head_Commit_Uri)
                         ->  link_commit_object_to_branch(Context, Branch_Uri, Head_Commit_Uri)
                         ;   true)),
                     _).
branch_create_(Repository_Descriptor, Branch_Descriptor, New_Branch_Name, Branch_Uri) :-
    branch_descriptor{} :< Branch_Descriptor,
    Repository_Descriptor \= Branch_Descriptor.repository_descriptor,
    !,

    (   has_branch(Branch_Descriptor.repository_descriptor, Branch_Descriptor.branch_name)
    ->  true
    ;   throw(error(origin_branch_does_not_exist(Branch_Descriptor.branch_name)))),

    % in this case, the source is a branch descriptor, but it could come from anywhere.
    % We'll have to copy over the commit onformation into the destination commit graph
    create_context(Branch_Descriptor.repository_descriptor, Origin_Context),
    context_default_prefixes(Origin_Context, Origin_Context_Defaults),
    create_context(Repository_Descriptor, Destination_Context),
    with_transaction(Destination_Context,
                     (   insert_branch_object(Destination_Context,
                                              New_Branch_Name,
                                              Branch_Uri),
                         (   branch_head_commit(Origin_Context_Defaults, Branch_Descriptor.branch_name, Head_Commit_Uri)
                         ->  commit_id_uri(Origin_Context_Defaults, Head_Commit_Id, Head_Commit_Uri),
                             copy_commits(Origin_Context_Defaults, Destination_Context, Head_Commit_Id),
                             link_commit_object_to_branch(Destination_Context, Branch_Uri, Head_Commit_Uri)
                         ;   true)),
                     _).
branch_create_(Repository_Descriptor, Commit_Descriptor, New_Branch_Name, Branch_Uri) :-
    commit_descriptor{repository_descriptor:Origin_Repository_Descriptor} :< Commit_Descriptor,
    Origin_Repository_Descriptor = Repository_Descriptor,
    !,

    create_context(Repository_Descriptor, Context),
    with_transaction(Context,
                     (   (   commit_id_uri(Context, Commit_Descriptor.commit_id, Commit_Uri)
                         ->  true
                         ;   throw(error(origin_commit_does_not_exist(Commit_Descriptor.commit_id)))),

                         (   insert_branch_object(Context,
                                                  New_Branch_Name,
                                                  Branch_Uri),
                             link_commit_object_to_branch(Context, Branch_Uri, Commit_Uri))),
                     _).
branch_create_(Destination_Repository_Descriptor, Commit_Descriptor, New_Branch_Name, Branch_Uri) :-
    commit_descriptor{repository_descriptor:Origin_Repository_Descriptor} :< Commit_Descriptor,

    % in this case, the source is a branch descriptor, but it could come from anywhere.
    % We'll have to copy over the commit onformation into the destination commit graph
    create_context(Origin_Repository_Descriptor, Origin_Context),
    context_default_prefixes(Origin_Context, Origin_Context_Defaults),

    (   commit_id_uri(Origin_Context_Defaults, Commit_Descriptor.commit_id, Commit_Uri)
    ->  true
    ;   throw(error(origin_commit_does_not_exist(Commit_Descriptor.commit_id)))),

    create_context(Destination_Repository_Descriptor, Destination_Context),
    with_transaction(Destination_Context,
                     (   insert_branch_object(Destination_Context,
                                              New_Branch_Name,
                                              Branch_Uri),
                         copy_commits(Origin_Context_Defaults, Destination_Context, Commit_Descriptor.commit_id),
                         link_commit_object_to_branch(Destination_Context, Branch_Uri, Commit_Uri)),
                     _).

/* create a branch in the given repository. Origin is the thing we wish to create a branch out of. it can be any kind of branch descriptor or commit descriptor. branch name is the name of the new branch. options contain a branch default prefix thingie. */
branch_create(Repository_Descriptor, Origin_Descriptor, New_Branch_Name, Branch_Uri) :-
    % ensure that we're putting this branch into a local repository
    (   repository_type(Repository_Descriptor.database_descriptor,
                        Repository_Descriptor.repository_name,
                        local)
    ->  true
    ;   throw(error(repository_is_not_local(Repository_Descriptor)))),

    (   has_branch(Repository_Descriptor, New_Branch_Name)
    ->  throw(error(branch_already_exists(New_Branch_Name)))
    ;   true),

    (   branch_create_(Repository_Descriptor, Origin_Descriptor, New_Branch_Name, Branch_Uri)
    ->  true
    ;   throw(error(origin_cannot_be_branched(Origin_Descriptor)))).

:- begin_tests(branch_api).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).
:- use_module(core(triple)).

:- use_module(db_create).

test(create_branch_from_nothing,
     [setup((setup_temp_store(State),
             create_db_without_schema('user|foo','test','a test'))),
      cleanup(teardown_temp_store(State))
     ]
    ) :-
    Repository_Descriptor = repository_descriptor{
                                database_descriptor: database_descriptor{
                                                         database_name: 'user|foo'
                                                     },
                                repository_name: "local"
                            },
    branch_create(Repository_Descriptor, empty, "moo", _),

    has_branch(Repository_Descriptor, "moo"),
    \+ branch_head_commit(Repository_Descriptor, "moo", _).

test(create_branch_from_local_branch_with_commits,
     [setup((setup_temp_store(State),
             create_db_without_schema('user|foo','test','a test'))),
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

    branch_create(Repository_Descriptor, Origin_Branch_Descriptor, "moo", _),

    has_branch(Repository_Descriptor, "moo"),
    branch_head_commit(Repository_Descriptor, "master", Commit_Uri),
    branch_head_commit(Repository_Descriptor, "moo", Commit_Uri).

test(create_branch_from_remote_branch,
     [setup((setup_temp_store(State),
             create_db_without_schema('user|foo','test','a test'),
             create_db_without_schema('user|bar','test','a test'))),
      cleanup(teardown_temp_store(State))
     ]
    ) :-
    Origin_Repository_Descriptor = repository_descriptor{
                                database_descriptor: database_descriptor{
                                                         database_name: 'user|foo'
                                                     },
                                repository_name: "local"
                            },
    Origin_Branch_Descriptor = branch_descriptor{
                                   repository_descriptor: Origin_Repository_Descriptor,
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

    Destination_Repository_Descriptor = repository_descriptor{
                                            database_descriptor: database_descriptor{
                                                                     database_name: 'user|bar'
                                                                 },
                                            repository_name: "local"
                                        },
    branch_create(Destination_Repository_Descriptor, Origin_Branch_Descriptor, "moo", _),

    has_branch(Destination_Repository_Descriptor, "moo"),
    branch_head_commit(Origin_Repository_Descriptor, "master", Commit_Uri),
    create_context(Origin_Repository_Descriptor, Origin_Context),
    prefixed_to_uri(Commit_Uri, Origin_Context.prefixes, Commit_Uri_Unprefixed),
    branch_head_commit(Destination_Repository_Descriptor, "moo", Commit_Uri_Unprefixed),

    Branch_Descriptor = branch_descriptor{
                            repository_descriptor: Destination_Repository_Descriptor,
                            branch_name: "moo"
                        },
    once(ask(Branch_Descriptor,
             (   t(foo, bar, baz),
                 t(baz, bar, foo)))).

test(create_branch_from_local_commit,
     [setup((setup_temp_store(State),
             create_db_without_schema('user|foo','test','a test'))),
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

    branch_head_commit(Repository_Descriptor, "master", Commit_Uri),
    commit_id_uri(Repository_Descriptor, Commit_Id, Commit_Uri),

    Commit_Descriptor = commit_descriptor{
                            repository_descriptor: Repository_Descriptor,
                            commit_id: Commit_Id
                        },

    branch_create(Repository_Descriptor, Commit_Descriptor, "moo", _),

    has_branch(Repository_Descriptor, "moo"),
    branch_head_commit(Repository_Descriptor, "moo", Commit_Uri),

    Branch_Descriptor = branch_descriptor{
                            repository_descriptor: Repository_Descriptor,
                            branch_name: "moo"
                        },

    once(ask(Branch_Descriptor,
             (   t(foo, bar, baz),
                 t(baz, bar, foo)))).

test(create_branch_from_remote_commit,
     [setup((setup_temp_store(State),
             create_db_without_schema('user|foo','test','a test'),
             create_db_without_schema('user|bar','test','a test'))),
      cleanup(teardown_temp_store(State))
     ]
    ) :-
    Origin_Repository_Descriptor = repository_descriptor{
                                database_descriptor: database_descriptor{
                                                         database_name: 'user|foo'
                                                     },
                                repository_name: "local"
                            },
    Origin_Branch_Descriptor = branch_descriptor{
                                   repository_descriptor: Origin_Repository_Descriptor,
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

    Destination_Repository_Descriptor = repository_descriptor{
                                            database_descriptor: database_descriptor{
                                                                     database_name: 'user|bar'
                                                                 },
                                            repository_name: "local"
                                        },

    branch_head_commit(Origin_Repository_Descriptor, "master", Commit_Uri),
    commit_id_uri(Origin_Repository_Descriptor, Commit_Id, Commit_Uri),

    Commit_Descriptor = commit_descriptor{
                            repository_descriptor: Origin_Repository_Descriptor,
                            commit_id: Commit_Id
                        },

    branch_create(Destination_Repository_Descriptor, Commit_Descriptor, "moo", _),

    has_branch(Destination_Repository_Descriptor, "moo"),

    create_context(Origin_Repository_Descriptor, Origin_Context),
    prefixed_to_uri(Commit_Uri, Origin_Context.prefixes, Commit_Uri_Unprefixed),
    branch_head_commit(Destination_Repository_Descriptor, "moo", Commit_Uri_Unprefixed),

    Branch_Descriptor = branch_descriptor{
                            repository_descriptor: Destination_Repository_Descriptor,
                            branch_name: "moo"
                        },
    once(ask(Branch_Descriptor,
             (   t(foo, bar, baz),
                 t(baz, bar, foo)))).
:- end_tests(branch_api).
