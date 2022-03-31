:- module(db_branch, [branch_create/5, branch_delete/3]).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(account)).
:- use_module(core(triple)).
:- use_module(core(document)).

:- use_module(library(terminus_store)).
:- use_module(core(transaction/validate), [commit_validation_object/2]).
:- use_module(library(plunit)).

create_schema(Repository_Context, New_Branch_Name, Branch_Uri, Schema, Prefixes) :-
    query_context_transaction_objects(Repository_Context, [Repository_Transaction]),
    Repository_Descriptor = (Repository_Transaction.descriptor),
    Database_Descriptor = (Repository_Descriptor.database_descriptor),
    atom_string(New_Branch_Name, New_Branch_Name_String),
    resolve_relative_descriptor(Repository_Descriptor, ["branch", New_Branch_Name_String], Branch_Descriptor),

    % let's create a cool new layer out of thin air but pretend it's actually a proper branch
    % We can't just open the descriptor, cause the branch doesn't yet exist in the commit graph.
    % Instead, we're gonna have to create a different sort of descriptor, and then bake a branch descriptor out of that.
    triple_store(Store),
    open_write(Store, Builder),

    Prototype_Descriptor = branch_graph{
              organization_name: (Database_Descriptor.organization_name),
              database_name: (Database_Descriptor.database_name),
              repository_name: (Repository_Descriptor.repository_name),
              branch_name: New_Branch_Name_String
          },
    Schema_Descriptor = (Prototype_Descriptor.put(type, schema)),
    Schema_RWO = read_write_obj{
                     descriptor: Schema_Descriptor,
                     read: _,
                     write: Builder},

    Commit_Info = commit_info{author: "system", message: "create initial schema", commit_type: 'InitialCommit'},

    Branch_Transaction = transaction_object{
                             parent: Repository_Transaction,
                             descriptor: Branch_Descriptor,
                             commit_info: Commit_Info,
                             instance_objects: [],
                             schema_objects: [Schema_RWO],
                             inference_objects: []
                         },

    create_context(Branch_Transaction, Query_Context),
    Prefix_Obj = (Prefixes.put('@type', "@context")),

    insert_context_document(Query_Context, Prefix_Obj),

    (   Schema = true
    ->  true
    ;   ask(Query_Context,
            insert('terminusdb://data/Schema', rdf:type, rdf:nil, schema))),


    transaction_objects_to_validation_objects([Branch_Transaction],
                                              [Branch_Validation]),
    validation_object{
        schema_objects: [Schema_Validation]
    } :< Branch_Validation,
    layer_to_id(Schema_Validation.read, Schema_Layer_Id),
    insert_initial_commit_object_on_branch(Repository_Transaction,
                                           Commit_Info,
                                           Branch_Uri,
                                           Commit_Uri),
    insert_layer_object(Repository_Transaction, Schema_Layer_Id, Schema_Layer_Uri),
    attach_layer_to_commit(Repository_Transaction, Commit_Uri, schema, Schema_Layer_Uri).

info_from_main(Repository_Context, Prefixes, Schema) :-
    query_context_transaction_objects(Repository_Context, [Repository_Transaction]),
    Repository_Descriptor = (Repository_Transaction.descriptor),

    resolve_relative_descriptor(Repository_Descriptor, ["branch", "main"], Branch_Descriptor),
    open_descriptor(Branch_Descriptor, commit_info{}, Branch_Transaction, [Repository_Transaction], _),

    database_prefixes(Branch_Transaction, Prefixes),
    (   is_schemaless(Branch_Transaction)
    ->  Schema = false
    ;   Schema = true).

branch_create_(Repository_Descriptor, empty(Input_Prefixes, Schema), New_Branch_Name, Branch_Uri) :-
    !,
    create_context(Repository_Descriptor, Context),

    info_from_main(Context, Main_Prefixes, Main_Schema),

    (   var(Input_Prefixes)
    ->  Prefixes = Main_Prefixes
    ;   Default_Prefixes = _{'@base': _Base, '@schema': _Schema},
        Default_Prefixes :< Main_Prefixes,
        Prefixes = Default_Prefixes.put(Input_Prefixes)),

    (   var(Schema)
    ->  Schema = Main_Schema
    ;   true),

    with_transaction(Context,
                     (   insert_branch_object(Context,
                                              New_Branch_Name,
                                              Branch_Uri),
                         create_schema(Context, New_Branch_Name, Branch_Uri, Schema, Prefixes)),
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

    % in this case, the source is a commit descriptor, but it could come from anywhere.
    % We'll have to copy over the commit information into the destination commit graph
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
branch_create(System_DB, Auth, Path, Origin_Option, Branch_Uri) :-

    do_or_die(
        resolve_absolute_string_descriptor(Path, Destination_Descriptor),
        error(invalid_target_absolute_path(Path),_)),

    do_or_die(
        (branch_descriptor{
             branch_name : New_Branch_Name,
             repository_descriptor : Repository_Descriptor
         } :< Destination_Descriptor),
        error(target_not_a_branch_descriptor(Destination_Descriptor),_)),

    (   Origin_Option = branch(Origin_Path)
    ->  do_or_die(
            resolve_absolute_string_descriptor(Origin_Path, Origin_Descriptor),
            error(invalid_origin_absolute_path(Origin_Path),_)),

        do_or_die(
            (   get_dict(repository_descriptor, Origin_Descriptor, Origin_Repository_Descriptor),
                get_dict(database_descriptor, Origin_Repository_Descriptor, Origin_Database_Descriptor),
                get_dict(database_name, Origin_Database_Descriptor, Database_Name),
                get_dict(organization_name, Origin_Database_Descriptor, Organization_Name)),
            error(source_not_a_valid_descriptor(Origin_Descriptor),_)),

        do_or_die(
            organization_database_name_uri(System_DB, Organization_Name, Database_Name, Scope_Iri),
            error(unknown_origin_database(Organization_Name, Database_Name),_))
    ;   Origin_Option = empty(_,_)
    ->  Origin_Descriptor = Origin_Option
    ;   throw(error(bad_origin_path_option(Origin_Option),_))),

    assert_auth_action_scope(System_DB, Auth, '@schema':'Action/branch', Scope_Iri),

    % ensure that we're putting this branch into a local repository
    do_or_die(
        repository_type(Repository_Descriptor.database_descriptor,
                        Repository_Descriptor.repository_name,
                        local),
        error(repository_is_not_local(Repository_Descriptor),_)),

    do_or_die(
        (\+ has_branch(Repository_Descriptor, New_Branch_Name)),
        error(branch_already_exists(New_Branch_Name),_)),

    do_or_die(
        branch_create_(Repository_Descriptor, Origin_Descriptor, New_Branch_Name, Branch_Uri),
        error(origin_cannot_be_branched(Origin_Descriptor),_)).

branch_delete_(Branch_Descriptor) :-
    Branch_Name = (Branch_Descriptor.branch_name),
    Repository_Descriptor = (Branch_Descriptor.repository_descriptor),
    create_context(Repository_Descriptor,Repository_Context),
    with_transaction(
        Repository_Context,
        (   branch_name_uri(Repository_Context, Branch_Name, Branch_Uri),
            delete_branch_object(Repository_Context,
                                 Branch_Uri)
        ),
        _).


/* delete a branch in the given repository.  */
branch_delete(System_DB, Auth, Path) :-

    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_target_absolute_path(Path),_)),

    do_or_die(
        branch_descriptor{
            branch_name : Branch_Name,
            repository_descriptor: Repository_Descriptor
        }:< Descriptor,
        error(not_a_branch_descriptor(Descriptor), _)),

    check_descriptor_auth(System_DB, Descriptor, '@schema':'Action/branch', Auth),

    do_or_die(
        has_branch(Repository_Descriptor, Branch_Name),
        error(branch_does_not_exist(Branch_Name),_)),

    branch_delete_(Descriptor).

:- begin_tests(branch_api, [concurrent(true)]).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).
:- use_module(core(triple)).

:- use_module(db_create).

test(create_branch_from_nothing_defaults_schemaless,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin","foo"))),
      cleanup(teardown_temp_store(State))
     ]
    ) :-

    Path = "admin/foo/local/branch/moo",
    super_user_authority(Auth),
    branch_create(system_descriptor{}, Auth, Path, empty(_,_), _),
    resolve_absolute_string_descriptor(Path, Descriptor),

    Repository_Descriptor = (Descriptor.repository_descriptor),

    has_branch(Repository_Descriptor, "moo"),
    branch_head_commit(Repository_Descriptor, "moo", Commit),
    commit_type(Repository_Descriptor, Commit, '@schema':'InitialCommit'),
    open_descriptor(Descriptor, Transaction),
    is_schemaless(Transaction).

test(create_branch_from_nothing_defaults_schema,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin","foo"))),
      cleanup(teardown_temp_store(State))
     ]
    ) :-

    Path = "admin/foo/local/branch/moo",
    super_user_authority(Auth),
    branch_create(system_descriptor{}, Auth, Path, empty(_,_), _),
    resolve_absolute_string_descriptor(Path, Descriptor),

    Repository_Descriptor = (Descriptor.repository_descriptor),

    has_branch(Repository_Descriptor, "moo"),
    branch_head_commit(Repository_Descriptor, "moo", Commit),
    commit_type(Repository_Descriptor, Commit, '@schema':'InitialCommit'),
    open_descriptor(Descriptor, Transaction),
    \+ is_schemaless(Transaction).

test(create_branch_from_nothing_half_configured,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin","foo"))),
      cleanup(teardown_temp_store(State))
     ]
    ) :-

    Path = "admin/foo/local/branch/moo",
    super_user_authority(Auth),
    branch_create(system_descriptor{}, Auth, Path, empty(_{'@schema': 'http://alternative.schema/', '@base': 'http://alternative.instance/'},_), _),
    resolve_absolute_string_descriptor(Path, Descriptor),

    Repository_Descriptor = (Descriptor.repository_descriptor),

    has_branch(Repository_Descriptor, "moo"),
    branch_head_commit(Repository_Descriptor, "moo", Commit),
    commit_type(Repository_Descriptor, Commit, '@schema':'InitialCommit'),
    open_descriptor(Descriptor, Transaction),
    is_schemaless(Transaction),
    database_prefixes(Transaction, Prefixes),
    _{'@base':"http://alternative.instance/",'@schema':"http://alternative.schema/"} :< Prefixes.

test(create_branch_from_nothing_configured,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin","foo"))),
      cleanup(teardown_temp_store(State))
     ]
    ) :-

    Path = "admin/foo/local/branch/moo",
    super_user_authority(Auth),
    branch_create(system_descriptor{}, Auth, Path, empty(_{'@schema': 'http://alternative.schema/', '@base': 'http://alternative.instance/'},true), _),
    resolve_absolute_string_descriptor(Path, Descriptor),

    Repository_Descriptor = (Descriptor.repository_descriptor),

    has_branch(Repository_Descriptor, "moo"),
    branch_head_commit(Repository_Descriptor, "moo", Commit),
    commit_type(Repository_Descriptor, Commit, '@schema':'InitialCommit'),
    open_descriptor(Descriptor, Transaction),
    \+ is_schemaless(Transaction),
    database_prefixes(Transaction, Prefixes),
    _{'@base':"http://alternative.instance/",'@schema':"http://alternative.schema/"} :< Prefixes.

test(create_branch_from_local_branch_with_commits,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin","foo"))),
      cleanup(teardown_temp_store(State))
     ]
    ) :-

    Origin_Branch_Path = "admin/foo/local/branch/main",
    resolve_absolute_string_descriptor(Origin_Branch_Path, Origin_Branch_Descriptor),
    Repository_Descriptor = (Origin_Branch_Descriptor.repository_descriptor),

    create_context(Origin_Branch_Descriptor, commit_info{author:"test", message:"first commit"}, Context1),
    with_transaction(Context1,
                     once(ask(Context1, insert(foo,bar,baz))),
                     _),
    create_context(Origin_Branch_Descriptor, commit_info{author:"test", message:"second commit"}, Context2),
    with_transaction(Context2,
                     once(ask(Context2, insert(baz,bar,foo))),
                     _),

    super_user_authority(Auth),
    Destination_Path = "admin/foo/local/branch/moo",
    branch_create(system_descriptor{}, Auth, Destination_Path, branch(Origin_Branch_Path),_),

    has_branch(Repository_Descriptor, "moo"),
    branch_head_commit(Repository_Descriptor, "main", Commit_Uri),
    branch_head_commit(Repository_Descriptor, "moo", Commit_Uri).

test(create_branch_from_remote_branch,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin","foo"),
             create_db_without_schema("admin","bar"))),
      cleanup(teardown_temp_store(State))
     ]
    ) :-
    Origin_Path = "admin/foo/local/branch/main",
    resolve_absolute_string_descriptor(Origin_Path, Origin_Branch_Descriptor),
    Origin_Repository_Descriptor = (Origin_Branch_Descriptor.repository_descriptor),

    create_context(Origin_Branch_Descriptor, commit_info{author:"test", message:"first commit"}, Context1),
    with_transaction(Context1,
                     once(ask(Context1, insert(foo,bar,baz))),
                     _),
    create_context(Origin_Branch_Descriptor, commit_info{author:"test", message:"second commit"}, Context2),
    with_transaction(Context2,
                     once(ask(Context2, insert(baz,bar,foo))),
                     _),

    Destination_Path = "admin/bar/local/branch/moo",
    super_user_authority(Auth),
    branch_create(system_descriptor{}, Auth, Destination_Path, branch(Origin_Path), _),

    resolve_absolute_string_descriptor(Destination_Path, Destination_Descriptor),
    Destination_Repository_Descriptor = (Destination_Descriptor.repository_descriptor),
    has_branch(Destination_Repository_Descriptor, "moo"),
    branch_head_commit(Origin_Repository_Descriptor, "main", Commit_Uri),
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
             create_db_without_schema("admin","foo"))),
      cleanup(teardown_temp_store(State))
     ]
    ) :-
    Origin_Path = "admin/foo/local/branch/main",
    resolve_absolute_string_descriptor(Origin_Path, Origin_Branch_Descriptor),
    Repository_Descriptor = (Origin_Branch_Descriptor.repository_descriptor),

    create_context(Origin_Branch_Descriptor, commit_info{author:"test", message:"first commit"}, Context1),
    with_transaction(Context1,
                     once(ask(Context1, insert(foo,bar,baz))),
                     _),
    create_context(Origin_Branch_Descriptor, commit_info{author:"test", message:"second commit"}, Context2),
    with_transaction(Context2,
                     once(ask(Context2, insert(baz,bar,foo))),
                     _),

    branch_head_commit(Repository_Descriptor, "main", Commit_Uri),
    commit_id_uri(Repository_Descriptor, Commit_Id, Commit_Uri),

    Commit_Descriptor = commit_descriptor{
                            repository_descriptor: Repository_Descriptor,
                            commit_id: Commit_Id
                        },
    resolve_absolute_string_descriptor(Commit_Path, Commit_Descriptor),

    Destination_Path = "admin/foo/local/branch/moo",
    super_user_authority(Auth),
    branch_create(system_descriptor{}, Auth, Destination_Path, branch(Commit_Path), _),

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
             create_db_without_schema("admin","foo"),
             create_db_without_schema("admin","bar"))),
      cleanup(teardown_temp_store(State))
     ]
    ) :-
    Origin_Path = "admin/foo/local/branch/main",
    resolve_absolute_string_descriptor(Origin_Path, Origin_Branch_Descriptor),
    Origin_Repository_Descriptor = (Origin_Branch_Descriptor.repository_descriptor),

    create_context(Origin_Branch_Descriptor, commit_info{author:"test", message:"first commit"}, Context1),
    with_transaction(Context1,
                     once(ask(Context1, insert(foo,bar,baz))),
                     _),
    create_context(Origin_Branch_Descriptor, commit_info{author:"test", message:"second commit"}, Context2),
    with_transaction(Context2,
                     once(ask(Context2, insert(baz,bar,foo))),
                     _),

    branch_head_commit(Origin_Repository_Descriptor, "main", Commit_Uri),
    commit_id_uri(Origin_Repository_Descriptor, Commit_Id, Commit_Uri),

    Commit_Descriptor = commit_descriptor{
                            repository_descriptor: Origin_Repository_Descriptor,
                            commit_id: Commit_Id
                        },
    resolve_absolute_string_descriptor(Commit_Path, Commit_Descriptor),
    Destination_Path = "admin/bar/local/branch/moo",

    super_user_authority(Auth),
    branch_create(system_descriptor{}, Auth, Destination_Path, branch(Commit_Path), _),

    resolve_absolute_string_descriptor(Destination_Path, Destination_Descriptor),
    Destination_Repository_Descriptor = (Destination_Descriptor.repository_descriptor),

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

:- use_module(core(document)).
test(create_empty_branch_and_insert_schema_doc,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin","foo"))),
      cleanup(teardown_temp_store(State))
     ]
    ) :-
    super_user_authority(Auth),
    branch_create(system_descriptor{}, Auth, "admin/foo/local/branch/new_empty_branch", empty(_,_), _),

    resolve_absolute_string_descriptor("admin/foo/local/branch/new_empty_branch", D),

    with_test_transaction(D,
                          Context,
                          insert_schema_document(Context,
                                                 _{
                                                     '@type': "Class",
                                                     '@id': "Thing"
                                                 })).

:- end_tests(branch_api).
