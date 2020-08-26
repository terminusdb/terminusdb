:- module(api_optimize, [api_optimize/3]).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(account)).
:- use_module(library(terminus_store)).
:- use_module(core(util/test_utils)).
:- use_module(core(triple)).

% Does some crazy-magic unspecified optimizations
api_optimize(SystemDB, Auth, Path) :-

    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_absolute_path(Path),_)),

    do_or_die(
        (   system_descriptor{} :< Descriptor,
            do_or_die(is_super_user(Auth),
                      error(requires_super_user,_))
        ;   database_descriptor{} :< Descriptor,
            check_descriptor_auth(SystemDB, Descriptor,
                                  system:meta_write_access, Auth)
        ;   repository_descriptor{} :< Descriptor,
            check_descriptor_auth(SystemDB, Descriptor,
                                  system:commit_write_access, Auth)
        ),
        error(not_a_valid_descriptor_for_optimization(Descriptor),_)),

    descriptor_optimize(Descriptor).

descriptor_optimize(system_descriptor{}) :-
    open_descriptor(system_descriptor{}, Transaction_Object),
    [Instance] = (Transaction_Object.instance_objects),
    Layer = (Instance.read),
    squash(Layer,New_Layer),
    storage(Store),
    system_instance_name(Graph_Name),
    safe_open_named_graph(Store, Graph_Name, Graph),
    nb_force_set_head(Graph,New_Layer).
descriptor_optimize(database_descriptor{
                        organization_name : Organization_Name,
                        database_name : Database_Name
                    }) :-
    Descriptor = database_descriptor{
                     organization_name : Organization_Name,
                     database_name : Database_Name
                 },
    open_descriptor(Descriptor, Transaction_Object),
    [Instance] = (Transaction_Object.instance_objects),
    Layer = (Instance.read),
    squash(Layer,New_Layer),
    storage(Store),
    organization_database_name(Organization_Name,Database_Name,Composite),
    safe_open_named_graph(Store, Composite, Graph),
    nb_force_set_head(Graph,New_Layer).
descriptor_optimize(repository_descriptor{
                        database_descriptor : Database_Descriptor,
                        repository_name : Repository_Name
                    }) :-
    Descriptor = repository_descriptor{
                     database_descriptor : Database_Descriptor,
                     repository_name : Repository_Name
                 },

    open_descriptor(Descriptor, Transaction_Object),
    [Instance] = (Transaction_Object.instance_objects),
    Layer = (Instance.read),
    squash(Layer,New_Layer),
    layer_to_id(New_Layer,Layer_Id),
    /* Do something special... что делать! */
    create_context(Database_Descriptor, Context),
    with_transaction(
        Context,
        update_repository_head(Context, Repository_Name, Layer_Id),
        _
    ).

:- begin_tests(optimize).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).

test(optimize_system,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "testdb"),
             create_db_without_schema("admin", "testdb2")
            )),
      cleanup(teardown_temp_store(State))]
    ) :-

    super_user_authority(Auth),
    api_optimize(system_descriptor{}, Auth, "_system"),

    open_descriptor(system_descriptor{}, Transaction),
    [Instance_Object] = (Transaction.instance_objects),
    Layer = (Instance_Object.read),

    % No parent
    \+ parent(Layer,_),

    database_exists("admin","testdb"),
    database_exists("admin","testdb2").


test(optimize_repo,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State))]
    ) :-

    Path = 'admin/testdb',
    resolve_absolute_string_descriptor(Path,Descriptor),
    Repository_Descriptor = (Descriptor.repository_descriptor),

    super_user_authority(Auth),

    askable_context(Descriptor, system_descriptor{}, Auth,
                    commit_info{ author : "me",
                                 message : "commit 1" },
                    Context),

    with_transaction(
        Context,
        ask(Context,
            insert(a,b,c)),
        _),

    askable_context(Descriptor, system_descriptor{}, Auth,
                    commit_info{ author : "me",
                                 message : "commit 2" },
                    Context2),

    with_transaction(
        Context2,
        ask(Context2,
            insert(e,f,g)),
        _),

    resolve_absolute_string_descriptor(Repo_Path,Repository_Descriptor),

    api_optimize(system_descriptor{}, Auth, Repo_Path),

    open_descriptor(Repository_Descriptor, Transaction),
    [Instance_Object] = (Transaction.instance_objects),
    Layer = (Instance_Object.read),
    % No parent
    \+ parent(Layer,_),

    has_branch(Repository_Descriptor, "main"),
    findall(X-Y-Z,
            ask(Descriptor,
                t(X,Y,Z)),
            Triples),
    sort(Triples,Sorted),
    sort([a-b-c,e-f-g],Correct),
    ord_seteq(Sorted, Correct).


test(optimize_db,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State))]
    ) :-

    Path = 'admin/testdb',
    resolve_absolute_string_descriptor(Path,Descriptor),
    Repository_Descriptor = (Descriptor.repository_descriptor),
    Database_Descriptor = (Repository_Descriptor.database_descriptor),
    super_user_authority(Auth),

    askable_context(Descriptor, system_descriptor{}, Auth,
                    commit_info{ author : "me",
                                 message : "commit 1" },
                    Context),

    with_transaction(
        Context,
        ask(Context,
            insert(a,b,c)),
        _),

    askable_context(Descriptor, system_descriptor{}, Auth,
                    commit_info{ author : "me",
                                 message : "commit 2" },
                    Context2),

    with_transaction(
        Context2,
        ask(Context2,
            insert(e,f,g)),
        _),

    resolve_absolute_string_descriptor(Database_Path,Database_Descriptor),

    api_optimize(system_descriptor{}, Auth, Database_Path),

    open_descriptor(Database_Descriptor, Transaction),
    [Instance_Object] = (Transaction.instance_objects),
    Layer = (Instance_Object.read),
    % No parent
    \+ parent(Layer,_),

    once(has_repository(Database_Descriptor, "local")),

    findall(X-Y-Z,
            ask(Descriptor,
                t(X,Y,Z)),
            Triples),
    sort(Triples,Sorted),
    sort([a-b-c,e-f-g],Correct),
    ord_seteq(Sorted, Correct).

:- end_tests(optimize).
