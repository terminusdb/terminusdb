:- module(db_graph, [create_graph/5, delete_graph/5]).
:- use_module(core(util)).
:- use_module(core(transaction)).
:- use_module(core(query)).
:- use_module(core(account)).

create_graph(System_DB, Auth, Path, Commit_Info, Transaction_Metadata) :-

    do_or_die(
        resolve_absolute_string_descriptor_and_graph(Path, Branch_Descriptor, Graph),
        error(invalid_absolute_graph_descriptor(Path),_)),

    Graph_Type = (Graph.type),
    Graph_Name = (Graph.name),

    do_or_die(
        memberchk(Graph_Type, [instance, schema, inference]),
        error(bad_graph_type(Graph_Type),_)),

    do_or_die(
        (branch_descriptor{repository_descriptor:Repo_Descriptor, branch_name: Branch_Name} :< Branch_Descriptor),
        error(not_a_branch_descriptor(Branch_Descriptor),_)),

    do_or_die(
        askable_context(Repo_Descriptor, System_DB, Auth, Commit_Info, Context),
        error(unresolvable_absolute_descriptor(Repo_Descriptor),_)),

    check_descriptor_auth(System_DB, Repo_Descriptor,
                          system:commit_write_access, Auth),

    with_transaction(
        Context,
        (   % does this branch exist? if not, error
            do_or_die(has_branch(Context, Branch_Name),
                      error(branch_does_not_exist(Branch_Descriptor))),

            % does this branch already have a commit?
            (   branch_head_commit(Context, Branch_Name, Commit_Uri)
            % it does! collect graph objects we'll need to re-insert on a new commit
            ->  findall(Graph_Type0-Graph_Name0-Graph_Layer_Uri0,
                        (   graph_for_commit(Context, Commit_Uri, Graph_Type0, Graph_Name0, Graph_Uri0),
                            ignore(layer_uri_for_graph(Context, Graph_Uri0, Graph_Layer_Uri0))),
                        Graphs)
            % it doesn't! assume a single instance main graph
            ;   Graphs = ["instance"-"main"-_]),

            % does the graph exist already? if so, error
            do_or_die((\+memberchk(Graph_Type-Graph_Name-_, Graphs)),
                      error(graph_already_exists(Branch_Descriptor, Graph_Name))),

            % now that we know we're in a good position, create a new commit
            insert_commit_object_on_branch(Context,
                                           Branch_Name,
                                           New_Commit_Id,
                                           New_Commit_Uri),

            forall(member(Existing_Graph_Type-Existing_Graph_Name-Existing_Graph_Layer_Uri, Graphs),
                   insert_graph_object(Context,
                                       New_Commit_Uri,
                                       New_Commit_Id,
                                       Existing_Graph_Type,
                                       Existing_Graph_Name,
                                       Existing_Graph_Layer_Uri,
                                       _Graph_Uri)),

            insert_graph_object(Context,
                                New_Commit_Uri,
                                New_Commit_Id,
                                Graph_Type,
                                Graph_Name,
                                _,
                                _)

        ),
        Transaction_Metadata).

delete_graph(System_DB, Auth, Path, Commit_Info, Transaction_Metadata) :-
    do_or_die(
        resolve_absolute_string_descriptor_and_graph(Path, Branch_Descriptor, Graph),
        error(invalid_absolute_graph_descriptor(Path),_)),

    Graph_Type = (Graph.type),
    Graph_Name = (Graph.name),

    do_or_die(
        memberchk(Graph_Type, [instance, schema, inference]),
        error(bad_graph_type(Graph_Type),_)),

    do_or_die(
        (branch_descriptor{repository_descriptor:Repo_Descriptor, branch_name: Branch_Name} :< Branch_Descriptor),
        error(not_a_branch_descriptor(Branch_Descriptor),_)),

    do_or_die(
        askable_context(Repo_Descriptor, System_DB, Auth, Commit_Info, Context),
        error(unresolvable_absolute_descriptor(Repo_Descriptor),_)),

    check_descriptor_auth(System_DB, Repo_Descriptor,
                          system:commit_write_access, Auth),

    with_transaction(
        Context,
        (   % does this branch exist? if not, error
            do_or_die(has_branch(Context, Branch_Name),
                      error(branch_does_not_exist(Branch_Descriptor))),

            % does this branch already have a commit?
            (   branch_head_commit(Context, Branch_Name, Commit_Uri)
            % it does! collect graph objects we'll need to re-insert on a new commit
            ->  findall(Graph_Type0-Graph_Name0-Graph_Layer_Uri0,
                        (   graph_for_commit(Context, Commit_Uri, Graph_Type0, Graph_Name0, Graph_Uri0),
                            ignore(layer_uri_for_graph(Context, Graph_Uri0, Graph_Layer_Uri0))),
                        Graphs)
            % it doesn't! We can't be deleting it so.
            ;   throw(error(graph_does_not_exists(Branch_Descriptor, Graph_Name)))),

            % does the graph exist already? if not, error
            (   memberchk(Graph_Type-Graph_Name-_, Graphs)
            ->  true
            ;   throw(error(graph_does_not_exists(Branch_Descriptor, Graph_Name)))),

            % now that we know we're in a good position, create a new commit
            insert_commit_object_on_branch(Context,
                                           Branch_Name,
                                           New_Commit_Id,
                                           New_Commit_Uri),

            forall(
                (
                    member(Existing_Graph_Type-Existing_Graph_Name-Existing_Graph_Layer_Uri, Graphs),
                    \+ (Graph_Type-Graph_Name-_) = (Existing_Graph_Type-Existing_Graph_Name-Existing_Graph_Layer_Uri)
                ),
                insert_graph_object(Context,
                                    New_Commit_Uri,
                                    New_Commit_Id,
                                    Existing_Graph_Type,
                                    Existing_Graph_Name,
                                    Existing_Graph_Layer_Uri,
                                    _Graph_Uri)
            )
        ),
        Transaction_Metadata).



:- begin_tests(graph_creation).
:- use_module(core(transaction)).
:- use_module(core(util/test_utils)).
:- use_module(db_create).

test(create_graph_on_empty_branch,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "test"))),
      cleanup(teardown_temp_store(State))]
    ) :-
    make_branch_descriptor("admin", "test", Descriptor),

    super_user_authority(Auth),
    create_graph(system_descriptor{},
                 Auth,
                 "admin/test/local/branch/main/schema/main",
                 commit_info{author:"test",message:"test"},
                 _),

    open_descriptor(Descriptor, Transaction),

    [Single_Instance_Object] = Transaction.instance_objects,
    branch_graph{type: instance, name: "main"} :< Single_Instance_Object.descriptor,
    [Single_Schema_Object] = Transaction.schema_objects,
    branch_graph{type: schema, name: "main"} :< Single_Schema_Object.descriptor.


test(delete_graph_on_branch,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin","test"))),
      cleanup(teardown_temp_store(State))]
    )
:-
    make_branch_descriptor("admin", "test", Descriptor),

    super_user_authority(Auth),
    create_graph(system_descriptor{},
                 Auth,
                 "admin/test/local/branch/main/schema/main",
                 commit_info{author:"test",message:"test"},
                 _),

    delete_graph(system_descriptor{},
                 Auth,
                 "admin/test/local/branch/main/schema/main",
                 commit_info{author:"test",message:"test"},
                 _),

    open_descriptor(Descriptor, Transaction),
    [Single_Instance_Object] = Transaction.instance_objects,
    branch_graph{type: instance, name: "main"} :< Single_Instance_Object.descriptor,
    [] = Transaction.schema_objects.

:- end_tests(graph_creation).
