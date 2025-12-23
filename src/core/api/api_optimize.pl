:- module(api_optimize, [api_optimize/3]).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(account)).
:- use_module(library(terminus_store)).
:- use_module(library(lists)).
:- use_module(library(plunit)).
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
                                  '@schema':'Action/meta_write_access', Auth)
        ;   repository_descriptor{} :< Descriptor,
            check_descriptor_auth(SystemDB, Descriptor,
                                  '@schema':'Action/meta_write_access', Auth)
        ;   branch_descriptor{} :< Descriptor,
            check_descriptor_auth(SystemDB, Descriptor,
                                  '@schema':'Action/meta_write_access', Auth)
        ),
        error(not_a_valid_descriptor_for_optimization(Descriptor),_)),

    descriptor_optimize(Descriptor).

named_graph_optimize(Graph_Name) :-
    storage(Store),
    safe_open_named_graph(Store,Graph_Name,Graph),
    (   head(Graph, Layer, Version)
    ->  squash(Layer,New_Layer),
        do_or_die(
            nb_force_set_head(Graph,New_Layer,Version),
            error(label_version_changed(Graph_Name,Version),_))
    ;   true).

descriptor_optimize(system_descriptor{}) :-
    system_instance_name(Graph_Name),
    named_graph_optimize(Graph_Name).
descriptor_optimize(database_descriptor{
                        organization_name : Organization_Name,
                        database_name : Database_Name
                    }) :-
    organization_database_name(Organization_Name,Database_Name,Composite),
    named_graph_optimize(Composite).
descriptor_optimize(repository_descriptor{
                        database_descriptor : Database_Descriptor,
                        repository_name : Repository_Name
                    }) :-
    Descriptor = repository_descriptor{
                     database_descriptor : Database_Descriptor,
                     repository_name : Repository_Name
                 },

    open_descriptor(Descriptor, Transaction_Object),
    storage(Store),
    % Optimize in-memory repository schema layers
    Schema_Objects = (Transaction_Object.schema_objects),
    forall(
        (   member(Schema, Schema_Objects),
            Layer = (Schema.read),
            ground(Layer)
        ),
        (   rollup(Layer),
            layer_to_id(Layer, Schema_Layer_Id),
            terminus_store:invalidate_layer_cache_entry(Store, Schema_Layer_Id),
            store_id_layer(Store, Schema_Layer_Id, Reloaded_Schema_Layer),
            Schema_Desc = (Schema.descriptor),
            replace_graph_head_if_named(Store, Schema_Desc, Reloaded_Schema_Layer)
        )
    ),
    % Optimize in-memory commit graph after rollup
    [Instance] = (Transaction_Object.instance_objects),
    Layer = (Instance.read),
    (   ground(Layer)
    ->  exponential_rollup_strategy(Layer),
        layer_to_id(Layer, Layer_Id),
        terminus_store:invalidate_layer_cache_entry(Store, Layer_Id),
        store_id_layer(Store, Layer_Id, Reloaded_Layer),
        Desc = (Instance.descriptor),
        replace_graph_head_if_named(Store, Desc, Reloaded_Layer)
    ;   true
    ),
    % Clear caches
    retractall(descriptor:retained_descriptor_layers(Descriptor, _)),
    abolish_all_tables.
descriptor_optimize(branch_descriptor{
                        repository_descriptor : Repository_Descriptor,
                        branch_name : Branch_Name
                    }) :-
    Descriptor = branch_descriptor{
                        repository_descriptor : Repository_Descriptor,
                        branch_name : Branch_Name
                    },

    open_descriptor(Descriptor, Transaction_Object),
    storage(Store),
    Schema_Objects = (Transaction_Object.schema_objects),
    forall(
        (   member(Schema, Schema_Objects),
            Layer = (Schema.read),
            ground(Layer)
        ),
        (   rollup(Layer),
            % Apply same invalidate+reload+set_head fix to schema layers
            layer_to_id(Layer, Schema_Layer_Id),
            terminus_store:invalidate_layer_cache_entry(Store, Schema_Layer_Id),
            store_id_layer(Store, Schema_Layer_Id, Reloaded_Schema_Layer),
            Schema_Desc = (Schema.descriptor),
            replace_graph_head_if_named(Store, Schema_Desc, Reloaded_Schema_Layer)
        )
    ),
    Instance_Objects = (Transaction_Object.instance_objects),
    forall(
        (   member(Instance, Instance_Objects),
            Layer = (Instance.read),
            ground(Layer)
        ),
        (   exponential_rollup_strategy(Layer),
            % Invalidate the ENTIRE parent chain cache by triggering register_rollup.
            % imprecise_rollup_upto(Layer, Layer) is a no-op for rollup creation,
            % but register_rollup still runs and invalidates the full parent chain.
            % This is necessary because exponential_rollup_strategy only invalidates
            % chains for layers it rolls up, not the full chain of the topmost layer.
            imprecise_rollup_upto(Layer, Layer),
            % Reload to get the layer with rolled-up parents
            layer_to_id(Layer, Layer_Id),
            store_id_layer(Store, Layer_Id, Reloaded_Layer),
            % Replace the head with the reloaded (flattened) layer
            Desc = (Instance.descriptor),
            replace_graph_head_if_named(Store, Desc, Reloaded_Layer)
        )
    ),
    % Clear caches that hold references to old deep-chain layers
    retractall(descriptor:retained_descriptor_layers(Descriptor, _)),
    abolish_all_tables,
    % Also roll up the repository layer (commit graph) to prevent O(n) traversal.
    % The commit graph accumulates child layers with each commit, and
    % triple_exists() traverses the ENTIRE parent chain on each query.
    open_descriptor(Repository_Descriptor, Repo_Transaction),
    [Repo_Instance] = (Repo_Transaction.instance_objects),
    Repo_Layer = (Repo_Instance.read),
    (   ground(Repo_Layer)
    ->  rollup(Repo_Layer),
        % Invalidate cache and reload from disk to get flattened version
        layer_to_id(Repo_Layer, Repo_Layer_Id),
        storage(Store),
        terminus_store:invalidate_layer_cache_entry(Store, Repo_Layer_Id),
        store_id_layer(Store, Repo_Layer_Id, Reloaded_Repo_Layer),
        % Replace the repo head with the reloaded flattened layer
        Repo_Desc = (Repo_Instance.descriptor),
        replace_graph_head_if_named(Store, Repo_Desc, Reloaded_Repo_Layer),
        % Clear retained layers for all descriptors in the chain
        retractall(descriptor:retained_descriptor_layers(Repository_Descriptor, _)),
        retractall(descriptor:retained_descriptor_layers(Descriptor, _)),
        % Also clear tables that cache predicate results keyed by layers
        abolish_all_tables
    ;   true).

% Helper predicate: Replace named graph head with reloaded layer
% Handles different descriptor types that map to named graphs
replace_graph_head_if_named(Store, Desc, Layer) :-
    (   get_dict(name, Desc, Graph_Name)
    ->  % labelled_graph, system_graph with name
        safe_open_named_graph(Store, Graph_Name, Graph),
        nb_set_head(Graph, Layer)
    ;   repo_graph{organization_name: Org, database_name: DB, type: instance} :< Desc
    ->  % repo_graph instance uses composite name
        organization_database_name(Org, DB, Composite),
        safe_open_named_graph(Store, Composite, Graph),
        nb_set_head(Graph, Layer)
    ;   commit_graph{type: schema} :< Desc
    ->  % commit_graph schema uses ref_ontology
        ref_ontology(Ref_Name),
        safe_open_named_graph(Store, Ref_Name, Graph),
        nb_set_head(Graph, Layer)
    ;   % branch_graph, commit_graph instance - no direct named graph head replacement
        % Layer is referenced via commit objects, cache invalidation handles this
        true
    ).


% Write a generator that gives us back Start and Stop positions as commit_ids
% based on a heuristic of exponential rollup
%
% i.e. r layers without a rollup needs a rollup
%      r rollups needs a rollup, etc.
%
/*

Example with r = 3

      .
     / \
    /   \
   /     \
  .   .   .   .   .
 / \ / \ / \ / \ / \
 ... ... ... ... ...  ..

Sizes: [ Commit1_ID-9, Commit2_ID-3, Commit3_ID-3, Commit4_ID-1, Commit5_ID-1 ]

Partitions: [ Commit1_ID-9 ], [ Commit2_ID-3, Commit3_ID-3 ], [Commit4_ID-1, Commit5_ID-1 ]

Concurrent Rollup Algorithm:

1. Get sizes
2. Get partitions
3. forall partitions:
     If len(partition) >= r,
     rollup(partition[0], partition[r])

*/

/*
 * An integer log
 */
log(N,B,0) :-
    B > N,
    !.
log(N,B,Ans) :-
    N1 is N // B,
    N1 > 0,
    log(N1, B, A),
    Ans is A + 1.

/* true if some rollup could occur upto distance */
positions(Length, Base, Start, End) :-
    Length >= Base,
    log(Length, Base, Exp),
    Offset is Base ** Exp,
    (   Start = 0,
        End is Offset - 1
    ;   Offset < Length,
        New_Length is Length - Offset,
        positions(New_Length, Base, Shifted_Start, Shifted_End),
        Start is Shifted_Start + Offset,
        End is Shifted_End + Offset).

rollup_base(3).  % Standard base

exponential_rollup_strategy(Layer) :-
    triple_store(Store),
    % Names = [Oldest, ... , Newest]
    layer_stack_names(Layer, Names),
    length(Names, Size),
    rollup_base(B),
    forall(
        (   nth1(N,Names,Name),
            positions(Size, B, Start, N),
            true
            % format(user_error, "About to optimize from Start: ~q to End: ~q~n", [Start, N])
        ),
        (   Start = 0 % rollup everything
        ->  store_id_layer(Store, Name, This_Layer),
            rollup(This_Layer)
        ;   store_id_layer(Store, Name, This_Layer),
            nth0(Start, Names, Upto_Id),
            store_id_layer(Store, Upto_Id, Upto),
            imprecise_rollup_upto(This_Layer, Upto)
        )
    ).

:- begin_tests(optimize).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).

:- use_module(library(ordsets)).

test(partition,[]) :-
    B1 = 2,
    findall(
        Start-End,
        (   positions(10, B1, Start, End)),
        Positions1),
    Positions1 = [0-7,8-9],

    B2 = 3,
    findall(
        Start-End,
        (   positions(27, B2, Start, End)),
        Positions2),
    Positions2 = [0-26],

    B3 = 3,
    findall(
        Start-End,
        (   positions(30, B3, Start, End)),
        Positions3),
    Positions3 = [0-26, 27-29].

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

test(optimize_branch,
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
                                 message : "commit 1" },
                    Context2),

    with_transaction(
        Context2,
        ask(Context2,
            insert(d,e,f)),
        _),

    api_optimize(system_descriptor{}, Auth, Path),

    resolve_absolute_string_descriptor(Repository_Path,Repository_Descriptor),
    api_optimize(system_descriptor{}, Auth, Repository_Path),

    resolve_absolute_string_descriptor(Database_Path,Database_Descriptor),
    api_optimize(system_descriptor{}, Auth, Database_Path),

    resolve_absolute_string_descriptor(System_Path,system_descriptor{}),
    api_optimize(system_descriptor{}, Auth, System_Path).


:- end_tests(optimize).
