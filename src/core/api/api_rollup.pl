:- module(api_rollup, [api_rollup/5]).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(triple)).
:- use_module(core(account)).
:- use_module(library(terminus_store)).
:- use_module(library(yall)).
:- use_module(library(lists)).
:- use_module(library(apply)).

% Roll up the repository layer (commit graph) for a branch descriptor.
% This prevents O(n) performance degradation from layer stack traversal.
% The repository layer accumulates child layers with each commit.
rollup_repository_for_branch(Descriptor) :-
    (   branch_descriptor{repository_descriptor: Repository_Descriptor} :< Descriptor
    ->  % Open the repository and roll up its instance layer (commit graph)
        open_descriptor(Repository_Descriptor, Repo_Transaction),
        [Instance] = (Repo_Transaction.instance_objects),
        Layer = (Instance.read),
        (   ground(Layer)
        ->  % Roll up and reload from disk to get flattened version
            rollup(Layer),
            layer_to_id(Layer, LayerId),
            storage(Store),
            store_id_layer(Store, LayerId, _ReloadedLayer),
            % Clear retained layers for this descriptor
            retractall(descriptor:retained_descriptor_layers(Repository_Descriptor, _))
        ;   true),
        % Also roll up the database layer (repo metadata)
        (   get_dict(database_descriptor, Repository_Descriptor, Database_Descriptor)
        ->  retractall(descriptor:retained_descriptor_layers(Database_Descriptor, _))
        ;   true)
    ;   true).



api_rollup(System_DB, Auth, Path, _Options, Status_List) :-
    % Clean up stale weak references from the Rust layer cache BEFORE rollup.
    % This removes dead entries that accumulated from previous operations.
    % The cleanup happens here (at start) because layers are still held in memory
    % during rollup, so cleanup at the end would be ineffective.
    storage(Store),
    cleanup_layer_cache(Store, _Removed),

    do_or_die(resolve_absolute_string_descriptor(Path, Descriptor),
              error(invalid_absolute_path(Path), _)),

    do_or_die(
        askable_context(Descriptor, System_DB, Auth, Context),
        error(unresolvable_collection(Descriptor), _)),

    assert_write_access(Context),
    get_dict(transaction_objects, Context, [Transaction]),

    database_instance(Transaction, Instances),
    database_schema(Transaction, Schema),
    database_inference(Transaction, Inference),

    append([Instances, Schema, Inference], Read_Write_Objects),

    maplist([Read_Write_Object, success(Desc)]>>(
                read_write_obj_reader(Read_Write_Object, Layer),
                get_dict(descriptor, Read_Write_Object, Desc),
                rollup(Layer)
            ),
            Read_Write_Objects,
            Status_List),

    % Roll up the repository layer (commit graph) to prevent O(n) traversal.
    % The commit graph accumulates child layers with each commit, and
    % triple_exists() traverses the ENTIRE parent chain on each query.
    rollup_repository_for_branch(Descriptor),

    % Clear retained_descriptor_layers cache for all rolled-up descriptors
    % This forces the next transaction to reload fresh Arc<InternalLayer> objects from disk
    % with parent chains pointing to the rolled-up layer hierarchy.
    maplist([Read_Write_Object]>>(
                get_dict(descriptor, Read_Write_Object, Desc),
                retractall(descriptor:retained_descriptor_layers(Desc, _))
            ),
            Read_Write_Objects).
