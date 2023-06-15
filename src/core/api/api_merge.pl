:- module(api_merge, [api_merge/6]).

:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(triple)).
:- use_module(core(account)).
:- use_module(library(terminus_store)).
:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(pcre)).
:- use_module(library(lists)).
:- use_module(library(option)).

api_merge(System_DB, Auth, Sources, Target, Commit_Id, Options) :-
    do_or_die(
        (   resolve_absolute_string_descriptor(Target, Target_Descriptor),
            resolve_relative_descriptor(Target_Descriptor, ["_commits"], Target_Repo)
        ),
        error(no_repository_descriptor(Target_Descriptor), _)),

    check_descriptor_auth(System_DB, Target_Descriptor,
                          '@schema':'Action/commit_write_access', Auth),

    maplist({System_DB, Auth}/[Source, Schema_Layer_Id, Instance_Layer_Id]>>(
                do_or_die(
                    resolve_absolute_string_descriptor(Source, Descriptor),
                    error(invalid_absolute_path(Source),_)),
                check_descriptor_auth(System_DB, Descriptor, '@schema':'Action/instance_read_access', Auth),
                do_or_die(
                    resolve_relative_descriptor(Descriptor, ["_commits"], Repo),
                    error(no_commit_descriptor(Descriptor), _)),

                open_descriptor(Repo, Repo_DB),
                (   branch_descriptor{} :< Descriptor
                ->  get_dict(branch_name, Descriptor, Branch_Name),
                    branch_head_commit(Repo_DB, Branch_Name, Commit_Uri)
                ;   commit_descriptor{} :< Descriptor
                ->  get_dict(commit_id, Descriptor, Commit_Id),
                    commit_id_uri(Repo_DB, Commit_Id, Commit_Uri)
                ;   throw(error(bad_merge_descriptor(Descriptor)))
                ),
                do_or_die(
                    layer_uri_for_commit(Repo_DB, Commit_Uri, instance, Instance_Layer_Uri),
                    error(instance_layer_missing_in_merged_data(Descriptor), _)),
                layer_uri_for_commit(Repo_DB, Commit_Uri, schema, Schema_Layer_Uri),
                layer_id_uri(Repo_DB, Instance_Layer_Id, Instance_Layer_Uri),
                layer_id_uri(Repo_DB, Schema_Layer_Id, Schema_Layer_Uri)
            ),
            Sources,
            Schema_Layer_Ids,
            Instance_Layer_Ids
           ),
    % todo, check all schema layers are compatible
    [Schema_Layer_Id|_] = Schema_Layer_Ids,
    config:tmp_path(Tmp_Path),
    triple_store(Store),

    catch(
        merge_base_layers(Store, Tmp_Path, Instance_Layer_Ids, Target_Instance_Layer_Id),
        error(rust_io_error('Other', Error_Message), _),
        (   re_matchsub('given layer is not a base layer: (?<layerid>[a-f0-9]*)',
                        Error_Message, Dict, [])
        ->  get_dict(layerid, Dict, LayerId),
            nth0(N, Instance_Layer_Ids, LayerId),
            nth0(N, Sources, LayerSource),
            resolve_absolute_string_descriptor(LayerSource, LayerDescriptor),
            throw(error(not_a_base_layer(LayerId, LayerDescriptor), _))
        ;   throw(error(rust_io_error('Other', Error_Message),_))
        )
    ),

    option(author(Author), Options),
    option(message(Message), Options),
    create_context(Target_Repo, Repo_Context),
    with_transaction(
        Repo_Context,
        (   insert_layer_object(
                Repo_Context,
                Schema_Layer_Id,
                Schema_Layer_Uri),
            insert_layer_object(
                Repo_Context,
                Target_Instance_Layer_Id,
                Target_Instance_Layer_Uri),
            get_time(Time),
            insert_base_commit_object(
                Repo_Context,
                Schema_Layer_Uri,
                Target_Instance_Layer_Uri,
                commit_info{
                    author: Author,
                    message: Message
                },
                Time,
                Commit_Id,
                _)
        ),
        _
    ).


