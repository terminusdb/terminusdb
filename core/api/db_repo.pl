:- module(db_repo, [local_repository_class_uri/1,
                    remote_repository_class_uri/1,
                    repository_head_prop_uri/1,
                    repository_name_prop_uri/1,
                    repo_layer_name_to_ref_layer_id/3
                   ]).

:- reexport(core(util/syntax)).

:- use_module(core(triple/terminus_bootstrap)).
:- use_module(core(triple/literals)).

repo_layer_name_to_ref_layer_id(Repo_Layer, Repo_Name, Ref_Layer_Id) :-
    repository_name_prop_uri(Repo_Name_Property_Uri),
    repository_head_prop_uri(Repo_Head_Property_Uri),
    layer_id_prop_uri(Layer_Id_Property_Uri),
    xsd_string_type_uri(Xsd_String_Type_Uri),

    predicate_id(Repo_Layer, Repo_Name_Property_Uri, Repo_Name_Property_Id),
    predicate_id(Repo_Layer, Repo_Head_Property_Uri, Repo_Head_Property_Id),
    predicate_id(Repo_Layer, Layer_Id_Property_Uri, Layer_Id_Property_Id),
    object_storage(Repo_Name^^Xsd_String_Type_Uri, Repo_Name_Literal),
    object_id(Repo_Layer, Repo_Name_Literal, Repo_Name_Id),

    id_triple(Repo_Layer, Repo_Uri_Id, Repo_Name_Property_Id, Repo_Name_Id),
    id_triple(Repo_Uri_Id, Repo_Head_Property_Id, Repo_Head_Id),
    id_triple(Repo_Head_Id, Layer_Id_Property_Id, Ref_Layer_Id_Id),

    object_id(Ref_Layer_Id_Literal, Ref_Layer_Id_Id),
    storage_object(Ref_Layer_Id_Literal, Ref_Layer_Id^^_).

repo_type_class_uri(local, Uri) :-
    local_repository_class_uri(Uri).
repo_type_class_uri(remote, Uri) :-
    remote_repository_class_uri(Uri).

repo_layer_insert_repo(Repo_Layer-Repo_Layer_Builder, Db_Name, Repo_Name, Repo_Type, Destination_Url, Repo_Uri) :-
    predicate_id(Repo_Layer, Repo_Name_Property_Uri, Repo_Name_Property_Id),

    object_storage(Repo_Name^^Xsd_String_Type_Uri, Repo_Name_Literal),
    (   triple(Repo_Layer, _, Repo_Name_Property_Uri, Repo_Name_Literal)
    ->  throw(error(repository_exists(Repo_Name),
                    context(repo_layer_insert_repo/4,
                            'repository already exists')))
    ;   true),

    % open layer builder if it's not there yet
    (   var(Repo_Layer_Builder)
    ->  open_write(Repo_Layer, Repo_Layer_Builder)
    ;   true),

    % now actually insert the thing
    repo_name_uri(Db_Name, Repo_Name, Repo_Type, Repo_Uri),
    rdf_type_uri(Rdf_Type_Uri),
    repo_type_class_uri(Repo_Type, Class_Uri),

    nb_add_triple(Repo_Layer_Builder, Repo_Uri, Rdf_Type_Uri, Class_Uri),
    % todo a few sub predicates for the different things to insert here would be nice
    true.

repo_layer_delete_repo(Asdf) :-
    true.

repo_layer_update_remote_url(Asdf).
repo_layer_update_ref_layer(Asdf).
repo_layer_rename_repo(Asdf).
