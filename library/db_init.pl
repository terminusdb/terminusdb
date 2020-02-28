:- module(db_init, []).

/** <module> Implementation of database graph management
 *
 * This module helps other modules with the representation of databases and
 * their associated graphs by bundling them as objects with some convenience
 * operators and accessors.
 *
 * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of TerminusDB.                                     *
 *                                                                       *
 *  TerminusDB is free software: you can redistribute it and/or modify   *
 *  it under the terms of the GNU General Public License as published by *
 *  the Free Software Foundation, under version 3 of the License.        *
 *                                                                       *
 *                                                                       *
 *  TerminusDB is distributed in the hope that it will be useful,        *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
 *  GNU General Public License for more details.                         *
 *                                                                       *
 *  You should have received a copy of the GNU General Public License    *
 *  along with TerminusDB.  If not, see <https://www.gnu.org/licenses/>. *
 *                                                                       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
:- use_module(library(terminus_store)).
:- use_module(library(triplestore)).
:- use_module(library(terminus_bootstrap)).

db_name_uri(Name, Uri) :-
    % TODO make invertible (probably by making idgen invertible)
    idgen('terminus:///terminus/document/Database', [Name], Uri).

db_exists(Layer, Name) :-
    database_name_property_uri(Database_Name_Property_Uri),
    xsd_string_type_uri(Xsd_String_Type_Uri),
    object_storage(literal(Xsd_String_Type_Uri, Name), Name_Literal),
    db_name_uri(Name, Db_Uri),

    triple(Layer,
           Db_Uri,
           Database_Name_Property_Uri,
           value(Name_Literal)).

insert_db_object_triples(Builder, Name) :-
    database_class_uri(Database_Class_Uri),
    database_name_property_uri(Database_Name_Property_Uri),
    rdf_type_uri(Rdf_Type_Uri),
    xsd_string_type_uri(Xsd_String_Type_Uri),
    object_storage(literal(Xsd_String_Type_Uri, Name), Name_Literal),
    db_name_uri(Name, Db_Uri),

    nb_add_triple(Builder,
                  Db_Uri,
                  Rdf_Type_Uri,
                  node(Database_Class_Uri)),
    nb_add_triple(Builder,
                  Db_Uri,
                  Database_Name_Property_Uri,
                  value(Name_Literal)).

insert_db_object(Name) :-
    % todo we should probably retry if this fails cause others may be moving the terminus db
    storage(Store),
    terminus_instance_name(Instance_Name),
    safe_open_named_graph(Store, Instance_Name, Graph),
    head(Graph, Layer),

    (   db_exists(Layer, Name)
    ->  throw(error(database_exists(Name),
                    context(insert_db_object/1,
                            'database already exists')))
    ;   true),

    open_write(Layer, Builder),
    insert_db_object_triples(Builder, Name),
    nb_commit(Builder, New_Layer),
    nb_set_head(Graph, New_Layer).

:- multifile prolog:message//1.
prolog:message(error(database_exists(Name), _)) -->
                [ 'The database ~w already exists'-[Name]].

local_repo_uri(Name, Uri) :-
    atomic_list_concat(['terminus:///', Name, '/document/Local'], Uri).

/**
 * create_repo_graph(+Name,-Repo_Write_Builder)
 */
create_repo_graph(Name,Graph,Builder) :-
    storage(Store),
    safe_create_named_graph(Store,Name,Graph),
    open_write(Store, Builder),
    local_repository_class_uri(Local_Class_Uri),
    local_repo_uri(Name, Local_Uri),

    % TODO: is this really all there is to do?
    write_instance(Builder,Local_Uri,'Local',Local_Class_Uri).

write_instance(Builder,URI,Label,Class) :-
    rdf_type_uri(Rdf_Type_Uri),
    label_prop_uri(Label_Prop),
    object_storage(literal(en, Label), Label_Literal),
    nb_add_triple(Builder,URI,Rdf_Type_Uri,node(Class)),
    nb_add_triple(Builder,URI,Label_Prop,Label_Literal).

create_ref_layer(Base_URI,Ref_Layer) :-
    storage(Store),
    open_write(Store,Layer_Builder),
    branch_class_uri(Branch_Class),
    ref_commit_prop_uri(Ref_Commit_Prop),
    ref_no_commit_uri(No_Commit),

    atomic_list_concat([Base_URI, '/', Name, '/document/Local'], Branch_URI),
    write_instance(Layer_Builder,Branch_URI,'Local',Branch_Class),
    nb_add_triple(Layer_Builder,Branch_URI,Ref_Commit_Prop,node(No_Commit)),

    ref_settings_class_uri(Settings_Class),
    ref_settings_base_uri_prop_uri(Settings_Prop),
    xsd_any_uri_type_uri(Xsd_Any_Uri_Type_Uri),
    object_storage(literal(Xsd_Any_Uri_Type_Uri, Base_URI), Base_URI_Literal),
    atomic_list_concat([Base_URI, '/', Name, '/document/Settings'], Settings_URI),
    write_instance(Layer_Builder,Settings_URI,'Settings',Settings_Class),
    nb_add_triple(Layer_Builder,Settings_URI,Settings_Prop,value(Base_URI_Literal)),

    nb_commit(Layer_Builder,Ref_Layer).

finalise_repo_graph(Repo_Graph, Repo_Builder, Name, Ref_Layer) :-
    % set local repo layer to ref layer
    layer_to_id(Ref_Layer, Layer_Id),
    local_repo_uri(Name, Local_Uri),
    shadow_layer_class_uri(Shadow_Layer_Class_Uri),
    atomic_list_concat(['terminus://', Name, '/document/ShadowLayer', Layer_Id], Shadow_Layer_Uri),
    atomic_list_concat(['Layer ', Layer_Id], Shadow_Layer_Name),
    write_instance(Repo_Builder, Shadow_Layer_Uri, Shadow_Layer_Name, Shadow_Layer_Class_Uri),

    layer_id_prop_uri(Layer_Id_Prop_Uri),
    xsd_string_type_uri(Xsd_String_Type_Uri),
    object_storage(literal(Xsd_String_Type_Uri, Layer_Id), Layer_Id_Literal),
    nb_add_triple(Repo_Builder, Shadow_Layer_Uri, Layer_Id_Prop_Uri, value(Layer_Id_Literal)),

    repository_head_prop_uri(Repository_Head_Prop_Uri),
    nb_add_triple(Repo_Builder, Local_Uri, Repository_Head_Prop_Uri, node(Shadow_Layer_Uri)),

    % and then do the commit and label set
    nb_commit(Repo_Builder, Repo_Layer),
    nb_set_head(Repo_Graph, Repo_Layer).

create_db(Name,Base_URI) :-
    % insert new db object into the terminus db
    insert_db_object(Name),
    %.. more to come ..

    % create repo graph - it has name as label
    % I think we want to commit this last...
    create_repo_graph(Name,Repo_Graph,Repo_Builder),

    % create ref layer with master branch and fake first commit
    create_ref_layer(Base_URI,Ref_Layer),

    % write layer id as local repo in repo graph
    finalise_repo_graph(Repo_Graph, Repo_Builder, Name, Ref_Layer),

    % update terminusdb with finalized
    finalise_terminus(Name).
