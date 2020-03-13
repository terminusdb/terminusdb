:- module(db_init, [
              create_db/2
          ]).

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
:- use_module(triplestore).
:- use_module(literals).
:- use_module(casting).
:- use_module(terminus_bootstrap).
:- use_module(database_utils).
:- use_module(syntax).


insert_db_object_triples(Builder, Name) :-
    database_class_uri(Database_Class_Uri),
    database_name_property_uri(Database_Name_Property_Uri),
    xsd_string_type_uri(Xsd_String_Type_Uri),
    object_storage(Name^^Xsd_String_Type_Uri, Name_Literal),
    db_name_uri(Name, Db_Uri),

    write_instance(Builder,Db_Uri,Name,Database_Class_Uri),
    nb_add_triple(Builder,
                  Db_Uri,
                  Database_Name_Property_Uri,
                  Name_Literal).

insert_db_object(Name) :-
    % todo we should probably retry if this fails cause others may be moving the terminus db
    storage(Store),
    terminus_instance_name(Instance_Name),
    safe_open_named_graph(Store, Instance_Name, Graph),
    head(Graph, Layer),

    (   db_exists_in_layer(Layer, Name)
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
    object_storage(Label@en, Label_Literal),
    nb_add_triple(Builder,URI,Rdf_Type_Uri,node(Class)),
    nb_add_triple(Builder,URI,Label_Prop,Label_Literal).

create_ref_layer(Name,Base_URI,Ref_Layer) :-
    storage(Store),
    open_write(Store,Layer_Builder),
    branch_class_uri(Branch_Class),

    atomic_list_concat([Base_URI, '/', Name, '/document/master'], Branch_URI),
    write_instance(Layer_Builder,Branch_URI,'master',Branch_Class),

    xsd_string_type_uri(Xsd_String_Type_Uri),
    object_storage("master"^^Xsd_String_Type_Uri, Branch_Name_Literal),
    ref_branch_name_prop_uri(Branch_Name_Prop_Uri),
    nb_add_triple(Layer_Builder,Branch_URI,Branch_Name_Prop_Uri,Branch_Name_Literal),

    xsd_any_uri_type_uri(Xsd_Any_Uri_Type_Uri),
    object_storage(Base_URI^^Xsd_Any_Uri_Type_Uri, Base_URI_Literal),

    ref_branch_base_uri_prop_uri(Branch_Base_URI),
    nb_add_triple(Layer_Builder,Branch_URI, Branch_Base_URI, Base_URI_Literal),

    nb_commit(Layer_Builder,Ref_Layer).

finalise_terminus(Name) :-
    storage(Store),
    terminus_instance_name(Instance_Name),
    safe_open_named_graph(Store, Instance_Name, Graph),
    head(Graph, Layer),
    open_write(Layer, Builder),
    finalized_element_uri(Finalized),
    database_state_prop_uri(State_Prop),

    % Tell terminus that we are actually finalized
    db_name_uri(Name, Db_Uri),
    nb_add_triple(Builder,Db_Uri,State_Prop,node(Finalized)),
    nb_commit(Builder,Final),
    nb_set_head(Graph,Final).

finalise_repo_graph(Repo_Graph, Repo_Builder, Name, Ref_Layer) :-
    % set local repo layer to ref layer
    layer_to_id(Ref_Layer, Layer_Id),
    local_repo_uri(Name, Local_Uri),
    layer_class_uri(Layer_Class_Uri),
    atomic_list_concat(['terminus://', Name, '/document/Layer', Layer_Id], Layer_Uri),
    atomic_list_concat(['Layer ', Layer_Id], Layer_Name),
    write_instance(Repo_Builder, Layer_Uri, Layer_Name, Layer_Class_Uri),

    layer_id_prop_uri(Layer_Id_Prop_Uri),
    xsd_string_type_uri(Xsd_String_Type_Uri),
    object_storage(Layer_Id^^Xsd_String_Type_Uri, Layer_Id_Literal),
    nb_add_triple(Repo_Builder, Layer_Uri, Layer_Id_Prop_Uri, Layer_Id_Literal),

    repository_head_prop_uri(Repository_Head_Prop_Uri),
    repository_name_prop_uri(Repository_Name_Prop_Uri),
    object_storage("local"^^Xsd_String_Type_Uri, Repository_Name_Literal),
    nb_add_triple(Repo_Builder, Local_Uri, Repository_Head_Prop_Uri, node(Layer_Uri)),
    nb_add_triple(Repo_Builder, Local_Uri, Repository_Name_Prop_Uri, Repository_Name_Literal),

    % and then do the commit and label set
    nb_commit(Repo_Builder, Repo_Layer),
    nb_set_head(Repo_Graph, Repo_Layer).

create_db(Name,Base_URI) :-
    % insert new db object into the terminus db
    insert_db_object(Name),

    % create repo graph - it has name as label
    % I think we want to commit this last...
    create_repo_graph(Name,Repo_Graph,Repo_Builder),

    % create ref layer with master branch
    create_ref_layer(Name,Base_URI,Ref_Layer),

    % write layer id as local repo in repo graph
    finalise_repo_graph(Repo_Graph, Repo_Builder, Name, Ref_Layer),

    % update terminusdb with finalized
    finalise_terminus(Name).
