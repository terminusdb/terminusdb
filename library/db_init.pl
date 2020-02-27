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

/**
 * create_repo_graph(+Name,-Repo_Write_Builder)
 */
create_repo_graph(Name,Repo_Builder) :-
    storage(Store),
    safe_create_named_graph(Store,Name,Graph),
    open_write(Layer, Builder),
    repository_class_uri(Class),
    rdf_type_uri(RDFType),
    atomic_list_concat(['terminus:///', Name, '/document/Local'], Local_Base),
    idgen(Local_Base, [], Local_URI),

    nb_add_triple(Repo_Builder,
                  Local_URI,
                  Rdf_Type_Uri,
                  node(Database_Class_Uri)),

    true.

create_db(Name) :-
    % insert new db object into the terminus db
    insert_db_object(Name),
    %.. more to come ..

    % create repo graph - it has name as label
    % I think we want to commit this last...
    create_repo_graph(Name,Repo_Builder),

    % create ref layer with master branch and fake first commit
    create_ref_layer(Repo_Builder,Ref_Layer),

    % create master / doc and schema
    create_master_doc_schema(Ref_Layer,New_Ref_Layer),

    % write layer id as local repo in repo graph
    finalise_repo_graph(Repo_Builder,Ref_Layer),

    % update terminusdb with finalized
    finalise_terminus(Name).
