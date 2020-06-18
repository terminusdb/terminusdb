:- module(db_create, [
              create_db/5,
              try_create_db/5,
              create_ref_layer/2
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
°*  but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
 *  GNU General Public License for more details.                         *
 *                                                                       *
 *  You should have received a copy of the GNU General Public License    *
 *  along with TerminusDB.  If not, see <https://www.gnu.org/licenses/>. *
 *                                                                       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

:- reexport(core(util/syntax)).
:- use_module(core(util)).
:- use_module(core(triple)).
:- use_module(core(query)).
:- use_module(core(transaction)).

:- use_module(library(terminus_store)).

insert_db_object_triples(Layer, Builder, Organization_Name, Database_Name, Label, Comment, Db_Uri) :-
    database_class_uri(Database_Class_Uri),
    resource_name_property_uri(Database_Name_Property_Uri),
    xsd_string_type_uri(Xsd_String_Type_Uri),
    object_storage(Database_Name^^Xsd_String_Type_Uri, Name_Literal),
    random_idgen('system:///terminus/data/Database', [Organization_Name, Database_Name], Db_Uri),

    write_instance(Builder,Db_Uri,Database_Name,Database_Class_Uri),
    nb_add_triple(Builder,
                  Db_Uri,
                  Database_Name_Property_Uri,
                  Name_Literal),

    label_prop_uri(Label_Prop),
    object_storage(Label@en, Label_Literal),

    nb_add_triple(Builder,
                  Db_Uri,
                  Label_Prop,
                  Label_Literal),

    comment_prop_uri(Comment_Prop),
    object_storage(Comment@en, Comment_Literal),

    nb_add_triple(Builder,
                  Db_Uri,
                  Comment_Prop,
                  Comment_Literal),

    do_or_die(organization_name_uri(Layer, Organization_Name, Organization_Uri),
              error(organization_does_not_exist(Organization_Name), context(insert_db_object_triples/7, _))),
    resource_includes_prop_uri(Resource_Includes_Prop),
    % Add the resource scope to server
    nb_add_triple(Builder,
                  Organization_Uri,
                  Resource_Includes_Prop,
                  node(Db_Uri)).

insert_db_object(Organization_Name, Database_Name, Label, Comment, Db_Uri) :-
    % todo we should probably retry if this fails cause others may be moving the terminus db
    storage(Store),
    system_instance_name(Instance_Name),
    safe_open_named_graph(Store, Instance_Name, Graph),
    head(Graph, Layer),

    (   database_exists(Layer, Organization_Name, Database_Name)
    ->  throw(error(database_exists(Organization_Name,Database_Name),
                    context(insert_db_object/1,
                            'database already exists')))
    ;   true),

    open_write(Layer, Builder),
    insert_db_object_triples(Layer, Builder, Organization_Name, Database_Name, Label, Comment,Db_Uri),
    nb_commit(Builder, New_Layer),
    nb_set_head(Graph, New_Layer).

:- multifile prolog:message//1.
prolog:message(error(database_exists(Name), _)) -->
                [ 'The database ~w already exists'-[Name]].

local_repo_uri(Name, Uri) :-
    atomic_list_concat(['terminusdb:///repository/', Name, '/data/Local'], Uri).

/**
 * create_repo_graph(+Organization,+Name)
 */
create_repo_graph(Organization,Database) :-
    storage(Store),
    organization_database_name(Organization,Database,Name),
    safe_create_named_graph(Store,Name,_Graph),
    Descriptor = database_descriptor{ organization_name : Organization,
                                      database_name: Database },
    create_context(Descriptor, Context),
    with_transaction(Context,
                     insert_local_repository(Context, "local", _),
                     _).

write_instance(Builder,URI,Label,Class) :-
    rdf_type_uri(Rdf_Type_Uri),
    label_prop_uri(Label_Prop),
    object_storage(Label@en, Label_Literal),
    nb_add_triple(Builder,URI,Rdf_Type_Uri,node(Class)),
    nb_add_triple(Builder,URI,Label_Prop,Label_Literal).

create_ref_layer(Descriptor,Prefixes) :-
    create_context(Descriptor, Context),
    with_transaction(
        Context,
        (   insert_branch_object(Context, "master", _),
            update_prefixes(Context, Prefixes)
        ),
        _).

finalize_system(Db_Uri) :-
    storage(Store),
    system_instance_name(Instance_Name),
    safe_open_named_graph(Store, Instance_Name, Graph),
    head(Graph, Layer),
    open_write(Layer, Builder),
    finalized_element_uri(Finalized),
    database_state_prop_uri(State_Prop),

    % Tell terminus that we are actually finalized
    nb_add_triple(Builder,Db_Uri,State_Prop,node(Finalized)),
    nb_commit(Builder,Final),
    nb_set_head(Graph,Final).

create_db(Organization_Name,Database_Name, Label, Comment, Prefixes) :-
    text_to_string(Organization_Name, Organization_Name_String),
    text_to_string(Database_Name, Database_Name_String),
    % insert new db object into the terminus db
    insert_db_object(Organization_Name_String, Database_Name_String, Label, Comment, Db_Uri),

    % create repo graph - it has name as label
    create_repo_graph(Organization_Name_String, Database_Name_String),

    % create ref layer with master branch
    Repository_Descriptor = repository_descriptor{
                                database_descriptor:
                                database_descriptor{
                                    organization_name: Organization_Name_String,
                                    database_name: Database_Name_String
                                },
                                repository_name: "local"
                            },
    create_ref_layer(Repository_Descriptor,Prefixes),

    % update system with finalized
    finalize_system(Db_Uri).


/*
 * try_create_db(Organization,DB,Label,Comment,Prefixes) is det.
 *
 * Try to create a database and associate resources
 */
try_create_db(Organization,DB,Label,Comment,Prefixes) :-
    % create the collection if it doesn't exist
    do_or_die(
        not(database_exists(DB)),
        error(database_already_exists(Label))),

    do_or_die(
        create_db(Organization, DB, Label, Comment, Prefixes),
        error(database_could_not_be_created(Label))).


:- begin_tests(database_creation).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).

test(create_db_and_check_master_branch, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State)),

         true((once(ask(Repo_Descriptor, t(_,ref:branch_name,"master"^^xsd:string))),
               \+ ask(Branch_Descriptor, t(_,_,_))))
         ])
:-
    Prefixes = _{ doc : 'http://somewhere/document', scm : 'http://somewhere/schema' },
    create_db(testdb, 'testdb', 'a test db', Prefixes),
    Database_Descriptor = database_descriptor{ database_name: "testdb"},
    Repo_Descriptor = repository_descriptor{ database_descriptor: Database_Descriptor, repository_name: "local" },
    Branch_Descriptor = branch_descriptor{ repository_descriptor: Repo_Descriptor, branch_name: "master" }.
:- end_tests(database_creation).
