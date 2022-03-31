:- module(descriptor,[
              is_descriptor/1,
              is_transaction/1,
              is_validation_object/1,
              open_read_write_obj/2,
              open_read_write_obj/4,
              open_descriptor/2,
              open_descriptor/3,
              open_descriptor/5,
              collection_descriptor_transaction_object/3,
              graph_descriptor_transaction_objects_read_write_object/3,
              instance_graph_descriptor_transaction_object/3,
              read_write_obj_reader/2,
              read_write_obj_builder/2,
              read_write_object_to_name/2,
              make_branch_descriptor/5,
              make_branch_descriptor/4,
              make_branch_descriptor/3,
              transactions_to_map/2,
              collection_descriptor_graph_filter_graph_descriptor/3,
              collection_descriptor_prefixes/2,
              collection_descriptor_default_write_graph/2,
              descriptor_to_loggable/2,
              ensure_transaction_has_builder/2,
              should_retain_layers_for_descriptor/1,
              retain_descriptor_layers/2
          ]).

/** <module> Descriptor Manipulation
 *
 * This file deals with the manipulation and opening of descriptors.
 *
 * Types:
 *
 * graph_descriptor --> labelled_graph{ label : atom
 *                                      type: atom,
 *                                      name: main }
 *                    | id_graph{ layer_id : atom,
 *                                type: atom,
 *                                name: string } % for debugging
 *                    | system_graph{ type : atom,
 *                                    name : atom }
 *                    | repo_graph { organization_name: string,
 *                                   database_name : string,
 *                                   type : atom,
 *                                   name : string }
 *                    | commit_graph{ organization_name: string,
 *                                    database_name : string,
 *                                    repository_name : string,
 *                                    type : atom,
 *                                    name : string }
 *                    | branch_graph{ organization_name: string,
 *                                    database_name : string,
 *                                    repository_name : string,
 *                                    branch_name : string,
 *                                    type : atom, % {instance, schema, inference}
 *                                    name : string }
 *                    | single_commit_graph{ organization_Name: string,
 *                                           database_name: string,
 *                                           repository_name: string,
 *                                           commit_id: string,
 *                                           type: atom,
 *                                           name: string }
 *
 * graph_filter --> type_filter{ types : list(atom) } % instance, inference, schema
 *                | type_name_filter{ type : atom }
 *
 * A named_graph refers to a file in the store - this has to be made unique so we don't get
 * collisionI s between different databases. Currently only used for the terminus and database graph
 * - one per repository. This uses the file label mechanism.
 *
 * A ref_graph is a layer id that can be resolved to a graph.
 *
 * collection_descriptor --> system_descriptor{}
 *                         | label_descriptor{ variety: atom,
 *                                             schema: string,
 *                                             instance: string }
 *                         | id_descriptor{ variety: atom,
 *                                          schema : string,
 *                                          instance: string } % only for querying!
 *                         | layer_descriptor{ variety: atom,
 *                                             instance : layer,
 *                                             schema : layer } % only for querying!
 *                         | database_descriptor{ organization_name : string,
 *                                                database_name : string }
 *                         | repository_descriptor{ database_descriptor : database_descriptor,
 *                                                  repository_name : string }
 *                         | branch_descriptor{ repository_descriptor: repository_descriptor,
 *                                              branch_name : string}, % the name of the thing advancing
 *                         | commit_descriptor{ repository_descriptor: repository_descriptor,
 *                                              commit_id : string} % the base of the commit
 *
 *
 * system_descriptor: refers to the core database with user and database management.
 * This database refers to the various database descriptors which can be opened "by name"
 * using the label mechanism.
 *
 * database_descriptor: is a per database graph collections that store information about all
 * local and remotes which exist
 *
 * repository descriptors: is used to refer to the branchs that exist in a given repository.
 *
 * branch_descriptors: refers to the precise graph collection which is a 'normal' database
 * object used in WOQL.
 *
 * Transaction management requires that we advance a file system label associated with the
 * repository descriptor when we have performed a full transaction. All other multi-graph
 * management is done within the graphs associated with the repository descriptor.
 *
 * The type polymorphism is designed to allow WOQL read transactions to work identically over
 * all of these cases. However, *write* transactions have to be done differently on each of
 * these by case, though triple writes can be done identically.
 *
 * read_write_obj ---> read_write_obj{ descriptor : graph_descriptor, read : Layer, write: Var_Or_Layer_Builder}
 *
 * transaction_object ---> transaction_object{ descriptor : collection_descriptor,
 *                                 <parent : transaction_object>, % except for database/terminus descriptors
 *                                 <commit_info : commit_info>, % only ref_descriptors
 *                                 instance_objects : list(read_write_obj),
 *                                 schema_objects : list(read_write_obj),
 *                                 inference_objects : list(read_write_obj) }
 *
 * commit_info ---> commit_info{ author : string,
 *                               message : string }
 *
 * Commit Info is required by writing commits
 */
 
 
/* * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of TerminusDB.                                     *
 *                                                                       *
 *  TerminusDB is free software: you can redistribute it and/or modify   *
 *  it under the terms of the  the Apache License, Version 2.0           *
 *  (the "License");                                                     *
 *  you may not use this file except in compliance with the License.     *
 *  You may obtain a copy of the License at                              *
 *                                                                       *
 *  http://www.apache.org/licenses/LICENSE-2.0                           *
 *                                                                       *
 *  Unless required by applicable law or agreed to in writing, software  *
 *  distributed under the License is distributed on an "AS IS" BASIS,    *
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or      *
 *  implied.                                                             *
 *  See the License for the specific language governing permissions and  *
 *  limitations under the License.                                       *
 *                                                                       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


:- reexport(core(util/syntax)).

:- use_module(repo_entity).
:- use_module(ref_entity).
:- use_module(layer_entity).


:- use_module(core(util)).
:- use_module(core(triple)).
:- use_module(core(query)).
:- use_module(config(terminus_config)).

:- use_module(library(terminus_store)).
:- use_module(library(lists)).
:- use_module(library(yall)).
:- use_module(library(apply)).
:- use_module(library(plunit)).
:- use_module(library(debug)).

is_descriptor_name(system_descriptor).
is_descriptor_name(label_descriptor).
is_descriptor_name(document_label_descriptor).
is_descriptor_name(id_descriptor).
is_descriptor_name(layer_descriptor).
is_descriptor_name(database_descriptor).
is_descriptor_name(repository_descriptor).
is_descriptor_name(branch_descriptor).
is_descriptor_name(commit_descriptor).

is_descriptor(Descriptor) :-
    is_dict(Descriptor),
    Type{} :< Descriptor,
    is_descriptor_name(Type).

is_transaction(Transaction) :-
    is_dict(Transaction),
    transaction_object{} :< Transaction.

is_validation_object(VO) :-
    is_dict(VO),
    validation_object{} :< VO.

graph_descriptor_layer_to_read_write_obj(Descriptor, Layer, read_write_obj{
                                                                descriptor: Descriptor,
                                                                read: Layer,
                                                                write: _Layer_Builder
                                                            }).

open_read_write_obj(Openable, Read_Write_Obj) :-
    open_read_write_obj(Openable, Read_Write_Obj, [], _).

open_read_write_obj(Layer, Read_Write_Obj, Map, New_Map) :-
    blob(Layer, layer),
    !,
    layer_to_id(Layer, Id),
    Descriptor = id_graph{id: Id,
                          type: instance},
    (   memberchk(Descriptor=Read_Write_Obj, Map)
    ->  New_Map = Map
    ;   graph_descriptor_layer_to_read_write_obj(Descriptor, Layer, Read_Write_Obj),
        New_Map = [Descriptor=Read_Write_Obj|Map]).
open_read_write_obj(Descriptor, Read_Write_Obj, Map, Map) :-
    member(Found_Descriptor=Read_Write_Obj, Map),
    Descriptor :< Found_Descriptor,
    !.
open_read_write_obj(Descriptor, Read_Write_Obj, Map, [Descriptor=Read_Write_Obj|Map]) :-
    Descriptor = system_graph{ type: Type},
    !,
    (   Type = instance
    ->  system_instance_name(Graph_Name)
    ;   Type = schema
    ->  system_schema_name(Graph_Name)
    ;   Type = inference,
        system_inference_name(Graph_Name)),
    storage(Store),
    safe_open_graph_head(Store, Graph_Name, Layer),
    graph_descriptor_layer_to_read_write_obj(Descriptor, Layer, Read_Write_Obj).
open_read_write_obj(Descriptor, Read_Write_Obj, Map, [Descriptor=Read_Write_Obj|Map]) :-
    Descriptor = labelled_graph{ label: Name,
                                 type: Type},
    !,
    memberchk(Type, [instance, schema, inferrence]),
    storage(Store),
    safe_open_named_graph(Store, Name, Graph),
    ignore(head(Graph, Layer)),
    graph_descriptor_layer_to_read_write_obj(Descriptor, Layer, Read_Write_Obj).
open_read_write_obj(Descriptor, Read_Write_Obj, Map, [Descriptor=Read_Write_Obj|Map]) :-
    Descriptor = id_graph{ layer_id: Layer_Id,
                           type: Type},
    !,
    memberchk(Type,[instance,schema]),
    storage(Store),
    store_id_layer(Store, Layer_Id, Layer),
    graph_descriptor_layer_to_read_write_obj(Descriptor, Layer, Read_Write_Obj).
open_read_write_obj(Descriptor, Read_Write_Obj, Map, [Descriptor=Read_Write_Obj|Map]) :-
    Descriptor = repo_graph{ organization_name : Organization_Name,
                             database_name: Database_Name,
                             type : Type},
    !,
    (   Type = instance
    ->  storage(Store),
        organization_database_name(Organization_Name,Database_Name,Composite),
        safe_open_named_graph(Store, Composite, Graph),
        ignore(head(Graph, Layer))
    ;   Type = schema
    ->  repository_ontology(Repository_Name),
        storage(Store),
        safe_open_graph_head(Store, Repository_Name, Layer)
    ),
    graph_descriptor_layer_to_read_write_obj(Descriptor, Layer, Read_Write_Obj).
open_read_write_obj(Descriptor,
                    Read_Write_Obj,
                    Map,
                    [Descriptor=Read_Write_Obj|New_Map]) :-
    Descriptor = commit_graph{ organization_name: Organization_Name,
                               database_name: Database_Name,
                               repository_name: Repository_Name,
                               type: Type},
    !,
    Repo_Descriptor = repo_graph{ organization_name: Organization_Name,
                                  database_name: Database_Name,
                                  type: instance},
    storage(Store),
    (   Type = instance
    ->  open_read_write_obj(Repo_Descriptor, Repository_Read_Write_Obj, Map, New_Map),

        repository_ontology(Repo_Name),
        safe_open_graph_head(Store, Repo_Name, Repo_Layer),

        Repo_Layer_Desc =
        layer_descriptor{
            variety: database_descriptor,
            instance: (Repository_Read_Write_Obj.read),
            schema: Repo_Layer
        },
        once(has_repository(Repo_Layer_Desc, Repository_Name)),
        ignore((   repository_head(Repo_Layer_Desc, Repository_Name, Commit_Layer_Id),
                   store_id_layer(Store, Commit_Layer_Id, Layer)))
    ;   Type = schema
    ->  New_Map = Map,
        ref_ontology(Ref_Name),
        safe_open_graph_head(Store, Ref_Name, Layer)
    ),
    graph_descriptor_layer_to_read_write_obj(Descriptor, Layer, Read_Write_Obj).
open_read_write_obj(Descriptor,
                    Read_Write_Obj,
                    Map,
                    [Descriptor=Read_Write_Obj|New_Map]) :-
    branch_graph{ organization_name: Organization_Name,
                  database_name: Database_Name,
                  repository_name: Repository_Name,
                  branch_name: Branch_Name,
                  type: Type
                } :< Descriptor,
    !,
    assertion(member(Type, [instance, schema])),
    Commit_Descriptor = commit_graph{ organization_name: Organization_Name,
                                      database_name : Database_Name,
                                      repository_name : Repository_Name,
                                      type: instance },

    open_read_write_obj(Commit_Descriptor, Commit_Read_Write_Obj, Map, New_Map),
    Repo = layer_descriptor{ instance: (Commit_Read_Write_Obj.read),
                             variety: repository_descriptor },
    (   branch_head_commit(Repo, Branch_Name, Commit_Uri)
    ->  commit_type(Repo, Commit_Uri, Commit_Type),
        ignore((layer_uri_for_commit(Repo, Commit_Uri, Type, Layer_Uri),
                layer_id_uri(Repo, Layer_Id, Layer_Uri),
                storage(Store),
                store_id_layer(Store, Layer_Id, Layer)))
    ;   Layer = _),

    Augmented_Descriptor = (Descriptor.put('commit_type', Commit_Type)),
    graph_descriptor_layer_to_read_write_obj(Augmented_Descriptor, Layer, Read_Write_Obj).
open_read_write_obj(Descriptor,
                    Read_Write_Obj,
                    Map,
                    [Descriptor=Read_Write_Obj|New_Map]) :-
    Descriptor = single_commit_graph{ organization_name: Organization_Name,
                                      database_name: Database_Name,
                                      repository_name: Repository_Name,
                                      commit_id: Commit_Id,
                                      type: Type },
    !,
    Commit_Descriptor = commit_graph{ organization_name: Organization_Name,
                                      database_name : Database_Name,
                                      repository_name : Repository_Name,
                                      type: instance },

    open_read_write_obj(Commit_Descriptor, Commit_Read_Write_Obj, Map, New_Map),
    Repo = layer_descriptor{ instance: (Commit_Read_Write_Obj.read),
                             variety: repository_descriptor },

    (   commit_id_uri(Repo, Commit_Id, Commit_Uri),
        layer_uri_for_commit(Repo, Commit_Uri, Type, Layer_Uri),
        layer_id_uri(Repo, Layer_Id, Layer_Uri),
        storage(Store),
        store_id_layer(Store, Layer_Id, Layer)
    ->  true
    ;   Layer = _),
    graph_descriptor_layer_to_read_write_obj(Descriptor, Layer, Read_Write_Obj).

read_write_obj_reader(Read_Write_Obj, _Layer) :-
    var(Read_Write_Obj.read),
    !,
    fail.
read_write_obj_reader(Read_Write_Obj, Layer) :-
    Layer = Read_Write_Obj.read.

/**
 * read_write_obj_builder(Read_Write_Obj, Layer_Builder) is semidet.
 *
 * Open a write object if it doesn't already exist. Set it nb destructively
 * so we can re-use it on back-tracking.
 *
 * WARNING this is non-back-tracking and side-effecting.
 */
read_write_obj_builder(Read_Write_Obj, Layer_Builder) :-
    ground(Read_Write_Obj.write),
    !,
    Layer_Builder = Read_Write_Obj.write.
read_write_obj_builder(Read_Write_Obj, Layer_Builder) :-
    var(Read_Write_Obj.read),
    !,

    triple_store(Store),
    open_write(Store, Layer_Builder),
    nb_set_dict(write,Read_Write_Obj,Layer_Builder).
read_write_obj_builder(Read_Write_Obj, Layer_Builder) :-
    open_write(Read_Write_Obj.read, Layer_Builder),
    nb_set_dict(write,Read_Write_Obj,Layer_Builder).

ensure_transaction_has_builder(schema, Transaction) :-
    [RWO] = (Transaction.schema_objects),
    read_write_obj_builder(RWO, _).
ensure_transaction_has_builder(instance, Transaction) :-
    [RWO] = (Transaction.instance_objects),
    read_write_obj_builder(RWO, _).
/*
read_write_obj_builder(Read_Write_Obj, Layer_Builder) :-
    (   get_dict(commit_type, (Read_Write_Obj.descriptor), 'http://terminusdb.com/schema/ref#InitialCommit')
    ->  triple_store(Store),
        open_write(Store, Layer_Builder),
        Layer = (Read_Write_Obj.read),
        nb_apply_delta(Layer_Builder, Layer),
        json_log_debug_formatted("applied delta!~n", [])
    ;   open_write(Read_Write_Obj.read, Layer_Builder)),

    nb_set_dict(write,Read_Write_Obj,Layer_Builder).
*/

/**
 * open_descriptor(Descriptor, Read_Graph_Descriptors, Write_Graph_Descriptors, Map, New_Map) is det.
 *
 * Implements the logic which allows us to special-case the
 * opening of descriptors for repository, branch and graph.
 *
 * It opens descriptors like an onion, avoiding duplicate opens with a Map.
 *
 * @Descriptor has type collection_descriptor
 * @Read_Graph_Descriptor has type list(collection_descriptor)
 *                        This specifies which read queries to open.
 * @Write_Graph_Descriptor has type list(collection_descriptor)
 *                         This specifies which write queries to open.
 * @Map has type list(descriptor=transaction_object)
 *      Keeps track of the descriptors which have already been opened so we are monotonic
 * @New_Map has type list(descriptor=transaction_object)
 *      Updated map
 */
open_descriptor_(Descriptor, _, _, _, _) :-
    var(Descriptor),
    !,
    throw(error(instantiation_error, _)).
open_descriptor_(Descriptor, _Commit_Info, Transaction_Object, Map, Map) :-
    memberchk(Descriptor=Transaction_Object, Map),
    !.
open_descriptor_(Layer, _Commit_Info, Transaction_Object, Map, [Descriptor=Transaction_Object|Map]) :-
    blob(Layer, layer),
    !,
    layer_to_id(Layer, Id),

    open_read_write_obj(Layer, Instance_Object, [], _),

    Descriptor = id_descriptor{ instance: Id, variety: branch_descriptor },
                                % assume branch instance graph
    Transaction_Object = transaction_object{
                             descriptor : Descriptor,
                             instance_objects : [Instance_Object],
                             schema_objects : [],
                             inference_objects : []
                         }.
open_descriptor_(system_descriptor{}, _Commit_Info, Transaction_Object, Map,
                 [system_descriptor{}=Transaction_Object|Map_2]) :-
    !,

    Instance_Graph = system_graph{ type: instance},
    Schema_Graph = system_graph{ type: schema},

    open_read_write_obj(Schema_Graph, Schema_Object, Map, Map_1),
    open_read_write_obj(Instance_Graph, Instance_Object, Map_1, Map_2),

    Transaction_Object = transaction_object{
                             descriptor : system_descriptor{},
                             instance_objects : [Instance_Object],
                             schema_objects : [Schema_Object],
                             inference_objects : []
                         }.
open_descriptor_(Descriptor, _Commit_Info, Transaction_Object, Map,
                 [Descriptor=Transaction_Object|New_Map]) :-
    id_descriptor{} :< Descriptor,
    !,
    (   get_dict(instance, Descriptor, Instance_ID)
    ->  Instance_Graph_Descriptor = id_graph{ layer_id : Instance_ID, type: instance},
        open_read_write_obj(Instance_Graph_Descriptor, Instance_RW, Map, Map1),
        Instance_Objects = [Instance_RW]
    ;   Map = Map1,
        Instance_Objects = []
    ),
    (   get_dict(schema, Descriptor, Schema_ID)
    ->  Schema_Graph_Descriptor = id_graph{ layer_id : Schema_ID, type: schema },
        open_read_write_obj(Schema_Graph_Descriptor, Schema_RW, Map1, New_Map),
        Schema_Objects = [Schema_RW]
    ;   Map1 = New_Map,
        Schema_Objects = []),

    Transaction_Object = transaction_object{
                             descriptor : Descriptor,
                             instance_objects : Instance_Objects,
                             schema_objects : Schema_Objects,
                             inference_objects : []
                         }.
open_descriptor_(Descriptor, _Commit_Info, Transaction_Object, Map, [Descriptor=Transaction_Object|Map]) :-
    layer_descriptor{} :< Descriptor,
    !,
    (   get_dict(instance, Descriptor, Instance_Layer)
    ->  open_read_write_obj(Instance_Layer, Instance_Object, [], _),
        Instance_Objects = [Instance_Object]
    ;   Instance_Objects = []),
    (   get_dict(schema, Descriptor, Schema_Layer)
    ->  open_read_write_obj(Schema_Layer, Schema_Object, [], _),
        Schema_Objects = [Schema_Object]
    ;   Schema_Objects = []),

    Transaction_Object = transaction_object{
                             descriptor : Descriptor,
                             instance_objects : Instance_Objects,
                             schema_objects : Schema_Objects,
                             inference_objects : []
                         }.
open_descriptor_(Descriptor, _Commit_Info, Transaction_Object, Map,
                 [Descriptor=Transaction_Object|Map_2]) :-
    label_descriptor{} :< Descriptor,
    !,
    (   get_dict(schema, Descriptor, Schema_Label)
    ->  Schema_Graph_Descriptor = labelled_graph{ label: Schema_Label, type: schema },
        open_read_write_obj(Schema_Graph_Descriptor, Schema_Read_Write_Obj, Map, Map_1),
        Schema_Objects = [Schema_Read_Write_Obj]
    ;   Map = Map_1,
        Schema_Objects = []
    ),
    (   get_dict(instance, Descriptor, Instance_Label)
    ->  Instance_Graph_Descriptor = labelled_graph{ label: Instance_Label, type: instance },
        open_read_write_obj(Instance_Graph_Descriptor, Instance_Read_Write_Obj, Map_1, Map_2),
        Instance_Objects = [Instance_Read_Write_Obj]
    ;   Map_1 = Map_2,
        Instance_Objects = []
    ),

    Transaction_Object = transaction_object{
                             descriptor: Descriptor,
                             instance_objects: Instance_Objects,
                             schema_objects: Schema_Objects,
                             inference_objects: []
                         }.
open_descriptor_(Descriptor, _Commit_Info, Transaction_Object, Map,
                 [Descriptor=Transaction_Object|Map_2]) :-
    database_descriptor{
        organization_name: Organization_Name,
        database_name: Database_Name
    } = Descriptor,
    !,

    Repository_Ontology_Graph = repo_graph{ organization_name: Organization_Name,
                                            database_name: Database_Name,
                                            type: schema },

    open_read_write_obj(Repository_Ontology_Graph, Repository_Ontology_Object, Map, Map_1),
    Instance_Graph = repo_graph{ organization_name: Organization_Name,
                                 database_name: Database_Name,
                                 type: instance },
    open_read_write_obj(Instance_Graph, Instance_Object, Map_1, Map_2),

    Transaction_Object = transaction_object{
                             descriptor : Descriptor,
                             instance_objects : [Instance_Object],
                             schema_objects : [Repository_Ontology_Object],
                             inference_objects : []
                         }.
open_descriptor_(Descriptor, _Commit_Info, Transaction_Object, Map,
                 [Descriptor=Transaction_Object|Map_3]) :-
    repository_descriptor{
        database_descriptor : Database_Descriptor,
        repository_name: Repository_Name
    } :< Descriptor,
    !,
    open_descriptor(Database_Descriptor, _, Database_Transaction_Object, Map, Map_1),

    database_descriptor{
        database_name : Database_Name,
        organization_name : Organization_Name
    } :< Database_Descriptor,

    Ref_Ontology_Graph = commit_graph{ organization_name: Organization_Name,
                                       database_name : Database_Name,
                                       repository_name: Repository_Name,
                                       type: schema },

    Instance_Graph = commit_graph{ organization_name: Organization_Name,
                                   database_name: Database_Name,
                                   repository_name: Repository_Name,
                                   type: instance},

    open_read_write_obj(Ref_Ontology_Graph, Ref_Ontology_Object, Map_1, Map_2),
    open_read_write_obj(Instance_Graph, Instance_Object, Map_2, Map_3),

    Transaction_Object = transaction_object{ parent : Database_Transaction_Object,
                                             descriptor : Descriptor,
                                             instance_objects : [Instance_Object],
                                             schema_objects : [Ref_Ontology_Object],
                                             inference_objects : []
                                           }.
open_descriptor_(Descriptor, Commit_Info, Transaction_Object, Map,
                 [Descriptor=Transaction_Object|Map_2]) :-
    branch_descriptor{ repository_descriptor : Repository_Descriptor,
                       branch_name: Branch_Name } = Descriptor,
    !,
    text_to_string(Branch_Name, Branch_Name_String),

    open_descriptor(Repository_Descriptor, _, Repository_Transaction_Object,
                    Map, Map_1),

    [Repo_Instance_Object] = (Repository_Transaction_Object.instance_objects),
    Repo = layer_descriptor{ instance: (Repo_Instance_Object.read),
                             variety: repository_descriptor },

    do_or_die(has_branch(Repo, Branch_Name_String),
              error(branch_does_not_exist(Descriptor), _)),

    Prototype = branch_graph{
                    organization_name: Repository_Descriptor.database_descriptor.organization_name,
                    database_name : Repository_Descriptor.database_descriptor.database_name,
                    repository_name : Repository_Descriptor.repository_name,
                    branch_name: Branch_Name_String
                },

    Instance_Descriptor = (Prototype.put(type, instance)),
    Schema_Descriptor = (Prototype.put(type, schema)),

    mapm(open_read_write_obj,
         [Instance_Descriptor, Schema_Descriptor],
         [Instance_Object, Schema_Object],
         Map_1, Map_2),

    Transaction_Object = transaction_object{
                             parent : Repository_Transaction_Object,
                             descriptor : Descriptor,
                             commit_info : Commit_Info,
                             instance_objects : [Instance_Object],
                             schema_objects : [Schema_Object],
                             inference_objects : []
                         }.
open_descriptor_(Descriptor, Commit_Info, Transaction_Object, Map,
                 [Descriptor=Transaction_Object|Map_2]) :-
    commit_descriptor{ repository_descriptor : Repository_Descriptor,
                       commit_id: Commit_Id } = Descriptor,
    !,
    text_to_string(Commit_Id, Commit_Id_String),

    open_descriptor(Repository_Descriptor, _, Repository_Transaction_Object,
                    Map, Map_1),

    [Commit_Instance_Object] = Repository_Transaction_Object.instance_objects,
    Repo = layer_descriptor{ instance: (Commit_Instance_Object.read),
                             variety: repository_descriptor },

    do_or_die(has_commit(Repo, Commit_Id),
              error(commit_does_not_exist(Descriptor), _)),

    Prototype = single_commit_graph{
                    organization_name: Repository_Descriptor.database_descriptor.organization_name,
                    database_name : Repository_Descriptor.database_descriptor.database_name,
                    repository_name : Repository_Descriptor.repository_name,
                    commit_id: Commit_Id_String
                },

    Instance_Descriptor = (Prototype.put(type, instance)),
    Schema_Descriptor = (Prototype.put(type, schema)),

    mapm(open_read_write_obj,
         [Instance_Descriptor, Schema_Descriptor],
         [Instance_Object, Schema_Object],
         Map_1, Map_2),

    Transaction_Object = transaction_object{
                             parent : Repository_Transaction_Object,
                             descriptor : Descriptor,
                             commit_info : Commit_Info,
                             instance_objects : [Instance_Object],
                             schema_objects : [Schema_Object],
                             inference_objects : []
                         }.

:- dynamic retained_descriptor_layers/2.

retain_descriptor_layers(Descriptor, Layers) :-
    (   retained_descriptor_layers(Descriptor, Stored_Layers),
        compare_layers(Stored_Layers, Layers)
    ->  true
    ;   retain_descriptor_layers_(Descriptor, Layers)).

retain_descriptor_layers_(Descriptor, Layers) :-
     with_mutex(lock_on_first_descriptor_assert,
                (   retractall(retained_descriptor_layers(Descriptor, _)),
                    asserta(retained_descriptor_layers(Descriptor, Layers)))).

compare_layers([], []).
compare_layers([Layer1|Ls1], [Layer2|Ls2]) :-
    layer_equals(Layer1, Layer2),

    compare_layers(Ls1, Ls2).

open_descriptor(Descriptor, Commit_Info, Transaction_Object, Map_In, Map_Out) :-
    open_descriptor_(Descriptor, Commit_Info, Transaction_Object, Map_In, Map_Out),
    (   should_retain_layers_for_descriptor(Descriptor)
    ->  layers_for_transaction(Transaction_Object, Layers),
        retain_descriptor_layers(Descriptor, Layers)
    ;   true).

open_descriptor(Descriptor, Commit_Info, Transaction_Object) :-
    open_descriptor(Descriptor, Commit_Info, Transaction_Object, [], _),
    log_descriptor_open(Descriptor).

open_descriptor(Descriptor, Transaction_Object) :-
    open_descriptor(Descriptor, commit_info{}, Transaction_Object).

should_retain_layers_for_descriptor(system_descriptor{}).
should_retain_layers_for_descriptor(D) :-
    pinned_databases(Pinned),
    memberchk(D, Pinned).

layers_for_transaction(Transaction, Layers) :-
    _{
        instance_objects: [Instance_RWO],
        schema_objects: [Schema_RWO]
    } :< Transaction,
    Instance_Layer = (Instance_RWO.read),
    Schema_Layer = (Schema_RWO.read),

    Our_Layers_Var = [Instance_Layer, Schema_Layer],
    exclude(var, Our_Layers_Var, Our_Layers),

    (   get_dict(parent, Transaction, Parent)
    ->  append(Our_Layers, Remainder, Layers),
        layers_for_transaction(Parent, Remainder)
    ;   Layers = Our_Layers).

log_descriptor_open(_Descriptor) :-
    % Skip all work if the debug log is not enabled
    \+ debug_log_enabled,
    !,
    true.
log_descriptor_open(Descriptor) :-
    layer_descriptor{} :< Descriptor,
    % We ignore this type as it's often used as part of an internal
    % operation and not interesting for log observers
    !.
log_descriptor_open(Descriptor) :-
    (   resolve_absolute_string_descriptor(S, Descriptor)
    ->  format(string(Message), "open descriptor: ~w", [S])
    ;   Message = "open unprintable descriptor"),
    do_or_die(descriptor_to_loggable(Descriptor, Loggable),
              error(descriptor_sanitize_failed(Descriptor), _)),
    json_log_debug(_{
                       message: Message,
                       descriptorAction: open,
                       descriptor: Loggable
                   }).

descriptor_to_loggable(system_descriptor{}, json{descriptorType: system}) :-
    !.
descriptor_to_loggable(Descriptor, Loggable) :-
    label_descriptor{variety: Variety,
                     instance: Instance} = Descriptor,
    !,
    Loggable = json{
                   descriptorType: label,
                   variety: Variety,
                   instance: Instance
               }.
descriptor_to_loggable(Descriptor, Loggable) :-
    label_descriptor{variety: Variety,
                     schema: Schema,
                     instance: Instance} = Descriptor,
    !,
    Loggable = json{
                   descriptorType: label,
                   variety: Variety,
                   schema: Schema,
                   instance: Instance
               }.
descriptor_to_loggable(Descriptor, Loggable) :-
    id_descriptor{variety: Variety,
                  instanceLabel: Instance} = Descriptor,
    !,
    Loggable = json{
                   descriptorType: id,
                   variety: Variety,
                   instanceLayer: Instance
               }.
descriptor_to_loggable(Descriptor, Loggable) :-
    id_descriptor{variety: Variety,
                  schemaLabel: Schema,
                  instanceLabel: Instance} = Descriptor,
    !,
    Loggable = json{
                   descriptorType: id,
                   variety: Variety,
                   schemaLayer: Schema,
                   instanceLayer: Instance
               }.
descriptor_to_loggable(Descriptor, Loggable) :-
    layer_descriptor{instance:Layer,
                     variety:Variety} = Descriptor,
    !,
    layer_to_id(Layer, Id),
    Loggable = json{descriptorType: layer,
                    instanceLayer: Id,
                    variety: Variety}.
descriptor_to_loggable(Descriptor, Loggable) :-
    database_descriptor{organization_name:Organization,
                        database_name:Database} = Descriptor,
    !,
    Loggable = json{descriptorType: database,
                    organization: Organization,
                    database: Database}.
descriptor_to_loggable(Descriptor, Loggable) :-
    repository_descriptor{database_descriptor:
                          database_descriptor{organization_name:Organization,
                                              database_name:Database},
                          repository_name: Repository} = Descriptor,
    !,
    Loggable = json{descriptorType: repository,
                    organization: Organization,
                    database: Database,
                    repository: Repository}.
descriptor_to_loggable(Descriptor, Loggable) :-
    branch_descriptor{
        repository_descriptor:
        repository_descriptor{database_descriptor:
                              database_descriptor{organization_name:Organization,
                                                  database_name:Database},
                              repository_name: Repository},
        branch_name: Branch} = Descriptor,
    !,
    Loggable = json{descriptorType: branch,
                    organization: Organization,
                    database: Database,
                    repository: Repository,
                    branch: Branch}.
descriptor_to_loggable(Descriptor, Loggable) :-
    commit_descriptor{
        repository_descriptor:
        repository_descriptor{database_descriptor:
                              database_descriptor{organization_name:Organization,
                                                  database_name:Database},
                              repository_name: Repository},
        commit_id: Commit} = Descriptor,
    !,
    Loggable = json{descriptorType: commit,
                    organization: Organization,
                    database: Database,
                    repository: Repository,
                    commit: Commit}.

graph_descriptor_find_read_write_object(_, [], _) :-
        !,
        fail.
graph_descriptor_find_read_write_object(Graph_Descriptor, [Read_Write_Obj|_Read_Write_Objs], Read_Write_Obj) :-
        Graph_Descriptor :< Read_Write_Obj.descriptor,
        !.
graph_descriptor_find_read_write_object(Graph_Descriptor, [_|Read_Write_Objs], Read_Write_Obj) :-
        graph_descriptor_find_read_write_object(Graph_Descriptor, Read_Write_Objs, Read_Write_Obj).

graph_descriptor_transaction_objects_read_write_object(_, [], _) :-
        !,
        fail.
graph_descriptor_transaction_objects_read_write_object(Graph_Descriptor, [Transaction_Object|_Transaction_Objects], Read_Write_Object) :-
        Instances = Transaction_Object.instance_objects,
        graph_descriptor_find_read_write_object(Graph_Descriptor, Instances, Read_Write_Object),
        !.
graph_descriptor_transaction_objects_read_write_object(Graph_Descriptor, [Transaction_Object|_Transaction_Objects], Read_Write_Object) :-
        Schemas = Transaction_Object.schema_objects,
        graph_descriptor_find_read_write_object(Graph_Descriptor, Schemas, Read_Write_Object),
        !.
graph_descriptor_transaction_objects_read_write_object(Graph_Descriptor, [Transaction_Object|_Transaction_Objects], Read_Write_Object) :-
        Inferences = Transaction_Object.inference_objects,
        graph_descriptor_find_read_write_object(Graph_Descriptor, Inferences, Read_Write_Object),
        !.
graph_descriptor_transaction_objects_read_write_object(Graph_Descriptor, [_|Transaction_Objects], Read_Write_Object) :-
        graph_descriptor_transaction_objects_read_write_object(Graph_Descriptor, Transaction_Objects, Read_Write_Object).

/*
 *
 * Find the Transaction Object associated with a graph descriptor
 *
 * (so that inference has a chance of working)
 */

instance_graph_descriptor_transaction_object(Graph_Descriptor, [Transaction_Object|_Transaction_Objects], Transaction_Object) :-
    RW_Objects = Transaction_Object.instance_objects,
    exists({Graph_Descriptor}/[
               read_write_obj{ descriptor : Found_Graph_Descriptor,
                               read : _,
                               write : _ }]>>(Graph_Descriptor :< Found_Graph_Descriptor),
           RW_Objects),
    !.
instance_graph_descriptor_transaction_object(Graph_Descriptor, [_Transaction_Object|Transaction_Objects], Transaction_Object) :-
    instance_graph_descriptor_transaction_object(Graph_Descriptor, Transaction_Objects, Transaction_Object).

collection_descriptor_transaction_object(Collection_Descriptor, [Transaction_Object|_Transaction_Objects], Transaction_Object) :-
    Transaction_Object.descriptor = Collection_Descriptor,
    !.
collection_descriptor_transaction_object(Collection_Descriptor, [_Transaction_Object|Transaction_Objects], Transaction_Object) :-
    collection_descriptor_transaction_object(Collection_Descriptor, Transaction_Objects, Transaction_Object).

read_write_object_to_name(Object, Name) :-
    Name = Object.descriptor.name.

make_branch_descriptor(Organization, DB, Repo_Name, Branch_Name, Branch_Descriptor) :-
    Database_Descriptor = database_descriptor{ organization_name: Organization,
                                               database_name : DB },
    Repository_Descriptor = repository_descriptor{ repository_name : Repo_Name,
                                                   database_descriptor : Database_Descriptor},
    Branch_Descriptor = branch_descriptor{ branch_name : Branch_Name,
                                           repository_descriptor : Repository_Descriptor}.

make_branch_descriptor(Organization, DB, Repo_Name, Branch_Descriptor) :-
    make_branch_descriptor(Organization, DB, Repo_Name, "main", Branch_Descriptor).

make_branch_descriptor(Organization, DB, Branch_Descriptor) :-
    make_branch_descriptor(Organization, DB, "local", "main", Branch_Descriptor).

/**
 * transactions_to_map(Context, Map) is det.
 *
 * Constructs a map of descriptor=... for each object necessary to be
 * re-used by open_descriptor/5
 */
transactions_to_map(Transactions, Map) :-
    mapm(transaction_to_map, Transactions, [], Map).

transaction_to_map(Transaction, Map_In, Map_Out) :-
    (   get_dict(parent,Transaction, Parent)
    ->  transaction_to_map(Parent,Map_In,Map1)
    ;   Map1 = Map_In),

    Instance_Objects = Transaction.instance_objects,
    Schema_Objects = Transaction.schema_objects,
    Inference_Objects = Transaction.inference_objects,
    append([Instance_Objects, Schema_Objects, Inference_Objects], Objects),
    convlist({Map1}/[Obj,Desc=Obj]>>(
                 get_dict(descriptor,Obj,Desc),
                 \+ memberchk(Desc=_,Map1)
             ), Objects, Graph_Map),
    union(Graph_Map, Map1, Map2),
    Descriptor = Transaction.descriptor,
    (   memberchk(Descriptor=_,Map2)
    ->  Map_Out = Map2
    ;   Map_Out = [Descriptor=Transaction|Map2]).

/**
 * collection_descriptor_graph_filter_graph_descriptor(+Descriptor,+Filter,?Graph) is semidet.
 * collection_descriptor_graph_filter_graph_descriptor(?Descriptor,+Filter,+Graph) is semidet.
 * collection_descriptor_graph_filter_graph_descriptor(+Descriptor,?Filter,+Graph) is semidet.
 *
 * Constructs any argument from the other two.
 *
 */
collection_descriptor_graph_filter_graph_descriptor(
    system_descriptor{},
    type_name_filter{ type : Type},
    system_graph{ type: Type}) :-
    !.
collection_descriptor_graph_filter_graph_descriptor(
    system_descriptor{},
    type_filter{ types : [Type] },
    system_graph{ type: Type}) :-
    !.
collection_descriptor_graph_filter_graph_descriptor(
    database_descriptor{
        organization_name: Organization,
        database_name : DB_Name
    },
    type_name_filter{ type : Type},
    repo_graph{ organization_name: Organization,
                database_name : DB_Name,
                type : Type }) :-
    !.
collection_descriptor_graph_filter_graph_descriptor(
    database_descriptor{
        organization_name: Organization,
        database_name : DB_Name
    },
    type_filter{ types : [Type]},
    repo_graph{ organization_name: Organization,
                database_name : DB_Name,
                type : Type }) :-
    !.
collection_descriptor_graph_filter_graph_descriptor(
    repository_descriptor{
        database_descriptor : database_descriptor{
                                  organization_name: Organization,
                                  database_name : DB_Name
                              },
        repository_name : Repo_Name
    },
    type_name_filter{ type : Type},
    commit_graph{ organization_name: Organization,
                  database_name : DB_Name,
                  repository_name : Repo_Name,
                  type: Type}) :-
    !.
collection_descriptor_graph_filter_graph_descriptor(
    repository_descriptor{
        database_descriptor : database_descriptor{
                                  organization_name: Organization,
                                  database_name : DB_Name
                              },
        repository_name : Repo_Name
    },
    type_filter{ types : [Type]},
    commit_graph{ organization_name: Organization,
                  database_name : DB_Name,
                  repository_name : Repo_Name,
                  type: Type}) :-
    !.
collection_descriptor_graph_filter_graph_descriptor(
    branch_descriptor{
        repository_descriptor :
        repository_descriptor{
            database_descriptor :
            database_descriptor{
                organization_name: Organization,
                database_name : DB_Name
            },
            repository_name : Repository_Name
        },
        branch_name : Branch_Name
    },
    type_name_filter{ type : Type},
    branch_graph{ organization_name: Organization,
                  database_name : DB_Name,
                  repository_name : Repository_Name,
                  branch_name : Branch_Name,
                  type: Type}) :-
    !.
collection_descriptor_graph_filter_graph_descriptor(
    branch_descriptor{
        repository_descriptor :
        repository_descriptor{
            database_descriptor :
            database_descriptor{
                organization_name: Organization,
                database_name : DB_Name
            },
            repository_name : Repository_Name
        },
        branch_name : Branch_Name
    },
    type_filter{ types : [Type] },
    branch_graph{ organization_name: Organization,
                  database_name : DB_Name,
                  repository_name : Repository_Name,
                  branch_name : Branch_Name,
                  type: Type}) :-
    !.
collection_descriptor_graph_filter_graph_descriptor(
    Label_Descriptor,
    type_name_filter{ type : Type },
    labelled_graph{ label: Label,
                    type: Type }) :-
    label_descriptor{} :< Label_Descriptor,
    get_dict(Type,Label_Descriptor,Label).

collection_descriptor_prefixes_(system_descriptor, Prefixes) :-
    Prefixes = _{ '@base': 'terminusdb:///system/data/',
                  '@schema': 'http://terminusdb.com/schema/system#' }.
collection_descriptor_prefixes_(database_descriptor, Prefixes) :-
    Prefixes = _{'@base' : 'terminusdb://repository/data/',
                 '@schema' : 'http://terminusdb.com/schema/repository#',
                 'layer' : "http://terminusdb.com/schema/layer#",
                 'layer_data' : "terminusdb://layer/data/"
                }.
collection_descriptor_prefixes_(repository_descriptor, Prefixes) :-
    Prefixes = _{'@base' : 'terminusdb://ref/data/',
                 '@schema' : 'http://terminusdb.com/schema/ref#',
                 'layer' : "http://terminusdb.com/schema/layer#",
                 'layer_data' : "terminusdb://layer/data/"
                }.
collection_descriptor_prefixes_(branch_descriptor, Prefixes) :-
    Prefixes = _{}.
collection_descriptor_prefixes_(commit_descriptor, Prefixes) :-
    Prefixes = _{}.

descriptor_variety(Descriptor, Variety) :-
    get_dict(variety, Descriptor, Variety),
    !.
descriptor_variety(Descriptor, Variety) :-
    Variety{} :< Descriptor.

collection_descriptor_prefixes(Descriptor, Prefixes) :-
    default_prefixes(Default_Prefixes),
    descriptor_variety(Descriptor, Variety),
    collection_descriptor_prefixes_(Variety, Nondefault_Prefixes),
    Prefixes = (Default_Prefixes.put(Nondefault_Prefixes)).

collection_descriptor_default_write_graph(system_descriptor{}, Graph_Descriptor) :-
    !,
    Graph_Descriptor = system_graph{
                           type : instance
                       }.
collection_descriptor_default_write_graph(Descriptor, Graph_Descriptor) :-
    database_descriptor{ organization_name : Organization,
                         database_name : Database } = Descriptor,
    !,
    Graph_Descriptor = repo_graph{
                           organization_name : Organization,
                           database_name : Database,
                           type : instance
                       }.
collection_descriptor_default_write_graph(Descriptor, Graph_Descriptor) :-
    repository_descriptor{
        database_descriptor : Database_Descriptor,
        repository_name : Repository_Name
    } = Descriptor,
    !,
    database_descriptor{ organization_name : Organization,
                         database_name : Database_Name } = Database_Descriptor,
    Graph_Descriptor = commit_graph{
                           organization_name : Organization,
                           database_name : Database_Name,
                           repository_name : Repository_Name,
                           type : instance
                       }.
collection_descriptor_default_write_graph(Descriptor, Graph_Descriptor) :-
    branch_descriptor{ branch_name : Branch_Name,
                       repository_descriptor : Repository_Descriptor
                     } :< Descriptor,
    !,
    repository_descriptor{
        database_descriptor : Database_Descriptor,
        repository_name : Repository_Name
    } :< Repository_Descriptor,
    database_descriptor{
        organization_name : Organization,
        database_name : Database_Name
    } :< Database_Descriptor,

    Graph_Descriptor = branch_graph{
                           organization_name : Organization,
                           database_name : Database_Name,
                           repository_name : Repository_Name,
                           branch_name : Branch_Name,
                           type : instance
                       }.
collection_descriptor_default_write_graph(Descriptor, Graph_Descriptor) :-
    label_descriptor{ instance: Label} :< Descriptor,
    !,
    text_to_string(Label, Label_String),
    Graph_Descriptor = labelled_graph{label:Label_String,
                                      type: instance
                                     }.
collection_descriptor_default_write_graph(_, empty).

:- begin_tests(open_descriptor, [concurrent(true)]).
:- use_module(core(util/test_utils)).
:- use_module(library(terminus_store)).
:- use_module(core(api)).
:- use_module(database).
:- use_module(core(util)).
:- use_module(library(ordsets)).

test(transactions_to_map,[
         setup((setup_temp_store(State),
                create_db_without_schema(admin,test))),
         cleanup(teardown_temp_store(State))
     ])
:-
    make_branch_descriptor("admin", "test", Descriptor),
    create_context(Descriptor, commit_info{author: "Me", message: "Soldier on"}, Context1),
    with_transaction(
        Context1,
        ask(Context1, insert(some,stuff,here)),
        _Meta_Data
    ),

    create_context(Descriptor, Context2),
    transactions_to_map(Context2.transaction_objects, Map),

    maplist([Desc=_,Desc]>>true, Map, Descriptors),
    list_to_ord_set(Descriptors, Desc_Set),

    list_to_ord_set(
        [branch_descriptor{branch_name:"main",
                           repository_descriptor:
                           repository_descriptor{
                               database_descriptor:
                               database_descriptor{
                                   database_name:"test",
                                   organization_name:"admin"},
                               repository_name:"local"}},
         database_descriptor{database_name:"test",
                             organization_name:"admin"},
         repository_descriptor{database_descriptor:
                               database_descriptor{
                                   database_name:"test",
                                   organization_name:"admin"},
                               repository_name:"local"},
         repo_graph{database_name:"test",organization_name:"admin",type:instance},
         repo_graph{database_name:"test",organization_name:"admin",type:schema},
         commit_graph{database_name:"test",
                      organization_name:"admin",
                      repository_name:"local",
                      type:instance},
         commit_graph{database_name:"test",
                      organization_name:"admin",
                      repository_name:"local",
                      type:schema},
         branch_graph{branch_name:"main",
                      commit_type:'http://terminusdb.com/schema/ref#ValidCommit',
                      database_name:"test",
                      organization_name:"admin",
                      repository_name:"local",
                      type:instance},
         branch_graph{branch_name:"main",
                      commit_type:'http://terminusdb.com/schema/ref#ValidCommit',
                      database_name:"test",
                      organization_name:"admin",
                      repository_name:"local",
                      type:schema}], Expected_Set),

    ord_seteq(Desc_Set, Expected_Set).

test(terminus, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ])
:-
    Descriptor = system_descriptor{},
    open_descriptor(Descriptor, Transaction),

    once(ask(Transaction, isa('SystemDatabase/system', 'SystemDatabase'))).

test(label, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ])
:-
    Descriptor = label_descriptor{instance: "test",
                                  variety: system_descriptor},

    triple_store(Store),
    create_named_graph(Store, test, Graph),
    open_write(Store, Builder),
    nb_add_triple(Builder, foo, bar, node(baz)),
    nb_commit(Builder, Layer),
    nb_set_head(Graph, Layer),

    open_descriptor(Descriptor, Transaction),
    once(ask(Transaction, t(X, Y, Z))),
    X = foo, Y = bar, Z = baz.

test(id, [
         setup(setup_temp_store(State)),
         cleanup(teardown_temp_store(State))
     ])
:-
    triple_store(Store),
    open_write(Store, Builder),
    nb_add_triple(Builder, foo, bar, node(baz)),
    nb_commit(Builder, Layer),
    layer_to_id(Layer, Id),

    Descriptor = id_descriptor{variety: system_descriptor,
                               instance: Id},

    open_descriptor(Descriptor, Transaction),
    once(ask(Transaction, t(X, Y, Z))),
    X = foo, Y = bar, Z = baz.

test(open_database_descriptor_as_atom, [
         setup((setup_temp_store(State),
                create_db_without_schema(admin,testdb))),
         cleanup(teardown_temp_store(State))
     ])
:-
    Descriptor = database_descriptor{ organization_name: admin,
                                      database_name: testdb },
    open_descriptor(Descriptor, _Transaction).

test(open_database_descriptor_as_string, [
         setup((setup_temp_store(State),
                create_db_without_schema(admin,testdb))),
         cleanup(teardown_temp_store(State))
     ])
:-
    Descriptor = database_descriptor{ organization_name: "admin",
                                      database_name: "testdb" },
    open_descriptor(Descriptor, _Transaction).

test(open_nonexistent_database_descriptor, [
         setup((setup_temp_store(State),
                create_db_without_schema(admin, testdb))),
         cleanup(teardown_temp_store(State))
     ])
:-
    Descriptor = database_descriptor{ organization_name: "admin",
                                      database_name: "nonexistent" },
    \+ open_descriptor(Descriptor, _Transaction).

test(open_repository_descriptor_with_atom, [
         setup((setup_temp_store(State),
                create_db_without_schema(admin,testdb))),
         cleanup(teardown_temp_store(State))
     ])
:-
    Database_Descriptor = database_descriptor{ organization_name: "admin",
                                               database_name: "testdb" },
    Repo_Descriptor = repository_descriptor{ database_descriptor: Database_Descriptor, repository_name: local },

    open_descriptor(Repo_Descriptor, _Transaction).

test(open_repository_descriptor_with_string, [
         setup((setup_temp_store(State),
                create_db_without_schema(admin,testdb))),
         cleanup(teardown_temp_store(State))
     ])
:-
    Database_Descriptor = database_descriptor{ organization_name: "admin",
                                               database_name: "testdb" },
    Repo_Descriptor = repository_descriptor{ database_descriptor: Database_Descriptor, repository_name: "local" },

    open_descriptor(Repo_Descriptor, _Transaction).

test(open_repository_descriptor_with_string, [
         setup((setup_temp_store(State),
                create_db_without_schema(admin,testdb))),
         cleanup(teardown_temp_store(State))
     ])
:-
    Database_Descriptor = database_descriptor{ organization_name: "admin",
                                               database_name: "testdb" },
    Repo_Descriptor = repository_descriptor{ database_descriptor: Database_Descriptor, repository_name: "nonexistent" },

    \+ open_descriptor(Repo_Descriptor, _Transaction).

test(open_branch_descriptor_with_atom, [
         setup((setup_temp_store(State),
                create_db_without_schema(admin,testdb))),
         cleanup(teardown_temp_store(State))
     ])
:-
    Database_Descriptor = database_descriptor{ organization_name: "admin",
                                               database_name: "testdb" },
    Repo_Descriptor = repository_descriptor{ database_descriptor: Database_Descriptor, repository_name: "local" },
    Branch_Descriptor = branch_descriptor{ repository_descriptor: Repo_Descriptor, branch_name: main },

    open_descriptor(Branch_Descriptor, _Transaction).

test(open_branch_descriptor_with_string, [
         setup((setup_temp_store(State),
                create_db_without_schema(admin,testdb))),
         cleanup(teardown_temp_store(State))
     ])
:-
    Database_Descriptor = database_descriptor{ organization_name: "admin",
                                               database_name: "testdb" },
    Repo_Descriptor = repository_descriptor{ database_descriptor: Database_Descriptor, repository_name: "local" },
    Branch_Descriptor = branch_descriptor{ repository_descriptor: Repo_Descriptor, branch_name: "main" },

    open_descriptor(Branch_Descriptor, _Transaction).

test(open_branch_descriptor_with_nonexistent, [
         setup((setup_temp_store(State),
                create_db_without_schema(admin,testdb))),
         cleanup(teardown_temp_store(State)),
         error(branch_does_not_exist(_))
     ])
:-
    Database_Descriptor = database_descriptor{ organization_name: "admin",
                                               database_name: "testdb" },
    Repo_Descriptor = repository_descriptor{ database_descriptor: Database_Descriptor, repository_name: "local" },
    Branch_Descriptor = branch_descriptor{ repository_descriptor: Repo_Descriptor, branch_name: "nonexistent" },

    open_descriptor(Branch_Descriptor, _Transaction).

test(open_commit_descriptor_with_atom, [
         setup((setup_temp_store(State),
                create_db_without_schema("admin", "testdb"))),
         cleanup(teardown_temp_store(State))
     ])
:-
    Database_Descriptor = database_descriptor{ organization_name: "admin",
                                               database_name: "testdb" },
    Repo_Descriptor = repository_descriptor{ database_descriptor: Database_Descriptor, repository_name: "local" },
    Branch_Descriptor = branch_descriptor{ repository_descriptor: Repo_Descriptor, branch_name: "main" },

    % add a commit to open later
    create_context(Branch_Descriptor,
                   commit_info{author: "flurps",
                               message: "flarps flerps florps"},
                   Branch_Context),

    with_transaction(Branch_Context,
                     once(ask(Branch_Context,
                              insert(foo, bar, baz))),
                     _),

    branch_head_commit(Repo_Descriptor, "main", Commit_Uri),
    commit_id_uri(Repo_Descriptor, Commit_Id, Commit_Uri),

    atom_string(Commit_Id_Atom, Commit_Id),

    Descriptor = commit_descriptor{ repository_descriptor: Repo_Descriptor, commit_id: Commit_Id_Atom },
    open_descriptor(Descriptor, Transaction),

    once(ask(Transaction, t(foo, bar, baz))).

test(open_commit_descriptor_with_string, [
         setup((setup_temp_store(State),
                create_db_without_schema(admin,testdb))),
         cleanup(teardown_temp_store(State))
     ])
:-
    Database_Descriptor = database_descriptor{ organization_name: "admin",
                                               database_name: "testdb" },
    Repo_Descriptor = repository_descriptor{ database_descriptor: Database_Descriptor, repository_name: "local" },
    Branch_Descriptor = branch_descriptor{ repository_descriptor: Repo_Descriptor, branch_name: "main" },

    % add a commit to open later
    create_context(Branch_Descriptor,
                   commit_info{author: "flurps",
                               message: "flarps flerps florps"},
                   Branch_Context),

    with_transaction(Branch_Context,
                     once(ask(Branch_Context,
                              insert(foo, bar, baz))),
                     _),

    branch_head_commit(Repo_Descriptor, "main", Commit_Uri),
    commit_id_uri(Repo_Descriptor, Commit_Id, Commit_Uri),

    Descriptor = commit_descriptor{ repository_descriptor: Repo_Descriptor,
                                    commit_id: Commit_Id },
    open_descriptor(Descriptor, Transaction),

    once(ask(Transaction, t(X, Y, Z))),
    X = foo, Y = bar, Z = baz.

test(open_commit_descriptor_with_nonexistent, [
         setup((setup_temp_store(State),
                create_db_without_schema(admin,testdb))),
         cleanup(teardown_temp_store(State))
     ])
:-
    Database_Descriptor = database_descriptor{ organization_name: "admin",
                                               database_name: "testdb" },
    Repo_Descriptor = repository_descriptor{ database_descriptor: Database_Descriptor, repository_name: "local" },
    Descriptor = commit_descriptor{ repository_descriptor: Repo_Descriptor, commit_id: "I do not exist" },
    catch(open_descriptor(Descriptor, _Transaction),
          error(E,_),
          true),

    E = commit_does_not_exist(_).


:- end_tests(open_descriptor).
