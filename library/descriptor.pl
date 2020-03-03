:- module(descriptor,[
              open_descriptor/2,
              open_descriptor/3,
              open_descriptor/5,
              collection_descriptor_transaction_object/3,
              instance_graph_descriptor_transaction_object/3
          ]).

/** <module> Descriptor Manipulation
 *
 * This file deals with the manipulation and opening of descriptors.
 *
 * Types:
 *
 * graph_descriptor --> labelled_graph{ name : atom }
 *                    | id_graph{ layer_id : atom } % for debugging
 *                    | repo_graph { database_name : atom }
 *                    | commit_graph{ database_name : atom,
 *                                    repository_name : atom }
 *                    | branch_graph{ database_name : atom,
 *                                    repository_name : atom,
 *                                    branch_name : atom,
 *                                    type : atom, % {instance, schema, inference}
 *                                    name : atom }
 *
 * A named_graph refers to a file in the store - this has to be made unique so we don't get
 * collisions between different databases. Currently only used for the terminus and database graph
 * - one per repository. This uses the file label mechanism.
 *
 * A ref_graph is a layer id that can be resolved to a graph.
 *
 * collection_descriptor --> terminus_descriptor{}
 *                         | database_descriptor{ database_name : uri }
 *                         | repository_descriptor{ database_descriptor : database_descriptor,
 *                                                  repository_name : uri }
 *                         | branch_descriptor{ repository_descriptor: repository_descriptor,
 *                                              branch_name : uri}, % the name of the thing advancing
 *                         | commit_descriptor{ repository_descriptor: repository_descriptor,
 *                                              last_commit : uri} % the base of the commit
 *
 * terminus_descriptor: refers to the core database with user and database management.
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
 * read_write_obj ---> read_obj{ descriptor : graph_descriptor, read : Layer, write: Var_Or_Layer_Builder}
 *
 * transaction_object ---> transaction_object{ descriptor : collection_descriptor,
 *                                 <parent : transaction_object>, % except for database/terminus descriptors
 *                                 <commit_info : commit_info>, % only ref_descriptors
 *                                 instance_objects : list(read_write_obj),
 *                                 schema_objects : list(read_write_obj),
 *                                 inference_objects : list(read_write_obj) }
 */

/* * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
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

:- use_module(terminus_bootstrap).
:- use_module(triplestore).

graph_descriptor_to_layer(Descriptor, Layer, Map, Map) :-
    memberchk(Descriptor=Layer, Map),
    !.
graph_descriptor_to_layer(Descriptor, Layer, Map, [Descriptor=Layer|Map]) :-
    Descriptor = labelled_graph{ name: Name },
    !,
    storage(Store),
    safe_open_named_graph(Store, Name, Graph),
    head(Graph, Layer).
graph_descriptor_to_layer(Descriptor, Layer, Map, [Descriptor=Layer|Map]) :-
    Descriptor = id_graph{ layer_id: Layer_Id },
    !,
    storage(Store),
    store_id_layer(Store, Layer_Id, Layer).
graph_descriptor_to_layer(Descriptor, Layer, Map, [Descriptor=Layer|Map]) :-
    Descriptor = repo_graph{ database_name: Name},
    !,
    storage(Store),
    safe_open_named_graph(Store, Name, Graph),
    head(Graph, Layer).
graph_descriptor_to_layer(Descriptor,
                          Layer,
                          Map,
                          [Descriptor=Layer|New_Map]) :-
    Descriptor = commit_graph{ database_name: Database_Name,
                               repository_name: Repository_Name},
    !,
    Repo_Descriptor = repo_graph{ database_name: Database_Name},
    graph_descriptor_to_layer(Repo_Descriptor, Repository_Layer, Map, New_Map),
    repo_layer_name_to_ref_layer_id(Repository_Layer, Repository_Name, Commit_Layer_Id),
    store_id_layer(Store, Commit_Layer_Id, Layer).
graph_descriptor_to_layer(Descriptor,
                          Layer,
                          Map,
                          [Descriptor=Layer|New_Map]) :-
    Descriptor = branch_graph{ database_name: Database_Name,
                               repository_name: Repository_Name,
                               branch_name: Ref_Uri,
                               type: Type,
                               name: Graph_Name },
    !,
    Commit_Descriptor = commit_graph{ database_name : Database_Name,
                                      repository_name : Repository_Name },
    graph_descriptor_to_layer(Commit_Descriptor, Commit_Layer, Map, New_Map),
    commit_layer_branch_type_name_to_data_layer_id(Commit_Layer, Type, Graph_Name, Layer_Id), % todo this does not exist yet
    store_id_layer(Store, Layer_Id, Layer).

open_read_write_obj(Descriptor, read_write_obj{ descriptor: Descriptor, read: Layer, write: Layer_Builder }, Map, New_Map) :-
    graph_descriptor_to_layer(Descriptor, Layer, Map, New_Map),
    freeze(Layer_Builder, open_write(Layer, Layer_Builder)).

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
open_descriptor(Descriptor, _Commit_Info, Transaction_Object, Map, Map) :-
    memberchk(Descriptor=Transaction_Object, Map),
    !.
open_descriptor(terminus_descriptor{}, _Commit_Info, Transaction_Object, Map,
                 [terminus_descriptor{}=Transaction_Object|Map_3]) :-
    !,

    terminus_schema_name(Schema_Name),
    Schema_Graph = labelled_graph{ name : Schema_Name },

    terminus_instance_name(Instance_Name),
    Instance_Graph = labelled_graph{ name : Instance_Name },

    terminus_inference_name(Inference_Name),
    Inference_Graph = labelled_graph{ name : Inference_Name },

    open_read_write_obj(Schema_Graph, Schema_Object, Map, Map_1),
    open_read_write_obj(Instance_Graph, Instance_Object, Map_1, Map_2),
    open_read_write_obj(Inference_Graph, Inference_Object, Map_2, Map_3),

    Transaction_Object = transaction_object{
                             descriptor : terminus_descriptor{},
                             instance_objects : [Instance_Object],
                             schema_objects : [Schema_Object],
                             inference_objects : [Inference_Object]
                         }.
open_descriptor(Descriptor, _Commit_Info, Transaction_Object, Map,
                 [Descriptor=Transaction_Object|Map_3]) :-
    database_descriptor{
        name: Database_Name
    } = Descriptor,
    !,
    layer_ontology(Layer_Ontology_Name),
    Layer_Ontology_Graph = labelled_graph{ name : Layer_Ontology_Name },
    repository_ontology(Repository_Ontology_Name),
    Repository_Ontology_Graph = labelled_graph{ name : Repository_Ontology_Name },

    open_read_write_obj(Layer_Ontology_Graph, Layer_Ontology_Object, Map, Map_1),
    open_read_write_obj(Repository_Ontology_Graph, Repository_Ontology_Object, Map_1, Map_2),
    Instance_Graph = repo_graph{ database_name: Database_Name },
    open_read_write_obj(Instance_Graph, Instance_Object, Map_2, Map_3),

    Transaction_Object = transaction_object{
                             descriptor : Descriptor,
                             instance_objects : [Instance_Object],
                             schema_objects : [Layer_Ontology_Object, Repository_Ontology_Object],
                             inference_objects : []
                         }.
open_descriptor(Descriptor, _Commit_Info, Transaction_Object, Map,
                 [Descriptor=Transaction_Object|New_Map]) :-
    repository_descriptor{
        database_descriptor : Database_Descriptor,
        repository_name: Repository_Name
    } = Descriptor,
    !,
    open_descriptor(Database_Descriptor, _, Database_Transaction_Object, Map, Map_1),

    layer_ontology(Layer_Ontology_Name),
    Layer_Ontology_Graph = labelled_graph{ name : Layer_Ontology_Name },
    ref_ontology(Ref_Ontology_Name),
    Ref_Ontology_Graph = labelled_graph{ name : Ref_Ontology_Name },

    Instance_Graph = commit_graph{ database_name: Database_Descriptor.database_name,
                                   repository_name: Repository_Name },

    open_read_write_obj(Layer_Ontology_Graph, Layer_Ontology_Object, Map_1, Map_2),
    open_read_write_obj(Ref_Ontology_Graph, Ref_Ontology_Object, Map_2, Map_3),
    open_read_write_obj(Instance_Graph, Instance_Object, Map_3, Map_4),

    Transaction_Object = transaction_object{
                             parent : Database_Transaction_Object,
                             descriptor : Descriptor,
                             instance_objects : [Instance_Object],
                             schema_objects : [Layer_Ontology_Object, Ref_Ontology_Object],
                             inference_objects : []
                         }.
open_descriptor(Descriptor, Commit_Info, Transaction_Object, Map,
                 [Descriptor=Transaction_Object|Map_4]) :-
    branch_descriptor{ repository_descriptor : Repository_Descriptor,
                       branch_name: Branch_Name } = Descriptor,
    !,

    open_descriptor(Repository_Descriptor, _, Repository_Transaction_Object,
                     Map, Map_1),

    [Instance_Object] = Repository_Transaction_Object.instance_objs,

    ref_layer_branch_commit(Instance_Object.read, Branch_Name, Commit),
    ref_layer_commit_graphs(Instance_Object.read, Commit,
                            Instance_Names,Schema_Names,Inference_Names),

    Prototype = branch_graph{
                    database_name : Repository_Descriptor.database_descriptor.database_name,
                    repository_name : Repository_Descriptor.repository_name,
                    branch_name: Branch_Name
                },
    maplist({Prototype}/[Instance_Name,Graph_Descriptor]>>(
                Graph_Descriptor = Prototype.put(_{type : instance,
                                                   name : Instance_Name})),
            Instance_Names,
            Instance_Descriptors),
    mapm([Instance_Descriptor,Instance_Object]>>(open_read_write_obj(Instance_Descriptor, Instance_Object)),
         Instance_Descriptors, Instance_Objects,
         Map_1, Map_2),

    maplist({Prototype}/[Schema_Name,Graph_Descriptor]>>(
                Graph_Descriptor = Prototype.put(_{type : schema,
                                                   name : Schema_Name})),
            Schema_Names,
            Schema_Descriptors),
    mapm([Schema_Descriptor,Schema_Object]>>(open_read_write_obj(Schema_Descriptor, Schema_Object)),
         Schema_Descriptors, Schema_Objects,
         Map_2, Map_3),

    maplist({Prototype}/[Inference_Name,Graph_Descriptor]>>(
                Graph_Descriptor = Prototype.put(_{type : inference,
                                                   name : Inference_Name})),
            Inference_Names,
            Inference_Descriptors),
    mapm([Inference_Descriptor,Inference_Object]>>(open_read_write_obj(Inference_Descriptor, Inference_Object)),
         Inference_Descriptors, Inference_Objects,
         Map_3, Map_4),

    Transaction_Object = transaction_object{
                             parent : Repository_Transaction_Object,
                             descriptor : Descriptor,
                             commit_info : Commit_Info,
                             instance_objects : Instance_Objects,
                             schema_objects : Schema_Objects,
                             inference_objects : Inference_Objects
                         }.

open_descriptor(Descriptor, Commit_Info, Transaction_Object) :-
    open_descriptor(Descriptor, Commit_Info, Transaction_Object, [], _).

open_descriptor(Descriptor, Transaction_Object) :-
    open_descriptor(Descriptor, commit_info{}, Transaction_Object).

/*
 *
 * Find the Transaction Object associated with a graph descriptor
 *
 * (so that inference has a chance of working)
 */

instance_graph_descriptor_transaction_object(Graph_Descriptor, [Transaction_Object|_Transaction_Objects], Transaction_Object) :-
    RW_Objects = Transaction_Object.instance_objects,
    exists({G_Descriptor}/[
               read_write_obj{ descriptor : G_Descriptor,
                               read : _,
                               write : _ }]>>true,
           RW_Objects),
    !.
instance_graph_descriptor_transaction_object(Graph_Descriptor, [_Transaction_Object|Transaction_Objects], Transaction_Object) :-
    instance_graph_descriptor_transaction_object(Graph_Descriptor, Transaction_Objects, Transaction_Object).

collection_descriptor_transaction_object(Collection_Descriptor, [Transaction_Object|Transaction_Objects], Transaction_Object) :-
    Transaction_Object.descriptor = Collection_Descriptor,
    !.
collection_descriptor_transaction_object(Collection_Descriptor, [Transaction_Object|Transaction_Objects], Transaction_Object) :-
    collection_descriptor_transaction_object(Collection_Descriptor, Transaction_Objects, Transaction_Object).
