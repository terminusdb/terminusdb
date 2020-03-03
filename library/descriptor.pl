:- module(descriptor,[
              open_descriptor/2
          ]).

/** <module> Descriptor Manipulation
 *
 * This file deals with the manipulation and opening of descriptors.
 *
 * Types:
 *
 * graph_descriptor --> labelled_graph{ name : atom }
 *                    | id_graph{ layer_id : atom } % for debugging
 *                    | repo_graph { database_name : atom,
 *                                   type : atom } % {instance, schema}
 *                    | commit_graph{ database_name : atom,
 *                                    repository_name : atom,
 *                                    type : atom } % {instance, schema}
 *                    | ref_graph{ database_name : atom,
 *                                 repository_name : atom,
 *                                 ref_name : atom,
 *                                 type : atom, % {instance, schema, inference}
 *                                 name : atom }
 *
 * A named_graph refers to a file in the store - this has to be made unique so we don't get
 * collisions between different databases. Currently only used for the terminus and database graph
 * - one per repository. This uses the file label mechanism.
 *
 * A ref_graph is a layer id that can be resolved to a graph.
 *
 * collection_descriptor --> terminus_descriptor
 *                         | database_descriptor{ database_name : uri,
 *                                                instance : list(graph_descriptor)}
 *                         | repository_descriptor{ database_descriptor : database_descriptor,
 *                                                  repository_name : uri,
 *                                                  instance : list(graph_descriptor)}
 *                         | ref_descriptor{ repository_descriptor: repository_descriptor,
 *                                           ref_name : uri, % the name of the thing advancing
 *                                           last_commit : uri, % the base of the commit
 *                                           author : string,
 *                                           message : string,
 *                                           schema : list(graph_descriptor),
 *                                           instance : list(graph_descriptor),
 *                                           inference : list(graph_descriptor)}
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
 *                                 <commit_author : string>, % only ref_descriptors
 *                                 <commit_message : string>, % only ref_descriptors
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

read_write_builder(Read_Write_Object, Builder) :-
.

/**
 * open_write_obj(Graph_Descriptor : graph_descriptor, Write_Object : write_object) is det.
 *
 * opens a graph descriptor with a writer builder.
 */
open_write_obj(Graph, write_obj{ descriptor : Graph, write : Layer_Builder }) :-
    Graph = named_graph{ name : Name },
    storage(Store),
    safe_open_named_graph(Store,Name,Obj),
    open_write(Obj,Layer_Builder).
open_write_obj(Graph, write_obj{ descriptor : Graph, write : Layer_Builder }) :-
    Graph = ref_graph{ layer_id : Layer_ID },
    storage(Store),
    store_id_layer(Store,Layer_ID,Layer),
    open_write(Layer,Layer_Builder).

open_graph_descriptor(Graph, read_write_obj { descriptor : Graph,
                                              read : Layer,
                                              write : _Builder}) :-
    labelled_graph{ name : Name } = Graph,
    !,
    storage(Store),
    safe_open_named_graph(Store,Name,Obj),
    head(Obj,Layer).
open_graph_descriptor(Graph, read_write_obj{ descriptor : Graph,
                                     read : Layer ,
                                     write : Write
                                   }) :-
    Graph = id_graph{ layer_id : Layer_ID },
    !,
    storage(Store),
    store_id_layer(Store,Layer_ID,Layer).


/**
 * descriptor_query(Descriptor, Read_Graph_Descriptors, Write_Graph_Descriptors, Map, New_Map) is det.
 *
 * Impements the logic which allows us to special-case the
 * opening of descriptors for repository, branch and graph.
 *
 * It opens descriptors like an onion, avoiding duplicate opens with a Map.
 *
 * @Descriptor has type collection_descriptor
 * @Read_Graph_Descriptor has type list(collection_descriptor)
 *                        This specifies which read queries to open.
 * @Write_Graph_Descriptor has type list(collection_descriptor)
 *                         This specifies which write queries to open.
 * @Map has type list(descriptor=query_object)
 *      Keeps track of the descriptors which have already been opened so we are monotonic
 * @New_Map has type list(descriptor=query_object)
 *      Updated map
 */
descriptor_query(Descriptor, _Read_Graph_Descriptors, _Write_Graph_Descriptors, Map, Map) :-
    memberchk(Descriptor=_, Map),
    !.
descriptor_query(terminus_descriptor, Read_Graph_Descriptors, Write_Graph_Descriptors, Map,
                 [terminus_descriptor=Query_Object|Map]) :-
    !,

    terminus_schema_name(Schema_Name),
    Schema_Graph = named_graph{ name : Schema_Name },

    terminus_instance_name(Instance_Name),
    Instance_Graph = named_graph{ name : Instance_Name },

    terminus_inference_name(Inference_Name),
    Inference_Graph = named_graph{ name : Inference_Name },

    % Get hard coded schema read/write objects
    included_read_objects([Schema_Graph],Read_Graph_Descriptors, Schema_Read_Objects),
    included_write_objects([Schema_Graph],Write_Graph_Descriptors, Schema_Write_Objects),

    % Get hard coded instance read/write objects
    included_read_objects([Instance_Graph],Read_Graph_Descriptors,Instance_Read_Objects),
    included_write_objects([Instance_Graph],Write_Graph_Descriptors, Instance_Write_Objects),

    % Get hard coded inference read/write objects
    included_read_objects([Inference_Graph],Read_Graph_Descriptors,Inference_Read_Objects),
    included_write_objects([Inference_Graph],Write_Graph_Descriptors,Inference_Write_Objects),

    Query_Object = query_obj{
                       descriptor : terminus_descriptor,
                       instance_read_objects : Instance_Read_Objects,
                       schema_read_objects : Schema_Read_Objects,
                       inference_read_objects : Inference_Read_Objects,
                       instance_write_objects : Instance_Write_Objects,
                       inference_write_objects : Inference_Write_Objects,
                       schema_write_objects : Schema_Write_Objects
                   }.
descriptor_query(Descriptor, Read_Graph_Descriptors, Write_Graph_Descriptors, Map,
                 [Descriptor=Query_Object|Map]) :-
    database_descriptor{
        instance : IL % server/user/db_name 
    } :< Descriptor,
    !,
    layer_ontology(Layer_Ontology_Name),
    Layer_Ontology_Graph = named_graph{ name : Layer_Ontology_Name },
    repository_ontology(Repository_Ontology_Name),
    Repository_Ontology_Graph = named_graph{ name : Repository_Ontology_Name },

    % Get hard coded layer and repository schema read/write objects
    Schema_Graphs = [Repository_Ontology_Graph, Layer_Ontology_Graph],
    included_read_objects(Schema_Graphs,Read_Graph_Descriptors, Schema_Read_Objects),
    included_write_objects(Schema_Graphs,Write_Graph_Descriptors, Schema_Write_Objects),

    % Get instance read/write objects
    included_read_objects(IL,Read_Graph_Descriptors,Instance_Read_Objects),
    included_write_objects(IL,Write_Graph_Descriptors, Instance_Write_Objects),

    Query_Object = query_obj{
                       descriptor : Descriptor,
                       instance_read_objects : Instance_Read_Objects,
                       schema_read_objects : Schema_Read_Objects,
                       inference_read_objects : [],
                       instance_write_objects : Instance_Write_Objects,
                       inference_write_objects : [],
                       schema_write_objects : Schema_Write_Objects
                   }.
descriptor_query(Descriptor, Read_Graph_Descriptors, Write_Graph_Descriptors, Map,
                 [Descriptor=Query_Object|New_Map]) :-
    repository_descriptor{
        database_descriptor : DB,
        instance : Instances
    } :< Descriptor,
    !,
    descriptor_query(DB, Read_Graph_Descriptors, Write_Graph_Descriptors, Map, New_Map),
    memberchk(DB,New_Map, Parent_Query_Obj),

    layer_ontology(Layer_Ontology_Name),
    Layer_Ontology_Graph = named_graph{ name : Layer_Ontology_Name },
    repository_ontology(Repository_Ontology_Name),
    Repository_Ontology_Graph = named_graph{ name : Repository_Ontology_Name },

    % Get hard coded layer and repository schema read/write objects
    Schema_Graphs = [Repository_Ontology_Graph, Layer_Ontology_Graph],
    included_read_objects(Schema_Graphs,Read_Graph_Descriptors, Read_Schema_Objects),
    included_write_objects(Schema_Graphs,Write_Graph_Descriptors, Write_Schema_Objects),

    % Get instance read/write objects
    included_read_objects(Instances,Read_Graph_Descriptors,Read_Instance_Objects),
    included_write_objects(Instances,Write_Graph_Descriptors, Write_Instance_Objects),

    Query_Object = query_obj{
                       parent : Parent_Query_Obj,
                       descriptor : Descriptor,
                       instance_read_objects : Read_Instance_Objects,
                       schema_read_objects : Read_Schema_Objects,
                       inference_read_objects : [],
                       instance_write_objects : Write_Instance_Objects,
                       schema_write_objects : Write_Schema_Objects,
                       inference_write_objects : []
                   }.
descriptor_query(Descriptor, Read_Graph_Descriptors, Write_Graph_Descriptors, Map,
                 [Descriptor=Query_Object|New_Map]) :-
    ref_descriptor{ repository_descriptor : Repository_Descriptor,
                    message : Message,
                    author : Author,
                    schema : Schema_List,
                    inference : Inference_List,
                    instance : Instance_List } :< Descriptor,
    !,
    descriptor_query(Repository_Descriptor, Read_Graph_Descriptors, Write_Graph_Descriptors,
                     Map, New_Map),
    memberchk(Repository_Descriptor, New_Map, Parent_Query_Obj),

    % Get schema read/write objects
    included_read_objects(Schema_List,Read_Graph_Descriptors, Schema_Read_Objects),
    included_write_objects(Schema_List,Write_Graph_Descriptors, Schema_Write_Objects),

    % Get instance read/write objects
    included_read_objects(Instance_List,Read_Graph_Descriptors, Instance_Read_Objects),
    included_write_objects(Instance_List,Write_Graph_Descriptors, Instance_Write_Objects),

    % Get inference read/write objects
    included_read_objects(Inference_List,Read_Graph_Descriptors, Inference_Read_Objects),
    included_write_objects(Inference_List,Write_Graph_Descriptors, Inference_Write_Objects),

    Query_Object = query_obj{
                       parent : Parent_Query_Obj,
                       descriptor : Descriptor,
                       commit_message : Message,
                       commit_author : Author,
                       instance_read_objects : Instance_Read_Objects,
                       schema_read_objects : Schema_Read_Objects,
                       inference_read_objects : Inference_Read_Objects,
                       instance_write_objects : Instance_Write_Objects,
                       schema_write_objects : Schema_Write_Objects,
                       inference_write_objects : Inference_Write_Objects
                   }.

update_read_objects([], New_Read_Objects, New_Read_Objects).
update_read_objects([Old_Read_Object|Old_Read_Objects], New_Read_Objects, Results) :-
    memberchk(read_obj{ descriptor: Old_Read_Object.descriptor, read: _}, New_Read_Objects),
    !,
    update_read_objects(Old_Read_Objects, New_Read_Objects, Results).
update_read_objects([Old_Read_Object|Old_Read_Objects], New_Read_Objects, [Old_Read_Object|Results]) :-
    update_read_objects(Old_Read_Objects, New_Read_Objects, Results).
