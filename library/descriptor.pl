:- module(descriptor,[
              open_descriptor/2,
              open_descriptor/3,
              open_descriptor/5,
              collection_descriptor_transaction_object/3,
              graph_descriptor_transaction_objects_read_write_object/3,
              instance_graph_descriptor_transaction_object/3,
              read_write_obj_reader/2,
              read_write_obj_builder/2,
              filter_read_write_objects/3
          ]).

/** <module> Descriptor Manipulation
 *
 * This file deals with the manipulation and opening of descriptors.
 *
 * Types:
 *
 * graph_descriptor --> labelled_graph{ name : atom }
 *                    | id_graph{ layer_id : atom } % for debugging
 *                    | terminus_graph{ type : atom,
 *                                      name : atom }
 *                    | repo_graph { database_name : atom,
 *                                   type : atom,
 *                                   name : atom }
 *                    | commit_graph{ database_name : atom,
 *                                    repository_name : atom,
 *                                    type : atom,
 *                                    name : atom }
 *                    | branch_graph{ database_name : atom,
 *                                    repository_name : atom,
 *                                    branch_name : atom,
 *                                    type : atom, % {instance, schema, inference}
 *                                    name : atom }
 *
 * graph_filter --> type_filter{ types : list(atom) } % instance, inference, schema
 *                | type_name_filter{ type : atom, names : list(string) }
 *
 * A named_graph refers to a file in the store - this has to be made unique so we don't get
 * collisions between different databases. Currently only used for the terminus and database graph
 * - one per repository. This uses the file label mechanism.
 *
 * A ref_graph is a layer id that can be resolved to a graph.
 *
 * collection_descriptor --> terminus_descriptor{}
 *                         | label_descriptor{ label: string }
 *                         | id_descriptor{ id : string } % only for querying!
 *                         | database_descriptor{ database_name : atom }
 *                         | repository_descriptor{ database_descriptor : database_descriptor,
 *                                                  repository_name : atom }
 *                         | branch_descriptor{ repository_descriptor: repository_descriptor,
 *                                              branch_name : atom}, % the name of the thing advancing
 *                         | commit_descriptor{ repository_descriptor: repository_descriptor,
 *                                              last_commit : atom} % the base of the commit
 *
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
 * read_write_obj ---> read_write_obj{ descriptor : graph_descriptor, read : Layer, write: Var_Or_Layer_Builder}
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
:- use_module(literals).
:- use_module(query).
:- use_module(utils).
:- reexport(syntax).

graph_descriptor_to_layer(Descriptor, Layer, Map, Map) :-
    memberchk(Descriptor=Layer, Map),
    !.
graph_descriptor_to_layer(Descriptor, Layer, Map, [Descriptor=Layer|Map]) :-
    Descriptor = terminus_graph{ type: Type, name: Name},
    !,
    (   Type = instance,
        Name = "main"
    ->  terminus_instance_name(Graph_Name)
    ;   Type = schema,
        Name = "main"
    ->  terminus_schema_name(Graph_Name)
    ;   Type = inference,
        Name = "main",
        terminus_inference_name(Graph_Name)),

    storage(Store),
    safe_open_named_graph(Store, Graph_Name, Graph),
    head(Graph, Layer).
graph_descriptor_to_layer(Descriptor, Layer, Map, [Descriptor=Layer|Map]) :-
    Descriptor = labelled_graph{ name: Name },
    !,
    storage(Store),
    safe_open_named_graph(Store, Name, Graph),
    ignore(head(Graph, Layer)).
graph_descriptor_to_layer(Descriptor, Layer, Map, [Descriptor=Layer|Map]) :-
    Descriptor = id_graph{ layer_id: Layer_Id },
    !,
    storage(Store),
    store_id_layer(Store, Layer_Id, Layer).
graph_descriptor_to_layer(Descriptor, Layer, Map, [Descriptor=Layer|Map]) :-
    Descriptor = repo_graph{ database_name: Database_Name,
                             type : Type,
                             name : Name},
    !,
    (   Type = instance,
        Name = "main"
    ->  storage(Store),
        safe_open_named_graph(Store, Database_Name, Graph),
        head(Graph, Layer)
    ;   Type = schema,
        Name = "layer"
    ->  storage(Store),
        layer_ontology(Layer_Name),
        safe_open_named_graph(Store, Layer_Name, Graph),
        head(Graph, Layer)
    ;   Type = schema,
        Name = "repository"
    ->  repository_ontology(Repository_Name),
        storage(Store),
        safe_open_named_graph(Store, Repository_Name, Graph),
        head(Graph, Layer)
    ).
graph_descriptor_to_layer(Descriptor,
                          Layer,
                          Map,
                          [Descriptor=Layer|New_Map]) :-
    Descriptor = commit_graph{ database_name: Database_Name,
                               repository_name: Repository_Name,
                               type: Type,
                               name: Name},
    !,
    Repo_Descriptor = repo_graph{ database_name: Database_Name,
                                  type: instance,
                                  name: "main"},

    (   Type = instance,
        Name = "main"
    ->  graph_descriptor_to_layer(Repo_Descriptor, Repository_Layer, Map, New_Map),
        repo_layer_name_to_ref_layer_id(Repository_Layer, Repository_Name, Commit_Layer_Id),
        storage(Store),
        store_id_layer(Store, Commit_Layer_Id, Layer)
    ;   Type = schema,
        Name = "layer"
    ->  New_Map = Map,
        storage(Store),
        layer_ontology(Layer_Name),
        safe_open_named_graph(Store, Layer_Name, Graph),
        head(Graph, Layer)
    ;   Type = schema,
        Name = "ref"
    ->  New_Map = Map,
        ref_ontology(Ref_Name),
        storage(Store),
        safe_open_named_graph(Store, Ref_Name, Graph),
        head(Graph, Layer)
    ).
graph_descriptor_to_layer(Descriptor,
                          Layer,
                          Map,
                          [Descriptor=Layer|New_Map]) :-
    Descriptor = branch_graph{ database_name: Database_Name,
                               repository_name: Repository_Name,
                               branch_name: Branch_Name,
                               type: Type,
                               name: Graph_Name },
    !,
    Commit_Descriptor = commit_graph{ database_name : Database_Name,
                                      repository_name : Repository_Name,
                                      type: instance,
                                      name: "main" },
    graph_descriptor_to_layer(Commit_Descriptor, Commit_Layer, Map, New_Map),
    (   commit_layer_branch_type_name_to_data_layer_id(Commit_Layer, Branch_Name, Type, Graph_Name, Layer_Id),
        storage(Store),
        store_id_layer(Store, Layer_Id, Layer)
    ->  true
    ;   Layer = _).

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

    once((id_triple(Repo_Layer, Repo_Uri_Id, Repo_Name_Property_Id, Repo_Name_Id),
          id_triple(Repo_Layer, Repo_Uri_Id, Repo_Head_Property_Id, Repo_Head_Id),
          id_triple(Repo_Layer, Repo_Head_Id, Layer_Id_Property_Id, Ref_Layer_Id_Id))),

    object_id(Repo_Layer, Ref_Layer_Id_Literal, Ref_Layer_Id_Id),
    storage_object(Ref_Layer_Id_Literal, Ref_Layer_Id^^_).

commit_layer_branch_type_name_to_data_layer_id(Commit_Layer, Branch_Name, Type, Graph_Name, Layer_ID) :-
    layer_to_id(Commit_Layer, Layer_ID),
    Collection_Descriptor = id_descriptor{ layer_id : Layer_ID },
    once(ask(Collection_Descriptor,
             (   t(Branch_URI, ref:branch_name, Branch_Name^^xsd:string),
                 t(Branch_URI, ref:ref_commit, Commit_URI),
                 t(Commit_URI, ref:Type, Graph_URI),
                 t(Graph_URI, ref:graph_name, Graph_Name^^xsd:string),
                 t(Graph_URI, ref:graph_layer, Layer_URI),
                 t(Layer_URI, layer:layer_id, Layer_ID^^xsd:string)
             ))).

open_read_write_obj(Descriptor, read_write_obj{ descriptor: Descriptor, read: Layer, write: _Layer_Builder }, Map, New_Map) :-
    graph_descriptor_to_layer(Descriptor, Layer, Map, New_Map).

read_write_obj_reader(Read_Write_Obj, _Layer) :-
    var(Read_Write_Obj.read),
    !,
    fail.
read_write_obj_reader(Read_Write_Obj, Layer) :-
    Layer = Read_Write_Obj.read.

read_write_obj_builder(Read_Write_Obj, Layer_Builder) :-
    ground(Read_Write_Obj.write),
    !,
    Layer_Builder = Read_Write_Obj.write.
read_write_obj_builder(Read_Write_Obj, Layer_Builder) :-
    var(Read_Write_Obj.read),
    !,

    storage(Store),
    open_write(Store, Read_Write_Obj.write),
    Layer_Builder = Read_Write_Obj.write.
read_write_obj_builder(Read_Write_Obj, Layer_Builder) :-
    open_write(Read_Write_Obj.read, Read_Write_Obj.write),
    Layer_Builder = Read_Write_Obj.write.
read_write_obj_builder(Read_Write_Obj, Layer_Builder) :-
    Layer_Builder = Read_Write_Obj.write.

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

    Instance_Graph = terminus_graph{ type: instance, name: "main"},
    Schema_Graph = terminus_graph{ type: schema, name: "main"},
    Inference_Graph = terminus_graph{ type: inference, name: "main"},

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
                [Descriptor=Transaction_Object|New_Map]) :-
    id_descriptor{ id : ID } :< Descriptor,
    !,
    Graph_Descriptor = id_graph{ layer_id : ID },
    open_read_write_obj(Graph_Descriptor, Instance, Map, New_Map),
    Transaction_Object = transaction_object{
                             descriptor : Descriptor,
                             instance_objects : [Instance],
                             schema_objects : [],
                             inference_objects : []
                         }.
open_descriptor(Descriptor, _Commit_Info, Transaction_Object, Map,
                 [Descriptor=Transaction_Object|Map_1]) :-
    label_descriptor{
        label: Label
    } = Descriptor,
    !,

    Graph_Descriptor = labelled_graph{ name: Label },
    open_read_write_obj(Graph_Descriptor, Read_Write_Obj, Map, Map_1),

    Transaction_Object = transaction_object{
                             descriptor: Descriptor,
                             instance_objects: [Read_Write_Obj],
                             schema_objects: [],
                             inference_objects: []
                         }.

open_descriptor(Descriptor, _Commit_Info, Transaction_Object, Map,
                 [Descriptor=Transaction_Object|Map_3]) :-
    database_descriptor{
        database_name: Database_Name
    } = Descriptor,
    !,

    Layer_Ontology_Graph = repo_graph{ database_name: Database_Name,
                                       type: schema,
                                       name: "layer" },
    Repository_Ontology_Graph = repo_graph{ database_name : Database_Name,
                                            type: schema,
                                            name: "repository" },

    open_read_write_obj(Layer_Ontology_Graph, Layer_Ontology_Object, Map, Map_1),
    open_read_write_obj(Repository_Ontology_Graph, Repository_Ontology_Object, Map_1, Map_2),
    Instance_Graph = repo_graph{ database_name: Database_Name,
                                 type: instance,
                                 name: "main" },
    open_read_write_obj(Instance_Graph, Instance_Object, Map_2, Map_3),

    Transaction_Object = transaction_object{
                             descriptor : Descriptor,
                             instance_objects : [Instance_Object],
                             schema_objects : [Layer_Ontology_Object, Repository_Ontology_Object],
                             inference_objects : []
                         }.
open_descriptor(Descriptor, _Commit_Info, Transaction_Object, Map,
                 [Descriptor=Transaction_Object|Map_4]) :-
    repository_descriptor{
        database_descriptor : Database_Descriptor,
        repository_name: Repository_Name
    } :< Descriptor,
    !,
    open_descriptor(Database_Descriptor, _, Database_Transaction_Object, Map, Map_1),

    Database_Name = Database_Descriptor.database_name,
    Layer_Ontology_Graph = commit_graph{ database_name: Database_Name,
                                         repository_name: Repository_Name,
                                         type: schema,
                                         name: "layer" },
    Ref_Ontology_Graph = commit_graph{ database_name : Database_Name,
                                       repository_name: Repository_Name,
                                       type: schema,
                                       name: "ref" },

    Instance_Graph = commit_graph{ database_name: Database_Name,
                                   repository_name: Repository_Name,
                                   type: instance,
                                   name: "main"},

    open_read_write_obj(Layer_Ontology_Graph, Layer_Ontology_Object, Map_1, Map_2),
    open_read_write_obj(Ref_Ontology_Graph, Ref_Ontology_Object, Map_2, Map_3),
    open_read_write_obj(Instance_Graph, Instance_Object, Map_3, Map_4),

    Transaction_Object = transaction_object{ parent : Database_Transaction_Object,
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

    [Instance_Object] = Repository_Transaction_Object.instance_objects,

    layer_to_id(Instance_Object.read, Ref_Layer_Id),
    Ref_Id_Descriptor = id_descriptor{ layer_id: Ref_Layer_Id},
    (   once(ask(Ref_Id_Descriptor,
                 (   t(Branch_Uri, ref:branch_name, Branch_Name^^xsd:string),
                     t(Branch_Uri, ref:ref_commit, Commit_Uri))))
    ->  findall(Instance_Graph_Name,
               (   t(Commit_Uri, ref:instance, Instance_Graph),
                   t(Instance_Graph, ref:graph_name, Instance_Graph_Name)
               ),
               Instance_Names),
        findall(Schema_Graph_Name,
               (   t(Commit_Uri, ref:schema, Schema_Graph),
                   t(Schema_Graph, ref:graph_name, Schema_Graph_Name)
               ),
               Schema_Names),
        findall(Inference_Graph_Name,
               (   t(Commit_Uri, ref:inference, Inference_Graph),
                   t(Inference_Graph, ref:graph_name, Inference_Graph_Name)
               ),
               Inference_Names)
    ;   % Note: There has never been a commit! Set up default graphs.
        Instance_Names = [main],
        Inference_Names = [main],
        Schema_Names = [main]
    ),

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
    mapm(open_read_write_obj,
         Instance_Descriptors, Instance_Objects,
         Map_1, Map_2),

    maplist({Prototype}/[Schema_Name,Graph_Descriptor]>>(
                Graph_Descriptor = Prototype.put(_{type : schema,
                                                   name : Schema_Name})),
            Schema_Names,
            Schema_Descriptors),
    mapm(open_read_write_obj,
         Schema_Descriptors, Schema_Objects,
         Map_2, Map_3),

    maplist({Prototype}/[Inference_Name,Graph_Descriptor]>>(
                Graph_Descriptor = Prototype.put(_{type : inference,
                                                   name : Inference_Name})),
            Inference_Names,
            Inference_Descriptors),
    mapm(open_read_write_obj,
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

graph_descriptor_find_read_write_object(_, [], _) :-
        !,
        fail.
graph_descriptor_find_read_write_object(Graph_Descriptor, [Read_Write_Obj|_Read_Write_Objs], Read_Write_Obj) :-
        Read_Write_Obj.descriptor = Graph_Descriptor,
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
               read_write_obj{ descriptor : Graph_Descriptor,
                               read : _,
                               write : _ }]>>true,
           RW_Objects),
    !.
instance_graph_descriptor_transaction_object(Graph_Descriptor, [_Transaction_Object|Transaction_Objects], Transaction_Object) :-
    instance_graph_descriptor_transaction_object(Graph_Descriptor, Transaction_Objects, Transaction_Object).

collection_descriptor_transaction_object(Collection_Descriptor, [Transaction_Object|_Transaction_Objects], Transaction_Object) :-
    Transaction_Object.descriptor = Collection_Descriptor,
    !.
collection_descriptor_transaction_object(Collection_Descriptor, [Transaction_Object|Transaction_Objects], Transaction_Object) :-
    collection_descriptor_transaction_object(Collection_Descriptor, Transaction_Objects, Transaction_Object).

read_write_object_to_name(Object, Name) :-
    Name = Object.descriptor.name.

/*
 * filter_read_write_objects(+Objects, +Names, Filtered) is det.
 */
filter_read_write_objects(Objects, Names, Filtered) :-
    include({Names}/[Object]>>(read_write_object_to_name(Object, Name),
                               memberchk(Name, Names)), Objects, Filtered).
