:- module(database,[
              database_name/2,
              database_instance/2,
              database_schema/2,
              database_error_instance/2,
              database_error_schema/2,
              database_inference/2,
              make_database/2,
              make_raw_database/2,
              terminus_database_name/1,
              terminus_database/1,
              terminus_context/1,
              make_database_from_database_name/2,
              database_name_list/1,
              database_record_list/1,
              database_record_instance_list/2,
              database_record_schema_list/2,
              database_record_inference_list/2,
              database_record_instance_list/3,
              database_record_schema_list/3,
              database_record_inference_list/3,
              is_schema_graph/2,
              default_instance_graph/3,
              db_size/2,
              db_modified_datetime/2,
              db_created_datetime/2,
              get_read_layer/3,
              get_write_builder/3,
              % commit_write_transaction/2,
              maybe_open_read_transaction/2,
              open_read_transaction/2,
              with_transaction/3
          ]).

:- meta_predicate(with_transaction(+, 0, 0)).

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

:- use_module(utils).
:- use_module(types).
:- use_module(sdk).
:- use_module(file_utils).
:- use_module(triplestore).

:- use_module(library(prolog_stack)).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).

:- op(2, xfx, ^^).

/*
 * Types:
 *
 * graph_descriptor --> named_graph{ name : atom }
 *                    | ref_graph{ layer_id : atom }
 *
 * A named_graph refers to a file in the store - this has to be made unique so we don't get
 * collisions between different databases. Currently only used for the terminus and database graph
 * - one per repository. This uses the file label mechanism.
 *
 * A ref_graph is a layer id that can be resolved to a graph.
 *
 * collection_descriptor --> terminus_descriptor
 *                         | database_descriptor{ database_name : atom,
 *                                                instance : list(graph_descriptor)}
 *                         | repository_descriptor{ database_descriptor : database_descriptor,
 *                                                  repository_layer : layer,
 *                                                  repository_name : atom,
 *                                                  instance : list(graph_descriptor)}
 *                         | ref_descriptor{ repository_descriptor: repository_descriptor,
 *                                           ref_layer : layer,
 *                                           ref_name : atom,
 *                                           schema : list(graph_descriptor)
 *                                           instance : list(graph_descriptor)
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
 * read_obj ---> read_obj{ descriptor : graph_descriptor, read : Layer}
 * write_obj ---> write_obj{ descriptor : graph_descriptor, write : Layer_Builder}
 * query_object ---> query_object{ collection_descriptor : collection_descriptor,
 *                                 instance_read_objects : list(read_obj),
 *                                 schema_read_objects : list(read_obj),
 *                                 inference_read_objects : list(read_obj),
 *                                 instance_write_objects : list(write_obj),
 *                                 schema_write_objects : list(read_obj),
 *                                 inference_write_objects : list(read_obj) }
 *
 * transaction_object ---> transaction_object{ collection_descriptors : list(collection_descriptor),
 *                                             query_objs : list(query_objs),
 *                                             post_objs : list(query_objs) }
 */


/*
 * terminus_repository_schema(Schema) is det.
 *
 * Returns the name of the repository schema
 */
terminus_repository_schema(Schema) :-
    db_path(Path),
    www_form_encode('terminus-repository-schema',Safe_GID),
    interpolate([Path,Safe_GID,'.label'],Label).

/**
 * open_read_obj(Graph_Descriptor : graph_descriptor, Read_Object : read_object) is det.
 *
 * opens a graph descriptor with a querable read object.
 */
open_read_obj(Graph, read_obj{ descriptor : Graph, read : Layer }) :-
    Graph = named_graph{ name : Name },
    storage(Store),
    safe_open_named_graph(Store,Name,Obj),
    head(Obj,Layer).
open_read_obj(Graph, read_obj{ descriptor : Graph, read : Layer }) :-
    Graph = ref_graph{ layer_id : Layer_ID },
    storage(Store),
    store_id_layer(Store,Layer_ID,Layer).

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

included_read_objects(Graph_Desriptors, Read_Graph_Descriptors, Read_Objects) :-
    include({Read_Graph_Descriptors}/[X]>>memberchk(X,Read_Graph_Descriptors),
            Graph_Descriptors,
            Read_Descriptors),
    maplist([Descriptor,Read_Obj]>>open_read_obj(Descriptor,Read_Obj),
            Read_Descriptors, Read_Objects).

included_write_objects(Graph_Desriptors, Read_Graph_Descriptors, Read_Objects) :-
    include({Read_Graph_Descriptors}/[X]>>memberchk(X,Read_Graph_Descriptors),
            Graph_Descriptors,
            Read_Descriptors),
    maplist([Descriptor,Read_Obj]>>open_write_obj(Descriptor,Read_Obj),
            Read_Descriptors, Read_Objects).

/**
 * descriptor_query(Descriptor, Write_Mask, Read_Mask, Query_Obj) is det.
 *
 * Impements the logic which allows us to special-case the
 * opening of descriptors for repository, branch and graph.
 *
 * @Descriptor has type repository{ ... } | branch{ ... }
 * @Write_Mask has type list(descriptor)
 *             This opens read queries. No mask means everything is opened.
 */
descriptor_query(Collection_Descriptor, Read_Graph_Descriptors, Write_Graph_Descriptors, Query_Obj) :-
    Collection_Descriptor = database_descriptor{
                                database_name : DB_Name,
                                instance : IL
                            },
    !,
    layer_ontology(Layer_Ontology_Name),
    Layer_Ontology_Graph = named_graph{ name : Layer_Ontology_Name },
    repository_ontology(Repository_Ontology_Name),
    Repository_Ontology_Graph = named_graph{ name : Repository_Ontology_Name },

    % Get hard coded layer and repository schema read/write objects
    Schema_Graphs = [Repository_Ontology_Graph, Layer_Ontology_Graph],
    include_read_objects(Schema_Graphs,Read_Graph_Descriptors, Read_Schema_Objects),
    include_write_objects(Schema_Graphs,Write_Graph_Descriptors, Write_Schema_Objects),

    % Get instance read/write objects
    include_read_objects(IL,Read_Graph_Descriptors,Read_Instance_Objects),
    include_write_objects(IL,Write_Graph_Descriptors, Write_Instance_Objects),

    Query_Obj = query_obj{
                    collection_descriptor : Collection_Descriptor,
                    instance_read_objects : Instance_Read_Objects,
                    schema_read_objects : Schema_Read_Objects,
                    inference_read_objects : [],
                    instance_write_objects : Write_Instance_Objects,
                    inference_write_objects : [],
                    schema_write_objects : Schema_Write_Objects
                }.
descriptor_query(Repository, Read_Mask, Write_Mask, Query_Obj) :-
    Collection_Descriptor = repository_descriptor{
                                database_descriptor : _DB,
                                repository_layer : _Layer,
                                repository_name : _Name,
                                instance : IL
                            },
    !,
    layer_ontology(Layer_Ontology_Name),
    Layer_Ontology_Graph = named_graph{ name : Layer_Ontology_Name },
    ref_ontology(Ref_Ontology_Name),
    Repository_Ontology_Graph = named_graph{ name : Repository_Ontology_Name },

    % Get hard coded layer and repository schema read/write objects
    Schema_Graphs = [Ref_Ontology_Graph, Layer_Ontology_Graph],
    include_read_objects(Schema_Graphs,Read_Graph_Descriptors, Read_Schema_Objects),
    include_write_objects(Schema_Graphs,Write_Graph_Descriptors, Write_Schema_Objects),

    % Get instance read/write objects
    include_read_objects(IL,Read_Graph_Descriptors,Read_Instance_Objects),
    include_write_objects(IL,Write_Graph_Descriptors, Write_Instance_Objects),

    Query_Obj = query_obj{
                    collection_descriptor : Collection_Descriptor,
                    instance_read_objects : Read_Instance_Objects,
                    schema_read_objects : Read_Schema_Objects,
                    inference_read_objects : [],
                    instance_write_objects : Write_Instance_Objects,
                    schema_write_objects : Write_Schema_Objects,
                    inference_write_objects : []
                }.
descriptor_query(Branch, Read_Mask, Write_Mask, Query_Obj) :-
    Branch = ref_descriptor{ database_name : DB_Name,
                             repository_name : Repository_Name,
                             branch_name : Branch_Name,
                             schema : Schema_List,
                             inference : Inf_List,
                             instance : Instance_List },
    !,
    true.

/*
 * database_name_to_descriptor(?DB_Name : atom, ?Database_Descriptor : collection_descriptor) is semidet.
 *
 * Gets a database name descriptor
 */
database_name_descriptor(DB_Name, database_descriptor{
                                      database_name : DB_Name,
                                      instance : [
                                          named_graph{
                                              name : DB_Name
                                          }
                                      ]}).

database_repository_descriptor(Query_Obj, 

/*
 * database_repository(+DB_Name,-Repo_Obj) is det.
 *
 * This takes a database name and returns a repository
 * object.
 *
 * A repository object contains the graph with all existing branches
 * and how to access them (graphs and their heads).
 *
 */
database_repository(DB_Name,
                    Repository_Name,
                    repository{ database_name : DB_Name,
                                repository_name : Repository_Name,
                                schema : [SL],
                                inference : [],
                                instance : [IL]}
                   ) :-
    storage(Store),
    terminus_repository_schema(Schema),
    safe_open_named_graph(Store,Schema,Schema_Obj),
    safe_open_named_graph(Store,DB_Name,Instance_Obj),
    head(Schema_Obj, SL),
    head(Instance_Obj, IL).

database_repository(DB_Name, Repository) :-
    database_repository(DB_Name, local, Repository).

/*
 * repository_branch(Repo_Obj, Branch_Name, Branch_Obj) is det.
 *
 * Loads a branch object. For bootstrapping reasons this must
 * do all graph queries manually (no use of the ontology.
 *
 * This means that ontological changes have to be carefully synced.
 */
repository_branch(repository{ database_name : _,
                              repository_name : _,
                              instance : LI,
                              inference : LInf,
                              instance : []}, Branch_Name,
                  branch{
                      database_name : _,
                      repository_name : _,
                      branch_name : _,
                      instance : _,
                      schema : _
                      inference : _
                  }) :-
    global_prefix_expand(terminus:'Branch', Branch_Class),
    global_prefix_expand(rdf:type, IsA),
    xrdf_db(L,Branch,IsA,Branch_Class),
    xrdf_db(

/*
 * Database term accessors.
 *
 * Deprecated: For backwards compatability.
 * Use dictionary accessors.
 */
database_name(DB,Name) :-
    DB.name = Name.

database_instance(DB,Instance) :-
    DB.instance = Instance.

database_inference(DB, Inference) :-
    DB.inference = Inference.

database_schema(DB,Schema) :-
    DB.schema = Schema.

database_read_transaction(DB, Read_Transaction) :-
    DB.read_transaction = Read_Transaction.

database_write_transaction(DB, Write_Transaction) :-
    DB.write_transaction = Write_Transaction.

database_error_schema(DB, Error_Schema) :-
    DB.error_schema = Error_Schema.

database_error_instance(DB, Error_Instance) :-
    DB.error_instance = Error_Instance.

/**
 * make_database(+DatabaseList:list,-Database:database) is det.
 *
 * Create a graph from a list containing some number of graph elements.
 * [instance=Instance,schema=Schema...]
 */
make_database(DatabaseList, Database) :-
    make_raw_database(DatabaseList, Database),
    schema:database_module(Database,Module),
    schema:ensure_schema_in_module(Database,Module).

/**
 * make_database(+DatabaseList:list,-Database:datatabase) is det.
 *
 * Create a graph from a list containing some number of graph elements.
 * [instance=Instance,schema=Schema...]
 *
 * This does NO schema compilation and is only used during initial pre-testing for cycles.
 */
make_raw_database(DatabaseList, Database) :-
    dict_create(Database, database, DatabaseList).

/*
 * terminus_database_name(Database_Name) is det.
 *
 * The name of the current terminus database.
 */
terminus_database_name(Database_Name) :-
    config:public_server_url(Server),
    atomic_list_concat([Server,'/terminus'],Database_Name).

/**
 * terminus_context(Context : dictionary) is det.
 *
 * JSON-LD of terminus context.
 */
terminus_context(_{doc : Doc,
                   scm : Schema,
                   terminus : 'http://terminusdb.com/schema/terminus#'
                  }) :-
    config:public_server_url(Server),
    atomic_list_concat([Server,'/terminus/document/'],Doc),
    atomic_list_concat([Server,'/terminus/schema#'],Schema).


/*
 * terminus_database(Database).
 *
 * Since this is prior to all other aspects of graph
 * management, we need to set the information manually.
*/
terminus_database(Database) :-
    terminus_database_name(Database_Name),
    atomic_list_concat([Database_Name,'/',schema],Schema),
    atomic_list_concat([Database_Name,'/',document],Instance),
    atomic_list_concat([Database_Name,'/',inference],Inference),
    make_database([name=Database_Name,
                   schema=[Schema],
                   inference=[Inference],
                   instance=[Instance],
                   read_transaction=[],
                   write_transaction=[]], EmptyDatabase),
    open_read_transaction(EmptyDatabase, Database).

/*
 *
 * This is needed for the prefix manufacture so there is a slight
 * complication with bootstrapping.
 */
database_name_list(Database_Atoms) :-
    terminus_database(DB),

    findall(Database_Name,
            (   xrdf(DB,DB.instance,
                     DB_Resource, rdf:type, terminus:'Database'),
                xrdf(DB,DB.instance,
                     DB_Resource, terminus:id, literal(type(_,Database_Name)))),
            Database_Names),

    maplist(atom_string, Database_Atomised, Database_Names),
    exclude(terminus_database_name, Database_Atomised, Database_Atoms).

database_record_list(Databases) :-
    terminus_database_name(Terminus_Name),
    connect(Terminus_Name, Terminus_DB),

    findall(DB_Resource,
            ask(Terminus_DB,
                t( DB_Resource , rdf/type , terminus/'Database')
               ),
            Databases).

database_record_schema_list(Database_Name, Schemas) :-
    terminus_database_name(Terminus_Name),
    connect(Terminus_Name, Terminus_DB),
    database_record_schema_list(Terminus_DB, Database_Name, Schemas).

database_record_schema_list(Terminus_DB, Database_Name, Schemas) :-
    findall(Schema,
            ask(Terminus_DB,
                where(
                    (   t( DB_Resource , terminus/id, Database_Name ^^ (xsd/anyURI)),
                        t( DB_Resource , rdf/type , terminus/'Database'),
                        t( DB_Resource , terminus/schema, Schema ^^ (xsd/string))
                    )
                )
               ),
            Schema_Strings),

    maplist(atom_string, Schemas, Schema_Strings).

database_record_instance_list(Database_Name, Schemas) :-
    terminus_database_name(Terminus_Name),
    connect(Terminus_Name, Terminus_DB),
    database_record_instance_list(Terminus_DB, Database_Name, Schemas).

database_record_instance_list(Terminus_DB, Database_Name,Instances) :-
    findall(Instance,
            ask(Terminus_DB,
                where(
                    (
                        t( DB_Resource , terminus/id, Database_Name ^^ (xsd/anyURI)),
                        t( DB_Resource , rdf/type , terminus/'Database'),
                        t( DB_Resource , terminus/instance, Instance ^^ (xsd/string))
                    )
                )
               ),
            Instance_Strings),

    maplist(atom_string, Instances, Instance_Strings).

database_record_inference_list(Database_Name, Schemas) :-
    terminus_database_name(Terminus_Name),
    connect(Terminus_Name, Terminus_DB),
    database_record_inference_list(Terminus_DB, Database_Name, Schemas).

database_record_inference_list(Terminus_DB,Database_Name,Inferences) :-
    findall(Inference,
            ask(Terminus_DB,
                where(
                    (   t( DB_Resource , terminus/id, Database_Name ^^ (xsd/anyURI)),
                        t( DB_Resource , rdf/type , terminus/'Database'),
                        t( DB_Resource , terminus/inference, Inference ^^ (xsd/string))
                    )
                )
               ),
            Inference_Strings),

    maplist(atom_string, Inferences, Inference_Strings).


/**
 * make_database_from_database_name(+Terminus_DB,+URI,-Database) is det.
 *
 */
make_database_from_database_name(Database_Name,Database) :-
    % Need a special case for the master database
    (   terminus_database_name(Database_Name)
    ->  terminus_database(Database)
    ;   terminus_database_name(Collection),
        connect(Collection,Terminus_DB),
        database_record_schema_list(Terminus_DB, Database_Name,Schemata),
        database_record_inference_list(Terminus_DB, Database_Name,Inferences),
        database_record_instance_list(Terminus_DB, Database_Name,Instances),

        make_database([name=Database_Name,
                       schema=Schemata,
                       instance=Instances,
                       inference=Inferences,
                       read_transaction=[],
                       write_transaction=[]
                      ], Database)
    ).


/*
 * is_schema_graph(Collection,Schema) is det.
 *
 *
 */
is_schema_graph(C,S) :-
    terminus_database_name(C),
    !,
    atomic_list_concat([C,'/schema'],S).
is_schema_graph(C,S) :-
    database_record_schema_list(C,Schemata),
    memberchk(S,Schemata).

default_instance_graph(Terminus_DB,Database,I) :-
    database_name(Database, Name),
    interpolate([Name,'/document'],I),
    (   database_record_instance_list(Terminus_DB, Name, Instances),
        memberchk(I, Instances)
    ->  true
    ;   format(atom(MSG),'Unable to guess a valid document store for database: ~q', [Database]),
        throw(http_reply(not_found(_{'terminus:message' : MSG,
                                     'terminus:status' : 'terminus:failure'})))
    ).

/*
 * db_size(DB_URI,Size) is det.
 *
 * Get an approximate of the database size.
 */
db_size(DB_URI,Size) :-
    aggregate_all(sum(File_Size),
                  (
                      graphs(DB_URI,Graph_IDs),
                      memberchk(Graph_ID,Graph_IDs),
                      current_checkpoint_directory(DB_URI,Graph_ID,CPD),
                      files(CPD,Files),
                      memberchk(File,Files),
                      interpolate([CPD,'/',File],Path),
                      size_file(Path, File_Size)
                  ),
                  Size).

/*
 * db_modified_datetime(+DB,-DateTimeString) is det.
 *
 * The last modified time
 */
db_modified_datetime(DB_URI,DateTimeString) :-
    aggregate_all(max(TimeStamp),
                  (
                      graphs(DB_URI,Graphs),
                      memberchk(Graph,Graphs),
                      graph_directory(DB_URI, Graph, Path),
                      time_file(Path, TimeStamp)
                  ),
                  MaxTimeStamp),
    stamp_date_time(MaxTimeStamp, DateTime, 0),
    format_time(atom(DateTimeString),'%FT%T%:z', DateTime).

/*
 * db_created_datetime(+DB,-DateTimeString) is det.
 *
 * The last modified time
 */
db_created_datetime(DB_URI,DateTimeString) :-
    collection_directory(DB_URI,Path),
    time_file(Path, TimeStamp),
    stamp_date_time(TimeStamp, DateTime, 0),
    format_time(atom(DateTimeString),'%FT%T%:z', DateTime).

/*
 * open_read_transaction(+DB1, -DB2) is det.
 *
 * Open read transaction for all named graphs.
 */
open_read_transaction(Database1, Database2) :-
    append([Database1.instance,
            Database1.inference,
            Database1.schema
           ], Graphs),
    storage(Store),
    % Only collect read layers if they have a head.
    convlist({Store,N}/[G, read_obj{dbid : N, graphid : G, layer : L}]>>(
                 safe_open_named_graph(Store,G,Obj),
                 head(Obj, L)
             ), Graphs, NL),
    Database2 = Database1.put(database{read_transaction:NL}).

/*
 * open_write_transaction(+DB1, +Graphs, -DB2) is det.
 *
 * Open a write transaction.
 */
open_write_transaction(Database1, Graphs, Database2) :-
    Database1.name = N,
    storage(Store),
    maplist({Store,Database1,N}/[G, write_obj{dbid : N, graphid : G, builder : B} ]>>(
                (   get_read_layer(Database1,G,L)
                ->  open_write(L, B)
                ;   open_write(Store, B))
            ), Graphs, NL),
    Database2 = Database1.put( database{write_transaction:NL} ).

commit_write_transaction(Database1, Database2) :-
    Database1.write_transaction = WTs,
    maplist([write_obj{dbid: N, graphid: G, builder: B},
             read_obj{dbid: N, graphid: G, layer: L}]>>(
                nb_commit(B, L)
            ), WTs, RTs),
    Database1.read_transaction = RTs1,
    update_read_layers(RTs1, RTs, RTs2),
    Database2 = Database1.put( database{write_transaction:[],
                                        read_transaction:RTs2}).

update_read_layers(RTs, [], RTs).
update_read_layers(RTs1, [read_obj{dbid:N, graphid:G, layer: L}|Records], RTs2) :-
    select(read_obj{dbid:N, graphid:G, layer: _}, RTs1,
           read_obj{dbid:N, graphid:G, layer: L}, RTs_I),
    !,
    update_read_layers(RTs_I, Records, RTs2).
update_read_layers(RTs1, [R|Records], RTs2) :-
    % \+ select(...)
    % doesn't exist in RTs1...
    update_read_layers([R|RTs1], Records, RTs2).

get_read_layer(Database,G,L) :-
    Database.name = DBN,
    Database.read_transaction = RT,
    memberchk( read_obj{dbid: DBN, graphid: G, layer: L}, RT).

get_write_builder(Database,G,W) :-
    Database.name = DBN,
    Database.write_transaction = WT,
    memberchk( write_obj{dbid: DBN, graphid: G, builder: W}, WT).

maybe_open_read_transaction(DB1,DB2) :-
    (   DB1.read_transaction = []
    ->  open_read_transaction(DB1, DB2)
    ;   DB1 = DB2).


/*
 * open_update_transaction(Transaction_Records:list) is det.
 *
 * Opens a new database Update which can be used during the transaction.
 */
open_pre_transaction([]).
open_pre_transaction([transaction_record{
                          pre_database : Pre,
                          write_graphs : Write_Graphs,
                          update_database : Update,
                          post_database : _}
                      | Rest]) :-
    open_read_transaction(Pre, Intermediate),
    open_write_transaction(Intermediate, Write_Graphs, Update),
    open_pre_transaction(Rest).

/*
 * open_post_transaction(Transction_Records:list) is det.
 *
 * Opens a final database to test before pushing head.
 */
open_post_transaction([]).
open_post_transaction([transaction_record{
                           pre_database : _,
                           update_database : Update,
                           write_graphs : _,
                           post_database : Post}
                       |Rest]) :-
    commit_write_transaction(Update,Post),
    open_post_transaction(Rest).

/*
 * commit_transaction(Transaction_Records:list) is det.
 *
 * Commits all altered graphs to head.
 */
commit_transaction([]).
commit_transaction([transaction_record{ pre_database: _,
                                        write_graphs: WGs,
                                        update_database :_,
                                        post_database: PDB }|Rest]) :-
    maplist({PDB}/[G]>>(
                storage(Store),
                safe_open_named_graph(Store,G,G_Obj),
                get_read_layer(PDB,G,Layer),
                nb_set_head(G_Obj, Layer)
            ), WGs),
    commit_transaction(Rest).

is_transaction_record(R) :-
    is_dict(R),
    transaction_record{} :< R.

get_transaction_records(Options,Records) :-
    include(is_transaction_record,Options,Records).

/**
 * with_transaction(+Options,:Query_Update,:Post) is semidet.
 *
 * Succeeds if Update and tests Post resulting in
 * member(witnesses(Witnesses), Options)
 *
 * Options is a list which contains any of:
 *
 *  * transaction_records([transaction_record{ pre_database : Pre_Database_1,
 *                                             write_graphs : [Graph_Id_1_1... Graph_Id_1_k],
 *                                             update_database : Update_Database_1,
 *                                             post_database : Post_Database_1 },
 *                         ...
 *                         transaction_record{ pre_database : Pre_Database_n,
 *                                             write_graphs : [Graph_Id_n_1 ... Graph_Id_n_j],
 *                                             update_database : Update_Database_n,
 *                                             post_database : Post_Database_n }])
 *
 *  Specifying each of the databases which exist in the pre, update and post phases.
 *  Each database object binds in turn the layers and builders of the transaction
 *  in its associated read_obj / write_obj fields.
 *
 *  * witnesses(Witnesses)
 *
 *  Which gives a list of the witnesses of failure.
 *
 */
with_transaction(Options,Query_Update,Post) :-
    (   get_transaction_records(Options,Records)
    ->  true
    ;   format(string(MSG), "No transaction records in options: ~q", [Options]),
        throw(http_reply(not_acceptable(_{'terminus:status' : 'terminus:error',
                                         'terminus:message' : MSG})))),

    % get read layer and layer builders for transaction
    (   open_pre_transaction(Records)
    ->  true
    ;   format(string(MSG), "Unable to process pre transaction records: ~q", [Records]),
        throw(http_reply(not_acceptable(_{'terminus:status' : 'terminus:error',
                                         'terminus:message' : MSG})))),

    (   call(Query_Update)
    ->  true
    ;   format(string(MSG), "Unable to perform the update requested: ~q", [Query_Update]),
        throw(http_reply(not_acceptable(_{'terminus:status' : 'terminus:error',
                                         'terminus:message' : MSG})))),

    (   open_post_transaction(Records)
    ->  true
    ;   format(string(MSG), "Unable to process post transaction records: ~q", [Records]),
        throw(http_reply(not_acceptable(_{'terminus:status' : 'terminus:error',
                                         'terminus:message' : MSG})))),

    (   call(Post)
    ->  (   memberchk(witnesses([]),Options)
        ->  (   commit_transaction(Records)
            ->  true
            ;   format(string(MSG), "Failed to commit transaction: ~q", [Records]),
                throw(http_reply(not_acceptable(_{'terminus:status' : 'terminus:error',
                                                  'terminus:message' : MSG}))))
        ;   true)
    ;   format(string(MSG), "Unable to run post_condition: ~q", [Post]),
        throw(http_reply(not_acceptable(_{'terminus:status' : 'terminus:error',
                                         'terminus:message' : MSG})))).


		 /*******************************
		 *     MISSING PREDICATES       *
		 *******************************/


collection_directory(DB_URI,Path) :-
    throw(error(system_error,
      context(collection_directory(DB_URI, Path), 'collection_directory/2 does not exist'))).


current_checkpoint_directory(A,B,C) :-
    throw(error(system_error,
      context(current_checkpoint_directory(A, B, C),
              'current_checkpoint_directory/3 does not exist'))).


graph_directory(A,B,C) :-
    throw(error(system_error,
      context(graph_directory(A, B, C),
              'graph_directory/3 does not exist'))).

graphs(A,B) :-
    throw(error(system_error,
      context(graphs(A,B), 'graphs/2 does not exist'))).

