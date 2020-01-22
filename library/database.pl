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
