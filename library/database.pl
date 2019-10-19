:- module(database,[
              database_name/2,
              database_instance/2,
              database_schema/2,
              database_error_instance/2,
              database_error_schema/2,
              database_inference/2,
              make_database/2,
              make_raw_database/2,
              database_identifiers/2,
              terminus_database_name/1,
              terminus_database/1,
              terminus_context/1,
              make_database_from_database_name/2,
              database_record_instance_list/2,
              database_record_schema_list/2,
              database_record_inference_list/2,
              is_schema_graph/2,
              default_instance_graph/2,
              db_size/2,
              db_modified_datetime/2,
              db_created_datetime/2
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

:- use_module(library(utils)).
:- use_module(library(types)).
:- use_module(library(sdk)).
:- use_module(library(file_utils)).

:- use_module(library(apply)).
:- use_module(library(apply_macros)).

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
    config:server(Server),
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
    config:server(Server),
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
                   instance=[Instance]], Database).


database_record_list(Databases) :-
    terminus_database_name(Terminus_Name),
    connect(Terminus_Name, Terminus_DB),

    findall(DB_Resource, 
            ask(Terminus_DB,
                t( DB_Resource , rdf/type , terminus/'Database') ,
               ),
            Databases).


database_record_schema_list(Database_Name, Schemas) :-
    terminus_database_name(Terminus_Name),
    connect(Terminus_Name,Terminus_DB),
    
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


database_record_instance_list(Database_Name,Instances) :-
    terminus_database_name(Terminus_Name),
    connect(Terminus_Name,Terminus_DB),
    
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

%database_record_inference_list(Database_Name,[inference]) :-
%    terminus_database_name(Database_Name),
%    !.
database_record_inference_list(Database_Name,Inferences) :-
    terminus_database_name(Terminus_Name),
    connect(Terminus_Name,Terminus_DB),
    
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
 * make_database_from_database_name(+URI,-Database) is det.
 * 
 */ 
make_database_from_database_name(Database_Name,Database) :-
    % Need a special case for the master database
    (   terminus_database_name(Database_Name)
    ->  terminus_database(Database)
    ;   database_record_schema_list(Database_Name,Schemata),
        database_record_inference_list(Database_Name,Inferences),
        database_record_instance_list(Database_Name,Instances),
        
        make_database([name=Database_Name,
                       schema=Schemata,
                       instance=Instances,
                       inference=Inferences
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
    member(S,Schemata).


default_instance_graph(Database,I) :-
    database_name(Database, Name),
    interpolate([Name,'/document'],I),
    (   database_record_instance_list(Name,Instances),
        member(I, Instances)
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
                      member(Graph_ID,Graph_IDs),
                      current_checkpoint_directory(DB_URI,Graph_ID,CPD),
                      files(CPD,Files),
                      member(File,Files),
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
                      member(Graph,Graphs),
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
    maplist({N}/[G, read_obj{dbid : N, graphid : G, layer : L}]>>(
                dbid_graphid_obj(N,G,Obj)
                head(Obj, L)
            ), Graphs, NL),
    Database2 = Database1.put(database{read_transaction=NL}).

/* 
 * open_write_transaction(+DB1, +Graphs, -DB2) is det.
 * 
 * Open a write transaction. 
 */ 
open_write_transaction(Database1, Graphs, Database2) :-
    maplist([G, write_obj{dbid : _N, graphid : _G, builder : B} ]>>(
                store(Store),
                open_write(Store, B)                
            ), Graphs, NL),
    Database2 = Database1.put( database{write_transaction=NL} ).

commit_write_transaction(Database1, Database2) :-
    Database1.write_transactions = WTs,
    maplist([write_obj{dbid: N, graphid: G, builder: B},
             read_obj{dbid: N, graphid: G, layer: L}]>>(
                nb_commit(B, L)
            ), WTs,RTs),
    Database1.read_transactions = RTs1,
    update_read_layers(RTs1, RTs, RTs2),                     
    Database1 = Database1.put( database{write_transactions=[]})
                         .put( database{read_transactions=RTs2}).

update_read_layers(RTs, [], RTs).
update_read_layers(RTs1, [read_obj{dbid:N, graphid:G, layer: L}|Records], RTs2) :-
    select(read_obj{dbid:N, graphid:G, layer: _}, RTs1, 
           read_obj{dbid:N, graphid:G, layer: L}, RTs_I),
    update_read_layers(RTs_I, Records, RTs2). 

get_read_layer(Database,G,L) :-
    Database.name = DBN, 
    Database.read_transaction = RT, 
    memberchk( read_obj{dbid: DBN, graphid: G, layer: L}, RT).

get_write_builder(Database,G,W) :-
    Database.name = DBN, 
    Database.write_transaction = WT, 
    memberchk( write_obj{dbid: DBN, graphid: G, builder: W}, WT).
