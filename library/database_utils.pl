:- module(database_utils,[
              create_db/1,
              delete_db/1,
              database_exists/1
          ]).

/** <module> Database Utilities
 * 
 * Various database level utilities. This is a layer above the triple store 
 * in terms of logic, and placed here as we want to be able to make use 
 * of WOQL and other libraries without circularity.
 *
 * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of TerminusDB.                                     *
 *                                                                       *
 *  TerminusDB is free software: you can redistribute it and/or modify   *
 *  it under the terms of the GNU General Public License as published by *
 *  the Free Software Foundation, either version 3 of the License, or    *
 *  (at your option) any later version.                                  *
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

:- use_module(library(file_utils)).
:- use_module(library(triplestore)).
:- use_module(library(utils)).
:- use_module(library(journaling)).
:- use_module(library(database)).


/* 
 * database_exists(DB_URI) is semidet.
 */ 
database_exists(DB_URI) :-
    collection_directory(DB_URI,DB_Path),
    exists_directory(DB_Path).

/** 
 * create_db(+DB:atom) is semidet.
 * 
 * Create a new empty graph
 */
create_db(DB_URI) :-

    % create the collection if it doesn't exist
    (   database_exists(DB_URI)
    ->  throw(http_reply(method_not_allowed('terminus:createdatabase')))
    ;   true),

    collection_directory(DB_URI,DB_Path),
    ensure_directory(DB_Path),
    interpolate([DB_Path,'/COLLECTION'],DB_File),
    touch(DB_File),

    % Set up schemata
    forall(
        (
            database_record_schema_list(DB_URI,Schemata),
            member(Schema,Schemata)
        ),
        (   
            % create the graph if it doesn't exist
            graph_directory(DB_URI,Schema,Schema_Path),
            ensure_directory(Schema_Path),
            make_checkpoint_directory(DB_URI, Schema, _),

            with_output_graph(
                graph(DB_URI,Schema,ckp,ttl),
                (
                    interpolate([DB_URI],Label),
                    interpolate([Schema,' ontology for ',DB_URI],Comment),
                    write_triple(DB_URI,Schema,ckp,DB_URI,rdf:type,owl:'Ontology'),
                    write_triple(DB_URI,Schema,ckp,DB_URI,rdfs:label,literal(lang(en,Label))),
                    write_triple(DB_URI,Schema,ckp,DB_URI,rdfs:comment,literal(lang(en,Comment)))
                )
            ),
            sync_from_journals(DB_URI,Schema)
        )
    ),

    % Set up instance graphs
    forall(
        (
            database_record_instance_list(DB_URI,Instances),
            member(Instance,Instances)
        ),
        (   
            % create the graph if it doesn't exist
            graph_directory(DB_URI,Instance,Instance_Path),
            ensure_directory(Instance_Path),
            make_checkpoint_directory(DB_URI, Instance, _),

            % Just make an empty checkpoint.
            with_output_graph(
                graph(DB_URI,Instance,ckp,ttl),
                (
                    true
                )
            ),
            
            sync_from_journals(DB_URI,Instance)
        )
    ).
    
delete_db(DB) :-
    collection_directory(DB,DB_Path),
    delete_directory_and_contents(DB_Path).
