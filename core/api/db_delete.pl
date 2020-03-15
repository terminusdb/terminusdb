:- module(db_delete,[
              delete_db/1
          ]).

/** <module> Database Deletion Logic
 *
 * Predicates for deleting databases
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

:- reexport(core(util/syntax)).
:- use_module(core(util)).
:- use_module(core(triple)).

:- use_module(library(terminus_store)).

begin_deleting_db_from_terminus(DB_Name,Graph,Layer,Final) :-
    finalized_element_uri(Finalized),
    deleting_element_uri(Deleting),
    database_state_prop_uri(State_Prop),
    open_write(Layer,Builder),

    db_name_uri(DB_Name, Db_Uri),
    nb_remove_triple(Builder,Db_Uri,State_Prop,node(Finalized)),
    nb_add_triple(Builder,Db_Uri,State_Prop,node(Deleting)),
    nb_commit(Builder,Final),
    nb_set_head(Graph,Final).

delete_db_from_terminus(DB_Name,Graph,Layer) :-
    deleting_element_uri(Deleting),
    database_state_prop_uri(State_Prop),
    database_name_property_uri(Name_Prop),
    open_write(Layer,Builder),

    db_name_uri(DB_Name, Db_Uri),
    nb_remove_triple(Builder,Db_Uri,State_Prop,node(Deleting)),

    (   triple(Layer,Db_Uri,Name_Prop,Name_Val)
    ->  nb_remove_triple(Builder,Db_Uri,Name_Prop,Name_Val)
    ;   true),

    nb_commit(Builder,Final),
    nb_set_head(Graph,Final).


/**
 * delete_db(+DB_Name) is semidet.
 *
 * Deletes a database if it exists, fails if it doesn't.
 */
delete_db(DB_Name) :-
    terminus_graph_layer(Graph,Layer),
    (   db_exists_in_layer(Layer,DB_Name)
    ->  true
    ;   format(atom(M), 'Database does not exist with the name ~q', [DB_Name]),
        throw(error(database_exists,M))),
    % Do something here? User may need to know what went wrong
    db_finalized_in_layer(Layer,DB_Name),

    begin_deleting_db_from_terminus(DB_Name,Graph,Layer,Deleting_Layer),

    db_path(Path),
    www_form_encode(DB_Name,DB_Name_Safe),
    atomic_list_concat([Path,DB_Name_Safe,'.label'], File_Path),

    (   exists_file(File_Path)
    ->  get_time(Time),
        atomic_list_concat([Path,DB_Name_Safe,'-',Time,'.deleted'],Deleted_File),
        rename_file(File_Path,Deleted_File)
    ;   format(atom(M), 'Database files do not exist with the name ~q', [DB_Name]),
        throw(error(database_exists,M))
    ),

    delete_db_from_terminus(DB_Name,Graph,Deleting_Layer).

