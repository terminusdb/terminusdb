:- module(db_delete,[
              try_delete_db/1,
              delete_db/1,
              force_delete_db/1
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
:- use_module(core(query)).
:- use_module(core(transaction)).

:- use_module(library(terminus_store)).

begin_deleting_db_from_terminus(Terminus,DB_Name) :-
    with_transaction(
        Terminus,
        ask(Terminus,
            (   t(Db_Uri, terminus:resource_name, DB_Name^^xsd:string),
                t(Db_Uri, terminus:database_state, terminus:finalized),
                delete(Db_Uri, terminus:database_state, terminus:finalized, "instance/main"),
                insert(Db_Uri, terminus:database_state, terminus:deleting, "instance/main"))),
        _Meta_Data).

delete_db_from_terminus(Terminus,DB_Name) :-
    with_transaction(
        Terminus,
        ask(Terminus,
            (   t(Db_Uri, terminus:resource_name, DB_Name^^xsd:string),
                delete_object(Db_Uri))),
        _Meta_Data).

/**
 * delete_db(+DB_Name) is semidet.
 *
 * Deletes a database if it exists, fails if it doesn't.
 */
delete_db(DB_Name) :-
    create_context(terminus_descriptor{}, Terminus),
    (   database_exists(Terminus,DB_Name)
    <>  throw(database_does_not_exist(DB_Name))),
    % Do something here? User may need to know what went wrong

    do_or_die(
        database_finalized(Terminus,DB_Name),
        error(database_not_finalized(DB_Name))),

    begin_deleting_db_from_terminus(Terminus,DB_Name),

    db_path(Path),
    www_form_encode(DB_Name,DB_Name_Safe),
    atomic_list_concat([Path,DB_Name_Safe,'.label'], File_Path),

    (   exists_file(File_Path)
    ->  delete_file(File_Path)
    ;   throw(database_files_do_not_exist(DB_Name))
    ),

    create_context(terminus_descriptor{}, Terminus_Staged),
    delete_db_from_terminus(Terminus_Staged,DB_Name).

/* Force deletion of databases in an inconsistent state */
force_delete_db(DB_Name) :-
    create_context(terminus_descriptor{}, Terminus),
    ignore(delete_db_from_terminus(Terminus,DB_Name)),
    db_path(Path),
    www_form_encode(DB_Name,DB_Name_Safe),
    atomic_list_concat([Path,DB_Name_Safe,'.label'], File_Path),
    catch(
        delete_file(File_Path),
        error(existence_error(file,_Fname), _Ctx),
        true).

/*
 * try_delete_db(DB_URI) is det.
 *
 * Attempt to delete a database given its URI
 */
try_delete_db(DB) :-
    do_or_die(
        delete_db(DB),
        error(database_cannot_be_deleted(DB))).
