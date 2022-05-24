:- module(upgrade_db,[get_db_version/1,
                      set_db_version/1,
                      maybe_upgrade/0
                     ]).

/** <module> Utilities to check and upgrade the database version
 *
 * This module is meant to upgrade old versions of the database to new database
 * formats.
 *
 * We want to create chaining rules for application of lifting operations so that we
 * only have to test each upgrade from V to V+1, but can apply arbitrary version
 * lifts. This is done using term expansion from run_upgrade_step/2 which automatically
 * constructs upgrade_step/2 so that we can test accessibility with accessible/2.
 *
 * In order to add an upgrade step, please add a clause to run_upgrade_step/2 and
 * change the current version set in database_version/1
 *
 * i.e.

 run_upgrade_step('1.2','1.3') :-
     % do stuff here.
     % ...
     true.
     
  * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of TerminusDB.                                     *
 *                                                                       *
 *  TerminusDB is free software: you can redistribute it and/or modify   *
 *  it under the terms of the  the Apache License, Version 2.0           *
 *  (the "License");                                                     *
 *  you may not use this file except in compliance with the License.     *
 *  You may obtain a copy of the License at:                             *
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

:- use_module(core(util)).

:- use_module(config(terminus_config), [db_path/1]).

:- use_module(library(pcre)).
:- use_module(library(lists)).
:- use_module(library(shell)).

:- multifile upgrade_step/2.

/**
 * database_version(-Version) is det.
 *
 * Supplies the current version number of the DB
 */
database_version('1.0.0').

/*
 * get_db_version(-Version) is det.
 *
 * Reports the version associated with the current backing store
 */
get_db_version(Version) :-
    db_path(DB_Path),
    storage_version_path(DB_Path, Version_File),
    (   exists_file(Version_File)
    ->  setup_call_cleanup(
            open(Version_File,read,Stream),
            (   read_string(Stream, "\n", "\r", _End, String),
                atom_string(Version,String)
            ),
            close(Stream)
        )
    ;   Version = 1).

/*
 * set_db_version(+Version) is det.
 *
 * Set the Database version
 */
set_db_version(Version) :-
    db_path(DB_Path),
    storage_version_path(DB_Path, Version_File),
    setup_call_cleanup(
        open(Version_File,update, Stream),
        write(Stream,Version),
        close(Stream)
    ).

/*
 * guess_collection_name(+Database_Name,-Collection) is semidet.
 *
 * Try to guess the collection name of graph
 */
guess_collection_name(Database_Name,Collection) :-
    re_matchsub('(?<Collection>(.*))%2fgraph%2f(main|model|import|error).*',
                Database_Name,
                Dict, []),
    get_dict('Collection',Dict,Collection).

/*
 * accessible(Version1,Version2,Path) is det.
 *
 * Determines if one version is accessible from another
 */
accessible(Version,Version,[]).
accessible(Version1,Version2,[Version1,Version2]) :-
    upgrade_step(Version1,Version2).
accessible(Version1,Version2,[Version1|Path]) :-
    upgrade_step(Version1,VersionX),
    accessible(VersionX, Version2, Path).

maybe_upgrade :-
    database_version(Target_Version),
    get_db_version(Current_Version),
    (   accessible(Current_Version, Target_Version, Path)
    ->  run_upgrade(Path)
    ;   format(atom(M), 'The version ~q is not accessible from database version ~q', [Target_Version,Current_Version]),
        throw(error(M))
    ).

run_upgrade([]).
run_upgrade([_Last_Version]).
run_upgrade([Version1,Version2|Rest]) :-
    run_upgrade_step(Version1,Version2),
    run_upgrade([Version2|Rest]).

/*
 * upgrade_step(Version1,Version2) is semidet.
 *
 * Describes if there is an upgrade from Version1 to Version2
 */
:- discontiguous upgrade_step/2.

/*
 * We do term expansion to add a upgrade_step/2 with the two
 * head terms from run_upgrade_step/2 so we can check accessibility
 * of an upgrade.
 */
:- multifile user:term_expansion/2.
:- dynamic user:term_expansion/2.

user:term_expansion((run_upgrade_step(X,Y):-Body),
                    [(run_upgrade_step(X,Y):-Body),
                     upgrade_step(X,Y)]).

/*
 * run_upgrade_step(X,Y) is nondet.
 *
 * Perform an upgrade step from version X, to version Y
 *
 * NOTE: shortcuts should go first in the clause order.
 */
:- discontiguous run_upgrade_step/2.
run_upgrade_step(none,'1.0.0') :-
    db_path(Path),
    subdirectories(Path,Database_Names),
    forall(
        member(Database_Name,Database_Names),
        (
            (   guess_collection_name(Database_Name,Collection_Name)
            ->  interpolate([Path,Collection_Name], Collection_Path),
                interpolate([Path,Database_Name], Database_Path),
                ensure_directory(Collection_Path),
                mv(Database_Path,Collection_Path),
                interpolate([Collection_Path,'/COLLECTION'], Collection_Marker),
                touch(Collection_Marker)
            ;   true % not really a graph
            )
        )
    ),
    set_db_version('1.0.0').
