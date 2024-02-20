:- module(file_utils,[
              terminus_path/1,
              storage_version_path/2,
              touch/1,
              sanitise_file_name/2,
              subdirectories/2,
              files/2,
              directories/2,
              terminus_schema_path/1,
              file_to_predicate/2
          ]).

:- use_module(utils).

:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(apply_macros)).
:- use_module(library(lists)).
:- use_module(library(pcre)).
:- use_module(library(url)).
:- use_module(library(filesex)).

/** <module> File Utils
 *
 * Utility predicate which help in the manipulation of files and store
 * some file system constants.
 *
 * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of TerminusDB.                                     *
 *                                                                       *
 *  TerminusDB is free software: you can redistribute it and/or modify   *
 *  it under the terms of the  the Apache License, Version 2.0           *
 *  (the "License");                                                     *
 *  you may not use this file except in compliance with the License.     *
 *  You may obtain a copy of the License at                              *
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

/**
 * terminus_path(-Path) is det.
 *
 * Fully qualified path name to current terminus installation.
 */
terminus_path(Path) :-
    once(
        file_search_path(terminus_home,Path)
    ).

/**
 * storage_version_path(-Path) is det.
 *
 * Path to storage version file.
 */
storage_version_path(DB_Path, Path) :-
    directory_file_path(DB_Path, 'STORAGE_VERSION', Path).

/**
 * terminus_schema_path(Path) is det.
 *
 * The path to the current terminus schema directory
 */
terminus_schema_path(Path) :-
    terminus_path(Base_Path),
    Schema_Relative_Path = '/terminus-schema/',
    interpolate([Base_Path,Schema_Relative_Path], Path).

/**
 * touch(+File) is det.
 *
 * Make an empty file, or change time stamp of current
 */
touch(File) :-
    open(File,append,Stream),
    close(Stream).

/**
 * sanitise_file_name(+G,-F) is det.
 *
 * Replace nasty characters.
/ * WW_FORM encoding seems to work...
 */
sanitise_file_name(G,F) :-
    www_form_encode(G,F).

/**
 * subdirectories(+Dir,-Dirs) is semidet.
 *
 * Show subdirectories only.
 */
subdirectories(Dir,Dirs) :-
    exists_directory(Dir),
    directory_files(Dir,Files),
    include({Dir}/[File]>>(\+ member(File,['.','..']),
                           interpolate([Dir,'/',File],FullPath),
                           exists_directory(FullPath)),
            Files,Dirs).

/**
 * excluded_file(File) is semidet.
 *
 * Files for exclusion from listings.
 * The categories are exclusive disjunctions, and the
 * predicate is therefore intended to be semidet.
 */
excluded_file(File) :-
    % emacs files
    sub_atom(File, 0, _, _, '.#').
excluded_file(File) :-
    % current directory and parent directory
    member(File, ['.','..']).

/**
 * files(+Dir,-NormalFiles) is semidet.
 *
 * Show normal (non-directory) files only.
 */
files(Dir,NormalFiles) :-
    directory_files(Dir,Files),
    include({Dir}/[File]>>(\+ excluded_file(File),
                           interpolate([Dir,'/',File],FullPath),
                           \+ exists_directory(FullPath)),
            Files,NormalFiles).
/**
 * directories(+Dir,-Directories) is semidet.
 *
 * Show directory names only.
 */
directories(Dir, Directories) :-
    directory_files(Dir,Files),
    include({Dir}/[File]>>
            (
                \+ excluded_file(File),
                interpolate([Dir,'/',File],FullPath),
                exists_directory(FullPath)
            ),
            Files, Directories).

/**
 * file_to_predicate(+Path, +Predicate) is det.
 *
 * Convert a file into a predicate
*/
:- meta_predicate file_to_predicate(?,1).
file_to_predicate(Path,Predicate) :-
    strip_module(Predicate, Module, P),
    Predicate_Retractall =.. [P, _],
    setup_call_cleanup(
        open(Path, read, Out),
        (   retractall(Module:Predicate_Retractall),
            read_string(Out,_,String),
            Predicate_Assert =.. [P, String],
            assertz(Module:Predicate_Assert)),
        close(Out)
    ).
