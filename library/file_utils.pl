:- module(file_utils,[
              terminus_path/1,
              db_relative_path/1,
              db_path/1,
              touch/1,
              ensure_directory/1,
              sanitise_file_name/2,
              subdirectories/2,
              files/2,
              directories/2,
              terminus_schema_path/1,
              temp_path/1
          ]).

:- use_module(utils).

:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(apply_macros)).

/** <module> File Utils
 *
 * Utility predicate which help in the manipulation of files and store
 * some file system constants.
 *
 * * * * * * * * * * * * * COPYRIGHT NOTICE  * * * * * * * * * * * * * * *
 *                                                                       *
 *  This file is part of TerminusDB.                                      *
 *                                                                       *
 *  TerminusDB is free software: you can redistribute it and/or modify    *
 *  it under the terms of the GNU General Public License as published by *
 *  the Free Software Foundation, under version 3 of the License.        *
 *                                                                       *
 *                                                                       *
 *  TerminusDB is distributed in the hope that it will be useful,         *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
 *  GNU General Public License for more details.                         *
 *                                                                       *
 *  You should have received a copy of the GNU General Public License    *
 *  along with TerminusDB.  If not, see <https://www.gnu.org/licenses/>.  *
 *                                                                       *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 */

/**
 * db_relative_path(-Path) is det.
 *
 * Storage location for hdt files
 */
db_relative_path('/storage/db/').

/**
 * db_relative_path(-Path) is det.
 *
 * Storage location for hdt files
 */
temp_relative_path('/temp/').

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
 * db_path(-Path) is det.
 *
 */
db_path(Path) :-
    terminus_path(BasePath),
    db_relative_path(RelPath),
    interpolate([BasePath,RelPath],Path).

/**
 * temp_path(-Path) is det.
 *
 * Temporary file location
 */
temp_path(Path) :-
    terminus_path(BasePath),
    temp_relative_path(RelPath),
    interpolate([BasePath,RelPath],Path).

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
 * ensure_directory(+Path) is det.
 *
 * Create a directory if it does not already exist
 */
ensure_directory(Directory) :-
    (   exists_directory(Directory)
    ->  true
    ;   make_directory(Directory)).

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
    atom_concat('.#', _, File).
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
