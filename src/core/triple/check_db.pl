:- module(upgrade_db,[get_db_version/1,
                      get_db_version/2,
                      set_db_version/0,
                      set_db_version/1,
                      set_db_version/2,
                      database_version/1,
                      assert_database_version_is_current/1
                     ]).

/** <module> Utilities to check the database version
 *
 * This module is meant to check the current version of the store, and
 * has utilities for getting and setting its version.
 */

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
database_version(2).

/*
 * get_db_version(-Version) is det.
 *
 * Reports the version associated with the current backing store
 */
:- table get_db_version/1.
get_db_version(Version) :-
    db_path(Path),
    get_db_version(Path, Version).

:- table get_db_version/2.
get_db_version(Path, Version) :-
    storage_version_path(Path, Version_File),
    (   exists_file(Version_File)
    ->  setup_call_cleanup(
            open(Version_File,read,Stream),
            (   read_string(Stream, "\n", "\r", _End, String),
                (   number_string(Version, String)
                ->  true
                ;   throw(error(storage_version_not_a_number, _))
                )
            ),
            close(Stream)
        )
    ;   (   \+ exists_directory(Path)
        ->  throw(error(storage_path_does_not_exist(Path), _))
        ;   directory_files(Path, Entries),
            subtract(Entries, ['.', '..'], [])
        ->  throw(error(storage_path_empty(Path), _))
        ;   throw(error(no_database_store_version, _))
        )
    ).


/*
 * set_db_version is det.
 *
 * Set the Database version to the current database version
 */
set_db_version :-
    db_path(Path),
    set_db_version(Path).

/*
 * set_db_version(+Version) is det.
 *
 * Set the Database version to Version
 */
set_db_version(Path) :-
    database_version(Version),
    set_db_version(Path, Version).

set_db_version(Path, Version) :-
    number_string(Version, Version_String),
    storage_version_path(Path, Version_File),
    setup_call_cleanup(
        open(Version_File,write, Stream),
        write(Stream,Version_String),
        close(Stream)
    ).

assert_database_version_is_current(Path) :-
    get_db_version(Path, Current_Version),
    database_version(Expected_Version),

    (   Current_Version = Expected_Version
    ->  true
    ;   Current_Version < Expected_Version
    ->  throw(error(store_outdated(Current_Version, Expected_Version), _))
    ;   throw(error(server_outdated(Current_Version, Expected_Version), _))).

:- begin_tests(upgrade_db).
:- use_module(library(lists)).

%  A scratch directory name unique per test run.
store_test_dir(Dir) :-
    tmp_file(check_db_test, Path),
    atomic_list_concat([Path, '-dir'], Dir).

test(missing_path_errors_distinctly, [
         throws(error(storage_path_does_not_exist('/definitely/not/a/store-path'), _))
     ]) :-
    get_db_version('/definitely/not/a/store-path', _).

test(empty_directory_errors_distinctly, [
         setup((store_test_dir(Dir), make_directory_path(Dir))),
         cleanup(delete_directory_and_contents(Dir)),
         throws(error(storage_path_empty(Dir), _))
     ]) :-
    get_db_version(Dir, _).

test(uninitialized_directory_errors_distinctly, [
         setup((store_test_dir(Dir),
                make_directory_path(Dir),
                directory_file_path(Dir, 'somefile', F),
                open(F, write, S),
                close(S))),
         cleanup(delete_directory_and_contents(Dir)),
         throws(error(no_database_store_version, _))
     ]) :-
    get_db_version(Dir, _).

test(corrupt_version_file_errors_distinctly, [
         setup((store_test_dir(Dir),
                make_directory_path(Dir),
                directory_file_path(Dir, 'STORAGE_VERSION', F),
                open(F, write, S),
                write(S, 'not-a-number'),
                close(S))),
         cleanup(delete_directory_and_contents(Dir)),
         throws(error(storage_version_not_a_number, _))
     ]) :-
    get_db_version(Dir, _).

test(valid_version_file_reads, [
         setup((store_test_dir(Dir),
                make_directory_path(Dir),
                directory_file_path(Dir, 'STORAGE_VERSION', F),
                open(F, write, S),
                write(S, '2'),
                close(S))),
         cleanup(delete_directory_and_contents(Dir))
     ]) :-
    get_db_version(Dir, Version),
    Version = 2.

:- end_tests(upgrade_db).
