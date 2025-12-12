:- module(api_info, [info/3]).

:- use_module(core(util)).
:- use_module(core(triple)).
:- use_module(config(terminus_config), [terminusdb_version/1,
                                        is_memory_mode/0]).

:- use_module(library(terminus_store), [terminus_store_version/1]).

info(_System_DB, Auth, Info) :-

    do_or_die(
        Auth \= doc:anonymous,
        error(access_not_authorized(Auth),_)),

    terminusdb_version(TerminusDB_Version),
    terminus_store_version(TerminusDB_Store_Version),
    current_prolog_flag(terminusdb_git_hash, Git_Hash),
    % In memory mode, there's no storage version file - use current database_version
    (   is_memory_mode
    ->  database_version(Storage_Version)
    ;   get_db_version(Storage_Version)
    ),
    number_string(Storage_Version, Storage_Version_String),

    Info = _{
               authority: Auth,
               terminusdb :
               _{
                   version : TerminusDB_Version,
                   git_hash : Git_Hash
               },
               terminusdb_store :
               _{
                   version : TerminusDB_Store_Version
               },
               storage :
               _{
                   version: Storage_Version_String
               }
           }.
