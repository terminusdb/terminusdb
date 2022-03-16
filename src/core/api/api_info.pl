:- module(api_info, [info/3]).

:- use_module(core(util)).
:- use_module(core(triple)).
:- use_module(config(terminus_config), [terminusdb_version/1]).

:- use_module(library(prolog_pack), [pack_property/2]).

info(_System_DB, Auth, Info) :-

    do_or_die(
        Auth \= doc:anonymous,
        error(access_not_authorized(Auth),_)),

    terminusdb_version(TerminusDB_Version),
    current_prolog_flag(terminus_store_prolog_version, TerminusDB_Store_Version),
    current_prolog_flag(terminusdb_git_hash, Git_Hash),
    get_db_version(Storage_Version),

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
                   version: Storage_Version
               }
           }.
