:- module(api_info, [info/3]).

:- use_module(core(util)).
:- use_module(core(triple)).

info(_System_DB, Auth, Info) :-

    do_or_die(
        Auth \= doc:anonymous,
        error(access_not_authorized(Auth),_)),

    version(TerminusDB_Version),
    current_prolog_flag(terminus_store_prolog_version, TerminusDB_Store_Version),
    get_db_version(Storage_Version),

    Info = _{
               authority: Auth,
               terminusdb :
               _{
                   version : TerminusDB_Version
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
