:- module(api_info, [info/3]).

:- use_module(core(util)).
:- use_module(core(triple)).

info(_System_DB, _Auth, Info) :-
    version(TerminusDB_Version),
    pack:pack_property(terminus_store_prolog, version(TerminusDB_Store_Version)),
    get_db_version(Storage_Version),

    Info = _{
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
