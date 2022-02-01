:- module(api_info, [info/3]).

:- use_module(core(util)).
:- use_module(core(triple)).

info(_System_DB, Auth, Info) :-

    do_or_die(
        Auth \= doc:anonymous,
        error(access_not_authorized(Auth),_)),

    version(TerminusDB_Version),
    (   pack:pack_property(terminus_store_prolog, version(TerminusDB_Store_Version))
    ->  true
    ;   TerminusDB_Store_Version = 'unknown'),
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
