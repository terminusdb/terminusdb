:- module(api, [delete_db/1,
                create_db/2,
                initialize_config/4,
                initialize_registry/0,
                initialize_index/3,
                initialize_database/2,
                initialize_database_with_path/3,
                initialize_database_with_store/3
               ]).

:- use_module(api/init).
:- use_module(api/db_init).
:- use_module(api/db_delete).
