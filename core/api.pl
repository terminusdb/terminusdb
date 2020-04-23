:- module(api, [
              % db_delete.pl
              delete_db/1,

              % db_init.pl
              create_db/4,

              % init.pl
              initialize_config/4,
              initialize_registry/0,
              initialize_index/3,
              initialize_database/2,
              initialize_database_with_path/3,
              initialize_database_with_store/3,

              % db_graph.pl
              create_graph/5,
              delete_graph/5,

              % db_branch.pl
              branch_create/5
          ]).

:- use_module(api/init).
:- use_module(api/db_init).
:- use_module(api/db_delete).
:- use_module(api/db_graph).
:- use_module(api/db_branch).
