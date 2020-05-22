:- module(api, [
              % db_delete.pl
              delete_db/1,

              % db_init.pl
              create_db/4,

              % init.pl
              initialize_config/4,
              initialize_registry/0,
              initialize_index/2,
              initialize_database/2,
              initialize_database_with_path/3,
              initialize_database_with_store/3,

              % db_graph.pl
              create_graph/5,
              delete_graph/5,

              % db_branch.pl
              branch_create/4,

              % db_rebase.pl
              rebase_on_branch/7,

              % db_fast_forward.pl
              % fast_forward_branch/4

              % db_pack.pl
              repository_context__previous_head_option__current_repository_head__pack/4,
              payload_repository_head_and_pack/3,
              repository_head_layerid/2,
              unpack/1,
              layer_layerids/2
          ]).

:- use_module(api/init).
:- use_module(api/db_init).
:- use_module(api/db_delete).
:- use_module(api/db_graph).
:- use_module(api/db_branch).
:- use_module(api/db_rebase).
:- use_module(api/db_fast_forward).
:- use_module(api/db_pack).
