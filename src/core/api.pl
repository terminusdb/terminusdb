:- module(api, [
              % init.pl
              bootstrap_files/0,
              index_template/1,
              initialize_flags/0,
              initialize_database/2,
              initialize_database_with_store/2,
              world_ontology_json/1,

              % db_delete.pl
              delete_db/5,
              force_delete_db/2,

              % db_create.pl
              create_db/9,
              create_db_unfinalized/10,
              create_schema/3,
              create_ref_layer/1,

              % db_branch.pl
              branch_create/5,
              branch_delete/3,

              % db_rebase.pl
              rebase_on_branch/9,
              cycle_context/4,

              % db_fast_forward.pl
              % fast_forward_branch/4

              % db_pack.pl
              payload_repository_head_and_pack/3,
              repository_head_layerid/2,
              unpack/1,
              pack/5,
              pack_from_context/3,
              layer_layerids/2,

              % db_fetch.pl
              remote_fetch/6,
              authorized_fetch/4,

              % db_clone.pl
              clone/10,

              % db_push.pl
              push/8,
              authorized_push/3,

              % db_unpack.pl
              unpack/4,

              % db_pull.pl
              pull/7,

              % graph_load.pl
              graph_update/6,
              graph_insert/6,

              % graph_dump.pl
              graph_dump/5,

              % api_frame.pl
              api_class_frame/5,
              api_filled_frame/5,

              % api_woql.pl
              woql_query_json/11,

              % api_squash.pl
              api_squash/6,

              % api_reset.pl
              api_reset/4,

              % api_optimize.pl
              api_optimize/3,

              % api_prefixes
              get_prefixes/4,
              update_prefixes/5,

              % api_db
              list_databases/4,
              list_database/6,
              list_existing_databases/3,
              pretty_print_databases/1,
              db_exists_api/4,

              % api_error.pl
              api_error_jsonld/3,
              api_error_jsonld/4,
              status_http_code/2,
              status_cli_code/2,
              generic_exception_jsonld/2,
              json_http_code/2,
              json_cli_code/2,

              % api_info.pl
              info/3,

              % api_remote.pl
              add_remote/5,
              remove_remote/4,
              update_remote/5,
              show_remote/5,
              list_remotes/4,

              % api_rollup.pl
              api_rollup/5,

              % api_bundle.pl
              bundle/5,

              % api_unbundle.pl
              unbundle/4,

              % api_document.pl
              api_get_document/6,
              api_get_documents/11,
              api_get_documents_by_type/12,
              api_get_documents_by_query/13,
              api_get_document_by_id/10,
              api_insert_documents/8,
              api_delete_documents/8,
              api_delete_document/7,
              api_replace_documents/8,
              api_nuke_documents/6,
              api_generate_document_ids/6,
              api_read_document_selector/16,

              % api_user_organizations.pl
              user_organizations/3,

              % api_patch.pl
              api_patch/6,
              api_diff/6,
              api_diff_id/8,
              api_diff_id_document/8,
              api_diff_all_documents/7,
              api_apply_squash_commit/7,

              % api_log.pl
              api_log/4,
              format_log/2,

              % api_access_control.pl
              api_get_roles/3,
              api_get_role_from_name/4,
              api_get_role_from_id/4,
              api_add_role/4,
              api_delete_role/3,
              api_update_role/3,
              api_get_organizations/3,
              api_get_organization_from_name/4,
              api_get_organization_from_id/4,
              api_add_organization/4,
              api_delete_organization/3,
              api_grant_capability/3,
              grant_document_to_ids/4,
              api_revoke_capability/3,
              api_get_resource_from_name/5,
              api_get_user_from_name/5,
              api_add_user/4,
              api_delete_user/3,
              api_add_user/4,
              api_delete_user/3,
              api_get_users/4,
              api_get_user_from_id/5,
              api_update_user_password/4,
              api_get_organizations_users/4,
              api_get_organizations_users_object/5,
              api_get_organizations_users_databases/5
          ]).

:- use_module(api/api_init).
:- use_module(api/db_create).
:- use_module(api/db_delete).
:- use_module(api/db_branch).
:- use_module(api/db_rebase).
:- use_module(api/db_fast_forward).
:- use_module(api/db_pack).
:- use_module(api/db_fetch).
:- use_module(api/db_clone).
:- use_module(api/db_push).
:- use_module(api/db_unpack).
:- use_module(api/db_pull).
:- use_module(api/graph_load).
:- use_module(api/graph_dump).
:- use_module(api/api_frame).
:- use_module(api/api_woql).
:- use_module(api/api_squash).
:- use_module(api/api_reset).
:- use_module(api/api_optimize).
:- use_module(api/api_prefixes).
:- use_module(api/api_db).
:- use_module(api/api_error).
:- use_module(api/api_info).
:- use_module(api/api_remote).
:- use_module(api/api_rollup).
:- use_module(api/api_bundle).
:- use_module(api/api_unbundle).
:- use_module(api/api_document).
:- use_module(api/api_user_organizations).
:- use_module(api/api_patch).
:- use_module(api/api_log).
:- use_module(api/api_access_control).
