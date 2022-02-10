:- module(triple, [
              % base_type.pl
              base_type/1,
              base_type_parent/2,
              basetype_subsumption_of/2,

              % casting.pl
              typecast/4,

              % database_utils.pl
              system_graph_layer/2,
              database_instance/2,
              database_inference/2,
              database_schema/2,
              organization_database_name/3,
              excluded_organization/1,
              excluded_database/1,
              error_on_excluded_organization/1,
              error_on_excluded_database/1,

              % iana.pl
              iana/3,

              % literals.pl
              literal_to_turtle/2,
              normalise_triple/2,
              object_storage/2,
              ground_object_storage/2,
              storage_object/2,
              date_string/2,
              duration_string/2,
              gyear_string/2,
              gmonth_string/2,
              gyear_month_string/2,
              gmonth_day_string/2,
              gday_string/2,
              current_xsd_date_time/1,
              uri_to_prefixed/3,
              schema_uri_to_prefixed/3,
              instance_uri_to_prefixed/3,
              prefixed_to_uri/3,
              prefixed_to_property/3,

              % temp_graph.pl
              extend_database_with_temp_graph/6,

              % constants.pl
              system_instance_name/1,
              system_schema_name/1,
              system_inference_name/1,
              layer_ontology/1,
              repository_ontology/1,
              ref_ontology/1,
              woql_ontology/1,

              rdf_type_uri/1,
              xsd_string_type_uri/1,
              xsd_any_uri_type_uri/1,
              label_prop_uri/1,
              comment_prop_uri/1,

              database_class_uri/1,
              resource_name_property_uri/1,
              database_state_prop_uri/1,
              finalized_element_uri/1,
              deleting_element_uri/1,
              resource_includes_prop_uri/1,
              organization_database_prop_uri/1,
              allow_origin_prop_uri/1,

              layer_class_uri/1,
              layer_id_prop_uri/1,

              branch_class_uri/1,
              ref_commit_prop_uri/1,
              ref_settings_class_uri/1,
              ref_settings_base_uri_prop_uri/1,
              ref_branch_base_uri_prop_uri/1,
              ref_branch_name_prop_uri/1,

              local_repository_class_uri/1,
              remote_repository_class_uri/1,

              repository_head_prop_uri/1,
              repository_name_prop_uri/1,
              repository_remote_url_prop_uri/1,
              repo_type_document_prefix/2,

              admin_organization_uri/1,
              super_user_authority/1,

              % triplestore
              safe_create_named_graph/3,
              safe_named_graph_exists/2,
              safe_open_named_graph/3,
              safe_delete_named_graph/2,
              xrdf/4,
              xquad/5,
              xrdf_db/4,
              xrdf_deleted/4,
              xrdf_added/4,
              insert/5,
              delete/5,
              delete_all/1,
              unlink_object/2,
              storage/1,
              triple_store/1,
              global_triple_store/1,
              local_triple_store/1,
              set_local_triple_store/1,
              retract_local_triple_store/1,
              default_triple_store/1,
              memory_triple_store/1,
              with_triple_store/2,

              % turtle_utils.pl
              graph_to_turtle/4,
              update_turtle_graph/2,
              insert_turtle_graph/2,
              dump_turtle_graph/2,

              % upgrade_db.pl
              get_db_version/1,
              set_db_version/1,
              maybe_upgrade/0
          ]).

:- use_module(triple/base_type).
:- use_module(triple/casting).
:- use_module(triple/database_utils).
:- use_module(triple/iana).
:- use_module(triple/literals).
:- use_module(triple/temp_graph).
:- use_module(triple/constants).
:- use_module(triple/triplestore).
:- use_module(triple/turtle_utils).
:- use_module(triple/upgrade_db).
