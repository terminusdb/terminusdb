:- module(triple, [
              % base_type.pl
              base_type/1,
              base_type_parent/2,

              % casting.pl
              typecast/4,
              hash/3,
              idgen/3,
              random_idgen/3,

              % database_utils.pl
              db_name_uri/2,
              db_exists_in_layer/2,
              db_finalized_in_layer/2,
              database_exists/1,
              terminus_graph_layer/2,
              database_instance/2,
              database_inference/2,
              database_schema/2,

              % iana.pl
              iana/3,

              % literals.pl
              fixup_schema_literal/2,
              normalise_triple/2,
              object_storage/2,
              storage_object/2,
              date_string/2,
              uri_to_prefixed/3,
              prefixed_to_uri/3,

              % temp_graph.pl
              extend_database_with_temp_graph/6,
              
              % terminus_bootstrap.pl
              terminus_instance_name/1,
              terminus_schema_name/1,
              terminus_inference_name/1,
              layer_ontology/1,
              repository_ontology/1,
              ref_ontology/1,

              rdf_type_uri/1,
              xsd_string_type_uri/1,
              xsd_any_uri_type_uri/1,
              label_prop_uri/1,

              database_class_uri/1,
              database_name_property_uri/1,
              database_state_prop_uri/1,
              finalized_element_uri/1,
              deleting_element_uri/1,

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

              repo_name_uri/4,

              % triplestore
              destroy_graph/2,
              safe_create_named_graph/3,
              safe_open_named_graph/3,
              xrdf/4,
              xrdf_db/4,
              xrdf_deleted/4,
              xrdf_added/4,
              insert/4,
              delete/4,
              update/5,
              storage/1,
              triple_store/1,
              global_triple_store/1,
              local_triple_store/1,
              retract_local_triple_store/1,
              default_triple_store/1,
              with_triple_store/2,

              % turtle_utils.pl
              graph_to_turtle/3,

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
:- use_module(triple/terminus_bootstrap).
:- use_module(triple/triplestore).
:- use_module(triple/turtle_utils).
:- use_module(triple/upgrade_db).
