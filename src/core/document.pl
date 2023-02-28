:- module(document,
          [
              % validation.pl
              refute_validation_objects/2,

              % json.pl
              idgen_random/2,
              idgen_hash/3,
              idgen_lexical/3,

              json_elaborate/3,
              json_schema_triple/3,
              json_schema_elaborate/3,
              context_triple/2,
              database_prefixes/2,
              database_schema_prefixes/2,
              database_and_default_prefixes/2,
              run_insert_document/4,
              create_graph_from_json/5,
              write_json_stream_to_builder/3,
              write_json_stream_to_schema/2,
              write_json_stream_to_instance/2,
              write_json_string_to_schema/2,
              write_json_string_to_instance/2,
              replace_json_schema/2,

              database_context_object/2,
              database_schema_context_object/2,
              get_document/3,
              get_document/4,
              get_document/5,
              get_document_uri/3,
              get_schema_document/3,
              get_schema_document_uri/2,
              get_document_uri_by_type/3,
              get_schema_document_uri_by_type/3,
              delete_document/2,
              insert_document/3,
              insert_document/7,
              insert_document_unsafe/8,
              replace_document/2,
              replace_document/3,
              replace_document/5,
              replace_document/8,
              nuke_documents/1,
              insert_schema_document/2,
              insert_schema_document_unsafe/3,
              delete_schema_document/2,
              replace_schema_document/2,
              replace_schema_document/3,
              replace_schema_document/4,
              nuke_schema_documents/1,
              json_read_required_context/3,
              insert_context_document/2,
              replace_context_document/2,
              schema_document_exists/2,
              document_exists/2,
              prefix_expand_schema/3,

              % instance.pl
              is_instance/3,

              % schema.pl
              class_subsumed/3,
              class_frame/3,
              class_frame/4,
              all_class_frames/2,
              all_class_frames/3,
              is_schemaless/1,

              % query.pl
              match_query_document_uri/4,
              match_expanded_query_document_uri/4,
              expand_query_document/5,

              % diff.pl
              simple_diff/4,

              % patch.pl
              simple_patch/4,
              patch_cost/2,

              % normalize.pl
              normalize_document/3,

              % apply.pl
              apply_diff/4,

              % json_rdf.pl
              is_json_document_type/1,
              is_json_subdocument_type/1,
              json_document_triple/3,
              json_subdocument_triple/4,
              assign_json_document_id/2,
              get_json_object/3
          ]).

:- use_module('document/validation').
:- use_module('document/json').
:- use_module('document/schema').
:- use_module('document/instance').
:- use_module('document/query').
:- use_module('document/patch').
:- use_module('document/diff').
:- use_module('document/normalize').
:- use_module('document/apply').
:- use_module('document/inference').
:- use_module('document/json_rdf').
