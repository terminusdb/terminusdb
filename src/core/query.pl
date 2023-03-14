:- module(query, [
              % ask.pl
              ask/2,
              ask/3,
              ask_ast/3,
              askable_prefixes/2,
              create_context/2,
              create_context/3,
              is_query_context/1,
              askable_context/4,
              askable_context/5,
              askable_settings_context/3,
              context_to_parent_transaction/2,
              context_extend_prefixes/3,
              context_default_prefixes/2,
              query_default_collection/2,
              query_default_write_graph/2,
              query_default_schema_write_graph/2,

              % global_prefixes.pl
              global_prefixes/2,
              global_prefix_expand/2,
              global_prefix_expand_safe/2,
              literal_expand/2,
              default_prefixes/1,
              prefix_list/2,


              % jsonld.pl
              expand/2,
              expand/3,
              expand_context/2,
              prefix_expand/3,
              compress/3,
              term_jsonld/2,
              term_jsonld/3,
              value_jsonld/2,
%              jsonld_triples/3,
              jsonld_id/2,
              get_key_document/4,
              compress_dict_uri/3,
              has_at/1,
              compress_uri/4,
              json_dict_create/2,

              % json_woql.pl
              woql_context/1,
              initialise_woql_contexts/0,
              json_woql/2,
              json_woql_path_element_error_message/4,
              json_value_cast_type/3,

              % query_response.pl
              run_context_ast_jsonld_response/5,
              pretty_print_query_response/3,

              % resolve_query_resource.pl
              resolve_string_descriptor/3,
              resolve_absolute_descriptor/2,
              resolve_relative_descriptor/3,
              resolve_absolute_string_descriptor/2,
              resolve_relative_string_descriptor/3,
              resolve_absolute_graph_descriptor/2,
              resolve_absolute_string_graph_descriptor/2,
              resolve_absolute_string_descriptor_and_graph/3,
              resolve_absolute_string_descriptor_and_default_graph/3,
              resolve_absolute_or_relative_string_descriptor/3,
              resolve_filter/2,
	      descriptor_organization/2,

              % woql_compile.pl
              lookup/3,
              lookup_backwards/3,
              compile_query/3,
              compile_query/4,
              empty_context/1,
              empty_context/2,
              filter_transaction_object_read_write_objects/3,
              not_literal/1,

              % path.pl
              compile_pattern/4,
              calculate_path_solutions/6,

              % metadata.pl
              transaction_object_size/2,
              transaction_object_triple_count/2,
              read_object_size/2,
              read_object_triple_count/2,

              % constraints.pl
              check_constraint_document/3
          ]).

:- use_module(query/expansions).
:- use_module(query/jsonld).
:- use_module(query/json_woql).
:- use_module(query/ask).
:- use_module(query/woql_compile).
:- use_module(query/resolve_query_resource).
:- use_module(query/global_prefixes).
:- use_module(query/query_response).
:- use_module(query/path).
:- use_module(query/metadata).
:- use_module(query/constraints).
