:- module(document,
          [
              % validation.pl
              refute_validation_objects/2,

              % json.pl
              json_elaborate/3,
              json_triple/4,
              json_schema_triple/3,
              json_schema_elaborate/2,
              context_triple/2,
              database_context/2,
              create_graph_from_json/5,
              write_json_stream_to_builder/3,
              write_json_stream_to_schema/2,
              write_json_stream_to_instance/2,
              write_json_string_to_schema/2,
              write_json_string_to_instance/2,

              get_document/3,
              delete_document/2,
              insert_document/3,
              update_document/3

          ]).

:- use_module('document/validation').
:- use_module('document/json').
:- use_module('document/schema').
:- use_module('document/instance').
