:- module(transaction, [
              % database.pl
              query_context_transaction_objects/2,
              run_transaction/1,
              run_transactions/1,

              % descriptor.pl
              open_descriptor/2,
              open_descriptor/3,
              open_descriptor/5,
              collection_descriptor_transaction_object/3,
              graph_descriptor_transaction_objects_read_write_object/3,
              instance_graph_descriptor_transaction_object/3,
              read_write_obj_reader/2,
              read_write_obj_builder/2,
              filter_read_write_objects/3,
              make_branch_descriptor/5,
              make_branch_descriptor/4,
              make_branch_descriptor/3,
              read_write_object_to_name/2,

              % validate.pl
              transaction_objects_to_validation_objects/2,
              commit_validation_objects/1
          ]).

:- use_module(transaction/database).
:- use_module(transaction/descriptor).
:- use_module(transaction/validate).
