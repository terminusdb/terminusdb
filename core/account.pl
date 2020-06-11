:- module(account, [
              % capabilities.pl
              user_key_auth/4,
              username_user_id/3,
              user_key_user_id/4,
              username_auth/3,
              user_id_auth_id/3,
              get_user/3,
              auth_action_scope/4,
              assert_auth_action_scope/4,
              assert_read_access/1,
              assert_read_access/2,
              assert_read_access/3,
              assert_write_access/1,
              assert_write_access/2,
              assert_write_access/3,
              write_cors_headers/2,
              authorisation_object/3,
              user_object/3,
              super_user_authority/1,
              check_descriptor_auth/4,

              % user_management.pl
              add_user/4,
              agent_name_uri/3,
              agent_name_exists/2,
              make_user_own_database/2,
              delete_user/1
          ]).

:- use_module(account/capabilities).
:- use_module(account/user_management).
