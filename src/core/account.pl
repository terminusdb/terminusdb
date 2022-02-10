:- module(account, [
              % capabilities.pl
              username_user_id/3,
              user_key_user_id/4,
              username_auth/3,
              get_user/3,
              auth_action_scope/4,
              assert_auth_action_scope/4,
              assert_read_access/1,
              assert_read_access/2,
              assert_read_access/3,
              assert_read_access/4,
              assert_write_access/1,
              assert_write_access/2,
              assert_write_access/3,
              assert_write_access/4,
              authorisation_object/3,
              user_accessible_database/3,
              check_descriptor_auth/4,
              is_super_user/1,
              is_super_user/2,

              % user_management.pl
              add_user/3,
              add_user/4,
              delete_user/1,
              delete_user/2,
              delete_user_transaction/3,
              delete_organization/1,
              delete_organization/2,
              delete_organization_transaction/3,
              update_user/2,
              update_user/3,
              update_user_transaction/4,
              add_organization/2,
              add_organization/3,
              add_organization_transaction/3,
              add_user_organization_transaction/4

          ]).

:- use_module(account/capabilities).
:- use_module(account/user_management).
