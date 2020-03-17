:- module(account, [user_key_auth/4,
                    user_key_user_id/4,
                    username_auth/3,
                    get_user/3,
                    auth_action_scope/4,
                    add_database_resource/3,
                    delete_database_resource/1,
                    write_cors_headers/2,
                    check_capabilities/2
                   ]).
:- use_module(account/capabilities).
