:- module(plugin_api_test_utils, [
    setup_temp_store/1,
    teardown_temp_store/1,
    create_db_without_schema/2,
    create_db_with_empty_schema/2,
    create_db_with_test_schema/2
]).

:- reexport(core(util/test_utils),
           [setup_temp_store/1, teardown_temp_store/1,
            create_db_without_schema/2, create_db_with_empty_schema/2,
            create_db_with_test_schema/2]).
