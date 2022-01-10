:- module(api_prefixes, [get_prefixes/4, update_prefixes/5]).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(triple)).
:- use_module(core(account)).
:- use_module(library(terminus_store)).
:- use_module(core(document)).

:- use_module(library(plunit)).

get_prefixes(Path, System_DB, Auth, JSON) :-

    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_absolute_path(Path),_)),

    check_descriptor_auth(System_DB, Descriptor, '@schema':'Action/instance_read_access', Auth),

    database_prefixes(Descriptor,JSON).

update_prefixes(Path, System_DB, Auth, Commit_Info, Document) :-

    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_absolute_path(Path),_)),

    check_descriptor_auth(System_DB, Descriptor, '@schema':'Action/instance_write_access', Auth),

    create_context(Descriptor, Commit_Info, Context),
    with_transaction(
        Context,
        insert_context_document(Context, Document),
        _).

:- begin_tests(api_prefixes).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).

test(get_prefixes,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State))]
    ) :-

    Path = "admin/testdb",

    super_user_authority(Auth),
    get_prefixes(Path,system_descriptor{},Auth,Prefixes),

    _{'@base':"http://somewhere.for.now/document/",
      '@schema':"http://somewhere.for.now/schema#",
      '@type':'Context'} :< Prefixes.


test(get_prefixes_auth_failure,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State)),
      error(access_not_authorised('terminusdb://system/data/User/Doug',
                                  '@schema':'Action/instance_read_access',
                                  _),
            _)
     ]) :-

    add_user("Doug", some("password"), User_URI),
    Path = "admin/testdb",
    get_prefixes(Path,system_descriptor{},User_URI,Prefixes),

    writeq(Prefixes).

test(update_prefixes_auth_failure,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State)),
      error(access_not_authorised('terminusdb://system/data/User/Doug',
                                  '@schema':'Action/instance_write_access',
                                  _),
            _)
     ]) :-

    add_user("Doug", some("password"), User_URI),
    Path = "admin/testdb",
    Commit_Info = commit_info{author: "me", message: "prefix it"},
    update_prefixes(Path, system_descriptor{}, User_URI, Commit_Info, _{}).

:- end_tests(api_prefixes).
