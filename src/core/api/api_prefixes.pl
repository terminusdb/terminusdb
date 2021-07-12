:- module(api_prefixes, [get_prefixes/4, update_prefixes/5]).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(triple)).
:- use_module(core(account)).
:- use_module(library(terminus_store)).
:- use_module(core(document)).

get_prefixes(Path, System_DB, Auth, JSON) :-

    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_absolute_path(Path),_)),

    check_descriptor_auth(System_DB, Descriptor, system:instance_read_access, Auth),

    database_context(Descriptor,JSON).

update_prefixes(Path, System_DB, Auth, Commit_Info, Document) :-

    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_absolute_path(Path),_)),

    check_descriptor_auth(System_DB, Descriptor, system:instance_read_access, Auth),

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

:- end_tests(api_prefixes).
