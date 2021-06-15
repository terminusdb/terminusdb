:- module(api_prefixes, [get_prefixes/4]).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(triple)).
:- use_module(core(account)).
:- use_module(library(terminus_store)).

get_prefixes(Path, System_DB, Auth, JSON) :-

    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_absolute_path(Path),_)),

    check_descriptor_auth(System_DB, Descriptor, system:instance_read_access, Auth),

    collection_descriptor_prefixes(Descriptor, Prefixes),

    JSON = _{'@context' : Prefixes}.

:- begin_tests(api_prefixes).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).

test(get_prefixes,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State)),
      fixme(document_refactor)]
    ) :-

    Path = "admin/testdb",

    super_user_authority(Auth),
    get_prefixes(Path,system_descriptor{},Auth,Prefixes),
    Result = (Prefixes.'@context'),
    _{doc:'http://somewhere.for.now/document/',
      scm:'http://somewhere.for.now/schema#'} :< Result.

:- end_tests(api_prefixes).
