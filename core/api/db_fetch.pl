:- module(db_fetch, [
              remote_fetch/3
          ]).


:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(db_pack).

:- meta_predicate remote_fetch(+, 3, -).
remote_fetch(Repository_Descriptor, Fetch_Predicate, New_Head_Layer_Id) :-
    do_or_die(
        (repository_descriptor{} :< Repository_Descriptor),
        error(fetch_requires_repository(Repository_Descriptor))),

    Database_Descriptor = (Repository_Descriptor.database_descriptor),
    do_or_die(
        repository_remote_url(Database_Descriptor, Repository_Descriptor.repository_name, URL),
        error(fetch_remote_has_no_url(Repository_Descriptor))),

    create_context(Database_Descriptor, Database_Context),
    (   repository_head(Database_Context,
                        (Repository_Descriptor.repository_name),
                        Repository_Head_Layer_Id)
    ->  Repository_Head_Option = some(Repository_Head_Layer_Id)
    ;   Repository_Head_Option = none),

    call(Fetch_Predicate, URL, Repository_Head_Option, Payload),

    payload_repository_head_and_pack(Payload, Head, Pack),
    New_Head_Layer_Id = Head,
    unpack(Pack),

    create_context(Database_Descriptor, Database_Context2),

    with_transaction(
        Database_Context2,
        (   do_or_die(
                (   var(Repository_Head_Layer_Id)
                ->  true
                ;   repository_head(Database_Context2, (Repository_Descriptor.repository_name), Repository_Head_Layer_Id)),
                error(repository_head_moved(
                          Repository_Descriptor,
                          Repository_Head_Layer_Id))),

            update_repository_head(Database_Context2,
                                   (Repository_Descriptor.repository_name),
                                   Head)
        ),_Meta_Data).

:- begin_tests(fetch_api).
:- use_module(core(util/test_utils)).
:- use_module(core(util)).
:- use_module(core(triple)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(db_graph).
:- use_module(db_pack).

get_pack_from_store(Store, URL, Repository_Head_Option, Payload) :-
    pattern_string_split('/pack/', URL, [_, Database_String]),
    string_concat(Database_String, "/local/_commits", Repository_String),
    resolve_absolute_string_descriptor(Repository_String, Repository_Descriptor),

    with_triple_store(Store,
                      (   create_context(Repository_Descriptor, Repository_Context),
                          repository_context__previous_head_option__payload(
                              Repository_Context,
                              Repository_Head_Option,
                              Payload))).

test(fetch_something,
     [setup((setup_temp_store(State),
             user_database_name(admin,"test", Name),
             (   database_exists(Name)
             ->  delete_db(Name)
             ;   true),
             create_db_without_schema(Name, 'test','a test'),
             resolve_absolute_string_descriptor('admin/test',Branch_Descriptor)
             )),
      cleanup(teardown_temp_store(State))
     ]
    ) :-
    create_context(Branch_Descriptor,commit_info{author: "tester", message: "testing"}, Branch_Context),
    with_transaction(
        Branch_Context,
        ask(Branch_Context,
            insert(a,b,c)),
        _),

    triple_store(Old_Store),

    with_temp_store(
        (
            user_database_name(admin, "test_local", Name2),
            create_db_without_schema(Name2, 'test local','a test'),
            resolve_absolute_string_descriptor(
                "admin/test_local/_meta", Database_Descriptor),
            create_context(Database_Descriptor, Database_Context),

            % Wrong URI
            Remote_URL = "http://fake_destination/pack/admin/test",
            with_transaction(
                Database_Context,
                insert_remote_repository(Database_Context,
                                         "terminus_remote",
                                         Remote_URL,
                                         _Head,
                                         _Repo_URI
                                        ),
                _),

            resolve_absolute_string_descriptor("admin/test_local/terminus_remote/_commits", Remote_Repository_Descriptor),

            remote_fetch(Remote_Repository_Descriptor,
                         get_pack_from_store(Old_Store),
                         _New_Head_Layer_Id),
            % todo also check head layer id is correct

            resolve_absolute_string_descriptor("admin/test_local/terminus_remote/branch/master", Remote_Master_Descriptor),
            open_descriptor(Remote_Master_Descriptor, Remote_Master_Transaction),
            once(ask(Remote_Master_Transaction,
                     t(a,b,c)))
        )),
    
    true.
:- end_tests(fetch_api).
