:- module(db_fetch, [
              remote_fetch/6,
              authorized_fetch/4
          ]).


:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(account)).
:- use_module(core(transaction)).
:- use_module(db_pack).
:- use_module(library(ssl)).
:- use_module(library(http/http_client)).
:- use_module(library(plunit)).
:- use_module(library(lists)).

:- meta_predicate remote_fetch(+, +, +, 3, -, -).
remote_fetch(System_DB, Auth, Path, Fetch_Predicate, New_Head_Layer_Id, Head_Has_Updated) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Repository_Descriptor),
        error(invalid_absolute_path(Path),_)),

    do_or_die(
        (repository_descriptor{} :< Repository_Descriptor),
        error(fetch_requires_repository(Repository_Descriptor),_)),

    check_descriptor_auth(System_DB, Repository_Descriptor, '@schema':'Action/fetch', Auth),

    Database_Descriptor = (Repository_Descriptor.database_descriptor),

    do_or_die(
        create_context(Database_Descriptor, Database_Context),
        error(unresolvable_collection(Database_Descriptor),_)),

    do_or_die(
        repository_remote_url(Database_Descriptor, Repository_Descriptor.repository_name, URL),
        error(fetch_remote_has_no_url(Repository_Descriptor), _)),

    with_transaction(
        Database_Context,
        (
            (   repository_head(Database_Context,
                                (Repository_Descriptor.repository_name),
                                Repository_Head_Layer_Id)
            ->  Repository_Head_Option = some(Repository_Head_Layer_Id)
            ;   Repository_Head_Option = none),

            call(Fetch_Predicate, URL, Repository_Head_Option, Payload_Option),
            (   some(Payload) = Payload_Option
            ->  payload_repository_head_and_pack(Payload, Head, Pack),
                New_Head_Layer_Id = Head,
                unpack(Pack),

                update_repository_head(Database_Context,
                                       (Repository_Descriptor.repository_name),
                                       Head),
                Head_Has_Updated = true
            ;   Repository_Head_Option = some(New_Head_Layer_Id)
            ->  Head_Has_Updated = false
            ;   throw(error(unexpected_pack_missing(Repository_Descriptor),_)))),
        _Meta_Data).

remote_pack_url(URL, Pack_URL) :-
    pattern_string_split('/', URL, Parts),
    do_or_die(append(Pre, [Organization,Database], Parts),
              error(db_url_malformatted(URL), _)),
    append(Pre, ["api", "pack", Organization, Database], All_Parts),
    merge_separator_split(Pack_URL,'/',All_Parts).

authorized_fetch(Authorization, URL, Repository_Head_Option, Payload_Option) :-
    (   some(Repository_Head) = Repository_Head_Option
    ->  Document = _{ repository_head: Repository_Head }
    ;   Document = _{}),

    remote_pack_url(URL,Pack_URL),

    http_post(Pack_URL,
              json(Document),
              Payload,
              [request_header('Authorization'=Authorization),
               json_object(dict),
               status_code(Status)
              ]),

    (   Status = 200
    ->  Payload_Option = some(Payload)
    ;   Status = 204
    ->  Payload_Option = none
    ;   throw(error(remote_connection_error(Payload),_))).

:- begin_tests(fetch_api).
:- use_module(core(util/test_utils)).
:- use_module(core(util)).
:- use_module(core(triple)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(db_pack).

get_pack_from_store(Store, URL, Repository_Head_Option, Payload_Option) :-
    pattern_string_split('/pack/', URL, [_, Database_String]),
    string_concat(Database_String, "/local/_commits", Repository_String),
    resolve_absolute_string_descriptor(Repository_String, Repository_Descriptor),
    super_user_authority(Auth),
    with_triple_store(Store,
                      (   askable_context(Repository_Descriptor, system_descriptor{}, Auth, Repository_Context),
                          pack_from_context(
                              Repository_Context,
                              Repository_Head_Option,
                              Payload_Option))).

test(fetch_something,
     [setup((setup_temp_store(State),
             (   database_exists(admin,test)
             ->  force_delete_db(admin,test)
             ;   true),
             create_db_without_schema(admin,test),
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

    % look up local repository layer id for comparison later
    resolve_absolute_string_descriptor("admin/test/_meta", Local_Database_Descriptor),
    repository_head(Local_Database_Descriptor, "local", Local_Repository_Layer_Id),

    triple_store(Old_Store),

    with_temp_store(
        (
            create_db_without_schema(admin,test_local),
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
            Remote_Repository = "admin/test_local/terminus_remote/_commits",
            super_user_authority(Auth),
            remote_fetch(system_descriptor{}, Auth,
                         Remote_Repository,
                         get_pack_from_store(Old_Store),
                         Remote_Repository_Layer_Id,
                         Head_Has_Updated),
            Local_Repository_Layer_Id = Remote_Repository_Layer_Id,
            Head_Has_Updated = true,

            resolve_absolute_string_descriptor("admin/test_local/terminus_remote/branch/main", Remote_Master_Descriptor),
            open_descriptor(Remote_Master_Descriptor, Remote_Master_Transaction),
            once(ask(Remote_Master_Transaction,
                     t(a,b,c)))
        )),

    true.
:- end_tests(fetch_api).
