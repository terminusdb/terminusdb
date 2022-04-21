:- module(db_push, [
              push/8,
              authorized_push/3
          ]).

:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(library(terminus_store)).
:- use_module(core(transaction)).
:- use_module(db_pack).
:- use_module(db_rebase).
:- use_module(db_create).
:- use_module(core(account)).
:- use_module(library(http/http_client)).
:- use_module(core(document)).

:- use_module(library(tus)).
:- use_module(library(plunit)).

% error conditions:
% - branch to push does not exist
% - repository does not exist
% - we tried to push to a repository that is not a remote
% - tried to push without having fetched first. The repository exists as an entity in our metadata graph but it hasn't got an associated commit graph. We always need one.
% - remote diverged - someone else committed and pushed and we know about that
% - We try to push an empty branch, but we know that remote is non-empty
% - remote returns an error
% -- history diverged (we check locally, but there's a race)
% -- remote doesn't know what we're talking about
% -- remote authorization failed
% - communication error while talking to the remote

:- meta_predicate push(+, +, +, +, +, +, 2, -).
push(System_DB, Auth, Branch, Remote_Name, Remote_Branch, _Options,
     Push_Predicate, Result) :-

    do_or_die(
        resolve_absolute_string_descriptor(Branch, Branch_Descriptor),
        error(invalid_absolute_path(Branch),_)),

    do_or_die(
        (branch_descriptor{} :< Branch_Descriptor),
        error(push_requires_branch(Branch_Descriptor),_)),

    check_descriptor_auth(System_DB, (Branch_Descriptor.repository_descriptor), '@schema':'Action/push', Auth),

    do_or_die(
        open_descriptor(Branch_Descriptor, _Branch_Transaction), % dodgy underscore
        error(unresolvable_absolute_descriptor(Branch_Descriptor),_)),

    Repository_Descriptor = (Branch_Descriptor.repository_descriptor),
    Database_Descriptor = (Repository_Descriptor.database_descriptor),

    do_or_die(
        repository_type(Database_Descriptor, Remote_Name, Type),
        error(no_repository_with_name(Database_Descriptor,Remote_Name),_)),

    do_or_die(
        Type = remote,
        error(push_attempted_on_non_remote(Database_Descriptor,Remote_Name),_)),

    repository_remote_url(Database_Descriptor, Remote_Name, Remote_URL),

    % Begin hypothetical rebase for pack
    askable_context(Repository_Descriptor, System_DB, Auth, Repository_Context_With_Prefixes),
    context_default_prefixes(Repository_Context_With_Prefixes, Repository_Context),

    resolve_relative_descriptor(Database_Descriptor,[Remote_Name, "_commits"],
                                Remote_Repository),

    create_context(Remote_Repository, Remote_Repository_Context_With_Prefixes),
    context_default_prefixes(Remote_Repository_Context_With_Prefixes,
                             Remote_Repository_Context),
    context_to_parent_transaction(Remote_Repository_Context, Database_Transaction),
    do_or_die(repository_head(Database_Transaction, Remote_Name, Last_Head_Id),
              error(push_has_no_repository_head(Remote_Repository), _)),
    (   branch_head_commit(Repository_Context,
                           (Branch_Descriptor.branch_name),
                           Local_Commit_Uri)
    ->  (   has_branch(Remote_Repository_Context,
                       Remote_Branch)
        ->  (   branch_head_commit(Remote_Repository_Context,
                                   Remote_Branch,
                                   Remote_Commit_Uri),
                \+ commit_uri_is_initial(Remote_Repository_Context, Remote_Commit_Uri)
            ->  Remote_Commit_Uri_Option = some(Remote_Commit_Uri)
            ;   Remote_Commit_Uri_Option = none
            ),
            branch_name_uri(Remote_Repository_Context, Remote_Branch, Remote_Branch_Uri)
        ;   insert_branch_object(Remote_Repository_Context_With_Prefixes, Remote_Branch, Remote_Branch_Uri),
            Remote_Commit_Uri_Option = none
        ),

        commit_id_uri(Repository_Context, Local_Commit_Id, Local_Commit_Uri),
        (   Remote_Commit_Uri_Option = some(Remote_Commit_Uri),
            commit_id_uri(Remote_Repository_Context, Remote_Commit_Id, Remote_Commit_Uri),
            most_recent_common_ancestor(Repository_Context, Remote_Repository_Context, Local_Commit_Id,
                                        Remote_Commit_Id, _Common_Commit_Id, _Local_Branch_Path, Remote_Branch_Path)
        % Shared history
        ->  do_or_die(Remote_Branch_Path = [],
                      error(remote_diverged(Remote_Repository,Remote_Branch_Path),_))
        % No commit yet on remote
        ;   Remote_Commit_Uri_Option = none
        ->  true
        % No shared history
        ;   throw(
                error(no_common_history(Remote_Repository),_))),

        (   (   Remote_Commit_Uri_Option = none
            ;   Remote_Commit_Id \= Local_Commit_Id)
        ->  copy_commits(Repository_Context, Remote_Repository_Context, Local_Commit_Id),
            reset_branch_head(Remote_Repository_Context_With_Prefixes, Remote_Branch_Uri, Local_Commit_Uri)
        ;   true)
    ;   (   has_branch(Remote_Repository_Context,
                       Remote_Branch),
            branch_head_commit(Remote_Repository_Context,
                               Remote_Branch, _)
        ->  throw(error(remote_not_empty_on_local_empty(Remote_Repository),_))
        ;   insert_branch_object(Remote_Repository_Context_With_Prefixes, Remote_Branch, _)
        )
    ),

    cycle_context(Remote_Repository_Context, Final_Context, Remote_Transaction_Object, _),
    pack_from_context(Final_Context, some(Last_Head_Id), Payload_Option),


    (   Payload_Option = none % Last_Head_Id = Current_Head_Id
    ->  Result = same(Last_Head_Id)
    ;   Payload_Option = some(Payload),
        catch(call(Push_Predicate, Remote_URL, Payload),
              E,
              (   E = error(Inner_E,_),
                  (   Inner_E = history_diverged
                  ;   Inner_E = remote_unknown
                  ;   Inner_E = authorization_failure(_)
                  ;   Inner_E = communication_failure(_))
              ->  throw(error(remote_unpack_failed(Inner_E),_))
              ;   throw(error(remote_unpack_unexpected_failure(E),_)))),
        Database_Transaction_Object = (Remote_Transaction_Object.parent),
        [Read_Obj] = (Remote_Transaction_Object.instance_objects),
        Layer = (Read_Obj.read),
        layer_to_id(Layer, Current_Head_Id),
        update_repository_head(Database_Transaction_Object, Remote_Name, Current_Head_Id),
        run_transactions([Database_Transaction_Object], true, _),
        Result = new(Current_Head_Id)
    ).

remote_unpack_url(URL, Pack_URL) :-
    pattern_string_split('/', URL, Parts),
    do_or_die(append(Pre, [Organization,Database], Parts),
              error(db_url_malformatted(URL), _)),
    append(Pre, ["api", "unpack", Organization, Database], All_Parts),
    merge_separator_split(Pack_URL,'/',All_Parts).

remote_tus_url(URL, TUS_URL) :-
    pattern_string_split('/', URL, [Protocol,Blank,Server|_Rest]),
    merge_separator_split(TUS_URL,'/',[Protocol,Blank,Server,"api","files"]).

% NOTE: What do we do with the remote branch? How do we send it?
authorized_push(Authorization, Remote_URL, Payload) :-
    catch(
        (   % Try TUS protocol (we could check resulting options too for create etc...)
            remote_tus_url(Remote_URL, TUS_URL),
            tus_options(TUS_URL, _TUS_Options, [request_header('Authorization'=Authorization)]),
            Using_TUS = true,

            setup_call_cleanup(
                tmp_file_stream(Tmp_File, Stream, [encoding(octet)]),
                format(Stream, "~s", Payload),
                close(Stream)
            ),

            tus_upload(Tmp_File, TUS_URL, Resource_URL, [request_header('Authorization'=Authorization)]),
            Data = json(_{resource_uri : Resource_URL})
        ),
        error(existence_error(url,_),_),
        % TUS failed, fall back to old style
        (   Data = bytes('application/octets',Payload),
            Using_TUS = false
        )
    ),

    remote_unpack_url(Remote_URL, Unpack_URL),
    catch(http_post(Unpack_URL,
                    Data,
                    Result,
                    [request_header('Authorization'=Authorization),
                     json_object(dict),
                     timeout(infinite),
                     status_code(Status_Code)
                    ]),
          E,
          throw(error(communication_failure(E),_))),

    (   200 = Status_Code
    ->  (   Using_TUS = true
        ->  tus_delete(Resource_URL, [tus_extension([termination])],
                       % assume extension to avoid pointless pre-flight
                       [request_header('Authorization'=Authorization)])
        ;   true)
    ;   400 = Status_Code,
        _{'@type': "api:UnpackErrorResponse", 'api:error' : Error} :< Result
    ->  (   _{'@type' : "api:NotALinearHistory"} :< Error
        ->  throw(error(history_diverged,_))
        ;   _{'@type' : "api:UnpackDestinationDatabaseNotFound"} :< Error
        ->  throw(error(remote_unknown,_))
        ;   throw(error(unknown_status_code(Status_Code, Result),_))
        )
    ;   403 = Status_Code
    ->  throw(error(remote_authorization_failure(Result),_))
    ;   throw(error(unknown_status_code,_))
    ).

:- begin_tests(push, [concurrent(true)]).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).
:- use_module(core(account)).

%:- use_module(db_create).
:- use_module(db_branch).

test_pusher(Expected, _Remote_URL, Payload) :-
    payload_repository_head_and_pack(Payload, _Head, Pack),
    pack_layerids_and_parents(Pack, Layer_Parents),
    do_or_die(
        memberchk(Expected-_,Layer_Parents),
        error(layer_not_found(Expected))).

test(push_on_empty,
     [setup((setup_temp_store(State),
             create_db_without_schema(admin,foo))),
      cleanup(teardown_temp_store(State))
     ])
:-
    resolve_absolute_string_descriptor("admin/foo", Descriptor),

    Database_Descriptor = (Descriptor.repository_descriptor.database_descriptor),

    resolve_relative_string_descriptor(Database_Descriptor, "remote/_commits", Repository_Descriptor),


    create_context(Database_Descriptor, Database_Context),
    with_transaction(Database_Context,
                     insert_remote_repository(Database_Context, "remote", "http://somewhere", _),
                     _),
    create_ref_layer(Repository_Descriptor),

    create_context(Descriptor, commit_info{author: "me",
                                           message: "something"},
                   Branch_Context),

    with_transaction(
        Branch_Context,
        ask(Branch_Context,
            (   insert(a,b,c),
                insert(c,d,e))),
        _Meta_Data_B),

    super_user_authority(Auth),

    once(ask(Descriptor.repository_descriptor,
                        t(_,layer:identifier, Expected_Layer_Id^^(xsd:string)))),

    push(system_descriptor{}, Auth, "admin/foo", "remote", "main", [], test_pusher(Expected_Layer_Id), _Result),

    resolve_absolute_string_descriptor("admin/foo/remote/branch/main", Remote_Branch),
    findall(X-Y-Z, ask(Remote_Branch, t(X,Y,Z)), Triples),
    sort(Triples, [a-b-c,c-d-e]).


test(push_twice,
     [setup((setup_temp_store(State),
             create_db_without_schema(admin,foo))),
      cleanup(teardown_temp_store(State))])
:-
    resolve_absolute_string_descriptor("admin/foo", Descriptor),

    Database_Descriptor = (Descriptor.repository_descriptor.database_descriptor),

    resolve_relative_string_descriptor(Database_Descriptor, "remote/_commits", Repository_Descriptor),


    create_context(Database_Descriptor, Database_Context),
    with_transaction(Database_Context,
                     insert_remote_repository(Database_Context, "remote", "http://somewhere", _),
                     _),
    create_ref_layer(Repository_Descriptor),

    create_context(Descriptor, commit_info{author: "me",
                                           message: "something"},
                   Branch_Context),

    with_transaction(
        Branch_Context,
        ask(Branch_Context,
            (   insert(a,b,c),
                insert(c,d,e))),
        _Meta_Data_B),

    super_user_authority(Auth),

    Repo = (Descriptor.repository_descriptor),

    once(ask(Repo,
             t(_,layer:identifier, Expected_Layer_Id^^(xsd:string)))),


    push(system_descriptor{}, Auth, "admin/foo", "remote", "main", [], test_pusher(Expected_Layer_Id), _Result),

    create_context(Descriptor, commit_info{author: "me",
                                           message: "something else"},
                   Branch_Context_2),

    with_transaction(
        Branch_Context_2,
        ask(Branch_Context_2,
            (   insert(h,i,j),
                insert(k,l,m))),
        _Meta_Data_B_2),


    push(system_descriptor{}, Auth, "admin/foo", "remote", "main", [], test_pusher(_Expected_Layer_Id_2), _Result_2),

    resolve_absolute_string_descriptor("admin/foo/remote/branch/main", Remote_Branch),
    findall(X-Y-Z, ask(Remote_Branch, t(X,Y,Z)), Triples),

    sort(Triples, [a-b-c,c-d-e,h-i-j,k-l-m]).

test(push_twice_with_second_push_changing_nothing,
     [setup((setup_temp_store(State),
             create_db_without_schema(admin,foo))),
      cleanup(teardown_temp_store(State))])
:-
    resolve_absolute_string_descriptor("admin/foo", Descriptor),

    Database_Descriptor = (Descriptor.repository_descriptor.database_descriptor),

    resolve_relative_string_descriptor(Database_Descriptor, "remote/_commits", Repository_Descriptor),


    create_context(Database_Descriptor, Database_Context),
    with_transaction(Database_Context,
                     insert_remote_repository(Database_Context, "remote", "http://somewhere", _),
                     _),
    create_ref_layer(Repository_Descriptor),

    create_context(Descriptor, commit_info{author: "me",
                                           message: "something"},
                   Branch_Context),

    with_transaction(
        Branch_Context,
        ask(Branch_Context,
            (   insert(a,b,c),
                insert(c,d,e))),
        _Meta_Data_B),

    super_user_authority(Auth),

    once(ask(Descriptor.repository_descriptor,
                        t(_,layer:identifier, Expected_Layer_Id^^(xsd:string)))),

    push(system_descriptor{}, Auth, "admin/foo", "remote", "main", [], test_pusher(Expected_Layer_Id), Result),
    Result = new(Head),

    push(system_descriptor{}, Auth, "admin/foo", "remote", "main", [], test_pusher(_Expected_Layer_Id_2), Result_2),

    Result_2 = same(Head).

test(push_empty_branch,
     [setup((setup_temp_store(State),
             create_db_without_schema(admin,foo))),
      cleanup(teardown_temp_store(State))])
:-
    resolve_absolute_string_descriptor("admin/foo", Descriptor),

    Database_Descriptor = (Descriptor.repository_descriptor.database_descriptor),

    resolve_relative_string_descriptor(Database_Descriptor, "remote/_commits", Remote_Repository_Descriptor),


    create_context(Database_Descriptor, Database_Context),
    with_transaction(Database_Context,
                     insert_remote_repository(Database_Context, "remote", "http://somewhere", _),
                     _),
    create_ref_layer(Remote_Repository_Descriptor),

    super_user_authority(Auth),
    push(system_descriptor{}, Auth, "admin/foo", "remote", "main", [], test_pusher(_New_Layer_Id), _Result),
    has_branch(Remote_Repository_Descriptor, "main"),

    true.

test(push_new_nonmaster_branch,
     [setup((setup_temp_store(State),
             create_db_without_schema(admin,foo))),
      cleanup(teardown_temp_store(State))
     ])
:-

    Destination_Path = "admin/foo/local/branch/work",

    super_user_authority(Auth),
    branch_create(system_descriptor{}, Auth, Destination_Path, empty(_,_), _),

    resolve_absolute_string_descriptor("admin/foo", Descriptor),

    Database_Descriptor = (Descriptor.repository_descriptor.database_descriptor),

    resolve_relative_string_descriptor(Database_Descriptor, "remote/_commits", Remote_Repository_Descriptor),


    create_context(Database_Descriptor, Database_Context),
    with_transaction(Database_Context,
                     insert_remote_repository(Database_Context, "remote", "http://somewhere", _),
                     _),
    create_ref_layer(Remote_Repository_Descriptor),

    super_user_authority(Auth),
    push(system_descriptor{}, Auth, "admin/foo/local/branch/work", "remote", "work", [], test_pusher(_New_Layer_Id), _Result),
    has_branch(Remote_Repository_Descriptor, "work"),

    true.

test(push_new_nonmaster_branch_with_content,
     [setup((setup_temp_store(State),
             create_db_without_schema(admin,foo))),
      cleanup(teardown_temp_store(State))
     ])
:-

    Destination_Path = "admin/foo/local/branch/work",
    super_user_authority(Auth),
    branch_create(system_descriptor{}, Auth, Destination_Path, empty(_,_), _),
    resolve_absolute_string_descriptor(Destination_Path, Work_Branch_Descriptor),
    Prefixes = _{'@base' : 'http://somewhere_else/',
                 '@schema': 'http://somewhere/schema#',
                 foo: 'http://the_foo_place/'},
    create_schema(Work_Branch_Descriptor, [], Prefixes),

    create_context(Work_Branch_Descriptor, commit_info{author:"test", message:"test"}, Work_Branch_Context),
    with_transaction(Work_Branch_Context,
                     ask(Work_Branch_Context,
                         insert(a,b,c)),
                     _),

    resolve_absolute_string_descriptor("admin/foo", Descriptor),

    Repository_Descriptor = (Descriptor.repository_descriptor),
    Database_Descriptor = (Repository_Descriptor.database_descriptor),

    resolve_relative_string_descriptor(Database_Descriptor, "remote/_commits", Remote_Repository_Descriptor),


    create_context(Database_Descriptor, Database_Context),
    with_transaction(Database_Context,
                     insert_remote_repository(Database_Context, "remote", "http://somewhere", _),
                     _),
    create_ref_layer(Remote_Repository_Descriptor),

    super_user_authority(Auth),
    push(system_descriptor{}, Auth, Destination_Path, "remote", "work", [], test_pusher(_New_Layer_Id), _Result),
    has_branch(Remote_Repository_Descriptor, "work"),
    branch_head_commit(Repository_Descriptor, "work", Head_Commit),
    branch_head_commit(Remote_Repository_Descriptor, "work", Head_Commit),

    true.

test(push_without_branch,
     [setup((setup_temp_store(State),
             create_db_without_schema(admin,foo))),
      cleanup(teardown_temp_store(State)),
      error(branch_does_not_exist(_))
     ])
:-

    resolve_absolute_string_descriptor("admin/foo", Descriptor),

    Database_Descriptor = (Descriptor.repository_descriptor.database_descriptor),

    resolve_relative_string_descriptor(Database_Descriptor, "remote/_commits", Remote_Repository_Descriptor),

    create_context(Database_Descriptor, Database_Context),
    with_transaction(Database_Context,
                     insert_remote_repository(Database_Context, "remote", "http://somewhere", _),
                     _),

    create_ref_layer(Remote_Repository_Descriptor),

    super_user_authority(Auth),

    push(system_descriptor{}, Auth, "admin/foo/local/branch/work", "remote", "work", [], test_pusher(_New_Layer_Id), _Result).

test(push_without_repository,
     [setup((setup_temp_store(State),
             create_db_without_schema(admin,foo))),
      cleanup(teardown_temp_store(State)),
      error(no_repository_with_name(_,_))
     ])
:-
    Destination_Path = "admin/foo/local/branch/work",
    super_user_authority(Auth),
    branch_create(system_descriptor{}, Auth, Destination_Path, empty(_,_), _),

    resolve_absolute_string_descriptor("admin/foo", Descriptor),

    Database_Descriptor = (Descriptor.repository_descriptor.database_descriptor),

    resolve_relative_string_descriptor(Database_Descriptor, "remote/_commits", Remote_Repository_Descriptor),

    super_user_authority(Auth),
    push(system_descriptor{}, Auth, "admin/foo/local/branch/work", "remote", "work", [], test_pusher(_New_Layer_Id), _Result),
    has_branch(Remote_Repository_Descriptor, "work"),

    true.

test(push_local,
     [setup((setup_temp_store(State),
             create_db_without_schema(admin,foo))),
      cleanup(teardown_temp_store(State)),
      error(push_attempted_on_non_remote(_,_))
     ])
:-
    Destination_Path = "admin/foo/local/branch/work",
    super_user_authority(Auth),
    branch_create(system_descriptor{}, Auth, Destination_Path, empty(_,_), _),

    resolve_absolute_string_descriptor(Destination_Path, Work_Branch_Descriptor),
    Prefixes = _{'@base' : 'http://somewhere_else/',
                 '@schema': 'http://somewhere/schema#',
                 foo: 'http://the_foo_place/'},
    create_schema(Work_Branch_Descriptor, [], Prefixes),

    create_context(Work_Branch_Descriptor, commit_info{author:"test", message:"test"}, Work_Branch_Context),
    with_transaction(Work_Branch_Context,
                     ask(Work_Branch_Context,
                         insert(a,b,c)),
                     _),

    resolve_absolute_string_descriptor("admin/foo", Descriptor),

    Repository_Descriptor =(Descriptor.repository_descriptor),
    Database_Descriptor = (Repository_Descriptor.database_descriptor),

    resolve_relative_string_descriptor(Database_Descriptor, "remote/_commits", Remote_Repository_Descriptor),


    create_context(Database_Descriptor, Database_Context),
    with_transaction(Database_Context,
                     insert_local_repository(Database_Context, "remote", _),
                     _),
    create_ref_layer(Remote_Repository_Descriptor),

    super_user_authority(Auth),
    push(system_descriptor{}, Auth, Destination_Path, "remote", "work", [], test_pusher(_New_Layer_Id), _Result),
    has_branch(Remote_Repository_Descriptor, "work"),
    branch_head_commit(Repository_Descriptor, "work", Head_Commit),
    branch_head_commit(Remote_Repository_Descriptor, "work", Head_Commit),

    true.

test(push_headless_remote,
     [setup((setup_temp_store(State),
             create_db_without_schema(admin,foo))),
      cleanup(teardown_temp_store(State)),
      error(push_has_no_repository_head(_))
     ])
:-
    Destination_Path = "admin/foo/local/branch/work",
    super_user_authority(Auth),
    branch_create(system_descriptor{}, Auth, Destination_Path, empty(_,_), _),

    resolve_absolute_string_descriptor(Destination_Path, Work_Branch_Descriptor),
    Prefixes = _{'@base' : 'http://somewhere_else/',
                 '@schema': 'http://somewhere/schema#',
                 foo: 'http://the_foo_place/'},
    create_schema(Work_Branch_Descriptor, [], Prefixes),

    create_context(Work_Branch_Descriptor, commit_info{author:"test", message:"test"}, Work_Branch_Context),
    with_transaction(Work_Branch_Context,
                     ask(Work_Branch_Context,
                         insert(a,b,c)),
                     _),

    resolve_absolute_string_descriptor("admin/foo", Descriptor),

    Repository_Descriptor = (Descriptor.repository_descriptor),
    Database_Descriptor = (Repository_Descriptor.database_descriptor),

    resolve_relative_string_descriptor(Database_Descriptor, "remote/_commits", Remote_Repository_Descriptor),

    create_context(Database_Descriptor, Database_Context),
    with_transaction(Database_Context,
                     insert_remote_repository(Database_Context, "remote", "http://fakeytown.mock",_),
                     _),

    super_user_authority(Auth),
    push(system_descriptor{}, Auth, Destination_Path, "remote", "work", [], test_pusher(_New_Layer_Id), _Result),
    has_branch(Remote_Repository_Descriptor, "work"),
    branch_head_commit(Repository_Descriptor, "work", Head_Commit),
    branch_head_commit(Remote_Repository_Descriptor, "work", Head_Commit),

    true.

test(push_prefixes,
     [setup((setup_temp_store(State),
             create_db_without_schema(admin,foo))),
      cleanup(teardown_temp_store(State))
     ])
:-
    resolve_absolute_string_descriptor("admin/foo", Descriptor),

    Database_Descriptor = (Descriptor.repository_descriptor.database_descriptor),

    resolve_relative_string_descriptor(Database_Descriptor, "remote/_commits", Remote_Repository_Descriptor),


    create_context(Database_Descriptor, Database_Context),
    with_transaction(Database_Context,
                     insert_remote_repository(Database_Context, "remote", "http://somewhere", _),
                     _),
    create_ref_layer(Remote_Repository_Descriptor),

    super_user_authority(Auth),

    push(system_descriptor{}, Auth, "admin/foo", "remote", "main", [], test_pusher(_), _Result),

    resolve_absolute_string_descriptor("admin/foo/remote/branch/main", Remote),
    database_prefixes(Remote, Prefixes),

    Prefixes = _{'@base' : "http://somewhere.for.now/document/",
                 '@schema': "http://somewhere.for.now/schema#",
                 '@type': 'Context'}.

erroring_push_predicate(Error, _Remote_Url, _Payload) :-
    throw(Error).

generic_setup_for_error_conditions(Branch_Descriptor, Auth) :-
    resolve_absolute_string_descriptor("admin/foo/local/_commits", Repository_Descriptor),

    resolve_relative_descriptor(Repository_Descriptor, ["branch", "main"], Branch_Descriptor),

    create_context(Branch_Descriptor, commit_info{author:"test", message:"test"}, Branch_Context),
    with_transaction(Branch_Context,
                     ask(Branch_Context,
                         insert(a,b,c)),
                     _),

    Database_Descriptor = (Branch_Descriptor.repository_descriptor.database_descriptor),

    resolve_relative_string_descriptor(Database_Descriptor, "remote/_commits", Remote_Repository_Descriptor),

    create_context(Database_Descriptor, Database_Context),
    with_transaction(Database_Context,
                     insert_remote_repository(Database_Context, "remote", "http://fakeytown.mock",_),
                     _),
    create_ref_layer(Remote_Repository_Descriptor),

    super_user_authority(Auth).


test(remote_diverged,
     [setup((setup_temp_store(State),
             create_db_without_schema(admin,foo))),
      cleanup(teardown_temp_store(State)),
      throws(error(remote_unpack_failed(history_diverged),_))
     ])
:-
    generic_setup_for_error_conditions(Branch_Descriptor, Auth),
    resolve_absolute_string_descriptor(Branch, Branch_Descriptor),
    push(system_descriptor{}, Auth, Branch, "remote", "main", [], erroring_push_predicate(error(history_diverged,_)), _Result).

test(remote_does_not_exist,
     [setup((setup_temp_store(State),
             create_db_without_schema(admin,foo))),
      cleanup(teardown_temp_store(State)),
      throws(error(remote_unpack_failed(remote_unknown),_))
     ])
:-
    generic_setup_for_error_conditions(Branch_Descriptor, Auth),
    resolve_absolute_string_descriptor(Branch, Branch_Descriptor),
    push(system_descriptor{}, Auth, Branch, "remote", "main", [], erroring_push_predicate(error(remote_unknown,_)), _Result).

test(remote_authorization_failed,
     [setup((setup_temp_store(State),
             create_db_without_schema(admin,foo))),
      cleanup(teardown_temp_store(State)),
      throws(error(remote_unpack_failed(authorization_failure(_)),_))
     ])
:-
    generic_setup_for_error_conditions(Branch_Descriptor, Auth),
    resolve_absolute_string_descriptor(Branch, Branch_Descriptor),
    push(system_descriptor{}, Auth, Branch, "remote", "main", [], erroring_push_predicate(error(authorization_failure(some_context_idunno),_)), _Result).

test(remote_communication_failed,
     [setup((setup_temp_store(State),
             create_db_without_schema(admin,foo))),
      cleanup(teardown_temp_store(State)),
      throws(error(remote_unpack_failed(communication_failure(_)),_))
     ])
:-
    generic_setup_for_error_conditions(Branch_Descriptor, Auth),
    resolve_absolute_string_descriptor(Branch, Branch_Descriptor),
    push(system_descriptor{}, Auth, Branch, "remote", "main", [], erroring_push_predicate(error(communication_failure(some_context_idunno),_)), _Result).

test(remote_gave_unknown_error,
     [setup((setup_temp_store(State),
             create_db_without_schema(admin,foo))),
      cleanup(teardown_temp_store(State)),
      throws(error(remote_unpack_unexpected_failure(error(phase_of_the_moon_is_wrong(full),_)),_))
     ])
:-
    generic_setup_for_error_conditions(Branch_Descriptor, Auth),
    resolve_absolute_string_descriptor(Branch, Branch_Descriptor),
    push(system_descriptor{}, Auth, Branch, "remote", "main", [], erroring_push_predicate(error(phase_of_the_moon_is_wrong(full),_)), _Result).

:- end_tests(push).
