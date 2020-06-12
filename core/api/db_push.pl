:- module(db_push, [
              push/6
          ]).

:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(library(terminus_store)).
:- use_module(core(transaction)).
:- use_module(db_pack).
:- use_module(db_rebase).
:- use_module(db_create).

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

:- meta_predicate push(+, +, +, +, 3, -).
push(Branch_Descriptor, Remote_Name, Remote_Branch, _Auth_ID,
     Push_Predicate, Result) :-
    do_or_die(
        open_descriptor(Branch_Descriptor, _Branch_Transaction), % dodgy underscore
        error(branch_does_not_exist_in_push(Branch_Descriptor))),

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
    create_context(Repository_Descriptor, Repository_Context_With_Prefixes),
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
                                   Remote_Commit_Uri)
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
        % No shared history
        ;   true),

        copy_commits(Repository_Context, Remote_Repository_Context, Local_Commit_Id),
        reset_branch_head(Remote_Repository_Context_With_Prefixes, Remote_Branch_Uri, Local_Commit_Uri)
    ;   (   has_branch(Remote_Repository_Context,
                       Remote_Branch),
            branch_head_commit(Remote_Repository_Context,
                               Remote_Branch, _)
        ->  throw(error(remote_not_empty_on_local_empty(Remote_Repository),_))
        ;   insert_branch_object(Remote_Repository_Context_With_Prefixes, Remote_Branch, _)
        )
    ),

    cycle_context(Remote_Repository_Context, Final_Context, Remote_Transaction_Object, _),
    repository_context__previous_head_option__payload(Final_Context, some(Last_Head_Id), Payload_Option),


    (   Payload_Option = none % Last_Head_Id = Current_Head_Id
    ->  Result = none
    ;   Payload_Option = some(Payload),
        catch(call(Push_Predicate, Remote_URL, Payload),
              E,
              (   E = error(Inner_E),
                  (   Inner_E = history_diverged
                  ;   Inner_E = remote_unknown
                  ;   Inner_E = authorization_failure(_)
                  ;   Inner_E = communication_failure(_))
              ->  throw(error(remote_unpack_failed(Inner_E)))
              ;   throw(error(remote_unpack_unexpected_failure(E))))),
        Database_Transaction_Object = (Remote_Transaction_Object.parent),
        [Read_Obj] = (Remote_Transaction_Object.instance_objects),
        Layer = (Read_Obj.read),
        layer_to_id(Layer, Current_Head_Id),
        update_repository_head(Database_Transaction_Object, Remote_Name, Current_Head_Id),
        run_transactions([Database_Transaction_Object], _),
        Result = some(Current_Head_Id)
    ).



:- begin_tests(push).
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
             create_db_without_schema('user|foo','test','a test'))),
      cleanup(teardown_temp_store(State))])
:-
    resolve_absolute_string_descriptor("user/foo", Descriptor),

    Database_Descriptor = (Descriptor.repository_descriptor.database_descriptor),

    resolve_relative_string_descriptor(Database_Descriptor, "remote/_commits", Repository_Descriptor),


    create_context(Database_Descriptor, Database_Context),
    with_transaction(Database_Context,
                     insert_remote_repository(Database_Context, "remote", "http://somewhere", _),
                     _),
    create_ref_layer(Repository_Descriptor,
                     _{doc : 'http://somewhere/', scm: 'http://somewhere/schema#'}),

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
                        t(_,layer:layer_id, Expected_Layer_Id^^(xsd:string)))),

    push(Descriptor, "remote", "master", Auth, test_pusher(Expected_Layer_Id), _Result),

    resolve_absolute_string_descriptor("user/foo/remote/branch/master", Remote_Branch),
    findall(X-Y-Z, ask(Remote_Branch, t(X,Y,Z)), Triples),
    sort(Triples, [a-b-c,c-d-e]).


test(push_twice,
     [setup((setup_temp_store(State),
             create_db_without_schema('user|foo','test','a test'))),
      cleanup(teardown_temp_store(State))])
:-
    resolve_absolute_string_descriptor("user/foo", Descriptor),

    Database_Descriptor = (Descriptor.repository_descriptor.database_descriptor),

    resolve_relative_string_descriptor(Database_Descriptor, "remote/_commits", Repository_Descriptor),


    create_context(Database_Descriptor, Database_Context),
    with_transaction(Database_Context,
                     insert_remote_repository(Database_Context, "remote", "http://somewhere", _),
                     _),
    create_ref_layer(Repository_Descriptor,
                     _{doc : 'http://somewhere/', scm: 'http://somewhere/schema#'}),

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
                        t(_,layer:layer_id, Expected_Layer_Id^^(xsd:string)))),

    push(Descriptor, "remote", "master", Auth, test_pusher(Expected_Layer_Id), _Result),

    create_context(Descriptor, commit_info{author: "me",
                                           message: "something else"},
                   Branch_Context_2),

    with_transaction(
        Branch_Context_2,
        ask(Branch_Context_2,
            (   insert(h,i,j),
                insert(k,l,m))),
        _Meta_Data_B_2),


    push(Descriptor, "remote", "master", Auth, test_pusher(_Expected_Layer_Id_2), _Result_2),

    resolve_absolute_string_descriptor("user/foo/remote/branch/master", Remote_Branch),
    findall(X-Y-Z, ask(Remote_Branch, t(X,Y,Z)), Triples),
    sort(Triples, [a-b-c,c-d-e,h-i-j,k-l-m]).

test(push_empty_branch,
     [setup((setup_temp_store(State),
             create_db_without_schema('user|foo','test','a test'))),
      cleanup(teardown_temp_store(State))])
:-
    resolve_absolute_string_descriptor("user/foo", Descriptor),

    Database_Descriptor = (Descriptor.repository_descriptor.database_descriptor),

    resolve_relative_string_descriptor(Database_Descriptor, "remote/_commits", Remote_Repository_Descriptor),


    create_context(Database_Descriptor, Database_Context),
    with_transaction(Database_Context,
                     insert_remote_repository(Database_Context, "remote", "http://somewhere", _),
                     _),
    create_ref_layer(Remote_Repository_Descriptor,
                     _{doc : 'http://somewhere/', scm: 'http://somewhere/schema#'}),

    super_user_authority(Auth),
    push(Descriptor, "remote", "master", Auth, test_pusher(_New_Layer_Id), _Result),
    has_branch(Remote_Repository_Descriptor, "master"),

    true.

test(push_new_nonmaster_branch,
     [setup((setup_temp_store(State),
             create_db_without_schema('user|foo','test','a test'))),
      cleanup(teardown_temp_store(State))])
:-
    resolve_absolute_string_descriptor("user/foo/local/_commits", Repository_Descriptor),
    branch_create(Repository_Descriptor, empty, "work", _),
    resolve_relative_descriptor(Repository_Descriptor, ["branch", "work"], Work_Branch_Descriptor),
    resolve_absolute_string_descriptor("user/foo", Descriptor),

    Database_Descriptor = (Descriptor.repository_descriptor.database_descriptor),

    resolve_relative_string_descriptor(Database_Descriptor, "remote/_commits", Remote_Repository_Descriptor),


    create_context(Database_Descriptor, Database_Context),
    with_transaction(Database_Context,
                     insert_remote_repository(Database_Context, "remote", "http://somewhere", _),
                     _),
    create_ref_layer(Remote_Repository_Descriptor,
                     _{doc : 'http://somewhere/', scm: 'http://somewhere/schema#'}),

    super_user_authority(Auth),
    push(Work_Branch_Descriptor, "remote", "work", Auth, test_pusher(_New_Layer_Id), _Result),
    has_branch(Remote_Repository_Descriptor, "work"),

    true.

test(push_new_nonmaster_branch_with_content,
     [setup((setup_temp_store(State),
             create_db_without_schema('user|foo','test','a test'))),
      cleanup(teardown_temp_store(State))])
:-
    resolve_absolute_string_descriptor("user/foo/local/_commits", Repository_Descriptor),
    branch_create(Repository_Descriptor, empty, "work", _),

    resolve_relative_descriptor(Repository_Descriptor, ["branch", "work"], Work_Branch_Descriptor),

    create_context(Work_Branch_Descriptor, commit_info{author:"test", message:"test"}, Work_Branch_Context),
    with_transaction(Work_Branch_Context,
                     ask(Work_Branch_Context,
                         insert(a,b,c)),
                     _),

    resolve_absolute_string_descriptor("user/foo", Descriptor),

    Database_Descriptor = (Descriptor.repository_descriptor.database_descriptor),

    resolve_relative_string_descriptor(Database_Descriptor, "remote/_commits", Remote_Repository_Descriptor),


    create_context(Database_Descriptor, Database_Context),
    with_transaction(Database_Context,
                     insert_remote_repository(Database_Context, "remote", "http://somewhere", _),
                     _),
    create_ref_layer(Remote_Repository_Descriptor,
                     _{doc : 'http://somewhere/', scm: 'http://somewhere/schema#'}),

    super_user_authority(Auth),
    push(Work_Branch_Descriptor, "remote", "work", Auth, test_pusher(_New_Layer_Id), _Result),
    has_branch(Remote_Repository_Descriptor, "work"),
    branch_head_commit(Repository_Descriptor, "work", Head_Commit),
    branch_head_commit(Remote_Repository_Descriptor, "work", Head_Commit),

    true.

test(push_without_branch,
     [setup((setup_temp_store(State),
             create_db_without_schema('user|foo','test','a test'))),
      cleanup(teardown_temp_store(State)),
      error(branch_does_not_exist(_))])
:-
    resolve_absolute_string_descriptor("user/foo/local/_commits", Repository_Descriptor),
    resolve_relative_descriptor(Repository_Descriptor, ["branch", "work"], Work_Branch_Descriptor),

    resolve_absolute_string_descriptor("user/foo", Descriptor),

    Database_Descriptor = (Descriptor.repository_descriptor.database_descriptor),

    resolve_relative_string_descriptor(Database_Descriptor, "remote/_commits", Remote_Repository_Descriptor),

    create_context(Database_Descriptor, Database_Context),
    with_transaction(Database_Context,
                     insert_remote_repository(Database_Context, "remote", "http://somewhere", _),
                     _),

    create_ref_layer(Remote_Repository_Descriptor,
                     _{doc : 'http://somewhere/', scm: 'http://somewhere/schema#'}),

    super_user_authority(Auth),

    push(Work_Branch_Descriptor, "remote", "work", Auth, test_pusher(_New_Layer_Id), _Result).

test(push_without_repository,
     [setup((setup_temp_store(State),
             create_db_without_schema('user|foo','test','a test'))),
      cleanup(teardown_temp_store(State)),
      error(no_repository_with_name(_,_))])
:-
        resolve_absolute_string_descriptor("user/foo/local/_commits", Repository_Descriptor),
    branch_create(Repository_Descriptor, empty, "work", _),
    resolve_relative_descriptor(Repository_Descriptor, ["branch", "work"], Work_Branch_Descriptor),
    resolve_absolute_string_descriptor("user/foo", Descriptor),

    Database_Descriptor = (Descriptor.repository_descriptor.database_descriptor),

    resolve_relative_string_descriptor(Database_Descriptor, "remote/_commits", Remote_Repository_Descriptor),

    super_user_authority(Auth),
    push(Work_Branch_Descriptor, "remote", "work", Auth, test_pusher(_New_Layer_Id), _Result),
    has_branch(Remote_Repository_Descriptor, "work"),

    true.

test(push_local,
     [setup((setup_temp_store(State),
             create_db_without_schema('user|foo','test','a test'))),
      cleanup(teardown_temp_store(State)),
      error(push_attempted_on_non_remote(_,_))])
:-
    resolve_absolute_string_descriptor("user/foo/local/_commits", Repository_Descriptor),
    branch_create(Repository_Descriptor, empty, "work", _),

    resolve_relative_descriptor(Repository_Descriptor, ["branch", "work"], Work_Branch_Descriptor),

    create_context(Work_Branch_Descriptor, commit_info{author:"test", message:"test"}, Work_Branch_Context),
    with_transaction(Work_Branch_Context,
                     ask(Work_Branch_Context,
                         insert(a,b,c)),
                     _),

    resolve_absolute_string_descriptor("user/foo", Descriptor),

    Database_Descriptor = (Descriptor.repository_descriptor.database_descriptor),

    resolve_relative_string_descriptor(Database_Descriptor, "remote/_commits", Remote_Repository_Descriptor),


    create_context(Database_Descriptor, Database_Context),
    with_transaction(Database_Context,
                     insert_local_repository(Database_Context, "remote", _),
                     _),
    create_ref_layer(Remote_Repository_Descriptor,
                     _{doc : 'http://somewhere/', scm: 'http://somewhere/schema#'}),

    super_user_authority(Auth),
    push(Work_Branch_Descriptor, "remote", "work", Auth, test_pusher(_New_Layer_Id), _Result),
    has_branch(Remote_Repository_Descriptor, "work"),
    branch_head_commit(Repository_Descriptor, "work", Head_Commit),
    branch_head_commit(Remote_Repository_Descriptor, "work", Head_Commit),

    true.

test(push_headless_remote,
     [setup((setup_temp_store(State),
             create_db_without_schema('user|foo','test','a test'))),
      cleanup(teardown_temp_store(State)),
      error(push_has_no_repository_head(_))])
:-
    resolve_absolute_string_descriptor("user/foo/local/_commits", Repository_Descriptor),
    branch_create(Repository_Descriptor, empty, "work", _),

    resolve_relative_descriptor(Repository_Descriptor, ["branch", "work"], Work_Branch_Descriptor),

    create_context(Work_Branch_Descriptor, commit_info{author:"test", message:"test"}, Work_Branch_Context),
    with_transaction(Work_Branch_Context,
                     ask(Work_Branch_Context,
                         insert(a,b,c)),
                     _),

    resolve_absolute_string_descriptor("user/foo", Descriptor),

    Database_Descriptor = (Descriptor.repository_descriptor.database_descriptor),

    resolve_relative_string_descriptor(Database_Descriptor, "remote/_commits", Remote_Repository_Descriptor),

    create_context(Database_Descriptor, Database_Context),
    with_transaction(Database_Context,
                     insert_remote_repository(Database_Context, "remote", "http://fakeytown.mock",_),
                     _),

    super_user_authority(Auth),
    push(Work_Branch_Descriptor, "remote", "work", Auth, test_pusher(_New_Layer_Id), _Result),
    has_branch(Remote_Repository_Descriptor, "work"),
    branch_head_commit(Repository_Descriptor, "work", Head_Commit),
    branch_head_commit(Remote_Repository_Descriptor, "work", Head_Commit),

    true.

erroring_push_predicate(Error, _Remote_Url, _Payload) :-
    throw(Error).

generic_setup_for_error_conditions(Branch_Descriptor, Auth) :-
    resolve_absolute_string_descriptor("user/foo/local/_commits", Repository_Descriptor),

    resolve_relative_descriptor(Repository_Descriptor, ["branch", "master"], Branch_Descriptor),

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
    create_ref_layer(Remote_Repository_Descriptor,
                     _{doc : 'http://somewhere/', scm: 'http://somewhere/schema#'}),

    super_user_authority(Auth).
    

test(remote_diverged,
     [setup((setup_temp_store(State),
             create_db_without_schema('user|foo','test','a test'))),
      cleanup(teardown_temp_store(State)),
      throws(error(remote_unpack_failed(history_diverged)))])
:-
    generic_setup_for_error_conditions(Branch_Descriptor, Auth),

    push(Branch_Descriptor, "remote", "master", Auth, erroring_push_predicate(error(history_diverged)), _Result).

test(remote_does_not_exist,
     [setup((setup_temp_store(State),
             create_db_without_schema('user|foo','test','a test'))),
      cleanup(teardown_temp_store(State)),
      throws(error(remote_unpack_failed(remote_unknown)))])
:-
    generic_setup_for_error_conditions(Branch_Descriptor, Auth),

    push(Branch_Descriptor, "remote", "master", Auth, erroring_push_predicate(error(remote_unknown)), _Result).

test(remote_authorization_failed,
     [setup((setup_temp_store(State),
             create_db_without_schema('user|foo','test','a test'))),
      cleanup(teardown_temp_store(State)),
      throws(error(remote_unpack_failed(authorization_failure(_))))])
:-
    generic_setup_for_error_conditions(Branch_Descriptor, Auth),

    push(Branch_Descriptor, "remote", "master", Auth, erroring_push_predicate(error(authorization_failure(some_context_idunno))), _Result).

test(remote_communication_failed,
     [setup((setup_temp_store(State),
             create_db_without_schema('user|foo','test','a test'))),
      cleanup(teardown_temp_store(State)),
      throws(error(remote_unpack_failed(communication_failure(_))))])
:-
    generic_setup_for_error_conditions(Branch_Descriptor, Auth),

    push(Branch_Descriptor, "remote", "master", Auth, erroring_push_predicate(error(communication_failure(some_context_idunno))), _Result).

test(remote_gave_unknown_error,
     [setup((setup_temp_store(State),
             create_db_without_schema('user|foo','test','a test'))),
      cleanup(teardown_temp_store(State)),
      throws(error(remote_unpack_unexpected_failure(error(phase_of_the_moon_is_wrong(full)))))])
:-
    generic_setup_for_error_conditions(Branch_Descriptor, Auth),

    push(Branch_Descriptor, "remote", "master", Auth, erroring_push_predicate(error(phase_of_the_moon_is_wrong(full))), _Result).

:- end_tests(push).
