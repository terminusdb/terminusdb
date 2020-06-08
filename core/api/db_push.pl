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

% 1. rebase on remote
% 2. pack and send
% 3. error if head moved

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
        error(no_repository_with_name(Database_Descriptor,Remote_Name))),

    do_or_die(
        Type = remote,
        error(push_attempted_on_non_remote(Database_Descriptor,Remote_Name))),

    do_or_die(
        repository_remote_url(Database_Descriptor, Remote_Name, Remote_URL),
        error(remote_repository_does_not_exist(Remote_Name))),

    % Begin hypothetical rebase for pack
    create_context(Repository_Descriptor, Repository_Context_With_Prefixes),
    context_default_prefixes(Repository_Context_With_Prefixes, Repository_Context),

    resolve_relative_descriptor(Database_Descriptor,[Remote_Name, "_commits"],
                                Remote_Repository),

    (   create_context(Remote_Repository, Remote_Repository_Context_With_Prefixes),
        context_default_prefixes(Remote_Repository_Context_With_Prefixes,
                                 Remote_Repository_Context),
        context_to_parent_transaction(Remote_Repository_Context, Database_Transaction),
        do_or_die(repository_head(Database_Transaction, Remote_Name, Last_Head_Id),
                  error(push_has_no_repository_head(Remote_Repository)))
    ->  (   branch_head_commit(Repository_Context,
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
            ->  (   Remote_Branch_Path = []
                ->  true
                ;   throw(error(remote_diverged(Remote_Repository,Remote_Branch_Path)))
                )
            % No shared history
            ;   true),

            copy_commits(Repository_Context, Remote_Repository_Context, Local_Commit_Id),
            reset_branch_head(Remote_Repository_Context, Remote_Branch_Uri, Local_Commit_Uri)
        ;   (   has_branch(Remote_Repository_Context,
                           Remote_Branch),
                branch_head_commit(Remote_Repository_Context,
                                   Remote_Branch, _)
            ->  throw(error(remote_not_empty_on_local_empty(Remote_Repository)))
            ;   insert_branch_object(Remote_Repository_Context_With_Prefixes, Remote_Branch, _)
            )
        )
    ;   % Remote doesn't even exist yet
        throw(error(remote_does_not_exist(Remote_Repository)))
    ),

    cycle_context(Remote_Repository_Context, Final_Context, Remote_Transaction_Object, _),
    repository_context__previous_head_option__payload(Final_Context, some(Last_Head_Id), Payload_Option),


    (   Payload_Option = none % Last_Head_Id = Current_Head_Id
    ->  Result = none
    ;   Payload_Option = some(Payload),
        call(Push_Predicate, Remote_URL, Remote_Branch, Payload),
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

test_pusher(Expected, _Remote_URL, _Remote_Branch, Payload) :-
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




:- end_tests(push).
