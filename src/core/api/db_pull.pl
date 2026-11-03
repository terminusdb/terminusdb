:- module(db_pull, [
              pull/7
          ]).

:- use_module(core(util)).
:- use_module(db_fetch).
:- use_module(db_fast_forward).
:- use_module(core(transaction)).
:- use_module(core(account)).
:- use_module(core(query)).
:- use_module(core(plugins)).

:- meta_predicate pull(+, +, +, +, +, 3, -).
pull(System_DB, Local_Auth, Our_Branch_Path, Remote_Name, Remote_Branch_Name, Fetch_Predicate,
     Status) :-

    do_or_die(
        resolve_absolute_string_descriptor(Our_Branch_Path,Our_Branch_Descriptor),
        error(invalid_absolute_path(Our_Branch_Path),_)),

    check_descriptor_auth(System_DB, Our_Branch_Descriptor, '@schema':'Action/schema_write_access', Local_Auth),
    check_descriptor_auth(System_DB, Our_Branch_Descriptor, '@schema':'Action/instance_write_access', Local_Auth),

    do_or_die(
        (   branch_descriptor{ branch_name : Our_Branch_Name } :< Our_Branch_Descriptor,
            open_descriptor(Our_Branch_Descriptor, _)),
        error(unknown_local_branch(Our_Branch_Path),_)),

    (   var(Remote_Branch_Name)
    ->  Remote_Branch_Name = Our_Branch_Name
    ;   true
    ),

    Our_Repository_Descriptor = (Our_Branch_Descriptor.repository_descriptor),
    Their_Repository_Descriptor = (Our_Repository_Descriptor.put(_{ repository_name : Remote_Name })),

    Their_Branch_Descriptor = branch_descriptor{
                                  branch_name : Remote_Branch_Name,
                                  repository_descriptor : Their_Repository_Descriptor
                              },

    resolve_absolute_string_descriptor(Their_Repository_Path, Their_Repository_Descriptor),
    % 1. fetch
    remote_fetch(System_DB, Local_Auth, Their_Repository_Path, Fetch_Predicate,
                 _New_Head_Layer_Id, Head_Has_Updated),

    do_or_die(open_descriptor(Their_Branch_Descriptor, _),
              error(not_a_valid_remote_branch(Their_Branch_Descriptor),_)),

    Status = status{
                 'api:pull_status': Inner_Status,
                 'api:fetch_status' : Head_Has_Updated
             },
    % 2. try fast forward - alert front end if impossible.
    catch_with_backtrace(
        (   fast_forward_branch(Our_Branch_Descriptor, Their_Branch_Descriptor, Applied_Commit_Ids),
            (   Applied_Commit_Ids = []
            ->  Inner_Status = "api:pull_unchanged"
            ;   Inner_Status = "api:pull_fast_forwarded"
            )
        ),
        E,
        (   E = error(Inner_E,Ctx),
            (   Inner_E = divergent_history(Common_Commit,_,[])
            ->  Inner_Status = "api:pull_ahead"
            ;   Inner_E = divergent_history(Common_Commit,_,_)
            ->  throw(error(pull_divergent_history(Common_Commit,Head_Has_Updated),Ctx))
            ;   Inner_E = no_common_history
            ->  throw(error(pull_no_common_history(Head_Has_Updated),Ctx))
            ;   throw(E))
        )
    ),

    % Fast-forwarded commits bypass the transaction commit path — fire
    % the hook so consumers (e.g. the indexer) see the applied commits.
    (   Inner_Status == "api:pull_fast_forwarded"
    ->  ignore(forall(post_fast_forward_hook(Our_Branch_Descriptor,
                                             Applied_Commit_Ids),
                      true))
    ;   true).


:- begin_tests(pull_indexer_hook, [concurrent(false)]).

:- use_module(core(util/test_utils)).
:- use_module(core(util)).
:- use_module(core(transaction)).
:- use_module(core(triple)).
:- use_module(core(query)).
:- use_module(core(plugins)).
:- use_module(db_pack).

:- dynamic ff_hook_clause_ref/1.

% The recorder clause records into the global recorded database — test
% block code does not live in the module, so a qualified call back into
% db_pull would not resolve at run time.
install_ff_hook_recorder :-
    assertz(plugins:(post_fast_forward_hook(D, I) :-
                     recordz(pull_ff_hook_call, D-I)),
            Ref),
    assertz(ff_hook_clause_ref(Ref)).

uninstall_ff_hook_recorder :-
    forall(ff_hook_clause_ref(Ref), erase(Ref)),
    retractall(ff_hook_clause_ref(_)),
    forall(recorded(pull_ff_hook_call, _, Ref2), erase(Ref2)).

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

test("pull fast-forward fires post_fast_forward_hook; unchanged pull does not",
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "pullsrc"),
             create_db_without_schema("admin", "pulldest"),
             install_ff_hook_recorder)),
      cleanup((uninstall_ff_hook_recorder,
               teardown_temp_store(State)))
     ]) :-
    % Commit a triple on the source so the pull has history to apply.
    resolve_absolute_string_descriptor("admin/pullsrc", Src_Branch),
    create_context(Src_Branch,
                   commit_info{author:"test", message:"test"},
                   Src_Context),
    with_transaction(Src_Context,
                     ask(Src_Context, insert(a,b,c)),
                     _),
    triple_store(Store),

    open_descriptor(system_descriptor{}, System),
    super_user_authority(Auth),

    % Attach a URL remote so remote_fetch can locate the (mocked) pack.
    resolve_absolute_string_descriptor("admin/pulldest/_meta", Dest_Meta),
    create_context(Dest_Meta,
                   commit_info{author:"test", message:"test"},
                   Dest_Meta_Context),
    with_transaction(Dest_Meta_Context,
                     insert_remote_repository(Dest_Meta_Context, "origin",
                                              "http://fake/pack/admin/pullsrc",
                                              _Repo_Uri),
                     _),

    pull(System, Auth, "admin/pulldest", "origin", "main",
         get_pack_from_store(Store), Status),
    get_dict('api:pull_status', Status, Pull_Status),
    assertion(Pull_Status == "api:pull_fast_forwarded"),

    once(recorded(pull_ff_hook_call, Desc-Ids)),
    assertion(Ids \== []),
    resolve_absolute_string_descriptor(Path, Desc),
    assertion(Path == 'admin/pulldest/local/branch/main'),

    % Second pull: nothing new — no additional hook call.
    pull(System, Auth, "admin/pulldest", "origin", "main",
         get_pack_from_store(Store), Status2),
    get_dict('api:pull_status', Status2, Pull_Status2),
    assertion(Pull_Status2 == "api:pull_unchanged"),
    findall(_, recorded(pull_ff_hook_call, _), Calls),
    assertion(length(Calls, 1)).

:- end_tests(pull_indexer_hook).


