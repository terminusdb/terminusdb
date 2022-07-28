:- module(api_reset, [api_reset/4]).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(account)).
:- use_module(library(terminus_store)).
:- use_module(core(util/test_utils)).

:- use_module(library(plunit)).

api_reset(System_DB, Auth, Path, Ref) :-

    do_or_die(
        resolve_absolute_string_descriptor(Path, Branch_Descriptor),
        error(invalid_absolute_path(Path),_)),

    do_or_die(
        branch_descriptor{ branch_name : Branch_Name } :< Branch_Descriptor,
        error(not_a_branch_descriptor(Branch_Descriptor),_)),

    Repo = (Branch_Descriptor.repository_descriptor),
    do_or_die(
        askable_context(Repo, System_DB, Auth, Context),
        error(unresolvable_absolute_descriptor(Repo), _)),

    do_or_die(
        (   resolve_absolute_string_descriptor(Ref, Ref_Descriptor)
        ->  true
        ;   resolve_relative_descriptor(Branch_Descriptor,
                                        ["commit", Ref],
                                        Ref_Descriptor)
        ),
        error(invalid_ref_path(Ref),_)),

    do_or_die(
        commit_descriptor{ commit_id : Commit_Id } :< Ref_Descriptor,
        error(not_a_commit_descriptor(Ref_Descriptor),_)),

    Ref_Repo = (Ref_Descriptor.repository_descriptor),

    do_or_die(
        Repo = Ref_Repo,
        error(different_repositories(Repo,Ref_Repo),_)),

    do_or_die(
        commit_id_uri(Repo,Commit_Id,Commit_Uri),
        error(unresolvable_commit_id(Commit_Id),_)),

    do_or_die(
        branch_name_uri(Repo, Branch_Name, Branch_Uri),
        error(branch_does_not_exist(Branch_Descriptor),_)),

    assert_write_access(Context),

    with_transaction(
        Context,
        reset_branch_head(Context, Branch_Uri, Commit_Uri),
        _).

:- begin_tests(reset).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).

test(reset_branch_to_past,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State))]
    ) :-

    Path = "admin/testdb",

    resolve_absolute_string_descriptor(Path,Descriptor),
    Repository_Descriptor = (Descriptor.repository_descriptor),

    super_user_authority(Auth),

    askable_context(Descriptor, system_descriptor{}, Auth,
                    commit_info{ author : "me",
                                 message : "commit 1" },
                    Context),

    with_transaction(
        Context,
        ask(Context,
            insert(a,b,c)),
        _),

    descriptor_commit_id_uri(Repository_Descriptor,
                             Descriptor,
                             Commit_Id,
                             _Commit_Uri),

    askable_context(Descriptor, system_descriptor{}, Auth,
                    commit_info{ author : "me",
                                 message : "commit 2" },
                    Context2),

    with_transaction(
        Context2,
        ask(Context2,
            insert(e,f,g)),
        _),

    resolve_relative_descriptor(Repository_Descriptor,
                                ["commit",Commit_Id],
                                Commit_Descriptor),
    resolve_absolute_string_descriptor(Ref,Commit_Descriptor),

    api_reset(system_descriptor{}, Auth, Path, Ref),

    findall(X-Y-Z,
            ask(Descriptor,
                t(X,Y,Z)),
            Triples),

    [a-b-c] = Triples.

:- end_tests(reset).

