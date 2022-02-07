:- module(api_bundle, [bundle/5]).

:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(triple)).
:- use_module(core(account)).
:- use_module(library(terminus_store)).
:- use_module(core(api/api_remote)).
:- use_module(core(api/db_push)).

:- use_module(library(md5)).
:- use_module(library(yall)).
:- use_module(library(plunit)).

bundle(System_DB, Auth, Path, Payload, Options) :-

    do_or_die(
        resolve_absolute_string_descriptor(Path, Branch_Descriptor),
        error(invalid_absolute_path(Path),_)),

    do_or_die(
        (branch_descriptor{} :< Branch_Descriptor),
        error(push_requires_branch(Branch_Descriptor),_)),

    % This looks like it could have race conditions and consistency problems.
    setup_call_cleanup(
        % Setup
        (   random_string(String),
            md5_hash(String, Remote_Name_Atom, []), % 32 chars.
            atom_string(Remote_Name_Atom,Remote_Name),
            add_remote(System_DB, Auth, Path, Remote_Name, "terminusdb:///bundle"),
            % This is crazy stupid... It should be possible to work with a repository without
            % creating empty layers.
            create_fake_repo_head(Branch_Descriptor, Remote_Name)
        ),
        % Call
        (   push(System_DB, Auth, Path, Remote_Name, "main", Options,
                 {Payload}/[_,P]>>(P = Payload),
                 _)
        ),

        % Cleanup
        remove_remote(System_DB, Auth, Path, Remote_Name)
    ).

create_fake_repo_head(Branch_Descriptor, Remote_Name) :-
    triple_store(Store),
    open_write(Store, Builder),
    nb_commit(Builder, Layer),
    layer_to_id(Layer, Layer_Id),
    get_dict(repository_descriptor, Branch_Descriptor, Repository_Descriptor),
    get_dict(database_descriptor, Repository_Descriptor, Database_Descriptor),
    create_context(Database_Descriptor, Context),
    with_transaction(
        Context,
        update_repository_head(Context, Remote_Name, Layer_Id),
        _).

:- begin_tests(bundle_tests).

:- use_module(core(util/test_utils)).


test(bundle,
     [setup((setup_temp_store(State),
             create_db_with_test_schema('admin','test'))),
      cleanup(teardown_temp_store(State))])
:-
    open_descriptor(system_descriptor{}, System),
    bundle(System, 'User/admin', 'admin/test', _, []).

test(create_fake_repo_head,
     [setup((setup_temp_store(State),
             create_db_with_test_schema('admin','test'))),
      cleanup(teardown_temp_store(State))])
:-
    resolve_absolute_string_descriptor('admin/test', Branch_Descriptor),
    create_fake_repo_head(Branch_Descriptor, _).


 :- end_tests(bundle_tests).
