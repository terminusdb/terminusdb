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
:- use_module(core(api/api_unbundle)).
:- use_module(db_pack).


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

test(bundle_unbundle_independent_store,
     [setup((setup_temp_store(State),
             create_db_without_schema(admin,test))),
      cleanup(teardown_temp_store(State))])
:-
    % commit something onto main
    resolve_absolute_string_descriptor('admin/test', Branch_Descriptor),
    create_context(Branch_Descriptor, commit_info{author:"tester", message:"testing"}, Context),
    with_transaction(Context,
                     once(ask(Context, insert(a,b,c))),
                     _),

    open_descriptor(system_descriptor{}, System),
    bundle(System, 'User/admin', 'admin/test', Payload, []),

    % move the bundle into a completely independent store
    with_temp_store(
        (   create_db_without_schema(admin,test2),
            open_descriptor(system_descriptor{}, System2),
            unbundle(System2, 'User/admin', 'admin/test2', Payload),
            % the bundled commit history is now visible on main
            resolve_absolute_string_descriptor("admin/test2/local/branch/main", Main_Descriptor),
            open_descriptor(Main_Descriptor, Main_Transaction),
            once(ask(Main_Transaction, t(a,b,c))))).

test(bundle_unbundle_repeated,
     [setup((setup_temp_store(State),
             create_db_without_schema(admin,test))),
      cleanup(teardown_temp_store(State))])
:-
    resolve_absolute_string_descriptor('admin/test', Branch_Descriptor),
    create_context(Branch_Descriptor, commit_info{author:"tester", message:"testing"}, Context),
    with_transaction(Context,
                     once(ask(Context, insert(a,b,c))),
                     _),

    open_descriptor(system_descriptor{}, System),
    bundle(System, 'User/admin', 'admin/test', Payload, []),

    % unbundling the same bundle twice reuses the existing layers
    create_db_without_schema(admin,test2),
    open_descriptor(system_descriptor{}, System2),
    unbundle(System2, 'User/admin', 'admin/test2', Payload),
    create_db_without_schema(admin,test3),
    open_descriptor(system_descriptor{}, System3),
    unbundle(System3, 'User/admin', 'admin/test3', Payload),

    resolve_absolute_string_descriptor("admin/test3/local/branch/main", Main_Descriptor),
    open_descriptor(Main_Descriptor, Main_Transaction),
    once(ask(Main_Transaction, t(a,b,c))).

test(bundle_unbundle_conflicting_layer,
     [setup((setup_temp_store(State),
             create_db_without_schema(admin,test))),
      cleanup(teardown_temp_store(State))])
:-
    resolve_absolute_string_descriptor('admin/test', Branch_Descriptor),
    create_context(Branch_Descriptor, commit_info{author:"tester", message:"testing"}, Context),
    with_transaction(Context,
                     once(ask(Context, insert(a,b,c))),
                     _),

    open_descriptor(system_descriptor{}, System),
    bundle(System, 'User/admin', 'admin/test', Payload, []),
    payload_repository_head_and_pack(Payload, _Head, Pack),
    pack_layerids_and_parents(Pack, Layer_Parents),
    once(member(Layer_Id-_, Layer_Parents)),

    % an existing layer with the same id but different content must not
    % be silently reused
    catch((  with_temp_store(
                 (   create_db_without_schema(admin,test2),
                     triple_store(Store),
                     create_empty_base_layer(Store, Layer_Id),
                     open_descriptor(system_descriptor{}, System2),
                     unbundle(System2, 'User/admin', 'admin/test2', Payload))),
              fail),
          error(pack_layer_mismatch(Mismatched),_),
          memberchk(Layer_Id, Mismatched)).

test(create_empty_base_layer,
     [setup((setup_temp_store(State),
             with_temp_store(
                 (   triple_store(Other_Store),
                     open_write(Other_Store, Builder),
                     nb_commit(Builder, Other_Layer),
                     layer_to_id(Other_Layer, Layer_Id))))),
      cleanup(teardown_temp_store(State))])
:-
    % Layer_Id is a valid layer id that does not exist in this store
    triple_store(Store),
    \+ store_id_layer(Store, Layer_Id, _),

    create_empty_base_layer(Store, Layer_Id),
    store_id_layer(Store, Layer_Id, Layer),
    layer_to_id(Layer, Layer_Id),
    \+ parent(Layer, _),
    layer_total_triple_count(Layer, 0),

    % creating a layer that already exists fails loudly
    catch(create_empty_base_layer(Store, Layer_Id), Error, true),
    nonvar(Error).

 :- end_tests(bundle_tests).
