:- module(db_rebase, [
              rebase_on_branch/9,
              cycle_context/4
          ]).
:- use_module(library(terminus_store)).
:- use_module(library(apply)).
:- use_module(library(plunit)).
:- use_module(library(lists)).

:- use_module(core(util)).
:- use_module(core(account)).
:- use_module(core(query)).
:- use_module(core(transaction)).

cycle_context(Context, New_Context, New_Transaction_Object, Validation_Object) :-
    [Transaction_Object] = Context.transaction_objects,
    transaction_objects_to_validation_objects([Transaction_Object], [Validation_Object]),
    validate_validation_objects([Validation_Object], Witnesses),
    (   Witnesses = []
    ->  true
    % put more stuff in error!
    ;   throw(error(schema_validation_error(Witnesses), _))),
    validation_objects_to_transaction_objects([Validation_Object], [New_Transaction_Object]),

    create_context(New_Transaction_Object, New_Context).

make_graph_validation_object_appear_as_changed(Graph_Validation_Object, New_Graph_Validation_Object) :-
    % TODO actually do a super awesome check to see if the last layer is different and if so set changed to true
    New_Graph_Validation_Object = Graph_Validation_Object.put(changed, true).

make_validation_object_appear_as_changed(Validation_Object, New_Validation_Object) :-
    maplist(make_graph_validation_object_appear_as_changed,
            Validation_Object.schema_objects,
            Schema_Objects),
    maplist(make_graph_validation_object_appear_as_changed,
            Validation_Object.instance_objects,
            Instance_Objects),
    maplist(make_graph_validation_object_appear_as_changed,
            Validation_Object.inference_objects,
            Inference_Objects),

    Objects = _{ instance_objects: Instance_Objects,
                 schema_objects: Schema_Objects,
                 inference_objects: Inference_Objects },

    New_Validation_Object = (Validation_Object.put(Objects)).

apply_commit_chain(Our_Repo_Context, _Their_Repo_Context, Us_Commit_Uri, _Author, _Auth_Object, [], [], Us_Commit_Uri, Our_Repo_Context, []) :-
    !,
    true.
apply_commit_chain(Our_Repo_Context, Their_Repo_Context, Us_Commit_Uri, Author, Auth_Object, [Their_Commit_Id|Their_Commit_Ids], [Strategy|Strategies], Final_Commit_Uri, Return_Context, [Report|Reports]) :-
    Report = report{
                 '@type' : "api:RebaseReport",
                 'api:origin_commit': Their_Commit_Id,
                 'api:applied': Applied_Commit_Ids,
                 'api:commit_type': Commit_Application_Type
             },
    % apply the commit
    commit_id_uri(Their_Repo_Context, Their_Commit_Id, Their_Commit_Uri), % we are renaming, Commit_Uri -> Their_Commit_Uri
    apply_commit_on_commit(Our_Repo_Context, Their_Repo_Context, Us_Commit_Uri, Their_Commit_Uri, Author, New_Commit_Id, New_Commit_Uri),

    % turn our repo context into a validation object
    cycle_context(Our_Repo_Context, Our_Repo_Context2, New_Our_Repo_Transaction_Object, _Our_Repo_Validation_Object),

    Our_Commit_Descriptor = commit_descriptor{
                            repository_descriptor: New_Our_Repo_Transaction_Object.descriptor,
                            commit_id: New_Commit_Id
                        },
    % todo also include database descriptor and read write objects in that list
    % should not actually matter though
    [Commit_Read_Write_Obj] = New_Our_Repo_Transaction_Object.instance_objects,
    open_descriptor(Our_Commit_Descriptor, _, Our_Commit_Transaction, [New_Our_Repo_Transaction_Object.descriptor=New_Our_Repo_Transaction_Object,Commit_Read_Write_Obj.descriptor=Commit_Read_Write_Obj], _Output_Map),

    transaction_objects_to_validation_objects([Our_Commit_Transaction], [Our_Commit_Validation_Object_Unchanged]),

    make_validation_object_appear_as_changed(Our_Commit_Validation_Object_Unchanged,
                                             Our_Commit_Validation_Object),

    % validate the branch context
    validate_validation_objects([Our_Commit_Validation_Object], Witnesses),

    % if it validates, yay! continue to the next commit (see below)

    (   Witnesses = []
    ->  (   Strategy = error
        ->  Our_Repo_Context3 = Our_Repo_Context2,
            New_Commit_Uri2 = New_Commit_Uri,
            Commit_Application_Type = "api:valid_commit",
            Applied_Commit_Ids = [New_Commit_Id]
        ;   Strategy = continue
        ->  throw(error(apply_commit(continue_on_valid_commit(Their_Commit_Id)), _))
        ;   Strategy = fixup(_Message,_Woql)
        ->  throw(error(apply_commit(fixup_on_valid_commit(Their_Commit_Id)), _)))
    ;   (   Strategy = error
        ->  throw(error(apply_commit(schema_validation_error(Their_Commit_Id, Witnesses)), _))
        ;   Strategy = continue
        ->  invalidate_commit(Our_Repo_Context2, New_Commit_Id),
            cycle_context(Our_Repo_Context2, Our_Repo_Context3, _, _),
            New_Commit_Uri2 = New_Commit_Uri,
            Commit_Application_Type = "api:invalid_commit",
            Applied_Commit_Ids = [New_Commit_Id]
        ;   Strategy = fixup(Message, Woql)
        ->  create_context(Our_Commit_Transaction, Our_Commit_Fixup_Context_Without_Auth),
            put_dict(_{authorization: Auth_Object,
                       commit_info: commit_info{author:Author,message:Message}},
                     Our_Commit_Fixup_Context_Without_Auth,
                     Our_Commit_Fixup_Context),
            compile_query(Woql, Prog, Our_Commit_Fixup_Context, Our_Commit_Fixup_Context2),
            forall(woql_compile:Prog, true),
            % turn context into validation
            Our_Commit_Fixup_Context2.transaction_objects = [Commit_Fixup_Transaction_Object],
            transaction_objects_to_validation_objects([Commit_Fixup_Transaction_Object], [Commit_Fixup_Validation_Object]),
            % validate
            validate_validation_objects([Commit_Fixup_Validation_Object], Fixup_Witnesses),
            (   Fixup_Witnesses = []
            ->  true
            ;   throw(error(apply_commit(fixup_error(Their_Commit_Id, Fixup_Witnesses)), _))),

            % commit validation
            commit_commit_validation_object(Commit_Fixup_Validation_Object, _Parent_Transaction_List, New_Commit_Id2, New_Commit_Uri2),
            % write commit object into our repo context
            cycle_context(Our_Repo_Context2, Our_Repo_Context3, _, _),

            Commit_Application_Type = fixup,
            Applied_Commit_Ids = [New_Commit_Id, New_Commit_Id2])
    ),

    apply_commit_chain(Our_Repo_Context3, Their_Repo_Context, New_Commit_Uri2, Author, Auth_Object, Their_Commit_Ids, Strategies, Final_Commit_Uri, Return_Context, Reports).

create_strategies([], _Strategy_Map, []).
create_strategies([Commit_ID|Their_Branch_Path], Strategy_Map, [Strategy|Strategies]) :-
    (   memberchk(Commit_ID=Strategy, Strategy_Map)
    ->  true
    ;   Strategy = error),
    create_strategies(Their_Branch_Path, Strategy_Map, Strategies).

rebase_on_branch(System_DB, Auth, Our_Branch_Path, Their_Branch_Path, Author, Strategy_Map, Optional_Common_Commit_Id, Their_Branch_History, Reports) :-
    benchmark_subject_start('rebase on branch'),

    do_or_die(
        resolve_absolute_string_descriptor(Our_Branch_Path, Our_Branch_Descriptor),
        error(invalid_target_absolute_path(Our_Branch_Path),_)),

    check_descriptor_auth(System_DB, Our_Branch_Descriptor, '@schema':'Action/commit_read_access', Auth),
    check_descriptor_auth(System_DB, Our_Branch_Descriptor, '@schema':'Action/schema_write_access', Auth),
    check_descriptor_auth(System_DB, Our_Branch_Descriptor, '@schema':'Action/instance_write_access', Auth),

    do_or_die(
        (branch_descriptor{} :< Our_Branch_Descriptor),
        error(rebase_requires_target_branch(Our_Branch_Descriptor),_)),

    do_or_die(
        resolve_absolute_string_descriptor(Their_Branch_Path, Their_Ref_Descriptor),
        error(invalid_source_absolute_path(Their_Branch_Path),_)),

    do_or_die(
        (   branch_descriptor{} :< Their_Ref_Descriptor
        ->  true
        ;   commit_descriptor{} :< Their_Ref_Descriptor),
        error(rebase_requires_source_branch(Their_Ref_Descriptor),_)),

    check_descriptor_auth(System_DB, Their_Ref_Descriptor, '@schema':'Action/commit_read_access', Auth),

    Our_Repo_Descriptor = (Our_Branch_Descriptor.repository_descriptor),
    Their_Repo_Descriptor = (Their_Ref_Descriptor.repository_descriptor),

    do_or_die(
        create_context(Our_Repo_Descriptor, Our_Repo_Context),
        error(unresolvable_target_descriptor(Our_Repo_Descriptor))),
    do_or_die(
        create_context(Their_Repo_Descriptor, Their_Repo_Context),
        error(unresolvable_source_descriptor(Their_Repo_Descriptor))),

    benchmark(after_checks),

    do_or_die(
        branch_name_uri(Our_Repo_Context, Our_Branch_Descriptor.branch_name, Our_Branch_Uri),
        error(target_branch_not_found(Our_Branch_Path), _)),
    do_or_die(
        descriptor_commit_id_uri(Their_Repo_Context, Their_Ref_Descriptor, Their_Commit_Id, Their_Commit_Uri),
        error(source_branch_not_found(Their_Branch_Path), _)),

    (   branch_head_commit(Our_Repo_Context, Our_Branch_Descriptor.branch_name, Our_Commit_Uri),
        commit_id_uri(Our_Repo_Context, Our_Commit_Id, Our_Commit_Uri),
        commit_type(Our_Repo_Context, Our_Commit_Uri, 'http://terminusdb.com/schema/ref#ValidCommit')
    ->  (   most_recent_common_ancestor(Our_Repo_Context, Their_Repo_Context, Our_Commit_Id, Their_Commit_Id, Common_Commit_Id, Our_Branch_History, Their_Branch_History),
            benchmark(after_ancestor_lookup)
        ->  Optional_Common_Commit_Id = some(Common_Commit_Id)
        % We have no common history
        ;   Optional_Common_Commit_Id = none,
            commit_uri_to_history_commit_ids(Our_Repo_Context, Our_Commit_Uri, Our_Branch_History),
            benchmark(history_lookup_1),
            commit_uri_to_history_commit_ids(Their_Repo_Context, Their_Commit_Uri, Their_Branch_History),
            benchmark(history_lookup_2))
    % Branch head commit may not exist if our branch is empty...
    ;   Optional_Common_Commit_Id = none,
        Our_Branch_History = [],
        commit_uri_to_history_commit_ids(Their_Repo_Context, Their_Commit_Uri, Their_Branch_History)
    ),

    benchmark(before_copy),

    % copy commits from their repo into ours, ensuring we got the latest
    copy_commits(Their_Repo_Context, Our_Repo_Context, Their_Commit_Id),
    benchmark(after_copy),
    cycle_context(Our_Repo_Context, Our_Repo_Context2, _, _),
    benchmark(after_first_cycle),

    % take their commit uri as the top on which we're gonna apply_commit_chain
    % list of commits to apply is ours since common

    create_strategies(Our_Branch_History, Strategy_Map, Strategies),

    (   Our_Branch_History = []
    % yay we're done! All commits are known to us, no need to do a thing
    ->  Semifinal_Context = Our_Repo_Context2,
        Final_Commit_Uri = Their_Commit_Uri,
        Reports = []
    ;   catch(
            apply_commit_chain(Our_Repo_Context2,
                               Their_Repo_Context,
                               Their_Commit_Uri,
                               Author,
                               Auth,
                               Our_Branch_History,
                               Strategies,
                               Final_Commit_Uri,
                               Semifinal_Context,
                               Reports),
            error(apply_commit(Error), _),
            throw(error(rebase_commit_application_failed(Error,Our_Branch_History), _))
        )
    ),

    benchmark(after_application),

    ignore(unlink_commit_object_from_branch(Semifinal_Context, Our_Branch_Uri)),

    link_commit_object_to_branch(Semifinal_Context, Our_Branch_Uri, Final_Commit_Uri),

    benchmark(after_commit_branch_relink),

    cycle_context(Semifinal_Context, _Final_Context, Transaction_Object, _),

    benchmark(after_second_cycle),

    Repo_Name = Transaction_Object.descriptor.repository_name,
    [Read_Write_Obj] = Transaction_Object.instance_objects,
    Layer = Read_Write_Obj.read,
    layer_to_id(Layer, Layer_Id),
    Database_Transaction_Object = (Transaction_Object.parent),

    update_repository_head(Database_Transaction_Object, Repo_Name, Layer_Id),

    benchmark(after_repository_head_update),

    run_transactions([Database_Transaction_Object], true, _),
    benchmark_subject_stop('rebase on branch').

:- begin_tests(rebase, [concurrent(true)]).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).
:- use_module(core(account)).
:- use_module(core(document)).

:- use_module(db_create).
:- use_module(db_branch).

test(rebase_target_branch_does_not_exist,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "foo"))),
      cleanup(teardown_temp_store(State)),
      throws(error(target_branch_not_found("admin/foo/local/branch/does_not_exist"), _))
     ])
:-
    super_user_authority(Auth),
    rebase_on_branch(system_descriptor{},
                     Auth,
                     "admin/foo/local/branch/does_not_exist",
                     "admin/foo/local/branch/other",
                     "rebaser",
                     [], _, _, []).

test(rebase_source_branch_does_not_exist,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "foo"))),
      cleanup(teardown_temp_store(State)),
      throws(error(source_branch_not_found("admin/foo/local/branch/does_not_exist"), _))
     ])
:-
    super_user_authority(Auth),
    rebase_on_branch(system_descriptor{},
                     Auth,
                     "admin/foo/local/branch/main",
                     "admin/foo/local/branch/does_not_exist",
                     "rebaser",
                     [], _, _, []).

test(rebase_fast_forward,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "foo"))),
      cleanup(teardown_temp_store(State))
     ])
:-
    Master_Path = "admin/foo",
    resolve_absolute_string_descriptor("admin/foo", Master_Descriptor),
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit a"}, Master_Context),
    with_transaction(Master_Context,
                     ask(Master_Context,
                         insert(a,b,c)),
                    _),

    Second_Path = "admin/foo/local/branch/second",
    super_user_authority(Auth),
    branch_create(system_descriptor{}, Auth, Second_Path, branch(Master_Path), _),

    resolve_absolute_string_descriptor(Second_Path, Second_Descriptor),

    create_context(Second_Descriptor, commit_info{author:"test",message:"commit b"}, Second_Context1),
    with_transaction(Second_Context1,
                     ask(Second_Context1,
                         (   insert(d,e,f),
                             delete(a,b,c))),
                     _),

    create_context(Second_Descriptor, commit_info{author:"test",message:"commit c"}, Second_Context2),
    with_transaction(Second_Context2,
                     ask(Second_Context2,
                         insert(g,h,i)),
                     _),

    super_user_authority(Auth),

    rebase_on_branch(system_descriptor{}, Auth, Master_Path, Second_Path, "rebaser",  [], some(Common_Commit_Id), Their_Applied_Commit_Ids, []),
    Repo_Descriptor = Master_Descriptor.repository_descriptor,

    commit_id_to_metadata(Repo_Descriptor, Common_Commit_Id, _, "commit a", _),

    [Old_Commit_B_Id, Old_Commit_C_Id] = Their_Applied_Commit_Ids,
    commit_id_to_metadata(Repo_Descriptor, Old_Commit_B_Id, _, "commit b", _),
    commit_id_to_metadata(Repo_Descriptor, Old_Commit_C_Id, _, "commit c", _),

    branch_head_commit(Repo_Descriptor, "main", Commit_C_Uri),
    commit_uri_to_parent_uri(Repo_Descriptor, Commit_C_Uri, Commit_B_Uri),
    commit_uri_to_parent_uri(Repo_Descriptor, Commit_B_Uri, Commit_A_Uri),


    commit_uri_to_metadata(Repo_Descriptor, Commit_A_Uri, _, "commit a", _),
    commit_uri_to_metadata(Repo_Descriptor, Commit_B_Uri, _, "commit b", _),
    commit_uri_to_metadata(Repo_Descriptor, Commit_C_Uri, _, "commit c", _),

    findall(t(S,P,O),
            ask(Master_Descriptor,
                t(S,P,O)),
            Triples_Unsorted),

    sort(Triples_Unsorted, Triples),
    [t(d,e,f),
     t(g,h,i)] = Triples,

    true.

test(rebase_fast_forward_from_commit,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "foo"))),
      cleanup(teardown_temp_store(State))
     ])
:-
    Master_Path = "admin/foo",
    resolve_absolute_string_descriptor("admin/foo", Master_Descriptor),
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit a"}, Master_Context),
    with_transaction(Master_Context,
                     ask(Master_Context,
                         insert(a,b,c)),
                    _),

    Second_Path = "admin/foo/local/branch/second",
    super_user_authority(Auth),
    branch_create(system_descriptor{}, Auth, Second_Path, branch(Master_Path), _),

    resolve_absolute_string_descriptor(Second_Path, Second_Descriptor),

    create_context(Second_Descriptor, commit_info{author:"test",message:"commit b"}, Second_Context1),
    with_transaction(Second_Context1,
                     ask(Second_Context1,
                         (   insert(d,e,f),
                             delete(a,b,c))),
                     _),

    create_context(Second_Descriptor, commit_info{author:"test",message:"commit c"}, Second_Context2),
    with_transaction(Second_Context2,
                     ask(Second_Context2,
                         insert(g,h,i)),
                     _),

    super_user_authority(Auth),

    descriptor_commit_id_uri((Second_Descriptor.repository_descriptor),
                             Second_Descriptor,
                             Commit_Id,
                             _Commit_Uri),

    atomic_list_concat(['admin/foo/local/commit/',Commit_Id], Commit_Path),

    rebase_on_branch(system_descriptor{}, Auth, Master_Path, Commit_Path, "rebaser",  [], some(Common_Commit_Id), Their_Applied_Commit_Ids, []),
    Repo_Descriptor = Master_Descriptor.repository_descriptor,

    commit_id_to_metadata(Repo_Descriptor, Common_Commit_Id, _, "commit a", _),

    [Old_Commit_B_Id, Old_Commit_C_Id] = Their_Applied_Commit_Ids,
    commit_id_to_metadata(Repo_Descriptor, Old_Commit_B_Id, _, "commit b", _),
    commit_id_to_metadata(Repo_Descriptor, Old_Commit_C_Id, _, "commit c", _),

    branch_head_commit(Repo_Descriptor, "main", Commit_C_Uri),
    commit_uri_to_parent_uri(Repo_Descriptor, Commit_C_Uri, Commit_B_Uri),
    commit_uri_to_parent_uri(Repo_Descriptor, Commit_B_Uri, Commit_A_Uri),


    commit_uri_to_metadata(Repo_Descriptor, Commit_A_Uri, _, "commit a", _),
    commit_uri_to_metadata(Repo_Descriptor, Commit_B_Uri, _, "commit b", _),
    commit_uri_to_metadata(Repo_Descriptor, Commit_C_Uri, _, "commit c", _),

    findall(t(S,P,O),
            ask(Master_Descriptor,
                t(S,P,O)),
            Triples_Unsorted),

    sort(Triples_Unsorted, Triples),
    [t(d,e,f),
     t(g,h,i)] = Triples,

    true.


test(rebase_divergent_history,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "foo"))),
      cleanup(teardown_temp_store(State))
     ])
:-
    Master_Path = "admin/foo",
    resolve_absolute_string_descriptor(Master_Path, Master_Descriptor),
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit a"}, Master_Context1),
    with_transaction(Master_Context1,
                     ask(Master_Context1,
                         insert(a,b,c)),
                    _),
    Second_Path = "admin/foo/local/branch/second",
    super_user_authority(Auth),
    branch_create(system_descriptor{}, Auth, Second_Path, branch(Master_Path), _),
    resolve_absolute_string_descriptor(Second_Path, Second_Descriptor),

    create_context(Second_Descriptor, commit_info{author:"test",message:"commit b"}, Second_Context1),
    with_transaction(Second_Context1,
                     ask(Second_Context1,
                         (   insert(d,e,f),
                             delete(a,b,c))),
                     _),

    create_context(Second_Descriptor, commit_info{author:"test",message:"commit c"}, Second_Context2),
    with_transaction(Second_Context2,
                     ask(Second_Context2,
                         insert(g,h,i)),
                     _),

    % we're also doing a commit on the original branch, to create a divergent history
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit d"}, Master_Context2),
    with_transaction(Master_Context2,
                     ask(Master_Context2,
                         insert(j,k,l)),
                     _),

    Repo_Descriptor = Master_Descriptor.repository_descriptor,
    branch_head_commit(Repo_Descriptor, "main", Old_Commit_D_Uri),
    commit_id_uri(Repo_Descriptor, Old_Commit_D_Id, Old_Commit_D_Uri),

    super_user_authority(Auth),
    rebase_on_branch(system_descriptor{}, Auth, Master_Path, Second_Path, "rebaser", [], some(Common_Commit_Id), Their_Commit_Ids, Reports),

    commit_id_to_metadata(Repo_Descriptor, Common_Commit_Id, _, "commit a", _),

    branch_head_commit(Repo_Descriptor, "main", New_Commit_D_Uri),
    commit_uri_to_parent_uri(Repo_Descriptor, New_Commit_D_Uri, Commit_C_Uri),
    commit_uri_to_parent_uri(Repo_Descriptor, Commit_C_Uri, Commit_B_Uri),
    commit_uri_to_parent_uri(Repo_Descriptor, Commit_B_Uri, Commit_A_Uri),

    commit_uri_to_metadata(Repo_Descriptor, Commit_A_Uri, _, "commit a", _),
    commit_uri_to_metadata(Repo_Descriptor, Commit_B_Uri, _, "commit b", _),
    commit_uri_to_metadata(Repo_Descriptor, Commit_C_Uri, _, "commit c", _),
    commit_uri_to_metadata(Repo_Descriptor, New_Commit_D_Uri, _, "commit d", _),
    commit_id_uri(Repo_Descriptor, Commit_B_Id, Commit_B_Uri),
    commit_id_uri(Repo_Descriptor, Commit_C_Id, Commit_C_Uri),
    commit_id_uri(Repo_Descriptor, New_Commit_D_Id, New_Commit_D_Uri),

    [Commit_B_Id, Commit_C_Id] = Their_Commit_Ids,

    [_{'@type' : "api:RebaseReport",
       'api:origin_commit': Old_Commit_D_Id,
       'api:applied' : [New_Commit_D_Id],
       'api:commit_type': "api:valid_commit"}] = Reports,

    findall(t(S,P,O),
            ask(Master_Descriptor,
                t(S,P,O)),
            Triples_Unsorted),

    sort(Triples_Unsorted, Triples),
    [t(d,e,f),
     t(g,h,i),
     t(j,k,l)] = Triples,

    true.

test(rebase_conflicting_history_errors,
     [setup((setup_temp_store(State),
             create_db_with_test_schema("admin","test"))),
      cleanup(teardown_temp_store(State)),
      throws(error(rebase_commit_application_failed(schema_validation_error(Failure_Commit_Id,_),[Failure_Commit_Id]), _))
     ])
:-
    Master_Path = "admin/test",
    resolve_absolute_string_descriptor(Master_Path, Master_Descriptor),
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit a"}, Master_Context1),
    City_Uri_String = "City/Dublin",

    Document = _{'@type': "City",
                 '@id' : City_Uri_String,
                 'name': "Dublin"
                },
    with_transaction(Master_Context1,
                     insert_document(Master_Context1,Document,_),
                     _
                    ),

    Repository_Descriptor = (Master_Descriptor.repository_descriptor),

    Second_Path = "admin/test/local/branch/second",
    super_user_authority(Auth),
    branch_create(system_descriptor{}, Auth, Second_Path, branch(Master_Path), _),
    resolve_absolute_string_descriptor(Second_Path, Second_Descriptor),

    % create a commit on the master branch, diverging history
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit b"}, Master_Context3_),
    context_extend_prefixes(Master_Context3_, _{worldOnt: "http://example.com/schema/worldOntology#"}, Master_Context3),

    Document2 = _{'@type': "City",
                  '@id': City_Uri_String,
                  'name': "Dubhlinn"
                 },

    with_transaction(Master_Context3,
                     replace_document(Master_Context3,Document2),
                     _),

    branch_head_commit(Repository_Descriptor, "main", Failure_Commit_Uri),
    commit_id_uri(Repository_Descriptor, Failure_Commit_Id, Failure_Commit_Uri),

    % create a commit on the second branch, diverging history
    create_context(Second_Descriptor, commit_info{author:"test",message:"commit b"}, Second_Context_),
    context_extend_prefixes(Second_Context_, _{worldOnt: "http://example.com/schema/worldOntology#"}, Second_Context),

    Document3 = _{'@type': "City",
                  '@id': City_Uri_String,
                  'name': "Baile Átha Cliath"
                 },

    with_transaction(Second_Context,
                     replace_document(Second_Context,Document3),
                     _),

    % rebase time!
    super_user_authority(Auth),


    rebase_on_branch(system_descriptor{}, Auth, Master_Path, Second_Path, "rebaser", [], _Common_Commit_Id, _Their_Commit_Ids, _Reports).

test(rebase_fast_forward_from_nothing,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "foo"))),
      cleanup(teardown_temp_store(State))
     ])
:-
    Main_Path = "admin/foo",
    Second_Path = "admin/foo/local/branch/second",
    super_user_authority(Auth),
    branch_create(system_descriptor{}, Auth, Second_Path, branch(Main_Path), _),

    resolve_absolute_string_descriptor(Second_Path, Second_Descriptor),

    create_context(Second_Descriptor, commit_info{author:"test",message:"commit b"}, Second_Context1),
    with_transaction(Second_Context1,
                     ask(Second_Context1,
                         (   insert(d,e,f),
                             delete(a,b,c))),
                     _),

    descriptor_commit_id_uri((Second_Descriptor.repository_descriptor),
                             Second_Descriptor,
                             Commit_Id1,
                             _Commit_Uri1),

    create_context(Second_Descriptor, commit_info{author:"test",message:"commit c"}, Second_Context2),
    with_transaction(Second_Context2,
                     ask(Second_Context2,
                         insert(g,h,i)),
                     _),

    super_user_authority(Auth),

    descriptor_commit_id_uri((Second_Descriptor.repository_descriptor),
                             Second_Descriptor,
                             Commit_Id2,
                             _Commit_Uri2),

    atomic_list_concat(['admin/foo/local/commit/',Commit_Id2], Commit_Path),

    rebase_on_branch(system_descriptor{}, Auth, Main_Path, Commit_Path, "rebaser",  [], Optional_Common_Commit_Id, Their_Applied_Commit_Ids, []),

    Optional_Common_Commit_Id = none,
    [Commit_Id1,Commit_Id2] = Their_Applied_Commit_Ids.

test(rebase_conflict,
     [
         setup((setup_temp_store(State),
                create_db_with_empty_schema("admin", "foo"),
                resolve_absolute_string_descriptor("admin/foo", Desc))),
         cleanup(teardown_temp_store(State)),
         error(rebase_commit_application_failed(
                   schema_validation_error(
                       _,
                       [witness{'@type':instance_not_cardinality_one,
                                class:'http://www.w3.org/2001/XMLSchema#string',
                                instance:'http://somewhere.for.now/document/User/Document1',
                                predicate:'http://somewhere.for.now/schema#name'}]),
                   _),
               _)
     ])
:-

    Schema = _{ '@id' : "User",
                '@type' : "Class",
                name : "xsd:string" },

    create_context(Desc, commit{author: "me", message: "something"}, Schema_Context),
    with_transaction(
        Schema_Context,
        insert_schema_document(Schema_Context, Schema),
        _
    ),


    Document1 = _{ '@id' : "User/Document1",
                   '@type' : "User",
                   name : "Document1"},

    create_context(Desc, commit{author: "me", message: "something"}, Context),
    with_transaction(
        Context,
        insert_document(Context, Document1, _),
        _),

    super_user_authority(Auth),
    Path = "admin/foo/local/branch/moo",
    branch_create(system_descriptor{}, Auth, Path, branch("admin/foo"), _),

    resolve_absolute_string_descriptor(Path, Desc2),
    create_context(Desc2, commit{author: "me", message: "something"}, Context2),

    Document2 = _{ '@id' : "User/Document1",
                   '@type' : "User",
                   name : "Document1 is changed X"},

    with_transaction(
        Context2,
        replace_document(Context2, Document2, _),
        _),


    Document3 = _{ '@id' : "User/Document1",
                   '@type' : "User",
                   name : "Document1 is changed Y"},

    create_context(Desc, commit{author: "me", message: "something"}, Context3),
    with_transaction(
        Context3,
        replace_document(Context3, Document3, _),
        _),

    rebase_on_branch(system_descriptor{}, Auth, "admin/foo", Path, "me", [], _Common_Commit_Id, _Their_Commit_Ids, _Reports).


:- end_tests(rebase).
