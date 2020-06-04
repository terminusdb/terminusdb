:- module(db_rebase, [
              rebase_on_branch/8,
              cycle_context/4
          ]).
:- use_module(library(terminus_store)).

:- use_module(core(query)).
:- use_module(core(transaction)).

cycle_context(Context, New_Context, New_Transaction_Object, Validation_Object) :-
    [Transaction_Object] = Context.transaction_objects,
    transaction_objects_to_validation_objects([Transaction_Object], [Validation_Object]),
    validate_validation_objects([Validation_Object], Witnesses),
    (   Witnesses = []
    ->  true
    % put more stuff in error!
    ;   throw(error(schema_validation_error(Witnesses)))),
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

    New_Validation_Object = Validation_Object.put(
                                                  _{
                                                      instance_objects: Instance_Objects,
                                                      schema_objects: Schema_Objects,
                                                      inference_objects: Inference_Objects
                                                  }).

apply_commit_chain(Our_Repo_Context, _Their_Repo_Context, Us_Commit_Uri, _Author, _Auth_Object, [], [], Us_Commit_Uri, Our_Repo_Context, []) :-
    !,
    true.
apply_commit_chain(Our_Repo_Context, Their_Repo_Context, Us_Commit_Uri, Author, Auth_Object, [Their_Commit_Id|Their_Commit_Ids], [Strategy|Strategies], Final_Commit_Uri, Return_Context, [Report|Reports]) :-
    Report = report{
                 origin_commit: Their_Commit_Id,
                 applied: Applied_Commit_Ids,
                 type: Commit_Application_Type
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
            Commit_Application_Type = valid,
            Applied_Commit_Ids = [New_Commit_Id]
        ;   Strategy = continue
        ->  throw(error(apply_commit(continue_on_valid_commit(Their_Commit_Id))))
        ;   Strategy = fixup(_Message,_Woql)
        ->  throw(error(apply_commit(fixup_on_valid_commit(Their_Commit_Id)))))
    ;   (   Strategy = error
        ->  throw(error(apply_commit(schema_validation_error(Their_Commit_Id, Witnesses))))
        ;   Strategy = continue
        ->  invalidate_commit(Our_Repo_Context2, New_Commit_Id),
            cycle_context(Our_Repo_Context2, Our_Repo_Context3, _, _),
            New_Commit_Uri2 = New_Commit_Uri,
            Commit_Application_Type = invalid,
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
            ;   throw(error(apply_commit(fixup_error(Their_Commit_Id, Fixup_Witnesses))))),

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

rebase_on_branch(Our_Branch_Descriptor, Their_Branch_Descriptor, Author, Auth_Object, Strategy_Map, Optional_Common_Commit_Id, Their_Branch_Path, Reports) :-
    Our_Repo_Descriptor = Our_Branch_Descriptor.repository_descriptor,
    Their_Repo_Descriptor = Their_Branch_Descriptor.repository_descriptor,
    create_context(Our_Repo_Descriptor, Our_Repo_Context),
    create_context(Their_Repo_Descriptor, Their_Repo_Context),

    branch_name_uri(Our_Repo_Context, Our_Branch_Descriptor.branch_name, Our_Branch_Uri),
    branch_head_commit(Our_Repo_Context, Our_Branch_Descriptor.branch_name, Our_Commit_Uri),
    commit_id_uri(Our_Repo_Context, Our_Commit_Id, Our_Commit_Uri),
    branch_head_commit(Their_Repo_Context, Their_Branch_Descriptor.branch_name, Their_Commit_Uri),
    commit_id_uri(Their_Repo_Context, Their_Commit_Id, Their_Commit_Uri),

    (   most_recent_common_ancestor(Our_Repo_Context, Their_Repo_Context, Our_Commit_Id, Their_Commit_Id, Common_Commit_Id, Our_Branch_Path, Their_Branch_Path)
    ->  Optional_Common_Commit_Id = some(Common_Commit_Id)
    ;   Optional_Common_Commit_Id = none,
        commit_uri_to_history_commit_ids(Our_Repo_Context, Our_Commit_Uri, Our_Branch_Path),
        commit_uri_to_history_commit_ids(Their_Repo_Context, Their_Commit_Uri, Their_Branch_Path)),

    % copy commits from their repo into ours, ensuring we got the latest
    copy_commits(Their_Repo_Context, Our_Repo_Context, Their_Commit_Id),
    cycle_context(Our_Repo_Context, Our_Repo_Context2, _, _),

    % take their commit uri as the top on which we're gonna apply_commit_chain
    % list of commits to apply is ours since common

    create_strategies(Our_Branch_Path, Strategy_Map, Strategies),

    (   Our_Branch_Path = []
    % yay we're done! All commits are known to us, no need to do a thing
    ->  Semifinal_Context = Our_Repo_Context2,
        Final_Commit_Uri = Their_Commit_Uri
    ;   catch(
            apply_commit_chain(Our_Repo_Context2,
                               Their_Repo_Context,
                               Their_Commit_Uri,
                               Author,
                               Auth_Object,
                               Our_Branch_Path,
                               Strategies,
                               Final_Commit_Uri,
                               Semifinal_Context,
                               Reports),
            error(apply_commit(Error)),
            throw(error(rebase(Error,Our_Branch_Path)))
        )
    ),

    unlink_commit_object_from_branch(Semifinal_Context, Our_Branch_Uri),
    link_commit_object_to_branch(Semifinal_Context, Our_Branch_Uri, Final_Commit_Uri),

    cycle_context(Semifinal_Context, _Final_Context, Transaction_Object, _),

    Repo_Name = Transaction_Object.descriptor.repository_name,
    [Read_Write_Obj] = Transaction_Object.instance_objects,
    Layer = Read_Write_Obj.read,
    layer_to_id(Layer, Layer_Id),
    Database_Transaction_Object = (Transaction_Object.parent),

    update_repository_head(Database_Transaction_Object, Repo_Name, Layer_Id),

    run_transactions([Database_Transaction_Object], _).

:- begin_tests(rebase).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).
:- use_module(core(account)).

:- use_module(db_create).
:- use_module(db_branch).
test(rebase_fast_forward,
     [setup((setup_temp_store(State),
             create_db_without_schema('user|foo','test','a test'))),
      cleanup(teardown_temp_store(State))])
:-
    resolve_absolute_string_descriptor("user/foo", Master_Descriptor),
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit a"}, Master_Context),
    with_transaction(Master_Context,
                     ask(Master_Context,
                         insert(a,b,c)),
                    _),

    branch_create(Master_Descriptor.repository_descriptor, Master_Descriptor, "second", _),
    resolve_absolute_string_descriptor("user/foo/local/branch/second", Second_Descriptor),

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
    rebase_on_branch(Master_Descriptor, Second_Descriptor, "rebaser", Auth, [], some(Common_Commit_Id), Their_Applied_Commit_Ids, []),
    Repo_Descriptor = Master_Descriptor.repository_descriptor,

    commit_id_to_metadata(Repo_Descriptor, Common_Commit_Id, _, "commit a", _),

    [Old_Commit_B_Id, Old_Commit_C_Id] = Their_Applied_Commit_Ids,
    commit_id_to_metadata(Repo_Descriptor, Old_Commit_B_Id, _, "commit b", _),
    commit_id_to_metadata(Repo_Descriptor, Old_Commit_C_Id, _, "commit c", _),

    branch_head_commit(Repo_Descriptor, "master", Commit_C_Uri),
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
             create_db_without_schema('user|foo','test','a test'))),
      cleanup(teardown_temp_store(State))])
:-
    resolve_absolute_string_descriptor("user/foo", Master_Descriptor),
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit a"}, Master_Context1),
    with_transaction(Master_Context1,
                     ask(Master_Context1,
                         insert(a,b,c)),
                    _),

    branch_create(Master_Descriptor.repository_descriptor, Master_Descriptor, "second", _),
    resolve_absolute_string_descriptor("user/foo/local/branch/second", Second_Descriptor),

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
    branch_head_commit(Repo_Descriptor, "master", Old_Commit_D_Uri),
    commit_id_uri(Repo_Descriptor, Old_Commit_D_Id, Old_Commit_D_Uri),

    super_user_authority(Auth),
    rebase_on_branch(Master_Descriptor, Second_Descriptor, "rebaser", Auth, [], some(Common_Commit_Id), Their_Commit_Ids, Reports),

    commit_id_to_metadata(Repo_Descriptor, Common_Commit_Id, _, "commit a", _),

    branch_head_commit(Repo_Descriptor, "master", New_Commit_D_Uri),
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
    [_{origin_commit: Old_Commit_D_Id,
       applied: [New_Commit_D_Id],
       type: valid}] = Reports,

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
             create_db_with_test_schema('user','test'))),
      cleanup(teardown_temp_store(State)),
      throws(error(rebase(schema_validation_error(Failure_Commit_Id,_),[Failure_Commit_Id])))
     ])
:-
    resolve_absolute_string_descriptor("user/test", Master_Descriptor),
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit a"}, Master_Context1_),
    context_extend_prefixes(Master_Context1_, _{worldOnt: "http://example.com/data/worldOntology#"}, Master_Context1),

    Object = _{'@type': "worldOnt:City",
               'worldOnt:name': _{'@type' : "xsd:string",
                                  '@value' : "Dublin"
                                 }
             },
    with_transaction(Master_Context1,
                     ask(Master_Context1,
                         update_object(Object)),
                    _),
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit b"}, Master_Context2_),
    context_extend_prefixes(Master_Context2_, _{worldOnt: "http://example.com/data/worldOntology#"}, Master_Context2),

    once(ask(Master_Context2, t(City_Uri, worldOnt:name, "Dublin"^^xsd:string))),
    format(string(City_Uri_String), "~w", [City_Uri]),

    Repository_Descriptor = (Master_Descriptor.repository_descriptor),
    branch_create(Repository_Descriptor, Master_Descriptor, "second", _),
    resolve_absolute_string_descriptor("user/test/local/branch/second", Second_Descriptor),

    % create a commit on the master branch, diverging history
    create_context(Master_Descriptor, commit_info{author:"test",message:"commit b"}, Master_Context3_),
    context_extend_prefixes(Master_Context3_, _{worldOnt: "http://example.com/data/worldOntology#"}, Master_Context3),

    Object2 = _{'@type': "worldOnt:City",
               '@id': City_Uri_String,
               'worldOnt:name': _{'@type' : "xsd:string",
                                  '@value' : "Dubhlinn"
                                 }
             },

    with_transaction(Master_Context3,
                     ask(Master_Context3,
                         update_object(Object2)
                         ),
                     _),

    branch_head_commit(Repository_Descriptor, "master", Failure_Commit_Uri),
    commit_id_uri(Repository_Descriptor, Failure_Commit_Id, Failure_Commit_Uri),

    % create a commit on the second branch, diverging history
    create_context(Second_Descriptor, commit_info{author:"test",message:"commit b"}, Second_Context_),
    context_extend_prefixes(Second_Context_, _{worldOnt: "http://example.com/data/worldOntology#"}, Second_Context),

    Object3 = _{'@type': "worldOnt:City",
               '@id': City_Uri_String,
               'worldOnt:name': _{'@type' : "xsd:string",
                                  '@value' : "Baile Átha Cliath"
                                 }
             },

    with_transaction(Second_Context,
                     ask(Second_Context,
                         update_object(Object3)),
                    _),

    % rebase time!
    super_user_authority(Auth),

    % this rebase should fail, but it doesn't right now due to failing cardinality check.
    %trace(validate_instance:refute_all_restrictions),
    rebase_on_branch(Master_Descriptor, Second_Descriptor, "rebaser", Auth, [], _Common_Commit_Id, _Their_Commit_Ids, _Reports).

:- end_tests(rebase).
