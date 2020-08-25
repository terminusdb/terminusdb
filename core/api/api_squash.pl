:- module(api_squash, [api_squash/6]).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(account)).
:- use_module(library(terminus_store)).
:- use_module(core(util/test_utils)).

% Take a path, squash and return a reference to the
% Ref.
%
% This should split on the four types of graph we are concerned with
% branch/ref, commit, labelled (system or repo)
%
api_squash(System_DB, Auth, Path, Commit_Info, Ref, Old_Ref) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_absolute_path(Path))),

    % Note: Check some sort of permissions here... but what?
    do_or_die(
        open_descriptor(Descriptor,_),
        error(not_a_valid_descriptor(Descriptor),_)),

    api_squash_descriptor(Descriptor, System_DB, Auth, Commit_Info, Ref, Old_Ref).

/* api_squash_descriptor(+Descriptor, +System_DB,
 *                       +Auth, +Commit_Info,
 *                       -Ref,-Old_Ref) is semidet.
 */
api_squash_descriptor(Descriptor,
                      _, _, _,
                      Ref, Old_Ref) :-
    (   system_descriptor{} :< Descriptor
    ;   database_descriptor{} :< Descriptor
    ;   repository_descriptor{} :< Descriptor),
    !,
    open_descriptor(Descriptor, Transaction_Object),
    [Instance] = (Transaction_Object.instance_objects),
    Layer = (Instance.read_write_obj),
    squash(Layer,New_Layer),
    layer_to_id(Layer,Old_Ref_Id),
    layer_to_id(New_Layer,Ref_Id),
    resolve_absolute_string_descriptor(String,Descriptor),
    atomic_list_concat([String,'/commit/',Ref_Id], Ref),
    atomic_list_concat([String,'/commit/',Old_Ref_Id], Old_Ref).
api_squash_descriptor(Descriptor,
                      System_DB, Auth, Commit_Info,
                      Ref, Old_Ref) :-
    Descriptor = branch_descriptor{
                     repository_descriptor: Repository_Descriptor,
                     branch_name : Branch_Name},
    !,

    Repository_Descriptor = repository_descriptor{
                                database_descriptor:
                                database_descriptor{
                                    database_name: Database_Name,
                                    organization_name: Organization_Name
                                },
                                repository_name: Repository_Name
                            },
    open_descriptor(Descriptor, Transaction_Object),

    askable_context(Repository_Descriptor, System_DB, Auth, Commit_Info, Context),

    %print_all_triples(Repository_Descriptor),
    %lookup_old_commit_uri_here....
    descriptor_commit_id_uri(Repository_Descriptor, Descriptor,
                             Old_Commit_Id, Old_Commit_Uri),

    with_transaction(
        Context,
        (   insert_base_commit_object(Context, Commit_Info, Commit_Id, Commit_Uri),

            forall(
                graph_for_commit(Repository_Descriptor, Old_Commit_Uri,
                                 Type, Name, Graph_Uri),
                (
                    Graph_Descriptor = branch_graph{
                                           organization_name: Organization_Name,
                                           database_name : Database_Name,
                                           repository_name : Repository_Name,
                                           branch_name : Branch_Name,
                                           type : Type,
                                           name : Name },
                    graph_descriptor_transaction_objects_read_write_object(
                        Graph_Descriptor, [Transaction_Object], RWO),
                    %lookup_rwo_for_graph_type_name...
                    get_dict(read,RWO,Layer),
                    squash(Layer,New_Layer),
                    layer_to_id(New_Layer,Layer_Id),
                    ref_entity:copy_new_graph_object(Repository_Descriptor,
                                                     Context,
                                                     Graph_Uri,
                                                     Old_Commit_Id, New_Graph_Uri),
                    insert_layer_object(Context, Layer_Id, Layer_Uri),
                    ref_entity:attach_layer_to_graph(Context,New_Graph_Uri,Layer_Uri),
                    ref_entity:attach_graph_to_commit(Context,Commit_Uri,Type,Name, New_Graph_Uri)
                )
            )
        ),
        _
    ),

    resolve_relative_descriptor(Repository_Descriptor,
                                ["commit",Commit_Id],
                                Commit_Descriptor),
    resolve_absolute_string_descriptor(Ref,Commit_Descriptor),
    resolve_relative_descriptor(Repository_Descriptor,
                                ["commit",Old_Commit_Id],
                                Old_Commit_Descriptor),
    resolve_absolute_string_descriptor(Old_Ref,Old_Commit_Descriptor).

:- begin_tests(squash).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).

test(squash_branch,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State))]
    ) :-

    Path = "admin/testdb",

    resolve_absolute_string_descriptor(Path,Descriptor),
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

    askable_context(Descriptor, system_descriptor{}, Auth,
                    commit_info{ author : "me",
                                 message : "commit 2" },
                    Context2),

    with_transaction(
        Context2,
        ask(Context2,
            insert(e,f,g)),
        _),

    Commit_Info = commit_info{ author : "me",
                               message : "squash" },

    api_squash(system_descriptor{}, Auth, Path, Commit_Info, Ref, _Old_Ref),

    resolve_absolute_string_descriptor(Ref, Ref_Askable),

    findall(X-Y-Z,
            ask(Ref_Askable,
                t(X,Y,Z)),
            Triples),
    sort(Triples,Sorted),
    sort([a-b-c,e-f-g],Correct),
    ord_seteq(Sorted, Correct).

:- end_tests(squash).
