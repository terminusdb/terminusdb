:- module(api_squash, [api_squash/6]).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(account)).
:- use_module(library(terminus_store)).
:- use_module(core(util/test_utils)).

:- use_module(library(plunit)).

% Take a path, squash and return a reference to the
% Commit.
%
% This should split on the four types of graph we are concerned with
% branch/ref, commit, labelled (system or repo)
%
api_squash(System_DB, Auth, Path, Commit_Info, Commit_Path, Old_Commit_Path) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_absolute_path(Path))),

    do_or_die(
        branch_descriptor{} :< Descriptor,
        error(not_a_branch_descriptor(Descriptor),_)),

    check_descriptor_auth(System_DB, Descriptor, '@schema':'Action/commit_write_access', Auth),

    do_or_die(
        open_descriptor(Descriptor,_),
        error(unresolvable_absolute_descriptor(Descriptor),_)),

    Descriptor = branch_descriptor{
                     repository_descriptor: Repository_Descriptor,
                     branch_name : Branch_Name},
    open_descriptor(Descriptor, Transaction),
    Repository_Descriptor = repository_descriptor{
                                database_descriptor:
                                database_descriptor{
                                    database_name: Database_Name,
                                    organization_name: Organization_Name
                                },
                                repository_name: Repository_Name
                            },

    askable_context(Repository_Descriptor, System_DB, Auth, Commit_Info, Context),

    %print_all_triples(Repository_Descriptor),
    %lookup_old_commit_uri_here....
    descriptor_commit_id_uri(Repository_Descriptor, Descriptor,
                             Old_Commit_Id, _Old_Commit_Uri),

    with_transaction(
        Context,
        (
            squash_commit_layer(Transaction, Organization_Name, Database_Name, Repository_Name, Branch_Name, schema, Schema_Layer_Id),
            insert_layer_object(Context, Schema_Layer_Id, Schema_Layer_Uri),

            ignore((squash_commit_layer(Transaction, Organization_Name, Database_Name, Repository_Name, Branch_Name, instance, Instance_Layer_Id),
                    insert_layer_object(Context, Instance_Layer_Id, Instance_Layer_Uri))),

            insert_base_commit_object(Context, Schema_Layer_Uri, Instance_Layer_Uri, Commit_Info, Commit_Id, _Commit_Uri)
        ),
        _
    ),

    resolve_relative_descriptor(Repository_Descriptor,
                                ["commit",Commit_Id],
                                Commit_Descriptor),
    resolve_absolute_string_descriptor(Commit_Path,Commit_Descriptor),
    resolve_relative_descriptor(Repository_Descriptor,
                                ["commit",Old_Commit_Id],
                                Old_Commit_Descriptor),
    resolve_absolute_string_descriptor(Old_Commit_Path,Old_Commit_Descriptor).

squash_commit_layer(Transaction, Organization_Name, Database_Name, Repository_Name, Branch_Name, Type, Layer_Id) :-
    Graph_Descriptor = branch_graph{
                           organization_name: Organization_Name,
                           database_name : Database_Name,
                           repository_name : Repository_Name,
                           branch_name : Branch_Name,
                           type : Type },
    graph_descriptor_transaction_objects_read_write_object(
        Graph_Descriptor, [Transaction], RWO),
    get_dict(read,RWO,Layer),
    (   ground(Layer)
    ->  squash(Layer,New_Layer),
        layer_to_id(New_Layer,Layer_Id)
    ;   fail
    ).


:- begin_tests(squash).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).

:- use_module(library(ordsets)).

test(squash_branch,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State))
     ]
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

    api_squash(system_descriptor{}, Auth, Path, Commit_Info, Commit_Path, _Old_Commit_Path),

    resolve_absolute_string_descriptor(Commit_Path, Commit_Askable),

    findall(X-Y-Z,
            ask(Commit_Askable,
                t(X,Y,Z)),
            Triples),

    sort(Triples,Sorted),
    sort([a-b-c,e-f-g],Correct),
    ord_seteq(Sorted, Correct),

    open_descriptor(Commit_Askable, Transaction),
    [RWO] = (Transaction.instance_objects),
    Layer = (RWO.read),
    \+ parent(Layer,_).


:- end_tests(squash).
