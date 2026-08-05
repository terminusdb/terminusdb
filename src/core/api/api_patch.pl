:- module(api_patch, [
              api_patch_resource/7,
              api_patch/6,
              api_diff/6,
              api_diff_id/8,
              api_diff_id_document/8,
              api_diff_all_documents/7,
              api_apply_squash_commit/7
          ]).

:- use_module(core(util)).
:- use_module(core(document)).
:- use_module(core(account)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(api/api_document), [
                  idlists_duplicates_toplevel/3,
                  nonground_captures/2
              ]).
:- use_module(core(document/apply), [apply_diff_ids_captures/7]).
:- use_module(core(document/history),
              [changed_document_id/2,
               commits_changed_id/5
              ]).
:- use_module(library(solution_sequences)).
:- use_module(library(lists)).
:- use_module(library(plunit)).
:- use_module(library(option)).
:- use_module(library(error), [must_be/2]).
:- use_module(library(assoc)).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).
:- use_module(library(yall)).

api_patch(_System_DB, _Auth, Patch, Before, After, Options) :-
    % no auth yet.
    simple_patch(Patch,Before,After,Options).

patch_id(Patch, Id) :-
    get_dict('@insert', Patch, Insert_Doc), !,
    (   get_dict('@id', Insert_Doc, Id)
    ->  true
    ;   get_dict('@capture', Insert_Doc, Capture)
    ->  atom_string(Id, Capture)
    ;   throw(error(no_id_in_patch(Patch), _))
    ).
patch_id(Patch, Id) :-
    get_dict('@delete', Patch, Delete_Candidate), !,
    (   is_dict(Delete_Candidate)
    ->  do_or_die(get_dict('@id', Delete_Candidate, Id),
                  error(no_id_in_patch(Patch), _))
    ;   (string(Delete_Candidate) ; atom(Delete_Candidate))
    ->  Delete_Candidate = Id
    ;   throw(error(no_id_in_patch(Patch), _))
    ).
patch_id(Patch, Id) :-
    do_or_die(
        get_dict('@id', Patch, Id),
        error(no_id_in_patch(Patch), _)
    ).

patch_id_pairs(Patch, Patch_And_Ids) :-
    (   is_list(Patch)
    ->  findall(P-Id,
                (
                    member(P, Patch),
                    patch_id(P, Id)
                ),
                Patch_And_Ids)
    ;   patch_id(Patch, Id),
        Patch_And_Ids = [Patch-Id]
    ).

map_apply_captures(Context,Options,Patch_And_Ids,Conflicts,Ids_List,Empty,Captures) :-
    mapm({Context, Options}/[Patch-_,Conflict,Ids,C1,C2]>>(
             apply_diff_ids_captures(Context, Patch, Conflict, Ids, Options, C1, C2)
         ),
         Patch_And_Ids,
         Conflicts,
         Ids_List,
         Empty,
         Captures
        ).

api_patch_resource(System_DB, Auth, Path, Patch, Commit_Info, Ids, Options) :-
    resolve_descriptor_auth(write, System_DB, Auth, Path, instance, Branch_Descriptor),
    create_context(Branch_Descriptor, Commit_Info, Context),
    merge_options(Options, options{keep:json{'@id':true, '@type':true}}, Merged_Options),
    empty_assoc(Empty),
    with_transaction(
        Context,
        (
            patch_id_pairs(Patch, Patch_Ids),
            map_apply_captures(Context,Merged_Options,Patch_Ids,Conflicts,Ids_List,Empty,Captures),
            !,
            die_if(nonground_captures(Captures, Nonground),
                   error(not_all_captures_found(Nonground), _)),

            exclude([null]>>true, Conflicts, Witnesses),
            (   Witnesses = []
            ->  true
            ;   throw(error(patch_conflicts(Witnesses)))
            ),
            exclude(=([]), Ids_List, Non_Empty_Ids_List),
            idlists_duplicates_toplevel(Non_Empty_Ids_List, Duplicates, Ids),
            die_if(Duplicates \= [], error(same_ids_in_one_transaction(Duplicates), _))
        ),
        _
    ).

api_diff(_System_DB, _Auth, Before, After, Diff, Options) :-
    % no auth yet.
    do_or_die((ground(Before),
               ground(After)),
              error(unground_patch, _)),
    simple_diff(Before,After,Diff,Options).

coerce_to_commit(Branch_Descriptor, Commit_Or_Version, Commit_Id) :-
    do_or_die(
        (   read_data_version(Commit_Or_Version, data_version(Type, Commit_Id))
        ->  do_or_die(
                (   branch = Type
                ;   commit = Type),
                error(bad_data_version(Commit_Or_Version), _)
            )
        ;   resolve_relative_descriptor(Branch_Descriptor,
                                        ["_commits"],
                                        Ref_Descriptor)
        ->  open_descriptor(Ref_Descriptor, T),
            (   commit_id_uri(T,
                              Commit_Or_Version,
                              _)
            ->  Commit_Id = Commit_Or_Version
            ;   branch_head_commit(T,
                                   Commit_Or_Version,
                                   Commit_Uri),
                commit_id_uri(T, Commit_Id, Commit_Uri))
        ;   Commit_Id = Commit_Or_Version
        ),
        error(not_a_valid_commit_or_branch(Commit_Or_Version), _)
    ).

document_from_commit(Branch_Descriptor, Commit_Id, Doc_Id, Document, Transaction,
                     Map_In, Map_Out) :-
    document_from_commit(Branch_Descriptor, Commit_Id, Doc_Id, Document, Transaction,
                         Map_In, Map_Out, options{}).

document_from_commit(Branch_Descriptor, Commit_Id, Doc_Id, Document, Transaction,
                     Map_In, Map_Out, Options) :-
    resolve_relative_descriptor(Branch_Descriptor,
                                ["commit", Commit_Id],
                                Commit_Descriptor),

    do_or_die(
        open_descriptor(Commit_Descriptor, commit_info{}, Transaction, Map_In, Map_Out),
        error(unresolvable_collection(Commit_Descriptor),_)),

    option(unfold(Unfold), Options, true),
    must_be(boolean, Unfold),

    Get_Document_Options = options{
                  compress_ids : true,
                  unfold: Unfold,
                  keep_json_type: true
              },
    get_document(Transaction, Doc_Id, Document, Get_Document_Options).

api_diff_id(System_DB, Auth, Path, Before_Version, After_Version, Doc_Id, Diff, Options) :-
    resolve_descriptor_auth(read, System_DB, Auth, Path, instance, Branch_Descriptor),
    coerce_to_commit(Branch_Descriptor, Before_Version, Before_Commit_Id),
    coerce_to_commit(Branch_Descriptor, After_Version, After_Commit_Id),

    (   document_from_commit(Branch_Descriptor, Before_Commit_Id, Doc_Id, Before, _, [], Map, Options)
    ->  (   document_from_commit(Branch_Descriptor, After_Commit_Id, Doc_Id, After, _, Map, _, Options)
        ->  simple_diff(Before,After,Diff,Options)
        ;   Diff = json{ '@op' : 'Delete',
                         '@delete' : Before }
        )
    ;   (   document_from_commit(Branch_Descriptor, After_Commit_Id, Doc_Id, After, _, [], _, Options)
        ->  Diff = json{ '@op' : 'Insert',
                         '@insert' : After }
        ;   fail)
    ).

api_diff_id_document(System_DB, Auth, Path, Before_Version, After_Document, Doc_Id, Diff, Options) :-
    resolve_descriptor_auth(read, System_DB, Auth, Path, instance, Branch_Descriptor),
    coerce_to_commit(Branch_Descriptor, Before_Version, Before_Commit_Id),

    document_from_commit(Branch_Descriptor, Before_Commit_Id, Doc_Id, Before, Transaction, [], _, Options),

    normalize_document(Transaction, After_Document, Normal_Document),
    simple_diff(Before,Normal_Document,Diff,Options).

commits_changed_id(Branch_Descriptor, Before_Commit_Id, After_Commit_Id, Changed) :-
    commits_changed_id(Branch_Descriptor, Before_Commit_Id, After_Commit_Id, Changed, options{}).

document_diffs_from_commits(Branch_Descriptor, Before_Commit_Id, After_Commit_Id, Diff, Options) :-
    option(start(Start), Options),
    option(count(Count), Options),
    limit(
        Count,
        offset(
            Start,
            (   commits_changed_id(Branch_Descriptor, Before_Commit_Id, After_Commit_Id, Doc_Id),
                (   document_from_commit(Branch_Descriptor, Before_Commit_Id, Doc_Id, Before, _, [], Map, Options)
                ->  (   document_from_commit(Branch_Descriptor, After_Commit_Id, Doc_Id, After, _, Map, _, Options)
                    ->  simple_diff(Before,After,Diff,Options)
                    ;   Diff = json{ '@op' : 'Delete',
                                     '@delete' : Before }
                    )
                ;   (   document_from_commit(Branch_Descriptor, After_Commit_Id, Doc_Id, After, _, [], _, Options)
                    ->  Diff = json{ '@op' : 'Insert',
                                     '@insert' : After }
                    ;   fail)
                ),
                \+ patch_cost(Diff, 0)
            )
        )
    ).

api_diff_all_documents(System_DB, Auth, Path, Before_Version, After_Version, Diffs, Options) :-
    resolve_descriptor_auth(read, System_DB, Auth, Path, instance, Branch_Descriptor),
    coerce_to_commit(Branch_Descriptor, Before_Version, Before_Commit_Id),
    coerce_to_commit(Branch_Descriptor, After_Version, After_Commit_Id),
    findall(Diff,
            document_diffs_from_commits(Branch_Descriptor,
                                        Before_Commit_Id,
                                        After_Commit_Id,
                                        Diff,
                                        Options),
            Diffs
           ).

api_apply_squash_commit(System_DB, Auth, Path, Commit_Info, Before_Version, After_Version, Options) :-
    resolve_descriptor_auth(write, System_DB, Auth, Path, instance, Branch_Descriptor),
    coerce_to_commit(Branch_Descriptor, Before_Version, Before_Commit_Id),
    coerce_to_commit(Branch_Descriptor, After_Version, After_Commit_Id),
    create_context(Branch_Descriptor, Commit_Info, Context),
    merge_options(Options, options{keep:json{'@id':true, '@type':true},
                                   count:inf,
                                   start:0}, Merged_Options),
    with_transaction(
        Context,
        (   findall(Witness,
                    (   document_diffs_from_commits(Branch_Descriptor,
                                                    Before_Commit_Id,
                                                    After_Commit_Id,
                                                    Diff,
                                                    Merged_Options),
                        apply_diff(Context,Diff,Witness,Merged_Options),
                        \+ Witness = null
                    ),
                    Witnesses),
            (   Witnesses = []
            ->  true
            ;   throw(error(apply_squash_witnesses(Witnesses)))
            )
        ),
        _
    ).


:- begin_tests(apply).

:- use_module(core(util/test_utils)).
:- use_module(core(document)).
:- use_module(core(triple)).
:- use_module(core(api/api_document)).
:- use_module(library(json)).

test(delete_missing,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Author = "gavin",
    Message = "test.",
    Graph_Type = instance,
    open_string('{"@type" : "City", "name" : "Warsaw"}', Stream),
    Options0 = [graph_type(Graph_Type),
                author(Author),
                message(Message),
                full_replace(false),
                json(false)],
    api_insert_documents(SystemDB, Auth, Path, Stream, no_data_version, Data_Version1, _Transaction_Meta_Data1, Ids, Options0),

    atom_json_dict(Id_Atom, Ids, []),
    open_string(Id_Atom, Stream2),

    Options1 = [graph_type(Graph_Type),
                author(Author),
                message(Message)],
    api_delete_documents(SystemDB, Auth, Path, Stream2, Data_Version1, Data_Version2, _Transaction_Meta_Data2, _Ids, Options1),

    Commit_Info = commit_info{ author : Author, message: Message },
    Options2 = [match_final_state(true)],
    Data_Version1 = data_version(branch,Commit1),
    Data_Version2 = data_version(branch,Commit2),
    % Matches because we have Options1
    api_apply_squash_commit(SystemDB, Auth, Path, Commit_Info, Commit1, Commit2, Options2),
    % Does not match because we have Options2
    Options3 = [match_final_state(false)],
    catch(
        api_apply_squash_commit(SystemDB, Auth, Path, Commit_Info, Commit1, Commit2, Options3),
        E,
        E = error(apply_squash_witnesses([json{'@id_does_not_exists':_,
                                               '@op':'DeleteConflict'}]))
    ).

test(insert_twice,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Author = "gavin",
    Message = "test.",
    Graph_Type = instance,
    Options = [graph_type(Graph_Type),
               author(Author),
               message(Message),
               full_replace(false),
               json(false)],
    open_string('{"@type" : "City", "name" : "Warsaw"}', Stream1),
    api_insert_documents(SystemDB, Auth, Path, Stream1, no_data_version, Data_Version1, _Transaction_Meta_Data1, _Ids1, Options),
    open_string('{"@type" : "City", "name" : "Dublin"}', Stream2),
    api_insert_documents(SystemDB, Auth, Path, Stream2, Data_Version1, Data_Version2, _Transaction_Meta_Data2, _Ids2, Options),

    Commit_Info = commit_info{ author : Author, message: Message },
    Options1 = [match_final_state(true)],
    Data_Version1 = data_version(branch,Commit1),
    Data_Version2 = data_version(branch,Commit2),
    % Matches because we have Options1
    api_apply_squash_commit(SystemDB, Auth, Path, Commit_Info, Commit1, Commit2, Options1),
    % Does not match because we have Options2
    Options2 = [match_final_state(false)],
    catch(
        api_apply_squash_commit(SystemDB, Auth, Path, Commit_Info, Commit1, Commit2, Options2),
        E,
        E = error(apply_squash_witnesses(
                      [json{'@id_already_exists':_,
                            '@op':'InsertConflict'}]))
    ).

:- end_tests(apply).

:- begin_tests(patch_resource).

:- use_module(core(util/test_utils)).
:- use_module(core(document)).
:- use_module(core(triple)).
:- use_module(core(api/api_document)).
:- use_module(library(json)).

test(patch_resource_insert,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Commit_Info = commit_info{ author : "test", message: "insert via patch" },
    Patch = [json{ '@op':'Insert',
                    '@insert':json{ '@id':"City/Leipzig",
                                    '@type':"City",
                                    name:"Leipzig" }}],
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, _Ids, []),
    resolve_absolute_string_descriptor(Path, Desc),
    open_descriptor(Desc, T),
    get_document(T, 'City/Leipzig', Doc),
    get_dict(name, Doc, "Leipzig").

test(patch_resource_delete,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Graph_Type = instance,
    Options = [graph_type(Graph_Type),
               author("test"),
               message("setup")],
    open_string('{"@type" : "City", "name" : "Warsaw"}', Stream),
    api_insert_documents(SystemDB, Auth, Path, Stream, no_data_version, _DV1, Inserted_Ids, Options),
    Inserted_Ids = [City_Id],
    Commit_Info = commit_info{ author : "test", message: "delete via patch" },
    Patch = [json{ '@op':'Delete',
                    '@delete':json{ '@id':City_Id }}],
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, _Ids, []),
    resolve_absolute_string_descriptor(Path, Desc),
    open_descriptor(Desc, T),
    \+ get_document(T, City_Id, _).

test(patch_resource_delete_string,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Graph_Type = instance,
    Options = [graph_type(Graph_Type),
               author("test"),
               message("setup")],
    open_string('{"@type" : "City", "name" : "Warsaw"}', Stream),
    api_insert_documents(SystemDB, Auth, Path, Stream, no_data_version, _DV1, Inserted_Ids, Options),
    Inserted_Ids = [City_Id],
    Commit_Info = commit_info{ author : "test", message: "delete via patch string" },
    Patch = [json{ '@op':'Delete',
                    '@delete':City_Id }],
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, _Ids, []),
    resolve_absolute_string_descriptor(Path, Desc),
    open_descriptor(Desc, T),
    \+ get_document(T, City_Id, _).

test(patch_resource_mixed,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Graph_Type = instance,
    Options = [graph_type(Graph_Type),
               author("test"),
               message("setup")],
    open_string('{"@type" : "City", "name" : "Warsaw"}', Stream),
    api_insert_documents(SystemDB, Auth, Path, Stream, no_data_version, _DV1, [City_Id], Options),
    Commit_Info = commit_info{ author : "test", message: "mixed patch" },
    Patch = [json{ '@op':'Delete',
                    '@delete':json{ '@id':City_Id }},
             json{ '@op':'Insert',
                    '@insert':json{ '@id':"City/Berlin",
                                    '@type':"City",
                                    name:"Berlin" }}],
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, _Ids, []),
    resolve_absolute_string_descriptor(Path, Desc),
    open_descriptor(Desc, T),
    get_document(T, 'City/Berlin', Berlin),
    get_dict(name, Berlin, "Berlin"),
    \+ get_document(T, City_Id, _).

test(patch_resource_insert_conflict,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State)),
      throws(error(patch_conflicts([json{'@id_already_exists':_,
                                          '@op':'InsertConflict'}])))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Graph_Type = instance,
    Options = [graph_type(Graph_Type),
               author("test"),
               message("setup")],
    open_string('{"@type" : "City", "name" : "Warsaw"}', Stream),
    api_insert_documents(SystemDB, Auth, Path, Stream, no_data_version, _DV1, [City_Id], Options),
    Commit_Info = commit_info{ author : "test", message: "insert conflict" },
    Patch = [json{ '@op':'Insert',
                    '@insert':json{ '@id':City_Id,
                                    '@type':"City",
                                    name:"Warsaw" }}],
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, _Ids, []).

test(patch_resource_delete_conflict,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State)),
      throws(error(patch_conflicts([json{'@id_does_not_exists':_,
                                         '@op':'DeleteConflict'}])))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Commit_Info = commit_info{ author : "test", message: "delete conflict" },
    Patch = [json{ '@op':'Delete',
                    '@delete':json{ '@id':"City/Nonexistent" }}],
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, _Ids, []).

test(patch_resource_insert_match_final_state,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Graph_Type = instance,
    Options = [graph_type(Graph_Type),
               author("test"),
               message("setup")],
    open_string('{"@type" : "City", "name" : "Warsaw"}', Stream),
    api_insert_documents(SystemDB, Auth, Path, Stream, no_data_version, _DV1, [City_Id], Options),
    Commit_Info = commit_info{ author : "test", message: "insert match final state" },
    Patch = [json{ '@op':'Insert',
                    '@insert':json{ '@id':City_Id,
                                    '@type':"City",
                                    name:"Warsaw" }}],
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, _Ids,
                       [match_final_state(true)]).

test(patch_resource_delete_match_final_state,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Commit_Info = commit_info{ author : "test", message: "delete match final state" },
    Patch = [json{ '@op':'Delete',
                    '@delete':json{ '@id':'City/Nonexistent' }}],
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, _Ids,
                       [match_final_state(true)]).

test(patch_resource_mixed_conflict,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State)),
      throws(error(patch_conflicts(_)))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Commit_Info = commit_info{ author : "test", message: "mixed conflict" },
    Patch = [json{ '@op':'Insert',
                    '@insert':json{ '@id':"City/Berlin",
                                    '@type':"City",
                                    name:"Berlin" }},
             json{ '@op':'Delete',
                    '@delete':json{ '@id':"City/Nonexistent" }}],
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, _Ids, []).

test(patch_resource_match_final_state_empty_ids,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Commit_Info = commit_info{ author : "test", message: "match final state empty ids" },
    Patch = [json{ '@op':'Delete',
                    '@delete':json{ '@id':"City/Nonexistent" }}],
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, _Ids,
                       [match_final_state(true)]).

test(patch_resource_insert_no_id_error,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_test_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State)),
      throws(error(no_id_in_patch(_), _))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Commit_Info = commit_info{ author : "test", message: "no id" },
    Patch = [json{ '@op':'Insert',
                    '@insert':json{ '@type':"City",
                                    name:"NoID" }}],
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, _Ids, []).

test(patch_resource_insert_with_captures,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_empty_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Schema = json{ '@type':"Class", '@id':"Person", name:"xsd:string", friend:"Person" },
    Commit_Info_Schema = commit_info{ author : "test", message: "add schema" },
    resolve_absolute_string_descriptor(Path, Schema_Desc),
    create_context(Schema_Desc, Commit_Info_Schema, Schema_Context),
    with_transaction(Schema_Context, insert_schema_document(Schema_Context, Schema), _),
    Commit_Info = commit_info{ author : "test", message: "captures insert" },
    Patch = [json{ '@op':'Insert',
                    '@insert':json{ '@type':"Person",
                                    '@capture':"PersonA",
                                    name:"Alice",
                                    friend:json{'@ref':"PersonB"}}},
             json{ '@op':'Insert',
                    '@insert':json{ '@type':"Person",
                                    '@capture':"PersonB",
                                    name:"Bob",
                                    friend:json{'@ref':"PersonA"}}}],
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, Ids, []),
    length(Ids, 2),
    resolve_absolute_string_descriptor(Path, Desc),
    open_descriptor(Desc, T),
    findall(Name,
            (   member(Id, Ids),
                get_document(T, Id, Doc),
                get_dict(name, Doc, Name)
            ),
            Names),
    memberchk("Alice", Names),
    memberchk("Bob", Names).

test(patch_resource_unmatched_capture_fails,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_empty_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State)),
      throws(error(not_all_captures_found(_), _))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Schema = json{ '@type':"Class", '@id':"Person", name:"xsd:string", friend:"Person" },
    Commit_Info_Schema = commit_info{ author : "test", message: "add schema" },
    resolve_absolute_string_descriptor(Path, Schema_Desc),
    create_context(Schema_Desc, Commit_Info_Schema, Schema_Context),
    with_transaction(Schema_Context, insert_schema_document(Schema_Context, Schema), _),
    Commit_Info = commit_info{ author : "test", message: "unmatched capture" },
    Patch = [json{ '@op':'Insert',
                    '@insert':json{ '@type':"Person",
                                    '@capture':"PersonA",
                                    name:"Alice",
                                    friend:json{'@ref':"Nonexistent"}}}],
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, _Ids, []).

test(patch_resource_insert_with_id_and_capture,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_empty_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Schema = json{ '@type':"Class", '@id':"Person", name:"xsd:string", friend:"Person" },
    Commit_Info_Schema = commit_info{ author : "test", message: "add schema" },
    resolve_absolute_string_descriptor(Path, Schema_Desc),
    create_context(Schema_Desc, Commit_Info_Schema, Schema_Context),
    with_transaction(Schema_Context, insert_schema_document(Schema_Context, Schema), _),
    Commit_Info = commit_info{ author : "test", message: "id and capture" },
    Patch = [json{ '@op':'Insert',
                    '@insert':json{ '@id':"Person/Alice",
                                    '@type':"Person",
                                    '@capture':"PersonA",
                                    name:"Alice",
                                    friend:json{'@ref':"PersonB"}}},
             json{ '@op':'Insert',
                    '@insert':json{ '@id':"Person/Bob",
                                    '@type':"Person",
                                    '@capture':"PersonB",
                                    name:"Bob",
                                    friend:json{'@ref':"PersonA"}}}],
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, Ids, []),
    length(Ids, 2),
    resolve_absolute_string_descriptor(Path, Desc),
    open_descriptor(Desc, T),
    findall(Name,
            (   member(Id, Ids),
                get_document(T, Id, Doc),
                get_dict(name, Doc, Name)
            ),
            Names),
    length(Names, 2),
    memberchk(Name_A, Names),
    (   atom_string(Name_A, "Alice")
    ->  true
    ;   atom_string(Name_A, "Bob")
    ->  true
    ;   fail
    ).

test(patch_resource_field_patch_with_captures,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_empty_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Schema = json{ '@type':"Class", '@id':"Person", name:"xsd:string", friend:"Person" },
    Commit_Info_Schema = commit_info{ author : "test", message: "add schema" },
    resolve_absolute_string_descriptor(Path, Schema_Desc),
    create_context(Schema_Desc, Commit_Info_Schema, Schema_Context),
    with_transaction(Schema_Context, insert_schema_document(Schema_Context, Schema), _),
    Commit_Info = commit_info{ author : "test", message: "field patch with captures" },
    Patch = [json{ '@op':'Insert',
                    '@insert':json{ '@id':"Person/Alice",
                                    '@type':"Person",
                                    '@capture':"PersonA",
                                    name:"Alice",
                                    friend:json{'@ref':"PersonB"}}},
             json{ '@op':'Insert',
                    '@insert':json{ '@id':"Person/Bob",
                                    '@type':"Person",
                                    '@capture':"PersonB",
                                    name:"Bob",
                                    friend:json{'@ref':"PersonA"}}},
             json{ '@op':'Insert',
                    '@insert':json{ '@id':"Person/Charlie",
                                    '@type':"Person",
                                    name:"Charlie",
                                    friend:json{'@ref':"PersonA"}}}],
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, Ids, []),
    Field_Commit = commit_info{ author : "test", message: "field patch" },
    Field_Patch = [json{ '@id':"Person/Charlie",
                         name:json{'@op':'SwapValue',
                                   '@before':"Charlie",
                                   '@after':"Charles"}}],
    api_patch_resource(SystemDB, Auth, Path, Field_Patch, Field_Commit, _, []),
    resolve_absolute_string_descriptor(Path, Desc),
    open_descriptor(Desc, T),
    findall(Name,
            (   member(Id, Ids),
                get_document(T, Id, Doc),
                get_dict(name, Doc, Name)
            ),
            Names),
    length(Names, 3),
    get_document(T, 'Person/Charlie', Doc_C),
    get_dict(name, Doc_C, Name_C),
    atom_string(Name_C, "Charles").

test(patch_resource_field_patch_swapvalue_with_ref,
     [setup((setup_temp_store(State),
             random_string(X),
             string_concat("admin/",X, Path),
             create_db_with_empty_schema("admin", X)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),
    Schema = json{ '@type':"Class", '@id':"Person", name:"xsd:string", friend:json{'@type':"Optional", '@class':"Person"} },
    Commit_Info_Schema = commit_info{ author : "test", message: "add schema" },
    resolve_absolute_string_descriptor(Path, Schema_Desc),
    create_context(Schema_Desc, Commit_Info_Schema, Schema_Context),
    with_transaction(Schema_Context, insert_schema_document(Schema_Context, Schema), _),
    Setup_Commit = commit_info{ author : "test", message: "insert Charlie" },
    Setup_Patch = [json{ '@op':'Insert',
                         '@insert':json{ '@id':"Person/Charlie",
                                         '@type':"Person",
                                         name:"Charlie"}}],
    api_patch_resource(SystemDB, Auth, Path, Setup_Patch, Setup_Commit, _, []),
    Patch = [json{ '@op':'Insert',
                   '@insert':json{ '@id':"Person/Alice",
                                   '@type':"Person",
                                   '@capture':"PersonA",
                                   name:"Alice",
                                   friend:json{'@ref':"PersonB"}}},
             json{ '@op':'Insert',
                   '@insert':json{ '@id':"Person/Bob",
                                   '@type':"Person",
                                   '@capture':"PersonB",
                                   name:"Bob",
                                   friend:json{'@ref':"PersonA"}}},
             json{ '@id':"Person/Charlie",
                   friend:json{'@op':'SwapValue',
                               '@before':null,
                               '@after':json{'@ref':"PersonA"}}}],
    Commit_Info = commit_info{ author : "test", message: "insert and swap Charlie friend with ref" },
    api_patch_resource(SystemDB, Auth, Path, Patch, Commit_Info, _, []),
    resolve_absolute_string_descriptor(Path, Desc),
    open_descriptor(Desc, T),
    get_document(T, 'Person/Charlie', Doc_C),
    get_dict(friend, Doc_C, Friend_C),
    atom_string(Friend_C, "Person/Alice").

:- end_tests(patch_resource).
