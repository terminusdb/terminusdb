:- module(ref_entity, [
              has_branch/2,
              branch_name_uri/3,
              branch_head_commit/3,
              has_commit/2,
              commit_id_uri/3,
              commit_id_to_metadata/5,
              commit_uri_to_metadata/5,
              commit_id_to_parent_uri/3,
              commit_uri_to_parent_uri/3,
              descriptor_commit_id_uri/4,
              layer_uri_for_commit/4,
              insert_branch_object/3,
              delete_branch_object/2,
              insert_base_commit_object/3,
              insert_base_commit_object/4,
              insert_base_commit_object/5,
              insert_child_commit_object/4,
              insert_child_commit_object/5,
              insert_child_commit_object/6,
              insert_commit_object_on_branch/4,
              insert_commit_object_on_branch/5,
              insert_commit_object_on_branch/6,
              unlink_commit_object_from_branch/2,
              link_commit_object_to_branch/3,
              reset_branch_head/3,
              copy_commits/3,
              apply_commit_on_branch/7,
              apply_commit_on_branch/8,
              apply_commit_on_commit/7,
              apply_commit_on_commit/8,
              commit_is_valid/2,
              invalidate_commit/2,
              most_recent_common_ancestor/7,
              commit_uri_to_history_commit_ids/3,
              commit_uri_to_history_commit_uris/3,
              update_prefixes/2,
              repository_prefixes/2,
              copy_prefixes/2,
              attach_layer_to_commit/4
          ]).
:- use_module(library(terminus_store)).

:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(document)).

:- use_module(layer_entity).
:- use_module(descriptor).
:- use_module(validate).

has_branch(Askable, Branch_Name) :-
    ground(Branch_Name),
    !,
    once(ask(Askable,
             (
                 t(Branch_Uri, rdf:type, '@schema':'Branch'),
                 t(Branch_Uri, name, Branch_Name^^xsd:string)
             ))).
has_branch(Askable, Branch_Name) :-
    ask(Askable,
        (
            t(Branch_Uri, rdf:type, '@schema':'Branch'),
            t(Branch_Uri, name, Branch_Name^^xsd:string))).

branch_name_uri(Askable, Branch_Name, Branch_Uri) :-
    once(ask(Askable,
             (   t(Branch_Uri, rdf:type, '@schema':'Branch'),
                 t(Branch_Uri, name, Branch_Name^^xsd:string)))).

branch_head_commit(Askable, Branch_Name, Commit_Uri) :-
    branch_name_uri(Askable, Branch_Name, Branch_Uri),
    once(ask(Askable,
             t(Branch_Uri, head, Commit_Uri))).

has_commit(Askable, Commit_Id) :-
    ground(Commit_Id),
    !,
    commit_id_uri(Askable, Commit_Id, _).
has_commit(Askable, Commit_Id) :-
    ask(Askable,
        t(_, identifier, Commit_Id^^xsd:string)).

descriptor_commit_id_uri(Askable, Descriptor, Commit_Id, Commit_Uri) :-
    commit_descriptor{ commit_id : Commit_Id} :< Descriptor,
    !,
    commit_id_uri(Askable, Commit_Id, Commit_Uri).
descriptor_commit_id_uri(Askable, Descriptor, Commit_Id, Commit_Uri) :-
    branch_head_commit(Askable, Descriptor.branch_name, Commit_Uri),
    commit_id_uri(Askable, Commit_Id, Commit_Uri).

commit_id_uri(Askable, Commit_Id, Commit_Uri) :-
    once(ask(Askable,
             t(Commit_Uri, identifier, Commit_Id^^xsd:string))).

commit_uri_to_metadata(Askable, Commit_Uri, Author, Message, Timestamp) :-
    once(ask(Askable,
             (   t(Commit_Uri, author, Author^^xsd:string),
                 t(Commit_Uri, message, Message^^xsd:string),
                 t(Commit_Uri, timestamp, Timestamp^^xsd:decimal)))).

commit_id_to_metadata(Askable, Commit_Id, Author, Message, Timestamp) :-
    commit_id_uri(Askable, Commit_Id, Commit_Uri),
    commit_uri_to_metadata(Askable, Commit_Uri, Author, Message, Timestamp).

commit_uri_to_parent_uri(Askable, Commit_Uri, Parent_Commit_Uri) :-
    once(ask(Askable,
             t(Commit_Uri, parent, Parent_Commit_Uri))).

commit_id_to_parent_uri(Askable, Commit_Id, Parent_Commit_Uri) :-
    commit_id_uri(Askable, Commit_Id, Commit_Uri),
    commit_uri_to_parent_uri(Askable, Commit_Uri, Parent_Commit_Uri).

layer_uri_for_commit(Askable, Commit_Uri, Type, Layer_Uri) :-
    ask(Askable,
        (   t(Commit_Uri, Type, Layer_Uri))).

insert_branch_object(Context, Branch_Name, Branch_Uri) :-
    insert_document(
        Context,
        _{ '@type' : 'Branch',
           'name' : Branch_Name },
        Branch_Uri).

delete_branch_object(Context, Branch_Uri) :-
    delete_document(Context, Branch_Uri).

insert_base_commit_object(Context, Commit_Id, Commit_Uri) :-
    insert_base_commit_object(Context, Context.commit_info, Commit_Id, Commit_Uri).
insert_base_commit_object(Context, Commit_Info, Commit_Id, Commit_Uri) :-
    get_time(Now),
    insert_base_commit_object(Context, Commit_Info, Now, Commit_Id, Commit_Uri).
insert_base_commit_object(Context, Commit_Info, Timestamp, Commit_Id, Commit_Uri) :-
    (   var(Commit_Id)
    ->  random_string(Commit_Id)
    ;   true),

    insert_document(
        Context,
        _{ '@type' : 'ValidCommit',
           'identifier' : Commit_Id,
           'author' : (Commit_Info.author),
           'message' : (Commit_Info.message),
           'timestamp' : Timestamp
         },
        Commit_Uri).

insert_child_commit_object(Context, Parent_Commit_Uri, Commit_Id, Commit_Uri) :-
    insert_child_commit_object(Context, Parent_Commit_Uri, Context.commit_info, Commit_Id, Commit_Uri).

insert_child_commit_object(Context, Parent_Commit_Uri, Commit_Info, Commit_Id, Commit_Uri) :-
    get_time(Now),
    insert_child_commit_object(Context, Parent_Commit_Uri, Commit_Info, Now, Commit_Id, Commit_Uri).

insert_child_commit_object(Context, Parent_Commit_Uri, Commit_Info, Timestamp, Commit_Id, Commit_Uri) :-
    insert_base_commit_object(Context, Commit_Info, Timestamp, Commit_Id, Commit_Uri),
    once(ask(Context,
             insert(Commit_Uri, parent, Parent_Commit_Uri))).

insert_commit_object_on_branch(Context, Branch_Name, Commit_Id, Commit_Uri) :-
    insert_commit_object_on_branch(Context, Context.commit_info, Branch_Name, Commit_Id, Commit_Uri).

insert_commit_object_on_branch(Context, Commit_Info, Branch_Name, Commit_Id, Commit_Uri) :-
    get_time(Now),
    insert_commit_object_on_branch(Context, Commit_Info, Now, Branch_Name, Commit_Id, Commit_Uri).
insert_commit_object_on_branch(Context, Commit_Info, Timestamp, Branch_Name, Commit_Id, Commit_Uri) :-
    branch_name_uri(Context, Branch_Name, Branch_Uri),

    (   branch_head_commit(Context, Branch_Name, Old_Commit_Uri)
    % if branch already points at something, delete that reference
    ->  once(ask(Context,
                 delete(Branch_Uri, head, Old_Commit_Uri))),
        insert_child_commit_object(Context, Old_Commit_Uri, Commit_Info, Timestamp, Commit_Id, Commit_Uri)
    % if branch does not yet point at something, insert a base commit pointing at no parent
    ;   insert_base_commit_object(Context, Commit_Info, Timestamp, Commit_Id, Commit_Uri)),

    % in both cases, we need to insert the new reference to the commit
    link_commit_object_to_branch(Context, Branch_Uri, Commit_Uri).

unlink_commit_object_from_branch(Context, Branch_Uri) :-
    once(ask(Context,
             (   t(Branch_Uri, head, Commit_Uri),
                 delete(Branch_Uri, head, Commit_Uri)))).

link_commit_object_to_branch(Context, Branch_Uri, Commit_Uri) :-
    once(ask(Context,
             insert(Branch_Uri, head, Commit_Uri))).


reset_branch_head(Context, Branch_Uri, Commit_Uri) :-
    ignore(unlink_commit_object_from_branch(Context,Branch_Uri)),
    link_commit_object_to_branch(Context,Branch_Uri, Commit_Uri).

% Note: We should probably refactor to add this to copy graph / copy new graph
attach_layer_to_commit(Context, Commit_Uri, Graph_Type, Layer_Uri) :-
    once(ask(Context,
             (   insert(Commit_Uri, '@schema':Graph_Type, Layer_Uri)))).

graph_idgen(Context, Commit_Id, Graph_Type, Graph_Uri) :-
    once(
        ask(Context,
            idgen('@base':'Graph',
                  [Commit_Id^^xsd:string,
                   Graph_Type^^xsd:string],
                  Graph_Uri))).

copy_commit(Origin_Context, Destination_Context, Commit_Id) :-
    commit_id_uri(Origin_Context,
                  Commit_Id,
                  Commit_Uri),

    commit_id_to_metadata(Origin_Context, Commit_Id, Author, Message, Timestamp),
    Commit_Info = commit_info{author: Author, message: Message},

    (   commit_id_to_parent_uri(Origin_Context, Commit_Id, Parent_Uri)
    ->  insert_child_commit_object(Destination_Context,
                                   Parent_Uri,
                                   Commit_Info,
                                   Timestamp,
                                   Commit_Id,
                                   Commit_Uri)
    ;   insert_base_commit_object(Destination_Context,
                                  Commit_Info,
                                  Timestamp,
                                  Commit_Id,
                                  Commit_Uri)),

    forall(graph_for_commit(Origin_Context, Commit_Uri, Type, Graph_Name, Graph_Uri),
           (   copy_graph_object(Origin_Context, Destination_Context, Graph_Uri),
               attach_graph_to_commit(Destination_Context, Commit_Uri, Type, Graph_Name, Graph_Uri))).

copy_commits(Origin_Context, Destination_Context, Commit_Id) :-
    (   has_commit(Destination_Context, Commit_Id)
    ->  true
    ;   copy_commit(Origin_Context, Destination_Context, Commit_Id),
        (   commit_id_to_parent_uri(Origin_Context, Commit_Id, Parent_Uri)
        ->  commit_id_uri(Origin_Context, Parent_Id, Parent_Uri),
            copy_commits(Origin_Context, Destination_Context, Parent_Id)
        ;   true)).

%% copy_commits(Origin_Context, Destination_Context, Commit_Id) :-
%%     context_default_prefixes(Origin_Context, Origin_Context_Stripped),
%%     context_default_prefixes(Destination_Context, Destination_Context_Stripped),
%%     copy_commits_(Origin_Context_Stripped, Destination_Context_Stripped, Commit_Id).

:- begin_tests(branch_objects).
:- use_module(core(util/test_utils)).
:- use_module(database).

test(branch_insert,
     [setup((setup_temp_store(State),
             ensure_label(testlabel))),
      cleanup(teardown_temp_store(State))]
    ) :-
    Descriptor = label_descriptor{label:"testlabel"},
    ref_schema_context_from_label_descriptor(Descriptor, Context),

    with_transaction(Context,
                     (   insert_branch_object(Context, "foo", _),
                         insert_branch_object(Context, "bar", _),
                         insert_branch_object(Context, "baz", _)),
                     _),

    findall(Branch_Name,
            has_branch(Descriptor, Branch_Name),
            Branches),

    Branches = ["bar",
                "baz",
                "foo"].

:- end_tests(branch_objects).


:-begin_tests(commit_objects).
:- use_module(core(util/test_utils)).
:- use_module(database).

test(base_commit_insert,
     [setup((setup_temp_store(State),
             ensure_label(testlabel))),
      cleanup(teardown_temp_store(State))]
    ) :-
    Descriptor = label_descriptor{label:"testlabel"},
    ref_schema_context_from_label_descriptor(Descriptor, Context),

    with_transaction(Context,
                     insert_base_commit_object(Context,
                                               commit_info{author:"author",
                                                           message:"message"},
                                               1234.567,
                                               Commit_Id,
                                               _Commit_Uri),

                     _),

    commit_id_to_metadata(Descriptor, Commit_Id, Author, Message, Timestamp),

    Author = "author",
    Message = "message",
    Timestamp = 1234.567.

test(child_commit_insert,
     [setup((setup_temp_store(State),
             ensure_label(testlabel))),
      cleanup(teardown_temp_store(State))]
    ) :-
    Descriptor = label_descriptor{label:"testlabel"},
    ref_schema_context_from_label_descriptor(Descriptor, Context),

    with_transaction(Context,
                     (   insert_base_commit_object(Context,
                                                   commit_info{author:"author",
                                                               message:"message"},
                                                   1234.567,
                                                   _Parent_Commit_Id,
                                                   Parent_Commit_Uri),
                         insert_child_commit_object(Context,
                                                    Parent_Commit_Uri,
                                                    commit_info{author:"author2",
                                                                message:"message2"},
                                                    2345.678,
                                                    Commit_Id,
                                                    _Commit_Uri)),
                     _),

    commit_id_to_metadata(Descriptor, Commit_Id, Author, Message, Timestamp),

    Author = "author2",
    Message = "message2",
    Timestamp = 2345.678,

    commit_id_to_parent_uri(Descriptor, Commit_Id, Parent_Commit_Uri).

test(commit_on_branch_insert,
     [setup((setup_temp_store(State),
             ensure_label(testlabel))),
      cleanup(teardown_temp_store(State))]
    ) :-
    Descriptor = label_descriptor{label:"testlabel"},

    % first set up the branch
    ref_schema_context_from_label_descriptor(Descriptor, Context1),
    with_transaction(Context1,
                     insert_branch_object(Context1, "foo", _),
                     _),

    % then add a commit
    ref_schema_context_from_label_descriptor(Descriptor, Context2),
    with_transaction(Context2,
                     insert_commit_object_on_branch(Context2,
                                                    "foo",
                                                    Commit_Id,
                                                    Commit_Uri),
                     _),

    % ensure our commit can be found
    commit_id_uri(Descriptor, Commit_Id, Commit_Uri),

    % ensure it doesn't have a parent
    (   commit_id_to_parent_uri(Descriptor, Commit_Id, _)
    ->  throw(error('found parent where none was expected'))
    ;   true),

    % now make a second commit
    ref_schema_context_from_label_descriptor(Descriptor, Context3),

    with_transaction(Context3,
                     insert_commit_object_on_branch(Context3,
                                                    "foo",
                                                    Commit2_Id,
                                                    Commit2_Uri),
                     _),



    % ensure our commit can be found
    commit_id_uri(Descriptor, Commit2_Id, Commit2_Uri),

    % ensure it does have a parent
    commit_id_to_parent_uri(Descriptor, Commit2_Id, _).

:- end_tests(commit_objects).

:- begin_tests(graph_objects).
:- use_module(core(util/test_utils)).
:- use_module(database).
test(insert_graph_object_without_layer,
     [setup((setup_temp_store(State),
             ensure_label(testlabel))),
      cleanup(teardown_temp_store(State))]
    ) :-
    Descriptor = label_descriptor{label:"testlabel"},
    ref_schema_context_from_label_descriptor(Descriptor, Context),

    with_transaction(Context,
                     (   insert_base_commit_object(Context,
                                                   commit_info{author:"author",
                                                               message:"message"},
                                                   1234.567,
                                                   Commit_Id,
                                                   Commit_Uri),
                         insert_graph_object(Context,
                                             Commit_Uri,
                                             Commit_Id,
                                             schema,
                                             "foo",
                                             _,
                                             _),
                         insert_graph_object(Context,
                                             Commit_Uri,
                                             Commit_Id,
                                             instance,
                                             "foo",
                                             _,
                                             _),
                         insert_graph_object(Context,
                                             Commit_Uri,
                                             Commit_Id,
                                             schema,
                                             "bar",
                                             _,
                                             _)),
                     _),

    findall(Type-Name,
            graph_for_commit(Descriptor, Commit_Uri, Type, Name, _Graph_Uri),
            Graphs),


    Expected = [schema-"foo",
                instance-"foo",
                schema-"bar"],
    union(Graphs, Expected, Union),
    intersection(Graphs, Expected, Intersection),
    subtract(Union, Intersection, []).

test(insert_graph_object_with_layer,
     [setup((setup_temp_store(State),
             ensure_label(testlabel))),
      cleanup(teardown_temp_store(State))]
    ) :-
    Descriptor = label_descriptor{label:"testlabel"},
    ref_schema_context_from_label_descriptor(Descriptor, Context),

    with_transaction(Context,
                     (   insert_base_commit_object(Context,
                                                   commit_info{author:"author",
                                                               message:"message"},
                                                   1234.567,
                                                   Commit_Id,
                                                   Commit_Uri),
                         insert_layer_object(Context,
                                             "f3dfc8d0d103b0be9428938174326e6256ad1beb",
                                             Layer1_Uri),
                         insert_graph_object(Context,
                                             Commit_Uri,
                                             Commit_Id,
                                             schema,
                                             "foo",
                                             Layer1_Uri,
                                             _),

                         insert_layer_object(Context,
                                             "461ccac7287ac5712cf98445b385ee44bf64e474",
                                             Layer2_Uri),
                         insert_graph_object(Context,
                                             Commit_Uri,
                                             Commit_Id,
                                             instance,
                                             "foo",
                                             Layer2_Uri,
                                             _),

                         insert_layer_object(Context,
                                             "a3a29522ec767aa1a1cf321122f833726c102749",
                                             Layer3_Uri),
                         insert_graph_object(Context,
                                             Commit_Uri,
                                             Commit_Id,
                                             schema,
                                             "bar",
                                             Layer3_Uri,
                                             _)),
                     _),

    findall(Type-Name-Layer_Id,
            (   graph_for_commit(Descriptor, Commit_Uri, Type, Name, Graph_Uri),
                layer_uri_for_graph(Descriptor, Graph_Uri, Layer_Uri),
                layer_id_uri(Descriptor, Layer_Id, Layer_Uri)),
            Graphs),

    Expected = [schema-"foo"-"f3dfc8d0d103b0be9428938174326e6256ad1beb",
                instance-"foo"-"461ccac7287ac5712cf98445b385ee44bf64e474",
                schema-"bar"-"a3a29522ec767aa1a1cf321122f833726c102749"
               ],
    union(Graphs, Expected, Union),
    intersection(Graphs, Expected, Intersection),
    subtract(Union, Intersection, []).

:- end_tests(graph_objects).

:- begin_tests(copy_commits).
:- use_module(core(util/test_utils)).
:- use_module(core(triple)).
:- use_module(database).
test(copy_base_commit,
     [setup((setup_temp_store(State),
             ensure_label(testlabel1),
             ensure_label(testlabel2))),
      cleanup(teardown_temp_store(State))]) :-
    % set up a chain
    Descriptor1 = label_descriptor{ label: "testlabel1" },
    Descriptor2 = label_descriptor{ label: "testlabel2" },
    ref_schema_context_from_label_descriptor(Descriptor1, Context1_1),
    Layer1_Id = "f3dfc8d0d103b0be9428938174326e6256ad1beb",
    Layer2_Id = "461ccac7287ac5712cf98445b385ee44bf64e474",
    with_transaction(Context1_1,
                     (
                         insert_base_commit_object(Context1_1, commit_info{author: "author", message: "message"}, 'commit_id', Commit_Uri),
                         insert_layer_object(Context1_1,
                                             Layer1_Id,
                                             Layer1_Uri),
                         insert_layer_object(Context1_1,
                                             Layer2_Id,
                                             Layer2_Uri),
                         insert_graph_object(Context1_1, Commit_Uri, "commit_id", instance, "graph_1", Layer1_Uri, Graph1_Uri),
                         insert_graph_object(Context1_1, Commit_Uri, "commit_id", schema, "graph_2", Layer2_Uri, Graph2_Uri)
                     ),
                     _),


    ref_schema_context_from_label_descriptor(Descriptor1, Context1_2),
    ref_schema_context_from_label_descriptor(Descriptor2, Context2),

    with_transaction(Context2,
                     copy_commits(Context1_2, Context2, "commit_id"),
                     _),

    ref_schema_context_from_label_descriptor(Descriptor2, Context2_1),
    commit_id_uri(Context2_1, "commit_id", Commit_Uri),

    findall(Type-Name-Graph_Uri-Layer_Uri-Layer_Id,
            (   graph_for_commit(Context2_1, Commit_Uri, Type, Name, Graph_Uri),
                layer_uri_for_graph(Context2_1, Graph_Uri, Layer_Uri),
                layer_id_uri(Context2_1, Layer_Id, Layer_Uri)
            ),
            Graphs),

    Expected = [
        'http://terminusdb.com/schema/ref#instance'-"graph_1"-Graph1_Uri-Layer1_Uri-Layer1_Id,
        'http://terminusdb.com/schema/ref#schema'-"graph_2"-Graph2_Uri-Layer2_Uri-Layer2_Id
    ],

    list_to_ord_set(Graphs, Graph_Set),
    list_to_ord_set(Expected, Expected_Set),
    print_term(here, []),nl,

    ord_seteq(Graph_Set, Expected_Set).

test(copy_child_commit_with_no_shared_ancestors,
     [setup((setup_temp_store(State),
             ensure_label(testlabel1),
             ensure_label(testlabel2))),
      cleanup(teardown_temp_store(State))]) :-
    % set up a chain
    Descriptor1 = label_descriptor{ label: "testlabel1" },
    Descriptor2 = label_descriptor{ label: "testlabel2" },
    ref_schema_context_from_label_descriptor(Descriptor1, Context1_1),
    Layer1_Id = "f3dfc8d0d103b0be9428938174326e6256ad1beb",
    Layer2_Id = "461ccac7287ac5712cf98445b385ee44bf64e474",
    Layer3_Id = "a3a29522ec767aa1a1cf321122f833726c102749",
    with_transaction(Context1_1,
                     (
                         % first commit
                         insert_base_commit_object(Context1_1, commit_info{author: "author", message: "commit 1"}, 'commit1_id', Commit1_Uri),
                         insert_layer_object(Context1_1,
                                             Layer1_Id,
                                             Layer1_Uri),
                         insert_graph_object(Context1_1, Commit1_Uri, "commit1_id", instance, "main", Layer1_Uri, _Graph1_Uri),

                         % second commit
                         insert_child_commit_object(Context1_1, Commit1_Uri, commit_info{author: "author", message: "commmit 2"}, 'commit2_id', Commit2_Uri),
                         insert_layer_object(Context1_1,
                                             Layer2_Id,
                                             Layer2_Uri),
                         insert_graph_object(Context1_1, Commit2_Uri, "commit2_id", instance, "main", Layer2_Uri, _Graph2_Uri),

                         % third commit
                         insert_child_commit_object(Context1_1, Commit2_Uri, commit_info{author: "author", message: "commit 3"}, 'commit3_id', Commit3_Uri),
                         insert_layer_object(Context1_1,
                                             Layer3_Id,
                                             Layer3_Uri),
                         insert_graph_object(Context1_1, Commit3_Uri, "commit3_id", instance, "main", Layer3_Uri, _Graph3_Uri)
                     ),
                     _),


    ref_schema_context_from_label_descriptor(Descriptor1, Context1_2),
    ref_schema_context_from_label_descriptor(Descriptor2, Context2),

    with_transaction(Context2,
                     copy_commits(Context1_2, Context2, "commit3_id"),
                     _),

    ref_schema_context_from_label_descriptor(Descriptor2, Context2_1),
    findall(Commit_Id-Commit_Uri,
            (   has_commit(Context2_1, Commit_Id),
                commit_id_uri(Context2_1, Commit_Id, Commit_Uri)),
            Commits_Unsorted),
    length(Commits_Unsorted, 3),
    sort(Commits_Unsorted, Commits),

    Expected = ["commit1_id"-Commit1_Uri,
                "commit2_id"-Commit2_Uri,
                "commit3_id"-Commit3_Uri],
    Expected = Commits.

test(copy_child_commit_with_some_shared_ancestors,
     [setup((setup_temp_store(State),
             ensure_label(testlabel1),
             ensure_label(testlabel2))),
      cleanup(teardown_temp_store(State))]) :-
    % set up a chain
    Descriptor1 = label_descriptor{ label: "testlabel1" },
    Descriptor2 = label_descriptor{ label: "testlabel2" },
    ref_schema_context_from_label_descriptor(Descriptor1, Context1_1),
    Layer1_Id = "f3dfc8d0d103b0be9428938174326e6256ad1beb",
    Layer2_Id = "461ccac7287ac5712cf98445b385ee44bf64e474",
    Layer3_Id = "a3a29522ec767aa1a1cf321122f833726c102749",
    with_transaction(Context1_1,
                     (
                         % first commit
                         insert_base_commit_object(Context1_1, commit_info{author: "author", message: "commit 1"}, 'commit1_id', Commit1_Uri),
                         insert_layer_object(Context1_1,
                                             Layer1_Id,
                                             Layer1_Uri),
                         insert_graph_object(Context1_1, Commit1_Uri, "commit1_id", instance, "main", Layer1_Uri, _Graph1_Uri),

                         % second commit
                         insert_child_commit_object(Context1_1, Commit1_Uri, commit_info{author: "author", message: "commmit 2"}, 'commit2_id', Commit2_Uri),
                         insert_layer_object(Context1_1,
                                             Layer2_Id,
                                             Layer2_Uri),
                         insert_graph_object(Context1_1, Commit2_Uri, "commit2_id", instance, "main", Layer2_Uri, _Graph2_Uri),

                         % third commit
                         insert_child_commit_object(Context1_1, Commit2_Uri, commit_info{author: "author", message: "commit 3"}, 'commit3_id', Commit3_Uri),
                         insert_layer_object(Context1_1,
                                             Layer3_Id,
                                             Layer3_Uri),
                         insert_graph_object(Context1_1, Commit3_Uri, "commit3_id", instance, "main", Layer3_Uri, _Graph3_Uri)
                     ),
                     _),


    ref_schema_context_from_label_descriptor(Descriptor1, Context1_2),
    ref_schema_context_from_label_descriptor(Descriptor2, Context2_1),

    with_transaction(Context2_1,
                     copy_commits(Context1_2, Context2_1, "commit2_id"),
                     _),

    ref_schema_context_from_label_descriptor(Descriptor2, Context2_2),

    with_transaction(Context2_2,
                     copy_commits(Context1_2, Context2_2, "commit3_id"),
                     _),

    ref_schema_context_from_label_descriptor(Descriptor2, Context2_3),
    findall(Commit_Id-Commit_Uri,
            (   has_commit(Context2_3, Commit_Id),
                commit_id_uri(Context2_3, Commit_Id, Commit_Uri)),
            Commits_Unsorted),
    length(Commits_Unsorted, 3),
    sort(Commits_Unsorted, Commits),

    Expected = ["commit1_id"-Commit1_Uri,
                "commit2_id"-Commit2_Uri,
                "commit3_id"-Commit3_Uri],
    Expected = Commits.

test(copy_child_commit_that_already_exists,
     [setup((setup_temp_store(State),
             ensure_label(testlabel1),
             ensure_label(testlabel2))),
      cleanup(teardown_temp_store(State))]) :-
    % set up a chain
    Descriptor1 = label_descriptor{ label: "testlabel1" },
    Descriptor2 = label_descriptor{ label: "testlabel2" },
    ref_schema_context_from_label_descriptor(Descriptor1, Context1_1),
    Layer1_Id = "f3dfc8d0d103b0be9428938174326e6256ad1beb",
    Layer2_Id = "461ccac7287ac5712cf98445b385ee44bf64e474",
    Layer3_Id = "a3a29522ec767aa1a1cf321122f833726c102749",
    with_transaction(Context1_1,
                     (
                         % first commit
                         insert_base_commit_object(Context1_1, commit_info{author: "author", message: "commit 1"}, 'commit1_id', Commit1_Uri),
                         insert_layer_object(Context1_1,
                                             Layer1_Id,
                                             Layer1_Uri),
                         insert_graph_object(Context1_1, Commit1_Uri, "commit1_id", instance, "main", Layer1_Uri, _Graph1_Uri),

                         % second commit
                         insert_child_commit_object(Context1_1, Commit1_Uri, commit_info{author: "author", message: "commmit 2"}, 'commit2_id', Commit2_Uri),
                         insert_layer_object(Context1_1,
                                             Layer2_Id,
                                             Layer2_Uri),
                         insert_graph_object(Context1_1, Commit2_Uri, "commit2_id", instance, "main", Layer2_Uri, _Graph2_Uri),

                         % third commit
                         insert_child_commit_object(Context1_1, Commit2_Uri, commit_info{author: "author", message: "commit 3"}, 'commit3_id', Commit3_Uri),
                         insert_layer_object(Context1_1,
                                             Layer3_Id,
                                             Layer3_Uri),
                         insert_graph_object(Context1_1, Commit3_Uri, "commit3_id", instance, "main", Layer3_Uri, _Graph3_Uri)
                     ),
                     _),


    ref_schema_context_from_label_descriptor(Descriptor1, Context1_2),
    ref_schema_context_from_label_descriptor(Descriptor2, Context2_1),

    with_transaction(Context2_1,
                     copy_commits(Context1_2, Context2_1, "commit3_id"),
                     _),

    ref_schema_context_from_label_descriptor(Descriptor2, Context2_2),

    with_transaction(Context2_2,
                     copy_commits(Context1_2, Context2_2, "commit3_id"),
                     _),

    ref_schema_context_from_label_descriptor(Descriptor2, Context2_3),
    findall(Commit_Id-Commit_Uri,
            (   has_commit(Context2_3, Commit_Id),
                commit_id_uri(Context2_3, Commit_Id, Commit_Uri)),
            Commits_Unsorted),
    length(Commits_Unsorted, 3),
    sort(Commits_Unsorted, Commits),

    Expected = ["commit1_id"-Commit1_Uri,
                "commit2_id"-Commit2_Uri,
                "commit3_id"-Commit3_Uri],
    Expected = Commits.


:- end_tests(copy_commits).

read_write_obj_for_graph(Askable, Commit_Uri, Graph_Type, Graph_Name, Read_Write_Obj) :-
    (   graph_for_commit(Askable, Commit_Uri, Graph_Type, Graph_Name, Graph_Uri)
    ->  true
    ;   throw(error(graph_does_not_exist(Commit_Uri, Graph_Type, Graph_Name)))),


    (   layer_uri_for_graph(Askable, Graph_Uri, Layer_Uri),
        layer_id_uri(Askable, Layer_Id, Layer_Uri)
    ->  Graph_Descriptor = id_graph{
                               layer_id: Layer_Id,
                               type: instance,
                               name: "main"
                           },
        open_read_write_obj(Graph_Descriptor, Read_Write_Obj)
    ;   Read_Write_Obj = read_write_obj{
                             descriptor: empty{},
                             read: _,
                             write: _
                         }).

apply_graph_change(Us_Repo_Context, Them_Repo_Askable, New_Commit_Uri, New_Commit_Id, Us_Commit_Uri, Them_Commit_Uri, Graph_Type, Graph_Name, New_Graph_Uri) :-
    % find current head layer for the given graph
    read_write_obj_for_graph(Us_Repo_Context, Us_Commit_Uri, Graph_Type, Graph_Name, Us_Read_Write_Obj),
    read_write_obj_for_graph(Them_Repo_Askable, Them_Commit_Uri, Graph_Type, Graph_Name, Them_Read_Write_Obj),

    read_write_obj_builder(Us_Read_Write_Obj, Builder),
    read_write_obj_reader(Them_Read_Write_Obj, Layer),
    nb_apply_delta(Builder,Layer),

    read_write_obj_to_graph_validation_obj(Us_Read_Write_Obj, Us_Validation_Obj, [], _),
    (   ground(Us_Validation_Obj.read)
    ->  layer_to_id(Us_Validation_Obj.read, Layer_Id),
        insert_layer_object(Us_Repo_Context, Layer_Id, Layer_Uri)
    ;   Layer_Uri = _
    ),

    % in both cases, we need to write a graph object and return its URI
    insert_graph_object(Us_Repo_Context, New_Commit_Uri, New_Commit_Id, Graph_Type, Graph_Name, Layer_Uri, New_Graph_Uri).

ensure_graph_sets_equal(Us_Repo_Askable, Them_Repo_Askable, Us_Commit_Uri, Them_Commit_Uri) :-
    findall(Type-Name,
            graph_for_commit(Us_Repo_Askable, Us_Commit_Uri, Type, Name, _),
            Us_Graphs),
    findall(Type-Name,
            graph_for_commit(Them_Repo_Askable, Them_Commit_Uri, Type, Name, _),
            Them_Graphs),
    list_to_ord_set(Us_Graphs, Us_Graph_Set),
    list_to_ord_set(Them_Graphs, Them_Graph_Set),
    (   ord_seteq(Us_Graph_Set, Them_Graph_Set)
    ->  true
    ;   throw(error(graph_sets_not_equal(Us_Commit_Uri, Them_Commit_Uri)))).

ensure_graph_sets_included(Us_Repo_Askable, Them_Repo_Askable, Us_Commit_Uri, Them_Commit_Uri, New_Graphs) :-
    findall(Type1-Name1,
            graph_for_commit(Us_Repo_Askable, Us_Commit_Uri, Type1, Name1, _),
            Us_Graphs),
    findall(Type2-Name2,
            graph_for_commit(Them_Repo_Askable, Them_Commit_Uri, Type2, Name2, _),
            Them_Graphs),
    list_to_ord_set(Us_Graphs, Us_Graph_Set),
    list_to_ord_set(Them_Graphs, Them_Graph_Set),
    ord_subtract(Them_Graph_Set, Us_Graph_Set, New_Graphs),
    (   ord_subtract(Us_Graph_Set, Them_Graph_Set, [])
    ->  true
    ;   throw(error(graph_sets_not_included(Us_Commit_Uri, Them_Commit_Uri)))).

apply_commit_on_branch(Us_Repo_Context, Them_Repo_Askable, Us_Branch_Name, Them_Commit_Uri, Author, New_Commit_Id, New_Commit_Uri) :-
    get_time(Now),
    apply_commit_on_branch(Us_Repo_Context, Them_Repo_Askable, Us_Branch_Name, Them_Commit_Uri, Author, Now, New_Commit_Id, New_Commit_Uri).

apply_commit_on_branch(Us_Repo_Context, Them_Repo_Askable, Us_Branch_Name, Them_Commit_Uri, Author, Timestamp, New_Commit_Id, New_Commit_Uri) :-
    % look up current head
    (   branch_head_commit(Us_Repo_Context, Us_Branch_Name, Us_Commit_Uri)
    ->  true
    ;   throw(error(not_implemented))),

    branch_name_uri(Us_Repo_Context, Us_Branch_Name, Us_Branch_Uri),

    apply_commit_on_commit(Us_Repo_Context, Them_Repo_Askable, Us_Commit_Uri, Them_Commit_Uri, Author, Timestamp, New_Commit_Id, New_Commit_Uri),
    unlink_commit_object_from_branch(Us_Repo_Context, Us_Branch_Uri),
    link_commit_object_to_branch(Us_Repo_Context, Us_Branch_Uri, New_Commit_Uri),

    % Note, this doesn't yet commit the commit graph.
    % We may actually have written an invalid commit here.

    true.

apply_commit_on_commit(Us_Repo_Context, Them_Repo_Askable, Us_Commit_Uri, Them_Commit_Uri, Author, New_Commit_Id, New_Commit_Uri) :-
    get_time(Now),
    apply_commit_on_commit(Us_Repo_Context, Them_Repo_Askable, Us_Commit_Uri, Them_Commit_Uri, Author, Now, New_Commit_Id, New_Commit_Uri).

apply_commit_on_commit(Us_Repo_Context, Them_Repo_Askable, Us_Commit_Uri, Them_Commit_Uri, Author, Timestamp, New_Commit_Id, New_Commit_Uri) :-
    % ensure graph sets are equivalent. if not, error
    ensure_graph_sets_included(Us_Repo_Context, Them_Repo_Askable, Us_Commit_Uri, Them_Commit_Uri, New_Graphs),

    % create new commit info
    commit_id_uri(Them_Repo_Askable, Them_Commit_Id, Them_Commit_Uri),
    commit_id_to_metadata(Them_Repo_Askable, Them_Commit_Id, _Them_Author, Message, _Them_Timestamp),
    Commit_Info = commit_info{author: Author, message: Message},

    % create new commit
    insert_child_commit_object(
        Us_Repo_Context,
        Us_Commit_Uri,
        Commit_Info,
        Timestamp,
        New_Commit_Id,
        New_Commit_Uri),

    forall(graph_for_commit(Us_Repo_Context,
                            Us_Commit_Uri,
                            Type,
                            Name,
                            _Graph_Uri),
           apply_graph_change(Us_Repo_Context,
                              Them_Repo_Askable,
                              New_Commit_Uri,
                              New_Commit_Id,
                              Us_Commit_Uri,
                              Them_Commit_Uri,
                              Type,
                              Name,
                              _New_Graph_Uri)),

    forall(member(Graph_Type-Graph_Name,
                  New_Graphs),
           (   once(graph_for_commit(Them_Repo_Askable, Them_Commit_Uri, Graph_Type, Graph_Name, Graph_Uri)),
               copy_new_graph_object(Them_Repo_Askable, Us_Repo_Context, Graph_Uri,
                                     Them_Commit_Id, New_Graph_Uri),
               attach_graph_to_commit(Us_Repo_Context, New_Commit_Uri,
                                      Graph_Type, Graph_Name, New_Graph_Uri)
           )
          ),

    % Note, this doesn't yet commit the commit graph.
    % We may actually have written an invalid commit here.

    true.

repository_prefixes(Repository_Askable, Prefixes) :-
    findall(Key-Value,
            (   ask(Repository_Askable,
                    (   t(PrefixPair, ref:prefix, Key_String^^xsd:string),
                        t(PrefixPair, ref:prefix_uri, Value_String^^xsd:string))),
                atom_string(Key,Key_String),
                atom_string(Value,Value_String)),
            Key_Value_Pairs),
    dict_create(Prefixes,_,Key_Value_Pairs).

insert_prefix(Context, Key, URI) :-
    ask(Context,
        (   idgen('terminusdb:///repository/data/PrefixPair',[Key], Pair),
            insert(Pair, rdf:type, ref:'PrefixPair'),
            insert(ref:default_prefixes, ref:prefix_pair, Pair),
            insert(Pair, ref:prefix, Key^^xsd:string),
            insert(Pair, ref:prefix_uri, URI^^xsd:string)
        )
       ).

remove_prefix(Context, Key) :-
    ask(Context,
        (   t(Pair, ref:prefix, Key^^xsd:string),
            t(Pair, ref:prefix_uri, URI^^xsd:string),
            delete(Pair, rdf:type, ref:'PrefixPair'),
            delete(ref:default_prefixes, ref:prefix_pair, Pair),
            delete(Pair, ref:prefix, Key^^xsd:string),
            delete(Pair, ref:prefix_uri, URI^^xsd:string))).

update_prefix(Context, Key, URI) :-
    ask(Context,
        (   t(Pair, ref:prefix, Key^^xsd:string),
            t(Pair, ref:prefix_uri, Old_URI^^xsd:string),
            delete(Pair, ref:prefix_uri, Old_URI^^xsd:string),
            insert(Pair, ref:prefix_uri, URI^^xsd:string))).

update_prefixes(Context, Prefixes) :-
    repository_prefixes(Context, Old_Prefixes),

    dict_keys(Prefixes, Prefixes_Keys),
    list_to_ord_set(Prefixes_Keys, Prefixes_Keys_Set),
    dict_keys(Old_Prefixes, Old_Prefixes_Keys),
    list_to_ord_set(Old_Prefixes_Keys, Old_Prefixes_Keys_Set),

    ord_subtract(Prefixes_Keys_Set, Old_Prefixes_Keys_Set, Keys_To_Add),
    ord_subtract(Old_Prefixes_Keys_Set, Prefixes_Keys_Set, Keys_To_Remove),
    ord_intersect(Prefixes_Keys_Set, Old_Prefixes_Keys_Set, Keys_To_Potentially_Update),

    exclude({Old_Prefixes, Prefixes}/[X]>>(get_dict(X, Old_Prefixes, Result),
                                           get_dict(X, Prefixes, Result)),
            Keys_To_Potentially_Update,
            Keys_To_Update),

    % first, delete all prefixes that are no longer needed
    forall(member(Key, Keys_To_Add),
           (   get_dict(Key, Prefixes, URI),
               insert_prefix(Context, Key, URI))),

    % second, insert prefixes that are new
    forall(member(Key, Keys_To_Remove),
           remove_prefix(Context, Key)),

    % third, update existing prefixes that have changed
    forall(member(Key, Keys_To_Update),
           (   get_dict(Key, Prefixes, URI),
               update_prefix(Context, Key, URI))).

copy_prefixes(Repo_From_Askable, Repo_To_Context) :-
    repository_prefixes(Repo_From_Askable, Prefixes),
    update_prefixes(Repo_To_Context, Prefixes).

:- begin_tests(commit_application).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).
:- use_module(core(api)).
:- use_module(database).
test(apply_single_addition,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "testdb1"),
             create_db_without_schema("admin", "testdb2")
            )),
      cleanup(teardown_temp_store(State))]) :-
    % create single commit on both databases with the same single main graph
    % rebase one commit on the other
    % query to ensure all triples are now reachable

    resolve_absolute_string_descriptor("admin/testdb1", Descriptor1),
    resolve_absolute_string_descriptor("admin/testdb2", Descriptor2),

    create_context(Descriptor1, commit_info{author: "me", message: "commit a"}, Context1),
    with_transaction(Context1,
                     ask(Context1, insert(a,b,c)),
                     _),
    create_context(Descriptor2, commit_info{author: "me", message: "commit b"}, Context2),
    with_transaction(Context2,
                     ask(Context2, insert(d,e,f)),
                     _),

    create_context(Descriptor1.repository_descriptor, Context3),
    create_context(Descriptor2.repository_descriptor, Context4),

    branch_head_commit(Context4, "main", Commit_B_Uri),

    with_transaction(Context3,
                     apply_commit_on_branch(Context3, Context4, "main", Commit_B_Uri,
                                            "rebaser",
                                            12345,
                                            _New_Commit_Id,
                                            New_Commit_B_Uri),
                     _),

    Repo_Descriptor = (Descriptor1.repository_descriptor),
    create_context(Repo_Descriptor, Context5),
    commit_uri_to_parent_uri(Context5, New_Commit_B_Uri, Commit_A_Uri),
    commit_uri_to_metadata(Context5, Commit_A_Uri, _, "commit a", _),
    commit_uri_to_metadata(Context5, New_Commit_B_Uri, _, "commit b", _),

    once(ask(Context5,
             (   t(a,b,c),
                 t(d,e,f),
                 addition(d,e,f)))).

test(apply_single_removal,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "testdb1"),
             create_db_without_schema("admin", "testdb2")
            )),
      cleanup(teardown_temp_store(State))]) :-
    % create single commit on both databases with the same single main graph
    % rebase one commit on the other
    % query to ensure all triples are now reachable

    resolve_absolute_string_descriptor("admin/testdb1", Descriptor1),
    resolve_absolute_string_descriptor("admin/testdb2", Descriptor2),

    create_context(Descriptor1, commit_info{author: "me", message: "commit a"}, Context1),
    with_transaction(Context1,
                     ask(Context1,
                         (   insert(a,b,c),
                             insert(d,e,f))),
                     _),
    create_context(Descriptor2, commit_info{author: "me", message: "commit b"}, Context2_1),
    with_transaction(Context2_1,
                     ask(Context2_1, insert(d,e,f)),
                     _),
    create_context(Descriptor2, commit_info{author: "me", message: "commit b"}, Context2_2),
    with_transaction(Context2_2,
                     ask(Context2_2, delete(d,e,f)),
                     _),

    create_context(Descriptor1.repository_descriptor, Context3),
    create_context(Descriptor2.repository_descriptor, Context4),

    branch_head_commit(Context4, "main", Commit_B_Uri),

    with_transaction(Context3,
                     apply_commit_on_branch(Context3, Context4, "main", Commit_B_Uri,
                                            "rebaser",
                                            12345,
                                            _New_Commit_Id,
                                            _New_Commit_Uri),
                     _),

    once(ask(Descriptor1,
             (   t(a,b,c),
                 removal(d,e,f)))).

test(apply_existing_addition,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "testdb1"),
             create_db_without_schema("admin", "testdb2")
            )),
      cleanup(teardown_temp_store(State))]) :-
    % create single commit on both databases with the same single main graph
    % rebase one commit on the other
    % query to ensure all triples are now reachable

    resolve_absolute_string_descriptor("admin/testdb1", Descriptor1),
    resolve_absolute_string_descriptor("admin/testdb2", Descriptor2),

    create_context(Descriptor1, commit_info{author: "me", message: "commit a"}, Context1),
    with_transaction(Context1,
                     ask(Context1, insert(a,b,c)),
                     _),
    create_context(Descriptor2, commit_info{author: "me", message: "commit b"}, Context2),
    with_transaction(Context2,
                     ask(Context2, insert(a,b,c)),
                     _),

    create_context(Descriptor1.repository_descriptor, Context3),
    create_context(Descriptor2.repository_descriptor, Context4),

    branch_head_commit(Context4, "main", Commit_B_Uri),

    with_transaction(Context3,
                     apply_commit_on_branch(Context3, Context4, "main", Commit_B_Uri,
                                            "rebaser",
                                            12345,
                                            _New_Commit_Id,
                                            _New_Commit_Uri),
                     _),

    once(ask(Descriptor1,
             (   t(a,b,c)))).
            %not(addition(a,b,c)))). % since we're reusing the previous layer, rather than creating a new (empty) one, we're actually still seeing an addition. Is that a problem?

test(apply_nonexisting_removal,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "testdb1"),
             create_db_without_schema("admin", "testdb2")
            )),
      cleanup(teardown_temp_store(State))]) :-
    % create single commit on both databases with the same single main graph
    % rebase one commit on the other
    % query to ensure all triples are now reachable

    resolve_absolute_string_descriptor("admin/testdb1", Descriptor1),
    resolve_absolute_string_descriptor("admin/testdb2", Descriptor2),

    create_context(Descriptor1, commit_info{author: "me", message: "commit a"}, Context1),
    with_transaction(Context1,
                     ask(Context1,
                         (   insert(a,b,c))),
                     _),
    create_context(Descriptor2, commit_info{author: "me", message: "commit b"}, Context2_1),
    with_transaction(Context2_1,
                     ask(Context2_1, insert(d,e,f)),
                     _),
    create_context(Descriptor2, commit_info{author: "me", message: "commit b"}, Context2_2),
    with_transaction(Context2_2,
                     ask(Context2_2, delete(d,e,f)),
                     _),

    create_context(Descriptor1.repository_descriptor, Context3),
    create_context(Descriptor2.repository_descriptor, Context4),

    branch_head_commit(Context4, "main", Commit_B_Uri),

    with_transaction(Context3,
                     apply_commit_on_branch(Context3, Context4, "main", Commit_B_Uri,
                                            "rebaser",
                                            12345,
                                            _New_Commit_Id,
                                            _New_Commit_Uri),
                     _),

    once(ask(Descriptor1,
             (   t(a,b,c),
                 not(removal(d,e,f))))).

:- end_tests(commit_application).

most_recent_common_ancestor(Repo1_Context, Repo2_Context, Commit1_Id, Commit2_Id, Final_Commit_Id, Commit1_Path, Commit2_Path) :-
    commit_id_uri(Repo1_Context, Commit1_Id, Commit1_Uri),
    commit_id_uri(Repo2_Context, Commit2_Id, Commit2_Uri),
    
    % Note: this isn't great time complexity
    ask(Repo1_Context, path(Commit1_Uri, (star(p(ref:commit_parent)), p(ref:commit_id)), Final_Commit_Id^^xsd:string, Commit1_Edge_Path_Reversed)),
    ask(Repo2_Context, path(Commit2_Uri, (star(p(ref:commit_parent)), p(ref:commit_id)), Final_Commit_Id_2^^xsd:string, Commit2_Edge_Path_Reversed)),

    Final_Commit_Id = Final_Commit_Id_2,

    !, % we're only interested in one solution!

    reverse(Commit1_Edge_Path_Reversed, [_|Commit1_Edge_Path]),
    reverse(Commit2_Edge_Path_Reversed, [_|Commit2_Edge_Path]),

    maplist({Repo1_Context}/[Commit_Edge, Intermediate_Commit_Id]>>(
                get_dict('http://terminusdb.com/schema/woql#subject',Commit_Edge, Intermediate_Commit_Uri),
                commit_id_uri(Repo1_Context, Intermediate_Commit_Id, Intermediate_Commit_Uri)),
            Commit1_Edge_Path,
            Commit1_Path),
    maplist({Repo2_Context}/[Commit_Edge, Intermediate_Commit_Id]>>(
                get_dict('http://terminusdb.com/schema/woql#subject',Commit_Edge, Intermediate_Commit_Uri),
                commit_id_uri(Repo2_Context, Intermediate_Commit_Id, Intermediate_Commit_Uri)),
            Commit2_Edge_Path,
            Commit2_Path).

:- begin_tests(common_ancestor).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).
:- use_module(core(api)).
:- use_module(database).
test(common_ancestor_after_branch_and_some_commits,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State))]) :-

    Origin_Path = "admin/testdb",
    resolve_absolute_string_descriptor(Origin_Path, Descriptor),
    create_context(Descriptor, commit_info{author:"test",message: "commit a"}, Commit_A_Context),
    with_transaction(Commit_A_Context,
                     ask(Commit_A_Context,
                         insert(a,b,c)),
                     _),
    create_context(Descriptor, commit_info{author:"test",message: "commit b"}, Commit_B_Context),
    with_transaction(Commit_B_Context,
                     ask(Commit_B_Context,
                         insert(d,e,f)),
                     _),

    super_user_authority(Auth),
    Destination_Path = "admin/testdb/local/branch/second",
    branch_create(system_descriptor{}, Auth, Destination_Path, some(Origin_Path), _),

    create_context(Descriptor, commit_info{author:"test",message: "commit c"}, Commit_C_Context),
    with_transaction(Commit_C_Context,
                     ask(Commit_C_Context,
                         insert(g,h,i)),
                     _),
    create_context(Descriptor, commit_info{author:"test",message: "commit d"}, Commit_D_Context),
    with_transaction(Commit_D_Context,
                     ask(Commit_D_Context,
                         insert(k,l,m)),
                     _),

    resolve_absolute_string_descriptor("admin/testdb/local/branch/second", Second_Descriptor),
    create_context(Second_Descriptor, commit_info{author:"test",message: "commit e"}, Commit_E_Context),
    with_transaction(Commit_E_Context,
                     ask(Commit_E_Context,
                         insert(n,o,p)),
                     _),
    create_context(Second_Descriptor, commit_info{author:"test",message: "commit f"}, Commit_F_Context),
    with_transaction(Commit_F_Context,
                     ask(Commit_F_Context,
                         insert(q,r,s)),
                     _),

    Repo_Descriptor = Descriptor.repository_descriptor,
    create_context(Repo_Descriptor, Repo_Context),

    branch_head_commit(Repo_Context, "main", Head1_Commit_Uri),
    commit_id_uri(Repo_Context, Head1_Commit_Id, Head1_Commit_Uri),
    branch_head_commit(Repo_Context, "second", Head2_Commit_Uri),
    commit_id_uri(Repo_Context, Head2_Commit_Id, Head2_Commit_Uri),

    most_recent_common_ancestor(Repo_Context, Repo_Context, Head1_Commit_Id, Head2_Commit_Id, Common_Commit_Id, Branch1_Path, Branch2_Path),

    Repo_Descriptor = Descriptor.repository_descriptor,
    commit_id_to_metadata(Repo_Descriptor, Common_Commit_Id, _, "commit b", _),

    Branch1_Path = [CommitC_Id, CommitD_Id],
    Branch2_Path = [CommitE_Id, CommitF_Id],
    commit_id_to_metadata(Repo_Descriptor, CommitC_Id, _, "commit c", _),
    commit_id_to_metadata(Repo_Descriptor, CommitD_Id, _, "commit d", _),
    commit_id_to_metadata(Repo_Descriptor, CommitE_Id, _, "commit e", _),
    commit_id_to_metadata(Repo_Descriptor, CommitF_Id, _, "commit f", _).

:- end_tests(common_ancestor).

commit_is_valid(Context, Commit_Id) :-
    commit_id_uri(Context, Commit_Id, Commit_Uri),
    once(ask(Context,
             t(Commit_Uri, rdf:type, ref:'ValidCommit'))).

invalidate_commit(Context, Commit_Id) :-
    (   commit_is_valid(Context, Commit_Id)
    ->  commit_id_uri(Context, Commit_Id, Commit_Uri),
        once(ask(Context,
                 (   delete(Commit_Uri, rdf:type, ref:'ValidCommit'),
                     insert(Commit_Uri, rdf:type, ref:'InvalidCommit'))))
    ;   true).

commit_uri_to_history_commit_uris_(Context, Commit_Uri, [Commit_Uri|History_Commit_Uris]) :-
    (   commit_uri_to_parent_uri(Context, Commit_Uri, Parent_Uri)
    ->  commit_uri_to_history_commit_uris_(Context, Parent_Uri, History_Commit_Uris)
    ;   History_Commit_Uris = []).

commit_uri_to_history_commit_uris(Context, Commit_Uri, History_Commit_Uris) :-
    commit_uri_to_history_commit_uris_(Context, Commit_Uri, Reversed_History_Commit_Uris),
    reverse(Reversed_History_Commit_Uris, History_Commit_Uris).

commit_uri_to_history_commit_ids_(Context, Commit_Uri, [Commit_Id|History_Commit_Ids]) :-
    commit_id_uri(Context, Commit_Id, Commit_Uri),
    (   commit_uri_to_parent_uri(Context, Commit_Uri, Parent_Uri)
    ->  commit_uri_to_history_commit_ids_(Context, Parent_Uri, History_Commit_Ids)
    ;   History_Commit_Ids = []).

commit_uri_to_history_commit_ids(Context, Commit_Uri, History_Commit_Ids) :-
    commit_uri_to_history_commit_ids_(Context, Commit_Uri, Reversed_History_Commit_Ids),
    reverse(Reversed_History_Commit_Ids, History_Commit_Ids).

:- begin_tests(commit_history).
:- use_module(core(util/test_utils)).
:- use_module(core(query)).
:- use_module(core(api)).
:- use_module(database).
test(commit_history_ids,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State))]) :-
    resolve_absolute_string_descriptor("admin/testdb", Descriptor),
    create_context(Descriptor, commit_info{author:"test",message: "commit a"}, Commit_A_Context),
    with_transaction(Commit_A_Context,
                     ask(Commit_A_Context,
                         insert(a,b,c)),
                     _),
    create_context(Descriptor, commit_info{author:"test",message: "commit b"}, Commit_B_Context),
    with_transaction(Commit_B_Context,
                     ask(Commit_B_Context,
                         insert(d,e,f)),
                     _),

    Repository_Descriptor = Descriptor.repository_descriptor,

    branch_head_commit(Repository_Descriptor,
                       "main",
                       Commit_Uri),
    commit_uri_to_history_commit_ids(Repository_Descriptor, Commit_Uri, [Commit_A_Id, Commit_B_Id]),

    commit_id_to_metadata(Repository_Descriptor, Commit_A_Id, _, "commit a", _),
    commit_id_to_metadata(Repository_Descriptor, Commit_B_Id, _, "commit b", _),

    true.
test(commit_history_uris,
     [setup((setup_temp_store(State),
             create_db_without_schema("admin", "testdb")
            )),
      cleanup(teardown_temp_store(State))]) :-
    resolve_absolute_string_descriptor("admin/testdb", Descriptor),
    create_context(Descriptor, commit_info{author:"test",message: "commit a"}, Commit_A_Context),
    with_transaction(Commit_A_Context,
                     ask(Commit_A_Context,
                         insert(a,b,c)),
                     _),
    create_context(Descriptor, commit_info{author:"test",message: "commit b"}, Commit_B_Context),
    with_transaction(Commit_B_Context,
                     ask(Commit_B_Context,
                         insert(d,e,f)),
                     _),

    Repository_Descriptor = Descriptor.repository_descriptor,

    branch_head_commit(Repository_Descriptor,
                       "main",
                       Commit_Uri),
    commit_uri_to_history_commit_uris(Repository_Descriptor, Commit_Uri, [Commit_A_Uri, Commit_B_Uri]),

    commit_uri_to_metadata(Repository_Descriptor, Commit_A_Uri, _, "commit a", _),
    commit_uri_to_metadata(Repository_Descriptor, Commit_B_Uri, _, "commit b", _),

    true.

:- end_tests(commit_history).
