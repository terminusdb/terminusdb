:- module(ref_entity, [
              has_branch/2,
              branch_name_uri/3,
              branch_base_uri/3,
              branch_head_commit/3,
              has_commit/2,
              commit_id_uri/3,
              commit_to_metadata/5,
              commit_to_parent/3,
              graph_for_commit/5,
              layer_uri_for_graph/3,
              insert_branch_object/4,
              insert_base_commit_object/3,
              insert_base_commit_object/4,
              insert_base_commit_object/5,
              insert_child_commit_object/4,
              insert_child_commit_object/5,
              insert_child_commit_object/6,
              insert_commit_object_on_branch/4,
              insert_commit_object_on_branch/5,
              insert_commit_object_on_branch/6,
              link_commit_object_to_branch/3,
              insert_graph_object/7
          ]).
:- use_module(core(util)).
:- use_module(core(query)).

:- use_module(layer_entity).

has_branch(Askable, Branch_Name) :-
    ask(Askable,
        t(_, ref:branch_name, Branch_Name^^xsd:string)).

branch_name_uri(Askable, Branch_Name, Branch_Uri) :-
    once(ask(Askable,
             t(Branch_Uri, ref:branch_name, Branch_Name^^xsd:string))).

branch_base_uri(Askable, Branch_Name, Base_Uri) :-
    branch_name_uri(Askable, Branch_Name, Branch_Uri),
    once(ask(Askable,
             t(Branch_Uri, ref:branch_base_uri, Base_Uri^^xsd:anyURI))).

branch_head_commit(Askable, Branch_Name, Commit_Uri) :-
    branch_name_uri(Askable, Branch_Name, Branch_Uri),
    once(ask(Askable,
             t(Branch_Uri, ref:ref_commit, Commit_Uri))).

has_commit(Askable, Commit_Id) :-
    ground(Commit_Id), % todo make this an error-throwing assertion
    commit_id_uri(Askable, Commit_Id, _).

commit_id_uri(Askable, Commit_Id, Commit_Uri) :-
    once(ask(Askable,
             t(Commit_Uri, ref:commit_id, Commit_Id^^xsd:string))).

commit_to_metadata(Askable, Commit_Id, Author, Message, Timestamp) :-
    commit_id_uri(Askable, Commit_Id, Commit_Uri),
    once(ask(Askable,
             (   t(Commit_Uri, ref:commit_author, Author^^xsd:string),
                 t(Commit_Uri, ref:commit_message, Message^^xsd:string),
                 t(Commit_Uri, ref:commit_timestamp, Timestamp_String^^xsd:decimal)))),
    number_string(Timestamp, Timestamp_String).

commit_to_parent(Askable, Commit_Id, Parent_Commit_Uri) :-
    commit_id_uri(Askable, Commit_Id, Commit_Uri),
    once(ask(Askable,
             t(Commit_Uri, ref:commit_parent, Parent_Commit_Uri))).

graph_for_commit(Askable, Commit_Uri, Type, Name, Graph_Uri) :-
    ask(Askable,
        (   t(Commit_Uri, ref:Type, Graph_Uri),
            t(Graph_Uri, ref:graph_name, Name^^xsd:string))).

layer_uri_for_graph(Askable, Graph_Uri, Layer_Uri) :-
    once(ask(Askable,
             (   t(Graph_Uri, ref:graph_layer, Layer_Uri)))).

insert_branch_object(Context, Branch_Name, Base_Uri, Branch_Uri) :-
    once(ask(Context,
             (   idgen(doc:'Branch', [Branch_Name^^xsd:string], Branch_Uri),
                 insert(Branch_Uri, rdf:type, ref:'Branch'),
                 insert(Branch_Uri, ref:branch_base_uri, Base_Uri^^xsd:anyURI),
                 insert(Branch_Uri, ref:branch_name, Branch_Name^^xsd:string)))).

insert_base_commit_object(Context, Commit_Id, Commit_Uri) :-
    insert_base_commit_object(Context, Context.commit_info, Commit_Id, Commit_Uri).
insert_base_commit_object(Context, Commit_Info, Commit_Id, Commit_Uri) :-
    get_time(Now),
    insert_base_commit_object(Context, Commit_Info, Now, Commit_Id, Commit_Uri).
insert_base_commit_object(Context, Commit_Info, Timestamp, Commit_Id, Commit_Uri) :-
    (   var(Commit_Id)
    ->  random_string(Commit_Id)
    ;   true),
    format(string(Timestamp_String), '~q', [Timestamp]),
    (   var(Commit_Uri)
    ->  once(ask(Context, idgen(doc:'Commit', [Commit_Id^^xsd:string], Commit_Uri)))
    ;   true),


    once(ask(Context,
             (   insert(Commit_Uri, rdf:type, ref:'Commit'),
                 insert(Commit_Uri, ref:commit_id, Commit_Id^^xsd:string),
                 insert(Commit_Uri, ref:commit_author, Commit_Info.author^^xsd:string),
                 insert(Commit_Uri, ref:commit_message, Commit_Info.message^^xsd:string),
                 insert(Commit_Uri, ref:commit_timestamp, Timestamp_String^^xsd:decimal)))).

insert_child_commit_object(Context, Parent_Commit_Uri, Commit_Id, Commit_Uri) :-
    insert_child_commit_object(Context, Parent_Commit_Uri, Context.commit_info, Commit_Id, Commit_Uri).
insert_child_commit_object(Context, Parent_Commit_Uri, Commit_Info, Commit_Id, Commit_Uri) :-
    get_time(Now),
    insert_child_commit_object(Context, Parent_Commit_Uri, Commit_Info, Now, Commit_Id, Commit_Uri).
insert_child_commit_object(Context, Parent_Commit_Uri, Commit_Info, Timestamp, Commit_Id, Commit_Uri) :-
    insert_base_commit_object(Context, Commit_Info, Timestamp, Commit_Id, Commit_Uri),
    once(ask(Context,
             insert(Commit_Uri, ref:commit_parent, Parent_Commit_Uri))).

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
                 delete(Branch_Uri, ref:ref_commit, Old_Commit_Uri))),
        insert_child_commit_object(Context, Old_Commit_Uri, Commit_Info, Timestamp, Commit_Id, Commit_Uri)
    % if branch does not yet point at something, insert a base commit pointing at no parent
    ;   insert_base_commit_object(Context, Commit_Info, Timestamp, Commit_Id, Commit_Uri)),

    % in both cases, we need to insert the new reference to the commit
    link_commit_object_to_branch(Context, Branch_Uri, Commit_Uri).

link_commit_object_to_branch(Context, Branch_Uri, Commit_Uri) :-
    once(ask(Context,
             insert(Branch_Uri, ref:ref_commit, Commit_Uri))).

attach_graph_to_commit(Context, Commit_Uri, Graph_Type, Graph_Name, Graph_Uri) :-
    once(ask(Context,
             (   insert(Commit_Uri, ref:Graph_Type, Graph_Uri),
                 insert(Graph_Uri, ref:graph_name, Graph_Name^^xsd:string)))).

attach_layer_to_graph(Context, Graph_Uri, Graph_Layer_Uri) :-
    once(ask(Context,
             insert(Graph_Uri, ref:graph_layer, Graph_Layer_Uri))).

insert_graph_object(Context, Commit_Uri, Commit_Id, Graph_Type, Graph_Name, Graph_Layer_Uri, Graph_Uri) :-
    once(ask(Context,
             (   idgen(doc:'Graph', [Commit_Id^^xsd:string,
                                     Graph_Type^^xsd:string,
                                     Graph_Name^^xsd:string], Graph_Uri),
                 insert(Graph_Uri, rdf:type, ref:'Graph')))),
    attach_graph_to_commit(Context, Commit_Uri, Graph_Type, Graph_Name, Graph_Uri),

    % also attach a layer if it is there
    (   ground(Graph_Layer_Uri)
    ->  attach_layer_to_graph(Context, Graph_Uri, Graph_Layer_Uri)
    ;   true).

copy_graph_object(Origin_Context, Destination_Context, Graph_Uri) :-
    once(ask(Destination_Context,
             insert(Graph_Uri, rdf:type, ref:'Graph'))),

    (   layer_uri_for_graph(Origin_Context, Graph_Uri, Layer_Uri)
    ->  layer_id_uri(Origin_Context, Layer_Id, Layer_Uri),
        insert_layer_object(Destination_Context, Layer_Id, Layer_Uri),
        attach_layer_to_graph(Destination_Context, Graph_Uri, Layer_Uri)
    ;   true).

copy_commit(Origin_Context, Destination_Context, Commit_Id) :-
    commit_id_uri(Origin_Context,
                  Commit_Id,
                  Commit_Uri),
    commit_to_metadata(Origin_Context, Commit_Id, Author, Message, Timestamp),
    Commit_Info = commit_info{author: Author, message: Message},

    (   commit_to_parent(Origin_Context, Commit_Id, Parent_Uri)
    ->  insert_child_commit_object(Destination_Context,
                                   Parent_Uri,
                                   Commit_Info,
                                   Commit_Id,
                                   Timestamp,
                                   Commit_Uri)
    ;   insert_base_commit_object(Destination_Context,
                                  Commit_Info,
                                  Timestamp,
                                  Commit_Id,
                                  Commit_Uri)),

    forall(graph_for_commit(Origin_Context, Commit_Uri, Type, Graph_Name, Graph_Uri),
           (   copy_graph_object(Origin_Context, Destination_Context, Graph_Uri),
               attach_graph_to_commit(Destination_Context, Commit_Uri, Type, Graph_Name, Graph_Uri))).

copy_commits_(Origin_Context, Destination_Context, Commit_Id) :-
    (   has_commit(Destination_Context, Commit_Id)
    ->  true
    ;   copy_commit(Origin_Context, Destination_Context, Commit_Id),
        (   commit_to_parent(Origin_Context, Commit_Id, Parent_Uri)
        ->  commit_id_uri(Origin_Context, Parent_Id, Parent_Uri),
            copy_commits_(Origin_Context, Destination_Context, Parent_Id)
        ;   true)).

copy_commits(Origin_Context, Destination_Context, Commit_Id) :-
    context_default_prefixes(Origin_Context, Origin_Context_Stripped),
    context_default_prefixes(Destination_Context, Destination_Context_Stripped),
    copy_commits_(Origin_Context_Stripped, Destination_Context_Stripped, Commit_Id).

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
                     (   insert_branch_object(Context, "foo", "terminus://base1", _),
                         insert_branch_object(Context, "bar", "terminus://base2", _),
                         insert_branch_object(Context, "baz", "terminus://base2", _)),
                     _),

    findall(Branch_Name-Base_Uri,
            (   has_branch(Descriptor, Branch_Name),
                branch_base_uri(Descriptor, Branch_Name, Base_Uri)),
            Branches),

    Branches = ["bar"-"terminus://base2",
                "baz"-"terminus://base2",
                "foo"-"terminus://base1"].
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

    commit_to_metadata(Descriptor, Commit_Id, Author, Message, Timestamp),

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

    commit_to_metadata(Descriptor, Commit_Id, Author, Message, Timestamp),

    Author = "author2",
    Message = "message2",
    Timestamp = 2345.678,

    commit_to_parent(Descriptor, Commit_Id, Parent_Commit_Uri).

test(commit_on_branch_insert,
     [setup((setup_temp_store(State),
             ensure_label(testlabel))),
      cleanup(teardown_temp_store(State))]
    ) :-
    Descriptor = label_descriptor{label:"testlabel"},

    % first set up the branch
    ref_schema_context_from_label_descriptor(Descriptor, Context1),
    with_transaction(Context1,
                     insert_branch_object(Context1, "foo", "terminus://base1", _),
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
    (   commit_to_parent(Descriptor, Commit_Id, _)
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
    commit_to_parent(Descriptor, Commit2_Id, _).

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
    Layer3_Id = "a3a29522ec767aa1a1cf321122f833726c102749",
    with_transaction(Context1_1,
                     (
                         insert_base_commit_object(Context1_1, commit_info{author: "author", message: "message"}, 'commit_id', Commit_Uri),
                         insert_layer_object(Context1_1,
                                             Layer1_Id,
                                             Layer1_Uri),
                         insert_layer_object(Context1_1,
                                             Layer2_Id,
                                             Layer2_Uri),
                         insert_layer_object(Context1_1,
                                             Layer3_Id,
                                             Layer3_Uri),
                         insert_graph_object(Context1_1, Commit_Uri, "commit_id", instance, "graph_1", Layer1_Uri, Graph1_Uri),
                         insert_graph_object(Context1_1, Commit_Uri, "commit_id", instance, "graph_2", Layer2_Uri, Graph2_Uri),
                         insert_graph_object(Context1_1, Commit_Uri, "commit_id", schema, "graph_3", Layer3_Uri, Graph3_Uri)
                     ),
                     _),


    ref_schema_context_from_label_descriptor(Descriptor1, Context1_2),
    ref_schema_context_from_label_descriptor(Descriptor2, Context2),

    with_transaction(Context2,
                     copy_commits(Context1_2, Context2, "commit_id"),
                     _),

    prefixed_to_uri(Commit_Uri, Context1_1.prefixes, Commit_Uri_Unprefixed),
    prefixed_to_uri(Graph1_Uri, Context1_1.prefixes, Graph1_Uri_Unprefixed),
    prefixed_to_uri(Graph2_Uri, Context1_1.prefixes, Graph2_Uri_Unprefixed),
    prefixed_to_uri(Graph3_Uri, Context1_1.prefixes, Graph3_Uri_Unprefixed),
    prefixed_to_uri(Layer1_Uri, Context1_1.prefixes, Layer1_Uri_Unprefixed),
    prefixed_to_uri(Layer2_Uri, Context1_1.prefixes, Layer2_Uri_Unprefixed),
    prefixed_to_uri(Layer3_Uri, Context1_1.prefixes, Layer3_Uri_Unprefixed),

    commit_id_uri(Descriptor2, "commit_id", Commit_Uri_Unprefixed),

    findall(Type-Name-Graph_Uri-Layer_Uri-Layer_Id,
            (   graph_for_commit(Descriptor2, Commit_Uri_Unprefixed, Type, Name, Graph_Uri),
                layer_uri_for_graph(Descriptor2, Graph_Uri, Layer_Uri),
                layer_id_uri(Descriptor2, Layer_Id, Layer_Uri)),
            Graphs),
    Expected = [
        instance-"graph_1"-Graph1_Uri_Unprefixed-Layer1_Uri_Unprefixed-Layer1_Id,
        instance-"graph_2"-Graph2_Uri_Unprefixed-Layer2_Uri_Unprefixed-Layer2_Id,
        schema-"graph_3"-Graph3_Uri_Unprefixed-Layer3_Uri_Unprefixed-Layer3_Id
    ],

    list_to_ord_set(Graphs, Graph_Set),
    list_to_ord_set(Expected, Expected_Set),
    writeq(Graph_Set),nl,
    writeq(Expected_Set),nl,

    ord_seteq(Graph_Set, Expected_Set).
:- end_tests(copy_commits).
