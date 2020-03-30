:- module(db_graph, [create_graph/5]).
:- use_module(core(util)).
:- use_module(core(transaction)).
:- use_module(core(query)).

graph_for_commit(Askable, Commit_Uri, Type, Name, Layer_Uri) :-
    ask(Askable,
        (   t(Commit_Uri, ref:Type, Graph_Uri),
            t(Graph_Uri, ref:graph_name, Name^^xsd:string),
            (   t(Graph_Uri, ref:graph_layer, Layer_Uri)
            ;   true))).

insert_commit_object(Context, Commit_Uri) :-
    random_string(Commit_Id),
    once(ask(Context,
             (   idgen(doc:'Commit',[Commit_Id], Commit_Uri),
                 insert(Commit_Uri, rdf:type, ref:'Commit'),
                 insert(Commit_Uri, ref:commit_id, Commit_Id^^xsd:string),
                 insert(Commit_Uri, ref:commit_author, Context.commit_info.author^^xsd:string),
                 insert(Commit_Uri, ref:commit_message, Context.commit_info.message^^xsd:string),
                 timestamp_now(Now),
                 insert(Commit_Uri, ref:commit_timestamp, Now)))).

insert_graph_object(Context, Commit_Uri, Commit_Id, Graph_Type, Graph_Name, Graph_Layer_Uri, Graph_Uri) :-
    once(ask(Context,
             (   idgen(doc:'Graph', [Commit_Id, Graph_Type, Graph_Name], Graph_Uri),
                 insert(Commit_Uri, ref:Graph_Type, Graph_Uri),
                 insert(Graph_Uri, ref:graph_name, Graph_Name^^xsd:string)))),

    % also attach a layer if it is there
    (   ground(Graph_Uri)
    ->  once(ask(Context,
                 insert(Graph_Uri, ref:graph_layer, Graph_Layer_Uri)))
    ;   true).


create_graph(Branch_Descriptor, Commit_Info, Graph_Type, Graph_Name, Transaction_Metadata) :-
    memberchk(Graph_Type, [instance, schema, inference]),
    branch_descriptor{repository_descriptor:Repo_Descriptor, branch_name: Branch_Name} :< Branch_Descriptor,

    (   create_context(Repo_Descriptor, Commit_Info, Context)
    ->  true
    ;   throw(error(cannot_open_context(Repo_Descriptor)))),
    
    with_transaction(Context,
                     (   % does this branch exist? if not, error
                         (   ask(Context,
                                 t(Branch_Uri, ref:branch_name, Branch_Name^^xsd:string))
                         ->  true
                         ;   throw(error(branch_does_not_exist(Branch_Descriptor)))),
                         % does this branch already have a commit?
                         (   ask(Context,
                                 t(Branch_Uri, ref:ref_commit, Commit_Uri))
                          % it does! collect graph objects we'll need to re-insert on a new commit
                         ->  findall(Graph_Type-Graph_Name-Graph_Layer_Uri,
                                     graph_for_commit(Commit_Uri, Graph_Type, Graph_Name, Graph_Layer_Uri),
                                     Graphs)
                          % it doesn't! assume a single instance main graph
                         ;   Graphs = ["instance"-"main"-_]),
                             
                         % does the graph exist already? if so, error
                         (   memberchk(Graph_Type-Graph_Name-_, Graphs)
                         ->  throw(error(graph_already_exists(Branch_Descriptor, Graph_Name)))
                         ;   true),
                         
                         % now that we know we're in a good position, create a new commit
                         insert_commit_object(Context, Commit_Uri),
                         forall(member(Graph_Type-Graph_Name-Graph_Layer_Uri, Graphs),
                                insert_graph_object(Context, Commit_Uri, Graph_Type, Graph_Name, Graph_Layer_Uri, _Graph_Uri))
                     ),
                     Transaction_Metadata).
