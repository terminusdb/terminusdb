:- module(db_pack, [
              repository_context__previous_head_option__current_repository_head__pack/4,
              payload_repository_head_and_pack/3,
              repository_head_layerid/2,
              unpack/1,
              layer_layerids/2
          ]).

:- use_module(library(terminus_store)).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(triple)).

payload_repository_head_and_pack(Data, Head, Pack) :-
    ground(Head),
    ground(Pack),
    !,
    string_concat(Head,Pack,Data).
payload_repository_head_and_pack(Data, Head, Pack) :-
    ground(Data),
    !,
    sub_string(Data, 0, 40, Remaining_Length, Head),
    sub_string(Data, 40, Remaining_Length, _, Pack).

repository_context__previous_head_option__current_repository_head__pack(Repository_Context, Repo_Head_Option, Current_Repository_Head, Pack) :-
    % NOTE: Check to see that commit is in the history of Descriptor
    repository_context_to_layer(Repository_Context, Layer),
    layer_to_id(Layer, Current_Repository_Head),
    repository_layer_to_layerids(Layer, Repo_Head_Option, Layer_Ids),
    storage(Store),
    pack_export(Store,Layer_Ids,Pack).
    % For now just sent back the string representing the history

repository_context_to_layer(Repository_Context,Layer) :-
    [Transaction_Object] = (Repository_Context.transaction_objects),
    [Read_Write_Obj] = (Transaction_Object.instance_objects),
    Layer = (Read_Write_Obj.read).

repository_layer_to_layerids(Layer, Repo_Head_Option, Layer_Ids) :-
    % Should only be one instance object
    child_until_parents(Layer, Repo_Head_Option, Layers),
    maplist(layer_layerids, Layers, Layer_Ids_List),
    append(Layer_Ids_List, Layer_Ids).

child_until_parents(Child, some(Child_ID), []) :-
    layer_to_id(Child, Child_ID),
    !.
child_until_parents(Child, Until, [Child|Layer]) :-
    parent(Child,Parent), % has a parent
    !,
    child_until_parents(Parent, Until, Layer).
child_until_parents(Child, _Until, [Child]).

% Include self!
layer_layerids(Layer, [Self_Layer_Id|Layer_Ids]) :-
    layer_to_id(Layer,Self_Layer_Id),
    findall(Layer_Id,
            ask(Layer,
                addition(_, layer:layer_id, Layer_Id^^xsd:string)),
            Layer_Ids).

repository_head_layerid(Repository,Layer_ID) :-
    [Read_Write_Obj] = (Repository.instance_objects),
    Layer = (Read_Write_Obj.read),
    layer_to_id(Layer,Layer_ID).

layer_exists(Layer_ID) :-
    storage(Store),
    store_id_layer(Store,Layer_ID,_).

assert_fringe_is_known([]).
assert_fringe_is_known([Layer_ID|Rest]) :-
    (   layer_exists(Layer_ID)
    ->  true
    ;   throw(error(unknown_layer_reference(Layer_ID)))),
    assert_fringe_is_known(Rest).

layerids_and_parents_fringe(Layerids_Parents,Fringe) :-
    layerids_and_parents_fringe_(Layerids_Parents,Layerids_Parents,Fringe).

layerids_and_parents_fringe_([],_,[]).
layerids_and_parents_fringe_([_-some(Parent_ID)|Remainder], Layerids_Parents, Fringe) :-
    member(Parent_ID-_,Layerids_Parents),
    !,
    layerids_and_parents_fringe_(Remainder,Layerids_Parents,Fringe).
layerids_and_parents_fringe_([_-some(Parent_ID)|Remainder], Layerids_Parents, [Parent_ID|Fringe]) :-
    !,
    layerids_and_parents_fringe_(Remainder,Layerids_Parents,Fringe).
layerids_and_parents_fringe_([_-none|Remainder], Layerids_Parents, Fringe) :-
    layerids_and_parents_fringe_(Remainder,Layerids_Parents,Fringe).

layerids_unknown(Layer_Ids,Unknown_Layer_Ids) :-
    exclude(layer_exists,Layer_Ids,Unknown_Layer_Ids).

unpack(Pack) :-
   pack_layerids_and_parents(Pack,Layer_Parents),
   % all layers and their parents [Layer_ID-Parent_ID,....]
   % Are these valid? Parent is a Layer in the list or we have the parent.
   layerids_and_parents_fringe(Layer_Parents,Fringe),
   assert_fringe_is_known(Fringe),
   % Filter this list to layers we don't know about
   findall(L, member(L-_,Layer_Parents), Layer_Ids),
   layerids_unknown(Layer_Ids, Unknown_Layer_Ids),
   % Extract only these layers.
   storage(Store),
   pack_import(Store,Unknown_Layer_Ids,Pack).


:- begin_tests(pack).
:- use_module(core(util/test_utils)).
:- use_module(db_branch).

test(context_repository_head_pack,
     [blocked('Probably also a store build problem'),
      setup((setup_temp_store(State),
             create_db_without_schema('user|foo','test','a test'))),
      cleanup(teardown_temp_store(State))
     ]
    ) :-

    Repository_Descriptor = repository_descriptor{
                                database_descriptor: database_descriptor{
                                                         database_name: 'user|foo'
                                                     },
                                repository_name: "local"
                            },
    Origin_Branch_Descriptor = branch_descriptor{
                                   repository_descriptor: Repository_Descriptor,
                                   branch_name: "master"
                               },

    create_context(Origin_Branch_Descriptor, commit_info{author:"test", message:"first commit"}, Context1),
    with_transaction(Context1,
                     once(ask(Context1, insert(foo,bar,baz))),
                     _),

    open_descriptor(database_descriptor{ database_name: 'user|foo' },
                    Database_Transaction),

    repository_head(Database_Transaction, "local", Repo_Stage_1_Layer_ID),

    create_context(Origin_Branch_Descriptor, commit_info{author:"test", message:"second commit"}, Context2),
    with_transaction(Context2,
                     once(ask(Context2, insert(baz,bar,foo))),
                     _),

    branch_create(Repository_Descriptor, Origin_Branch_Descriptor, "moo", _),

    create_context(Repository_Descriptor, Repository_Context),

    repository_context__previous_head_option__current_repository_head__pack(
        Repository_Context,
        some(Repo_Stage_1_Layer_ID),
        _Repository_Head,
        Pack),

    pack_layerids_and_parents(Pack,Layerids_and_Parents),
    writeq(Layerids_and_Parents),
    nl,
    true.

:- end_tests(pack).
