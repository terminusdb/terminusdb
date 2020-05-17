:- module(db_pack, [
              context_repository_head_pack/3,
              repository_head_layerid/2,
              unpack/1
          ]).

:- use_module(library(terminus_store)).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(triple)).

context_repository_head_pack(Context, Repo_Head_Option, Pack) :-
    % NOTE: Check to see that commit is in the history of Descriptor
    context_repository_layerids(Context, Repo_Head_Option, Layer_Ids),
    storage(Store),
    store_layerids_pack(Store,Layer_Ids,Pack).
    % For now just sent back the string representing the history

% STUB!
store_layerids_pack(_Store, Layer_Ids, Pack) :-
    format(string(Pack),'Layer Ids: ~q', [Layer_Ids]).

:- use_module(core(transaction)).
context_repository_layerids(Context, Repo_Head_Option, Layer_Ids) :-
    % Should only be one instance object
    [Transaction_Object] = (Context.transaction_objects),
    [Read_Write_Obj] = (Transaction_Object.instance_objects),
    Layer = (Read_Write_Obj.read),
    child_parents_until(Layer, Layers, Repo_Head_Option),
    maplist(layer_layerids, Layers, Layer_Ids_List),
    append(Layer_Ids_List, Layer_Ids).

child_parents_until(Child, [], just(Child_ID)) :-
    layer_to_id(Child, Child_ID),
    !.
child_parents_until(Child, [Child|Layer], Until) :-
    parent(Child,Parent), % has a parent
    !,
    child_parents_until(Parent, Layer, Until).
child_parents_until(Child, [Child], _Until).

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

layerids_and_parents_fringe([],[]).
layerids_and_parents_fringe([Layer_ID-Parent_ID|Remainder], Fringe) :-
    member(Parent_ID-_,Remainder),
    !,
    layerids_and_parents_fringe(Remainder,Fringe).
layerids_and_parents_fringe([Layer_ID-Parent_ID|Remainder], [Parent_ID|Fringe]) :-
    layerids_and_parents_fringe(Remainder,Fringe).

layerids_unknown(Layer_Ids,Unknown_Layer_Ids) :-
    exclude(layer_exists,Layer_Ids,Unknown_Layer_Ids).

unpack(Pack) :-
   pack_layerids_and_parents(Pack,Layer_Parents),
    % all layers and their parents [Layer_ID-Parent_ID,....]
    % Are these valid? Parent is a Layer in the list or we have the parent.
   layerids_and_parents_fringe(Layer_Parents,Fringe),
   assert_fringe_is_known(Fringe),
   % Filter this list to layers we don't know about
   findall(L, member(L-,Layer_Parents), Layer_Ids),
   layerids_unknown(Layer_Ids, Unknown_Layer_Ids),
   % Extract only these layers.
   storage(Store),
   forall( member(Layer_Id, Unknown_Layer_Ids), extract(Store,Layer_Id,Pack)).


