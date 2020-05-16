:- module(db_pack, [
              context_repository_head_pack/3,
              repository_head_layerid/2
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
    child_parents_until(Layer, Layer_Ids, Repo_Head_Option).

child_parents_until(Child, [], Until) :-
    layer_to_id(Child, Child_ID),
    just(Child_ID) = Until,
    !.
child_parents_until(Child, [Child_ID|Layer], Until) :-
    parent(Child,Parent), % has a parent
    !,
    layer_to_id(Child, Child_ID),
    child_parents_until(Parent, Layer, Until).
child_parents_until(Child, [Child_ID], _Until) :-
    % \+ parent(Child,Parent)
    % has no parent
    layer_to_id(Child, Child_ID).

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

