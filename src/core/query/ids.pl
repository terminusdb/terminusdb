:- module(ids,
          [ids/2,
           ids/3,
           realise_value/2,
           realise_id/3,
           fix/1,
           fix_term/1,
           op(700, xfx, ∈)]
         ).
:- op(700, xfx, ∈).
:- use_module(library(terminus_store)).
:- use_module(core(triple)).

ids(Var, Ids) :-
    var(Ids),
    !,
    get_attr(Var, ids, Ids).
ids(Var, Ids) :-
    put_attr(Y, ids, Ids),
    Var = Y.

ids(Var, Graph, Id) :-
    dict_create(Ids, ids, [Graph-Id]),
    ids(Var,Ids).

id_graph_value(p(I),Graph,Value) :-
    predicate_id(Graph, Predicate, I),
    atom_string(Value, Predicate).
id_graph_value(o(I),Graph,Value) :-
    object_id(Graph, Storage, I),
    storage_object(Storage, Value).

cross_check_value(Ids, Ids) :-
    get_dict(value, Ids, _),
    !.
cross_check_value(Ids, New_Ids) :-
    dict_keys(Ids, Keys),
    (   Keys = [Key|Rest],
        Rest \= [] % no sense in cross checking ourselves.
    ->  get_dict(Key, Ids, Id),
        id_graph_value(Id, Key, Value),
        forall(
            member(Other_Key, Rest),
            (   get_dict(Other_Key, Ids, Other_Id),
                id_graph_value(Other_Id, Other_Key, Value)
            )
        ),
        put_dict(_{value:Value}, Ids, New_Ids)
    ;   Ids = New_Ids
    ).

unify_value(X, Y) :-
    atom(X), atom(Y),
    !,
    X = Y.
unify_value(X, Y) :-
    atom(X), string(Y),
    !,
    atom_string(X, Y).
unify_value(X, Y) :-
    atom(Y), string(X),
    !,
    atom_string(Y, X).
unify_value(X, X).

attr_unify_hook(Attributed_Ids, Y) :-
    nonvar(Y),
    !,
    (   id(_) = Y
    ->  true % This is a fixed id, no sense in looking it up.
    ;   get_dict(value, Attributed_Ids, Value)
    ->  unify_value(Y, Value)
    ;   get_dict(Key, Attributed_Ids, Id),
        id_graph_value(Id, Key, Value)
    ->  unify_value(Y, Value)
    ).
attr_unify_hook(Attributed_Ids, Y) :-
    (   get_attr(Y, ids, Ids)
    ->  Attributed_Ids >:< Ids,
        put_dict(Attributed_Ids, Ids, New_Ids),
        cross_check_value(New_Ids, Final_Ids),
        put_attr(Y, ids, Final_Ids)
    ;   var(Y)
    ->  put_attr(Y, ids, Attributed_Ids)
    ).

realise_value(X, Value) :-
    ground(X),
    !,
    X = Value.
realise_value(X, Value) :-
    get_attr(X, ids, Ids),
    !,
    (   get_dict(value, Ids, Value)
    ->  true
    ;   (   get_dict(Key, Ids, Id),
            \+ Key = value
        ->  id_graph_value(Id, Key, Value),
            put_dict(ids{value:Value}, Ids, New_Ids),
            put_attr(X, ids, New_Ids)
        ;   true
        )
    ).

fix(X) :-
    realise_value(X, Value),
    !,
    X = Value.
fix(_).

fix_term(Term) :-
    term_variables(Term, Vars),
    maplist(fix,Vars).

realise_id(X, Graph, Id) :-
    var(X),
    !,
    (   get_attr(X, ids, Ids)
    ->  (   get_dict(Graph, Ids, Id)
        ->  true
        ;   get_dict(value, Ids, Object)
        ->  id_graph_value(Id, Graph, Object),
            ids(Y, ids{value:Object}),
            X = Y
        ;   once((get_dict(Key, Ids, Other_Id),
                  Key \= value))
        ->  id_graph_value(Other_Id, Key, Object),
            id_graph_value(Id, Graph, Object),
            dict_create(New_Ids, ids, [Key-Other_Id,value-Object]),
            ids(Y, New_Ids),
            X = Y
        ;   true
        )
    ;   true
    ).
realise_id(id(Id), _, o(Id)) :-
    !.
realise_id(id(Id), _, p(Id)) :-
    !.
realise_id(X, Graph, p(Id)) :-
    !,
    (   atom(X)
    ->  atom_string(X, S)
    ;   X = S),
    predicate_id(Graph, S, Id).
realise_id(X, Graph, o(Id)) :-
    ground(X),
    !,
    (   atom(X)
    ->  atom_string(X, S)
    ;   X = S),
    object_storage(S, Storage),
    object_id(Graph, Storage, Id).
realise_id(_X, _Graph, o(_Id)).

attribute_goals(X) -->
    { get_attr(X, ids, Ids) },
    [X ∈ Ids].
