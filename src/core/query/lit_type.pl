:- module(lit_type,
          [lit_type/2,
           lit_lang/2
          ]
         ).

:- use_module(core(triple/base_type)).

lit_type(X, Lit_Type) :-
    var(X),
    var(Lit_Type),
    !,
    get_attr(X, lit_type, type(Lit_Type)).
lit_type(X, Lit_Type) :-
    put_attr(Y, lit_type, type(Lit_Type)),
    X = Y.

lit_lang(X, Lit_Type) :-
    var(X),
    var(Lit_Type),
    !,
    get_attr(X, lit_type, lang(Lit_Type)).
lit_lang(X, Lit_Type) :-
    put_attr(Y, lit_type, lang(Lit_Type)),
    X = Y.

attr_unify_hook(type(Lit_Type), Y) :-
    nonvar(Y),
    !,
    Y = _^^Lit_Type.
attr_unify_hook(lang(Lang), Y) :-
    nonvar(Y),
    !,
    Y = _@Lang.
attr_unify_hook(Lit_Type, Y) :-
    format(user_error, 'Lit_Type: ~q against ~q~n', [Lit_Type, Y]),
    (   get_attr(Y, lit_type, Internal_Lit_Type)
    ->  (   Internal_Lit_Type = type(IT)
        ->  Lit_Type = type(T),
            base_type(T),
            (   basetype_subsumption_of(IT, T)
            ->  put_attr(Y, lit_type, Internal_Lit_Type)
            ;   basetype_subsumption_of(T, IT)
            ->  true
            ;   false
            )
        ;   Internal_Lit_Type = lang(_)
        ->  Lit_Type = Internal_Lit_Type
        ;   false
        )
    ;   put_attr(Y, lit_type, Lit_Type)
    ).

attribute_goals(X) -->
    { get_attr(X, lit_type, type(T)) },
    !,
    [X : T].
attribute_goalos(X) -->
    { get_attr(X, lit_type, lang(Lang)) },
    !,
    [X : Lang].
