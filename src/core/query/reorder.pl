:- module('query/reorder',[
              optimize_read_order/2
          ]).

:- use_module(core(util)).
:- use_module(core(query/partition), [partition/3]).

is_var(v(_)).

non_var(X) :- \+ is_var(X).

term_vars(v(X), Vars) =>
    Vars = [X].
term_vars(Term, Vars) =>
    Term =.. [_|Rest],
    maplist(term_vars, Rest, Vars_Lists),
    append(Vars_Lists, Vars).

po(t(X, P, Y), t(_A, _Q, _B)),
non_var(X), non_var(P), non_var(Y) =>
    true.
po(t(X, P, _Y), t(_A, _Q, B)),
non_var(X), non_var(P),
is_var(B) =>
    true.
po(t(_X, P, Y), t(A, Q, _B)),
non_var(P), non_var(Y),
(   is_var(A)
;   is_var(Q)) =>
    true.
po(t(X, _P, _Y), t(A, Q, B)),
non_var(X),
(   is_var(A),
    is_var(Q)
;   is_var(A),
    is_var(B)
;   is_var(B),
    is_var(Q)) =>
    true.
po(t(_X, _P, Y), t(A, Q, B)),
non_var(Y),
(   is_var(A),
    is_var(Q)
;   is_var(A),
    is_var(B)
;   is_var(B),
    is_var(Q)) =>
    true.
po(t(_X, P, _Y), t(A, _Q, B)),
non_var(P),
is_var(A),
is_var(B) =>
    true.
po(t(_X, _P, _Y), t(A, Q, B)),
is_var(A),
is_var(B),
is_var(Q) =>
    true.
po(t(_X, _P, _Y), t(_A, _B, _C)) =>
    false.
po(t(_X, _P, _Y), Term),
term_vars(Term, Vars),
Vars = [] =>
    false.
po(Term, t(_X, _P, _Y)),
term_vars(Term, Vars) =>
    Vars = [].
po(TermA, TermB),
term_vars(TermA, VarsA),
term_vars(TermB, VarsB),
length(VarsA,NA),
length(VarsB,NB),
NA < NB =>
    true.
po(TermA, TermB) =>
    compare((<), TermA, TermB).

po_comp(Op, X, Y) :-
    (   po(X, Y)
    ->  Op = (<)
    ;   po(Y, X)
    ->  Op = (>)
    ;   compare(Op,X,Y)
    ).

sort_definedness(Terms, Sorted) :-
    predsort(po_comp, Terms, Sorted).

order_conjuncts(Terms, Sorted) :-
    sort_definedness(Terms, Def_Sort),
    sort_bound(Def_Sort, Bound),
    !,
    undummy_bind(Bound, Sorted).

metasub(v(X), Vars, XO),
memberchk(X, Vars) =>
    XO = mv(X).
metasub(select(Vars,_Query), Vars, _Result) =>
    throw(error(unimplemented)).
metasub(Term, Vars, Result) =>
    Term =.. [F|Args],
    maplist({Vars}/[Arg,New]>>metasub(Arg, Vars, New),
            Args,
            New_Args),
    Result =.. [F|New_Args].

metaunsub(mv(X), XO) =>
    XO = v(X).
metaunsub(Term, Result) =>
    Term =.. [F|Args],
    maplist([Arg,New]>>metaunsub(Arg, New),
            Args,
            New_Args),
    Result =.. [F|New_Args].

undummy_bind([], []).
undummy_bind([Term|Terms], [Unbound|Unbounds]) :-
    Term =.. [F|Args],
    maplist(metaunsub, Args, Unsubed),
    Unbound =.. [F|Unsubed],
    undummy_bind(Terms, Unbounds).

dummy_bind([], _, Terms) =>
    Terms = [].
dummy_bind([Term|Terms], Vars, Results) =>
    metasub(Term,Vars,TermSub),
    Results = [TermSub|Bound],
    dummy_bind(Terms,Vars,Bound).

flatten_conjuncts((A,B), F) =>
    flatten_conjuncts(A,AF),
    flatten_conjuncts(B,BF),
    append(AF,BF,F).
flatten_conjuncts(Term, F) =>
    F = [Term].

deep_order((A;B), Deep) =>
    deep_order(A, DA),
    deep_order(B, DB),
    Deep = (DA;DB).
deep_order(group_by(Selection,Template,Query,Result),Deep) =>
    deep_order(Query, DQuery),
    Deep = group_by(Selection,Template,DQuery,Result).
deep_order((A,B),Deep) =>
    flatten_conjuncts((A,B), Flat),
    order_conjuncts(Flat, Ordered),
    xfy_list(',', Deep, Ordered).
deep_order(Term, Deep) =>
    Term = Deep.

sort_bound([], []).
sort_bound([Term|Terms], [Deep|Sorted]) :-
    deep_order(Term, Deep),
    term_vars(Deep, Vars),
    dummy_bind(Terms, Vars, Bound),
    sort_bound(Bound, Sorted).

optimize_read_order(Read, Ordered) :-
    order_conjuncts(Read, Ordered).

:- begin_tests(reorder_query).

test(reorder, []) :-

    Term = (
        t(a,b,v(c)),
        (   t(v(c),f,g),
            (   insert(a,b,c),
                insert(d,b,c),
                (   insert(e,f,g),
                    insert(f,g,h))))),

    partition(Term,Reads_Unordered,_Writes),
    optimize_read_order(Reads_Unordered, Reads),
    xfy_list(',', Prog, Reads),

    Prog = (t(a,b,v(c)),t(v(c),f,g)).

test(reorder_partitioned, []) :-

    Term = (
        (   t(a,b,v(c)),
            t(v(c),f,v(a))),
        (   t(v(c),f,v(b)),
            t(v(b),h,v(c))
        )
    ),
    partition(Term,Reads_Unordered,_Writes),
    optimize_read_order(Reads_Unordered, Reads),
    xfy_list(',', Prog, Reads),
    Prog = (
        t(a,b,v(c)),
        t(v(c),f,v(a)),
        t(v(c),f,v(b)),
        t(v(b),h,v(c))
    ).

test(reorder_partitioned_backwards, []) :-

    Term = (
        t(v(c),f,v(b)),
        t(v(c),f,v(a)),
        t(v(b),h,v(c)),
        t(a,b,v(c))
    ),
    partition(Term,Reads_Unordered,_Writes),
    optimize_read_order(Reads_Unordered, Reads),
    xfy_list(',', Prog, Reads),

    Prog = (
        t(a,b,v(c)),
        t(v(c),f,v(b)),
        t(v(c),f,v(a)),
        t(v(b),h,v(c))
    ).

test(reorder_ors, []) :-

    Term = (
        t(v(c),f,v(b)),
        t(v(c),f,v(a)),
        (    t(v(e),f,v(a)),
             t(c,f,v(a))
        ;    t(v(e),f,v(a)),
             t(d,f,v(a))
        ),
        t(v(b),h,v(c)),
        t(a,b,v(c))
    ),
    partition(Term,Reads_Unordered,_Writes),
    optimize_read_order(Reads_Unordered, Reads),
    xfy_list(',', Prog, Reads),

    Prog = (
        t(a,b,v(c)),
        t(v(c),f,v(b)),
        t(v(c),f,v(a)),
        t(v(b),h,v(c)),
        (   t(c,f,v(a)),
            t(v(e),f,v(a))
        ;   t(d,f,v(a)),
            t(v(e),f,v(a))
		)
    ).


:- end_tests(reorder_query).
