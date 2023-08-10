:- module('query/reorder',[
              optimize_read_order/2
          ]).

:- use_module(core(util)).
:- use_module(partition, [partition/3]).
:- use_module(woql_compile, [mode_for_compound/2]).

:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(sort)).
:- use_module(library(apply_macros)).

is_var(v(_)).

non_var(X) :- \+ is_var(X).

term_vars(v(X), Vars) =>
    Vars = [X].
term_vars(List, Vars),
is_list(List) =>
    maplist(term_vars, List, Var_List),
    append(Var_List, Vars_Unsorted),
    sort(Vars_Unsorted, Vars).
term_vars(Dict, Vars),
is_dict(Dict) =>
    dict_pairs(Dict, _, Pairs),
    maplist([_-V,Var]>>term_vars(V,Var), Pairs, Var_List),
    append(Var_List, Vars_Unsorted),
    sort(Vars_Unsorted, Vars).
term_vars(not(_), Vars) =>
    Vars = [].
term_vars(select(VL, Query), Vars) =>
    term_vars(VL, VLVars),
    term_vars(Query, V),
    intersection(V,VLVars,Vars).
term_vars(group_by(Unique,_Template,Query,Result), Vars) =>
    term_vars(Unique, UVars),
    term_vars(Query, QVars),
    intersection(UVars, QVars, Both_Vars),
    term_vars(Result, RVars),
    union(Both_Vars, RVars, Vars).
term_vars(Term, Vars) =>
    Term =.. [_|Rest],
    maplist(term_vars, Rest, Vars_Lists),
    append(Vars_Lists, Vars_Unsorted),
    sort(Vars_Unsorted,Vars).

term_mvars(mv(X), MVars) =>
    MVars = [X].
term_mvars(List, MVars),
is_list(List) =>
    maplist(term_mvars, List, MVar_List),
    append(MVar_List, MVars_Unsorted),
    sort(MVars_Unsorted, MVars).
term_mvars(Dict, MVars),
is_dict(Dict) =>
    dict_pairs(Dict, _, Pairs),
    maplist([_-MV,MVar]>>term_mvars(MV,MVar), Pairs, MVar_List),
    append(MVar_List, MVars_Unsorted),
    sort(MVars_Unsorted, MVars).
term_mvars(not(_), MVars) =>
    MVars = [].
term_mvars(select(MVL, Query), MVars) =>
    term_mvars(MVL, MVLMVars),
    term_mvars(Query, MV),
    intersection(MV,MVLMVars,MVars).
term_mvars(group_by(Unique,_Template,Query,Result), MVars) =>
    term_mvars(Unique, UMVars),
    term_mvars(Query, QMVars),
    intersection(UMVars, QMVars, Both_MVars),
    term_mvars(Result, RMVars),
    union(Both_MVars, RMVars, MVars).
term_mvars(Term, MVars) =>
    Term =.. [_|Rest],
    maplist(term_mvars, Rest, MVars_Lists),
    append(MVars_Lists, MVars_Unsorted),
    sort(MVars_Unsorted,MVars).

po(t(X, P, Y), t(_A, _Q, _B)),
non_var(X), non_var(P), non_var(Y) =>
    true.
po(t(X, P, _Y), t(_A, _Q, B)),
non_var(X), non_var(P),
is_var(B) =>
    true.
po(t(_X, P, Y), t(A, Q, B)),
non_var(P), non_var(Y),
is_var(A), non_var(Q), non_var(B) =>
    (   P = rdf:type
    ->  Q = rdf:type
    ;   false
    ).
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
po(TermA, _TermB),
violates_static_mode(TermA) =>
    false.
po(_TermA, TermB),
violates_static_mode(TermB) =>
    true.
po(TermA, TermB),
term_vars(TermA, VarsA),
term_vars(TermB, VarsB),
length(VarsA,NA),
length(VarsB,NB),
NA < NB =>
    true.
po(TermA, TermB) =>
    compare((<), TermA, TermB).

violates_static_mode(TermA) :-
    mode_for_compound(TermA, Modes),
    memberchk(v(_)-ground, Modes).

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
    sort_bound(Def_Sort, Sorted).

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
metaunsub(Dict, X),
is_dict(Dict) =>
    dict_pairs(Dict, Functor, Pairs),
    maplist([P-V,P-V2]>>metaunsub(V,V2), Pairs, New_Pairs),
    dict_create(X, Functor, New_Pairs).
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
deep_order(order_by(L,Query), Deep) =>
    deep_order(Query, DQuery),
    Deep = order_by(L, DQuery).
deep_order(into(G, Query), Deep) =>
    deep_order(Query, DQuery),
    Deep = into(G, DQuery).
deep_order(count(Query, N), Deep) =>
    deep_order(Query, DQuery),
    Deep = count(DQuery, N).
deep_order(not(Query), Deep) =>
    deep_order(Query, DQuery),
    Deep = not(DQuery).
deep_order(select(Vars,Query),Deep) =>
    deep_order(Query, DQuery),
    Deep = select(Vars,DQuery).
deep_order(limit(N,Query),Deep) =>
    deep_order(Query, DQuery),
    Deep = limit(N,DQuery).
deep_order(using(G,Query),Deep) =>
    deep_order(Query, DQuery),
    Deep = using(G,DQuery).
deep_order(from(G,Query),Deep) =>
    deep_order(Query, DQuery),
    Deep = from(G,DQuery).
deep_order(start(N,Query),Deep) =>
    deep_order(Query, DQuery),
    Deep = start(N,DQuery).
deep_order(distinct(X,Query),Deep) =>
    deep_order(Query, DQuery),
    Deep = distinct(X,DQuery).
deep_order((A,B),Deep) =>
    flatten_conjuncts((A,B), Flat),
    optimize_conjuncts(Flat, Ordered),
    xfy_list(',', Deep, Ordered).
deep_order(Term, Deep) =>
    Term = Deep.

sort_bound([], []).
sort_bound([Term|Terms], [Deep|Sorted]) :-
    deep_order(Term, Deep),
    term_vars(Deep, Vars),
    dummy_bind(Terms, Vars, Bound),
    order_conjuncts(Bound, Sorted).

optimize_conjuncts(Read, Ordered) :-
    commutative_partitions(Read, Partitions),
    maplist(order_conjuncts, Partitions, Ordered_Partitions),
    append(Ordered_Partitions, Ordered).

optimize_read_order(Read, Ordered) :-
    optimize_conjuncts(Read, Ordered_Bound),
    undummy_bind(Ordered_Bound, Ordered).

non_commutative(start(_,_)) =>
    true.
non_commutative(limit(_,_)) =>
    true.
non_commutative(once(_)) =>
    true.
non_commutative(_) =>
    false.

commutative_partitions([], [[]]).
commutative_partitions([ReadHead|Rest], Partitions) :-
    (   non_commutative(ReadHead)
    ->  commutative_partitions(Rest,New_Partitions),
        Partitions = [[],[ReadHead]|New_Partitions]
    ;   commutative_partitions(Rest,[H|New_Partitions]),
        Partitions = [[ReadHead|H]|New_Partitions]
    ).

disconnected(T) :-
    term_mvars(T, MVars),
    MVars = [],
    term_vars(T, Vars),
    Vars \= [].

split_at([], [], []).
split_at([T|Rest], [], [T|Rest]) :-
    disconnected(T),
    !.
split_at([T|Rest], [T|Start], End) :-
    split_at(Rest,Start,End).

split_disconnected([], [], []).
split_disconnected([Head], [Head], []) :-
    !.
split_disconnected([Head|Rest], [Head|Start], End) :-
    split_at(Rest,Start,End).

disconnected_partitions([], []).
disconnected_partitions([H|Conjunctions], [Start|Partitions]) :-
    split_disconnected([H|Conjunctions], Start, Rest),
    disconnected_partitions(Rest, Partitions).

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
        t(v(b),h,v(c)),
        t(v(c),f,v(a))
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
        t(v(b),h,v(c)),
        t(v(c),f,v(a)),
        (   t(c,f,v(a)),
            t(v(e),f,v(a))
        ;   t(d,f,v(a)),
            t(v(e),f,v(a))
		)
    ).

test(select, []) :-

    Term = select(
               [v(a), v(b)],
               (   t(v(a),f,g),
                   t(a,b,v(b))
               )
           ),

    partition(Term,Reads_Unordered,_Writes),
    optimize_read_order(Reads_Unordered, Reads),
    xfy_list(',', Prog, Reads),
    Prog = select(
               [v(a), v(b)],
               (   t(a,b,v(b)),
                   t(v(a),f,g)
               )
           ).

test(limit, []) :-

    Term = limit(
               3,
               (   t(v(a),f,g),
                   t(a,b,v(b))
               )
           ),

    partition(Term,Reads_Unordered,_Writes),
    optimize_read_order(Reads_Unordered, Reads),
    xfy_list(',', Prog, Reads),
    Prog = limit(
               3,
               (   t(a,b,v(b)),
                   t(v(a),f,g)
               )
           ).

test(internal_cuts, []) :-

    Term = (
        t(v(a),p,v(d)),
        t(x,p,v(a)),
        once(t(v(b),g,v(a))),
        t(v(b),q,v(d)),
        limit(
            10,
            t(v(b),r,v(e))),
        t(v(f), s, v(e)),
        t(v(e), s, t)
    ),

    partition(Term,Reads_Unordered,_Writes),
    optimize_read_order(Reads_Unordered, Reads),
    xfy_list(',', Prog, Reads),
    Prog = (
        t(x,p,v(a)),
        t(v(a),p,v(d)),
        once(t(v(b),g,v(a))),
        t(v(b),q,v(d)),
        limit(10,t(v(b),r,v(e))),
        t(v(e),s,t),
        t(v(f),s,v(e))
    ).

test(order_chains) :-
    Term = (t(v(x1), 'P1082', v(x2)),
            t(v(x1), 'P17', v(x3)),
            t(v(x3), 'P31', 'Q6256'),
            t(v(x3), 'P463', v(x4))),
    partition(Term,Reads_Unordered,_Writes),
    optimize_read_order(Reads_Unordered, Reads),
    xfy_list(',', Prog, Reads),

    Prog = (
        t(v(x3),'P31','Q6256'),
        t(v(x3),'P463',v(x4)),
        t(v(x1),'P17',v(x3)),
        t(v(x1),'P1082',v(x2))
    ).

test(order_dicts) :-
    order_conjuncts([_{a:1, b:v('X')}=_{a:v('Y'), b:2}],
                    [_{a:1, b:v('X')}=_{a:v('Y'), b:2}]).


test(order_type) :-
    Reads = [
        t(v(x), rdf:type, '@schema':'Branch'),
        t(v(x), name, "main"^^xsd:string)
    ],
    optimize_read_order(Reads, Ordered),
    Ordered = [
        t(v(x),name,"main"^^xsd:string),
		t(v(x),rdf:type,'@schema':'Branch')
	].

test(disconnected_partitions) :-
    disconnected_partitions(
        [t(v(x), y, z), t(mv(x), w, y), t(v(y), z, w)],
        [[t(v(x), y, z), t(mv(x), w, y)],
         [t(v(y), z, w)]]
    ).

:- end_tests(reorder_query).
