
:- module('query/partition',[
              partition/3,
              safe_guard_removal/2
          ]).

:- use_module(core(util)).
:- use_module(core(query/reorder), [optimize_read_order/2]).

/* This partitions a tree cleanly into two segments or fails:
   Reads:  read only
   Writes: write only
*/
partition((A,B), Reads, Writes) :-
    partition(A, A_Reads, []),
    !,
    partition(B, B_Reads, Writes),
    append(A_Reads, B_Reads, Reads).
partition((A,B), Reads, Writes) :-
    partition(B, [], B_Writes),
    !,
    partition(A, Reads, A_Writes),
    append(A_Writes, B_Writes, Writes).
partition((A;B), [(A;B)], []) :-
    /* just fail if we are doing disjunctive writes */
    !,
    partition(A, _, []),
    partition(B, _, []).
partition(not(Q), [not(Q)], []) :-
    /* just fail if we have a write in a not. */
    !,
    partition(Q, _, []).
partition(once(Q), [once(Q)], []) :-
    !,
    partition(Q, _, []).
partition(once(Q), [], [once(Q)]) :-
    partition(Q, [], _),
    !.
partition(limit(N,Q), [limit(N,Q)], []) :-
    /* just fail if we have a limit on a write */
    !,
    partition(Q, _, []).
partition(select(V,Q), [select(V,Q)], []) :-
    /* just fail if we have a select on a write */
    !,
    partition(Q, _, []).
partition(opt(P), [opt(P)], []) :-
    /* just fail if we have an opt on a write */
    !,
    partition(P, _, []).
partition(when(A,B), Reads, Writes) :-
    /* assume "when"s have a read only head */
    !,
    partition(A, A_Reads, []),
    partition(B, B_Reads, Writes),
    append(A_Reads, B_Reads, Reads).
partition(using(C,P), Reads, Writes) :-
    !,
    partition(P, P_Reads, P_Writes),
    (   P_Reads = []
    ->  Reads = [],
        xfy_list(',', Q, P_Writes),
        Writes = [using(C,Q)]
    ;   P_Writes = []
    ->  Writes = [],
        xfy_list(',', Q, P_Reads),
        Reads = [using(C,Q)]
    ->  xfy_list(',', A, P_Reads),
        xfy_list(',', B, P_Writes),
        Reads = [using(C,A)],
        Writes = [using(C,B)]
    ).
partition(from(C,P), Reads, Writes) :-
    partition(P, P_Reads, P_Writes),
    !,
    (   P_Reads = []
    ->  Reads = [],
        xfy_list(',', Q, P_Writes),
        Writes = [from(C,Q)]
    ;   P_Writes = []
    ->  Writes = [],
        xfy_list(',', Q, P_Reads),
        Reads = [from(C,Q)]
    ->  xfy_list(',', A, P_Reads),
        xfy_list(',', B, P_Writes),
        Reads = [from(C,A)],
        Writes = [from(C,B)]
    ).
partition(start(N,P), [start(N,P)], []) :-
    partition(P, _, []),
    !.
partition(count(P,N), [count(P,N)], []) :-
    partition(P, _, []),
    !.
partition(where(P), Reads, Writes) :-
    % where means nothing
    partition(P, Reads, Writes),
    !.
partition(order_by(L,S), [order_by(L,S)], []) :-
    partition(S, _, []),
    !.
partition(into(C,P), Reads, Writes) :-
    partition(P, P_Reads, P_Writes),
    !,
    (   P_Reads = []
    ->  Reads = [],
        xfy_list(',', Q, P_Writes),
        Writes = [into(C,Q)]
    ;   P_Writes = []
    ->  Writes = [],
        xfy_list(',', Q, P_Reads),
        Reads = [into(C,Q)]
    ->  xfy_list(',', A, P_Reads),
        xfy_list(',', B, P_Writes),
        Reads = [into(C,A)],
        Writes = [into(C,B)]
    ).
partition(group_by(G,T,Q,A), [group_by(G,T,Q,A)], []) :-
    partition(Q, _, []),
    !.
partition(insert(A,B,C), Reads, Writes) :-
    !,
    Reads = [],
    Writes = [insert(A,B,C)].
partition(insert(A,B,C,D), Reads, Writes) :-
    !,
    Reads = [],
    Writes = [insert(A,B,C,D)].
partition(delete(A,B,C), Reads, Writes) :-
    !,
    Reads = [],
    Writes = [delete(A,B,C)].
partition(delete(A,B,C,D), Reads, Writes) :-
    !,
    Reads = [],
    Writes = [delete(A,B,C,D)].
partition(replace_document(A,B), Reads, Writes) :-
    !,
    Reads = [],
    Writes = [replace_document(A,B)].
partition(replace_document(A,B,C), Reads, Writes) :-
    !,
    Reads = [],
    Writes = [replace_document(A,B,C)].
partition(delete_document(A), Reads, Writes) :-
    !,
    Reads = [],
    Writes = [delete_document(A)].
partition(T,[T],[]) :-
    /* Everything else should be read only
     * Note: A bit more energy here would remove the default case and need for cuts.
     */
    !.


/*
 * safe_guard_removal(Term, NewTerm) is det.
 */
safe_guard_removal(Term, Prog) :-
    partition(Term,Reads_Unordered,Writes),
    optimize_read_order(Reads_Unordered, Reads),
    (   Writes = []
    ->  xfy_list(',', Prog, Reads)
    ;   Reads = []
    ->  xfy_list(',', Write_Term, Writes),
        Prog = immediately(Write_Term)
    ;   xfy_list(',', A, Reads),
        xfy_list(',', B, Writes),
        Prog = (A,immediately(B))
    ),
    !.
safe_guard_removal(Term, Term).

:- begin_tests(guards).

test(guard_removal_is_impossible, []) :-

    AST = (
        t(a,b,c),
        insert(a,b,c)
    ;   t(e,f,g),
        insert(d,b,c)),

    safe_guard_removal(AST, AST).

test(guard_removal_is_safe, []) :-

    AST = (
        t(a,b,c),
        t(e,f,g),
        insert(a,b,c),
        insert(d,b,c),
        insert(e,f,g)
    ),

    safe_guard_removal(AST, AST2),

    AST2 = ((
                   t(a,b,c),
                   t(e,f,g)),
            immediately(
                (
                    insert(a,b,c),
                    insert(d,b,c),
                    insert(e,f,g)))).

test(alternating_inserts, []) :-

    AST = (
        t(a,b,c),
        insert(a,b,c),
        t(e,f,g),
        insert(d,b,c),
        insert(e,f,g)
    ),

    safe_guard_removal(AST, AST).

test(guard_removal_with_deep_inserts, []) :-

    AST = (
        t(a,b,c),
        (   t(e,f,g),
            (   insert(a,b,c),
                insert(d,b,c),
                (   insert(e,f,g),
                    insert(f,g,h))))),

    safe_guard_removal(AST, AST2),

    AST2 = ((t(a,b,c),
             t(e,f,g)),
            immediately(
                (insert(a,b,c),
                 insert(d,b,c),
                 insert(e,f,g),
                 insert(f,g,h)))).

test(guard_single_query, []) :-

    AST = t(a,b,c),

    safe_guard_removal(AST, (t(a,b,c))).


test(guard_single_insertion, []) :-

    AST = insert(a,b,c),

    safe_guard_removal(AST, immediately(insert(a,b,c))).

test(guard_single_deletion, []) :-

    AST = delete(a,b,c),

    safe_guard_removal(AST, immediately(delete(a,b,c))).

test(guard_double_insertion, []) :-

    AST = (insert(a,b,c),insert(d,e,f)),

    safe_guard_removal(AST, (immediately((insert(a,b,c),insert(d,e,f))))).

:- end_tests(guards).
