:- module('query/algebra',
          [
              termlist_conjunction/2,
              termlist_disjunction/2,
              conjunct/3,
              unannotated/2,
              effective_operator/2
          ]).

:- use_module(library(sort)).

:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(lists)).

unannotated(X^^_, X) :-
    !.
unannotated(X, X).

effective_operator(<, @<).
effective_operator(>, @>).
effective_operator(>=, @>=).
effective_operator(=<, @=<).
effective_operator(=, =).
effective_operator(\=, \=).

conjunct(true, Term, Term) :-
    !.
conjunct(Term, true, Term) :-
    !.
conjunct(Term0,Term1,and(Term0,Term1)).

precedence(false,-2).
precedence(true,-1).
precedence(t,0).
precedence(isa,1).
precedence(and,2).
precedence(impl,3).
precedence(or,4).
precedence(op,5).
precedence(not,6).

term_priority(Op,Term1,Term2) :-
    Term1 =.. [F|_],
    Term2 =.. [G|_],
    (   F = G
    ->  compare(Op,Term1,Term2)
    ;   precedence(F,FP),
        precedence(G,GP),
        compare(Op, FP, GP)
    ).

conj_flatten([],[]).
conj_flatten([and(X,Y)|Rest],Flattened) :-
    !,
    conj_flatten([X,Y|Rest], Flattened).
conj_flatten([or(X,Y)|Rest],[or(F1,F2)|Flattened]) :-
    !,
    termlist_conjunction([X],F1),
    termlist_conjunction([Y],F2),
    conj_flatten(Rest, Flattened).
conj_flatten([X|Rest],[X|Flattened]) :-
    conj_flatten(Rest,Flattened).

conjunction_sort(Terms, Ordered) :-
    conj_flatten(Terms,Flattened),
    predsort(term_priority,Flattened,Ordered).

termlist_conjunction(Unsorted, Conj) :-
    conjunction_sort(Unsorted, Terms),
    (   Terms = []
    ->  Conj = false
    ;   Terms = [Conj]
    ->  true
    ;   Terms = [Term0|Rest],
        foldl([Term1, Term2, Term3]>>conjunct(Term2,Term1,Term3), Rest, Term0, Conj)
    ).

disjunction_sort(Terms, Ordered) :-
    maplist([Term,F]>>termlist_conjunction([Term],F), Terms, Ordered).

termlist_disjunction(Unsorted, Disj) :-
    disjunction_sort(Unsorted, Terms),
    (   Terms = []
    ->  Disj = true
    ;   Terms = [Disj]
    ->  true
    ;   Terms = [Term0|Rest],
        foldl([Term1, Term2, or(Term2,Term1)]>>true, Rest, Term0, Disj)
    ).

:- begin_tests(algebra).

test(order_and_or_op,[]) :-
    Term = and(and(t(Customer, age, Age),
                   or(op(>, Age, 30),
                      op(<, Age, 10))),
               isa(Customer, 'Customer')),
    termlist_conjunction([Term], Result),
    Result = and(and(t(A,age,B),isa(A,'Customer')),or(op(>,B,30),op(<,B,10))).

test(order_inside_or,[]) :-
    Term = and(and(t(Customer, age, Age),
                   or(and(op(>, Age, 30),
                          t(Customer, age, Age)),
                      op(<, Age, 10))),
               isa(Customer, 'Customer')),
    termlist_conjunction([Term], Result),
    Result = and(and(t(A,age,B),isa(A,'Customer')),or(and(t(A,age,B),op(>,B,30)),op(<,B,10))).


test(toplevel_or,[]) :-
    Term = and(and(t(Customer, age, Age),
                   or(and(op(>, Age, 30),
                          t(Customer, age, Age)),
                      op(<, Age, 10))),
               isa(Customer, 'Customer')),
    termlist_disjunction([Term], Result),
    Result = and(and(t(A,age,B),isa(A,'Customer')),or(and(t(A,age,B),op(>,B,30)),op(<,B,10))).

:- end_tests(algebra).
