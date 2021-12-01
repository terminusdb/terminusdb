:- module('document/diff',
          [patch/3,
           diff/3]).

:- use_module(core(util)).

% example for lists:
%
% diff := end
%       | cpy(Item,Diff)
%       | del(Item,Diff)
%       | ins(Item,Diff)
%
% Takes a pair of list of lists and obtains a diff.

patch(ins(Item,Diff)) -->
    patch(Diff),
    insert(Item).
patch(del(Item,Diff)) -->
    delete(Item),
    patch(Diff).
patch(cpy(Item,Diff)) -->
    delete(Item),
    patch(Diff),
    insert(Item).
patch(end) --> [].

insert(Item, Xs, [Item|Xs]).

delete(Item, [Item|Xs], Xs).

cost(ins(_,Diff), N) :-
    cost(Diff, M),
    N is 1 + M.
cost(del(_,Diff), N) :-
    cost(Diff, M),
    N is 1 + M.
cost(cpy(_,Diff), N) :-
    cost(Diff, M),
    N is 1 + M.
cost(end, 0).

diff([], [], end).
diff([], [Y|Ys], ins(Y,Zs)) :-
    diff([], Ys, Zs).
diff([X|Xs], [], del(X,Zs)) :-
    diff(Xs, [], Zs).
diff([X|Xs], [Y|Ys], Best) :-
    diff(Xs, [Y|Ys], Diff1),
    diff([X|Xs], Ys, Diff2),
    (   X = Y
    ->  diff(Xs, Ys, Diff3),
        min(del(X,Diff1), ins(X,Diff2), DiffI),
        min(DiffI, cpy(X,Diff3), Best)
    ;   min(del(X,Diff1), ins(X,Diff2), Best)
    ).

min(Diff1, Diff2, Diff) :-
    cost(Diff1, Cost1),
    cost(Diff2, Cost2),
    (   Cost1 =< Cost2
    ->  Diff = Diff1
    ;   Diff = Diff2).


