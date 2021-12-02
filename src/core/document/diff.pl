:- module('document/diff',
          [patch_list/3,
           diff_list/3]).

:- use_module(core(util)).

% example for lists:
%
% diff := end_list
%       | cpy_list(Item,Diff)
%       | del_list(Item,Diff)
%       | ins_list(Item,Diff)
%

patch_list(ins_list(Item,Diff)) -->
    patch_list(Diff),
    insert_list(Item).
patch_list(del_list(Item,Diff)) -->
    delete_list(Item),
    patch_list(Diff).
patch_list(cpy_list(Item,Diff)) -->
    delete_list(Item),
    patch_list(Diff),
    insert_list(Item).
patch_list(end_list) --> [].

insert_list(Item, Xs, [Item|Xs]).

delete_list(Item, [Item|Xs], Xs).

cost_list(ins_list(_,Diff), N) :-
    cost_list(Diff, M),
    N is 1 + M.
cost_list(del_list(_,Diff), N) :-
    cost_list(Diff, M),
    N is 1 + M.
cost_list(cpy_list(_,Diff), N) :-
    cost_list(Diff, M),
    N is 1 + M.
cost_list(end_list, 0).

diff_list([], [], end_list).
diff_list([], [Y|Ys], ins_list(Y,Zs)) :-
    diff_list([], Ys, Zs).
diff_list([X|Xs], [], del_list(X,Zs)) :-
    diff_list(Xs, [], Zs).
diff_list([X|Xs], [Y|Ys], Best) :-
    diff_list(Xs, [Y|Ys], Diff1),
    diff_list([X|Xs], Ys, Diff2),
    (   X = Y
    ->  diff_list(Xs, Ys, Diff3),
        min_list(del_list(X,Diff1), ins_list(X,Diff2), DiffI),
        min_list(DiffI, cpy_list(X,Diff3), Best)
    ;   min_list(del_list(X,Diff1), ins_list(X,Diff2), Best)
    ).

min_list(Diff1, Diff2, Diff) :-
    cost_list(Diff1, Cost1),
    cost_list(Diff2, Cost2),
    (   Cost1 =< Cost2
    ->  Diff = Diff1
    ;   Diff = Diff2).


% example for trees:
%
% diff := end_tree
%       | cpy_tree(Label,Index,Diff)
%       | del_tree(Lable,Index,Item,Diff)
%       | ins_list(Item,Diff)
%
