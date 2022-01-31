:- module('util/tables',
          [
              columns/2,
              rows/2,
              split_list/4,
              split_table/7,
              split_column/4,
              table_window/6,
              replace_table_window/5,
              is_table/1
          ]).

:- use_module(library(lists)).

is_table([[]]).
is_table([[_|_]|_]).

%%% Table utils
rows(T, Length) :-
    length(T, Length).

columns([], 0).
columns([R|_], Length) :-
    length(R, Length).

split_list(Index,List,Left,Right) :-
    length(Left, Index),
    append(Left,Right,List).

split_table(In, X, Y, Top_Left, Right, Bottom, Bottom_Right) :-
    when((   nonvar(In)
         ;   nonvar(Rows_Top),
             nonvar(Rows_Bottom)),
         split_row(In, Y, Rows_Top, Rows_Bottom)),
    split_column(Rows_Top, X, Top_Left, Right),
    split_column(Rows_Bottom, X, Bottom, Bottom_Right).

split_row(In, N, Top, Bottom) :-
    split_list(N, In, Top, Bottom).

split_column([], _, [], []) :-
    !.
split_column(L, 0, [], L) :-
    !.
split_column([R|Rows], N, [R|Rows], []) :-
    length(R,N),
    !.
split_column([R|Rows], N, [Left|Top], [Right|Bottom]) :-
    split_list(N, R, Left, Right),
    split_column(Rows, N, Top, Bottom).


row_slice(_,0,_,[]) :- !.
row_slice(0,W,[Elt|L1],[Elt|L2]) :-
    !,
    Wp is W - 1,
    row_slice(0,Wp,L1,L2).
row_slice(X,W,[_|L1],L2) :-
    Xp is X - 1,
    row_slice(Xp,W,L1,L2).

table_window(_,_,_,0,_,[]) :- !.
table_window(X,W,0,H,[R1|M1],[R2|M2]) :-
    !,
    row_slice(X,W,R1,R2),
    Hp is H - 1,
    table_window(X,W,0,Hp,M1,M2).
table_window(X,W,Y,H,[_|M1],M2) :-
    Yp is Y - 1,
    table_window(X,W,Yp,H,M1,M2).

replace_table_window(X,Y,Window,Table,New_Table) :-
    columns(Window,C),
    rows(Window,R),
    split_table(Table, X, Y, Top_Left, Right, Bottom, Bottom_Right),
    split_table(Bottom_Right, C, R, _, BR_Right, BR_Bottom, BR_Bottom_Right),
    split_table(New_Bottom_Right, C, R, Window, BR_Right, BR_Bottom, BR_Bottom_Right),
    split_table(New_Table, X, Y, Top_Left, Right, Bottom, New_Bottom_Right).


:- begin_tests(tables).

test(split_table, []) :-
    M1 = [[a, b, c],
          [d, e, f],
          [g, h, i]],
    split_table(M1,1,1,TL,TR,BL,BR),
    TL = [[a]],
    TR = [[b, c]],
    BL = [[d], [g]],
    BR = [[e, f],
          [h, i]].

test(split_table_2, []) :-
    M1 = [[a, b, c],
          [d, e, f],
          [g, h, i]],
    split_table(M1,2,1,TL,TR,BL,BR),
    TL = [[a, b]],
    TR = [[c]],
    BL = [[d, e], [g, h]],
    BR = [[f],
          [i]].

test(replace_window, []) :-
    Window = [[3], [6]],
    M1 = [[null, null, null],
          [null, null, null],
          [null, null, null]],
    replace_table_window(2, 1, Window, M1, M2),
    M2 = [[null,null,null],
          [null,null,3],
          [null,null,6]].

:- end_tests(tables).
