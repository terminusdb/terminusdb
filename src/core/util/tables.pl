:- module('util/tables',
          [
              row_length/2,
              column_length/2,
              split_list/4,
              split_table/7,
              split_column/4,
              table_window/6,
              replace_table_window/5
          ]).

%%% Table utils
row_length(T, Length) :-
    length(T, Length).

column_length([], 0).
column_length([R|_], Length) :-
    length(R, Length).

split_list(Index,List,Left,Right) :-
    length(Left, Index),
    append(Left,Right,List).

split_table(In, N, M, Top_Left, Right, Bottom, Bottom_Right) :-
    when((   nonvar(In)
         ;   nonvar(Rows_Top),
             nonvar(Rows_Bottom)),
         split_row(In, N, Rows_Top, Rows_Bottom)),
    split_column(Rows_Top, M, Top_Left, Right),
    split_column(Rows_Bottom, M, Bottom, Bottom_Right).

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
    row_length(Window,R),
    column_length(Window,C),
    split_table(Table, X, Y, Top_Left, Right, Bottom, Bottom_Right),
    split_table(Bottom_Right, R, C, _, BR_Right, BR_Bottom, BR_Bottom_Right),
    split_table(New_Bottom_Right, R, C, Window, BR_Right, BR_Bottom, BR_Bottom_Right),
    split_table(New_Table, X, Y, Top_Left, Right, Bottom, New_Bottom_Right).
