:- module('document/diff',
          [patch/3]).

:- use_module(core(util)).

%
% diff := copy(N, M, Item, Diff_TR, Diff_BL, Diff_BR)
%       | swap(N, M, Old, New, Diff_TR, Diff_BL, Diff_BR)
%       | keep
%
% TR = Top Right
% BL = Bottom Left
% BR = Bottom Right

patch(copy(N,M,Diff_TR,Diff_BL,Diff_BR), In, Out) :-
    split_matrix(In, N, M, TL, TR, BL, BR),
    patch(Diff_TR, TR, TR_New),
    patch(Diff_BL, BL, BL_New),
    patch(Diff_BR, BR, BR_New),
    split_matrix(Out, N, M, TL, TR_New, BL_New, BR_New).
patch(swap(N1,M1,N2,M2,Old,New,Diff_TR,Diff_BL,Diff_BR), In, Out) :-
    split_matrix(In, N1, M1, Old, TR, BL, BR),
    patch(Diff_TR, TR, TR_New),
    patch(Diff_BL, BL, BL_New),
    patch(Diff_BR, BR, BR_New),
    split_matrix(Out, N2, M2, New, TR_New, BL_New, BR_New).
patch(keep, T, T).

cost(copy(_,_,TR,BL,BR),Cost) :-
    cost(TR,TR_Cost),
    cost(BL,BL_Cost),
    cost(BR,BR_Cost),
    Cost is TR_Cost + BL_Cost + BR_Cost + 1.
cost(swap(N1,M1,N2,M2,_,_,TR,BL,BR),Cost) :-
    cost(TR,TR_Cost),
    cost(BL,BL_Cost),
    cost(BR,BR_Cost),
    Cost is N1 * M1 + N2 * M2 + TR_Cost + BL_Cost + BR_Cost + 1.
cost(keep,1).

diff(T1,T2, Diff) :-
    sfoldr(
        infimum,
        diff_(T1,T2),
        infinity,
        Diff).

infimum(infinity, Diff, Diff).
infimum(Diff, infinity, Diff).
infimum(DiffX, DiffY, Diff) :-
    cost(DiffX, CostX),
    cost(DiffY, CostY),
    (   CostX =< CostY
    ->  Diff = DiffX
    ;   Diff = DiffY).

diff_(T,T,keep).
diff_(T1,T2,Diff) :-
    row_length(T1,R1),
    column_length(T1,C1),
    row_length(T2,R2),
    column_length(T2,C2),
    !,
    between(0,R1,N1),
    between(0,R2,N2),
    between(0,C1,M1),
    between(0,C2,M2),
    once((   member(X, [N1,N2]),
             X \= 0
         ;   member(Y, [M1,M2]),
             Y \= 0)),
    split_matrix(T1, N1, M1, TL1, TR1, BL1, BR1),
    split_matrix(T2, N2, M2, TL2, TR2, BL2, BR2),
    (   TL1 = TL2
    ->  diff_(TR1,TR2,Diff_TR),
        diff_(BL1,BL2,Diff_BL),
        diff_(BR1,BR2,Diff_BR),
        Diff = copy(N1,M1,Diff_TR,Diff_BL,Diff_BR)
    ;   diff_(TR1,TR2,Diff_TR),
        diff_(BL1,BL2,Diff_BL),
        diff_(BR1,BR2,Diff_BR),
        Diff = swap(N1,M1,N2,M2,TL1,TL2,Diff_TR,Diff_BL,Diff_BR)
    ).

row_length(T, Length) :-
    length(T, Length).

column_length([], 0).
column_length([R|_], Length) :-
    length(R, Length).

max_row_length(T, Length) :-
    % For ragged matrix calculations
    foldl(
        [L,Max,New_Max]>>(
            length(L,Len),
            New_Max is max(Len,Max)
        ),
        T,0,Length).

split(Index,List,Left,Right) :-
    length(Left, Index),
    append(Left,Right,List).

split_matrix(In, N, M, Top_Left, Right, Bottom, Bottom_Right) :-
    when((   ground(In)
         ;   ground(Rows_Top),
             ground(Rows_Bottom)),
         split_row(In, N, Rows_Top, Rows_Bottom)),
    split_column(Rows_Top, M, Top_Left, Right),
    split_column(Rows_Bottom, M, Bottom, Bottom_Right).

/*
split_row([[]], 0, [[]], [[]]) :- !.
split_row(T, 0, [[]], T) :- !.
split_row(T, N, T, [[]]) :-
    length(T,N),
    !.
*/
split_row(In, N, Top, Bottom) :-
    split(N, In, Top, Bottom).

split_column([], _, [], []).
split_column([R|Rows], N, [Left|Top], [Right|Bottom]) :-
    split(N, R, Left, Right),
    split_column(Rows, N, Top, Bottom).

symmetrize(T1,T2,T3,T4) :-
    max_row_length(T1,Max_Row1),
    max_row_length(T2,Max_Row2),
    Max_Row is max(Max_Row1,Max_Row2),
    column_length(T1,Col1),
    column_length(T2,Col2),
    Max_Col is max(Col1,Col2),
    matrix_fill(T1,Max_Row,Max_Col,T3),
    matrix_fill(T2,Max_Row,Max_Col,T4).

matrix_fill([], Max_Row, Max_Col, T) :-
    repeat_term(null,Max_Row,Row),
    repeat_term(Row,Max_Col,T).
matrix_fill([R|Rows], Max_Row, Max_Col, [NR|NRows]) :-
    length(R,Row_Length),
    Row_Delta is Max_Row - Row_Length,
    repeat_term(null,Row_Delta,Tail),
    append(R,Tail,NR),
    succ(Sub_Col, Max_Col),
    matrix_fill(Rows, Max_Row, Sub_Col, NRows).

:- begin_tests(matrix).

test(split_row,[]) :-

    T1 = [ [ 1, 2, 3 ],
           [ 4, 5, 6 ],
           [ 7, 8, 9 ],
           [ x, x, x ] ],

    split_row(T1, 3, T2, T3),

    T2 = [ [ 1, 2, 3 ],
           [ 4, 5, 6 ],
           [ 7, 8, 9 ] ],
    T3 = [ [ x, x, x ] ].

test(split_column,[]) :-

    T1 = [ [ 1, 2, 3, a ],
           [ 4, 5, 6, b ],
           [ 7, 8, 9, c ] ],

    split_column(T1, 3, T2, T3),

    T2 = [ [ 1, 2, 3 ],
           [ 4, 5, 6 ],
           [ 7, 8, 9 ] ],
    T3 = [ [ a ],
           [ b ],
           [ c ] ].

test(split_empty_matrix,[]) :-
    split_matrix([[]],
                 0,
                 0,
                 TL,TR,
                 BL,BR),
    TL = [],
    TR = [],
    BL = [[]],
    BR = [[]].

test(split_matrix,[]) :-

    M = [ [ 1, 2, 3, a ],
          [ 4, 5, 6, b ],
          [ 7, 8, 9, c ],
          [ x, x, x, y ] ],

    split_matrix(M,3,3,TL,TR,BL,BR),

    TL = [[1,2,3],
          [4,5,6],
          [7,8,9]],
    TR = [[a],
          [b],
          [c]],
    BL = [[x,x,x]],
    BR = [[y]].

test(unsplit_matrix,[]) :-

    TL = [[1,2,3],
          [4,5,6],
          [7,8,9]],
    TR = [[a],
          [b],
          [c]],
    BL = [[x,x,x]],
    BR = [[y]],

    split_matrix(M,3,3,TL,TR,BL,BR),

    M = [ [ 1, 2, 3, a],
          [ 4, 5, 6, b],
          [ 7, 8, 9, c],
          [ x, x, x, y] ].

test(patch_col, []) :-
    T1 = [[o,1,2],
          [o,3,4]],

    T2 = [[1,2],
          [3,4]],

    Diff = swap(2,1,
                2,0,
                [[o],
                 [o]],
                [[],
                 []],
                copy(2,2,
                     keep,
                     keep,
                     keep),
                keep,
                keep),

    patch(Diff,T1,T2).

test(diff_col, []) :-
    T1 = [[o,1,2],
          [o,3,4]],

    T2 = [[1,2],
          [3,4]],

    diff(T1,T2,Diff),
    writeq(Diff),
    Diff = swap(2,1,2,0,
                [[o],[o]],
                [[],[]],keep,keep,keep),

    patch(Diff,T1,T2).

test(diff_empty_row, []) :-
    T1 = [ [ 1, 2, 3 ] ],

    T2 = [ ],

    diff_(T1,T2,Diff),
    writeq(Diff).

test(diff_shared_middle, []) :-
    T1 = [ [ o, o, o, o ],
           [ a, 1, 2, 3 ],
           [ b, 4, 5, 6 ],
           [ c, 7, 8, 9 ],
           [ d, x, x, x ] ],

    T2 = [ [ 1, 2, 3, "goo" ],
           [ 4, 5, 6, "gar" ],
           [ 7, 8, 9, "gaz" ] ],

    diff(T1,T2,Diff),
    writeq(Diff).


test(diff_row_col, []) :-
    T1 = [ [ 1, 2, 3 ],
           [ 4, 5, 6 ],
           [ 7, 8, 9 ],
           [ x, x, x ] ],

    T2 = [ [ 1, 2, 3, "goo" ],
           [ 4, 5, 6, "gar" ],
           [ 7, 8, 9, "gaz" ] ],

    diff(T1,T2,Diff),

    Diff = copy(3,3,
                swap(3,1,
                     [[null],[null],[null]],
                     [["goo"],["gar"],["gaz"]],
                     keep,
                     keep,
                     keep),
                swap(1,3,
                     [[x,x,x]],
                     [[null,null,null]],
                     keep,
                     keep,
                     keep),
                keep).

:- end_tests(matrix).

/*
T1 = [ [ 1, 2, 3 ],  |
       [ 4, 5, 6 ],  |
       [ 7, 8, 9 ],  |
       [ x, x, x ] ] | end
------------------------
      end            | end

T1 = [[]],| end
     ------------------
     end  |[ [ 1, 2, 3 ],
          |  [ 4, 5, 6 ],
          |  [ 7, 8, 9 ],
          |  [ x, x, x ] ]

T2 = [ [ 1, 2, 3, "goo" ],
       [ 4, 5, 6, "gar" ],
       [ 7, 8, 9, "gaz" ] ]



T3 = [ [ 1, 2, 3],
       [ 4, 5, 6],
       [ 7, 8, 9] ]


T4 = [ [ x, 2, 3],
       [ 4, 5, 6],
       [ 7, 8, 9] ]


*/

