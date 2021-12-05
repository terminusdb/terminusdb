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

diff(T1,T2, Diff) :-
    diff(T1,T2,Diff,_).

best_cost(State,Cost) :-
    arg(1, State, Cost).

best_diff(State,Diff) :-
    arg(2, State, Diff).

diff(T1,T2,Diff,Cost) :-
    State = best(inf,keep),
    (   diff_(T1,T2,New_Diff,0,New_Cost,State),
        nb_setarg(1,State,New_Cost),
        nb_setarg(2,State,New_Diff),
        fail
    ;   best_cost(State, Cost),
        best_diff(State, Diff)
    ).

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
cost(keep, 1).

down_from(From,To,X) :-
    between(To,From,Y),
    X is From - Y.

diff_(M,M,keep,
      Current_Cost,Cost,State) :-
    !,
    best_cost(State, Best_Cost),
    Cost is Current_Cost + 1,
    Cost < Best_Cost.
diff_([],M2,swap(0,0,R2,C2,[],M2,keep,keep,keep),
      Current_Cost,Cost,State) :-
    !,
    row_length(M2,R2),
    column_length(M2,C2),
    best_cost(State, Best_Cost),
    Cost is Current_Cost + R2 * C2 + 4, % 1 for each op
    Cost < Best_Cost.
diff_(M1,[],swap(R1,C1,0,0,M1,[],keep,keep,keep),
      Current_Cost,Cost,State) :-
    !,
    row_length(M1,R1),
    column_length(M1,C1),
    best_cost(State, Best_Cost),
    Cost is Current_Cost + R1 * C1 + 4, % 1 for each op
    Cost < Best_Cost.
diff_(T1,T2,Diff,
      Current_Cost,Cost,State) :-
    row_length(T1,R1),
    column_length(T1,C1),
    row_length(T2,R2),
    column_length(T2,C2),
    !,
    down_from(R1,0,I1),
    down_from(R2,0,I2),
    down_from(C1,0,J1),
    down_from(C2,0,J2),
    % *Some* matrix gets smaller
    once((   Size1 is I1 * J1,
             Size1 >= 1
         ;   Size2 is I2 * J2,
             Size2 >= 1)),
    % the same section has advanced in both row and column
    split_matrix(T1, I1, J1, TL1, TR1, BL1, BR1),
    split_matrix(T2, I2, J2, TL2, TR2, BL2, BR2),
    best_cost(State, Best_Cost),
    (   TL1 = TL2
    ->  New_Cost is Current_Cost + 1,
        New_Cost < Best_Cost,
        diff_(TR1,TR2,Diff_TR,New_Cost,Cost1,State),
        diff_(BL1,BL2,Diff_BL,Cost1,Cost2,State),
        diff_(BR1,BR2,Diff_BR,Cost2,Cost,State),
        Diff = copy(I1,J1,Diff_TR,Diff_BL,Diff_BR)
    ;   New_Cost is Current_Cost + I1 * J1 + I2 * J2 + 1,
        New_Cost < Best_Cost,
        diff_(TR1,TR2,Diff_TR,New_Cost,Cost1,State),
        diff_(BL1,BL2,Diff_BL,Cost1,Cost2,State),
        diff_(BR1,BR2,Diff_BR,Cost2,Cost,State),
        Diff = swap(I1, J1, I2, J2,
                    TL1,TL2,Diff_TR,Diff_BL,Diff_BR)
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

split_row(In, N, Top, Bottom) :-
    split(N, In, Top, Bottom).

split_column([], _, [], []) :-
    !.
split_column(L, 0, [], L) :-
    !.
split_column([R|Rows], N, [R|Rows], []) :-
    length(R,N),
    !.
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

% Performs a "hash convolution" of the original matrix
% by running a window across and hashing the window to produce
% a new smaller matrix.
hash_blocks(N,M,T1,T2) :-
    row_length(T1,R),
    column_length(T1,C),

    findall(Columns,
            (   between(0,R,I),
                I =< R,
                In is I + N,
                In =< R,
                findall(Print,
                        (   between(0,C,J),
                            J =< C,
                            Jn is J + M,
                            Jn =< C,
                            split_matrix(T1,In,Jn,TL,_,_,_),
                            split_matrix(TL,I,J,_,_,_,Finger),
                            format(string(S), '~q', [Finger]),
                            crypto_data_hash(S, Print, [algorithm(md5)])
                        ),
                        Columns),
                \+ Columns = []
            ),
            T2).

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
                [],
                copy(2,2,
                     keep,
                     keep,
                     keep),
                keep,
                keep),

    patch(Diff,T1,T2).

test(patch_row_bottom, []) :-
    T1 = [[1,2],
          [3,4],
          [o,o]],

    T2 = [[1,2],
          [3,4]],

    Diff = copy(2,2,
                keep,
                swap(1,2,
                     0,0,
                     [[o,o]],
                     [],
                     keep,
                     keep,
                     keep),
                keep),

    patch(Diff,T1,T2).

test(diff_delete_col, []) :-
    T1 = [[o,1,2],
          [o,3,4]],

    T2 = [[1,2],
          [3,4]],

    diff(T1,T2,Diff),

    Diff = swap(2,1,2,0,[[o],
                         [o]],[],keep,keep,keep),
    patch(Diff,T1,T2).

test(diff_delete_row, []) :-
    T1 = [ [ 1, 2, 3 ] ],

    T2 = [ ],

    diff(T1,T2,Diff),
    Diff = swap(1, 3, 0, 0,
                [[1, 2, 3]],
                [], keep, keep, keep),

    patch(Diff, T1, T2).

test(diff_delete_row2, []) :-
    T1 = [ [ 1, 2, 3 ],
           [ 4, 5, 6 ] ],

    T2 = [ [ 1, 2, 3 ] ],

    diff(T1,T2,Diff),

    Diff = copy(1,3,
                keep,
                swap(1,3,0,0,[[4,5,6]],[],
                     keep,
                     keep,
                     keep),
                keep),

    patch(Diff,T1,T2).

test(diff_insert_row, []) :-
    T1 = [ ],
    T2 = [ [ 1, 2, 3 ] ],

    diff(T1,T2,Diff),
    Diff = swap(0,0,1,3,[],[[1,2,3]],keep,keep,keep),
    patch(Diff,T1,T2).

test(diff_insert_row2, []) :-
    T1 = [ [ 1, 2, 3 ] ],
    T2 = [ [ 1, 2, 3 ],
           [ 4, 5, 6 ] ],

    diff(T1,T2,Diff),
    Diff = copy(1,3,
                keep,
                swap(0,0,1,3,[],[[4,5,6]],keep,keep,keep),
                keep),

    patch(Diff,T1,T2).

test(diff_insert_col_middle, [
         %blocked(does_not_terminate)
     ]) :-
    T1 = [ [ 1, 3 ] ],
    T2 = [ [ 1, 2, 3 ] ],

    diff(T1,T2,Diff),
    Diff = swap(1,1,1,2,[[1]],[[1,2]],keep,keep,keep),
    patch(Diff,T1,T2).

test(diff_insert_simple_row_middle, [
         %blocked(does_not_terminate)
     ]) :-
    T1 = [ [ 1 ], [ 3 ] ],
    T2 = [ [ 1 ], [ 2 ], [ 3 ] ],

    diff(T1,T2,Diff,Cost),
    Diff = swap(1,1,2,1,[[1]],[[1],[2]],keep,keep,keep),
    cost(Diff,Cost),
    patch(Diff,T1,T2).

test(diff_insert_row_middle, [
         %blocked(does_not_terminate)
     ]) :-
    T1 = [ [ 1, 2, 3 ],
           [ 7, 8, 9 ] ],
    T2 = [ [ 1, 2, 3 ],
           [ 4, 5, 6 ],
           [ 7, 8, 9 ] ],

    diff(T1,T2,Diff,Cost),
    Diff = copy(1,3,keep,swap(0,3,1,3,
                              [],
                              [[4,5,6]],
                              keep,keep,keep),keep),
    cost(Diff,Cost),
    patch(Diff,T1,T2).

test(diff_shared_corner, []) :-
    M1 = [[a,b],
          [c,1]],
    M2 = [[1]],
    diff(M1,M2, Diff, Cost),
    Diff = swap(2,2,1,1,[[a,b],[c,1]],[[1]],keep,keep,keep),
    cost(Diff,Cost),
    patch(Diff, M1, M2).

test(diff_shared_middle_row, []) :-
    M1 = [[a,b,d,3],
          [x,1,2,x],
          [o,o,o,o]],
    M2 = [[1,2]],
    diff(M1,M2, Diff, Cost),
    % Not ideal
    Diff = swap(3,4,1,2,[[a,b,d,3],[x,1,2,x],[o,o,o,o]],[[1,2]],
                keep,keep,keep),
    cost(Diff,Cost),
    patch(Diff, M1, M2).

test(diff_shared_middle, [
         blocked('heat death of the universe anyone?')
     ]) :-
    T1 = [ [ o, o, o, o ],
           [ a, 1, 2, 3 ],
           [ b, 4, 5, 6 ],
           [ c, 7, 8, 9 ],
           [ d, x, x, x ] ],

    T2 = [ [ 1, 2, 3, "goo" ],
           [ 4, 5, 6, "gar" ],
           [ 7, 8, 9, "gaz" ] ],

    diff(T1,T2,Diff,Cost),
    writeq(Diff),
    cost(Diff, Cost).

test(diff_row_col_simple, []) :-
    T1 = [ [ 1, x ] ],

    T2 = [ [ "gaz" ],
           [ "gar" ] ],
    diff(T1,T2,Diff,Cost),
    Diff = swap(1,2,2,1,[[1,x]],[["gaz"],["gar"]],keep,keep,keep),
    cost(Diff, Cost),
    patch(Diff, T1, T2).

test(diff_row_col, []) :-
    T1 = [ [ 1, 2, 3 ],
           [ 4, 5, 6 ],
           [ 7, 8, 9 ],
           [ x, x, x ] ],

    T2 = [ [ 1, 2, 3, "goo" ],
           [ 4, 5, 6, "gar" ],
           [ 7, 8, 9, "gaz" ] ],

    diff(T1,T2,Diff,Cost),
    Diff = copy(3,3,
                swap(0,0,3,1,[],[["goo"],["gar"],["gaz"]],
                     keep,keep,keep),
                swap(1,3,0,0,[[x,x,x]],[],keep,keep,keep),
                keep),
    cost(Diff, Cost),
    patch(Diff, T1, T2).

test(hash_shared_middle, [
     ]) :-
    T1 = [ [ o, o, o, o ],
           [ a, 1, 2, 3 ],
           [ b, 4, 5, 6 ],
           [ c, 7, 8, 9 ],
           [ d, x, x, x ] ],
    hash_blocks(3,3,T1,Blocks1),

    T2 = [ [ 1, 2, 3, "goo" ],
           [ 4, 5, 6, "gar" ],
           [ 7, 8, 9, "gaz" ] ],

    hash_blocks(3,3,T2,Blocks2),

    writeq(Blocks1),
    writeq(Blocks2),

    %diff(T1,T2,Diff,Cost),
    %writeq(Diff),
    %cost(Diff, Cost),
    true.

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

