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
        format(user_error,'~nNew_Cost: ~q', [New_Cost]),
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
    X is From - Y + 1.

%:- table diff_/6.
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
    Size1 is I1 * J1,
    Size2 is I2 * J2,
    once((   Size1 >= 1
         ;   Size2 >= 1)),
    once((   Size1 < R1 * C1
         ;   Size2 < R2 * C2)),
    split_matrix(T1, I1, J1, TL1, TR1, BL1, BR1),
    split_matrix(T2, I2, J2, TL2, TR2, BL2, BR2),
    best_cost(State, Best_Cost),
    (   TL1 = TL2
    ->  New_Cost is Current_Cost + 1,
        % Add cost of minimum difference based on dimensions
        % 3 because that's absolutely minimal from the next 3 diffs.
        Size_TR1 is I1 * (C1  - J1),
        Size_TR2 is I2 * (C2 - J2),
        Cost1_LB is New_Cost + abs(Size_TR1 - Size_TR2) + 3,
        Cost1_LB < Best_Cost,
        diff_(TR1,TR2,Diff_TR,New_Cost,Cost1,State),
        % 2 to go
        Size_BL1 is (R1 - I1) * J1,
        Size_BL2 is (R2 - I2) * J2,
        Cost2_LB is Cost1 + abs(Size_BL1 - Size_BL2) + 2,
        Cost2_LB < Best_Cost,
        diff_(BL1,BL2,Diff_BL,Cost1,Cost2,State),
        % 1 left
        Size_BR1 is (R1 - I1) * (C1 - J1),
        Size_BR2 is (R2 - I2) * (C2 - J2),
        Cost_LB is Cost2 + abs(Size_BR1 - Size_BR2) + 1,
        Cost_LB < Best_Cost,
        diff_(BR1,BR2,Diff_BR,Cost2,Cost,State),
        % Best be less than best
        Cost < Best_Cost,
        Diff = copy(I1,J1,Diff_TR,Diff_BL,Diff_BR)
    ;   New_Cost is Current_Cost + I1 * J1 + I2 * J2 + 1,
        % 3 because that's absolutely minimal from the next 3 diffs.
        Size_TR1 is I1 * (C1  - J1),
        Size_TR2 is I2 * (C2 - J2),
        Cost1_LB is New_Cost + abs(Size_TR1 - Size_TR2) + 3,
        Cost1_LB < Best_Cost,
        diff_(TR1,TR2,Diff_TR,New_Cost,Cost1,State),
        % 2 to go
        Size_BL1 is (R1 - I1) * J1,
        Size_BL2 is (R2 - I2) * J2,
        Cost2_LB is Cost1 + abs(Size_BL1 - Size_BL2) + 2,
        Cost2_LB < Best_Cost,
        diff_(BL1,BL2,Diff_BL,Cost1,Cost2,State),
        % 1 left
        Size_BR1 is (R1 - I1) * (C1 - J1),
        Size_BR2 is (R2 - I2) * (C2 - J2),
        Cost_LB is Cost2 + abs(Size_BR1 - Size_BR2) + 1,
        Cost_LB < Best_Cost,
        diff_(BR1,BR2,Diff_BR,Cost2,Cost,State),
        % Best be less than best
        Cost < Best_Cost,
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
                findall(Print-I-J,
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

fingerprint_compare(Delta,X-_-_,Y-_-_) :-
    compare(Delta,X,Y).

index_match_compare(Delta,M1,M2) :-
    compare(Delta,M1,M2).

collect_match_indices([],_,[]) :- !.
collect_match_indices(_,[],[]) :- !.
collect_match_indices([X-IX-JX|XR],[X-IY-JY|YR],[match(IX-JX,IY-JY)|R]) :-
    !,
    collect_match_indices(XR,YR,R).
collect_match_indices([X-IX-JX|XR],[Y-IY-JY|YR],Rest) :-
    compare(Delta,X,Y),
    (   Delta = (<)
    ->  collect_match_indices(XR,[Y-IY-JY|YR],Rest)
    ;   Delta = (>)
    ->  collect_match_indices([X-IX-JX|XR],YR,Rest)
    ).

match(Blocks1,Blocks2,Match_Indices) :-
    append(Blocks1,Prints1),
    predsort(fingerprint_compare,Prints1,Sorted_Prints1),
    append(Blocks2,Prints2),
    predsort(fingerprint_compare,Prints2,Sorted_Prints2),
    collect_match_indices(Sorted_Prints1,Sorted_Prints2,Unsorted_Indices),
    predsort(index_match_compare,Unsorted_Indices,Match_Indices).

block(Rows,Cols,[match(IX-JX,IY-JY)|Match_Indices],Block) :-
    (   block_(Rows,Cols,IX,JX,IY,JY,Match_Indices,Block)
    ;   block(Rows,Cols,Match_Indices,Block)
    ).

block_(Rows,Cols,IX,JX,IY,JY,Match_Indices,block(IX,JX,IX_BR,JX_BR,
                                                 IY,JY,IY_BR,JY_BR)) :-
    seek_top_right(Rows,Cols,IX,IY,Match_Indices,IX_TR,IY_TR),
    seek_bottom_left(Rows,Cols,IX,IY,IXTR,IYTR,Match_Indices,JX_BL,JY_BL),
    IX_BR is IX_TR + Rows,
    IY_BR is IY_TR + Rows,
    JX_BR is JX_BL + Cols,
    JY_BR is JY_BL + Cols.

seek_top_right(Rows,Cols,IX,IY,[],ITR,IY) :- !.
seek_top_right(Rows,Cols,IX,IY,[match(IXN-JXN,IYN,JYN)|Match_Indices],
               IXTR,IYTR) :-
    (   IXN is IX + 1,
        IYN is IY + 1
    ->  seek_top_right(Rows,Cols,IXN,IYN,Match_Indices,Match_Indices,
                       IXTR,IYTR)
    ;   IXTR = IX,
        IYTR = IY
    ).

rectangle(I_Max,J_Max,A,I,J) :-
    between(1,I_Max,I),
    between(1,J_Max,J),
    A is I * J.

areas(I_Max,J_Max,Areas) :-
    findall(A-I-J,
            (
                down_from(I_Max,1,I),
                down_from(J_Max,1,J),
                A is I * J
            ),
            AIJs),
    findall(A-Group, group_by(A, I-J, member(A-I-J,AIJs), Group), As),
    sort(1,>,As,Areas).

adiff(T1,T2,Diff,Cost) :-
    row_length(T1,R1),
    column_length(T1,C1),
    row_length(T2,R2),
    column_length(T2,C2),
    R is min(R1,R2),
    C is min(C1,C2),
    Rows is R div 2,
    Cols is C div 2,
    hash_blocks(Rows,Cols,T1,Blocks1),
    hash_blocks(Rows,Cols,T2,Blocks2),
    match(Blocks1,Blocks2,Match_Indices),
    copy_blocks(Rows,Cols,Match_Indices,Blocks).

/*

%seek_bottom_left

copy_blocks(Rows,Cols,Match_Indices,Blocks) :-
    findall(
        Block,
        block(Rows, Cols, Match_Indices, Block),
        Blocks).

% an approximate diff
adiff(T1,T2,Match_Indices,Blocks) :-
    row_length(T1,R1),
    column_length(T1,C1),
    row_length(T2,R2),
    column_length(T2,C2),
    R is min(R1,R2),
    C is min(C1,C2),
    Rows is R div 2,
    Cols is C div 2,
    hash_blocks(Rows,Cols,T1,Blocks1),
    hash_blocks(Rows,Cols,T2,Blocks2),
    match(Blocks1,Blocks2,Match_Indices),
    copy_blocks(Rows,Cols,Match_Indices,Blocks).


X X X X
X X X X
X X X X
X X X X

--------

X X X
X X X
X X X

X X X X +

X X
X X +

X
X
X
X


*/


/* Fast(er) table diff

Optimal diffs for tables is NP-Hard so we need to find an approximate
solution that feels "reasonable".

An approximate window-comparison algorithm

Start with two matrices M1 and M2

Keep track of all matches in a bit vector
F = [0,0,0,0,0,0,0...]

1) Find the min row (i) and column (j) parameters of M1 and M2
2) Create all windows of size A = i * j
   a) If maximal match (greatest number of non-overlapping matches)
      has been found (boundary of no match / match found) in F go to 5
   b) If A < 2 then return swap M1 for M2 and return calculated score
   c) If there are no more windows, use "search_down" procedure which
   cuts i,j in two and repeats from 2
3) Create matices H1 and H2 of all hashes of the values in the i*j
window convolved over M1 and M2 (respectively)
4) Search for matches of hash-values in H1 and H2
   a) If there are no matches, return to 2
   b) If there are matches
       i) Record in F at index i*j
       ii) increase A by .5 and repeat 2
5) Cut non-overlaping maximal match(es) from M1 and M2 (choose
arbitrarily from top-left preference)

6) Decompose M1 and M2 into 4 overlaping matrices:

  a) Left
  b) Top
  c) Bottom
  d) Right

7) Return to 1 for each of Left, Top, Bottom, Right

  a) Choose the maximal match from the alternatives starting with
  Left, Top, Bottom and then Right.
  b) Use best match scores to paste back together M1 and M2


M1:
   |     |
---|-----|---
   |X....|
   |....X|....
---|-----|----
   |    .|....
   |    X|....
   |    .|....

M2:

   |     |
---|-----|----=
   |X....|
   |....X|....
---|-----|-----
   |    .|....
   |     |X....
   |     |.....



*/

% I_Max is the maximum size in rows before running into the edge of one matrix
% J_Max is the maximum size in columns before running into the edge of one matrix
% A is the size of the area we are interested in.

fast_diff(T1,T2,Diff) :-
    row_length(T1,R1),
    column_length(T1,C1),
    row_length(T2,R2),
    column_length(T2,C2),
    I_Max is min(R1,R2),
    J_Max is min(C1,C2),
    areas(I_Max,J_Max,Areas),
    length(Areas, Length),
    length(Max_Area, Length),
    Area = I_Max * J_Max,
    binary_area_search(T1,T2,Area,Areas,Max_Area,Bounding_Boxes).

all_hash_matches(T1,T2,IJs,Matches) :-
    hash_blocks(Rows,Cols,T1,Blocks1),
    hash_blocks(Rows,Cols,T2,Blocks2),
    match(Blocks1,Blocks2,Matches).

binary_area_search(T1,T2,Area,Areas,Max_Area,Bounding_Boxes) :-
    all_hash_matches(T1,T2,IJs,Matches),
    (   maximum_match(Matches,Match)
    %   No match means we need to get smaller
    ->  nth(Area, Max_Area, Match),
        Previous is Area - 1,
        nth(Previous, Max_Area, Max),
        (   var(Max)
        ->  Next_Area = 
            binary_area_search()
        %   Boundary between max and non max is 
        ;   
    ;   true
    ).

maximum_match(Matches,Match) :-
    true.

/*
match_vector(A,[]).

max_blocks(A,I_Max,J_Max,Blocks) :-
    match_vector(A,F),
    search_max_blocks(A,I_Max,J_Max,Max_Vector,Blocks,F),
    rectangle(I_Max,J_Max,A,I,J).


search_max_blocks(A,Max_Vector,Blocks) :-
    (   A <= 2
    ->  Blocks = []
    ;   New_A is A div 2
    ->  search_max_blocks(New_A
    match(

test2 :-
    A is I_Max + J_Max,
    rectangle(I_Max,J_Max,A,I,J),
    match_blocks(I,J,T1,T2,Match_Blocks).

match_blocks(I,J,T1,T2,Match_Blocks) :-
    convolve(I,J,T1,H1),
    convolve(I,J,T2,H2),
    match(H1,H2,Match_Blocks).

*/

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
    writeq(Diff),
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
    Diff = swap(2,1,1,0,[[a],[c]],[],
                swap(1,1,0,1,[[b]],[],keep,keep,keep),keep,keep),
    cost(Diff,Cost),
    patch(Diff, M1, M2).

test(diff_pathological, []) :-
    M1	= [[o, o],
           [2, 3],
           [5, 6],
           [8, 9],
           [x, x]],
    M2	= [[2, 3, "goo"],
           [5, 6, "gar"],
           [8, 9, "gaz"]],
    diff(M1,M2, Diff, Cost),
    nl,writeq(Diff),nl,
    writeq(Cost).

test(diff_shared_middle, [
         %blocked('heat death of the universe anyone?')
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
    Diff = swap(5,1,3,0,[[o],[a],[b],[c],[d]],[],
                swap(1,3,0,4,[[o,o,o]],[],
                     keep,
                     copy(3,3,
                          swap(0,0,3,1,[],[["goo"],["gar"],["gaz"]],
                               keep,
                               keep,
                               keep),
                          swap(1,3,0,0,[[x,x,x]],[],
                               keep,
                               keep,
                               keep),
                          keep),
                     keep),
                keep,
                keep),

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
    hash_blocks(2,2,T1,Blocks1),

    T2 = [ [ 1, 2, 3, "goo" ],
           [ 4, 5, 6, "gar" ],
           [ 7, 8, 9, "gaz" ] ],

    hash_blocks(2,2,T2,Blocks2),

    writeq(Blocks1),
    writeq(Blocks2),

    %diff(T1,T2,Diff,Cost),
    %writeq(Diff),
    %cost(Diff, Cost),
    true.


test(block_match, []) :-
    Block1 = [['8688d301ebf8db90a862fc7ca2563276'-0-0,
               '9ad8d0dff9efe6243ef1028ee37d6255'-0-1,
               fa3690fa244566858f98fcc0bc1d4d6c-0-2],
              [bb3f8faf1064e167a7a8e1e96009da5c-1-0,
               '1f7d30ed644028dc78ee383d1935f7e5'-1-1,
               a7266ce2951c6247099f5489477f3d60-1-2],
              ['133b497528dc3a573ccc7b8c4850cf01'-2-0,
               '8651c12360cbcb4807bbe0e71cc6f091'-2-1,
               '439b71ef21da2f4f789af951fe6e25e2'-2-2],
              [f957370098b3f5a36a5bb821aa83dceb-3-0,
               '60ba7c730ae5713a6b2d6a1cc56697d2'-3-1,
               '93092b4e18d0e77439ce599b9697f3b2'-3-2]],

    Block2 = [['1f7d30ed644028dc78ee383d1935f7e5'-0-0,
               a7266ce2951c6247099f5489477f3d60-0-1,
               '88746a60a6baab9e79a8246496581c64'-0-2],
              ['8651c12360cbcb4807bbe0e71cc6f091'-1-0,
               '439b71ef21da2f4f789af951fe6e25e2'-1-1,
               e02cb232d6677d62e3b1c91dba9d53f8-1-2]],

    match(Block1,Block2,Match_Indices),

    Match_Indices = [match(1-1,0-0),
                     match(1-2,0-1),
                     match(2-1,1-0),
                     match(2-2,1-1)].

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



T3 = [ [ 1, |2, 3],
       [ 4, |5, 6],
       [ 7, |8, 9] ]


T4 = [ [ x, |2, 3],
       [ 4, |5, 6],
       [ 7, |8, 9] ]

generate_area(I_Max,J_Max, As),
member(I-J,As),

     A   (A-1)
F = [ 0 ,  1  , .. ]
This is maximal [ [], [[1,0,2,3],[0,1,3,2]], ......]

As = [ [3-3] , [2-3 , 3-2], [2-2], [1-3 , 3-1], [1-2 , 2-1] ]

A = 9  [ x ]  [ y ]
fails

A = 4, [2-2]
Two answers T = [ [ 4, 5],
                  [ 7, 8]]
 or

     T = [ [2, 3],
           [5, 6]]]

     T = [ [5, 6],
           [8, 9]]]

               A = 6                  A = 4
F = [ [], [[1,0,2,3],[0,1,3,2]], [[ ... ],[..]], ......]


[ 2, 3
  5, 6
  8, 9 ]

Cut_Left1 = [ 1,
              4,
              7 ]


As
list(BBs, Size_As),


5 rectangle generator fails

A := ( 9 + 5) div 2
7 rectangle generator fails

Index walking:


Match could be as:

    0  1  2  3  4  5  6
0   x  x  x  x  x  x  x
1   x  x  x  x  x  x  x
2   x        x  x  x  x
3   x  x  x  x  x  x  x  x
4            x  x  x  x
5            x  x  x  x

Walk should be:

    1  2  4  7 10 13 15      block(0,0,1,6)
    3  5  8 11 14 16 17
    6
    9
    12


[['8688d301ebf8db90a862fc7ca2563276'-0-0,'9ad8d0dff9efe6243ef1028ee37d6255'-0-1,fa3690fa244566858f98fcc0bc1d4d6c-0-2],
 [bb3f8faf1064e167a7a8e1e96009da5c-1-0,'1f7d30ed644028dc78ee383d1935f7e5'-1-1,a7266ce2951c6247099f5489477f3d60-1-2],
 ['133b497528dc3a573ccc7b8c4850cf01'-2-0,'8651c12360cbcb4807bbe0e71cc6f091'-2-1,'439b71ef21da2f4f789af951fe6e25e2'-2-2],
 [f957370098b3f5a36a5bb821aa83dceb-3-0,'60ba7c730ae5713a6b2d6a1cc56697d2'-3-1,'93092b4e18d0e77439ce599b9697f3b2'-3-2]]

[['1f7d30ed644028dc78ee383d1935f7e5'-0-0,a7266ce2951c6247099f5489477f3d60-0-1,'88746a60a6baab9e79a8246496581c64'-0-2],
 ['8651c12360cbcb4807bbe0e71cc6f091'-1-0,'439b71ef21da2f4f789af951fe6e25e2'-1-1,e02cb232d6677d62e3b1c91dba9d53f8-1-2]]

*/

