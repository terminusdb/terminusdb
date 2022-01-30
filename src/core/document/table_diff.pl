:- module('document/table_diff',
          [
              is_table/1,
              table_diff/3
          ]).

:- use_module(core(util)).
:- use_module(core('util/tables')).
:- use_module(library(clpfd)).
:- use_module(library(thread)).

/* Fast(er) table diff */

best_area(Op,I0-J0,I1-J1) :-
    A is max(I0,J0),
    B is max(I1,J1),
    (   A = B
    ->  (   I0 < I1
        ->  Op = (>)
        ;   I1 < I0
        ->  Op = (<)
        ;   Op = (=)
        )
    ;   A < B
    ->  Op = (>)
    ;   Op = (<)
    ).

areas(I_Max,J_Max,Min_Area,Areas) :-
    findall(A-I-J,
            (
                down_from(I_Max,1,I),
                down_from(J_Max,1,J),
                A is I * J,
                A >= Min_Area
            ),
            AIJs),
    findall(A-Group_Sorted,
            (   group_by(A, I-J, member(A-I-J,AIJs), Group),
                predsort(best_area,Group,Group_Sorted)
            ),
            As),
    sort(0,>,As,Areas).

windows(Width,Height,N,M,Exclusions,I,J) :-
    #(I) #=< #(N) - #(Width),
    #(J) #=< #(M) - #(Height),
    Exclusions = r(I,Width,J,Height),
    labeling([min(I),min(J)],[I,J]).

window_values_equal(X,Y) :-
    compare(Order,X,Y),
    Order = (=).

window_values_less(X,Y) :-
    X @< Y.

window_rectangle(Window,r(X,W,Y,H)) :-
    matrix_size(Window, W, H),
    window_offset(Window, X, Y).

not_overlapping_with(Window, Exclusions) :-
    window_rectangle(Window,Rectangle),
    \+ \+ Rectangle = Exclusions,
    disjoint2([Rectangle,Exclusions]).

extend_exclusions(_, [], _, _, Left_Windows0, Right_Windows0, Left_Windows0, Right_Windows0).
extend_exclusions([], _, _, _, Left_Windows0, Right_Windows0, Left_Windows0, Right_Windows0).
extend_exclusions([W1|Windows1],
                  [W2|Windows2],
                  Left_Exclusions, Right_Exclusions,
                  Left_Windows0, Right_Windows0,
                  Left_Windows1, Right_Windows1) :-
    window_values_equal(W1,W2),
    !,
    (   not_overlapping_with(W1,Left_Exclusions),
        not_overlapping_with(W2,Right_Exclusions)
    ->  extend_exclusions(Windows1,Windows2,
                          Left_Exclusions, Right_Exclusions,
                          [W1|Left_Windows0],[W2|Right_Windows0],
                          Left_Windows1,Right_Windows1)
    ;   extend_exclusions(Windows1,Windows2,
                          Left_Exclusions, Right_Exclusions,
                          Left_Windows0,Right_Windows0,
                          Left_Windows1,Right_Windows1)
    ).
extend_exclusions([W1|Windows1],
                  [W2|Windows2],
                  Left_Exclusions, Right_Exclusions,
                  Left_Windows0, Right_Windows0,
                  Left_Windows1, Right_Windows1) :-
    window_values_less(W1,W2),
    !,
    extend_exclusions(Windows1,
                      [W2|Windows2],
                      Left_Exclusions, Right_Exclusions,
                      Left_Windows0,Right_Windows0,
                      Left_Windows1,Right_Windows1).
extend_exclusions([W1|Windows1],
                  [_|Windows2],
                  Left_Exclusions, Right_Exclusions,
                  Left_Windows0, Right_Windows0,
                  Left_Windows1, Right_Windows1) :-
    extend_exclusions([W1|Windows1],
                      Windows2,
                      Left_Exclusions, Right_Exclusions,
                      Left_Windows0,Right_Windows0,
                      Left_Windows1,Right_Windows1).

% test_area(4,[4-[2-2,1-4,4-1]], M1, M2, [ ... ], [ ... ], Left_Exc, Right_Exc)
test_area(Area,Areas,M1,M2,
          Left_Exclusions,Right_Exclusions,
          Left_Windows0,Right_Windows0,
          Left_WindowsN,Right_WindowsN) :-
    memberchk(Area-WHs,Areas),

    foldl({M1,M2,Left_Exclusions,Right_Exclusions}/
          [W-H,
           Left_Windows_In-Right_Windows_In,
           Left_Windows_Out-Right_Windows_Out]>>(
              (   test_shape(W,H,M1,M2,
                             Left_Exclusions,Right_Exclusions,
                             Left_Windows,Right_Windows)
              ->  true
              ;   Left_Windows = [],
                  Right_Windows = []
              ),
              append(Left_Windows_In, Left_Windows, Left_Windows_Out),
              append(Right_Windows_In, Right_Windows, Right_Windows_Out)
          ),
          WHs,
          []-[],
          Left_Windows1-Right_Windows1),
    !,
    % Check to see that progress was made.
    \+ Left_Windows1 = [],
    \+ Right_Windows1 = [],
    append(Left_Windows0, Left_Windows1, Left_WindowsN),
    append(Right_Windows0, Right_Windows1, Right_WindowsN).

all_windows_of_shape(M,W,H,Exclusions,Windows) :-
    matrix_size(M,R,C),
    findall(Window,
            (   windows(W,H,R,C,Exclusions,X,Y),
                matrix_window(M,X,Y,W,H,Window)
            ),
            Windows_Unsorted),
    msort(Windows_Unsorted,Windows).

test_shape(W,H,M1,M2,
           Left_Exclusions, Right_Exclusions,
           Left_Windows,Right_Windows) :-

    all_windows_of_shape(M1,W,H,Left_Exclusions,Windows1),
    all_windows_of_shape(M2,W,H,Right_Exclusions,Windows2),

    extend_exclusions(Windows1,Windows2,
                      Left_Exclusions,Right_Exclusions,
                      [], [],
                      Left_Windows,Right_Windows).

previous_area(This,[Last-_,This-_|_],Last) :-
    !.
previous_area(This,[_-_,Not_This-_|Rest],Last) :-
    previous_area(This, [Not_This-_|Rest],Last).

next_area(This,[This-_,Next-_|_],Next) :-
    !.
next_area(This,[_-_|Rest],Next) :-
    next_area(This, Rest,Next).

% :- table area_search/10.
area_search(Left,Left,_,_,_,
            _,
            _, _,
            _, _) :-
    fail.
area_search(Left,Right,M1,M2,Areas,
            Area,
            Left_Exclusions,Right_Exclusions,
            Left_Windows0,Right_Windows0,
            Left_WindowsN,Right_WindowsN) :-
    Mid is (Left + Right) div 2,
    nth0(Mid, Areas, Test_Area-_),
    (   copy_term(Left_Exclusions,Left_Exclusions_Fresh),
        copy_term(Right_Exclusions,Right_Exclusions_Fresh),
        test_area(Test_Area,Areas,M1,M2,
                  Left_Exclusions_Fresh,Right_Exclusions_Fresh,
                  Left_Windows0,Right_Windows0,
                  Left_Windows1,Right_Windows1)
    %       Test that we are at the boundary of a success for matching
    %       (Shapes *must* be monotonic in testing, i.e. 00000011111)
    ->  (   \+ (Mid_minus_1 is Mid - 1,
                nth0(Mid_minus_1,Areas,Previous-_),
                test_area(Previous,Areas,M1,M2,
                          Left_Exclusions,Right_Exclusions,
                          [],[],
                          _,_))
        ->  Test_Area = Area,
            Left_Exclusions = Left_Exclusions_Fresh,
            Right_Exclusions = Right_Exclusions_Fresh,
            Left_Windows1 = Left_WindowsN,
            Right_Windows1 = Right_WindowsN
        ;   New_Right is Mid - 1,
            area_search(Left,New_Right,M1,M2,Areas,Area,
                        Left_Exclusions,Right_Exclusions,
                        Left_Windows0,Right_Windows0,
                        Left_WindowsN,Right_WindowsN)
        )
    ;   New_Left is Mid + 1,
        area_search(New_Left,Right,M1,M2,Areas,Area,
                    Left_Exclusions,Right_Exclusions,
                    Left_Windows0,Right_Windows0,
                    Left_WindowsN,Right_WindowsN)
    ).

%%
%% collect_windows(+,+,+,+,?,?,-,-,-,-)
%%
collect_windows(A0,T1,T2,Areas,LE,RE,LW0,RW0,LWN,RWN) :-
    nth0(L,Areas,A0-_),
    length(Areas,R),
    * format(user_error,'.',[]),
    area_search(L,R,T1,T2,Areas,A1,LE,RE,LW0,RW0,LW1,RW1),
    !,
    (   next_area(A1,Areas,A2)
    ->  collect_windows(A2,T1,T2,Areas,LE,RE,LW1,RW1,LWN,RWN)
    ;   LW1=LWN, RW1=RWN
    ).
collect_windows(_,_,_,_,_,_,LW,RW,LW,RW).

exclusion_template(T,Exclusions) :-
    matrix_size(T,N,M),
    exclusion_template(N,M,Exclusions).

exclusion_template(N,M,r(X,W,Y,H)) :-
    W in 0..sup,
    H in 0..sup,
    X in 0..sup,
    Y in 0..sup,
    #(W) #=< #(N),
    #(H) #=< #(M),
    #(X) #=< #(N) - #(W),
    #(Y) #=< #(M) - #(H).

atomize_table(LL,LLA) :-
    maplist([Row,RowA]>>
            maplist(term_to_atom,Row,RowA),
            LL,LLA).

table_diff_area_max(LL1,LL2,Diff) :-
    atomize_table(LL1,LLA1),
    atomize_table(LL2,LLA2),
    as_matrix(LLA1,T1),
    as_matrix(LLA2,T2),
    all_windows(T1,T2,LE,RE,LW,RW),
    build_diff(LL1,LL2,T1,T2,LE,RE,LW,RW,Diff).

all_windows(LL1,LL2,Left_Windows,Right_Windows) :-
    atomize_table(LL1,LLA1),
    atomize_table(LL2,LLA2),
    as_matrix(LLA1,T1),
    as_matrix(LLA2,T2),
    all_windows(T1,T2,_LE,_RE,Left_Windows,Right_Windows).

all_windows(T1,T2,LE,RE,Left_Windows,Right_Windows) :-
    matrix_size(T1,Length1,Height1),
    matrix_size(T2,Length2,Height2),
    Window_Length is min(Length1,Length2),
    Window_Height is min(Height1,Height2),
    Min_Area = 2,
    areas(Window_Length, Window_Height, Min_Area, Areas),
    Areas = [Area-_|_],
    exclusion_template(T1,LE),
    exclusion_template(T2,RE),
    collect_windows(Area,T1,T2,Areas,
                    LE,RE,
                    [],[],
                    Left_Windows,Right_Windows).

table_diff(M1,M2,Diff) :-
    table_diff_heuristic(M1,M2,Diff),
    * first_solution(Diff,
                     [
                         table_diff_heuristic(M1,M2,Diff),
                         table_diff_area_max(M1,M2,Diff)
                     ],
                     []).

table_diff_heuristic(LL1,LL2,Diff) :-
    atomize_table(LL1,LLA1),
    atomize_table(LL2,LLA2),
    as_matrix(LLA1,M1),
    as_matrix(LLA2,M2),
    heuristic_windows(M1,M2,LE,RE,LW,RW),
    build_diff(LL1,LL2,M1,M2,LE,RE,LW,RW,Diff).

heuristic_windows(LL1,LL2,Left_Windows,Right_Windows) :-
    atomize_table(LL1,LLA1),
    atomize_table(LL2,LLA2),
    as_matrix(LLA1,M1),
    as_matrix(LLA2,M2),
    heuristic_windows(M1,M2,_LE,_RE,Left_Windows,Right_Windows).

heuristic_windows(M1,M2,LE,RE,Left_Windows,Right_Windows) :-
    matrix_size(M1,Length1,Height1),
    matrix_size(M2,Length2,Height2),
    Window_Length is min(Length1,Length2),
    Window_Height is min(Height1,Height2),
    Min_Area = 2,
    Everything is Window_Length * Window_Height,
    Everything_Areas = [Everything-[Window_Length-Window_Height]],

    exclusion_template(M1,LE),
    exclusion_template(M2,RE),

    collect_windows(Everything,M1,M2,Everything_Areas,
                    LE,RE,
                    [],[],
                    Left_Windows0,Right_Windows0),
    * format(user_error, '~nTried everything~n', []),

    areas(Window_Length, 1, Min_Area, Row_Areas),
    Row_Areas = [Row_Area-Row_IJs|Rest_Row_Areas],
    Full_Row = [Row_Area-Row_IJs], % try full rows first
    collect_windows(Row_Area,M1,M2,Full_Row,
                    LE,RE,
                    Left_Windows0,Right_Windows0,
                    Left_Windows1,Right_Windows1),
    * format(user_error, '~nTried rows~n', []),

    areas(1, Window_Height, Min_Area, Column_Areas),
    Column_Areas = [Column_Area-Column_IJs|Rest_Column_Areas],
    Full_Column = [Column_Area-Column_IJs], % try full rows first

    collect_windows(Column_Area,M1,M2,Full_Column,
                    LE,RE,
                    Left_Windows1,Right_Windows1,
                    Left_Windows2,Right_Windows2),
    * format(user_error, '~nTried columns~n', []),

    (   Rest_Row_Areas = [Rest_Row_Area-_|_]
    ->  collect_windows(Rest_Row_Area,M1,M2,Rest_Row_Areas,
                        LE,RE,
                        Left_Windows2,Right_Windows2,
                        Left_Windows3,Right_Windows3),
        * format(user_error, '~nTried sub-rows~n', [])
    ;   Left_Windows2 = Left_Windows3,
        Right_Windows2 = Right_Windows3
    ),

    (   Rest_Column_Areas = [Rest_Column_Area-_|_]
    ->  collect_windows(Rest_Column_Area,M1,M2,Rest_Column_Areas,
                        LE,RE,
                        Left_Windows3,Right_Windows3,
                        Left_Windows,Right_Windows),
        * format(user_error, '~nTried sub-columns~n', [])
    ;   Left_Windows3 = Left_Windows,
        Right_Windows3 = Right_Windows
    ).

random_matrix(N,M,Matrix) :-
    findall(
        Row,
        (   between(1,M,_),
            length(Row, N),
            maplist([A]>>(
                        random(0,10,N),
                        term_to_atom(N,A)
                    ), Row)
        ),
        Matrix).

swap_in_row(N,M,Row1,Row2) :-
    Min is min(N,M),
    Max is max(N,M),
    swap_in_row_(Min,Max,Row1,Row2).

swap_in_row_(0, Max, [X|Row1], [Y|Row2]) :-
    !,
    replace(Max,X,Row1,Y,Row2).
swap_in_row_(N, Max, [X|Row1], [X|Row2]) :-
    Np is N - 1,
    swap_in_row_(Np, Max, Row1, Row2).

replace(1, X, [Y|Row], Y, [X|Row]) :- !.
replace(N, X, [A|Row1], Y, [A|Row2]) :-
    Np is N - 1,
    replace(Np, X, Row1, Y, Row2).

swap_columns(N,M,TS1,TS2) :-
    maplist({N,M}/[Row_In,Row_Out]>>
            swap_in_row(N,M,Row_In,Row_Out),
            TS1, TS2).

position_list(Position,[X,Y,W,H]) :-
    get_dict('@x', Position, X),
    get_dict('@height',Position, H),
    get_dict('@y', Position, Y),
    get_dict('@width',Position, W).

position_sort(Patches,Sorted) :-
    predsort([Op,Patch1,Patch2]>>
             (   get_dict('@at',Patch1,P1),
                 position_list(P1,L1),
                 get_dict('@at',Patch2,P2),
                 position_list(P2,L2),
                 compare(Op,L1,L2)
             ),  Patches, Sorted).

move_sort(Patches,Sorted) :-
    predsort([Op,Patch1,Patch2]>>
             (   get_dict('@from', Patch1, From_P1),
                  position_list(From_P1,From_L1),
                  get_dict('@to', Patch1, To_P1),
                  position_list(To_P1, To_L1),
                  get_dict('@from', Patch2, From_P2),
                  position_list(From_P2,From_L2),
                  get_dict('@to', Patch2, To_P2),
                  position_list(To_P2, To_L2),
                  compare(Op,[From_L1,To_L1],[From_L2,To_L2])
              ),  Patches, Sorted).

build_diff(LL1,LL2,M1,M2,Exclusions1,Exclusions2,Matches1,Matches2,Diff) :-
    sort(Matches1,Matches1_Sorted),
    sort(Matches2,Matches2_Sorted),
    diff_copies_and_moves(Matches1_Sorted,Matches2_Sorted,
                          LL1,LL2,
                          M1,M2,
                          Matches, Moves),
    diff_unmatched(LL1,M1,Exclusions1,Deletes),
    diff_unmatched(LL2,M2,Exclusions2,Inserts),
    move_sort(Moves,MovesS),
    position_sort(Matches,MatchesS),
    position_sort(Inserts,InsertsS),
    position_sort(Deletes,DeletesS),
    matrix_size(M1,R1,C1),
    matrix_size(M2,R2,C2),
    Diff =
    json{ '@op' : "ModifyTable",
          dimensions : _{ '@before' : [R1,C1],
                          '@after' : [R2,C2] },
          copies : MatchesS,
          moves : MovesS,
          inserts : InsertsS,
          deletes : DeletesS }.

diff_copies_and_moves([],[],_,_,_,_,[],[]) :-
    !.
diff_copies_and_moves([W1|Matches1],[W2|Matches2],LL1,LL2,M1,M2,[Copy|Copies],Moves) :-
    % this is a copy
    window_rectangle(W1,R),
    window_rectangle(W2,R),
    !,
    R =.. [_,X,Width,Y,Height],
    table_window(X,Width,Y,Height,LL1,LL),
    Copy = json{ '@value' : LL,
                 '@at' : json{ '@x' : X,
                               '@y' : Y,
                               '@width' : Width,
                               '@height' : Height
                             }},
    diff_copies_and_moves(Matches1,Matches2,LL1,LL2,M1,M2,Copies,Moves).
diff_copies_and_moves([W1|Matches1],[W2|Matches2],LL1,LL2,M1,M2,Copies,[Move|Moves]) :-
    % this is a move
    window_rectangle(W1,R1),
    window_rectangle(W2,R2),
    !,
    R1 =.. [_,X1,Width,Y1,Height],
    R2 =.. [_,X2,_,Y2,_],
    table_window(X1,Width,Y1,Height,LL1,LLW),
    Move = json{ '@value' : LLW,
                 '@from' : json{ '@x' : X1,
                                 '@y' : Y1,
                                 '@width' : Width,
                                 '@height' : Height
                               },
                 '@to' : json{ '@x' : X2,
                               '@y' : Y2,
                               '@width' : Width,
                               '@height' : Height
                             }},
    diff_copies_and_moves(Matches1,Matches2,LL1,LL2,M1,M2,Copies,Moves).

diff_unmatched(LL,M,Exclusions,Unmatched) :-
    all_windows_of_shape(M,1,1,Exclusions,Windows),
    maplist({LL}/[W,U]>>(
                window_rectangle(W,R),
                R =.. [_,X,Width,Y,Height],
                table_window(X,Width,Y,Height,LL,LLW),
                U = json{ '@value' : LLW,
                          '@at' : json{ '@x' : X,
                                        '@y' : Y,
                                        '@width' : Width,
                                        '@height' : Height
                                      }}
            ),
            Windows,
            Unmatched).

:- begin_tests(table_diff).

test(windows_1x3, []) :-

    %% All, 1x3 windows
    %%
    %%   0 1 2
    %% 0 . . .
    %% 1 . . x
    %% 2 . . .

    N = 3,
    M = 3,
    R = r(2,1,1,1),
    exclusion_template(N,M,Exclusions),
    disjoint2([R,Exclusions]),
    findall(I-J,windows(1,3,N,M,Exclusions,I,J), IJs),
    IJs = [0-0,1-0].

test(windows_2x2, []) :-

    %% All, 2x2 windows
    %%
    %%   0 1 2 3
    %% 0 . . . .
    %% 1 . . x x
    %% 2 x . . .
    %% 3 . . . .

    N = 4,
    M = 4,
    exclusion_template(N,M,Exclusions),
    disjoint2([r(2,2,1,1),r(0,1,2,1),Exclusions]),
    findall(I-J,windows(2,2,N,M,Exclusions,I,J), IJs),
    IJs = [0-0,1-2,2-2].

test(match_2x1_window, []) :-
    T1 = [ [ a , b ] ],

    T2 = [ [ a , b ] ],

    all_windows(T1,T2,W1,W2),
    maplist(window_rectangle,W1,E1),
    maplist(window_rectangle,W2,E2),
    E1 = [r(0,2,0,1)],
    E2 = [r(0,2,0,1)].

test(match_3x3_window, []) :-
    T1 = [ [ a, x, y, z, a],
           [ b, '1', '2', '3', b],
           [ c, '4', '5', '6', c],
           [ d, '7', '8', '9', d],
           [ a, a, a, a, a] ],

    T2 = [ [ '1', '2', '3'],
           [ '4', '5', '6'],
           [ '7', '8', '9'] ],

    all_windows(T1,T2,W1,W2),
    maplist(window_rectangle,W1,E1),
    maplist(window_rectangle,W2,E2),
    E1 = [r(1,3,1,3)],
    E2 = [r(0,3,0,3)].

% can we make 10x1000 take a second?
test(random_10x10, []) :-
    T1 = [[7,4,9,5,6,0,0,7,5,2],
          [4,7,9,6,5,1,3,6,6,3],
          [6,2,2,5,0,9,9,4,5,6],
          [7,6,0,1,9,8,3,4,9,2],
          [7,3,6,2,7,0,7,5,5,7],
          [5,2,4,9,9,1,9,8,6,0],
          [7,2,5,6,9,6,0,8,3,7],
          [9,4,5,7,8,6,4,9,1,8],
          [7,7,1,4,3,7,0,7,4,2],
          [8,9,5,5,0,4,2,4,8,2]],

    T2 = [[2,6,9,2,8,6,7,9,5,5],
          [9,4,3,6,8,9,3,4,6,0],
          [6,1,7,0,5,1,6,9,4,4],
          [0,0,2,4,5,2,3,5,4,2],
          [7,9,6,2,8,5,9,5,3,1],
          [0,5,9,9,1,8,9,6,0,8],
          [6,7,0,3,1,6,0,1,3,0],
          [7,7,4,6,4,5,7,2,4,7],
          [9,8,8,6,1,7,9,3,2,6],
          [2,1,4,6,1,6,8,4,7,2]],
    all_windows(T1,T2,W1,W2),
    length(W1,L1),
    length(W2,L2),
    L1 < 30, L1 > 20,
    L2 < 30, L2 > 20.

window_blocks(M,WS,Blocks) :-
    maplist({M}/[Window,Block]>>(
                window_rectangle(Window,r(X,W,Y,H)),
                table_window(X,W,Y,H,M,Block)
            ),WS,Blocks).

test(random_12x12, []) :-
    T1 = [[9,1,6,4,8,1,0,9,8,9,2,4],
          [4,8,9,4,2,2,5,7,1,2,5,4],
          [1,0,7,7,0,8,2,6,8,6,9,4],
          [5,9,2,9,0,4,8,1,8,0,0,7],
          [6,5,8,4,2,6,5,3,2,2,8,4],
          [7,9,6,9,6,3,3,4,6,8,3,7],
          [7,2,1,7,4,4,8,0,4,3,9,7],
          [8,9,3,5,6,9,6,9,0,0,7,2],
          [0,7,0,0,6,8,2,2,0,9,1,6],
          [7,0,9,7,5,6,7,8,4,3,7,5],
          [1,5,9,6,7,8,3,6,2,4,0,0],
          [0,3,6,8,8,8,0,0,1,8,1,9]],

    T2 = [[5,2,0,4,0,3,0,6,2,7,3,1],
          [4,4,0,1,4,0,6,0,9,2,7,7],
          [6,9,0,5,6,5,1,2,0,5,5,4],
          [3,9,6,5,1,4,6,1,0,9,7,3],
          [8,8,5,5,7,8,4,5,1,3,8,8],
          [1,8,2,5,4,9,6,6,2,7,4,3],
          [2,7,7,6,4,9,8,3,9,2,6,9],
          [0,8,4,8,2,3,5,4,4,2,7,7],
          [3,1,8,9,7,4,3,2,2,0,9,6],
          [5,4,7,9,6,7,8,4,4,3,4,5],
          [4,8,0,2,7,9,5,3,8,9,2,7],
          [4,7,6,4,5,2,7,0,6,5,6,4]],

    all_windows(T1,T2,WS1,WS2),
    length(WS1,L1),
    length(WS2,L2),
    L1 > 30, L1 < 42,
    L2 > 30, L2 < 42.

test(random_12x12_heuristic, []) :-
    T1 = [[9,1,6,4,8,1,0,9,8,9,2,4],
          [4,8,9,4,2,2,5,7,1,2,5,4],
          [1,0,7,7,0,8,2,6,8,6,9,4],
          [5,9,2,9,0,4,8,1,8,0,0,7],
          [6,5,8,4,2,6,5,3,2,2,8,4],
          [7,9,6,9,6,3,3,4,6,8,3,7],
          [7,2,1,7,4,4,8,0,4,3,9,7],
          [8,9,3,5,6,9,6,9,0,0,7,2],
          [0,7,0,0,6,8,2,2,0,9,1,6],
          [7,0,9,7,5,6,7,8,4,3,7,5],
          [1,5,9,6,7,8,3,6,2,4,0,0],
          [0,3,6,8,8,8,0,0,1,8,1,9]],

    T2 = [[5,2,0,4,0,3,0,6,2,7,3,1],
          [4,4,0,1,4,0,6,0,9,2,7,7],
          [6,9,0,5,6,5,1,2,0,5,5,4],
          [3,9,6,5,1,4,6,1,0,9,7,3],
          [8,8,5,5,7,8,4,5,1,3,8,8],
          [1,8,2,5,4,9,6,6,2,7,4,3],
          [2,7,7,6,4,9,8,3,9,2,6,9],
          [0,8,4,8,2,3,5,4,4,2,7,7],
          [3,1,8,9,7,4,3,2,2,0,9,6],
          [5,4,7,9,6,7,8,4,4,3,4,5],
          [4,8,0,2,7,9,5,3,8,9,2,7],
          [4,7,6,4,5,2,7,0,6,5,6,4]],

    heuristic_windows(T1,T2,WS1,WS2),
    window_blocks(T1,WS1,Blocks1),
    window_blocks(T2,WS2,Blocks2),
    length(Blocks1, L1),
    length(Blocks2, L2),
    L1 > 35, L1 < 42,
    L2 > 35, L2 < 42.

test(swap_columns, []) :-
    T1 = [[a,b,c,d],
          [e,f,g,h],
          [i,j,k,l],
          [m,n,o,p]],

    swap_columns(0,1,T1,T2),
    heuristic_windows(T1,T2,WS1,WS2),

    maplist({T1,T2}/[Window1,Window2,Block1,Block2]>>(
                window_rectangle(Window1,r(X1,W1,Y1,H1)),
                table_window(X1,W1,Y1,H1,T1,Block1),
                window_rectangle(Window2,r(X2,W2,Y2,H2)),
                table_window(X2,W2,Y2,H2,T2,Block2)
            ),WS1,WS2,Blocks1,Blocks2),
    sort(Blocks1, Blocks_Sorted1),
    sort(Blocks2, Blocks_Sorted2),

    Blocks_Sorted1 = [[[a],[e],[i],[m]],
                      [[b],[f],[j],[n]],
                      [[c],[g],[k],[o]],
                      [[d],[h],[l],[p]]],
    Blocks_Sorted2 = [[[a],[e],[i],[m]],
                      [[b],[f],[j],[n]],
                      [[c],[g],[k],[o]],
                      [[d],[h],[l],[p]]].

test(swap_columns_and_sort, []) :-
    T1 = [[a,b,c,d],
          [e,f,g,h],
          [i,j,k,l],
          [m,n,o,p]],

    swap_columns(0,1,T1,T2),
    T1=[H1|TT1],
    T2=[H2|TT2],
    sort(0,@>,TT2,TS2),

    HT1 = [H1|TT1],
    HT2 = [H2|TS2],

    [H2|TS2] = [[b,a,c,d],
                [n,m,o,p],
                [j,i,k,l],
                [f,e,g,h]],

    heuristic_windows(HT1,HT2,WS1,WS2),
    window_blocks(HT1,WS1,Blocks_Unsorted1),
    window_blocks(HT2,WS2,Blocks_Unsorted2),
    sort(Blocks_Unsorted1,Blocks1),
    sort(Blocks_Unsorted2,Blocks2),

    Blocks1 = [[[c,d]],
               [[g,h]],
               [[k,l]],
               [[o,p]]],
    Blocks2 = [[[c,d]],
               [[g,h]],
               [[k,l]],
               [[o,p]]].

spreadsheet1(
    [
        ['Job Title','Company','Location','Company Size','Company Industry'],
        ['Sr. Mgt.','Boeing','USA','Large','Aerospace'],
        ['Data Architect','Airbus','France','Large','Aerospace'],
        ['Founder','Ellie Tech','Sweden','Startup','AI'],
        ['Platform Engineer','Adidas','Germany','Large','Apparel'],
        ['Lead Data Engineering ','Auto Trader','Uk',' Medium','Automotive'],
        ['Architect','Mercedes-Benz','Germany','Large','Automotive'],
        ['Data Engineer','Auto Maker','USA','Medium','Automotive'],
        ['Data Architect','Big Bank','Australia','Large','Bank'],
        ['Data Analytics','L\'Oreal','France','Large','Beauty'],
        ['Chief Technologist','tag.bio','USA','Startup','bioinformatics'],
        ['lead a Digital Capabilities team','Ecolab','USA','Large','Chemicals'],
        ['Lead Data & Analytics','Intel','USA','Large','Chips'],
        ['Data Director','Manutan','France','Medium','Commerce'],
        ['Architect','Liberty Mutual Tech','USA','Large','Conglomerate'],
        ['Lead Data Engineering','Wipro','India','Large','Consultancy'],
        ['Partner & MD','BCG Gamma','USA','Large','Consultancy'],
        ['Advisory Consultant','ThoughtWorks ','Germany','Medium','Consultancy'],
        ['Consultant','PwC','Norway','Large','Consultancy'],
        ['VP Tech','PMSquare','USA','Small','Consultancy'],
        ['Partner','ArtefactAsia','China','Small','Consultancy'],
        ['Data Strategist','Credera','UK','Medium','Consultancy'],
        ['CTO','Idean','France','Large','Consultancy'],
        ['Data Engineer','Itility','Netherlands','Small','Consultancy'],
        ['Founder','Leukos.io','Germany','Small','Consultancy'],
        ['Software Developer','IBM','USA','Large','Consultancy'],
        ['Architect','Webstep','Norway','Small','Consultancy'],
        ['software engineer','Netlight','Germany','Small','Consultancy'],
        ['Consultant','Octo Technology','France','Small','Consultancy'],
        ['Data Engineer','Syntio','Croatia','Small','Consultancy'],
        ['Lead Data Analytics','Tata Consultancy','India','Large','Consultancy'],
        ['Data Practice','EY Consulting','Australia','Large','Consultancy'],
        ['Data Engineering Lead','Tiger Analytics','USA','Medium','Consultancy'],
        ['CTO','Graph Aware','Italy','Startup','Consultancy'],
        ['CEO','YPSI','France','Small','Consultancy'],
        ['Data Consultant','Solita ','Sweden','Medium','Consultancy'],
        ['Data Architect','Futuric','Finland','Small','Consultancy'],
        ['Architect ','Capgemini ','Norway','Large','Consulting'],
        ['Director of Data Engineering','Slalom Build','USA','Large','Consulting'],
        ['Partner Engineer','Ripple Labs','USA','Scale up','Crypto'],
        ['Chief Architect','TecAlliance','Germany','Medium','Data for Automotive'],
        ['VP Product','Immuta','USA','Scale up','Data Governance'],
        ['Product Manager','Dremio','USA','Scale up','Data Lake'],
        ['CEO','MayaData','USA','Startup','Data management'],
        ['data engineer','Apollo GraphQL','USA','Startup','Data management'],
        ['lead product offering','RAW Labs','USA','Startup','Data management'],
        ['solutions architect','AWS','USA','Large','Data management'],
        ['technology evangelist','Brobridge','Taiwan','Startup','Data Management'],
        ['Architect','Datavard','Germany','Medium','Data management'],
        ['Analytics Architect','Pure Storage','USA','Large','Data Management'],
        ['DevRel','DataStax','USA','Scale up','Data management'],
        ['Engineering Manager','IOTICS','Germany','startup','Data management'],
        ['Customer Engineer ','Microsoft','USA','Large','Data management'],
        ['Knowledge Manager','LucidWorks','USA','Scale up','Data management'],
        ['Founder','Kensu.io','USA','Startup','Data management'],
        ['Engineer','Google','USA','Large','Data management'],
        ['Founder','Nexla','USA','Startup','Data management'],
        ['Solutions Engineer','Snowflake','USA','Scale up','Data management'],
        ['Architect','Refinitiv','UK','Large','Data management'],
        ['Founder','Information Fabrik','Germany','Medium','Data management'],
        ['Founder','Secoda.co','Canada','Startup','Data management'],
        ['Founder ','Select Star','USA','Startup','Data management'],
        ['CTO','Sarus.tech','France','Startup','Data management'],
        ['Director of Category','Cinchy','Canada','Startup','Data management'],
        ['Founder','Daracoral','USA','Startup','Data management'],
        ['Product Manager ','Confluent','USA','Scale up','Data management'],
        ['Founder','Keboola','Czechia','Startup','Data management'],
        ['Founder','LakeFS','Israel','Startup','Data management'],
        ['Principal Scientist','Data.world','USA','Scale up','Data management'],
        ['Principal Data Scientist','Zapier','USA','Scale up','Data management'],
        ['Founder','DataHub (LinkedIn spin out)','USA','Startup','Data management'],
        ['CEO','Monte Carlo','Israel','Startup','Data management'],
        ['Solutions Architect','Solace','Canada','Startup','Data management'],
        ['Director Product','Medidata','UK','Small','Data management'],
        ['Product Manager','Lotics','UK','Small','Data Providers'],
        ['strategist','Oracle','USA','Large','Databases'],
        ['Group Product Manager','Tinder','USA','Scaleup','Dating'],
        ['Data Product Manager','Just Eat Takeaway','USA','Scale up','Delivery App'],
        ['Head of Data','Labelium','Spain','Small','Digital Agency'],
        ['Head of Data Engineering','Estante Magica','Brazil','Startup','EdTech'],
        ['Data Engineer','Cloud Academy','UK','Startup','EdTech'],
        ['Data Engineer','Amsterdam University','Netherlands','Large','Education'],
        ['IT architect','Alliander','Netherlands','Large','Energy'],
        ['Enterprise Architect','Schlumberger','USA','Large','Energy'],
        ['Senior Architect ','Schneider Electric','France','Large','Energy'],
        ['Head of ML Engineering ','Origami Energy','UK','Small','Energy'],
        ['Knowledge Engineer','Siemens Energy','Germany','Large','Energy'],
        ['CDO','Total','France','Large','Energy'],
        ['Solution Architect','ENI','Italk','Large','Energy'],
        ['Data Architect','HERA','Italy','Large','Energy'],
        ['Director','Arup','Netherlands','Large','Engineering'],
        ['Architect','Philips','Netherlands','Large','Engineering'],
        ['Consultant','TNG Consultancy','Germany','Medium','Engineering Consultancy'],
        ['data engineer','OPUS','Germany','Small','Fashion'],
        ['Data Engineer','Lookiero','USA','Startup','Fashion'],
        ['Director Data Engineering','Aritzia','Canada','Large','Fashion'],
        ['Data Engineer','Netflix','USA','Scale up','Film'],
        ['Enterprise Architect','Northern Trust ','USA','Large','Finance'],
        ['Head of Data Engineering','Kestra Financial','USA','Large','Finance'],
        ['Data Strategist','Al Rajhi Bank','Saudi','Large','Finance'],
        ['Senior DB Dev ','Fisher Investment','USA','Large','Finance'],
        ['Data Manager','Lowell Financial','UK','Large','Finance'],
        ['Head of Platform','Schroders Investment Management','UK','Large','Finance'],
        ['Data Architect','Nationwide ','UK','Large','Finance'],
        ['Enterprise Architect','TD Bank','Canada','Large','Finance'],
        ['Chief Data Officer','XP Inc','Brazil','Large','Finance'],
        ['Data Engineer','SEB','Sweden','Large ','Finance'],
        ['Enterprise Architect','Bank of New Zealand','New Zealand','Large','Finance'],
        ['Architect','UBS','UK','Large','Finance'],
        ['Head Data Integration','Saxo Bank','Netherlands','Large','Finance'],
        ['VP of Data ','SoFi','USA','Scale up','Fintech'],
        ['Data Engineer','B89','Peru','startup','FinTech'],
        ['Data Engineering Director','Kyriba','USA','Scale up','FinTech'],
        ['Data Architect','WEX','USA','Scale up','FinTech'],
        ['Principal Engineer','Intuit','USA','scaleup','FinTech'],
        ['Lead Data Engineer','Afterpay','Australia','Large','FinTech'],
        ['Data Engineer','Jumo.world','South Africa','Startup','Fintech'],
        ['CTO','RVU','UK','Startup','Fintech'],
        ['Enterprise Architect','Voya','USA','Scale up','Fintech'],
        ['Founder','DataDiligence','Switzerland','Startup','Fintech'],
        ['Senior Data Engineer','WorldRemit','UK','Scale up','Fintech'],
        ['Data Platform Lead','OLX Group','Germany','Large','Fintech'],
        ['VP Data Engineering','Pleo','Denmark','Startup','Fintech'],
        ['Data Engineer','B89','Peru','Startup','Fintech'],
        ['Senior Data Engineering','Peloton','USA','Scale up','Fitness'],
        ['engineering manager + data engineer','HelloFresh','Germany','Scale up','Food'],
        ['Head of master data','Nobia','Sweden','Medium','Furniture'],
        ['Data Product Lead','Norwegian public roads administration','Norway','Large','Government'],
        ['Head of Data Engineering','Norwegian Gov','Norway','Large','Government'],
        ['Principal Engineer','NAV','Norway','Large','Government'],
        ['Consultant','European Commission','EU','Large','Government'],
        ['Owner Data Analytics','City of Zurich','Switzerland','Large','Government'],
        ['Consultant Architect','Dutch Railroad','Netherlands','Large','Government'],
        ['Senior Solution Architect','CTTI','Catalonia','Large','Government'],
        ['Principal Software Engineer','AthenaHealth','USA','Large','Health'],
        ['Data Engineer','FindHotel','UK','Startup','Hotel Search'],
        ['Interactive Team','Workday','USA','Scale up','HR management software'],
        ['Head Data Strategy','Manulife ','Canada','Large','Insurance'],
        ['Head of Data','Uniqa','Germany','Large','Insurance'],
        ['Director of Data Engineering','Flexera','Northern Ireland','Large','IT Solutions'],
        ['CTO','TermScout','USA','Startup','LegalTech'],
        ['Tech Lead','ZenCargo','UK','Scale up','Logistics'],
        ['engineering manager ','Flexport','USA','Scale up','Logistics'],
        ['Lead Data Engineer','Maersk','Denmark','Large','Logistics'],
        ['Tech Lead','Reckitt','UK','Large','Manufacturer'],
        ['Head Data Engineering','Bose Corp','USA','Large','Manufacturing'],
        ['VP Platform','kenshoo.com','Israel','Medium','MarTech'],
        ['Data Architect','RTL','Netherlands','Large','Media'],
        ['Principal Architect','ITV','UK','Large ','Media'],
        ['Head of Tech','Signal-AI','UK','Scale up','Media'],
        ['Data Manager','DPG Media','Belgium','Large','Media'],
        ['Integration Architect','BHP','South Africa','Large','Mining '],
        ['Founder','Evidently AI','USA','Startup','MLoOps'],
        ['Marketing','Molecula','USA','Startup','MLOps'],
        ['Founder','Elementi','USA','Startup','MLOps'],
        ['Data Architect','Carfax','USA','Large','Motor '],
        ['Data Engineering Lead','Epidemic Sound','Sweden','Scale up','Music'],
        ['engineering manager','Rudderstack','USA','startup','OSS data pipeline'],
        ['Data Architect','CleverTech','Peru','Small','Outsource IT'],
        ['Data Engineer','SysMap Solutions','Brazil','Medium','Outsource IT'],
        ['Data Engineering Lead','Altimetrik','India','Scale up','Outsource IT'],
        ['Software Architect','Derivco','South Africa','Medium','Outsource IT'],
        ['architect','Roche Polska','Poland','Large','Pharma'],
        ['department of Research Informatics & Software Engineering','Genentech Research','USA','Large (part of Roche)','Pharma'],
        ['Professional Cloud Architect','Roche','USA','Large','Pharma'],
        ['Tech Lead Data Engineering','Prezi','Hungary','Scale up','Presentation Software'],
        ['Chief Architect','Zillow','USA','Scale up','Property Tech'],
        ['Data Infra','Airbnb','USA','Scale up','Property Tech'],
        ['Data Engineer ','Springer Nature','Germany','Large','Publishing'],
        ['Lead Data Citizenship','SEEK','Australia','Medium','Recruitment'],
        ['Director of Data Mesh','Gloo','USA','Startup','Religion'],
        ['CEO','Psyda','Pakistan','Startup','Research'],
        ['Architect','G Research','UK','Small','Research'],
        ['Research Director','45 Research','USA','Medium','Research'],
        ['lead Infrastructure','Instacart','USA','Scale up','Retail'],
        ['Data Engineer','IKEA','Sweden','Large','Retail'],
        ['Data Modelers x 2','Breuninger','Germany','Large','Retail'],
        ['Data Team Leader','AO.com','UK','Medium','Retail'],
        ['Head of Data Science','Franprix','France','Large','Retail'],
        ['leading the data team','Starship Technologies','USA','Scale up','Robotics'],
        ['software engineer','Abnormal Security','USA','Scale up','Security'],
        ['Chief Data Officer','Avast','Czechia','Scale up','Security'],
        ['Pre Sales Engineering','Orange Business Services','France','Large','Telco'],
        ['Cyber Analytics','TELUS','Canada','Large','Telco'],
        ['Product Owner','BT','UK','Large','Telecoms'],
        ['Lead Data Engineer','Chantelle','France','Medium','Textiles'],
        ['Director Data Science ','Booking.com','Netherlands','Large','Travel']
    ]).

spreadsheet2(
    [
        ['Job Title','Company','Location','Company Size','Company Industry'],
        ['Sr. Mgt.','Boeing','USA','Large','Aerospace'],
        ['Data Architect','Airbus','France','Large','Aerospace'],
        ['Founder','Ellie Tech','Sweden','Startup','AI'],
        ['Platform Engineer','Adidas','Germany','Large','Apparel'],
        ['Lead Data Engineering ','Auto Trader','Uk',' Medium','Automotive'],
        ['Architect','Mercedes-Benz','Germany','Large','Automotive'],
        ['Data Engineer','Auto Maker','USA','Medium','Automotive'],
        ['Data Architect','Big Bank','Australia','Large','Bank'],
        ['Data Analytics','L\'Oreal','France','Large','Beauty'],
        ['Chief Technologist','tag.bio','USA','Startup','bioinformatics'],
        ['lead a Digital Capabilities team','Ecolab','USA','Large','Chemicals'],
        ['Lead Data & Analytics','Intel','USA','Large','Chips'],
        ['Data Director','Manutan','France','Medium','Commerce'],
        ['Architect','Liberty Mutual Tech','USA','Large','Conglomerate'],
        ['Lead Data Engineering','Wipro','India','Large','Consultancy'],
        ['Partner & MD','BCG Gamma','USA','Large','Consultancy'],
        ['Advisory Consultant','ThoughtWorks ','Germany','Medium','Consultancy'],
        ['Consultant','PwC','Norway','Large','Consultancy'],
        ['VP Tech','PMSquare','USA','Small','Consultancy'],
        ['Partner','ArtefactAsia','China','Small','Consultancy'],
        ['Data Strategist','Credera','UK','Medium','Consultancy'],
        ['CTO','Idean','France','Large','Consultancy'],
        ['Data Engineer','Itility','Netherlands','Small','Consultancy'],
        ['Founder','Leukos.io','Germany','Small','Consultancy'],
        ['Software Developer','IBM','USA','Large','Consultancy'],
        ['Architect','Webstep','Norway','Small','Consultancy'],
        ['software engineer','Netlight','Germany','Small','Consultancy'],
        ['Consultant','Octo Technology','France','Small','Consultancy'],
        ['Data Engineer','Syntio','Croatia','Small','Consultancy'],
        ['Lead Data Analytics','Tata Consultancy','India','Large','Consultancy'],
        ['Data Practice','EY Consulting','Australia','Large','Consultancy'],
        ['Data Engineering Lead','Tiger Analytics','USA','Medium','Consultancy'],
        ['CTO','Graph Aware','Italy','Startup','Consultancy'],
        ['CEO','YPSI','France','Small','Consultancy'],
        ['Data Consultant','Solita ','Sweden','Medium','Consultancy'],
        ['Data Architect','Futuric','Finland','Small','Consultancy'],
        ['Architect ','Capgemini ','Norway','Large','Consulting'],
        ['Director of Data Engineering','Slalom Build','USA','Large','Consulting'],
        ['Partner Engineer','Ripple Labs','USA','Scale up','Crypto'],
        ['Chief Architect','TecAlliance','Germany','Medium','Data for Automotive'],
        ['VP Product','Immuta','USA','Scale up','Data Governance'],
        ['Product Manager','Dremio','USA','Scale up','Data Lake'],
        ['CEO','MayaData','USA','Startup','Data management'],
        ['data engineer','Apollo GraphQL','USA','Startup','Data management'],
        ['lead product offering','RAW Labs','USA','Startup','Data management'],
        ['solutions architect','AWS','USA','Large','Data management'],
        ['technology evangelist','Brobridge','Taiwan','Startup','Data Management'],
        ['Architect','Datavard','Germany','Medium','Data management'],
        ['Analytics Architect','Pure Storage','USA','Large','Data Management'],
        ['DevRel','DataStax','USA','Scale up','Data management'],
        ['Engineering Manager','IOTICS','Germany','startup','Data management'],
        ['Customer Engineer ','Microsoft','USA','Large','Data management'],
        ['Knowledge Manager','LucidWorks','USA','Scale up','Data management'],
        ['Founder','Kensu.io','USA','Startup','Data management'],
        ['Engineer','Google','USA','Large','Data management'],
        ['Founder','Nexla','USA','Startup','Data management'],
        ['Solutions Engineer','Snowflake','USA','Scale up','Data management'],
        ['Architect','Refinitiv','UK','Large','Data management'],
        ['Founder','Information Fabrik','Germany','Medium','Data management'],
        ['Founder','Secoda.co','Canada','Startup','Data management'],
        ['Founder ','Select Star','USA','Startup','Data management'],
        ['CTO','Sarus.tech','France','Startup','Data management'],
        ['Director of Category','Cinchy','Canada','Startup','Data management'],
        ['Founder','Daracoral','USA','Startup','Data management'],
        ['Product Manager ','Confluent','USA','Scale up','Data management'],
        ['Founder','Keboola','Czechia','Startup','Data management'],
        ['Founder','LakeFS','Israel','Startup','Data management'],
        ['Principal Scientist','Data.world','USA','Scale up','Data management'],
        ['Principal Data Scientist','Zapier','USA','Scale up','Data management'],
        ['Founder','DataHub (LinkedIn spin out)','USA','Startup','Data management'],
        ['CEO','Monte Carlo','Israel','Startup','Data management'],
        ['Solutions Architect','Solace','Canada','Startup','Data management'],
        ['Director Product','Medidata','UK','Small','Data management'],
        ['Product Manager','Lotics','UK','Small','Data Providers'],
        ['strategist','Oracle','USA','Large','Databases'],
        ['Group Product Manager','Tinder','USA','Scaleup','Dating'],
        ['Data Product Manager','Just Eat Takeaway','USA','Scale up','Delivery App'],
        ['Head of Data','Labelium','Spain','Small','Digital Agency'],
        ['Head of Data Engineering','Estante Magica','Brazil','Startup','EdTech'],
        ['Data Engineer','Cloud Academy','UK','Startup','EdTech'],
        ['Data Engineer','Amsterdam University','Netherlands','Large','Education'],
        ['IT architect','Alliander','Netherlands','Large','Energy'],
        ['Enterprise Architect','Schlumberger','USA','Large','Energy'],
        ['Senior Architect ','Schneider Electric','France','Large','Energy'],
        ['Head of ML Engineering ','Origami Energy','UK','Small','Energy'],
        ['Knowledge Engineer','Siemens Energy','Germany','Large','Energy'],
        ['CDO','Total','France','Large','Energy'],
        ['Solution Architect','ENI','Italk','Large','Energy'],
        ['Data Architect','HERA','Italy','Large','Energy'],
        ['Director','Arup','Netherlands','Large','Engineering'],
        ['Architect','Philips','Netherlands','Large','Engineering'],
        ['Consultant','TNG Consultancy','Germany','Medium','Engineering Consultancy'],
        ['data engineer','OPUS','Germany','Small','Fashion'],
        ['Data Engineer','Lookiero','USA','Startup','Fashion'],
        ['Director Data Engineering','Aritzia','Canada','Large','Fashion'],
        ['Data Engineer','Netflix','USA','Scale up','Film'],
        ['Enterprise Architect','Northern Trust ','USA','Large','Finance'],
        ['Head of Data Engineering','Kestra Financial','USA','Large','Finance'],
        ['Data Strategist','Al Rajhi Bank','Saudi','Large','Finance'],
        ['Senior DB Dev ','Fisher Investment','USA','Large','Finance'],
        ['Data Manager','Lowell Financial','UK','Large','Finance'],
        ['Head of Platform','Schroders Investment Management','UK','Large','Finance'],
        ['Data Architect','Nationwide ','UK','Large','Finance'],
        ['Enterprise Architect','TD Bank','Canada','Large','Finance'],
        ['Chief Data Officer','XP Inc','Brazil','Large','Finance'],
        ['Data Engineer','SEB','Sweden','Large ','Finance'],
        ['Enterprise Architect','Bank of New Zealand','New Zealand','Large','Finance'],
        ['Architect','UBS','UK','Large','Finance'],
        ['Head Data Integration','Saxo Bank','Netherlands','Large','Finance'],
        ['VP of Data ','SoFi','USA','Scale up','Fintech'],
        ['Data Engineer','B89','Peru','startup','FinTech'],
        ['Data Engineering Director','Kyriba','USA','Scale up','FinTech'],
        ['Data Architect','WEX','USA','Scale up','FinTech'],
        ['Principal Engineer','Intuit','USA','scaleup','FinTech'],
        ['Lead Data Engineer','Afterpay','Australia','Large','FinTech'],
        ['Data Engineer','Jumo.world','South Africa','Startup','Fintech'],
        ['CTO','RVU','UK','Startup','Fintech'],
        ['Enterprise Architect','Voya','USA','Scale up','Fintech'],
        ['Founder','DataDiligence','Switzerland','Startup','Fintech'],
        ['Senior Data Engineer','WorldRemit','UK','Scale up','Fintech'],
        ['Data Platform Lead','OLX Group','Germany','Large','Fintech'],
        ['VP Data Engineering','Pleo','Denmark','Startup','Fintech'],
        ['Data Engineer','B89','Peru','Startup','Fintech'],
        ['Senior Data Engineering','Peloton','USA','Scale up','Fitness'],
        ['engineering manager + data engineer','HelloFresh','Germany','Scale up','Food'],
        ['Head of master data','Nobia','Sweden','Medium','Furniture'],
        ['Data Product Lead','Norwegian public roads administration','Norway','Large','Government'],
        ['Head of Data Engineering','Norwegian Gov','Norway','Large','Government'],
        ['Principal Engineer','NAV','Norway','Large','Government'],
        ['Consultant','European Commission','EU','Large','Government'],
        ['Owner Data Analytics','City of Zurich','Switzerland','Large','Government'],
        ['Consultant Architect','Dutch Railroad','Netherlands','Large','Government'],
        ['Senior Solution Architect','CTTI','Catalonia','Large','Government'],
        ['Principal Software Engineer','AthenaHealth','USA','Large','Health'],
        ['Data Engineer','FindHotel','UK','Startup','Hotel Search'],
        ['Interactive Team','Workday','USA','Scale up','HR management software'],
        ['Head Data Strategy','Manulife ','Canada','Large','Insurance'],
        ['Head of Data','Uniqa','Germany','Large','Insurance'],
        ['Director of Data Engineering','Flexera','Northern Ireland','Large','IT Solutions'],
        ['CTO','TermScout','USA','Startup','LegalTech'],
        ['Tech Lead','ZenCargo','UK','Scale up','Logistics'],
        ['engineering manager ','Flexport','USA','Scale up','Logistics'],
        ['Lead Data Engineer','Maersk','Denmark','Large','Logistics'],
        ['Tech Lead','Reckitt','UK','Large','Manufacturer'],
        ['Head Data Engineering','Bose Corp','USA','Large','Manufacturing'],
        ['VP Platform','kenshoo.com','Israel','Medium','MarTech'],
        ['Data Architect','RTL','Netherlands','Large','Media'],
        ['Principal Architect','ITV','UK','Large ','Media'],
        ['Head of Tech','Signal-AI','UK','Scale up','Media'],
        ['Data Manager','DPG Media','Belgium','Large','Media'],
        ['Integration Architect','BHP','South Africa','Large','Mining '],
        ['Founder','Evidently AI','USA','Startup','MLoOps'],
        ['Marketing','Molecula','USA','Startup','MLOps'],
        ['Founder','Elementi','USA','Startup','MLOps'],
        ['Data Architect','Carfax','USA','Large','Motor '],
        ['Data Engineering Lead','Epidemic Sound','Sweden','Scale up','Music'],
        ['engineering manager','Rudderstack','USA','startup','OSS data pipeline'],
        ['Data Architect','CleverTech','Peru','Small','Outsource IT'],
        ['Data Engineer','SysMap Solutions','Brazil','Medium','Outsource IT'],
        ['Data Engineering Lead','Altimetrik','India','Scale up','Outsource IT'],
        ['Software Architect','Derivco','South Africa','Medium','Outsource IT'],
        ['architect','Roche Polska','Poland','Large','Pharma'],
        ['department of Research Informatics & Software Engineering','Genentech Research','USA','Large (part of Roche)','Pharma'],
        ['Professional Cloud Architect','Roche','USA','Large','Pharma'],
        ['Tech Lead Data Engineering','Prezi','Hungary','Scale up','Presentation Software'],
        ['Chief Architect','Zillow','USA','Scale up','Property Tech'],
        ['Data Infra','Airbnb','USA','Scale up','Property Tech'],
        ['Data Engineer ','Springer Nature','Germany','Large','Publishing'],
        ['Lead Data Citizenship','SEEK','Australia','Medium','Recruitment'],
        ['Director of Data Mesh','Gloo','USA','Startup','Religion'],
        ['CEO','Psyda','Pakistan','Startup','Research'],
        ['Architect','G Research','UK','Small','Research'],
        ['Research Director','45 Research','USA','Medium','Research'],
        ['lead Infrastructure','Instacart','USA','Scale up','Retail'],
        ['Data Engineer','IKEA','Sweden','Large','Retail'],
        ['Data Modelers x 2','Breuninger','Germany','Large','Retail'],
        ['Data Team Leader','AO.com','UK','Medium','Retail'],
        ['Head of Data Science','Franprix','France','Large','Retail'],
        ['leading the data team','Starship Technologies','USA','Scale up','Robotics'],
        ['software engineer','Abnormal Security','USA','Scale up','Security'],
        ['Chief Data Officer','Avast','Czechia','Scale up','Security'],
        ['Pre Sales Engineering','Orange Business Services','France','Large','Telecoms'],
        ['Cyber Analytics','TELUS','Canada','Large','Telecoms'],
        ['Product Owner','BT','UK','Large','Telecoms'],
        ['Lead Data Engineer','Chantelle','France','Medium','Textiles'],
        ['Director Data Science ','Booking.com','Netherlands','Large','Travel']
    ]).

:- use_module(core('document/patch')).
test(my_spreadsheet_diff_area_max, []) :-
    spreadsheet1(T1),
    spreadsheet2(T2),

    table_diff_area_max(T1,T2,Diff),

    Diff = _{'@op':"ModifyTable",
             dimensions:_{'@after':[5,187],
                          '@before':[5,187]},
             copies:Copies,
             deletes:_Deletes,
             inserts:_Inserts,
             moves:[]},

    % Depends on solution order!
    Copies = [ json{ '@at':json{'@height':182,'@width':5,'@x':0,'@y':0},
                     '@value':_
                   }
               | _],

    simple_patch(Diff,T1,T2).

test(my_spreadsheet_first_col_sorted_windows, [blocked(slow)]) :-
    spreadsheet1([H|T1]),
    sort(T1,TS1),
    heuristic_windows([H|T1],[H|TS1],E1,E2),
    length(E1,187),
    length(E2,187).


test(my_spreadsheet_first_col_sorted_col_swapped_windows, [blocked(slow)]) :-
    spreadsheet1(SS),
    swap_columns(1,2,SS,SS2),
    SS2 = [H|T1],
    sort(T1,TS1),
    heuristic_windows([H|T1],[H|TS1],E1,E2),
    length(E1,187),
    length(E2,187).

:- end_tests(table_diff).

/*
 *  random(12,12,M),
 */
