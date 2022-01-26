:- module('document/table_diff',
          [
              %table_diff/4
          ]).

:- use_module(core(util)).
:- use_module(library(clpfd)).

/* Fast(er) table diff */

down_from(From,To,X) :-
    between(To,From,Y),
    X is From - Y + 1.

row_length(T, Length) :-
    length(T, Length).

column_length([], 0).
column_length([R|_], Length) :-
    length(R, Length).

split_matrix(In, N, M, Top_Left, Right, Bottom, Bottom_Right) :-
    when((   ground(In)
         ;   ground(Rows_Top),
             ground(Rows_Bottom)),
         split_row(In, N, Rows_Top, Rows_Bottom)),
    split_column(Rows_Top, M, Top_Left, Right),
    split_column(Rows_Bottom, M, Bottom, Bottom_Right).

split(Index,List,Left,Right) :-
    length(Left, Index),
    append(Left,Right,List).

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

disjoint_rectangles(Rs) :-
        must_be(list, Rs),
        non_overlapping(Rs).

non_overlapping([]).
non_overlapping([R|Rs]) :-
        maplist(non_overlapping_(R), Rs),
        non_overlapping(Rs).

non_overlapping_(A, B) :-
        a_apart_from_b(A, B),
        a_apart_from_b(B, A).

a_apart_from_b(r(_,AX,AW,AY,AH), r(_,BX,BW,BY,BH)) :-
        ?(AX) #=< ?(BX) #/\ ?(BX) #< ?(AX) + ?(AW) #==>
                   ?(AY) + ?(AH) #=< ?(BY) #\/ ?(BY) + ?(BH) #=< ?(AY),
        ?(AY) #=< ?(BY) #/\ ?(BY) #< ?(AY) + ?(AH) #==>
                   ?(AX) + ?(AW) #=< ?(BX) #\/ ?(BX) + ?(BW) #=< ?(AX).

column_slice(_,0,_,[]) :- !.
column_slice(0,H,[Elt|L1],[Elt|L2]) :-
    !,
    Hp is H - 1,
    column_slice(0,Hp,L1,L2).
column_slice(Y,H,[_|L1],L2) :-
    Yp is Y - 1,
    column_slice(Yp,H,L1,L2).

window(_,0,_,_,_,[]) :- !.
window(0,W,Y,H,[R1|M1],[R2|M2]) :-
    !,
    column_slice(Y,H,R1,R2),
    Wp is W - 1,
    window(0,Wp,Y,H,M1,M2).
window(X,W,Y,H,[_|M1],M2) :-
    Xp is X - 1,
    window(Xp,W,Y,H,M1,M2).

windows(Width,Height,N,M,Exclusions,I,J) :-
    Right #= N - Width,
    Bottom #= M - Height,
    I in 0 .. Right,
    J in 0 .. Bottom,
    R = r(_,I,Width,J,Height),
    disjoint_rectangles([R|Exclusions]).

window_values_equal(r(X,_,_,_,_),r(X,_,_,_,_)).

window_values_less(r(X,_,_,_,_),r(Y,_,_,_,_)) :-
    X @< Y.

extend_exclusions(_, [],Left_Exclusions0, Right_Exclusions0, Left_Exclusions0, Right_Exclusions0).
extend_exclusions([], _, Left_Exclusions0, Right_Exclusions0, Left_Exclusions0, Right_Exclusions0).
extend_exclusions([W1|Windows1],
                  [W2|Windows2],
                  Left_Exclusions0, Right_Exclusions0,
                  Left_Exclusions1, Right_Exclusions1) :-
    (   window_values_equal(W1,W2)
    ->  disjoint_rectangles([W1|Left_Exclusions0]),
        disjoint_rectangles([W2|Right_Exclusions0]),
        extend_exclusions(Windows1,Windows2,
                          [W1|Left_Exclusions0],[W2|Right_Exclusions0],
                          Left_Exclusions1,Right_Exclusions1)
    ;   extend_exclusions(Windows1,Windows2,
                          Left_Exclusions0,Right_Exclusions0,
                          Left_Exclusions1,Right_Exclusions1)
    ).
extend_exclusions([W1|Windows1],
                  [W2|Windows2],
                  Left_Exclusions0, Right_Exclusions0,
                  Left_Exclusions1, Right_Exclusions1) :-
    window_values_less(W1,W2),
    !,
    extend_exclusions(Windows1,
                      [W2|Windows2],
                      Left_Exclusions0,Right_Exclusions0,
                      Left_Exclusions1,Right_Exclusions1).
extend_exclusions([W1|Windows1],
                  [_|Windows2],
                  Left_Exclusions0, Right_Exclusions0,
                  Left_Exclusions1, Right_Exclusions1) :-
    extend_exclusions([W1|Windows1],
                      Windows2,
                      Left_Exclusions0,Right_Exclusions0,
                      Left_Exclusions1,Right_Exclusions1).

test_area(Area,Areas,M1,M2,
          Left_Exclusions0,Right_Exclusions0,
          Left_Exclusions1,Right_Exclusions1) :-
    memberchk(Area-WHs,Areas),
    row_length(M1,R1),
    column_length(M1,C1),

    row_length(M2,R2),
    column_length(M2,C2),

    findall(r(Window1,X1,W,Y1,H),
            (   member(W-H,WHs),
                windows(W,H,R1,C1,Left_Exclusions0,X1,Y1),
                label([X1,Y1]),
                window(X1,W,Y1,H,M1,Window1)
            ),
            Windows1),
    sort(0,@=<,Windows1,Windows1_Sorted),

    findall(r(Window2,X2,W,Y2,H),
            (   member(W-H,WHs),
                windows(W,H,R2,C2,Left_Exclusions0,X2,Y2),
                label([X2,Y2]),
                window(X2,W,Y2,H,M2,Window2)
            ),
            Windows2),
    sort(0,@=<,Windows2,Windows2_Sorted),

    extend_exclusions(Windows1_Sorted,Windows2_Sorted,
                      Left_Exclusions0,Right_Exclusions0,
                      Left_Exclusions1,Right_Exclusions1),

    \+ Left_Exclusions0 = Left_Exclusions1,
    \+ Right_Exclusions0 = Right_Exclusions1.

previous_area(This,[Last-_,This-_|_],Last) :-
    !.
previous_area(This,[_-_,Not_This-_|Rest],Last) :-
    previous_area(This, [Not_This-_|Rest],Last).

next_area(This,[This-_,Next-_|_],Next) :-
    !.
next_area(This,[_-_|Rest],Next) :-
    next_area(This, Rest,Next).

area_search(Left,Left,_,_,_,
            _,
            _, _,
            _, _) :-
    fail.
area_search(Left,Right,M1,M2,Areas,
            Area,
            Left_Exclusions0,Right_Exclusions0,
            Left_ExclusionsN,Right_ExclusionsN) :-
    Mid is (Left + Right) div 2,
    nth0(Mid, Areas, Test_Area-_),
    (   test_area(Test_Area,Areas,M1,M2,
                  Left_Exclusions0,Right_Exclusions0,
                  Left_Exclusions1,Right_Exclusions1)
    ->  (   \+ (Mid_minus_1 is Mid - 1,
                nth0(Mid_minus_1,Areas,Previous-_),
                test_area(Previous,Areas,M1,M2,
                          Left_Exclusions0,Right_Exclusions0,
                          _,_))
        ->  Test_Area = Area,
            Left_Exclusions1 = Left_ExclusionsN,
            Right_Exclusions1 = Right_ExclusionsN
        ;   New_Right is Mid - 1,
            area_search(Left,New_Right,M1,M2,Areas,Area,
                        Left_Exclusions0,Right_Exclusions0,
                        Left_ExclusionsN,Right_ExclusionsN)
        )
    ;   New_Left is Mid + 1,
        area_search(New_Left,Right,M1,M2,Areas,Area,
                    Left_Exclusions0,Right_Exclusions0,
                    Left_ExclusionsN,Right_ExclusionsN)
    ).

collect_exclusions(A0,T1,T2,Areas,LE0,RE0,LEN,REN) :-
    nth0(L,Areas,A0-_),
    length(Areas,R),
    format(user_error,'.',[]),
    area_search(L,R,T1,T2,Areas,A1,LE0,RE0,LE1,RE1),
    !,
    next_area(A1,Areas,A2),
    collect_exclusions(A2,T1,T2,Areas,LE1,RE1,LEN,REN).
collect_exclusions(_,_,_,_,LE,RE,LE,RE).

all_exclusions(T1,T2,Left_Exclusions,Right_Exclusions) :-
    row_length(T1,Length1),
    column_length(T1,Height1),
    row_length(T2,Length2),
    column_length(T2,Height2),
    Window_Length is min(Length1,Length2),
    Window_Height is min(Height1,Height2),
    areas(Window_Length, Window_Height, Areas),
    Areas = [Area-_|_],
    collect_exclusions(Area,T1,T2,Areas,[],[],
                       Left_Exclusions,
                       Right_Exclusions).


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

:- begin_tests(table_diff).

test(all_exclusions, []) :-
    T1 = [ [ a, x, y, z, a],
           [ b, 1, 2, 3, b],
           [ c, 4, 5, 6, c],
           [ d, 7, 8, 9, d],
           [ a, a, a, a, a] ],

    T2 = [ [ 1, 2, 3],
           [ 4, 5, 6],
           [ 7, 8, 9] ],

    all_exclusions(T1,T2,E1,E2),
    nl,
    writeq(E1),nl,
    writeq(E2),nl.

test(random_exclusions_4x4, []) :-
    random_matrix(4,4, T1),
    random_matrix(4,4, T2),
    profile(ignore(all_exclusions(T1,T2,E1,E2))),
    nl,
    writeq(E1),nl,
    writeq(E2),nl.

test(random_exclusions, []) :-
    random_matrix(30,30, T1),
    random_matrix(20,10, T2),
    time(all_exclusions(T1,T2,E1,E2)),
    nl,
    writeq(E1),nl,
    writeq(E2),nl.

:- end_tests(table_diff).
