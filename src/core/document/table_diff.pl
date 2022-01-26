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

disjoint_rectangles(Rs) :-
    must_be(list, Rs),
    non_overlapping(Rs).

non_overlapping([]).
non_overlapping([R|Rs]) :-
        maplist(non_overlapping_(R), Rs),
        non_overlapping(Rs).

not_overlapping_with(R,Rs) :-
    maplist(non_overlapping_(R), Rs).

non_overlapping_(A, B) :-
        a_not_in_b(A, B),
        a_not_in_b(B, A).

a_not_in_b(r(_,AX,AW,AY,AH), r(_,BX,BW,BY,BH)) :-
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
    not_overlapping_with(R,Exclusions).

window_values_equal(r(X,_,_,_,_),r(Y,_,_,_,_)) :-
    compare(Order,X,Y),
    Order = (=).

window_values_less(r(X,_,_,_,_),r(Y,_,_,_,_)) :-
    X @< Y.

extend_exclusions(_, [], Left_Exclusions0, Right_Exclusions0, Left_Exclusions0, Right_Exclusions0).
extend_exclusions([], _, Left_Exclusions0, Right_Exclusions0, Left_Exclusions0, Right_Exclusions0).
extend_exclusions([W1|Windows1],
                  [W2|Windows2],
                  Left_Exclusions0, Right_Exclusions0,
                  Left_Exclusions1, Right_Exclusions1) :-
    window_values_equal(W1,W2),
    !,
    (   not_overlapping_with(W1,Left_Exclusions0),
        not_overlapping_with(W2,Right_Exclusions0)
    ->  extend_exclusions(Windows1,Windows2,
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

% test_area(4,[4-[2-2,1-4,4-1]], M1, M2, [ ... ], [ ... ], Left_Exc, Right_Exc)
test_area(Area,Areas,M1,M2,
          Left_Exclusions0,Right_Exclusions0,
          Left_ExclusionsN,Right_ExclusionsN) :-
    memberchk(Area-WHs,Areas),

    foldl(
        {M1,M2}/[W-H,
                 Left_Exclusions_In-Right_Exclusions_In,
                 Left_Exclusions_Out-Right_Exclusions_Out]>>(
            test_shape(W,H,M1,M2,
                       Left_Exclusions_In,Right_Exclusions_In,
                       Left_Exclusions_Out,Right_Exclusions_Out)
            ),
        WHs,
        Left_Exclusions0-Right_Exclusions0,
        Left_ExclusionsN-Right_ExclusionsN),
    !,
    \+ Left_Exclusions0 = Left_ExclusionsN,
    \+ Right_Exclusions0 = Right_ExclusionsN.


test_shape(W,H,M1,M2,
           Left_Exclusions0,Right_Exclusions0,
           Left_ExclusionsN,Right_ExclusionsN) :-
    matrix_size(M1,R1,C1),
    matrix_size(M2,R2,C2),

    findall(r(Window1,X1,W,Y1,H),
            (   windows(W,H,R1,C1,Left_Exclusions0,X1,Y1),
                label([X1,Y1]),
                matrix_window(M1,X1,Y1,W,H,Window1)
            ),
            Windows1),
    sort(0,@=<,Windows1,Windows1_Sorted),

    findall(r(Window2,X2,W,Y2,H),
            (   windows(W,H,R2,C2,Right_Exclusions0,X2,Y2),
                label([X2,Y2]),
                matrix_window(M2,X2,Y2,W,H,Window2)
            ),
            Windows2),
    sort(0,@=<,Windows2,Windows2_Sorted),

    %% time: 17.364s for random_exclusions_10x10
    %% extend_exclusions(Windows1_Sorted,Windows2_Sorted,
    %%                   Left_Exclusions0, Right_Exclusions0,
    %%                   Left_Exclusions1, Right_Exclusions1),
    %% !,
    %% \+ Left_Exclusions0 = Left_Exclusions1,
    %% \+ Right_Exclusions0 = Right_Exclusions1,

    %% Left_Exclusions1 = Left_ExclusionsN,
    %% Right_Exclusions1 = Right_ExclusionsN.

    % time: 12.963s for random_exclusions_10x10
    extend_exclusions(Windows1_Sorted,Windows2_Sorted,
                      [],[],
                      Left_Exclusions1,Right_Exclusions1),
    !,
    append(Left_Exclusions0,Left_Exclusions1,Left_ExclusionsN),
    append(Right_Exclusions0,Right_Exclusions1,Right_ExclusionsN).


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
    (   next_area(A1,Areas,A2)
    ->  collect_exclusions(A2,T1,T2,Areas,LE1,RE1,LEN,REN)
    ;   LE1=LEN, RE1=REN
    ).
collect_exclusions(_,_,_,_,LE,RE,LE,RE).

all_exclusions(M1,M2,Left_Exclusions,Right_Exclusions) :-
    as_matrix(M1,T1),
    as_matrix(M2,T2),
    matrix_size(T1,Length1,Height1),
    matrix_size(T2,Length2,Height2),
    Window_Length is min(Length1,Length2),
    Window_Height is min(Height1,Height2),
    Min_Area = 2,
    areas(Window_Length, Window_Height, Min_Area, Areas),
    Areas = [Area-_|_],
    collect_exclusions(Area,T1,T2,Areas,[],[],
                       Left_Exclusions,
                       Right_Exclusions).

heuristic_exclusions(M1,M2,Left_Exclusions,Right_Exclusions) :-
    as_matrix(M1,T1),
    as_matrix(M2,T2),
    matrix_size(T1,Length1,Height1),
    matrix_size(T2,Length2,Height2),
    Window_Length is min(Length1,Length2),
    Window_Height is min(Height1,Height2),
    Min_Area = 2,
    Everything is Window_Length * Window_Height,
    Everything_Areas = [Everything-[Window_Length,Window_Height]],
    collect_exclusions(Everything,T1,T2,Everything_Areas,
                       [],[],
                       Left_Exclusions0,Right_Exclusions0),
    format(user_error, '~nTried everything~n', []),
    areas(Window_Length, 1, Min_Area, Areas),
    Areas = [Row_Area-IJs|Rest_Areas],
    Full_Row = [Row_Area-IJs], % try full rows first

    collect_exclusions(Row_Area,T1,T2,Full_Row,
                       Left_Exclusions0,Right_Exclusions0,
                       Left_Exclusions1,Right_Exclusions1),

    format(user_error, '~nTried rows~n', []),
    (   Rest_Areas = [Area-_|_]
    ->  collect_exclusions(Area,T1,T2,Rest_Areas,
                           Left_Exclusions1,Right_Exclusions1,
                           Left_Exclusions,Right_Exclusions),
        format(user_error, '~nTried sub-rows~n', [])
    ;   Left_Exclusions1 = Left_Exclusions,
        Right_Exclusions1 = Right_Exclusions
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
    windows(1,3,N,M,[r(bob,2,1,1,1)],I,J),
    findall(I-J,label([I,J]), IJs),
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
    windows(2,2,N,M,[r(bob,2,2,1,1),r(jim,0,1,2,1)],I,J),
    findall(I-J,label([I,J]), IJs),
    IJs = [0-0,1-2,2-2].

test(some_exclusions, []) :-
    T1 = [ [ a , b ] ],

    T2 = [ [ a , b ] ],

    all_exclusions(T1,T2,E1,E2),
    E1 = [r(_,0,2,0,1)],
    E2 = [r(_,0,2,0,1)].

test(all_exclusions, []) :-
    T1 = [ [ a, x, y, z, a],
           [ b, '1', '2', '3', b],
           [ c, '4', '5', '6', c],
           [ d, '7', '8', '9', d],
           [ a, a, a, a, a] ],

    T2 = [ [ '1', '2', '3'],
           [ '4', '5', '6'],
           [ '7', '8', '9'] ],

    all_exclusions(T1,T2,E1,E2),

    E1 = [r(_,1,3,1,3)],
    E2 = [r(_,0,3,0,3)].

% can we make 10x1000 take a second?
test(random_exclusions_10x10, []) :-
    T1 = [['7','4','9','5','6','0','0','7','5','2'],
          ['4','7','9','6','5','1','3','6','6','3'],
          ['6','2','2','5','0','9','9','4','5','6'],
          ['7','6','0','1','9','8','3','4','9','2'],
          ['7','3','6','2','7','0','7','5','5','7'],
          ['5','2','4','9','9','1','9','8','6','0'],
          ['7','2','5','6','9','6','0','8','3','7'],
          ['9','4','5','7','8','6','4','9','1','8'],
          ['7','7','1','4','3','7','0','7','4','2'],
          ['8','9','5','5','0','4','2','4','8','2']],

    T2 = [['2','6','9','2','8','6','7','9','5','5'],
          ['9','4','3','6','8','9','3','4','6','0'],
          ['6','1','7','0','5','1','6','9','4','4'],
          ['0','0','2','4','5','2','3','5','4','2'],
          ['7','9','6','2','8','5','9','5','3','1'],
          ['0','5','9','9','1','8','9','6','0','8'],
          ['6','7','0','3','1','6','0','1','3','0'],
          ['7','7','4','6','4','5','7','2','4','7'],
          ['9','8','8','6','1','7','9','3','2','6'],
          ['2','1','4','6','1','6','8','4','7','2']],
    time(ignore(all_exclusions(T1,T2,E1,E2))),
    length(E1,25),
    length(E2,25).

test(random_exclusions_12x12, []) :-
    T1 = [['9','1','6','4','8','1','0','9','8','9','2','4'],
          ['4','8','9','4','2','2','5','7','1','2','5','4'],
          ['1','0','7','7','0','8','2','6','8','6','9','4'],
          ['5','9','2','9','0','4','8','1','8','0','0','7'],
          ['6','5','8','4','2','6','5','3','2','2','8','4'],
          ['7','9','6','9','6','3','3','4','6','8','3','7'],
          ['7','2','1','7','4','4','8','0','4','3','9','7'],
          ['8','9','3','5','6','9','6','9','0','0','7','2'],
          ['0','7','0','0','6','8','2','2','0','9','1','6'],
          ['7','0','9','7','5','6','7','8','4','3','7','5'],
          ['1','5','9','6','7','8','3','6','2','4','0','0'],
          ['0','3','6','8','8','8','0','0','1','8','1','9']],

    T2 = [['5','2','0','4','0','3','0','6','2','7','3','1'],
          ['4','4','0','1','4','0','6','0','9','2','7','7'],
          ['6','9','0','5','6','5','1','2','0','5','5','4'],
          ['3','9','6','5','1','4','6','1','0','9','7','3'],
          ['8','8','5','5','7','8','4','5','1','3','8','8'],
          ['1','8','2','5','4','9','6','6','2','7','4','3'],
          ['2','7','7','6','4','9','8','3','9','2','6','9'],
          ['0','8','4','8','2','3','5','4','4','2','7','7'],
          ['3','1','8','9','7','4','3','2','2','0','9','6'],
          ['5','4','7','9','6','7','8','4','4','3','4','5'],
          ['4','8','0','2','7','9','5','3','8','9','2','7'],
          ['4','7','6','4','5','2','7','0','6','5','6','4']],

    all_exclusions(T1,T2,E1,E2),
    length(E1,36),
    length(E2,36).

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

test(my_spreadsheet, []) :-
    spreadsheet1(T1),
    spreadsheet2(T2),

    all_exclusions(T1,T2,E1,E2),

    E1 = [r(_,0,5,0,182),r(_,0,4,184,3),r(_,0,3,182,2),r(_,4,1,184,3),r(_,3,1,182,2)],
    E2 = [r(_,0,5,0,182),r(_,0,4,184,3),r(_,0,3,182,2),r(_,4,1,184,3),r(_,3,1,182,2)].

test(my_spreadsheet_first_col_sorted, []) :-
    spreadsheet1([H|T1]),
    sort(T1,TS1),
    profile(heuristic_exclusions([H|T1],[H|TS1],E1,E2)),
    length(E1,L1),
    length(E2,L2),
    writeq([L1,L2]).


:- end_tests(table_diff).

/*
 *  random(12,12,M),
 */
