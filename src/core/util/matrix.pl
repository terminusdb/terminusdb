:- module(matrix, [
              as_matrix/2,
              matrix_row/3,
              matrix_col/3,
              as_list_of_lists/2,
              matrix_window/6,
              matrix_size/3,
              window_offset/3
          ]).

:- export('$matrix':window_matrix/2).

as_matrix(List, Matrix) :-
    '$matrix':as_matrix(List, Matrix).

as_list_of_lists(Matrix, List) :-
    '$matrix':as_list_of_lists(Matrix, List).

matrix_row(Matrix, Col_Num, Col) :-
    blob(Matrix, matrix),
    !,
    '$matrix':matrix_row(Matrix, Col_Num, Col).
matrix_row(Window, Col_Num, Col) :-
    blob(Window, window),
    !,
    '$matrix':window_row(Window, Col_Num, Col).


matrix_col(Matrix, Col_Num, Col) :-
    blob(Matrix, matrix),
    !,
    '$matrix':matrix_col(Matrix, Col_Num, Col).
matrix_col(Window, Col_Num, Col) :-
    blob(Window, window),
    !,
    '$matrix':window_col(Window, Col_Num, Col).
matrix_col(Unknown, _, _) :-
    throw(error(type_error(one_of(matrix, window), Unknown), _)).

matrix_window(Matrix, X, Y, Width, Height, Window) :-
    blob(Matrix, matrix),
    !,
    '$matrix':matrix_window(Matrix, X, Y, Width, Height, Window).
matrix_window(Window, X, Y, Width, Height, New_Window) :-
    blob(Window, window),
    !,
    '$matrix':window_window(Window, X, Y, Width, Height, New_Window).
matrix_window(Unknown, _, _, _, _, _) :-
    throw(error(type_error(one_of(matrix, window), Unknown), _)).

matrix_size(M, W, H) :-
    '$matrix':matrix_size(M, W, H).

window_offset(W, X, Y) :-
    '$matrix':window_offset(W, X, Y).
