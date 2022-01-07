nth00(I, J, Board, Cell) :-
    nth0(I, Board, Row),
    nth0(J, Row, Cell).
% Assumes that the board is valid (rectangular)
board_size(Board, size(Cols, Rows)) :-
    length(Board, Rows),
    nth0(0, Board, Row),
    length(Row, Cols).
inside_board(Board, position(X, Y)) :-
    board_size(Board, size(Cols, Rows)),
    X >= 0,
    X < Cols,
    Y >= 0,
    Y < Rows.
empty_cell(Board, position(X, Y)) :-
    nth00(X, Y, Board, empty).

% initial_board(+Cols, +Rows, -Board)
% Creates a board with the given number of Rows and Cols
initial_board(1, 1, [[empty]]).
initial_board(Cols, 1, [[empty|Rest]]) :-
    Cols > 1,
    Cols1 is Cols - 1,
    initial_board(Cols1, 1, [Rest]).
initial_board(Cols, Rows, [Row|Rest]) :-
    Rows > 1,
    Rows1 is Rows - 1,
    initial_board(Cols, 1, [Row]),
    initial_board(Cols, Rows1, Rest).
