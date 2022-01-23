% nth00(+Column, Row, +Board, -Cell)
% Returns the cell at the given column and row.
nth00(I, J, Board, Cell) :-
    nth0(J, Board, Row),
    nth0(I, Row, Cell).

% board_size(+Board -Size)
% Returns the size of the board.
board_size(Board, size(Cols, Rows)) :-
    length(Board, Rows),
    nth0(0, Board, Row),
    length(Row, Cols).

% inside_board(+Board, +Position)
% True if position is inside the board.
inside_board(Board, position(X, Y)) :-
    board_size(Board, size(Cols, Rows)),
    X >= 0,
    X < Cols,
    Y >= 0,
    Y < Rows.

% empty_cell(+Board, +Position)
% True if position is empty.
empty_cell(Board, position(X, Y)) :-
    nth00(X, Y, Board, empty).

% initial_board(+Cols, +Rows, -Board)
% Creates a board with the given number of Rows and Cols.
initial_board(1, 1, [[empty]]). % 1x1 board
initial_board(Cols, 1, [[empty|Rest]]) :- % 1xCols board
    Cols > 1,
    Cols1 is Cols - 1,
    initial_board(Cols1, 1, [Rest]). 
initial_board(Cols, Rows, [Row|Rest]) :- % ColsxRows board
    Rows > 1,
    Rows1 is Rows - 1,
    initial_board(Cols, 1, [Row]), % create a row
    initial_board(Cols, Rows1, Rest). % create the rest of the board
