:- use_module(library(apply)).

% GameState(Board, CurrentPlayer, LastPiecePlayed)
% board([[Piece]])               Piece is one of [x, o, empty]
% current_player(Player)         Player is one of [x, o]
% last_piece_played(Piece)       Piece is one of [(X, Y), none]




% play/0

% display_game(+GameState)

% initial_state(+Size, -GameState)
% Size is size(X, Y)
initial_state(size(Cols, Rows), game_state(board(Board), current_player(1), last_piece_played(none))) :-
    initial_board(Cols, Rows, Board).

% initial_board(+Cols, +Rows, -Board)
% Creates a board with the given number of Rows and Cols
initial_board(Cols, 1, [Row]) :- initial_board_row(Cols, Row).
initial_board(Cols, Rows, [Row|Rest]) :-
    Rows > 1,
    Rows1 is Rows - 1,
    initial_board_row(Cols, Row),
    initial_board(Cols, Rows1, Rest).

initial_board_row(1, [empty]).
initial_board_row(Cols, [empty|Rest]) :-
    Cols > 1,
    Cols1 is Cols - 1,
    initial_board_row(Cols1, Rest).

% lenght_all(+Lists, +Length)
% Checks whether all lists in the list of lists have the given length
length_all([], _).
length_all([List|Rest], Cols) :-
    length(List, Cols),
    length_all(Rest, Cols).

% initial_board(1, 1, [[empty]]).
% initial_board(Cols, 1, [[empty|Rest]]) :-
%     Cols > 0,
%     Cols1 is Cols - 1,
%     initial_board(Cols1, 1, [Rest]).
% initial_board(Cols, Rows, [Row|Rest]) :-
%     Cols > 0,
%     Rows > 0,
%     Rows1 is Rows - 1,
%     initial_board(Cols1, Rows1, [Rest]).



% move(+GameState, +Move, -NewGameState)

% game_over(+GameState, -Winner)

% valid_moves(+GameState, -ListOfMoves)

% value(+GameState, +Player, -Value)

% choose_move(+GameState, +Level, -Move)

