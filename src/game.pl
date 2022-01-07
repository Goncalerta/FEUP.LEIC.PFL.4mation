% initial_state(+Config, -GameState)
% Size is size(X, Y)
initial_state(
    config(
        size(Cols, Rows), %
        player_x(Px), % TODO
        player_o(Py),  % TODO
        first_player(First), %
        winning_length(Goal) % TODO
    ), game_state(Board, current_player(First), last_piece_played(none))
) :-
    initial_board(Cols, Rows, Board).




% move(+GameState, +Move, -NewGameState)
move(
    game_state(Board, current_player(Player), last_piece_played(_)), 
    Move, 
    game_state(BoardNext, current_player(PlayerNext), last_piece_played(PieceNext))
) :-
    move_board(Board, Move, Player, BoardNext),
    next_player(Player, PlayerNext),
    PieceNext = Move.

% next_player(+Player, -NextPlayer)
% Returns the next player after a move.
next_player(player_x, player_o).
next_player(player_o, player_x).

% move_board(+Board, +Move, +Piece, -BoardNext)
% Piece is one of [player_x, player_o]
move_board([[_|Rest] | RowRest], position(0, 0), Piece, [[CellNext|RestNext] | RowRestNext]) :-
    CellNext = Piece,
    Rest = RestNext,
    RowRest = RowRestNext.
move_board([[Cell|Rest]], position(X, 0), Piece, [[CellNext|RestNext]]) :-
    X > 0,
    Cell = CellNext,
    X1 is X - 1,
    move_board([Rest], position(X1, 0), Piece, [RestNext]).
move_board([Row|Rest], position(X, 0), Piece, [RowNext|RestNext]) :-
    X > 0,
    Rest \== [],
    RestNext \== [],
    move_board([Row], position(X, 0), Piece, [RowNext]),
    Rest = RestNext.
move_board([Row|Rest], position(X, Y), Piece, [RowNext|RestNext]) :-
    X >= 0,
    Y > 0,
    Row = RowNext,
    Y1 is Y - 1,
    move_board(Rest, position(X, Y1), Piece, RestNext).
 
% game_over(+GameState, -Winner)
% Winner is one of [player_x, player_o, none]
game_over(GameState, none) :-
    valid_moves(GameState, []).
game_over(game_state(Board, current_player(_), last_piece_played(_)), Winner) :-
    wins_game(Board, Winner).

wins_game(Board, Winner) :-
    wins_game(Board, Winner, 4).
wins_game(_, _, 0).
wins_game(Board, Winner, PiecesToWin) :-
    nth00(I, J, Board, Winner),
    PiecesToWin1 is PiecesToWin - 1,
    I1 is I + 1,
    wins_game_horizontally(Board, I1, J, Winner, PiecesToWin1).
wins_game(Board, Winner, PiecesToWin) :-
    nth00(I, J, Board, Winner),
    PiecesToWin1 is PiecesToWin - 1,
    J1 is J + 1,
    wins_game_vertically(Board, I, J1, Winner, PiecesToWin1).
wins_game(Board, Winner, PiecesToWin) :-
    nth00(I, J, Board, Winner),
    PiecesToWin1 is PiecesToWin - 1,
    I1 is I + 1,
    J1 is J + 1,
    wins_game_diagonally_down(Board, I1, J1, Winner, PiecesToWin1).
wins_game(Board, Winner, PiecesToWin) :-
    nth00(I, J, Board, Winner),
    PiecesToWin1 is PiecesToWin - 1,
    I1 is I + 1,
    J1 is J - 1,
    wins_game_diagonally_up(Board, I1, J1, Winner, PiecesToWin1).

wins_game_horizontally(_, _, _, _, 0).
wins_game_horizontally(Board, I, J, Winner, PiecesToWin) :-
    nth00(I, J, Board, Winner),
    PiecesToWin1 is PiecesToWin - 1,
    I1 is I + 1,
    wins_game_horizontally(Board, I1, J, Winner, PiecesToWin1).

wins_game_vertically(_, _, _, _, 0).
wins_game_vertically(Board, I, J, Winner, PiecesToWin) :-
    nth00(I, J, Board, Winner),
    PiecesToWin1 is PiecesToWin - 1,
    J1 is J + 1,
    wins_game_vertically(Board, I, J1, Winner, PiecesToWin1).

wins_game_diagonally_down(_, _, _, _, 0).
wins_game_diagonally_down(Board, I, J, Winner, PiecesToWin) :-
    nth00(I, J, Board, Winner),
    PiecesToWin1 is PiecesToWin - 1,
    I1 is I + 1,
    J1 is J + 1,
    wins_game_diagonally_down(Board, I1, J1, Winner, PiecesToWin1).

wins_game_diagonally_up(_, _, _, _, 0).
wins_game_diagonally_up(Board, I, J, Winner, PiecesToWin) :-
    nth00(I, J, Board, Winner),
    PiecesToWin1 is PiecesToWin - 1,
    I1 is I + 1,
    J1 is J - 1,
    wins_game_diagonally_up(Board, I1, J1, Winner, PiecesToWin1).

% valid_moves(+GameState, -ListOfMoves)
valid_moves(game_state(Board, current_player(_), last_piece_played(LastMove)), ListOfMoves) :-
    findall(Move, valid_move(Board, LastMove, Move), ListOfMoves).

valid_move(Board, none, position(Cols, Rows)) :-
    empty_cell(Board, position(Cols, Rows)).

valid_move(Board, position(LastX, LastY), position(MoveX, MoveY)) :-
    empty_cell(Board, position(MoveX, MoveY)),
    DistX is (MoveX - LastX),
    DistY is (MoveY - LastY),
    between(-1, 1, DistX),
    between(-1, 1, DistY).

% value(+GameState, +Player, -Value)

% choose_move(+GameState, +Level, -Move)
choose_move(GameState, 1, Move) :-
    valid_moves(GameState, ListOfMoves),
    random_member(Move, ListOfMoves).

