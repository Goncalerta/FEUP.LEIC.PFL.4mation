% initial_state(+Config, -GameState)
% Creates the initial game state from the given configuration.
initial_state(
    config(
        size(Cols, Rows), % size of the board
        Players, % players(Px, Po), where Px and Po may each be human, bot(1) or bot(2)
        first_player(First), % player to make the first move
        win_target(Goal) % number of consecutive pieces to win
    ), 
    game_state(
        Board, % list of lists of cells representing the board, where each cell may be empty, player_x or player_o
        current_player(First), % player to make the next move
        last_move(none), % last move made (none if no move has been made yet)
        Players, % players(Px, Po), where Px and Po may each be human, bot(1) or bot(2)
        win_target(Goal) % number of consecutive pieces to win
    )
) :-
    initial_board(Cols, Rows, Board).

% player_info(+Player, +Players, -PlayerInfo)
% Returns the information about the given player (whether is human, bot(1) or bot(2)).
player_info(player_x, players(Px, _), Px).
player_info(player_o, players(_, Po), Po).

% next_player(+Player, -NextPlayer)
% Returns the next player after a move.
next_player(player_x, player_o).
next_player(player_o, player_x).

% valid_move(+Board, +Last, +Move)
% Represents whether the given move is valid.
valid_move(Board, none, Move) :-
    empty_cell(Board, Move). % if no move has been made yet, the only requirement is that the cell is empty
valid_move(Board, position(LastX, LastY), position(MoveX, MoveY)) :-
    empty_cell(Board, position(MoveX, MoveY)), % the cell must always be empty
    DistX is (MoveX - LastX), % the distance between the last move and the current move
    DistY is (MoveY - LastY),
    between(-1, 1, DistX), % the distance must be between -1 and 1 (adjacent cells)
    between(-1, 1, DistY).

% move_board(+Board, +Move, +Piece, -BoardNext)
% Places Piece in the given position of the Board, and returns the new Board.
move_board([[_|Rest] | RowRest], position(0, 0), Piece, [[Piece|Rest] | RowRest]). % When in the correct position; Place the piece here and leave the rest of the board unchanged
move_board([[Cell|Rest]], position(X, 0), Piece, [[Cell|RestNext]]) :- % When in correct and only row; Leave this cell unchanged and check the rest of the row
    X > 0,
    X1 is X - 1, % decrement the X position because we are moving to the next cell
    move_board([Rest], position(X1, 0), Piece, [RestNext]). % Check the rest of the row
move_board([Row|Rest], position(X, 0), Piece, [RowNext|Rest]) :- % When in the correct row but there are more; Place the piece in this row and leave the rest of the board unchanged.
    X > 0,
    Rest \== [],
    RestNext \== [],
    move_board([Row], position(X, 0), Piece, [RowNext]). % Place the piece in this row.
move_board([Row|Rest], position(X, Y), Piece, [Row|RestNext]) :- % Not in correct row; Leave this row unchanged and check the rest of the board
    X >= 0,
    Y > 0,
    Y1 is Y - 1, % decrement the Y position because we are moving to the next row
    move_board(Rest, position(X, Y1), Piece, RestNext). % Check the rest of the board

% move(+GameState, +Move, -NewGameState)
% Validade and make a move, returning the new game state.
move(
    game_state(
        Board, 
        current_player(Player), 
        last_move(Last), 
        Players, 
        Goal
    ),
    Move,
    game_state(
        BoardNext, 
        current_player(PlayerNext), 
        last_move(Move),
        Players,
        Goal
    )
) :-
    valid_move(Board, Last, Move), % the move must be valid
    move_board(Board, Move, Player, BoardNext), % make the move
    next_player(Player, PlayerNext). % update the current player
 
% game_over(+GameState, -Winner)
% Checks whether the game is over, and returns the winner if it is (or none if it is a draw).
game_over(GameState, none) :-
    valid_moves(GameState, []). % if there are no valid moves, the game is over in a draw
game_over(
    game_state(
        Board, 
        _, 
        _,
        _, 
        win_target(Goal)
    ), 
    Winner
) :-
    (Winner = player_x; Winner = player_o), % the winner is either player_x or player_o
    wins_game(Board, Winner, Goal).

% wins_game(+Board, +Player, +Goal)
% Checks whether the given player has won the game.
wins_game(_, _, 0). % If no more pieces needed to win, the player won.
wins_game(Board, Winner, Goal) :-
    nth00(I, J, Board, Winner), % There exists a piece where the player has a piece in this position
    Goal1 is Goal - 1,
    I1 is I + 1,
    wins_game_horizontally(Board, I1, J, Winner, Goal1). % Check if there are Goal-1 pieces to the right
wins_game(Board, Winner, Goal) :-
    nth00(I, J, Board, Winner), % There exists a piece where the player has a piece in this position
    Goal1 is Goal - 1,
    J1 is J + 1,
    wins_game_vertically(Board, I, J1, Winner, Goal1). % Check if there are Goal-1 pieces below
wins_game(Board, Winner, Goal) :-
    nth00(I, J, Board, Winner), % There exists a piece where the player has a piece in this position
    Goal1 is Goal - 1,
    I1 is I + 1,
    J1 is J + 1,
    wins_game_diagonally_down(Board, I1, J1, Winner, Goal1). % Check if there are Goal-1 pieces below and to the right
wins_game(Board, Winner, Goal) :-
    nth00(I, J, Board, Winner), % There exists a piece where the player has a piece in this position
    Goal1 is Goal - 1,
    I1 is I + 1,
    J1 is J - 1,
    wins_game_diagonally_up(Board, I1, J1, Winner, Goal1). % Check if there are Goal-1 pieces above and to the right

% wins_game_horizontally(+Board, +I, +J, +Player, +Goal)
% Checks whether the given player has won the game only considering the horizontal direction.
wins_game_horizontally(_, _, _, _, 0). % If no more pieces needed to win, the player won.
wins_game_horizontally(Board, I, J, Winner, Goal) :-
    nth00(I, J, Board, Winner), % The player has a piece in this position
    Goal1 is Goal - 1,
    I1 is I + 1,
    wins_game_horizontally(Board, I1, J, Winner, Goal1). % Check if there are Goal-1 pieces to the right

% wins_game_vertically(+Board, +I, +J, +Player, +Goal)
% Checks whether the given player has won the game only considering the vertical direction.
wins_game_vertically(_, _, _, _, 0). % If no more pieces needed to win, the player won.
wins_game_vertically(Board, I, J, Winner, Goal) :-
    nth00(I, J, Board, Winner), % The player has a piece in this position
    Goal1 is Goal - 1,
    J1 is J + 1,
    wins_game_vertically(Board, I, J1, Winner, Goal1). % Check if there are Goal-1 pieces below

% wins_game_diagonally_down(+Board, +I, +J, +Player, +Goal)
% Checks whether the given player has won the game only considering the descending diagonal direction.
wins_game_diagonally_down(_, _, _, _, 0). % If no more pieces needed to win, the player won.
wins_game_diagonally_down(Board, I, J, Winner, Goal) :-
    nth00(I, J, Board, Winner), % The player has a piece in this position
    Goal1 is Goal - 1,
    I1 is I + 1,
    J1 is J + 1,
    wins_game_diagonally_down(Board, I1, J1, Winner, Goal1). % Check if there are Goal-1 pieces below and to the right

% wins_game_diagonally_up(+Board, +I, +J, +Player, +Goal)
% Checks whether the given player has won the game only considering the ascending diagonal direction.
wins_game_diagonally_up(_, _, _, _, 0). % If no more pieces needed to win, the player won.
wins_game_diagonally_up(Board, I, J, Winner, Goal) :-
    nth00(I, J, Board, Winner), % The player has a piece in this position
    Goal1 is Goal - 1,
    I1 is I + 1,
    J1 is J - 1,
    wins_game_diagonally_up(Board, I1, J1, Winner, Goal1). % Check if there are Goal-1 pieces above and to the right

% valid_moves(+GameState, -ListOfMoves)
% Returns the list of valid moves given the state of the game
valid_moves(
    game_state(Board, _, last_move(LastMove), _, _),
    ListOfMoves
) :-
    findall(Move, valid_move(Board, LastMove, Move), ListOfMoves).

% value(+GameState, +Player, -Value)
% Quantifies the value of the current game state, to be used as an heuristic in the bot's greedy algorithm
value(GameState, Player, Value) :-
    game_over(GameState, Player), % if the player won, the value must be as big as possible
    !, % stop the search
    GameState = game_state(Board, _, _, _, _),
    board_size(Board, size(Cols, Rows)),
    Value is 1 + Cols*(Cols+1) + Rows*(Rows+1). % no non-winning value is as big as this
value(GameState, Player, -1) :- % if the other player has a winning move next turn, the value must be as low as possible
    valid_moves(GameState, ValidMoves),
    next_player(Player, Opponent),
    findall(
        Mv,
        NewState^( 
            member(Mv, ValidMoves), 
            move(GameState, Mv, NewState), 
            game_over(NewState, Opponent) 
        ), 
        [_|_] % if there is a move that is valid and makes the opponent win
    ),
    !. % stop the search
value(game_state(Board, _, last_move(Last), _, _), Player, Value) :-
    !, % stop the search
    Last = position(Col, Row),
    ColRight is Col + 1,
    RowDown is Row + 1,
    RowUp is Row - 1,
    value_counter(Board, Last, direction(-1, 0), Player, 0, AccLeft, IncAccLeft), % count the value to the left
    value_counter(Board, position(ColRight, Row), direction(1, 0), Player, IncAccLeft, AccRight, _), % continuing counting to the right
    value_counter(Board, Last, direction(0, -1), Player, 0, AccUp, IncAccUp), % count the value above
    value_counter(Board, position(Col, RowDown), direction(0, 1), Player, IncAccUp, AccDown, _), % continuing counting below
    value_counter(Board, Last, direction(-1, -1), Player, 0, AccLeftUp, IncAccLeftUp), % count the value to the left and above
    value_counter(Board, position(ColRight, RowDown), direction(1, 1), Player, IncAccLeftUp, AccRightDown, _), % continuing counting to the right and below
    value_counter(Board, Last, direction(-1, 1), Player, 0, AccLeftDown, IncAccLeftDown), % count the value to the left and below
    value_counter(Board, position(ColRight, RowUp), direction(1, -1), Player, IncAccLeftDown, AccRightUp, _), % continuing counting to the right and above
    Value is (AccLeft + AccRight + AccUp + AccDown + AccLeftUp + AccRightDown + AccLeftDown + AccRightUp). % sum all the counts
value(_, _, 0). % no previous move; the value is 0.

% value_counter(+Board, +Position, +Direction, +Player, +Inc, -Result, -ResultInc)
% Counts the value of the given position in the given direction, starting from the given position, and incrementing the result by the given increment.
value_counter(B, M, D, P, I, R, RI) :- value_counter(B, M, D, P, I, R, RI, 0). % auxiliary Accumulator
value_counter(Board, position(Col, Row), direction(I, J), Player, Inc, Result, ResultInc, Acc) :-
    nth00(Col, Row, Board, Player), % the position has the player's piece
    Col1 is Col + I, % move to the next position in the given direction
    Row1 is Row + J,
    Inc1 is Inc + 1, % add 1 to the increment
    Acc1 is Acc + Inc1, % increment the accumulator
    !,
    value_counter(Board, position(Col1, Row1), direction(I, J), Player, Inc1, Result, ResultInc, Acc1). % Keep counting in the next position
value_counter(_, _, _, _, Inc, Result, Inc, Result). % At the end, the result is the accumulator and the increment is the result increment

% choose_move(+GameState, +Level, -Move)
% Chooses a move to play given the state of the game and the level of the bot
choose_move(GameState, 1, Move) :-
    valid_moves(GameState, ListOfMoves),
    random_member(Move, ListOfMoves). % level 1: choose a random valid move
choose_move(GameState, 2, Move):-
    GameState = game_state(_, current_player(Player), _, _, _),
    valid_moves(GameState, ListOfMoves),
    setof(
        Value-Mv, 
        NewState^( 
            member(Mv, ListOfMoves), 
            move(GameState, Mv, NewState), 
            value(NewState, Player, Value) 
        ), 
        OrderedMoves % calculate the value of each valid move and order them by value
    ),
    choose_last_move(OrderedMoves, _-Move). % level 2: choose the move that maximizes the value

% choose_last_move(+List, -Move)
% Chooses the last element in the list
choose_last_move([Last], Last).
choose_last_move([_ | Moves], Last) :- choose_last_move(Moves, Last).
