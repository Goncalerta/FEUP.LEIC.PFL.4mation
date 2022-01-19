% initial_state(+Config, -GameState)
% Size is size(X, Y)
initial_state(
    config(
        size(Cols, Rows),
        Players,
        first_player(First),
        win_target(Goal)
    ), 
    game_state(
        Board, 
        current_player(First), 
        last_move(none), 
        Players, 
        win_target(Goal)
    )
) :-
    initial_board(Cols, Rows, Board).

% next_player(+Player, -NextPlayer)
% Returns the next player after a move.
next_player(player_x, player_o).
next_player(player_o, player_x).

% player_info(Player, Players, PlayerInfo)
player_info(player_x, players(Px, _), Px).
player_info(player_o, players(_, Po), Po).

valid_move(Board, none, Move) :-
    empty_cell(Board, Move).

valid_move(Board, position(LastX, LastY), position(MoveX, MoveY)) :-
    empty_cell(Board, position(MoveX, MoveY)),
    DistX is (MoveX - LastX),
    DistY is (MoveY - LastY),
    between(-1, 1, DistX),
    between(-1, 1, DistY).

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

% move(+GameState, +Move, -NewGameState)
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
    valid_move(Board, Last, Move),
    move_board(Board, Move, Player, BoardNext),
    next_player(Player, PlayerNext).
 
% game_over(+GameState, -Winner)
% Winner is one of [player_x, player_o, none]
game_over(GameState, none) :-
    valid_moves(GameState, []).

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
    (Winner = player_x; Winner = player_o),
    wins_game(Board, Winner, Goal).

wins_game(_, _, 0).
wins_game(Board, Winner, Goal) :-
    nth00(I, J, Board, Winner),
    Goal1 is Goal - 1,
    I1 is I + 1,
    wins_game_horizontally(Board, I1, J, Winner, Goal1).
wins_game(Board, Winner, Goal) :-
    nth00(I, J, Board, Winner),
    Goal1 is Goal - 1,
    J1 is J + 1,
    wins_game_vertically(Board, I, J1, Winner, Goal1).
wins_game(Board, Winner, Goal) :-
    nth00(I, J, Board, Winner),
    Goal1 is Goal - 1,
    I1 is I + 1,
    J1 is J + 1,
    wins_game_diagonally_down(Board, I1, J1, Winner, Goal1).
wins_game(Board, Winner, Goal) :-
    nth00(I, J, Board, Winner),
    Goal1 is Goal - 1,
    I1 is I + 1,
    J1 is J - 1,
    wins_game_diagonally_up(Board, I1, J1, Winner, Goal1).

wins_game_horizontally(_, _, _, _, 0).
wins_game_horizontally(Board, I, J, Winner, Goal) :-
    nth00(I, J, Board, Winner),
    Goal1 is Goal - 1,
    I1 is I + 1,
    wins_game_horizontally(Board, I1, J, Winner, Goal1).

wins_game_vertically(_, _, _, _, 0).
wins_game_vertically(Board, I, J, Winner, Goal) :-
    nth00(I, J, Board, Winner),
    Goal1 is Goal - 1,
    J1 is J + 1,
    wins_game_vertically(Board, I, J1, Winner, Goal1).

wins_game_diagonally_down(_, _, _, _, 0).
wins_game_diagonally_down(Board, I, J, Winner, Goal) :-
    nth00(I, J, Board, Winner),
    Goal1 is Goal - 1,
    I1 is I + 1,
    J1 is J + 1,
    wins_game_diagonally_down(Board, I1, J1, Winner, Goal1).

wins_game_diagonally_up(_, _, _, _, 0).
wins_game_diagonally_up(Board, I, J, Winner, Goal) :-
    nth00(I, J, Board, Winner),
    Goal1 is Goal - 1,
    I1 is I + 1,
    J1 is J - 1,
    wins_game_diagonally_up(Board, I1, J1, Winner, Goal1).

% valid_moves(+GameState, -ListOfMoves)
valid_moves(
    game_state(Board, _, last_move(LastMove), _, _),
    ListOfMoves
) :-
    findall(Move, valid_move(Board, LastMove, Move), ListOfMoves).

% value(+GameState, +Player, -Value)
value(GameState, Player, Value) :-
    game_over(GameState, Player),
    !,
    GameState = game_state(Board, _, _, _, _),
    board_size(Board, size(Cols, Rows)),
    Value is 2 + Cols*(Cols+1) + Rows*(Rows+1).

value(GameState, Player, -1) :-
    valid_moves(GameState, ValidMoves),
    next_player(Player, Opponent),
    findall(
        Mv,
        NewState^( 
            member(Mv, ValidMoves), 
            move(GameState, Mv, NewState), 
            game_over(NewState, Opponent) 
        ), 
        [_|_]
    ),
    !.

value(game_state(Board, _, last_move(Last), _, _), Player, Value) :-
    !,
    Last = position(Col, Row),
    ColRight is Col + 1,
    RowDown is Row + 1,
    RowUp is Row - 1,
    value_counter(Board, Last, direction(-1, 0), Player, 0, AccLeft, IncAccLeft),
    value_counter(Board, position(ColRight, Row), direction(1, 0), Player, IncAccLeft, AccRight, _),
    value_counter(Board, Last, direction(0, -1), Player, 0, AccUp, IncAccUp),
    value_counter(Board, position(Col, RowDown), direction(0, 1), Player, IncAccUp, AccDown, _),
    value_counter(Board, Last, direction(-1, -1), Player, 0, AccLeftUp, IncAccLeftUp),
    value_counter(Board, position(ColRight, RowDown), direction(1, 1), Player, IncAccLeftUp, AccRightDown, _),
    value_counter(Board, Last, direction(-1, 1), Player, 0, AccLeftDown, IncAccLeftDown),
    value_counter(Board, position(ColRight, RowUp), direction(1, -1), Player, IncAccLeftDown, AccRightUp, _),
    Value is (AccLeft + AccRight + AccUp + AccDown + AccLeftUp + AccRightDown + AccLeftDown + AccRightUp).

value(_, _, 0).

value_counter(B, M, D, P, I, R, RI) :- value_counter(B, M, D, P, I, R, RI, 0).
value_counter(Board, position(Col, Row), direction(I, J), Player, Inc, Result, ResultInc, Acc) :-
    nth00(Col, Row, Board, Player),
    Col1 is Col + I,
    Row1 is Row + J,
    Inc1 is Inc + 1,
    Acc1 is Acc + Inc1,
    !,
    value_counter(Board, position(Col1, Row1), direction(I, J), Player, Inc1, Result, ResultInc, Acc1).
value_counter(_, _, _, _, Inc, Result, Inc, Result).

% choose_move(+GameState, +Level, -Move)
choose_move(GameState, 1, Move) :-
    valid_moves(GameState, ListOfMoves),
    random_member(Move, ListOfMoves).

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
        OrderedMoves
    ),
    choose_last_move(OrderedMoves, _-Move).

choose_last_move([Last], Last).
choose_last_move([_ | Moves], Last) :- choose_last_move(Moves, Last).
