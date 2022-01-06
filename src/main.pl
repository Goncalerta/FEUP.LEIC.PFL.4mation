:-use_module(library(lists)).
:-use_module(library(between)).
:-use_module(library(random)).

% GameState(Board, CurrentPlayer, LastPiecePlayed)
% [[Piece]]                      Piece is one of [player_x, player_o, empty]
% current_player(Player)         Player is one of [player_x, player_o]
% last_piece_played(Piece)       Piece is one of [position(X, Y), none]

% not(X) :- X, !, fail.
% not(_).

% % ifthen(+Condition, +Then, +Else)
% % ifthen(+Condition, +Then)
% ifthen(Condition, Then) :- Condition, Then.
% ifthen(Condition, Then, _) :- ifthen(Condition, Then).
% ifthen(Condition, _, Else) :- not(Condition), Else.




% play/0
play :- display_game(
    menu_state(
        current(main),
        config(
            size(8, 8),
            player_x(human),
            player_o(human),
            first_player(player_x),
            winning_length(4)
        )
    )
).

display_game(MenuState) :-
    MenuState = menu_state(CurrentMenu, Config),
    show_menu(CurrentMenu, Config),
    read(Response),
    parse_response(MenuState, Response).

parse_response(GameState, Response) :- response(GameState, Response, Action), !, Action.

parse_response(GameState, Response) :-
    format('Invalid option \'~w\'.\n', [Response]),
    read(NewResponse),
    parse_response(GameState, NewResponse).

print_player(player_x) :- write('player X').
print_player(player_o) :- write('player O').

print_player_config(human) :- write('human').
print_player_config(bot, Level) :- write('bot with level'), write(Level).

show_menu(current(main), _) :-
    write('Welcome to 4Mation!\n'),
    write('-------------------\n'),
    write('    > play.\t\tStart a new game.\n'),
    write('    > config.\t\tGame configurations.\n'),
    write('    > rules.\t\tSee rules.\n'),
    write('    > exit.\t\tQuit game.\n'),
    nl.

show_menu(current(rules), _) :-
    write('Rules of the game\n'),
    write('-------------------\n'),
    write('4Mation is an exciting strategy game that is very easy to learn\n'),
    write('but hard to master. The goal of the game is to create a line,\n'),
    write('column or diagonal of a number of consecutive pieces determined\n'),
    write('before the start of the game.\n'),
    nl,
    write('Each player takes turns placing a single piece on the board.\n'),
    write('The first player can place a piece anywhere on the board.\n'),
    write('In all other turns, the player must place a piece orthogonally\n'),
    write('or diagonally adjacent to the piece placed by the previous\n'),
    write('player.\n'),
    nl,
    write('The game ends when a player has created a line, column or\n'),
    write('diagonal of the target number of consecutive pieces of\n'),
    write('their type. In this case, that player is considered the winner.\n'),
    write('The game also can end if there are no valid moves to make.\n'),
    write('In this case, the game ends in a draw.\n'),
    nl,
    write('    > back.\t\tGo back.\n'),
    nl.

show_menu(
    current(config), 
    config(
        size(Cols, Rows), 
        player_x(Px), 
        player_o(Py), 
        first_player(First), 
        winning_length(Goal)
    )
) :-
    write('Current configurations\n'),
    write('-------------------------\n'),
    format('Board size:       ~dx~d.\n', [Cols, Rows]),
    format('Win condition:    ~d pieces\n', [Goal]),
    write('Player X:         '), print_player_config(Px), nl,
    write('Player O:         '), print_player_config(Py), nl,
    write('First to play:    '), print_player(First), nl,
    nl,
    write('Configure\n'),
    write('-------------------------\n'),
    write('    > board_size(columns, rows).     Set the board size.\n'),
    write('    columns, rows - positive integer.\n\n'),
    write('    > win_pieces(pieces).            Set the number of consecutive pieces to win the game.\n'),
    write('    pieces - positive integer.\n'),
    write('    columns, rows - positive integer.\n\n'),
    write('    > bot(player, level).            Set player as a bot with given AI level.\n'),
    write('    player - player_x or player_o.\n'),
    write('    level - 1 (random) or 2 (greedy).\n\n'),
    write('    > human(player).                 Set player as a human.\n'),
    write('    player - player_x or player_o.\n\n'),
    write('    > first_player(player).          Set player as the first to play.\n'),
    write('    player - player_x or player_o.\n\n'),
    write('    > back.\t\tGo back.\n'),
    nl.

% show_options(MenuState) :-
%     findall([Command, Description], response(MenuState, Command, Description, _), Commands),
%     maplist(format('    > ~w.\t\t~w\n'), Commands).


response(
    menu_state(current(main), Config), 
    play,
    start_game(Config)
).

response(
    menu_state(current(main), Config), 
    config,
    display_game(menu_state(current(config), Config))
).

response(
    menu_state(current(main), Config), 
    rules,
    display_game(menu_state(current(rules), Config))
).

response(
    menu_state(current(main), _), 
    exit,
    write('Thank you for playing!\n')
).

response(
    menu_state(current(config), OldConfig), 
    Command,
    display_game(menu_state(current(config), NewConfig))
) :- response_config(Command, OldConfig, NewConfig).

response(
    menu_state(current(config), Config), 
    back, 
    display_game(menu_state(current(main), Config))
).

response(
    menu_state(current(rules), Config), 
    back, 
    display_game(menu_state(current(main), Config))
).

response_config(
    board_size(Cols, Rows), 
    config(_, Px, Py, F, G), 
    config(size(Cols, Rows), Px, Py, F, G)
) :-
    Cols > 0,
    Rows > 0.

response_config(
    win_pieces(Goal),
    config(Size, Px, Py, F, _),
    config(Size, Px, Py, F, winning_length(Goal))
) :-
    Goal > 0.

response_config(
    bot(player_x, Level),
    config(Size, _, Py, F, G),
    config(Size, player_x(bot, Level), Py, F, G)
) :-
    Level > 0,
    Level < 3.

response_config(
    bot(player_o, Level),
    config(Size, Px, _, F, G),
    config(Size, Px, player_o(bot, Level), F, G)
) :-
    Level > 0,
    Level < 3.

response_config(
    human(player_x),
    config(Size, _, Py, F, G),
    config(Size, player_x(human), Py, F, G)
).

response_config(
    human(player_o),
    config(Size, Px, _, F, G),
    config(Size, Px, player_o(human), F, G)
).

response_config(
    first_player(First),
    config(Size, Px, Py, _, G),
    config(Size, Px, Py, first_player(First), G)
) :-
    (First = player_x; First = player_o).



% response(
%     menu_state(current(config), config(_, Px, Py)), 
%     board_size(Cols, Rows), 
%     display_game(menu_state(current(config), config(size(Cols, Rows), Px, Py)))
% ).
% response(
%     menu_state(current(config), config(Size), Px, Py)), 
%     bot(player_x, Level), 
%     display_game(menu_state(current(config), config(size(Cols, Rows), PX, PY)))
% ).
% response(menu_state(current(config), config(_, player_x(), _)), board_size(Cols, Rows), start_game(Config)).

% parse_response(menu_state(current(main), Config), Response) :- 
%     Response = play, !, start_game(BoardSize).
% parse_response(menu_state(current(main), Config), Response) :- 
%     Response = config, !, config_game(BoardSize).
% parse_response(menu_state(current(main), Config), Response) :- 
%     Response = exit, !, write('Thank you for playing!\n').

% parse_response(State, Response) :- 
%     format('Invalid option \'~w\'.\n', [Response]),
%     read(NewResponse),
%     parse_response(State, NewResponse).


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

% initial_state(+Size, -GameState)
% Size is size(X, Y)
initial_state(size(Cols, Rows), game_state(Board, current_player(player_x), none)) :-
    initial_board(Cols, Rows, Board).

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

