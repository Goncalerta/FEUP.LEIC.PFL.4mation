%%%%% GENERIC HELPER PRINT FUNCTIONS %%%%%

% print_n(+S, +N)
% Prints S N times
print_n(_, 0).
print_n(S, N) :-
    N > 0,
    write(S),
    N1 is N - 1,
    print_n(S, N1).

% margin(N)
% Prints N spaces
margin(N) :-
    print_n(' ', N).

% print_banner_line(+Line, +Padding, +Margin)
% Prints a line of a banner given the assymetric padding
print_banner_line(Line, assymetric_padding(PaddingLeft, PaddingRight), Margin) :-
    margin(Margin),
    write('|'),
    margin(PaddingLeft),
    write(Line),
    margin(PaddingRight),
    write('|'),
    nl.

% print_banner_line(+Line, +Width, +Margin)
% Prints a line of a banner with the given width, calculating the necessary padding
print_banner_line(Line, Width, Margin) :-
    atom_length(Line, LineLength),
    NeededPadding is Width - LineLength,
    LeftPadding is NeededPadding // 2,
    RightPadding is NeededPadding - LeftPadding, % NeededPadding - LeftPadding is the remainder (it might be different from LeftPadding)
    print_banner_line(Line, assymetric_padding(LeftPadding, RightPadding), Margin).

% print_banner_bar(+LineLenght, +Margin)
% Prints a horizontal bar of the banner given
print_banner_bar(LineLength, Margin) :-
    margin(Margin),
    write('+'),
    print_n('-', LineLength), 
    write('+'),
    nl.

% print_banner_n_empty_lines(+N, +LineLength, +Margin)
% Prints N empty lines of the banner with the given line length and margin
print_banner_n_empty_lines(0, _, _). % Base case, don't print anything
print_banner_n_empty_lines(N, LineLength, Margin) :-
    N > 0,
    margin(Margin),
    write('|'),
    print_n(' ', LineLength),
    write('|'),
    nl,
    N1 is N - 1,
    print_banner_n_empty_lines(N1, LineLength, Margin). % print the rest of the empty lines

% print_banner_lines(+Lines, +LineLength, +Margin)
% Prints the lines of the banner given
print_banner_lines([], _, _). % Base case, don't print anything
print_banner_lines([Line|Rest], LineLength, Margin) :-
    print_banner_line(Line, LineLength, Margin), % Print the line
    print_banner_lines(Rest, LineLength, Margin). % Print the rest of the lines

% print_banner_with_width(+Lines, +Width, +HeightPadding, +Margin)
% Prints a banner with the given lines, width and vertical padding
print_banner_with_width(Lines, Width, HeightPadding, Margin) :-
    print_banner_bar(Width, Margin),
    print_banner_n_empty_lines(HeightPadding, Width, Margin),
    print_banner_lines(Lines, Width, Margin),
    print_banner_n_empty_lines(HeightPadding, Width, Margin),
    print_banner_bar(Width, Margin).

% print_banner(+Lines, +Width, +HeightPadding, +Margin)
% Prints a banner with the given lines, width, vertical padding and margin
print_banner(Lines, WidthPadding, HeightPadding, Margin) :-
    maplist(atom_length, Lines, LineLengths),
    max_member(LargestLineLength, LineLengths), % Find the largest line length
    Width is WidthPadding + WidthPadding + LargestLineLength, % Calculate the width of the banner based on the padding and the largest line length
    print_banner_with_width(Lines, Width, HeightPadding, Margin). % Print the banner

%%%%% APPLICATION SPECIFIC HELPER FUNCTIONS %%%%%

% get_symbol(+Cell, -Symbol)
% Gets the symbol of the cell
get_symbol(empty, ' '). % Empty cell
get_symbol(legal_move, '.'). % Valid move
get_symbol(player_x, 'X'). % Player X piece
get_symbol(player_o, 'O'). % Player O piece

% print_player(+Player)
% Prints the player given
print_player(player_x) :- write('player X').
print_player(player_o) :- write('player O').

% print_player_config(+Player)
% Prints the player configuration given (human or bot)
print_player_config(human) :- write('human').
print_player_config(bot(Level)) :- write('bot with level '), write(Level).

%%%%% OPEN MENU, START GAME, GAME LOOP %%%%%

% open_menu(+MenuState)
% Opens the given menu
open_menu(MenuState) :-
    display_menu(MenuState), % display
    read_response(MenuState, Action), % read user input
    do_menu_action(MenuState, Action). % execute user action

% play_game(+Config)
% Starts the game with the given configuration
play_game(Config) :-
    initial_state(Config, GameState), % get initial state from configuration
    play_turn(GameState, Config). % play first turn

% play_turn(+GameState, +Config)
% Plays a turn of the game
play_turn(GameState, Config) :- % game over case
    GameState = game_state(Board, _, _, _, _),
    game_over(GameState, Winner),
    display_board(Board), % display the final state
    nl,
    open_menu(menu_state(game_over(Winner), Config)). % open gameover menu
play_turn(GameState, Config) :-
    GameState = game_state(_, current_player(Player), _, Players, _),
    display_game(GameState), % display
    player_info(Player, Players, PlayerInfo), % check if player is human or bot
    play_turn(GameState, Config, PlayerInfo). % play turn given the player info (human or bot)

% play_turn(+GameState, +Config, +PlayerInfo)
% Plays a turn of the game given the player info
play_turn(GameState, Config, human) :- % human case
    GameState = game_state(_, _, _, _, win_target(Goal)),
    format('Get a row, column or diagonal of ~w consecutive pieces to win.\n', Goal),
    nl,
    write('    |: move(column, row).     Put a piece on the given position.\n'),
    write('    column - character; row - positive integer (must be a valid move).\n\n'),
    write('    |: give_up.               Stop playing the game (the opponent automatically wins).\n'),
    nl,
    read_response(GameState, Action), % read user input
    !,
    do_game_action(GameState, Config, Action). % execute user action
play_turn(GameState, Config, bot(Level)) :- % bot case
    write('The bot is thinking...\n'),
    sleep(1), % wait 1 second before making the move
    choose_move(GameState, Level, Move), % choose move based on bot level
    move(GameState, Move, NewGameState), % make the move
    play_turn(NewGameState, Config). % play next turn

%%%%% INPUT %%%%%

% read_response(+State, -Action)
% Reads the user input and returns the corresponding action
read_response(State, Action) :- 
    read_term(Response, [syntax_errors(quiet)]), % don't crash on synstax errors; instead will show a error message and read again
    !,
    process_response(State, Response, Action).
read_response(State, Action) :- % if the response is not valid, show an error message and read again
    write('Invalid option. Unable to understand term.\n'),
    read_response(State, Action).

% process_response(+State, +Response, -Action)
% Processes the response given and returns the corresponding action
process_response(State, Response, Action) :-
    ground(Response), % the response must be ground
    response(State, Response, Action), % check if the response is valid and return action
    !.
process_response(State, Response, Action) :- % if the response is not valid, show an error message and read again
    ground(Response),
    !,
    format('Invalid option \'~w\'.\n', [Response]),
    read_response(State, Action).
process_response(State, _, Action) :- % response not ground, show an error message and read again
    write('Invalid option. You may not use variables in your input.\n'),
    read_response(State, Action).

%%%%% DISPLAY MENUS %%%%%

% display_menu(+MenuState)
% Displays the given menu
display_menu(menu_state(main, _)) :-
    nl,nl,nl,
    print_banner(['Welcome to 4MATION!'], 4, 1, 6),
    nl,
    write('    |: play.\t\tStart a new game.\n'),
    write('    |: config.\t\tGame configurations.\n'),
    write('    |: rules.\t\tSee rules.\n'),
    write('    |: exit.\t\tQuit game.\n'),
    nl.

display_menu(menu_state(rules, _)) :-
    print_banner(['Rules'], 12, 1, 12),
    nl,
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
    write('    |: back.            Go back.\n'),
    nl.

display_menu(
    menu_state(
        config, 
        config(
            size(Cols, Rows),
            players(Px, Po), 
            first_player(First), 
            win_target(Goal)
        )
    )
) :-
    print_banner(['Configure'], 12, 1, 16),
    nl,
    write('    |: board_size(columns, rows).     Set the board size.\n'),
    write('    columns, rows - positive integer (max 26).\n\n'),
    write('    |: win_pieces(pieces).            Set the number of consecutive pieces to win the game.\n'),
    write('    pieces - positive integer.\n\n'),
    write('    |: bot(player, level).            Set player as a bot with given AI level.\n'),
    write('    player - player_x or player_o.\n'),
    write('    level - 1 (random) or 2 (greedy).\n\n'),
    write('    |: human(player).                 Set player as a human.\n'),
    write('    player - player_x or player_o.\n\n'),
    write('    |: first_player(player).          Set player as the first to play.\n'),
    write('    player - player_x or player_o.\n\n'),
    write('    |: back.                          Go back.\n'),
    nl,
    print_banner(['Current Configurations'], 2, 0, 0),
    format('Board size:       ~dx~d.\n', [Cols, Rows]),
    format('Win condition:    ~d pieces\n', [Goal]),
    write('Player X:         '), print_player_config(Px), nl,
    write('Player O:         '), print_player_config(Po), nl,
    write('First to play:    '), print_player(First), nl,
    nl.

display_menu(menu_state(game_over(none), _)) :-
    nl,
    write('Game Over! The game ended in a draw.'),
    nl,
    write('    |: again.           Play again.\n'),
    write('    |: back.            Go back.\n'),
    nl.

display_menu(menu_state(game_over(Winner), _)) :-
    nl,
    write('Game Over! Player '), print_player(Winner), write(' wins!'),
    nl,
    write('    |: again.           Play again.\n'),
    write('    |: back.            Go back.\n'),
    nl.

%%%%% DISPLAY GAME %%%%%

% display_board_bar(+Cols)
% Prints a horizontal bar of the board
display_board_bar(Cols) :-
    margin(4),
    write('|'),
    print_n('---|', Cols).

% mark_valid_moves(+Row, +RowNum, +ValidMoves, -RowWithLegalMoves)
% Marks the valid moves in the given row, returning the new row
mark_valid_moves(Row, RowNum, ValidMoves, RowWithLegalMoves) :-
    mark_valid_moves(Row, 0, RowNum, ValidMoves, RowWithLegalMoves). % Auxiliary column index
mark_valid_moves([], _, _, _, []). % End of row
mark_valid_moves([empty | InNext], ColNum, RowNum, ValidMoves, [legal_move | OutNext]) :- % mark legal move
    memberchk(position(ColNum, RowNum), ValidMoves), % the current position is a legal move
    !,
    ColNum1 is ColNum + 1,
    mark_valid_moves(InNext, ColNum1, RowNum, ValidMoves, OutNext). % mark next column
mark_valid_moves([Element | InNext], ColNum, RowNum, ValidMoves, [Element | OutNext]) :- % leave element as is
    ColNum1 is ColNum + 1,
    mark_valid_moves(InNext, ColNum1, RowNum, ValidMoves, OutNext). % mark next column

% display_board_row(+Row, +RowNum, +ValidMoves)
% Prints a row of the board
display_board_row(Row, RowNum, ValidMoves) :-
    RowNum1 is RowNum - 1,
    format(' ~|~` t~d~2+ ', [RowNum]),  % write number with leading spaces
    write('|'),
    mark_valid_moves(Row, RowNum1, ValidMoves, RowWithLegalMoves),
    maplist(get_symbol, RowWithLegalMoves, RowSymbols), % get symbols based on cell's content and legal_move marks
    maplist(format(' ~s |'), RowSymbols). % print symbols

% display_board_header(+Cols)
% Prints the header of the board
display_board_header(Cols) :-
    char_code(a, Start), % start with letter 'a'
    End is Start + Cols - 1, % end with letter 'a' + number of columns - 1
    numlist(Start, End, ColLabels), % get column labels
    margin(4),
    maplist(format('  ~c '), ColLabels), % print column labels
    nl,
    display_board_bar(Cols),
    nl.

% display_board_body(+Rows, +ValidMoves)
% Prints the body of the board
display_board_body(Rows, ValidMoves) :-
    display_board_body(Rows, 1, ValidMoves). % Auxiliary row index
display_board_body([], _, _). % End of board
display_board_body([Row|Next], RowNum, ValidMoves) :-
    display_board_row(Row, RowNum, ValidMoves), % print row
    nl,
    length(Row, Cols),
    display_board_bar(Cols), % print bar
    nl,
    NextRowNum is RowNum + 1,
    display_board_body(Next, NextRowNum, ValidMoves). % print next row

% display_board(+Board, +ValidMoves)
% Prints the board
display_board(Board, ValidMoves) :-
    board_size(Board, size(Cols, _)),
    nl,
    display_board_header(Cols), % print header
    display_board_body(Board, ValidMoves). % print body

% display_game(+Board)
% Prints the board, without showing valid moves
display_board(Board) :-
    display_board(Board, []).

% display_game(+GameState)
% Displays the given game state
display_game(GameState) :-
    GameState = game_state(Board, current_player(Player), _, _, _),
    valid_moves(GameState, ValidMoves), % get valid moves
    display_board(Board, ValidMoves), % print board
    nl,
    write('It\'s '), print_player(Player), write('\'s turn. '). % print player turn

%%%%% RESPONSES %%%%%

% response(+State, +Response, -Action)
% Represents a valid user input given the state of the game or menu, and returns the associated action.
response(
    menu_state(main, _), % on main menu
    play,
    start_game % the response starts the game
).

response(
    menu_state(main, _), % on main menu
    config,
    open_menu(config) % the response opens the config menu
).

response(
    menu_state(main, _), % on main menu
    rules,
    open_menu(rules) % the response opens the rules menu
).

response(
    menu_state(main, _), % on main menu
    exit,
    exit % the response exits the game
).

response(
    game_state(_, _, _, _, _), % on game
    move(ColChar, RowNum),
    move(Col, Row) % the response moves the current player
) :-
    atom_length(ColChar, 1), % the input column is a char
    char_code(ColChar, ColNum),
    Col is ColNum - "a", % convert column to index
    Row is RowNum - 1. % convert row to index

response(
    game_state(_, _, _, _, _), % on game
    give_up,
    give_up % this response makes the player give up and end the game
).

response(
    menu_state(config, OldConfig), % on config menu
    Command,
    config(NewConfig) % the response changes the config
) :- response_config(Command, OldConfig, NewConfig). % must be a valid config command

response(
    menu_state(config, _), % on config menu
    back, 
    open_menu(main) % the response goes back to the main menu
).

response(
    menu_state(rules, _), % on rules menu
    back, 
    open_menu(main) % the response goes back to the main menu
).

response(
    menu_state(game_over(_), _), % on game over menu
    again,
    start_game % the response starts a new game
).

response(
    menu_state(game_over(_), _), % on game over menu
    back,
    open_menu(main) % the response goes back to the main menu
).

% response_config(+Command, +OldConfig, -NewConfig)
% Represents a valid user input on the config menu, updates the configuration and returns the associated config.
response_config(
    board_size(Cols, Rows), % change board size
    config(_, Players, F, win_target(Goal)), 
    config(size(Cols, Rows), Players, F, win_target(Goal))
) :-
    integer(Cols),
    integer(Rows),
    Cols > 0,
    Cols =< 26,
    Rows > 0,
    Rows =< 26.

response_config(
    bot(player_x, Level), % make player_x a bot
    config(Size, players(_, Po), F, G),
    config(Size, players(bot(Level), Po), F, G)
) :-
    integer(Level),
    Level > 0,
    Level < 3.

response_config(
    bot(player_o, Level), % make player_o a bot
    config(Size, players(Px, _), F, G),
    config(Size, players(Px, bot(Level)), F, G)
) :-
    integer(Level),
    Level > 0,
    Level < 3.

response_config(
    human(player_x), % make player_x a human
    config(Size, players(_, Po), F, G),
    config(Size, players(human, Po), Po, F, G)
).

response_config(
    human(player_o), % make player_o a human
    config(Size, players(Px, _), F, G),
    config(Size, players(Px, human), F, G)
).

response_config(
    first_player(First), % change first player
    config(Size, Players, _, G),
    config(Size, Players, first_player(First), G)
) :-
    (First = player_x; First = player_o).

response_config(
    win_pieces(Goal), % change win pieces
    config(Size, Players, F, _),
    config(Size, Players, F, win_target(Goal))
) :-
    integer(Goal),
    Goal > 0.

%%%%% ACTIONS %%%%%

% do_menu_action(+MenuState, +Action)
% Executes the given action on the menu state.
do_menu_action(menu_state(_, Config), start_game) :-
    play_game(Config). % start game

do_menu_action(menu_state(_, Config), open_menu(Menu)) :-
    open_menu(menu_state(Menu, Config)). % open menu

do_menu_action(menu_state(config, _), config(Config)) :-
    open_menu(menu_state(config, Config)). % reopen config menu with new configuration

do_menu_action(_, exit). % do nothing; exits game

% do_game_action(+GameState, +Action)
% Executes the given action on the game state.
do_game_action(GameState, Config, move(Col, Row)) :- % valid move
    move(GameState, position(Col, Row), NewGameState), % make the move
    play_turn(NewGameState, Config). % play next turn
do_game_action(GameState, Config, move(Col, Row)) :- % invalid move
    GameState = game_state(Board, _, _, _, _),
    empty_cell(Board, position(Col, Row)), % the cell is empty; the reason the move is invalid must be because the move is not adjacent to last
    ColNum is Col + "a",
    RowNum is Row + 1,
    char_code(ColChar, ColNum),
    format('Move (~w, ~w) is not valid. It must be adjacent to the last move.\n', [ColChar, RowNum]),
    read_response(GameState, Action), % read another input
    do_game_action(GameState, Config, Action). % execute the new action
do_game_action(GameState, Config, move(Col, Row)) :- % invalid move; empty_cell check failed
    GameState = game_state(Board, _, _, _, _),
    inside_board(Board, position(Col, Row)), % the cell is inside the board; the reason the move is invalid must be because the cell is not empty
    ColNum is Col + "a",
    RowNum is Row + 1,
    char_code(ColChar, ColNum),
    format('Move (~w, ~w) is not valid. You must play on an empty cell.\n', [ColChar, RowNum]),
    read_response(GameState, Action), % read another input
    do_game_action(GameState, Config, Action). % execute the new action
do_game_action(GameState, Config, move(Col, Row)) :- % invalid move; inside_board check failed
    ColNum is Col + "a",
    RowNum is Row + 1,
    char_code(ColChar, ColNum),
    format('Move (~w, ~w) is not valid. You must play inside the board.\n', [ColChar, RowNum]),
    read_response(GameState, Action), % read another input
    do_game_action(GameState, Config, Action). % execute the new action

do_game_action(game_state(_, current_player(Player), _, _, _), Config, give_up) :- % current player gave up
    next_player(Player, Opponent),
    open_menu(menu_state(game_over(Opponent), Config)). % open game over menu. the opponent is the winner
