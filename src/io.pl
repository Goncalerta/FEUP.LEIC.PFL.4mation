print_n(_, 0).
print_n(S, N) :-
    N > 0,
    write(S),
    N1 is N - 1,
    print_n(S, N1).

margin(N) :-
    print_n(' ', N).

print_banner_line(Line, assymetric_padding(PaddingLeft, PaddingRight), Margin) :-
    margin(Margin),
    write('|'),
    margin(PaddingLeft),
    write(Line),
    margin(PaddingRight),
    write('|'),
    nl.

print_banner_line(Line, Width, Margin) :-
    atom_length(Line, LineLength),
    NeededPadding is Width - LineLength,
    LeftPadding is NeededPadding // 2,
    RightPadding is NeededPadding - LeftPadding,
    print_banner_line(Line, assymetric_padding(LeftPadding, RightPadding), Margin).

print_banner_bar(LineLength, Margin) :-
    margin(Margin),
    write('+'),
    print_n('-', LineLength), 
    write('+'),
    nl.

print_banner_n_empty_lines(0, _, _).
print_banner_n_empty_lines(N, LineLength, Margin) :-
    N > 0,
    margin(Margin),
    write('|'),
    print_n(' ', LineLength),
    write('|'),
    nl,
    N1 is N - 1,
    print_banner_n_empty_lines(N1, LineLength, Margin).

print_banner_lines([], _, _).
print_banner_lines([Line|Rest], LineLength, Margin) :-
    print_banner_line(Line, LineLength, Margin),
    print_banner_lines(Rest, LineLength, Margin).

print_banner_with_width(Lines, Width, HeightPadding, Margin) :-
    print_banner_bar(Width, Margin),
    print_banner_n_empty_lines(HeightPadding, Width, Margin),
    print_banner_lines(Lines, Width, Margin),
    print_banner_n_empty_lines(HeightPadding, Width, Margin),
    print_banner_bar(Width, Margin).

print_banner(Lines, WidthPadding, HeightPadding, Margin) :-
    maplist(atom_length, Lines, LineLengths),
    max_member(LargestLineLength, LineLengths),
    Width is WidthPadding + WidthPadding + LargestLineLength,
    print_banner_with_width(Lines, Width, HeightPadding, Margin).

get_symbol(empty, ' ').
get_symbol(legal_move, '.').  
get_symbol(player_x, 'X').
get_symbol(player_o, 'O').

print_player(player_x) :- write('player X').
print_player(player_o) :- write('player O').

print_player_config(human) :- write('human').
print_player_config(bot(Level)) :- write('bot with level '), write(Level).

open_menu(MenuState) :-
    display_menu(MenuState),
    read_response(MenuState, Action),
    do_menu_action(MenuState, Action).

play_game(Config) :-
    initial_state(Config, GameState),
    play_turn(GameState, Config).

play_turn(GameState, Config) :-
    GameState = game_state(Board, _, _, _, _),
    game_over(GameState, Winner),
    display_board(Board),
    nl,
    open_menu(menu_state(game_over(Winner), Config)).

play_turn(GameState, Config) :-
    GameState = game_state(_, current_player(Player), _, Players, _),
    display_game(GameState),
    player_info(Player, Players, PlayerInfo),
    play_turn(GameState, Config, PlayerInfo).

play_turn(GameState, Config, human) :-
    GameState = game_state(_, _, _, _, win_target(Goal)),
    format('Get a row, column or diagonal of ~w consecutive pieces to win.\n', Goal),
    nl,
    write('    |: move(column, row).     Put a piece on the given position.\n'),
    write('    column - character; row - positive integer (must be a valid move).\n\n'),
    write('    |: give_up.               Stop playing the game (the opponent automatically wins).\n'),
    nl,
    read_response(GameState, Action),
    !,
    do_game_action(GameState, Config, Action).

play_turn(GameState, Config, bot(Level)) :-
    write('The bot is thinking...\n'),
    sleep(1),
    choose_move(GameState, Level, Move),
    move(GameState, Move, NewGameState),
    play_turn(NewGameState, Config).

read_response(State, Action) :- 
    read_term(Response, [syntax_errors(quiet)]),
    !,
    process_response(State, Response, Action).

read_response(State, Action) :-
    write('Invalid option. Unable to understand term.\n'),
    read_response(State, Action).

process_response(State, Response, Action) :-
    ground(Response), 
    response(State, Response, Action), 
    !.

process_response(State, Response, Action) :-
    ground(Response),
    !,
    format('Invalid option \'~w\'.\n', [Response]),
    read_response(State, Action).

process_response(State, _, Action) :-
    write('Invalid option. You may not use variables in your input.\n'),
    read_response(State, Action).

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

display_board_bar(Cols) :-
    margin(4),
    write('|'),
    print_n('---|', Cols).

markValidMoves(Row, RowNum, ValidMoves, RowWithLegalMoves) :-
    markValidMoves(Row, 0, RowNum, ValidMoves, RowWithLegalMoves).
markValidMoves([], _, _, _, []).
markValidMoves([empty | InNext], ColNum, RowNum, ValidMoves, [legal_move | OutNext]) :-
    memberchk(position(ColNum, RowNum), ValidMoves),
    !,
    ColNum1 is ColNum + 1,
    markValidMoves(InNext, ColNum1, RowNum, ValidMoves, OutNext).

markValidMoves([Element | InNext], ColNum, RowNum, ValidMoves, [Element | OutNext]) :-
    ColNum1 is ColNum + 1,
    markValidMoves(InNext, ColNum1, RowNum, ValidMoves, OutNext).

display_board_row(Row, RowNum, ValidMoves) :-
    RowNum1 is RowNum - 1,
    format(' ~|~` t~d~2+ ', [RowNum]),  % write number with leading spaces
    write('|'),
    markValidMoves(Row, RowNum1, ValidMoves, RowWithLegalMoves),
    maplist(get_symbol, RowWithLegalMoves, RowSymbols),
    maplist(format(' ~s |'), RowSymbols).

display_board_header(Cols) :-
    char_code(a, Start),
    End is Start + Cols - 1,
    numlist(Start, End, ColLabels),
    margin(4),
    maplist(format('  ~c '), ColLabels),
    nl,
    display_board_bar(Cols),
    nl.

display_board_body(Rows, ValidMoves) :-
    display_board_body(Rows, 1, ValidMoves).
display_board_body([], _, _).
display_board_body([Row|Next], RowNum, ValidMoves) :-
    display_board_row(Row, RowNum, ValidMoves),
    nl,
    length(Row, Cols),
    display_board_bar(Cols),
    nl,
    NextRowNum is RowNum + 1,
    display_board_body(Next, NextRowNum, ValidMoves).
    
display_board(Board, ValidMoves) :-
    board_size(Board, size(Cols, _)),
    nl,
    display_board_header(Cols),
    display_board_body(Board, ValidMoves).

display_board(Board) :-
    display_board(Board, []).

display_game(GameState) :-
    GameState = game_state(Board, current_player(Player), _, _, _),
    valid_moves(GameState, ValidMoves),
    display_board(Board, ValidMoves),
    nl,
    write('It\'s '), print_player(Player), write('\'s turn. ').

response(
    menu_state(main, _), 
    play,
    start_game
).

response(
    menu_state(main, _), 
    config,
    open_menu(config)
).

response(
    menu_state(main, _), 
    rules,
    open_menu(rules)
).

response(
    menu_state(main, _), 
    exit,
    exit
).

response(
    game_state(Board, _, _, _, _),
    move(ColChar, RowNum),
    move(Col, Row)
) :-
    atom_length(ColChar, 1),
    char_code(ColChar, ColNum),
    Col is ColNum - "a",
    Row is RowNum - 1,
    board_size(Board, size(BoardCols, BoardRows)),
    Col >= 0,
    Col < BoardCols,
    Row >= 0,
    Row < BoardRows.

response(
    game_state(_, _, _, _, _), 
    give_up,
    give_up
).

response(
    menu_state(config, OldConfig), 
    Command,
    config(NewConfig)
) :- response_config(Command, OldConfig, NewConfig).

response(
    menu_state(config, _), 
    back, 
    open_menu(main)
).

response(
    menu_state(rules, _), 
    back, 
    open_menu(main)
).

response(
    menu_state(game_over(_), _), 
    again,
    start_game
).

response(
    menu_state(game_over(_), _), 
    back,
    open_menu(main)
).

response_config(
    board_size(Cols, Rows), 
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
    bot(player_x, Level),
    config(Size, players(_, Po), F, G),
    config(Size, players(bot(Level), Po), F, G)
) :-
    integer(Level),
    Level > 0,
    Level < 3.

response_config(
    bot(player_o, Level),
    config(Size, players(Px, _), F, G),
    config(Size, players(Px, bot(Level)), F, G)
) :-
    integer(Level),
    Level > 0,
    Level < 3.

response_config(
    human(player_x),
    config(Size, players(_, Po), F, G),
    config(Size, players(human, Po), Po, F, G)
).

response_config(
    human(player_o),
    config(Size, players(Px, _), F, G),
    config(Size, players(Px, human), F, G)
).

response_config(
    first_player(First),
    config(Size, Players, _, G),
    config(Size, Players, first_player(First), G)
) :-
    (First = player_x; First = player_o).

response_config(
    win_pieces(Goal),
    config(Size, Players, F, _),
    config(Size, Players, F, win_target(Goal))
) :-
    integer(Goal),
    Goal > 0.

do_menu_action(menu_state(_, Config), start_game) :-
    play_game(Config).

do_menu_action(menu_state(_, Config), open_menu(Menu)) :-
    open_menu(menu_state(Menu, Config)).

do_menu_action(menu_state(config, _), config(Config)) :-
    open_menu(menu_state(config, Config)).

do_menu_action(_, exit).

do_game_action(GameState, Config, move(Col, Row)) :-
    move(GameState, position(Col, Row), NewGameState),
    play_turn(NewGameState, Config).

do_game_action(GameState, Config, move(Col, Row)) :-
    GameState = game_state(Board, _, _, _, _),
    empty_cell(Board, position(Col, Row)),
    ColNum is Col + "a",
    RowNum is Row + 1,
    char_code(ColChar, ColNum),
    format('Move (~w, ~w) is not valid. It must be adjacent to the last move.\n', [ColChar, RowNum]),
    read_response(GameState, Action),
    do_game_action(GameState, Config, Action).

do_game_action(GameState, Config, move(Col, Row)) :-
    ColNum is Col + "a",
    RowNum is Row + 1,
    char_code(ColChar, ColNum),
    format('Move (~w, ~w) is not valid. You must play on an empty cell.\n', [ColChar, RowNum]),
    read_response(GameState, Action),
    do_game_action(GameState, Config, Action).

do_game_action(game_state(_, current_player(Player), _, _, _), Config, give_up) :-
    next_player(Player, Opponent),
    open_menu(menu_state(game_over(Opponent), Config)).
