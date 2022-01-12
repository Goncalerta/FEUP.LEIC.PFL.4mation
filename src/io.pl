

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
get_symbol(legal_move, '?').  
get_symbol(player_x, 'X').
get_symbol(player_o, 'O').

print_player(player_x) :- write('player X').
print_player(player_o) :- write('player O').

print_player_config(human) :- write('human').
print_player_config(bot(Level)) :- write('bot with level '), write(Level).

open_menu(MenuState) :-
    MenuState = menu_state(CurrentMenu, Config),
    display_menu(CurrentMenu, Config),
    read_response(MenuState).

play_game(Config) :-
    initial_state(Config, GameState),
    play_turn(GameState, Config).

% play_turn(GameState, Config) :-
%     %GameState = game_state(Board, current_player(Player), last_piece_played(Piece)),
%     game_over(GameState, Winner),
%     !, % TODO confirm if needed
%     % display_game(GameState, Winner),
%     % open_menu(menu_state(current(game_over, Winner), Config)).
%     write('END').

play_turn(GameState) :-
    %GameState = game_state(Board, current_player(Player), last_piece_played(Piece)),
    display_game(GameState),
    read_response(GameState).

read_response(GameState) :- 
    read_term(Response, [syntax_errors(quiet)]),
    !,
    process_response(GameState, Response).

read_response(GameState) :-
    write('Invalid option. Unable to understand term.\n'),
    read_response(GameState).

process_response(GameState, Response) :-
    ground(Response), 
    response(GameState, Response, Action), 
    !, 
    Action.

process_response(GameState, Response) :-
    ground(Response),
    !,
    format('Invalid option \'~w\'.\n', [Response]),
    read_response(GameState).

process_response(GameState, _) :-
    write('Invalid option. You may not use variables in your input.\n'),
    read_response(GameState).

display_menu(current(main), _) :-
    nl,nl,nl,
    print_banner(['Welcome to 4MATION!'], 4, 1, 6),
    nl,
    write('    |: play.\t\tStart a new game.\n'),
    write('    |: config.\t\tGame configurations.\n'),
    write('    |: rules.\t\tSee rules.\n'),
    write('    |: exit.\t\tQuit game.\n'),
    nl.

display_menu(current(rules), _) :-
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
    current(config), 
    config(
        size(Cols, Rows),
        player_x(Px), 
        player_o(Py), 
        first_player(First), 
        winning_length(Goal)
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
    write('Player O:         '), print_player_config(Py), nl,
    write('First to play:    '), print_player(First), nl,
    nl.

display_board_bar(Cols) :-
    margin(4),
    write('|'),
    print_n('---|', Cols).

display_board_row(Row, RowNum) :-
    length(Row, _),
    format(' ~|~` t~d~2+ ', [RowNum]),  % write number with leading spaces
    write('|'),
    maplist(get_symbol, Row, RowSymbols),
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

display_board_body(Rows) :-
    display_board_body(Rows, 1).
display_board_body([], _).
display_board_body([Row|Next], RowNum) :-
    display_board_row(Row, RowNum),
    nl,
    length(Row, Cols),
    display_board_bar(Cols),
    nl,
    NextRowNum is RowNum + 1,
    display_board_body(Next, NextRowNum).
    
display_board(Board) :-
    board_size(Board, size(Cols, _)),
    nl,
    display_board_header(Cols),
    display_board_body(Board).

display_game(GameState) :-
    GameState = game_state(Board, current_player(Player), last_piece_played(_)),
    display_board(Board),
    nl,
    write('It\'s '), print_player(Player), write('\'s turn. '),
    % informacao se e human ou bot
    % format('Get a row, column or diagonal of ~w consecutive pieces to win.\n', WinLen),
    nl,
    write('    |: move(column, row).     Put a piece on the given position.\n'),
    write('    column - character; row - positive integer (must be a valid move).\n\n'),
    write('    |: give_up.               Stop playing the game (the opponent automatically wins).\n').

response(
    menu_state(current(main), Config), 
    play,
    play_game(Config)
).

response(
    menu_state(current(main), Config), 
    config,
    open_menu(menu_state(current(config), Config))
).

response(
    menu_state(current(main), Config), 
    rules,
    open_menu(menu_state(current(rules), Config))
).

response(
    menu_state(current(main), _), 
    exit,
    write('Thank you for playing!\n')
).

response(
    game_state(B, C, L), 
    move(ColChar, RowNum),
    (
        char_code(ColChar, ColNum),
        Col is ColNum - "a",
        Row is RowNum - 1,
        write(Col), nl,
        move(game_state(B, C, L), position(Col, Row), NewGameState),
        play_turn(NewGameState)
    )
). % TODO CONSTRAIN SIZE

%move_action
% converte o move(...) do user em position(...)
% que pode ser num valid move, out of bounds move, invalid move etc

response(
    game_state(B, C, L), 
    give_up,
    halt % TODO
).

response(
    game_state(State), 
    give_up,
    give_up(game_state(State)) % TODO
).

response(
    menu_state(current(config), OldConfig), 
    Command,
    open_menu(menu_state(current(config), NewConfig))
) :- response_config(Command, OldConfig, NewConfig).

response(
    menu_state(current(config), Config), 
    back, 
    open_menu(menu_state(current(main), Config))
).

response_config(
    board_size(Cols, Rows), 
    config(_, Px, Py, F, G), 
    config(size(Cols, Rows), Px, Py, F, G)
) :-
    integer(Cols),
    integer(Rows),
    Cols > 0,
    Cols =< 26,
    Rows > 0,
    Rows =< 26.

response_config(
    win_pieces(Goal),
    config(Size, Px, Py, F, _),
    config(Size, Px, Py, F, winning_length(Goal))
) :-
    integer(Goal),
    Goal > 0.

response_config(
    bot(player_x, Level),
    config(Size, _, Py, F, G),
    config(Size, player_x(bot(Level)), Py, F, G)
) :-
    integer(Level),
    Level > 0,
    Level < 3.

response_config(
    bot(player_o, Level),
    config(Size, Px, _, F, G),
    config(Size, Px, player_o(bot(Level)), F, G)
) :-
    integer(Level),
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

response(
    menu_state(current(rules), Config), 
    back, 
    open_menu(menu_state(current(main), Config))
).
