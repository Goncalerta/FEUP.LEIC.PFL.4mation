:-use_module(library(lists)).
:-use_module(library(between)).
:-use_module(library(random)).
:-use_module(library(system)).

:- ensure_loaded('board').
:- ensure_loaded('game').
:- ensure_loaded('io').

% play/0
% Opens the main menu with the default game configuration.
play :- open_menu(
    menu_state(
        main,
        config(
            size(8, 8),
            players(human, human),
            first_player(player_x),
            win_target(4)
        )
    )
), write('Thank you for playing!\n').

% Opens the game in the state shown in initial.png
image(initial_png) :-
    play_turn(
        game_state(
            [
                [empty, empty, empty, empty, empty],
                [empty, empty, empty, empty, empty],
                [empty, empty, empty, empty, empty],
                [empty, empty, empty, empty, empty],
                [empty, empty, empty, empty, empty]
            ],
            current_player(player_x),
            last_move(none),
            players(human, bot(1)),
            win_target(4)
        ),
        config(
            size(5, 5),
            players(human, bot(1)),
            first_player(player_x),
            win_target(4)
        )
    ).

% Opens the game in the state shown in midgame.png
image(midgame_png) :-
    play_turn(
        game_state(
            [
                [empty, empty, player_o, empty, empty],
                [empty, empty, empty, player_x, player_o],
                [empty, player_x, empty, player_x, empty],
                [empty, empty, player_o, empty, empty],
                [empty, empty, empty, empty, empty]
            ],
            current_player(player_x),
            last_move(position(2, 0)),
            players(human, bot(1)),
            win_target(4)
        ),
        config(
            size(5, 5),
            players(human, bot(1)),
            first_player(player_x),
            win_target(4)
        )
    ).

% Opens the game in the state shown in gameover.png
image(gameover_png) :-
    play_turn(
        game_state(
            [
                [empty, player_o, player_o, empty, empty],
                [player_x, player_x, empty, player_x, player_o],
                [player_o, player_x, empty, player_x, empty],
                [player_o, player_x, player_o, empty, empty],
                [empty, player_x, empty, empty, empty]
            ],
            current_player(player_o),
            last_move(position(1, 4)),
            players(human, bot(1)),
            win_target(4)
        ),
        config(
            size(5, 5),
            players(human, bot(1)),
            first_player(player_x),
            win_target(4)
        )
    ).
