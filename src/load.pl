:-use_module(library(lists)).
:-use_module(library(between)).
:-use_module(library(random)).
:-use_module(library(system)).

:- ensure_loaded('board').
:- ensure_loaded('game').
:- ensure_loaded('io').

% play/0
play :- open_menu(
    menu_state(
        main,
        config(
            size(8, 8),
            player_x(human),
            player_o(human),
            first_player(player_x),
            win_target(4)
        )
    )
), write('Thank you for playing!\n').
