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

:- ensure_loaded('util').
:- ensure_loaded('board').
:- ensure_loaded('game').
:- ensure_loaded('io').

% play/0
play :- open_menu(
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
