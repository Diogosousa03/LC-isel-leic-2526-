:- module(main, [main/0, start/0, start_game/4]).

% modules
:- use_module('Board/board').
:- use_module('Game/game').
:- use_module('Game/turns').
:- use_module('Game/state'). 


main :- start.

start :-
    nl,
    write('========================='), nl,
    write('       Connect 4         '), nl,
    write('========================='), nl,
    game_init.

game_init :-
    empty_board(Board),
    print_board(Board),
    init_turn(P1, P2),
    Current = P1,
    start_game(Board, P1, P2, Current).


start_game(Board, P1, P2, Current) :-
    print_board(Board),
    write('Choose a column (1-7): '), nl,
    read(Col),

    Current = player(_, ColorPlay),   % Get color of current player

    (   insert(Board, Col, ColorPlay, NewBoard) ->
            true
    ;   write('Invalid move! Try again.'), nl,
        start_game(Board, P1, P2, Current)       % retry
    ),

    % --------------------------------------------
    % CHECK WIN CONDITION
    % --------------------------------------------
    (   game_check(NewBoard, Current) ->
        print_board(NewBoard),
        write('========================='), nl,
        write('      GAME OVER!         '), nl,
        write('  Player '), write(ColorPlay), write(' wins!'), nl,
        write('========================='), nl,
        !
    ;   stalemate_check(Board) ->
        next_turn(Current, P1, P2, Next),
        start_game(NewBoard, P1, P2, Next)
    ;   write('========================='), nl,
        write('      GAME OVER! STALEMATE        '), nl,
        write('========================='), nl,
        !
).

