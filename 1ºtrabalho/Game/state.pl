:- module(state, [game_check/2, stalemate_check/1]).

:- use_module('game').     % For get_element/3


/* ============================================================
   GAME CHECK
   Checks if the current player has won.
   Current = player(Number, Color)
   ============================================================ */

game_check(Board, player(_, Color)) :-
    win_horizontal(Board, Color);
    win_vertical(Board, Color);
    win_diagonal(Board, Color).


/* ============================================================
   STALEMATE CHECK
   True if the board has no empty spaces
   ============================================================ */
stalemate_check(Board) :-
    member(Row, Board),
    member(' ', Row).


/* ============================================================
   1. HORIZONTAL WIN
   ============================================================ */
win_in_line(Line, Color) :-
    sublist([Color, Color, Color, Color], Line).

win_horizontal(Board, Color) :-
    member(Row, Board),
    win_in_line(Row, Color).


/* -------- sublist + conc helper -------- */
sublist(S, L) :-
    conc(_, Tail, L),
    conc(S, _, Tail).

conc([], L, L).
conc([H|T], L2, [H|R]) :-
    conc(T, L2, R).


/* ============================================================
   2. VERTICAL WIN
   ============================================================ */
win_vertical(Board, Color) :-
    transpose(Board, Columns),
    win_horizontal(Columns, Color).

transpose([[]|_], []) :- !.
transpose(Matrix, [Column|Rest]) :-
    first_column(Matrix, Column, RemainingRows),
    transpose(RemainingRows, Rest).

first_column([], [], []).
first_column([[H|T]|Rest], [H|Hs], [T|Ts]) :-
    first_column(Rest, Hs, Ts).


/* ============================================================
   3. DIAGONAL WIN
   ============================================================ */
win_diagonal(Board, Color) :-
    diagonal_right(Board, Color);
    diagonal_left(Board, Color).

diagonal_right(Board, Color) :-
    diagonal_lines(Board, 1, 1, 1, 1, Color).

diagonal_left(Board, Color) :-
    diagonal_lines(Board, 1, 7, 1, -1, Color).

/* Generate all diagonal paths and test for 4 in a row */
diagonal_lines(Board, Row, Col, DR, DC, Color) :-
    diagonal_from(Board, Row, Col, DR, DC, Diag),
    win_in_line(Diag, Color).

/* Collect diagonal elements safely */
diagonal_from(Board, Row, Col, DR, DC, [Elem|Rest]) :-
    safe_get(Board, Row, Col, Elem),
    R2 is Row + DR,
    C2 is Col + DC,
    diagonal_from(Board, R2, C2, DR, DC, Rest).
diagonal_from(_, Row, Col, _, _, []) :-
    Row < 1 ; Row > 6 ; Col < 1 ; Col > 7.

/* Safe element access */
safe_get(Board, Row, Col, Elem) :-
    Row >= 1, Row =< 6,
    Col >= 1, Col =< 7,
    get_element(Row, Board, Line),
    get_element(Col, Line, Elem).
