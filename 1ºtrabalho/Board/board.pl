:- module(board, [empty_board/1, print_board/1]).

empty_board([
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ']
]).

print_board(Board) :-
    nl,
    write(' 1  2  3  4  5  6  7'), nl,  % two spaces for alignment
    write('---------------------'), nl,
    print_rows(Board),
    write('---------------------'), nl.

print_rows([]) :- !.
print_rows([H|T]) :-
    print_row(H),
    print_rows(T).

print_row(Row) :-
    write('|'),
    print_cells(Row),
    nl.

print_cells([]) :- !.
print_cells([H|T]) :-
    ( H = ' ' -> write('_') ; write(H) ),
    write('|'),
    print_cells(T).
