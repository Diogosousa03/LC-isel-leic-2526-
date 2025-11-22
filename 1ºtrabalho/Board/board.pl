

%Board representation and printing

:- module(board, [empty_board/1]).


empty_board([
    [empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty]
]).
print_board(Board) :-
    nl,
    write(' 1  2  3  4  5  6  7'), nl,
    write('---------------------'), nl,
    print_rows(Board),
    write('---------------------'), nl.