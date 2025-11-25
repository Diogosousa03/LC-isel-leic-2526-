%Game logic of the game

:-module(game, [insert/4,get_element/3]).



insert(Board, Index, Value, NewBoard):-
    first_empty_row(Board, ColIndex, RowIndex),   
    get_element(RowIndex, Board, OldRow),




get_element(1,[H|_], H).

get_element(Index,[_|T], Elem):-
    Index>1,
    Index1 is Index-1,
    get_element(Index1,T,Elem).


first_empty_row(Board, ColIndex, RowIndex) :-
    reverse(Board, RevBoard),              
    first_empty_row_bottom(RevBoard, ColIndex, 1, RevIndex),
    length(Board, Len),
    RowIndex is Len - RevIndex + 1.       


first_empty_row_bottom([Row|_], ColIndex, CurrentIndex, CurrentIndex) :-
    get_element(ColIndex, Row, Elem),
    Elem == ' '.


first_empty_row_bottom([_|T], ColIndex, CurrentIndex, RowIndex) :-
    NextIndex is CurrentIndex + 1,
    first_empty_row_bottom(T, ColIndex, NextIndex, RowIndex).




