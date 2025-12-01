:- module(game, [insert/4, get_element/3]).

/* ============================================================
   get_element(+Index, +List, -Elem)
   ------------------------------------------------------------
   Retorna o elemento de uma lista dado um índice (1-based).

   - Caso base: se Index = 1, devolve o head da lista.
   - Caso recursivo: decrementa o índice e procura na cauda.
   ============================================================ */
get_element(1,[H|_], H).
get_element(Index, [_|T], Elem) :-
    Index > 1,
    Index1 is Index - 1,
    get_element(Index1, T, Elem).

/* ============================================================
   replace(+List, +Index, +X, -NewList)
   ------------------------------------------------------------
   Substitui o elemento no índice dado por um novo valor X.

   - Caso base: se Index = 1, substitui o head da lista.
   - Caso recursivo: mantém o head e continua na cauda.
   ============================================================ */
replace([_|T], 1, X, [X|T]).
replace([H|T], Index, X, [H|R]) :-
    Index > 1,
    Index1 is Index - 1,
    replace(T, Index1, X, R).

/* ============================================================
   insert(+Board, +ColIndex, +Piece, -NewBoard)
   ------------------------------------------------------------
   Insere uma peça numa coluna do tabuleiro:

   - Encontra a primeira linha vazia na coluna (pela gravidade).
   - Substitui o valor na linha correta.
   - Retorna o novo tabuleiro atualizado.
   ============================================================ */
insert(Board, ColIndex, Piece, NewBoard) :-
    first_empty_row(Board, ColIndex, RowIndex),
    get_element(RowIndex, Board, OldRow),
    replace(OldRow, ColIndex, Piece, NewRow),
    replace(Board, RowIndex, NewRow, NewBoard).

/* ============================================================
   first_empty_row(+Board, +ColIndex, -RowIndex)
   ------------------------------------------------------------
   Encontra a primeira linha vazia numa coluna, considerando
   que as peças caem para baixo (começando da última linha).

   - Reverte o tabuleiro para percorrer de baixo para cima.
   - Calcula o índice correto na tabela original.
   ============================================================ */
first_empty_row(Board, ColIndex, RowIndex) :-
    reverse(Board, RevBoard),
    first_empty_row_bottom(RevBoard, ColIndex, 1, RevIndex),
    length(Board, Len),
    RowIndex is Len - RevIndex + 1.

/* ============================================================
   first_empty_row_bottom(+Rows, +ColIndex, +CurrentIndex, -RowIndex)
   ------------------------------------------------------------
   Procura recursivamente a primeira linha vazia a partir do fundo:

   - Caso base: se o elemento na coluna é ' ' → devolve o índice atual.
   - Caso recursivo: incrementa CurrentIndex e continua na lista.
   ============================================================ */
first_empty_row_bottom([Row|_], ColIndex, CurrentIndex, CurrentIndex) :-
    nonvar(ColIndex),
    get_element(ColIndex, Row, Elem),
    Elem == ' '.

first_empty_row_bottom([_|T], ColIndex, CurrentIndex, RowIndex) :-
    NextIndex is CurrentIndex + 1,
    first_empty_row_bottom(T, ColIndex, NextIndex, RowIndex).
