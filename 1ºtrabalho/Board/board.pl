:- module(board, [empty_board/1, print_board/1]).

/* ============================================================
   empty_board(-Board)
   ------------------------------------------------------------
   Devolve o tabuleiro vazio de 6 linhas e 7 colunas.
   Cada célula contém o caractere ' ' (espaço) para indicar vazio.
   ============================================================ */
empty_board([
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ']
]).

/* ============================================================
   print_board(+Board)
   ------------------------------------------------------------
   Imprime o tabuleiro no ecrã:
   - Mostra os números das colunas (1 a 7).
   - Mostra separadores visuais para facilitar a leitura.
   ============================================================ */
print_board(Board) :-
    nl,
    write(' 1  2  3  4  5  6  7'), nl,   
    write('---------------------'), nl,
    print_rows(Board),             
    write('---------------------'), nl.

/* ============================================================
   print_rows(+Rows)
   ------------------------------------------------------------
   Imprime todas as linhas do tabuleiro, uma a uma.
   ============================================================ */
print_rows([]) :- !.
print_rows([H|T]) :-
    print_row(H),
    print_rows(T).

/* ============================================================
   print_row(+Row)
   ------------------------------------------------------------
   Imprime uma linha do tabuleiro:
   - Usa '|' para separar células.
   ============================================================ */
print_row(Row) :-
    write('|'),
    print_cells(Row),
    nl.

/* ============================================================
   print_cells(+Cells)
   ------------------------------------------------------------
   Imprime todas as células de uma linha:
   - Se for ' ' (vazia), imprime '_'
   - Caso contrário, imprime o valor (ex: 'r' ou 'y')
   ============================================================ */
print_cells([]) :- !.
print_cells([H|T]) :-
    ( H = ' ' -> write('_') ; write(H) ),  
    write('|'),
    print_cells(T).
