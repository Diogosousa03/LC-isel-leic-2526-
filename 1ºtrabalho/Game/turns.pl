:- module(turns, [init_turn/2, next_turn/4]).

/* ============================================================
   available_pieces(-List)
   ------------------------------------------------------------
   Lista das peças disponíveis no jogo.
   Cada jogador escolhe uma peça antes do jogo começar.
   ============================================================ */
available_pieces([r,y,b,w]).

/* ============================================================
   init_turn(-P1, -P2)
   ------------------------------------------------------------
   Inicializa o turno definindo os jogadores:

   - Mostra as peças disponíveis.
   - Jogador 1 escolhe uma peça.
   - Remove essa peça da lista.
   - Jogador 2 escolhe uma peça das restantes.
   - Cria estruturas player(ID, Piece) para ambos.
   ============================================================ */
init_turn(P1, P2) :- 
    available_pieces(List1),
    write('Player 1, choose a piece:'), write(List1), nl,
    read(Piece1),
    P1 = player(1, Piece1),

    % Remove a peça escolhida pelo jogador 1 da lista
    remove_available_pieces(Piece1, List1, List2),

    write('Player 2, choose a piece:'), write(List2), nl,
    read(Piece2),
    P2 = player(2, Piece2).

/* ============================================================
   remove_available_pieces(+Piece, +List, -NewList)
   ------------------------------------------------------------
   Remove um elemento específico da lista.

   - Caso o primeiro elemento seja a peça procurada,
     devolve simplesmente a cauda.
   - Caso contrário, mantém o elemento atual e continua a busca.
   ============================================================ */
remove_available_pieces(Piece, [Piece|Tail], Tail).
remove_available_pieces(Piece, [H|T], [H|T1]) :-
    remove_available_pieces(Piece, T, T1).

/* ============================================================
   next_turn(+Current, +P1, +P2, -Next)
   ------------------------------------------------------------
   Alterna o turno entre jogador 1 e jogador 2.

   - Se o jogador atual for P1 → próximo é P2
   - Caso contrário → próximo é P1
   ============================================================ */
next_turn(Current, P1, P2, Next) :-
    (Current = P1 ->
        Next = P2
    ;
        Next = P1
    ).
