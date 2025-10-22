%Agente Desmond Doss
%Dimensão do campo de guerra
campo(15,15).

%Posição dos objetos
agente(pos(0, 0)).
tendas_medicas([pos(1,13), pos(13,1)]).
estacoes_recarga([pos(0,8), pos(7,13), pos(8,2), pos(13,5)]).
soldados_feridos([pos(7,2), pos(5,4), pos(7,8), pos(11,5), pos(13,12)]).
bombas_armadas([pos(2,4), pos(3,11), pos(4,13), pos(5,1), pos(5,8), pos(8,4), pos(10,6), pos(12,6), pos(12,12), pos(14,12)]).
soldados_inimigos([pos(0,9), pos(2,6), pos(2,8), pos(4,4), pos(5,3), pos(5,5), pos(6,10), pos(8,8), pos(8,13), pos(9,2), pos(11,8), pos(13,10)]).
tanques([pos(8,9), pos(8,10), pos(9,9), pos(9,10), pos(11,3), pos(11,4), pos(12,3), pos(12,4)]).

% Regras de Movimentação (Pos_Atual, NovaPosicao)
mover_norte(pos(X,Y), pos(X, Y1)) :- Y1 is Y - 1.
mover_sul(pos(X,Y), pos(X, Y1))   :- Y1 is Y + 1.
mover_leste(pos(X,Y), pos(X1, Y)) :- X1 is X + 1.
mover_oeste(pos(X,Y), pos(X1, Y)) :- X1 is X - 1.

% Verifica se posição está dentro dos limites do campo
posicao_valida(pos(X,Y)) :-
    campo(Largura, Altura),
    X >= 0, X < Largura,
    Y >= 0, Y < Altura.

% Identifica se uma posição é bloqueada e por qual tipo
posicao_bloqueada_tipo(Pos, tanque) :-
    tanques(T), member(Pos, T).
posicao_bloqueada_tipo(Pos, soldado) :-
    soldados_inimigos(I), member(Pos, I).
posicao_bloqueada_tipo(Pos, bomba) :-
    bombas_armadas(B), member(Pos, B).

% Predicado simples indicando bloqueio (sem detalhar o tipo)
posicao_bloqueada(Pos) :-
    posicao_bloqueada_tipo(Pos, _).

% Validação única: posição válida e não bloqueada
validar_posicao(Pos) :-
    posicao_valida(Pos),
    \+ posicao_bloqueada(Pos).

:- dynamic agente/1, energia/1, carregando/1, soldados_feridos/1, bombas_armadas/1.

% inicializações (não duplicam se já houver fatos no arquivo)
:- ( current_predicate(agente/1) -> true ; assertz(agente(pos(0,0))) ).
:- ( current_predicate(energia/1) -> true ; assertz(energia(100)) ).
:- ( current_predicate(carregando/1) -> true ; assertz(carregando(false)) ).
% soldados_feridos/1 e bombas_armadas/1 já existem no arquivo; como foram declarados, current_predicate será true e não serão sobrescritos.

% Estado composto do agente
estado_agente(Pos, Energia, Carregando, SoldadosRestantes, BombasRestantes) :-
    agente(Pos),
    energia(Energia),
    carregando(Carregando),
    soldados_feridos(SoldadosRestantes),
    bombas_armadas(BombasRestantes).

% Consultas rápidas
posicao_agente(Pos) :- agente(Pos).
nivel_energia(E) :- energia(E).
esta_carregando(true)  :- carregando(true).
esta_carregando(false) :- carregando(false).
soldados_restantes(L) :- soldados_feridos(L).
bombas_restantes(L)   :- bombas_armadas(L).

% Modificadores simples
set_posicao_agente(NovaPos) :-
    retractall(agente(_)),
    assertz(agente(NovaPos)).

% garante limites de energia 0..100
set_energia(N) :-
    integer(N),
    N1 is max(0, N),
    ( N1 > 100 -> N2 = 100 ; N2 = N1 ),
    retractall(energia(_)),
    assertz(energia(N2)).

gastar_energia(N) :-
    integer(N), N > 0,
    energia(E),
    E >= N,
    E2 is E - N,
    retractall(energia(_)),
    assertz(energia(E2)).

recuperar_energia(N) :-
    integer(N), N > 0,
    energia(E),
    E2 is E + N,
    ( E2 > 100 -> E3 = 100 ; E3 = E2 ),
    retractall(energia(_)),
    assertz(energia(E3)).

% recarregar totalmente se estiver em estação de recarga
recarregar_estacao :-
    agente(Pos),
    estacoes_recarga(Ests),
    member(Pos, Ests),
    set_energia(100).

% adjacência ortogonal (N/S/E/O)
adjacente(pos(X,Y), pos(X1,Y1)) :-
    DX is abs(X - X1),
    DY is abs(Y - Y1),
    DX + DY =:= 1.

% remove todos os elementos de Rem da lista Orig, produzindo Res
remove_todos(Orig, [], Orig).
remove_todos(Orig, [H|T], Res) :-
    (   select(H, Orig, Mid)
    ->  remove_todos(Mid, T, Res)
    ;   remove_todos(Orig, T, Res)
    ).

% remove todas as ocorrências de Pos em Lista -> New
remove_pos_da_lista(Pos, Lista, New) :-
    findall(P, (member(P, Lista), P \= Pos), New).

% movimento unitário em 4 direções (apenas N/S/E/O)
direction_move(norte, pos(X,Y), pos(X, Y1)) :- Y1 is Y + 1.
direction_move(sul,   pos(X,Y), pos(X, Y1)) :- Y1 is Y - 1.
direction_move(leste, pos(X,Y), pos(X1, Y)) :- X1 is X + 1.
direction_move(oeste, pos(X,Y), pos(X1, Y)) :- X1 is X - 1.

% Mover em uma direção (norte|sul|leste|oeste) e desarmar bombas ortogonalmente adjacentes à nova posição.
% Regras:
%  - energia é 0..100; com 0 não pode mover
%  - custo de movimento = ceil(E0 * 0.025)
%  - custo de desarme por bomba = ceil(E0 * 0.05)
%  - custos calculados sobre energia antes da operação (E0)
%  - desarme só de bombas ortogonalmente adjacentes
%  - se estiver carregando um ferido quando ocorrer desarme, o estado carregando é desligado e
%    garante-se que o ferido correspondente não volte para a lista de feridos
mover_dir(Direction) :-
    member(Direction, [norte,sul,leste,oeste]),
    agente(OldPos),
    direction_move(Direction, OldPos, NewPos),
    validar_posicao(NewPos),
    energia(E0),
    E0 > 0,                      % não pode mover se estiver com 0%
    % custo de movimento (inteiro, ceil)
    MoveCostFloat is E0 * 0.025,
    MoveCost is ceiling(MoveCostFloat),
    bombas_armadas(Bs),
    % identificar bombas ortogonalmente adjacentes à NewPos
    findall(B, (member(B, Bs), adjacente(NewPos, B)), Adj),
    length(Adj, NAdj),
    ( NAdj =:= 0
    ->  % apenas movimento
        TotalCost is MoveCost,
        E0 >= TotalCost,
        set_posicao_agente(NewPos),
        gastar_energia(TotalCost)
    ;  % há bomba(s) adjacente(s)
        ( carregando(true)
        ->  % se estiver carregando, NÃO desarmar: apenas mover (se possível)
            TotalCost is MoveCost,
            E0 >= TotalCost,
            set_posicao_agente(NewPos),
            gastar_energia(TotalCost)
        ;  % não está carregando -> pode desarmar como antes
            DisarmCostFloat is E0 * 0.05,
            DisarmCost is ceiling(DisarmCostFloat),
            TotalDisarm is DisarmCost * NAdj,
            TotalCost is MoveCost + TotalDisarm,
            E0 >= TotalCost,
            % remover bombas adjacentes
            remove_todos(Bs, Adj, NewBs),
            retractall(bombas_armadas(_)),
            assertz(bombas_armadas(NewBs)),
            % se estava carregando um soldado (aqui não acontece), garante-se estado consistente
            ( carregando(true) ->
                agente(Apos),
                soldados_feridos(SF),
                remove_pos_da_lista(Apos, SF, SF2),
                retractall(soldados_feridos(_)),
                assertz(soldados_feridos(SF2)),
                set_carregando(false)
            ; true ),
            % mover e descontar energia total
            set_posicao_agente(NewPos),
            gastar_energia(TotalCost)
        )
    ).

% define o estado de carregando (true/false)
set_carregando(B) :-
    (B == true ; B == false),
    retractall(carregando(_)),
    assertz(carregando(B)).

% Pegar um soldado ferido (se estiver na mesma posição e não estiver carregando)
pegar_soldado :-
    carregando(false),
    agente(Pos),
    soldados_feridos(L),
    member(Pos, L),
    select(Pos, L, NewL),
    retractall(soldados_feridos(_)),
    assertz(soldados_feridos(NewL)),
    set_carregando(true).

% Soltar soldado: se estiver em tenda médica, salva (remove de vez);
% caso contrário, repõe na posição atual se não já presente.
soltar_soldado :-
    carregando(true),
    agente(Pos),
    (   tendas_medicas(Ts), member(Pos, Ts)
    ->  % soltar na tenda (soldado salvo)
        set_carregando(false)
    ;   % soltar no chão: adicionar de volta à lista de soldados feridos se não já presente
        soldados_feridos(L),
        ( member(Pos, L) -> true ; retractall(soldados_feridos(_)), assertz(soldados_feridos([Pos|L])) ),
        set_carregando(false)
    ).

% Pegar bomba se estiver na mesma posição do agente
pegar_bomba :-
    agente(Pos),
    bombas_armadas(Bs),
    member(Pos, Bs),
    select(Pos, Bs, NewBs),
    retractall(bombas_armadas(_)),
    assertz(bombas_armadas(NewBs)),
    !.

% Soltar bomba na posição atual (se não duplicar)
soltar_bomba :-
    agente(Pos),
    bombas_armadas(Bs),
    ( member(Pos, Bs) -> true ; retractall(bombas_armadas(_)), assertz(bombas_armadas([Pos|Bs])) ).



