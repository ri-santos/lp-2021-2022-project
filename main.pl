extrai_ilhas_linha(N, L, I) :-
    extrai_ilhas_linha(N, L, 1, [], I).

extrai_ilhas_linha(_, [], _, Ilhas, Ilhas).

extrai_ilhas_linha(N, [C|R], Cont, Ilhas, I) :-
    C =\= 0,
    append(Ilhas, [ilha(C, (N, Cont))], NovoIlhas),
    Cont1 is Cont + 1,
    extrai_ilhas_linha(N, R, Cont1, NovoIlhas, I).

extrai_ilhas_linha(N, [_|R], Cont, Ilhas, I) :-
    Cont1 is Cont + 1,
    extrai_ilhas_linha(N, R, Cont1, Ilhas, I).


ilhas(Puz, Ilhas) :-
    ilhas(Puz, 1, [], Ilhas).

ilhas([], _, Acc, Acc).

ilhas([L|R], N, Acc, Ilhas) :-
    extrai_ilhas_linha(N, L, I),
    append(Acc, I, NovoAcc),
    N1 is N+1,
    ilhas(R, N1, NovoAcc, Ilhas).


vizinhas(Ilhas, ilha(_, (L, C)), Vizinhas) :-
    findall(ilha(Pon, (L, Col)), (member(ilha(Pon, (L, Col)), Ilhas), Col < C), IlhasLinhaEsq),
    findall(ilha(Pon, (L, Col)), (member(ilha(Pon, (L, Col)), Ilhas), Col > C), IlhasLinhaDir),
    findall(ilha(Pon, (Lin, C)), (member(ilha(Pon, (Lin, C)), Ilhas), Lin < L), IlhasColCima),
    findall(ilha(Pon, (Lin, C)), (member(ilha(Pon, (Lin, C)), Ilhas), Lin > L), IlhasColBaixo),
    ((length(IlhasLinhaEsq, NEsq), nth1(NEsq, IlhasLinhaEsq, Esq)); Esq = []),
    (nth1(1, IlhasLinhaDir, Dir) ; Dir = []),
    ((length(IlhasColCima, NCima), nth1(NCima, IlhasColCima, Cima)) ; Cima = []),
    (nth1(1, IlhasColBaixo, Baixo) ; Baixo = []),
    Viz = [Cima, Esq, Dir, Baixo], flatten(Viz, Vizinhas).


estado(Ilhas, Estado) :-
    estado_aux(Ilhas, Ilhas, [], Estado).
estado_aux(_, [], Entradas, Entradas).
estado_aux(Ilhas, [C|R], Entradas, Estado):-
    vizinhas(Ilhas, C, V),
    append(Entradas, [[C, V, []]], NEnt),
    estado_aux(Ilhas, R, NEnt, Estado).


posicoes_entre((L1, C1), (L2, C2), Pos) :-
    (L1 == L2, C1 < C2, C11 is C1 + 1, C21 is C2 - 1,
    findall((L1, C), between(C11, C21, C), Pos));
    (L1 == L2, C1 > C2, C11 is C1 - 1, C21 is C2 + 1,
    findall((L1, C), between(C21, C11, C), Pos));
    (C1 == C2, L1 < L2, L11 is L1 + 1, L21 is L2 - 1, 
    findall((L, C1), between(L11, L21, L), Pos));
    (C1 == C2, L1 > L2, L11 is L1 - 1, L21 is L2 + 1, 
    findall((L, C1), between(L21, L11, L), Pos)).


cria_ponte((L1, C1), (L2, C2), Ponte) :-
    (L1 < L2, Ponte = ponte((L1, C1), (L2, C2))) ;
    (L1 == L2, C1 =< C2, Ponte = ponte((L1, C1), (L2, C2)));
    Ponte = ponte((L2, C2), (L1, C1)).


caminho_livre_aux(Pos,C) :- not(member(C, Pos)).
caminho_livre(I1, I2, Pos, ilha(_, (L1, C1)), ilha(_, (L2, C2))) :-
    posicoes_entre((L1, C1), (L2, C2), Entre),
    (subset(Entre, Pos), member((L1, C1), [I1, I2]), member((L2, C2), [I1,I2]);
    maplist(caminho_livre_aux(Pos), Entre)).


actualiza_vizinhas_entrada(P1, P2, Posicoes, [I, V, P], NovaEnt) :-
    include(caminho_livre(P1, P2, Posicoes, I), V, VizActual),
    NovaEnt = [I, VizActual, P].


actualiza_vizinhas_apos_pontes(Estado, P1, P2, NovaEnt) :-
    posicoes_entre(P1, P2, Pos),
    maplist(actualiza_vizinhas_entrada(P1, P2, Pos), Estado, NovaEnt).


ilhas_terminadas(Estado, Ilhas_term) :-
    findall(ilha(N, P), (member([ilha(N, P), _, L], Estado), N \== 'X', length(L, N)), Ilhas_term).


tira_ilhas_terminadas_entrada_aux(L, V) :- member(V, L).
tira_ilhas_terminadas_entrada(Ilhas_term, [I, V, P], NovaEnt) :-
    exclude(tira_ilhas_terminadas_entrada_aux(Ilhas_term), V, NovaV),
    NovaEnt = [I, NovaV, P].


tira_ilhas_terminadas(Estado, Ilhas_term, NovoEstado) :-
    maplist(tira_ilhas_terminadas_entrada(Ilhas_term), Estado, NovoEstado).


marca_ilhas_terminadas_entrada(Ilhas_term, [ilha(Pon, Pos), V, L], NovaEntrada) :-
    (member(ilha(Pon, Pos), Ilhas_term), NovaEntrada = [ilha('X', Pos), V, L]) ; NovaEntrada = [ilha(Pon, Pos), V, L].


marca_ilhas_terminadas(Estado, Ilhas_term, NovoEstado) :-
    maplist(marca_ilhas_terminadas_entrada(Ilhas_term), Estado, NovoEstado).


trata_ilhas_terminadas(Estado, NovoEstado) :-
    ilhas_terminadas(Estado, Ilhas_term),
    tira_ilhas_terminadas(Estado, Ilhas_term, TiraIlhas),
    marca_ilhas_terminadas(TiraIlhas, Ilhas_term, NovoEstado).


junta_pontes_aux(Ilha1, Ilha2, Ponte, N_pontes, [ilha(P, Pos), V, Pon], NovaEnt) :-
    ((Ilha1 == ilha(P, Pos); Ilha2 == ilha(P, Pos)),
    ((N_pontes == 1, append(Pon, [Ponte], NovaPon));
    (N_pontes == 2, append(Pon, [Ponte, Ponte], NovaPon))),
    NovaEnt = [ilha(P, Pos), V, NovaPon]);
    NovaEnt = [ilha(P, Pos), V, Pon].

junta_pontes(Estado, Num_pontes, ilha(P1, Pos1) , ilha(P2, Pos2), Novo_Estado) :-
    cria_ponte(Pos1, Pos2, Ponte),
    actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, NovoEstado),
    maplist(junta_pontes_aux(ilha(P1, Pos1), ilha(P2, Pos2), Ponte, Num_pontes), NovoEstado, EstadoPon),
    trata_ilhas_terminadas(EstadoPon, Novo_Estado).