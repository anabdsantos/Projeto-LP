% 109260-Ana Claudia Santos 
:- use_module(library(clpfd)). % para poder usar transpose/2
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % ver listas completas
:- ['puzzlesAcampar.pl']. 

/*vizinhanca((L, C), Vizinhanca) e verdade se Vizinhanca e uma lista ordenada de cima
para baixo e da esquerda para a direita, sem elementos repetidos, com as coordenadas 
das posicoes imediatamente acima, imediatamente a esquerda, imediatamente a direita e 
imediatamente abaixo da coordenada (L, C).
*/
vizinhanca((L,C),Vizinhanca):-
    LB is L-1,
    CE is C-1,
    CD is C+1,
    LA is L+1,
    Vizinhanca=[(LB,C),(L,CE),(L,CD),(LA,C)].


/* vizinhancaAlargada((L, C), VizinhancaAlargada) e verdade se VizinhancaAlargada e
uma lista ordenada de cima para baixo e da esquerda para a direita, sem elementos repetidos,
com as coordenadas anteriores e ainda as diagonais da coordenada (L, C).
*/
vizinhancaAlargada((L,C),VizinhancaAlargada):-
    LB is L-1,
    CE is C-1,
    CD is C+1,
    LA is L+1,
    VizinhancaAlargada=[(LB,CE),(LB,C),(LB,CD),(L,CE),(L,CD),(LA,CE),(LA,C),(LA,CD)].


/* 
todasCelulas(Tabuleiro, TodasCelulas) e verdade se TodasCelulas e uma lista ordenada 
de cima para baixo e da esquerda para a direita, sem elementos repetidos, com todas as
coordenadas do tabuleiro Tabuleiro.
*/
todasCelulas(Tabuleiro,TodasCelulas):-
    todasCelulasAuxiliar(Tabuleiro,1,TodasCelulas).
% Predicado auxiliar para encontrar todas as coordenadas correspondentes ao tabuleiro 
todasCelulasAuxiliar([],_,[]).
todasCelulasAuxiliar([Linha|RestoTabuleiro],LinhaAtual,TodasCelulas):-
    colunas(Linha,1,LinhaAtual,Colunas),
    Linha2 is LinhaAtual+1, % Atualizacao da linha
    todasCelulasAuxiliar(RestoTabuleiro,Linha2,RestoTodasCelulas),
    append(Colunas,RestoTodasCelulas,TodasCelulas).
% Predicado auxiliar extra para a cada linha associar as colunas existentes
colunas([],_,_,[]).
colunas([_|RestoLinha],Coluna,LinhaAtual,[(LinhaAtual,Coluna)|RestoCelulas]):-
    Coluna1 is Coluna+1, % Atualizacao das colunas
    colunas(RestoLinha,Coluna1,LinhaAtual,RestoCelulas).


/* todasCelulas(Tabuleiro, TodasCelulas, Objecto) e verdade se TodasCelulas e uma
lista ordenada de cima para baixo e da esquerda para a direita, sem elementos repetidos,
com todas as coordenadas do tabuleiro Tabuleiro em que existe um objecto do tipo Objecto
(neste contexto (tal como no anterior) objecto e uma tenda (t), relva (r), arvore (a) ou 
ainda uma variavel (por exemplo X), para indicar os espacos nao preenchidos). 
*/
todasCelulas(Tabuleiro, TodasCelulas, Objeto) :-
    findall((Linha, Coluna), (
        nth1(Linha, Tabuleiro, LinhaTabuleiro),
        nth1(Coluna, LinhaTabuleiro, Elemento),
        (   % Quando o objeto nao e uma variavel, e verificada a igualdade
            (nonvar(Objeto), Elemento == Objeto) ;
            % Quando o objeto e uma variavel, o predicado verifica tambem o elemento
            (var(Objeto),(var(Elemento)))
        )
        ), TodasCelulas).


/*
calculaObjectosTabuleiro(Tabuleiro, ContagemColunas, ContagemColunas, Objecto) e
verdade se Tabuleiro for um tabuleiro, Objecto for o tipo de objecto que se procura, e
ContagemColunas e ContagemColunas forem, respectivamente, listas com o numero desses
objectos por linha e por coluna
*/

calculaObjectosTabuleiro(Tabuleiro,ContagemLinhas,ContagemColunas,Objeto):-
    calculaColuna(Tabuleiro,Objeto,ContagemLinhas),
    transpose(Tabuleiro,Colunas), % Para ser possivel fazer a contagem por coluna
    calculaColuna(Colunas,Objeto,ContagemColunas).
% Predicado auxiliar que vai adicionando a uma lista o numero de objetos encontrado por linha
calculaColuna([],_,[]).
calculaColuna([Linha|RestoTabuleiro],Objeto1,[Count|Contagem]):-
    calcula(Objeto1,Linha,Count),
    calculaColuna(RestoTabuleiro,Objeto1,Contagem).
% Predicado auxiliar que encontra todos os membros de uma lista que sejam iguais ao objeto dado
calcula(Objeto1,Lista,N):-
    findall(1,(
        (member(Elemento,Lista), 
        Elemento==Objeto1); % Quando o objeto nao e uma variavel, e verificada a igualdade
        (member(Elemento,Lista), var(Elemento),var(Objeto1))
        ) % Quando o objeto e uma variavel, o predicado verifica tambem o elemento
    ,N2),
    length(N2,N).


/*
celulaVazia(Tabuleiro, (L, C)) e verdade se Tabuleiro for um tabuleiro que nao tem
nada ou tem relva nas coordenadas (L, C). De notar que se as coordenadas nao fizerem parte
do tabuleiro, o predicado nao deve falhar.
*/
celulaVazia(Tabuleiro, (L, C)) :-
    % Verifica se a coordenada esta dentro dos limites do tabuleiro
    % O predicado nao falha caso isso nao se verifique 
    \+(coordenadaDentroDosLimitesTabuleiro(L, C, Tabuleiro)),!;
    (todasCelulas(Tabuleiro, TD, X),
    ((var(X),member((L,C),TD)))); % Verifica se a celula e uma variavel
    (todasCelulas(Tabuleiro,TD2,r),member((L,C),TD2)). % Verifica se a celula e relva
% Predicado auxiliar que garante os limites do tabuleiro
coordenadaDentroDosLimitesTabuleiro(L, C, Tabuleiro) :-
    length(Tabuleiro, Linhas), % Obtem o numero de linhas no tabuleiro
    between(1, Linhas, L),
    nth1(L, Tabuleiro, Linha),
    length(Linha, Colunas), % Obtem o numero de colunas no tabuleiro
    between(1, Colunas, C).


/*
insereObjectoCelula(Tabuleiro, TendaOuRelva, (L, C)) e verdade se Tabuleiro e um
tabuleiro e (L, C) sao as coordenadas onde queremos inserir o objecto TendaOuRelva.
*/
insereObjectoCelula(Tabuleiro,TendaOuRelva,(L,C)):-
    todasCelulas(Tabuleiro,TodasCelulas,_),
    ( member((L,C),TodasCelulas), % Verifica se a coordenada esta vazia (nao tem relva)
    maplist(nth1(C),Tabuleiro,Linha), % Obtem a coluna C do tabuleiro
    nth1(L,Linha,TendaOuRelva), % Modifica o elemento L da coluna C
    Tabuleiro=Tabuleiro);
    Tabuleiro=Tabuleiro. % Se a coordenada nao esta vazia mantem o tabuleiro inalterado


/*
insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, (L, C1), (L, C2)) e verdade se Tabuleiro 
e um tabuleiro, e (L, C1) e (L, C2) sao as coordenadas, na Coluna L, entre as quais (incluindo) 
se insere o objecto TendaOuRelva.
*/
insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, (L, C1), (L, C2)):-
    nth1(L,Tabuleiro,Linha),
    altera(Linha, C1, C2, TendaOuRelva, 1, NovaLinha),
    substitui(Tabuleiro, NovaLinha, L, Tabuleiro).
% Predicado auxiliar para modificar as colunas
altera([], _, _, _, _, []).
altera([Elemento|Resto], C1, C2, TendaOuRelva, Indice, [NovoElemento|NovaResto]) :-
    (var(Elemento),Indice >= C1, Indice =< C2, NovoElemento = TendaOuRelva),
    IndiceNovo is Indice + 1,
    altera(Resto, C1, C2, TendaOuRelva, IndiceNovo, NovaResto).
altera([Elemento2|Resto],C1,C2,TendaOuRelva,Indice,[NovoElemento2|NovaResto]):-
    ((Indice<C1;Indice>C2);nonvar(Elemento2)),NovoElemento2=Elemento2,
    IndiceNovo2 is Indice + 1, % Atualizacao da posicao
    altera(Resto,C1,C2,TendaOuRelva,IndiceNovo2,NovaResto).
% Predicado auxiliar que substitui as colunas no proprio tabuleiro
substitui([], _, _, []).
substitui([_|Resto], NovaLinha, 1, [NovaLinha|Resto]).
substitui([LinhaAtual|Resto], NovaLinha, C, [LinhaAtual|NovoResto]) :-
    C > 1,
    C1 is C - 1, % Atualizacao da coluna
    substitui(Resto, NovaLinha, C1, NovoResto).


/*
relva(Puzzle) e verdade se Puzzle e um puzzle que, apos a aplicacao do predicado, tem
relva em todas as Colunas/colunas cujo numero de tendas ja atingiu o numero de tendas possivel
nessas Colunas/colunas.
*/
relva(Puzzle):-
    Puzzle=(Tabuleiro,LL,LC),
    length(LL,N),
    calculaObjectosTabuleiro(Tabuleiro,CL,CC,t),
    relvaPorLinha(Tabuleiro,LL,CL,N,1,Tabuleiro),
    transpose(Tabuleiro,Transposto), % Para tratar as colunas como linhas
    relvaPorColuna(Transposto,LC,CC,N,1,Transposto),
    transpose(Transposto,Tabuleiro). % Volta a posicionar o tabuleiro no formato original     
relvaPorLinha(Tabuleiro,[],[],_,_,Tabuleiro).
relvaPorLinha(Tabuleiro,[Linha|Lista],[Contagem|OutraLista],N,I,NovoTabuleiro):-
    ((Linha == Contagem, % Quando o numero de tendas existente e o suposto de tenda coincidem
        insereObjectoEntrePosicoes(Tabuleiro, r, (I,1), (I,N)), % O predicado coloca relva nessa linha
        NovoTabuleiro = Tabuleiro); 
        NovoTabuleiro = Tabuleiro), % Se isso nao se verificar o tabuleiro fica inalterado
        I =< N,
        IN is I + 1,
    relvaPorLinha(Tabuleiro,Lista,OutraLista,N,IN,NovoTabuleiro).
relvaPorLinha(Tabuleiro,_,_,N,N,Tabuleiro). % Caso terminal para quando quando I atinge N 
relvaPorColuna(Tabuleiro2,LC,CC,N,1,Tabuleiro2):- % Faz novamente o mesmo mas para as colunas
    relvaPorLinha(Tabuleiro2,LC,CC,N,1,Tabuleiro2).


/* 
inacessiveis(Tabuleiro) e verdade se Tabuleiro e um tabuleiro que, apos a aplicacao do
predicado, tem relva em todas as posicoes inacessiveis
*/
inacessiveis(Tabuleiro):-
    todasCelulas(Tabuleiro,TodasCelulas,_),
    verificacaoVizinhanca(TodasCelulas,Tabuleiro,Tabuleiro).
% Predicado auxiliar que verifica as vizinhancas de todos os espacos vazios
verificacaoVizinhanca([], Tabuleiro, Tabuleiro).
verificacaoVizinhanca([(L,C)|Resto], Tabuleiro, NovoTabuleiro) :-
    vizinhanca((L,C), Vizinhanca),
    (celulasVazias(Tabuleiro, Vizinhanca),
        (
            insereObjectoCelula(Tabuleiro, r, (L,C)), % Se todas as celulas vizinhas sao vazias, coloca relva na celula (L, C)
            verificacaoVizinhanca(Resto, Tabuleiro, NovoTabuleiro)
        );
        % Se nao, continua a recursao
        verificacaoVizinhanca(Resto, Tabuleiro, NovoTabuleiro)
    ).
% Predicado auxiliar extra para garantir se todos os membros da vizinhanca sao espacos vazios
celulasVazias(Tabuleiro, Vizinhanca) :-
    forall(member((L, C), Vizinhanca), celulaVazia(Tabuleiro, (L, C))).


/*
aproveita(Puzzle) e verdade se Puzzle e um puzzle que, apos a aplicacao do predicado,
tem tendas em todas as linhas e colunas as quais faltavam colocar X tendas e que tinham 
exactamente X posicoes livres.
*/
aproveita(Puzzle) :-
    Puzzle = (Tabuleiro, LL, LC),
    length(LL, N),
    calculaObjectosTabuleiro(Tabuleiro, CL, CC, _),
    calculaObjectosTabuleiro(Tabuleiro, Tenda, _, t),
    tendasLinha(Tabuleiro, LL, CL, Tenda, N, 1),
    transpose(Tabuleiro, Transposto),
    tendasColuna(Transposto, LC, CC, Tenda, N, 1),
    transpose(Transposto, Tabuleiro).
% Predicado auxiliar que percorre as listas de espacos vazios existentes e tendas supostas por linha    
tendasLinha(_, [], [], _, _, _).
tendasLinha(Tabuleiro, [Linha | RestoLinhas], [Contagem | RestoContagens], Tenda, N, I) :-
    nth1(I, Tenda, Tendas),
    Contagem2 is Linha - Tendas, %  Permite saber quantas tendas faltam colocar
    (Contagem2 == Contagem, % Compara o numero de espacos vazios com as tendas que faltam
        insereObjectoEntrePosicoes(Tabuleiro, t, (I, 1), (I, N)),
        IN is I + 1,
        tendasLinha(Tabuleiro, RestoLinhas, RestoContagens, Tenda, N, IN);
        IN is I + 1,
    tendasLinha(Tabuleiro, RestoLinhas, RestoContagens, Tenda, N, IN)
    ).
tendasColuna(Tabuleiro, LC, CC, Tenda, N, I) :- % estrategia igual para as colunas 
    tendasLinha(Tabuleiro,LC,CC,Tenda,N,I).


/*
limpaVizinhancas(Puzzle) e verdade se Puzzle e um puzzle que, apos a aplicacao do
predicado, tem relva em todas as posicoes a volta de uma tenda
*/ 
limpaVizinhancas(Puzzle):-
    Puzzle=(Tabuleiro,_,_),
    todasCelulas(Tabuleiro,Tendas,t),
    tendas(Tendas,Tabuleiro,Tabuleiro).
%Predicado auxiliar que encontra a vizinhanca alargada de todas as tendas   
tendas([],Tabuleiro,Tabuleiro).
tendas([(L,C)|TD],Tabuleiro,NovoTabuleiro):-
    vizinhancaAlargada((L,C),VizinhancaAlargada), 
    relvaVizinhancaTendas(Tabuleiro,VizinhancaAlargada,NovoTabuleiro),
    tendas(TD,Tabuleiro,NovoTabuleiro).
% Predicado auxiliar que coloca relva em todas as posicoes da vizinhanca alargada de uma tenda  
relvaVizinhancaTendas(Tabuleiro,[],Tabuleiro).
relvaVizinhancaTendas(Tabuleiro, [(L, C) | RestoVizinhanca],NovoTabuleiro) :-
    insereObjectoCelula(Tabuleiro, r, (L, C)),
    relvaVizinhancaTendas(Tabuleiro,RestoVizinhanca,NovoTabuleiro).


/*
unicaHipotese(Puzzle) e verdade se Puzzle e um puzzle que, apos a aplicacao do predicado, 
todas as arvores que tinham apenas uma posicao livre na sua vizinhanca que lhes
permitia ficar ligadas a uma tenda, tem agora uma tenda nessa posicao.
*/
unicaHipotese(Puzzle):-
    Puzzle = (Tabuleiro, _, _),
    todasCelulas(Tabuleiro, Arvores, a),
    todasCelulas(Tabuleiro, EspacosLivres, _),
    hipotese(Tabuleiro, Arvores, EspacosLivres, Tabuleiro).
% Predicado auxiliar que permite colocar tenda nas coordenadas que correspondem aos criterios do UnicaHipotese       
hipotese(Tabuleiro, [], _, Tabuleiro).
hipotese(Tabuleiro, [(L, C) | Arvores], EspacosLivres, NovoTabuleiro) :-
    vizinhanca((L, C), Vizinhanca),
    vizinhosVazios((L,C),Vizinhanca, EspacosLivres,Tabuleiro,PosicoesLivres),
    length(PosicoesLivres, N), 
    (
        (N == 1,  % Se apenas existir uma coordenada que passe nos criterios e colocada uma tenda
            nth1(1, PosicoesLivres, (LINHA,_)),
            nth1(1,PosicoesLivres,(_,COLUNA)),
            insereObjectoCelula(Tabuleiro, t, (LINHA,COLUNA)),
        hipotese(Tabuleiro, Arvores, EspacosLivres, NovoTabuleiro))
        ;
        hipotese(Tabuleiro, Arvores, EspacosLivres, NovoTabuleiro)). % Caso contrario a funcao e chamada recursivamente
% Predicado auxiliar extra que encontra todos os "vizinhos" vazios da arvore                      
vizinhosVazios((L,C),Vizinhanca, EspacosLivres, Tabuleiro, PosicoesLivres) :-
    findall((L1, C1), 
    (
        member((L1, C1), Vizinhanca),
        member((L1, C1), EspacosLivres),
        semTendasNaVizinhanca((L, C), Tabuleiro)
    ),
    PosicoesLivres).
% Predicado auxiliar extra que garante que nao existem tendas associadas a essa arvore
semTendasNaVizinhanca((L, C), Tabuleiro) :-
    vizinhanca((L, C), Vizinhanca),
    todasCelulas(Tabuleiro, Tendas, t),
    not((member((L1, C1), Vizinhanca), member((L1, C1), Tendas))).


/*  
valida(LArv, LTen) e verdade se LArv e LTen sao listas com todas as coordenadas em
que existem, respectivamente, arvores e tendas, e r avaliado para verdade se for possivel
estabelecer uma relacao em que existe uma e uma unica tenda para cada arvore nas suas
vizinhancas.
*/
valida(LArv,LTen):-
    length(LArv,NumeroArvores),
    length(LTen,NumeroTendas),
    NumeroArvores==NumeroTendas,
    umaTendaPorArvore(LArv,LTen),
    tendasSemTendasVizinhas(LTen).
% Predicado auxiliar que encontra a vizinhanca de todas as arvores, verificando assim se a cada arvore corresponde uma e uma so tenda
umaTendaPorArvore([],[]).
umaTendaPorArvore([(L,C)|Arvores],LTen):-
    vizinhanca((L,C),Vizinhanca),
    soUmaTenda(Vizinhanca,LTen,Tenda),
    delete(LTen,Tenda,NovoLTen),
    umaTendaPorArvore(Arvores,NovoLTen).
%Predicado auxiliar que garante a associacao de uma tenda a cada arvore
%Funciona mesmo que existam mais que uma na sua vizinhanca  
soUmaTenda(Vizinhanca,LTen,Tenda):-
    % Queremos encontrar todos os vizinhos tendas
    findall((L1,C1),(member((L1,C1),Vizinhanca),member((L1,C1),LTen)),Tendas), 
    member(Tenda,Tendas).%Ao escolher um membro, este e associado a essa arvore e ser nao der certo o predicado tenta novamente com OUTRO membro da lista
%Predicado auxiliar que garante que nenhuma tenda tem nenhuma tenda vizinha
tendasSemTendasVizinhas([]).
tendasSemTendasVizinhas([(L, C) | Tendas]):-
    tendasTendasVizinhas((L,C),Tendas),
    tendasSemTendasVizinhas(Tendas).
tendasTendasVizinhas(_,[]).
tendasTendasVizinhas((L,C),[(L2,C2)|Tendas2]):-
    vizinhancaAlargada((L,C),VizinhancaAlargada),
    \+member((L2,C2),VizinhancaAlargada),
    tendasTendasVizinhas((L,C),Tendas2).


/*
resolve(Puzzle) e verdade se Puzzle e um puzzle que, apos a aplicacao do predicado, fica resolvido.
*/
resolve(Puzzle) :-
    Puzzle = (Tabuleiro, LL, LC),
    relva(Puzzle),
    inacessiveis(Tabuleiro),
    unicaHipotese(Puzzle),
    aproveita(Puzzle),
    limpaVizinhancas(Puzzle),
    colocaTendas(Tabuleiro, LL, LC),
    relva(Puzzle).
% Predicado auxiliar que por tentativa e erro coloca as tendas que faltam.      
colocaTendas(Tabuleiro, LL, LC) :-
    todasCelulas(Tabuleiro, Arvores, a),
    coloca(Tabuleiro, Arvores, [], ListaTendas),
    validacao(Tabuleiro,Arvores,LL,LC,ListaTendas).
% Predicado auxiliar que verifica se uma lista de tendas esta dentro das regras do puzzle           
validacao(Tabuleiro,Arvores,LL,LC,ListaTendas):-
    valida(Arvores,ListaTendas),
    insereTendas(Tabuleiro, t, ListaTendas),
    calculaObjectosTabuleiro(Tabuleiro, CL, CC, t),
    CL == LL,
    CC == LC.
% Predicado auxiliar extra que cria uma lista de tendas que pertencam a vizinhanca das arvores
coloca(_, [], ListaTendas, ListaTendas).
coloca(Tabuleiro, [(L, C) | RestoArvores], ListaTendasAtual, ListaTendas) :-
    vizinhanca((L, C), VizinhancaArvore),
    todasCelulas(Tabuleiro, EspacosVazios, _),
    espacoDisponivel(VizinhancaArvore, EspacosVazios, TendaAColocar), 
    coloca(Tabuleiro, RestoArvores, [TendaAColocar | ListaTendasAtual], ListaTendas).
% Predicado auxiliar extra que escolhe aleatoriamente um membro da vizinhanca da arvore que esteja vazio
espacoDisponivel(VizinhancaArvore, EspacosVazios, TendaAColocar) :-
    findall((L1, C1), (
        member((L1, C1), VizinhancaArvore),
        member((L1, C1), EspacosVazios)
        ), Espacinhos),
        member(TendaAColocar, Espacinhos).
% Predicado auxiliar extra que insere tendas em todas as coordenadas de uma determinada lista
insereTendas(_, _, []).
insereTendas(Tabuleiro, TendaOuRelva, [(L, C) | RestoCoordenadas]) :-
    insereObjectoCelula(Tabuleiro, TendaOuRelva, (L, C)),
    insereTendas(Tabuleiro, TendaOuRelva, RestoCoordenadas).
