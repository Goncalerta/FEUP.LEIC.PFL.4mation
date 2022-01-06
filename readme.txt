(passar para pdf depois mas por agora é mais facil escrever aqui)

Identificação do trabalho e do grupo 
    (designação do grupo, número e nome completo de cada um dos elementos), 
    assim como indicação da contribuição (em percentagem, somando 100%) de 
    cada elemento do grupo para o trabalho;

Grupo 4Mation_5:
Joel Alexandre Vieira Fernandes up201904977@edu.fe.up.pt - __%
Pedro Gonçalo de Castro Correia up201905348@edu.fe.up.pt - __%

● Instalação e Execução
    incluir todos os passos necessários para correta execução do jogo em ambientes Linux e 
    Windows (para além da instalação do SICStus Prolog 4.7);
*Provavelmente nada*

● Descrição do jogo: 
    descrição sumária do jogo e suas regras (até 350 palavras); devem incluir
    ainda ligações usadas na recolha de informação (página oficial do jogo, livro de regras, etc.);

Em 4Mation, dois jogadores colocam à vez uma peça no tabuleiro de maneira a criar
uma linha, coluna ou diagonal de 4 peças jogadas por si. Nesta implementação do jogo, as peças do 
jogador 1 serão representadas por 'X' e as do jogador 2 por 'O'. Para além disso, o número necessário
de peças consecutivas para ganhar o jogo é configurável.

O tamanho do tabuleiro é definido antes de iniciar o jogo. O primeiro jogador pode colocar uma peça
em qualquer posição do tabuleiro. Em todas as jogadas seguintes, a peça do jogador que está a jogar
tem de ser colocada numa posição adjacente (ortogonalmente ou diagonalmente) à peça colocada na jogada 
do jogador anterior.

O jogo termina quando um dos jogadores consegue formar uma linha, coluna ou diagonal de 4 peças suas,
sendo considerado o vencedor, ou quando não há nenhuma jogada possível, sendo considerado empate.

Mais informações, bem como a descrição do jogo feita pela editora, podem ser encontradas em https://boardgamegeek.com/boardgame/329175/4mation.

● Lógica do Jogo: 
    descrever (não basta copiar código fonte) o projeto e implementação da lógica
    do jogo em Prolog. O predicado de início de jogo deve ser play/0. Esta secção deve ter
    informação sobre os seguintes tópicos (até 2400 palavras no total):
o Representação interna do estado do jogo:
    indicação de como representam o estado do
    jogo, incluindo tabuleiro (tipicamente usando lista de listas com diferentes átomos para as
    peças), jogador atual, e eventualmente peças capturadas e/ou ainda por jogar, ou outras
    informações que possam ser necessárias (dependendo do jogo). Deve incluir exemplos da
    representação em Prolog de estados de jogo inicial, intermédio e final, e indicação do
    significado de cada átomo (ie., como representam as diferentes peças).
*TODO*

o Visualização do estado de jogo: 
    descrição da implementação do predicado de visualização
    do estado de jogo. Pode incluir informação sobre o sistema de menus criado, assim como
    interação com o utilizador, incluindo formas de validação de entrada. O predicado de
    visualização deverá chamar-se display_game(+GameState), recebendo o estado de jogo
    atual (que inclui o jogador que efetuará a próxima jogada). Serão valorizadas visualizações
    apelativas e intuitivas. Serão também valorizadas representações de estado de jogo e
    implementação de predicados de visualização flexíveis, por exemplo, funcionando para
    qualquer tamanho de tabuleiro, usando um predicado initial_state(+Size, -GameState)
    que recebe o tamanho do tabuleiro como argumento e devolve o estado inicial do jogo.
    o Execução de Jogadas: Validação e execução de uma jogada, obtendo o novo estado do
    jogo. O predicado deve chamar-se move(+GameState, +Move, -NewGameState).
*TODO*

o Final do Jogo: 
    Verificação da situação de fim do jogo, com identificação do vencedor. O
    predicado deve chamar-se game_over(+GameState, -Winner).
*TODO*

o Lista de Jogadas Válidas: 
    Obtenção de lista com jogadas possíveis. O predicado deve
    chamar-se valid_moves(+GameState, -ListOfMoves).
*TODO*

o Avaliação do Estado do Jogo*: 
    Forma(s) de avaliação do estado do jogo do ponto de vista
    de um jogador, quantificada através do predicado value(+GameState, +Player, -Value).
*TODO*

o Jogada do Computador*: 
    Escolha da jogada a efetuar pelo computador, dependendo do
    nível de dificuldade, através de um predicado choose_move(+GameState, +Level, -Move).
    O nível 1 deverá devolver uma jogada válida aleatória. O nível 2 deverá devolver a melhor
    jogada no momento (algoritmo míope), tendo em conta a avaliação do estado de jogo.
*TODO*

● Conclusões: Conclusões do trabalho, incluindo limitações do trabalho desenvolvido (known
issues), assim como possíveis melhorias identificadas (roadmap). (até 250 palavras)
*TODO*

● Bibliografia: 
    Listagem de livros, artigos, páginas Web e outros recursos usados durante o
    desenvolvimento do trabalho
*what, é preciso alguma coisa?!*
