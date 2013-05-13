program Dijkistra
    
    ! --> Programa Dijkistra.f95
    !
    ! --> Este programa implementa o algorítimo de dijkistra para o cálculo do caminho mais curto
    ! entre os elementos de um grafo (formado por roteadores em uma rede), utilizando a linguegem 
    ! de programação Fortran, versão 95;
    !
    !--> Instituto Federal de Educação, Ciência e Tecnologia do sudeste de Minas Gerais
    !    Câmpus Barbacena
    !
    !--> Disciplina: TCP-IP e Rotemaento
    !--> Professor: Herlon Ayres Camargo
    !--> Aluno: Paulo Vitor Francisco
    !--> Barbacena, 25 de Abril de 2013
    

    ! Estrutura referente aos nodos que serão visitados
    type elemento
        integer :: anterior !refere ao nodo anterior que está no caminho mais curto
        integer :: distancia !distância da origem até o nodoatual
        character*14 :: situacao !recebe os valores "processado" e "nao_processado"
    end type        
    
    ! Declaração de Variáveis
    !integer, parameter:: dp=SELECTED_REAL_KIND(15, 300) !define o parâmetro dp para indicar a quantidade de casas decimais de valores reais
    !real(kind=dp), dimension(:,:), allocatable:: matriz !matriz que será alocada dinâmicamente na memória
    integer, dimension(:,:), allocatable:: matriz !matriz que será alocada dinâmicamente na memória
    integer, parameter :: arq = 10 !parâmetro que define um nome (arq) para o valor 10 que irá referênciar o arquivo em disco
    integer, parameter :: INFINITO = 1000000 !valor definido como infinito
    integer, parameter :: MAXIMO = 4096 !máximo de nodos que o grafo de distâncias pode possuir
    character*255, parameter :: NOME_ARQUIVO = 'matriz-212.dat' !paâmetro que recebe a localização do arquivo de rotas no disco
    integer :: tamanho !tamanho da matriz, que será obtida ao ler a primeira linha do arquivo em disco
    character*2048 :: l !variável que receberá o conteúdo lido de cada linha do arquivo
    !integer :: tam !variável que receberá o tamnho da linha lida do arquivo
    integer:: linha, coluna, erro, fim_arquivo, pos1, pos2, n, aux, origem, destino, minimo, indice !variáveis de controle
    type (elemento), dimension(:), allocatable:: caminho_curto !vetor dinâmico que receberá os nodos que compõe o caminho mais curto
    
    
    ! >>>>> Início do programa principal <<<<<
    
    call system('clear')
    print *, ''
    print *, ''
    print *, '|---------------------------------------------------------------|'
    print *, '| Algorítimo de Djikstra - Linguagem Fortran 95                 |'
    print *, '| Disciplina de TCP e Roteamento - IFET Campus Barbacena - 2013 |'
    print *, '|---------------------------------------------------------------|'    
    print *, ''
    print *, ''

    ! Abertura do arquivo contendo as distâncias
    open(unit=arq, file=NOME_ARQUIVO, status='old', iostat=erro, access='sequential')
    
    ! Verifica se o arquivo foi aberto comsucesso
    if (erro == 0) then

        ! Aloca um espaço na memória para uma matriz dinâmica(ponteiro para uma matriz de valores inteiros)
        allocate(matriz(MAXIMO,MAXIMO))
        
        linha = 1
        coluna = 1
        ! faz a leituta do arquivo para obter os valores que preencherão a matriz
        
        do !1
            read(arq,'(A)', iostat=fim_arquivo) l
            if (fim_arquivo < 0) then
                exit
            end if
            
            ! tokeniza a linha lida do arquivo para obter os valores das distâncias entre os elementos do grafo
            pos1 = 1
            n = 1
            
            do !2
                pos2 = index(l(pos1:), ' ')
                if (l(pos1+1:) == '' .and. l(pos1+2:) == '') exit
                if (pos2 == 0) then
                    read(l(pos1:), '(i10)') matriz(linha,n)
                    exit
                end if
                
                ! armazena o valor extraido da string para a matriz de inteiros.
                read(l(pos1:pos1+pos2-2), '(i10)') matriz(linha,n)
                n = n + 1
                pos1 = pos2+pos1
             end do !2

             linha = linha + 1
        end do !1
        
        tamanho = n

    else !se o arquivo não for aberto com sucesso
        print *,'Erro na abertura do arquivo!'
        print *,''
        print *,''
        stop
    end if
    
    ! inicialização do vetor de caminhos mais curtos com valores default
    allocate(caminho_curto(MAXIMO))
    indice = 1
    do
    	if (indice > MAXIMO) exit 
        caminho_curto(indice)%anterior = -1
        caminho_curto(indice)%distancia = INFINITO
        caminho_curto(indice)%situacao = 'nao_processado'
        indice = indice + 1
    end do
    
    do
        print *, 'Forneça o roteador de origem (valor da linha ou coluna que ele se encontra na matriz de distâncias):'
        read(*,*) origem
        if ( (origem > tamanho) .or. (origem < 1) ) then
            print *, ''
            print *, ' Forneça um valor válido!'
        else
            print *, ''
            print *, 'Forneça o roteador de destino (valor da linha ou coluna que ele se encontra na matriz de distâncias):'
            read(*,*) destino
            if ( (destino > tamanho) .or. (destino < 1) ) then
                print *, ''
                print *, ' Forneça um valor válido!'
            else
                exit
            end if
            
            exit
        end if
        
    end do
    
    caminho_curto(origem)%distancia = 0
    caminho_curto(origem)%situacao = 'processado'
    
    ! --> Cálculo do caminho mais curto

    aux = origem
    linha = 1
    coluna = 1
    pos1 = 1
    pos2 = 1
    
    !procura um caminho melhor para chegar até o nodo que está representado na variável "aux"
    do !1
    
    	if (aux == destino) exit
    	
    	indice = 1
    	do !2
    	
			if(indice > tamanho) exit
			
			!verifica se o nodo atual não é o nodo de origem ou se o nodo atual ainda não foi processado
			if ((matriz(aux,indice) /= INFINITO).and.(matriz(aux,indice) /= 0).and.(caminho_curto(indice)%situacao == 'nao_processado')) then
				!verifica se a distância do nodo até a origem é maior que a distância de nodo vizinho + a distância
				!entre o nodo atual até o nodo anterior
				if (caminho_curto(aux)%distancia + matriz(aux,indice) < caminho_curto(indice)%distancia) then
					caminho_curto(indice)%anterior = aux
					caminho_curto(indice)%distancia = caminho_curto(aux)%distancia + matriz(aux,indice)
				end if
			end if
			indice = indice + 1
			
    	end do !2
    	
    	aux = 1
    	minimo = INFINITO
    	indice = 1
    	do !3
    		
    		if(indice > tamanho) exit
    		!procura o nodo com menor distância para marcá-lo como processado
    		if( (caminho_curto(indice)%situacao == 'nao_processado') .and. ((caminho_curto(indice)%distancia < minimo)) ) then
    			minimo = caminho_curto(indice)%distancia
    			aux = indice
    		end if
    		indice = indice + 1
    	end do !3
    	caminho_curto(aux)%situacao = 'processado'

    end do !1
        
    !imprime uma tabela com todos os caminhos mais curtos de qualquer nodoaté o nodo de origem
    linha = 1
    print *, 'Tamanho da matriz dedistãncias: ', tamanho, 'x', tamanho
    print *, 'Nodo de origem: ', origem
    print *, '------------------------------------------------------------------'
    print *, '| Nodo atual    | Nodo anterior | Distância até o nodo de origem |'
    print *, '------------------------------------------------------------------'
    
    do
    	if(linha > tamanho) exit
    	print *, '| ', linha, ' | ', caminho_curto(linha)%anterior , ' | ', caminho_curto(linha)%distancia
  	    print *, '------------------------------------------------------------------'
    	linha = linha + 1
    end do
    
    !imprime o menor caminho do nodo de origem até o nodo de destino
    print *, ''
    print *, 'Caminho mais curto do nodo de origem até o nodo de destino: '

    linha = destino
    print *, '--> ', destino
    do
        if (linha == origem) exit
        print *, '--> ', caminho_curto(linha)%anterior
        linha = caminho_curto(linha)%anterior
    end do
    print *, 'Distância = ', caminho_curto(destino)%distancia
    !fecha o arquivo
    close(arq)
    
    !libera o espaço de memória ocupado pelos ponterios
    deallocate(matriz)
    deallocate(caminho_curto)
end
