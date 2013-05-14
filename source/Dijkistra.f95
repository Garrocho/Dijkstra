program Dijkistra
    
    type no
        integer :: anterior !refere ao nodo anterior que está no caminho mais curto
        integer :: distancia !distância da origem até o nodoatual
        character*14 :: proc !recebe os valores "proc" e "nao_proc"
    end type        

    integer, dimension(:,:), allocatable:: matriz !matriz que será alocada dinâmicamente na memória
    integer, parameter :: INFINITO = 1000000 !valor definido como infinito
    integer, parameter :: MAXIMO = 4096 !máximo de nodos que o grafo de distâncias pode possuir
    integer :: tamanho !tamanho da matriz, que será obtida ao ler a primeira linha do arquivo em disco
    character*2048 :: l !variável que receberá o conteúdo lido de cada linha do arquivo
    integer:: linha, coluna, erro, fim_arquivo, pos1, pos2, n, aux, origem, destino, minimo, indice, tam, pos = 1
    CHARACTER (len=32) :: arg, arq
    type (no), dimension(:), allocatable:: caminho_curto !vetor dinâmico que receberá os nodos que compõe o caminho mais curto
    
    tam = IARGC()
    IF (tam == 3) THEN
        CALL GETARG(1, arg)
        READ(arg(pos:), '(A)') arq
        CALL GETARG(2, arg)
        READ(arg(pos:), '(i10)') origem
        CALL GETARG(3, arg)
        READ(arg(pos:), '(i10)') destino
    ELSE
        PRINT*, 'Argumentos Inválidos... <Endereço da Matriz> <Nó de Origem> <Nó de Destino>'
        STOP
    END IF

    open(unit=12, file=arq, iostat=erro)
    
    ! Verifica se o arquivo foi aberto comsucesso
    if (erro == 0) then

        ! Aloca um espaço na memória para uma matriz dinâmica(ponteiro para uma matriz de valores inteiros)
        allocate(matriz(MAXIMO,MAXIMO))
        
        linha = 1
        coluna = 1
        ! faz a leituta do arquivo para obter os valores que preencherão a matriz
        
        do !1
            read(12,'(A)', iostat=fim_arquivo) l
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

    else
        print *,'Erro na abertura do arquivo!'
        stop
    end if
    
    ! inicialização do vetor de caminhos mais curtos com valores default
    allocate(caminho_curto(MAXIMO))
    indice = 1
    do
    	if (indice > MAXIMO) exit 
        caminho_curto(indice)%anterior = -1
        caminho_curto(indice)%distancia = INFINITO
        caminho_curto(indice)%proc = 'nao_proc'
        indice = indice + 1
    end do
    
    caminho_curto(origem)%distancia = 0
    caminho_curto(origem)%proc = 'proc'
    
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
			
			!verifica se o nodo atual não é o nodo de origem ou se o nodo atual ainda não foi proc
			if ((matriz(aux,indice) /= INFINITO).and.(matriz(aux,indice) /= 0).and.(caminho_curto(indice)%proc == 'nao_proc')) then
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
    		!procura o nodo com menor distância para marcá-lo como proc
    		if( (caminho_curto(indice)%proc == 'nao_proc') .and. ((caminho_curto(indice)%distancia < minimo)) ) then
    			minimo = caminho_curto(indice)%distancia
    			aux = indice
    		end if
    		indice = indice + 1
    	end do !3
    	caminho_curto(aux)%proc = 'proc'

    end do !1

    print *, 'Caminho mais curto do nodo de origem até o nodo de destino: '

    linha = destino
    print *, '--> ', destino
    do
        if (linha == origem) exit
        print *, '--> ', caminho_curto(linha)%anterior
        linha = caminho_curto(linha)%anterior
    end do
    print *, 'Distância = ', caminho_curto(destino)%distancia

    close(12)
    deallocate(matriz)
    deallocate(caminho_curto)
end
