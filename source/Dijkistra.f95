program Dijkistra
    
    INTEGER, DIMENSION(:,:), ALLOCATABLE:: matriz !matriz que será alocada dinâmicamente na memória
    INTEGER, PARAMETER :: INFINITO = 1000000, MAXIMO = 4096
    CHARACTER*2048 :: linha
    INTEGER:: l, coluna, erro, fim_arquivo, pos1, pos2, n, aux, origem, destino, minimo, indice, tam, tamanho, pos = 1
    CHARACTER (LEN=32) :: arg, arq
    
    TYPE no
        INTEGER :: anterior, distancia
        CHARACTER*15 :: proc
    END TYPE
    
    TYPE (no), DIMENSION(:), ALLOCATABLE :: caminho_curto
    
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
    IF (erro == 0) THEN

        ! Aloca um espaço na memória para uma matriz dinâmica(ponteiro para uma matriz de valores inteiros)
        ALLOCATE(matriz(MAXIMO,MAXIMO))
        
        l = 1
        coluna = 1
        ! faz a leituta do arquivo para obter os valores que preencherão a matriz
        
        do !1
            read(12,'(A)', iostat=fim_arquivo) linha
            if (fim_arquivo < 0) then
                exit
            end if
            
            ! tokeniza a linha lida do arquivo para obter os valores das distâncias entre os elementos do grafo
            pos1 = 1
            n = 1
            
            do !2
                pos2 = index(linha(pos1:), ' ')
                if (linha(pos1+1:) == '' .and. linha(pos1+2:) == '') exit
                if (pos2 == 0) then
                    read(linha(pos1:), '(i10)') matriz(l,n)
                    exit
                end if
                
                ! armazena o valor extraido da string para a matriz de inteiros.
                read(linha(pos1:pos1+pos2-2), '(i10)') matriz(l,n)
                n = n + 1
                pos1 = pos2+pos1
             end do !2

             l = l + 1
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
    l = 1
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

    l = destino
    print *, '--> ', destino
    do
        if (l == origem) exit
        print *, '--> ', caminho_curto(l)%anterior
        l = caminho_curto(l)%anterior
    end do
    print *, 'Distância = ', caminho_curto(destino)%distancia

    close(12)
    deallocate(matriz)
    deallocate(caminho_curto)
end
