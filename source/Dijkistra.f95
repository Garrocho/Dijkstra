program Dijkistra
    
    INTEGER, DIMENSION(:,:), ALLOCATABLE:: matriz
    INTEGER, PARAMETER :: INFINITO = 1000000, MAXIMO = 4096
    CHARACTER*2048 :: linha
    INTEGER:: l, coluna, erro, fim_arquivo, pos1, pos2, n, aux, origem, destino, minimo, indice, tam, tamanho, pos = 1
    CHARACTER (LEN=32) :: arg, arq
    
    TYPE no
        INTEGER :: anterior, distancia
        CHARACTER*15 :: proc
    END TYPE
    
    TYPE (no), DIMENSION(:), ALLOCATABLE :: caminho
    
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
    allocate(caminho(MAXIMO))

    DO indice = 1, MAXIMO 
        caminho(indice)%anterior = -1
        caminho(indice)%distancia = INFINITO
        caminho(indice)%proc = '0'
    END DO
    
    caminho(origem)%distancia = 0
    caminho(origem)%proc = '1'
    
    ! --> Cálculo do caminho mais curto
    aux = origem
    l = 1
    coluna = 1
    pos1 = 1
    pos2 = 1
    
    !procura um caminho melhor para chegar até o nodo que está representado na variável "aux"
    DO !1
    
    	IF (aux == destino) EXIT
    	
    	indice = 1
    	DO !2
    	
			IF (indice > tamanho) EXIT
			
			!verifica se o nodo atual não é o nodo de origem ou se o nodo atual ainda não foi proc
			IF ((matriz(aux,indice) /= INFINITO).AND.(matriz(aux,indice) /= 0).AND.(caminho(indice)%proc == '0')) THEN
				!verifica se a distância do nodo até a origem é maior que a distância de nodo vizinho + a distância
				!entre o nodo atual até o nodo anterior
				IF (caminho(aux)%distancia + matriz(aux,indice) < caminho(indice)%distancia) THEN
					caminho(indice)%anterior = aux
					caminho(indice)%distancia = caminho(aux)%distancia + matriz(aux,indice)
				END IF
			END IF
			indice = indice + 1
			
    	END DO !2
    	
    	aux = 1
    	minimo = INFINITO
    	indice = 1
    	DO !3
    		
    		IF (indice > tamanho) EXIT
    		!procura o nodo com menor distância para marcá-lo como proc
    		IF ((caminho(indice)%proc == '0') .AND. ((caminho(indice)%distancia < minimo)) ) THEN
    			minimo = caminho(indice)%distancia
    			aux = indice
    		END IF
    		indice = indice + 1
    	END DO !3
    	caminho(aux)%proc = '1'

    END DO !1

    PRINT *, 'Caminho mais curto do nodo de origem até o nodo de destino: '

    l = destino
    PRINT *, destino
    DO
        IF (l == origem) EXIT
        PRINT *, caminho(l)%anterior
        l = caminho(l)%anterior
    END DO
    PRINT *, 'Distância = ', caminho(destino)%distancia

    CLOSE(12)
    DEALLOCATE(matriz)
    DEALLOCATE(caminho)
END
