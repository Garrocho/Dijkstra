program Dijkistra
    
    INTEGER, DIMENSION(:,:), ALLOCATABLE:: matriz
    INTEGER, PARAMETER :: INF=2**30, MAXIMO = 4096
    CHARACTER*2048 :: linha
    INTEGER:: l, coluna, erro, fim, pos1, pos2, n, aux, origem, destino, minimo, indice, tam, tamanho, pos = 1
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

    OPEN(UNIT=12, FILE=arq, IOSTAT=erro)
    
    IF (erro == 0) THEN

        ALLOCATE(matriz(MAXIMO,MAXIMO))
        
        l = 1
        coluna = 1
        
        DO
            READ(12,'(A)', IOSTAT=fim) linha
            IF (fim < 0) THEN
                EXIT
            END IF
            
            pos1 = 1
            n = 1
            
            DO
                pos2 = index(linha(pos1:), ' ')
                IF (linha(pos1+1:) == '' .AND. linha(pos1+2:) == '') EXIT
                IF (pos2 == 0) THEN
                    READ(linha(pos1:), '(i10)') matriz(l,n)
                    EXIT
                END IF
                
                READ(linha(pos1:pos1+pos2-2), '(i10)') matriz(l,n)
                n = n + 1
                pos1 = pos2+pos1
             END DO

             l = l + 1
        END DO
        
        tamanho = n

    ELSE
        PRINT *,'Erro ao Abrir o arquivo ', arq
        STOP
    END IF
    
    CLOSE(12)
    
    ALLOCATE(caminho(MAXIMO))

    DO indice = 1, MAXIMO 
        caminho(indice)%anterior = -1
        caminho(indice)%distancia = INF
        caminho(indice)%proc = '0'
    END DO
    
    caminho(origem)%distancia = 0
    caminho(origem)%proc = '1'
    
    aux = origem
    l = 1
    coluna = 1
    pos1 = 1
    pos2 = 1
 
    DO
    	IF (aux == destino) EXIT

    	indice = 1
    	DO
    	
			IF (indice > tamanho) EXIT
			
			!verifica se o nodo atual não é o nodo de origem ou se o nodo atual ainda não foi proc
			IF ((matriz(aux,indice) /= INF).AND.(matriz(aux,indice) /= 0).AND.(caminho(indice)%proc == '0')) THEN
				!verifica se a distância do nodo até a origem é maior que a distância de nodo vizinho + a distância
				!entre o nodo atual até o nodo anterior
				IF (caminho(aux)%distancia + matriz(aux,indice) < caminho(indice)%distancia) THEN
					caminho(indice)%anterior = aux
					caminho(indice)%distancia = caminho(aux)%distancia + matriz(aux,indice)
				END IF
			END IF
			indice = indice + 1
			
    	END DO
    	
    	aux = 1
    	minimo = INF
    	indice = 1
    	DO
    		
    		IF (indice > tamanho) EXIT
    		!procura o nodo com menor distância para marcá-lo como proc
    		IF ((caminho(indice)%proc == '0') .AND. ((caminho(indice)%distancia < minimo)) ) THEN
    			minimo = caminho(indice)%distancia
    			aux = indice
    		END IF
    		indice = indice + 1
    	END DO
    	caminho(aux)%proc = '1'

    END DO

    PRINT *, 'Caminho mais curto: '

    l = destino
    PRINT *, destino
    DO
        IF (l == origem) EXIT
        PRINT *, caminho(l)%anterior
        l = caminho(l)%anterior
    END DO
    PRINT *, 'Distância = ', caminho(destino)%distancia

    DEALLOCATE(matriz)
    DEALLOCATE(caminho)
END
