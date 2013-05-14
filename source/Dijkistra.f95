program Dijkistra

    INTEGER:: linha, coluna, verifica, pos1, pos2, corrente, origem, destino, menor_distancia, indice, tamanho
    INTEGER, DIMENSION(:,:), ALLOCATABLE:: MatrizAdj
    INTEGER, PARAMETER :: INF=2**30
    CHARACTER (LEN=32) :: argumento, arquivo
    CHARACTER*1200 :: dados_linha
    TYPE no
        INTEGER :: no_anterior, distancia, processado
    END TYPE
    TYPE (no), DIMENSION(:), ALLOCATABLE :: caminho

    tamanho = IARGC()
    IF (tamanho == 3) THEN
        CALL GETARG(1, argumento)
        READ(argumento(1:), '(A)') arquivo
        CALL GETARG(2, argumento)
        READ(argumento(1:), '(i10)') origem
        CALL GETARG(3, argumento)
        READ(argumento(1:), '(i10)') destino
    ELSE
        PRINT*, 'Argumentos Inválidos... <Endereço da Matriz> <Nó de Origem> <Nó de Destino>'
        STOP
    END IF

    OPEN(UNIT=12, FILE=arquivo, IOSTAT=verifica)

    IF (verifica == 0) THEN

        ALLOCATE(MatrizAdj(500, 500))

        linha = 1
        coluna = 1

        DO
            READ(12,'(A)', IOSTAT=verifica) dados_linha
            IF (verifica < 0) EXIT

            pos1 = 1
            tamanho = 1

            DO
                IF (dados_linha(pos1+1:) == '' .AND. dados_linha(pos1+2:) == '') EXIT

                pos2 = index(dados_linha(pos1:), ' ')

                IF (pos2 == 0) THEN
                    READ(dados_linha(pos1:), '(i10)') MatrizAdj(linha, tamanho)
                    EXIT
                END IF

                READ(dados_linha(pos1:pos1+pos2-2), '(i10)') MatrizAdj(linha, tamanho)
                tamanho = tamanho + 1
                pos1 = pos2 + pos1
             END DO

             linha = linha + 1
        END DO

    ELSE
        PRINT *, 'Arquivo Inexistente...  ', arquivo
        STOP
    END IF

    CLOSE(12)
    
    WRITE(*, *) tamanho

    IF ((origem > tamanho).OR.(origem < 1).OR.(destino > tamanho).OR.(destino < 1)) THEN
        PRINT *, 'Nó de Origem ou Destino Inválido...  '
        STOP
    END IF

    ALLOCATE(caminho(tamanho))

    DO indice = 1, tamanho
        caminho(indice)%no_anterior = -1
        caminho(indice)%distancia = INF
        caminho(indice)%processado = 0
    END DO

    caminho(origem)%distancia = 0
    caminho(origem)%processado = 1

    corrente = origem
    linha = 1
    coluna = 1

    DO
    	IF (corrente == destino) EXIT
    	DO indice = 1, tamanho
			IF ((MatrizAdj(corrente, indice) /= INF).AND.(MatrizAdj(corrente, indice) /= 0).AND.(caminho(indice)%processado == 0)) THEN
				IF (caminho(corrente)%distancia + MatrizAdj(corrente, indice) < caminho(indice)%distancia) THEN
					caminho(indice)%no_anterior = corrente
					caminho(indice)%distancia = caminho(corrente)%distancia + MatrizAdj(corrente, indice)
				END IF
			END IF
    	END DO

    	corrente = 1
    	menor_distancia = INF

    	DO indice = 1, tamanho
    		IF ((caminho(indice)%processado == 0) .AND. ((caminho(indice)%distancia < menor_distancia)) ) THEN
    			menor_distancia = caminho(indice)%distancia
    			corrente = indice
    		END IF
    	END DO
    	caminho(corrente)%processado = 1
    END DO

    PRINT *, 'caminho mais curto: '
    indice = destino
    PRINT *, destino

    DO
        IF (indice == origem) EXIT
        PRINT *, caminho(indice)%no_anterior
        indice = caminho(indice)%no_anterior
    END DO

    PRINT *, 'distanciaância: ', caminho(destino)%distancia
    DEALLOCATE(MatrizAdj)
    DEALLOCATE(caminho)
END
