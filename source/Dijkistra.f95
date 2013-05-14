! @author: Charles Tim Batista Garrocho
! @contact: charles.garrocho@gmail.com
! @copyright: (C) 2013 Fortran Software Open Source

program Dijkistra

    INTEGER :: INF=2**30, origem, destino, corrente, tamanho, menor_distancia, linha, coluna, verifica, cont, pos, pos_espaco
    INTEGER, DIMENSION(:, :), ALLOCATABLE :: matrizAdj
    CHARACTER (LEN=32) :: argumento, arquivo
    CHARACTER*2000 :: dados_linha
    TYPE no
        INTEGER :: antecessor, distancia, processado
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

        ALLOCATE(matrizAdj(1000, 1000))

        linha = 1
        coluna = 1

        DO
            READ(12,'(A)', IOSTAT=verifica) dados_linha
            IF (verifica < 0) EXIT

            pos = 1
            tamanho = 1

            DO
                IF ((dados_linha(pos+1:) == '').AND.(dados_linha(pos+2:) == '')) EXIT

                pos_espaco = index(dados_linha(pos:), ' ')
                READ(dados_linha(pos:pos+pos_espaco-2), '(i10)') matrizAdj(linha, tamanho)
                tamanho = tamanho + 1
                pos = pos_espaco + pos
            END DO

            linha = linha + 1
        END DO

    ELSE
        PRINT *, 'Arquivo Inexistente...  ', arquivo
        STOP
    END IF

    CLOSE(12)

    IF ((origem > tamanho).OR.(origem < 1).OR.(destino > tamanho).OR.(destino < 1)) THEN
        PRINT *, 'Nó de Origem ou Destino Inválido...  '
        STOP
    END IF

    ALLOCATE(caminho(tamanho))

    DO cont = 1, tamanho
        caminho(cont)%antecessor = -1
        caminho(cont)%distancia = INF
        caminho(cont)%processado = 0
    END DO

    caminho(origem)%distancia = 0
    caminho(origem)%processado = 1

    corrente = origem
    linha = 1
    coluna = 1

   DO
        menor_distancia = INF
        IF (corrente == destino) EXIT

        DO cont = 1, tamanho
            IF ((matrizAdj(corrente, cont) /= INF).AND.(matrizAdj(corrente, cont) /= 0).AND.(caminho(cont)%processado /= 1)) THEN
                IF (caminho(corrente)%distancia + matrizAdj(corrente, cont) < caminho(cont)%distancia) THEN
                    caminho(cont)%antecessor = corrente
                    caminho(cont)%distancia = caminho(corrente)%distancia + matrizAdj(corrente, cont)
                END IF
            END IF
        END DO

        corrente = 1

        DO cont = 1, tamanho
            IF ((caminho(cont)%processado == 0).AND.((caminho(cont)%distancia < menor_distancia))) THEN
                menor_distancia = caminho(cont)%distancia
                corrente = cont
            END IF
        END DO
        caminho(corrente)%processado = 1
    END DO
    
    DEALLOCATE(matrizAdj)
    
    PRINT *, 'Origem: ', origem
    PRINT *, 'Destino: ', destino 
    PRINT *, 'Custo: ', caminho(destino)%distancia
    PRINT *, 'Caminho: '

    DO
        IF (destino == origem) EXIT
        PRINT *, caminho(destino)%antecessor
        destino = caminho(destino)%antecessor
    END DO

    DEALLOCATE(caminho)
END
