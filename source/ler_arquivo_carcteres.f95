PROGRAM ler_arquivo_carcteres
	IMPLICIT NONE
	CHARACTER (LEN=1500) :: conteudo
	character (len=1500) :: line
	CHARACTER :: ch
	INTEGER :: tamanho, controle, altura, largura, i, lenstr, k, ipos
	INTEGER, DIMENSION(100,100) :: MatrizAdj
	OPEN(UNIT=1, FILE='../data/matriz-212.dat')
	tamanho = 0
	controle = 0
    altura = 0
    largura = 0
    k = 0

    do  
        read(1,'(a)', iostat=controle) line
        if(controle /= 0) exit
        line=adjustl(line)
        ipos=index(line,'!')
        if(ipos == 1) cycle
        if(ipos /= 0) line=line(:ipos-1)
        WRITE(*, *) line
        lenstr=len(line)
        do i=1, lenstr
           ch=conteudo(i:i)
           if(ch == ' ') then
               WRITE(*, *) 'ESPACO'
           ELSE if (ch == '\n') then
               WRITE(*, *) 'TERMINOU'
               EXIT
           ELSE
               WRITE(*, *) ch
           end if
        end do
        if(len_trim(line) /= 0) exit
    end do
	CLOSE(1)
END
