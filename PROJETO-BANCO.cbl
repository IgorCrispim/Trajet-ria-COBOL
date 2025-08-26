      ******************************************************************
      * Author:    IGOR CRISPIM
      * Date:      26/08/2025
      * Purpose:   SIMULAÇÃO DE BACK-END DE UM BANCO
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJETO-BANCO.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ARQ-CONTA ASSIGN TO "contas.dat"
                ORGANIZATION IS INDEXED
                ACCESS MODE  IS DYNAMIC
                RECORD KEY   IS CONTA-NUM
                FILE STATUS  IS FS-CONTA.

           SELECT ARQ-ID ASSIGN TO "id.dat"
                ORGANIZATION IS SEQUENTIAL
                ACCESS MODE  IS SEQUENTIAL
                FILE STATUS  IS FS-ID.

       DATA DIVISION.
       FILE SECTION.

       FD ARQ-CONTA.
       01 REG-CONTA.
          05 CONTA-NUM                 PIC 9(06).   *> chave primária
          05 NOME                      PIC X(30).
          05 SALDO                     PIC 9(07)V99.

       FD ARQ-ID.
       01 REG-CONTROLE.
          05 ULT-CONTA                 PIC 9(06).

       WORKING-STORAGE SECTION.
       77 FS-CONTA                     PIC XX.
       77 FS-ID                        PIC 9(06).
       77 WS-OPCAO                     PIC 9.
       77 WS-VALOR                     PIC 9(07)V99.
       77 WS-AUX                       PIC 9(07)V99.



       PROCEDURE DIVISION.
      ******************************************************************
      *      FUNÇÃO DE MANIPULAÇÃO DE ARQUIVO
      ******************************************************************
       P100-MAIN.
             OPEN I-O ARQ-CONTA
                 IF FS-CONTA = "35" THEN
                   OPEN OUTPUT ARQ-CONTA
                   CLOSE ARQ-CONTA
                   OPEN I-O ARQ-CONTA
                END-IF.

             OPEN I-O ARQ-ID
                 IF FS-ID = "35"
                   OPEN OUTPUT ARQ-ID
                   MOVE 0 TO ULT-CONTA
                   WRITE REG-CONTROLE
                   CLOSE ARQ-ID
                   OPEN I-O ARQ-ID
                 END-IF.

                PERFORM P200-MENU
       .
      ******************************************************************
      *      FUNÇÃO DE IMPLEMENTAÇÃO DO MENU DO SISTEMA
      ******************************************************************
       P200-MENU.
             DISPLAY'**************************************************'
             DISPLAY 'QUAL OPERACAO DESEJA REALIZAR? '
             DISPLAY '01 - CRIAR UMA CONTA NOVA'
             DISPLAY '02 - CONSULTAR INFORMACOES DE UMA CONTA'
             DISPLAY '03 - REALIZAR UMA TRANSFERENCIA BANCARIA'
             DISPLAY '04 - REALIZAR UM DEPOSITO OU SAQUE DE SUA CONTA'
             DISPLAY '05 - FINALIZAR O PROGRAMA'
             ACCEPT WS-OPCAO

             IF WS-OPCAO > 5 OR WS-OPCAO < 1
                DISPLAY 'OPERACAO INEXISTENTE, TENTE NOVAMENTE!'
                PERFORM P200-MENU
             END-IF

             EVALUATE WS-OPCAO

                WHEN WS-OPCAO = 1
                   PERFORM P300-CRIAR
                WHEN WS-OPCAO = 2
                   PERFORM P400-CONSULTAR
                WHEN WS-OPCAO = 3
                   PERFORM P500-TRANFERENCIA
                WHEN WS-OPCAO = 4
                   PERFORM P600-DEP-SAQ
                WHEN WS-OPCAO = 5
                   PERFORM P900-TERMINAL

             END-EVALUATE
       .
      ******************************************************************
      *      FUNÇÃO QUE CRIA A CONTA NO BANCO DE DADOS
      ******************************************************************
       P300-CRIAR.
             DISPLAY 'DIGITE O NOME DO TITULAR DA CONTA: '
             ACCEPT NOME

             READ ARQ-ID
             ADD 1 TO ULT-CONTA
             MOVE ULT-CONTA TO FS-ID
             REWRITE REG-CONTROLE

             MOVE FS-ID TO CONTA-NUM
             MOVE ZEROS TO SALDO

             DISPLAY 'CRIANDO CONTA: ' CONTA-NUM
             DISPLAY 'PARABENS ' NOME ' A SUA CONTA FOI CRIADA COM '
                     'SUCESSO!'

             WRITE REG-CONTA

             DISPLAY 'RETORNANDO PARA O MENU...'
             PERFORM P200-MENU
       .
      ******************************************************************
      *      FUNÇÃO PARA CONSULTAR DADOS DE UMA CONTA EXISTENTE
      ******************************************************************
       P400-CONSULTAR.
             DISPLAY 'QUAL CONTA DESEJA CONSULTAR ?'
             DISPLAY 'INFORME O ID DA CONTA DESEJADA!'
             ACCEPT FS-ID

             READ REG-CONTA INVALID KEY
                DISPLAY 'CONTA NAO ENCONTRADA! TENTE NOVAMENTE'
                PERFORM P400-CONSULTAR.
             READ REG-CONTA NOT INVALID KEY
                DISPLAY 'DADOS DA CONTA ' FS-ID
                DISPLAY '**********************************************'
                DISPLAY 'NOME: '          NOME
                DISPLAY 'SALDO: '         SALDO
                DISPLAY 'ID: '            CONTA-NUM

                DISPLAY 'REDIRECIONANDO PARA O MENU...'
                PERFORM P200-MENU
       .
      ******************************************************************
      *      FUNÇÃO QUE REALIZA UMA TRANSFERENCIA BANCARIA ENTRE DUAS CONTAS
      ******************************************************************
       P500-TRANSFERENCIA.
             DISPLAY 'QUAL CONTA IRA REALIZAR A TRANSFERENCIA? '
             DISPLAY 'DIGITE O ID DA CONTA! '
             ACCEPT FS-ID
             READ REG-CONTA INVALID KEY
                DISPLAY 'CONTA NAO ENCONTRADA, TENTE NOVAMENTE'
                PERFORM P500-TRANSFERENCIA
             READ REG-CONTA NOT INVALID KEY
                DISPLAY 'QUAL O VALOR DA TRANSFERENCIA? '
                ACCEPT WS-VALOR
                MOVE ZEROS TO WS-AUX
                COMPUTE WS-AUX = SALDO - WS-VALOR
                IF SALDO <= 0 OR WS-AUX < 0 THEN
                   DISPLAY 'CONTA COM SAUDO INSUFICIENTE'
                   DISPLAY 'O SALDO DA CONTA ' NUM-CONTA ' : ' SALDO
                   DISPLAY 'TENTE NOVAMENTE!'
                   PERFORM P500-TRANSFERENCIA
                ELSE
                   MOVE WS-AUX TO SALDO
                   WRITE REG-CONTA

                   DISPLAY 'QUAL CONTA IRA RECEBER A TRANSFERENCIA? '
                   ACCEPT FS-ID
                   READ REG-CONTA  INVALID KEY
                      DISPLAY 'CONTA NAO ENCONTRADA, TENTE NOVAMENTE'
                      PERFORM P500-TRANSFERENCIA
                   READ REG-CONTA NOT INVALID KEY
                      MOVE ZEROS TO WS-AUX
                      COMPUTE WS-AUX = SALDO + WS-VALOR
                      MOVE WS-AUX TO SALDO

                      WRITE REG-CONTA
                      DISPLAY 'TRANSFERENCIA FEITA COM SUCESSO!'
                      DISPLAY 'RETORNANDO PARA O MENU...'
                      PERFORM P200-MENU
                 END-IF
       .
      ******************************************************************
      *      FUNÇÃO QUE REALIZA UM DEPOSITO OU SAQUE EM UMA CONTA EXISTENTE
      ******************************************************************
       P600-DEP-SAQ.
             MOVE ZEROS TO WS-OPCAO
             DISPLAY 'DIGITE 01 CASO QUERIA REALIZAR UM SAQUE OU '
                     'DIGITE 02 CASO QUEIRA REALIZAR UM DEPOSITO '
             ACCEPT WS-OPCAO
             EVALUATE WS-OPCAO
                WHEN WS-OPCAO = 1
                   DISPLAY 'QUAL CONTA IRA REALIZAR A MOVIMENTACAO? '
                   DISPLAY 'DIGITE O ID DA CONTA'
                   ACCEPT FS-ID
                   READ REG-CONTA INVALID KEY
                      'CONTA NAO ENCONTRADA, TENTE NOVAMENTE'
                      PERFORM P600-DEP-SAQ
                   READ REG-CONTA NOT INVALID KEY
                      DISPLAY 'QUAL O VALOR DA MOVIMENTACAO? '
                      ACCEPT WS-VALOR
                      COMPUTE WS-AUX = SALDO + WS-VALOR
                      MOVE WS-AUX TO SALDO
                      DISPLAY 'DEPOSITO REALIZADO COM SUCESSO! '
                      DISPLAY 'O SALDO ATUAL DA CONTA ' CONTA-NUM ' - '
                              SALDO
                      DISPLAY 'RETORNANDO PARA O MENU...'
                      WRITE REG-CONTA
                      PERFORM P200-MENU
                WHEN WS-OPCAO = 2
                   DISPLAY 'QUAL CONTA IRA REALIZAR A MOVIMENTACAO? '
                   DISPLAY 'DIGITE O ID DA CONTA'
                   ACCEPT FS-ID
                   READ REG-CONTA INVALID KEY
                      'CONTA NAO ENCONTRADA, TENTE NOVAMENTE'
                      PERFORM P600-DEP-SAQ
                   READ REG-CONTA NOT INVALID KEY
                      DISPLAY 'QUAL O VALOR DA MOVIMENTACAO? '
                      ACCEPT WS-VALOR
                      COMPUTE WS-AUX = SALDO - WS-VALOR
                      MOVE WS-AUX TO SALDO
                      DISPLAY 'SAQUE REALIZADO COM SUCESSO! '
                      DISPLAY 'O SALDO ATUAL DA CONTA ' CONTA-NUM ' - '
                              SALDO
                      DISPLAY 'RETORNANDO PARA O MENU...'
                      WRITE REG-CONTA
                      PERFORM P200-MENU
                 END-EVALUATE


       .

      ******************************************************************
      *      FUNÇÃO PARA FINALIZAR O PROGRAMA
      ******************************************************************
       P900-TERMINAL.
            CLOSE ARQ-CONTA.
            CLOSE ARQ-ID.
            STOP RUN.
       END PROGRAM PROJETO-BANCO.
