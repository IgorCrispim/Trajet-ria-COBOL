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
                FILE STATUS  IS FS-ID-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD ARQ-CONTA.
       01 REG-CONTA.
          05 CONTA-NUM                 PIC 9(06).   *> chave primária
          05 NOME                      PIC X(30).
          05 CPF                       PIC 9(11).
          05 SENHA                     PIC X(10).
          05 SALDO                     PIC S9(07)V99.

       FD ARQ-ID.
       01 REG-CONTROLE.
          05 ULT-CONTA                 PIC 9(06).

       WORKING-STORAGE SECTION.
       77 FS-CONTA                     PIC XX.
       77 FS-ID-STATUS                 PIC XX.
       77 FS-ID                        PIC 9(06).
       77 WS-OPCAO                     PIC 9.
       77 WS-VALOR                     PIC S9(07)V99.
       77 WS-AUX                       PIC S9(07)V99.
       77 WS-ERRO                      PIC 9 VALUE ZERO.
       77 WS-SENHA                     PIC X(10).
       77 WS-SENHA-2                   PIC X(10).
       77 WS-RANGE                     PIC 9(06).
       77 WS-RANGE-2                   PIC 9(06).



       PROCEDURE DIVISION.
      ******************************************************************
      *      FUNÇÃO DE MANIPULAÇÃO DE ARQUIVO
      ******************************************************************
       P100-MAIN.
             OPEN I-O ARQ-CONTA
                 IF FS-CONTA EQUAL TO "35" THEN
                   OPEN OUTPUT ARQ-CONTA
                   CLOSE ARQ-CONTA
                   OPEN I-O ARQ-CONTA
                END-IF.

             OPEN I-O ARQ-ID
                 IF FS-ID-STATUS EQUAL TO "35"
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
             MOVE ZEROS TO WS-ERRO
             DISPLAY'**************************************************'
             DISPLAY 'QUAL OPERACAO DESEJA REALIZAR? '
             DISPLAY '01 - CRIAR UMA CONTA NOVA'
             DISPLAY '02 - CONSULTAR INFORMACOES DE UMA CONTA'
             DISPLAY '03 - REALIZAR UMA TRANSFERENCIA BANCARIA'
             DISPLAY '04 - REALIZAR UM DEPOSITO OU SAQUE DE SUA CONTA'
             DISPLAY '05 - IMPRIMIR TODAS AS CONTAS EM UM RANGE DE ID'
             DISPLAY '06 - FINALIZAR O PROGRAMA'
             ACCEPT WS-OPCAO

             IF WS-OPCAO > 6 OR WS-OPCAO < 1
                DISPLAY 'OPERACAO INEXISTENTE, TENTE NOVAMENTE!'
                PERFORM P200-MENU
             END-IF

             EVALUATE WS-OPCAO

                WHEN 1
                   PERFORM P300-CRIAR
                WHEN 2
                   PERFORM P400-CONSULTAR
                WHEN 3
                   PERFORM P500-TRANsFERENCIA
                WHEN 4
                   PERFORM P600-DEP-SAQ
                WHEN 5
                   PERFORM P700-IMPRIMIR-CONTAS
                WHEN 6
                   PERFORM P900-TERMINAL

             END-EVALUATE
       .
      ******************************************************************
      *      FUNÇÃO QUE CRIA A CONTA NO BANCO DE DADOS
      ******************************************************************
       P300-CRIAR.
             DISPLAY 'DIGITE O NOME DO TITULAR DA CONTA: '
             ACCEPT NOME
             DISPLAY 'DIGITE O CPF DO BENEFICIARIO: '
             ACCEPT CPF
             DISPLAY 'DIGITE A SUA SENHA DE ATE 10 CARACTERES'
             ACCEPT WS-SENHA

             MOVE ZEROS TO WS-ERRO
             PERFORM UNTIL WS-SENHA EQUAL TO WS-SENHA-2
             ADD 1 TO WS-ERRO
             IF WS-ERRO LESS THAN 2 THEN
                DISPLAY 'CONFIRME A SENHA DIGITADA ANTERIORMENTE'
                ACCEPT WS-SENHA-2
             ELSE
                IF WS-ERRO LESS THAN 5 THEN
                   DISPLAY 'SENHA ERRADA, TENTE NOVAMENTE'
                   ACCEPT WS-SENHA-2
                ELSE
                   DISPLAY 'MUITOS ERROS CONSECUTIVOS, REDIRECIONANDO '
                           'PARA O MENU PRINCIPAL...'
                   PERFORM P200-MENU
                END-IF
             END-IF
             END-PERFORM

             MOVE WS-SENHA TO SENHA


             READ ARQ-ID
             ADD 1 TO ULT-CONTA
             MOVE ULT-CONTA TO FS-ID
             REWRITE REG-CONTROLE

             MOVE FS-ID TO CONTA-NUM
             MOVE ZEROS TO SALDO

             DISPLAY 'CRIANDO CONTA: ' CONTA-NUM
             DISPLAY 'PARABENS ' NOME
             DISPLAY 'A SUA CONTA FOI CRIADA COM SUCESSO! '

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
             MOVE FS-ID TO CONTA-NUM

             READ ARQ-CONTA RECORD KEY IS CONTA-NUM
                INVALID KEY
                   ADD 1 TO WS-ERRO
                   DISPLAY 'CONTA NAO ENCONTRADA! TENTE NOVAMENTE'
                   IF WS-ERRO GREATER THAN 3 THEN
                      DISPLAY 'MUITOS ERROS CONSECUTIVOS'
                      DISPLAY 'REDIRECIONANDO PARA O MENU...'
                      PERFORM P200-MENU
                   ELSE
                      PERFORM P400-CONSULTAR
                   END-IF
                NOT INVALID KEY
                   DISPLAY 'DADOS DA CONTA ' FS-ID
                   DISPLAY '*******************************************'
                   DISPLAY 'NOME: '          NOME
                   DISPLAY 'CPF: '           CPF
                   DISPLAY 'SALDO: '         SALDO
                   DISPLAY 'ID: '            CONTA-NUM

                   DISPLAY 'REDIRECIONANDO PARA O MENU...'
                   PERFORM P200-MENU
             END-READ
       .
      ******************************************************************
      *      FUNÇÃO QUE REALIZA UMA TRANSFERENCIA BANCARIA ENTRE DUAS CONTAS
      ******************************************************************
       P500-TRANSFERENCIA.
             DISPLAY 'QUAL CONTA IRA REALIZAR A TRANSFERENCIA? '
             DISPLAY 'DIGITE O ID DA CONTA! '
             ACCEPT FS-ID
             MOVE FS-ID TO CONTA-NUM
             READ ARQ-CONTA RECORD KEY IS CONTA-NUM
                INVALID KEY
                   DISPLAY 'CONTA NAO ENCONTRADA, TENTE NOVAMENTE'
                   PERFORM P500-TRANSFERENCIA
                NOT INVALID KEY
                   DISPLAY 'DIGITE A SENHA DA CONTA ' FS-ID
                   ACCEPT WS-SENHA
                   IF WS-SENHA NOT EQUAL TO SENHA THEN
                      DISPLAY 'SENHA ERRADA, TRANSACAO CANCELADA! '
                      PERFORM P200-MENU
                   ELSE
                      DISPLAY 'QUAL O VALOR DA TRANSFERENCIA? '
                      ACCEPT WS-VALOR
                      MOVE ZEROS TO WS-AUX
                      COMPUTE WS-AUX = SALDO - WS-VALOR
                      IF SALDO EQUAL TO 0 OR WS-AUX LESS THAN 0 THEN
                         ADD 1 TO WS-ERRO
                         IF WS-ERRO GREATER THAN 3 THEN
                            DISPLAY 'MUITOS ERROS CONSECUTIVOS'
                            DISPLAY 'REDIRECIONANDO PARA O MENU...'
                            PERFORM P200-MENU
                         ELSE
                            DISPLAY 'CONTA COM SAUDO INSUFICIENTE'
                            DISPLAY 'O SALDO DA CONTA ' CONTA-NUM
                                    ' : ' SALDO
                            DISPLAY 'TENTE NOVAMENTE!'
                            PERFORM P500-TRANSFERENCIA
                      ELSE
                         MOVE WS-AUX TO SALDO
                         REWRITE REG-CONTA

                         DISPLAY 'QUAL CONTA IRA RECEBER A'
                                 ' TRANSFERENCIA? '
                         ACCEPT FS-ID
                         MOVE FS-ID TO CONTA-NUM
                         READ ARQ-CONTA RECORD KEY IS CONTA-NUM
                            INVALID KEY
                               ADD 1 TO WS-ERRO
                               IF WS-ERRO GREATER THAN 3 THEN
                                  DISPLAY 'MUITOS ERROS CONSECUTIVOS'
                                  DISPLAY 'REDIRECIONANDO PARA O'
                                          ' MENU...'
                                  PERFORM P200-MENU
                               ELSE
                                  DISPLAY 'CONTA NAO ENCONTRADA, '
                                          'TENTE NOVAMENTE'
                                  PERFORM P500-TRANSFERENCIA
                            NOT INVALID KEY
                               MOVE ZEROS TO WS-AUX
                               COMPUTE WS-AUX = SALDO + WS-VALOR
                               MOVE WS-AUX TO SALDO

                               REWRITE REG-CONTA
                               DISPLAY 'TRANSFERENCIA FEITA COM'
                                       ' SUCESSO!'
                               DISPLAY 'RETORNANDO PARA O MENU...'
                               PERFORM P200-MENU
                         END-READ
                      END-IF
                   END-IF
             END-READ
       .
      ******************************************************************
      *      FUNÇÃO QUE REALIZA UM DEPOSITO OU SAQUE EM UMA CONTA EXISTENTE
      ******************************************************************
       P600-DEP-SAQ.
             MOVE ZEROS TO WS-OPCAO
             DISPLAY 'DIGITE 01 CASO QUEIRA REALIZAR UM DEPOSITO '
             DISPLAY 'DIGITE 02 CASO QUERIA REALIZAR UM SAQUE '
             ACCEPT WS-OPCAO
             EVALUATE WS-OPCAO
                WHEN 1
                   DISPLAY 'QUAL CONTA IRA REALIZAR A MOVIMENTACAO? '
                   DISPLAY 'DIGITE O ID DA CONTA'
                   ACCEPT FS-ID
                   MOVE FS-ID TO CONTA-NUM
                   READ ARQ-CONTA RECORD KEY IS CONTA-NUM
                      INVALID KEY
                         ADD 1 TO WS-ERRO
                         IF WS-ERRO GREATER THAN 3 THEN
                            DISPLAY 'MUITOS ERROS CONSECUTIVOS'
                            DISPLAY 'REDIRECIONANDO PARA O MENU...'
                            PERFORM P200-MENU
                         ELSE
                            DISPLAY 'CONTA NAO ENCONTRADA,'
                                    ' TENTE NOVAMENTE'
                            PERFORM P600-DEP-SAQ
                         END-IF
                      NOT INVALID KEY
                         DISPLAY 'DIGITE A SENHA DA CONTA ' FS-ID
                         ACCEPT WS-SENHA
                         IF WS-SENHA NOT EQUAL TO SENHA THEN
                            DISPLAY 'SENHA ERRADA,'
                                    ' TRANSACAO CANCELADA! '
                            PERFORM P200-MENU
                         ELSE

                            DISPLAY 'QUAL O VALOR DA MOVIMENTACAO? '
                            ACCEPT WS-VALOR
                            COMPUTE WS-AUX = SALDO + WS-VALOR
                            MOVE WS-AUX TO SALDO
                            DISPLAY 'DEPOSITO REALIZADO COM SUCESSO! '
                            DISPLAY 'O SALDO ATUAL DA CONTA ' CONTA-NUM
                                 ' : ' SALDO
                            DISPLAY 'RETORNANDO PARA O MENU...'
                            REWRITE REG-CONTA
                            PERFORM P200-MENU
                         END-IF
                   END-READ
                WHEN 2
                   DISPLAY 'QUAL CONTA IRA REALIZAR A MOVIMENTACAO? '
                   DISPLAY 'DIGITE O ID DA CONTA'
                   ACCEPT FS-ID
                   MOVE FS-ID TO CONTA-NUM
                   READ ARQ-CONTA RECORD KEY IS CONTA-NUM
                      INVALID KEY
                         ADD 1 TO WS-ERRO
                         IF WS-ERRO GREATER THAN 3 THEN
                            DISPLAY 'MUITOS ERROS CONSECUTIVOS'
                            DISPLAY 'REDIRECIONANDO PARA O MENU...'
                            PERFORM P200-MENU
                         ELSE
                            DISPLAY 'CONTA NAO ENCONTRADA,'
                                    ' TENTE NOVAMENTE'
                            PERFORM P600-DEP-SAQ
                      NOT INVALID KEY
                         DISPLAY 'DIGITE A SENHA DA CONTA ' FS-ID
                         ACCEPT WS-SENHA
                         IF WS-SENHA NOT EQUAL TO SENHA THEN
                            DISPLAY 'SENHA ERRADA,'
                                    ' TRANSACAO CANCELADA! '
                            PERFORM P200-MENU
                         ELSE
                            DISPLAY 'QUAL O VALOR DA MOVIMENTACAO? '
                            ACCEPT WS-VALOR
                            IF WS-VALOR GREATER THAN SALDO THEN
                               DISPLAY 'SALDO INSUFICIENTE, TRANSACAO '
                                       'CANCELADA'
                               PERFORM P200-MENU
                            ELSE
                               COMPUTE WS-AUX = SALDO - WS-VALOR
                               MOVE WS-AUX TO SALDO
                               DISPLAY 'SAQUE REALIZADO COM SUCESSO! '
                               DISPLAY 'O SALDO ATUAL DA CONTA '
                                       CONTA-NUM
                                       ' : ' SALDO
                               DISPLAY 'RETORNANDO PARA O MENU...'
                               REWRITE REG-CONTA
                               PERFORM P200-MENU
                            END-IF
                         END-IF
                   END-READ
             END-EVALUATE


       .

      ******************************************************************
      *      FUNÇÃO PARA TESTAR SE A SENHA ESTÁ CORRETA
      ******************************************************************
       P700-IMPRIMIR-CONTAS.
             DISPLAY '*************************************************'
             MOVE ZEROS TO WS-RANGE
                           WS-RANGE-2
             DISPLAY 'DIGITE O RANGE DE CONTAS QUE DESEJA MOSTRAR'
             DISPLAY 'DIGITE O LIMITE INFERIOR'
             ACCEPT WS-RANGE
             DISPLAY 'DIGITE O LIMITE SUPERIOR'
             ACCEPT WS-RANGE-2
             MOVE WS-RANGE TO CONTA-NUM
             SUBTRACT 1 FROM CONTA-NUM

             PERFORM VARYING WS-RANGE FROM WS-RANGE BY 1
             UNTIL WS-RANGE > WS-RANGE-2
                ADD 1 TO CONTA-NUM
                READ ARQ-CONTA RECORD KEY IS CONTA-NUM
                   INVALID KEY
                      DISPLAY '****************************************'
                   NOT INVALID KEY
                      DISPLAY '****************************************'
                      DISPLAY 'CONTA: ' CONTA-NUM
                      DISPLAY 'NOME: '  NOME
                      DISPLAY 'CPF: '   CPF
                      DISPLAY 'SALDO: ' SALDO

             END-PERFORM
             PERFORM P200-MENU
       .
      ******************************************************************
      *      FUNÇÃO PARA FINALIZAR O PROGRAMA
      ******************************************************************
       P900-TERMINAL.
            CLOSE ARQ-CONTA.
            CLOSE ARQ-ID.
            STOP RUN.
       END PROGRAM PROJETO-BANCO.
