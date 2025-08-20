      ******************************************************************
      * Author:    Igor Crispim
      * Date:      20/08/2025
      * Purpose:   aprendizado sobre as operações matemáticas no COBOL
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OPERACOES-MATEMATICAS.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
          SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-NUMEROS.
          03 WS-SOMA           PIC 9(05)V99.
          03 WS-MUL            PIC 9(05)V99.
          03 WS-DVD            PIC 9(05)V99.
          03 WS-SUB            PIC 9(05)V99.
          03 WS-POT            PIC 9(05)V99.
          03 WS-N-1            PIC 9(03)V99.
          03 WS-N-2            PIC 9(03)V99.
          03 WS-EXIBIR         PIC ZZ.ZZ9,99.
          03 WS-EXIBIR-NEG     PIC -ZZ.ZZ9,99.

       77 WS-OPERACAO          PIC X(01).
       77 WS-FIM               PIC X(01).

       PROCEDURE DIVISION.
      ************************FUNÇÃO PRINCIPAL**************************
       P100-MAIN.
             DISPLAY '****** PROGRAMA DE CALCULO *****'

             INITIALISE WS-NUMEROS

             DISPLAY 'INFORME O PRIMEIRO NUMERO: '
             ACCEPT WS-N-1

             DISPLAY 'INFORME O SEGUNDO NUMERO: '
             ACCEPT WS-N-2

             PERFORM P200-CAL THRU P200-FIM.
      ***********************FUNÇÃO DOS CALCULOS************************
       P200-CAL.

             DISPLAY 'QUAL OPERACAO DESEJA REALIZAR ? '
             DISPLAY 'DIGITE "+" PARA REALIZAR UMA SOMA'
             DISPLAY 'DIGITE "*" PARA REALIZAR UMA MULTIPLICACAO'
             DISPLAY 'DIGITE "/" PARA REALIZAR UMA DIVISAO'
             DISPLAY 'DIGITE "-" PARA REALIZAR UMA SUBTRACAO'
             DISPLAY 'DIGITE "#" PARA REALIZAR UMA POTENCIACAO'

             MOVE ZEROS      TO WS-SOMA
             MOVE ZEROS      TO WS-MUL
             MOVE ZEROS      TO WS-DVD
             MOVE ZEROS      TO WS-SUB
             MOVE ZEROS      TO WS-EXIBIR

             ACCEPT WS-OPERACAO

             EVALUATE WS-OPERACAO
             WHEN '+'
                COMPUTE WS-SOMA = WS-N-1 + WS-N-2
                MOVE WS-SOMA TO WS-EXIBIR
                DISPLAY 'O RESULTADO E: ' WS-EXIBIR
             WHEN '*'
                COMPUTE WS-MUL = WS-N-1 * WS-N-2
                IF WS-MUL < 0
                   MOVE WS-MUL  TO WS-EXIBIR-NEG
                   DISPLAY 'O RESULTADO E: ' WS-EXIBIR-NEG
                ELSE
                   MOVE WS-MUL  TO WS-EXIBIR
                   DISPLAY 'O RESULTADO E: ' WS-EXIBIR
                END-IF
             WHEN '/'
                COMPUTE WS-DVD = WS-N-1 / WS-N-2
                IF WS-DVD < 0
                   MOVE WS-DVD  TO WS-EXIBIR-NEG
                   DISPLAY 'O RESULTADO E: ' WS-EXIBIR-NEG
                ELSE
                   MOVE WS-DVD  TO WS-EXIBIR
                   DISPLAY 'O RESULTADO E: ' WS-EXIBIR
                END-IF
             WHEN '-'
                COMPUTE WS-SUB = WS-N-1 - WS-N-2
                IF WS-SUB < 0
                   MOVE WS-SUB  TO WS-EXIBIR-NEG
                   DISPLAY 'O RESULTADO E: ' WS-EXIBIR-NEG
                ELSE
                   MOVE WS-SUB  TO WS-EXIBIR
                   DISPLAY 'O RESULTADO E: ' WS-EXIBIR
                END-IF
             WHEN '#'
                COMPUTE WS-POT = WS-N-1 ** WS-N-2
                MOVE WS-POT     TO WS-EXIBIR
                DISPLAY 'O RESULTADO E: ' WS-EXIBIR
             END-EVALUATE

             DISPLAY 'DESEJA REALIZAR MAIS ALGUM CALCULO? '
             DISPLAY 'Y = SIM'
             DISPLAY 'N = NAO'

             ACCEPT WS-FIM

             EVALUATE WS-FIM
             WHEN 'Y'
                PERFORM P100-MAIN
             WHEN 'N'
                PERFORM P900-TERMINAL
             WHEN OTHER
                PERFORM P900-TERMINAL
             END-EVALUATE


       .
       P200-FIM.






       P900-TERMINAL.
            STOP RUN.
       END PROGRAM OPERACOES-MATEMATICAS.
