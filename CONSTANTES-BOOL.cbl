      ******************************************************************
      * Author:    IGOR CRISPIM
      * Date:      01/09/2025
      * Purpose:   IMPLEMENTAÇÃO DE CONSTANTES E BOOLEANOS
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONSTANTES-BOOL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-REGISTRO.
          03 WS-NOME                       PIC X(30).
          03 WS-BRASILEIRO                 PIC X.
             88 WS-BRASIL-OK               VALUE 'T' FALSE 'F'.
          03 WS-CAMBIO                     PIC 9.
             88 WS-TP-CAMBIO               VALUE 1 THRU 3.
.

       PROCEDURE DIVISION.
       P100-MAIN.
             DISPLAY 'INFORME O NOME DO OPERADOR'
             ACCEPT WS-NOME
             DISPLAY 'CIDADAO BRASILEIRO? <T> OR <F> '
             ACCEPT WS-BRASILEIRO
             DISPLAY 'INFORME A MOEDA DE OPERACAO: '
             DISPLAY '1 - REAL BRASILEIRO'
             DISPLAY '2 - DOLAR AMERICANO'
             DISPLAY '3 - EURO'
             ACCEPT WS-CAMBIO

             DISPLAY 'RESULTADOS: '
             DISPLAY 'OPERADOR: ' WS-NOME

             IF WS-BRASIL-OK THEN
                DISPLAY 'CIDADAO BRASILEIRO'
             ELSE
                DISPLAY 'CIDADAO ESTRANGEIRO'
             END-IF

             EVALUATE WS-CAMBIO
                WHEN 1
                   DISPLAY 'MOEDA REAL BRASILEIRO'
                WHEN 2
                   DISPLAY 'MOEDA DOLAR AMERICANO'
                WHEN 3
                   DISPLAY 'MOEDA EURO'
                WHEN OTHER
                   DISPLAY 'MOEDA INVALIDA'
             END-EVALUATE

             PERFORM P900-TERMINAL




       .
       P900-TERMINAL.
            STOP RUN.
       END PROGRAM CONSTANTES-BOOL.
