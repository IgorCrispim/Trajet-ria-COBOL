      ******************************************************************
      * Author: IGOR CRISPIM
      * Date:   13/08/2025
      * Purpose:   APRENDIZADO DE ESTRUTURAS DE DECISÃO COMBINADAS
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AULA-ESTRUTURAS-DECISAO.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       77 WS-MES                              PIC 99 VALUE ZEROS.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
          DISPLAY 'INFORME UM NUMERO DE MES: '
          ACCEPT WS-MES

          EVALUATE WS-MES
             WHEN 01
                DISPLAY 'JANEIRO'
             WHEN 02
                DISPLAY 'FEVEREIRO'
             WHEN 03
                DISPLAY 'MARCO'
             WHEN 04
                DISPLAY 'ABRIL'
             WHEN 05
                DISPLAY 'MAIO'
             WHEN 06
                DISPLAY 'JUNHO'
             WHEN 07
                DISPLAY 'JULHO'
             WHEN 08
                DISPLAY 'AGOSTO'
             WHEN 09
                DISPLAY 'SETEMBRO'
             WHEN 10
                DISPLAY 'OUTUBRO'
             WHEN 11
                DISPLAY 'NOVEMBRO'
             WHEN 12
                DISPLAY 'DEZEMBRO'
             WHEN OTHER
                IF WS-MES NOT IS NUMERIC
                   DISPLAY 'ERRO: DIGITE APENAS NUMEROS'
                   PERFORM MAIN-PROCEDURE
                ELSE
                   DISPLAY 'ERRO: MES INVALIDO'
                   PERFORM MAIN-PROCEDURE
                END-IF
             END-EVALUATE.




       P900-FIM.
            STOP RUN.
       END PROGRAM AULA-ESTRUTURAS-DECISAO.
