      ******************************************************************
      *    AUTOR:                              IGOR CRISPIM
      *    DATA:                               16/06/2025
      *    OBJETIVO:                           TRABALHAR A ESTRUTURAÇÃO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AULA-ESTRUTURA.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       77 WS-MOSTRA                  PIC   X(020)   VALUE SPACES.

       LINKAGE SECTION.
       SCREEN SECTION.
      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
      *                  SEÇÃO PRINCIPAL DO CÓDIGO
      ******************************************************************
       MAIN-PROC SECTION.
       P100-MAIN.
          MOVE 'P100-MAIN '      TO WS-MOSTRA
          DISPLAY WS-MOSTRA
          PERFORM SEC-PROC.

       P100-MAIN-FIM.
       P200-MAIN.
          MOVE  'P200-MAIN '      TO WS-MOSTRA
          DISPLAY WS-MOSTRA
          PERFORM P200-SEC.

       P200-MAIN-FIM.

       P300-MAIN.
          IF WS-MOSTRA NOT EQUAL 'P200-SEC ' THEN
             MOVE  'P300-MAIN '      TO WS-MOSTRA
             DISPLAY WS-MOSTRA
             PERFORM FIM-PROC
          ELSE
             PERFORM FIM-PROC
          END-IF.
       P300-MAIN-FIM.
      ******************************************************************
      *                  SEGUNDA SEÇÃO
      ******************************************************************
       SEC-PROC SECTION.
       P100-SEC.

          IF WS-MOSTRA NOT EQUAL 'P100-SEC ' THEN
             MOVE  'P100-SEC '      TO WS-MOSTRA
             DISPLAY WS-MOSTRA
             PERFORM P200-MAIN THRU P200-MAIN-FIM
          END-IF.
       P200-SEC.
          MOVE 'P200-SEC ' TO WS-MOSTRA
          DISPLAY WS-MOSTRA
          PERFORM P300-MAIN THRU P300-MAIN-FIM.
      ******************************************************************
      *                  FINALIZAR O CÓDIGO
      ******************************************************************
       FIM-PROC SECTION.
          DISPLAY 'FIM DO CÓDIGO'
          STOP RUN.
       END PROGRAM AULA-ESTRUTURA.
      ******************************************************************
