      ******************************************************************
      * Author:    IGOR CRISPIM
      * Date:      21/08/2025
      * Purpose:   ARRAY COM OCCURS NO COBOL
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARRAY-OCCURS.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-DIAS-SEMANA.
          03 FILLER                  PIC X(09) VALUE '01DOMINGO'.
          03 FILLER                  PIC X(09) VALUE '02SEGUNDA'.
          03 FILLER                  PIC X(09) VALUE '03TERCA'.
          03 FILLER                  PIC X(09) VALUE '04QUARTA'.
          03 FILLER                  PIC X(09) VALUE '05QUINTA'.
          03 FILLER                  PIC X(09) VALUE '06SEXTA'.
          03 FILLER                  PIC X(09) VALUE '07SABADO'.

       01 FILLER REDEFINES WS-DIAS-SEMANA OCCURS 7 TIMES.
          03 WS-NUM-DIA              PIC 99.
          03 WS-NOME-DIA             PIC X(07).

       77 WS-INDEX                   PIC 999 VALUE ZEROS.


       PROCEDURE DIVISION.
       P100-MAIN.
             DISPLAY '******* PROGRAMA DE ARRAY *******'

             PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 7
                DISPLAY 'NUMERO: ' WS-NUM-DIA(WS-INDEX)' NOME: '
                         WS-NOME-DIA(WS-INDEX)
             END-PERFORM





       .
       P900-TERMINAL.
            STOP RUN.
       END PROGRAM ARRAY-OCCURS.
