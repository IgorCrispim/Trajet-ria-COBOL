      ******************************************************************
      * Author:    IGOR CRISPIM
      * Date:      22/08/2025
      * Purpose:   ARRAY COM OCCURS NO COBOL - DINÂMICO
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARRAY-OCCURS-2.
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
       77 WS-VLR-AUX                 PIC 999 VALUE ZEROS.

       01 WS-CONTRATO.
          03 WS-REG-FIN.
             05 WS-NUM-CONTRATO      PIC 9(05).
             05 WS-NUM-PRESTACOES    PIC 9(09).
             05 WS-PRESTACOES OCCURS 1 TO 420 TIMES
                              DEPENDING ON WS-NUM-PRESTACOES
                              PIC 9(9)V99.





       PROCEDURE DIVISION.
       P100-MAIN.
      *       DISPLAY '******* PROGRAMA DE ARRAY *******'

      *       PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 7
      *          DISPLAY 'NUMERO: ' WS-NUM-DIA(WS-INDEX)' NOME: '
      *                   WS-NOME-DIA(WS-INDEX)
      *       END-PERFORM



              DISPLAY 'INFORME O NUMERO DE PRESTACOES: '
              ACCEPT WS-NUM-PRESTACOES

              DISPLAY 'INFORME O VALOR DA PRESTACAO: '
              ACCEPT WS-VLR-AUX


              IF WS-NUM-PRESTACOES > 420 OR WS-NUM-PRESTACOES < 1 THEN
                 DISPLAY 'NUMERO INVALIDO, TENTE NOVAMENTE: '
                 PERFORM P100-MAIN
              ELSE
                 MOVE ZEROS TO WS-INDEX
                 PERFORM VARYING WS-INDEX FROM 1 BY 1
                 UNTIL WS-INDEX > WS-NUM-PRESTACOES

                   MOVE WS-VLR-AUX      TO WS-PRESTACOES(WS-INDEX)

                 END-PERFORM

              END-IF


              MOVE ZEROS TO WS-INDEX
              PERFORM VARYING WS-INDEX FROM 1 BY 1
              UNTIL WS-INDEX > WS-NUM-PRESTACOES

                DISPLAY 'PRETACAO: 'WS-INDEX
                        ' VALOR: '  WS-PRESTACOES(WS-INDEX)

              END-PERFORM




       .
       P900-TERMINAL.
            STOP RUN.
       END PROGRAM ARRAY-OCCURS-2.
