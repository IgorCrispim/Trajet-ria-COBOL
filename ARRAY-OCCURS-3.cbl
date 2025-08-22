      ******************************************************************
      * Author:    IGOR CRISPIM
      * Date:      22/08/2025
      * Purpose:   ARRAY COM OCCURS NO COBOL - DINÂMICO
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARRAY-OCCURS-3.
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
          03 WS-EVENTO OCCURS 4 TIMES.
             05 WS-NUM-EVENTO        PIC 99.
             05 WS-DESC-EVENTO       PIC X(15).

       77 WS-INDEX-EVENTO            PIC 999   VALUE ZEROS.
       77 WS-INDEX                   PIC 999   VALUE ZEROS.
       77 WS-VLR-AUX                 PIC 999   VALUE ZEROS.
       77 WS-FLAG                    PIC X.



       PROCEDURE DIVISION.
       P100-MAIN.
             DISPLAY 'NUMERO:        NOME: '

             PERFORM VARYING WS-INDEX-EVENTO FROM 1 BY 1
                             UNTIL WS-INDEX > 7
                DISPLAY WS-NUM-DIA(WS-INDEX) '        '
                WS-NOME-DIA(WS-INDEX)
             END-PERFORM

             MOVE 'N'                    TO WS-FLAG
             MOVE ZEROS                  TO WS-INDEX-EVENTO
                                            WS-INDEX

             PERFORM UNTIL WS-FLAG = 'S' OR 's'
                ADD 1                    TO WS-INDEX-EVENTO
                DISPLAY 'INFORME O NUMERO DA SEMANA: '
                ACCEPT WS-INDEX
                DISPLAY 'INFORME O NUMERO DO EVENTO: '
                ACCEPT WS-NUM-EVENTO(WS-INDEX,WS-INDEX-EVENTO)
                DISPLAY 'INFORME A DESCRICAO DO EVENTO: '
                ACCEPT WS-DESC-EVENTO(WS-INDEX, WS-INDEX-EVENTO)

             END-PERFORM


       .
       P900-TERMINAL.
            STOP RUN.
       END PROGRAM ARRAY-OCCURS-3.
