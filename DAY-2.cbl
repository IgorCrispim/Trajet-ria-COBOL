      ******************************************************************
      * Author:    IGOR CRISPIM
      * Date:      25/08/2025
      * Purpose:   DAY-2
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY-2.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-DIAS-SEMANA.
          03 FILLER                 PIC X(09) VALUE '01DOMINGO'.
          03 FILLER                 PIC X(09) VALUE '02SEGUNDA'.
          03 FILLER                 PIC X(09) VALUE '03TERCA'.
          03 FILLER                 PIC X(09) VALUE '04QUARTA'.
          03 FILLER                 PIC X(09) VALUE '05QUINTA'.
          03 FILLER                 PIC X(09) VALUE '06SEXTA'.
          03 FILLER                 PIC X(09) VALUE '07SABADO'.

       01 FILLER REDEFINES WS-DIAS-SEMANA OCCURS 7 TIMES.
          03 WS-NUM-DIA             PIC 9(02).
          03 WS-NOME-DIA            PIC X(07).
          03 WS-EVENTO OCCURS 5 TIMES.
             05 WS-HORA             PIC X(05).
             05 WS-DESC             PIC X(40).

       77 WS-INDEX                  PIC 9     VALUE ZEROS.
       77 WS-INDEX-2                PIC 9     VALUE ZEROS.
       77 WS-DIA-USUARIO            PIC 9     VALUE ZEROS.
       77 WS-NUM-EVENTO             PIC 9     VALUE ZERO.
       77 WS-SAIDA                  PIC X(01) VALUE 'Q'.




       PROCEDURE DIVISION.
      ******************************************************************
      *      FUNÇÃO INICIAL DO PROGRAMA - PEGA O DIA DA SEMANA
      ******************************************************************
       P100-MAIN.
             DISPLAY 'QUAL O DIA DA SEMANA DESEJA ADICIONAR EVENTOS ?'
             DISPLAY WS-NUM-DIA(2) ' ' WS-NOME-DIA(2)
             DISPLAY WS-NUM-DIA(3) ' ' WS-NOME-DIA(3)
             DISPLAY WS-NUM-DIA(4) ' ' WS-NOME-DIA(4)
             DISPLAY WS-NUM-DIA(5) ' ' WS-NOME-DIA(5)
             DISPLAY WS-NUM-DIA(6) ' ' WS-NOME-DIA(6)
             DISPLAY WS-NUM-DIA(7) ' ' WS-NOME-DIA(7)


             PERFORM P200-AGENDA THRU P200-AGENDA-FIM
       .
      ******************************************************************
      *      FUNÇÃO DE CRIAÇÃO DA AGENDA EM SÍ
      ******************************************************************
       P200-AGENDA.
             MOVE ZEROS TO WS-INDEX
             ACCEPT WS-DIA-USUARIO
             PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 7
                IF  WS-INDEX = WS-NUM-DIA(WS-INDEX)
                AND WS-INDEX = WS-DIA-USUARIO THEN
                   DISPLAY 'QUAL O NUMERO DO EVENTO DE 1 A 5? '
                   ACCEPT WS-NUM-EVENTO
                   DISPLAY 'QUAL O HORARIO DO EVENTO? '
                   ACCEPT WS-HORA(WS-INDEX, WS-NUM-EVENTO)
                   DISPLAY 'QUAL A DESCRICAO DO EVENTO? '
                   ACCEPT WS-DESC(WS-INDEX, WS-NUM-EVENTO)
                END-IF
             END-PERFORM

             DISPLAY 'DESEJA ADICIONAR MAIS EVENTOS AA AGENDA ? '
                     '"Y" FOR YES OR "Q" FOR QUIT'
             ACCEPT WS-SAIDA
             IF WS-SAIDA = 'Y' THEN
                PERFORM P100-MAIN
             ELSE
                PERFORM P300-PRINT THRU P300-PRINT-FIM
             END-IF



       .
       P200-AGENDA-FIM.

      ******************************************************************
      *      FUNÇÃO DE DISPLAY DA AGENDA
      ******************************************************************
       P300-PRINT.
             DISPLAY 'ANTES DE SAIR DESEJA VER A SUA AGENDA SEMANAL ?'
             DISPLAY '"Y" FOR YES OR "Q" FOR QUIT'
             MOVE 'X' TO WS-SAIDA
             ACCEPT WS-SAIDA

             IF WS-SAIDA = 'Y' THEN
                MOVE ZEROS TO WS-INDEX
                              WS-INDEX-2
                PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 7
                   PERFORM VARYING WS-INDEX-2 FROM 1 BY 1 UNTIL
                                   WS-INDEX-2 > 5
                      IF WS-HORA(WS-INDEX,WS-INDEX-2) NOT = SPACES
                         DISPLAY ' '
                         DISPLAY WS-NOME-DIA(WS-INDEX)
                               ' EVENTO: ' WS-INDEX-2
                               ' HORA: ' WS-HORA(WS-INDEX,WS-INDEX-2)
                               ' DESCRICAO: '
                               WS-DESC(WS-INDEX, WS-INDEX-2)
                         DISPLAY ' '
                      ELSE
                         DISPLAY 'BUCETINHA'
                      END-IF
                   END-PERFORM
                END-PERFORM
                PERFORM P900-TERMINAL

             ELSE
                PERFORM P900-TERMINAL
             END-IF



       .
       P300-PRINT-FIM.

      ******************************************************************
      *      FUNÇÃO DE FINALIZAR O PROGRAMA
      ******************************************************************
       .
       P900-TERMINAL.
            STOP RUN.
       END PROGRAM DAY-2.
