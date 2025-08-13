      ******************************************************************
      * Author:                                IGOR CRISPIM
      * Date:                                  14/06/2025
      * Purpose:                               APRENDENDO STRING
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AULA-STRING.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 WS-STRING                   PIC X(50) VALUE SPACES.
       77 WS-DISPLAY                  PIC X(99) VALUE SPACES.
       77 WS-DISPLAY2                 PIC X(80) VALUE SPACES.
       77 WS-POINTER                  PIC 9(09) VALUE ZEROS.
        01 WS-ENDERECO.
          03 WS-RUA                   PIC X(30) VALUE SPACES.
          03 WS-NUMERO                PIC 9(04) VALUE ZEROS.
          03 WS-BAIRRO                PIC X(30) VALUE SPACES.
          03 WS-CIDADE                PIC X(20) VALUE SPACES.
          03 WS-UF                    PIC X(02) VALUE SPACES.
          03 WS-CEP                   PIC X(09) VALUE SPACES.

       PROCEDURE DIVISION.
      ******************************************************************
      *                   INICIALIZA AS VARIAVEIS
      ******************************************************************
       P100-INICIO.
             MOVE 'RUA 310'          TO WS-RUA
             MOVE 301                TO WS-NUMERO
             MOVE 'SANTA CLARA'      TO WS-BAIRRO
             MOVE 'SAO PAULO'        TO WS-CIDADE
             MOVE 'SP'               TO WS-UF
             MOVE '01500-005'        TO WS-CEP
             DISPLAY WS-ENDERECO

           PERFORM P200-INICIO THRU P200-FIM.
       P100-FIM.
      ******************************************************************
      *                   REALIZA AS CONCATENACOES
      ******************************************************************
       P200-INICIO.
             STRING WS-ENDERECO
                DELIMITED BY SIZE INTO WS-DISPLAY
             END-STRING.

           DISPLAY 'PRIMEIRO: ' WS-DISPLAY.
             STRING WS-RUA(1:12)
                    ' - '
                    WS-BAIRRO(1:5)
                    ' - '
                    WS-CEP(1:8)
                DELIMITED BY SIZE INTO WS-DISPLAY2
           END-STRING.
           DISPLAY 'SEGUNDO: ' WS-DISPLAY2.

       P200-FIM.
            STOP RUN.
       END PROGRAM AULA-STRING.
