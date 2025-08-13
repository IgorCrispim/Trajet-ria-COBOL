      ******************************************************************
      * Author: Igor Crispim
      * Date:   12/08/2025
      * Purpose:   Aprendizado sobre Arquivos e Layouts
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VOLTA-AS-AULAS-2.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIENTE ASSIGN TO
           'C:\Users\igorc\OneDrive\Documentos\CLIENTE.TXT'
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS  IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD CLIENTE.
       01 REG-CLIENTE.
          03 CD-CLIENTE           PIC 9(03).
          03 NM-CLIENTE           PIC X(20).
          03 TL-CLIENTE           PIC X(09).

       WORKING-STORAGE SECTION.
       77 WS-FS                   PIC 99.
       77 WS-EOF                  PIC 99.

       01 WS-DADOS.
          03 WS-CD-CLIENTE           PIC 9(03).
          03 WS-NM-CLIENTE           PIC X(20).
          03 WS-TL-CLIENTE           PIC X(09).



       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY '-----   LISTAGEM DE CLIENTE   -----'
            SET WS-FS             TO 0.
            SET WS-EOF            TO 0.


            OPEN INPUT CLIENTE

            PERFORM UNTIL WS-EOF EQUAL 1
                READ CLIENTE INTO WS-DADOS
                AT END
                   MOVE 1 TO WS-EOF
                NOT AT END
                   DISPLAY 'CODIGO: ' WS-CD-CLIENTE
                           ' NOME: ' WS-NM-CLIENTE
                           ' NUMERO: ' WS-TL-CLIENTE
                END-READ

            END-PERFORM



            CLOSE CLIENTE

            STOP RUN.
       END PROGRAM VOLTA-AS-AULAS-2.
