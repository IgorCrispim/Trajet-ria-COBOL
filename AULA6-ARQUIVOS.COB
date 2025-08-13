      ******************************************************************
      * Author:                           IGOR CRISPIM
      * Date:                             12/06/2025
      * Purpose:                          DEMONSTRAR TECNICAS DE ARQUIVO
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID. AULA7-WSS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT STUDENT ASSIGN TO
       'C:\Users\igorc\Downloads\arquivo teste.txt'
       ORGANIZATION IS SEQUENTIAL.


       DATA DIVISION.


       FILE SECTION.
       FD  STUDENT.
       01  STUDENT-FILE.
           03 STUDENT-ID                  PIC    9(005).
           03 STUDENT-NAME                PIC    A(020).



       WORKING-STORAGE SECTION.
       01  WS-DATA                        PIC    X(025) VALUE SPACES.
       01  FILLER REDEFINES WS-DATA.
           03 WS-ID-STUDENT               PIC    9(005).
           03 WS-NM-STUDENT               PIC    A(020).
       77  WS-EOF                         PIC    A      VALUE SPACE.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.

           OPEN INPUT STUDENT.
           PERFORM UNTIL WS-EOF = 'F'
             READ STUDENT INTO WS-DATA
                AT END MOVE 'F' TO WS-EOF
                   NOT AT END
                      DISPLAY WS-ID-STUDENT ' - ' WS-NM-STUDENT
              END-READ
           END-PERFORM.
           CLOSE STUDENT.
            STOP RUN.

       END PROGRAM AULA7-WSS.
