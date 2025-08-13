      ******************************************************************
      * Author:                            IGOR CRISPIM
      * Date:                              12/06/2025
      * Purpose:                           DECIDIR APROVAÇÃO DE ALUNOS
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                         AULA4.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  WS-NOTA1       PIC      9(004)V99 VALUES ZEROS.
       01  WS-NOTA2       PIC      9(004)V99 VALUES ZEROS.
       01  WS-NOTA3       PIC      9(004)V99 VALUES ZEROS.
       01  WS-NOME-ALUNO  PIC      X(022) VALUES SPACE.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY 'DIGITE O NOME DO ALUNO A SER ANALISADO'
           ACCEPT WS-NOME-ALUNO
           DISPLAY 'DIGITE O RESULTADO DA PRIMEIRA NOTA'
           ACCEPT WS-NOTA1
           DISPLAY 'DIGITE O RESULTADO DA SEGUNDA NOTA'
           ACCEPT WS-NOTA2
           DISPLAY 'DIGITE O RESULTADO DA TERCEIRA NOTA'
           ACCEPT WS-NOTA3
           COMPUTE WS-NOTA1 = (WS-NOTA1 + WS-NOTA2 + WS-NOTA3) / 3
           IF WS-NOTA1 >= 7 THEN
              DISPLAY 'APROVADO! '
              DISPLAY 'A MEDIA DO ' WS-NOME-ALUNO ' FOI DE ' WS-NOTA1

           ELSE
              DISPLAY 'REPROVADO! '
              DISPLAY'A MEDIA DO ' WS-NOME-ALUNO ' FOI DE ' WS-NOTA1


            STOP RUN.
       END PROGRAM                         AULA4.
