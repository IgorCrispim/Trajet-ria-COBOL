      ******************************************************************
      * Author:                                 IGOR CRISPIM
      * Date:                                   16/06/2025
      * Purpose:                                APRENDIZADO DE UNSTRING
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AULA-UNSTRING.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 WS-POINTER           PIC   9(002)   VALUE ZEROS.
       77 WS-CONTADOR-1        PIC   9(002)   VALUE ZEROS.
       77 WS-CONTADOR-2        PIC   9(002)   VALUE ZEROS.
       77 WS-CONTADOR-3        PIC   9(002)   VALUE ZEROS.
       77 WS-TOT-CAMPOS        PIC   9(002)   VALUE ZEROS.
       77 WS-NOME-COMPLETO     PIC   X(060)   VALUE ZEROS.
       77 WS-PRIMEIRO-NOME     PIC   X(020)   VALUE ZEROS.
       77 WS-NOME-MEIO         PIC   X(020)   VALUE ZEROS.
       77 WS-ULTIMO-NOME       PIC   X(020)   VALUE ZEROS.

       PROCEDURE DIVISION.
      ******************************************************************
      *                    LÓGICA PRINCIPAL DO CÓDIGO
      ******************************************************************
       P100-INICIO.
          DISPLAY 'EXEMPLO1: '
          INITIALISE           WS-POINTER
                               WS-CONTADOR-1
                               WS-CONTADOR-2
                               WS-CONTADOR-3
                               WS-PRIMEIRO-NOME
                               WS-NOME-MEIO
                               WS-ULTIMO-NOME
                               WS-TOT-CAMPOS
          MOVE 'TIAO BALINHA ACEROLA' TO WS-NOME-COMPLETO
          MOVE 1                      TO WS-POINTER

          UNSTRING WS-NOME-COMPLETO
                   DELIMITED BY SPACES
                   INTO WS-PRIMEIRO-NOME
                        WS-NOME-MEIO
                        WS-ULTIMO-NOME


          END-UNSTRING.

          PERFORM P800-DISPLAY THRU P800-DISPLAY-FIM.

          DISPLAY 'EXEMPLO2: '
          INITIALISE           WS-POINTER
                               WS-CONTADOR-1
                               WS-CONTADOR-2
                               WS-CONTADOR-3
                               WS-PRIMEIRO-NOME
                               WS-NOME-MEIO
                               WS-ULTIMO-NOME
                               WS-TOT-CAMPOS
          MOVE 'TIAO*BALINHA;ACEROLA' TO WS-NOME-COMPLETO
          MOVE 1                      TO WS-POINTER

          UNSTRING WS-NOME-COMPLETO
                   DELIMITED BY '*' OR ';'
                   INTO WS-PRIMEIRO-NOME
                        WS-NOME-MEIO
                        WS-ULTIMO-NOME

                   WITH POINTER WS-POINTER
                   TALLYING IN  WS-TOT-CAMPOS

          END-UNSTRING.

          PERFORM P800-DISPLAY THRU P800-DISPLAY-FIM.

          DISPLAY 'EXEMPLO3: '
          INITIALISE           WS-POINTER
                               WS-CONTADOR-1
                               WS-CONTADOR-2
                               WS-CONTADOR-3
                               WS-PRIMEIRO-NOME
                               WS-NOME-MEIO
                               WS-ULTIMO-NOME
                               WS-TOT-CAMPOS
          MOVE 'TIAO*BALINHA;ACEROLA' TO WS-NOME-COMPLETO
          MOVE 1                      TO WS-POINTER

          UNSTRING WS-NOME-COMPLETO
                   DELIMITED BY '*' OR ';'
                   INTO WS-PRIMEIRO-NOME   COUNT IN WS-CONTADOR-1
                        WS-NOME-MEIO       COUNT IN WS-CONTADOR-2
                        WS-ULTIMO-NOME     COUNT IN WS-CONTADOR-3

                   WITH POINTER WS-POINTER
                   TALLYING IN  WS-TOT-CAMPOS

          END-UNSTRING.
          PERFORM P800-DISPLAY THRU P800-DISPLAY-FIM.

          DISPLAY 'EXEMPLO4: '
          INITIALISE           WS-POINTER
                               WS-CONTADOR-1
                               WS-CONTADOR-2
                               WS-CONTADOR-3
                               WS-PRIMEIRO-NOME
                               WS-NOME-MEIO
                               WS-ULTIMO-NOME
                               WS-TOT-CAMPOS
          MOVE 'TIAO*BALINHA;;;;;ACEROLA' TO WS-NOME-COMPLETO
          MOVE 1                          TO WS-POINTER

          UNSTRING WS-NOME-COMPLETO
                   DELIMITED BY '*' OR ALL ';'
                   INTO WS-PRIMEIRO-NOME   COUNT IN WS-CONTADOR-1
                        WS-NOME-MEIO       COUNT IN WS-CONTADOR-2
                        WS-ULTIMO-NOME     COUNT IN WS-CONTADOR-3

                   WITH POINTER WS-POINTER
                   TALLYING IN  WS-TOT-CAMPOS

          END-UNSTRING.
          PERFORM P800-DISPLAY THRU P800-DISPLAY-FIM.
          PERFORM P900-TERMINA THRU P900-FIM.


       P100-FIM.


      ******************************************************************
      *                    FUNÇÃO PARA MOSTRAR NA TELA
      ******************************************************************
       P800-DISPLAY.
          DISPLAY 'PRIMEIRO NOME..: '   WS-PRIMEIRO-NOME.
          DISPLAY 'SEGUNDO NOME...: '   WS-NOME-MEIO.
          DISPLAY 'ULTIMO NOME....: '   WS-ULTIMO-NOME.
          DISPLAY 'PONTEIRO.......: '   WS-POINTER.
          DISPLAY 'TOTAL DE CAMPOS: '   WS-TOT-CAMPOS.
          DISPLAY 'TAMANHO PLVR 1.: '   WS-CONTADOR-1
          DISPLAY 'TAMANHO PLVR 2.: '   WS-CONTADOR-2
          DISPLAY 'TAMANHO PLVR 3.: '   WS-CONTADOR-3
          .


       P800-DISPLAY-FIM.

      ******************************************************************
      *                    FUNÇÃO PARA FINALIZAR O PROGRAMA
      ******************************************************************
       P900-TERMINA.
                      STOP RUN.
       P900-FIM.
       END PROGRAM AULA-UNSTRING.
