      ******************************************************************
      * Author:                               IGOR CRISPIM
      * Date:                                 12/06/2025
      * Purpose:                              Mostre o nome do mes de um
      *                                       calendário normal
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AULA5.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-DATA                PIC      X(010) VALUE SPACES.
       01  WS-DATA-R     REDEFINES WS-DATA.
           03  WS-DATA-DD         PIC      9(002).
           03  FILLER             PIC      X.
           03  WS-DATA-MM         PIC      9(002).
           03  FILLER             PIC      X.
           03  WS-DATA-YYYY       PIC      9(004).
           03  FILLER             PIC      X.

       01  WS-DATA-NOME           PIC      X(015) VALUE SPACES.
       01  WS-CONTINUACAO         PIC      A(001) VALUES SPACES.

       PROCEDURE DIVISION.
      ******************************************************************
      *      RESETA AS VARIAVEIS E CHAMA AS OUTRAS FUNÇÕES
      ******************************************************************
       P100-INICIO.
           DISPLAY 'INICIO DO PROGRAMA'.
           MOVE SPACES TO WS-DATA
                          WS-DATA-NOME.

           DISPLAY 'INFORME UMA DATA'.
           ACCEPT WS-DATA.
           PERFORM P200-VER-MES-INICIO THRU P200-VER-MES-FIM.

           PERFORM P300-MOSTRAR-INICIO THRU P300-MOSTRAR-FIM.

           PERFORM P900-TERMINAL       THRU P900-TERMINAL-FIM.

       P100-FIM.
      ******************************************************************
      *      DESCOBRE QUAL O MES EM QUESTÃO
      ******************************************************************
       P200-VER-MES-INICIO.
           DISPLAY 'VERIFICANDO QUAL O MES'.
           EVALUATE WS-DATA-MM
             WHEN 01
                MOVE 'JANEIRO' TO WS-DATA-NOME

             WHEN 02
                MOVE 'FEVEREIRO' TO WS-DATA-NOME

             WHEN 03
                MOVE 'MARCO' TO WS-DATA-NOME

             WHEN 04
                MOVE 'ABRIL' TO WS-DATA-NOME

             WHEN 05
                MOVE 'MAIO' TO WS-DATA-NOME

             WHEN 06
                MOVE 'JUNHO' TO WS-DATA-NOME

             WHEN 07
                MOVE 'JULHO' TO WS-DATA-NOME

             WHEN 08
                MOVE 'AGOSTO' TO WS-DATA-NOME

             WHEN 09
                MOVE 'SETEMBRO' TO WS-DATA-NOME

             WHEN 10
                MOVE 'OUTUBRO' TO WS-DATA-NOME

             WHEN 11
                MOVE 'NOVEMBRO' TO WS-DATA-NOME

             WHEN 12
                MOVE 'DEZEMBRO' TO WS-DATA-NOME


           END-EVALUATE.


       P200-VER-MES-FIM.

      ******************************************************************
      *      MOSTRA O RESULTADO PARA O USUARIO
      ******************************************************************
       P300-MOSTRAR-INICIO.
           DISPLAY 'O MES EM QUESTAO E: ' WS-DATA-NOME .
           DISPLAY 'DESEJA CONTINUAR ? '.
           DISPLAY 'TECLE "Y" OU "N" '.
           ACCEPT WS-CONTINUACAO.

           IF WS-CONTINUACAO = "Y" THEN
              PERFORM P100-INICIO THRU P100-FIM

           ELSE IF WS-CONTINUACAO = "N" THEN
              DISPLAY 'FINALIZANDO O PROGRAMA'
              PERFORM P900-TERMINAL THRU P900-TERMINAL-FIM

           ELSE
              DISPLAY 'ESCOLHA UMA TECLA VALIDA'
              PERFORM P400-CONTINUACAO-INICIO THRU P400-CONTINUACAO-FIM

           END-IF.


       P300-MOSTRAR-FIM.

      ******************************************************************
      *      PERGUNTA AO USUARIO SE DESEJA PESQUISAR MAIS UMA DATA
      ******************************************************************
       P400-CONTINUACAO-INICIO.
           DISPLAY 'DESEJA CONTINUAR ? '.
           DISPLAY 'TECLE "Y" OU "N" '.
           ACCEPT WS-CONTINUACAO.

           IF WS-CONTINUACAO = "Y" THEN
              PERFORM P100-INICIO THRU P100-FIM

           ELSE IF WS-CONTINUACAO = "N" THEN
              DISPLAY 'FINALIZANDO O PROGRAMA'
              PERFORM P900-TERMINAL THRU P900-TERMINAL-FIM

           ELSE
              DISPLAY 'ESCOLHA UMA TECLA VALIDA'
              PERFORM P400-CONTINUACAO-INICIO THRU P400-CONTINUACAO-FIM

           END-IF.



       P400-CONTINUACAO-FIM.

      ******************************************************************
      *      FINALIZA O PROGRAMA
      ******************************************************************
       P900-TERMINAL.
            STOP RUN.
       P900-TERMINAL-FIM.
       END PROGRAM AULA5.
