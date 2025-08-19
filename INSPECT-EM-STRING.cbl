      ******************************************************************
      * Author:    IGOR CRISPIM
      * Date:      18/08/2025
      * Purpose:   UTILIZAÇÃO DE INSPECT EM UMA STRING
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSPECT-STRING.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 WS-STRING                  PIC   X(50)
          VALUE 'TODAS AS COMPRAS FEITAS EM 01/07 NAO TEM PREMIACAO'.

       77 WS-TESTE                   PIC   X(2)
          VALUE 'EM'.

       77 WS-TESTE-2                 PIC   X(1).

       01 WS-CONTADORES.
          03 WS-CONT-1                PIC   99.
          03 WS-CONT-2                PIC   99.
          03 WS-CONT-3                PIC   99.
          03 WS-CONT-4                PIC   99.
          03 WS-CONT-5                PIC   99.
          03 WS-CONT-6                PIC   99.
          03 WS-CONT-7                PIC   99.


       01 WS-CONTADORES-2.
          03 WS-CONT-2-1                PIC   99.
          03 WS-CONT-2-2                PIC   99.
          03 WS-CONT-2-3                PIC   99.


       PROCEDURE DIVISION.
      ******************************************************************
      *                     FUNÇÃO MAIN DO CÓDIGO
      ******************************************************************
       P100-INICIO.
             DISPLAY 'REGUA DE CARACT: '
             '01234567890123456789012345678901234567890123456789'


             DISPLAY 'STRING ORIGINAL: '   WS-STRING.

             DISPLAY 'QUAL CARACTERE VOCE DESEJAR CONTAR NA FRASE? '
             ACCEPT WS-TESTE-2

      ****************** FORMATO 1: CONTAGEM DE CARACTERES**************
             INITIALISE WS-CONTADORES
             INSPECT WS-STRING TALLYING WS-CONT-1 FOR ALL ZEROS
             DISPLAY 'QUANTIDADE DE ZEROS NA STRING: ' WS-CONT-1

             INSPECT WS-STRING TALLYING WS-CONT-2 FOR ALL SPACES
             DISPLAY 'QUANTIDADE DE ESPACOS: ' WS-CONT-2

             INSPECT WS-STRING TALLYING WS-CONT-3 FOR ALL 'A'
             DISPLAY 'QUANTIDADE DE LETRAS "A": ' WS-CONT-3

             INSPECT WS-STRING TALLYING WS-CONT-4 FOR ALL WS-TESTE
             DISPLAY 'QUANTIDADE DE "EM": ' WS-CONT-4

             INSPECT WS-STRING TALLYING WS-CONT-5 FOR CHARACTERS
                BEFORE INITIAL ZEROS
             DISPLAY 'QUANTIDADE DE CARACTERES '
                     'ANTES DO ZERO : ' WS-CONT-5

             INSPECT WS-STRING TALLYING WS-CONT-6 FOR CHARACTERS
                AFTER INITIAL ZEROS
             DISPLAY 'QUANTIDADE DE CARACTERES '
                     'DEPOIS DO ZERO : ' WS-CONT-6

             INSPECT WS-STRING TALLYING WS-CONT-7 FOR ALL WS-TESTE-2
             DISPLAY 'QUANTIDADE DE CARACTERES BUSCADOS: ' WS-CONT-7

      ****************** FORMATO 2: REPLACE*****************************
             DISPLAY 'METODO 2'

             INITIALISE WS-CONTADORES-2
             DISPLAY 'TROCANDO "A" POR "X": '
             INSPECT WS-STRING REPLACING ALL 'A' BY 'X'
             DISPLAY 'NOVA STRING: ' WS-STRING

             DISPLAY 'TROCANDO "X" POR "Y" DEPOIS DO PRIMEIRO X: '
             INSPECT WS-STRING REPLACING ALL 'X' BY '#'
                AFTER INITIAL 'X'
             DISPLAY WS-STRING

             DISPLAY 'TROCANDO TODOS OS CARACTERES ANTERIORES A # POR $'
             INSPECT WS-STRING REPLACING CHARACTERS BY '$'
                BEFORE INITIAL '#'
             DISPLAY WS-STRING

             DISPLAY 'TROCANDO O PRIMEIRO "#" POR "w": '
             INSPECT WS-STRING REPLACING FIRST '#' BY 'W'
             DISPLAY WS-STRING
      ****************** FORMATO 3: SUBSTITUIÇÃO INTELIGENTE************
             DISPLAY 'METODO 3: SUBSTITUICAO INTELIGENTE: '
             INITIALISE WS-CONTADORES

             DISPLAY 'CONTANDO ALTERACAO DE "#S" NUMA CONDICAO...'
             INSPECT WS-STRING TALLYING WS-CONT-2-1 FOR ALL '#S'
                AFTER ' C' BEFORE 'T'
             DISPLAY 'TAMANHO DO CORTE: ' WS-CONT-2-1
             DISPLAY 'NOVA STRING: ' WS-STRING

             DISPLAY 'FAZENDO ALTERACAO DE "#S" NUMA CONDICAO...'
             INSPECT WS-STRING REPLACING ALL '#S' BY '11'
                AFTER ' C' BEFORE 'T'
             DISPLAY 'TAMANHO DO CORTE: ' WS-CONT-2-1
             DISPLAY 'NOVA STRING: ' WS-STRING

      ****************** FORMATP 4: CONVERSAO INTELIGENTE***************
             INITIALISE WS-CONTADORES
             DISPLAY 'METODO 4: '
             DISPLAY 'CONVERTE UMA COLECAO DE 11 POR CARACTERES '
                     'DE IGUAIS QUANTIDADES'
             INSPECT WS-STRING CONVERTING '11' TO ALL '@@'
             DISPLAY 'NOVA STRING: ' WS-STRING

             PERFORM P900-TERMINAL


           .
       P100-FIM.


       P900-TERMINAL.
            STOP RUN.
       END PROGRAM INSPECT-STRING.
