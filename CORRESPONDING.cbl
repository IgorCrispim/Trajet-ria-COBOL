      ******************************************************************
      * Author:    IGOR CRISPIM
      * Date:      19/08/2025
      * Purpose:   UTILIZA플O DA ISNTRU플O CORRESPONDING
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CORRESPONDING-LEARNING.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-REG-1.
          03 WS-NOME                    PIC X(10).
          03 WS-TEL                     PIC X(09).
          03 WS-SALARIO                 PIC 9(04)V99.
          03 WS-UF                      PIC X(02).


       01 WS-REG-2.
          03 WS-NOME                    PIC X(10).
          03 WS-TEL                     PIC X(09).
          03 WS-SALARIO                 PIC 9(04)V99.
          03 WS-UF                      PIC X(02).






       PROCEDURE DIVISION.
      ******************************************************************
      *                        FUN플O PRINCIPAL
      ******************************************************************
       P100-INICIO.

             MOVE 'MARCOS'               TO WS-NOME     OF WS-REG-1
             MOVE '9999-9999'            TO WS-TEL      OF WS-REG-1
             MOVE 2300.50                TO WS-SALARIO  OF WS-REG-1
             MOVE 'CE'                   TO WS-UF       OF WS-REG-1

             DISPLAY '***** REG 1 ******'
             DISPLAY WS-NOME                            OF WS-REG-1
             DISPLAY WS-TEL                             OF WS-REG-1
             DISPLAY WS-SALARIO                         OF WS-REG-1
             DISPLAY WS-UF                              OF WS-REG-1


             DISPLAY '***** REG 2 ******'
             DISPLAY WS-NOME                            OF WS-REG-2
             DISPLAY WS-TEL                             OF WS-REG-2
             DISPLAY WS-SALARIO                         OF WS-REG-2
             DISPLAY WS-UF                              OF WS-REG-2

             MOVE WS-REG-1               TO WS-REG-2

             DISPLAY WS-REG-1
             DISPLAY WS-REG-2

             INITIALISE WS-REG-2

             MOVE CORR WS-REG-1          TO WS-REG-2
             DISPLAY 'USO DE MOVE CORR: '

             DISPLAY '***** REG 1 ******'
             DISPLAY WS-NOME                            OF WS-REG-1
             DISPLAY WS-TEL                             OF WS-REG-1
             DISPLAY WS-SALARIO                         OF WS-REG-1
             DISPLAY WS-UF                              OF WS-REG-1


             DISPLAY '***** REG 2 ******'
             DISPLAY WS-NOME                            OF WS-REG-2
             DISPLAY WS-TEL                             OF WS-REG-2
             DISPLAY WS-SALARIO                         OF WS-REG-2
             DISPLAY WS-UF                              OF WS-REG-2


             DISPLAY WS-REG-1
             DISPLAY WS-REG-2


             ADD 200                     TO WS-SALARIO  OF WS-REG-1

             DISPLAY '***** REG 1 - COM ADD******'
             DISPLAY WS-NOME                            OF WS-REG-1
             DISPLAY WS-TEL                             OF WS-REG-1
             DISPLAY WS-SALARIO                         OF WS-REG-1
             DISPLAY WS-UF                              OF WS-REG-1

             SUBTRACT 50                FROM WS-SALARIO OF WS-REG-1

             DISPLAY '***** REG 1 - COM SUBTRACT ******'
             DISPLAY WS-NOME                            OF WS-REG-1
             DISPLAY WS-TEL                             OF WS-REG-1
             DISPLAY WS-SALARIO                         OF WS-REG-1
             DISPLAY WS-UF                              OF WS-REG-1

             ADD CORR WS-REG-1          TO WS-REG-2
             DISPLAY 'USO DE ADD CORR: '

             DISPLAY '***** REG 1 ******'
             DISPLAY WS-NOME                            OF WS-REG-1
             DISPLAY WS-TEL                             OF WS-REG-1
             DISPLAY WS-SALARIO                         OF WS-REG-1
             DISPLAY WS-UF                              OF WS-REG-1


             DISPLAY '***** REG 2 ******'
             DISPLAY WS-NOME                            OF WS-REG-2
             DISPLAY WS-TEL                             OF WS-REG-2
             DISPLAY WS-SALARIO                         OF WS-REG-2
             DISPLAY WS-UF                              OF WS-REG-2

             SUBTRACT CORR WS-REG-1     FROM WS-REG-2
             DISPLAY 'USO DE SUBTRACT CORR: '

             DISPLAY '***** REG 1 ******'
             DISPLAY WS-NOME                            OF WS-REG-1
             DISPLAY WS-TEL                             OF WS-REG-1
             DISPLAY WS-SALARIO                         OF WS-REG-1
             DISPLAY WS-UF                              OF WS-REG-1


             DISPLAY '***** REG 2 ******'
             DISPLAY WS-NOME                            OF WS-REG-2
             DISPLAY WS-TEL                             OF WS-REG-2
             DISPLAY WS-SALARIO                         OF WS-REG-2
             DISPLAY WS-UF                              OF WS-REG-2








       .
      ******************************************************************
      *                        FUN플O DE FINALIZAR O PROGRAMA
      ******************************************************************
       P900-TERMINAL.
            STOP RUN.
       END PROGRAM CORRESPONDING-LEARNING.
