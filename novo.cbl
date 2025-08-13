       identification division.
       PROGRAM-ID.             novo.
      *AUTOR                   IGOR CRISPIM
      *DATA                    11/06/2025
      *DATA-ATUALIZACAO        11/06/2025

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  800-WHEN-COMPILED.
           05  800-COMPILED-DATE-YYYY     pic X(4) VALUE SPACES.
           05  800-COMPILED-DATE-MM       pic X(2) VALUE SPACES.
           05  800-COMPILED-DATE-DD       pic X(2) VALUE SPACES.
           05  800-COMPILED-TIME-HH       pic X(2) VALUE SPACES.
           05  800-COMPILED-TIME-MM       pic X(2) VALUE SPACES.
           05  800-COMPILED-TIME-SS       pic X(2) VALUE SPACES.
           05  FILLER                     PIC X(7) VALUE SPACES.
       01  800-CURRENT-DATE.
           05  800-CURRENT-DATE-YYYY     pic X(4) value SPACES.
           05  800-CURRENT-DATE-MM       pic X(2) value SPACES.
           05  800-CURRENT-DATE-DD       pic X(2) value SPACES.
           05  800-CURRENT-TIME-HH       pic X(2) value SPACES.
           05  800-CURRENT-TIME-MM       pic X(2) value SPACES.
           05  800-CURRENT-TIME-SS       pic X(2) value SPACES.
           05  FILLER                    PIC X(7) VALUE SPACES.

       01  WS-DATA-FORMATADO-BR.
           05 WS-DATA-FORMATADO-BR       PIC X(21) VALUE SPACES.
           05 WS-DATA-FORMATADO-BR-R REDEFINES WS-DATA-FORMATADO-BR.
               08  WS-DATE-YYY      pic X(4).
               08  WS-DATE-MM       pic X(2).
               08  WS-DATE-DD       pic X(2).
               08  WS-TIME-HH       pic X(2).
               08  WS-TIME-MM       pic X(2).
               08  WS-TIME-SS       pic X(2).
               08  FILLER           PIC X(7).

       PROCEDURE DIVISION.
       MAIN.
           DISPLAY 'INICIO DA JORNADA COBOL'
           MOVE function WHEN-COMPILED TO 800-WHEN-COMPILED
           MOVE FUNCTION CURRENT-DATE TO 800-CURRENT-DATE
           MOVE FUNCTION CURRENT-DATE TO WS-DATA-FORMATADO-BR-R
           DISPLAY 'TESTANDO A DATA DE COMPILACAO'
           DISPLAY 'DATA DE COMPILACAO: '
               800-COMPILED-DATE-YYYY  '/'
               800-COMPILED-DATE-MM    '/'
               800-COMPILED-DATE-DD    SPACE
               800-COMPILED-TIME-HH    ':'
               800-COMPILED-TIME-MM    ':'
               800-COMPILED-TIME-SS
           DISPLAY 'TESTANDO A DATA ATUAL'
           DISPLAY 'DATA ATUAL: '
               800-CURRENT-DATE-YYYY  '/'
               800-CURRENT-DATE-MM    '/'
               800-CURRENT-DATE-DD    SPACE
               800-CURRENT-TIME-HH    ':'
               800-CURRENT-TIME-MM    ':'
               800-CURRENT-TIME-SS
           DISPLAY 'DATA ATUAL pt/br: '
               WS-DATE-DD          '/'
               WS-DATE-MM          '/'
               WS-DATE-YYY         SPACE
               WS-TIME-HH          ':'
               WS-TIME-MM          ':'
               WS-TIME-SS

           DISPLAY 'EXECUÇÃO FEITA COM SUCESSO'
           STOP RUN.
