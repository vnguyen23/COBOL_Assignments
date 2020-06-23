       program-id. Conundrum2 as "Conundrum2".

       environment division.
       configuration section.

       data division.
       working-storage section.
       01 WS-COUNTS.
       03 WS-COUNT-1 PIC 99.
       03 WS-COUNT-2 PIC 99.
       03 WS-COUNT-3 PIC 99.
       01 WS-STRING PIC X(20) VALUE "AARDVARK EXTRA".
       procedure division.

       INITIALIZE WS-COUNTS

            INSPECT WS-STRING TALLYING WS-COUNT-1 FOR ALL "A"
                       REPLACING LEADING "A" BY "B"
                       ALL "R" BY "S"
       display "WS-COUNT-1:" WS-COUNT-1
       display "WS-COUNT-2:" WS-COUNT-2
       display "WS-COUNT-3:" WS-COUNT-3
       display "WS-STRING:" WS-STRING
               
       end program Conundrum2.
