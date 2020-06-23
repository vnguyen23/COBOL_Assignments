       identification division.
       program-id. Perform_Verb as "Perform_Verb".

       environment division.
       
       data division.
       working-storage section.
       01  WS-TIMES PIC 9(4).
       01  WS-NUM1  PIC 9(4).
       01  WS-NUM2  PIC 9(4).
       01  WS-NUM3  PIC 9(6).

       procedure division.
       PROG.
           PERFORM INIT-PARA
           PERFORM LOOP-PARA WS-TIMES TIMES
           PERFORM END-PARA.
           
       INIT-PARA.
           DISPLAY "A SIMPLE MULTIPLIER"
           DISPLAY "HOW MANY TIMES (1 TO 6)?"
           ACCEPT WS-TIMES
           IF (WS-TIMES > 6) OR (WS-TIMES < 1)
               MOVE 1 TO WS-TIMES
           END-IF.
       
       LOOP-PARA.
           DISPLAY "FIRST NUMBER?"
           ACCEPT WS-NUM1
           DISPLAY "SECOND NUMBER?"
           ACCEPT WS-NUM2
           MULTIPLY WS-NUM1 BY WS-NUM2 GIVING WS-NUM3
           DISPLAY "PRODUCT OF " WS-NUM1 " AND "WS-NUM2 " IS " WS-NUM3.
           
       END-PARA.
           DISPLAY "MULTIPLICATION DONE " WS-TIMES " TIME(S)"
           DISPLAY "THANK YOU FOR THAT!"
           STOP RUN.
           
       end program Perform_Verb.
