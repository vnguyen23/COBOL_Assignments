       identification division.
       program-id. Perform_Until as "Perform_Until".

       environment division.
       configuration section.

       data division.
       working-storage section.
       01 WS-COUNTER PIC 9.

       procedure division.
       MAINLINE SECTION.
       START-UP.
           PERFORM INIT-SECT
           PERFORM LOOP-SECT UNTIL WS-COUNTER > 4
           PERFORM END-PARA
           STOP "Press <CR> to stop"
           STOP RUN.

      *----------------------------------------
       INIT-SECT SECTION.
       INIT-PARA.
           DISPLAY "IN INIT PARA"
           MOVE ZERO TO WS-COUNTER.
       INIT-EXIT.
           EXIT.

      *----------------------------------------
       LOOP-SECT SECTION.
       LOOP-PARA.
           ADD 1 TO WS-COUNTER
           DISPLAY "DOING LOOP PARA..." WS-COUNTER.
       LOOP-EXIT.
           EXIT.

      *-----------------------------------------
       END-SECT SECTION.
       END-PARA.
           DISPLAY "IN END PARA - STOPPING".
       END-EXIT.
           EXIT.

       end program Perform_Until.