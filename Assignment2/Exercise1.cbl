       IDENTIFICATION DIVISION.
       PROGRAM-ID.  Exercise1 as "Exercise1".
       AUTHOR.  Van Nguyen (040919914).
              
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Num1           PIC 999  VALUE ZEROS.
       01 Num2           PIC 999  VALUE ZEROS.
       01 Result         PIC 999999 VALUE ZEROS.
       01 Operator       PIC X  VALUE SPACE.
       
       PROCEDURE DIVISION.
       Calculator.
           PERFORM 3 TIMES
               DISPLAY "Enter First Number (max 3-digits)  : " WITH NO 
               ADVANCING
               ACCEPT Num1
               DISPLAY "Enter Second Number (max 3-digits) : " WITH NO 
               ADVANCING
               ACCEPT Num2
               DISPLAY "Enter operator (+ or *) : " WITH NO ADVANCING
               ACCEPT Operator
               IF Operator = "+" THEN
                   ADD Num1, Num2 GIVING Result
               END-IF
               
               
               IF Operator = "*" THEN
                   MULTIPLY Num1 BY Num2 GIVING Result
               END-IF
               DISPLAY "Result is = ", Result
           END-PERFORM.
           STOP RUN.
       END PROGRAM Exercise1.