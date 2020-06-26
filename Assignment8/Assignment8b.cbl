       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHANGES-FILE  
               ASSIGN "\COBOLClass_Eclipse\DataFiles\changes29.DAT".
           SELECT EMPLOYEE-FILE 
               ASSIGN "\COBOLClass_Eclipse\DataFiles\EMPLOYEE29.DAT"
                       ORGANIZATION IS INDEXED
                       ACCESS IS RANDOM
                       RECORD KEY EMPLOYEE-NO.
       
       DATA DIVISION.
       FILE SECTION.
       FD  CHANGES-FILE.
       01  CHANGES-REC.
           03  CHANGE-TYPE      PIC X.
           88  ADD-A-RECORD     VALUE "A".
           88  CHANGE-A-RECORD  VALUE "C".
           88  DELETE-A-RECORD  VALUE "D".
           03  EMP-NO           PIC X(8).
           03  EMP-INITS        PIC X(4).
           03  EMP-SURNAME      PIC X(16).
           03  EMP-SALARY       PIC 9(6)V99.
           03  EMP-ADDRESS      PIC X(40).
           03  EMP-DEPT         PIC X(10).
       FD  EMPLOYEE-FILE.
       01  EMPLOYEE-REC.
           03  EMPLOYEE-NO      PIC X(8).
           03  EMPLOYEE-INITS   PIC X(4).
           03  EMPLOYEE-SURNAME PIC X(16).
           03  EMPLOYEE-SALARY  PIC 9(6)V99.
           03  EMPLOYEE-ADDRESS PIC X(40).
           03  EMPLOYEE-DEPT    PIC X(10).
       WORKING-STORAGE SECTION.
       01  WS-EOF                  PIC 9 VALUE 0.
           88  NO-MORE-CHANGES           VALUE 1.
       01  WS-NO-RECORD-FOUND-FLAG PIC 9.
           88  RECORD-FOUND              VALUE 0.
       PROCEDURE DIVISION.
       PROG.
       INIT-PARA.
           DISPLAY "INDEXED FILE PROGRAM STARTING"
           OPEN INPUT CHANGES-FILE
           OPEN I-O   EMPLOYEE-FILE
           READ CHANGES-FILE
             AT END MOVE 1 TO WS-EOF 
           END-READ.
       BOD-PARA.
           PERFORM PROCESS-CHANGES-REC UNTIL NO-MORE-CHANGES.
       END-PARA.
           DISPLAY "PROGRAM CONCLUDES"
           CLOSE CHANGES-FILE
                 EMPLOYEE-FILE
           STOP RUN.
       PROCESS-CHANGES-REC.
           PERFORM REC-STA
           PERFORM REC-BOD
           PERFORM REC-FIN.
       REC-STA.
           MOVE 0 TO WS-NO-RECORD-FOUND-FLAG
           MOVE EMP-NO TO EMPLOYEE-NO
           READ EMPLOYEE-FILE KEY IS EMPLOYEE-NO
             INVALID KEY
               MOVE 1 TO WS-NO-RECORd-FOUND-FLAG
           END-READ.
       REC-BOD.
           IF RECORD-FOUND
               PERFORM REC-FOUND
           ELSE
               PERFORM REC-NOT-FOUND
           END-IF.
       REC-FIN.
           READ CHANGES-FILE
             AT END MOVE 1 TO WS-EOF
           END-READ.
       REC-FOUND.
           EVALUATE TRUE
             WHEN ADD-A-RECORD
               PERFORM INVALID-ADD
             WHEN CHANGE-A-RECORD
               PERFORM VALID-CHANGE
             WHEN DELETE-A-RECORD
               PERFORM VALID-DELETE
             WHEN OTHER
               DISPLAY "INVALID TYPE " CHANGE-TYPE
               STOP RUN
           END-EVALUATE.
       REC-NOT-FOUND.
           EVALUATE TRUE
             WHEN ADD-A-RECORD
               PERFORM VALID-ADD
             WHEN CHANGE-A-RECORD
               PERFORM INVALID-CHANGE
             WHEN DELETE-A-RECORD
               PERFORM INVALID-DELETE
             WHEN OTHER
               DISPLAY "INVALID TYPE " CHANGE-TYPE
               STOP RUN
           END-EVALUATE.
       INVALID-ADD.
           DISPLAY "CANNOT ADD EXISTING RECORD " EMP-NO
           DISPLAY CHANGES-REC.
       VALID-CHANGE.
           IF EMP-SALARY UNEQUAL ZERO
               MOVE EMP-SALARY TO EMPLOYEE-SALARY
           IF EMP-ADDRESS UNEQUAL SPACES
               MOVE EMP-ADDRESS TO EMPLOYEE-ADDRESS
           IF EMP-DEPT UNEQUAL SPACES
               MOVE EMP-DEPT TO EMPLOYEE-DEPT
           REWRITE EMPLOYEE-REC.
       VALID-DELETE.
           DELETE EMPLOYEE-FILE.
       VALID-ADD.
           MOVE EMP-INITS TO EMPLOYEE-INITS
           MOVE EMP-SURNAME TO EMPLOYEE-SURNAME
           MOVE EMP-SALARY TO EMPLOYEE-SALARY
           MOVE EMP-ADDRESS TO EMPLOYEE-ADDRESS
           MOVE EMP-DEPT TO EMPLOYEE-DEPT
           WRITE EMPLOYEE-REC.
       INVALID-CHANGE.
           DISPLAY "CANNOT AMEND NON-EXISTENT RECORD "
               EMP-NO
           DISPLAY CHANGES-REC.
       INVALID-DELETE.
           DISPLAY "CANNOT DELETE NON-EXISTENT RECORD "
               EMPLOYEE-NO
           DISPLAY CHANGES-REC.
