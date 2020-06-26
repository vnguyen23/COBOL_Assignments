       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE    
                   ASSIGN 
                   "C:\DataFiles\InfileSales.dat"
                   ORGANIZATION IS SEQUENTIAL.
           SELECT PRINTFILE 
                   ASSIGN 
                   "C:\DataFiles\OutReportSummary.txt"
                   ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  INFILE.
       01  INREC.
           03  IN-REGION   PIC X.
           03  IN-NAME     PIC X(20).
           03  IN-SALES    PIC 9(6)V99.
       FD  PRINTFILE.
       01  PRINTREC        PIC X(80).
       WORKING-STORAGE SECTION.
       01  WS-EDITED-DATE  PIC 9999/99/99.
       01  WS-EOF          PIC X(3) VALUE "NO".
           88  END-OF-FILE          VALUE "YES".
       01  WS-REGION-TABLE.
           03              PIC X(18) VALUE "EEASTERN NNORTHERN".
           03              PIC X(18) VALUE "SSOUTHERNWWESTERN ".
       01  WS-REGION-TABLE-REDEF REDEFINES WS-REGION-TABLE.
           03  WS-REG      OCCURS 4 INDEXED BY REGION-IX
                                    ASCENDING KEY WS-LETTER.
               05  WS-LETTER PIC X.
               05  WS-REGION PIC X(8).
       01  WS-REGN         VALUE LOW-VALUES.
           03  WS-RG       PIC X.
           03              PIC X(7).
       01  WS-DATE         PIC 9(8).
       01  WS-LINE-CNT     PIC 99 VALUE 99.
       01  WS-PAGE-CNT     PIC 99 VALUE 0.
       01  WS-RECORD-CNT   PIC 9(4) VALUE 0.
       01  WS-TOTALS         COMP VALUE LOW-VALUES.
           03  WS-REGION-TOT     PIC 9(8)V99.
           03  WS-GRAND-TOT      PIC 9(10)V99.
       01  WS-HEADING-LINE.
           03                    PIC X(20) VALUE "       REPORT DATED ".
           03  WS-PRINT-DATE     PIC 9999/99/99.
           03                    PIC X(60) VALUE SPACES.
           03  WS-PRINT-PAGE-CNT PIC Z9.
       01  WS-REGION-START-LINE.
           03                    PIC X(8) VALUE "REGION:".
           03  WS-PRINT-REGION   PIC X(8).
       01  WS-DETAIL.
      *    d) Add spacing on the detail line to move the name file 
      *       by 20 characters
           03  WS-PRINT-NAME     PIC X(40).
           03  WS-PRINT-SALES    PIC ZZZ,ZZ9.99.
       01  WS-SPACES             PIC X(132) VALUE SPACES.
       01  WS-REGION-END-LINE.
           03                    PIC X(20) VALUE SPACES.
           03                    PIC X(27) VALUE
                           "TOTALS FOR THIS REGION ARE ".
           03  WS-PRINT-REGION-TOTAL
                                 PIC ZZ,ZZZ,ZZ9.99.
       01  WS-FINAL-LINE.
           03                    PIC X(20) VALUE SPACES.
           03                    PIC X(20) VALUE "GRAND TOTAL ".
           03  WS-PRINT-GRAND-TOTAL PIC Z,ZZZ,ZZZ,ZZ9.99.
       PROCEDURE DIVISION.
       PROG.
           PERFORM INIT-PARA 
           PERFORM BOD-PARA 
           PERFORM END-PARA
           .
       INIT-PARA.
           DISPLAY "PRINT PROGRAM STARTING" 
           OPEN INPUT  INFILE
                OUTPUT PRINTFILE 
           ACCEPT WS-DATE FROM DATE YYYYMMDD
           MOVE WS-DATE to WS-EDITED-DATE 
           DISPLAY "Date is " WS-EDITED-DATE 
      *    a) Ensure the date appears on the report by moving the 
      *       accepted current date to the heading line
           READ INFILE
               AT END MOVE "YES" TO WS-EOF
           END-READ
           .
       BOD-PARA.
           PERFORM PROCESS-REC UNTIL END-OF-FILE
           .
       END-PARA.
           IF WS-RECORD-CNT = 0
               DISPLAY "NO RECORDS ON INPUT FILE!" 
           END-IF    
           PERFORM END-OF-REGION 
           MOVE WS-GRAND-TOT TO WS-PRINT-GRAND-TOTAL 
           WRITE PRINTREC FROM WS-FINAL-LINE AFTER 4 
           CLOSE INFILE
                 PRINTFILE 

           STOP RUN
           .
       PROCESS-REC.
           IF WS-LINE-CNT > 45
                PERFORM WRITE-HEADINGS 
           END-IF     
           IF IN-REGION UNEQUAL WS-RG
             AND WS-REGN UNEQUAL LOW-VALUES
               PERFORM END-OF-REGION
           END-IF
           IF IN-REGION UNEQUAL WS-RG
               PERFORM START-OF-REGION 
           END-IF
           ADD 1 TO WS-RECORD-CNT
           PERFORM WRITE-DETAIL
           READ INFILE
               AT END MOVE "YES" TO WS-EOF
           END-READ
           .
       WRITE-HEADINGS.
           WRITE PRINTREC FROM WS-SPACES AFTER PAGE
           ADD 1 TO WS-PAGE-CNT
           MOVE WS-PAGE-CNT TO WS-PRINT-PAGE-CNT
      *vAN'S CODE    
           MOVE WS-EDITED-DATE TO WS-PRINT-DATE
           WRITE PRINTREC FROM WS-HEADING-LINE AFTER 2
           MOVE 2 TO WS-LINE-CNT
           IF WS-PAGE-CNT > 1
               PERFORM START-OF-REGION
           END-IF
           .
       END-OF-REGION.
           MOVE WS-REGION-TOT TO WS-PRINT-REGION-TOTAL
           MOVE 0 TO WS-REGION-TOT
      *    c) Adjust the number of blank lines after the region. 
           WRITE PRINTREC FROM WS-REGION-END-LINE AFTER 1
           ADD 1 TO WS-LINE-CNT
           .
       START-OF-REGION.
           SEARCH ALL WS-REG
               AT END DISPLAY "UNKNOWN REGION " IN-REGION
                      CLOSE INFILE
                            PRINTFILE
                      STOP RUN
               WHEN IN-REGION = WS-LETTER(REGION-IX)
                 MOVE WS-REGION(REGION-IX) TO WS-print-region
           END-SEARCH      
           MOVE in-region to ws-regn
      *    b) Adjust the number of blank lines before the region.
           WRITE PRINTREC FROM WS-REGION-START-LINE AFTER 1
           ADD 1 TO WS-LINE-CNT
           .
       WRITE-DETAIL.
           MOVE IN-NAME TO WS-PRINT-NAME
           MOVE IN-SALES TO WS-PRINT-SALES
      *    e) Add IN-Sales to the region and grand totals
           WRITE PRINTREC FROM WS-DETAIL AFTER 1
           ADD 1 TO WS-LINE-CNT
           .
