       program-id. Exercise1 as "Exercise1".

       environment division.
       configuration section.

       data division.
       working-storage section.
       01 WS-NUM1 PIC 9(4) VALUE 10.
       01 WS-NUM2 PIC 9(4) VALUE 10.
       01 WS-NUM3 PIC 9(4) VALUE 100.
       01 WS-NUM4 PIC 9(4) VALUE 100.
       01 WS-NUMA PIC 9(4) VALUE 10.
       01 WS-NUMB PIC 9(4) VALUE 10.
       01 WS-NUMC PIC 9(4) VALUE 10.
       01 WS-NUMD PIC 9(4) VALUE 100.
       01 WS-NUME PIC 9(4) VALUE 10.

       procedure division.
      *SUBTRACT WS-NUM1 WS-NUM2 FROM WS-NUM3 WS-NUM4.
       ADD WS-NUM1 WS-NUM2 TO WS-NUM3 GIVING WS-NUM4.
       
       display "WS-NUM1: " WS-NUM1
       display "WS-NUM2: " WS-NUM2
       display "WS-NUM3: " WS-NUM3
       display "WS-NUM4: " WS-NUM4
       display "------------------------"
       
      *SUBTRACT WS-NUMA WS-NUMB WS-NUMC FROM WS-NUMD GIVING WS-NUME.
      *MULTIPLY WS-NUMA BY WS-NUMB WS-NUMC.
      *MULTIPLY WS-NUMA BY WS-NUMB GIVING WS-NUMC.
      *DIVIDE WS-NUMD BY 7 GIVING WS-NUMA REMAINDER WS-NUMB.
       COMPUTE WS-NUMC = (WS-NUM1 * WS-NUM2) - (WS-NUMA / WS-NUMB) 
       + WS-NUM3                                                        
       display "WS-NUMA: " WS-NUMA
       display "WS-NUMB: " WS-NUMB
       display "WS-NUMC: " WS-NUMC
       display "WS-NUMD: " WS-NUMD
       display "WS-NUME: " WS-NUME
           goback.

       end program Exercise1.
