       program-id. Exercise2 as "Exercise2".

       environment division.
       configuration section.

       data division.
       working-storage section.
       01  FIELD-1 PIC X(10) VALUE "AAABBXBBAA".
       01  FIELD-2 PIC X(10) VALUE "LastName".
       01  FIELD-3 PIC X(10) VALUE "FirstName".
       01  FIELD-4 PIC X(20) VALUE " ".

       procedure division.
      *INSPECT FIELD-1 REPLACING FIRST "B" BY "Z"
      *INSPECT FIELD-1 REPLACING ALL "B" BY "C"
       display "Test1"
       STRING FIELD-2 DELIMITED BY SIZE FIELD-3 DELIMITED BY SPACE      
       INTO FIELD-4
      *ON OVERFLOW DISPLAY "OVERFLOW!"
       display "FIELD-2: " FIELD-2
       display "FIELD-3: " FIELD-3
       display "FIELD-4: " FIELD-4

       
       
           goback.

       end program Exercise2.
