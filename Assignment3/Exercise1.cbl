       identification division.
       program-id. Exercise1 as "Exercise1".

       environment division.
       input-output section.
       file-control.
           Select StudentFile assign "C:\DataFiles\Student_File.dat".

       data division.
       file section.
       fd StudentFile.
       01 StudentId.
           03 StudentName.
               05 Surname     PIC X(20).
               05 Initials    PIC X(4).
               05 GivenName   PIC X(20).
           03 DateOfBirth.
               05 YearOfBirth	      PIC 9(4).
               05 MonthOfBirth        PIC 99.
               05 DayOfBirth          PIC 99.
           03 ProgramCode   PIC X(5).
           03 Gender        PIC A.

       procedure division.


           goback.

       end program Exercise1.
