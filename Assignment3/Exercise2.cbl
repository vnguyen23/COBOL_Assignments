       identification division.
       program-id. Exercise2 as "Exercise2".

       environment division.
       input-output section.
       file-control.
           Select Exercise2Data assign "C:\DataFiles\Data_File.dat".

       data division.
       file section.
       fd Exercise2Data.
       01 CustomerProfile.
           03  Credit-Card-Number          PIC 9(16).
           03  Social-Insurance-Number     PIC X(15).             
           03  Postal-Code                 PIC X(6).
           03  Credit-Card-Balance         PIC S9(5)V99. 
           03  Mortgage_Rate               PIC V9(3).
           03  Placeholder                 PIC X(25) VALUE SPACES.
           03  Province                    PIC X(8) VALUE "Province".
           03  Total-Income                PIC 9(7)V99 COMP-3.
           03  Grocery-Item-Price          PIC 9(3)V99.
           03  Provincial-Tax-Rate         PIC V9(3).
       
      
       procedure division.
           move 5 to Mortgage_Rate
           display Mortgage_Rate
           
      *    open input Exercise2Data
      *    read Exercise2Data
      *    display CustomerProfile
           
      *    close Exercise2Data
           

           stop run.

       end program Exercise2.
