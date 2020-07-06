       identification division.
       program-id. Exercise3 as "Exercise3".

       environment division.
       configuration section.

       data division.
       working-storage section.
       01  Multiplication-Square.
	       03  multiplication-table occurs 10.*>verticaldirection
	           05  multiplier-table occurs 10. *>horizontal direction
                   07  product pic ZZ9.
               
       01   multiplicand     pic 99. *>subscript of multiplication-table
       01   multiplier       pic 99. *>subscript of multiplier-table
       01   counter          pic 99.
       01   table-number     pic Z9.
       

       procedure division.
       main-program.
           perform process-multiplication
           perform display-result
           
           stop run.
      **********************************************************
      *******PERFORM MULTIPLICATION FOR TABLES FROM 1 to 10*****
      **********************************************************
       process-multiplication.
           perform varying multiplicand from 1 by 1 until multiplicand  
           > 10
               perform varying multiplier from 1 by 1 until 
               multiplier > 10
                   multiply multiplicand by multiplier giving 
                   product(multiplicand, multiplier)
                   
               end-perform
           end-perform   
           exit.
           
      **********************************************************
      ****************DISPLAY TIME TABLES***********************
      **********************************************************
       display-result.    
           display "Multiplication table 10x10:"
           *>display first row of multiplication square
           display "  "multiplication-table(1)
           *>display the result of multiplication for 10 tables 
           *>including table# at beginning of the row
           perform varying counter from 1 by 1 until 
           counter > 10
               move counter to table-number            
               *>display the table# where multiplication happens
               display table-number with no advancing
               *>display results of multiplication for table# above 
               display multiplication-table(counter)                    
               
           end-perform
           exit.

       end program Exercise3.
