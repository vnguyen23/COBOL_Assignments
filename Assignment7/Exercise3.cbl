       program-id. Exercise3 as "Exercise3".

       environment division.
       configuration section.

       data division.
       working-storage section.
       01  Time-Table.
	       03  vertical-di occurs 10.
		       05  individual-table  pic 99.
		       05  multiple-by pic ZZ9 occurs 10. 
               
       01   multiplier1  pic 99.
       01   multiplier2  pic 99.
       01   multiplier3  pic Z9.
       

       procedure division.
           perform varying multiplier1 from 1 by 1 until multiplier1 > 
           10
               perform varying multiplier2 from 1 by 1 until 
               multiplier2 > 10
                   multiply multiplier2 by multiplier1 giving 
                   multiple-by(multiplier1, multiplier2)
               end-perform
           end-perform    
           
      *display "Time-Table:" Time-Table
           display "  "vertical-di(1)
           perform varying multiplier1 from 1 by 1 until multiplier1 > 
           10
               move multiplier1 to multiplier3
               display multiplier3 vertical-di(multiplier1)
               
           end-perform
           stop run.

       end program Exercise3.
