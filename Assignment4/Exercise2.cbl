       identification division.
       program-id. Exercise2 as "Exercise2".

       environment division.
       configuration section.

       data division.
       working-storage section.
       01 number-of-integers   PIC 99.
       01 value-of-integer     PIC 9(9).
       01 total-value          PIC 9(9).
       01 avg-value            PIC 9(9).99.
       01 counter              PIC 99.
       01 comment-allstars     PIC X(80) value ALL "*".
       01 comment-mix          PIC X(25) value ALL "*".
       01 blank-line           PIC X(132) value SPACES.
        
       
       procedure division.
      *--------------------------------------------------------------
      *Main procedure
      *--------------------------------------------------------------
       main-program.
           perform comment-area.
           perform initilization.
           perform average-calculation.
           
           stop run.
           
      *--------------------------------------------------------------
      *Comment area
      *--------------------------------------------------------------
       comment-area.
           display comment-allstars
           display "*" comment-mix "         VAN NGUYEN         "       
           comment-mix "*"
           display "*" comment-mix "   Student ID:  040919914   "       
           comment-mix "*"
           display comment-allstars
           display blank-line.
      
           exit.
      *--------------------------------------------------------------     
      *This paragraph provides instructions to users on the input of 
      *data and initializes variables that need initial values.
      *--------------------------------------------------------------  
       initilization.
           display "Please enter the number of integers you would like
      -    "to calculate average of." X"0A" 
           "The number should be anywhere between 2 and 15 inclusive."
      *    move 0 to total-value
           move 1 to counter.
           
           exit.
           
      *--------------------------------------------------------------
      *This paragraph calculates average of the entered numbers.
      *--------------------------------------------------------------
       average-calculation.    
           display "Number of integers is: " with no advancing
           accept number-of-integers
           perform accept-of-integer-values until 
           counter > number-of-integers
           divide total-value by number-of-integers giving avg-value
           display "Average value of the entered numbers is: " avg-value
               
           exit.
           
      *--------------------------------------------------------------     
      *This paragraph accepts the positive integer values, and sums up
      *the integers to prepare for the average calculation.
      *-------------------------------------------------------------    
       accept-of-integer-values.
           display "Enter positive integer number " counter ": " with no
           advancing
           accept value-of-integer
           add value-of-integer to total-value
           add 1 to counter
           exit.
           
      *----------------------------------------------------------------
       end program Exercise2.
