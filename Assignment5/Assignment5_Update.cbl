      ***************************************************************
      ************************ VAN NGUYEN ***************************   
      ******************** Student ID:  040919914 *******************
      ***************************************************************
       identification division.
       program-id. Assignment5 as "Assignment5".

       environment division.
       
       data division.
       working-storage section.
       01 cities.
           03 FILLER             PIC X(4) value SPACES.
           03 FILLER             PIC X(8) value "MONTREAL".
           03 FILLER             PIC X(4) value SPACES.
           03 FILLER             PIC X(6) value "OTTAWA".
           03 FILLER             PIC X(4) value SPACES.
           03 FILLER             PIC X(7) value "TORONTO".
           03 FILLER             PIC X(4) value SPACES.
           03 FILLER             PIC X(8) value "KINGSTON".
           03 FILLER             PIC X(4) value SPACES.
           03 FILLER             PIC X(8) value "CORNWALL".
         
       01 city-avg-temps.
           03 FILLER             PIC X(5) value SPACES.
           03 avg-temp-city-1    PIC x(5).
           03 FILLER             PIC X(6) value SPACES.
           03 avg-temp-city-2    PIC x(5).
           03 FILLER             PIC X(6) value SPACES.
           03 avg-temp-city-3    PIC x(5).
           03 FILLER             PIC X(6) value SPACES.
           03 avg-temp-city-4    PIC x(5).
           03 FILLER             PIC X(7) value SPACES.
           03 avg-temp-city-5    PIC x(5).
         
       01 temperature          PIC 99V9 value 0.
       01 counter              PIC 9 value 1.
       01 total-temperature    PIC 9(3)V9 value 0.
       01 avg-temperature      PIC 99.9 value 0.
       01 temporary            PIC X(4).
       01 celcius-avg-temperature    PIC X(5).
       01 blank-line           PIC X(100) value SPACES.
      

       procedure division.
     
      *--------------------------------------------------------------
      *Main procedure
      *--------------------------------------------------------------
       main-program.
           perform all-cities-temperature-input                         
           perform average-temperature-display
           
           stop run.
       
      *------------------------------------------------------------     -
      *Display the average temperatures of all cities
      *-------------------------------------------------------------
       average-temperature-display.
           display
           "Here are the average temperatures for cities in the area:"
           display blank-line
      
           *>display city names    
           display cities
           *>display corresponding avg temperatures for each city
           display city-avg-temps
           
           exit.
      *-------------------------------------------------------------
      *Input temperature for all cities
      *-------------------------------------------------------------    
       all-cities-temperature-input.
           display 
           "ENTER the last 5 daily high temperatures for: Montreal"
           perform one-city-temperature-input
           move celcius-avg-temperature to avg-temp-city-1              
       
           display 
           "ENTER the last 5 daily high temperatures for: Ottawa"
           perform one-city-temperature-input
           move celcius-avg-temperature to avg-temp-city-2 
       
           display 
           "ENTER the last 5 daily high temperatures for: Toronto"
           perform one-city-temperature-input
           move celcius-avg-temperature to avg-temp-city-3
       
           display 
           "ENTER the last 5 daily high temperatures for: Kingston"
           perform one-city-temperature-input
           move celcius-avg-temperature to avg-temp-city-4
       
           display 
           "ENTER the last 5 daily high temperatures for: Cornwall"
           perform one-city-temperature-input
           move celcius-avg-temperature to avg-temp-city-5              
                                                                        
           exit.
       
      *-------------------------------------------------------------
      *Enter last 5 daily temperature for each city  and prepare data 
      *for average calculation
      *-------------------------------------------------------------
       one-city-temperature-input.
           
           display blank-line
           display "Temperatures (one decimal place only)"
           *> perform 5 times of temperature input
           perform until counter > 5
               display "Enter temperature #" counter ": " with no
               advancing
               accept temperature
               
               *>sum up daily temperature to prepare for avg calculation
               add temperature to total-temperature
               
               add 1 to counter
           end-perform
           
           *>calculate avg temp
           divide total-temperature by 5 giving avg-temperature         
           *>convert avg-temperature to alphanumeric then add character 
           *>"C" after it
           move avg-temperature to temporary
           STRING temporary , "C" DELIMITED BY SIZE INTO
           celcius-avg-temperature
           
           *>set counter and total-temperature back to initial values   
           move 1 to counter
           move 0 to total-temperature
           
           DISPLAY SPACE UPON CRT *>clear screen
           
           exit.

       end program Assignment5.
