      *Initialize values
      *Variables level other than 1 are in B area
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Exercise1 as "Exercise1".

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           select WeeklySalary assign 
           "C:\DataFiles\Assignment#6_WeeklySalaryDataFile.dat"  
           organization is line sequential.
           
           select GiftList assign 
           "C:\DataFiles\SalespersonRecognition.rpt"
           organization is line sequential.
           
           select SalaryExpense assign
           "C:\DataFiles\SalaryExpense.rpt"
           organization is line sequential.
           
       DATA DIVISION.
       FILE SECTION.
       fd WeeklySalary.
      *********EmployeeSalary record, length 59 characters*********
       01  EmployeeSalary.
           03    EmployeeID             PIC X(4).
           03    JobCode                PIC X.
               88  Manager              VALUE "M".
               88  Salesperson          VALUE "S".
               88  Administration       VALUE "A".
               88  Custodial-Staff      VALUE "C".
           03    LastName               PIC X(14).
           03    FirstName              PIC X(14).
           03    StartDate              PIC X(8).
           03    StoreNumber            PIC X(4).
           03    WklySalary             PIC 9(4)V99.
           03    SaleAmount             PIC 9(6)V99.
      ********End of EmployeeSalary record**************************
      
       fd  GiftList.
      ********HighPerforming record, length 52 characters***********
       01  HighPerforming. 
           03    HPLastName             PIC X(19).
           03    HPFirstName            PIC X(19).
           03    HPStoreNumber          PIC X(14).
      ********End of HighPerforming record***************************  
      
       fd SalaryExpense.
      ********SalaryExp record, length 47 characters*****************   
       01  SalaryExp.
           03    SE-JobCode             PIC X(15).
           03    SE-WklySalary          PIC Z9(3).99.
           03    FILLER                 PIC X(25) VALUE SPACES.
      *********End of SalaryExprecord********************************
       
       WORKING-STORAGE SECTION.
      ******Variables for header of file “SalespersonRecognition.rpt”**
       01 FileHeadingSalespersonRecognition. 
           03 FILLER                    PIC X(42) VALUE 
         "Employee recognition for the week ending: ".
           03 CurrDate                  PIC X(10).
       
       01  DataHeading-01.
           03  FILLER                    PIC X(52) VALUE ALL "-". 
         
       01  DataHeading-02.
           03    FILLER                  PIC X(10) VALUE "Last Name".
           03    FILLER                  PIC X(9) VALUE SPACES.
           03    FILLER                  PIC X(10) VALUE "First Name".
           03    FILLER                  PIC X(9) VALUE SPACES.
           03    FILLER                  PIC X(12) VALUE "Store #".
           03    FILLER                  PIC X(7) VALUE SPACES.
      *****End variables of header of file “SalespersonRecognition.rpt” 
      
      *****Variables for header of file “SalaryExpense.rpt”********** 
       01  FileHeadingSalaryExpense.
           03 FILLER                     PIC X(36) VALUE 
         "Salary expense for the week ending: ".
           03 CurrDate                   PIC 9(10).
      *****End variables for header of file “SalaryExpense.rpt”******
      
      ******Total salary variables************************************
       01  WS-Store1_M_Total             PIC 9(4)V99 value ZEROS.
       01  WS-Store1_S_Total             PIC 9(4)V99 value ZEROS.
       01  WS-Store1_A_Total             PIC 9(4)V99 value ZEROS.
       01  WS-Store1_C_Total             PIC 9(4)V99 value ZEROS.
       01  WS-Store2_M_Total             PIC 9(4)V99 value ZEROS.
       01  WS-Store2_S_Total             PIC 9(4)V99 value ZEROS.
       01  WS-Store2_A_Total             PIC 9(4)V99 value ZEROS.
       01  WS-Store2_C_Total             PIC 9(4)V99 value ZEROS.
       01  WS-Store3_M_Total             PIC 9(4)V99 value ZEROS.
       01  WS-Store3_S_Total             PIC 9(4)V99 value ZEROS.
       01  WS-Store3_A_Total             PIC 9(4)V99 value ZEROS.
       01  WS-Store3_C_Total             PIC 9(4)V99 value ZEROS.
      *****End of Total salary variables*****************************
      
       01  CurrentDate.
           03 CurrentYear                PIC 9(4).
           03 CurrentMonth               PIC 99.
           03 CurrentDay                 PIC 99.
         
       01  WS-END-OF-FILE                PIC 9 VALUE 0.
           88  end-of-file               VALUE 1.
       01  WS-StoreNumber                PIC X(8).
           
       PROCEDURE DIVISION.
       main-program.
           perform open-file
           perform write-header
           perform read-file until end-of-file 
           perform close-file
           stop run.
           
      *-------------------------------------------------------------
      *open-file paragraph opens all input and output files this 
      *exercise works with 
      *-------------------------------------------------------------
       open-file.
           open input WeeklySalary 
                output GiftList
                output SalaryExpense
           exit.
           
      *-------------------------------------------------------------
      *write-header psragraph writes headers for the output files 
      *“SalespersonRecognition.rpt” and “SalaryExpense.rpt”
      *-------------------------------------------------------------
       write-header.
           perform initiate-header
           *>write header of file “SalespersonRecognition.rpt”
           write HighPerforming from FileHeadingSalespersonRecognition  
           write HighPerforming from DataHeading-01 after advancing 1 
           line
           write HighPerforming from DataHeading-02
           write HighPerforming from DataHeading-01
       
           *>write header of file “SalaryExpense.rpt”
           write SalaryExp from FileHeadingSalaryExpense
           
           exit.
           
      *-------------------------------------------------------------
      *read-file paragraph read the input file sequentially; 
      *after reading each record:
      *- calls select-and-write-high-performing-sales-people paragraph
      *- calls calculate-salary-expense paragraph
      *at the end of file, call write-salary-expense-to-file paragraph
      *-------------------------------------------------------------
       read-file.
           read WeeklySalary
               at end    
                   set end-of-file to true
                   perform write-salary-expense-to-file
           end-read
           perform select-and-write-high-performing-sales-people        
           perform calculate-salary-expense
      
           exit.
       
      *-------------------------------------------------------------
      *close-file paragraph closes all input and output files openned in
      *Procedure section
      *-------------------------------------------------------------
       close-file.
           close WeeklySalary 
                 GiftList
                 SalaryExpense
           exit.
           
      *-------------------------------------------------------------    
      *initiate-header paragraph:
      *- gets current date from system
      *- generate date string with "-" separation
      *-------------------------------------------------------------
       initiate-header.
           accept CurrentDate from DATE YYYYMMDD
           
           String CurrentYear, "-", CurrentMonth, "-", CurrentDay
           DELIMITED BY SIZE INTO CurrDate of 
           FileHeadingSalespersonRecognition
           
           String CurrentYear, "-", CurrentMonth, "-", CurrentDay
           DELIMITED BY SIZE INTO CurrDate of 
           FileHeadingSalaryExpense
           exit.
           
      *-------------------------------------------------------------
      *select-and-write-high-performing-sales-people paragraph:         
      *- finds the list of sales people who had over $100,000 sale 
      *amount
      *- writes the list to output file “SalespersonRecognition.rpt”
      *-------------------------------------------------------------    
       select-and-write-high-performing-sales-people.
           if Salesperson and SaleAmount > 100000.00
           then move LastName to HPLastName
                move FirstName to HPFirstName
                move StoreNumber to HPStoreNumber
                write HighPerforming 
           end-if.
           
           exit.
      *-------------------------------------------------------------
      *calculate-salary-expense paragraph caculates total salary expense
      *for every job code of every store                                
      *-------------------------------------------------------------
       calculate-salary-expense.
           if StoreNumber = 0001
               if Manager
                   then add WklySalary to WS-Store1_M_Total
               else 
                   if Salesperson
                       then add WklySalary to WS-Store1_S_Total
                   else 
                       if Administration
                           then add WklySalary to WS-Store1_A_Total
                       else 
                           if Custodial-Staff
                               then add WklySalary to WS-Store1_C_Total
                           end-if
                       end-if
                   end-if
               end-if
           else 
               if StoreNumber = 0002
                   if Manager
                       then add WklySalary to WS-Store2_M_Total
                   else 
                       if Salesperson
                           then add WklySalary to WS-Store2_S_Total
                       else 
                           if Administration
                               then add WklySalary to WS-Store2_A_Total
                           else 
                               if Custodial-Staff
                                   then add WklySalary to 
                                   WS-Store2_C_Total
                               end-if
                           end-if
                       end-if
                   end-if
               else 
                   if StoreNumber = 0003
                       if Manager
                           then add WklySalary to WS-Store3_M_Total
                       else 
                           if Salesperson
                               then add WklySalary to WS-Store3_S_Total
                           else 
                               if Administration
                                   then add WklySalary to 
                                   WS-Store3_A_Total
                               else 
                                   if Custodial-Staff
                                       then add WklySalary to 
                                       WS-Store3_C_Total
                                   end-if
                               end-if
                           end-if
                       end-if
                   end-if
               end-if
           end-if
           
           exit.
       
      *-------------------------------------------------------------    
      *write-salary-expense-to-file paragraph writes total salary for 
      *every job code of every store to the output file "SalaryExpense.
      *rpt"
      *-------------------------------------------------------------
       write-salary-expense-to-file.
      *write salary of Store #1
           *>write Store #1
           move "STORE #1" to WS-StoreNumber
           write SalaryExp from WS-StoreNumber  after advancing 1 line
           *>write Management total salary
           move "Management:" to SE-JobCode
           move WS-Store1_M_Total to SE-WklySalary      
           write SalaryExp
           *>write Sales total salary
           move "Sales     :" to SE-JobCode
           move WS-Store1_S_Total to SE-WklySalary      
           write SalaryExp
           *>write Administration total salary
           move "Admin     :" to SE-JobCode
           move WS-Store1_A_Total to SE-WklySalary      
           write SalaryExp
           *>write Custodial-Staff total salary
           move "Custodial :" to SE-JobCode
           move WS-Store1_C_Total to SE-WklySalary      
           write SalaryExp 
      *write salary of Store #2
           *>write Store #2
           move "STORE #2" to WS-StoreNumber 
           write SalaryExp from WS-StoreNumber after advancing 1 line
           *>write Management total salary
           move "Management:" to SE-JobCode
           move WS-Store2_M_Total to SE-WklySalary      
           write SalaryExp 
           *>write Sales total salary
           move "Sales     :" to SE-JobCode
           move WS-Store2_S_Total to SE-WklySalary      
           write SalaryExp
           *>write Administration total salary
           move "Admin     :" to SE-JobCode
           move WS-Store2_A_Total to SE-WklySalary      
           write SalaryExp
           *>write Custodial-Staff total salary
           move "Custodial :" to SE-JobCode
           move WS-Store2_C_Total to SE-WklySalary      
           write SalaryExp before advancing 1 line     
      *write salary of Store #3
           *>write Store #3
           move "STORE #3" to WS-StoreNumber
           write SalaryExp from WS-StoreNumber after advancing 1 line
           *>write Management total salary
           move "Management:" to SE-JobCode
           move WS-Store3_M_Total to SE-WklySalary      
           write SalaryExp
           *>write Sales total salary
           move "Sales     :" to SE-JobCode
           move WS-Store3_S_Total to SE-WklySalary      
           write SalaryExp
           *>write Administration total salary
           move "Admin     :" to SE-JobCode
           move WS-Store3_A_Total to SE-WklySalary      
           write SalaryExp
           *>write Custodial-Staff total salary
           move "Custodial :" to SE-JobCode
           move WS-Store3_C_Total to SE-WklySalary      
           write SalaryExp before advancing 1 line
           
           exit.
      
       END PROGRAM Exercise1.
