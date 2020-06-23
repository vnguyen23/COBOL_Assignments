       program-id. Exercise1 as "Exercise1".

       environment division.
       input-output section.
       file-control.
           select WeeklySalary assign 
           "C:\DataFiles\Assignment#6_WeeklySalaryDataFile.dat"  
           organization is line sequential.
           
           select SalaryExpense assign
           "C:\DataFiles\SalaryExpense.rpt"
           organization is line sequential.

       data division.
       file section.
       fd WeeklySalary.
       01  EmployeeSalary.
         03    EmployeeID              PIC X(4).
         03    JobCode                 PIC X.
           88  Manager                 VALUE "M".
           88  Saleperson              VALUE "S".
           88  Administration          VALUE "A".
           88  Custodial-Staff         VALUE "C".
         03    LastName                PIC X(14).
         03    FirstName               PIC X(14).
         03    StartDate               PIC X(8).
         03    StoreNumber             PIC X(4).
         03    WklySalary              PIC 9(4)V99.
         03    SaleAmount              PIC 9(6)V99.
         
       fd SalaryExpense.
       01  SalaryExp.
         03    SE-JobCode              PIC X(15).
         03    SE-WklySalary           PIC 9(4).99.
         03    FILLER                  PIC X(25) VALUE SPACES.
         
       working-storage section.
       01  WS-END-OF-FILE              PIC 9 VALUE 0.
           88  end-of-file             VALUE 1.
           
       01 FileHeadingSalaryExpense.
         03 FILLER           PIC X(36) VALUE 
         "Salary expense for the week ending: ".
         03 CurrDate             PIC 9(10).
         
       01 CurrentDate.
         03 CurrentYear      PIC 9(4).
         03 CurrentMonth     PIC 99.
         03 CurrentDay       PIC 99.
       
       01  WS-StoreNumber                 PIC X(8).
       01  WS-Position                     PIC X(20).
       01  WS-SalaryExp.
         03    WS-Store1_M_Total          PIC 9(4)V99.
         03    WS-Store1_S_Total          PIC 9(4)V99.
         03    WS-Store1_A_Total          PIC 9(4)V99.
         03    WS-Store1_C_Total          PIC 9(4)V99.
         03    WS-Store2_M_Total          PIC 9(4)V99.
         03    WS-Store2_S_Total          PIC 9(4)V99.
         03    WS-Store2_A_Total          PIC 9(4)V99.
         03    WS-Store2_C_Total          PIC 9(4)V99.
         03    WS-Store3_M_Total          PIC 9(4)V99.
         03    WS-Store3_S_Total          PIC 9(4)V99.
         03    WS-Store3_A_Total          PIC 9(4)V99.
         03    WS-Store3_C_Total          PIC 9(4)V99.
        
       procedure division.
       main-program.
           perform initialization
           perform open-file
           perform write-header
           perform read-file until end-of-file 
           perform close-file
           stop run.
           
       initialization.
           *> Get current date from system
           accept CurrentDate from DATE YYYYMMDD
           String CurrentYear, "-", CurrentMonth, "-", CurrentDay
           DELIMITED BY SIZE INTO CurrDate of 
           FileHeadingSalaryExpense
           exit.
       
       open-file.
           open input WeeklySalary 
                output SalaryExpense
           exit.
           
       write-header.
           write SalaryExp from FileHeadingSalaryExpense
           exit.
           
       read-file.
           read WeeklySalary
               at end    
                   set end-of-file to true
                   perform write-to-salary-expense-file
           end-read
           perform calculate-salary-expense
           exit.
           
       calculate-salary-expense.
           if StoreNumber = 0001
               if Manager
               then add WklySalary to WS-Store1_M_Total
               else if Saleperson 
               then add WklySalary to WS-Store1_S_Total
               else if Administration
               then add WklySalary to WS-Store1_A_Total
               else if Custodial-Staff
               then add WklySalary to WS-Store1_C_Total
               end-if
           else if StoreNumber = 0002
               if Manager
               then add WklySalary to WS-Store2_M_Total
               else if Saleperson 
               then add WklySalary to WS-Store2_S_Total
               else if Administration
               then add WklySalary to WS-Store2_A_Total
               else if Custodial-Staff
               then add WklySalary to WS-Store2_C_Total
               end-if                                                    
            else if StoreNumber = 0003
               if Manager
               then add WklySalary to WS-Store3_M_Total
               else if Saleperson 
               then add WklySalary to WS-Store3_S_Total
               else if Administration
               then add WklySalary to WS-Store3_A_Total
               else if Custodial-Staff
               then add WklySalary to WS-Store3_C_Total
               end-if
           end-if
      *    move 0 to Temporary
           
           exit.
       
       write-to-salary-expense-file.
      *write salary of Store #1
           *>write Store #1
           move "STORE #1" to WS-StoreNumber
           write SalaryExp from WS-StoreNumber
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
           
       close-file.
           close WeeklySalary 
                 SalaryExpense
           exit.

       end program Exercise1.
