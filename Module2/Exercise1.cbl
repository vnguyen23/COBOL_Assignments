       identification division.
       program-id.     Exercise1 as "Exercise1".                        .
       author.         Van Nguyen.
       date-written.   2020-05-13.
       date-compiled.   2020-05-13.
       
       environment division.
       configuration section.
       source-computer.    Desktop.
       object-computer.    Desktop.
       input-output section.
       file-control.
           Select infile assign 
           "C:\Van\Training\COBOL\AlgonquinCollege\DataFiles\DataIn.dat"
      -    .                                                            
           Select outfile assign 
           "C:\Van\Training\COBOL\AlgonquinCollege\DataFiles\DataOut.dat
      -    "".
       data division.
       file section.
       fd infile.
       01 infile-record    pic x(80).
       fd outfile.
       01 outfile-record   pic x(100).
       working-storage section.
       01 work-field       pic x(20).
       01 counter-field    pic 99.
       linkage section.
       01 ls-field         pic x(10).
       procedure division.
       001-Main section.
       001-begin.
           open input infile
           open output outfile
           perform 010-read-write
           close infile outfile
           stop run.
       010-read-write section.
       010-begin.
           read infile
           move infile-record to outfile-record
           write outfile-record
           display outfile-record.
       end program Exercise1.
