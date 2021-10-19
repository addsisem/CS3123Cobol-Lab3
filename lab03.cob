       IDENTIFICATION DIVISION.
       PROGRAM-ID. LAB3.
       AUTHOR. Addyson Sisemore
      * LAB EXERCISE 3.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'DA-S-INPUT'.
           SELECT PRNT-FILE ASSIGN TO 'UR-S-PRNT'.

       DATA DIVISION.

       FILE SECTION.
       FD INPUT-FILE
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.
       01 INPUT-REC     PIC X(80).
       FD PRNT-FILE
           LABEL RECORDS ARE OMITTED.
       01 PRNT-REC      PIC X(125).
       WORKING-STORAGE SECTION.
       01 TOTAL         PIC 9(5)V99.
      **************************************************************
      * LAYOUT FOR THE INPUT FILE *
      **************************************************************
       01 INPUT-DATA.
         03 I-NAME       PIC X(20).
         03 I-DEGREE     PIC X(4).
         03 I-YEAR       PIC X(4).
         03 I-LOAN       PIC 9(5)V99.
         03 I-PAID1      PIC 9(4)V99.
         03 I-PAID2      PIC 9(4)V99.
         03 I-PAID3      PIC 9(4)V99.
         03 I-PAID4      PIC 9(4)V99.
         03 FILLER       PIC X(21).
      **************************************************************
      * LAYOUT FOR THE 1ST DATA LINE OF REPORT PRNTING *
      **************************************************************
       01 PRNT-DATA1.
         03 FILLER      PIC X(8)        VALUE SPACES.
         03 L-NAME1     PIC X(20).
         03 L-DEGREE1   PIC X(4).
         03 FILLER      PIC X(4)        VALUE SPACES.
         03 L-YEAR1     PIC X(4).
         03 FILLER      PIC X(3)        VALUE SPACES.
         03 L-LOAN1     PIC 99999.99.
         03 FILLER      PIC X(3)        VALUE SPACES.
         03 L-PAID01    PIC 9999.99.
         03 FILLER      PIC X(3)        VALUE SPACES.
         03 L-PAID02    PIC 9999.99.
         03 FILLER      PIC X(3)        VALUE SPACES.
         03 L-PAID03    PIC 9999.99.
         03 FILLER      PIC X(3)        VALUE SPACES.
         03 L-PAID04    PIC 9999.99.
         03 FILLER      PIC X(3)        VALUE SPACES.
         03 L-TOTAL1    PIC 99999.99.
         03 FILLER      PIC X(3)        VALUE SPACES.
         03 L-BALANCE   PIC 99999.99.
      **************************************************************
      * LAYOUT FOR THE 1ST HEADING LINE OF REPORT PRNTING *
      **************************************************************
       01 PRNT-HEADING1.
         03 FILLER      PIC X(6)        VALUE SPACES.
         03 FILLER      PIC X(20)       VALUE 'NAME'.
         03 FILLER      PIC X(8)        VALUE 'DEGREE'.
         03 FILLER      PIC X(7)        VALUE 'YEAR'.
         03 FILLER      PIC X(11)       VALUE 'LOAN'.
         03 FILLER      PIC X(10)       VALUE 'PAID1'.
         03 FILLER      PIC X(10)       VALUE 'PAID2'.
         03 FILLER      PIC X(10)       VALUE 'PAID3'.
         03 FILLER      PIC X(10)       VALUE 'PAID4'.
         03 FILLER      PIC X(11)       VALUE 'TOT PAID'.
         03 FILLER      PIC X(8)        VALUE 'BALANCE'.
       01 MISC.
      **************************************************************
      *       END OF FILE (EOF) SWITCHES *
      *       0 = NOT AT EOF 1 = AT EOF *
      **************************************************************
         03 EOF-I      PIC 9   VALUE 0.
      **************************************************************
      *       START OF PROCEDURE DIVISION       *
      **************************************************************
       PROCEDURE DIVISION.
       000-MAINLINE.
           OPEN INPUT INPUT-FILE
             OUTPUT PRNT-FILE.
           PERFORM 2000-READ-INPUT.
           PERFORM 1400-PRINT-HEAD.
           PERFORM 1500-LOOP
             UNTIL EOF-I = 1.
           CLOSE INPUT-FILE
             PRNT-FILE.
           STOP RUN.
       1400-PRINT-HEAD.
           WRITE PRNT-REC FROM PRNT-HEADING1
             AFTER ADVANCING PAGE.
           MOVE SPACES TO PRNT-REC.
           WRITE PRNT-REC
             AFTER ADVANCING 1 LINE.
       1500-LOOP.
           PERFORM 1700-CALC-TOTAL.
           PERFORM 1800-CALC-BALANCE.
           PERFORM 1600-PRINT-DATA.
           PERFORM 2000-READ-INPUT.
      **************************************************************
      * PRINTS THE SCHEDULE INFORMATION *
      **************************************************************
       1600-PRINT-DATA.
           MOVE I-NAME          TO L-NAME1.
           MOVE I-DEGREE        TO L-DEGREE1.
           MOVE I-YEAR          TO L-YEAR1.
           MOVE I-LOAN          TO L-LOAN1.
           MOVE I-PAID1         TO L-PAID01.
           MOVE I-PAID2         TO L-PAID02.
           MOVE I-PAID3         TO L-PAID03.
           MOVE I-PAID4         TO L-PAID04.
           MOVE TOTAL           TO L-TOTAL1.
             WRITE PRNT-REC FROM PRNT-DATA1
                AFTER ADVANCING 1 LINE.
      **************************************************************
      * CALCULATE THE TOTAL AMOUNT PAID
      **************************************************************
       1700-CALC-TOTAL.
           ADD I-PAID1 TO I-PAID2 GIVING
                TOTAL.
           ADD I-PAID3 TO TOTAL.
           ADD I-PAID4 TO TOTAL.
      **************************************************************
      * CALCULATE LEFTOVER BALANCE
      **************************************************************
       1800-CALC-BALANCE.
           SUBTRACT TOTAL FROM I-LOAN GIVING
                L-BALANCE.
      **************************************************************
      * READS THE INPUT FILE *
      **************************************************************
       2000-READ-INPUT.
           READ INPUT-FILE INTO INPUT-DATA
             AT END MOVE 1 TO EOF-I.
