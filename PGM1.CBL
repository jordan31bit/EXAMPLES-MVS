          IDENTIFICATION DIVISION.
            PROGRAM-ID. EXAMP-PGM1.
          ENVIRONMENT DIVISION.
          CONFIGURATION SECTION.
          INPUT-OUTPUT SECTION.
          FILE-CONTROL.
            SELECT IN-PAYROLL
            ASSIGN TO UT-S-INPUTF.
            SELECT OUT-REPORT
            ASSIGN TO UT-S-OUTRPT.
            SELECT SORT-WRKFILE
            ASSIGN TO UR-S-WORK.
          DATA DIVISION.
          FILE SECTION.
          FD  IN-PAYROLL
              RECORDING MODE IS F
              LABEL RECORDS ARE OMITTED
              RECORD CONTAINS 80 CHARACTERS
              BLOCK CONTAINS 10 RECORDS
              DATA RECORD IS RECORDS-IN.
          01  RECORDS-IN.
              02 EMPLOYEE-ID PIC 9(10).
              02 EMPLOYEE-NAME PIC X(23).
              02 EMPLOYEE-SALARY PIC 9(4).
              02 FILLER PIC X(43).
          FD  OUT-REPORT
              RECORDING MODE IS F
              LABEL RECORDS ARE OMITTED
              RECORD CONTAINS 80 CHARACTERS
              BLOCK CONTAINS 10 RECORDS
              DATA RECORD IS PAYROLL-REPORT.
          01  PAYROLL-REPORT.
              02 RPT-ID PIC 9(10).
              02 RPT-NAME PIC X(23).
              02 RPT-SALARY PIC 9(4).
              02 FILLER PIC X(43).
          SD  SORT-WRKFILE
              RECORDING MODE IS F
              RECORD CONTAINS 80 CHARACTERS
              DATA RECORD IS SORT-RECORD.
          01  SORT-RECORD.
              02 SD-ID PIC 9(10).
              02 SD-NAME PIC X(23).
              02 SD-SALARY PIC 9(4).
              02 FILLER PIC X(43).
      *
          WORKING-STORAGE SECTION.
              01 WS-EMPLOYEE.
                02 WS-ID PIC 9(10).
                02 WS-NAME PIC X(23).
                02 WS-SALARY PIC 9(4).
                02 FILLER PIC X(43).
          01 WS-EOF PIC A VALUE 'N'.
      *
          PROCEDURE DIVISION.
          MAIN.
           OPEN INPUT IN-PAYROLL
           PERFORM READ-INPUT UNTIL WS-EOF = 'Y'
           CLOSE IN-PAYROLL
           PERFORM SORT-RECORDS
           STOP RUN.
      *
          SORT-RECORDS.
           SORT SORT-WRKFILE
             ON ASCENDING KEY SD-NAME
             USING IN-PAYROLL GIVING OUT-REPORT.
      *
          READ-INPUT.
           READ IN-PAYROLL
           AT END MOVE 'Y' TO WS-EOF.
           MOVE RECORDS-IN TO WS-EMPLOYEE
           DISPLAY WS-EMPLOYEE
           MOVE ZEROS TO RECORDS-IN.
