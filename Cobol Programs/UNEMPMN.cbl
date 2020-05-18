      *------------------------
       IDENTIFICATION DIVISION.
      *------------------------
       PROGRAM-ID.     UNEMPMN
       AUTHOR.         Otto B. Routine.
      ****************************************************************
      *  Dataset "UNEMPIN" contains the user input selections of
      *  a Record Key and/or an All Records indicator.
      *
      *  The main program (UNEMPMN) passes these two user input
      *  data items to the first subroutine (UNEMPSB) which returns
      *  either a table (if All Records indicator is Y) or a single
      *  record (if All Records indicator is N and a valid record key
      *  is provided).
      *
      *  The first subroutine (UNEMPSB) calls one of two other
      *  subroutines (RTALLREC, or RTONEREC) depending on the
      *  AllRecords indicator.  This is because if AllRecords
      *  is selected, I need to access the VSAM file sequentially
      *  in order to read through and retrieve all of the records
      *  (RTALLREC), but I need to access the VSAM file randomly
      *  in order to retrieve a single record based on the record key
      *  provided by the user (RTONEREC)
      *
      *  Report is in CSV format with headers and is displayed through
      *  PRTLINE as well as printed to a dataset (UNEMPOUT)
      *  Some basic error handling is included.
      ****************************************************************
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINT-LINE ASSIGN TO PRTLINE.
           SELECT USER-INPUT ASSIGN TO USRINP.
           SELECT PGM-OUTPUT ASSIGN TO PGMOUT.
      *-------------
       DATA DIVISION.
      *-------------
       FILE SECTION.
       FD  PRINT-LINE RECORDING MODE F.
       01  PRINT-REC                    PIC X(357).
      *
       FD  USER-INPUT RECORDING MODE F.
       01  USER-INPUT-RECORD.
           05  USER-RECORD-ID           PIC X(08)   VALUE '20110701'.
           05  USER-ALL-RECORDS-ID      PIC X       VALUE 'N'.
               88 ALL-RECORDS-REQUESTED             VALUE 'Y'.
               88 VALID-ALL-RECORDS-ID              VALUES 'Y' 'N'.
           05  FILLER                   PIC X(71).
      *
       FD  PGM-OUTPUT RECORDING MODE F.
       01  OUTPUT-ROW                   PIC X(357).
      *
       WORKING-STORAGE SECTION.
       01  WS-SUBROUTINE                PIC X(8)    VALUE 'UNEMPSB'.
       01  WS-CLAIMSDB-ROW-COUNTER      PIC 9(02)   VALUE 0.
       01  WS-CLAIMSDB-TABLE-MAXROWS    PIC 9(03)   VALUE 0.
       01  WS-CLAIMSDB-TABLE.
           05  WS-CLAIMSDB-ROW PIC X(357) OCCURS 110 TIMES.
       01  WS-CLAIMSDB-RECORD                           PIC X(357).
      *     05  WS-RECORD-KEY                            PIC X(08).
      *    05  WS-CLMDB-CBAGE-FIELDS.
      *        10  WSAGE-DATE                           PIC X(10).
      *        10  WSAGE-INA                            PIC 9(06).
      *        10  WSAGE-LESS-THAN-22                   PIC 9(06).
      *        10  WSAGE-22-TO-24                       PIC 9(06).
      *        10  WSAGE-25-TO-34                       PIC 9(06).
      *        10  WSAGE-35-TO-44                       PIC 9(06).
      *        10  WSAGE-45-TO-54                       PIC 9(06).
      *        10  WSAGE-55-TO-59                       PIC 9(06).
      *        10  WSAGE-60-TO-64                       PIC 9(06).
      *        10  WSAGE-GRTR-THAN-64                   PIC 9(06).
      *    05  WS-CLMDB-CBETH-FIELDS.
      *        10  WSETH-DATE                            PIC X(10).
      *        10  WSETH-INA                             PIC 9(06).
      *        10  WSETH-HISPANIC-OR-LATINO              PIC 9(06).
      *        10  WSETH-NOT-HISPANIC-OR-LATINO          PIC 9(06).
      *    05  WS-CLMDB-CBIND-FIELDS.
      *        10  WSIND-DATE                            PIC X(10).
      *        10  WSIND-INA                             PIC 9(06).
      *        10  WSIND-WHOLESALE-TRADE                 PIC 9(06).
      *        10  WSIND-TRANSPORTATION-WAREHOUSE        PIC 9(06).
      *        10  WSIND-CONSTRUCTION                    PIC 9(06).
      *        10  WSIND-FINANCE-INSURANCE               PIC 9(06).
      *        10  WSIND-MANUFACTURING                   PIC 9(06).
      *        10  WSIND-AGR-FORESTRY-FISH-HUNT          PIC 9(06).
      *        10  WSIND-PUBLIC-ADMINISTRATION           PIC 9(06).
      *        10  WSIND-UTILITIES                       PIC 9(06).
      *        10  WSIND-ACCOM-FOODSERVICES              PIC 9(06).
      *        10  WSIND-INFORMATION                     PIC 9(06).
      *        10  WSIND-PROF-SCI-TECHSERVICES           PIC 9(06).
      *        10  WSIND-RE-RENTAL-LEASING               PIC 9(06).
      *        10  WSIND-OTHER-SERV-EXC-PUB-ADM          PIC 9(06).
      *        10  WSIND-MGT-OF-COMPANIES-ENT            PIC 9(06).
      *        10  WSIND-EDUCATIONAL-SERVICES            PIC 9(06).
      *        10  WSIND-MINING                          PIC 9(06).
      *        10  WSIND-HEALTHCARE-SOCIALASST           PIC 9(06).
      *        10  WSIND-ARTS-ENTERTAINMENT-REC          PIC 9(06).
      *        10  WSIND-ADM-SUP-WSTMGMT-REMSERV         PIC 9(06).
      *        10  WSIND-RETAIL-TRADE                    PIC 9(06).
      *    05  WS-CLMDB-CBRAC-FIELDS.
      *        10  WSRAC-DATE                            PIC X(10).
      *        10  WSRAC-INA                             PIC 9(06).
      *        10  WSRAC-WHITE                           PIC 9(06).
      *        10  WSRAC-ASIAN                           PIC 9(06).
      *        10  WSRAC-BLACK-OR-AFRAM                  PIC 9(06).
      *        10  WSRAC-AM-INDIAN-OR-AL-NATIVE          PIC 9(06).
      *        10  WSRAC-NAT-HAW-OR-PAC-ISLANDER         PIC 9(06).
      *    05  WS-CLMDB-CBGEN-FIELDS.
      *        10  WSGEN-DATE                            PIC X(10).
      *        10  WSGEN-INA                             PIC 9(06).
      *        10  WSGEN-FEMALE                          PIC 9(06).
      *        10  WSGEN-MALE                            PIC 9(06).
      *
       01  WS-HEADER1.
           05  FILLER               PIC X(09) VALUE 'REC KEY |'.
           05  FILLER               PIC X(11) VALUE 'CBAGEDATE |'.
           05  FILLER               PIC X(07) VALUE 'INA   |'.
           05  FILLER               PIC X(07) VALUE 'LT22  |'.
           05  FILLER               PIC X(07) VALUE '22TO24|'.
           05  FILLER               PIC X(07) VALUE '25TO34|'.
           05  FILLER               PIC X(07) VALUE '35TO44|'.
           05  FILLER               PIC X(07) VALUE '45TO54|'.
           05  FILLER               PIC X(07) VALUE '55TO59|'.
           05  FILLER               PIC X(07) VALUE '60T064|'.
           05  FILLER               PIC X(07) VALUE 'GT64  |'.
           05  FILLER               PIC X(11) VALUE 'CBETHDATE |'.
           05  FILLER               PIC X(07) VALUE 'INA   |'.
           05  FILLER               PIC X(07) VALUE 'HISLAT|'.
           05  FILLER               PIC X(07) VALUE 'NOTHIS|'.
           05  FILLER               PIC X(11) VALUE 'CBINDDATE |'.
           05  FILLER               PIC X(07) VALUE 'INA   |'.
           05  FILLER               PIC X(07) VALUE 'WHTRD |'.
           05  FILLER               PIC X(07) VALUE 'TRWH  |'.
           05  FILLER               PIC X(07) VALUE 'CONST |'.
           05  FILLER               PIC X(07) VALUE 'FNIN  |'.
           05  FILLER               PIC X(07) VALUE 'MANF  |'.
           05  FILLER               PIC X(07) VALUE 'FSHNT |'.
           05  FILLER               PIC X(07) VALUE 'PUBAD |'.
           05  FILLER               PIC X(07) VALUE 'UTIL  |'.
           05  FILLER               PIC X(07) VALUE 'ACFD  |'.
           05  FILLER               PIC X(07) VALUE 'INFO  |'.
           05  FILLER               PIC X(07) VALUE 'TECH  |'.
           05  FILLER               PIC X(07) VALUE 'RNTLS |'.
           05  FILLER               PIC X(07) VALUE 'OTHER |'.
           05  FILLER               PIC X(07) VALUE 'COENT |'.
           05  FILLER               PIC X(07) VALUE 'EDSRV |'.
           05  FILLER               PIC X(07) VALUE 'MINING|'.
           05  FILLER               PIC X(07) VALUE 'HCSA  |'.
           05  FILLER               PIC X(07) VALUE 'ARTENT'.
           05  FILLER               PIC X(07) VALUE 'WASTE |'.
           05  FILLER               PIC X(07) VALUE 'RETAIL|'.
           05  FILLER               PIC X(11) VALUE 'CBRACDATE |'.
           05  FILLER               PIC X(07) VALUE 'INA   |'.
           05  FILLER               PIC X(07) VALUE 'WHITE |'.
           05  FILLER               PIC X(07) VALUE 'ASIAN |'.
           05  FILLER               PIC X(07) VALUE 'BLACK |'.
           05  FILLER               PIC X(07) VALUE 'NATAM |'.
           05  FILLER               PIC X(07) VALUE 'HAWPAC|'.
           05  FILLER               PIC X(11) VALUE 'CBGENDATE |'.
           05  FILLER               PIC X(07) VALUE 'INA   |'.
           05  FILLER               PIC X(07) VALUE 'FEMALE|'.
           05  FILLER               PIC X(07) VALUE ' MALE '.
      *
       01  WS-HEADER2               PIC X(357) VALUE ALL '-'.
      *
      *------------------------
       PROCEDURE DIVISION.
      *------------------------
       OPEN-FILES.
           DISPLAY 'IN MAIN MODULE'.
           OPEN OUTPUT PRINT-LINE.
           OPEN OUTPUT PGM-OUTPUT.
           OPEN INPUT USER-INPUT.
           READ USER-INPUT.
           DISPLAY 'USER INPUT - ALL RECORDS INDICATOR: '
                    USER-ALL-RECORDS-ID.
           DISPLAY 'USER INPUT SINGLE RECORD ID: '
                    USER-RECORD-ID.
           PERFORM ERROR-HANDLING1.
           CALL WS-SUBROUTINE USING USER-RECORD-ID,
                                    USER-ALL-RECORDS-ID,
                                    WS-CLAIMSDB-TABLE-MAXROWS,
                                    WS-CLAIMSDB-TABLE,
                                    WS-CLAIMSDB-RECORD.
           DISPLAY 'BACK IN MAIN MODULE'.
           IF ALL-RECORDS-REQUESTED
              DISPLAY WS-CLAIMSDB-TABLE-MAXROWS
              PERFORM PRINT-CLAIMSDB-HEADERS
              PERFORM PRINT-CLAIMSDB-ROWS VARYING
                      WS-CLAIMSDB-ROW-COUNTER FROM 1 BY 1 UNTIL
                      WS-CLAIMSDB-ROW-COUNTER EQUAL
                      WS-CLAIMSDB-TABLE-MAXROWS
           ELSE
              PERFORM PRINT-CLAIMSDB-HEADERS
              PERFORM PRINT-CLAIMSDB-RECORD
           END-IF.
      *
       CLOSE-STOP.
           CLOSE PRINT-LINE.
           CLOSE PGM-OUTPUT.
           CLOSE USER-INPUT.
           STOP RUN.
      *
       ERROR-HANDLING1.
           IF NOT VALID-ALL-RECORDS-ID
              DISPLAY
              "All Records Indicator must by Y or N, please try again."
              GO TO CLOSE-STOP
           END-IF.
      *
       PRINT-CLAIMSDB-HEADERS.
           WRITE OUTPUT-ROW FROM WS-HEADER1.
           WRITE OUTPUT-ROW FROM WS-HEADER2.
           WRITE PRINT-REC FROM WS-HEADER1.
           WRITE PRINT-REC FROM WS-HEADER2.
      *
       PRINT-CLAIMSDB-RECORD.
           WRITE OUTPUT-ROW FROM WS-CLAIMSDB-RECORD.
           WRITE PRINT-REC FROM WS-CLAIMSDB-RECORD.
      *
       PRINT-CLAIMSDB-ROWS.
           MOVE WS-CLAIMSDB-ROW(WS-CLAIMSDB-ROW-COUNTER) TO OUTPUT-ROW.
           WRITE OUTPUT-ROW.
           MOVE WS-CLAIMSDB-ROW(WS-CLAIMSDB-ROW-COUNTER) TO PRINT-REC.
           WRITE PRINT-REC.
      *

