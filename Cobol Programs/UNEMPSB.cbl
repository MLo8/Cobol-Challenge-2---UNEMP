      *------------------------
       IDENTIFICATION DIVISION.
      *------------------------
       PROGRAM-ID.      UNEMPSB.
      *---------------------
       ENVIRONMENT DIVISION.
      *---------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *------------------------
       DATA DIVISION.
      *------------------------
       WORKING-STORAGE SECTION.
       01  WS-CLMDB-STATUS              PIC X(02)   VALUE SPACES.
           88 WS-CLMDB-SUCCESS                      VALUE '00'.
           88 WS-CLMDB-EOF                          VALUE '10'.
       01  WS-COUNTER                   PIC X(03)   VALUE '000'.
       01  WS-RETRIEVE-ALL-RECORDS      PIC X(08)   VALUE 'RTALLREC'.
       01  WS-RETRIEVE-SINGLE-RECORD    PIC X(08)   VALUE 'RTONEREC'.
      *
       LINKAGE SECTION.
       01  LS-RECORD-ID                                 PIC X(08).
       01  LS-ALL-RECORDS-ID                            PIC X.
           88 LS-ALL-RECORDS-REQUESTED  VALUE 'Y'.
       01  LS-CLAIMSDB-TABLE-MAXROWS                    PIC 9(03).
       01  LS-CLAIMSDB-TABLE.
           05  LS-CLAIMSDB-ROW PIC X(357) OCCURS 110 TIMES.
       01  LS-CLAIMSDB-RECORD                           PIC X(357).
      *    05  WS-RECORD-KEY                            PIC X(08).
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
      *--------------------------------------------
       PROCEDURE DIVISION USING LS-RECORD-ID,
                                LS-ALL-RECORDS-ID,
                                LS-CLAIMSDB-TABLE-MAXROWS,
                                LS-CLAIMSDB-TABLE,
                                LS-CLAIMSDB-RECORD.
      *--------------------------------------------
           DISPLAY 'IN FIRST CALLED SUBROUTINE.'
           IF LS-ALL-RECORDS-REQUESTED
              CALL WS-RETRIEVE-ALL-RECORDS USING
                                           LS-CLAIMSDB-TABLE-MAXROWS
                                           LS-CLAIMSDB-TABLE
           ELSE
              CALL WS-RETRIEVE-SINGLE-RECORD USING
                                             LS-RECORD-ID
                                             LS-CLAIMSDB-RECORD
           END-IF.
           EXIT PROGRAM.