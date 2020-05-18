      *------------------------
       IDENTIFICATION DIVISION.
      *------------------------
       PROGRAM-ID.      RTONEREC.
      *---------------------
       ENVIRONMENT DIVISION.
      *---------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLAIMS-DATABASE ASSIGN TO CLMSDB
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS RECORD-KEY
                  FILE STATUS IS WS-CLMDB-STATUS.
      *------------------------
       DATA DIVISION.
      *------------------------
       FILE SECTION.
       FD  CLAIMS-DATABASE.
       01  CLAIMSDB-RECORD.
           05  RECORD-KEY                               PIC X(08).
           05  CLMDB-CBAGE-FIELDS.
               10  CBAGE-DATE                           PIC X(10).
               10  CBAGE-INA                            PIC 9(06).
               10  CBAGE-LESS-THAN-22                   PIC 9(06).
               10  CBAGE-22-TO-24                       PIC 9(06).
               10  CBAGE-25-TO-34                       PIC 9(06).
               10  CBAGE-35-TO-44                       PIC 9(06).
               10  CBAGE-45-TO-54                       PIC 9(06).
               10  CBAGE-55-TO-59                       PIC 9(06).
               10  CBAGE-60-TO-64                       PIC 9(06).
               10  CBAGE-GRTR-THAN-64                   PIC 9(06).
           05  CLMDB-CBETH-FIELDS.
               10  CBETH-DATE                            PIC X(10).
               10  CBETH-INA                             PIC 9(06).
               10  CBETH-HISPANIC-OR-LATINO              PIC 9(06).
               10  CBETH-NOT-HISPANIC-OR-LATINO          PIC 9(06).
           05  CLMDB-CBIND-FIELDS.
               10  CBIND-DATE                            PIC X(10).
               10  CBIND-INA                             PIC 9(06).
               10  CBIND-WHOLESALE-TRADE                 PIC 9(06).
               10  CBIND-TRANSPORTATION-WAREHOUSE        PIC 9(06).
               10  CBIND-CONSTRUCTION                    PIC 9(06).
               10  CBIND-FINANCE-INSURANCE               PIC 9(06).
               10  CBIND-MANUFACTURING                   PIC 9(06).
               10  CBIND-AGR-FORESTRY-FISH-HUNT          PIC 9(06).
               10  CBIND-PUBLIC-ADMINISTRATION           PIC 9(06).
               10  CBIND-UTILITIES                       PIC 9(06).
               10  CBIND-ACCOM-FOODSERVICES              PIC 9(06).
               10  CBIND-INFORMATION                     PIC 9(06).
               10  CBIND-PROF-SCI-TECHSERVICES           PIC 9(06).
               10  CBIND-RE-RENTAL-LEASING               PIC 9(06).
               10  CBIND-OTHER-SERV-EXC-PUB-ADM          PIC 9(06).
               10  CBIND-MGT-OF-COMPANIES-ENT            PIC 9(06).
               10  CBIND-EDUCATIONAL-SERVICES            PIC 9(06).
               10  CBIND-MINING                          PIC 9(06).
               10  CBIND-HEALTHCARE-SOCIALASST           PIC 9(06).
               10  CBIND-ARTS-ENTERTAINMENT-REC          PIC 9(06).
               10  CBIND-ADM-SUP-WSTMGMT-REMSERV         PIC 9(06).
               10  CBIND-RETAIL-TRADE                    PIC 9(06).
           05  CLMDB-CBRAC-FIELDS.
               10  CBRAC-DATE                            PIC X(10).
               10  CBRAC-INA                             PIC 9(06).
               10  CBRAC-WHITE                           PIC 9(06).
               10  CBRAC-ASIAN                           PIC 9(06).
               10  CBRAC-BLACK-OR-AFRAM                  PIC 9(06).
               10  CBRAC-AM-INDIAN-OR-AL-NATIVE          PIC 9(06).
               10  CBRAC-NAT-HAW-OR-PAC-ISLANDER         PIC 9(06).
           05  CLMDB-CBGEN-FIELDS.
               10  CBGEN-DATE                            PIC X(10).
               10  CBGEN-INA                             PIC 9(06).
               10  CBGEN-FEMALE                          PIC 9(06).
               10  CBGEN-MALE                            PIC 9(06).
      *
       WORKING-STORAGE SECTION.
       01  WS-CLMDB-STATUS              PIC X(02)   VALUE SPACES.
           88 WS-CLMDB-SUCCESS                      VALUE '00'.
           88 WS-BAD-KEY                            VALUES '20' '23'.
           88 WS-CLMDB-EOF                          VALUE '10'.
       01  WS-CLAIMSDB-CSV              PIC X(357).
      *
       LINKAGE SECTION.
       01  LS2-RECORD-ID                                PIC X(08).
       01  LS2-CLAIMSDB-RECORD                          PIC X(357).
      *--------------------------------------------
       PROCEDURE DIVISION USING LS2-RECORD-ID,
                                LS2-CLAIMSDB-RECORD.
      *--------------------------------------------
           DISPLAY 'IN SUBROUTINE TO RETRIEVE ONE RECORD, RTONEREC'
           OPEN INPUT CLAIMS-DATABASE.
           DISPLAY 'OPENING STATUS ' WS-CLMDB-STATUS.
           IF WS-CLMDB-SUCCESS
              DISPLAY "FILE OPEN SUCCESSFUL"
           ELSE
              DISPLAY "FILE OPENING ERROR"
           END-IF.
      *
           MOVE LS2-RECORD-ID TO RECORD-KEY.
           READ CLAIMS-DATABASE.
           DISPLAY 'READING STATUS ' WS-CLMDB-STATUS.
           IF WS-BAD-KEY
              DISPLAY 'Invalid or duplicte key, please try again.'
           ELSE
              PERFORM CREATE-CSV-FILE
              MOVE WS-CLAIMSDB-CSV TO LS2-CLAIMSDB-RECORD
           END-IF
           EXIT PROGRAM.
       CREATE-CSV-FILE.
           STRING
           RECORD-KEY ","
           CBAGE-DATE ","
           CBAGE-INA ","
           CBAGE-LESS-THAN-22 ","
           CBAGE-22-TO-24 ","
           CBAGE-25-TO-34 ","
           CBAGE-35-TO-44 ","
           CBAGE-45-TO-54 ","
           CBAGE-55-TO-59 ","
           CBAGE-60-TO-64 ","
           CBAGE-GRTR-THAN-64 ","
           CBETH-DATE ","
           CBETH-INA ","
           CBETH-HISPANIC-OR-LATINO ","
           CBETH-NOT-HISPANIC-OR-LATINO ","
           CBIND-DATE ","
           CBIND-INA ","
           CBIND-WHOLESALE-TRADE ","
           CBIND-TRANSPORTATION-WAREHOUSE ","
           CBIND-CONSTRUCTION ","
           CBIND-FINANCE-INSURANCE ","
           CBIND-MANUFACTURING ","
           CBIND-AGR-FORESTRY-FISH-HUNT ","
           CBIND-PUBLIC-ADMINISTRATION ","
           CBIND-UTILITIES ","
           CBIND-ACCOM-FOODSERVICES ","
           CBIND-INFORMATION ","
           CBIND-PROF-SCI-TECHSERVICES ","
           CBIND-RE-RENTAL-LEASING ","
           CBIND-OTHER-SERV-EXC-PUB-ADM ","
           CBIND-MGT-OF-COMPANIES-ENT ","
           CBIND-EDUCATIONAL-SERVICES ","
           CBIND-MINING ","
           CBIND-HEALTHCARE-SOCIALASST ","
           CBIND-ARTS-ENTERTAINMENT-REC ","
           CBIND-ADM-SUP-WSTMGMT-REMSERV ","
           CBIND-RETAIL-TRADE ","
           CBRAC-DATE ","
           CBRAC-INA ","
           CBRAC-WHITE ","
           CBRAC-ASIAN ","
           CBRAC-BLACK-OR-AFRAM ","
           CBRAC-AM-INDIAN-OR-AL-NATIVE ","
           CBRAC-NAT-HAW-OR-PAC-ISLANDER ","
           CBGEN-DATE ","
           CBGEN-INA ","
           CBGEN-FEMALE ","
           CBGEN-MALE
           DELIMITED BY SIZE INTO WS-CLAIMSDB-CSV.
      *
