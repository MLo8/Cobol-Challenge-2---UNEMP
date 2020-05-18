      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    UNEMPDB
       AUTHOR.        Otto B. in there.
      ****************************************************************
      *  Because I don't have access to a VSAM file utility,
      *  I created this program to try to read through and display
      *  all of the records in my VSAM file (CLMSDB).
      *  I used access mode sequential to cycle through in a
      *  simple fashion like the sample programs I've seen.
      *  Note:  It worked in that I can see the contents, but now
      *  I see that my other program (UNEMP1) is not writing the
      *  records properly.  Also noticed some key issues.
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINT-LINE ASSIGN TO PRTLINE.
           SELECT CLAIMS-DATABASE ASSIGN TO CLMSDB
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS SEQUENTIAL
                  RECORD KEY IS RECORD-KEY
                  FILE STATUS IS WS-CLMDB-STATUS.
      *
      *-------------
       DATA DIVISION.
      *-------------
       FILE SECTION.
       FD  PRINT-LINE RECORDING MODE F.
       01  PRINT-REC                                 PIC X(316).
      *
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
       01  FLAGS.
           05 LASTREC                   PIC X       VALUE 'N'.
      *
       01  WS-RECORD-COUNTER            PIC 9(02)   VALUE 0.
      *
       01  WS-CLMDB-STATUS              PIC X(02)   VALUE SPACES.
           88 WS-CLMDB-SUCCESS                      VALUE '00'.
           88 WS-CLMDB-EOF                          VALUE '10'.
      *
       01  WS-CLAIMSDB-RECORD.
           05  WS-RECORD-KEY                            PIC X(08).
           05  WS-CLMDB-CBAGE-FIELDS.
               10  WSAGE-DATE                           PIC X(10).
               10  WSAGE-INA                            PIC 9(06).
               10  WSAGE-LESS-THAN-22                   PIC 9(06).
               10  WSAGE-22-TO-24                       PIC 9(06).
               10  WSAGE-25-TO-34                       PIC 9(06).
               10  WSAGE-35-TO-44                       PIC 9(06).
               10  WSAGE-45-TO-54                       PIC 9(06).
               10  WSAGE-55-TO-59                       PIC 9(06).
               10  WSAGE-60-TO-64                       PIC 9(06).
               10  WSAGE-GRTR-THAN-64                   PIC 9(06).
           05  WS-CLMDB-CBETH-FIELDS.
               10  WSETH-DATE                            PIC X(10).
               10  WSETH-INA                             PIC 9(06).
               10  WSETH-HISPANIC-OR-LATINO              PIC 9(06).
               10  WSETH-NOT-HISPANIC-OR-LATINO          PIC 9(06).
           05  WS-CLMDB-CBIND-FIELDS.
               10  WSIND-DATE                            PIC X(10).
               10  WSIND-INA                             PIC 9(06).
               10  WSIND-WHOLESALE-TRADE                 PIC 9(06).
               10  WSIND-TRANSPORTATION-WAREHOUSE        PIC 9(06).
               10  WSIND-CONSTRUCTION                    PIC 9(06).
               10  WSIND-FINANCE-INSURANCE               PIC 9(06).
               10  WSIND-MANUFACTURING                   PIC 9(06).
               10  WSIND-AGR-FORESTRY-FISH-HUNT          PIC 9(06).
               10  WSIND-PUBLIC-ADMINISTRATION           PIC 9(06).
               10  WSIND-UTILITIES                       PIC 9(06).
               10  WSIND-ACCOM-FOODSERVICES              PIC 9(06).
               10  WSIND-INFORMATION                     PIC 9(06).
               10  WSIND-PROF-SCI-TECHSERVICES           PIC 9(06).
               10  WSIND-RE-RENTAL-LEASING               PIC 9(06).
               10  WSIND-OTHER-SERV-EXC-PUB-ADM          PIC 9(06).
               10  WSIND-MGT-OF-COMPANIES-ENT            PIC 9(06).
               10  WSIND-EDUCATIONAL-SERVICES            PIC 9(06).
               10  WSIND-MINING                          PIC 9(06).
               10  WSIND-HEALTHCARE-SOCIALASST           PIC 9(06).
               10  WSIND-ARTS-ENTERTAINMENT-REC          PIC 9(06).
               10  WSIND-ADM-SUP-WSTMGMT-REMSERV         PIC 9(06).
               10  WSIND-RETAIL-TRADE                    PIC 9(06).
           05  WS-CLMDB-CBRAC-FIELDS.
               10  WSRAC-DATE                            PIC X(10).
               10  WSRAC-INA                             PIC 9(06).
               10  WSRAC-WHITE                           PIC 9(06).
               10  WSRAC-ASIAN                           PIC 9(06).
               10  WSRAC-BLACK-OR-AFRAM                  PIC 9(06).
               10  WSRAC-AM-INDIAN-OR-AL-NATIVE          PIC 9(06).
               10  WSRAC-NAT-HAW-OR-PAC-ISLANDER         PIC 9(06).
           05  WS-CLMDB-CBGEN-FIELDS.
               10  WSGEN-DATE                            PIC X(10).
               10  WSGEN-INA                             PIC 9(06).
               10  WSGEN-FEMALE                          PIC 9(06).
               10  WSGEN-MALE                            PIC 9(06).
      *
       01  WS-HEADER1.
           05  FILLER               PIC X(08) VALUE 'REC KEY '.
           05  FILLER               PIC X(10) VALUE 'CBAGEDATE '.
           05  FILLER               PIC X(06) VALUE 'INA   '.
           05  FILLER               PIC X(06) VALUE 'LT22  '.
           05  FILLER               PIC X(06) VALUE '22TO24'.
           05  FILLER               PIC X(06) VALUE '25TO34'.
           05  FILLER               PIC X(06) VALUE '35TO44'.
           05  FILLER               PIC X(06) VALUE '45TO54'.
           05  FILLER               PIC X(06) VALUE '55TO59'.
           05  FILLER               PIC X(06) VALUE '60T064'.
           05  FILLER               PIC X(06) VALUE 'GT64  '.
           05  FILLER               PIC X(10) VALUE 'CBETHDATE '.
           05  FILLER               PIC X(06) VALUE 'INA   '.
           05  FILLER               PIC X(06) VALUE 'HISLAT'.
           05  FILLER               PIC X(06) VALUE 'NOTHIS'.
           05  FILLER               PIC X(10) VALUE 'CBINDDATE '.
           05  FILLER               PIC X(06) VALUE 'INA   '.
           05  FILLER               PIC X(06) VALUE 'WHTRD '.
           05  FILLER               PIC X(06) VALUE 'TRWH  '.
           05  FILLER               PIC X(06) VALUE 'CONST '.
           05  FILLER               PIC X(06) VALUE 'FNIN  '.
           05  FILLER               PIC X(06) VALUE 'MANF  '.
           05  FILLER               PIC X(06) VALUE 'FSHNT '.
           05  FILLER               PIC X(06) VALUE 'PUBAD '.
           05  FILLER               PIC X(06) VALUE 'UTIL  '.
           05  FILLER               PIC X(06) VALUE 'ACFD  '.
           05  FILLER               PIC X(06) VALUE 'INFO  '.
           05  FILLER               PIC X(06) VALUE 'TECH  '.
           05  FILLER               PIC X(06) VALUE 'RNTLS '.
           05  FILLER               PIC X(06) VALUE 'OTHER '.
           05  FILLER               PIC X(06) VALUE 'COENT '.
           05  FILLER               PIC X(06) VALUE 'EDSRV '.
           05  FILLER               PIC X(06) VALUE 'MINING'.
           05  FILLER               PIC X(06) VALUE 'HCSA  '.
           05  FILLER               PIC X(06) VALUE 'ARTENT'.
           05  FILLER               PIC X(06) VALUE 'WASTE '.
           05  FILLER               PIC X(06) VALUE 'RETAIL'.
           05  FILLER               PIC X(10) VALUE 'CBRACDATE '.
           05  FILLER               PIC X(06) VALUE 'INA   '.
           05  FILLER               PIC X(06) VALUE 'WHITE '.
           05  FILLER               PIC X(06) VALUE 'ASIAN '.
           05  FILLER               PIC X(06) VALUE 'BLACK '.
           05  FILLER               PIC X(06) VALUE 'NATAM '.
           05  FILLER               PIC X(06) VALUE 'HAWPAC'.
           05  FILLER               PIC X(10) VALUE 'CBGENDATE '.
           05  FILLER               PIC X(06) VALUE 'INA   '.
           05  FILLER               PIC X(06) VALUE 'FEMALE'.
           05  FILLER               PIC X(06) VALUE ' MALE '.
      *
       01  WS-HEADER2               PIC X(310) VALUE ALL '-'.
      *
      *------------------
       PROCEDURE DIVISION.
      *------------------
       OPEN-FILES.
           OPEN OUTPUT PRINT-LINE.
           DISPLAY 'ACCESSING CLAIMS DATABASE'.
           OPEN INPUT CLAIMS-DATABASE.
           DISPLAY WS-CLMDB-STATUS
           IF WS-CLMDB-SUCCESS
              DISPLAY "FILE OPEN SUCCESSFUL"
           ELSE
              DISPLAY "FILE OPENING ERROR"
              GO TO CLOSE-STOP
           END-IF.

      *
       PRINT-HEADERS.
           WRITE PRINT-REC FROM WS-HEADER1.
           WRITE PRINT-REC FROM WS-HEADER2.
      *
       READ-NEXT-RECORD.
           PERFORM READ-RECORD
            PERFORM UNTIL LASTREC = 'Y'
            PERFORM WRITE-RECORD
            PERFORM READ-RECORD
            END-PERFORM
           .
      *
       CLOSE-STOP.
           CLOSE PRINT-LINE.
           CLOSE CLAIMS-DATABASE.
           STOP RUN.
      *
       READ-RECORD.
           READ CLAIMS-DATABASE
           AT END MOVE 'Y' TO LASTREC
           END-READ.
      *
       WRITE-RECORD.
           MOVE CLAIMSDB-RECORD TO WS-CLAIMSDB-RECORD
           MOVE  WS-CLAIMSDB-RECORD TO PRINT-REC.
           WRITE PRINT-REC.
      *
