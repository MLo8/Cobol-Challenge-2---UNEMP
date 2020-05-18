      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    UNEMP
       AUTHOR.        Otto B. Employed.
      ***************************************************************
      *
      *  Using the UNSTRING function to extract the distinct fields
      *  from the CSV file and move them to the record elements.
      *
      *  I used STRING to swap the order of the Record Key from
      *  MMDDYYYY to YYYYMMDD so that it would sort properly.
      *
      *  Each of the 5 input files populate a portion of the records
      *  When processing the first input file (claims by age) I used
      *  WRITE to populate the VSAM file because the written records
      *  are new. When procdessing subsequent input files I used
      *  REWRITE because by then the records exist and need to be
      *  updated... not sure if there is a better way to do this.
      *
      ***************************************************************
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINT-LINE ASSIGN TO PRTLINE.
           SELECT CLAIMS-BY-AGE ASSIGN TO CLMAGE.
           SELECT CLAIMS-BY-ETHNICITY ASSIGN TO CLMETH.
           SELECT CLAIMS-BY-INDUSTRY ASSIGN TO CLMIND.
           SELECT CLAIMS-BY-RACE ASSIGN TO CLMRAC.
           SELECT CLAIMS-BY-GENDER ASSIGN TO CLMGEN.
           SELECT CLAIMS-DATABASE ASSIGN TO CLMSDB
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS RANDOM
                  RECORD KEY IS RECORD-KEY
                  FILE STATUS IS WS-CLMDB-STATUS.
      *
      *-------------
       DATA DIVISION.
      *-------------
       FILE SECTION.
       FD  PRINT-LINE RECORDING MODE F.
       01  PRINT-REC                                PIC X(144).
      *
       FD  CLAIMS-BY-AGE RECORDING MODE F.
       01  CBAGE-REC                                PIC X(144).
      *
       FD  CLAIMS-BY-ETHNICITY RECORDING MODE F.
       01  CBETH-REC                                PIC X(144).
      *
       FD  CLAIMS-BY-INDUSTRY RECORDING MODE F.
       01  CBIND-REC                                PIC X(144).
      *
       FD  CLAIMS-BY-RACE RECORDING MODE F.
       01  CBRAC-REC                                PIC X(144).
      *
       FD  CLAIMS-BY-GENDER RECORDING MODE F.
       01  CBGEN-REC                                PIC X(144).
      *
       FD  CLAIMS-DATABASE.
       01  CLAIMSDB-RECORD.
           05  RECORD-KEY                               PIC X(08).
           05  CLMDB-CBAGE-FIELDS.
               10  DB-CBAGE-DATE                        PIC X(10).
               10  DB-CBAGE-INA                         PIC 9(06).
               10  DB-CBAGE-LESS-THAN-22                PIC 9(06).
               10  DB-CBAGE-22-TO-24                    PIC 9(06).
               10  DB-CBAGE-25-TO-34                    PIC 9(06).
               10  DB-CBAGE-35-TO-44                    PIC 9(06).
               10  DB-CBAGE-45-TO-54                    PIC 9(06).
               10  DB-CBAGE-55-TO-59                    PIC 9(06).
               10  DB-CBAGE-60-TO-64                    PIC 9(06).
               10  DB-CBAGE-GRTR-THAN-64                PIC 9(06).
           05  CLMDB-CBETH-FIELDS.
               10  DB-CBETH-DATE                        PIC X(10).
               10  DB-CBETH-INA                         PIC 9(06).
               10  DB-CBETH-HISPANIC-OR-LATINO          PIC 9(06).
               10  DB-CBETH-NOT-HISP-OR-LATINO          PIC 9(06).
           05  CLMDB-CBIND-FIELDS.
               10  DB-CBIND-DATE                        PIC X(10).
               10  DB-CBIND-INA                         PIC 9(06).
               10  DB-CBIND-WHOLESALE-TRADE             PIC 9(06).
               10  DB-CBIND-TRANS-WAREHOUSE             PIC 9(06).
               10  DB-CBIND-CONSTRUCTION                PIC 9(06).
               10  DB-CBIND-FINANCE-INSURANCE           PIC 9(06).
               10  DB-CBIND-MANUFACTURING               PIC 9(06).
               10  DB-CBIND-AGR-FOR-FISH-HUNT           PIC 9(06).
               10  DB-CBIND-PUBLIC-ADMINISTRATION       PIC 9(06).
               10  DB-CBIND-UTILITIES                   PIC 9(06).
               10  DB-CBIND-ACCOM-FOODSERVICES          PIC 9(06).
               10  DB-CBIND-INFORMATION                 PIC 9(06).
               10  DB-CBIND-PROF-SCI-TECHSERVICES       PIC 9(06).
               10  DB-CBIND-RE-RENTAL-LEASING           PIC 9(06).
               10  DB-CBIND-OTH-SERV-EXC-PUB-ADM        PIC 9(06).
               10  DB-CBIND-MGT-OF-COMPANIES-ENT        PIC 9(06).
               10  DB-CBIND-EDUCATIONAL-SERVICES        PIC 9(06).
               10  DB-CBIND-MINING                      PIC 9(06).
               10  DB-CBIND-HEALTHCARE-SOCIALASST       PIC 9(06).
               10  DB-CBIND-ARTS-ENT-REC                PIC 9(06).
               10  DB-CBIND-ADM-SUP-WSTMGMT-RMSRV       PIC 9(06).
               10  DB-CBIND-RETAIL-TRADE                PIC 9(06).
           05  CLMDB-CBRAC-FIELDS.
               10  DB-CBRAC-DATE                        PIC X(10).
               10  DB-CBRAC-INA                         PIC 9(06).
               10  DB-CBRAC-WHITE                       PIC 9(06).
               10  DB-CBRAC-ASIAN                       PIC 9(06).
               10  DB-CBRAC-BLACK-OR-AFRAM              PIC 9(06).
               10  DB-CBRAC-AM-IND-OR-AL-NATIVE         PIC 9(06).
               10  DB-CBRAC-NAT-HAW-OR-PAC-ISL          PIC 9(06).
           05  CLMDB-CBGEN-FIELDS.
               10  DB-CBGEN-DATE                        PIC X(10).
               10  DB-CBGEN-INA                         PIC 9(06).
               10  DB-CBGEN-FEMALE                      PIC 9(06).
               10  DB-CBGEN-MALE                        PIC 9(06).
      *
       WORKING-STORAGE SECTION.
       01  FLAGS.
           05 LASTREC                   PIC X       VALUE SPACE.
      *
       01  DIVIDER.
           05  FILLER                   PIC X(144)  VALUE ALL '*'.
      *
       01  WS-CLMDB-STATUS              PIC X(02)   VALUE SPACES.
           88 WS-CLMDB-SUCCESS                      VALUE '00'.
           88 WS-CLMDB-NOT-PRESENT                  VALUE '35'.
      *
       01  WS-YYYYMMDD                  PIC X(08)   VALUE SPACES.
      *
       01  CBAGE-FIELDS.
           05  CBAGE-RECORD-ID.
               15  CBAGE-MM                             PIC X(02).
               15  CBAGE-DD                             PIC X(02).
               15  CBAGE-YYYY                           PIC X(04).
           05  CBAGE-FIELDS-NF.
               10  CBAGE-DATE                           PIC X(10).
               10  CBAGE-INA                            PIC 9(06).
               10  LESS-THAN-22                         PIC 9(06).
               10  22-TO-24                             PIC 9(06).
               10  25-TO-34                             PIC 9(06).
               10  35-TO-44                             PIC 9(06).
               10  45-TO-54                             PIC 9(06).
               10  55-TO-59                             PIC 9(06).
               10  60-TO-64                             PIC 9(06).
               10  GRTR-THAN-64                         PIC 9(06).
           05  FILLER                                   PIC X(72).
      *
       01  CBETH-FIELDS.
           05  CBETH-RECORD-ID.
               15  CBETH-MM                             PIC X(02).
               15  CBETH-DD                             PIC X(02).
               15  CBETH-YYYY                           PIC X(04).
           05  CBETH-FIELDS-NF.
               10  CBETH-DATE                           PIC X(10).
               10  CBETH-INA                            PIC 9(06).
               10  HISPANIC-OR-LATINO                   PIC 9(06).
               10  NOT-HISPANIC-OR-LATINO               PIC 9(06).
           05  FILLER                                   PIC X(102).
      *
         01  CBIND-FIELDS.
           05  CBIND-RECORD-ID.
               10  CBIND-MM                             PIC X(02).
               10  CBIND-DD                             PIC X(02).
               10  CBIND-YYYY                           PIC X(04).
           05  CBIND-FIELDS-NF.
               10  CBIND-DATE                           PIC X(10).
               10  CBIND-INA                            PIC 9(06).
               10  WHOLESALE-TRADE                      PIC 9(06).
               10  TRANSPORTATION-WAREHOUSE             PIC 9(06).
               10  CONSTRUCTION                         PIC 9(06).
               10  FINANCE-INSURANCE                    PIC 9(06).
               10  MANUFACTURING                        PIC 9(06).
               10  AGR-FORESTRY-FISHING-HUNTING         PIC 9(06).
               10  PUBLIC-ADMINISTRATION                PIC 9(06).
               10  UTILITIES                            PIC 9(06).
               10  ACCOMODATION-FOODSERVICES            PIC 9(06).
               10  INFORMATION                          PIC 9(06).
               10  PROF-SCI-TECHSERVICES                PIC 9(06).
               10  REALESTATE-RENTAL-LEASING            PIC 9(06).
               10  OTHER-SERVICES-EXC-PUB-ADM           PIC 9(06).
               10  MGT-OF-COMPANIES-ENTERPRISES         PIC 9(06).
               10  EDUCATIONAL-SERVICES                 PIC 9(06).
               10  MINING                               PIC 9(06).
               10  HEALTHCARE-SOCIALASSISTANCE          PIC 9(06).
               10  ARTS-ENTERTAINMENT-RECREATION        PIC 9(06).
               10  ADM-SUP-WASTEMGMT-REMEDIASERV        PIC 9(06).
               10  RETAIL-TRADE                         PIC 9(06).
      *
       01  CBRAC-FIELDS.
           05  CBRAC-RECORD-ID.
               15  CBRAC-MM                             PIC X(02).
               15  CBRAC-DD                             PIC X(02).
               15  CBRAC-YYYY                           PIC X(04).
           05  CBRAC-FIELDS-NF.
               10  CBRAC-DATE                           PIC X(10).
               10  CBRAC-INA                            PIC 9(06).
               10  WHITE                                PIC 9(06).
               10  ASIAN                                PIC 9(06).
               10  BLACK-OR-AFRAM                       PIC 9(06).
               10  AM-INDIAN-OR-AL-NATIVE               PIC 9(06).
               10  NAT-HAWAIIAN-OR-PAC-ISLANDER         PIC 9(06).
           05  FILLER                                   PIC X(90).
      *
       01  CBGEN-FIELDS.
           05  CBGEN-RECORD-ID.
               15  CBGEN-MM                             PIC X(02).
               15  CBGEN-DD                             PIC X(02).
               15  CBGEN-YYYY                           PIC X(04).
           05  CBGEN-FIELDS-NF.
               10  CBGEN-DATE                           PIC X(10).
               10  CBGEN-INA                            PIC 9(06).
               10  FEMALE                               PIC 9(06).
               10  MALE                                 PIC 9(06).
           05  FILLER                                   PIC X(108).
      *------------------
       PROCEDURE DIVISION.
      *------------------
       OPEN-FILES.
           OPEN INPUT  CLAIMS-BY-AGE.
           OPEN INPUT  CLAIMS-BY-ETHNICITY.
           OPEN INPUT  CLAIMS-BY-INDUSTRY.
           OPEN INPUT  CLAIMS-BY-RACE.
           OPEN INPUT  CLAIMS-BY-GENDER.
           OPEN OUTPUT PRINT-LINE.
           OPEN OUTPUT CLAIMS-DATABASE.
           DISPLAY 'INITIAL OPEN'.
           DISPLAY WS-CLMDB-STATUS.
           IF WS-CLMDB-SUCCESS
              DISPLAY "FILE OPEN SUCCESSFUL"
           ELSE
              DISPLAY "FILE OPENING ERROR"
              GO TO CLOSE-STOP
           END-IF.
      *
       READ-NEXT-RECORD.
           PERFORM READ-CBAGE-RECORD
            PERFORM UNTIL LASTREC = 'Y'
            PERFORM WRITE-CBAGE-RECORD
            PERFORM READ-CBAGE-RECORD
            END-PERFORM
           .
      *
           MOVE DIVIDER TO PRINT-REC.
           WRITE PRINT-REC.
           MOVE 'N' TO LASTREC.
      *
           DISPLAY 'SET UP CLAIMS DB FOR I-O'
           CLOSE CLAIMS-DATABASE.
           OPEN I-O CLAIMS-DATABASE.
           DISPLAY 'SUBSEQUENT OPEN'.
           DISPLAY WS-CLMDB-STATUS.
           IF WS-CLMDB-SUCCESS
              DISPLAY "FILE OPEN SUCCESSFUL"
           ELSE
              DISPLAY "FILE OPENING ERROR"
              GO TO CLOSE-STOP
           END-IF.
      *
           PERFORM READ-CBETH-RECORD
            PERFORM UNTIL LASTREC = 'Y'
            PERFORM WRITE-CBETH-RECORD
            PERFORM READ-CBETH-RECORD
            END-PERFORM
           .
      *
           MOVE DIVIDER TO PRINT-REC.
           WRITE PRINT-REC.
           MOVE 'N' TO LASTREC.
      *
           PERFORM READ-CBIND-RECORD
            PERFORM UNTIL LASTREC = 'Y'
            PERFORM WRITE-CBIND-RECORD
            PERFORM READ-CBIND-RECORD
            END-PERFORM
           .
      *
           MOVE DIVIDER TO PRINT-REC.
           WRITE PRINT-REC.
           MOVE 'N' TO LASTREC.
      *
           PERFORM READ-CBRAC-RECORD
            PERFORM UNTIL LASTREC = 'Y'
            PERFORM WRITE-CBRAC-RECORD
            PERFORM READ-CBRAC-RECORD
            END-PERFORM
           .
      *
           MOVE DIVIDER TO PRINT-REC.
           WRITE PRINT-REC.
           MOVE 'N' TO LASTREC.
      *
           PERFORM READ-CBGEN-RECORD
            PERFORM UNTIL LASTREC = 'Y'
            PERFORM WRITE-CBGEN-RECORD
            PERFORM READ-CBGEN-RECORD
            END-PERFORM
           .
      *
       CLOSE-STOP.
           CLOSE CLAIMS-BY-AGE.
           CLOSE CLAIMS-BY-ETHNICITY.
           CLOSE CLAIMS-BY-INDUSTRY.
           CLOSE CLAIMS-BY-RACE.
           CLOSE CLAIMS-BY-GENDER.
           CLOSE PRINT-LINE.
           CLOSE CLAIMS-DATABASE.
           STOP RUN.
      *
       READ-CBAGE-RECORD.
           READ CLAIMS-BY-AGE
           AT END MOVE 'Y' TO LASTREC
           NOT AT END
             UNSTRING CBAGE-REC DELIMITED BY ',' OR ' '
               INTO CBAGE-RECORD-ID
                    CBAGE-DATE
                    CBAGE-INA
                    LESS-THAN-22
                    22-TO-24
                    25-TO-34
                    35-TO-44
                    45-TO-54
                    55-TO-59
                    60-TO-64
                    GRTR-THAN-64
             END-UNSTRING
           END-READ.
      *
       WRITE-CBAGE-RECORD.
           MOVE CBAGE-FIELDS  TO  PRINT-REC.
           WRITE PRINT-REC.
           STRING CBAGE-YYYY DELIMITED BY SIZE
                  CBAGE-MM DELIMITED BY SIZE
                  CBAGE-DD DELIMITED BY SIZE
                  INTO RECORD-KEY.
           MOVE CBAGE-FIELDS-NF TO CLMDB-CBAGE-FIELDS.
           WRITE CLAIMSDB-RECORD.
      *
       READ-CBETH-RECORD.
           READ CLAIMS-BY-ETHNICITY
           AT END MOVE 'Y' TO LASTREC
           NOT AT END
             UNSTRING CBETH-REC DELIMITED BY ',' OR ' '
               INTO CBETH-RECORD-ID
                    CBETH-DATE
                    CBETH-INA
                    HISPANIC-OR-LATINO
                    NOT-HISPANIC-OR-LATINO
             END-UNSTRING
           END-READ.
      *
       WRITE-CBETH-RECORD.
           MOVE CBETH-FIELDS        TO  PRINT-REC.
           WRITE PRINT-REC.
           STRING CBETH-YYYY DELIMITED BY SIZE
                  CBETH-MM DELIMITED BY SIZE
                  CBETH-DD DELIMITED BY SIZE
                  INTO RECORD-KEY.
           READ CLAIMS-DATABASE.
           MOVE  CBETH-FIELDS-NF TO CLMDB-CBETH-FIELDS.
           REWRITE CLAIMSDB-RECORD.
      *
       READ-CBIND-RECORD.
           READ CLAIMS-BY-INDUSTRY
           AT END MOVE 'Y' TO LASTREC
           NOT AT END
             UNSTRING CBIND-REC DELIMITED BY ',' OR ' '
               INTO CBIND-RECORD-ID
                    CBIND-DATE
                    CBIND-INA
                    WHOLESALE-TRADE
                    TRANSPORTATION-WAREHOUSE
                    CONSTRUCTION
                    FINANCE-INSURANCE
                    MANUFACTURING
                    AGR-FORESTRY-FISHING-HUNTING
                    PUBLIC-ADMINISTRATION
                    UTILITIES
                    ACCOMODATION-FOODSERVICES
                    INFORMATION
                    PROF-SCI-TECHSERVICES
                    REALESTATE-RENTAL-LEASING
                    OTHER-SERVICES-EXC-PUB-ADM
                    MGT-OF-COMPANIES-ENTERPRISES
                    EDUCATIONAL-SERVICES
                    MINING
                    HEALTHCARE-SOCIALASSISTANCE
                    ARTS-ENTERTAINMENT-RECREATION
                    ADM-SUP-WASTEMGMT-REMEDIASERV
                    RETAIL-TRADE
             END-UNSTRING
           END-READ.
      *
       WRITE-CBIND-RECORD.
           MOVE CBIND-FIELDS-NF     TO  PRINT-REC.
           WRITE PRINT-REC.
           STRING CBIND-YYYY DELIMITED BY SIZE
                  CBIND-MM DELIMITED BY SIZE
                  CBIND-DD DELIMITED BY SIZE
                  INTO RECORD-KEY.
           READ CLAIMS-DATABASE.
           MOVE  CBIND-FIELDS-NF TO CLMDB-CBIND-FIELDS.
           REWRITE CLAIMSDB-RECORD.
      *
       READ-CBRAC-RECORD.
           READ CLAIMS-BY-RACE
           AT END MOVE 'Y' TO LASTREC
           NOT AT END
             UNSTRING CBRAC-REC DELIMITED BY ',' OR ' '
               INTO CBRAC-RECORD-ID
                    CBRAC-DATE
                    CBRAC-INA
                    WHITE
                    ASIAN
                    BLACK-OR-AFRAM
                    AM-INDIAN-OR-AL-NATIVE
                    NAT-HAWAIIAN-OR-PAC-ISLANDER
             END-UNSTRING
           END-READ.
      *
       WRITE-CBRAC-RECORD.
           MOVE CBRAC-FIELDS        TO  PRINT-REC.
           WRITE PRINT-REC.
           STRING CBRAC-YYYY DELIMITED BY SIZE
                  CBRAC-MM DELIMITED BY SIZE
                  CBRAC-DD DELIMITED BY SIZE
                  INTO RECORD-KEY.
           READ CLAIMS-DATABASE.
           MOVE  CBRAC-FIELDS-NF TO CLMDB-CBRAC-FIELDS.
           REWRITE CLAIMSDB-RECORD.
      *
       READ-CBGEN-RECORD.
           READ CLAIMS-BY-GENDER
           AT END MOVE 'Y' TO LASTREC
           NOT AT END
             UNSTRING CBGEN-REC DELIMITED BY ',' OR ' '
               INTO CBGEN-RECORD-ID
                    CBGEN-DATE
                    CBGEN-INA
                    FEMALE
                    MALE
             END-UNSTRING
           END-READ.
      *
       WRITE-CBGEN-RECORD.
           MOVE CBGEN-FIELDS        TO  PRINT-REC.
           WRITE PRINT-REC.
           STRING CBGEN-YYYY DELIMITED BY SIZE
                  CBGEN-MM DELIMITED BY SIZE
                  CBGEN-DD DELIMITED BY SIZE
                  INTO RECORD-KEY.
           READ CLAIMS-DATABASE.
           MOVE  CBGEN-FIELDS-NF TO CLMDB-CBGEN-FIELDS.
           REWRITE CLAIMSDB-RECORD.
      *