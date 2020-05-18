//UNEMPDB JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(UNEMPDB),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(UNEMPDB),DISP=SHR
//***************************************************/
// IF RC = 0 THEN
//***************************************************/
//RUN      EXEC PGM=UNEMPDB
//STEPLIB  DD DSN=&SYSUID..LOAD,DISP=SHR
//CLMSDB   DD DSN=&SYSUID..CLMDB,DISP=SHR
//PRTLINE   DD SYSOUT=*,OUTLIM=15000
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF