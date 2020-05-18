//UNEMP JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(UNEMP),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(UNEMP),DISP=SHR
//***************************************************/
// IF RC = 0 THEN
//***************************************************/
//RUN      EXEC PGM=UNEMP
//STEPLIB  DD DSN=&SYSUID..LOAD,DISP=SHR
//CLMAGE   DD DSN=&SYSUID..CBAGE,DISP=SHR
//CLMETH   DD DSN=&SYSUID..CBETH,DISP=SHR
//CLMIND   DD DSN=&SYSUID..CBIND,DISP=SHR
//CLMRAC   DD DSN=&SYSUID..CBRAC,DISP=SHR
//CLMGEN   DD DSN=&SYSUID..CBGEN,DISP=SHR
//CLMSDB   DD DSN=&SYSUID..CLMDB,DISP=SHR
//PRTLINE   DD SYSOUT=*,OUTLIM=15000
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF