//UNMPEXEC JOB  1,NOTIFY=&SYSUID,RESTART=RUN
//***************************************************/
//*     COMPILE RTALLREC
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(RTALLREC),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(RTALLREC),DISP=SHR
//***************************************************/
//*     COMPILE RTONEREC
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(RTONEREC),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(RTONEREC),DISP=SHR
//***************************************************/
//*     COMPILE UNEMPSB
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(UNEMPSB),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(UNEMPSB),DISP=SHR
//***************************************************/
//*     COMPILE UNEMPMN
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(UNEMPMN),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(UNEMPMN),DISP=SHR
//***************************************************/
// IF RC = 0 THEN
//***************************************************/
//*     RUN UNEMPMN
//***************************************************/
//RUN       EXEC PGM=UNEMPMN
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//CLMSDB    DD DSN=&SYSUID..CLMDB,DISP=SHR
//USRINP    DD DSN=&SYSUID..UNEMPIN,DISP=SHR
//PGMOUT    DD DSN=&SYSUID..UNEMPOUT,DISP=SHR
//PRTLINE   DD SYSOUT=*,OUTLIM=15000
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF