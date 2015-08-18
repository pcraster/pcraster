C     Last change:  ERB  15 Jan 2003   12:59 pm
C=======================================================================
      SUBROUTINE SEN1BAS6DF(ISENALL,ISEN,IPRINTS,IUSEN,LCB1,LCLN,LCSV,
     &                     NPE,NPLIST,RCLOSE,IUHEAD,MXSEN,LCSNEW,IOUT,
     &                     LCBSCA,LCISEN)
C     VERSION 19990618 ERB
C     ******************************************************************
C     INITIALIZE VARIABLES FOR SENSITIVITY PROCESS
C     ******************************************************************
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER ISENALL, ISEN, IPRINTS, IUSEN, LCB1, LCLN, LCSV, NPE,
     &        NPLIST
      REAL RCLOSE
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
C
C-----INITIALIZE GLOBAL VARIABLES THAT BELONG PRIMARILY TO THE
C     SENSITIVITY PROCESS
      ISENALL = 0
      ISEN = 0
      IF (IUSEN.GT.0) ISEN = 1
      IPRINTS = 0
      IUHEAD =0
      MXSEN = 0
      RCLOSE = 0.0
C
C-----INITIALIZE POINTERS FOR ARRAYS THAT MAY BE REFERENCED BUT MAY NOT
C     OTHERWISE GET ALLOCATED
      NPE = 1
      NPLIST= 1
      LCB1 = 1
      LCBSCA = 1
      LCISEN = 1
      LCLN = 1
      LCSNEW = 1
      LCSV = 1
C
C  Read ISENALL if SEN is active
      IF (IUSEN.GT.0) THEN
        CALL URDCOM(IUSEN,0,LINE)
        LLOC = 1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,N,DUM,IOUT,IUSEN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISENALL,DUM,IOUT,
     &             IUSEN)
        REWIND(IUSEN)
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE SEN1BAS6AL(ISUM,ISUMI,NCOL,NROW,NLAY,IOUT,IUHEAD,
     &                      NPLIST,IU,IPAR,LCHCLO,LCRCLO,LCLN,IPRINTS,
     &                      LCISEN,LCBU,LCBL,LCB1,ISENALL,IREWND,LCSNEW,
     &                      LCSOLD,ISUMGZ,ISEN,ISENSU,ISENPU,ISENFM,
     &                      IPES,MXSEN,LCBSCA,ITMXP,MAXUNIT,MINRSV,
     &                      MAXRSV,NSTP,NPER,NTIMES,LCSEND,LCSNDT)
C     VERSION 19990402 ERB
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR SENSITIVITY PROCESS
C     ******************************************************************
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER IOUT, IPAR, IPRINTS, ISOLD, ISUM, IU, IUHEAD,
     &        LCB1, LCBL, LCBU, LCHCLO, LCRCLO, LCISEN,
     &        LCLN, NCOL, NLAY, NODES, NPLIST, NROW
      DIMENSION NSTP(NPER)
      CHARACTER*200 LINE
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
C
C1------IDENTIFY PROCESS AND PRINT HEADING
      IREWND=0
      WRITE (IOUT,500) IU
  500 FORMAT (/,' SEN1BAS6 -- SENSITIVITY PROCESS, ',
     &        'VERSION 1.0, 10/15/98',/,' INPUT READ FROM UNIT ',I4)
C
C-------READ & PRINT ITEM 1 OF THE SEN INPUT FILE
      CALL URDCOM(IU,IOUT,LINE)
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPLIST,DUM,IOUT,IU)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISENALL,DUM,IOUT,IU)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUHEAD,DUM,IOUT,IU)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXSEN,DUM,IOUT,IU)
      WRITE (IOUT,505) NPLIST
  505 FORMAT (/,
     &   ' NUMBER OF PARAMETER VALUES TO BE READ FROM SEN FILE:',I5)
      IF (NPLIST.LE.0) THEN
        WRITE (IOUT,510)
  510   FORMAT(' ERROR: NPLIST MUST BE > 0 -- STOP EXECUTION',
     &         ' (SEN1BAS6AL)')
        CALL USTOP(' ')
      ENDIF
C
      WRITE (IOUT,560) ISENALL
  560 FORMAT (' ISENALL............................................:',
     &        I5)
      IF (ISENALL.GT.0 .AND. IPAR.GT.0) THEN
        IPAR = 0
        WRITE(IOUT,570)
  570   FORMAT(1X,'SENSITIVITIES WILL BE CALCULATED FOR ALL',
     &         ' PARAMETERS LISTED IN SEN FILE')
        IPES = 0
        ITMXP = 1
        WRITE(IOUT,575)
  575   FORMAT(' PARAMETER-ESTIMATION PROCESS HAS BEEN DEACTIVATED',
     &         ' BECAUSE ISENALL>0')
      ELSEIF (ISENALL.LT.0) THEN
        IF (IPAR.GE.0) IPAR = -1
        IF (IPAR.EQ.-2) IPAR = -3
        ISEN = 0
        WRITE(IOUT,580)
  580   FORMAT(
     &  ' SENSITIVITY PROCESS HAS BEEN DEACTIVATED BECAUSE ISENALL<0')
        IF (IPES.NE.0) THEN
          IPES = 0
          ITMXP = 1
          WRITE(IOUT,590)
  590     FORMAT(' PARAMETER-ESTIMATION PROCESS HAS BEEN DEACTIVATED',
     &           ' BECAUSE ISENALL<0')
        ENDIF
      ENDIF
C
      IF (ISEN.NE.0) THEN
        IF (IUHEAD.GT.0) THEN
          WRITE (IOUT,600)
  600     FORMAT(' SENSITIVITIES WILL BE STORED IN SCRATCH FILES')
        ELSE
          WRITE (IOUT,620)
  620     FORMAT(' SENSITIVITIES WILL BE STORED IN MEMORY')
        ENDIF
        WRITE (IOUT,630) MXSEN
  630   FORMAT(' FOR UP TO ',I3,' PARAMETERS')
        IF (ISENALL.GT.0) THEN
          IF (NPLIST.GT.MXSEN) THEN
            WRITE (IOUT,640) NPLIST,MXSEN
  640       FORMAT(/,
     &' NPLIST = ',I4,' AND MXSEN = ',I4,', BUT MXSEN MUST BE',/,
     &' GREATER THAN OR EQUAL TO NPLIST WHEN ISENALL > 0',/,
     &' -- STOP EXECUTION (SEN1BAS6AL)')
            CALL USTOP(' ')
          ENDIF
        ENDIF
      ENDIF
C
C     READ ITEM 2: CONTROLS ON PRINTING/SAVING OF SENSITIVITY ARRAYS
      READ (IU,*) IPRINTS, ISENSU, ISENPU, ISENFM
C
      IF (ISEN.NE.0) THEN
        IF (IPAR.NE.1) THEN
          IF (ISENSU.GT.0 .OR. ISENPU.GT.0) THEN
            IF (IPRINTS.EQ.0) THEN
              WRITE (IOUT,730)
            ELSEIF (IPRINTS.EQ.1) THEN
              WRITE (IOUT,740)
            ELSE
              WRITE (IOUT,750)
              CALL USTOP(' ')
            ENDIF
          ELSE
            WRITE (IOUT,660)
          ENDIF
  660 FORMAT (1X,'SENSITIVITY ARRAYS WILL NOT BE PRINTED OR SAVED')
  730 FORMAT (1X,'OUTPUT CONTROL FILE WILL CONTROL PRINTING/SAVING',
     &' OF SENSITIVITY ARRAYS')
  740 FORMAT (1X,'SENSITIVITY ARRAYS WILL BE PRINTED AND/OR SAVED FOR',
     &' ALL TIME STEPS')
  750 FORMAT (1X,'ERROR: IPRINTS MUST BE EITHER 0 OR 1',
     &        '   -- STOP EXECUTION (SEN1BAS6AL)')
C
          IF (ISENSU.GT.0) WRITE (IOUT,760) ISENSU
  760 FORMAT(1X,'SENSITIVITY ARRAYS WILL BE SAVED TO UNIT ',I4)
          IF (ISENPU.GT.0) WRITE (IOUT,770) ISENPU, ISENFM
  770 FORMAT(1X,'SENSITIVITY ARRAYS WILL BE PRINTED TO UNIT ',I4,
     &       ' USING FORMAT CODE ',I3)
  850 FORMAT(/,1X,57('*'),/,
     &       6X,'USE OF THE ',A3,' PACKAGE IS INCOMPATIBLE WITH THE',/,
     &       6X,'SENSITIVITY PROCESS',/,
     &       6X,'-- STOP EXECUTION',/,1X,57('*'))
        END IF
      ENDIF
C
C     COUNT TIME STEPS IN SIMULATION
      NTIMES = 0
      DO 20 I = 1,NPER
        NTIMES = NTIMES + NSTP(I)
   20 CONTINUE
C
C-------STORE, IN ISOLD, LOCATION OF FIRST UNALLOCATED SPACE IN X.
      ISOLD = ISUM
      ISOLDZ = ISUMGZ
      ISOLDI = ISUMI
C
C2------ALLOCATE SPACE FOR ARRAYS
C
C----------FULL GRID ARRAYS
      NODES = NROW*NCOL*NLAY
      LCSNEW = ISUMGZ
      ISUMGZ = ISUMGZ + NODES
      LCSOLD = ISUM
      ISUM = ISUM + NODES
C----------ARRAYS FOR PARAMETER DEFINITION
      LCLN = ISUMI
      ISUMI = ISUMI + NPLIST
      LCB1 = ISUM
      ISUM = ISUM + NPLIST
      LCISEN = ISUMI
      ISUMI = ISUMI + NPLIST
      LCBU = ISUM
      ISUM = ISUM + NPLIST
      LCBL = ISUM
      ISUM = ISUM + NPLIST
      LCBSCA = ISUM
      ISUM = ISUM + NPLIST
C----------MISCELLANEOUS ARRAYS
      LCHCLO = ISUM
      ISUM = ISUM + NPLIST
      LCRCLO = ISUM
      ISUM = ISUM + NPLIST
      LCSEND = ISUM
      ISUM = ISUM + MXSEN*NTIMES
      LCSNDT = ISUM
      ISUM = ISUM + NTIMES
C
C-------OPEN FILES FOR SAVING SENSITIVITY ARRAYS
      IERR = 0
      IF (IUHEAD.GT.0) THEN
        MAXS = IUHEAD + MXSEN - 1
        IF (IUHEAD.GT.MAXUNIT) THEN
          MAXUNIT = MAXS
        ELSE
          IF (MAXS.GE.MINRSV) THEN
            WRITE(IOUT,860)MINRSV,MAXRSV,IUHEAD,MXSEN
            CALL USTOP(' ')
          ENDIF
        ENDIF
  860 FORMAT(' ERROR: UNIT-NUMBER CONFLICT FOR SCRATCH FILES.',/,
     &' UNIT NUMBERS ',I3,' - ',I3,' ARE RESERVED, BUT IUHEAD = ',I3,
     &' AND MXSEN = ',I3,/,' STOP EXECUTION (SEN1BAS6AL)')
        DO 880 I = 1, MXSEN
          IUNIT4 = IUHEAD - 1 + I
          OPEN (UNIT=IUNIT4,STATUS='SCRATCH',FORM='UNFORMATTED',
     &          ERR=890)
  880   CONTINUE
        GOTO 900
  890   CONTINUE
        IERR = 1
  900   CONTINUE
      ENDIF
      IF (IERR.NE.0) THEN
        WRITE(IOUT,895) IUNIT4,MXSEN,IUHEAD
  895   FORMAT(' ERROR IN OPENING SCRATCH FILE',I5,/,' MAKE SURE',I5,
     &  ' FORTRAN UNIT NUMBERS INCLUDING AND AFTER IUHEAD, WHICH',/,
     &  ' EQUALS',I3,', ARE UNSPECIFIED.  (SEN1BAS6AL)')
        CALL USTOP(' ')
      ENDIF
C
C     WRITE MESSAGE SHOWING AMOUNT ALLOCATED FOR SEN PROCESS
      IUX = ISUM - ISOLD
      WRITE (IOUT,515) IUX
  515 FORMAT (/1X,I10,' ELEMENTS IN X ARRAY ARE USED FOR SENSITIVITIES')
      IUZ = ISUMGZ - ISOLDZ
      WRITE (IOUT,520) IUZ
  520 FORMAT (1X,I10,' ELEMENTS IN Z ARRAY ARE USED FOR SENSITIVITIES')
      IUI = ISUMI - ISOLDI
      WRITE (IOUT,525) IUI
  525 FORMAT (1X,I10,' ELEMENTS IN IX ARRAY ARE USED FOR SENSITIVITIES')
C
C9------RETURN
      RETURN
      END
C=======================================================================
      SUBROUTINE SEN1BAS6RP(BL,BU,FAC,ISENS,IOUT,IU,LN,NPE,NPLIST,
     &                      DETWTP,ISENALL,BSCAL,MXSEN)
C     VERSION 20010924 ERB
C     ******************************************************************
C     READ LIST OF PARAMETER INFORMATION
C     ******************************************************************
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER IERR, ISENS, LN, NPLIST
      REAL BL, BU, FAC
      DIMENSION BL(NPLIST), BU(NPLIST), ISENS(NPLIST), LN(NPLIST),
     &          BSCAL(NPLIST)
      CHARACTER*10 PNI, PNJ
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
  500 FORMAT (' PARAMETER "',A10,
     &        '" IS LISTED MORE THAN ONCE IN SEN FILE',/,
     &        ' -- STOP EXECUTION (SEN1BAS6RP)')
  505 FORMAT (16I5)
  510 FORMAT (//,' DEFINITION OF TEMPORAL AND SPATIAL EXTENT OF ',
     &        'PARAMETERS (PARAMETERIZATION)')
  520 FORMAT (/,' INFORMATION ON PARAMETERS LISTED IN SEN FILE',/,
     &   41X,'LOWER',9X,'UPPER',7X,'ALTERNATE',/
     &   24X,'VALUE IN SEN   REASONABLE    REASONABLE     SCALING',/,
     &   '    NAME     ISENS  LN   INPUT FILE      LIMIT  ',
     &   '       LIMIT         FACTOR',/,
     &   ' ----------  -----  --  ------------  ------------',
     &   '  ------------  ------------')
  530 FORMAT (/,
     &' NUMBERS INDICATING PARAMETER TYPES FOR WHICH INTERPOLATION IS AC
     &COMPLISHED USING LOG-TRANSFORMED VALUES, WHERE 1 INDICATES T, 2 KV
     &, 3 S1, AND 4 S2:',/,'   ',9I5)
  545 FORMAT(/,' SEN FILE CONTAINS NO PARAMETERS FOR',
     &         ' WHICH ISENS > 0',/,' -- STOP EXECUTION (SEN1BAS6RP)')
  570 FORMAT(1X,A10,3X,I2,4X,I2,4(2X,G12.5))
  580 FORMAT (/,
     &   'ESTIMATED ERROR VARIANCE <= 0 -- STOP EXECUTION (SEN1BAS6RP)')
  590 FORMAT(' ERROR ENCOUNTERED IN READING SEN PROCESS INPUT FILE',/,
     &       ' -- STOP EXECUTION (SEN1BAS6RP)')
  600 FORMAT(' BSCAL MUST BE > 0.0 -- STOP EXECUTION (SEN1BAS6RP)')
  620 FORMAT(1X,77('-'))
  630 FORMAT(' FOR THE PARAMETERS LISTED IN THE TABLE ABOVE,',
     &       ' PARAMETER VALUES IN INDIVIDUAL',/,
     &       ' PACKAGE INPUT FILES ARE REPLACED BY THE VALUES FROM',
     &       ' THE SEN INPUT FILE.  THE',/,
     &       ' ALTERNATE SCALING FACTOR IS USED TO SCALE SENSITIVITIES',
     &       ' IF IT IS LARGER THAN',/,
     &       ' THE PARAMETER VALUE IN ABSOLUTE VALUE AND THE PARAMETER',
     &       ' IS NOT LOG-TRANSFORMED.')
  660 FORMAT(/,' BECAUSE ISENALL > 0, ALL ISENS ARE SET TO 1')
  661 FORMAT(/,' BECAUSE ISENALL < 0, ALL ISENS ARE SET TO 0')
  662 FORMAT(/,' ISENS IS GREATER THAN ZERO FOR ',I4,' PARAMETERS')
  663 FORMAT(/,' ISENS IS GREATER THAN ZERO FOR ',I1,' PARAMETER')
  670 FORMAT(/,
     &' ERROR: MXSEN, WHICH IS ',I4,', MUST BE INCREASED TO AT LEAST ',
     &I4,' TO ACCOMODATE',/,
     &' ALL PARAMETERS FOR WHICH ISENS > 0 -- STOP EXECUTION',
     &' (SEN1BAS6RP)')
  680 FORMAT(/,
     &' ERROR FOUND IN SENSITIVITY PROCESS INPUT FILE.  SEARCH ABOVE',/,
     &' FOR "STOP EXECUTION" (SEN1BAS6RP)')
C     ------------------------------------------------------------------
C
C-----INITIALIZE VARIABLES
      FAC = 1.0
      IERR = 0
      DETWTP = 0.0
      NPE = 0
C-----READ ITEM 3:  LIST OF PARAMETER INFORMATION
      IF (NPLIST.GT.0) THEN
        DO 50 I=1,NPLIST
          LN(I) = 0
   50   CONTINUE
        WRITE (IOUT,520)
        DO 70 I=1,NPLIST
          READ(IU,*,ERR=80) PARNAM(I),ISENS(I),LN(I),B(I),
     &                      BL(I),BU(I),BSCAL(I)
          WRITE(IOUT,570) PARNAM(I),ISENS(I),LN(I),B(I),BL(I),
     &                    BU(I),BSCAL(I)
          IACTIVE(I) = -99
          IF (BSCAL(I).LE.0.0) THEN
            WRITE(IOUT,600)
            IERR = 1
          ENDIF
          IF (ISENALL.GT.0) THEN
            ISENS(I) = 1
          ELSEIF (ISENALL.LT.0) THEN
            ISENS(I) = 0
          ENDIF
C---------CHECK FOR DUPLICATE PARAMETER NAME
          IF (I.GT.1) THEN
            CALL UCASE(PARNAM(I),PNI,1)
            IM1 = I-1
            DO 60 J=1,IM1
              CALL UCASE(PARNAM(J),PNJ,1)
              IF (PNI.EQ.PNJ) THEN
                WRITE(IOUT,500) PARNAM(I)
                IERR = 1
              ENDIF
 60         CONTINUE
          ENDIF
          IF (ISENS(I).GT.0) THEN
            NPE = NPE+1
            IPPTR(NPE) = I
          ENDIF
 70     CONTINUE
        WRITE (IOUT,620)
        WRITE (IOUT,630)
        IF (ISENALL.GT.0) THEN
          WRITE (IOUT,660)
        ELSEIF (ISENALL.LT.0) THEN
          WRITE (IOUT,661)
        ELSE
          IF (NPE.EQ.1) THEN
            WRITE (IOUT,663) NPE
          ELSE
            WRITE (IOUT,662) NPE
          ENDIF
        ENDIF
        GOTO 90
 80     CONTINUE
        WRITE(IOUT,590)
        IERR = 1
 90     CONTINUE
      ENDIF
C
      IPSUM=NPLIST
C
      IF (ISENALL.GE.0) THEN
C
C-------ARE THERE STILL PARAMETERS TO ESTIMATE?
        IF (NPE.EQ.0) THEN
          WRITE (IOUT,545)
          IERR = 1
        ENDIF
C
      ENDIF
C
      IF (NPE.GT.MXSEN) THEN
        WRITE(IOUT,670) MXSEN,NPE
        IERR = 1
      ENDIF
C
      IF (IERR.GT.0) THEN
        WRITE(IOUT,680)
        CALL USTOP(' ')
      ENDIF
C
      CLOSE(UNIT=IU)
      RETURN
      END
C=======================================================================
      SUBROUTINE SEN1BAS6CM(JT,IOUT,LN,B1,IERR,NPER,HCLO,RCLO,HCLOSE,
     &                      RCLOSE,IPAR,NPE,NPLIST,ISENS,NSTP,PERLEN,
     &                      TSMULT,IUD4,NOTICECOUNT)
C     VERSION 19990326 ERB
C     ******************************************************************
C     CHECK AND STORE DATA FOR SENSITIVITY CALCULATIONS AND
C     PARAMETER ESTIMATION, AND INITIALIZE SOME VARIABLES.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL B1, BB, HCLO, PERLEN, RCLO, TSMULT
      INTEGER I, IERR, IOUT, IP, JT, LN
      INTEGER ISENS(NPLIST), NSTP(NPER)
      DIMENSION B1(NPLIST), LN(NPLIST), PERLEN(NPER),
     &          TSMULT(NPER), HCLO(NPLIST), RCLO(NPLIST)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
  540 FORMAT (//,' EXECUTION TERMINATED DUE TO ERRORS IN',
     &        ' INPUT FILE(S).',/,
     &        ' SEARCH ABOVE FOR THE PHRASE "STOP EXECUTION" TO FIND',
     &        ' ERROR MESSAGES (SEN1BAS6MS)')
  610 FORMAT(/,' CONVERGENCE CRITERIA FOR SENSITIVITIES')
  620 FORMAT(
     &  1X,'PARAMETER      HCLOSE        RCLOSE',/
     &  1X,'----------  ------------  ------------')
  630 FORMAT(
     &  1X,'PARAMETER      HCLOSE',/
     &  1X,'----------  ------------')
  640 FORMAT(1X,A10,2X,G12.5,2X,G12.5)
  650 FORMAT(1X,38('-'))
  655 FORMAT(1X,24('-'))
  660 FORMAT(/,
     &1X,'*** NOTICE *** THE NUMBER OF STRESS PERIODS AND TIME STEPS ',
     &'DEFINED IN THE',/,
     &1X,'DISCRETIZATION INPUT FILE ARE NOT ALL NEEDED TO PROVIDE ',
     &'SIMULATED EQUIVALENTS',/,
     &1X,'TO THE OBSERVATIONS. TO REDUCE EXECUTION TIME, THE TIME ',
     &'REPRESENTED BY THE',/,
     &1X,'SIMULATION HAS BEEN REDUCED. THE REVISED FINAL STRESS ',
     &'PERIOD, NUMBER OF TIME',/,
     &1X,'STEPS, AND STRESS-PERIOD LENGTH ARE:',/,
     &3X,'STRESS PERIOD: ',I5,/,3X,
     &'NUMBER OF TIME STEPS: ',I5,/,3X,'STRESS PERIOD LENGTH: ',G12.5)
  670 FORMAT(' ERROR -- DUPLICATE OBSERVATION NAME: "',A,'"',
     &' -- STOP EXECUTION (SEN1BAS6MS)')
  680 FORMAT(' ERROR: OBSERVATIONS ARE REQUIRED -- STOP EXECUTION',
     &' (SEN1BAS6MS)')
  690 FORMAT(
     &' ERROR: AN OBSERVATION TIME IS LATER THAN THE END OF THE',/,
     &' FINAL TIME STEP -- STOP EXECUTION (SEN1BAS6MS)')
C
C------STOP IF THERE ARE ERRORS IN THE DATA SET
      IF (IERR.GT.0) THEN
        WRITE (IOUT,540)
        CALL USTOP(' ')
      ENDIF
C
C-------CALCULATE CONVERGENCE CRITERIA FOR SENS.-EQ. SENSITIVITIES
      DO 70 IP = 1, NPLIST
        HCLO(IP) = 0.
        RCLO(IP) = 0.
        IF (ISENS(IP).GT.0) THEN
          BB = ABS(B(IP))
          IF (BB.LT.1.E-15) BB = 1.0
          HCLO(IP) = HCLOSE/(BB*100.)
          RCLO(IP) = RCLOSE/(BB*100.)
        ENDIF
   70 CONTINUE
C
C-----PRINT INFORMATION ON PARAMETERS TO UNDERGO ESTIMATION OR
C     SENSITIVITY ANALYSIS
      IF (IPAR.GE.0 .OR. IPAR.EQ.-2) THEN
        WRITE(IOUT,610)
        IF (IUD4.EQ.0) THEN
          WRITE (IOUT,620)
        ELSE
          WRITE (IOUT,630)
        ENDIF
        DO 85 IP=1,NPE
          IIPP = IPPTR(IP)
          IF (IUD4.EQ.0) THEN
            WRITE (IOUT,640) PARNAM(IIPP),HCLO(IIPP),
     &                       RCLO(IIPP)
          ELSE
            WRITE (IOUT,640) PARNAM(IIPP),HCLO(IIPP)
          ENDIF
 85     CONTINUE
        IF (IUD4.EQ.0) THEN
          WRITE(IOUT,650)
        ELSE
          WRITE(IOUT,655)
        ENDIF
      ENDIF
C
C-------SAVE INITIAL PARAMETER VALUES AND
C-------CONVERT INDICATED PARAMETERS TO NATURAL LOGS
      DO 90 IP = 1, NPLIST
        B1(IP) = B(IP)
        IF (LN(IP).GT.0) B1(IP) = LOG(B(IP))
   90 CONTINUE
C
   95 CONTINUE
C
      IF (IPAR.LE.-2) RETURN
      IF (JT.EQ.0) THEN
        IF (IPAR.EQ.1) THEN
          WRITE(IOUT,680)
          CALL USTOP(' ')
        ELSE
          RETURN
        ENDIF
      ENDIF
C
C-----ENSURE THAT SIMULATION DOES NOT GO LONGER THAN NECESSARY
      KSTPE = 0
      ONE=1.
      DO 140 I = 1, NPER
        PTIM = 0.0
        DELT = PERLEN(I)/FLOAT(NSTP(I))
        IF(TSMULT(I).NE.ONE) DELT=PERLEN(I)*(ONE-TSMULT(I))/
     &                           (ONE-TSMULT(I)**NSTP(I))
        DO 130 J = 1, NSTP(I)
          KSTPE = KSTPE + 1
          PTIM = PTIM + DELT
          IF (KSTPE.EQ.JT) THEN
            NPERNEW = I
            NSTPNEW = J
            GOTO 150
          ENDIF
          DELT = DELT * TSMULT(I)
 130    CONTINUE
 140  CONTINUE
      WRITE(IOUT,690)
      CALL USTOP(' ')
 150  CONTINUE
C
      IF (NPERNEW.NE.NPER .OR. NSTPNEW.NE.NSTP(NPER)) THEN
        NPER = NPERNEW
        NSTP(NPER) = NSTPNEW
        PERLEN(NPER) = PTIM
        WRITE(IOUT,660)NPERNEW,NSTPNEW,PTIM
        NOTICECOUNT = NOTICECOUNT+1
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE SEN1BAS6ZS(IUHEAD,LENXHS,NCOL,NPE,NROW,NLAY,SNEW,SOLD,
     &                     XHS,SEND,NTIMES)
C     VERSION 19990326 ERB
C     ******************************************************************
C     SET SENSITIVITY ARRAYS TO ZERO AND STORE ON DISK OR IN MEMORY.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL SEND, SOLD
      DOUBLE PRECISION SNEW(NCOL*NROW*NLAY)
      DIMENSION SEND(NPE,NTIMES), SOLD(NCOL*NROW*NLAY), XHS(LENXHS)
C     ------------------------------------------------------------------
C
      NODES = NLAY*NROW*NCOL
      DO 20 J = 1,NTIMES
        DO 10 I = 1,NPE
          SEND(I,J) = 0.0
   10   CONTINUE
   20 CONTINUE
C-------INITIALIZE SNEW AND SOLD
      DO 30 I = 1,NODES
        SOLD(I) = 0.0
        SNEW(I) = 0.0
  30  CONTINUE
C
C-------SAVE INITIAL SENSITIVITIES ON DISK OR IN MEMORY
      IUTM = IUHEAD - 1
      IPTR = 1 - NODES
      DO 60 IP = 1,NPE
        IF (IUHEAD.GT.0) THEN
          IUTM = IUTM + 1
C         WRITE SOLD FOR ONE PARAMETER TO A SCRATCH FILE
          WRITE(IUTM) (SOLD(I),I=1,NODES)
          REWIND(IUTM)
        ELSE
          IPTR = IPTR + NODES
C         COPY SOLD FOR ONE PARAMETER TO MEMORY
          DO 50 I = 1,NODES
            XHS(IPTR-1+I) = SOLD(I)
   50     CONTINUE
        ENDIF
   60 CONTINUE
C
      RETURN
      END
C=======================================================================
      SUBROUTINE SEN1BAS6CP(IOUTG,NPLIST,ISENSU,CHEDFM)
C     VERSION 20010827 ERB
C     ******************************************************************
C     CHECK THAT PARAMETER DEFINITIONS ARE COMPLETE.  CHECK FOR FILE
C     OUTPUT/TYPE INCOMPATIBILITY
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*10 PNI, PNIP
      CHARACTER*11 FMT
      CHARACTER*20 CHEDFM
      LOGICAL LOP
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
  100 FORMAT(' DUPLICATE PARAMETER NAME: ',A10,/,
     &               ' -- STOP EXECUTION (SEN1BAS6CP)')
  110 FORMAT(' PARAMETER "',A10,'" HAS NOT BEEN DEFINED',/,
     &           ' -- STOP EXECUTION (SEN1BAS6CP)')
  120 FORMAT(/,
     &' *** ERROR: BINARY OUTPUT SPECIFIED FOR A FORMATTED FILE.',/,
     &'  CHECK ISENSU, CHEDFM, AND FILE TYPE -- STOP EXECUTION',
     &' (SEN1BAS6CP)')
  130 FORMAT(/,' *** ERROR: TEXT OUTPUT SPECIFIED FOR A BINARY FILE.',/,
     &'  CHECK ISENSU, CHEDFM, AND FILE TYPE -- STOP EXECUTION',
     &' (SEN1BAS6CP)')
  140 FORMAT(/,' *** WARNING: ISENSU>0, BUT UNIT ISENSU NOT OPENED IN',
     &' THE NAME FILE ***',/)
C
C-----CHECK THAT ALL PARAMETERS IN SEN FILE HAVE BEEN DEFINED
C     AND THAT THERE ARE NO DUPLICATE PARAMETER NAMES
      IERR = 0
      DO 90 IP = 1, NPLIST
        CALL UCASE(PARNAM(IP),PNIP,1)
        IF (IP.GT.1) THEN
          DO 30 I=1,IP-1
            CALL UCASE(PARNAM(I),PNI,1)
            IF (PNIP.EQ.PNI) THEN
              IERR = 1
              WRITE(IOUTG,100) PARNAM(IP)
            ENDIF
   30     CONTINUE
        ENDIF
        IF (PARTYP(IP).EQ.' ') THEN
          IERR = 1
          WRITE(IOUTG,110) PARNAM(IP)
        ENDIF
   90 CONTINUE
C
      IF (IERR.NE.0) CALL USTOP(' ')
      IDEFPAR=1
C
C     IF ISENSU>0, CHECK THAT FILE OUTPUT AND FILE TYPE ARE COMPATIBLE
      IF (ISENSU.GT.0) THEN
        INQUIRE(UNIT=ISENSU,OPENED=LOP,FORM=FMT)
        IF (LOP) THEN
          IF (FMT.EQ.'FORMATTED') THEN    ! FILE OPENED FOR TEXT I/O
            IF (CHEDFM.EQ.' ') THEN       ! OUTPUT IS BINARY
              WRITE(IOUTG,120)
              CALL USTOP(' ')
            ENDIF
          ELSE                            ! FILE OPENED FOR BINARY I/O
            IF (CHEDFM.NE.' ') THEN       ! OUTPUT IS TEXT
              WRITE(IOUTG,130)
              CALL USTOP(' ')
            ENDIF
          ENDIF
        ELSE
          WRITE(IOUTG,140)
        ENDIF
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE SEN1BAS6CC(HCLO,RCLO,FAC,HCLOSES,IP,NPLIST,RCLOSES,
     &                     IIPP,PIDTMP,NCOL,NROW,NLAY,IUHEAD,SNEW,SOLD,
     &                     XHS,LENXHS)
C     VERSION 19990623 ERB
C     ******************************************************************
C     SET HCLOSES AND RCLOSES TO APPROPRIATE CONVERGENCE CRITERIA, WHICH
C     HAVE BEEN PREVIOUSLY DEFINED.  ASSIGN IIPP AND PIDTMP
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER IP, IIPP
      REAL FAC, HCLO, HCLOSES, RCLO, RCLOSES, SOLD, XHS
      DOUBLE PRECISION SNEW
      DIMENSION HCLO(NPLIST), RCLO(NPLIST), SNEW(NCOL*NROW*NLAY),
     &          SOLD(NCOL*NROW*NLAY), XHS(LENXHS)
      CHARACTER*4 PIDTMP
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
C
      IIPP = IPPTR(IP)
      PIDTMP = PARTYP(IIPP)
      HCLOSES = HCLO(IIPP)*FAC
      RCLOSES = RCLO(IIPP)*FAC
      NODES = NCOL*NROW*NLAY
C
C     SET SNEW=SOLD=VALUES FROM DISK OR MEMORY
      IF (IUHEAD.GT.0) THEN
        IUTM = IUHEAD - 1 + IP
C       READ SOLD FOR THIS PARAMETER FROM A SCRATCH FILE
        READ(IUTM) (SOLD(I),I=1,NODES)
        REWIND(IUTM)
      ELSE
        IPTR = (IP-1)*NODES + 1
C       ASSIGN SOLD FOR THIS PARAMETER FROM MEMORY
        DO 50 I = 1,NODES
          SOLD(I) = XHS(IPTR-1+I)
   50   CONTINUE
      ENDIF
C     ASSIGN SNEW = SOLD
      DO 60 I = 1,NODES
        SNEW(I) = SOLD(I)
   60 CONTINUE
C
      RETURN
      END
C=======================================================================
      SUBROUTINE SEN1BAS6FM(NCOL,NLAY,NROW,RHS)
C-----VERSION 19990323 ERB
C     ******************************************************************
C     PREPARE TO CALCULATE SENSITIVITY-EQUATION RHS FOR ONE PARAMETER
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER NCOL, NLAY, NROW
      REAL RHS(NCOL*NROW*NLAY)
C     ------------------------------------------------------------------
C
C     INITIALIZE RIGHT-HAND SIDE TO ZERO
      CALL UPOPRELARR(RHS,NLAY*NROW*NCOL,0.0)
C
      RETURN
      END
C=======================================================================
      SUBROUTINE SEN1BAS6CS(HNEW,IBOUND,CR,CC,CV,HCOF,RHS,NCOL,NROW,
     &                      NLAY,IOUT,SEND,NPE,NTIMES,IP,ITS)
C-----VERSION 22DEC1998 SEN1BAS6CS
C
C     ******************************************************************
C     ACCUMULATE ALL INFLOWS AND ALL OUTFLOWS TO ALL CELLS, AND COMPARE
C     THE TOTALS.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION HNEW,DZERO,Q,C,SN,SP
      DIMENSION HNEW(NCOL,NROW,NLAY), IBOUND(NCOL,NROW,NLAY),
     &          CR(NCOL,NROW,NLAY), CC(NCOL,NROW,NLAY),
     &          CV(NCOL,NROW,NLAY), HCOF(NCOL,NROW,NLAY),
     &          RHS(NCOL,NROW,NLAY), SEND(NPE,NTIMES)
C     ------------------------------------------------------------------
C
C2------ASSIGN VALUES TO FIELDS THAT ARE CONSTANT DURING AN ITERATION
      DZERO=0.
      SP=DZERO
      SN=DZERO
C
C6------STEP THROUGH CELLS CALCULATING FLOWS
      DO 150 K=1,NLAY
      DO 150 I=1,NROW
      DO 150 J=1,NCOL
C
C
      IF(IBOUND(J,I,K).LE.0)GO TO 150
C
C6E1----NEIGHBOR IS 1 ROW BACK
      IF(I.NE.1) THEN
      IF(IBOUND(J,I-1,K).LT.0) THEN
         C=CC(J,I-1,K)
         Q=C*(HNEW(J,I-1,K)-HNEW(J,I,K))
         IF(Q.GT.DZERO) THEN
            SP=SP+Q
         ELSE
            SN=SN-Q
         END IF
      END IF
      END IF
C
C6E2----NEIGHBOR IS 1 ROW AHEAD
      IF(I.NE.NROW) THEN
      IF(IBOUND(J,I+1,K).LT.0) THEN
         C=CC(J,I,K)
         Q=C*(HNEW(J,I+1,K)-HNEW(J,I,K))
         IF(Q.GT.DZERO) THEN
            SP=SP+Q
         ELSE
            SN=SN-Q
         END IF
      END IF
      END IF
C
C6E3----NEIGHBOR IS 1 COLUMN BACK
      IF(J.NE.1) THEN
      IF(IBOUND(J-1,I,K).LT.0) THEN
         C=CR(J-1,I,K)
         Q=C*(HNEW(J-1,I,K)-HNEW(J,I,K))
         IF(Q.GT.DZERO) THEN
            SP=SP+Q
         ELSE
            SN=SN-Q
         END IF
      END IF
      END IF
C
C6E4----NEIGHBOR IS 1 COLUMN AHEAD
      IF(J.NE.NCOL) THEN
      IF(IBOUND(J+1,I,K).LT.0) THEN
         C=CR(J,I,K)
         Q=C*(HNEW(J+1,I,K)-HNEW(J,I,K))
         IF(Q.GT.DZERO) THEN
            SP=SP+Q
         ELSE
            SN=SN-Q
         END IF
      END IF
      END IF
C
C6E5----NEIGHBOR IS 1 LAYER BEHIND
      IF(K.NE.1) THEN
      IF(IBOUND(J,I,K-1).LT.0) THEN
         C=CV(J,I,K-1)
         Q=C*(HNEW(J,I,K-1)-HNEW(J,I,K))
         IF(Q.GT.DZERO) THEN
            SP=SP+Q
         ELSE
            SN=SN-Q
         END IF
      END IF
      END IF
C
C6E6----NEIGHBOR IS 1 LAYER AHEAD
      IF(K.NE.NLAY) THEN
      IF(IBOUND(J,I,K+1).LT.0) THEN
         C=CV(J,I,K)
         Q=C*(HNEW(J,I,K+1)-HNEW(J,I,K))
         IF(Q.GT.DZERO) THEN
            SP=SP+Q
         ELSE
            SN=SN-Q
         END IF
      END IF
      END IF
C
C
      Q=-RHS(J,I,K)
      IF(Q.GT.DZERO) THEN
         SP=SP+Q
      ELSE
         SN=SN-Q
      END IF
      Q=HCOF(J,I,K)
      Q=Q*HNEW(J,I,K)
      IF(Q.GT.DZERO) THEN
         SP=SP+Q
      ELSE
         SN=SN-Q
      END IF
C
  150 CONTINUE
C
C
      WRITE(IOUT,161) SP,SN
  161 FORMAT(1X,'SUM OF POSITIVE RATES=',1PE12.5,
     1        '    SUM OF NEGATIVE RATES=',1PE12.5)
      E=SP-SN
      A=(SP+SN)/2.
      IF (A.NE.0.0) THEN
        PE=100.*E/A
        WRITE(IOUT,162) PE
  162   FORMAT(1X,'              PERCENT DISCREPANCY=',F8.2)
        SEND(IP,ITS) = ABS(PE)
      ENDIF
C
C11-----RETURN
600   RETURN
C
      END
C=======================================================================
      SUBROUTINE SEN1BAS6OT(IHDDFL,IOUT,ISA,KSTP,IIPP,PIDTMP,SNEW,
     &                      BUFF,IOFLG,IBOUND,KPER,DELT,PERTIM,TOTIM,
     &                      ITMUNI,NCOL,NROW,NLAY,ICNVG,ISENFM,ISENPU,
     &                      ISENSU,CHEDFM,IXSEC,LBHDSV,HNOFLO,IP,
     &                      NPE,IPRINTS,IERR,BSCAL,NPLIST,LN)
C
C     VERSION 20000313 ERB
C     ******************************************************************
C     PRINT AND SAVE ARRAYS OF SENSITIVITIES FOR THE PARAMETERS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER PIDTMP*4
      CHARACTER*20 CHEDFM
      INTEGER IBOUND, ICNVG, ISENFM, ISENSU, IHDDFL,
     &        IIPP, IOFLG, IOUT, ISA, IPRINTS, ITMUNI,
     &        IXSEC, KPER, KSTP, LBHDSV, NCOL, NLAY, NROW
      REAL BB, BUFF, DELT, PERTIM, TOTIM, HNOFLO
      DOUBLE PRECISION SNEW
      DIMENSION BUFF(NCOL,NROW,NLAY), IBOUND(NCOL,NROW,NLAY),
     &          IOFLG(NLAY,5), SNEW(NCOL,NROW,NLAY), BSCAL(NPLIST),
     &          LN(NPLIST)
C     ------------------------------------------------------------------
C
C     DETERMINE IF SENSITIVITY DISTRIBUTIONS ARE TO BE PRINTED THIS TIME STEP
      IPTF = 0
      IF (IPRINTS.EQ.0) THEN
        IF (IHDDFL.NE.0) THEN
          DO 20 I=1,NLAY
            IF (IOFLG(I,1).GT.0) THEN
              IPTF = 1
              GOTO 30
            ENDIF
  20      CONTINUE
  30      CONTINUE
        ENDIF
      ELSEIF (IPRINTS.EQ.1) THEN
        IPTF = 1
      ENDIF
C
      IF (IPTF.NE.0 .AND. ISENPU.GT.0) THEN
C-------PRINT TITLES FOR SENSITIVITY ARRAYS
        CALL SSEN1BAS6PT(ISENPU,IIPP,BSCAL,NPLIST,LN)
      ENDIF
C
      IF (ISA.EQ.0) THEN
        IF (IPTF.NE.0) WRITE (IOUT,600) KSTP
 600    FORMAT (/,' FOR TIME STEP',I5,' ALL VALUES ARE 0.0',/)
      ELSE
C-------SET VALUE FOR CALCULATING ONE-PERCENT SCALED SENSITIVITIES
        CALL SSEN1BAS6BB(BB,IIPP,PIDTMP)
        CALL SSEN1BAS6OS(SNEW,BUFF,IOFLG,IBOUND,KSTP,KPER,DELT,PERTIM,
     &                  TOTIM,ITMUNI,NCOL,NROW,NLAY,ICNVG,IHDDFL,ISENFM,
     &                  ISENPU,ISENSU,IOUT,CHEDFM,IXSEC,LBHDSV,BB,
     &                  HNOFLO,IP,NPE,IIPP,IPRINTS,IERR,BSCAL,NPLIST,LN)
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE SEN1BAS6TM(NCOL,NROW,NLAY,IUHEAD,IP,BUFF,XHS,LENXHS,
     &                     SNEW)
C     VERSION 19990623 ERB
C     ******************************************************************
C     SAVE CURRENT SENSITIVITY ARRAY (SNEW) ON DISK OR IN MEMORY
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL BUFF, XHS
      INTEGER IP, IUHEAD, IUTM, NCOL, NLAY, NODES, NROW, LENXHS
      DOUBLE PRECISION SNEW(NCOL*NROW*NLAY)
      DIMENSION BUFF(NCOL*NROW*NLAY), XHS(LENXHS)
C     ------------------------------------------------------------------
      NODES = NCOL*NROW*NLAY
      IF (IUHEAD.GT.0) THEN
C       SAVE SNEW IN AN UNFORMATTED SCRATCH FILE
C       ASSIGN BUFF = SNEW
        DO 50 I = 1,NODES
          BUFF(I) = SNEW(I)
   50   CONTINUE
        IUTM = IUHEAD - 1 + IP
        WRITE(IUTM) (BUFF(I),I=1,NODES)
        REWIND (IUTM)
      ELSE
C       SAVE SNEW IN MEMORY
        IPTR = (IP-1)*NODES + 1
        DO 60 I = 1,NODES
          XHS(IPTR-1+I) = SNEW(I)
   60   CONTINUE
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE SEN1BAS6PD(IOUT,NPE,NPER,NSTP,NTIMES,SEND,SNDT)
C     VERSION 20000615 ERB
C     ******************************************************************
C     PRINT LARGEST PERCENT DISCREPANCY FROM SOLUTION OF SENSITIVITIES
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION NSTP(NPER), SEND(NPE,NTIMES), SNDT(NTIMES)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
C
  500 FORMAT(/,
     &' LARGEST (ABSOLUTE VALUE) SOLVER DISCREPANCY FOR SENSITIVITIES',/
     &'   WAS ',F8.2,' PERCENT AND OCCURRED FOR PARAMETER "',A,'"',/,
     &'   IN STRESS PERIOD ',I4,', TIME STEP ',I4)
  510 FORMAT(/,' ALL SOLVER DISCREPANCIES FOR SENSITIVITIES WERE 0.00')
C
C     MERGE ARRAY CONTAINING PERCENT DISCREPANCIES FOR ALL PARAMETERS
C     FOR ALL TIME STEPS
      CALL PLL1MX(SEND,SNDT,NPE,NTIMES)
C
C     DETERMINE LARGEST SOLVER DISCREPANCY AND ASSOCIATED PARAMETER,
C     STRESS PERIOD AND TIME STEP
      KTS = 0
      MSDPAR = 0
      SENDISCMAX = 0.0
      DO 30 K = 1, NPER
        DO 20 J = 1, NSTP(K)
          KTS = KTS + 1
          DO 10 I = 1, NPE
            IF (SEND(I,KTS).GT.SENDISCMAX) THEN
              SENDISCMAX = SEND(I,KTS)
              MSDPAR = IPPTR(I)
              MSDSP = K
              MSDTS = J
            ENDIF
   10     CONTINUE
   20   CONTINUE
   30 CONTINUE
C
      IF (MSDPAR.GT.0) THEN
        WRITE (IOUT,500) SENDISCMAX,PARNAM(MSDPAR),MSDSP,MSDTS
      ELSE
        WRITE (IOUT,510)
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE SSEN1BAS6OS(SNEW,BUFF,IOFLG,IBOUND,KSTP,KPER,DELT,
     &                      PERTIM,TOTIM,ITMUNI,NCOL,NROW,NLAY,ICNVG,
     &                      IHDDFL,ISENFM,ISENPU,ISENSU,IOUT,CHEDFM,
     &                      IXSEC,LBHDSV,B,HNOFLO,IP,NPE,IIPP,
     &                      IPRINTS,IERR,BSCAL,NPLIST,LN)
C
C     VERSION 20000313 ERB
C     ******************************************************************
C     OUTPUT SENSITIVITY ARRAYS AND TIME SUMMARY
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER I, IBOUND, ICNVG, IHDDFL, ISENFM, ISENSU, IOFLG, IOUT,
     &        IPFLG, ITMUNI, IXSEC, J, K, KPER, KSTP, LBHDSV, IP,
     &        NPE
      REAL B, BUFF, DELT, PERTIM, TOTIM, HNOFLO
      DOUBLE PRECISION SNEW, DB, HNF
      DIMENSION SNEW(NCOL,NROW,NLAY), IOFLG(NLAY,5),
     &          IBOUND(NCOL,NROW,NLAY), BUFF(NCOL,NROW,NLAY),
     &          BSCAL(NPLIST), LN(NPLIST)
      CHARACTER*20 CHEDFM
C     ------------------------------------------------------------------
C
C1------CLEAR PRINTOUT FLAG (IPFLG)
      IPFLG=0
      HNF=HNOFLO
C
C2------IF ITERATIVE PROCEDURE FAILED TO CONVERGE PRINT MESSAGE
      IF(ICNVG.EQ.0) THEN
        WRITE(IOUT,1) KSTP,KPER
        IF (IERR.EQ.0) IERR = -1
      ENDIF
    1 FORMAT(/,1X,79('*'),/,1X,'**',T79,'**',/,1X,'**',6X,
     &      '****FAILED TO CONVERGE IN TIME STEP',I3,
     &      ' OF STRESS PERIOD',I3,'****',T79,'**',/,1X,'**',T79,
     &      '**',/,1X,79('*'),/)
C
C3------IF HEAD AND DRAWDOWN FLAG (IHDDFL) IS SET AND IPRINTS=0, WRITE
C3------SENSITIVITY ARRAYS IN ACCORDANCE WITH FLAGS IN IOFLG.
      IF(IHDDFL.EQ.0 .AND. IPRINTS.EQ.0) GO TO 100
C
C     SET SCALING FACTOR
      DB=DABS(DBLE(B))
      IF (LN(IIPP).LE.0) THEN
        IF(DB.LT.DBLE(BSCAL(IIPP))) THEN
          DB = DBLE(BSCAL(IIPP))
        ENDIF
      ENDIF
C
C     SCALE THE SNEW ARRAY TO BE ONE-PERCENT SCALED SENSITIVITIES
      DO 50 K=1,NLAY
        DO 49 I=1,NROW
          DO 48 J=1,NCOL
            SNEW(J,I,K)=SNEW(J,I,K)*DB/100.0
            IF(IBOUND(J,I,K).EQ.0) SNEW(J,I,K)=HNF
   48     CONTINUE
   49   CONTINUE
   50 CONTINUE
C
C     PRINT ONE-PERCENT SCALED SENSITIVITIES
      CALL SSEN1BAS6OT(SNEW,BUFF,IOFLG,KSTP,KPER,NCOL,NROW,NLAY,IOUT,
     &                ISENFM,ISENPU,ISENSU,IPFLG,PERTIM,TOTIM,CHEDFM,
     &                IXSEC,LBHDSV,IIPP,IPRINTS,IBOUND)
C
C     UNSCALE THE SNEW ARRAY
      DO 70 K=1,NLAY
        DO 69 I=1,NROW
          DO 68 J=1,NCOL
            IF(IBOUND(J,I,K).NE.0) SNEW(J,I,K)=SNEW(J,I,K)*100.0/DB
   68     CONTINUE
   69   CONTINUE
   70 CONTINUE
C
C2------IF ITERATIVE PROCEDURE FAILED TO CONVERGE PRINT MESSAGE AGAIN
      IF(ICNVG.EQ.0) WRITE(IOUT,1) KSTP,KPER
C
  100 CONTINUE
C
C5------END PRINTOUT WITH TIME SUMMARY AND FORM FEED IF ANY PRINTOUT
C5------WILL BE PRODUCED.
      IF(IPFLG.NE.0 .AND. IP.EQ.NPE) THEN
        CALL SGWF1BAS6T(KSTP,KPER,DELT,PERTIM,TOTIM,ITMUNI,IOUT)
        WRITE(IOUT,101)
  101   FORMAT(' ')
      ENDIF
C
C6------RETURN
      RETURN
      END
C=======================================================================
      SUBROUTINE SSEN1BAS6OT(SNEW,BUFF,IOFLG,KSTP,KPER,NCOL,NROW,NLAY,
     &                      IOUT,ISENFM,ISENPU,ISENSU,IPFLG,PERTIM,
     &                      TOTIM,CHEDFM,IXSEC,LBHDSV,IIPP,IPRINTS,
     &                      IBOUND)
C
C     VERSION 20010522 ERB
C     ******************************************************************
C     PRINT AND RECORD SENSITIVITY ARRAYS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER I, IBOUND, IFIRST, ISENFM, ISENSU, IOFLG, IOUT, IPFLG,
     &        IXSEC, J, K, KK, KL, KPER, KSTP, LBHDSV
      REAL BUFF, PERTIM, TOTIM
      CHARACTER*16 TEXT
      DOUBLE PRECISION SNEW
      DIMENSION IBOUND(NCOL,NROW,NLAY), IOFLG(NLAY,4),
     &          BUFF(NCOL,NROW,NLAY), SNEW(NCOL,NROW,NLAY)
      CHARACTER*20 CHEDFM
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
C
      TEXT = PARNAM(IIPP)//' SENS.'
C1------FOR EACH LAYER MOVE SNEW TO BUFF IF PRINT OR SAVE IS REQUESTED.
      DO 59 K=1,NLAY
C
C2------IS SENSITIVITY NEEDED FOR THIS LAYER?
      KL=K
      IF(IXSEC.NE.0) KL=1
      IF(IOFLG(KL,1).EQ.0 .AND. IOFLG(KL,3).EQ.0 .AND. IPRINTS.EQ.0)
     &    GO TO 59
C
C3------MOVE SNEW TO BUFF FOR THE LAYER.
      DO 58 I=1,NROW
      DO 58 J=1,NCOL
      BUFF(J,I,K)=SNEW(J,I,K)
   58 CONTINUE
   59 CONTINUE
C
C4------FOR EACH LAYER: DETERMINE IF SENSITIVITIES SHOULD BE PRINTED.
C4------IF SO THEN CALL ULAPRS OR ULAPRW TO PRINT SENSITIVITY ARRAYS.
      IF(IXSEC.EQ.0) THEN
        IF (ISENPU.GT.0) THEN
          DO 69 K=1,NLAY
          KK=K
          IF(IOFLG(K,1).EQ.0 .AND. IPRINTS.EQ.0) GO TO 69
          IF(ISENFM.LT.0) CALL ULAPRS(BUFF(1,1,K),TEXT,KSTP,KPER,
     1                 NCOL,NROW,KK,-ISENFM,ISENPU)
          IF(ISENFM.GE.0) CALL ULAPRW(BUFF(1,1,K),TEXT,KSTP,KPER,
     1                 NCOL,NROW,KK,ISENFM,ISENPU)
          IPFLG=1
   69     CONTINUE
        ENDIF
C
C4A-----PRINT SENSITIVITY ARRAY FOR CROSS SECTION.
      ELSE
        IF (ISENPU.GT.0) THEN
          IF(IOFLG(1,1).NE.0 .OR. IPRINTS.EQ.1) THEN
            IF(ISENFM.LT.0) CALL ULAPRS(BUFF,TEXT,KSTP,KPER,
     1                   NCOL,NLAY,-1,-ISENFM,ISENPU)
            IF(ISENFM.GE.0) CALL ULAPRW(BUFF,TEXT,KSTP,KPER,
     1                   NCOL,NLAY,-1,ISENFM,ISENPU)
            IPFLG=1
          ENDIF
        ENDIF
      ENDIF
C
C5------FOR EACH LAYER: DETERMINE IF SENSITIVITIES SHOULD BE SAVED ON DISK.
C5------IF SO THEN CALL ULASAV OR ULASV2 TO SAVE SENSITIVITY ARRAYS.
      IFIRST=1
      IF(ISENSU.LE.0) GO TO 80
      IF(IXSEC.EQ.0) THEN
        DO 79 K=1,NLAY
        KK=K
        IF(IOFLG(K,3).EQ.0 .AND. IPRINTS.EQ.0) GO TO 79
        IF(IFIRST.EQ.1) WRITE(IOUT,74) TEXT,ISENSU,KSTP,KPER
   74   FORMAT(1X,/1X,A,' WILL BE SAVED ON UNIT',I4,
     1      ' AT END OF TIME STEP',I3,', STRESS PERIOD',I3)
        IFIRST=0
        IF(CHEDFM.EQ.' ') THEN
           CALL ULASAV(BUFF(1,1,K),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NROW,KK,ISENSU)
        ELSE
           CALL ULASV2(BUFF(1,1,K),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NROW,KK,ISENSU,CHEDFM,LBHDSV,IBOUND)
        END IF
   79   CONTINUE
C
C5A-----SAVE SENSITIVITIES FOR CROSS SECTION.
      ELSE
        IF(IOFLG(1,3).NE.0 .OR. IPRINTS.EQ.1) THEN
          WRITE(IOUT,74) ISENSU,KSTP,KPER
          IF(CHEDFM.EQ.' ') THEN
             CALL ULASAV(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NLAY,-1,ISENSU)
          ELSE
             CALL ULASV2(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                  NLAY,-1,ISENSU,CHEDFM,LBHDSV,IBOUND)
          END IF
        END IF
      END IF
C
C6------RETURN.
   80 RETURN
      END
C=======================================================================
      SUBROUTINE SSEN1BAS6BB(BB,IIPP,PIDTMP)
C     VERSION 19980811 ERB
C     ******************************************************************
C     SET VALUE FOR CALCULATING ONE-PERCENT SCALED SENSITIVITIES
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER IIPP
      REAL BB
      CHARACTER*4 PIDTMP
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
C
      BB = 1.
      IF (PIDTMP.NE.'CH  ') BB = B(IIPP)
C
      RETURN
      END
C=======================================================================
      SUBROUTINE SSEN1BAS6PT(ISENPU,IIPP,BSCAL,NPLIST,LN)
C-----VERSION 20000313 ERB
C     ******************************************************************
C     PRINT TITLES OF PRINTED ARRAYS WHEN THE PARAMETER-ESTIMATION
C     PACKAGE IS ACTIVE
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER ISENPU, IIPP
      DIMENSION BSCAL(NPLIST), LN(NPLIST)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
  550 FORMAT (/,
     &     ' ONE-PERCENT SCALED SENSITIVITIES FOR PARAMETER "',
     &     A,'", SCALED BY *B/100.',/)
  555 FORMAT (/,
     &     ' SENSITIVITIES FOR PARAMETER "',
     &     A,'" ARE NOT SCALED BECAUSE OF THE PARAMETER TYPE.',/)
  560 FORMAT (/,
     &     ' ONE-PERCENT SCALED SENSITIVITIES FOR PARAMETER "',
     &     A,'",',/,' SCALED BY *BSCAL/100.0,',
     &     ' WHERE BSCAL IS THE ALTERNATE SCALING FACTOR',/)
C
C    PRINT TITLE FOR SENSITIVITY DISTRIBUTIONS
      IF (LN(IIPP).LE.0) THEN
        IF (ABS(B(IIPP)).GE.BSCAL(IIPP)) THEN
          IF (PARTYP(IIPP).NE.'CH  ') THEN
            WRITE (ISENPU,550) PARNAM(IIPP)
          ELSE
            WRITE (ISENPU,555) PARNAM(IIPP)
          ENDIF
        ELSE
          IF (PARTYP(IIPP).NE.'CH  ') THEN
            WRITE (ISENPU,560) PARNAM(IIPP)
          ELSE
            WRITE (ISENPU,555) PARNAM(IIPP)
          ENDIF
        ENDIF
      ELSE
        IF (PARTYP(IIPP).NE.'CH  ') THEN
          WRITE (ISENPU,550) PARNAM(IIPP)
        ELSE
          WRITE(ISENPU,555) PARNAM(IIPP)
        ENDIF
      ENDIF
C
      RETURN
      END

