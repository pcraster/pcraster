! Time of File Save by ERB: 6/15/2006 12:56PM
C     Last change:  ERB  29 Aug 2002    1:22 pm
      SUBROUTINE GLO1BAS6DF(INUNIT,IUNIT,CUNIT,IREWND,NIUNIT,IOUTG,IOUT,
     1                    VERSION,NCOL,NROW,NLAY,NPER,ITMUNI,ISUMGX,
     2                    MXPER,ISUMIG,ISUMGZ,INBAS,LENUNI,ISUMX,ISUMZ,
     3                    ISUMIX,LAYHDT,IUDIS,IFREFM,INAMLOC,IPRTIM,
     4                    IBDT,SHOWPROG,NOTICECOUNT)
C
C-----VERSION 24JAN2000 GLO1BAS6DF
C     ******************************************************************
C     GLOBAL DEFINITION
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
      INCLUDE 'parallel.inc'
      CHARACTER*4 CUNIT(NIUNIT)
      CHARACTER*10 CHDATE, CHTIME, CHZONE
      INTEGER IBDT(8)
      INTEGER LAYHDT(999), IUNIT(NIUNIT), IREWND(NIUNIT)
      CHARACTER*40 VERSION
      CHARACTER*200 LINE
      LOGICAL SHOWPROG
C     ------------------------------------------------------------------
C
C     Get current date and time, assign to IBDT, and write to screen
C-----oschmitz start
C      CALL DATE_AND_TIME(CHDATE,CHTIME,CHZONE,IBDT)
C      IF (MYID.EQ.MPROC) WRITE(*,2) (IBDT(I),I=1,3),(IBDT(I),I=5,7)
C-----oschmitz end
    2 FORMAT(1X,'Run start date and time (yyyy/mm/dd hh:mm:ss): ',
     &I4,'/',I2.2,'/',I2.2,1X,I2,':',I2.2,':',I2.2,/)
C
C  Open all files in name file
      CALL SGLO1BAS6OPEN(INUNIT,IOUTG,IOUT,IUNIT,CUNIT,IREWND,NIUNIT,
     &                 VERSION,INBAS)
C
C  Check for the FREE format and other options in the BAS file.
      CALL URDCOM(INBAS,0,LINE)
      IFREFM=0
      IPRTIM=0
      SHOWPROG=.FALSE.
      LLOC=1
      WRITE(IOUTG,'(1X)')
    5 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INBAS)
      IF(LINE(ISTART:ISTOP).EQ.'FREE') THEN
         IFREFM=1
         WRITE(IOUTG,6)
    6    FORMAT (1X,'THE FREE FORMAT OPTION HAS BEEN SELECTED')
      ELSEIF(LINE(ISTART:ISTOP).EQ.'PRINTTIME') THEN
         IPRTIM=1
         WRITE(IOUTG,2) (IBDT(I),I=1,3),(IBDT(I),I=5,7)
      ELSEIF(LINE(ISTART:ISTOP).EQ.'SHOWPROGRESS') THEN
         SHOWPROG=.TRUE.
      END IF
      IF(LLOC.LT.200) GO TO 5
      REWIND(INBAS)
C
C  Initialize parameter definition variables.
      IPSUM=0
      ICLSUM=0
      IDEFPAR=0
      DO 10 N=1,MXPAR
        PARNAM(N)=' '
        PARTYP(N)=' '
        IPLOC(1,N)=0
        IPLOC(2,N)=0
        IACTIVE(N)=0
   10 CONTINUE
      DO N=1,MXMLT
        MLTNAM(N) = ' '
      ENDDO
      DO N=1,MXZON
        ZONNAM(N) = ' '
      ENDDO
C
C------Check for existence of discretization file
      INDIS=IUNIT(IUDIS)
      IF(INDIS.LE.0) THEN
         WRITE(IOUTG,*) ' DIS file must be specified for MODFLOW to run'
         CALL USTOP(' ')
      END IF
      WRITE(IOUTG,590) INDIS
  590 FORMAT(1X,/1X,'DISCRETIZATION INPUT DATA READ FROM UNIT ',I4)
C
C5------READ NUMBER OF LAYERS, ROWS, COLUMNS, STRESS PERIODS, AND
C5------ITMUNI USING FREE OR FIXED FORMAT.
C
C2------READ FIRST RECORD AND WRITE
      CALL URDCOM(INDIS,IOUTG,LINE)
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NLAY,R,IOUTG,INDIS)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NROW,R,IOUTG,INDIS)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NCOL,R,IOUTG,INDIS)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPER,R,IOUTG,INDIS)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITMUNI,R,IOUTG,INDIS)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,LENUNI,R,IOUTG,INDIS)
C
C6------PRINT # OF LAYERS, ROWS, COLUMNS AND STRESS PERIODS.
      WRITE(IOUTG,600) NLAY,NROW,NCOL
  600 FORMAT(1X,I4,' LAYERS',I10,' ROWS',I10,' COLUMNS')
      WRITE(IOUTG,610) NPER
  610 FORMAT(1X,I4,' STRESS PERIOD(S) IN SIMULATION')
      IF(NPER.GT.MXPER) THEN
         WRITE(IOUTG,620) MXPER
  620    FORMAT(1X,'THE MAXIMUM NUMBER OF STRESS PERIODS IS:',I5,/
     1      1X,'ABORTING BECAUSE THE MAXIMUM IS EXCEEDED')
         CALL USTOP(' ')
      END IF
C
C6.5----STOP THE SIMULATION IF THERE ARE MORE THAN 999 LAYERS.
      IF(NLAY.GT.999) THEN
         WRITE(IOUT,625)
  625    FORMAT(1X,/1X,'YOU HAVE SPECIFIED MORE THAN 999 MODEL LAYERS'/
     1 1X,'SPACE IS RESERVED FOR A MAXIMUM OF 999 LAYERS')
         CALL USTOP(' ')
      END IF
C
C7------SELECT AND PRINT A MESSAGE SHOWING TIME UNIT.
      IF(ITMUNI.LT.0 .OR. ITMUNI.GT.5) ITMUNI=0
      IF(ITMUNI.EQ.0) THEN
         WRITE(IOUTG,630)
  630    FORMAT(1X,'MODEL TIME UNIT IS UNDEFINED')
      ELSE IF(ITMUNI.EQ.1) THEN
         WRITE(IOUTG,640)
  640    FORMAT(1X,'MODEL TIME UNIT IS SECONDS')
      ELSE IF(ITMUNI.EQ.2) THEN
         WRITE(IOUTG,650)
  650    FORMAT(1X,'MODEL TIME UNIT IS MINUTES')
      ELSE IF(ITMUNI.EQ.3) THEN
         WRITE(IOUTG,660)
  660    FORMAT(1X,'MODEL TIME UNIT IS HOURS')
      ELSE IF(ITMUNI.EQ.4) THEN
         WRITE(IOUTG,670)
  670    FORMAT(1X,'MODEL TIME UNIT IS DAYS')
      ELSE
         WRITE(IOUTG,680)
  680    FORMAT(1X,'MODEL TIME UNIT IS YEARS')
      END IF
C
C7------SELECT AND PRINT A MESSAGE SHOWING LENGTH UNIT.
      IF(LENUNI.LT.0 .OR. LENUNI.GT.3) LENUNI=0
      IF(LENUNI.EQ.0) THEN
         WRITE(IOUTG,690)
  690    FORMAT(1X,'MODEL LENGTH UNIT IS UNDEFINED')
      ELSE IF(LENUNI.EQ.1) THEN
         WRITE(IOUTG,700)
  700    FORMAT(1X,'MODEL LENGTH UNIT IS FEET')
      ELSE IF(LENUNI.EQ.2) THEN
         WRITE(IOUTG,710)
  710    FORMAT(1X,'MODEL LENGTH UNIT IS METERS')
      ELSE IF(LENUNI.EQ.3) THEN
         WRITE(IOUTG,720)
  720    FORMAT(1X,'MODEL LENGTH UNIT IS CENTIMETERS')
      END IF
C
C     INITIALIZE POINTERS USED IN ALLOCATING SPACE IN MAIN-UNIT ARRAYS
      ISUMGX=1
      ISUMIG=1
      ISUMGZ=1
      ISUMX=1
      ISUMZ=1
      ISUMIX=1
C
C     INITIALIZE OTHER POINTERS AND COUNTERS
      INAMLOC=1
      NOTICECOUNT=0
C
C     INITIALIZE HEAD-DEPENDENT THICKNESS INDICATOR TO CODE
C     INDICATING LAYER IS UNDEFINED
      DO 100 I=1,NLAY
        LAYHDT(I)=-1
  100 CONTINUE
C
      RETURN
      END
C=======================================================================
      SUBROUTINE GLO1BAS6AL(INDIS,NCNFBD,NBOTM,NCOL,NROW,NLAY,LCBOTM,
     1          LCDELR,LCDELC,ISUM,IOUT,LCHNEW,LCIBOU,LCCR,LCCC,LCCV,
     2          LCRHS,LCHCOF,LCHOLD,LCBUFF,LCSTRT,ISUMZ,ISUMI,ISEN,
     3          IOBS,IPES,ISENALL,ITMXP,IPAR,INZONE,INMULT,NMLTAR,
     4          NZONAR,NML,NZN,LCRMLT,LCIZON,IGWT)
C
C-----VERSION 05JAN1999 GLO1BAS6AL
C     ******************************************************************
C     ALLOCATE GLOBAL DATA; SET IPAR
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      COMMON /DISCOM/LBOTM(999),LAYCBD(999)
C     ------------------------------------------------------------------
C
C  Print message about GWT Package
      IF (IGWT.GT.0) THEN
        WRITE(IOUT,5)
    5   FORMAT(' THE GROUND-WATER TRANSPORT PROCESS IS ACTIVE')
      ELSE
        WRITE(IOUT,6)
    6   FORMAT(' THE GROUND-WATER TRANSPORT PROCESS IS INACTIVE')
      END IF
C
      CALL SGLO1BAS6IPAR(ISEN,IOBS,IPES,ISENALL,ITMXP,IPAR,IOUT)
C
C  Allocate memory for Zone and Multiplier arrays
      CALL SGLO1BAS6AL(NCOL,NROW,IOUT,INZONE,INMULT,NMLTAR,NZONAR,
     1             NML,NZN,ISUM,LCRMLT,ISUMI,LCIZON)
C
C1-----ALLOCATE DELR, DELC
      LCDELR=ISUM
      ISUM=ISUM+NCOL
      LCDELC=ISUM
      ISUM=ISUM+NROW
C
C------Read confining bed information
      READ(INDIS,*) (LAYCBD(K),K=1,NLAY)
      LAYCBD(NLAY)=0
      WRITE(IOUT,*) ' Confining bed flag for each layer:'
      WRITE(IOUT,'(20I4)') (LAYCBD(K),K=1,NLAY)
C
C------Count confining beds, setup the pointer to each layer's
C------bottom array (LBOTM), and setup LAYCBD to be the confining
C------bed number for each layer.
      NCNFBD=0
      DO 10 K=1,NLAY
      LBOTM(K)=K+NCNFBD
      IF(LAYCBD(K).NE.0) THEN
         NCNFBD=NCNFBD+1
         LAYCBD(K)=NCNFBD
      END IF
   10 CONTINUE
      NBOTM=NLAY+NCNFBD
C
C------Allocate space for BOTM.  Note that NBOTM+1 arrays are allocated
C------because BOTM(J,I,0) contains the top elevation of layer 1.
      LCBOTM=ISUM
      ISUM=ISUM+NCOL*NROW*(NBOTM+1)
C
C-------Allocate space for global arrays
      NRCL=NROW*NCOL*NLAY
      LCHNEW=ISUMZ
      ISUMZ=ISUMZ+NRCL
      LCHOLD=ISUM
      ISUM=ISUM+NRCL
      LCIBOU=ISUMI
      ISUMI=ISUMI+NRCL
      LCCR=ISUM
      ISUM=ISUM+NRCL
      LCCC=ISUM
      ISUM=ISUM+NRCL
      LCCV=ISUM
      ISUM=ISUM+NROW*NCOL*(NLAY-1)
      LCHCOF=ISUM
      ISUM=ISUM+NRCL
      LCRHS=ISUM
      ISUM=ISUM+NRCL
      LCBUFF=ISUM
      ISUM=ISUM+NRCL
      LCSTRT=ISUM
      ISUM=ISUM+NRCL
C
C------RETURN.
      RETURN
      END
C=======================================================================
      SUBROUTINE GLO1BAS6RP(IN,NCOL,NROW,NLAY,BOTM,NBOTM,IOUT,DELR,DELC,
     &                    NPER,PERLEN,NSTP,TSMULT,ISSFLG,ITRSS,INZONE,
     &                    INMULT,NMLTAR,NZONAR,RMLT,IZON,NML,NZN)
C
C-----VERSION 24JAN2000 GLO1BAS6RP
C     ******************************************************************
C     READ GLOBAL ARRAYS.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      COMMON /DISCOM/LBOTM(999),LAYCBD(999)
      DIMENSION PERLEN(NPER),NSTP(NPER),TSMULT(NPER),ISSFLG(NPER)
      DIMENSION BOTM(NCOL,NROW,0:NBOTM),DELR(NCOL),DELC(NROW),
     &          RMLT(NCOL,NROW,NMLTAR),IZON(NCOL,NROW,NZONAR)
      CHARACTER*200 LINE
      CHARACTER*24 ANAME(5)
      DATA ANAME(1) /'                    DELR'/
      DATA ANAME(2) /'                    DELC'/
      DATA ANAME(3) /'TOP ELEVATION OF LAYER 1'/
      DATA ANAME(4) /'  MODEL LAYER BOTTOM EL.'/
      DATA ANAME(5) /'BOT. EL. OF QUASI-3D BED'/
C     ------------------------------------------------------------------
C
C------Read the DELR and DELC arrays.
      CALL U1DREL(DELR,ANAME(1),NCOL,IN,IOUT)
      CALL U1DREL(DELC,ANAME(2),NROW,IN,IOUT)
C
C------Read the top elevation of layer 1.
      CALL U2DREL(BOTM(1,1,0),ANAME(3),NROW,NCOL,0,IN,IOUT)
C
C------Read the bottom elevations.
      DO 20 K=1,NLAY
      KK=K
      CALL U2DREL(BOTM(1,1,LBOTM(K)),ANAME(4),NROW,NCOL,KK,IN,IOUT)
      IF(LAYCBD(K).NE.0) CALL U2DREL(BOTM(1,1,LBOTM(K)+1),ANAME(5),
     1          NROW,NCOL,KK,IN,IOUT)
   20 CONTINUE
C
C1------READ AND WRITE LENGTH OF STRESS PERIOD, NUMBER OF TIME STEPS,
C1------TIME STEP MULTIPLIER, AND STEADY-STATE FLAG..
      WRITE(IOUT,61)
   61 FORMAT(1X,//1X,'STRESS PERIOD     LENGTH       TIME STEPS',
     1            '     MULTIPLIER FOR DELT    SS FLAG',/1X,76('-'))
      ISS=0
      ITR=0
      DO 200 N=1,NPER
      READ(IN,'(A)') LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,PERLEN(N),IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NSTP(N),R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TSMULT(N),IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF (LINE(ISTART:ISTOP).EQ.'TR') THEN
         ISSFLG(N)=0
         ITR=1
      ELSE IF (LINE(ISTART:ISTOP).EQ.'SS') THEN
         ISSFLG(N)=1
         ISS=1
      ELSE
         WRITE(IOUT,62)
   62    FORMAT(' SSFLAG MUST BE EITHER "SS" OR "TR"',
     1      ' -- STOP EXECUTION (GLO1BAS6RP)')
         CALL USTOP(' ')
      END IF
      WRITE (IOUT,63) N,PERLEN(N),NSTP(N),TSMULT(N),LINE(ISTART:ISTOP)
   63 FORMAT(1X,I8,1PG21.7,I7,0PF25.3,A11)
C
C1A-----STOP IF NSTP LE 0, PERLEN LE 0., OR TSMULT LE 0.
      IF(NSTP(N).LE.0) THEN
         WRITE(IOUT,160)
  160    FORMAT(1X,/1X,
     1  'THERE MUST BE AT LEAST ONE TIME STEP IN EVERY STRESS PERIOD')
         CALL USTOP(' ')
      END IF
      ZERO=0.
      IF(PERLEN(N).EQ.ZERO .AND. ISSFLG(N).EQ.0) THEN
         WRITE(IOUT,165)
  165    FORMAT(1X,/1X,
     1  'PERLEN MUST NOT BE 0.0 FOR TRANSIENT STRESS PERIODS')
         CALL USTOP(' ')
      END IF
      IF(TSMULT(N).LE.ZERO) THEN
         WRITE(IOUT,170)
  170    FORMAT(1X,/1X,'TSMULT MUST BE GREATER THAN 0.0')
         CALL USTOP(' ')
      END IF
      IF(PERLEN(N).LT.ZERO) THEN
         WRITE(IOUT,175)
  175    FORMAT(1X,/1X,
     1  'PERLEN CANNOT BE LESS THAN 0.0 FOR ANY STRESS PERIOD')
         CALL USTOP(' ')
      END IF
  200 CONTINUE
C
C  Assign ITRSS
      IF(ISS.EQ.0 .AND. ITR.NE.0) THEN
         ITRSS=1
         WRITE(IOUT,70)
   70    FORMAT(/,1X,'TRANSIENT SIMULATION')
      ELSE IF(ISS.NE.0 .AND. ITR.EQ.0) THEN
         ITRSS=0
         WRITE(IOUT,75)
   75    FORMAT(/,1X,'STEADY-STATE SIMULATION')
      ELSE
         ITRSS=-1
         WRITE(IOUT,80)
   80    FORMAT(/,1X,'COMBINED STEADY-STATE AND TRANSIENT SIMULATION')
      END IF
C
      WRITE(IOUT,'(//)')
C
C  Read Zone and Multiplier arrays
      CALL SGLO1BAS6RP(INZONE,INMULT,NCOL,NROW,IOUT,NMLTAR,NZONAR,
     1                       RMLT,IZON,NML,NZN)
C
      RETURN
      END
C=======================================================================
      SUBROUTINE GLO1BAS6CK(IOUTG,ISEN,NPLIST)
C
C-----VERSION 20030908 GLO1BAS6CK
C     ******************************************************************
C     PRINT NUMBER OF DEFINED PARAMETERS AND CHECK THAT ALL PARAMETERS
C     LISTED IN THE SEN FILE HAVE BEEN DEFINED
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
  500 FORMAT(/,
     &1X,I4,' PARAMETER HAS BEEN DEFINED IN ALL PACKAGES.',/,
     &' (SPACE IS ALLOCATED FOR ',I4,' PARAMETERS.)')
  505 FORMAT(/,
     &1X,I4,' PARAMETERS HAVE BEEN DEFINED IN ALL PACKAGES.',/,
     &' (SPACE IS ALLOCATED FOR ',I4,' PARAMETERS.)')
  510 FORMAT(/,
     &' ERROR: MXPAR IN FILE "param.inc" MUST BE INCREASED TO AT LEAST '
     &,I4,/,' -- STOP EXECUTION (GLO1BAS6CK)')
  520 FORMAT(/,' *** ERROR: PARAMETER "',A,'" IS LISTED IN THE SEN',
     &' FILE BUT IS NOT DEFINED')
  530 FORMAT(/,' -- STOP EXECUTION (GLO1BAS6CK)')
C
C     PRINT NUMBER OF DEFINED PARAMETERS
      IF (IPSUM.EQ.1) WRITE (IOUTG,500) IPSUM,MXPAR
      IF (IPSUM.GT.1) WRITE (IOUTG,505) IPSUM,MXPAR
      IF (IPSUM.GT.MXPAR) THEN
        WRITE(IOUTG,510) IPSUM
        CALL USTOP(' ')
      ENDIF
C
C     CHECK THAT ALL PARAMETERS LISTED IN THE SEN FILE ARE DEFINED
      IERR = 0
      IF (ISEN.GT.0) THEN
        DO 10 IP=1,NPLIST
          IF (IACTIVE(IP).EQ.-99) THEN
            WRITE(IOUTG,520) PARNAM(IP)
            IERR = 1
          ENDIF
   10   CONTINUE
      ENDIF
      IF (IERR.GT.0) THEN
        WRITE(IOUTG,530)
        CALL USTOP(' ')
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE GLO1BAS6ET(IBDT,IOUTG,IPRTIM,NOTICECOUNT)
C
C-----VERSION 20011126
C     ******************************************************************
C     Get end time and calculate elapsed time
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'parallel.inc'
      CHARACTER*10 CHDATE, CHTIME, CHZONE
      INTEGER IBDT(8), IEDT(8), IDPM(12)
      DATA IDPM/31,28,31,30,31,30,31,31,30,31,30,31/ ! Days per month
      DATA NSPD/86400/  ! Seconds per day
C     ------------------------------------------------------------------
  900 FORMAT(/,1X,'A Message has been generated concerning',
     &' this simulation.',/,' Search above for "NOTICE".')
  910 FORMAT(/,1X,I3,1X,'Messages have been generated concerning',
     &' this simulation.',/,' Search above for "NOTICE".')
 1000 FORMAT(1X,'Run end date and time (yyyy/mm/dd hh:mm:ss): ',
     &I4,'/',I2.2,'/',I2.2,1X,I2,':',I2.2,':',I2.2)
 1010 FORMAT(1X,'Elapsed run time: ',I3,' Days, ',I2,' Hours, ',I2,
     &' Minutes, ',I2,' Seconds',/)
 1020 FORMAT(1X,'Elapsed run time: ',I2,' Hours, ',I2,
     &' Minutes, ',I2,' Seconds',/)
 1030 FORMAT(1X,'Elapsed run time: ',I2,' Minutes, ',
     &I2,'.',I3.3,' Seconds',/)
 1040 FORMAT(1X,'Elapsed run time: ',I2,'.',I3.3,' Seconds',/)
C
C     If any NOTICEs have been written to GLOBAL file, write a message
C     directing the user to look for them
      IF (NOTICECOUNT.EQ.1) THEN
        WRITE(IOUTG,900)
      ELSEIF (NOTICECOUNT.GT.1) THEN
        WRITE(IOUTG,910) NOTICECOUNT
      ENDIF
C
C     Get current date and time, assign to IEDT, and write to screen
      CALL DATE_AND_TIME(CHDATE,CHTIME,CHZONE,IEDT)
C-----oschmitz start
C      IF (MYID.EQ.MPROC) WRITE(*,1000) (IEDT(I),I=1,3),(IEDT(I),I=5,7)
C-----oschmitz end
C
C     Calculate elapsed time in days and seconds
      NDAYS=0
      LEAP=0
      IF (MOD(IEDT(1),4).EQ.0) LEAP = 1
      IBD = IBDT(3)            ! BEGIN DAY
      IED = IEDT(3)            ! END DAY
C     FIND DAYS
      IF (IBDT(2).NE.IEDT(2)) THEN
C       MONTHS DIFFER
        MB = IBDT(2)             ! BEGIN MONTH
        ME = IEDT(2)             ! END MONTH
        NM = ME-MB+1             ! NUMBER OF MONTHS TO LOOK AT
        IF (MB.GT.ME) NM = NM+12
        MC=MB-1
        DO 10 M=1,NM
          MC=MC+1                ! MC IS CURRENT MONTH
          IF (MC.EQ.13) MC = 1
          IF (MC.EQ.MB) THEN
            NDAYS = NDAYS+IDPM(MC)-IBD
            IF (MC.EQ.2) NDAYS = NDAYS + LEAP
          ELSEIF (MC.EQ.ME) THEN
            NDAYS = NDAYS+IED
          ELSE
            NDAYS = NDAYS+IDPM(MC)
            IF (MC.EQ.2) NDAYS = NDAYS + LEAP
          ENDIF
   10   CONTINUE
      ELSEIF (IBD.LT.IED) THEN
C       START AND END IN SAME MONTH, ONLY ACCOUNT FOR DAYS
        NDAYS = IED-IBD
      ENDIF
      ELSEC=NDAYS*NSPD
C
C     ADD OR SUBTRACT SECONDS
      ELSEC = ELSEC+(IEDT(5)-IBDT(5))*3600.0
      ELSEC = ELSEC+(IEDT(6)-IBDT(6))*60.0
      ELSEC = ELSEC+(IEDT(7)-IBDT(7))
      ELSEC = ELSEC+(IEDT(8)-IBDT(8))*0.001
C
C     CONVERT SECONDS TO DAYS, HOURS, MINUTES, AND SECONDS
      NDAYS = ELSEC/NSPD
      RSECS = MOD(ELSEC,86400.0)
      NHOURS = RSECS/3600.0
      RSECS = MOD(RSECS,3600.0)
      NMINS = RSECS/60.0
      RSECS = MOD(RSECS,60.0)
      NSECS = RSECS
      RSECS = MOD(RSECS,1.0)
      MSECS = NINT(RSECS*1000.0)
      NRSECS = NSECS
      IF (RSECS.GE.0.5) NRSECS=NRSECS+1
C
C     Write elapsed time to screen
C-----oschmitz start
C      IF (MYID.EQ.MPROC) THEN
C        IF (NDAYS.GT.0) THEN
C          WRITE(*,1010) NDAYS,NHOURS,NMINS,NRSECS
C        ELSEIF (NHOURS.GT.0) THEN
C          WRITE(*,1020) NHOURS,NMINS,NRSECS
C        ELSEIF (NMINS.GT.0) THEN
C          WRITE(*,1030) NMINS,NSECS,MSECS
C        ELSE
C          WRITE(*,1040) NSECS,MSECS
C        ENDIF
C      ENDIF
C-----oschmitz end
C
C     Write times to global file if requested
      IF (IPRTIM.GT.0) THEN
        WRITE(IOUTG,'(1X)')
        WRITE(IOUTG,1000) (IEDT(I),I=1,3),(IEDT(I),I=5,7)
        IF (NDAYS.GT.0) THEN
          WRITE(IOUTG,1010) NDAYS,NHOURS,NMINS,NRSECS
        ELSEIF (NHOURS.GT.0) THEN
          WRITE(IOUTG,1020) NHOURS,NMINS,NRSECS
        ELSEIF (NMINS.GT.0) THEN
          WRITE(IOUTG,1030) NMINS,NSECS,MSECS
        ELSE
          WRITE(IOUTG,1040) NSECS,MSECS
        ENDIF
      ENDIF
C
      RETURN
      END
C=======================================================================

      SUBROUTINE SGLO1BAS6OPEN(INUNIT,IOUTG,IOUT,IUNIT,CUNIT,
     1              IREWND,NIUNIT,VERSION,INBAS)
C
C-----VERSION 24JAN2000 SGLO1BAS6OPEN
C     ******************************************************************
C     OPEN FILES.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION IUNIT(NIUNIT),IREWND(NIUNIT)
      CHARACTER*1 DIGIT(0:9)
      CHARACTER*2 PSUF
      CHARACTER*4 CUNIT(NIUNIT)
      CHARACTER*7 FILSTAT
      CHARACTER*20 FILACT, FMTARG, ACCARG
      CHARACTER*40 VERSION, SPACES
      CHARACTER*200 LINE, FNAME
      LOGICAL LOP, MASTER
      INCLUDE 'parallel.inc'
      INCLUDE 'openspec.inc'
      DATA (DIGIT(I),I=0,9)/'0','1','2','3','4','5','6','7','8','9'/
C     ---------------------------------------------------------------
C
C1------INITIALIZE CONSTANTS.
      INBAS=0
      IOUTG=0
      NFILE=0
      IOUT=0
      DO 5 I=1,NIUNIT
      IUNIT(I)=0
      IREWND(I)=1
5     CONTINUE
      SPACES=' '
      LENVER=NONB_LEN(VERSION,40)
      INDENT=40-(LENVER+8)/2
      IFIRST=1
      KLIST=0
      PSUF='00'
      MASTER=.TRUE.
      IF(MYID.NE.MPROC) THEN
        MASTER=.FALSE.
        ITEN=MYID/10
        IONE=MOD(MYID,10)
        PSUF(1:1)=DIGIT(ITEN)
        PSUF(2:2)=DIGIT(IONE)
      ENDIF
C
C-------DETERMINE IF BOTH GLOBAL AND LIST FILES WILL BE PRODUCED --
C       READ FIRST TWO FILE NAMES THEN REWIND NAME FILE
C
C2------READ A LINE; IGNORE BLANK LINES AND PRINT COMMENT LINES.
10    READ(INUNIT,'(A)',END=1000) LINE
      IF(LINE.EQ.' ') GO TO 10
      IF(LINE(1:1).EQ.'#') THEN
        IF(NFILE.NE.0 .AND. IOUTG.NE.0) WRITE(IOUTG,'(A)') LINE
        GO TO 10
      END IF
C
C3------DECODE THE FILE TYPE AND UNIT NUMBER.
      LLOC=1
      CALL URWORD(LINE,LLOC,ITYP1,ITYP2,1,N,R,IOUT,INUNIT)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IU,R,IOUT,INUNIT)
C
C     KEEP TRACK OF LARGEST UNIT NUMBER
      IF (IU.GT.MAXUNIT) MAXUNIT = IU
C
C4------CHECK FOR A VALID FILE TYPE.
      FMTARG='FORMATTED'
      ACCARG='SEQUENTIAL'
      FILSTAT='UNKNOWN'
      FILACT=' '
C
C4A-----FIRST ENTRY MUST BE FILE-TYPE "LIST" OR "GLOBAL"
      IF(NFILE.EQ.0) THEN
         IF(LINE(ITYP1:ITYP2).EQ.'GLOBAL') THEN
            IOUTG=IU
            IF(IFIRST.EQ.1) KLIST=KLIST+1
            FILSTAT='REPLACE'
         ELSE IF(LINE(ITYP1:ITYP2).EQ.'LIST') THEN
            IOUTG=IU
            IOUT=IU
            IF(IFIRST.EQ.1) KLIST=KLIST+1
            FILSTAT='REPLACE'
         ELSE
            WRITE(*,*)
     1       ' FIRST ENTRY IN NAME FILE MUST BE "GLOBAL" OR "LIST".'
            CALL USTOP(' ')
         END IF
C
C4A-----2ND FILE CAN BE "LIST" FILE TYPE.
      ELSE IF(LINE(ITYP1:ITYP2).EQ.'LIST') THEN
         IOUT=IU
         IF(IFIRST.EQ.1) KLIST=KLIST+1
         FILSTAT='REPLACE'
C
C4B-----CHECK FOR "BAS" FILE TYPE.
      ELSE IF(LINE(ITYP1:ITYP2).EQ.'BAS6') THEN
         INBAS=IU
         FILSTAT='OLD    '
         FILACT=ACTION(1)
C
C4C-----CHECK FOR "UNFORMATTED" FILE TYPE.
      ELSE IF(LINE(ITYP1:ITYP2).EQ.'DATA(BINARY)' .OR.
     1        LINE(ITYP1:ITYP2).EQ.'DATAGLO(BINARY)') THEN
         FMTARG=FORM
         ACCARG=ACCESS
C
C4D-----CHECK FOR "FORMATTED" FILE TYPE.
      ELSE IF(LINE(ITYP1:ITYP2).EQ.'DATA' .OR.
     1        LINE(ITYP1:ITYP2).EQ.'DATAGLO') THEN
         FMTARG='FORMATTED'
         ACCARG='SEQUENTIAL'
C
C4E-----CHECK FOR MAJOR OPTIONS.
      ELSE
         IF(IFIRST.EQ.0) THEN
            DO 20 I=1,NIUNIT
               IF(LINE(ITYP1:ITYP2).EQ.CUNIT(I)) THEN
                  IUNIT(I)=IU
                  FILSTAT='OLD    '
                  FILACT=ACTION(1)
                  GO TO 30
               END IF
20          CONTINUE
            WRITE(IOUTG,21) LINE(ITYP1:ITYP2)
21          FORMAT(1X,'ILLEGAL FILE TYPE IN NAME FILE: ',A)
            CALL USTOP(' ')
30          CONTINUE
         ENDIF
      END IF
C
      IF(IFIRST.EQ.1) THEN
         NFILE=NFILE+1
         IOUT=0
         IOUTG=0
         IF(NFILE.EQ.2) THEN
            IFIRST=0
            NFILE=0
            REWIND(INUNIT)
         ENDIF
         GOTO 10
      ENDIF
C
C5------DETERMINE FILE NAME AND WRITE THE FILE NAME IF THE FILE IS NOT
C5------THE LISTING FILE.  THEN OPEN THE FILE.
      CALL URWORD(LINE,LLOC,INAM1,INAM2,0,N,R,IOUTG,INUNIT)
      IFLEN=INAM2-INAM1+1
      FNAME(1:IFLEN)=LINE(INAM1:INAM2)
      INQUIRE(UNIT=IU,OPENED=LOP)
      IF (LOP) CLOSE (UNIT=IU)
C-----IF FILE STATUS IS AMBIGUOUS (FILE TYPES THAT START WITH "DATA"),
C     CHECK FOR "REPLACE" OR "OLD" OPTION
      IF (FILSTAT.EQ.'UNKNOWN') THEN
        CALL URWORD(LINE,LLOC,IOPT1,IOPT2,1,N,R,IOUT,INUNIT)
        IF (LINE(IOPT1:IOPT2).EQ.'REPLACE' .OR.
     &      LINE(IOPT1:IOPT2).EQ.'OLD')
     &      FILSTAT = LINE(IOPT1:IOPT2)
      ENDIF
      IF (FILACT.EQ.' ') FILACT=ACTION(2)
      IF(.NOT.MASTER) THEN
        IF(LINE(ITYP1:ITYP2).EQ.'GLOBAL') THEN
          IFLEN=12
          FNAME(1:IFLEN)='mf2kglob.p'//PSUF
        ELSEIF(LINE(ITYP1:ITYP2).EQ.'LIST') THEN
          IFLEN=12
          FNAME(1:IFLEN)='mf2klist.p'//PSUF
        ELSEIF(LINE(ITYP1:ITYP2).EQ.'DATA(BINARY)' .OR.
     1       LINE(ITYP1:ITYP2).EQ.'DATAGLO(BINARY)' .OR.
     1       LINE(ITYP1:ITYP2).EQ.'DATA' .OR.
     1       LINE(ITYP1:ITYP2).EQ.'DATAGLO') THEN
          IF (FILSTAT.EQ.'REPLACE') THEN
            IFLEN=INAM2-INAM1+5
            FNAME(1:IFLEN)=LINE(INAM1:INAM2)//'.p'//PSUF
          ENDIF
        ENDIF
      ENDIF
C
      IF(NFILE.NE.0) WRITE(IOUTG,50) FNAME(1:IFLEN),
     1     LINE(ITYP1:ITYP2),IU,FILSTAT,FMTARG,ACCARG
50    FORMAT(1X,/1X,'OPENING ',A,/
     &  1X,'FILE TYPE:',A,'   UNIT ',I4,3X,'STATUS:',A,/
     &  1X,'FORMAT:',A,3X,'ACCESS:',A)
C
      OPEN(UNIT=IU,FILE=FNAME(1:IFLEN),FORM=FMTARG,
     1      ACCESS=ACCARG,STATUS=FILSTAT,ACTION=FILACT,ERR=2000)
C
C6------IF THE OPENED FILE IS THE LISTING FILE, WRITE ITS NAME.
C6------GO BACK AND READ NEXT RECORD.
      IF(NFILE.LE.1) THEN
         IF(NFILE.EQ.0) THEN
            WRITE(IOUTG,60) SPACES(1:INDENT),VERSION(1:LENVER)
60          FORMAT(34X,'MODFLOW-2000',/,
     &             6X,'U.S. GEOLOGICAL SURVEY MODULAR',
     &             ' FINITE-DIFFERENCE GROUND-WATER FLOW MODEL',/,
     &             A,'VERSION ',A,/)
            IF(KLIST.EQ.1) THEN
               WRITE(IOUTG,70)
70             FORMAT(/,1X,'This model run combines GLOBAL and LIST ',
     &                'output into this single file.',/)
            ELSEIF(KLIST.EQ.2)THEN
               WRITE(IOUTG,80)'GLOBAL file.'
80             FORMAT(/,1X,'This model run produced both GLOBAL and ',
     &                'LIST files.  This is the ',A,/)
            ENDIF
            WRITE(IOUTG,90) LINE(INAM1:INAM2),IU
90          FORMAT(1X,'GLOBAL LISTING FILE: ',A,/25X,'UNIT ',I4)
         ELSE
            IF(KLIST.EQ.2) THEN
               WRITE(IOUT,60) SPACES(1:INDENT),VERSION(1:LENVER)
               WRITE(IOUT,80)'LIST file.'
            ENDIF
         ENDIF
      ENDIF
      NFILE=NFILE+1
      GO TO 10
C
C7------END OF NAME FILE.  RETURN PROVIDED THAT LISTING FILE AND BAS
C7------FILES HAVE BEEN OPENED.
1000  IF(NFILE.EQ.0) THEN
         WRITE(*,*) ' NAME FILE IS EMPTY.'
         CALL USTOP(' ')
      ELSE IF(INBAS.EQ.0) THEN
         WRITE(IOUTG,*) ' BAS PACKAGE FILE HAS NOT BEEN OPENED.'
         CALL USTOP(' ')
      END IF
      INQUIRE(UNIT=INUNIT,OPENED=LOP)
      IF (LOP) CLOSE (UNIT=INUNIT)
      IF(IOUT.EQ.0) IOUT=IOUTG
      RETURN
C
 2000 CONTINUE
C     FILE-OPENING ERROR
      WRITE(*,2010)FNAME(1:IFLEN),IU,FILSTAT,FMTARG,ACCARG,FILACT
      WRITE(IOUTG,2010)FNAME(1:IFLEN),IU,FILSTAT,FMTARG,ACCARG,FILACT
 2010 FORMAT(/,1X,'*** ERROR OPENING FILE "',A,'" ON UNIT ',I5,/,
     &7X,'SPECIFIED FILE STATUS: ',A,/
     &7X,'SPECIFIED FILE FORMAT: ',A,/
     &7X,'SPECIFIED FILE ACCESS: ',A,/
     &7X,'SPECIFIED FILE ACTION: ',A,/
     &2X,'-- STOP EXECUTION (SGLO1BAS6OPEN)')
      CALL USTOP(' ')
C
      END
C=======================================================================
      SUBROUTINE SGLO1BAS6AL(NCOL,NROW,IOUT,INZONE,INMULT,NMLTAR,NZONAR,
     1             NML,NZN,ISUMGX,LCRMLT,ISUMIG,LCIZON)
C
C-----VERSION 24JAN2000 SGLO1BAS6AL
C     ******************************************************************
C     ALLOCATE SPACE FOR MULTIPLIER AND ZONE ARRAYS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
C
C------Read Number of Zone Arrays if Zone Option is active.
      NZN=0
      IF(INZONE.GT.0) THEN
         WRITE(IOUT,1) INZONE
    1    FORMAT(1X,/1X,'ZONE OPTION, INPUT READ FROM UNIT ',I4)
         CALL URDCOM(INZONE,IOUT,LINE)
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NZN,R,IOUT,INZONE)
         WRITE(IOUT,2) NZN
    2    FORMAT(1X,I5,' ZONE ARRAYS')
         IF(NZN.LT.0) NZN=0
      END IF
      IF(NZN.GT.0) THEN
        LCIZON=ISUMIG
        ISUMIG=ISUMIG+NROW*NCOL*NZN
        NZONAR=NZN
      ELSE
        LCIZON=1
        NZONAR=1
      ENDIF
C
C------Read Number of Multiplier Arrays if Multiplier Option is active.
      NML=0
      IF(INMULT.GT.0) THEN
         WRITE(IOUT,11) INMULT
   11    FORMAT(1X,/1X,'MULTIPLIER OPTION, INPUT READ FROM UNIT ',I4)
         CALL URDCOM(INMULT,IOUT,LINE)
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NML,R,IOUT,INMULT)
         WRITE(IOUT,12) NML
   12    FORMAT(1X,I3,' MULTIPLIER ARRAYS')
         IF(NML.LT.0) NML=0
      END IF
      IF(NML.GT.0) THEN
        LCRMLT=ISUMGX
        ISUMGX=ISUMGX+NROW*NCOL*NML
        NMLTAR=NML
      ELSE
        LCRMLT=1
        NMLTAR=1
      ENDIF
C
C  Initialize names of zones, multipliers, and parameters
      DO 10 I=1,NZONAR
      ZONNAM(I)=' '
10    CONTINUE
      DO 20 I=1,NMLTAR
      MLTNAM(I)=' '
20    CONTINUE
C
C6------RETURN.
      RETURN
      END
C=======================================================================
      SUBROUTINE SGLO1BAS6RP(INZONE,INMULT,NCOL,NROW,IOUT,NMLTAR,NZONAR,
     1                       RMLT,IZON,NML,NZN)
C
C-----VERSION 24SEPT2000 SGLO1BAS6RP
C     ******************************************************************
C     Read and prepare multiplier and zone arrays.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
      DIMENSION RMLT(NCOL,NROW,NMLTAR),IZON(NCOL,NROW,NZONAR)
      CHARACTER*200 LINE
      CHARACTER*20 RW
      CHARACTER*1 COP
      CHARACTER*24 ANAME
      CHARACTER*10 CTMP1,CTMP2
C     ------------------------------------------------------------------
C
C-----Read the multiplier array names and arrays
      IF(NML.GT.0) THEN
        DO 2000 M=1,NML
C  Read a line describing a multiplier array
          READ (INMULT,'(A)') LINE
C  Get the name of the new array
          LLOC=1
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INMULT)
C  Add new multiplier name into list
          MLTNAM(M)=LINE(ISTART:ISTOP)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INMULT)
          IF(LINE(ISTART:ISTOP).NE.'FUNCTION') THEN
C  Define array using array reader
             ANAME=' MULT. ARRAY: '//MLTNAM(M)
             CALL U2DREL(RMLT(1,1,M),ANAME,NROW,NCOL,0,INMULT,IOUT)
          ELSE
C  Define array as aritmetic combination of other multiplier arrays
C  Start by initializing the array to 0.
             WRITE(IOUT,10) MLTNAM(M)
   10        FORMAT(1X,/1X,'Calculated multiplier array: ',A)
             DO 20 I=1,NROW
             DO 20 J=1,NCOL
             RMLT(J,I,M)=0.
20           CONTINUE
C
C  Get the names of the multipliers and the operands
             READ (INMULT,'(A)') LINE
             LLOC=1
             NOP=0
   25        IF(NOP.EQ.0) THEN
C  No operator is specified before the first operand -- define it to be " "
                COP=' '
             ELSE
C  Get the operator that precedes each operand after the first operand.
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INMULT)
                IF(LINE(ISTART:ISTOP).EQ.'+' .OR.
     1             LINE(ISTART:ISTOP).EQ.'-' .OR.
     2             LINE(ISTART:ISTOP).EQ.'*' .OR.
     3             LINE(ISTART:ISTOP).EQ.'/') THEN
                   COP=LINE(ISTART:ISTOP)
                ELSE
                   GO TO 1000
                END IF
             END IF
             NOP=NOP+1
C  Get the operand
             CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,INMULT)
             WRITE(IOUT,37 ) COP,LINE(ISTART:ISTOP)
   37        FORMAT(1X,'                        ',A,' ARRAY ',A)
C  Lookup the operand in the list of existing multipliers
             DO 50 MM=1,M
               CTMP1=MLTNAM(MM)
               CALL UPCASE(CTMP1)
               CTMP2=LINE(ISTART:ISTOP)
               CALL UPCASE(CTMP2)
               IF(CTMP1.EQ.CTMP2) GO TO 60
   50        CONTINUE
             WRITE(IOUT,51) LINE(ISTART:ISTOP)
   51        FORMAT(1X,
     1        'ARRAY OPERAND HAS NOT BEEN PREVIOUSLY DEFINED:',A)
             CALL USTOP(' ')
C
C  Apply the + operator
   60        IF(COP.EQ.'+' .OR. COP.EQ.' ') THEN
                DO 100 I = 1, NROW
                DO 100 J = 1, NCOL
                  RMLT(J,I,M) = RMLT(J,I,M)+ RMLT(J,I,MM)
  100           CONTINUE
             ELSE IF(COP.EQ.'-') THEN
                DO 200 I = 1, NROW
                DO 200 J = 1, NCOL
                  RMLT(J,I,M) = RMLT(J,I,M)- RMLT(J,I,MM)
  200           CONTINUE
             ELSE IF(COP.EQ.'*') THEN
                DO 300 I = 1, NROW
                DO 300 J = 1, NCOL
                  RMLT(J,I,M) = RMLT(J,I,M)* RMLT(J,I,MM)
  300           CONTINUE
             ELSE
                DO 400 I = 1, NROW
                DO 400 J = 1, NCOL
                  RMLT(J,I,M) = RMLT(J,I,M)/ RMLT(J,I,MM)
  400           CONTINUE
             END IF
C  Get the next operator
             GO TO 25
C
C  Done defining the array -- print it.
C  Get the print code
1000          IPRN=0
              L=20-ISTOP+ISTART
              IF(L.GT.1)  THEN
                 RW=' '
                 RW(L:20)=LINE(ISTART:ISTOP)
                 READ(RW,'(I20)',ERR=1200) IPRN
              END IF
 1200         IF(IPRN.GE.0) THEN
                 ANAME=' MULT. ARRAY: '//MLTNAM(M)
                 CALL ULAPRWC(RMLT(1,1,M),NCOL,NROW,0,IOUT,IPRN,
     1                 ANAME)
              END IF
          END IF
 2000   CONTINUE
      ENDIF
C
C-----Read the zone array names and arrays
      IF(NZN.GT.0) THEN
         DO 3000 NZ=1,NZONAR
         READ(INZONE,'(A)') ZONNAM(NZ)
         CALL U2DINT(IZON(1,1,NZ),'  ZONE ARRAY: '//ZONNAM(NZ),
     1            NROW,NCOL,0,INZONE,IOUT)
 3000    CONTINUE
      END IF
C
      RETURN
      END
      SUBROUTINE SGLO1BAS6IPAR(ISEN,IOBS,IPES,ISENALL,ITMXP,IPAR,IOUT)
C
C-----VERSION 24SEPT2000 SGLO1BAS6IPAR
C     ******************************************************************
C     Define IPAR
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*70 CMODE(8)
      DATA (CMODE(I),I=1,8)/'FORWARD','FORWARD WITH OBSERVATIONS',
     &     'FORWARD WITH PARAMETER-VALUE SUBSTITUTION',
     &     'FORWARD WITH OBSERVATIONS AND PARAMETER-VALUE SUBSTITUTION',
     &     'PARAMETER SENSITIVITY',
     &     'PARAMETER SENSITIVITY WITH OBSERVATIONS',
     &     'SENSITIVITY ANALYSIS TO EVALUATE POTENTIAL FOR PARAMETER EST
     &IMATION',
     &     'PARAMETER ESTIMATION'/
C     ------------------------------------------------------------------
C
C-----DEFINE IPAR BASED ON ACTIVE PROCESSES, ISENALL, ITMXP
      IPAR = -3
      IF (IOBS.GT.0) THEN
C       OBS PROCESS IS ACTIVE
        WRITE(IOUT,490)
  490   FORMAT(/,' THE OBSERVATION PROCESS IS ACTIVE')
        IF (IPES.GT.0) THEN
C         PES PROCESS IS ACTIVE
          IF (ISEN.GT.0) THEN
C           SEN PROCESS IS ACTIVE
            IF (ISENALL.EQ.0) THEN
              WRITE(IOUT,500)
  500         FORMAT(' THE SENSITIVITY PROCESS IS ACTIVE')
            ELSEIF (ISENALL.GT.0) THEN
              WRITE(IOUT,505)
  505         FORMAT(' THE SENSITIVITY PROCESS IS ACTIVE, AND',
     &               ' ISENALL > 0')
            ELSE
              WRITE(IOUT,510)
  510         FORMAT(' THE SENSITIVITY PROCESS IS ACTIVE, BUT',
     &               ' ISENALL < 0')
            ENDIF
            IPAR = 1
          ELSE
C           SEN PROCESS IS INACTIVE
            WRITE(IOUT,520)
  520       FORMAT(' THE SENSITIVITY PROCESS IS INACTIVE')
            WRITE(IOUT,530)
  530       FORMAT(/' ERROR--WHEN PES PROCESS IS ACTIVE, BOTH SEN AND',
     &         ' OBS PROCESSES',/,
     &         ' MUST ALSO BE ACTIVE -- STOP EXECUTION (SGLO1BAS6IPAR)')
            CALL USTOP(' ')
          ENDIF
          IF (ITMXP.GT.0) THEN
            WRITE(IOUT,540)
  540       FORMAT(' THE PARAMETER-ESTIMATION PROCESS IS ACTIVE')
          ELSEIF (ITMXP.EQ.0) THEN
            WRITE(IOUT,550)
  550       FORMAT(' THE PARAMETER-ESTIMATION PROCESS IS ACTIVE,',
     &             ' BUT MAX-ITER = 0')
          ENDIF
        ELSEIF (ISEN.GT.0) THEN
C         SEN PROCESS IS ACTIVE AND PES PROCESS IS INACTIVE
          WRITE(IOUT,500)
          WRITE(IOUT,560)
  560     FORMAT(' THE PARAMETER-ESTIMATION PROCESS IS INACTIVE')
          IPAR = 0
        ELSE
C         SEN AND PES PROCESSES ARE INACTIVE
          WRITE(IOUT,520)
          WRITE(IOUT,560)
          IPAR = -1
        ENDIF
      ELSE
C       OBS PROCESS IS INACTIVE
        WRITE(IOUT,570)
  570   FORMAT(/,' THE OBSERVATION PROCESS IS INACTIVE')
        IF (IPES.GT.0) THEN
C         PES PROCESS IS ACTIVE
          WRITE(IOUT,530)
          CALL USTOP(' ')
        ELSEIF (ISEN.GT.0) THEN
C         SEN PROCESS IS ACTIVE AND PES PROCESS IS INACTIVE
          IF (ISENALL.GE.0) THEN
            WRITE(IOUT,500)
          ELSE
            WRITE(IOUT,510)
          ENDIF
          WRITE(IOUT,560)
          IPAR = -2
        ELSE
C         SEN AND PES PROCESSES ARE INACTIVE
          WRITE(IOUT,520)
          WRITE(IOUT,560)
        ENDIF
      ENDIF
C
C     DETERMINE MODE
      IF (IPAR.EQ.-3) THEN
        MODE = 1
      ELSEIF (IPAR.EQ.-2) THEN
        IF (ISENALL.LT.0) THEN
          MODE = 3
        ELSE
          MODE = 5
        ENDIF
      ELSEIF (IPAR.EQ.-1) THEN
        MODE = 2
      ELSEIF (IPAR.EQ.0) THEN
        IF (ISENALL.LT.0) THEN
          MODE = 4
        ELSE
          MODE = 6
        ENDIF
      ELSE
        IF (ISENALL.LT.0) THEN
          MODE = 4
        ELSEIF (ISENALL.GT.0) THEN
          MODE = 6
        ELSEIF (ITMXP.EQ.0) THEN
          MODE = 7
        ELSE
          MODE = 8
        ENDIF
      ENDIF
C
      LENG = NONB_LEN(CMODE(MODE),70)
      WRITE(IOUT,580) CMODE(MODE)(1:LENG)
  580 FORMAT(/,' MODE: ',A/)

      RETURN
      END
