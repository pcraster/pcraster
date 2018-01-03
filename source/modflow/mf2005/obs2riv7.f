      MODULE OBSRIVMODULE
         INTEGER, SAVE, POINTER  ::NQRV,NQCRV,NQTRV,IURVOBSV,IPRT
         INTEGER, SAVE, DIMENSION(:),   POINTER ::NQOBRV
         INTEGER, SAVE, DIMENSION(:),   POINTER ::NQCLRV
         INTEGER, SAVE, DIMENSION(:),   POINTER ::IOBTS
         REAL,    SAVE, DIMENSION(:),   POINTER ::FLWSIM
         REAL,    SAVE, DIMENSION(:),   POINTER ::FLWOBS
         REAL,    SAVE, DIMENSION(:),   POINTER ::TOFF
         REAL,    SAVE, DIMENSION(:),   POINTER ::OTIME
         REAL,    SAVE, DIMENSION(:,:), POINTER ::QCELL
         CHARACTER*12,SAVE,DIMENSION(:),POINTER ::OBSNAM
      TYPE OBSRIVTYPE
         INTEGER, POINTER  ::NQRV,NQCRV,NQTRV,IURVOBSV,IPRT
         INTEGER,     DIMENSION(:),   POINTER ::NQOBRV
         INTEGER,     DIMENSION(:),   POINTER ::NQCLRV
         INTEGER,     DIMENSION(:),   POINTER ::IOBTS
         REAL,        DIMENSION(:),   POINTER ::FLWSIM
         REAL,        DIMENSION(:),   POINTER ::FLWOBS
         REAL,        DIMENSION(:),   POINTER ::TOFF
         REAL,        DIMENSION(:),   POINTER ::OTIME
         REAL,        DIMENSION(:,:), POINTER ::QCELL
         CHARACTER*12,DIMENSION(:),POINTER ::OBSNAM
      END TYPE
      TYPE(OBSRIVTYPE),  SAVE   ::OBSRIVDAT(10)
      END MODULE
C  NQRV -- number of cell groups
C  NQCRV -- total number of cells in all groups
C  NQTRV -- total number of observations -- sum of the number of times for each group
C  NQOBRV(NQRV) -- The number of observations in each observation group
C  NQCLRV(NQRV) -- The number of cells in each observation group
C  IOBTS(NQTRV) -- Observation time step
C  FLWSIM(NQTRV) -- Simulated value
C  FLWOBS(NQTRV) -- Observed value
C  TOFF(NQTRV) -- Fractional offset between time steps
C  OTIME(NQTRV) -- Observation time in model time units
C  QCELL(4,NQCRV) -- Location and proportion factor for each observation cell


      SUBROUTINE OBS2RIV7AR(IURVOB,IURV,IGRID)
C     ******************************************************************
C     ALLOCATE MEMORY AND READ FLOW OBSERVATIONS AT RIVER CELLS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,  ONLY:IOUT,NPER,NSTP,PERLEN,TSMULT,ISSFLG,
     1                  NCOL,NROW,NLAY,ITRSS
      USE OBSRIVMODULE
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
      ALLOCATE(NQRV,NQCRV,NQTRV,IURVOBSV,IPRT)
C
      ZERO=0.0
      IERR=0
C  NT is the observation counter.
      NT=0
C  NC is the cell counter.
      NC=0
C
C     IDENTIFY PROCESS AND PACKAGE
      WRITE(IOUT,7) IURVOB
    7 FORMAT(/,' OBS2RIV7 -- OBSERVATION PROCESS (RIVER FLOW ',
     &    'OBSERVATIONS)',/,' VERSION 2, 02/28/2006',/,
     &    ' INPUT READ FROM UNIT ',I4)
C
Cx------Stop if GWFRIV is not active
      IF (IURV.EQ.0) THEN
        WRITE (IOUT,29 )
   29   FORMAT (/,' RIVER PACKAGE OF GWF IS NOT OPEN')
        CALL USTOP(' ')
        RETURN
      ENDIF
C
Cx------Read items 0 and 1.
      CALL URDCOM(IURVOB,IOUT,LINE)
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NQRV,DUM,IOUT,IURVOB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NQCRV,DUM,IOUT,IURVOB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NQTRV,DUM,IOUT,IURVOB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IURVOBSV,DUM,IOUT,IURVOB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,IDUM,DUM,IOUT,IURVOB)
      IPRT=1
      IF(LINE(ISTART:ISTOP).EQ.'NOPRINT') THEN
        IPRT=0
        WRITE(IOUT,*) 'NOPRINT option for RIVER OBSERVATIONS'
      END IF
      WRITE (IOUT,9) NQRV, NQCRV, NQTRV
    9 FORMAT (/,
     &     ' NUMBER OF FLOW-OBSERVATION RIVER-CELL GROUPS.....: ',I6,/,
     &     '   NUMBER OF CELLS IN RIVER-CELL GROUPS...........: ',I6,/,
     &     '   NUMBER OF RIVER-CELL FLOWS.....................: ',I6)
      IF(NQTRV.LE.0) THEN
         WRITE(IOUT,*) ' NUMBER OF OBSERVATIONS LESS THAN OR EQUAL TO 0'
         CALL USTOP(' ')
      END IF
      IF(IURVOBSV.GT.0) THEN
         WRITE(IOUT,21) IURVOBSV
   21    FORMAT(1X,
     1      'RIVER OBSERVATIONS WILL BE SAVED ON UNIT.........:',I7)
      ELSE
         WRITE(IOUT,22)
   22    FORMAT(1X,'RIVER OBSERVATIONS WILL NOT BE SAVED IN A FILE')
      END IF
C
Cx------Allocate memory
      ALLOCATE(NQOBRV(NQRV))
      ALLOCATE(NQCLRV(NQRV))
      ALLOCATE(IOBTS(NQTRV))
      ALLOCATE(FLWSIM(NQTRV))
      ALLOCATE(FLWOBS(NQTRV))
      ALLOCATE(TOFF(NQTRV))
      ALLOCATE(OTIME(NQTRV))
      ALLOCATE(QCELL(4,NQCRV))
      ALLOCATE(OBSNAM(NQTRV))
C
Cx------Initialize simulated equivalents
      DO 15 N=1,NQTRV
      FLWSIM(N)=ZERO
   15 CONTINUE
C
Cx------READ AND WRITE TIME-OFFSET MULTIPLIER FOR FLOW-OBSERVATION TIMES.
      READ(IURVOB,*) TOMULTRV
      IF(IPRT.NE.0) WRITE (IOUT,20) TOMULTRV
   20 FORMAT (/,' OBSERVED RIVER-CELL FLOW DATA',/,' -- TIME OFFSETS',
     &        ' ARE MULTIPLIED BY: ',G12.5)
C
Cx------LOOP THROUGH CELL GROUPS.
      DO 200 IQ = 1,NQRV
C
Cx------READ NUMBER OF OBSERVATIONS AND NUMBER OF CELLS FOR ONE GROUP
Cx------(ITEM 3).
        READ (IURVOB,*) NQOBRV(IQ), NQCLRV(IQ)
        IF(IPRT.NE.0) WRITE (IOUT,25) IQ, 'RIV', NQCLRV(IQ), NQOBRV(IQ)
   25   FORMAT (/,'   GROUP NUMBER: ',I6,'   BOUNDARY TYPE: ',A,
     &  '   NUMBER OF CELLS IN GROUP: ',I6,/,
     &  '   NUMBER OF FLOW OBSERVATIONS: ',I6,//,
     &  40X,'OBSERVED',/,
     &  20X,'REFER.',13X,'RIVER FLOW',/,
     &  7X,'OBSERVATION',2X,'STRESS',4X,'TIME',5X,'GAIN (-) OR',14X,/,
     &  2X,'OBS#    NAME',6X,'PERIOD   OFFSET',5X,'LOSS (+)')
C
Cx------SET FLAG FOR SETTING ALL PORTION FACTORS TO 1
        IFCTFLG = 0
        IF (NQCLRV(IQ).LT.0) THEN
          IFCTFLG = 1
          NQCLRV(IQ) = -NQCLRV(IQ)
        ENDIF
C
C
Cx------READ THE OBSERVATION NAMES, TIMES, AND MEASURED VALUES FOR
Cx------ONE CELL GROUP (ITEM 4)
        NT1 = NT + 1
        NT2 = NT + NQOBRV(IQ)
        DO 30 J = NT1, NT2
          READ (IURVOB,*) OBSNAM(J), IREFSP, TOFFSET, FLWOBS(J)
          IF(IPRT.NE.0) WRITE(IOUT,27 ) J,OBSNAM(J),IREFSP,TOFFSET,
     1                                  FLWOBS(J)
   27     FORMAT (I6,1X,A12,2X,I4,2X,G11.4,1X,G11.4)
          CALL UOBSTI(OBSNAM(J),IOUT,ISSFLG,ITRSS,NPER,NSTP,IREFSP,
     &                IOBTS(J),PERLEN,TOFF(J),TOFFSET,TOMULTRV,TSMULT,1,
     &                OTIME(J))
   30   CONTINUE
C
Cx------READ LAYER, ROW, COLUMN, AND FACTOR (ITEM 5) FOR EACH CELL IN
Cx------THE CELL GROUP.
        NC1 = NC + 1
        NC2 = NC + NQCLRV(IQ)
        IF(IPRT.NE.0) WRITE (IOUT,54)
   54   FORMAT (/,'       LAYER  ROW  COLUMN    FACTOR')
        DO 100 L = NC1, NC2
          READ (IURVOB,*) (QCELL(I,L),I=1,4)
          IF(IFCTFLG.EQ.1) QCELL(4,L) = 1.
          IF(IPRT.NE.0) WRITE (IOUT,55) (QCELL(I,L),I=1,4)
   55     FORMAT (4X,F8.0,F6.0,F7.0,F9.2)
          I = QCELL(2,L)
          J = QCELL(3,L)
          IF (J.LE.0 .OR. J.GT.NCOL .OR. I.LE.0 .OR. I.GT.NROW) THEN
            WRITE (IOUT,59)
   59       FORMAT (/,' ROW OR COLUMN NUMBER INVALID',
     &          ' -- STOP EXECUTION (OBS2RIV7AR)',/)
            IERR = 1
          ENDIF
  100   CONTINUE
C
Cx------END OF INPUT FOR ONE CELL GROUP -- UPDATE COUNTERS.
        NC = NC2
        NT = NT2
  200 CONTINUE
C
C
      IF (IERR.GT.0) THEN
        WRITE(IOUT,620)
  620 FORMAT (/,1X,'ERROR: SEARCH ABOVE FOR ERROR MESSAGE(S)',/,
     &' -- STOP EXECUTION (OBS2RIV7)')
        CALL USTOP(' ')
      ENDIF
C
Cx------RETURN.
      CALL SOBS2RIV7PSV(IGRID)
      RETURN
      END
      SUBROUTINE OBS2RIV7SE(IGRID)
C     ******************************************************************
C     CALCULATE SIMULATED EQUIVALENTS TO OBSERVED FLOWS FOR THE RIVER
C     PACKAGE
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IOUT,HNEW,IBOUND
      USE GWFRIVMODULE, ONLY:NRIVER,RIVR
      USE OBSBASMODULE,ONLY:ITS
      USE OBSRIVMODULE
      DOUBLE PRECISION HHNEW, C, HB , RBOT
C     ------------------------------------------------------------------
      CALL SGWF2RIV7PNT(IGRID)
      CALL SOBS2RIV7PNT(IGRID)
C
C-------INITIALIZE VARIABLES
      ZERO = 0.0
      NC = 0
      NT1 = 1
C
Cx------JRBOT IS FLAG FOR PRINTING THE HEADING FOR CELLS THAT ARE NOT
Cx------HEAD DEPENDENT.  THE FLAG IS USED TO PRINT THIS HEADING ONLY ONCE.
      JRBOT = 0
C
C-------LOOP THROUGH CELL GROUPS
      DO 800 IQ = 1, NQRV
        NT2 = NT1 + NQOBRV(IQ) - 1
C
Cx------LOOK THROUGH ALL OBSERVATIONS FOR THIS GROUP TO SEE FIND OBSERVATIONS
Cx------FOR THE CURRENT TIME STEP.
        DO 600 NT = NT1, NT2
          IF (IOBTS(NT).EQ.ITS .OR.
     &        (IOBTS(NT).EQ.ITS-1.AND.TOFF(NT).GT.ZERO)) THEN
C
Cx------FOUND AN OBSERVATION FOR CURRENT TIME STEP.
Cx------INITIALIZE NUMBER OF DRY CELLS IN OBSERVATION (KRBOT) AND
Cx------NUMBER OF NON-HEAD-DEPENDENT CELLS (IRBOT).
            IRBOT = 0
            KRBOT = 0
C
Cx------LOOP THROUGH CELLS IN THE CELL GROUP
            NC1 = NC + 1
            NC2 = NC + NQCLRV(IQ)
            NB = 0
            DO 400 N = NC1, NC2
              K = QCELL(1,N)
              I = QCELL(2,N)
              J = QCELL(3,N)
C
Cx------LOOP THROUGH ACTIVE RIVER REACHES TO FIND A MATCH.
              DO 100 MNB = 1, NRIVER
                NB = NB + 1
                IF (NB.GT.NRIVER) NB = 1
                KK = RIVR(1,NB)
                II = RIVR(2,NB)
                JJ = RIVR(3,NB)
C
Cx------DO SIMULATED EQUIVALENT CALCULATIONS IF THIS IS A MATCH.
                IF (I.EQ.II.AND.J.EQ.JJ.AND.K.EQ.KK) THEN
C
Cx------CHECK IF THE MATCHED REACH IS IN A DRY CELL.
                  IF (IBOUND(J,I,K).EQ.0) THEN
                    KRBOT = KRBOT + 1
                    GOTO 400
                  ENDIF
C
Cx------COMPUTE FLOW FOR THE REACH.
                  HHNEW = HNEW(J,I,K)
                  HB = RIVR(4,NB)
                  C = RIVR(5,NB)
                  RBOT = RIVR(6,NB)
                  FLWCEL = C*(HB-HHNEW)
                  IF(HHNEW.LE.RBOT) THEN
                    FLWCEL = C*(HB-RBOT)
                    IF (JRBOT.EQ.0) WRITE (IOUT,83 )
   83                 FORMAT (/,
     &                ' HEADS AT RIVER CELLS ARE BELOW THE',
     &                ' BOTTOM OF THE RIVER BED AT THE CELLS LISTED',/,
     &                ' BELOW.  THESE CONDITIONS DIMINISH THE IMPACT',
     &                ' OF THE OBSERVATION ON ESTIMATES OF',/,
     &       ' ALL PARAMETERS EXCEPT THOSE THAT CONTROL THE HYDRAULIC',
     &                ' CONDUCTIVITY OF THE',/,
     &                ' RIVER BED.  (SEE TEXT FOR MORE INFORMATION).')
                    JRBOT = 1
                    IF (IRBOT.EQ.0) THEN
                      WRITE (IOUT,92 ) NT, OBSNAM(NT), ITS
   92                 FORMAT (/,' OBS# ',I6,', ID ',A,', TIME STEP ',I5)
                      WRITE (IOUT,93 )
   93                 FORMAT ('    LAYER   ROW  COLUMN')
                    ENDIF
                    IRBOT = IRBOT + 1
                    WRITE (IOUT,97 ) K, I, J
   97               FORMAT(3I7)
                  ENDIF
C
Cx------CALCULATE THE FACTOR FOR TEMPORAL INTERPOLATION.
                  TFACT = 1.0
                  IF (TOFF(NT).GT.ZERO) THEN
                    IF (IOBTS(NT).EQ.ITS) TFACT = 1. - TOFF(NT)
                    IF (IOBTS(NT).EQ.ITS-1) TFACT = TOFF(NT)
                  ENDIF
C
Cx------ADD FLOW FOR THE REACH TO THE SIMULATED EQUIVALENT.
Cx------QCELL(4,N) IS THE PORTION FACTOR.
                  FLWSIM(NT) = FLWSIM(NT) + FLWCEL*TFACT*QCELL(4,N)
                  GO TO 400
                ENDIF
  100         CONTINUE
C
Cx------LOOKED THROUGH ENTIRE LIST OF ACTIVE RIVER REACHES WITHOUT
Cx------FINDING OBSERVATION CELL.  STOP.
              WRITE (IOUT,140) N, IQ, OBSNAM(NT),K,I,J
  140 FORMAT  (' CELL ',I6,
     1         ' OF RIVER OBSERVATION CELL GROUP',I5,/,
     2         ' NOT FOUND IN CELLS LISTED FOR RIVER PACKAGE',/,
     3         ' OBSERVATION NAME:',A,/,
     4         ' CELL LAYER, ROW, AND COLUMN:',3I8,/,
     5         '  -- STOP EXECUTION (OBS2RIV7SE)')
                CALL USTOP(' ')
C
Cx------END OF LOOP FOR THE CELLS IN ONE CELL GROUP FOR ONE OBSERVATION TIME..
  400       CONTINUE
C
C-------PRINT NUMBER OF CELLS AT WHICH HEAD IS BELOW THE BOTTOM OF THE
C-------RIVER BED; CHECK FOR ALL CELLS IN OBSERVATION BEING DRY.
            IF(IRBOT.GT.0) WRITE (IOUT,530) IRBOT, NQCLRV(IQ)
  530           FORMAT (I7,' OF THE',I7,' CELLS USED TO SIMULATE THE',
     &            ' GAIN OR LOSS ARE',/,22X,'AFFECTED.')
            IF(KRBOT.EQ.NQCLRV(IQ)) THEN
              WRITE (IOUT,535)
  535         FORMAT(' ALL CELLS INCLUDED IN THIS OBSERVATION ARE DRY')
            ENDIF
          ENDIF
C
Cx------END OF LOOP FOR OBSERVATION TIMES IN ONE CELL GROUP
  600   CONTINUE
C
C-------UPDATE COUNTERS
  700   NC = NC + NQCLRV(IQ)
        NT1 = NT2 + 1
C
Cx------END OF LOOP FOR ALL CELL GROUPS.
  800 CONTINUE
C
Cx------RETURN
      RETURN
      END
      SUBROUTINE OBS2RIV7OT(IGRID)
C     ******************************************************************
C     WRITE ALL OBSERVATIONS TO LISTING FILE.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY: IOUT
      USE OBSRIVMODULE
      DOUBLE PRECISION SQ,SUMSQ
C     ------------------------------------------------------------------
      CALL SOBS2RIV7PNT(IGRID)
C
C1------WRITE OBSERVATIONS TO LISTING FILE.
      IF(IPRT.NE.0) WRITE(IOUT,17)
   17 FORMAT(1X,/,1X,'RIVER FLOW OBSERVATIONS',/,
     1  1X,'OBSERVATION       OBSERVED           SIMULATED',/
     2  1X,'  NAME              VALUE              VALUE',
     3     '             DIFFERENCE',/
     4  1X,'----------------------------------------------',
     5     '----------------------')
      SUMSQ=0.
      DO 100 N=1,NQTRV
      DIFF=FLWOBS(N)-FLWSIM(N)
      SQ=DIFF*DIFF
      SUMSQ=SUMSQ+SQ
      IF(IPRT.NE.0) WRITE(IOUT,27) OBSNAM(N),FLWOBS(N),FLWSIM(N),DIFF
   27 FORMAT(1X,A,1P,3G20.11)
  100 CONTINUE
      WRITE(IOUT,28) SUMSQ
   28 FORMAT(1X,/,1X,'RIV FLOW SUM OF SQUARED DIFFERENCE:',1P,E15.5)
C
C2------WRITE OBSERVATIONS TO SEPARATE FILE.
      IF(IURVOBSV.GT.0) CALL UOBSSV(IURVOBSV,NQTRV,FLWSIM,
     1                      FLWOBS,OBSNAM,0)
C
C3------RETURN.
      RETURN
      END
      SUBROUTINE OBS2RIV7DA(IGRID)
C  Deallocate OBSRIV memory
      USE OBSRIVMODULE
C
      CALL SOBS2RIV7PNT(IGRID)
      DEALLOCATE(NQRV)
      DEALLOCATE(NQCRV)
      DEALLOCATE(NQTRV)
      DEALLOCATE(IURVOBSV)
      DEALLOCATE(IPRT)
      DEALLOCATE(NQOBRV)
      DEALLOCATE(NQCLRV)
      DEALLOCATE(IOBTS)
      DEALLOCATE(FLWSIM)
      DEALLOCATE(FLWOBS)
      DEALLOCATE(TOFF)
      DEALLOCATE(OTIME)
      DEALLOCATE(QCELL)
      DEALLOCATE(OBSNAM)
C
      RETURN
      END
      SUBROUTINE SOBS2RIV7PNT(IGRID)
C  Change OBSRIV data to a different grid.
      USE OBSRIVMODULE
C
      NQRV=>OBSRIVDAT(IGRID)%NQRV
      NQCRV=>OBSRIVDAT(IGRID)%NQCRV
      NQTRV=>OBSRIVDAT(IGRID)%NQTRV
      IURVOBSV=>OBSRIVDAT(IGRID)%IURVOBSV
      IPRT=>OBSRIVDAT(IGRID)%IPRT
      NQOBRV=>OBSRIVDAT(IGRID)%NQOBRV
      NQCLRV=>OBSRIVDAT(IGRID)%NQCLRV
      IOBTS=>OBSRIVDAT(IGRID)%IOBTS
      FLWSIM=>OBSRIVDAT(IGRID)%FLWSIM
      FLWOBS=>OBSRIVDAT(IGRID)%FLWOBS
      TOFF=>OBSRIVDAT(IGRID)%TOFF
      OTIME=>OBSRIVDAT(IGRID)%OTIME
      QCELL=>OBSRIVDAT(IGRID)%QCELL
      OBSNAM=>OBSRIVDAT(IGRID)%OBSNAM
C
      RETURN
      END
      SUBROUTINE SOBS2RIV7PSV(IGRID)
C  Save OBSRIV data for a grid.
      USE OBSRIVMODULE
C
      OBSRIVDAT(IGRID)%NQRV=>NQRV
      OBSRIVDAT(IGRID)%NQCRV=>NQCRV
      OBSRIVDAT(IGRID)%NQTRV=>NQTRV
      OBSRIVDAT(IGRID)%IURVOBSV=>IURVOBSV
      OBSRIVDAT(IGRID)%IPRT=>IPRT
      OBSRIVDAT(IGRID)%NQOBRV=>NQOBRV
      OBSRIVDAT(IGRID)%NQCLRV=>NQCLRV
      OBSRIVDAT(IGRID)%IOBTS=>IOBTS
      OBSRIVDAT(IGRID)%FLWSIM=>FLWSIM
      OBSRIVDAT(IGRID)%FLWOBS=>FLWOBS
      OBSRIVDAT(IGRID)%TOFF=>TOFF
      OBSRIVDAT(IGRID)%OTIME=>OTIME
      OBSRIVDAT(IGRID)%QCELL=>QCELL
      OBSRIVDAT(IGRID)%OBSNAM=>OBSNAM
C
      RETURN
      END
