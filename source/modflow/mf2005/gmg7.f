C--------------------------------------------------------------------
C Requires the Fortran 2003 ISO_C_BINDING Intrinsic Module
C--------------------------------------------------------------------
      MODULE GMGMODULE
        USE ISO_C_BINDING
        INTEGER,SAVE,POINTER  ::IITER,IADAMPGMG,ISM,ISC,IOUTGMG
        INTEGER,SAVE,POINTER  ::ISIZ,IPREC,IIOUT
        INTEGER,SAVE,POINTER  ::SITER,TSITER
        TYPE ( C_PTR ),SAVE   ::GMGID
        INTEGER,SAVE,POINTER  ::IUNITMHC
        REAL   ,SAVE,POINTER  ::HCLOSEGMG,RCLOSEGMG,DAMPGMG
        REAL   ,SAVE,POINTER  ::DUP,DLOW,CHGLIMIT
        REAL   ,SAVE,POINTER,DIMENSION(:,:,:)::HNEWLAST
        DOUBLE PRECISION,SAVE,POINTER :: BIGHEADCHG
        DOUBLE PRECISION,SAVE,POINTER  :: RELAXGMG
      TYPE GMGTYPE
        INTEGER,POINTER  ::IITER,IADAMPGMG,ISM,ISC,IOUTGMG
        INTEGER,POINTER  ::ISIZ,IPREC,IIOUT
        INTEGER,POINTER  ::SITER,TSITER
        TYPE ( C_PTR )   ::GMGID
        INTEGER,POINTER  ::IUNITMHC
        REAL   ,POINTER  ::HCLOSEGMG,RCLOSEGMG,DAMPGMG
        REAL   ,POINTER  ::DUP,DLOW,CHGLIMIT
        REAL   ,POINTER,DIMENSION(:,:,:)::HNEWLAST
        DOUBLE PRECISION,POINTER :: BIGHEADCHG
        DOUBLE PRECISION,POINTER  :: RELAXGMG
      END TYPE
      TYPE(GMGTYPE), SAVE ::GMGDAT(10)
      
      INTERFACE 
        SUBROUTINE GMG7AP(HNEW,RHS,CR,CC,CV,HCOF,HNOFLO,IBOUND,
     &                    IITER,MXITER,RCLOSE,HCLOSE,
     &                    KITER,KSTP,KPER,NCOL,NROW,NLAY,
     &                    ICNVG,SITER,TSITER,DAMP,IADAMP,
     &                    IOUTGMG,IOUT,GMGID,
     &                    IUNITMHC,DUP,DLOW,CHGLIMIT,BIGHEADCHG,
     &                    HNEWLAST)  
          USE ISO_C_BINDING
          USE GMG_C_INTERFACE
          IMPLICIT NONE
          REAL RHS(*),CR(*),CC(*),CV(*),HCOF(*),HNEWLAST(*)
          TARGET RHS, CR, CC, CV, HCOF
          REAL HNOFLO,RCLOSE,HCLOSE,DAMP
          TARGET HNOFLO
          REAL DUP,DLOW,CHGLIMIT
          DOUBLE PRECISION BIGHEADCHG
          DOUBLE PRECISION HNEW(*)
          INTEGER IBOUND(*)
          INTEGER MXITER,IITER,KITER,KSTP,KPER,NCOL,NROW,NLAY,ICNVG,
     1            IOUTGMG,IOUT
          TYPE ( C_PTR ) :: GMGID
          INTEGER SITER,TSITER
          INTEGER IADAMP,IUNITMHC
        END SUBROUTINE GMG7AP
      END INTERFACE
      
      END MODULE GMGMODULE
C
      SUBROUTINE GMG7AR(IN,MXITER,IGRID)
C--------------------------------------------------------------------
C     READS INPUT FROM FILE TYPE GMG SPECIFIED IN NAME FILE
C     ALLOCATES GMG SOLVER
C     EXPLICIT DECLARATIONS
C--------------------------------------------------------------------
      USE GLOBAL,   ONLY:IOUT,NCOL,NROW,NLAY
      USE GMGMODULE,ONLY:IITER,IADAMPGMG,ISM,ISC,IOUTGMG,ISIZ,
     1                   IPREC,IIOUT,SITER,TSITER,GMGID,HCLOSEGMG,
     2                   RCLOSEGMG,DAMPGMG,RELAXGMG,
     3                   IUNITMHC,DUP,DLOW,CHGLIMIT,HNEWLAST,
     4                   BIGHEADCHG
      USE GMG_C_INTERFACE
      IMPLICIT NONE
      CHARACTER*200 LINE
      INTEGER IN,MXITER,IGRID,IERR,ICOL,NDUM,ISTOP,ISTART
      REAL    RDUM
C
C--------------------------------------------------------------------
C     ALLOCATE POINTERS
C--------------------------------------------------------------------
      ALLOCATE(IITER,IADAMPGMG,ISM,ISC,IOUTGMG,ISIZ,IPREC,IIOUT,
     1         SITER,TSITER,IUNITMHC)
      ALLOCATE(HCLOSEGMG,RCLOSEGMG,DAMPGMG,RELAXGMG)
      ALLOCATE(DUP,DLOW,CHGLIMIT)
      ALLOCATE(BIGHEADCHG)
C
C--------------------------------------------------------------------
C     READ INPUT FILE
C--------------------------------------------------------------------
      CALL URDCOM(IN,IOUT,LINE)
      READ(LINE,*) RCLOSEGMG,IITER,HCLOSEGMG,MXITER
C
      CALL URDCOM(IN,IOUT,LINE)
      ICOL = 1
      CALL URWORD(LINE,ICOL,ISTART,ISTOP,3,NDUM,DAMPGMG,IOUT,IN)
      CALL URWORD(LINE,ICOL,ISTART,ISTOP,2,IADAMPGMG,RDUM,IOUT,IN)
      CALL URWORD(LINE,ICOL,ISTART,ISTOP,2,IOUTGMG,RDUM,IOUT,IN)
      IUNITMHC = 0
      NDUM = -1
      CALL URWORD(LINE,ICOL,ISTART,ISTOP,2,NDUM,RDUM,-1,IN)
      IF (NDUM>0) IUNITMHC = NDUM
C
      DUP=0.
      DLOW=0.
      CHGLIMIT=0.
      CALL URDCOM(IN,IOUT,LINE)
      IF(IADAMPGMG.EQ.0 .OR. IADAMPGMG.EQ.1) THEN
        READ(LINE,*) ISM,ISC
      ELSE IF(IADAMPGMG.EQ.2) THEN
        READ(LINE,*) ISM,ISC,DUP,DLOW,CHGLIMIT
      ELSE
        WRITE(IOUT,400)
  400   FORMAT(/,1X,'ERROR IN GMG INPUT: IADAMP MUST BE ONE OF 0, 1,',
     1    ' OR 2 (GMG1ALG)')
        CALL USTOP(' ')
      END IF
C
      IIOUT=IOUT
      IF(IOUTGMG .GT. 2) IIOUT=6
C
      SITER=0
      TSITER=0
      RELAXGMG=0.0D0
      IF(ISC .EQ. 4) THEN
        CALL URDCOM(IN,IOUT,LINE)
        READ(LINE,*) RELAXGMG
      END IF
C
      IF(DAMPGMG .LE. 0.0 .OR. DAMPGMG .GT. 1.0) DAMPGMG=1.0
C
C--------------------------------------------------------------------
C     ALLOCATE
C--------------------------------------------------------------------
      IF(IUNITMHC.GT.0 .OR. IADAMPGMG.EQ.2) THEN
        ALLOCATE(HNEWLAST(NCOL,NROW,NLAY))
      ELSE
        ALLOCATE(HNEWLAST(1,1,1))
      END IF
C
C---- CHECK FOR FORCED DOUBLE PRECISION
C
      IPREC=0
      IF(KIND(DAMPGMG) .EQ. 8) IPREC=1
C
      CALL MF2KGMG_ALLOCATE(GMGID,NCOL,NROW,NLAY,IPREC,ISM,ISC,
     &                      RELAXGMG,ISIZ,IERR)
      IF(IERR .NE. 0) THEN
        CALL USTOP('ALLOCATION ERROR IN SUBROUTINE GMG1ALG')
      END IF
C
      WRITE(IIOUT,500) RCLOSEGMG,IITER,HCLOSEGMG,MXITER,
     &                 DAMPGMG,IADAMPGMG,IOUTGMG,
     &                 ISM,ISC,RELAXGMG
C
      IF(IADAMPGMG==1) WRITE(IIOUT,510)
      IF(IADAMPGMG==2) THEN
        WRITE(IIOUT,512)
        WRITE(IIOUT,513)DUP,DLOW,CHGLIMIT
      ENDIF
      IF(ISM .EQ. 0) WRITE(IIOUT,520)
      IF(ISM .EQ. 1) WRITE(IIOUT,525)
      IF(ISC .EQ. 0) WRITE(IIOUT,530)
      IF(ISC .EQ. 1) WRITE(IIOUT,531)
      IF(ISC .EQ. 2) WRITE(IIOUT,532)
      IF(ISC .EQ. 3) WRITE(IIOUT,533)
      IF(ISC .EQ. 4) WRITE(IIOUT,534)
      IF(IUNITMHC.GT.0) WRITE(IOUT,501) IUNITMHC
C
      WRITE(IIOUT,540) ISIZ
C
C--------------------------------------------------------------------
C     FORMAT STATEMENTS
C--------------------------------------------------------------------
  500 FORMAT(1X,'-------------------------------------------------',/,
     &       1X,'GMG -- PCG GEOMETRIC MULTI-GRID SOLUTION PACKAGE:',/,
     &       1X,'-------------------------------------------------',/,
     &       1X,'RCLOSE  = ',1P,E8.2,'; INNER CONVERGENCE CRITERION',/,
     &       1X,'IITER   = ',I8,'; MAX INNER ITERATIONS            ',/,
     &       1X,'HCLOSE  = ',1P,E8.2,'; OUTER CONVERGENCE CRITERION',/,
     &       1X,'MXIITER = ',I8,'; MAX OUTER ITERATIONS            ',/,
     &       1X,'DAMP    = ',1P,E8.2,'; DAMPING PARAMETER          ',/,
     &       1X,'IADAMP  = ',I8,'; ADAPTIVE DAMPING FLAG           ',/,
     &       1X,'IOUTGMG = ',I8,'; OUTPUT CONTROL FLAG             ',/,
     &       1X,'ISM     = ',I8,'; SMOOTHER FLAG                   ',/,
     &       1X,'ISC     = ',I8,'; COARSENING FLAG                 ',/,
     &       1X,'RELAX   = ',1P,E8.2,'; RELAXATION PARAMETER       ',/,
     &       1X,"-------------------------------------------------")
C
  501 FORMAT(1X,'Head change will be saved on unit',I5)
  510 FORMAT(1X,"COOLEY'S ADAPTIVE DAMPING METHOD IMPLEMENTED")
  512 FORMAT(1X,'RELATIVE REDUCED RESIDUAL ADAPTIVE DAMPING METHOD',
     1    ' WILL BE USED')
  513 FORMAT(5X,'WITH DUP = ',G10.3,' DLOW = ',G10.3,' AND CHGLIMIT = ',
     1     G10.3)
  520 FORMAT(1X,'ILU SMOOTHING IMPLEMENTED')
  525 FORMAT(1X,'SGS SMOOTHING IMPLEMENTED')
C
  530 FORMAT(1X,'FULL COARSENING')
  531 FORMAT(1X,'COARSENING ALONG COLUMNS AND ROWS ONLY')
  532 FORMAT(1X,'COARSENING ALONG ROWS AND LAYERS ONLY')
  533 FORMAT(1X,'COARSENING ALONG COLUMNS AND LAYERS ONLY')
  534 FORMAT(1X,'NO COARSENING')
C
  540 FORMAT(1X,'-------------------------------------------------',/,
     &       1X,I4,' MEGABYTES OF MEMORY ALLOCATED BY GMG',/,
     &       1X,'-------------------------------------------------',/)
C
      CALL GMG7PSV(IGRID)
      RETURN
      END
C***********************************************************************
      SUBROUTINE GMG7AP(HNEW,RHS,CR,CC,CV,HCOF,HNOFLO,IBOUND,
     &                  IITER,MXITER,RCLOSE,HCLOSE,
     &                  KITER,KSTP,KPER,NCOL,NROW,NLAY,
     &                  ICNVG,SITER,TSITER,DAMP,IADAMP,
     &                  IOUTGMG,IOUT,GMGID,
     &                  IUNITMHC,DUP,DLOW,CHGLIMIT,BIGHEADCHG,
     &                  HNEWLAST)
C***********************************************************************
C     GMG7AP CALLS THE FOLLOWING FUNCTIONS FROM THE GMG LIBRARY:
C
C    MF2KMG_ASSEMBLE:
C      -- ASSEMBLES CELL-CENTERED FINITE DIFFERENCE
C         MATRIX AND MULTIGRID PRECONDITIONED CONJUGATE
C         GRADIENT SOLVER.  RETURNS L2-NORM OF RESIDUAL BIGR0
C
C    MF2KMG_EVAL:
C      -- APPROXIMATES THE HEAD CHANGE E FOR A*E=R WHERE A IS THE
C         CCFD MATRIX, AND R IS THE INITIAL RESIDUAL.
C         RETURNS THE L2-NORM OF THE RESIDUAL R-A*E.
C
C    MF2KGMG_BIGH:
C      -- COMPUTES MAX HEAD CHANGE BIGH AND RETURNS LOCATION
C         (COL,ROW,LAY) OF MAX HEAD CHANGE.  ABSOLUTE VALUE
C         OF BIGH IS MAX-NORM OF HEAD CHANGE.
C
C    MF2KMG_UPDATE:
C      -- ADDS THE CORRECTION TO THE HEADS HNEW=HNEW+DAMP*E.
C--------------------------------------------------------------------
      USE ISO_C_BINDING
      USE GMG_C_INTERFACE
      IMPLICIT NONE
      REAL RHS(*),CR(*),CC(*),CV(*),HCOF(*),HNEWLAST(*)
      TARGET RHS, CR, CC, CV, HCOF
      REAL HNOFLO,RCLOSE,HCLOSE,DAMP
      TARGET HNOFLO
      REAL DUP,DLOW,CHGLIMIT
      DOUBLE PRECISION BIGHEADCHG
      DOUBLE PRECISION HNEW(*)
      INTEGER IBOUND(*)
      INTEGER MXITER,IITER,KITER,KSTP,KPER,NCOL,NROW,NLAY,ICNVG,
     1        IOUTGMG,IOUT
      TYPE ( C_PTR ) :: GMGID
      INTEGER SITER,TSITER
      INTEGER IADAMP,IUNITMHC
C
      INTEGER IIOUT
      DOUBLE PRECISION DRCLOSE
C
      INTEGER ITER,IERR
      INTEGER JBIGH,IBIGH,KBIGH
C
      DOUBLE PRECISION BIGH,BIGR,BIGR0
      DOUBLE PRECISION S,DH,DAMP0
      DOUBLE PRECISION, SAVE ::BIGH0
      DOUBLE PRECISION, SAVE ::DDAMP
      DOUBLE PRECISION       ::RSQ
      REAL                   ::DAMPA,BIGHA
      
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C  Save HNEW if MHC is being used.
C--------------------------------------------------------------------
      IF (IADAMP.EQ.2 .OR. IUNITMHC.GT.0) CALL MHC7IT(NCOL,NROW,NLAY,
     1                  HNEW,HNEWLAST)
C
C---- INITIALIZE VARIABLES
C
      ICNVG=0
      IIOUT=IOUT
      IF(IOUTGMG .GT. 2) IIOUT=6
      IF(KITER .EQ. 1) DDAMP=DAMP
      DAMP0=DDAMP
C
C--------------------------------------------------------------------
C     ASSEMBLE SOLVER
C--------------------------------------------------------------------
      CALL MF2KGMG_ASSEMBLE(GMGID,BIGR0,C_LOC(CR),C_LOC(CC),
     &                      C_LOC(CV),C_LOC(HCOF),HNEW,C_LOC(RHS),
     &                      C_LOC(HNOFLO),IBOUND,IERR)
      IF(IERR .NE. 0) THEN
        CALL USTOP('GMG ASSEMBLY ERROR IN SUBROUTINE GMG1AP')
      END IF
C
C--------------------------------------------------------------------
C     SCALE CLOSURE CRITERION FOR INNER ITERATION BASED ON CURRENT
C     VALUE OF DAMPING AND INITIAL RESIDUAL.
C--------------------------------------------------------------------
      DRCLOSE=DDAMP*RCLOSE+(1.0D0-DDAMP)*BIGR0
      IF (IADAMP==2 .AND. DDAMP < 0.5D0) THEN
        DRCLOSE=RCLOSE
      ENDIF
C
C--------------------------------------------------------------------
C     COMPUTE HEAD CHANGE
C--------------------------------------------------------------------
      CALL MF2KGMG_EVAL(GMGID,ITER,BIGR,DRCLOSE,IITER,IOUTGMG,IIOUT)
      SITER=SITER+ITER
C
C--------------------------------------------------------------------
C     COMPUTE MAX HEAD CHANGE
C--------------------------------------------------------------------
      CALL MF2KGMG_BIGH(GMGID,BIGH,JBIGH,IBIGH,KBIGH)
C
C--------------------------------------------------------------------
C     CHECK FOR CLOSURE
C--------------------------------------------------------------------
       IF(MXITER .EQ. 1 .AND. BIGR .LE. RCLOSE) THEN
         DDAMP=1.0;
         ICNVG=1
         GOTO 100
       END IF
C
       IF(ABS(BIGH) .LE. HCLOSE .AND. BIGR .LE. RCLOSE) THEN
         DDAMP=1.0;
         ICNVG=1
         GOTO 100
       END IF
C
C--------------------------------------------------------------------------
C     ADJUST DAMPING PARAMETER
C--------------------------------------------------------------------------
      IF(IADAMP .EQ. 1) THEN
C  Cooley's Method
        IF(KITER .GT. 1) THEN
          DH=BIGH/BIGH0
          S=DH/DDAMP
          IF(S .GE. -1.0D0) THEN
            DDAMP=(3.0D0+S)/(3.0D0+ABS(S))
          ELSE
            DDAMP=0.5D0/ABS(S)
          END IF
          IF(DDAMP .LT. DAMP) DDAMP=DAMP
        END IF
      ELSE IF(IADAMP.EQ.2) THEN
C  Relative Reduced Residual Method
        RSQ=BIGR
        DAMPA=DAMP
        BIGHA=BIGH
        CALL GMG1ADAMP2(RSQ,DDAMP,DAMPA,DUP,DLOW,KITER,BIGHA,IOUT,
     1                  CHGLIMIT,BIGHEADCHG)
      END IF
C
C--------------------------------------------------------------------
C     ADD CORRECTION
C--------------------------------------------------------------------
  100 CONTINUE
      CALL MF2KGMG_UPDATE(GMGID,HNEW,DDAMP)
      BIGH0=BIGH
C
      IF(IOUTGMG .NE. 0) THEN
        WRITE(IIOUT,510) ITER,DDAMP,BIGR,
     &                   ABS(BIGH),JBIGH,IBIGH,KBIGH
        IF(ICNVG .EQ. 1 ) THEN
          TSITER=TSITER+SITER
          WRITE(IIOUT,500) KSTP,KPER,KITER,SITER,TSITER
          SITER=0
        END IF
      END IF
C
C--------------------------------------------------------------------
C  Call MHC to Compute Head Change if MHC is active
C--------------------------------------------------------------------
      IF(IADAMP.EQ.2 .OR. IUNITMHC.GT.0)
     1  CALL MHC7AP(IUNITMHC,KITER,KSTP,KPER,NCOL,NROW,NLAY,IBOUND,
     2            HNEW,HNEWLAST,DDAMP,BIGHEADCHG)
C
C--------------------------------------------------------------------
C     FORMAT STATEMENTS
C--------------------------------------------------------------------
C
  500 FORMAT(1X,'-------------------------------',/,
     &       1X,'TIME STEP            : ',I6,/,
     &       1X,'STRESS PERIOD        : ',I6,/,
     &       1X,'GMG CALLS            : ',I6,/,
     &       1X,'PCG ITERATIONS       : ',I6,/,
     &       1X,'-------------------------------',/,
     &       1X,'TOTAL PCG ITERATIONS : ',I6,/,
     &       1X,'-------------------------------',/)
C
  510 FORMAT(1X,'-------------------------------------',
     &          '--------------------',/,
     &       1X,'PCG ITERATIONS                    : ',I4,/,
     &       1X,'DAMPING                           : ',0P,F5.3,/,
     &       1X,'L2-NORM OF RESIDUAL               : ',1P,E10.4,/,
     &       1X,'MAX HEAD CHANGE                   : ',1P,E10.4,/,
     &       1X,'MAX HEAD CHANGE AT (COL,ROW,LAY)  : (',
     &           I6,',',I6,',',I6,')',/,
     &       1X,'-------------------------------------',
     &          '--------------------',/)
C
      RETURN
      END

C***********************************************************************
C     SUBROUTINE RESPRINT IS CALLED FROM THE SOLVER FOR OUTPUT
C     OF REDUCTION HISTORY.
C
C     THE ITERATION (I), THE RESIDUAL (RES), AND THE
C     CONVERGENCE FACTOR (CFAC) ARE PRINTED.
C***********************************************************************
      SUBROUTINE RESPRINT(IOUT,I,RES,CFAC) BIND ( C, NAME='RESPRINT' )
      USE ISO_C_BINDING
      INTEGER ( C_INT    ), INTENT ( IN    ) :: IOUT
      INTEGER ( C_INT    ), INTENT ( IN    ) :: I
      REAL    ( C_DOUBLE ), INTENT ( IN    ) :: RES
      REAL    ( C_DOUBLE ), INTENT ( IN    ) :: CFAC
C
C---- PRINT RESIDUALS
C
      WRITE(IOUT,100) I,RES,CFAC
C
C--------------------------------------------------------------------
C     FORMAT STATEMENTS
C--------------------------------------------------------------------
  100 FORMAT(1X,'ITER:',I4,
     &       2X,'RES: ',1P,E10.4,
     &       2X,'CFAC: ',0P,F5.3)
C
      RETURN
      END
C
      SUBROUTINE GMG1ADAMP2(RSQ1,DDAMP,DAMP,DUP,DLOW,KITER,BIGHPCG,
     &                      IOUT,CHGLIMIT,BIGHEADCHG)
C
C-----MODIFIED FROM LMG (MEHL & HILL, 2001, USGS OFR 01-177), SUBROUTINE
C     ADAMP2 VERSION 1.1 22JUL2002.  MODIFICATIONS ARE DOCUMENTED IN
C     BANTA, 2006, IGWMC, "MODFLOW AND MORE, 2006" CONFERENCE PROCEEDINGS,
C     P. 596-600.
C     ******************************************************************
C     THIS SUBROUTINE CALCULATES THE DAMPING PARAMETER USING THE
C     RESIDUAL REDUCTION METHOD, AS MODIFIED BY BANTA, 2006.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      IMPLICIT NONE
      !   Argument list variables
      INTEGER,          INTENT(IN)    :: IOUT, KITER
      REAL,             INTENT(IN)    :: BIGHPCG, CHGLIMIT, DLOW, DUP
      DOUBLE PRECISION, INTENT(IN)    :: BIGHEADCHG
      REAL,             INTENT(INOUT) :: DAMP
      DOUBLE PRECISION, INTENT(INOUT) :: RSQ1, DDAMP
      !   Local variables
      LOGICAL          :: OSCIL,OSCILH,OSCILLATING
      DOUBLE PRECISION :: RSQ2,RSQ3,DDLOW,DDUP
      DOUBLE PRECISION :: BIGH1,BIGH2,BIGH3, BIGHABS, DDAMPTMP
      DOUBLE PRECISION :: DDLOWINTRL, DDUPINTRL, HCINC, HCDEC
      INTEGER          :: IPER, IMULT, ICONST, NRAN
      INTEGER          :: KSMALLERINC, KBIGGERINC, KSMALLERDEC,
     &                    KBIGGERDEC, KSMALLERDAMP
      REAL             :: DAMP3, RRED, OSTERM, QRAND
      !
      DATA ICONST, IMULT, IPER /1283,106,6075/
      SAVE BIGH1, BIGH2, BIGH3
      SAVE RSQ2,RSQ3,NRAN,DAMP3
      SAVE KSMALLERINC, KBIGGERINC, KSMALLERDEC, KBIGGERDEC, DDLOWINTRL,
     &     DDUPINTRL, KSMALLERDAMP
      SAVE OSCILLATING, HCINC, HCDEC
C1
C1------CALCULATE RESIDUAL AND INITIALIZE VARIABLES FOR FIRST ITERATION
      RSQ1=DSQRT(RSQ1)
      OSCIL = .FALSE.
      OSCILH = .FALSE.
      IF(KITER .EQ. 1)THEN
        RSQ3=2.*RSQ1+1.
        DAMP3=DAMP
        NRAN=1
        BIGH3=BIGHEADCHG
        BIGH2=BIGHEADCHG
        BIGH1=BIGHEADCHG
        OSCILLATING=.FALSE.
        DDLOWINTRL=DLOW
        DDUPINTRL=DUP
        KSMALLERINC=0
        KBIGGERINC=0
        KSMALLERDEC=0
        KBIGGERDEC=0
        HCINC=1.0D8
        HCDEC=-1.0D8
        KSMALLERDAMP=0
      ELSE
C2------CALCULATE THE RELATIVE REDUCTION IN THE RESIDUAL FOR THIS
C2------ITERATION ("RRED") SCALED BY THE DAMPING.
C2A-----IF THE REDUCTION IS GREATER THAN 50%, THEN USE THAT RELATIVE
C2A-----REDUCTION AS THE DAMPING VALUE FOR THE NEXT ITERATION.  THE IDEA
C2A-----HERE IS WHEN WE ARE IN A LINEAR PART OF THE SOLUTION, THE "RRED"
C2A-----WILL MOVE TOWARDS VALUES NEAR OR > 1, AND LITTLE OR NO DAMPING
C2A-----WILL BE APPLIED, WHICH IS APPROPRIATE.
        BIGH3=BIGH2
        BIGH2=BIGH1
        BIGH1=BIGHEADCHG
        RRED=((RSQ2-RSQ1)/RSQ2)/DDAMP
        IF(RRED .GT. 0.5)THEN
c          DDAMP=RRED
c check if we've had 2 successive lowerings of the residuals
          IF(RSQ3 .GT. RSQ2 .AND. RSQ2 .GT. RSQ1)THEN
            DDAMP=0.5*(DDAMP + MIN(DUP,RRED)) ! increase DAMP slower
            IF(DDAMP .LT. DLOW) DDAMP=DLOW
c check if we were doing better before.  If so, use previous damping
          ELSEIF(RSQ3 .LT. RSQ2)THEN
            DDAMP = DAMP3
          ENDIF
        ELSEIF (RRED<0.0) THEN
          !  Solution is diverging--set damp to DLOW
          DDAMP=DLOW
        ELSE
C2B-----THE RELATIVE REDUCTION IS LESS THAN 50%, INDICATING THE
C2B-----SOLUTION IS NOT PROGRESSING ADEQUATELY.  BASED ON HOW FAR FROM
C2B-----"RRED" IS FROM 50%, THE DAMPING IS MORE OR LESS AGGRESSIVE.
C2B-----NOTE: FOR THIS CASE, THE MAXIMUM VALUE OF DAMP WILL BE 0.3 + DLOW
C2B-----SO SETTING DLOW=0.2 IS APPROPRIATE SO DAMP WON'T EXCEED 0.5
          DDAMP=0.075/(0.75-RRED) + DLOW
        ENDIF
C3
C3-----CHECK TO SEE IF SOLUTION IS OSCILLATING BY CHECKING THE
C3-----RELATIVE DIFFERENCE BETWEEN THE CURRENT RESIDUAL AND THE
C3-----RESIDUAL FROM TWO ITERATIONS AGO.  IF THEY ARE W/ IN 10%, THEN
C3-----THE SOLUTION IS OSCILLATING.
C3-----MAKE SURE DDAMP DOESN'T EXCEED DUP
C3-----IF THE SOLUTION IS OSCILLATING, AND WE ARE APPLYING A SIMILAR
C3-----VALUE OF DAMP (W/ IN 1%) THAT WE APPLIED PREVIOUSLY, THEN WE ARE
C3-----CAUGHT IN SOME TYPE OF ATTRACTOR. SO GENERATE A RANDOM VALUE OF
C3-----DDAMP AND HOPE TO GET OUT OF THE BASIN OF THIS ATTRACTOR.
C3-----NOTE THAT THIS IS VERY MUCH A DESPERATION EFFORT!
C
!       First determine if head-change signs are oscillating
        IF (BIGH1*BIGH2<0.0D0 .OR. BIGH2*BIGH3<0.0D0) THEN
          OSCILH = .TRUE.
        ENDIF

        OSTERM=(RSQ3-RSQ1)/RSQ3
        IF(ABS(OSTERM) .LT. 0.10) OSCIL =.TRUE.
        IF(DDAMP .GT. DUP)DDAMP=DUP
        ! Original test was insufficient to determine oscillation.
        ! Revised test of head oscillation 12/12/05 -- ERB

!   Check for oscillation based on heads
        IF (BIGH1> 0.0D0) THEN
          IF (BIGH1 < HCINC) THEN  ! if closer to solution (MHC>0)
            HCINC=BIGH1
            KSMALLERINC=KSMALLERINC+1
          ELSE
            KBIGGERINC=KBIGGERINC+1
          ENDIF
        ELSEIF (BIGH1 < 0.0D0) THEN
          IF (BIGH1 > HCDEC) THEN  ! if closer to solution (MHC<0)
            HCDEC=BIGH1
            KSMALLERDEC=KSMALLERDEC+1
          ELSE
            KBIGGERDEC=KBIGGERDEC+1
          ENDIF
        ENDIF
        IF (KSMALLERDEC>5 .AND. KSMALLERINC>5) THEN
          DDUPINTRL=DUP
          DDLOWINTRL=DLOW
          KSMALLERINC=0
          KBIGGERINC=0
          KSMALLERDEC=0
          KBIGGERDEC=0
          IF (OSCILLATING) THEN
            WRITE(IOUT,55)
   55 FORMAT(' Solution is no longer OSCILLATING')
            OSCILLATING=.FALSE.
            KSMALLERDAMP=0
          ENDIF
        ENDIF
        IF (KBIGGERINC>10 .OR. KBIGGERDEC>10) THEN
          IF (KSMALLERDAMP > 10) THEN
            !   Making DAMP smaller is not working
            DDUPINTRL=DUP
            DDLOWINTRL=DLOW
            KSMALLERINC=0
            KBIGGERINC=0
            KSMALLERDEC=0
            KBIGGERDEC=0
            IF (OSCILLATING) THEN
              OSCILLATING=.FALSE.
            ENDIF
            KSMALLERDAMP=0
          ELSE
            DDUPINTRL=DDUPINTRL*0.2D0
            IF (DDUPINTRL < DUP*1.0E-2) DDUPINTRL = DUP*1.0E-2
            DDLOWINTRL=DDLOWINTRL*0.1D0
            IF (DDLOWINTRL < DLOW*1.0E-2) DDLOWINTRL = DLOW*1.0E-2
            KSMALLERINC=0
            KBIGGERINC=0
            KSMALLERDEC=0
            KBIGGERDEC=0
            OSCILLATING=.TRUE.
            DDAMP=DDLOWINTRL
            KSMALLERDAMP = KSMALLERDAMP+1
          ENDIF
        ENDIF


        IF(OSCILH .AND. (OSCIL .OR. OSCILLATING) .AND.
     &       ABS(DDAMP-DAMP3)/DAMP3 .LT. 0.03) THEN
          NRAN=MOD(NRAN*IMULT+ICONST,IPER)
          QRAND=FLOAT(NRAN)/FLOAT(IPER)
          IF (OSCILLATING) THEN
            DDUP=DDUPINTRL
            DDLOW=DDLOWINTRL
          ELSE
            DDUP=DUP
            DDLOW=DLOW
          ENDIF
          DDAMP=DDLOW+QRAND*(DDUP*2.0D0)
        ENDIF


C4
C4-----STORE VALUES OF DAMP AND RESIDUALS FOR NEXT ITERATION
        RSQ3=RSQ2
        DAMP3=DAMP
      ENDIF
      RSQ2=RSQ1
      DAMP=DDAMP
      !
      !  Prevent extreme head changes
      BIGHABS=ABS(BIGHpcg)
      IF (BIGHABS>CHGLIMIT) THEN
        DDAMPTMP=CHGLIMIT/BIGHABS
        IF (DDAMPTMP<DDAMP) THEN
          DDAMP=DDAMPTMP
          DAMP=DDAMP
          DAMP3=DAMP
        ENDIF
      ENDIF
      !
C5
      RETURN
      END
C
      SUBROUTINE GMG7DA(IGRID)
C  Deallocate GMG data
      USE GMGMODULE
      USE GMG_C_INTERFACE
      CALL GMG7PNT(IGRID)
      CALL MF2KGMG_FREE(GMGID)
      DEALLOCATE(IITER,IADAMPGMG,ISM,ISC,IOUTGMG,ISIZ,IPREC,IIOUT,
     1           SITER,TSITER)
      DEALLOCATE(HCLOSEGMG,RCLOSEGMG,DAMPGMG,RELAXGMG)
      DEALLOCATE(IUNITMHC,DUP,DLOW,CHGLIMIT,HNEWLAST,BIGHEADCHG)
C
      RETURN
      END
C
      SUBROUTINE GMG7PNT(IGRID)
C  Set pointers to GMG data for a grid
      USE GMGMODULE
C
      IITER=>GMGDAT(IGRID)%IITER
      IADAMPGMG=>GMGDAT(IGRID)%IADAMPGMG
      ISM=>GMGDAT(IGRID)%ISM
      ISC=>GMGDAT(IGRID)%ISC
      IOUTGMG=>GMGDAT(IGRID)%IOUTGMG
      ISIZ=>GMGDAT(IGRID)%ISIZ
      IPREC=>GMGDAT(IGRID)%IPREC
      IIOUT=>GMGDAT(IGRID)%IIOUT
      SITER=>GMGDAT(IGRID)%SITER
      TSITER=>GMGDAT(IGRID)%TSITER
C Note: GMGID is a C_PTR; use = not =>
      GMGID=GMGDAT(IGRID)%GMGID
      HCLOSEGMG=>GMGDAT(IGRID)%HCLOSEGMG
      RCLOSEGMG=>GMGDAT(IGRID)%RCLOSEGMG
      DAMPGMG=>GMGDAT(IGRID)%DAMPGMG
      RELAXGMG=>GMGDAT(IGRID)%RELAXGMG
      IUNITMHC=>GMGDAT(IGRID)%IUNITMHC
      DUP=>GMGDAT(IGRID)%DUP
      DLOW=>GMGDAT(IGRID)%DLOW
      CHGLIMIT=>GMGDAT(IGRID)%CHGLIMIT
      HNEWLAST=>GMGDAT(IGRID)%HNEWLAST
      BIGHEADCHG=>GMGDAT(IGRID)%BIGHEADCHG
C
      RETURN
      END
C
      SUBROUTINE GMG7PSV(IGRID)
C  Save pointers to GMG data
      USE GMGMODULE
C
      GMGDAT(IGRID)%IITER=>IITER
      GMGDAT(IGRID)%IADAMPGMG=>IADAMPGMG
      GMGDAT(IGRID)%ISM=>ISM
      GMGDAT(IGRID)%ISC=>ISC
      GMGDAT(IGRID)%ISIZ=>ISIZ
      GMGDAT(IGRID)%IOUTGMG=>IOUTGMG
      GMGDAT(IGRID)%IPREC=>IPREC
      GMGDAT(IGRID)%IIOUT=>IIOUT
      GMGDAT(IGRID)%SITER=>SITER
      GMGDAT(IGRID)%TSITER=>TSITER
C Note: GMGID is a C_PTR; use = not =>
      GMGDAT(IGRID)%GMGID=GMGID
      GMGDAT(IGRID)%HCLOSEGMG=>HCLOSEGMG
      GMGDAT(IGRID)%RCLOSEGMG=>RCLOSEGMG
      GMGDAT(IGRID)%DAMPGMG=>DAMPGMG
      GMGDAT(IGRID)%RELAXGMG=>RELAXGMG
      GMGDAT(IGRID)%IUNITMHC=>IUNITMHC
      GMGDAT(IGRID)%DUP=>DUP
      GMGDAT(IGRID)%DLOW=>DLOW
      GMGDAT(IGRID)%CHGLIMIT=>CHGLIMIT
      GMGDAT(IGRID)%HNEWLAST=>HNEWLAST
      GMGDAT(IGRID)%BIGHEADCHG=>BIGHEADCHG
C
      RETURN
      END

