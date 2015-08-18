! Time of File Save by ERB: 6/20/2006 4:31PM

C***********************************************************************
C     SUBROUTINE RESPRINT IS CALLED FROM THE SOLVER FOR OUTPUT
C     OF REDUCTION HISTORY.
C
C     THE ITERATION (I), THE RESIDUAL (RES), AND THE
C     CONVERGENCE FACTOR (CFAC) ARE PRINTED.
C***********************************************************************
      SUBROUTINE RESPRINT(IOUT,I,RES,CFAC)
C      !DEC$ ATTRIBUTES ALIAS:'_resprint' :: RESPRINT
      IMPLICIT NONE
      INTEGER IOUT,I
      DOUBLEPRECISION RES,CFAC
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
C***********************************************************************
C SUBROUTINE ARGUMENTS:
C
C      NCOL -- NUMBER OF COLUMNS
C      NROW -- NUMBER OF ROWS
C      NLAY -- NUMBER OF LAYERS
C      MXITER -- MAX OUTER ITERATIONS
C      IITER -- MAX INNER ITERATIONS PER OUTER ITERATION
C      RCLOSE -- RESIDUAL CONVERGENCE CRITERION
C      HCLOSE -- HEAD-CHANGE CONVERGENCE CRITERION
C      DAMP -- DAMPING PARAMETER
C      IADAMP -- ADAPTIVE DAMPING FLAG
C      IOUTGMG -- OUTPUT CONTROLL
C      IN -- INPUT UNIT NUMBER
C      IOUT -- OUTPUT UNIT NUMBER
C      HNEW -- CURRENT APPROXIMATION
C      RHS -- RIGHT-HAND SIDE
C      CR,CC,CV -- CONDUCTANCE ARRAYS
C      HCOF -- SOURCE ARRAY
C      HNOFLO -- NOFLOW VALUE
C      IBOUND -- BOUNDARY FLAG
C      KITER -- CURRENT OUTER ITERATION
C      KSTP -- CURRENT TIME-STEP
C      KPER -- CURRENT STRESS PERIOD
C      ICNVG -- CONVERGENCE FLAG
C***********************************************************************
C
C***********************************************************************
C     SUBROUTINE GMG1ALG:
C     READS INPUT FROM FILE TYPE GMG SPECIFIED IN NAME FILE
C     ALLOCATES GMG SOLVER
C
C      ISIZ -- NUMBER OF MB ALLOCATED BY GMG
C      IPREC -- PRECISION FLAG (0=SINGLE, OTHERWISE DOUBLE)
C      ISM -- SMOOTHER FLAG (0=ILU, GAUSS-SEIDEL OTHERWISE)
C      ISC -- SEMI-COARSENING FLAG
C             0 : MAX COARSENING FOR COLUMNS, ROWS, AND LAYERS
C             1 : MAX COARSENING FOR COLUMNS AND ROWS.
C             2 : MAX COARSENING FOR ROWS AND LAYERS
C             3 : MAX COARSENING FOR COLUMNS AND LAYERS
C             4 : NO COARSENING
C      RELAX -- ILU RELAXATION PARAMETER (IF ISC .EQ. 4)
C      IERR -- NEGATIVE VALUE INDICATES ERROR
C      IIOUT -- EQUALS IOUT UNLESS IOUTGMG IS EVEN, THEN UNIT 6
C
C***********************************************************************
      SUBROUTINE GMG1ALG(NCOL,NROW,NLAY,MXITER,IITER,
     &                   RCLOSE,HCLOSE,DAMP,IADAMP,IOUTGMG,IN,IOUT)
C--------------------------------------------------------------------
C     EXPLICIT DECLERATIONS
C--------------------------------------------------------------------
      USE MHC, ONLY: MHC1ALG
      IMPLICIT NONE
C
      INTEGER NCOL,NROW,NLAY,MXITER,IITER
      REAL RCLOSE,HCLOSE,DAMP
      DOUBLEPRECISION RELAX
      INTEGER IADAMP,IOUTGMG,IN,IOUT
C
      INTEGER ISIZ,IPREC,ISM
      INTEGER ISC,IERR,IIOUT
C
      CHARACTER*200 LINE
      ! Changes by ERB marked by !ERB
      INTEGER ICOL, IUNITMHC, ISTART, ISTOP, NDUM  !ERB
      REAL DUP, DLOW, CHGLIMIT, RDUM               !ERB
      COMMON /GMGCOMMON/DUP,DLOW,CHGLIMIT          !ERB
C
C--------------------------------------------------------------------
C     READ AND PRINT COMMENTS
C--------------------------------------------------------------------
      CALL URDCOM(IN,IOUT,LINE)
C
C--------------------------------------------------------------------
C     READ INPUT FILE
C--------------------------------------------------------------------
      READ(LINE,*) RCLOSE,IITER,HCLOSE,MXITER
      CALL URDCOM(IN,IOUT,LINE)
!      READ(LINE,*) DAMP,IADAMP,IOUTGMG !ERB--Need to optionally read IUNITMHC
      ICOL = 1                                                   !ERB
      CALL URWORD(LINE,ICOL,ISTART,ISTOP,3,NDUM,DAMP,IOUT,IN)    !ERB
      CALL URWORD(LINE,ICOL,ISTART,ISTOP,2,IADAMP,RDUM,IOUT,IN)  !ERB
      CALL URWORD(LINE,ICOL,ISTART,ISTOP,2,IOUTGMG,RDUM,IOUT,IN) !ERB
      IUNITMHC = 0                                               !ERB
      NDUM = -1                                                  !ERB
      CALL URWORD(LINE,ICOL,ISTART,ISTOP,2,NDUM,RDUM,-1,IN)      !ERB
      IF (NDUM>0) IUNITMHC = NDUM                                !ERB
!
      CALL URDCOM(IN,IOUT,LINE)
      IF (IADAMP==0 .OR. IADAMP==1) THEN       !ERB
        READ(LINE,*) ISM,ISC
      ELSEIF (IADAMP==2) THEN                  !ERB
        READ(LINE,*) ISM,ISC,DUP,DLOW,CHGLIMIT !ERB
      ELSE                                     !ERB
        WRITE(IOUT,400)                        !ERB
  400 FORMAT(/,1X,'ERROR IN GMG INPUT: IADAMP MUST BE ONE OF 0, 1,',
     &    ' OR 2 (GMG1ALG)')                   !ERB
        CALL USTOP(' ')                        !ERB
      ENDIF                                    !ERB
C
      RELAX=0.0D0
      IF(ISC .EQ. 4) THEN
        CALL URDCOM(IN,IOUT,LINE)
        READ(LINE,*) RELAX
      END IF
C
      IF(DAMP .LE. 0.0 .OR. DAMP .GT. 1.0) DAMP=1.0
      IIOUT=IOUT
      IF(IOUTGMG .GT. 2) IIOUT=6
C
C--------------------------------------------------------------------
C     ALLOCATE
C--------------------------------------------------------------------
C
C---- CHECK FOR FORCED DOUBLE PRECISION
C
      IPREC=0
      IF(KIND(DAMP) .EQ. 8) IPREC=1
C
      CALL MF2KGMG_ALLOCATE(NCOL,NROW,NLAY,IPREC,ISM,ISC,
     &                      RELAX,ISIZ,IERR)
      IF(IERR .NE. 0) THEN
        CALL USTOP('ALLOCATION ERROR IN SUBROUTINE GMG1ALG')
      END IF
C
      WRITE(IIOUT,500) RCLOSE,IITER,HCLOSE,MXITER,
     &                 DAMP,IADAMP,IOUTGMG,
     &                 ISM,ISC,RELAX
C
      IF (IADAMP==1) WRITE(IIOUT,510)               !ERB
      IF (IADAMP==2) THEN                           !ERB
        WRITE(IIOUT,512)                            !ERB
        WRITE(IIOUT,513)DUP,DLOW,CHGLIMIT           !ERB
      ENDIF                                         !ERB
      IF (IADAMP==2 .OR. IUNITMHC>0) THEN           !ERB
        !   Activate Max. Head Change (MHC) module  !ERB
        CALL MHC1ALG(IUNITMHC,IOUT,NCOL,NROW,NLAY)  !ERB
      ENDIF                                         !ERB
C
      IF(ISM .EQ. 0) WRITE(IIOUT,520)
      IF(ISM .EQ. 1) WRITE(IIOUT,525)
      IF(ISC .EQ. 0) WRITE(IIOUT,530)
      IF(ISC .EQ. 1) WRITE(IIOUT,531)
      IF(ISC .EQ. 2) WRITE(IIOUT,532)
      IF(ISC .EQ. 3) WRITE(IIOUT,533)
      IF(ISC .EQ. 4) WRITE(IIOUT,534)
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
  510 FORMAT(1X,"COOLEY'S ADAPTIVE DAMPING METHOD IMPLEMENTED")
  512 FORMAT(1X,'RELATIVE REDUCED RESIDUAL ADAPTIVE DAMPING METHOD',   
     &' WILL BE USED')                                                 
  513 FORMAT(5X,'WITH DUP = ',G9.3,' DLOW = ',G9.3,' AND CHGLIMIT = ', 
     &G9.3)                                                            
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
      RETURN
      END
C
C***********************************************************************
C  SUBROUTINE GMG1AP CALLS THE FOLLOWING FUNCTIONS FROM THE GMG LIBRARY:
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
C
C      IERR -- NEGATIVE VALUE INDICATES ERROR
C      IIOUT -- EQUALS IOUT UNLESS IOUTGMG IS EVEN, THEN UNIT 6
C      DRCLOSE -- PCG CONVERGENCE CRITERION
C      ITER -- PCG ITERATIONS
C      BIGH -- MAX HEAD CHANGE.  ABS(BIGH)=MAX-NORM
C      BIGR0 -- L2-NORM OF INITIAL RESIDUAL
C      BIGR -- L2-NORM OF UPDATED RESIDUAL
C      S,DH,DAMP0,BIGH0,DDAMP -- FOR COMPUTING ADAPTIVE DAMPING
C      SITER -- TOTAL PCG ITERATIONS FOR STRESS/TIME PERIOD
C      TSITER -- TOTAL PCG ITERATIONS FOR ALL STRESS/TIME PERIODS
C
C***********************************************************************
      SUBROUTINE GMG1AP(HNEW,RHS,CR,CC,CV,HCOF,HNOFLO,IBOUND,
     &                  IITER,MXITER,RCLOSE,HCLOSE,
     &                  KITER,KSTP,KPER,
     &                  ICNVG,DAMP,IADAMP,IOUTGMG,IOUT)
C--------------------------------------------------------------------
C--------------------------------------------------------------------
      USE MHC, ONLY: DAMPMHC, MHC_ACTIVE, MHC1AD, MHC1IT, MHC1OT    !ERB
      IMPLICIT NONE
      REAL RHS(*),CR(*),CC(*),CV(*),HCOF(*)
      REAL HNOFLO,RCLOSE,HCLOSE,DAMP
      DOUBLEPRECISION HNEW(*)
      INTEGER IBOUND(*)
      INTEGER MXITER,IITER,KITER,KSTP,KPER,ICNVG,IOUTGMG,IOUT
      INTEGER IADAMP
C
      INTEGER IIOUT
      DOUBLEPRECISION DRCLOSE
C
      INTEGER ITER,IERR
      INTEGER JBIGH,IBIGH,KBIGH
C
      DOUBLEPRECISION BIGH,BIGR,BIGR0
      DOUBLEPRECISION S,DH,DAMP0
      DOUBLEPRECISION, SAVE :: BIGH0
      DOUBLEPRECISION, SAVE :: DDAMP
      INTEGER, SAVE :: SITER=0
      INTEGER, SAVE :: TSITER=0
!
      REAL DUP, DLOW, DAMPA, BIGHA, CHGLIMIT       !ERB
      DOUBLE PRECISION :: RSQ                      !ERB
      COMMON /GMGCOMMON/DUP,DLOW,CHGLIMIT          !ERB
      !                                            !ERB
      IF (MHC_ACTIVE) THEN                         !ERB
        IF (KITER .EQ. 1) CALL MHC1AD(KPER,KSTP)   !ERB
        CALL MHC1IT(HNEW)                          !ERB
      ENDIF                                        !ERB
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
      CALL MF2KGMG_ASSEMBLE(BIGR0,CR,CC,CV,HCOF,HNEW,RHS,HNOFLO,IBOUND,
     &                      IERR)
      IF(IERR .NE. 0) THEN
        WRITE(IOUT,400)                                        !ERB
  400 FORMAT(1X,'GMG ASSEMBLY ERROR IN SUBROUTINE GMG1AP')     !ERB
        CALL USTOP('GMG ASSEMBLY ERROR IN SUBROUTINE GMG1AP')
      END IF
C
C--------------------------------------------------------------------
C     SCALE CLOSURE CRITERION FOR INNER ITERATION BASED ON CURRENT
C     VALUE OF DAMPING AND INITIAL RESIDUAL.
C--------------------------------------------------------------------
      DRCLOSE=DDAMP*RCLOSE+(1.0D0-DDAMP)*BIGR0
      IF (IADAMP==2 .AND. DDAMP < 0.5D0) THEN     !ERB
        DRCLOSE=RCLOSE                            !ERB
      ENDIF                                       !ERB
C
C--------------------------------------------------------------------
C     COMPUTE HEAD CHANGE
C--------------------------------------------------------------------
      CALL MF2KGMG_EVAL(ITER,BIGR,DRCLOSE,IITER,IOUTGMG,IIOUT)
      SITER=SITER+ITER
C
C--------------------------------------------------------------------
C     COMPUTE MAX HEAD CHANGE
C--------------------------------------------------------------------
      CALL MF2KGMG_BIGH(BIGH,JBIGH,IBIGH,KBIGH);
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
C     ADJUST DAMPING PARAMETER USING COOLEY'S METHOD
C--------------------------------------------------------------------------
      IF(IADAMP == 1) THEN                       !ERB
        ! Use Cooley's method                    !ERB
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
      ELSEIF (IADAMP == 2) THEN                                     !ERB
        ! Use relative reduced residual method                      !ERB
        ! Call LMG routine to calculate DDAMP                       !ERB
        RSQ=BIGR                                                    !ERB
        DAMPA=DAMP                                                  !ERB
        BIGHA=BIGH                                                  !ERB
        CALL GMG1ADAMP2(RSQ,DDAMP,DAMPA,DUP,DLOW,KITER,BIGHA,IOUT,  !ERB
     &                  CHGLIMIT)                                   !ERB
      END IF

C
C--------------------------------------------------------------------
C     ADD CORECTION
C--------------------------------------------------------------------
  100 CONTINUE
      DAMPMHC=DDAMP                                                 !ERB
      CALL MF2KGMG_UPDATE(HNEW,DDAMP)
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
      !
      IF (MHC_ACTIVE) CALL MHC1OT(KITER,IBOUND,HNEW)                !ERB
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
     &       1X,'DAMPING                           : ',0P,E10.4,/,  
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
      SUBROUTINE GMG1ADAMP2(RSQ1,DDAMP,DAMP,DUP,DLOW,KITER,BIGHPCG,
     &                      IOUT,CHGLIMIT)
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
      USE MHC, ONLY: BIGHEADCHG
      IMPLICIT NONE
      !   Argument list variables
      INTEGER,          INTENT(IN)    :: IOUT, KITER
      REAL,             INTENT(IN)    :: BIGHPCG, CHGLIMIT, DLOW, DUP
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
C2B-----SO SETTING DLOW=0.2 IS APPROPIATE SO DAMP WON'T EXCEED 0.5
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
C3-----NOTE THAT THIS IS VERY MUCH A DESPARATION EFFORT!
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

C***********************************************************************
      SUBROUTINE GMG1DA()                  !ERB
C     Deallocate arrays                    !ERB
      USE MHC, ONLY: MHC_ACTIVE, MHC1DA    !ERB
      !                                    !ERB
      CALL MF2KGMG_FREE()                  !ERB
      IF (MHC_ACTIVE) CALL MHC1DA()        !ERB
      RETURN                               !ERB
      END SUBROUTINE                       !ERB