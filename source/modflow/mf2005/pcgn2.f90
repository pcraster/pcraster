MODULE PCGN
  ! ... LAST MODIFIED:  R.L. NAFF,  OCTOBER, 2012
  ! ... MODULE BASED ON DE45, PCG2 AND LMG1, BY 
  ! ... A.W. HARBAUGH, M.C. HILL AND S.W. MEH, RESPECTIVELY.
  ! ...
  ! ... THIS MODULE SERVES AN THE INTERFACE BETWEEN THE PCG SOLVER
  ! ... AND MODFLOW.  MODFLOW CALLS SUBROUTINES IN THIS MODULE, AND 
  ! ... THE MODULE CALLS APPROPRIATE SUBROUTINES IN THE PCG SOLVER.
  ! ...
  ! ... R.L. Naff 
  ! ...
  ! ... VERSION 2.0.4 October, 2012
  ! ... 
  ! ... 08/08: VERSION 2.0, FOR USE WITH MF2005
  ! ... 04/09: CORRECTED MINOR ERROR IN REPORTING NODAL LOCATION OF
  ! ...        OF MAXIMUM HEAD CHANGE.
  ! ... 02/10: CORRECTED MINOR ERROR ASSOCIATED WITH REPORTING OF 
  ! ...        NONCONVERGENCE (KITER=MO_ITER), SUBROUTINE SECONDARY_CLOSE.
  ! ... 10/11: REPLACED WHOLE-ARRAY OPERATION IN SUBROUTINE NONLINEAR WITH
  ! ...        BLAS-LIKE SUBROUTINE CALL; SEE SUBROUTINE UPDATE_HEAD.
  ! ... 10/12: CORRECTED LOGIC ERROR IN SUBROUTINE NONLINEAR; LOGIC ERROR
  ! ...        RESULTED IN EXCESSIVE INNER ITERATIONS WHEN KITER=1, KSTP/=1.
  ! ... 
  USE GLOBAL, ONLY: IOUT, NCOL, NROW, NLAY, NODES
  ! ... MODULE GLOBAL DEFINED IN gwf2bas7.f
  USE PCG_MAIN
  IMPLICIT NONE
  TYPE PCGNTYPE
     INTEGER, POINTER  :: FILL, MO_ITER, MI_ITER, PRGUNIT, PC_UNIT, &
          TS_UNIT, CNVG_A, DAMP_A
     DOUBLE PRECISION, POINTER :: SAV_DAMP, DRELAX, MAG_CLOSE, SAV_CLOSE, &
          C_RATE, D_RATE, RCLOSE, HCLOSE, LIMHDCHG, MIN_DAMP, MIN_CLOSE
     DOUBLE PRECISION, DIMENSION(:), POINTER :: DD, DX, DY, DZ, RES, HCH
  END TYPE PCGNTYPE
  TYPE(PCGNTYPE), DIMENSION(1:10), TARGET ::PCGNDAT
  TYPE(PCGNTYPE), POINTER ::PT
  ! ...
  INTEGER, SAVE :: MGRID=0, KITER, KSTP, KPER
  REAL :: HCLOSEPCGN
  DOUBLE PRECISION, PARAMETER :: ZERO=0.D0, ONE=1.D0, TWO=2.D0, &     
       THREE=3.D0, FOUR=4.D0, FIVE=5.D0, SEVEN=7.D0, EIGHT=8.D0, &
       TEN=1.D1, HUNDRED=1.D2, SQRTEN=3.16227766017, &     
       HALF=0.5D0, QUARTER=0.25D0, TENTH=0.1D0, HUNDRETH=.01D0
  DOUBLE PRECISION, PARAMETER :: MZ=TINY(ZERO), LARGE=HUGE(1.0_8)
  DOUBLE PRECISION, PARAMETER :: SMALL=EPSILON(1.0_8)*HUNDRED
  ! ... POINTERS
  INTEGER, DIMENSION(:), POINTER :: IBOUND
  REAL, DIMENSION(:), POINTER :: CR, CC, CV, HCOF, RHS
  DOUBLE PRECISION, DIMENSION(:), POINTER :: HNEW
  PRIVATE; PUBLIC :: PCGN2AR, PCGN2AP, PCGN2DA, HCLOSEPCGN
  ! ... HCLOSEPCGN NEEDED IN gwf2mnw7.f
CONTAINS

  SUBROUTINE PCGN2AR(IN,FREE_FMT,ITER_MO,IGRID) 
    ! ... 
    ! ... ***************************************************************
    ! ... PURPOSE: READ INFO FOR PCG SOLVER AND PICARD ALGORITHM.
    ! ... ***************************************************************
    ! ... 
    ! ... SPECIFICATIONS:
    ! ... ---------------------------------------------------------------
    ! ...    ARGUMENT LIST
    ! ... ---------------------------------------------------------------
    INTEGER, INTENT(IN) :: IN, FREE_FMT, IGRID
    INTEGER, INTENT(OUT) :: ITER_MO
    ! ... ---------------------------------------------------------------
    ! ...    LOCAL VARIABLES
    ! ... ---------------------------------------------------------------
    INTEGER :: ITER_MI, IPUNIT, IFILL, UNIT_PC, UNIT_TS, ADAMP, &
         ACNVG, MCNVG, L_COUNT, IOS, IERR
    REAL :: DAMP, DAMP_LB, CNVG_LB, RELAX, RATE_C, RATE_D, CHGLIMIT, &
         CLOSE_R, CLOSE_H
    CHARACTER(LEN=12) :: C_VAL1, C_VAL2, C_VAL3, C_VAL4
    CHARACTER(LEN=1) :: CHECK
    CHARACTER(LEN=256) :: CHAR_STRING
    CHARACTER(LEN=80), DIMENSION(1:4) :: DATA_STRING
    !$ INTEGER :: N_PROC
    !$ INTEGER, EXTERNAL :: OMP_GET_NUM_PROCS
    !$ LOGICAL, SAVE :: INIT=.TRUE.
    ! ... ===============================================================
    ! ... OPEN MP DYNAMIC THREAD ADJUSTMENT 
    ! ... OPEN MP COMPILER FLAG MUST BE SET TO ACTIVATE PARALLELISM
    ! ...
    !$ IF (INIT) THEN
    !$    N_PROC=OMP_GET_NUM_PROCS()
    !$    CALL OMP_SET_NUM_THREADS(N_PROC)
    !$    INIT=.FALSE.
    !$ ENDIF
    ! ...
    ! ... PRINT A MESSAGE IDENTIFYING PCGN PACKAGE
    WRITE(IOUT,300)
300 FORMAT(1X,/1X,'PCG7AR -- PCGN INTERFACE PACKAGE', &     
         ', VERSION 2.0, 08/2008')
    ! ... 
    ! ... STRIP OUT COMMENT CARDS (#) AND WRITE DATA TO DATA_STRING ARRAY
    ! ... 
    L_COUNT=0
    DO
       READ(UNIT=IN,FMT='(A)',IOSTAT=IOS) CHAR_STRING
       IF (IOS<0) THEN ! EOF
          EXIT
       ELSEIF (IOS>0) THEN
          WRITE(IOUT,*) '*** BAD RECORD ENCOUNTERED, PCGN DATA INPUT ***'
          STOP
       ELSE
          IF (LEN_TRIM(CHAR_STRING)==0) CYCLE
          CHECK=CHAR_STRING
          IF (CHECK=='#') CYCLE
          L_COUNT=L_COUNT+1
          DATA_STRING(L_COUNT)=TRIM(CHAR_STRING)
          ! ... READ MAXIMUM OF FOUR DATA RECORDS
          IF (L_COUNT==4) EXIT
       ENDIF
    ENDDO
    ! ... READ IFILL,MO_ITER,MI_ITER,CLOSE_R,CLOSE_H,RELAX,UNIT_PC,UNIT_TS,
    ! ...  ADAMP,DAMP,DAMP_LB,RATE_D,CHGLIMIT,ACNVG,CNVG_LB,MCNVG,RATE_C,IPUNIT
    IF(FREE_FMT.EQ.0) THEN
       READ (UNIT=DATA_STRING(1),FMT=400,IOSTAT=IOS) ITER_MO, ITER_MI, &
            CLOSE_R, CLOSE_H
       IF (IOS/=0) THEN
          WRITE (IOUT,800) 'TER_MO, ITER_MI, CLOSE_R, CLOSE_H', DATA_STRING(1)
          STOP
       ENDIF
400    FORMAT (2I10,2F10.0)
       READ (UNIT=DATA_STRING(2),FMT=405,IOSTAT=IOS) RELAX, IFILL, &
            UNIT_PC, UNIT_TS
       IF (IOS/=0) THEN
          WRITE (IOUT,800) 'RELAX, IFILL, UNIT_PC, UNIT_TS', DATA_STRING(2)
          STOP
       ENDIF
405    FORMAT (F10.0,3I10)
       IF (ITER_MO>1) THEN
          ! ... NONLINEAR DAMPING
          READ (UNIT=DATA_STRING(3),FMT=410,IOSTAT=IOS) ADAMP, DAMP, &
               DAMP_LB, RATE_D, CHGLIMIT
          IF (IOS/=0) THEN
             WRITE (IOUT,800) 'ADAMP, DAMP, DAMP_LB, RATE_D, CHGLIMIT', &
                  DATA_STRING(3)
             STOP
          ENDIF
410       FORMAT (I10,4F10.0)
          ! ... NONLINEAR CONVERGENCE
          READ (UNIT=DATA_STRING(4),FMT=415,IOSTAT=IOS) ACNVG, CNVG_LB, &
               MCNVG, RATE_C, IPUNIT
          IF (IOS/=0) THEN
             WRITE (IOUT,800) 'ACNVG, CNVG_LB, MCNVG, RATE_C, IPUNIT', &
                  DATA_STRING(4)
             STOP
          ENDIF
415       FORMAT (I10,F10.0,I10,F10.0,I10)
       ELSE
          ! ... LINEAR DEFAULT VALUES
          ADAMP=0;ACNVG=0;DAMP=ONE;DAMP_LB=ZERO;RATE_C=ZERO
          CNVG_LB=ZERO;MCNVG=1;RATE_D=ZERO;CHGLIMIT=ZERO;IPUNIT=0
       ENDIF
    ELSE
       READ (UNIT=DATA_STRING(1),FMT=*,IOSTAT=IOS) ITER_MO,ITER_MI, &
            CLOSE_R,CLOSE_H
       IF (IOS/=0) THEN
          WRITE (IOUT,800) 'TER_MO,ITER_MI,CLOSE_R,CLOSE_H', DATA_STRING(1)
          STOP
       ENDIF
       READ (UNIT=DATA_STRING(2),FMT=*,IOSTAT=IOS)  RELAX, IFILL, &
            UNIT_PC,UNIT_TS
       IF (IOS/=0) THEN
          WRITE (IOUT,800) 'RELAX, IFILL, UNIT_PC, UNIT_TS', DATA_STRING(2)
          STOP
       ENDIF
       IF (ITER_MO>1) THEN
          ! ... NONLINEAR DAMPING
          READ (UNIT=DATA_STRING(3),FMT=*,IOSTAT=IOS) ADAMP, DAMP, &
               DAMP_LB, RATE_D, CHGLIMIT
          IF (IOS/=0) THEN
             WRITE (IOUT,800) 'ADAMP, DAMP, DAMP_LB, RATE_D, CHGLIMIT', &
                  DATA_STRING(3)
             STOP
          ENDIF
          ! ... NONLINEAR CONVERGENCE
          READ (UNIT=DATA_STRING(4),FMT=*,IOSTAT=IOS) ACNVG, CNVG_LB, &
               MCNVG, RATE_C, IPUNIT
          IF (IOS/=0) THEN
             WRITE (IOUT,800) 'ACNVG, CNVG_LB, MCNVG, RATE_C, IPUNIT', &
                  DATA_STRING(4)
             STOP
          ENDIF
       ELSE
          ! ... LINEAR DEFAULT VALUES
          ADAMP=0;ACNVG=0;DAMP=ONE;DAMP_LB=ZERO;RATE_C=ZERO
          CNVG_LB=ZERO;MCNVG=1;RATE_D=ZERO;CHGLIMIT=ZERO;IPUNIT=0
       ENDIF
    ENDIF
    ! ... 
    ! ... PRINT TOTAL ARRAY STORAGE REQUIRED
    IF (IFILL==0) THEN
       WRITE (IOUT,450) 4*NODES
    ELSE
       WRITE (IOUT,450) 8*NODES
    ENDIF
450 FORMAT (1X,'TOTAL ARRAY STORAGE REQUIRED IS ',I7)
    ! ... 
    ! ... PRINT VALUES FOR INPUT VARIABLES
    ! ... 
    ! ... SOLVER PARAMETERS
    WRITE (IOUT,500)
500 FORMAT (1X,///,20X,'SOLUTION BY PCG', /, 8X,46('-'))
    WRITE (IOUT,510) IFILL
510 FORMAT (1X,7X,'FILL LEVEL OF UDU PRECONDITIONER =',I9)
    WRITE (IOUT,515) RELAX
515 FORMAT (1X,22X,'RELAXATION FACTOR =',G15.5)
    WRITE (IOUT,520) CLOSE_R
520 FORMAT (1X,7X,'RESIDUAL-BASED CLOSURE CRITERION =',G15.5)
    WRITE (IOUT,525) CLOSE_H
525 FORMAT (1X,9X,'MAXIMUM HEAD CLOSURE CRITERION =',G15.5)
    WRITE (IOUT,530) ITER_MO
530 FORMAT (1X,2X,'MAXIMUM NUMBER OF NONLINER ITERATIONS =',I9)
    WRITE (IOUT,535) ITER_MI
535 FORMAT (1X,7X,'MAXIMUM NUMBER OF PCG ITERATIONS =',I9)
    ! ... NONLINEAR PICARD PARAMETERS
    IF (ITER_MO>1) THEN
       WRITE (IOUT,540) ADAMP
540    FORMAT (1X,18X,'ADAPTIVE DAMPING FLAG =',I9)
       WRITE (IOUT,545) DAMP
545    FORMAT (1X,22X,'DAMPING PARAMETER =',G15.5)
       WRITE (IOUT,550) DAMP_LB
550    FORMAT (1X,20X,'DAMPING LOWER BOUND =',G15.5)
       WRITE (IOUT,555) RATE_D
555    FORMAT (1X,16X,'DAMPING ADJUSTMENT RATE =',G15.5)
       WRITE (IOUT,560) CHGLIMIT
560    FORMAT (1X,10X,'MAXIMUM ALLOWABLE HEAD CHANGE =',G15.5)
       WRITE (IOUT,570) ACNVG
570    FORMAT (1X,14X,'ADAPTIVE CONVERGENCE FLAG =',I9)
       WRITE (IOUT,575) CNVG_LB
575    FORMAT (1X,16X,'CONVERGENCE LOWER BOUND =',G15.5)
       WRITE (IOUT,580) MCNVG
580    FORMAT (1X,14X,'CONVERGENCE MAGNIFICATION =',I9)
       WRITE (IOUT,585) RATE_C
585    FORMAT (1X,12X,'CONVERGENCE ADJUSTMENT RATE =',G15.5)
       ! ... 
       IF (IPUNIT>0) THEN
          WRITE (IOUT,590) IPUNIT
       ELSE
          WRITE (IOUT,595)
       ENDIF
590    FORMAT (1X,10X,'NONLINEAR PROGRESS PRINT FLAG IS ON',/, &
            1X,10X,'MAXIMUM HEAD CHANGES WILL BE WRITTEN TO UNIT ',I4)
595    FORMAT (1X,10X,'NONLINEAR PROGRESS PRINT FLAG IS OFF')
       ! ... 
       IF (UNIT_PC>0) THEN
          WRITE (IOUT,600)
       ELSE
          WRITE (IOUT,605)
          IF (IPUNIT<0) WRITE (IOUT,610)
       ENDIF
600    FORMAT (1X,10X,'INNER ITERATION CONVERGENCE PRINT FLAG IS ON')
605    FORMAT (1X,10X,'INNER ITERATION CONVERGENCE PRINT FLAG IS OFF')
610    FORMAT (1X,10X,'MINIMAL PROGRESS REPORTING TO LIST FILE IN EFFECT')
       ! ... 
       IF (UNIT_TS>0) THEN
          WRITE (IOUT,615)
       ELSE
          WRITE (IOUT,620)
       ENDIF
615    FORMAT (1X,10X,'SOLVER TIMING FLAG IS ON')
620    FORMAT (1X,10X,'SOLVER TIMING FLAG IS OFF')
    ENDIF
    ! ...
    ! ... ALLOCATE POINTER SPACE
    ! ... 
    IERR=0; MGRID=MAX(IGRID,MGRID)
    DO
       ALLOCATE (PCGNDAT(IGRID)%FILL, PCGNDAT(IGRID)%MO_ITER, &
            PCGNDAT(IGRID)%MI_ITER, PCGNDAT(IGRID)%CNVG_A, &
            PCGNDAT(IGRID)%DAMP_A, PCGNDAT(IGRID)%PRGUNIT, &
            PCGNDAT(IGRID)%PC_UNIT, PCGNDAT(IGRID)%TS_UNIT, &
            PCGNDAT(IGRID)%SAV_DAMP, PCGNDAT(IGRID)%DRELAX, &
            PCGNDAT(IGRID)%MAG_CLOSE, PCGNDAT(IGRID)%SAV_CLOSE, &
            PCGNDAT(IGRID)%C_RATE, PCGNDAT(IGRID)%D_RATE, &
            PCGNDAT(IGRID)%RCLOSE, PCGNDAT(IGRID)%HCLOSE, &
            PCGNDAT(IGRID)%LIMHDCHG, PCGNDAT(IGRID)%MIN_DAMP, &
            PCGNDAT(IGRID)%MIN_CLOSE,STAT=IERR)
       IF (IERR/=0) EXIT
       ALLOCATE(PCGNDAT(IGRID)%DD(NODES),STAT=IERR)
       IF (IERR/=0) EXIT
       ALLOCATE(PCGNDAT(IGRID)%RES(NODES),STAT=IERR)
       IF (IERR/=0) EXIT
       IF (NCOL>1) THEN
          ALLOCATE(PCGNDAT(IGRID)%DX(NODES),STAT=IERR)
       ELSE
          ALLOCATE(PCGNDAT(IGRID)%DX(1),STAT=IERR)
       ENDIF
       IF (IERR/=0) EXIT
       IF (NROW>1) THEN
          ALLOCATE(PCGNDAT(IGRID)%DY(NODES),STAT=IERR)
       ELSE
          ALLOCATE(PCGNDAT(IGRID)%DY(1),STAT=IERR)
       ENDIF
       IF (IERR/=0) EXIT
       IF (NLAY>1) THEN
          ALLOCATE(PCGNDAT(IGRID)%DZ(NODES),STAT=IERR)
       ELSE
          ALLOCATE(PCGNDAT(IGRID)%DZ(1),STAT=IERR)
       ENDIF
       IF (IERR/=0) EXIT
       IF (ITER_MO>1) THEN
          ALLOCATE(PCGNDAT(IGRID)%HCH(NODES),STAT=IERR)
       ELSE
          ALLOCATE(PCGNDAT(IGRID)%HCH(1),STAT=IERR)
       ENDIF
       EXIT
    ENDDO
    IF (IERR/=0) THEN
       WRITE(IOUT,630) IERR
       WRITE(IOUT,640)
       CALL PCGN_DEALLOCATE(MGRID)
       STOP
    ENDIF
    PT=>PCGNDAT(IGRID)
    PT%SAV_CLOSE=TENTH
630 FORMAT (/,1X,' UNABLE TO ALLOCATE STORAGE REQUIRED FOR', & 
         ' PCG SOLVER, ERROR CODE: ',I6)
640 FORMAT (/,1X,' ERROR ENCOUNTERED IN SUBROUTINE PCGNRP, &
         &MODULE PCGN.')
    ! ... 
    ! ... INTERPRET AND SET INPUT PARAMETERS
    ! ...
    ! ... ASSIGN MAIN TRANSFER VARIABLES
    PT%MO_ITER=ITER_MO; PT%MI_ITER=ITER_MI; PT%RCLOSE=CLOSE_R
    PT%HCLOSE=CLOSE_H; PT%DRELAX=RELAX; PT%FILL=IFILL; 
    PT%PRGUNIT=IPUNIT;PT%PC_UNIT=UNIT_PC; PT%TS_UNIT=UNIT_TS
    HCLOSEPCGN=CLOSE_H
    ! ...
    ! ... ASSIGN PICARD ITERATION TRANSFER VARIABLES
    ! ...
    ! ... CONVERGENCE ADJUSTMENTS; SAV_CLOSE PRESET TO 0.1
    PT%CNVG_A=ACNVG; PT%MIN_CLOSE=CNVG_LB; PT%C_RATE=RATE_C
    PT%DAMP_A=ADAMP; PT%SAV_DAMP=DAMP; PT%MIN_DAMP=DAMP_LB; PT%D_RATE=RATE_D
    PT%MAG_CLOSE=PT%SAV_CLOSE; PT%LIMHDCHG=CHGLIMIT
    IF (ITER_MO==1) THEN
       WRITE (IOUT,700)
    ELSE
       SELECT CASE(PT%CNVG_A)
       CASE(0)
          ! ... NO ADJUSTMENT FOR PCG_CLOSE; DEFAULT VALUES USED
          PT%MIN_CLOSE=ZERO; PT%C_RATE=ZERO; MCNVG=0
       CASE(1)
          ! ... ADAPTIVE PCG CLOSURE BASED ON LINEARITY
          PT%C_RATE=ZERO; MCNVG=0
          IF (PT%MIN_CLOSE>=PT%SAV_CLOSE.OR.PT%MIN_CLOSE<=SMALL) THEN
             ! ... MIN_CLOSE SET TOO LARGE OR SMALL (NEGATIVE)
             PT%MIN_CLOSE=ZERO; PT%CNVG_A=0
             C_VAL1='ACNVG=0'; C_VAL2='CNVG_LB=0'; C_VAL3='MCNVG=0'
             C_VAL4='RATE_C=0'
             WRITE (IOUT,810) TRIM(C_VAL1), TRIM(C_VAL2), TRIM(C_VAL3), &
                  TRIM(C_VAL4)
          ENDIF
       CASE(2)
          ! ... ENHANCED PCG CLOSURE
          PT%MIN_CLOSE=ZERO
          IF (MCNVG<0.OR.(MCNVG>6.AND.PT%C_RATE>=ONE)) THEN
             ! ... MCNVG MUST BE > 0 FOR CNVG_A=2 OR CONFUSING INTENT
             PT%C_RATE=ZERO; PT%CNVG_A=0; MCNVG=0
             C_VAL1='ACNVG=0'; C_VAL2='CNVG_LB=0'; C_VAL3='MCNVG=0'
             C_VAL4='RATE_C=0'
             WRITE (IOUT,810) TRIM(C_VAL1), TRIM(C_VAL2), TRIM(C_VAL3), &
                  TRIM(C_VAL4)
          ELSE
             IF (MCNVG>6) THEN
                ! ... MCNVG SET TOO LARGE; RESET
                MCNVG=6
                C_VAL1='MCNVG=6'; C_VAL2=''; C_VAL3=''; C_VAL4=''
                WRITE (IOUT,810) TRIM(C_VAL1)
             ENDIF
             ! ... SET MAG_CLOSE
             PT%MAG_CLOSE=PT%SAV_CLOSE**MCNVG
             IF (PT%C_RATE>=ONE) THEN
                ! ... C_RATE SET TOO LARGE; RESET
                PT%C_RATE=ZERO
                C_VAL1='CNVG_LB=0'; C_VAL2='RATE_C=0'; C_VAL3=''; C_VAL4=''
                WRITE (IOUT,810) TRIM(C_VAL1), TRIM(C_VAL2)
             ENDIF
          ENDIF
       CASE DEFAULT
          PT%MIN_CLOSE=ZERO; PT%C_RATE=ZERO; PT%CNVG_A=0; MCNVG=0
          C_VAL1='ACNVG=0'; C_VAL2='CNVG_LB=0'; C_VAL3='MCNVG=0'
          C_VAL4='RATE_C=0'
          WRITE (IOUT,810) TRIM(C_VAL1), TRIM(C_VAL2), TRIM(C_VAL3), &
               TRIM(C_VAL4)
       END SELECT
       ! ... DAMPING ADJUSTMENTS
       ! ... STORE INPUT DAMPING VALUE FOR LATER USE
       IF (PT%SAV_DAMP<=ZERO.OR.PT%SAV_DAMP>ONE) THEN
          PT%SAV_DAMP=HALF; C_VAL1='DAMP=0.5'; C_VAL2=''; C_VAL3=''; C_VAL4=''
          WRITE (IOUT,810) TRIM(C_VAL1), TRIM(C_VAL2), TRIM(C_VAL3), &
               TRIM(C_VAL4)
       ENDIF
       SELECT CASE(PT%DAMP_A)
       CASE(0)
          ! ... NO ADJUSTMENT TO DAMP; DEFAULT VALUES USED
          PT%D_RATE=ZERO; PT%MIN_DAMP=ZERO; PT%LIMHDCHG=ZERO
       CASE(1,2)
          ! ... AUTO DAMPING OR ENHANCED DAMPING
          IF (PT%DAMP_A==1.AND.PT%LIMHDCHG<SMALL) PT%LIMHDCHG=LARGE
          IF (PT%MIN_DAMP<SMALL.OR.PT%MIN_DAMP>PT%SAV_DAMP.OR. &
               PT%D_RATE<SMALL.OR.PT%D_RATE>=ONE) THEN
             ! ... MIN_DAMP OR D_RATE OUT OF RANGE
             PT%D_RATE=ZERO; PT%DAMP_A=0; PT%MIN_DAMP=ZERO; PT%LIMHDCHG=ZERO
             C_VAL1='ADAMP=0'; C_VAL2='DAMP_LB=0'; C_VAL3='RATE_D=0'
             C_VAL4='CHGLIMIT=0'
             WRITE (IOUT,810) TRIM(C_VAL1), TRIM(C_VAL2), TRIM(C_VAL3), &
                  TRIM(C_VAL4)
          ENDIF
       CASE DEFAULT
          PT%D_RATE=ZERO; PT%DAMP_A=0; PT%MIN_DAMP=ZERO; PT%LIMHDCHG=ZERO
          C_VAL1='ADAMP=0'; C_VAL2='DAMP_LB=0'; C_VAL3='RATE_D=0'
          C_VAL4='CHGLIMIT=0'
          WRITE (IOUT,810) TRIM(C_VAL1), TRIM(C_VAL2), TRIM(C_VAL3), &
               TRIM(C_VAL4)
       END SELECT
       ! ... 
       ! ... PRINT STYLE, NONLINEAR SOLVE
       ! ... 
       SELECT CASE(PT%CNVG_A)
       CASE(0)
          WRITE (IOUT,710)
       CASE(1)
          WRITE (IOUT,715)
       CASE(2)
          WRITE (IOUT,720)
          IF (PT%C_RATE>SMALL) THEN
             WRITE (IOUT,725)
          ELSE
             WRITE (IOUT,730)
          ENDIF
       END SELECT
       ! ... 
       SELECT CASE(PT%DAMP_A)
       CASE(0)
          WRITE (IOUT,735)
       CASE(1)
          WRITE (IOUT,740)
       CASE(2)
          WRITE (IOUT,745)
       END SELECT
    ENDIF
    NULLIFY(PT)
700 FORMAT (1X,10X,'LINEAR SOLUTION BY PCG ITERATION')
710 FORMAT (1X,10X,'PICARD: STANDARD RELATIVE CONVERGENCE, INNER ITERATION')
715 FORMAT (1X,10X,'PICARD: ADAPTIVE RELATIVE CONVERGENCE, &
         &INNER ITERATION')
720 FORMAT (1X,10X,'PICARD: ENHANCED RELATIVE CONVERGENCE, INNER ITERATION')
725 FORMAT (1X,10X,'PICARD: INITIAL ENHANCEMENT; INCREASING BY RATE_C')
730 FORMAT (1X,10X,'PICARD: CONSTANT ENHANCEMENT THROUGHOUT NONLINEAR SOLVE')
735 FORMAT (1X,10X,'PICARD: DAMPING HELD CONSTANT')
740 FORMAT (1X,10X,'PICARD: ADAPTIVE DAMPING')
745 FORMAT (1X,10X,'PICARD: INITIALLY ENHANCED DAMPING, INCREASING BY &
         &RATE_D')
    ! ...
800 FORMAT (1X,/,1X,10X,'***BAD DATA READ, SUBROUTINE PCGNRP***',/, & 
         1X,8X,'VARIABLES READ WERE: ',A40,/, &
         1X,8X,'DATA READ WERE: ', A50)
810 FORMAT (1X,/,1X,20X,'***WARNING!!!***',/, &
         1X,8X,'INCONSISTENT DATA HAS BEEN ENTERED',/, &
         1X,8X,'THE FOLLOWING VARIABLES HAVE BEEN RESET',/, &
         'TO DEFAULT VALUES: ', A12,', ', A12,', ', A12,', ', A12)
    ! ... 
  END SUBROUTINE PCGN2AR
  ! ... *****************************************************************

  ! ... *****************************************************************
  SUBROUTINE PCGN2AP(HNEWAP,RHSAP,CRAP,CCAP,CVAP,HCOFAP,IBDAP, &  
       KKITER,KKSTP,KKPER,ICNVG,HNOFLO,IGRID)
    ! ... 
    ! ... ***************************************************************
    ! ... PURPOSE: INTERFACE FOR PCG PACKAGE
    ! ...          CALLS SUBROUTINE PCG_INIT AND PCG
    ! ...          CONTAINS NON-LINEAR ITERATION ALGORITHM
    ! ... ***************************************************************
    ! ... 
    ! ... SPECIFICATIONS:
    ! ... ---------------------------------------------------------------
    ! ...    ARGUMENT LIST
    ! ... ---------------------------------------------------------------
    INTEGER, INTENT(IN) :: KKITER, KKSTP, KKPER, IGRID
    INTEGER, INTENT(INOUT)  :: ICNVG
    REAL, INTENT(IN) :: HNOFLO
    INTEGER, DIMENSION(*), TARGET, INTENT(INOUT) :: IBDAP
    REAL, DIMENSION(*), TARGET, INTENT(IN) :: CRAP, CCAP, CVAP, HCOFAP
    REAL, DIMENSION(*), TARGET, INTENT(INOUT)  :: RHSAP
    DOUBLE PRECISION, DIMENSION(*), TARGET, INTENT(INOUT) :: HNEWAP
    ! ... 
    ! ... CRAP, CCAP, CVAP, HCOFAP, RHSAP, HNEWAP, IBDAP: 
    ! ...   SURROGATES FOR CR, CC, CV, HCOF, RHS, HNEW, IBOUND
    ! ... KITER: OUTER ITERATION COUNTER
    ! ... KSTP: TIME STEP COUNTER
    ! ... KPER: STRESS PERIOD COUNTER
    ! ... ---------------------------------------------------------------
    ! ...    LOCAL VARIABLES
    ! ... ---------------------------------------------------------------
    LOGICAL, SAVE :: INIT=.TRUE.
    INTEGER, DIMENSION(1:10), SAVE :: NITER=0
    INTEGER, SAVE :: X_FLAG, PREV_IGRID=0
    DOUBLE PRECISION :: RTR
    INTEGER, DIMENSION(1:3) :: MRKR
    INTEGER, DIMENSION(1:2) :: ERR_STAT
    CHARACTER(LEN=32), DIMENSION(1:3), SAVE :: MRKR_NAM
    ! ... ===============================================================
    KITER=KKITER; KSTP=KKSTP; KPER=KKPER
    IF (INIT) THEN
       ! ... MARKER NAMES FOR PCG TIME AND CONVERGENCE UNITS
       MRKR_NAM(1)='STRESS PERIOD'
       MRKR_NAM(2)='TIME STEP'
       MRKR_NAM(3)='OUTER ITERATION'
       INIT=.FALSE.
    ENDIF
    ERR_STAT=0
    IF (IGRID/=PREV_IGRID) THEN
       ! ... SET POINTER
       ! ... 
       PT=>PCGNDAT(IGRID)
       ! ... 
       ! ... POINT POINTERS IN PREAMBLE TO ASSUMED-SIZE ARRAYS
       ! ... 
       IBOUND=>IBDAP(1:NODES); CR=>CRAP(1:NODES); CC=>CCAP(1:NODES)
       CV=>CVAP(1:NODES); HCOF=>HCOFAP(1:NODES); RHS=>RHSAP(1:NODES)
       HNEW=>HNEWAP(1:NODES)
       ! ...
       X_FLAG=2
       IF (PT%MO_ITER==1.OR.PT%CNVG_A==1) X_FLAG=1
       CALL PCG_INIT(PT%FILL, NCOL, NROW, NLAY, PT%DD, PT%DX, PT%DY, PT%DZ, &
            PT%DRELAX, X_FLAG, PT%TS_UNIT, PT%PC_UNIT, MRKR_NAM, &
            ERR_STAT)
       IF (ERR_STAT(1)/=0) THEN
          IF (ERR_STAT(1)==10) THEN
             WRITE(IOUT,500) ERR_STAT(2)
             WRITE(IOUT,510)
          ELSE
             WRITE(IOUT,540)
             WRITE(IOUT,510)
          ENDIF
          CALL PCGN_DEALLOCATE(MGRID)
          STOP
       ENDIF
       PREV_IGRID=IGRID
    ENDIF
    ! ... SET MARKERS  VALUES
    MRKR(1)=KKPER; MRKR(2)=KKSTP; MRKR(3)=KKITER
    ! ... FORM COEFFICIENTS, RESIDUAL VECTOR
    PT%DX=ZERO; PT%DY=ZERO; PT%DZ=ZERO 
    CALL CDS_RES(RTR,HNOFLO,PT%PRGUNIT,PT%DD,PT%DX,PT%DY,PT%DZ,PT%RES)
    ICNVG=0
    IF (PT%MO_ITER==1) THEN
       ! ... SOLVE LINEAR PROBLEM
       CALL LINEAR(PT%MI_ITER,PT%RCLOSE,MRKR,NITER(IGRID),ICNVG,ERR_STAT)
    ELSE
       CALL NONLINEAR(PT%MO_ITER,PT%MI_ITER,PT%CNVG_A,PT%DAMP_A, &
            PT%PRGUNIT,IGRID,PT%RCLOSE,PT%HCLOSE,PT%SAV_DAMP, &
            PT%MAG_CLOSE,PT%SAV_CLOSE,PT%C_RATE,PT%D_RATE, &
            PT%LIMHDCHG,PT%MIN_DAMP,PT%MIN_CLOSE,RTR,MRKR, &
            NITER(IGRID),ICNVG,ERR_STAT)
    ENDIF
    IF (ERR_STAT(1)/=0) THEN
       IF (ERR_STAT(1)==10) THEN
          WRITE(IOUT,500) ERR_STAT(2)
          WRITE(IOUT,515)
       ELSEIF (ERR_STAT(1)==15) THEN
          CALL PIVOT_CK(ERR_STAT(2))
          WRITE(IOUT,515)
       ELSEIF (ERR_STAT(1)==20) THEN
          WRITE(IOUT,520)
          WRITE(IOUT,515)
       ELSE
          WRITE(IOUT,540)
          WRITE(IOUT,515)
       ENDIF
       CALL PCGN_DEALLOCATE(MGRID)
       STOP
    ENDIF
    IF (ICNVG/=0.AND.PT%PRGUNIT>-1) &
         WRITE (IOUT,550) KKITER, KKSTP, KKPER, NITER(IGRID)
500 FORMAT (/,1X,' UNABLE TO ALLOCATE STORAGE REQUIRED FOR', & 
         ' PCG SOLVER, ERROR CODE: ',I6)
505 FORMAT (/,1X,' ERROR ENCOUNTERED IN SUBROUTINE PCGNRP, &
         &MODULE PCGN.')
510 FORMAT (/,1X,' ERROR ENCOUNTERED IN SUBROUTINE PCG_INIT, &
         &MODULE PCG_MAIN.')
515 FORMAT (/,1X,' ERROR ENCOUNTERED IN SUBROUTINE PCG, &
         &MODULE PCG_MAIN.')
520 FORMAT (/,1X,' DIMENSION MISMATCH DISCOVERED;')
540 FORMAT (/,1X,' UNKNOWN ERROR;')
550 FORMAT (/,1X,I4,' CALLS TO PCG ROUTINE FOR TIME STEP',I4, &     
         ' IN STRESS PERIOD ',I4,/,I8,' TOTAL ITERATIONS')
    ! ...
  END SUBROUTINE PCGN2AP
  ! ... ***************************************************************

  ! ... ***************************************************************
    SUBROUTINE LINEAR(MI_ITER,RCLOSE,MRKR,NITER,ICNVG,ERR_STAT)
      ! ... SOLVE LINEAR PROBLEM
      ! ... -------------------------------------------------------------
      ! ...    ARGUMENT LIST
      ! ... -------------------------------------------------------------
      INTEGER, INTENT(IN) :: MI_ITER
      DOUBLE PRECISION, INTENT(IN) :: RCLOSE
      INTEGER, DIMENSION(1:3), INTENT(IN) :: MRKR
      INTEGER, INTENT(INOUT) :: NITER, ICNVG
      INTEGER, DIMENSION(1:2), INTENT(INOUT) :: ERR_STAT
      ! ... -------------------------------------------------------------
      ! ...    LOCAL VARIABLES: 
      ! ... -------------------------------------------------------------
      DOUBLE PRECISION :: DRCLOSE
      ! ... =============================================================
      NITER=MI_ITER; DRCLOSE=RCLOSE
      ! ... CALL PCG SOLVER WITH ABSOLUTE CONVERGENCE
      CALL PCG(RESID=PT%RES, XX=HNEW, CONVG_I=DRCLOSE, MAX_ITER=NITER, &
           C_FLAG=1, MARKER=MRKR, ERR_STAT=ERR_STAT)
      IF (ERR_STAT(1)/=0) RETURN
      ! ... SOLVER TERMINATION CONTROL, LINEAR PROBLEM 
      IF (NITER>0) THEN
         WRITE (IOUT,510) NITER, KSTP, KPER
         WRITE (IOUT,525) SQRT(DRCLOSE)
         ICNVG=1
      ELSE
         NITER=-NITER
         WRITE (IOUT,520) KSTP, KPER, NITER
         WRITE (IOUT,530) SQRT(DRCLOSE)
      ENDIF
      ! ... -------------------------------------------------------------
510   FORMAT (/,1X,I4,' TOTAL ITERATIONS FOR TIME STEP',I4, &     
           ' IN STRESS PERIOD ',I4)
520   FORMAT (/,1X,' TIME STEP',I4,' IN STRESS PERIOD ',I4,/, &     
           ' DID NOT CONVERGE IN',I4,' ITERATIONS')
525   FORMAT (/,1X,' RESIDUAL L2 NORM AT CONVERGENCE: ',1PE12.5)
530   FORMAT (/,1X,' RESIDUAL L2 NORM AT TERMINATION: ',1PE12.5)
      ! ... -------------------------------------------------------------
    END SUBROUTINE LINEAR
  ! ... ***************************************************************

  ! ... ***************************************************************
    SUBROUTINE NONLINEAR(MO_ITER,MI_ITER,CNVG_A,DAMP_A,IPUNIT,IGRID, &
         RCLOSE,HCLOSE,SAV_DAMP,MAG_CLOSE,SAV_CLOSE,C_RATE,D_RATE, &
         LIMHDCHG,MIN_DAMP,MIN_CLOSE,RTR,MRKR,NITER,ICNVG,ERR_STAT)
      ! ... SOLVE NONLINEAR PROBLEM BY PICARD ITERATION
      ! ... SET MI_ITER LOOSELY; INNER ITERATION CONSTRAINED BY
      ! ... SPECIFIED REDUCTION IN INITIAL L2 NORM (EPS_I).
      ! ... 
      ! ... -------------------------------------------------------------
      ! ...    ARGUMENT LIST
      ! ... -------------------------------------------------------------
      INTEGER, INTENT(IN) :: MO_ITER, MI_ITER, CNVG_A, DAMP_A, IPUNIT, &
           IGRID
      DOUBLE PRECISION, INTENT(IN) :: RCLOSE, HCLOSE, SAV_DAMP, MAG_CLOSE, &
           SAV_CLOSE, C_RATE, D_RATE, LIMHDCHG, MIN_DAMP, MIN_CLOSE, RTR
      INTEGER, DIMENSION(1:3), INTENT(IN) :: MRKR
      INTEGER, INTENT(INOUT) :: NITER, ICNVG
      INTEGER, DIMENSION(1:2), INTENT(INOUT) :: ERR_STAT
      ! ... -------------------------------------------------------------
      ! ...    LOCAL VARIABLES
      ! ... -------------------------------------------------------------
      INTEGER :: I, NO_ITER, MHC_NODE, C_FLAG
      INTEGER, SAVE :: NET_CHANGE, IB0_COUNT
      INTEGER, SAVE :: IB0_BASE(10), NC_BASE(10), TOT_CELLS(10)=0
      DOUBLE PRECISION :: EPS_O, EPS_IP, HTH, L2HR, RATIO_L, HLAST, HTHIS
      DOUBLE PRECISION, SAVE :: EPS_I, MXHCH, DDAMP, RTRP=ZERO, PCG_CLOSE
      ! ... =============================================================
      ! ...                         ***NOTES***
      ! ... IN SUBSEQUENT CALLS TO SUBROUTINE PCG, THE ARGUMENT LIST VARIABLES 
      ! ... FREQUENTLY HAVE DUAL PURPOSES.  VARIABLES ON INPUT CARRY A 
      ! ... DIFFERENT SIGNIFICANCES THAN THE SAME VARIABLES ON OUTPUT.
      ! ... REFER TO MODULE PCG_MAIN FOR THE SIGNIFICANCE OF THESE 
      ! ... DIFFERENCES.
      ! ... 
      ! ... CNVG_A=0: STANDARD CONVERGENCE
      ! ... CNVG_A=1: EPS_I CAN BE ALTERED IN MODULE PCG SOLVE (ADAPTIVE)
      ! ... CNVG_A=2: PCG CLOSE ALTERED WITHIN PICARD ITERATION (ENHANCE)
      ! ... 
      ! ... DAMP_A=0: CONSTANT DAMPING
      ! ... DAMP_A=1: ADAPTIVE DAMPING
      ! ... DAMP_A=2: ENHANCED DAMPING
      ! ... 
      ! ... CHECK RESIDUAL-BASED CONVERGENCE
      ! ...
      IF (SQRT(RTR)<REAL(RCLOSE)) THEN
         ! ... CONVERGENCE BASED ON RESIDUAL L2 NORM
         IF (IPUNIT>-1) WRITE (IOUT,500) SQRT(RTR), MXHCH
         ICNVG=1
         ! ... END OF TIME STEP UPDATE TO DRY CELLS
         IB0_BASE(IGRID)=IB0_COUNT
         TOT_CELLS(IGRID)=TOT_CELLS(IGRID)+NET_CHANGE
         NC_BASE(IGRID)=NC_BASE(IGRID)+NET_CHANGE
         IF (IPUNIT>-1) WRITE (IOUT,530) TOT_CELLS(IGRID), KSTP, KPER
         RETURN
      ENDIF
      ! ... 
      IF (KITER==1) THEN
         IF (CNVG_A==1) THEN
            CALL PROG1AD(KPER,KSTP,IPUNIT)
         ELSE !CNVG_A=2
            CALL PROG2AD(KPER,KSTP,IPUNIT)
         ENDIF
      ENDIF
      IF (KSTP==1.AND.KITER==1) THEN
         IF (KPER==1) THEN
            ! ... INITIAL STRESS PERIOD/TIME STEP ASSUMED TO BE MOST DIFFICULT
            ! ... ABSOLUTE CONVERGENCE
            C_FLAG=1
            EPS_IP=ZERO
            EPS_I=SQRTEN
            EPS_O=-ONE
            ! ... SET INITIAL VALUES FOR PCG_CLOSE AND DDAMP
            SELECT CASE(CNVG_A)
            CASE(0,1)
               PCG_CLOSE=SAV_CLOSE
            CASE(2)
               PCG_CLOSE=MAG_CLOSE               
            END SELECT
            SELECT CASE(DAMP_A)
            CASE(0)
               DDAMP=SAV_DAMP
            CASE(1)
               DDAMP=SQRT(SAV_DAMP*MIN_DAMP)
            CASE(2)
               DDAMP=MIN_DAMP
            END SELECT
         ELSE
            ! ... NEW STRESS PERIOD; RESET DAMPING AND CONVERGENCE
            ! ... ASSUMES PRIOR INNER ITERATION AVAILABLE
            ! ... RELATIVE CONVERGENCE
            C_FLAG=2
            EPS_IP=EPS_I
            SELECT CASE(CNVG_A)
            CASE(0)
               EPS_I=SAV_CLOSE**2
            CASE(1)
               EPS_I=SAV_CLOSE**2
               EPS_O=-ONE
            CASE(2)
               IF (C_RATE>SMALL) THEN
                  PCG_CLOSE=SQRT(MAG_CLOSE*SAV_CLOSE)
                  EPS_I=PCG_CLOSE
               ENDIF
            END SELECT
            SELECT CASE(DAMP_A)
            CASE(1)
               DDAMP=EXP((TWO*LOG(SAV_DAMP)+LOG(MIN_DAMP))/THREE)
            CASE(2)
               DDAMP=SQRT(SAV_DAMP*MIN_DAMP)
            END SELECT
         ENDIF
         NO_ITER=MI_ITER
         PT%HCH=ZERO
         ! ... FIRST INNER ITERATION; CALL PCG SOLVER
         CALL PCG(RESID=PT%RES, XX=PT%HCH, CONVG_I=EPS_I, MAX_ITER=NO_ITER, &
              C_FLAG=C_FLAG, CONVG_O=EPS_O, MARKER=MRKR, ERR_STAT=ERR_STAT)
         IF (ERR_STAT(1)/=0) RETURN
         IF (NO_ITER<0) THEN
            NITER=-NO_ITER
         ELSE
            NITER=NO_ITER
         ENDIF
         ! ... FIND MAXIMUM HEAD CHANGE
         CALL MAX_HCH(RTR,MXHCH,MHC_NODE,IB0_COUNT,L2HR)
         CALL DAMP_ADJ(DAMP_A,KITER,KSTP,MIN_DAMP,SAV_DAMP,L2HR,MXHCH, &
              D_RATE,LIMHDCHG,DDAMP) 
         ! ... UPDATE SOLUTION
         IF (IPUNIT>0) HLAST=HNEW(MHC_NODE)
         CALL UPDATE_HEAD(HNEW,PT%HCH,DDAMP)
         IF (IPUNIT>0) HTHIS=HNEW(MHC_NODE)
         ! ... UPDATE INACTIVE CELL COUNT
         IF (KPER==1.AND.KSTP==1) THEN
            ! ... STARTING VALUE FOR IB0_BASE
            IB0_BASE(IGRID)=IB0_COUNT; NET_CHANGE=0; NC_BASE(IGRID)=0 
         ELSE
            NET_CHANGE=IB0_COUNT-IB0_BASE(IGRID)
            IF (KSTP==1) NC_BASE(IGRID)=0 
         ENDIF
         ! ... PRINT PROGRESS
         IF (IPUNIT>0) THEN
            IF (CNVG_A==1) THEN
               RATIO_L=EPS_IP/EPS_O
               CALL PROG1OT(KITER,NET_CHANGE+NC_BASE(IGRID),RATIO_L,L2HR, &
                    MXHCH,MHC_NODE,DDAMP,HLAST,HTHIS,IPUNIT,NCOL,NROW)
            ELSE ! CNVG_A=2
               CALL PROG2OT(KITER,NET_CHANGE+NC_BASE(IGRID),L2HR,MXHCH, &
                    MHC_NODE,DDAMP,HLAST,HTHIS,IPUNIT,NCOL,NROW)
            ENDIF
         ENDIF
         ! ... CHECK MAXIMUM HEAD CHANGE NORM
         CALL SECONDARY_CLOSE(MXHCH,RCLOSE,HCLOSE,RTR,ICNVG,KITER,MO_ITER, &
              IPUNIT)
      ELSE ! KSTP>1
         IF (CNVG_A==2) THEN
            ! ... RELAX CLOSURE CRITERION FOR PCG SOLVER
            ! ... PCG_CLOSE ALWAYS COMPARED TO STANDARD CONVERGENCE
            IF (C_RATE>SMALL.AND.RTR<RTRP) THEN
               IF (SAV_CLOSE>PCG_CLOSE) THEN
                  PCG_CLOSE=PCG_CLOSE+C_RATE*PCG_CLOSE
               ELSE
                  PCG_CLOSE=SAV_CLOSE
               ENDIF
            ENDIF
            ! ...
         ENDIF
         ! ... SAVE MAX HEAD AND EXIT VALUE OF EPS_I 
         EPS_IP=EPS_I
         ! ... RELATIVE CONVERGENCE
         C_FLAG=2
         EPS_I=PCG_CLOSE
         EPS_O=MIN_CLOSE
         NO_ITER=MI_ITER
         ! ... INNER ITERATION; CALL PCG SOLVER
         PT%HCH=ZERO
         CALL PCG(RESID=PT%RES, XX=PT%HCH, CONVG_I=EPS_I, MAX_ITER=NO_ITER, &
              C_FLAG=C_FLAG, CONVG_O=EPS_O, MARKER=MRKR, ERR_STAT=ERR_STAT)
         IF (ERR_STAT(1)/=0) RETURN
         IF (NO_ITER<0) THEN
            NITER=NITER-NO_ITER
         ELSE
            NITER=NITER+NO_ITER
         ENDIF
         ! ... UPDATE DAMPING
         CALL MAX_HCH(RTR,MXHCH,MHC_NODE,IB0_COUNT,L2HR)
         CALL DAMP_ADJ(DAMP_A,KITER,KSTP,MIN_DAMP,SAV_DAMP,L2HR,MXHCH, &
              D_RATE,LIMHDCHG,DDAMP) 
         ! ... UPDATE SOLUTION
         IF (IPUNIT>0) HLAST=HNEW(MHC_NODE)
         CALL UPDATE_HEAD(HNEW,PT%HCH,DDAMP)
         IF (IPUNIT>0) HTHIS=HNEW(MHC_NODE)
         ! ... UPDATE INACTIVE CELL COUNT AND PRINT PROGRESS
         NET_CHANGE=IB0_COUNT-IB0_BASE(IGRID)
         IF (IPUNIT>0) THEN
            IF (CNVG_A==1) THEN
               RATIO_L=EPS_IP/EPS_O
               CALL PROG1OT(KITER,NET_CHANGE+NC_BASE(IGRID),RATIO_L,L2HR, &
                    MXHCH,MHC_NODE,DDAMP,HLAST,HTHIS,IPUNIT,NCOL,NROW)
            ELSE ! CNVG_A=2
               CALL PROG2OT(KITER,NET_CHANGE+NC_BASE(IGRID),L2HR,MXHCH, &
                    MHC_NODE,DDAMP,HLAST,HTHIS,IPUNIT,NCOL,NROW)
            ENDIF
         ENDIF
         ! ... CHECK MAXIMUM HEAD CHANGE NORM
         CALL SECONDARY_CLOSE(MXHCH,RCLOSE,HCLOSE,RTR,ICNVG,KITER,MO_ITER, &
              IPUNIT)
      ENDIF
      IF (ICNVG/=0) THEN
         ! ... END OF TIME STEP UPDATE TO DRY CELLS
         IB0_BASE(IGRID)=IB0_COUNT
         TOT_CELLS(IGRID)=TOT_CELLS(IGRID)+NET_CHANGE
         NC_BASE(IGRID)=NC_BASE(IGRID)+NET_CHANGE
         IF (IPUNIT>-1) WRITE (IOUT,530) TOT_CELLS(IGRID), KSTP, KPER
      ENDIF
      RTRP=RTR
      ! ... -----------------------------------------------------------
500   FORMAT (/,1X,' CONVERGENCE DECLARED BASED ON MAXIMUM L2 NORM &
           &CRITERION',/,1X,' RESIDUAL L2 NORM AT CONVERGENCE: ', &
           1PE12.5,/,1X,' MAXIMUM HEAD CHANGE AT CONVERGENCE: ',1PE12.5)
530   FORMAT (/,1X,I7,' TOTAL DRY CELLS BY TIME STEP',I4, &     
           ' IN STRESS PERIOD ',I4)
      ! ... -----------------------------------------------------------
    END SUBROUTINE NONLINEAR
  ! ... ***************************************************************

  ! ... ***************************************************************
  SUBROUTINE CDS_RES(RTR, HNOFLO, IPUNIT, DD, DX, DY, DZ, RES)
    ! ...
    ! ... ******************************************************************
    ! ... PURPOSE: BUILD AND STORE MATRIX IN COMPRESSED DIAGONAL FORMAT; 
    ! ... FORM RESIDUAL VECTOR
    ! ... ******************************************************************
    ! ...
    ! ... ------------------------------------------------------------------
    ! ...    ARGUMENT LIST
    ! ... ------------------------------------------------------------------
    INTEGER :: IPUNIT
    REAL, INTENT(IN) :: HNOFLO
    DOUBLE PRECISION, INTENT(OUT) :: RTR
    DOUBLE PRECISION, DIMENSION(:), INTENT(OUT) :: DD, DX, DY, DZ, RES
    ! ... ------------------------------------------------------------------
    ! ...    LOCAL VARIABLES
    ! ... ------------------------------------------------------------------
    INTEGER :: I, J, K, N, LEVEL_CT, ROW_CT, NRN, NRL, NCN, NCL, &     
         NLN, NLL, NRC
    DOUBLE PRECISION :: RR, EE, COND
    ! ... ==================================================================
    NRC=NROW*NCOL; RTR=ZERO
    DO K=1,NLAY
       LEVEL_CT=(K-1)*NRC
       DO I=1,NROW
          ROW_CT=(I-1)*NCOL
          DO J=1,NCOL
             N = LEVEL_CT+ROW_CT+J
             ! ...
             ! ... CALCULATE 1 DIMENSIONAL SUBSCRIPT OF CURRENT CELL AND
             ! ... SKIP CALCULATIONS IF CELL IS INACTIVE
             IF (IBOUND(N).LE.0) THEN
                DD(N)=ONE; RES(N)=ZERO
                CYCLE
             ENDIF
             RR=-RHS(N)
             EE=ZERO
             ! ...
             ! ... CALCULATE 1 DIMENSIONAL SUBSCRIPTS FOR LOCATING THE 6
             ! ... SURROUNDING CELLS
             NRN = N + NCOL
             NRL = N - NCOL
             NCN = N + 1
             NCL = N - 1
             NLN = N + NRC
             NLL = N - NRC
             ! ...
             ! ... GET CONDUCTANCES TO NEIGHBORING CELLS
             ! ... NEIGHBOR IS 1 ROW BACK
             IF (I.NE.1) THEN
                IF (IBOUND(NRL)/=0) THEN
                   COND = CC(NRL)
                   EE=EE+COND
                   RR=RR+COND*HNEW(NRL)
                ENDIF
             ENDIF
             ! ...
             ! ... NEIGHBOR IS 1 ROW AHEAD
             IF (I.NE.NROW) THEN
                IF (IBOUND(NRN)/=0) THEN
                   COND = CC(N)
                   EE=EE+COND
                   RR=RR+COND*HNEW(NRN)
                   IF (IBOUND(NRN)>0) THEN
                      DY(N)=-COND
                   ENDIF
                ENDIF
             ENDIF
             ! ...
             ! ... NEIGHBOR IS 1 COLUMN BACK
             IF (J.NE.1) THEN
                IF (IBOUND(NCL)/=0) THEN
                   COND = CR(NCL)
                   EE=EE+COND
                   RR=RR+COND*HNEW(NCL)
                ENDIF
             ENDIF
             ! ...
             ! ... NEIGHBOR IS 1 COLUMN AHEAD
             IF (J.NE.NCOL) THEN
                IF (IBOUND(NCN)/=0) THEN
                   COND = CR(N)
                   EE=EE+COND
                   RR=RR+COND*HNEW(NCN)
                   IF (IBOUND(NCN)>0) THEN
                      DX(N)=-COND
                   ENDIF
                ENDIF
             ENDIF
             ! ...
             ! ... NEIGHBOR IS 1 LAYER BEHIND
             IF (K.NE.1) THEN
                IF (IBOUND(NLL)/=0) THEN
                   COND = CV(NLL)
                   EE=EE+COND
                   RR=RR+COND*HNEW(NLL)
                ENDIF
             ENDIF
             ! ...
             ! ... NEIGHBOR IS 1 LAYER AHEAD
             IF (K.NE.NLAY) THEN
                IF (IBOUND(NLN)/=0) THEN
                   COND = CV(N)
                   EE=EE+COND
                   RR=RR+COND*HNEW(NLN)
                   IF (IBOUND(NLN)>0) THEN
                      DZ(N)=-COND
                   ENDIF
                ENDIF
             ENDIF
             ! ...    
             ! ... MAKE CELL INACTIVE IF ALL SURROUNDING CELLS ARE INACTIVE 
             ! ... AND SET HNEW TO HNOFLO
             IF (ABS(EE).LT.SMALL) THEN
                IBOUND(N) = 0
                HNEW(N) = HNOFLO
                DD(N)=ONE; RES(N)=ZERO
                IF (IPUNIT>-1) WRITE(IOUT,500) J, I, K
                CYCLE
             ENDIF
             ! ...
             ! ... SET THE DIAGONAL AS A POSITIVE VALUE.
             EE=EE-REAL(HCOF(N),8)
             DD(N)=EE
             RR=RR-EE*HNEW(N)
             RES(N)=RR
             RTR=RTR+RR**2
          ENDDO
       ENDDO
    ENDDO
500 FORMAT(1X,'PCGN ASSEMBLER HAS DISCOVERED AN ADDITIONAL INACTIVE CELL'&
         ,/,' MESH LOCATION: J=',I4,', I=',I4,', K=',I4)
    ! ...
  END SUBROUTINE CDS_RES
  ! ... ***************************************************************

  ! ... ***************************************************************
  SUBROUTINE MAX_HCH(RTR, MXHCH, MHC_NODE, IB0_COUNT, L2HR)
    ! ...
    ! ... ******************************************************************
    ! ... PURPOSE: EXTRACT LARGEST ABSOLUTE HEAD CHANGE;
    ! ... FORM HEAD-CHANGE L2 NORM; PRINT MESSAGE
    ! ... ******************************************************************
    ! ...
    ! ... ---------------------------------------------------------
    ! ...    ARGUMENT LIST
    ! ... ---------------------------------------------------------
    DOUBLE PRECISION, INTENT(IN) :: RTR
    DOUBLE PRECISION, INTENT(OUT) :: MXHCH, L2HR
    INTEGER, INTENT(OUT) :: MHC_NODE, IB0_COUNT
    ! ... --------------------------------------------------------------
    ! ...    LOCAL VARIABLES
    ! ... --------------------------------------------------------------
    INTEGER :: I, J, K, N, NODE_SAVE
    DOUBLE PRECISION :: HTH, HS
    ! ... ==============================================================
    MXHCH=ZERO; HTH=ZERO; IB0_COUNT=0
    DO N=1,NODES
       IF (IBOUND(N)<1) THEN
          IF (IBOUND(N)==0) IB0_COUNT=IB0_COUNT+1
          CYCLE
       ENDIF
       HTH=HTH+PT%HCH(N)**2
       IF (ABS(PT%HCH(N))>ABS(MXHCH)) THEN
          NODE_SAVE=N
          MXHCH=PT%HCH(N)
       ENDIF
    ENDDO
    MHC_NODE=NODE_SAVE
    L2HR=SQRT(HTH*RTR)
    ! ...
  END SUBROUTINE MAX_HCH
  ! ... ***************************************************************

  ! ... ***************************************************************
  SUBROUTINE SECONDARY_CLOSE(MXHCH,RCLOSE,HCLOSE,RTR,ICNVG,KITER,MO_ITER, &
       IPUNIT)
    ! ...
    ! ... RLN:  MODIFIED 02/2010; ELIMINATED RETURNING ICNVG=-1 WHEN 
    ! ...         KITER=MO_ITER (OLD MF2000 SETTING).
    ! ... ACCEPTABLE VALUES FOR ICNVG WITH MF2005 ARE 0 (NO CONVERGENCE) 
    ! ... OR 1 (CONVERGENCE).
    ! ... ******************************************************************
    ! ... PURPOSE: SECONDARY SOLVER TERMINATION CHECK, NONLINEAR PROBLEM;
    ! ... CHECKS FOR CLOSURE AGAINST MAXIMUM HEAD CHANGE.
    ! ... RTR: SQUARE OF L2 NORM AT TERMINATION.
    ! ... ******************************************************************
    ! ...
    ! ... ---------------------------------------------------------
    ! ...    ARGUMENT LIST
    ! ... ---------------------------------------------------------
    INTEGER, INTENT(IN) :: KITER, MO_ITER,IPUNIT
    INTEGER, INTENT(INOUT) :: ICNVG
    DOUBLE PRECISION, INTENT(IN) :: MXHCH, RCLOSE, HCLOSE, RTR
    ! ... ---------------------------------------------------------
    ! ...    LOCAL VARIABLES
    ! ... ---------------------------------------------------------
    INTEGER, SAVE :: NHC, NHC_CK
    ! ... ---------------------------------------------------------
    IF (KITER==1) THEN
       NHC=0; NHC_CK=0
       RETURN
    ENDIF
    IF (KITER<MO_ITER) THEN
       ! ... CHECK MAXIMUM HEAD CHANGE NORM
       IF (ABS(MXHCH)<HCLOSE) THEN
          NHC=NHC+1; NHC_CK=NHC_CK+1
          IF (NHC>2.AND.IPUNIT>-1) THEN
             WRITE (IOUT,500) 
             IF (NHC==NHC_CK) THEN
                WRITE (IOUT,510) MXHCH
             ELSE
                WRITE (IOUT,520) MXHCH
             ENDIF
             WRITE (IOUT,530) SQRT(RTR)
             ICNVG=1
             IF (SQRT(RTR)>HUNDRED*REAL(RCLOSE)) WRITE (IOUT,525)
          ENDIF
       ELSE
          NHC_CK=0
       ENDIF
    ELSE
       ! ... NO OUTER CONVERGENCE: KITER=MO_ITER.
       ! ... SIMULATION IS TERMINATED BY MF2005 MAIN
       ! ... WITH APPROPRIATE MESSAGE.
       WRITE (IOUT,540) SQRT(RTR)
    ENDIF
    ! ... -----------------------------------------------------------
500 FORMAT (/,1X,' CONVERGENCE DECLARED BASED ON MAXIMUM HEAD CHANGE &
         &CRITERION')
510 FORMAT (1X,' MAXIMUM HEAD CHANGE AT CONVERGENCE: ',1PE12.5)
520 FORMAT (6X,' ***PROVISIONAL CONVERGENCE***',/,1X, &     
         ' MAXIMUM HEAD CHANGE AT CONVERGENCE: ',1PE12.5, &     
         /,1X,' CHECK MASS BALANCE FOR THIS TIME STEP')
525 FORMAT (11X,' ***WARNING***', &
         /,1X,' LARGE RESIDUAL L2 NORM FOR THIS SOLUTION', &
         /,1X,' CHECK THAT MASS BALANCE ERROR NOT EXCESSIVE')
530 FORMAT (1X,' RESIDUAL L2 NORM AT CONVERGENCE: ',1PE12.5)
540 FORMAT (/,1X,' RESIDUAL L2 NORM AT TERMINATION: ',1PE12.5)
    ! ... -----------------------------------------------------------
  END SUBROUTINE SECONDARY_CLOSE
  ! ... ***************************************************************

  ! ... ***************************************************************
  SUBROUTINE DAMP_ADJ(DAMP_A,KITER,KSTP,MIN_DAMP,SAV_DAMP,EPS,MXHCH, &
       D_RATE,CHGLIMIT,DAMP)
    ! ...
    ! ... RLN:  MODIFIED 02/2010; FOR CASE DAMP_A=1, VALUES OF RATE_D>0.1 
    ! ...         NOT PERMITTED -- RATE_D RESET TO 0.1
    ! ... ******************************************************************
    ! ... PURPOSE: ADJUST DAMPING PARAMETER IN RESPONSE TO PROGRESS IN
    ! ... CONVERGENCE OF NONLINEAR PROBLEM.
    ! ... ******************************************************************
    ! ...
    ! ... DAMP_A: DETERMINES CASE
    ! ... CASE(1): ADAPTIVE DAMPING
    ! ... ADJUST DAMPING PARAMETER BASED ON CONVERGENCE HISTORY
    ! ... EPS/EPS_P<1 AND MXHCH/MXHCHP<1: CONVERGENT REALM  
    ! ... EPS/EPS_P>1 OR MXHCH/MXHCHP>1: NONCONVERGENT REALM
    ! ... EPS: PCG EXIT L2_NORM OF CURRENT PICARD ITERATION
    ! ... EPS_P: PCG EXITL2_NORM OF PREVIOUS PICARD ITERATION
    ! ... MXHCH: MAX HEAD CHANGE FOR CURRENT PICARD ITERATION
    ! ... D_RATE: SMALLEST VALUE OF RATIO_E FOR WHICH DAMPING ADJUSTED
    ! ... MIN_DAMP: MINIMUM ALLOWED DAMPING VALUE
    ! ... SAV_DAMP: MAXIMUM ALLOWED DAMPING VALUE
    ! ... CASE(2): ENHANCED DAMPING
    ! ... INCREASES DAMPING PARAMETER BY D_RATE PROVIDED THAT 
    ! ...   PICARD ITERATION SHOWS CONVERGENCE
    ! ... MIN_DAMP: STARTING DAMPING VALUE (SET ELSEWHERE)
    ! ... SAV_DAMP: MAXIMUM ALLOWED DAMPING VALUE
    ! ... 
    ! ... ---------------------------------------------------------
    ! ...    ARGUMENT LIST
    ! ... ---------------------------------------------------------
    INTEGER, INTENT(IN) :: DAMP_A,KITER, KSTP
    DOUBLE PRECISION, INTENT(IN) :: MIN_DAMP, SAV_DAMP, EPS, MXHCH, D_RATE, &
         CHGLIMIT
    DOUBLE PRECISION, INTENT(INOUT) :: DAMP
    ! ... ---------------------------------------------------------
    ! ...    LOCAL VARIABLES
    ! ... ---------------------------------------------------------
    INTEGER :: COUNT=0
    DOUBLE PRECISION :: RATIO_E, RATIO_H, LRATIO_E, LRATIO_R, RATIO_L
    DOUBLE PRECISION, SAVE ::  EPS_P, MXHCHP, DAMPP
    ! ... ---------------------------------------------------------
    SELECT CASE(DAMP_A)
    CASE(0)
       RETURN
    CASE(1)
       ! ... ADAPTIVE DAMPING BASED ON CONVERGENCE HISTORY
       IF (DAMP<=SMALL) THEN
          DAMP=MIN_DAMP
          EPS_P=EPS; MXHCHP=MXHCH
          RETURN
       ELSEIF(DAMP>SAV_DAMP) THEN
          DAMP=SAV_DAMP
          EPS_P=EPS; MXHCHP=MXHCH
          RETURN
       ELSEIF (KITER==1.AND.KSTP==1) THEN
          EPS_P=EPS; MXHCHP=MXHCH
          RETURN
       ENDIF
       RATIO_E=EPS/EPS_P
       IF (RATIO_E<ZERO) THEN
          WRITE (*,505) KITER, RATIO_E
          RATIO_E=ABS(RATIO_E)
       ENDIF
       RATIO_H=ABS(MXHCH/MXHCHP)
       ! ...
       DAMPP=DAMP
       ! ... INCREASE DAMPING PARAMETER
       IF (RATIO_H<=ONE.AND.RATIO_E<=ONE) THEN
          IF (D_RATE<=TENTH) THEN
             RATIO_L=LOG10(RATIO_E)/LOG10(D_RATE)
          ELSE
             ! ... PRIMARY PROTECTION FOR D_RATE IN SUBROUTINE PCGN2AR
             RATIO_L=-LOG10(RATIO_E)
          ENDIF
          ! xxx RATIO_L=LOG10(RATIO_E)/LOG10(D_RATE)
          IF (RATIO_L<ONE) THEN
             DAMP=DAMP+RATIO_L*(SAV_DAMP-DAMP)
          ELSE
             DAMP=SAV_DAMP
          ENDIF
          COUNT=0
       ENDIF
       ! ... DECREASE DAMPING PARAMETER
       IF (RATIO_H>ONE) THEN
          ! ... POSSIBLE DIVERGENCE THROUGH HEAD CHANGE
          DAMP=DAMP/RATIO_H
       ENDIF
       IF (RATIO_E>ONE) THEN
          ! ... POSSIBLE DIVERGENCE THROUGH L2 NORM
          DAMP=DAMP/RATIO_E
       ENDIF
       DAMP=SQRT(DAMP*DAMPP)
       ! ... LIMIT HEAD CHANGE
       IF (ABS(MXHCH)>CHGLIMIT.AND.DAMP>CHGLIMIT/ABS(MXHCH)) THEN
          DAMP=CHGLIMIT/ABS(MXHCH)
       ENDIF
       ! ... LOWER BOUND CHECK
       IF (DAMP<MIN_DAMP) THEN
          DAMP=MIN_DAMP
          COUNT=COUNT+1
          IF (COUNT>10) THEN
             ! ... DAMPING APPEARS STUCK AT MIN_DAMP; JAR IT LOOSE
             DAMP=EXP((2*LOG(MIN_DAMP)+LOG(SAV_DAMP))/THREE)
          ENDIF
       ENDIF
    CASE(2)
       ! ... D_RATE DAMPING RELAXATION
       IF (KITER==1.AND.KSTP==1) THEN
          EPS_P=EPS; MXHCHP=MXHCH
          RETURN
       ENDIF
       RATIO_E=EPS/EPS_P
       IF (RATIO_E<ZERO) THEN
          WRITE (*,505) KITER, RATIO_E
          RATIO_E=ABS(RATIO_E)
       ENDIF
       RATIO_H=ABS(MXHCH/MXHCHP)
       ! ... INCREASE DAMPING PARAMETER
       IF (DAMP<SAV_DAMP) THEN
          IF (RATIO_H<=ONE.AND.RATIO_E<=ONE) THEN
             DAMP=DAMP+D_RATE*DAMP
          ENDIF
       ENDIF
       IF (DAMP>SAV_DAMP) DAMP=SAV_DAMP
    END SELECT
    EPS_P=EPS; MXHCHP=MXHCH
    ! ... -----------------------------------------------------------
505 FORMAT(/,1X,' WARNING: RATIO_E NEGATIVE IN SUBROUTINE NONLINEAR', &
         /,10X,' OUTER ITERATION:', I5, ' RATIO_E:',1PE12.5)
    ! ... -----------------------------------------------------------
  END SUBROUTINE DAMP_ADJ
  ! ... ***************************************************************

  ! ... ***************************************************************
  SUBROUTINE PIVOT_CK(E_NODE)
    ! ...
    ! ... RLN:  MODIFIED 04/2009 FOR ERROR IN ROW-COLUMN INDEXING
    ! ... ******************************************************************
    ! ... PURPOSE: PRINT ZERO OR NEGATIVE PIVOT INFO.
    ! ... ******************************************************************
    ! ...
    ! ... ------------------------------------------------------------------
    ! ...   ARGUMENT LIST
    ! ... ------------------------------------------------------------------
    INTEGER :: E_NODE
    ! ... ------------------------------------------------------------------
    ! ...    LOCAL VARIABLES
    ! ... ------------------------------------------------------------------
    INTEGER :: MAX_COL, MIN_COL, MAX_ROW, MIN_ROW, MAX_LAY, MIN_LAY
    INTEGER :: II, JJ, KK 
    ! ... ==================================================================
    IF (E_NODE>0) THEN
       WRITE(IOUT,500) 
    ELSE
       ! ... NEGATIVE ERROR INDICATES NEGATIVE PIVOT
       E_NODE=-E_NODE
       WRITE(IOUT,505)
    ENDIF
    II=MOD(E_NODE-1,NCOL*NROW)/NCOL+1
    JJ=MOD(MOD(E_NODE-1,NCOL*NROW),NCOL)+1
    KK=(E_NODE-1)/(NCOL*NROW)+1
    WRITE(IOUT,510) JJ, II, KK
    CALL CK_DOMAIN
    WRITE(IOUT,515) 
    ! ...   
500 FORMAT (/,1X,' A ZERO PIVOT WAS ENCOUNTERED AT LOCATION:')
505 FORMAT (/,1X,' A NEGATIVE PIVOT WAS ENCOUNTERED AT LOCATION:')
510 FORMAT (/,1X,' COLUMN=',i4,', ROW=',i4,', LAYER=',i4)
515 FORMAT (/,1X,' PRECONDITIONED CONJUGATE GRADIENT SOLVER CANNOT &
         &PROCEED.') 
  END SUBROUTINE PIVOT_CK
  ! ... ***************************************************************

  ! ... ***************************************************************
  SUBROUTINE CK_DOMAIN
    ! ...
    ! ... ******************************************************************
    ! ... PURPOSE: CHECK DOMAIN FOR INTEGRITY
    ! ... ******************************************************************
    ! ...   
    ! ... ------------------------------------------------------------------
    ! ...   ARGUMENT LIST: NONE
    ! ... ------------------------------------------------------------------
    ! ... ------------------------------------------------------------------
    ! ...    LOCAL VARIABLES
    ! ... ------------------------------------------------------------------
    INTEGER :: I, J, K, NODE, NRN, NRL, NCN, NCL, NLN, NLL
    INTEGER :: LEVEL_CT, ROW_CT, NRC, II, C_MAX
    INTEGER :: D_NO, MAX_NODE, ERROR, D_CT
    INTEGER :: MAX_COL, MIN_COL, MAX_ROW, MIN_ROW, MAX_LAY, MIN_LAY
    INTEGER, DIMENSION(:), ALLOCATABLE :: C_NODE, CK_D
    LOGICAL :: FLAG
    ! ... ==================================================================
    ALLOCATE (CK_D(1:SIZE(IBOUND)), STAT=ERROR)
    CK_D=0; D_NO=0; MAX_NODE=0
    NRC=NROW*NCOL
    DO K=1,NLAY
       LEVEL_CT=(K-1)*NRC
       DO I=1,NROW
          ROW_CT=(I-1)*NCOL
          DO J=1,NCOL
             NODE = LEVEL_CT+ROW_CT+J
             IF (IBOUND(NODE)==0) THEN
                CYCLE
             ELSEIF (CK_D(NODE)==0) THEN
                ! ... IDENTIFY CONNECTIVITY IF UNKNOWN NODE
                D_NO=D_NO+1
                CK_D(NODE)=D_NO
             ENDIF
             MAX_NODE=MAX(MAX_NODE,NODE)
             NRN = NODE + NCOL
             NRL = NODE - NCOL
             NCN = NODE + 1
             NCL = NODE - 1
             NLN = NODE + NRC
             NLL = NODE - NRC
             ! ... PROJECT CONNECTION IDENTITY LATERALLY
             IF (I.NE.NROW) THEN
                IF (IBOUND(NRN)/=0) THEN
                   CK_D(NRN)=CK_D(NODE)
                   MAX_NODE=MAX(MAX_NODE,NRN)
                ENDIF
             ENDIF
             IF (K.NE.NLAY) THEN
                IF (IBOUND(NLN)/=0) THEN
                   CK_D(NLN)=CK_D(NODE)
                   MAX_NODE=MAX(MAX_NODE,NLN)
                ENDIF
             ENDIF
             IF (J.NE.NCOL) THEN
                IF (CK_D(NCN)/=0) THEN
                   ! ... CHECK SIMILITUDE OF FORWARD CONNECTION IDENTITY
                   IF (CK_D(NCN)>CK_D(NODE)) THEN
                      CALL RENUMBER(CK_D,MAX_NODE,CK_D(NODE),CK_D(NCN))
                   ELSEIF (CK_D(NCN)<CK_D(NODE)) THEN
                      CALL RENUMBER(CK_D,MAX_NODE,CK_D(NCN),CK_D(NODE))
                   ENDIF
                ELSEIF (IBOUND(NCN)/=0) THEN
                   ! ... PROJECT CONNECTION IDENTITY FORWARD
                   CK_D(NCN)=CK_D(NODE)
                   MAX_NODE=MAX(MAX_NODE,NCN)
                ENDIF
             ENDIF
             ! ... CHECK SIMILITUDE OF LATERAL CONNECTION IDENITIES
             IF (I.NE.1) THEN
                IF (CK_D(NRL)/=0) THEN
                   IF (CK_D(NRL)>CK_D(NODE)) THEN
                      CALL RENUMBER(CK_D,MAX_NODE,CK_D(NODE),CK_D(NRL))
                   ELSEIF (CK_D(NRL)<CK_D(NODE)) THEN
                      CALL RENUMBER(CK_D,MAX_NODE,CK_D(NRL),CK_D(NODE))
                   ENDIF
                ENDIF
             ENDIF
             IF (K.NE.1) THEN
                IF (CK_D(NLL)/=0) THEN
                   IF (CK_D(NLL)>CK_D(NODE)) THEN
                      CALL RENUMBER(CK_D,MAX_NODE,CK_D(NODE),CK_D(NLL))
                   ELSEIF (CK_D(NLL)<CK_D(NODE)) THEN
                      CALL RENUMBER(CK_D,MAX_NODE,CK_D(NLL),CK_D(NODE))
                   ENDIF
                ENDIF
             ENDIF
          ENDDO
       ENDDO
    ENDDO
    ALLOCATE (C_NODE(1:D_NO), STAT=ERROR)
    C_NODE=0
    DO NODE=1, NRC*NLAY
       IF (CK_D(NODE)>0) C_NODE(CK_D(NODE))=C_NODE(CK_D(NODE))+1
    ENDDO
    ! ... COUNT NUMBER OF DOMAIN PARTITIONS
    D_CT=0; C_MAX=0
    DO I=1, D_NO
       IF (C_NODE(I)>0) THEN
          D_CT=D_CT+1
          C_MAX=MAX(C_MAX,C_NODE(I))
       ENDIF
    ENDDO
    ! ... C_MAX ASSUMED TO REPRESENT MAIN DOMAIN
    IF (D_CT==1) THEN
       WRITE (IOUT,500)
       DEALLOCATE (C_NODE,CK_D)
       RETURN
    ELSEIF (D_CT==0) THEN
       WRITE (IOUT,505)
       DEALLOCATE (C_NODE,CK_D)
       RETURN
    ENDIF
    WRITE (IOUT,510) D_CT-1
    ! ... EXTRACT PARTITION INFORMATION
    D_CT=0
    DO II=1, D_NO
       IF (C_NODE(II)==0.OR.C_NODE(II)==C_MAX) CYCLE
       MAX_COL=0; MAX_ROW=0; MAX_LAY=0
       MIN_COL=100000000; MIN_ROW=100000000; MIN_LAY=100000000
       FLAG=.TRUE.
       DO K=1,NLAY
          LEVEL_CT=(K-1)*NRC
          DO I=1,NROW
             ROW_CT=(I-1)*NCOL
             DO J=1,NCOL
                NODE = LEVEL_CT+ROW_CT+J
                IF (CK_D(NODE)/=II) CYCLE
                IF (FLAG) THEN
                   D_CT=D_CT+1
                   FLAG=.FALSE.
                ENDIF
                MAX_COL=MAX(MAX_COL,J); MAX_ROW=MAX(MAX_ROW,I)
                MAX_LAY=MAX(MAX_LAY,K)
                MIN_COL=MIN(MIN_COL,J); MIN_ROW=MIN(MIN_ROW,I)
                MIN_LAY=MIN(MIN_LAY,K)
             ENDDO
          ENDDO
       ENDDO
       WRITE (IOUT,515) D_CT, MIN_COL, MAX_COL, MIN_ROW, MAX_ROW, &
            MIN_LAY, MAX_LAY
    ENDDO
    ! ... 
    DEALLOCATE (C_NODE,CK_D)
    ! ...
500 FORMAT (/,1X,' THE DOMAIN INTEGRITY APPEARS TO BE COMPLETE')
505 FORMAT (/,1X,' THE DOMAIN INTEGRITY ANALYSIS IS DEFECTIVE')
510 FORMAT (/,1X,' THE DOMAIN INTEGRITY APPEARS TO BE COMPROMISED', &
         /,1X,' AT LEAST',I3,' SUBDOMAIN(S) ARE PRESENT')
515 FORMAT (/,1X,' PARTITION',I3,' IS CONTAINED IN FOLLOWING REGION:', &
         /,1X,' FROM COLUMN',I5,' TO COLUMN',I5, &
         /,1X,'    FROM ROW',I5,'    TO ROW',I5, &
         /,1X,'  FROM LAYER',I5,'  TO LAYER',I5)
    ! ... 
  END SUBROUTINE CK_DOMAIN
  ! ... ***************************************************************

  ! ... ***************************************************************
  SUBROUTINE RENUMBER (CK_D, NO_NODES,NEW_NO,OLD_NO)
    INTEGER, DIMENSION(:), INTENT(INOUT) :: CK_D
    INTEGER, INTENT(IN) :: NO_NODES,NEW_NO,OLD_NO
    INTEGER :: NODE
    DO NODE=1, NO_NODES
       IF (CK_D(NODE)==OLD_NO) CK_D(NODE)=NEW_NO
    ENDDO
  END SUBROUTINE RENUMBER
  ! ... ***************************************************************

  ! ... ***************************************************************
  SUBROUTINE PCGN_DEALLOCATE(NGRID)
    INTEGER, INTENT(IN) :: NGRID
    INTEGER :: I
    DO I=1, NGRID
       CALL PCGN2DA(I)
    ENDDO
  END SUBROUTINE PCGN_DEALLOCATE
  ! ... ***************************************************************

  ! ... ***************************************************************
  SUBROUTINE PCGN2DA(IGRID)
    ! ... *************************************************************
    ! ... DEALLOCATE ALL ALLOCATIONS AT JOB TERMINATION
    ! ... *************************************************************
    INTEGER, INTENT(IN) :: IGRID
    IF (IGRID==1) THEN
       CALL PCG_FIN
       NULLIFY(PT,CR,CC,CV,HCOF,RHS,HNEW,IBOUND)
    ENDIF
    IF (ASSOCIATED(PCGNDAT(IGRID)%FILL))     DEALLOCATE(PCGNDAT(IGRID)%FILL)
    IF (ASSOCIATED(PCGNDAT(IGRID)%MO_ITER))  DEALLOCATE(PCGNDAT(IGRID)%MO_ITER)
    IF (ASSOCIATED(PCGNDAT(IGRID)%MI_ITER))  DEALLOCATE(PCGNDAT(IGRID)%MI_ITER)
    IF (ASSOCIATED(PCGNDAT(IGRID)%CNVG_A))   DEALLOCATE(PCGNDAT(IGRID)%CNVG_A)
    IF (ASSOCIATED(PCGNDAT(IGRID)%DAMP_A))   DEALLOCATE(PCGNDAT(IGRID)%DAMP_A)
    IF (ASSOCIATED(PCGNDAT(IGRID)%PRGUNIT))  DEALLOCATE(PCGNDAT(IGRID)%PRGUNIT)
    IF (ASSOCIATED(PCGNDAT(IGRID)%PC_UNIT))  DEALLOCATE(PCGNDAT(IGRID)%PC_UNIT)
    IF (ASSOCIATED(PCGNDAT(IGRID)%TS_UNIT))  DEALLOCATE(PCGNDAT(IGRID)%TS_UNIT)
    IF (ASSOCIATED(PCGNDAT(IGRID)%SAV_DAMP)) &
         DEALLOCATE(PCGNDAT(IGRID)%SAV_DAMP)
    IF (ASSOCIATED(PCGNDAT(IGRID)%DRELAX ))  DEALLOCATE(PCGNDAT(IGRID)%DRELAX)
    IF (ASSOCIATED(PCGNDAT(IGRID)%MAG_CLOSE)) &
         DEALLOCATE(PCGNDAT(IGRID)%MAG_CLOSE)
    IF (ASSOCIATED(PCGNDAT(IGRID)%SAV_CLOSE)) &
         DEALLOCATE(PCGNDAT(IGRID)%SAV_CLOSE)
    IF (ASSOCIATED(PCGNDAT(IGRID)%C_RATE))   DEALLOCATE(PCGNDAT(IGRID)%C_RATE) 
    IF (ASSOCIATED(PCGNDAT(IGRID)%D_RATE))   DEALLOCATE(PCGNDAT(IGRID)%D_RATE)
    IF (ASSOCIATED(PCGNDAT(IGRID)%RCLOSE))   DEALLOCATE(PCGNDAT(IGRID)%RCLOSE) 
    IF (ASSOCIATED(PCGNDAT(IGRID)%HCLOSE))   DEALLOCATE(PCGNDAT(IGRID)%HCLOSE)
    IF (ASSOCIATED(PCGNDAT(IGRID)%LIMHDCHG)) &
         DEALLOCATE(PCGNDAT(IGRID)%LIMHDCHG) 
    IF (ASSOCIATED(PCGNDAT(IGRID)%MIN_DAMP)) DEALLOCATE(PCGNDAT(IGRID)%MIN_DAMP)
    IF (ASSOCIATED(PCGNDAT(IGRID)%MIN_CLOSE)) &
         DEALLOCATE(PCGNDAT(IGRID)%MIN_CLOSE)
    IF (ASSOCIATED(PCGNDAT(IGRID)%DD))       DEALLOCATE(PCGNDAT(IGRID)%DD)
    IF (ASSOCIATED(PCGNDAT(IGRID)%DX))       DEALLOCATE(PCGNDAT(IGRID)%DX)
    IF (ASSOCIATED(PCGNDAT(IGRID)%DY))       DEALLOCATE(PCGNDAT(IGRID)%DY)
    IF (ASSOCIATED(PCGNDAT(IGRID)%DZ))       DEALLOCATE(PCGNDAT(IGRID)%DZ)
    IF (ASSOCIATED(PCGNDAT(IGRID)%RES))      DEALLOCATE(PCGNDAT(IGRID)%RES)
    IF (ASSOCIATED(PCGNDAT(IGRID)%HCH))      DEALLOCATE(PCGNDAT(IGRID)%HCH)
    CLOSE(IOUT)
  END SUBROUTINE PCGN2DA
  ! ... ***************************************************************

  ! ... PROG SUBROUTINES: PRINT NONLINEAR PROGRESS TO CSV FILE
  ! ... MODIFIED FROM  MODULE MHC, AS CREATED BY E.R. BANTA
  ! ... VERSION 1.0 NOVEMBER 2007


  SUBROUTINE PROG1AD(KPER,KSTP,IUNIT)
    !   Time-step advance
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: KPER, KSTP, IUNIT
    !
    !   Format statements
100 FORMAT('"Maximum head changes for Stress Period ',I5,   &
         ', Time Step ',I5,'"')
110 FORMAT('Iteration,ib0_count,ratio_l,Damp,L2hr,Hprev,Hcurr,',   &
         'Max_chg,Layer,Row,Column')
    !
    IF (IUNIT>0) THEN
       WRITE(IUNIT,100)KPER,KSTP
       WRITE(IUNIT,110)
    ENDIF
    !
    RETURN
  END SUBROUTINE PROG1AD

  SUBROUTINE PROG2AD(KPER,KSTP,IUNIT)
    !   Time-step advance
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: KPER, KSTP, IUNIT
    !
    !   Format statements
100 FORMAT('"Maximum head changes for Stress Period ',I5,   &
         ', Time Step ',I5,'"')
110 FORMAT('Iteration,ib0_count,Damp,L2hr,Hprev,Hcurr,',   &
         'Max_chg,Layer,Row,Column')
    !
    IF (IUNIT>0) THEN
       WRITE(IUNIT,100)KPER,KSTP
       WRITE(IUNIT,110)
    ENDIF
    !
    RETURN
  END SUBROUTINE PROG2AD

  SUBROUTINE PROG1OT(KITER,IB0_COUNT,RATIO_L,L2HR,HEADCHGMAX,MHC_NODE, &
       DAMPMHC,HLAST,HTHIS,IUNIT,NCOL,NROW)
    ! ... RLN:  MODIFIED 04/2009 FOR ERROR IN ROW-COLUMN INDEXING
    !   PRINT CONVERGENCE PROGRESS
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: KITER, IB0_COUNT, MHC_NODE, IUNIT, NCOL, NROW
    DOUBLE PRECISION, INTENT(IN) :: RATIO_L, L2HR, HEADCHGMAX, DAMPMHC, HLAST, &
         HTHIS
    ! ... ------------------------------------------------------------------
    ! ...    LOCAL VARIABLES
    ! ... ------------------------------------------------------------------
    INTEGER :: NRCMHC, IMHC, JMHC, KMHC
    ! ...
100 FORMAT(I9,', ',I9,', ',G12.5,', ',  F7.5,', ',G12.5,', ',G14.7, &
         ', ',G14.7,', ',G12.5,', ',I5,', ',I6,', ',I6)
    NRCMHC=NCOL*NROW
    KMHC=(MHC_NODE-1)/NRCMHC+1
    IMHC=MOD(MHC_NODE-1,NRCMHC)/NCOL+1
    JMHC=MOD(MOD(MHC_NODE-1,NRCMHC),NCOL)+1
    WRITE(IUNIT,100)KITER,IB0_COUNT,RATIO_L,DAMPMHC,L2HR,HLAST,HTHIS, &
         HEADCHGMAX,KMHC,IMHC,JMHC
  END SUBROUTINE PROG1OT

  SUBROUTINE PROG2OT(KITER,IB0_COUNT,L2HR,HEADCHGMAX,MHC_NODE, &
       DAMPMHC,HLAST,HTHIS,IUNIT,NCOL,NROW)
    ! ... RLN:  MODIFIED 04/2009 FOR ERROR IN ROW-COLUMN INDEXING
    !   PRINT CONVERGENCE PROGRESS
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: KITER, IB0_COUNT, MHC_NODE, IUNIT, NCOL, NROW
    DOUBLE PRECISION, INTENT(IN) :: L2HR, HEADCHGMAX, DAMPMHC, HLAST, &
         HTHIS
    ! ... ------------------------------------------------------------------
    ! ...    LOCAL VARIABLES
    ! ... ------------------------------------------------------------------
    INTEGER :: NRCMHC, IMHC, JMHC, KMHC
    ! ...
100 FORMAT(I9,', ',I9,', ',  F7.5,', ',G12.5,', ',G14.7, &
         ', ',G14.7,', ',G12.5,', ',I5,', ',I6,', ',I6)
    NRCMHC=NCOL*NROW
    KMHC=(MHC_NODE-1)/NRCMHC+1
    IMHC=MOD(MHC_NODE-1,NRCMHC)/NCOL+1
    JMHC=MOD(MOD(MHC_NODE-1,NRCMHC),NCOL)+1
    WRITE(IUNIT,100)KITER,IB0_COUNT,DAMPMHC,L2HR,HLAST,HTHIS, &
         HEADCHGMAX,KMHC,IMHC,JMHC
  END SUBROUTINE PROG2OT

  SUBROUTINE UPDATE_HEAD(Y,X,R)
    ! ... MODEL: Y=Y+R*X
    ! ... Y, X: ARRAYS
    ! ... R: SCALAR
    DOUBLE PRECISION, DIMENSION(:), INTENT(INOUT) :: Y
    DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: X
    DOUBLE PRECISION, INTENT(IN) :: R
    INTEGER, SAVE :: MOD_NO=8
    INTEGER :: II, M, ASIZE
    ASIZE=SIZE(Y)
    M=MOD(ASIZE,MOD_NO)
    !$OMP PARALLEL DEFAULT(NONE) &
    !$OMP SHARED(Y,X,R) &
    !$OMP PRIVATE(II) &
    !$OMP FIRSTPRIVATE(M,MOD_NO,ASIZE)
    IF (M>0) THEN
       !$OMP DO           
       DO II=1, M
          Y(II)=Y(II)+R*X(II)
       ENDDO
       !$OMP END DO
    ENDIF
    IF (ASIZE>MOD_NO) THEN
       !$OMP DO           
       DO II=M+1, ASIZE, MOD_NO
          Y(II)=Y(II)+R*X(II)
          Y(II+1)=Y(II+1)+R*X(II+1)
          Y(II+2)=Y(II+2)+R*X(II+2)
          Y(II+3)=Y(II+3)+R*X(II+3)
          Y(II+4)=Y(II+4)+R*X(II+4)
          Y(II+5)=Y(II+5)+R*X(II+5)
          Y(II+6)=Y(II+6)+R*X(II+6)
          Y(II+7)=Y(II+7)+R*X(II+7)
       ENDDO
       !$OMP END DO
    ENDIF
    !$OMP END PARALLEL
    ! ...
  END SUBROUTINE UPDATE_HEAD

END MODULE PCGN


