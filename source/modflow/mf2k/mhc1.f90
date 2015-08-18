! Time of File Save by ERB: 6/20/2006 3:45PM
MODULE MHC
  !   Maximum Head Change module, 6/20/2006 ERB
  PRIVATE
  !   Public data
  PUBLIC :: BIGHEADCHG, DAMPMHC, MHC_ACTIVE
  !   Public procedures
  PUBLIC :: MHC1ALG, MHC1AD, MHC1IT, MHC1OT, MHC1DA
  !
  DOUBLE PRECISION :: BIGHEADCHG=0.0D0
  REAL :: DAMPMHC=1.0
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: HNEWLAST
  INTEGER :: NCOLMHC, NROWMHC, NLAYMHC
  INTEGER :: IOMHC=0
  LOGICAL :: MHC_ACTIVE=.FALSE.
CONTAINS
  !=============================================================================
  SUBROUTINE MHC1ALG(IUNITMHC,IOUT,NCOL,NROW,NLAY)
    !   Activate Max. Head Change module and allocate memory
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: IUNITMHC, IOUT, NCOL, NROW, NLAY
    !
    !   Format statement
    100 FORMAT(1X,'Maximum head changes will be written to unit ',I4)
    !
    MHC_ACTIVE = .TRUE.
    IOMHC = IUNITMHC
    IF (IOMHC>0) WRITE(IOUT,100)IOMHC
    ALLOCATE (HNEWLAST(NCOL,NROW,NLAY))
    NCOLMHC = NCOL
    NROWMHC = NROW
    NLAYMHC = NLAY
    RETURN
  END SUBROUTINE MHC1ALG
  !=============================================================================
  SUBROUTINE MHC1AD(KPER,KSTP)
    !   Time-step advance
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: KPER, KSTP
    !
    !   Format statements
    100 FORMAT('"Maximum head changes for Stress Period ',I5,   &
               ', Time Step ',I5,'"')
    110 FORMAT('Iteration,Max_chg,Layer,Row,Column,Damp,',   &
               'Hprev,Hcurr')
    !
    IF (IOMHC>0) THEN
      WRITE(IOMHC,100)KPER,KSTP
      WRITE(IOMHC,110)
    ENDIF
    !
    RETURN
  END SUBROUTINE MHC1AD
  !=============================================================================
  SUBROUTINE MHC1IT(HNEW)
    !   Store heads at beginning of iteration
    IMPLICIT NONE
    !   Argument-list variables
    DOUBLE PRECISION, DIMENSION(NCOLMHC,NROWMHC,NLAYMHC), INTENT(IN) :: HNEW
    !   Local variables
    INTEGER :: I, J, K
    !
    DO K=1,NLAYMHC
      DO I=1,NROWMHC
        DO J=1,NCOLMHC
          HNEWLAST(J,I,K)=HNEW(J,I,K)
        ENDDO
      ENDDO
    ENDDO
    RETURN
   END SUBROUTINE MHC1IT
   !============================================================================
   SUBROUTINE MHC1OT(KITER,IBOUND,HNEW)
     !   Calculate and write head changes
    IMPLICIT NONE
    !   Argument-list variables
    INTEGER, INTENT(IN) :: KITER
    INTEGER, DIMENSION(NCOLMHC,NROWMHC,NLAYMHC), INTENT(IN) :: IBOUND
    DOUBLE PRECISION, DIMENSION(NCOLMHC,NROWMHC,NLAYMHC), INTENT(IN) :: HNEW
    !   Local variables
    REAL :: HEADCHG, HEADCHGMAX, HEADCHGMAXNEG, HEADCHGMAXPOS,   &
            HLAST, HLASTNEG, HLASTPOS, HTHIS, HTHISNEG, HTHISPOS
    INTEGER :: I, IMAXHEADCHG, IMAXHEADCHGNEG, IMAXHEADCHGPOS,   &
              J, JMAXHEADCHG, JMAXHEADCHGNEG, JMAXHEADCHGPOS,   &
              K, KMAXHEADCHG, KMAXHEADCHGNEG, KMAXHEADCHGPOS
    !
    100 FORMAT(I9,', ',G12.5,', ',I5,', ',I6,', ',I6,', ',   &
               G12.5,', ',G14.7,', ',G14.7)
    !
    HEADCHG = 0.0
    HEADCHGMAX = 0.0
    HEADCHGMAXNEG = 0.0
    HEADCHGMAXPOS = 0.0
    IMAXHEADCHG = 0
    IMAXHEADCHGNEG = 0
    IMAXHEADCHGPOS = 0
    JMAXHEADCHG = 0
    JMAXHEADCHGNEG = 0
    JMAXHEADCHGPOS = 0
    KMAXHEADCHG = 0
    KMAXHEADCHGNEG = 0
    KMAXHEADCHGPOS = 0
    HLASTNEG=0.0
    HLASTPOS=0.0
    HTHISNEG=0.0
    HTHISPOS=0.0
    !
    !   Find positive and negative max. head changes
    DO K=1,NLAYMHC
      DO I=1,NROWMHC
        DO J=1,NCOLMHC
          IF (IBOUND(J,I,K) > 0) THEN
            HEADCHG = HNEW(J,I,K)-HNEWLAST(J,I,K)
            IF (HEADCHG > 0.0) THEN
              IF (HEADCHG > HEADCHGMAXPOS) THEN
                HEADCHGMAXPOS = HEADCHG
                IMAXHEADCHGPOS = I
                JMAXHEADCHGPOS = J
                KMAXHEADCHGPOS = K
                HLASTPOS = HNEWLAST(J,I,K)
                HTHISPOS = HNEW(J,I,K)
              ENDIF
            ELSE
              IF (HEADCHG < HEADCHGMAXNEG) THEN
                HEADCHGMAXNEG = HEADCHG
                IMAXHEADCHGNEG = I
                JMAXHEADCHGNEG = J
                KMAXHEADCHGNEG = K
                HLASTNEG = HNEWLAST(J,I,K)
                HTHISNEG = HNEW(J,I,K)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
    ENDDO
    !
    !   Find previous and current head values at cell where
    !   absolute head change is largest
    IF (ABS(HEADCHGMAXPOS) > ABS(HEADCHGMAXNEG)) THEN
      HEADCHGMAX = HEADCHGMAXPOS
      IMAXHEADCHG = IMAXHEADCHGPOS
      JMAXHEADCHG = JMAXHEADCHGPOS
      KMAXHEADCHG = KMAXHEADCHGPOS
      HLAST = HLASTPOS
      HTHIS = HTHISPOS
    ELSE
      HEADCHGMAX = HEADCHGMAXNEG
      IMAXHEADCHG = IMAXHEADCHGNEG
      JMAXHEADCHG = JMAXHEADCHGNEG
      KMAXHEADCHG = KMAXHEADCHGNEG
      HLAST = HLASTNEG
      HTHIS = HTHISNEG
    ENDIF
    !
    !   Store max. head change for use by solver
    BIGHEADCHG=HEADCHGMAX
    !
    !   Write values for current iteration.  Note that DAMPMHC may be,
    !   and is expected to be, assigned each iteration by the solver.
    IF (IOMHC>0) THEN
     WRITE(IOMHC,100)KITER,HEADCHGMAX,KMAXHEADCHG,IMAXHEADCHG,   &
                      JMAXHEADCHG,DAMPMHC,HLAST,HTHIS
    ENDIF
    RETURN
  END SUBROUTINE MHC1OT
  !=============================================================================
  SUBROUTINE MHC1DA()
    !   Deallocate memory
    IF (ALLOCATED(HNEWLAST)) DEALLOCATE(HNEWLAST)
    RETURN
  END SUBROUTINE MHC1DA
  !=============================================================================
END MODULE MHC