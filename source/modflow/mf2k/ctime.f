C     Last change:  ERB  11 Jul 2001    1:07 pm
C#######################################################################
      SUBROUTINE CTIME(TIME)
C#######################################################################
C     RETURNS CURRENT CPU-TIME IN SECONDS (REAL*4!)
C
C      DOUBLE PRECISION ACCUM
C      INTEGER          IERROR, ITICKS, MCLOCK
C      REAL             CPUTIM
C swm: for g77,SUN f77, or MS Developer Studio, uncomment the line below
c       REAL             TIME, ETIME, T(2)
C
      TIME = 0.0
CMPQ  CALL TIMER(ITICKS)
CMPQ  TIME=0.01*REAL(ITICKS)
C
C===> IBM/VS-Fortran
C
C     CALL CPUTIME (ACCUM, IERROR)
C     TIME = ACCUM * 1.0D-6
C
C===> IBM-AIX
C
C      TIME = MCLOCK () * 0.01
C
C===> Ardent/Titan
C
CARD  TIME = CPUTIM (0.0)
C
C===> Targon / iPSC/2
C
CTARG TIME = MCLOCK () * 1E-3
C
C swm: compiler specific intrinsic timing function.
C swm: for g77,SUN f77, or MS Developer Studio, uncomment the line below
c       TIME=ETIME(T)
C swm: for Intel Fortran (Fortran 90) uncomment the line below
c        CALL CPU_TIME(TIME)
C 
      RETURN
      END
