C     Last change:  ERB   8 Nov 2001    1:32 pm
      SUBROUTINE SEN1CHD6FM(MXCHD,CHDS,SNEW,PERLEN,PERTIM,
     &            NCOL,NROW,NLAY,NCHDVL,IOUT,IP,IERR,IERRU)
C
C     VERSION 20011108 ERB
C     ******************************************************************
C     COMPUTE DERIVATIVE FOR TIME STEP AT EACH TIME-VARIANT SPECIFIED
C     HEAD CELL
C     ******************************************************************
C     Modified 11/8/2001 to support parameter instances - ERB
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
      DOUBLE PRECISION SNEW
      DIMENSION CHDS(NCHDVL,MXCHD),SNEW(NCOL,NROW,NLAY)
C     ------------------------------------------------------------------
C
C     FIND INSTANCE NUMBER
      NI = IACTIVE(IP)
      IF (NI.EQ.0) RETURN
C
C     DETERMINE RANGE OF ELEMENTS CURRENTLY ACTIVE FOR THIS PARAMETER
      IPOS1 = IPLOC(1,IP)
      NPC = IPLOC(2,IP)-IPOS1+1
      NUMINST = IPLOC(3,IP)
      IF (NUMINST.GT.1) NPC = NPC/NUMINST
C
C2------COMPUTE PROPORTION OF STRESS PERIOD TO CENTER OF THIS TIME STEP
      IF (PERLEN.EQ.0.0) THEN
        FRAC=1.0
      ELSE
        FRAC=PERTIM/PERLEN
      ENDIF
C
C2------PROCESS EACH ENTRY IN THE SPECIFIED-HEAD CELL LIST (CHDS)
      DO 100 L=1,NPC
        ICP=IPOS1-1+L+(NI-1)*NPC
C
C3------GET COLUMN, ROW AND LAYER OF CELL CONTAINING BOUNDARY
        IL=CHDS(1,ICP)
        IR=CHDS(2,ICP)
        IC=CHDS(3,ICP)
C
        IF (PERLEN.EQ.0.0 .AND. CHDS(4,ICP).NE.CHDS(5,ICP)) THEN
          IF (IERR.EQ.0) IERR = -1
          WRITE(IOUT,200)IL,IR,IC
          WRITE(IERRU,200)IL,IR,IC
  200     FORMAT(/,' ***WARNING***  FOR CHD CELL (',I3,',',I3,',',I3,
     &           '), START HEAD AND END HEAD DIFFER',/,
     &           ' FOR A STRESS PERIOD OF ZERO LENGTH --',/,
     &           ' USING ENDING HEAD AS CONSTANT HEAD',
     &           ' (SEN1CHD6FM)',/)
        ENDIF
C5------COMPUTE SENSITIVITY AT CELL BY LINEAR INTERPOLATION.
        FAC=CHDS(4,ICP)+(CHDS(5,ICP)-CHDS(4,ICP))*FRAC
C
C6------UPDATE THE APPROPRIATE SNEW VALUE
        SNEW(IC,IR,IL)=FAC
  100 CONTINUE
C
C7------RETURN
      RETURN
      END
