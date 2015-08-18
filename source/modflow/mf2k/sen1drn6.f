C     Last change:  ERB   8 Nov 2001   10:01 am
      SUBROUTINE SEN1DRN6FM(MXDRN,DRAI,HNEW,NCOL,NROW,NLAY,IBOUND,RHS,
     &                      IP,NDRNVL)
C-----VERSION 20011108 ERB
C     ******************************************************************
C     FOR DRAIN CELLS : CALCULATE MATRIX AND VECTOR DERIVATIVES,
C     MULTIPLY BY HEADS, AND ADD COMPONENTS TO RHS.
C     ******************************************************************
C     Modified 11/8/2001 to support parameter instances - ERB
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL DRAI, RHS
      INTEGER I, IBOUND, II, J, K, MXDRN, NCOL, NLAY, NROW
      DOUBLE PRECISION DF, HNEW(NCOL,NROW,NLAY)
      DIMENSION DRAI(NDRNVL,MXDRN), IBOUND(NCOL,NROW,NLAY),
     &          RHS(NCOL,NROW,NLAY)
      INCLUDE 'param.inc'
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
C-----LOOP THROUGH PARAMETER CELLS
      DO 20 II = 1, NPC
        ICP = IPOS1-1+II+(NI-1)*NPC
        K = DRAI(1,ICP)
        I = DRAI(2,ICP)
        J = DRAI(3,ICP)
        IF (IBOUND(J,I,K).GT.0) THEN
C-------CALCULATE CONTRIBUTION TO SENSITIVITY.
          ELEVD = DRAI(4,ICP)
          IF (HNEW(J,I,K).GT.ELEVD) THEN
            DF = DRAI(5,ICP)*(ELEVD-HNEW(J,I,K))
            RHS(J,I,K) = RHS(J,I,K) - DF
          ENDIF
        ENDIF
   20 CONTINUE
C
      RETURN
      END
