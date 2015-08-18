C     Last change:  ERB   8 Nov 2001   10:32 am
      SUBROUTINE SEN1GHB6FM(MXBND,BNDS,HNEW,NCOL,NROW,NLAY,IBOUND,RHS,
     &                      IP,NGHBVL)
C-----VERSION 20011108 ERB
C     ******************************************************************
C     FOR GENERAL-HEAD BOUNDARIES: CALCULATE MATRIX AND VECTOR
C     DERIVATIVES, MULTIPLY BY HEADS, AND ADD COMPONENTS TO RHS.
C     ******************************************************************
C     Modified 11/8/2001 to support parameter instances - ERB
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL BNDS, RHS
      INTEGER I, IBOUND, II, J, K, MXBND, NCOL, NLAY, NROW
      DOUBLE PRECISION DF, HNEW(NCOL,NROW,NLAY)
      DIMENSION BNDS(NGHBVL,MXBND), IBOUND(NCOL,NROW,NLAY),
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
        K = BNDS(1,ICP)
        I = BNDS(2,ICP)
        J = BNDS(3,ICP)
        IF (IBOUND(J,I,K).GT.0) THEN
C-------CALCULATE CONTRIBUTION TO SENSITIVITY.
          ELEVB = BNDS(4,ICP)
          DF = BNDS(5,ICP)*(ELEVB-HNEW(J,I,K))
          RHS(J,I,K) = RHS(J,I,K) - DF
        ENDIF
   20 CONTINUE
C
      RETURN
      END
