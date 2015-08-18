! Time of File Save by ERB: 6/6/2006 4:22PM
C     Last change:  ERB   8 Nov 2001    3:06 pm
      SUBROUTINE SEN1DRT1FM(MXDRT,DRTF,HNEW,NCOL,NROW,NLAY,IBOUND,RHS,
     &                      IP,NDRTVL,IDRTFL)
C-----VERSION 20060606 ERB
C     ******************************************************************
C     FOR DRAIN-RETURN AND RECIPIENT CELLS: CALCULATE MATRIX AND VECTOR
C     DERIVATIVES, MULTIPLY BY HEADS, AND ADD COMPONENTS TO RHS.
C     ******************************************************************
C     Modified 11/8/2001 to support parameter instances - ERB
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL DRTF, RHS
      INTEGER I, IBOUND, II, J, K, MXDRT, NCOL, NLAY, NROW
      DOUBLE PRECISION DF, HH, HNEW(NCOL,NROW,NLAY)
      DIMENSION DRTF(NDRTVL,MXDRT), IBOUND(NCOL,NROW,NLAY),
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
C-----LOOP THROUGH PARAMETER CELLS
      DO 20 II = 1, NPC
        ICP = IPOS1-1+II+(NI-1)*NPC
        K = DRTF(1,ICP)
        I = DRTF(2,ICP)
        J = DRTF(3,ICP)
        IF (IBOUND(J,I,K).GT.0) THEN
C---------CALCULATE CONTRIBUTION TO SENSITIVITY
          ELEVD = DRTF(4,ICP)
          HH = HNEW(J,I,K)
          IF (HH.GT.ELEVD) THEN
            FACTOR = DRTF(5,ICP)
            DF = FACTOR*(ELEVD-HH)
            RHS(J,I,K) = RHS(J,I,K) - DF
C  CALCULATE CONTRIBUTION TO SENSITIVITY OF RETURN FLOW
            IF (IDRTFL.GT.0) THEN
              KR = DRTF(6,ICP)
              IF (KR.NE.0) THEN
                IR = DRTF(7,ICP)
                JR = DRTF(8,ICP)
                IF (IBOUND(JR,IR,KR) .GT. 0) THEN
                  RFPROP = DRTF(9,ICP)
                  DFR = RFPROP*FACTOR*(HH-ELEVD)
                  RHS(JR,IR,KR) = RHS(JR,IR,KR) - DFR
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
   20 CONTINUE
C
      RETURN
      END
