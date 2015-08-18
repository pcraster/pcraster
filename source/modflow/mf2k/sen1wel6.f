C     Last change:  ERB   7 Nov 2001    4:52 pm
      SUBROUTINE SEN1WEL6FM(NWELLS,MXWELL,WELL,NCOL,NROW,NLAY,IBOUND,
     &                     RHS,IP,NWELVL)
C-----VERSION 19980730 ERB
C     ******************************************************************
C     CALCULATE CONTRIBUTION TO SENSITIVITY OF ONE CELL-LIST PARAMETER
C     ******************************************************************
C     Modified 11/7/2001 to support parameter instances - ERB
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL WELL, FACTOR, RHS
      INTEGER I, IB, IBOUND, ICP, II, IP, J, JB, JJ, K, KB,
     &        MXWELL, NCOL,  NLAY, NWELLS, NROW
      DOUBLE PRECISION DF
      DIMENSION WELL(NWELVL,MXWELL), IBOUND(NCOL,NROW,NLAY),
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
C-----LOOP THROUGH CELLS CONTROLLED BY THIS PARAMETER
      DO 20 II = 1, NPC
        ICP = IPOS1-1+II+(NI-1)*NPC
        K = WELL(1,ICP)
        I = WELL(2,ICP)
        J = WELL(3,ICP)
        IF (IBOUND(J,I,K).LT.1) GOTO 20
C-------LOOP THROUGH TO SEE IF THIS CELL IS BEING USED.
C-------IF SO, CALCULATE CONTRIBUTION TO SENSITIVITY.
        DO 10 JJ = 1, NWELLS
          KB = WELL(1,JJ)
          IB = WELL(2,JJ)
          JB = WELL(3,JJ)
          IF (KB.EQ.K.AND.IB.EQ.I.AND.JB.EQ.J) THEN
            FACTOR = WELL(4,ICP)
            DF = FACTOR
            RHS(J,I,K) = RHS(J,I,K) - DF
            GOTO 20
          ENDIF
   10   CONTINUE
   20 CONTINUE
C
      RETURN
      END
