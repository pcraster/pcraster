C     Last change:  ERB   8 Nov 2001   11:23 am
      SUBROUTINE SEN1STR6FM(NSTREM,MXSTRM,STRM,HNEW,NCOL,NROW,NLAY,
     &                    IBOUND,RHS,ISTRM,IP)
C-----VERSION 20011108 ERB
C     ******************************************************************
C     FOR STREAMFLOW-ROUTING BOUNDARIES : CALCULATE MATRIX AND VECTOR
C     DERIVATIVES, MULTIPLY BY HEADS, AND ADD COMPONENTS TO THE
C     SENSITIVITY OR RHS.
C     ******************************************************************
C     Modified 11/8/2001 to support parameter instances - ERB
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL  FACTOR, RHS, STRM
      INTEGER I, IBOUND, II, IM, IR, IS, ISTRM, J, JJ, K,
     &        MXSTRM, NCOL, NLAY, NROW
      DOUBLE PRECISION DF, HNEW(NCOL,NROW,NLAY)
      DIMENSION IBOUND(NCOL,NROW,NLAY), ISTRM(5,MXSTRM),
     &          RHS(NCOL,NROW,NLAY), STRM(11,MXSTRM)
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
C-------LOOP THROUGH PARAMETER CELLS
      DO 20 II = 1, NPC
        ICP = IPOS1-1+II+(NI-1)*NPC
        DF = 0.0
        K = ISTRM(4,ICP)
        I = ISTRM(5,ICP)
cc        ELEVP = STRM(2,ICP)
C-------LOOP THROUGH TO SEE IF THIS REACH IS BEING USED.
C-------IF SO, CALCULATE CONTRIBUTION TO SENSITIVITY.
        DO 10 JJ = 1, NSTREM
          IS = ISTRM(4,JJ)
          IR = ISTRM(5,JJ)
          ELEVS = STRM(2,JJ)
Cerb Changed 5/9/01 because segment and reach uniquely identify each STR
Cerb feature -- no need to check stage (which varies anyway, when ICALC
Cerb is greater than 0.
cc          IF (IS.EQ.K.AND.IR.EQ.I .AND. ELEVS.EQ.ELEVP) THEN
          IF (IS.EQ.K.AND.IR.EQ.I) THEN
            IF (STRM(10,JJ).LE.0.0 .AND. STRM(11,JJ).GE.0.0) GOTO 20
            K = ISTRM(1,JJ)
            I = ISTRM(2,JJ)
            J = ISTRM(3,JJ)
            IF (IBOUND(J,I,K).LE.0) GOTO 20
            FACTOR = STRM(3,ICP)
            IF (HNEW(J,I,K).GT.STRM(4,JJ)) THEN
              HEAD=HNEW(J,I,K)
              DF=FACTOR*(ELEVS-HEAD)
            ELSE
              IM = 2
              IF (STRM(10,JJ).LE.0.) IM = 5
              DF = FACTOR*(STRM(IM,JJ)-STRM(4,JJ))
            ENDIF
C-------FOR SENSITIVITY-EQUATION SENSITIVITIES
            RHS(J,I,K) = RHS(J,I,K) - DF
            GOTO 20
          ENDIF
   10   CONTINUE
   20 CONTINUE
C
      RETURN
      END
