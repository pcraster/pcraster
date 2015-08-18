C     Last change:  ERB  21 Nov 2001    9:52 am
      SUBROUTINE SEN1RCH6FM(NCOL,NROW,NLAY,DELR,DELC,RMLT,NRCHOP,IRCH,
     &                      IBOUND,RHS,IZON,NMLTAR,NZONAR,IP)
C     VERSION 20011121 ERB
C     ******************************************************************
C     CALCULATE FORCING FUNCTION DERIVATIVE FOR RCH.  ADD TO RHS
C     ******************************************************************
C     Modified 11/21/2001 to support parameter instances - ERB
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL DELC, DELR, RHS, SM, RMLT, ZERO
      INTEGER IBOUND, IC, IFL, NRCHOP, IRCH, IR, IZ, K, 
     &        KK, LZ1, M, IZON, NCOL, NLAY, NROW, NZ
      DOUBLE PRECISION RO
      DIMENSION DELR(NCOL), DELC(NROW), IRCH(NCOL,NROW), 
     &          RMLT(NCOL,NROW,NMLTAR), IBOUND(NCOL,NROW,NLAY), 
     &          RHS(NCOL,NROW,NLAY),
     &          IZON(NCOL,NROW,NZONAR)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
      NI = IACTIVE(IP)
      IF (NI.EQ.0) RETURN
      ZERO = 0.0
      ICL1 = IPLOC(1,IP)
      ICL2 = IPLOC(2,IP)
      NUMINST = IPLOC(3,IP)
      IF (NUMINST.GT.1) THEN
C       SELECT CORRECT INSTANCE
        NCLU = (ICL2-ICL1+1)/NUMINST
        ICL1 = ICL1+(NI-1)*NCLU
        ICL2 = ICL1+NCLU-1
      ENDIF
C-------LOOP THROUGH CLUSTERS
      DO 70 K = ICL1,ICL2
        M = IPCLST(2,K)
        LZ1 = IPCLST(3,K)
        LASTZ = IPCLST(4,K)
C-------LOOP THROUGH CELLS
        DO 60 IR = 1, NROW
          DO 50 IC = 1, NCOL
            SM = 1.
            IF (M.GT.0) SM = RMLT(IC,IR,M)
            IF (LZ1.GT.0) THEN
              IFL = 0
              DO 10 IZ = 5,LASTZ
                NZ = IPCLST(IZ,K)
                IF (NZ.EQ.0 .OR. IFL.EQ.1) GOTO 20
                IF (NZ.EQ.IZON(IC,IR,LZ1)) IFL = 1
   10         CONTINUE
   20         IF (IFL.EQ.0) SM = ZERO
            ENDIF
            IF (SM.EQ.ZERO) GOTO 50
C----------NRCHOP=1
            IF (NRCHOP.EQ.1 .AND. IBOUND(IC,IR,1).LT.1) GOTO 50
            IF (NRCHOP.EQ.1 .AND. IBOUND(IC,IR,1).GT.0) THEN
              RO = SM*DELR(IC)*DELC(IR)
              KK = 1
            ENDIF
C----------NRCHOP=2
            IF (NRCHOP.EQ.2) THEN
              IF (IRCH(IC,IR).EQ.0) GOTO 50  ! ERB 1/11/07
              IF (IBOUND(IC,IR,(IRCH(IC,IR))).LT.1) GOTO 50
              IF (IBOUND(IC,IR,(IRCH(IC,IR))).GT.0) THEN
                RO = SM*DELR(IC)*DELC(IR)
                KK = IRCH(IC,IR)
              ENDIF
            ENDIF
C--------NRCHOP=3
            IF (NRCHOP.EQ.3) THEN
              DO 30 KK = 1, NLAY
                IF (IBOUND(IC,IR,KK).GT.0) THEN
                  RO = SM*DELR(IC)*DELC(IR)
                  GOTO 40
                ENDIF
   30         CONTINUE
              GOTO 50
            ENDIF
   40       CONTINUE
C----------CONTRIBUTIONS TO RHS.
            RHS(IC,IR,KK) = RHS(IC,IR,KK) - RO
   50     CONTINUE
   60   CONTINUE
   70 CONTINUE
C
      RETURN
      END
