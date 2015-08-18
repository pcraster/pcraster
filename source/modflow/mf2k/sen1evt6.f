C     Last change:  ERB  11 Jan 2007   10:12 am
      SUBROUTINE SEN1EVT6FM(NCOL,NROW,NLAY,DELR,DELC,RMLT,NEVTOP,IEVT,
     &                  IBOUND,RHS,SURF,EXDP,HNEW,IZON,NMLTAR,NZONAR,IP)
C-----VERSION 20011121 ERB
C     ******************************************************************
C     CALCULATE FORCING FUNCTION DERIVATIVE FOR ETM. MULTIPLY BY
C     THE ADJOINT STATE AND ADD TO THE DERIVATIVE, OR ADD TO RHS, AS
C     NEEDED.
C     ******************************************************************
C     Modified 11/21/2001 to support parameter instances - ERB
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL DDD, DELC, DELR, EXDP, H, RHS, S, SM,
     &     RMLT, SURF, XXX, ZERO
      INTEGER IBOUND, IC, IFL, IL, NEVTOP, IEVT, IR, IZ, K,
     &        KK, IZON, NCOL, NLAY,
     &        NROW, NZ
      DOUBLE PRECISION RO, HNEW(NCOL,NROW,NLAY)
      DIMENSION DELR(NCOL), DELC(NROW), IEVT(NCOL,NROW),
     &          RMLT(NCOL,NROW,NMLTAR), IBOUND(NCOL,NROW,NLAY),
     &          RHS(NCOL,NROW,NLAY), SURF(NCOL,NROW),
     &          EXDP(NCOL,NROW), IZON(NCOL,NROW,NZONAR)
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
C-----LOOP THROUGH CLUSTERS
      DO 70 K = ICL1, ICL2
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
              DO 10 IZ = 5, LASTZ
                NZ = IPCLST(IZ,K)
                IF (NZ.EQ.0 .OR. IFL.EQ.1) GOTO 20
                IF (NZ.EQ.IZON(IC,IR,LZ1)) IFL = 1
   10         CONTINUE
   20         IF (IFL.EQ.0) SM = ZERO
            ENDIF
            IF (SM.EQ.ZERO) GOTO 50
C--------NEVTOP=1
            IF (NEVTOP.EQ.1 .AND. IBOUND(IC,IR,1).LT.1) GOTO 50
            IF (NEVTOP.EQ.1 .AND. IBOUND(IC,IR,1).GT.0) THEN
              RO = SM*DELR(IC)*DELC(IR)
              KK = 1
            ENDIF
C--------NEVTOP=2
            IF (NEVTOP.EQ.2) THEN
              IF (IEVT(IC,IR).EQ.0) GOTO 50  ! ERB 1/11/07
              IF (IBOUND(IC,IR,(IEVT(IC,IR))).LT.1) GOTO 50
              IF (IBOUND(IC,IR,(IEVT(IC,IR))).GT.0) THEN
                RO = SM*DELR(IC)*DELC(IR)
                KK = IEVT(IC,IR)
              ENDIF
            ENDIF
C--------NEVTOP=3 -- added 3/31/05 ERB
            IF (NEVTOP.EQ.3) THEN
              KK = 0
              DO IL=1,NLAY
                IF (IBOUND(IC,IR,IL).LT.0) GOTO 50
                IF (IBOUND(IC,IR,IL).GT.0) THEN
                  RO = SM*DELR(IC)*DELC(IR)
                  KK = IL
                  EXIT
                ENDIF
              ENDDO
              IF (KK.EQ.0) GOTO 50
            ENDIF
C--------ADJUST
            RO = -RO
            S = SURF(IC,IR)
            H = HNEW(IC,IR,KK)
            IF (H.LT.S) THEN
              DDD = S - H
              XXX = EXDP(IC,IR)
              IF (DDD.GE.XXX) GOTO 50
C             FIRST TERM IN PARENS IS FROM HCOF, SECOND IS FROM RHS
              RO = RO*(H/XXX - (S-XXX)/XXX)
            ENDIF
C--------CONTRIBUTIONS TO RHS.
            RHS(IC,IR,KK) = RHS(IC,IR,KK) - RO
   50     CONTINUE
   60   CONTINUE
   70 CONTINUE
C
      RETURN
      END
