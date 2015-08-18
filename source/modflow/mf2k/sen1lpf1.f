C     Last change:  ERB  14 Jan 2003    4:01 pm
      SUBROUTINE SEN1LPF1SV(IZON,KPER,NCOL,NLAY,NMLTAR,NPLIST,NROW,
     &                      NZONAR,RMLT,SV)
C-----VERSION 19980916 ERB
C     ******************************************************************
C     INITIALIZE SV ARRAY
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL RMLT, SV
      INTEGER I, IIP, IZON, KPER, NCOL,
     &        NLAY, NMLTAR, NPLIST, NROW, NZONAR
      CHARACTER*4 PIDTMP
      DIMENSION IZON(NCOL,NROW,NZONAR),
     &          RMLT(NCOL,NROW,NMLTAR), SV(NCOL,NROW,NLAY)
      INCLUDE 'param.inc'
C
C     ------------------------------------------------------------------
C
C-------INITIALIZE SV ARRAY
      IF (KPER.EQ.1) THEN
  100   DO 140 IIP = 1, NPLIST
          IF (PARTYP(IIP).EQ.'VKCB' .OR. PARTYP(IIP).EQ.'HK  ') THEN
            DO 130 K = 1, NLAY - 1
              DO 120 I = 1, NROW
                DO 110 J = 1, NCOL
                  SV(J,I,K) = 0.
  110           CONTINUE
  120         CONTINUE
  130       CONTINUE
            GOTO 190
          ENDIF
  140   CONTINUE
 190    CONTINUE
      ENDIF
C
      DO 200 IIP = 1, NPLIST
        PIDTMP = PARTYP(IIP)
        BB = B(IIP)
        IF (PIDTMP.EQ.'VKCB') CALL SSEN1LPF1VKCB(SV,RMLT,NCOL,NROW,
     &                            NLAY,BB,KPER,IZON,NMLTAR,NZONAR,IIP)
 200  CONTINUE
C
      RETURN
      END
C=======================================================================
      SUBROUTINE SEN1LPF1FM(RMLT,H,NCOL,NROW,NLAY,ISS,PID,HK,DELR,DELC,
     &                      IBOUND,DELT,RHS,HOLD,IZON,CV,SV,NMLTAR,
     &                      NZONAR,IP,BOTM,NBOTM,VKA,IUHFB,HFB,MXACTFB,
     &                      NHFB,HANI)
C     VERSION 19990402 ERB
C     ******************************************************************
C      CALCULATE MATRIX DERIVATIVES AND MULTIPLY BY HEADS AS NEEDED.
C      ADD RESULTING CONTRIBUTION TO RHS.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL CO, CO1, CV, DELC, DELR,
     &     DELT, R0, RHS, SF,
     &     HOLD, SM, RMLT, RMLT0, SV, TH0, TH1,
     &     ZERO, ONE, BOTM, VKA
      INTEGER I, IBM, IBOUND, IBP, IFLAG0, IFLAG1, IL, IND,
     &        ISS, IZ, J, K,
     &        LT, M, IZON, NCOL, NLAY,
     &        NRC, NROW
      CHARACTER*4 PID
      DOUBLE PRECISION DZERO, HH, HH1, HO, HP
      DOUBLE PRECISION H(NCOL*NROW*NLAY)
      DIMENSION RMLT(NCOL,NROW,NMLTAR),DELR(NCOL), DELC(NROW),
     &          RHS(NCOL,NROW,NLAY), IBOUND(NCOL,NROW,NLAY),
     &          HOLD(NCOL*NROW*NLAY),
     &          CV(NCOL,NROW,NLAY), IZON(NCOL,NROW,NZONAR),
     &          BOTM(NCOL,NROW,0:NBOTM),
     &          SV(NCOL,NROW,NLAY), HK(NCOL,NROW,NLAY),
     &          VKA(NCOL,NROW,NLAY), HFB(7,MXACTFB),
     &          HANI(NCOL,NROW,NLAY)
      COMMON /DISCOM/LBOTM(999),LAYCBD(999)
      COMMON /LPFCOM/LAYTYP(999),LAYAVG(999),CHANI(999),LAYVKA(999),
     1               LAYWET(999)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
C
      ZERO = 0.0
      DZERO = 0.0
      ONE = 1.0
      NRC = NROW*NCOL
C
      DO 140 ICL = IPLOC(1,IP),IPLOC(2,IP)
        IL = IPCLST(1,ICL)
        SF = 1.0
        IF (IL.GT.0 .AND. PID.NE.'ANI ') THEN
          LZ1 = IPCLST(3,ICL)
          M = IPCLST(2,ICL)
        ENDIF
        IF (ISS.NE.0 .AND. (PID.EQ.'SS  '.OR.PID.EQ.'SY  ')) GOTO 140
        K = IL
        LT = LAYTYP(IL)
C-----HORIZONTAL CONDUCTANCES
        IF (PID.EQ.'HK  ' .OR. PID.EQ.'HANI') THEN
C         IF INTERPOLATION IS BY METHOD OTHER THAN HARMONIC MEAN, STOP
C         WITH ERROR
          DO 70 I = 1, NROW
            DO 60 J = 1, NCOL
              IF (IBOUND(J,I,K).EQ.0) GOTO 60
              IND = J + NCOL*(I-1) + NRC*(K-1)
              HO = H(IND)
              R0 = ZERO
              RMLT0 = ONE
C%ERA%HANI    IF (IL.GT.0 .AND. PID.NE.'HANI') THEN
              IF (IL.GT.0) THEN
                IF (M.EQ.0) RMLT0 = SF
                IF (M.GT.0) RMLT0 = SF*RMLT(J,I,M)
                IF (RMLT0.NE.ZERO .AND. LZ1.GT.0) THEN
                  IFLAG0 = 0
                  DO 30 IZ = 5,IPCLST(4,ICL)
                    NZ = IPCLST(IZ,ICL)
                    IF (NZ.EQ.0 .OR. IFLAG0.EQ.1) GOTO 40
                    IF (IZON(J,I,LZ1).EQ.NZ) IFLAG0 = 1
   30             CONTINUE
   40             IF (IFLAG0.EQ.0) RMLT0 = ZERO
                ENDIF
              ENDIF
              TH0 = BOTM(J,I,LBOTM(K)-1) - BOTM(J,I,LBOTM(K))
              IF (LT.NE.0 .AND. HO.LT.BOTM(J,I,LBOTM(K)-1))
     &            TH0 = HO - BOTM(J,I,LBOTM(K))
C-------CR
              IF (J.NE.NCOL) THEN
                IF (PID.NE.'HANI' .AND.
     &              IBOUND(J+1,I,K).NE.0) THEN
                  CALL SSEN1LPF1CH(CO,TH1,HP,I,J,K,'CR',IL,M,RMLT0,RMLT,
     &                        LZ1,IZON,SF,LT,HK,NCOL,NROW,
     &                        NLAY,DELC,DELR,H,TH0,BOTM(1,1,LBOTM(K)),
     &                        BOTM(1,1,LBOTM(K)-1),NMLTAR,NZONAR,ICL,C,
     &                        HANI)
                  IF (IUHFB.GT.0 .AND. CO.NE.0.)
     &                CALL SSEN1HFB6MD(C,'CR',CO,DELC,DELR,HFB,I,J,K,
     &                                 MXACTFB,NCOL,NHFB,NROW,TH0,
     &                                 TH1)
                  HH = HO - HP
                  R0 = R0 + CO*HH
                  RHS(J+1,I,K) = RHS(J+1,I,K) - CO*HH
                ENDIF
              ENDIF
C-------CC
              IF (I.EQ.NROW) GOTO 50
              IF (IBOUND(J,I+1,K).EQ.0) GOTO 50
              IF (PID.EQ.'HK  ') THEN
                CALL SSEN1LPF1CH(CO,TH1,HP,I,J,K,'CC',IL,M,RMLT0,RMLT,
     &                          LZ1,IZON,SF,LT,HK,NCOL,NROW,
     &                          NLAY,DELC,DELR,H,TH0,BOTM(1,1,LBOTM(K)),
     &                          BOTM(1,1,LBOTM(K)-1),NMLTAR,NZONAR,ICL,
     &                          C,HANI)
C%ERA%HANI
              ELSEIF (PID.EQ.'HANI') THEN
                CALL SSEN1LPF1CHN(CO,TH1,HP,I,J,K,IL,M,RMLT0,RMLT,
     &                          LZ1,IZON,SF,LT,HK,NCOL,NROW,
     &                          NLAY,DELC,DELR,H,TH0,BOTM(1,1,LBOTM(K)),
     &                          BOTM(1,1,LBOTM(K)-1),NMLTAR,NZONAR,ICL,
     &                          C,HANI)
              ENDIF
              IF (IUHFB.GT.0 .AND. CO.NE.0.)
     &              CALL SSEN1HFB6MD(C,'CC',CO,DELC,DELR,HFB,I,J,K,
     &                               MXACTFB,NCOL,NHFB,NROW,TH0,TH1)
              IF (CO.EQ.ZERO) GOTO 50
              HH = HO - HP
              R0 = R0 + CO*HH
              RHS(J,I+1,K) = RHS(J,I+1,K) - CO*HH
   50         RHS(J,I,K) = RHS(J,I,K) + R0
   60       CONTINUE
   70     CONTINUE
        ENDIF
C-------CV
        IF (PID.EQ.'VK  ' .OR. PID.EQ.'VANI' .OR. (PID.EQ.'HK  ' .AND.
     &      LAYVKA(K).NE.0) .OR. PID.EQ.'VKCB') THEN
          DO 90 I = 1, NROW
            DO 80 J = 1, NCOL
              CALL SSEN1LPF1CV(CO,CO1,IBP,IBM,PID,IL,SF,RMLT,M,NCOL,
     &                       NROW,LZ1,CV,SV,NLAY,DELR,DELC,J,I,K,HK,
     &                       IZON,IBOUND,NMLTAR,NZONAR,ICL,BOTM,NBOTM,
     &                       VKA,H)
              IF (CO.EQ.ZERO .AND. CO1.EQ.ZERO) GOTO 80
              IND = J + NCOL*(I-1) + NRC*(K-1)
              HH = DZERO
              HH1 = DZERO
              IF (K.LT.NLAY .AND. IBP.NE.0) HH = H(IND) - H(IND+NRC)
              IF (PID.NE.'VKCB' .AND. K.GT.1 .AND. IBM.NE.0)
     &            HH1 = H(IND-NRC) - H(IND)
              IF (K.LT.NLAY .AND. IBP.NE.0) THEN
                RHS(J,I,K) = RHS(J,I,K) + CO*HH
                RHS(J,I,K+1) = RHS(J,I,K+1) - CO*HH
C               ACCOUNT FOR UNCONFINED LAYER UNDERLYING ACTIVE LAYER
C               FIRST, MAKE CORRECTION IF NEEDED FOR CV BELOW
                IF (LAYTYP(K+1).NE.0 .AND.
     &              H(IND+NRC).LT.BOTM(J,I,LBOTM(K+1)-1)) THEN
                  RHS(J,I,K) = RHS(J,I,K) -
     &                CO*(BOTM(J,I,LBOTM(K+1)-1)-H(IND+NRC))
                  RHS(J,I,K+1) = RHS(J,I,K+1) +
     &                CO*(BOTM(J,I,LBOTM(K+1)-1)-H(IND+NRC))
                ENDIF
              ENDIF
              IF (PID.NE.'VKCB' .AND. K.GT.1 .AND. IBM.NE.0) THEN
                RHS(J,I,K-1) = RHS(J,I,K-1) + CO1*HH1
                RHS(J,I,K) = RHS(J,I,K) - CO1*HH1
C               NEXT, MAKE CORRECTION IF NEEDED FOR CV ABOVE
                IF (LT.NE.0 .AND. H(IND).LT.BOTM(J,I,LBOTM(K)-1)) THEN
                  RHS(J,I,K-1) = RHS(J,I,K-1) -
     &                CO1*(BOTM(J,I,LBOTM(K)-1)-H(IND))
                  RHS(J,I,K) = RHS(J,I,K) +
     &                CO1*(BOTM(J,I,LBOTM(K)-1)-H(IND))
                ENDIF
              ENDIF
   80       CONTINUE
   90     CONTINUE
        ENDIF
C-----S
        IF (PID.EQ.'SS  ' .OR. PID.EQ.'SY  ') THEN
          IF (ISS.NE.0) GOTO 140
          IF (PID.EQ.'SY' .AND. LAYTYP(K).EQ.0) GOTO 140
          DO 130 I = 1, NROW
            DO 120 J = 1, NCOL
              IF (IBOUND(J,I,K).LT.1) GOTO 120
              IND = J + NCOL*(I-1) + NRC*(K-1)
              HO = H(IND)
              SHO = HOLD(IND)
              TP = BOTM(J,I,LBOTM(K)-1)
              IF (LT.NE.0) THEN
                IF (PID.EQ.'SS  ' .AND. HO.LT.TP .AND. SHO.LT.TP)
     &              GOTO 120
                IF (PID.EQ.'SY  ' .AND. HO.GE.TP .AND. SHO.GE.TP)
     &              GOTO 120
              ENDIF
              TH0 = BOTM(J,I,LBOTM(K)-1) - BOTM(J,I,LBOTM(K))
              IF (LT.NE.0 .AND. HO.LT.BOTM(J,I,LBOTM(K)-1))
     &            TH0 = HO - BOTM(J,I,LBOTM(K))
              IF (IL.LT.0) THEN
                IF (PID.EQ.'SS  ') THEN
                  CO = TH0*DELR(J)*DELC(I)/DELT
                ELSE
                  CO = DELR(J)*DELC(I)/DELT
                ENDIF
              ELSEIF (IL.GT.0) THEN
                SM = SF
                IF (M.NE.0) SM = SF*RMLT(J,I,M)
                IF (SM.NE.ZERO .AND. LZ1.GT.0) THEN
                  IFLAG1 = 0
                  DO 100 IZ = 5,IPCLST(4,ICL)
                    NZ = IPCLST(IZ,ICL)
                    IF (NZ.EQ.0 .OR. IFLAG1.EQ.1) GOTO 110
                    IF (IZON(J,I,LZ1).EQ.NZ) IFLAG1 = 1
  100             CONTINUE
  110             IF (IFLAG1.EQ.0) SM = ZERO
                ENDIF
                IF (SM.EQ.ZERO) GOTO 120
                IF (PID.EQ.'SS  ') THEN
                  CO = SM*TH0*DELR(J)*DELC(I)/DELT
                ELSE
                  CO = SM*DELR(J)*DELC(I)/DELT
                ENDIF
              ENDIF
C        DAH AND DBH
              HO = H(IND)
              IF (LT.NE.0) THEN
                IF (SHO.GE.TP .AND. HO.LT.TP) THEN
                  IF (PID.EQ.'SS  ') HO = TP
                  IF (PID.EQ.'SY  ') SHO = TP
                ELSEIF (SHO.LT.TP .AND. HO.GE.TP) THEN
                  IF (PID.EQ.'SS  ') SHO = TP
                  IF (PID.EQ.'SY  ') HO = TP
                ENDIF
              ENDIF
              RHS(J,I,K) = RHS(J,I,K) - CO*(SHO-HO)
  120       CONTINUE
  130     CONTINUE
        ENDIF
  140 CONTINUE
C
      RETURN
      END
C=======================================================================
      SUBROUTINE SEN1LPF1UN(ISS,DELT,NCOL,NROW,NLAY,SOLD,HNEW,SNEW,DELR,
     &                      DELC,IBOUND,RHS,SC1,CR,CC,KITER,SC2,HK,BOTM,
     &                      NBOTM,HOLD,CV,HANI,VKA)
C     VERSION 19990402 ERB
C     FORMERLY WAS SEN1AP
C     ******************************************************************
C     COMPUTE SENSITIVITY-EQUATION RHS TERMS FOR UNCONFINED AQUIFERS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL BOTM, CC, CR, CV, DELC, DELR, DELT, HOLD, RHS, SC1, SC2, SCC,
     &     HK
      INTEGER I, IBOUND, J, K, KITER, KT, LT, NCOL, NLAY,
     &        NROW, NBOTM
      DOUBLE PRECISION HNEW(NCOL,NROW,NLAY), SNEW(NCOL,NROW,NLAY)
      DIMENSION HOLD(NCOL,NROW,NLAY), SOLD(NCOL,NROW,NLAY),
     &          DELR(NCOL), DELC(NROW), IBOUND(NCOL,NROW,NLAY),
     &          RHS(NCOL,NROW,NLAY), BOTM(NCOL,NROW,0:NBOTM),
     &          CC(NCOL,NROW,NLAY), CR(NCOL,NROW,NLAY),
     &          SC1(NCOL,NROW,NLAY), SC2(NCOL,NROW,NLAY),
     &          HK(NCOL,NROW,NLAY), CV(NCOL,NROW,NLAY),
     &          HANI(NCOL,NROW,NLAY), VKA(NCOL,NROW,NLAY)
      COMMON /DISCOM/LBOTM(999),LAYCBD(999)
      COMMON /LPFCOM/LAYTYP(999),LAYAVG(999),CHANI(999),LAYVKA(999),
     1               LAYWET(999)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
C
C-------TERMS FOR UNCONFINED AQUIFERS
      IF (KITER.GT.1) THEN
        DO 60 K = 1, NLAY
          LT = LAYTYP(K)
          IF (LT.NE.0) THEN
C%ERA%HANI
            CALL SSEN1LPF1NL(HNEW,SNEW,NCOL,NROW,NLAY,HK,VKA,DELR,DELC,
     &                     IBOUND,RHS,BOTM,NBOTM,CR,CC,CV,K,HANI)
            IF (K.GT.1) THEN
              DO 50 I = 1, NROW
                DO 40 J = 1, NCOL
                  IF (IBOUND(J,I,K).NE.0 .AND. IBOUND(J,I,K-1).NE.0
     &                .AND. HNEW(J,I,K).LT.BOTM(J,I,LBOTM(K)-1)) THEN
                    RHS(J,I,K-1) = RHS(J,I,K-1)+CV(J,I,K-1)*SNEW(J,I,K)
                    RHS(J,I,K) = RHS(J,I,K)-CV(J,I,K-1)*SNEW(J,I,K)
                  ENDIF
   40           CONTINUE
   50         CONTINUE
            ENDIF
          ENDIF
   60   CONTINUE
      ENDIF
C
C-------B MATRIX TIMES SOLUTION FROM LAST TIME STEP FOR SENSITIVITY-
C-------EQUATION SENSITIVITIES
      IF (ISS.EQ.0) THEN
        KT = 0
        DO 110 K = 1, NLAY
          IF (LAYTYP(K).EQ.0) THEN
            DO 80 I = 1, NROW
              DO 70 J = 1, NCOL
                RHS(J,I,K) = RHS(J,I,K) - SOLD(J,I,K)*SC1(J,I,K)/DELT
   70         CONTINUE
   80       CONTINUE
          ELSE
            KT = KT + 1
            DO 100 I = 1, NROW
              DO 90 J = 1, NCOL
                SCC = SC1(J,I,K)
                IF (HOLD(J,I,K).LT.BOTM(J,I,LBOTM(K)-1))
     &              SCC = SC2(J,I,KT)
                RHS(J,I,K) = RHS(J,I,K) - SOLD(J,I,K)*SCC/DELT
   90         CONTINUE
  100       CONTINUE
          ENDIF
  110   CONTINUE
      ENDIF
      RETURN
      END
C=======================================================================
      SUBROUTINE SSEN1LPF1VKCB(SV,RMLT,NCOL,NROW,NLAY,BB,KPER,IZON,
     &                         NMLTAR,NZONAR,IP)
C-----VERSION 2000 08AUG1995
C     VERSION 19980527 ERB
C     ******************************************************************
C     FOR PID=KV : PUT NEW PARAMETER VALUES IN MODEL ARRAYS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL BB, CO, DRC, SF, RMLT, SV, ZERO
      INTEGER I, IZ, J, KPER,
     &        M, IZON, NCOL, NL, NLAY,
     &        NROW, NZ
      DIMENSION SV(NCOL,NROW,NLAY), RMLT(NCOL,NROW,NMLTAR),
     &          IZON(NCOL,NROW,NZONAR)
C
      COMMON /DISCOM/LBOTM(999),LAYCBD(999)
      COMMON /LPFCOM/LAYTYP(999),LAYAVG(999),CHANI(999),LAYVKA(999),
     1               LAYWET(999)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
      ZERO = 0.0
      IF (KPER.GT.1) RETURN
C-------LOOP THROUGH LAYERS
      DO 90 ICL = IPLOC(1,IP),IPLOC(2,IP)
        NL = IPCLST(1,ICL)
        SF = 1.0*BB
        M = IPCLST(2,ICL)
        LZ1 = IPCLST(3,ICL)
        LZN = 5
        IF (LZ1.NE.0) LZN = IPCLST(4,ICL)
        DO 30 IZ = 5,LZN
          IF (LZ1.NE.0) THEN
            NZ = IPCLST(IZ,ICL)
            IF (NZ.EQ.0) GOTO 40
          ENDIF
          DO 20 I = 1, NROW
            DO 10 J = 1, NCOL
              CO = ZERO
              DRC = 1.
              IF (LZ1.EQ.0) THEN
                IF (M.EQ.0) CO = SF*DRC
                IF (M.NE.0) THEN
                  IF (RMLT(J,I,M).NE.ZERO) CO = SF*RMLT(J,I,M)*DRC
                ENDIF
              ELSEIF (IZON(J,I,LZ1).EQ.NZ) THEN
                IF (M.EQ.0) CO = SF*DRC
                IF (M.NE.0) THEN
                  IF (RMLT(J,I,M).NE.ZERO) CO = SF*RMLT(J,I,M)*DRC
                ENDIF
              ENDIF
              SV(J,I,NL) = SV(J,I,NL) + CO
   10       CONTINUE
   20     CONTINUE
   30   CONTINUE
   40   CONTINUE
   90 CONTINUE
C
      RETURN
      END
C=======================================================================
      SUBROUTINE SSEN1LPF1NL(H,A,NC,NR,NL,HK,VKA,DELR,DELC,IBOUND,RHS,
     &                       BOTM,NBOTM,CR,CC,CV,K,HANI)
C-----VERSION 1000 01FEB1992
C     ******************************************************************
C     ADD NONLINEAR TERMS FOR SENSITIVITY EQUATION CALCULATIONS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL BOTM, CC, CO, CR, D1CC, D1CR, D2CC, D2CR, DELC, DELR,
     &     RHS, HK, TOP1, TOP2, YX, ZERO, KV1
      INTEGER I, IBOUND, IND, J, K, NC, NC1, NL, NR, NR1,
     &        NRC, KHANI
      DOUBLE PRECISION A(NC*NR*NL), AO, AP, H(NC*NR*NL)
      DOUBLE PRECISION BO, BP, HO, HP, TH1, TH2
      DIMENSION CR(NC,NR,NL), CC(NC,NR,NL), CV(NC,NR,NL), HK(NC,NR,NL),
     &          VKA(NC,NR,NL), DELR(NC), DELC(NR), RHS(NC,NR,NL),
     &          IBOUND(NC,NR,NL), BOTM(NC,NR,0:NBOTM), HANI(NC,NR,NL)
      COMMON /DISCOM/LBOTM(999),LAYCBD(999)
      COMMON /LPFCOM/LAYTYP(999),LAYAVG(999),CHANI(999),LAYVKA(999),
     1               LAYWET(999)
C     ------------------------------------------------------------------
C
      ZERO = 0.0
      NRC = NR*NC
      NR1 = NR - 1
      NC1 = NC - 1
C
C%ERA%HANI
C      YX = CHANI(K)*2.
      YX = CHANI(K)
C
C      CR
C
      IF (NC.GT.1) THEN
        DO 20 I = 1, NR
          DO 10 J = 1, NC1
            IF (IBOUND(J,I,K).EQ.0 .OR. IBOUND(J+1,I,K).EQ.0) GOTO 10
            IND = J + NC*(I-1) + NRC*(K-1)
            HO = H(IND)
            HP = H(IND+1)
            TOP1 = BOTM(J,I,LBOTM(K)-1)
            TOP2 = BOTM(J+1,I,LBOTM(K)-1)
            IF (TOP1.LT.HO .AND. TOP2.LT.HP) GOTO 10
            AO = A(IND)
            AP = A(IND+1)
            BO = BOTM(J,I,LBOTM(K))
            BP = BOTM(J+1,I,LBOTM(K))
            TH1 = HO - BO
            TH2 = HP - BP
C%ERA%HANI
            IF(YX.LE.0.0) THEN
              KHANI=-YX
              YX0=HANI(J,I,KHANI)*2.
              YX1=HANI(J+1,I,KHANI)*2.
            ELSE
              YX0=YX*2.
              YX1=YX*2.
            ENDIF
C-------MATRIX DERIVATIVES
            D1CR = 0.0
            D2CR = 0.0
            IF (TOP1.GT.HO)
     &        D1CR = (CR(J,I,K)**2)*DELR(J)/
     &               (DELC(I)*YX0*HK(J,I,K)*(TH1**2))
            IF (TOP2.GT.HP)
     &        D2CR = (CR(J,I,K)**2)*DELR(J+1)/
     &               (DELC(I)*YX1*HK(J+1,I,K)*(TH2**2))
C-------MULTIPLY BY SENSITIVITIES FROM LAST ITERATION
            CO = D1CR*AO + D2CR*AP
C-------MULTIPLY BY HEAD VECTOR AND ADD TO RHS
            RHS(J,I,K) = RHS(J,I,K) - CO*(HP-HO)
            RHS(J+1,I,K) = RHS(J+1,I,K) - CO*(HO-HP)
   10     CONTINUE
   20   CONTINUE
      ENDIF
C
C      CC
C
      IF (NR.GT.1) THEN
        DO 40 J = 1, NC
          DO 30 I = 1, NR1
            IF (IBOUND(J,I,K).EQ.0 .OR. IBOUND(J,I+1,K).EQ.0) GOTO 30
            IND = J + NC*(I-1) + NRC*(K-1)
            HO = H(IND)
            HP = H(IND+NC)
            TOP1 = BOTM(J,I,LBOTM(K)-1)
            TOP2 = BOTM(J,I+1,LBOTM(K)-1)
            IF (TOP1.LT.HO .AND. TOP2.LT.HP) GOTO 30
            AO = A(IND)
            AP = A(IND+NC)
            BO = BOTM(J,I,LBOTM(K))
            BP = BOTM(J,I+1,LBOTM(K))
            TH1 = HO - BO
            TH2 = HP - BP
C-------MATRIX DERIVATIVES
            D1CC = 0.0
            D2CC = 0.0
            IF (TOP1.GT.HO)
     &          D1CC = (CC(J,I,K)**2)*DELC(I)/
     &                 (2.*DELR(J)*HK(J,I,K)*(TH1**2))
            IF (TOP2.GT.HP)
     &          D2CC = (CC(J,I,K)**2)*DELC(I+1)/
     &                 (2.*DELR(J)*HK(J,I+1,K)*(TH2**2))
C-------MULTIPLY BY DERIVATIVES FROM LAST ITERATION
            CO = D1CC*AO + D2CC*AP
C-------MULTIPLY BY HEAD VECTOR AND ADD TO RHS
            RHS(J,I,K) = RHS(J,I,K) - CO*(HP-HO)
            RHS(J,I+1,K) = RHS(J,I+1,K) - CO*(HO-HP)
   30     CONTINUE
   40   CONTINUE
      ENDIF
C
C      CV
C
      IF (NL.GT.1.AND.K.LT.NL) THEN
        DO 60 J = 1, NC
          DO 50 I = 1, NR
            IF (IBOUND(J,I,K).EQ.0 .OR. IBOUND(J,I,K+1).EQ.0) GOTO 50
            IND = J + NC*(I-1) + NRC*(K-1)
            HO = H(IND)
            HP = H(IND+NRC)
            TOP1 = BOTM(J,I,LBOTM(K)-1)
            IF (TOP1.LT.HO) GOTO 50
            AO = A(IND)
            BO = BOTM(J,I,LBOTM(K))
            TH1 = HO - BO
            KV1 = VKA(J,I,K)
            IF(LAYVKA(K).NE.0) KV1= HK(J,I,K)/KV1
C-------MATRIX DERIVATIVES
            D1CV=0.
            IF (TOP1.GT.HO)
     &          D1CV = -(CV(J,I,K)**2)/(DELC(I)*2.*DELR(J)*KV1)
C-------MULTIPLY BY DERIVATIVES FROM LAST ITERATION
            CO = ZERO
            CO = D1CV*AO
C-------MULTIPLY BY HEAD VECTOR AND ADD TO RHS
            RHS(J,I,K) = RHS(J,I,K) - CO*(HP-HO)
            RHS(J,I,K+1) = RHS(J,I,K+1) - CO*(HO-HP)
   50     CONTINUE
   60   CONTINUE
      ENDIF
      RETURN
      END
C=======================================================================
      SUBROUTINE SSEN1LPF1CH(CO,TH1,HP,I,J,K,CHAR,IL,M,RMLT0,RMLT,LZ1,
     &                       IZON,SF,LT,HK,NCOL,NROW,NLAY,DELC,DELR,H,
     &                       TH0,BOT,TOP,NMLTAR,NZONAR,ICL,C,HANI)
C-----VERSION 1000 08AUG1995
C     VERSION 19990405 ERB
C     FORMERLY SSN1P1
C     ******************************************************************
C     CALCULATE THE DERIVATIVE OF THE HORIZONTAL CONDUCTANCES WITH
C     RESPECT TO PARAMETER VALUES, FOR HARMONIC MEAN CONDUCTANCES
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL AN, BOT, CO, D0, D1, DC, DELC, DELR, DR, DT0, DT1, DU, DV,
     &     FAC, SF, RMLT, RMLT0, RMLT1, HK, T0, T1, TH0, TH1,
     &     TOP, U, V, ZERO
      INTEGER I, IFLAG1, II, IJ, IL, IND, IZ, J, K, LT, LZ1,
     &        M, IZON, NCOL, NLAY, NROW, NZ, KHANI
      CHARACTER*2 CHAR
      DIMENSION RMLT(NCOL,NROW,NMLTAR), IZON(NCOL,NROW,NZONAR),
     &          HK(NCOL,NROW,NLAY), DELC(NROW), DELR(NCOL),
     &          BOT(NCOL,NROW), TOP(NCOL,NROW), HANI(NCOL,NROW,NLAY)
      DOUBLE PRECISION DZERO, H(NCOL*NROW*NLAY), HP
      COMMON /LPFCOM/LAYTYP(999),LAYAVG(999),CHANI(999),LAYVKA(999),
     1               LAYWET(999)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
      DZERO = 0.0
      ZERO = 0.0
      HP = DZERO
      CO = ZERO
      DR = DELR(J)
      DC = DELC(I)
      II = 0
      IJ = 0
      AN = CHANI(K)
C%ERA%HANI
      AN0=1.0
      AN1=1.0
      IND = J + NCOL*(I-1) + NROW*NCOL*(K-1)
      IF (CHAR.EQ.'CR') IJ = 1
      IF (CHAR.EQ.'CC') II = 1
      RMLT1 = 1.
      IF (IL.GT.0) THEN
        IF (M.EQ.0) RMLT1 = SF
        IF (M.GT.0) RMLT1 = SF*RMLT(J+IJ,I+II,M)
        IF (RMLT1.NE.ZERO .AND. LZ1.GT.0) THEN
          IFLAG1 = 0
          DO 10 IZ = 5,IPCLST(4,ICL)
            NZ = IPCLST(IZ,ICL)
            IF (NZ.EQ.0 .OR. IFLAG1.EQ.1) GOTO 20
            IF (IZON(J+IJ,I+II,LZ1).EQ.NZ) IFLAG1 = 1
   10     CONTINUE
   20     IF (IFLAG1.EQ.0) RMLT1 = ZERO
        ENDIF
      ENDIF
      IF (IL.GT.0 .AND. RMLT0.EQ.ZERO .AND. RMLT1.EQ.ZERO) RETURN
      IF (CHAR.EQ.'CR') THEN
        HP = H(IND+1)
        FAC = 2.*DC
        D0 = DR
        D1 = DELR(J+1)
      ELSEIF (CHAR.EQ.'CC') THEN
        HP = H(IND+NCOL)
C%ERA%HANI
        IF(AN.LE.0.0) THEN
          KHANI=-AN
          AN0=HANI(J,I,KHANI)
          AN1=HANI(J,I+II,KHANI)
        ELSE
          AN0=AN
          AN1=AN
        ENDIF
C%ERA%HANI
        FAC = 2.*DR
        D0 = DC
        D1 = DELC(I+1)
      ENDIF
      TH1 = TOP(J+IJ,I+II) - BOT(J+IJ,I+II)
      IF (LT.GT.0 .AND. HP.LT.TOP(J+IJ,I+II)) TH1 = HP - BOT(J+IJ,I+II)
C%ERA%HANI
      T0 = HK(J,I,K)*AN0*TH0
      T1 = HK(J+IJ,I+II,K)*AN1*TH1
      DT0 = RMLT0*AN0*TH0
      DT1 = RMLT1*AN1*TH1
C U AND V ARE THE NUMERATOR AND DENOMINATOR, RESPECTIVELY, OF THE
C CONDUCTANCE TERM DIVIDED BY WHAT IS ALREADY IN FAC.
C DU AND DV ARE THEIR DERIVATIVES WITH RESPECT TO THE PARAMETER.
C UOV IS U DIVIDED BY V (U OVER V).
      U = T0*T1
      V = T0*D1 + T1*D0
      DU = T0*DT1 + T1*DT0
      DV = D1*DT0 + D0*DT1
      CO=0.
C-----CHANGE VALUE TO MACHINE ZERO -- ASK STEVE
      IF(ABS(V).GT.1E-24) THEN
        C = FAC*U/V
        CO = FAC*(1/V**2)*(V*DU-U*DV)
      ENDIF
      RETURN
      END
C=======================================================================
      SUBROUTINE SSEN1LPF1CHN(CO,TH1,HP,I,J,K,IL,M,RMLT0,RMLT,LZ1,IZON,
     &                        SF,LT,HK,NCOL,NROW,NLAY,DELC,DELR,H,TH0,
     &                        BOT,TOP,NMLTAR,NZONAR,ICL,C,HANI)
C-----VERSION 1000 08AUG1995
C     VERSION 19990405 ERB
C     FORMERLY SSN1P1
C     ******************************************************************
C     CALCULATE THE DERIVATIVE OF THE HORIZONTAL CONDUCTANCES WITH
C     RESPECT TO HORIZONTAL ANISOTROPY, FOR HARMONIC MEAN CONDUCTANCES
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL AN, BOT, CO, D0, D1, DC, DELC, DELR, DR, DT0, DT1, DU, DV,
     &     FAC, SF, RMLT, RMLT0, RMLT1, HK, T0, T1, TH0, TH1,
     &     TOP, U, V, ZERO
      INTEGER I, IFLAG1, IL, IND, IZ, J, K, LT, LZ1,
     &        M, IZON, NCOL, NLAY, NROW, NZ, KHANI
      DIMENSION RMLT(NCOL,NROW,NMLTAR), IZON(NCOL,NROW,NZONAR),
     &          HK(NCOL,NROW,NLAY), DELC(NROW), DELR(NCOL),
     &          BOT(NCOL,NROW), TOP(NCOL,NROW) ,HANI(NCOL,NROW,NLAY)
      DOUBLE PRECISION DZERO, H(NCOL*NROW*NLAY), HP
      COMMON /LPFCOM/LAYTYP(999),LAYAVG(999),CHANI(999),LAYVKA(999),
     1               LAYWET(999)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
      DZERO = 0.0
      ZERO = 0.0
      HP = DZERO
      CO = ZERO
      DR = DELR(J)
      DC = DELC(I)
      IND = J + NCOL*(I-1) + NROW*NCOL*(K-1)
      RMLT1 = 1.
      AN = CHANI(K)
      IF (IL.GT.0) THEN
        IF (M.EQ.0) RMLT1 = SF
        IF (M.GT.0) RMLT1 = SF*RMLT(J,I+1,M)
        IF (RMLT1.NE.ZERO .AND. LZ1.GT.0) THEN
          IFLAG1 = 0
          DO 10 IZ = 5,IPCLST(4,ICL)
            NZ = IPCLST(IZ,ICL)
            IF (NZ.EQ.0 .OR. IFLAG1.EQ.1) GOTO 20
            IF (IZON(J,I+1,LZ1).EQ.NZ) IFLAG1 = 1
   10     CONTINUE
   20     IF (IFLAG1.EQ.0) RMLT1 = ZERO
        ENDIF
      ENDIF
      IF (IL.GT.0 .AND. RMLT0.EQ.ZERO .AND. RMLT1.EQ.ZERO) RETURN
      HP = H(IND+NCOL)
      FAC = 2.*DR
      D0 = DC
      D1 = DELC(I+1)
      TH1 = TOP(J,I+1) - BOT(J,I+1)
      IF (LT.NE.0 .AND. HP.LT.TOP(J,I+1)) TH1 = HP - BOT(J,I+1)
      KHANI=-AN
      T0 = HK(J,I,K)*HANI(J,I,KHANI)*TH0
      T1 = HK(J,I+1,K)*HANI(J,I+1,KHANI)*TH1
      DT0 = HK(J,I,K)*RMLT0*TH0
      DT1 = HK(J,I+1,K)*RMLT1*TH1
C U AND V ARE THE NUMERATOR AND DENOMINATOR, RESPECTIVELY, OF THE
C CONDUCTANCE TERM DIVIDED BY WHAT IS ALREADY IN FAC.
C DU AND DV ARE THEIR DERIVATIVES WITH RESPECT TO THE PARAMETER.
C UOV IS U DIVIDED BY V (U OVER V).
      U = T0*T1
      V = T0*D1 + T1*D0
      DU = T0*DT1 + T1*DT0
      DV = D1*DT0 + D0*DT1
      CO=0.
C-----CHANGE VALUE TO MACHINE ZERO -- ASK STEVE
      IF(ABS(V).GT.1E-24) THEN
        C = FAC*U/V
        CO = FAC*(1/V**2)*(V*DU-U*DV)
      ENDIF
      RETURN
      END
C=======================================================================
      SUBROUTINE SSEN1LPF1CV(CO,CO1,IBP,IBM,PID,IL,SF,RMLT,M,NCOL,NROW,
     &                       LZ1,CV,SV,NLAY,DELR,DELC,J,I,K,HK,IZON,
     &                       IBOUND,NMLTAR,NZONAR,ICL,BOTM,NBOTM,VKA,
     &                       HNEW)
C-----VERSION 1000 08AUG1995
C     VERSION 19990405 ERB
C     FORMERLY SSN1P2
C     ******************************************************************
C     CALCULATE THE DERIVATIVE OF THE VERTICAL CONDUCTANCES WITH
C     RESPECT TO PARAMETER VALUES
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL BOTM, CO, CO1, COD, CON, CV, DELC, DELR, DELZCB,
     &     SF, SM, RMLT, SV, TH0, ZERO
      DOUBLE PRECISION HNEW(NCOL,NROW,NLAY)
      INTEGER I, IBM, IBOUND, IBP, IFLAG1, IL, IZ, J, K,
     &        LZ1, M, IZON, NCOL, NLAY, NROW, NZ
      CHARACTER*4 PID
      DIMENSION RMLT(NCOL,NROW,NMLTAR), BOTM(NCOL,NROW,0:NBOTM),
     &          SV(NCOL,NROW,NLAY), IZON(NCOL,NROW,NZONAR),
     &          HK(NCOL,NROW,NLAY), DELC(NROW), DELR(NCOL),
     &          IBOUND(NCOL,NROW,NLAY),
     &          CV(NCOL,NROW,NLAY), VKA(NCOL,NROW,NLAY)
      INCLUDE 'param.inc'
      COMMON /DISCOM/LBOTM(999),LAYCBD(999)
      COMMON /LPFCOM/LAYTYP(999),LAYAVG(999),CHANI(999),LAYVKA(999),
     &               LAYWET(999)
C     ------------------------------------------------------------------
      ZERO = 0.0
      IBP = 0
      IBM = 0
      CO = ZERO
      CO1 = ZERO
C-----CALCULATE THICKNESSES OF LAYER K AND CONFINING BED BELOW LAYER K
      TH0 = BOTM(J,I,LBOTM(K)-1) - BOTM(J,I,LBOTM(K))
      IF (LAYTYP(K).NE.0 .AND. HNEW(J,I,K).LT.BOTM(J,I,LBOTM(K)-1)) THEN
        TH0 = HNEW(J,I,K) - BOTM(J,I,LBOTM(K))
      ENDIF
      IF (K.LT.NLAY) DELZCB = BOTM(J,I,LBOTM(K))
     &                        - BOTM(J,I,LBOTM(K+1)-1)
C
      IF (K.LT.NLAY) IBP = IBOUND(J,I,K+1)
      IF (K.GT.1) IBM = IBOUND(J,I,K-1)
      IF (IBOUND(J,I,K).EQ.0 .OR. (PID.EQ.'VKCB'.AND.IBP.EQ.0) .OR.
     &    (PID.NE.'VKCB'.AND.IBP.EQ.0.AND.IBM.EQ.0)) RETURN
      SM = 1.
      IF ((PID.EQ.'VKCB' .OR. PID.EQ.'HK  ' .OR. PID.EQ.'VK  ' .OR.
     &     PID.EQ.'VANI') .AND. IL.GT.0) THEN
        SM = SF
        IF (M.NE.0) THEN
          SM = ZERO
          IF (RMLT(J,I,M).NE.ZERO) SM = SF*RMLT(J,I,M)
        ENDIF
        IF (SM.NE.ZERO .AND. LZ1.GT.0) THEN
          IFLAG1 = 0
          DO 10 IZ = 5,IPCLST(4,ICL)
            NZ = IPCLST(IZ,ICL)
            IF (NZ.EQ.0 .OR. IFLAG1.EQ.1) GOTO 20
            IF (IZON(J,I,LZ1).EQ.NZ) IFLAG1 = 1
   10     CONTINUE
   20     IF (IFLAG1.EQ.0) SM = ZERO
        ENDIF
        IF (SM.EQ.ZERO) RETURN
      ENDIF
      IF (PID.EQ.'VKCB')
     &    CO = (CV(J,I,K)**2)*SM*DELZCB/((SV(J,I,K)**2)*DELR(J)*DELC(I))
      IF (PID.EQ.'HK  ' .OR. PID.EQ.'VK  ' .OR. PID.EQ.'VANI') THEN
        IF (PID.EQ.'HK  ') THEN
          CON = -((TH0)/2.)*SM*VKA(J,I,K)
          COD = (HK(J,I,K)**2)*DELR(J)*DELC(I)
        ENDIF
        IF (PID.EQ.'VK  ' .OR. PID.EQ.'VANI') THEN
          CON = SM*TH0/2.
          IF (LAYVKA(K).NE.0) COD = HK(J,I,K)*DELR(J)*DELC(I)
          IF (LAYVKA(K).EQ.0) COD = -(VKA(J,I,K)**2)*DELR(J)*DELC(I)
        ENDIF
        IF (K.LT.NLAY .AND. IBP.NE.0) CO = -(CV(J,I,K)**2)*CON/COD
        IF (K.GT.1 .AND. IBM.NE.0) CO1 = -(CV(J,I,K-1)**2)*CON/COD
      ENDIF
C
      RETURN
      END

