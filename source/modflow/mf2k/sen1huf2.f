C     VERSION 2.0, 07/02/2003
C  I edited all occurrences of common block HUFCOM (in sen1huf2.f, 
C  lmt6.f, gwfhuf2.f, and obs1bas6.f) to put all REAL arrays before all 
C  INTEGER arrays.  The original order is OK when both REALs and 
C  INTEGERs are KIND=4.  But when REALs are promoted to DOUBLE 
C  PRECISION, KIND goes from 4 to 8, and this generates alignment 
C  problems.  The alignment problems are avoided when all variables of 
C  larger KIND precede all variables of smaller KIND. -- ERB 6/29/2006
      SUBROUTINE SEN1HUF2FM(H,NCOL,NROW,NLAY,PID,HK,HKCC,DELR,
     &                     DELC,IBOUND,RHS,CV,BOTM,NBOTM,HUFTHK,
     &                     NHUF,IP,IZON,NZONAR,RMLT,NMLTAR,IUHFBP,
     &                     HFBP,MXACTFB,NHFB,HOLD,DELT,ISS,IOUT,GS)
C     ******************************************************************
C      CALCULATE MATRIX DERIVATIVES AND MULTIPLY BY HEADS AS NEEDED.
C      ADD RESULTING CONTRIBUTION TO RHS.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL CO, CV,  DELC, DELR, C, DELT, HH, R0, RHS, RMLT, RMLT0, TH0,
     &     ZERO, ONE, BOTM , MID1 , MID2, TH0L, TH1L
      INTEGER I, IBM, IBOUND, IBP, IND, ISS, J, K, LT, IZON, NCOL, NLAY,
     &        NRC, NROW
      CHARACTER*4 PID
      DOUBLE PRECISION H(NCOL*NROW*NLAY), HP
      DIMENSION DELR(NCOL), DELC(NROW),
     & RHS(NCOL,NROW,NLAY), IBOUND(NCOL,NROW,NLAY),GS(NCOL,NROW),
     & CV(NCOL,NROW,NLAY),BOTM(NCOL,NROW,0:NBOTM),
     & HK(NCOL,NROW,NLAY), HUFTHK(NCOL,NROW,NHUF,2),
     & RMLT(NCOL,NROW,NMLTAR),IZON(NCOL,NROW,NZONAR),
     & HFBP(7,MXACTFB),HOLD(NCOL*NROW*NLAY),HKCC(NCOL,NROW,NLAY)
      COMMON /DISCOM/LBOTM(999),LAYCBD(999)
      COMMON /HUFCOM/HGUHANI(999),HGUVANI(999),LTHUF(999),LAYWT(999)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
C
      ZERO = 0.0
      ONE = 1.0
      NRC = NROW*NCOL
C
      DO 140 ICL = IPLOC(1,IP),IPLOC(2,IP)
        NU = IPCLST(1,ICL)
        NZ = IPCLST(3,ICL)
        NM = IPCLST(2,ICL)
C-----HORIZONTAL CONDUCTANCES
        IF (PID.EQ.'HK  '.OR.PID.EQ.'HANI'.OR.PID.EQ.'KDEP') THEN
          DO 70 I = 1, NROW
            DO 60 J = 1, NCOL
              DO 55 K=1,NLAY
                LT=LTHUF(K)
                IF (IBOUND(J,I,K).EQ.0) GOTO 55
                IND = J + NCOL*(I-1) + NRC*(K-1)
                HO = H(IND)
                R0 = ZERO
                RMLT0 = ONE
                CALL UHUF2RMLT(RMLT0,J,I,NZ,NM,ICL,IZON,NZONAR,
     &                         RMLT,NMLTAR,NROW,NCOL)
C
C Get thicknesses of hydrogeologic unit in this cell and adjacent cells
                TOP0=BOTM(J,I,LBOTM(K)-1)
                IF(LTHUF(K).NE.0.AND.H(IND).LT.TOP0) TOP0=H(IND)
                BOT0=BOTM(J,I,LBOTM(K))
                CALL UHUF2THK(TOP0,BOT0,
     &                          HUFTHK(J,I,NU,1),
     &                          HUFTHK(J,I,NU,2),TH0,D1,D2)
C-------CR
                IF (J.NE.NCOL) THEN
                  IF (PID.NE.'HANI'.AND.IBOUND(J+1,I,K).NE.0) THEN
                    TOP1C=BOTM(J+1,I,LBOTM(K)-1)
                    IF(LTHUF(K).NE.0.AND.H(IND+1).LT.TOP1C)
     &                  TOP1C=H(IND+1)
                    BOT1C=BOTM(J+1,I,LBOTM(K))
                    CALL UHUF2THK(TOP1C,BOT1C,
     &                          HUFTHK(J+1,I,NU,1),
     &                          HUFTHK(J+1,I,NU,2),TH1C,D1,D2)
                    IF(TH0.EQ.0..AND.TH1C.EQ.0.) GOTO 54
                    IF(PID.EQ.'HK') THEN
                      CALL SSEN1HUF2CH(CO,TH0,TH1C,HP,I,J,K,'CR',RMLT0,
     &                          HK,HKCC,NCOL,NROW,NLAY,DELC,DELR,H,
     &                          BOTM(1,1,LBOTM(K)),BOTM(1,1,LBOTM(K)-1),
     &                          NZ,NM,ICL,IZON,NZONAR,RMLT,NMLTAR,C,
     &                          TH0L,TH1L,HUFTHK,NHUF,NU,GS)
                    ELSEIF(PID.EQ.'KDEP') THEN
                      CALL SSEN1HUF2CKDEP(CO,TH0,TH1C,HP,I,J,K,'CR',
     &                          RMLT0,HK,HKCC,NCOL,NROW,NLAY,DELC,DELR,
     &                          H,BOTM(1,1,LBOTM(K)),
     &                          BOTM(1,1,LBOTM(K)-1),GS,
     &                          NZ,NM,ICL,IZON,NZONAR,RMLT,NMLTAR,C,
     &                          TH0L,TH1L,HUFTHK,NHUF,NU)
                    ENDIF
                  IF (IUHFBP.GT.0 .AND. CO.NE.0.)
     &                CALL SSEN1HFB6MD(C,'CR',CO,DELC,DELR,HFBP,I,J,K,
     &                                 MXACTFB,NCOL,NHFB,NROW,
     &                                 TH0L,TH1L)
                    HH = HO - HP
                    R0 = R0 + CO*HH
                    RHS(J+1,I,K) = RHS(J+1,I,K) - CO*HH
                  ENDIF
                ENDIF
C-------CC
   54           IF (I.EQ.NROW) GOTO 50
                IF (IBOUND(J,I+1,K).EQ.0) GOTO 50
                  TOP1R=BOTM(J,I+1,LBOTM(K)-1)
                  IF(LTHUF(K).NE.0.AND.H(IND+NCOL).LT.TOP1R)
     &                TOP1R=H(IND+NCOL)
                  BOT1R=BOTM(J,I+1,LBOTM(K))
                  CALL UHUF2THK(TOP1R,BOT1R,
     &                          HUFTHK(J,I+1,NU,1),
     &                          HUFTHK(J,I+1,NU,2),TH1R,D1,D2)
                  IF(TH0.EQ.0..AND.TH1R.EQ.0.) GOTO 50
                IF (PID.EQ.'HK  ') THEN
                  CALL SSEN1HUF2CH(CO,TH0,TH1R,HP,I,J,K,'CC',RMLT0,
     &                          HK,HKCC,NCOL,NROW,NLAY,DELC,DELR,H,
     &                          BOTM(1,1,LBOTM(K)),BOTM(1,1,LBOTM(K)-1),
     &                          NZ,NM,ICL,IZON,NZONAR,RMLT,NMLTAR,C,
     &                          TH0L,TH1L,HUFTHK,NHUF,NU,GS)
                ELSEIF(PID.EQ.'HANI') THEN
                  CALL SSEN1HUF2CHN(CO,TH0,TH1R,HP,I,J,K,RMLT0,
     &                          HKCC,NCOL,NROW,NLAY,DELC,DELR,H,
     &                          BOTM(1,1,LBOTM(K)),BOTM(1,1,LBOTM(K)-1),
     &                          GS,NZ,NM,ICL,IZON,NZONAR,RMLT,NMLTAR,C,
     &                          TH0L,TH1L,HUFTHK,NHUF,NU)
                ELSEIF(PID.EQ.'KDEP') THEN
                  CALL SSEN1HUF2CKDEP(CO,TH0,TH1R,HP,I,J,K,'CC',
     &                          RMLT0,HK,HKCC,NCOL,NROW,NLAY,DELC,DELR,
     &                          H,BOTM(1,1,LBOTM(K)),
     &                          BOTM(1,1,LBOTM(K)-1),GS,
     &                          NZ,NM,ICL,IZON,NZONAR,RMLT,NMLTAR,C,
     &                          TH0L,TH1L,HUFTHK,NHUF,NU)
                ENDIF
                IF (IUHFBP.GT.0 .AND. CO.NE.0.)
     &              CALL SSEN1HFB6MD(C,'CC',CO,DELC,DELR,HFBP,I,J,K,
     &                               MXACTFB,NCOL,NHFB,NROW,TH0L,
     &                               TH1L)
                  IF (CO.EQ.ZERO) GOTO 50
                HH = HO - HP
                R0 = R0 + CO*HH
                RHS(J,I+1,K) = RHS(J,I+1,K) - CO*HH
   50           RHS(J,I,K) = RHS(J,I,K) + R0
   55         CONTINUE
   60       CONTINUE
   70     CONTINUE
        ENDIF
C--End Horizontal
C-------CV
        IF (PID.EQ.'VK'.OR.PID.EQ.'VANI'
     &      .OR.(PID.EQ.'HK'.AND.HGUVANI(NU).NE.0.0)
     &      .OR.(PID.EQ.'KDEP'.AND.HGUVANI(NU).NE.0.0)) THEN
          DO 90 I = 1, NROW
            DO 80 J = 1, NCOL
              CALL UHUF2RMLT(RMLT0,J,I,NZ,NM,ICL,IZON,NZONAR,
     &                       RMLT,NMLTAR,NROW,NCOL)
              IF(RMLT0.EQ.0.) GOTO 80
              DO 85 K=1,NLAY-1
                IND = J + NCOL*(I-1) + NRC*(K-1)
                TOP1=BOTM(J,I,LBOTM(K)-1)
                IF(LTHUF(K).NE.0.AND.H(IND).LT.TOP1) TOP1=H(IND)
                BOT1=BOTM(J,I,LBOTM(K))
                TOP2=BOTM(J,I,LBOTM(K))
                BOT2=BOTM(J,I,LBOTM(K)+1)
                MID1=0.5*(TOP1+BOT1)
                MID2=0.5*(TOP2+BOT2)
                IF(LTHUF(K+1).NE.0.AND.H(IND+NRC).LT.TOP2)
     &              MID2 = TOP2
                CALL UHUF2THK(MID1,MID2,
     &                  HUFTHK(J,I,NU,1),HUFTHK(J,I,NU,2),THK1,D1,D2)
                IF(THK1.EQ.0.) GOTO 85
                CALL SSEN1HUF2CV(PID,CO,THK1,IBP,IBM,NCOL,NROW,NLAY,CV,
     &                        DELR,DELC,J,I,K,IBOUND,NU,IZON,
     &                        NZONAR,RMLT0,RMLT,NMLTAR,IOUT,HUFTHK,NHUF,
     &                        D1,D2,GS(J,I))
                IF (CO.EQ.ZERO) GOTO 85
                HH = ZERO
                IF (K.LT.NLAY .AND. IBP.NE.0) HH = H(IND) - H(IND+NRC)
                IF (K.LT.NLAY .AND. IBP.NE.0) THEN
                  RHS(J,I,K) = RHS(J,I,K) + CO*HH
                  RHS(J,I,K+1) = RHS(J,I,K+1) - CO*HH
C               ACCOUNT FOR UNCONFINED LAYER UNDERLYING ACTIVE LAYER
                  IF (LTHUF(K+1).NE.0 .AND.H(IND+NRC).LT.BOT1) THEN
                    RHS(J,I,K) = RHS(J,I,K) - CO*(BOT1-H(IND+NRC))
                    RHS(J,I,K+1) = RHS(J,I,K+1) + CO*(BOT1-H(IND+NRC))
                  ENDIF
                ENDIF
   85         CONTINUE
   80       CONTINUE
   90     CONTINUE
        ENDIF
C-----S
        IF (PID.EQ.'SS  ' .OR. PID.EQ.'SY  ' .OR. PID.EQ.'SYTP') THEN
          IF (ISS.NE.0) GOTO 140
          DO 130 I = 1, NROW
            DO 120 J = 1, NCOL
              CALL UHUF2RMLT(RMLT0,J,I,NZ,NM,ICL,IZON,NZONAR,
     &                       RMLT,NMLTAR,NROW,NCOL)
              IF(RMLT0.EQ.0.0) GOTO 120
              DO 110 K = 1, NLAY
                IF (IBOUND(J,I,K).LT.1) GOTO 110
                LT=LTHUF(K)
                IND = J + NCOL*(I-1) + NRC*(K-1)
                HO = H(IND)
                SHO = HOLD(IND)
                TOP = BOTM(J,I,LBOTM(K)-1)
                BOT = BOTM(J,I,LBOTM(K))
                IF (LT.NE.0) THEN
                  IF (PID.EQ.'SS  ' .AND. HO.LT.TOP .AND. SHO.LT.TOP)
     &              GOTO 110
                  IF (PID.EQ.'SY  ' .AND. HO.GE.TOP .AND. SHO.GE.TOP)
     &              GOTO 110
                ELSEIF(LT.EQ.0 .AND. PID.EQ.'SY  ') THEN
                  GOTO 110
                ENDIF
                TOPU = HUFTHK(J,I,NU,1)
                THCKU = HUFTHK(J,I,NU,2)
                BOTU = TOPU - THCKU
                CALL UHUF2THK(TOP,BOT,TOPU,THCKU,TH0,D1,D2)
                IF(ABS(TH0).LT.1E-6) GOTO 110
                IF (PID.EQ.'SS  ') THEN
                  CO = RMLT0*TH0*DELR(J)*DELC(I)/DELT
                  IF (LT.NE.0) THEN
                    IF (SHO.GE.TOP .AND. HO.LT.TOP) THEN
                      HO = TOP
                    ELSEIF (SHO.LT.TOP .AND. HO.GE.TOP) THEN
                      SHO = TOP
                    ENDIF
                  ENDIF
                  RHS(J,I,K) = RHS(J,I,K) - CO*(SHO-HO)
                ELSEIF(PID.EQ.'SY  ') THEN
                  CO = RMLT0*DELR(J)*DELC(I)/DELT
                  CALL SEN1HUF2SC2(TOP,BOT,TOPU,BOTU,HO,SHO,CRHS,CO)
                  RHS(J,I,K) = RHS(J,I,K) - CRHS
                ELSEIF(PID.EQ.'SYTP') THEN
                  CO = RMLT0*DELR(J)*DELC(I)/DELT
                  RHS(J,I,K) = RHS(J,I,K) - CO*(SHO-HO)
                ENDIF
  110         CONTINUE
  120       CONTINUE
  130     CONTINUE
        ENDIF
  140 CONTINUE
  150 CONTINUE
C
      RETURN
      END
C=======================================================================
      SUBROUTINE SEN1HUF2UN(ISS,DELT,NCOL,NROW,NLAY,SOLD,HNEW,
     &                  SNEW,DELR,DELC,IBOUND,RHS,SC1,CR,CC,KITER,
     &                  HK,HKCC,BOTM,NBOTM,HOLD,CV,HUFTHK,NHUF,IZON,
     &                  NZONAR,RMLT,NMLTAR,GS,IOUT)
C     ******************************************************************
C     COMPUTE SENSITIVITY-EQUATION RHS TERMS FOR UNCONFINED AQUIFERS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL BOTM, CC, CR, CV, DELC, DELR, DELT, HOLD, RHS, SC1, SCC,
     &     HK
      INTEGER I, IBOUND, J, K, KITER, KT, LT, NCOL, NLAY,
     &        NROW, NBOTM
      DOUBLE PRECISION HNEW(NCOL,NROW,NLAY), SNEW(NCOL,NROW,NLAY)
      DIMENSION HOLD(NCOL,NROW,NLAY), SOLD(NCOL,NROW,NLAY),
     &          DELR(NCOL), DELC(NROW), IBOUND(NCOL,NROW,NLAY),
     &          RHS(NCOL,NROW,NLAY), BOTM(NCOL,NROW,0:NBOTM),
     &          CC(NCOL,NROW,NLAY), CR(NCOL,NROW,NLAY),
     &          SC1(NCOL,NROW,NLAY),HK(NCOL,NROW,NLAY),
     &          HKCC(NCOL,NROW,NLAY), CV(NCOL,NROW,NLAY),
     &          HUFTHK(NCOL,NROW,NHUF,2), IZON(NCOL,NROW,NZONAR),
     &          RMLT(NCOL,NROW,NMLTAR), GS(NCOL,NROW)
      COMMON /DISCOM/LBOTM(999),LAYCBD(999)
      COMMON /HUFCOM/HGUHANI(999),HGUVANI(999),LTHUF(999),LAYWT(999)
C     ------------------------------------------------------------------
C
C-------TERMS FOR UNCONFINED AQUIFERS
      IF (KITER.GT.1) THEN
        DO 60 K = 1, NLAY
          LT = LTHUF(K)
          IF (LT.NE.0) THEN
            CALL SSEN1HUF2NL(HNEW,SNEW,NCOL,NROW,NLAY,HK,HKCC,DELR,DELC,
     &                       IBOUND,RHS,BOTM,NBOTM,CR,CC,CV,K,LT,NHUF,
     &                       HUFTHK,RMLT,IZON,NMLTAR,NZONAR,GS)
          ENDIF
          IF (K.GT.1 .AND. LT.NE.0) THEN
            DO 50 I = 1, NROW
              DO 40 J = 1, NCOL
                IF (IBOUND(J,I,K).NE.0 .AND. IBOUND(J,I,K-1).NE.0
     &              .AND. HNEW(J,I,K).LT.BOTM(J,I,LBOTM(K)-1)) THEN
                  RHS(J,I,K-1) = RHS(J,I,K-1)+CV(J,I,K-1)*SNEW(J,I,K)
                  RHS(J,I,K) = RHS(J,I,K)-CV(J,I,K-1)*SNEW(J,I,K)
                ENDIF
   40         CONTINUE
   50       CONTINUE
          ENDIF
   60   CONTINUE
      ENDIF
C
C-------B MATRIX TIMES SOLUTION FROM LAST TIME STEP FOR SENSITIVITY-
C-------EQUATION SENSITIVITIES
      IF (ISS.EQ.0) THEN
        KT = 0
        DO 110 K = 1, NLAY
          IF (LTHUF(K).EQ.0) THEN
            DO 80 I = 1, NROW
              DO 70 J = 1, NCOL
                RHS(J,I,K) = RHS(J,I,K) - SOLD(J,I,K)*SC1(J,I,K)/DELT
   70         CONTINUE
   80       CONTINUE
          ELSE
            KT = KT + 1
            DO 100 I = 1, NROW
              DO 90 J = 1, NCOL
                IF(IBOUND(J,I,K).EQ.0) GOTO 90
                SCC = SC1(J,I,K)
                HO=HOLD(J,I,K)
                TOP=BOTM(J,I,LBOTM(K)-1)
                BOT=BOTM(J,I,LBOTM(K))
                IF (HO.LT.TOP) THEN
                    SCC = 0.0
                    CALL SGWF1HUF2SC2(2,J,I,K,TOP,BOT,1.0D0,HO,1.0,DUM,
     &                    SCC,HUFTHK,NCOL,NROW,NHUF,IZON,NZONAR,RMLT,
     &                    NMLTAR,DELR(J)*DELC(I),IOUT)
                ENDIF
                RHS(J,I,K) = RHS(J,I,K) - SOLD(J,I,K)*SCC/DELT
   90         CONTINUE
  100       CONTINUE
          ENDIF
  110   CONTINUE
      ENDIF
      RETURN
      END
C=======================================================================
      SUBROUTINE SSEN1HUF2NL(HN,SN,NC,NR,NL,HK,HKCC,DELR,DELC,IBOUND,
     &  RHS,BOTM,NBOTM,CR,CC,CV,K,LT,NHUF,HUFTHK,RMLT,IZON,NMLTAR,
     &  NZONAR,GS)
C     ******************************************************************
C     ADD NONLINEAR TERMS FOR SENSITIVITY EQUATION CALCULATIONS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL BO, BOTM, CC, CO, COCC, COCR, CR, D1CC, D1CR, D2CC, D2CR,
     &     DELC, DELR,RHS, HK, THO, THI1, THJ1, TOPO, TOPI1, TOPJ1,
     &     ZERO, MID, MID2, LAMBDA,MULTKDEP
      INTEGER I, IBOUND, J, K, LT, NC, NL, NR
      DOUBLE PRECISION SN(NC,NR,NL),AO,AI1,AJ1,HN(NC,NR,NL),
     &  HO,HI1,HJ1,HP
      DIMENSION CR(NC,NR,NL), CC(NC,NR,NL), HK(NC,NR,NL),
     &  DELR(NC), DELC(NR), RHS(NC,NR,NL), IBOUND(NC,NR,NL),
     &  BOTM(NC,NR,0:NBOTM),HKCC(NC,NR,NL),HUFTHK(NC,NR,NHUF,2),
     &  HUFTMP(999), CV(NC,NR,NL), RMLT(NC,NR,NMLTAR),
     &  IZON(NC,NR,NZONAR), GS(NC,NR)
      COMMON /DISCOM/LBOTM(999),LAYCBD(999)
      COMMON /HUFCOM/HGUHANI(999),HGUVANI(999),LTHUF(999),LAYWT(999)
C     ------------------------------------------------------------------
C
      ZERO = 0.0
C
C      CR & CC
C
      DO 20 I = 1, NR
        DO 10 J = 1, NC
          IBDO = IBOUND(J,I,K)
          IF(IBDO.EQ.0) GOTO 10
          HO = HN(J,I,K)
          TOPO = BOTM(J,I,LBOTM(K)-1)
          BO = BOTM(J,I,LBOTM(K))
          THO = HO - BO
          TRO = HK(J,I,K) * THO
          TCO = HKCC(J,I,K) * THO
          AO = SN(J,I,K)
          NUO = 0
          HSING = HO
          IF(TOPO.GT.HO) CALL SSEN1HUF2HDFND(HUFTHK,NC,NR,NHUF,
     &                                       HSING,I,J,NUO)
          IF(NUO.NE.0) CALL SSEN1HUF2HKFND(NC,NR,NHUF,HUFTHK,IZON,
     &                                     NZONAR,RMLT,NMLTAR,IOUT,
     &                                     I,J,NUO,HKO,HKCCO,
     &                                     BOTM(1,1,LBOTM(K)-1),
     &                                     BOTM(1,1,LBOTM(K)),
     &                                     GS)
          IF(J.LT.NC) THEN
            IBDJ1 = IBOUND(J+1,I,K)
          ELSE
            IBDJ1 = 0
          ENDIF
          IF(I.LT.NR) THEN
            IBDI1 = IBOUND(J,I+1,K)
          ELSE
            IBDI1 = 0
          ENDIF
          HI1 = ZERO
          TOPI1 = ZERO
          IF(IBDI1.NE.0) THEN
            HI1 = HN(J,I+1,K)
            TOPI1 = BOTM(J,I+1,LBOTM(K)-1)
            BI1 = BOTM(J,I+1,LBOTM(K))
            THI1 = HI1 - BI1
            TCI1 = HKCC(J,I+1,K) * THI1
            AI1 = SN(J,I+1,K)
            NUI1 = 0
            HSING = HI1
            IF(TOPI1.GT.HI1) CALL SSEN1HUF2HDFND(HUFTHK,NC,NR,NHUF,
     &                                           HSING,I+1,J,NUI1)
            IF(NUI1.NE.0) CALL SSEN1HUF2HKFND(NC,NR,NHUF,HUFTHK,IZON,
     &                                        NZONAR,RMLT,NMLTAR,IOUT,
     &                                        I+1,J,NUI1,HKI1,HKCCI1,
     &                                        BOTM(1,1,LBOTM(K)-1),
     &                                        BOTM(1,1,LBOTM(K)),
     &                                        GS)
          ENDIF
          HJ1 = ZERO
          TOPJ1 = ZERO
          IF(IBDJ1.NE.0) THEN
            HJ1 = HN(J+1,I,K)
            TOPJ1 = BOTM(J+1,I,LBOTM(K)-1)
            BJ1 = BOTM(J+1,I,LBOTM(K))
            THJ1 = HJ1 - BJ1
            TRJ1 = HK(J+1,I,K) * THJ1
            AJ1 = SN(J+1,I,K)
            NUJ1 = 0
            HSING = HJ1
            IF(TOPJ1.GT.HJ1) CALL SSEN1HUF2HDFND(HUFTHK,NC,NR,NHUF,
     &                                           HSING,I,J+1,NUJ1)
            IF(NUJ1.NE.0) CALL SSEN1HUF2HKFND(NC,NR,NHUF,HUFTHK,IZON,
     &                                        NZONAR,RMLT,NMLTAR,IOUT,
     &                                        I,J+1,NUJ1,HKJ1,HKCCJ1,
     &                                        BOTM(1,1,LBOTM(K)-1),
     &                                        BOTM(1,1,LBOTM(K)),
     &                                        GS)
          ENDIF
C-------MATRIX DERIVATIVES
          COCR = ZERO
          COCC = ZERO
          IF(J.LT.NC .AND. IBDJ1.GT.0) THEN
            D1CR = ZERO
            D2CR = ZERO
            IF (TOPO.GT.HO)
     &        D1CR = (CR(J,I,K)**2.)*DELR(J)*HKO/
     &               (DELC(I)*2.*TRO**2.)
            IF (TOPJ1.GT.HJ1)
     &        D2CR = (CR(J,I,K)**2.)*DELR(J+1)*HKJ1/
     &               (DELC(I)*2.*TRJ1**2.)
            COCR = D1CR*AO + D2CR*AJ1
            RHS(J,I,K) = RHS(J,I,K) - COCR*(HJ1-HO)
            RHS(J+1,I,K) = RHS(J+1,I,K) - COCR*(HO-HJ1)
          ENDIF
          IF(I.LT.NR .AND. IBDI1.GT.0) THEN
            D1CC = ZERO
            D2CC = ZERO
            IF (TOPO.GT.HO)
     &          D1CC = (CC(J,I,K)**2.)*DELC(I)*HKCCO/
     &                 (2.*DELR(J)*TCO**2.)
            IF (TOPI1.GT.HI1)
     &          D2CC = (CC(J,I,K)**2.)*DELC(I+1)*HKCCI1/
     &                 (2.*DELR(J)*TCI1**2.)
            COCC = D1CC*AO + D2CC*AI1
            RHS(J,I,K) = RHS(J,I,K) - COCC*(HI1-HO)
            RHS(J,I+1,K) = RHS(J,I+1,K) - COCC*(HO-HI1)
          ENDIF
   10   CONTINUE
   20 CONTINUE
C
C      CV
C
      IF (NL.GT.1.AND.K.LT.NL) THEN
        DO 60 J = 1, NC
          DO 50 I = 1, NR
            IF (IBOUND(J,I,K).EQ.0 .OR. IBOUND(J,I,K+1).EQ.0) GOTO 50
            HO = HN(J,I,K)
            HP = HN(J,I,K+1)
            TOP1 = ZERO
            IF (LT.NE.0) TOP1 = BOTM(J,I,LBOTM(K)-1)
            IF (TOP1.LT.HO) GOTO 50
            AO = SN(J,I,K)
            BO = BOTM(J,I,LBOTM(K))
            MID = 0.5*(HO + BO)
            BOT2=BOTM(J,I,LBOTM(K)+1)
            MID2=0.5*(BO+BOT2)
C-------FIRST, FIND UNIT THAT LAYER MID POINT IS IN
            CALL SSEN1HUF2HDFND(HUFTHK,NC,NR,NHUF,MID,I,J,NNU)
C-------UNIT NOT FOUND, SKIP CALCULATIONS
            IF(NNU.EQ.0) GOTO 50
   80       CONTINUE
C-------NOW FIND THE KV OF THE UNIT
            CALL UHUF2THK(MID,MID2,HUFTHK(J,I,NNU,1),
     &                        HUFTHK(J,I,NNU,2),THK1,D1,D2)
            HUFTMP(NNU)=0.0
            CALL UHUF2POP(HUFTMP,'VK  ',NC,NR,NHUF,I,J,HUFTHK,
     &                        IZON,NZONAR,RMLT,NMLTAR,NNU,IOUT)
            CALL UHUF2POP(HUFTMP,'VANI',NC,NR,NHUF,I,J,HUFTHK,
     &                        IZON,NZONAR,RMLT,NMLTAR,NNU,IOUT)
            IF(HGUVANI(NNU).EQ.0.0) THEN
              VK1=HUFTMP(NNU)
            ELSE
              IF(HUFTMP(NNU).EQ.0.0) THEN
                VK1 = HGUVANI(NNU)
              ELSE
                VK1 = HUFTMP(NNU)
              ENDIF
              CALL SSEN1HUF2PGET(HKU,HANIU,LAMBDA,NC,NR,NHUF,I,J,
     &                           HUFTHK,IZON,NZONAR,RMLT,NMLTAR,NNU,
     &                           IOUT)
              MULTKDEP = 1.0
              IF(LAMBDA.NE.0.) CALL SGWF1HUF2KDEP(LAMBDA,D1,D2,
     &                                            GS(J,I),MULTKDEP)
              VK1=HKU*MULTKDEP/VK1
            ENDIF
C-------MATRIX DERIVATIVES
            D1CV=0.
            IF (TOP1.GT.HO)
     &          D1CV = -(CV(J,I,K)**2.)/(DELC(I)*2.*DELR(J)*VK1)
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
c======================================================================
      SUBROUTINE SEN1HUF2SC2(TOP,BOT,TOPU,BOTU,HN,HO,CRHS,CO)
C
C     ******************************************************************
C     Compute contributions to RHS for sensitivities for convertible cell
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL TOPU, BOTU, TOP, BOT, CRHS
C
      CRHS = 0.0

      IF(TOPU.GT.TOP) TOPU=TOP
      IF(BOTU.LT.BOT) BOTU=BOT
C-----Compute contributions for this unit to flow in layer
      IF(HO.GE.TOP) THEN
C-------Layer converts, water table is coming down
        IF(HN.LE.TOPU.AND.HN.GE.BOTU) THEN
C---------New head is in this unit
          CRHS=CRHS+CO*(TOPU-HN)
        ELSEIF(HN.LE.BOTU) THEN
C---------New head is below this unit
          CRHS=CRHS+CO*(TOPU-BOTU)
        ENDIF
      ELSEIF(HN.GE.TOP) THEN
C-------Layer converts, water table is going up
        IF(HO.LE.TOPU.AND.HO.GE.BOTU) THEN
C---------Old head is in this unit
          CRHS=CRHS+CO*(HO-TOPU)
        ELSEIF(HO.LE.BOTU) THEN
C---------Old head is below this unit
          CRHS=CRHS+CO*(BOTU-TOPU)
        ENDIF
      ELSEIF(HO.LE.TOP.AND.HN.LE.TOP) THEN
C-------Layer does not convert, just use SC2
        IF(HO.GE.HN) THEN
C---------Water table is coming down
          IF(HO.LE.TOPU.AND.HO.GE.BOTU .AND.
     &       HN.LE.TOPU.AND.HN.GE.BOTU) THEN
C------------Old and new heads are both in this unit
            CRHS=CRHS+CO*(HO-HN)
          ELSEIF(HO.LE.TOPU.AND.HO.GE.BOTU) THEN
C-----------Old head is in this unit
            CRHS=CRHS+CO*(HO-BOTU)
          ELSEIF(HN.LE.TOPU.AND.HN.GE.BOTU) THEN
C-----------New head is in this unit
            CRHS=CRHS+CO*(TOPU-HN)
          ELSEIF(HO.GE.TOPU.AND.HN.LE.BOTU) THEN
C-----------Old head is above and new head is below this unit
            CRHS=CRHS+CO*(TOPU-BOTU)
          ENDIF
        ELSE
C---------Water table is going up
          IF(HO.LE.TOPU.AND.HO.GE.BOTU .AND.
     &       HN.LE.TOPU.AND.HN.GE.BOTU) THEN
C-----------Old and new heads are both in this unit
            CRHS=CRHS+CO*(HO-HN)
          ELSEIF(HO.LE.TOPU.AND.HO.GE.BOTU) THEN
C-----------Old head is in this unit
            CRHS=CRHS+CO*(HO-TOPU)
          ELSEIF(HN.LE.TOPU.AND.HN.GE.BOTU) THEN
C-----------New head is in this unit
            CRHS=CRHS+CO*(BOTU-HN)
          ELSEIF(HO.LE.BOTU.AND.HN.GE.TOPU) THEN
C-----------Old head is below and new head is above this unit
            CRHS=CRHS+CO*(BOTU-TOPU)
          ENDIF
        ENDIF
      ENDIF
C
C4------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SSEN1HUF2HKFND(NCOL,NROW,NHUF,HUFTHK,IZON,NZONAR,
     &  RMLT,NMLTAR,IOUT,I,J,NU,HKCR,HKCC,TOP,BOT,GS)
C
C     ******************************************************************
C     Find the horizontal hydraulic conductivity of a hydrogeologic unit.
C     accounting for KDEP parameters for intervals within model layers
C     (not between grid nodes for CV)
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL LAMBDA, MULTKDEP
      DIMENSION RMLT(NCOL,NROW,NMLTAR),
     &          IZON(NCOL,NROW,NZONAR), HUFTMP(999),
     &          HUFTHK(NCOL,NROW,NHUF,2), TOP(NCOL,NROW),
     &          BOT(NCOL,NROW), GS(NCOL,NROW)
      COMMON /HUFCOM/HGUHANI(999),HGUVANI(999),LTHUF(999),LAYWT(999)
C
      HUFTMP(NU) = 0.0
      CALL UHUF2POP(HUFTMP,'KDEP',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                  IZON,NZONAR,RMLT,NMLTAR,NU,IOUT)
      LAMBDA = HUFTMP(NU)
      CALL SSEN1HUF2KDEPTH(D1,D2,GS(J,I),
     &                     HUFTHK(J,I,NU,1),
     &                     HUFTHK(J,I,NU,2),
     &                     TOP(J,I),BOT(J,I))
      MULTKDEP=1.0
      IF(LAMBDA.NE.0.) CALL SGWF1HUF2KDEP(LAMBDA,GS(J,I)-D1,GS(J,I)-D2,
     &                                   GS(J,I),MULTKDEP)
      HUFTMP(NU)=0.0
      CALL UHUF2POP(HUFTMP,'HK  ',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                  IZON,NZONAR,RMLT,NMLTAR,NU,IOUT)
      HKCR = MULTKDEP * HUFTMP(NU)
      HUFTMP(NU)=0.0
      CALL UHUF2POP(HUFTMP,'HANI',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                  IZON,NZONAR,RMLT,NMLTAR,NU,IOUT)
      IF(HGUHANI(NU).GT.0..AND.HUFTMP(NU).EQ.0.) THEN
        HANI = HGUHANI(NU)
      ELSE
        HANI = HUFTMP(NU)
      ENDIF
      HKCC = HKCR * HANI
C
C4------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SSEN1HUF2HDFND(HUFTHK,NCOL,NROW,NHUF,HD,I,J,NNU)
C
C     ******************************************************************
C     Find the hydrogeologic unit that an elevation is in.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL HUFTHK(NCOL,NROW,NHUF,2)
C
C-------FIRST, FIND UNIT THAT HEAD IS IN
      NNU = 0
      DO 70 NU = 1,NHUF
        TOPU=HUFTHK(J,I,NU,1)
        THCKU=HUFTHK(J,I,NU,2)
        IF(ABS(THCKU).LT.1E-4) GOTO 70
        BOTU=TOPU-THCKU
        IF(BOTU.LT.HD .AND. HD.LT.TOPU) THEN
          NNU = NU
          GOTO 80
        ENDIF
   70 CONTINUE
C
C4------RETURN
   80 RETURN
      END
C=======================================================================
      SUBROUTINE SSEN1HUF2PGET(
     &  HKU,HANIU,LMBDU,NCOL,NROW,NHUF,I,J,
     &  HUFTHK,IZON,NZONAR,RMLT,NMLTAR,NU,IOUT)
C     ******************************************************************
C     GET HK, HANI, AND KDEP VALUES FOR A GIVEN I,J AND UNIT
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL LMBDU
      DIMENSION RMLT(NCOL,NROW,NMLTAR),IZON(NCOL,NROW,NZONAR),
     &  HUFTMP(999),HUFTHK(NCOL,NROW,NHUF,2)
      COMMON /HUFCOM/HGUHANI(999),HGUVANI(999),LTHUF(999),LAYWT(999)
C     ------------------------------------------------------------------
      HUFTMP(NU) = 0.0
      CALL UHUF2POP(HUFTMP,'HK  ',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                    IZON,NZONAR,RMLT,NMLTAR,NU,IOUT)
      HKU = HUFTMP(NU)
      HUFTMP(NU) = 0.0
      CALL UHUF2POP(HUFTMP,'HANI',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                    IZON,NZONAR,RMLT,NMLTAR,NU,IOUT)
      IF(HGUHANI(NU).GT.0..AND.HUFTMP(NU).EQ.0.) THEN
        HANIU = HGUHANI(NU)
      ELSE
        HANIU = HUFTMP(NU)
      ENDIF
      HUFTMP(NU) = 0.0
      CALL UHUF2POP(HUFTMP,'KDEP',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                    IZON,NZONAR,RMLT,NMLTAR,NU,IOUT)
      LMBDU = HUFTMP(NU)
      RETURN
      END
C=======================================================================
      SUBROUTINE SSEN1HUF2CH(CO,TH0,TH1,HP,I,J,K,CHAR,RMLT0,
     &                  HK,HKCC,NCOL,NROW,NLAY,DELC,DELR,H,BOT,TOP,NZ,
     &                  NM,ICL,IZON,NZONAR,RMLT,NMLTAR,C,TH0L,TH1L,
     &                  HUFTHK,NHUF,NU,GS)
C     ******************************************************************
C     CALCULATE THE DERIVATIVE OF THE HORIZONTAL CONDUCTANCES WITH
C     RESPECT TO PARAMETER VALUES, FOR HARMONIC MEAN CONDUCTANCES
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL BOT, CO, D0, D1, DC, DELC, DELR, DR, DT0, DT1, DU, DV, FAC,
     &     RMLT, RMLT0, RMLT1, HK, T0, T1, TH0, TH1, TOP, U, V, ZERO,
     &     LMBD0, LMBD1, MULTKDEP0, MULTKDEP1
      INTEGER I, II, IJ, IND, J, K, IZON, NCOL, NLAY, NROW, NZ
      CHARACTER*2 CHAR
      DIMENSION HK(NCOL,NROW,NLAY), DELC(NROW), DELR(NCOL),
     & BOT(NCOL,NROW), TOP(NCOL,NROW),HKCC(NCOL,NROW,NLAY),
     & RMLT(NCOL,NROW,NMLTAR),IZON(NCOL,NROW,NZONAR),
     & HUFTHK(NCOL,NROW,NHUF,2),GS(NCOL,NROW)
      DOUBLE PRECISION H(NCOL*NROW*NLAY), HP, H0
      COMMON /HUFCOM/HGUHANI(999),HGUVANI(999),LTHUF(999),LAYWT(999)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
      ZERO = 0.0
      HP = ZERO
      CO = ZERO
      C = ZERO
      DR = DELR(J)
      DC = DELC(I)
      II = 0
      IJ = 0
      AN0 = 1.0
      AN1 = 1.0
      IND = J + NCOL*(I-1) + NROW*NCOL*(K-1)
      IF (CHAR.EQ.'CR') IJ = 1
      IF (CHAR.EQ.'CC') II = 1
      RMLT1 = 1.
      IF (NZ.GT.0) THEN
        RMLT1=0.
        DO 30 JJ = 5,IPCLST(4,ICL)
          IF(IZON(J+IJ,I+II,NZ).EQ.IPCLST(JJ,ICL)) THEN
            IF(NM.GT.0) THEN
              RMLT1=RMLT(J+IJ,I+II,NM)
            ELSE
              RMLT1=1.
            ENDIF
          END IF
   30   CONTINUE
      ELSEIF(NM.GT.0) THEN
        RMLT1=RMLT(J+IJ,I+II,NM)
      ENDIF
      IF(RMLT0.EQ.0..AND.RMLT1.EQ.0.) RETURN
      CALL SSEN1HUF2PGET(HKU0,HANI0,LMBD0,NCOL,NROW,NHUF,I,J,HUFTHK,
     &                   IZON,NZONAR,RMLT,NMLTAR,NU,IOUT)
      CALL SSEN1HUF2PGET(HKU1,HANI1,LMBD1,NCOL,NROW,NHUF,I+II,J+IJ,
     &                   HUFTHK,IZON,NZONAR,RMLT,NMLTAR,NU,IOUT)
      CALL SSEN1HUF2KDEPTH(D10,D20,GS(J,I),
     &                     HUFTHK(J,I,NU,1),
     &                     HUFTHK(J,I,NU,2),
     &                     TOP(J,I),BOT(J,I))
      CALL SSEN1HUF2KDEPTH(D11,D21,GS(J+IJ,I+II),
     &                     HUFTHK(J+IJ,I+II,NU,1),
     &                     HUFTHK(J+IJ,I+II,NU,2),
     &                     TOP(J+IJ,I+II),BOT(J+IJ,I+II))
      MULTKDEP0=1.0
      IF(LMBD0.NE.0.) CALL SGWF1HUF2KDEP(LMBD0,GS(J,I)-D10,GS(J,I)-D20,
     &                                   GS(J,I),MULTKDEP0)
      MULTKDEP1=1.0
      IF(LMBD1.NE.0.) CALL SGWF1HUF2KDEP(LMBD1,GS(J+IJ,I+II)-D11,
     &                                   GS(J+IJ,I+II)-D21,
     &                                   GS(J+IJ,I+II),MULTKDEP1)
      IF (CHAR.EQ.'CR') THEN
        HP = H(IND+1)
        FAC = 2.*DC
        D0 = DR
        D1 = DELR(J+1)
        HK0=HK(J,I,K)
        HK1=HK(J+IJ,I+II,K)
      ELSEIF (CHAR.EQ.'CC') THEN
        HP = H(IND+NCOL)
        FAC = 2.*DR
        D0 = DC
        D1 = DELC(I+1)
        HK0=HKCC(J,I,K)
        HK1=HKCC(J+IJ,I+II,K)
        AN0=HANI0
        AN1=HANI1
      ENDIF
      H0=H(IND)
      TOP0L=TOP(J,I)
      TH0L=TOP0L-BOT(J,I)
      IF(LTHUF(K).NE.0.AND.H0.LT.TOP0L) TH0L=H0-BOT(J,I)
      TOP1L=TOP(J+IJ,I+II)
      TH1L=TOP1L-BOT(J+IJ,I+II)
      IF(LTHUF(K).NE.0.AND.HP.LT.TOP1L) TH1L=HP-BOT(J+IJ,I+II)
      T0 = HK0*TH0L
      T1 = HK1*TH1L
      DT0 = RMLT0*MULTKDEP0*AN0*TH0
      DT1 = RMLT1*MULTKDEP1*AN1*TH1
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
        CO = FAC*(1./V**2.)*(V*DU-U*DV)
      ENDIF
      RETURN
      END
C=======================================================================
      SUBROUTINE SSEN1HUF2CHN(CO,TH0,TH1,HP,I,J,K,RMLT0,
     &  HKCC,NCOL,NROW,NLAY,DELC,DELR,H,BOT,TOP,GS,NZ,
     &  NM,ICL,IZON,NZONAR,RMLT,NMLTAR,C,TH0L,TH1L,HUFTHK,NHUF,NU)
C     ******************************************************************
C     CALCULATE THE DERIVATIVE OF THE HORIZONTAL CONDUCTANCES WITH
C     RESPECT TO HORIZONTAL ANISOTROPY, FOR HARMONIC MEAN CONDUCTANCES
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL BOT, CO, D0, D1, DC, DELC, DELR, DR, DT0, DT1, DU, DV, FAC,
     &     RMLT, RMLT0, RMLT1, T0, T1, TH0, TH1, TOP, U, V, ZERO
      INTEGER I, IND, J, K, IZON, NCOL, NLAY, NROW, NZ
      DIMENSION DELC(NROW), DELR(NCOL),
     & BOT(NCOL,NROW), TOP(NCOL,NROW),HKCC(NCOL,NROW,NLAY),
     & RMLT(NCOL,NROW,NMLTAR),IZON(NCOL,NROW,NZONAR),
     & HUFTHK(NCOL,NROW,NHUF,2),GS(NCOL,NROW)
      DOUBLE PRECISION H(NCOL*NROW*NLAY), HP, H0
      COMMON /HUFCOM/HGUHANI(999),HGUVANI(999),LTHUF(999),LAYWT(999)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
      ZERO = 0.0
      HP = ZERO
      CO = ZERO
      C = ZERO
      DR = DELR(J)
      DC = DELC(I)
      IND = J + NCOL*(I-1) + NROW*NCOL*(K-1)
      CALL UHUF2RMLT(RMLT1,J,I+1,NZ,NM,ICL,IZON,NZONAR,
     &               RMLT,NMLTAR,NROW,NCOL)
      IF(RMLT0.EQ.0..AND.RMLT1.EQ.0.) RETURN
      H0=H(IND)
      HP = H(IND+NCOL)
      FAC = 2.*DR
      D0 = DC
      D1 = DELC(I+1)
      TOP0L=TOP(J,I)
      TH0L=TOP0L-BOT(J,I)
      IF(LTHUF(K).NE.0.AND.H0.LT.TOP0L) TH0L=H0-BOT(J,I)
      TOP1L=TOP(J,I+1)
      TH1L=TOP1L-BOT(J,I+1)
      IF(LTHUF(K).NE.0.AND.HP.LT.TOP1L) TH1L=HP-BOT(J,I+1)
      T0 = HKCC(J,I,K)*TH0L
      T1 = HKCC(J,I+1,K)*TH1L
C--Get horizontal hydraulic conductivity of this unit
      CALL SSEN1HUF2HKFND(NCOL,NROW,NHUF,HUFTHK,IZON,
     &                    NZONAR,RMLT,NMLTAR,IOUT,
     &                    I,J,NU,HKU0,HKCCUO,TOP,BOT,GS)
      CALL SSEN1HUF2HKFND(NCOL,NROW,NHUF,HUFTHK,IZON,
     &                    NZONAR,RMLT,NMLTAR,IOUT,
     &                    I+1,J,NU,HKU1,HKCCU1,TOP,BOT,GS)
      DT0 = HKU0*RMLT0*TH0
      DT1 = HKU1*RMLT1*TH1
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
        CO = FAC*(1./V**2.)*(V*DU-U*DV)
      ENDIF
      RETURN
      END
C=======================================================================
      SUBROUTINE SSEN1HUF2CV(PID,CO,THK1,IBP,IBM,NCOL,NROW,NLAY,CV,
     &                    DELR,DELC,J,I,K,IBOUND,NU,IZON,
     &                    NZONAR,RMLT0,RMLT,NMLTAR,IOUT,HUFTHK,NHUF,
     &                    D1,D2,GS)
C     ******************************************************************
C     CALCULATE THE DERIVATIVE OF THE VERTICAL CONDUCTANCES WITH
C     RESPECT TO PARAMETER VALUES
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL CO, CV, DELC, DELR, ZERO, GS, LAMBDA, MULTKDEP
      INTEGER I, IBM, IBOUND, IBP, J, K, IZON, NCOL, NLAY, NROW
      CHARACTER*4 PID
      DIMENSION DELC(NROW), DELR(NCOL), RMLT(NCOL,NROW,NMLTAR),
     &          IBOUND(NCOL,NROW,NLAY), CV(NCOL,NROW,NLAY),
     &          IZON(NCOL,NROW,NZONAR), HUFTMP(999),
     &          HUFTHK(NCOL,NROW,NHUF,2)
      COMMON /HUFCOM/HGUHANI(999),HGUVANI(999),LTHUF(999),LAYWT(999)
C     ------------------------------------------------------------------
      ZERO = 0.0
      IBP = 0
      IBM = 0
      CO = ZERO
      IF(K.GT.1) IBM=IBOUND(J,I,K-1)
      IBP=IBOUND(J,I,K+1)
      IF (IBOUND(J,I,K).EQ.0 .OR. IBP.EQ.0) RETURN
      IF(PID.EQ.'VK') THEN
C
C
C---Vertical hydraulic conductivity
C   (This corresponds to Eq. B-24 in HUF Manual)
C-----First, get (additive) KV for this unit
        HUFTMP(NU)=0.0
        CALL UHUF2POP(HUFTMP,'VK  ',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                    IZON,NZONAR,RMLT,NMLTAR,NU,IOUT)
        COD = DELR(J)*DELC(I)*(HUFTMP(NU))**2.
        CO = (CV(J,I,K)**2.)*RMLT0*THK1/COD
      ELSE
C
C
C---Vertical Anisotropy
C   (This corresponds to Eq. B-29c in HUF Manual)
        IF(PID.EQ.'VANI') THEN
C-----First, get (additive) KH for this unit
          CALL SSEN1HUF2PGET(HKU,HANIU,LAMBDA,NCOL,NROW,NHUF,I,J,HUFTHK,
     &                       IZON,NZONAR,RMLT,NMLTAR,NU,IOUT)
          MULTKDEP = 1.0
          IF(LAMBDA.NE.0.) CALL SGWF1HUF2KDEP(LAMBDA,D1,D2,GS,MULTKDEP)
          COD=DELR(J)*DELC(I)*HKU*MULTKDEP
          CO=-(CV(J,I,K)**2.)*THK1*RMLT0/COD
C
C
C---Horizontal hydraulic conductivity
C   (This corresponds to Eq. B-31c in HUF Manual)
        ELSEIF(PID.EQ.'HK') THEN
C-----First, get VANI for this unit
          HUFTMP(NU)=0.0
          CALL UHUF2POP(HUFTMP,'VANI',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                    IZON,NZONAR,RMLT,NMLTAR,NU,IOUT)
          IF(HGUVANI(NU).GT.0..AND.HUFTMP(NU).EQ.0.) THEN
            VANIU = HGUVANI(NU)
          ELSE
            VANIU = HUFTMP(NU)
          ENDIF
          CALL SSEN1HUF2PGET(HKU,HANIU,LAMBDA,NCOL,NROW,NHUF,I,J,HUFTHK,
     &                       IZON,NZONAR,RMLT,NMLTAR,NU,IOUT)
          MULTKDEP = 1.0
          IF(LAMBDA.NE.0.) CALL SGWF1HUF2KDEP(LAMBDA,D1,D2,GS,MULTKDEP)
          COD = DELR(J)*DELC(I)*(HKU*MULTKDEP/VANIU)**2.
          CO = ((CV(J,I,K)**2.)*THK1*RMLT0*MULTKDEP/VANIU)/COD
C
C
C---Depth-decay coefficient
        ELSEIF(PID.EQ.'KDEP') THEN
          HUFTMP(NU)=0.0
          CALL UHUF2POP(HUFTMP,'KDEP',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                    IZON,NZONAR,RMLT,NMLTAR,NU,IOUT)
          LAMBDA = HUFTMP(NU)
          HUFTMP(NU)=0.0
          CALL UHUF2POP(HUFTMP,'VANI',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                    IZON,NZONAR,RMLT,NMLTAR,NU,IOUT)
          IF(HGUVANI(NU).GT.0..AND.HUFTMP(NU).EQ.0.) THEN
            VANIU = HGUVANI(NU)
          ELSE
            VANIU = HUFTMP(NU)
          ENDIF
          HUFTMP(NU)=0.0
          CALL UHUF2POP(HUFTMP,'HK  ',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                    IZON,NZONAR,RMLT,NMLTAR,NU,IOUT)
          HKU = HUFTMP(NU)
          CALL SGWF1HUF2KDEP(LAMBDA,D1,D2,GS,MULTKDEP)
          CALL SSEN1HUF2DMULTKDEP(DMDM,LAMBDA,RMLT0,GS-D2,GS-D1)
          COD = DELR(J)*DELC(I)*((MULTKDEP**2.)*HKU)/VANIU
          CO = ((CV(J,I,K)**2.)*THK1*DMDM)/COD
        ENDIF
      ENDIF

      RETURN
      END
C=======================================================================
      SUBROUTINE SSEN1HUF2CO(CO,IVSWTCH,I,J,K,IIPP,H,NCOL,NROW,NLAY,
     &                    PID,HK,HKCC,DELR,DELC,IBOUND,CV,BOTM,NBOTM,
     &                    HUFTHK,NHUF,IZON,NZONAR,RMLT,NMLTAR,
     &                    IUHFB,HFB,MXACTFB,NHFB,IOUT,GS)
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C     SUBROUTINE TO COMPUTE SENSITIVITIES OF THE SIX CELL-FACE
C     CONDUCTANCES FOR THE HUF PACKAGE
C     IF IVSWTCH <> 0, ONLY CALCULATE VERTICAL CONDUCTANCE SENSITIVITIES
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C        SPECIFICATIONS:
C----------------------------------------------------------------------
C---ARGUMENTS:
      REAL DELC, DELR, RMLT, BOTM, HFB, HK, CV, CO
      INTEGER I, J, K, IIPP, IBOUND, IZON, NCOL, NHUF,
     &        NROW, NLAY, IUHFB, MXACTFB, NHFB, NMLTAR, NZONAR
      DOUBLE PRECISION H(NCOL,NROW,NLAY),HP
      DIMENSION IBOUND(NCOL,NROW,NLAY), DELR(NCOL), DELC(NROW),
     &          RMLT(NCOL,NROW,NMLTAR),
     &          HK(NCOL,NROW,NLAY), HKCC(NCOL,NROW,NLAY),
     &          CO(6), IZON(NCOL,NROW,NZONAR),
     &          CV(NCOL,NROW,NLAY),BOTM(NCOL,NROW,0:NBOTM),
     &          HFB(7,MXACTFB), HUFTHK(NCOL,NROW,NHUF,2),
     &          GS(NCOL,NROW)
      CHARACTER*4 PID
C---LOCAL:
C      DOUBLE PRECISION
      REAL RMLT0, C, TOP1, TOP2, BOT1, BOT2, MID1,
     &     MID2
      INTEGER ICL, LT, JCNT, JJ, NZ, ICNT, II, KK
C---COMMON:
      COMMON /DISCOM/LBOTM(999),LAYCBD(999)
      COMMON /HUFCOM/HGUHANI(999),HGUVANI(999),LTHUF(999),LAYWT(999)
      INCLUDE 'param.inc'
C----------------------------------------------------------------------
      EXTERNAL SSEN1HUF2CH, SSEN1HUF2CV, SSEN1HFB6MD
      INTRINSIC IABS
C----------------------------------------------------------------------
C---FORMAT:
C----------------------------------------------------------------------
C
      ZERO = 0.0
      ONE = 1.0
c
      DO 100 ICL = IPLOC(1,IIPP),IPLOC(2,IIPP)
        NU = IPCLST(1,ICL)
        NZ = IPCLST(3,ICL)
        NM = IPCLST(2,ICL)
        LT = LTHUF(K)
        IF (IVSWTCH.EQ.0) THEN
          IF (PID.EQ.'HK  ' .OR. PID.EQ.'HANI' .OR.
     &        PID.EQ.'KDEP') THEN
C-------CR--------------------------------------------------------------
            JCNT = 0
            DO 200 JJ = J-1, J
              JCNT = JCNT + 1
              IF (JJ.LT.1 .OR. JJ.GE.NCOL)
     &           GOTO 200
              IF (IBOUND(JJ,I,K).EQ.0 .OR.
     &           IBOUND(JJ+1,I,K).EQ.0) GOTO 200
              RMLT0 = ONE
              IF (NZ.GT.0) THEN
                RMLT0=ZERO
                DO 30 JZ = 5,IPCLST(4,ICL)
                  IF(IZON(JJ,I,NZ).EQ.IPCLST(JZ,ICL)) THEN
                    IF(NM.GT.0) THEN
                      RMLT0=RMLT(JJ,I,NM)
                    ELSE
                      RMLT0=ONE
                    ENDIF
                  END IF
   30           CONTINUE
              ELSEIF(NM.GT.0) THEN
                RMLT0=RMLT(JJ,I,NM)
              ENDIF

C
C Get thicknesses of hydrogeologic unit in this cell and adjacent cells
              TOP0=BOTM(JJ,I,LBOTM(K)-1)
              IF(LT.NE.0.AND.H(JJ,I,K).LT.TOP0) TOP0=H(JJ,I,K)
              BOT0=BOTM(JJ,I,LBOTM(K))
              CALL UHUF2THK(TOP0,BOT0,
     1                        HUFTHK(JJ,I,NU,1),
     2                        HUFTHK(JJ,I,NU,2),TH0,ATPU,ABTU)
              TOP1C=BOTM(JJ+1,I,LBOTM(K)-1)
              IF(LT.NE.0.AND.H(JJ+1,I,K).LT.TOP1C)
     &           TOP1C=H(JJ+1,I,K)
              BOT1C=BOTM(JJ+1,I,LBOTM(K))
              CALL UHUF2THK(TOP1C,BOT1C,
     1                        HUFTHK(JJ+1,I,NU,1),
     2                        HUFTHK(JJ+1,I,NU,2),TH1C,ATPU,ABTU)
              IF(TH0.EQ.0..AND.TH1C.EQ.0.) GOTO 200
              IF(PID.EQ.'HK') THEN
                CALL SSEN1HUF2CH(COR,TH0,TH1C,HP,I,JJ,K,'CR',
     &                         RMLT0,HK,HKCC,NCOL,NROW,NLAY,DELC,DELR,
     &                         H,BOTM(1,1,LBOTM(K)),
     &                         BOTM(1,1,LBOTM(K)-1),NZ,NM,ICL,
     &                         IZON,NZONAR,RMLT,NMLTAR,C,TH0L,TH1L,
     &                         HUFTHK,NHUF,NU,GS)
              ELSEIF(PID.EQ.'KDEP') THEN
                CALL SSEN1HUF2CKDEP(COR,TH0,TH1C,HP,I,JJ,K,'CR',
     &                            RMLT0,HK,HKCC,NCOL,NROW,NLAY,DELC,
     &                            DELR,H,BOTM(1,1,LBOTM(K)),
     &                            BOTM(1,1,LBOTM(K)-1),GS,
     &                            NZ,NM,ICL,IZON,NZONAR,RMLT,NMLTAR,C,
     &                            TH0L,TH1L,HUFTHK,NHUF,NU)
              ENDIF
              IF (IUHFB.GT.0 .AND. COR.NE.0.)
     &          CALL SSEN1HFB6MD(C,'CR',COR,DELC,DELR,HFB,I,JJ,
     &                         K,MXACTFB,NCOL,NHFB,NROW,
     &                         TH0L,TH1L)
              CO(JCNT)=CO(JCNT)+COR
  200       CONTINUE
C-------CC--------------------------------------------------------------
            ICNT = 2
            DO 300 II = I-1, I
              ICNT = ICNT + 1
              IF (II.LT.1 .OR. II.GE.NROW .OR.
     &            IBOUND(J,II+1,K).EQ.0) GOTO 300
              CALL UHUF2RMLT(RMLT0,J,II,NZ,NM,ICL,IZON,NZONAR,
     &                       RMLT,NMLTAR,NROW,NCOL)

C
C Get thicknesses of hydrogeologic unit in this cell and adjacent cells
              TOP0=BOTM(J,II,LBOTM(K)-1)
              IF(LT.NE.0.AND.H(J,II,K).LT.TOP0) TOP0=H(J,II,K)
              BOT0=BOTM(J,II,LBOTM(K))
              CALL UHUF2THK(TOP0,BOT0,
     &                        HUFTHK(J,II,NU,1),
     &                        HUFTHK(J,II,NU,2),TH0,ATPU,ABTU)
              TOP1R=BOTM(J,II+1,LBOTM(K)-1)
              IF(LT.NE.0.AND.H(J,II+1,K).LT.TOP1R)
     &           TOP1R=H(J,II+1,K)
              BOT1R=BOTM(J,II+1,LBOTM(K))
              CALL UHUF2THK(TOP1R,BOT1R,
     &                        HUFTHK(J,II+1,NU,1),
     &                        HUFTHK(J,II+1,NU,2),TH1R,ATPU,ABTU)
              IF(TH0.EQ.0..AND.TH1R.EQ.0.) GOTO 300
              IF (PID.EQ.'HK  ') THEN
                CALL SSEN1HUF2CH(COC,TH0,TH1R,HP,II,J,K,'CC',
     &                         RMLT0,HK,HKCC,NCOL,NROW,NLAY,DELC,DELR,
     &                         H,BOTM(1,1,LBOTM(K)),
     &                         BOTM(1,1,LBOTM(K)-1),NZ,NM,ICL,
     &                         IZON,NZONAR,RMLT,NMLTAR,C,TH0L,TH1L,
     &                         HUFTHK,NHUF,NU,GS)
              ELSEIF(PID.EQ.'HANI') THEN
                CALL SSEN1HUF2CHN(COC,TH0,TH1R,HP,II,J,K,RMLT0,
     &                          HKCC,NCOL,NROW,NLAY,DELC,DELR,H,
     &                          BOTM(1,1,LBOTM(K)),BOTM(1,1,LBOTM(K)-1),
     &                          GS,NZ,NM,ICL,IZON,NZONAR,RMLT,NMLTAR,C,
     &                          TH0L,TH1L,HUFTHK,NHUF,NU)
              ELSEIF(PID.EQ.'KDEP') THEN
                CALL SSEN1HUF2CKDEP(COC,TH0,TH1R,HP,II,J,K,'CC',
     &                              RMLT0,HK,HKCC,NCOL,NROW,NLAY,DELC,
     &                              DELR,H,BOTM(1,1,LBOTM(K)),
     &                              BOTM(1,1,LBOTM(K)-1),GS,
     &                              NZ,NM,ICL,IZON,NZONAR,RMLT,NMLTAR,C,
     &                              TH0L,TH1L,HUFTHK,NHUF,NU)
              ENDIF
              IF (IUHFB.GT.0 .AND. COC.NE.0.)
     &          CALL SSEN1HFB6MD(C,'CC',COC,DELC,DELR,HFB,II,J,
     &                           K,MXACTFB,NCOL,NHFB,NROW,
     &                           TH0L,TH1L)
              CO(ICNT)=CO(ICNT)+COC
  300       CONTINUE
          ENDIF
        ENDIF
C-------CV--------------------------------------------------------------
        IF (PID.EQ.'VK  ' .OR. PID.EQ.'VANI' .OR.
     &     (PID.EQ.'HK  ' .AND. HGUVANI(NU).NE.0) .OR.
     &     (PID.EQ.'KDEP'.AND.HGUVANI(NU).NE.0.0)) THEN
          KCNT=4
          CALL UHUF2RMLT(RMLT0,J,I,NZ,NM,ICL,IZON,NZONAR,
     &                   RMLT,NMLTAR,NROW,NCOL)
          IF(RMLT0.EQ.0.) GOTO 100
          DO 400 KK = K-1, K
            KCNT = KCNT + 1
            IF (KK.LT.1 .OR. KK.EQ.NLAY) GOTO 400
            TOP1=BOTM(J,I,LBOTM(KK)-1)
            IF(LTHUF(KK).NE.0.AND.H(J,I,KK).LT.TOP1)
     &         TOP1=H(J,I,KK)
            BOT1=BOTM(J,I,LBOTM(KK))
            TOP2=BOT1
            IF(LTHUF(KK+1).NE.0.AND.H(J,I,KK+1).LT.TOP2)
     &          TOP2=H(J,I,KK+1)
            BOT2=BOTM(J,I,LBOTM(KK)+1)
            MID1=0.5*(TOP1+BOT1)
            MID2=0.5*(TOP2+BOT2)
            CALL UHUF2THK(MID1,MID2,HUFTHK(J,I,NU,1),
     1            HUFTHK(J,I,NU,2),THK1,D1,D2)
            IF(THK1.EQ.0.) GOTO 400
            CALL SSEN1HUF2CV(PID,COV,THK1,IBP,IBM,NCOL,NROW,
     &                       NLAY,CV,DELR,DELC,J,I,KK,IBOUND,
     &                       NU,IZON,NZONAR,RMLT0,RMLT,NMLTAR,IOUT,
     &                       HUFTHK,NHUF,D1,D2,GS(J,I))
            CO(KCNT)=CO(KCNT)+COV
  400     CONTINUE
        ENDIF
  100 CONTINUE
      RETURN
      END
C=======================================================================
      SUBROUTINE SSEN1HUF2CKDEP(CO,TH0,TH1,HP,I,J,K,CHAR,RMLT0,
     &  HK,HKCC,NCOL,NROW,NLAY,DELC,DELR,H,BOT,TOP,GS,
     &  NZ,NM,ICL,IZON,NZONAR,RMLT,NMLTAR,C,TH0L,TH1L,
     &  HUFTHK,NHUF,NU)
C     ******************************************************************
C     CALCULATE THE DERIVATIVE OF THE HORIZONTAL CONDUCTANCES WITH
C     RESPECT TO KDEP PARAMETERS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL BOT, CO, D0, D1, DC, DELC, DELR, DR, DT0, DT1, DU, DV, FAC,
     &     RMLT, RMLT0, RMLT1, HK, T0, T1, TH0, TH1, TOP, U, V, ZERO,
     &     LMBD0, LMBD1
      INTEGER I, II, IJ, IND, J, K, IZON, NCOL, NLAY, NROW, NZ
      CHARACTER*2 CHAR
      DIMENSION HK(NCOL,NROW,NLAY), DELC(NROW), DELR(NCOL),
     & BOT(NCOL,NROW), TOP(NCOL,NROW),HKCC(NCOL,NROW,NLAY),
     & RMLT(NCOL,NROW,NMLTAR),IZON(NCOL,NROW,NZONAR),
     & HUFTHK(NCOL,NROW,NHUF,2), GS(NCOL,NROW)
      DOUBLE PRECISION H(NCOL*NROW*NLAY), HP, H0
      COMMON /HUFCOM/HGUHANI(999),HGUVANI(999),LTHUF(999),LAYWT(999)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
      ZERO = 0.0
      HP = ZERO
      CO = ZERO
      C = ZERO
      DR = DELR(J)
      DC = DELC(I)
      II = 0
      IJ = 0
      IND = J + NCOL*(I-1) + NROW*NCOL*(K-1)
      IF (CHAR.EQ.'CR') IJ = 1
      IF (CHAR.EQ.'CC') II = 1
      RMLT1 = 1.
      IF (NZ.GT.0) THEN
        RMLT1=0.
        DO 30 JJ = 5,IPCLST(4,ICL)
          IF(IZON(J+IJ,I+II,NZ).EQ.IPCLST(JJ,ICL)) THEN
            IF(NM.GT.0) THEN
              RMLT1=RMLT(J+IJ,I+II,NM)
            ELSE
              RMLT1=1.
            ENDIF
          END IF
   30   CONTINUE
      ELSEIF(NM.GT.0) THEN
        RMLT1=RMLT(J+IJ,I+II,NM)
      ENDIF
      IF(RMLT0.EQ.0..AND.RMLT1.EQ.0.) RETURN
      CALL SSEN1HUF2PGET(HKU0,HANI0,LMBD0,NCOL,NROW,NHUF,I,J,HUFTHK,
     &                   IZON,NZONAR,RMLT,NMLTAR,NU,IOUT)
      CALL SSEN1HUF2PGET(HKU1,HANI1,LMBD1,NCOL,NROW,NHUF,I+II,J+IJ,
     &                   HUFTHK,IZON,NZONAR,RMLT,NMLTAR,NU,IOUT)
      IF (CHAR.EQ.'CR') THEN
        HP = H(IND+1)
        FAC = 2.*DC
        D0 = DR
        D1 = DELR(J+1)
        HK0=HK(J,I,K)
        HK1=HK(J+IJ,I+II,K)
        HANI0 = 1.0
        HANI1 = 1.0
      ELSEIF (CHAR.EQ.'CC') THEN
        HP = H(IND+NCOL)
        FAC = 2.*DR
        D0 = DC
        D1 = DELC(I+1)
        HK0=HKCC(J,I,K)
        HK1=HKCC(J+IJ,I+II,K)
      ENDIF
      H0=H(IND)
      TOP0L=TOP(J,I)
      TH0L=TOP0L-BOT(J,I)
      IF(LTHUF(K).NE.0.AND.H0.LT.TOP0L) TH0L=H0-BOT(J,I)
      TOP1L=TOP(J+IJ,I+II)
      TH1L=TOP1L-BOT(J+IJ,I+II)
      IF(LTHUF(K).NE.0.AND.HP.LT.TOP1L) TH1L=HP-BOT(J+IJ,I+II)
      T0 = HK0*TH0L
      T1 = HK1*TH1L
C-----THIS IS WHAT IS MODIFIED TO INCLUDE dC/dKDEP
      CALL SSEN1HUF2KDEPTH(D10,D20,GS(J,I),
     &                     HUFTHK(J,I,NU,1),
     &                     HUFTHK(J,I,NU,2),
     &                     TOP(J,I),BOT(J,I))
      CALL SSEN1HUF2KDEPTH(D11,D21,GS(J+IJ,I+II),
     &                     HUFTHK(J+IJ,I+II,NU,1),
     &                     HUFTHK(J+IJ,I+II,NU,2),
     &                     TOP(J+IJ,I+II),BOT(J+IJ,I+II))
      CALL SSEN1HUF2DMULTKDEP(DMDM0,LMBD0,RMLT0,D20,D10)
      CALL SSEN1HUF2DMULTKDEP(DMDM1,LMBD1,RMLT1,D21,D11)
      DT0 = DMDM0*HKU0*HANI0*TH0
      DT1 = DMDM1*HKU1*HANI1*TH1
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
        CO = FAC*(1./V**2.)*(V*DU-U*DV)
      ENDIF
      RETURN
      END
C=======================================================================
      SUBROUTINE SSEN1HUF2KDEPTH(D1,D2,GS,TOPU,THCKU,TOPL,BOTL)
C     ******************************************************************
C     DETERMINE THE DEPTHS THAT APPLY WITHIN AN INTERVAL
C     ******************************************************************
C     ------------------------------------------------------------------
      TOP = TOPU
      IF(TOPU.GT.TOPL) TOP = TOPL
      D1 = GS - TOP
      BOT = TOPU - THCKU
      IF(BOT.LT.BOTL) BOT = BOTL
      D2 = GS - BOT
      RETURN
      END
C=======================================================================
      SUBROUTINE SSEN1HUF2DMULTKDEP(DMDM,LMBD,RMLT,D2,D1)
C     ******************************************************************
C     CALCULATE THE DERIVATIVE OF THE DEPTH DECAY FUNCTION
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL LMBD
C     ------------------------------------------------------------------
      P1 = (10.**(-LMBD*D1))
      P2 = (10.**(-LMBD*D2))
      U = P2 - P1
      DU = LOG(10.0)*RMLT*(D1*P1-D2*P2)
      V = -LMBD*(D2-D1)*LOG(10.0)
      DV = -RMLT*(D2-D1)*LOG(10.0)
      DMDM = 0.0
      IF(ABS(V).GT.1E-24) THEN
        DMDM = (1./V**2.)*(V*DU-U*DV)
      ENDIF

      RETURN
      END
c======================================================================
      SUBROUTINE SEN1HUF2VDFM(
     &  HNEW,SNEW,IBOUND,HK,HKCC,CR,CC,CV,VDHT,VDHD,DVDH,RHS,
     &  NCOL,NROW,NLAY,DELR,DELC,HUFTHK,NHUF,BOTM,NBOTM,IP,PID,
     &  IZON,NZONAR,RMLT,NMLTAR,GS)
C
C     ******************************************************************
C     Calculate sensitivity of stiffness matrix and RHS for LVDA
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION HNEW,SNEW,DFL5,DFR5,DFT5,DFB5,DTF5,DFL9,DFR9,
     &  DFT9,DFB9,DTF9
      CHARACTER*4 PID
      DIMENSION RHS(NCOL,NROW,NLAY),
     &  VDHT(NCOL,NROW,NLAY,3),DELR(NCOL),DELC(NROW),
     &  HK(NCOL,NROW,NLAY),HKCC(NCOL,NROW,NLAY),CV(NCOL,NROW,NLAY),
     &  IBOUND(NCOL,NROW,NLAY), HNEW(NCOL,NROW,NLAY),
     &  CR(NCOL,NROW,NLAY),CC(NCOL,NROW,NLAY),VDHD(NCOL,NROW,NLAY),
     &  HUFTHK(NCOL,NROW,NHUF,2),BOTM(NCOL,NROW,0:NBOTM),
     &  DVDH(NCOL,NROW,NLAY,3),SNEW(NCOL,NROW,NLAY),
     &  RMLT(NCOL,NROW,NMLTAR),IZON(NCOL,NROW,NZONAR),GS(NCOL,NROW)
C-----------------------------------------------------------------------
C
      ZERO = 0.0
      TWO = 2.0
      DB = 0.1
      CALL SSEN1HUF2DA(IBOUND,HK,HKCC,VDHD,VDHT,DVDH,DB,NCOL,NROW,NLAY,
     &                 HUFTHK,NHUF,BOTM,NBOTM,IP,DELR,DELC,IZON,NZONAR,
     &                 RMLT,NMLTAR,PID,HNEW,GS)
C-------Loop through all cells
      DO 600 K=1,NLAY
        DO 610 I=1,NROW
          DO 620 J=1,NCOL
C-------Skip calculation if cell is inactive
            IF(IBOUND(J,I,K).EQ.0) THEN
              CR(J,I,K)=ZERO
              CC(J,I,K)=ZERO
            ELSE
C-------Grab transmissivities
C---------Local cell 1, I,J
              TAA1 = VDHT(J,I,K,1)
              TBB1 = VDHT(J,I,K,3)
C-------Calculate CR equivalent
              IF(J.NE.NCOL) THEN
                IF(IBOUND(J+1,I,K).NE.0) THEN
                  TAA2 = VDHT(J+1,I,K,1)
                  CR(J,I,K)=TWO*TAA2*TAA1/(TAA1+TAA2)
                ELSE
                  CR(J,I,K)=ZERO
                END IF
              ELSE
                CR(J,I,K)=ZERO
              END IF
C
C-------and Calculate CC equivalent
              IF(I.NE.NROW) THEN
                IF(IBOUND(J,I+1,K).NE.0) THEN
                  TBB4 = VDHT(J,I+1,K,3)
                  CC(J,I,K)=TWO*TBB4*TBB1/(TBB1+TBB4)
                ELSE
                  CC(J,I,K)=ZERO
                END IF
              ELSE
                CC(J,I,K)=ZERO
              END IF
C
C-------and subtract elements from RHS
              CALL SSEN1HUF2VDF9(I,J,K,VDHT,DVDH,DB,HNEW,SNEW,IBOUND,
     &                           NLAY,NROW,NCOL,
     &                           DFL9,DFR9,DFT9,DFB9,PID)
              DTF9 = DFL9 - DFR9 + DFT9 - DFB9
              CALL SSEN1HUF2VDF5(I,J,K,VDHT,DVDH,DB,HNEW,SNEW,NLAY,
     &                           NROW,NCOL,CR,CC,DFL5,DFR5,DFT5,DFB5,
     &                           PID,DABH,IP,DELR,DELC,IBOUND,
     &                           CV,BOTM,NBOTM,HUFTHK,NHUF,IZON,NZONAR,
     &                           RMLT,NMLTAR,IOUT,GS)

              DTF5 = DFL5 + DFR5 + DFT5 + DFB5
              RHS(J,I,K) = RHS(J,I,K) - (DTF9-DTF5) - DABH
            END IF
  620     CONTINUE
  610   CONTINUE
  600 CONTINUE
C
C
C-------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SSEN1HUF2DA(
     1  IBOUND,HK,HKCC,VDHD,VDHT,DVDH,DB,NCOL,NROW,NLAY,HUFTHK,NHUF,
     2  BOTM,NBOTM,IP,DELR,DELC,IZON,NZONAR,RMLT,NMLTAR,PID,HNEW,GS)
C
C     ******************************************************************
C     Populate arrays for perturbed and unperturbed parameter values
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION HNEW
      INCLUDE 'param.inc'
      CHARACTER*4 PID
      DIMENSION DVDH(NCOL,NROW,NLAY,3),
     &    VDHT(NCOL,NROW,NLAY,3),VDHD(NCOL,NROW,NLAY),
     &    HK(NCOL,NROW,NLAY),HKCC(NCOL,NROW,NLAY),
     &    IBOUND(NCOL,NROW,NLAY),BOTM(NCOL,NROW,0:NBOTM),
     &    HUFTHK(NCOL,NROW,NHUF,2),DELR(NCOL),DELC(NROW),
     &    RMLT(NCOL,NROW,NMLTAR),IZON(NCOL,NROW,NZONAR),
     &    HNEW(NCOL,NROW,NLAY),GS(NCOL,NROW)
C-----------------------------------------------------------------------
C
C-----Perturb parameter value
      BOLD = B(IP)
      DB = DB * B(IP)
      IF(PID.EQ.'LVDA' .AND. ABS(DB).LT.1E-6) DB = 15.
      B(IP) = B(IP) + DB
C-----Populate HK, HKCC, VDHD arrays
      CALL SGWF1HUF2VDHHV(
     &  IBOUND,HK,HKCC,VDHD,NCOL,NROW,NLAY,HUFTHK,NHUF,BOTM,NBOTM,
     &  IZON,NZONAR,RMLT,NMLTAR,HNEW,GS)

C-----Populate DVDH array
      CALL SGWF1HUF2VDHT(
     &  IBOUND,HK,HKCC,VDHD,DVDH,NCOL,NROW,NLAY,DELR,DELC)
C-----Reset parameter value
      B(IP) = BOLD
C-----Populate HK, HKCC, VDHD arrays
      CALL SGWF1HUF2VDHHV(
     &  IBOUND,HK,HKCC,VDHD,NCOL,NROW,NLAY,HUFTHK,NHUF,BOTM,NBOTM,
     &  IZON,NZONAR,RMLT,NMLTAR,HNEW,GS)

C-----Populate VDHT array
      CALL SGWF1HUF2VDHT(
     &  IBOUND,HK,HKCC,VDHD,VDHT,NCOL,NROW,NLAY,DELR,DELC)
C
C
C-------RETURN
      RETURN
      END
C=======================================================================
      SUBROUTINE SSEN1HUF2VDF5(I,J,K,VDHT,DVDH,DB,HNEW,SNEW,NLAY,NROW,
     &                         NCOL,CR,CC,DFL5,DFR5,DFT5,DFB5,PID,DABH,
     &                         IP,DELR,DELC,IBOUND,CV,BOTM,
     &                         NBOTM,HUFTHK,NHUF,IZON,NZONAR,RMLT,
     &                         NMLTAR,IOUT,GS)
C     ******************************************************************
C     COMPUTE SENSITIVITY OF FLOW THROUGH FOUR CELL FACES FOR 5-POINT
C     STENCIL
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*4 PID
      DOUBLE PRECISION HNEW,SNEW,H1,DH1,DFT5,DFB5,DFL5,DFR5
C
      DIMENSION HNEW(NCOL,NROW,NLAY), IBOUND(NCOL,NROW,NLAY),
     &          VDHT(NCOL,NROW,NLAY,3), CR(NCOL,NROW,NLAY),
     &          CC(NCOL,NROW,NLAY),DVDH(NCOL,NROW,NLAY,3),
     &          SNEW(NCOL,NROW,NLAY), DELR(NCOL), DELC(NROW),
     &          RMLT(NCOL,NROW,NMLTAR), IZON(NCOL,NROW,NZONAR),
     &          CV(NCOL,NROW,NLAY), BOTM(NCOL,NROW,0:NBOTM),
     &          HUFTHK(NCOL,NROW,NHUF,2),GS(NCOL,NROW)
C     ------------------------------------------------------------------
C
      ZERO = 0.0
      H1 = HNEW(J,I,K)
      DH1 = SNEW(J,I,K)
      DABH = ZERO
      IF(I.GT.1) THEN
        CCT = CC(J,I-1,K)
        IF(PID.EQ.'HK  '.OR.PID.EQ.'HANI'.OR.PID.EQ.'LVDA') THEN
          CALL SSEN1HUF2VDDC(I,J,K,I-1,J,3,VDHT,DVDH,DB,DCCT,
     &  NCOL,NROW,NLAY)
        ELSE
          DCCT = ZERO
        ENDIF
        DFT5 = DCCT * (HNEW(J,I-1,K)-H1) + CCT * (SNEW(J,I-1,K)-DH1)
        DABH = DABH + DCCT * (HNEW(J,I-1,K)-H1)
      ELSE
        DFT5=ZERO
      ENDIF
      IF(I.LT.NROW) THEN
        CCB = CC(J,I,K)
        IF(PID.EQ.'HK  '.OR.PID.EQ.'HANI'.OR.PID.EQ.'LVDA') THEN
          CALL SSEN1HUF2VDDC(I,J,K,I+1,J,3,VDHT,DVDH,DB,DCCB,
     &  NCOL,NROW,NLAY)
        ELSE
          DCCB = ZERO
        ENDIF
        DFB5= DCCB * (HNEW(J,I+1,K)-H1) + CCB * (SNEW(J,I+1,K)-DH1)
        DABH = DABH + DCCB * (HNEW(J,I+1,K)-H1)
      ELSE
        DFB5=ZERO
      ENDIF
      IF(J.GT.1) THEN
        CRL = CR(J-1,I,K)
        IF(PID.EQ.'HK  '.OR.PID.EQ.'HANI'.OR.PID.EQ.'LVDA') THEN
          CALL SSEN1HUF2VDDC(I,J,K,I,J-1,1,VDHT,DVDH,DB,DCRL,
     &  NCOL,NROW,NLAY)
        ELSE
          DCRL = ZERO
        ENDIF
        DFL5= DCRL * (HNEW(J-1,I,K)-H1) + CRL * (SNEW(J-1,I,K)-DH1)
        DABH = DABH + DCRL * (HNEW(J-1,I,K)-H1)
      ELSE
        DFL5=ZERO
      ENDIF
      IF(J.LT.NCOL) THEN
        CRR = CR(J,I,K)
        IF(PID.EQ.'HK  '.OR.PID.EQ.'HANI'.OR.PID.EQ.'LVDA') THEN
          CALL SSEN1HUF2VDDC(I,J,K,I,J+1,1,VDHT,DVDH,DB,DCRR,
     &  NCOL,NROW,NLAY)
        ELSE
          DCRR = ZERO
        ENDIF
        DFR5= DCRR * (HNEW(J+1,I,K)-H1) + CRR * (SNEW(J+1,I,K)-DH1)
        DABH = DABH + DCRR * (HNEW(J+1,I,K)-H1)
      ELSE
        DFR5=ZERO
      ENDIF
      IF(NLAY.GT.1) THEN
        DCVT = ZERO
        DCVB = ZERO
        CALL SSEN1HUF2VDDCV(DCVT,DCVB,I,J,K,IP,HNEW,NCOL,NROW,NLAY,
     &                      PID,DELR,DELC,IBOUND,CV,BOTM,NBOTM,
     &                      HUFTHK,NHUF,IZON,NZONAR,RMLT,NMLTAR,IOUT,GS)

        IF(K.GT.1) THEN
          DABH = DABH + DCVT * (HNEW(J,I,K-1)-H1)
        ENDIF
        IF(K.LT.NLAY) THEN
          DABH = DABH + DCVB * (HNEW(J,I,K+1)-H1)
        ENDIF
      ENDIF
C
C-------RETURN
      RETURN
      END
C=======================================================================
      SUBROUTINE SSEN1HUF2VDDCV(COT,COB,I,J,K,IIPP,H,NCOL,NROW,NLAY,
     &                    PID,DELR,DELC,IBOUND,CV,BOTM,NBOTM,
     &                    HUFTHK,NHUF,IZON,NZONAR,RMLT,NMLTAR,IOUT,GS)
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C     SUBROUTINE TO COMPUTE CONDUCTANCE SENSITIVITIES FOR THE LVDA CAPABILITY
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C        SPECIFICATIONS:
C----------------------------------------------------------------------
C---ARGUMENTS:
      DOUBLE PRECISION H
      REAL DELC, DELR, RMLT, BOTM, CV
      INTEGER I, J, K, IIPP, IBOUND, IZON, NCOL, NHUF,
     &        NROW, NLAY, NMLTAR, NZONAR
      DIMENSION IBOUND(NCOL,NROW,NLAY), DELR(NCOL), DELC(NROW),
     &          RMLT(NCOL,NROW,NMLTAR), IZON(NCOL,NROW,NZONAR),
     &          CV(NCOL,NROW,NLAY), BOTM(NCOL,NROW,0:NBOTM),
     &          HUFTHK(NCOL,NROW,NHUF,2),H(NCOL,NROW,NLAY),
     &          GS(NCOL,NROW)
      CHARACTER*4 PID
C---COMMON:
      COMMON /DISCOM/LBOTM(999),LAYCBD(999)
      COMMON /HUFCOM/HGUHANI(999),HGUVANI(999),LTHUF(999),LAYWT(999)
      INCLUDE 'param.inc'
C----------------------------------------------------------------------
C
      DO 100 ICL = IPLOC(1,IIPP),IPLOC(2,IIPP)
        NU = IPCLST(1,ICL)
        NZ = IPCLST(3,ICL)
        NM = IPCLST(2,ICL)
C-------CV--------------------------------------------------------------
        IF (PID.EQ.'VK  ' .OR. PID.EQ.'VANI' .OR.
     &     (PID.EQ.'HK  ' .AND. HGUVANI(NU).NE.0.0)) THEN
          CALL UHUF2RMLT(RMLT0,J,I,NZ,NM,ICL,IZON,NZONAR,
     &                   RMLT,NMLTAR,NROW,NCOL)
          IF(RMLT0.EQ.0.) GOTO 100
          DO 400 KK = K-1, K
            IF (KK.LT.1 .OR. KK.EQ.NLAY) GOTO 400
            TOP1=BOTM(J,I,LBOTM(KK)-1)
            IF(LTHUF(KK).NE.0.AND.H(J,I,KK).LT.TOP1)
     &         TOP1=H(J,I,KK)
            BOT1=BOTM(J,I,LBOTM(KK))
            TOP2=BOT1
            IF(LTHUF(KK+1).NE.0.AND.H(J,I,KK+1).LT.TOP2)
     &          TOP2=H(J,I,KK+1)
            BOT2=BOTM(J,I,LBOTM(KK)+1)
            RMID1=0.5*(TOP1+BOT1)
            RMID2=0.5*(TOP2+BOT2)
            CALL UHUF2THK(RMID1,RMID2,
     1            HUFTHK(J,I,NU,1),HUFTHK(J,I,NU,2),THK1,D1,D2)
            IF(THK1.EQ.0.) GOTO 400
            CALL SSEN1HUF2CV(PID,COV,THK1,IBP,IBM,NCOL,NROW,
     &                       NLAY,CV,DELR,DELC,J,I,KK,IBOUND,
     &                       NU,IZON,NZONAR,RMLT0,RMLT,NMLTAR,IOUT,
     &                       HUFTHK,NHUF,D1,D2,GS(J,I))
            IF(KK.LT.K) THEN
              COT = COT + COV
            ELSE
              COB = COB + COV
            ENDIF
  400     CONTINUE
        ENDIF
  100 CONTINUE
      RETURN
      END
C
C=======================================================================
      SUBROUTINE SSEN1HUF2VDDC(I,J,K,II,JJ,ID,VDHT,DVDH,DB,CO,
     &  NCOL,NROW,NLAY)
C     ******************************************************************
C     COMPUTE SENSITIVITY OF CONDUCTANCE TERM
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION VDHT(NCOL,NROW,NLAY,3), DVDH(NCOL,NROW,NLAY,3)
C     ------------------------------------------------------------------
C
      T0 = VDHT(J,I,K,ID)
      T1 = VDHT(JJ,II,K,ID)
      DT0 = DVDH(J,I,K,ID)
      DT1 = DVDH(JJ,II,K,ID)
      DT0 = (DT0 - T0) / DB
      DT1 = (DT1 - T1) / DB
      U = 2.0 * T0 * T1
      V = T0 + T1
      DU = 2.0 * (T0*DT1 + DT0*T1)
      DV = DT0 + DT1
      CO=0.
      IF(ABS(V).GT.1E-24) THEN
        CO = (1./V**2)*(V*DU-U*DV)
      ENDIF
C
C-------RETURN
      RETURN
      END
C=======================================================================
      SUBROUTINE SSEN1HUF2VDF9(I,J,K,VDHT,DVDH,DB,HNEW,SNEW,IBOUND,NLAY,
     &                         NROW,NCOL,DFL,DFR,DFT,DFB,PID)
C     ******************************************************************
C     COMPUTE SENSITIVITY OF FLOW THROUGH FOUR CELL FACES FOR 9-POINT
C     STENCIL
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*4 PID
      DOUBLE PRECISION HNEW,SNEW,H1,H2,H3,H4,H5,H6,H7,H8,H9,
     &  DH1,DH2,DH3,DH4,DH5,DH6,DH7,DH8,DH9,DFL,DFR,DFT,DFB
      DOUBLE PRECISION
     &             AS11234,AS21234,AS31234,AS41234,
     &             AW11234,AW21234,AW31234,AW41234,
     &             DAS11234,DAS21234,DAS31234,DAS41234,
     &             DAW11234,DAW21234,DAW31234,DAW41234,
     &             AS16145,AS26145,AS36145,AS46145,
     &             AE16145,AE26145,AE36145,AE46145,
     &             DAS16145,DAS26145,DAS36145,DAS46145,
     &             DAE16145,DAE26145,DAE36145,DAE46145,
     &             AN17816,AN27816,AN37816,AN47816,
     &             AE17816,AE27816,AE37816,AE47816,
     &             DAN17816,DAN27816,DAN37816,DAN47816,
     &             DAE17816,DAE27816,DAE37816,DAE47816,
     &             AN18921,AN28921,AN38921,AN48921,
     &             AW18921,AW28921,AW38921,AW48921,
     &             DAN18921,DAN28921,DAN38921,DAN48921,
     &             DAW18921,DAW28921,DAW38921,DAW48921
C
      DIMENSION HNEW(NCOL,NROW,NLAY), IBOUND(NCOL,NROW,NLAY),
     &     VDHT(NCOL,NROW,NLAY,3), DVDH(NCOL,NROW,NLAY,3),
     &     SNEW(NCOL,NROW,NLAY)
C     ------------------------------------------------------------------
C
      ZERO=0.0
C-------Set scaling factor for inactive cells around current cell
      TMPCOF = 1E+8
C
C-------Calculate sensitivities of a coefficients
C-------Quadrant I
      CALL SSEN1HUF2VDA1(
     &             IBOUND,VDHT,DVDH,DB,NCOL,NROW,NLAY,I,J,K,TMPCOF,PID,
     &             AS11234,AS21234,AS31234,AS41234,
     &             AW11234,AW21234,AW31234,AW41234,
     &             DAS11234,DAS21234,DAS31234,DAS41234,
     &             DAW11234,DAW21234,DAW31234,DAW41234,
     &             TAA1,TAB1,TBB1,DTAA1,DTAB1,DTBB1)
C-------Quadrant II
      CALL SSEN1HUF2VDA2(
     &             IBOUND,VDHT,DVDH,DB,NCOL,NROW,NLAY,I,J,K,TMPCOF,PID,
     &             AS16145,AS26145,AS36145,AS46145,
     &             AE16145,AE26145,AE36145,AE46145,
     &             DAS16145,DAS26145,DAS36145,DAS46145,
     &             DAE16145,DAE26145,DAE36145,DAE46145)
C-------Quadrant III
      CALL SSEN1HUF2VDA3(
     &             IBOUND,VDHT,DVDH,DB,NCOL,NROW,NLAY,I,J,K,TMPCOF,PID,
     &             AN17816,AN27816,AN37816,AN47816,
     &             AE17816,AE27816,AE37816,AE47816,
     &             DAN17816,DAN27816,DAN37816,DAN47816,
     &             DAE17816,DAE27816,DAE37816,DAE47816)
C-------Quadrant IV
      CALL SSEN1HUF2VDA4(
     &             IBOUND,VDHT,DVDH,DB,NCOL,NROW,NLAY,I,J,K,TMPCOF,PID,
     &             AN18921,AN28921,AN38921,AN48921,
     &             AW18921,AW28921,AW38921,AW48921,
     &             DAN18921,DAN28921,DAN38921,DAN48921,
     &             DAW18921,DAW28921,DAW38921,DAW48921)
C-------Get heads and sensitivities for 9 local cells
      CALL SSEN1HUF2VDHGET(
     &             IBOUND,HNEW,SNEW,J,I,K,NCOL,NROW,NLAY,
     &             H1,H2,H3,H4,H5,H6,H7,H8,H9,
     &             DH1,DH2,DH3,DH4,DH5,DH6,DH7,DH8,DH9,
     &             IBD2,IBD3,IBD4,IBD5,IBD6,IBD7,IBD8,IBD9)
C-------Calculate dTaa1/db, dTab1/db, and dTbb1/db
      IF(PID.EQ.'HK  '.OR.PID.EQ.'HANI'.OR.PID.EQ.'LVDA') THEN
        DTAA1 = (DTAA1 - TAA1) / DB
        DTAB1 = (DTAB1 - TAB1) / DB
        DTBB1 = (DTBB1 - TBB1) / DB
      ELSE
        DTAA1 = ZERO
        DTAB1 = ZERO
        DTBB1 = ZERO
      ENDIF
C
C-------CALCULATE SENSITIVITY OF FLOW THROUGH THE LEFT FACE.
      CALL SSEN1HUF2VDDFL(DFL,
     &             AS16145,AS26145,AS36145,AS46145,
     &             AE16145,AE26145,AE36145,AE46145,
     &             DAS16145,DAS26145,DAS36145,DAS46145,
     &             DAE16145,DAE26145,DAE36145,DAE46145,
     &             AN17816,AN27816,AN37816,AN47816,
     &             AE17816,AE27816,AE37816,AE47816,
     &             DAN17816,DAN27816,DAN37816,DAN47816,
     &             DAE17816,DAE27816,DAE37816,DAE47816,
     &             H1,H4,H5,H6,H7,H8,TAA1,TAB1,TBB1,
     &             DH1,DH4,DH5,DH6,DH7,DH8,DTAA1,DTAB1,DTBB1)
C
C-------CALCULATE SENSITIVITY OF FLOW THROUGH THE RIGHT FACE.
      CALL SSEN1HUF2VDDFR(DFR,
     &             AS11234,AS21234,AS31234,AS41234,
     &             AW11234,AW21234,AW31234,AW41234,
     &             DAS11234,DAS21234,DAS31234,DAS41234,
     &             DAW11234,DAW21234,DAW31234,DAW41234,
     &             AN18921,AN28921,AN38921,AN48921,
     &             AW18921,AW28921,AW38921,AW48921,
     &             DAN18921,DAN28921,DAN38921,DAN48921,
     &             DAW18921,DAW28921,DAW38921,DAW48921,
     &             H1,H2,H3,H4,H8,H9,TAA1,TAB1,TBB1,
     &             DH1,DH2,DH3,DH4,DH8,DH9,DTAA1,DTAB1,DTBB1)
C
C-------CALCULATE SENSITIVITY OF FLOW THROUGH THE TOP FACE.
      CALL SSEN1HUF2VDDFT(DFT,
     &             AN17816,AN27816,AN37816,AN47816,
     &             AE17816,AE27816,AE37816,AE47816,
     &             DAN17816,DAN27816,DAN37816,DAN47816,
     &             DAE17816,DAE27816,DAE37816,DAE47816,
     &             AN18921,AN28921,AN38921,AN48921,
     &             AW18921,AW28921,AW38921,AW48921,
     &             DAN18921,DAN28921,DAN38921,DAN48921,
     &             DAW18921,DAW28921,DAW38921,DAW48921,
     &             H7,H8,H1,H6,H9,H2,TAA1,TAB1,TBB1,
     &             DH7,DH8,DH1,DH6,DH9,DH2,DTAA1,DTAB1,DTBB1)
C
C-------CALCULATE SENSITIVITY OF FLOW THROUGH THE BOTTOM FACE.
      CALL SSEN1HUF2VDDFB(DFB,
     &             AS16145,AS26145,AS36145,AS46145,
     &             AE16145,AE26145,AE36145,AE46145,
     &             DAS16145,DAS26145,DAS36145,DAS46145,
     &             DAE16145,DAE26145,DAE36145,DAE46145,
     &             AS11234,AS21234,AS31234,AS41234,
     &             AW11234,AW21234,AW31234,AW41234,
     &             DAS11234,DAS21234,DAS31234,DAS41234,
     &             DAW11234,DAW21234,DAW31234,DAW41234,
     &             H1,H2,H3,H4,H5,H6,TAA1,TAB1,TBB1,
     &             DH1,DH2,DH3,DH4,DH5,DH6,DTAA1,DTAB1,DTBB1)
C
C-------RETURN.
      RETURN
      END
c======================================================================
      SUBROUTINE SSEN1HUF2VDHGET(
     1  IBOUND,HNEW,SNEW,J,I,K,NCOL,NROW,NLAY,
     2  H1,H2,H3,H4,H5,H6,H7,H8,H9,
     &  DH1,DH2,DH3,DH4,DH5,DH6,DH7,DH8,DH9,
     3  IBD2,IBD3,IBD4,IBD5,IBD6,IBD7,IBD8,IBD9)
C
C     ******************************************************************
C     Grab heads, sensitivities, and ibound for all local cells
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION HNEW,SNEW,H1,H2,H3,H4,H5,H6,H7,H8,H9,
     &  DH1,DH2,DH3,DH4,DH5,DH6,DH7,DH8,DH9
      DIMENSION HNEW(NCOL,NROW,NLAY),IBOUND(NCOL,NROW,NLAY),
     &  SNEW(NCOL,NROW,NLAY)
C-----------------------------------------------------------------------
C
C
      ZERO = 0.0
      H1 = HNEW(J,I,K)
      DH1 = SNEW(J,I,K)
C-------Local cell 2, I,J+1
      IF(J.LT.NCOL .AND. IBOUND(J+1,I,K).NE.0) THEN
        H2 = HNEW(J+1,I,K)
        DH2 = SNEW(J+1,I,K)
        IBD2 = IABS(IBOUND(J+1,I,K))
      ELSE
        H2 = ZERO
        DH2 = ZERO
        IBD2 = 0
      ENDIF
C-------Local cell 3, I+1,J+1
      IF(I.LT.NROW.AND.J.LT.NCOL.AND.IBOUND(J+1,I+1,K).NE.0) THEN
        H3 = HNEW(J+1,I+1,K)
        DH3 = SNEW(J+1,I+1,K)
        IBD3 = IABS(IBOUND(J+1,I+1,K))
      ELSE
        H3 = ZERO
        DH3 = ZERO
        IBD3 = 0
      ENDIF
C-------Local cell 4, I+1,J
      IF(I.LT.NROW .AND. IBOUND(J,I+1,K).NE.0) THEN
        H4 = HNEW(J,I+1,K)
        DH4 = SNEW(J,I+1,K)
        IBD4 = IABS(IBOUND(J,I+1,K))
      ELSE
        H4 = ZERO
        DH4 = ZERO
        IBD4 = 0
      ENDIF
C-------Local cell 5, I+1,J-1
      IF(J.GT.1.AND.I.LT.NROW .AND. IBOUND(J-1,I+1,K).NE.0) THEN
        H5 = HNEW(J-1,I+1,K)
        DH5 = SNEW(J-1,I+1,K)
        IBD5 = IABS(IBOUND(J-1,I+1,K))
      ELSE
        H5 = ZERO
        DH5 = ZERO
        IBD5 = 0
      ENDIF
C-------Local cell 6, I,J-1
      IF(J.GT.1 .AND. IBOUND(J-1,I,K).NE.0) THEN
        H6 = HNEW(J-1,I,K)
        DH6 = SNEW(J-1,I,K)
        IBD6 = IABS(IBOUND(J-1,I,K))
      ELSE
        H6 = ZERO
        DH6 = ZERO
        IBD6 = 0
      ENDIF
C-------Local cell 7, I-1,J-1
      IF(I.GT.1.AND.J.GT.1 .AND. IBOUND(J-1,I-1,K).NE.0) THEN
        H7 = HNEW(J-1,I-1,K)
        DH7 = SNEW(J-1,I-1,K)
        IBD7 = IABS(IBOUND(J-1,I-1,K))
      ELSE
        H7 = ZERO
        DH7 = ZERO
        IBD7 = 0
      ENDIF
C-------Local cell 8, I-1,J
      IF(I.GT.1 .AND. IBOUND(J,I-1,K).NE.0) THEN
        H8 = HNEW(J,I-1,K)
        DH8 = SNEW(J,I-1,K)
        IBD8 = IABS(IBOUND(J,I-1,K))
      ELSE
        H8 = ZERO
        DH8 = ZERO
        IBD8 = 0
      ENDIF
C-------Local cell 9, I-1,J+1
      IF(J.LT.NCOL.AND.I.GT.1 .AND. IBOUND(J+1,I-1,K).NE.0) THEN
        H9 = HNEW(J+1,I-1,K)
        DH9 = SNEW(J+1,I-1,K)
        IBD9 = IABS(IBOUND(J+1,I-1,K))
      ELSE
        H9 = ZERO
        DH9 = ZERO
        IBD9 = 0
      ENDIF
C-------RETURN.
      RETURN
      END
c======================================================================
      SUBROUTINE SSEN1HUF2VDA1(
     &  IBOUND,VDHT,DVDH,DB,NCOL,NROW,NLAY,I,J,K,TMPCOF,PID,
     &  AS11234,AS21234,AS31234,AS41234,
     &  AW11234,AW21234,AW31234,AW41234,
     &  DAS11234,DAS21234,DAS31234,DAS41234,
     &  DAW11234,DAW21234,DAW31234,DAW41234,
     &  TAA1,TAB1,TBB1,DTAA1,DTAB1,DTBB1)
C
C     ******************************************************************
C     Calculate sensitivities of the A coefficients for Quadrant I
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*4 PID
      DOUBLE PRECISION
     &             DAS11234,DAS21234,DAS31234,DAS41234,
     &             DAW11234,DAW21234,DAW31234,DAW41234,
     &             AS11234,AS21234,AS31234,AS41234,
     &             AW11234,AW21234,AW31234,AW41234
      DIMENSION VDHT(NCOL,NROW,NLAY,3),IBOUND(NCOL,NROW,NLAY),
     &  DVDH(NCOL,NROW,NLAY,3)
C-----------------------------------------------------------------------
C
C-----Call for perturbed parameter values
      IF(PID.EQ.'HK  '.OR.PID.EQ.'HANI'.OR.PID.EQ.'LVDA') THEN
        CALL SGWF1HUF2VDA1(
     &             IBOUND,DVDH,NCOL,NROW,NLAY,I,J,K,TMPCOF,
     &             DAS11234,DAS21234,DAS31234,DAS41234,
     &             DAW11234,DAW21234,DAW31234,DAW41234,
     &             DTAA1,DTAB1,DTBB1)
      ENDIF
C-----Call for unperturbed parameter values
      CALL SGWF1HUF2VDA1(
     &             IBOUND,VDHT,NCOL,NROW,NLAY,I,J,K,TMPCOF,
     &             AS11234,AS21234,AS31234,AS41234,
     &             AW11234,AW21234,AW31234,AW41234,
     &             TAA1,TAB1,TBB1)
C-----Calculate sensitivities
      IF(PID.EQ.'HK  '.OR.PID.EQ.'HANI'.OR.PID.EQ.'LVDA') THEN
        DAS11234 = (DAS11234 - AS11234) / DB
        DAS21234 = (DAS21234 - AS21234) / DB
        DAS31234 = (DAS31234 - AS31234) / DB
        DAS41234 = (DAS41234 - AS41234) / DB
        DAW11234 = (DAW11234 - AW11234) / DB
        DAW21234 = (DAW21234 - AW21234) / DB
        DAW31234 = (DAW31234 - AW31234) / DB
        DAW41234 = (DAW41234 - AW41234) / DB
      ELSE
        DTAA1 = 0.0
        DTAB1 = 0.0
        DTBB1 = 0.0
        DAS11234 = 0.0
        DAS21234 = 0.0
        DAS31234 = 0.0
        DAS41234 = 0.0
        DAW11234 = 0.0
        DAW21234 = 0.0
        DAW31234 = 0.0
        DAW41234 = 0.0
      ENDIF
C
C
C-------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SSEN1HUF2VDA2(
     &  IBOUND,VDHT,DVDH,DB,NCOL,NROW,NLAY,I,J,K,TMPCOF,PID,
     &  AS16145,AS26145,AS36145,AS46145,
     &  AE16145,AE26145,AE36145,AE46145,
     &  DAS16145,DAS26145,DAS36145,DAS46145,
     &  DAE16145,DAE26145,DAE36145,DAE46145)
C
C     ******************************************************************
C     Calculate sensitivities of the A coefficients for Quadrant II
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*4 PID
      DOUBLE PRECISION
     &  AS16145,AS26145,AS36145,AS46145,
     &  AE16145,AE26145,AE36145,AE46145,
     &  DAS16145,DAS26145,DAS36145,DAS46145,
     &  DAE16145,DAE26145,DAE36145,DAE46145
      DIMENSION VDHT(NCOL,NROW,NLAY,3),IBOUND(NCOL,NROW,NLAY),
     &  DVDH(NCOL,NROW,NLAY,3)
C-----------------------------------------------------------------------
C
C-----Call for unperturbed parameter values
      CALL SGWF1HUF2VDA2(
     &            IBOUND,VDHT,NCOL,NROW,NLAY,I,J,K,TMPCOF,
     &            AS16145,AS26145,AS36145,AS46145,
     &            AE16145,AE26145,AE36145,AE46145)
      IF(PID.EQ.'HK  '.OR.PID.EQ.'HANI'.OR.PID.EQ.'LVDA') THEN
C-------Call for perturbed parameter values
        CALL SGWF1HUF2VDA2(
     &            IBOUND,DVDH,NCOL,NROW,NLAY,I,J,K,TMPCOF,
     &            DAS16145,DAS26145,DAS36145,DAS46145,
     &            DAE16145,DAE26145,DAE36145,DAE46145)
CC-----Calculate sensitivities
        DAS16145 = (DAS16145 - AS16145) / DB
        DAS26145 = (DAS26145 - AS26145) / DB
        DAS36145 = (DAS36145 - AS36145) / DB
        DAS46145 = (DAS46145 - AS46145) / DB
        DAE16145 = (DAE16145 - AE16145) / DB
        DAE26145 = (DAE26145 - AE26145) / DB
        DAE36145 = (DAE36145 - AE36145) / DB
        DAE46145 = (DAE46145 - AE46145) / DB
      ELSE
        DAS16145 = 0.0
        DAS26145 = 0.0
        DAS36145 = 0.0
        DAS46145 = 0.0
        DAE16145 = 0.0
        DAE26145 = 0.0
        DAE36145 = 0.0
        DAE46145 = 0.0
      ENDIF
C
C
C-------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SSEN1HUF2VDA3(
     &  IBOUND,VDHT,DVDH,DB,NCOL,NROW,NLAY,I,J,K,TMPCOF,PID,
     &  AN17816,AN27816,AN37816,AN47816,
     &  AE17816,AE27816,AE37816,AE47816,
     &  DAN17816,DAN27816,DAN37816,DAN47816,
     &  DAE17816,DAE27816,DAE37816,DAE47816)
C
C     ******************************************************************
C     Calculate sensitivities of the A coefficients for Quadrant III
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*4 PID
      DOUBLE PRECISION
     &  AN17816,AN27816,AN37816,AN47816,
     &  AE17816,AE27816,AE37816,AE47816,
     &  DAN17816,DAN27816,DAN37816,DAN47816,
     &  DAE17816,DAE27816,DAE37816,DAE47816
      DIMENSION VDHT(NCOL,NROW,NLAY,3),IBOUND(NCOL,NROW,NLAY),
     &  DVDH(NCOL,NROW,NLAY,3)
C-----------------------------------------------------------------------
C
C-----Call for unperturbed parameter values
      CALL SGWF1HUF2VDA3(
     &            IBOUND,VDHT,NCOL,NROW,NLAY,I,J,K,TMPCOF,
     &            AN17816,AN27816,AN37816,AN47816,
     &            AE17816,AE27816,AE37816,AE47816)
      IF(PID.EQ.'HK  '.OR.PID.EQ.'HANI'.OR.PID.EQ.'LVDA') THEN
C-------Call for perturbed parameter values
        CALL SGWF1HUF2VDA3(
     &            IBOUND,DVDH,NCOL,NROW,NLAY,I,J,K,TMPCOF,
     &            DAN17816,DAN27816,DAN37816,DAN47816,
     &            DAE17816,DAE27816,DAE37816,DAE47816)
C-------Calculate sensitivities
        DAN17816 = (DAN17816 - AN17816) / DB
        DAN27816 = (DAN27816 - AN27816) / DB
        DAN37816 = (DAN37816 - AN37816) / DB
        DAN47816 = (DAN47816 - AN47816) / DB
        DAE17816 = (DAE17816 - AE17816) / DB
        DAE27816 = (DAE27816 - AE27816) / DB
        DAE37816 = (DAE37816 - AE37816) / DB
        DAE47816 = (DAE47816 - AE47816) / DB
      ELSE
        DAN17816 = 0.0
        DAN27816 = 0.0
        DAN37816 = 0.0
        DAN47816 = 0.0
        DAE17816 = 0.0
        DAE27816 = 0.0
        DAE37816 = 0.0
        DAE47816 = 0.0
      ENDIF
C
C
C-------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SSEN1HUF2VDA4(
     &  IBOUND,VDHT,DVDH,DB,NCOL,NROW,NLAY,I,J,K,TMPCOF,PID,
     &  AN18921,AN28921,AN38921,AN48921,
     &  AW18921,AW28921,AW38921,AW48921,
     &  DAN18921,DAN28921,DAN38921,DAN48921,
     &  DAW18921,DAW28921,DAW38921,DAW48921)
C
C     ******************************************************************
C     Calculate sensitivities of the A coefficients for Quadrant IV
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*4 PID
      DOUBLE PRECISION
     &  AN18921,AN28921,AN38921,AN48921,
     &  AW18921,AW28921,AW38921,AW48921,
     &  DAN18921,DAN28921,DAN38921,DAN48921,
     &  DAW18921,DAW28921,DAW38921,DAW48921
      DIMENSION VDHT(NCOL,NROW,NLAY,3),IBOUND(NCOL,NROW,NLAY),
     &  DVDH(NCOL,NROW,NLAY,3)
C-----------------------------------------------------------------------
C
C-----Call for unperturbed parameter values
      CALL SGWF1HUF2VDA4(
     &            IBOUND,VDHT,NCOL,NROW,NLAY,I,J,K,TMPCOF,
     &            AN18921,AN28921,AN38921,AN48921,
     &            AW18921,AW28921,AW38921,AW48921)
      IF(PID.EQ.'HK  '.OR.PID.EQ.'HANI'.OR.PID.EQ.'LVDA') THEN
C-----Call for perturbed parameter values
      CALL SGWF1HUF2VDA4(
     &            IBOUND,DVDH,NCOL,NROW,NLAY,I,J,K,TMPCOF,
     &            DAN18921,DAN28921,DAN38921,DAN48921,
     &            DAW18921,DAW28921,DAW38921,DAW48921)
C-----Calculate sensitivities
        DAN18921 = (DAN18921 - AN18921) / DB
        DAN28921 = (DAN28921 - AN28921) / DB
        DAN38921 = (DAN38921 - AN38921) / DB
        DAN48921 = (DAN48921 - AN48921) / DB
        DAW18921 = (DAW18921 - AW18921) / DB
        DAW28921 = (DAW28921 - AW28921) / DB
        DAW38921 = (DAW38921 - AW38921) / DB
        DAW48921 = (DAW48921 - AW48921) / DB
      ELSE
        DAN18921 = 0.0
        DAN28921 = 0.0
        DAN38921 = 0.0
        DAN48921 = 0.0
        DAW18921 = 0.0
        DAW28921 = 0.0
        DAW38921 = 0.0
        DAW48921 = 0.0
      ENDIF
C
C
C-------RETURN
      RETURN
      END
C======================================================================
      SUBROUTINE SSEN1HUF2VDDFL(DFL,
     &             AS16145,AS26145,AS36145,AS46145,
     &             AE16145,AE26145,AE36145,AE46145,
     &             DAS16145,DAS26145,DAS36145,DAS46145,
     &             DAE16145,DAE26145,DAE36145,DAE46145,
     &             AN17816,AN27816,AN37816,AN47816,
     &             AE17816,AE27816,AE37816,AE47816,
     &             DAN17816,DAN27816,DAN37816,DAN47816,
     &             DAE17816,DAE27816,DAE37816,DAE47816,
     &             H1,H4,H5,H6,H7,H8,TAA1,TAB1,TBB1,
     &             DH1,DH4,DH5,DH6,DH7,DH8,DTAA1,DTAB1,DTBB1)
C     ******************************************************************
C     COMPUTE SENSITIVITY OF LEFT CELL FACE FLOW
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION DFS2A1,DFS2A2,DFS2B1,DFS2B2,DFN3A1,DFN3A2,
     &  DFN3B1,DFN3B2,DFL,H1,H4,H5,H6,H7,H8,
     &  DH1,DH4,DH5,DH6,DH7,DH8,
     &             AS16145,AS26145,AS36145,AS46145,
     &             AE16145,AE26145,AE36145,AE46145,
     &             DAS16145,DAS26145,DAS36145,DAS46145,
     &             DAE16145,DAE26145,DAE36145,DAE46145,
     &             AN17816,AN27816,AN37816,AN47816,
     &             AE17816,AE27816,AE37816,AE47816,
     &             DAN17816,DAN27816,DAN37816,DAN47816,
     &             DAE17816,DAE27816,DAE37816,DAE47816
C     ------------------------------------------------------------------
C
      DFS2A1 = AS16145*H6 + AS26145*H1 + AS36145*H4 + AS46145*H5
      DFS2A1 = -DTAA1*(H1 - DFS2A1)
C
      DFS2A2 = DAS16145*H6 + DAS26145*H1 + DAS36145*H4 + DAS46145*H5
     &       + AS16145*DH6 + AS26145*DH1 + AS36145*DH4 + AS46145*DH5
      DFS2A2 = -TAA1*(DH1 - DFS2A2)
C
      DFS2B1 = AE16145*H6 + AE26145*H1 + AE36145*H4 + AE46145*H5
      DFS2B1 = -DTAB1*(DFS2B1 - H1)
C
      DFS2B2 = DAE16145*H6 + DAE26145*H1 + DAE36145*H4 + DAE46145*H5
     &       + AE16145*DH6 + AE26145*DH1 + AE36145*DH4 + AE46145*DH5
      DFS2B2 = -TAB1*(DFS2B2 - DH1)
C
      DFN3A1 = AN17816*H7 + AN27816*H8 + AN37816*H1 + AN47816*H6
      DFN3A1 = -DTAA1*(H1 - DFN3A1)
C
      DFN3A2 = DAN17816*H7 + DAN27816*H8 + DAN37816*H1 + DAN47816*H6
     &       + AN17816*DH7 + AN27816*DH8 + AN37816*DH1 + AN47816*DH6
      DFN3A2 = -TAA1*(DH1 - DFN3A2)
C
      DFN3B1 = AE17816*H7 + AE27816*H8 + AE37816*H1 + AE47816*H6
      DFN3B1 = -DTAB1*(H1 - DFN3B1)
C
      DFN3B2 = DAE17816*H7 + DAE27816*H8 + DAE37816*H1 + DAE47816*H6
     &       + AE17816*DH7 + AE27816*DH8 + AE37816*DH1 + AE47816*DH6
      DFN3B2 = -TAB1*(DH1 - DFN3B2)
C
      DFL = DFS2A1 + DFS2B1 + DFN3A1 + DFN3B1
     &    + DFS2A2 + DFS2B2 + DFN3A2 + DFN3B2
C
C
C-------RETURN
      RETURN
      END
C======================================================================
      SUBROUTINE SSEN1HUF2VDDFR(DFR,
     &             AS11234,AS21234,AS31234,AS41234,
     &             AW11234,AW21234,AW31234,AW41234,
     &             DAS11234,DAS21234,DAS31234,DAS41234,
     &             DAW11234,DAW21234,DAW31234,DAW41234,
     &             AN18921,AN28921,AN38921,AN48921,
     &             AW18921,AW28921,AW38921,AW48921,
     &             DAN18921,DAN28921,DAN38921,DAN48921,
     &             DAW18921,DAW28921,DAW38921,DAW48921,
     &             H1,H2,H3,H4,H8,H9,TAA1,TAB1,TBB1,
     &             DH1,DH2,DH3,DH4,DH8,DH9,DTAA1,DTAB1,DTBB1)
C     ******************************************************************
C     COMPUTE SENSITIVITY OF RIGHT CELL FACE FLOW
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION DFS1A1,DFS1A2,DFS1B1,DFS1B2,DFN4A1,DFN4A2,
     &  DFN4B1,DFN4B2,DFR,H1,H2,H3,H4,H8,H9,
     &  DH1,DH2,DH3,DH4,DH8,DH9,
     &             AS11234,AS21234,AS31234,AS41234,
     &             AW11234,AW21234,AW31234,AW41234,
     &             DAS11234,DAS21234,DAS31234,DAS41234,
     &             DAW11234,DAW21234,DAW31234,DAW41234,
     &             AN18921,AN28921,AN38921,AN48921,
     &             AW18921,AW28921,AW38921,AW48921,
     &             DAN18921,DAN28921,DAN38921,DAN48921,
     &             DAW18921,DAW28921,DAW38921,DAW48921
C     ------------------------------------------------------------------
C
      DFS1A1 = AS11234*H1 + AS21234*H2 + AS31234*H3 + AS41234*H4
      DFS1A1 = -DTAA1*(DFS1A1 - H1)
C
      DFS1A2 = DAS11234*H1 + DAS21234*H2 + DAS31234*H3 + DAS41234*H4
     &       + AS11234*DH1 + AS21234*DH2 + AS31234*DH3 + AS41234*DH4
      DFS1A2 = -TAA1*(DFS1A2 - DH1)
C
      DFS1B1 = AW11234*H1 + AW21234*H2 + AW31234*H3 + AW41234*H4
      DFS1B1 = -DTAB1*(DFS1B1 - H1)
C
      DFS1B2 = DAW11234*H1 + DAW21234*H2 + DAW31234*H3 + DAW41234*H4
     &       + AW11234*DH1 + AW21234*DH2 + AW31234*DH3 + AW41234*DH4
      DFS1B2 = -TAB1*(DFS1B2 - DH1)
C
      DFN4A1 = AN18921*H8 + AN28921*H9 + AN38921*H2 + AN48921*H1
      DFN4A1 = -DTAA1*(DFN4A1 - H1)
C
      DFN4A2 = DAN18921*H8 + DAN28921*H9 + DAN38921*H2 + DAN48921*H1
     &       + AN18921*DH8 + AN28921*DH9 + AN38921*DH2 + AN48921*DH1
      DFN4A2 = -TAA1*(DFN4A2 - DH1)
C
      DFN4B1 = AW18921*H8 + AW28921*H9 + AW38921*H2 + AW48921*H1
      DFN4B1 = -DTAB1*(H1 - DFN4B1)
C
      DFN4B2 = DAW18921*H8 + DAW28921*H9 + DAW38921*H2 + DAW48921*H1
     &       + AW18921*DH8 + AW28921*DH9 + AW38921*DH2 + AW48921*DH1
      DFN4B2 = -TAB1*(DH1 - DFN4B2)
C
      DFR = DFS1A1 + DFS1B1 + DFN4A1 + DFN4B1
     &    + DFS1A2 + DFS1B2 + DFN4A2 + DFN4B2
C
C
C-------RETURN
      RETURN
      END
C======================================================================
      SUBROUTINE SSEN1HUF2VDDFT(DFT,
     &             AN17816,AN27816,AN37816,AN47816,
     &             AE17816,AE27816,AE37816,AE47816,
     &             DAN17816,DAN27816,DAN37816,DAN47816,
     &             DAE17816,DAE27816,DAE37816,DAE47816,
     &             AN18921,AN28921,AN38921,AN48921,
     &             AW18921,AW28921,AW38921,AW48921,
     &             DAN18921,DAN28921,DAN38921,DAN48921,
     &             DAW18921,DAW28921,DAW38921,DAW48921,
     &             H7,H8,H1,H6,H9,H2,TAA1,TAB1,TBB1,
     &             DH7,DH8,DH1,DH6,DH9,DH2,DTAA1,DTAB1,DTBB1)
C     ******************************************************************
C     COMPUTE SENSITIVITY OF TOP CELL FACE FLOW
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION DFE3A1,DFE3A2,DFE3B1,DFE3B2,DFW4A1,DFW4A2,
     &  DFW4B1,DFW4B2,DFT,H7,H8,H1,H6,H9,H2,
     &  DH7,DH8,DH1,DH6,DH9,DH2,
     &             AN17816,AN27816,AN37816,AN47816,
     &             AE17816,AE27816,AE37816,AE47816,
     &             DAN17816,DAN27816,DAN37816,DAN47816,
     &             DAE17816,DAE27816,DAE37816,DAE47816,
     &             AN18921,AN28921,AN38921,AN48921,
     &             AW18921,AW28921,AW38921,AW48921,
     &             DAN18921,DAN28921,DAN38921,DAN48921,
     &             DAW18921,DAW28921,DAW38921,DAW48921
C     ------------------------------------------------------------------
C
      DFE3A1 = AN17816*H7 + AN27816*H8 + AN37816*H1 + AN47816*H6
      DFE3A1 = -DTAB1*(H1 - DFE3A1)
C
      DFE3A2 = DAN17816*H7 + DAN27816*H8 + DAN37816*H1 + DAN47816*H6
     &       + AN17816*DH7 + AN27816*DH8 + AN37816*DH1 + AN47816*DH6
      DFE3A2 = -TAB1*(DH1 - DFE3A2)
C
      DFE3B1 = AE17816*H7 + AE27816*H8 + AE37816*H1 + AE47816*H6
      DFE3B1 = -DTBB1*(H1 - DFE3B1)
C
      DFE3B2 = DAE17816*H7 + DAE27816*H8 + DAE37816*H1 + DAE47816*H6
     &       + AE17816*DH7 + AE27816*DH8 + AE37816*DH1 + AE47816*DH6
      DFE3B2 = -TBB1*(DH1 - DFE3B2)
C
      DFW4A1 = AN18921*H8 + AN28921*H9 + AN38921*H2 + AN48921*H1
      DFW4A1 = -DTAB1*(DFW4A1 - H1)
C
      DFW4A2 = DAN18921*H8 + DAN28921*H9 + DAN38921*H2 + DAN48921*H1
     &       + AN18921*DH8 + AN28921*DH9 + AN38921*DH2 + AN48921*DH1
      DFW4A2 = -TAB1*(DFW4A2 - DH1)
C
      DFW4B1 = AW18921*H8 + AW28921*H9 + AW38921*H2 + AW48921*H1
      DFW4B1 = -DTBB1*(H1 - DFW4B1)
C
      DFW4B2 = DAW18921*H8 + DAW28921*H9 + DAW38921*H2 + DAW48921*H1
     &       + AW18921*DH8 + AW28921*DH9 + AW38921*DH2 + AW48921*DH1
      DFW4B2 = -TBB1*(DH1 - DFW4B2)
C
      DFT = DFE3A1 + DFE3B1 + DFW4A1 + DFW4B1
     &    + DFE3A2 + DFE3B2 + DFW4A2 + DFW4B2
C
C
C-------RETURN
      RETURN
      END
C======================================================================
      SUBROUTINE SSEN1HUF2VDDFB(DFB,
     &             AS16145,AS26145,AS36145,AS46145,
     &             AE16145,AE26145,AE36145,AE46145,
     &             DAS16145,DAS26145,DAS36145,DAS46145,
     &             DAE16145,DAE26145,DAE36145,DAE46145,
     &             AS11234,AS21234,AS31234,AS41234,
     &             AW11234,AW21234,AW31234,AW41234,
     &             DAS11234,DAS21234,DAS31234,DAS41234,
     &             DAW11234,DAW21234,DAW31234,DAW41234,
     &             H1,H2,H3,H4,H5,H6,TAA1,TAB1,TBB1,
     &             DH1,DH2,DH3,DH4,DH5,DH6,DTAA1,DTAB1,DTBB1)
C     ******************************************************************
C     COMPUTE SENSITIVITY OF BOTTOM CELL FACE FLOW
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION DFE2A1,DFE2A2,DFE2B1,DFE2B2,DFW1A1,DFW1A2,
     &  DFW1B1,DFW1B2,DFB,H1,H2,H3,H4,H5,H6,
     &  DH1,DH2,DH3,DH4,DH5,DH6,
     &             AS16145,AS26145,AS36145,AS46145,
     &             AE16145,AE26145,AE36145,AE46145,
     &             DAS16145,DAS26145,DAS36145,DAS46145,
     &             DAE16145,DAE26145,DAE36145,DAE46145,
     &             AS11234,AS21234,AS31234,AS41234,
     &             AW11234,AW21234,AW31234,AW41234,
     &             DAS11234,DAS21234,DAS31234,DAS41234,
     &             DAW11234,DAW21234,DAW31234,DAW41234
C     ------------------------------------------------------------------
C
      DFE2A1 = AS16145*H6 + AS26145*H1 + AS36145*H4 + AS46145*H5
      DFE2A1 = -DTAB1*(H1 - DFE2A1)
C
      DFE2A2 = DAS16145*H6 + DAS26145*H1 + DAS36145*H4 + DAS46145*H5
     &       + AS16145*DH6 + AS26145*DH1 + AS36145*DH4 + AS46145*DH5
      DFE2A2 = -TAB1*(DH1 - DFE2A2)
C
      DFE2B1 = AE16145*H6 + AE26145*H1 + AE36145*H4 + AE46145*H5
      DFE2B1 = -DTBB1*(DFE2B1 - H1)
C
      DFE2B2 = DAE16145*H6 + DAE26145*H1 + DAE36145*H4 + DAE46145*H5
     &       + AE16145*DH6 + AE26145*DH1 + AE36145*DH4 + AE46145*DH5
      DFE2B2 = -TBB1*(DFE2B2 - DH1)
C
      DFW1A1 = AS11234*H1 + AS21234*H2 + AS31234*H3 + AS41234*H4
      DFW1A1 = -DTAB1*(DFW1A1 - H1)
C
      DFW1A2 = DAS11234*H1 + DAS21234*H2 + DAS31234*H3 + DAS41234*H4
     &       + AS11234*DH1 + AS21234*DH2 + AS31234*DH3 + AS41234*DH4
      DFW1A2 = -TAB1*(DFW1A2 - DH1)
C
      DFW1B1 = AW11234*H1 + AW21234*H2 + AW31234*H3 + AW41234*H4
      DFW1B1 = -DTBB1*(DFW1B1 - H1)
C
      DFW1B2 = DAW11234*H1 + DAW21234*H2 + DAW31234*H3 + DAW41234*H4
     &       + AW11234*DH1 + AW21234*DH2 + AW31234*DH3 + AW41234*DH4
      DFW1B2 = -TBB1*(DFW1B2 - DH1)
C
      DFB = DFE2A1 + DFE2B1 + DFW1A1 + DFW1B1
     &    + DFE2A2 + DFE2B2 + DFW1A2 + DFW1B2
C
C
C-------RETURN
      RETURN
      END


