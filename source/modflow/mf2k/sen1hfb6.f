C     Last change:  ERB   6 Sep 2002    3:09 pm
C=======================================================================
      SUBROUTINE SEN1HFB6FM(BOTM,DELC,DELR,HNEW,HFB,IIPP,MXACTFB,MXHFB,
     &                      NBOTM,NCOL,NLAY,NROW,RHS,LAYHDT,NHFBNP,
     &                      IBOUND)
C-----VERSION 20010222 ERB
C     ******************************************************************
C     FOR PARAMETERIZED HORIZONTAL FLOW BARRIERS: FORMULATE RIGHT-HAND
C     SIDE TO CALCULATE SENSITIVITIES FOR HFB PARAMETERS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL BOTM, C, DE, DELC, DELR, DU, DV, HCDW, HFB, HO, RHS, TDW,
     &     U, V, ZERO
      INTEGER IBOUND, I1, I2, II, J1, J2, K, NBOTM, NCOL, NLAY,
     &        NROW
      DOUBLE PRECISION HNEW(NCOL,NROW,NLAY)
      DIMENSION BOTM(NCOL,NROW,0:NBOTM), DELC(NROW), DELR(NCOL),
     &          HFB(7,MXHFB), RHS(NCOL,NROW,NLAY), LAYHDT(NLAY),
     &          IBOUND(NCOL,NROW,NLAY)
      COMMON /DISCOM/LBOTM(999),LAYCBD(999)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
C
      ZERO = 0.0
C
C     COPY UNMODIFIED CONDUCTANCE FROM ACTIVE PART OF HFB
C     ARRAY TO PART WHERE PARAMETER INFORMATION IS STORED
      CALL SSEN1HFB6FC(HFB,IIPP,MXACTFB,MXHFB,NHFBNP)
C
C     LOOP THROUGH BARRIERS CONTROLLED BY THIS PARAMETER
      DO 50 II = IPLOC(1,IIPP),IPLOC(2,IIPP)
        K = HFB(1,II)
        I1 = HFB(2,II)
        J1 = HFB(3,II)
        I2 = HFB(4,II)
        J2 = HFB(5,II)
C
C-------IF CELL ON EITHER SIDE OF BARRIER IS INACTIVE, CONTRIBUTION TO
C       SENSITIVITY IS ZERO -- ERB 8/25/03
        IF (IBOUND(J1,I1,K).EQ.0 .OR. IBOUND(J2,I2,K).EQ.0) GOTO 50
C
C-------GET THE HYDRAULIC CHARACTERISTIC FOR THIS BARRIER
C-------AND CALCULATE THO, TH1, HO, AND HP
        HCDW = HFB(6,II)*B(IIPP)
        TH0 = BOTM(J1,I1,LBOTM(K)-1) - BOTM(J1,I1,LBOTM(K))
        TH1 = BOTM(J2,I2,LBOTM(K)-1) - BOTM(J2,I2,LBOTM(K))
        HO = HNEW(J1,I1,K)   ! Changed 9/6/02 - ERB
        HP = HNEW(J2,I2,K)   ! Changed 9/6/02 - ERB
C-------FOR VARIABLE THICKNESS LAYERS, CALCULATE SATURATED THICKNESSES
        IF (LAYHDT(K).GT.0) THEN
          IF (HO.LT.BOTM(J1,I1,LBOTM(K)-1)) TH0 =
     &                                      HO - BOTM(J1,I1,LBOTM(K))
          IF (HP.LT.BOTM(J2,I2,LBOTM(K)-1)) TH1 =                       ! Changed 9/6/02 - ERB
     &                                      HP - BOTM(J2,I2,LBOTM(K))   ! Changed 9/6/02 - ERB
        ENDIF
        TDW = HCDW*(TH0+TH1)/2.0
C-------VARIABLES THAT DEPEND ON WHETHER THE BARRIER IS
C-------BETWEEN ROWS OR COLUMNS
c        IF (I1.EQ.I2) THEN
        IF (J1.EQ.J2) THEN   ! Changed 9/6/02 - ERB
          DE = DELR(J1)
        ELSE
          DE = DELC(I1)
        ENDIF
        HH = HO - HP
C-------U AND V ARE THE NUMERATOR AND DENOMINATOR, RESPECTIVELY, OF THE
C----------CONDUCTANCE INCLUDING THE HORIZONTAL FLOW BARRIER TERM.
C-------C IS THE CONDUCTANCE WITHOUT THE HORIZONTAL FLOW BARRIER TERM.
C-------DU IS THE DERIVATIVE OF THE NUMERATOR WITH RESPECT TO THE
C----------HFB PARAMETER
C-------DV IS THE DERIVATIVE OF THE DENOMINATOR WITH RESPECT TO THE
C----------HFB PARAMETER
C-------USE THE BASIC EQUATION FOR THE DERIVATIVE OF U/V
C----------THAT IS,(V DU - U DV)/V**2
        C = HFB(7,II)
        U = C*TDW*DE
        V = C+TDW*DE
        DU = C*DE*HFB(6,II)*(TH0+TH1)/2.0
        DV = DE*HFB(6,II)*(TH0+TH1)/2.0
        CO = (V*DU - U*DV)/V**2
        IF (CO.NE.ZERO) THEN
C---------CALCULATE RHS
          RHS(J2,I2,K) = RHS(J2,I2,K) - CO*HH
          RHS(J1,I1,K) = RHS(J1,I1,K) + CO*HH
        ENDIF
50    CONTINUE
      RETURN
      END
C=======================================================================
      SUBROUTINE SSEN1HFB6MD(C,CHAR,CO,DELC,DELR,HFB,I,J,K,MXACTFB,
     &                       NCOL,NHFB,NROW,TH0,TH1)
C-----VERSION 19990507 ERB
C     ******************************************************************
C     FOR HORIZONTAL FLOW BARRIERS: MODIFY CONDUCTANCE DERIVATIVES FROM
C     SSENLPF1CH (OR SSENHUF1CH) AS NEEDED.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL CO, DE, DELC, DELR, DU, DV, HFB, TDW, TH0, TH1, U, V
      INTEGER I, I1, I2, J, J1, J2, K, NCOL, NHFB, NROW
      CHARACTER*2 CHAR
      DIMENSION HFB(7,MXACTFB), DELR(NCOL), DELC(NROW)
C     ------------------------------------------------------------------
C
C-----DETERMINE IF THE CONDUCTANCE INVOLVES A HORIZONTAL FLOW BARRIER
C
C-----CHECK THE ROW AND COLUMN BY LOOPING THROUGH ALL HORIZONTAL FLOW
C-----BOUNDARY CELLS
C
      DO 40 NB = 1,NHFB
        K1 = HFB(1,NB)
        IF (K1.EQ.K) THEN
          I1 = HFB(2,NB)
          J1 = HFB(3,NB)
          I2 = HFB(4,NB)
          J2 = HFB(5,NB)
          IF (I1.EQ.I.AND.J1.EQ.J .OR. I2.EQ.I.AND.J2.EQ.J) THEN
            IF (I1.EQ.I.AND.J1.EQ.J .AND.
     &         (I2.EQ.I+1 .OR. J2.EQ.J+1)) THEN
              IF (I2.EQ.I+1 .AND. CHAR.EQ.'CC') GOTO 50
              IF (J2.EQ.J+1 .AND. CHAR.EQ.'CR') GOTO 50
            ELSEIF (I2.EQ.I.AND.J2.EQ.J .AND.
     &             (I1.EQ.I+1. .OR. J1.EQ.J+1)) THEN
              IF (I1.EQ.I+1 .AND. CHAR.EQ.'CC') GOTO 50
              IF (J1.EQ.J+1 .AND. CHAR.EQ.'CR') GOTO 50
            ENDIF
          ENDIF
        ENDIF
40    CONTINUE
      RETURN
C-----THE CONDUCTANCE INVOLVES A HORIZONTAL FLOW BARRIER
50    CONTINUE
      TDW = HFB(6,NB)*(TH0+TH1)/2.0
      IF (I1.EQ.I2) THEN
        DE = DELC(I1)
      ELSE
        DE = DELR(J1)
      ENDIF
C-----U AND V ARE THE NUMERATOR AND DENOMINATOR, RESPECTIVELY, OF THE
C--------CONDUCTANCE INCLUDING THE HORIZONTAL FLOW BARRIER TERMS.
C-----C IS THE CONDUCTANCE WITHOUT THE HORIZONTAL FLOW BARRIER TERMS.
C-----DU IS THE DERIVATIVE OF THE NUMERATOR WITH RESPECT TO THE
C--------T PARAMETER
C-----DV IS THE DERIVATIVE OF THE DENOMINATOR WITH RESPECT TO THE
C--------T PARAMETER
C-----USE THE BASIC EQUATION FOR THE DERIVATIVE OF U/V
C--------THAT IS,(V DU - U DV)/V**2
      U = C*TDW*DE
      V = C+TDW*DE
      DU = TDW*DE*CO
      DV = CO
      CO = (V*DU - U*DV)/V**2
      RETURN
      END
C=======================================================================
      SUBROUTINE SSEN1HFB6FC(HFB,IIPP,MXACTFB,MXHFB,NHFBNP)
C
C-----VERSION 19990524 ERB
C     ******************************************************************
C     COPY UNMODIFIED CONDUCTANCE FOR PARAMETERIZED HORIZONTAL FLOW
C     BARRIERS FROM ACTIVE PART OF HFB ARRAY TO PARAMETER PART
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL HFB(7,MXHFB)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
      IFIRST = NHFBNP + 1
C     LOOP THROUGH BARRIERS CONTROLLED BY PARAMETER IIPP
      DO 50 IBP = IPLOC(1,IIPP),IPLOC(2,IIPP)
        IBPK = HFB(1,IBP)
        IBPI1 = HFB(2,IBP)
        IBPJ1 = HFB(3,IBP)
        IBPI2 = HFB(4,IBP)
        IBPJ2 = HFB(5,IBP)
C       SEARCH FOR MATCHING BARRIER IN ACTIVE PART OF HFB ARRAY
        DO 30 IBA = IFIRST,MXACTFB
          IBAK = HFB(1,IBA)
          IBAI1 = HFB(2,IBA)
          IBAJ1 = HFB(3,IBA)
          IBAI2 = HFB(4,IBA)
          IBAJ2 = HFB(5,IBA)
          IF (IBPK.EQ.IBAK .AND. IBPI1.EQ.IBAI1 .AND. IBPJ1.EQ.IBAJ1
     &        .AND. IBPI2.EQ.IBAI2 .AND. IBPJ2.EQ.IBAJ2) THEN
C           COPY UNMODIFIED CONDUCTANCE
            HFB(7,IBP) = HFB(7,IBA)
            GOTO 40
          ENDIF
   30   CONTINUE
   40   CONTINUE
   50 CONTINUE
C
      RETURN
      END


