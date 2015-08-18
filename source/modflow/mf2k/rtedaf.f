C
C     ************************  ROUTE  *********************************
C
      SUBROUTINE ROUTE
     I                (AO,A1,A2,SL,DT,IDBG,IERR,J,LUOT,NN,NXSEC,
     M                 NS,F,PX,QI,TRB,TF,VI,W1,W2,X,TIME,JTS)
C
C     + + + PURPOSE + + +
C     Route through each branch.

C     + + + PARAMETERS + + +
      INCLUDE 'params.inc'
C     NOSC   - Maximum number of cross sections (nodes) allowed in branch
C     NOSH   - Maximum number of shocks allowed in brach
C              (NOSH should be at least 4 times NOSC)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    IDBG,IERR,J,LUOT,NN,NS,NXSEC
      REAL       AA,AO(NOSC),A1(NOSC),A2(NOSC),BB,SL(NOSC),DT,F(NOSH),
     $           QI,TRB(NOSC),TF(NOSC),PX(NOSH),VI,W1(NOSC),W2(NOSC),
     $           X(NOSC)
C
C      + + + ARGUMENT DEFINITIONS + + +
C     AO(I)   - cross sectional area at zero flow
C     A1(I)   - coefficient in area equation A=AO+A1(Q**A2).
C     A2(I)   - coefficient in area equation A=AO+A1(Q**A2).
C     SL(I)   - Slope, wave dispersion coefficient = Q/(2*S*W)
C     DT      - time step size in seconds
C     F(K)    - flow in shock K
C     IDBG    - debugger code(0=no, 1=write debug code)
C     J       - time step
C     NN      - branch number
C     NS      - number of shocks
C     NXSEC   - number of Eulerian nodes (subreaches in a branch)
C     PX(K)   - location of u/s boundary of shock K
C     QI      - Insignificant discharge (QP/100000)
C     TRB(I)  - new flow in trib at node I
C     TF(I)   - flow in trib at start of time step at node I
C     VI      - Insignificant volume (QI*DTS)
C     X(I)    - dist of node I from u/s boundry
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  I,IDT,IC(NOSH),INX,JJ,K,KC,KK,KL,KM,KR,K1,L,LT(NOSH),
     #         LTTF(NOSC),MX,N,NSN,NT(NOSH),NTC,NTG,NTW,KAI             ********
      REAL     A,ATF(NOSC),C(NOSH),COF,DL,DLM,PDX,QL,QS,QT,TPT,VM,VS,
     #         VL,VT,XI,XL,XR1,XR,ERV,DLR                               ********
C
C     + + + LOCAL VARIABLE DEFINITIONS + + +
C     A      - cross sectional area A=AO+A1*Q**A2
C     ATF(I) - actual tributary flow at node as liminted by supply
C     COF    - local coefficient
C     TPT    - local coefficient
C     XR1    - local coefficient
C     XI     - local coefficient
C     C(K)   - celerity of shock K
C     DL     - dispersion length DL=(SQRT(2*D*DT)) where D=dispersion coef
C     DLM    - minimum value of DL to keep NS<NOSH
C     IC(K)  - code for shock status (0=move complete, 1=no limits,
C              2=will pass node next, 3=will pass shock next)
C     INX    - number of subreaches
C     K      - shock number
C     KC     - critical shock
C     KL     - last shock in series
C     KM     - number of shock to be mixed
C     KR     - reference shock or wave on upstream side
C     LT(K)  - last time shock K was updated (in seconds*100)
C     LTTF(I)- last time actual trib flow was updated (in sec*100)
C     MX     - subreach where shock is located
C     NSN    - new number of shocks
C     NT(K)  - next time shock K needs updated (in seconds*100).
C     NTC    - time to next critical point, in 0.01 seconds
C     NTG    - time to pass next node in 0.01 seconds
C     NTW    - time to pass next wave in 0.01 seconds
C     PDX    - part of subreach or shock in question
C     QL     - largest discharge of mixed wave
C     QS     - smallest discharge of mixed wave
C     QT     - trial value of discharge
C     VL     - volume of large wave
C     VM     - volume of mixed shock
C     VS     - volume of small wave
C     VT     - trial volume
C     XL     - left coordinate of mixed shock
C     XR     - right coordinate of mixed shock
C
C     + + + INTRINSICS + + +
      INTRINSIC ABS,FLOAT,SQRT,MAX,MIN,IFIX
C
C     + + + EXTERNALS + + +
      EXTERNAL FMX,FKAI,FTPT,FVOL,FC
C
C     + + + END SPECIFICATIONS + + +
C
C     unix what information
C     INCLUDE 'versn.inc'
C     VERSN = '@(#)DAFLOW - last modified October 4, 1998, hej'
C     Compution of volumes made more robust to allow dry sections
C
      INX=NXSEC-1
C
C     **** Add shock and set inflow boundary values statements *********
      TF(1)=TRB(1)
        IF((ABS(TF(1)-F(1))).LT.QI)GO TO 20
        NS=NS+1
        DO 10 K=NS,2,-1
          F(K)=F(K-1)
          PX(K)=PX(K-1)
   10   CONTINUE
   20 CONTINUE
      F(1)=TF(1)
      IF(F(1).GT.0.0)THEN
        A=AO(1)+A1(1)*(F(1)**A2(1))
      ELSE
        A=AO(1)
      END IF
      PX(1)=0.0
      IF(A.GT.AO(1))PX(1)=X(1)-DT*F(1)/A
C
C     ************ Disperse constituents statements ********************
      DLM=0.5*(X(NXSEC)-X(1))/FLOAT(NOSH-NXSEC)
      KR=1
      XR=PX(1)
   30 KL=KR
          IF(KL.GE.NS)GO TO 130
            XL=XR
            CALL FMX(MX,NXSEC,PX(KL+1),X)
            DL=(DT*F(KL)**(1.0-W2(MX))/(W1(MX)*SL(MX)))**0.5
            DLR=A1(MX)*A2(MX)*(F(KL)**(A2(MX)-0.5-W2(MX)*0.5))
            DLR=DLR/((W1(MX)*SL(MX)*DT)**0.5)
            IF(DLR.GT.1.15)IERR=1
            IF(DL.LT.1.0)THEN
              KR=KR+1
              XR=PX(KR)
              GO TO 30
            END IF
          IF(DL.GE.DLM)GO TO 40
            DL=DLM
            WRITE(LUOT,*)' Dispersion length increased to keep',
     $                  ' NS < NOSH'
   40     CONTINUE
          AA=ABS(100.0*(PX(KL+1)-DL-XL))
          IF(AA.GT.DL)THEN
            XL=MAX(XL,(PX(KL+1)-DL))
          END IF
          XR=XL+2.0*DL
          XR=MIN(XR,X(NXSEC))
          CALL FKAI(KR,NS,PX,XR)
          XR1=XR
          IF(KR.GT.1.AND.KR.LT.NS)XR1=0.5*(PX(KR)+PX(KR+1))
          XR=MIN(XR,XR1)
          IF((KL+1).NE.KR)GO TO 50
            CALL FMX(MX,NXSEC,PX(KL),X)
            TPT=F(KL)
            CALL FTPT(MX,NXSEC,TPT,TF,X,PX(KR))
            IF((ABS(F(KR)-TPT)).LT.QI)GO TO 30
C
C         ****** Compute volume in mixed shock and find range of Q's ***
   50     CALL FVOL(AO,A1,A2,KL,NS,NXSEC,F,PX,TF,VM,X,XL,XR,QS,QL)
          IF(IDBG.EQ.1.AND.NN.EQ.1)WRITE(LUOT,*)'COMPUTE VOLUMES WITH ',
     $                          'KL,KR,XL,XR,DL', KL,KR,XL,XR,DL
C
C       ********** Correct flow in shock KR for tributaries ************
        CALL FMX(MX,NXSEC,PX(KR),X)
        CALL FTPT(MX,NXSEC,F(KR),TF,X,XR)
C
C       *************** Renumber shocks ********************************
        IF(XL.GT.PX(KL).OR.KL.EQ.1)THEN
          NSN=KL+2+NS-KR
          KM=KL+1
        ELSE
          NSN=KL+1+NS-KR
          KM=KL
        END IF
        IF(NSN.GT.NOSH)THEN
          WRITE(*,*)' Too many shocks in branch',NN
          IERR=21
          GO TO 290
        END IF
        IF(NSN.LE.NS)GO TO 80
C         ****************** Add shock *********************************
          DO 70 K=NS,KR,-1
            PX(K+1)=PX(K)
            F(K+1)=F(K)
   70     CONTINUE
   80   PX(KM+1)=XR
        F(KM+1)=F(KR)
        PX(KM)=XL
        IF(XR.GE.X(NXSEC))NSN=NSN-1
        K1=KM+2
        IF(K1.GT.NSN)GO TO 100
          KK=NS-NSN
          IF(KK.LE.0)GO TO 100
            DO 90 K=K1,NSN
              PX(K)=PX(K+KK)
              F(K)=F(K+KK)
   90       CONTINUE
  100   NS=NSN
C
C       *********** Compute mixed discharge ****************************
        F(KM)=1.01*QI
        CALL FVOL(AO,A1,A2,KM,NS,NXSEC,F,PX,TF,VS,X,XL,XR,AA,BB)
        IF(VS.LT.VM)THEN
          AA=1.01*QI
          QS=MAX(QS,AA)
        ELSE
          QS=0.0
        END IF
        F(KM)=QS
        CALL FVOL(AO,A1,A2,KM,NS,NXSEC,F,PX,TF,VS,X,XL,XR,AA,BB)
        F(KM)=QL
        CALL FVOL(AO,A1,A2,KM,NS,NXSEC,F,PX,TF,VL,X,XL,XR,AA,BB)
        ERV=VL-VS
        IF(IDBG.EQ.1.AND.NN.EQ.1)WRITE(LUOT,*)'Start,VM',VM
  110   IF(ABS(ERV).GT.VI)THEN
C         ************** Need another itteration ***********************
          IF(QS.GT.0.0)THEN
            AA=(ALOG(QL)-ALOG(QS))/(ALOG(VL)-ALOG(VS))
            QT=QL*((VM/VL)**AA)
          ELSE
            QT=QS+(QL-QS)*(VM-VS)/(VL-VS)
          END IF
          F(KM)=QT
          CALL FVOL(AO,A1,A2,KM,NS,NXSEC,F,PX,TF,VT,X,XL,XR,AA,BB)
          ERV=VM-VT
          IF(IDBG.EQ.1.AND.NN.EQ.1)WRITE(LUOT,*)'QS,VS,QL,VL,QT,VT',
     #                                QS,VS,QL,VL,QT,VT 
          IF(ERV.GT.0.0)THEN
C           ***************** Concave up *******************************
            QS=QT
            VS=VT
          ELSE
C           ****************** Concave down ****************************
            QL=QT
            VL=VT
          END IF
          GO TO 110
        END IF
C       ******************* Done ***************************************
        KR=KM+1
      GO TO 30
  130 CONTINUE
      IF(PX(NS).GE.X(NXSEC))NS=NS-1
      PX(1)=X(1)
      JJ=IFIX(TIME/DT+0.501)-JTS+1
      IF(IDBG.EQ.1)WRITE(LUOT,*)' AFTER MIX J,N',JJ,NN
      IF(IDBG.EQ.1)WRITE(LUOT,*)' PX=',(PX(K),K=1,NS)
      IF(IDBG.EQ.1)WRITE(LUOT,*)' F=',(F(K),K=1,NS)
C
C     ******************** Add shocks at tributaries *******************
      DO 160 I=2,INX
        LTTF(I)=0
        ATF(I)=0.0
        IF((ABS(TF(I)-TRB(I))).LT.QI)GO TO 160
          CALL FKAI(K,NS,PX,X(I))
          A=AO(I)+A1(I)*(F(K)**A2(I))
          XI=0.01
          IF(A.GT.0.0)XI=QI*DT/A
          IF(ABS(X(I)-PX(K)).LT.XI)GO TO 160
C           **************************** Add shock *********************
            CALL FKAI(KK,NS,PX,X(I))
            NS=NS+1
            K1=KK+2
            DO 150 K=NS,K1,-1
              PX(K)=PX(K-1)
              F(K)=F(K-1)
  150       CONTINUE
            PX(KK+1)=X(I)
            CALL FMX (MX,NXSEC,PX(KK),X)
            TPT=F(KK)
            CALL FTPT(MX,NXSEC,TPT,TF,X,X(I))
            IF(TPT.LE.0.0)THEN
              F(KK+1)=0.0
            ELSE
              F(KK+1)=TPT
            END IF
  160 CONTINUE
      IF(IDBG.EQ.1)WRITE(LUOT,*)'AFTER TF SHOCKS'
      IF(IDBG.EQ.1)WRITE(LUOT,*)'PX=',(PX(K),K=1,NS)
      IF(IDBG.EQ.1)WRITE(LUOT,*)'F=',(F(K),K=1,NS)
C
C     ************************** Move shocks ***************************
      DO 180 K=2,NS
        CALL FC(AO,C(K),A1,A2,K,NS,NXSEC,F,PX,TRB,X,QI)
        LT(K)=0
  180 CONTINUE
C     *********************** Compute break points *********************
      IDT=IFIX(DT*100.0+0.5)
      KC=0
      K=1
  190 K=K+1
        IF(K.GT.NS)GO TO 220
          IF(KC.NE.0)GO TO 220
  200       IC(K)=1
              NT(K)=IDT
              CALL FMX(MX,NXSEC,PX(K),X)
              IF(C(K).LE.0.0)THEN
                NTG=2*IDT
              ELSE
                NTG=LT(K)+IFIX(0.5+100.0*(X(MX+1)-PX(K))/C(K))
              END IF
              IF(NTG.GT.NT(K))GO TO 210
                IC(K)=2
                NT(K)=NTG
  210         IF(K.GE.NS)GO TO 220
                PDX=PX(K+1)-PX(K)
                PDX=PDX+0.01*(C(K)*FLOAT(LT(K))-C(K+1)*FLOAT(LT(K+1)))
                COF=PDX/DT
                TPT=C(K)-C(K+1)
        IF(TPT.LE.COF)GO TO 190
                NTW=IFIX(0.5+100.0*PDX/(C(K)-C(K+1)))
        IF(NTW.GT.NT(K))GO TO 190
                IC(K)=3
                NT(K)=NTW
        IF(KC.EQ.0.AND.K.LT.NS)GO TO 190
C       ********************** Find critical shock ********************
  220   KC=1
        NTC=IDT
        DO 230 K=2,NS
          IF(IC(K).EQ.0)GO TO 230
            IF(NT(K).GT.NTC)GO TO 230
              NTC=NT(K)
              KC=K
  230   CONTINUE
        IF(KC.EQ.1)GO TO 290
C         *********************** Move critical shock *****************
          K=KC
          CALL FMX(MX,NXSEC,PX(K),X)
          PX(K)=PX(K)+0.01*C(K)*FLOAT(NT(K)-LT(K))
          IF(IC(K).EQ.2)PX(K)=X(MX+1)
          IF(IC(K).EQ.3.AND.C(K+1).LE.0.0)PX(K)=PX(K+1)
          LT(K)=NT(K)
          IF(IC(K).NE.1)GO TO 240
C           **************************** Move complete *****************
            IC(K)=0
            GO TO 220
  240     IF(IC(K).NE.3)GO TO 270
C           *********************** Passed shock ***********************
            AA=ABS(PX(K)-X(MX+1))
            IF(AA.LT.1.0)THEN
C             **************** Passed node to ************************** 
              MX=MX+1
              AA=F(K)+TRB(MX)
              IF(AA.LT.0.0)THEN
C               ***************** Channel is dry ***********************
                ATF(MX)=ATF(MX)-F(K)*FLOAT(NT(K)-LTTF(MX))*0.01
              ELSE
C               ************ No limit on withdrawal ********************
                ATF(MX)=ATF(MX)+TRB(MX)*FLOAT(NT(K)-LTTF(MX))*0.01
            END IF
            LTTF(MX)=NT(K)
            END IF
C  Limit loop to NS-1  1-24-2003
C            DO 260 KK=K,NS
            DO 260 KK=K,NS-1
              IF(KK.LE.K)GO TO 250
                PX(KK)=PX(KK+1)
                LT(KK)=LT(KK+1)
                IC(KK)=IC(KK+1)
  250         C(KK)=C(KK+1)
              F(KK)=F(KK+1)
  260       NT(KK)=NT(KK+1)
            NS=NS-1
  270     IF(IC(K).NE.2)GO TO 280
C           ************************ Passed node ***********************
            MX=MX+1
            IF(MX.LT.NXSEC)GO TO 280
              IC(K)=0
              NS=NS-1
              GO TO 220
  280     IF(IC(K).EQ.2)THEN
            AA=F(K)+TRB(MX)
            IF(AA.LT.0.0)THEN
C             ***************** Channel is dry *************************
              ATF(MX)=ATF(MX)-F(K)*FLOAT(NT(K)-LTTF(MX))*0.01
              F(K)=0.0
            ELSE
C             ************ No limit on withdrawal **********************
              ATF(MX)=ATF(MX)+TRB(MX)*FLOAT(NT(K)-LTTF(MX))*0.01
              F(K)=AA
            END IF
            LTTF(MX)=NT(K)
          END IF
          CALL FC(AO,C(K),A1,A2,K,NS,NXSEC,F,PX,TRB,X,QI)
C         ******************** Update NTW for upstream wave ************
          IF(K.EQ.2) GO TO 200
          PDX=PX(K)-PX(K-1)
          PDX=PDX+0.01*(C(K-1)*FLOAT(LT(K-1))-C(K)*FLOAT(LT(K)))
          COF=PDX/DT
          TPT=C(K-1)-C(K)
          IF(TPT.LT.COF)GO TO 200
            NTW=IFIX(0.5+100.0*PDX/TPT)
          IF(NTW.GT.NT(K-1))GO TO 200
            IC(K-1)=3
            NT(K-1)=NTW
          GO TO 200
  290   CONTINUE
C       *************  Complete trib withdrawls ************************
        DO 300 I=2,INX
          XR=X(I)-0.1
          CALL FKAI (KAI,NS,PX,XR)
          CALL FMX(MX,NXSEC,PX(KAI),X)
          TPT=F(KAI)
          CALL FTPT (MX,NXSEC,TPT,TRB,X,XR)
          AA=TPT+TRB(I)
          IF(AA.LT.0.0)THEN
C           *************** Channel is dry *****************************
            ATF(I)=ATF(I)-TPT*FLOAT(IDT-LTTF(I))*0.01
            TF(I)=-TPT
          ELSE
C             ************ No limit on withdrawal **********************
            ATF(I)=ATF(I)+TRB(I)*FLOAT(IDT-LTTF(I))*0.01
            TF(I)=TRB(I)
          END IF
          TRB(I)=ATF(I)/DT
  300   CONTINUE
        IF(IDBG.EQ.1)WRITE(LUOT,*)'AFTER MOVE J,N, PX=',JJ,NN,
     #                           (PX(K),K=1,NS)
        IF(IDBG.EQ.1)WRITE(LUOT,*)'F',(F(K),K=1,NS)
C
  999 RETURN
      END
C
C
C
      SUBROUTINE FKAI (KAI,NS,PX,X)
C
C     + + + PURPOSE + + +
C     Find the value of KAI which is the shock located at node I
C
C     + + + PARAMETERS + + +
      INCLUDE 'params.inc'
C     NOSH   - Maximum number of shocks allowed in branch
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER KAI,NS
      REAL    PX(NOSH),X
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   ID, K
C
C     + + + INTRINSICS + + +
      INTRINSIC  IFIX
C
C     + + + END SPECIFICATIONS + + +
C
      K=0
    1 K=K+1
        IF(K.GT.NS) GO TO 2
          ID=IFIX(10.0*(X-PX(K)))
      IF(ID.GE.0)GO TO 1
    2 KAI=K-1
C
      RETURN
      END
C
C
C
      SUBROUTINE FMX (MX,NXSEC,PX,X)
C
C     + + + PURPOSE + + +
C     Find the subreach occupied by PX
C
C     + + + PARAMETERS + + +
      INCLUDE 'params.inc'
C     NOSC   - Maximum number of cross sections (nodes) allowed in branch
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER MX,NXSEC
      REAL PX,X(NOSC)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I,ID
C
C     + + + INTRINSICS + + +
C     INTRINSIC IFIX
C
C     + + + END SPECIFICATIONS + + +
C
      IF(PX.GT.X(1))THEN
        I=0
    1   I=I+1
          IF(I.GT.NXSEC) GO TO 2
            ID=IFIX(10.0*(X(I)-PX))
        IF(ID.LE.0)GO TO 1
    2   MX=I-1
      ELSE
        MX=1
      END IF
      IF(MX.GE.NXSEC)MX=NXSEC-1
C
      RETURN
      END
C
C
C
      SUBROUTINE FVOL(AO,A1,A2,KL,NS,NXSEC,F,PX,TF,V,X,XL,XR,QS,QL)
C
C     + + + PURPOSE + + +
C     This subroutine finds the volume of water in a subreach
C     bounded by XL and XR as well as find the range of Q's
C
C     + + + PARAMETERS + + +
      INCLUDE 'params.inc'
C
C     + + + PARAMETER DEFINITIONS + + +
C     NOSC   - Maximum number of cross sections (nodes) allowed in brach
C     NOSH   - Maximum number of shocks allowed in branch
C              (NOSH should be at least 4 times NOSC)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER KL,NS,NXSEC
      REAL AO(NOSC),A1(NOSC),A2(NOSC),F(NOSH),PX(NOSH),QL,QS,TF(NOSC),
     #     V,X(NOSC),XL,XR
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   K, MX
      REAL      A, TPT, TXL, TXR
C
C     + + + EXTERNALS + + +
      EXTERNAL   FMX, FTPT
C
C     + + + END SPECIFICATIONS + + +
C
      TXL=XL
      V=0.0
      K=KL
      TPT=F(K)
      CALL FMX(MX,NXSEC,PX(K),X)
      CALL FTPT(MX,NXSEC,TPT,TF,X,XL)
      CALL FMX(MX,NXSEC,XL,X)
      QS=TPT
      QL=TPT
   10   IF(TPT.GT.0.0)THEN
          A=AO(MX)+A1(MX)*(TPT**A2(MX))
        ELSE
          A=AO(MX)
        END IF
        IF(TPT.LT.QS)QS=TPT
        IF(TPT.GT.QL)QL=TPT
        TXR=XR+X(NXSEC)
        IF(K.LT.NS)TXR=PX(K+1)
        IF(TXR.GE.X(MX+1))THEN
          TXR=X(MX+1)
          TPT=TPT+TF(MX+1)
          MX=MX+1
          IF(MX.GE.NXSEC)TXR=XR
        ELSE
          K=K+1
          TPT=F(K)
          TXR=PX(K)
        END IF
        IF(TXR.GE.XR)TXR=XR
        V=V+A*(TXR-TXL)
        TXL=TXR
      IF(TXR.LT.XR)GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE FC(AO,C,A1,A2,K,NS,NXSEC,F,PX,TF,X,QI)
C
C     + + + PURPOSE + + +
C     Compute the wave celerity for shock K.
C
C     + + + PARAMETERS + + +
      INCLUDE 'params.inc'
C     NOSC   - Maximum number of cross sections (nodes) allowed in branch
C     NOSH   - Maximum number of shocks allowed in branch
C              (NOSH should be at least 4 times NOSC)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   K,NS,NXSEC
      REAL      AO(NOSC),A1(NOSC),A2(NOSC),C,F(NOSH),PX(NOSH),
     #          TF(NOSC),X(NOSC)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   MX,MXU
      REAL      AD,AU,COF,QM,TPT,XR
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   FMX, FTPT
C
C     + + + END SPECIFICATIONS + + +
C
      C=0.0
      CALL FMX (MX,NXSEC,PX(K),X)
      AU=AO(MX)
      CALL FMX (MXU,NXSEC,PX(K-1),X)
      AD=AO(MX)
      TPT=F(K-1)
      XR=PX(K)
      COF=ABS(PX(K)-X(MX))
      IF(COF.LT.0.1)XR=X(MX)+0.1
      CALL FTPT(MXU,NXSEC,TPT,TF,X,XR)
      IF(TPT.LE.0.0)TPT=0.0
      QM=(TPT+F(K))/2.0
      IF(QM.GT.QI)C=(QM**(1.0-A2(MX)))/(A2(MX)*A1(MX))
      IF(TPT.GT.QI)AU=AO(MX)+A1(MX)*(TPT**A2(MX))
      IF(F(K).GT.QI)AD=AO(MX)+A1(MX)*(F(K)**A2(MX))
      COF=ABS(AU-AD)
      IF(COF.GT.0.1)COF=ABS((AU-AD)/QM)
      IF(COF.GT.0.01)C=(TPT-F(K))/(AU-AD)
C
      RETURN
      END
C
C
C
      SUBROUTINE FTPT (MX,NXSEC,TPT,TF,X,XL)
C
C     + + + PURPOSE + + +
C     Find the flow (TPT) at XL in shock affected
C     by tributary inflow between X(MX) and XL.
C
C     + + + PARAMETERS + + +
      INCLUDE 'params.inc'
C     NOSC   - Maximum number of cross sections (nodes) allowed in branch
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MX,NXSEC
      REAL      TPT,TF(NOSC),X(NOSC),XL
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, I1, I2
C
C     + + + END SPECIFICATIONS + + +
C
      I1=MX+1
      I2=0
    1 I2=I2+1
        IF(I2.GE.NXSEC)GO TO 2
      IF(XL.GE.X(I2))GO TO 1
    2 I2=I2-1
      IF(I2.LT.I1)GO TO 4
        DO 3 I=I1,I2
          IF(TPT.LE.0.0)TPT=0.0
          TPT=TPT+TF(I)
    3   CONTINUE
    4 CONTINUE
C
      RETURN
      END
