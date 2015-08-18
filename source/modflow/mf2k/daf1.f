      SUBROUTINE GWF1DAF1ALP(IERR,LUFLW,LUGW,LUIN,LUOT,IDAFCB,IDAFBK)
C
C     + + + + + + + + + + + + + PURPOSE  + + + + + + + + + + + + + + + +
C     Read input and do preliminary computations
C
C     + + +  + + + + + + + + + + + PARAMETERS  + + + + + + + + + + + + +
      INCLUDE 'params.inc'
C     NOBR   - Maximum number of branches allowed in model
C     NOSC   - Maximum number of cross sections (nodes) allowed in branch
C     NOTS   - Maximum of time steps per ground water step
C
C     + + + + + + + + + + + COMMONS  + + + + + + + + + + + + + + + + + +
      INCLUDE 'startdaf.com'
      INCLUDE 'ground.com'
C
C     + + + + + + + + + + + COMMON DEFINTIONS (startdaf.com) + + + + + +
C     IDBG,NBRCH NXSEC(N) VIN(I,N)
C
C     +  + + + + + + + + + + COMMON DEFINTIONS  (ground.com) + + + + + +
C     AQGW(I,N,J) BC(I,N,J) BEL(I,N) BTH(I,N) CND(I,N) NCL(I,N) NLY(I,N)
C     NRW(I,N) VGW(I,N,J)
C
C     + + + + + + + + + + + + LOCAL VARIABLES  + + + + + + + + + + + + +
      INTEGER    I,IDAFBK,IDAFCB,IERR,II,INX,J,LUFLW,LUIN,LUGW,LUOT,N,NN
      CHARACTER*80 TITLE
      LOGICAL OPTEST
C
C     + + + + + + + + + + + LOCAL DEFINITIONS  + + + + + + + + + + + + +
C     IDAFBK   Central vs backward differencing flag for MODFLOW
C     IDAFCB   Print code for MODFLOW
C     IERR     Error code (0=ok, 20<stop as gracefully as you can)
C     TITLE    Title of program (80 characters max)
C
C     + + + + + + + + + + + + INTRINSICS + + + + + + + + + + + + + + + +
C     + + + + + + + + + + + EXTERNALS  + + + + + + + + + + + + + + + + +
      EXTERNAL STARTDAF
C     + + + + + + + + + + + INPUT FORMATS  + + + + + + + + + + + + + + +
 1000 FORMAT (A)
C     + + + + + + + + + + + + + + OUTPUT FORMATS + + + + + + + + + + + +
 2000 FORMAT(1X,A)
 2010 FORMAT(2I6,2F10.2,G15.3,3I6)
 2020 FORMAT(' Something wrong in subroutine GWF1DAF1AL')
 2030 FORMAT(1X,'The DAFLOW ground-water file (file type DAFG) was',/
     1       1X,'not included in the name file')
 2040 FORMAT(1X,'The DAFLOW output flow file was not included in',/
     1       1X,'the name file.  This file must be specified as type',/
     2       1X,'DATA and the unit must be one greater than the',/
     3       1X,'DAFG file.')
C
C     + + + + + + + + + + + END SPECIFICATIONS + + + + + + + + + + + + +
C
C     ***************** zero arrays and preliminaries *******************
      IF(LUFLW.LE.0) THEN
        WRITE(LUOT,2040)
        CALL USTOP(' ')
      END IF
      INQUIRE(UNIT=LUFLW,OPENED=OPTEST)
      IF(.NOT.OPTEST) THEN
         WRITE(LUOT,2040)
         CALL USTOP(' ')
      END IF
      IERR=0
      DO 40 N=1,NOBR
        DO 40 I=1,NOSC
          BEL(I,N)=0.0
          BTH(I,N)=0.0
          CND(I,N)=0.0
          NCL(I,N)=0
          NLY(I,N)=0
          NRW(I,N)=0
          DO 40 J=1,NOTS
            BC(I,N,J)=0.0
            AQGW(I,N,J)=0.0
            VGW(I,N,J)=0.0
   40 CONTINUE
C
C     ********************** Initialize DAFLOW *************************
      CALL STARTDAF (IERR,LUFLW,LUIN,LUOT)
      IF(IERR.GT.20)GO TO 999
C
C     ******************** read ground water input *********************
      IF(LUGW.LE.0) THEN
         WRITE(LUOT,2030)
         CALL USTOP(' ')
      END IF
      READ(LUGW,1000) TITLE
      WRITE(LUOT,2000) TITLE
      READ(LUGW,1000) TITLE
      WRITE(LUOT,2000) TITLE
      READ(LUGW,1000) TITLE
      WRITE(LUOT,2000) TITLE
C
C     ************* read data for each branch and subreach *************
      DO 60 NN=1,NBRCH
        INX=NXSEC(NN)-1
        VGW(1,NN,1)=VIN(1,NN)
        DO 50 II=2,INX
           VGW(II,NN,1)=VIN(II,NN)
           READ(LUGW,*)N,I,BEL(I,N),BTH(I,N),CND(I,N),NLY(I,N),
     #                NRW(I,N),NCL(I,N)
          WRITE(LUOT,2010)N,I,BEL(I,N),BTH(I,N),CND(I,N),NLY(I,N),
     #                    NRW(I,N),NCL(I,N)
   50   CONTINUE
   60 CONTINUE
      IDAFCB=0
      IDBG=0
      IDAFBK=0
      READ(LUGW,1000) TITLE
      READ(LUGW,*,END=70) IDAFCB,IDBG,IDAFBK
   70 IF(IDAFCB.LT.0) WRITE(LUOT,*)
     1   ' CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL NOT 0'
      IF(IDAFCB.GT.0) WRITE(LUOT,*)
     1   ' CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT',IDAFCB
      IF(IDBG.NE.1) WRITE(LUOT,*) ' DAF debugging is turned off'
      IF(IDBG.EQ.1) WRITE(LUOT,*) ' DAF debugging is turned on'
      IF(IDAFBK.EQ.0) WRITE(LUOT,*)
     1   ' DAFLOW is using central differencing for ground-water head'
      IF(IDAFBK.NE.0) WRITE(LUOT,*)
     1   ' DAFLOW is using backward differencing for ground-water head'
  999 CLOSE(LUGW)
      IF(IERR.GT.20)THEN
        WRITE(LUOT,2020)
        CALL USTOP(' ')
      END IF
      RETURN
      END
C
      SUBROUTINE GWF1DAF1AD(DELT,IERR,ITMUNI,LUIN,LUOT)
C
C     + + + + + + + + + + + + + PURPOSE  + + + + + + + + + + + + + + + +
C     Compute NHRR, set VIN for repeated cycles, and
C     read boundary conditions for NHRR time steps
C
C     + + +  + + + + + + + + + + + PARAMETERS  + + + + + + + + + + + + +
      INCLUDE 'params.inc'
C
C     + + + + + + + + + + + COMMONS  + + + + + + + + + + + + + + + + + +
      INCLUDE 'startdaf.com'
      INCLUDE 'ground.com'
C
C     + + + + + + + + + + ++ COMMON VARIABLES (startdaf.com) + + + + + +
C     DT F(K,N) FI(K,N) JTS NBRCH NHRR NS(N) NSI(N) NXSEC(N)
C     PX(K,N) PXI(K,N) TF(I,N) TFI(I,N) TIME TRB(I,N) VIN(I,N)
C
C     + + + + + + + + + + + + + COMMON VARIABLES (ground.com)+ + + + + +
C     BC(I,N,J) VGW(I,N)
C
C     + + + + + + + + + + + + LOCAL VARIABLES  + + + + + + + + + + + + +
      INTEGER I,IERR,INX,ITMUNI,J,K,LUIN,LUOT,N
      REAL    AA,DELT
C
C     + + + + + + + + + + + LOCAL DEFINITIONS  + + + + + + + + + + + + +
C     DELT     Unitless time step size in MODFLOW
C     IERR     Error code (0=ok, 20<stop as gracefully as you can)
C     ITMUNI   Code for units of DELT (1=sec,2=min,3=hr,4=day,5=year)
C
C     + + + + + + + + + + + + INTRINSICS + + + + + + + + + + + + + + + +
      INTRINSIC  ABS, FLOAT, IFIX
C
C     + + + + + + + + + + + EXTERNALS  + + + + + + + + + + + + + + + + +
C     EXTERNAL GETBC
C
C     + + + + + + + + + + + +  INPUT FORMATS + + + + + + + + + + + + + +
C     + + + + + + + + + + + + + + OUTPUT FORMATS + + + + + + + + + + + +
 2000   FORMAT(' Something wrong in subroutine GWF1DAF1AD for time',I5)
C      + + + + + + + + + + + END SPECIFICATIONS  + + + + + + + + + + + +
C
C     *********************  Set initial conditions  *******************
       DO 20 N=1,NBRCH
        NSI(N)=NS(N)
        INX=NXSEC(N)-1
        DO 10 I=1,INX
          TFI(I,N)=TF(I,N)
          VIN(I,N)=VGW(I,N,NHRR)
          BC(I,N,1)=BC(I,N,NHRR)
   10   CONTINUE
        DO 20 K=1,NS(N)
          FI(K,N)=F(K,N)
          PXI(K,N)=PX(K,N)
   20  CONTINUE
C    ***  Compute number of daflow time steps per MODFLOW time step ****
      AA=0.0
      IF(ITMUNI.EQ.1)AA=DELT/(3600.0*DT)
      IF(ITMUNI.EQ.2)AA=DELT/(60.0*DT)
      IF(ITMUNI.EQ.3)AA=DELT/DT
      IF(ITMUNI.EQ.4)AA=DELT*24.0/DT
      IF(ITMUNI.EQ.5)AA=DELT*24.*365.0/DT
      NHRR=IFIX(AA)
      AA=ABS(AA-FLOAT(NHRR))
      IF(AA.GT.0.01)THEN
        WRITE(LUOT,*)' MODFLOW time step is not an even multiple of',
     #               ' the daflow time step.'
        IERR=22
      END IF
      WRITE(LUOT,*)' No of DAFLOW steps per MODFLOW step = ',NHRR
      DO 50 J=1,NHRR
        DO 30 N=1,NBRCH
          DO 30 I=1,NXSEC(N)
            IF(J.GT.1) BC(I,N,J)=BC(I,N,J-1)
            TRB(I,N)=BC(I,N,J)
   30   CONTINUE
        CALL GETBC (IERR,J,LUIN,LUOT)
        DO 50 N=1,NBRCH
          INX=NXSEC(N)-1
          DO 50 I=1,INX
            BC(I,N,J)=TRB(I,N)
   50 CONTINUE
      IF(IERR.GT.20)THEN
        I=IFIX(TIME/DT+0.501)-JTS+1
        WRITE(LUOT,2000)I
        CALL USTOP(' ')
      END IF
      RETURN
      END
C
      SUBROUTINE GWF1DAF1FM (IERR,ITMUNI,HNEW,HOLD,LUOT,
     1        IBOUND,HCOF,RHS,NCOL,NROW,NLAY,KITER,IDAFBK)
C
C     + + + + + + + + + + + + + PURPOSE  + + + + + + + + + + + + + + + +
C     *****This subroutine solves daflow for NHRR time steps and computes
C     the ground water exchange.
C     + + + + + + + + + + + + PARAMETERS + + + + + + + + + + + + + + + +
      INCLUDE 'params.inc'
C     NOBR   - Maximum number of branches allowed in model
C
C     + + + + + + + + + + + + + COMMONS  + + + + + + + + + + + + + + + +
      INCLUDE 'startdaf.com'
      INCLUDE 'ground.com'
C
C     + + + + + + + + COMMON DEFINTIONS  (startdaf.com)  + + + + + + + +
C     AQ(I,N) NBRCH NHRR NXSEC(N) TIME TRB(I,N) V(I,N) VIN(I,N)
C
C     + + + + + + + + COMMON DEFINTIONS  (ground.com)  + + + + + + + + +
C     AQGW(I,N,J) BC(I,N,J) CCSTR(I,N) NCL(I,N) NLY(I,N) NRW(I,N)
C     RHSSTR(I,N) SEP(I,N) SSEP(I,N) VGW(I,N)
C
C     + + + + + + + + + + LOCAL VARIABLES  + + + + + + + + + + + + + + +
      INTEGER     I,IERR,INX,ITMUNI,J,JCD(NOBR),JN,K,L,LUOT,N,
     #            NCD(NOBR),NN
      INTEGER IDAFBK,NCOL,NROW,NLAY,IBOUND(NCOL,NROW,NLAY),KITER
      DOUBLE PRECISION HNEW(NCOL,NROW,NLAY)
      REAL HCOF(NCOL,NROW,NLAY),RHS(NCOL,NROW,NLAY)
      REAL AA,HOLD(NCOL,NROW,NLAY),VO
C
C     + + + + + + + + + + + LOCAL DEFINITIONS  + + + + + + + + + + + + +
C     HNEW(M,N,L) Ground water head at end of time step in cell M,N,L
C     HOLD(M,N,L) Ground water head at start of time step in cell M,N,L
C     IDAFBK   Central vs backward differencing flag for MODFLOW
C     IERR     Error code IERR<20 for warning, IERR>20 fatal
C              (1=DL too large, 21=too many shocks)
C     ITMUNI   Code for units of DELT (1=sec,2=min,3=hr,4=day,5=year)
C     JCD(M)   Code for junction mixing (0=mixed, 1=not known)
C     NCD(N)   Branch code (0=routed, 1=not routed)
C     NCOL     Number of columns in ground-water model
C     NLAY     Number of layers in ground-water model
C     NROW     Number of rows in ground-water model
C     VO       Volume in subreach at beginning of time step
C
C     + + + + + + + + + + + + + INTRINSICS + + + + + + + + + + + + + + +
      INTRINSIC FLOAT,IFIX
C     + + + + + + + + + + + + + EXTERNALS  + + + + + + + + + + + + + + +
      EXTERNAL PRERTE, SETJNVL, SEEP, RTBR, LIMSEEP, FGQ, SETJV2
C
C     + + + + + + + + + + + + + INPUT FORMATS  + + + + + + + + + + + + +
C
C     + + + + + + + + + + + + + OUTPUT FORMATS + + + + + + + + + + + + +
 2000 FORMAT(' Warning DL>C DT, Increase DT to smooth.')
 2010 FORMAT(' Something wrong in subroutine GWF1DAF1FM for time',I5)
 2020 FORMAT(' Too many waves in branch',I3,' at J=',I5)
C
C     + + + + + + + + + + + + END SPECIFICATIONS + + + + + + + + + + + +
C
C     ********************* Preliminaries ******************************
      CALL PRERTE
      DO 10 N=1,NBRCH
        DO 10 I=1,NXSEC(N)
          SEP(I,N)=0.0
          SSEP(I,N)=0.0
          CCSTR(I,N)=0.0
          RHSSTR(I,N)=0.0
   10   CONTINUE
C     ************************ Start time loop *************************
      IERRR=0
      DO 300 J=1,NHRR
        CALL SETJNVL (JCD,NCD)
        CALL SEEP (HNEW,HOLD,J,LUOT,IBOUND,NCOL,NROW,NLAY,KITER,
     1             IDAFBK)
        IF(IERR.GT.20)GO TO 900
C       ************* Add seepage to trib flow *************************
        DO 20 N=1,NBRCH
          DO 20 I=1,NXSEC(N)
            TRB(I,N)=BC(I,N,J)+SEP(I,N)
   20   CONTINUE
C
C       ************************* Route branches ***********************
        DO 200 NN=1,NBRCH
          CALL RTBR (IERR,LUOT,J,JCD,JN,N,NCD)
          IF(IERR.EQ.1)IERRR=1
          IF(IERR.GT.20)GO TO 900
          AQGW(1,N,J)=AQ(1,N)
          DO 100 I=1,NXSEC(N)
            IF(I.LT.NXSEC(N))THEN
              IF(I.GT.1. AND. NLY(I,N).GT.0 )CALL LIMSEEP (HOLD,I,
     1                    J,LUOT,N,NCOL,NROW,NLAY,IDAFBK)
              IF(J.EQ.1)THEN
                VO=VIN(I,N)
              ELSE
                VO=VGW(I,N,J-1)
              END IF
              CALL FGQ (I,J,LUOT,N,VO)
            END IF
            AQGW(I,N,J)=AQ(I,N)
            VGW(I,N,J)=V(I,N)
  100     CONTINUE
          CALL SETJV2 (JCD,JN,NCD)
  200   CONTINUE
  300 CONTINUE
C     ************************ End of time loop ************************
C
      IF(IERRR.EQ.1)WRITE(LUOT,2000)
      AA=1.0
      IF(ITMUNI.EQ.2)AA=60.0
      IF(ITMUNI.EQ.3)AA=3600.0
      IF(ITMUNI.EQ.4)AA=86400.0
      IF(ITMUNI.EQ.5)AA=31536000.0
      DO 400 N=1,NBRCH
        INX=NXSEC(N)-1
        DO 400 I=2,INX
          L=NLY(I,N)
          IF(L.GT.0) THEN
            K=NRW(I,N)
            NN=NCL(I,N)
            IF(IBOUND(NN,K,L).GT.0) THEN
              RHS(NN,K,L)=RHS(NN,K,L)+AA*RHSSTR(I,N)/FLOAT(NHRR)
              HCOF(NN,K,L)=HCOF(NN,K,L)-AA*CCSTR(I,N)/FLOAT(NHRR)
            END IF
          END IF
  400 CONTINUE
  900 CONTINUE
C       ************* Nasty error comes to here ************************
        IF(IERR.GT.20)THEN
          I=IFIX(TIME/DT+0.501)-JTS+1
          WRITE(LUOT,2010)I
          IF(IERR.EQ.21)WRITE(LUOT,2020)N,I
          CALL USTOP(' ')
        END IF
      RETURN
      END
C
      SUBROUTINE SEEP (HNEW,HOLD,J,LUOT,IBOUND,NCOL,NROW,NLAY,
     1           KITER,IDAFBK)
C
C     + + + + + + + + + + + + + PURPOSE  + + + + + + + + + + + + + + + +
C     *****This subroutine computes the seepage to the channel
C     + + + + + + + + + + + + PARAMETERS + + + + + + + + + + + + + + + +
      INCLUDE 'params.inc'
C     + + + + + + + + + + + + + COMMONS  + + + + + + + + + + + + + + + +
      INCLUDE 'startdaf.com'
      INCLUDE 'ground.com'
C
C     + + + + + + + + COMMON DEFINTIONS  (startdaf.com)  + + + + + + + +
C     AO(I,N) A1(I,N) A2(I,N) IDBG NBRCH NHRR NXSEC(N) VIN(I,N)
C     W1(I,N) W2(I,N) X(I,N)
C
C     + + + + + + + + COMMON DEFINTIONS  (ground.com)  + + + + + + + + +
C     BEL(I,N) BTH(I,N) CND(I,N) CSTR(I,N) NCL(I,N) NLY(I,N)
C     NRW(I,N) QSTR(I,N) SEP(I,N) STAGE(I,N) VGW(I,N)
C
C     + + + + + + + + + + LOCAL VARIABLES  + + + + + + + + + + + + + + +
      INTEGER I,IDAFBK,INX,J,KITER,LUOT,N
      INTEGER NCOL,NROW,NLAY,IBOUND(NCOL,NROW,NLAY)
      DOUBLE PRECISION HNEW(NCOL,NROW,NLAY)
      REAL A,AA,AR,DPT,HD,HOLD(NCOL,NROW,NLAY),Q,W
C
C     + + + + + + + + + + + LOCAL DEFINITIONS  + + + + + + + + + + + + +
C     A        Cross sectional area of flow in subreach
C     AR       Surface area of water in subreach
C     DPT      Depth of flow (A/W)
C     HNEW(M,N,L) Ground water head at end of time ste in cell M,N,L
C     HD       Head, either water surface elev or seepage head
C     HOLD(M,N,L) Ground water head at start of time step in cell M,N,L
C     IDAFBK   Central vs backward differencing flag for MODFLOW
C     J        Time step
C     W        Top with of water surface in subreach
C
C     + + + + + + + + + + + + + INTRINSICS + + + + + + + + + + + + + + +
C     INTRINSIC  FLOAT
C
C     + + + + + + + + + + + + + EXTERNALS  + + + + + + + + + + + + + + +
C
C     + + + + + + + + + + + + + INPUT FORMATS  + + + + + + + + + + + + +
C
C     + + + + + + + + + + + + + OUTPUT FORMATS + + + + + + + + + + + + +
 2000 FORMAT(' For time step ',I5,', branch ',I3,
     #       ' The requested Seepage =')
 2010 FORMAT(10G11.4)
C
C     + + + + + + + + + + + + END SPECIFICATIONS + + + + + + + + + + + +
C
C     ******************** Compute seepage terms ***********************
      DO 20 N=1,NBRCH
        INX=NXSEC(N)-2
        DO 10 I=1,INX
          SEP(I+1,N)=0.
          QSTR(I+1,N)=0.
          STAGE(I+1,N)=0.
          CSTR(I+1,N)=0.
          IF(NLY(I+1,N).GT.0) THEN
            IF(IBOUND(NCL(I+1,N),NRW(I+1,N),NLY(I+1,N)).NE.0) THEN
C             ******** MODFLOW cell is active **************************
              IF(KITER.EQ.1)THEN
                IF(J.EQ.1) THEN
                  A=VIN(I,N)/(X(I+1,N)-X(I,N))
                ELSE
                  A=VGW(I,N,J-1)/(X(I+1,N)-X(I,N))
                END IF
              ELSE
                A=VGW(I,N,J)/(X(I+1,N)-X(I,N))
              END IF
              IF(A.GT.AO(I,N)) THEN
                Q=((A-AO(I,N))/A1(I,N))**(1.0/A2(I,N))
              ELSE
                Q=0.0
              END IF
              IF(Q.GT.0.0) THEN
                W=W1(I,N)*(Q**W2(I,N))
                DPT=A/W
              ELSE
                W=0.0
                DPT=0.0
              END IF
              AR=W*(X(I+1,N)-X(I,N))
              IF(IDAFBK.EQ.0) THEN
                HD=HOLD(NCL(I+1,N),NRW(I+1,N),NLY(I+1,N))
              ELSE
                HD=HNEW(NCL(I+1,N),NRW(I+1,N),NLY(I+1,N))
              END IF
              AA=HNEW(NCL(I+1,N),NRW(I+1,N),NLY(I+1,N))
              HD=HD+(FLOAT(J)-0.5)*(AA-HD)/FLOAT(NHRR)
              HD=HD-BEL(I+1,N)-DPT
              AA=DPT+BTH(I+1,N)+HD
C              IF(IDBG.EQ.1) WRITE(LUOT,*) ' I,DPT,DPT+BTH+HD',
C     1                                    I+1,DPT,AA
              IF(AA.LT.0.0) THEN
C               * GW head does not have hydraulic connection to stream *
                IF(DPT.GT.0) THEN
                  HD=-BTH(I+1,N)-DPT
                  SEP(I+1,N)=CND(I+1,N)*HD*AR/BTH(I+1,N)
                ELSE
                  SEP(I+1,N)=0.0
                END IF
                QSTR(I+1,N)=SEP(I+1,N)
                STAGE(I+1,N)=0.
                CSTR(I+1,N)=0.
              ELSE
C               ** GW head  has hydraulic connection too stream ********
                IF(DPT.LE.0.0 .AND. HD.LE.0.0) THEN
                  SEP(I+1,N)=0.0
                  STAGE(I+1,N)=0.0
                  CSTR(I+1,N)=0.0
                ELSE
                  IF(HD.GT.0.0) THEN
                    AA=2.0*W2(I,N)
                    IF(A2(I,N).GT.AA)THEN
C                   ********* Width depth ratio decreases with Q *******
                      AA=A2(I,N)-W2(I,N)
                      AA=((HD+DPT)*W1(I,N)/A1(I,N))**(1.0/AA)
                      AR=(W1(I,N)*AA**W2(I,N))*(X(I+1,N)-X(I,N))
                      IF(IDBG.EQ.1) WRITE(LUOT,*)
     1      'Width, Area based on gw head',W1(I,N)*AA**W2(I,N),AR
                    ELSE
C                   ******** Width depth ratio increases with Q ********
                      AR=(W+2.0*HD)*(X(I+1,N)-X(I,N))
                      IF(IDBG.EQ.1) WRITE(LUOT,*)
     1     'Width, Area based on GW head',W+2.0*HD,AR
                    END IF
                  END IF
                  CSTR(I+1,N)=CND(I+1,N)*AR/BTH(I+1,N)
                  SEP(I+1,N)=CSTR(I+1,N)*HD
                  STAGE(I+1,N)=BEL(I+1,N)+DPT
                END IF
                QSTR(I+1,N)=0.0
              END IF
            END IF
          END IF
C          IF(IDBG.EQ.1) WRITE(LUOT,*) ' I,CSTR,STAGE,QSTR,SEP',
C     1             I+1,CSTR(I+1,N),STAGE(I+1,N),QSTR(I+1,N),SEP(I+1,N)
   10   CONTINUE
        IF(IDBG.EQ.1)WRITE(LUOT,2000)J,N
        INX=NXSEC(N)-1
        IF(IDBG.EQ.1)WRITE(LUOT,2010)(SEP(I,N),I=2,INX)
   20 CONTINUE
C
      RETURN
      END
C
      SUBROUTINE LIMSEEP (HOLD,I,J,LUOT,N,NCOL,NROW,NLAY,IDAFBK)
C
C     + + + + + + + + + + + + + PURPOSE  + + + + + + + + + + + + + + + +
C     *** This subroutine apportions any increase in TRB (reduction in
C     *** outflow from the stream) that is calculated by ROUTE between
C     *** BC and SEP.  Negative BC values are increased to 0 prior to
C     *** increasing negative SEP values.  Coefficients SSEP, RHSSTR,
C     *** & CCSTR  are also set.
C     + + + + + + + + + + + + PARAMETERS + + + + + + + + + + + + + + + +
      INCLUDE 'params.inc'
C     + + + + + + + + + + + + + COMMONS  + + + + + + + + + + + + + + + +
      INCLUDE 'startdaf.com'
      INCLUDE 'ground.com'
C
C     + + + + + + + + COMMON DEFINTIONS  (startdaf.com)  + + + + + + + +
C     DT JTS QI IDBG NHRR TIME TRB(I,N)
C
C     + + + + + + + + COMMON DEFINTIONS  (ground.com)  + + + + + + + + +
C     BC(I,N,J) CCSTR(I,N)  CSTR(I,N) NCL(I,N) NLY(I,N) NRW(I,N)
C     QSTR(I,N) RHSSTR(I,N) SEP(I,N) SSEP(I,N) STAGE(I,N)
C
C     + + + + + + + + + + LOCAL VARIABLES  + + + + + + + + + + + + + + +
      INTEGER I,IDAFBK,J,JJ,LUOT,N,NCOL,NROW,NLAY
      REAL    AA,BCNEW,HOLD(NCOL,NROW,NLAY),TRBOLD,TRBCHG
C
C     + + + + + + + + + + + LOCAL DEFINITIONS  + + + + + + + + + + + + +
C     BCNEW     - Adjusted value of withdrawal caused by lack of water
C     HOLD(M,N,L) Ground water head at start of time step in cell M,N,L
C     IDAFBK    - Central vs backward differencing flag for MODFLOW
C     N         - Branch number
C     NCOL      - Number of columns in ground-water model
C     NLAY      - Number of layers in ground-water model
C     NROW      - Number of rows in ground-water model
C     J         - Time step number
C     TRBOLD    - Value of TRB that was sent to ROUTE (BC+SEP).
C     TRBCHG    - Reduction necessary in the extraction
C
C     + + + + + + + + + + + + + INTRINSICS + + + + + + + + + + + + + + +
      INTRINSIC FLOAT,IFIX
C     + + + + + + + + + + + + + EXTERNALS  + + + + + + + + + + + + + + +
C     + + + + + + + + + + + + + INPUT FORMATS  + + + + + + + + + + + + +
C     + + + + + + + + + + + + + OUTPUT FORMATS + + + + + + + + + + + + +
 4030 FORMAT(' Channel dry, reduced BC withdrawal to',G12.3,
     $        ' at time step',I6,' Branch',I4,' Node',I4)
 4040 FORMAT(' Channel dry, limited GW seepage to',G12.3,
     $        ' at time step',I6,' Branch',I4,' Node',I4)
C     + + + + + + + + + + + + END SPECIFICATIONS + + + + + + + + + + + +
C
      JJ=IFIX(TIME/DT+0.501) -JTS +1
      TRBOLD=SEP(I,N)+BC(I,N,J)
C     *** ROUTE will have modified TRB only when TRBOLD is negative. ***
      IF(TRBOLD.LT.0.0) THEN
        TRBCHG=TRB(I,N)-TRBOLD
C       * Apportion change in TRB only if there was a significant change
        IF(TRBCHG.GT. -1.E-6*TRBOLD) THEN
          IF(BC(I,N,J).LT.0) THEN
            IF(-BC(I,N,J).GE.TRBCHG) THEN
C             ********* The entire TRB change is taken from BC *********
              BCNEW=BC(I,N,J)+TRBCHG
              IF(IDBG.EQ.1) WRITE(LUOT,4030) BCNEW,J,N,I
            ELSE
C             ** Part of TRB change is taken from SEP and part from BC *
              IF(IDBG.EQ.1) WRITE(LUOT,4030) 0.,J,N,I
              SEP(I,N)=SEP(I,N)+BC(I,N,J)+TRBCHG
              QSTR(I,N)=SEP(I,N)
              CSTR(I,N)=0.0
              STAGE(I,N)=0.0
              IF(IDBG.EQ.1) WRITE(LUOT,4040) SEP(I,N),JJ,N,I
            END IF
          ELSE
C           ********* The entire TRB change is taken from SEP **********
            SEP(I,N)=SEP(I,N)+TRBCHG
            QSTR(I,N)=SEP(I,N)
            CSTR(I,N)=0.0
            STAGE(I,N)=0.0
             JJ=IFIX(TIME/DT+0.501)-JTS+1
            IF(IDBG.EQ.1) WRITE(LUOT,4040) SEP(I,N),JJ,N,I
          END IF
        END IF
      END IF
C
C     ************** Accumulate SSEP, RHSSTR, and CCSTR ****************
      IF(IDAFBK.EQ.0) THEN
        AA=(FLOAT(J)-.5)/FLOAT(NHRR)
      ELSE
        AA=1.0
      END IF
      SSEP(I,N)=SSEP(I,N)+SEP(I,N)
      RHSSTR(I,N)=RHSSTR(I,N)
     1               +CSTR(I,N)*HOLD(NCL(I,N),NRW(I,N),NLY(I,N))*(1.-AA)
     2               -STAGE(I,N)*CSTR(I,N)+QSTR(I,N)
      CCSTR(I,N)=CCSTR(I,N)+CSTR(I,N)*AA
      IF(IDBG.EQ.1)THEN
        IF(N.EQ.1) WRITE(LUOT,*)'Final I, SEP ',I,SEP(I,N)
C       WRITE(LUOT,*)'I,SSEP,CCSTR,RHSSTR ',
C    1                                I,SSEP(I,N),CCSTR(I,N),RHSSTR(I,N)
      END IF
      RETURN
      END
C
      SUBROUTINE GWF1DAF1BD (LUFLW,LUOT,ITMUNI,DELT,VBVL,VBNM,MSUM,
     #           KSTP,KPER,IDAFCB,ICBCFL,BUFF,PERTIM,TOTIM,NCOL,NROW,
     #           NLAY,IBOUND)
C
C     + + + + + + + + + + + + + PURPOSE  + + + + + + + + + + + + + + + +
C     *** This subroutine prints the results ***************************
C     + + + + + + + + + + + + PARAMETERS + + + + + + + + + + + + + + + +
      INCLUDE 'params.inc'
C     + + + + + + + + + + + + + COMMONS  + + + + + + + + + + + + + + + +
      INCLUDE 'startdaf.com'
      INCLUDE 'ground.com'
C
C     + + + + + +  COMMON DEFINTIONS  (startdaf.com) + + + + + + + + + +
C     AQ(I,N) NBRCH NHRR NXSEC(N) TRB(I,N) V(I,N)
C
C     + + + + + + + + COMMON DEFINTIONS  (ground.com)  + + + + + + + + +
C     AQGW(I,N,J) BC(I,N,J) NCL(I,N) NRW(I,N) NLY(I,N) SSEP(I,N)
C     VGW(I,N,J)
C
C     + + + + + + + + + + LOCAL VARIABLES  + + + + + + + + + + + + + + +
      INTEGER NCOL,NROW,NLAY,IBOUND(NCOL,NROW,NLAY)
      INTEGER I,IBD,ICBCFL,IDAFCB,INX,ITMUNI,J,KPER,KSTP,LUFLW,LUOT,N
      INTEGER IBDLBL,IC,IL,IR
      REAL AA,DELT,BUFF(NCOL,NROW,NLAY),TOTIM,VBVL(4,MSUM)
      REAL RATE,RIN,ROUT,ZERO
      CHARACTER*16 VBNM(MSUM),TEXT
      DATA TEXT/'          DAFLOW'/
C
C     + + + + + + + + + + + LOCAL DEFINITIONS  + + + + + + + + + + + + +
C     DELT     Unitless time step size in MODFLOW
C     IDAFCB   Print code for MODFLOW
C     ITMUNI   Code for units of DELT (1=sec,2=min,3=hr,4=day,5=year)
C
      DOUBLE PRECISION RATIN,RATOUT,RRATE
C     + + + + + + + + + + + + + INTRINSICS + + + + + + + + + + + + + + +
C     INTRINSIC  FLOAT
C     + + + + + + + + + + + + + EXTERNALS  + + + + + + + + + + + + + + +
C     EXTERNAL PRTFLW, UBDSV2, UBDSVA, UBUDSV
C
C    + + + + + + + + + + + + INPUT FORMATS + + + + + + + + + + + + + + +
C     + + + + + + + + + + + + + OUTPUT FORMATS + + + + + + + + + + + + +
 2000 FORMAT(1X,/1X,A,'   PERIOD',I3,'   STEP',I3)
 2010 FORMAT(1X,'REACH',I4,'   LAYER',I3,'   ROW',I4,'   COL',I4,
     1       '   RATE',1PG15.6)
C     + + + + + + + + + + + + END SPECIFICATIONS + + + + + + + + + + + +
C
C     ************************ Write results  **************************
C
C  Count stream nodes
      NSNODE=0
      DO 10 N=1,NBRCH
        NSNODE=NSNODE+NXSEC(N)-2
  10  CONTINUE
C
      DO 50 J=1,NHRR
        DO 20 N=1,NBRCH
          DO 20 I=1,NXSEC(N)
            AQ(I,N)=AQGW(I,N,J)
            V(I,N)=VGW(I,N,J)
            TRB(I,N)=BC(I,N,J)
   20   CONTINUE
        CALL PRTFLW (LUFLW,LUOT)
   50 CONTINUE
C
C  COMPUTE MODFLOW'S BUDGET TERMS
        AA=1.0
        IF(ITMUNI.EQ.2)AA=60.0
        IF(ITMUNI.EQ.3)AA=3600.0
        IF(ITMUNI.EQ.4)AA=86400.0
        IF(ITMUNI.EQ.5)AA=31536000.0
C
C1------INITIALIZE CELL-BY-CELL FLOW TERM FLAG (IBD) AND
C1------ACCUMULATORS (RATIN AND RATOUT).
      ZERO=0.
      RATIN=ZERO
      RATOUT=ZERO
      IBD=0
      IF(IDAFCB.LT.0 .AND. ICBCFL.NE.0) IBD=-1
      IF(IDAFCB.GT.0) IBD=ICBCFL
      IBDLBL=0
C
C2------IF CELL-BY-CELL FLOWS WILL BE SAVED AS A LIST, WRITE HEADER.
      IF(IBD.EQ.2) CALL UBDSV2(KSTP,KPER,TEXT,IDAFCB,NCOL,NROW,NLAY,
     1          NSNODE,LUOT,DELT,PERTIM,TOTIM,IBOUND)
C
C3------CLEAR THE BUFFER.
      DO 60 IL=1,NLAY
        DO 60 IR=1,NROW
          DO 60 IC=1,NCOL
            BUFF(IC,IR,IL)=ZERO
   60 CONTINUE
C
C4------IF NO REACHES, SKIP FLOW CALCULATIONS.
      IF(NBRCH.EQ.0)GO TO 200
C
C5------LOOP THROUGH EACH RIVER REACH CALCULATING FLOW.
        DO 150 N=1,NBRCH
          INX=NXSEC(N)-1
          DO 150 I=2,INX
            IC=NCL(I,N)
            IR=NRW(I,N)
            IL=NLY(I,N)
            RATE=ZERO
            IF(IBOUND(IC,IR,IL).LE.0.OR.IL.LE.0)GO TO 99
            RATE=-AA*SSEP(I,N)/FLOAT(NHRR)
            RRATE=RATE
C
C5G-----PRINT THE INDIVIDUAL RATES IF REQUESTED(IDAFCB<0).
        IF(IBD.LT.0) THEN
          IF(IBDLBL.EQ.0) WRITE(LUOT,2000) TEXT,KPER,KSTP
          WRITE(LUOT,2010) N,IL,IR,IC,RATE
          IBDLBL=1
        END IF
C
C5H------ADD RATE TO BUFFER.
        BUFF(IC,IR,IL)=BUFF(IC,IR,IL)+RATE
C
C5I-----SEE IF FLOW IS INTO AQUIFER OR INTO RIVER.
        IF(RATE)94,99,96
C
C5J-----AQUIFER IS DISCHARGING TO RIVER SUBTRACT RATE FROM RATOUT.
   94 RATOUT=RATOUT-RRATE
      GO TO 99
C
C5K-----AQUIFER IS RECHARGED FROM RIVER; ADD RATE TO RATIN.
   96 RATIN=RATIN+RRATE
C
C5L-----IF SAVING CELL-BY-CELL FLOWS IN LIST, WRITE FLOW.
   99 IF(IBD.EQ.2) CALL UBDSVA(IDAFCB,NCOL,NROW,IC,IR,IL,RATE,IBOUND,
     1                        NLAY)
  150   CONTINUE
C
C6------IF CELL-BY-CELL FLOW WILL BE SAVED AS A 3-D ARRAY,
C6------CALL UBUDSV TO SAVE THEM.
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,IDAFCB,BUFF,NCOL,NROW,
     1                          NLAY,LUOT)
C
C7------MOVE RATES,VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
  200 RIN=RATIN
      ROUT=RATOUT
      VBVL(3,MSUM)=RIN
      VBVL(4,MSUM)=ROUT
      VBVL(1,MSUM)=VBVL(1,MSUM)+RIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
      VBNM(MSUM)=TEXT
C
C8------INCREMENT BUDGET TERM COUNTER.
      MSUM=MSUM+1
      RETURN
      END
C
