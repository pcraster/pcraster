C     Last change:  ERB   3 Sep 2002    3:03 pm
C=======================================================================
      SUBROUTINE PES1GAU1AP(X,ND,NPE,HOBS,WT,WP,C,SCLE,G,H,DD,DMAX,CSA,
     &                      TOL,IND,IFO,AMP,AP,DMX,IOUT,B1,ITERP,IPRINT,
     &                      LN,MPR,PRM,JMAX,NFIT,R,GD,U,NOPT,XD,S,
     &                      SOSR,NIPR,IPR,BUFF,WTP,NHT,WTQ,IOWTQ,NDMH,
     &                      IOSTAR,NPLIST,MPRAR,IPRAR,NDMHAR,BPRI,RMARM,
     &                      IAP,DMXA,NPAR,AMPA,AMCA,AAP,ITMXP,RMAR,IPNG,
     &                      NPNG,NPNGAR)
C     VERSION 20020708 ERB
C     ******************************************************************
C     REGRESSION BY THE MODIFIED GAUSS-NEWTON METHOD
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL ABDMXU, ADMXN, ADMXP, ADMXU, AMP, AP1, APU, B1, BDMXU,
     &     BPRI, BUFF, CSA, DDC, DET, DMAX, DMX, DMX1, DMXN, DMXO, DMXP,
     &     DMXU, H, HOBS, PRM, SOSR, TMPA, TMPB, W, WP, WT,
     &     WTQ, X, XD
      INTEGER I, IAP, IFO, IND, IOSTAR, IOUT, IOWTQ, IP, IP1, IPM, IPNG,
     &        IPR, IPRINT, ITERP, J, JJN, JJP, JJU, JMAX, LN, LNN, MPR,
     &        N, ND, NDMH, NFIT, NHT, NIPR, NOPT, NPE, NP1, NPNG, NPNGAR
      DOUBLE PRECISION ABDMX, ABDMXN, ABDMXP, AP, APN, APO, APP, BDMX,
     &                 BDMXN, BDMXP, SPR, TOL
      DOUBLE PRECISION C(NPE,NPE), SCLE(NPE), DD(NPE), DTMPA,
     &                 R(NPE*NPE/2+NPE),  S(NPE),
     &                 U(NPE), G, GD, DMXA(ITMXP)
      CHARACTER*1 CTYPE, DTYPE
      DIMENSION X(NPE,ND), HOBS(ND), WT(ND), WP(MPRAR), H(ND),
     &          BPRI(IPRAR), B1(NPLIST), G(NPE), LN(NPLIST),
     &          PRM(NPLIST+1,MPRAR), GD(NPE), XD(NPE,ND), NIPR(IPRAR),
     &          BUFF(MPRAR), NPAR(ITMXP), AMPA(ITMXP), AMCA(ITMXP),
     &          AAP(ITMXP), IPNG(NPNGAR)
      DIMENSION WTQ(NDMHAR,NDMHAR), WTP(IPRAR,IPRAR)
      INCLUDE 'param.inc'
      INCLUDE 'parallel.inc'
C     ------------------------------------------------------------------
  490 FORMAT (/,1X,71('-'),/,
     &        ' PARAMETER VALUES AND STATISTICS FOR ALL PARAMETER-',
     &        'ESTIMATION ITERATIONS',/,
     &        1X,71('-'),
     &        //,5X,'MODIFIED GAUSS-NEWTON CONVERGES IF THE ABSOLUTE',
     &        ' VALUE OF THE MAXIMUM',/,
     &        ' FRACTIONAL PARAMETER CHANGE (MAX CALC. CHANGE) IS',
     &        ' LESS THAN TOL OR IF THE',/,
     &        ' SUM OF SQUARED, WEIGHTED RESIDUALS CHANGES LESS',
     &        ' THAN SOSC OVER TWO',/,
     &        ' PARAMETER-ESTIMATION ITERATIONS.')

  500 FORMAT (/,' SCALED LEAST-SQUARES MATRIX :')
  505 FORMAT (/,' SCALED GRADIENT VECTOR :')
  510 FORMAT (/,' MODIFIED GAUSS-NEWTON PROCEDURE FOR',
     &        ' PARAMETER-ESTIMATION ITERATION NO. = ',I5,/)
  515 FORMAT (' VALUES FROM SOLVING THE NORMAL EQUATION :',/,
     &     2X,' MARQUARDT PARAMETER ------------------- = ',G11.5,/,
     &     2X,' MAX. FRAC. PAR. CHANGE (TOL=',G10.3,') = ',G11.5,/,
     &     2X,'      OCCURRED FOR PARAMETER  "',A,'"',' TYPE ',A,/)
  520 FORMAT (' CALCULATION OF DAMPING PARAMETER',/,
     &     2X,' MAX-CHANGE SPECIFIED: ',G9.2,'   USED: ',G9.2,/,
     &     2X,' OSCILL. CONTROL FACTOR (1, NO EFFECT)-- = ',G11.5,/,
     &     2X,' DAMPING PARAMETER (RANGE 0 TO 1) ------ = ',G11.5,/,
     &     2X,'      CONTROLLED BY PARAMETER "',A,'"',' TYPE ',A,/)
  525 FORMAT (6(3X,A10))
  530 FORMAT (6(2X,1PG11.4))
  535 FORMAT (E10.3,I5,/,3E10.3)
  540 FORMAT (/,
     &' *** PARAMETER ESTIMATION CONVERGED BY SATISFYING THE',
     &' TOL CRITERION ***',/)
  545 FORMAT (' UPDATED ESTIMATES OF REGRESSION PARAMETERS :',/)
  550 FORMAT (/,
     &' ERROR: CALCULATED PARAMETER CHANGE OF ',G12.5,/,
     &' FOR LOG-TRANSFORMED PARAMETER "',A,'" IS TOO LARGE FOR ',/,
     &' CALCULATION OF EXPONENTIAL -- STOP EXECUTION (PES1GAU1AP)')
C
CC PRINT VARIABLE AND ARRAY VALUES FOR DEBUGGING
C      WRITE(IOUT,775)ND,NPE,DMAX,CSA,TOL,IFO,NHT,NDMH,AP,DMX,JMAX,NFIT,
C     &               NOPT,SOSR,IOWTQ,IPR,MPR,IOSTAR,NPLIST
C  775 FORMAT('AT TOP OF PES1GAU1AP, ND NPE DMAX CSA TOL =',/,
C     &2I5,3G16.8,/,
C     &' IFO NHT NDMH AP DMX JMAX NFIT =',/,
C     &3I5,2G16.8,2I5,/
C     &'and NOPT SOSR IOWTQ IPR MPR IOSTAR NPLIST =',/,
C     &I5,G16.8,5I5)
C      WRITE(IOUT,776)(IP,IP=1,NPE)
C  776 FORMAT(50X,'X FOR ESTIMATED PARAMETER NUMBER(S)',/,
C     &2X,'OBS#',6X,'H',15X,'HOBS',12X,'WT',7X,I8,14I16)
C  777 FORMAT(I5,20G16.8)
C      DO 2 I=1,ND
C        WRITE(IOUT,777)I,H(I),HOBS(I),WT(I),(X(IP,I),IP=1,NPE)
C    2 CONTINUE
C      WRITE(IOUT,778)
C  778 FORMAT(' Par#',4X,'LN',8X,'B',14X,'B1',14X,'WP')
C  779 FORMAT(I5,1X,I5,3G16.8)
C  780 FORMAT(I5,38X,1G16.8)
C      DO 3 IP=1,NPLIST
C        WRITE(IOUT,779)IP,LN(IP),B(IP),B1(IP),WP(IP)
C    3 CONTINUE
C      IF (MPR.GT.0) THEN
C        DO 4 I=NPLIST+1,NPLIST+MPR
C          WRITE(IOUT,780)I,WP(I)
C    4   CONTINUE
C      ENDIF
C      WRITE(IOUT,781)
C  781 FORMAT('Est.Par#',7X,'DD')
C  782 FORMAT(I5,5X,G16.8)
C      DO 5 IP=1,NPE
C        WRITE(IOUT,782)IP,DD(IP)
C    5 CONTINUE
C      WRITE(IOUT,783)
C  783 FORMAT('PRM ARRAY:')
C      DO 6 J = 1,MPRAR
C        WRITE(IOUT,784)(PRM(I,J),I=1,NPLIST+1)
C    6 CONTINUE
C  784 FORMAT(10G16.8)
C      WRITE(IOUT,785)
C  785 FORMAT('NIPR ARRAY:')
C      WRITE(IOUT,784)(NIPR(I),I=1,IPRAR)
C      WRITE(IOUT,786)
C  786 FORMAT('WTQ ARRAY:')
C      DO 7 J=1,NDMHAR
C        WRITE(IOUT,784)(WTQ(I,J),I=1,NDMHAR)
C    7 CONTINUE
C
C     IF THIS IS THE FIRST ITERATION, PRINT HEADING
      IF (ITERP.EQ.1) WRITE (IOUT,490)
C
C-------INITIALIZE SOME VARIABLES.
      NP1 = NPE - 1
      AMP = 0.0
      IND = 0
C-------ASSEMBLE LEAST-SQUARES MATRIX (C) AND GRADIENT VECTOR (G)
C----------INITIALIZE C AND CONVERT LN PARAMETERS
      DO 20 IP = 1, NPE
        IIPP = IPPTR(IP)
        DO 10 I = 1, NPE
          C(I,IP) = 0.0
   10   CONTINUE
        IF (LN(IIPP).GT.0) B(IIPP) = LOG(B(IIPP))
        G(IP) = 0.0
   20 CONTINUE
C----------CALCULATE SENSITIVITY CONTRIBUTIONS TO C AND G.
C----------CALCULATE SUM OF SQUARED RESIDUALS.
      DO 50 N = 1, NHT
        TMPA = HOBS(N) - H(N)
        W = WT(N)
        IF (W.LT.0.) THEN
          W = 1.E-20
cc ERB 7-8-02          IF (IFO.EQ.0) WT(N) = -WT(N)
        ENDIF
        DO 40 IP = 1, NPE
          DTMPA = DBLE(W)*DBLE(X(IP,N))
          DO 30 I = IP, NPE
            C(I,IP) = DBLE(X(I,N))*DTMPA + C(I,IP)
   30     CONTINUE
          G(IP) = DTMPA*TMPA + G(IP)
   40   CONTINUE
   50 CONTINUE
      CALL SOBS1BAS6WF(NPE,NHT,NDMH,WTQ,X,C,G,HOBS,H,IOWTQ,NDMHAR,
     &                 ND)
C----------ACCOUNT FOR PRIOR INFORMATION
C-------------ESTIMATES OF PARAMETER SUMS
      IF (MPR.GT.0) THEN
        DO 100 IPM = 1, MPR
          TMPA = 0.
          DO 70 IP = 1, NPLIST
            TMPA = TMPA + PRM(IP,IPM)*B(IP)
   70     CONTINUE
          TMPA = PRM(NPLIST+1,IPM) - TMPA
          DO 90 IP = 1, NPE
            IIPP = IPPTR(IP)
            IF (PRM(IIPP,IPM).EQ.0.) GOTO 90
            DTMPA = DBLE(WP(IPM))*DBLE(PRM(IIPP,IPM))
            DO 80 I = IP, NPE
              C(I,IP) = DBLE(PRM(IPPTR(I),IPM))*DTMPA + C(I,IP)
   80       CONTINUE
            G(IP) = DTMPA*TMPA + G(IP)
   90     CONTINUE
  100   CONTINUE
      ENDIF
C-------------CORRELATED PRIOR
      IF (IPR.GT.0) CALL SPES1GAU1PF(IPR,NIPR,WTP,C,G,NPE,NPLIST,B,
     &                               IPRAR,BPRI)
C-------QUASI-NEWTON ADDITION TO COEFFICIENT MATRIX
      IF ((NFIT.GT.0.OR.SOSR.GT.0.) .AND. NOPT.EQ.1 .AND. IFO.EQ.0)
     &    CALL SPES1GAU1QN(C,DD,G,NPE,R,GD,U,ITERP,X,ND,HOBS,H,WT,S,
     &                     NFIT,XD,SOSR,NHT,NDMH,WTQ,IOWTQ)
C-------FOR ONE PARAMETER CASE
      IF (NPE.LT.2) THEN
C-------CALCULATE STEP LENGTH FOR SINGLE-PARAMETER CASE
        DET = C(1,1)
        SCLE(1) = 1.0
        IF (IFO.GT.0) THEN
          IF (LN(IPPTR(1)).GT.0) B(IPPTR(1)) = EXP(B(IPPTR(1)))
          RETURN
        ENDIF
        DD(1) = G(1)/DET
      ELSE
C-------SCALE COEFFICIENT MATRIX AND GRADIENT VECTOR
        DO 110 IP = 1, NPE
          SCLE(IP) = 1.0
          IF (C(IP,IP).GT.1.E-30) SCLE(IP) = DSQRT(C(IP,IP))
  110   CONTINUE
        DO 130 IP = 1, NP1
          DTMPA = SCLE(IP)
          IP1 = IP + 1
          DO 120 I = IP1, NPE
            C(I,IP) = C(I,IP)/(SCLE(I)*DTMPA)
            C(IP,I) = C(I,IP)
  120     CONTINUE
          G(IP) = G(IP)/DTMPA
          C(IP,IP) = 1.0 + AMP
  130   CONTINUE
        G(NPE) = G(NPE)/SCLE(NPE)
        C(NPE,NPE) = 1.0 + AMP
C-------PRINT AS INDICATED BY IPRINT
        IF (IPRINT.NE.0) THEN
          WRITE (IOUT,500)
          DO 140 J = 1, NPE
            WRITE (IOUT,530) (C(I,J),I=1,NPE)
  140     CONTINUE
          WRITE (IOUT,505)
          WRITE (IOUT,530) (G(I),I=1,NPE)
        ENDIF
C-------COMPUTE PARAMETER STEP LENGTHS
        CALL SPES1GAU1SL(C,DD,G,NPE,CSA,IND,IFO,AMP,DET,RMARM,RMAR)
C-------IF MATRIX EQUATION IS SINGULAR, OR CONVERGENCE HAS BEEN REACHED:
        IF (IND.GT.0 .OR. IFO.GT.0) THEN
C---------CONVERT NATURAL LOGS OF PARAMETER VALUES AND RETURN
          DO 150 IP = 1, NPE
            IIPP = IPPTR(IP)
            IF (LN(IIPP).GT.0) B(IIPP) = EXP(B(IIPP))
  150     CONTINUE
          RETURN
        ENDIF
C-------UNSCALE PARAMETER CHANGE VECTOR
        DO 160 IP = 1, NPE
          DD(IP) = DD(IP)/SCLE(IP)
  160   CONTINUE
      ENDIF
C-----COMPUTE DAMPING PARAMETER AND NEW ESTIMATES OF REGRESSION
C-----PARAMETERS
C----------DEFINITION OF VARIABLES
C----------x IS U for untransformed parameters
C---------------N for transformed parameters with negative DD
C---------------P for transformed parameters with positive DD
C----------ABDMXx Absolute value of BDMXx
C----------ADMXx Values used to calculate the damping parameter (Col B,
C----------------------Table B1)
C----------AP   Value of the damping parameter used. Results in all
C----------------------parameters respecting MAX-CHANGE or MAX-CHANGE*
C----------APx  Value of the damping parameter if only the U, N, or P
C----------------------parameters were considered
C----------BDMXx  Maximum fractional parameter change calculated
C----------------------by the normal equations (Col A, Table B1)
C----------DDMAXx Maximum dd value; used only for x= N and P
C----------DMAXIx Indicator for adjusting dmax for extreme
C----------------------parameter values (B/B0 of eq. B4)
C----------DMXx  Values used for oscillation control (Col C, Table B1)
C----------JDx  Parameter that governs damping; JDx may not equal JJx
C----------JJx  Parameter with maximum change for convergence testing
C----------DEFINITION OF VARIABLE NOT CHANGED IN THE CALCULATIONS
C----------IAP = 0, Apply dmax in native space (AP is set to equal 0)
C----------IAP = 1, Apply dmax in parameter optimization
C--------------------------(possibly transformed) space
      DMXO = DMX
      ADMXU = 0.0
      ADMXN = 0.0
      ADMXP = 0.0
      BDMXU = 0.0
      BDMXN = 0.0
      BDMXP = 0.0
      ABDMXU = 0.0
      ABDMXN = 0.0
      ABDMXP = 0.0
      DMAXIU = 0.0
      DMAXIP = 0.0
      DDMAXN = 0.0
      DDMAXP = 0.0
      APO = AP
      APU = 1.0
      APN = 1.0
      APP = 1.0
      JJU = 1
      JJP = 1
      JJN = 1
      JDP = 1
      JDN = 1
      DO 170 IP = 1, NPE
        IIPP = IPPTR(IP)
        DTMPA = 0.0
        TMPB = 0.0
        LNN = LN(IIPP)
C-------PARAMETERS ADJUSTED AS IN REGRESSION SPACE
        IF (LNN.LE.0 .OR. IAP.EQ.1) THEN
          DTMPA = B(IIPP)
          IF (DABS(DTMPA).EQ.0.0) DTMPA = 1.0
          DTMPA = DD(IP)/ABS(DTMPA)
          TMPB = DABS(DTMPA)
C------------IF THIS IS THE LARGEST CHANGE, SAVE THE INFORMATION
C------------FOR CONVERGENCE AND OSCILLATION CONTROL
          IF(TMPB.GT.ADMXU) THEN
            ADMXU = TMPB
            JJU = IIPP
            DMXU = DTMPA
            BDMXU = DMXU
            ABDMXU = ABS(DMXU)
          ENDIF
C------------ADJUST DMAX IF THE PARAMETER VALUE IS VERY SMALL
          DMAXTMP = DMAX
          IF (B1(IIPP) .NE. 0.0) THEN
            DMAXIU = B(IIPP)/B1(IIPP)
            CALL SPES1GAU1DM (DMAXTMP,DMAXIU)
          ENDIF
          IF (DMAXTMP .LT. DMAX) DMAXTMP = DMAX
C------------CALCULATE THE DAMPING THAT WOULD BE NEEDED FOR THIS
C------------PARAMETER (COL B, TABLE B1)
          TEMP = 1.0
          IF (TMPB.NE.0.0) TEMP = DMAXTMP/TMPB
C------------IF THIS IS THE SMALLEST DAMPING PARAMETER, SAVE INFORMATION
          IF (NPE.EQ.1 .OR. APU.GT.TEMP) THEN
            IF (TEMP.LT.1.0) APU = TEMP
            JDU = IIPP
            DMAXU = DMAXTMP
          ENDIF
        ENDIF
C-------LOG-TRANSFORMED PARAMETERS ADJUSTED AS NATIVE
        IF (LNN.GT.0 .AND. IAP.EQ.0) THEN
C------------DECREASING PARAMETER VALUES
          IF (DD(IP) .LT. 0.0) THEN
C-----------------DAMPING PARAMETER
            IF (DMAX .LT. 1.0) THEN
              ADMXN = LOG(-DMAX+1.)/DD(IP)
              IF (ADMXN .LT. APN) THEN
                APN = ADMXN
                JDN = IIPP
              ENDIF
            ENDIF
C------------------LARGEST CHANGE
            IF(NPE.EQ.1 .OR. DD(IP).LT.DDMAXN) THEN
              JJN = IIPP
              DDMAXN = DD(IP)
              DMXN = DD(IP)
              IF (B(IIPP).NE.0.0) DMXN = DD(IP)/ABS(B(IIPP))
              BDMXN = EXP(DD(IP))-1.0
              ABDMXN = ABS(BDMXN)
            ENDIF
          ELSE
C------------INCREASING PARAMETER VALUE
C-----------------DAMPING PARAMETER
            DMAXTMP = DMAX
            DMAXIP = EXP(B(IIPP))/EXP(B1(IIPP))
            CALL SPES1GAU1DM (DMAXTMP,DMAXIP)
            ADMXP = LOG(DMAXTMP+1.)/DD(IP)
            IF (ADMXP .LT. APP) THEN
              APP = ADMXP
              JDP = IIPP
              DMAXP = DMAXTMP
            ENDIF
C------------------LARGEST CHANGE
            IF(NPE.EQ.1 .OR. DD(IP).GT.DDMAXP) THEN
              JJP = IIPP
              DDMAXP = DD(IP)
              DMXP = DD(IP)
              IF (B(IIPP).NE.0.0) DMXP = DD(IP)/ABS(B(IIPP))
              IF (DD(IP).LE.709.) THEN
                BDMXP = EXP(DD(IP))-1.0
              ELSE
                WRITE(IOUT,550) DD(IP), PARNAM(IIPP)
                CALL USTOP(' ')
              ENDIF
              ABDMXP = ABS(BDMXP)
            ENDIF
          ENDIF
        ENDIF
  170 CONTINUE
C
C-------FOR CONVERGENCE CHECK, FIND LARGEST BDMXx (NATIVE PARAMETER CHANGE
C-------FROM THE NORMAL EQUATIONS) AND ASSOCIATED PARAMETER NUMBER.
      IF (ABDMXU.GE.ABDMXP .AND. ABDMXU.GE.ABDMXN) THEN
        JCHG = JJU
        BDMX = BDMXU
        ABDMX = ABDMXU
        CTYPE = 'U'
        DMX = DMXU
      ELSEIF (ABDMXP.GE.ABDMXU .AND. ABDMXP.GE.ABDMXN) THEN
        JCHG = JJP
        BDMX = BDMXP
        ABDMX = ABDMXP
        CTYPE = 'P'
        DMX = DMXP
      ELSEIF (ABDMXN.GE.ABDMXU .AND. ABDMXN.GE.ABDMXP) THEN
        JCHG = JJN
        BDMX = BDMXN
        ABDMX = ABDMXN
        CTYPE = 'N'
        DMX = DMXN
      ENDIF
C
C-----WITH A MIXTURE OF NATIVE AND TRANSFORMED PARAMETERS, THE
C-----DAMPING PARAMETER MAY NOT BE ASSOCIATED WITH THE PARAMETER
C-----WITH THE LARGEST NATIVE PARAMETER CHANGE AS CALCULATED BY
C-----THE NORMAL EQUATIONS. USE DAMPING PARAMETERS FOR ALL
C-----TYPES CALCULATED ABOVE AND PICK THE SMALLEST ONE.
C
C
C-----DETERMINE THE DAMPING PARAMETER
C
C---------IF ALL APx = 1.0, SET DAMPING PARAMETER TO 1.0
      IF (APU.EQ.1.0 .AND. APN.EQ.1.0 .AND. APP.EQ.1.0) THEN
        AP=1.0
        JJ = JCHG
        DMAX1 = DMAX
        DTYPE = CTYPE
C       DMX FROM ABOVE
C--------...OTHERWISE, FIND THE SMALLEST DAMPING PARAMETER
      ELSE
        IF (APU.LE.APP .AND. APU.LE.APN) THEN
          AP = APU
          JJ = JDU
          DMX = DMXU
          DMAX1 = DMAXU
          DTYPE = 'U'
        ELSEIF (APP.LE.APU .AND. APP.LE.APN) THEN
          AP = APP
          JJ = JDP
          DMX = DMXP
          DMAX1 = DMAXP
          DTYPE = 'P'
        ELSEIF (APN.LE.APU .AND. APN.LE.APP) THEN
          AP = APN
          JJ = JDN
          DMX = DMXN
          DMAX1 = DMAX
          DTYPE = 'N'
        ENDIF
      ENDIF
C
C---------IF THE SAME PARAMETER CONTROLLED THE DAMPING PARAMETER
C---------LAST ITERATION, CONSIDER OSCILLATION CONTROL.
C-----------THESE CALCULATIONS ARE DONE IN TRANSFORMED PARAMETER
C-----------SPACE USING DMXx
      OCF = 1.0
      IF (ITERP.GT.1) THEN
        IF (JJ.EQ.JMAX) THEN
          DMX1 = DMX
          SPR = DMX1/(APO*DMXO)
          IF (SPR.LT.-1.) THEN
            AP1 = .5/ABS(SPR)
          ELSE
            AP1 = (3.+SPR)/(3.+ABS(SPR))
          ENDIF
        ELSE
          AP1 = 1.0
        ENDIF
        IF(AP1.LT.AP) then
          AP = AP1
          OCF = AP
        ENDIF
      ENDIF
      JMAX = JJ
C
C---------UPDATE PARAMETERS AND CONVERT TO PHYSICAL VALUES
      DO 180 IP = 1, NPE
        IIPP = IPPTR(IP)
          DDC = AP*DD(IP)
        B(IIPP) = B(IIPP) + DDC
        DD(IP) = DDC
        IF (LN(IIPP).GT.0) B(IIPP) = EXP(B(IIPP))
  180 CONTINUE
C
C-------PRINT TO THE LISTING FILE
      WRITE (IOUT,510) ITERP
      WRITE (IOUT,515) AMP, TOL, BDMX, PARNAM(JCHG),CTYPE
      WRITE (IOUT,520) DMAX, DMAX1, OCF, AP, PARNAM(JMAX), DTYPE
C-------CHECK FOR PARAMETER VALUES <= 0 THAT SHOULD BE > 0
      CALL SPES1GAU1CN(B1,IOUT,IPNG,ITERP,LN,NPE,NPLIST,NPNG,NPNGAR)
      WRITE (IOUT,545)
      WRITE (IOUT,525) (PARNAM(IPPTR(IP)),IP=1,NPE)
      WRITE (IOUT,'(1X)')
      WRITE (IOUT,530) (B(IPPTR(IP)),IP=1,NPE)
C-------PRINT TO THE SCREEN
      IF (IOSTAR.NE.1) THEN
        WRITE (*,510) ITERP
        WRITE (*,515) AMP, TOL, BDMX, PARNAM(JCHG), CTYPE
        WRITE (*,520) DMAX, DMAX1, OCF, AP, PARNAM(JMAX), DTYPE
        WRITE (*,545)
        WRITE (*,525) (PARNAM(IPPTR(IP)),IP=1,NPE)
        WRITE (*,'(1X)')
        WRITE (*,530) (B(IPPTR(IP)),IP=1,NPE)
      ENDIF
C-------CHECK FOR CONVERGENCE
      IF (ABDMX.LE.TOL) THEN
        IFO = 1
        WRITE (IOUT,540)
      ENDIF
C-------STORE STATISTICS FOR THIS ITERATION
      DMXA(ITERP) = BDMX
      NPAR(ITERP) = JMAX
      AMPA(ITERP) = AMP
      AMCA(ITERP) = DMAX1
      AAP(ITERP) = AP
C
      RETURN
      END
C======================================================================
      SUBROUTINE SPES1GAU1DM (DMAX1,DMAXI)
C     VERSION 20000424 ERB
C     ******************************************************************
C     ADJUST DMAX FOR PARAMETER VALUES FAR FROM THE STARTING VALUE SO
C     SO THAT EXTREME VALUES CAN RECOVER.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL DMAX1, DMAXI, TEMP
C     ------------------------------------------------------------------
      IF (DMAXI.EQ.0.0 .OR. DMAX1.LT.0.4) RETURN
      TEMP = DMAX1
      DMAX1 = 1./(DMAXI*(DMAX1+1.)**4)
      IF (DMAX1.LT.TEMP) DMAX1=TEMP
C
      RETURN
      END
C=======================================================================
      SUBROUTINE SPES1GAU1IN(NPE,C)
C-----VERSION 1000 01FEB1992
C     ******************************************************************
C         COMPLETE CALCULATION OF THE INVERSE OF SCALED
C         COEFFICIENT MATRIX C STARTING FROM DECOMPOSED MATRIX
C         FROM SPES1GAU1SL, FOR NPE>1
C     ******************************************************************
C         SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER I, IM1, J, K, KP1, NPE, NP1
      DOUBLE PRECISION C(NPE,NPE), DSUM
C     ------------------------------------------------------------------
      NP1 = NPE - 1
      C(NPE,NPE) = 1.0/C(NPE,NPE)
      DO 50 K = 1, NP1
        KP1 = K + 1
        DO 20 I = KP1, NPE
          DSUM = 0.0
          IM1 = I - 1
          DO 10 J = K, IM1
            DSUM = DSUM + C(I,J)*C(J,K)
   10     CONTINUE
          C(K,I) = -DSUM
          C(I,K) = -DSUM*C(I,I)
   20   CONTINUE
        DO 40 I = 1, K
          DSUM = C(K,I)
          DO 30 J = KP1, NPE
            DSUM = DSUM + C(J,K)*C(I,J)
   30     CONTINUE
          C(K,I) = DSUM
          C(I,K) = C(K,I)
   40   CONTINUE
   50 CONTINUE
      RETURN
      END
C=======================================================================
      SUBROUTINE SPES1GAU1PR(NPE,IOUT,NPLIST,IPR,MPR,IFO,OUTNAM,PAREST,
     &                       ITMXP,NPAR,DMXA,IPRINT,AAP,AMCA,RSQA,RSPA,
     &                       AMPA,ITERPK,SSPI,SSTO)
C     VERSION 20010613 ERB
C-----VERSION 1001 01JUN1992
C     ******************************************************************
C     PRINT PARAMETER VALUES AND STATISTICS FOR ALL ITERATIONS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL PAREST
      INTEGER IOUT, IP, IPR, IUPA, MPR, ITER, NPAR, NPE, NPLIST
      CHARACTER*200 OUTNAM
      CHARACTER*84 FN
      LOGICAL LOP
      DIMENSION AAP(ITMXP), AMCA(ITMXP), AMPA(ITMXP), NPAR(ITMXP+1),
     &          PAREST(ITMXP+1,NPLIST), RSPA(ITMXP+1), RSQA(ITMXP+1),
     &          SSPI(ITMXP+1), SSTO(ITMXP+1)
      DOUBLE PRECISION DMXA(ITMXP+1)
      INCLUDE 'param.inc'
      INCLUDE 'parallel.inc'
C     ------------------------------------------------------------------
  500 FORMAT (8F14.0)
  515 FORMAT (//,' SUMMARY OF PARAMETER VALUES AND STATISTICS FOR',/,
     &           '      ALL PARAMETER-ESTIMATION ITERATIONS')
  520 FORMAT (/,1X,71('-'))
  525 FORMAT (F10.0,I5,/,3F10.0)
  530 FORMAT (/,1X,'PARAMETER-ESTIMATION ITERATION: ',I4)
  535 FORMAT (/,1X,'FINAL PARAMETER VALUES AND STATISTICS:')
  540 FORMAT (/,1X,'PARAMETER NAME(S) AND VALUE(S):',/)
  545 FORMAT (6(3X,A10))
  550 FORMAT (6(2X,1PG11.4))
  560 FORMAT (1X,3(2X,G10.3,2X))
  565 FORMAT (/,1X,'SUMS OF SQUARED WEIGHTED',
     &        ' RESIDUALS:',/,3X,'OBSERVATIONS',3X,'PRIOR INFO.',4X,
     &        'TOTAL')
  570 FORMAT (/,1X,'PARAMETER UPDATE:',/,
     &        1X,'MAX CHANGE   MAX CALC.',4X,'AMP OR',/,
     &        1X,'PARAMETER',5X,'CHANGE',7X,
     &        'AGMX',/,1X,A10,2X,G10.3,2X,G10.3)
  580 FORMAT (/,1X,'PARAMETER ESTIMATION DID NOT CONVERGE IN THE ',
     &        'ALLOTTED NUMBER OF ITERATIONS')
  585 FORMAT (/,1X,'*** PARAMETER ESTIMATION CONVERGED BY SATISFYING',
     &        ' THE TOL CRITERION ***')
  590 FORMAT (/,1X,'*** PARAMETER ESTIMATION CONVERGED BY SATISFYING',
     &        ' THE SOSC CRITERION ***')
  600 FORMAT(' ERROR READING VALUE FOR PARAMETER ',A,' FROM UNIT ',I5,/
     &       ' -- STOP EXECUTION (SPES1GAU1PR)')
  620 FORMAT(' ERROR READING DMX, NPAR, AMP FROM UNIT ',I5,/
     &       ' -- STOP EXECUTION (SPES1GAU1PR)')
  630 FORMAT (/,' WARNING: ERROR IN OPENING FILE: ',A,' (SPES1GAU1PR)')
  640 FORMAT (1X,'PARAMETER: ',A,/,1X,'ITERATION   ESTIMATE')
  650 FORMAT (1X,I5,6X,G14.7)
  680 FORMAT(/,1X,'SUMS OF SQUARED WEIGHTED RESIDUALS FOR EACH',
     &' ITERATION',//,
     &8X,' SUMS OF SQUARED WEIGHTED RESIDUALS',/
     &1X,'ITER.',2X,'OBSERVATIONS',2X,'PRIOR INFO. ',5X,'TOTAL')
  690 FORMAT(1X,I5,3(2X,G12.5),2X,A,2X,G12.5)
  695 FORMAT(1X,'FINAL',3(2X,G12.5))
  700 FORMAT(/,
     &' SELECTED STATISTICS FROM MODIFIED GAUSS-NEWTON ITERATIONS',//,
     &8X,'MAX. PARAMETER CALC. CHANGE   MAX. CHANGE     DAMPING',/,
     &' ITER.     PARNAM     MAX. CHANGE',7X,'ALLOWED      PARAMETER',/,
     &' -----   ----------  ',13('-'),3X,13('-'),2X,12('-'))
  710 FORMAT(1X,I4,4X,A,2X,G13.6,3X,G13.6,2X,G12.5)
C
C     WRITE SUMMARY OF PARAMETER VALUES AND STATISTICS FOR ALL
C     PARAMETER-ESTIMATION ITERATIONS
      IF (IPRINT.GT.0) WRITE (IOUT,515)
      LAST = 0
C
      DO 100 ITER = 1, ITERPK
        IF (IPRINT.GT.0) WRITE (IOUT,520)
        IF (NPAR(ITER) .EQ. 0) LAST = 1
        IF (LAST.EQ.0) THEN
          IF (IPRINT.GT.0) WRITE (IOUT,530) ITER
        ELSE
          WRITE (IOUT,535)
        ENDIF
        IF (IPRINT.GT.0 .OR. LAST.NE.0) THEN
          WRITE (IOUT,540)
          WRITE (IOUT,545) (PARNAM(IPPTR(IP)),IP=1,NPE)
          WRITE (IOUT,'(1X)')
          WRITE (IOUT,550) (PAREST(ITER,IPPTR(IP)),IP=1,NPE)
        ENDIF
        RSQPI = 0.0
        IF (IPR.GT.0 .OR. MPR.GT.0) RSQPI = RSPA(ITER) - RSQA(ITER)
        IF (IPRINT.GT.0 .OR. LAST.NE.0) THEN
          WRITE (IOUT,565)
          WRITE (IOUT,560) RSQA(ITER), RSQPI, RSPA(ITER)
        ENDIF
        IF (LAST.EQ.0) THEN
          IF (IPRINT.GT.0)
     &       WRITE (IOUT,570) PARNAM(NPAR(ITER)), DMXA(ITER), AMPA(ITER)
        ENDIF
      IF (LAST.EQ.1) GOTO 120
  100 CONTINUE
  120 CONTINUE
C
      WRITE (IOUT,520)
      IF (IFO.EQ.0) ITER = ITER - 1
C
C     WRITE SELECTED STATISTICS FOR EACH ITERATION
      WRITE (IOUT,700)
      DO 125 I=1,ITER
        IF (NPAR(I).GT.0) THEN
          WRITE (IOUT,710) I,PARNAM(NPAR(I)),DMXA(I),AMCA(I),AAP(I)
        ENDIF
  125 CONTINUE
C
C     WRITE SSWR FOR EACH ITERATION
      WRITE (IOUT,680)
      DO 130 I=1,ITER
        SSO = SSTO(I) - SSPI(I)
        IF (NPAR(I).GT.0) THEN
          WRITE (IOUT,690) I,SSO,SSPI(I),SSTO(I)
        ELSE
          WRITE (IOUT,695) SSO,SSPI(ITER),SSTO(ITER)
        ENDIF
  130 CONTINUE
C
C     WRITE MESSAGE CONCERNING CONVERGENCE
      IF (IFO.EQ.0) THEN
        WRITE (IOUT,580)
      ELSEIF (IFO.EQ.1) THEN
        WRITE (IOUT,585)
      ELSEIF (IFO.EQ.2) THEN
        WRITE (IOUT,590)
      ENDIF
      WRITE (IOUT,520)
C
C-----WRITE PARAMETER ESTIMATES TO _pa FILE
C     OPEN OUTPUT FILE
      IF (OUTNAM.NE.'NONE' .AND. MYID.EQ.MPROC) THEN
        LENGNAM = NONB_LEN(OUTNAM,200)
C       FIND AN UNUSED FILE UNIT AND OPEN THE FILE
        FN = OUTNAM(1:LENGNAM)//'._pa'
        IUPA = IGETUNIT(1,1000)
        IF (IUPA.GT.0) THEN
          OPEN(IUPA,FILE=FN,ERR=150)
        ELSE
          GOTO 150
        ENDIF
        GOTO 160
  150   CONTINUE
        WRITE(IOUT,630) FN
  160   CONTINUE
C
C       WRITE PARAMETER ESTIMATES FOR EACH ITERATION FOR EACH PARAMETER
C       TO _pa FILE
        DO 180 IP = 1,NPE
          IIPP = IPPTR(IP)
          WRITE (IUPA,640) PARNAM(IIPP)
          DO 170 IT = 1,ITER
            WRITE (IUPA,650) IT, PAREST(IT,IPPTR(IP))
  170     CONTINUE
  180   CONTINUE
C
C       CLOSE _pa FILE
        INQUIRE(UNIT=IUPA,OPENED=LOP)
        IF (LOP) CLOSE (UNIT=IUPA)
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE SPES1GAU1QN(C,DD,G,NPE,R,GD,U,ITERP,X,ND,HOBS,H,WT,S,
     &                       NFIT,XD,SOSR,NHT,NDMH,WTQ,IOWTQ)
C-----VERSION 1001 01JAN1998
C     ******************************************************************
C     COMPUTE QUASI-NEWTON COMPONENT OF C MATRIX AND ADD TO C
C      (NOTE: PRIOR NOT INCLUDED BECAUSE LINEAR TERMS DO NOT CONTRIBUTE)
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL H, HOBS, SOSR, WT, WTQ, X, XD
      INTEGER I, IIP, IOWTQ, IPP, ITERP, J, JM1, JP1, K, K1, L, L1, M,
     &        N, N1, ND, NDMH, NFIT, NHT, NPE
      DOUBLE PRECISION C(NPE,NPE), DD(NPE), SUM, TMPA, DS, DGD, DU, CF,
     &                 QD, TMP, T, R(NPE*NPE/2+NPE), DPGD, DPU, U(NPE),
     &                 S(NPE), G, GD, DPGI, W1
      DIMENSION G(NPE), GD(NPE), X(NPE,ND), HOBS(ND), H(ND), WT(ND),
     &          XD(NPE,ND), WTQ(NDMH,NDMH)
C     ------------------------------------------------------------------
C
C------FOR FIRST PARAMETER-ESTIMATION ITERATIONS, INITIALIZE UPDATING
C------MATRIX R AND SAVE G AND X
      IF (ITERP.EQ.1) THEN
        I = 0
        DO 20 IIP = 1, NPE
          DO 10 IPP = IIP, NPE
            I = I + 1
            R(I) = 0.0
   10     CONTINUE
          GD(IIP) = G(IIP)
   20   CONTINUE
        DO 40 N = 1, ND
          DO 30 IIP = 1, NPE
            XD(IIP,N) = X(IIP,N)
   30     CONTINUE
   40   CONTINUE
        RETURN
      ENDIF
C
C------FOR SUBSEQUENT ITERATIONS, CALCULATE MATRIX R
      DO 50 IIP = 1, NPE
        S(IIP) = 0.0
   50 CONTINUE
C---------CONTRIBUTION FROM OBSERVATIONS WITH DIAGONAL WEIGHTING
      IF (NHT.GT.0) THEN
        DO 70 N = 1, NHT
          IF (WT(N).LT.0.0) GOTO 70
          W1 = (HOBS(N)-H(N))*WT(N)
          DO 60 IIP = 1, NPE
            S(IIP) = S(IIP) + W1*(XD(IIP,N)-X(IIP,N))
   60     CONTINUE
   70   CONTINUE
      ENDIF
C---------CONTRIBUTION FROM OBSERVATIONS WITH FULL WEIGHT MATRIX
      IF (NDMH.GT.0) THEN
        IF (IOWTQ.GT.0) THEN
          DO 100 K = 1, NDMH
            IF (WTQ(K,K).GT.0.0) THEN
              TMP = 0.0
              DO 80 L = 1, NDMH
                IF (WTQ(L,L).GT.0.0) THEN
                  L1 = NHT + L
                  TMP = TMP + DBLE(WTQ(K,L))*DBLE(HOBS(L1)-H(L1))
                ENDIF
   80         CONTINUE
              K1 = NHT + K
              DO 90 I = 1, NPE
                S(I) = S(I) + DBLE(XD(I,K1)-X(I,K1))*TMP
   90         CONTINUE
            ENDIF
  100     CONTINUE
        ELSE
          DO 120 N = 1, NDMH
            IF (WTQ(N,N).GT.0.0) THEN
              N1 = NHT + N
              W1 = (HOBS(N1)-H(N1))*WTQ(N,N)
              DO 110 IIP = 1, NPE
                S(IIP) = S(IIP) + W1*(XD(IIP,N1)-X(IIP,N1))
  110         CONTINUE
            ENDIF
  120     CONTINUE
        ENDIF
      ENDIF
C
      DO 130 J = 1, NPE
        GD(J) = GD(J) - G(J)
  130 CONTINUE
C
      K = 0
      QD = 0.0
      DS = 0.0
      DO 160 J = 1, NPE
        M = NPE - J + 1
        K = K + 1
        L = K
        SUM = 0.0
        IF (J.NE.NPE) THEN
          JP1 = J + 1
          DO 140 I = JP1, NPE
            K = K + 1
            SUM = SUM + DD(I)*R(K)
  140     CONTINUE
        ENDIF
        TMPA = DD(J)*R(L)
        QD = QD + (SUM+SUM+TMPA)*DD(J)
        SUM = SUM + TMPA
        IF (J.NE.1) THEN
          JM1 = J - 1
          DO 150 I = 1, JM1
            L = L - M
            SUM = SUM + R(L)*DD(J-I)
            M = M + 1
  150     CONTINUE
        ENDIF
        U(J) = SUM
        DS = DS + DD(J)*S(J)
  160 CONTINUE
C
      TMP = 1.0
      IF (QD.NE.0.0) TMP = DABS(DS/QD)
      T = 1.0
      IF (TMP.LT.1.0) T = TMP
      DGD = 0.0
      DU = 0.0
      DO 170 J = 1, NPE
        DGD = DGD + DD(J)*GD(J)
        U(J) = S(J) - T*U(J)
        DU = DU + DD(J)*U(J)
  170 CONTINUE
      CF = DU/(DGD*DGD)
      K = 0
      DO 190 J = 1, NPE
        DO 180 I = J, NPE
          K = K + 1
          DPGD = GD(J)
          DPU = U(J)
          DPGI = GD(I)
          R(K) = T*R(K) + (U(I)*DPGD+DPGI*DPU)/DGD - CF*DPGI*DPGD
  180   CONTINUE
  190 CONTINUE
C------SAVE GRADIENT
      DO 200 IIP = 1, NPE
        GD(IIP) = G(IIP)
  200 CONTINUE
C-----SAVE SENSITIVITIES
      DO 220 N = 1, ND
        DO 210 IIP = 1, NPE
          XD(IIP,N) = X(IIP,N)
  210   CONTINUE
  220 CONTINUE
C------ADD QN APPROXIMATION TO COEFFICIENT MATRIX
      IF ((NFIT.GT.0.AND.ITERP.GT.NFIT) .OR. SOSR.LT.0.0) THEN
        I = 0
        DO 240 IIP = 1, NPE
          DO 230 IPP = IIP, NPE
            I = I + 1
            C(IPP,IIP) = C(IPP,IIP) + R(I)
  230     CONTINUE
  240   CONTINUE
      ENDIF
      RETURN
      END
C=======================================================================
      SUBROUTINE SPES1GAU1SL(C,DD,G,NPE,CSA,IND,IFO,AMP,DET,RMARM,RMAR)
C-----VERSION 1000 01FEB1992
C     ******************************************************************
C         COMPUTE PARAMETER STEP LENGTHS USING THE MARQUARDT PROCEDURE
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL AMP, CSA, DET, SUM, SUMA, SUMB
      INTEGER I, IFO, IND, IP1, J, K, KP1, NM1, NPE
      DOUBLE PRECISION C(NPE,NPE), DD(NPE), DPIV, DTMPA, DSUM, G
      DIMENSION G(NPE)
C     ------------------------------------------------------------------
C         COMPUTE TRIAL PARAMETER STEP LENGTHS USING LDU FACTORIZATION:
C           DECOMPOSE MATRIX
      NM1 = NPE - 1
   10 IND = 0
      DET = 1.
      DO 40 K = 1, NM1
        DPIV = C(K,K)
        DET = DET*DPIV
        IF (DPIV.GT.1.E-13) THEN
          DPIV = 1.0/DPIV
          KP1 = K + 1
          DO 30 J = KP1, NPE
            DTMPA = C(J,K)*DPIV
            DO 20 I = J, NPE
              C(I,J) = C(I,J) - DTMPA*C(I,K)
   20       CONTINUE
   30     CONTINUE
          C(K,K) = DPIV
        ELSE
          IND = 1
          GOTO 110
        ENDIF
   40 CONTINUE
      DET = DET*C(NPE,NPE)
      IF (C(NPE,NPE).LE.1.E-13) THEN
        IND = 1
        IF (IFO.EQ.0) GOTO 110
      ENDIF
      IF (IFO.GT.0) RETURN
      DO 50 K = 1, NPE
        DD(K) = G(K)
   50 CONTINUE
C           FORWARD SUBSTITUTE
      DO 70 K = 1, NM1
        DTMPA = DD(K)*C(K,K)
        KP1 = K + 1
        DO 60 J = KP1, NPE
          DD(J) = DD(J) - C(J,K)*DTMPA
   60   CONTINUE
   70 CONTINUE
C           BACK SUBSTITUTE
      DD(NPE) = DD(NPE)/C(NPE,NPE)
      I = NPE
   80 I = I - 1
      IF (I.GT.0) THEN
        IP1 = I + 1
        DSUM = 0.0
        DO 90 J = IP1, NPE
          DSUM = DSUM + C(J,I)*DD(J)
   90   CONTINUE
        DD(I) = (DD(I)-DSUM)*C(I,I)
        GOTO 80
      ENDIF
C         CHECK SOLUTION AND ADD MARQUARDT PARAMETER IF NEEDED
      SUM = 0.0
      SUMA = 0.0
      SUMB = 0.0
      DO 100 I = 1, NPE
        SUM = SUM + DD(I)*G(I)
        SUMA = SUMA + DD(I)*DD(I)
        SUMB = SUMB + G(I)*G(I)
  100 CONTINUE
      IF (SUM.GT.CSA*SQRT(SUMA*SUMB)) RETURN
  110 AMP = RMARM*AMP + RMAR
      IF (AMP.GT.1.0) RETURN
      DO 130 I = 1, NPE
        C(I,I) = 1.0 + DBLE(AMP)
        DO 120 J = I, NPE
          C(J,I) = C(I,J)
  120   CONTINUE
  130 CONTINUE
      GOTO 10
      END
C======================================================================
      SUBROUTINE SPES1GAU1PF(IPR,NIPR,WTP,C,G,NPE,NPLIST,B,IPRAR,BPRI)
C     VERSION 20031002 ERB
C**********************************************************************
C     ADD COMPONENTS TO THE MATRIX AND VECTOR OF THE REGRESSION
C     EQUATION FOR PRIOR INFORMATION WITH A FULL WEIGHT MATRIX
C**********************************************************************
C      SPECIFICATIONS
      REAL B, BDIF, BPRI, WPP, WTP
      INTEGER I, IPR, IPRAR, J, K, K1, L, L1, NIPR, NPE, NPLIST
      DOUBLE PRECISION C(NPE,NPE), G(NPE)
      DIMENSION NIPR(IPRAR), B(NPLIST), BPRI(IPRAR), WTP(IPRAR,IPRAR)
C---------------------------------------------------------------------
C------ADDITIONS TO C AND G
      DO 60 I = 1, NPE
        DO 50 J = I, NPE
          DO 40 K = 1, IPR
            K1 = NIPR(K)
            IF (K1.EQ.I) THEN
              DO 30 L = 1, IPR
                WPP = WTP(L,K)
                L1 = NIPR(L)
                IF (L1.EQ.J) THEN
                  C(J,I) = C(J,I) + WPP
                ENDIF
                IF (J.EQ.I) THEN
                  BDIF = BPRI(L) - B(L1)
                  G(I) = G(I) + WPP*BDIF
                ENDIF
   30         CONTINUE
            ENDIF
   40     CONTINUE
   50   CONTINUE
   60 CONTINUE
      RETURN
      END
C=======================================================================
      SUBROUTINE SPES1GAU1CN(B1,IOUTG,IPNG,ITERP,LN,NPE,NPLIST,NPNG,
     &                       NPNGAR)
C-----VERSION 20030128 ERB
C     ******************************************************************
C     CHECK FOR PARAMETER VALUES <= 0 THAT SHOULD BE > 0.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL B1, BOLD
      INTEGER I, IIP, IOUTG, IPNG, LN, NPE, NPNG, NPLIST
      CHARACTER*4 PIDTMP
      DIMENSION B1(NPLIST), IPNG(NPNGAR), LN(NPLIST)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
  500 FORMAT ('  PARAMETER "',A,
     &        '" < 0 : NOT PHYSICALLY REASONABLE.',/,
     &        '  ESTIMATED VALUE OF ',G13.6,' CHANGED TO ',G13.6,
     &        ' (PES1GAU1CN)',/)
  505 FORMAT ('  LN PARAMETER "',A,'" <= 0 : NOT ',
     &        'PHYSICALLY OR MATHEMATICALLY REASONABLE.',/,
     &        '  ESTIMATED VALUE OF ',G13.6,' CHANGED TO ',G13.6,
     &        ' (PES1GAU1CN)',/)
C
cc      IF (ITERP.GT.1) THEN
        DO 20 IIP = 1, NPE
          IIPP = IPPTR(IIP)
          PIDTMP = PARTYP(IIPP)
          IF (B(IIPP).LE.B1(IIPP)/1.E6 .AND. LN(IIPP).LE.0 .AND.
     &        (PIDTMP.EQ.'HK  ' .OR. PIDTMP.EQ.'SS  ' .OR.
     &        PIDTMP.EQ.'SY  ' .OR. PIDTMP.EQ.'VK ' .OR.
     &        PIDTMP.EQ.'VANI' .OR. PIDTMP.EQ.'GHB ' .OR.
     &        PIDTMP.EQ.'RIV ' .OR. PIDTMP.EQ.'STR ' .OR.
     &        PIDTMP.EQ.'DRN ' .OR. PIDTMP.EQ.'ANI ' .OR.
     &        PIDTMP.EQ.'EVT ' .OR. PIDTMP.EQ.'VKCB' .OR.
     &        PIDTMP.EQ.'DRT ' .OR. PIDTMP.EQ.'ETS ')) THEN
            IF (NPNG.GT.0) THEN
              DO 10 I = 1, NPNG
                IF (IIPP.EQ.IPNG(I)) GOTO 20
   10         CONTINUE
            ENDIF
            BOLD = B(IIPP)
            B(IIPP) = B1(IIPP)/100.
            WRITE (IOUTG,500) PARNAM(IIPP), BOLD, B(IIPP)
          ENDIF
          IF (B(IIPP).LT.1.E-14 .AND. LN(IIPP).GT.0) THEN
            BOLD = B(IIPP)
            B(IIPP) = 1.E-14
            WRITE (IOUTG,505) PARNAM(IIPP), BOLD, B(IIPP)
          ENDIF
   20   CONTINUE
cc      ENDIF
C
      RETURN
      END

