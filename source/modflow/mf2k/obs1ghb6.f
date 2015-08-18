! Time of File Save by ERB: 3/15/2004 11:28AM
C     Last change:  ERB   3 Sep 2002    2:20 pm
      SUBROUTINE OBS1GHB6AL(IUGBOB,NQ,NQC,NQT,IOUT,NQGB,NQTGB,IOBSUM,
     &                     LCOBGHB,ITMXP,LCSSGB,ISUM,IOBS)
C     VERSION 20000125
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR FLOW OBSERVATIONS AT GENERAL-HEAD
C     BOUNDARY CELLS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER IOUT, IUGBOB, NQ, NQC, NQT, NQGB, NQCGB, NQTGB
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
C     IDENTIFY PROCESS
      WRITE(IOUT,490) IUGBOB
  490 FORMAT(/,' OBS1GHB6 -- OBSERVATION PROCESS (GENERAL HEAD',
     &    ' BOUNDARY FLOW OBSERVATIONS)',/,' VERSION 1.0, 10/15/98',/,
     &    ' INPUT READ FROM UNIT ',I4)
C
C  Turn off observation package if OBS is not active
      IF(IOBS.LE.0) THEN
        WRITE(IOUT,610)
610     FORMAT(/,1X,'WARNING: OBSERVATION (OBS) FILE IS NOT LISTED BUT',
     &      ' THE GHB OBSERVATION',/,' FILE (GBOB) IS',
     &     ' LISTED -- TURNING OFF GHB OBSERVATIONS (OBS1GHB6AL)')
        IUGBOB = 0
        RETURN
      ENDIF
C
C  Read data
      CALL URDCOM(IUGBOB,IOUT,LINE)
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NQGB,DUM,IOUT,IUGBOB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NQCGB,DUM,IOUT,IUGBOB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NQTGB,DUM,IOUT,IUGBOB)
      WRITE (IOUT,500) NQGB, NQCGB, NQTGB
  500 FORMAT (/,
     &    ' NUMBER OF FLOW-OBSERVATION GENERAL-HEAD-CELL GROUPS: ',I6,/,
     &    '   NUMBER OF CELLS IN GENERAL-HEAD-CELL GROUPS......: ',I6,/,
     &    '   NUMBER OF GENERAL-HEAD-CELL FLOWS................: ',I6)
C
      NQ = NQ + NQGB
      NQC = NQC + NQCGB
      NQT = NQT + NQTGB
      LCSSGB = ISUM
      ISUM = ISUM + ITMXP + 1
C     POINTER TO OBSERVATION ARRAYS
      LCOBGHB = IOBSUM
      IOBSUM = IOBSUM + NQTGB
C
      RETURN
      END
C=======================================================================
      SUBROUTINE OBS1GHB6RP(NCOL,NROW,NPER,IUGBOB,IOUT,OBSNAM,
     &                      NHT,JT,IBT,NQOB,NQCL,IQOB,
     &                      QCLS,IERR,HOBS,TOFF,WTQ,IOWTQ,IPRN,
     &                      NDMH,NSTP,PERLEN,TSMULT,ISSA,ITRSS,NQAR,
     &                      NQCAR,NQTAR,IQ1,NQT1,NDD,IUGB,NQGB,NQTGB,NT,
     &                      NC,IPLOT,NAMES,ND,IPR,MPR,IOWTQGB,OTIME)
C     VERSION 20010921 ERB
C     ******************************************************************
C     READ, CHECK AND STORE FLOW-OBSERVATION DATA FOR DRAIN BOUNDARIES.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL BLANK, EVFGB, HOBS, PERLEN, QCLS, TOFF, TOFFSET, TOMULTGB,
     &     TSMULT, WTQ
      INTEGER I, I4, IBT, IERR, IOUT, IOWTQ, IPRN, IQ, IQOB, IUGBOB,
     &        IWT, J, JT, L, N, NC, NC1, NC2, NCOL, NDMH, NHT, NPER,
     &        NQCL, NQOB, NROW, NSTP, NT, NT1, NT2, IUGB, ISSA
      INTEGER IPLOT(ND+IPR+MPR)
      CHARACTER*12 OBSNAM(NDD), NAMES(ND+IPR+MPR)
      CHARACTER*20 FMTIN*20, ANAME*50
      DIMENSION IBT(2,NQAR), NQOB(NQAR), NQCL(NQAR), IQOB(NQTAR),
     &          QCLS(5,NQCAR),  HOBS(ND), TOFF(ND), NSTP(NPER),
     &          PERLEN(NPER), TSMULT(NPER), ISSA(NPER)
      DIMENSION WTQ(NDMH,NDMH)
      CHARACTER*10 STATYP(0:2)
      REAL OTIME(ND)
      DATA (STATYP(I),I=0,2)/'VARIANCE','STD. DEV.','COEF. VAR.'/
      DATA ANAME/'COVARIANCE OF GERERAL-HEAD-CELL FLOW OBSERVATIONS '/
C     ------------------------------------------------------------------
  500 FORMAT (15X,2F5.0,F10.0)
  505 FORMAT (8F10.0)
  517 FORMAT (/,' GENERAL-HEAD-CELL FLOW OBSERVATION VARIANCES',
     &        ' ARE MULTIPLIED BY: ',G15.4)
  520 FORMAT (/,' OBSERVED GENERAL-HEAD-CELL FLOW DATA',/,
     &' -- TIME OFFSETS ARE MULTIPLIED BY: ',G12.5)
  525 FORMAT (/,'   GROUP NUMBER: ',I6,'   BOUNDARY TYPE: ',A,
     &'   NUMBER OF CELLS IN GROUP: ',I6,/,
     &'   NUMBER OF FLOW OBSERVATIONS: ',I6,//,
     &40X,'OBSERVED',/,
     &20X,'REFER.',12X,'BOUNDARY FLOW',/,
     &7X,'OBSERVATION',2X,'STRESS',4X,'TIME',5X,'GAIN (-) OR',14X,
     &'STATISTIC   PLOT',/,
     &2X,'OBS#    NAME',6X,'PERIOD   OFFSET',5X,'LOSS (+)',
     &4X,'STATISTIC     TYPE      SYM.')
  535 FORMAT (I6,1X,A12,2X,I4,2X,G11.4,1X,G11.4,1X,G11.4,2X,A10,
     &1X,I5)
  540 FORMAT (/,'       LAYER  ROW  COLUMN    FACTOR')
  550 FORMAT (4X,F8.0,F6.0,F7.0,F9.2)
  555 FORMAT (4X,F8.0,F6.0,F9.2)
  560 FORMAT (/,' FOR OBS ',I6,' STATISTIC RELATED TO WEIGHT < OR =0',/,
     &        ' -- STOP EXECUTION (OBS1GHB6RP)',/)
  565 FORMAT (/,' GENERAL HEAD BOUNDARY PACKAGE',
     &        ' IS NOT OPEN -- STOP EXECUTION (OBS1GHB6RP)')
  590 FORMAT (/,' ROW OR COLUMN NUMBER INVALID',
     &        ' -- STOP EXECUTION (OBS1GHB6RP)',/)
  605 FORMAT (/,' OBSERVATION ',I6,' EQUALS ZERO, THE STATISTIC ',
     &        'CAN NOT BE A',/,' COEFFICIENT OF VARIATION (ISTAT=2)',
     &        ' -- STOP EXECUTION (OBS1GHB6RP)')
  615 FORMAT (//,1X,A,/,1X,42('-'))
  620 FORMAT (/,1X,'ERROR: SEARCH ABOVE FOR ERROR MESSAGE(S)',/,
     &' -- STOP EXECUTION (OBS1GHB6RP)')
C
C-----READ TIME-OFFSET MULTIPLIER FOR FLOW-OBSERVATION TIMES AND INPUT
C     ERROR VARIANCE FOR FLOW OBSERVATIONS
      READ(IUGBOB,*) TOMULTGB, EVFGB, IOWTQGB
      IF (IOWTQGB.GT.0) IOWTQ = 1
C
C-------INITIALIZE VARIABLES
      IQ2 = IQ1 + NQGB - 1
      DO 20 IQ = IQ1,IQ2
        IBT(2,IQ) = 0
   20 CONTINUE
C-------WRITE TITLE AND LOOP THROUGH CELL GROUPS (READ ITEMS 3, 4, AND 5)
      WRITE (IOUT,517) EVFGB
      WRITE (IOUT,520) TOMULTGB
      DO 120 IQ = IQ1,IQ2
C       READ ITEM 3
        READ (IUGBOB,*) NQOB(IQ), NQCL(IQ)
        IBT(1,IQ) = 2
        WRITE (IOUT,525) IQ, 'GHB', NQCL(IQ), NQOB(IQ)
C----------SET FLAG FOR SETTING ALL FACTORS TO 1
        I4 = 0
        IF (NQCL(IQ).LT.0) THEN
          I4 = 1
          NQCL(IQ) = -NQCL(IQ)
        ENDIF
C----------READ TIME STEPS, MEASURED FLOWS, AND WEIGHTS.
        NT1 = NT + 1
        NT2 = NT + NQOB(IQ)
        DO 30 J = NT1, NT2
          N = NHT + J
C---------READ ITEM 4
          IF (IOWTQGB.GT.0) THEN
            READ (IUGBOB,*) OBSNAM(N), IREFSP, TOFFSET, HOBS(N), STAT,
     &                      ISP, IPLOT(N)
            NAMES(N) = OBSNAM(N)
            WRITE (IOUT,535) N, OBSNAM(N), IREFSP, TOFFSET, HOBS(N),
     &                       0.0, STATYP(ISP), IPLOT(N)
          ELSE
            READ (IUGBOB,*) OBSNAM(N), IREFSP, TOFFSET, HOBS(N),
     &                      WTQ(J,J), IWT, IPLOT(N)
            NAMES(N) = OBSNAM(N)
            WRITE (IOUT,535) N, OBSNAM(N), IREFSP, TOFFSET, HOBS(N),
     &                       WTQ(J,J), STATYP(IWT), IPLOT(N)
            IF (HOBS(N).EQ.0 .AND. IWT.EQ.2) THEN
              WRITE (IOUT,605) N
              IERR = 1
            ENDIF
            IF (IWT.EQ.2) WTQ(J,J) = WTQ(J,J)*HOBS(N)
            IF (IWT.GT.0) WTQ(J,J) = WTQ(J,J)*WTQ(J,J)
            WTQ(J,J) = WTQ(J,J)*EVFGB
            IF (WTQ(J,J).LE.0.) THEN
              WRITE (IOUT,560) N
              IERR = 1
            ENDIF
          ENDIF
          CALL UOBSTI(OBSNAM(N),IOUT,ISSA,ITRSS,NPER,NSTP,IREFSP,
     &                IQOB(J),PERLEN,TOFF(N),TOFFSET,TOMULTGB,TSMULT,1,
     &                OTIME(N))
C----------ERROR CHECKING
          IF (IQOB(J).GE.JT) THEN
            JT = IQOB(J)
            IF (TOFF(N).GT.0.) JT = JT+1
          ENDIF
          IF (IUGB.EQ.0) THEN
            WRITE (IOUT,565)
            IERR = 1
          ENDIF
   30   CONTINUE
C----------READ LAYER, ROW, COLUMN, AND FACTOR (ITEM 5)
        NC1 = NC + 1
        NC2 = NC + NQCL(IQ)
        WRITE (IOUT,540)
        DO 40 L = NC1, NC2
          READ (IUGBOB,*) (QCLS(I,L),I=1,4)
          IF (I4.EQ.1) QCLS(4,L) = 1.
          WRITE (IOUT,550) (QCLS(I,L),I=1,4)
          I = QCLS(2,L)
          J = QCLS(3,L)
          IF (J.LE.0 .OR. J.GT.NCOL .OR. I.LE.0 .OR. I.GT.NROW) THEN
            WRITE (IOUT,590)
            IERR = 1
          ENDIF
   40   CONTINUE
C-------UPDATE COUNTERS
        NC = NC2
        NT = NT2
  120 CONTINUE
      IQ1 = IQ2 + 1
C
C-------READ FULL COVARIANCE MATRIX ON GENERAL-HEAD-CELL FLOW-OBSERVATION DATA
      IPRN = 0
      NQT2 = NQT1 + NQTGB - 1
      IF (IOWTQGB.GT.0 .AND. NQTGB.GT.0) THEN
C       READ ITEM 6
        READ (IUGBOB,*) FMTIN, IPRN
C       READ ITEM 7
        DO 140 I = NQT1,NQT2
          READ (IUGBOB,FMTIN) (BLANK,J=NQT1,I-1), (WTQ(I,J),J=I,NQT2)
          DO 130 J = I, NQT2
            WTQ(I,J) = WTQ(I,J)*EVFGB
            IF (I.EQ.J) THEN
              IF (WTQ(I,J).LT.0.) WTQ(I,J) = -WTQ(I,J)
            ELSE
              WTQ(J,I) = WTQ(I,J)
            ENDIF
  130     CONTINUE
  140   CONTINUE
        IF (IPRN.GE.0) THEN
          WRITE (IOUT,615) ANAME
          CALL UARRSUBPRW(WTQ,NDMH,NDMH,NQT1,NQT2,NQT1,NQT2,IPRN,IOUT,
     &                    OBSNAM(NHT+1),NDMH)
        ENDIF
      ENDIF
      NQT1 = NQT2 + 1
C
      IF (IERR.GT.0) THEN
        WRITE(IOUT,620)
        CALL USTOP(' ')
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE OBS1GHB6FM(NQ,NQOB,NQCL,IQOB,QCLS,IBT,MXBND,NBOUND,
     &                      BNDS,HNEW,NCOL,NROW,NLAY,IOUT,IBOUND,NHT,
     &                      OBSNAM,H,TOFF,ITS,NQAR,NQCAR,NQTAR,NGHBVL,
     &                      ND,WTQ,NDMH)
C     VERSION 19981020 ERB
C     ******************************************************************
C     CALCULATE SIMULATED EQUIVALENTS TO OBSERVED FLOWS FOR THE GENERAL
C     HEAD BOUNDARY PACKAGE
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL BNDS, C, FACT, H, HB, QCLS, TOFF, ZERO
      INTEGER I, IBOUND, IBT, IBT1, IFLAG, II, IOUT, IQ, IQOB, ITS, J,
     &        JJ, K, KK, MXBND, N, NB, NBN, NBOUND, NC, NC1, NC2, NCOL,
     &        NHT, NLAY, NQ, NQCL, NQOB, NROW, NT, NT1, NT2
      CHARACTER*12 OBSNAM(ND)
      DOUBLE PRECISION HH, HHNEW, HNEW(NCOL,NROW,NLAY)
      DIMENSION BNDS(NGHBVL,MXBND), IBOUND(NCOL,NROW,NLAY), IBT(2,NQAR),
     &          NQOB(NQAR), NQCL(NQAR), IQOB(NQTAR), QCLS(5,NQCAR),
     &          H(ND), TOFF(ND), WTQ(NDMH,NDMH)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
  505 FORMAT (/,' OBS# ',I6,', ID ',A4,', TIME STEP ',I5)
  520 FORMAT (3I7)
  535 FORMAT (' ALL CELLS INCLUDED IN THIS OBSERVATION ARE INACTIVE.  ',
     &        'THE OBSERVATION WILL',/
     &        ,' BE OMITTED FROM THIS PARAMETER-ESTIMATION ITERATION')
  540 FORMAT (' CELL # ',I6,
     &        ' OF HEAD-DEP. BOUNDARY GAIN OR LOSS OBS# ',I6,' ID=',A,/,
     &        ' NOT FOUND IN CELLS LISTED FOR GHB PACKAGE',/,
     &        ' -- STOP EXECUTION (OBS1GHB6FM)')
C
C-------INITIALIZE VARIABLES
      ZERO = 0.0
      NC = 0
      NT1 = 1
C-------LOOP THROUGH BOUNDARY FLOWS
      DO 60 IQ = 1, NQ
        IBT1 = IBT(1,IQ)
        NT2 = NT1 + NQOB(IQ) - 1
        IF (IBT1.NE.2) GOTO 50
C----------WAS THERE A MEASUREMENT AT THIS BOUNDARY THIS TIME STEP?
        DO 40 NT = NT1, NT2
          IF (IQOB(NT).EQ.ITS .OR.
     &        (IQOB(NT).EQ.ITS-1.AND.TOFF(NHT+NT).GT.ZERO)) THEN
C----------ASSIGN VARIABLES ACCORDING TO BOUNDARY TYPE
            KRBOT = 0
            NBN = NBOUND
C----------LOOP THROUGH CELLS.
            NC1 = NC + 1
            NC2 = NC + NQCL(IQ)
            NB = 0
            DO 30 N = NC1, NC2
              K = QCLS(1,N)
              I = QCLS(2,N)
              J = QCLS(3,N)
C----------LOOP THROUGH DATA FILE TO FIND A MATCH.
              IFLAG = 0
              DO 10 MNB = 1, NBN
                NB = NB + 1
                IF (NB.GT.NBN) NB = 1
                KK = BNDS(1,NB)
                II = BNDS(2,NB)
                JJ = BNDS(3,NB)
C----------DO CALCULATIONS IF THIS IS A MATCH
                IF (I.EQ.II.AND.J.EQ.JJ.AND.K.EQ.KK) THEN
                  IFLAG = 1
                  IF (IBOUND(J,I,K).EQ.0) THEN
                    KRBOT = KRBOT + 1
                    GOTO 30
                  ENDIF
C-------------ASSIGN VARIABLE VALUES
                  HHNEW = HNEW(J,I,K)
                  HB = BNDS(4,NB)
                  C = BNDS(5,NB)
C-------------CALCULATE FLOWS
                  HH = C*(HB-HHNEW)
                  GOTO 20
                ENDIF
   10         CONTINUE
              IF (IFLAG.EQ.0) THEN
                WRITE (IOUT,540) N, NHT + NT, OBSNAM(NHT+NT)
                CALL USTOP(' ')
              ENDIF
C-------------SUM VALUES FROM INDIVIDUAL CELLS.
C----------------CALCULATE FACTOR FOR TEMPORAL INTERPOLATION
   20         FACT = 1.0
              IF (TOFF(NHT+NT).GT.ZERO) THEN
                IF (IQOB(NT).EQ.ITS) FACT = 1. - TOFF(NHT+NT)
                IF (IQOB(NT).EQ.ITS-1) FACT = TOFF(NHT+NT)
              ENDIF
C---------------FLOWS
              H(NHT+NT) = H(NHT+NT) + HH*FACT*QCLS(4,N)
   30       CONTINUE
C-----------CHECK FOR DISCONNECTED OBSERVATIONS
            IF (KRBOT.EQ.NQCL(IQ)) THEN
              WTQ(NT,NT) = -WTQ(NT,NT)
              WRITE (IOUT,535)
            ENDIF
          ENDIF
   40   CONTINUE
C-------UPDATE COUNTERS
   50   NC = NC + NQCL(IQ)
        NT1 = NT2 + 1
C
   60 CONTINUE
      RETURN
      END
C=======================================================================
      SUBROUTINE OBS1GHB6DR(NQ,NQOB,NQCL,IQOB,QCLS,IBT,MXBND,NBOUND,
     &                      BNDS,HNEW,IP,SNEW,NCOL,NROW,NLAY,IOUT,
     &                      IBOUND,NHT,X,OBSNAM,NPE,LN,TOFF,NPLIST,
     &                      ITS,NQAR,NQCAR,NQTAR,NGHBVL,IERR,IERRU,ND)
C-----VERSION 1001 15OCT1992
C     VERSION 19980915 ERB
C     ******************************************************************
C     CALCULATE SENSITIVITIES FOR FLOW OBSERVATIONS FOR THE GENERAL
C     HEAD BOUNDARY PACKAGE
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL BNDS, C, FACT, HB, HHNEW, QCLS, TOFF, X, XX, ZERO
      INTEGER I, IBOUND, IBT, IBT1, IFLAG, II, IOUT, IP, IQ, IQOB, ITS,
     &        J, JJ, K, KK, LN, MXBND, N, NB, NBN, NBOUND, NC, NC1, NC2,
     &        NCOL, NHT, NLAY, NPE, NQ, NQCL, NQOB, NROW, NT, NT1,
     &        NT2
      CHARACTER*4 PIDTMP
      CHARACTER*12 OBSNAM(ND)
      DOUBLE PRECISION HNEW(NCOL,NROW,NLAY), SNEW(NCOL,NROW,NLAY)
      DIMENSION BNDS(NGHBVL,MXBND), IBOUND(NCOL,NROW,NLAY),
     &          X(NPE,ND), IBT(2,NQAR), LN(NPLIST), NQOB(NQAR),
     &          NQCL(NQAR), IQOB(NQTAR), QCLS(5,NQCAR), TOFF(ND)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
  540 FORMAT (' CELL # ',I6,
     &        ' OF HEAD-DEP. BOUNDARY GAIN OR LOSS OBS# ',I6,' ID=',A,
     &        ' NOT FOUND IN CELLS LISTED FOR',/,' GHB PACKAGE',
     &        ' -- STOP EXECUTION (OBS1GHB6DR)')
C
C-------INITIALIZE VARIABLES
      ZERO = 0.0
      NC = 0
      NT1 = 1
      IIPP = IPPTR(IP)
      PIDTMP = PARTYP(IIPP)
      IF (PIDTMP.EQ.'GHB ')
     &    CALL SOBS1GHB6QC(IBT,IIPP,NGHBVL,MXBND,NQ,NQCL,BNDS,QCLS,
     &                     NQAR,NQCAR)
C-------LOOP THROUGH BOUNDARY FLOWS
      DO 60 IQ = 1, NQ
        IBT1 = IBT(1,IQ)
        NT2 = NT1 + NQOB(IQ) - 1
        IF (IBT1.NE.2) GOTO 50
C----------WAS THERE A MEASUREMENT AT THIS BOUNDARY THIS TIME STEP?
        DO 40 NT = NT1, NT2
          IF (IQOB(NT).EQ.ITS .OR.
     &        (IQOB(NT).EQ.ITS-1.AND.TOFF(NHT+NT).GT.ZERO)) THEN
C----------ASSIGN VARIABLES ACCORDING TO BOUNDARY TYPE
            NBN = NBOUND
C----------LOOP THROUGH CELLS.
            NC1 = NC + 1
            NC2 = NC + NQCL(IQ)
            NB = 0
            DO 30 N = NC1, NC2
              K = QCLS(1,N)
              I = QCLS(2,N)
              J = QCLS(3,N)
C----------LOOP THROUGH DATA FILE TO FIND A MATCH.
              IFLAG = 0
              DO 10 MNB = 1, NBN
                NB = NB + 1
                IF (NB.GT.NBN) NB = 1
                KK = BNDS(1,NB)
                II = BNDS(2,NB)
                JJ = BNDS(3,NB)
C----------DO CALCULATIONS IF THIS IS A MATCH
                IF (I.EQ.II.AND.J.EQ.JJ.AND.K.EQ.KK) THEN
                  IF (IBOUND(J,I,K).LT.1) GOTO 30
                  IFLAG = 1
C-------------ASSIGN VARIABLE VALUES
                  HHNEW = HNEW(J,I,K)
                  HB = BNDS(4,NB)
                  C = BNDS(5,NB)
C-------------CALCULATE SENSITIVITIES
                  XX = -C*SNEW(J,I,K)
                  IF (IIPP.EQ.IBT(2,IQ))
     &                XX = XX + QCLS(5,N)*(HB-HHNEW)
                  GOTO 20
                ENDIF
   10         CONTINUE
              IF (IFLAG.EQ.0) THEN
                WRITE (IOUT,540) N, NHT + NT, OBSNAM(NHT+NT)
                WRITE (IERRU,540) N, NHT + NT, OBSNAM(NHT+NT)
                IERR = 1
                RETURN
              ENDIF
C-------------SUM VALUES FROM INDIVIDUAL CELLS.
C----------------CALCULATE FACTOR FOR TEMPORAL INTERPOLATION
   20         FACT = 1.0
              IF (TOFF(NHT+NT).GT.ZERO) THEN
                IF (IQOB(NT).EQ.ITS) FACT = 1. - TOFF(NHT+NT)
                IF (IQOB(NT).EQ.ITS-1) FACT = TOFF(NHT+NT)
              ENDIF
C---------------SENSITIVITY-EQUATION SENSITIVITIES
              IF (LN(IIPP).GT.0) XX = XX*B(IIPP)
              X(IP,NHT+NT) = X(IP,NHT+NT) + XX*FACT*QCLS(4,N)
   30       CONTINUE
          ENDIF
   40   CONTINUE
C-------UPDATE COUNTERS
   50   NC = NC + NQCL(IQ)
        NT1 = NT2 + 1
C
   60 CONTINUE
      RETURN
      END
C=======================================================================
      SUBROUTINE OBS1GHB6PR(ITERSS,ITMXP,IUSS,SSGB)
C
C     VERSION 20010613 ERB
C     ******************************************************************
C     WRITE CONTRIBUTION TO SSWR OF FLOW OBSERVATIONS AT GENERAL-HEAD-
C     BOUNDARY CELLS TO _ss FILE
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER ITMXP, IUSS
      LOGICAL LOP
      DIMENSION SSGB(ITMXP+1)
C     ------------------------------------------------------------------
  660 FORMAT(1X,'ITERATION',2X,A)
  670 FORMAT(1X,I5,6X,G14.7)
C
      INQUIRE(UNIT=IUSS,OPENED=LOP)
      IF (LOP) THEN
        WRITE (IUSS,660)'SSWR-(GENERAL-HEAD BOUNDARY FLOW OBSERVATIONS O
     &NLY)'
C       WRITE CONTRIBUTION TO SSWR FOR EACH ITERATION
        DO 10 IT = 1, ITERSS
          WRITE(IUSS,670) IT,SSGB(IT)
   10   CONTINUE
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE SOBS1GHB6OH(IO,IOWTQGB,IOUT,NHT,NQTGB,HOBS,H,WTQ,
     &                       OBSNAM,IDIS,WTQS,D,AVET,NPOST,NNEGT,NRUNS,
     &                       RSQ,ND,MPR,IPR,NDMH,WTRL,NRSO,IUGDO,OUTNAM,
     &                       IPLOT,IPLPTR,LCOBGHB,ISSWR,SSGB,ITMXP,
     &                       OTIME)
C     VERSION 19990423 ERB
C     ******************************************************************
C     CALCULATE AND PRINT WEIGHTED RESIDUALS FOR GENERAL-HEAD BOUNDARY
C     FLOW OBSERVATIONS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL AVE, AVET, D, H, HOBS, RES, RSQ, SWH, VMAX, VMIN, WT2, WTQ,
     &     WTQS, WTR, WTRL
      INTEGER IDIS, IO, IOUT, IOWTQGB, IPR,
     &        J, MPR, N, ND, NDMH, NHT, NMAX, NMIN,
     &        NNEG, NNEGT, NPOS, NPOST, NQ1, NQ2, NQTGB, NRSO, NRUNS
      INTEGER IUGDO(6), IPLOT(ND+IPR+MPR), IPLPTR(ND+IPR+MPR)
      CHARACTER*12 OBSNAM(ND)
      CHARACTER*200 OUTNAM
      DIMENSION H(ND), HOBS(ND), D(ND+MPR+IPR), WTQ(NDMH,NDMH),
     &          WTQS(NDMH,NDMH), SSGB(ITMXP+1)
      REAL OTIME(ND)
C     ------------------------------------------------------------------
C
  500 FORMAT (/,' DATA FOR FLOWS REPRESENTED USING THE GENERAL-HEAD',
     &' BOUNDARY PACKAGE',//,
     &8X,'OBSERVATION      MEAS.      CALC.',14X,'WEIGHTED',/,
     &'   OBS#    NAME',10X,'FLOW',7X,'FLOW',4X,'RESIDUAL',3X,
     &        'RESIDUAL',/)
  505 FORMAT (/,' DATA FOR FLOWS REPRESENTED USING THE GENERAL-HEAD',
     &' BOUNDARY PACKAGE',//,
     &8X,'OBSERVATION',6X,'MEAS.      CALC.',26X,'WEIGHTED',/,
     &3X,'OBS#    NAME',10X,'FLOW',7X,'FLOW',5X,'RESIDUAL',2X,
     &'WEIGHT**.5',2X,'RESIDUAL',/)
  510 FORMAT (1X,I6,1X,A12,1X,5(1X,G10.3))
  515 FORMAT (1X,I6,1X,A12,2X,G10.3,'   DISCONNECTED')
  520 FORMAT (/,' SUM OF SQUARED WEIGHTED RESIDUALS',/,
     &'   (GENERAL-HEAD BOUNDARY FLOWS ONLY)  ',G11.5)
  525 FORMAT (/,' STATISTICS FOR GENERAL-HEAD BOUNDARY FLOW',
     &        ' RESIDUALS :',/,
     &        ' MAXIMUM WEIGHTED RESIDUAL  :',G10.3,' OBS#',I7,/,
     &        ' MINIMUM WEIGHTED RESIDUAL  :',G10.3,' OBS#',I7,/,
     &        ' AVERAGE WEIGHTED RESIDUAL  :',G10.3,/,
     &        ' # RESIDUALS >= 0. :',I7,/,' # RESIDUALS < 0.  :',I7,/,
     &        ' NUMBER OF RUNS: ',I6,'  IN ',I6,' OBSERVATIONS')
  530 FORMAT (2G20.7)
  535 FORMAT (' ')
  540 FORMAT (2(G15.7,1X),I5,2X,A,2X,G15.7)
  550 FORMAT (G15.7,1X,I5,2X,A)
C
      IF (IO.EQ.1) THEN
        IF (IOWTQGB.GT.0) THEN
          WRITE (IOUT,500)
        ELSE
          WRITE (IOUT,505)
        ENDIF
      ENDIF
      IDISGB = 0
      NRESGB = 0
      NRUNSGB = 1
      RSQGB = 0.0
      NNEG = 0
      NPOS = 0
      VMAX = -1.E20
      VMIN = 1.E20
      AVE = 0.
      DO 20 N = LCOBGHB, LCOBGHB+NQTGB-1
        NQ1 = N - NHT
        IF (WTQ(NQ1,NQ1).LT.0.) THEN
          IF (IO.EQ.1) WRITE (IOUT,515) N, OBSNAM(N), HOBS(N)
          IDIS = IDIS + 1
          IDISGB = IDISGB + 1
          GOTO 20
        ENDIF
        NRSO = NRSO + 1
        NRESGB = NRESGB + 1
        IPLPTR(NRSO) = N
        RES = HOBS(N) - H(N)
        IF (IOWTQGB.GT.0) THEN
          WTR = 0.0
          SWH = 0.0
          OWH = 0.0
          DO 10 J = 1, NQTGB
            NQ2 = LCOBGHB + J - 1
            IF (WTQ(NQ2,NQ2).LT.0.0) GOTO 10
            WTR = WTR + WTQS(NQ1,J)*(HOBS(NQ2)-H(NQ2))
            SWH = SWH + WTQS(NQ1,J)*H(NQ2)
            OWH = OWH + WTQS(NQ1,J)*HOBS(NQ2)
   10     CONTINUE
          IF (IO.EQ.1) WRITE (IOUT,510) N, OBSNAM(N),
     &                                  HOBS(N), H(N), RES, WTR
        ELSE
          WT2 = WTQS(NQ1,NQ1)
          WTR = RES*WT2
          OWH = HOBS(N)*WT2
          SWH = H(N)*WT2
          IF (IO.EQ.1) WRITE (IOUT,510) N, OBSNAM(N),
     &                                  HOBS(N), H(N), RES, WT2, WTR
        ENDIF
        IF (IO.EQ.1) THEN
          IF (OUTNAM.NE.'NONE') THEN
            WRITE (IUGDO(1),540) H(N), HOBS(N), IPLOT(N), OBSNAM(N),
     &                           OTIME(N)
            WRITE (IUGDO(2),540) SWH, OWH, IPLOT(N), OBSNAM(N)
            WRITE (IUGDO(3),540) SWH, WTR, IPLOT(N), OBSNAM(N)
            WRITE (IUGDO(4),550) RES, IPLOT(N), OBSNAM(N)
            WRITE (IUGDO(5),550) WTR, IPLOT(N), OBSNAM(N)
            D(NRSO) = WTR
          ENDIF
        ENDIF
        RSQ = RSQ + (WTR**2)
        RSQGB = RSQGB + (WTR**2)
        IF (WTR.GT.VMAX) THEN
          VMAX = WTR
          NMAX = N
        ENDIF
        IF (WTR.LT.VMIN) THEN
          VMIN = WTR
          NMIN = N
        ENDIF
        IF (WTR.GE.0.) NPOS = NPOS + 1
        IF (WTR.LT.0.) NNEG = NNEG + 1
        IF (N.GT.1) THEN
          IF (WTRL*WTR.LT.0.) NRUNS = NRUNS + 1
        ENDIF
        IF (N.GT.LCOBGHB) THEN
          IF (WTRL*WTR.LT.0.) NRUNSGB = NRUNSGB + 1
        ENDIF
        WTRL = WTR
        AVE = AVE + WTR
   20 CONTINUE
      IF (ISSWR.GT.0) SSGB(ISSWR) = RSQGB
      IF (NQTGB.NE.IDISGB) THEN
        AVET = AVET + AVE
        NPOST = NPOST + NPOS
        NNEGT = NNEGT + NNEG
        AVE = AVE/REAL(NQTGB-IDISGB)
        IF (IO.EQ.1) THEN
          WRITE (IOUT,525) VMAX, NMAX, VMIN, NMIN, AVE, NPOS, NNEG,
     &                     NRUNSGB, NRESGB
          WRITE (IOUT,520) RSQGB
        ENDIF
      ENDIF
      RETURN
      END
C=======================================================================
      SUBROUTINE SOBS1GHB6QC(IBT,IIPP,NGHBVL,MXBND,NQ,NQCL,BNDS,QCLS,
     &                       NQAR,NQCAR)
C     VERSION 20011108 ERB
C     ******************************************************************
C     POPULATE QCLS ARRAY ELEMENT 5 FOR GHB FLOW-OBSERVATION CELLS ON
C     ONE GHB-PARAMETER-CONTROLLED HEAD-DEPENDENT BOUNDARY IF IT IS
C     ACTIVE THIS STRESS PERIOD
C     ******************************************************************
C     Modified 11/8/2001 to support parameter instances - ERB
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION IBT(2,NQAR), NQCL(NQAR), BNDS(NGHBVL,MXBND),
     &          QCLS(5,NQCAR)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
      MCOLFAC = 5
      NC = 0
C     DEFINE NUMBER OF INSTANCES (NUMINST), NUMBER OF LIST ENTRIES PER
C     INSTANCE (NLST), AND, FOR THE ACTIVE INSTANCE, INSTANCE NUMBER
C     (NI) AND FIRST AND LAST POSITIONS IN PACKAGE ARRAY (IPL1 and IPL2)
      NI = IACTIVE(IIPP)
      NUMINST = IPLOC(3,IIPP)
      NLST = IPLOC(2,IIPP)-IPLOC(1,IIPP)+1
      IF (NUMINST.GT.1) NLST = NLST/NUMINST
      IPL1 = IPLOC(1,IIPP)+(NI-1)*NLST
      IPL2 = IPL1+NLST-1
C-------LOOP THROUGH ALL CELL GROUPS
      DO 120 IQ=1,NQ
        IFLAG = 0
C---------DETERMINE STARTING AND ENDING CELLS IN THIS CELL GROUP
        NC1 = NC + 1
        NC2 = NC + NQCL(IQ)
        IF (NI.GT.0) THEN
C---------IF PID FOR THIS PARAMETER MATCHES BOUNDARY TYPE SPECIFIED
C         FOR THIS CELL GROUP
          IF (IBT(1,IQ).EQ.2) THEN
C-----------LOOP THROUGH CELLS IN THIS GROUP
            DO 100 JJ = NC1,NC2
              KO = QCLS(1,JJ)
              IO = QCLS(2,JJ)
              JO = QCLS(3,JJ)
C
C-------------LOOP THROUGH PARAMETER (INSTANCE) CELLS IN PACKAGE ARRAY
              DO 80 II = IPL1,IPL2
                K = BNDS(1,II)
                I = BNDS(2,II)
                J = BNDS(3,II)
C---------------IF OBSERVATION CELL IS ON THE PARAMETER-CONTROLLED
C               BOUNDARY, PUT PARAMETER FACTOR IN QCLS(5)
                IF (KO.EQ.K .AND. IO.EQ.I .AND. JO.EQ.J) THEN
                  IBT(2,IQ) = IIPP
                  IFLAG = 1
                  QCLS(5,JJ) = BNDS(MCOLFAC,II)
                  GOTO 100
                ENDIF
   80         CONTINUE
   90         CONTINUE
  100       CONTINUE
          ENDIF
        ENDIF
        IF (IFLAG.EQ.0 .AND. IBT(2,IQ).EQ.IIPP) IBT(2,IQ) = 0
        NC = NC2
  120 CONTINUE
C
      RETURN
      END

