! Time of File Save by ERB: 5/12/2005 5:07PM
C  1-3-2003  Added code to apply damping only to external iterations
C
      SUBROUTINE PCG2ALG(ISUM,ISUMI,LCV,LCSS,LCP,LCCD,LCHCHG,LCLHCH,
     &                   LCRCHG,LCLRCH,MXITER,ITER1,NCOL,NROW,NLAY,IN,
     &                   IOUT,NPCOND,LCIT1,LCHCSV,IFREFM,IREWND,ISUMZ,
     &                   LCHPCG)
C
C-----VERSION 24JAN2003 PCG2ALG
C
C     ******************************************************************
C     ALLOCATE STORAGE IN THE X ARRAY FOR PCG ARRAYS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*200 LINE
      INTEGER ICG, IN, IOUT, ISIZ, ISOLD, ISUM, ISUMI, ISUMZ, ITER1,
     &        LCCD, LCHCHG, LCIT1, LCLHCH, LCLRCH, LCP, LCRCHG, LCSS,
     &        LCV, MXITER, NCOL, NLAY, NPCOND, NRC, NROW, IFREFM
C     ------------------------------------------------------------------
      IREWND=0
C
C-------PRINT A MESSAGE IDENTIFYING PCG PACKAGE
      WRITE (IOUT,500)
  500 FORMAT (1X,/1X,'PCG2 -- CONJUGATE GRADIENT SOLUTION PACKAGE',
     &        ', VERSION 2.4, 12/29/98')
C
C-------READ AND PRINT COMMENTS, MXITER,ITER1 AND NPCOND
      CALL URDCOM(IN,IOUT,LINE)
      IF(IFREFM.EQ.0) THEN
         READ (LINE,505) MXITER, ITER1, NPCOND
  505    FORMAT (3I10)
      ELSE
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXITER,R,IOUT,IN)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITER1,R,IOUT,IN)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPCOND,R,IOUT,IN)
      END IF
      WRITE (IOUT,510) MXITER, ITER1, NPCOND
  510 FORMAT (' MAXIMUM OF ',I6,' CALLS OF SOLUTION ROUTINE',/,
     &        ' MAXIMUM OF ',I6,
     &        ' INTERNAL ITERATIONS PER CALL TO SOLUTION ROUTINE',/,
     &        ' MATRIX PRECONDITIONING TYPE :',I5)
C
C-------ALLOCATE SPACE FOR THE PCG ARRAYS
      ISOLD = ISUM
      ISZOLD = ISUMZ
      NRC = NROW*NCOL
      ISIZ = NRC*NLAY
      LCHPCG=ISUMZ
      IF(MXITER.GT.1) ISUMZ=ISUMZ+ISIZ
      LCV = ISUMZ
      ISUMZ = ISUMZ + ISIZ
      LCSS = ISUMZ
      ISUMZ = ISUMZ + ISIZ
      LCP = ISUMZ
      ISUMZ = ISUMZ + ISIZ
      LCCD = ISUM
      ISUM = ISUM + ISIZ
      LCHCSV = ISUM
      ISUM = ISUM + ISIZ
      LCHCHG = ISUM
      ISUM = ISUM + MXITER*ITER1
      LCLHCH = ISUMI
      ISUMI = ISUMI + 3*MXITER*ITER1
      LCRCHG = ISUM
      ISUM = ISUM + MXITER*ITER1
      LCLRCH = ISUMI
      ISUMI = ISUMI + 3*MXITER*ITER1
      LCIT1 = ISUMI
      ISUMI = ISUMI + MXITER*ITER1
C
C-------CALCULATE AND PRINT THE SPACE USED IN THE X ARRAY
      ICG = ISUM - ISOLD
      WRITE (IOUT,515) ICG
  515 FORMAT (1X,I10,' ELEMENTS IN X ARRAY ARE USED BY PCG')
      ICG = 7*MXITER*ITER1
      WRITE (IOUT,516) ICG
  516 FORMAT (1X,I10,' ELEMENTS IN IX ARRAY ARE USED BY PCG')
      ICG = ISUMZ - ISZOLD
      WRITE (IOUT,517) ICG
  517 FORMAT (1X,I10,' ELEMENTS IN Z ARRAY ARE USED BY PCG')
C
C-------RETURN
      RETURN
      END
C
C
      SUBROUTINE PCG2RPG(MXITER,ITER1,HCLOSE,RCLOSE,NPCOND,NBPOL,RELAX,
     &                   IPRPCG,IN,IOUT,MUTPCG,NITER,DAMP,IFREFM)
C
C-----VERSION 29DEC1998 PCG2RPG
C     01SEPT1990 IPCGCD OMITTED; NITER ADDED
C     01JUNE1995 DAMP ADDED
C
C     ******************************************************************
C     READ DATA FOR PCG
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL DAMP, HCLOSE, RCLOSE, RELAX
      INTEGER IN, IOUT, IPRPCG, ITER1, MUTPCG, MXITER, NBPOL,
     &        NITER, NPCOND, IFREFM
C     ------------------------------------------------------------------
C
C-------READ HCLOSE,RCLOSE,RELAX,NBPOL,IPRPCG,MUTPCG
      IF(IFREFM.EQ.0) THEN
         READ (IN,500) HCLOSE,RCLOSE,RELAX,NBPOL,IPRPCG,MUTPCG,DAMP
  500    FORMAT (3F10.0,3I10,F10.0)
      ELSE
         READ (IN,*) HCLOSE,RCLOSE,RELAX,NBPOL,IPRPCG,MUTPCG,DAMP
      END IF
C
C-------PRINT MXITER,ITER1,NPCOND,HCLOSE,RCLOSE,RELAX,NBPOL,IPRPCG,
C-------MUTPCG,DAMP
        WRITE (IOUT,505)
  505   FORMAT (1X,///,36X,'SOLUTION BY THE CONJUGATE-GRADIENT METHOD',
     &        /,35X,43('-'))
      WRITE (IOUT,510) MXITER
  510 FORMAT (1X,19X,'MAXIMUM NUMBER OF CALLS TO PCG ROUTINE =',I9)
      WRITE (IOUT,515) ITER1
  515 FORMAT (1X,23X,'MAXIMUM ITERATIONS PER CALL TO PCG =',I9)
      WRITE (IOUT,520) NPCOND
  520 FORMAT (1X,30X,'MATRIX PRECONDITIONING TYPE =',I9)
      IF (NPCOND.EQ.2) WRITE (IOUT,525)
  525 FORMAT (1X,53X,'THE MATRIX WILL BE SCALED')
      WRITE (IOUT,530) RELAX, NBPOL
  530 FORMAT (1X,7X,'RELAXATION FACTOR (ONLY USED WITH',
     &        ' PRECOND. TYPE 1) =',E15.5,/,1X,
     &        'PARAMETER OF POLYNOMIAL PRECOND.',
     &        ' = 2 (2) OR IS CALCULATED :',I9)
      WRITE (IOUT,535) HCLOSE
  535 FORMAT (1X,24X,'HEAD CHANGE CRITERION FOR CLOSURE =',E15.5)
      WRITE (IOUT,540) RCLOSE
  540 FORMAT (1X,20X,'RESIDUAL CHANGE CRITERION FOR CLOSURE =',E15.5)
      IF (IPRPCG.LE.0) IPRPCG = 999
      WRITE (IOUT,545) IPRPCG, MUTPCG
  545 FORMAT (1X,11X,'PCG HEAD AND RESIDUAL CHANGE PRINTOUT INTERVAL =',
     &        I9,/,1X,4X,
     &        'PRINTING FROM SOLVER IS LIMITED(1) OR SUPPRESSED (>1) =',
     &        I9)
      IF (DAMP.LE.0.0) DAMP = 1.0
      WRITE (IOUT,550) DAMP
  550 FORMAT (1X,40X,'DAMPING PARAMETER =',E15.5)
      NITER = 0
C
      RETURN
      END
C
C
      SUBROUTINE PCG2AP(HNEW,IBOUND,CR,CC,CV,HCOF,RHS,V,SS,P,CD,HCHG,
     &                  LHCH,RCHG,LRCH,KITER,NITER,HCLOSE,RCLOSE,ICNVG,
     &                  KSTP,KPER,IPRPCG,MXITER,ITER1,NPCOND,NBPOL,NSTP,
     &                  NCOL,NROW,NLAY,NODES,RELAX,IOUT,MUTPCG,
     &                  IT1,DAMP,RES,HCSV,IERR,IERRU,HPCG)
C-----VERSION 29DEC1998 PCG2AP
C     01JULY1990 COMMENT STATEMENTS ADDED AND MODIFIED
C     01SEPT1990 IPCGCD OMITTED; STATEMENT 590 ADDED
C     27SEPT1990 STATEMENT IN DO 155 LOOP CHANGED
C     01SEPT1991 ADDED STATEMENTS RELATED TO SENSITIVITY CALCULATIONS
C                AT THE END OF THE 115 LOOP.  CHANGED THE 510 FORMAT
C                STATEMENT AND THE PRECEDING IF STATEMENT
C     20MAR1992  CHANGED 510 FORMAT STATEMENT; OMITTED 2 LINES IN DO 160
C                LOOP
C     01MAY1993  ADDED DEL TO CALCULATION OF THE CHOLESKY DIAGONAL
C     15JUNE1993 MADE CELLS SURROUNDED BY DRY CELLS INACTIVE
C     01JUNE1995 ADDED DAMP
C     29DEC1998  ADDED RES AND HCSV TO AVOID DESTROYING RHS AND HCOF.
C                REMOVED STATEMENTS THAT CALCULATE ACCURACY OF SOLUTION
C                FOR SENSITIVITY CALCULATIONS.
C
C     ******************************************************************
C     SOLUTION BY THE CONJUGATE GRADIENT METHOD -
C                                          UP TO ITER1 ITERATIONS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL BIGH, BIGR, BPOLY, C0, C1, C2, CC, CD, CD1, CR, CV, DAMP,
     &     HCHG, HCLOSE, HCOF, RCHG, RCLOSE, RELAX, RHS, T
      INTEGER I, IBOUND, IC, ICNVG, IH, II, IICNVG, IITER, IL, IOUT,
     &        IPRPCG, IR, IT1, ITER1, J, JH, JJ, JR, K, KH, KITER,
     &        KK, KPER, KR, KSTP, LHCH, LRCH, MUTPCG, MXITER, N, NBPOL,
     &        NC, NCD, NCF, NCL, NCN, NCOL, NH, NITER, NL, NLAY, NLL,
     &        NLN, NLS, NLZ, NODES, NORM, NPCOND, NR, NRB, NRC, NRH,
     &        NRL, NRN, NROW, NSTP
      DOUBLE PRECISION DZERO, DONE
      PARAMETER (DZERO=0.D0,DONE=1.D0)
      DOUBLE PRECISION HNEW, HHCOF, RRHS, DEL, HPCG
      DOUBLE PRECISION Z, B, D, E, F, H, S, ALPHA, V, SS, P
      DOUBLE PRECISION ZHNEW, BHNEW, DHNEW, FHNEW, HHNEW, SHNEW
      DOUBLE PRECISION SRNEW, SROLD, SSCR, SSCC, SSCV, VCC, VCR, VCV
      DOUBLE PRECISION CDCC, CDCR, CDCV
      DOUBLE PRECISION PN, VN, HCHGN, RCHGN, PAP
      DOUBLE PRECISION FCC, FCR, FCV, FV
      DOUBLE PRECISION DDAMP, BIGGESTPOS, BIGGESTNEG
C
      DIMENSION HNEW(NODES), IBOUND(NODES), CR(NODES), CC(NODES),
     &          CV(NODES), HCOF(NODES), RHS(NODES), IT1(MXITER*ITER1),
     &          V(NODES), SS(NODES), P(NODES), CD(NODES),
     &          HCHG(MXITER*ITER1), LHCH(3,MXITER*ITER1),
     &          RCHG(MXITER*ITER1), LRCH(3,MXITER*ITER1),
     &          RES(NODES), HCSV(NODES), HPCG(NODES)
C     ------------------------------------------------------------------
      BIGH = 1.0
      BIGGESTPOS = HUGE(BIGH)
      BIGGESTNEG = -BIGGESTPOS
C
!      IF(NITER.EQ.0) THEN
!        WRITE(IOUT,895)
!        IFIRST=1
!        IF(NLAY .GT. 1) THEN
!          NODESM1LAY=NODES-NCOL*NROW
!          WRITE(IOUT,900) (I,CC(I),CR(I),CV(I),HCOF(I),
!     1     RHS(I),HNEW(I),IBOUND(I),I=1,NODESM1LAY)
!          IFIRST=NODESM1LAY+1
!        ENDIF
!        WRITE(IOUT,901) (I,CC(I),CR(I),HCOF(I),
!     1     RHS(I),HNEW(I),IBOUND(I),I=IFIRST,NODES)
!      ENDIF
! 895  FORMAT ('     I',5X,'CC',13X,'CR',13X,'CV',12X,'HCOF',12X,'RHS',
!     &        12X,'HNEW',7X,'IBOUND')
! 900  FORMAT (I6,6G15.8,I5)
! 901  FORMAT (I6,2G15.8,15X,3G15.8,I5)
      DDAMP=DAMP
C-------ASSIGN VARIABLE EQUAL TO THE NUMBER OF CELLS IN ONE LAYER
      NRC = NROW*NCOL
C-------INITIALIZE VARIABLES USED TO CALCULATE ITERATION PARAMETERS
      SRNEW = DZERO
      BPOLY = 0.
      IF (NPCOND.NE.1) RELAX = 1.
      NORM = 0
      IF (NPCOND.EQ.2) THEN
         NORM = 1
         DO 8 N=1,NODES
         HCSV(N)=HCOF(N)
    8    CONTINUE
      END IF
C-------INITIALIZE VARIABLE USED TO TEST FOR NEGATIVE CHOLESKY DIAGONAL
      CD1 = 0.
C------CLEAR PCG WORK ARRAYS.
      DO 10 N = 1, NODES
        SS(N) = 0.
        P(N) = 0.
        V(N) = 0.
   10 CONTINUE
C------STORE PREVIOUS HEAD IF MXITER>1
      IF(MXITER.GT.1) THEN
         DO 15 N=1,NODES
         HPCG(N)=HNEW(N)
   15    CONTINUE
      END IF
C------FOR NPCOND=1, INITIALIZE CHOLESKY DIAGONAL
      IF (NPCOND.EQ.1) THEN
        DO 20 N = 1, NODES
          CD(N) = 0.
   20   CONTINUE
      ENDIF
C
C------CALCULATE THE RESIDUAL. IF NORM=1, CALCULATE THE DIAGONALS OF
C------THE A MATRIX,AND STORE THEM IN HCOF.
      DO 50 K = 1, NLAY
        DO 40 I = 1, NROW
          DO 30 J = 1, NCOL
C
C-------CALCULATE 1 DIMENSIONAL SUBSCRIPT OF CURRENT CELL AND
C-------SKIP CALCULATIONS IF CELL IS INACTIVE
            N = J + (I-1)*NCOL + (K-1)*NRC
            IF (IBOUND(N).EQ.0) THEN
              CC(N) = 0.
              CR(N) = 0.
              IF (N.LE.(NODES-NRC)) CV(N) = 0.
              IF (N.GT.1) CR(N-1) = 0.
              IF (N.GT.NCOL) CC(N-NCOL) = 0.
              IF (N.LE.(NODES-NRC) .AND. N.GT.NRC) CV(N-NRC) = 0.
              HCOF(N) = 0.
              RHS(N) = 0.
              GOTO 30
            ENDIF
C
C-------CALCULATE 1 DIMENSIONAL SUBSCRIPTS FOR LOCATING THE 6
C-------SURROUNDING CELLS
            NRN = N + NCOL
            NRL = N - NCOL
            NCN = N + 1
            NCL = N - 1
            NLN = N + NRC
            NLL = N - NRC
C
C-------CALCULATE 1 DIMENSIONAL SUBSCRIPTS FOR CONDUCTANCE TO THE 6
C-------SURROUNDING CELLS.
            NCF = N
            NCD = N - 1
            NRB = N - NCOL
            NRH = N
            NLS = N
            NLZ = N - NRC
C
C-----GET CONDUCTANCES TO NEIGHBORING CELLS
C-------NEIGHBOR IS 1 ROW BACK
            B = DZERO
            BHNEW = DZERO
            IF (I.NE.1) THEN
              B = CC(NRB)
              BHNEW = B*(HNEW(NRL)-HNEW(N))
            ENDIF
C
C-------NEIGHBOR IS 1 ROW AHEAD
            H = DZERO
            HHNEW = DZERO
            IF (I.NE.NROW) THEN
              H = CC(NRH)
              HHNEW = H*(HNEW(NRN)-HNEW(N))
            ENDIF
C
C-------NEIGHBOR IS 1 COLUMN BACK
            D = DZERO
            DHNEW = DZERO
            IF (J.NE.1) THEN
              D = CR(NCD)
              DHNEW = D*(HNEW(NCL)-HNEW(N))
            ENDIF
C
C-------NEIGHBOR IS 1 COLUMN AHEAD
            F = DZERO
            FHNEW = DZERO
            IF (J.NE.NCOL) THEN
              F = CR(NCF)
              FHNEW = F*(HNEW(NCN)-HNEW(N))
            ENDIF
C
C-------NEIGHBOR IS 1 LAYER BEHIND
            Z = DZERO
            ZHNEW = DZERO
            IF (K.NE.1) THEN
              Z = CV(NLZ)
              ZHNEW = Z*(HNEW(NLL)-HNEW(N))
            ENDIF
C
C-------NEIGHBOR IS 1 LAYER AHEAD
            S = DZERO
            SHNEW = DZERO
            IF (K.NE.NLAY) THEN
              S = CV(NLS)
              SHNEW = S*(HNEW(NLN)-HNEW(N))
            ENDIF
C
            IF (I.EQ.NROW) CC(N) = 0.
            IF (J.EQ.NCOL) CR(N) = 0.
C-------15JUN1993 SKIP CALCULATIONS AND MAKE CELL INACTIVE IF ALL
C                 SURROUNDING CELLS ARE INACTIVE
            IF (B+H+D+F+Z+S.EQ.0.) THEN
              IBOUND(N) = 0
              HCOF(N) = 0.
              RHS(N) = 0.
              WRITE(IOUT,27) K,I,J
   27           FORMAT (/,
     &        ' ISOLATED CELL IS BEING ELIMINATED (LAYER,ROW,COL):',
     &        3I6,/,' (PCG2AP)')
              GOTO 30
            ENDIF
C
C-------CALCULATE THE RESIDUAL AND STORE IT IN RES.  TO SCALE A,
C-------CALCULATE THE DIAGONAL OF THE A MATRIX, AND STORE IT IN HCOF.
            E = -Z - B - D - F - H - S
            RRHS = RHS(N)
            HHCOF = HNEW(N)*HCOF(N)
            RES(N) = RRHS - ZHNEW - BHNEW - DHNEW - HHCOF - FHNEW -
     &               HHNEW - SHNEW
            IF (NORM.EQ.1) HCOF(N) = HCOF(N) + E
            IF (IBOUND(N).LT.0) RES(N) = 0.
   30     CONTINUE
   40   CONTINUE
   50 CONTINUE
C
C-------SCALE CC,CR,CV,RES AND HNEW IF NORM=1.
      IF (NORM.EQ.1) THEN
        DO 80 K = 1, NLAY
          DO 70 I = 1, NROW
            DO 60 J = 1, NCOL
              N = J + (I-1)*NCOL + (K-1)*NRC
              IF (IBOUND(N).EQ.0) GOTO 60
              HHCOF = SQRT(-HCOF(N))
              IF (N.LE.(NODES-NCOL) .AND. CC(N).GT.0.)
     &            CC(N) = CC(N)/(HHCOF*(SQRT(-HCOF(N+NCOL))))
              IF (CR(N).GT.0.)
     &            CR(N) = CR(N)/(HHCOF*(SQRT(-HCOF(N+1))))
              IF (N.LE.(NODES-NRC) .AND. CV(N).GT.0.)
     &            CV(N) = CV(N)/(HHCOF*(SQRT(-HCOF(N+NRC))))
              HNEW(N) = HNEW(N)*HHCOF
              RES(N) = RES(N)/HHCOF
   60       CONTINUE
   70     CONTINUE
   80   CONTINUE
      ENDIF
C
C-------CALCULATE PARAMETER B OF THE POLYNOMIAL PRECONDITIONING METHOD
      IF (NPCOND.EQ.2) THEN
        IF (NBPOL.EQ.2) THEN
          BPOLY = 2
          GOTO 120
        ENDIF
        DO 110 K = 1, NLAY
          DO 100 I = 1, NROW
            DO 90 J = 1, NCOL
C
              N = J + (I-1)*NCOL + (K-1)*NRC
              IF (IBOUND(N).LT.1) GOTO 90
C
              NCF = N
              NCD = N - 1
              NRB = N - NCOL
              NRH = N
              NLS = N
              NLZ = N - NRC
C
              B = DZERO
              IF (I.NE.1) B = CC(NRB)
              H = DZERO
              IF (I.NE.NROW) H = CC(NRH)
              D = DZERO
              IF (J.NE.1) D = CR(NCD)
              F = DZERO
              IF (J.NE.NCOL) F = CR(NCF)
              Z = DZERO
              IF (K.NE.1) Z = CV(NLZ)
              S = DZERO
              IF (K.NE.NLAY) S = CV(NLS)
C
C-------NOTE : ABS. VAL. OF THE DIAG. OF THE SCALED A MATRIX IS 1.
              HHCOF = HCOF(N)
              IF (NORM.EQ.1) HHCOF = DONE
              T = DABS(Z) + DABS(B) + DABS(D) + ABS(HHCOF) + DABS(F)
     &            + DABS(H) + DABS(S)
              IF (T.GT.BPOLY) BPOLY = T
   90       CONTINUE
  100     CONTINUE
  110   CONTINUE
C
C-------CALCULATE ITERATION PARAMETERS FOR POLYNOMIAL PRECONDITIONING
C-------METHOD FOR A NEGATIVE DEFINITE MATRIX.
  120   C0 = (15./32.)*(BPOLY**3)
        C1 = (27./16.)*(BPOLY**2)
        C2 = (9./4.)*BPOLY
      ENDIF
C
C-------START INTERNAL ITERATIONS
      IITER = 0
      IF (KITER.EQ.1) NITER = 0
      ICNVG = 0
      IICNVG = 0
  130 IITER = IITER + 1
      NITER = NITER + 1
C
C-------INITIALIZE VARIABLES THAT TRACK MAXIMUM HEAD CHANGE AND RESIDUAL
C-------VALUE DURING EACH ITERATIONS
      BIGH = 0.
      BIGR = 0.
C-------INITIALIZE DEL (ADDED 01MAY1993)
      DEL = 0.
C
C
C-------CHECK NPCOND FOR PRECONDITIONING TYPE AND EXECUTE PROPER CODE
      IF (NPCOND.EQ.2) THEN
C
C-------POLYNOMIAL PRECONDITIONING
        DO 140 N = 1, NODES
          V(N) = RES(N)
  140   CONTINUE
        CALL SPCG2E(IBOUND,RES,HCOF,CR,CC,CV,V,SS,C2,NORM,NCOL,NROW,
     &              NLAY,NODES)
        CALL SPCG2E(IBOUND,RES,HCOF,CR,CC,CV,SS,V,C1,NORM,NCOL,NROW,
     &              NLAY,NODES)
        CALL SPCG2E(IBOUND,RES,HCOF,CR,CC,CV,V,SS,C0,NORM,NCOL,NROW,
     &              NLAY,NODES)
      ELSE
C
C-------CHOLESKY PRECONDITIONING
C
C-------STEP THROUGH CELLS TO CALCULATE THE DIAGONAL OF THE CHOLESKY
C-------MATRIX (FIRST INTERNAL ITERATION ONLY) AND THE INTERMEDIATE
C-------SOLUTION.  STORE THEM IN CD AND V, RESPECTIVELY.
  150   DO 180 K = 1, NLAY
          DO 170 I = 1, NROW
            DO 160 J = 1, NCOL
C
              N = J + (I-1)*NCOL + (K-1)*NRC
              IF (IBOUND(N).LT.1) GOTO 160
C
C-------CALCULATE V
              H = DZERO
              VCC = DZERO
              IC = N - NCOL
              IF (I.NE.1) THEN
                H = CC(IC)
                IF (CD(IC).NE.0.) VCC = H*V(IC)/CD(IC)
              ENDIF
C
              F = DZERO
              VCR = DZERO
              IR = N - 1
              IF (J.NE.1) THEN
                F = CR(IR)
                IF (CD(IR).NE.0.) VCR = F*V(IR)/CD(IR)
              ENDIF
C
              S = DZERO
              VCV = DZERO
              IL = N - NRC
              IF (K.NE.1) THEN
                S = CV(IL)
                IF (CD(IL).NE.0.) VCV = S*V(IL)/CD(IL)
              ENDIF
              V(N) = RES(N) - VCR - VCC - VCV
C
C-------CALCULATE CD - FIRST INTERNAL ITERATION ONLY
              IF (IITER.EQ.1) THEN
                CDCR = DZERO
                CDCC = DZERO
                CDCV = DZERO
                FCC = DZERO
                FCR = DZERO
                FCV = DZERO
                IF (IR.GT.0) THEN
                  IF (CD(IR).NE.0.) CDCR = (F**2)/CD(IR)
                ENDIF
                IF (IC.GT.0) THEN
                  IF (CD(IC).NE.0.) CDCC = (H**2)/CD(IC)
                ENDIF
                IF (IL.GT.0) THEN
                  IF (CD(IL).NE.0.) CDCV = (S**2)/CD(IL)
                ENDIF
                IF (NPCOND.EQ.1) THEN
                  IF (IR.GT.0) THEN
                    FV = CV(IR)
C                 MODIFIED FROM HILL(1990) 9/27/90: 2 REPLACES 1
                    IF (K.EQ.NLAY .AND. ((J+I).GT.2)) FV = DZERO
                    IF (CD(IR).NE.0.) FCR = (F/CD(IR))*(CC(IR)+FV)
                  ENDIF
                  IF (IC.GT.0) THEN
                    FV = CV(IC)
                    IF (K.EQ.NLAY .AND. (I.GT.1)) FV = DZERO
                    IF (CD(IC).NE.0.) FCC = (H/CD(IC))*(CR(IC)+FV)
                  ENDIF
                  IF (IL.GT.0) THEN
                    IF (CD(IL).NE.0.) FCV = (S/CD(IL))*(CR(IL)+CC(IL))
                  ENDIF
                ENDIF
                IF (NORM.EQ.0) THEN
                  B = DZERO
                  H = DZERO
                  D = DZERO
                  F = DZERO
                  Z = DZERO
                  S = DZERO
                  IF (I.NE.1) B = CC(IC)
                  IF (I.NE.NROW) H = CC(N)
                  IF (J.NE.1) D = CR(IR)
                  IF (J.NE.NCOL) F = CR(N)
                  IF (K.NE.1) Z = CV(IL)
                  IF (K.NE.NLAY) S = CV(N)
                  HHCOF = HCOF(N) - Z - B - D - F - H - S
                ENDIF
                IF (NORM.EQ.1) HHCOF = -DONE
                CD(N) = (DONE+DEL)*HHCOF - CDCR - CDCC - CDCV -
     &                  RELAX*(FCR+FCC+FCV)
                IF (CD1.EQ.0. .AND. CD(N).NE.0.) CD1 = CD(N)
C--------.LT. CHANGED TO .LE. 01SEPT1991
                IF (CD(N)*CD1.LE.0.) THEN
C--------CHANGED 500 FORMAT 01SEPT1991 AND 20MAR1992
C--------CHANGED 500 FORMAT AND WRITE STATEMENT AND ADDED DEL 01MAY1993
                  DEL = 1.5D0*DEL + .001D0
                  IF (DEL.GT..5D0) THEN
                    WRITE (IOUT,500)
                    WRITE (IERRU,500)
                    IERR = 1
                    RETURN
                  ENDIF
                  GOTO 150
                ENDIF
  500           FORMAT (/,
     &        ' MATRIX IS SEVERELY NON-DIAGONALLY DOMINANT.  CHECK',
     &        ' INPUT FILES.',/,' -- STOP EXECUTION (PCG2AP)')
              ENDIF
C
  160       CONTINUE
  170     CONTINUE
  180   CONTINUE
C
C-------STEP THROUGH EACH CELL AND SOLVE FOR S OF THE CONJUGATE
C-------GRADIENT ALGORITHM BY BACK SUBSTITUTION. STORE RESULT IN SS.
        DO 210 KK = NLAY, 1, -1
          DO 200 II = NROW, 1, -1
            DO 190 JJ = NCOL, 1, -1
C
              N = JJ + (II-1)*NCOL + (KK-1)*NRC
              IF (IBOUND(N).LT.1) GOTO 190
C
              NC = N + 1
              NR = N + NCOL
              NL = N + NRC
              SSCR = DZERO
              SSCC = DZERO
              SSCV = DZERO
              IF (JJ.NE.NCOL) SSCR = CR(N)*SS(NC)/CD(N)
              IF (II.NE.NROW) SSCC = CC(N)*SS(NR)/CD(N)
              IF (KK.NE.NLAY) SSCV = CV(N)*SS(NL)/CD(N)
              VN = V(N)/CD(N)
              SS(N) = VN - SSCR - SSCC - SSCV
  190       CONTINUE
  200     CONTINUE
C-------SKIP OVER OTHER PRECONDITIONING TYPES
  210   CONTINUE
      ENDIF
C
C-------CALCULATE P OF THE CONJUGATE GRADIENT ALGORITHM
      SROLD = SRNEW
      SRNEW = DZERO
      DO 220 N = 1, NODES
        IF (IBOUND(N).GT.0) SRNEW = SRNEW + SS(N)*RES(N)
  220 CONTINUE
C
      IF (IITER.EQ.1) THEN
        DO 230 N = 1, NODES
          P(N) = SS(N)
  230   CONTINUE
      ELSE
        DO 240 N = 1, NODES
          P(N) = SS(N) + (SRNEW/SROLD)*P(N)
  240   CONTINUE
      ENDIF
C
C-------CALCULATE ALPHA OF THE CONJUGATE GRADIENT ROUTINE.
C-------FOR THE DENOMINATOR OF ALPHA, MULTIPLY THE MATRIX A BY THE
C-------VECTOR P, AND STORE IN V; THEN MULTIPLY P BY V.  STORE IN PAP.
      PAP = DZERO
      DO 270 K = 1, NLAY
        DO 260 I = 1, NROW
          DO 250 J = 1, NCOL
C
            N = J + (I-1)*NCOL + (K-1)*NRC
            V(N) = 0.
            IF (IBOUND(N).LT.1) GOTO 250
C
            NRN = N + NCOL
            NRL = N - NCOL
            NCN = N + 1
            NCL = N - 1
            NLN = N + NRC
            NLL = N - NRC
C
            NCF = N
            NCD = NCL
            NRB = NRL
            NRH = N
            NLS = N
            NLZ = NLL
C
            B = DZERO
            IF (I.NE.1) B = CC(NRB)
            H = DZERO
            IF (I.NE.NROW) H = CC(NRH)
            D = DZERO
            IF (J.NE.1) D = CR(NCD)
            F = DZERO
            IF (J.NE.NCOL) F = CR(NCF)
            Z = DZERO
            IF (K.NE.1) Z = CV(NLZ)
            S = DZERO
            IF (K.NE.NLAY) S = CV(NLS)
C
            IF (NORM.EQ.0) PN = P(N)
            IF (NORM.EQ.1) PN = DZERO
            BHNEW = DZERO
            HHNEW = DZERO
            DHNEW = DZERO
            FHNEW = DZERO
            ZHNEW = DZERO
            SHNEW = DZERO
            IF (NRL.GT.0) BHNEW = B*(P(NRL)-PN)
            IF (NRN.LE.NODES) HHNEW = H*(P(NRN)-PN)
            IF (NCL.GT.0) DHNEW = D*(P(NCL)-PN)
            IF (NCN.LE.NODES) FHNEW = F*(P(NCN)-PN)
            IF (NLL.GT.0) ZHNEW = Z*(P(NLL)-PN)
            IF (NLN.LE.NODES) SHNEW = S*(P(NLN)-PN)
C
C-------CALCULATE THE PRODUCT OF MATRIX A AND VECTOR P AND STORE
C-------RESULT IN V.
            PN = HCOF(N)*P(N)
            IF (NORM.EQ.1) PN = -P(N)
            VN = ZHNEW + BHNEW + DHNEW + PN + FHNEW + HHNEW + SHNEW
            V(N) = VN
            PAP = PAP + P(N)*VN
  250     CONTINUE
  260   CONTINUE
  270 CONTINUE
C
C-------CALCULATE ALPHA
      ALPHA = 1.D0
      IF (PAP.EQ.0. .AND. MXITER.EQ.1) THEN
        WRITE (IOUT,505)
        WRITE (IERRU,505)
        IERR = 1
        RETURN
      ENDIF
  505 FORMAT (/,' CONJUGATE-GRADIENT METHOD FAILED.',/,
     &    ' SET MXITER GREATER THAN ONE AND TRY AGAIN.  STOP EXECUTION')
      IF (PAP.NE.0.) ALPHA = SRNEW/PAP
C
C-------CALCULATE NEW HEADS AND RESIDUALS, AND SAVE THE LARGEST
C-------CHANGE IN HEAD AND THE LARGEST VALUE OF THE RESIDUAL.
      DO 300 K = 1, NLAY
        DO 290 I = 1, NROW
          DO 280 J = 1, NCOL
C
            N = J + (I-1)*NCOL + (K-1)*NRC
            IF (IBOUND(N).LT.1) GOTO 280
C
C-------HEAD
            HCHGN = ALPHA*P(N)
            IF (DABS(HCHGN).GT.ABS(BIGH)) THEN
              IF (HCHGN.LT.BIGGESTPOS .AND. HCHGN.GT.BIGGESTNEG) THEN
                BIGH = HCHGN
              ELSE
                IF (HCHGN.GT.0.0) THEN
                  BIGH = 0.9999*BIGGESTPOS
                ELSE
                  BIGH = 0.9999*BIGGESTNEG
                ENDIF
              ENDIF
              IH = I
              JH = J
              KH = K
              NH = N
            ENDIF
            HNEW(N) = HNEW(N) + HCHGN
C
C--------RESIDUAL (V IS THE PRODUCT OF MATRIX A AND VECTOR P)
            RCHGN = -ALPHA*V(N)
            RES(N) = RES(N) + RCHGN
            IF (ABS(RES(N)).GT.ABS(BIGR)) THEN
              BIGR = RES(N)
              IR = I
              JR = J
              KR = K
              NR = N
            ENDIF
  280     CONTINUE
  290   CONTINUE
  300 CONTINUE
C
C-------UNSCALE LARGEST CHANGE IN HEAD AND LARGEST RESIDUAL, AND
C-------CHECK THE CONVERGENCE CRITERION
      IF (NORM.EQ.1) THEN
        BIGH = BIGH/SQRT(-HCOF(NH))
        BIGR = BIGR*SQRT(-HCOF(NR))
      ENDIF
      BIGH=BIGH*DAMP
      BIGR=BIGR*DAMP
      IF (MXITER.EQ.1) THEN
        IF (ABS(BIGH).LE.HCLOSE .AND. ABS(BIGR).LE.RCLOSE) ICNVG = 1
      ELSE
        IF (IITER.EQ.1 .AND. ABS(BIGH).LE.HCLOSE .AND. ABS(BIGR)
     &      .LE.RCLOSE) ICNVG = 1
      ENDIF
      IF (ABS(BIGH).LE.HCLOSE .AND. ABS(BIGR).LE.RCLOSE) IICNVG = 1
C
C-------STORE THE LARGEST UNSCALED HEAD CHANGE AND RESIDUAL VALUE
C-------(THIS ITERATION) AND THEIR LOCATIONS.
      II = NITER
      HCHG(II) = BIGH
      LHCH(1,II) = KH
      LHCH(2,II) = IH
      LHCH(3,II) = JH
C
      RCHG(II) = BIGR
      LRCH(1,II) = KR
      LRCH(2,II) = IR
      LRCH(3,II) = JR
C
      IT1(II) = 0
      IF (IITER.EQ.1) IT1(II) = 1
C-------GO TO NEXT INTERNAL ITERATION IF CONVERGENCE HAS NOT BEEN
C-------REACHED AND IITER IS LESS THAN ITER1
      IF (MXITER.EQ.1) THEN
        IF (ICNVG.EQ.0 .AND. IITER.LT.ITER1) GOTO 130
      ELSEIF (IICNVG.EQ.0 .AND. IITER.LT.ITER1) THEN
        GOTO 130
      ENDIF
C
C-------UNSCALE CR,CC,CV AND HNEW
      IF (NORM.EQ.1) THEN
        DO 310 N = 1, NODES
          IF (IBOUND(N).EQ.0) GOTO 310
          HHCOF = SQRT(-HCOF(N))
          IF (N.LE.(NODES-NCOL) .AND. CC(N).GT.0.)
     &        CC(N) = CC(N)*(HHCOF*(SQRT(-HCOF(N+NCOL))))
          IF (N.LE.(NODES-1) .AND. CR(N).GT.0.)
     &        CR(N) = CR(N)*(HHCOF*(SQRT(-HCOF(N+1))))
          IF (N.LE.(NODES-NRC) .AND. CV(N).GT.0.)
     &        CV(N) = CV(N)*(HHCOF*(SQRT(-HCOF(N+NRC))))
          HNEW(N) = HNEW(N)/HHCOF
  310   CONTINUE
      ENDIF
C
C-------AT END OF EXTERNAL ITERATION, APPLY DAMP
      IF(MXITER.GT.1) THEN
         DO 320 N=1,NODES
         IF(IBOUND(N).LE.0) GO TO 320
         HNEW(N)= (DONE-DDAMP)*HPCG(N) + DDAMP*HNEW(N)
  320    CONTINUE
      END IF
C
C-------IF END OF TIME STEP, PRINT # OF ITERATIONS THIS STEP
      IF (ICNVG.NE.0 .OR. KITER.EQ.MXITER) THEN
        IF (MUTPCG.LT.2) THEN
          IF (KSTP.EQ.1) WRITE (IOUT,510)
  510     FORMAT (1X,/1X)
          WRITE (IOUT,515) KITER, KSTP, KPER, NITER
  515     FORMAT (I6,' CALLS TO PCG ROUTINE FOR TIME STEP',I4,
     &            ' IN STRESS PERIOD ',I4,/,I6,' TOTAL ITERATIONS')
          IF (MUTPCG.LE.0) THEN
C
C-------PRINT HEAD CHANGE EACH ITERATION IF PRINTOUT INTERVAL IS REACHED
            IF (ICNVG.EQ.0 .OR. KSTP.EQ.NSTP .OR. MOD(KSTP,IPRPCG).EQ.0)
     &          CALL SPCG2P(HCHG,LHCH,RCHG,LRCH,ITER1,NITER,MXITER,IOUT,
     &                      NPCOND,BPOLY,IT1,MUTPCG,NCOL,NROW)
          ENDIF
        ELSE IF (MUTPCG.EQ.3) THEN
          IF (ICNVG.EQ.0) CALL SPCG2P(HCHG,LHCH,RCHG,LRCH,ITER1,NITER,
     &             MXITER,IOUT,NPCOND,BPOLY,IT1,0,NCOL,NROW)
        ENDIF
        NITER = 0
      ENDIF
C
C-------RESTORE HCOF IF NEEDED AND RETURN
      IF(NORM.EQ.1) THEN
         DO 600 N=1,NODES
         HCOF(N)=HCSV(N)
  600    CONTINUE
      END IF
      RETURN
C
      END
C
C
      SUBROUTINE SPCG2P(HCHG,LHCH,RCHG,LRCH,ITER1,NITER,MXITER,IOUT,
     &                  NPCOND,BPOLY,IT1,MUTPCG,NCOL,NROW)
C
C-----VERSION 29DEC1998 SPCG2P
C     ******************************************************************
C     PRINT MAXIMUM HEAD CHANGE AND RESIDUAL VALUE FOR EACH ITERATION
C                           DURING A TIME STEP
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL BPOLY, HCHG, RCHG
      INTEGER I, IOUT, IT1, ITER1, J, LHCH, LRCH, MUTPCG, MXITER, NITER,
     &        NPCOND, NGRP, K, L1, L2, JMIN
C
      DIMENSION HCHG(MXITER*ITER1), LHCH(3,MXITER*ITER1)
      DIMENSION RCHG(MXITER*ITER1), LRCH(3,MXITER*ITER1)
      DIMENSION IT1(MXITER*ITER1)
C     ------------------------------------------------------------------
C
      IF (NPCOND.EQ.2) WRITE (IOUT,500) BPOLY
  500 FORMAT(1X,/1X,'B OF THE POLYNOMIAL PRECONDITIONING METHOD:',E13.4)
      IF (MUTPCG.EQ.0) THEN
         WRITE(IOUT,5)
5        FORMAT(1X,/1X,'MAXIMUM HEAD CHANGE FOR EACH ITERATION',
     1       ' (1 INDICATES THE FIRST INNER ITERATION):',//
     2       1X,5('   HEAD CHANGE '),/
     3       1X,5('  LAYER,ROW,COL')/1X,75('-'))
         NGRP=(NITER-1)/5 +1
         DO 20 K=1,NGRP
            L1=(K-1)*5 +1
            L2=L1+4
            IF(K.EQ.NGRP) L2=NITER
            IF (NCOL.LE.999 .AND. NROW.LE.999) THEN
              WRITE(IOUT,10) (IT1(J),HCHG(J),J=L1,L2)
              WRITE(IOUT,11) ((LHCH(I,J),I=1,3),J=L1,L2)
10            FORMAT(5(2X,I1,G12.4))
11            FORMAT(1X,5(:'  (',I3,',',I3,',',I3,')'))
            ELSE
              WRITE(IOUT,13) (IT1(J),HCHG(J),J=L1,L2)
              WRITE(IOUT,14) ((LHCH(I,J),I=1,3),J=L1,L2)
13            FORMAT(5(4X,I1,G12.4,2X))
14            FORMAT(1X,5(:'  (',I3,',',I5,',',I5,')'))
            ENDIF
20       CONTINUE
         WRITE(IOUT,25)
25       FORMAT(1X,/1X,'MAXIMUM RESIDUAL FOR EACH ITERATION',
     1       ' (1 INDICATES THE FIRST INNER ITERATION):',//
     2       1X,5('   RESIDUAL    '),/
     3       1X,5('  LAYER,ROW,COL')/1X,75('-'))
         DO 30 K=1,NGRP
            L1=(K-1)*5 +1
            L2=L1+4
            IF(K.EQ.NGRP) L2=NITER
            WRITE(IOUT,10) (IT1(J),RCHG(J),J=L1,L2)
            WRITE(IOUT,11) ((LRCH(I,J),I=1,3),J=L1,L2)
30       CONTINUE
         WRITE(IOUT,31)
31       FORMAT(1X,/1X)
      ELSE
         WRITE(IOUT,35)
35       FORMAT (1X,/1X,'MAXIMUM HEAD CHANGE FOR LAST ITER1 ITERATIONS',
     1          /1X,'(1 INDICATES THE FIRST INNER ITERATION):',//,
     2       1X,5('   HEAD CHANGE '),/
     3       1X,5('  LAYER,ROW,COL')/1X,75('-'))
         JMIN=MAX(1,NITER-ITER1+1)
         NGRP=(NITER-JMIN)/5 +1
         DO 40 K=1,NGRP
            L1=(K-1)*5 +JMIN
            L2=L1+4
            IF(K.EQ.NGRP) L2=NITER
            WRITE(IOUT,10) (IT1(J),HCHG(J),J=L1,L2)
            WRITE(IOUT,11) ((LHCH(I,J),I=1,3),J=L1,L2)
40       CONTINUE
         WRITE(IOUT,45)
45       FORMAT(1X,/1X,'MAXIMUM RESIDUAL FOR LAST ITER1 ITERATIONS',
     1          /1X,'(1 INDICATES THE FIRST INNER ITERATION):',//,
     2       1X,5('   RESIDUAL    '),/
     3       1X,5('  LAYER,ROW,COL')/1X,75('-'))
         DO 50 K=1,NGRP
            L1=(K-1)*5 +JMIN
            L2=L1+4
            IF(K.EQ.NGRP) L2=NITER
            WRITE(IOUT,10) (IT1(J),RCHG(J),J=L1,L2)
            WRITE(IOUT,11) ((LRCH(I,J),I=1,3),J=L1,L2)
50       CONTINUE
         WRITE(IOUT,31)
      END IF
C
      RETURN
      END
C
C
      SUBROUTINE SPCG2E(IBOUND,RES,HCOF,CR,CC,CV,VIN,VOUT,C,NORM,NCOL,
     &                  NROW,NLAY,NODES)
C
C
C-----VERSION 29DEC1998 SPCG2E
C     ******************************************************************
C           MATRIX MULTIPLICATIONS FOR POLYNOMIAL PRECONDITIONING
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL C, CC, CR, CV, HCOF, RES
      INTEGER I, IBOUND, J, K, N, NCD, NCF, NCL, NCN, NCOL, NLAY, NLL,
     &        NLN, NLS, NLZ, NODES, NORM, NRB, NRC, NRH, NRL, NRN, NROW
      DOUBLE PRECISION VN, CRHS, Z, B, D, F, H, S, ZV, BV, DV, FV, HV,
     &                 SV, DZERO, VIN, VOUT
      DIMENSION IBOUND(NODES), CR(NODES), CC(NODES), CV(NODES),
     &          RES(NODES), VIN(NODES), VOUT(NODES), HCOF(NODES)
C     ------------------------------------------------------------------
C
      DZERO = 0.D0
      NRC = NROW*NCOL
      DO 30 K = 1, NLAY
        DO 20 I = 1, NROW
          DO 10 J = 1, NCOL
C
            N = J + (I-1)*NCOL + (K-1)*NRC
            VOUT(N) = 0.
            IF (IBOUND(N).LT.1) GOTO 10
C
            NRN = N + NCOL
            NRL = N - NCOL
            NCN = N + 1
            NCL = N - 1
            NLN = N + NRC
            NLL = N - NRC
C
            NCF = N
            NCD = NCL
            NRB = NRL
            NRH = N
            NLS = N
            NLZ = NLL
C
            B = DZERO
            BV = DZERO
            IF (I.NE.1 .AND. IBOUND(NRL).GE.0) THEN
              B = CC(NRB)
              BV = B*VIN(NRL)
            ENDIF
            H = DZERO
            HV = DZERO
            IF (I.NE.NROW .AND. IBOUND(NRN).GE.0) THEN
              H = CC(NRH)
              HV = H*VIN(NRN)
            ENDIF
            D = DZERO
            DV = DZERO
            IF (J.NE.1 .AND. IBOUND(NCL).GE.0) THEN
              D = CR(NCD)
              DV = D*VIN(NCL)
            ENDIF
            F = DZERO
            FV = DZERO
            IF (J.NE.NCOL .AND. IBOUND(NCN).GE.0) THEN
              F = CR(NCF)
              FV = F*VIN(NCN)
            ENDIF
            Z = DZERO
            ZV = DZERO
C      IF STATEMENT REARRANGED 01JUN1993
            IF (K.NE.1) THEN
              IF (IBOUND(NLL).GE.0) THEN
                Z = CV(NLZ)
                ZV = Z*VIN(NLL)
              ENDIF
            ENDIF
            S = DZERO
            SV = DZERO
            IF (K.NE.NLAY .AND. IBOUND(NLN).GE.0) THEN
              S = CV(NLS)
              SV = S*VIN(NLN)
            ENDIF
C
C-------CALCULATE THE PRODUCT OF MATRIX A AND VECTOR VIN AND STORE
C------ RESULT IN VOUT
            VN = HCOF(N)*VIN(N)
            IF (NORM.EQ.1) VN = -VIN(N)
            CRHS = C*RES(N)
            VOUT(N) = CRHS + ZV + BV + DV + VN + FV + HV + SV
   10     CONTINUE
   20   CONTINUE
   30 CONTINUE
      RETURN
      END
