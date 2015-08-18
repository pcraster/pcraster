C  I edited all occurrences of common block HUFCOM (in sen1huf2.f, 
C  lmt6.f, gwfhuf2.f, and obs1bas6.f) to put all REAL arrays before all 
C  INTEGER arrays.  The original order is OK when both REALs and 
C  INTEGERs are KIND=4.  But when REALs are promoted to DOUBLE 
C  PRECISION, KIND goes from 4 to 8, and this generates alignment 
C  problems.  The alignment problems are avoided when all variables of 
C  larger KIND precede all variables of smaller KIND. -- ERB 6/29/2006
C=======================================================================
      SUBROUTINE OBS1BAS6DF(IOBS,IOSTAR,IOWTQ,IOWTQDR,IOWTQGB,
     &                      IOWTQRV,IOWTQST,IQ1,IUOBS,JT,LCCOFF,LCHFB,
     &                      LCIPLO,LCIPLP,LCIQOB,LCNDER,LCNQOB,LCOBADV,
     &                      LCOBDRN,LCOBGHB,LCOBBAS,LCOBRIV,LCOBSE,
     &                      LCOBSTR,LCQCLS,LCROFF,LCSSAD,LCSSCH,LCSSDR,
     &                      LCSSGB,LCSSGF,LCSSPI,LCSSRV,LCSSST,LCSSTO,
     &                      LCWT,LCWTQ,MOBS,NC,ND,NDMH,NDMHAR,NH,
     &                      NOBADV,NQ,NQC,NQT,NQT1,NQTDR,NQTGB,NQTRV,
     &                      NQTST,NQTCH,NT,NTT2,IOBSUM,LCX,LCBUF2,NDAR,
     &                      LCOBDRT,LCSSDT,NQTDT,IOWTQDT,LCSSSF,NQTSF,
     &                      LCOBSFR,IOWTQSF,NHT,LCRSQA,LCRSPA,LCBUF1,
     &                      LCH,LCHOBS,LCWTQS,LCHANI,LCXND,LCOTIM,
     &                      OBSALL)
C     VERSION 20020709
C     ******************************************************************
C     INITIALIZE VARIABLES FOR OBSERVATION PROCESS
C     ******************************************************************
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER IOBS, IOSTAR, LCCOFF, LCHFB, LCIPLO, LCIPLP, LCIQOB,
     &        LCNQOB, LCOBADV, LCOBDRN, LCOBGHB, LCOBBAS, LCOBRIV,
     &        LCOBSFR, LCOBSTR, LCQCLS, LCROFF, LCSSAD, LCSSCH, LCSSDR,
     &        LCSSGB, LCSSGF, LCSSPI, LCSSRV, LCSSST, LCSSTO, LCWT,
     &        LCWTQ, LCXND, ND, NDMH, NH, NOBADV, NQ, NQC, NQT, NQTDR,
     &        NQTGB, NQTRV, NQTST, NQTCH, NTT2
      LOGICAL OBSALL
C     ------------------------------------------------------------------
      OBSALL = .FALSE.
      IOBS = 0
      IOBSUM=1
      IF (IUOBS.GT.0) IOBS = 1
C
C-----INITIALIZE GLOBAL VARIABLES THAT BELONG PRIMARILY TO THE
C     OBSERVATION PROCESS
      IOSTAR = 0
      IOWTQ = 0
      IOWTQDR = 0
      IOWTQGB = 0
      IOWTQRV = 0
      IOWTQST = 0
      IQ1 = 1
      JT = 0
      MOBS = 0
      NC = 0
      ND = 0
      NDMH = 0
      NH = 0
      NHT = 0
      NQT1 = 1
      NT = 0
      NTT2 = 0
C
C-----INITIALIZE POINTERS AND DIMENSIONS FOR ARRAYS THAT
C     MAY BE REFERENCED BUT MAY NOT OTHERWISE GET ALLOCATED
      LCBUF1 = 1
      LCBUF2 = 1
      LCCOFF = 1
      LCH = 1
      LCHANI = 1
      LCHFB = 1
      LCHOBS = 1
      LCIPLO = 1
      LCIPLP = 1
      LCIQOB = 1
      LCNDER = 1
      LCNQOB = 1
      LCOBADV = 1
      LCOBDRN = 1
      LCOBGHB = 1
      LCOBBAS = 1
      LCOBRIV = 1
      LCOBSE = 1
      LCOBSTR = 1
      LCOTIM = 1
      LCQCLS = 1
      LCROFF = 1
      LCRSPA = 1
      LCRSQA = 1
      LCSSAD = 1
      LCSSCH = 1
      LCSSDR = 1
      LCSSGB = 1
      LCSSGF = 1
      LCSSPI = 1
      LCSSRV = 1
      LCSSST = 1
      LCSSTO = 1
      LCWT = 1
      LCWTQ = 1
      LCWTQS = 1
      LCX=1
      LCXND = 1
      NDAR= 1
      NDMHAR = 1
      NOBADV = 0
      NQ = 0
      NQC = 0
      NQT = 0
      NQTDR = 0
      NQTGB = 0
      NQTRV = 0
      NQTST = 0
      NQTCH = 0
C
C     FOR DRT PACKAGE
      IOWTQDT = 0
      LCOBDRT = 1
      LCSSDT = 1
      NQTDT = 0
C
C     FOR SFR PACKAGE
      IOWTQSF = 0
      LCOBSFR = 1
      LCSSSF = 1
      NQTSF = 0
C
      RETURN
      END
C=======================================================================
      SUBROUTINE OBS1BAS6AL(IOUB,IOUT,ISCALS,ISEN,IUOBS,OUTNAM,ISOLDX,
     &                      ISOLDZ,ISOLDI,ISUMX,ISUMZ,ISUMIX,OBSALL)
C     VERSION 19990726 ERB
C     ******************************************************************
C     OBTAIN BASE NAME FOR GRAPH-DATA OUTPUT FILES AND READ ISCALS.
C     ALSO OPEN ._b FILE IF OUTNAM IS NOT 'NONE'
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER IOUT, IUOBS
      CHARACTER*200 LINE, OUTNAM, OUTNAMU
      CHARACTER*83 FN
      LOGICAL OBSALL
      INCLUDE 'parallel.inc'
C     ------------------------------------------------------------------
  500 FORMAT (/,' OBS1BAS6 -- OBSERVATION PROCESS, ',
     &        'VERSION 1.0, 4/27/99',/,' INPUT READ FROM UNIT ',I3)
  505 FORMAT (' COMPOSITE SCALED SENSITIVITIES WILL BE PRINTED')
  510 FORMAT (' UNSCALED OBSERVATION SENSITIVITIES WILL BE PRINTED')
  515 FORMAT (' DIMENSIONLESS SCALED OBSERVATION SENSITIVITIES WILL BE',
     &        ' PRINTED')
  520 FORMAT (' ONE-PERCENT SCALED OBSERVATION SENSITIVITIES WILL BE',
     &        ' PRINTED')
  530 FORMAT (' OBSERVATION GRAPH-DATA OUTPUT FILES WILL',
     &        ' NOT BE PRINTED')
  540 FORMAT (' OBSERVATION GRAPH-DATA OUTPUT FILES',/,
     &' WILL BE PRINTED AND NAMED USING THE BASE: ',A)
  545 FORMAT(' THE EXTENDED SET OF OUTPUT FILES WILL BE PRINTED')
  550 FORMAT(/,
     &       ' WARNING: ERROR IN OPENING PARAMETER-VALUE OUTPUT FILE',/,
     &       '   -- TABLE OF PARAMETER VALUES AND STATISTICS FOR ALL ',
     &       ' ITERATIONS WILL NOT BE',/,'   PRODUCED (OBS1BAS6AL)',/)
C
      ISOLDX = ISUMX
      ISOLDZ = ISUMZ
      ISOLDI = ISUMIX
      WRITE (IOUT,500) IUOBS
C
C     READ FILE BASE NAME FOR GRAPH-DATA OUTPUT FILES
      CALL URDCOM(IUOBS,IOUT,LINE)
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,IDUM,DUM,IOUT,IUOBS)
      OUTNAM = LINE(ISTART:ISTOP)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISCALS,DUM,IOUT,IUOBS)
C
      CALL UCASE(OUTNAM,OUTNAMU,1)
      IF (OUTNAMU.EQ.'NONE') THEN
        OUTNAM = OUTNAMU
        WRITE (IOUT,530)
      ELSE
        LENG = NONB_LEN(OUTNAM,200)
        WRITE (IOUT,540) OUTNAM(1:LENG)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,IDUM,DUM,IOUT,IUOBS)
        IF (LINE(ISTART:ISTOP) .EQ. 'ALLFILES') THEN
          OBSALL = .TRUE.
          WRITE (IOUT,545)
        ENDIF
      ENDIF
C
C     IF OUTNAM IS NOT 'NONE', LOCATE AN UNUSED UNIT FOR IOUB OUTPUT AND
C     OPEN IT
C
      IF (MYID.EQ.MPROC) THEN
        IF (OUTNAM.NE.'NONE') THEN
          IOUB = IGETUNIT(1,1000)
          IF (IOUB.GT.0) THEN
            FN = OUTNAM(1:LENG)//'._b'
            OPEN(IOUB,FILE=FN,ERR=20)
            CLOSE(UNIT=IOUB,STATUS='DELETE')
            OPEN(IOUB,FILE=FN,ERR=20)
          ELSE
            GOTO 20
          ENDIF
          GOTO 40
C         IF ERROR IN OPENING A FILE, TURN OFF OUTPUT FLAG, PRINT
C         WARNING, AND CONTINUE
   20     CONTINUE
          WRITE(IOUT,550)
          IOUB = 0
        ENDIF
      ENDIF
   40 CONTINUE
C
      IF (ISEN.GT.0) THEN
        IF (ISCALS.LT.0) WRITE (IOUT,505)
        IF (ISCALS.EQ.0) WRITE (IOUT,510)
        IF (ISCALS.EQ.1 .OR. ISCALS.EQ.3) WRITE (IOUT,515)
        IF (ISCALS.EQ.2 .OR. ISCALS.EQ.3) WRITE (IOUT,520)
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE OBS1BAS6AC(EV,ISUM,ISUMZ,ISUMI,LCTOFF,NH,LCH,ND,LCHOBS,
     &                      LCWT,NDMH,NDMHAR,LCWTQ,LCWTQS,LCW1,LCW2,LCX,
     &                      NPLIST,LCXD,IPAR,IOUT,IDRY,JDRY,NQ,NQAR,NQC,
     &                      NQCAR,NQT,NQTAR,NHAR,MOBS,MOBSAR,LCIBT,
     &                      LCNQOB,LCNQCL,LCIQOB,LCQCLS,LCIPLO,LCIPLP,
     &                      IPR,MPR,IPRAR,LCBUF1,LCSSTO,ITMXP,LBUFF,
     &                      LCOBSE,ISOLDX,ISOLDZ,ISOLDI,MXSEN,LCBUF2,
     &                      NDAR,NHT,LCRSQA,LCRSPA,LCXND,LCOTIM)
C     VERSION 20020709 ERB
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR OBSERVATIONS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER IDRY, IOUT, IPAR, ISP, ISUM, ISUMZ, ISUMI, JDRY,
     &        LCH, LCHOBS, LCIPLO, LCTOFF, LCW1, LCW2, LCWT, LCWTQ,
     &        LCWTQS, LCX, LCXD, LCXND, ND, NDMH, NH, NHT, NPLIST
C
      INCLUDE 'parallel.inc'
C     ------------------------------------------------------------------
C
C-----DEFINE NON-ZERO ARRAY DIMENSIONS
      NQAR = NQ
      IF (NQ.EQ.0) NQAR = 1
      NQCAR = NQC
      IF (NQC.EQ.0) NQCAR = 1
      NQTAR = NQT
      IF (NQT.EQ.0) NQTAR = 1
      NHAR = NH
      IF (NH.EQ.0) NHAR = 1
      MOBSAR = MOBS
      IF (MOBS.EQ.0) MOBSAR = 1
C
      IDRY = 0
      JDRY = 0
      ND = ND + NQT
C
C-------IF THERE ARE NO OBSERVATIONS, STOP WITH ERROR MESSAGE
      IF (ND.EQ.0) THEN
        WRITE (IOUT,480)
        CALL USTOP(' ')
      ENDIF
  480 FORMAT(1X,'ERROR: THE OBSERVATION PROCESS IS ACTIVE, BUT',
     &       ' NO OBSERVATIONS HAVE BEEN READ',/,
     &       '   -- STOP EXECUTION (OBS1BAS6AC)')
C
      ND = ND + 1
C----------ARRAYS USED FOR ALL DEPENDENT-VARIABLE DATA
      LCTOFF = ISUM
      ISUM = ISUM + ND
      LCH = ISUM
      ISUM = ISUM + ND + IPR + MPR
      LCHOBS = ISUM
      ISUM = ISUM + ND
      LCIPLO = ISUMI
      ISUMI = ISUMI + ND + IPR + MPR
      LCIPLP = ISUMI
      ISUMI = ISUMI + ND + IPR + MPR
      LCOBSE = ISUMI
      ISUMI = ISUMI + ND + IPR + MPR
      LCWT = ISUM
      ISUM = ISUM + ND
      NDMH = ND - NHT
      LCWTQ = ISUM
      ISUM = ISUM + NDMH*NDMH
      LCWTQS = ISUM
      ISUM = ISUM + NDMH*NDMH
      IF (NUMPROCS .GT. 1) THEN
        LCXND = ISUM
        ISUM = ISUM + ND
      ENDIF
      IF (IPAR.NE.1) THEN
C       (IF PES PROCESS IS ACTIVE, MEMORY FOR SSTO ARRAY WAS ALLOCATED
C       IN PESBAS1AL)
        LCSSTO = ISUM
        ISUM = ISUM + ITMXP + 1
      END IF
      LCRSQA = ISUM
      ISUM = ISUM + ITMXP + 1
      LCRSPA = ISUM
      ISUM = ISUM + ITMXP + 1
      LCW1 = ISUMZ
      ISUMZ = ISUMZ + NDMH*NDMH
      LCW2 = ISUMZ
      ISUMZ = ISUMZ + NDMH
      LCX = ISUM
      ISUM = ISUM + NPLIST*ND
      LCXD = ISUM
      ISUM = ISUM + NPLIST*ND
      LCBUF1 = ISUM
      LBUFF = IPRAR + 2*(ND+MPR+IPR) + 2*MXSEN
      ISUM= ISUM + LBUFF
      IF(LBUFF.LT.1) LBUFF = 1
      IF (MXSEN.GT.0) THEN
        LCBUF2 = ISUM
        ISUM = ISUM + MXSEN*ND
      ENDIF
      LCOTIM = ISUM
      ISUM = ISUM + ND
C------------FLOW-DATA ARRAYS
      IF (NQ.GT.0) THEN
        LCIBT = ISUMI
        ISUMI = ISUMI + NQ*2
        LCNQOB = ISUMI
        ISUMI = ISUMI + NQ
        LCNQCL = ISUMI
        ISUMI = ISUMI + NQ
      ENDIF
      IF (NQT.GT.0) THEN
        LCIQOB = ISUMI
        ISUMI = ISUMI + NQT
      ENDIF
      IF (NQC.GT.0) THEN
        LCQCLS = ISUM
        ISUM = ISUM + 5*NQC
      ENDIF
C----------RESTORE ORIGINAL VALUES
      ND = ND - 1
      IF (ND.GT.0) NDAR=ND
      NDMH = ND - NHT
      IF (NDMH.GT.0) NDMHAR = NDMH
C8------PRINT AMOUNT OF SPACE USED BY OBSERVATION PROCESS.
      ISP = ISUM - ISOLDX
      WRITE (IOUT,500) ISP
  500 FORMAT (/,1X,I10,' ELEMENTS IN X ARRAY ARE USED FOR OBSERVATIONS')
      ISP = ISUMZ - ISOLDZ
      WRITE (IOUT,501) ISP
  501 FORMAT (1X,I10,' ELEMENTS IN Z ARRAY ARE USED FOR OBSERVATIONS')
      ISP = ISUMI - ISOLDI
      WRITE (IOUT,502) ISP
  502 FORMAT (1X,I10,' ELEMENTS IN IX ARRAY ARE USED FOR OBSERVATIONS')
C
C-------SET MULTIPLICATIVE FACTOR FOR VARIANCES OF HEADS AND FLOWS
      EV = 1.0
      WRITE (IOUT,640) EV
 640  FORMAT (/,' COMMON ERROR VARIANCE FOR ALL OBSERVATIONS SET TO: ',
     &        G15.4)
C      RETURN
      RETURN
      END
C=======================================================================
      SUBROUTINE OBS1BAS6RP(ND,NDAR,NDMH,NDMHAR,NQCAR,QCLS,RSQO,RSQOO,
     &                      RSQP,WT,WTQ,WTQS,OTIME)
C     VERSION 20010924 ERB
C     ******************************************************************
C     INITIALIZE VARIABLES AND ARRAYS USED FOR OBSERVATIONS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER NDMHAR
      REAL WTQS(NDMHAR,NDMHAR)
      DIMENSION QCLS(5,NQCAR), WT(NDAR), WTQ(NDMHAR,NDMHAR)
      REAL OTIME(NDAR)
C     ------------------------------------------------------------------
C
      RSQO = 0.0
      RSQOO = 0.0
      RSQP = 0.0
C
C-------INITIALIZE WT, WTQ, AND OTIME
      DO 10 N = 1, ND
        WT(N) = 1.0
        OTIME(N) = -1.0
   10 CONTINUE
      IF (NDMH.GT.0) THEN
        DO 40 I = 1, NDMH
          DO 30 J = 1, NDMH
            WTQ(I,J) = 0.0
   30     CONTINUE
          WTQ(I,I) = 1.0
   40   CONTINUE
      ENDIF
C
C-----INITIALIZE WTQS
      DO 60 I=1,NDMHAR
        DO 50 J=1,NDMHAR
          WTQS(I,J) = 0.0
   50   CONTINUE
   60 CONTINUE
C
C-----INITIALIZE QCLS(5,n)
      DO 110 I = 1, NQCAR
          QCLS(5,I) = 0.0
 110  CONTINUE
C
      RETURN
      END
C=======================================================================
      SUBROUTINE OBS1BAS6CK(NC,ND,NQC,NT,NQT,IOUT,OBSNAM)
C     VERSION 030604 ERB
C     ******************************************************************
C-----CHECK OBSERVATION DATA AGAINST ALLOCATED STORAGE, AND CHECK FOR
C     DUPLICATE OBSERVATION NAMES
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*12 OBSNAM(ND), CTMP1, CTMP2
      CHARACTER(LEN=12), ALLOCATABLE :: NAM(:)
      INTEGER IERR, NC, NQC, NT, NQT, IOUT
C     ------------------------------------------------------------------
  550 FORMAT(/,1X,'*** WARNING:  THE OBSERVATION DATA SET CONTAINS',
     &' DUPLICATE OBSERVATION NAMES.')
  555 FORMAT(/,1X,'*** Duplicate OBSNAM: ',A)
  580 FORMAT (/,' NQC CAN BE REDUCED FROM',I5,' TO',I5)
  585 FORMAT (/,' NQT CAN BE REDUCED FROM',I5,' TO',I5)
  595 FORMAT (/,' NUMBER OF NODES IN THE NQ CELL GROUPS, ',I5,
     &        ' > NQC (',I5,')  OR  NUMBER FLOW OBSERVATIONS',I5,
     &        ' > NQT (',I5,') -- STOP EXECUTION (OBS1BAS6CK)',/)
  610 FORMAT (/,' NQT (SUM OF NQTRV, NQTDR, NQTGB, AND NQTST) IS ',I5,
     &        ' BUT NEEDS TO ',/,
     &        'EQUAL THE ACTUAL TOTAL NUMBER OF FLOW OBSERVATIONS,',
     &        I5,' -- STOP EXECUTION (OBS1BAS6CK)')
C
C     Allocate NAM array, populate it with uppercased OBSNAM values,
C     sort it, then the check for duplicate OBSNAM values need be done
C     only on adjacent elements.
      ALLOCATE(NAM(ND))
      DO I=1,ND
        CALL UCASE(OBSNAM(I),NAM(I),1)
      ENDDO
C     SORT NAM
      CALL SHELLSORT(NAM,ND)
C     LOOK FOR DUPLICATE OBSERVATION NAMES AND WRITE WARNING IF FOUND
      IWARN=0
      DO 50 I=1,ND-1
        IF (NAM(I).EQ.NAM(I+1)) THEN
          WRITE(IOUT,555) NAM(I)
          IWARN = IWARN+1
        ENDIF
   50 CONTINUE
      IF (IWARN.GT.0) WRITE(IOUT,550)
      DEALLOCATE(NAM)
C
      IERR = 0
C-------EXCEEDED STORAGE RESERVED FOR FLOW DATA?
      IF (NC.GT.NQC .OR. NT.GT.NQT) THEN
        WRITE (IOUT,595) NC, NQC, NT, NQT
        IERR = 1
      ENDIF
C-------ALLOCATED TOO MUCH STORAGE?
      IF (NQC.GT.NC) THEN
        WRITE (IOUT,580) NQC, NC
        NQC = NC
      ENDIF
      IF (NQT.GT.NT) THEN
        WRITE (IOUT,585) NQT, NT
        WRITE (IOUT,610) NQT, NT
        IERR = 1
      ENDIF
      IF (IERR.NE.0) CALL USTOP(' ')
C
      RETURN
      END
C=======================================================================
      SUBROUTINE OBS1BAS6QM(NDMH,WTQ,WTQS,DTLWTQ,W1,W2,EV,IOWTQ,
     &                      IPRN,IOUT,NDMHAR,OBSALL,OUTNAM,ND,NH,
     &                      WT)
C     VERSION 19981019 ERB
C     ******************************************************************
C     CALCULATE THE INVERSE AND THE SQUARE-ROOT OF THE INVERSE OF THE
C     COVARIANCE MATRIX OF HEAD DEPENDENT FLOWS FROM SVD (I.E. DECOMPO-
C     SITION OF THE COVARIANCE MATRIX INTO MATRIX OF EIGENVECTORS AND
C     DIAGONAL MATRIX OF CORRESPONDING EIGENVALUES).
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL DTLWTQ, EV, TMP, WTQ, WTQS, WT
      INTEGER I, IOUT, IOWTQ, IPRN, J, NDMH
      CHARACTER*200 FN, OUTNAM
      CHARACTER*16 ANAME
      DOUBLE PRECISION W1(NDMHAR,NDMHAR), W2(NDMHAR)
      DIMENSION WTQ(NDMHAR,NDMHAR), WTQS(NDMHAR,NDMHAR), WT(ND)
      LOGICAL OBSALL
      DATA ANAME/'                '/
C     ------------------------------------------------------------------
  500 FORMAT (//,6X,'WEIGHT MATRIX FOR HEAD DEP. FLOWS',/,6X,75('-'))
  505 FORMAT (//,6X,'SQUARE-ROOT OF FLOW WEIGHT MATRIX',/,6X,75('-'))
  520 FORMAT(1X,A)
  525 FORMAT(I10)
  530 FORMAT(1X,G14.7)
C
      IF (IOWTQ.LT.1) THEN
        DTLWTQ = 0.
        DO 10 I = 1, NDMH
          WTQ(I,I) = EV/WTQ(I,I)
          WTQS(I,I) = SQRT(WTQ(I,I))
          DTLWTQ = DTLWTQ + LOG(WTQ(I,I))
   10   CONTINUE
C
        IF (OBSALL) THEN
C         WRITE WEIGHTS TO _wt FILE
          IWTF = IGETUNIT(7,1000)
          FN = TRIM(OUTNAM)//'._wt'
          OPEN(IWTF,FILE=FN)
C         WEIGHTS ARE UNCORRELATED.  WRITE WT AND DIAGONAL TERMS OF
C         WTQ ONLY.
          WRITE(IWTF,520) 'VECTOR'
          WRITE(IWTF,525) ND
          IF (NH.GT.0) THEN
            WRITE(IWTF,530) (WT(I),I=1,NH)
          ENDIF
          IF (NDMH.GT.0) THEN
            WRITE(IWTF,530) (WTQ(I,I),I=1,NDMH)
          ENDIF
          CLOSE(IWTF)
        ENDIF
C
        RETURN
      ENDIF
      IF (NDMH.EQ.1) THEN
        WTQ(1,1) = EV/WTQ(1,1)
        WTQS(1,1) = SQRT(WTQ(1,1))
        DTLWTQ = LOG(WTQ(1,1))
        IF (IPRN.GE.0) THEN
          WRITE (IOUT,505)
          CALL ULAPRW(WTQS,ANAME,0,0,NDMH,NDMH,0,IPRN,IOUT)
        ENDIF
        RETURN
      ENDIF
C     DECOMPOSE
      CALL SVD (NDMH,WTQ,WTQS,DTLWTQ,W1,W2,NDMHAR)
C     MULTIPLY BY COMMON ERROR VARIANCE
      DTLWTQ = EV*DTLWTQ
      TMP = SQRT(EV)
      DO 70 I = 1, NDMH
        DO 60 J = 1, NDMH
          WTQ(I,J) = EV*WTQ(I,J)
          WTQ(I,J) = TMP*WTQ(I,J)
   60   CONTINUE
   70 CONTINUE
C     PRINT
      IF (IPRN.GE.0) THEN
        WRITE (IOUT,500)
        CALL ULAPRW(WTQ,ANAME,0,0,NDMH,NDMH,0,IPRN,IOUT)
        WRITE (IOUT,505)
        CALL ULAPRW(WTQS,ANAME,0,0,NDMH,NDMH,0,IPRN,IOUT)
      ENDIF
C
      IF (OBSALL) THEN
C       WRITE WEIGHT MATRIX TO _wt FILE
        IWTF = IGETUNIT(7,1000)
        FN = TRIM(OUTNAM)//'._wt'
        OPEN(IWTF,FILE=FN)
        WRITE(IWTF,520) 'COMPRESSEDMATRIX'
CERB    Write WT and WTQ to _wt file in compressed form
        CALL SOBS1BAS6WCM(IOUT,IWTF,ND,NDMH,NH,WT,WTQ)
        CLOSE(IWTF)
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE OBS1BAS6FM(H,ND,NDAR,NDMH,NDMHAR,WT,WTQ)
C     VERSION 20020708 ERB
C     ******************************************************************
C     INITIALIZE SIMULATED-EQUIVALENTS ARRAY AND ENSURE THAT THE WT
C     ARRAY AND THE DIAGONAL ELEMENTS OF THE WTQ ARRAY CONTAIN NO
C     NEGATIVE NUMBERS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER ND, NDAR, NDMH, NDMHAR
      REAL H, WT, WTQ
      DIMENSION H(NDAR), WT(NDAR), WTQ(NDMHAR,NDMHAR)
C     ------------------------------------------------------------------
C
      IF (ND.GT.0) THEN
        DO 10 I = 1, ND
          H(I) = 0.0
          IF (WT(I).LT.0.0) WT(I) = -WT(I)
   10   CONTINUE
      ENDIF
C
      IF (NDMH.GT.0) THEN
        DO 20 I=1, NDMH
          IF (WTQ(I,I).LT.0.0) WTQ(I,I) = -WTQ(I,I)
   20   CONTINUE
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE OBS1BAS6DR(ND,NPE,X)
C     VERSION 19991220 ERB
C     ******************************************************************
C     INITIALIZE SENSITIVITY ARRAY
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER ND, NPE
      REAL X
      DIMENSION X(NPE,ND)
C     ------------------------------------------------------------------
C
      DO 20 N = 1,ND
        DO 10 IIP = 1, NPE
          X(IIP,N) = 0.0
   10   CONTINUE
   20 CONTINUE
C
      RETURN
      END
C=======================================================================
      SUBROUTINE OBS1BAS6SS(IOUT,NPE,NH,OBSNAM,KPER,KSTP,BUF1,X,H,WT,
     &                      HOBS,IPRINT,IFO,ITERP,IPAR,NPER,LN,LASTX,
     &                      ISCALS,WP,MPR,PRM,RSQ,RSQP,IPR,NIPR,WTPS,ND,
     &                      WTQ,WTQS,IOWTQ,NDMH,NTT2,KTDIM,
     &                      IOSTAR,NPLIST,NSTP,MPRAR,IPRAR,OUTNAM,
     &                      IPLOT,EQNAM,NAMES,IPLPTR,NDMHAR,NQTDR,NQTRV,
     &                      NQTGB,NQTST,NQTCH,IOWTQCH,IOWTQDR,IOWTQRV,
     &                      IOWTQGB,IOWTQST,LCOBBAS,LCOBDRN,LCOBRIV,
     &                      LCOBGHB,LCOBSTR,LCOBCHD,LCOBADV,SSGF,SSDR,
     &                      SSRV,SSGB,SSST,SSAD,SSCH,SSPI,SSTO,ITMXP,
     &                      IOUTG,BUF2,IPES,BPRI,BSCAL,RSQA,RSPA,
     &                      LCOBDRT,SSDT,NQTDT,IOWTQDT,NRSO,NPOST,NNEGT,
     &                      NRUNS,NQTSF,IOWTQSF,LCOBSFR,SSSF,NHT,OTIME,
     &                      OBSALL)
C
C     VERSION 20000313 ERB
C     ******************************************************************
C     PRINT DATA FOR OBSERVED HEAD POINTS AND FOR OBSERVED FLOWS IF THIS
C     IS THE FINAL TIME STEP.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL BUF1, BPRI, H, HOBS, PRM, RSQ, RSQP, WP, WT, WTQ,
     &     WTQS, X
      INTEGER IOSTAR, KTDIM, NPLIST, NSTP, NTT2
      INTEGER IFO, IO, IOUT, IOWTQ, IPAR, IPR, IPRINT, ISCALS, ITERP,
     &        KPER, LASTX, LN, MPR, ND, NDMH, NH, NHT, NIPR, NPE, NPER
      INTEGER IPLOT(ND+IPR+MPR), IPLPTR(ND+IPR+MPR)
      CHARACTER*10 EQNAM(MPRAR)
      CHARACTER*12 OBSNAM(ND), NAMES(ND+IPR+MPR)
      CHARACTER*200 OUTNAM
      DIMENSION BUF1(IPRAR+2*(ND+MPR+IPR)), BUF2(NPE,ND), X(NPE,ND),
     &          H(ND), WT(ND), HOBS(ND), BPRI(IPRAR),
     &          LN(NPLIST), WP(MPRAR), PRM(NPLIST+1,MPRAR),
     &          NIPR(IPRAR), NSTP(NPER),
     &          SSGF(ITMXP+1), SSDR(ITMXP+1), SSRV(ITMXP+1),
     &          SSGB(ITMXP+1), SSST(ITMXP+1), SSAD(ITMXP+1),
     &          SSCH(ITMXP+1), SSPI(ITMXP+1), SSTO(ITMXP+1),
     &          BSCAL(NPLIST), RSQA(ITMXP+1), RSPA(ITMXP+1)
      DIMENSION WTQ(NDMHAR,NDMHAR), WTQS(NDMHAR,NDMHAR),
     &          WTPS(IPRAR,IPRAR)
      DIMENSION SSDT(ITMXP+1), SSSF(ITMXP+1)
      REAL OTIME(ND)
      LOGICAL OBSALL
C
      INCLUDE 'param.inc'
      INCLUDE 'parallel.inc'
C     ------------------------------------------------------------------
C
  500 FORMAT (4G14.7)
  555 FORMAT (/,'   TOTAL SUM OF SQUARED, WEIGHTED RESIDUALS:',G10.3,/)
C
C-----CALCULATE THE SUM OF SQUARED RESIDUALS AND PRINT RESIDUALS FOR
C-----OBSERVATIONS AND PRIOR INFORMATION
C-------SET FLAG WHICH CONTROLS PRINTING OF RESIDUALS
      IO = 0
      ISSWR = ITERP
      IF (IFO.NE.0) ISSWR = ITERP + 1
      IF (KPER.EQ.NPER .AND. KSTP.EQ.NSTP(NPER)) THEN
        IF (IPAR.GE.0 .AND. (IPRINT.NE.0.OR.ITERP.EQ.1)) IO = 1
        IF (IPAR.LT.0 .OR. IFO.NE.0) IO = 2
        CALL SOBS1BAS6OH(WP,IOUT,NH,H,HOBS,WT,OBSNAM,ND,MPR,PRM,RSQ,
     &                   RSQP,IO,LN,IPR,NIPR,WTPS,
     &                   BUF1(IPRAR+1),BUF1(IPRAR+ND+MPR+IPR+1),WTQ,
     &                   WTQS,NDMH,NTT2,KTDIM,NPLIST,MPRAR,IPRAR,OUTNAM,
     &                   IPLOT,EQNAM,NAMES,IPLPTR,NDMHAR,NQTDR,NQTRV,
     &                   NQTGB,NQTST,NQTCH,IOWTQCH,IOWTQDR,IOWTQRV,
     &                   IOWTQGB,IOWTQST,LCOBBAS,LCOBDRN,LCOBRIV,
     &                   LCOBGHB,LCOBSTR,LCOBCHD,LCOBADV,ISSWR,SSGF,
     &                   SSDR,SSRV,SSGB,SSST,SSAD,SSCH,SSPI,SSTO,ITMXP,
     &                   IPES,BPRI,LCOBDRT,SSDT,NQTDT,IOWTQDT,NRSO,
     &                   NPOST,NNEGT,NRUNS,NQTSF,IOWTQSF,LCOBSFR,SSSF,
     &                   NHT,OTIME)
        RSQA(ISSWR) = RSQ
        RSPA(ISSWR) = RSQP
        IF (IOSTAR.NE.1 .AND. MYID.EQ.MPROC) WRITE (*,555) RSQP
C
C-------IF REQUESTED, PRINT UNSCALED, DIMENSIONLESS SCALED, OR
C       ONE-PERCENT SCALED SENSITIVITIES
        IF (IFO.NE.0 .AND. LASTX.EQ.0) THEN
C         PRINT OBS-SEN TABLE(S) TO THE GLOBAL FILE
          IF (MYID.EQ.MPROC)
     &     CALL SOBS1BAS6ST(BUF1,IOUTG,IOWTQ,IPLOT,IPR,ISCALS,LN,MPR,ND,
     &                      NDMH,NDMHAR,NHT,NPE,NPLIST,OBSNAM,OUTNAM,WT,
     &                      WTQ,WTQS,X,ITERP,BUF2,BSCAL,OBSALL)
        ENDIF
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE OBS1BAS6IQ(QCLS,NQCAR)
C-----VERSION 20000517 ERB
C     ******************************************************************
C     INITIALIZE QCLS ARRAY ELEMENT 5 TO 0.0
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL QCLS(5,NQCAR)
C     ------------------------------------------------------------------
      DO 10 I = 1, NQCAR
        QCLS(5,I) = 0.0
   10 CONTINUE
C
      RETURN
      END
C=======================================================================
      SUBROUTINE OBS1BAS6OT(IOUT,IOUTG,NPE,NH,OBSNAM,BUF1,X,H,WT,HOBS,
     &                      IPRINT,IFO,ITERP,IPAR,LN,ISCALS,WP,MPR,PRM,
     &                      RSQ,RSQP,RSQO,RSQOO,SOSC,SOSR,IPR,NIPR,
     &                      WTPS,ND,WTQ,WTQS,IOWTQ,NDMH,NTT2,KTDIM,
     &                      NPLIST,MPRAR,IPRAR,OUTNAM,IPLOT,EQNAM,NAMES,
     &                      IPLPTR,NDMHAR,NQTDR,NQTRV,NQTGB,NQTST,NQTCH,
     &                      IOWTQCH,IOWTQDR,IOWTQRV,IOWTQGB,IOWTQST,
     &                      LCOBBAS,LCOBDRN,LCOBRIV,LCOBGHB,LCOBSTR,
     &                      LCOBCHD,LCOBADV,SSGF,SSDR,SSRV,SSGB,SSST,
     &                      SSAD,SSCH,SSPI,SSTO,ITMXP,BUF2,IPES,BPRI,
     &                      BSCAL,LCOBDRT,SSDT,NQTDT,IOWTQDT,NRSO,NPOST,
     &                      NNEGT,NRUNS,NQTSF,IOWTQSF,LCOBSFR,SSSF,NHT,
     &                      OTIME,OBSALL)
C
C     VERSION 20000609 ERB
C     ******************************************************************
C     CHECK FOR PARAMETER-ESTIMATION CONVERGENCE BY SOSC CRITERION.
C     PRINT DATA FOR OBSERVED HEAD POINTS AND FOR OBSERVED FLOWS.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL BUF1, BPRI, H, HOBS, PRM, RSQ, RSQO, RSQOO, RSQP,
     &     SOSC, SOSR, TEMP, WP, WT, WTQ, WTQS, X
      INTEGER KTDIM, NPLIST, NTT2
      INTEGER IFO, IOUT, IOUTG, IOWTQ, IPAR, IPR, IPRINT, ISCALS, ITERP,
     &        LN, MPR, ND, NDMH, NH, NHT, NIPR, NPE
      INTEGER IPLOT(ND+IPR+MPR), IPLPTR(ND+IPR+MPR)
      CHARACTER*10 EQNAM(MPRAR)
      CHARACTER*12 OBSNAM(ND), NAMES(ND+IPR+MPR)
      CHARACTER*200 OUTNAM
      DIMENSION BUF1(IPRAR+2*(ND+MPR+IPR)+2*NPE), BUF2(NPE,ND),
     &          X(NPE,ND), H(ND), WT(ND), HOBS(ND), BPRI(IPRAR),
     &          LN(NPLIST), WP(MPRAR), PRM(NPLIST+1,MPRAR),
     &          NIPR(IPRAR), SSGF(ITMXP+1), SSDR(ITMXP+1),
     &          SSRV(ITMXP+1), SSGB(ITMXP+1), SSST(ITMXP+1),
     &          SSAD(ITMXP+1), SSCH(ITMXP+1), SSPI(ITMXP+1),
     &          SSTO(ITMXP+1), BSCAL(NPLIST)
      DIMENSION WTQ(NDMHAR,NDMHAR), WTQS(NDMHAR,NDMHAR),
     &          WTPS(IPRAR,IPRAR)
      DIMENSION SSDT(ITMXP+1), SSSF(ITMXP+1)
      REAL OTIME(ND)
      LOGICAL OBSALL
C
      INCLUDE 'param.inc'
      INCLUDE 'parallel.inc'
C     ------------------------------------------------------------------
C
  545 FORMAT (/,' PARAMETER ESTIMATION CONVERGED BECAUSE SUM OF',
     &        ' SQUARED, WEIGHTED RESIDUALS',/,
     &        4X,'HAS NOT CHANGED',F7.4,' PERCENT IN 2 ITERATIONS',/)
  550 FORMAT (/,' ADD R MATRIX OF EQUATION (51) TO EQUATION',
     &       '  (50) OF TEXT FOR ALL SUBSEQUENT',/,
     &       4X,'ITERATIONS BECAUSE THE SUM OF SQUARED, WEIGHTED',
     &       ' RESIDUALS HAS NOT CHANGED',/,
     &       4X,'MORE THAN',F7.4,' PERCENT IN 2 ITERATIONS')
  552 FORMAT(1X,'*** WARNING: COMPOSITE SCALED SENSITIVITY = 0.0 FOR',
     &    ' PARAMETER "',A,'"')
  553 FORMAT(1X,'*** ERROR: COMPOSITE SCALED SENSITIVITY = 0.0 FOR',
     &    ' PARAMETER "',A,'"')
  556 FORMAT(/,1X,'PARAMETER(S) LISTED IN WARNING(S) ABOVE',
     &    ' APPARENTLY IS (ARE) UNUSED.',/,' EXECUTION TIME',
     &    ' CAN BE SAVED',
     &    ' BY TURNING OFF CALCULATION OF SENSITIVITIES',/,
     &    ' FOR LISTED PARAMETER(S).')
  557 FORMAT(/,1X,'PARAMETER(S) LISTED IN ERROR(S) ABOVE',
     &    ' APPARENTLY IS (ARE) UNUSED.  TO ALLOW',/,
     &    ' PARAMETER ESTIMATION TO CONTINUE, ELIMINATE PARAMETER(S)',
     &    ' OR SET ISENS <= 0 IN',/,
     &    ' THE SENSITIVITY PROCESS INPUT FILE.')
  560 FORMAT (/,' STARTING VALUES OF REGRESSION PARAMETERS :',/)
  570 FORMAT (6(3X,A10))
  580 FORMAT (6(2X,1PG11.4))
  600 FORMAT (/,' SUMS OF SQUARED, WEIGHTED RESIDUALS:',/,
     &'   ALL DEPENDENT VARIABLES: ',G12.5,/,
     &'   DEP. VARIABLES PLUS PARAMETERS: ',G12.5)
C
C-----CALCULATE THE SUM OF SQUARED RESIDUALS AND PRINT RESIDUALS FOR
C-----OBSERVATIONS AND PRIOR INFORMATION
C-----TEST FOR REDUCTION IN SUM OF SQUARED, WEIGHTED RESIDUALS
      IF (IPAR.GT.0 .AND. (SOSC.GT.0..OR.SOSR.GT.0.)) THEN
        IF (ITERP.GT.2) THEN
          TEMP = (ABS(RSQP-RSQO)/RSQP) + (ABS(RSQO-RSQOO)/RSQO)
          IF (IFO.EQ.0 .AND. TEMP.LT.SOSC) THEN
            IFO = 2
            WRITE (IOUTG,545) SOSC*100.
            CALL SOBS1BAS6OH(WP,IOUT,NH,H,HOBS,WT,OBSNAM,ND,MPR,PRM,
     &                       RSQ,RSQP,1,LN,IPR,NIPR,WTPS,
     &                       BUF1(IPRAR+1),BUF1(IPRAR+ND+MPR+IPR+1),WTQ,
     &                       WTQS,NDMH,NTT2,KTDIM,NPLIST,MPRAR,IPRAR,
     &                       OUTNAM,IPLOT,EQNAM,NAMES,IPLPTR,NDMHAR,
     &                       NQTDR,NQTRV,NQTGB,NQTST,NQTCH,IOWTQCH,
     &                       IOWTQDR,IOWTQRV,IOWTQGB,IOWTQST,LCOBBAS,
     &                       LCOBDRN,LCOBRIV,LCOBGHB,LCOBSTR,LCOBCHD,
     &                       LCOBADV,0,SSGF,SSDR,SSRV,SSGB,SSST,SSAD,
     &                       SSCH,SSPI,SSTO,ITMXP,IPES,BPRI,LCOBDRT,
     &                       SSDT,NQTDT,IOWTQDT,NRSO,NPOST,NNEGT,NRUNS,
     &                       NQTSF,IOWTQSF,LCOBSFR,SSSF,NHT,OTIME)
          ENDIF
          IF (IFO.EQ.0 .AND. TEMP.LT.SOSR) THEN
            WRITE (IOUTG,550) SOSR*100.
            SOSR = -SOSR
          ENDIF
        ENDIF
        IF (ITERP.GT.1) RSQOO = RSQO
        IF (ITERP.GT.0) RSQO = RSQP
      ENDIF
C-----IF REQUESTED, PRINT UNSCALED OR SCALED OBSERVATION SENSITIVITIES
      IF (IPAR.NE.-1) THEN
        IF (IFO.NE.0 .OR. IPRINT.NE.0 .OR. ITERP.EQ.1) THEN
          IF (MYID.EQ.MPROC) THEN
            CALL SOBS1BAS6ST(BUF1,IOUTG,IOWTQ,IPLOT,IPR,ISCALS,LN,MPR,
     &                       ND,NDMH,NDMHAR,NHT,NPE,NPLIST,OBSNAM,
     &                       OUTNAM,WT,WTQ,WTQS,X,ITERP,BUF2,BSCAL,
     &                       OBSALL)
C           LOOP THROUGH COMPOSITE SCALED SENSITIVITIES.
C           IF ANY VALUE=0, PRINT WARNING OR ERROR FOR THAT PARAMETER.
            KERR = 0
            DO 100 IP=1,NPE
              IF (BUF1(IP).EQ.0.0) THEN
                IF (KERR.EQ.0) WRITE(IOUTG,'(1X)')
                IF (IPAR.LT.1) THEN
                  WRITE(IOUTG,552)PARNAM(IPPTR(IP))
                ELSE
                  WRITE(IOUTG,553)PARNAM(IPPTR(IP))
                ENDIF
                KERR = KERR+1
              ENDIF
  100       CONTINUE
            IF (KERR.GT.0) THEN
              IF (IPAR.LT.1) THEN
                WRITE(IOUTG,556)
              ELSE
                WRITE(IOUTG,557)
                CALL USTOP(' ')
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C
C
C     IF THIS IS THE FIRST ITERATION, PRINT STARTING PARAMETER VALUES
      IF (IPAR.GT.0 .AND. ITERP.EQ.1) THEN
        WRITE (IOUTG,560)
        WRITE (IOUTG,570) (PARNAM(IPPTR(IP)),IP=1,NPE)
        WRITE (IOUTG,'(1X)')
        WRITE (IOUTG,580) (B(IPPTR(IP)),IP=1,NPE)
      ENDIF
C
C     PRINT SUMS OF SQUARED, WEIGHTED RESIDUALS
      IF (IPAR.GT.0 .AND. IFO.EQ.0) WRITE (IOUTG,600) RSQ,RSQP
C
      RETURN
      END
C=======================================================================
      SUBROUTINE OBS1BAS6OH(WP,IOUT,NH,H,HOBS,WT,OBSNAM,ND,MPR,PRM,RSQ,
     &                      RSQP,IOIN,LN,IPR,NIPR,WTPS,
     &                      D,R,WTQ,WTQS,NDMH,NTT2,KTDIM,NPLIST,MPRAR,
     &                      IPRAR,OUTNAM,IPLOT,EQNAM,NAMES,IPLPTR,
     &                      NDMHAR,NQTDR,NQTRV,NQTGB,NQTST,NQTCH,
     &                      IOWTQCH,IOWTQDR,IOWTQRV,IOWTQGB,IOWTQST,
     &                      LCOBBAS,LCOBDRN,LCOBRIV,LCOBGHB,LCOBSTR,
     &                      LCOBCHD,LCOBADV,ISSWR,SSGF,SSDR,SSRV,SSGB,
     &                      SSST,SSAD,SSCH,SSPI,SSTO,ITMXP,IPES,BPRI,
     &                      LCOBDRT,SSDT,NQTDT,IOWTQDT,NRSO,NPOST,NNEGT,
     &                      NRUNS,NQTSF,IOWTQSF,LCOBSFR,SSSF,NHT,OTIME)
C     VERSION 20020823 ERB
C     ******************************************************************
C     CALL SOBS1BAS6OH TO CALCULATE AND PRINT WEIGHTED RESIDUALS FOR
C     DEPENDENT-VARIABLE OBSERVATIONS, PRIOR PARAMETER ESTIMATES, AND
C     PRIOR ESTIMATES OF PARAMETER SUMS.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL BPRI, D, H, HOBS, PRM, R, RSQ, RSQP,
     &     WP, WT, WTQ, WTQS
      INTEGER IOIN, IOUT, IPR, KTDIM, LN, MPR, ND,
     &        NDMH, NH, NHT, NIPR, NNEGT, NPOST, NRSO, NRUNS, NTT2
      INTEGER IPLOT(ND+IPR+MPR), IPLPTR(ND+IPR+MPR)
      CHARACTER*10 EQNAM(MPRAR)
      CHARACTER*12 OBSNAM(ND), NAMES(ND+IPR+MPR)
      CHARACTER*200 OUTNAM
      DIMENSION WP(MPRAR), H(ND), HOBS(ND), WT(ND),
     &          PRM(NPLIST+1,MPRAR), LN(NPLIST), NIPR(IPRAR),
     &          D(ND+MPR+IPR), R(ND+MPR+IPR),
     &          SSGF(ITMXP+1), SSDR(ITMXP+1), SSRV(ITMXP+1),
     &          SSGB(ITMXP+1), SSST(ITMXP+1), SSAD(ITMXP+1),
     &          SSCH(ITMXP+1), SSPI(ITMXP+1), SSTO(ITMXP+1), BPRI(IPRAR)
      DIMENSION WTQ(NDMHAR,NDMHAR), WTQS(NDMHAR,NDMHAR),
     &          WTPS(IPRAR,IPRAR)
      DIMENSION SSDT(ITMXP+1), SSSF(ITMXP+1)
      REAL OTIME(ND)
C
      CALL SOBS1BAS6OH(WP,IOUT,NH,H,HOBS,WT,OBSNAM,ND,MPR,PRM,RSQ,
     &                 RSQP,IOIN,LN,IPR,NIPR,WTPS,
     &                 D,R,WTQ,WTQS,NDMH,NTT2,KTDIM,NPLIST,MPRAR,
     &                 IPRAR,OUTNAM,IPLOT,EQNAM,NAMES,IPLPTR,
     &                 NDMHAR,NQTDR,NQTRV,NQTGB,NQTST,NQTCH,
     &                 IOWTQCH,IOWTQDR,IOWTQRV,IOWTQGB,IOWTQST,LCOBBAS,
     &                 LCOBDRN,LCOBRIV,LCOBGHB,LCOBSTR,LCOBCHD,
     &                 LCOBADV,ISSWR,SSGF,SSDR,SSRV,SSGB,SSST,SSAD,
     &                 SSCH,SSPI,SSTO,ITMXP,IPES,BPRI,LCOBDRT,SSDT,
     &                 NQTDT,IOWTQDT,NRSO,NPOST,NNEGT,NRUNS,NQTSF,
     &                 IOWTQSF,LCOBSFR,SSSF,NHT,OTIME)
      RETURN
      END
C=======================================================================
      SUBROUTINE OBS1BAS6NC(BUF1,BUF2,IOUTG,IOWTQ,IPLOT,IPR,ISCALS,
     &                      ITERP,LN,MPR,ND,NDMH,NDMHAR,NHT,NPE,NPLIST,
     &                      OBSNAM,OUTNAM,WT,WTQ,WTQS,X,BSCAL,OBSALL)
C     VERSION 20000313 ERB
C     ******************************************************************
C     IF PARAMETER ESTIMATION DOES NOT CONVERGE, CALL ROUTINE THAT
C     PRINTS OBSERVATION-SENSITIVITY TABLES
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*12 OBSNAM(ND)
      CHARACTER*200 OUTNAM
      DIMENSION BUF1(NPE), BUF2(NPE,ND), IPLOT(ND+IPR+MPR),
     &          LN(NPLIST), WT(ND), WTQ(NDMHAR,NDMHAR),
     &          WTQS(NDMHAR,NDMHAR), X(NPE,ND), BSCAL(NPLIST)
      LOGICAL OBSALL
      INCLUDE 'parallel.inc'
C     ------------------------------------------------------------------
      IF (MYID.EQ.MPROC) THEN
        CALL SOBS1BAS6ST(BUF1,IOUTG,IOWTQ,IPLOT,IPR,ISCALS,LN,MPR,ND,
     &                   NDMH,NDMHAR,NHT,NPE,NPLIST,OBSNAM,OUTNAM,WT,
     &                   WTQ,WTQS,X,ITERP,BUF2,BSCAL,OBSALL)
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE OBS1BAS6PR1(IFO,IOUTG,ITERPK,ITERSS,ITMXP,IUSS,NPAR,
     &                       OUTNAM)
C
C     VERSION 20010612 ERB
C     ******************************************************************
C     OPEN _ss FILE
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER IFO, IOUTG, ITERPK, ITERSS, ITMXP, IUSS, LENGNAM, NPAR
      CHARACTER*200 OUTNAM
      CHARACTER*84 FN
      DIMENSION NPAR(ITMXP+1)
C     ------------------------------------------------------------------
  630 FORMAT (/,' WARNING: ERROR IN OPENING FILE: ',A,' (OBS1BAS6PR1)')
C
C     DETERMINE NUMBER OF ITERATIONS TO BE PRINTED TO _ss FILE
      LAST = 0
      DO 100 ITERSS = 1, ITERPK
        IF (NPAR(ITERSS) .EQ. 0) LAST = 1
        IF (LAST.EQ.1) GOTO 120
  100 CONTINUE
  120 CONTINUE
      IF (IFO.EQ.0) ITERSS = ITERSS - 1
C
      LENGNAM = NONB_LEN(OUTNAM,200)
C       FIND AN UNUSED FILE UNIT AND OPEN THE _ss FILE
      FN = OUTNAM(1:LENGNAM)//'._ss'
      IUSS = IGETUNIT(1,1000)
      IF (IUSS.GT.0) THEN
        OPEN(IUSS,FILE=FN,ERR=150)
      ELSE
        GOTO 150
      ENDIF
  140 CONTINUE
      GOTO 160
  150 CONTINUE
      WRITE(IOUTG,630) FN
  160 CONTINUE
C
      RETURN
      END
C=======================================================================
      SUBROUTINE OBS1BAS6PR2(IPR,ITERSS,ITMXP,IUSS,MPR,SSTO)
C
C     VERSION 20010612 ERB
C     ******************************************************************
C     WRITE TOTAL SSWR TO _ss FILE
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER IPR, ITMXP, IUSS, MPR
      LOGICAL LOP
      DIMENSION SSTO(ITMXP+1)
C     ------------------------------------------------------------------
  660 FORMAT(1X,'ITERATION',2X,A)
  670 FORMAT(1X,I5,6X,G14.7)
C
      INQUIRE(UNIT=IUSS,OPENED=LOP)
      IF (LOP) THEN
C       CHOOSE HEADER TO WRITE
        IF (MPR.GT.0 .OR. IPR.GT.0) THEN
          WRITE (IUSS,660)
     &        'SSWR-(TOTAL FOR ALL OBSERVATIONS AND PRIOR INFORMATION)'
        ELSE
          WRITE (IUSS,660)'SSWR-(TOTAL FOR ALL OBSERVATIONS)'
        ENDIF
C
C       WRITE TOTAL SSWR FOR EACH ITERATION
        DO 10 IT = 1, ITERSS
          WRITE(IUSS,670) IT,SSTO(IT)
   10   CONTINUE
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE OBS1BAS6RE(WP,IOUTG,IOUT,NHT,H,HOBS,WT,NDMH,ND,IPAR,
     &                      MPR,PRM,IPR,NIPR,WTPS,BUF1,LBUFF,WTQ,WTQS,
     &                      NPLIST,MPRAR,IPRAR,NDMHAR,NAMES,IOBSEQ,
     &                      BPRI,RSQP,NRSO,NPOST,NNEGT,NRUNS)
C-----VERSION 1001 01JUN1993
C     VERSION 20000509 ERB
C     ******************************************************************
C     MEASURE OF NORMALITY AND INDEPENDENCE
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL AVE, AVET, BDIF, BDIF1, BDIF2, BPRI, BUF1, BWP, DEN1, DEN2,
     &     DIF, H, HOBS, PRM, RES, RMIN, RNORM, RNUM, STAT1, STAT2,
     &     TEMP, TEMP1, TMP, U, W, WP, WPSR, WT, WT2, WTQ, WTQS, WTR
      INTEGER I, I1, IIP, IMP, IOBSEQ, IOUT, IOUTG, IPAR, IPR, J, L, L1,
     &        MPR, N, ND, NHT, NIPR, NMIN, NN, NND, NQ2, NDMH
      CHARACTER*12 NAMES(ND+IPR+MPR), NAML, NAMLU, NAMS, NAMSU, OBSMIN,
     &             PNAMU
      DIMENSION WP(MPRAR), H(ND+IPR+MPR),
     &          HOBS(ND), WT(ND), PRM(NPLIST+1,MPRAR), NIPR(IPRAR),
     &          BUF1(LBUFF), IOBSEQ(ND+IPR+MPR), BPRI(IPRAR)
      DIMENSION WTQ(NDMHAR,NDMHAR), WTQS(NDMHAR,NDMHAR),
     &          WTPS(IPRAR,IPRAR)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
  440 FORMAT (/,' ORDERED DEPENDENT-VARIABLE WEIGHTED RESIDUALS',/,
     &        ' NUMBER OF RESIDUALS INCLUDED:',I10)
  460 FORMAT (/,1X,'SMALLEST AND LARGEST WEIGHTED RESIDUALS',//,
     &6X,'SMALLEST WEIGHTED RESIDUALS',6X,'|',7X,
     &'LARGEST WEIGHTED RESIDUALS'/
     &16X,'WEIGHTED   PERCENT OF  |',17X,'WEIGHTED   PERCENT OF',/,
     &1X,'NAME',11X,'RESIDUAL    OBJ FUNC   |  NAME',11X,
     &'RESIDUAL    OBJ FUNC')
  470 FORMAT (1X,A,2X,G10.3,3X,F6.2,5X,'|',2X,A,2X,G10.3,3X,F6.2)
  502 FORMAT(/,' COULD NOT CALCULATE THE CORRELATION BETWEEN ORDERED',
     &        ' WEIGHTED RESIDUALS AND',/,
     &        ' NORMAL ORDER STATISTICS (EQ.38 OF TEXT)')
  505 FORMAT (/,' ORDERED WEIGHTED RESIDUALS',/,
     &          ' NUMBER OF RESIDUALS INCLUDED:',I10)
  510 FORMAT ('   ',7G11.3)
  515 FORMAT (/,' CORRELATION BETWEEN ORDERED WEIGHTED RESIDUALS AND',
     &        ' NORMAL ORDER STATISTICS',/,' FOR OBSERVATIONS =',G13.3)
  516 FORMAT (/,' CORRELATION BETWEEN ORDERED WEIGHTED RESIDUALS AND',
     &          ' NORMAL ORDER STATISTICS',/,' FOR OBSERVATIONS AND',
     &          ' PRIOR INFORMATION =',G13.3)
  520 FORMAT(/,1X,'* WEIGHTED RESIDUAL ASSOCIATED WITH CORRELATED ',
     &'PRIOR INFORMATION')
C
      AVET = 0.
      U = 0.
      NND = 0
C     POPULATE IOBSEQ WITH ORIGINAL OBSERVATION SEQUENCE NUMBERS.  IF
C     H AND NAMES ARRAYS NEED TO BE KEPT IN ORDER, THIS CODE COULD BE
C     MODIFIED TO USE IOBSEQ AS A POINTER ARRAY, AND H AND NAMES COULD
C     BE LEFT AS IS
      DO 5 I = 1, ND+IPR+MPR
        IOBSEQ(I) = I
    5 CONTINUE
C-------------HEADS
      IF (NHT.GT.0) THEN
        DO 10 N = 1, NHT
          W = WT(N)
          IF (W.GE.0.) THEN
            RES = HOBS(N) - H(N)
            WT2 = SQRT(W)
            WTR = RES*WT2
            NND = NND + 1
            H(NND) = WTR
            NAMES(NND) = NAMES(N)
            AVET = AVET + WTR
          ENDIF
   10   CONTINUE
      ENDIF
C-----HEAD-DEPENDENT FLOWS
      IF (NDMH.GT.0) THEN
C SC-CHANGE 28.02.96: FOLLOWING LOOP CHANGED DUE TO FULL COV. ON FLOWS
        DO 20 N = 1, NDMH
          BUF1(N) = H(NHT+N)
   20   CONTINUE
        DO 40 N = 1, NDMH
          IF (WTQ(N,N).GE.0.) THEN
            WTR = 0.0
            DO 30 J = 1, NDMH
              NQ2 = NHT + J
              WTR = WTR + WTQS(N,J)*(HOBS(NQ2)-BUF1(J))
   30       CONTINUE
            NND = NND + 1
            H(NND) = WTR
            NAMES(NND) = NAMES(NHT+N)
            IOBSEQ(NND) = IOBSEQ(NHT+N)
            AVET = AVET + WTR
          ENDIF
   40   CONTINUE
      ENDIF
C------TEST FOR NORMALITY OF THE DEPENDENT-VARIABLE WEIGHTED RESIDUALS
C-------CALCULATE THE MEAN
      AVE = AVET/REAL(NND)
C-------ORDER THE RESIDUALS
      DO 60 NN = 1, NND-1
        NMIN = NN
        RMIN = H(NN)
        OBSMIN = NAMES(NN)
        ISEQMIN = IOBSEQ(NN)
        DO 50 N = NN+1, NND
          IF (H(N).LE.RMIN) THEN
            RMIN = H(N)
            OBSMIN = NAMES(N)
            ISEQMIN = IOBSEQ(N)
            NMIN = N
          ENDIF
   50   CONTINUE
        IF (NMIN.NE.NN) THEN
          H(NMIN) = H(NN)
          H(NN) = RMIN
          NAMES(NMIN) = NAMES(NN)
          NAMES(NN) = OBSMIN
          IOBSEQ(NMIN) = IOBSEQ(NN)
          IOBSEQ(NN) = ISEQMIN
        ENDIF
   60 CONTINUE
      WRITE (IOUT,440) NND
      WRITE (IOUT,510) (H(N),N=1,NND)

C------CALCULATE THE STATISTIC
      RNUM = 0.
      DEN1 = 0.
      DEN2 = 0.
      DO 70 N = 1, NND
        RNORM = (REAL(N)-.5)/REAL(NND)
        CALL SOBS1BAS6UN(U,RNORM,-1)
        DIF = H(N) - AVE
        RNUM = RNUM + DIF*U
        DEN1 = DEN1 + DIF**2
        DEN2 = DEN2 + U**2
   70 CONTINUE
      STAT1 = -1.0
      IF(DEN1*DEN2.GT.0.) THEN
        STAT1 = RNUM**2/(DEN1*DEN2)
      ENDIF
      STAT2 = -1.0
      IF (IPAR.GT.0 .AND. (IPR.GT.0 .OR. MPR.GT.0)) THEN
C-------PRIOR INFORMATION ON PARAMETER SUMS
        IF (MPR.GT.0) THEN
          DO 110 IMP = 1, MPR
            TEMP = 0.
            DO 100 IIP = 1, NPLIST
              TEMP = TEMP + PRM(IIP,IMP)*B(IIP)
  100       CONTINUE
            WPSR = WP(IMP)**.5
            TEMP1 = PRM(NPLIST+1,IMP)
            BDIF = TEMP1 - TEMP
            BWP = BDIF*WPSR
            NND = NND + 1
            H(NND) = BWP
            AVET = AVET + BWP
  110     CONTINUE
        ENDIF
        IF (IPR.GT.0) THEN
          DO 150 I1 = 1, IPR
            I = NIPR(I1)
            TEMP = 0.
            TMP = 0.
            DO 140 L1 = 1, I1
              L = NIPR(L1)
              WPSR = WTPS(I1,L1)
              BDIF1 = BPRI(L1)
              BDIF2 = B(L)
              TEMP = TEMP + WPSR*BDIF1
              TMP = TMP + WPSR*BDIF2
  140       CONTINUE
            NND = NND + 1
            H(NND) = TEMP - TMP
            AVET = AVET + H(NND)
  150     CONTINUE
        ENDIF
C-------CALCULATE THE MEAN
        AVET1 = AVET/REAL(NND)
C-------ORDER THE RESIDUALS
        DO 170 NN = 1, NND-1
          RMIN = H(NN)
          NMIN = NN
          OBSMIN = NAMES(NN)
          ISEQMIN = IOBSEQ(NN)
          DO 160 N = NN, NND
            IF (H(N).LE.RMIN) THEN
              RMIN = H(N)
              OBSMIN = NAMES(N)
              ISEQMIN = IOBSEQ(N)
              NMIN = N
            ENDIF
  160     CONTINUE
          IF (NMIN.NE.NN) THEN
            H(NMIN) = H(NN)
            H(NN) = RMIN
            NAMES(NMIN) = NAMES(NN)
            NAMES(NN) = OBSMIN
            IOBSEQ(NMIN) = IOBSEQ(NN)
            IOBSEQ(NN) = ISEQMIN
          ENDIF
  170   CONTINUE
        WRITE (IOUT,505) NND
        WRITE (IOUT,510) (H(N),N=1,NND)
CC-------CALCULATE THE STATISTIC
        RNUM = 0.
        DEN1 = 0.
        DEN2 = 0.
        DO 180 N = 1, NND
          RNORM = (REAL(N)-.5)/REAL(NND)
          CALL SOBS1BAS6UN(U,RNORM,-1)
          DIF = H(N) - AVET1
          RNUM = RNUM + DIF*U
          DEN1 = DEN1 + DIF**2
          DEN2 = DEN2 + U**2
  180   CONTINUE
        STAT2 = RNUM**2/(DEN1*DEN2)
      ENDIF
C
C-----PRINT TABLE OF SMALLEST AND LARGEST WEIGHTED RESIDUALS
      WRITE (IOUTG,460)
      IFOOT = 0
      IF (NND.GE.10) THEN
        NREST = 5
      ELSE
        NREST = NND/2
      ENDIF
      DO 210 I = 1,NREST
        HS = H(I)
        HL = H(NND+1-I)
        NAMS = NAMES(I)
        NAML = NAMES(NND+1-I)
        CALL UCASE(NAMS,NAMSU,1)
        CALL UCASE(NAML,NAMLU,1)
        IF (IPR.GT.0) THEN
          DO 200 IP = 1,NPLIST
            CALL UCASE(PARNAM(IP),PNAMU,1)
            IF (NAMSU.EQ.PNAMU) THEN
              NAMS(12:12) = '*'
              IFOOT = 1
            ENDIF
            IF (NAMLU.EQ.PNAMU) THEN
              NAML(12:12) = '*'
              IFOOT = 1
            ENDIF
  200     CONTINUE
        ENDIF
        WRITE (IOUTG,470) NAMS, HS, 100.0*HS**2/RSQP,
     &                    NAML, HL, 100.0*HL**2/RSQP
  210 CONTINUE
      IF (IFOOT.EQ.1) WRITE (IOUTG,520)
C
C     CALCULATE AND PRINT RUNS STATISTIC FOR ALL RESIDUALS, INCLUDING
C     PRIOR INFORMATION
      CALL SOBS1BAS6RS(AVET,NRSO,NPOST,NNEGT,NRUNS,IOUTG)
C
C     WRITE STATISTICS (R2N STATISTIC) FOR CORRELATION BETWEEN ORDERED
C     WEIGHTED RESIDUALS AND NORMAL ORDER STATISTICS, AND COMMENTS ON
C     INTERPRETING THEM -- WRITE OUTPUT TO BOTH GLOBAL AND LIST FILES
C     UNLESS GLOBAL AND LIST OUTPUT GO TO THE SAME FILE
      IF (STAT1.GT.0.) THEN
        WRITE (IOUTG,515) STAT1
      ELSE
        WRITE(IOUTG,502)
      ENDIF
      IF (STAT2.GT.0.0) THEN
        WRITE (IOUTG,516) STAT2
      ENDIF
      CALL SOBS1BAS6CO(IOUTG,ND,MPR+IPR)
      IF (IOUTG.NE.IOUT) THEN
        IF (STAT1.GT.0.) THEN
          WRITE (IOUT,515) STAT1
        ELSE
          WRITE(IOUT,502)
        ENDIF
        IF (STAT2.GT.0.0) THEN
          WRITE (IOUT,516) STAT2
        ENDIF
        CALL SOBS1BAS6CO(IOUT,ND,MPR+IPR)
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE SOBS1BAS6OH(WP,IOUT,NH,H,HOBS,WT,OBSNAM,ND,MPR,PRM,RSQ,
     &                       RSQP,IOIN,LN,IPR,NIPR,WTPS,
     &                       D,R,WTQ,WTQS,NDMH,NTT2,KTDIM,NPLIST,MPRAR,
     &                       IPRAR,OUTNAM,IPLOT,EQNAM,NAMES,IPLPTR,
     &                       NDMHAR,NQTDR,NQTRV,NQTGB,NQTST,NQTCH,
     &                       IOWTQCH,IOWTQDR,IOWTQRV,IOWTQGB,IOWTQST,
     &                       LCOBBAS,LCOBDRN,LCOBRIV,LCOBGHB,LCOBSTR,
     &                       LCOBCHD,LCOBADV,ISSWR,SSGF,SSDR,SSRV,SSGB,
     &                       SSST,SSAD,SSCH,SSPI,SSTO,ITMXP,IPES,BPRI,
     &                       LCOBDRT,SSDT,NQTDT,IOWTQDT,NRSO,NPOST,
     &                       NNEGT,NRUNS,NQTSF,IOWTQSF,LCOBSFR,SSSF,NHT,
     &                       OTIME)
C     VERSION 20020620 ERB
C     ******************************************************************
C     CALCULATE AND PRINT WEIGHTED RESIDUALS FOR DEPENDENT-VARIABLE
C     OBSERVATIONS, PRIOR PARAMETER ESTIMATES, AND PRIOR ESTIMATES OF
C     PARAMETER SUMS.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL AVET, BPRI, D, H, HOBS, PRM, R, RSQ, RSQP,
     &     WP, WT, WTQ, WTQS, WTRL, ZERO
      INTEGER IDIS, IO, IOIN, IOUT, IPR, JDRY, KTDIM, LN, MPR, N, ND,
     &        NDMH, NH, NHT, NIPR, NNEGT, NPOST, NRSO, NRUNS, NTT2
      INTEGER IUGDO(6), IPLOT(ND+IPR+MPR), IPLPTR(ND+IPR+MPR), LENGNAM
      CHARACTER*10 EQNAM(MPRAR)
      CHARACTER*12 OBSNAM(ND), NAMES(ND+IPR+MPR)
      CHARACTER*200 OUTNAM, OUTTMP
      CHARACTER*84 FN
      LOGICAL LOP
      DIMENSION WP(MPRAR), H(ND), HOBS(ND), WT(ND),
     &          PRM(NPLIST+1,MPRAR), LN(NPLIST), NIPR(IPRAR),
     &          D(ND+MPR+IPR), R(ND+MPR+IPR),
     &          SSGF(ITMXP+1), SSDR(ITMXP+1), SSRV(ITMXP+1),
     &          SSGB(ITMXP+1), SSST(ITMXP+1), SSAD(ITMXP+1),
     &          SSCH(ITMXP+1), SSPI(ITMXP+1), SSTO(ITMXP+1), BPRI(IPRAR)
      DIMENSION WTQ(NDMHAR,NDMHAR), WTQS(NDMHAR,NDMHAR),
     &          WTPS(IPRAR,IPRAR)
      DIMENSION SSDT(ITMXP+1), SSSF(ITMXP+1)
      CHARACTER*4 SUF(6)
      REAL OTIME(ND)
C
C      INCLUDE 'mpif.h'
      INCLUDE 'parallel.inc'
C
      DATA (SUF(I),I=1,6)/'._os','._ww','._ws','._r ','._w ','._nm'/
C     ------------------------------------------------------------------
C
  500 FORMAT (/,' SUM OF SQUARED WEIGHTED RESIDUALS (ALL DEPENDENT',
     &        ' VARIABLES)  ',G11.5)
  505 FORMAT (' SUM OF SQUARED WEIGHTED RESIDUALS (WITH PARAMETERS) ',
     &        G11.5)
 535  FORMAT(/,
     &       ' WARNING: ERROR IN OPENING GRAPH-DATA OUTPUT FILE(S)',/,
     &       ' -- NO GRAPH-DATA OUTPUT FILES',
     &       ' WILL BE PRODUCED (SOBS1BAS6OH)',/)
C
      ZERO = 0.0
      IO = IOIN
      IF (IO.EQ.2) IO = 1
      RSQ = ZERO
      RSQP = ZERO
      NNEGT = 0
      NPOST = 0
      AVET = ZERO
      WTRL = ZERO
      JDRY = 0
      IDIS = 0
      NRUNS = 1
      NRSO = 0
C
      IF (MYID.EQ.MPROC) THEN
        OUTTMP = OUTNAM
      ELSE
        OUTTMP = 'NONE'
      ENDIF
C
      DO 5 I=1,ND+IPR+MPR
        IPLPTR(I) = 0
    5 CONTINUE
C
C-----OPEN GRAPH-DATA OUTPUT FILES
      IF (OUTTMP.NE.'NONE' .AND. IO.EQ.1) THEN
        LENGNAM = NONB_LEN(OUTTMP,200)
C
C       FOR EACH FILE, FIND AN UNUSED FILE UNIT AND OPEN THE FILE
        DO 10 I=1,6
          FN = OUTTMP(1:LENGNAM)//SUF(I)
          IU = IGETUNIT(1,1000)
          IF (IU.GT.0) THEN
            OPEN(IU,FILE=FN,ERR=20)
            CLOSE(UNIT=IU,STATUS='DELETE')
            OPEN(IU,FILE=FN,ERR=20)
            IUGDO(I) = IU
          ELSE
            GOTO 20
          ENDIF
   10   CONTINUE
        GOTO 40
C       IF ERROR IN OPENING A FILE, CLOSE ANY OPENED OUTPUT FILES,
C       TURN OFF OUTPUT FLAG, PRINT WARNING, AND CONTINUE
   20   CONTINUE
        IF (I.GT.1) THEN
          DO 30 J=1,I-1
            INQUIRE(UNIT=IUGDO(J),OPENED=LOP)
            IF (LOP) CLOSE(UNIT=IUGDO(J))
   30     CONTINUE
        ENDIF
        OUTTMP = 'NONE'
        WRITE (IOUT,535)
   40   CONTINUE
      ENDIF
C
C-------------HEAD OBSERVATIONS
      IF (NH.GT.0) CALL SOBS1BAS6HOH(NH,ND,WT,OBSNAM,HOBS,H,JDRY,IO,
     &                               IOUT,D,RSQ,NRUNS,NPOST,NNEGT,MPR,
     &                               IPR,AVET,WTRL,NRSO,IUGDO,OUTTMP,
     &                               IPLOT,IPLPTR,LCOBBAS,ISSWR,SSGF,
     &                               ITMXP,OTIME)
C-----HEAD-DEPENDENT FLOW OBSERVATIONS:
C-------DRAIN FLOW OBSERVATIONS
      IF (NQTDR.GT.0) CALL SOBS1DRN6OH(IO,IOWTQDR,IOUT,NHT,NQTDR,HOBS,H,
     &                                 WTQ,OBSNAM,IDIS,WTQS,D,AVET,
     &                                 NPOST,NNEGT,NRUNS,RSQ,ND,MPR,IPR,
     &                                 NDMH,WTRL,NRSO,IUGDO,OUTTMP,
     &                                 IPLOT,IPLPTR,LCOBDRN,ISSWR,SSDR,
     &                                 ITMXP,OTIME)
C-------RIVER FLOW OBSERVATIONS
      IF (NQTRV.GT.0) CALL SOBS1RIV6OH(IO,IOWTQRV,IOUT,NHT,NQTRV,HOBS,H,
     &                                 WTQ,OBSNAM,IDIS,WTQS,D,AVET,
     &                                 NPOST,NNEGT,NRUNS,RSQ,ND,MPR,IPR,
     &                                 NDMH,WTRL,NRSO,IUGDO,OUTTMP,
     &                                 IPLOT,IPLPTR,LCOBRIV,ISSWR,SSRV,
     &                                 ITMXP,OTIME)
C-------GENERAL-HEAD BOUNDARY FLOW OBSERVATIONS
      IF (NQTGB.GT.0) CALL SOBS1GHB6OH(IO,IOWTQGB,IOUT,NHT,NQTGB,HOBS,H,
     &                                 WTQ,OBSNAM,IDIS,WTQS,D,AVET,
     &                                 NPOST,NNEGT,NRUNS,RSQ,ND,MPR,IPR,
     &                                 NDMH,WTRL,NRSO,IUGDO,OUTTMP,
     &                                 IPLOT,IPLPTR,LCOBGHB,ISSWR,SSGB,
     &                                 ITMXP,OTIME)
C-------STREAMFLOW-ROUTING (STR) FLOW OBSERVATIONS
      IF (NQTST.GT.0) CALL SOBS1STR6OH(IO,IOWTQST,IOUT,NHT,NQTST,HOBS,H,
     &                                 WTQ,OBSNAM,IDIS,WTQS,D,AVET,
     &                                 NPOST,NNEGT,NRUNS,RSQ,ND,MPR,IPR,
     &                                 NDMH,WTRL,NRSO,IUGDO,OUTTMP,
     &                                 IPLOT,IPLPTR,LCOBSTR,ISSWR,SSST,
     &                                 ITMXP,OTIME)
C-------STREAMFLOW-ROUTING (SFR) FLOW OBSERVATIONS
cc      IF (NQTSF.GT.0) CALL SOBS1SFR1OH(IO,IOWTQSF,IOUT,NHT,NQTSF,HOBS,H,
cc     &                                 WTQ,OBSNAM,IDIS,WTQS,D,AVET,
cc     &                                 NPOST,NNEGT,NRUNS,RSQ,ND,MPR,IPR,
cc     &                                 NDMH,WTRL,NRSO,IUGDO,OUTTMP,
cc     &                                 IPLOT,IPLPTR,LCOBSFR,ISSWR,SSSF,
cc     &                                 ITMXP,OTIME)
C-------CONSTANT-HEAD BOUNDARY FLOW OBSERVATIONS
      IF (NQTCH.GT.0) CALL SOBS1BAS6FOH(IO,IOWTQCH,IOUT,NHT,NQTCH,HOBS,
     &                                  H,WTQ,OBSNAM,IDIS,WTQS,D,AVET,
     &                                  NPOST,NNEGT,NRUNS,RSQ,ND,MPR,
     &                                  IPR,NDMH,WTRL,NRSO,IUGDO,OUTTMP,
     &                                  IPLOT,IPLPTR,LCOBCHD,ISSWR,SSCH,
     &                                  ITMXP,OTIME)
C-------DRAIN-RETURN FLOW OBSERVATIONS
      IF (NQTDT.GT.0) CALL SOBS1DRT1OH(IO,IOWTQDT,IOUT,NHT,NQTDT,HOBS,H,
     &                                 WTQ,OBSNAM,IDIS,WTQS,D,AVET,
     &                                 NPOST,NNEGT,NRUNS,RSQ,ND,MPR,IPR,
     &                                 NDMH,WTRL,NRSO,IUGDO,OUTTMP,
     &                                 IPLOT,IPLPTR,LCOBDRT,ISSWR,SSDT,
     &                                 ITMXP,OTIME)
C-------ADVECTIVE-TRANSPORT OBSERVATIONS
      IF (NTT2.GT.0) CALL SOBS1ADV2O(NHT,NTT2,HOBS,H,WTQ,IOUT,D,IDIS,
     &                               IDTT,JDRY,RSQ,NRUNS,AVET,NPOST,
     &                               NNEGT,KTDIM,ND,MPR,IPR,IO,OBSNAM,N,
     &                               NDMH,WTRL,NRSO,IUGDO,OUTTMP,IPLOT,
     &                               IPLPTR,LCOBADV,ISSWR,SSAD,ITMXP,
     &                               OTIME)
C------PRINT WEIGHTED RESIDUALS FOR PRIOR INFORMATION ON INDIVIDUAL
C------PARAMETERS
      RSQP = RSQ
      IF (ISSWR.GT.0 .AND. IPES.GT.0) SSPI(ISSWR) = 0.0
C-----PRINT WEIGHTED RESIDUALS FOR PRIOR INFORMATION ON PARAMETER SUMS
      IF (MPR.GT.0) CALL SPES1BAS6PE(IO,IOUT,MPR,PRM,LN,WP,D,RSQP,NRUNS,
     &                              AVET,NPOST,NNEGT,IPR,ND,WTRL,NRSO,
     &                              NPLIST,MPRAR,IUGDO,OUTTMP,
     &                              IPLOT,EQNAM,IPLPTR,ISSWR,SSPI,ITMXP)
C-------PRINT WEIGHTED RESIDUALS FOR CORRELATED PRIOR
      IF (IPR.GT.0) CALL SPES1BAS6PC(IPR,NIPR,LN,IO,IOUT,WTPS,
     &                              D,NRUNS,AVET,NPOST,NNEGT,RSQP,MPR,
     &                              ND,WTRL,NRSO,NPLIST,IPRAR,
     &                              IPLOT,NAMES,OUTTMP,IUGDO,IPLPTR,
     &                              ISSWR,SSPI,ITMXP,BPRI)
C-------FINAL PRINTOUT
      WRITE (IOUT,500) RSQ
      IF (MPR.GT.0 .OR. IPR.GT.0) WRITE (IOUT,505) RSQP
C
      IF (ISSWR.GT.0) THEN
        IF (MPR.EQ.0 .AND. IPR.EQ.0) THEN
          SSTO(ISSWR) = RSQ
        ELSE
          SSTO(ISSWR) = RSQP
        ENDIF
      ENDIF
C
      IF (IO.EQ.1) THEN
C       CALCULATE AND PRINT THE RUNS STATISTIC FOR ALL RESIDUALS
        CALL SOBS1BAS6RS(AVET,NRSO,NPOST,NNEGT,NRUNS,IOUT)
        IF (OUTTMP.NE.'NONE') CALL SOBS1BAS6OR(D,R,IUGDO(6),IPR,MPR,ND,
     &                                         NRSO,IPLPTR,IPLOT,NAMES)
      ENDIF
C
C-----CLOSE GRAPH-DATA OUTPUT FILES
      IF (OUTTMP.NE.'NONE' .AND. IO.EQ.1) THEN
        DO 50 I=1,6
          INQUIRE(UNIT=IUGDO(I),OPENED=LOP)
          IF (LOP) CLOSE(UNIT=IUGDO(I))
  50    CONTINUE
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE SOBS1BAS6ST(BUF1,IOUT,IOWTQ,IPLOT,IPR,ISCALS,LN,MPR,ND,
     &                       NDMH,NDMHAR,NHT,NPE,NPLIST,OBSNAM,OUTNAM,
     &                       WT,WTQ,WTQS,X,ITERP,BUF2,BSCAL,OBSALL)
C
C     VERSION 20000313 ERB
C     ******************************************************************
C     PRINT OBSERVATION-SENSITIVITY TABLES
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER IOUT, IOWTQ, IPLOT, IPR, ISCALS, LN, MPR, ND, NDMH,
     &        NHT, NPE, NSECTS
      INTEGER IUSNO(4)
      REAL BUF1, BUF2, WT, WTQ, WTQS
      CHARACTER*4 SUF(4)
      CHARACTER*12 OBSNAM(ND)
      CHARACTER*200 OUTNAM
      CHARACTER*84 FN
      LOGICAL LOP, OBSALL, PRCSS
      DIMENSION BUF1(NPE), BUF2(NPE,ND), IPLOT(ND+IPR+MPR),
     &          LN(NPLIST), WT(ND), WTQ(NDMHAR,NDMHAR),
     &          WTQS(NDMHAR,NDMHAR), X(NPE,ND), BSCAL(NPLIST)
      INCLUDE 'param.inc'
      DATA (SUF(I),I=1,4)/'._sc','._sd','._s1','._su'/
C     ------------------------------------------------------------------
  510 FORMAT (' ')
  515 FORMAT ('OBSERVATION SENSITIVITY TABLE(S) FOR PARAMETER',
     &        '-ESTIMATION ITERATION ',I5)
  520 FORMAT (/,' SENSITIVITIES',//,9X,'PARAMETER:',1X,5(2X,A10))
  530 FORMAT (/,' DIMENSIONLESS SCALED SENSITIVITIES',
     &        ' (SCALED BY B*(WT**.5))',//,
     &        9X,'PARAMETER:',1X,5(2X,A10))
  540 FORMAT (' OBS #  OBSERVATION')
  550 FORMAT (1X,I5,2X,A12,5(2X,G10.3),25(:/,20X,5(2X,G10.3)))
  560 FORMAT (/,7X,' COMPOSITE SCALED SENSITIVITIES ',
     &        '((SUM OF THE SQUARED VALUES)/ND)**.5',/,20X,
     &        5(2X,G10.3))
  570 FORMAT(/,
     &       ' WARNING: ERROR IN OPENING OUTPUT FILE(S)',/,
     &       ' -- NO OBSERVATION-SENSITIVITY OUTPUT FILES',
     &       ' WILL BE PRODUCED (OBS1BAS6ST)',/)
  580 FORMAT(/,' ONE-PERCENT SCALED SENSITIVITIES',
     &        ' (SCALED BY B/100.0)',//,
     &        9X,'PARAMETER:',1X,5(2X,A10))
  590 FORMAT (' OBSERVATION  SYMBOL',500(2X,A10,3X))
  600 FORMAT(1X,A,1X,I6,500(1X,G14.7))
  610 FORMAT (1X,A,1X,G14.7)
  620 FORMAT (/,2X,'PARAMETER    COMPOSITE SCALED SENSITIVITY',/,
     &          2X,'----------   ----------------------------')
  630 FORMAT (2X,A,4X,1P,E12.5)
C     ------------------------------------------------------------------
C
C     SET FLAG IF COMPOSITE SCALED SENSITIVITIES ARE TO BE PRINTED
      PRCSS = .FALSE.
      IF (ISCALS.LT.0 .OR. ISCALS.EQ.1 .OR. ISCALS.EQ.3 .OR.
     &    OUTNAM.NE.'NONE') PRCSS = .TRUE.
C
C     OPEN OBSERVATION-SENSITIVITY OUTPUT FILES
      IF (OUTNAM.NE.'NONE') THEN
        LENGNAM = NONB_LEN(OUTNAM,200)
C
C       FOR EACH FILE, FIND AN UNUSED FILE UNIT AND OPEN THE FILE
        DO 10 I=1,4
          IF (I.EQ.4 .AND. .NOT. OBSALL) CYCLE
          FN = OUTNAM(1:LENGNAM)//SUF(I)
          IU = IGETUNIT(50,1000)
          IF (IU.GT.0) THEN
            OPEN(IU,FILE=FN,ERR=20)
            CLOSE(UNIT=IU,STATUS='DELETE')
C           Note that RECL=7530 provides a long enough record length for
C           500 parameters.  If NPE > 500, RECL will need to be
C           increased -- ERB 03/06/2002
            OPEN(IU,FILE=FN,ERR=20,RECL=7530)
C            OPEN(IU,FILE=FN,ERR=20)
            IUSNO(I) = IU
          ELSE
            GOTO 20
          ENDIF
   10   CONTINUE
        GOTO 40
C       IF ERROR IN OPENING A FILE, CLOSE ANY OPENED OUTPUT FILES,
C       TURN OFF OUTPUT FLAG, PRINT WARNING, AND CONTINUE
   20   CONTINUE
        IF (I.GT.1) THEN
          DO 30 J=1,I-1
            INQUIRE(UNIT=IUSNO(J),OPENED=LOP)
            IF (LOP) CLOSE(UNIT=IUSNO(J))
   30     CONTINUE
        ENDIF
        OUTNAM = 'NONE'
        WRITE (IOUT,570)
   40   CONTINUE
      ENDIF
C     DETERMINE HOW MANY SECTIONS EACH OBSERVATION-SENSITIVITY TABLE
C     WILL CONTAIN
      A = (NPE-0.1)/5.0
      NSECTS = INT(A) + 1
C
      WRITE (IOUT,510)
      WRITE (IOUT,515) ITERP
      IF (ISCALS.EQ.0) THEN
C       PRINT UNSCALED SENSITIVITIES
        DO 60 IS = 1,NSECTS
          IG1 = IS*5 - 4
          IG2 = IS*5
          IF (IG2.GT.NPE) IG2 = NPE
          WRITE (IOUT,520) (PARNAM(IPPTR(IIP)),IIP=IG1,IG2)
          WRITE (IOUT,540)
          DO 50 N = 1, ND
            WRITE (IOUT,550) N, OBSNAM(N), (X(IIP,N),IIP=IG1,IG2)
   50     CONTINUE
   60   CONTINUE
      ENDIF
C
C     WRITE MATRIX OF UNSCALED SENSITIVITIES TO _su FILE
      IF (OUTNAM.NE.'NONE' .AND. OBSALL) THEN
        WRITE (IUSNO(4),590) (PARNAM(IPPTR(I)),I=1,NPE)
        DO 55 N = 1, ND
          WRITE (IUSNO(4),600) OBSNAM(N),IPLOT(N),(X(IIP,N),IIP=1,NPE)
   55   CONTINUE
      ENDIF
C
C     IF BSCAL WILL OVERRIDE B IN SCALING FOR ANY PARAMETERS, WRITE
C     MESSAGE INDICATING WHICH PARAMETERS
      IF (ISCALS.LT.0 .OR. ISCALS.EQ.1 .OR. ISCALS.EQ.3)
     &    CALL SOBS1BAS6BS(BSCAL,IOUT,LN,NPE,NPLIST)
C     PRINT DIMENSIONLESS SCALED SENSITIVITIES
      DO 140 IS = 1,NSECTS
        IG1 = IS*5 - 4
        IG2 = IS*5
        IF (IG2.GT.NPE) IG2 = NPE
        IF (ISCALS.EQ.1 .OR. ISCALS.EQ.3) THEN
          WRITE (IOUT,530) (PARNAM(IPPTR(IIP)),IIP=IG1,IG2)
          WRITE (IOUT,540)
        ENDIF
        IF (OUTNAM.NE.'NONE' .AND. IS.EQ.NSECTS) WRITE (IUSNO(2),590)
     &      (PARNAM(IPPTR(I)),I=1,NPE)
        DO 70 IIP = IG1, IG2
          BUF1(IIP) = 0.
   70   CONTINUE
        IF (NHT.GT.0) THEN
          DO 90 N = 1, NHT
            DO 80 IIP = IG1, IG2
              IF (WT(N).LT.0.) THEN
                BUF2(IIP,N) = 0.
                GOTO 80
              ENDIF
              IIPP = IPPTR(IIP)
              BB = ABS(B(IIPP))
              IF (LN(IIPP).LE.0) THEN
C               PARAMETER IS NOT LOG-TRANSFORMED
                IF (BB.LT.BSCAL(IIPP)) BB = BSCAL(IIPP)
              ELSE
C               PARAMETER IS LOG-TRANSFORMED
                BB = 1.0
              ENDIF
              BUF2(IIP,N) = BB*X(IIP,N)*(WT(N)**.5)
              BUF1(IIP) = BUF1(IIP) + (BUF2(IIP,N)*BUF2(IIP,N))
   80       CONTINUE
            IF (ISCALS.EQ.1 .OR. ISCALS.EQ.3)
     &          WRITE (IOUT,550) N, OBSNAM(N),
     &                           (BUF2(IIP,N),IIP=IG1,IG2)
            IF (OUTNAM.NE.'NONE' .AND. IS.EQ.NSECTS)
     &          WRITE (IUSNO(2),600) OBSNAM(N), IPLOT(N),
     &                               (BUF2(IIP,N),IIP=1,NPE)
   90     CONTINUE
        ENDIF
        IF (NDMH.GT.0) THEN
          DO 120 N = NHT+1, ND
            NQ1 = N - NHT
            DO 110 IIP = IG1,IG2
              IIPP = IPPTR(IIP)
              IF (WTQ(NQ1,NQ1).LT.0.) THEN
                BUF2(IIP,N) = 0.
                GOTO 110
              ENDIF
              BB = ABS(B(IIPP))
              IF (LN(IIPP).LE.0) THEN
C               PARAMETER IS NOT LOG-TRANSFORMED
                IF (BB.LT.BSCAL(IIPP)) BB = BSCAL(IIPP)
              ELSE
C               PARAMETER IS LOG-TRANSFORMED
                BB = 1.0
              ENDIF
              IF (IOWTQ.GT.0) THEN
                TMP = 0.
                DO 100 K = 1, NDMH
                  IF (WTQ(K,K).GT.0.) TMP = TMP + WTQS(NQ1,K)
     &                                      *X(IIP,NHT+K)
  100           CONTINUE
                BUF2(IIP,N) = BB*TMP
              ELSE
                BUF2(IIP,N) = BB*X(IIP,N)*WTQS(NQ1,NQ1)
              ENDIF
C             CONTRIBUTION TO COMPOSITE SCALED SENSITIVITY
              BUF1(IIP) = BUF1(IIP) + (BUF2(IIP,N)*BUF2(IIP,N))
  110       CONTINUE
            IF (ISCALS.EQ.1 .OR. ISCALS.EQ.3)
     &          WRITE (IOUT,550) N, OBSNAM(N),
     &                           (BUF2(IIP,N),IIP=IG1,IG2)
            IF (OUTNAM.NE.'NONE' .AND. IS.EQ.NSECTS)
     &          WRITE (IUSNO(2),600) OBSNAM(N), IPLOT(N),
     &                               (BUF2(IIP,N),IIP=1,NPE)
  120     CONTINUE
        ENDIF
C       COMPLETE CALCULATION OF COMPOSITE SCALED SENSITIVITIES
        DO 130 IIP = IG1, IG2
          BUF1(IIP) = (BUF1(IIP)/ND)**.5
  130   CONTINUE
        IF (ISCALS.EQ.1 .OR. ISCALS.EQ.3)
     &      WRITE (IOUT,560) (BUF1(IIP),IIP=IG1,IG2)
        IF (OUTNAM.NE.'NONE' .AND. IS.EQ.NSECTS)
     &      WRITE (IUSNO(1),610)
     &          (PARNAM(IPPTR(IIP)),BUF1(IIP),IIP=1,NPE)
  140 CONTINUE
C     PRINT COMPOSITE SCALED SENSITIVITY FOR ALL PARAMETERS
      IF (PRCSS) THEN
        WRITE (IOUT,620)
        DO 150 IIP = 1, NPE
          WRITE (IOUT,630) PARNAM(IPPTR(IIP)),BUF1(IIP)
  150   CONTINUE
      ENDIF
C
      IF (ISCALS.EQ.2 .OR. ISCALS.EQ.3 .OR. OUTNAM.NE.'NONE') THEN
C       IF BSCAL WILL OVERRIDE B IN SCALING FOR ANY PARAMETERS, WRITE
C       MESSAGE INDICATING WHICH PARAMETERS
        IF (ISCALS.EQ.2 .OR. ISCALS.EQ.3)
     &      CALL SOBS1BAS6BS(BSCAL,IOUT,LN,NPE,NPLIST)
C       PRINT ONE-PERCENT SCALED SENSITIVITIES
        DO 230 IS = 1, NSECTS
          IG1 = IS*5 - 4
          IG2 = IS*5
          IF (IG2.GT.NPE) IG2 = NPE
          IF (ISCALS.EQ.2 .OR. ISCALS.EQ.3) THEN
            WRITE (IOUT,580) (PARNAM(IPPTR(IIP)),IIP=IG1,IG2)
            WRITE (IOUT,540)
          ENDIF
          IF (OUTNAM.NE.'NONE' .AND. IS.EQ.NSECTS) WRITE (IUSNO(3),590)
     &        (PARNAM(IPPTR(I)),I=1,NPE)
          IF (NHT.GT.0) THEN
            DO 190 N = 1, NHT
              DO 180 IIP = IG1, IG2
                IF (WT(N).LT.0.) THEN
                  BUF2(IIP,N) = 0.
                  GOTO 180
                ENDIF
                IIPP = IPPTR(IIP)
                BB = ABS(B(IIPP))
                IF (LN(IIPP).LE.0) THEN
C                 PARAMETER IS NOT LOG-TRANSFORMED
                  IF (BB.LT.BSCAL(IIPP)) BB = BSCAL(IIPP)
                ELSE
C                 PARAMETER IS LOG-TRANSFORMED
                  BB = 1.0
                ENDIF
                BUF2(IIP,N) = BB*X(IIP,N)/100.0
  180         CONTINUE
              IF (ISCALS.EQ.2 .OR. ISCALS.EQ.3)
     &            WRITE (IOUT,550) N, OBSNAM(N),
     &                             (BUF2(IIP,N),IIP=IG1,IG2)
              IF (OUTNAM.NE.'NONE' .AND. IS.EQ.NSECTS)
     &            WRITE (IUSNO(3),600) OBSNAM(N), IPLOT(N),
     &                                 (BUF2(IIP,N),IIP=1,NPE)
  190       CONTINUE
          ENDIF
          IF (NDMH.GT.0) THEN
            DO 220 N = NHT+1, ND
              NQ1 = N - NHT
              DO 210 IIP = IG1, IG2
                IIPP = IPPTR(IIP)
                IF (WTQ(NQ1,NQ1).LT.0.) THEN
                  BUF2(IIP,N) = 0.
                  GOTO 210
                ENDIF
                IIPP = IPPTR(IIP)
                BB = ABS(B(IIPP))
                IF (LN(IIPP).LE.0) THEN
C                 PARAMETER IS NOT LOG-TRANSFORMED
                  IF (BB.LT.BSCAL(IIPP)) BB = BSCAL(IIPP)
                ELSE
C                 PARAMETER IS LOG-TRANSFORMED
                  BB = 1.0
                ENDIF
                BUF2(IIP,N) = BB*X(IIP,N)/100.0
  210         CONTINUE
              IF (ISCALS.EQ.2 .OR. ISCALS.EQ.3)
     &            WRITE (IOUT,550) N, OBSNAM(N),
     &                             (BUF2(IIP,N),IIP=IG1,IG2)
              IF (OUTNAM.NE.'NONE' .AND. IS.EQ.NSECTS)
     &            WRITE (IUSNO(3),600) OBSNAM(N), IPLOT(N),
     &                                 (BUF2(IIP,N),IIP=1,NPE)
  220       CONTINUE
          ENDIF
  230   CONTINUE
      ENDIF
C
C-----CLOSE SENSITIVITY OUTPUT FILES
      IF (OUTNAM.NE.'NONE') THEN
        DO 250 I=1,4
          IF (I.EQ.4 .AND. .NOT. OBSALL) CYCLE
          INQUIRE(UNIT=IUSNO(I),OPENED=LOP)
          IF (LOP) CLOSE(UNIT=IUSNO(I))
  250   CONTINUE
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE SOBS1BAS6CO(IOUT,ND,MPR)
C
C     VERSION 19990415 ERB  -- Copied from UCODE
C     ******************************************************************
C     WRITE COMMENTS ON INTERPRETATION, CUSTOMIZED BASED ON NUMBER OF
C     RESIDUALS AND CORRELATION BETWEEN ORDERED WEIGHTED RESIDUALS AND
C     NORMAL ORDER STATISTICS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER IFLAG, IOUT, MPR, ND, NDR
      REAL RN205, RN210
C     ------------------------------------------------------------------
      NDR = ND + MPR
      CALL RSTAT (NDR,RN205,RN210,IFLAG)
      WRITE (IOUT,1001)
 1001 FORMAT (/,' ----------------------------------------',
     &        '----------------------------------')
      WRITE(IOUT,1002)
 1002 FORMAT(' COMMENTS ON THE INTERPRETATION OF THE CORRELATION ',
     & 'BETWEEN',/,' WEIGHTED RESIDUALS AND NORMAL ORDER STATISTICS:')
      IF (IFLAG.EQ.1) WRITE(IOUT,996)RN205,RN210
 996  FORMAT(
     &   /,' The critical value for correlation at the 5%'
     &      ,' significance level is ',f5.3
     &,//,1X,'IF the reported CORRELATION is GREATER than the 5% critic'
     & ,'al value, ACCEPT',/,1x,'the hypothesis that the weighted'
     & ,' residuals are INDEPENDENT AND NORMALLY',/,1x,'DISTRIBUTED at'
     & ,' the 5% significance level.  The probability that this   ',/
     & ,' conclusion is wrong is less than 5%.',
     & //,' IF the reported correlation IS LESS THAN the 5% critical'
     &,' value REJECT the',/,' hypothesis that the weighted residuals'
     & ,' are INDEPENDENT AND NORMALLY',/,' DISTRIBUTED at the 5%'
     &,' significance level.',//,' The'
     & ,' analysis can also be done using the 10% significance level.',/
     & ,' The associated critical value is ',f5.3)
      IF (IFLAG.EQ.0) WRITE(IOUT,997)
 997  FORMAT(/,1X
     &      ,'Generally, IF the reported CORRELATION is LESS than the'
     &      ,' critical value,',/,' at the '
     &      ,'selected significance level (usually 5 or 10%), the '
     &      ,'hypothesis',/,' that the '
     &      ,'weighted residuals are INDEPENDENT AND NORMALLY '
     &      ,'DISTRIBUTED',/,' would be REJECTED.  HOWEVER, '
     &      ,'in this case, conditions are outside of',/,' the range of'
     &      ,' published critical values as discussed below.')
      IF (IFLAG.EQ.2) WRITE(IOUT,995)
 995  FORMAT(/,1X
     &    ,'Generally, IF the reported CORRELATION is GREATER than the'
     &    ,' critical value,',/,' at the '
     &    ,'selected significance level (usually 5 or 10%), the '
     &    ,'hypothesis',/,' that the '
     &    ,'weighted residuals are INDEPENDENT AND NORMALLY '
     &    ,'DISTRIBUTED',/,' would be ACCEPTED.  HOWEVER, '
     &    ,'in this case, conditions are outside of',/,' the range of'
     &    ,' published critical values as discussed below.')
      IF (IFLAG.EQ.0) WRITE (IOUT,998) NDR,RN205,RN210
 998  FORMAT(/,1x,'The sum of the number of observations and prior'
     & ,' information items is ',i5
     & ,/,1x,'which is less than'
     & ,' 35, the minimum value for which critical values are'
     & ,/,1x,'published.  Therefore, the critical values'
     & ,' for the 5 and 10% significance'
     & ,/,1x,'levels are less than ',f5.3,' and ',f5.3,', respectively.'
     & ,//,' CORRELATIONS GREATER than these critical values'
     & ,' indicate that, probably, the '
     & ,/,1x,'weighted residuals ARE'
     & ,' INDEPENDENT AND NORMALLY DISTRIBUTED.',//,1x
     & ,'Correlations LESS than these critical values MAY BE '
     & ,'ACCEPTABLE, and',/,' rejection of the hypothesis'
     & ,' is not necessarily warranted.',//
     & ,' The Kolmogorov-Smirnov test can be used'
     & ,' to further evaluate the residuals.')
      IF (IFLAG.EQ.2) WRITE (IOUT,999) NDR,RN205,RN210
 999  FORMAT(/,1x,'The sum of the number of observations and prior'
     & ,' information items is ',i5
     & ,/,1x,'which is greater than'
     & ,' 200, the maximum value for which critical values are'
     & ,/,1x,'published.  Therefore, the critical values'
     & ,' for the 5 and 10% significance'
     & ,/,1x,'levels are greater than ',f5.3,' and ',f5.3,', respect'
     & ,'ively.',//,' CORRELATIONS GREATER THAN these critical values'
     & ,' suggest that, probably,',/
     & ,' the weighted residuals ARE'
     & ,' INDEPENDENT AND NORMALLY DISTRIBUTED.'
     & ,//,1x,'Correlations LESS THAN these critical values clearly'
     & ,' indicate that we CAN',/,1x,'REJECT the hypothesis.'
     & ,//,1x,'The Kolmogorov-Smirnov test can be used'
     & ,' to further evaluate the residuals.')
      WRITE (IOUT,1003)
 1003 FORMAT (' ----------------------------------------',
     &        '----------------------------------',/)
      RETURN
      END
C=======================================================================
      SUBROUTINE SOBS1BAS6BS(BSCAL,IOUT,LN,NPE,NPLIST)
C     VERSION 20000313 ERB
C     ******************************************************************
C     DETERMINE IF BSCAL WILL APPLY TO SCALING OF ANY PARAMETERS.  IF
C     SO, WRITE MESSAGE AND LIST PARAMETER(S)
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION BSCAL(NPLIST), LN(NPLIST)
      INCLUDE 'param.inc'
      DIMENSION IBSFLG(MXPAR)
C     ------------------------------------------------------------------
  500 FORMAT(/,
     &' FOR THE SCALING OF THE SENSITIVITIES BELOW, B IS REPLACED BY',/,
     &' BSCAL (THE ALTERNATE SCALING FACTOR) FOR PARAMETER(S):')
  510 FORMAT(3X,6(2X,A))
C
C     DETERMINE IF BSCAL APPLIES TO ANY PARAMETER(S)
      K = 0
      DO 10 IP = 1,NPE
        IIPP = IPPTR(IP)
        BB = ABS(B(IIPP))
        IF (LN(IIPP).LE.0) THEN
C         PARAMETER IS NOT LOG-TRANSFORMED
          IF (BB.LT.BSCAL(IIPP)) THEN
            K = K + 1
            IBSFLG(K) = IIPP
          ENDIF
        ENDIF
   10 CONTINUE
C
C     WRITE MESSAGE LISTING PARAMETERS TO WHICH BSCAL APPLIES
      IF (K.GT.0) THEN
        WRITE(IOUT,500)
        WRITE(IOUT,510) (PARNAM(IBSFLG(I)),I=1,K)
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE SOBS1BAS6CC(ND,WT,HOBS,H,R,R1,MPR,WP,B,PRM,
     &                       IPR,NIPR,WTPS,NHT,WTQ,WTQS,IOWTQ,NDMH,
     &                       NPLIST,MPRAR,IPRAR,NDMHAR,BPRI)
C     VERSION 20000201 ERB
C     ******************************************************************
C     CALCULATE CORRELATION COEFFICIENT
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION SUMA, SUMB, SUMC, SUMD, SUM, TMP, TEMP, TMPP
      REAL B, BDIF1, BDIF2, BPRI, H, HOBS, OBS, PRM, R, R1, WP,
     &      WPSR, WT, WTQ, WTQS
      INTEGER I, I1, II, IOWTQ, IPR, J, L, L1, MPR, N, ND, NDMH,
     &        NHT, NIPR
      DIMENSION WT(ND), HOBS(ND), H(ND), B(NPLIST),
     &          PRM(NPLIST+1,MPRAR), WP(MPRAR), NIPR(IPRAR),
     &          BPRI(IPRAR)
      DIMENSION WTQ(NDMHAR,NDMHAR), WTQS(NDMHAR,NDMHAR),
     &          WTPS(IPRAR,IPRAR)
C     ------------------------------------------------------------------
      OBS = ND
      SUMA = 0.0
      SUMB = 0.0
      SUMC = 0.0
      SUMD = 0.0
      SUM = 0.0
C-------OBSERVATIONS WITH DIAGONAL WEIGHT MATRIX
      IF(NHT.GT.0) THEN
        DO 10 N = 1, NHT
          IF (WT(N).LT.0.0) THEN
            OBS = OBS - 1.0
            GOTO 10
          ENDIF
          TMP = WT(N)**.5
          TEMP = TMP*HOBS(N)
          TMP = TMP*H(N)
          SUMA = SUMA + TEMP
          SUMB = SUMB + TMP
          SUMC = SUMC + TEMP*TEMP
          SUMD = SUMD + TMP*TMP
          SUM = SUM + TEMP*TMP
   10   CONTINUE
      ENDIF
C-------OBSERVATIONS WITH FULL WEIGHT MATRIX
      IF(NDMH.GT.0) THEN
        DO 30 I = 1, NDMH
          IF (WTQ(I,I).LT.0.0) THEN
            OBS = OBS - 1.
            GOTO 30
          ENDIF
          IF (IOWTQ.GT.0) THEN
            TEMP = 0.0
            TMP = 0.0
            DO 20 J = 1, NDMH
              N = NHT + J
              TMP = TMP + WTQS(I,J)*H(N)
              TEMP = TEMP + WTQS(I,J)*HOBS(N)
   20       CONTINUE
          ELSE
            N = NHT + I
            TMP = WTQS(I,I)*H(N)
            TEMP = WTQS(I,I)*HOBS(N)
          ENDIF
          SUMA = SUMA + TEMP
          SUMB = SUMB + TMP
          SUMC = SUMC + TEMP*TEMP
          SUMD = SUMD + TMP*TMP
          SUM = SUM + TEMP*TMP
   30   CONTINUE
      ENDIF
      TEMP = (OBS*SUMC-SUMA*SUMA)*(OBS*SUMD-SUMB*SUMB)
      IF (TEMP.GT.0.0) THEN
        R = (OBS*SUM-SUMA*SUMB)/TEMP**.5
      ELSE
        R = 100.0
      ENDIF
      R1 = R
C------PRIOR PARAMETER ESTIMATES WITH DIAGONAL WEIGHTING
      IF (MPR.GT.0) THEN
        OBS = OBS + MPR
        DO 50 I = 1, MPR
          IF (WP(I).GT.0.) THEN
            TMP = WP(I)**.5
            TEMP = TMP*PRM(NPLIST+1,I)
            TMPP = 0.0
            DO 40 II = 1, NPLIST
              TMPP = TMPP + PRM(II,I)*B(II)
   40       CONTINUE
            TMP = TMPP*TMP
            SUMA = SUMA + TEMP
            SUMB = SUMB + TMP
            SUMC = SUMC + TEMP*TEMP
            SUMD = SUMD + TMP*TMP
            SUM = SUM + TEMP*TMP
          ENDIF
   50   CONTINUE
      ENDIF
C-------PRIOR INFORMATION WITH A FULL WEIGHT MATRIX
      IF (IPR.GT.0) THEN
        DO 90 I1 = 1, IPR
          I = NIPR(I1)
          TEMP = 0.0
          TMP = 0.0
          DO 80 L1 = 1, I1
            L = NIPR(L1)
            WPSR = WTPS(I1,L1)
            BDIF1 = BPRI(L1)
            BDIF2 = B(L)
            TEMP = TEMP + WPSR*BDIF1
            TMP = TMP + WPSR*BDIF2
   80     CONTINUE
          SUMA = SUMA + TEMP
          SUMB = SUMB + TMP
          SUMC = SUMC + TEMP*TEMP
          SUMD = SUMD + TMP*TMP
          SUM = SUM + TEMP*TMP
   90   CONTINUE
      ENDIF
      TEMP = (OBS*SUMC-SUMA*SUMA)*(OBS*SUMD-SUMB*SUMB)
      IF (TEMP.GT.0.0) THEN
        R1 = (OBS*SUM-SUMA*SUMB)/TEMP**.5
      ELSE
        R1 = 100.0
      ENDIF
      RETURN
      END
C=======================================================================
      SUBROUTINE SOBS1BAS6WF(NPE,NHT,NDMH,WTQ,X,C,G,HOBS,H,IOWTQ,
     &                       NDMHAR,ND)
C-----VERSION 1000 02FEB1996
C     ******************************************************************
C     ADD SENSITIVITY CONTRIBUTIONS FROM NON-HEAD DATA WITH FULL WEIGHT
C     MATRIX TO C AND G
C     (DEVELOPED BY STEEN CHRISTENSEN, AARHUS UNIVERSITY)
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL DTMPA, H, HOBS, TMPA, W, WTQ, X
      INTEGER I, IOWTQ, IP, J, K, L, L1, N, N1, NHT, NPE, NDMH
      DOUBLE PRECISION C(NPE,NPE), G(NPE), TMP
      DIMENSION WTQ(NDMHAR,NDMHAR), X(NPE,ND), HOBS(ND), H(ND)
C     ------------------------------------------------------------------
C
      IF (IOWTQ.GT.0) THEN
        DO 50 J = 1, NPE
          DO 40 K = 1, NDMH
            TMP = 0.0
            IF (WTQ(K,K).LT.0.0) THEN
              DO 10 L = 1, NDMH
                TMP = TMP + 1.E-20*DBLE(X(J,NHT+L))
   10         CONTINUE
            ELSE
              DO 20 L = 1, NDMH
                IF (WTQ(L,L).LT.0.0) THEN
                  TMP = TMP + 1.E-20*DBLE(X(J,NHT+L))
                ELSE
                  TMP = TMP + DBLE(WTQ(K,L))*DBLE(X(J,NHT+L))
                ENDIF
   20         CONTINUE
            ENDIF
            DO 30 I = J, NPE
              C(I,J) = C(I,J) + DBLE(X(I,NHT+K))*TMP
   30       CONTINUE
   40     CONTINUE
   50   CONTINUE
        DO 90 I = 1, NPE
          DO 80 K = 1, NDMH
            TMP = 0.0
            IF (WTQ(K,K).LT.0.0) THEN
              DO 60 L = 1, NDMH
                L1 = NHT + L
                TMP = TMP + 1.E-20*DBLE(HOBS(L1)-H(L1))
   60         CONTINUE
            ELSE
              DO 70 L = 1, NDMH
                L1 = NHT + L
                IF (WTQ(L,L).LT.0.0) THEN
                  TMP = TMP + 1.E-20*DBLE(HOBS(L1)-H(L1))
                ELSE
                  TMP = TMP + DBLE(WTQ(K,L))*DBLE(HOBS(L1)-H(L1))
                ENDIF
   70         CONTINUE
            ENDIF
            G(I) = G(I) + DBLE(X(I,NHT+K))*TMP
   80     CONTINUE
   90   CONTINUE
      ELSE
C     IF WEIGHT MATRIX IS DIAGONAL
        DO 120 N = 1, NDMH
          N1 = N + NHT
          TMPA = HOBS(N1) - H(N1)
          W = WTQ(N,N)
          IF (W.LT.0) W = 1.E-20
          DO 110 IP = 1, NPE
            DTMPA = DBLE(W)*DBLE(X(IP,N1))
            DO 100 I = IP, NPE
              C(I,IP) = DBLE(X(I,N1))*DTMPA + C(I,IP)
  100       CONTINUE
            G(IP) = DTMPA*TMPA + G(IP)
  110     CONTINUE
  120   CONTINUE
      ENDIF
      RETURN
      END
C=======================================================================
      SUBROUTINE SOBS1BAS6ML(RSQP,WT,NPE,ND,WP,MPR,IOUT,EV,IPR,DETWTP,
     &                       NHT,DTLWTQ,MPRAR)
C-----VERSION 1000 01FEB1992
C     ******************************************************************
C     CALCULATE STATISTICS BASED ON THE MAXIMUM LIKELIHOOD OBJECTIVE
C     FUNCTION
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL AEV, AIC, BIC, DETWTP, DTLWTQ, EV, OF, PROD, PRODP, RND, RP,
     &     RSQP, WP, WT
      INTEGER IOUT, IPR, MPR, N, ND, NHT, NPE
      DIMENSION WT(ND), WP(MPRAR)
C     ------------------------------------------------------------------
  500 FORMAT (/,'  MAX LIKE OBJ FUNC = ',G11.5,/,
     &        '  AIC STATISTIC---- = ',G11.5,/,
     &        '  BIC STATISTIC---- = ',G11.5)
C
      AEV = LOG(EV)
      RND = REAL(ND)
      RP = REAL(MPR+IPR)
C-----DEPENDENT-VARIABLE PART OF WEIGHT MATRIX
      PROD = DTLWTQ
      DO 10 N = 1, NHT
        IF (WT(N).GT.0.) THEN
          PROD = PROD + LOG(WT(N))
        ELSE
          RND = RND - 1.0
        ENDIF
   10 CONTINUE
C-----PRIOR PART OF WEIGHT MATRIX
C       UNCORRELATED PRIOR
      PRODP = 0.0
      IF (MPR.GT.0) THEN
        DO 20 I = 1, MPR
          IF (WP(I).NE.0.0) PRODP = PRODP + LOG(WP(I))
   20   CONTINUE
      ENDIF
C       CORRELATED PRIOR
      IF (IPR.GT.0) PRODP = PRODP + DETWTP
C-----LOG-LIKELIHOOD FUNCTION
      OF = (RND+RP)*LOG(2.*3.14159265) + (RND+RP)*AEV - PROD - PRODP +
     &     (RSQP/EV)
C-----AIC AND BIC
      AIC = OF + 2.0*REAL(NPE)
      BIC = OF + NPE*LOG(RND+RP)
      WRITE (IOUT,500) OF, AIC, BIC
      RETURN
      END
C=======================================================================
      SUBROUTINE OBS1BAS6HAL(IUH,NH,MOBS,MAXM,ISUM,ISUMI,LCNDER,LCCOFF,
     &                       LCROFF,LCIOFF,LCJOFF,LCRINT,LCMLAY,LCPR,ND,
     &                       IOUT,IOBSUM,LCOBBAS,ITMXP,LCSSGF,IOBS,NHT)
C     VERSION 20000125
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR HEAD OBSERVATIONS
C     ******************************************************************
      INTEGER IOUT, ISUM, IUH, LCCOFF, LCIOFF, LCJOFF, LCMLAY,
     &        LCNDER, LCPR, LCRINT, LCROFF, MAXM, MOBS, ND, NH
      CHARACTER*200 LINE
C
C-----------------------------------------------------------------------
C
C     IDENTIFY PROCESS
      WRITE(IOUT,490) IUH
  490 FORMAT(/,' HEAD OBSERVATIONS -- INPUT READ FROM UNIT ',I3)
C
C  Turn off observation package if OBS is not active
      IF(IOBS.LE.0) THEN
        WRITE(IOUT,610)
610     FORMAT(/,1X,'WARNING: OBSERVATION (OBS) FILE IS NOT LISTED BUT',
     &      ' THE HEAD OBSERVATION',/,' FILE (HOB) IS',
     &     ' LISTED -- TURNING OFF HEAD OBSERVATIONS (OBS1BAS6HAL)')
        IUH = 0
        RETURN
      ENDIF
C
C  Read data
      CALL URDCOM(IUH,IOUT,LINE)
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NH,DUM,IOUT,IUH)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MOBS,DUM,IOUT,IUH)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MAXM,DUM,IOUT,IUH)
      IF (MAXM.EQ.1) THEN
        WRITE (IOUT,500) NH, MOBS, MAXM-1
      ELSE
        WRITE (IOUT,500) NH, MOBS, MAXM
      ENDIF
  500 FORMAT (/,
     &     ' NUMBER OF HEADS....................................:',I5,/,
     &     '   NUMBER OF MULTILAYER HEADS.......................:',I5,/,
     &     '   MAXIMUM NUMBER OF LAYERS FOR MULTILAYER HEADS....:',I5)
C
C-----ERROR CHECKING
      IF (MAXM.EQ.1) THEN
        WRITE (IOUT,555)
        CALL USTOP(' ')
      ENDIF
  555 FORMAT (/,' MAXM CAN NOT EQUAL 1 -- STOP EXECUTION')
      IF (MAXM.EQ.0) MAXM = 1
C
C----------HEAD DATA ARRAYS
      IS = ISUM
      ISI= ISUMI
      LCNDER = ISUMI
      ISUMI = ISUMI + 5*NH
      LCCOFF = ISUM
      ISUM = ISUM + NH
      LCROFF = ISUM
      ISUM = ISUM + NH
      LCIOFF = ISUMI
      ISUMI = ISUMI + NH
      LCJOFF = ISUMI
      ISUMI = ISUMI + NH
      LCRINT = ISUM
      ISUM = ISUM + 4*NH
      LCMLAY = ISUMI
      ISUMI = ISUMI + MAXM*MOBS
      LCPR = ISUM
      ISUM = ISUM + MAXM*MOBS
      LCSSGF = ISUM
      ISUM = ISUM + ITMXP + 1
C
      IS = ISUM - IS
      ISI = ISUMI - ISI
C     POINTERS TO OBSERVATION ARRAYS
      LCOBBAS = IOBSUM
      IOBSUM = IOBSUM + NH
C
      ND = ND + NH
      NHT = NHT + NH
C
      RETURN
      END
C=======================================================================
      SUBROUTINE OBS1BAS6HRP(NCOL,NROW,NLAY,NPER,IUH,IOUT,OBSNAM,NH,
     &                       NDER,JT,JOFF,IOFF,HOBS,WT,DELR,DELC,RINT,
     &                       COFF,ROFF,MLAY,PR,MOBS,IERR,TOFF,EV,EVH,
     &                       MAXM,NSTP,PERLEN,TSMULT,ISSA,ITRSS,NHAR,
     &                       MOBSAR,IPLOT,NAMES,ND,IPR,MPR,OTIME)
C-----VERSION 1001 01SEP1995
C     VERSION 20030728 ERB
C     ******************************************************************
C     READ, CHECK AND STORE DATA FOR HYDRAULIC HEAD LOCATIONS.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL COFF, DELC, DELR, DUM, EV, EVH, HOBS, PERLEN, PR, RINT,
     &     ROFF, TOFF, TOFFSET, TPR, TSMULT, WT, WT1, WT2
      INTEGER I, IERR, IOFF, IOUT, ITT, IUH, IWT, J, JOFF, JT,
     &        K, KK, M, MAXM, ML, ML1, MLAY, MLL, MM, MOBS, N, N1, NCOL,
     &        NDER, NH, NLAY, NPER, NROW, NSTP, NT, NTC, ISSA
      INTEGER IPLOT(ND+IPR+MPR)
      CHARACTER*12 OBSNAM(ND), NAMES(ND+MPR+IPR)
      DIMENSION NDER(5,NHAR), IOFF(NHAR), JOFF(NHAR), HOBS(ND), WT(ND),
     &          DELR(NCOL), DELC(NROW), RINT(4,NHAR), COFF(NHAR),
     &          ROFF(NHAR), MLAY(MAXM,MOBSAR), PR(MAXM,MOBSAR),
     &          TOFF(ND), NSTP(NPER), PERLEN(NPER), TSMULT(NPER),
     &          ISSA(NPER)
      CHARACTER*10 STATYP(0:2)
      REAL OTIME(ND)
      DATA (STATYP(I),I=0,2)/'VARIANCE','STD. DEV.','COEF. VAR.'/
C     ------------------------------------------------------------------
  505 FORMAT (8(I5,F5.0))
  515 FORMAT (2X,'TRANSIENT DATA AT THIS LOCATION, ITT =',I4)
  525 FORMAT (/,' FOR OBS',I5,
     &        ' STATISTIC RELATED TO WEIGHT < OR = 0 -- STOP EXECUTION',
     &        ' (OBS1BAS6HRP)',/)
  527 FORMAT (/,' HEAD OBSERVATION VARIANCES ARE MULTIPLIED BY: ',G15.4)
  530 FORMAT (/,' OBSERVED HEAD DATA -- TIME OFFSETS ARE',
     &' MULTIPLIED BY: ',G12.5,//,
     &20X,'REFER.',/,
     &7X,'OBSERVATION',2X,'STRESS',4X,'TIME',30X,'STATISTIC',
     &3X,'PLOT',/,
     &2X,'OBS#    NAME',6X,'PERIOD',
     &3X,'OFFSET    OBSERVATION  STATISTIC',5X,'TYPE',6X,'SYM.')
  535 FORMAT (1X,I5,1X,A12,2X,I4,2X,G11.4,1X,G11.4,1X,G11.4,2X,A10,
     &        1X,I5)
  540 FORMAT (5X,'MULTIPLE LAYERS AND PROPORTIONS :',5(I5,',',F5.2,3X))
  550 FORMAT (' FOR OBS',I5,' ROW OR COLUMN NUMBER INVALID -- ',
     &        'STOP EXECUTION (OBS1BAS6HRP)',/)
  555 FORMAT (' FOR OBS ',I5,' LAYER INVALID -- STOP EXECUTION',
     &        ' (OBS1BAS6HRP)',/)
  560 FORMAT (/,' FOR OBS',I5,' MULTILAYER PROPORTIONS DO NOT SUM ',
     &        'TO 1.0 -- STOP EXECUTION (OBS1BAS6HRP)',/)
  565 FORMAT (/,' NUMBER OF MULTILAYER OBSERVATIONS EXCEEDS MOBS -- ',
     &        'STOP EXECUTION (OBS1BAS6HRP)',/)
  570 FORMAT (' ')
  575 FORMAT (' FOR OBS',I5,
     &        ' ITT MUST = 1 OR 2 -- STOP EXECUTION (OBS1BAS6HRP)',/)
  580 FORMAT (/,' OBSERVATION',I5,' EQUALS ZERO; THE STATISTIC ',
     &        'CAN NOT BE A',/,' COEFFICIENT OF VARIATION (IWT=2) -- ',
     &        'STOP EXECUTION (OBS1BAS6HRP)',/)
  590 FORMAT (/,53X,'HEAD CHANGE',/,54X,'REFERENCE',/,
     &8X,'OBSERVATION',19X,'ROW',5X,'COL    OBSERVATION',/,
     &2X,'OBS#',5X,'NAME',7X,'LAY  ROW  COL  OFFSET  OFFSET',3X,
     &'(IF > 0)')
  600 FORMAT (1X,I5,2X,A12,2X,I3,2(1X,I4),2(2X,F6.3),3X,I6)
  610 FORMAT(/,1X,'ERROR: SEARCH ABOVE FOR ERROR MESSAGE(S)',/,1X,
     &'STOP EXECUTION -- (OBS1BAS6HRP)')
  620 FORMAT(/,1X,'ERROR: VALUE ENTERED FOR MAXM IN HOB FILE IS',
     &    ' SMALLER THAN THE MAXIMUM NUMBER',/,' OF LAYERS',
     &    ' IN A MULTILAYER HEAD OBSERVATION, WHICH IS ',
     &    I3,' -- INCREASE MAXM.')
C
C-----READ TIME-OFFSET MULTIPLIER FOR HEAD-OBSERVATION TIMES AND INPUT
C     ERROR VARIANCE FOR HEADS (ITEM 2)
      READ(IUH,*) TOMULTHD, EVH
C-------WRITE INTRODUCTORY LINES
      WRITE (IOUT,527) EVH
      WRITE (IOUT,530) TOMULTHD
C-------INITIALIZE VARIABLES
      KL = 0
      ML = 0
      NT = 0
      NTC = 0
      ITT = 0
      IF (MOBS.GT.0) THEN
        DO 20 MM = 1, MOBS
          DO 10 M = 1, MAXM
            MLAY(M,MM) = 0
            PR(M,MM) = 0.0
   10     CONTINUE
   20   CONTINUE
      ENDIF
      DO 30 N = 1, NH
        NDER(5,N) = 0
   30 CONTINUE
C-------LOOP THROUGH HEAD OBSERVATIONS
      DO 90 N = 1, NH
C----------READ FIRST TRANSIENT OBSERVATIONS AT ONE LOCATION
        IF (N.GT.1) THEN
          IF (NDER(4,N-1).LT.0.OR.NTC.LT.NT) THEN
            IF (NDER(4,N-1).LT.0) THEN
              N1 = N - 1
              NT = -NDER(4,N1)
              NTC = 1
C-----------READ ITEM 6 (FIRST OCCURENCE)
              READ (IUH,*) OBSNAM(N1), IREFSP, TOFFSET, HOBS(N1),
     &                       WT(N1), DUM, IWT, IPLOT(N1)
              NAMES(N1) = OBSNAM(N1)
              WRITE (IOUT,535) N1, OBSNAM(N1), IREFSP, TOFFSET,
     &                         HOBS(N1),WT(N1),STATYP(IWT),IPLOT(N1)
              CALL UOBSTI(OBSNAM(N1),IOUT,ISSA,ITRSS,NPER,NSTP,IREFSP,
     &                    NDER(4,N1),PERLEN,TOFF(N1),TOFFSET,TOMULTHD,
     &                    TSMULT,0,OTIME(N1))
              IF (WT(N1).LE.0.) THEN
                WRITE (IOUT,525) N1
                IERR = 1
              ENDIF
              IF (HOBS(N1).EQ.0 .AND. IWT.EQ.2) THEN
                WRITE (IOUT,580) N1
                IERR = 1
              ENDIF
              IF (IWT.EQ.2) WT(N1) = WT(N1)*HOBS(N1)
              IF (IWT.GT.0) WT(N1) = WT(N1)*WT(N1)
              WT(N1) = WT(N1)*EVH
            ENDIF
C-------SUBSEQUENT OBSERVATIONS AT ONE LOCATION
            NTC = NTC + 1
C----------ASSIGN INFORMATION WHICH STAYS THE SAME
            DO 40 I = 1, 3
              NDER(I,N) = NDER(I,N1)
   40       CONTINUE
            ROFF(N) = ROFF(N1)
            COFF(N) = COFF(N1)
            IOFF(N) = IOFF(N1)
            JOFF(N) = JOFF(N1)
            DO 50 I = 1, 4
              RINT(I,N) = RINT(I,N1)
   50       CONTINUE
            IF (NDER(1,N1).LT.0) THEN
              ML1 = ML
              ML = ML + 1
              DO 60 M = 1, MAXM
                PR(M,ML) = PR(M,ML1)
                MLAY(M,ML) = MLAY(M,ML1)
   60         CONTINUE
            ENDIF
C----------READ INFORMATION UNIQUE TO THIS OBSERVATION (ITEM 6)
            READ (IUH,*) OBSNAM(N), IREFSP, TOFFSET, HOBS(N), WT1,
     &                     WT2, IWT, IPLOT(N)
            NAMES(N) = OBSNAM(N)
            IF (ITT.EQ.1) WT(N) = WT1
            IF (ITT.EQ.2) THEN
              WT(N) = WT2
              HOBS(N) = HOBS(N) - HOBS(N1)
              NDER(5,N) = N1
            ENDIF
            WRITE (IOUT,535) N, OBSNAM(N), IREFSP, TOFFSET, HOBS(N),
     &                       WT(N), STATYP(IWT), IPLOT(N)
            CALL UOBSTI(OBSNAM(N),IOUT,ISSA,ITRSS,NPER,NSTP,IREFSP,
     &                  NDER(4,N),PERLEN,TOFF(N),TOFFSET,TOMULTHD,
     &                  TSMULT,0,OTIME(N))
            IF (WT(N).LE.0.) THEN
              WRITE (IOUT,525) N
              IERR = 1
            ENDIF
            IF (IWT.EQ.2) WT(N) = WT(N)*HOBS(N)
            IF (IWT.GT.0) WT(N) = WT(N)*WT(N)
            WT(N) = WT(N)*EVH
            IF (NTC.EQ.NT) WRITE (IOUT,570)
            GOTO 80
          ENDIF
        ENDIF
C----------READ ITEM 3
        READ (IUH,*) OBSNAM(N), (NDER(I,N),I=1,3), IREFSP, TOFFSET,
     &                 ROFF(N), COFF(N), HOBS(N), WT(N), IWT, IPLOT(N)
        NAMES(N) = OBSNAM(N)
        WRITE (IOUT,535) N, OBSNAM(N), IREFSP, TOFFSET, HOBS(N),
     &                   WT(N), STATYP(IWT), IPLOT(N)
        IF (IREFSP.LT.0) THEN
          NDER(4,N) = IREFSP
        ELSE
          CALL UOBSTI(OBSNAM(N),IOUT,ISSA,ITRSS,NPER,NSTP,IREFSP,
     &                NDER(4,N),PERLEN,TOFF(N),TOFFSET,TOMULTHD,TSMULT,
     &                0,OTIME(N))
        ENDIF
        IF (HOBS(N).EQ.0 .AND. IWT.EQ.2) THEN
          WRITE (IOUT,580) N
          IERR = 1
        ENDIF
        IF (IWT.EQ.2) WT(N) = WT(N)*HOBS(N)
        IF (IWT.GT.0) WT(N) = WT(N)*WT(N)
        WT(N) = WT(N)*EVH
        IF (WT(N).LE.0. .AND. NDER(4,N).GE.0) THEN
          WRITE (IOUT,525) N
          IERR = 1
        ENDIF
C-------ERROR CHECKING
        K = NDER(1,N)
        I = NDER(2,N)
        J = NDER(3,N)
        IF (J.LE.0 .OR. J.GT.NCOL .OR. I.LE.0 .OR. I.GT.NROW) THEN
          WRITE (IOUT,550) N
          IERR = 1
        ENDIF
C-------INITIALIZE SOME VARIABLES
        MM = 1
        TPR = 1.
C-------READ INFORMATION FOR MULTILAYER OBSERVATIONS (ITEM 4)
        IF (K.LT.0) THEN
          ML = ML + 1
          READ (IUH,*) (MLAY(M,ML),PR(M,ML),M=1,-K)
          WRITE(IOUT,540) (MLAY(M,ML),PR(M,ML),M=1,-K)
          MM = -K
          IF (MM.GT.KL) KL = MM
          TPR = 0.0
        ENDIF
C-------READ FLAG FOR USING TEMPORAL CHANGES IN HEAD (ITEM 5)
        IF (NDER(4,N).LT.0) THEN
          READ (IUH,*) ITT
          WRITE (IOUT,515) ITT
          IF (ITT.NE.1 .AND. ITT.NE.2) THEN
            WRITE (IOUT,575) N
            CALL USTOP(' ')
          ENDIF
        ENDIF
C-------ERROR CHECKING
        DO 70 M = 1, MM
          KK = K
C----------ASSIGN LAYER NUMBERS AND ADD PROPORTIONS FOR MULTILAYER
C----------OBSERVATION WELLS
          IF (K.LT.0) THEN
            KK = MLAY(M,ML)
            IF (KK.EQ.0) GOTO 70
            TPR = TPR + PR(M,ML)
          ENDIF
C----------CHECK LAYER NUMBER
          IF (KK.LE.0 .OR. KK.GT.NLAY) THEN
            WRITE (IOUT,555) N
            IERR = 1
          ENDIF
   70   CONTINUE
C---------CHECK SUM OF PROPORTIONS FOR MULTILAYER OBS WELLS
        IF (K.LT.0 .AND. ABS(1.-TPR).GT..02) THEN
          WRITE (IOUT,560) N
          IERR = 1
        ENDIF
C-------CALCULATE INTERPOLATION COEFFICIENTS
        MLL = 0
        IF (K.LT.0) MLL = MLAY(1,ML)
        CALL SOBS1BAS6HIA(NDER(1,N),COFF(N),ROFF(N),DELR,DELC,NCOL,
     &                  NROW,RINT(1,N),JOFF(N),IOFF(N),MLL)
C-------KEEP TRACK OF LATEST MEASUREMENT
   80   CONTINUE
        IF (NDER(4,N).GE.JT) THEN
          JT = NDER(4,N)
          IF (TOFF(N).GT.0.) JT = JT + 1
        ENDIF
   90 CONTINUE
C
C-------PRINT TABLE SHOWING OBSERVATION LOCATION AND REFERENCE
C       OBSERVATION WHERE DRAWDOWN IS TO BE CALCULATED
      WRITE (IOUT,590)
      DO 95 N = 1, NH
        WRITE (IOUT,600) N, OBSNAM(N), (NDER(I,N),I=1,3), ROFF(N),
     &                   COFF(N), NDER(5,N)
   95 CONTINUE
C
C-------ERROR CHECKING -
C-------EXCEEDED STORAGE RESERVED FOR MULTILAYER OBSERVATIONS?
      IF (ML.GT.MOBS) THEN
        WRITE (IOUT,565)
        IERR = 1
      ENDIF
      IF (KL.GT.MAXM) THEN
        WRITE(IOUT,620) KL
        IERR = 1
      ENDIF
C
C     IF ERROR, PRINT MESSAGE AND STOP
      IF (IERR.GT.0) THEN
        WRITE(IOUT,610)
        CALL USTOP(' ')
      ENDIF
C
C-------CONVERT HEAD OBSERVATION VARIANCES TO WEIGHTS.
      DO 100 N = 1, NH
        WT(N) = EV/WT(N)
  100 CONTINUE
      RETURN
      END
C=======================================================================
      SUBROUTINE OBS1BAS6HFM(NH,NDER,IOFF,JOFF,MLAY,IBOUND,RINT,OBSNAM,
     &                       COFF,ROFF,DELR,DELC,NCOL,NROW,NLAY,PR,H,WT,
     &                       HNEW,IDRY,NPE,TOFF,MAXM,JDRY,IPAR,IOUT,
     &                       ITS,NHAR,MOBSAR,ND,IPES,IYCFLG,STRT)
C     VERSION 20010810 ERB
C     ******************************************************************
C     INTERPOLATE HEADS.  ACCOUNT FOR DRY CELLS, IF NEEDED.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL COFF, DELC, DELR, H, PR, PROP, RINT, ROFF, STRT, TOFF, WT,
     &     ZERO
      INTEGER IBOUND, IDRY, II, IO, IOFF, IOUT, IPAR, ITS, JDRY, JJ,
     &        JO, JOFF, K, KK, M, MAXM, ML, MLAY, MLL, MM, N, N1, NCOL,
     &        NDER, NH, NLAY, NROW
      CHARACTER*12 OBSNAM(ND)
      DOUBLE PRECISION HNEW(NCOL,NROW,NLAY), V, V0
      DIMENSION COFF(NHAR), DELC(NROW), DELR(NCOL), H(ND),
     &          IBOUND(NCOL,NROW,NLAY), IOFF(NHAR), JOFF(NHAR),
     &          MLAY(MAXM,MOBSAR), NDER(5,NHAR),
     &          PR(MAXM,MOBSAR), RINT(4,NHAR), ROFF(NHAR),
     &          STRT(NCOL,NROW,NLAY), TOFF(ND), WT(ND)
      COMMON /DISCOM/LBOTM(999),LAYCBD(999)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
  490 FORMAT (/,' HEAD OBS#',I5,', ID ',A,
     &' OMITTED BECAUSE INITIAL OBSERVATION IS DRY OR',/,' HAS HAD ITS',
     &' INTERPOLATION CHANGED (OBS1BAS6HFM)')
  495 FORMAT (/,' HEAD OBS#',I5,', ID ',A,' IS DRY -- OMIT',
     &        ' (OBS1BAS6HFM)')
  500 FORMAT (/,' HEAD OBS#',I5,', ID ',A,
     &' OMITTED BECAUSE IBOUND=0 FOR CELL(S)',/,' REQUIRED FOR',
     &' INTERPOLATION AND OBSERVATION IS MULTILAYER OR INVOLVES',/,
     &' TEMPORAL INTERPOLATION (OBS1BAS6HFM)')
  505 FORMAT (/,' INTERPOLATION FOR HEAD OBS#',I5,', ID ',A,' CHANGED',
     &     ' BECAUSE AT LEAST ONE',/,
     &' NEIGHBORING CELL REQUIRED FOR INTERPOLATION IS DRY',
     &' OR INACTIVE (OBS1BAS6HFM)')
  510 FORMAT (/,I6,' OF',I5,' OBSERVATIONS OMITTED.  EXECUTION ',
     &        'STOPS IF NUMBER OF REMAINING',/,
     &' OBSERVATIONS IS LESS THAN OR EQUAL TO NUMBER OF PARAMETERS')
  515 FORMAT (/,'THE NUMBER OF PARAMETERS TO BE ESTIMATED EQUALS OR',
     &' EXCEEDS THE NUMBER OF OBSERVATIONS',
     &' -- STOP EXECUTION')
C
C
      ZERO = 0.0
C
C-------CHECK FOR NODES USED TO INTERPOLATE HEADS THAT HAVE GONE DRY OR
C-------ARE OTHERWISE INACTIVE.
C-------ELIMINATE OBSERVATIONS OR RECALC. INTERPOLATION COEFFICIENTS.
C------CHECK FOR OBSERVATIONS THAT NEED TO BE OMITTED OR NEED TO HAVE
C------THE INTERPOLATION RECALCULATED
C------IDRY = # OBS OMITTED; JDRY = # INTERPOLATIONS CHANGED
      IF (ITS.EQ.1) THEN
        IDRY = 0
        JDRY = 0
      ENDIF
      ML = 0
      DO 30 N = 1, NH
        K = NDER(1,N)
        II = NDER(2,N)
        JJ = NDER(3,N)
        IO = IOFF(N)
        JO = JOFF(N)
        MM = 1
        IF (K.LT.0) THEN
          ML = ML + 1
          MM = MAXM
        ENDIF
        IF ((NDER(4,N).EQ.ITS.OR.(TOFF(N).GT.ZERO.AND.
     &       NDER(4,N).EQ.ITS-1)) .AND. WT(N).GT.ZERO) THEN
C-------IF THE OBSERVATION THIS IS TO BE SUBTRACTED FROM IS DRY, MAKE
C-------THIS ONE DRY, TOO
          N1 = NDER(5,N)
          IF (N1.GT.0) THEN
            IF (WT(N1).LT.ZERO.OR.COFF(N1).GE.5.) THEN
              IDRY = IDRY + 1
              WT(N) = -ABS(WT(N))
              WRITE (IOUT,490) N, OBSNAM(N)
              GOTO 30
            ENDIF
          ENDIF
C-------CHECK FOR DRY OBSERVATIONS OR INTERPOLATIONS AFFECTED BY DRY
C-------CELLS
          DO 20 M = 1, MM
            KK = K
            IF (K.LT.0) KK = MLAY(M,ML)
            IF (KK.EQ.0) GOTO 30
              IF (IBOUND(JJ,II,KK).EQ.0) THEN
                IDRY = IDRY + 1
                WT(N) = -ABS(WT(N))
                WRITE (IOUT,495) N, OBSNAM(N)
                GOTO 30
C             CHECK TO SEE IF A CELL USED IN INTERPOLATION IS INACTIVE
              ELSEIF ((RINT(2,N).NE.ZERO.AND.IBOUND(JJ+JO,II,KK)
     &                .EQ.0) .OR.
     &                (RINT(3,N).NE.ZERO.AND.IBOUND(JJ,II+IO,KK)
     &                .EQ.0) .OR.
     &                (RINT(4,N).NE.ZERO.AND.IBOUND(JJ+JO,II+IO,KK)
     &                .EQ.0)) THEN
                IF (MM.GT.1 .OR. TOFF(N).GT.ZERO) THEN
                  IDRY = IDRY + 1
                  WT(N) = -ABS(WT(N))
                  WRITE (IOUT,500) N, OBSNAM(N)
                  GOTO 30
                ENDIF
                WRITE (IOUT,505) N, OBSNAM(N)
                IF (COFF(N).GT..5) COFF(N) = COFF(N) - 5.
                MLL = 0
                IF (NDER(1,N).LT.0) MLL = MLAY(1,ML)
                CALL SOBS1BAS6HIB(NDER(1,N),COFF(N),ROFF(N),DELR,DELC,
     &                            IBOUND,NCOL,NROW,NLAY,RINT(1,N),
     &                            JOFF(N),IOFF(N),MLL)
                COFF(N) = COFF(N) + 5.
                JDRY = JDRY + 1
CC INSERT ELSEIF TO SEE IF A NEIGHBORING CELL HAS REWET, IF SO,
CC RECALCULATE RINT
              ENDIF
   20     CONTINUE
        ENDIF
   30 CONTINUE
      IF (IDRY.GT.0) WRITE (IOUT,510) IDRY, ND
      IF ((ND-IDRY).LE.NPE .AND. IPES.GT.0 .AND. IYCFLG.LT.1) THEN
        WRITE (IOUT,515)
        CALL USTOP(' ')
      ENDIF
C
C-----INTERPOLATION
      ML = 0
      DO 60 N = 1, NH
C-------UPDATE COUNTER FOR MULTILAYER WELLS
        K = NDER(1,N)
        MM = 1
        IF (K.LT.0) THEN
          ML = ML + 1
          MM = -K
        ENDIF
C-------DRY WELL?
        IF (IPAR.GT.0 .AND. WT(N).LT.ZERO) GOTO 60
C-------OBSERVATION AT THIS TIME STEP?
        IF ((NDER(4,N).NE.ITS-1.OR.TOFF(N).LE.ZERO) .AND.
     &      NDER(4,N).NE.ITS) GOTO 60
        II = NDER(2,N)
        JJ = NDER(3,N)
        IO = IOFF(N)
        JO = JOFF(N)
        V = 0.0
        V0 = 0.0
        DO 40 M = 1, MM
          KK = K
          PROP = 1.
          IF (K.LT.0) THEN
            KK = MLAY(M,ML)
            PROP = PR(M,ML)
          ENDIF
          IF (KK.EQ.0) GOTO 50
C--------CALCULATE CONTRIBUTION FROM THIS LAYER TO HEADS
          V = V + PROP*(RINT(1,N)*HNEW(JJ,II,KK)+
     &                  RINT(2,N)*HNEW(JJ+JO,II,KK)+
     &                  RINT(3,N)*HNEW(JJ,II+IO,KK)+
     &                  RINT(4,N)*HNEW(JJ+JO,II+IO,KK))
          IF (ITS.EQ.1) THEN
            V0 = V0 + PROP*(RINT(1,N)*STRT(JJ,II,KK)+
     &                      RINT(2,N)*STRT(JJ+JO,II,KK)+
     &                      RINT(3,N)*STRT(JJ,II+IO,KK)+
     &                      RINT(4,N)*STRT(JJ+JO,II+IO,KK))
          ENDIF
   40   CONTINUE
C-------INDEX WHICH, IF NOT ZERO, IDENTIFIES THE HEAD USED TO
C-------CALCULATE DRAWDOWN
   50   N1 = NDER(5,N)
C---------HEADS OR DRAWDOWNS
        IF (NDER(4,N).EQ.ITS) H(N) = V
        IF (ITS.EQ.1 .AND. NDER(4,N).EQ.0) H(N) = V0
        IF (NDER(4,N).EQ.ITS-1 .AND. TOFF(N).GT.ZERO)
     &      H(N) = H(N) + TOFF(N)*(V-H(N))
        IF (N1.GT.0 .AND. ((NDER(4,N).EQ.ITS.AND.TOFF(N).EQ.ZERO).OR.
     &      (NDER(4,N).EQ.ITS-1.AND.TOFF(N).GT.ZERO)))
     &       H(N) = H(N) - H(N1)
   60 CONTINUE
      RETURN
      END
C=======================================================================
      SUBROUTINE OBS1BAS6HDR(NH,NDER,IOFF,JOFF,MLAY,RINT,NCOL,NROW,NLAY,
     &                       PR,WT,SNEW,X,IP,NPE,LN,TOFF,MAXM,IPAR,
     &                       NPLIST,ITS,NHAR,MOBSAR,ND)
C     VERSION 19981020 ERB
C     FORMERLY WAS SSEN1U
C     ******************************************************************
C     CALCULATE RHS FOR SENSITIVITIES. ACCOUNT FOR DRY CELLS, IF NEEDED.
C     COMPUTE OBSERVATION SENSITIVITIES.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL PR, PROP, RINT, TOFF, WT, X, ZERO
      INTEGER II, IIPP, IO, IOFF, IP, IPAR, ITS, JJ, JO, JOFF, K, KK,
     &        LN, LNIIPP, M, MAXM, ML, MLAY, MM, N, N1, NCOL, NDER, NH,
     &        NLAY, NPE, NROW
      DOUBLE PRECISION SNEW(NCOL,NROW,NLAY), V
      DIMENSION X(NPE,ND), WT(ND), RINT(4,NHAR), JOFF(NHAR),
     &          IOFF(NHAR), MLAY(MAXM,MOBSAR), PR(MAXM,MOBSAR),
     &          NDER(5,NHAR), LN(NPLIST), TOFF(ND)
      COMMON /DISCOM/LBOTM(999),LAYCBD(999)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
  500 FORMAT (/,' HEAD OBS#',I5,', ID ',A4,
     &       ' IS DRY OR INACTIVE -- OMIT (OBS1BAS6HDR)')
  505 FORMAT (/,' INTERPOLATION FOR HEAD OBS#',I5,', ID ',A4,' CHANGED',
     &    ' BECAUSE AT LEAST ONE NEIGHBORING CELL IS DRY (OBS1BAS6HDR)')
  510 FORMAT (/,I6,' OF',I5,' OBSERVATIONS OMITTED.  EXECUTION ',
     &        'STOPS IF # OF REMAINING OBS <= # OF PARAMETERS')
  515 FORMAT (/,' -- STOP EXECUTION')
C
C
      ZERO = 0.0
      IIPP = IPPTR(IP)
      LNIIPP = LN(IIPP)
C
C-----INTERPOLATION
      ML = 0
      DO 60 N = 1, NH
C-------UPDATE COUNTER FOR MULTILAYER WELLS
        K = NDER(1,N)
        MM = 1
        IF (K.LT.0) THEN
          ML = ML + 1
          MM = -K
        ENDIF
C-------DRY WELL?
        IF (IPAR.GT.0 .AND. WT(N).LT.ZERO) GOTO 60
C-------OBSERVATION AT THIS TIME STEP?
        IF ((NDER(4,N).NE.ITS-1.OR.TOFF(N).LE.ZERO) .AND.
     &      NDER(4,N).NE.ITS) GOTO 60
        II = NDER(2,N)
        JJ = NDER(3,N)
        IO = IOFF(N)
        JO = JOFF(N)
        V = 0.0
        DO 40 M = 1, MM
          KK = K
          PROP = 1.
          IF (K.LT.0) THEN
            KK = MLAY(M,ML)
            PROP = PR(M,ML)
          ENDIF
          IF (KK.EQ.0) GOTO 50
C--------CALCULATE CONTRIBUTION FROM THIS LAYER TO
C-----------SENSITIVITY-EQUATION SENSITIVITIES
          V = V + PROP*(RINT(1,N)*SNEW(JJ,II,KK)+
     &                  RINT(2,N)*SNEW(JJ+JO,II,KK)+
     &                  RINT(3,N)*SNEW(JJ,II+IO,KK)+
     &                  RINT(4,N)*SNEW(JJ+JO,II+IO,KK))
   40   CONTINUE
C-------INDEX WHICH, IF NOT ZERO, IDENTIFIES THE HEAD USED TO
C-------CALCULATE DRAWDOWN
   50   N1 = NDER(5,N)
C---------SENSITIVITY-EQUATION SENSITIVITIES
        IF (NDER(4,N).EQ.ITS) THEN
          IF (LNIIPP.GT.0) THEN
            X(IP,N) = B(IIPP)*V
          ELSE
            X(IP,N) = V
          ENDIF
        ENDIF
        IF (NDER(4,N).EQ.ITS-1 .AND. TOFF(N).GT.ZERO) THEN
          IF (LNIIPP.GT.0) THEN
            X(IP,N) = X(IP,N) + TOFF(N)*((B(IIPP)*V)-X(IP,N))
          ELSE
            X(IP,N) = X(IP,N) + TOFF(N)*(V-X(IP,N))
          ENDIF
        ENDIF
        IF (N1.GT.0 .AND. ((NDER(4,N).EQ.ITS.AND.TOFF(N).EQ.ZERO).OR.
     &      (NDER(4,N).EQ.ITS-1.AND.TOFF(N).GT.ZERO)))
     &      X(IP,N) = X(IP,N) - X(IP,N1)
   60 CONTINUE
      RETURN
      END
C=======================================================================
      SUBROUTINE OBS1BAS6HPR(ITERSS,ITMXP,IUSS,SSGF)
C
C     VERSION 20010613 ERB
C     ******************************************************************
C     WRITE CONTRIBUTION TO SSWR OF HEAD OBSERVATIONS TO _ss FILE
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER ITMXP, IUSS
      LOGICAL LOP
      DIMENSION SSGF(ITMXP+1)
C     ------------------------------------------------------------------
  660 FORMAT(1X,'ITERATION',2X,A)
  670 FORMAT(1X,I5,6X,G14.7)
C
      INQUIRE(UNIT=IUSS,OPENED=LOP)
      IF (LOP) THEN
C       CHOOSE HEADER TO WRITE
        WRITE (IUSS,660)'SSWR-(HEAD OBSERVATIONS ONLY)'
C
C       WRITE CONTRIBUTION TO SSWR FOR EACH ITERATION
        DO 10 IT = 1, ITERSS
          WRITE(IUSS,670) IT,SSGF(IT)
   10   CONTINUE
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE SOBS1BAS6HOH(NH,ND,WT,OBSNAM,HOBS,H,JDRY,IO,IOUT,D,RSQ,
     &                     NRUNS,NPOST,NNEGT,MPR,IPR,AVET,WTRL,NRSO,
     &                     IUGDO,OUTNAM,IPLOT,IPLPTR,LCOBBAS,ISSWR,
     &                     SSGF,ITMXP,OTIME)
C     FORMERLY SHED1O
C-----VERSION 19990421 ERB
C     ******************************************************************
C     CALCULATE AND PRINT WEIGHTED RESIDUALS FOR HEAD OBSERVATIONS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL AVE, AVET, D, H, HOBS, RES, RSQ, VMAX,
     &     VMIN, W, WT, WT2, WTR, WTRL
      INTEGER IO, IOUT, IPR, JDRY, MPR,
     &        N, ND, NH, NMAX, NMIN, NNEG, NNEGT, NPOS, NPOST,
     &        NRSO, NRUNS
      INTEGER IUGDO(6), IPLOT(ND+IPR+MPR), IPLPTR(ND+IPR+MPR)
      CHARACTER*12 OBSNAM(ND)
      CHARACTER*200 OUTNAM
      DIMENSION H(ND), HOBS(ND), WT(ND), D(ND+MPR+IPR), SSGF(ITMXP+1)
      REAL OTIME(ND)
C     ------------------------------------------------------------------
C
  500 FORMAT (/,' DATA AT HEAD LOCATIONS',//,8X,'OBSERVATION',5X,
     &        'OBSER-',5X,'SIMUL.',26X,
     &        'WEIGHTED',/,
     &        '   OBS#    NAME',9X,
     &        'VATION',5X,'EQUIV.',4X,'RESIDUAL',2X,'WEIGHT**.5',2X,
     &        'RESIDUAL',/,26X,'*',10X,'*')
  505 FORMAT (1X,I6,1X,A,1X,5(1X,G10.3))
  507 FORMAT (/,1X,'* THE OBSERVATION (AND CORRESPONDING SIMULATED',
     &    ' EQUIVALENT) IS',' HEAD OR TEMPORAL',/,3X,
     &    'CHANGE IN HEAD, AS SPECIFIED IN THE "HOB" ',
     &    'INPUT FILE.  NEGATIVE TEMPORAL',/,3X,
     &    'CHANGES INDICATE DRAWDOWN.')
  510 FORMAT (1X,I6,1X,A,2X,G10.3,'     OMITTED')
  515 FORMAT (/,' SUM OF SQUARED WEIGHTED RESIDUALS (HEADS ONLY)  ',
     &        G11.5)
  520 FORMAT (/,' STATISTICS FOR HEAD RESIDUALS :',/,
     &        ' MAXIMUM WEIGHTED RESIDUAL  :',G10.3,' OBS#',I7,/,
     &        ' MINIMUM WEIGHTED RESIDUAL  :',G10.3,' OBS#',I7,/,
     &        ' AVERAGE WEIGHTED RESIDUAL  :',G10.3,/,
     &        ' # RESIDUALS >= 0. :',I7,/,' # RESIDUALS < 0.  :',I7,/,
     &        ' NUMBER OF RUNS  :',I5,'  IN',I5,' OBSERVATIONS')
  525 FORMAT (2G20.7)
  530 FORMAT (' ')
  540 FORMAT (2(G15.7,1X),I5,2X,A,2X,G15.7)
  550 FORMAT (G15.7,1X,I5,2X,A)
C
      IF (IO.EQ.1) WRITE (IOUT,500)
      NNEG = 0
      NPOS = 0
      VMAX = -1.E20
      VMIN = 1.E20
      AVE = 0.
      DO 10 N = LCOBBAS, LCOBBAS+NH-1
        W = WT(N)
        IF (W.LT.0.) THEN
          IF (IO.EQ.1) WRITE (IOUT,510) N, OBSNAM(N), HOBS(N)
          JDRY = JDRY + 1
          GOTO 10
        ENDIF
        NRSO = NRSO + 1
        IPLPTR(NRSO) = N
        RES = HOBS(N) - H(N)
        WT2 = SQRT(W)
        WTR = RES*WT2
        IF (IO.EQ.1) THEN
          WRITE (IOUT,505) N, OBSNAM(N), HOBS(N),
     &                     H(N), RES, WT2, WTR
          IF (OUTNAM.NE.'NONE') THEN
            WRITE (IUGDO(1),540) H(N), HOBS(N), IPLOT(N), OBSNAM(N),
     &                           OTIME(N)
            WRITE (IUGDO(2),540) WT2*H(N), WT2*HOBS(N), IPLOT(N),
     &                           OBSNAM(N)
            WRITE (IUGDO(3),540) WT2*H(N), WTR, IPLOT(N), OBSNAM(N)
            WRITE (IUGDO(4),550) RES, IPLOT(N), OBSNAM(N)
            WRITE (IUGDO(5),550) WTR, IPLOT(N), OBSNAM(N)
            D(N-JDRY) = WTR
          ENDIF
        ENDIF
        RSQ = RSQ + (WTR**2)
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
        WTRL = WTR
        AVE = AVE + WTR
   10 CONTINUE
      IF (IO.EQ.1) WRITE (IOUT,507)
      IF (ISSWR.GT.0) SSGF(ISSWR) = RSQ
      IF (NRSO.NE.0) THEN
        AVET = AVET + AVE
        NPOST = NPOST + NPOS
        NNEGT = NNEGT + NNEG
        AVE = AVE/REAL(NRSO)
        IF (IO.EQ.1) WRITE (IOUT,520) VMAX, NMAX, VMIN, NMIN, AVE, NPOS,
     &                                NNEG, NRUNS, NRSO
        IF (IO.EQ.1) WRITE (IOUT,515) RSQ
      ENDIF
      RETURN
      END
C=======================================================================
      SUBROUTINE SOBS1BAS6HIA(NDER,COFF,ROFF,DELR,DELC,NCOL,NROW,
     &                      RINT,JOFF,IOFF,MLAY)
C-----VERSION 19990621 ERB
C     ******************************************************************
C     CALCULATE INTERPOLATION COEFFICIENTS FOR LOCATING OBSERVED HEADS
C     ASSUMING ALL CELLS ARE ACTIVE.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL COFF, DELC, DELR, RINT, ROFF
      INTEGER I, I1, IBI, IBIJ, IBJ, IOFF, J, J1, JOFF, K,
     &        MLAY, NCOL, NDER, NROW
      DIMENSION NDER(5), DELR(NCOL), DELC(NROW), RINT(4)
C     ------------------------------------------------------------------
C
      K = NDER(1)
      IF (K.LT.0) K = MLAY
      I = NDER(2)
      J = NDER(3)
      I1 = I + 1
      J1 = J + 1
      IOFF = 1
      JOFF = 1
      IF (ROFF.LT.0.) THEN
        I1 = I - 1
        IOFF = -1
      ENDIF
      IF (COFF.LT.0.) THEN
        J1 = J - 1
        JOFF = -1
      ENDIF
      IF (I1.GE.1 .AND. I1.LE.NROW) IBI = 1
      IF (J1.GE.1 .AND. J1.LE.NCOL) IBJ = 1
      IF (I1.GE.1 .AND. I1.LE.NROW .AND. J1.GE.1 .AND. J1.LE.NCOL)
     &    IBIJ = 1
      IF (I1.LT.1 .OR. I1.GT.NROW) THEN
        ROFF = 0.
        IBI = 0
      ENDIF
      IF (J1.LT.1 .OR. J1.GT.NCOL) THEN
        COFF = 0.
        IBJ = 0
      ENDIF
      IF (I1.LT.1 .OR. I1.GT.NROW .OR. J1.LT.1 .OR. J1.GT.NCOL) IBIJ = 0
C
      CALL SOBS1BAS6HBF(COFF,DELC,DELR,I,I1,IBI,IBIJ,IBJ,IOFF,J,J1,JOFF,
     &                NCOL,NROW,RINT,ROFF)
C
      RETURN
      END
C=======================================================================
      SUBROUTINE SOBS1BAS6HIB(NDER,COFF,ROFF,DELR,DELC,IBOUND,NCOL,NROW,
     &                      NLAY,RINT,JOFF,IOFF,MLAY)
C-----VERSION 19990621 ERB
C     FORMERLY PART OF SSEN1I
C     ******************************************************************
C     CALCULATE INTERPOLATION COEFFICIENTS FOR LOCATING OBSERVED HEADS
C     USING CURRENT IBOUND VALUES.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL COFF, DELC, DELR, RINT, ROFF
      INTEGER I, I1, IBI, IBIJ, IBJ, IBOUND, IOFF, J, J1, JOFF, K,
     &        MLAY, NCOL, NDER, NLAY, NROW
      DIMENSION NDER(5), DELR(NCOL), DELC(NROW), IBOUND(NCOL,NROW,NLAY),
     &          RINT(4)
C     ------------------------------------------------------------------
C
      K = NDER(1)
      IF (K.LT.0) K = MLAY
      I = NDER(2)
      J = NDER(3)
      I1 = I + 1
      J1 = J + 1
      IOFF = 1
      JOFF = 1
      IF (ROFF.LT.0.) THEN
        I1 = I - 1
        IOFF = -1
      ENDIF
      IF (COFF.LT.0.) THEN
        J1 = J - 1
        JOFF = -1
      ENDIF
      IF (I1.GE.1 .AND. I1.LE.NROW) IBI = IBOUND(J,I1,K)
      IF (J1.GE.1 .AND. J1.LE.NCOL) IBJ = IBOUND(J1,I,K)
      IF (I1.GE.1 .AND. I1.LE.NROW .AND. J1.GE.1 .AND. J1.LE.NCOL)
     &    IBIJ = IBOUND(J1,I1,K)
      IF (I1.LT.1 .OR. I1.GT.NROW) THEN
        ROFF = 0.
        IBI = 0
      ENDIF
      IF (J1.LT.1 .OR. J1.GT.NCOL) THEN
        COFF = 0.
        IBJ = 0
      ENDIF
      IF (I1.LT.1 .OR. I1.GT.NROW .OR. J1.LT.1 .OR. J1.GT.NCOL) IBIJ = 0
C
      CALL SOBS1BAS6HBF(COFF,DELC,DELR,I,I1,IBI,IBIJ,IBJ,IOFF,J,J1,JOFF,
     &                NCOL,NROW,RINT,ROFF)
C
      RETURN
      END
C=======================================================================
      SUBROUTINE SOBS1BAS6HBF(COFF,DELC,DELR,I,I1,IBI,IBIJ,IBJ,IOFF,J,
     &                      J1,JOFF,NCOL,NROW,RINT,ROFF)
C-----VERSION 19990621 ERB
C     FORMERLY PART OF SSEN1I
C     ******************************************************************
C     CALCULATE BASIS FUNCTIONS FOR INTERPOLATING OBSERVED HEADS.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL A, COFF, DC, DCF, DELC, DELR, DR, DRF, RINT, ROFF
      INTEGER I, I1, IBI, IBIJ, IBJ, IOFF, IR, J, J1, JOFF, NCOL, NROW
      DIMENSION DELR(NCOL), DELC(NROW), RINT(4)
C     ------------------------------------------------------------------
C
      IF ((ABS(ROFF).LT..001.AND.ABS(COFF).LT..001) .OR.
     &    (ABS(ROFF).LT..001.AND.IBJ.EQ.0) .OR.
     &    (ABS(COFF).LT..001.AND.IBI.EQ.0) .OR. (IBI.EQ.0.AND.IBJ.EQ.0))
     &    THEN
        IOFF = 0
        JOFF = 0
        DO 10 IR = 1, 4
          RINT(IR) = .25
   10   CONTINUE
        RETURN
      ENDIF
C
C---CALCULATE CONSTANTS
      IF (ABS(ROFF).GE..001) THEN
        DC = (DELC(I)+DELC(I1))/2.
        DCF = ABS(ROFF)*DELC(I)
      ENDIF
      IF (ABS(COFF).GE..001) THEN
        DR = (DELR(J)+DELR(J1))/2.
        DRF = ABS(COFF)*DELR(J)
      ENDIF
      IF (ABS(ROFF).GE..001 .AND. ABS(COFF).GE..001) A = 1/(DC*DR)
C
C---LINEAR INTERPOLATION
      IF (ABS(ROFF).LT..001 .OR. (IBI.EQ.0.AND.IBIJ.EQ.0)) THEN
        IOFF = 0
        RINT(1) = 0.5*(1.-DRF/DR)
        RINT(2) = 0.5*DRF/DR
        RINT(3) = RINT(1)
        RINT(4) = RINT(2)
C
      ELSEIF (ABS(COFF).LT..001 .OR. (IBJ.EQ.0.AND.IBIJ.EQ.0)) THEN
        JOFF = 0
        RINT(1) = 0.5*(1.-DCF/DC)
        RINT(2) = RINT(1)
        RINT(3) = 0.5*DCF/DC
        RINT(4) = RINT(3)
C
C---CALCULATE BASIS FUNCTIONS FOR INTERPOLATION ON A RECTANGLE
      ELSEIF (IBJ.NE.0 .AND. IBI.NE.0 .AND. IBIJ.NE.0) THEN
        RINT(3) = A*(DR-DRF)*DCF
        RINT(4) = A*DRF*DCF
        RINT(2) = A*DRF*(DC-DCF)
        RINT(1) = A*(DR-DRF)*(DC-DCF)
C
C---CALCULATE BASIS FUNCTIONS FOR INTERPOLATION ON A TRIANGLE
      ELSEIF (IBJ.EQ.0) THEN
        RINT(1) = A*(DR*DC-DR*DCF)
        RINT(2) = 0.0
        RINT(3) = A*(DR*DCF-DC*DRF)
        RINT(4) = A*(DC*DRF)
C
      ELSEIF (IBI.EQ.0) THEN
        RINT(1) = A*(DR*DC-DC*DRF)
        RINT(4) = A*(DR*DCF)
        RINT(2) = A*(DC*DRF-DR*DCF)
        RINT(3) = 0.0
C
      ELSEIF (IBIJ.EQ.0) THEN
        RINT(1) = A*(DR*DC-DC*DRF-DR*DCF)
        RINT(3) = A*(DR*DCF)
        RINT(2) = A*(DC*DRF)
        RINT(4) = 0.0
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE OBS1BAS6FAL(IUCHOB,NQ,NQC,NQT,IOUT,NQCH,NQTCH,IOBSUM,
     &                       LCOBCHD,ITMXP,LCSSCH,ISUM,IOBS)
C     VERSION 20000125
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR FLOW OBSERVATIONS AT CONSTANT-HEAD
C     BOUNDARY CELLS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER IOUT, IUCHOB, NQ, NQC, NQT, NQCH, NQTCH
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
C     IDENTIFY PROCESS
      WRITE(IOUT,490) IUCHOB
  490 FORMAT(/,' OBS1BAS6F -- OBSERVATION PROCESS (CONSTANT-HEAD',
     &    ' BOUNDARY FLOW OBSERVATIONS)',/,' VERSION 1.0, 12/03/99',/,
     &    ' INPUT READ FROM UNIT ',I3)
C
C  Turn off observation package if OBS is not active
      IF(IOBS.LE.0) THEN
        WRITE(IOUT,610)
610     FORMAT(/,1X,'WARNING: OBSERVATION (OBS) FILE IS NOT LISTED BUT',
     &      ' THE CHD OBSERVATION',/,' FILE (CHOB) IS',
     &     ' LISTED -- TURNING OFF CHD OBSERVATIONS (OBS1BAS6FAL)')
        IUCHOB = 0
        RETURN
      ENDIF
C
C  read data
      CALL URDCOM(IUCHOB,IOUT,LINE)
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NQCH,DUM,IOUT,IUCHOB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NQCCH,DUM,IOUT,IUCHOB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NQTCH,DUM,IOUT,IUCHOB)
      WRITE (IOUT,500) NQCH, NQCCH, NQTCH
   10 FORMAT(15I5)
  500 FORMAT (/,
     &    ' NUMBER OF FLOW-OBSERVATION CONSTANT-HEAD-CELL GROUPS:',I5,/,
     &    '   NUMBER OF CELLS IN CONSTANT-HEAD-CELL GROUPS......:',I5,/,
     &    '   NUMBER OF CONSTANT-HEAD-CELL FLOWS................:',I5)
C
      NQ = NQ + NQCH
      NQC = NQC + NQCCH
      NQT = NQT + NQTCH
      LCSSCH = ISUM
      ISUM = ISUM + ITMXP + 1
C     POINTER TO OBSERVATION ARRAYS
      LCOBCHD = IOBSUM
      IOBSUM = IOBSUM + NQTCH
C
      RETURN
      END
C=======================================================================
      SUBROUTINE OBS1BAS6FRP(NCOL,NROW,NPER,IUCHOB,IOUT,OBSNAM,NHT,JT,
     &                       IBT,NQOB,NQCL,IQOB,QCLS,IERR,HOBS,TOFF,
     &                       WTQ,IOWTQ,IPRN,NDMH,NSTP,PERLEN,TSMULT,
     &                       ISSA,ITRSS,NQAR,NQCAR,NQTAR,IQ1,NQT1,NDD,
     &                       NQCH,NQTCH,NT,NC,IPLOT,NAMES,ND,IPR,MPR,
     &                       IOWTQCH,NLAY,OTIME)
C     VERSION 20010921 ERB
C     ******************************************************************
C     READ, CHECK AND STORE FLOW-OBSERVATION DATA FOR CHD BOUNDARIES.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL BLANK, EVFCH, HOBS, PERLEN, QCLS, TOFF, TOFFSET, TOMULTCH,
     &     TSMULT, WTQ
      INTEGER I, I4, IBT, IERR, IOUT, IOWTQ, IPRN, IQ, IQOB, IUCHOB,
     &        IWT, J, JT, L, N, NC, NC1, NC2, NCOL, NDMH, NHT, NPER,
     &        NQCL, NQOB, NROW, NSTP, NT, NT1, NT2, ISSA
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
      DATA ANAME/'COVARIANCE OF CONSTANT-HEAD-CELL FLOW OBSERVATIONS'/
C     ------------------------------------------------------------------
  500 FORMAT (15X,2F5.0,F10.0)
  505 FORMAT (8F10.0)
  510 FORMAT (A4,6X,I5,3F10.0,I5)
  515 FORMAT (A3,2X,2I5)
  517 FORMAT (/,' CONSTANT-HEAD-CELL FLOW OBSERVATION VARIANCES',
     &        ' ARE MULTIPLIED BY: ',G15.4)
  520 FORMAT (/,' OBSERVED CONSTANT-HEAD-CELL FLOW DATA',/,
     &' -- TIME OFFSETS ARE MULTIPLIED BY: ',G12.5)
  525 FORMAT (/,'   GROUP NUMBER: ',I3,'   BOUNDARY TYPE: ',A,
     &'   NUMBER OF CELLS IN GROUP: ',I5,/,
     &'   NUMBER OF FLOW OBSERVATIONS: ',I5,//,
     &40X,'OBSERVED',/,
     &20X,'REFER.',12X,'BOUNDARY FLOW',/,
     &7X,'OBSERVATION',2X,'STRESS',4X,'TIME',5X,'GAIN (-) OR',14X,
     &'STATISTIC   PLOT',/,
     &2X,'OBS#    NAME',6X,'PERIOD   OFFSET',5X,'LOSS (+)',
     &4X,'STATISTIC     TYPE      SYM.')
  535 FORMAT (1X,I5,1X,A12,2X,I4,2X,G11.4,1X,G11.4,1X,G11.4,2X,A10,
     &1X,I5)
  540 FORMAT (/,'       LAYER  ROW  COLUMN    FACTOR')
  550 FORMAT (4X,F8.0,F6.0,F7.0,F9.2)
  555 FORMAT (4X,F8.0,F6.0,F9.2)
  560 FORMAT (/,' FOR OBS',I5,' STATISTIC RELATED TO WEIGHT < OR =0 -- '
     &        ,'STOP EXECUTION (OBS1BAS6FRP)',/)
  590 FORMAT (/,' ROW OR COLUMN NUMBER INVALID',
     &        ' -- STOP EXECUTION (OBS1BAS6FRP)',/)
  600 FORMAT (/,' CELL NOT DESIGNATED AS CONSTANT HEAD',
     &        ' -- STOP EXECUTION (OBS1BAS6FRP)',/)
  605 FORMAT (/,' OBSERVATION',I5,' EQUALS ZERO, THE STATISTIC ',
     &        'CAN NOT BE A',/,' COEFFICIENT OF VARIATION (ISTAT=2)',
     &        ' -- STOP EXECUTION (OBS1BAS6FRP)')
  615 FORMAT (//,1X,A,/,1X,42('-'))
  620 FORMAT (/,' ERROR:  SEE ABOVE FOR ERROR MESSAGE AND "STOP',
     &        ' EXECUTION" (OBS1BAS6FRP)')
C
C-----READ TIME-OFFSET MULTIPLIER FOR FLOW-OBSERVATION TIMES AND INPUT
C     ERROR VARIANCE FOR FLOW OBSERVATIONS
      READ(IUCHOB,*) TOMULTCH, EVFCH, IOWTQCH
      IF (IOWTQCH.GT.0) IOWTQ = 1
C
C-------INITIALIZE VARIABLES
      IQ2 = IQ1 + NQCH - 1
      DO 20 IQ = IQ1,IQ2
        IBT(2,IQ) = 0
   20 CONTINUE
C-------WRITE TITLE AND LOOP THROUGH CELL GROUPS (READ ITEMS 3, 4, AND 5)
      WRITE (IOUT,517) EVFCH
      WRITE (IOUT,520) TOMULTCH
      DO 120 IQ = IQ1,IQ2
C       READ ITEM 3
        READ (IUCHOB,*) NQOB(IQ), NQCL(IQ)
        IBT(1,IQ) = 5
        WRITE (IOUT,525) IQ, 'CHD', NQCL(IQ), NQOB(IQ)
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
          IF (IOWTQCH.GT.0) THEN
            READ (IUCHOB,*) OBSNAM(N), IREFSP, TOFFSET, HOBS(N), STAT,
     &                      ISP, IPLOT(N)
            NAMES(N) = OBSNAM(N)
            WRITE (IOUT,535) N, OBSNAM(N), IREFSP, TOFFSET, HOBS(N),
     &                       0.0, STATYP(ISP), IPLOT(N)
          ELSE
            READ (IUCHOB,*) OBSNAM(N), IREFSP, TOFFSET, HOBS(N),
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
            WTQ(J,J) = WTQ(J,J)*EVFCH
            IF (WTQ(J,J).LE.0.) THEN
              WRITE (IOUT,560) N
              IERR = 1
            ENDIF
          ENDIF
          CALL UOBSTI(OBSNAM(N),IOUT,ISSA,ITRSS,NPER,NSTP,IREFSP,
     &                IQOB(J),PERLEN,TOFF(N),TOFFSET,TOMULTCH,TSMULT,1,
     &                OTIME(N))
C----------ERROR CHECKING
          IF (IQOB(J).GE.JT) THEN
            JT = IQOB(J)
            IF (TOFF(N).GT.0.) JT = JT+1
          ENDIF
   30   CONTINUE
C----------READ LAYER, ROW, COLUMN, AND FACTOR (ITEM 5)
        NC1 = NC + 1
        NC2 = NC + NQCL(IQ)
        WRITE (IOUT,540)
        DO 40 L = NC1, NC2
          READ (IUCHOB,*) (QCLS(I,L),I=1,4)
          IF (QCLS(4,L).EQ.0. .OR. I4.EQ.1) QCLS(4,L) = 1.
          WRITE (IOUT,550) (QCLS(I,L),I=1,4)
          K = QCLS(1,L)
          I = QCLS(2,L)
          J = QCLS(3,L)
          IF (K.LE.0 .OR. K.GT.NLAY .OR .J.LE.0 .OR. J.GT.NCOL .OR.
     &        I.LE.0 .OR. I.GT.NROW) THEN
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
C-------READ FULL COVARIANCE MATRIX ON CONSTANT-HEAD-CELL
C       FLOW-OBSERVATION DATA
      IPRN = 0
      NQT2 = NQT1 + NQTCH - 1
      IF (IOWTQCH.GT.0 .AND. NQTCH.GT.0) THEN
C       READ ITEM 6
        READ (IUCHOB,*) FMTIN, IPRN
C       READ ITEM 7
        DO 140 I = NQT1,NQT2
          READ (IUCHOB,FMTIN) (BLANK,J=NQT1,I-1), (WTQ(I,J),J=I,NQT2)
          DO 130 J = I, NQT2
            WTQ(I,J) = WTQ(I,J)*EVFCH
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
      SUBROUTINE OBS1BAS6FFM(NQ,NQOB,NQCL,IQOB,QCLS,IBT,HNEW,NCOL,NROW,
     &                       NLAY,IBOUND,NHT,H,TOFF,ITS,NQAR,NQCAR,
     &                       NQTAR,ICHFLG,CR,CC,CV,BOTM,NBOTM,LAYHDT,ND,
     &                       IOUT,KKPER)
C     VERSION 20011114 ERB
C     ******************************************************************
C     CALCULATE SIMULATED EQUIVALENTS TO OBSERVED CONSTANT-HEAD FLOWS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL FACT, H, QCLS, TOFF, ZERO
      INTEGER I, IBOUND, IBT, IBT1, IOUT, IQ, IQOB, ITS, J, K, LAYHDT,
     &        N, NC, NC1, NC2, NCOL, NHT, NLAY, NQ, NQCL, NQOB, NROW,
     &        NT, NT1, NT2
      DOUBLE PRECISION HNEW(NCOL,NROW,NLAY), RATE
      DIMENSION IBOUND(NCOL,NROW,NLAY), IBT(2,NQAR), NQOB(NQAR),
     &          NQCL(NQAR), IQOB(NQTAR), QCLS(5,NQCAR), H(ND), TOFF(ND),
     &          CR(NCOL,NROW,NLAY), CC(NCOL,NROW,NLAY),
     &          CV(NCOL,NROW,NLAY), BOTM(NCOL,NROW,0:NBOTM),
     &          LAYHDT(NLAY)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
  500 FORMAT(/,
     &' *** ERROR: CONSTANT-HEAD FLOW OBSERVATION SPECIFIED FOR CELL (',
     &I3,',',I5,',',I5,'),',/,
     &12X,'BUT THIS CELL IS NOT CONSTANT-HEAD IN STRESS PERIOD ',I4,/
     &12X,'-- STOP EXECUTION (OBS1BAS6FFM)')
C
C-------INITIALIZE VARIABLES
      ZERO = 0.0
      NC = 0
      NT1 = 1
C-------LOOP THROUGH BOUNDARY FLOWS
      DO 60 IQ = 1, NQ
        IBT1 = IBT(1,IQ)
        NT2 = NT1 + NQOB(IQ) - 1
        IF (IBT1.NE.5) GOTO 50
C----------WAS THERE A MEASUREMENT AT THIS BOUNDARY THIS TIME STEP?
        DO 40 NT = NT1, NT2
          IF (IQOB(NT).EQ.ITS .OR.
     &        (IQOB(NT).EQ.ITS-1.AND.TOFF(NHT+NT).GT.ZERO)) THEN
C----------LOOP THROUGH CELLS.
            NC1 = NC + 1
            NC2 = NC + NQCL(IQ)
            DO 30 N = NC1, NC2
              K = QCLS(1,N)
              I = QCLS(2,N)
              J = QCLS(3,N)
              IF (IBOUND(J,I,K).GE.0) THEN
                WRITE(IOUT,500) K,I,J,KKPER
                CALL USTOP(' ')
              ENDIF
C-------------CALL SUBROUTINE TO CALCULATE CONSTANT-HEAD FLOW FOR CELL
              CALL SOBS1BAS6FFLW(J,I,K,ICHFLG,IBOUND,HNEW,CR,CC,CV,BOTM,
     &                          NBOTM,NCOL,NROW,NLAY,RATE,LAYHDT)
C-------------SUM VALUES FROM INDIVIDUAL CELLS.
C----------------CALCULATE FACTOR FOR TEMPORAL INTERPOLATION
   20         FACT = 1.0
              IF (TOFF(NHT+NT).GT.ZERO) THEN
                IF (IQOB(NT).EQ.ITS) FACT = 1. - TOFF(NHT+NT)
                IF (IQOB(NT).EQ.ITS-1) FACT = TOFF(NHT+NT)
              ENDIF
C---------------FLOWS
              H(NHT+NT) = H(NHT+NT) + RATE*FACT*QCLS(4,N)
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
      SUBROUTINE OBS1BAS6FDR(NQ,NQOB,NQCL,IQOB,QCLS,IBT,HNEW,IP,SNEW,
     &                       NCOL,NROW,NLAY,IBOUND,NHT,X,TOFF,ITS,
     &                       NQAR,NQCAR,NQTAR,ICHFLG,BOTM,NBOTM,PIDTMP,
     &                       LAYHDT,RMLT,NMLTAR,IZON,NZONAR,DELC,DELR,
     &                       HFB,NHFB,IUHFB,MXACTFB,SV,VKA,HK,HANI,CR,
     &                       CC,CV,NPE,IERR,IERRU,IOUT,IULPF,LN,NPLIST,
     &                       ND,IUHUF,HKCC,HUFTHK,NHUF,GS)
C     ******************************************************************
C     CALCULATE SENSITIVITIES FOR FLOW OBSERVATIONS FOR THE CONSTANT
C     HEAD BOUNDARY PACKAGE
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL FACT, QCLS, TOFF, X, ZERO
      INTEGER I, IBOUND, IBT, IBT1, IP, IQ, IQOB, ITS, J, K, LN, N, NC,
     &        NC1, NC2, NCOL, NHT, NLAY, NPE, NQ, NQCL, NQOB, NROW,
     &        NT, NT1, NT2
      DOUBLE PRECISION HNEW(NCOL,NROW,NLAY), SNEW(NCOL,NROW,NLAY),
     &        DRATE
      DIMENSION IBOUND(NCOL,NROW,NLAY), X(NPE,ND), IBT(2,NQAR),
     &          NQOB(NQAR), NQCL(NQAR), IQOB(NQTAR),
     &          QCLS(5,NQCAR), TOFF(ND),
     &          CR(NCOL,NROW,NLAY), CC(NCOL,NROW,NLAY),
     &          CV(NCOL,NROW,NLAY), BOTM(NCOL,NROW,0:NBOTM),
     &          LAYHDT(NLAY),RMLT(NCOL,NROW,NMLTAR),
     &          DELR(NCOL),DELC(NROW),
     &          HFB(7,MXACTFB),SV(NCOL,NROW,NLAY),VKA(NCOL,NROW,NLAY),
     &          HK(NCOL,NROW,NLAY),HANI(NCOL,NROW,NLAY),
     &          IZON(NCOL,NROW,NZONAR), LN(NPLIST),
     &          HKCC(NCOL,NROW,NLAY),HUFTHK(NCOL,NROW,NHUF,2),
     &          GS(NCOL,NROW)
      INCLUDE 'param.inc'
      CHARACTER*4 PIDTMP
C     ------------------------------------------------------------------
  505 FORMAT (/,' OBS#',I5,', ID ',A4,', TIME STEP ',I5)
  510 FORMAT ('    LAYER   ROW  COLUMN')
  520 FORMAT (3I7)
  525 FORMAT (' *',I5,I7)
  540 FORMAT (' CELL #',I5,
     &        ' OF HEAD-DEP. BOUNDARY GAIN OR LOSS OBS#',I5,' ID=',A,
     &        ' NOT FOUND IN CELLS LISTED FOR',/,'CHD PACKAGE',
     &        ' -- STOP EXECUTION (OBS1BAS6FDR)')
C
C-------INITIALIZE VARIABLES
      ZERO = 0.0
      NC = 0
      NT1 = 1
      IIPP = IPPTR(IP)
C-------LOOP THROUGH BOUNDARY FLOWS
      DO 60 IQ = 1, NQ
        IBT1 = IBT(1,IQ)
        NT2 = NT1 + NQOB(IQ) - 1
        IF (IBT1.NE.5) GOTO 50
C----------WAS THERE A MEASUREMENT AT THIS BOUNDARY THIS TIME STEP?
        DO 40 NT = NT1, NT2
          IF (IQOB(NT).EQ.ITS .OR.
     &        (IQOB(NT).EQ.ITS-1.AND.TOFF(NHT+NT).GT.ZERO)) THEN
C----------LOOP THROUGH CELLS.
            NC1 = NC + 1
            NC2 = NC + NQCL(IQ)
            DO 30 N = NC1, NC2
              K = QCLS(1,N)
              I = QCLS(2,N)
              J = QCLS(3,N)
C-------------CALL SUBROUTINE TO CALCULATE CONSTANT-HEAD FLOW
C----------------SENSITIVITIES FOR CELL
              CALL SOBS1BAS6FFLWDR(J,I,K,ICHFLG,IBOUND,HNEW,CR,CC,CV,
     &                            BOTM,NBOTM,NCOL,NROW,NLAY,DRATE,
     &                            PIDTMP,IIPP,LAYHDT,RMLT,NMLTAR,IZON,
     &                            NZONAR,DELC,DELR,HFB,NHFB,IUHFB,
     &                            MXACTFB,SV,VKA,HK,HANI,SNEW,IERR,
     &                            IERRU,IOUT,IULPF,IUHUF,HKCC,HUFTHK,
     &                            NHUF,GS)
C-------------SUM VALUES FROM INDIVIDUAL CELLS.
C----------------CALCULATE FACTOR FOR TEMPORAL INTERPOLATION
   20         FACT = 1.0
              IF (TOFF(NHT+NT).GT.ZERO) THEN
                IF (IQOB(NT).EQ.ITS) FACT = 1. - TOFF(NHT+NT)
                IF (IQOB(NT).EQ.ITS-1) FACT = TOFF(NHT+NT)
              ENDIF
C---------------SENSITIVITY-EQUATION SENSITIVITIES
              IF (LN(IIPP).GT.0) DRATE = DRATE*B(IIPP)
              X(IP,NHT+NT) = X(IP,NHT+NT) + DRATE*FACT*QCLS(4,N)
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
      SUBROUTINE OBS1BAS6FPR(ITERSS,ITMXP,IUSS,SSCH)
C
C     VERSION 20010613 ERB
C     ******************************************************************
C     WRITE CONTRIBUTION TO SSWR OF FLOW OBSERVATIONS AT CONSTANT-HEAD
C     NODES TO _ss FILE
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER ITMXP, IUSS
      LOGICAL LOP
      DIMENSION SSCH(ITMXP+1)
C     ------------------------------------------------------------------
  660 FORMAT(1X,'ITERATION',2X,A)
  670 FORMAT(1X,I5,6X,G14.7)
C
      INQUIRE(UNIT=IUSS,OPENED=LOP)
      IF (LOP) THEN
        WRITE (IUSS,660)'SSWR-(CONSTANT-HEAD FLOW OBSERVATIONS ONLY)'
C       WRITE CONTRIBUTION TO SSWR FOR EACH ITERATION
        DO 10 IT = 1, ITERSS
          WRITE(IUSS,670) IT,SSCH(IT)
   10   CONTINUE
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE SOBS1BAS6FOH(IO,IOWTQCH,IOUT,NHT,NQTCH,HOBS,H,WTQ,
     &                        OBSNAM,IDIS,WTQS,D,AVET,NPOST,NNEGT,NRUNS,
     &                        RSQ,ND,MPR,IPR,NDMH,WTRL,NRSO,IUGDO,
     &                        OUTNAM,IPLOT,IPLPTR,LCOBCHD,ISSWR,SSCH,
     &                        ITMXP,OTIME)
C     VERSION 19990423 ERB
C     ******************************************************************
C     CALCULATE AND PRINT WEIGHTED RESIDUALS FOR CONSTANT-HEAD BOUNDARY
C     FLOW OBSERVATIONS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL AVE, AVET, D, H, HOBS, RES, RSQ, SWH, VMAX, VMIN, WT2, WTQ,
     &     WTQS, WTR, WTRL
      INTEGER IDIS, IO, IOUT, IOWTQCH, IPR,
     &        J, MPR, N, ND, NDMH, NHT, NMAX, NMIN,
     &        NNEG, NNEGT, NPOS, NPOST, NQ1, NQ2, NQTCH, NRSO, NRUNS
      INTEGER IUGDO(6), IPLOT(ND+IPR+MPR), IPLPTR(ND+IPR+MPR)
      CHARACTER*12 OBSNAM(ND)
      CHARACTER*200 OUTNAM
      DIMENSION H(ND), HOBS(ND), D(ND+MPR+IPR), WTQ(NDMH,NDMH),
     &          WTQS(NDMH,NDMH), SSCH(ITMXP+1)
      REAL OTIME(ND)
C     ------------------------------------------------------------------
C
  500 FORMAT (/,' DATA FOR FLOW OBSERVATIONS AT BOUNDARIES REPRESENTED',
     &' AS CONSTANT-HEAD',//,
     &7X,'OBSERVATION      MEAS.      CALC.',14X,'WEIGHTED',/,
     &'  OBS#    NAME',10X,'FLOW',7X,'FLOW',4X,'RESIDUAL',3X,
     &        'RESIDUAL',/)
  505 FORMAT (/,' DATA FOR FLOW OBSERVATIONS AT BOUNDARIES REPRESENTED',
     &' AS CONSTANT-HEAD',//,
     &7X,'OBSERVATION',6X,'MEAS.      CALC.',26X,'WEIGHTED',/,
     &2X,'OBS#    NAME',10X,'FLOW',7X,'FLOW',5X,'RESIDUAL',2X,
     &'WEIGHT**.5',2X,'RESIDUAL',/)
  510 FORMAT (1X,I5,1X,A12,1X,5(1X,G10.3))
  515 FORMAT (1X,I5,1X,A12,2X,G10.3,'   DISCONNECTED')
  520 FORMAT (/,' SUM OF SQUARED WEIGHTED RESIDUALS',/,
     &'   (CONSTANT-HEAD BOUNDARY FLOWS ONLY)  ',G11.5)
  525 FORMAT (/,' STATISTICS FOR CONSTANT-HEAD BOUNDARY FLOW',
     &        ' RESIDUALS :',/,
     &        ' MAXIMUM WEIGHTED RESIDUAL  :',G10.3,' OBS#',I7,/,
     &        ' MINIMUM WEIGHTED RESIDUAL  :',G10.3,' OBS#',I7,/,
     &        ' AVERAGE WEIGHTED RESIDUAL  :',G10.3,/,
     &        ' # RESIDUALS >= 0. :',I7,/,' # RESIDUALS < 0.  :',I7,/,
     &        ' NUMBER OF RUNS  :',I5,'  IN',I5,' OBSERVATIONS')
  530 FORMAT (2G20.7)
  535 FORMAT (' ')
  540 FORMAT (2(G15.7,1X),I5,2X,A,2X,G15.7)
  550 FORMAT (G15.7,1X,I5,2X,A)
C
      IF (IO.EQ.1) THEN
        IF (IOWTQCH.GT.0) THEN
          WRITE (IOUT,500)
        ELSE
          WRITE (IOUT,505)
        ENDIF
      ENDIF
      IDISCH = 0
      NRESCH = 0
      NRUNSCH = 1
      RSQCH = 0.0
      NNEG = 0
      NPOS = 0
      VMAX = -1.E20
      VMIN = 1.E20
      AVE = 0.
      DO 20 N = LCOBCHD, LCOBCHD+NQTCH-1
        NQ1 = N - NHT
        IF (WTQ(NQ1,NQ1).LT.0.) THEN  ! For constant heads, never true?
          IF (IO.EQ.1) WRITE (IOUT,515) N, OBSNAM(N), HOBS(N)
          IDIS = IDIS + 1
          IDISCH = IDISCH + 1
          GOTO 20
        ENDIF
        NRSO = NRSO + 1
        NRESCH = NRESCH + 1
        IPLPTR(NRSO) = N
        RES = HOBS(N) - H(N)
        IF (IOWTQCH.GT.0) THEN
          WTR = 0.0
          SWH = 0.0
          OWH = 0.0
          DO 10 J = 1, NQTCH
            NQ2 = LCOBCHD + J - 1
            IF (WTQ(NQ2,NQ2).LT.0.0) GOTO 10 ! For constant heads, never true?
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
        RSQCH = RSQCH + (WTR**2)
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
        IF (N.GT.LCOBCHD) THEN
          IF (WTRL*WTR.LT.0.) NRUNSCH = NRUNSCH + 1
        ENDIF
        WTRL = WTR
        AVE = AVE + WTR
   20 CONTINUE
      IF (ISSWR.GT.0) SSCH(ISSWR) = RSQCH
      IF (NQTCH.NE.IDISCH) THEN
        AVET = AVET + AVE
        NPOST = NPOST + NPOS
        NNEGT = NNEGT + NNEG
        AVE = AVE/REAL(NQTCH-IDISCH)
        IF (IO.EQ.1) THEN
          WRITE (IOUT,525) VMAX, NMAX, VMIN, NMIN, AVE, NPOS, NNEG,
     &                     NRUNSCH, NRESCH
          WRITE (IOUT,520) RSQCH
        ENDIF
      ENDIF
      RETURN
      END
C=======================================================================
      SUBROUTINE SOBS1BAS6FFLW(J,I,K,ICHFLG,IBOUND,HNEW,CR,CC,CV,BOTM,
     &                        NBOTM,NCOL,NROW,NLAY,RATE,LAYHDT)
C     VERSION 20010924 ERB
C     ******************************************************************
C     CALCULATE CONSTANT-HEAD BOUNDARY FLOW FOR A GIVEN CELL
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER LAYHDT
      DOUBLE PRECISION HNEW,HD,X1,X2,X3,X4,X5,X6,RATE
C
      DIMENSION HNEW(NCOL,NROW,NLAY), IBOUND(NCOL,NROW,NLAY),
     1     CR(NCOL,NROW,NLAY), CC(NCOL,NROW,NLAY),
     2     CV(NCOL,NROW,NLAY), BOTM(NCOL,NROW,0:NBOTM), LAYHDT(NLAY)
C
      COMMON /DISCOM/LBOTM(999),LAYCBD(999)
C     ------------------------------------------------------------------
C
C6------CLEAR VALUES FOR FLOW RATE THROUGH EACH FACE OF CELL.
      ZERO=0.
      X1=ZERO
      X2=ZERO
      X3=ZERO
      X4=ZERO
      X5=ZERO
      X6=ZERO
C
C7------CALCULATE FLOW THROUGH THE LEFT FACE.
C7------COMMENTS A-C APPEAR ONLY IN THE SECTION HEADED BY COMMENT 7,
C7------BUT THEY APPLY IN A SIMILAR MANNER TO SECTIONS 8-12.
C
C7A-----IF THERE IS NO FLOW TO CALCULATE THROUGH THIS FACE, THEN GO ON
C7A-----TO NEXT FACE.  NO FLOW OCCURS AT THE EDGE OF THE GRID, TO AN
C7A-----ADJACENT NO-FLOW CELL, OR TO AN ADJACENT CONSTANT-HEAD CELL
C7A-----WHEN ICHFLG IS 0.
      IF(J.EQ.1) GO TO 30
      IF(IBOUND(J-1,I,K).EQ.0) GO TO 30
      IF(ICHFLG.EQ.0 .AND. IBOUND(J-1,I,K).LT.0) GO TO 30
C
C7B-----CALCULATE FLOW THROUGH THIS FACE INTO THE ADJACENT CELL.
      HDIFF=HNEW(J,I,K)-HNEW(J-1,I,K)
      X1=HDIFF*CR(J-1,I,K)
C
C8------CALCULATE FLOW THROUGH THE RIGHT FACE.
   30 IF(J.EQ.NCOL) GO TO 60
      IF(IBOUND(J+1,I,K).EQ.0) GO TO 60
      IF(ICHFLG.EQ.0 .AND. IBOUND(J+1,I,K).LT.0) GO TO 60
      HDIFF=HNEW(J,I,K)-HNEW(J+1,I,K)
      X2=HDIFF*CR(J,I,K)
C
C9------CALCULATE FLOW THROUGH THE BACK FACE.
   60 IF(I.EQ.1) GO TO 90
      IF (IBOUND(J,I-1,K).EQ.0) GO TO 90
      IF(ICHFLG.EQ.0 .AND. IBOUND(J,I-1,K).LT.0) GO TO 90
      HDIFF=HNEW(J,I,K)-HNEW(J,I-1,K)
      X3=HDIFF*CC(J,I-1,K)
C
C10-----CALCULATE FLOW THROUGH THE FRONT FACE.
   90 IF(I.EQ.NROW) GO TO 120
      IF(IBOUND(J,I+1,K).EQ.0) GO TO 120
      IF(ICHFLG.EQ.0 .AND. IBOUND(J,I+1,K).LT.0) GO TO 120
      HDIFF=HNEW(J,I,K)-HNEW(J,I+1,K)
      X4=HDIFF*CC(J,I,K)
C
C11-----CALCULATE FLOW THROUGH THE UPPER FACE.
  120 IF(K.EQ.1) GO TO 150
      IF (IBOUND(J,I,K-1).EQ.0) GO TO 150
      IF(ICHFLG.EQ.0 .AND. IBOUND(J,I,K-1).LT.0) GO TO 150
      HD=HNEW(J,I,K)
      IF(LAYHDT(K).EQ.0) GO TO 122
      TMP=HD
      TOP=BOTM(J,I,LBOTM(K)-1)
      IF(TMP.LT.TOP) HD=TOP
  122 HDIFF=HD-HNEW(J,I,K-1)
      X5=HDIFF*CV(J,I,K-1)
C
C12-----CALCULATE FLOW THROUGH THE LOWER FACE.
  150 IF(K.EQ.NLAY) GO TO 180
      IF(IBOUND(J,I,K+1).EQ.0) GO TO 180
      IF(ICHFLG.EQ.0 .AND. IBOUND(J,I,K+1).LT.0) GO TO 180
      HD=HNEW(J,I,K+1)
      IF(LAYHDT(K+1).EQ.0) GO TO 152
      TMP=HD
      TOP=BOTM(J,I,LBOTM(K+1)-1)
      IF(TMP.LT.TOP) HD=TOP
  152 HDIFF=HNEW(J,I,K)-HD
      X6=HDIFF*CV(J,I,K)
C
C13-----SUM THE FLOWS THROUGH SIX FACES OF CONSTANT HEAD CELL
 180  RATE=X1+X2+X3+X4+X5+X6
C
C-----RETURN
      RETURN
      END
C=======================================================================
      SUBROUTINE SOBS1BAS6FFLWDR(J,I,K,ICHFLG,IBOUND,HNEW,CR,CC,CV,BOTM,
     &                          NBOTM,NCOL,NROW,NLAY,DRATE,PIDTMP,IIPP,
     &                          LAYHDT,RMLT,NMLTAR,IZON,NZONAR,DELC,
     &                          DELR,HFB,NHFB,IUHFB,MXACTFB,SV,VKA,HK,
     &                          HANI,SNEW,IERR,IERRU,IOUT,IULPF,IUHUF,
     &                          HKCC,HUFTHK,NHUF,GS)
C     ******************************************************************
C     CALCULATE CONSTANT-HEAD BOUNDARY FLOW SENSITIVITY FOR A GIVEN CELL
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION HNEW,HD,DX1,DX2,DX3,DX4,DX5,DX6,DRATE,SNEW
C
      CHARACTER*4 PIDTMP
      DIMENSION HNEW(NCOL,NROW,NLAY), IBOUND(NCOL,NROW,NLAY),
     1     CR(NCOL,NROW,NLAY), CC(NCOL,NROW,NLAY),
     2     CV(NCOL,NROW,NLAY), BOTM(NCOL,NROW,0:NBOTM),
     &     LAYHDT(NLAY),RMLT(NCOL,NROW,NMLTAR),IZON(NCOL,NROW,NZONAR),
     &     DELR(NCOL),DELC(NROW),SNEW(NCOL,NROW,NLAY),
     &     HFB(7,MXACTFB),SV(NCOL,NROW,NLAY),VKA(NCOL,NROW,NLAY),
     &     HK(NCOL,NROW,NLAY),HANI(NCOL,NROW,NLAY),
     &     HKCC(NCOL,NROW,NLAY),HUFTHK(NCOL,NROW,NHUF,2),GS(NCOL,NROW)
C
      COMMON /DISCOM/LBOTM(999),LAYCBD(999)
C     ------------------------------------------------------------------
  600 FORMAT(/,
     &' SENSITIVITIES FOR FLOW OBSERVATIONS AT BOUNDARIES REPRESENTED',
     &' AS CONSTANT-HEAD',/,
     &' ARE NOT SUPPORTED FOR SELECTED FLOW PACKAGE',
     &' -- STOP EXECUTION (SOBS1BAS6FFLWDR)')
C
C6------CLEAR VALUES FOR FLOW SENSITIVITIES THROUGH EACH FACE OF CELL.
      ZERO=0.
      DX1=ZERO
      DX2=ZERO
      DX3=ZERO
      DX4=ZERO
      DX5=ZERO
      DX6=ZERO
      COL=ZERO
      COR=ZERO
      COB=ZERO
      COF=ZERO
      COU=ZERO
      COD=ZERO
      IF(IULPF.GT.0) THEN
C-----GET CONDUCTANCE SENSITIVITIES FOR LPF PACKAGE
        IF (PIDTMP.EQ.'HANI' .OR. PIDTMP.EQ.'HK  ' .OR.
     &    PIDTMP.EQ.'VK  ' .OR. PIDTMP.EQ.'VANI' .OR.
     &    PIDTMP.EQ.'VKCB' .OR. PIDTMP.EQ.'SS  ' .OR.
     &    PIDTMP.EQ.'SY  ')
     &      CALL SOBS1BAS6FLPFCO(K,I,J,COL,COR,COB,COF,COU,COD,NCOL,
     &                           NROW,NLAY,PIDTMP,IIPP,LAYHDT,IBOUND,
     &                           RMLT,NMLTAR,IZON,NZONAR,BOTM,NBOTM,
     &                           HNEW,DELC,DELR,HFB,NHFB,IUHFB,MXACTFB,
     &                           CV,SV,VKA,HK,HANI,IOUT)
      ELSEIF(IUHUF.GT.0) THEN
C-----GET CONDUCTANCE SENSITIVITIES FOR HUF PACKAGE
        IF (PIDTMP.EQ.'HANI' .OR. PIDTMP.EQ.'HK  ' .OR.
     &      PIDTMP.EQ.'VK  ' .OR. PIDTMP.EQ.'VANI' .OR.
     &      PIDTMP.EQ.'KDEP')
     &        CALL SOBS1BAS6FHUFCO(K,I,J,COL,COR,COB,COF,COU,COD,
     &                             IIPP,HNEW,NCOL,NROW,NLAY,PIDTMP,HK,
     &                             HKCC,DELR,DELC,IBOUND,CV,BOTM,NBOTM,
     &                             HUFTHK,NHUF,IZON,NZONAR,RMLT,NMLTAR,
     &                             IUHFB,HFB,MXACTFB,NHFB,IOUT,GS)
      ELSE
C-----CONDUCTANCE SENSITIVITIES FOR ANY OTHER FLOW PACKAGE NOT SUPPORTED
        WRITE(IOUT,600)
        WRITE(IERRU,600)
        IERR = 1
        RETURN
      ENDIF
C
C7------CALCULATE FLOW SENSITIVITY THROUGH THE LEFT FACE.
C7------COMMENTS A-C APPEAR ONLY IN THE SECTION HEADED BY COMMENT 7,
C7------BUT THEY APPLY IN A SIMILAR MANNER TO SECTIONS 8-12.
C
C7A-----IF THERE IS NO FLOW TO CALCULATE THROUGH THIS FACE, THEN GO ON
C7A-----TO NEXT FACE.  NO FLOW OCCURS AT THE EDGE OF THE GRID, TO AN
C7A-----ADJACENT NO-FLOW CELL, OR TO AN ADJACENT CONSTANT-HEAD CELL
C7A-----WHEN ICHFLG IS 0.
      IF(J.EQ.1) GO TO 30
      IF(IBOUND(J-1,I,K).EQ.0) GO TO 30
      IF(ICHFLG.EQ.0 .AND. IBOUND(J-1,I,K).LT.0) GO TO 30
C
C7B-----CALCULATE FLOW SENSITIVITY THROUGH THIS FACE INTO THE ADJACENT
C       CELL.
      HDIFF=HNEW(J,I,K)-HNEW(J-1,I,K)
      DX1=COL*HDIFF+CR(J-1,I,K)*(SNEW(J,I,K)-SNEW(J-1,I,K))
C
C8------CALCULATE FLOW SENSITIVITY THROUGH THE RIGHT FACE.
   30 IF(J.EQ.NCOL) GO TO 60
      IF(IBOUND(J+1,I,K).EQ.0) GO TO 60
      IF(ICHFLG.EQ.0 .AND. IBOUND(J+1,I,K).LT.0) GO TO 60
      HDIFF=HNEW(J,I,K)-HNEW(J+1,I,K)
      DX2=COR*HDIFF+CR(J,I,K)*(SNEW(J,I,K)-SNEW(J+1,I,K))
C
C9------CALCULATE FLOW SENSITIVITY THROUGH THE BACK FACE.
   60 IF(I.EQ.1) GO TO 90
      IF (IBOUND(J,I-1,K).EQ.0) GO TO 90
      IF(ICHFLG.EQ.0 .AND. IBOUND(J,I-1,K).LT.0) GO TO 90
      HDIFF=HNEW(J,I,K)-HNEW(J,I-1,K)
      DX3=COB*HDIFF+CC(J,I-1,K)*(SNEW(J,I,K)-SNEW(J,I-1,K))
C
C10-----CALCULATE FLOW SENSITIVITY THROUGH THE FRONT FACE.
   90 IF(I.EQ.NROW) GO TO 120
      IF(IBOUND(J,I+1,K).EQ.0) GO TO 120
      IF(ICHFLG.EQ.0 .AND. IBOUND(J,I+1,K).LT.0) GO TO 120
      HDIFF=HNEW(J,I,K)-HNEW(J,I+1,K)
      DX4=COF*HDIFF+CC(J,I,K)*(SNEW(J,I,K)-SNEW(J,I+1,K))
C
C11-----CALCULATE FLOW SENSITIVITY THROUGH THE UPPER FACE.
  120 IF(K.EQ.1) GO TO 150
      IF (IBOUND(J,I,K-1).EQ.0) GO TO 150
      IF(ICHFLG.EQ.0 .AND. IBOUND(J,I,K-1).LT.0) GO TO 150
      HD=HNEW(J,I,K)
      DHD=SNEW(J,I,K)
      IF(LAYHDT(K).EQ.0) GO TO 122
      TMP=HD
      TOP=BOTM(J,I,LBOTM(K)-1)
      IF(TMP.LT.TOP) THEN
        HD=TOP
        DHD=0.
      ENDIF
  122 HDIFF=HD-HNEW(J,I,K-1)
      DX5=COU*HDIFF+CV(J,I,K-1)*(DHD-SNEW(J,I,K-1))
C
C12-----CALCULATE FLOW SENSITIVITY THROUGH THE LOWER FACE.
  150 IF(K.EQ.NLAY) GO TO 180
      IF(IBOUND(J,I,K+1).EQ.0) GO TO 180
      IF(ICHFLG.EQ.0 .AND. IBOUND(J,I,K+1).LT.0) GO TO 180
      HD=HNEW(J,I,K+1)
      IF(LAYHDT(K+1).EQ.0) GO TO 152
      TMP=HD
      DHD=SNEW(J,I,K+1)
      TOP=BOTM(J,I,LBOTM(K+1)-1)
      IF(TMP.LT.TOP) THEN
        HD=TOP
        DHD=0.
      ENDIF
  152 HDIFF=HNEW(J,I,K)-HD
      DX6=COD*HDIFF+CV(J,I,K)*(SNEW(J,I,K)-DHD)
C
C13-----SUM THE FLOW SENSITIVITIES THROUGH SIX FACES OF CONSTANT HEAD
C       CELL
 180  DRATE=DX1+DX2+DX3+DX4+DX5+DX6
C
C-----RETURN
      RETURN
      END
C=======================================================================
      SUBROUTINE SOBS1BAS6FLPFCO(KPT,IPT,JPT,COL,COR,COB,COF,COU,COD,
     &                           NCOL,NROW,NLAY,PIDTMP,IIPP,LAYHDT,
     &                           IBOUND,RMLT,NMLTAR,IZON,NZONAR,BOTM,
     &                           NBOTM,HNEW,DELC,DELR,HFB,NHFB,IUHFB,
     &                           MXACTFB,CV,SV,VKA,HK,HANI,IOUT)
C     VERSION 20020503
C     ******************************************************************
C     CALCULATE CONDUCTANCE DERIVATIVES FOR CONSTANT-HEAD BOUNDARY FLOW
C     SENSITIVITY FOR A GIVEN CELL -- (CURRENTLY SUPPORTED ONLY WHEN LPF
C     IS ACTIVE)
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION HNEW, HP
C
      CHARACTER*4 PIDTMP
      DIMENSION HNEW(NCOL,NROW,NLAY), IBOUND(NCOL,NROW,NLAY),
     &     CV(NCOL,NROW,NLAY), BOTM(NCOL,NROW,0:NBOTM),
     &     LAYHDT(NLAY),RMLT(NCOL,NROW,NMLTAR),IZON(NCOL,NROW,NZONAR),
     &     DELR(NCOL),DELC(NROW),
     &     HFB(7,MXACTFB),SV(NCOL,NROW,NLAY),VKA(NCOL,NROW,NLAY),
     &     HK(NCOL,NROW,NLAY),HANI(NCOL,NROW,NLAY),CO(6)
C
      COMMON /DISCOM/LBOTM(999),LAYCBD(999)
      COMMON /LPFCOM/LAYTYP(999),LAYAVG(999),CHANI(999),LAYVKA(999),
     1               LAYWET(999)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
      ZERO=0.
      DO 10 I=1,6
        CO(I)=ZERO
   10 CONTINUE
C
C       CALL SSEN1LPF1CH TO COMPUTE HORIZONTAL CONDUCTANCE SENSITIVITIES
C     ONLY COMPUTE CONDUCTANCE SENSITIVITIES IF NEEDED
      DO 140 ICL = IPLOC(1,IIPP),IPLOC(2,IIPP)
        IL = IPCLST(1,ICL)
        IF (IL.EQ.0) GOTO 140
        K = IL
        SF = 1.0
        LT = LAYHDT(KPT)
        LZ1 = IPCLST(3,ICL)
        M = IPCLST(2,ICL)
        IF (PIDTMP.NE.'VK  '.AND.PIDTMP.NE.'VANI'.AND.KPT.EQ.K) THEN
C-------CR--------------------------------------------------------------
          JCNT = 0
          DO 90 JJ = JPT-1, JPT
            JCNT = JCNT + 1
            IF (JJ.LT.1 .OR. JJ.GE.NCOL) GOTO 90
            IF (IBOUND(JJ,IPT,KPT).EQ.0 .OR.
     &          IBOUND(JJ+1,IPT,KPT).EQ.0) GOTO 90
            RMLT0 = 1.0
            IF (IL.GT.0 .AND. PIDTMP.NE.'ANI ') THEN
              IF (M.EQ.0) RMLT0 = SF
              IF (M.GT.0) RMLT0 = SF*RMLT(JJ,IPT,M)
              IF (RMLT0.NE.0.0 .AND. LZ1.GT.0) THEN
                IFLAG0 = 0
                DO 70 IZ = 5,IPCLST(4,ICL)
                  NZ = IPCLST(IZ,ICL)
                  IF (NZ.EQ.0 .OR. IFLAG0.EQ.1) GOTO 80
                  IF (IZON(JJ,IPT,LZ1).EQ.NZ) IFLAG0 = 1
   70           CONTINUE
   80           IF (IFLAG0.EQ.0) RMLT0 = 0.0
              ENDIF
            ENDIF
            TH1 = BOTM(JJ,IPT,LBOTM(KPT)-1)-BOTM(JJ,IPT,LBOTM(KPT))
            IF (LT.GT.0 .AND.
     &          HNEW(JJ,IPT,KPT).LT.BOTM(JJ,IPT,LBOTM(KPT)-1))
     &          TH1 = HNEW(JJ,IPT,KPT) - BOTM(JJ,IPT,LBOTM(KPT))
            CALL SSEN1LPF1CH(CO(JCNT),TH2,HP,IPT,JJ,KPT,'CR',IL,M,
     &                      RMLT0,RMLT,LZ1,IZON,SF,LT,HK,
     &                      NCOL,NROW,NLAY,DELC,DELR,HNEW,TH1,
     &                      BOTM(1,1,LBOTM(KPT)),
     &                      BOTM(1,1,LBOTM(KPT)-1),NMLTAR,NZONAR,
     &                      ICL,C,HANI)
            IF (IUHFB.GT.0 .AND. CO(JCNT).NE.0.)
     &          CALL SSEN1HFB6MD(C,'CR',CO(JCNT),DELC,DELR,HFB,IPT,
     &                           JJ,KPT,MXACTFB,NCOL,NHFB,NROW,
     &                           TH1,TH2)
   90     CONTINUE
C-------CC--------------------------------------------------------------
          ICNT = 2
          DO 120 II = IPT-1, IPT
            ICNT = ICNT + 1
            IF (II.LT.1 .OR. II.GE.NROW .OR.
     &          IBOUND(JPT,II+1,KPT).EQ.0) GOTO 120
            RMLT0 = 1.0
            IF (IL.GT.0 .AND. PIDTMP.NE.'ANI ') THEN
              IF (M.EQ.0) RMLT0 = SF
              IF (M.GT.0) RMLT0 = SF*RMLT(JPT,II,M)
              IF (RMLT0.NE.0.0 .AND. LZ1.GT.0) THEN
                IFLAG0 = 0
                DO 100 IZ = 5, IPCLST(4,ICL)
                  NZ = IPCLST(IZ,ICL)
                  IF (NZ.EQ.0 .OR. IFLAG0.EQ.1) GOTO 110
                  IF (IZON(JPT,II,LZ1).EQ.NZ) IFLAG0 = 1
  100           CONTINUE
  110           IF (IFLAG0.EQ.0) RMLT0 = 0.0
              ENDIF
            ENDIF
            TH1 = BOTM(JPT,II,LBOTM(KPT)-1)-BOTM(JPT,II,LBOTM(KPT))
            IF (LT.GT.0 .AND.
     &          HNEW(JPT,II,KPT).LT.BOTM(JPT,II,LBOTM(KPT)-1))
     &          TH1 = HNEW(JPT,II,KPT) - BOTM(JPT,II,LBOTM(KPT))
            IF (PIDTMP.EQ.'HK  ') THEN
              CALL SSEN1LPF1CH(CO(ICNT),TH2,HP,II,JPT,KPT,'CC',IL,M,
     &                        RMLT0,RMLT,LZ1,IZON,SF,LT,
     &                        HK,NCOL,NROW,NLAY,DELC,DELR,HNEW,TH1,
     &                        BOTM(1,1,LBOTM(KPT)),
     &                        BOTM(1,1,LBOTM(KPT)-1),NMLTAR,NZONAR,
     &                        ICL,C,HANI)
              IF (IUHFB.GT.0 .AND. CO(ICNT).NE.0.)
     &            CALL SSEN1HFB6MD(C,'CC',CO(ICNT),DELC,DELR,HFB,
     &                            II,JPT,KPT,MXACTFB,NCOL,
     &                            NHFB,NROW,TH1,TH2)
            ENDIF
  120     CONTINUE
        ENDIF
C-------CV--------------------------------------------------------------
        IF (PIDTMP.EQ.'VK  '. OR. PIDTMP.EQ.'VANI' .OR.
     &      (PIDTMP.EQ.'HK  ' .AND. LAYVKA(K).NE.0) .OR.
     &      PIDTMP.EQ.'VKCB') THEN
          DO 130 KK = KPT-1, KPT+1
            IF (KK.LT.1 .OR. KK.GT.NLAY) GOTO 130
            IF(KK.NE.IPCLST(1,ICL)) GOTO 130
            CALL SSEN1LPF1CV(COD,COU,IBP,IBM,PIDTMP,IL,SF,RMLT,M,
     &                       NCOL,NROW,LZ1,CV,SV,NLAY,DELR,
     &                       DELC,JPT,IPT,KK,HK,IZON,IBOUND,
     &                       NMLTAR,NZONAR,ICL,BOTM,NBOTM,VKA,HNEW)
            IF (KK.LT.KPT) CO(5) = CO(5) + COD
            IF (KK.EQ.KPT) THEN
              CO(5) = CO(5) + COU
              CO(6) = CO(6) + COD
            ENDIF
            IF (KK.GT.KPT) CO(6) = CO(6) + COU
  130     CONTINUE
        ENDIF
  140 CONTINUE
      COL=CO(1)
      COR=CO(2)
      COB=CO(3)
      COF=CO(4)
      COU=CO(5)
      COD=CO(6)
      RETURN
      END
C=======================================================================
      SUBROUTINE SOBS1BAS6FHUFCO(K,I,J,COL,COR,COB,COF,COU,COD,
     &                           IIPP,H,NCOL,NROW,NLAY,PID,HK,HKCC,
     &                           DELR,DELC,IBOUND,CV,BOTM,NBOTM,
     &                           HUFTHK,NHUF,IZON,NZONAR,RMLT,NMLTAR,
     &                           IUHFB,HFB,MXACTFB,NHFB,IOUT,GS)
C     ******************************************************************
C     CALCULATE CONDUCTANCE DERIVATIVES FOR CONSTANT-HEAD BOUNDARY FLOW
C     SENSITIVITY FOR A GIVEN CELL FOR HUF PACKAGE
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL DELC, DELR, RMLT, BOTM, HFB, HK, CV, CO
      INTEGER I, J, K, IIPP, IBOUND, IZON, NCOL, NHUF,
     &        NROW, NLAY, IUHFB, MXACTFB, NHFB, NMLTAR, NZONAR
      DOUBLE PRECISION H(NCOL,NROW,NLAY)
      DIMENSION IBOUND(NCOL,NROW,NLAY), DELR(NCOL), DELC(NROW),
     &          RMLT(NCOL,NROW,NMLTAR),
     &          HK(NCOL,NROW,NLAY), HKCC(NCOL,NROW,NLAY),
     &          CO(6), IZON(NCOL,NROW,NZONAR),
     &          CV(NCOL,NROW,NLAY),BOTM(NCOL,NROW,0:NBOTM),
     &          HFB(7,MXACTFB), HUFTHK(NCOL,NROW,NHUF,2),
     &          GS(NCOL,NROW)
      CHARACTER*4 PID
C
      COMMON /DISCOM/LBOTM(999),LAYCBD(999)
      COMMON /HUFCOM/HGUHANI(999),HGUVANI(999),LTHUF(999),LAYWT(999)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------

      DO 10 ID=1,6
        CO(ID) = 0.0
   10 CONTINUE
      CALL SSEN1HUF2CO(CO,0,I,J,K,IIPP,H,NCOL,NROW,NLAY,
     &                 PID,HK,HKCC,DELR,DELC,IBOUND,CV,BOTM,NBOTM,
     &                 HUFTHK,NHUF,IZON,NZONAR,RMLT,NMLTAR,
     &                 IUHFB,HFB,MXACTFB,NHFB,IOUT,GS)
      COL=CO(1)
      COR=CO(2)
      COB=CO(3)
      COF=CO(4)
      COU=CO(5)
      COD=CO(6)
      RETURN
      END
C=======================================================================
      SUBROUTINE SOBS1BAS6OR(D,R,IU,IPR,MPR,ND,NRSO,IPLPTR,IPLOT,NAMES)
C     VERSION 19981112 ERB
C     ******************************************************************
C     OUTPUT ORDERED RESIDUALS AND CORRESPONDING VALUES FROM NORMAL
C     DISTRIBUTION
C     ******************************************************************
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL D, R, RMIN, RNORM
      INTEGER IPR, N, N1, N2, ND, NMIN
      INTEGER IPLOT(ND+IPR+MPR), IPLPTR(ND+IPR+MPR)
      CHARACTER*12 NAMES(ND+IPR+MPR)
      DIMENSION D(NRSO), R(NRSO)
C     ------------------------------------------------------------------
  500 FORMAT (2(G15.7,1X),I5,2X,A)
C
C     ORDER THE RESIDUALS FOR VALID OBSERVATIONS AND PRIOR INFORMATION
      DO 20 N1 = 1, NRSO-1
        NMIN = N1
        RMIN = D(N1)
        DO 10 N2 = N1+1, NRSO
          IF (D(N2).LE.RMIN) THEN
            RMIN = D(N2)
            IPTRMIN = IPLPTR(N2)
            NMIN = N2
          ENDIF
   10   CONTINUE
        IF (NMIN.NE.N1) THEN
          D(NMIN) = D(N1)
          D(N1) = RMIN
          IPLPTR(NMIN) = IPLPTR(N1)
          IPLPTR(N1) = IPTRMIN
        ENDIF
   20 CONTINUE
C
      DO 30 N = 1, NRSO
        RNORM = (REAL(N)-.5)/(REAL(NRSO))
        CALL SOBS1BAS6UN(R(N),RNORM,-1)
   30 CONTINUE
C
      DO 40 N = 1, NRSO
        IPTR = IPLPTR(N)
        WRITE (IU,500) D(N), R(N), IPLOT(IPTR), NAMES(IPTR)
   40 CONTINUE
C
      RETURN
      END
C=======================================================================
      SUBROUTINE SOBS1BAS6UN(U,RNORM,IP)
C-----VERSION 19981117 ERB
C     ******************************************************************
C     FIND THE PROBABILITY RELATED TO A U (IP=1), OR A U RELATED TO A
C     PROBABILITY (IP=-1) FOR A STANDARD GAUSSIAN DISTRIBUTION
C     ******************************************************************
C        SPECIFICATIONS:
      REAL AU, RNORM, U
      DOUBLE PRECISION PNORM, ARNORM, FACTOR
      INTEGER I, IP
      DIMENSION PNORM(2,54)
C     ------------------------------------------------------------------
      DATA (PNORM(1,I),I=1,54)/0.0, .15, .20, .25, .30, .35, .40, .45,
     &      .50, .55, .60, .65, .70, .75, .80, .85, .90, .95, 1.00,
     &      1.05, 1.10, 1.15, 1.20, 1.25, 1.30, 1.35, 1.40, 1.45, 1.50,
     &      1.55, 1.60, 1.65, 1.70, 1.75, 1.80, 1.85, 1.90, 1.95, 2.0,
     &      2.1, 2.2, 2.3, 2.4, 2.5, 2.6, 2.7, 2.8, 2.9, 3.0, 3.5, 4.0,
     &      4.5, 5.0, 5.5/
      DATA (PNORM(2,I),I=1,54)/.5, .5596, .5793, .5987, .6179, .6368,
     &      .6554, .6736, .6915, .7088, .7257, .7422, .7580, .7734,
     &      .7881, .8023, .8159, .8289, .8413, .8531, .8643, .8749,
     &      .8849, .8944, .90320, .91149, .91924, .92647, .93319,
     &      .93943, .94520, .95053, .95543, .95994, .96407, .96784,
     &      .97128, .97441, .9772, .9821, .98610, .9893, .9918, .9938,
     &      .9953, .9965, .9974, .9981, .9987, .99976737,  .99996833,
     &      .99999660, .99999971, 1.0/
C     ------------------------------------------------------------------
C
C-----GIVEN U, GET THE CUMULATIVE PROBABILITY
      IF (IP.EQ.1) THEN
C-------FIND THE VALUES ABOVE AND BELOW U
        AU = ABS(U)
        IF (AU.GE.5.5) THEN
          RNORM = 1.0
          IF (U.LT.0.0) RNORM = 0.0
        ELSE
          DO 10 I = 1, 53
            IF (AU.GE.PNORM(1,I) .AND. AU.LT.PNORM(1,I+1)) THEN
C-------INTERPOLATE
              FACTOR = (AU-PNORM(1,I))/(PNORM(1,I+1)-PNORM(1,I))
              RNORM = PNORM(2,I) + FACTOR*(PNORM(2,I+1)-PNORM(2,I))
              IF (U.LT.0) RNORM = 1.0 - RNORM
              GOTO 30
            ENDIF
   10     CONTINUE
          CALL USTOP('ERROR IN SOBS1BAS6UN -- U NOT FOUND')
        ENDIF
C-----GIVEN THE CUMULATIVE PROBABILITY, GET U
      ELSEIF (IP.EQ.-1) THEN
C-------FIND THE VALUES ABOVE AND BELOW RNORM
        ARNORM = RNORM
        IF (RNORM.LT..50) ARNORM = 1. - RNORM
        IF (ARNORM.EQ.1.0) THEN
          U = 5.5
          IF (RNORM.LT..5) U = -5.5
        ELSE
          DO 20 I = 1, 53
            IF (ARNORM.GE.PNORM(2,I) .AND. ARNORM.LT.PNORM(2,I+1)) THEN
C-------INTERPOLATE
              FACTOR = (ARNORM-PNORM(2,I))/(PNORM(2,I+1)-PNORM(2,I))
              U = PNORM(1,I) + FACTOR*(PNORM(1,I+1)-PNORM(1,I))
              IF (RNORM.LT..50) U = -U
              GOTO 30
            ENDIF
   20     CONTINUE
          CALL USTOP('ERROR IN SOBS1BAS6UN -- RNORM NOT FOUND')
        ENDIF
      ENDIF
   30 RETURN
      END
C=======================================================================
      SUBROUTINE SOBS1BAS6RS(AVET,NRSO,NPOST,NNEGT,NRUNS,IOUT)
C-----VERSION 20010215 ERB
C     ******************************************************************
C     CALCULATE AND PRINT THE RUNS STATISTIC FOR ALL RESIDUALS
C     ******************************************************************
C        SPECIFICATIONS:
      REAL AVET
      INTEGER IOUT, NNEGT, NPOST, NRSO, NRUNS
C     ------------------------------------------------------------------
  510 FORMAT (/,' STATISTICS FOR ALL RESIDUALS :',/,
     &        ' AVERAGE WEIGHTED RESIDUAL  :',E10.3,/,
     &        ' # RESIDUALS >= 0. :',I7,/,' # RESIDUALS < 0.  :',I7,/,
     &        ' NUMBER OF RUNS  :',I5,'  IN',I5,' OBSERVATIONS')
  515 FORMAT (/,' INTERPRETING THE CALCULATED RUNS STATISTIC VALUE OF '
     &        ,G13.3,/,' NOTE: THE FOLLOWING APPLIES ONLY IF ',/,
     &        '        # RESIDUALS >= 0 . IS GREATER THAN 10 AND ',/,
     &        '        # RESIDUALS < 0.   IS GREATER THAN 10',/,
     &        ' THE NEGATIVE VALUE MAY INDICATE TOO FEW RUNS:',/,
     &        '    IF THE VALUE IS LESS THAN -1.28,',
     &        ' THERE IS LESS THAN A 10 PERCENT',/,
     &        7X,'CHANCE THE VALUES ARE RANDOM,',/,
     &        '    IF THE VALUE IS LESS THAN -1.645,',
     &        ' THERE IS LESS THAN A 5 PERCENT',/,
     &        7X,'CHANCE THE VALUES ARE RANDOM,',/,
     &        '    IF THE VALUE IS LESS THAN -1.96,',
     &        ' THERE IS LESS THAN A 2.5 PERCENT',/,
     &        7X,'CHANCE THE VALUES ARE RANDOM.',/)
  520 FORMAT (/,' INTERPRETING THE CALCULATED RUNS STATISTIC VALUE OF '
     &        ,G13.3,/,' NOTE: THE FOLLOWING APPLIES ONLY IF ',/,
     &        '        # RESIDUALS >= 0 . IS GREATER THAN 10 AND ',/,
     &        '        # RESIDUALS < 0.   IS GREATER THAN 10',/,
     &        ' THE POSITIVE VALUE MAY INDICATE TOO MANY RUNS:',/,
     &        '    IF THE VALUE IS GREATER THAN 1.28,',
     &        ' THERE IS LESS THAN A 10 PERCENT',/,
     &        7X,'CHANCE THE VALUES ARE RANDOM,',/,
     &        '    IF THE VALUE IS GREATER THAN 1.645,',
     &        ' THERE IS LESS THAN A 5 PERCENT',/,
     &        7X,'CHANCE THE VALUES ARE RANDOM,',/,
     &        '    IF THE VALUE IS GREATER THAN 1.96,',
     &        ' THERE IS LESS THAN A 2.5 PERCENT',/,
     &        7X,'CHANCE THE VALUES ARE RANDOM.',/)
  525 FORMAT (/,' STATISTICS FOR ALL RESIDUALS :',/,
     &        ' AVERAGE WEIGHTED RESIDUAL  :',E10.3,/,
     &        ' # RESIDUALS >= 0. :',I7,/,' # RESIDUALS < 0.  :',I7,/,
     &        ' NUMBER OF RUNS  :',I5,'  IN',I5,' OBSERVATIONS',/,
     &         /,' COULD NOT CALCULATE THE RUNS STATISTIC')
  530 FORMAT (/,
     &       ' THE NUMBER OF RUNS EQUALS THE EXPECTED NUMBER OF RUNS')
C
      RP = REAL(NPOST)
      RN = REAL(NNEGT)
      RNP = 2.*RP*RN
      RNS = RP + RN
      RNR = REAL(NRUNS)
      IF (RNP.GT.0.0 .AND. RNP.GT.RNS) THEN
        ERUNS = (RNP/RNS) + 1.0
        SDRUNS = ((RNP*(RNP-RNS))/((RNS**2.)*(RNS-1.)))**.5
        STRUNS = (RNR-ERUNS+.5)/SDRUNS
        ST2RNS = (RNR-ERUNS-.5)/SDRUNS
        WRITE (IOUT,510) AVET/REAL(NRSO), NPOST, NNEGT, NRUNS, NRSO
        IF (ERUNS-RNR.LT.1.E-30) THEN
          WRITE(IOUT,530)
        ELSE
          IF (STRUNS.LT.0.0) WRITE (IOUT,515) STRUNS
          IF (ST2RNS.GT.0.0) WRITE (IOUT,520) ST2RNS
        ENDIF
      ELSE
        WRITE (IOUT,525) AVET/REAL(NRSO), NPOST, NNEGT, NRUNS, NRSO
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE SOBS1BAS6WCM(IOUT,IWTF,ND,NDMH,NH,WT,WTQ)
C-----VERSION 20030715 ERB
C     ******************************************************************
C     PRINT THE WT AND WTQ ARRAYS AS A COMPRESSED MATRIX
C     ******************************************************************
C        SPECIFICATIONS:
      INTEGER IWTF, NDMH, NH
      REAL WT(NH), WTQ(NDMH,NDMH)
C     ------------------------------------------------------------------
   50 FORMAT(/,1X,
     &    '*** WARNING: PROGRAMMER ERROR -- ND NOT EQUAL TO NH+NDMH',
     &    ' IN SOBS1BAS6WCM:',/,
     &    1X,'  ND = ',I10,/,
     &    1X,'  NH = ',I10,/,
     &    1X,'NDMH = ',I10,/,
     &    1X,'_wt FILE NOT CORRECTLY PRODUCED ***')
  100 FORMAT(3(I15))
  150 FORMAT(10(3X,A))
  200 FORMAT(I20,5X,G15.7)
C
      IF (ND.NE.NH+NDMH) THEN
        WRITE(IOUT,50)
        RETURN
      ENDIF
C
C     DETERMINE NUMBER OF NON-ZERO ENTRIES
      NNZ = NH
      DO J=1,NDMH
        DO I=1,NDMH
          IF (WTQ(I,J).NE.0.0) NNZ = NNZ+1
        ENDDO
      ENDDO
C
C     TRUE MATRIX WOULD HAVE ND ROWS AND COLUMNS
      WRITE(IWTF,100) NNZ,ND,ND
C
C     INDX IS POSITION IN TRUE MATRIX OF EACH NON-ZERO VALUE, ASSUMING
C     COLUMN-MAJOR STORAGE ORDER
      IF (NH.GT.0) THEN
C       WRITE INDEX AND WEIGHT FOR EACH HEAD OBSERVATION
        DO I=1,NH
          INDX = 1+(I-1)*(ND+1)
          WRITE(IWTF,200) INDX,WT(I)
        ENDDO
      ENDIF
      INDX1 = NH*ND
      IF (NDMH.GT.0) THEN
C       WRITE INDEX AND WEIGHT FOR EACH NON-HEAD OBSERVATION
        DO J=1,NDMH
          DO I=1,NDMH
            IF (WTQ(I,J).NE.0.0) THEN
              INDX = INDX1+(J-1)*ND+NH+I
              WRITE(IWTF,200) INDX,WTQ(I,J)
            ENDIF
          ENDDO
        ENDDO
      ENDIF
C
      RETURN
      END
