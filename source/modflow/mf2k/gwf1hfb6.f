C     Last change:  ERB  22 Oct 2002    2:06 pm
      SUBROUTINE GWF1HFB6ALP(INHFB,IOUT,ISUM,LCHFB,MXACTFB,NHFBNP,
     &                   NPHFB,MXHFB,IHFB,NOPRHB)
C
C-----VERSION 11JAN2000 GWF1HFB6ALP
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR PARAMETERIZED HORIZONTAL FLOW BARRIER
C     PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER INHFB, IOUT, ISUM, LCHFB, MXACTFB, MXFBP, NHFBNP,
     &        NPHFB
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
C
C1------IDENTIFY PACKAGE.
      WRITE(IOUT,1) INHFB
    1 FORMAT(1X,'HFB6 -- HORIZONTAL FLOW BARRIER',
     &' PACKAGE, VERSION 6, 1/11/1000.',/,'   INPUT READ FROM UNIT ',I4)
C
C2------READ AND PRINT NPHFB, MXFB, NHFBNP
      CALL URDCOM(INHFB,IOUT,LINE)
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPHFB,DUM,IOUT,INHFB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXFBP,DUM,IOUT,INHFB)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NHFBNP,DUM,IOUT,INHFB)
      WRITE(IOUT,500) NPHFB,MXFBP
  500 FORMAT(1X,I5,' PARAMETERS DEFINE A MAXIMUM OF ',I6,
     &       ' HORIZONTAL FLOW BARRIERS')
      WRITE(IOUT,530) NHFBNP
  530 FORMAT(1X,I6,' HORIZONTAL FLOW BARRIERS NOT DEFINED BY',
     &       ' PARAMETERS')
C
C3------SET LCHFB EQUAL TO ADDRESS OF FIRST UNUSED SPACE IN RX.
      LCHFB = ISUM
C-------READ OPTION.
      NOPRHB = 0
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,INHFB)
      IF(LINE(ISTART:ISTOP).EQ.'NOPRINT') THEN
        WRITE(IOUT,3)
    3   FORMAT(1X,
     &'LISTS OF HORIZONTAL FLOW BARRIER CELLS WILL NOT BE PRINTED')
        NOPRHB = 1
      END IF
C
C4------CALCULATE AMOUNT OF SPACE USED BY HFB PACKAGE.
      MXACTFB = NHFBNP+MXFBP
      IHFB = MXACTFB + 1
      MXHFB = MXACTFB + MXFBP
      ISP = 7*MXHFB
      ISUM = ISUM+ISP
C
C5------PRINT AMOUNT OF SPACE USED BY HFB PACKAGE.
      WRITE(IOUT,4)ISP
    4 FORMAT(1X,I10,' ELEMENTS IN RX ARRAY ARE USED FOR',
     &/,10X,' HORIZONTAL FLOW BARRIER PACKAGE')
C
C6------RETURN
      RETURN
      END
C=======================================================================
      SUBROUTINE GWF1HFB6RPPA(BOTM,CR,CC,DELR,DELC,HFB,INHFB,MXACTFB,
     &                        NBOTM,NCOL,NROW,NLAY,NODES,NHFBNP,NHFB,
     &                        NPHFB,IOUT,IOUTG,ITERP,MXHFB,IHFB,LAYHDT,
     &                        INAMLOC,NOPRHB)
C
C-----VERSION 11JAN2000 GWF1HFB6RPPA
C     ******************************************************************
C     READ AND INITIALIZE DATA FOR PARAMETERIZED HORIZONTAL FLOW BARRIER
C     PACKAGE.  READ PARAMETER DEFINITIONS AND NON-PARAMETER BARRIERS
C     ******************************************************************
C
C       SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION BOTM(NCOL,NROW,0:NBOTM), CC(NODES), CR(NODES),
     &          DELC(NROW), DELR(NCOL), HFB(7,MXHFB), LAYHDT(NLAY)
      COMMON /DISCOM/LBOTM(999),LAYCBD(999)
      CHARACTER*16 AUX(1)
C     ------------------------------------------------------------------
  500 FORMAT(//,1X,I5,' HFB parameters')
  510 FORMAT(/,1X,I6,' BARRIERS NOT DEFINED BY PARAMETERS')
  520 FORMAT(1X,'BARRIER  LAYER  IROW1  ICOL1  IROW2  ICOL2',
     &       '    HYDCHR')
  530 FORMAT(1X,I6,5(2X,I5),2X,G12.5,2X,A)
  540 FORMAT(/,1X,1I6,' HFB BARRIERS')
  550 FORMAT(/,
     &' ERROR: SELECTED FLOW PACKAGE DOES NOT SUPPORT HFB PACKAGE',/,
     &' -- STOP EXECUTION (GWF1HFB6RPPA)')
C
      IF (LAYHDT(1).LT.0) THEN
        WRITE(IOUT,550)
        CALL USTOP(' ')
      ENDIF
C
      ITERPU = ITERP
      IOUTU = IOUT
      IF (NOPRHB.EQ.1) THEN
        ITERPU = 99
        IOUTU = -IOUT
      ENDIF
C
C
C     READ PARAMETER DEFINITIONS (ITEMS 2 AND 3)
      IF (ITERP.EQ.1) WRITE(IOUTG,500) NPHFB
      IF (NPHFB.GT.0) THEN
        LSTSUM = IHFB
        DO 20 K = 1,NPHFB
          LSTBEG = LSTSUM
          CALL UPARLSTRP(LSTSUM,MXHFB,INHFB,IOUTG,IP,'HFB ','HFB ',
     &                   ITERP,NUMINST,INAMLOC)
          NLST=LSTSUM-LSTBEG
          CALL SGWF1HFB6RL(NLST,HFB,LSTBEG,7,MXHFB,INHFB,IOUTG,
     &         'BARRIER  LAYER  IROW1  ICOL1  IROW2  ICOL2     FACTOR',
     &         NCOL,NROW,NLAY,6,6,ITERPU)
          CALL SGWF1HFB6CK(HFB,MXHFB,LSTBEG,LSTSUM-1,IOUTG)
   20   CONTINUE
      ENDIF
C
      NHFB = 0
C     READ BARRIERS NOT DEFINED BY PARAMETERS (ITEM 4)
      WRITE(IOUT,510) NHFBNP
      IF (NHFBNP.GT.0) THEN
        NHFB = NHFB + NHFBNP
        WRITE(IOUT,520)
        DO 30 I = 1,NHFBNP
          READ(INHFB,*)LAYER,IROW1,ICOL1,IROW2,ICOL2,HYDCHR
          IF (NOPRHB.EQ.0)
     &        WRITE(IOUT,530)I,LAYER,IROW1,ICOL1,IROW2,ICOL2,HYDCHR
          HFB(1,I) = LAYER
          HFB(2,I) = IROW1
          HFB(3,I) = ICOL1
          HFB(4,I) = IROW2
          HFB(5,I) = ICOL2
          HFB(6,I) = HYDCHR
          HFB(7,I) = 0.0
   30   CONTINUE
        CALL SGWF1HFB6CK(HFB,MXHFB,1,NHFBNP,IOUT)
      ENDIF
C
C-----SUBSTITUTE DATA FOR PARAMETERIZED BARRIERS INTO ACTIVE SECTION OF
C-----HFB ARRAY
      CALL PRESET('HFB ')
      IF (NPHFB.GT.0) THEN
C       READ NUMBER OF ACTIVE HFB PARAMETERS (ITEM 5)
        READ(INHFB,*) NACTHFB
        IF (NACTHFB.GT.0) THEN
          DO 50 I = 1,NACTHFB
C           READ AND ACTIVATE AN HFB PARAMETER (ITEM 6)
            NAUX=0
            CALL UPARLSTSUB(INHFB,'HFB ',IOUTU,'HFB ',HFB,7,MXHFB,6,
     &                      MXACTFB,NHFB,6,6,
     &'BARRIER  LAYER  IROW1  ICOL1      IROW2           ICOL2         H
     &YDR. CHAR.',AUX,1,NAUX)
   50     CONTINUE
        ENDIF
      ENDIF
C
C3----CHECK HFB CELL LOCATIONS.  MODIFY HORIZONTAL BRANCH CONDUCTANCES
C3----FOR CONSTANT T LAYERS.
      CALL SGWF1HFB6MC(BOTM,CR,CC,DELR,DELC,HFB,MXHFB,NBOTM,NCOL,NROW,
     &              NLAY,1,NHFB,IOUT,LAYHDT)
C
      WRITE (IOUT,540) NHFB
C4----RETURN
      RETURN
      END
C=======================================================================
      SUBROUTINE GWF1HFB6FM(BOTM,CC,CR,DELC,DELR,HFB,HNEW,MXACTFB,
     &                   NBOTM,NCOL,NHFB,NLAY,NROW,LAYHDT)
C
C-----VERSION 11JAN2000 GWF1HFB6FM
C     ******************************************************************
C     MODIFY HORIZONTAL BRANCH CONDUCTANCES IN VARIABLE-TRANSMISSIVITY
C     LAYERS TO ACCOUNT FOR HORIZONTAL FLOW BARRIERS (PARAMETERIZED
C     HORIZONTAL FLOW BARRIER PACKAGE).  STORE UNMODIFIED HORIZONTAL
C     CONDUCTANCES IN HFB(7,#) TO ALLOW CALCULATION OF SENSITIVITIES.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION HNEW
      DIMENSION BOTM(NCOL,NROW,0:NBOTM), CC(NCOL,NROW,NLAY),
     &          CR(NCOL,NROW,NLAY), DELC(NROW), DELR(NCOL),
     &          HFB(7,MXACTFB), HNEW(NCOL,NROW,NLAY), LAYHDT(NLAY)
C
      COMMON /DISCOM/LBOTM(999),LAYCBD(999)
C     ------------------------------------------------------------------
C1----FOR EACH BARRIER, MODIFY HORIZONTAL BRANCH
C1----CONDUCTANCES IF LAYER IS CONVERTIBLE
      DO 10 II=1,NHFB
        K = HFB(1,II)
C       IF LAYHDT=0, THICKNESS AND CONDUCTANCE DO NOT VARY, AND
C       MODIFICATION OF CONDUCTANCE DUE TO BARRIER WAS DONE IN
C       SGWF1HFB6N
        IF (LAYHDT(K).GT.0) THEN
C2--------CELL (J1,I1,K) IS THE ONE WHOSE HORIZONTAL BRANCH
C3--------CONDUCTANCES ARE TO BE MODIFIED.
          I1 = HFB(2,II)
          J1 = HFB(3,II)
C4--------CELL (J2,I2,K) IS THE CELL NEXT TO CELL (J1,I1,K) AND
C4--------SEPARATED FROM IT BY THE BARRIER.
          I2 = HFB(4,II)
          J2 = HFB(5,II)
          HCDW = HFB(6,II)
C
C5--------IF I1=I2, MODIFY HORIZONTAL BRANCH CONDUCTANCES ALONG ROW
C5--------DIRECTION.
          IF (I1.EQ.I2) THEN
C
C6----------IF CR(J1,I1,K) NOT = 0, CELLS ON EITHER SIDE OF BARRIER ARE
C6----------ACTIVE
            IF (CR(J1,I1,K).NE.0.) THEN
C
C7------------CALCULATE AVERAGE SATURATED THICKNESS BETWEEN CELLS
C7------------(J1,I1,K) AND (J2,I2,K).  NOTE: NEGATIVE SATURATED
C7------------THICKNESS DOES NOT OCCUR; OTHERWISE, CR(J1,I1,K) WOULD BE
C7------------ZERO AND THE FOLLOWING CALCULATION FOR SATURATED THICKNESS
C7------------WOULD BE SKIPPED.
              HD1 = HNEW(J1,I1,K)
              HD2 = HNEW(J2,I2,K)
              IF (HD1.GT.BOTM(J1,I1,LBOTM(K)-1)) HD1 =
     &                                           BOTM(J1,I1,LBOTM(K)-1)
              IF (HD2.GT.BOTM(J2,I2,LBOTM(K)-1)) HD2 =
     &                                           BOTM(J2,I2,LBOTM(K)-1)
              THKAVG = ((HD1-BOTM(J1,I1,LBOTM(K))) +
     &                 (HD2-BOTM(J2,I2,LBOTM(K))))/2.
C
C             STORE UNMODIFIED CR FOR CALCULATING SENSITIVITIES
              HFB(7,II) = CR(J1,I1,K)
C8------------MODIFY CR(J1,I1,K) TO ACCOUNT FOR BARRIER.
              TDW = THKAVG*HCDW
              CR(J1,I1,K) = TDW*CR(J1,I1,K)*DELC(I1)/
     &                      (TDW*DELC(I1)+CR(J1,I1,K))
            ENDIF
C
C9--------CASE OF J1=J2. MODIFY HORIZONTAL BRANCH CONDUCTANCES ALONG
C9--------COLUMN DIRECTION.
          ELSE
C10---------IF CC(J1,I1,K) NOT = 0, CELLS ON EITHER SIDE OF BARRIER ARE
C10---------ACTIVE
            IF (CC(J1,I1,K).NE.0.) THEN
C
C11-----------CALCULATE AVERAGE SATURATED THICKNESS BETWEEN CELLS
C11-----------(J1,I1,K) AND (J2,I2,K).  NEGATIVE SATURATED THICKNESS
C11-----------DOES NOT OCCUR FOR THE SAME REASON AS DESCRIBED ABOVE.
              HD1 = HNEW(J1,I1,K)
              HD2 = HNEW(J2,I2,K)
              IF (HD1.GT.BOTM(J1,I1,LBOTM(K)-1)) HD1 =
     &                                           BOTM(J1,I1,LBOTM(K)-1)
              IF (HD2.GT.BOTM(J2,I2,LBOTM(K)-1)) HD2 =
     &                                           BOTM(J2,I2,LBOTM(K)-1)
              THKAVG = ((HD1-BOTM(J1,I1,LBOTM(K))) +
     &                 (HD2-BOTM(J2,I2,LBOTM(K))))/2.
C
C             STORE UNMODIFIED CC FOR CALCULATING SENSITIVITIES
              HFB(7,II) = CC(J1,I1,K)
C12-----------MODIFY CC(J1,I1,K) TO ACCOUNT FOR BARRIER.
              TDW = THKAVG*HCDW
              CC(J1,I1,K) = TDW*CC(J1,I1,K)*DELR(J1)/
     &                      (TDW*DELR(J1)+CC(J1,I1,K))
            ENDIF
          ENDIF
        ENDIF
   10 CONTINUE
C
C13---RETURN
      RETURN
      END
C=======================================================================
      SUBROUTINE SGWF1HFB6MC(BOTM,CR,CC,DELR,DELC,HFB,MXHFB,NBOTM,NCOL,
     &                    NROW,NLAY,IB1,IB2,IOUT,LAYHDT)
C
C-----VERSION 11JAN2000 SGWF1HFB6MC
C     ******************************************************************
C     MODIFY HORIZONTAL CONDUCTANCES (CR AND CC) FOR CONFINED LAYERS TO
C     ACCOUNT FOR HORIZONTAL FLOW BARRIERS  (PARAMETERIZED HORIZONTAL
C     FLOW BARRIER PACKAGE).  STORE UNMODIFIED HORIZONTAL CONDUCTANCES
C     IN HFB(7,#) TO ALLOW CALCULATION OF SENSITIVITIES.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION BOTM(NCOL,NROW,0:NBOTM), CR(NCOL,NROW,NLAY),
     &          CC(NCOL,NROW,NLAY), DELR(NCOL), DELC(NROW),
     &          HFB(7,MXHFB), LAYHDT(NLAY)
      COMMON /DISCOM/LBOTM(999),LAYCBD(999)
C     ------------------------------------------------------------------
C
C1------INITIALIZE ERROR FLAG TO ZERO.
      IERFLG=0
C
C2----DO FOR EACH BARRIER IN RANGE.
      DO 10 II = IB1,IB2
        K = HFB(1,II)
C
C3------FIND ROW AND COLUMN NUMBERS OF THE TWO CELLS ON BOTH SIDES
C3------OF THE BARRIER.
        I1 = HFB(2,II)
        J1 = HFB(3,II)
        I2 = HFB(4,II)
        J2 = HFB(5,II)
        TH0 = BOTM(J1,I1,LBOTM(K)-1) - BOTM(J1,I1,LBOTM(K))
        TH1 = BOTM(J2,I2,LBOTM(K)-1) - BOTM(J2,I2,LBOTM(K))
        THKAVG = (TH0+TH1)/2.0
        TDW = THKAVG*HFB(6,II)
C
C4------IF I1=I2, BARRIER IS BETWEEN TWO CELLS ON THE SAME ROW.
        IF (I1.EQ.I2) THEN
C5--------IF J2-J1=1, THE TWO CELLS ARE NEXT TO ONE ANOTHER (DATA OK).
C5--------OTHERWISE, PRINT ERROR MESSAGE AND SET ERROR FLAG TO 1.
          IF ((J2-J1).EQ.1) THEN
C           BARRIER CELLS ARE ADJACENT
C6----------IF LAYER IS CONFINED AND BOTH CELLS ARE ACTIVE, SAVE
C6----------ORIGINAL CR FOR COMPUTING SENSITIVITIES AND MODIFY CR
            IF (LAYHDT(K).EQ.0) THEN
C7------------IF CR(J1,I1,K) NOT 0, BOTH CELLS ARE ACTIVE.
              IF (CR(J1,I1,K).NE.0.) THEN
                HFB(7,II) = CR(J1,I1,K)
C8--------------MODIFY CR(J1,I1,K) TO ACCOUNT FOR BARRIER.
                CR(J1,I1,K) = TDW*CR(J1,I1,K)*DELC(I1)/
     &                        (TDW*DELC(I1)+CR(J1,I1,K))
              ENDIF
            ENDIF
            GOTO 10
          ENDIF
C
C9------IF J1=J2, BARRIER IS BETWEEN TWO CELLS ON THE SAME COLUMN.
        ELSEIF (J1.EQ.J2) THEN
C10-------IF I2-I1=1, THE TWO CELLS ARE NEXT TO ONE ANOTHER (DATA OK).
C10-------OTHERWISE, PRINT ERROR MESSAGE AND SET ERROR FLAG TO 1.
          IF ((I2-I1).EQ.1) THEN
C           BARRIER CELLS ARE ADJACENT
C11---------IF LAYER IS CONFINED AND BOTH CELLS ARE ACTIVE, SAVE
C11---------ORIGINAL CC FOR COMPUTING SENSITIVITIES AND MODIFY CC
            IF (LAYHDT(K).EQ.0) THEN
C12-----------IF CC(J1,I1,K) NOT 0, BOTH CELLS ARE ACTIVE.
              IF (CC(J1,I1,K).NE.0.) THEN
                HFB(7,II) = CC(J1,I1,K)
C13-------------MODIFY CC(J1,I1,K) TO ACCOUNT FOR BARRIER
                CC(J1,I1,K) = TDW*CC(J1,I1,K)*DELR(J1)/
     &                        (TDW*DELR(J1)+CC(J1,I1,K))
              ENDIF
            ENDIF
            GOTO 10
          ENDIF
        ENDIF
C
C14-----CELLS ARE NOT ADJACENT. PRINT ERROR MESSAGE AND SET ERROR FLAG.
   80   WRITE (IOUT,1) II
    1   FORMAT (1X,'ERROR DETECTED IN LOCATION DATA OF BARRIER NO. ',I6)
        IERFLG=1
C
   10 CONTINUE
C
C15---HALT EXECUTION IF ERRORS ARE DETECTED.
      IF (IERFLG.EQ.1) CALL USTOP(' ')
C
C16---RETURN
      RETURN
      END
C=======================================================================
      SUBROUTINE SGWF1HFB6CK(HFB,MXHFB,IB1,IB2,IOUT)
C
C-----VERSION 11JAN2000 SGWF1HFB6CK
C     ******************************************************************
C     CHECK HFB CELL LOCATIONS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION HFB(7,MXHFB)
C     ------------------------------------------------------------------
C
C1----INITIALIZE ERROR FLAG TO ZERO.
      IERFLG=0
C
C2----CHECK EACH BARRIER IN RANGE.
      DO 10 II = IB1,IB2
C3------FIND ROW AND COLUMN NUMBERS OF THE TWO CELLS ON BOTH SIDES
C3------OF THE BARRIER AND REARRANGE HFB ARRAY.
        I1 = MIN(HFB(2,II),HFB(4,II))
        J1 = MIN(HFB(3,II),HFB(5,II))
        I2 = MAX(HFB(2,II),HFB(4,II))
        J2 = MAX(HFB(3,II),HFB(5,II))
        HFB(2,II) = I1
        HFB(3,II) = J1
        HFB(4,II) = I2
        HFB(5,II) = J2
C
        ID = I2 - I1
        JD = J2 - J1
        IF (ID.LT.0 .OR. ID.GT.1 .OR. JD.LT.0 .OR. JD.GT.1 .OR.
     &      ID.EQ.JD) THEN
C14-------CELLS ARE NOT ADJACENT. PRINT ERROR MESSAGE AND SET ERROR FLAG.
   80     WRITE (IOUT,1) II
    1     FORMAT (1X,'ERROR DETECTED IN LOCATION DATA OF BARRIER NO. ',
     &            I6)
          IERFLG=1
        ENDIF
C
   10 CONTINUE
C
C15---HALT EXECUTION IF ERRORS ARE DETECTED.
      IF (IERFLG.EQ.1) CALL USTOP(' ')
C
C16---RETURN
      RETURN
      END
C=======================================================================
      SUBROUTINE SGWF1HFB6RL(NLIST,HFB,LSTBEG,LDIM,MXHFB,INPACK,
     &                       IOUT,LABEL,NCOL,NROW,NLAY,ISCLOC1,
     &                       ISCLOC2,ITERP)
C
C-----VERSION 09SEP2002 SGWF1HFB6RL
C     ******************************************************************
C     Read and print a list of parameter-controlled HFB barriers.
C     ******************************************************************
      CHARACTER*(*) LABEL
      DIMENSION HFB(LDIM,MXHFB)
      CHARACTER*200 LINE,FNAME
      CHARACTER*120 BUF
      CHARACTER*1 DASH(120)
      DATA DASH/120*'-'/
      DATA NUNOPN/99/
      INCLUDE 'param.inc'
      INCLUDE 'openspec.inc'
C
C  Check for and decode EXTERNAL and SFAC records.
      IN = INPACK
      ICLOSE = 0
      READ(IN,'(A)') LINE
      SFAC = 1.
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'EXTERNAL') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,R,IOUT,IN)
         IN = I
         IF (ITERP.EQ.1) WRITE(IOUT,111) IN
  111    FORMAT(1X,'Reading list on unit ',I4)
         READ(IN,'(A)') LINE
      ELSE IF(LINE(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
         FNAME = LINE(ISTART:ISTOP)
         IN = NUNOPN
         IF (ITERP.EQ.1) WRITE(IOUT,115) IN,FNAME
  115    FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
         OPEN(UNIT=IN,FILE=FNAME,ACTION=ACTION(1))
C         OPEN(UNIT=IN,FILE=FNAME)
         ICLOSE = 1
         READ(IN,'(A)') LINE
      END IF
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'SFAC') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,SFAC,IOUT,IN)
         IF (ITERP.EQ.1) THEN
            WRITE(IOUT,116) SFAC
  116       FORMAT(1X,'LIST SCALING FACTOR= ',1PG12.5)
            IF(ISCLOC1.EQ.ISCLOC2) THEN
               WRITE(IOUT,113) ISCLOC1
  113          FORMAT(1X,'(THE SCALE FACTOR WAS APPLIED TO FIELD ',I2,
     &                ')')
            ELSE
               WRITE(IOUT,114) ISCLOC1,ISCLOC2
  114          FORMAT(1X,'(THE SCALE FACTOR WAS APPLIED TO FIELDS ',
     1           I2,'-',I2,')')
            END IF
         ENDIF
         READ(IN,'(A)') LINE
      END IF
C
C  Define label for printout
      BUF = LABEL
      NBUF = LEN(LABEL)+3
      IF (ITERP.EQ.1) THEN
         WRITE(IOUT,103) BUF(1:NBUF)
         WRITE(IOUT,104) (DASH(J),J=1,NBUF)
  103    FORMAT(1X,/1X,A)
  104    FORMAT(1X,120A)
      ENDIF
C
      N = NLIST+LSTBEG-1
      DO 250 II=LSTBEG,N
C  Read a line into the buffer.  (The first line has already been read
C  in order to scan for EXTERNAL and SFAC records.)
      IF(II.NE.LSTBEG) READ(IN,'(A)') LINE
C
C  Read the non-optional values from a line.
      LLOC = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,K,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I1,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,J1,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I2,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,J2,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,FACTOR,IOUT,IN)
      HFB(1,II) = K
      HFB(2,II) = I1
      HFB(3,II) = J1
      HFB(4,II) = I2
      HFB(5,II) = J2
      HFB(6,II) = FACTOR*SFAC
C
C  Write the values that were read
      NN = II-LSTBEG+1
      IF (ITERP.EQ.1) WRITE(IOUT,205) NN,K,I1,J1,I2,J2,HFB(6,II)
205   FORMAT(1X,I6,2X,I5,1X,4(2X,I5),2X,1G11.4)
C
C  Check for illegal grid location
      IF(K.LT.1 .OR. K.GT.NLAY) THEN
         WRITE(IOUT,*) ' Layer number in list is outside of the grid'
         CALL USTOP(' ')
      END IF
      IF(I1.LT.1 .OR. I1.GT.NROW .OR. I2.LT.1 .OR. I2.GT.NROW) THEN
         WRITE(IOUT,*) ' Row number in list is outside of the grid'
         CALL USTOP(' ')
      END IF
      IF(J1.LT.1 .OR. J1.GT.NCOL .OR. J2.LT.1 .OR. J2.GT.NCOL) THEN
         WRITE(IOUT,*) ' Column number in list is outside of the grid'
         CALL USTOP(' ')
      END IF
  250 CONTINUE
      IF(ICLOSE.NE.0) CLOSE(UNIT=IN)
C
      RETURN
      END

