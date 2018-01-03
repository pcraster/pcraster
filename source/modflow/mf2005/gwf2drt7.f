      MODULE GWFDRTMODULE
        INTEGER,SAVE,POINTER   ::NDRTCL,MXDRT,NDRTVL,NDRTNP,IDRTCB
        INTEGER,SAVE,POINTER   ::NPDRT,IDRTPB,IDRTFL,NRFLOW,NOPRDT
        REAL,         SAVE, DIMENSION(:,:), POINTER ::DRTF
        CHARACTER*16, SAVE, DIMENSION(:),   POINTER ::DRTAUX
      TYPE GWFDRTTYPE
        INTEGER, POINTER   ::NDRTCL,MXDRT,NDRTVL,NDRTNP,IDRTCB
        INTEGER, POINTER   ::NPDRT,IDRTPB,IDRTFL,NRFLOW,NOPRDT
        REAL,         DIMENSION(:,:), POINTER ::DRTF
        CHARACTER*16, DIMENSION(:),   POINTER ::DRTAUX
      END TYPE
      TYPE(GWFDRTTYPE), SAVE :: GWFDRTDAT(10)
      END MODULE GWFDRTMODULE



      SUBROUTINE GWF2DRT7AR(IN,IGRID)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE AND READ PARAMETERS FOR DRAINS AND
C     RETURN FLOWS
C     ******************************************************************
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IOUT,NCOL,NROW,NLAY,IFREFM
      USE GWFDRTMODULE, ONLY:NDRTCL,MXDRT,NDRTVL,NDRTNP,IDRTCB,NPDRT,
     1                       IDRTPB,IDRTFL,NRFLOW,NOPRDT,DRTF,DRTAUX
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
      ALLOCATE(NDRTCL,MXDRT,NDRTVL,NDRTNP,IDRTCB)
      ALLOCATE(NPDRT,IDRTPB,IDRTFL,NRFLOW,NOPRDT)
C
C1------IDENTIFY PACKAGE AND INITIALIZE NDRTCL.
      WRITE(IOUT,1)IN
    1 FORMAT(1X,/
     &1X,'DRT7 -- DRAIN RETURN PACKAGE, VERSION 7, 2/28/2006',/,
     &' INPUT READ FROM UNIT ',I4)
      NDRTCL=0
      NDRTNP=0
      IDRTFL=0
C
C2------READ MAXIMUM NUMBER OF DRAINS AND UNIT OR FLAG FOR
C2------CELL-BY-CELL FLOW TERMS.
C     READ COMMENTS (ITEM 0)
      CALL URDCOM(IN,IOUT,LINE)
C     READ ITEM 1
      IF (IFREFM.EQ.0) THEN
        READ(LINE,'(4I10)') MXADRT,IDRTCB,NPDRT,MXL
        LLOC=41
      ELSE
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXADRT,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDRTCB,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPDRT,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXL,R,IOUT,IN)
      ENDIF
      WRITE(IOUT,3) MXADRT
    3 FORMAT(1X,'MAXIMUM OF ',I6,
     &' ACTIVE DRAINS WITH RETURN FLOW AT ONE TIME')
      IF (IDRTCB.LT.0) WRITE(IOUT,7)
    7 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL NOT 0')
      IF (IDRTCB.GT.0) WRITE(IOUT,8) IDRTCB
    8 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I4)
C
      IF (NPDRT.GT.0) THEN
        WRITE(IOUT,9) NPDRT,MXL
    9   FORMAT(1X,I5,' Named Parameters     ',I5,' List entries')
      ELSE
        WRITE(IOUT,'(A)') ' No named parameters'
      END IF

C3------READ AUXILIARY VARIABLES AND CBC ALLOCATION OPTION.
      NAUX=0
      NOPRDT=0
      ALLOCATE (DRTAUX(20))
   10 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'AUXILIARY' .OR.
     &       LINE(ISTART:ISTOP).EQ.'AUX') THEN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
        IF (NAUX.LT.20) THEN
          NAUX=NAUX+1
          DRTAUX(NAUX)=LINE(ISTART:ISTOP)
          WRITE(IOUT,12) DRTAUX(NAUX)
   12     FORMAT(1X,'AUXILIARY DRAIN-RETURN FLOW VARIABLE: ',A)
        ENDIF
        GOTO 10
      ELSEIF (LINE(ISTART:ISTOP).EQ.'RETURNFLOW') THEN
        IDRTFL=4
        WRITE(IOUT,13)
        GOTO 10
   13   FORMAT(1X,'RETURN FLOW OPTION IS SELECTED')
      ELSE IF(LINE(ISTART:ISTOP).EQ.'NOPRINT') THEN
         WRITE(IOUT,14)
   14    FORMAT(1X,'LISTS OF DRAIN-RETURN CELLS WILL NOT BE PRINTED')
         NOPRDT = 1
         GO TO 10
      ENDIF
      NDRTVL=5+NAUX+2+IDRTFL
C
C4------ALLOCATE SPACE FOR THE DRTF ARRAY.
      IDRTPB=MXADRT+1
      MXDRT=MXADRT+MXL
      ALLOCATE (DRTF(NDRTVL,MXDRT))
C
C5------READ NAMED PARAMETERS.
      WRITE(IOUT,500) NPDRT
  500 FORMAT(1X,//1X,I5,' Drain-return parameters')
      IF (NPDRT.GT.0) THEN
        NAUX=NDRTVL-5-2-IDRTFL
        LSTSUM=IDRTPB
        ITERPU = 1
        IF (NOPRDT .EQ.1) ITERPU = 99
        DO 100 K=1,NPDRT
          LSTBEG=LSTSUM
C5A-----READ ITEM 2
          CALL UPARLSTRP(LSTSUM,MXDRT,IN,IOUT,IP,'DRT','DRT',1,
     &                   NUMINST)
          NLST=LSTSUM-LSTBEG
          IF (NUMINST.GT.1) NLST = NLST/NUMINST
C5B-----ASSIGN STARTING INDEX FOR READING INSTANCES
          IF (NUMINST.EQ.0) THEN
            IB=0
          ELSE
            IB=1
          ENDIF
C         READ LIST(S) OF CELLS, PRECEDED BY INSTANCE NAME IF NUMINST>0
          LB=LSTBEG
          DO 50 I=IB,NUMINST
            IF (I.GT.0) THEN
              CALL UINSRP(I,IN,IOUT,IP,1)
            ENDIF
C5C-----READ ITEM 3
            CALL SGWF2DRT7LR(NLST,DRTF,LB,NDRTVL,MXDRT,IN,IOUT,
     &                       DRTAUX,5,NAUX,IFREFM,NCOL,NROW,NLAY,ITERPU,
     &                       IDRTFL)
            LB = LB+NLST
   50     CONTINUE
  100   CONTINUE
      ENDIF
C
C6------RETURN
      CALL SGWF2DRT7PSV(IGRID)
      RETURN
      END
      SUBROUTINE GWF2DRT7RP(IN,IGRID)
C     ******************************************************************
C     READ DRAIN HEAD, CONDUCTANCE AND BOTTOM ELEVATION.  IF THE
C     RETURNFLOW OPTION IS SELECTED, READ RECIPIENT CELL AND PROPORTION.
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IOUT,NCOL,NROW,NLAY,IFREFM
      USE GWFDRTMODULE, ONLY:NDRTCL,MXDRT,NDRTVL,NDRTNP,NPDRT,
     1                       IDRTPB,IDRTFL,NRFLOW,NOPRDT,DRTF,DRTAUX
C     ------------------------------------------------------------------
      CALL SGWF2DRT7PNT(IGRID)
C
C1------READ ITMP (NUMBER OF DRAINS OR FLAG TO REUSE DATA) AND
C1------NUMBER OF PARAMETERS.
      IF (NPDRT.GT.0) THEN
        IF (IFREFM.EQ.0) THEN
          READ(IN,'(2I10)') ITMP,NP
        ELSE
          READ(IN,*) ITMP,NP
        ENDIF
      ELSE
        NP=0
        IF (IFREFM.EQ.0) THEN
          READ(IN,'(I10)') ITMP
        ELSE
          READ(IN,*) ITMP
        ENDIF
      ENDIF
C
C------CALCULATE SOME CONSTANTS
      NAUX=NDRTVL-5-2-IDRTFL
      ITERPU = 1
      IOUTU = IOUT
      IF (NOPRDT.EQ.1) THEN
        ITERPU = 99
        IOUTU = -IOUT
      ENDIF
C
C2------DETERMINE THE NUMBER OF NON-PARAMETER DRAIN-RETURN CELLS.
      IF (ITMP.LT.0) THEN
        WRITE(IOUT,7)
    7   FORMAT(1X,/,
     &' REUSING NON-PARAMETER DRAIN-RETURN CELLS FROM',
     &' LAST STRESS PERIOD')
      ELSE
        NDRTNP=ITMP
      ENDIF
C
C3------IF THERE ARE NEW NON-PARAMETER DRAIN-RETURN CELLS, READ THEM.
      MXADRT=IDRTPB-1
      IF (ITMP.GT.0) THEN
        IF (NDRTNP.GT.MXADRT) THEN
          WRITE(IOUT,500) NDRTNP,MXADRT
  500     FORMAT(1X,/1X,'THE NUMBER OF ACTIVE DRT DRAINS (',I6,
     &           ') IS GREATER THAN MXADRT(',I6,')')
          CALL USTOP(' ')
        ENDIF
        CALL SGWF2DRT7LR(NDRTNP,DRTF,1,NDRTVL,MXDRT,IN,IOUT,
     &                   DRTAUX,5,NAUX,IFREFM,NCOL,NROW,NLAY,ITERPU,
     &                   IDRTFL)
      ENDIF
      NDRTCL=NDRTNP
C
C1C-----IF THERE ARE ACTIVE DRT PARAMETERS, READ THEM AND SUBSTITUTE
      CALL PRESET('DRT')
      IF (NP.GT.0) THEN
        NREAD=NDRTVL-2
        DO 30 N=1,NP
          CALL SGWF2DRT7LS(IN,IOUTU,DRTF,NDRTVL,MXDRT,NREAD,MXADRT,
     &                     NDRTCL,DRTAUX,20,NAUX,IDRTFL)
   30   CONTINUE
      ENDIF
C
C     COUNT NUMBER OF DRAIN-RETURN CELLS THAT CAN HAVE RETURN FLOW
      NRFLOW = 0
      IF (IDRTFL.GT.0) THEN
        DO 40 I=1,NDRTCL
          IF (DRTF(6,I) .GT. 0.0) NRFLOW = NRFLOW + 1
   40   CONTINUE
      ENDIF
C
C3------PRINT NUMBER OF DRAIN-RETURN CELLS IN CURRENT STRESS PERIOD.
      WRITE (IOUT,510) NDRTCL
  510 FORMAT(1X,/1X,I6,' DRAIN-RETURN CELLS')
C
C8------RETURN.
      RETURN
      END
      SUBROUTINE GWF2DRT7FM(IGRID)
C     ******************************************************************
C     ADD DRAIN-RETURN FLOW TO SOURCE TERMS FOR BOTH DRAIN-RETURN CELLS
C     AND RECIPIENT CELLS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:HNEW,HCOF,RHS,IBOUND
      USE GWFDRTMODULE, ONLY:NDRTCL,DRTF,IDRTFL
C
      DOUBLE PRECISION EEL
C     ------------------------------------------------------------------
      CALL SGWF2DRT7PNT(IGRID)
C
C1------IF NDRTCL<=0 THERE ARE NO DRAINS. RETURN.
      IF (NDRTCL.LE.0) RETURN
C
C2------PROCESS EACH CELL IN THE DRAIN-RETURN CELL LIST.
      DO 100 L=1,NDRTCL
C
C3------GET COLUMN, ROW AND LAYER OF CELL CONTAINING DRAIN.
        IL=DRTF(1,L)
        IR=DRTF(2,L)
        IC=DRTF(3,L)
C
C4-------IF THE CELL IS EXTERNAL SKIP IT.
        IF (IBOUND(IC,IR,IL).LE.0) GOTO 100
C
C5-------IF THE CELL IS INTERNAL GET THE DRAIN DATA.
        EL=DRTF(4,L)
        EEL=EL
C
C6------IF HEAD IS LOWER THAN DRAIN THEN SKIP THIS CELL.
        IF (HNEW(IC,IR,IL).LE.EEL) GOTO 100
C
C7------HEAD IS HIGHER THAN DRAIN. ADD TERMS TO RHS AND HCOF.
        C=DRTF(5,L)
        HCOF(IC,IR,IL)=HCOF(IC,IR,IL)-C
        RHS(IC,IR,IL)=RHS(IC,IR,IL)-C*EL
        IF (IDRTFL.GT.0) THEN
          ILR = DRTF(6,L)
          IF (ILR.NE.0) THEN
            IRR = DRTF(7,L)
            ICR = DRTF(8,L)
            IF (IBOUND(ICR,IRR,ILR) .GT. 0) THEN
              RFPROP = DRTF(9,L)
              H = HNEW(IC,IR,IL)
              RHS(ICR,IRR,ILR) = RHS(ICR,IRR,ILR)
     &                           - RFPROP*C*(H-EL)
            END IF
          ENDIF
        ENDIF
  100 CONTINUE
C
C8------RETURN.
      RETURN
      END
      SUBROUTINE GWF2DRT7BD(KSTP,KPER,IGRID)
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR DRAIN-RETURN CELLS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL ,      ONLY: IOUT,HNEW,IBOUND,BUFF,NCOL,NROW,NLAY
      USE GWFBASMODULE, ONLY: MSUM,VBNM,VBVL,PERTIM,TOTIM,DELT,ICBCFL,
     1                        IAUXSV
      USE GWFDRTMODULE, ONLY: DRTF,NDRTCL,MXDRT,IDRTCB,NDRTVL,IDRTFL,
     1                        NRFLOW,DRTAUX
C
      DOUBLE PRECISION HHNEW,EEL,CC,CEL,RATIN,RATOUT,QQ
      CHARACTER*16 TEXT
      DATA TEXT /'    DRAINS (DRT)'/
C     ------------------------------------------------------------------
      CALL SGWF2DRT7PNT(IGRID)
C
C1------INITIALIZE CELL-BY-CELL FLOW TERM FLAG (IBD) AND
C1------ACCUMULATORS (RATIN AND RATOUT).
      ZERO=0.
      RATIN=ZERO
      RATOUT=ZERO
      IBD=0
      IF (IDRTCB.LT.0 .AND. ICBCFL.NE.0) IBD=-1
      IF (IDRTCB.GT.0) IBD=ICBCFL
      IBDLBL=0
C
C2------IF CELL-BY-CELL FLOWS WILL BE SAVED AS A LIST, WRITE HEADER.
      IF (IBD.EQ.2) THEN
        NAUX = NDRTVL - 5 - 2 - IDRTFL
        IF (IAUXSV.EQ.0) NAUX = 0
        CALL UBDSV4(KSTP,KPER,TEXT,NAUX,DRTAUX,IDRTCB,NCOL,NROW,NLAY,
     &              NDRTCL+NRFLOW,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      ENDIF
C
C3------CLEAR THE BUFFER.
      DO 30 IL=1,NLAY
        DO 20 IR=1,NROW
          DO 10 IC=1,NCOL
            BUFF(IC,IR,IL)=ZERO
   10     CONTINUE
   20   CONTINUE
   30 CONTINUE
C
C4------IF THERE ARE NO DRAIN-RETURN CELLS THEN DO NOT ACCUMULATE FLOW.
      IF (NDRTCL.LE.0) GOTO 200
C
C5------LOOP THROUGH EACH DRAIN-RETURN CELL, CALCULATING FLOW.
      DO 100 L=1,NDRTCL
C
C5A-----GET LAYER, ROW & COLUMN OF CELL CONTAINING DRAIN.
        IL=DRTF(1,L)
        IR=DRTF(2,L)
        IC=DRTF(3,L)
        Q=ZERO
        ILR=0
        IF (IDRTFL.GT.0) THEN
          QIN=ZERO
          ILR = DRTF(6,L)
          IF (ILR.NE.0) THEN  ! Condition added 8/26/08 ERB
            IRR = DRTF(7,L)
            ICR = DRTF(8,L)
          ENDIF
        END IF
C
C5B-----IF CELL IS NO-FLOW OR CONSTANT-HEAD, IGNORE IT.
        IF (IBOUND(IC,IR,IL).LE.0) GOTO 99
C
C5C-----GET DRAIN PARAMETERS FROM DRAIN-RETURN LIST.
        EL=DRTF(4,L)
        EEL=EL
        C=DRTF(5,L)
        HHNEW=HNEW(IC,IR,IL)
C
C5D-----IF HEAD HIGHER THAN DRAIN, CALCULATE Q=C*(EL-HHNEW).
C5D-----SUBTRACT Q FROM RATOUT.
        IF (HHNEW.GT.EEL) THEN
          CC=C
          CEL=C*EL
          QQ=CEL - CC*HHNEW
          Q=QQ
          RATOUT=RATOUT-QQ
          IF (IDRTFL.GT.0) THEN
            IF (ILR.NE.0 .AND. IBOUND(ICR,IRR,ILR).GT.0) THEN
              RFPROP = DRTF(9,L)
              QQIN = RFPROP*(CC*HHNEW-CEL)
              QIN = QQIN
              RATIN = RATIN + QQIN
            ENDIF
          ENDIF
        ENDIF
C
C5E-----PRINT THE INDIVIDUAL RATES IF REQUESTED(IDRTCB<0).
        IF (IBD.LT.0) THEN
          IF (IBDLBL.EQ.0) WRITE(IOUT,61) TEXT,KPER,KSTP
   61     FORMAT(1X,/1X,A,'   PERIOD ',I4,'   STEP ',I3)
          WRITE(IOUT,62) L,IL,IR,IC,Q
   62     FORMAT(1X,'DRAIN ',I6,'   LAYER ',I3,'   ROW ',I5,
     &       '   COL ',I5,'   RATE ',1PG15.6)
          IF (ILR.NE.0) THEN
            WRITE(IOUT,550) L,ILR,IRR,ICR,QIN
  550       FORMAT(1X,'DRAIN ',I6,' RETURN:  LAYER ',I3,'   ROW ',I5,
     &       '   COL ',I5,'   RATE ',1PG15.6)
          ENDIF
          IBDLBL=1
        ENDIF
C
C5F-----ADD Q TO BUFFER.
        BUFF(IC,IR,IL) = BUFF(IC,IR,IL) + Q
        IF (IDRTFL.GT.0 .AND. ILR.GT.0)
     &      BUFF(ICR,IRR,ILR) = BUFF(ICR,IRR,ILR) + QIN
C
C5G-----IF SAVING CELL-BY-CELL FLOWS IN A LIST, WRITE FLOW.  OR IF
C5G-----RETURNING THE FLOW IN THE DRTF ARRAY, COPY FLOW TO DRTF.
   99   IF (IBD.EQ.2) THEN
          CALL UBDSVB(IDRTCB,NCOL,NROW,IC,IR,IL,Q,DRTF(:,L),NDRTVL,NAUX,
     &                10,IBOUND,NLAY)
          IF (IDRTFL.NE.0 .AND. ILR.GT.0)
     &      CALL UBDSVB(IDRTCB,NCOL,NROW,ICR,IRR,ILR,QIN,DRTF(:,L),
     &                  NDRTVL,NAUX,10,IBOUND,NLAY)
        ENDIF
        DRTF(NDRTVL,L) = Q
        DRTF(NDRTVL-1,L) = QIN
  100 CONTINUE
C
C6------IF CELL-BY-CELL FLOW WILL BE SAVED AS A 3-D ARRAY,
C6------CALL UBUDSV TO SAVE THEM.
      IF (IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,IDRTCB,BUFF,NCOL,NROW,
     &                          NLAY,IOUT)
C
C7------MOVE RATES,VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
  200 CONTINUE
      RIN = RATIN
      ROUT=RATOUT
      VBVL(3,MSUM)=RIN
      VBVL(1,MSUM)=VBVL(1,MSUM)+RIN*DELT
      VBVL(4,MSUM)=ROUT
      VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
      VBNM(MSUM)=TEXT
C
C8------INCREMENT BUDGET TERM COUNTER.
      MSUM=MSUM+1
C
C9------RETURN.
      RETURN
      END
      SUBROUTINE SGWF2DRT7LR(NLIST,DRTF,LSTBEG,NDRTVL,MXDRT,
     &                       INPACK,IOUT,DRTAUX,NCAUX,NAUX,IFREFM,
     &                       NCOL,NROW,NLAY,ITERP,IDRTFL)
C     ******************************************************************
C     Read and print a list of drain and optional associated
C     return-flow recipient cells.  NAUX of the values in the list are
C     optional -- auxiliary data.
C     ******************************************************************
      CHARACTER*57 LABEL1, LABEL2, LABEL3
      CHARACTER*16 DRTAUX(NCAUX)
      DIMENSION DRTF(NDRTVL,MXDRT)
      CHARACTER*200 LINE,FNAME
      DATA NUNOPN/99/
      INCLUDE 'openspec.inc'
C     ------------------------------------------------------------------
C
      IERR = 0
      ISCLOC1 = 5
      ISCLOC2 = 5
      IN=INPACK
      ICLOSE=0
      LABEL1='DRAIN NO.  LAYER   ROW   COL     DRAIN EL.  STRESS FACTOR'
      LABEL2='          ----DRAIN CELL----  --RECIPIENT CELL--   RETURN'
      LABEL3='DRAIN NO.  LAYER   ROW   COL   LAYER   ROW   COL    PROP.'
C
C  Check for and decode EXTERNAL and SFAC records.
      READ(IN,'(A)') LINE
      SFAC=1.
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF (LINE(ISTART:ISTOP).EQ.'EXTERNAL') THEN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,R,IOUT,IN)
        IN=I
        WRITE(IOUT,510) IN
  510   FORMAT(1X,'Reading list on unit ',I4)
        READ(IN,'(A)') LINE
      ELSEIF (LINE(ISTART:ISTOP).EQ.'OPEN/CLOSE') THEN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
        FNAME=LINE(ISTART:ISTOP)
        IN=NUNOPN
        WRITE(IOUT,520) IN,FNAME
  520   FORMAT(1X,/1X,'OPENING FILE ON UNIT ',I4,':',/1X,A)
        OPEN(UNIT=IN,FILE=FNAME,ACTION=ACTION(1))
        ICLOSE=1
        READ(IN,'(A)') LINE
      ENDIF
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF (LINE(ISTART:ISTOP).EQ.'SFAC') THEN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,SFAC,IOUT,IN)
        WRITE(IOUT,530) SFAC
  530   FORMAT(1X,'LIST SCALING FACTOR= ',1PG12.5)
        IF (ISCLOC1.EQ.ISCLOC2) THEN
          WRITE(IOUT,540) ISCLOC1
  540     FORMAT(1X,'(THE SCALE FACTOR WAS APPLIED TO FIELD ',I2,')')
        ELSE
          WRITE(IOUT,550) ISCLOC1,ISCLOC2
  550     FORMAT(1X,'(THE SCALE FACTOR WAS APPLIED TO FIELDS ',
     &           I2,'-',I2,')')
        ENDIF
        READ(IN,'(A)') LINE
      ENDIF
C
C  Write a label for the list.
      WRITE(IOUT,'(1X)')
      CALL ULSTLB(IOUT,LABEL1,DRTAUX,NCAUX,NAUX)
C
C  Read the list
      NREAD2=NDRTVL-2
      NREAD1=NREAD2-NAUX
      N=NLIST+LSTBEG-1
      DO 100 II=LSTBEG,N
C  Read a line into the buffer.  (The first line has already been read
C  in order to scan for EXTERNAL and SFAC records.)
        IF (II.NE.LSTBEG) READ(IN,'(A)') LINE
C
C  Read the non-optional values from the line.
        IF (IDRTFL.EQ.0) THEN
          IF (IFREFM.EQ.0) THEN
            READ(LINE,'(3I10,9F10.0)') K,I,J,(DRTF(JJ,II),JJ=4,NREAD1)
            LLOC=10*NREAD1+1
          ELSE
            LLOC=1
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,K,R,IOUT,IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,R,IOUT,IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,J,R,IOUT,IN)
            DO 10 JJ=4,NREAD1
              CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,DRTF(JJ,II),
     &                    IOUT,IN)
   10       CONTINUE
          ENDIF
        ELSE
          IF (IFREFM.EQ.0) THEN
            READ(LINE,'(3I10,2F10.0,3I10,9F10.0)') K,I,J,
     &          (DRTF(JJ,II),JJ=4,5),KR,IR,JR,(DRTF(JJ,II),JJ=9,NREAD1)
            LLOC=10*NREAD1+1
          ELSE
            LLOC=1
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,K,R,IOUT,IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,R,IOUT,IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,J,R,IOUT,IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,DRTF(4,II),IOUT,
     &                  IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,DRTF(5,II),IOUT,
     &                  IN)
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,KR,R,IOUT,IN)
            IF (KR.EQ.0 .AND. NREAD1.EQ.9 .AND. NAUX.EQ.0) THEN
              IR = 0
              JR = 0
              DRTF(9,II) = 0.0
            ELSE
              CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IR,R,IOUT,IN)
              CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,JR,R,IOUT,IN)
              DO 20 JJ=9,NREAD1
                CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,DRTF(JJ,II),
     &                      IOUT,IN)
   20         CONTINUE
            ENDIF
          ENDIF
          DRTF(6,II) = KR
          DRTF(7,II) = IR
          DRTF(8,II) = JR
        ENDIF
        DRTF(1,II)=K
        DRTF(2,II)=I
        DRTF(3,II)=J
        DO 50 ILOC=ISCLOC1,ISCLOC2
          DRTF(ILOC,II)=DRTF(ILOC,II)*SFAC
   50   CONTINUE
C
C  Read the optional values from the line
        IF (NAUX.GT.0) THEN
          DO 60 JJ=NREAD1+1,NREAD2
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,IDUM,DRTF(JJ,II),IOUT,
     &                  IN)
   60     CONTINUE
        ENDIF
C
C  Write the values that were read and that are not related to
C  return flow.
        NN=II-LSTBEG+1
        IF (IDRTFL.EQ.0) THEN
          WRITE(IOUT,570) NN,K,I,J,(DRTF(JJ,II),JJ=4,NREAD2)
  570     FORMAT(1X,I6,I7,I7,I7,14G16.4)
        ELSE
          IF (NREAD2.GE.10) THEN
            WRITE(IOUT,570) NN,K,I,J,(DRTF(JJ,II),JJ=4,5),
     &                      (DRTF(JJ,II),JJ=10,NREAD2)
          ELSE
            WRITE(IOUT,570) NN,K,I,J,(DRTF(JJ,II),JJ=4,5)
          ENDIF
        ENDIF
C
C  Check for illegal grid location
        IF (K.LT.1 .OR. K.GT.NLAY) THEN
          WRITE(IOUT,*) ' ERROR: Layer number is outside of the grid'
          IERR = 1
        ENDIF
        IF (I.LT.1 .OR. I.GT.NROW) THEN
          WRITE(IOUT,*) ' ERROR: Row number is outside of the grid'
          IERR = 1
        ENDIF
        IF (J.LT.1 .OR. J.GT.NCOL) THEN
          WRITE(IOUT,*) ' ERROR: Column number is outside of the grid'
          IERR = 1
        ENDIF
        IF (IERR.NE.0) CALL USTOP(' ')
  100 CONTINUE
C
C     Check and write data related to return-flow recipient cells
      IF (IDRTFL.GT.0) THEN
        WRITE(IOUT,'(/,1X,A,/,1X,A)') LABEL2,LABEL3
        NN = 0
        DO 110 II=LSTBEG,N
          NN = NN + 1
          K = DRTF(1,II)
          I = DRTF(2,II)
          J = DRTF(3,II)
          KR = DRTF(6,II)
          IR = DRTF(7,II)
          JR = DRTF(8,II)
          RFP = DRTF(9,II)
          WRITE(IOUT,600) NN,K,I,J,KR,IR,JR,RFP
  600     FORMAT(1X,I6,3I7,3I7,2X,F8.6)
C
C  Check for illegal grid location
          IF (KR.NE.0) THEN
            IF (KR.LT.0 .OR. KR.GT.NLAY) THEN
              WRITE(IOUT,*) ' ERROR: Layer number is outside of the',
     &                      ' grid'
              IERR = 1
            ENDIF
            IF (IR.LT.1 .OR. IR.GT.NROW) THEN
              WRITE(IOUT,*) ' ERROR: Row number is outside of the grid'
              IERR = 1
            ENDIF
            IF (JR.LT.1 .OR. JR.GT.NCOL) THEN
              WRITE(IOUT,*) ' ERROR: Column number is outside of the',
     &                      ' grid'
              IERR = 1
            ENDIF
C
C  Check for invalid return-flow proportion
            IF (RFP.LT.0.0 .OR. RFP.GT.1.0) THEN
              WRITE(IOUT,590)
  590         FORMAT(' ERROR: Proportion must be between 0.0 and 1.0')
              IERR = 1
            ENDIF
          ENDIF
C
C  If the proportion = 0 or KR = 0, set all indices and proportion to 0
          IF (KR.EQ.0 .OR. RFP.EQ.0.0) THEN
            DRTF(6,II) = 0.0
            DRTF(7,II) = 0.0
            DRTF(8,II) = 0.0
            DRTF(9,II) = 0.0
          ENDIF
          IF (IERR.NE.0) CALL USTOP(' ')
  110   CONTINUE
      ENDIF
C
      IF (ICLOSE.NE.0) CLOSE(UNIT=IN)
C
      RETURN
      END
      SUBROUTINE SGWF2DRT7LS(IN,IOUTU,DRTF,NDRTVL,MXDRT,NREAD,MXADRT,
     &                       NDRTCL,DRTAUX,NCAUX,NAUX,IDRTFL)
C     ******************************************************************
C     Read a list parameter name, look it up in the list of parameters,
C     and substitute values into active part of package array.
C     ******************************************************************
C     Modified 11/8/2001 to support parameter instances - ERB
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE PARAMMODULE
      CHARACTER*3 PACK, PTYP
      DIMENSION DRTF(NDRTVL,MXDRT)
      CHARACTER*57 LABEL1, LABEL2, LABEL3
      CHARACTER*16 DRTAUX(NCAUX)
      CHARACTER*200 LINE
      CHARACTER*10 CTMP1, CTMP2, CTMP3, CTMP4
C     ------------------------------------------------------------------
  500 FORMAT(/,' Parameter:  ',A)
  510 FORMAT(1X,'Parameter type conflict:',/
     &       1X,'Named parameter:',A,' was defined as type:',A,/
     &       1X,'However, this parameter is used in the ',A,
     &       ' file, so it should be type:',A)
  512 FORMAT(/,1X,'Blank instance name in the ',A,
     &       ' file for parameter ',A)
  514 FORMAT(3X,'Instance:  ',A)
  516 FORMAT(/,1X,'The ',A,' file specifies undefined instance "',
     &       A,'" for parameter ',A)
  520 FORMAT(1X,/1X,'THE NUMBER OF ACTIVE LIST ENTRIES (',I6,
     &       ')',/1X,'IS GREATER THAN THE MAXIMUM ALLOWED (',I6,')')
  530 FORMAT(1X,I6,I7,I7,I7,14G16.4)
  550 FORMAT(/,1X,'*** ERROR: PARAMETER "',A,
     &'" HAS ALREADY BEEN ACTIVATED THIS STRESS PERIOD',/,
     &' -- STOP EXECUTION (SGWF2DRT7LS)')
  600 FORMAT(1X,I6,3I7,3I7,2X,F8.6)
C
      PACK = 'DRT'
      PTYP = 'DRT'
      IPVL1 = 5
      IPVL2 = 5
      LABEL1='DRAIN NO.  LAYER   ROW   COL     DRAIN EL.  CONDUCTANCE  '
      LABEL2='          ----DRAIN CELL----  --RECIPIENT CELL--   RETURN'
      LABEL3='DRAIN NO.  LAYER   ROW   COL   LAYER   ROW   COL    PROP.'
      IOUT = ABS(IOUTU)
C
      READ(IN,'(A)') LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,IDUM,RDUM,IOUT,IN)
      WRITE(IOUT,500) LINE(ISTART:ISTOP)
      IF(LINE(ISTART:ISTOP).EQ.' ') THEN
        WRITE(IOUT,*) ' Blank parameter name in the ',PACK,' file.'
        CALL USTOP(' ')
      END IF
C
      CTMP1=LINE(ISTART:ISTOP)
      CALL UPCASE(CTMP1)
      DO 100 IP=1,MXPAR
        CTMP2=PARNAM(IP)
        CALL UPCASE(CTMP2)
        IF(CTMP1.EQ.CTMP2) THEN
          IF(PARTYP(IP).NE.PTYP) THEN
            WRITE(IOUT,510) PARNAM(IP),PARTYP(IP),PACK,PTYP
            CALL USTOP(' ')
          ENDIF
C
C         DESIGNATE CELLS CORRESPONDING TO CORRECT PARAMETER INSTANCE
          NLST=IPLOC(2,IP)-IPLOC(1,IP)+1
          NUMINST=IPLOC(3,IP)
          ILOC=IPLOC(4,IP)
          NI=1
          IF(NUMINST.GT.0) THEN
            NLST=NLST/NUMINST
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,IDUM,RDUM,IOUT,IN)
            CTMP3=LINE(ISTART:ISTOP)
            IF(CTMP3.EQ.' ') THEN
              WRITE(IOUT,512)PACK,PARNAM(IP)
              CALL USTOP(' ')
            ENDIF
            WRITE(IOUT,514) CTMP3
            CALL UPCASE(CTMP3)
            DO 10 KI=1,NUMINST
              CTMP4=INAME(ILOC+KI-1)
              CALL UPCASE(CTMP4)
              IF(CTMP3.EQ.CTMP4) THEN
                NI=KI
                GOTO 15
              ENDIF
   10       CONTINUE
            WRITE(IOUT,516) PACK,CTMP3,PARNAM(IP)
            CALL USTOP(' ')
   15       CONTINUE
          ENDIF
C
          IF (IACTIVE(IP).GT.0) THEN
            WRITE(IOUT,550) PARNAM(IP)
            CALL USTOP(' ')
          ENDIF
C
          IACTIVE(IP)=NI
C
          NDRTCL=NDRTCL+NLST
          IF(NDRTCL.GT.MXADRT) THEN
            WRITE(IOUT,520) NDRTCL,MXADRT
            CALL USTOP(' ')
          ENDIF
C
C  Write label for list values
          IF (IOUTU.GT.0) CALL ULSTLB(IOUT,LABEL1,DRTAUX,NCAUX,NAUX)
C
C  Substitute values
          DO 60 I=1,NLST
            II=NDRTCL-NLST+I
            III=I-1+IPLOC(1,IP)+(NI-1)*NLST
            DO 20 J=1,NREAD
              DRTF(J,II)=DRTF(J,III)
   20       CONTINUE
            DO 40 IPVL=IPVL1,IPVL2
              DRTF(IPVL,II)=DRTF(IPVL,II)*B(IP)
   40       CONTINUE
            IL=DRTF(1,II)
            IR=DRTF(2,II)
            IC=DRTF(3,II)
            IF (IOUTU.GT.0) THEN
              IF (IDRTFL.EQ.0) THEN
                WRITE(IOUT,530) II,IL,IR,IC,(DRTF(JJ,II),JJ=4,NREAD)
              ELSE
                IF (NREAD.GE.10) THEN
                  WRITE(IOUT,530) II,IL,IR,IC,(DRTF(JJ,II),JJ=4,5),
     &                            (DRTF(JJ,II),JJ=10,NREAD)
                ELSE
                  WRITE(IOUT,530) II,IL,IR,IC,(DRTF(JJ,II),JJ=4,5)
                ENDIF
              ENDIF
            ENDIF
   60     CONTINUE
          GOTO 120
        ENDIF
  100 CONTINUE
C
      WRITE(IOUT,*) ' The ',PACK,
     &   ' file specifies an undefined parameter:',LINE(ISTART:ISTOP)
      CALL USTOP(' ')
C
  120 CONTINUE
C
C     WRITE DATA RELATED TO RETURN-FLOW RECIPIENT CELLS
      IF (IDRTFL.GT.0 .AND. IOUTU.GT.0) THEN
        WRITE(IOUT,'(/,1X,A,/,1X,A)') LABEL2,LABEL3
        NN = 0
        DO 140 II=NDRTCL-NLST+1,NDRTCL
          NN = NN + 1
          K = DRTF(1,II)
          I = DRTF(2,II)
          J = DRTF(3,II)
          KR = DRTF(6,II)
          IR = DRTF(7,II)
          JR = DRTF(8,II)
          RFP = DRTF(9,II)
          WRITE(IOUT,600) NN,K,I,J,KR,IR,JR,RFP
  140   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE GWF2DRT7DA(IGRID)
C  Deallocate DRT MEMORY
      USE GWFDRTMODULE
C
        CALL SGWF2DRT7PNT(IGRID)
        DEALLOCATE(NDRTCL)
        DEALLOCATE(MXDRT)
        DEALLOCATE(NDRTVL)
        DEALLOCATE(NDRTNP)
        DEALLOCATE(IDRTCB)
        DEALLOCATE(NPDRT)
        DEALLOCATE(IDRTPB)
        DEALLOCATE(IDRTFL)
        DEALLOCATE(NRFLOW)
        DEALLOCATE(NOPRDT)
        DEALLOCATE(DRTF)
        DEALLOCATE(DRTAUX)
C
      RETURN
      END
      SUBROUTINE SGWF2DRT7PNT(IGRID)
C  Change DRT data to a different grid.
      USE GWFDRTMODULE
C
        NDRTCL=>GWFDRTDAT(IGRID)%NDRTCL
        MXDRT=>GWFDRTDAT(IGRID)%MXDRT
        NDRTVL=>GWFDRTDAT(IGRID)%NDRTVL
        NDRTNP=>GWFDRTDAT(IGRID)%NDRTNP
        IDRTCB=>GWFDRTDAT(IGRID)%IDRTCB
        NPDRT=>GWFDRTDAT(IGRID)%NPDRT
        IDRTPB=>GWFDRTDAT(IGRID)%IDRTPB
        IDRTFL=>GWFDRTDAT(IGRID)%IDRTFL
        NRFLOW=>GWFDRTDAT(IGRID)%NRFLOW
        NOPRDT=>GWFDRTDAT(IGRID)%NOPRDT
        DRTF=>GWFDRTDAT(IGRID)%DRTF
        DRTAUX=>GWFDRTDAT(IGRID)%DRTAUX
C
      RETURN
      END
      SUBROUTINE SGWF2DRT7PSV(IGRID)
C  Save DRT data for a grid.
      USE GWFDRTMODULE
C
        GWFDRTDAT(IGRID)%NDRTCL=>NDRTCL
        GWFDRTDAT(IGRID)%MXDRT=>MXDRT
        GWFDRTDAT(IGRID)%NDRTVL=>NDRTVL
        GWFDRTDAT(IGRID)%NDRTNP=>NDRTNP
        GWFDRTDAT(IGRID)%IDRTCB=>IDRTCB
        GWFDRTDAT(IGRID)%NPDRT=>NPDRT
        GWFDRTDAT(IGRID)%IDRTPB=>IDRTPB
        GWFDRTDAT(IGRID)%IDRTFL=>IDRTFL
        GWFDRTDAT(IGRID)%NRFLOW=>NRFLOW
        GWFDRTDAT(IGRID)%NOPRDT=>NOPRDT
        GWFDRTDAT(IGRID)%DRTF=>DRTF
        GWFDRTDAT(IGRID)%DRTAUX=>DRTAUX
C
      RETURN
      END
