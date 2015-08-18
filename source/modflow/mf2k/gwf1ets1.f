      SUBROUTINE GWF1ETS1ALP(ISUM,ISUMI,LCIETS,LCETSR,LCETSX,LCETSS,
     &                       NCOL,NROW,NETSOP,IN,IOUT,IETSCB,IFREFM,
     &                       NPETS,IETSPF,NETSEG,LCPXDP,LCPETM,NSEGAR)
C
C-----VERSION 20100315 ERB
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR EVAPOTRANSPIRATION SEGMENTS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
  500 FORMAT(1X,/
     &1X,'ETS1 -- EVAPOTRANSPIRATION SEGMENTS PACKAGE, VERSION 1,',
     &     ' 5/2/2000',/,9X,'INPUT READ FROM UNIT ',I4)
  510 FORMAT(
     &1X,I5,' SEGMENTS DEFINE EVAPOTRANSPIRATION RATE FUNCTION')
  520 FORMAT(' EVAPOTRANSPIRATION RATE FUNCTION IS LINEAR')
  530 FORMAT(
     &' ERROR: EVAPOTRANSPIRATION RATE FUNCTION MUST CONTAIN AT',/,
     &' LEAST ONE SEGMENT -- STOP EXECUTION (GWF1ETS1ALP)')
  540 FORMAT(1X,'ILLEGAL ET OPTION CODE. SIMULATION ABORTING')
  550 FORMAT(1X,'OPTION 1 -- EVAPOTRANSPIRATION FROM TOP LAYER')
  560 FORMAT(1X,'OPTION 2 -- EVAPOTRANSPIRATION FROM ONE SPECIFIED',
     &   ' NODE IN EACH VERTICAL COLUMN')
  564 FORMAT(1X,'OPTION 3 -- EVAPOTRANSPIRATION FROM UPPERMOST ACTIVE ',
     &   'CELL')
!  566 FORMAT(1X,'OPTION 4 -- EVAPOTRANSPIRATION FROM UPPERMOST ACTIVE ',
!     &   'CELL IN STACK OF',/,1X,
!     &   'CONTIGUOUS ACTIVE CELLS THAT INCLUDES BOTTOMMOST ACTIVE ',
!     &   'CELL')
  570 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I4)
  580 FORMAT(1X,I10,' ELEMENTS IN RX ARRAY ARE USED BY ETS')
  590 FORMAT(1X,I10,' ELEMENTS IN IR ARRAY ARE USED BY ETS')
C
C1------IDENTIFY PACKAGE.
      IETSPF=20
      WRITE(IOUT,500)IN
C
C     READ COMMENT LINE(S) (ITEM 0)
      CALL URDCOM(IN,IOUT,LINE)
C
C2------READ ET OPTION (NETSOP), UNIT OR FLAG FOR CELL-BY-CELL FLOW
C       TERMS (IETSCB), NUMBER OF PARAMETERS (NPETS), AND NUMBER OF
C       SEGMENTS (NETSEG) (ITEM 1)
      IF (IFREFM.EQ.0) THEN
        READ(LINE,'(4I10)') NETSOP,IETSCB,NPETS,NETSEG
      ELSE
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NETSOP,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IETSCB,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPETS,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NETSEG,R,IOUT,IN)
      ENDIF
C
C3------CHECK TO SEE THAT ET OPTION IS LEGAL.
!      IF (NETSOP.GE.1 .AND. NETSOP.LE.4) GO TO 10
      IF (NETSOP.GE.1 .AND. NETSOP.LE.3) GO TO 10
C
C3A-----OPTION IS ILLEGAL -- PRINT A MESSAGE & ABORT SIMULATION.
      WRITE(IOUT,540)
      CALL USTOP(' ')
C
C4------OPTION IS LEGAL -- PRINT THE OPTION CODE.
   10 CONTINUE
      IF (NETSOP.EQ.1) WRITE(IOUT,550)
      IF (NETSOP.EQ.2) WRITE(IOUT,560)
      IF (NETSOP.EQ.3) WRITE(IOUT,564) ! Add option 3 ERB 9/7/2006
!      IF (NETSOP.EQ.4) WRITE(IOUT,566) ! Add option 4 ERB 9/7/2006
C
C5------IF CELL-BY-CELL FLOWS ARE TO BE SAVED, THEN PRINT UNIT NUMBER.
      IF (IETSCB.GT.0) WRITE(IOUT,570) IETSCB
C
C-----PRINT NUMBER OF PARAMETERS TO BE USED
      CALL UPARARRAL(-1,IOUT,LINE,NPETS)
C
C     PRINT MESSAGE IDENTIFYING NUMBER OF SEGMENTS IN ET VS. HEAD CURVE
      IF(NETSEG.GT.1) THEN
        WRITE(IOUT,510) NETSEG
        NSEGAR = NETSEG - 1
      ELSEIF (NETSEG.EQ.1) THEN
        WRITE(IOUT,520)
        NSEGAR = 1
      ELSE
        WRITE(IOUT,530)
        CALL USTOP(' ')
      ENDIF
C
C6------ALLOCATE SPACE FOR THE ARRAYS ETSR, ETSX, ETSS, PXDP, AND PETM.
      IRK=ISUM
      LCETSR=ISUM
      ISUM=ISUM+NCOL*NROW
      LCETSX=ISUM
      ISUM=ISUM+NCOL*NROW
      LCETSS=ISUM
      ISUM=ISUM+NCOL*NROW
      IF (NETSEG.GT.1) THEN
        LCPXDP=ISUM
        ISUM=ISUM+NCOL*NROW*(NETSEG-1)
        LCPETM=ISUM
        ISUM=ISUM+NCOL*NROW*(NETSEG-1)
      ELSE
        LCPXDP=1
        LCPETM=1
      ENDIF
C
C7------ALLOCATE SPACE FOR LAYER INDICATOR ARRAY (IETS) EVEN IF ET
C7------OPTION IS NOT 2, TO AVOID ERROR OF ARRAY (IR) NOT LARGE ENOUGH
      LCIETS=ISUMI
      ISUMI=ISUMI+NCOL*NROW
C
C8------CALCULATE & PRINT AMOUNT OF SPACE USED BY ET PACKAGE.
      IRK=ISUM-IRK
      WRITE(IOUT,580)IRK
      IRK=NCOL*NROW
      WRITE(IOUT,590)IRK
C
C9------RETURN.
      RETURN
      END
C=======================================================================
      SUBROUTINE GWF1ETS1RPPD(IN,IOUT,NPETS,ITERP,INAMLOC)
C
C-----VERSION 20011120 ERB
C     ******************************************************************
C     READ EVAPOTRANSPIRATION SEGMENTS PARAMETER DEFINITIONS
C     ******************************************************************
C     Modified 11/21/2001 to support parameter instances - ERB
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*4 PTYP
C     ------------------------------------------------------------------
C
C-------READ NAMED PARAMETERS
      IF (ITERP.EQ.1) WRITE(IOUT,5) NPETS
    5 FORMAT(1X,//1X,I5,' Evapotranspiration segments parameters')
      IF (NPETS.GT.0) THEN
        DO 20 K=1,NPETS
C         UPARARRRP READS PARAMETER NAME AND DEFINITION (ITEMS 2 AND 3)
          CALL UPARARRRP(IN,IOUT,N,0,PTYP,ITERP,1,INAMLOC)
          IF(PTYP.NE.'ETS') THEN
            WRITE(IOUT,7)
    7       FORMAT(1X,'Parameter type must be ETS')
            CALL USTOP(' ')
          ENDIF
   20   CONTINUE
      ENDIF
C
C8------RETURN
   60 RETURN
      END
C=======================================================================
      SUBROUTINE GWF1ETS1RPSS(NETSOP,IETS,ETSR,ETSX,ETSS,DELR,DELC,NCOL,
     &                        NROW,IN,IOUT,IFREFM,NPETS,RMLT,IZON,
     &                        NMLTAR,NZONAR,IETSPF,NETSEG,PXDP,PETM,
     &                        NSEGAR)
C
C     VERSION 20000620 ERB
C     ******************************************************************
C     READ EVAPOTRANSPIRATION DATA, AND PERFORM SUBSTITUTION USING
C     PARAMETER VALUES IF ETS PARAMETERS ARE DEFINED
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*24 ANAME(6)
      DIMENSION IETS(NCOL,NROW),ETSR(NCOL,NROW),ETSX(NCOL,NROW),
     &          ETSS(NCOL,NROW),DELR(NCOL),DELC(NROW),
     &          RMLT(NCOL,NROW,NMLTAR),IZON(NCOL,NROW,NZONAR),
     &          PXDP(NCOL,NROW,NSEGAR),PETM(NCOL,NROW,NSEGAR)
C
      DATA ANAME(1) /'   ET LAYER INDEX (IETS)'/
      DATA ANAME(2) /'       ET SURFACE (ETSS)'/
      DATA ANAME(3) /' EVAPOTRANS. RATE (ETSR)'/
      DATA ANAME(4) /' EXTINCTION DEPTH (ETSX)'/
      DATA ANAME(5) /'EXTINCT. DEP. PROPORTION'/
      DATA ANAME(6) /'      ET RATE PROPORTION'/
C     ------------------------------------------------------------------
C
C1------READ FLAGS SHOWING WHETHER DATA FROM PREVIOUS STRESS PERIOS ARE
C       TO BE REUSED.
      IF (NETSEG.GT.1) THEN
        IF(IFREFM.EQ.0) THEN
          READ(IN,'(5I10)') INETSS,INETSR,INETSX,INIETS,INSGDF
        ELSE
          READ(IN,*) INETSS,INETSR,INETSX,INIETS,INSGDF
        ENDIF
      ELSE
        IF(NETSOP.EQ.2) THEN
          IF(IFREFM.EQ.0) THEN
            READ(IN,'(4I10)') INETSS,INETSR,INETSX,INIETS
          ELSE
            READ(IN,*) INETSS,INETSR,INETSX,INIETS
          ENDIF
        ELSE
          IF(IFREFM.EQ.0) THEN
            READ(IN,'(3I10)') INETSS,INETSR,INETSX
          ELSE
            READ(IN,*) INETSS,INETSR,INETSX
          ENDIF
        ENDIF
      ENDIF
C
C2------TEST INETSS TO SEE WHERE SURFACE ELEVATION COMES FROM.
      IF (INETSS.LT.0) THEN
C2A------IF INETSS<0 THEN REUSE SURFACE ARRAY FROM LAST STRESS PERIOD
        WRITE(IOUT,10)
   10   FORMAT(1X,/1X,'REUSING ETSS FROM LAST STRESS PERIOD')
      ELSE
C3-------IF INETSS=>0 THEN CALL MODULE U2DREL TO READ SURFACE.
        CALL U2DREL(ETSS,ANAME(2),NROW,NCOL,0,IN,IOUT)
      ENDIF
C
C4------TEST INETSR TO SEE WHERE MAX ET RATE COMES FROM.
      IF (INETSR.LT.0) THEN
C4A-----IF INETSR<0 THEN REUSE MAX ET RATE.
        WRITE(IOUT,20)
   20   FORMAT(1X,/1X,'REUSING ETSR FROM LAST STRESS PERIOD')
      ELSE
C5------IF INETSR=>0 CALL MODULE U2DREL TO READ MAX ET RATE.
        IF(NPETS.EQ.0) THEN
          CALL U2DREL(ETSR,ANAME(3),NROW,NCOL,0,IN,IOUT)
        ELSE
C    INETSR is the number of parameters to use this stress period
          CALL PRESET('ETS')
          WRITE(IOUT,30)
   30     FORMAT(1X,///1X,
     &        'ETSR array defined by the following parameters:')
          IF (INETSR.EQ.0) THEN
            WRITE(IOUT,35)
   35       FORMAT(' ERROR: When parameters are defined for the ETS',
     &      ' Package, at least one parameter',/,' must be specified',
     &      ' each stress period -- STOP EXECUTION (GWF1ETS1RPSS)')
            CALL USTOP(' ')
          ENDIF
          CALL UPARARRSUB2(ETSR,NCOL,NROW,0,INETSR,IN,IOUT,'ETS',
     &                     ANAME(3),'ETS',IETSPF,RMLT,IZON,NMLTAR,
     &                     NZONAR)
        ENDIF
C
C6------MULTIPLY MAX ET RATE BY CELL AREA TO GET VOLUMETRIC RATE
        DO 50 IR=1,NROW
          DO 40 IC=1,NCOL
            ETSR(IC,IR)=ETSR(IC,IR)*DELR(IC)*DELC(IR)
   40     CONTINUE
   50   CONTINUE
      ENDIF
C
C7------TEST INETSX TO SEE WHERE EXTINCTION DEPTH COMES FROM
      IF (INETSX.LT.0) THEN
C7A------IF INETSX<0 REUSE EXTINCTION DEPTH FROM LAST STRESS PERIOD
        WRITE(IOUT,60)
   60   FORMAT(1X,/1X,'REUSING ETSX FROM LAST STRESS PERIOD')
      ELSE
C8-------IF INETSX=>0 CALL MODULE U2DREL TO READ EXTINCTION DEPTH
        CALL U2DREL(ETSX,ANAME(4),NROW,NCOL,0,IN,IOUT)
      ENDIF
C
C9------IF OPTION(NETSOP) IS 2 THEN WE NEED AN INDICATOR ARRAY.
      IF (NETSOP.EQ.2) THEN
C10------IF INIETS<0 THEN REUSE LAYER INDICATOR ARRAY.
        IF (INIETS.LT.0) THEN
          WRITE(IOUT,70)
   70     FORMAT(1X,/1X,'REUSING IETS FROM LAST STRESS PERIOD')
        ELSE
C11------IF INIETS=>0 THEN CALL MODULE U2DINT TO READ INDICATOR ARRAY.
          CALL U2DINT(IETS,ANAME(1),NROW,NCOL,0,IN,IOUT)
        ENDIF
      ENDIF
C
C12------IF ET FUNCTION IS SEGMENTED PXDP AND PETM ARRAYS ARE NEEDED.
      IF (NETSEG.GT.1) THEN
C13------IF INSGDF<0 THEN REUSE PXDP AND PETM ARRAYS.
        IF (INSGDF.LT.0) THEN
          WRITE(IOUT,80)
   80     FORMAT(1X,/1X,
     &           'REUSING PXDP AND PETM FROM LAST STRESS PERIOD')
C14------IF INSGDF=>0 THEN CALL MODULE U2DREL TO READ PXDP AND PETM
C        ARRAYS.
        ELSE
          DO 90 I = 1,NETSEG-1
            WRITE(IOUT,100) I
            CALL U2DREL(PXDP(1,1,I),ANAME(5),NROW,NCOL,0,IN,IOUT)
            CALL U2DREL(PETM(1,1,I),ANAME(6),NROW,NCOL,0,IN,IOUT)
   90     CONTINUE
        ENDIF
      ENDIF
  100 FORMAT(/,' PXDP AND PETM ARRAYS FOR INTERSECTION ',I4,
     &' OF HEAD/ET RELATION:')
C
C15-----RETURN
      RETURN
      END
C=======================================================================
      SUBROUTINE GWF1ETS1FM(NETSOP,IETS,ETSR,ETSX,ETSS,RHS,HCOF,IBOUND,
     &                      HNEW,NCOL,NROW,NLAY,NETSEG,PXDP,PETM,
     &                      NSEGAR)
C
C-----VERSION 20100315 ERB
C     ******************************************************************
C        ADD EVAPOTRANSPIRATION TO RHS AND HCOF
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION HNEW, HH, SS, XX, DD, PXDP1, PXDP2
      DIMENSION IETS(NCOL,NROW), ETSR(NCOL,NROW), ETSX(NCOL,NROW),
     &          ETSS(NCOL,NROW), RHS(NCOL,NROW,NLAY),
     &          HCOF(NCOL,NROW,NLAY), IBOUND(NCOL,NROW,NLAY),
     &          HNEW(NCOL,NROW,NLAY), PXDP(NCOL,NROW,NSEGAR),
     &          PETM(NCOL,NROW,NSEGAR)
C     ------------------------------------------------------------------
C
C1------PROCESS EACH HORIZONTAL CELL LOCATION
      DO 30 IR=1,NROW
        DO 20 IC=1,NCOL
C
          IF (NETSOP.EQ.1) THEN
C2----------SET THE LAYER INDEX EQUAL TO 1      .
            IL=1
          ELSEIF (NETSOP.EQ.2) THEN
C3----------GET LAYER INDEX FROM IETS ARRAY
            IL=IETS(IC,IR)
            IF (IL.EQ.0) GO TO 20  ! ERB 1/11/07
          ELSEIF (NETSOP.EQ.3) THEN
C3A---------FIND UPPERMOST ACTIVE CELL
            IL = 1 ! If stack is inactive, this is the default
            FINDFIRST: DO ILQ=1,NLAY
              IF (IBOUND(IC,IR,ILQ).NE.0) THEN
                IL = ILQ
                EXIT FINDFIRST
              ENDIF
            ENDDO FINDFIRST
!          ELSEIF (NETSOP.EQ.4) THEN
!C3B---------FIND UPPERMOST ACTIVE CELL IN CONTIGUOUS STACK OF ACTIVE CELLS
!C           THAT INCLUDES LOWERMOST ACTIVE CELL
!            IL = 1 ! If stack is inactive, this is the default
!            FINDLAY: DO KL=NLAY,1,-1
!              IF (IBOUND(IC,IR,KL)==0) THEN
!                ! Cell is inactive, go up a layer
!                CYCLE FINDLAY
!              ELSE
!                ! Cell is active (variable-head or constant-head)
!                IF (KL>1) THEN
!                  IF (IBOUND(IC,IR,KL-1) .NE. 0) THEN
!                    ! Cell above is active; go up a layer
!                    CYCLE FINDLAY
!                  ENDIF
!                ENDIF
!              ENDIF
!              !   Cell is active and cell above is either (a) inactive, or
!              !   (b) nonexistent because this cell is in layer 1:
!              !   Evapotranspiration is from this cell.
!              IL = KL
!              !   Check IL and higher cells for constant head
!              DO KLC=1,IL
!C5B-------------IF CELL IS CONSTANT HEAD MOVE ON TO NEXT HORIZONTAL LOCATION.
!                IF(IBOUND(IC,IR,KLC).LT.0) GOTO 20
!              ENDDO
!              EXIT FINDLAY
!            ENDDO FINDLAY
          ENDIF
C
C4------IF THE CELL IS EXTERNAL IGNORE IT.
          IF (IBOUND(IC,IR,IL).GT.0) THEN
            C=ETSR(IC,IR)
            S=ETSS(IC,IR)
            SS=S
            HH=HNEW(IC,IR,IL)
C
C5------IF HEAD IN CELL IS GREATER THAN OR EQUAL TO ETSS, ET IS CONSTANT
            IF(HH.GE.SS) THEN
C
C5A-----SUBTRACT -ETSR FROM RHS
              RHS(IC,IR,IL)=RHS(IC,IR,IL) + C
            ELSE
C
C6------IF DEPTH TO WATER>=EXTINCTION DEPTH THEN ET IS 0
              DD=SS-HH
              X=ETSX(IC,IR)
              XX=X
              IF (DD.LT.XX) THEN
C7------VARIABLE RANGE. ADD ET TERMS TO BOTH RHS AND HCOF.
C
                IF (NETSEG.GT.1) THEN
C                 DETERMINE WHICH SEGMENT APPLIES BASED ON HEAD, AND
C                 CALCULATE TERMS TO ADD TO RHS AND HCOF
C
C                 SET PROPORTIONS CORRESPONDING TO ETSS ELEVATION
                  PXDP1 = 0.0
                  PETM1 = 1.0
                  DO 10 ISEG = 1,NETSEG
C                   SET PROPORTIONS CORRESPONDING TO LOWER END OF
C                   SEGMENT
                    IF (ISEG.LT.NETSEG) THEN
                      PXDP2 = PXDP(IC,IR,ISEG)
                      PETM2 = PETM(IC,IR,ISEG)
                    ELSE
                      PXDP2 = 1.0
                      PETM2 = 0.0
                    ENDIF
                    IF (DD.LE.PXDP2*XX) THEN
C                     HEAD IS IN DOMAIN OF THIS SEGMENT
                      GOTO 15
                    ENDIF
C                   PROPORTIONS AT LOWER END OF SEGMENT WILL BE FOR
C                   UPPER END OF SEGMENT NEXT TIME THROUGH LOOP
                    PXDP1 = PXDP2
                    PETM1 = PETM2
   10             CONTINUE
   15             CONTINUE
C                 CALCULATE TERMS TO ADD TO RHS AND HCOF BASED ON
C                 SEGMENT THAT APPLIES AT HEAD ELEVATION
                  THCOF = -(PETM1-PETM2)*C/((PXDP2-PXDP1)*X)
                  TRHS = THCOF*(S-PXDP1*X) + PETM1*C
                ELSE
C                 CALCULATE TERMS TO ADD TO RHS AND HCOF BASED ON SIMPLE
C                 LINEAR RELATION OF ET VS. HEAD
                  TRHS = C-C*S/X
                  THCOF = -C/X
                ENDIF
                RHS(IC,IR,IL)=RHS(IC,IR,IL)+TRHS
                HCOF(IC,IR,IL)=HCOF(IC,IR,IL)+THCOF
              ENDIF
            ENDIF
          ENDIF
   20   CONTINUE
   30 CONTINUE
C
C8------RETURN
      RETURN
      END
C=======================================================================
      SUBROUTINE GWF1ETS1BD(NETSOP,IETS,ETSR,ETSX,ETSS,IBOUND,HNEW,NCOL,
     &                      NROW,NLAY,DELT,VBVL,VBNM,MSUM,KSTP,KPER,
     &                      IETSCB,ICBCFL,BUFF,IOUT,PERTIM,TOTIM,NETSEG,
     &                      PXDP,PETM,NSEGAR)
C-----VERSION 201003215 ERB
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR EVAPOTRANSPIRATION SEGMENTS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*16 VBNM(MSUM), TEXT
      DOUBLE PRECISION HNEW, RATOUT, QQ, HH, SS, DD, XX, HHCOF, RRHS,
     &                 PXDP1, PXDP2
      DIMENSION IETS(NCOL,NROW), ETSR(NCOL,NROW), ETSX(NCOL,NROW),
     &          ETSS(NCOL,NROW), IBOUND(NCOL,NROW,NLAY),
     &          VBVL(4,MSUM), HNEW(NCOL,NROW,NLAY),
     &          BUFF(NCOL,NROW,NLAY), PXDP(NCOL,NROW,NSEGAR),
     &          PETM(NCOL,NROW,NSEGAR)
C
      DATA TEXT /'     ET SEGMENTS'/
C     ------------------------------------------------------------------
C
C1------CLEAR THE RATE ACCUMULATOR.
      ZERO=0.
      RATOUT=ZERO
C
C2------SET CELL-BY-CELL BUDGET SAVE FLAG (IBD) AND CLEAR THE BUFFER.
      IBD=0
      IF(IETSCB.GT.0) IBD=ICBCFL
      DO 30 IL=1,NLAY
        DO 20 IR=1,NROW
          DO 10 IC=1,NCOL
            BUFF(IC,IR,IL)=ZERO
   10     CONTINUE
   20   CONTINUE
   30 CONTINUE
C
C3------PROCESS EACH HORIZONTAL CELL LOCATION.
      DO 70 IR=1,NROW
        DO 60 IC=1,NCOL
C
          IF (NETSOP.EQ.1) THEN
C2----------SET THE LAYER INDEX EQUAL TO 1      .
            IL=1
          ELSEIF (NETSOP.EQ.2) THEN
C3----------GET LAYER INDEX FROM IETS ARRAY
            IL=IETS(IC,IR)
            IF (IL.EQ.0) GO TO 60  ! ERB 1/11/07
          ELSEIF (NETSOP.EQ.3) THEN
C3A---------FIND UPPERMOST ACTIVE CELL
            IL = 1 ! If stack is inactive, this is the default
            FINDFIRST: DO ILQ=1,NLAY
              IF (IBOUND(IC,IR,ILQ).NE.0) THEN
                IL = ILQ
                EXIT FINDFIRST
              ENDIF
            ENDDO FINDFIRST
            IETS(IC,IR) = IL
!          ELSEIF (NETSOP.EQ.4) THEN
!C3B---------FIND UPPERMOST ACTIVE CELL IN CONTIGUOUS STACK OF ACTIVE CELLS
!C           THAT INCLUDES LOWERMOST ACTIVE CELL
!            IL = 1 ! If stack is inactive, this is the default
!            FINDLAY: DO KL=NLAY,1,-1
!              IF (IBOUND(IC,IR,KL)==0) THEN
!                ! Cell is inactive, go up a layer
!                CYCLE FINDLAY
!              ELSE
!                ! Cell is active (variable-head or constant-head)
!                IF (KL>1) THEN
!                  IF (IBOUND(IC,IR,KL-1) .NE. 0) THEN
!                    ! Cell above is active; go up a layer
!                    CYCLE FINDLAY
!                  ENDIF
!                ENDIF
!              ENDIF
!              !   Cell is active and cell above is either (a) inactive, or
!              !   (b) nonexistent because this cell is in layer 1:
!              !   Evapotranspiration is from this cell.
!              IL = KL
!              !   Check IL and higher cells for constant head
!              DO KLC=1,IL
!C5B-------------IF CELL IS CONSTANT HEAD MOVE ON TO NEXT HORIZONTAL LOCATION.
!                IF(IBOUND(IC,IR,KLC).LT.0) GOTO 60
!              ENDDO
!              EXIT FINDLAY
!            ENDDO FINDLAY
!            IETS(IC,IR) = IL
          ENDIF
C
C6------IF CELL IS EXTERNAL THEN IGNORE IT.
          IF (IBOUND(IC,IR,IL).GT.0) THEN
            C=ETSR(IC,IR)
            S=ETSS(IC,IR)
            SS=S
            HH=HNEW(IC,IR,IL)
C
C7------IF HEAD IN CELL => ETSS,SET Q=MAX ET RATE.
            IF (HH.GE.SS) THEN
             QQ=-C
            ELSE
C
C8------IF DEPTH=>EXTINCTION DEPTH, ET IS 0.
              X=ETSX(IC,IR)
              XX=X
              DD=SS-HH
              IF (DD.LT.XX) THEN
C9------VARIABLE RANGE.  CALCULATE Q DEPENDING ON NUMBER OF SEGMENTS
C
                IF (NETSEG.GT.1) THEN
C                 DETERMINE WHICH SEGMENT APPLIES BASED ON HEAD, AND
C                 CALCULATE TERMS TO ADD TO RHS AND HCOF
C
C                 SET PROPORTIONS CORRESPONDING TO ETSS ELEVATION
                  PXDP1 = 0.0
                  PETM1 = 1.0
                  DO 40 ISEG = 1,NETSEG
C                   SET PROPORTIONS CORRESPONDING TO LOWER END OF
C                   SEGMENT
                    IF (ISEG.LT.NETSEG) THEN
                      PXDP2 = PXDP(IC,IR,ISEG)
                      PETM2 = PETM(IC,IR,ISEG)
                    ELSE
                      PXDP2 = 1.0
                      PETM2 = 0.0
                    ENDIF
                    IF (DD.LE.PXDP2*XX) THEN
C                     HEAD IS IN DOMAIN OF THIS SEGMENT
                      GOTO 50
                    ENDIF
C                   PROPORTIONS AT LOWER END OF SEGMENT WILL BE FOR
C                   UPPER END OF SEGMENT NEXT TIME THROUGH LOOP
                    PXDP1 = PXDP2
                    PETM1 = PETM2
   40             CONTINUE
   50             CONTINUE
C9------CALCULATE ET RATE BASED ON SEGMENT THAT APPLIES AT HEAD
C9------ELEVATION
                  HHCOF = -(PETM1-PETM2)*C/((PXDP2-PXDP1)*X)
                  RRHS = -HHCOF*(S-PXDP1*X) - PETM1*C
                ELSE
C10-----SIMPLE LINEAR RELATION.  Q=-ETSR*(HNEW-(ETSS-ETSX))/ETSX, WHICH
C10-----IS FORMULATED AS Q= -HNEW*ETSR/ETSX + (ETSR*ETSS/ETSX -ETSR).
                  HHCOF = -C/X
                  RRHS = (C*S/X) - C
                ENDIF
                QQ = HH*HHCOF + RRHS
              ELSE
                QQ = 0.0
              ENDIF
            ENDIF
C
C10-----ACCUMULATE TOTAL FLOW RATE.
            Q=QQ
            RATOUT=RATOUT-QQ
C
C11-----ADD Q TO BUFFER.
            BUFF(IC,IR,IL)=Q
          ENDIF
   60   CONTINUE
   70 CONTINUE
C
C12-----IF CELL-BY-CELL FLOW TO BE SAVED, CALL APPROPRIATE UTILITY
C12-----MODULE SAVE THEM.
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,IETSCB,BUFF,NCOL,NROW,
     &                         NLAY,IOUT)
      IF(IBD.EQ.2) CALL UBDSV3(KSTP,KPER,TEXT,IETSCB,BUFF,IETS,NETSOP,
     &                   NCOL,NROW,NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
C
C13-----MOVE TOTAL ET RATE INTO VBVL FOR PRINTING BY BAS1OT.
      ROUT=RATOUT
      VBVL(3,MSUM)=ZERO
      VBVL(4,MSUM)=ROUT
C
C14-----ADD ET(ET_RATE TIMES STEP LENGTH) TO VBVL.
      VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
C
C15-----MOVE BUDGET TERM LABELS TO VBNM FOR PRINT BY MODULE BAS1OT.
      VBNM(MSUM)=TEXT
C
C16-----INCREMENT BUDGET TERM COUNTER.
      MSUM=MSUM+1
C
C17-----RETURN.
      RETURN
      END
C=======================================================================

