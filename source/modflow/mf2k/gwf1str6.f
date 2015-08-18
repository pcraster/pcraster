! Time of File Save by ERB: 3/9/2006 5:28PM
C     Last change:  ERB  12 Sep 2002    4:49 pm
      SUBROUTINE GWF1STR6ALP(ISUM,ISUMI,LCSTRM,ICSTRM,MXSTRM,NSTREM,IN,
     1              IOUT,ISTCB1,ISTCB2,NSS,NTRIB,NDIV,ICALC,CONST,
     2              LCTBAR,LCTRIB,LCIVAR,LCFGAR,NPSTR,ISTRPB)
C
C-----VERSION 15APRIL1998 GWF1STR6ALP
C-----VERSION 12MAR1999 -- modified to use character strings longer
C-----than 4 characters and to change VBVL and VBNM DIMENSION to MSUM
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR STREAMS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
C
C1------IDENTIFY PACKAGE AND INITIALIZE NSTREM.
      WRITE(IOUT,1) IN
    1 FORMAT(1X,/1X,'STRM -- STREAM PACKAGE, VERSION 6, 6/98 ',
     1'INPUT READ FROM UNIT ',I4)
      NSTREM=0
C
C2------ READ MXACTS, NSS, NTRIB, ISTCB1, AND ISTCB2.
      CALL URDCOM(IN,IOUT,LINE)
      CALL UPARLSTAL(IN,IOUT,LINE,NPSTR,MXPS)
      READ(LINE,3)MXACTS,NSS,NTRIB,NDIV,ICALC,CONST,ISTCB1,ISTCB2
    3 FORMAT(5I10,F10.0,2I10)
      IF(MXACTS.LT.0)MXACTS=0
      IF(NSS.LT.0)NSS=0
      WRITE(IOUT,4)MXACTS,NSS,NTRIB
    4 FORMAT(1X,'MAXIMUM OF ',I6,' ACTIVE STREAM NODES AT ONE TIME',/
     1   1X,'NUMBER OF STREAM SEGMENTS IS ',I6,/
     2   1X,'NUMBER OF STREAM TRIBUTARIES IS ',I6)
      IF(NDIV.GT.0) WRITE(IOUT,5)
    5 FORMAT(1H ,'DIVERSIONS FROM STREAMS HAVE BEEN SPECIFIED')
      IF(ICALC.GT.0) WRITE(IOUT,6) CONST
    6 FORMAT(1X,'STREAM STAGES WILL BE CALCULATED USING A CONSTANT OF '
     1       ,F10.4)
      IF(ISTCB1.GT.0) WRITE(IOUT,7) ISTCB1,ISTCB2
    7 FORMAT(1X,'CELL BUDGETS WILL BE SAVED ON UNITS ',I4,' AND ',I4)
C
C3------SET LCSTRM EQUAL TO ADDRESS OF FIRST UNUSED SPACE IN RX.
      ISTRPB=MXACTS+1
      MXSTRM=MXACTS+MXPS
      LCSTRM=ISUM
C
C4------CALCULATE AMOUNT OF SPACE NEEDED FOR STRM LIST.
      ISPA=11*MXSTRM
      ISUM=ISUM+ISPA
C
C5------CALCULATE AMOUNT OF SPACE NEEDED FOR ISTRM LIST.
      ICSTRM=ISUMI
      ISPB=5*MXSTRM
      ISUMI=ISUMI+ISPB
C
C6------CALCULATE AMOUNT OF SPACE NEEEDED FOR ITRBAR LIST.
      LCTBAR=ISUMI
      ISPC=NSS*NTRIB
      ISUMI=ISUMI+ISPC
C
C7------CALCULATE AMOUNT OF SPACE NEEDED FOR ARTRIB LIST.
      LCTRIB=ISUM
      ISPD=NSS
      ISUM=ISUM+ISPD
C
C8A-----CALCULATE AMOUNT OF SPACE NEEDED FOR IDIVAR LIST.
      LCIVAR=ISUMI
      ISPE=NSS
      ISUMI=ISUMI+ISPE
C
C8B-----CALCULATE AMOUNT IF SPACE NEEDED FOR NDFGAR LIST.
      LCFGAR=ISUMI
      ISPF=NSS
      ISUMI=ISUMI+ISPF
C
C9------PRINT AMOUNT OF SPACE USED BY STREAM PACKAGE.
      ISP=ISPA+ISPD
      WRITE (IOUT,8)ISP
    8 FORMAT(1X,I10,' ELEMENTS IN RX ARRAY ARE USED FOR STREAMS')
      ISP=ISPB+ISPC+ISPE+ISPF
      WRITE (IOUT,9)ISP
    9 FORMAT(1X,I10,' ELEMENTS IN IR ARRAY ARE USED FOR STREAMS')
C
C10-----RETURN.
      RETURN
      END
      SUBROUTINE GWF1STR6RPPD(IN,IOUT,NCOL,NROW,NLAY,NPSTR,STRM,ISTRM,
     &         ISTRPB,MXSTRM,ITERP,INAMLOC)
C
C-----VERSION 20011108 GWF1STR6RPPD
C     ******************************************************************
C     READ STREAM PARAMETERS
C     ******************************************************************
C     Modified 11/8/2001 to support parameter instances - ERB
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION STRM(11,MXSTRM),ISTRM(5,MXSTRM)
C     ------------------------------------------------------------------
C
C-------READ NAMED PARAMETERS.
      IF (ITERP.EQ.1) THEN
        IRDFLG = 0
        WRITE(IOUT,1000) NPSTR
 1000   FORMAT(1X,//1X,I5,' Stream parameters')
      ELSE
        IRDFLG = 1
      ENDIF
      IF(NPSTR.GT.0) THEN
        LSTSUM=ISTRPB
        DO 20 K=1,NPSTR
          LSTBEG=LSTSUM
          CALL UPARLSTRP(LSTSUM,MXSTRM,IN,IOUT,IP,'STR','STR',ITERP,
     &                   NUMINST,INAMLOC)
          NLST=LSTSUM-LSTBEG
          IF (NUMINST.GT.1) NLST = NLST/NUMINST
C         ASSIGN STARTING INDEX FOR READING INSTANCES
          IF (NUMINST.EQ.0) THEN
            IB=0
          ELSE
            IB=1
          ENDIF
C         READ LIST(S) OF CELLS, PRECEDED BY INSTANCE NAME IF NUMINST>0
          LB=LSTBEG
          DO 10 I=IB,NUMINST
            IF (I.GT.0) THEN
              CALL UINSRP(I,IN,IOUT,IP,ITERP)
            ENDIF
            CALL SGWF1STR6R(NLST,MXSTRM,STRM,ISTRM,LB,IN,
     &                      IOUT,NCOL,NROW,NLAY,IRDFLG)
            LB=LB+NLST
   10     CONTINUE
   20   CONTINUE
      END IF
C
C6------RETURN
      RETURN
      END
      SUBROUTINE GWF1STR6RPSS(STRM,ISTRM,NSTREM,MXSTRM,IN,IOUT,ITRBAR,
     1       NDIV,NSS,NTRIB,IDIVAR,ICALC,IPTFLG,NCOL,NROW,NLAY,NPSTR,
     2       ISTRPB)
C
C-----VERSION 15APRIL1998 GWF1STR6RPSS
C     ******************************************************************
C     READ STREAM DATA:  INCLUDES SEGMENT AND REACH NUMBERS, CELL
C         SEQUENCE OF SEGMENT AND REACH, FLOW INTO MODEL AT BOUNDARY,
C         STREAM STAGE, STREAMBED CONDUCTANCE, AND STREAMBED TOP AND
C         BOTTOM ELEVATIONS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION STRM(11,MXSTRM),ISTRM(5,MXSTRM),ITRBAR(NSS,NTRIB),
     1          IDIVAR(NSS)
C     ------------------------------------------------------------------
C
C1A-----IF MXSTREAM IS LESS THAN 1 THEN STREAM IS INACTIVE. RETURN.
      IF(MXSTRM.LT.1) RETURN
C
C1B-----READ ITMP(NUMBER OF STREAM CELLS OR PARAMETERS).
      READ(IN,1)ITMP,IRDFLG,IPTFLG
    1 FORMAT(4I10)
C
C
      MXACTS=ISTRPB-1
      IF(NPSTR.LE.0) THEN
C
C2A-----IF ITMP <0 THEN REUSE NON-PARAMETER DATA FROM LAST STRESS PERIOD.
         IF(ITMP.LT.0) THEN
            WRITE(IOUT,2)
    2       FORMAT(/,'REUSING STREAM NODES FROM LAST STRESS PERIOD')
            RETURN
         ELSE
C
C3A-----IF THERE ARE NEW NON-PARAMETER STREAM CELLS, READ THEM
            NSTREM=ITMP
            IF(NSTREM.GT.MXACTS) THEN
               WRITE(IOUT,99) NSTREM,MXACTS
   99          FORMAT(1X,/1X,'THE NUMBER OF ACTIVE STREAM CELLS (',
     1              I6,') IS GREATER THAN MXACTS(',I6,')')
               CALL USTOP(' ')
            END IF
            CALL SGWF1STR6R(NSTREM,MXSTRM,STRM,ISTRM,1,IN,IOUT,NCOL,
     1             NROW,NLAY,IRDFLG)
         END IF
      ELSE
C
C1C-----IF THERE ARE ACTIVE STR PARAMETERS, READ THEM AND SUBSTITUTE
         NSTREM=0
         CALL PRESET('STR')
         IF(ITMP.GT.0) THEN
            DO 100 N=1,ITMP
            CALL UPARLSTLOC(IN,'STR',IOUT,'STR',IBEG,IEND,PV)
            NLST=IEND-IBEG+1
            NSTREM=NSTREM+NLST
            IF(NLST.GT.MXACTS) THEN
               WRITE(IOUT,99) NLST,MXACTS
               CALL USTOP(' ')
            END IF
            DO 50 I=1,NLST
            II=NSTREM-NLST+I
            III=IBEG+I-1
            DO 40 J=1,5
            STRM(J,II)=STRM(J,III)
            ISTRM(J,II)=ISTRM(J,III)
   40       CONTINUE
            STRM(3,II)=STRM(3,II)*PV
            IF(IRDFLG.EQ.0) THEN
               IF (N.EQ.1 .AND. I.EQ.1) WRITE(IOUT,4)
    4 FORMAT(/,4X,'LAYER   ROW    COL    SEGMENT   REACH   STREAMFLOW',
     16X,'STREAM    STREAMBED     STREAMBED BOT  STREAMBED TOP',/27X,
     2'NUMBER   NUMBER                   STAGE   CONDUCTANCE',6X,
     3'ELEVATION      ELEVATION',/3X,110('-'))
               WRITE(IOUT,6)(ISTRM(J,II),J=1,5),(STRM(J,II),J=1,5)
            ENDIF
    6       FORMAT(1X,1X,I6,2I7,2I9,7X,G11.4,G12.4,G11.4,4X,2G13.4)
   50       CONTINUE
C
  100       CONTINUE
         END IF
      END IF
C
C3------PRINT NUMBER OF REACHES IN CURRENT STRESS PERIOD.
      WRITE (IOUT,101) NSTREM
  101 FORMAT(1X,/1X,I6,' STREAM REACHES')
C
C4------IF THERE ARE NO STREAM REACHES THEN RETURN.
      IF(NSTREM.EQ.0) RETURN
C
C6----READ AND PRINT DATA IF STREAM STAGE IS CALCULATED.
      IF(ICALC.LE.0) GO TO 300
      IF(IRDFLG.EQ.0) WRITE(IOUT,7)
    7 FORMAT(/,4X,'LAYER',3X,'ROW',4X,'COL   ',' SEGMENT',3X,
     1'REACH',8X,'STREAM',13X,'STREAM',10X,'ROUGH',/27X,'NUMBER',3X,
     2 'NUMBER',8X,'WIDTH',14X,'SLOPE',10X,'COEF.',/3X,110('-'))
      DO 280 II=1,NSTREM
      READ(IN,8) STRM(6,II),STRM(7,II),STRM(8,II)
    8 FORMAT(3F10.0)
      IF(IRDFLG.EQ.0) WRITE(IOUT,9)ISTRM(1,II),
     &    ISTRM(2,II),ISTRM(3,II),ISTRM(4,II),ISTRM(5,II),
     1    STRM(6,II),STRM(7,II),STRM(8,II)
    9 FORMAT(2X,I6,2I7,2I9,7X,G12.4,4X,G13.4,4X,G12.4)
  280 CONTINUE
C
C7------INITIALIZE ALL TRIBUTARY SEGMENTS TO ZERO.
  300 DO 320 IK=1,NSS
      DO 320 JK=1,NTRIB
      ITRBAR(IK,JK)=0
  320 CONTINUE
C
C8-----INITIALIZE DIVERSION SEGMENT ARRAY TO ZERO.
      DO 325 IK=1,NSS
      IDIVAR(IK)=0
  325 CONTINUE
C
C9-----READ AND PRINT TRIBUTARY SEGMENTS.
      IF(NTRIB.LE.0) GO TO 343
      IF(IRDFLG.EQ.0) WRITE(IOUT,10)NTRIB
   10 FORMAT(/,30X,'MAXIMUM NUMBER OF TRIBUTARY STREAMS IS ',I6,//1X,
     1 20X,'STREAM SEGMENT',15X,'TRIBUTARY STREAM SEGMENT NUMBERS')
      DO 340 IK=1,NSS
      READ(IN,11) (ITRBAR(IK,JK),JK=1,NTRIB)
   11 FORMAT(10I5)
      IF(IRDFLG.EQ.0) WRITE(IOUT,12) IK,(ITRBAR(IK,JK),JK=1,NTRIB)
   12 FORMAT(19X,I6,20X,10I5)
  340 CONTINUE
C
C10----READ AND PRINT DIVERSION SEGMENTS NUMBERS.
  343 IF(NDIV.LE.0) GO TO 350
      IF(IRDFLG.EQ.0) WRITE(IOUT,13)
   13 FORMAT(/,10X,'DIVERSION SEGMENT NUMBER',10X,
     1       'UPSTREAM SEGMENT NUMBER')
      DO 345 IK=1,NSS
      READ(IN,14) IDIVAR(IK)
   14 FORMAT(I10)
      IF(IRDFLG.EQ.0) WRITE(IOUT,15) IK,IDIVAR(IK)
   15 FORMAT(19X,I6,27X,I6)
  345 CONTINUE
C
C11----SET FLOW OUT OF REACH, FLOW INTO REACH, AND FLOW THROUGH
C      STREAM BED TO ZERO.
  350 DO 360 II =1,NSTREM
      STRM(9,II)=0.0
      STRM(10,II)=0.0
      STRM(11,II)=0.0
  360 CONTINUE
C
C12------RETURN
      RETURN
      END
      SUBROUTINE GWF1STR6FM(NSTREM,STRM,ISTRM,HNEW,HCOF,RHS,IBOUND,
     1                  MXSTRM,NCOL,NROW,NLAY,NSS,ITRBAR,NTRIB,ARTRIB,
     2                  IDIVAR,NDFGAR,ICALC,CONST)
C
C-----VERSION 15APRIL1998 GWF1STR6FM
C                                                                      C
C     *****************************************************************C
C     ADD STREAM TERMS TO RHS AND HCOF IF FLOW OCCURS IN MODEL CELL    C
C     *****************************************************************C
C                                                                      C
C     SPECIFICATIONS:                                                  C
C     -----------------------------------------------------------------C
C                                                                      C
      DOUBLE PRECISION HNEW
      DIMENSION STRM(11,MXSTRM),ISTRM(5,MXSTRM),HNEW(NCOL,NROW,NLAY),
     1          HCOF(NCOL,NROW,NLAY),RHS(NCOL,NROW,NLAY),
     2          IBOUND(NCOL,NROW,NLAY),ITRBAR(NSS,NTRIB),ARTRIB(NSS),
     3          IDIVAR(NSS),NDFGAR(NSS)
C     -----------------------------------------------------------------C
C                                                                      C
C1------IF NSTREM<=0 THERE ARE NO STREAMS. RETURN.                     C
      IF(NSTREM.LE.0)RETURN
C                                                                      C
C2A-----PROCESS EACH CELL IN THE STREAM LIST.                          C
C2B-----INITIALIZE NDFGAR ARRAY TO ZERO.                               C
      DO 5 I=1,NSS
      NDFGAR(I)=0
    5 CONTINUE
C                                                                      C
C3------DETERMINE LAYER, ROW, COLUMN OF EACH REACH.                    C
      DO 500 L=1,NSTREM
      LL=L-1
      IL=ISTRM(1,L)
      IR=ISTRM(2,L)
      IC=ISTRM(3,L)
C                                                                      C
C4----06FEB1990, CHECK FOR CELLS OUTSIDE MOVED TO C12, C16 AND C18.    C
C                                                                      C
C5------DETERMINE STREAM SEGMENT AND REACH NUMBER.                     C
      ISTSG=ISTRM(4,L)
      NREACH=ISTRM(5,L)
C                                                                      C
C6------SET FLOWIN EQUAL TO STREAM SEGMENT INFLOW IF FIRST REACH.      C
      IF(NREACH.GT.1) GO TO 200
      FLOWIN=STRM(1,L)
C                                                                      C
C7------STORE OUTFLOW FROM PREVIOUS SEGMENT IN ARTRIB IF SEGMENT >1.   C
      IF(ISTSG.EQ.1)GO TO 50
      IFLG = ISTRM(4,LL)
      ARTRIB(IFLG)=STRM(9,LL)
C                                                                      C
C8A-----CHECK UPSTREAM SEGMENT FOR DIVERSIONS.                         C
      DO 40 NSFLG = 1,NSS
      IF(IFLG.NE.IDIVAR(NSFLG)) GO TO 40
C                                                                      C
C8B-----DETERMINE AMOUNT OF FLOW TO BE DIVERTED.                       C
      DO 20 IDL=1,NSTREM
      IF(NSFLG.NE.ISTRM(4,IDL)) GO TO 20
      IF(ISTRM(5,IDL).NE.1) GO TO 20
      DUM=ARTRIB(IFLG)-STRM(1,IDL)
C                                                                      C
C8C-----SUBTRACT FLOW FROM UPSTREAM SEGMENT IF THERE IS ENOUGH FLOW    C
C-------IN UPSTREAM SEGMENT.                                           C
      IF(DUM.GE.0.0) ARTRIB(IFLG)=DUM
      IF(DUM.LT.0.0) NDFGAR(IFLG)=1
   20 CONTINUE
   40 CONTINUE
   50 IF(IDIVAR(ISTSG).LE.0) GO TO 60
      NDFLG=IDIVAR(ISTSG)
      IF(NDFGAR(NDFLG).EQ.1) FLOWIN=0.0
   60 IF(FLOWIN.GE.0.0) GO TO 300
C                                                                      C
C9-----SUM TRIBUTARY OUTFLOW AND USE AS INFLOW INTO DOWNSTREAM SEGMENT.C
      FLOWIN =0.
      DO 100 ITRIB=1,NTRIB
      INODE=ITRBAR(ISTSG,ITRIB)
      IF(INODE.LE.0) GO TO 100
      FLOWIN=FLOWIN+ARTRIB(INODE)
  100 CONTINUE
C                                                                      C
C10-----IF REACH >1, SET INFLOW EQUAL TO OUTFLOW FROM UPSTREAM REACH.  C
  200 IF(NREACH.GT.1) FLOWIN=STRM(9,LL)
C                                                                      C
C11----COMPUTE STREAM STAGE IN REACH IF ICALC IS GREATER THAN 1.       C
  300 IF(ICALC.LE.0) GO TO 310
      XNUM=((FLOWIN+STRM(9,L))/2.0)*STRM(8,L)
      DNOM=CONST*STRM(6,L)*(SQRT(STRM(7,L)))
      DEPTH=(XNUM/DNOM)**0.6
      IF(DEPTH.LE.0.) DEPTH=0.
      STRM(2,L)=DEPTH+STRM(5,L)
  310 HSTR=STRM(2,L)
C                                                                      C
C12----DETERMINE LEAKAGE THROUGH STREAMBED.                            C
      IF(IBOUND(IC,IR,IL).LE.0) GO TO 315
      IF(FLOWIN.LE.0.) HSTR=STRM(5,L)
      CSTR=STRM(3,L)
      SBOT=STRM(4,L)
      H=HNEW(IC,IR,IL)
      T=HSTR-SBOT
C                                                                      C
C13----COMPUTE LEAKAGE AS A FUNCTION OF STREAM STAGE AND HEAD IN CELL. C
      FLOBOT=CSTR*(HSTR-H)
C                                                                      C
C14----RECOMPUTE LEAKAGE IF HEAD IN CELL IS BELOW STREAMBED BOTTOM.    C
      IQFLG=0
      IF(H.GT.SBOT) GO TO 312
      IQFLG=1
      FLOBOT=CSTR*T
C                                                                      C
C15----SET LEAKAGE EQUAL TO STREAM INFLOW IF LEAKAGE MORE THAN INFLOW. C
  312 IF(FLOBOT.LE.FLOWIN) GO TO 320
      IQFLG=1
      FLOBOT=FLOWIN
C                                                                      C
C16-----STREAMFLOW OUT EQUALS STREAMFLOW IN MINUS LEAKAGE.             C
  315 IF(IBOUND(IC,IR,IL).LE.0) FLOBOT=0.
  320 FLOWOT=FLOWIN-FLOBOT
      IF((ISTSG.GT.1).AND.(NREACH.EQ.1)) STRM(9,LL)=ARTRIB(IFLG)
C                                                                      C
C17----STORE STREAM INFLOW, OUTFLOW AND LEAKAGE FOR EACH REACH.        C
      STRM(9,L)=FLOWOT
      STRM(10,L)=FLOWIN
      STRM(11,L)=FLOBOT
C                                                                      C
C18----RETURN TO STEP 3 IF STREAM INFLOW IS LESS THAN OR EQUAL TO ZERO C
C       AND LEAKAGE IS GREATER THAN OR EQUAL TO ZERO OR IF CELL        C
C       IS NOT ACTIVE--IBOUND IS LESS THAN OR EQUAL TO ZERO.           C
      IF(IBOUND(IC,IR,IL).LE.0) GO TO 500
      IF((FLOWIN.LE.0.0).AND.(FLOBOT.GE.0.0)) GO TO 500
C                                                                      C
C19------IF HEAD > BOTTOM THEN ADD TERMS TO RHS AND HCOF.              C
      IF(IQFLG.GT.0) GO TO 400
      RHS(IC,IR,IL)=RHS(IC,IR,IL)-CSTR*HSTR
      HCOF(IC,IR,IL)=HCOF(IC,IR,IL)-CSTR
      GO TO 500
C                                                                      C
C20------IF HEAD < BOTTOM THEN ADD TERM ONLY TO RHS.                   C
  400 RHS(IC,IR,IL)=RHS(IC,IR,IL)-FLOBOT
  500 CONTINUE
C                                                                      C
C21-----RETURN.                                                        C
      RETURN
      END
      SUBROUTINE GWF1STR6BD(NSTREM,STRM,ISTRM,IBOUND,MXSTRM,HNEW,NCOL,
     1  NROW,NLAY,DELT,VBVL,VBNM,MSUM,KSTP,KPER,ISTCB1,ISTCB2,ICBCFL,
     2  BUFF,IOUT,NTRIB,NSS,ARTRIB,ITRBAR,IDIVAR,NDFGAR,ICALC,CONST,
     3  IPTFLG)
C-----VERSION  2  18DEC1990 GWF1STR6BD                                     C
C                                                                      C
C     *****************************************************************C
C     CALCULATE VOLUMETRIC BUDGET FOR STREAMS                          C
C     *****************************************************************C
C                                                                      C
C     SPECIFICATIONS:                                                  C
C     -----------------------------------------------------------------C
      CHARACTER*16 VBNM(MSUM),TEXT,STRTXT
      DOUBLE PRECISION HNEW
      DIMENSION STRM(11,MXSTRM),ISTRM(5,MXSTRM),IBOUND(NCOL,NROW,NLAY),
     1          HNEW(NCOL,NROW,NLAY),VBVL(4,MSUM),
     2          BUFF(NCOL,NROW,NLAY),ARTRIB(NSS),ITRBAR(NSS,NTRIB),
     3             IDIVAR(NSS),NDFGAR(NSS)
      DATA   TEXT/'  STREAM LEAKAGE'/
      DATA STRTXT/'STREAM FLOW OUT '/
C     -----------------------------------------------------------------C
C                                                                      C
C1------SET IBD=1 IF BUDGET TERMS SHOULD BE SAVED ON DISK.             C
      IBD=0
      RATIN = 0.
      RATOUT = 0.
C                                                                      C
C2------IF NO REACHES, KEEP ZEROS IN ACCUMULATORS.                     C
      IF(NSTREM.EQ.0) GO TO 600
C                                                                      C
C3A-----TEST TO SEE IF CELL-BY-CELL TERMS ARE NEEDED.                  C
      IF((ICBCFL.EQ.0).OR.(ISTCB1.LE.0)) GO TO 10
C                                                                      C
C3B-----CELL-BY-CELL TERMS ARE NEEDED, SET IBD AND CLEAR BUFFER.       C
      IBD = 1
      DO 5 IL=1,NLAY
      DO 5 IR=1,NROW
      DO 5 IC=1,NCOL
      BUFF(IC,IR,IL)=0.
    5 CONTINUE
C                                                                      C
C3C-----INITIALIZE NDFGAR ARRAY TO ZERO.                               C
      DO 7 I=1,NSS
      NDFGAR(I)=0
    7 CONTINUE
C                                                                      C
C4------IF THERE ARE STREAMS THEN ACCUMULATE LEAKAGE TO OR FROM THEM.  C
   10 DO 500 L=1,NSTREM
      LL=L-1
C                                                                      C
C5---DETERMINE REACH LOCATION.                                         C
      IL=ISTRM(1,L)
      IR=ISTRM(2,L)
      IC=ISTRM(3,L)
C                                                                      C
C6----06FEB1990, CHECK FOR CELLS OUTSIDE MOVED TO C14, C18 AND C29.    C
C                                                                      C
C7------DETERMINE SEGMENT AND REACH NUMBER.                            C
      ISTSG=ISTRM(4,L)
      NREACH=ISTRM(5,L)
      IF(NREACH.GT.1) GO TO 200
C                                                                      C
C8------SET FLOWIN EQUAL TO SEGMENT INFLOW IF FIRST REACH.             C
      FLOWIN=STRM(1,L)
C                                                                      C
C9------STORE OUTFLOW FROM PREVIOUS SEGMENT IN ARTRIB IF SEGMENT >1.   C
      IF(ISTSG.EQ.1) GO TO 50
      IFLG = ISTRM(4,LL)
      ARTRIB(IFLG)=STRM(9,LL)
C                                                                      C
C10A----CHECK UPSTREAM SEGMENT FOR DIVERSIONS.                         C
      DO 40 NSFLG = 1,NSS
      IF(IFLG.NE.IDIVAR(NSFLG)) GO TO 40
C                                                                      C
C10B----DETERMINE AMOUNT OF FLOW TO BE DIVERTED.                       C
      DO 20 IDL=1,NSTREM
      IF(NSFLG.NE.ISTRM(4,IDL)) GO TO 20
      IF(ISTRM(5,IDL).NE.1) GO TO 20
      DUM=ARTRIB(IFLG)-STRM(1,IDL)
C                                                                      C
C10C----SUBTRACT FLOW FROM UPSTREAM SEGMENT IF THERE IS ENOUGH FLOW    C
C       IN UPSTREAM SEGMENT.                                           C
      IF(DUM.GE.0.0) ARTRIB(IFLG)=DUM
      IF(DUM.LT.0.0) NDFGAR(IFLG)=1
   20 CONTINUE
   40 CONTINUE
   50 IF(IDIVAR(ISTSG).LE.0) GO TO 60
      NDFLG=IDIVAR(ISTSG)
      IF(NDFGAR(NDFLG).EQ.1) FLOWIN=0.0
   60 IF(FLOWIN.GE.0.0) GO TO 300
C                                                                      C
C11--SUM TRIBUTARY OUTFLOW AND USE AS INFLOW INTO DOWNSTREAM SEGMENT.  C
      FLOWIN =0.
      DO 100 ITRIB=1,NTRIB
      INODE=ITRBAR(ISTSG,ITRIB)
      IF(INODE.LE.0) GO TO 100
      FLOWIN=FLOWIN+ARTRIB(INODE)
  100 CONTINUE
C                                                                      C
C12-----IF REACH >1, SET INFLOW EQUAL TO OUTFLOW FROM UPSTREAM REACH.  C
  200 IF(NREACH.GT.1) FLOWIN=STRM(9,LL)
C                                                                      C
C13----COMPUTE STREAM STAGE IN REACH IF ICALC > 1.                     C
  300 IF(ICALC.LE.0) GO TO 310
      XNUM=((FLOWIN+STRM(9,L))/2.0)*STRM(8,L)
      DNOM=CONST*STRM(6,L)*(SQRT(STRM(7,L)))
      DEPTH=(XNUM/DNOM)**0.6
      IF((DEPTH).LE.0) DEPTH=0.
      STRM(2,L)=DEPTH+STRM(5,L)
  310 HSTR=STRM(2,L)
C                                                                      C
C14----DETERMINE LEAKAGE THROUGH STREAMBED.                            C
      IF(IBOUND(IC,IR,IL).LE.0) GO TO 315
      IF(FLOWIN.LE.0.0) HSTR=STRM(5,L)
      CSTR=STRM(3,L)
      SBOT=STRM(4,L)
      H=HNEW(IC,IR,IL)
      T=HSTR-SBOT
C                                                                      C
C15----COMPUTE LEAKAGE AS A FUNCTION OF STREAM STAGE AND HEAD IN CELL. C
      FLOBOT=CSTR*(HSTR-H)
C                                                                      C
C16----RECOMPUTE LEAKAGE IF HEAD IN CELL IS BELOW STREAMBED BOTTOM.    C
      IF(H.GT.SBOT) GO TO 312
      FLOBOT=CSTR*T
C                                                                      C
C17----SET LEAKAGE EQUAL TO STREAM INFLOW IF LEAKAGE MORE THAN INFLOW. C
  312 IF(FLOBOT.LE.FLOWIN) GO TO 320
      FLOBOT=FLOWIN
C                                                                      C
C18----STREAMFLOW OUT EQUALS STREAMFLOW IN MINUS LEAKAGE.              C
  315 IF(IBOUND(IC,IR,IL).LE.0) FLOBOT=0.
  320 FLOWOT=FLOWIN-FLOBOT
      IF((ISTSG.GT.1).AND.(NREACH.EQ.1)) STRM(9,LL)=ARTRIB(IFLG)
C                                                                      C
C19----STORE STREAM INFLOW, OUTFLOW AND LEAKAGE FOR EACH REACH.        C
      STRM(9,L)=FLOWOT
      STRM(10,L)=FLOWIN
      STRM(11,L)=FLOBOT
C                                                                      C
C20----IF LEAKAGE FROM STREAMS IS TO BE SAVED THEN ADD RATE TO BUFFER.  C
      IF(IBD.EQ.1) BUFF(IC,IR,IL)=BUFF(IC,IR,IL)+FLOBOT
C                                                                      C
C21----DETERMINE IF FLOW IS INTO OR OUT OF MODEL CELL.                 C
C       SKIP ESTIMATE OF LEAKAGE FROM STREAM IF LEAKAGE IS ZERO.       C
      IF(FLOBOT)494,500,496
C                                                                      C
C22-----SUBTRACT FLOW RATE FROM RATOUT IF AQUIFER DISCHARGES TO STREAM.C
  494 RATOUT=RATOUT-FLOBOT
      GO TO 500
C                                                                      C
C23-----ADD FLOW RATE TO RATIN IF STREAM DISCHARGES TO AQUIFER.        C
  496 RATIN=RATIN+FLOBOT
  500 CONTINUE
C                                                                      C
C24-----IF BUDGET TERMS WILL BE SAVED THEN WRITE TO DISK.              C
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,ISTCB1,BUFF,NCOL,NROW,
     1                          NLAY,IOUT)
C                                                                      C
C25A-----MOVE RATES INTO VBVL FOR PRINTING BY MODULE BAS_OT.           C
  600 VBVL(3,MSUM)=RATIN
      VBVL(4,MSUM)=RATOUT
C                                                                      C
C25B-----MOVE PRODUCT OF RATE AND TIME STEP INTO VBVL ACCUMULATORS.    C
      VBVL(1,MSUM)=VBVL(1,MSUM)+RATIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+RATOUT*DELT
C                                                                      C
C25C-----MOVE BUDGET TERM LABELS INTO VBNM FOR PRINTING BY BAS_OT.     C
      VBNM(MSUM)=TEXT
C                                                                      C
C26-----INCREASE BUDGET TERM COUNTER BY ONE.                           C
      MSUM=MSUM+1
C                                                                      C
C27-----RESET IBD COUNTER TO ZERO.                                     C
      IBD=0
C28----IF STREAM OUTFLOW FROM EACH REACH IS TO BE STORED ON DISK       C
C     THEN STORE OUTFLOW RATES TO BUFFER.                              C
      IF((ICBCFL.EQ.0).OR.(ISTCB2.LE.0)) GO TO 625
      IBD = 1
      DO 605 IL=1,NLAY
      DO 605 IR=1,NROW
      DO 605 IC=1,NCOL
  605 BUFF(IC,IR,IL)=0.
C                                                                      C
C29-----SAVE STREAMFLOWS OUT OF EACH REACH ON DISK.                    C
      DO 615 L=1,NSTREM
      IC=ISTRM(3,L)
      IR=ISTRM(2,L)
      IL=ISTRM(1,L)
      IF(IBOUND(IC,IR,IL).LE.0) GO TO 615
      BUFF(IC,IR,IL)=BUFF(IC,IR,IL)+STRM(9,L)
  615 CONTINUE
      CALL UBUDSV(KSTP,KPER,STRTXT,ISTCB2,BUFF,NCOL,NROW,NLAY,IOUT)
C                                                                      C
C30-----PRINT STREAMFLOW RATES AND LEAKAGE FOR EACH REACH.             C
  625 IF((ISTCB1.GE.0).OR.(ICBCFL.LE.0)) GO TO 800
      IF(IPTFLG.GT.0) GO TO 800
      IF(ICALC.GT.0) GO TO 700
      WRITE(IOUT,650)
  650 FORMAT(/,12X,'LAYER',6X,'ROW',5X,'COLUMN',5X,'STREAM',4X,
     1'REACH',6X,'FLOW INTO',5X,'FLOW INTO',5X,'FLOW OUT OF'/42X,
     2      'NUMBER',4X,'NUMBER',4X,'STREAM REACH',4X,'AQUIFER',
     3      6X,'STREAM REACH'//)
      DO 690 L=1,NSTREM
      IL=ISTRM(1,L)
      IR=ISTRM(2,L)
      IC=ISTRM(3,L)
      WRITE(IOUT,675)IL,IR,IC,ISTRM(4,L),ISTRM(5,L),
     1     STRM(10,L),STRM(11,L),STRM(9,L)
c  675 FORMAT(1X,5X,5I10,8X,G9.3,5X,G9.3,8X,G9.3)     ! Changed 3/9/06 ERB
  675 FORMAT(1X,5X,5I10,4X,G14.7,1X,G14.7,1X,G14.7)
  690 CONTINUE
      GO TO 800
  700 WRITE(IOUT,710)
  710 FORMAT(/,12X,'LAYER',6X,'ROW',5X,'COLUMN',5X,'STREAM',4X,
     1'REACH',6X,'FLOW INTO',5X,'FLOW INTO',5X,'FLOW OUT OF',5X,
     2'HEAD IN'/42X,      'NUMBER',4X,'NUMBER',4X,'STREAM REACH',
     3 4X,'AQUIFER',6X,'STREAM REACH',5X,'STREAM'//)
      DO 750 L=1,NSTREM
      IL=ISTRM(1,L)
      IR=ISTRM(2,L)
      IC=ISTRM(3,L)
      WRITE(IOUT,775)IL,IR,IC,ISTRM(4,L),ISTRM(5,L),
     1     STRM(10,L),STRM(11,L),STRM(9,L),STRM(2,L)
c  775 FORMAT(1X,5X,5I10,8X,G9.3,5X,G9.3,7X,G9.3,4X,F9.2) ! Changed 3/9/06 ERB
  775 FORMAT(1X,5X,5I10,4X,G14.7,1X,G14.7,1X,G14.7,2X,F9.2)
  750 CONTINUE
  800 CONTINUE
C                                                                      C
C31-----RETURN.                                                        C
      RETURN
      END
      SUBROUTINE SGWF1STR6R(NLST,MXSTRM,STRM,ISTRM,LSTBEG,IN,
     1          IOUT,NCOL,NROW,NLAY,IRDFLG)
C-----VERSION 15APRIL1998 SGWF1STR6R
C                                                                      C
C     *****************************************************************C
C     READ STRM AND ISTRM
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION STRM(11,MXSTRM),ISTRM(5,MXSTRM)
C     ------------------------------------------------------------------
C
C5------READ AND PRINT DATA FOR EACH STREAM CELL.
      IF(IRDFLG.EQ.0) WRITE(IOUT,4)
    4 FORMAT(/,4X,'LAYER   ROW    COL    SEGMENT   REACH   STREAMFLOW',
     16X,'STREAM    STREAMBED     STREAMBED BOT  STREAMBED TOP',/27X,
     2'NUMBER   NUMBER                   STAGE   CONDUCTANCE',6X,
     3'ELEVATION      ELEVATION',/3X,110('-'))
      N=NLST+LSTBEG-1
      DO 250 II=LSTBEG,N
      READ(IN,5)K,I,J,ISTRM(4,II),ISTRM(5,II),STRM(1,II),STRM(2,II),
     1STRM(3,II),STRM(4,II),STRM(5,II)
    5 FORMAT(5I5,F15.0,4F10.0)
      IF(IRDFLG.EQ.0) WRITE(IOUT,6)K,I,J,ISTRM(4,II),
     1ISTRM(5,II),STRM(1,II),STRM(2,II),STRM(3,II),STRM(4,II),STRM(5,II)
    6 FORMAT(1X,1X,I6,2I7,2I9,7X,G11.4,G12.4,G11.4,4X,2G13.4)
      ISTRM(1,II)=K
      ISTRM(2,II)=I
      ISTRM(3,II)=J
C
C  Check for illegal grid location
      IF(K.LT.1 .OR. K.GT.NLAY) THEN
         WRITE(IOUT,*) ' Layer number in list is outside of the grid'
         CALL USTOP(' ')
      END IF
      IF(I.LT.1 .OR. I.GT.NROW) THEN
         WRITE(IOUT,*) ' Row number in list is outside of the grid'
         CALL USTOP(' ')
      END IF
      IF(J.LT.1 .OR. J.GT.NCOL) THEN
         WRITE(IOUT,*) ' Column number in list is outside of the grid'
         CALL USTOP(' ')
      END IF
  250 CONTINUE
C
      RETURN
      END
