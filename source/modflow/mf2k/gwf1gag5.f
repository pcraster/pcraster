! Time of File Save by ERB: 3/8/2006 3:07PM
C     revised       DEP  10 MAY 2006
C     revised       DEP  16 May 2005
C     revised:      DEP  21 Apr 2005
C     revised:      DEP  09 Feb 2004 & 22 Mar 2004
C     Last change:  ERB  15 Jan 2003    2:44 pm
C     REVISED:      LFK  07 Aug 2003
C  GAGE5 Gaging Stations
C  1/99
C
C-------SUBROUTINE GWF1GAG5DF
C
Cdep  Added new variables for Unsaturated-Zone Flow Beneath Streams
      SUBROUTINE GWF1GAG5DF(NUMGAGE,LSGAGE,NSTRM,ICSTRM,NLAKES,LKACC7,
     &                      LCSTAG,LSLAKE,NLAKESAR,NSTRMAR,NSS,NSSAR,
     &                      LCIVAR,NUZST,NUMAVE)
C     VERSION  5:CONNECTED TO LAK3 PACKAGE AND MODFLOW-GWT-- May 10,2006
C     ******************************************************************
C     INITIALIZE POINTER VARIABLES USED BY SFR2 TO SUPPORT LAKE3 AND
C     GAGE PACKAGES AND THE GWT PROCESS revised dep for Unsaturated flow 
C     ******************************************************************
      NUMGAGE=0
      LSGAGE=1
C
C     CONNECTION TO SFR2 PACKAGE
Cdep   added Unsaturated-Zone Flow variables for SFR2
        NSTRM=0
        NSTRMAR=1
        NSS=0
        NSSAR=1
        ICSTRM=1
        LCIVAR=1
        NUZST=1
        NUMAVE=1
C
C     CONNECTION TO LAK3 PACKAGE
        NLAKES=0
        NLAKESAR=1
        LKACC7=1
        LCSTAG=1
        LSLAKE=1
      RETURN
      END

C GWF1GAG5ALP ALLOCATE SPACE FOR GAGING STATIONS
C
C     ******************************************************************
      SUBROUTINE GWF1GAG5ALP(INGAGE,ISUMIR,ISUMRX,LSGAGE,NUMGAGE,IOUT,
     *    IUNITSFR,IUNITLAK,LKACC7,LCSTAG,LSLAKE,ICSTRM,LCIVAR)
C
C     ******************************************************************
      IF(IUNITSFR.LE.0.AND.IUNITLAK.LE.0) THEN
         WRITE(IOUT,1)
    1    FORMAT(1X,' GAGE PACKAGE ACTIVE EVEN THOUGH SFR AND LAK3 ',
     &         'PACKAGES ARE INACTIVE: ',
     &         'GAGE PACKAGE IS BEING TURNED OFF')
         INGAGE=0
         LSGAGE=1
         NUMGAGE=0
         RETURN
      END IF
      READ(INGAGE,*) NUMGAGE
      IF(NUMGAGE.LE.0) THEN
         WRITE(IOUT,2)
    2    FORMAT(1X,' NUMGAGE=0, SO GAGE IS BEING TURNED OFF')
         INGAGE=0
         LSGAGE=1
         NUMGAGE=0
         RETURN
      END IF
C
C     ARRAY IS SEGMENT (or LAKE) NUMBER, REACH NUMBER, UNIT#.
      LSGAGE=ISUMIR
      ISUMIR=ISUMIR+NUMGAGE*4
      ISP=NUMGAGE*4
C
C     DEFINE ICSTRM AND NSTRM IF SFR2 NOT ACTIVE.
      IF (IUNITSFR.LE.0) THEN
         ICSTRM=ISUMIR
         ISUMIR=ISUMIR+5   ! ERB
         LCIVAR=ISUMIR
         ISUMIR=ISUMIR+2   ! ERB
         ISP=ISP+7
      END IF
C
C     DEFINE LKACC7,LCSTAG,LSLAKE IF LAK3 NOT ACTIVE.
      ISPRX=0
      IF (IUNITLAK.LE.0) THEN
         LKACC7=ISUMRX
         ISUMRX=ISUMRX+1
         LCSTAG=ISUMRX
         ISUMRX=ISUMRX+1
         LSLAKE=ISUMRX
         ISUMRX=ISUMRX+1
         ISPRX=ISPRX+3
      END IF
C
C     PRINT SPACE USED BY GAGE PACKAGE.
      WRITE (IOUT,101)ISPRX
  101 FORMAT(1X,I10,' ELEMENTS IN RX ARRAY ARE USED BY GAGE')
      WRITE(IOUT,102) ISP
  102 FORMAT(1X,I10,' ELEMENTS IN IR ARRAY ARE USED BY GAGE')
C
      RETURN
      END
C
C
C  GWF1GAG5RPP READ GAGING STATION INPUT FILE
C
C     ******************************************************************
C
      SUBROUTINE GWF1GAG5RPP(IGGLST,NUMGAGE,IOUT,INGAGE)
C
C     ******************************************************************
C
C     READ GAGING STATION LOCATIONS
C     ******************************************************************
C     IGGLST ARRAY IS (1) SEGMENT (or LAKE) NUMBER; (2) REACH NUMBER
C          (null for LAKE); (3) UNIT #; and (4) OUTTYPE
C
      DIMENSION IGGLST(4,NUMGAGE)
C
C     ******************************************************************
C
      IF (NUMGAGE.GT.1.OR.NUMGAGE.LT.1) WRITE (IOUT,140) NUMGAGE
      IF (NUMGAGE.EQ.1) WRITE (IOUT,141) NUMGAGE
C INITIALIZE GAGE COUNTERS
         NSG=0
         NLG=0
C READ THE FIRST RECORD OF LIST
      DO 135 IOB=1,NUMGAGE
         READ(INGAGE,*) IGGLST(1,IOB)
         BACKSPACE INGAGE
         IF (IGGLST(1,IOB).GT.0) THEN
C           for stream:
            NSG=NSG+1
            READ(INGAGE,*) IGGLST(1,IOB),IGGLST(2,IOB),IGGLST(3,IOB),
     *                     IGGLST(4,IOB)
         ELSE
            IF(IGGLST(1,IOB).EQ.0) THEN
               WRITE(IOUT,170)
               CALL USTOP(' ')
            ELSE
C              for lake:
               NLG=NLG+1
               READ(INGAGE,*) IGGLST(1,IOB),IGGLST(3,IOB)
               IGGLST(2,IOB)=0
C              check for negative unit number, which designates OUTTYPE is read
               IF (IGGLST(3,IOB).LT.0) THEN
                BACKSPACE INGAGE
                READ(INGAGE,*) IGGLST(1,IOB),IGGLST(3,IOB),IGGLST(4,IOB)
               ELSE
                 IGGLST(4,IOB)=0
               END IF
            END IF
         END IF
C
  135 CONTINUE
C
C PRINT STREAM GAGES
      IF (NSG.GT.0) THEN
        WRITE (IOUT,*) 'Stream Gages:'
        WRITE (IOUT,150)
        DO 136 IOB=1,NUMGAGE
          IF (IGGLST(1,IOB).GT.0) THEN
            WRITE(IOUT,'(5I8,13X,A40)') IOB,IGGLST(1,IOB),
     *                        IGGLST(2,IOB),IGGLST(3,IOB),IGGLST(4,IOB)
          END IF
  136   CONTINUE
      END IF
C
C PRINT LAKE GAGES
      IF (NLG.GT.0) THEN
        WRITE (IOUT,*) 'Lake Gages:'
        WRITE (IOUT,155)
        DO 137 IOB=1,NUMGAGE
          IF (IGGLST(1,IOB).LT.0) THEN
            IF (IGGLST(3,IOB).LT.0) THEN
              WRITE(IOUT,'(4I8)') IOB,IGGLST(1,IOB),
     *                  IGGLST(3,IOB),IGGLST(4,IOB)
            ELSE
              WRITE(IOUT,'(3I8)') IOB,IGGLST(1,IOB),IGGLST(3,IOB)
            END IF
          END IF
  137   CONTINUE
      END IF
      WRITE (IOUT,180)
C
  140 FORMAT(///I4,' GAGING STATIONS WERE SPECIFIED.',/5X,'(Lakes are ',
     *'identified by a negative value of the Lake Number)',/5X,'RECORDS'
     1,' WILL BE WRITTEN TO SEPARATE OUTPUT FILES REPRESENTED BY ',
     2'FOLLOWING UNIT NUMBERS:',/)
  141 FORMAT(///I4,' GAGING STATION WAS SPECIFIED.',/5X,'(Lakes are ',
     *'identified by a negative value of the Lake Number)',/5X,'RECORDS'
     1,' WILL BE WRITTEN TO SEPARATE OUTPUT FILE REPRESENTED BY ',
     2'FOLLOWING UNIT NUMBER:')
  150 FORMAT('  GAGE #   SEGMENT   REACH   UNIT   OUTTYPE')
  155 FORMAT('  GAGE #    LAKE     UNIT   OUTTYPE')
  170 FORMAT(/'*** ERROR *** Expected non-zero value for segment no.'/
     * 25X,'EXECUTION STOPPING')
 180  FORMAT(///)
      RETURN
      END
C
C
C GWF1GAG5I GAGING STATIONS--WRITE HEADER LINES TO OUTPUT FILES
C                       --DETERMINE & SAVE CROSS-REFERENCE INDEX
C                       --RECORD INITIAL CONDITIONS FOR LAKE GAGES
Cdep  revised to include unsaturated flow in SFR2 may 10, 2006
C     ******************************************************************
C
      SUBROUTINE GWF1GAG5I(IGGLST,NUMGAGE,IOUT,IUNITGWT,STAGES,CLAKE,
     *                  NLAKES,ISTRM,NSTRM,IDIVAR,DUM,NSOL,VOL,
     *                  NLAKESAR,NSTRMAR,NSSAR)
C
C     ******************************************************************
C
      CHARACTER*1 A
      CHARACTER*2 B
      CHARACTER*7 CONCNAME
      CHARACTER*11 DCTSNAME
      CHARACTER*12 DCCMNAME
      CHARACTER*1256  LFRMAT
C TEMPORARY ARRAYS
      ALLOCATABLE CONCNAME(:),DCTSNAME(:),DCCMNAME(:),DUMMY(:,:)
      DIMENSION IGGLST(4,NUMGAGE),VOL(NLAKESAR)
      DIMENSION STAGES(NLAKESAR),CLAKE(NLAKESAR,NSOL),ISTRM(5,NSTRMAR)
      DIMENSION IDIVAR(2,NSSAR)
C ALLOCATE TEMPORARY ARRAYS
      ALLOCATE(CONCNAME(NSOL),DCTSNAME(NSOL),DCCMNAME(NSOL),
     *DUMMY(NLAKESAR,NSOL))
C
C     ******************************************************************
C
      do i = 1,NLAKESAR
         DUMMY(i,1)=0.0
      end do

C  LOOP OVER GAGING STATIONS
      DO 10 IOG=1,NUMGAGE
         IG=IGGLST(1,IOG)
         IG3=ABS(IGGLST(3,IOG))
         IF (IG.GT.0) THEN
C---Stream gage; save stream reach index; write header lines
            IG2=IGGLST(2,IOG)
            DO 20 IRCH=1,NSTRM
               IF (ISTRM(4,IRCH).EQ.IG.AND.ISTRM(5,IRCH).EQ.IG2) THEN
C---              Convert reach no. from segment list to master list
                  IGGLST(2,IOG)=IRCH
                  GO TO 30
               END IF
 20         CONTINUE
            WRITE (IOUT,100) IOG,IG3
            GO TO 10
 30         CONTINUE
            IF (IGGLST(2,IOG).GT.0) THEN
               II=IGGLST(2,IOG)
               WRITE (IG3,200) IOG,ISTRM(1,II),ISTRM(2,II),ISTRM(3,II),
     *                         ISTRM(4,II),ISTRM(5,II)
C---Check if gage station is for a diversion (outtype is 5)
               IF(IGGLST(4,IOG).EQ.5) THEN
                 IF(IDIVAR(1,IG).LE.0.OR.IDIVAR(2,IG).GT.0) THEN
                   WRITE(IG3,201) IOG,IG
                   IGGLST(4,IOG)=0
                 ELSE IF (ISTRM(5,II).NE.1) THEN
                   WRITE(IG3,202) IOG,IG,ISTRM(5,II)
                   IGGLST(4,IOG)=0
                 ELSE
                   WRITE(IG3,203) IG,IDIVAR(1,IG),IDIVAR(2,IG)
                 END IF
               END IF
C              TRANSPORT OFF
               IF (IUNITGWT.LE.0) THEN
C                GET OUTTYPE
Cdep---Added new options for printing unsaturated flow beneath streams
                 SELECT CASE (IGGLST(4,IOG))
                   CASE (0)
                     WRITE (IG3,250)
                   CASE (1)
                     WRITE (IG3,255)
                   CASE (2)
                     WRITE (IG3,260)
                   CASE (3)
                     WRITE (IG3,251)
                   CASE (4)
                     WRITE (IG3,265)
                   CASE (5)
                     WRITE (IG3,267)
                   CASE (6)
                     WRITE (IG3,268)
                   CASE (7)
                     WRITE (IG3,269)
                 END SELECT
               ELSE
C              TRANSPORT ON
C                GET OUTTYPE
                 IF (NSOL.LE.0) THEN
                    WRITE (IOUT,240)
                     CALL USTOP(' ')
                 END IF
                 SELECT CASE (IGGLST(4,IOG))
                 CASE(0)
                  IF (NSOL.EQ.1) WRITE (IG3,270)
                  IF (NSOL.GT.1) WRITE (IG3,272) NSOL
                 CASE(1)
                  IF (NSOL.EQ.1) WRITE (IG3,275)
                  IF (NSOL.GT.1) WRITE (IG3,277) NSOL
                 CASE(2)
                  IF (NSOL.EQ.1) WRITE (IG3,280)
                  IF (NSOL.GT.1) WRITE (IG3,282) NSOL
                 CASE(3)
                  IF (NSOL.EQ.1) WRITE (IG3,281)
                  IF (NSOL.GT.1) WRITE (IG3,284) NSOL
                 CASE(4)
                  IF (NSOL.EQ.1) WRITE (IG3,285)
                  IF (NSOL.GT.1) WRITE (IG3,287) NSOL
                 CASE(5)
                  IF (NSOL.EQ.1) WRITE (IG3,290)
                  IF (NSOL.GT.1) WRITE (IG3,292) NSOL
C LFK:  warning messages added below
                 CASE(6)
                  WRITE (IG3,294) IOG
                 CASE(7)
                  WRITE (IG3,294) IOG
                 CASE(8)
                  WRITE (IG3,294) IOG
                 END SELECT
               END IF
            END IF
         ELSE
C---Lake gage; write header lines; write initial conditions
            LK=-IG
            IF (LK.GT.NLAKES) THEN
               WRITE (IOUT,105) IOG,IG3
               GO TO 10
            ELSE
               WRITE (IG3,210) IOG,LK
C              TRANSPORT OFF
               IF (IUNITGWT.LE.0) THEN
C                GET OUTTYPE
                 SELECT CASE (IGGLST(4,IOG))
                   CASE (0)
                     WRITE (IG3,305)
                     WRITE (IG3,400) DUM,STAGES(LK),VOL(LK)
                   CASE (1)
!dep 7/5/2009  added 1 dummy term for time step lake budget error
                     WRITE (IG3,306)
                     WRITE (IG3,401) DUM,STAGES(LK),VOL(LK),DUM,DUM,DUM,
     *                               DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM
                   CASE (2)
                     WRITE (IG3,307)
                     WRITE (IG3,402) DUM,STAGES(LK),VOL(LK),DUM,DUM,DUM,
     *                               DUM,DUM
                   CASE (3)
                     WRITE (IG3,308)
                     WRITE (IG3,403) DUM,STAGES(LK),VOL(LK),DUM,DUM,DUM,
     *                       DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,
     *                       DUM,DUM
!dep New option for printing volumetric flow rates
                   CASE (4)
                       WRITE (IG3,309)
                       WRITE (IG3,404) DUM,STAGES(LK),VOL(LK),DUM,DUM,
     *                                 DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,
     *                                 DUM,DUM
                 END SELECT
               ELSE
C              TRANSPORT ON
C                Prepare array of header names for multiple constituents
                 DFLAG=0
                 IF(IGGLST(4,IOG).EQ.2.OR.IGGLST(4,IOG).EQ.3) DFLAG=1
                 DO 1000 ISOL=1,NSOL
                   IF (ISOL.LT.10) THEN
                     WRITE(A,'(I1)') ISOL
                     CONCNAME(ISOL)='Conc'//'_0'//A
                     IF(DFLAG.EQ.1) THEN
                       DCTSNAME(ISOL)='Del-C'//'_0'//A//'-TS'
                       DCCMNAME(ISOL)='Del-C'//'_0'//A//'-Cum'
                     END IF
                   ELSE IF (ISOL.GT.9.AND.ISOL.LT.100) THEN
                     WRITE(B,'(I2)') ISOL
                     CONCNAME(ISOL)='Conc'//'_'//B
                     IF(DFLAG.EQ.1) THEN
                       DCTSNAME(ISOL)='Del-C'//'_'//B//'-TS'
                       DCCMNAME(ISOL)='Del-C'//'_'//B//'-Cum'
                     END IF
                   ELSE
                     WRITE(IOUT,*) '***ERROR***  NSOL TOO BIG'
                     CALL USTOP(' ')
                   END IF
 1000            CONTINUE
C                GET OUTTYPE
                 SELECT CASE (IGGLST(4,IOG))
                 CASE(0)
                   WRITE (LFRMAT,315) NSOL
                   WRITE (IG3,LFRMAT) (CONCNAME(ISOL),ISOL=1,NSOL)
                   WRITE (LFRMAT,425) NSOL
                   WRITE (IG3,LFRMAT) DUM,STAGES(LK),VOL(LK),
     *              (CLAKE(LK,ISOL),ISOL=1,NSOL)
                 CASE(1)
!dep  added 1 dum to output for TSLAKERR
                   WRITE (LFRMAT,316) NSOL
                   WRITE (IG3,LFRMAT) (CONCNAME(ISOL),ISOL=1,NSOL)
                   WRITE (LFRMAT,426) NSOL
                   WRITE (IG3,LFRMAT) DUM,STAGES(LK),VOL(LK),
     *              (CLAKE(LK,ISOL),ISOL=1,NSOL),
     *              DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM
                 CASE(2)
                   WRITE (LFRMAT,317) NSOL,NSOL,NSOL
                   WRITE (IG3,LFRMAT) (CONCNAME(ISOL),ISOL=1,NSOL),
     *               (DCTSNAME(ISOL),ISOL=1,NSOL),
     *               (DCCMNAME(ISOL),ISOL=1,NSOL)
                   WRITE (LFRMAT,427) NSOL,NSOL,NSOL
                   WRITE (IG3,LFRMAT) DUM,STAGES(LK),VOL(LK),
     *              (CLAKE(LK,ISOL),ISOL=1,NSOL),
     *              DUM,DUM,(DUMMY(LK,ISOL),ISOL=1,NSOL),
C-LFK     *              DUM,DUM,(DUMMY(LK,ISOL),ISOL=1,NSOL)
     *              DUM,DUM,(DUMMY(LK,ISOL),ISOL=1,NSOL),DUM
                 CASE(3)
                   WRITE (LFRMAT,318) NSOL,NSOL,NSOL
                   WRITE (IG3,LFRMAT) (CONCNAME(ISOL),ISOL=1,NSOL),
     *               (DCTSNAME(ISOL),ISOL=1,NSOL),
     *               (DCCMNAME(ISOL),ISOL=1,NSOL)
                   WRITE (LFRMAT,428) NSOL,NSOL,NSOL
                   WRITE (IG3,LFRMAT) DUM,STAGES(LK),VOL(LK),
     *              (CLAKE(LK,ISOL),ISOL=1,NSOL),
     *              DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,
     *              DUM,DUM,(DUMMY(LK,ISOL),ISOL=1,NSOL),
     *              DUM,DUM,(DUMMY(LK,ISOL),ISOL=1,NSOL),dum
!dep new option for printing volumetric flow rates  7/5/2009
                 CASE(4)
                   WRITE (LFRMAT,319) NSOL
                   WRITE (IG3,LFRMAT) (CONCNAME(ISOL),ISOL=1,NSOL)
                   WRITE (LFRMAT,429) NSOL
                   WRITE (IG3,LFRMAT) DUM,STAGES(LK),VOL(LK),
     *              (CLAKE(LK,ISOL),ISOL=1,NSOL),DUM,
     *              DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM
                 END SELECT
               END IF
            END IF
         END IF
 10   CONTINUE
C
 100  FORMAT (/2X,'*** WARNING ***   GAGE ',I3,' NOT LOCATED ON ACTIVE',
     *   ' STREAM REACH',/10X,'NO DATA WILL BE WRITTEN TO UNIT ',I3/)
 105  FORMAT (/2X,'*** WARNING ***   GAGE ',I3,' NOT LOCATED ON ACTIVE',
     *   ' LAKE',/10X,'NO DATA WILL BE WRITTEN TO UNIT ',I3/)
 200  FORMAT (1X,'"GAGE No.',I3,':  K,I,J Coord. = ',I3,',',I3,',',I3,
     *   ';  STREAM SEGMENT = ',I3,';  REACH = ',I3,' "')
 201  FORMAT (/2X,'*** WARNING ***  GAGE ',I3,' ON STREAM SEGMENT ',I3,
     *   ' NOT A DIVERSION AS THERE IS NO UPSTREAM SEGMENT OR ',
     *   ' DIVERSION TYPE (IPRIOR)',/10X,
     *   ' RESETTING OUTTYPE FROM 5 TO 0')
 202  FORMAT (/2X,'*** WARNING ***  GAGE ',I3,' ON STREAM SEGMENT ',I3,
     *   ' REACH NO. ',I3,' IS NOT LOCATED ON FIRST REACH OF A',
     *   ' DIVERSION',/10X,' RESETTING OUTTYPE FROM 5 TO 0')
 203  FORMAT (/2X,'STREAM SEGMENT ',I3,' IS DIVERTED FROM SEGMENT ',I3,
     *        ' DIVERSION TYPE IS IPRIOR OF ',I3)
 210  FORMAT (1X,'"GAGE No.',I3,':  Lake No. = ',I3,' "')
 240  FORMAT (/2X,'*** ERROR ***   NSOL NEEDED BUT NOT DEFINED IN ',
     *   'GAGE PACKAGE.  PROGRAM TERMINATING.')
C     minor format adjustments below by LFK, July 2006
!dep more minor format adjustments  7/5/2009
C 250  FORMAT (1X,'" DATA:   Time',8X,'Stage',9X,'Flow"')
 250  FORMAT (5X,'"DATA:   Time',11X,'Stage',12X,'Flow"')
 251  FORMAT (5X,'"DATA:   Time',11X,'Stage',12X,'Flow"')
C 255  FORMAT (1X,'" DATA:   Time',8X,'Stage',9X,'Flow',
 255  FORMAT (5X,'"DATA:   Time',11X,'Stage',12X,'Flow',
     *           11X,'Depth',11X,'Width',6X,'Midpt-Flow',9X,
     *           'Precip.',14x,'ET',10x,'Runoff"')
C 260  FORMAT (1X,'" DATA:   Time',8X,'Stage',9X,'Flow',
 260  FORMAT (5X,'"DATA:   Time',11X,'Stage',12X,'Flow',
     *           5X,'Conductance',8X,'HeadDiff',7X,'Hyd.Grad."')
C 265  FORMAT (1X,'" DATA:   Time',8X,'Stage',9X,'Flow',
 265  FORMAT (5X,'"DATA:   Time',11X,'Stage',12X,'Flow',
     *           11X,'Depth',11X,'Width',6X,'Midpt-Flow',9X,
     *           'Precip.',14x,'ET',10X,'Runoff',5X,'Conductance',
     *           8X,'HeadDiff',7X,'Hyd.Grad."')
C 267  FORMAT (1X,'" DATA:   Time',8X,'Stage',5X,
 267  FORMAT (5X,'"DATA:   Time',11X,'Stage',7X,
     *           'Max.-Rate',3X,'Rate-Diverted',3X,
     *           'Upstream-Flow"')
Cdep---added option for printing unsaturated flow beneath streams
C 268  FORMAT (1X,'" DATA:   Time',6X,'Stage',8X,'Depth',7X,
 268  FORMAT (5X,'"DATA:   Time',11X,'Stage',11X,'Depth',9X,
     *           'GW-Head',4X,'Midpt-Flow',7X,'Stream-Loss',8X,
     *           'GW-Rech.',2X,'Chnge-UZ-Stor.',3X,
     *           'Vol.-UZ-Stor."')
Cdep---added option for printing water content in unsaturated zone
 269  FORMAT (1X,'"DATA:   Time',11X,'Depth',7X,
     *           'Width-Avg.-Water-Content',5X,
     *           'Cell-1-Water-Content"')
C     following formats modified by LFK, July 2006:
 270  FORMAT (2X,'"DATA:    Time',9X,'Stage',11X,'Flow',
     *           '     Concentration"')
 272  FORMAT (2X,'"DATA:   Time',9X,'Stage',11X,'Flow',
     *           '     Concentration ',
     *           'of ',I3,' Solutes "')
C275  FORMAT (1X,'" DATA:   Time',8X,'Stage',9X,'Flow',
 275  FORMAT (2X,'"DATA:   Time',9X,'Stage',11X,'Flow',
     *          10X,'Depth',10X,'Width',6X,'Midpt-Flow',7X,
     *          'Precip.',12X,'ET',10X,'Runoff',
     *          '     Concentration"')
 277  FORMAT (2X,'"DATA:   Time',9X,'Stage',11X,'Flow',
     *           10X,'Depth',10X,'Width',6X,'Midpt-Flow',7X
     *           'Precip.',12X,'ET',10X,'Runoff',
     *           '    Concentration ',
     *           'of ',I3,' Solutes "')
C280  FORMAT (1X,'" DATA:   Time',8X,'Stage',9X,'Flow',
 280  FORMAT (2X,'"DATA:   Time',9X,'Stage',11X,'Flow',
     *           6X,'Conductance',5X,'HeadDiff',7X,'Hyd.Grad.',
     *           '    Concentration"')
 281  FORMAT (2X,'"DATA:  Time',9X,'Stage',11X,'Flow',
     *           '     Concentration      Load "')
 282  FORMAT (2X,'"DATA:   Time',9X,'Stage',11X,'Flow',
     *           6X,'Conductance',5X,'HeadDiff',7X,'Hyd.Grad.',
     *           '    Concentration ',
     *           'of ',I3,' Solutes "')
 284  FORMAT (2X,'"DATA:   Time',9X,'Stage',11X,'Flow',
     *           '    Concentration  &  Load ',
     *           'of ',I3,' Solutes "')
C285  FORMAT (1X,'" DATA:   Time',8X,'Stage',9X,'Flow',
 285  FORMAT (2X,'"DATA:   Time',9X,'Stage',11X,'Flow',
     *           10X,'Depth',10X,'Width',6X,'Midpt-Flow',7X,
     *           'Precip.',12X,'ET',10X,'Runoff',6X'Conductance',
     *           5X,'HeadDiff',7X,'Hyd.Grad.',
     *           '    Concentration      Load "')
 287  FORMAT (2X,'"DATA:   Time',9X,'Stage',11X,'Flow',
     *           10X,'Depth',10X,'Width',6X,'Midpt-Flow',7X,
     *           'Precip.',12X,'ET',10X,'Runoff',6x,'Conductance',
     *           5X,'HeadDiff',7X,'Hyd.Grad.',
     *           '    Concentration  &  Load ',
     *           'of ',I3,' Solutes "')
C290  FORMAT (1X,'" DATA:   Time',5X,'Stage',5X,
 290  FORMAT (2X,'"DATA:   Time',9X,'Stage',8X,
     *           'Max.-Rate',5X,'Rate-Diverted',3X,
     *           'Upstream-Flow   Concentration',7X,
     *           'Load "')
 292  FORMAT (2X,'"DATA:   Time',9X,'Stage',8X,
     *           'Max.-Rate',5X,'Rate-Diverted',3X,
     *           'Upstream-Flow   Concentration & ',
     *           'Load of ',I3,' Solutes "')
C  LFK
 294  FORMAT (1X,'"****Warning: Gage ',I5,' was specified with an ',
     *       'unsaturated flow option beneath stream.'/1x,
     *       'The GWT Process does not support unsaturated flow ',
     *       'beneath streams; no output will be printed to gage file.')
C305  FORMAT (1X,'" DATA:   Time',7X,'Stage(H)',5X,'Volume "')
 305  FORMAT (4X,'"DATA:    Time',7X,'Stage(H)',9X,'Volume "')
c markstro
c 306  FORMAT (1X,'"DATA:  Time',6X,'Stage(H)',2X,'Volume',5X,'Precip.',
c     1 5x,'Evap.',5x,'Runoff',4x,'GW-Inflw',3x,'GW-Outflw',2x,'SW-Inflw'
c     2 ,3x,'SW-Outflw',x,'Withdrawal',1x,'Lake-Inflx',x,'Total-Cond "')
C306  FORMAT (2X,'"DATA:   Time',7X,'Stage(H)',3X,'Volume',6X,'Precip.',
 306  FORMAT (4X,'"DATA:    Time',7X,'Stage(H)',9X,'Volume',8X,'Precip.'
     *,10X,'Evap.',9X,'Runoff',7X,'GW-Inflw',6X,'GW-Outflw',7X,
     * 'SW-Inflw',6X,'SW-Outflw',5X,'Withdrawal',5X,'Lake-Inflx',4X,
     * 'Total-Cond.',4X,'Percent-Err "')
c end markstro
 307  FORMAT (4X,'"DATA:    Time',7X,'Stage(H)',9X,'Volume',
C     * 6x,'Del-H-TS',4x,'Del-V-TS',3x,'Del-H-Cum',3x,'Del-V-Cum "')
     * 7x,'Del-H-TS',7x,'Del-V-TS',6x,'Del-H-Cum',6x,'Del-V-Cum',2x,
     * 'Cum-Prcnt-Err "')
c markstro
c 308  FORMAT (1X,'"DATA:    Time',6X,'Stage(H)',2X,'Volume',5X,'Precip.',
c     1 5x,'Evap.',5x,'Runoff',4x,'GW-Inflw',3x,'GW-Outflw',2x,'SW-Inflw'
c     2 ,3x,'SW-Outflw',x,'Withdrawal',1x,'Lake-Inflx',x,'Total-Cond ',
c     * 2x,'Del-H-TS',3x,'Del-V-TS',2x,'Del-H-Cum',2x,'Del-V-Cum "')
C308  FORMAT (2X,'"DATA:    Time',7X,'Stage(H)',3X,'Volume',6X,'Precip.',
 308  FORMAT (4X,'"DATA:    Time',7X,'Stage(H)',9X,'Volume',8X,
C     1 6x,'Evap.',6x,'Runoff',5x,'GW-Inflw',4x,'GW-Outflw',3x,'SW-Inflw'
     1 'Precip.',10x,'Evap.',9x,'Runoff',7x,'GW-Inflw',6x,'GW-Outflw',
     2 7x,'SW-Inflw',6x,'SW-Outflw',5x,'Withdrawal',5x,'Lake-Inflx',4x,
     * 'Total-Cond.',7x,'Del-H-TS',7x,'Del-V-TS',6x,'Del-H-Cum',6x,
     * 'Del-V-Cum',2x,'Cum-Prcnt-Err "')
c end markstro
Cdep 4/20/2009 FORMAT FOR VOLUMETRIC FLOW RATES
 309  FORMAT (4X,'"DATA:    Time',7X,'Stage(H)',9X,'Volume',5X,
     * 'Vol.Change',8X,'Precip.',10x,'Evap.',5x,'    Runoff',
     * 7x,'GW-Inflw',6x,'GW-Outflw',7x,'SW-Inflw',6x,'SW-Outflw',5x,
     * 'Withdrawal',5x,'Lake-Inflx',5x,'Total-Cond',4x,'Percent-Err "')
C 315  FORMAT ('( 2X,''"DATA:   Time'',7X,''Stage(H)'',3X,''Volume'',3X,'
 315  FORMAT ('(1X,''"DATA:    Time'',9X,''Stage(H)'',7X,''Volume'',2X,'
     *,I2,'A12, '' "'')')
C 316  FORMAT ('( 2X,''"DATA:  Time'',7X,''Stage(H)'',3X,''Volume'',3X,'
 316  FORMAT ('( 1X,''"DATA:    Time'',8X,''Stage(H)'',7X,''Volume'',3X,
     *',I2,'A12,6X,''   Precip'',10x,''Evap.'',9x,''Runoff'',7x,'
     *'GW-Inflw'',6x,''GW-Outflw'',7x,''SW-Inflw'',6x,''SW-Outflw'',6x,
     *''Withdrawal'',5x,''Lake-Inflx'',5x,''Total-Cond.',4x,
     *'Percent-Err  "'')')
C 317  FORMAT ('( 2X,''"DATA:   Time'',7X,''Stage(H)'',3X,''Volume'',3X,'
C-LFK
 317  FORMAT ('( 1X,''"DATA:    Time'',9X,''Stage(H)'',7X,''Volume'',3X,
c     *,I2,'A12,7x,''Del-H-TS'',7x,''Del-V-TS'',3x,',I2,'A12,6x,
     *',I2,'A12,7x,''Del-H-TS'',7x,''Del-V-TS     '', ',I2,'A12,5x,
c     *''Del-H-Cum'',6x,''Del-V-Cum'',0x,',I2,'A12,'' "'')')
     *''Del-H-Cum'',5x,'' Del-V-Cum    '', ',I2,'A12,3x,'
     *'Cum-Prcnt-Err  "'')')
C318  FORMAT ('( 2X,''"DATA:   Time'',7X,''Stage(H)'',3X,''Volume'',3X,'
C-LFK
 318  FORMAT('( 1X,''"DATA:    Time'',9X,''Stage(H)'',7X,''Volume'',3X,'
C     *,I2,'A12,8X,''Precip'',7x,''Evap.'',7x,''Runoff'',5x,''GW-Inflw'',
     *,I2,'A12,8X,'' Precip'',10x,''Evap.'',9x,''Runoff'',7x,''GW-Inflw'
     *',6x,''GW-Outflw'',7x,''SW-Inflw'',6x,''SW-Outflw'',5x,
     *'' Withdrawal'',5x,''Lake-Inflx'',5x,''Total-Cond.'',5x,
c     *,0x,',I2,'A12,3x,
     *''Del-H-TS'',7x,''Del-V-TS     '', ',I2,'A12,4x,''Del-H-Cum'',
c     *''Del-H-Cum'',6x,''Del-V-Cum'',0x,',I2,'A12,'' "'')')
     *4x,''  Del-V-Cum     '', ',I2,'A12,2x'' Cum-Prcnt-Err "'')')
Cdep 4/20/2009 revised format to denote option 4    
 319  FORMAT ('( 1X,''"DATA:    Time'',8X,''Stage(H)'',7X,''Volume'',3X,
     *',I2,'A12,5X,'' Vol.Change'',8X,''Precip'',10x,''Evap.'',9x,
     *''Runoff'',8x,''GW-Inflw'',6x,''GW-Outflw'',7x,''SW-Inflw'',6x,
     *''SW-Outflw'',5x,'' Withdrawal'',5x,''Lake-Inflx'',4x,
     *''Total-Cond.    Percent-Err "'')')
cdep  Made initial lake output for format statement 400 consistent with
cdep   subsequent output for transient simulations. June 17, 2006
!dep   revised output formats  7/6/2009
C 400  FORMAT (4X,1PE11.3,0PF11.3,1PE11.3)
!dep 400  FORMAT (4X,1PE12.5,0PF12.5,1X,1PE12.5)
 400  FORMAT (4X,1PE14.7,1X,0PF14.7,1X,1PE14.7)
C-LFK
!dep 401  FORMAT (4X,1PE12.5,0PF12.5,1X,1P11E12.4)
 401  FORMAT (4X,1PE14.7,1X,0PF14.7,1X,12(1PE14.7,1X))
C-LFK
!dep 402  FORMAT (4X,1PE12.5,0PF12.5,1X,1P5E12.4)
 402  FORMAT (4X,1PE14.7,1X,0PF14.7,1X,6(1PE14.7,1X))
C-LFK
!dep 403  FORMAT (4X,1PE12.5,0PF12.5,1X,1P15E12.4)
 403  FORMAT (4X,1PE14.7,1X,0PF14.7,1X,16(1PE14.7,1X))
 404  FORMAT (4X,1PE14.7,1X,0PF14.7,1X,13(1PE14.7,1X))
!dep 425  FORMAT ('(4X,1PE12.5,0PF12.5,1X,1PE12.5,1X,',I3,'(E12.5,1X))')
 425  FORMAT ('(4X,1PE14.7,1X,0PF14.7,1X,1PE14.7,1X,',I3,
     +'(1PE14.7,1X))')
!dep 426  FORMAT ('(4X,1PE12.5,0PF12.5,1X,1PE12.5,1X,',I3,'(E12.5,1X),
!dep     *10E12.5)')
 426  FORMAT ('(4X,1PE14.7,1X,0PF13.7,1X,1PE14.7,1X,',I3,
     *'(1PE14.7,1X),11(1PE14.7,1X))')
!dep 427  FORMAT ('(4X,1PE12.5,0PF12.5,1X,1PE12.5,1X,',I3,'(E12.5,1X),
!dep    *E12.5,X,E12.5,X,',I3,'(E12.5,1X),E12.5,X,E12.5,X,
!dep    *',I3,'(E12.5,1X))')
C-LFK
 427  FORMAT ('(4X,1PE14.7,1X,0PF14.7,1X,1PE14.7,1X,',I3,'(1PE14.7,1X),
     *1PE14.7,1X,1PE14.7,1X,',I3,'(1PE14.7,1X),1PE14.7,1X,1PE14.7,1X,',
     *I3,'(1PE14.7,1X),1PE14.7,1PE14.7)')
!dep 428  FORMAT ('(4X,1PE12.5,0PF12.5,1X,1PE12.5,1X,',I3,'(E12.5,1X),
!dep     *10E12.5,E12.5,X,E12.5,X,',I3,'(E12.5,1X),E12.5,X,E12.5,X,
!dep     *',I3,'(E12.5,1X))')
C-LFK  format for case 3:
 428  FORMAT ('(4X,1PE14.7,1X,0PF14.7,1X,1PE14.7,1X,',I3,'(1PE14.7,1X),
     *10(1PE14.7,1X),1PE14.7,1X,1PE14.7,1X,',I3,'(1PE14.7,1X),1PE14.7,
     *1X,1PE14.7,1X,',I3,'(1PE14.7,1X),1PE14.7)')
!dep new option
c 429  FORMAT ('(4X,1PE14.7,1X,0PF14.7,1X,1PE14.7,1X,',I3,'1X,'
c     *'(1PE14.7,1X),12(1PE14.7,1X)')
 429  FORMAT ('(4X,1PE14.7,1X,0PF13.7,1X,1PE14.7,1X,',I3,
     *'(1PE14.7,1X),12(1PE14.7,1X))')
C
C  RELEASE MEMORY
      DEALLOCATE(CONCNAME,DCTSNAME,DCCMNAME,DUMMY)
      RETURN
      END
C
C
C SGWF1GAG5LO Lake GAGING STATIONS--RECORD DATA (Write output to separate files)
C
C     ******************************************************************
C
!dep  Passed two new variables into lake gage ouput   7/6/2009
!dep  Passed DELH AND TDELH from LAK3BD into gage output 
!dep   in place of STGOLD2 AND STAGES 8/27/2009
      SUBROUTINE SGWF1GAG5LO(IGGLST,NUMGAGE,IUNITGWT,STGNEW,CLAKE,
     *                  NLAKES,GAGETM,NSOL,VOL,
     *                  PRECIP,EVAP,RNF,
     *                  GWIN,GWOUT,SURFIN,SURFOT,
     *                  WITHDRW,FLXINL,SUMCNN,
     *                  DELH,VOLOLD,TDELH,VOLINIT,CLKOLD,CLAKINIT,
c-lfk     *                  STGOLD2,VOLOLD,STAGES,VOLINIT,CLKOLD,CLAKINIT,
     *                  DELVOL,TSLAKERR,CMLAKERR,DELT,ISS)
C
C     ******************************************************************
C
!dep  made PRECIP and EVAP arrays double precision--2/14/2007
      CHARACTER*1256  LFRMAT
      DIMENSION IGGLST(4,NUMGAGE),VOL(NLAKES)
!dep      DIMENSION STGNEW(NLAKES),CLAKE(NLAKES,NSOL)
      DIMENSION CLAKE(NLAKES,NSOL)
      DIMENSION RNF(NLAKES),
     * GWIN(NLAKES),GWOUT(NLAKES),SURFIN(NLAKES),SURFOT(NLAKES),
     * FLXINL(NLAKES),SUMCNN(NLAKES),
     * VOLOLD(NLAKES),VOLINIT(NLAKES),DELH(NLAKES),TDELH(NLAKES),
c-LFK     * STGOLD2(NLAKES),VOLOLD(NLAKES),STAGES(NLAKES),VOLINIT(NLAKES),
     * CLKOLD(NLAKES,NSOL),CLAKINIT(NLAKES,NSOL)
!dep    dimension delvol,tslkaerr, anc cmlakerr  (7/6/2009)
      DIMENSION DELVOL(NLAKES),TSLAKERR(NLAKES),CMLAKERR(NLAKES)
      DOUBLE PRECISION PRECIP(NLAKES),EVAP(NLAKES),WITHDRW(NLAKES)
      DOUBLE PRECISION STGNEW(NLAKES)
      ALLOCATABLE DELCTS(:,:),DELCCUM(:,:)
      ALLOCATE(DELCTS(NLAKES,NSOL),DELCCUM(NLAKES,NSOL))
C
C     ******************************************************************
C
C  LOOP OVER GAGING STATIONS
      DO 10 IOG=1,NUMGAGE
         IG1=IGGLST(1,IOG)
         IG3=ABS(IGGLST(3,IOG))
         IF (IG1.GT.0) THEN
            GO TO 10
         ELSE
C---Lake gage: write time, stage, volume, concentration of each solute
            LK=-IG1
            IF (LK.GT.NLAKES) THEN
               GO TO 10
            ELSE
!dep  all arrays in LAK3 converted to volumetric fluxes 
!dep  compute volumes per time step for printing  (7/6/2009)
            PP = PRECIP(LK)*DELT
            ET = EVAP(LK)*DELT
            RUNF = RNF(LK)*DELT
            SRIN = SURFIN(LK)*DELT
            SROT = SURFOT(LK)*DELT
            WDRW = WITHDRW(LK)*DELT
            GWFIN = GWIN(LK)*DELT
            GWFOT = GWOUT(LK)*DELT
            VOLRATE = (VOL(LK)-VOLOLD(LK))/DELT
            DELTAVOL=  VOL(LK)-VOLOLD(LK) 
!dep  FLUXIN is a volumetric rate
            FLUXIN = FLXINL(LK)/DELT
            DELHTS=DELH(LK)
            DELHCUM=TDELH(LK)
c-LFK            DELHTS=STGNEW(LK)-STGOLD2(LK)
c-LFK            DELHCUM=STGNEW(LK)-STAGES(LK)
            IF (ISS.NE.0) THEN
               DELHTS=0.0
               DELHCUM=0.0
            END IF
C
C              TRANSPORT OFF
               IF (IUNITGWT.LE.0) THEN
C                GET OUTTYPE
                 SELECT CASE (IGGLST(4,IOG))
                 CASE (0)
                   WRITE (IG3,300) GAGETM,STGNEW(LK),VOL(LK)
                 CASE (1)
                   WRITE (IG3,401) GAGETM,STGNEW(LK),VOL(LK),PP,ET,
     *                             RUNF,GWFIN,GWFOT,SRIN,SROT,WDRW,
     *                             FLXINL(LK),SUMCNN(LK),TSLAKERR(LK)
                 CASE (2)
                   WRITE (IG3,402) GAGETM,STGNEW(LK),VOL(LK),
     *                     DELHTS,DELVOL(LK),
     *                     DELHCUM,VOL(LK)-VOLINIT(LK),
     *                     CMLAKERR(LK)
                 CASE (3)
                   WRITE (IG3,403) GAGETM,STGNEW(LK),VOL(LK),PP,ET,RUNF,
     *                     GWFIN,GWFOT,SRIN,SROT,WDRW,FLXINL(LK),
     *                     SUMCNN(LK),DELHTS,
     *                     DELVOL(LK),DELHCUM,
     *                     VOL(LK)-VOLINIT(LK),CMLAKERR(LK)
!dep   New option for printing time series of volumetric rates  
                 CASE (4)
                   WRITE (IG3,404) GAGETM,STGNEW(LK),VOL(LK),VOLRATE,
     *                     PRECIP(LK),EVAP(LK),RNF(LK),GWIN(LK),
     *                     GWOUT(LK),SURFIN(LK),SURFOT(LK),WITHDRW(LK),
     *                     FLUXIN,SUMCNN(LK),TSLAKERR(LK)
                 END SELECT
               ELSE
C              TRANSPORT ON
                 SELECT CASE (IGGLST(4,IOG))
                 CASE (0)
                   WRITE (LFRMAT,425) NSOL
                   WRITE (IG3,LFRMAT) GAGETM,STGNEW(LK),VOL(LK),
     *                             (CLAKE(LK,ISOL),ISOL=1,NSOL)
                 CASE (1)
                   WRITE (LFRMAT,426) NSOL
                   WRITE (IG3,LFRMAT) GAGETM,STGNEW(LK),VOL(LK),
     *              (CLAKE(LK,ISOL),ISOL=1,NSOL),PP,ET,RUNF,GWFIN,
     *              GWFOT,SRIN,SROT,WDRW,FLXINL(LK),SUMCNN(LK),
     *              TSLAKERR(LK)
                 CASE (2)
                   DO 744 ISOL=1,NSOL
                     DELCTS(LK,ISOL)=CLAKE(LK,ISOL)-CLKOLD(LK,ISOL)
                     DELCCUM(LK,ISOL)=CLAKE(LK,ISOL)-CLAKINIT(LK,ISOL)
  744              CONTINUE
                   WRITE (LFRMAT,427) NSOL,NSOL,NSOL
                   WRITE (IG3,LFRMAT) GAGETM,STGNEW(LK),VOL(LK),
     *                   (CLAKE(LK,ISOL),ISOL=1,NSOL),
     *                   DELHTS,DELVOL(LK),
     *                   (DELCTS(LK,ISOL),ISOL=1,NSOL),
     *                   DELHCUM,VOL(LK)-VOLINIT(LK),
C-LFK     *                   FLXINL(LK),SUMCNN(LK),
     *                   (DELCCUM(LK,ISOL),ISOL=1,NSOL),CMLAKERR(LK)
                 CASE (3)
                   DO 745 ISOL=1,NSOL
                     DELCTS(LK,ISOL)=CLAKE(LK,ISOL)-CLKOLD(LK,ISOL)
                     DELCCUM(LK,ISOL)=CLAKE(LK,ISOL)-CLAKINIT(LK,ISOL)
  745              CONTINUE
                   WRITE (LFRMAT,428) NSOL,NSOL,NSOL
C-LFK
                   WRITE (IG3,LFRMAT) GAGETM,STGNEW(LK),VOL(LK),
     *                (CLAKE(LK,ISOL),ISOL=1,NSOL),PP,ET,RUNF,GWFIN,
     *                GWFOT,SRIN,SROT,WDRW,FLXINL(LK),SUMCNN(LK),
     *                DELHTS,DELVOL(LK),
     *                (DELCTS(LK,ISOL),ISOL=1,NSOL),
     *                DELHCUM,VOL(LK)-VOLINIT(LK),
     *                (DELCCUM(LK,ISOL),ISOL=1,NSOL),CMLAKERR(LK)
!dep New option for printing times series of volumetric rates (7/6/2009)
                 CASE (4)
                   WRITE (LFRMAT,426) NSOL
C-LFK                   WRITE (IG3,LFRMAT) GAGETM,STGNEW(LK),VOLRATE,
                   WRITE (IG3,LFRMAT) GAGETM,STGNEW(LK),VOL(LK),
     *             (CLAKE(LK,ISOL),ISOL=1,NSOL),VOLRATE,PRECIP(LK),
     *             EVAP(LK),RNF(LK),GWIN(LK),GWOUT(LK),SURFIN(LK),
     *             SURFOT(LK),WITHDRW(LK),FLUXIN,SUMCNN(LK),TSLAKERR(LK)
                 END SELECT
               END IF
            END IF
         END IF
 10   CONTINUE
C
Clfk  change formats in following for consistency with p/o for initial conds.
!dep 300  FORMAT (4X,1PE12.5,0PF12.5,1X,1PE12.5)
 300  FORMAT (4X,1PE14.7,1X,0PF14.7,1X,1PE14.7)
C-LFK
!dep 401  FORMAT (4X,1PE12.5,0PF12.5,1X,1P11E12.4)
 401  FORMAT (4X,1PE14.7,1X,0PF14.7,1X,12(1PE14.7,1X))
C-LFK
!dep 402  FORMAT (4X,1PE12.5,0PF12.5,1X,1P5E12.4)
 402  FORMAT (4X,1PE14.7,1X,0PF14.7,1X,6(1PE14.7,1X))
C-LFK
!dep 403  FORMAT (4X,1PE12.5,0PF12.5,1X,1P15E12.4)
 403  FORMAT (4X,1PE14.7,1X,0PF14.7,1X,16(1PE14.7,1X))
!dep  option 4 added to print volumetric flow rates (7/6/2009)
 404  FORMAT (4X,1PE14.7,1X,0PF14.7,1X,13(1PE14.7,1X))
!dep 425  FORMAT ('(4X,1PE12.5,0PF12.5,1X,1PE12.5,1X,',I3,'(E12.5,1X))')
 425  FORMAT ('(4X,1PE14.7,1X,0PF14.7,1X,1PE14.7,1X,',I3,
     +'(1PE14.7,1X))')
!dep 426  FORMAT ('(4X,1PE12.5,0PF12.5,1X,1PE12.5,1X,',I3,'(E12.5,1X),
!dep     *10E12.5)')
C-LFK
 426  FORMAT ('(4X,1PE14.7,1X,0PF13.7,1X,1PE14.7,1X,',I3,
     *'(1PE14.7,1X),12(1PE14.7,1X))')
C-LFK     *11(1X,1PE14.7)')
!dep 427  FORMAT ('(4X,1PE12.5,0PF12.5,1X,1PE12.5,1X,',I3,'(E12.5,1X),
!dep     *E12.5,X,E12.5,X,',I3,'(E12.5,1X),E12.5,X,E12.5,X,
!dep     *',I3,'(E12.5,1X))')
c 427  FORMAT ('(4X,1PE14.7,1X,0PF14.7,1X,1PE14.7,1X,',I3,
c     *'(1PE14.7,1X),1PE14.7,1X,1PE14.7,1X,',I3,'(1PE14.7,1X),1PE14.7,
c     *1X,1PE14.7,1X,',I3,'(1PE14.7,1X),1PE14.7)')     
 427  FORMAT ('(4X,1PE14.7,1X,0PF14.7,1X,1PE14.7,1X,',I3,
     *'(1PE14.7,1X),1PE14.7,1X,1PE14.7,1X,',I3,'(1PE14.7,1X),1PE14.7,
     *1X,1PE14.7,1X,',I3,'(1PE14.7,1X),1PE14.7)')     
!dep 428  FORMAT ('(4X,1PE12.5,0PF12.5,1X,1PE12.5,1X,',I3,'(E12.5,1X),
!dep     *10E12.5,E12.5,X,E12.5,X,',I3,'(E12.5,1X),E12.5,X,E12.5,X,
!dep     *',I3,'(E12.5,1X))')
C-LFK  format for case 3:
 428  FORMAT ('(4X,1PE14.7,1X,0PF14.7,1X,1PE14.7,1X,',I3,'(1PE14.7,1X),
     *10(1PE14.7,1x),1PE14.7,1X,1PE14.7,1X,',I3,'(1PE14.7,1X),1PE14.7,
     *1x,1PE14.7,1X,',I3,'(1PE14.7,1X),1PE14.7)')
C
C
C  RELEASE MEMORY
      DEALLOCATE(DELCTS,DELCCUM)
      RETURN
      END
C
C
C SGWF1GAG5SO Stream GAGING STATIONS--RECORD DATA (Write output to separate files)
C
Cdep---file revised to include unsaturated flow beneath streams
C     ******************************************************************
C
      SUBROUTINE SGWF1GAG5SO(IGGLST,NUMGAGE,IUNITGWT,STRM,ISEG,
     *                  NSEGDIM,NSTRM,GAGETM,NSOL,COUT,SFRQ,NSS,SEG,
     *                  SGOTFLW,IDIVAR,AVWAT,WAT1,AVDPT,NUZST,NUMAVE,
     *                  IBD )
C
C     ******************************************************************
C
      CHARACTER*50  LFRMAT
      REAL SFRQ(5,NSTRM)
      ALLOCATABLE CLOAD(:)
Cdep   Increased dimensions for SEG arrays dep 4/21/2005
      DIMENSION IGGLST(4,NUMGAGE),ISEG(4,NSEGDIM)
      DIMENSION STRM(24,NSTRM),COUT(NSTRM,NSOL)
      DIMENSION SEG(26,NSS),IDIVAR(2,NSS),SGOTFLW(NSS)
      DIMENSION AVWAT(NUZST,NUMAVE),AVDPT(NUZST,NUMAVE)
      DIMENSION WAT1(NUZST,NUMAVE)
C     ALLOCATE MEMORY
      ALLOCATE(CLOAD(NSOL))
C
C     ******************************************************************
C
C  LOOP OVER GAGING STATIONS
      DO 10 IOG=1,NUMGAGE
         IG1=IGGLST(1,IOG)
         IG3=IGGLST(3,IOG)
         IF (IG1.GT.0) THEN
            II=IGGLST(2,IOG)
C
C        CALCULATE STREAM DEPTH
              DEPTH=STRM(7,II)
                IF (ISEG(1,IG1).EQ.0) THEN
                 DEPTH=STRM(15,II)-STRM(3,II)
              END IF
C       COMPUTE DIVERSION RATES IF OUTTYPE IS 5
              IF(IGGLST(4,IOG).EQ.5) THEN
                   IUPSEG=IDIVAR(1,IG1)
                   UPSTRFLW=STRM(10,II)+SGOTFLW(IUPSEG)
                   IF(IDIVAR(2,IG1).GE.-1) PMXDVRT=SEG(2,IG1)
                   IF(IDIVAR(2,IG1).EQ.-2) PMXDVRT=SEG(2,IG1)*UPSTRFLW
                   IF(IDIVAR(2,IG1).EQ.-3) PMXDVRT=UPSTRFLW-SEG(2,IG1)
              END IF
C
C              TRANSPORT OFF
               IF (IUNITGWT.LE.0) THEN
C                GET OUTTYPE
                 SELECT CASE (IGGLST(4,IOG))
                   CASE (0)
                     WRITE (IG3,250) GAGETM,STRM(15,II),STRM(9,II)
                   CASE (1)
                     WRITE (IG3,255) GAGETM,STRM(15,II),STRM(9,II),
     *                    DEPTH,STRM(5,II),SFRQ(1,II),STRM(14,II),
     +                    STRM(13,II),STRM(12,II)
                   CASE (2)
                     WRITE (IG3,260) GAGETM,STRM(15,II),STRM(9,II),
     *                    STRM(16,II),STRM(17,II),STRM(18,II)
                   CASE (3)
                     WRITE (IG3,250) GAGETM,STRM(15,II),STRM(9,II)
                   CASE (4)
                       WRITE (IG3,265) GAGETM,STRM(15,II),STRM(9,II),
     *                       DEPTH,STRM(5,II),SFRQ(1,II),STRM(14,II),
     +                       STRM(13,II),STRM(12,II),STRM(16,II),
     *                       STRM(17,II),STRM(18,II)
C          OUTTYPE 5 IS USED TO PRINT TIME SERIES FOR DIVERSIONS.
                   CASE (5)
                     WRITE (IG3,270) GAGETM,STRM(15,II),PMXDVRT,
     *                    STRM(10,II),UPSTRFLW
Cdep       OUTTYPE 6 IS USED TO PRINT TIME SERIES FOR UNSATURATED FLOW.
                   CASE (6)
                     WRITE (IG3,275) GAGETM,STRM(15,II),STRM(7,II),
     *                      STRM(19,II),SFRQ(1,II),STRM(11,II),
     *                      STRM(21,II),STRM(22,II),STRM(23,II)
Cdep       OUTTYPE 7 IS USED TO PRINT WATER CONTENT PROFILES FOR UNSATURATED FLOW.
                   CASE (7)
                     IF(IBD.NE.0) THEN
                       WRITE (IG3,280) GAGETM
                       DO IL=1,NUMAVE-1
                         WRITE (IG3,285) AVDPT(II,IL),AVWAT(II,IL),
     *                                   WAT1(II,IL)
                       END DO
                     END IF
                 END SELECT
               ELSE
C              TRANSPORT ON
C                GET OUTTYPE
                 IF (NSOL.LE.0) THEN
                     CALL USTOP(' ')
                 END IF
                 SELECT CASE (IGGLST(4,IOG))
                 CASE(0)
                  WRITE (LFRMAT,450) NSOL
                  WRITE (IG3,LFRMAT) GAGETM,STRM(15,II),
     *                          STRM(9,II),(COUT(II,ISOL),ISOL=1,NSOL)
                 CASE(1)
                  WRITE (LFRMAT,455) NSOL
                  WRITE (IG3,LFRMAT) GAGETM,STRM(15,II),STRM(9,II),
     *                    DEPTH,STRM(5,II),SFRQ(1,II),STRM(14,II),
     *                    STRM(13,II),STRM(12,II),
     *                    (COUT(II,ISOL),ISOL=1,NSOL)
                 CASE(2)
                  WRITE (LFRMAT,460) NSOL
                  WRITE (IG3,LFRMAT) GAGETM,STRM(15,II),STRM(9,II),
     *                    STRM(16,II),STRM(17,II),STRM(18,II),
     *                          (COUT(II,ISOL),ISOL=1,NSOL)
                 CASE(3)
                  DO 5 ISOL=1,NSOL
                     CLOAD(ISOL)=STRM(9,II)*COUT(II,ISOL)
    5             CONTINUE
                  WRITE (LFRMAT,452) NSOL
                  WRITE (IG3,LFRMAT) GAGETM,STRM(15,II),
     *                      STRM(9,II),
     *                      (COUT(II,ISOL),CLOAD(ISOL),ISOL=1,NSOL)
                 CASE(4)
                  DO 6 ISOL=1,NSOL
                     CLOAD(ISOL)=STRM(9,II)*COUT(II,ISOL)
    6             CONTINUE
                  WRITE (LFRMAT,465) NSOL
                  WRITE (IG3,LFRMAT) GAGETM,STRM(15,II),STRM(9,II),
     *                    DEPTH,STRM(5,II),SFRQ(1,II),STRM(14,II),
     +                    STRM(13,II),STRM(12,II),
     *                    STRM(16,II),STRM(17,II),STRM(18,II),
     *                    (COUT(II,ISOL),CLOAD(ISOL),ISOL=1,NSOL)
C              OUTTYPE 5 IS USED TO PRINT TIME SERIES OF DIVERSIONS
                   CASE (5)
                 DO 7 ISOL=1,NSOL
                     CLOAD(ISOL)=STRM(9,II)*COUT(II,ISOL)
    7               CONTINUE
                  WRITE (LFRMAT,470) NSOL
                  WRITE (IG3,LFRMAT) GAGETM,STRM(15,II),PMXDVRT,
     *                    STRM(10,II),UPSTRFLW,
     *                    (COUT(II,ISOL),CLOAD(ISOL),ISOL=1,NSOL)
                 END SELECT
               END IF
         ELSE
            GO TO 10
         END IF
 10   CONTINUE
C
!dep 250  FORMAT (4X,1PE11.4,2X,2(E11.4,2X))
 250  FORMAT (4X,1PE14.7,2X,2(1PE14.7,2X))
!dep 255  FORMAT (4X,1PE11.4,2X,5(E11.4,2X))
 255  FORMAT (4X,1PE14.7,2X,8(1PE14.7,2X))
!dep 260  FORMAT (4X,1PE11.4,2X,5(E11.4,2X))
 260  FORMAT (4X,1PE14.7,2X,5(1PE14.7,2X))
!dep 265  FORMAT (4X,1PE11.4,3X,4(E11.4,2X),2X,4(E11.4,2X))
 265  FORMAT (4X,1PE14.7,2X,4(1PE14.7,2X),7(1PE14.7,2X))
!dep 270  FORMAT (4X,1PE11.4,2X,2(E11.4,3X),4X,2(E11.4,4X))
 270  FORMAT (4X,1PE14.7,2X,2(1PE14.7,2X),2(1PE14.7,2X))
!dep 275  FORMAT (4X,1PE11.4,2X,5(E11.4,2X),3(E11.4,5X))
 275  FORMAT (4X,1PE14.7,2X,5(1PE14.7,2X),3(1PE14.7,2X))
!dep 280  FORMAT (4X,1PE11.4)
 280  FORMAT (4X,1PE14.7)
!dep 285  FORMAT (16X,1PE11.3,8X,E12.4,15X,E12.4)
 285  FORMAT (20X,1PE14.7,17X,1PE14.7,11X,1PE14.7)
!dep 450  FORMAT ('(4X,1PE11.4,1X,2(E11.4,1X),',I3,'E11.3,1X))')
 450  FORMAT ('(4X,1PE14.7,1X,2(1PE14.7,1X),',I3,'(1PE14.7,1X))')
!dep 452  FORMAT ('(4X,1PE11.4,1X,2(E11.4,1X),',I3,'(2(E11.3,1X)))')
 452  FORMAT ('(4X,1PE14.7,1X,2(1PE14.7,1X),',I3,'(2(1PE14.7,1X)))')
!dep 455  FORMAT ('(4X,1PE11.4,1X,5(E11.4,1X),',I3,'(E11.3,1X))')
 455  FORMAT ('(4X,1PE14.7,1X,8(1PE14.7,1X),',I3,'(1PE14.7,1X))') 
!dep 460  FORMAT ('(4X,1PE11.4,1X,5(E11.4,1X),',I3,'(E11.3,1X))')
 460  FORMAT ('(4X,1PE14.7,1X,5(1PE14.7,1X),',I3,'(1PE14.7,1X))')
!dep 465  FORMAT ('(4X,1PE11.4,1X,8(E11.4,1X),',I3,'(2(E11.3,1X)))')
 465  FORMAT ('(4X,1PE14.7,1X,11(1PE14.7,1X),',I3,'(2(1PE14.7,1X)))')
!dep 470  FORMAT ('(4X,1PE11.4,1X,4(E11.4,2X),',I3,'(2(E11.3,1X)))')
 470  FORMAT ('(4X,1PE14.7,1X,4(1PE14.7,2X),',I3,'(2(1E14.7,1X)))')
C
C  RELEASE MEMORY
      DEALLOCATE(CLOAD)
C
      RETURN
      END
