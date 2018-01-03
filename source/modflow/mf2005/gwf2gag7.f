C $Id: gwf2gag7.f 3234 2007-03-24 01:44:52Z deprudic $      
      MODULE GWFGAGMODULE
        INTEGER,SAVE,POINTER  ::NUMGAGE
        INTEGER,SAVE,  DIMENSION(:,:),  POINTER :: IGGLST
      TYPE GWFGAGTYPE
        INTEGER,     POINTER  ::NUMGAGE
        INTEGER,       DIMENSION(:,:),  POINTER :: IGGLST
      END TYPE
      TYPE(GWFGAGTYPE), SAVE:: GWFGAGDAT(10)
      END MODULE GWFGAGMODULE
C
C-------SUBROUTINE GWF2GAG7AR
      SUBROUTINE GWF2GAG7AR(INGAGE,IUNITSFR,IUNITLAK,IGRID)
C     ******************************************************************
C     READ FIRST GAGING STATION PACKAGE RECORD
C     ALLOCATE ARRAY STORAGE FOR GAGING STATION PACKAGE
C     READ GAGING STATION LOCATIONS
C     ******************************************************************
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IOUT
      USE GWFGAGMODULE
C     ------------------------------------------------------------------
      ALLOCATE (NUMGAGE)
      NUMGAGE = 0
C
C1------TURN OFF GAGE PACKAGE IF ACTIVE AND 
C         STREAMS AND LAKES ARE INACTIVE.
      IF(IUNITSFR.LE.0.AND.IUNITLAK.LE.0) THEN
         WRITE(IOUT,1)
    1    FORMAT(' GAGE PACKAGE ACTIVE EVEN THOUGH SFR AND LAK ',
     &        'PACKAGES ARE INACTIVE: GAGE PACKAGE IS BEING TURNED OFF')
         INGAGE=0
         RETURN
      END IF
C
C2------READ NUMBER OF GAGES, TURN OFF GAGE PACKAGE IF NUMBER OF GAGES
C         IS LESS THAN OR EQUAL TO ZERO.
      READ(INGAGE,*) NUMGAGE
      IF(NUMGAGE.LE.0) THEN
         WRITE(IOUT,2)
    2    FORMAT(' NUMGAGE=0, SO GAGE IS BEING TURNED OFF')
         INGAGE=0
         NUMGAGE=0
         RETURN
      END IF
C
C3------IGGLST ARRAY IS: 
C         (1) SEGMENT (or LAKE) NUMBER;
C         (2) REACH NUMBER (NULL FOR LAKE);
C         (3) UNIT #; and 
C         (4) OUTTYPE
      NGAGESAR = 1
      IF (NUMGAGE.GT.0) NGAGESAR = NUMGAGE
      ALLOCATE (IGGLST(4,NGAGESAR))
      IGGLST = 0
      IF (NUMGAGE.EQ.0) GOTO 999
      IF (NUMGAGE.GT.1.OR.NUMGAGE.LT.1) WRITE (IOUT,140) NUMGAGE
      IF (NUMGAGE.EQ.1) WRITE (IOUT,141) NUMGAGE
C
C4------INITIALIZE GAGE COUNTERS.
         NSG=0
         NLG=0
C
C5------READ THE FIRST RECORD OF LIST.
      DO 135 IOB=1,NUMGAGE
         READ(INGAGE,*) IGGLST(1,IOB)
         BACKSPACE INGAGE
         IF (IGGLST(1,IOB).GT.0) THEN
C
C6------STREAM GAGE.
            NSG=NSG+1
            READ(INGAGE,*) IGGLST(1,IOB),IGGLST(2,IOB),IGGLST(3,IOB),
     *                     IGGLST(4,IOB)
         ELSE
            IF(IGGLST(1,IOB).EQ.0) THEN
               WRITE(IOUT,170)
               CALL USTOP(' ')
            ELSE
C
C7------LAKE GAGE.
               NLG=NLG+1
               READ(INGAGE,*) IGGLST(1,IOB),IGGLST(3,IOB)
               IGGLST(2,IOB)=0
C
C8------CHECK FOR NEGATIVE UNIT NUMBER, WHICH DESIGNATES THAT
C         THAT OUTTYPE FOR A LAKE IS READ.
               IF (IGGLST(3,IOB).LT.0) THEN
                BACKSPACE INGAGE
                READ(INGAGE,*) IGGLST(1,IOB),IGGLST(3,IOB),IGGLST(4,IOB)
               ELSE
                 IGGLST(4,IOB)=0
               END IF
            END IF
         END IF
  135 CONTINUE
C
C9------PRINT STREAM GAGES.
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
C10-----PRINT LAKE GAGES.
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
C11-----FORMATS.
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
C
  999 CALL SGWF2GAG7PSV(IGRID)
C
C12-----RETURN.
      RETURN
      END SUBROUTINE GWF2GAG7AR
C
C-------SUBROUTINE GWF2GAG7RP
      SUBROUTINE GWF2GAG7RP(IUNITGWT,IUNITLAK,IUNITUZF,NSOL,IGRID)
C     ******************************************************************
C     GWF2GAG5RP GAGING STATIONS--WRITE HEADER LINES TO OUTPUT FILES
C                       --DETERMINE & SAVE CROSS-REFERENCE INDEX
C                       --RECORD INITIAL CONDITIONS FOR LAKE GAGES
C     ******************************************************************

      USE GLOBAL,       ONLY:IOUT
      USE GWFGAGMODULE
      USE GWFLAKMODULE, ONLY:NLAKES,STAGES,VOL,CLAKE
      USE GWFSFRMODULE, ONLY:NSTRM,ISTRM,IDIVAR
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      INTEGER NSOL,IGRID,IUNITLAK,IOG,IG,IG2,IG3,IRCH,II,IUNITGWT
      INTEGER  IUNITUZF,LK,DFLAG,ISOL
      REAL DUM,DUMMY
      CHARACTER*1 A
      CHARACTER*2 B
      CHARACTER*7 CONCNAME
      CHARACTER*9 DCTSNAME
      CHARACTER*10 DCCMNAME
      CHARACTER*1256  LFRMAT
C     ------------------------------------------------------------------
C     TEMPORARY ARRAYS
C     ------------------------------------------------------------------
      ALLOCATABLE CONCNAME(:),DCTSNAME(:),DCCMNAME(:),DUMMY(:,:)
C     ------------------------------------------------------------------
C     ALLOCATE TEMPORARY ARRAYS
C     ------------------------------------------------------------------
      ALLOCATE(CONCNAME(NSOL),DCTSNAME(NSOL),DCCMNAME(NSOL))
C     ------------------------------------------------------------------
C
C1------SET POINTERS FOR THE CURRENT GRID.
      CALL SGWF2GAG7PNT(IGRID)
C
      DUM=0.0D0
      IF (IUNITLAK.GT.0) THEN
        ALLOCATE(DUMMY(NLAKES,NSOL))
        DUMMY=0.0
      END IF
C
C2------LOOP OVER GAGING STATIONS.
      DO 10 IOG=1,NUMGAGE
         IG=IGGLST(1,IOG)
         IG3=ABS(IGGLST(3,IOG))
         IF (IG.GT.0) THEN
C
C3------STREAM GAGE; SAVE STREAM REACH INDEX; WRITE HEADER LINES.
            IG2=IGGLST(2,IOG)
            DO 20 IRCH=1,NSTRM
               IF (ISTRM(4,IRCH).EQ.IG.AND.ISTRM(5,IRCH).EQ.IG2) THEN
C
C4------CONVERT REACH NUMBER FROM SEGMENT LIST TO MASTER LIST.
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
C
C5------CHECK IF GAGE STATION IS FOR A DIVERSION (OUTTYPE IS 5).
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
C
C6------TRANSPORT IS OFF.
               IF (IUNITGWT.LE.0) THEN
C
C7------GET VARIABLE OUTTYPE.
                 SELECT CASE (IGGLST(4,IOG))
                   CASE (0)
                     WRITE (IG3,250)
                   CASE (1)
Cdep  Revised output to include precipitation, et, and runoff
                     IF(IUNITUZF.LE.0) THEN
                       WRITE (IG3,255)
                     ELSE
                       WRITE (IG3,256)
                     END IF
                   CASE (2)
                     WRITE (IG3,260)
                   CASE (3)
                     WRITE (IG3,251)
                   CASE (4)
Cdep  Revised output to include precipitation, et, and runoff
                     IF(IUNITUZF.LE.0) THEN
                       WRITE (IG3,265)
                     ELSE
                       WRITE (IG3,266)
                     END IF
                   CASE (5)
                     WRITE (IG3,267)
                   CASE (6)
                     WRITE (IG3,268)
                   CASE (7)
                     WRITE (IG3,269)
                 END SELECT
C
C8------TRANSPORT IS ON.
               ELSE
                 IF(IUNITUZF.GT.0) WRITE (IOUT,296)              
C
C9------GET VARIABLE OUTTYPE.
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
C
C10-----LAKE GAGE; SAVE LAKE INDEX; WRITE HEADER LINES.
            LK=-IG
            IF (IUNITLAK.LT.1) THEN
               WRITE (IOUT,104)
               GO TO 10
            END IF
            IF (LK.GT.NLAKES) THEN
               WRITE (IOUT,105) IOG,IG3
               GO TO 10
            ELSE
               WRITE (IG3,210) IOG,LK
C
C11-----TRANSPORT IS OFF.
               IF (IUNITGWT.LE.0) THEN
C
C12-----GET VARIABLE OUTTYPE.
                 SELECT CASE (IGGLST(4,IOG))
                   CASE (0)
                     WRITE (IG3,305)
                     WRITE (IG3,400) DUM,STAGES(LK),VOL(LK)
                   CASE (1)
                     IF (IUNITUZF.LE.0) THEN
                       WRITE (IG3,306)
Cdep 6/9/2009  added 1 dummy term for time step lake budget error
                       WRITE (IG3,401) DUM,STAGES(LK),VOL(LK),DUM,DUM,
     *                                 DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,
     +                                 DUM
                     ELSE
Cdep 6/9/2009 added 2 dummy terms for lake seepage to unsaturated zone and
Cdep          time step lake budget error
                       WRITE (IG3,310)
                       WRITE (IG3,405) DUM,STAGES(LK),VOL(LK),DUM,DUM,
     *                                 DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,
     *                                 DUM,DUM,DUM
                     END IF
                   CASE (2)
                     WRITE (IG3,307)
                     WRITE (IG3,402) DUM,STAGES(LK),VOL(LK),DUM,DUM,DUM,
     *                               DUM,DUM
                   CASE (3)
                     IF (IUNITUZF.LE.0) THEN
                       WRITE (IG3,308)
                       WRITE (IG3,403) DUM,STAGES(LK),VOL(LK),DUM,DUM,
     *                                 DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,
     *                                 DUM,DUM,DUM,DUM,DUM
                     ELSE
Cdep 4/20/2009 added 1 dummy term for lake seepage to unsaturated zone
                       WRITE (IG3,311)
                       WRITE (IG3,406) DUM,STAGES(LK),VOL(LK),DUM,DUM,
     *                                 DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,
     *                                 DUM,DUM,DUM,DUM,DUM,DUM,DUM

                     END IF
!dep 4/20/2009 New option for printing volumetric flow rates
                   CASE (4)
                     IF (IUNITUZF.LE.0) THEN
                       WRITE (IG3,309)
                       WRITE (IG3,404) DUM,STAGES(LK),VOL(LK),DUM,DUM,
     *                                 DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,
     *                                 DUM,DUM
                     ELSE
!dep 4/20/2009 added 1 dummy term for lake seepage to unsaturated zone
                       WRITE (IG3,312)
                       WRITE (IG3,407) DUM,STAGES(LK),VOL(LK),DUM,DUM,
     *                                 DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,
     *                                 DUM,DUM,DUM,DUM
                     END IF
                 END SELECT
C
C13-----TRANSPORT IS ON.
               ELSE
C
C14-----PREPARE ARRAY OF HEADER NAMES FOR MULTIPLE CONSTITUENTS.
                 IF(IUNITUZF.GT.0) WRITE(IOUT,320)
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
C16-----FORMATS.
 100  FORMAT (/2X,'*** WARNING ***   GAGE ',I3,' NOT LOCATED ON ACTIVE',
     *   ' STREAM REACH',/10X,'NO DATA WILL BE WRITTEN TO UNIT ',I3/)
 104  FORMAT (/2X,'*** WARNING ***   GAGE ',I3,' SPECIFIED, YET LAKES',
     *   ' NOT ACTIVE',/10X,'NO DATA WILL BE WRITTEN TO UNIT ',I3/)
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
 203  FORMAT (1X,'"STREAM SEGMENT ',I3,' IS DIVERTED FROM SEGMENT ',I3,
     *        ' DIVERSION TYPE IS IPRIOR OF ',I3,' "')
 210  FORMAT (1X,'"GAGE No.',I3,':  Lake No. = ',I3,' "')
 240  FORMAT (/2X,'*** ERROR ***   NSOL NEEDED BUT NOT DEFINED IN ',
     *   'GAGE PACKAGE.  PROGRAM TERMINATING.')
C     minor format adjustments below by LFK, July 2006
 250  FORMAT (5X,'"DATA: Time',11X,'Stage',12X,'Flow"')
 251  FORMAT (5X,'"DATA: Time',11X,'Stage',12X,'Flow"')
 255  FORMAT (5X,'"DATA: Time',11X,'Stage',12X,'Flow',
     *           11X,'Depth',11X,'Width',6X,'Midpt-Flow',9X,
     +           'Precip.',14X,'ET',10X,'Runoff"')
 256  FORMAT (5X,'"DATA: Time',11X,'Stage',12X,'Flow',
     *           11X,'Depth',11X,'Width',6X,'Midpt-Flow',9X,
     +           'Precip.',14X,'ET',6X,'SFR-Runoff',6X,
     +           'UZF-Runoff"')
 260  FORMAT (5X,'"DATA: Time',11X,'Stage',12X,'Flow',
     *           5X,'Conductance',8X,'HeadDiff',7X,'Hyd.Grad."')
 265  FORMAT (5X,'"DATA: Time',11X,'Stage',12X,'Flow',
     *           11X,'Depth',11X,'Width',6X,'Midpt-Flow',9X,
     +           'Precip.',14X,'ET',10X,'Runoff',5X,'Conductance',
     *           8X,'HeadDiff',7X,'Hyd.Grad."')
 266  FORMAT (5X,'"DATA: Time',11X,'Stage',12X,'Flow',
     *           11X,'Depth',11X,'Width',6X,'Midpt-Flow',9X,
     +           'Precip.',14X,'ET',6X,'SFR-Runoff',6X,
     +           'UZF-Runoff',5X,'Conductance',8X,'HeadDiff',
     *           7X,'Hyd.Grad. "')
 267  FORMAT (5X,'"DATA: Time',11X,'Stage',7X,
     *           'Max.-Rate',3X,'Rate-Diverted',3X,
     *           'Upstream-Flow "')
Cdep---added option for printing unsaturated flow beneath streams
 268  FORMAT (5X,'"DATA: Time',11X,'Stage',11X,'Depth',9X,
     *           'GW-Head',4X,'Midpt-Flow',7X,'Stream-Loss',8X,
     *           'GW-Rech.',2X,'Chnge-UZ-Stor.',3X,
     *           'Vol.-UZ-Stor."')
Cdep---added option for printing water content in unsaturated zone
 269  FORMAT (5X,'"DATA: Time',11X,'Depth',7X,
     *           'Width-Ave.-Water-Content',5X,
     *           'Cell-Water-Content"')
C     following formats modified by LFK, July 2006:
 270  FORMAT (2X,'"DATA: Time',9X,'Stage',11X,'Flow',
     *           '     Concentration"')
 272  FORMAT (2X,'"DATA: Time',9X,'Stage',11X,'Flow',
     *           '     Concentration ',
     *           'of ',I3,' Solutes "')
  275  FORMAT (2X,'"DATA: Time',9X,'Stage',11X,'Flow',
     *          10X,'Depth',10X,'Width',6X,'Midpt-Flow',7X,
     *          'Precip.',12X,'ET',10X,'Runoff',
     *          '     Concentration"')
 277  FORMAT (2X,'"DATA: Time',9X,'Stage',11X,'Flow',
     *           10X,'Depth',10X,'Width',6X,'Midpt-Flow',7X,
     *           'Precip.',12X,'ET',10X,'Runoff',
     *           '    Concentration ',
     *           'of ',I3,' Solutes "')
 280  FORMAT (2X,'"DATA: Time',9X,'Stage',11X,'Flow',
     *           6X,'Conductance',5X,'HeadDiff',7X,'Hyd.Grad.',
     *           '    Concentration"')
 281  FORMAT (2X,'"DATA: Time',9X,'Stage',11X,'Flow',
     *           '     Concentration      Load "')
 282  FORMAT (2X,'"DATA: Time',9X,'Stage',11X,'Flow',
     *           6X,'Conductance',5X,'HeadDiff',7X,'Hyd.Grad.',
     *           '    Concentration ',
     *           'of ',I3,' Solutes "')
 284  FORMAT (2X,'"DATA: Time',9X,'Stage',11X,'Flow',
     *           '    Concentration  &  Load ',
     *           'of ',I3,' Solutes "')
C285  FORMAT (1X,'" DATA:   Time',8X,'Stage',9X,'Flow',
 285  FORMAT (2X,'"DATA: Time',9X,'Stage',11X,'Flow',
     *           10X,'Depth',10X,'Width',6X,'Midpt-Flow',7X,
     *           'Precip.',12X,'ET',10X,'Runoff',6X,'Conductance',
     *           5X,'HeadDiff',7X,'Hyd.Grad.',
     *           '    Concentration      Load "')
 287  FORMAT (2X,'"DATA: Time',9X,'Stage',11X,'Flow',
     *           10X,'Depth',10X,'Width',6X,'Midpt-Flow',7X,
     *           'Precip.',12X,'ET',10X,'Runoff',6x,'Conductance',
     *           5X,'HeadDiff',7X,'Hyd.Grad.',
     *           '    Concentration  &  Load ',
     *           'of ',I3,' Solutes "')
 290  FORMAT (2X,'"DATA: Time',9X,'Stage',8X,
     *           'Max.-Rate',5X,'Rate-Diverted',3X,
     *           'Upstream-Flow   Concentration',7X,
     *           'Load "')
 292  FORMAT (2X,'"DATA: Time',9X,'Stage',8X,
     *           'Max.-Rate',5X,'Rate-Diverted',3X,
     *           'Upstream-Flow   Concentration & ',
     *           'Load of ',I3,' Solutes "')
C  LFK
 294  FORMAT (1X,'"****Warning: Gage ',I5,' was specified with an ',
     *        'unsaturated flow option beneath stream.'/1x,
     *        'The GWT Process does not support unsaturated flow ',
     *        'beneath streams, no output will be printed to gage.')
 296  FORMAT (1X,'*****WARNING  UZF PACKAGE ACTIVE WITH TRANSPORT ',/1X,
     +        'GWT PROCESS DOES NOT SUPPORT THE UZF PACKAGE',/1X,
     +        'RUNOFF FROM UZF TO GAGED STREAM WILL NOT BE PRINTED')
 305  FORMAT (4X,'"DATA: Time',7X,'Stage(H)',9X,'Volume "')
 306  FORMAT (4X,'"DATA: Time',7X,'Stage(H)',9X,'Volume',8X,'Precip.'
     *,10X,'Evap.',9X,'Runoff',7X,'GW-Inflw',6X,'GW-Outflw',7X,
     * 'SW-Inflw',6X,'SW-Outflw',5X,'Withdrawal',5X,'Lake-Inflx',4X,
     * 'Total-Cond.',4X,'Percent-Err "')
 307  FORMAT (4X,'"DATA: Time',7X,'Stage(H)',9X,'Volume',
     * 7x,'Del-H-TS',7x,'Del-V-TS',6x,'Del-H-Cum',6x,'Del-V-Cum',2x,
     * 'Cum-Prcnt-Err "')
 308  FORMAT (4X,'"DATA: Time',7X,'Stage(H)',9X,'Volume',8X,
     1 'Precip.',10x,'Evap.',9x,'Runoff',7x,'GW-Inflw',6x,'GW-Outflw',
     2 7x,'SW-Inflw',6x,'SW-Outflw',5x,'Withdrawal',5x,'Lake-Inflx',4x,
     * 'Total-Cond.',7x,'Del-H-TS',7x,'Del-V-TS',6x,'Del-H-Cum',6x,
     * 'Del-V-Cum',2x,'Cum-Prcnt-Err "')
Cdep 4/20/2009 revised format to include lake seepage to UZF
 309  FORMAT (4X,'"DATA: Time',7X,'Stage(H)',9X,'Volume',5X,
     * 'Vol.Change',8X,'Precip.',10x,'Evap.',5x,'    Runoff',
     * 7x,'GW-Inflw',6x,'GW-Outflw',7x,'SW-Inflw',6x,'SW-Outflw',5x,
     * 'Withdrawal',5x,'Lake-Inflx',5x,'Total-Cond',4x,'Percent-Err "')
Cdep 4/20/2009 revised format to include lake seepage to UZF
 310  FORMAT (4X,'"DATA: Time',7X,'Stage(H)',9X,'Volume',8X,'Precip.'
     1 ,10x,'Evap.',5x,'LAK-Runoff',5x,'UZF-Runoff',7x,
     2 'GW-Inflw',6x,'GW-Outflw',5x,'LAK-to-UZF',7x,'SW-Inflw',6x,
     * 'SW-Outflw',5x,'Withdrawal',5x,'Lake-Inflx',4x,'Total-Cond.',
     + 4x,'Percent-Err "')
Cdep 4/20/2009 revised format to include lake seepage to UZF
 311  FORMAT (4X,'"DATA: Time',7X,'Stage(H)',9X,'Volume',8X,'Precip.'
     1 ,10x,'Evap.',5x,'LAK-Runoff',5x,'UZF-Runoff',7x,
     2 'GW-Inflw',6x,'GW-Outflw',5x,'LAK-to-UZF',7x,'SW-Inflw',6x,
     * 'SW-Outflw',5x,'Withdrawal',5x,'Lake-Inflx',4x,'Total-Cond.',7x,
     * 'Del-H-TS',7x,'Del-V-TS',6x,'Del-H-Cum',6x,'Del-V-Cum',2x,
     + 'Cum-Prcnt-Err "')
Cdep 4/20/2009 revised format to include lake seepage to UZF
 312  FORMAT (4X,'"DATA: Time',7X,'Stage(H)',9X,'Volume',5X,
     *'Vol.Change',8X,'Precip.',10x,'Evap.',5x,'LAK-Runoff',5x,
     * 'UZF-Runoff',7x,'GW-Inflw',6x,'GW-Outflw',5x,'LAK-to-UZF',7x,
     * 'SW-Inflw',6x,'SW-Outflw',5x,'Withdrawal',5x,'Lake-Inflx',4x,
     * 'Total-Cond.',4x,'Percent-Err "')
 315  FORMAT ('(1X,''"DATA: Time'',9X,''Stage(H)'',7X,''Volume'',2X,'
     *,I2,'A12, '' "'')')
 316  FORMAT ('( 1X,''"DATA: Time'',8X,''Stage(H)'',7X,''Volume'',3X,
     *',I2,'A12,6X,''   Precip'',10x,''Evap.'',9x,''Runoff'',7x,'
     *'GW-Inflw'',6x,''GW-Outflw'',7x,''SW-Inflw'',6x,''SW-Outflw'',6x,
     *''Withdrawal'',5x,''Lake-Inflx'',5x,''Total-Cond.',4x,
     *'Percent-Err  "'')')
 317  FORMAT ('( 1X,''"DATA: Time'',9X,''Stage(H)'',7X,''Volume'',3X,
     *',I2,'A12,7x,''Del-H-TS'',7x,''Del-V-TS     '', ',I2,'A12,5x,
     *''Del-H-Cum'',5x,'' Del-V-Cum    '', ',I2,'A12,3x,'
     *'Cum-Prcnt-Err  "'')')
 318  FORMAT('( 1X,''"DATA: Time'',9X,''Stage(H)'',7X,''Volume'',3X,'
     *,I2,'A12,8X,'' Precip'',10x,''Evap.'',9x,''Runoff'',7x,''GW-Inflw'
     *',6x,''GW-Outflw'',7x,''SW-Inflw'',6x,''SW-Outflw'',5x,
     *'' Withdrawal'',5x,''Lake-Inflx'',5x,''Total-Cond.'',5x,
     *''Del-H-TS'',7x,''Del-V-TS     '', ',I2,'A12,4x,''Del-H-Cum'',
     *4x,''  Del-V-Cum     '', ',I2,'A12,2x'' Cum-Prcnt-Err "'')')
Cdep 4/20/2009 revised format to denote option 4    
 319  FORMAT ('( 1X,''"DATA: Time'',8X,''Stage(H)'',7X,''Volume'',3X,
     *',I2,'A12,5X,'' Vol.Change'',8X,''Precip'',10x,''Evap.'',9x,
     *''Runoff'',8x,''GW-Inflw'',6x,''GW-Outflw'',7x,''SW-Inflw'',6x,
     *''SW-Outflw'',5x,'' Withdrawal'',5x,''Lake-Inflx'',4x,
     *''Total-Cond.    Percent-Err "'')')
 320  FORMAT (1X,'*****WARNING  UZF PACKAGE ACTIVE WITH TRANSPORT ',/1X,
     +        'GWT PROCESS DOES NOT SUPPORT THE UZF PACKAGE',/1X,
     +        'RUNOFF FROM UZF TO GAGED LAKE WILL NOT BE PRINTED')
 400  FORMAT (4X,1PE14.7,1X,0PF14.7,1X,1PE14.7)
 401  FORMAT (4X,1PE14.7,1X,0PF14.7,1X,12(1PE14.7,1X))
 402  FORMAT (4X,1PE14.7,1X,0PF14.7,1X,6(1PE14.7,1X))
 403  FORMAT (4X,1PE14.7,1X,0PF14.7,1X,16(1PE14.7,1X))
Cdep 4/20/2009 added one term to FORMATS 404 and 405
 404  FORMAT (4X,1PE14.7,1X,0PF14.7,1X,13(1PE14.7,1X))
 405  FORMAT (4X,1PE14.7,1X,0PF14.7,1X,14(1PE14.7,1X))
 406  FORMAT (4X,1PE14.7,1X,0PF14.7,1X,18(1PE14.7,1X))
 407  FORMAT (4X,1PE14.7,1X,0PF14.7,1X,15(1PE14.7,1X))
 425  FORMAT ('(4X,1PE14.7,1X,0PF14.7,1X,1PE14.7,1X,',I3,
     +'(1PE14.7,1X))')
 426  FORMAT ('(4X,1PE14.7,1X,0PF13.7,1X,1PE14.7,1X,',I3,
     *'(1PE14.7,1X),11(1PE14.7,1X))')
 427  FORMAT ('(4X,1PE14.7,1X,0PF14.7,1X,1PE14.7,1X,',I3,'(1PE14.7,1X),
     *1PE14.7,1X,1PE14.7,1X,',I3,'(1PE14.7,1X),1PE14.7,1X,1PE14.7,1X,',
     *I3,'(1PE14.7,1X),1PE14.7,1PE14.7)')
 428  FORMAT ('(4X,1PE14.7,1X,0PF14.7,1X,1PE14.7,1X,',I3,'(1PE14.7,1X),
     *10(1PE14.7,1X),1PE14.7,1X,1PE14.7,1X,',I3,'(1PE14.7,1X),1PE14.7,
     *1X,1PE14.7,1X,',I3,'(1PE14.7,1X),1PE14.7)')
 429  FORMAT ('(4X,1PE14.7,1X,0PF13.7,1X,1PE14.7,1X,',I3,
     *'(1PE14.7,1X),12(1PE14.7,1X))')
C
C17-----RELEASE MEMORY.
      DEALLOCATE(CONCNAME,DCTSNAME,DCCMNAME)
      IF (IUNITLAK.GT.0) DEALLOCATE (DUMMY)
C18-----RETURN.
      RETURN
      END SUBROUTINE GWF2GAG7RP
C
C
C SGWF2GAG5LO Lake GAGING STATIONS--RECORD DATA
Cdep 4/20/2009 added Lake seepage to unsaturated flow 
      SUBROUTINE SGWF2GAG7LO(IUNITGWT,IUNITUZF,CLAKE,GAGETM,GWIN,GWOUT,
     2                       SEEP,FLXINL,VOLOLD,CLKOLD,CLAKINIT,NSOL)
C     ******************************************************************
C     WRITE TIME SERIES OUTPUT FOR EACH LAKE GAGE
C     EACH TIME SERIES IS WRITTEN TO A SEPARATE FILE
Cdep  FIXED MISS MATCH OF ARRAYS PASSED FROM GWF2LAK3BD   12/06/2005
C     ******************************************************************
      USE GWFGAGMODULE
      USE GWFBASMODULE, ONLY: DELT
      USE GWFLAKMODULE, ONLY:NLAKES,RNF,VOL,STGNEW,PRECIP,EVAP,
     1                       SURFIN,SURFOT,WITHDRW,SUMCNN,DELH,TDELH,
     2                       VOLINIT,OVRLNDRNF,TSLAKERR,CMLAKERR,DELVOL,
     3                       SEEPUZ     
C     ------------------------------------------------------------------
      DIMENSION CLAKE(NLAKES,NSOL)
cdep 4/20/2009 dimensioned SEEP array to nlakes
      DIMENSION GWIN(NLAKES),GWOUT(NLAKES),FLXINL(NLAKES),
     1          VOLOLD(NLAKES),CLKOLD(NLAKES,NSOL),CLAKINIT(NLAKES,NSOL)
      DOUBLE PRECISION SEEP(NLAKES)
      CHARACTER*1256  LFRMAT
      ALLOCATABLE DELCTS(:,:),DELCCUM(:,:)
      ALLOCATE(DELCTS(NLAKES,NSOL),DELCCUM(NLAKES,NSOL))
C     ------------------------------------------------------------------
C
C1------LOOP OVER LAKE GAGING STATIONS.
      DO 10 IOG=1,NUMGAGE
         IG1=IGGLST(1,IOG)
         IG3=ABS(IGGLST(3,IOG))
         IF (IG1.GT.0) THEN
            GO TO 10
         ELSE
C
C2------LAKE GAGE: WRITE TIME, STAGE, VOLUME, AND 
C         CONCENTRATION OF EACH SOLUTE.
            LK=-IG1
            IF (LK.GT.NLAKES) THEN
               GO TO 10
            ELSE
!dep  all arrays in LAK3 converted to volumetric fluxes 
!dep  compute volumes per time step for printing  (4/19/2009)
            PP = PRECIP(LK)*DELT
            ET = EVAP(LK)*DELT
            RUNF = RNF(LK)*DELT
            SRIN = SURFIN(LK)*DELT
            SROT = SURFOT(LK)*DELT
            WDRW = WITHDRW(LK)*DELT
!dep  added 4/17/2009
            GWFIN = GWIN(LK)*DELT
            GWFOT = GWOUT(LK)*DELT
            UZFRNF = OVRLNDRNF(LK)*DELT
!dep added 4/20/2009
            SEEPUZF = SEEPUZ(LK)*DELT
            VOLRATE = (VOL(LK)-VOLOLD(LK))/DELT 
!dep  FLUXIN is a volumetric rate 4/20/2009
            FLUXIN = FLXINL(LK)/DELT
            DELHTS=DELH(LK)
            DELHCUM=TDELH(LK)
C
C3------TRANSPORT IS OFF.
               IF (IUNITGWT.LE.0) THEN
C
C4------GET VARIABLE OUTTYPE.
Cdep   4/17/2009 fixed output from uzfrunoff and ground inflow and outflow
                 SELECT CASE (IGGLST(4,IOG))
                 CASE (0)
                   WRITE (IG3,300) GAGETM,STGNEW(LK),VOL(LK)
                 CASE (1)
                 IF (IUNITUZF.LE.0) THEN
                   WRITE (IG3,401) GAGETM,STGNEW(LK),VOL(LK),
     *              PP,ET,RUNF,GWFIN,GWFOT,SRIN,SROT,
     *              WDRW,FLXINL(LK),SUMCNN(LK),TSLAKERR(LK)
                 ELSE
Cdep   4/20/2009 added lake seepage to unsaturated zone 
                   WRITE (IG3,405) GAGETM,STGNEW(LK),VOL(LK),
     *              PP,ET,RUNF,UZFRNF,GWFIN,GWFOT,SEEPUZF,
     +              SRIN,SROT,WDRW,FLXINL(LK),SUMCNN(LK),
     +              TSLAKERR(LK)
                 END IF
                 CASE (2)
                   WRITE (IG3,402) GAGETM,STGNEW(LK),VOL(LK),
     *                     DELHTS,DELVOL(LK),DELHCUM,
     *                     VOL(LK)-VOLINIT(LK),CMLAKERR(LK)
                 CASE (3)
                 IF (IUNITUZF.LE.0) THEN
                   WRITE (IG3,403) GAGETM,STGNEW(LK),VOL(LK),PP,ET,RUNF,
     *                     GWFIN,GWFOT,SRIN,SROT,WDRW,FLXINL(LK),
     *                     SUMCNN(LK),DELHTS,DELVOL(LK),DELHCUM,
     *                     VOL(LK)-VOLINIT(LK),CMLAKERR(LK)
                 ELSE
Cdep   4/20/2009 added lake seepage to unsaturated zone 
                   WRITE (IG3,406) GAGETM,STGNEW(LK),VOL(LK),
     *              PP,ET,RUNF,UZFRNF,GWFIN,GWFOT,SEEPUZF,
     *              SRIN,SROT,WDRW,FLXINL(LK),SUMCNN(LK),
     *              DELHTS,DELVOL(LK),DELHCUM,VOL(LK)-VOLINIT(LK),
     +              CMLAKERR(LK)
                 END IF
!dep   New option for printing time series of volumetric rates
                 CASE (4)
                 IF (IUNITUZF.LE.0) THEN
                   WRITE (IG3,404) GAGETM,STGNEW(LK),VOL(LK),VOLRATE,
     *              PRECIP(LK),EVAP(LK),RNF(LK),GWIN(LK),GWOUT(LK),
     *              SURFIN(LK),SURFOT(LK),WITHDRW(LK),FLUXIN,
     *              SUMCNN(LK),TSLAKERR(LK)
                 ELSE
                   WRITE (IG3,407) GAGETM,STGNEW(LK),VOL(LK),VOLRATE,
     *              PRECIP(LK),EVAP(LK),RNF(LK),OVRLNDRNF(LK),GWIN(LK),
     *              GWOUT(LK),SEEPUZ(LK),SURFIN(LK),SURFOT(LK),
     *              WITHDRW(LK),FLUXIN,SUMCNN(LK),TSLAKERR(LK)
                 END IF
                 END SELECT
C
C5------TRANSPORT IS ON.
               ELSE
C
C6------GET VARIABLE OUTTYPE.
Cdep 4/20/2009 changed variable names according to if volumes or 
C       volumetric rates
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
C7------FORMATS
C
Clfk  change formats in following for consistency with p/o for initial conds.
Cdep  added one value to each format for printing percent error
 300  FORMAT (4X,1PE14.7,1X,0PF14.7,1X,1PE14.7)
 401  FORMAT (4X,1PE14.7,1X,0PF14.7,1X,12(1PE14.7,1X))
 402  FORMAT (4X,1PE14.7,1X,0PF14.7,1X,6(1PE14.7,1X))
 403  FORMAT (4X,1PE14.7,1X,0PF14.7,1X,16(1PE14.7,1X))
Cdep 4/20/2009 added one term to formats 404 and 405
 404  FORMAT (4X,1PE14.7,1X,0PF14.7,1X,13(1PE14.7,1X))
 405  FORMAT (4X,1PE14.7,1X,0PF14.7,1X,14(1PE14.7,1X))
 406  FORMAT (4X,1PE14.7,1X,0PF14.7,1X,18(1PE14.7,1X))
 407  FORMAT (4X,1PE14.7,1X,0PF14.7,1X,15(1PE14.7,1X))
 425  FORMAT ('(4X,1PE14.7,1X,0PF14.7,1X,1PE14.7,1X,',I3,
     +'(1PE14.7,1X))')
 426  FORMAT ('(4X,1PE14.7,1X,0PF13.7,1X,1PE14.7,1X,',I3,
     *'(1PE14.7,1X),12(1PE14.7,1X))')
 427  FORMAT ('(4X,1PE14.7,1X,0PF14.7,1X,1PE14.7,1X,',I3,
     *'(1PE14.7,1X),1PE14.7,1X,1PE14.7,1X,',I3,'(1PE14.7,1X),1PE14.7,
     *1X,1PE14.7,1X,',I3,'(1PE14.7,1X),1PE14.7)')
 428  FORMAT ('(4X,1PE14.7,1X,0PF14.7,1X,1PE14.7,1X,',I3,'(1PE14.7,1X),
     *10(1PE14.7,1x),1PE14.7,1X,1PE14.7,1X,',I3,'(1PE14.7,1X),1PE14.7,
     *1x,1PE14.7,1X,',I3,'(1PE14.7,1X),1PE14.7)')
C
C8------RELEASE MEMORY.
      DEALLOCATE(DELCTS,DELCCUM)
C
C9------RETURN.
      RETURN
      END SUBROUTINE SGWF2GAG7LO
C
C SUBROUTINE SGWF2GAG7SO
      SUBROUTINE SGWF2GAG7SO(IUNITGWT,IUNITUZF,GAGETM,COUT,SFRQ,IBD,
     1                       NSOL)
C     ******************************************************************
C     WRITE TIME SERIES OUTPUT FOR EACH STREAM GAGE
C     EACH TIME SERIES IS WRITTEN TO A SEPARATE FILE
C     ******************************************************************
      USE GWFGAGMODULE
      USE GWFSFRMODULE, ONLY:NSTRM,NUMAVE,IDIVAR,STRM,ISEG,SEG,SGOTFLW,
     1                       AVWAT,WAT1,AVDPT
C     ------------------------------------------------------------------

      CHARACTER*50  LFRMAT
      REAL SFRQ(5,NSTRM)
      DIMENSION COUT(NSTRM,NSOL)
      ALLOCATABLE CLOAD(:)
      ALLOCATE(CLOAD(NSOL))
C     ------------------------------------------------------------------
C
C1------LOOP OVER STREAM GAGING STATIONS.
      DO 10 IOG=1,NUMGAGE
         IG1=IGGLST(1,IOG)
         IG3=IGGLST(3,IOG)
         IF (IG1.GT.0) THEN
            II=IGGLST(2,IOG)
C
C2------DETERMINE STREAM DEPTH.
              DEPTH=STRM(7,II)
                IF (ISEG(1,IG1).EQ.0) THEN
                 DEPTH=STRM(15,II)-STRM(3,II)
              END IF
C
C3------COMPUTE DIVERSION RATES IF OUTTYPE IS 5.
              IF(IGGLST(4,IOG).EQ.5) THEN
                   IUPSEG=IDIVAR(1,IG1)
                   UPSTRFLW=STRM(10,II)+SGOTFLW(IUPSEG)
                   IF(IDIVAR(2,IG1).GE.-1) PMXDVRT=SEG(2,IG1)
                   IF(IDIVAR(2,IG1).EQ.-2) PMXDVRT=SEG(2,IG1)*UPSTRFLW
                   IF(IDIVAR(2,IG1).EQ.-3) PMXDVRT=UPSTRFLW-SEG(2,IG1)
              END IF
C
C4------TRANSPORT IS OFF.
               IF (IUNITGWT.LE.0) THEN
C
C5------GET VARIABLE OUTTYPE.
                 SELECT CASE (IGGLST(4,IOG))
                   CASE (0)
                     WRITE (IG3,250) GAGETM,STRM(15,II),STRM(9,II)
                   CASE (1)
                     IF(IUNITUZF.LE.0) THEN
                       WRITE (IG3,255) GAGETM,STRM(15,II),STRM(9,II),
     *                       DEPTH,STRM(5,II),SFRQ(1,II),STRM(14,II),
     +                       STRM(13,II),STRM(12,II)
                     ELSE
                       WRITE (IG3,256) GAGETM,STRM(15,II),STRM(9,II),
     *                       DEPTH,STRM(5,II),SFRQ(1,II),STRM(14,II),
     +                       STRM(13,II),STRM(12,II),STRM(24,II)
                     END IF
                   CASE (2)
                     WRITE (IG3,260) GAGETM,STRM(15,II),STRM(9,II),
     *                    STRM(16,II),STRM(17,II),STRM(18,II)
                   CASE (3)
                     WRITE (IG3,250) GAGETM,STRM(15,II),STRM(9,II)
                   CASE (4)
                     IF(IUNITUZF.LE.0) THEN
                       WRITE (IG3,265) GAGETM,STRM(15,II),STRM(9,II),
     *                       DEPTH,STRM(5,II),SFRQ(1,II),STRM(14,II),
     +                       STRM(13,II),STRM(12,II),
     *                       STRM(16,II),STRM(17,II),STRM(18,II)
                     ELSE
                       WRITE (IG3,266) GAGETM,STRM(15,II),STRM(9,II),
     *                       DEPTH,STRM(5,II),SFRQ(1,II),STRM(14,II),
     +                       STRM(13,II),STRM(12,II),STRM(24,II),
     *                       STRM(16,II),STRM(17,II),STRM(18,II)
                     END IF
C
C6------OUTTYPE 5 IS USED TO PRINT TIME SERIES FOR A DIVERSION.
                   CASE (5)
                     WRITE (IG3,270) GAGETM,STRM(15,II),PMXDVRT,
     *                    STRM(10,II),UPSTRFLW
C
C7------OUTTYPE 6 IS USED TO PRINT TIME SERIES FOR UNSATURATED FLOW.
                   CASE (6)
                     WRITE (IG3,275) GAGETM,STRM(15,II),STRM(7,II),
     *                      STRM(19,II),SFRQ(1,II),STRM(11,II),
     *                      STRM(21,II),STRM(22,II),STRM(23,II)
C
C8------OUTTYPE 7 IS USED TO PRINT WATER CONTENT PROFILES BENEATH 
C         STREAMBED.
                   CASE (7)
                     IF(IBD.NE.0) THEN
                       WRITE (IG3,280) GAGETM
                       DO IL=1,NUMAVE-1
                         WRITE (IG3,285) AVDPT(IL,II),AVWAT(IL,II),
     *                                   WAT1(IL,II)     
                       END DO 
                     END IF                  
                 END SELECT
C
C9------TRANSPORT IS ON.
               ELSE

C
C10-----GET VARIABLE OUTTYPE.
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
Cdep   Revised format to include precip, ET, and specified runoff
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
  5               CONTINUE
                  WRITE (LFRMAT,452) NSOL
                  WRITE (IG3,LFRMAT) GAGETM,STRM(15,II),
     *                      STRM(9,II),
     *                      (COUT(II,ISOL),CLOAD(ISOL),ISOL=1,NSOL)
                 CASE(4)
                  DO 6 ISOL=1,NSOL
                     CLOAD(ISOL)=STRM(9,II)*COUT(II,ISOL)
  6               CONTINUE
                  WRITE (LFRMAT,465) NSOL
Cdep   Revised format to include precip, ET, and specified runoff
                  WRITE (IG3,LFRMAT) GAGETM,STRM(15,II),STRM(9,II),
     *                    DEPTH,STRM(5,II),SFRQ(1,II),STRM(14,II),
     *                    STRM(13,II),STRM(12,II),STRM(16,II),
     *                    STRM(17,II),STRM(18,II),
     *                    (COUT(II,ISOL),CLOAD(ISOL),ISOL=1,NSOL)
C
C11-----OUTTYPE 5 IS USED TO PRINT TIME SERIES FOR A DIVERSION.
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
C12-----FORMATS.
C
 250  FORMAT (4X,1PE14.7,2X,2(1PE14.7,2X))
 255  FORMAT (4X,1PE14.7,2X,8(1PE14.7,2X))
 256  FORMAT (4X,1PE14.7,2X,9(1PE14.7,2X))
 260  FORMAT (4X,1PE14.7,2X,5(1PE14.7,2X))
 265  FORMAT (4X,1PE14.7,2X,4(1PE14.7,2X),7(1PE14.7,2X))
 266  FORMAT (4X,1PE14.7,2X,4(1PE14.7,2X),8(1PE14.7,2X))
 270  FORMAT (4X,1PE14.7,2X,2(1PE14.7,2X),2(1PE14.7,2X))
 275  FORMAT (4X,1PE14.7,2X,5(1PE14.7,2X),3(1PE14.7,2X))
 280  FORMAT (4X,1PE14.7)
 285  FORMAT (20X,1PE14.7,17X,1PE14.7,11X,1PE14.7)
 450  FORMAT ('(4X,1PE14.7,1X,2(1PE14.7,1X),',I3,'1PE14.7,1X))')
 452  FORMAT ('(4X,1PE14.7,1X,2(1PE14.7,1X),',I3,'(2(1PE14.7,1X)))')
 455  FORMAT ('(4X,1PE14.7,1X,8(1PE14.7,1X),',I3,'(1PE14.7,1X))')
 460  FORMAT ('(4X,1PE14.7,1X,5(1PE14.7,1X),',I3,'(1PE14.7,1X))')
 465  FORMAT ('(4X,1PE14.7,1X,11(1PE14.7,1X),',I3,'(2(1PE14.7,1X)))')
 470  FORMAT ('(4X,1PE14.7,1X,4(1PE14.7,2X),',I3,'(2(1E14.7,1X)))')
C
C13-----RELEASE MEMORY.
      DEALLOCATE(CLOAD)
C
C14-----RETURN.
      RETURN
      END SUBROUTINE SGWF2GAG7SO
C
C-------SUBROUTINE GWF2GAG7DA      
      SUBROUTINE GWF2GAG7DA(IGRID)
C  Deallocate GAG data for a grid.
      USE GWFGAGMODULE
      INTEGER IGRID
C
      DEALLOCATE (GWFGAGDAT(IGRID)%NUMGAGE)
      DEALLOCATE (GWFGAGDAT(IGRID)%IGGLST)
C
      END SUBROUTINE GWF2GAG7DA
C
C-------SUBROUTINE SGWF2GAG7PNT
      SUBROUTINE SGWF2GAG7PNT(IGRID)
C  Change GAG data to a different grid.
      USE GWFGAGMODULE
      INTEGER IGRID
C
      NUMGAGE=>GWFGAGDAT(IGRID)%NUMGAGE
      IGGLST=>GWFGAGDAT(IGRID)%IGGLST
C
      END SUBROUTINE SGWF2GAG7PNT
C
C-------SUBROUTINE SGWF2GAG7PSV      
      SUBROUTINE SGWF2GAG7PSV(IGRID)
C  Save GAG data for a grid.
      USE GWFGAGMODULE
C
      GWFGAGDAT(IGRID)%NUMGAGE=>NUMGAGE
      GWFGAGDAT(IGRID)%IGGLST=>IGGLST
C
      END SUBROUTINE SGWF2GAG7PSV
