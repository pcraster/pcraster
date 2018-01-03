! Time of File Save by ERB: 3/23/2004 1:57PM
Crgn&dep Revised method of computing lake depth March-August 2009. 
Crgn&dep Previously depth computed using surface area of lake at 
Crgn&dep beginning of time step but the method neglected the change 
Crgn&dep in lake area caused by a change over the time step. 
Crgn&dep This could cause substantial lake mass errors. Added explicit 
Crgn&dep lake stage calculations to determine lake seepage.
Crgn  Made EVAP, PRECIP, SEEP, and SEEP3 double precision Nov. 6, 2006
Cdep  Converted from MODFLOW-2000 to MODFLOW-2005 May 2006, RGN and DEP
Cdep  Lake Package modified by DEP and RGN May 20 through June 29, 2006
Cdep  to compute lake outflow as a function of lake stage inside the
Cdep  FORMULATE MODULE. Lake outflow had been previously computed in the
Cdep  Streamflow-Routing Package. The Streamflow-Routing (sfr7) Package
Cdep  was also modified to remain compatible with the modifications in
Cdep  the Lake Package.
C     Modifications made February and March 21, 2004; DEP
C     Last change:  MLM & LFK  10 Oct 2003;  LFK 21 Jan 2004
C     Previous change:  ERB  13 Sep 2002    9:22 am
C
C
      SUBROUTINE GWF2LAK7AR(IN,IUNITSFR,IUNITGWT,IUNITUZF,NSOL,IGRID)
C
C------USGS VERSION 7.1; JUNE 2006 GWF2LAK7AR; 
C------UPDATED FOR MF-2005, FEBRUARY 6, 2012  
C     ******************************************************************
C     INITIALIZE POINTER VARIABLES USED BY SFR1 TO SUPPORT LAKE3 AND
C     GAGE PACKAGES AND THE GWT PROCESS
C     ******************************************************************
C
      USE GWFLAKMODULE
      USE GLOBAL,       ONLY: IOUT, NCOL, NROW, NLAY, IFREFM, ITRSS,
     +                        NODES, IUNIT       !EDM
      USE GWFSFRMODULE, ONLY: NSS
C
C      ******************************************************************
C      ALLOCATE ARRAY STORAGE FOR LAKES
C      ******************************************************************
C
C      ------------------------------------------------------------------
C      SPECIFICATIONS:
       CHARACTER (LEN=40):: CARD 
       CHARACTER*200 line
C      ------------------------------------------------------------------
Crsr  Allocate lake variables used by SFR even if lakes not active so that
C       argument lists are defined     
      ALLOCATE (NLAKES, NLAKESAR,THETA,LAKUNIT,NSFRLAK,NLKFLWTYP)       !EDM
      ALLOCATE(LKFLOWTYPE(6)) ! POSITION 1: STORAGE; 2: DELVOL; 3: PRECIP; 4: EVAP; 5: RUNOFF; 6: WITHDRAWL
C
C--REINITIALIZE LKFLOWTYPE WITH EACH STRESS PERIOD
      NLKFLWTYP=0
      IF(IUNIT(49).NE.0) THEN
        LKFLOWTYPE(1)='NA'
        LKFLOWTYPE(2)='NA'
        LKFLOWTYPE(3)='NA'
        LKFLOWTYPE(4)='NA'
        LKFLOWTYPE(5)='NA'
        LKFLOWTYPE(6)='NA'
      ENDIF
C
      NLAKES = 0
      LAKUNIT = IN
      NLAKESAR = 1
      THETA = 0.0
      IF (IN.GT.0) THEN
Cdep added SURFDEPTH 3/3/2009
        ALLOCATE (ILKCB, NSSITR, SSCNCR, SURFDEPTH)
        ALLOCATE (MXLKND, LKNODE, ICMX, NCLS, LWRT, NDV, NTRB)
        ALLOCATE (IRDTAB)
C
C1------IDENTIFY PACKAGE AND INITIALIZE LKNODE.
      WRITE(IOUT,1) IN
      LKNODE=0
Cdep  initialize number of iterations and closure criteria to zero.
      DUM = 0.0
      NSSITR = 0
      SSCNCR = 0.0
      SURFDEPTH = 0.0
!
      lloc = 1
      IRDTAB = 0
      NPP = 0
      MXVL = 0
      CALL URDCOM(In, IOUT, line)
! Check for alternate option to specify stage/vol/area tables.
      CALL UPARLSTAL(IN,IOUT,LINE,NPP,MXVL)
      lloc = 1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'TABLEINPUT') THEN
         IRDTAB = 1
         WRITE(IOUT,32)
   32  FORMAT(1X,I10,' Stage, volume and area relationship specified ',
     +                'based on an external tabular input file')
      ELSE
        BACKSPACE IN
        WRITE(IOUT,'(A)') ' Model grid will be used to develop ',
     +                     ' volume and area relationship. '
      END IF
C
C2------READ NLAKES, ILKCB.
C
Cdep  Revised input statement to read THETA,NSSITR,SSCNCR for
Cdep  transient simulations when THETA is negative.
        IF(IFREFM.EQ.0) THEN
           READ(IN,'(2I10)')NLAKES,ILKCB
           IF (ITRSS.LE.0) THEN
              READ(IN,'(F10.2,I10,F10.2)') THETA,NSSITR,SSCNCR
              IF (THETA.LT.0.0) BACKSPACE IN
           ELSE
              READ(IN,'(F10.2)') THETA
              IF (THETA.LT.0.0) BACKSPACE IN
           END IF
        ELSE
           READ(IN,*) NLAKES,ILKCB
           IF (ITRSS.LE.0) THEN
              READ(IN,*) THETA,NSSITR,SSCNCR
              IF(THETA.LT.0.0) BACKSPACE IN
           ELSE
              READ(IN,*) THETA
              IF(THETA.LT.0.0) BACKSPACE IN
           END IF
        END IF
      
Cdep    Set default values for number of iterations and closure criteria
Cdep     for transient simulations when using original version of 
Cdep     LAKE Package.
        IF(THETA.GE.0.0.AND.NSSITR.EQ.0) THEN
          NSSITR=100
          SSCNCR=1.0E-05
        ELSE IF(THETA.LT.0.0)THEN 
          THETA=ABS(THETA) 
          IF(IFREFM.EQ.0) THEN 
Cdep fixed format can't read in exponent notation 
!rsr, old data sets may not have SURFDEPTH, may need to trap this for some compilers 
            READ (IN, '(A)') CARD 
            NUMCHAR = LEN(TRIM(CARD)) 
            IF ( NUMCHAR>30 ) THEN 
              READ(CARD,'(F10.2,I10,2F10.5)') DUM,NSSITR,SSCNCR,
     +                                         SURFDEPTH 
            ELSE 
              READ(CARD,'(F10.2,I10,F10.5)') DUM,NSSITR,SSCNCR 
            ENDIF 
          ELSE 
            READ(IN,*,IOSTAT=IOS) DUM,NSSITR,SSCNCR,SURFDEPTH 
            IF ( IOS.NE.0 ) SURFDEPTH = 0.0 
          END IF
        END IF
Cdep   Add check to reset THETA when > 1 or < 0.5.
        IF(THETA.GT.1.0) THEN
          THETA = 1.0
        ELSE IF(THETA.LT.0.5)THEN
          THETA = 0.0
        END IF
      END IF
C
C
C  SET NLAKES ARRAY VARIABLE TO NLAKES IF NLAKES GREATER THAN 0.
      IF (NLAKES.GT.0) NLAKESAR = NLAKES
      ALLOCATE (VOL(NLAKESAR), STGOLD(NLAKESAR), STGNEW(NLAKESAR))
      ALLOCATE (RUNF(NLAKESAR), RUNOFF(NLAKESAR))
      ALLOCATE(STGOLD2(NLAKESAR))
      ALLOCATE (VOLOLDD(NLAKESAR))
!     ALLOCATE (VOLOLDD(NLAKESAR), VOLOLD(NLAKES), VOLINIT(NLAKES))
      ALLOCATE (STGITER(NLAKESAR))
      ALLOCATE (LAKSEEP(NCOL,NROW))
      STGNEW = 0.0D0
      STGOLD = 0.0D0
      STGOLD2 = 0.0D0
      STGITER = 0.0D0
      VOLOLDD = 0.0D0
      LAKSEEP = 0.0
      RUNF = 0.0D0
      RUNOFF = 0.0D0
Cdep initialized VOLOLD and VOLINIT  6/4/2009 (VOLOLD is single precision)
!     VOLOLD = 0.0
!     VOLINIT = 0.0
      VOL = 0.0
      CALL SGWF2LAK7PSV1(IGRID)
      IF (IN.LT.1) RETURN
C
C Lakes are active
      ALLOCATE (STAGES(NLAKESAR), CLAKE(NLAKESAR,NSOL))
      STAGES = 0.0
      CLAKE = 0.0
C Budget variables for GSFLOW   
      ALLOCATE (TOTGWIN_LAK,TOTGWOT_LAK,TOTDELSTOR_LAK,TOTSTOR_LAK)  
      ALLOCATE (TOTEVAP_LAK,TOTPPT_LAK,TOTRUNF_LAK,TOTWTHDRW_LAK)
      ALLOCATE (TOTSURFIN_LAK,TOTSURFOT_LAK)
      TOTGWIN_LAK = 0.0
      TOTGWOT_LAK = 0.0
      TOTDELSTOR_LAK = 0.0
      TOTSTOR_LAK = 0.0
      TOTEVAP_LAK = 0.0
      TOTPPT_LAK = 0.0
      TOTRUNF_LAK = 0.0
      TOTWTHDRW_LAK = 0.0
      TOTSURFIN_LAK = 0.0
      TOTSURFOT_LAK = 0.0
C
C  VALUE OF MXLKND (NUMBER OF LAKE-AQUIFER INTERFACES) IS AN ESTIMATE.
C    TO SAVE MEMORY, REDUCE ITS SIZE IF APPROPRIATE.
C    IF MXLKND TOO SMALL, ERROR MESSAGE WILL BE PRINTED.
      MXLKND=NCOL*NROW*NLAY/2
      IF (NLAKES.LT.1) THEN
        WRITE(IOUT,2)
        IN=0
        NLAKES = 0
      ELSE
      WRITE(IOUT,5) MXLKND,NLAKES
      IF (ILKCB.GT.0) WRITE(IOUT,7) ILKCB
      IF (ILKCB.LE.0) WRITE(IOUT,9)
Cdep   Write THETA, NSSITR, SSCNCR
      IF (ITRSS.GT.0) THEN
        WRITE(IOUT,22) THETA
        WRITE(IOUT,10) NSSITR, SSCNCR
      ELSE
        WRITE(IOUT,11) THETA, NSSITR, SSCNCR
      END IF
Cdep   Changed default values for NSSITR and SSCNCR and revised
Cdep     print statements using format statement 10.   
Cdep      IF(ITRSS.LE.0.AND.NSSITR.EQ.0) NSSITR = 50
Cdep      IF(ITRSS.LE.0.AND.SSCNCR.EQ.0.0) SSCNCR = 0.01
Cdep      IF(ITRSS.EQ.0) WRITE(IOUT,23) NSSITR, SSCNCR
Cdep      IF(ITRSS.LT.0) WRITE(IOUT,24) NSSITR, SSCNCR
C-lfk  1     FORMAT(/1X,'LAK7 -- LAKE PACKAGE, VERSION 7, 2/06/2012',
1     FORMAT(/1X,'LAK7 -- LAKE PACKAGE, VERSION 7, 1/07/2013',
     1' INPUT READ FROM UNIT',I3)
2       FORMAT(1X,' NUMBER OF LAKES=0, ',
     1              ' SO LAKE PACKAGE IS BEING TURNED OFF')
5     FORMAT(1X,'SPACE ALLOCATION FOR',I7,' GRID CELL FACES ADJACENT TO
     1LAKES'/1X,'MAXIMUM NUMBER OF LAKES IS',I3, ' FOR THIS SIMULATION')
7     FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE RECORDED ON UNIT',I5)
9     FORMAT(1X,'CELL-BY-CELL SEEPAGES WILL NOT BE PRINTED OR SAVED')
Cdep added format statement when starting with transient simulation
  10  FORMAT(//1X,'LAKE PACKAGE HAS BEEN MODIFIED TO ITERATIVELY ',
     1 'SOLVE FOR LAKE STAGE DURING TRANSIENT STRESS PERIODS:',/1X,
     2 'MAXIMUM NUMBER OF ITERATIONS (NSSITR) = ',I5,/1X,
     3 'CLOSURE CRITERIA FOR LAKE STAGE (SSCNCR) = ',1PE13.6,/1X, !gsf
     4 'DEFAULT VALUES FOR TRANSIENT ONLY SIMULATIONS ARE: ',
     5 'NSSITR = 100 AND SSCNCR = 0.0001',/1X,'VALUES OTHER THAN ',
     6 'DEFAULT CAN BE READ BY SPECIFYING A THETA LESS THAN ZERO ',
     7 'THEN ADDING NSSITR AND SSCNCR PER ORIGINAL INSTRUCTIONS.',/1X,
     8 'NEGATIVE THETA MUST BE LESS THAN ZERO BUT NOT MORE THAN ',
     9 'ONE. THETA IS CONVERTED TO A POSITIVE VALUE.',/1X,
     * 'MINIMUM AND MAXIMUM LAKE STAGES FOR TRANSIENT ',
     * 'SIMULATIONS ARE SET TO BOTTOM AND TOP ELEVATIONS USED TO ',
     * 'COMPUTE LAKE VOLUME, RESPECTIVELY.',//)
Cdep added format statement for steady state only simulations.
  11  FORMAT(//1X,'NEWTON ITERATION METHOD FOR COMPUTING LAKE STAGE ',
     1 'DURING STEADY-STATE STRESS PERIODS HAS BEEN MODIFIED:',/1X,
     2 'SPECIFIED THETA OF ',F6.3,' WILL BE AUTOMATICALLY CHANGED TO ',
     3 '1.0 FOR ALL STEADY STATE STRESS PERIODS.',/1X,
     4 'MAXIMUM NUMBER OF STEADY-STATE ITERATIONS (NSSITR) = ',I5,/1X,
     5 'CLOSURE CRITERIA FOR STEADY-STATE LAKE STAGE (SSCNCR) = ',
     6  1PE13.6,//) !gsf
Cdep revised print statement to note that time weighting of theta can
Cdep  vary only between 0.5 and 1 for transient simulations
Cdep   22 FORMAT(/1X,'THETA = ',F10.2,'  METHOD FOR UPDATING LAKE STAGES IN
Cdep     1ITERATIONS OF THE SOLUTION FOR AQUIFER HEADS.'/20X,'0.0 IS EXPLICI
Cdep     2T, 0.5 IS CENTERED, AND 1.0 IS FULLY IMPLICIT.')
   22 FORMAT(/1X,'THETA = ',F6.3,/1X,'THETA IS THE TIME WEIGHTING ',
     *'FACTOR FOR COMPUTING LAKE STAGE DURING TRANSIENT MODFLOW ', 
     *'TIME STEPS AND ITS DEFINITION HAS BEEN MODIFIED.',/1X,'A THETA ',
     *'OF LESS THEN 0.5 IS AUTOMATICALLY SET TO 0 AND LAKE STAGE IS ',
     *'EQUAL TO THE STAGE AT THE END OF THE PREVIOUS TIME STEP. ',/1X,
     *'TRANSIENT SIMULATIONS OF LAKE STAGE WITH THE CURRENT TIME STEP ',
     *'REQUIRES A THETA BETWEEN 0.5 AND 1.0. ',/1X,'VALUES GREATER ',
     *'THAN 1.0 ARE AUTOMATICALLY RESET TO  1.0 AND VALUES LESS ',
     *'THAN 0.5 ARE RESET TO 0.0.',/1X,'A THETA OF 0.5 REPRESENTS THE ',
     *'AVERAGE LAKE STAGE DURING A TIME STEP.',/1X,'A THETA OF 1.0 ',
     *'REPRESENTS THE LAKE STAGE AT THE END OF THE TIME STEP.',//)
Cdep   23 FORMAT(/1X,'STEADY-STATE SOLUTION FOR LAKES.'
Cdep     2/1X,'MAXIMUM NUMBER OF ITERATIONS = ',I4,3X,
Cdep     1'CONVERGENCE CRITERION = ',1PE9.2)
Cdep   24 FORMAT(/1X,'COMBINED STEADY-STATE/TRANSIENT SOLUTION FOR LAKES.'
Cdep     2/1X,'MAXIMUM NUMBER OF ITERATIONS = ',I4,3X,
Cdep     1'CONVERGENCE CRITERION = ',1PE9.2)

        ALLOCATE (ILAKE(5,MXLKND), BEDLAK(MXLKND), CNDFCT(MXLKND))
        ALLOCATE (PRCPLK(NLAKES), EVAPLK(NLAKES), WTHDRW(NLAKES))
        ALLOCATE (RNF(NLAKES), CRNF(NLAKES,NSOL), CUMRNF(NLAKES))
        ALLOCATE (CUMUZF(NLAKES))
        ALLOCATE (ISUB(NLAKES,NLAKES), SILLVT(NLAKES,NLAKES))
        ALLOCATE (IRK(2,NLAKES))
        ALLOCATE (CUMPPT(NLAKES), CUMEVP(NLAKES), CUMGWI(NLAKES))
        ALLOCATE (CUMGWO(NLAKES), CUMSWI(NLAKES), CUMSWO(NLAKES))
        ALLOCATE (CUMWDR(NLAKES), CUMFLX(NLAKES))
        ALLOCATE (CAUG(NLAKES,NSOL), CPPT(NLAKES,NSOL))
        ALLOCATE (CLAKINIT(NLAKESAR,NSOL))
        ALLOCATE (ICS(NLAKES),BOTTMS(NLAKES), BGAREA(NLAKES))
        ALLOCATE (SSMN(NLAKES), SSMX(NLAKES))
        ALLOCATE (LKARR1(NCOL,NROW,NLAY), BDLKN1(NCOL,NROW,NLAY))
        ALLOCATE (EVAP(NLAKES), PRECIP(NLAKES), SEEP(NLAKES),
     +            SEEP3(NLAKES),EVAP3(NLAKES), PRECIP3(NLAKES))
        ALLOCATE (SEEPUZ(NLAKES))
        ALLOCATE (FLWITER(NLAKES),FLWITER3(NLAKES))
        ALLOCATE (SURFA(NLAKES), SURFIN(NLAKES), SURFOT(NLAKES))
        ALLOCATE (SUMCNN(NLAKES), SUMCHN(NLAKES))
        ALLOCATE (NCNCVR(NLAKES), LIMERR(NLAKES), DSRFOT(NLAKES))
Cdep  Allocate arrays that track lake budgets for dry lakes
        ALLOCATE (EVAPO(NLAKES),WITHDRW(NLAKES),FLWIN(NLAKES))
        ALLOCATE (GWRATELIM(NLAKES))
        EVAPO = 0.0
        WITHDRW = 0.0D0
        FLWIN = 0.0
        FLWITER = 0.0D0
        FLWITER3 = 0.0D0
        EVAP = 0.0D0
        PRECIP = 0.0D0
        EVAP3 = 0.0D0
        PRECIP3 = 0.0D0
        IF ( IRDTAB.GT.0 ) THEN
          ALLOCATE(LAKTAB(NLAKES))
        ELSE
          ALLOCATE(LAKTAB(1))
        END IF
        LAKTAB = 0
!rsr    GWRATLIM= 0.0
Cdep  Allocate space for three arrays used in GAGE Package 
C       when Solute Transport is active
        ALLOCATE (XLAKES(NLAKES,1), XLAKINIT(NLAKES,1))
        ALLOCATE (XLKOLD(NLAKES,1))
crsr  Allocate arrays for BD subroutine
        ALLOCATE (LDRY(NODES), FLXINL(NLAKES))
        ALLOCATE (NCNT(NLAKES), NCNST(NLAKES))
        ALLOCATE (SVT(NLAKES), KSUB(NLAKES), STGADJ(NLAKES))
        ALLOCATE (MSUB(NLAKES,NLAKES), MSUB1(NLAKES))
        ALLOCATE (GWIN(NLAKES), GWOUT(NLAKES))
        ALLOCATE (DELH(NLAKES), TDELH(NLAKES))
Cdep   Allocate lake budget error arrays for BD subroutine 6/9/2009
        ALLOCATE (CUMVOL(NLAKES), CMLAKERR(NLAKES))
        ALLOCATE (CUMLKIN(NLAKES), CUMLKOUT(NLAKES))
        ALLOCATE (DELVOL(NLAKES), TSLAKERR(NLAKES))  
Cdep initialized VOLOLD and VOLINIT  6/4/2009 (VOLOLD is single precision)
        ALLOCATE (VOLOLD(NLAKES), VOLINIT(NLAKES))  
        VOLOLD = 0.0
        VOLINIT = 0.0
      END IF
Cdep   ALLOCATE SPACE FOR CONNECTION WITH STREAMS
      IF (IUNITSFR.LE.0) THEN
        NSSAR = 1
      ELSE
        NSSAR = NSS
      END IF
Cdep   ALLOCATE SPACE FOR FLOB ARRAY WHEN TRANSPORT ACTIVE.   
      IF (IUNITGWT.LE.0.AND.IUNIT(49).LE.0) THEN
        MXLKAR = 1
      ELSE
        MXLKAR = MXLKND
      END IF
Cdep    ALLOCATE SPACE FOR OVERLAND FLOW WHEN UNSATURATED FLOW ACTIVE.
! RGN Allocate NUZFAR to nlakes for all cases because of the GAG package 5/28/09
!      IF (IUNITUZF.LE.0) THEN
!       NUZFAR = 1
!      ELSE
        NUZFAR = NLAKESAR
!      END IF

      !rsr, what if NLAKES < 1, sanity check
      IF (NLAKES<1 ) THEN
        print *, 'nlakes dimension problem in lak7', nlakes
        stop
      ENDIF

      ALLOCATE (ITRB(NLAKES,NSSAR), IDIV(NLAKES,NSSAR))
      ALLOCATE (FLOB(MXLKAR))
      ALLOCATE (OVRLNDRNF(NUZFAR), CUMLNDRNF(NUZFAR))
Cdep    ALLOCATE SPACE FOR DEPTHTABLE, AREATABLE, AND VOLUMETABLE
      ALLOCATE (DEPTHTABLE(151,NLAKES), AREATABLE(151,NLAKES))
      ALLOCATE (VOLUMETABLE(151,NLAKES))
C Tributary inflow to lakes for LMT
      ALLOCATE (LAKSFR(NSSAR),ILKSEG(NSSAR),ILKRCH(NSSAR),SWLAK(NSSAR),
     &          DELVOLLAK(NLAKES))
      ITRB = 0
      IDIV = 0
      FLOB = 0.0
      OVRLNDRNF = 0.0
      CUMLNDRNF = 0.0
      CUMUZF = 0.0
      DEPTHTABLE = 0.0D0
      AREATABLE = 0.0D0
      VOLUMETABLE = 0.0D0
Cdep initialized lake budget error arrays  6/9/2009
      CUMVOL = 0.0
      DELVOL = 0.0
      CMLAKERR = 0.0
      TSLAKERR = 0.0
      CUMLKOUT = 0.0
      CUMLKIN = 0.0
C-----SAVE POINTERS FOR GRID AND RETURN
      CALL SGWF2LAK7PSV(IGRID)
C
C11-----RETURN.
      RETURN
      END
C
      SUBROUTINE GWF2LAK7RP(IN,IUNITBCF,IUNITGWT,IUNITLPF,IUNITHUF,
     +                      IUNITSFR,IUNITUZF,KKPER,NSOL,IOUTS,IGRID)
C
C------USGS VERSION 7.1;  JUNE 2006 GWF2LAK7RP
C        REVISED FEBRUARY 6, 2012
C     ******************************************************************
C       READ INPUT DATA FOR THE LAKE PACKAGE.
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFLAKMODULE
      USE GLOBAL,       ONLY: IOUT, NCOL, NROW, NLAY, IFREFM, IBOUND,
     +                        LBOTM, BOTM, DELR, DELC, ISSFLG,IUNIT
C
      IMPLICIT NONE
C
C     USE GWFSFRMODULE, ONLY: NSS
C     ------------------------------------------------------------------
C     FUNCTIONS
C     ------------------------------------------------------------------
      DOUBLE PRECISION VOLTERP
      EXTERNAL VOLTERP
C     ------------------------------------------------------------------
      INTEGER IGRID,ISS,LM,IUNITGWT,IN,ISOL,L1,I,J,K,KK,LK,ITMP,ITMP1,
     &        INC,NSOL,N,I2,K1,K2,K3,K4,I1,IC,IS,JK,NSLMS,
     &        IUNITSFR,LAKEFLG,LAKE,NTYP,J2,KKPER,IOUTS,IUNITNUM,L,
     &        IL,IR,ITYPE,IUNITBCF,IUNITLPF,IUNITHUF,IUNITUPW,
     &        IUNITUZF,IC1,II,IFACE,LID,M
      REAL BOTIJ,TBNC,TBELV,TOPMST,GTSDPH,EVOL,BOTLK
      CHARACTER*24 ANAME(2)
!     CHARACTER*30 LFRMAT
!dep  added STGINIT as double precision
      DOUBLE PRECISION STGINIT      
      DATA ANAME(1)/'           LAKE ID ARRAY'/
      DATA ANAME(2)/'  LAKEBED LEAKANCE ARRAY'/
C
C     ------------------------------------------------------------------
C------SET POINTERS FOR THE CURRENT GRID.
      CALL SGWF2LAK7PNT(IGRID)
C
C1A-----IF MXLKND IS LESS THAN 1, THEN LAKE IS INACTIVE. RETURN.
      IF(MXLKND.LT.1) RETURN
C
C1A1----READ INITIAL CONDITIONS FOR ALL LAKES (ONLY READ ONCE)
      ISS = ISSFLG(KKPER)
      IF (KKPER.EQ.1) THEN
         WRITE (IOUT,19)
         IF(ISS.NE.0) WRITE (IOUT,20)
         IF(ISS.EQ.0) WRITE (IOUT,820)
         IF (IUNITGWT.EQ.0) THEN
            DO 30 LM=1,NLAKES
               IF (IFREFM.EQ.0) THEN
                 IF ( IRDTAB.GT.0 ) THEN
                   IF(ISS.NE.0) READ (IN,'(3F10.4,I5)') STAGES(LM),
     1                                SSMN(LM),SSMX(LM),LAKTAB(LM)
                   IF(ISS.EQ.0) READ (IN,'(F10.4,I5)') STAGES(LM),
     2                                               LAKTAB(LM)
                 ELSE
                   IF(ISS.NE.0) READ (IN,'(3F10.4)') STAGES(LM),
     1              SSMN(LM),SSMX(LM)
                   IF(ISS.EQ.0) READ (IN,'(F10.4)') STAGES(LM)
                 END IF
               ELSE
                 IF ( IRDTAB.GT.0 ) THEN
                   IF(ISS.NE.0) READ (IN,*)STAGES(LM),SSMN(LM),SSMX(LM),
     1                                     LAKTAB(LM)
                   IF(ISS.EQ.0) READ (IN,*) STAGES(LM),LAKTAB(LM)
                 ELSE
                   IF(ISS.NE.0) READ (IN,*) STAGES(LM),SSMN(LM),SSMX(LM)
                   IF(ISS.EQ.0) READ (IN,*) STAGES(LM)
                 END IF
               END IF
            IF(ISS.NE.0) WRITE (IOUT,22) LM,STAGES(LM),SSMN(LM),SSMX(LM)
            IF(ISS.EQ.0) WRITE (IOUT,22) LM,STAGES(LM)
 30         CONTINUE
         ELSE
            WRITE (IOUTS,21) NSOL
!            WRITE (LFRMAT,23) NSOL  !LFRMAT is not set
            DO 35 LM=1,NLAKES
               IF (IFREFM.EQ.0) THEN
                 IF ( IRDTAB.GT.0 ) THEN
                   IF(ISS.NE.0) READ(IN,'(100F10.4)') STAGES(LM),
     1                SSMN(LM),SSMX(LM),(CLAKE(LM,ISOL),ISOL=1,NSOL),
     2                LAKTAB(LM)
                   IF(ISS.EQ.0) READ (IN,'(100F10.4)') STAGES(LM),
     1                          (CLAKE(LM,ISOL),ISOL=1,NSOL),LAKTAB(LM)
                 ELSE
                   IF(ISS.NE.0) READ(IN,'(100F10.4)') STAGES(LM),
     1                SSMN(LM),SSMX(LM),(CLAKE(LM,ISOL),ISOL=1,NSOL)
                   IF(ISS.EQ.0) READ (IN,'(100F10.4)') STAGES(LM),
     1                          (CLAKE(LM,ISOL),ISOL=1,NSOL)
                 END IF
               ELSE
                 IF ( IRDTAB.GT.0 ) THEN
                   IF(ISS.NE.0) READ (IN,*) STAGES(LM),SSMN(LM),
     1                          SSMX(LM),(CLAKE(LM,ISOL),ISOL=1,NSOL),
     2                          LAKTAB(LM)
                   IF(ISS.EQ.0) READ (IN,*) STAGES(LM),
     1                          (CLAKE(LM,ISOL),ISOL=1,NSOL),LAKTAB(LM)
                 ELSE
                   IF(ISS.NE.0) READ (IN,*) STAGES(LM),SSMN(LM),
     1                          SSMX(LM),(CLAKE(LM,ISOL),ISOL=1,NSOL)
                   IF(ISS.EQ.0) READ (IN,*) STAGES(LM),
     1                          (CLAKE(LM,ISOL),ISOL=1,NSOL)
                 END IF
               END IF
            IF(ISS.NE.0) WRITE (IOUT,22) LM,STAGES(LM),SSMN(LM),SSMX(LM)
            IF(ISS.EQ.0) WRITE (IOUT,22) LM,STAGES(LM)
 35           WRITE (IOUTS,*) LM,(CLAKE(LM,ISOL),ISOL=1,NSOL)
cgage
C            CLAKINIT=CLAKE
         END IF
      END IF
C
      WRITE (IOUT,'(/)')
      WRITE(IOUT,822)
 19   FORMAT(//1X,'LAKE PACKAGE ACTIVE:  CALCULATED LAKE STAGE FOR EACH 
     1TIME STEP WILL BE STORED IN HNEW ARRAY.')
 20   FORMAT(///1X,'INITIAL LAKE STAGE:  LAKE    STAGE    SS MIN    SS M
     1AX'/)
 21   FORMAT (//1X,'INITIAL LAKE CONCENTRATIONS:  LAKE   CONCENTRATION (
     1NSOL =',I3,')'/)                                                  
 22   FORMAT (22X,I3,3F10.3)
 23   FORMAT ('(31X,I3,3X,1P',I3,'(E12.3))')                            
 820  FORMAT (/1X,'INITIAL LAKE STAGE:  LAKE    STAGE'/)
 822  FORMAT(//1X,'If any subsequent steady-state stress periods, min. a
     1nd max. stages for each lake will be read in Record 9a.'//)
C
! RGN 9/25/12 moved this to read lake bathymetry before stress period information.
      IF ( KKPER==1 .AND. IRDTAB.GT.0 ) THEN
        DO L1=1,NLAKES
          WRITE(IOUT,1399) L1
          iunitnum = LAKTAB(L1)
 1399 FORMAT(//1X,'STAGE/VOLUME RELATION FOR LAKE',I3//6X,'STAGE',
     1        8X,'VOLUME',8X,'AREA'/)
          DO  INC=1,151
          READ(iunitnum,*) DEPTHTABLE(INC,L1), VOLUMETABLE(INC,L1),
     +                    AREATABLE(INC,L1)
          WRITE(IOUT,1315) DEPTHTABLE(INC,L1), VOLUMETABLE(INC,L1),
     +                    AREATABLE(INC,L1)
          END DO
        END DO
      END IF
C1B-----READ ITMP (FLAG TO REUSE LAKE-GEOMETRY DATA).
      IF(IFREFM.EQ.0) THEN
         READ(IN,'(3I10)') ITMP, ITMP1, LWRT
      ELSE
         READ(IN,*) ITMP, ITMP1, LWRT
      END IF
C
C2A-----IF ITMP < 0 THEN REUSE LAKE CONFIGURATION DATA FROM LAST STRESS
C       PERIOD.
      IF(ITMP.GE.0) GO TO 50
      WRITE (IOUT,'(/)')
      WRITE(IOUT,2)
    2 FORMAT(1H ,'REUSING LAKE CONFIGURATION DATA FROM LAST STRESS PERIO
     1D'/)
      GO TO 800
C
C4------IF THERE ARE NO LAKE NODES THEN RETURN.
   50 LKNODE = 0
      IF(ITMP.EQ.0) GOTO 900
C
C   INITIALIZE BGAREA
      DO 60 LK=1,NLAKES
      BGAREA(LK)=0.0
   60 CONTINUE
C
C5------READ INTEGER ARRAYS THAT DEFINE THE POSITIONS OF ALL LAKES IN
C5A     EACH MODEL GRID LAYER.  THEN READ ARRAYS OF LAKEBED CONDUCTANCES
C5B     IN EACH LAYER.
C
C   READ ARRAY OF LAKE ID'S, LAYER BY LAYER
C   REVISED 11/30/2005 DEP
      DO 125 K=1,NLAY
      KK = K
      CALL U2DINT(LKARR1(:,:,KK),ANAME(1),NROW,NCOL,KK,IN,IOUT)
  125 CONTINUE
C
C   CHECK THAT ALL ENTRIES ARE VALID LAKE ID NUMBERS OR ZERO
C
      DO 130 K=1,NLAY
      DO 130 I=1,NCOL
      DO 130 J=1,NROW
      IF(LKARR1(I,J,K).GT.0.AND.LKARR1(I,J,K).LE.NLAKES) GO TO 130
      LKARR1(I,J,K)=0
  130 CONTINUE
C
C   CHECK IF LAKE CELLS HAVE VALUES OF IBOUND=0; WARN IF INCONSISTENT
C
      WRITE (IOUT,'(/)')
      DO 132 K=1,NLAY
      DO 132 I=1,NCOL
      DO 132 J=1,NROW
      IF(LKARR1(I,J,K).GT.0.AND.IBOUND(I,J,K).NE.0) THEN
         WRITE (IOUT,232) IBOUND(I,J,K),LKARR1(I,J,K),I,J,K
  232    FORMAT (7X,'*** WARNING: IBOUND = ',I2,
     1  ' & LKARR = ',I2,' at CELL I=',I3,
     2  ', J=',I3,', K=',I3,' ***')
      END IF
  132 CONTINUE
C
C   READ ARRAY OF BED LEAKANCES, LAYER BY LAYER
Cdep    REVISED 11/30/2005
      WRITE (IOUT,'(/)')
      DO 135 K=1,NLAY
      KK = K
      CALL U2DREL(BDLKN1(:,:,KK),ANAME(2),NROW,NCOL,KK,IN,IOUT)
  135 CONTINUE
C
        WRITE(IOUT,36)
        WRITE(IOUT,4)
36    FORMAT(/7X,'LOCATIONS, LAKE #, INTERFACE TYPE FOR GRID CELLS',
     1 ' ADJACENT TO LAKES:',5X,/
     3 5X,71('-'))
4     FORMAT(5X,'LAYER #',4X,'ROW #',4X,'COLUMN #',3X,'LAKE #',
     1       2X,'INTERFACE TYPE',2X,'LAKEBED LEAKANCE')
C
C   IDENTIFY LAKE BORDER CELLS, ASSIGN CELL TYPE ID'S, COMPUTE AND
C     ASSIGN LAKE-AQUIFER INTERFACE CONDUCTANCES.
C
      M = 0
      DO 180 I=1,NCOL
      DO 180 J=1,NROW
      K = 1
      IF(LKARR1(I,J,K).EQ.0) GO TO 150
      IF(NLAY.EQ.1) GO TO 145
C   Keep searching in vertical direction until non-lake cell is found, 
C     and define interface there ("K" for interface is layer below 
C     bottom of lake)
      DO 140 K=2,NLAY
      IF(LKARR1(I,J,K).EQ.0) GO TO 145
  140 CONTINUE
C   Make sure that K=NLAY if lake extends to bottom cell of grid:
      K=NLAY
C      GO TO 145
C
C   VERTICAL LAKEBED INTERFACE (TYPE 0) DETECTED
C
  145 M = M + 1
      IF(M.LE.MXLKND) GO TO 147
      WRITE(IOUT,149) I,J,K
  149 FORMAT(/1X,'MAXIMUM NUMBER OF GRID CELLS ADJACENT TO LAKES HAS BEE
     1N EXCEEDED WITH CELL ',3I5,'  REDEFINE VARIABLE MXLKND TO A LARGER
     2 VALUE IN MODULE GWF2LAK7AR')
      CALL USTOP(' ')
  147 ILAKE(1,M) = K
      ILAKE(2,M) = J
      ILAKE(3,M) = I
Cdep  changed if statement August 24, 2009
Cdep      IF(K.GT.1.AND.LKARR1(I,J,K).EQ.0) LID = LKARR1(I,J,K-1)
Cdep      IF(LKARR1(I,J,K).NE.0) LID = LKARR1(I,J,K)
      IF(K.GT.1) THEN
        IF(LKARR1(I,J,K).EQ.0) THEN
          LID = LKARR1(I,J,K-1)
        ELSE
          LID = LKARR1(I,J,K)
        END IF
      ELSE IF (K.EQ.1) THEN
        IF(LKARR1(I,J,K).EQ.0) THEN
          LID = 0
        ELSE
          LID = LKARR1(I,J,K)
        END IF
      END IF
      ILAKE(4,M) = LID
      ILAKE(5,M) = 6
      IF ( K.GT.1 ) THEN             !RGN 5/21/12 added IF test
        BEDLAK(M) = BDLKN1(I,J,K-1)
      ELSE                           !RGN
        BEDLAK(M) = BDLKN1(I,J,K)    !RGN
      END IF                         !RGN
      IF(K.EQ.NLAY.AND.LKARR1(I,J,K).NE.0) BEDLAK(M) = 0.0
      BGAREA(LID) = BGAREA(LID) + DELC(J)*DELR(I)
C-LFK-JAN. 2013
c-lfk        WRITE(IOUT,5) (ILAKE(I1,M),I1=1,5), BEDLAK(M)
      WRITE(IOUT,6) (ILAKE(I1,M),I1=1,5), BEDLAK(M)
5     FORMAT(5I10,10X,F10.5)
6     FORMAT(5I10,12X,1PE10.3)
C-LFK
      IF(LKARR1(I,J,K).NE.0) GO TO 180
C
C   SEARCH FOR CELL(S) ADJACENT TO LAKE
C
  150 K2 = K
      DO 175 K1=K2,NLAY
cgzh fix for 2D-problems
      IF(NCOL.EQ.1) GO TO 165
      IF(I.NE.1) GO TO 1151
      IF(LKARR1(I+1,J,K1).EQ.0) GO TO 165
      GO TO 1153
 1151 IF(I.NE.NCOL) GO TO 1152
      IF(LKARR1(I-1,J,K1).EQ.0) GO TO 165
      GO TO 1153
 1152 IF(LKARR1(I+1,J,K1).EQ.0.AND.LKARR1(I-1,J,K1).EQ.0) GO TO 165
C
C   CELL(S) LATERALLY ADJACENT TO LAKE IN X-DIRECTION (TYPE 1) DETECTED
C
 1153 DO 160 N=1,2
      IF(N.EQ.2) GO TO 155
      IF(I.EQ.1) GO TO 160
      IF(LKARR1(I-1,J,K1).EQ.0) GO TO 160
      I2 = I-1
      IFACE=1
      GO TO 157
  155 IF(I.EQ.NCOL) GO TO 160
      IF(LKARR1(I+1,J,K1).EQ.0) GO TO 160
      I2 = I + 1
      IFACE=2
  157 M = M + 1
      IF(M.LE.MXLKND) GO TO 158
      WRITE(IOUT,149) I,J,K1
      CALL USTOP(' ')
  158 ILAKE(1,M) = K1
      ILAKE(2,M) = J
      ILAKE(3,M) = I
      ILAKE(4,M) = LKARR1(I2,J,K1)
      ILAKE(5,M) = IFACE
      BEDLAK(M) = BDLKN1(I,J,K1)
      K4 = K1 - 1
      DO 3158 K3=1,K4
      IF(LKARR1(I,J,K3).EQ.0) GO TO 3158
      GO TO 3162
 3158 CONTINUE
      BEDLAK(M) = BDLKN1(I,J,1)
 3162 CONTINUE
c-lfk      WRITE(IOUT,5) (ILAKE(I1,M),I1=1,5), BEDLAK(M)
C-LFK-JAN. 2013
c-lfk        WRITE(IOUT,5) (ILAKE(I1,M),I1=1,5), BEDLAK(M)
      WRITE(IOUT,6) (ILAKE(I1,M),I1=1,5), BEDLAK(M)
C-LFK
  160 CONTINUE
cgzh fix for 2D-problems
  165 IF(NROW.EQ.1) GO TO 175
      IF(J.NE.1) GO TO 1161
      IF(LKARR1(I,J+1,K1).EQ.0) GO TO 175
      GO TO 1163
 1161 IF(J.NE.NROW) GO TO 1162
      IF(LKARR1(I,J-1,K1).EQ.0) GO TO 175
      GO TO 1163
 1162 IF(LKARR1(I,J+1,K1).EQ.0.AND.LKARR1(I,J-1,K1).EQ.0) GO TO 175
C
C   CELL(S) LATERALLY ADJACENT TO LAKE IN Y-DIRECTION (TYPE 2) DETECTED
C
 1163 DO 170 N=1,2
      IF(N.EQ.2) GO TO 172
      IF(J.EQ.1) GO TO 170
      IF(LKARR1(I,J-1,K1).EQ.0) GO TO 170
      J2 = J - 1
      IFACE=4
      GO TO 174
  172 IF(J.EQ.NROW) GO TO 170
      IF(LKARR1(I,J+1,K1).EQ.0) GO TO 170
      J2 = J + 1
      IFACE=3
  174 M = M + 1
      IF(M.LE.MXLKND) GO TO 176
      WRITE(IOUT,149) I,J,K1
      CALL USTOP(' ')
  176 ILAKE(1,M) = K1
      ILAKE(2,M) = J
      ILAKE(3,M) = I
      ILAKE(4,M) = LKARR1(I,J2,K1)
      ILAKE(5,M) = IFACE
      BEDLAK(M) = BDLKN1(I,J,K1)
      K4 = K1 - 1
      DO 4158 K3=1,K4
      IF(LKARR1(I,J,K3).EQ.0) GO TO 4158
      GO TO 4162
 4158 CONTINUE
      BEDLAK(M) = BDLKN1(I,J,1)
 4162 CONTINUE
c-lfk      WRITE(IOUT,5) (ILAKE(I1,M),I1=1,5), BEDLAK(M)
C-LFK-JAN. 2013
c-lfk        WRITE(IOUT,5) (ILAKE(I1,M),I1=1,5), BEDLAK(M)
      WRITE(IOUT,6) (ILAKE(I1,M),I1=1,5), BEDLAK(M)
C-LFK
  170 CONTINUE
  175 CONTINUE
  180 CONTINUE
      WRITE(IOUT,195) M
  195 FORMAT(/5X,'NUMBER OF LAKE-AQUIFER CELL INTERFACES = ',I5)
      LKNODE = M
C
C   SET LAKE BOTTOM ELEVATIONS
      DO 295 LK=1,NLAKES
  295 BOTTMS(LK) = 999999
C
      DO 350 II=1,LKNODE
      K = ILAKE(1,II)
      J = ILAKE(2,II)
      I = ILAKE(3,II)
C  Convert ILAKE(5,II):  1 and 2 are type 1,  3 and 4 are type 2, 
C    6 is type 0
      NTYP = (ILAKE(5,II)+1)/2
      IF(NTYP.EQ.3) NTYP=0
      IF(NTYP.EQ.0) THEN
        LAKE = ILAKE(4,II)
Cdep  changed if statement August 24, 2009
Cdep        IF(K.GT.1) BOTLK = BOTM(I,J,LBOTM(K-1))
Cdep        IF(K.EQ.NLAY.AND.LKARR1(I,J,K).GT.0) BOTLK = BOTM(I,J,LBOTM(K))
         IF(K.EQ.1.OR.K.EQ.NLAY.AND.LKARR1(I,J,K).GT.0) THEN
            BOTLK = BOTM(I,J,LBOTM(K))
         ELSE IF (K.EQ.0) THEN
            BOTLK = BOTM(I,J,LBOTM(1))
         ELSE
            BOTLK = BOTM(I,J,LBOTM(K-1))
         END IF     
        IF(BOTLK.LT.BOTTMS(LAKE)) BOTTMS(LAKE) = BOTLK
      END IF
  350 CONTINUE
C
C-- COMPUTE AND PRINT STAGE/VOLUME TABLES WHEN MORE THAN ONE LAYER
Cdep  revised print statement to include stage/area tables
C
      IF ( IRDTAB.EQ.0 ) THEN
!      IF(NLAY.EQ.1) GO TO 1331       !RGN 5/21/12
      DO 1330 L1=1,NLAKES
      WRITE(IOUT,1306) L1
Cdep  revised print statement to include area
 1306 FORMAT(//1X,'STAGE/VOLUME RELATION FOR LAKE',I3//6X,'STAGE',
     1        8X,'VOLUME',8X,'AREA'/)
      DO  INC=1,151
        AREATABLE(INC,L1) = 0.D0
      END DO
      EVOL = 0.0
      GTSDPH = 40.0
      TOPMST = BOTTMS(L1)+GTSDPH
      TBELV = BOTTMS(L1)
      DO 1340 I=1,NCOL
      DO 1340 J=1,NROW
      IF(LKARR1(I,J,1).NE.L1) GO TO 1340
Cdep Revised estimate of DTHK to be thickness of top most 
C     layer 6/09/2009
      IF(BOTM(I,J,0).GT.TOPMST) TOPMST = BOTM(I,J,0) 
!      DTHK = BOTM(I,J,0) - BOTM(I,J,1)   RGN this was causing problems 7/8/11
!      IF (DTHK.LE.GTSDPH) THEN
!        TOPMST = BOTM(I,J,1)+DTHK
!      ELSE 
!        TOPMST = BOTM(I,J,1)+GTSDPH
!      END IF
 1340 CONTINUE
      TBNC = (TOPMST-BOTTMS(L1))/150.0
Cdep Revised looping for computing lake stage, volume, 
Cdep   and area Apr 2009.
Cdep   WRITE(IOUT,1315) TBELV, EVOL
      DO  INC=1,151
        IF (INC.GT.1) THEN
          VOLUMETABLE(INC,L1)=VOLUMETABLE(INC-1,L1)
        END IF
        DO I=1,NCOL
          DO J=1,NROW
            LAKEFLG = 0
            K = 1
            MOSTBOT: DO WHILE (LAKEFLG.EQ.0)
              IF(LKARR1(I,J,K).EQ.L1) THEN
                LAKEFLG = K
              END IF
              IF(K.EQ.NLAY)EXIT MOSTBOT
              K = K + 1
            END DO MOSTBOT
            IF(LAKEFLG.GT.0) THEN
              K=LAKEFLG
              FINDBOT: DO WHILE(LKARR1(I,J,K).GT.0)
                K=K+1
                IF(K.EQ.NLAY+1) EXIT
              END DO FINDBOT
              BOTIJ = BOTM(I,J,LBOTM(K-1))
              IF(INC.EQ.1) THEN     
                IF(TBELV+1.0E-03.GT.BOTIJ) THEN
                  AREATABLE(INC,L1)=AREATABLE(INC,L1)+DELC(J)*DELR(I)
                  DEPTHTABLE(INC,L1)=TBELV
                END IF
              ELSE
                IF (TBELV-BOTIJ.GT.1.0E-03) THEN
                  AREATABLE(INC,L1)=AREATABLE(INC,L1)+DELC(J)*DELR(I)
                  DEPTHTABLE(INC,L1)=TBELV
                  IF(ABS(TBELV-BOTIJ).GT.1.0E-03) THEN
                    VOLUMETABLE(INC,L1)=VOLUMETABLE(INC,L1)+
     +                                (DELC(J)*DELR(I))*TBNC
                  END IF
                END IF               
              END IF
            END IF  
          END DO
        END DO       
Cdep PRINT TABLE OF ELEVATION, VOLUME, AND AREA
        WRITE(IOUT,1315) DEPTHTABLE(INC,L1), VOLUMETABLE(INC,L1), 
     +                    AREATABLE(INC,L1) 
        TBELV = TBELV + TBNC 
      END DO
 1315 FORMAT(3(1X,1PE13.5))
      WRITE(IOUT,1326)
 1326 FORMAT(120X)
Cdep  set minimum and maximum lake stages for transient simulations
      IF(ISS.EQ.0) THEN
        SSMN(L1)=BOTTMS(L1)
        SSMX(L1)=TBELV
      END IF
 1330 CONTINUE
 1331 CONTINUE
      END IF
      IF(IUNITSFR.LE.0) THEN
         NDV=0
         NTRB=0
      END IF
C
C
C--  READ LINKAGE PARAMETERS FOR COALESCING LAKES
C
C    FOR EACH CONNECTED LAKE SYSTEM, READ LAKE NUMBERS OF CENTER LAKES
C    AND ADJOINING LAKES AND SILL ELEVATIONS.  ENTER CARD IMAGES
C    FOR SUBLAKE SYSTEMS EVEN IF LINKED TO MAIN LAKE SYSTEM.  SYSTEMS
C    MUST BE ORDERED HIERARCHICALLY.
C
      ICMX = 0
      NCLS=0
      IF(IFREFM.EQ.0) THEN
        READ(IN,'(I5)') NSLMS
      ELSE
        READ(IN,*) NSLMS
      END IF
      WRITE(IOUT,680) NSLMS
  680 FORMAT(/1X,'NUMBER OF CONNECTED LAKE SYSTEMS IN SIMULATION IS ',I3
     1)
      IF(NSLMS.LE.0) GO TO 760
      DO 700 IS=1,NSLMS
      IF(IFREFM.EQ.0) THEN
        READ(IN,'(16I5)',END=750) IC,(ISUB(IS,I),I=1,IC)
      ELSE
        READ(IN,*,END=750) IC,(ISUB(IS,I),I=1,IC)
      END IF
      IF(IC.LE.0) GO TO 750
      IF(IC.GT.ICMX) ICMX=IC
      ICS(IS)=IC
      IC1 = IC - 1
      IF(IFREFM.EQ.0) THEN
        READ(IN,'(100F10.2)') (SILLVT(IS,I),I=1,IC1)
      ELSE
        READ(IN,*) (SILLVT(IS,I),I=1,IC1)
      END IF
      WRITE(IOUT,18) IS, ICS(IS), ISUB(IS,1)
   18 FORMAT(/10X,'SYSTEM',I3//2X,'NUMBER OF LAKES IN SYSTEM',I5,
     1  '  CENTER LAKE NUMBER',I5//1X,'SUBLAKE NUMBER',3X,
     2  'SILL ELEVATION'/)
      DO 715 JK=2,IC
  715 WRITE(IOUT,717) ISUB(IS,JK), SILLVT(IS,JK-1)
  717 FORMAT(8X,I2,8X,F10.2)
  700 CONTINUE
  750 CONTINUE
      NCLS=IS-1
      WRITE(IOUT,751) NCLS
  751 FORMAT(/1X,'READ DATA FOR',I5,' LAKE SYSTEMS'/)
  760 CONTINUE
C
C----- READ LAKE PRECIPITATION, EVAPORATION, RUNOFF, AND WITHDRAWAL RATES.
C      IF ITMP1 LT 0, SPECIFICATIONS FROM LAST STRESS PERIOD ARE USED.
C
  800 IF(ITMP1.GE.0) GO TO 801
      WRITE(IOUT,802)
  802 FORMAT(1H0,'REUSING RECH,ET,WITHDRAWAL RATES FROM LAST STRESS PERI
     1OD'/)
      GOTO 900
  801 IF(ISS.NE.0.AND.KKPER.GT.1) WRITE(IOUT,7)
7     FORMAT(/1X,'LAKE',7X,'PRECIP',5X,'EVAP',5X,'RUNOFF',
     2     3X,'WITHDRAW',3X,'BOTTOM',5X,'AREA',5X,'SS MIN',3X,'SS MAX'
     1/90('-'))
      IF(ISS.EQ.0.OR.KKPER.EQ.1) WRITE(IOUT,77)
   77 FORMAT(/1X,'LAKE',7X,'PRECIP',5X,'EVAP',5X,'RUNOFF',
     2     3X,'WITHDRAW',3X,'BOTTOM',5X,'AREA',5X,/70('-'))
      IF (IUNITGWT.GT.0) WRITE (IOUTS,8)
 8    FORMAT (//1X,'LAKE',4X,'SOLUTE',6X,'CPPT',6X,'CRNF',6X,'CAUG'/)
C
      DO 300 LM=1,NLAKES
        IF(IFREFM.EQ.0) THEN
          IF(ISS.NE.0.AND.KKPER.GT.1) READ(IN,'(6F10.4)') PRCPLK(LM),
     1     EVAPLK(LM),RNF(LM),WTHDRW(LM),SSMN(LM),SSMX(LM)
          IF(ISS.EQ.0.OR.KKPER.EQ.1) READ(IN,'(6F10.4)') PRCPLK(LM),
     1     EVAPLK(LM),RNF(LM),WTHDRW(LM)
        ELSE
          IF(ISS.NE.0.AND.KKPER.GT.1) READ(IN,*) PRCPLK(LM),EVAPLK(LM),
     1     RNF(LM),WTHDRW(LM),SSMN(LM),SSMX(LM)
          IF(ISS.EQ.0.OR.KKPER.EQ.1) READ(IN,*) PRCPLK(LM),EVAPLK(LM),
     1     RNF(LM),WTHDRW(LM)
        END IF
C
C--EDM: SET FOLLOWING VALUES FOR LMT
        IF(IUNIT(49).NE.0) THEN
          IF(PRCPLK(LM).NE.0.AND.LKFLOWTYPE(3).EQ.'NA') THEN
            LKFLOWTYPE(3)='PRECIP'
            NLKFLWTYP = NLKFLWTYP + 1
          ENDIF
          IF(EVAPLK(LM).NE.0.AND.LKFLOWTYPE(4).EQ.'NA') THEN 
            LKFLOWTYPE(4)='EVAP'
            NLKFLWTYP = NLKFLWTYP + 1
          ENDIF
          IF(RNF(LM).NE.0.AND.LKFLOWTYPE(5).EQ.'NA') THEN
            LKFLOWTYPE(5)='RUNOFF'
            NLKFLWTYP = NLKFLWTYP + 1
          ENDIF
          IF(WTHDRW(LM).NE.0.AND.LKFLOWTYPE(6).EQ.'NA') THEN
            LKFLOWTYPE(6)='WITHDRAW'
            NLKFLWTYP = NLKFLWTYP + 1
          ENDIF
        ENDIF
C
        IF(ISS.NE.0.AND.KKPER.GT.1)WRITE(IOUT,9)LM,PRCPLK(LM),EVAPLK(LM)
     1   ,RNF(LM),WTHDRW(LM),BOTTMS(LM),BGAREA(LM),SSMN(LM),SSMX(LM)
9       FORMAT(1X,I3,4X,1P,3E10.3,1X,5E10.3)
        IF(ISS.EQ.0.OR.KKPER.EQ.1)WRITE(IOUT,9)LM,PRCPLK(LM),EVAPLK(LM),
     1   RNF(LM),WTHDRW(LM),BOTTMS(LM),BGAREA(LM)
        IF(IUNITGWT.LE.0) GO TO 300
        DO 850 ISOL=1,NSOL
          IF(IFREFM.EQ.0) THEN
            IF(WTHDRW(LM).LT.0.0) THEN
              READ(IN,'(3F10.4)')CPPT(LM,ISOL),CRNF(LM,ISOL),
     +                          CAUG(LM,ISOL)
            ELSE
              READ(IN,'(2F10.4)')CPPT(LM,ISOL),CRNF(LM,ISOL)
            END IF
          ELSE
            IF(WTHDRW(LM).LT.0.0) THEN
              READ(IN,*) CPPT(LM,ISOL),CRNF(LM,ISOL),CAUG(LM,ISOL)
            ELSE
              READ(IN,*) CPPT(LM,ISOL),CRNF(LM,ISOL)
            END IF
          END IF
          IF(WTHDRW(LM).LT.0.0)WRITE(IOUTS,840) LM,ISOL,
     +         CPPT(LM,ISOL),CRNF(LM,ISOL),CAUG(LM,ISOL)       
          IF(WTHDRW(LM).GE.0.0)
     1    WRITE(IOUTS,841) LM,ISOL,CPPT(LM,ISOL),CRNF(LM,ISOL)
  840     FORMAT(1X,I3,6X,I3,4X,1P,3E10.2)
  841   FORMAT(1X,I3,6X,I3,4X,1P,2E10.2)
  850   CONTINUE
C        WRITE (IOUTS,'(/)')
  300 CONTINUE
      WRITE (IOUT,'(/)')
C
C------Define Initial Lake Volume & Initialize Cumulative Budget Terms
      IF(KKPER.EQ.1) THEN
!dep revised calculation of initial lake volume July 2009
        STGINIT=0.0D0
        DO 8400 LK=1,NLAKES
!dep 8400    VOL(LK)=0.0
             STGINIT=STAGES(LK)
             VOL(LK)=VOLTERP(STGINIT,LK)
             VOLINIT(LK)=VOL(LK)
 8400   CONTINUE
        DO 8450 LK=1,NLAKES
             CUMPPT(LK)=0.0
             CUMEVP(LK)=0.0
             CUMRNF(LK)=0.0
             CUMGWI(LK)=0.0
             CUMGWO(LK)=0.0
             CUMSWI(LK)=0.0
             CUMSWO(LK)=0.0
             CUMWDR(LK)=0.0
             CUMFLX(LK)=0.0
 8450   CONTINUE
            DO 8900 L=1,LKNODE
               IL=ILAKE(1,L)
               IR=ILAKE(2,L)
               IC=ILAKE(3,L)
               LAKE=ILAKE(4,L)
C------Convert ILAKE(5,L):  1 and 2 are type 1,  3 and 4 are type 2, 
C        6 is type 0
               ITYPE = (ILAKE(5,L)+1)/2
               IF(ITYPE.EQ.3) ITYPE=0
               IF(ITYPE.NE.0) GO TO 8900
               IF(IL.GT.1) BOTLK = BOTM(IC,IR,LBOTM(IL-1))
               IF(IL.EQ.NLAY.AND.LKARR1(IC,IR,IL).GT.0)
     1            BOTLK = BOTM(IC,IR,LBOTM(IL))
 8900       CONTINUE
      ENDIF

 900  IF (IUNITBCF.GT.0) THEN  ! rsr, moved if block from main
        CALL SGWF2LAK7BCF7RPS()
      ELSE IF (IUNITLPF.GT.0) THEN
        CALL SGWF2LAK7LPF7RPS()
      ELSE IF (IUNITHUF.GT.0) THEN
        CALL SGWF2LAK7HUF7RPS()
      ELSE
        WRITE (IOUT, *) 'LAK Package requires BCF, LPF, or HUF'
        CALL USTOP(' ')
      END IF
      IF (IUNITSFR.GT.0) CALL SGWF2LAK7SFR7RPS()     
C
C7------RETURN
      RETURN
      END
C
      SUBROUTINE GWF2LAK7AD(KKPER,KKSTP,IUNITGWT,IGRID)
C
C------VERSION 7.1 JUNE 2006 GWF2LAK7AD; REVISED FEBRUARY 6, 2012
C
C     ******************************************************************
C     ADVANCE TO NEXT TIME STEP FOR TRANSIENT LAKE SIMULATION, AND COPY
C             INITIAL LAKE STAGES TO STGOLD FOR STEADY STATE.
C     ******************************************************************
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFLAKMODULE, ONLY: NLAKES, LKNODE, FLOB, STAGES,
     +                        STGNEW, STGOLD, VOLOLDD, VOLOLD, VOLINIT,
     +                        BOTTMS, IDIV, STGOLD2, NDV
      USE GWFSFRMODULE, ONLY: DLKSTAGE
      USE GLOBAL,       ONLY: IOUT
C     ------------------------------------------------------------------
C     FUNCTIONS
C     ------------------------------------------------------------------
      DOUBLE PRECISION VOLTERP
      EXTERNAL VOLTERP
C     ------------------------------------------------------------------
C
C------SET POINTERS FOR THE CURRENT GRID.
      CALL SGWF2LAK7PNT(IGRID)
C
C1 --- COPY INITIAL LAKE STAGES TO STGOLD.
! RGN COMBINED IF AND ADDED VOLOLDD 4/17/09
Cdep  initialized VOLINIT and VOLOLD to VOLOLDD 6/4/2009
      DO I=1,NLAKES
        IF(KKPER.EQ.1.AND.KKSTP.EQ.1) THEN
          STGOLD(I)=STAGES(I)
          VOLOLDD(I)=VOLTERP(STGOLD(I),I)
          VOLOLD(I) = VOLOLDD(I)
          VOLINIT(I) = VOLOLDD(I)
          STGNEW(I)=STAGES(I)
        ELSE
          STGOLD2(I)=STGNEW(I)
          STGOLD(I)=STGNEW(I)
          VOLOLDD(I)=VOLTERP(STGOLD(I),I)
          VOLOLD(I)=VOLOLDD(I)
        END IF 
! Moved this code from 7FM  10/19/10
         DO IDV=1,NDV
           INODE=IDIV(I,IDV)
           IF (INODE.GT.0) THEN
             IF( DLKSTAGE(1,INODE).LT.DBLE(BOTTMS(I))) THEN
               WRITE(IOUT,971)I,BOTTMS(I),
     +                           DLKSTAGE(1,INODE),INODE
               CALL USTOP(' ')
             END IF
           END IF
         END DO
 ! To hear.
      END DO
 971           FORMAT(' BOTTOM ELEVATION OF LAKE ',I5,' IS ', F10.2,
     +                 ' AND IS ABOVE OUTLET ELEVATION OF ', F10.2,
     +                 ' FOR STREAM SEGMENT ',I5,/1X,
     +                 ' THIS WILL CAUSE PROBLEMS IN COMPUTING LAKE',
     +                 ' STAGE USING THE NEWTON METHOD. '/1X,
     +                 ' ELEVATION OF STREAM OUTLET MUST BE GREATER'
     +                 ' THAN OR EQUAL TO THE LOWEST ELEVATION OF THE',
     +                 ' LAKE.',/1X,'*****PROGRAM STOPPING'/)
C2 ----- IF NOT FIRST TIME STEP, OR FIRST STRESS PERIOD, UPDATE
C           STGOLD BY STGNEW.
! RGN MOVED TO ABOVE. STGOLD SHOULD BE UPDATED EVERY TIME STEP! 4/17/09
!      IF (KKPER.NE.1.OR.KKSTP.NE.1) THEN
!            DO 30 K=1,NLAKES
!               STGOLD(K)=STGNEW(K)
!               VOLOLD(K)=VOLTERP(STGOLD(K),K))
!30             STGOLD2(K)=STGNEW(K)
!      ENDIF
C
C-----Initialize FLOB array (stores cell by cell flux between lake and
C                            aquifer)
      IF (IUNITGWT.GT.0) THEN
        DO 50 LK=1,LKNODE
 50        FLOB(LK)=0.0
      END IF
C
C3------RETURN
      RETURN
      END
C
      SUBROUTINE GWF2LAK7ST(NFLG,IGRID)
C   ********************************************************************
C   SET IBOUND VALUES SO THAT RECHARGE AND EVAPOTRANSPIRATION (ET) WILL
C   BE ASSIGNED CORRECTLY UNDERNEATH DRYING LAKES (NFLG = 0), OR RESET
C   IBOUND AFTER RECHARGE AND ET ARE COMPUTED (NFLG = 1).
C   ********************************************************************
C
C   SPECIFICATIONS:
C
C-----------------------------------------------------------------------
      USE GWFLAKMODULE, ONLY: LKNODE, ILAKE, STGOLD
      USE GLOBAL,       ONLY: IBOUND, LBOTM, BOTM
C-----------------------------------------------------------------------
C
C------SET POINTERS FOR THE CURRENT GRID.
      CALL SGWF2LAK7PNT(IGRID)

      IF(LKNODE.EQ.0) RETURN
      DO 10 L=1,LKNODE
C  Convert ILAKE(5,L):  1 and 2 are type 1,  3 and 4 are type 2, 6 is type 0
      ITYPE = (ILAKE(5,L)+1)/2
      IF(ITYPE.EQ.3) ITYPE=0
C
C-------ONLY CHANGE IBOUND FOR VERTICALLY ADJACENT NODE FACES
      IF(ITYPE.NE.0) GO TO 10
      IL = ILAKE(1,L)
      IR = ILAKE(2,L)
      IC = ILAKE(3,L)
C
C-------RESET AFTER EXECUTING RECHARGE OR ET ROUTINES
      IF(NFLG.EQ.1) GO TO 8
C
C-------RESET BEFORE EXECUTING RECHARGE OR ET ROUTINES
      IF ( IL.GT.1 ) THEN       !RGN 5/21/12 added IF test
        IBOUND(IC,IR,IL-1) = -7
      ELSE                      !RGN
        IBOUND(IC,IR,IL) = -7   !RGN
      END IF                    !RGN
C
C-------THIS IS THE CORRECT ASSIGNMENT IF PORTION OF LAKE IN COLUMN
C       IS WET.
      LAKE = ILAKE(4,L)
      IF(STGOLD(LAKE).GT.BOTM(IC,IR,LBOTM(IL)-1)) GO TO 10
C
C-------IF PORTION OF LAKE IN NODE IS DRY, LET RECHARGE AND ET BE
C       APPLIED TO THE AQUIFER NODE UNDERNEATH THE LAKE BY SETTING
C       IBOUND EQUAL TO 0.
    8 IF ( il.GT.1 ) THEN        !RGN 5/21/12 added IF test
!     8  IBOUND(IC,IR,IL-1) = 0  !RGN
        IBOUND(IC,IR,IL-1) = 0   !RGN
      ELSE                       !RGN
        IBOUND(IC,IR,IL) = 0     !RGN
      END IF                     !RGN
   10 CONTINUE
C
C3------RETURN
      RETURN
      END
C
      SUBROUTINE GWF2LAK7FM(KKITER,KKPER,KKSTP,IUNITSFR,IUNITUZF,IGRID)
C
C----- USGS VERSION 7.1; JUNE 2006 GWF2LAK7FM
Cdep  MODIFIED SUBROUTINE TO ITERATIVELY SOLVE FOR LAKE STAGE EVEN
C       DURING TRANSIENT STRESS PERIODS. REVISIONS MARCH THROUGH
C       AUGUST 2009  DEP AND RGN
C     ******************************************************************
C     ADD LAKE TERMS TO RHS AND HCOF IF SEEPAGE OCCURS IN MODEL CELLS
C     ******************************************************************
C
      USE GWFLAKMODULE
      USE GLOBAL,       ONLY: NCOL, NROW, NLAY, IBOUND, IOUT, ISSFLG, 
     +                        DELR, DELC, LBOTM, BOTM, HNEW, HCOF, RHS 
      USE GWFBASMODULE, ONLY: DELT
      USE GWFSFRMODULE, ONLY: STRIN, STROUT, FXLKOT, DLKSTAGE, SEG
      USE GWFUZFMODULE, ONLY: SURFDEP,IUZFBND,FINF,VKS
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
Cdep  Added functions for interpolating between areas, derivatives, 
Cdep     and outflow rates
C     ------------------------------------------------------------------
C     FUNCTIONS
C     -----------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION FINTERP, DERIVTERP, OUTFLWTERP, VOLTERP, SURFTERP
      EXTERNAL FINTERP, DERIVTERP, OUTFLWTERP, VOLTERP, SURFTERP
      DOUBLE PRECISION STGTERP, FXLKOT_TERP
      EXTERNAL STGTERP, FXLKOT_TERP
C     -----------------------------------------------------------------
C     ARGUMENTS
C     -----------------------------------------------------------------
      INTEGER, INTENT(IN) :: KKITER, KKPER, IUNITSFR, IUNITUZF, IGRID, 
     1                       KKSTP    
Cdep  added runoff and flobo3
!      REAL :: RUNOFF
Cdep  added unsaturated flow beneath lakes flag as a local variable
      INTEGER ISS, LK, ITRIB, INODE, LAKE, MTER, IICNVG, L1, MAXITER
      INTEGER NCNV, LL, II, L, IC, IR, IL, ITYPE
      INTEGER LI, INOFLO, IDV, IL1,n
Cdep  added SURFDPTH, CONDMX,BOTLKUP,BOTLKDN  3/3/2009
      DOUBLE PRECISION BOTLK,BOTCL,CONDUC,H,FLOBOT,STGON,
     1                 FLOBO3,THET1,CLOSEZERO,
     2                 SURFDPTH,CONDMX,BOTLKUP,BOTLKDN, FLOTOUZF,
     3                 VOL2,WITHDRW3
!!     3                 VOL2,RAMPGW,RAMPSTGO,RAMPSTGN,
!!     4                 RAMPSTGON,HTEMP,WITHDRW3  
Cdep  added double precision variables
      DOUBLE PRECISION RESID1, RESID2, DERIV, DSTAGE, DLSTG, Botlake, 
     1                 Splakout, dy, SRFPT, HD
      DOUBLE PRECISION AREA, RAIN, EV, THCK, SSMN1, SSMX1,
     1                 OUTFLOW, DSTG, VOLNEW1, VOLNEW2, STAGE2
!      DOUBLE PRECISION RUNF
!      PARAMETER(CLOSEZERO = 1.0E-07)
C     ------------------------------------------------------------------
C------SET POINTERS FOR THE CURRENT GRID.
      CALL SGWF2LAK7PNT(IGRID)
      CLOSEZERO = 1.0D-09
      DLSTG = 0.00001D0
      SURFDPTH = DBLE(SURFDEPTH)
      VOLNEW1 = 0.0
      VOLNEW2 = 0.0

C1------IF LKNODE<=0 THERE ARE NO LAKE NODES. RETURN.
      IF (LKNODE.LE.0) RETURN
      ISS = ISSFLG(KKPER)
C
C2------PROCESS EACH CELL IN THE ILAKE LIST.
Cdep   added STGITER, and STGNEW to INITIALIZATION.
      DO LK=1,NLAKES
        IF(KKITER.EQ.1)THEN
         STGITER(LK) = STGOLD(LK)
         STGNEW(LK) = STGOLD(LK)
        END IF
        NCNCVR(LK) = 0
        LIMERR(LK) = 0
        SURFIN(LK)=0.0
        DSRFOT(LK)=0.0
        GWRATELIM(LK) = 0.0
        SURFOT(LK)=0.0
      END DO
      BOTLKUP = 0.0D0
      BOTLKDN = 0.0D0
      CONDMX = 0.0D0
      VOL2 = 0.0D0
      WITHDRW3 = 0.0D0
C
C2A --- SUM UP INFLOWS FROM INFLOWING STREAM REACHES.
      IF (IUNITSFR.GT.0) THEN
        DO 200 LK=1,NLAKES
        DO 200 ITRIB=1,NTRB
            INODE=ITRB(LK,ITRIB)
            IF (INODE.LE.0) GO TO 200
            SURFIN(LK)=SURFIN(LK)+STRIN(INODE)
200     CONTINUE
      END IF
C
C2B --- SUM UP OVERLAND RUNOFF INTO LAKE.
      DO LAKE = 1,NLAKES
C EDM - Add indices to RUNF and RUNOFF because these terms need to be 
C       saved for each lake in the simulation for writing to the FTL file by LMT
          
        IF(RNF(LAKE).GE.0.0) RUNF(LAKE) = RNF(LAKE)
        IF(RNF(LAKE).LT.0.0) RUNF(LAKE) =-RNF(LAKE)*PRCPLK(LAKE)
     1                                    *BGAREA(LAKE)
        IF (IUNITUZF.GT.0) THEN
          RUNOFF(LAKE) = OVRLNDRNF(LAKE)
        ELSE
          RUNOFF(LAKE) = 0.0
        END IF
C
C2C --- SUM OF BOTH STREAMFLOW IN AND OVERLAND RUNOFF.
C         (INCLUDES LAKE VOLUME).
        FLWIN(LAKE) = SURFIN(LAKE)+RUNF(LAKE)+RUNOFF(LAKE)+
     +                VOLTERP(STGOLD(LAKE),LAKE)/DELT
      END DO
C
C3  --- TIME WEIGHTING FACTOR. 
      
      THET1 = DBLE(THETA)
      IF ( ISS==1 ) THET1 = 1.0
      IF(THET1-0.5.LT.-CLOSEZERO) THET1=0.0D0
      MTER = NSSITR
      IICNVG = 0
      L1 = 0
      MAXITER = MTER
      IF ( THET1.LT.CLOSEZERO ) MAXITER = 0
C
C4------MASTER LOOP FOR COMPUTING GROUNDWATER INTERACTION WITH LAKES.
Cdep&rgn   Revised April-August 2009
      CONVERGE: DO WHILE (L1<=MAXITER)
        L1 = L1 + 1
C
C4B-----NCNV IS LAKE CONVERGENCE FLAG. IT IS 0 WHEN ALL LAKES HAVE
C         CONVERGED. NCNCVR(LAKE) IS CONVERGENCE FLAG FOR EACH LAKE.
        NCNV = 0
        DO LAKE=1,NLAKES
          IF(NCNCVR(LAKE).EQ.0) NCNV = 1
        END DO
        IF ( L1.GT.MAXITER ) NCNV = 0
        IF ( THET1.LT.CLOSEZERO) NCNV = 0
        IF ( NCNV.EQ.0 ) IICNVG = 1
C
C4C-----INITIALIZE VARIABLES.
        DO LL=1,NLAKES 
          SUMCNN(LL) = 0.0
          SUMCHN(LL) = 0.0
          EVAP(LL)=0.0D0
          PRECIP(LL)=0.0D0
          SEEP(LL)=0.0D0
          SEEP3(LL)=0.0D0
          SEEPUZ(LL)=0.0D0
          SURFA(LL)=0.0
          EVAPO(LL) = EVAPLK(LL)
          WITHDRW(LL) = WTHDRW(LL)
          FLWITER(LL) = FLWIN(LL) 
          FLWITER3(LL) = FLWIN(LL)
          IF ( ISS==1 ) THEN
            FLWITER(LL) = 1.0E10 
            FLWITER3(LL) = 1.0E10
          END IF
        END DO
C
C5------II LOOP BALANCES INFLOWS AND OUTFLOWS TO/FROM A LAKE.
C   WHEN II=1, FLOW INTO LAKE FROM ALL SOURCES ARE CALCULATED.
C   WHEN II=2, SEEPAGE TO AND FROM LAKE AND RESIDUAL TERMS ARE ADDED TO 
C   GROUNDWATER MATRIX.LAKE SEEPAGE TO GROUNDWATER LIMITED TO AVAILABLE
C   WATER IN LAKE.
        DO II = 1,2
          DO L=1,LKNODE
            IL=ILAKE(1,L)
            IR=ILAKE(2,L)
            IC=ILAKE(3,L)
C
C5B------DETERMINE LAKE AND NODAL LAYER,ROW,COLUMN NUMBER.
            LAKE=ILAKE(4,L)
            IF( ISS.EQ.1 ) STGOLD(LAKE)=STGNEW(LAKE)
            STGON = (1.0D0-THET1)*STGOLD(LAKE) + THET1*STGNEW(LAKE)
            ITYPE = (ILAKE(5,L)+1)/2
            IF(ITYPE.EQ.3) ITYPE=0
            AREA = DELR(IC)*DELC(IR)
            IF(IL.GT.1) THEN
              BOTLK = BOTM(IC,IR,LBOTM(IL-1))
            ELSE
              BOTLK = BOTM(IC,IR,LBOTM(IL))
            END IF
            BOTCL = BOTM(IC,IR,LBOTM(IL))
            IF(IL.EQ.NLAY.AND.CNDFCT(L).EQ.0.0) BOTLK = BOTCL
            RAIN = PRCPLK(LAKE)
            EV = EVAPLK(LAKE)
            CONDUC=CNDFCT(L) 
            FLOBOT = 0.0D0
            FLOBO3 = 0.0D0
            FLOTOUZF = 0.0D0
            IF ( NCNCVR(LAKE).NE.2 ) THEN
              IL1 = IL
              IF ( ITYPE.EQ.0 ) THEN
                DO WHILE (IL1 .LE. NLAY)
                  IF( IBOUND(IC,IR,IL1).GT.0 ) THEN 
                    EXIT
                  ELSE
                    IL1 = IL1 + 1
                  END IF
                END DO
                IF ( IL1.GT.NLAY ) IL1 = NLAY
              END IF
              IF( IBOUND(IC,IR,IL1).LE.0 ) THEN
 !               IF ( CONDUC.GT.CLOSEZERO ) WRITE(IOUT,506) L,IC,IR,IL
                CONDUC = 0.0
              END IF  
              IF(CONDUC.GT.0.0) THEN
                H=HNEW(IC,IR,IL1)  ! IL changed to IL1 !RGN 1/9/2017
                INOFLO = 0
C
C9A------CALCULATE SEEPAGE.
C    
                CALL GET_FLOBOT(IC, IR, IL1, ITYPE, INOFLO,CONDUC,
     1                FLOBOT,FLOBO3,FLOTOUZF,DLSTG,CLOSEZERO,H,
     2                THET1,ISS,LAKE,II,SURFDPTH,AREA,IUNITUZF,
     3                BOTLK,BOTCL,L1)
C
C9B------ADD SEEPAGE RATES AND RESIDUAL TERMS TO GROUNDWATER MATRIX 
C         WHEN ITYPE = 0.
                IF( NCNV == 0 .AND. II==2 ) THEN 
                  IF ( L==LKNODE ) NCNCVR(LAKE) = 2
                  IF ( ITYPE.EQ.0 ) THEN
                    IF ( INOFLO==1 ) THEN
                      RHS(IC,IR,IL1)=RHS(IC,IR,IL1)-FLOBOT
                    ELSE
                      IF (STGON-BOTLK.GT.CLOSEZERO)THEN
                        IF (H.LE.BOTLK) THEN
                          IF (IUNITUZF.EQ.0) THEN
                              RHS(IC,IR,IL1)=RHS(IC,IR,IL1)-FLOBOT
                          ELSE IF (IUZFBND(IC,IR).EQ.0 ) THEN
                              RHS(IC,IR,IL1)=RHS(IC,IR,IL1)-FLOBOT
                          END IF
                        ELSE  
                          RHS(IC,IR,IL1)=RHS(IC,IR,IL1) - STGON*CONDUC
                          HCOF(IC,IR,IL1)=HCOF(IC,IR,IL1) - CONDUC
                        END IF
                      ELSE
                        IF ( H.GT.BOTLK ) THEN
                          RHS(IC,IR,IL1)=RHS(IC,IR,IL1) - BOTLK*CONDUC
                          HCOF(IC,IR,IL1)=HCOF(IC,IR,IL1) - CONDUC
                        END IF
                      END IF
                    END IF
C
C9BC-----ADD SEEPAGE RATES AND RESIDUAL TERMS TO GROUNDWATER MATRIX 
C         WHEN ITYPE = 1 OR 2.                  
                  ELSE IF ( ITYPE.EQ.1.OR.ITYPE.EQ.2 ) THEN
                    IF ( INOFLO==1 ) THEN
                      RHS(IC,IR,IL1)=RHS(IC,IR,IL1)-FLOBOT
                    ELSE
                      IF( IBOUND(IC,IR,IL1).GT.0 ) THEN 
                        HD = H
                        IF( H.GT.BOTM(IC,IR,LBOTM(IL1)-1) )
     1                             HD = BOTM(IC,IR,LBOTM(IL1)-1)
C
C9D------CONDUCTANCE ACROSS VERTICAL CELL FACE DEPENDENT ON 
C          SATURATED THICKNESS.
                        THCK = HD - BOTCL
                        IF( THCK.LE.0.0 ) THCK = 0.0
                      END IF
                      IF (STGON.GT.BOTCL.OR.THCK.GT.0.0) THEN
                        IF(STGON-BOTCL.GT.CLOSEZERO)THEN
                          IF ( H-BOTCL.GT.CLOSEZERO ) THEN
                            RHS(IC,IR,IL1)=RHS(IC,IR,IL1) - STGON*CONDUC
                            HCOF(IC,IR,IL1)=HCOF(IC,IR,IL1) - CONDUC
                          END IF
                        ELSE IF( H-BOTCL.GT.CLOSEZERO ) THEN
                          RHS(IC,IR,IL1)=RHS(IC,IR,IL1) - BOTCL*CONDUC
                          HCOF(IC,IR,IL1)=HCOF(IC,IR,IL1) - CONDUC
                        END IF
                      END IF
                    END IF
                  END IF
C
C10------COMPUTE NET SEEPAGE THROUGH LAKEBED SEDIMENTS 
C          WHEN II=2.
                END IF
                IF (II==2)THEN
                  SEEP(LAKE)=SEEP(LAKE)-FLOBOT-FLOTOUZF
                  SEEP3(LAKE)=SEEP3(LAKE)-FLOBO3
                END IF
              END IF
            END IF
          END DO
        END DO
C
C11------ONLY COMPUTE LAKE LEVEL AFTER SCANNING THRU ALL NODES OF
C          A LAKE.
        DO LAKE=1,NLAKES
C
C11B-----SET STGITER TO STGNEW WHEN THET1>0 AND TO STGOLD 
C          WHEN THET=0.
          IF( THET1.GT.CLOSEZERO ) THEN
            IF ( L1.LT.MTER) STGITER(LAKE) = STGNEW(LAKE)
          ELSE
            STGITER(LAKE) = STGOLD(LAKE)
          END IF
C
C12------COMPUTE EVAPORATION AND PRECIPITATION USING STGOLD AND 
C          ADD PRECIPITATION TO FLWITER AND FLWITER3.
          SURFA(LAKE)=FINTERP(STGNEW(LAKE),LAKE)
          EVAP(LAKE)=EVAPLK(LAKE)*SURFA(LAKE)
          PRECIP(LAKE)=PRCPLK(LAKE)*SURFA(LAKE)
          FLWITER(LAKE) = FLWITER(LAKE) + PRECIP(LAKE)
          SRFPT=FINTERP(STGNEW(LAKE)+DLSTG,LAKE)
          EVAP3(LAKE)=EVAPLK(LAKE)*SRFPT
          PRECIP3(LAKE)=PRCPLK(LAKE)*SRFPT
          FLWITER3(LAKE) = FLWITER3(LAKE) + PRECIP3(LAKE)
C
C13------LIMIT WITHDRW TO LAKE INFLOW WHEN WITHDRAWALS EXCEED
C           INFLOW (INCLUDING AVAILABLE LAKE STORAGE).
          IF(WITHDRW(LAKE).GE.FLWITER(LAKE)) THEN
            WITHDRW(LAKE) = FLWITER(LAKE)
            FLWITER(LAKE) = 0.0D0
          ELSE
            FLWITER(LAKE)  = FLWITER(LAKE) - WITHDRW(LAKE)
          END IF
          IF(WITHDRW(LAKE).GE.FLWITER3(LAKE)) THEN
            WITHDRW3 = FLWITER3(LAKE)
            FLWITER3(LAKE) = 0.0
          ELSE
            WITHDRW3 = WITHDRW(LAKE)
            FLWITER3(LAKE)  = FLWITER3(LAKE) - WITHDRW3
          END IF
C
C14------LIMIT EVAPORATION TO LAKE INFLOW WHEN EVAPORATION EXCEEDS
C          INFLOW (INCLUDING AVAILABLE LAKE STORAGE AND WITHDRAWALS).
          IF ( EVAP(LAKE)>=FLWITER(LAKE) ) THEN
            EVAP(LAKE)=FLWITER(LAKE)
            FLWITER(LAKE) = 0.0D0
          ELSE
            FLWITER(LAKE) = FLWITER(LAKE) - EVAP(LAKE)  
          END IF
          IF ( EVAP3(LAKE)>=FLWITER3(LAKE) ) THEN
            EVAP3(LAKE)=FLWITER3(LAKE)
            FLWITER3(LAKE) = 0.0D0
          ELSE
            FLWITER3(LAKE) = FLWITER3(LAKE) - EVAP3(LAKE)  
          END IF
            SSMN1 = SSMN(LAKE)
            SSMX1 = SSMX(LAKE)
C
C15-----SUM UP OUTFLOWS FROM OUTFLOWING STREAM REACHES.
            DSRFOT(LAKE) = 0.0D0
            SURFOT(LAKE) = 0.0D0 
 ! 11/10 Outflow is zero when all is lost to ET. ******* NEW(.AND.FLWITER(LAKE).GT.CLOSEZERO)
            IF(IUNITSFR.GT.0.AND.FLWITER(LAKE).GT.CLOSEZERO) THEN
              DO IDV=1,NDV
                INODE=IDIV(LAKE,IDV)
                IF (INODE.GT.0) THEN   
                  Splakout = DBLE(SEG(2,INODE))
                  DSTAGE = STGITER(LAKE)
                  IF ( SEG(2,INODE).LE.1.0e-6 ) THEN
                    Botlake = DBLE(BOTTMS(LAKE))
                    DSRFOT(LAKE) = DSRFOT(LAKE) + DERIVTERP(DSTAGE,
     +                               INODE)
                    STROUT(INODE) = OUTFLWTERP(DSTAGE,INODE)
                  ELSE
! Set Botlake to elevation of stream channel for specified flow diversion.
                    Botlake = SEG(8,INODE)
                    STROUT(INODE) = 0.0
                    FXLKOT(INODE) = FXLKOT_TERP(DSTAGE,Botlake,
     +                                          Splakout,dy)
                    DSRFOT(LAKE) = DSRFOT(LAKE) + dy
                  END IF
                  SURFOT(LAKE)= SURFOT(LAKE) + STROUT(INODE)+ 
     +                          FXLKOT(INODE)
                  IF(SURFOT(LAKE).LT.CLOSEZERO )SURFOT(LAKE)=0.0
                END IF
              END DO
            END IF
            IF ( THET1.GT.CLOSEZERO ) THEN
              IF ( NCNV == 1 ) THEN
! Calc overland flow again.
C EDM - Add indices to RUNF and RUNOFF because these terms need to be 
C       saved for each lake in the simulation for writing to the FTL file by LMT
                IF(RNF(LAKE).GE.0.0) RUNF(LAKE) = RNF(LAKE)
                IF(RNF(LAKE).LT.0.0) RUNF(LAKE) =-RNF(LAKE)*
     +                               PRCPLK(LAKE)*BGAREA(LAKE)
                IF (IUNITUZF.GT.0) THEN
                  RUNOFF(LAKE) = OVRLNDRNF(LAKE)
                ELSE
                  RUNOFF(LAKE) = 0.0
                END IF
C
C2C --- SUM OF BOTH STREAMFLOW IN AND OVERLAND RUNOFF.
C         (INCLUDES LAKE VOLUME).
        FLWIN(LAKE) = SURFIN(LAKE)+RUNF(LAKE)+RUNOFF(LAKE)+
     +                VOLTERP(STGOLD(LAKE),LAKE)/DELT
C
C16-----COMPUTE NEW LAKE STAGE USING NEWTON METHOD AND THET1>0.
C
C16B----COMPUTE RESIDUALS FOR TRANSIENT SIMULATIONS.
                IF(ISS.EQ.0) THEN
                  VOLNEW1 = VOLTERP(STGNEW(LAKE),LAKE)
                  RESID1 = (PRECIP(LAKE)-EVAP(LAKE)+RUNF(LAKE)+
     1                RUNOFF(LAKE)-WITHDRW(LAKE)+SURFIN(LAKE)-
     2                SURFOT(LAKE)+SEEP(LAKE))-
     3                (VOLNEW1-VOLOLDD(LAKE))/DELT
                  OUTFLOW = SURFOT(LAKE)+ DSRFOT(LAKE)*DLSTG
                  IF(OUTFLOW.LT.0.0)SURFOT(LAKE)=0.0
                  STAGE2 = STGNEW(LAKE)+DLSTG
                  VOLNEW2 = VOLTERP(STAGE2,LAKE)
                  RESID2 = (PRECIP3(LAKE)-EVAP3(LAKE)+RUNF(LAKE)+
     1                RUNOFF(LAKE)-WITHDRW3+SURFIN(LAKE)-OUTFLOW+
     2                SEEP3(LAKE))-(VOLNEW2-VOLOLDD(LAKE))/DELT
C
C16C----COMPUTE RESIDUALS FOR STEADY STATE SIMULATIONS.
                ELSE
                  RESID1 = (PRECIP(LAKE)-EVAP(LAKE)+RUNF(LAKE)+
     1                RUNOFF(LAKE)-WITHDRW(LAKE)+SURFIN(LAKE)-
     2                SURFOT(LAKE)+SEEP(LAKE))
                  OUTFLOW = SURFOT(LAKE)+ DSRFOT(LAKE)*DLSTG
                  RESID2 = (PRECIP3(LAKE)-EVAP3(LAKE)+RUNF(LAKE)+
     1                RUNOFF(LAKE)-WITHDRW3+SURFIN(LAKE)-
     2                OUTFLOW+SEEP3(LAKE))
                END IF
C
C16D----DETERMINE DERIVATIVE AND COMPUTE NEW LAKE STAGE.
                IF ( DABS(RESID2-RESID1).GT.0.0 ) THEN
                  DERIV = (RESID2-RESID1)/(DLSTG)
                  DSTG = RESID1/DERIV
                  STGNEW(LAKE) = STGITER(LAKE) - DSTG
                  DSTG = ABS(DSTG)
                ELSE
C16E----LINEAR CASE. SIMPLY CALCULATE STAGE BASED ON VOLUME.
                  VOL2 = RESID1
                  IF ( VOL2.LT.0.0 ) VOL2 = 0.0
                  STGNEW(LAKE) = STGTERP(VOL2,LAKE)
                  DSTG = ABS(STGNEW(LAKE) - STGITER(LAKE))
                  NCNCVR(LAKE) = 1
                END IF
 !     IF (lake==3)then
 !     write(iout,222)PRECIP(LAKE),EVAP(LAKE),RUNF(LAKE),RUNOFF(LAKE),
 !    1                WITHDRW(LAKE),SURFIN(LAKE),SURFOT(LAKE),
 !    2                SEEP(LAKE),VOLNEW1,VOLOLDD(LAKE),STGNEW(LAKE),
 !    3                resid1,SURFA(LAKE),deriv,dstg
 !     write(iout,222)PRECIP3(LAKE),EVAP3(LAKE),RUNF(LAKE),RUNOFF(LAKE),
 !    1                WITHDRW3,SURFIN(LAKE),OUTFLOW,
 !    2                SEEP3(LAKE),VOLNEW2,VOLOLDD(LAKE),STGNEW(LAKE),
 !    3                resid2,SRFPT,deriv,dstg
 !     END IF
 !222  format(15e20.10)
                IF(STGNEW(LAKE).LT.BOTTMS(LAKE)) 
     +             STGNEW(LAKE)=BOTTMS(LAKE)
                IF(DSTG.LE.SSCNCR) NCNCVR(LAKE) = 1
              END IF
C
C17-----COMPUTE NEW LAKE STAGE EXPLICITLY WITH THET1=0.
            ELSE
              IF(ISS.EQ.0) THEN
C
C17B----COMPUTE LAKE VOLUME FOR TRANSIENT SIMULATIONS.     
C EDM - Add indices to RUNF and RUNOFF because these terms need to be 
C       saved for each lake in the simulation for writing to the FTL file by LMT
                VOL2 = DELT*(PRECIP(LAKE)-EVAP(LAKE)+RUNF(LAKE)+
     1                RUNOFF(LAKE)-WITHDRW(LAKE)+SURFIN(LAKE)-
     2                SURFOT(LAKE)+SEEP(LAKE))+ VOLOLDD(LAKE)
C
C17C----COMPUTE LAKE VOLUME FOR STEADY STATE SIMULATIONS.
              ELSE
                VOL2 = (PRECIP(LAKE)-EVAP(LAKE)+RUNF(LAKE)+
     1                RUNOFF(LAKE)-WITHDRW(LAKE)+SURFIN(LAKE)-
     2                SURFOT(LAKE)+SEEP(LAKE))
              END IF
C
C17D----NEW LAKE STAGE COMPUTED FROM LAKE VOLUME.
              STGNEW(LAKE) = STGTERP(VOL2,LAKE)
              IF(STGNEW(LAKE).LT.BOTTMS(LAKE)) 
     +             STGNEW(LAKE)=BOTTMS(LAKE)
            END IF
        END DO
        IF ( IICNVG==1 ) EXIT CONVERGE
      END DO CONVERGE
C
C18-----PRINT NEW LAKE STAGE AND PREVIOUS LAKE ITERATION STAGE
C        WHEN LAKE STAGE DOES NOT CONVERGE.
      DO LAKE=1,NLAKES
        IF( L1.GE.MTER.AND.NCNCVR(LAKE).EQ.0 ) THEN
          WRITE(IOUT,1004) KKITER,  
     1    LAKE,STGNEW(LAKE), STGITER(LAKE)
        END IF
      END DO
C
C19------FORMAT STATEMENTS
Cdep  101   FORMAT(4I5,3E20.10)    !format used for debugging
Cdep  202  FORMAT(i5,8(1X,E20.10)) !format used for debugging
  506  FORMAT(1X,'ERROR - NO AQUIFER UNDER LAKE CELL ',4I5)
 1004  FORMAT(1X,'ITERATION ',I4,2X,'LAKE ',I4,2X,'NEW STAGE ',1PE15.8, !gsf
     1  '  DID NOT CONVERGE-- PREVIOUS INTERNAL ITERATION STAGE  ',
     2  1PE15.8,/) !gsf
      END SUBROUTINE GWF2LAK7FM
C
      SUBROUTINE GWF2LAK7BD(KSTP,KPER,IUNITGWT,IUNITGAGE,IUNITSFR,
     1                     IUNITUZF,NSOL,IGRID)
C
C----- USGS VERSION 7.1; JUNE 2006 GWF2LAK7BD
C      Revisions MARCH through AUGUST, 2009  DEP&RGN
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR LAKES
C     ******************************************************************
C
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFLAKMODULE
      USE GLOBAL,       ONLY: NCOL, NROW, NLAY, NODES, IBOUND, IOUT,
     +                        ISSFLG, DELR, DELC, LBOTM, BOTM, HNEW,
     +                        BUFF,IUNIT
     
      USE GWFBASMODULE, ONLY: MSUM, ICBCFL, IAUXSV, DELT, PERTIM, TOTIM,
     +                        HNOFLO, VBVL, VBNM
      USE GWFSFRMODULE, ONLY: STRIN, DLKSTAGE, SLKOTFLW
      USE GWFUZFMODULE, ONLY: SURFDEP,IUZFBND,FINF,VKS
      IMPLICIT NONE
      !rsr: argument IUNITSFR not used
      CHARACTER*16 TEXT
Cdep  Added functions for interpolating between areas, derivatives,
Cdep     and outflow rates
C     ------------------------------------------------------------------
C     FUNCTIONS
C     -----------------------------------------------------------------
      DOUBLE PRECISION FINTERP, DERIVTERP, OUTFLWTERP, VOLTERP, STGTERP,
     +                 SURFTERP
      EXTERNAL FINTERP, DERIVTERP, OUTFLWTERP, VOLTERP, STGTERP, 
     +         SURFTERP
C     -----------------------------------------------------------------
C     -----------------------------------------------------------------
C     ARGUMENTS
C     -----------------------------------------------------------------
      DOUBLE PRECISION BOTLK,BOTCL,CONDUC,H,FLOBOT,STGON,
     1RATE,RATIN,RATOUT
      DOUBLE PRECISION THET1,SURFDPTH,CONDMX,BOTLKUP,BOTLKDN,VOL2
      DOUBLE PRECISION FLOTOUZF,SILLELEV,ADJSTAGE, voltest, areatest
      DOUBLE PRECISION RAMPGW,RAMPSTGO,RAMPSTGN,RAMPSTGON,HTEMP
      DOUBLE PRECISION CLOSEZERO, FLOBO2, FLOBO3, DLSTG
      DOUBLE PRECISION RUNFD, AREA, RAIN
      REAL zero, FACE, R, WDRAW, OLDSTAGE, AVHD, TOTARE, SUM
      REAL STGTST, SVT1, TVOLM, STO, FLSUM, TV, PPTIN, EOUT
      REAL SEEPUZF, QIN, QOUT, QSIN, QSOUT, DENOM
      INTEGER L1, IGRID, ISS, KPER, IBD, KCNT, LDR, NAUX, KSTP, IL
      INTEGER IR, IC, LK, ITRIB, INODE, LAKE, IUNITUZF, II, L
      INTEGER IUNITGWT, LL, K, J, I, JCLS, ICL, IC4, ICM4
      INTEGER ITYPE, INOFLO, IC5, ICM, IS1, IS2, ICl1, LK3, LK1
      INTEGER ICM2, IC2, IC3, ICNR, ICM3, ICNT, ICM1, L11, ICNR1
      INTEGER ILB, IRB, ICB, LDR1, NN, IIC, JIC, LIC, IUNITGAGE
      INTEGER NSOL, ILL, IL2, IUNITSFR, IL1
      DIMENSION JCLS(NCLS,ICMX)
      DIMENSION ILB(5),IRB(5),ICB(5)
      CHARACTER*16 LAKAUX(20)
      DIMENSION FACE(1)
      DATA TEXT /'   LAKE  SEEPAGE'/
      DATA LAKAUX(1)/'IFACE'/
!      PARAMETER (CLOSEZERO=1.0E-7)
C     ------------------------------------------------------------------
C
C------SET POINTERS FOR THE CURRENT GRID.
      CALL SGWF2LAK7PNT(IGRID)
      ISS = ISSFLG(KPER)
      CLOSEZERO = 1.0D-09
      SURFDPTH = DBLE(SURFDEPTH)
C
C1------SET IBD=1 IF BUDGET TERMS SHOULD BE SAVED ON DISK.
      ZERO = 0.0
      IBD=0
      KCNT = 0
      RATIN = 0.
      RATOUT =0.
      LAKSEEP = 0.0
      DLSTG = 0.00001D0
C1A-----Set Lake budget terms for GSFLOW to zero.
      TOTGWIN_LAK = 0.0
      TOTGWOT_LAK = 0.0
      TOTDELSTOR_LAK = 0.0
      TOTSTOR_LAK = 0.0
      TOTEVAP_LAK = 0.0
      TOTPPT_LAK = 0.0
      TOTRUNF_LAK = 0.0
      TOTWTHDRW_LAK = 0.0
      TOTSURFIN_LAK = 0.0
      TOTSURFOT_LAK = 0.0
Cdep  initialize CONDMX, BOTLKUP, AND BOTLKDN TO ZERO. 3/3/2009
      FLOBO2 = 0.0D0
      CONDMX = 0.0D0
      BOTLKUP = 0.0D0
      BOTLKDN = 0.0D0
      L1 = 0
Cdep  initialize SURFDPTH TO 1.0D0 3/3/2009
      SURFDPTH = DBLE(SURFDEPTH)
C
      DO 104 LDR = 1,NODES
  104 LDRY(LDR) = 0
      LDR = 0
C
C1B-----TEST TO SEE IF CELL-BY-CELL TERMS ARE NEEDED.
      IF(ILKCB.GT.0) IBD=ICBCFL
C1C-----IF COMPACT BUDGET, WRITE LIST HEADER
      IF(IBD.EQ.2) THEN
C-LFK         NAUX=0
C-LFK         IF(IAUXSV.NE.0) NAUX=1
         NAUX=1
         CALL UBDSV4(KSTP,KPER,TEXT,NAUX,LAKAUX,ILKCB,NCOL,NROW,NLAY,
     1               LKNODE,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      END IF
C
C1D------IF NO LAKE NODES, KEEP ZERO IN ACCUMULATORS.
      IF (LKNODE.EQ.0) GO TO 1200
C
C1E-----CLEAR CELL-BY-CELL BUFFER.
      DO 5 IL=1,NLAY
      DO 5 IR=1,NROW
      DO 5 IC=1,NCOL
5        BUFF(IC,IR,IL)=ZERO
C
C2------PROCESS EACH CELL IN THE ILAKE LIST.
      DO 100 LK=1,NLAKES
        FLXINL(LK)=ZERO
        SURFIN(LK)=ZERO
Cdep   Initialize flow limiting and 3 dummy arrays to zero 
Cdep      for gage package
        GWRATELIM(LK) = ZERO
        XLAKES(lk,1) = ZERO
        XLKOLD(lk,1) = ZERO
        XLAKINIT(lk,1) = ZERO
100     CONTINUE
C2A------SUM UP INFLOWS FROM INFLOWING STREAM REACHES.
Cdep   removed delta from computation of SURFIN
C - EDM Initialize variables needed for LMT
      NSFRLAK = 0
      ILKSEG = 0
      ILKRCH = 0
      LAKSFR = 0
      SWLAK = 0.
      IF (IUNITSFR.GT.0) THEN
        DO 200 LK=1,NLAKES
           DO 200 ITRIB=1,NTRB
              INODE=ITRB(LK,ITRIB)
              IF (INODE.LE.0) GO TO 200
              SURFIN(LK)=SURFIN(LK)+STRIN(INODE)
C--EDM - SET UP VARIABLES NEEDED BY LMT
              IF (IUNIT(49).NE.0) THEN
                NSFRLAK = NSFRLAK + 1
                ILKSEG(NSFRLAK) = INODE
                LAKSFR(NSFRLAK) = LK
                SWLAK(NSFRLAK) = -STRIN(INODE)
              ENDIF
200     CONTINUE
      END IF
C
C2B------SET DOUBLE PRECISION THET1 TO THETA AND IF LESS THAN
C          0.5, SET THET1 TO 0.0D0 (EXPLICIT LAKE STAGE).
      THET1 = DBLE(THETA)
      IF ( ISS==1 ) THET1 = 1.0
      IF(THET1-0.5.LT.-CLOSEZERO) THET1=0.0D0
C2C------INITIALIZE SUMMATION PARAMETERS.
          DO LAKE = 1,NLAKES
            SUMCNN(LAKE) = ZERO
            SUMCHN(LAKE) = ZERO
            EVAP(LAKE)=ZERO
            PRECIP(LAKE)=ZERO
            SEEP(LAKE)=ZERO
            SEEPUZ(LAKE)=ZERO
            VOL(LAKE)=ZERO
            SURFA(LAKE)=ZERO
            GWIN(LAKE)=ZERO
            GWOUT(LAKE)=ZERO
            WITHDRW(LAKE) = WTHDRW(LAKE)
            IF(RNF(LAKE).GE.0.0) RUNF(LAKE) = RNF(LAKE)
            IF(RNF(LAKE).LT.0.0) RUNF(LAKE) =-RNF(LAKE)*PRCPLK(LAKE)*
     +                                  BGAREA(LAKE)
            IF (IUNITUZF.GT.0) THEN
              RUNOFF(LAKE) = OVRLNDRNF(LAKE)
            ELSE
              RUNOFF(LAKE) = 0.0
            END IF
            FLWIN(LAKE) = SURFIN(LAKE)+RUNF(LAKE)+RUNOFF(LAKE)+
     +                    VOLTERP(STGOLD(LAKE),LAKE)/DELT
            IF ( ISS==1 ) THEN
              FLWIN(LAKE) = 1.0E10 
            END IF
            FLWITER(LAKE) = FLWIN(LAKE)   !RGN 5/21/12
          END DO
C
C3------MASTER NODE LOOP -- COMPUTE LAKEBED SEEPAGE TERMS AND 
C         BUDGET TERMS.
         IF (ILKCB.LT.0.AND.ICBCFL.NE.0) WRITE (IOUT,'(//)')
C3B-----II LOOP IS TO BALANCES INFLOWS AND OUTFLOWS TO/FROM A LAKE.
C         WHEN II=1, FLOW INTO LAKE FROM ALL SOURCES ARE CALCULATED.
C         WHEN II=2, SEEPAGE TO AND FROM LAKE AND RESIDUAL TERMS ARE  
C         ADDED TO GROUNDWATER MATRIX.LAKE SEEPAGE TO GROUNDWATER 
C         LIMITED TO AVAILABLE WATER IN LAKE.
         DO II = 1,2
           DO L=1,LKNODE
             IL=ILAKE(1,L)
             IR=ILAKE(2,L)
             IC=ILAKE(3,L)
C
C4------DETERMINE LAKE AND NODAL LAYER,ROW,COLUMN NUMBER.
             LAKE=ILAKE(4,L)
C
C5-------SET STGOLD TO STGNEW FOR STEADY STATE SIMULATIONS.
C          COMPUTE STGON AS A FRACTION OF STGOLD AND STGNEW.
             IF(ISS.EQ.1 ) STGOLD(LAKE)=STGNEW(LAKE)
             STGON = (1.0D0-THET1)*STGOLD(LAKE) + THET1*STGNEW(LAKE)
             ITYPE = (ILAKE(5,L)+1)/2
             IF(ITYPE.EQ.3) ITYPE=0
             AREA = DELR(IC)*DELC(IR)
             IF(IL.GT.1) THEN
               BOTLK = BOTM(IC,IR,LBOTM(IL-1))
             ELSE
               BOTLK = BOTM(IC,IR,LBOTM(IL))
             END IF
             BOTCL = BOTM(IC,IR,LBOTM(IL))
             IF(IL.EQ.NLAY.AND.CNDFCT(L).EQ.0.0) BOTLK = BOTCL
             RAIN = PRCPLK(LAKE)
!             EV = EVAPLK(LAKE)
C
C5B------CONDUCTANCE FACTOR NEEDED FOR SEEPAGE CALCULATIONS.
C         FLOB01 USED TO CALCULATE SEEPAGE WITH STGOLD.
C         FLOBO2 USED TO CALCULATE SEEPAGE WITH STGNEW.
C         FLOBOT IS A FRACTION OF BOTH FLOBO1 AND FLOBO2 AND 
C           IS DEPENDENT ON VALUE OF THET1.
             CONDUC=CNDFCT(L) 
             FLOBOT = 0.0D0
             FLOBO3 = 0.0D0
             FLOTOUZF = 0.0D0 
             RATE = 0.0
             IL1 = IL
             IF ( ITYPE.EQ.0 ) THEN       !RGN 2-18-2014 added this if check
               DO WHILE (IL1 .LE. NLAY)               
                 IF( IBOUND(IC,IR,IL1).GT.0 ) THEN 
                   EXIT
                 ELSE
                   IL1 = IL1 + 1
                 END IF
               END DO
               IF ( IL1.GT.NLAY ) IL1 = NLAY 
             END IF
             IF( IBOUND(IC,IR,IL1).LE.0 ) THEN
 !  Commented next line out 12/27/10
 !              IF ( CONDUC.GT.CLOSEZERO )WRITE(IOUT,506) L,IC,IR,IL
               CONDUC = 0.0
 ! 506          FORMAT(1X,'ERROR - NO AQUIFER UNDER LAKE CELL ',4I5)
             END IF
             IF(CONDUC.GT.0.0) THEN
               H=HNEW(IC,IR,IL1)   ! IL changed to IL1 !RGN 1/9/2017
C
C5C------DETERMINE UPPERMOST ACTIVE CELL IF NOT CELL(IL)
C
               CALL GET_FLOBOT(IC, IR, IL1, ITYPE, INOFLO,CONDUC,
     1                FLOBOT,FLOBO3,FLOTOUZF,DLSTG,CLOSEZERO,H,
     2                THET1,ISS,LAKE,II,SURFDPTH,AREA,IUNITUZF,
     3                BOTLK,BOTCL,L1)
C
C9-------SET RATE TO FLOBOT AND SET FLOB(L) TO FLOBOT WHEN 
C          SOLUTE TRANSPORT IS ACTIVE. 
               IF ( II==2 ) THEN
                 SEEP(LAKE)=SEEP(LAKE)-FLOBOT
! Save seepage to UZF for writing Lake-to-UZF in GAG Package.
                 SEEPUZ(LAKE)=SEEPUZ(LAKE)+FLOTOUZF 
                 RATE=FLOBOT    
                 IF (IUNITGWT.GT.0.OR.IUNIT(49).GT.0) FLOB(L)=FLOBOT !EDM
                 IF (ILKCB.LT.0.AND.ICBCFL.NE.0) WRITE(IOUT,880)
     1            TEXT,KPER,KSTP,L,IL,IR,IC,RATE
880        FORMAT(1X,A,'   PERIOD',I3,'   STEP',I6,'   NODE',I4, !gsf
     1            '   LAYER',I3,'   ROW',I4,'   COL',I4,'   RATE',
     2            G15.7)
C
C10------ADD RATE TO BUFFER.
                 BUFF(IC,IR,IL1)=BUFF(IC,IR,IL1)+RATE
                 LAKSEEP(IC,IR) = LAKSEEP(IC,IR)+ RATE + FLOTOUZF  !10/4/2014
C
C10B-----CHECK IF RATE IS DISCHARGING FROM AQUIFER (NEGATIVE RATE).
Cdep            IF (RATE) 885,899,890
                 IF(RATE.LT.0.0D0) THEN
C
C10C-----SUBTRACT RATE FROM RATOUT.
Cdep 885         RATOUT=RATOUT-RATE
                 RATOUT=RATOUT-RATE
                 GWIN(LAKE)=GWIN(LAKE)-RATE        
Cdep         GO TO 899
C
C10D------CHECK IF RATE IS RECHARGING AQUIFER (POSITIVE RATE).
                 ELSE IF (RATE+FLOTOUZF.GT.0.0D0) THEN  !11/3/2014 added flotouzf
C
C10E------ADD RATE TO RATIN.
Cdep 890         RATIN=RATIN+RATE
                   RATIN=RATIN+RATE
                   GWOUT(LAKE)=GWOUT(LAKE)+RATE
                 END IF
C11-------IF SAVING COMPACT BUDGET, WRITE FLOW FOR ONE LAKE FACE.
               END IF
             END IF
899          IF(IBD.EQ.2.and.II.EQ.2) THEN
               FACE(1)=ILAKE(5,L)
               R=RATE
               CALL UBDSVB(ILKCB,NCOL,NROW,IC,IR,IL1,R,FACE(1),1,
     1                     NAUX,1,IBOUND,NLAY)
                 END IF    
             END DO
         END DO
C
C12------COMPUTE EVAPORATION AND PRECIPITATION USING STGOLD AND 
C          ADD PRECIPITATION TO FLWIN.
         DO LL = 1,NLAKES
           SURFA(LL)=FINTERP(STGNEW(LL),LL)
           EVAP(LL) = EVAPLK(LL)*SURFA(LL)
           PRECIP(LL) = PRCPLK(LL)*SURFA(LL)
           FLWIN(LL) = FLWIN(LL) + PRECIP(LL)
C
C13------LIMIT WITHDRW TO LAKE INFLOW WHEN WITHDRAWALS EXCEED
C           INFLOW (INCLUDING AVAILABLE LAKE STORAGE).
           IF( WITHDRW(LL)-DBLE(FLWIN(LL)).GT.1.0E-04 ) THEN
             WITHDRW(LL) = FLWIN(LL)
             FLWIN(LL) = 0.0
           ELSE
             FLWIN(LL)  = FLWIN(LL) - WITHDRW(LL)
           END IF
C
C14------LIMIT EVAPORATION TO LAKE INFLOW WHEN EVAPORATION EXCEEDS
C          INFLOW (INCLUDING AVAILABLE LAKE STROAGE AND WITHDRAWALS).          
           IF ( EVAP(LL)-DBLE(FLWIN(LL)).GT.1.0E-04 ) THEN
             EVAP(LL)=FLWIN(LL)
             FLWIN(LL) = 0.0D0
           ELSE
             FLWIN(LL)=FLWIN(LL)-EVAP(LL)
           END IF
         END DO
Cdep  August 28, 2009  moved to end of do loop
C15-----COMPUTE CHANGE IN STAGE DURING TIME STEP AND FOR SIMULATION.
C       SKIP IF STEADY STATE SIMULATION.
C      IF(ISS.LE.0) GO TO 905
C      DO 903 LAKE=1,NLAKES
C            DELH(LAKE)=STGNEW(LAKE)-STGOLD(LAKE)
C            TDELH(LAKE)=STGNEW(LAKE)-STAGES(LAKE)
C  903 CONTINUE
! RGN commented next line out. 5/4/09. 
!      GO TO 1350
C
C16-----COMPUTE STGON AS FRACTION OF STGOLD AND STGNEW.
  905   DO 1000 LAKE=1,NLAKES
           VOL2 = 0.0
Cdep       STGON = (1.0-THETA)*STGOLD(LAKE) + THETA*STGNEW(LAKE)
Cdep       Changed THETA to THET1
         STGON = (1.0D0-THET1)*STGOLD(LAKE) + THET1*STGNEW(LAKE)
C
C17-----COMPUTE RUNOFF INTO LAKE FROM LAKE PACKAGE AND FROM UZF.
Cdep   Changed WTHDRW(LAKE) TO WITHDRW(LAKE)
          WDRAW=WITHDRW(LAKE)
       IF(RNF(LAKE).GE.ZERO) RUNF(LAKE) = RNF(LAKE)
       IF(RNF(LAKE).LT.ZERO) RUNF(LAKE) =-RNF(LAKE)*PRCPLK(LAKE)
     +                                   *BGAREA(LAKE)
Cdep  Added runoff from Unsaturated Flow Package
       IF (IUNITUZF.GT.0) THEN
         RUNOFF(LAKE) = OVRLNDRNF(LAKE)
       ELSE
         RUNOFF(LAKE) = 0.0
       END IF
Cdep  Created RUNFD and added to STGNEW
C-LFK         RUNFD = RUNF(LAKE)+ RUNOFF(LAKE)
         RUNFD = RUNF(LAKE) + RUNOFF(LAKE)
C
C18------COMPUTE LAKE VOLUME FROM ALL INFLOWS AND OUTFLOWS FOR 
C          TRANSIENT SIMULATION AND THEN COMPUTE STGNEW FROM
C          NEW VOLUME.
!RGN Volume made equal to sum of inflows and outflows plus
!RGN   plus lake storage from previous time step  4/17/09
         IF(ISS.EQ.0)THEN
           VOL2 = VOLOLDD(LAKE)+(PRECIP(LAKE)-EVAP(LAKE)
     +                -WDRAW+RUNFD+SURFIN(LAKE)-SURFOT(LAKE)+GWIN(LAKE)    !10/4/2014 added SEEPUZ(LAKE)
     +                -GWOUT(LAKE)-SEEPUZ(LAKE))*DELT
          IF(VOL2.LE.0.0) VOL2=0.0
          VOL(LAKE) = VOL2
          STGNEW(LAKE)= STGTERP(VOL2,LAKE)
C
C18B-----COMPUTE LAKE VOLUME FROM ALL INFLOWS AND OUTFLOWS FOR 
C          STEADY STATE SIMULATION.
         ELSE
           VOL2 = VOLTERP(STGNEW(LAKE),LAKE)
           IF(VOL2.LE.0.0D0) VOL2 = 0.0D0
           VOL(LAKE) = VOL2
         END IF
C18C-----STGON IS FRACTION OF STGOLD AND STGNEW AND SURFACE AREA
C
C          IS BASED ON STGOLD.
         STGON = (1.0D0-THET1)*STGOLD(LAKE) + THET1*STGNEW(LAKE)
         SURFA(LAKE)=FINTERP(STGNEW(LAKE),LAKE)
         IF(STGNEW(LAKE)-BOTTMS(LAKE).LT.CLOSEZERO) GO TO 1110
C
C19------COMPUTE LAKE BUDGET VOLUMES FOR GSFLOW CSV FILE.
Cdep  EVAP, PRECIP,WDRW AND RUNFD are volumetric rates 4/19/2009
      TOTGWIN_LAK = TOTGWIN_LAK + GWIN(LAKE)*DELT
      TOTGWOT_LAK = TOTGWOT_LAK - GWOUT(LAKE)*DELT
      TOTDELSTOR_LAK = TOTDELSTOR_LAK + VOL(LAKE) - VOLOLDD(LAKE)
      TOTSTOR_LAK = TOTSTOR_LAK + VOL(LAKE)
      TOTEVAP_LAK = TOTEVAP_LAK - EVAP(LAKE)*DELT
      TOTPPT_LAK = TOTPPT_LAK + PRECIP(LAKE)*DELT
      TOTRUNF_LAK = TOTRUNF_LAK + RUNFD*DELT
      TOTWTHDRW_LAK = TOTWTHDRW_LAK - WDRAW*DELT
      TOTSURFIN_LAK = TOTSURFIN_LAK + SURFIN(LAKE)*DELT
      TOTSURFOT_LAK = TOTSURFOT_LAK - SURFOT(LAKE)*DELT
C
C20------WRITE WHEN LAKE VOLUME HAS INITIALLY GONE DRY.
      IF(VOL(LAKE).LE.ZERO) WRITE(IOUT,1114) LAKE
 1114 FORMAT(1X,'..........LAKE',I3,' JUST GONE DRY..........')
!dep  August 27, 2009   set delh and tdelh to zero for steady state
        IF (ISS.EQ.1) THEN 
          IF(KPER.EQ.1) THEN
            STAGES(LAKE)=STGNEW(LAKE)
          END IF
          DELH(LAKE)= 0.0
          TDELH(LAKE)= 0.0
!dep  August 27, 2009  set delh and tdelh for transient simulations
        ELSE
          OLDSTAGE= STGOLD(LAKE)
          DELH(LAKE)=STGNEW(LAKE)-OLDSTAGE
          TDELH(LAKE)=STGNEW(LAKE)-STAGES(LAKE)
        END IF
      GO TO 1000
C
C21----WRITE WHEN LAKE CONTINUES TO BE DRY.
 1110 AVHD = ZERO
!      BOTARE = ZERO
      WRITE(IOUT,1112) LAKE
 1112 FORMAT(1X,'..........LAKE',I3,' IS DRY..........') 
      IF(NLAY.EQ.1) GO TO 1000
      DO 1115 L=1,LKNODE
      L1 = ILAKE(4,L)
C  Convert ILAKE(5,L):  1 and 2 are type 1,  3 and 4 are type 2, 6 is type 0
      ITYPE = (ILAKE(5,L)+1)/2
      IF(ITYPE.EQ.3) ITYPE=0
      IF(L1.NE.LAKE.OR.ITYPE.NE.0) GO TO 1115
      K = ILAKE(1,L)
      J = ILAKE(2,L)
      I = ILAKE(3,L)
      IF(K.EQ.NLAY.AND.IBOUND(I,J,K).EQ.0) GO TO 1000
      IF(K.EQ.1) GO TO 1115
      IF(BOTM(I,J,LBOTM(K-1)).GT.BOTTMS(LAKE)) GO TO 1115
Cdep   lake no longer refills on basis of average head beneath aquifer.
C      CHECK FOR CONDITION (AVERAGE HEAD IN UNDERLYING AQUIFER
C      GREATER THAN BOTTOM OF LAKE) FOR REWETTING A DRY LAK
c lfk  eliminate DO 1117 loop, ignore LAYHDT value below lake; and only
c           check layer below lake interface for IBOUND value:
c orig lines
c      DO 1117 K1=K,NLAY
c      IF(LAYHDT(K1).EQ.0) GO TO 1117
c      K2 = K1
c      IF(IBOUND(I,J,K1).LE.0) GO TO 1117
c      K2 = K
c      IF(IBOUND(I,J,K).LE.0) GO TO 1117
c      GO TO 1119
c 1117 CONTINUE
c      AVHD = AVHD + BOTM(I,J,LBOTM(K2))*DELR(I)*DELC(J)
c      GO TO 1121
c 1119 AVHD = AVHD + HNEW(I,J,K1)*DELR(I)*DELC(J)
c
c new lines
cdep      IF(IBOUND(I,J,K).LE.0) THEN
cdep        AVHD = AVHD + BOTM(I,J,LBOTM(K))*DELR(I)*DELC(J)
cdep      ELSE
cdep        AVHD = AVHD + HNEW(I,J,K)*DELR(I)*DELC(J)
cdep     END IF
C
Cdep  1121 BOTARE = BOTARE + DELR(I)*DELC(J)
Cdep     BOTARE = BOTARE + DELR(I)*DELC(J)
 1115 CONTINUE
Cdep      IF(BOTARE.LE.ZERO) GO TO 1000
Cdep      AVHD = AVHD/BOTARE
Cdep     IF(AVHD.LT.BOTTMS(LAKE)) GO TO 1125
Cdep      WRITE(IOUT,1122)
Cdep 1122 FORMAT(/1X,'AQUIFER HEAD UNDERNEATH BOTTOM OF LAKE IS HIGHER THAN
Cdep     1LAKE BOTTOM ELEVATION')
Cdep  Revised to not reset lake to average ground-water level beneath lake.
Cdep 1120 STGNEW(LAKE) = BOTTMS(LAKE)
Cdep      SURFA(LAKE) = BOTARE
Cdep      VOL(LAKE) = (STGNEW(LAKE)-BOTTMS(LAKE))*SURFA(LAKE)
Cdep  revised program to allow lakes to rewet while keeping track of lake
Cdep   inflows and outflow
Cdep      WRITE(IOUT,1184) LAKE, STGNEW(LAKE)
Cdep 1184 FORMAT(/1X,'LAKE',I3,' HAS REWET.  SET STAGE OF LAKE TO',F10.2,
Cdep     1  2X,'FT'/)
Cdep     GO TO 1000
C
C-----CHECK FOR STREAM OR AUGMENTATION INFLOWS
C
Cdep 1125 TSTVOL = SURFIN(LAKE)
Cdep     IF(WDRAW.LT.ZERO) TSTVOL = TSTVOL - WDRAW
Cdep     IF(TSTVOL.LE.ZERO) GO TO 1000
Cdep     STGNEW(LAKE) = BOTTMS(LAKE)
Cdep    SURFA(LAKE) = BOTARE
Cdep      AVHD = BOTTMS(LAKE) + TSTVOL/BOTARE
Cdep  revised program to allow lakes to rewet while keeping track of lake
Cdep   inflows and outflow    
Cdep   WRITE(IOUT,1123)
Cdep 1123 FORMAT(/1X,'THERE ARE NON-ZERO SURFACE-WATER INFLUXES INTO THE DRY
Cdep     1 LAKE')
Cdep      GO TO 1120
C
 1000     CONTINUE
C
C22------ADJUST STAGES OF COALESCENT MULTIPLE-LAKE SYSTEMS.
C
      KCNT = 0
      IF(NCLS.LE.0) GO TO 1350
      DO 1205 I=1,NCLS
      DO 1205 J=1,ICMX
 1205 JCLS(I,J) = 0
C
C22B-----CHECK EACH LAKE SYSTEM (ICL) FOR CURRENT CONNECTIONS TO 
C          SUBLAKES.
      DO 1300 ICL=1,NCLS
      DO 1206 K=1,NLAKES
      SVT(K) = ZERO
      NCNST(K) = 0
 1206 NCNT(K) = 0
C
C22C-----ELIMINATE (JCLS=2) ALL LAKES THAT HAVE ALREADY HAD THEIR STAGES
C          ADJUSTED AS PART OF A CONNECTED SYSTEM OF LAKES AND SUBLAKES.
      DO 1210 IC4=ICL,NCLS
      ICM4 = ICS(IC4)
      DO 1210 IC5=1,ICM4
      IF(JCLS(IC4,IC5).EQ.1) JCLS(IC4,IC5) = 2
 1210 CONTINUE
Cdep 1215 IF(JCLS(ICL,1).GE.2) GO TO 1300
      IF(JCLS(ICL,1).GE.2) GO TO 1300
C
C22D-----TAG CENTER LAKE BY SETTING JCLS=1 AND THEN CHECK SUBLAKES FOR
C   CONNECTIONS.  IF CONNECTED, SET JCLS=1 AND NCNT=1.
      ICM = ICS(ICL)
      IS1 = ISUB(ICL,1)
      JCLS(ICL,1) = 1
      NCNT(IS1) = 1
      SVT(IS1) = -99999.
      DO 1220 J=2,ICM
      IS2 = ISUB(ICL,J)
      IF(IS2.LE.0) GO TO 1225
      IF(STGNEW(IS1).LE.SILLVT(ICL,J-1).AND.STGNEW(IS2).LE.
     1  SILLVT(ICL,J-1)) GO TO 1220
      JCLS(ICL,J) = 1
      NCNT(IS2) = 1
      SVT(IS2) = SILLVT(ICL,J-1)
 1220 CONTINUE
 1225 IF(ICL.EQ.NCLS) GO TO 1240
C
C22E-----CHECK TO SEE IF CENTER LAKES OF REMAINING LAKE SYSTEMS
C          ARE THE SAME AS CONNECTED SUBLAKES OF THE PRESENT LAKE SYSTEM.
C          IF SO, CHECK THEIR SUBLAKES FOR CONNECTIONS TO THE CENTER 
C          LAKES. IF SUBLAKES ARE ADDED TO THE SYSTEM, THEN CHECK 
C          REMAINING CENTER LAKES FOR AN IDENTITY WITH THE NEWLY
C          ADDED SUBLAKES. ALL CONNECTED LAKES ARE APPROPRIATELY
C          TAGGED (JCLS=1 AND NCNT=1).
      ICL1 = ICL + 1
      DO 1230 LK=ICL1,NCLS
      IF(JCLS(LK,1).EQ.2) GO TO 1230
      LK3 = LK - 1
      DO 1227 LK1=1,LK3
      ICM2 = ICS(LK1)
      DO 1227 IC2=2,ICM2
      IF(ISUB(LK1,IC2).NE.ISUB(LK,1).OR.JCLS(LK1,IC2).NE.1) GO TO 1227
      JCLS(LK,1) = 1
      IS1 = ISUB(LK,1)
      NCNT(IS1) = 1
      ICM3 = ICS(LK)
      DO 1226 IC3=2,ICM3
      IS2 = ISUB(LK,IC3)
      IF(IS2.LE.0) GO TO 1227
      IF(STGNEW(IS1).LE.SILLVT(LK,IC3-1).AND.STGNEW(IS2).LE.
     1  SILLVT(LK,IC3-1)) GO TO 1226
      JCLS(LK,IC3) = 1
      NCNT(IS2) = 1
      SVT(IS2) = SILLVT(LK,IC3-1)
 1226 CONTINUE
 1227 CONTINUE
 1230 CONTINUE
C
C22F-----COUNT NUMBER OF LAKES IDENTIFIED AS A CONNECTED PART 
C          OF THE PRESENT LAKE SYSTEM, STORE LAKE NUMBERS IN KSUB, 
C          AND CUMULATE TOTAL SURFACE AREA.
 1240 ICNT = 0
      KCNT = KCNT + 1
      TOTARE = ZERO
      DO 1245 L=1,NLAKES
      IF(NCNT(L).NE.1) GO TO 1245
      ICNT = ICNT + 1
      TOTARE = TOTARE + SURFA(L)
      KSUB(ICNT) = L
      MSUB(ICNT,KCNT) = L
 1245 CONTINUE
      MSUB1(KCNT) = ICNT
      IF(ICNT.LE.1) KCNT = KCNT - 1
      IF(ICNT.LE.1) GO TO 1300
      IF(LWRT.GT.0.OR.ICBCFL.LE.0) GO TO 1251
      WRITE(IOUT,1250) KSTP, ICNT, TOTARE
 1250 FORMAT(/1X,80('-')/1X,'TIME STEP',I6,5X,'NUMBER OF CONNECTED LAKE !gsf
     1S IS',I3,5X,'TOTAL AREA = ',D16.9/)
 1251 CONTINUE
C
C22G-----COMPUTE STAGE ADJUSTMENTS (STGADJ) REQUIRED FOR CONNECTED 
C           LAKES TO HAVE THE SAME STAGE.
Cdep     changed adjustment on basis of volumes exchanged among lakes.
      DO 1270 I=1,ICNT
      L1 = KSUB(I)
      SUM = ZERO
      DO 1265 J=1,ICNT
      IF(J.EQ.I) GO TO 1265
      L = KSUB(J)
      SUM = SUM + VOLTERP(STGNEW(L),L)-VOLTERP(STGNEW(L1),L)
C     SUM = SUM + SURFA(L)*(STGNEW(L)-STGNEW(L1))
 1265 CONTINUE
      STGADJ(L1) = SUM/TOTARE
 1270 CONTINUE
C
C22H-----CHECK FOR NEWLY COMPUTED LAKE STAGES (STGNEW) LESS 
C          THAN SILL ELEVATIONS OF THE LAKES (SVT).
 1272 ICNR = 0
      ICM3 = ICS(ICL)
      DO 1275 IC3=2,ICM3
      L = ISUB(ICL,IC3)
      IF(L.LE.0) GO TO 1275
      IF(NCNST(L).EQ.1) GO TO 1275
      IF(NCNT(L).EQ.0) GO TO 1275
      IF(STGNEW(L).LT.SVT(L)) GO TO 1275
      STGTST = STGNEW(L) + STGADJ(L)
      IF(STGTST.GE.SVT(L)) GO TO 1275
C
C22I-----ADJUST STAGE TO SILL ELEVATION.
C
      NCNST(L) = 1
Cdep  revised calculation of FLXINL using volumes 6/7/2009
Cdep  created a double precision local variable named SILLELEV
      SILLELEV  = SVT(L)
      FLXINL(L) = VOLTERP(SILLELEV,L)-VOLTERP(STGNEW(L),L)
      STGADJ(L) = SVT(L) - STGNEW(L)
      STGNEW(L) = SVT(L)
Cdep   commented out calculation of FLXINL using surface areas
C      FLXINL(L) = STGADJ(L)*SURFA(L)
      VOL(L) = VOL(L) + FLXINL(L)
      TOTARE = TOTARE - SURFA(L)
      NCNT(L) = 2
      ICNR = 1
      WRITE(IOUT,2238) L,L,STGADJ(L),STGNEW(L)
 1275 CONTINUE
      IF(ICL.EQ.NCLS) GO TO 1277
C
C22J-----IF A LAKE STAGE IS ADJUSTED TO THE SILL ELEVATION, CHECK TO SEE
C          WHETHER THERE ARE SUBLAKES OF THIS LAKE AND ADJUST THEM TO 
C          THE SILL ELEVATION UNLESS THE ORIGINAL STAGE IS ALREADY LOWER, 
C          IN WHICH CASE THEY ARE NO LONGER CONNECTED.
      ICL1 = ICL + 1
      DO 2230 LK=ICL1,NCLS
      IS1 = ISUB(LK,1)
      IF(NCNT(IS1).EQ.0) GO TO 2230
      ICM1 = ICS(LK)
      DO 2225 IC2=2,ICM1
      IS2 = ISUB(LK,IC2)
      IF(NCNST(IS2).EQ.1) GO TO 2225
      IF(NCNT(IS2).EQ.0) GO TO 2225
      IF(STGNEW(IS2).LT.SVT(IS2)) GO TO 2225
      SVT1 = SVT(IS2)
      IF(SVT(IS1).GT.SVT1.AND.NCNT(IS1).EQ.2) SVT1 = SVT(IS1)
      STGTST = STGNEW(IS2) + STGADJ(IS2)
      IF(STGTST.GE.SVT1) GO TO 2225
      ICNR = 1
      NCNST(IS2) = 1
      NCNT(IS2) = 2
      STGADJ(IS2) = SVT1 - STGNEW(IS2)
Cdep  revised calculation of FLXINL using volumes 6/7/2009
Cdep  created a double precision local variable named SILLELEV
      SILLELEV = SVT1
      FLXINL(IS2)=VOLTERP(SILLELEV,IS2)-VOLTERP(STGNEW(IS2),IS2)
      STGNEW(IS2) = SVT1
Cdep commented calculation of FLXINL using surface area
C      FLXINL(IS2) = STGADJ(IS2)*SURFA(IS2)
      VOL(IS2) = VOL(IS2) + FLXINL(IS2)
      TOTARE = TOTARE - SURFA(IS2)
      IF(LWRT.GT.0.OR.ICBCFL.LE.0) GO TO 2225
      L11 = L
      IF(SVT(IS2).GT.SVT(L)) L11 = IS2
      WRITE(IOUT,2238) IS2,L11,STGADJ(IS2),STGNEW(IS2)
 2238 FORMAT(1X,'READJUST STAGE OF LAKE ',I3,' TO LAKE ',I3,
     1  ' SILL ELEVATION BY ',F5.2,' TO ',F7.2)
 2225 CONTINUE
 2230 CONTINUE
 1277 IF(ICNR.LE.0) GO TO 1280
C
C22K-----RECOMPUTE STAGE ADJUSTMENTS CONSTRAINED NOT TO LOWER LAKES 
C          BELOW SILL ELEVATIONS.
C
      ICNR1 = 0
      DO 1370 I=1,ICNT
      L1 = KSUB(I)
      IF(NCNT(L1).EQ.0) GO TO 1370
      IF(NCNST(L1).EQ.1) GO TO 1370
      SUM = ZERO
      DO 1365 J=1,ICNT
      IF(J.EQ.I) GO TO 1365
      L = KSUB(J)
      IF(NCNT(L).EQ.0) GO TO 1365
Cdep changed computation of volume change 6/09/2009
      IF(NCNST(L).EQ.0) SUM = SUM + 
     +        VOLTERP(STGNEW(L),L)-VOLTERP(STGNEW(L1),L)
C      IF(NCNST(L).EQ.0) SUM = SUM + SURFA(L)*(STGNEW(L)-STGNEW(L1))
      IF(NCNST(L).EQ.1) SUM = SUM - SURFA(L)*STGADJ(L)
 1365 CONTINUE
      STGADJ(L1) = SUM/TOTARE
      STGTST = STGNEW(L1) + STGADJ(L1)
      IF(STGTST.GE.SVT(L1)) GO TO 1370
      ICNR1 = 1
 1370 CONTINUE
      IF(ICNR1.NE.0) GO TO 1272
 1280 IF(LWRT.GT.0.OR.ICBCFL.LE.0) GO TO 1281
      WRITE(IOUT,1286)
 1286 FORMAT(//11X,'SURFACE',7X,'SILL',3X,'WATER BUDGET',2X,'STAGE',2X,
     1 'CORRECTED',3X,'LAKE VOLUME'/2X,'LAKE',7X,'AREA',6X,'ELEVATION',
     2  3X,'STAGE',3X,'CORRECTION',2X,'STAGE',5X,'CORRECTION')
 1281 CONTINUE
      TVOLM = ZERO
      DO 1290 I=1,ICNT
      L = KSUB(I)
      STO = STGNEW(L)
      IF(NCNST(L).EQ.1) STO = STGNEW(L) - STGADJ(L)
      IF(NCNST(L).EQ.1) GO TO 1285
Cdep  revised calculation of FLXINL using volumes 6/7/2009
Cdep  created a double precision local variable named ADJSTAGE
      ADJSTAGE = STGNEW(L)+STGADJ(L)
      FLXINL(L) = VOLTERP(ADJSTAGE,L)-VOLTERP(STGNEW(L),L)
      STGNEW(L) = STGNEW(L) + STGADJ(L)
Cdep commented calculation of FLXINL using surface area
C      FLXINL(L) = STGADJ(L)*SURFA(L)
      VOL(L) = VOL(L) + FLXINL(L)
 1327 FORMAT(/10X,'WARNING -- SUM OF INTERLAKE FLUXES ',F10.0,' EXCEEDS
     110**6 OF THE TOTAL VOLUME'/)
      WRITE(IOUT,1301)
 1301 FORMAT(1X,80('-')/)
 1285 TVOLM = TVOLM + VOL(L)
      IF(LWRT.GT.0.OR.ICBCFL.LE.0) GO TO 1290
      WRITE(IOUT,1269) L,SURFA(L),SVT(L),STO,STGADJ(L),STGNEW(L),
     1  FLXINL(L)
 1269 FORMAT(1X,I3,1X,G15.5,4F10.2,G15.5)
 1290 CONTINUE
C
C22L-----RECOMPUTE TIME STEP AND CUMULATIVE STAGE CHANGES FOR 
C          CONNECTED LAKES.
      DO 1295 I=1,ICNT
      L = KSUB(I)
!dep August 27, 2009 Transient only
        IF (ISS.NE.1) THEN
          OLDSTAGE = STGOLD(L)
          DELH(L) = STGNEW(L) - OLDSTAGE
          TDELH(L) = STGNEW(L) - STAGES(L)
        END IF
 1295 CONTINUE
 1300 CONTINUE
C
C22M-----CHECK ON SUM OF CONNECTED-LAKE INTERCHANGE VOLUMES.
      FLSUM = ZERO
      DO 1325 L=1,NLAKES
      FLSUM = FLSUM + FLXINL(L)
 1325 CONTINUE
      IF(LWRT.GT.0.OR.ICBCFL.LE.0) GO TO 1350
      TV = TVOLM/1000000.
      IF(FLSUM.GE.TV) WRITE(IOUT,1327) FLSUM
 1350 CONTINUE
C
C23------CHECK FOR LAKE CELLS GOING DRY.
      LDR = 0
      DO 950 L=1,LKNODE
         IL=ILAKE(1,L)
         IR=ILAKE(2,L)
         IC=ILAKE(3,L)
         LAKE=ILAKE(4,L)
C  Convert ILAKE(5,L):  1 and 2 are type 1,  3 and 4 are type 2, 6 is type 0
         ITYPE = (ILAKE(5,L)+1)/2
         IF(ITYPE.EQ.3) ITYPE=0
         IF(ITYPE.NE.0) GO TO 950
         IF(IBOUND(IC,IR,IL).GT.0) BOTLK = BOTM(IC,IR,LBOTM(IL-1))
         IF(IBOUND(IC,IR,IL).EQ.0) BOTLK = BOTM(IC,IR,LBOTM(IL))
Cdep  revised to set STGNEW to BOTTOM of LAKE when LESS than BOTLK.
         IF (STGNEW(LAKE).LE.BOTLK) THEN
           LDR = LDR + 1
           LDRY(LDR) = L
Cdep           STGNEW(LAKE)=BOTLK
         END IF
  950 CONTINUE
      IF(LWRT.GT.0.OR.ICBCFL.LE.0) GO TO 951
      IF(LDR.EQ.0) WRITE(IOUT,875) LDR
  875 FORMAT(//1X,I5,2X,'LAKE CELLS ARE DRY.')
      IF(LDR.EQ.0) GO TO 951
      WRITE(IOUT,874) LDR
  874 FORMAT(/5X,'SECTIONS OF THE LAKE BOTTOM HAVE BECOME DRY.  THE DRY
     1SECTIONS'/5X,'LIE ABOVE THE FOLLOWING',I3,' AQUIFER CELLS (LAYER,R
     2OW,COLUMN):')
      LDR1 = 0
      DO 952 L=1,LDR
      LDR1 = LDR1 + 1
      L1 = LDRY(L)
      ILB(LDR1) = ILAKE(1,L1)
      IRB(LDR1) = ILAKE(2,L1)
      ICB(LDR1) = ILAKE(3,L1)
      IF(LDR1.LT.5) GO TO 952
      WRITE(IOUT,876) (ILB(I),IRB(I),ICB(I),I=1,5)
  876 FORMAT(5X,5('(',I3,',',I3,',',I3,')',2X))
      LDR1 = 0
  952 CONTINUE
      IF(LDR1.GT.0) WRITE(IOUT,876) (ILB(I),IRB(I),ICB(I),I=1,LDR1)
  951 CONTINUE
Cdep  Added following do loop.
C24------Set Lake Stage to bottom of lake when lake is dry and set
C       lake volume to zero.
      DO LAKE=1,NLAKES
        IF(STGNEW(LAKE).LE.BOTTMS(LAKE)) THEN
          STGNEW(LAKE) = BOTTMS(LAKE)
          VOL(LAKE) = 0.0
        END IF
      END DO
!dep Moved call to GAG7LO to just above comment line 13 6/9/2009
!dep      IF(IUNITGWT.GT.0) GO TO 1086
!dep      NDUM=1
!dep  Buff was passed to GAGE package as a dummy array because 
!dep     transport is inactive and the variables were not defined.
!dep      IF(IUNITGWT.LE.0 .AND. IUNITGAGE.GT.0) 
!dep     *  CALL SGWF2GAG7LO(IUNITGWT,IUNITUZF,XLAKES,TOTIM,
!dep     *  GWIN,GWOUT,SEEP,FLXINL,VOLOLD,XLKOLD,XLAKINIT,NSOL)
C
C25------WRITE WARNINGS IF LAKE STAGES EXCEED SPECIFIED MINIMUMS
C          AND MAXIMUMS FOR STEADY STATE SIMULATIONS.
 1086 DO LAKE=1,NLAKES
        IF(ISS.GT.0) THEN
          IF (STGNEW(LAKE).LT.SSMN(LAKE)) THEN
            WRITE(IOUT,972) STGNEW(LAKE), SSMN(LAKE), LAKE 
  972       FORMAT(/1X,'WARNING-- COMPUTED STAGE OF ',F10.2,
     1               ' IS LESS THAN SPECIFIED MINIMUM ',F10.2,
     2               ' FOR LAKE ',I5)
          ELSE IF (STGNEW(LAKE).GT.SSMX(LAKE)) THEN
            WRITE(IOUT,973) STGNEW(LAKE), SSMX(LAKE), LAKE 
  973       FORMAT(/1X,'WARNING-- COMPUTED STAGE OF ',F10.2,
     1               ' IS GREATER THAN SPECIFIED MAXIMUM ',F10.2,
     2               ' FOR LAKE ',I5)
          END IF
        END IF
      END DO
      IF(LWRT.GT.0.OR.ICBCFL.LE.0) GO TO 1061
C
C26------WRITE BUDGET SUMMARIES.
            WRITE(IOUT,1025) KPER, KSTP, DELT, PERTIM, TOTIM
 1025 FORMAT(/1X,'PERIOD ',I5,5X,'TIME STEP',I6,5X,'TIME STEP LENGTH ', !gsf
     1   1PE11.4/1X,'PERIOD TIME ',E11.4,5X,'TOTAL SIMULATION TIME ',
     2   E11.4)
            WRITE(IOUT,1040)
 1040 FORMAT(//17X,'HYDROLOGIC BUDGET SUMMARIES FOR SIMULATED LAKES'
     1    ,/,5X,'(ALL FLUID FLUXES ARE VOLUMES ADDED TO THE LAKE DURING'
     2    ,' PRESENT TIME STEP)'
     3    ,/,5X,'------------------------------------------------------'
     4    ,'----------------------------------')
Cdep REVISED PRINT STATEMENT WHEN THERE IS RUNOFF FROM UZF PACKAGE
      IF (IUNITUZF.EQ.0) THEN
            WRITE(IOUT,1045)
 1045 FORMAT(1X,'LAKE',4X,'STAGE',9X,'VOLUME',5X,'VOL. CHANGE',6X,
     1      'PRECIP',5X,'EVAPORATION',8X,'RUNOFF')
      ELSE 
            WRITE(IOUT,3045)
 3045 FORMAT(' LAKE',4x,'STAGE',9X,'VOLUME',5X,'VOL. CHANGE',6X,
     1       'PRECIP',5X,'EVAPORATION',3X,' SPECIFIED RUNOFF',5X, 
     2       'COMPUTED RUNOFF',4X, 'TOTAL RUNOFF')   
      END IF
C
C27-----WRITE LAKE BUDGETS FOR A TIMES STEP (VOLUMES PER TIME STEP).
 1061       DO 1100 NN=1,NLAKES
              PPTIN=PRECIP(NN)*DELT
              EOUT=EVAP(NN)*DELT
              SEEPUZF = SEEPUZ(NN)*DELT
              IF(RNF(NN).GE.ZERO) RUNF(NN) = RNF(NN)
              IF(RNF(NN).LT.ZERO) RUNF(NN) =-RNF(NN)*PRCPLK(NN)
     1                                         *BGAREA(NN)
              RUNFD = RUNF(NN)*DELT
              IF (IUNITUZF.GT.0) THEN
                RUNOFF(NN) = OVRLNDRNF(NN)*DELT
              ELSE
                RUNOFF(NN) = 0.0
              END IF
C
              CUMPPT(NN)=CUMPPT(NN)+PPTIN
              CUMEVP(NN)=CUMEVP(NN)+EOUT
              CUMRNF(NN)=CUMRNF(NN)+RUNFD
              CUMUZF(NN)=CUMUZF(NN)+SEEPUZF
              IF (IUNITUZF.GT.0) THEN
                CUMLNDRNF(NN) = CUMLNDRNF(NN) + RUNOFF(NN)
              END IF
C-lfk
              IF(ISS.NE.0) THEN
                DELVOL(NN)=0.0
                VOLINIT(NN)=VOL(NN)
              ELSE
                DELVOL(NN)=VOL(NN)-VOLOLD(NN)
              END IF
C
              DELVOLLAK(NN)=DELVOL(NN)/DELT
C-EDM
              IF(IUNIT(49).NE.0 ) THEN
                IF ( LKFLOWTYPE(1).EQ.'NA' ) THEN
                  LKFLOWTYPE(1)='VOLUME'
                  NLKFLWTYP = NLKFLWTYP + 1
                END IF
              ENDIF
              IF(IUNIT(49).NE.0 ) THEN
                IF ( LKFLOWTYPE(2).EQ.'NA' ) THEN
                  LKFLOWTYPE(2)='DELVOL'
                  NLKFLWTYP = NLKFLWTYP + 1
                END IF
              ENDIF
C
              IF(LWRT.GT.0.OR.ICBCFL.LE.0) GO TO 1100
              IF(IUNITUZF.EQ.0) THEN
                IF(ISS.NE.0) THEN
                  WRITE(IOUT,1049) NN,STGNEW(NN),VOL(NN),
     +                             PPTIN,EOUT,RUNFD
                ELSE
                  WRITE(IOUT,1050) NN,STGNEW(NN),VOL(NN),DELVOL(NN),
     +                             PPTIN,EOUT,RUNFD
                END IF
              ELSE
                IF(ISS.NE.0) THEN
                  WRITE(IOUT,3049) NN,STGNEW(NN),VOL(NN),
     +                             PPTIN,EOUT,RUNFD,RUNOFF(NN),RUNFD+
     +                             RUNOFF(NN)
                ELSE
                  WRITE(IOUT,3050) NN,STGNEW(NN),VOL(NN),DELVOL(NN),
     +                             PPTIN,EOUT,RUNFD,RUNOFF(NN),RUNFD+
     +                             RUNOFF(NN)
                END IF
              END IF
 1100       CONTINUE
 1049 FORMAT(1X,I3,2(1X,1PE13.6),'     N/A (SS) ',3(1X,1PE13.6))
 1050 FORMAT(1X,I3,6(1X,1PE13.6))
 3049 FORMAT(1X,I3,2(1X,1PE13.6),'     N/A (SS) ',2(1X,1PE13.6),
     +  2X,1PE13.6,9X,1PE13.6,5X,1PE13.6)
 3050 FORMAT(I4,5(1X,1PE13.6),2X,1PE13.6,9X,1PE13.6,5X,1PE13.6)
       IF(LWRT.LE.0.AND.ICBCFL.GT.0) THEN
         IF(IUNITUZF.EQ.0) THEN
           WRITE(IOUT,1046)
         ELSE 
           WRITE(IOUT,1047)
         END IF
       END IF
 1046 FORMAT(/12X,'GROUND WATER',12X,'SURFACE WATER',8X,'WATER'/1X,
     1 'LAKE',4X,'INFLOW',5X,'OUTFLOW',6X,'INFLOW',5X,'OUTFLOW'7X,'USE')
 1047 FORMAT(/12X,'GROUND WATER',12X,'SURFACE WATER',8X,'WATER',10X,
     1 'UZF INFIL.'/1X,'LAKE',4X,'INFLOW',5X,'OUTFLOW',6X,'INFLOW',5X,
     2 'OUTFLOW',7X,'USE',8X,'FROM LAKE')
C
C28-----DETERMINE LAKE BUDGET ERROR FOR A TIME STEP.
            DO 1101 NN=1,NLAKES
              PPTIN=PRECIP(NN)*DELT
              EOUT=EVAP(NN)*DELT
              SEEPUZF = SEEPUZ(NN)*DELT
              IF(RNF(NN).GE.ZERO) RUNF(NN) = RNF(NN)
              IF(RNF(NN).LT.ZERO) RUNF(NN) =-RNF(NN)*PRCPLK(NN)
     1                                         *BGAREA(NN)
              RUNFD = RUNF(NN)*DELT
              IF (IUNITUZF.GT.0) THEN
                RUNOFF(NN) = OVRLNDRNF(NN)*DELT
              ELSE
                RUNOFF(NN) = 0.0
              END IF
              QIN=GWIN(NN)*DELT
              QOUT=GWOUT(NN)*DELT
              QSIN=SURFIN(NN)*DELT
              QSOUT=SURFOT(NN)*DELT
C
              CUMGWI(NN)=CUMGWI(NN)+QIN
              CUMGWO(NN)=CUMGWO(NN)+QOUT
              CUMSWI(NN)=CUMSWI(NN)+QSIN
              CUMSWO(NN)=CUMSWO(NN)+QSOUT
C-LFK
              WDRAW=WITHDRW(NN)*DELT
C-LFK      Calculate accuracy of lake budget FOR TIME STEP
              CUMLKIN(NN)=PPTIN+RUNFD+RUNOFF(NN)+QIN+QSIN
              CUMLKOUT(NN)=EOUT+WDRAW+QOUT+QSOUT+SEEPUZF
              IF (CUMLKIN(NN).GT.CUMLKOUT(NN)) THEN
                   DENOM=CUMLKIN(NN)
                 ELSE IF (CUMLKOUT(NN).NE.0.0) THEN
                   DENOM=CUMLKOUT(NN)
                 ELSE 
                   DENOM=ABS(DELVOL(NN))
                 END IF
              IF (abs(DENOM).GT.Closezero) THEN
                 TSLAKERR(NN)=(CUMLKIN(NN)-CUMLKOUT(NN)-DELVOL(NN)
     1                       +FLXINL(NN))*100./DENOM
              ELSE 
C            Note: if both ins & outs = 0.0, err set = 1e20
!                 TSLAKERR(NN)=1.0E20
                TSLAKERR(NN)=0.0
               END IF
              IF(LWRT.LE.0.AND.ICBCFL.GT.0)THEN
                IF(IUNITUZF.EQ.0) THEN
                  WRITE(IOUT,1051) NN,QIN,QOUT,QSIN,QSOUT,WDRAW
                ELSE
                  WRITE(IOUT,1052) NN,QIN,QOUT,QSIN,QSOUT,WDRAW,SEEPUZF
                END IF
              END IF
 1101       END DO
 1051 FORMAT(1X,I3,1P,5E12.4)
 1052 FORMAT(1X,I3,1P,5E12.4,5X,E12.4)
      IF(LWRT.LE.0.AND.ICBCFL.GT.0) WRITE(IOUT,1035)
C-LFK
 1035 FORMAT(/6X,'CONNECTED LAKE',3X,'TIME-STEP'
     1 ,9X,'STAGE-CHANGE',10X,'PERCENT'/1X,'LAKE',4X,'INFLUX',
     2  6X,'SURFACE AREA',3X,'TIME STEP',2X,'CUMULATIVE',4X,
     3  'DISCREPANCY')
C
C29-----DETERMINE CUMULATIVE LAKE BUDGET ERRORS. 
            DO 1105 NN=1,NLAKES
Cdep  All values are volumes per time (times DELT)
              WDRAW=WITHDRW(NN)*DELT
C
              CUMWDR(NN)=CUMWDR(NN)+WDRAW
              CUMFLX(NN)=CUMFLX(NN)+FLXINL(NN)
C-LFK
              IF(ISS.NE.0) THEN
                 CUMVOL(NN)=0.0
                 ELSE
                   CUMVOL(NN)=VOL(NN)-VOLINIT(NN)
                 END IF
C-LFK      Calculate accuracy of CUMULATIVE lake budget
              CUMLKIN(NN)=CUMPPT(NN)+CUMRNF(NN)+CUMGWI(NN)+CUMSWI(NN)
     +                    +CUMLNDRNF(NN)
              CUMLKOUT(NN)=CUMEVP(NN)+CUMWDR(NN)+CUMGWO(NN)+CUMSWO(NN)
     +                     +CUMUZF(NN)
              IF (CUMLKIN(NN).GT.CUMLKOUT(NN)) THEN
                   DENOM=CUMLKIN(NN)
                 ELSE IF (CUMLKOUT(NN).NE.0.0) THEN
                   DENOM=CUMLKOUT(NN)
                 ELSE 
                   DENOM=ABS(CUMVOL(NN))
                 END IF
              IF (DENOM.NE.0.0) THEN
                 CMLAKERR(NN)=(CUMLKIN(NN)-CUMLKOUT(NN)-CUMVOL(NN)
     1                       +CUMFLX(NN))*100./DENOM
              ELSE 
C     Note: if both ins & outs = 0.0, err set = 1e20
!                 CMLAKERR(NN)=1.0E20
                CMLAKERR(NN)=0.0
              END IF
              IF(LWRT.LE.0.AND.ICBCFL.GT.0) THEN
                IF(ISS.NE.0) THEN
                  WRITE(IOUT,1054) NN,FLXINL(NN),SURFA(NN),
     2                              TSLAKERR(NN)
                ELSE
                  WRITE(IOUT,1055) NN,FLXINL(NN),SURFA(NN),
     2                        DELH(NN),TDELH(NN),TSLAKERR(NN)
                END IF
 1054 FORMAT(1X,I3,1P,1E13.4,2X,E13.4,'     N/A (SS) ','  N/A (SS) ',
     *         3X,0P,F9.3)
 1055 FORMAT(1X,I3,1P,1E13.4,2X,2E13.4,E12.4,3X,0P,F9.3)
              END IF
 1105       END DO
C 1055   FORMAT(1X,I3,1PE13.4,2X,2PE13.4,1PE12.4,3X,0P,F9.3)
        IF(LWRT.GT.0.OR.ICBCFL.LE.0) GO TO 1041
        WRITE(IOUT,1301)
C
C30------WRITE CUMULATIVE BUDGET SUMMARIES.
        WRITE(IOUT,2040)
 2040 FORMAT(//12X,'CUMULATIVE HYDROLOGIC BUDGET SUMMARIES FOR SIMULATED
     1 LAKES'
     2    ,/,5X,'(ALL FLUID FLUXES ARE SUMS OF VOLUMES ADDED TO THE'
     3    ,' LAKE SINCE INITIAL TIME)'
     4    ,/,5X,'------------------------------------------------------'
     5    ,'---------------------')
Cdep  added computed runoff from UZF Package to lake budget
         IF(IUNITUZF.LE.0) THEN
           WRITE(IOUT,2045)
 2045      FORMAT(1X,'LAKE',7X,'PRECIP',7X,'EVAP',7X,'RUNOFF')
         ELSE
           WRITE(IOUT,4045)
 4045      FORMAT(1X,'LAKE',7X,'PRECIP',7X,'EVAP',5X,'SPECIFIED RUNOFF',
     +            3X,'COMPUTED RUNOFF',3X,'TOTAL RUNOFF')             
         END IF           
      DO 2100 NN=1,NLAKES
        IF(IUNITUZF.LE.0) THEN
          WRITE(IOUT,2050) NN,CUMPPT(NN),CUMEVP(NN),CUMRNF(NN)
        ELSE
          WRITE(IOUT,4050) NN,CUMPPT(NN),CUMEVP(NN),CUMRNF(NN),
     +          CUMLNDRNF(NN),CUMRNF(NN)+CUMLNDRNF(NN)
        END IF
 2100 CONTINUE
 2050 FORMAT(1X,I3,3X,1P,3E12.4)
 4050 FORMAT(1X,I3,3X,1P,2E12.4,3(5X,E12.4))
        IF(IUNITUZF.LE.0) THEN
          WRITE(IOUT,2046)
        ELSE
          WRITE(IOUT,2047)
        END IF
 2046 FORMAT(/12X,'GROUND WATER',12X,'SURFACE WATER'/1X,'LAKE',4X,
     1 'INFLOW',5X,'OUTFLOW',6X,'INFLOW',5X,'OUTFLOW')
 2047 FORMAT(/12X,'GROUND WATER',12X,'SURFACE WATER',7X,'UZF INFIL.'
     1 /1X,'LAKE',4X,'INFLOW',5X,'OUTFLOW',6X,
     2'INFLOW',5X,'OUTFLOW',5X,'FROM LAKE')
      DO 2101 NN=1,NLAKES
        IF(IUNITUZF.LE.0) THEN
          WRITE(IOUT,2051) NN,CUMGWI(NN),CUMGWO(NN),CUMSWI(NN),
     1                   CUMSWO(NN)
        ELSE
          WRITE(IOUT,2052) NN,CUMGWI(NN),CUMGWO(NN),CUMSWI(NN),
     1                   CUMSWO(NN),CUMUZF(NN)
        END IF
 2101 END DO
 2051 FORMAT(1X,I3,1P,4E12.4)  
 2052 FORMAT(1X,I3,1P,4E12.4,2X,E12.4)
      WRITE(IOUT,2035)
C-LFK
 2035 FORMAT(/9X,'WATER',4X,'CONNECTED LAKE',3X,'CHANGE',7X,'PERCENT'/
     1       1X,'LAKE',5X,'USE',9X,'INFLUX',7X,'IN VOL.',4X,
     2       'DISCREPANCY')
      DO 2105 NN=1,NLAKES
C-LFK
      IF(ISS.NE.0) THEN
       WRITE(IOUT,2054) NN,CUMWDR(NN),CUMFLX(NN),CMLAKERR(NN)
      ELSE
       WRITE(IOUT,2055) NN,CUMWDR(NN),CUMFLX(NN),CUMVOL(NN),CMLAKERR(NN)
      END IF
 2105 CONTINUE
C-LFK
 2054 FORMAT(1X,I3,1P,2E13.4,'    N/A (SS)   ',0P,F9.3)
 2055 FORMAT(1X,I3,1P,3E13.4,2X,0P,F9.3)
      WRITE(IOUT,1301)
      IF(KCNT.LE.0) GO TO 11041
      IF (KCNT.GT.1) THEN
        WRITE(IOUT,11055) KCNT
11055 FORMAT(/1X,I3,' CONNECTED LAKE SETS'/)
        DO 11056 IIC=1,KCNT
          JIC = MSUB1(IIC)
          WRITE(IOUT,11057) JIC, (MSUB(LIC,IIC),LIC=1,JIC)
11057 FORMAT(1X,I3,' LAKES:  ',25I3)
11056   CONTINUE
      ELSE
        WRITE(IOUT,21055) KCNT
21055 FORMAT(/1X,I3,' CONNECTED LAKE SET'/)
C-LFK
      IIC=1
      JIC = MSUB1(IIC)        
      WRITE(IOUT,11057) JIC, (MSUB(LIC,KCNT),LIC=1,JIC)
      END IF
11041 CONTINUE
 1041 CONTINUE
C
C31-----dep   Moved Call to gage to follow budget summaries 6/9/2009
C-LFK   don't call GAG5LO from here in Lake Package if GWT is active
!dep replaced stgold2 and stages with delh and tdelh
      IF(IUNITGWT.LE.0) THEN
        IF (IUNITGAGE.GT.0)THEN 
          CALL SGWF2GAG7LO(IUNITGWT,IUNITUZF,XLAKES,TOTIM,
     +         GWIN,GWOUT,SEEP,FLXINL,VOLOLD,XLKOLD,XLAKINIT,NSOL)
        END IF
      END IF
C
C32-----IF C-B-C TERMS WILL BE SAVED THEN WRITE TO DISK.
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,ILKCB,BUFF,NCOL,NROW,
     1                         NLAY,IOUT)
C
C32A-----MOVE RATES INTO VBVL FOR PRINTING BY MODULE BAS OT.
1200  VBVL(3,MSUM)=RATIN
      VBVL(4,MSUM)=RATOUT
C
C32B-----MOVE PRODUCT OF RATE AND TIME STEP INTO VBVL ACCUMULATORS.
      VBVL(1,MSUM)=VBVL(1,MSUM)+RATIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+RATOUT*DELT
C
C32C-----MOVE BUDGET TERM LABELS INTO VBVM FOR PRINTING BY BAS OT.
      VBNM(MSUM)=TEXT
C33------INCREASE BUDGET COUNTER.
      MSUM=MSUM+1
C
C        Substitute Lake Stage values for HNOFLO values at non-dry lake
C        cells in HNEW array; loop over all lake nodes.  If Lake Stage
C        is below bottom of lake cell, set HNEW = HNOFLO.
C
      DO 1900 L=1,LKNODE
         IL=ILAKE(1,L)
         ILL=IL-1
         IR=ILAKE(2,L)
         IC=ILAKE(3,L)
         LAKE=ILAKE(4,L)
         ITYPE = (ILAKE(5,L)+1)/2
         IF(ITYPE.EQ.3) ITYPE=0
         IF(ITYPE.NE.0) GO TO 1900
         IF(IBOUND(IC,IR,IL).GT.0) THEN
            BOTLK = BOTM(IC,IR,LBOTM(ILL))
            IF (STGNEW(LAKE).GT.BOTLK) HNEW(IC,IR,ILL)=STGNEW(LAKE)
            IF (ILL.GT.1) THEN
               ILL=ILL-1
               DO 1890 IL2=1,ILL
                 IF (STGNEW(LAKE).GT.BOTM(IC,IR,LBOTM(IL2))) THEN
                    HNEW(IC,IR,IL2)=STGNEW(LAKE)
                 ELSE
                   HNEW(IC,IR,IL2)=HNOFLO
                 END IF
1890           CONTINUE
            END IF
         ELSE IF(IBOUND(IC,IR,IL).EQ.0) THEN
            BOTLK = BOTM(IC,IR,LBOTM(IL))
            IF (STGNEW(LAKE).GT.BOTLK) HNEW(IC,IR,IL)=STGNEW(LAKE)
            IF (IL.GT.1) THEN
               ILL=IL-1
               DO 1892 IL2=1,ILL
                 IF (STGNEW(LAKE).GT.BOTM(IC,IR,LBOTM(IL2))) THEN
                    HNEW(IC,IR,IL2)=STGNEW(LAKE)
                 ELSE
                   HNEW(IC,IR,IL2)=HNOFLO
                 END IF
1892           CONTINUE
            END IF
         END IF
C
1900  CONTINUE
C
C34-----RETURN.
      RETURN
      END
C
      SUBROUTINE SGWF2LAK7SFR7RPS()
C
C    *******************************************************************
C--  IF STREAMS EXIST, DEFINE CONNECTIONS BETWEEN LAKES AND STREAMS
C    *******************************************************************
C
C    -------------------------------------------------------------------
C        SPECIFICATIONS:
C    -------------------------------------------------------------------
      USE GWFLAKMODULE, ONLY: NLAKES, NTRB, NDV, ITRB, IDIV, IRK
      USE GLOBAL,       ONLY: IOUT, NODES
      USE GWFSFRMODULE, ONLY: NSS, IDIVAR, IOTSG, SEG,  ISEG
C
C-- DOUBLE CHECK SIZE OF IRK (STORED IN BUFF) vs. NLAKES
C
      IF ((NLAKES*2).GT.NODES) THEN
         WRITE (IOUT,*) '***NLAKES too large for BUFF in Subroutine GWF2
     1LAK7SFR7RPS***  STOP EXECUTION'
         CALL USTOP(' ')
      END IF
C
C-- INITIALIZE ARRAYS
C
c     DO 50 I=1,NSS
c     DO 50 LK=1,NLAKES
c     ITRB(LK,I) = 0
c  50 IDIV(LK,I) = 0
      DO 55 LK=1,NLAKES
      IRK(1,LK) = 0
   55 IRK(2,LK) = 0
      NTRB = 0
      NDV = 0
C
C-- Build arrays to define lake tributary & diversion links ...
C        based on stream package input data
C
C---  Stream Inflow to Lakes
      DO 100 LSEG=1,NSS
      IF(IOTSG(LSEG).LT.0) THEN
        LAKE = -IOTSG(LSEG)
        IRK(1,LAKE) = IRK(1,LAKE) + 1
        K1 = IRK(1,LAKE)
        ITRB(LAKE,K1) = LSEG
        IF(IRK(1,LAKE).GT.NTRB) NTRB = IRK(1,LAKE)
      ENDIF
C
C---  Stream Outflow from Lakes
      IF(IDIVAR(1,LSEG).LT.0) THEN
        LAKE = -IDIVAR(1,LSEG)
        IRK(2,LAKE) = IRK(2,LAKE) + 1
        K1 = IRK(2,LAKE)
        IDIV(LAKE,K1) = LSEG
        IF(IRK(2,LAKE).GT.NDV) NDV = IRK(2,LAKE)
      ENDIF
  100 CONTINUE
C
C--  PRINT LAKE INFLOW STREAM SEGMENTS.
      WRITE(IOUT,10)
10    FORMAT(6X,'LAKE ',4X,'INFLOWING STREAM SEGMENT')
      DO 520 IK=1,NLAKES
      DO 519 JK=1,NSS
      IF(ITRB(IK,JK).LE.0) GO TO 521
  519 CONTINUE
  521 JK1 = JK - 1
      IF(JK1.GT.0) WRITE(IOUT,15) IK,(ITRB(IK,JK),JK=1,JK1)
15    FORMAT(5X,I5,14X,100I5)
  520 CONTINUE
      WRITE(IOUT,103) NTRB
103    FORMAT(/1X,'MAXIMUM NUMBER OF STREAMS INFLOWING TO A',
     1    ' LAKE IS',I5/)
C
C--  PRINT LAKE STREAM OUTFLOW SEGMENT (FROM A LAKE) NUMBERS.
C
      WRITE(IOUT,13)
13    FORMAT(6X,'LAKE ',4X,'OUTFLOWING STREAM',' SEGMENT')
      DO 600 IK=1,NLAKES
      DO 523 JK=1,NSS
      IF(IDIV(IK,JK).LE.0) GO TO 527
  523 CONTINUE
  527 JK1 = JK - 1
      IF(JK1.GT.0) WRITE(IOUT,15) IK,(IDIV(IK,JK),JK=1,JK1)
  600 CONTINUE
C
Cdep-- PRINT WARNING IF OUTFLOWING STREAM IS ASSIGNED ICALC =0.
Cdep    ADDED OCTOBER 15, 2004; DAVID PRUDIC
      DO ls = 1, NSS
        IF (IDIVAR(1,ls).LT.0) THEN
          lk = -IDIVAR(1,ls)
          IF (ISEG(1,ls).LE.0 .AND. SEG(2,ls).LE.0.0) THEN
            WRITE (IOUT, 9007) ls, lk, ISEG(1,ls), SEG(2,ls)
          END IF
        END IF
      END DO      
      WRITE(IOUT,133) NDV
133   FORMAT(/1X,'MAXIMUM NUMBER OF STREAMS OUTFLOWING',
     1    ' FROM A LAKE IS',I5/)
 9007 FORMAT(/, ' WARNING****  OUTFLOWING STREAM SEGMENT', I6,
     +       ' FROM LAKE', I6, ' HAS AN ICALC VALUE OF', I6,
     +       ' AND FLOW INTO THE SEGMENT IS', E12.4, /,
     +       ' NO OUTFLOW FROM THE LAKE INTO ',
     +       'SEGMENT WILL BE SIMULATED', /,
     +       ' SUGGEST CHANGING ICALC TO ANOTHER OPTION')
C
C-- RETURN
      RETURN
      END
      SUBROUTINE SGWF2LAK7BCF7RPS()
C
C     ******************************************************************
C     COMPUTE VERTICAL CONDUCTANCES AND HORIZONTAL CONDUCTANCES PER UNIT
C     THICKNESS FOR LAKES WHEN BCF PACKAGE IS USED
C     ******************************************************************
C
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFLAKMODULE, ONLY: LKNODE, BEDLAK, LKARR1, ILAKE, CNDFCT
      USE GLOBAL,       ONLY: NLAY, IOUT, DELR, DELC, LAYHDT
!!      USE GLOBAL,       ONLY: NLAY, IOUT, DELR, DELC, LAYHDT,NCOL,NROW
      USE GWFBCFMODULE, ONLY: IWDFLG, HY, CVWD, TRPY
C
      WRITE(IOUT,108)
  108 FORMAT(//9X,'C',15X,'INTERFACE CONDUCTANCES BETWEEN LAKE AND ',
     1  'AQUIFER CELLS'/
     2  3X,'L',5X,'O',10X,'(IF TYPE = 6, CONDUCTANCE (L^2/T) IS ',
     3  'BETWEEN AQUIFER CELL AND OVERLYING LAKE CELL.)',/
     4  3X,'A',5X,'L',2X,'L',2X,'T',
     5  4X,'(IF TYPE = 1 TO 4, CONDUCTANCES ARE PER UNIT SATURATED ',
     6  'THICKNESS (L/T).)'/
     7  3X,'Y',2X,'R',2X,'U',2X,'A',2X,'Y'/
     8  3X,'E',2X,'O',2X,'M',2X,'K',2X,'P',
     9  24X,'LAKEBED',6X,'C O N D U C T A N C E S'/3X,'R',2X,'W',2X,
     1  'N',2X,'E',
     2  2X,'E',5X,'DELTA Y',3X,'DELTA X',2X,'LEAKANCE',3X,'LAKEBED',3X,
     3  'AQUIFER',2X,'COMBINED'/1X,79('_'))
C
      IWRN = 0
      IWRN1 = 0
      DO 350 II=1,LKNODE
      K = ILAKE(1,II)
      J = ILAKE(2,II)
      I = ILAKE(3,II)
      CNDFCT(II) = 0.0
C  Convert ILAKE(5,II):  1 and 2 are type 1,  3 and 4 are type 2, 6 is type 0
      NTYP = (ILAKE(5,II)+1)/2
      IF(NTYP.EQ.3) NTYP=0
      NTYP = NTYP + 1
      IF(NTYP.EQ.1) THEN
C
C  Vertical Conductance
C    for vertical interface, "K" is layer below bottom of lake
C
        CNDFC1=0.0
        IF(K.EQ.NLAY.AND.LKARR1(I,J,K).GT.0) GO TO 315
        IF(BEDLAK(II).LE.0.0) GO TO 315
          IWRN1 = 1
        CNDFC1 = BEDLAK(II)*DELR(I)*DELC(J)
        IF (IWDFLG.EQ.0) THEN
          CNDFCT(II) = CNDFC1
        ELSE
          IF(CVWD(I,J,K-1).LE.0.0.OR.CNDFC1.LE.0.0) GO TO 315
          CNDFCT(II) = 1.0/(0.5/CVWD(I,J,K-1)+1.0/CNDFC1)
        END IF
  315   IF (IWDFLG.EQ.0) THEN
          WRITE(IOUT,7324) (ILAKE(I1,II),I1=1,5),DELC(J),DELR(I),
     1        BEDLAK(II),CNDFC1,CNDFCT(II)
c-lfk
 7324   FORMAT(1X,5I3,2X,1P,4E10.2,10X,E11.3)
C 7324     FORMAT(1X,5I3,2X,1P,4E10.2,10X,E10.2)
        ELSE
          CVWD2= 2.0*CVWD(I,J,K-1)
          WRITE(IOUT,7325) (ILAKE(I1,II),I1=1,5),DELC(J),DELR(I),
     1        BEDLAK(II),CNDFC1,CVWD2,CNDFCT(II)
c-lfk
 7325   FORMAT(1X,5I3,2X,1P,5E10.2,E11.3)
c 7325     FORMAT(1X,5I3,2X,1P,6E10.2)
        END IF
      ELSE
C
C  Horizontal conductance
C
C  HY not read in, thus unavailable.
C
Cdep  348   IF(LAYHDT(K).EQ.0) THEN
        IF(LAYHDT(K).EQ.0) THEN
          IF(NTYP.EQ.2) CNDFCT(II) = BEDLAK(II)*DELC(J)
          IF(NTYP.EQ.3) CNDFCT(II) = BEDLAK(II)*DELR(I)
          WRITE(IOUT,7324) (ILAKE(I1,II),I1=1,5),DELC(J),DELR(I),
     1        BEDLAK(II),CNDFCT(II),CNDFCT(II)
          IWRN = 1
        ELSE
C
C  HY read in, thus available.
C
        TT = HY(I,J,K)
        IF(NTYP.EQ.2) CNDFC2 = 2.0*TT*DELC(J)/DELR(I)
        IF(NTYP.EQ.3) CNDFC2 = 2.0*TRPY(K)*TT*DELR(I)/DELC(J)
        IF(NTYP.EQ.2) CNDFC1 = BEDLAK(II)*DELC(J)
        IF(NTYP.EQ.3) CNDFC1 = BEDLAK(II)*DELR(I)
        IF (CNDFC1.GT.0.0.AND.CNDFC2.GT.0.0) 
     *         CNDFCT(II) = 1.0/(1.0/CNDFC2+1.0/CNDFC1)
        WRITE(IOUT,7325) (ILAKE(I1,II),I1=1,5),DELC(J),DELR(I),
     1    BEDLAK(II),CNDFC1,CNDFC2,CNDFCT(II)
        END IF
      END IF
  350 CONTINUE
C
C  WRITE WARNINGS ON LAKE/AQUIFER CONDUCTANCES, IF NECESSARY
          IF(IWRN.EQ.1.OR.IWRN1.EQ.1) WRITE(IOUT,345)
  345     FORMAT(//5X,'NOTE: INFORMATION ABOUT CALCULATED LAKE/AQUIFER C
     1ONDUCTANCES WHEN USING BCF PACKAGE FOLLOWS: '/)
          IF(IWRN.EQ.1) WRITE(IOUT,346)
  346     FORMAT(1X,'NODE(S) ADJACENT TO LAKE IN CONFINED LAYER:'/
     1    1X,'LAKE/AQUIFER CONDUCTANCES BASED SOLELY ON LAKEBED SPECIFIC
     2ATION'/)
          IF(IWRN1.EQ.1) WRITE(IOUT,347)
  347     FORMAT(1X,'IF WETDRY FLAG NOT TURNED ON, VERTICAL LEAKANCES AR
     1E NOT SAVED:'/1X,'THEREFORE, LAKE/AQUIFER CONDUCTANCES ARE BASED S
     2OLELY ON LAKEBED SPECIFICATION'/)
          IF(IWRN.EQ.1.OR.IWRN1.EQ.1) WRITE(IOUT,'(//)')
C
      RETURN
      END
C
      SUBROUTINE SGWF2LAK7LPF7RPS()
C
C     ******************************************************************
C     COMPUTE VERTICAL CONDUCTANCES AND HORIZONTAL CONDUCTANCES PER UNIT
C     THICKNESS FOR LAKES WHEN LPF PACKAGE IS USED
C     ******************************************************************
C
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFLAKMODULE, ONLY: LKNODE, BEDLAK, LKARR1, ILAKE, CNDFCT
      USE GLOBAL,       ONLY: NLAY, IOUT, LBOTM, LAYCBD, DELR, DELC,
     +                        BOTM
      USE GWFLPFMODULE, ONLY: CHANI, LAYVKA, VKA, VKCB, HANI, HK
C
      WRITE(IOUT,108)
  108 FORMAT(//9X,'C',15X,'INTERFACE CONDUCTANCES BETWEEN LAKE AND ',
     1  'AQUIFER CELLS'/
     2  3X,'L',5X,'O',10X,'(IF TYPE = 6, CONDUCTANCE (L^2/T) IS ',
     3  'BETWEEN AQUIFER CELL AND OVERLYING LAKE CELL.)',/
     4  3X,'A',5X,'L',2X,'L',2X,'T',
     5  4X,'(IF TYPE = 1 TO 4, CONDUCTANCES ARE PER UNIT SATURATED ',
     6  'THICKNESS (L/T).)'/
     7  3X,'Y',2X,'R',2X,'U',2X,'A',2X,'Y'/
     8  3X,'E',2X,'O',2X,'M',2X,'K',2X,'P',
     9  24X,'LAKEBED',6X,'C O N D U C T A N C E S'/3X,'R',2X,'W',2X,
     1  'N',2X,'E',
     2  2X,'E',5X,'DELTA Y',3X,'DELTA X',2X,'LEAKANCE',3X,'LAKEBED',3X,
     3  'AQUIFER',2X,'COMBINED'/1X,79('_'))
C
      DO 350 II=1,LKNODE
      K = ILAKE(1,II)
      J = ILAKE(2,II)
      I = ILAKE(3,II)
      CAQ = 0.0
      CNDFCT(II) = 0.0
C  Convert ILAKE(5,II):  1 and 2 are type 1,  3 and 4 are type 2, 6 is type 0
      NTYP = (ILAKE(5,II)+1)/2
      IF(NTYP.EQ.3) NTYP=0
      NTYP=NTYP + 1
      IF(NTYP.EQ.1) THEN
C
C  Vertical Conductance
C    for vertical interface, "K" is layer below bottom of lake
        CNDFC1=0.0
        IF(K.EQ.NLAY.AND.LKARR1(I,J,K).GT.0) GO TO 315
        IF(BEDLAK(II).LE.0.0) GO TO 315
        CNDFC1 = BEDLAK(II)*DELR(I)*DELC(J)
        IF(LAYVKA(K).EQ.0) THEN
           VK=VKA(I,J,K)
        ELSE
           VK=HK(I,J,K)/VKA(I,J,K)
        END IF
c   skip if zero vk
        IF(VK.LE.0.0) GO TO 350
        BBOT=BOTM(I,J,LBOTM(K))
        TTOP=BOTM(I,J,LBOTM(K)-1)
        CAQ=VK*DELR(I)*DELC(J)/((TTOP-BBOT)*0.5)
        IF(LAYCBD(K-1).GT.0) THEN
c   skip if zero vkcb
          IF(VKCB(I,J,LAYCBD(K-1)).LE.0.0) GO TO 350
          BBOT=BOTM(I,J,LBOTM(K)-1)
          TTOP=BOTM(I,J,LBOTM(K-1))
          CCB=VKCB(I,J,LAYCBD(K-1))*DELR(I)*DELC(J)/(TTOP-BBOT)
          !include VKCB
          CAQ = 1.0/(1.0/CAQ + 1.0/CCB)
        END IF
        CNDFCT(II) = 1.0/(1.0/CAQ+1.0/CNDFC1)
  315   WRITE(IOUT,7325) (ILAKE(I1,II),I1=1,5),DELC(J),DELR(I),
     1             BEDLAK(II),CNDFC1,CAQ,CNDFCT(II)
      ELSE
C
C  Horizontal conductance
C
        TT = HK(I,J,K)
C X-DIRECTION
        IF(NTYP.EQ.2) CNDFC2 = 2.0*TT*DELC(J)/DELR(I)
C Y-DIRECTION
        IF(NTYP.EQ.3) THEN
          IF(CHANI(K).LE.0) THEN
            KH=-CHANI(K)
            CNDFC2 = 2.0*HANI(I,J,KH)*TT*DELR(I)/DELC(J)
          ELSE
            CNDFC2 = 2.0*CHANI(K)*TT*DELR(I)/DELC(J)
          END IF
        END IF
        IF(NTYP.EQ.2) CNDFC1 = BEDLAK(II)*DELC(J)
        IF(NTYP.EQ.3) CNDFC1 = BEDLAK(II)*DELR(I)
        IF (CNDFC1.GT.0.0.AND.CNDFC2.GT.0.0) 
     *         CNDFCT(II) = 1.0/(1.0/CNDFC2+1.0/CNDFC1)
        WRITE(IOUT,7325) (ILAKE(I1,II),I1=1,5),DELC(J),DELR(I),
     1    BEDLAK(II),CNDFC1,CNDFC2,CNDFCT(II)
c-lfk
 7325   FORMAT(1X,5I3,2X,1P,5E10.2,E11.3)
c 7325   FORMAT(1X,5I3,2X,1P,6E10.2)
      END IF
  350 CONTINUE
C
      RETURN
      END
C
C
      SUBROUTINE SGWF2LAK7HUF7RPS()
C
C     ******************************************************************
C     COMPUTE VERTICAL CONDUCTANCES AND HORIZONTAL CONDUCTANCES PER UNIT
C     THICKNESS FOR LAKES WHEN HUF PACKAGE IS USED
C     ******************************************************************
C
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFLAKMODULE, ONLY: LKNODE, BEDLAK, LKARR1, ILAKE, CNDFCT
      USE GLOBAL,       ONLY: NLAY, IOUT, DELR, DELC
!!      USE GLOBAL,       ONLY: NLAY, IOUT, LBOTM, DELR, DELC, BOTM
c-lfk      USE GWFLPFMODULE, ONLY: VKA, HK
      USE GWFHUFMODULE, ONLY: VKAH !!,HK,HKCC
C
      WRITE(IOUT,108)
  108 FORMAT(//9X,'C',15X,'INTERFACE CONDUCTANCES BETWEEN LAKE AND ',
     1  'AQUIFER CELLS'/
     2  3X,'L',5X,'O',10X,'(IF TYPE = 6, CONDUCTANCE (L^2/T) IS ',
     3  'BETWEEN AQUIFER CELL AND OVERLYING LAKE CELL.)',/
     4  3X,'A',5X,'L',2X,'L',2X,'T',
     5  4X,'(IF TYPE = 1 TO 4, CONDUCTANCES ARE PER UNIT SATURATED ',
     6  'THICKNESS (L/T).)'/
     7  3X,'Y',2X,'R',2X,'U',2X,'A',2X,'Y'/
     8  3X,'E',2X,'O',2X,'M',2X,'K',2X,'P',
     9  24X,'LAKEBED',6X,'C O N D U C T A N C E S'/3X,'R',2X,'W',2X,
     1  'N',2X,'E',
     2  2X,'E',5X,'DELTA Y',3X,'DELTA X',2X,'LEAKANCE',3X,'LAKEBED',3X,
     3  'AQUIFER',2X,'COMBINED'/1X,79('_'))
C
      DO 350 II=1,LKNODE
      K = ILAKE(1,II)
      J = ILAKE(2,II)
      I = ILAKE(3,II)
      CAQ = 0.0
      CNDFCT(II) = 0.0
C  Convert ILAKE(5,II):  1 and 2 are type 1,  3 and 4 are type 2, 6 is type 0
      NTYP = (ILAKE(5,II)+1)/2
      IF(NTYP.EQ.3) NTYP=0
      NTYP=NTYP + 1
      IF(NTYP.EQ.1) THEN
C
C  Vertical Conductance
C    for vertical interface, "K" is layer below bottom of lake
        CNDFC1=0.0
        IF(K.EQ.NLAY.AND.LKARR1(I,J,K).GT.0) GO TO 315
        IF(BEDLAK(II).LE.0.0) GO TO 315
        CNDFC1 = BEDLAK(II)*DELR(I)*DELC(J)
C-LFK
        VK=VKAH(I,J,K)
C-LFK   VK=VKA(I,J,K)
c   skip if zero vk
c-LFK        IF(VK.LE.0.0) GO TO 350
        IF(VK.LE.0.0) THEN
          CNDFCT(II) = CNDFC1
        ELSE
c          BBOT=BOTM(I,J,LBOTM(K))
c          TTOP=BOTM(I,J,LBOTM(K)-1)
c          CAQ=VK*DELR(I)*DELC(J)/((TTOP-BBOT)*0.5)
c          CNDFCT(II) = 1.0/(1.0/CAQ+1.0/CNDFC1)
c-lfk    When HUF is active, set effective lakebed conductance only on basis of user-specified lakebed leakance value
          CNDFCT(II) = CNDFC1
        END IF
C-LFK   (ABOVE BLOCK MODIFIED 1/2/2013 WITH IF-THEN-ELSE-ENDIF CONTROLS TO AVOID ZERO LKNCE)
  315   WRITE(IOUT,7325) (ILAKE(I1,II),I1=1,5),DELC(J),DELR(I),
c-lfk
     1             BEDLAK(II),CNDFC1,CNDFCT(II)
c    1             BEDLAK(II),CNDFC1,CAQ,CNDFCT(II)
      ELSE
C
C  Horizontal conductance
C
c        TT = HK(I,J,K)
C-LFK
c        TY = HKCC(I,J,K)
!gsf    TY = HKCC(I,J,K)
C-LFK        TY = 0.0     !gsf
C X-DIRECTION
c        IF(NTYP.EQ.2) CNDFC2 = 2.0*TT*DELC(J)/DELR(I)
C Y-DIRECTION
c        IF(NTYP.EQ.3) CNDFC2 = 2.0*TY*DELC(J)/DELR(I)
        IF(NTYP.EQ.2) CNDFC1 = BEDLAK(II)*DELC(J)
        IF(NTYP.EQ.3) CNDFC1 = BEDLAK(II)*DELR(I)
c        IF (CNDFC1.GT.0.0.AND.CNDFC2.GT.0.0) 
c     *         CNDFCT(II) = 1.0/(1.0/CNDFC2+1.0/CNDFC1)
c-lfk    When HUF is active, set effective lakebed conductance only on basis of user-specified lakebed leakance value
          CNDFCT(II) = CNDFC1
        WRITE(IOUT,7325) (ILAKE(I1,II),I1=1,5),DELC(J),DELR(I),
c-lfk
     1    BEDLAK(II),CNDFC1,CNDFCT(II)
c    1    BEDLAK(II),CNDFC1,CNDFC2,CNDFCT(II)
c-lfk
 7325   FORMAT(1X,5I3,2X,1P,4E10.2,10x,E11.3)
C 7325   FORMAT(1X,5I3,2X,1P,6E10.2)
      END IF
  350 CONTINUE
C
C--LFK   INFO FOR HUF-LAKE COMBO:
C  WRITE WARNINGS ON LAKE/AQUIFER CONDUCTANCES
          WRITE(IOUT,345)
  345     FORMAT(//5X,'NOTE: INFORMATION ABOUT CALCULATED LAKE/AQUIFER C
     1ONDUCTANCES WHEN USING HUF PACKAGE FOLLOWS: '/)
          WRITE(IOUT,346)
  346     FORMAT(1X,'FOR ALL NODE(S) ADJACENT TO LAKE: '/
     1    1X,'COMBINED LAKE/AQUIFER CONDUCTANCES BASED SOLELY ON LAKEBED
     2 LEAKANCE SPECIFICATION'/)
          WRITE(IOUT,'(//)')
C
      RETURN
      END
Cdep  Added function statements to compute derivatives for Newton method
Cdep     used in solving lake stage in the FORMULATE SUBROUTINE (LAK7FM).      
      DOUBLE PRECISION FUNCTION FINTERP (STAGE,LN)
Cdep&rgn  FUNCTION LINEARLY INTERPOLATES BETWEEN TWO VALUES
C          OF LAKE STAGE TO CALCULATE LAKE AREA.
C         ADDED 5/16/2006-- changed 12/2007 from "DOUBLE PRECISION FUNCTION"
C          to "FUNCTION"
      USE GWFLAKMODULE, ONLY: AREATABLE, DEPTHTABLE
      IMPLICIT NONE
      DOUBLE PRECISION STAGE, AREA, TOLF2, FOLD
      DOUBLE PRECISION a1, a2, d1, d2
      INTEGER LN, IFLG, I
      TOLF2=1.0E-7
      IF (STAGE.GT.DEPTHTABLE(151,LN))THEN
        FINTERP =  AREATABLE(151,LN) 
        RETURN
      END IF
      IFLG = 0
      I = 1
      DO WHILE ( IFLG.EQ.0 )
        a1 = AREATABLE(I,LN)
        a2 = AREATABLE(I+1,LN)
        d1 = DEPTHTABLE(I,LN)
        d2 = DEPTHTABLE(I+1,LN)
        FOLD=ABS(STAGE-d1)     
        IF (FOLD .LE. TOLF2) THEN  
          AREA=AREATABLE(I,LN)
          IFLG = 1
        ELSEIF (STAGE.GT.d1 .AND. STAGE.LT.d2)THEN
          AREA=((a2-a1)/(d2-d1))*STAGE+a2-((a2-a1)/(d2-d1))*d2
          IFLG = 1
        END IF
        I = I + 1
        IF( I.GT.150 ) THEN
          IFLG = 1 
          AREA = AREATABLE(151,LN)
        END IF
      END DO
      FINTERP = AREA
      RETURN
      END FUNCTION FINTERP
!  RGN Added function statements to compute calculate surface area form volume     
      DOUBLE PRECISION FUNCTION SURFTERP (VOLUME,LN)
C     FUNCTION LINEARLY INTERPOLATES BETWEEN TWO VALUES
C          OF LAKE VOLUME TO CALCULATE LAKE AREA.
      USE GWFLAKMODULE, ONLY: AREATABLE, VOLUMETABLE
      DOUBLE PRECISION VOLUME
      TOLF2=1.0E-7
      IF (VOLUME.GT.VOLUMETABLE(151,LN))THEN
        SURFTERP =  AREATABLE(151,LN) 
        RETURN
      END IF
      IFLG = 0
      I = 1
      DO WHILE ( IFLG.EQ.0 )
        FOLD=ABS(VOLUME-VOLUMETABLE(I,LN))     
        IF (FOLD .LE. TOLF2) THEN  
          AREA=AREATABLE(I,LN)
          IFLG = 1
        ELSEIF (VOLUME.GT.VOLUMETABLE(I,LN) .AND. VOLUME.LT.
     1          VOLUMETABLE(I+1,LN))THEN
          AREA=((AREATABLE(I+1,LN)-AREATABLE(I,LN))/
     1         (VOLUMETABLE(I+1,LN)- VOLUMETABLE(I,LN)))*
     2         VOLUME+AREATABLE(I+1,LN)-((AREATABLE(I+1,LN)-
     3         AREATABLE(I,LN))/(VOLUMETABLE(I+1,LN)-
     4         VOLUMETABLE(I,LN)))*VOLUMETABLE(I+1,LN)                 
          IFLG = 1
        END IF
        I = I + 1
        IF( I.GT.150 ) IFLG = 1 
      END DO
      SURFTERP = AREA
      RETURN
      END FUNCTION SURFTERP
!
!     Interpolate lake volume as a function of lake stage
C     used in solving lake stage in the FORMULATE SUBROUTINE (LAK7FM).      
      DOUBLE PRECISION FUNCTION VOLTERP (STAGE,LN)
C     FUNCTION LINEARLY INTERPOLATES BETWEEN TWO VALUES
C          OF LAKE STAGE TO CALCULATE LAKE VOLUME.
      USE GWFLAKMODULE, ONLY: VOLUMETABLE, DEPTHTABLE, AREATABLE
      IMPLICIT NONE
      INTEGER LN, IFLG, I
      DOUBLE PRECISION STAGE, VOLUME, TOLF2, FOLD
      TOLF2=1.0E-7
      IF (STAGE.GT.DEPTHTABLE(151,LN))THEN
 ! bug 5/4/09 changed FINTERP TO VOLUME
        VOLTERP =  VOLUMETABLE(151,LN)+(STAGE-DEPTHTABLE(151,LN))*
     +             AREATABLE(151,LN) 
        RETURN
      END IF
      IFLG = 0
      I = 1
      DO WHILE ( IFLG.EQ.0 )
        FOLD=ABS(STAGE-DEPTHTABLE(I,LN))     
        IF (FOLD .LE. TOLF2) THEN  
          VOLUME=VOLUMETABLE(I,LN)
          IFLG = 1
        ELSEIF (STAGE.GT.DEPTHTABLE(I,LN) .AND. STAGE.LT.
     1          DEPTHTABLE(I+1,LN))THEN
          VOLUME=((VOLUMETABLE(I+1,LN)-VOLUMETABLE(I,LN))/
     1         (DEPTHTABLE(I+1,LN)- DEPTHTABLE(I,LN)))*
     2         STAGE+VOLUMETABLE(I+1,LN)-((VOLUMETABLE(I+1,LN)-
     3         VOLUMETABLE(I,LN))/(DEPTHTABLE(I+1,LN)-
     4         DEPTHTABLE(I,LN)))*DEPTHTABLE(I+1,LN)                 
          IFLG = 1
        END IF
        I = I + 1
        IF( I.GT.150 ) THEN
          IFLG = 1 
          VOLUME = VOLUMETABLE(151,LN)
        END IF
      END DO
      VOLTERP = VOLUME
      IF ( VOLTERP.LT.TOLF2 ) VOLTERP = TOLF2
      RETURN
      END FUNCTION VOLTERP
!     Interpolate lake STAGE as a function of lake VOLUME
C     used in solving lake stage in the FORMULATE SUBROUTINE (LAK7FM).      
      DOUBLE PRECISION FUNCTION STGTERP (VOLUME,LN)
C     FUNCTION LINEARLY INTERPOLATES BETWEEN TWO VALUES
C          OF LAKE VOLUME TO CALCULATE LAKE STAGE.
      USE GWFLAKMODULE, ONLY: VOLUMETABLE, DEPTHTABLE,AREATABLE
      DOUBLE PRECISION VOLUME
!!      DOUBLE PRECISION VOLUME, STAGE
      TOLF2=1.0E-7
      IF (VOLUME.GT.VOLUMETABLE(151,LN))THEN
        STGTERP =  DEPTHTABLE(151,LN)+(VOLUME-VOLUMETABLE(151,LN))/
     +             AREATABLE(151,LN) 
        RETURN
      END IF
      IFLG = 0
      I = 1
      DO WHILE ( IFLG.EQ.0 )
        FOLD=ABS(VOLUME-VOLUMETABLE(I,LN))     
        IF (FOLD .LE. TOLF2) THEN  
          STGTERP=DEPTHTABLE(I,LN)
          IFLG = 1
        ELSEIF (VOLUME.GT.VOLUMETABLE(I,LN) .AND. VOLUME.LT.
     1          VOLUMETABLE(I+1,LN))THEN
          STGTERP=((DEPTHTABLE(I+1,LN)-DEPTHTABLE(I,LN))/
     1         (VOLUMETABLE(I+1,LN)- VOLUMETABLE(I,LN)))*
     2         VOLUME+DEPTHTABLE(I+1,LN)-((DEPTHTABLE(I+1,LN)-
     3         DEPTHTABLE(I,LN))/(VOLUMETABLE(I+1,LN)-
     4         VOLUMETABLE(I,LN)))*VOLUMETABLE(I+1,LN)                 
          IFLG = 1
        END IF
        I = I + 1
        IF( I.GT.150 ) THEN
          IFLG = 1 
          STGTERP= 0.0
        END IF
      END DO
      RETURN
      END FUNCTION STGTERP
C------FUNCTION DERIVTERP FOR INTERPOLATING DERIVATIVE OF LAKE OUTFLOW. 
      DOUBLE PRECISION FUNCTION DERIVTERP (STAGE,LSEG)
Cdep&rgn  FUNCTION LINEARLY INTERPOLATES BETWEEN TWO VALUES
C          OF LAKE STAGE TO CALCULATE LAKE OUTFLOW DERIVATIVE.
C         ADDED 5/16/2006-- changed 12/2007 from "DOUBLE PRECISION FUNCTION"
C          to "FUNCTION"
      USE GWFSFRMODULE, ONLY: DLKOTFLW, DLKSTAGE
      DOUBLE PRECISION STAGE, DEROTFLW, FOLD
      TOLF2=1.0E-7
      IF (STAGE.GT.DLKSTAGE(200,LSEG))THEN
        DERIVTERP =  DLKOTFLW(200,LSEG)
        RETURN
      END IF
      IFLG = 0
      I = 1
      DO WHILE ( IFLG.EQ.0 )
        FOLD=ABS(STAGE-DLKSTAGE(I,LSEG))     
        IF (FOLD .LE. TOLF2) THEN  
          DEROTFLW=DLKOTFLW(I,LSEG)
          IFLG = 1          !rsr, changed ISFLG to IFLG
        ELSEIF (STAGE.LT.DLKSTAGE(1,LSEG)) THEN
          DEROTFLW=0.0D0
          IFLG = 1
        ELSEIF (STAGE.GT.DLKSTAGE(I,LSEG) .AND. STAGE.LT.
     1          DLKSTAGE(I+1,LSEG))THEN
          DEROTFLW=((DLKOTFLW(I+1,LSEG)-DLKOTFLW(I,LSEG))/
     1         (DLKSTAGE(I+1,LSEG)- DLKSTAGE(I,LSEG)))*
     2         STAGE+DLKOTFLW(I+1,LSEG)-((DLKOTFLW(I+1,LSEG)-
     3         DLKOTFLW(I,LSEG))/(DLKSTAGE(I+1,LSEG)-
     4         DLKSTAGE(I,LSEG)))*DLKSTAGE(I+1,LSEG)
          IFLG = 1
        END IF
        I = I + 1
        IF( I.GT.199) IFLG = 1
      END DO
      DERIVTERP = DEROTFLW
      RETURN
      END FUNCTION DERIVTERP 
C------FUNCTION OUTFLWTERP FOR INTERPOLATING DERIVATIVE OF LAKE OUTFLOW. 
      DOUBLE PRECISION FUNCTION OUTFLWTERP (STAGE,LSEG)
Cdep&rgn  FUNCTION LINEARLY INTERPOLATES BETWEEN TWO VALUES
C          OF LAKE OUTFLOW STORED IN SLKOTFLW ARRAY.
C         ADDED 5/16/2006-- changed 12/2007 from "DOUBLE PRECISION FUNCTION"
C          to "FUNCTION"
      USE GWFSFRMODULE, ONLY: SLKOTFLW, DLKSTAGE
      DOUBLE PRECISION STAGE, OUTFLOW, FOLD
      TOLF2=1.0E-9
      IF (STAGE.GT.DLKSTAGE(200,LSEG))THEN
        OUTFLWTERP =  SLKOTFLW(200,LSEG) 
        RETURN
      END IF
      IFLG = 0
      I = 1
      DO WHILE ( IFLG.EQ.0 )
        FOLD=DABS(STAGE-DLKSTAGE(I,LSEG))     
        IF (FOLD .LE. TOLF2) THEN
          OUTFLOW=SLKOTFLW(I,LSEG)
          IFLG = 1
        ELSEIF (STAGE.LT.DLKSTAGE(1,LSEG)) THEN
          OUTFLOW=0.0D0
          IFLG = 1
        ELSEIF (STAGE.GT.DLKSTAGE(I,LSEG) .AND. STAGE.LT.
     1          DLKSTAGE(I+1,LSEG))THEN
          OUTFLOW=((SLKOTFLW(I+1,LSEG)-SLKOTFLW(I,LSEG))/
     1         (DLKSTAGE(I+1,LSEG)- DLKSTAGE(I,LSEG)))*
     2         STAGE+SLKOTFLW(I+1,LSEG)-((SLKOTFLW(I+1,LSEG)-
     3         SLKOTFLW(I,LSEG))/(DLKSTAGE(I+1,LSEG)-
     4         DLKSTAGE(I,LSEG)))*DLKSTAGE(I+1,LSEG)
          IFLG = 1
        END IF
        I = I + 1
        IF( I.GT.199) IFLG = 1
      END DO
      OUTFLWTERP = OUTFLOW
      RETURN
      END FUNCTION OUTFLWTERP    
C
C------FUNCTION FXLKOT_TERP FOR SMOOTHING SPECIFIED LAKE OUTFLOWS TO STREAMS.
C
      DOUBLE PRECISION FUNCTION FXLKOT_TERP(DSTAGE,Botlake,Splakout,dy)
      IMPLICIT NONE
      DOUBLE PRECISION DSTAGE,Botlake,Splakout, s, aa, ad, b, x, y, dy
      FXLKOT_TERP = 0.0D0
      s = 2.0
      x = DSTAGE-Botlake
      aa = -1.0d0/(s**2.0d0)
      ad = -2.0D0/(s**2.0d0)
      b = 2.0d0/s
      y = aa*x**2.0d0 + b*x
      dy = (ad*x + b)*Splakout
      IF ( x.LE.0.0 ) THEN
        y = 0.0D0
        dy = 0.0D0
      ELSE IF ( x-s.GT.-1.0e-14 ) THEN
        y = 1.0D0
        dy = 0.0D0
      END IF
      FXLKOT_TERP = y*Splakout
      END FUNCTION FXLKOT_TERP
C
      SUBROUTINE GET_FLOBOT(IC, IR, IL, ITYPE, INOFLO,CONDUC,
     1                FLOBOT,FLOBO3,FLOTOUZF,DLSTG,CLOSEZERO,H,
     2                THET1,ISS,LAKE,II,SURFDPTH,AREA,IUNITUZF,
     3                BOTLK,BOTCL,L1)
C
C     ******************************************************************
C     CALCULATE SEEPAGE BETWEEN LAKE AND GW CELLS
C     ******************************************************************
C
      USE GWFLAKMODULE
      USE GLOBAL,       ONLY: IBOUND, LBOTM, BOTM, LAYHDT
!!      USE GLOBAL,       ONLY: IBOUND, IOUT, LBOTM, BOTM, NLAY,LAYHDT
      USE GWFUZFMODULE, ONLY: IUZFBND,FINF,VKS
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     FUNCTIONS
C     -----------------------------------------------------------------
C     -----------------------------------------------------------------
C     ARGUMENTS
      DOUBLE PRECISION FLOBO3,FLOBOT,CONDUC,H,THET1,CLOSEZERO,DLSTG,
     1                 SURFDPTH,AREA,BOTLK,BOTCL,HH 
      INTEGER ISS, LAKE, II, IC, IR, IL, ITYPE, IUNITUZF, L1  
C     -----------------------------------------------------------------    
      INTEGER INOFLO
!!      INTEGER ICHECK, LI, INOFLO
      DOUBLE PRECISION FLOBO1,FLOBO2,CONDMX,BOTLKUP,
     1                 BOTLKDN,FLOTOUZF,RAMPGW,RAMPSTGO,RAMPSTGN,
     2                 HTEMP,HD,THCK,RAMPUP
!!     2                 RAMPSTGON,HTEMP,HD,THCK,RAMPUP
C
C5C-----INITIALIZE GROUNDWATER SEEPAGE VARIABLES AND CONDUCTANCE FACTOR.
      FLOBO1 = 0.0D0
      FLOBO2 = 0.0D0
C
C6------COMPUTE SEEPAGE INTO OR OUT OF A LAKE BED NODE WHEN ITYPE=0.
C       HEAD CANNOT FALL BELOW LAKE BOTTOM
        IF (ITYPE.EQ.0) THEN 
C
C6B------RAMP CONDUCTANCE ACROSS HORIZONTAL CELL FACE WHEN
C          LAKE STAGE AND GROUNDWATER HEAD NEAR LAKEBED.    
          BOTLKUP = BOTLK + SURFDPTH
          BOTLKDN = BOTLK
          CONDMX = CONDUC
          HH = H
          IF ( HH.LT.BOTLKDN ) THEN
            HH = BOTLKDN
            INOFLO = 1
          END IF
          IF(SURFDPTH.GT.CLOSEZERO) THEN
            RAMPGW = CONDMX-(CONDMX/SURFDPTH)*
     +                           (BOTLKUP-HH) 
            IF ( RAMPGW-CONDMX.GT.0.0D0 ) RAMPGW = CONDMX  
            IF ( RAMPGW.LE.0.0D0 ) RAMPGW = 0.0D0 
            RAMPSTGO = CONDMX-(CONDMX/SURFDPTH)*
     +                           (BOTLKUP-STGOLD(LAKE))
            IF ( RAMPSTGO-CONDMX.GT.0.0D0 ) RAMPSTGO = CONDMX   
            IF ( RAMPSTGO.LE.0.0D0 ) RAMPSTGO = 0.0D0
            RAMPSTGN = CONDMX-(CONDMX/SURFDPTH)*
     +                           (BOTLKUP-STGNEW(LAKE))      
            IF ( RAMPSTGN-CONDMX.GT.0.0D0 ) RAMPSTGN = CONDMX   
            IF ( RAMPSTGN.LE.0.0D0 ) RAMPSTGN = 0.0D0
          ELSE
            RAMPGW=CONDMX
            RAMPSTGO=CONDMX
            RAMPSTGN=CONDMX
          END IF
          IF( HH-BOTLKDN.GT.CLOSEZERO ) THEN
            HTEMP = HH
          ELSE
            HTEMP=BOTLKDN
          END IF
C
C6C------COMPUTE LAKE SEEPAGE FOR STGOLD USING FLOBO1.
C        USE UPSTREAM WEIGHTING
          IF ( HH.LT.STGOLD(LAKE) ) THEN
            RAMPUP = RAMPSTGO
          ELSE
            RAMPUP = RAMPGW
          END IF
          CONDUC = RAMPUP
          IF( STGOLD(LAKE)-BOTLKDN.GT.CLOSEZERO ) THEN
            FLOBO1=CONDUC*(STGOLD(LAKE)-HTEMP)
          ELSE
            FLOBO1=CONDUC*(BOTLKDN-HTEMP)
          END IF 
          IF ( IUNITUZF.GT.0 ) THEN
            IF ( IUZFBND(IC,IR).GT.0 )THEN
              IF (H-BOTLK.LT.-0.5*SURFDPTH) THEN           !11/3/14 changed to H for UZF
                IF ( VKS(IC,IR)*AREA-FLOBO1.LT.CLOSEZERO )
     +                          THEN
                  FLOBO1 = VKS(IC,IR)*AREA
                END IF
              END IF
            END IF
          END IF
C
C6D------COMPUTE LAKE SEEPAGE FOR STGNEW USING FLOBO2 AND FLOBO3.
C        USE UPSTREAM WEIGHTING
          IF ( HH.LT.STGNEW(LAKE) ) THEN
            RAMPUP = RAMPSTGN
          ELSE
            RAMPUP =  RAMPGW
          END IF
          CONDUC = RAMPUP
          IF( STGNEW(LAKE)-BOTLKDN.GT.CLOSEZERO ) THEN
            FLOBO2 = CONDUC*(STGNEW(LAKE)-HTEMP)       
            FLOBO3 = CONDUC*(STGNEW(LAKE)+DLSTG-HTEMP)
          ELSE
            FLOBO2 = CONDUC*(BOTLKDN-HTEMP)
            FLOBO3 = CONDUC*(BOTLKDN+DLSTG-HTEMP)
          END IF
          IF ( IUNITUZF.GT.0 ) THEN
            IF ( IUZFBND(IC,IR).GT.0 )THEN
              IF ( H-BOTLK.LT.-0.5*SURFDPTH ) THEN
                IF ( VKS(IC,IR)*AREA-FLOBO2.LT.CLOSEZERO )
     +                           THEN
                  FLOBO2 = VKS(IC,IR)*AREA
                  FLOBO3 = VKS(IC,IR)*AREA
                END IF
              END IF
            END IF
          END IF
C
C6E------COMPUTE LAKE SEEPAGE (FLOBOT) AS A FRACTION OF FLOBO1 AND 
C          FLOB02 AND FLOBO3 AS A FRACTION OF FLOBO1 AND FLOBO3.
          FLOBOT = THET1*FLOBO2 + (1.0D0-THET1)*FLOBO1
          FLOBO3 = THET1*FLOBO3 + (1.0D0-THET1)*FLOBO1  
!          CONDUC = THET1*RAMPSTGN + (1.0D0-THET1)*RAMPSTGO
          IF ( IUNITUZF.GT.0 ) THEN
            IF ( IUZFBND(IC,IR).GT.0 )THEN
              IF ( H-BOTLK.LT.-0.5*SURFDPTH ) THEN  !11/3/14 changed to H for UZF, was HH
                IF ( FLOBOT/AREA.GT.VKS(IC,IR) ) THEN
                  FLOBOT = VKS(IC,IR)*AREA
                  FLOBO3 = FLOTOUZF
                END IF
                FLOTOUZF = FLOBOT 
                FLOBOT = 0.0D0
                IF ( abs((STGNEW(LAKE)-BOTLK)) > closezero ) THEN
                  CONDUC = FLOTOUZF/(STGNEW(LAKE)-BOTLK)
                ELSE
                  CONDUC = 0.0
                END IF
                FINF(IC,IR)=FLOTOUZF/AREA
              END IF
            END IF
          END IF        
C
C7------COMPUTE SEEPAGE INTO OR OUT OF A LAKE WALL NODE 
C         WHEN ITYPE=1 OR 2.       
        ELSE IF ( ITYPE.EQ.1.OR.ITYPE.EQ.2 ) THEN
          IF( IBOUND(IC,IR,IL).GT.0 ) THEN 
            HD = H
            IF( H.GT.BOTM(IC,IR,LBOTM(IL)-1) )
     1                         HD = BOTM(IC,IR,LBOTM(IL)-1)
C
C7B------CONDUCTANCE ACROSS VERTICAL CELL FACE DEPENDENT ON 
C          SATURATED THICKNESS.
            IF ( LAYHDT(il).GT.0 ) THEN
              THCK = HD - BOTCL
            ELSE
              THCK = BOTM(IC,IR,LBOTM(IL)-1) - BOTCL
            END IF
            IF( THCK.LE.0.0 ) THCK = 0.0
            CONDUC = CONDUC*THCK 
            IF ( H.LT.BOTM(IC,IR,LBOTM(IL)) )
     +               H = BOTM(IC,IR,LBOTM(IL))
C
C7C------COMPUTE LAKE SEEPAGE FOR STGOLD USING FLOBO1. 
            IF( STGOLD(LAKE)-BOTCL.GT.CLOSEZERO ) THEN
              FLOBO1 = CONDUC*(STGOLD(LAKE)-H)  
            ELSE IF ( H-BOTCL.GT.CLOSEZERO ) THEN
              FLOBO1 = CONDUC*(BOTCL-H)  
            END IF
C
C7D------COMPUTE LAKE SEEPAGE FOR STGNEW USING FLOBO2 AND FLOBO3.
            IF( STGNEW(LAKE)-BOTCL.GT.CLOSEZERO )THEN
              FLOBO3 = CONDUC*(STGNEW(LAKE)+DLSTG-H)
              FLOBO2 = CONDUC*(STGNEW(LAKE)-H)                  
            ELSE IF ( H-BOTCL.GT.CLOSEZERO ) THEN
              FLOBO3 = CONDUC*(BOTCL+DLSTG-H)
              FLOBO2 = CONDUC*(BOTCL-H)
            ELSE IF ( STGNEW(LAKE)+DLSTG.GE.BOTCL )THEN
              FLOBO3 = CONDUC*(STGNEW(LAKE)+DLSTG-H)
            END IF 
C
C7E------COMPUTE LAKE SEEPAGE (FLOBOT) AS A FRACTION OF FLOBO1 AND 
C         FLOB02 AND FLOBO3 AS A FRACTION OF FLOBO1 AND FLOBO3.       
            FLOBOT = THET1*FLOBO2 + (1.0D0-THET1)*FLOBO1
            FLOBO3  = THET1*FLOBO3 + (1.0D0-THET1)*FLOBO1
            SUMCNN(LAKE) = SUMCNN(LAKE) + CONDUC
          END IF
        END IF
C
C8-------SEEPAGE RATES ADDED TO MATRIX AND RESIDUAL TERMS.
C8B------COMPUTE FLWITER AND FLWITER3 DURING FIRST LOOP THROUGH
C          CALCULATIONS. NEGATIVE FLOBOT MEANS INTO LAKE
        IF ( II==1 ) THEN
          IF ( FLOBOT.LT.0.0D0 ) FLWITER(LAKE) = 
     +             FLWITER(LAKE) - FLOBOT
          IF ( FLOBO3.LT.0.0D0 ) FLWITER3(LAKE) =  
     +             FLWITER3(LAKE) - FLOBO3
        END IF
C8C------COMPUTE FLWITER AND FLOWITER3 DURING SECOND LOOP THROUGH 
C          CALCULATIONS.
        IF ( II==2 ) THEN
          IF ( FLOBOT>=FLWITER(LAKE) ) THEN
            IF ( FLOBOT.GT.CLOSEZERO ) THEN
!              FLOBO2=FLWITER(LAKE)
!              FLOBOT = THET1*FLOBO2 + (1.0D0-THET1)*FLOBO1
              FLOBOT = FLWITER(LAKE)
              FLWITER(LAKE) = 0.0
              INOFLO = 1
            END IF
          ELSE IF ( FLOBOT.GT.CLOSEZERO )THEN
            FLWITER(LAKE) = FLWITER(LAKE) - FLOBOT
          END IF
          IF ( FLOTOUZF>=FLWITER(LAKE) ) THEN
            IF ( FLOTOUZF.GT.CLOSEZERO ) THEN
              FLOTOUZF=FLWITER(LAKE)
 !             FLOTOUZF = THET1*FLOTOUZF + (1.0D0-THET1)*FLOBO1
              FLWITER(LAKE) = 0.0
              INOFLO = 1
            END IF
          ELSE IF ( FLOTOUZF.GT.CLOSEZERO )THEN
            FLWITER(LAKE) = FLWITER(LAKE) - FLOTOUZF
          END IF
          IF ( FLOBO3>=FLWITER3(LAKE) ) THEN
            IF ( FLOBO3.GT.CLOSEZERO ) THEN
              FLOBO3=FLWITER3(LAKE)
 !             FLOBO3  = THET1*FLOBO3 + (1.0D0-THET1)*FLOBO1
              FLWITER3(LAKE) = 0.0
              INOFLO = 1
            END IF
          ELSE IF ( FLOBO3.GT.CLOSEZERO )THEN
            FLWITER3(LAKE) = FLWITER3(LAKE) - FLOBO3
          END IF
        END IF
C
C6E------COMPUTE LAKE SEEPAGE (FLOBOT) AS A FRACTION OF FLOBO1 AND 
C          FLOB02 AND FLOBO3 AS A FRACTION OF FLOBO1 AND FLOBO3.   
      RETURN
      END SUBROUTINE GET_FLOBOT   
C
C   
C
      SUBROUTINE GWF2LAK7DA(IUNITLAK, IGRID)
Cdep  End of FUNCTIONS used for Newton method in 
Cdep     FORMULATE SUBROUTINE (LAK7FM).
C  Deallocate LAK data  
      USE GWFLAKMODULE
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER, INTENT(IN) :: IUNITLAK, IGRID
C
      DEALLOCATE (GWFLAKDAT(IGRID)%NLAKES)
      DEALLOCATE (GWFLAKDAT(IGRID)%NLAKESAR)
      DEALLOCATE (GWFLAKDAT(IGRID)%THETA)
      DEALLOCATE (GWFLAKDAT(IGRID)%STGNEW)
      DEALLOCATE (GWFLAKDAT(IGRID)%STGOLD)
      DEALLOCATE (GWFLAKDAT(IGRID)%STGOLD2)
      DEALLOCATE (GWFLAKDAT(IGRID)%STGITER)
      DEALLOCATE (GWFLAKDAT(IGRID)%VOL)
      DEALLOCATE (GWFLAKDAT(IGRID)%LAKUNIT)
      IF ( IUNITLAK.LT.1 ) RETURN

      DEALLOCATE (GWFLAKDAT(IGRID)%ILKCB)
      DEALLOCATE (GWFLAKDAT(IGRID)%LAKTAB)
      DEALLOCATE (GWFLAKDAT(IGRID)%IRDTAB)
      DEALLOCATE (GWFLAKDAT(IGRID)%NSSITR)
Cdep  deallocate SURFDEPTH 3/3/2009
      DEALLOCATE (GWFLAKDAT(IGRID)%SURFDEPTH)
      DEALLOCATE (GWFLAKDAT(IGRID)%MXLKND)
      DEALLOCATE (GWFLAKDAT(IGRID)%LKNODE)
      DEALLOCATE (GWFLAKDAT(IGRID)%ICMX)
      DEALLOCATE (GWFLAKDAT(IGRID)%NCLS)
      DEALLOCATE (GWFLAKDAT(IGRID)%LWRT)
      DEALLOCATE (GWFLAKDAT(IGRID)%NDV)
      DEALLOCATE (GWFLAKDAT(IGRID)%NTRB)
      DEALLOCATE (GWFLAKDAT(IGRID)%SSCNCR)
      DEALLOCATE (GWFLAKDAT(IGRID)%ICS)
      DEALLOCATE (GWFLAKDAT(IGRID)%NCNCVR)
      DEALLOCATE (GWFLAKDAT(IGRID)%LIMERR)
      DEALLOCATE (GWFLAKDAT(IGRID)%ILAKE)
      DEALLOCATE (GWFLAKDAT(IGRID)%ITRB)
      DEALLOCATE (GWFLAKDAT(IGRID)%IDIV)
      DEALLOCATE (GWFLAKDAT(IGRID)%ISUB)
      DEALLOCATE (GWFLAKDAT(IGRID)%IRK)
      DEALLOCATE (GWFLAKDAT(IGRID)%LKARR1)
      DEALLOCATE (GWFLAKDAT(IGRID)%STAGES)
      DEALLOCATE (GWFLAKDAT(IGRID)%FLOB)
      DEALLOCATE (GWFLAKDAT(IGRID)%DSRFOT)
      DEALLOCATE (GWFLAKDAT(IGRID)%PRCPLK)
      DEALLOCATE (GWFLAKDAT(IGRID)%EVAPLK)
      DEALLOCATE (GWFLAKDAT(IGRID)%BEDLAK)
      DEALLOCATE (GWFLAKDAT(IGRID)%WTHDRW)
      DEALLOCATE (GWFLAKDAT(IGRID)%RNF)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMRNF)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMPPT)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMEVP)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMGWI)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMGWO)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMSWI)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMSWO)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMWDR)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMFLX)
      DEALLOCATE (GWFLAKDAT(IGRID)%CNDFCT)
      DEALLOCATE (GWFLAKDAT(IGRID)%VOLINIT)
      DEALLOCATE (GWFLAKDAT(IGRID)%BOTTMS)
      DEALLOCATE (GWFLAKDAT(IGRID)%BGAREA)
      DEALLOCATE (GWFLAKDAT(IGRID)%SSMN)
      DEALLOCATE (GWFLAKDAT(IGRID)%SSMX)
      DEALLOCATE (GWFLAKDAT(IGRID)%EVAP)
      DEALLOCATE (GWFLAKDAT(IGRID)%PRECIP)
      DEALLOCATE (GWFLAKDAT(IGRID)%EVAP3)
      DEALLOCATE (GWFLAKDAT(IGRID)%PRECIP3)
      DEALLOCATE (GWFLAKDAT(IGRID)%SEEP)
      DEALLOCATE (GWFLAKDAT(IGRID)%SEEP3)
      DEALLOCATE (GWFLAKDAT(IGRID)%SURFA)
      DEALLOCATE (GWFLAKDAT(IGRID)%SURFIN)
      DEALLOCATE (GWFLAKDAT(IGRID)%SURFOT)
      DEALLOCATE (GWFLAKDAT(IGRID)%SUMCNN)
      DEALLOCATE (GWFLAKDAT(IGRID)%SUMCHN)
      DEALLOCATE (GWFLAKDAT(IGRID)%CLAKE)
      DEALLOCATE (GWFLAKDAT(IGRID)%CRNF)
      DEALLOCATE (GWFLAKDAT(IGRID)%SILLVT)
      DEALLOCATE (GWFLAKDAT(IGRID)%CAUG)
      DEALLOCATE (GWFLAKDAT(IGRID)%CPPT)
      DEALLOCATE (GWFLAKDAT(IGRID)%CLAKINIT)
      DEALLOCATE (GWFLAKDAT(IGRID)%BDLKN1)
Cdep  Added arrays that track lake budgets for dry lakes
      DEALLOCATE (GWFLAKDAT(Igrid)%EVAPO)
      DEALLOCATE (GWFLAKDAT(Igrid)%WITHDRW)
      DEALLOCATE (GWFLAKDAT(Igrid)%FLWIN)
      DEALLOCATE (GWFLAKDAT(Igrid)%FLWITER)
      DEALLOCATE (GWFLAKDAT(Igrid)%FLWITER3)
      DEALLOCATE (GWFLAKDAT(Igrid)%GWRATELIM)
Cdep  Deallocate arrays used in conjunction with UZF Package
      DEALLOCATE (GWFLAKDAT(Igrid)%OVRLNDRNF)
      DEALLOCATE (GWFLAKDAT(Igrid)%CUMLNDRNF)
      DEALLOCATE (GWFLAKDAT(Igrid)%CUMUZF)
Cdep  Deallocate arrays for storing depth, and area arrays
      DEALLOCATE (GWFLAKDAT(Igrid)%DEPTHTABLE)
      DEALLOCATE (GWFLAKDAT(Igrid)%AREATABLE)
      DEALLOCATE (GWFLAKDAT(Igrid)%VOLUMETABLE)
      DEALLOCATE (GWFLAKDAT(Igrid)%XLAKES)
      DEALLOCATE (GWFLAKDAT(Igrid)%XLAKINIT)
      DEALLOCATE (GWFLAKDAT(Igrid)%XLKOLD)
Crsr allocate BD arrays
      DEALLOCATE (GWFLAKDAT(IGRID)%LDRY)
      DEALLOCATE (GWFLAKDAT(IGRID)%NCNT)
      DEALLOCATE (GWFLAKDAT(IGRID)%NCNST)
      DEALLOCATE (GWFLAKDAT(IGRID)%KSUB)
      DEALLOCATE (GWFLAKDAT(IGRID)%MSUB1)
      DEALLOCATE (GWFLAKDAT(IGRID)%MSUB)
      DEALLOCATE (GWFLAKDAT(IGRID)%FLXINL)
      DEALLOCATE (GWFLAKDAT(IGRID)%VOLOLD)
      DEALLOCATE (GWFLAKDAT(IGRID)%GWIN)
      DEALLOCATE (GWFLAKDAT(IGRID)%GWOUT)
      DEALLOCATE (GWFLAKDAT(IGRID)%DELH)
      DEALLOCATE (GWFLAKDAT(IGRID)%TDELH)
      DEALLOCATE (GWFLAKDAT(IGRID)%SVT)
      DEALLOCATE (GWFLAKDAT(IGRID)%STGADJ)
      DEALLOCATE (GWFLAKDAT(IGRID)%TOTGWIN_LAK)
      DEALLOCATE (GWFLAKDAT(IGRID)%TOTGWOT_LAK)
      DEALLOCATE (GWFLAKDAT(IGRID)%TOTDELSTOR_LAK)
      DEALLOCATE (GWFLAKDAT(IGRID)%TOTSTOR_LAK)
      DEALLOCATE (GWFLAKDAT(IGRID)%TOTEVAP_LAK)
      DEALLOCATE (GWFLAKDAT(IGRID)%TOTPPT_LAK)
      DEALLOCATE (GWFLAKDAT(IGRID)%TOTRUNF_LAK)
      DEALLOCATE (GWFLAKDAT(IGRID)%TOTWTHDRW_LAK)
      DEALLOCATE (GWFLAKDAT(IGRID)%TOTSURFIN_LAK)
      DEALLOCATE (GWFLAKDAT(IGRID)%TOTSURFOT_LAK)
      DEALLOCATE (GWFLAKDAT(IGRID)%VOLOLDD)
Cdep  Added arrays that calculate lake budgets 6/9/2009
      DEALLOCATE (GWFLAKDAT(IGRID)%DELVOL)
      DEALLOCATE (GWFLAKDAT(IGRID)%TSLAKERR)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMVOL)
      DEALLOCATE (GWFLAKDAT(IGRID)%CMLAKERR)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMLKIN)
      DEALLOCATE (GWFLAKDAT(IGRID)%CUMLKOUT)
      DEALLOCATE (GWFLAKDAT(IGRID)%LAKSEEP)
      DEALLOCATE (GWFLAKDAT(IGRID)%RUNF)      !EDM
      DEALLOCATE (GWFLAKDAT(IGRID)%RUNOFF)    !EDM
      DEALLOCATE (GWFLAKDAT(IGRID)%LKFLOWTYPE)
      DEALLOCATE (GWFLAKDAT(IGRID)%NLKFLWTYP)
      END SUBROUTINE GWF2LAK7DA

      SUBROUTINE SGWF2LAK7PNT(IGRID)
C  Set pointers to LAK data for grid      
      USE GWFLAKMODULE
C
      LKFLOWTYPE=>GWFLAKDAT(IGRID)%LKFLOWTYPE
      NLKFLWTYP=>GWFLAKDAT(IGRID)%NLKFLWTYP
      NLAKES=>GWFLAKDAT(IGRID)%NLAKES
      NLAKESAR=>GWFLAKDAT(IGRID)%NLAKESAR
      ILKCB=>GWFLAKDAT(IGRID)%ILKCB
      LAKTAB=>GWFLAKDAT(IGRID)%LAKTAB
      IRDTAB=>GWFLAKDAT(IGRID)%IRDTAB
      NSSITR=>GWFLAKDAT(IGRID)%NSSITR
      MXLKND=>GWFLAKDAT(IGRID)%MXLKND
      LKNODE=>GWFLAKDAT(IGRID)%LKNODE
      ICMX=>GWFLAKDAT(IGRID)%ICMX
      NCLS=>GWFLAKDAT(IGRID)%NCLS
      LWRT=>GWFLAKDAT(IGRID)%LWRT
      NDV=>GWFLAKDAT(IGRID)%NDV
      NTRB=>GWFLAKDAT(IGRID)%NTRB
      THETA=>GWFLAKDAT(IGRID)%THETA
      SSCNCR=>GWFLAKDAT(IGRID)%SSCNCR
Cdep  added SURFDEPTH 3/3/2009
      SURFDEPTH=>GWFLAKDAT(IGRID)%SURFDEPTH
      ICS=>GWFLAKDAT(IGRID)%ICS
      NCNCVR=>GWFLAKDAT(IGRID)%NCNCVR
      LIMERR=>GWFLAKDAT(IGRID)%LIMERR
      ILAKE=>GWFLAKDAT(IGRID)%ILAKE
      ITRB=>GWFLAKDAT(IGRID)%ITRB
      IDIV=>GWFLAKDAT(IGRID)%IDIV
      ISUB=>GWFLAKDAT(IGRID)%ISUB
      IRK=>GWFLAKDAT(IGRID)%IRK
      LKARR1=>GWFLAKDAT(IGRID)%LKARR1
      STAGES=>GWFLAKDAT(IGRID)%STAGES
      STGNEW=>GWFLAKDAT(IGRID)%STGNEW
      STGOLD=>GWFLAKDAT(IGRID)%STGOLD
      STGOLD2=>GWFLAKDAT(IGRID)%STGOLD2
      STGITER=>GWFLAKDAT(IGRID)%STGITER
      VOL=>GWFLAKDAT(IGRID)%VOL
      FLOB=>GWFLAKDAT(IGRID)%FLOB
      DSRFOT=>GWFLAKDAT(IGRID)%DSRFOT
      PRCPLK=>GWFLAKDAT(IGRID)%PRCPLK
      EVAPLK=>GWFLAKDAT(IGRID)%EVAPLK
      BEDLAK=>GWFLAKDAT(IGRID)%BEDLAK
      WTHDRW=>GWFLAKDAT(IGRID)%WTHDRW
      RNF=>GWFLAKDAT(IGRID)%RNF
      CUMRNF=>GWFLAKDAT(IGRID)%CUMRNF
      CUMPPT=>GWFLAKDAT(IGRID)%CUMPPT
      CUMEVP=>GWFLAKDAT(IGRID)%CUMEVP
      CUMGWI=>GWFLAKDAT(IGRID)%CUMGWI
      CUMGWO=>GWFLAKDAT(IGRID)%CUMGWO
      CUMSWI=>GWFLAKDAT(IGRID)%CUMSWI
      CUMSWO=>GWFLAKDAT(IGRID)%CUMSWO
      CUMWDR=>GWFLAKDAT(IGRID)%CUMWDR
      CUMFLX=>GWFLAKDAT(IGRID)%CUMFLX
      CNDFCT=>GWFLAKDAT(IGRID)%CNDFCT
      VOLINIT=>GWFLAKDAT(IGRID)%VOLINIT
      BOTTMS=>GWFLAKDAT(IGRID)%BOTTMS
      BGAREA=>GWFLAKDAT(IGRID)%BGAREA
      SSMN=>GWFLAKDAT(IGRID)%SSMN
      SSMX=>GWFLAKDAT(IGRID)%SSMX
      EVAP=>GWFLAKDAT(IGRID)%EVAP
      PRECIP=>GWFLAKDAT(IGRID)%PRECIP
      EVAP3=>GWFLAKDAT(IGRID)%EVAP3
      PRECIP3=>GWFLAKDAT(IGRID)%PRECIP3
      SEEP=>GWFLAKDAT(IGRID)%SEEP
      SEEP3=>GWFLAKDAT(IGRID)%SEEP3
      SURFA=>GWFLAKDAT(IGRID)%SURFA
      SURFIN=>GWFLAKDAT(IGRID)%SURFIN
      SURFOT=>GWFLAKDAT(IGRID)%SURFOT
      SUMCNN=>GWFLAKDAT(IGRID)%SUMCNN
      SUMCHN=>GWFLAKDAT(IGRID)%SUMCHN
      CLAKE=>GWFLAKDAT(IGRID)%CLAKE
      CRNF=>GWFLAKDAT(IGRID)%CRNF
      SILLVT=>GWFLAKDAT(IGRID)%SILLVT
      CAUG=>GWFLAKDAT(IGRID)%CAUG
      CPPT=>GWFLAKDAT(IGRID)%CPPT
      CLAKINIT=>GWFLAKDAT(IGRID)%CLAKINIT
      BDLKN1=>GWFLAKDAT(IGRID)%BDLKN1
Cdep  Added arrays that track lake budgets for dry lakes
      EVAPO=>GWFLAKDAT(Igrid)%EVAPO
      WITHDRW=>GWFLAKDAT(Igrid)%WITHDRW
      FLWIN=>GWFLAKDAT(Igrid)%FLWIN
      FLWITER=>GWFLAKDAT(Igrid)%FLWITER
      FLWITER3=>GWFLAKDAT(Igrid)%FLWITER3
      GWRATELIM=>GWFLAKDAT(Igrid)%GWRATELIM
Cdep  added two variable arrays
      OVRLNDRNF=>GWFLAKDAT(Igrid)%OVRLNDRNF
      CUMLNDRNF=>GWFLAKDAT(Igrid)%CUMLNDRNF
      CUMUZF=>GWFLAKDAT(Igrid)%CUMUZF
Cdep  added three variable arrays for depth,area, and volume
      DEPTHTABLE=>GWFLAKDAT(Igrid)%DEPTHTABLE
      AREATABLE=>GWFLAKDAT(Igrid)%AREATABLE   
      VOLUMETABLE=>GWFLAKDAT(Igrid)%VOLUMETABLE   
      XLAKES=>GWFLAKDAT(Igrid)%XLAKES
      XLAKINIT=>GWFLAKDAT(Igrid)%XLAKINIT
      XLKOLD=>GWFLAKDAT(Igrid)%XLKOLD
Crsr allocate BD arrays
      LDRY=>GWFLAKDAT(IGRID)%LDRY
      NCNT=>GWFLAKDAT(IGRID)%NCNT
      NCNST=>GWFLAKDAT(IGRID)%NCNST
      KSUB=>GWFLAKDAT(IGRID)%KSUB
      MSUB1=>GWFLAKDAT(IGRID)%MSUB1
      MSUB=>GWFLAKDAT(IGRID)%MSUB
      FLXINL=>GWFLAKDAT(IGRID)%FLXINL
      VOLOLD=>GWFLAKDAT(IGRID)%VOLOLD
      GWIN=>GWFLAKDAT(IGRID)%GWIN
      GWOUT=>GWFLAKDAT(IGRID)%GWOUT
      DELH=>GWFLAKDAT(IGRID)%DELH
      TDELH=>GWFLAKDAT(IGRID)%TDELH
      SVT=>GWFLAKDAT(IGRID)%SVT
      STGADJ=>GWFLAKDAT(IGRID)%STGADJ
      TOTGWIN_LAK=>GWFLAKDAT(IGRID)%TOTGWIN_LAK
      TOTGWOT_LAK=>GWFLAKDAT(IGRID)%TOTGWOT_LAK
      TOTDELSTOR_LAK=>GWFLAKDAT(IGRID)%TOTDELSTOR_LAK
      TOTSTOR_LAK=>GWFLAKDAT(IGRID)%TOTSTOR_LAK
      TOTEVAP_LAK=>GWFLAKDAT(IGRID)%TOTEVAP_LAK
      TOTPPT_LAK=>GWFLAKDAT(IGRID)%TOTPPT_LAK
      TOTRUNF_LAK=>GWFLAKDAT(IGRID)%TOTRUNF_LAK
      TOTWTHDRW_LAK=>GWFLAKDAT(IGRID)%TOTWTHDRW_LAK
      TOTSURFIN_LAK=>GWFLAKDAT(IGRID)%TOTSURFIN_LAK
      TOTSURFOT_LAK=>GWFLAKDAT(IGRID)%TOTSURFOT_LAK
      LAKUNIT=>GWFLAKDAT(IGRID)%LAKUNIT
      VOLOLDD=>GWFLAKDAT(IGRID)%VOLOLDD
Cdep  Allocate lake budget error arrays 6/9/2009
      DELVOL=>GWFLAKDAT(IGRID)%DELVOL
      TSLAKERR=>GWFLAKDAT(IGRID)%TSLAKERR
      CUMVOL=>GWFLAKDAT(IGRID)%CUMVOL
      CMLAKERR=>GWFLAKDAT(IGRID)%CMLAKERR
      CUMLKOUT=>GWFLAKDAT(IGRID)%CUMLKOUT
      CUMLKIN=>GWFLAKDAT(IGRID)%CUMLKIN
      LAKSEEP=>GWFLAKDAT(IGRID)%LAKSEEP
      RUNF=>GWFLAKDAT(IGRID)%RUNF        !EDM
      RUNOFF=>GWFLAKDAT(IGRID)%RUNOFF    !EDM
      END SUBROUTINE SGWF2LAK7PNT

      SUBROUTINE SGWF2LAK7PSV1(IGRID)
C  Save LAK data for a grid for data shared with SFR
      USE GWFLAKMODULE
C
      GWFLAKDAT(IGRID)%NLAKES=>NLAKES
      GWFLAKDAT(IGRID)%NLAKESAR=>NLAKESAR
      GWFLAKDAT(IGRID)%THETA=>THETA
      GWFLAKDAT(IGRID)%STGOLD=>STGOLD
      GWFLAKDAT(IGRID)%STGOLD2=>STGOLD2
      GWFLAKDAT(IGRID)%STGNEW=>STGNEW
      GWFLAKDAT(IGRID)%STGITER=>STGITER
      GWFLAKDAT(IGRID)%VOL=>VOL
      GWFLAKDAT(IGRID)%LAKUNIT=>LAKUNIT
      END SUBROUTINE SGWF2LAK7PSV1

      SUBROUTINE SGWF2LAK7PSV(IGRID)
C  Save LAK data for a grid
      USE GWFLAKMODULE
C
      GWFLAKDAT(IGRID)%LKFLOWTYPE=>LKFLOWTYPE
      GWFLAKDAT(IGRID)%NLKFLWTYP=>NLKFLWTYP
      GWFLAKDAT(IGRID)%ILKCB=>ILKCB
      GWFLAKDAT(IGRID)%NSSITR=>NSSITR
      GWFLAKDAT(IGRID)%MXLKND=>MXLKND
      GWFLAKDAT(IGRID)%LKNODE=>LKNODE
      GWFLAKDAT(IGRID)%ICMX=>ICMX
      GWFLAKDAT(IGRID)%LAKTAB=>LAKTAB
      GWFLAKDAT(IGRID)%IRDTAB=>IRDTAB
      GWFLAKDAT(IGRID)%NCLS=>NCLS
      GWFLAKDAT(IGRID)%LWRT=>LWRT
      GWFLAKDAT(IGRID)%NDV=>NDV
      GWFLAKDAT(IGRID)%NTRB=>NTRB
      GWFLAKDAT(IGRID)%SSCNCR=>SSCNCR
Cdep  Added SURDEPTH 3/3/2009
      GWFLAKDAT(IGRID)%SURFDEPTH=>SURFDEPTH
      GWFLAKDAT(IGRID)%ICS=>ICS
      GWFLAKDAT(IGRID)%NCNCVR=>NCNCVR
      GWFLAKDAT(IGRID)%LIMERR=>LIMERR
      GWFLAKDAT(IGRID)%ILAKE=>ILAKE
      GWFLAKDAT(IGRID)%ITRB=>ITRB
      GWFLAKDAT(IGRID)%IDIV=>IDIV
      GWFLAKDAT(IGRID)%ISUB=>ISUB
      GWFLAKDAT(IGRID)%IRK=>IRK
      GWFLAKDAT(IGRID)%LKARR1=>LKARR1
      GWFLAKDAT(IGRID)%STAGES=>STAGES
      GWFLAKDAT(IGRID)%FLOB=>FLOB
      GWFLAKDAT(IGRID)%DSRFOT=>DSRFOT
      GWFLAKDAT(IGRID)%PRCPLK=>PRCPLK
      GWFLAKDAT(IGRID)%EVAPLK=>EVAPLK
      GWFLAKDAT(IGRID)%BEDLAK=>BEDLAK
      GWFLAKDAT(IGRID)%WTHDRW=>WTHDRW
      GWFLAKDAT(IGRID)%RNF=>RNF
      GWFLAKDAT(IGRID)%CUMRNF=>CUMRNF
      GWFLAKDAT(IGRID)%CUMPPT=>CUMPPT
      GWFLAKDAT(IGRID)%CUMEVP=>CUMEVP
      GWFLAKDAT(IGRID)%CUMGWI=>CUMGWI
      GWFLAKDAT(IGRID)%CUMGWO=>CUMGWO
      GWFLAKDAT(IGRID)%CUMSWI=>CUMSWI
      GWFLAKDAT(IGRID)%CUMSWO=>CUMSWO
      GWFLAKDAT(IGRID)%CUMWDR=>CUMWDR
      GWFLAKDAT(IGRID)%CUMFLX=>CUMFLX
      GWFLAKDAT(IGRID)%CNDFCT=>CNDFCT
      GWFLAKDAT(IGRID)%VOLINIT=>VOLINIT
      GWFLAKDAT(IGRID)%BOTTMS=>BOTTMS
      GWFLAKDAT(IGRID)%BGAREA=>BGAREA
      GWFLAKDAT(IGRID)%SSMN=>SSMN
      GWFLAKDAT(IGRID)%SSMX=>SSMX
      GWFLAKDAT(IGRID)%EVAP=>EVAP
      GWFLAKDAT(IGRID)%PRECIP=>PRECIP
      GWFLAKDAT(IGRID)%EVAP3=>EVAP3
      GWFLAKDAT(IGRID)%PRECIP3=>PRECIP3
      GWFLAKDAT(IGRID)%SEEP=>SEEP
      GWFLAKDAT(IGRID)%SEEP3=>SEEP3
      GWFLAKDAT(IGRID)%SURFA=>SURFA
      GWFLAKDAT(IGRID)%SURFIN=>SURFIN
      GWFLAKDAT(IGRID)%SURFOT=>SURFOT
      GWFLAKDAT(IGRID)%SUMCNN=>SUMCNN
      GWFLAKDAT(IGRID)%SUMCHN=>SUMCHN
      GWFLAKDAT(IGRID)%CLAKE=>CLAKE
      GWFLAKDAT(IGRID)%CRNF=>CRNF
      GWFLAKDAT(IGRID)%SILLVT=>SILLVT
      GWFLAKDAT(IGRID)%CAUG=>CAUG
      GWFLAKDAT(IGRID)%CPPT=>CPPT
      GWFLAKDAT(IGRID)%CLAKINIT=>CLAKINIT
      GWFLAKDAT(IGRID)%BDLKN1=>BDLKN1
Cdep  Added arrays that track lake budgets for dry lakes
      GWFLAKDAT(Igrid)%EVAPO=>EVAPO
      GWFLAKDAT(Igrid)%WITHDRW=>WITHDRW
      GWFLAKDAT(Igrid)%FLWIN=>FLWIN
      GWFLAKDAT(Igrid)%FLWITER=>FLWITER
      GWFLAKDAT(Igrid)%FLWITER3=>FLWITER3
      GWFLAKDAT(Igrid)%GWRATELIM=>GWRATELIM
Cdep  added two variable arrays
      GWFLAKDAT(Igrid)%OVRLNDRNF=>OVRLNDRNF
      GWFLAKDAT(Igrid)%CUMLNDRNF=>CUMLNDRNF
      GWFLAKDAT(Igrid)%CUMUZF=>CUMUZF
Cdep  added three variable arrays for depth, area, and volume
      GWFLAKDAT(Igrid)%DEPTHTABLE=>DEPTHTABLE
      GWFLAKDAT(Igrid)%AREATABLE=>AREATABLE
      GWFLAKDAT(Igrid)%VOLUMETABLE=>VOLUMETABLE
      GWFLAKDAT(Igrid)%XLAKES=>XLAKES
      GWFLAKDAT(Igrid)%XLAKINIT=>XLAKINIT
      GWFLAKDAT(Igrid)%XLKOLD=>XLKOLD
Crsr allocate BD arrays
      GWFLAKDAT(IGRID)%LDRY=>LDRY
      GWFLAKDAT(IGRID)%NCNT=>NCNT
      GWFLAKDAT(IGRID)%NCNST=>NCNST
      GWFLAKDAT(IGRID)%KSUB=>KSUB
      GWFLAKDAT(IGRID)%MSUB1=>MSUB1
      GWFLAKDAT(IGRID)%MSUB=>MSUB
      GWFLAKDAT(IGRID)%FLXINL=>FLXINL
      GWFLAKDAT(IGRID)%VOLOLD=>VOLOLD
      GWFLAKDAT(IGRID)%GWIN=>GWIN
      GWFLAKDAT(IGRID)%GWOUT=>GWOUT
      GWFLAKDAT(IGRID)%DELH=>DELH
      GWFLAKDAT(IGRID)%TDELH=>TDELH
      GWFLAKDAT(IGRID)%SVT=>SVT
      GWFLAKDAT(IGRID)%STGADJ=>STGADJ
Cdep  Allocate lake budget error arrays 6/9/2009
      GWFLAKDAT(IGRID)%DELVOL=>DELVOL
      GWFLAKDAT(IGRID)%TSLAKERR=>TSLAKERR
      GWFLAKDAT(IGRID)%CUMVOL=>CUMVOL
      GWFLAKDAT(IGRID)%CMLAKERR=>CMLAKERR
      GWFLAKDAT(IGRID)%CUMLKOUT=>CUMLKOUT
      GWFLAKDAT(IGRID)%CUMLKIN=>CUMLKIN
crgn Allocate budget arrays for GSFLOW CSV file
      GWFLAKDAT(IGRID)%TOTGWIN_LAK=>TOTGWIN_LAK
      GWFLAKDAT(IGRID)%TOTGWOT_LAK=>TOTGWOT_LAK
      GWFLAKDAT(IGRID)%TOTDELSTOR_LAK=>TOTDELSTOR_LAK
      GWFLAKDAT(IGRID)%TOTSTOR_LAK=>TOTSTOR_LAK
      GWFLAKDAT(IGRID)%TOTEVAP_LAK=>TOTEVAP_LAK
      GWFLAKDAT(IGRID)%TOTPPT_LAK=>TOTPPT_LAK
      GWFLAKDAT(IGRID)%TOTRUNF_LAK=>TOTRUNF_LAK
      GWFLAKDAT(IGRID)%TOTWTHDRW_LAK=>TOTWTHDRW_LAK
      GWFLAKDAT(IGRID)%TOTSURFIN_LAK=>TOTSURFIN_LAK
      GWFLAKDAT(IGRID)%TOTSURFOT_LAK=>TOTSURFOT_LAK 
      GWFLAKDAT(IGRID)%VOLOLDD=>VOLOLDD  
      GWFLAKDAT(IGRID)%LAKSEEP=>LAKSEEP
      GWFLAKDAT(IGRID)%RUNF=>RUNF        !EDM
      GWFLAKDAT(IGRID)%RUNOFF=>RUNOFF    !EDM
      END SUBROUTINE SGWF2LAK7PSV
