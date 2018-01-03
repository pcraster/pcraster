C
C
C     ******************************************************************
C     CHECK FOR STREAMBED BELOW CELL BOTTOM. RECORD REACHES FOR PRINTING
C     ******************************************************************
      MODULE ICHKSTRBOT_MODULE
      USE GWFSFRMODULE,ONLY:ISTRM,STRM,NSTRM
      USE GLOBAL,ONLY:BOTM,IBOUND,LBOTM
      implicit none
      type check_bot
        integer ltype,irchnum,iflag,iunit
      end type check_bot      
      public check_bot
      CONTAINS
      FUNCTION ICHKSTRBOT(self)
      type (check_bot), intent(in) :: self
      INTEGER JRCH,IRCH,KRCH,JSEG,ISEG,ICHKSTRBOT
      ICHKSTRBOT = 0
      KRCH = ISTRM(1,self%IRCHNUM)
      IRCH = ISTRM(2,self%IRCHNUM)
      JRCH = ISTRM(3,self%IRCHNUM)
      JSEG = ISTRM(4,self%IRCHNUM)
      ISEG = ISTRM(5,self%IRCHNUM)
      IF ( self%LTYPE.GT.0  .AND. IBOUND(JRCH,IRCH,KRCH).GT.0 ) THEN 
        IF ( STRM(4, self%IRCHNUM)-BOTM(JRCH,IRCH,LBOTM(KRCH))
     +                                      .LT.-1.0E-12 ) THEN
          IF ( self%IFLAG.EQ.0 ) THEN
          WRITE(self%IUNIT,*)
          WRITE(self%IUNIT,*)' REACHES WITH ALTITUDE ERRORS:'
          WRITE(self%IUNIT,*)'   LAY    ROW    COL    SEG  REACH      ',
     +                'STR.ELEV.      CELL-BOT.'
          END IF
          WRITE(self%IUNIT,100)KRCH,IRCH,JRCH,JSEG,ISEG,
     +                STRM(4, self%IRCHNUM),BOTM(JRCH,IRCH,LBOTM(KRCH))
          ICHKSTRBOT = 1
        END IF
      END IF
      IF ( self%IFLAG.GT.0 .AND. self%IRCHNUM.EQ.NSTRM ) THEN
        WRITE(self%IUNIT,*)' MODEL STOPPING DUE TO REACH ALTITUDE ERROR'
        CALL USTOP(' ')
      END IF
  100 FORMAT(5I7,2F15.7)
      END FUNCTION ICHKSTRBOT
      END MODULE ICHKSTRBOT_MODULE
C
C-------SUBROUTINE GWF2SFR7AR
      SUBROUTINE GWF2SFR7AR(In, Iunitbcf, Iunitlpf, Iunithuf, Iunitgwt, 
     +                      Nsol, Iouts, Iunituzf, Igrid)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR STREAMS
C     INITIALIZE VARIABLES FOR SFR PACKAGES
C     READ STREAM DATA THAT IS CONSTANT FOR ENTIRE SIMULATION:
C     REACH DATA AND PARAMETER DEFINITIONS
!--------REVISED FOR MODFLOW-2005 RELEASE 1.9, FEBRUARY 6, 2012
C     ******************************************************************
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFSFRMODULE
      USE GLOBAL,       ONLY: IOUT, IBOUND, BOTM, STRT, DELR, DELC, 
     +                        ITRSS, NCOL, NROW, LAYHDT, IUNIT  !CJM added ncol and nrow
      USE GWFLPFMODULE, ONLY: SC2LPF=>SC2
      USE GWFBCFMODULE, ONLY: SC1, SC2, LAYCON
      USE GWFHUFMODULE, ONLY: SC2HUF
      USE ICHKSTRBOT_MODULE
      IMPLICIT NONE
      INTRINSIC ABS, DBLE
      type (check_bot) :: uzfar_check
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER In, Iunitbcf, Iunitlpf, Iunithuf, Iunitgwt, Nsol, Iouts, 
     +        Iunituzf, Igrid
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      CHARACTER*200 line
!IFACE
      CHARACTER*8 face
      INTEGER iface,ndash
      INTEGER lloc, istart, istop, nparseg, i, ii, nlst, lb, ichk, icalc
      INTEGER nseg, nreach, krch, irch, jrch, jseg, ireach, ksfropt
      INTEGER krck, irck, jrck, jsegck, ireachck, kkptflg, ib
      INTEGER lstsum, lstbeg, numinst, idum(1), ip, iterp, mstrmar
      INTEGER nssar, nstrmar, NPP, MXVL, IRFG
      INTEGER intchk, Iostat
!!      INTEGER nssar, nstrmar, Ltyp, NPP, MXVL, IRFG, ITRFLG
      INTEGER k, kkrch, IERR, IFLG
      REAL r, seglen, sumlen, thsslpe, thislpe, uhcslpe, rchlen, dist
      REAL epsslpe
      character(len=16)  :: text        = 'SFR2'
      logical :: found
C     ------------------------------------------------------------------
      Version_sfr =
     +'$Id: gwf2sfr7_NWT.f 2359 2012-04-05 00:41:23Z rniswon $'
      iterp = 1
      idum(1) = 0
      ALLOCATE (NSS, NSTRM,TOTSPFLOW)
      ALLOCATE (NSFRPAR, ISTCB1, ISTCB2, IUZT, MAXPTS)
      ALLOCATE (ISFROPT, NSTRAIL, ISUZN, NSFRSETS)
      ALLOCATE (NUZST, NSTOTRL, NUMAVE)
      ALLOCATE (ITMP, IRDFLG, IPTFLG, NP)
      ALLOCATE (CONST, DLEAK, IRTFLG, NUMTIM, WEIGHT, FLWTOL)
      ALLOCATE (NSEGDIM)
      ALLOCATE (SFRRATIN, SFRRATOUT)
      ALLOCATE (STRMDELSTOR_CUM, STRMDELSTOR_RATE)
      ALLOCATE (ITRFLG)
      ALLOCATE (FLOWTYPE(5)) ! POSITION 1: VOLUME; 2: REACH LENGTH; 3: PRECIP; 4: EVAP; 5: RUNOFF
      ALLOCATE (NFLOWTYPE)
      IF(IUNIT(49).GT.0) THEN
        ALLOCATE (NINTOT)                             !EDM - FOR LMT
      ENDIF
      ALLOCATE (FACTOR)
C1------IDENTIFY PACKAGE AND INITIALIZE NSTRM.
      WRITE (IOUT, 9001) In
 9001 FORMAT (1X, /, ' SFR7 -- STREAMFLOW ROUTING PACKAGE, '
     +        ,'VERSION 1.0.8, 2017-02-01', /, 9X, 
     +         'INPUT READ FROM UNIT', I4)
C
C2------READ COMMENT RECORDS, NSTRM, NSS, NSFRPAR, NPARSEG, CONST,
C         DLEAK, ISTCB1, ISTCB2.
      ISFROPT = 0
      IUZT = 0
      IRTFLG = 0
      NUMTIM = 1
      NSEGDIM = 1
      FLWTOL = 1.0E-4
      STRMDELSTOR_CUM = 0.0E0
      STRMDELSTOR_RATE = 0.0E0
      SFRRATIN = 0.0
      SFRRATOUT = 0.0
      TOTSPFLOW = 0.0D0
      NUMTAB = 0
      MAXVAL = 1
      IRFG = 0
      MXVL = 0
      NPP = 0
      ITRFLG = 0
      lloc = 1
      IERR = 0
      IFLG = 0
      found = .false.
      factor = 1.0
      NFLOWTYPE=0
      IF(IUNIT(49).GT.0) THEN  !IUNIT(49): LMT
        FLOWTYPE(1) = 'NA'
        FLOWTYPE(2) = 'NA'
        FLOWTYPE(3) = 'NA'
        FLOWTYPE(4) = 'NA'
        FLOWTYPE(5) = 'NA'
      ENDIF
C
C2A------CHECK FOR KEYWORDS.  IF NO VALID KEYWORDS FOUND
C        THEN VERIFY THAT FIRST VALUE IS INTEGER AND PROCEED.
      CALL URDCOM(In, IOUT, line)
      CALL UPARLSTAL(IN,IOUT,LINE,NPP,MXVL)
      DO
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
        select case (LINE(ISTART:ISTOP))
          case('OPTIONS')
            write(iout,'(/1x,a)') 'PROCESSING '//
     +            trim(adjustl(text)) //' OPTIONS'
          case('REACHINPUT')
            IRFG = 1
            WRITE(IOUT,32)
            found = .true.
!support old input style
            do
              CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
              select case (LINE(ISTART:ISTOP))
              case('TRANSROUTE')
                  ITRFLG = 1
                  WRITE(iout,*)
                  WRITE(IOUT,'(A)')' TRANSIENT ROUTING IN STREAMS ',
     +                            'IS ACTIVE'
                  WRITE(iout,*)
              case('TABFILES')
                  CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NUMTAB,R,IOUT,IN)
                  IF(NUMTAB.LT.0) NUMTAB=0
                  CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MAXVAL,R,IOUT,IN)
                  IF(MAXVAL.LT.0) MAXVAL=0
                  WRITE(IOUT,31) NUMTAB,MAXVAL
              case default
                exit
             end select
           end do
!support old input style
          case('TRANSROUTE')
            ITRFLG = 1
            WRITE(iout,*)
            WRITE(IOUT,'(A)')' TRANSIENT ROUTING IN STREAMS IS ACTIVE'
            WRITE(iout,*)
            found = .true.
          case('TABFILES')
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NUMTAB,R,IOUT,IN)
            IF(NUMTAB.LT.0) NUMTAB=0
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MAXVAL,R,IOUT,IN)
            IF(MAXVAL.LT.0) MAXVAL=0
            WRITE(IOUT,31) NUMTAB,MAXVAL
            found = .true.
          case('LOSSFACTOR')
            WRITE(IOUT,*)
            CALL URWORD(line, lloc, istart, istop, 3, i, FACTOR,IOUT,In)
            WRITE(IOUT,33) FACTOR
           found = .true.
          case ('END')
            write(iout,'(/1x,a)') 'END PROCESSING '//
     +            trim(adjustl(text)) //' OPTIONS'
            CALL URDCOM(In, IOUT, line)
            exit
          case default
            read(line(istart:istop),*,IOSTAT=Iostat) intchk
            if( Iostat .ne. 0 ) then
              ! Not an integer.  Likely misspelled or unsupported 
              ! so terminate here.
              WRITE(IOUT,*) 'Invalid '//trim(adjustl(text))
     +                   //' Option: '//LINE(ISTART:ISTOP)
              CALL USTOP('Invalid '//trim(adjustl(text))
     +                   //' Option: '//LINE(ISTART:ISTOP))
            else
              ! Integer found.  This is likely NSTRM, so exit.
              write(iout,'(/1x,a)') 'END PROCESSING '//
     +          trim(adjustl(text)) //' OPTIONS'
              exit
            endif
        end select
        CALL URDCOM(In, IOUT, line)
      ENDDO
   32 FORMAT(1X,I10,' Some stream information will be read by reach. ',
     +                'This option replaces NSTRM<0')
   31 FORMAT(1X,I10,' Specified inflow files will be read ',
     +                 'with a maximum of ',I10,' row entries per file')
  33  FORMAT('Stream loss will be calculated as a factor ',
     +                 'of the streambed hydraulic conductivity. ',
     +                 'Multiplication factor is equal to ',E20.10)
!
      lloc = 1
      CALL URWORD(line, lloc, istart, istop, 2, NSTRM, r, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 2, NSS, r, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 2, NSFRPAR, r, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 2, nparseg, r, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 3, i, CONST, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 3, i, DLEAK, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 2, ISTCB1, r, IOUT, In)
      CALL URWORD(line, lloc, istart, istop, 2, ISTCB2, r, IOUT, In)
      IF ( NSTRM.LT.0 ) THEN
!        WRITE(IOUT, 9036)
! 9036   FORMAT (//, 'NSTRM IS NEGATIVE AND THIS METHOD FOR ',
!     +          'SPECIFYING INFORMATION BY REACH HAS BEEN REPLACED BY ',
!     +          'THE KEYWORD OPTION "REACHINPUT"--PROGRAM STOPPING ',/)
!        CALL USTOP(' ')
        IRFG = 1
        NSTRM = ABS(NSTRM)
      END IF
C
C3------READ ISFROPT FLAGS WHEN NSTRM IS LESS THAN ZERO.
      IF ( IRFG.GT.0 ) THEN   !RGN not needed now that keywords are used.
        IF ( NSFRPAR.GT.0 ) THEN
          WRITE(IOUT, 9002)
 9002  FORMAT (//, 'KEYWORD "REACHINPUT" IS SPECIFIED AND NSFRPAR IS ',
     +  'GREATER THAN  ZERO ', /1X , ' ALTERNATE SFR7 OPTIONS DO NOT ',
     +  'SUPPORT PARAMETERS--PROGRAM STOPPING ',/)
          CALL USTOP(' ')
        END IF
        NSTRM = ABS(NSTRM)
        CALL URWORD(line, lloc, istart, istop, 2, ISFROPT, r, IOUT, In)
C
C4------READ UNSATURATED FLOW VARIABLES WHEN ISFROPT GREATER THAN 1.
        IF ( ISFROPT.GE.2 ) THEN
          IUZT = 1
          CALL URWORD(line, lloc, istart, istop, 2, NSTRAIL, r, IOUT, 
     +                In)
          CALL URWORD(line, lloc, istart, istop, 2, ISUZN, r, IOUT, In)
          CALL URWORD(line, lloc, istart, istop, 2, NSFRSETS, r, IOUT, 
     +                In)
        END IF
      END IF
!4b-----Data read for transient routing.
        IF ( ITRFLG.EQ.1 ) THEN
          CALL URWORD(line, lloc, istart, istop, 2, IRTFLG, r, IOUT, In)
          IF ( IRTFLG .GT. 0 ) THEN
            NUMTIM = 1
            WEIGHT = 1.0
            FLWTOL = 1.0D-6
            CALL URWORD(line,lloc,istart,istop,2,NUMTIM,r,IOUT,In)
            CALL URWORD(line,lloc,istart,istop,3,i,WEIGHT,IOUT,In)
            CALL URWORD(line,lloc,istart,istop,3,i,FLWTOL,IOUT,In)
            IF ( NUMTIM.LT.1 ) NUMTIM = 1
            IF ( WEIGHT.LT.0.0 .OR. WEIGHT.GT.1.0 ) WEIGHT=1.0
            IF ( FLWTOL.LT.1.0e-6 ) FLWTOL=1.0e-6
          ELSE
            NUMTIM = 1
            WEIGHT = 1.0
            FLWTOL = 1.0e-6
          END IF
        END IF
      IF ( NSS.LT.0 ) NSS = 0
      IF ( NSFRPAR.LE.0 ) THEN
        NSFRPAR = 0
        nparseg = 0
      END IF
      IF ( nparseg.LT.0 ) nparseg = 0
! RGN if ISFROPT=2 or 4 then you cannot use BCF Package.
      IF ( Iunitbcf.GT.0 ) THEN
        IF ( ISFROPT.EQ.2 .OR. ISFROPT.EQ.4 ) THEN
          WRITE(IOUT, 9045)
 9045  FORMAT (//, 'If the BCF Package is used and unsaturated ',
     +  'flow is active then ISFROPT must equal 3 or 5. ',
     +  '--PROGRAM STOPPING ',/)
          CALL USTOP(' ')
        END IF
      END IF
      nssar = 1
      IF (NSS.GT.0) nssar = NSS
      nstrmar = 1
      IF (NSTRM.GT.0) nstrmar = NSTRM
! RGN put NSEGDIM into module for FMP 10/15/11
      NSEGDIM = NSS + nparseg
      IF (nsegdim.LT.1) nsegdim = 1
!IFACE
!4c-----Look for IFACE Flag
      iface=0
      CALL URWORD(line,lloc,istart,istop,1,i,r,IOUT,In)
      IF(line(istart:istop).EQ.'IFACE') THEN
        iface=1
        WRITE ( IOUT, '(1X,A)')
     +    'IFACE values will be read from reach records'
      END IF
C
C5------CALCULATE SPACE NEEDED FOR TABULATED DISCHARGE VERSUS FLOW
C         AND WIDTH RELATIONS.
      MAXPTS = 3*50
C
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR STREAMS
C     ******************************************************************
C
Cdep  changed DSTROT to FXLKOT
      ALLOCATE (STRIN(nssar), STROUT(nssar), FXLKOT(nssar))
      STRIN = 0.0
      STROUT = 0.0
      FXLKOT = 0.0
!IFACE -- 6th element of ISTRM is for IFACE
      ALLOCATE (STRM(31,nstrmar), ISTRM(6,nstrmar))  !EDM - new index for XSA for LMT
      ALLOCATE (HSTRM(nstrmar,NUMTIM), HWDTH(nstrmar,NUMTIM))
      ALLOCATE (QSTRM(nstrmar,NUMTIM))
      ALLOCATE (HWTPRM(nstrmar,NUMTIM))
!      ALLOCATE (DVRCH(nstrmar),DVEFF(nstrmar))        !cjm
!      ALLOCATE (DVRCELL(NCOL*NROW,2,nss))  !cjm
!      ALLOCATE (DVRPERC(NCOL,NROW))   !cjm
      ALLOCATE (RECHSAVE(NCOL,NROW))
      ALLOCATE (FNETSEEP(NCOL,NROW)) !rgn printing net recharge in UZF
      STRM = 0.0  
      HSTRM = 0.0
      QSTRM = 0.0
      HWDTH = 0.0
      HWTPRM = 0.0
      ISTRM = 0
!      DVRCH = 0       !cjm
!      DVEFF = 0.0     !cjm
!      DVRCELL = 0     !cjm
       RECHSAVE = 0.0
!      DVRPERC = 0.0   !cjm
      FNETSEEP = 0.0  !rgn
!changed to seg(27,nsegdim) to store GW flow to streams by segment.
      ALLOCATE (SEG(27,nsegdim), ISEG(4,nsegdim), IDIVAR(2,nsegdim))  
!      ALLOCATE (IDVFLG)
!      IDVFLG = 0
Cdep  allocate space for stream outflow derivatives for lake package
      ALLOCATE (DLKOTFLW(200,nssar), SLKOTFLW(200,nssar))
      ALLOCATE (DLKSTAGE(200,nssar))
      IF ( NUMTAB.GT.0 ) THEN
        ALLOCATE (TABFLOW(MAXVAL,nssar+NUMTAB+1), 
     +            TABTIME(MAXVAL,nssar+NUMTAB+1))
      ELSE
        ALLOCATE (TABFLOW(1,1), TABTIME(1,1))
      END IF 
      ALLOCATE (ISFRLIST(3,nssar+NUMTAB))
      TABFLOW = 0.0
      TABTIME = 0.0
      ISFRLIST = 0
      SEG = 0.0
      ISEG = 0
      IDIVAR = 0
      DLKOTFLW = 0.0D0
      DLKSTAGE = 0.0D0
      SLKOTFLW = 0.0D0
      ALLOCATE (IOTSG(nsegdim))
      IOTSG = 0
      ALLOCATE (SFRQ(5,nstrmar))
      SFRQ = 0.0
      IF ( Iunitgwt.GT.0 ) THEN
        ALLOCATE (CONCQ(nsegdim,Nsol), CONCRUN(nsegdim,Nsol))
        ALLOCATE (CONCPPT(nsegdim,Nsol))
      ELSE
        ALLOCATE (CONCQ(1,Nsol), CONCRUN(1,Nsol), CONCPPT(1,Nsol))
      END IF
C
C6------PRINT INFORMATION THAT WAS READ.
      WRITE (IOUT, 9003) NSTRM, NSS, NSFRPAR, nparseg, DLEAK, CONST
      IF ( ISFROPT.EQ.1 ) WRITE (IOUT, 9004)
      IF ( ISFROPT.GE.2 ) WRITE (IOUT, 9005)
      IF ( ISTCB1.GT.0 ) WRITE (IOUT, 9006) ISTCB1
      IF ( ISTCB2.GT.0 ) WRITE (IOUT, 9007) ISTCB2
      IF ( IRTFLG.GT.0 ) WRITE (IOUT, 9035) NUMTIM,WEIGHT,FLWTOL
 9003 FORMAT (//, ' NUMBER OF STREAM REACHES IS', I5, //, 
     +        ' NUMBER OF STREAM SEGMENTS IS', I5, //, 
     +        ' NUMBER OF STREAM PARAMETERS IS', I5, //, 
     +        ' NUMBER OF STREAM SEGMENTS DEFINED USING PARAMETERS IS',
     +        I15, //, ' MAXIMUM ERROR FOR STREAM LEAKAGE RATES IS', 
     +        1PE10.2, //, ' CONSTANT FOR MANNINGS EQUATION IS', E12.4,
     +        ///)
 9004 FORMAT (//, ' USING DATA INPUT MODIFIED FROM ORIGINAL SFR ',
     +        'PROGRAM FOR FARM PACKAGE', /)
 9005 FORMAT (//, ' OPTION FOR UNSATURATED FLOW BENEATH STREAMBEDS IS ',
     +        'ACTIVE ', //)
 9006 FORMAT (' FLOW TO AND FROM GROUND WATER FOR EACH STREAM REACH ',
     +        'WILL BE SAVED ON UNIT', I5)
 9007 FORMAT (' STREAM OUTPUT WILL BE WRITTEN TO FILE ON UNIT', I5)
 9035 FORMAT (//, ' TRANSIENT STREAMFLOW ROUTING IS ACTIVE', //,
     +        ' NUMBER OF SUB TIME STEPS IS ', I5, //,
     +        ' WEIGHTING FACTOR FOR CHANNEL STORAGE IS ', E12.4, //,
     +        ' STREAMFLOW TOLERANCE IS ', E12.4,
     +        ///)
C
C7------CHECK FOR ERRORS.
c      IF ( NSTRM.LE.0 .OR. NSS.LE.0 ) THEN
c        WRITE (IOUT, 9008)
c        In = 0
c        NSS = 0
c        NSTRM = 0
c        RETURN
c      END IF
      IF ( NSFRPAR.GT.0 .AND. nparseg.LE.0 ) THEN
        WRITE (IOUT, 9009)
        In = 0
        NSS = 0
        NSTRM = 0
        RETURN
      END IF
      IF ( IUZT.EQ.1 ) THEN
        IF ( NSTRAIL.LT.0 ) THEN
          WRITE (IOUT, 9010)
          NSTRAIL = ABS(NSTRAIL)
        END IF
        IF ( NSTRAIL.EQ.0 ) THEN
          WRITE (IOUT, 9011)
          IUZT = 0
        END IF
      END IF
      IF ( DLEAK.LE.0.0 ) THEN
        DLEAK = 0.00001
        WRITE (IOUT, 9012)
      END IF
 9008 FORMAT (//, ' NO STREAM REACHES (NSTRM) AND/OR SEGMENTS (NSS)--',
     +        //, ' SFR PACKAGE BEING TURNED OFF'///)
 9009 FORMAT (//, ' NO STREAM SEGMENTS DEFINED BY PARAMETERS--', 
     +        'NSFRPAR GT ZERO AND NPARSEG LE ZERO', //, 
     +        ' SFR PACKAGE BEING TURNED OFF'///)
 9010 FORMAT (//, ' NUMBER OF TRAILING WAVES IS LESS THAN ZERO', 
     +        '--SETTING VALUE TO A POSITIVE VALUE'///)
 9011 FORMAT (//, ' VERTICAL FLOW THROUGH UNSATURATED ZONE IS ', 
     +        'ACTIVE AND NUMBER OF TRAILING WAVES IS ZERO-- ', 
     +        ' RESETTING UNSATURATED FLOW TO BE INACTIVE '///)
 9012 FORMAT (//, ' *** WARNING ***   DLEAK IS LESS THAN OR EQUAL', 
     +        ' TO ZERO --- DLEAK ASSIGNED A VALUE OF 0.0001', ///)
C
      IF ( IUZT.EQ.1 ) THEN
C
C8------ALLOCATE SPACE FOR UNSATURATED FLOW.
        NUZST = NSTRM
        NSTOTRL = ISUZN*NSTRAIL*NSFRSETS
        NUMAVE = 21
        mstrmar = nstrmar
      ELSE
C
C9------ALLOCATE ONLY ONE ARRAY ELEMENT IF UNSATURATED FLOW IS INACTIVE.
        NUZST = 1
        NSTOTRL = 1
        NUMAVE = 1
        ISUZN = 1
        NSTRAIL = 1
        NSFRSETS = 1
        mstrmar = 1
      END IF
C
C ALLOCATE AND INITIALIZE ARRAYS
C
      ALLOCATE (THTS(NUZST), THTR(NUZST), THTI(NUZST), EPS(NUZST))
      THTS = 0.0D0
      THTR = 0.0D0
      THTI = 0.0D0
      EPS = 0.0D0
      ALLOCATE (UHC(NUZST))
      UHC = 0.0
      ALLOCATE (XSEC(16, nsegdim), QSTAGE(MAXPTS,nsegdim)) 
      XSEC = 0.0
      QSTAGE = 0.0
      ALLOCATE (NSEGCK(nssar), SGOTFLW(nssar), DVRSFLW(nssar))
      NSEGCK = 0
      SGOTFLW = 0.0
      DVRSFLW = 0.0
      ALLOCATE (SFRUZBD(10))
      SFRUZBD = 0.0
C
C10-----READ AND PRINT DATA FOR EACH STREAM REACH. 
!IFACE
      ndash=50
      face=' '
      if(iface.eq.1) then
        face='   IFACE'
        ndash=ndash+8
      end if
      IF ( ISFROPT.EQ.0 ) THEN
        WRITE (IOUT, 9013) face
      ELSE IF ( ISFROPT.EQ.1 ) THEN
        ndash=ndash+55
        WRITE (IOUT, 9014) face
      ELSE IF ( ISFROPT.EQ.2 ) THEN
        ndash=ndash+90
        WRITE (IOUT, 9015) face
      ELSE IF ( ISFROPT.EQ.3 ) THEN
        ndash=ndash+101
        WRITE (IOUT, 9016) face
      ELSE IF ( ISFROPT.EQ.4 ) THEN
        WRITE (IOUT, 9013) face
      ELSE IF ( ISFROPT.EQ.5 ) THEN
        WRITE (IOUT, 9013) face
      END IF
!IFACE
      WRITE (IOUT,'(3X,200A)') ('-',i=1,ndash)
 9013 FORMAT (1X, //, 3X, 'STREAM NETWORK DESCRIPTION: ', //, 3X,
     +        'LAYER    ROW    COL   SEGMENT   REACH     LENGTH', /,
     +        26X, 'NUMBER   NUMBER    IN CELL',A)
 9014 FORMAT (1X, //, 3X, 'STREAM NETWORK DESCRIPTION: ', //, 3X,
     +        'LAYER    ROW    COL   SEGMENT   REACH     LENGTH',
     +        '     STREAMBED     STREAMBED   STREAMBED     STREAMBED',
     +        /, 26X, 'NUMBER   NUMBER    IN CELL    TOP ELEV.    ', 
     +        '   SLOPE     THICKNESS', '   HYDR. CONDUCT.',A)
 9015 FORMAT (1X, //, 3X, 'STREAM NETWORK DESCRIPTION: ', //, 3X, 
     +        'LAYER  ROW   COL SEGMENT  REACH    LENGTH', 
     +        '    STREAMBED   STREAMBED   STREAMBED   ', 
     +        'STREAMBED   SATURATED   INITIAL', 
     +        '    RESIDUAL    BROOKS/COREY', /, 20X, 
     +        'NUMBER  NUMBER   IN CELL', 
     +        '    TOP ELEV.     SLOPE     THICKNESS   ', 
     +        'HYD. COND.  WAT.CONT.  WAT.CONT.   ', 
     +        'WAT.CONT.     EPSILON',A)
 9016 FORMAT (1X, //, 3X, 'STREAM NETWORK DESCRIPTION: ', //, 3X, 
     +        'LAYER  ROW   COL SEGMENT  REACH   LENGTH     ', 
     +        'STREAMBED   STREAMBED   STREAMBED  STREAMBED', 
     +        '    SATURATED    INITIAL', 
     +        '    RESIDUAL  BROOKS/COREY  SAT. VERT.', /, 20X, 
     +        'NUMBER  NUMBER  IN CELL     ', 
     +        'TOP ELEV.     SLOPE     THICKNESS  ', 
     +        'HYD. COND.   WAT.CONT.   WAT.CONT.   ', 
     +        'WAT.CONT.    EPSILON    HYD. COND.',A)
C
C11-----READ AND WRITE DATA FOR EACH REACH ON BASIS OF ISFROPT.
      nseg = 0
      nreach = 0
!      IF ( Iunithuf.GT.0 ) THEN
!        IF ( ISFROPT.NE.3 .AND. ISFROPT.NE.5 ) THEN
!          WRITE (IOUT, 9034)
! 9034     FORMAT (//, ' ***ERROR***  HUF PACKAGE IS ACTIVE ', 
!     +          'AND ISFROPT NOT 3 or 5 ',/, 
!     +          ' PROGRAM IS STOPPING')
!          CALL USTOP(' ')
!        END IF
!      END IF
     
      DO ii = 1, NSTRM
        IF ( ISFROPT.EQ.0 ) THEN
!IFACE
          IF(iface.eq.1) THEN
            READ (In, *) krch, irch, jrch, jseg, ireach, STRM(1, ii),
     +                 ISTRM(6, ii)
          ELSE
            READ (In, *) krch, irch, jrch, jseg, ireach, STRM(1, ii)
          END IF
        ELSE IF ( ISFROPT.EQ.1 ) THEN
!IFACE
          IF (iface.eq.1) THEN
            READ (In, *) krch, irch, jrch, jseg, ireach, STRM(1, ii), 
     +                 STRM(3, ii), STRM(2, ii), STRM(8, ii), 
     +                 STRM(6, ii), ISTRM(6, ii)
          ELSE
            READ (In, *) krch, irch, jrch, jseg, ireach, STRM(1, ii), 
     +                 STRM(3, ii), STRM(2, ii), STRM(8, ii), 
     +                 STRM(6, ii)
          END IF
          STRM(4, ii) = STRM(3, ii) - STRM(8, ii)
          IF ( STRM(2, ii).LE.0.0 ) THEN
            WRITE (IOUT, 9017) jseg, ireach
            CALL USTOP(' ')
          END IF
        ELSE IF ( ISFROPT.EQ.2 ) THEN
!IFACE
          IF (iface.eq.1) THEN
            READ (In, *) krch, irch, jrch, jseg, ireach, STRM(1, ii), 
     +                 STRM(3, ii), STRM(2, ii), STRM(8, ii), 
     +                 STRM(6, ii), THTS(ii), THTI(ii), EPS(ii),
     +                 ISTRM(6, ii)
          ELSE
            READ (In, *) krch, irch, jrch, jseg, ireach, STRM(1, ii), 
     +                 STRM(3, ii), STRM(2, ii), STRM(8, ii), 
     +                 STRM(6, ii), THTS(ii), THTI(ii), EPS(ii)
          END IF
          STRM(4, ii) = STRM(3, ii) - STRM(8, ii)
          IF ( STRM(2, ii).LE.0.0 ) THEN
            WRITE (IOUT, 9017) jseg, ireach
            CALL USTOP(' ')
          END IF
        ELSE IF ( ISFROPT.EQ.3 ) THEN
!IFACE
          IF (iface.eq.1) THEN
            READ (In, *) krch, irch, jrch, jseg, ireach, STRM(1, ii), 
     +                 STRM(3, ii), STRM(2, ii), STRM(8, ii), 
     +                 STRM(6, ii), THTS(ii), THTI(ii), EPS(ii),
     +                 UHC(ii), ISTRM(6, ii)
          ELSE
            READ (In, *) krch, irch, jrch, jseg, ireach, STRM(1, ii), 
     +                 STRM(3, ii), STRM(2, ii), STRM(8, ii), 
     +                 STRM(6, ii), THTS(ii), THTI(ii), EPS(ii), UHC(ii)
          END IF
          STRM(4, ii) = STRM(3, ii) - STRM(8, ii)
          IF ( STRM(2, ii).LE.0.0 ) THEN
            WRITE (IOUT, 9017) jseg, ireach
            CALL USTOP(' ')
          END IF
        ELSE IF ( ISFROPT.EQ.4 .OR. ISFROPT.EQ.5 ) THEN
!IFACE
          IF (iface.eq.1) THEN
            READ (In, *) krch, irch, jrch, jseg, ireach, STRM(1, ii),
     +                 ISTRM(6, ii)
          ELSE
            READ (In, *) krch, irch, jrch, jseg, ireach, STRM(1, ii)
          END IF
        END IF
        IF ( IUZT.EQ.1 ) THEN
          IF ( IBOUND(jrch, irch, krch).LE.0 )
     +      WRITE (IOUT, 9018) ireach, jseg
        END IF
 9017   FORMAT (//, ' ***ERROR***  SLOPE IS SPECIFIED LESS THAN OR ', 
     +          'EQUAL TO ZERO FOR SEGMENT', I8, ' REACH', I8, /, 
     +          ' PROGRAM IS STOPPING')
 9018   FORMAT (5X, '**WARNING** CELL BENEATH STREAM REACH IS INACTIVE',
     +          /, 5X, 'CELL BELOW MUST HAVE THE SAME SPECIFIC YIELD',
     +          /, 5X, 'AND THE TOP ELEVATION OF ACTIVE CELL MUST ', 
     +          'EQUAL BOTTOM OF INACTIVE CELL', /, 5X, 
     +          'INACTIVE CELL IS BELOW STREAM REACH AND SEGMENT:', 2I5)
C
C12-----CALCULATE RESIDUAL WATER CONTENT FROM SATURATED WATER CONTENT 
C        AND SPECIFIC YIELD WHEN UNSATURATED FLOW IS ACTIVE.
        IF ( ABS(ITRSS).EQ.1 ) THEN
          IF ( ISFROPT.EQ.2 .OR. ISFROPT.EQ.3 ) THEN
            IF ( Iunitlpf.GT.0 ) THEN
              THTR(ii) = THTS(ii) - SC2LPF(jrch, irch, krch)
     +                   /(DELR(jrch)*DELC(irch))
            ELSE IF ( Iunitbcf.GT.0 ) THEN
              IF ( LAYCON(krch).LT.2 ) THEN
                THTR(ii) = THTS(ii) - SC1(jrch, irch, krch)
     +                   /(DELR(jrch)*DELC(irch))
              ELSE
                kkrch = 0
                DO k = 1, krch
                IF(LAYCON(K).EQ.3 .OR. LAYCON(K).EQ.2)kkrch = kkrch + 1
                END DO
                THTR(ii) = THTS(ii) - SC2(jrch, irch, kkrch)
     +                   /(DELR(jrch)*DELC(irch))
              END IF
            ELSE IF ( Iunithuf.GT.0 ) THEN
              THTR(ii) = THTS(ii) - SC2HUF(jrch, irch)
            END IF
          END IF
        ELSEIF ( ISFROPT.EQ.2 .OR. ISFROPT.EQ.3 ) THEN
          THTR(ii) = 0.0
        END IF
        IF ( ISFROPT.EQ.0 .AND. IFLG.EQ.0 ) THEN
!IFACE
          IF (iface.eq.1) THEN
            WRITE (IOUT, 9019) krch, irch, jrch, jseg, ireach,
     +                       STRM(1, ii), ISTRM(6,ii)
          ELSE
            WRITE (IOUT, 9019) krch, irch, jrch, jseg, ireach,
     +                       STRM(1, ii)
          END IF
        ELSE IF ( ISFROPT.EQ.1 .AND. IFLG.EQ.0 ) THEN
          IF (iface.eq.1) THEN
            WRITE (IOUT, 9020) krch, irch, jrch, jseg, ireach,
     +                       STRM(1, ii), STRM(3, ii), STRM(2, ii),
     +                       STRM(8, ii), STRM(6, ii), ISTRM(6,ii)
          ELSE
            WRITE (IOUT, 9020) krch, irch, jrch, jseg, ireach,
     +                       STRM(1, ii), STRM(3, ii), STRM(2, ii),
     +                       STRM(8, ii), STRM(6, ii)
          END IF
        ELSE IF ( ISFROPT.EQ.2 .AND. IFLG.EQ.0 ) THEN
          IF (iface.eq.1) THEN
            WRITE (IOUT, 9021) krch, irch, jrch, jseg, ireach,
     +                       STRM(1, ii), STRM(3, ii), STRM(2, ii),
     +                       STRM(8, ii), STRM(6, ii), THTS(ii),
     +                       THTI(ii), THTR(ii), EPS(ii), ISTRM(6,ii)
          ELSE
            WRITE (IOUT, 9021) krch, irch, jrch, jseg, ireach,
     +                       STRM(1, ii), STRM(3, ii), STRM(2, ii),
     +                       STRM(8, ii), STRM(6, ii), THTS(ii),
     +                       THTI(ii), THTR(ii), EPS(ii)
          END IF
        ELSE IF ( ISFROPT.EQ.3 .AND. IFLG.EQ.0 ) THEN
          IF (iface.eq.1) THEN
            WRITE (IOUT, 9022) krch, irch, jrch, jseg, ireach,
     +                       STRM(1, ii), STRM(3, ii), STRM(2, ii),
     +                       STRM(8, ii), STRM(6, ii), THTS(ii),
     +                       THTI(ii), THTR(ii), EPS(ii), UHC(ii),
     +                       ISTRM(6,ii)
          ELSE
            WRITE (IOUT, 9022) krch, irch, jrch, jseg, ireach,
     +                       STRM(1, ii), STRM(3, ii), STRM(2, ii),
     +                       STRM(8, ii), STRM(6, ii), THTS(ii),
     +                       THTI(ii), THTR(ii), EPS(ii), UHC(ii)
          END IF
        ELSE IF ( ISFROPT.EQ.4 .OR. ISFROPT.EQ.5 .AND. IFLG.EQ.0 ) THEN
          IF (iface.eq.1) THEN
            WRITE (IOUT, 9019) krch, irch, jrch, jseg, ireach,
     +                       STRM(1, ii), ISTRM(6,ii)
          ELSE
            WRITE (IOUT, 9019) krch, irch, jrch, jseg, ireach,
     +                       STRM(1, ii)
          END IF
        END IF
 9019   FORMAT (2X, I5, 2I7, I8, I9, 3X, 1PE11.4,I8)
 9020   FORMAT (2X, I6, 2I7, I8, I9, 3X, 1PE11.4, 2X, 1PE11.4, 2X, 
     +          1PE11.4, 2X, 1PE11.4, 2X, 1PE11.4,I8)
 9021   FORMAT (3(1X, I5), 1X, I5, 3X, I5, 1X, 5(1X, 1PE11.4), 
     +          3(1X, 0PE11.4), 1(1X, 1PE11.4),I8)
 9022   FORMAT (3(1X, I5), 1X, I5, 3X, I5, 1X, 5(1X, 1PE11.4), 
     +          3(1X, 0PE11.4), 2(1X, 1PE11.4),I8)
C
C13-----CHECK RANGE AND ORDER FOR SEGMENTS AND REACHES.
        IF ( jseg.LE.0 .OR. jseg.GT.NSS ) THEN
          WRITE (IOUT, 9023)
          CALL USTOP(' ')
        END IF
        IF ( jseg.NE.nseg ) THEN
          nseg = nseg + 1
          nreach = 0
          IF ( jseg.NE.nseg ) THEN
            WRITE (IOUT, 9024)
            CALL USTOP(' ')
          END IF
        END IF
        nreach = nreach + 1
        IF ( ireach.NE.nreach ) THEN
          WRITE (IOUT, 9025)
          CALL USTOP(' ')
        END IF
 9023   FORMAT (' SEGMENT MUST BE GREATER THAN 0 AND LESS THAN NSS')
 9024   FORMAT (' SEGMENTS MUST BE IN ORDER FROM 1 THROUGH NSS')
 9025   FORMAT (' EACH SEGMENT MUST START WITH REACH 1, AND', /, 
     +          ' REACHES MUST BE NUMBERED CONSECUTIVELY')
        ISTRM(1, ii) = krch
        ISTRM(2, ii) = irch
        ISTRM(3, ii) = jrch
        ISTRM(4, ii) = jseg
        ISTRM(5, ii) = ireach
        SEG(1, ISTRM(4, ii)) = SEG(1, ISTRM(4, ii)) + STRM(1, ii)
C       Number of reaches in segment added to ISEG
        ISEG(4, jseg) = ireach
        IF ( ISFROPT.LT.4.AND.ISFROPT.GT.0 )THEN  
          uzfar_check%ltype = LAYHDT(krch)
          uzfar_check%irchnum = ii
          uzfar_check%iflag = IFLG
          uzfar_check%iunit = IOUT
          IERR = ICHKSTRBOT(uzfar_check)
          IF ( IERR.GT.0 ) IFLG = IERR
        END IF
      END DO
C
C14-----READ SEGMENT INFORMATION FOR FIRST STRESS PERIOD.
      IF ( NSFRPAR.EQ.0 ) THEN
        READ (In, *) ITMP, IRDFLG, IPTFLG
        NP = 0
        nlst = NSS
        lb = 1
        ichk = 1
        CALL SGWF2SFR7RDSEG(nlst, lb, In, Iunitgwt, Iunituzf, NSEGCK,
     +                      NSS, ichk, 1, Nsol)
      END IF
C
C15-----COMPUTE UNSATURATED VARIABLE WHEN SPECIFIED BY SEGMENT.
      IF ( IUZT.EQ.1 ) THEN
        irch = 1
        ksfropt = 0
        DO nseg = 1, NSS
          icalc = ISEG(1, nseg)
          seglen = SEG(1, nseg)
          sumlen = 0.0
          IF ( icalc.EQ.1 .OR. icalc.EQ.2 ) THEN
            IF ( ISFROPT.EQ.4 .OR. ISFROPT.EQ.5 ) THEN
              ksfropt = 1
              thsslpe = (SEG(18, nseg)-SEG(22, nseg))/seglen
              thislpe = (SEG(19, nseg)-SEG(23, nseg))/seglen
              epsslpe = (SEG(20, nseg)-SEG(24, nseg))/seglen
              IF ( ISFROPT.EQ.5 )
     +             uhcslpe = (SEG(21, nseg)-SEG(25, nseg))/seglen
            END IF
          END IF
          DO ii = 1, ISEG(4, nseg)
            IF ( icalc.EQ.1 .OR. icalc.EQ.2 ) THEN
              krck = ISTRM(1, irch)
              irck = ISTRM(2, irch)
              jrck = ISTRM(3, irch)
              rchlen = STRM(1, irch)
              dist = sumlen + (0.5*rchlen)
              IF ( ksfropt.EQ.1 ) THEN
                THTS(irch) = SEG(18, nseg) - (thsslpe*dist)
                THTI(irch) = SEG(19, nseg) - (thislpe*dist)
                EPS(irch) = SEG(20, nseg) - (epsslpe*dist)
                IF ( ISFROPT.EQ.5 ) UHC(irch) = SEG(21, nseg)
     +               - (uhcslpe*dist)
              END IF
C
C16-----CALCULATE RESIDUAL WATER CONTENT FROM SATURATED WATER CONTENT
C         AND SPECIFIC YIELD WHEN UNSATURATED FLOW IS ACTIVE.
! RGN 5/8/09 Fixed calculation of THTR to include HUF
              IF ( ITRSS.EQ.1 ) THEN
                IF ( ISFROPT.EQ.4 .OR. ISFROPT.EQ.5 ) THEN
                  IF ( Iunitlpf.GT.0 ) THEN
                    THTR(irch) = THTS(irch) - SC2LPF(jrck, irck, krck)
     +                         /(DELR(jrck)*DELC(irck))
                  ELSE IF ( Iunitbcf.GT.0 ) THEN
                    IF( LAYCON(krck).EQ.0 ) THEN
                      THTR(irch) = THTS(irch) - SC1(jrck, irck, krck)
     +                         /(DELR(jrck)*DELC(irck))
                    ELSE
                      kkrch = 0
                      DO k = 1, krch
                        IF(LAYCON(K).EQ.3 .OR. LAYCON(K).EQ.2)
     +                   kkrch = kkrch + 1
                      END DO
                      THTR(irch) = THTS(irch) - SC2(jrck, irck, kkrch)
     +                         /(DELR(jrck)*DELC(irck))
                    END IF
                  ELSE IF( Iunithuf.GT.0 ) THEN
                    THTR(irch) = THTS(irch) - SC2HUF(jrck, irck)
                  END IF
                END IF
              ELSEIF ( ISFROPT.EQ.4 .OR. ISFROPT.EQ.5 ) THEN
                THTR(irch) = 0.0
              END IF
C
C17-----CHECK THAT RESIDUAL WATER CONTENT IS LESS THAN
C         SATURATED WATER CONTENT.
              IF ( IUZT.EQ.1 ) THEN
                IF ( THTR(irch).GE.THTS(irch) ) THEN
                  WRITE (IOUT, 9026)
                  CALL USTOP(' ')
                END IF
                IF ( THTI(irch).GT.THTS(irch) ) THEN
                  WRITE (IOUT, 9027)
                  CALL USTOP(' ')
                END IF
Cdep  Added check that THTI is greater than THTR.
                IF ( THTI(irch).LT.THTR(irch) ) THEN
                  WRITE (IOUT, 9028)ISTRM(4,irch), ISTRM(5,irch),
     +                              THTR(irch)
                  THTI(irch) = THTR(irch)
                END IF 
              END IF
              sumlen = sumlen + rchlen
            END IF
            irch = irch + 1
          END DO
        END DO
      END IF
 9026 FORMAT (' RESIDUAL WATER CONTENT IS EQUAL OR GREATER THAN ', 
     +        'SATURATED WATER CONTENT. CHECK INPUT DATA FOR SPECIFIC', 
     +        ' YIELD AND SATURATED WATER CONTENT')
 9027 FORMAT (' INITIAL WATER CONTENT IS GREATER THAN SATURATED ', 
     +        'WATER CONTENT. CHECK INPUT DATA')
 9028 FORMAT (' INITIAL WATER CONTENT IS LESS THAN RESIDUAL ',
     +        'WATER CONTENT FOR STREAM SEGMENT: ',I5,' REACH: ',I5,
     +        ' INITIAL WATER CONTENT RESET TO RESIDUAL OF ',E12.5) 
C
C18-----CHECK IF STREAM REACH IS IN ACTIVE CELL.
      kkptflg = 0
      DO ichk = 1, NSTRM
        krck = ISTRM(1, ichk)
        irck = ISTRM(2, ichk)
        jrck = ISTRM(3, ichk)
        jsegck = ISTRM(4, ichk)
        ireachck = ISTRM(5, ichk)
        IF ( IBOUND(jrck, irck, krck).EQ.0 ) THEN
          kkptflg = kkptflg + 1
          IF ( kkptflg.EQ.1 ) WRITE (IOUT, 9029) jsegck, ireachck, 
     +                              IBOUND(jrck, irck, krck), krck,
     +                              irck, jrck
        ELSE IF ( IBOUND(jrck, irck, krck).LT.0 ) THEN
          WRITE (IOUT, 9030) jsegck, ireachck, IBOUND(jrck, irck, krck),
     +                       krck, irck, jrck
        END IF
      END DO
      IF ( kkptflg.EQ.1 ) THEN
        WRITE (IOUT, 9031)
      ELSE IF ( kkptflg.GT.1 ) THEN
        WRITE (IOUT, 9032) kkptflg
      END IF
C
 9029 FORMAT (/, ' *** WARNING *** FIRST OCCURRENCE WHERE A ', 
     +        'STREAM REACH IS ASSIGNED TO AN INACTIVE CELL IS SEGMENT',
     +        I5, ' REACH NO.', I5, /, '  IBOUND ARRAY VALUE IS', I5,
     +        ' AT LAYER', I5, '; ROW', I5, '; COLUMN', I5, '.')
 9030 FORMAT (/, ' *** WARNING *** STREAM SEGMENT', I5, ' REACH NO.',
     +        I5, ' IS CONNECTED TO A CONSTANT HEAD CELL.'/,
     +        ' IBOUND ARRAY VALUE IS', I5, ' AT ', 'LAYER', I5,
     +        '; ROW', I5, '; COLUMN', I5, '.', /,
     +        ' NO STREAM LEAKAGE WILL BE ALLOWED-- SUGGEST ', 
     +        'REMOVING STREAM REACH FROM CELL OR CHANGE CELL ', 
     +        'TO VARIABLE HEAD.', /)
 9031 FORMAT (/, ' *** WARNING *** ONLY 1 STREAM REACH WAS ', 
     +        'ASSIGNED TO A CELL WHERE THE IBOUND ARRAY WAS ZERO.', /,
     +        ' PROGRAM SEARCHES FOR UPPERMOST ACTIVE CELL IN VERTICAL',
     +        ' COLUMN,IF ALL CELLS ARE INACTIVE, STREAM LEAKAGE WILL',
     +        ' NOT BE ALLOWED. ', /)
 9032 FORMAT (/, ' *** WARNING *** A TOTAL OF', I6, 'STREAM REACHES ', 
     +        'WERE ASSIGNED TO CELLS WHERE THE IBOUND ARRAY WAS ZERO.',
     +        /, ' PROGRAM SEARCHES FOR UPPERMOST ACTIVE CELL IN',
     +        ' VERTICAL COLUMN FOR ALL OCCURRENCES.', /,
     +        ' IF ALL CELLS IN A VERTICAL COLUMN ARE INACTIVE,',
     +        ' STREAM LEAKAGE WILL NOT BE ALLOWED FOR ASSOCIATED',
     +        ' STREAM REACH. ', /)
C
C19-----READ PARAMETER DEFINITIONS.
      IF ( NSFRPAR.GT.0 ) THEN
        lstsum = NSS + 1
        DO ii = 1, NSFRPAR
          lstbeg = lstsum
          CALL UPARLSTRP(lstsum, nsegdim, In, IOUT, ip, 'SFR', 'SFR', 
     +                   iterp, numinst)
          nlst = lstsum - lstbeg
          IF ( numinst.GT.1 ) nlst = nlst/numinst
C
C20-----ASSIGN STARTING INDEX FOR READING INSTANCES.
          IF ( numinst.EQ.0 ) THEN
            ib = 0
          ELSE
            ib = 1
          END IF
C
C21-----READ LIST(S) OF CELLS, PRECEDED BY INSTANCE NAME IF NUMINST>0.
Cdep  Revised to change ib loop counter
          lb = lstbeg
          DO i = ib, numinst
            IF ( i.GT.0 ) CALL UINSRP(i, In, IOUT, ip, iterp)
            ichk = 0
            CALL SGWF2SFR7RDSEG(nlst, lb, In, Iunitgwt, Iunituzf, idum, 
     +                          1, ichk, 1, Nsol)
            CALL SGWF2SFR7PRSEG(nlst, lb, Iunitgwt, 1, Nsol, Iouts)
            lb = lb + nlst
          END DO
        END DO
      END IF
C
      WRITE (IOUT, 9033)
 9033 FORMAT (//)
C
C22-----INITIALIZE VARIABLES AND LISTS FOR UNSATURATED FLOW BENEATH STREAM.
C         NWAVS INITIALLY SET TO 1.
      ALLOCATE (FOLDFLBT(mstrmar))
      Nfoldflbt = mstrmar
      FOLDFLBT = 0.0D0
      ALLOCATE (UZFLWT(ISUZN,NUZST), UZSTOR(ISUZN,NUZST))
      UZFLWT = 0.0D0
      UZSTOR = 0.0D0
      ALLOCATE (UZWDTH(ISUZN,NUZST), UZSEEP(ISUZN,NUZST))
      UZWDTH = 0.0D0
      UZSEEP = 0.0D0
      ALLOCATE (DELSTOR(ISUZN,NUZST), UZOLSFLX(ISUZN,NUZST))
      DELSTOR = 0.0D0
      UZOLSFLX = 0.0D0
      ALLOCATE (NWAVST(ISUZN,NUZST))
      NWAVST = 1
      ALLOCATE (UZDPIT(NSTOTRL,NUZST), UZDPST(NSTOTRL,NUZST))
      UZDPIT = 0.0D0
      UZDPST = 0.0D0
      ALLOCATE (UZTHIT(NSTOTRL,NUZST), UZTHST(NSTOTRL,NUZST))
      UZTHIT = 0.0D0
      UZTHST = 0.0D0
      ALLOCATE (UZSPIT(NSTOTRL,NUZST), UZSPST(NSTOTRL,NUZST))
      UZSPIT = 0.0D0
      UZSPST = 0.0D0
      ALLOCATE (UZFLIT(NSTOTRL,NUZST), UZFLST(NSTOTRL,NUZST))
      UZFLIT = 0.0D0
      UZFLST = 0.0D0
      ALLOCATE (LTRLIT(NSTOTRL,NUZST), LTRLST(NSTOTRL,NUZST))
      LTRLIT = 0
      LTRLST = 0
      ALLOCATE (ITRLIT(NSTOTRL,NUZST), ITRLST(NSTOTRL,NUZST))
      ITRLIT = 0
      ITRLST = 0
      ALLOCATE (ITRLSTH(NSTOTRL))
      ITRLSTH = 0
      ALLOCATE (WETPER(ISUZN,NUZST))
      WETPER = 0.0D0
      ALLOCATE (AVDPT(NUMAVE,NUZST), AVWAT(NUMAVE,NUZST))
      AVDPT = 0.0
      AVWAT = 0.0
      ALLOCATE (WAT1(NUMAVE,NUZST))
      WAT1 = 0.0
C
C22B-----INITIALIZE VARIABLES FOR STREAM DEPTH, LEAKAGE, AND 
C         PREVIOUS HEAD BENEATH STREAM.
      ALLOCATE (SUMLEAK(nstrmar))
      SUMLEAK = 0.0D0
      ALLOCATE (SUMRCH(nstrmar))
      SUMRCH = 0.0D0
      ALLOCATE (HLDSFR(nstrmar))
      HLDSFR = 0.0D0
C     ------------------------------------------------------------------
C
      IF ( Iunitlpf.GT.0 .OR. Iunithuf.GT.0 ) THEN 
        IF ( ISFROPT.EQ.2.OR.ISFROPT.EQ.4 )
     +    CALL SGWF2SFR7UHC(Iunitlpf, Iunithuf)
      END IF
C
C23-----SAVE POINTERS FOR GRID AND RETURN.
      CALL SGWF2SFR7PSV(Igrid)
      RETURN
      END SUBROUTINE GWF2SFR7AR
C
C-------SUBROUTINE SGWF2SFR7UHC
      SUBROUTINE SGWF2SFR7UHC(Iunitlpf, Iunithuf)
C     ******************************************************************
C     SETS UNSATURATED VERTICAL HYDRAULIC CONDUCTIVITY TO VERTICAL
C     HYDRAULIC CONDUCTIVITY IN THE LAYER-PROPERTY FLOW PACKAGE.
!--------REVISED FOR MODFLOW-2005 RELEASE 1.9, FEBRUARY 6, 2012
C     ******************************************************************
      USE GWFSFRMODULE, ONLY: NSTRM, ISTRM, UHC
      USE GLOBAL,       ONLY: IOUT, IBOUND, LAYHDT
      USE GWFLPFMODULE, ONLY: LAYVKA, VKA, HK
      USE GWFHUFMODULE, ONLY: HGUVANI, NHUF, HKHUF=>HK, VKAH
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
      INTEGER Iunitlpf, Iunithuf
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      INTEGER ichk, irck, jrck, krck
C     ------------------------------------------------------------------
C
C1------SET UHC EQUAL TO VKA IF STREAM IS IN ACTIVE CELL.
      DO ichk = 1, NSTRM
        krck = ISTRM(1, ichk)
        irck = ISTRM(2, ichk)
        jrck = ISTRM(3, ichk)
        IF ( Iunitlpf.GT.0 ) THEN
          IF ( IBOUND(jrck, irck, krck).GT.0 ) THEN
            IF ( LAYVKA(krck).EQ.0 ) THEN
              UHC(ichk) = VKA(jrck, irck, krck)
            ELSE
              UHC(ichk) = VKA(jrck, irck, krck)*
     +                    HK(jrck, irck, krck)
            END IF
            IF ( LAYHDT(krck).LE.0 ) THEN
            WRITE (IOUT, *) 'PROGRAM TERMINATED-LAYTYP MUST BE GREATER',
     +                      ' THAN ZERO WHEN ISFROPT IS 2 OR 4.'
            CALL USTOP(' ')
            END IF
          END IF
        ELSE
          IF ( IBOUND(jrck, irck, krck).GT.0 ) THEN
            IF ( HGUVANI(NHUF).LE.0.0 ) THEN
              UHC(ichk) = VKAH(jrck, irck, krck)
            ELSE
              UHC(ichk) = HGUVANI(NHUF)*
     +                              HKHUF(jrck, irck, krck)
            END IF
          END IF
        END IF 
      END DO
C
C2------RETURN.
      RETURN
      END SUBROUTINE SGWF2SFR7UHC
C
C-------SUBROUTINE GWF2SFR7RP
      SUBROUTINE GWF2SFR7RP(In, Iunitgwt, Iunitlak, Kkper, Kkstp, Nsol,
     +                      Iouts, Iunitbcf, Iunitlpf, Iunithuf, 
     +                      Iunituzf, Igrid)
C     ******************************************************************
C     READ STREAM DATA FOR STRESS PERIOD
!--------REVISED FOR MODFLOW-2005 RELEASE 1.9, FEBRUARY 6, 2012
C     Compute three new tables for lake outflow
C     ******************************************************************
      USE GWFSFRMODULE
      USE GLOBAL,       ONLY: IOUT, ISSFLG, IBOUND, BOTM, HNEW, NLAY, 
     +                        LAYHDT, IUNIT
      USE PARAMMODULE,  ONLY: MXPAR, PARTYP, IACTIVE, IPLOC
      USE ICHKSTRBOT_MODULE
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
      type (check_bot) :: uzfrp_check
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER Kkper, Kkstp, In, Iunitgwt, Iunitlak, Nsol, Iouts, Igrid
      Integer Iunitbcf, Iunitlpf, Iunithuf, Iunituzf
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      DOUBLE PRECISION h, sbot
      REAL avdpth, avhc, avthk, bottom, dist, dpslpe, dpth1, dpth2,
     +     dpthlw, dndiff, eldn, elslpe, etsw, flw1, flw2, flwlw,
     +     hcslpe, pptsw,rchlen, rough, roughbnk, roughch, runoff,
     +     seglen, strlen, sumlen, thkslpe, top, wdslpe, wdth1, wdth2, 
     +     wdthlw, width, updiff, zero
      INTEGER i, ic, icalc, ichk, icp, iflginit, ii, ik, il, ilay, ip,
     +        ipt, ir, irch, irp, isoptflg, iss, istep, istsg, iwvcnt,
     +        jj, jk, k5, k6, k7, kk, ksfropt, kss, ktot, l, lstbeg,
     +        nseg, nstrpts,krck,irck,jrck,ireachck, j, numval,iunitnum,
     +        ierr,IFLG
C     ------------------------------------------------------------------
C
C-------SET POINTERS FOR CURRENT GRID.
      CALL SGWF2SFR7PNT(Igrid)
      IERR = 0
      IFLG = 0
C
C1------READ ITMP FLAG TO REUSE NON-PARAMETER DATA, 2 PRINTING FLAGS,
C         AND NUMBER OF PARAMETERS BEING USED IN CURRENT STRESS PERIOD. 
      iss = ISSFLG(Kkper)
      zero = 1.0E-7
Cdep added NSFRPAR to IF statement
      IF ( Kkper.GT.1 ) THEN
        IF ( NSFRPAR.EQ.0 ) THEN
          READ (In, *) ITMP, IRDFLG, IPTFLG
          NP = 0
        ELSE
          READ (In, *) ITMP, IRDFLG, IPTFLG, NP
        END IF
      ELSE IF ( NSFRPAR.GT.0 ) THEN
        READ (In, *) ITMP, IRDFLG, IPTFLG, NP
      END IF
C
C2------CHECK FOR TOO MANY SEGMENTS.
      IF ( ITMP.GT.NSS ) THEN
        WRITE (IOUT, 9001)
        CALL USTOP(' ')
      END IF
C
C3------REUSE NON-PARAMETER DATA FROM LAST STRESS PERIOD IF ITMP<0.
      IF ( ITMP.GE.0 ) THEN
C
C4------NOT REUSING DATA -- INITIALIZE NSEGCK LIST TO ZERO FOR ALL
C         SEGMENTS. Moved NSEGCK below ELSE IF 6/9/2005 dep
        IF ( Kkper.GT.1 ) THEN
          DO kss = 1, NSS
            NSEGCK(kss) = 0
          END DO
        END IF
      ELSE IF ( Kkper.EQ.1 ) THEN
        WRITE (IOUT, 9002)
        CALL USTOP(' ')
      ELSE IF ( NSFRPAR.EQ.0 .AND. IUZT.EQ.0 ) THEN
        WRITE (IOUT, 9003)
        RETURN
      ELSE IF ( NSFRPAR.NE.0 ) THEN
C
C5------INITIALIZE NSEGCK TO 0 FOR SEGMENTS THAT ARE DEFINED BY 
C         CURRENTLY USED PARAMETERS.
        WRITE (IOUT, 9003)
        DO ip = 1, MXPAR
          IF ( PARTYP(ip).EQ.'SFR' .AND. IACTIVE(ip).GT.0 ) THEN
            DO ic = IPLOC(1, ip), IPLOC(2, ip)
              NSEGCK(ISEG(3, ic)) = 0
            END DO
          END IF
        END DO
      END IF
 9001 FORMAT (/, ' CANNOT SPECIFY MORE THAN NSS STREAM SEGMENTS')
 9002 FORMAT (//, '  ***  STREAM SEGMENTS MUST BE DEFINED FOR ', 
     +        'FIRST STRESS PERIOD; CODE STOPPING ***')
 9003 FORMAT (/, ' REUSING STREAM SEGMENT DATA FROM LAST STRESS PERIOD')
C
C6------READ NON-PARAMETER STREAM SEGMENT DATA.
      IF ( ITMP.GT.0 ) THEN
        lstbeg = 1
        ichk = 1
        IF ( ISFROPT.GT.0 ) THEN
          IF ( Kkper.GT.1 ) CALL SGWF2SFR7RDSEG(ITMP, lstbeg, In, 
     +                                        Iunitgwt, Iunituzf, 
     +                                        NSEGCK, NSS, ichk, Kkper, 
     +                                        Nsol)
Crgn 10/16/06 fixed logic for calls to RDSEG
        ELSEIF( NSFRPAR.EQ.0 ) THEN
          IF ( Kkper.GT.1 )CALL SGWF2SFR7RDSEG(ITMP, lstbeg, In, 
     +                        Iunitgwt, Iunituzf, NSEGCK, NSS,
     +                        ichk, Kkper, Nsol)
        ELSEIF( NSFRPAR.GT.0 ) THEN
          CALL SGWF2SFR7RDSEG(ITMP, lstbeg, In, 
     +                        Iunitgwt, Iunituzf, NSEGCK, NSS,
     +                        ichk, Kkper, Nsol)
        END IF
      END IF
C
C7------DEACTIVATE ANY PREVIOUSLY USED STREAM PARAMETERS, AND 
C         ACTIVATE PARAMETERS BEING USED IN CURRENT STRESS PERIOD.
      IF ( NSFRPAR.NE.0 ) THEN
        CALL PRESET('SFR')
        DO jj = 1, NP
          CALL SGWF2SFR7PARMOV(In, Iunitgwt, Nsol)
        END DO
      END IF
C
C8------CHECK FOR ERRORS IN SEGMENT DATA.
      IF ( ITMP.GT.0 .OR. NSFRPAR.NE.0 ) THEN
        DO nseg = 1, NSS
          IF ( ISFROPT.EQ.0 ) THEN
            IF ( NSEGCK(nseg).LE.0 .AND. Kkper.EQ.1 ) THEN
              WRITE (IOUT, 9004) nseg
            ELSE IF ( NSEGCK(nseg).GT.1 ) THEN
              WRITE (IOUT, 9005) nseg, NSEGCK(nseg)
              CALL USTOP(' ')
            END IF
          END IF
C
C9------READ DATA ACCORDING TO VARIABLE ISFROPT.
          isoptflg = 0
          IF ( ISFROPT.EQ.1 .OR. ISFROPT.EQ.2 .OR. ISFROPT.EQ.3 )
     +         isoptflg = 1
          IF ( isoptflg.EQ.0 .AND. SEG(8,nseg).LE.SEG(13,nseg) ) THEN
            WRITE (IOUT, 9006) nseg
            IF ( ISEG(1, nseg).EQ.1 .OR. ISEG(1, nseg).EQ.2 ) THEN
              WRITE (IOUT, 9007) nseg, ISEG(1, nseg)
              CALL USTOP(' ')
            END IF
          END IF
          IF ( IDIVAR(2, nseg).GT.0 ) THEN
            WRITE (IOUT, 9008) nseg
            IDIVAR(2, nseg) = 0
          ELSE IF ( IDIVAR(2, nseg).LT.-3 ) THEN
            WRITE (IOUT, 9009) nseg
            IDIVAR(2, nseg) = 0
          ELSE IF ( IDIVAR(2, nseg).EQ.-2 ) THEN
            IF ( SEG(2, nseg).LT.0.0 .OR. SEG(2, nseg).GT.1.0 ) THEN
              WRITE (IOUT, 9010) nseg
              SEG(2, nseg) = 0.0
            END IF
          END IF
        END DO
 9004   FORMAT (/, 5X, '*** WARNING ***  INPUT DATA FOR SEGMENT', I7,
     +          ' WERE NOT DEFINED')
 9005   FORMAT (/, 5X, '*** ERROR ***  DATA FOR SEGMENT', I6, 
     +          ' WERE DEFINED', I3, ' TIMES (INSTEAD OF ONCE)')
 9006   FORMAT (/, 5X, '*** WARNING *** UPSTREAM ELEVATION IS ', 
     +          'EQUAL TO OR LOWER THAN DOWNSTREAM ELEVATION FOR ', 
     +          'SEGMENT No. ', I6)
 9007   FORMAT (/, 5X, '*** ERROR ***  ', 
     +          'SLOPE IS ZERO OR NEGATIVE FOR SEGMENT No.', I5, 
     +          '   SLOPE MUST BE POSITIVE WHEN ICALC IS', I3)
 9008   FORMAT (/, 5X, '*** WARNING *** IPRIOR > 0 FOR NSEG =', I7, /,
     +          10X, 'THIS OPTION NOT YET AVAILABLE; CODE WILL ', 
     +          'ASSUME IPRIOR = 0', /)
 9009   FORMAT (/, 5X, '*** WARNING *** IPRIOR < -3 FOR NSEG =', I7, /,
     +          10X, 'THIS VALUE IS OUT OF RANGE; CODE WILL ', 
     +          'ASSUME IPRIOR = 0', /)
 9010   FORMAT (/, 5X, '*** WARNING *** IPRIOR = -2 FOR NSEG =', I7,
     +          ' & FLOW VALUE IS OUT OF RANGE (.0 - 1.);', /, 10X,
     +          'ASSUME NO DIVERSION OF FLOW', /)
C
C10-----PLACE STREAM SEGMENT IDENTITY NUMBERS IN ISEG ARRAY.
C         5 ASSIGNED TO SEGMENTS NOT RECEIVING TRIBUTARY FLOW.
C         6 ASSIGNED TO SEGMENTS THAT DIVERT FLOW.
C         7 ASSIGNED TO SEGMENTS RECEIVING TRIBUTARY FLOW.
        k5 = 0
        k6 = 0
        k7 = 0
        DO nseg = 1, NSS
C
C11-----IDENTIFY SEGMENTS THAT DIVERT FLOW.
          IF ( IDIVAR(1, nseg).NE.0 ) THEN
            ISEG(3, nseg) = 6
            k6 = k6 + 1
C
C12-----IDENTIFY SEGMENTS THAT DO NOT DIVERT FLOW.
          ELSE
            jj = 0
C
C13-----IDENTIFY SEGMENTS THAT RECEIVE TRIBUTARY FLOW.
            DO ii = 1, NSS
              IF ( IOTSG(ii).EQ.nseg ) jj = 1
            END DO
C
C14-----IDENTIFY SEGMENTS THAT DO NOT RECEIVE TRIBUTARY FLOW.
            IF ( jj.EQ.0 ) THEN
              ISEG(3, nseg) = 5
              k5 = k5 + 1
            ELSE
              ISEG(3, nseg) = 7
              k7 = k7 + 1
              IF ( jj.NE.1 ) WRITE (IOUT, 9011) nseg, jj
            END IF
          END IF
        END DO
C
C15-----TALLY DIFFERENT STREAM SEGMENT TYPES.
        ktot = k5 + k6 + k7
        WRITE (IOUT, 9012) k5, k6, k7
C
C16-----PRINT WARNING IF TALLIED SEGMENTS LESS THAN NSS.
        IF ( ktot.NE.NSS ) THEN
          WRITE (IOUT, 9013) ktot, NSS
          CALL USTOP(' ')
        END IF
 9011   FORMAT (//, 5X, '*** WARNING *** ERROR WHILE ', 
     +          'CLASSIFYING SEGMENTS:   NSEG =', I6, 4X, 'JJ =', I6,//)
 9012   FORMAT (///1X, 'CLASSIFICATION & COUNT OF STREAM SEGMENTS ', 
     +          'BASED ON SOURCE OF INFLOW:', //, 16X, 
     +          'HEADWATER     DIVERSION     RECEIVES TRIBUTARY FLOW', /
     +          16X, '---------     ---------    ', 
     +          ' -----------------------', /, 16X, I6, I15, I16, /)
 9013   FORMAT (/, 5X, '*** WARNING ***  INTERNAL ERROR SUMMING ', 
     +          'TYPES OF STREAM SEGMENTS:  NSEG =', I6, 5X, 'JJ =',
     +          I6//)
C
C17-----PRINT INPUT DATA IF IRDFLG IS ZERO.
C         SKIP IF INPUT READ BY REACHES (ISFROPT = 1, 3, OR 5)
        IF ( IRDFLG.LE.0 ) CALL SGWF2SFR7PRSEG(NSS, 1, Iunitgwt, Kkper,
     +                                         Nsol, Iouts)
C
C18-----COMPUTE STREAM REACH VARIABLES.
        irch = 1
        ksfropt = 0
        DO nseg = 1, NSS
          ireachck = ISTRM(5, irch)
          icalc = ISEG(1, nseg)
          seglen = SEG(1, nseg)
          runoff = SEG(3, nseg)
          etsw = SEG(4, nseg)
          pptsw = SEG(5, nseg)
          sumlen = 0.0
C
C--SET SOME VALUES NEEDED BY THE LMT PACKAGE
C--AS EACH SEGMENT IS READ, DETERMINE IF ANY OF THE FOLLOWING ARE ACTIVE
C  (1) STORAGE (TRANSIENT ROUTING); (2) PRECIP; (3) EVAP; (4) USER-SPECIFIED RUNOFF; 
C  (5) RUNOFF FROM UZF1 PACKAGE; (6) UNSATURATED FLOW BENEATH REACH (NOT AVAILABLE YET)
          IF(IUNIT(49).GT.0) THEN  !IUNIT(49): LMT
            IF(FLOWTYPE(1).EQ.'NA') THEN !Originally: (ITRFLG.EQ.1.AND.FLOWTYPE(1).EQ.'NA')
              NFLOWTYPE = NFLOWTYPE + 1
              FLOWTYPE(1)='VOLUME'
            ENDIF
            IF(FLOWTYPE(2).EQ.'NA') THEN
              NFLOWTYPE = NFLOWTYPE + 1
              FLOWTYPE(2)='RCHLEN'
            ENDIF
            IF(SEG(5,nseg).NE.0.AND.FLOWTYPE(3).EQ.'NA') THEN  ! CHECK FOR SURFACE WATER PRECIP
              NFLOWTYPE = NFLOWTYPE + 1
              FLOWTYPE(3)='PRECIP'
            ENDIF
            IF(SEG(4,nseg).NE.0.AND.FLOWTYPE(4).EQ.'NA') THEN  ! CHECK FOR SURFACE WATER EVAP
              NFLOWTYPE = NFLOWTYPE + 1
              FLOWTYPE(4)='EVAP'
            ENDIF
            IF(SEG(3,nseg).NE.0.AND.FLOWTYPE(5).EQ.'NA') THEN  ! CHECK FOR USER-SPECIFIED RUNOFF
              NFLOWTYPE = NFLOWTYPE + 1
              FLOWTYPE(5)='RUNOFF'
            ENDIF
          ENDIF
C
C19-----COMPUTE VARIABLES NEEDED FOR STREAM LEAKAGE.
          IF ( icalc.EQ.0 .OR. icalc.EQ.1 ) THEN
            wdslpe = (SEG(9, nseg)-SEG(14, nseg))/seglen
            IF ( icalc.EQ.0 ) dpslpe = (SEG(10, nseg)-SEG(15, nseg))
     +                                 /seglen
          END IF
          IF ( ISFROPT.EQ.0 .OR. ISFROPT.EQ.4 .OR. ISFROPT.EQ.5 ) THEN
            ksfropt = 1
            elslpe = (SEG(8, nseg)-SEG(13, nseg))/seglen
            hcslpe = (SEG(6, nseg)-SEG(11, nseg))/seglen
            thkslpe = (SEG(7, nseg)-SEG(12, nseg))/seglen
          END IF
          DO ii = 1, ISEG(4, nseg)
            krck = ISTRM(1, irch)
            irck = ISTRM(2, irch)
            jrck = ISTRM(3, irch)
            rchlen = STRM(1, irch)
            dist = sumlen + (0.5*rchlen)
            STRM(12, irch) = runoff*(rchlen/seglen)             
            IF ( ksfropt.EQ.1 ) THEN
              avhc = SEG(6, nseg) - (hcslpe*dist)
              avthk = SEG(7, nseg) - (thkslpe*dist)
              STRM(2, irch) = elslpe
              STRM(3, irch) = SEG(8, nseg) - (elslpe*dist)
              STRM(4, irch) = STRM(3, irch) - avthk
!             
              uzfrp_check%ltype = LAYHDT(krck)
              uzfrp_check%irchnum = IRCH
              uzfrp_check%iflag = IFLG
              uzfrp_check%iunit = IOUT
              IERR = ICHKSTRBOT(uzfrp_check)
              IF ( IERR.GT.0 )IFLG = IERR
!
              STRM(6, irch) = avhc
              STRM(8, irch) = avthk 
C20-----COMPUTE STREAMBED ELEVATION AND STREAM WIDTH FOR BEGINNING
C         OF EACH STREAM SEGMENT FOR COMPUTATION OF LAKE OUTFLOW.
cdep 4/26/2006
            ELSE
              IF ( ii.EQ.1) THEN
                SEG(8,nseg) = STRM(3,irch) + ( 0.5 * STRM(1,irch)
     +                       * STRM(2,irch) )
              END IF            
            END IF
!dep 4/28/2008 Added check and warning for streambed thickness
            IF (STRM(8, irch).LT.CLOSEZERO)THEN
              WRITE (IOUT, 9030) nseg, irch, STRM(8, irch)
              STRM(8, irch) = 1.0
            END IF
!dep 4/28/2008 end of change
            IF ( icalc.EQ.0 ) THEN
              avdpth = SEG(10, nseg) - (dpslpe*dist)
              STRM(5, irch) = SEG(9, nseg) - (wdslpe*dist)
              STRM(7, irch) = avdpth
              STRM(13, irch) = etsw*rchlen*STRM(5, irch)
              STRM(14, irch) = pptsw*rchlen*STRM(5, irch)
              STRM(15, irch) = avdpth + STRM(3, irch)
              IF ( ksfropt.EQ.1 ) STRM(16, irch)
     +             = (avhc*STRM(5, irch)*rchlen)/avthk
            ELSE IF ( icalc.EQ.1 ) THEN
              STRM(5, irch) = SEG(9, nseg) - (wdslpe*dist)
              STRM(7, irch) = 1.0
              STRM(13, irch) = etsw*rchlen*STRM(5, irch)
              STRM(14, irch) = pptsw*rchlen*STRM(5, irch)
              STRM(15, irch) = STRM(3, irch)
              IF ( ksfropt.EQ.1 ) STRM(16, irch)
     +             = (avhc*STRM(5, irch)*rchlen)/avthk
            ELSE IF ( icalc.GE.2 .AND. icalc.LE.4 ) THEN
              STRM(5, irch) = 1.0
              STRM(7, irch) = 1.0
              STRM(13, irch) = etsw*rchlen
              STRM(14, irch) = pptsw*rchlen
              STRM(15, irch) = STRM(3, irch)
              IF ( ksfropt.EQ.1 ) 
     +             STRM(16, irch) = STRM(5, irch)*STRM(1, irch)
     +                              *STRM(6, irch)/STRM(8, irch)
C
C21-----STOP IF ICALC LESS THAN 0 AND GREATER THAN 4.
            ELSE
              STOP 'icalc problem, < 0 or > 4'
            END IF
            sumlen = sumlen + rchlen
            irch = irch + 1
          END DO
        END DO
C
C22-----CHECK VALUES IN STREAM CROSS SECTION LIST (XSEC).
        DO nseg = 1, NSS
          icalc = ISEG(1, nseg)
          IF ( icalc.EQ.2 ) THEN
            IF ( ABS(XSEC(1,nseg)).GT.zero ) THEN
              WRITE (IOUT, 9014) nseg
              CALL USTOP(' ')
            END IF
            DO jj = 1, 8
              IF ( XSEC(jj, nseg).LT.0.0 ) THEN
                WRITE (IOUT, 9015) nseg, jj, XSEC(jj, nseg)
                CALL USTOP(' ')
              END IF
              kk = jj + 8
              IF ( XSEC(kk, nseg).LT.0.0 ) THEN
                WRITE (IOUT, 9016) nseg, kk, XSEC(kk, nseg)
                CALL USTOP(' ')
              END IF
            END DO
          END IF
        END DO
 9030   FORMAT (/, ' *** WARNING *** STREAMBED THICKNESS', 
     +          'FOR SEGMENT ',I10,' REACH ',I10,  
     +          ' IS', E12.4,' WHICH IS ZERO OR LESS. '/, !gsf
     +          ' VALUE MUST BE GREATER THAN ZERO-- IT HAS BEEN ',
     +          'RESET TO 1.0')
 9014   FORMAT (1X, /, ' *** ERROR *** EIGHT POINT CROSS ', 
     +          'SECTION FOR STREAM SEGMENT', I7,
     +          ' DOES NOT BEGIN WITH ZERO FOR FIRST VALUE --', 
     +          'PROGRAM STOPPING')
 9015   FORMAT (1X, /, ' *** ERROR *** STREAM SEGMENT', I7, 
     +          ' HAS A NEGATIVE X DISTANCE FOR POINT', I6, 
     +          ' INPUT VALUE IS', E11.3, /, 
     +          ' ALL VALUES MUST BE POSITIVE WITH ', 
     +          'FIRST X VALUE STARTING AT EXTREME LEFT ', 
     +          'EDGE OF SECTION LOOKING DOWNSTREAM PROGRAM STOPPING')
 9016   FORMAT (1X, /, ' *** ERROR *** STREAM SEGMENT', I7, 
     +          ' HAS A NEGATIVE Z DISTANCE FOR POINT', I6, 
     +          ' INPUT VALUE IS', E11.3, /, 
     +          ' ALL VALUES MUST BE POSITIVE RELATIVE ', 
     +          'TO STREAMBED ELEVATION ')
C
C23-----CHECK ROUGHNESS COEFFICIENTS WHEN ICALC = 1 OR 2.
        DO nseg = 1, NSS
          icalc = ISEG(1, nseg)
          IF ( icalc.EQ.1 ) THEN
            rough = SEG(16, nseg)
            IF ( rough.LE.0.0 ) THEN
              WRITE (IOUT, 9017) rough
              CALL USTOP(' ')
            END IF
          ELSE IF ( icalc.EQ.2 ) THEN
            roughch = SEG(16, nseg)
            roughbnk = SEG(17, nseg)
            IF ( roughch.LE.0.0 ) THEN
              WRITE (IOUT, 9018) roughch
              CALL USTOP(' ')
            ELSE IF ( roughbnk.LE.0.0 ) THEN
              WRITE (IOUT, 9019) roughbnk
              CALL USTOP(' ')
            END IF
          END IF
        END DO
 9017   FORMAT ('*** ERROR *** ROUGHNESS COEFFICIENT WHEN ', 
     +          'ICALC = 1 IS LESS THAN OR EQUAL TO ZERO', //, 
     +          ' VALUE IS', 1PE11.3, //, ' PROGRAM STOPPING')
 9018   FORMAT ('*** ERROR *** ROUGHNESS COEFFICIENT FOR ', 
     +          'CHANNEL WHEN ICALC =2 IS LESS THAN OR EQUAL TO ZERO',//
     +          ' VALUE IS', 1PE11.3, //, ' PROGRAM STOPPING')
 9019   FORMAT ('*** ERROR *** ROUGHNESS COEFFICIENT FOR BANK ', 
     +          'WHEN ICALC =2 IS LESS THAN OR EQUAL TO ZERO', //,
     +          ' VALUE IS', 1PE11.3, //, ' PROGRAM STOPPING')
C
C24-----CHECK VALUES IN TABLE OF FLOW VERSUS DEPTH AND WIDTH
C         WHEN ICALC = 4.
        DO nseg = 1, NSS
          icalc = ISEG(1, nseg)
          IF ( icalc.EQ.4 ) nstrpts = ISEG(2, nseg)
          IF ( icalc.EQ.4 ) THEN
            flwlw = QSTAGE(1, nseg)
            IF ( flwlw.LE.0.0 ) THEN
              WRITE (IOUT, 9020) nseg
              QSTAGE(1, nseg) = 0.1
            END IF
            dpthlw = QSTAGE(1+nstrpts, nseg)
            IF ( dpthlw.LE.0.0 ) THEN
              WRITE (IOUT, 9021) nseg
              QSTAGE(1+nstrpts, nseg) = 0.01
            END IF
            wdthlw = QSTAGE(1+2*nstrpts, nseg)
            IF ( wdthlw.LE.0.0 ) THEN
              WRITE (IOUT, 9022) nseg
              QSTAGE(1+2*nstrpts, nseg) = 1.0
            END IF
            DO ipt = 2, nstrpts
              flw1 = QSTAGE(ipt-1, nseg)
              flw2 = QSTAGE(ipt, nseg)
              dpth1 = QSTAGE((ipt-1)+nstrpts, nseg)
              dpth2 = QSTAGE(ipt+nstrpts, nseg)
              wdth1 = QSTAGE((ipt-1)+(2*nstrpts), nseg)
              wdth2 = QSTAGE(ipt+(2*nstrpts), nseg)
              IF ( flw2.LE.flw1 ) THEN
                WRITE (IOUT, 9023) nseg, flw2, ipt
                CALL USTOP(' ')
              END IF
              IF ( dpth2.LE.dpth1 ) THEN
                WRITE (IOUT, 9024) nseg, dpth2, ipt
                CALL USTOP(' ')
              END IF
              IF ( wdth2.LT.wdth1 ) WRITE (IOUT, 9025) nseg, wdth2, ipt
            END DO
          END IF
        END DO
C
        WRITE (IOUT, 9026)
      END IF
 9020 FORMAT (/, ' *** WARNING *** FIRST FLOW VALUE IN ', 
     +        'TABLE OF FLOW VERSUS DEPTH AND WIDTH IS ', 
     +        'LESS THAN OR EQUAL TO ZERO FOR SEGMENT NUMBER', I7, /,
     +        ' VALUE SHOULD BE GREATER THAN ZERO-- IT HAS BEEN RESET ',
     +        'TO 0.1 BUT MAY CAUSE INSTABILITY')
 9021 FORMAT (/, ' *** WARNING *** FIRST DEPTH VALUE IN TABLE ',
     +        'OF FLOW VERSUS DEPTH AND WIDTH IS LESS THAN ', 
     +        'OR EQUAL TO ZERO FOR SEGMENT NUMBER', I7, /, 
     +        ' VALUE SHOULD BE GREATER THAN ZERO-- ', 
     +        'IT HAS BEEN RESET TO 0.01 BUT MAY CAUSE INSTABILITY')
 9022 FORMAT (/, ' *** WARNING *** FIRST WIDTH VALUE IN TABLE OF ', 
     +        'FLOW VERSUS DEPTH AND WIDTH IS LESS THAN OR EQUAL',
     +        ' TO ZERO FOR SEGMENT NUMBER', I7, /, 
     +        ' VALUE SHOULD BE GREATER THAN ZERO-- IT HAS BEEN ', 
     +        'RESET TO 1.0 BUT MAY CAUSE INSTABILITY')
 9023 FORMAT (/, ' *** ERROR *** SEGMENT NUMBER', I7, 
     +        'HAS SPECIFIED FLOW VALUE OF', 1PE11.2, ' IN LOCATION',
     +        I6, ' THAT IS LESS THAN OR EQUAL TO PRECEDING VALUE', /,
     +        ' FLOW VALUES MUST BE GREATER THAN PRECEDING VALUE',
     +        ' IN TABLE-- PROGRAM STOPPING')
 9024 FORMAT (/, ' *** ERROR *** SEGMENT NUMBER', I7, 
     +        'HAS SPECIFIED DEPTH VALUE OF', 1PE11.2, ' IN LOCATION',
     +        I6, ' THAT IS LESS THAN OR EQUAL TO PRECEDING VALUE', /,
     +        ' DEPTH VALUES MUST BE GREATER THAN PRECEDING VALUE',
     +        ' IN TABLE-- PROGRAM STOPPING')
 9025 FORMAT (/, ' *** WARNING *** SEGMENT NUMBER', I7, 
     +        ' HAS SPECIFIED WIDTH VALUE OF', 1PE11.2, ' IN LOCATION',
     +        I6, ' THAT IS LESS THAN PRECEDING VALUE', /, 
     +        ' FOR MOST CHANNELS, WIDTH NORMALLY INCREASES WITH FLOW')
 9026 FORMAT (//)
C
C25-----COMPUTE STREAMBED ELEVATIONS FOR TOP AND BOTTOM, AND STREAMBED
C        SLOPE FROM LAND SURFACE ELEVATION WHEN SPECIFIED.
C        MODIFIED BY WOLFGANG SCHMID FOR FARM PROCESS.
Crgn---3/19/07 separated if statement to avoid referencing zero elements in array.
      IF( ABS(IRDFLG).EQ.2 ) THEN
        DO irch = 2, NSTRM
          IF( ISTRM(4, irch).GT.1 ) THEN
            IF( IDIVAR(1,ISTRM(4, irch)-1).GT.0 ) THEN
              icp = ISTRM(3, irch-1)
              irp = ISTRM(2, irch-1)
              IF( ISTRM(5, irch).EQ.1 )  SEG(13, ISTRM(4, irch)-1) =
     +           BOTM(icp, irp, 0) - SEG(13, ISTRM(4, irch)-1 )
            END IF
          END IF
        END DO
        DO nseg = 1, NSS
          IF( IDIVAR(1, nseg).GT.0 ) THEN
C
C26-----COMPUTE STREAMBED TOP ELEVATION FOR CANAL REACHES
C        IN FARM PROCESS.
            seglen = SEG(1, nseg)
            sumlen = 0.0
            DO irch = 1, NSTRM
              IF( IDIVAR(1, ISTRM(4, irch)).EQ.IDIVAR(1, nseg) ) THEN
                icalc = ISEG(1, nseg)  
                rchlen = STRM(1, irch)
                dist = sumlen + (0.5 * rchlen)
                sumlen = sumlen + rchlen
                ic = ISTRM(3, irch)
                ir = ISTRM(2, irch)
                updiff = 0.0   
                IF( ISTRM(5, irch).EQ.1 ) updiff = BOTM(ic, ir, 0) -
     +                                    SEG(8, ISTRM(4, irch))
                dndiff = SEG(13, ISTRM(4, irch))
                STRM(3, irch) = BOTM(ic, ir, 0) - (updiff -
     +                          (((updiff - dndiff) / seglen) * dist))
                avthk = SEG(7, nseg) - (((SEG(7, nseg) -
     +                  SEG(12, nseg)) / seglen) * dist)
                STRM(4, irch) = STRM(3, irch) - avthk
                IF ( icalc.EQ.0 ) THEN
                  STRM(15, irch) = avdpth + STRM(3, irch)
                ELSE IF ( icalc.EQ.1 ) THEN
                  STRM(15, irch) = STRM(3, irch)
                ELSE IF ( icalc.GE.2 .AND. icalc.LE.4 ) THEN
                  STRM(15, irch) = STRM(3, irch)
                END IF
              END IF
            END DO
C
C27-----COMPUTE STREAMBED SLOPE FOR CANAL REACHES IN FARM PROCESS.
C       NOTE THAT FIRST AND LAST REACH CAN NOT BE CANAL REACHES.
            DO irch = 2, NSTRM-1
              IF( IDIVAR(1, ISTRM(4, irch)).EQ.IDIVAR(1, nseg) ) THEN
                STRM(2, irch) = (STRM(3, irch-1) - STRM(3,irch+1) )
     +                           / (0.5 * STRM(1, irch-1) +
     +                          STRM(1, irch) + 0.5 * STRM(1, irch+1))
                IF( ISTRM(5, irch).EQ.1 ) THEN
                  STRM(2, irch) = (SEG(8, ISTRM(4, irch)) -
     +                             STRM(3, irch+1)) / (STRM(1, irch) +
     +                             0.5 * STRM(1, irch+1))
                END IF
                IF( ISTRM(5, irch+1).LT.ISTRM(5, irch) ) THEN
                  ic = ISTRM(3, irch)
                  ir = ISTRM(2, irch)
                  dndiff = SEG(13,ISTRM(4, irch))
                  eldn = BOTM(ic,ir,0) - dndiff
                  STRM(2, irch) = (STRM(3, irch-1) - eldn) / (0.5 *
     +                             STRM(1, irch-1) + STRM(1, irch))
                END IF
                IF( STRM(2, irch).LT.zero ) THEN
                  IF( STRM(2, irch ).LT.zero ) STRM(2, irch) = 1.0E-06
                  WRITE(IOUT,9027)  ISTRM(4,irch), ISTRM(5,irch),
     +                              STRM(2, irch)
 9027             FORMAT(1X,'SLOPE FOR SEGMENT AND REACH ',2(1x,I5),
     +                   'IS LESS THAN 1.0E-07: SETTING SLOPE TO '
     +                   '1.0E-06 ')
                END IF
              END IF
            END DO
          END IF
        END DO
C
        WRITE (IOUT, 9028)
 9028 FORMAT (//)
      END IF
C
C29-----SET FLAGS FOR STEADY STATE OR TRANSIENT SIMULATIONS.
      iflginit = 0
      IF ( Kkper.EQ.1 ) THEN
        iflginit = 1
      ELSE IF ( iss.EQ.0 .AND. ISSFLG(Kkper-1).NE.0 ) THEN
        iflginit = 2
      END IF
C
C30-----DETERMINE VARIABLES WHEN UNSATURATED FLOW IS ACTIVE.
      DO l = 1, NSTRM
        il = ISTRM(1, l)
        ir = ISTRM(2, l)
        ic = ISTRM(3, l)
        h = HNEW(ic, ir, il)
        HLDSFR(l) = h
        istsg = ISTRM(4, l)
        icalc = ISEG(1, istsg)
        IF ( icalc.EQ.2 .AND. IUZT.EQ.1 ) CALL CHANNELAREA(istsg, l)
        IF ( IUZT.EQ.1 .AND. iflginit.GE.1 ) THEN
          sbot = STRM(4, l)
          strlen = STRM(1, l)
          width = STRM(5, l)
C
C31-----SKIP IF CELL IS OUTSIDE ACTIVE BOUNDARY OR IS NOT WATER TABLE.
Cdep
C31B-----SEARCH FOR UPPER MOST ACTIVE CELL IN STREAM REACH.
          ilay = il   
          IF ( IBOUND(ic, ir, il).GT.0 ) THEN
            TOPCELL: DO WHILE ( ilay.LE.NLAY )
              IF ( HNEW(ic, ir, ilay).LE.BOTM(ic,ir,ilay) ) THEN
                ilay = ilay + 1
              ELSE
                EXIT TOPCELL
              END IF
            END DO TOPCELL
          END IF
          IF ( ilay.LE.NLAY ) THEN
            il = ilay
            h = HNEW(ic, ir, il)
          ELSE
            h = DBLE(BOTM(ic,ir,NLAY))
          END IF
          IF ( IBOUND(ic, ir, il).LE.0 ) THEN
            UZDPST(1, l) = 0.0D0
            UZFLST(1, l) = 0.0D0
            UZSPST(1, l) = 0.0D0
            UZTHST(1, l) = THTR(l)
            UZSTOR(1, l) = 0.0D0
            UZOLSFLX(1, l) = 0.0D0
C
C32-----BREAK CHANNEL INTO ISUZN WIDTHS FOR UNSATURATED FLOW 
C         WHEN ICALC IS 2 AND UNSATURATED FLOW IS ACTIVE.
          ELSE IF ( icalc.EQ.2 ) THEN
!            CALL CHANNELAREA(istsg, l) moved to above 
            istep = NSTOTRL/ISUZN
            DO jk = 1, NSTOTRL
              UZTHST(jk, l) = THTR(l)
            END DO
C
C33-----INITIALIZE UNSATURATED ZONE ARRAYS WHEN GROUND WATER HEAD
C         IS LESS THAN BOTTOM OF STREAMBED.
            IF ( sbot.GT.h ) THEN
              iwvcnt = 1
              DO i = 1, ISUZN
                UZDPST(iwvcnt, l) = sbot - h
                UZSPST(iwvcnt, l) = 0.0D0
                NWAVST(i, l) = 1
C
C34-----INITIALIZE UNSATURATED ZONES ARRAYS FOR SECOND STRESS PERIOD
C         WHEN FIRST STRESS PERIOD IS STEADY STATE.
                IF ( iflginit.EQ.2 ) THEN
                  IF ( UZSEEP(i, l).GT.0.0 ) THEN
                    UZFLST(iwvcnt, l) = UZSEEP(i, l)
                    UZTHST(iwvcnt, l) = (((UZFLST(iwvcnt,l)/UHC(l))**(
     +                                  1.0D0/EPS(l)))*(THTS(l)-THTR(l))
     +                                  ) + THTR(l)
                    top = UZTHST(iwvcnt, l) - THTR(l)
                    UZSTOR(i, l) = UZDPST(iwvcnt, l)*top
     +                             *WETPER(i, l)*strlen
                    UZOLSFLX(i, l) = UZSEEP(i, l)
                  ELSE
                    UZFLST(iwvcnt, l) = 0.0D0
                    UZTHST(iwvcnt, l) = THTR(l)
                    UZSTOR(i, l) = 0.0D0
                    UZOLSFLX(i, l) = 0.0D0
                  END IF
C
C35-----INITIALIZE UNSATURATED ZONE ARRAYS WHEN FIRST STRESS PERIOD IS
C         TRANSIENT.
                ELSE IF ( iss.EQ.0 ) THEN
                  top = THTI(l) - THTR(l)
                  IF ( top.LT.CLOSEZERO ) top = 0.0
                  UZTHST(1, l) = THTI(l)
                  UZSTOR(1, l) = UZDPST(1, l)*top*WETPER(1, l)*strlen
                  bottom = THTS(l) - THTR(l)
                  IF ( bottom.LT.CLOSEZERO .OR. top.LT.CLOSEZERO ) THEN
                    UZFLST(1, l) = 0.0D0
                  ELSE
                    UZFLST(1, l) = UHC(l)*(top/bottom)**EPS(l)
                  END IF
C
C36-----INITIALIZE UNSATURATED ZONE ARRAYS WHEN FIRST STRESS PERIOD IS
C         STEADY STATE.
                ELSE
                  UZTHST(1, l) = THTR(l)
                  UZSTOR(1, l) = 0.0D0
                  UZFLST(1, l) = 0.0D0
                  UZOLSFLX(1, l) = 0.0D0
                END IF
                iwvcnt = iwvcnt + istep
              END DO
C
C37-----INITIALIZE UNSATURATED ZONE ARRAYS TO ZERO WHEN NO UNSATURATED
C         ZONE.
            ELSE
              iwvcnt = 1
              istep = NSTOTRL/ISUZN
              DO i = 1, ISUZN
                UZDPST(iwvcnt, l) = 0.0D0
                UZFLST(iwvcnt, l) = 0.0D0
                UZSPST(iwvcnt, l) = 0.0D0
                UZTHST(1, l) = THTR(l)
                iwvcnt = iwvcnt + istep
              END DO
            END IF
            UZOLSFLX(1, l) = UZFLST(1, l)
C
C38-----ONLY ONE UNSATURATED ZONE WIDTH WHEN ICALC IS 1.
          ELSE IF ( icalc.EQ.1 ) THEN
            WETPER(1, l) = width
C
C39-----INITIALIZE UNSATURATED ZONE ARRAYS WHEN GROUND WATER HEAD
C         IS LESS THAN BOTTOM OF STREAMBED.
            IF ( sbot.GT.h ) THEN
              UZDPST(1, l) = sbot - h
              UZSPST(1, l) = 0.0D0
              NWAVST(1, l) = 1
C
C40-----INITIALIZE UNSATURATED ZONE ARRAYS FOR SECOND STRESS PERIOD
C         WHEN FIRST STRESS PERIOD IS STEADY STATE.
              IF ( iflginit.EQ.2 ) THEN
                IF ( UZSEEP(1, l).GT.0.0 ) THEN
                  UZFLST(1, l) = UZSEEP(1, l)
                  UZTHST(1, l) = (((UZFLST(1,l)/UHC(l))**(1.0D0/EPS(l)))
     +                           *(THTS(l)-THTR(l))) + THTR(l)
                  top = UZTHST(1, l) - THTR(l)
                  IF ( top.LT.CLOSEZERO ) top = 0.0
                  UZSTOR(1, l) = UZDPST(1, l)*top*WETPER(1, l)*strlen
                  UZOLSFLX(1, l) = UZSEEP(1, l)
                ELSE
                  UZFLST(1, l) = 0.0D0
                  UZTHST(1, l) = THTR(l)
                  UZSTOR(1, l) = 0.0D0
                  UZOLSFLX(1, l) = 0.0D0
                END IF
C
C41-----INITIALIZE UNSATURATED ZONE ARRAYS WHEN FIRST STRESS PERIOD IS
C         TRANSIENT.
              ELSE IF ( iss.EQ.0 ) THEN
                UZTHST(1, l) = THTI(l)
                top = THTI(l) - THTR(l)
                IF ( top.LT.CLOSEZERO ) top = 0.0
                UZSTOR(1, l) = UZDPST(1, l)*top*width*strlen
                bottom = THTS(l) - THTR(l)
                IF ( bottom.LT.CLOSEZERO .OR. top.LT.CLOSEZERO ) THEN
                  UZFLST(1, l) = 0.0D0
                ELSE
                  UZFLST(1, l) = UHC(l)*(top/bottom)**EPS(l)
                END IF
C
C42-----INITIALIZE UNSATURATED ZONE ARRAYS WHEN FIRST STRESS PERIOD IS
C         STEADY STATE.
              ELSE
                UZTHST(1, l) = THTR(l)
                UZSTOR(1, l) = 0.0D0
                UZFLST(1, l) = 0.0D0
              END IF
            ELSE
C
C43-----INITIALIZE UNSATURATED ZONE ARRAYS TO ZERO WHEN NO UNSATURATED
C         ZONE.
              UZTHST(1, l) = THTR(l)
              UZDPST(1, l) = 0.0D0
              UZFLST(1, l) = 0.0D0
              UZSPST(1, l) = 0.0D0
              UZSTOR(1, l) = 0.0D0
            END IF
            DO ik = 2, NSTOTRL
              UZTHST(ik, l) = THTR(l)
            END DO
            UZOLSFLX(1, l) = UZFLST(1, l)
          END IF
        END IF
      END DO
Cdep    Added new subroutine to compute tables for lake outflow
C44-----COMPUTE VALUES FOR ARRAYS DKLOTFLW AND DLKSTAGE WHEN OUTFLOW FROM
C        LAKES ARE COMPUTED IN THE LAKE PACKAGE.
      IF ( Iunitlak.GT.0 ) THEN
        CALL GWF2SFR7LAKOUTFLW(1)
      END IF
CC45-----READ TABLES FOR SPECIFIED INFLOWS
      IF ( Kkper.EQ.1 ) THEN
        IF ( NUMTAB.GT.0 ) THEN
          DO i=1,NUMTAB
! segment number, number of rows, unit number
            READ(In,*)ISFRLIST(1,i),ISFRLIST(2,i),ISFRLIST(3,i)
            WRITE(iout,9033)ISFRLIST(1,i),ISFRLIST(3,i)
            WRITE(iout,9031)
            numval = ISFRLIST(2,i)
            iunitnum = ISFRLIST(3,i)
            DO j = 1, numval
              READ(iunitnum,*)TABTIME(j,ISFRLIST(1,i)),
     +                     TABFLOW(j,ISFRLIST(1,i))
              IF ( TABFLOW(j,ISFRLIST(1,i)).LT.0.0 ) THEN
                TABFLOW(j,ISFRLIST(1,i)) = 0.0
                WRITE(IOUT,9029)
              END IF
              WRITE(IOUT,9032)TABTIME(j,ISFRLIST(1,i)),
     +                        TABFLOW(j,ISFRLIST(1,i))
            END DO
          END DO
        END IF
      END IF
 9029 FORMAT('A NEGATIVE VALUE FOR FLOW WAS SPECIFIED IN A ',
     +        'SFR TABULAR INFLOW FILE. VALUE WILL BE RESET TO ZERO')
 9033 FORMAT('TABULAR INFLOWS WERE READ FOR SEGMENT ',I6,/
     +       'FROM FILE UNIT NUMBER ',I6,/)
 9031 FORMAT(10X,'TIMES',20X,'INFLOWS')
 9032 FORMAT(5X,F20.10,1X,F20.10)
      RETURN
      END SUBROUTINE GWF2SFR7RP
C
C-------SUBROUTINE GWF2SFR7FM
      SUBROUTINE GWF2SFR7FM(Kkiter, Kkper, Kkstp, Iunitlak, Iunitrch, 
     +                      Iunituzf, Igrid)
C     *****************************************************************
C     ADD STREAM TERMS TO RHS AND HCOF IF FLOW OCCURS IN MODEL CELL
!--------REVISED FOR MODFLOW-2005 RELEASE 1.9, FEBRUARY 6, 2012
C     *****************************************************************
!      USE GWFRCHMODULE,ONLY:RECH  !cjm
!!      USE GWFUZFMODULE,ONLY:FINF  !cjm
      USE GWFSFRMODULE
      USE GLOBAL,       ONLY: NLAY, IOUT, ISSFLG, IBOUND, HNEW, HCOF, 
     +                        RHS, BOTM, LBOTM
!!      USE GLOBAL,       ONLY: NLAY, IOUT, ISSFLG, IBOUND, HNEW, HCOF, 
!!     +                        RHS, BOTM, LBOTM, DELR, DELC
      USE GWFBASMODULE, ONLY: DELT, TOTIM, HDRY
      USE GWFLAKMODULE, ONLY: THETA, STGOLD, STGNEW, VOL, LKARR1
      IMPLICIT NONE
      INTRINSIC IABS, ABS, DABS, MIN, DSQRT, FLOAT, SQRT, SNGL
C     -----------------------------------------------------------------
C     SPECIFICATIONS:
C     -----------------------------------------------------------------
C     FUNCTIONS
C     -----------------------------------------------------------------
      REAL CALCUNSATFLOBOT
      EXTERNAL CALCUNSATFLOBOT
      DOUBLE PRECISION SMOOTH
      EXTERNAL SMOOTH
C     -----------------------------------------------------------------
C     ARGUMENTS
C     -----------------------------------------------------------------
      INTEGER Kkiter, Kkper, Iunitlak, Igrid, Kkstp, Iunitrch,
     +        Iunituzf
C     -----------------------------------------------------------------
C     LOCAL VARIABLES
C     -----------------------------------------------------------------
      DOUBLE PRECISION cstr, cstr1, cstr2, dbleak, dlet1, dlet2, dlfh, 
     +                 dlh, dlpp1, dlpp2, dlwp1, dlwp2, depth, depthx, 
     +                 depthp, deptha, depthb, depthc, depthd, depth1, 
     +                 depth2, dlkstr, deps, et1, et2, fhstr1, fhstr2,
     +                 flobot, flobot1, flobot2, flowc, flowin, flowot,
     +                 flwmdpta, flwmdptb, flwmdptc, flwmdptd, flwmdpt1,
     +                 flwmdpt2, flwmpt, h, hstr, pp1, pp2, sbot, slope,
     +                 strleak, strtop, trbflw, upflw, wetperm,
     +                 wetperma, wetpermb, wetpermc, wetpermd, wetperm1,
     +                 wetperm2, wetpermp, wetpermx, width, widtha, 
     +                 widthb, widthc, widthd, width1, width2, widthp, 
     +                 widthx, bwdth, cdpth, fdpth, awdth, f1, f2, fp, 
     +                 enpt1, enpt2, flwen1, flwen2, flwp, flobotp, 
     +                 flobotold, flwpetp, flwx, flwmpt2, flwest, 
     +                 flwpet1, flwpet2, err, dlhold, precip, etstr, 
     +                 runof, runoff, qa, qb, qc, qd, hstrave, fbot
      DOUBLE PRECISION fbcheck, hld, totflwt, sbdthk, thetas, epsilon, 
     +                 thr, thet1, dvrsn, fact,
     +                 depthtr, dwdh, wetpermsmooth,cstrsmooth
!      DOUBLE PRECISION rhsh1, rhsh2, hcofh1, hcofh2
      REAL areamax, avhc, errold, fks, ha, qcnst, seep, 
     +     stgon, strlen, roughch, roughbnk, widthch, deltinc, qlat, 
     +     fltest, Transient_bd
!!     +     fltest, Transient_bd, dvt, dum, totdum  !CJM
!      real fin, fout
      INTEGER i, ibflg, ic, icalc, idivseg, iflg, iic, iic2, iic3, iic4,
     +        il, ilay, iprior, iprndpth, iprvsg, ir, istsg, itot,itrib,
     +        itstr, iwidthcheck, kerp, kss, l, lk, ll, nstrpts, nreach,
     +        maxwav, icalccheck, iskip, iss, lsub, numdelt, irt, !  ii, 
     +        lfold, illake, lakid
!!      INTEGER irr, icc, icount  !cjm
      DOUBLE PRECISION FIVE_THIRDS
      PARAMETER (FIVE_THIRDS=5.0D0/3.0D0)
C     -----------------------------------------------------------------
C
C-------SET POINTERS FOR CURRENT GRID.
      CALL SGWF2SFR7PNT(Igrid)
C
      iss = ISSFLG(Kkper)
      numdelt = NUMTIM
C
C1------RETURN IF NO STREAMS (NSTRM<=0).
      IF ( NSTRM.LE.0 ) RETURN
      maxwav = NSFRSETS*NSTRAIL
C
C2------THERE ARE STREAMS.  INITIALIZE SEGMENT INFLOWS AND OUTFLOWS
C         TO ZERO FOR LAKE PACKAGE.
      itstr = 0
      iprvsg = -1 
      fact = FACTOR
!      fin = 0.0
!      fout = 0.0
      IF ( Iunitlak.GT.0 ) THEN
        DO i = 1, NSS
          STRIN(i) = 0.0
Cdep commented out FXLKOT to allow it vary with lake stage in LAK7
Cdep          FXLKOT(i) = 0.0
        END DO
Cdep  Change time weighting factor for lake stage calculation.
Cdep   6/27/2005
        IF (iss.NE.1) THEN
          thet1 = THETA
        ELSE
          thet1 = 1.0D0
        END IF
      END IF      
C
C2b-----START INTERNAL TIME LOOP FOR STREAMFLOW ROUTING.
      deltinc = DELT
      IF ( IRTFLG.GT.0 .AND. iss.EQ.0 ) THEN
        deltinc = DELT/FLOAT(numdelt)
      ELSE
        numdelt = 1
      END IF
      DO irt = 1, numdelt
C
C3------DETERMINE LAYER, ROW, COLUMN OF EACH REACH.
        DO l = 1, NSTRM
          lfold = l
          IF ( Nfoldflbt==1 ) lfold = 1
!          flowin = 0.0D0
          flowin = strm(10, l)
          dvrsn = 0.0D0
!         rhsh1 = 0.0d0
!         rhsh2 = 0.0d0
!         hcofh1 = 0.0d0
!         hcofh2 = 0.0d0
          Transient_bd = 0.0
!dep changed to allow allocation of QA and QB from Steady State
          IF ( irt.EQ.1 ) THEN
	      IF (iss.NE.0) THEN
              STRM(25,l) = STRM(9,l)
              STRM(26,l) = STRM(10,l)
	      ELSE 
              STRM(27,l) = STRM(25,l)
              STRM(28,l) = STRM(26,l)
	      END IF
            SUMLEAK(l) = 0.0D0
            SUMRCH(l)  = 0.0D0
          END IF
          lsub = l
          ll = l - 1
          il = ISTRM(1, l)
          ir = ISTRM(2, l)
          ic = ISTRM(3, l)
          strtop = STRM(3, l)
C
C4------DETERMINE STREAM SEGMENT AND REACH NUMBER.
          istsg = ISTRM(4, l)
          nreach = ISTRM(5, l)
          icalc = ISEG(1, istsg)
! RGN 5/9/09 set slope for all icalc
          slope = STRM(2, l)
          IF ( icalc.EQ.1 .OR. icalc.EQ.2 ) THEN
            roughch = SEG(16, istsg)
            IF(icalc.EQ.1) widthch = SEG(9, istsg)
            IF(icalc.EQ.2) roughbnk = SEG(17, istsg)
          END IF
          IF ( icalc.EQ.4 ) nstrpts = ISEG(2, istsg)
C
C5------SET FLOWIN EQUAL TO STREAM SEGMENT INFLOW IF FIRST REACH.
          IF ( nreach.EQ.1 ) THEN
            IF ( ISEG(3, istsg).EQ.5 ) flowin = SEG(2, istsg)
C
C6------STORE OUTFLOW FROM PREVIOUS SEGMENT IN SGOTFLW LIST AND IN
C         STRIN FOR LAKE PACKAGE.
            IF ( istsg.GT.1 ) THEN
              iprvsg = ISTRM(4, ll)
              SGOTFLW(iprvsg) = STRM(9, ll)
              IF ( Iunitlak.GT.0 ) STRIN(iprvsg) = STRM(9, ll)
            END IF
C
C7------COMPUTE INFLOW OF A STREAM SEGMENT EMANATING FROM A LAKE.
            IF ( (Iunitlak.GT.0) .AND. (IDIVAR(1,istsg).LT.0) ) THEN
              lk = IABS(IDIVAR(1, istsg))
C DEP moved the following code to SFR7AD to initialize FXLKOT at every time step.
C DEP FXLKOT is now adjusted in LAK7FM for limiting to available lake water.
C
C8------CHECK IF LAKE OUTFLOW IS SPECIFIED AT A FIXED RATE.
!              IF ( SEG(2, istsg).GT.CLOSEZERO .AND. 
!     +             VOL(lk).GT.CLOSEZERO ) THEN
!                IF( SEG(2, istsg)*DELT-VOL(lk).LT.-CLOSEZERO )THEN
!                  FXLKOT(istsg) = SEG(2, istsg)
!                ELSE
!                  FXLKOT(istsg) = VOL(lk)/DELT
!                  WRITE(IOUT,9000) lk,FXLKOT(istsg)
!9000              FORMAT(/5X, '*** WARNING *** SPECIFIED OUTFLOW ',
!     +                   'VOLUME FOR TIME STEP IS GREATER THAN ',
!     +                   'VOLUME IN LAKE ',I5,//' RATE HAS BEEN ', 
!     +                   'DECREASED TO ',1PE15.7)
!                END IF
!                flowin = FXLKOT(istsg)
!              ELSE IF ( SEG(2, istsg).LT.-CLOSEZERO ) THEN
!                WRITE (IOUT, 9001) istsg
! 9001           FORMAT (/5X, '*** WARNING *** NEGATIVE LAKE OUTFLOW ',
!     +                  'NOT ALLOWED; SEG = ', I6, /10X, 
!     +                  'CODE WILL ASSUME FLOW = 0.0'/)
!                SEG(2, istsg) = 0.0
!                flowin = SEG(2, istsg)
!                FXLKOT(istsg) = flowin
!              END IF
C
C9------SPECIFIED FLOW FROM LAKE IS ZERO AND ICALC IS ZERO.
!              IF ( icalc.EQ.0 ) THEN
!              END IF
C
C9B-----ESTIMATE LAKE OUTFLOW FOR FIRST ITERATION OF SIMULATION.
              IF ( SEG(2, istsg).LT.CLOSEZERO ) THEN
                IF( Kkper.EQ.1 .AND. Kkstp.EQ.1 .AND. Kkiter.EQ.1 ) THEN
                  stgon = (1.0-thet1)*STGOLD(lk) + thet1*STGNEW(lk)
                  dlkstr = stgon - SEG(8, istsg)
C
C10-----FLOW FROM LAKE COMPUTED USING MANNINGS FORMULA AND ASSUMING A
C         WIDE RECTANGULAR CHANNEL.
                  IF ( dlkstr.GT.NEARZERO .AND. icalc.EQ.1 ) THEN
! RGN 10/3/11 added smoothing for constant width channels that go dry
                    flowin = (CONST/roughch)*widthch*
     +                        smooth(dlkstr,dwdh)
     +                       *(dlkstr**FIVE_THIRDS)*(DSQRT(slope))
C
C11-----FLOW FROM LAKE COMPUTED USING MANNINGS FORMULA AND EIGHT POINT
C         CROSS-SECTIONAL AREA.
                  ELSE IF ( dlkstr.GT.NEARZERO .AND. icalc.EQ.2 ) THEN
                    CALL GWF2SFR7FLW(dlkstr, istsg, roughch, 
     +                             roughbnk, slope, wetperm, 
     +                             flowin, width)
C
C12-----FLOW FROM LAKE COMPUTED USING FORMULA-- Q=(DEPTH/CDPTH)**1/FDPTH).
                  ELSE IF ( dlkstr.GT.NEARZERO .AND. icalc.EQ.3 ) THEN
                    cdpth = SEG(9, istsg)
                    fdpth = SEG(10, istsg)
                    flowin = (dlkstr/cdpth)**(1.0D0/fdpth)
C
C13-----FLOW FROM LAKE COMPUTED USING TABULATED VALUES.
                  ELSE IF ( dlkstr.GT.NEARZERO .AND. icalc.EQ.4 ) THEN
                    CALL GWF2SFR7TBF(flowin, dlkstr, width, 
     +                           nstrpts, nreach, istsg, 
     +                           Kkiter, 0)
                  ELSE IF ( dlkstr.LT.NEARZERO .AND. icalc.GT.0 ) THEN
                    flowin = 0.0D0
                  END IF
                  STROUT(istsg)= flowin
                ELSE
                  flowin = STROUT(istsg)
                END IF
              ELSE
                flowin = FXLKOT(istsg)
              END IF
            END IF
C
C14-----COMPUTE ONE OR MORE DIVERSIONS FROM UPSTREAM SEGMENT.
Crgn&dep   revised computation of diversions and added subroutine
            IF( istsg.GT.1 )THEN
              DO kss = 2, NSS
                upflw = SGOTFLW(istsg-1)
                idivseg = kss
                IF( IDIVAR(1,kss).EQ.istsg-1 ) THEN
                   dvrsn = SEG(2,idivseg)
                   iprior = IDIVAR(2,kss)
                  CALL GWF2SFR7DIVERS(iprior, upflw, dvrsn)
                  DVRSFLW(kss) = dvrsn
                  SGOTFLW(istsg-1) = SGOTFLW(istsg-1) - dvrsn
                END IF
              END DO
C
C20-----SET FLOW INTO DIVERSION IF SEGMENT IS DIVERSION.
              IF( ISEG(3,istsg).EQ.6 ) THEN
                IF( IDIVAR(1,istsg).GT.0 ) flowin = DVRSFLW(istsg)
              END IF
            END IF
C20B-----STORE OUTFLOW FROM PREVIOUS SEGMENT FOR RECHARGE  !cjm   
!            IF ( istsg.GT.1 ) THEN
!              IF (Iunitrch.GT.0 .OR. Iunituzf.GT.0) THEN
!                iprvsg = ISTRM(4, ll)
!                IF ( DVRCH(iprvsg) .GT. 0) THEN
!                  IDVFLG = 1
!                  DO icount = 1, DVRCH(iprvsg)
!                    irr = DVRCELL(icount, 1, iprvsg)
!                    icc = DVRCELL(icount, 2, iprvsg)
!                    IF ( Iunituzf.GT.0 ) THEN
!                      dvt = SGOTFLW(iprvsg)*DVRPERC(icc,irr)
!                      dvt = dvt/(DELR(IC)*DELC(IR))
!                      FINF(icc, irr) = RECHSAVE(icc, irr) + 
!     +                              dvt*(1.0-DVEFF(iprvsg))
!                    ELSE
!                      dvt = (SGOTFLW(iprvsg) / float(DVRCH(iprvsg)))
!                      RECH(icc, irr) = RECHSAVE(icc, irr) + 
!     +                              dvt*(1.0-DVEFF(iprvsg))
!                    END IF
!                  END DO
!                END IF
!              END IF
!            END IF
C
C21-----SUM TRIBUTARY OUTFLOW AND USE AS INFLOW INTO DOWNSTREAM SEGMENT.
            IF ( istsg.GE.1 .AND. ISEG(3, istsg).EQ.7 ) THEN
              flowin = 0.0D0
              DO itrib = 1, NSS
                IF ( istsg.EQ.IOTSG(itrib) ) THEN
                  trbflw = SGOTFLW(itrib)
                  flowin = flowin + trbflw
                END IF
              END DO
              flowin = flowin + SEG(2, istsg)
              IF ( flowin.LT.-NEARZERO ) THEN
                flowin = 0.0D0
!                WRITE (IOUT, 9002) istsg  ! RGN Stop write 
 9002           FORMAT (//2X, '*** WARNING *** FLOW INTO TRIBUTARY ', 
     +                  'STREAM SEGMENT No. ', I6, ' WAS NEGATIVE; ', 
     +                  'FLOWIN RE-SET = 0.0'/)
              END IF
            END IF
C
C22-----SET INFLOW EQUAL TO OUTFLOW FROM UPSTREAM REACH WHEN REACH 
C         IS GREATER THAN 1.
          ELSE IF ( nreach.GT.1 ) THEN
            flowin = STRM(9, ll)
          END IF
C
C23-----SEARCH FOR UPPER MOST ACTIVE CELL IN STREAM REACH.
          ilay = il
          TOPCELL: DO WHILE ( ilay.LE.NLAY )
            IF ( IBOUND(ic, ir, ilay).EQ.0 ) THEN
              ilay = ilay + 1
            ELSE
              EXIT TOPCELL
            END IF
          END DO TOPCELL
          IF ( ilay.LE.NLAY ) il = ilay
            SUMLEAK(l) = 0.0D0
            SUMRCH(l)  = 0.0D0
            h = HNEW(ic, ir, il)
C
C24-----INITIALIZE VARIABLES.
          iprndpth = 0
          depth = STRM(7, l)
          IF ( depth.LT.NEARZERO ) depth = 0.D0
          strtop = STRM(3, l)
          sbot = STRM(4, l)
          width = STRM(5, l)
          wetperm = width
          strlen = STRM(1, l)
          hld = HLDSFR(l)
! Added code to test for BCF or LPF 11/19/07
          IF ( ABS(SNGL(hld)-HDRY).LT.CLOSEZERO ) hld = h
          avhc = STRM(6, l)
          sbdthk = STRM(8, l)
          hstr = depth + STRM(3, l)
! factor for losing streams
          if ( hstr-h > 0.0 ) then
            avhc = avhc*fact
          else
            avhc = STRM(6, l)
          end if
          cstr = STRM(16, l)
          precip = STRM(14, l)
          etstr = STRM(13, l)
          runof = STRM(12, l)
          runoff = STRM(24, l)
          strleak = strlen*avhc
          depthx = 0.0D0
          widthp = 0.0D0
          dbleak = DLEAK
          deps = 0.999*DLEAK
          dlh = deps
          dlhold = 1.0D6
          itot = 0
          iskip = 0
          IF ( icalc.EQ.1 ) THEN
            qcnst = CONST*width*SQRT(slope)/roughch
          ELSE IF ( icalc.EQ.3 ) THEN
            cdpth = SEG(9, istsg)
            fdpth = SEG(10, istsg)
            awdth = SEG(14, istsg)
            bwdth = SEG(15, istsg)
          END IF
C
C25-----INITIALIZE UNSATURATED ZONE VARIABLES.
          icalccheck = 0
          flobotold = 0.0D0
          areamax = 0.0
          IF ( IUZT.EQ.1 ) THEN
            IF ( icalc.EQ.1 .OR. icalc.EQ.2 ) icalccheck = 1
            IF ( icalccheck.EQ.1 ) THEN
              IF ( icalc.EQ.1 ) THEN
                wetperm = STRM(5, l)
                areamax = WETPER(1, l)*strlen
                UZSEEP(1, l) = 0.0
              ELSE IF ( icalc.EQ.2 ) THEN
                DO i = 1, ISUZN
                  UZSEEP(i, l) = 0.0D0
                  areamax = areamax + WETPER(i, l)*strlen
                END DO
              END IF
              thetas = THTS(l)
              fks = UHC(l)
              thr = THTR(l)
              epsilon = EPS(l)
              ha = -.15
              totflwt = 0.0D0
              fbcheck = 1.0D-12/deltinc
              IF ( fbcheck.LT.5.0D-8 ) fbcheck = 5.0D-8
            END IF
          END IF
C
C26-----SET STREAMBED HYDRAULIC CONDUCTIVITY AND STREAM LEAKAGE TO
C         ZERO WHEN NOT AN ACTIVE CELL.
          IF ( IBOUND(ic, ir, il).LE.0 ) THEN
            avhc = 0.0
            strleak = 0.0D0
            h = hstr
            IF ( icalc.LE.1 ) iskip = 1
          END IF
C26a----CHECK IF LAKE INUNDATES STREAM. IF SO THEN SET avhc and cstr = 0.
          IF ( Iunitlak.GT.0 ) THEN
            illake = il - 1
            IF ( illake.GE.1 ) THEN
              lakid = LKARR1(ic, ir, illake)
              IF ( lakid.GT.0 ) THEN
                IF ( STGNEW(lakid).GT.
     +              dble(BOTM(ic,ir,LBOTM(illake)))) THEN
                  cstr = 0.0
                  avhc = 0.0
                END IF
              END IF
            END IF
          END IF
C
C27-----BEGIN COMPUTATION OF STREAM DEPTH FOR ACTIVE CELL.
C
C28-----COMPUTE FLOW AT MIDPOINT OF REACH IGNORING STREAMBED LEAKAGE.
          IF ( icalc.EQ.0 ) THEN
            flwmpt = flowin + 0.5D0*(runof+runoff-etstr+precip)
Crgn 10/23/06 initialize flowc
            flowc = flowin + runof+runoff-etstr+precip
            SFRQ(4, l) = width
            IF ( flwmpt.LT.NEARZERO ) flwmpt = 0.0D0
          END IF
          IF ( icalc.EQ.1 ) SFRQ(4, l) = width
          IF ( icalc.EQ.1 ) THEN
            flowc = flowin + (runof+runoff-etstr+precip)
            flwmpt = flowin + 0.5D0*(runof+runoff-etstr+precip)
            IF ( flwmpt.LT.NEARZERO ) flwmpt = 0.0D0
            IF ( flowc.LT.NEARZERO ) flowc = 0.0D0
! RGN might need to correct depth for smooth width
            depth = (flwmpt/qcnst)**0.6D0
            IF ( depth.LT.NEARZERO ) THEN
              depth = 0.0D0
              hstr = strtop
            ELSE
              hstr = strtop + depth
            END IF
            cstr = (avhc*width*strlen)/sbdthk
          ELSE IF ( icalc.GE.2 ) THEN
            flwmpt = flowin + 0.5*(runof+runoff)
            flowc = flowin + runof + runoff
            IF ( flowc.LT.NEARZERO ) flowc = 0.0D0
C
C29-----CALCULATE AN INITIAL ESTIMATE OF FLOW IN CHANNEL.
            IF ( flwmpt.LT.NEARZERO ) THEN
              flwmpt = 0.0D0
              depth = 0.0D0
              width = 0.0D0
              wetperm = 1.0D0
              IF ( avhc.LE.CLOSEZERO ) iskip = 1
            END IF
            IF ( iskip.EQ.0 ) THEN
              IF ( flwmpt.LT.NEARZERO .AND. h.GT.strtop ) THEN
                flwest = (strlen*avhc/sbdthk)*(ABS(strtop-h))
              ELSE
                flwest = flwmpt
              END IF
              IF ( flwest.LT.NEARZERO ) THEN
                depth = 0.0D0
                width = 1.0D0
                wetperm = width
              ELSE IF ( icalc.EQ.2 ) THEN
                CALL GWF2SFR7DPTH(flwest, slope, istsg, nreach, 
     +                            roughch, roughbnk, wetperm,
     +                            depth, itstr, width, iprndpth)
              ELSE IF ( icalc.EQ.3 ) THEN
                depth = cdpth*(flwest**fdpth)
                width = awdth*(flwest**bwdth)
                wetperm = width
              ELSE IF ( icalc.EQ.4 ) THEN
                CALL GWF2SFR7TBD(flwest, depth, width, nstrpts,
     +                           istsg)
                wetperm = width
              END IF
              cstr = (avhc*wetperm*strlen)/sbdthk
              flowc = flowc + (precip-etstr)*width
            END IF
          END IF
C
C30-----ESTIMATE DEPTH USING BISECTION METHOD WHEN ICALC IS GREATER THAN 0.
          iflg = 1
C
C30b----SKIP NEWTON METHOD WHEN ICALC IS 1 AND SURFACE INFLOW IS ZERO.
! RGN 10/4/11 need newton to smooth width to zero for drying channels
          IF ( icalc.EQ.1 .AND. hstr.LE.strtop .AND. h.LE.strtop) 
     +         iflg = 0
C30c----SKIP NEWTON METHOD WHEN REACH OUTSIDE ACTIVE AREA AND ISKIP IS 1.
          IF ( iskip.NE.0 ) iflg = 0
          IF ( h.LE.strtop .AND. flowc.LT.NEARZERO ) iflg = 0
          IF ( icalc.GE.1 .AND. iflg.EQ.1 ) THEN
C
C31-----ESTIMATE INITIAL ENDPOINTS.
            enpt1 = 0.0D0
            IF ( depth.GT.NEARZERO ) THEN
              IF ( (strtop-h).GT.NEARZERO ) THEN
                enpt2 = 0.9D0*depth
              ELSE
                enpt2 = 1.1D0*depth - (strtop-h)
              END IF
            ELSE IF ( (strtop-h).GT.NEARZERO ) THEN
              enpt2 = 1.0D0
            ELSE
              enpt2 = 0.99D0*(h-strtop)
            END IF
C
C32-----ESTIMATE FLOW AT ENDPOINT1.
! RGN might need to smooth width for end points
            IF ( h.GT.strtop ) THEN
              flobot1 = cstr*(strtop-h)
              flwen1 = flwmpt - 0.5D0*flobot1
            ELSE
              flobot1 = 0.0D0
              flwen1 = flwmpt
            END IF
C
C33-----ESTIMATE DEPTH FOR ENDPOINTS WHEN ICALC IS 1.
            IF ( icalc.EQ.1 ) THEN
              IF ( h.GT.sbot ) THEN
                flobot2 = (cstr*(strtop+enpt2-h))
              ELSE IF ( icalccheck.EQ.1 ) THEN
                flobot2 = CALCUNSATFLOBOT(enpt2, avhc, fks, width,
     +                                    sbdthk, areamax, strlen,
     +                                    fbcheck, NWAVST(:,l), maxwav,
     +                                    FOLDFLBT(lfold))
              ELSE
                flobot2 = (cstr*(strtop+enpt2-sbot))
              END IF
              IF ( flobot2.GT.flowc ) flobot2 = flowc
              depth2 = ((flwmpt-0.5D0*flobot2)/qcnst)**0.6D0
              depth1 = ((flwmpt-0.5D0*flobot1)/qcnst)**0.6D0
C
C34-----ESTIMATE DEPTH, WIDTH AND WETTED PERIMETER WHEN
C         ICALC IS GREATER THAN OR EQUAL TO 2.
            ELSE IF ( icalc.GE.2 ) THEN
              IF ( icalc.EQ.2 ) THEN
                CALL GWF2SFR7FLW(enpt2, istsg, roughch,
     +                           roughbnk, slope, wetperm2, flwen2,
     +                           width2)
              ELSE IF ( icalc.EQ.3 ) THEN
                flwen2 = (enpt2/cdpth)**(1.0/fdpth)
                IF ( flwen2.GT.NEARZERO ) THEN
                  width2 = awdth*(flwen2**bwdth)
                  wetperm2 = width2
                ELSE
                  width2 = 0.0D0
                  wetperm2 = 1.0D0
                END IF
              ELSE IF ( icalc.EQ.4 ) THEN
                CALL GWF2SFR7TBF(flwen2, enpt2, width2, nstrpts, 
     +                           nreach, istsg, Kkiter, 0)
                wetperm2 = width2
              END IF
C
C35-----ESTIMATE LEAKAGE THROUGH STREAMBED WHEN ICALC GREATER THAN OR
C         EQUAL TO 2.
              IF ( width2.GT.NEARZERO ) THEN
                flwpet2 = (precip-etstr)*width2
              ELSE
                flwpet2 = (precip-etstr)
                IF ( flwpet2.LT.NEARZERO ) flwpet2 = 0.0D0
              END IF
              IF ( h.GT.sbot ) THEN
                flobot2 = ((avhc*wetperm2*strlen/sbdthk)*(strtop-h))
              ELSE IF ( icalccheck.EQ.1 ) THEN
                flobot2 = CALCUNSATFLOBOT(enpt2, avhc, fks, wetperm2, 
     +                                    sbdthk, areamax, strlen, 
     +                                    fbcheck, NWAVST(:,l), maxwav,
     +                                    FOLDFLBT(lfold))
              ELSE
                flobot2 = ((avhc*wetperm2*strlen/sbdthk)
     +                    *(strtop+enpt2-sbot))
              END IF
              flwmpt2 = flwmpt
              IF ( flobot2.GE.flowc+flwpet2 ) THEN
                flobot2 = flowc + flwpet2
                flwmpt2 = 0.5D0*(flowc+flwpet2)
              END IF
              flwen2 = flwmpt2
              IF ( icalc.EQ.2 ) THEN
                CALL GWF2SFR7DPTH(flwen1, slope, istsg, nreach, 
     +                            roughch, roughbnk, 
     +                            wetperm1, depth1, itstr, width1, 
     +                            iprndpth)
              ELSE IF ( icalc.EQ.3 ) THEN
                depth1 = cdpth*(flwen1**fdpth)
                width1 = awdth*(flwen1**bwdth)
                wetperm1 = width1
              ELSE IF ( icalc.EQ.4 ) THEN
                CALL GWF2SFR7TBD(flwen1, depth1, width1, nstrpts,
     +                           istsg)
              END IF
C
C36-----SET DEPTH2 AND WIDTH2 TO ZERO, AND WETPERM2 TO ONE WHEN FLOW AT 
C         ENDPOINT2 IS LESS THAN OR EQUAL TO ZERO.
              IF ( flwen2.LT.NEARZERO ) THEN
                depth2 = 0.0D0
                width2 = 0.0D0
                wetperm2 = 1.0D0
C
C37-----OTHERWISE CALCULATE DEPTH2, WIDTH2, AND WETPERM2.
              ELSE IF ( icalc.EQ.2 ) THEN
                CALL GWF2SFR7DPTH(flwen2, slope, istsg, nreach, 
     +                            roughch, roughbnk, 
     +                            wetperm2, depth2, itstr, width2, 
     +                            iprndpth)
              ELSE IF ( icalc.EQ.3 ) THEN
                depth2 = cdpth*(flwen2**fdpth)
                width2 = awdth*(flwen2**bwdth)
                wetperm2 = width2
              ELSE IF ( icalc.EQ.4 ) THEN
                CALL GWF2SFR7TBD(flwen2, depth2, width2, nstrpts, 
     +                           istsg)
              END IF
            END IF
C
C38-----DETERMINE ROOT FOR ENDPOINT 1 WHEN DEPTH IS GREATER THAN 0.
            IF ( depth1.GT.NEARZERO ) THEN
              f1 = enpt1 - depth1
            ELSE
              enpt1 = 0.0D0
              width1 = 0.0D0
              wetperm1 = 1.0D0
              f1 = enpt1 - 0.0D0
C
C39-----DETERMINE ROOT FOR ENDPOINT 2 WHEN DEPTH IS GREATER THAN 0.
            END IF
            IF ( depth2.GT.NEARZERO ) THEN
              f2 = enpt2 - depth2
              IF ( f2.LT.NEARZERO ) enpt2 = depth2
            ELSE
              depth2 = 0.0D0
              width2 = 0.0D0
              wetperm2 = 1.0D0
              f2 = enpt2 - 0.0D0
            END IF
            iflg = 1
C
C40-----ITERATE THROUGH NEWTON METHOD TO FIND ESTIMATE OF STREAM DEPTH
C         AND STREAMBED LEAKAGE WHEN ICALC IS GREATER THAN 0.
            depthp = (enpt1+enpt2)*0.5D0
            depthx = depthp
            iic = 0
            iic2 = 0
            iic3 = 0
            fhstr1 = 0.0D0
            fhstr2 = 0.0D0
            flobotp = 0.0D0
            DO WHILE ( iflg.EQ.1 )
              itot = itot + 1
              ibflg = 0
              depth1 = depthp
              depth2 = depth1 + 2.0D0*(deps)
              hstr = depth1 + STRM(3, l)
              if ( hstr-h > 0.0 ) then
                avhc = avhc*fact
              else
                avhc = STRM(6, l)
              end if
C
C41-----CALCULATE FLOBOT1 AND FLOBOT2 FOR ICALC EQUAL TO 1.
Cdep  Corrected depth1+dlh and depth2+dlh to be depth1 and depth2.
              IF ( icalc.EQ.1 ) THEN
! RGN 10/4/11 added smoothing for width when icalc=1
                width1 = width*smooth(depth1,dwdh)
                width2 = width*smooth(depth2,dwdh)
                flwmdpt1 = smooth(depth1,dwdh)*
     +                     qcnst*(depth1**FIVE_THIRDS)
                flwmdpt2 = smooth(depth2,dwdh)*
     +                     qcnst*(depth2**FIVE_THIRDS)
                IF ( h.GT.sbot ) THEN
                   flobot1 = smooth(depth1,dwdh)*
     +                       cstr*((depth1+strtop)-h)
                   flobot2 = smooth(depth2,dwdh)*
     +                       cstr*((depth2+strtop)-h)
                ELSE IF ( icalccheck.EQ.1 ) THEN
                  flobot1 = CALCUNSATFLOBOT(depth1, avhc, fks,
     +                        width1, sbdthk, areamax, strlen, fbcheck,
     +                        NWAVST(:,l), maxwav, FOLDFLBT(lfold))
                  flobot2 = CALCUNSATFLOBOT(depth2, avhc, fks,
     +                        width2, sbdthk, areamax, strlen, fbcheck,
     +                        NWAVST(:,l), maxwav, FOLDFLBT(lfold))
                ELSE
                  flobot1 = smooth(depth1,dwdh)*
     +                      cstr*((depth1+strtop)-sbot)
                  flobot2 = smooth(depth2,dwdh)*
     +                      cstr*((depth2+strtop)-sbot)
                END IF
C                
C42-----USE BISECTION WHEN FLOBOT1 IS LIMITED BY FLOW IN CHANNEL.
                IF ( flobot1.GE.flowc ) THEN
                  enpt2 = depthp
                  depthp = (enpt1+enpt2)*0.5D0
                  IF ( h.GT.sbot ) THEN
                    flobotp = smooth(depthp,dwdh)*
     +                        cstr*((depthp+strtop)-h)
                  ELSE
                    flobotp = smooth(depthp,dwdh)*
     +                        cstr*((depthp+strtop)-sbot)
                  END IF
                  IF ( 0.5D0*flobotp.GT.flwmpt ) flobotp = flowc
! might be issue here using depthp for smoothing
                  depthx = ((flwmpt-0.5D0*flobotp)/
     +                     (smooth(depthp,dwdh)*qcnst))**0.6D0
                  ibflg = 1
                ELSE
                  fhstr1 = (flwmpt-0.5D0*flobot1) - (flwmdpt1)
                  fhstr2 = (flwmpt-0.5D0*flobot2) - (flwmdpt2)
                END IF
              ELSE IF ( icalc.GE.2 ) THEN
C
C43-----CALCULATE NEWTON VARIABLES FOR ICALC EQUAL TO 2.
                IF ( icalc.EQ.2 ) THEN
                  deptha = depth1 - (deps*depth1)
                  IF ( deptha.LT.NEARZERO ) deptha = depth1
                  depthb = depth1 + (deps*depth1)
                  IF ( depthb.LT.NEARZERO ) depthb = deps
                  depth2 = depth1 + (2.D0*deps)
                  depthc = depth2 - (deps*depth2)
                  IF ( depthc.LT.NEARZERO ) depthc = depth2
                  depthd = depth2 + (deps*depth2)
                  IF ( depthd.LT.NEARZERO ) depthd = deps
                  CALL GWF2SFR7FLW(deptha, istsg, roughch, 
     +                             roughbnk, slope, wetperma, 
     +                             flwmdpta, widtha)
                  CALL GWF2SFR7FLW(depthb, istsg, roughch, 
     +                             roughbnk, slope, wetpermb, 
     +                             flwmdptb, widthb)
                  CALL GWF2SFR7FLW(depthc, istsg, roughch, 
     +                             roughbnk, slope, wetpermc, 
     +                             flwmdptc, widthc)
                  CALL GWF2SFR7FLW(depthd, istsg, roughch, 
     +                             roughbnk, slope, wetpermd, 
     +                             flwmdptd, widthd)
                  CALL GWF2SFR7FLW(depth1, istsg, roughch, 
     +                             roughbnk, slope, wetperm1, 
     +                             flwmdpt1, width1)
                  CALL GWF2SFR7FLW(depth2, istsg, roughch, 
     +                             roughbnk, slope, wetperm2, 
     +                             flwmdpt2, width2)
C
C44-----CALCULATE NEWTON VARIABLES FOR ICALC EQUAL TO 3.
                ELSE IF ( icalc.EQ.3 ) THEN
                  flwmdpt1 = (depth1/cdpth)**(1.0/fdpth)
                  flwmdpt2 = (depth2/cdpth)**(1.0/fdpth)
                  IF ( flwmdpt1.GT.NEARZERO ) THEN
                    width1 = awdth*(flwmdpt1**bwdth)
                    wetperm1 = width1
                  ELSE
                    width1 = 0.0D0
                    wetperm1 = 1.0D0
                  END IF
                  IF ( flwmpt2.GT.NEARZERO ) THEN
                    width2 = awdth*(flwmdpt2**bwdth)
                    wetperm2 = width2
                  ELSE
                    width2 = 0.0D0
                    wetperm2 = 1.0D0
                  END IF
                  deptha = depth1 - (deps*depth1)
                  IF ( deptha.LT.NEARZERO ) deptha = depth1
                  depthb = depth1 + (deps*depth1)
                  IF ( depthb.LT.NEARZERO ) depthb = deps
                  depth2 = depth1 + (2*deps)
                  depthc = depth2 - (deps*depth2)
                  IF ( depthc.LT.NEARZERO ) depthc = depth2
                  depthd = depth2 + (deps*depth2)
                  IF ( depthd.LT.NEARZERO ) depthd = deps
                  flwmdpta = (deptha/cdpth)**(1.0/fdpth)
                  flwmdptb = (depthb/cdpth)**(1.0/fdpth)
                  flwmdptc = (depthc/cdpth)**(1.0/fdpth)
                  flwmdptd = (depthd/cdpth)**(1.0/fdpth)
                  widtha = awdth*(flwmdpta**bwdth)
                  widthb = awdth*(flwmdptb**bwdth)
                  widthc = awdth*(flwmdptc**bwdth)
                  widthd = awdth*(flwmdptd**bwdth)
                  wetperma = widtha
                  wetpermb = widthb
                  wetpermc = widthc
                  wetpermd = widthd
C
C45-----CALCULATE NEWTON VARIABLES FOR ICALC EQUAL TO 4.
                ELSE IF ( icalc.EQ.4 ) THEN
                  deptha = depth1 - (deps*depth1)
                  IF ( deptha.LT.NEARZERO ) deptha = depth1
                  depthb = depth1 + (deps*depth1)
                  IF ( depthb.LT.NEARZERO ) depthb = deps
                  depthc = depth2 - (deps*depth2)
                  IF ( depthc.LT.NEARZERO ) depthc = depth2
                  depthd = depth2 + (deps*depth2)
                  IF ( depthd.LT.NEARZERO ) depthd = deps
                  CALL GWF2SFR7TBF(flwmdpta, deptha, widtha, 
     +                         nstrpts, nreach, istsg, Kkiter, 0)
                  CALL GWF2SFR7TBF(flwmdptb, depthb, widthb, 
     +                         nstrpts, nreach, istsg, Kkiter, 0)
                  CALL GWF2SFR7TBF(flwmdptc, depthc, widthc, 
     +                         nstrpts, nreach, istsg, Kkiter, 0)
                  CALL GWF2SFR7TBF(flwmdptd, depthd, widthd, 
     +                         nstrpts, nreach, istsg, Kkiter, 0)
                  CALL GWF2SFR7TBF(flwmdpt1, depth1, width1, 
     +                         nstrpts, nreach, istsg, Kkiter, 0)
                  CALL GWF2SFR7TBF(flwmdpt2, depth2, width2, 
     +                         nstrpts, nreach, istsg, Kkiter, 0)
                  wetperma = widtha
                  wetpermb = widthb
                  wetpermc = widthc
                  wetpermd = widthd
                  wetperm1 = width1
                  wetperm2 = width2
                END IF
C
C46-----CALCULATE DERIVATIVES FOR DEPTH DEPENDENT VARIABLES FOR ICALC
C         GREATER THAN 1.
                dlpp1 = (precip*(widtha-widthb))/(deptha-depthb)
                dlpp2 = (precip*(widthc-widthd))/(depthc-depthd)
                dlet1 = (etstr*(widtha-widthb))/(deptha-depthb)
                dlet2 = (etstr*(widthc-widthd))/(depthc-depthd)
                dlwp1 = (wetperma-wetpermb)/(deptha-depthb)
                dlwp2 = (wetpermc-wetpermd)/(depthc-depthd)
Cdep revised pp1,pp2,et1,and et2, wrong placement of parenthesis.
                pp1 = precip*(width1)+dlpp1*dlh
                pp2 = precip*(width2)+dlpp2*dlh
                et1 = etstr*(width1)+dlet1*dlh
                et2 = etstr*(width2)+dlet2*dlh
                cstr1 = ((wetperm1+dlwp1*dlh)*strleak)/sbdthk
                cstr2 = ((wetperm2+dlwp2*dlh)*strleak)/sbdthk
C
C47-----CALCULATE FLOBOT1 AND FLOBOT2 WHEN ICALC GREATER THAN 1.
                IF ( h.GE.sbot ) THEN
                  flobot1 = cstr1*((depth1+strtop)-h)
                  flobot2 = cstr2*((depth2+strtop)-h)
                ELSE IF ( icalccheck.EQ.1 ) THEN
                  flobot1 = CALCUNSATFLOBOT(depth1, avhc, fks,
     +                        wetperm1, sbdthk, areamax, strlen,
     +               fbcheck, NWAVST(:,l), maxwav, FOLDFLBT(lfold))
                  flobot2 = CALCUNSATFLOBOT(depth2, avhc, fks,
     +                        wetperm2, sbdthk, areamax, strlen,
     +                fbcheck, NWAVST(:,l), maxwav, FOLDFLBT(lfold))
                ELSE
                  flobot1 = cstr1*((depth1+strtop)-sbot)
                  flobot2 = cstr2*((depth2+strtop)-sbot)
                END IF
C
C48-----DETERMINE IF LEAKAGE LIMITED BY FLOW IN CHANNEL.
                IF ( width1.GT.NEARZERO ) THEN
Cdep revised flwpet1, wrong placement of parenthesis.
                  flwpet1 = precip*width1+(dlpp1*dlh)
     +                      - etstr*width1+(dlet1*dlh)
                ELSE
                  flwpet1 = (precip-etstr)
                  IF ( flwpet1.LT.NEARZERO ) flwpet1 = 0.0D0
                END IF
C
C49-----USE BISECTION WHEN LEAKAGE IS LIMITED BY FLOW IN CHANNEL.
                IF ( flobot1.GT.flowc+flwpet1 ) THEN
                  depthp = (enpt1+enpt2)*0.5D0
                  ibflg = 1
                  IF ( icalc.EQ.2 ) THEN
                    CALL GWF2SFR7FLW(depthp, istsg, roughch,
     +                               roughbnk, slope, wetpermp, 
     +                               flwx, widtha)
                  ELSE IF ( icalc.EQ.3 ) THEN
                    flwx = (depthp/cdpth)**(1.0/fdpth)
                    IF ( flwx.GT.NEARZERO ) THEN
                      widthp = awdth*(flwx**bwdth)
                      wetpermp = widthp
                    ELSE
                      widthp = 0.0D0
                      wetpermp = 1.0D0
                    END IF
                  ELSE IF ( icalc.EQ.4 ) THEN
                    CALL GWF2SFR7TBF(flwx, depthp, widthp,
     +                          nstrpts, nreach, istsg, Kkiter, 0)
                    wetpermp = widthp
                  END IF
                  cstr1 = wetpermp*strleak/sbdthk
                  IF ( h.GT.sbot ) THEN
                    flobotp = cstr1*((depthp+strtop)-h)
                  ELSE IF ( icalccheck.EQ.1 ) THEN
                    flobotp = CALCUNSATFLOBOT(depthp, avhc, fks,
     +                          wetpermp, sbdthk, areamax, strlen, 
     +                          fbcheck, NWAVST(:,l), maxwav,
     +                          FOLDFLBT(lfold))
                  ELSE
                    flobotp = cstr1*((depthp+strtop)-sbot)
                  END IF
                  IF ( flobotp.GT.flowc+flwpet1 ) flobotp = flowc
     +                                                      + flwpet1
                  flwmpt = flwmpt + 0.5D0*flwpet1
                  flwx = flwmpt - 0.5D0*flobotp
                  IF ( flwx.LT.NEARZERO ) THEN
                    depthx = 0.0D0
                    widthx = 0.0D0
                    wetpermx = 1.0D0
                  ELSE IF ( icalc.EQ.2 ) THEN
                    CALL GWF2SFR7DPTH(flwx, slope, istsg, nreach, 
     +                                roughch, roughbnk, wetpermp, 
     +                                depthx, itstr, widthp,iprndpth)
                  ELSE IF ( icalc.EQ.3 ) THEN
                    depthx = cdpth*(flwx**fdpth)
Crgn changed widthp to widthx 12/5/06
                    widthx = awdth*(flwx**bwdth)
                    wetpermx = widthx
                  ELSE IF ( icalc.EQ.4 ) THEN
                    CALL GWF2SFR7TBD(flwx, depthx, widthx,
     +                               nstrpts, istsg)
                    wetpermx = widthx
                  END IF
C
C50-----CALCULATE LEAKAGE FROM NEWTON METHOD WHEN NOT LIMITED BY FLOW.
                ELSE
                  fhstr1 = (flwmpt-0.5D0*(pp1-et1+flobot1)) - (flwmdpt1)
                  fhstr2 = (flwmpt-0.5D0*(pp2-et2+flobot2)) - (flwmdpt2)
                END IF
                IF ( depthp.LE.dbleak*0.000001D0 ) THEN
                  depthx = depthp
                  flobotp = 0.0D0
                  widthx = 0.0D0
                  wetpermx = 1.0D0
                  iflg = 0
                END IF
              END IF
              IF ( ibflg.EQ.0 ) THEN
                dlfh = 0.0D0
                IF ( ABS( depth1-depth2 ).GT.0.0D0 ) THEN
                  dlfh = (fhstr1-fhstr2)/(depth1-depth2)
                END IF
                IF ( DABS(dlfh).GT.NEARZERO ) THEN
                  dlh = -fhstr1/dlfh
                ELSE
                  dlh = 0.0D0
                END IF
                depthp = depth1 + dlh
                IF ( iflg.GT.0 ) THEN
                  IF ( (depthp.GE.enpt2) .OR. (depthp.LE.enpt1) ) THEN
                    IF ( DABS(dlh).GT.DABS(dlhold) .OR.
     +                   depthp.LT.NEARZERO ) THEN
                      depthp = (enpt1+enpt2)*0.5D0
                      ibflg = 1
                    END IF
                  END IF
C
C51-----SET FLAGS TO DETERMINE IF NEWTON METHOD OSCILLATES OR 
C         IF CONVERGENCE IS SLOW.
                  IF ( flobot1*flobotold.LT.NEARZERO ) THEN
                    iic2 = iic2 + 1
                  ELSE
                    iic2 = 0
                  END IF
                  IF ( flobot1.LT.NEARZERO ) THEN
                    iic3 = iic3 + 1
                  ELSE
                    iic3 = 0
                  END IF
                  IF ( dlh*dlhold.LT.NEARZERO .OR.
     +                 DABS(dlh).GT.DABS(dlhold) ) iic = iic + 1
                  iic4 = 0
                  IF ( iic3.GT.7 .AND. iic.GT.12 ) iic4 = 1
C
C52-----SWITCH TO BISECTION WHEN NEWTON METHOD OSCILLATES OR WHEN
C         CONVERGENCE IS SLOW.
                  IF ( iic2.GT.7 .OR. iic.GT.12 .OR. iic4.EQ.1 ) THEN
                    ibflg = 1
                    depthp = (enpt1+enpt2)*0.5D0
                  END IF
C
C53-----COMPUTE FLOBOTP ON BASIS OF DEPTHP AND THEN ESTIMATE DEPTHX FROM
C         FLOBOTP.
                  IF ( icalc.EQ.1 ) THEN
                    IF ( h.GT.sbot ) THEN
! RGN smooth width 10/4/11
                      flobotp = smooth(depthp,dwdh)*
     +                          cstr*(strtop+depthp-h)
                    ELSE IF ( icalccheck.EQ.1 ) THEN
! RGN smooth width 10/4/11
                      widthp = smooth(depthp,dwdh)*width
                      flobotp = CALCUNSATFLOBOT(depthp, avhc, fks, 
     +                            widthp, sbdthk, areamax, strlen,
     +                            fbcheck, NWAVST(:,l), maxwav, 
     +                            FOLDFLBT(lfold))
                    ELSE
                      flobotp = (smooth(depthp,dwdh)*cstr*
     +                          (strtop+depthp-sbot))
                    END IF
                    IF ( flobotp.GE.flowc ) THEN
                      flobotp = flowc
                      IF ( DABS(enpt1-enpt2).LE.dbleak*0.000001D0 )
! RGN smooth width 10/4/11
     +                     depthp = ((flwmpt-0.5D0*flobotp)/
     +                              (smooth(depthp,dwdh)*qcnst))**.6D0
                    END IF
                    depthx = ((flwmpt-0.5D0*flobotp)/
     +                       (smooth(depthp,dwdh)*qcnst))**0.6D0
                  ELSE IF ( icalc.GE.2 ) THEN
                    IF ( icalc.EQ.2 ) THEN
                      CALL GWF2SFR7FLW(depthp, istsg, roughch, roughbnk,
     +                                 slope, wetpermp, flwp, widthp)
                    ELSE IF ( icalc.EQ.3 ) THEN
                      flwp = (depthp/cdpth)**(1.0/fdpth)
                      IF ( flwp.GT.NEARZERO ) THEN
                        widthp = awdth*(flwp**bwdth)
                        wetpermp = widthp
                      ELSE
                        widthp = 0.0D0
                        wetpermp = 1.0D0
                      END IF
                    ELSE IF ( icalc.EQ.4 ) THEN
                      CALL GWF2SFR7TBF(flwp, depthp, widthp, 
     +                                 nstrpts, nreach, istsg,
     +                                 Kkiter, 0)
                      wetpermp = widthp
                    END IF
                    IF ( widthp.GT.NEARZERO ) THEN
                      flwpetp = (precip-etstr)*widthp
                    ELSE
                      flwpetp = (precip-etstr)
                      IF ( flwpetp.LT.NEARZERO ) flwpetp = 0.0D0
                    END IF
                    IF ( h.GT.sbot ) THEN
                      flobotp = ((avhc*wetpermp*strlen/sbdthk)
     +                          *(strtop+depthp-h))
                    ELSE IF ( icalccheck.EQ.1 ) THEN
                      flobotp = CALCUNSATFLOBOT(depthp, avhc, fks,
     +                            wetpermp, sbdthk, areamax, strlen,
     +                            fbcheck, NWAVST(:,l), maxwav,
     +                            FOLDFLBT(lfold))
                    ELSE
                      flobotp = ((avhc*wetpermp*strlen/sbdthk)
     +                          *(strtop+depthp-sbot))
                    END IF
                    flwmpt = flwmpt + 0.5D0*flwpetp
                    flwx = flwmpt - 0.5D0*flobotp
C
C54-----USE BISECTION WHEN LEAKAGE LIMITED BY FLOW FOR ICALC GREATER 
C         OR EQUAL TO 2.
                    IF ( flobotp.GT.flowc+flwpetp ) THEN
                      depthp = (enpt1+enpt2)*0.5D0
                      ibflg = 1
                      IF ( icalc.EQ.2 ) THEN
                        CALL GWF2SFR7FLW(depthp, istsg, roughch,
     +                                   roughbnk, slope, wetpermp,
     +                                   flwp, widthp)
                      ELSE IF ( icalc.EQ.3 ) THEN
                        flwp = (depthp/cdpth)**(1.0/fdpth)
                        IF ( flwp.GT.NEARZERO ) THEN
                          widthp = awdth*(flwp**bwdth)
                          wetpermp = widthp
                        ELSE
                          widthp = 0.0D0
                          wetpermp = 1.0D0
                        END IF
                      ELSE IF ( icalc.EQ.4 ) THEN
                        CALL GWF2SFR7TBF(flwp, depthp, widthp, 
     +                                   nstrpts, nreach, istsg, 
     +                                   Kkiter, 0)
                        wetpermp = widthp
                      END IF
                      IF ( widthp.GT.NEARZERO ) THEN
                        flwpetp = (precip-etstr)*widthp
                      ELSE
                        flwpetp = (precip-etstr)
                        IF ( flwpetp.LT.NEARZERO ) flwpetp = 0.0D0
                      END IF
                      IF ( h.GT.sbot ) THEN
                        flobotp = ((avhc*wetpermp*strlen/sbdthk)
     +                            *(strtop+depthp-h))
                      ELSE IF ( icalccheck.EQ.1 ) THEN
                        flobotp = CALCUNSATFLOBOT(depthp, avhc, fks, 
     +                              wetpermp, sbdthk, areamax, strlen, 
     +                              fbcheck, NWAVST(:,l), maxwav,
     +                              FOLDFLBT(lfold))
                      ELSE
                        flobotp = ((avhc*wetpermp*strlen/sbdthk)
     +                            *(strtop+depthp-sbot))
                      END IF
                      flwmpt = flwmpt + 0.5D0*flwpetp
                      flwx = flwmpt - 0.5D0*flobotp
                      IF ( flobotp.GE.flowc+flwpetp ) flobotp = flowc + 
     +                                                          flwpetp
                    END IF
C
C55-----ESTIMATE DEPTHX WHEN ICALC IS GREATER THAN OR EQUAL TO 2.
                    IF ( flwx.LT.NEARZERO ) THEN
                      depthx = 0.0D0
                      widthx = 0.0D0
                      wetpermx = 1.0D0
                    ELSE IF ( icalc.EQ.2 ) THEN
                      CALL GWF2SFR7DPTH(flwx, slope, istsg, nreach, 
     +                                  roughch, roughbnk, wetpermx, 
     +                                  depthx, itstr, widthx, iprndpth)
                    ELSE IF ( icalc.EQ.3 ) THEN
                      depthx = cdpth*(flwx**fdpth)
                      widthx = awdth*(flwx**bwdth)
                      wetpermx = widthx
                    ELSE IF ( icalc.EQ.4 ) THEN
                      CALL GWF2SFR7TBD(flwx, depthx, widthx, 
     +                                 nstrpts, istsg)
                      wetpermx = widthx
                    END IF
                  END IF
                END IF
              END IF
C
C56-----COMPUTE DIFFERENCE BETWEEN DEPTHP AND DEPTHX.
              fp = depthp - depthx
              IF ( ibflg.EQ.1 ) dlh = fp
C
C57-----CHANGE ENDPOINTS IF BISECTION HAS BEEN USED.
              IF ( ibflg.EQ.1 ) THEN
                IF ( f1*fp.LT.0.0D0 ) THEN
C
C58-----ROOT IS BETWEEN F1 AND FP.
                  enpt2 = depthp
                  f2 = fp
                ELSE
C
C59-----ROOT IS BETWEEN FP AND F2.
                  enpt1 = depthp
                  f1 = fp
C
C60-----CALCULATE ERROR.
                END IF
                err = MIN(DABS(fp), DABS(enpt2-enpt1))
              ELSE
                err = dlh
              END IF
C
C61-----SET DEPTH TO DEPTHP AND FLOBOT TO FLOBOTP WHEN IS ERROR LESS
C         THAN TOLERANCE.
              IF ( DABS(err).LT.dbleak ) THEN
                iflg = 0
                depth = depthp
                flobot = flobotp
                IF ( icalc.GE.2 ) THEN
                  width = widthp
                  wetperm = wetpermp
 !                 IF ( wetperm.GT.width ) width =  wetperm
                  flowc = flowin + runof + runoff - etstr*width + 
     +                    precip*width
                  flwmpt = flowin + 
     +                     0.5D0*(runof+runoff+precip*width-etstr*width-
     +                     flobot)
!dep  August 26, 2009  added ELSE to recalculate flwmpt for ICALC=0 or 1
                ELSE
                  flowc = flowin + runof + runoff - etstr +
     +                    precip
                  flwmpt = flowin +
     +                     0.5D0*(runof+runoff+precip-etstr-flobot) 
                END IF
              END IF
C
C62-----PRINT WARNING THAT REACH FAILED TO CONVERGE AFTER 100 ITERATIONS.
C         AND SET DEPTH TO DEPTHP AND FLOBOT TO FLOBOTP.
              IF ( itot.GE.100 ) THEN
                iflg = 0
                WRITE (IOUT, 9003) istsg, nreach, Kkiter, err, errold
 9003           FORMAT (//5X, '*** WARNING *** SFR FAILED TO ', 
     +                  'CONVERGE FOR SEGMENT', I7, ' REACH NO.', I7, 
     +                  ' MODFLOW ITERATION IS', I7, ' LAST ITERATION',
     +                  G21.10, ' PREVIOUS ITERATION', G21.10)
                depth = depthp
                flobot = flobotp
                IF ( icalc.GE.2 ) THEN
                  width = widthp
                  wetperm = wetpermp
                  flowc = flowin + runof + runoff - etstr*width + 
     +                    precip*width
                  flwmpt = flowin + 
     +                     0.5D0*(runof+runoff+precip*width-etstr*width-
     +                     flobot)
!dep  August 26, 2009  added ELSE to recalculate flwmpt for ICALC=0 or 1
                ELSE
                  flowc = flowin + runof + runoff - etstr + precip
                  flwmpt = flowin +
     +                     0.5D0*(runof + runoff + precip-etstr-flobot)
                END IF
              END IF
              errold = err
              dlhold = dlh
              flobotold = flobot1
              IF ( ibflg.EQ.1 ) flobotold = flobotp
              SFRQ(4, l) = width
C
C63-----END OF NEWTON LOOP.
            END DO
C
C64-----DEFINE HSTR, CSTR, WIDTH, AND FLOWOT.
            hstr = depth + STRM(3, l)
            IF ( icalc.GE.2 ) cstr = (avhc*wetperm*strlen)/sbdthk
          END IF
C
C65-----ROUTE FLOW WITHOUT ANY STREAM LEAKAGE (FLOBOT IS ZERO) WHEN
C          MODEL CELL IS INACTIVE. Revised dep 5/19/2005
          IF ( iskip.NE.0 .OR. itot.EQ.0 ) THEN
            IF ( icalc.EQ.0 .OR. icalc.EQ.1 ) THEN
              flowc = flowin + runof + runoff + (precip-etstr)
              flwmpt = flowin + 0.5D0*(runof+runoff+(precip-etstr))
!FLOW AT MIDPOINT CANNOT BE LESS THAN ZERO.
              IF( flwmpt.LT.NEARZERO ) flwmpt = 0.0D0
! RGN 10/3/07 added check for flwmpt to avoid NaN values.
              IF ( icalc.EQ.1 .AND. flwmpt.GT.NEARZERO ) THEN
                depth = (flwmpt/(smooth(depth,dwdh)*qcnst))**0.6D0
              ELSE
                depth = 0.0
              END IF
              IF ( flowc.GT.NEARZERO ) THEN
                IF ( runof.LT.NEARZERO ) THEN
                  IF ( flowin+runoff+precip.GT.NEARZERO ) THEN
                    IF ( flowin+runoff+precip-etstr.LE.-(runof) ) THEN
                      runof = -(flowin+runoff+precip-etstr)
                    ELSE IF ( flowin+runoff+precip+runof.LE.etstr ) THEN
                      etstr = flowin + runoff + precip + runof
                    END IF
                  END IF
                ELSE IF ( flowin+runoff+precip+runof.LE.etstr ) THEN
                  etstr = flowin + runof + runoff + precip - flobot
                ELSE IF ( flowin+runoff+precip+runof.LT.NEARZERO ) THEN
                  etstr = 0.0D0
                END IF
                flowc = flowin + runof + runoff + precip - etstr
              ELSE IF ( runof.LT.NEARZERO ) THEN
                IF ( flowin+runoff+precip.LT.NEARZERO ) THEN
                  runof = 0.0D0
                  etstr = 0.0D0
                ELSE IF ( flowin+runoff+precip-etstr.LE.-(runof) ) THEN
                  runof = -(flowin+runoff+precip-etstr)
                ELSE IF ( flowin+runoff+precip+runof.LE.etstr ) THEN
                  etstr = flowin + runoff + precip + runof
                END IF
              ELSE IF ( etstr.GE.flowin+runof+runoff+precip ) THEN
                etstr = flowin + runof + runoff + precip
              ELSE
                etstr = 0.0D0
              END IF
            ELSE IF ( icalc.GE.2 ) THEN
              depth = 0.0D0
              cstr = 0.0D0
crgn used width in calculations.
              IF ( icalc.EQ.2 ) THEN
                width = XSEC(6, istsg) - XSEC(3, istsg)
              ELSE IF ( icalc.EQ.3 ) THEN
                width = 10.0D0
              ELSE IF ( icalc.EQ.4 ) THEN
                width = QSTAGE((1+2*nstrpts), istsg)
     +                  + QSTAGE(3*nstrpts, istsg)/2.0D0
              END IF
              flowc = flowin + runoff  + runof + (precip-etstr)*width
!dep  August 26, 2009 fixed flwmpt calculation  not half of flowc
              flwmpt = flowin + 
     +                 0.5D0* (runoff+runof+(precip-etstr)*width)
              IF ( flowc.LT.NEARZERO ) THEN
                flowc = 0.0D0
                etstr = (flowin + runoff + runof)/width + precip
                flwmpt = 0.0D0
              END IF
            END IF
C
C66-----COMPUTE STREAM LEAKAGE IF STREAM DEPTH WAS NOT COMPUTED
C         USING EITHER BISECTION OR NEWTON METHOD. revised dep
            IF ( iskip.EQ.0 ) THEN
              cstr = strleak*width/sbdthk
crgn added next 4 lines.
              IF (icalc.GT.1 ) THEN
                etstr = etstr*width
                precip = precip*width
              END IF
              IF ( flowc.LT.NEARZERO ) THEN
                hstr = STRM(3, l)
                IF ( h.LT.hstr ) THEN
                  flobot = 0.0D0
                ELSE
                  flobot = cstr*(hstr-h)
                  IF ( runof.LT.NEARZERO ) THEN
                    IF ( flowin+runoff+precip-flobot.LT.NEARZERO ) THEN
                      runof = 0.0D0
                      etstr = 0.0D0
                    ELSE IF ( flowin+runoff+precip-flobot-etstr.LE.
     +                        -runof ) THEN
                      runof = -(flowin+runoff+precip-flobot-etstr)
                    ELSE IF ( flowin+runoff+precip-flobot+runof.LE.
     +                        etstr ) THEN
                      etstr = flowin + runoff + precip - flobot + runof
                    END IF
                  ELSE IF ( flowin+runoff+runof+precip-flobot.LE.etstr )
     +                      THEN
                    etstr = flowin + runof + runoff + precip - flobot
                  ELSE IF ( flowin+runoff+runof+precip-flobot.LT.
     +                      NEARZERO ) THEN
                    etstr = 0.0D0
                  END IF
                END IF
                flowc = flowin + runof + runoff + precip - etstr
              ELSE IF ( h.LT.sbot ) THEN
                flobot = cstr*(hstr-sbot)
                IF ( icalc.EQ.1 ) flobot = smooth(depth,dwdh)*flobot
              ELSE
                flobot = cstr*(hstr-h)
                IF ( icalc.EQ.1 ) flobot = smooth(depth,dwdh)*flobot
              END IF
            ELSE
              flobot = 0.0D0
            END IF
          END IF
          IF ( flowc.GT.NEARZERO .AND. icalccheck.EQ.1 ) THEN
            IF ( h.LT.sbot ) THEN
              wetpermsmooth = wetperm*smooth(depth,dwdh)
              flobot = CALCUNSATFLOBOT(depth, avhc, fks, wetpermsmooth,
     +                 sbdthk, areamax, strlen, fbcheck, NWAVST(:,l),
     +                 maxwav, FOLDFLBT(lfold))
            ELSE
              DO i = 1, ISUZN
                UZSEEP(i, l) = 0.0D0
              END DO
            END IF
          ELSE IF ( icalccheck.EQ.1 ) THEN
            DO i = 1, ISUZN
              UZSEEP(i, l) = 0.0D0
            END DO
          END IF
          IF ( flobot.GE.flowc ) flobot = flowc
C          
C67-----ROUTE FLOW DOWN CHANNEL WHEN ACTIVE.
          IF ( IRTFLG.NE.0 .AND. iss.EQ.0 ) THEN
            qlat = (runof + runoff + precip - etstr)/strlen
            qa = STRM(28,l)
            qb = STRM(27,l)
            IF ( ABS(TOTIM-DELT).LT.1.0E-10 ) qb = 0.0
            IF ( ABS(TOTIM-DELT).LT.1.0E-10 ) qa = 0.0
            qc = flowin
            fltest = 0.0
            IF (  flobot.LT.0.0D0 ) THEN
              fltest=qa+qb+qc+qlat*strlen-flobot
            ELSE
              fltest=qa+qb+qc+qlat*strlen
            END IF
            IF ( fltest.GT.NEARZERO ) THEN
              CALL ROUTE_CHAN(qa, qb, qc, qd, qcnst, cdpth, awdth, 
     +               fdpth, bwdth, deltinc, icalc, strlen, slope, 
     +               istsg, nreach, itstr, qlat, flobot, width, l,
     +               Transient_bd, depthtr)
            ELSE 
              qd = 0.0
            END IF
            STRM(28,l) = qc
            STRM(27,l) = qd
            flowot = qd
            flowc = qc
            QSTRM(l,irt) = (qc + qd)/2.0
            STRMDELSTOR_RATE = STRMDELSTOR_RATE + 
     +                         (qc + qd)/2.0 - (qa + qb)/2.0
            STRMDELSTOR_CUM = STRMDELSTOR_CUM + 
     +                         ((qc + qd)/2.0 - (qa + qb)/2.0)*deltinc
          ELSE
C
C68-----STREAMFLOW OUT EQUALS STREAMFLOW IN MINUS LEAKAGE.
            flowot = flowc - flobot
            qc = flowc
            qd = flowot
            IF ( flowot.LT.NEARZERO ) THEN
              flowot = 0.0D0
              flobot = flowc
              flwmpt = 0.5D0*flowc
              depth = 0.0D0
            END IF
          END IF
! rgn 2/12/08 to address bug found by Arlen for SS dependency on DELT
          IF ( ISS.EQ.0 ) THEN
            SUMLEAK(l) = SUMLEAK(l) + (flobot*deltinc)
            IF ( irt.EQ.numdelt) SUMLEAK(l) = SUMLEAK(l)/DELT
          ELSE
            SUMLEAK(l) = flobot
          END IF
C
C
C69-----STORE STREAM INFLOW, OUTFLOW, LEAKAGE, STAGE, AND STREAMBED
C          CONDUCTANCE FOR EACH REACH.
            STRM(9, l) = flowot
            STRM(10, l) = flowin
            STRM(11, l) = flobot
            STRM(15, l) = hstr
            STRM(16, l) = cstr
            IF ( icalc.GE.1 ) STRM(7, l) = depth
            IF ( icalc.GE.2 ) STRM(5, l) = width
            IF ( icalc.GE.2 ) STRM(20, l) = wetperm
          HSTRM(l,irt) = hstr
          HWDTH(l,irt) = width
          HWTPRM(l,irt) = wetperm
C70-----STORE OUTFLOW FROM LAST REACH IN LAST SEGMENT IN STRIN
C          LIST FOR LAKE PACKAGE.
          IF ( Iunitlak.GT.0 ) THEN
            IF ( l.EQ.NSTRM .AND. istsg.EQ.NSS ) STRIN(istsg) = flowot
          END IF
C
C71-----STORE OUTFLOW FOR LAST SEGMENT IN SGOTFLW LIST AND IN
C         STRIN FOR LAKE PACKAGE.
          IF ( l.EQ.NSTRM ) THEN
            SGOTFLW(istsg) = STRM(9, l)
            IF ( Iunitlak.GT.0 ) STRIN(istsg) = STRM(9, l)
          END IF
          IF ( irt.EQ.numdelt ) THEN
C
C72-----CALCULATE SEEPAGE THROUGH UNSATURATED ZONE.
            IF ( h.LT.sbot .AND. icalccheck.EQ.1 ) THEN
              fbot = SUMLEAK(l)
              CALL CALC_UNSAT_INFIL(fbot, UZSEEP(:,l), 
     +                               UZTHST(:,l),thr, ha, thetas, 
     +                               epsilon, fks,avhc, depth, sbdthk, 
     +                               WETPER(:,l), UZWDTH(:,l), flowc,
     +                               NWAVST(:,l),strlen, iwidthcheck,
     +                               icalc)
              IF ( ICALC.EQ.1 ) flobot = UZSEEP(1,l)*UZWDTH(1,l)*strlen
            END IF
C
C73-----ROUTE SEEPAGE THROUGH UNSATURATED ZONE.
            kerp = 0
            IF ( h.LT.sbot .OR. hld.LT.sbot ) kerp = 1
            IF ( kerp.EQ.1 .AND. iss.EQ.0 .AND. icalccheck.EQ.1 ) THEN
              CALL ROUTWAVESIT(lsub, seep, h, hld, thr, thetas, fks, 
     +                       epsilon, icalc, NWAVST(:,l), UZWDTH(:,l), 
     +                       UZFLWT(:,l), UZOLSFLX(:,l), UZSEEP(:,l), 
     +                       ITRLST(:,l), LTRLST(:,l), UZSPST(:,l), 
     +                       UZFLST(:,l), UZDPST(:,l), UZTHST(:,l), 
     +                       ITRLIT(:,l), LTRLIT(:,l), UZSPIT(:,l), 
     +                       UZFLIT(:,l), UZDPIT(:,l), UZTHIT(:,l),  
     +                       DELT, Sbot)
C
C74-----SUM SEEPAGE TO WATER TABLE.
              totflwt = 0.0D0
              IF ( icalc.EQ.2 ) THEN
                DO i = 1, ISUZN
                  totflwt = totflwt + UZFLWT(i, l)
                END DO
              ELSE IF ( icalc.EQ.1 ) THEN
                totflwt = UZFLWT(1, l)
              END IF
              SUMRCH(l) = SUMRCH(l) + totflwt/DELT
            END IF
            IF ( icalccheck.EQ.0 ) THEN 
              IF ( h.LT.sbot )
     +            SUMRCH(l) = SUMRCH(l) + flobot
            ELSE IF ( iss.NE.0 ) THEN
              IF ( h.LT.sbot )
     +          SUMRCH(l) = SUMRCH(l) + flobot
            END IF
          END IF
C
C75-----STORE FLOWS NEEDED FOR SENSITIVITIES. - ERB
            IF ( IRTFLG.EQ.0 ) THEN
              SFRQ(1, l) = flwmpt
              SFRQ(2, l) = flowc
              SFRQ(3, l) = flobot
              SFRQ(5, l) = flowin
            ELSE
              SFRQ(1, l) = (qc + qd)/2.0
              SFRQ(2, l) = (qc + qd)/2.0
              SFRQ(3, l) = flobot
              SFRQ(5, l) = qc
            END IF
C
C76-----ADD TERMS TO RHS AND HCOF IF FLOBOT IS NOT ZERO.
          IF ( irt.EQ.numdelt ) THEN
            hstrave = 0.0D0
            DO i = 1, numdelt
              hstrave = hstrave + HSTRM(l,i)
            END DO
            hstrave = hstrave/FLOAT(numdelt)
            cstrsmooth = cstr
            IF ( icalc.EQ.1 ) cstrsmooth = cstr*
     +                        smooth(hstrave,dwdh)
            IF ( ABS(SUMLEAK(l)).GT.0.0 ) THEN
C
C77-----ADD TERMS TO RHS AND HCOF WHEN GROUND-WATER HEAD LESS THAN
C         STREAMBED BOTTOM ELEVATION.
              IF ( h.LT.sbot ) THEN
                 RHS(ic, ir, il) = RHS(ic, ir, il) - SUMRCH(l)        
!       fin=fin+sumrch(l)
cdep  changed dbleak to -CLOSEZERO
              ELSE IF ( SUMLEAK(l)-flowc.LT.-CLOSEZERO ) THEN
C
C78-----STREAM LEAKAGE IS NOT HEAD DEPENDENT.
                IF ( iss.EQ.0 ) THEN        
                    RHS(ic, ir, il) = RHS(ic, ir, il) - 
     +                                (cstrsmooth*hstrave)- SUMRCH(l)
!      if (hstrave.gt.h)fin=fin+sumrch(l)+cstrsmooth*(hstrave-h)
!      if( hstrave.lt.h)then
!       fout=fout+cstrsmooth*(hstrave-h)
!       fin=fin+sumrch(l)
!      end if
                ELSE
                    RHS(ic, ir, il) = RHS(ic, ir, il) - 
     +                           (cstrsmooth*hstrave)
!      if (hstrave.gt.h)then
!      fin=fin+cstrsmooth*(hstrave-h)
!      else
!      fout=fout+cstrsmooth*(hstrave-h)
!      end if
                END IF
                HCOF(ic, ir, il) = HCOF(ic, ir, il) - cstrsmooth
              ELSE
C
C79-----CONSTANT STREAMBED LEAKAGE IS LIMITED BY STREAMFLOW OR 
C         STREAMBED CONDUCTANCE IN REACH.
                IF ( iss.EQ.0 ) THEN
                    RHS(ic, ir, il) = RHS(ic, ir, il) 
     +                              - SUMLEAK(l)- SUMRCH(l)
 !     fin=fin+sumleak(l)+sumrch(l)
                ELSE
                  RHS(ic, ir, il) = RHS(ic, ir, il) - SUMLEAK(l)
!       fin=fin+sumleak(l)
                END IF
C
C80-----ADD TERM ONLY TO RHS WHEN GROUND-WATER HEAD IS LESS THAN
C         STREAMBED BOTTOM ELEVATION.
              END IF
            ELSE IF ( h.LT.sbot .OR. hld.LT.sbot ) THEN
              RHS(ic, ir, il) = RHS(ic, ir, il) - SUMRCH(l)
 !      fin=fin+sumrch(l)
            END IF
          END IF
!      write(iout,*)'in fm',l,fin,fout
        END DO !rsr, end l = 1, NSTRM loop
C        
C81-----END INTERNAL TIME LOOP FOR ROUTING STREAMFLOWS.
      END DO !rsr, end irt loop
C
C82-----RETURN.
      RETURN
      END SUBROUTINE GWF2SFR7FM
C
C-------SUBROUTINE GWF2SFR7BD
      SUBROUTINE GWF2SFR7BD(Kkstp, Kkper, Iunitgwt, Iunitlak, Iunitgage,
     +                      Iunituzf, Nsol, Iunitrch, Igrid)  !cjm (added Iunitrch)
C     *****************************************************************
C     CALCULATE VOLUMETRIC GROUND-WATER BUDGET FOR STREAMS AND SUM
C     STREAMFLOWS IN MODELED AREA
!--------REVISED FOR MODFLOW-2005 RELEASE 1.9, FEBRUARY 6, 2012
C     *****************************************************************
      USE GWFSFRMODULE
      USE GWFLAKMODULE, ONLY: LKARR1, STGNEW
!!      USE GWFLAKMODULE, ONLY: VOL, LKARR1, STGNEW, STGOLD
      USE GLOBAL,       ONLY: NCOL, NROW, NLAY, IOUT, ISSFLG, IBOUND,
     +                        HNEW, BUFF, BOTM, LBOTM, IUNIT
!!      USE GLOBAL,       ONLY: NCOL, NROW, NLAY, IOUT, ISSFLG, IBOUND,
!!     +                        HNEW, BUFF, BOTM, LBOTM, DELR, DELC
!IFACE
      USE GWFBASMODULE, ONLY: MSUM, ICBCFL, IBUDFL, DELT, PERTIM, TOTIM,
     +                        VBVL, VBNM, HDRY, IAUXSV
!      USE GWFRCHMODULE,ONLY:RECH  !cjm
!!      USE GWFUZFMODULE,ONLY:FINF  !cjm
      IMPLICIT NONE
      INTRINSIC FLOAT, ABS, IABS, DSQRT, DLOG10, SQRT, SNGL
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     FUNCTIONS
C     ------------------------------------------------------------------
      REAL CALCUNSATFLOBOT
      EXTERNAL CALCUNSATFLOBOT
      DOUBLE PRECISION SMOOTH
      EXTERNAL SMOOTH
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER Kkstp, Kkper, Iunitgwt, Iunitlak, Iunitgage, Iunituzf, 
     +        Iunitrch  !cjm (added Iunitrch)  
      INTEGER Igrid, Nsol
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      REAL areamax, avhc, fks, ha, rin, rout, strlen,
     +     zero, sfrbudg_in, sfrbudg_out, qlat, deltinc, qcnst, rtime,
     +     fltest, Transient_bd, Transient_bd_tot !!, dvt  !cjm (added dvt)
!IFACE
      REAL xface(1)
      INTEGER naux
      INTEGER i, ibd, iblst, ibdlbl, ibdst, ibstlb, ic, icalc, idivseg, 
     +        il, ilay, iout1, iout2, iprior, iprvsg, ir, istsg, itrib,
     +        iwidthcheck, kss, l, lk, ll, nreach, numdelt, maxwav,
     +        icalccheck, iss, lsub, irt, itstr, imassroute, lfold
      INTEGER illake, LAKID
!!      INTEGER irr, icc, icount  !cjm
      DOUBLE PRECISION h, hstr, sbot, cstr, ratin, ratout, flowin,
     +                 flobot, flow, flowot, sbdthk, upflw, trbflw,
     +                 width, wetperm, runof, runoff, precip, etstr,
     +                 slope, cdpth, fdpth, hdiff, grad, depth,
     +                 hld, fbcheck, totflwt, totdelstor, totuzstor,
     +                 thetas, epsilon, thr, qa, qb, qc, qd, awdth,
     +                 bwdth, gwflow, dvrsn, fbot, depthtr, strtop,
     +                 dwdh, fact
      EXTERNAL CALC_XSA
      DOUBLE PRECISION CALC_XSA
C     ------------------------------------------------------------------
C     LOCAL STATIC VARIABLES
C     ------------------------------------------------------------------
      CHARACTER*16 text, strtxt, txtlst
      DATA text/'  STREAM LEAKAGE'/
      DATA strtxt/'STREAMFLOW OUT  '/
      DATA txtlst/'STREAM LISTING  '/
      DATA iwidthcheck/1/
C     -----------------------------------------------------------------
C
C-------SET POINTERS FOR THE CURRENT GRID.
      CALL SGWF2SFR7PNT(Igrid)
C
C1------INITIALIZE CELL BY CELL FLOW TERM FLAG (IBD) AND
C         ACCUMULATORS (RATIN AND RATOUT).
      iss = ISSFLG(Kkper)
      ibd = 0
      ibdst = 0
      iblst = 0
      zero = 0.
      ratin = zero
      ratout = zero
      sfrbudg_in = zero
      sfrbudg_out = zero
      numdelt = NUMTIM
      itstr = 0
      STRMDELSTOR_RATE = 0.0E0
      SFRRATIN = zero
      SFRRATOUT = zero
      TOTSPFLOW = 0.0D0
      Transient_bd_tot = 0.0
      Transient_bd = 0.0
      lfold = 0
      fact = FACTOR
      maxwav = NSFRSETS*NSTRAIL
      IF(IUNIT(49).GT.0) THEN  !IUNIT(49): LMT
        NINTOT = 0              
      ENDIF
      IF ( IUZT.EQ.1 ) THEN
        SFRUZBD(4) = zero
        SFRUZBD(5) = zero
        SFRUZBD(6) = zero
        SFRUZBD(7) = zero
        SFRUZBD(8) = zero
        SFRUZBD(9) = zero
        SFRUZBD(10) = zero
      END IF
C
C1b-----PRINT STREAM RESULTS WHENEVER BUDGET TERMS ARE PRINTED.
Cdep    revised to allow for compact and non compact budgets
      IF ( ISTCB1.EQ.-1 .AND. ICBCFL.NE.0 ) THEN
        ibd = -1
        iblst = -1
      ELSE IF ( ISTCB1.EQ.-1 .AND. IBUDFL.GT.0 ) THEN
        ibd = -1
        iblst = -1
      ELSE IF ( ISTCB1.GT.0 .AND. ICBCFL.NE.0 ) THEN
        ibd = ICBCFL
        iout1 = ISTCB1
      END IF
      IF ( ISTCB2.GT.0 .AND. ICBCFL.NE.0 ) THEN
        ibdst = -1
        iout2 = ISTCB2
      ELSE IF ( ISTCB2.LT.0 .AND. ICBCFL.NE.0 ) THEN
        ibdst = ICBCFL
        iout2 = ABS(ISTCB2)
      END IF
      ibdlbl = 0
      ibstlb = 0
C
C2------WRITE HEADER WHEN CELL-BY-CELL FLOWS WILL BE SAVED AS A LIST.
!IFACE
      IF ( ibd.EQ.2 ) THEN
        naux=1
        IF(IAUXSV.EQ.0) naux=0
        CALL UBDSV4(Kkstp, Kkper, text, naux, 'IFACE           ',
     +                            iout1, NCOL, NROW,NLAY,
     +                            NSTRM, IOUT, DELT, PERTIM, 
     +                            TOTIM, IBOUND)
      END IF
      IF ( ibdst.EQ.2 ) CALL UBDSV2 (Kkstp, Kkper, strtxt, iout2, NCOL, 
     +                               NROW, NLAY, NSTRM, IOUT, DELT, 
     +                               PERTIM, TOTIM, IBOUND)
C
C3------CLEAR BUFFERS.
      DO il = 1, NLAY
        DO ir = 1, NROW
          DO ic = 1, NCOL
            BUFF(ic, ir, il) = zero
          END DO
        END DO
      END DO
C
C4------INITIALIZE SEGMENT INFLOWS AND OUTFLOWS TO ZERO FOR LAKE PACKAGE 
C         WHEN THERE ARE STREAMS.
      iprvsg = -1
      IF ( Iunitlak.GT.0 ) THEN
        DO i = 1, NSS
          STRIN(i) = 0.0
Cdep          STROUT(i) = 0.0
Cdep          FXLKOT(i) = 0.0
        END DO
      END IF
C
C5-----START INTERNAL TIME LOOP FOR ROUTING STREAMFLOWS.
      deltinc = DELT
      IF ( IRTFLG .GT. 0 .AND. iss.EQ.0 ) THEN
        deltinc = DELT/FLOAT(numdelt)
      ELSE
        numdelt = 1
      END IF
      imassroute = 0
      DO irt = 1, numdelt
        rtime = TOTIM - DELT + irt*deltinc
        IF ( irt.EQ.numdelt ) imassroute = 1
C
C5b------DETERMINE LAYER, ROW, COLUMN OF EACH REACH.
        DO l = 1, NSTRM
          lfold = l
          IF ( Nfoldflbt==1 ) lfold = 1
          gwflow = 0.0D0
          dvrsn = 0.0D0
          flowin = 0.0D0  
          depthtr = 0.0
          IF ( irt.EQ.1 ) THEN
            SUMLEAK(l) = 0.0D0
            SUMRCH(l) = 0.0
            STRM(29,l) = 0.0
          END IF
          lsub = l
          ll = l - 1
          il = ISTRM(1, l)
          ir = ISTRM(2, l)
          ic = ISTRM(3, l)
C
C6------DETERMINE STREAM SEGMENT AND REACH NUMBER.
          istsg = ISTRM(4, l)
          nreach = ISTRM(5, l)
          icalc = ISEG(1, istsg)
          strtop = STRM(3, l)
! RGN 5/9/09 set slope for all icalc
          slope = STRM(2, l)
C
C7------SET FLOWIN EQUAL TO STREAM SEGMENT INFLOW IF FIRST REACH.
          IF ( nreach.EQ.1 ) THEN
            flowin = SEG(2, istsg)
!EDM - Count connection for LMT
            IF(IUNIT(49).GT.0) THEN  !IUNIT(49): LMT
              IF ( ISEG(3, istsg).EQ.5 ) THEN  
                NINTOT = NINTOT + 1 
              ENDIF
            ENDIF
            IF ( IDIVAR(1,istsg).EQ.0 ) 
     +          sfrbudg_in = sfrbudg_in + SEG(2, istsg)
C
C8------STORE OUTFLOW FROM PREVIOUS SEGMENT IN SGOTFLW LIST AND IN
C         STRIN FOR LAKE PACKAGE.
            IF ( istsg.GT.1 ) THEN
              iprvsg = ISTRM(4, ll)
              SGOTFLW(iprvsg) = STRM(9, ll)
              IF ( Iunitlak.GT.0 ) STRIN(iprvsg) = STRM(9, ll)
              IF ( Iunitlak.GT.0 .AND. IDIVAR(1,istsg).LT.0 ) 
     +               sfrbudg_out = sfrbudg_out + STRIN(iprvsg)
            END IF
C
C9------COMPUTE INFLOW OF A STREAM SEGMENT EMANATING FROM A LAKE.
            IF ( (Iunitlak.GT.0) .AND. (IDIVAR(1,istsg).LT.0) ) THEN
              lk = IABS(IDIVAR(1, istsg))
C10-----OUTFLOW FROM LAKE COMPUTED IN LAKE PACKAGE
              IF ( SEG(2,istsg).LE.CLOSEZERO ) THEN
                flowin = STROUT(istsg)
                sfrbudg_in = sfrbudg_in + flowin
              ELSE
C11-----SET SPECIFIED OUTFLOW FROM LAKE
                flowin = FXLKOT(istsg)
                sfrbudg_in = sfrbudg_in + FXLKOT(istsg)
              END IF
C              NINTOT = NINTOT + 1    !EDM  WITH NEW FTL FORMAT, THIS INDEX SHOULD NO LONGER BE COUNTED AS 'CONNECT SFR LAK' WILL ACCOUNT FOR IT
            END IF
C
C14-----COMPUTE ONE OR MORE DIVERSIONS FROM UPSTREAM SEGMENT.
Crgn&dep   revised computation of diversions and added subroutine
            IF( istsg.GT.1 )THEN
              DO kss = 2, NSS
                upflw = SGOTFLW(istsg-1)
                idivseg = kss
                IF( IDIVAR(1,kss).EQ.istsg-1 ) THEN
                   dvrsn = SEG(2,idivseg)
                   iprior = IDIVAR(2,kss)
                  CALL GWF2SFR7DIVERS(iprior, upflw, dvrsn)
                  DVRSFLW(kss) = dvrsn
                  SGOTFLW(istsg-1) = SGOTFLW(istsg-1) - dvrsn
                END IF
              END DO
C
C20-----SET FLOW INTO DIVERSION IF SEGMENT IS DIVERSION.
              IF( ISEG(3,istsg).EQ.6 ) THEN
!EDM - For LMT
                IF( IDIVAR(1,istsg).GT.0 ) THEN
                  flowin = DVRSFLW(istsg)
                  IF(IUNIT(49).GT.0) NINTOT = NINTOT + 1
                ENDIF
              END IF
            END IF
C
C20B-----STORE OUTFLOW FROM PREVIOUS SEGMENT FOR RECHARGE  !cjm
!           IF ( istsg.GT.1 ) THEN
!             IF (Iunitrch .GT. 0) THEN
!               iprvsg = ISTRM(4, ll)
!               IF ( DVRCH(iprvsg) .GT. 0) THEN
!                 DO icount = 1, DVRCH(iprvsg)
!                   irr = DVRCELL(icount, 1, iprvsg)
!                   icc = DVRCELL(icount, 2, iprvsg)
!                   IF ( Iunituzf.GT.0 ) THEN
!                     dvt = SGOTFLW(iprvsg)*DVRPERC(icc,irr)
!                     dvt = dvt/(DELR(IC)*DELC(IR))
!                     FINF(icc, irr) = RECHSAVE(icc, irr) + 
!     +                              dvt*(1.0-DVEFF(iprvsg))
!                   ELSE
!                     dvt = (SGOTFLW(iprvsg) / float(DVRCH(iprvsg)))
!                     RECH(icc, irr) = RECHSAVE(icc, irr) + 
!     +                              dvt*(1.0-DVEFF(iprvsg))
!                   END IF
!                 END DO
!               END IF
!             END IF
!           END IF
C           
C22-----SUM TRIBUTARY OUTFLOW AND USE AS INFLOW INTO DOWNSTREAM SEGMENT.
            IF ( istsg.GE.1 .AND. ISEG(3, istsg).EQ.7 ) THEN
              itrib = 1
              flowin = 0.0D0
              DO WHILE ( itrib.LE.NSS )
                IF ( istsg.EQ.IOTSG(itrib) ) THEN
                  trbflw = SGOTFLW(itrib)
                  flowin = flowin + trbflw
                  IF(IUNIT(49).GT.0) THEN  !IUNIT(49): LMT
                    NINTOT = NINTOT + 1   !EDM
                  ENDIF
                END IF
                itrib = itrib + 1
              END DO
              flowin = flowin + SEG(2, istsg)  !SEG(2,istsg) stores specified inflow, and should have a spot in "Headwaters" flows
              IF(IUNIT(49).GT.0) THEN  !IUNIT(49): LMT
                IF(SEG(2,ISTSG).GT.CLOSEZERO) THEN  !Possible to have both tributary inflow and specified inflow. if the latter exist, count it next
                  NINTOT = NINTOT + 1   !EDM
                ENDIF
              END IF
C
C23-----CHECK IF SPECIFIED "FLOW" IS WITHDRAWAL (i.e., negative), THAT WATER IS AVAILABLE.
              IF ( flowin.LT.0.0D0 ) THEN
                flowin = 0.0D0
 !               WRITE (IOUT, 9003) istsg
! 9003          FORMAT (//2X, '*** WARNING *** FLOW INTO DIVERSIONARY ',
!     +                 'STREAM SEGMENT No. ', I6, ' WAS NEGATIVE; ', 
!     +                 'FLOWIN RE-SET = 0.0'/)
              END IF
            END IF
C
C24-----SET INFLOW EQUAL TO OUTFLOW FROM UPSTREAM REACH, WHEN REACH
C         GREATER THAN 1.
          ELSE IF ( nreach.GT.1 ) THEN
            flowin = STRM(9, ll)
            IF(IUNIT(49).GT.0) THEN  !IUNIT(49): LMT
              NINTOT = NINTOT + 1    !EDM
            ENDIF
          END IF
C
C- EDM -IF OUTSEG=0 THEN SEGMENT IS A NETWORK SINK AND SHOULD BE COUNTED FOR LMT
          IF(IUNIT(49).GT.0) THEN  !IUNIT(49): LMT
            IF(IOTSG(ISTSG).EQ.0.AND.NREACH.EQ.ISEG(4,ISTSG)) THEN
              NINTOT = NINTOT + 1
            ENDIF
          ENDIF
C
C25-----SEARCH FOR UPPER MOST ACTIVE CELL IN STREAM REACH. Revised ERB
          ilay = il
          TOPCELL: DO WHILE ( ilay.LE.NLAY )
            IF ( IBOUND(ic, ir, ilay).EQ.0 ) THEN
              ilay = ilay + 1
            ELSE
              EXIT TOPCELL
            END IF
          END DO TOPCELL
          IF ( ilay.LE.NLAY ) il = ilay
C
C26-----DETERMINE LEAKAGE THROUGH STREAMBED.
          hstr = HSTRM(l,irt)
          depth = hstr - STRM(3, l)
          IF ( icalc.EQ.0 ) THEN
            depth = STRM(15, l) - STRM(3, l)
            hstr = STRM(15, l)
          END IF
          cstr = STRM(16, l)
          width = HWDTH(l,irt)
          IF ( icalc.EQ.1 ) THEN
            cstr = cstr*smooth(depth,dwdh)
            width = width*smooth(depth,dwdh)
          END IF
          sbot = STRM(4, l)
          sbdthk = STRM(8, l)
          strlen = STRM(1, l)
          icalccheck = 0
          totflwt = 0.0D0
          totdelstor = 0.0D0
          IF ( icalc.GE.2 ) wetperm = HWTPRM(l,irt)
          IF ( IUZT.EQ.1 ) THEN
            IF ( icalc.EQ.1 .OR. icalc.EQ.2 ) icalccheck = 1
            IF ( icalccheck.EQ.1 ) THEN
              fks = UHC(l)
              thr = THTR(l)
              epsilon = EPS(l)
              thetas = THTS(l)
              ha = -0.15
              areamax = 0.0
              fbcheck = 1.0D-12/DELT
              IF ( fbcheck.LT.5.0D-8 ) fbcheck = 5.0D-8
              avhc = STRM(6, l)
              IF ( icalc.EQ.2 ) THEN
                DO i = 1, ISUZN
                  UZSEEP(i, l) = 0.0D0
                  areamax = areamax + WETPER(i, l)*strlen
                END DO
              ELSE IF ( icalc.EQ.1 ) THEN
                wetperm = width
 ! RGN 10/4/11 next line is not correct
 !               WETPER(1, l) = width
                UZSEEP(1, l) = 0.0D0
                areamax = WETPER(1, l)*strlen
              END IF
            END IF
          END IF
C26a----CHECK IF LAKE INUNDATES STREAM. IF SO THEN SET avhc and cstr = 0.
          IF ( Iunitlak.GT.0 ) THEN
            illake = il - 1
            IF ( illake.GE.1 ) THEN
              lakid = LKARR1(ic, ir, illake)
              IF ( lakid.GT.0 ) THEN
                IF ( STGNEW(lakid).GT.
     +               dble(BOTM(ic,ir,LBOTM(illake)))) THEN
                  cstr = 0.0
                  avhc = 0.0
                END IF
              END IF
            END IF
          END IF
          IF ( icalc.EQ.1 ) THEN
            qcnst = CONST*width*SQRT(slope)/SEG(16, istsg)
          ELSE IF ( icalc.EQ.3 ) THEN
            awdth = SEG(14, istsg)
            bwdth = SEG(15, istsg)
          END IF
C
C26b-----SET STREAMBED HYDRAULIC CONDUCTIVITY AND STREAM LEAKAGE TO
C         ZERO WHEN NOT AN ACTIVE CELL.
          IF ( IBOUND(ic, ir, il).LE.0 ) THEN
            avhc = 0.0
!           strleak = 0.0D0
            h = hstr
          END IF
          IF ( ilay.LE.NLAY .AND. IBOUND(ic, ir, il).GT.0 ) THEN
C
C27-----COMPUTE HEAD DIFFERENCE ACROSS STREAMBED.
            h = HNEW(ic, ir, il)
            hld = HLDSFR(l)
! Added code to test for BCF or LPF 11/19/07
            IF ( ABS(SNGL(hld)-HDRY).LT.CLOSEZERO ) hld = h
            IF ( irt.EQ.numdelt ) HLDSFR(l) = h
            IF ( h.LT.sbot ) THEN
              hdiff = hstr - sbot
              grad = (hdiff)/sbdthk
            ELSE
              hdiff = hstr - h
              grad = (hdiff)/sbdthk
            END IF
C
C28-----COMPUTE LEAKAGE ACROSS STREAMBED.
            flobot = cstr*(hdiff)
C
C29-----STREAMFLOW OUT EQUALS STREAMFLOW IN MINUS LEAKAGE.
          ELSE
            flobot = 0.0D0
            grad = 0.0D0
            hdiff = 0.0D0
          END IF
C
C30-----COMPUTE FLOW IN STREAM CHANNEL AND SET LEAKAGE EQUAL TO FLOW
C         IF LEAKAGE MORE THAN FLOW. Revised DEP
          runof = STRM(12, l)
          runoff = STRM(24, l)
          IF ( icalc.LE.1 ) THEN
            precip = STRM(14, l)
            etstr = STRM(13, l)
          ELSE IF ( icalc.GE.2 .AND. icalc.LE.4 ) THEN
C
C31-----NO PRECIPITATION OR ET FROM CHANNEL WHEN WIDTH IS ZERO.
            precip = STRM(14, l)*width
            etstr = STRM(13, l)*width
          END IF
          flow = flowin + runof + runoff + precip - etstr
          IF ( flow.LT.NEARZERO ) THEN
            IF ( icalc.EQ.0 ) depth = 0.0D0
            IF ( flobot.GE.0.0D0 ) THEN
              flobot = 0.0D0
              IF ( runof.LT.NEARZERO ) THEN
                IF ( flowin+runoff+precip.LT.NEARZERO ) THEN
                  runof = 0.0D0
                  etstr = 0.0D0
                ELSE IF (runof.GE.flowin+runoff+precip-etstr) THEN
                  runof = flowin + runoff + precip - etstr
                ELSE IF ( etstr.GE.flowin+runoff+precip+runof ) THEN
                  etstr = flowin + runoff + precip + runof
                END IF
              ELSE IF ( flowin+runof+runoff+precip.GT.NEARZERO ) THEN
                etstr = flowin + runof + runoff + precip
              ELSE
                etstr = 0.0D0
              END IF
            ELSE IF ( runof.LT.0.0D0 ) THEN
              IF ( flowin+runoff+precip-flobot.LT.NEARZERO ) THEN
                runof = 0.0D0
                etstr = 0.0D0
              ELSE IF (runof.GE.flowin+runoff+precip-flobot-etstr)
     +                 THEN
                runof = -(flowin+runoff+precip-flobot-etstr)
              ELSE IF (etstr.GE.flowin+runoff+precip-flobot+runof) THEN
                etstr = flowin + runoff + precip - flobot + runof
              END IF
            ELSE IF ( etstr.GT.flowin+runoff+runof+precip-flobot ) THEN
              etstr = flowin + runof + runoff + precip - flobot
            ELSE IF ( flowin+runoff+runof+precip-flobot.LT.
     +                NEARZERO ) THEN
              etstr = 0.0D0
            END IF
            flow = flowin + runof + runoff + precip - etstr
          END IF
          IF ( flobot.GE.flow ) THEN
            flobot = flow
            IF ( icalc.EQ.0 ) depth = 0.0D0
          END IF
! RGN Fixed statement. Only call subroutine if no UZ flow.
          IF ( icalccheck.EQ.1 ) THEN
            IF ( h.LT.sbot )
     +        flobot = CALCUNSATFLOBOT(depth, avhc, fks, wetperm,
     +                     sbdthk, areamax, strlen, fbcheck,
     +                     NWAVST(:,l), maxwav, FOLDFLBT(lfold))
          END IF
          IF ( flobot.GE.flow ) flobot = flow
C
C32-----ROUTE FLOW IN CHANNELS WHEN IRTFLG IS NOT ZERO AND 
C         SIMULATION IS TRANSIENT.
          IF ( IRTFLG.NE.0 .AND. iss.EQ.0 ) THEN
            qlat = (runof + runoff + precip - etstr)/strlen
            qa = STRM(26,l)
            qb = STRM(25,l)
! RGN 6/10/09 set icalc 3 information
            IF ( icalc.EQ.3 ) THEN
              cdpth = SEG(9, istsg)
              fdpth = SEG(10, istsg)
              awdth = SEG(14, istsg)
              bwdth = SEG(15, istsg)
            END IF
            IF ( ABS(TOTIM-DELT).LT.1.0E-10 ) qb = 0.0
            IF ( ABS(TOTIM-DELT).LT.1.0E-10 ) qa = 0.0
            qc = flowin
            fltest = 0.0
            IF (  flobot.LT.0.0D0 ) THEN
              fltest=qa+qb+qc+qlat*strlen-flobot
            ELSE
              fltest=qa+qb+qc+qlat*strlen
            END IF
            IF ( fltest.GT.NEARZERO ) THEN
              Transient_bd = 0.0
              CALL ROUTE_CHAN(qa, qb, qc, qd, qcnst, cdpth, awdth, 
     +                fdpth, bwdth, deltinc, icalc, strlen, slope,
     +                istsg, nreach, itstr, qlat, flobot, width, l,
     +                Transient_bd, depthtr)
              STRM(30,l) = Transient_bd
              Transient_bd_tot = Transient_bd_tot + Transient_bd
            ELSE
              qd = 0.0
            END IF
            IF ( Qd.LT.FLWTOL ) THEN
              Qd = 0.0
              depthtr = 0.0
            END IF
            flowot = qd
            STRM(25,l) = qd
            STRM(26,l) = qc
          ELSE
C
! EDM calc x-sectional area of channel for LMT w/ SFR mass routine
!  First, need some terms to send to CALC_XSA
            qlat = (runof + runoff + precip - etstr)/strlen
            qa = STRM(10,l)
            qb = STRM(9,l)
            IF ( icalc.EQ.3 ) THEN
              cdpth = SEG(9, istsg)
              fdpth = SEG(10, istsg)
              awdth = SEG(14, istsg)
              bwdth = SEG(15, istsg)
            END IF
            STRM(31,l) = CALC_XSA(qa,qcnst,cdpth,awdth,fdpth,bwdth,
     +                            icalc,slope,istsg,nreach,itstr,width,
     +                            depthtr)
            flowot = flow - flobot
            qc = flowin
            qd = flowot
          END IF
          IF ( ISS.EQ.0 ) THEN
            SUMLEAK(l) = SUMLEAK(l) + (flobot*deltinc)
            IF ( irt.EQ.numdelt) SUMLEAK(l) = SUMLEAK(l)/DELT
          ELSE
            SUMLEAK(l) = flobot
          END IF
          IF ( icalccheck.EQ.1 .AND. irt.EQ.numdelt ) THEN
            IF ( h.LT.sbot ) THEN
              fbot = SUMLEAK(l)
              CALL CALC_UNSAT_INFIL(fbot, UZSEEP(:,l), 
     +                            UZTHST(:,l),thr, ha, thetas, 
     +                            epsilon, fks, avhc,depth, sbdthk, 
     +                            WETPER(:,l),UZWDTH(:,l), flow, 
     +                            NWAVST(:,l),strlen, iwidthcheck, 
     +                            icalc)
              IF ( ICALC.EQ.1 ) flobot = UZSEEP(1,l)*UZWDTH(1,l)*strlen
            ELSE
              DO i = 1, ISUZN
                UZSEEP(i, l) = 0.0D0
              END DO
            END IF
          END IF
C
C33-----STORE OUTFLOW FROM LAST REACH IN LAST SEGMENT IN STRIN
C         LIST FOR LAKE PACKAGE.
          IF ( Iunitlak.GT.0 ) THEN
            IF ( l.EQ.NSTRM .AND. istsg.EQ.NSS ) STRIN(istsg) = flowot
          END IF
C
C34-----STORE STREAM INFLOW, OUTFLOW, AND LEAKAGE FOR EACH REACH.
          STRM(9, l) = flowot
          STRM(10, l) = flowin
          STRM(11, l) = flobot
          IF ( flobot<0.0) SEG(27,istsg) = SEG(27,istsg) + flobot
          STRM(17, l) = hdiff
          STRM(18, l) = grad
          STRM(19, l) = h
C
C35-----ADD FLOW TO STREAM BUDGET OUTFLOW IF FLOW LEAVES ACTIVE MODEL. 
          IF ( irt.EQ.numdelt ) THEN
            IF ( l .LT. NSTRM ) THEN
              IF ( IOTSG(istsg) .EQ. 0 .AND. 
     +             ISTRM(4, l) .NE. ISTRM(4, l+1)) THEN
                 sfrbudg_out = sfrbudg_out + flowot
              END IF
            ELSE IF( IOTSG(istsg) .EQ. 0 ) THEN
              sfrbudg_out = sfrbudg_out + flowot
            END IF
            IF ( flobot .GT. 0.0D0 ) THEN
              sfrbudg_out = sfrbudg_out + SUMLEAK(l)
            ELSE
              sfrbudg_in = sfrbudg_in - SUMLEAK(l)
            END IF 
            sfrbudg_in = sfrbudg_in + precip 
            sfrbudg_in = sfrbudg_in + runof + runoff + flowin
            sfrbudg_out = sfrbudg_out + etstr
          END IF 
C
C36-----STORE OUTFLOW FOR LAST SEGMENT IN SGOTFLW LIST AND IN STRIN FOR
C         LAKE PACKAGE.
          IF ( l.EQ.NSTRM ) THEN
            SGOTFLW(istsg) = STRM(9, l)
            IF ( Iunitlak.GT.0 ) STRIN(istsg) = STRM(9, l)
          END IF
C
C37-----ADD RATES TO BUFFERS.
          IF ( icalccheck.EQ.1 .AND. iss.EQ.0 ) THEN
            IF ( irt.EQ.numdelt ) THEN
              CALL UZMASSBAL(lsub, h, hld, thr, thetas, epsilon, fks, 
     +                     UZDPST(:,l), UZTHST(:,l), UZSPST(:,l), 
     +                     UZFLST(:,l), LTRLST(:,l), ITRLST(:,l), 
     +                     UZFLWT(:,l), UZSTOR(:,l), DELSTOR(:,l), 
     +                     NWAVST(:,l), UZOLSFLX(:,l), UZWDTH(:,l), 
     +                     WETPER(:,l), UZSEEP(:,l), ratin, ratout, il, 
     +                     ir, ic, flobot, sbot, strlen, totflwt, 
     +                     totuzstor, totdelstor, iwidthcheck,
     +                     AVDPT(:,l), AVWAT(:,l), WAT1(:,l), ibd,
     +                     icalc, DELT, imassroute, Iunitgage, 
     +                     gwflow)
            END IF
            FOLDFLBT(l) = flobot
          ELSE
            gwflow = flobot
            BUFF(ic, ir, il) = BUFF(ic, ir, il) + flobot
            IF ( IUZT.GT.0 ) totdelstor = 0.0D0
C
C38-----SUBTRACT FLOBOT FROM RATOUT WHEN GROUND WATER DISCHARGES
C         TO STREAM REACH.
            IF ( flobot.LT.0.0D0 ) THEN
              ratout = ratout - flobot
              SFRUZBD(9) = -flobot
            END IF
C 
C39-----ADD FLOBOT TO RATIN WHEN STREAM RECHARGES GROUND WATER.
            IF ( flobot.GT.0.0D0 ) THEN
              ratin = ratin + flobot
              SFRUZBD(8) = flobot
            END IF
            IF ( icalccheck.EQ.1 .AND. sbot.GT.h ) THEN
              totflwt = flobot*DELT
              SFRUZBD(1) = SFRUZBD(1) + flobot*DELT
              SFRUZBD(2) = 0.0
              SFRUZBD(3) = SFRUZBD(3) + flobot*DELT
              SFRUZBD(4) = SFRUZBD(4) + flobot
              SFRUZBD(5) = 0.0
              SFRUZBD(6) = SFRUZBD(6) + flobot
            ELSE IF ( sbot.LT.h ) THEN
              totflwt = 0.0
            END IF
          END IF
C
C40-----PRINT STREAMFLOWS AND RATES FOR EACH REACH TO MAIN LIST IF
C         REQUESTED (ISTCB1<0 and IBD<0)AND NO UNSATURATED FLOW.
          IF ( IUZT.EQ.0 ) THEN
            IF ( ibd.LT.0 .AND. IPTFLG.LE.0 ) THEN
              IF ( ibdlbl.EQ.0 ) WRITE (IOUT, 9004) txtlst, Kkper, Kkstp
              WRITE (IOUT, 9005) il, ir, ic, ISTRM(4, l), ISTRM(5, l), 
     +                           STRM(10, l), STRM(11, l), STRM(9, l), 
     +                           SNGL(runof+runoff), SNGL(precip), 
     +                          SNGL(etstr), STRM(15, l), SNGL(depth), 
     +                           STRM(5, l), STRM(16, l), SNGL(grad)
              ibdlbl = 1
            END IF
C
C41-----PRINT STREAMFLOWS AND RATES FOR EACH REACH TO STREAM LIST
C         IF REQUESTED (ISTCB2>0).
            IF ( ibdst.LT.0 .AND. IPTFLG.LE.0 ) THEN
              IF ( ibstlb.EQ.0 ) WRITE (iout2, 9004) txtlst, Kkper,
     +                                               Kkstp
              WRITE (iout2, 9005) il, ir, ic, ISTRM(4, l), ISTRM(5, l), 
     +                            STRM(10, l), STRM(11, l), STRM(9, l), 
     +                            SNGL(runof+runoff), SNGL(precip), 
     +                            SNGL(etstr), STRM(15, l), SNGL(depth),
     +                            STRM(5, l), STRM(16, l), SNGL(grad)
              ibstlb = 1
            END IF
C
C42-----PRINT STREAMFLOWS AND RATES FOR EACH REACH TO MAIN LIST
C         WHEN UNSATURATED FLOW IS ACTIVE.
          ELSE
            IF (ibd.LT.0 .AND. IPTFLG.LE.0 .AND. imassroute.EQ.1) THEN
              IF ( ibdlbl.EQ.0 ) WRITE (IOUT, 9006) txtlst, Kkper, Kkstp
              WRITE (IOUT, 9007) il, ir, ic, ISTRM(4,l), ISTRM(5,l), 
     +                           STRM(10,l), STRM(11,l), STRM(9,l), 
     +                           SNGL(runof+runoff), SNGL(precip), 
     +                           SNGL(etstr), STRM(15,l), SNGL(depth), 
     +                           STRM(5,l), STRM(16,l), totflwt/DELT,
     +                           totdelstor/DELT
              ibdlbl = 1
            END IF
C
C43-----PRINT STREAMFLOWS AND RATES FOR EACH REACH TO STREAM LIST
C         WHEN UNSATURATED FLOW IS ACTIVE.
            IF ( ibdst.LT.0 .AND. IPTFLG.LE.0 .AND.
     +           imassroute.EQ.1 ) THEN
              IF ( ISTCB2.EQ.IOUT ) THEN
                IF ( ibstlb.EQ.0 ) WRITE (iout2, 9006) txtlst, Kkper, 
     +                                               Kkstp
                 WRITE (iout2, 9007) il, ir, ic, ISTRM(4,l), ISTRM(5,l),
     +                           STRM(10,l), STRM(11,l), STRM(9,l), 
     +                           SNGL(runof+runoff), SNGL(precip), 
     +                           SNGL(etstr), STRM(15,l), SNGL(depth), 
     +                           STRM(5,l), STRM(16,l), totflwt/DELT,
     +                           totdelstor/DELT
                ibstlb = 1
              ELSE
              
                IF ( ibstlb.EQ.0 ) WRITE (iout2, 9009) txtlst, Kkper, 
     +                                               Kkstp
                WRITE (iout2, 9010) il, ir, ic, ISTRM(4,l), ISTRM(5,l), 
     +                           STRM(10,l), STRM(11,l), STRM(9,l), 
     +                           SNGL(runof+runoff), SNGL(precip), 
     +                           SNGL(etstr), STRM(15,l), SNGL(depth), 
     +                           STRM(5,l), STRM(16,l), totflwt/DELT,
     +                           totdelstor/DELT, SNGL(h)
                ibstlb = 1
              END IF
            END IF
          END IF
C43B----SAVE SEEPAGE TO ARRAY FOR PRINTING NET SEEPAGE IN UZF
          FNETSEEP(IC,IR) = gwflow
C
C44-----SAVE FLOW TO AND FROM GROUND WATER IN A LIST FILE WHEN 
C         IBD IS EQUAL TO 2. revised dep 5/10/2006--fixed 9/15/2006
cDEP   need to fix for unsaturated flow
!IFACE
          IF ( imassroute.EQ.1 .AND. ibd.EQ.2 ) THEN
              xface(1)=ISTRM(6,l)
              CALL UBDSVB(iout1, NCOL, NROW, ic, ir, il, 
     +                 SNGL(gwflow), xface, 1, naux, 1,IBOUND, NLAY)
          END IF
          IF ( IRTFLG.EQ.0 ) THEN
            SFRQ(1, l) = (flowin+flowot)/2.0
            SFRQ(2, l) = (flowin+flowot)/2.0
            SFRQ(3, l) = flobot
            SFRQ(5, l) = flowin
          ELSE
            STRM(15, l) = depthtr + STRM(3, l)
            STRM(7, l) = depthtr
            SFRQ(1, l) = (qc + qd)/2.0
            SFRQ(2, l) = (qc + qd)/2.0
            SFRQ(3, l) = flobot
            SFRQ(5, l) = qc
          END IF
        END DO
!        IF ( Irtflg.NE.0 )WRITE(IOUT,*)
!     +         'TRANSIENT FLOW ERROR = ', Transient_bd
C
C45-----RECORD STREAM GAGING STATION DATA (IF SOLUTE TRANSPORT NOT ACTIVE).
          IF ( Iunitgwt.LE.0 .AND. Iunitgage.GT.0 ) THEN
            CALL SGWF2GAG7SO(Iunitgwt, Iunituzf, rtime, BUFF, SFRQ,
     +                        ibd, Nsol)
          END IF
C
C46-----END OF INTERNAL TIME LOOP FOR ROUTING FLOW IN CHANNELS.
      END DO
C
C46Bdep----New loop to compute specified flows from external sources
Cadded 6/27/2008
      DO istsg = 1, NSS      
         IF ( ISEG(3, istsg).EQ.5 .OR. ISEG(3, istsg).EQ.7 ) THEN
              TOTSPFLOW = TOTSPFLOW + SEG(2, istsg)
         END IF
      END DO
      IF ( ibd.NE.0 .AND. IPTFLG.LE.0 ) WRITE (IOUT, 9008)
 9004 FORMAT (1X, ///1X, A, '   PERIOD ', I6, '   STEP ', I8, //, 
     +        ' LAYER ROW COL. STREAM  RCH.  FLOW INTO     ', 
     +        'FLOW TO   FLOW OUT OF   OVRLND.     DIRECT     ', 
     +        'STREAM       STREAM     STREAM     STREAM   ', 
     +        'STREAMBED  STREAMBED', /16X, 
     +        'SEG.NO.  NO.  STRM. RCH.    AQUIFER    STRM. RCH.',
     +        '    RUNOFF     PRECIP       ET          HEAD    ', 
     +        '   DEPTH      WIDTH   CONDCTNC.   GRADIENT'/)
 9005 FORMAT (1X, I3, I5, I5, 2I6, 3X, 1PE11.4, 1X, E11.4, 2X, E11.4,
     +        3(1X, E10.3), 1X, E12.5, 4(1X, E10.3))
Cdep  revised format statement
 9006 FORMAT (1X, ///1X, A, '   PERIOD ', I6, '   STEP ', I8, //,
     +        ' LAYER ROW COL. STREAM  RCH.   FLOW INTO   ', 
     +        ' STREAM     FLOW OUT OF   OVRLND.    DIRECT     ', 
     +        'STREAM      STREAM      STREAM     STREAM  ', 
     +        'STREAMBED   FLOW TO   CHNG. UNSAT. ', /16X, 
     +        'SEG.NO.  NO.   STRM. RCH.    LOSS       ', 
     +        'STRM. RCH.   RUNOFF     PRECIP       ET         ', 
     +        'HEAD        DEPTH      WIDTH  CONDCTNC.   ', 
     +        'WAT. TAB.', /)
 9007 FORMAT (1X, I3, I5, I5, 2I6, 3X, 1PE11.4, 1X, E11.4, 2X, E11.4,
     +        3(1X, E10.3), 1X, E12.5, 5(1X, E10.3))
 9008 FORMAT (//)
 9009 FORMAT (1X, ///1X, A, '   PERIOD ', I6, '   STEP ', I8, //,
     +        ' LAYER ROW COL. STREAM  RCH.   FLOW INTO   ', 
     +        ' STREAM     FLOW OUT OF   OVRLND.    DIRECT     ', 
     +        'STREAM      STREAM      STREAM     STREAM  ', 
     +        'STREAMBED   FLOW TO   CHNG. UNSAT.   GW ', /16X, 
     +        'SEG.NO.  NO.   STRM. RCH.    LOSS       ', 
     +        'STRM. RCH.   RUNOFF     PRECIP       ET         ', 
     +        'HEAD        DEPTH      WIDTH  CONDCTNC.   ', 
     +        'WAT. TAB.   STORAGE     HEAD', /)
 9010 FORMAT (1X, I3, I5, I5, 2I6, 3X, 1PE11.4, 1X, E11.4, 2X, E11.4,
     +        3(1X, E10.3), 1X, E12.5, 6(1X, E10.3))
C
C47-----PRINT MASS BALANCE OF UNSATURATED ZONE IF IUZT GREATER THAN 0.
      IF ( IBUDFL.GT.0 .and. icalccheck.EQ.1) 
     +     CALL GWF2SFR7UZOT(Kkstp, Kkper)
C
C48-----SAVE FLOW TO AND FROM GROUND WATER AS A 3-D ARRAY WHEN 
C         IBD IS EQUAL TO 1. revised dep 5/10/2006
      IF ( ibd.EQ.1 ) CALL UBUDSV(Kkstp, Kkper, text, iout1, BUFF, NCOL,
     +                            NROW, NLAY, IOUT)
C
C49-----MOVE RATES, VOLUMES, AND LABELS INTO ARRAYS FOR PRINTING.
      rin = ratin
      rout = ratout
      VBVL(3, MSUM) = rin
      VBVL(4, MSUM) = rout
      VBVL(1, MSUM) = VBVL(1, MSUM) + rin*DELT
      VBVL(2, MSUM) = VBVL(2, MSUM) + rout*DELT
      VBNM(MSUM) = text
C
C50-----INCREMENT BUDGET TERM COUNTER.
      MSUM = MSUM + 1
C
C51-----STREAMFLOW OUT OF EACH REACH IS SAVED TO A LIST FILE
C       WHEN COMPACT BUDGET REQUESTED OR TO A 3-D ARRAY
C       WHEN STANDARD UNFORMATTED BUDGET. revised dep 5/10/2006
      IF ( ibdst.GT.0 ) THEN
        DO il = 1, NLAY
          DO ir = 1, NROW
            DO ic = 1, NCOL
              BUFF(ic, ir, il) = zero
            END DO
          END DO
        END DO
        DO l = 1, NSTRM
          il = ISTRM(1, l)
          ir = ISTRM(2, l)
          ic = ISTRM(3, l)
          ilay = il
          BUFF(ic, ir, il) = BUFF(ic, ir, il) + STRM(9, l)
Cdep   added compact budget option for streamflow out of reach
          IF ( ibdst.EQ.2 )  CALL UBDSVA(iout2, NCOL, NROW, ic, ir, 
     +                                   il, STRM(9, l), IBOUND, NLAY)
        END DO
        IF ( ibdst.EQ.1 ) CALL UBUDSV(Kkstp, Kkper, strtxt, iout2, 
     +              BUFF, NCOL, NROW, NLAY, IOUT)
      END IF
      SFRRATIN = RATIN
      SFRRATOUT = RATOUT
C
C52-----RETURN.
      RETURN
      END SUBROUTINE GWF2SFR7BD
C
C-------SUBROUTINE GWF2SFR7LAKOUTFLW
      SUBROUTINE GWF2SFR7LAKOUTFLW(kkiter)
C     *****************************************************************
C     CALCULATE ARRAYS OF LAKE STAGE, FLOW, AND THE DERIVATIVE OF
C     FLOWS FOR STREAM SEGMENTS THAT HAVE INFLOWS DETERMINED BY
C     LAKE STAGE
!--------REVISED FOR MODFLOW-2005 RELEASE 1.9, FEBRUARY 6, 2012
C     *****************************************************************
      USE GWFSFRMODULE
!!      USE GLOBAL,     ONLY:IOUT
      IMPLICIT NONE
      INTRINSIC FLOAT, ABS, IABS, DSQRT, DLOG10, SQRT, SNGL
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      INTEGER icalc, istsg, l, lk, nreach, nstrpts, kkiter
      REAL    roughch, roughbnk, widthch
      DOUBLE PRECISION finc, strbdtop, dlkstr1, dlkstr2, slope, cdpth,
     +                 fdpth, flwdlk1, flwdlk2, wdthlk1, wdthlk2,
     +                 wetperm1, wetperm2, width1, width2 
C     ------------------------------------------------------------------
C     LOCAL STATIC VARIABLES
C     ------------------------------------------------------------------
      DOUBLE PRECISION FIVE_THIRDS, DPMAXLK, delh
      PARAMETER (FIVE_THIRDS=5.0D0/3.0D0)
      PARAMETER (DPMAXLK=10.0D0)
C
C1------LOOP THROUGH ALL STREAM REACHES.
C
!rgn value for delh may not be optimal for all problems
!rgn value for DPMAXLK may not be sufficient for all problems
      delh = 0.01
      DO l = 1, NSTRM
        istsg = ISTRM(4, l)
        nreach = ISTRM(5, l)
        icalc = ISEG(1, istsg)
        IF ( icalc.EQ.1 .OR. icalc.EQ.2 ) THEN
          slope = STRM(2, l)
          roughch = SEG(16, istsg)
          IF ( icalc.EQ.1 )widthch = SEG(9, istsg)
          IF ( icalc.EQ.2 )roughbnk = SEG(17, istsg)
        END IF
        IF ( icalc.EQ.4 ) nstrpts = ISEG(2, istsg)
C
C2------DETERMINE SEGMENTS THAT GET THEIR INFLOWS FROM A LAKE.
        IF ( nreach.EQ.1 .AND. IDIVAR(1, istsg).LT.0 ) THEN
          finc = DPMAXLK/200.0D0
          strbdtop = SEG(8, istsg)
C
Cdep    Added tables for computing lake outflow in Lake Package
C3------CALCULATE TABLES FOR LAKE STAGE AND CHANGE IN LAKE OUTFLOW.
          DO lk = 1, 200
            IF ( lk.EQ.1 )THEN
              DLKSTAGE(1, istsg) = strbdtop
            ELSE
              DLKSTAGE(lk, istsg) = DLKSTAGE(lk-1, istsg) + finc
            END IF
             dlkstr1 = DLKSTAGE(lk, istsg)- strbdtop
             dlkstr2 = dlkstr1 + delh
C
C3------ICALC EQUALS 1.
            IF ( icalc.EQ.1 ) THEN
              flwdlk2 = (CONST/roughch)*widthch
     +                  *(dlkstr2**FIVE_THIRDS)*(DSQRT(slope)) 
              DLKOTFLW(lk, istsg) = FIVE_THIRDS*flwdlk2/dlkstr2
              SLKOTFLW(lk, istsg) = (CONST/roughch)*widthch
     +                  *(dlkstr1**FIVE_THIRDS)*(DSQRT(slope)) 
C
C4------ICALC EQUALS 2.
            ELSE IF ( icalc.EQ.2 ) THEN
              CALL GWF2SFR7FLW(dlkstr1, istsg, roughch, 
     +                        roughbnk, slope, wetperm1, 
     +                        flwdlk1, width1)
              CALL GWF2SFR7FLW(dlkstr2, istsg, roughch, 
     +                        roughbnk, slope, wetperm2, 
     +                        flwdlk2, width2)
              DLKOTFLW(lk, istsg) = (flwdlk1-flwdlk2)/(-delh)
              SLKOTFLW(lk, istsg) = flwdlk1
c
C3-----ICALC EQUALS 3 USING FORMULA-- Q=(DEPTH/CDPTH)**1/FDPTH).
            ELSE IF ( icalc.EQ.3 ) THEN
              cdpth = SEG(9, istsg)
              fdpth = SEG(10, istsg)
              DLKOTFLW(lk, istsg) = (1.0D0/(cdpth*fdpth)) *
     +                              (dlkstr2/cdpth)**(1.0D0/fdpth-1.0D0)
              SLKOTFLW(lk, istsg) = (dlkstr1/cdpth)**(1.0/fdpth)
C
C4-----FLOW FROM LAKE COMPUTED USING TABULATED VALUES.
            ELSE IF ( icalc.EQ.4 ) THEN
              CALL GWF2SFR7TBF(flwdlk1, dlkstr1, wdthlk1,
     +                         nstrpts, nreach, istsg, kkiter, 0)
              CALL GWF2SFR7TBF(flwdlk2, dlkstr2, wdthlk2,
     +                         nstrpts, nreach, istsg, kkiter, 0)
              DLKOTFLW(lk, istsg) = (flwdlk1-flwdlk2)/(-delh)
              SLKOTFLW(lk, istsg) = flwdlk1
            END IF
          END DO
        END IF
      END DO
      RETURN
      END SUBROUTINE GWF2SFR7LAKOUTFLW
C
C-------SUBROUTINE GWF2SFR7DIVERS
      SUBROUTINE GWF2SFR7DIVERS(Iprior, Upflw, Dvrsn)
C     ******************************************************************
C     COMPUTES DIVERSIONS FROM AN UPSTREAM SEGMENT
!--------REVISED FOR MODFLOW-2005 RELEASE 1.9, FEBRUARY 6, 2012
C     ******************************************************************
      IMPLICIT NONE
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER Iprior
      DOUBLE PRECISION  Upflw, Dvrsn
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      DOUBLE PRECISION dif, prcnt
C     ------------------------------------------------------------------
C1------IF IPRIOR IS ZERO THEN FLOW DIVERTED CAN BE ALL OF
C         STREAMFLOW UP TO SPECIFIED FLOW.
      IF ( Iprior.EQ.0 ) THEN
        dif = Upflw - Dvrsn
        IF ( dif.LT.0.0D0 ) THEN
          Dvrsn = Upflw
        END IF
C
C2------IF IPRIOR IS -1 THEN FLOW DIVERTED ONLY IF SPECIFIED FLOW
C         AVAILABLE OTHERWISE NO FLOW DIVERTED.
      ELSE IF ( Iprior.EQ.-1 ) THEN
        dif = Upflw - Dvrsn
        IF ( dif.LT.0.0D0 ) THEN
          Dvrsn = 0.0D0
        END IF
C
C3------IF IPRIOR IS -2 THEN FLOW DIVERTED IS PERCENTAGE OF
C         AVAILABLE STREAMFLOW.
      ELSE IF ( Iprior.EQ.-2 ) THEN
        prcnt = Dvrsn
        Dvrsn = Upflw*prcnt
C
C4------IF IPRIOR IS -3 THEN FLOW DIVERTED ONLY WHEN STREAMFLOW
C         EXCEEDS SPECIFIED FLOW (FLOOD CONTROL DIVERSION).
      ELSE IF ( Iprior.EQ.-3 ) THEN
        IF ( Upflw.GT.Dvrsn ) THEN
          Dvrsn = Upflw - Dvrsn
        ELSE
          Dvrsn = 0.0D0
        END IF
      END IF
      RETURN
      END SUBROUTINE GWF2SFR7DIVERS 
C
C-------SUBROUTINE GWF2SFR7UZOT
      SUBROUTINE GWF2SFR7UZOT(Kkstp, Kkper)
C     ******************************************************************
C     PRINTS MASS BALANCE FOR ENTIRE UNSATURATED ZONE
!--------REVISED FOR MODFLOW-2005 RELEASE 1.9, FEBRUARY 6, 2012
C     ******************************************************************
      USE GWFSFRMODULE, ONLY: SFRUZBD, CLOSEZERO
      USE GLOBAL,       ONLY: IOUT
      IMPLICIT NONE
      INTRINSIC ABS
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER Kkper, Kkstp
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      REAL adiffr, adiffv, bigvl1, bigvl2, diffr, diffv, prcntdifr, 
     +     prcntdifv, small, totrin, totrot, totvin, totvot
      CHARACTER*17 text1, text2, text3
      CHARACTER*18 val1, val2
C     ------------------------------------------------------------------
      text1 = '      STREAM LOSS'
      text2 = 'CHANGE IN STORAGE'
      text3 = '   RECHARGE TO GW'
      bigvl1 = 9.99999E11
      bigvl2 = 9.99999E10
      small = 0.1
      WRITE (IOUT, 9001) Kkstp, Kkper
      WRITE (IOUT, 9002)
C
C1------PRINT INDIVIDUAL INFLOW RATES AND VOLUMES AND THEIR TOTALS.
C
C1a-----STREAM LEAKAGE.
      IF ( ABS(SFRUZBD(1)).GT.CLOSEZERO .AND. 
     +     (SFRUZBD(1).GE.bigvl1 .OR. SFRUZBD(1).LT.small) ) THEN
        WRITE (val1, '(1PE18.4)') SFRUZBD(1)
      ELSE
        WRITE (val1, '(F18.4)') SFRUZBD(1)
      END IF
      IF ( ABS(SFRUZBD(4)).GT.CLOSEZERO .AND. 
     +     (SFRUZBD(4).GE.bigvl1 .OR. SFRUZBD(4).LT.small) ) THEN
        WRITE (val2, '(1PE18.4)') SFRUZBD(4)
      ELSE
        WRITE (val2, '(F18.4)') SFRUZBD(4)
      END IF
      WRITE (IOUT, 9003) text1, val1, text1, val2
C
C1b-----CHANGE IN STORAGE.
      IF ( ABS(SFRUZBD(2)).GT.CLOSEZERO .AND. 
     +     (ABS(SFRUZBD(2)).GE.bigvl1 .OR. 
     +       ABS(SFRUZBD(2)).LT.small) ) THEN
        WRITE (val1, '(1PE18.4)') SFRUZBD(2)
      ELSE
        WRITE (val1, '(F18.4)') SFRUZBD(2)
      END IF
      IF ( ABS(SFRUZBD(5)).GT.CLOSEZERO .AND. 
     +     (ABS(SFRUZBD(5)).GE.bigvl1 .OR. 
     +       ABS(SFRUZBD(5)).LT.small) ) THEN
        WRITE (val2, '(1PE18.4)') SFRUZBD(5)
      ELSE
        WRITE (val2, '(F18.4)') SFRUZBD(5)
      END IF
      WRITE (IOUT, 9003) text2, val1, text2, val2
C
C1c-----RECHARGE.
      IF ( ABS(SFRUZBD(3)).GT.CLOSEZERO .AND. 
     +     (SFRUZBD(3).GE.bigvl1 .OR. SFRUZBD(3).LT.small) ) THEN
        WRITE (val1, '(1PE18.4)') SFRUZBD(3)
      ELSE
        WRITE (val1, '(F18.4)') SFRUZBD(3)
      END IF
      IF ( ABS(SFRUZBD(6)).GT.CLOSEZERO .AND. 
     +     (SFRUZBD(6).GE.bigvl1 .OR. SFRUZBD(6).LT.small) ) THEN
        WRITE (val2, '(1PE18.4)') SFRUZBD(6)
      ELSE
        WRITE (val2, '(F18.4)') SFRUZBD(6)
      END IF
      WRITE (IOUT, 9003) text3, val1, text3, val2
C
C2------SUM INFLOWS AND OUTFLOWS.
      IF ( SFRUZBD(2).GT.0.0 ) THEN
        totvin = SFRUZBD(1)
        totvot = SFRUZBD(3) + SFRUZBD(2)
      ELSE
        totvin = SFRUZBD(1) - SFRUZBD(2)
        totvot = SFRUZBD(3)
      END IF
      IF ( SFRUZBD(5).GT.0.0 ) THEN
        totrin = SFRUZBD(4)
        totrot = SFRUZBD(6) + SFRUZBD(5)
      ELSE
        totrin = SFRUZBD(4) - SFRUZBD(5)
        totrot = SFRUZBD(6)
      END IF
      IF ( ABS(totrin+totrot).GT.CLOSEZERO ) THEN
        prcntdifr = 100.*(totrin-totrot)/(totrin+totrot)/2.0
      ELSE
        prcntdifr = 0.0
      END IF
      IF ( ABS(totvin+totvot).GT.CLOSEZERO ) THEN
        prcntdifv = 100.*(totvin-totvot)/(totvin+totvot)/2.0
      ELSE
        prcntdifv = 0.0
      END IF
C
C3------PRINT TOTALS AND RATES TO GROUND WATER.
      IF ( ABS(totvin).GT.CLOSEZERO .AND. (totvin.GE.bigvl1 .OR. 
     +         totvin.LT.small) ) THEN
        WRITE (val1, '(1PE18.4)') totvin
      ELSE
        WRITE (val1, '(F18.4)') totvin
      END IF
      IF ( ABS(totrin).GT.CLOSEZERO .AND. (totrin.GE.bigvl1 .OR. 
     +     totrin.LT.small) ) THEN
        WRITE (val2, '(1PE18.4)') totrin
      ELSE
        WRITE (val2, '(F18.4)') totrin
      END IF
      WRITE (IOUT, 9004) val1, val2
C
C4------PRINT TOTALS AND RATES FROM GROUND WATER.
      IF ( ABS(totvot).GT.CLOSEZERO .AND. (totvot.GE.bigvl1 .OR. 
     +   totvot.LT.small) ) THEN
        WRITE (val1, '(1PE18.4)') totvot
      ELSE
        WRITE (val1, '(F18.4)') totvot
      END IF
      IF ( ABS(totrot).GT.CLOSEZERO .AND. (totrot.GE.bigvl1 .OR. 
     +     totrot.LT.small) ) THEN
        WRITE (val2, '(1PE18.4)') totrot
      ELSE
        WRITE (val2, '(F18.4)') totrot
      END IF
      WRITE (IOUT, 9006) val1, val2
C
C5------PRINT DIFFERENCES AND PERCENT DIFFERENCES BETWEEN INPUT
C         AND OUTPUT RATES AND VOLUMES.
      diffv = totvin - totvot
      adiffv = ABS(diffv)
      IF ( ABS(adiffv).GT.CLOSEZERO .AND. (adiffv.GE.bigvl2 .OR. 
     +     adiffv.LT.small) ) THEN
        WRITE (val1, '(1PE18.4)') diffv
      ELSE
        WRITE (val1, '(F18.4)') diffv
      END IF
      diffr = totrin - totrot
      adiffr = ABS(diffr)
      IF ( ABS(adiffr).GT.CLOSEZERO .AND. (adiffr.GE.bigvl2 .OR. 
     +     adiffr.LT.small) ) THEN
        WRITE (val2, '(1PE18.4)') diffr
      ELSE
        WRITE (val2, '(F18.4)') diffr
      END IF
      WRITE (IOUT, 9007) val1, val2
      WRITE (IOUT, 9008) prcntdifv, prcntdifr
C
 9001 FORMAT ('1', /2X, 
     +        'VOLUMETRIC BUDGET FOR UNSATURATED ZONE BENEATH ', 
     +        'STREAMS AT END OF TIME STEP', I6, ' STRESS PERIOD', !gsf
     +         I4/2X, 96('-'))
 9002 FORMAT (1X, /4X, 'CUMULATIVE VOLUMES', 13X, 'L**3', 4X, 
     +        'RATES FOR THIS TIME STEP', 9X, 'L**3/T'/4X, 18('-'), 
     +        21X, 24('-')//11X)
 9003 FORMAT (1X, 2X, A18, ' =', A18, 5X, A18, ' =', A18)
 9004 FORMAT (1X, /13X, 'TOTAL IN =', A, 15X, 'TOTAL IN =', A)
 9006 FORMAT (1X, /12X, 'TOTAL OUT =', A, 14X, 'TOTAL OUT =', A)
 9007 FORMAT (1X, /13X, 'IN - OUT =', A, 15X, 'IN - OUT =', A)
 9008 FORMAT (1X, /1X, ' PERCENT DISCREPANCY =', 1X, F15.2, 5X, 
     +        ' PERCENT DISCREPANCY =', 2X, F15.2, ///)
C
C6------RETURN.
      RETURN
      END SUBROUTINE GWF2SFR7UZOT
C
C-------SUBROUTINE GWF2SFR7DPTH
      SUBROUTINE GWF2SFR7DPTH(Flow, Slope, Istsg, Nreach, Roughch, 
     +                        Roughbnk, Wetperm, Depth, Itstr, Totwdth, 
     +                        Iprndpth)
C     ******************************************************************
C     COMPUTE STREAM DEPTH GIVEN FLOW USING 8-POINT CROSS SECTION
!--------REVISED FOR MODFLOW-2005 RELEASE 1.9, FEBRUARY 6, 2012
C     ******************************************************************
      USE GWFSFRMODULE, ONLY: CONST, XSEC, NEARZERO
      USE GLOBAL,       ONLY: IOUT
      IMPLICIT NONE
      INTRINSIC DMIN1, SQRT, DABS
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL Roughbnk, Roughch
      INTEGER Iprndpth, Istsg, Itstr, Nreach
      DOUBLE PRECISION Flow, Slope, Wetperm, Depth, Totwdth
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      INTEGER i, iflg
      DOUBLE PRECISION flow1, flow2, flow3, y0, ymin, xnum, dnom, stage,
     +                 depth1, depth2, depth3, f1, f2, f3, err1, err2, 
     +                 err3
C     ------------------------------------------------------------------
C
C1------INITIALIZE VARIABLES TO ZERO.
      Totwdth = 0.0D0
      Wetperm = 0.0D0
C
C2------FIND LOWEST POINT IN CHANNEL.
      ymin = XSEC(9, Istsg)
      DO i = 9, 16
        y0 = XSEC(i, Istsg)
        ymin = DMIN1(ymin, y0)
      END DO
C
C3------ESTIMATE INITIAL DEPTH ASSUMING WIDE RECTANGULAR CHANNEL.
      depth1 = 0.0D0
      xnum = Flow*Roughch
      dnom = CONST*(XSEC(6, Istsg)-XSEC(3, Istsg))*SQRT(Slope)
      IF ( dnom.GT.0.0 ) depth1 = (xnum/dnom)**0.6D0
      IF ( depth1.GT.0.0D0 ) THEN
        stage = depth1 + ymin
        flow1 = 0.0D0
        CALL GWF2SFR7FLW(stage, Istsg, Roughch, Roughbnk, Slope, 
     +                   Wetperm, flow1, Totwdth)
        f1 = Flow - flow1
        depth2 = 1.1D0*depth1
        stage = depth2 + ymin
        flow2 = 0.0D0
        CALL GWF2SFR7FLW(stage, Istsg, Roughch, Roughbnk, Slope, 
     +                   Wetperm, flow2, Totwdth)
        f2 = Flow - flow2
C
C4------ESTIMATE NEW DEPTH USING EITHER BISECTION OR SECANT METHOD
C         FOR SOLVING ROOTS.
        depth3 = 0.0D0
        IF ( (f1*f2).LT.0.0D0 .AND. 
     +       ((depth1.GT.2.D0*depth2) .OR. (depth2.GT.(2.D0*depth1))) )
     +       THEN
C
C5------USE BISECTION METHOD.
          depth3 = (depth1+depth2)*0.5D0
C
C6------USE SECANT METHOD.
        ELSE
          depth3 = depth2 - (f2*(depth2-depth1)/(f2-f1))
        END IF
        err1 = DABS(depth3-depth1)
        err2 = DABS(depth3-depth2)
        IF ( err1.LT.0.000001 .AND. err2.LT.0.000001 ) THEN
          depth3 = (depth1+depth2)*0.5D0
C
C7------CONTINUE RECALCULATING DEPTH3 UNTIL ERROR LESS THAN 0.000001
C         OR 100 ITERATIONS HAVE BEEN REACHED.
        ELSE
          iflg = 1
          Itstr = 1
          DO WHILE ( iflg.GT.0 )
            IF ( f1*f2.LT.0.0D0 .AND. 
     +           (depth1.GT.2.0D0*depth2 .OR. depth2.GT.2.0D0*depth1) )
     +           THEN
              depth3 = (depth1+depth2)*0.5D0
            ELSE IF ( DABS(f2-f1).GT.0.0D0 ) THEN
              depth3 = depth2 - (f2*(depth2-depth1)/(f2-f1))
            ELSE IF ( DABS(f2-f1).LT.NEARZERO ) THEN
              depth3 = (depth1+depth2)*0.5D0
            END IF
            err1 = DABS(depth3-depth1)
            err2 = DABS(depth3-depth2)
            err3 = DABS(f2-f1)
            IF ( (err1.LT.0.000001 .OR. err2.LT.0.000001) .AND. 
     +           Itstr.GT.2 ) iflg = 0
            IF ( err3.LT.0.0001 .AND. Itstr.GT.2 ) iflg = 0
            stage = depth3 + ymin
            CALL GWF2SFR7FLW(stage, Istsg, Roughch, Roughbnk, Slope, 
     +                       Wetperm, flow3, Totwdth)
            f3 = Flow - flow3
            IF ( f2*f3.LT.0.0D0 ) THEN
C
C8------ROOT IS BETWEEN DEPTH2 AND DEPTH3.
              IF ( f1*f2.LT.0.0D0 ) THEN
C
C9------ROOT IS BETWEEN DEPTH1 AND DEPTH2.
                depth1 = depth3
                f1 = f3
              ELSE
C
C10-----DEPTH1 AND DEPTH2 ON SAME SIDE OF ROOT;
C          REPLACE WITH FARTHER ROOT.
                IF ( DABS(f1).GT.DABS(f2) ) THEN
                  depth1 = depth2
                  f1 = f2
                END IF
                depth2 = depth3
                f2 = f3
              END IF
C
C11-----DEPTH2 AND DEPTH3 ARE ON THE SAME SIDE OF ROOT.
            ELSE IF ( f1*f2.LT.0.0D0 ) THEN
C
C12-----ROOT IS BETWEEN DEPTH1 AND DEPTH2.
              depth2 = depth3
              f2 = f3
            ELSE
C
C13-----DEPTH1,DEPTH2, AND DEPTH3 ARE ON SAME SIDE OF ROOT;
C          REPLACE WITH NEAREST VALUE.
              IF ( DABS(f1).GT.DABS(f2) ) THEN
                depth1 = depth2
                f1 = f2
              END IF
              depth2 = depth3
              f2 = f3
            END IF
            Itstr = Itstr + 1
            IF ( Iprndpth.EQ.1 ) THEN
              WRITE (IOUT, 9001) Itstr, iflg, Flow, depth1, depth2, 
     +                           depth3, f1, f2, f3
 9001         FORMAT (1X/, 'ITSTR,IFLG,FLOW,DEPTH1,DEPTH2,DEPTH3,F1,F2,'
     +                , ',F3 ', 2I5, 7(2X, D15.6))
            END IF
C
C14-----PRINT WARNING MESSAGE IF SECANT METHOD FAILED TO FIND A DEPTH.
            IF ( Itstr.GT.100 ) THEN
              iflg = 0
              WRITE (IOUT, 9002) Istsg, Nreach, depth3, depth1, depth2
 9002         FORMAT (1X/, 'SECANT METHOD FAILED TO FIND SOLUTION FOR', 
     +                ' STREAM SEGMENT ', I5, ' REACH ', I5, 
     +                'ESTIMATED DEPTH IS ', D15.6, 'AND BOUNDS ARE ', 
     +                2(2X, D15.6))
            END IF
          END DO
        END IF
C
C15-----CALCULATE WETTED PERIMETER AND WIDTH FROM FINAL DEPTH.
        Depth = depth3
        stage = Depth + ymin
        flow1 = Flow
        CALL GWF2SFR7FLW(stage, Istsg, Roughch, Roughbnk, Slope, 
     +                   Wetperm, flow1, Totwdth)
      ELSE
        Depth = 0.0D0
        Wetperm = 0.0D0
        Totwdth = 0.0D0
        Itstr = 0
      END IF
C
C16-----RETURN.
      RETURN
      END SUBROUTINE GWF2SFR7DPTH
C
C-------SUBROUTINE GWF2SFR7FLW
      SUBROUTINE GWF2SFR7FLW(Depth, Istsg, Roughch, Roughbnk, Slope, 
     +                       Wetperm, Flow, Totwdth)
C     *******************************************************************
C     COMPUTE FLOW IN STREAM GIVEN DEPTH USING 8-POINT CROSS SECTION
!--------REVISED FOR MODFLOW-2005 RELEASE 1.9, FEBRUARY 6, 2012
C     *******************************************************************
      USE GWFSFRMODULE, ONLY: XSEC, CONST
      IMPLICIT NONE
      INTRINSIC DABS, DSQRT
C     -------------------------------------------------------------------
C     SPECIFICATIONS:
C     -------------------------------------------------------------------
C     ARGUMENTS
C     -------------------------------------------------------------------
      REAL Roughbnk, Roughch
      INTEGER Istsg
      DOUBLE PRECISION Flow, Wetperm, Totwdth, Depth, Slope
C     -------------------------------------------------------------------
C     LOCAL VARIABLES
C     -------------------------------------------------------------------
      REAL fac, r, rough, subarea, wtprm
      INTEGER i, ii, j
      DOUBLE PRECISION xleft, yleft, dpthleft, dpthrght, xright, yright,
     +                 x0, y0, x1, y1, wtprm1, wtprm2, wtprm3, width1, 
     +                 width2, width3, width, subarea1, subarea2, 
     +                 subarea3, subflow1, subflow2, subflow3
C     -------------------------------------------------------------------
C
C1------INITIALIZE VARIABLES TO ZERO.
      Totwdth = 0.0D0
      Wetperm = 0.0D0
      subarea1 = 0.0D0
      subarea2 = 0.0D0
      subarea3 = 0.0D0
      subflow1 = 0.0D0
      subflow2 = 0.0D0
      subflow3 = 0.0D0
      wtprm1 = 0.0D0
      wtprm2 = 0.0D0
      wtprm3 = 0.0D0
      width1 = 0.0D0
      width2 = 0.0D0
      width3 = 0.0D0
      fac = 2./3.
      j = 1
C
C2------INITIALIZE X AND Y POINTS; START AT I = 2.
      DO i = 2, 8
        ii = i + 8
        x0 = XSEC(i-1, Istsg)
        y0 = XSEC(ii-1, Istsg)
        x1 = XSEC(i, Istsg)
        y1 = XSEC(ii, Istsg)
        IF ( Depth.GT.y0 ) THEN
C
C3------LEFT ENDPOINT IS SUBMERGED.
          xleft = x0
          yleft = y0
          dpthleft = Depth - yleft
          IF ( Depth.GT.y1 ) THEN
C
C4------LEFT AND RIGHT ENDPOINTS ARE SUBMERGED.
            xright = x1
            yright = y1
            dpthrght = Depth - yright
          ELSE
C
C5------LEFT ENDPOINT IS SUBMERGED AND RIGHT IS NOT.
            dpthrght = 0.0D0
            yright = Depth
            xright = x0 + (x1-x0)*(yright-y0)/(y1-y0)
          END IF
          width = DABS(xright-xleft)
        ELSE IF ( Depth.GT.y1 ) THEN
C
C6------LEFT ENDPOINT IS ABOVE WATER AND RIGHT IS SUBMERGED.
          dpthleft = 0.0D0
          yleft = Depth
          xleft = x1 - (x1-x0)*(y1-yleft)/(y1-y0)
          xright = x1
          yright = y1
          dpthrght = Depth - yright
          width = DABS(xright-xleft)
        ELSE
C
C7------LEFT AND RIGHT ENDPOINTS ARE ABOVE WATER.
          dpthleft = 0.0D0
          dpthrght = 0.0D0
          width = 0.0D0
        END IF
C
C8------ADD AREA AND WETPERM FOR SUBSECTION OF CHANNEL.
        IF ( dpthleft+dpthrght.GT.0.0 ) THEN
          subarea = 0.5D0*(dpthleft+dpthrght)*width
          wtprm = DSQRT(((xleft-xright)*(xleft-xright))
     +            +((yleft-yright)*(yleft-yright)))
          IF ( j.EQ.1 ) THEN
            subarea1 = subarea1 + subarea
            wtprm1 = wtprm1 + wtprm
            width1 = width1 + width
          END IF
          IF ( j.EQ.2 ) THEN
            subarea2 = subarea2 + subarea
            wtprm2 = wtprm2 + wtprm
            width2 = width2 + width
          END IF
          IF ( j.EQ.3 ) THEN
            subarea3 = subarea3 + subarea
            wtprm3 = wtprm3 + wtprm
            width3 = width3 + width
          END IF
        END IF
C
C9------COMPUTE FLOW FOR EACH SUBSECTION OF CHANNEL.
        IF ( i.EQ.3 .OR. i.EQ.6 .OR. i.EQ.8 ) THEN
          IF ( j.EQ.2 ) THEN
            rough = Roughch
          ELSE
            rough = Roughbnk
          END IF
          IF ( j.EQ.1 .AND. wtprm1.GT.0.0 ) THEN
            r = (subarea1/wtprm1)**fac
            subflow1 = (CONST/rough)*subarea1*r*(Slope)**0.5D0
          ELSE IF ( j.EQ.2 .AND. wtprm2.GT.0.0 ) THEN
            r = (subarea2/wtprm2)**fac
            subflow2 = (CONST/rough)*subarea2*r*(Slope)**0.5D0
          ELSE IF ( j.EQ.3 .AND. wtprm3.GT.0.0 ) THEN
            r = (subarea3/wtprm3)**fac
            subflow3 = (CONST/rough)*subarea3*r*(Slope)**0.5D0
          END IF
          j = j + 1
        END IF
      END DO
C
C10-----SUM FLOW, WETTED PERIMETER, AND WIDTH FOR SUBSECTIONS.
      Flow = subflow1 + subflow2 + subflow3
      Totwdth = width1 + width2 + width3
      Wetperm = wtprm1 + wtprm2 + wtprm3
C
C11-----RETURN.
      RETURN
      END SUBROUTINE GWF2SFR7FLW
C
C-------SUBROUTINE GWF2SFR7TBD
      SUBROUTINE GWF2SFR7TBD(Flow, Depth, Width, Nstrpts, Istsg)
C     *******************************************************************
C     COMPUTE DEPTH AND WIDTH IN STREAM GIVEN FLOW USING RATING TABLES.
!--------REVISED FOR MODFLOW-2005 RELEASE 1.9, FEBRUARY 6, 2012
C     *******************************************************************
      USE GWFSFRMODULE, ONLY: QSTAGE
      IMPLICIT NONE
      INTRINSIC DLOG10
C     -------------------------------------------------------------------
C     SPECIFICATIONS:
C     -------------------------------------------------------------------
C     ARGUMENTS
C     -------------------------------------------------------------------
      INTEGER Istsg, Nstrpts
      DOUBLE PRECISION Flow, Depth, Width
C     -------------------------------------------------------------------
C     LOCAL VARIABLES
C     -------------------------------------------------------------------
      INTEGER iflwlw, istghi, istglw, istp, iwthhi, iwthlw
      DOUBLE PRECISION flwlw, flwhi, stglw, stghi, wthlw, wthhi, dflwlw,
     +                 dflwhi, dstglw, dstghi, dwthlw, dwthhi, dlgflw, 
     +                 dlgsls, dlgslw, dlgstg, dlgwth
C     -------------------------------------------------------------------
C
C1------DEFINE RANGE OF FLOW, DEPTH, AND WIDTH FROM RATING TABLE.
      flwlw = QSTAGE(1, Istsg)
      stglw = QSTAGE(1+Nstrpts, Istsg)
      wthlw = QSTAGE(1+(2*Nstrpts), Istsg)
      flwhi = QSTAGE(Nstrpts, Istsg)
      stghi = QSTAGE(2*Nstrpts, Istsg)
      wthhi = QSTAGE(3*Nstrpts, Istsg)
C
C2------USE A LINEAR INTERPOLATION TO ESTIMATE DEPTH AND WIDTH WHEN
C         FLOW IS LESS THAN LOWEST VALUE IN TABLE.
      IF ( Flow.LE.flwlw ) THEN
        Depth = (stglw/flwlw)*Flow
        Width = (wthlw/flwlw)*Flow
C
C3------OTHERWISE USE A LOG INTERPOLATION TO ESTIMATE DEPTH AND WIDTH.
      ELSE IF ( Flow.GT.flwlw ) THEN
C
C4------FIND NEAREST VALUES OF FLOW, DEPTH, AND WIDTH IN TABLE.
        istp = 2
        flwhi = QSTAGE(istp, Istsg)
        DO WHILE ( Flow.GT.flwhi .AND. istp.LT.Nstrpts )
          istp = istp + 1
          flwhi = QSTAGE(istp, Istsg)
        END DO
        IF ( Flow.LE.flwhi ) THEN
          istghi = istp + Nstrpts
          istglw = istghi - 1
          iwthhi = istp + (2*Nstrpts)
          iwthlw = iwthhi - 1
          iflwlw = istp - 1
          stghi = QSTAGE(istghi, Istsg)
          stglw = QSTAGE(istglw, Istsg)
          wthhi = QSTAGE(iwthhi, Istsg)
          wthlw = QSTAGE(iwthlw, Istsg)
          flwlw = QSTAGE(iflwlw, Istsg)
        ELSE IF ( Flow.GT.flwhi ) THEN
C
C5------COMPUTED FLOW EXCEEDS HIGHEST FLOW IN TABLE.
          flwlw = QSTAGE(Nstrpts-1, Istsg)
          stglw = QSTAGE((2*Nstrpts)-1, Istsg)
          stghi = QSTAGE(2*Nstrpts, Istsg)
          wthlw = QSTAGE((3*Nstrpts)-1, Istsg)
          wthhi = QSTAGE((3*Nstrpts), Istsg)
        END IF
C
C6------COMPUTE DEPTH AND WIDTH FROM LOG INTERPOLATION.
        dstglw = DLOG10(stglw)
        dstghi = DLOG10(stghi)
        dwthlw = DLOG10(wthlw)
        dwthhi = DLOG10(wthhi)
        dflwlw = DLOG10(flwlw)
        dflwhi = DLOG10(flwhi)
        dlgflw = DLOG10(Flow) - dflwlw
        dlgsls = (dstghi-dstglw)/(dflwhi-dflwlw)
        dlgslw = (dwthhi-dwthlw)/(dflwhi-dflwlw)
        dlgstg = dstglw + (dlgsls*dlgflw)
        dlgwth = dwthlw + (dlgslw*dlgflw)
        Depth = 10.D0**dlgstg
        Width = 10.D0**dlgwth
      END IF
C
C7------RETURN.
      RETURN
      END SUBROUTINE GWF2SFR7TBD
C
C-------SUBROUTINE GWF2SFR7TBF
      SUBROUTINE GWF2SFR7TBF(Flow, Depth, Width, Nstrpts, Nreach, Istsg,
     +                       Kkiter, Itb)
C     *******************************************************************
C     COMPUTE FLOW AND WIDTH IN STREAM GIVEN DEPTH USING RATING TABLES.
!--------REVISED FOR MODFLOW-2005 RELEASE 1.9, FEBRUARY 6, 2012
C     *******************************************************************
      USE GWFSFRMODULE, ONLY: QSTAGE
!!      USE GWFSFRMODULE, ONLY: QSTAGE, DVRPERC
      USE GLOBAL,       ONLY: IOUT
      IMPLICIT NONE
      INTRINSIC DLOG10
C     -------------------------------------------------------------------
C     SPECIFICATIONS:
C     -------------------------------------------------------------------
C     ARGUMENTS
C     -------------------------------------------------------------------
      INTEGER Istsg, Itb, Kkiter, Nreach, Nstrpts
      DOUBLE PRECISION Flow, Depth, Width
C     -------------------------------------------------------------------
C     LOCAL VARIABLES
C     -------------------------------------------------------------------
      INTEGER iflwhi, iflwlw, istglw, istp, iwthhi, iwthlw
      DOUBLE PRECISION flwlw, flwhi, stglw, stghi, wthlw, wthhi, dflwlw,
     +                 dflwhi, dstglw, dstghi, dwthlw, dwthhi, dlgflw, 
     +                 dlgslf, dlgslw, dlgstg, dlgwth
!!      REAL dum, totdum
C     -------------------------------------------------------------------
C
C1------DEFINE RANGE OF FLOW, DEPTH, AND WIDTH FROM RATING TABLE.
      flwlw = QSTAGE(1, Istsg)
      stglw = QSTAGE(1+Nstrpts, Istsg)
      wthlw = QSTAGE(1+(2*Nstrpts), Istsg)
      flwhi = QSTAGE(Nstrpts, Istsg)
      stghi = QSTAGE(2*Nstrpts, Istsg)
      wthhi = QSTAGE(3*Nstrpts, Istsg)
C
C2------USE A LINEAR INTERPOLATION TO ESTIMATE FLOW AND WIDTH WHEN
C         DEPTH IS LESS THAN LOWEST VALUE IN TABLE.
      IF ( Depth.LE.stglw ) THEN
        Flow = (flwlw/stglw)*Depth
        Width = (wthlw/flwlw)*Flow
C
C3------OTHERWISE USE A LOG INTERPOLATION TO ESTIMATE FLOW AND WIDTH.
      ELSE IF ( Depth.GT.stglw ) THEN
        istp = 2
        stghi = QSTAGE(istp+Nstrpts, Istsg)
C
C4------FIND NEAREST VALUES OF FLOW, DEPTH, AND WIDTH IN TABLE.
        DO WHILE ( Depth.GT.stghi .AND. istp.LT.Nstrpts )
          istp = istp + 1
          stghi = QSTAGE(istp+Nstrpts, Istsg)
        END DO
        IF ( Depth.LE.stghi ) THEN
          istglw = (istp-1) + Nstrpts
          iflwhi = istp
          iflwlw = istp - 1
          iwthhi = istp + (2*Nstrpts)
          iwthlw = iwthhi - 1
          stglw = QSTAGE(istglw, Istsg)
          wthhi = QSTAGE(iwthhi, Istsg)
          wthlw = QSTAGE(iwthlw, Istsg)
          flwlw = QSTAGE(iflwlw, Istsg)
          flwhi = QSTAGE(iflwhi, Istsg)
        ELSE IF ( Depth.GT.stghi .AND. Itb.EQ.1 ) THEN
C
C5------PRINT WARNING IF COMPUTED DEPTH EXCEEDS HIGHEST DEPTH IN TABLE.
          WRITE (IOUT, 9001) Kkiter, Istsg, Nreach, Depth, stghi
 9001     FORMAT (1X/, 'FOR MODFLOW ITERATION ', I5, 
     +            ' DEPTH IN SEGMENT ', I5, ' REACH ', I5, ' IS ', 
     +            1PE10.3, ' AND IS GREATER THAN ', 
     +            'HIGHEST DEPTH LISTED IN RATING TABLE OF ', 1PE10.3, 
     +            //1X, 'ASSUMING SAME RELATION AS ', 
     +            'BETWEEN TWO HIGHEST DEPTHS IN TABLE'//)
          flwlw = QSTAGE(Nstrpts-1, Istsg)
          stglw = QSTAGE((2*Nstrpts)-1, Istsg)
          stghi = QSTAGE(2*Nstrpts, Istsg)
          wthlw = QSTAGE((3*Nstrpts)-1, Istsg)
          wthhi = QSTAGE((3*Nstrpts), Istsg)
          flwlw = QSTAGE(Nstrpts-1, Istsg)
          flwhi = QSTAGE(Nstrpts, Istsg)
          stglw = QSTAGE((2*Nstrpts)-1, Istsg)
          stghi = QSTAGE((2*Nstrpts), Istsg)
          wthlw = QSTAGE((3*Nstrpts)-1, Istsg)
          wthhi = QSTAGE((3*Nstrpts), Istsg)
        END IF
C
C6------COMPUTE DEPTH AND WIDTH FROM LOG INTERPOLATION.
        dstglw = DLOG10(stglw)
        dstghi = DLOG10(stghi)
        dwthlw = DLOG10(wthlw)
        dwthhi = DLOG10(wthhi)
        dflwlw = DLOG10(flwlw)
        dflwhi = DLOG10(flwhi)
        dlgstg = DLOG10(Depth) - dstglw
        dlgslf = (dflwhi-dflwlw)/(dstghi-dstglw)
        dlgslw = (dwthhi-dwthlw)/(dstghi-dstglw)
        dlgflw = dflwlw + (dlgslf*dlgstg)
        dlgwth = dwthlw + (dlgslw*dlgstg)
        Flow = 10.D0**dlgflw
        Width = 10.D0**dlgwth
      END IF
C
C7------RETURN.
      RETURN
      END SUBROUTINE GWF2SFR7TBF
C
C-------SUBROUTINE SGWF2SFR7RDSEG
      SUBROUTINE SGWF2SFR7RDSEG(Nlst, Lstbeg, In, Iunitgwt, Iunituzf, 
     +                          Ischk, Nischk, Ichk, Kkper, Nsol)
C     ******************************************************************
C     READ STREAM SEGMENT DATA -- parameters or non parameters
!--------REVISED FOR MODFLOW-2005 RELEASE 1.9, FEBRUARY 6, 2012
C     ******************************************************************
      USE GWFSFRMODULE, ONLY: NSS, MAXPTS, ISFROPT, IDIVAR, IOTSG, ISEG,
     +                        SEG, XSEC, QSTAGE, CONCQ, CONCRUN,CONCPPT
!!      USE GWFSFRMODULE, ONLY: NSS, MAXPTS, ISFROPT, IDIVAR, IOTSG, ISEG,
!!     +                        SEG, XSEC, QSTAGE, CONCQ, CONCRUN,CONCPPT,
!!     +                        DVRCH, DVRCELL, RECHSAVE, DVEFF, DVRPERC  !cjm (added DVRCH, DVRCELL and RECHSAVE)
      USE GLOBAL,       ONLY: IOUT
!      USE GWFUZFMODULE, ONLY: FINF
!      USE GWFRCHMODULE,ONLY: RECH  !cjm
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER Iunitgwt, Ichk, In, Ischk, Lstbeg, Nischk, Nlst, Kkper
      INTEGER Nsol, Iunituzf
      DIMENSION Ischk(Nischk)
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      INTEGER icalc, idum, ii, iqseg, isol, iupseg, jj, jk, lstend, n, 
     +        noutseg, nseg, nstrpts !!, numcell, i  !cjm (added numcell and i)
!!      REAL dum, totdum
C     ------------------------------------------------------------------
C
C1------READ STREAM SEGMENT DATA.
      lstend = Lstbeg + Nlst - 1
      DO iqseg = Lstbeg, lstend
!          DVRCELL = 0.0
!          DVRPERC = 0.0
!          DVRCH = 0.0
C
C2------ONLY READ FIRST 4 VARIABLES TO DETERMINE VALUE OF IUPSEG.
        READ (In, *) n, icalc, noutseg, iupseg
!       IF ( n.GT.NSS .OR. n.LT.1 ) THEN  !cjm (commented this line out)
        IF ( n.GT.NSS .OR. n.EQ.0 ) THEN              !cjm
          WRITE (IOUT, 9001) n
 9001     FORMAT (1X, /1X, 'SEGMENT NUMBER (NSEG) OUT OF RANGE: ', I6)
          IF ( Ichk.NE.0 ) THEN
            WRITE (IOUT, 9002) iqseg - Lstbeg + 1
 9002       FORMAT (1X, 'READING ENTRY ', I6, ' OF ITEM 6A')
          ELSE
            WRITE (IOUT, 9003) iqseg - Lstbeg + 1
 9003       FORMAT (1X, 'READING ENTRY ', I6, ' OF ITEM 4A')
          END IF
          CALL USTOP(' ')
        END IF
C        
C2a-----DETERMINE IF SEGMENT OUTFLOW WILL BE DIVERTED TO RECHARGE MF CELLS  !cjm
        IF ( N.LT.0 ) THEN
          N = ABS(N)
   !       DVRCH(N) = 1
	  !ELSE              !cjm 20090708
	  !  DVRCH(N) = 0       !cjm 20090708
        END IF
C
C3------DETERMINE WHERE DATA ARE STORED.
        IF ( Ichk.NE.0 ) THEN
C  Store data in active segment area
          nseg = n
          Ischk(n) = Ischk(n) + 1
        ELSE
C  Store data in parameter area
          nseg = iqseg
          ISEG(3, iqseg) = n
          SEG(1, nseg) = SEG(1, n)
        END IF
        BACKSPACE In
C
C4------READ DATA SET 4B FOR SEGMENTS THAT ARE NOT DIVERSIONS.
        IF ( iupseg.LE.0 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) idum, ISEG(1, nseg), IOTSG(nseg), 
     +                   IDIVAR(1, nseg), (SEG(jj, nseg), jj=2, 5)
          ELSE IF ( icalc.EQ.1 ) THEN
            READ (In, *) idum, ISEG(1, nseg), IOTSG(nseg), 
     +                   IDIVAR(1, nseg), (SEG(jj, nseg), jj=2, 5), 
     +                   SEG(16, nseg)
          ELSE IF ( icalc.EQ.2 ) THEN
            READ (In, *) idum, ISEG(1, nseg), IOTSG(nseg), 
     +                   IDIVAR(1, nseg), (SEG(jj, nseg), jj=2, 5), 
     +                   (SEG(jk, nseg), jk=16, 17)
          ELSE IF ( icalc.EQ.3 ) THEN
            READ (In, *) idum, ISEG(1, nseg), IOTSG(nseg), 
     +                   IDIVAR(1, nseg), (SEG(jj, nseg), jj=2, 5), 
     +                   SEG(9, nseg), SEG(10, nseg), SEG(14, nseg), 
     +                   SEG(15, nseg)
          ELSE IF ( icalc.EQ.4 ) THEN
            READ (In, *) idum, ISEG(1, nseg), IOTSG(nseg), 
     +                   IDIVAR(1, nseg), ISEG(2, nseg), 
     +                   (SEG(jj, nseg), jj=2, 5)
          END IF
C
C5------READ DATA 4B FOR SEGMENTS THAT ARE DIVERSIONS FROM STREAMS.
        ELSE IF ( icalc.LE.0 ) THEN
          READ (In, *) idum, ISEG(1, nseg), IOTSG(nseg), 
     +                 (IDIVAR(ii, nseg), ii=1, 2), 
     +                 (SEG(jj, nseg), jj=2, 5)
        ELSE IF ( icalc.EQ.1 ) THEN
          READ (In, *) idum, ISEG(1, nseg), IOTSG(nseg), 
     +                 (IDIVAR(ii, nseg), ii=1, 2), 
     +                 (SEG(jj, nseg), jj=2, 5), SEG(16, nseg)
        ELSE IF ( icalc.EQ.2 ) THEN
          READ (In, *) idum, ISEG(1, nseg), IOTSG(nseg), 
     +                 (IDIVAR(ii, nseg), ii=1, 2), 
     +                 (SEG(jj, nseg), jj=2, 5), 
     +                 (SEG(jk, nseg), jk=16, 17)
        ELSE IF ( icalc.EQ.3 ) THEN
          READ (In, *) idum, ISEG(1, nseg), IOTSG(nseg), 
     +                 (IDIVAR(ii, nseg), ii=1, 2), 
     +                 (SEG(jj, nseg), jj=2, 5), SEG(9, nseg), 
     +                 SEG(10, nseg), SEG(14, nseg), SEG(15, nseg)
        ELSE IF ( icalc.EQ.4 ) THEN
          READ (In, *) idum, ISEG(1, nseg), IOTSG(nseg), 
     +                 (IDIVAR(ii, nseg), ii=1, 2), ISEG(2, nseg), 
     +                 (SEG(jj, nseg), jj=2, 5)
        END IF
C
C6------READ DATA SET 4C.
        IF ( ISFROPT.EQ.0 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) (SEG(jj, nseg), jj=6, 10)
          ELSE IF ( icalc.EQ.1 ) THEN
            READ (In, *) (SEG(jj, nseg), jj=6, 9)
          ELSE IF ( icalc.GE.2 .AND. icalc.LE.4 ) THEN
            READ (In, *) (SEG(jj, nseg), jj=6, 8)
          END IF
        ELSE IF ( ISFROPT.EQ.1 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) SEG(9, nseg), SEG(10, nseg)
          ELSE IF ( icalc.EQ.1 ) THEN
            READ (In, *) SEG(9, nseg)
          END IF
        ELSE IF ( ISFROPT.EQ.2 .OR. ISFROPT.EQ.3 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) SEG(9, nseg), SEG(10, nseg)
          ELSE IF ( icalc.EQ.1 .AND. Kkper.EQ.1 ) THEN
            READ (In, *) SEG(9, nseg)
          END IF
        ELSE IF ( ISFROPT.EQ.4 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) (SEG(jj, nseg), jj=6, 10)
          ELSE IF ( icalc.EQ.1 ) THEN
            IF ( Kkper.EQ.1 ) THEN
              READ (In, *) (SEG(jj, nseg), jj=6, 9), 
     +                     (SEG(jj, nseg), jj=18, 20)
            ELSE
              READ (In, *) SEG(6, nseg)
            END IF
          ELSE IF ( icalc.EQ.2 ) THEN
            IF ( Kkper.EQ.1 ) THEN
              READ (In, *) (SEG(jj, nseg), jj=6, 8), 
     +                     (SEG(jj, nseg), jj=18, 20)
            ELSE
              READ (In, *) SEG(6, nseg)
            END IF
          ELSE IF ( icalc.GE.3 .AND. icalc.LE.4 ) THEN
            READ (In, *) (SEG(jj, nseg), jj=6, 8)
          END IF
        ELSE IF ( ISFROPT.EQ.5 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) (SEG(jj, nseg), jj=6, 10)
          ELSE IF ( icalc.EQ.1 ) THEN
            IF ( Kkper.EQ.1 ) THEN
              READ (In, *) (SEG(jj, nseg), jj=6, 9), 
     +                     (SEG(jj, nseg), jj=18, 21)
            ELSE
              READ (In, *) SEG(6, nseg)
            END IF
          ELSE IF ( icalc.EQ.2 ) THEN
            IF ( Kkper.EQ.1 ) THEN
              READ (In, *) (SEG(jj, nseg), jj=6, 8), 
     +                     (SEG(jj, nseg), jj=18, 21)
            ELSE
              READ (In, *) SEG(6, nseg)
            END IF
          ELSE IF ( icalc.GE.3 .AND. icalc.LE.4 ) THEN
            READ (In, *) (SEG(jj, nseg), jj=6, 8)
          END IF
        END IF
C
C7------READ DATA SET 4D.
        IF ( ISFROPT.EQ.0 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) (SEG(jj, nseg), jj=11, 15)
          ELSE IF ( icalc.EQ.1 ) THEN
            READ (In, *) (SEG(jj, nseg), jj=11, 14)
          ELSE IF ( icalc.GE.2 .AND. icalc.LE.4 ) THEN
            READ (In, *) (SEG(jj, nseg), jj=11, 13)
          END IF
        ELSE IF ( ISFROPT.EQ.1 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) SEG(14, nseg), SEG(15, nseg)
          ELSE IF ( icalc.EQ.1 ) THEN
            READ (In, *) SEG(14, nseg)
          END IF
        ELSE IF ( ISFROPT.EQ.2 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) SEG(14, nseg), SEG(15, nseg)
          ELSE IF ( icalc.EQ.1 .AND. Kkper.EQ.1 ) THEN
            READ (In, *) SEG(14, nseg)
          END IF
        ELSE IF ( ISFROPT.EQ.3 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) SEG(14, nseg), SEG(15, nseg)
          ELSE IF ( icalc.EQ.1 .AND. Kkper.EQ.1 ) THEN
            READ (In, *) SEG(14, nseg)
          END IF
        ELSE IF ( ISFROPT.EQ.4 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) (SEG(jj, nseg), jj=11, 15)
          ELSE IF ( icalc.EQ.1 ) THEN
            IF ( Kkper.EQ.1 ) THEN
              READ (In, *) (SEG(jj, nseg), jj=11, 14), 
     +                     (SEG(jj, nseg), jj=22, 24)
            ELSE
              READ (In, *) SEG(11, nseg)
            END IF
          ELSE IF ( icalc.EQ.2 ) THEN
            IF ( Kkper.EQ.1 ) THEN
              READ (In, *) (SEG(jj, nseg), jj=11, 13), 
     +                     (SEG(jj, nseg), jj=22, 24)
            ELSE
              READ (In, *) SEG(11, nseg)
            END IF
          ELSE IF ( icalc.GE.3 .AND. icalc.LE.4 ) THEN
            READ (In, *) (SEG(jj, nseg), jj=11, 13)
          END IF
        ELSE IF ( ISFROPT.EQ.5 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) (SEG(jj, nseg), jj=11, 15)
          ELSE IF ( icalc.EQ.1 ) THEN
            IF ( Kkper.EQ.1 ) THEN
              READ (In, *) (SEG(jj, nseg), jj=11, 14), 
     +                     (SEG(jj, nseg), jj=22, 25)
            ELSE
              READ (In, *) SEG(11, nseg)
            END IF
          ELSE IF ( icalc.EQ.2 ) THEN
            IF ( Kkper.EQ.1 ) THEN
              READ (In, *) (SEG(jj, nseg), jj=11, 13), 
     +                     (SEG(jj, nseg), jj=22, 25)
            ELSE
              READ (In, *) SEG(11, nseg)
            END IF
          ELSE IF ( icalc.GE.3 .AND. icalc.LE.4 ) THEN
            READ (In, *) (SEG(jj, nseg), jj=11, 13)
          END IF
        END IF
C
C8------READ DATA SET 4E FOR SEGMENT WHEN ICALC IS 2.
        IF ( icalc.EQ.2 ) THEN
C       ADDED CONDITIONAL IF WHEN UNSATURATED FLOW INACTIVE DEP
          IF ( Kkper.EQ.1 .OR. ISFROPT.LE.1 ) THEN
            READ (In, *) (XSEC(jj, nseg), jj=1, 8)
            READ (In, *) (XSEC(jj, nseg), jj=9, 16)
          END IF
        END IF
C
C9------READ DATA SET 4F FOR SEGMENT WHEN ICALC IS 4.
        IF ( icalc.EQ.4 ) THEN
          nstrpts = ISEG(2, nseg)
          IF ( nstrpts.LT.2 ) THEN
            WRITE (IOUT, 9004) n
 9004       FORMAT (/1X, 'NUMBER OF POINTS USED TO RELATE ', 
     +              'STREAMFLOW WITH STREAM DEPTH AND WIDTH FOR ', 
     +              'SEGMENT ', I6, ' IS LESS THAN TWO'//1X, 
     +              'PROGRAM STOPPING')
            CALL USTOP(' ')
          ELSE IF ( nstrpts.GT.MAXPTS/3 ) THEN
            WRITE (IOUT, 9005) n, nstrpts
 9005       FORMAT (/1X, 'FOR SEGMENT ', I6, ' NUMBER OF POINTS', 
     +              'USED TO RELATE STREAMFLOW WITH DEPTH AND ', 
     +              'WIDTH IS ', I5//1X, 'WHICH IS MORE THAN ', 
     +              'MAXIMUM NUMBER OF 50 POINTS', //1X, 
     +              'PROGRAM STOPPING'//)
            CALL USTOP(' ')
          ELSE
            READ (In, *) (QSTAGE(jj, nseg), jj=1, nstrpts)
            READ (In, *) (QSTAGE(jj, nseg), jj=nstrpts+1, 2*nstrpts)
            READ (In, *) (QSTAGE(jj, nseg), jj=2*nstrpts+1, 3*nstrpts)
          END IF
        END IF
C
C10-----READ DATA SET 4G FOR SEGMENT IF SOLUTES SPECIFIED.
        IF ( Iunitgwt.GT.0 ) THEN
          DO isol = 1, Nsol
            IF ( IDIVAR(1, nseg).EQ.0 ) THEN
              READ (In, *) CONCQ(nseg, isol), CONCRUN(nseg, isol), 
     +                     CONCPPT(nseg, isol)
            ELSE
              READ (In, *) CONCRUN(nseg, isol), CONCPPT(nseg, isol)
            END IF
          END DO
        END IF
C
C10b----READ CELL INDECES THAT RECEIVE RECHARGE: i,1 = ROW, i,2 = COL  !cjm
!        IF ( DVRCH(N).GT.0 ) THEN
!! Set old values to zero
!          READ(In, *)DVRCH(N),DVEFF(N)
!          totdum = 0.0
!          DO i = 1, DVRCH(N)
!            READ(In, *) DVRCELL(i,1,N),DVRCELL(i,2,N),dum
!            DVRPERC(DVRCELL(i,2,N),DVRCELL(i,1,N)) = dum
!            totdum = totdum + dum
!          END DO
!          IF ( totdum.GT.1.000001 ) WRITE(Iout,9006)totdum
!        END IF
!C
      END DO
! 9006 FORMAT(' ***Warning in SFR2*** ',/
!     1       'Fraction of diversion for each cell in group sums '/,
!     1       'to a value greater than one. Sum = ',E10.5)
C
C11-----RETURN.
      RETURN
      END SUBROUTINE SGWF2SFR7RDSEG
C
C-------SUBROUTINE SGWF2SFR7PARMOV
      SUBROUTINE SGWF2SFR7PARMOV(In, Iunitgwt, Nsol)
C     ******************************************************************
C     MOVE STREAM PARAMETER DATA INTO ACTIVE SEGMENTS
!--------REVISED FOR MODFLOW-2005 RELEASE 1.9, FEBRUARY 6, 2012
C     ******************************************************************
      USE GWFSFRMODULE, ONLY: IDIVAR, IOTSG, ISEG, SEG, XSEC, QSTAGE, 
     +                        CONCQ, CONCRUN, CONCPPT, NSEGCK
      USE GLOBAL,       ONLY: IOUT
      USE PARAMMODULE,  ONLY: IACTIVE, IPLOC, PARNAM, INAME, B
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER In, Iunitgwt, Nsol
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      REAL rdum
      INTEGER icalc, idum, iloc, ip, iqseg, isol, istart, istop, iupseg,
     +        jend, jj, ki, lloc, lstend, ni, nlst, nseg, nstrpts, 
     +        numinst, lstbeg
      CHARACTER*4 package
      CHARACTER*200 line
      CHARACTER*10 pname, ctmp3, ctmp4
C     ------------------------------------------------------------------
C
      package = 'SFR '
C
C1------READ PARAMETER NAME AND FIND IT IN THE PARAMETER LIST.
      READ (In, '(A)') line
      lloc = 1
      CALL URWORD(line, lloc, istart, istop, 0, idum, rdum, IOUT, In)
      pname = line(istart:istop)
      WRITE (IOUT, 9001) pname
 9001 FORMAT (/, ' Parameter:  ', A)
      CALL UPARFIND(pname, 'SFR', 'SFR', ip, IOUT)
C
C2------DESIGNATE CELLS CORRESPONDING TO CORRECT PARAMETER INSTANCE.
      nlst = IPLOC(2, ip) - IPLOC(1, ip) + 1
      numinst = IPLOC(3, ip)
      iloc = IPLOC(4, ip)
      ni = 1
      IF ( numinst.GT.0 ) THEN
        nlst = nlst/numinst
        CALL URWORD(line, lloc, istart, istop, 0, idum, rdum, IOUT, In)
        ctmp3 = line(istart:istop)
        IF ( ctmp3.EQ.' ' ) THEN
          WRITE (IOUT, 9002) package, PARNAM(ip)
 9002     FORMAT (/, 1X, 'Blank instance name in the ', A, 
     +            ' file for parameter ', A)
          CALL USTOP(' ')
        END IF
        WRITE (IOUT, 9003) ctmp3
 9003   FORMAT (3X, 'Instance:  ', A)
        CALL UPCASE(ctmp3)
        DO ki = 1, numinst
          ctmp4 = INAME(iloc+ki-1)
          CALL UPCASE(ctmp4)
          IF ( ctmp3.EQ.ctmp4 ) THEN
            ni = ki
            GOTO 100
          END IF
        END DO
        WRITE (IOUT, 9004) package, ctmp3, PARNAM(ip)
 9004   FORMAT (/, 1X, 'The ', A, 
     +          ' file specifies undefined instance "', A, 
     +          '" for parameter ', A)
        CALL USTOP(' ')
      END IF
C
 100  IF ( IACTIVE(ip).GT.0 ) THEN
        WRITE (IOUT, 9005) PARNAM(ip)
 9005   FORMAT (/, 1X, '*** ERROR: PARAMETER "', A, 
     +          '" HAS ALREADY BEEN ACTIVATED THIS STRESS PERIOD', /, 
     +          ' -- STOP EXECUTION (SGWF2SFR7PARMOV)')
        CALL USTOP(' ')
      END IF
C
      IACTIVE(ip) = ni
C
C3------MOVE EACH ENTRY FOR THE PARAMETER.
      lstbeg = IPLOC(1, ip) + (ni-1)*nlst
C changed iqseg to lstbeg in the following line. 4/25/06
      lstend = lstbeg + nlst - 1
      DO iqseg = lstbeg, lstend
C
C4------DETERMINE VALUES OF ICALC, NSEG, AND IUPSEG.
        icalc = ISEG(1, iqseg)
        nseg = ISEG(3, iqseg)
        iupseg = IDIVAR(1, iqseg)
C
C5------COUNT THE NUMBER OF TIMES A SEGMENT IS DEFINED.
        NSEGCK(nseg) = NSEGCK(nseg) + 1
C
C6------MOVE DATA SET 4A.
        ISEG(1, nseg) = ISEG(1, iqseg)
        IOTSG(nseg) = IOTSG(iqseg)
        IDIVAR(1, nseg) = IDIVAR(1, iqseg)
        IF ( iupseg.GT.0 ) IDIVAR(2, nseg) = IDIVAR(2, iqseg)
        SEG(2, nseg) = SEG(2, iqseg)
        SEG(3, nseg) = SEG(3, iqseg)
        SEG(4, nseg) = SEG(4, iqseg)
        SEG(5, nseg) = SEG(5, iqseg)
        IF ( icalc.EQ.1 ) THEN
          SEG(16, nseg) = SEG(16, iqseg)
        ELSE IF ( icalc.EQ.2 ) THEN
          SEG(16, nseg) = SEG(16, iqseg)
          SEG(17, nseg) = SEG(17, iqseg)
        ELSE IF ( icalc.EQ.3 ) THEN
          SEG(9, nseg) = SEG(9, iqseg)
          SEG(10, nseg) = SEG(10, iqseg)
          SEG(14, nseg) = SEG(14, iqseg)
          SEG(15, nseg) = SEG(15, iqseg)
        ELSE IF ( icalc.EQ.4 ) THEN
          ISEG(2, nseg) = ISEG(2, iqseg)
        END IF
C
C7------MOVE DATA SET 4B.
        IF ( icalc.LE.0 ) THEN
          jend = 10
        ELSE IF ( icalc.EQ.1 ) THEN
          jend = 9
        ELSE IF ( icalc.GE.2 .AND. icalc.LE.4 ) THEN
          jend = 8
        END IF
        DO jj = 6, jend
          SEG(jj, nseg) = SEG(jj, iqseg)
        END DO
        SEG(6, nseg) = SEG(6, nseg)*B(ip)
C
C8------MOVE DATA SET 4C.
        IF ( icalc.LE.0 ) THEN
          jend = 15
        ELSE IF ( icalc.EQ.1 ) THEN
          jend = 14
        ELSE IF ( icalc.GE.2 .AND. icalc.LE.4 ) THEN
          jend = 13
        END IF
        DO jj = 11, jend
          SEG(jj, nseg) = SEG(jj, iqseg)
        END DO
        SEG(11, nseg) = SEG(11, nseg)*B(ip)
C
C9------MOVE DATA SET 4D FOR SEGMENT WHEN ICALC IS 2.
        IF ( icalc.EQ.2 ) THEN
          DO jj = 1, 16
            XSEC(jj, nseg) = XSEC(jj, iqseg)
          END DO
        END IF
C
C10-----MOVE DATA SET 4E FOR SEGMENT WHEN ICALC IS 4.
        IF ( icalc.EQ.4 ) THEN
          nstrpts = ISEG(2, nseg)
          DO jj = 1, nstrpts*3
            QSTAGE(jj, nseg) = QSTAGE(jj, iqseg)
          END DO
        END IF
C
C11-----MOVE DATA SET 4F FOR SEGMENT IF SOLUTES SPECIFIED.
        IF ( Iunitgwt.GT.0 ) THEN
          DO isol = 1, Nsol
            IF ( IDIVAR(1, nseg).EQ.0 ) THEN
              CONCQ(nseg, isol) = CONCQ(iqseg, isol)
              CONCRUN(nseg, isol) = CONCRUN(iqseg, isol)
              CONCPPT(nseg, isol) = CONCPPT(iqseg, isol)
            ELSE
              CONCRUN(nseg, isol) = CONCRUN(iqseg, isol)
              CONCPPT(nseg, isol) = CONCPPT(iqseg, isol)
            END IF
          END DO
        END IF
C
      END DO
C12-----RETURN.
      RETURN
      END SUBROUTINE SGWF2SFR7PARMOV
C
C-------SUBROUTINE SGWF2SFR7PRSEG
      SUBROUTINE SGWF2SFR7PRSEG(Nlst, Lstbeg, Iunitgwt, Kkper, Nsol,
     +                          Iouts)
C     ******************************************************************
C     PRINT STREAM SEGMENT DATA -- parameters or non parameters
!--------REVISED FOR MODFLOW-2005 RELEASE 1.9, FEBRUARY 6, 2012
C     ******************************************************************
      USE GWFSFRMODULE, ONLY: ISFROPT, IDIVAR, IOTSG, ISEG, SEG, XSEC,
     +                        QSTAGE, CONCQ, CONCRUN, CONCPPT
      USE GLOBAL,       ONLY: IOUT
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER Nlst, Lstbeg, Iunitgwt, Kkper, Nsol, Iouts
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      INTEGER i, icalc, iflg, ii, ipt, isol, jj, lstend, nn, nseg, 
     +        nstrpts
C     ------------------------------------------------------------------
C
      lstend = Nlst + Lstbeg - 1
      WRITE (IOUT, 9001)
 9001 FORMAT (1X, //20X, 'INPUT DATA FOR EACH STREAM SEGMENT', /1X, 
     +        93('-')/)
C
C1------PRINT INPUT FLOW RATES FOR EACH STREAM SEGMENT.
      WRITE (IOUT, 9002)
 9002 FORMAT (1X, 'SEGMENT    SEG.     INFLOW   OVERLAND   ', 
     +        'STREAM    STREAM   ICALC  OUTFLOW  DIVERSION PRIORITY', 
     +        /4X, 'NO.    LENGTH     RATE     RUNOFF      ', 
     +        'ET       PPT.    METH.  TO SEG.  FROM SEG.    NO.'/)
      DO nseg = Lstbeg, lstend
        IF ( Lstbeg.EQ.1 ) THEN
          nn = nseg
        ELSE
          nn = ISEG(3, nseg)
        END IF
        WRITE (IOUT, 9003) nn, (SEG(ii, nseg), ii=1, 5), ISEG(1, nseg), 
     +                     IOTSG(nseg), (IDIVAR(jj, nseg), jj=1, 2)
 9003   FORMAT (1X, I6, 1X, 1P5E10.3, 2X, I3, 3X, I6, 3X, I6, 4X, I5)
      END DO
C
C2------PRINT STREAMBED PROPERTIES AND STREAM DIMENSIONS.
      IF ( Lstbeg.EQ.1 ) THEN
        IF ( ISFROPT.EQ.0 ) THEN
          WRITE (IOUT, 9004)
        ELSE IF ( ISFROPT.GT.0 .AND. ISFROPT.LT.4 ) THEN
          WRITE (IOUT, 9005)
        ELSE IF ( ISFROPT.EQ.4 ) THEN
          IF ( Kkper.EQ.1 ) THEN
            WRITE (IOUT, 9006)
          ELSE
            WRITE (IOUT, 9004)
          END IF
        ELSE IF ( ISFROPT.EQ.5 ) THEN
          IF ( Kkper.EQ.1 ) THEN
            WRITE (IOUT, 9008)
          ELSE
            WRITE (IOUT, 9004)
          END IF
        END IF
c rgn added else and write statement.
      ELSE
        WRITE(IOUT,210)
  210   FORMAT (1X,//9X,'STREAMBED PROPERTIES AND STREAM ',
     1        'DIMENSIONS',//1X,'SEGMENT  BED HYD. COND. FACTOR',2X,
     2        'BED THICKNESS     ELEV.-TOP OF BED     WIDTH OF ',
     3        'STREAM     DEPTH OF STREAM    STREAM ROUGHNESS',/1X,
     4        '   No.     UPPER     LOWER     UPPER     ',
     5        'LOWER     UPPER     LOWER     UPPER     LOWER     ',
     6        'UPPER     LOWER   CHANNEL      BANK'/)
      END IF
 9004 FORMAT (1X, //9X, 'STREAMBED PROPERTIES AND STREAM DIMENSIONS', //
     +        ' SEGMENT     BED HYD. COND.', 6X, 
     +        'BED THICKNESS     ELEV.-TOP OF BED     WIDTH OF ', 
     +        'STREAM     DEPTH OF STREAM    STREAM ROUGHNESS', /,
     +        '    No.     UPPER     LOWER     UPPER     ', 
     +        'LOWER     UPPER     LOWER     UPPER     LOWER     ', 
     +        'UPPER     LOWER   CHANNEL      BANK'/)
 9005 FORMAT (1X, //9X, 'STREAMBED PROPERTIES AND STREAM DIMENSIONS', //
     +        ' SEGMENT     WIDTH OF STREAM', 5X,
     +        'DEPTH OF STREAM    STREAM ROUGHNESS', /, 
     +        '    No.     UPPER     LOWER     UPPER     ', 
     +        'LOWER     CHANNEL      BANK'/)
 9006 FORMAT (1X, //9X, 'STREAMBED PROPERTIES AND STREAM DIMENSIONS', //
     +        ' SEGMENT     BED HYD. COND.', 6X, 
     +        'BED THICKNESS     ELEV.-TOP OF BED     WIDTH OF ', 
     +        'STREAM     DEPTH OF STREAM    STREAM ROUGHNESS  ', 
     +        '  SAT. WATER CONT.    INT. WATER CONT.    BROOKS/',
     +        'COREY EPS.'/, 
     +        '    No.     UPPER     LOWER     UPPER     ', 
     +        'LOWER     UPPER     LOWER     UPPER     LOWER     ',
     +        'UPPER     LOWER   CHANNEL      BANK     UPPER     ',
     +        'LOWER     UPPER     LOWER     UPPER     LOWER'/)
 9008 FORMAT (1X, //9X, 'STREAMBED PROPERTIES AND STREAM DIMENSIONS', //
     +        ' SEGMENT     BED HYD. COND.', 6X, 
     +        'BED THICKNESS     ELEV.-TOP OF BED     WIDTH OF ', 
     +        'STREAM     DEPTH OF STREAM    STREAM ROUGHNESS  ', 
     +        '  SAT. WATER CONT.    INT. WATER CONT.    BROOKS/',
     +        'COREY EPS.     UNSAT. HYD. COND.', /, 
     +        '    No.     UPPER     LOWER     UPPER     ', 
     +        'LOWER     UPPER     LOWER     UPPER     LOWER     ',
     +        'UPPER     LOWER   CHANNEL      BANK     UPPER     ',
     +        'LOWER     UPPER     LOWER     UPPER     LOWER     ',
     +        'UPPER'/)
      DO nseg = Lstbeg, lstend
        IF ( Lstbeg.EQ.1 ) THEN
          nn = nseg
        ELSE
          nn = ISEG(3, nseg)
        END IF
        icalc = ISEG(1, nseg)
        IF ( icalc.EQ.0 ) THEN
          IF ( ISFROPT.EQ.0 .OR. ISFROPT.GT.3 ) THEN
            WRITE (IOUT, 9010) nn, SEG(6, nseg), SEG(11, nseg), 
     +                         SEG(7, nseg), SEG(12, nseg), SEG(8, nseg)
     +                         , SEG(13, nseg), SEG(9, nseg), 
     +                         SEG(14, nseg), SEG(10, nseg), 
     +                         SEG(15, nseg)
 9010       FORMAT (I6, 1X, 1P10E10.3)
          ELSE
            WRITE (IOUT, 9011) nn, SEG(9, nseg), SEG(14, nseg), 
     +                         SEG(10, nseg), SEG(15, nseg)
 9011       FORMAT (I6, 1X, 1P4E10.3)
          END IF
        ELSE IF ( icalc.EQ.1 ) THEN
          IF ( ISFROPT.EQ.0 ) THEN
            WRITE (IOUT, 9012) nn, SEG(6, nseg), SEG(11, nseg), 
     +                         SEG(7, nseg), SEG(12, nseg), SEG(8, nseg)
     +                         , SEG(13, nseg), SEG(9, nseg), 
     +                         SEG(14, nseg), SEG(16, nseg)
 9012       FORMAT (I6, 1X, 1P8E10.3, 20X, 1PE10.3)
 ! RGN changed next line to "ISFROPT.GE.1" instead of "ISFROPT.GT.1"
          ELSE IF ( ISFROPT.GE.1 .AND. ISFROPT.LT.4 ) THEN
            WRITE (IOUT, 9013) nn, SEG(9, nseg), SEG(14, nseg), 
     +                         SEG(16, nseg)
 9013       FORMAT (I6, 3X, 1P2E10.3, 21X, 1PE10.3)
          ELSE IF ( ISFROPT.EQ.4 ) THEN
            IF ( Kkper.EQ.1 ) THEN
              WRITE (IOUT, 9014) nn, SEG(6, nseg), SEG(11, nseg), 
     +                           SEG(7, nseg), SEG(12, nseg), 
     +                           SEG(8, nseg), SEG(13, nseg), 
     +                           SEG(9, nseg), SEG(14, nseg), 
     +                           SEG(16, nseg), SEG(18, nseg), 
     +                           SEG(22, nseg), SEG(19, nseg), 
     +                           SEG(23, nseg), SEG(20, nseg), 
     +                           SEG(24, nseg)
 9014         FORMAT (I6, 1X, 1P8E10.3, 20X, 1PE10.3, 10X, 1P6E10.3)
            ELSE
              WRITE (IOUT, 9015) nn, SEG(6, nseg), SEG(11, nseg)
 9015         FORMAT (I6, 1X, 1P2E10.3)
            END IF
          ELSE IF ( ISFROPT.EQ.5 ) THEN
            IF ( Kkper.EQ.1 ) THEN
              WRITE (IOUT, 9016) nn, SEG(6, nseg), SEG(11, nseg), 
     +                           SEG(7, nseg), SEG(12, nseg), 
     +                           SEG(8, nseg), SEG(13, nseg), 
     +                           SEG(9, nseg), SEG(14, nseg), 
     +                           SEG(16, nseg), SEG(18, nseg), 
     +                           SEG(22, nseg), SEG(19, nseg), 
     +                           SEG(23, nseg), SEG(20, nseg), 
     +                           SEG(24, nseg), SEG(21, nseg), 
     +                           SEG(25, nseg)
 9016         FORMAT (I6, 1X, 1P8E10.3, 20X, 1PE10.3, 10X, 1P8E10.3)
            ELSE
              WRITE (IOUT, 9017) nn, SEG(6, nseg), SEG(11, nseg), 
     +                           SEG(16, nseg)
 9017         FORMAT (I6, 1X, 1P2E10.3, 60X, 1PE10.3)
            END IF
          END IF
        ELSE IF ( icalc.EQ.2 ) THEN
          IF ( ISFROPT.EQ.0 ) THEN
            WRITE (IOUT, 9018) nn, SEG(6, nseg), SEG(11, nseg), 
     +                         SEG(7, nseg), SEG(12, nseg), SEG(8, nseg)
     +                         , SEG(13, nseg), SEG(16, nseg), 
     +                         SEG(17, nseg)
 9018       FORMAT (I6, 1X, 1P6E10.3, 40X, 1P2E10.3)
 !  RGN this condition was missing 1/15/12
          ELSE IF ( ISFROPT.GE.1 .AND. ISFROPT.LT.4 ) THEN
            WRITE (IOUT, 9019) nn, SEG(16, nseg),SEG(17, nseg)
 9019       FORMAT (I6, 40X, 1P2E10.3)
          ELSE IF ( ISFROPT.EQ.4 ) THEN
            IF ( Kkper.EQ.1 ) THEN
              WRITE (IOUT, 9020) nn, SEG(6, nseg), SEG(11, nseg), 
     +                           SEG(7, nseg), SEG(12, nseg), 
     +                           SEG(8, nseg), SEG(13, nseg), 
     +                           SEG(16, nseg), SEG(17, nseg), 
     +                           SEG(18, nseg), SEG(22, nseg), 
     +                           SEG(19, nseg), SEG(23, nseg), 
     +                           SEG(20, nseg), SEG(24, nseg)
 9020         FORMAT (I6, 1X, 1P6E10.3, 40X, 1P8E10.3)
            ELSE
              WRITE (IOUT, 9021) nn, SEG(6, nseg), SEG(11, nseg), 
     +                           SEG(16, nseg), SEG(17, nseg)
 9021         FORMAT (I6, 1X, 1P2E10.3, 80X, 1P2E10.3)
            END IF
          ELSE IF ( ISFROPT.EQ.5 ) THEN
            IF ( Kkper.EQ.1 ) THEN
              WRITE (IOUT, 9022) nn, SEG(6, nseg), SEG(11, nseg), 
     +                           SEG(7, nseg), SEG(12, nseg), 
     +                           SEG(8, nseg), SEG(13, nseg), 
     +                           SEG(16, nseg), SEG(17, nseg), 
     +                           SEG(18, nseg), SEG(22, nseg), 
     +                           SEG(19, nseg), SEG(23, nseg), 
     +                           SEG(20, nseg), SEG(24, nseg), 
     +                           SEG(21, nseg), SEG(25, nseg)
 9022         FORMAT (I6, 1X, 1P6E10.3, 40X, 1P10E10.3)
            ELSE
              WRITE (IOUT, 9023) nn, SEG(6, nseg), SEG(11, nseg), 
     +                           SEG(16, nseg), SEG(17, nseg)
 9023         FORMAT (I6, 1X, 1P2E10.3, 80X, 1P2E10.3)
            END IF
          END IF
        ELSE IF ( icalc.GE.3 ) THEN
          IF ( ISFROPT.EQ.0 ) THEN
            WRITE (IOUT, 9024) nn, SEG(6, nseg), SEG(11, nseg), 
     +                         SEG(7, nseg), SEG(12, nseg), SEG(8, nseg)
     +                         , SEG(13, nseg)
 9024       FORMAT (I6, 1X, 1P6E10.3)
          END IF
        END IF
      END DO
C
C4------PRINT CROSS-SECTIONAL DATA FOR SEGMENTS WITH ICALC=2.
      iflg = 0
      DO nseg = Lstbeg, lstend
        IF ( Lstbeg.EQ.1 ) THEN
          nn = nseg
        ELSE
          nn = ISEG(3, nseg)
        END IF
        icalc = ISEG(1, nseg)
        IF ( icalc.EQ.2 .AND. iflg.EQ.0 ) THEN
          WRITE (IOUT, 9025)
 9025     FORMAT (1X, /1X, ' EIGHT POINT CROSS-SECTION DATA ', 
     +            'FOR SEGMENTS WITH ICALC = 2', /3X, ' X VALUES', 
     +            ' X VALUES START FROM LEFT SIDE LOOKING ', 
     +            'DOWNSTREAM', //5X, 'SEGMENT NO.', 
     +            '        X1        X2        X3        X4', 
     +            '        X5        X6        X7        X8')
          iflg = 1
        END IF
        IF ( icalc.EQ.2 .AND. iflg.EQ.1 ) THEN
          WRITE (IOUT, 9026) nn, (XSEC(i, nseg), i=1, 8)
 9026     FORMAT (7X, I6, 5X, 8(1PE10.3))
        END IF
      END DO
      iflg = 0
      DO nseg = Lstbeg, lstend
        IF ( Lstbeg.EQ.1 ) THEN
          nn = nseg
        ELSE
          nn = ISEG(3, nseg)
        END IF
        icalc = ISEG(1, nseg)
        IF ( icalc.EQ.2 .AND. iflg.EQ.0 ) THEN
          WRITE (IOUT, 9027)
 9027     FORMAT (1X, /3X, ' Z VALUES ARE RELATIVE TO STREAM', 
     +            'BED ELEVATION', //5X, 'SEGMENT NO.          ', 
     +            'Z1        Z2        Z3        Z4        Z5', 
     +            '        Z6        Z7        Z8')
          iflg = 1
        END IF
        IF ( icalc.EQ.2 .AND. iflg.EQ.1 ) THEN
          WRITE (IOUT, 9028) nn, (XSEC(i, nseg), i=9, 16)
 9028     FORMAT (7X, I6, 5X, 8(1PE10.3))
        END IF
      END DO
C
C5------PRINT STREAMFLOW, DEPTH AND WIDTH RELATIONS FOR SEGMENTS
C         WITH ICALC=3.
      iflg = 0
      DO nseg = Lstbeg, lstend
        IF ( Lstbeg.EQ.1 ) THEN
          nn = nseg
        ELSE
          nn = ISEG(3, nseg)
        END IF
        icalc = ISEG(1, nseg)
        IF ( icalc.EQ.3 .AND. iflg.EQ.0 ) THEN
          WRITE (IOUT, 9029)
 9029     FORMAT (/1X, 'STREAMFLOW RELATION WITH DEPTH IS ', 
     +            'BASED ON EQUATION Q = CDPTH*(DEPTH)**FDPTH', /1X, 
     +            'STREAMFLOW RELATION WITH WIDTH IS ', 
     +            'BASED ON EQUATION Q = AWDTH*(WIDTH)**BWDTH', //1X, 
     +            'SEGMENT NO.      CDPTH     FDPTH    ', 
     +            'AWDTH     BWDTH'/)
          iflg = 1
        END IF
        IF ( icalc.EQ.3 .AND. iflg.EQ.1 ) THEN
          WRITE (IOUT, 9030) nn, SEG(9, nseg), SEG(10, nseg), 
     +                       SEG(14, nseg), SEG(15, nseg)
 9030     FORMAT (5X, I6, 1P4E10.3)
        END IF
      END DO
C
C6------PRINT TABULATED VALUES FOR COMPUTING STREAM WIDTH AND DEPTH
C         FROM STREAMFLOW FOR SEGMENTS WITH ICALC=4.
      iflg = 0
      DO nseg = Lstbeg, lstend
        IF ( Lstbeg.EQ.1 ) THEN
          nn = nseg
        ELSE
          nn = ISEG(3, nseg)
        END IF
        icalc = ISEG(1, nseg)
        nstrpts = ISEG(2, nseg)
        IF ( icalc.EQ.4 .AND. iflg.EQ.0 ) THEN
          WRITE (IOUT, 9031)
 9031     FORMAT (1X, /1X, 'STREAMFLOW RELATION WITH DEPTH ', 
     +            'AND WIDTH IS BASED ON TABULATED VALUES', //2X, 
     +            'SEGMENT NO.   STREAMFLOW       DEPTH       ', 
     +            'WIDTH', /)
          iflg = 1
        END IF
        ipt = 1
        IF ( icalc.EQ.4 .AND. iflg.EQ.1 ) THEN
          DO WHILE ( ipt.LE.nstrpts )
            WRITE (IOUT, 9032) nn, QSTAGE(ipt, nseg), 
     +                         QSTAGE(nstrpts+ipt, nseg), 
     +                         QSTAGE(2*nstrpts+ipt, nseg)
 9032       FORMAT (5X, I6, 2X, 3(1PE13.4)) !gsf
            ipt = ipt + 1
          END DO
        END IF
      END DO
C
C7------PRINT SOLUTE DATA FOR EACH STREAM SEGMENT.
      IF ( Iunitgwt.GT.0 ) THEN
        DO isol = 1, Nsol
          WRITE (Iouts, 9033) isol
 9033     FORMAT (1X, //10X, ' DATA FOR EACH STREAM SEGMENT:', 
     +            ' SOLUTE No. ', I2//5X, 'SEGMENT          ', 
     +            'SOLUTE CONCENTRATION IN:    ', /5X, 
     +            'NUMBER       SEGMENT INFLOW   OVERLAND FLOW', 3X, 
     +            'PRECIPITATION')
          DO nseg = Lstbeg, lstend
            IF ( Lstbeg.EQ.1 ) THEN
              nn = nseg
            ELSE
              nn = ISEG(3, nseg)
            END IF
Cgzh   Change to output
            IF ( IDIVAR(1, nseg).EQ.0 ) THEN
              WRITE (Iouts, 9034) nn, CONCQ(nseg, isol), 
     +                            CONCRUN(nseg, isol), 
     +                            CONCPPT(nseg, isol)
            ELSE
              WRITE (Iouts, 9035) nn, CONCRUN(nseg, isol), 
     +                            CONCPPT(nseg, isol)
            END IF
 9034       FORMAT (1X, /4X, I6, 9X, 1PE10.3, 6X, E10.3, 6X, E10.3)
 9035       FORMAT (1X, /4X, I6, 9X, '   N/A    ', 6X, E10.3, 6X, E10.3)
          END DO
        END DO
        WRITE (Iouts, 9036)
 9036   FORMAT (//)
      END IF
C
      RETURN
      END SUBROUTINE SGWF2SFR7PRSEG
C
C-------FUNCTION CALCUNSATFLOBOT written by RGN, MAY 24, 2004
      REAL FUNCTION CALCUNSATFLOBOT(Depth, Avhc, Fks, Wetperm, Sbdthk, 
     +                              Areamax, Strlen, Fbcheck, Nwavst, 
     +                              Maxwav, Foldflbt)
      USE GWFSFRMODULE, ONLY: NSTRAIL, ISUZN, NEARZERO
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL Areamax, Avhc, Fks, Strlen
      INTEGER  Nwavst(ISUZN), Maxwav
      DOUBLE PRECISION Wetperm, Depth, Sbdthk, Fbcheck, Foldflbt
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      REAL flobotcheck, seep
      INTEGER nstrailpls1, ii
      DOUBLE PRECISION flobot, area
C     ------------------------------------------------------------------
      nstrailpls1 = 2*NSTRAIL + 1
      seep = Avhc*(1.0D0+(Depth)/(Sbdthk))
      area = Wetperm*Strlen
      flobot = seep*area
      IF ( flobot.GE.Fks*Areamax ) flobot = Fks*Areamax
      IF ( ABS(flobot).GT.1.0D-30 ) THEN
        flobotcheck = ABS(flobot/(Wetperm*Strlen)-Foldflbt
     +                /(Wetperm*Strlen))
        IF ( flobotcheck.LE.Fbcheck ) flobot = Foldflbt
        DO ii = 1, ISUZN
          IF ( Nwavst(ii).GT.Maxwav-nstrailpls1 ) flobot = 0.0D0
        END DO
      END IF
      IF ( flobot-Fbcheck.LT.NEARZERO ) flobot = 0.0D0
      CALCUNSATFLOBOT = flobot
      END FUNCTION CALCUNSATFLOBOT
C
C-------SUBROUTINE CALC_UNSAT_INFIL written by RGN, MAY 24, 2004
      SUBROUTINE CALC_UNSAT_INFIL(Flobot, Uzseep, Uzthst, Thr, Ha, 
     +                            Thetas, Epsilon, Fks, Avhc, Depth, 
     +                            Sbdthk, Wetper, Uzwdth, Flow, Nwavst, 
     +                            Strlen, Iwidthcheck, Icalc)
C     ******************************************************************
C     DEFINE UNSATURATED CELLS TO ACCOMMODATE STREAM LOSS.
!--------REVISED FOR MODFLOW-2005 RELEASE 1.9, FEBRUARY 6, 2012
C     ******************************************************************
      USE GWFSFRMODULE, ONLY: ISUZN, NSTOTRL
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL Avhc, Fks, Ha, Strlen
      INTEGER Iwidthcheck, Nwavst(ISUZN), Icalc
      DOUBLE PRECISION Flobot, Uzseep(ISUZN), Uzthst(NSTOTRL), Depth, 
     +                 Uzwdth(ISUZN), Flow, Sbdthk, Wetper(ISUZN), Thr, 
     +                 Thetas, Epsilon
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      REAL ftaken, porpress
      INTEGER i, imoistcheck, iset, k, ntotuzn
      DOUBLE PRECISION flobotleft, seepunsat, disconwidth, uzflobot
C     ------------------------------------------------------------------
      ntotuzn = NSTOTRL/ISUZN
      flobotleft = Flobot
      IF ( Flobot.LE.0.0D0 .OR. Flow.LE.0.0D0 ) THEN
        IF ( Uzthst(1).GT.Thr .OR. Nwavst(1).GT.1 ) Uzwdth(1)
     +       = Wetper(1)
        DO i = 1, ISUZN
          Uzseep(i) = 0.0D0
        END DO
        RETURN
      END IF
      IF ( Icalc.EQ.1 ) THEN
        Uzwdth(1) = Wetper(1)
        Iwidthcheck = 1
        IF ( Fks.LE.Avhc ) THEN
          Uzseep(1) = Fks
        ELSE
          Uzseep(1) = Avhc*(1.0D0+(Depth)/(Sbdthk))
        END IF
        uzflobot = Uzseep(1)*Uzwdth(1)*Strlen
        IF ( uzflobot.GT.Flow ) Uzseep(1) = Flow/(Uzwdth(1)*Strlen)
      ELSE IF ( Icalc.EQ.2 ) THEN
        IF ( Uzthst(Nwavst(1)).LE.Thr ) THEN
          porpress = Ha*(((Thr+.01D0)-Thr)/(Thetas-Thr))
     +               **(-(1.0D0/Epsilon))
        ELSE
          porpress = Ha*((Uzthst(Nwavst(1))-Thr)/(Thetas-Thr))
     +               **(-(1.0D0/Epsilon))
        END IF
        seepunsat = Avhc*(1.0D0+(Depth-porpress)/(Sbdthk))
        IF ( seepunsat.GT.Fks ) seepunsat = Fks
        imoistcheck = 0
        disconwidth = 0.0D0
        IF ( Uzthst(1).GT.Thr .OR. Nwavst(1).GT.1 ) imoistcheck = 1
        IF ( Flow.GT.0.0 .OR. imoistcheck.EQ.1 ) THEN
          Uzwdth(1) = Wetper(1)
          Iwidthcheck = 1
          disconwidth = Uzwdth(1)
          ftaken = seepunsat*disconwidth*Strlen
          IF ( Flobot.GT.ftaken ) THEN
            Uzseep(1) = seepunsat
            flobotleft = flobotleft - ftaken
          ELSE
            Uzseep(1) = Flobot/(disconwidth*Strlen)
            flobotleft = 0.0D0
          END IF
        END IF
        iset = 1 + ntotuzn
        DO k = 2, ISUZN
          imoistcheck = 0
          IF ( Uzthst(iset).GT.Thr .OR. Nwavst(k).GT.1 ) imoistcheck = 1
          IF ( flobotleft.GT.0.0 .OR. imoistcheck.EQ.1 ) THEN
            Uzwdth(k) = Wetper(k)
            Iwidthcheck = k
            IF ( flobotleft.GT.0.0 ) THEN
              disconwidth = disconwidth + Uzwdth(k)
              ftaken = seepunsat*Uzwdth(k)*Strlen
              IF ( flobotleft.GT.ftaken ) THEN
                Uzseep(k) = seepunsat
                flobotleft = flobotleft - ftaken
              ELSE
                Uzseep(k) = flobotleft/(Uzwdth(k)*Strlen)
                flobotleft = 0.0D0
              END IF
            END IF
          END IF
          iset = iset + ntotuzn
        END DO
      END IF
C-------RETURN.
      RETURN
      END SUBROUTINE CALC_UNSAT_INFIL
C
C-------SUBROUTINE UZMASSBAL written MAY 24, 2004
      SUBROUTINE UZMASSBAL(L, H, Hld, Thr, Thetas, Epsilon, Fks, Uzdpst,
     +                     Uzthst, Uzspst, Uzflst, Ltrlst, Itrlst, 
     +                     Uzflwt, Uzstor, Delstor, Nwavst, Uzolsflx, 
     +                     Uzwdth, Wetper, Uzseep, Ratin, Ratout, 
     +                     Il, Ir, Ic, Flobot, Sbot, Strlen, Totflwt, 
     +                     Totuzstor, Totdelstor, Iwidthcheck, Avdpt, 
     +                     Avwat, Wat1, Ibd, Icalc, Deltinc, Imassroute,
     +                     Iunitgage, Gwflow)
C     ******************************************************************
C     COMPUTE INFLOW, OUTFLOW, AND CHANGE IN STORAGE IN UNSATURATED
C     ZONE BENEATH STREAMBED.
!--------REVISED FOR MODFLOW-2005 RELEASE 1.9, FEBRUARY 6, 2012
C     ******************************************************************
      USE GWFSFRMODULE, ONLY: ISUZN,NSTOTRL,NUMAVE,STRM,ITRLSTH,SFRUZBD,
     +                        SUMLEAK,SUMRCH, NEARZERO, CLOSEZERO
      USE GLOBAL,       ONLY: BUFF
!!      USE GLOBAL,       ONLY: BUFF,IOUT
!      USE GWFBASMODULE, ONLY: DELT
      IMPLICIT NONE
      INTRINSIC ABS, DABS
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER L, Il, Ir, Ic, Iwidthcheck, Ibd, Icalc, Imassroute
      INTEGER Nwavst(ISUZN), Itrlst(NSTOTRL), Ltrlst(NSTOTRL)
      INTEGER Iunitgage
      REAL Fks, Strlen, Deltinc, Avdpt(NUMAVE), Avwat(NUMAVE),
     +     Wat1(NUMAVE)
      DOUBLE PRECISION Uzwdth(ISUZN), Uzflwt(ISUZN), Uzolsflx(ISUZN),
     +                 Uzseep(ISUZN), Uzstor(ISUZN), Delstor(ISUZN),
     +                 Wetper(ISUZN)
      DOUBLE PRECISION Uzspst(NSTOTRL), Uzflst(NSTOTRL),
     +                 Uzdpst(NSTOTRL), Uzthst(NSTOTRL)
      DOUBLE PRECISION H, Hld, Thr, Thetas, Epsilon, Ratin, Ratout,
     +                 Flobot, Sbot, Totflwt, Totuzstor, Totdelstor,
     +                 Gwflow
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      REAL depthinc, depthsave, fhold, hdif, htest1, htest2, seep, 
     +     totalwc, totalwc1, uzstorhold, widthsave, ftheta1, ftheta2, 
     +     eps_m1
      INTEGER i, ick, icounter, iflag, ii, iset, j, jj, jk, k, kk, 
     +        numwavhld, nwavecheck, iuznhold, ntotuzn, jm1
      DOUBLE PRECISION strtop, fm, fluxdif
      INTEGER loop
      ALLOCATABLE loop(:)
      ALLOCATE (loop(ISUZN))
C
C1------INITIALIZE VARIABLES.
C
      iflag = 0
      Totflwt = 0.0D0
      Totdelstor = 0.0D0
      Totuzstor = 0.0D0
      strtop = STRM(3, L)
      htest1 = H - Sbot
      htest2 = Hld - Sbot
      hdif = ABS(H-Hld)
      eps_m1 = Epsilon - 1.0D0
      fluxdif = 0.0D0
      iflag = 0
      iset = 1
      ntotuzn = NSTOTRL/ISUZN
      iuznhold = 0
      IF ( Icalc.EQ.2 ) THEN
        iuznhold = ISUZN
      ELSE IF ( Icalc.EQ.1 ) THEN
        iuznhold = 1
      END IF
      nwavecheck = 0
      DO i = 1, iuznhold
        fluxdif = fluxdif + DABS(Uzseep(i)-Uzolsflx(i))
        nwavecheck = nwavecheck + Nwavst(i)
      END DO
      IF ( fluxdif.LT.5.0E-10 ) iflag = 1
C
C1A---- NO UNSATURATED ZONE
      IF ( htest1.GE.-CLOSEZERO .AND. htest2.GE.-CLOSEZERO ) THEN
        iset = 1
        DO i = 1, iuznhold
          Delstor(i) = 0.0D0
          Uzstor(i) = 0.0D0
          Uzdpst(iset) = 0.0D0
          Uzthst(iset) = Thr
          iset = iset + ntotuzn
        END DO
        IF ( Imassroute.EQ.1 ) THEN
          IF ( Flobot.LT.0.0D0 ) THEN
            Ratout = Ratout - SUMLEAK(L)
            SFRUZBD(9) = SFRUZBD(9) - SUMLEAK(L)
          ELSE
            Ratin = Ratin + SUMLEAK(L)
            SFRUZBD(8) = SFRUZBD(8) + SUMLEAK(L)
          END IF
           Gwflow = SUMLEAK(L)
           BUFF(Ic, Ir, Il) = BUFF(Ic, Ir, Il) + Gwflow
        END IF
C
C2------REMOVE ALL UNSATURATED ZONE WAVES AND CALCULATE CHANGE IN 
C         STORAGE WHEN WATER TABLE RISES TO ELEVATION OF STREAMBED.
      ELSE IF ( htest1.GE.-CLOSEZERO .AND. htest2.LT.-CLOSEZERO ) THEN
        DO kk = 1, iuznhold
          loop(kk) = 0
        END DO
        iset = 1
        DO i = 1, iuznhold
          ick = 0
          IF ( Uzthst(iset).GT.Thr .OR. Nwavst(i).GT.1 ) ick = 1
          IF ( i.LE.Iwidthcheck .OR. ick.EQ.1 ) loop(i) = 1
          iset = iset + ntotuzn
        END DO
        iset = 1
        DO i = 1, iuznhold
          IF ( loop(i).GT.0 ) THEN
            ick = 0
            Delstor(i) = Uzstor(i)
            Uzstor(i) = 0.0D0
          END IF
          iset = iset + ntotuzn
        END DO
        CALL ROUTWAVESST(L, seep, H, Hld, Thr, Thetas, Fks, Epsilon, 
     +                   Iwidthcheck, strtop, Icalc, Nwavst, Uzwdth, 
     +                   Uzflwt, Uzolsflx, Uzseep, Itrlst, Ltrlst, 
     +                   Uzspst, Uzflst, Uzdpst, Uzthst, Deltinc)
        iset = 1
        DO kk = 1, iuznhold
          loop(kk) = 0
        END DO
        DO i = 1, iuznhold
          ick = 0
          IF ( Uzthst(iset).GT.Thr .OR. Nwavst(i).GT.1 ) ick = 1
          IF ( i.LE.Iwidthcheck .OR. ick.EQ.1 ) loop(i) = 1
          iset = iset + ntotuzn
        END DO
        iset = 1
        DO i = 1, iuznhold
          IF ( loop(i).GT.0 ) THEN
            Uzdpst(iset) = 0.0D0
            Uzthst(iset) = Thr
            Uzflst(iset) = 0.0D0
            Uzspst(iset) = 0.0D0
            Itrlst(iset) = 0
            Ltrlst(iset) = 0
            Nwavst(i) = 1
            DO ii = iset + 1, (iset+ntotuzn) - 1
              Uzdpst(ii) = 0.0D0
              Uzthst(ii) = Thr
              Uzflst(ii) = 0.0D0
              Uzspst(ii) = 0.0D0
              Itrlst(ii) = 0
              Ltrlst(ii) = 0
            END DO
          END IF
          iset = iset + ntotuzn
        END DO
        DO i = 1, iuznhold
          IF ( loop(i).GT.0 ) THEN
            Uzolsflx(i) = 0.0D0
            Totflwt = Totflwt + Uzflwt(i)
            Totdelstor = Totdelstor + Delstor(i)
          END IF
        END DO
        SUMRCH(L) = SUMRCH(L) + Totflwt
        STRM(29,L) = STRM(29,L) + Totdelstor
        IF ( Imassroute.EQ.1 ) THEN
          IF ( Flobot.LT.0.0D0 ) THEN
            Ratout = Ratout - SUMLEAK(L)
            Ratin = Ratin + SUMRCH(L)/deltinc
            SFRUZBD(2) = SFRUZBD(2) - SUMRCH(L)
            SFRUZBD(3) = SFRUZBD(3) + SUMRCH(L)
            SFRUZBD(5) = SFRUZBD(5) - SUMRCH(L)/deltinc
            SFRUZBD(6) = SFRUZBD(6) + SUMRCH(L)/deltinc
            SFRUZBD(9) = SFRUZBD(9) - SUMLEAK(L)           
          ELSE
            Ratin = Ratin + SUMLEAK(L) + SUMRCH(L)/deltinc
            SFRUZBD(2) = SFRUZBD(2) - SUMRCH(L)
            SFRUZBD(3) = SFRUZBD(3) + SUMRCH(L)
            SFRUZBD(5) = SFRUZBD(5) - SUMRCH(L)/deltinc
            SFRUZBD(6) = SFRUZBD(6) + SUMRCH(L)/deltinc
            SFRUZBD(8) = SFRUZBD(8) + SUMLEAK(L)
          END IF
          Gwflow = SUMLEAK(L) + SUMRCH(L)/deltinc
          BUFF(Ic, Ir, Il) = BUFF(Ic, Ir, Il) + Gwflow
        END IF
C
C3------CALCULATE CHANGE IN STORAGE AND UPDATE UNSATURATED ZONE WAVES
C         WHEN WATER TABLE REMAINS BELOW STREAMBED ELEVATION.
      ELSE IF ( hdif.LT.2.0E-4 .AND. nwavecheck.EQ.iuznhold .AND. 
     +          iflag.EQ.1 .AND. htest1.LT.2.0E-3 ) THEN
        iset = 1
        DO i = 1, iuznhold
          Delstor(i) = 0.0D0
          Uzdpst(iset) = Uzdpst(iset) - (H-Hld)
          Uzstor(i) = Uzdpst(iset)*(Uzthst(iset)-Thr)*Uzwdth(i)*Strlen
          SFRUZBD(10) = SFRUZBD(10) + Uzstor(i)
          Uzflwt(i) = Uzseep(i)*Uzwdth(i)*Strlen*Deltinc
          Uzolsflx(i) = Uzseep(i)
          SUMRCH(L) = SUMRCH(L) + Uzflwt(i)
          iset = iset + ntotuzn
        END DO 
        STRM(29,L) = 0.0
        IF ( Imassroute.EQ.1 ) THEN
          Ratin = Ratin + SUMRCH(L)/deltinc
          Gwflow = SUMRCH(L)/deltinc
          BUFF(Ic, Ir, Il) = BUFF(Ic, Ir, Il) + Gwflow
          SFRUZBD(1) = SFRUZBD(1) + SUMLEAK(L)*deltinc
          SFRUZBD(2) = SFRUZBD(2) + STRM(29,L)
          SFRUZBD(3) = SFRUZBD(3) + SUMRCH(L)
          SFRUZBD(4) = SFRUZBD(4) + SUMLEAK(L)
          SFRUZBD(5) = SFRUZBD(5) + STRM(29,L)/deltinc
          SFRUZBD(6) = SFRUZBD(6) + SUMRCH(L)/deltinc
          SFRUZBD(8) = SFRUZBD(8) + SUMLEAK(L)
          SFRUZBD(7) = SFRUZBD(7) + SUMLEAK(L)
        END IF
C
      ELSE IF ( htest1.LT.-CLOSEZERO .AND. htest2.LT.-CLOSEZERO ) THEN
        CALL ROUTWAVESST(L, seep, H, Hld, Thr, Thetas, Fks, Epsilon, 
     +                   Iwidthcheck, strtop, Icalc, Nwavst, Uzwdth, 
     +                   Uzflwt, Uzolsflx, Uzseep, Itrlst, Ltrlst, 
     +                   Uzspst, Uzflst, Uzdpst, Uzthst, Deltinc)
        DO kk = 1, iuznhold
          loop(kk) = 0
        END DO
        iset = 1
        DO i = 1, iuznhold
          ick = 0
          IF ( Uzthst(iset).GT.Thr .OR. Nwavst(i).GT.1 ) ick = 1
          IF ( i.LE.Iwidthcheck .OR. ick.EQ.1 ) loop(i) = 1
          iset = iset + ntotuzn
        END DO
        iset = 1
        DO i = 1, iuznhold
          IF ( loop(i).GT.0 ) THEN
C
C4------CALCULATE CHANGE IN UNSATURATED ZONE STORAGE WHEN WATER TABLE 
C         RISES.
            IF ( H.GT.Hld ) THEN
              fm = 0.0D0
              depthsave = Uzdpst(iset)
              jj = iset
              DO jk = iset+1, iset+Nwavst(i)-1
                IF ( ((Sbot-Uzdpst(jk)).LE.H) ) jj = jk
              END DO
              jk = iset + 1
C
C5------WATER TABLE RISES THROUGH WAVES.
              IF ( jj.GE.jk ) THEN
                DO j = iset, iset + Nwavst(i) - 1
                  ITRLSTH(j) = Itrlst(j)
                END DO
                numwavhld = Nwavst(i)
                Nwavst(i) = Nwavst(i) - (jj-iset)
                Uzdpst(iset) = depthsave - (H-Hld)
                Uzthst(iset) = Uzthst(jj)
                Uzflst(iset) = Uzflst(jj)
                Uzspst(iset) = 0.0D0
                Itrlst(iset) = 0
                Ltrlst(iset) = 0
                k = iset + 1
                DO j = jj + 1, iset + numwavhld - 1
                  Uzdpst(k) = Uzdpst(j)
                  Uzthst(k) = Uzthst(j)
                  Uzflst(k) = Uzflst(j)
                  Uzspst(k) = Uzspst(j)
                  Itrlst(k) = Itrlst(j)
                  Ltrlst(k) = Ltrlst(j)
                  k = k + 1
                END DO
C
C6------LOOP THROUGH NUMBER OF TRAIL WAVES INTERSECTED BY WATER TABLE.
                DO j = iset, jj + 1
                  IF ( j.EQ.jj+1 ) THEN
                    IF ( ITRLSTH(j).GT.0 ) THEN
C
C7------LEAD TRAIL WAVE BELOW WATER TABLE AND FIRST TRAIL WAVE IS 
C         ABOVE WATER TABLE. 
                      IF ( ITRLSTH(j).EQ.1 ) THEN
                        jm1 = j - 1
                        Ltrlst(jm1) = 1
                        Itrlst(jm1) = 0
                        fhold = (Uzthst(jm1)-Thr)/(Thetas-Thr)
                        IF ( fhold.LT.CLOSEZERO ) fhold = 0.0
                        IF ( DABS(Uzthst(jm1)-Uzthst(j-2)).LT.NEARZERO )
     +                       THEN
                          fhold = ((Uzthst(jm1)-Thr)/(Thetas-Thr))
     +                            **Epsilon
                          IF ( fhold.LT.CLOSEZERO ) fhold = 0.0
                          Uzspst(jm1) = (Epsilon*Fks/(Thetas-Thr))*fhold
     +                                  **eps_m1
                        ELSE
                          fhold = ((Uzthst(j-2)-Thr)/(Thetas-Thr))
     +                            **Epsilon
                          IF ( fhold.LT.CLOSEZERO ) fhold = 0.0
                          ftheta1 = Fks*fhold
                          fhold = ((Uzthst(jm1)-Thr)/(Thetas-Thr))
     +                            **Epsilon
                          IF ( fhold.LT.CLOSEZERO ) fhold = 0.0
                          ftheta2 = Fks*fhold
                          Uzspst(jm1) = (ftheta1-ftheta2)
     +                                  /(Uzthst(j-2)-Uzthst(jm1))
                        END IF
                      ELSE
C
C8------LEAD TRAIL WAVE BELOW WATER TABLE AND MULTIPLE TRAIL WAVES 
C         ABOVE WATER TABLE.
                        DO k = iset + 1, iset + ITRLSTH(j)
                          Ltrlst(k) = 1
                          Itrlst(k) = 0
                          fhold = (Uzthst(k)-Thr)/(Thetas-Thr)
                          IF ( fhold.LT.CLOSEZERO ) fhold = 0.0
                          IF ( DABS(Uzthst(k)-Uzthst(k-1)).LT.NEARZERO )
     +                         THEN
                            fhold = ((Uzthst(k)-Thr)/(Thetas-Thr))
     +                              **Epsilon
                            IF ( fhold.LT.CLOSEZERO ) fhold = 0.0
                            Uzspst(k) = (Epsilon*Fks/(Thetas-Thr))*fhold
     +                                  **eps_m1
                          ELSE
                            fhold = ((Uzthst(k-1)-Thr)/(Thetas-Thr))
     +                              **Epsilon
                            IF ( fhold.LT.CLOSEZERO ) fhold = 0.0
                            ftheta1 = Fks*fhold
                            fhold = ((Uzthst(k)-Thr)/(Thetas-Thr))
     +                              **Epsilon
                            IF ( fhold.LT.CLOSEZERO ) fhold = 0.0
                            ftheta2 = Fks*fhold
                            Uzspst(k) = (ftheta1-ftheta2)
     +                                  /(Uzthst(k-1)-Uzthst(k))
                          END IF
                        END DO
                      END IF
                    END IF
                  ELSE IF ( j.NE.jj ) THEN
C
C9------MULTIPLE TRAIL WAVES BELOW AND ABOVE WATER TABLE.
                    IF ( ITRLSTH(j).GT.jj-j+1 ) THEN
                      DO k = iset + 1, iset + ITRLSTH(j) - (jj-j) - 1
                        Ltrlst(k) = 1
                        Itrlst(k) = 0
                        fhold = (Uzthst(k)-Thr)/(Thetas-Thr)
                        IF ( fhold.LT.CLOSEZERO ) fhold = 0.0
                        IF ( DABS(Uzthst(k)-Uzthst(k-1)).LT.NEARZERO )
     +                       THEN
                          fhold = ((Uzthst(k)-Thr)/(Thetas-Thr))
     +                            **Epsilon
                          IF ( fhold.LT.CLOSEZERO ) fhold = 0.0
                          Uzspst(k) = (Epsilon*Fks/(Thetas-Thr))*fhold
     +                                **eps_m1
                        ELSE
                          fhold = ((Uzthst(k-1)-Thr)/(Thetas-Thr))
     +                            **Epsilon
                          IF ( fhold.LT.CLOSEZERO ) fhold = 0.0
                          ftheta1 = Fks*fhold
                          fhold = ((Uzthst(k)-Thr)/(Thetas-Thr))
     +                            **Epsilon
                          IF ( fhold.LT.CLOSEZERO ) fhold = 0.0
                          ftheta2 = Fks*fhold
                          Uzspst(k) = (ftheta1-ftheta2)
     +                                /(Uzthst(k-1)-Uzthst(k))
                        END IF
                      END DO
                    END IF
C
C10-----ONLY ONE LEAD TRAIL AND ONE TRAIL WAVE BELOW WATER TABLE
C         AND THERE ARE MULTIPLE TRAIL WAVES IN SET ABOVE WATER TABLE.
                  ELSE IF ( ITRLSTH(j).GT.1 ) THEN
                    DO k = iset + 1, iset + ITRLSTH(j) - 1
                      Ltrlst(k) = 1
                      Itrlst(k) = 0
                      IF ( DABS(Uzthst(k)-Uzthst(k-1)).LT.NEARZERO )
     +                     THEN
                        fhold = ((Uzthst(k)-Thr)/(Thetas-Thr))
     +                          **Epsilon
                        IF ( fhold.LT.CLOSEZERO ) fhold = 0.0
                        Uzspst(k) = (Epsilon*Fks/(Thetas-Thr))*fhold
     +                              **eps_m1
                      ELSE
                        fhold = ((Uzthst(k-1)-Thr)/(Thetas-Thr))
     +                          **Epsilon
                        IF ( fhold.LT.CLOSEZERO ) fhold = 0.0
                        ftheta1 = Fks*fhold
                        fhold = ((Uzthst(k)-Thr)/(Thetas-Thr))
     +                          **Epsilon
                        IF ( fhold.LT.CLOSEZERO ) fhold = 0.0
                        ftheta2 = Fks*fhold
                        Uzspst(k) = (ftheta1-ftheta2)
     +                              /(Uzthst(k-1)-Uzthst(k))
                      END IF
                    END DO
                  END IF
                END DO
C
C11-----DETERMINE VOLUME OF WATER IN WAVES BELOW WATER TABLE.
                fm = 0.0D0
                j = iset
                DO WHILE ( j.LE.iset+Nwavst(i)-2 )
                  IF ( Ltrlst(j).EQ.1 .AND. Itrlst(j+1).GT.0 ) THEN
                    k = j
                    DO WHILE ( k.LE.j+Itrlst(j+1)-1 )
                      fm = fm + (Uzthst(k)-Thr)*(Uzdpst(k)-Uzdpst(k+1))
                      k = k + 1
                    END DO
                    IF ( k.EQ.iset+Nwavst(i)-1 ) THEN
                      fm = fm + (Uzthst(k)-Thr)*Uzdpst(k)
                    ELSE IF ( iset+Nwavst(i)-1.GT.k+1 .AND. Itrlst(k+2)
     +                        .GT.0 .AND. Ltrlst(k+1).EQ.1 ) THEN
                      fm = fm + (Uzthst(k)-Thr)*(Uzdpst(k)-Uzdpst(k+1))
                    ELSE
                      fm = fm + (Uzthst(k)-Thr)*(Uzdpst(k)-Uzdpst(k+1))
                    END IF
                    j = k
                  ELSE
                    fm = fm + (Uzthst(j)-Thr)*(Uzdpst(j)-Uzdpst(j+1))
                  END IF
                  j = j + 1
                END DO
                IF ( j.EQ.iset+Nwavst(i)-1 ) fm = fm + 
     +               (Uzthst(iset+Nwavst(i)-1)-Thr)
     +               *Uzdpst(iset+Nwavst(i)-1)
C
C12-----COMPUTE VOLUME OF WATER BELOW WATER TABLE WHEN
C         WHEN NO WAVES INTERSECTED.
              ELSE
                fm = 0.0D0
                j = iset
                Uzdpst(iset) = Uzdpst(iset) - (H-Hld)
                DO WHILE ( j.LE.iset+Nwavst(i)-2 )
                  IF ( Ltrlst(j).EQ.1 .AND. Itrlst(j+1).GT.0 ) THEN
                    k = j
                    DO WHILE ( k.LE.j+Itrlst(j+1)-1 )
                      fm = fm + (Uzthst(k)-Thr)*(Uzdpst(k)-Uzdpst(k+1))
                      k = k + 1
                    END DO
                    IF ( k.EQ.iset+Nwavst(i)-1 ) THEN
                      fm = fm + (Uzthst(k)-Thr)*Uzdpst(k)
                    ELSE IF ( iset+Nwavst(i)-1.GT.k+1 .AND. Itrlst(k+2)
     +                        .GT.0 .AND. Ltrlst(k+1).EQ.1 ) THEN
                      fm = fm + (Uzthst(k)-Thr)*(Uzdpst(k)-Uzdpst(k+1))
                    ELSE
                      fm = fm + (Uzthst(k)-Thr)*(Uzdpst(k)-Uzdpst(k+1))
                    END IF
                    j = k
                  ELSE
                    fm = fm + (Uzthst(j)-Thr)*(Uzdpst(j)-Uzdpst(j+1))
                  END IF
                  j = j + 1
                END DO
                IF ( j.EQ.iset+Nwavst(i)-1 ) fm = fm + 
     +               (Uzthst(iset+Nwavst(i)-1)-Thr)
     +               *Uzdpst(iset+Nwavst(i)-1)
              END IF
              IF ( fm.LT.0.0 ) fm = 0.0D0
              uzstorhold = Uzstor(i)
              Uzstor(i) = fm*Uzwdth(i)*Strlen
              Delstor(i) = Uzstor(i) - uzstorhold
C
C13-----CALCULATE CHANGE IN UNSATURATED ZONE STORAGE WHEN GROUND- 
C         WATER LEVEL DROPS.
            ELSE IF ( H.LE.Hld ) THEN
              fm = 0.0D0
              j = iset
              DO WHILE ( j.LE.iset+Nwavst(i)-2 )
                IF ( Ltrlst(j).EQ.1 .AND. Itrlst(j+1).GT.0 ) THEN
                  k = j
                  DO WHILE ( k.LE.j+Itrlst(j+1)-1 )
                    fm = fm + (Uzthst(k)-Thr)*(Uzdpst(k)-Uzdpst(k+1))
                    k = k + 1
                  END DO
                  IF ( k.EQ.iset+Nwavst(i)-1 ) THEN
                    fm = fm + (Uzthst(k)-Thr)*Uzdpst(k)
                  ELSE IF ( iset+Nwavst(i)-1.GT.k+1 .AND. Itrlst(k+2)
     +                      .GT.0 .AND. Ltrlst(k+1).EQ.1 ) THEN
                    fm = fm + (Uzthst(k)-Thr)*(Uzdpst(k)-Uzdpst(k+1))
                  ELSE
                    fm = fm + (Uzthst(k)-Thr)*(Uzdpst(k)-Uzdpst(k+1))
                  END IF
                  j = k
                ELSE
                  fm = fm + (Uzthst(j)-Thr)*(Uzdpst(j)-Uzdpst(j+1))
                END IF
                j = j + 1
              END DO
              IF ( j.EQ.iset+Nwavst(i)-1 ) fm = fm + 
     +             (Uzthst(iset+Nwavst(i)-1)-Thr)
     +             *Uzdpst(iset+Nwavst(i)-1)
              uzstorhold = Uzstor(i)
              Uzstor(i) = fm*Uzwdth(i)*Strlen
              Delstor(i) = Uzstor(i) - uzstorhold
            END IF
            IF ( Uzflwt(i).LE.0.0 ) Uzflwt(i) = 0.0D0
          END IF
          iset = iset + ntotuzn
        END DO
        DO i = 1, iuznhold
          IF ( loop(i).GT.0 ) THEN
            Totflwt = Totflwt + Uzflwt(i)
            Totdelstor = Totdelstor + Delstor(i)
            Totuzstor = Totuzstor + Uzstor(i)
          END IF
        END DO
        SUMRCH(L) = SUMRCH(L) + Totflwt
        STRM(29,L) = STRM(29,L) + Totdelstor
        IF ( Imassroute.EQ.1 ) THEN
          Gwflow = SUMRCH(L)/deltinc
          Ratin = Ratin + Gwflow
          BUFF(Ic, Ir, Il) = BUFF(Ic, Ir, Il) + Gwflow
          SFRUZBD(1) = SFRUZBD(1) + SUMLEAK(L)*deltinc
          SFRUZBD(2) = SFRUZBD(2) + STRM(29,L)
          SFRUZBD(3) = SFRUZBD(3) + SUMRCH(L)
          SFRUZBD(4) = SFRUZBD(4) + SUMLEAK(L)
          SFRUZBD(5) = SFRUZBD(5) + STRM(29,L)/deltinc
          SFRUZBD(6) = SFRUZBD(6) + SUMRCH(L)/deltinc
          SFRUZBD(8) = SFRUZBD(8) + SUMLEAK(L)
          SFRUZBD(7) = SFRUZBD(7) + SUMLEAK(L)
          SFRUZBD(10) = SFRUZBD(10) + Totuzstor
        END IF
C
C14-----UPDATE ALL UNSATURATED ZONE WAVES WHEN WATER TABLE DROPS
C         BELOW STREAMBED.
      ELSE IF ( htest1.LE.-CLOSEZERO .AND. htest2.GE.-CLOSEZERO ) THEN
        iset = 1
        DO i = 1, iuznhold
          Delstor(i) = 0.0D0
          Nwavst(i) = 1
          DO j = iset, iset + 5
            Uzthst(j) = Thr
            Uzdpst(j) = 0.0D0
            Uzspst(j) = 0.0D0
            Uzflst(j) = 0.0D0
            Itrlst(j) = 0
            Ltrlst(j) = 0
          END DO
          iset = iset + ntotuzn
        END DO
        CALL ROUTWAVESST(L, seep, H, Hld, Thr, Thetas, Fks, Epsilon, 
     +                   Iwidthcheck, Sbot, Icalc, Nwavst, Uzwdth, 
     +                   Uzflwt, Uzolsflx, Uzseep, Itrlst, Ltrlst, 
     +                   Uzspst, Uzflst, Uzdpst, Uzthst, Deltinc)
        DO kk = 1, iuznhold
          loop(kk) = 0
        END DO
        iset = 1
        DO i = 1, iuznhold
          ick = 0
          IF ( Uzthst(iset).GT.Thr .OR. Nwavst(i).GT.1 ) ick = 1
          IF ( i.LE.Iwidthcheck .OR. ick.EQ.1 ) loop(i) = 1
          iset = iset + ntotuzn
        END DO
        iset = 1
        DO i = 1, iuznhold
          IF ( loop(i).GT.0 ) THEN
            icounter = iset + Nwavst(i) - 1
            Delstor(i) = (Uzthst(icounter)-Thr)*(Uzdpst(icounter))
            DO j = iset, iset + Nwavst(i) - 2
              Delstor(i) = Delstor(i) + (Uzthst(j)-Thr)
     +                     *(Uzdpst(j)-Uzdpst(j+1))
            END DO
            Delstor(i) = Delstor(i)*Uzwdth(i)*Strlen
            Uzstor(i) = Delstor(i)
          END IF
          iset = iset + ntotuzn
        END DO
        DO i = 1, iuznhold
          IF ( loop(i).GT.0 ) THEN
            Totflwt = Totflwt + Uzflwt(i)
            Totdelstor = Totdelstor + Delstor(i)
            Totuzstor = Totuzstor + Uzstor(i)
          END IF
        END DO
        SUMRCH(L) = SUMRCH(L) + Totflwt
        STRM(29,L) = STRM(29,L) + Totdelstor
C
C15-----UPDATE RATES AND BUFFERS.
C
        IF ( Imassroute.EQ.1 ) THEN
          Gwflow = SUMRCH(L)/deltinc
          Ratin = Ratin + Gwflow
          BUFF(Ic, Ir, Il) = BUFF(Ic, Ir, Il) + Gwflow
          SFRUZBD(1) = SFRUZBD(1) + SUMLEAK(L)*deltinc
          SFRUZBD(2) = SFRUZBD(2) + STRM(29,L)
          SFRUZBD(3) = SFRUZBD(3) + SUMRCH(L)
          SFRUZBD(4) = SFRUZBD(4) + SUMLEAK(L)
          SFRUZBD(5) = SFRUZBD(5) + STRM(29,L)/deltinc
          SFRUZBD(6) = SFRUZBD(6) + SUMRCH(L)/deltinc
          SFRUZBD(7) = SFRUZBD(7) + SUMLEAK(L)
          SFRUZBD(8) = SFRUZBD(8) + SUMLEAK(L)
          SFRUZBD(10) = SFRUZBD(10) + Totuzstor
        END IF
      END IF
C
C16-----TOTAL WATER CONTENT OVER SPECIFIED DEPTH
C         FOR PRINTING WATER CONTENT PROFILES.
      IF ( Ibd.NE.0 .AND. Iunitgage.GT.0 ) THEN
        IF ( H.LT.Sbot ) THEN
          depthinc = Uzdpst(1)/20.001D0
          depthsave = depthinc
          totalwc = 0.0
          totalwc1 = 0.0
          k = 1
          DO WHILE ( depthsave.LE.Uzdpst(1) .AND. depthsave.GT.0.0 )
            widthsave = 0.0
            iset = 1
            fm = 0.0D0
            DO i = 1, iuznhold
              widthsave = widthsave + Wetper(i)
              jj = iset
              jk = iset + Nwavst(i) - 1
              DO WHILE ( jk.GE.iset )
                IF ( Uzdpst(jk).LT.depthsave ) jj = jk
                jk = jk - 1
              END DO
              IF ( jj.GT.iset ) THEN
                fm = fm + Uzthst(jj-1)*(depthsave-Uzdpst(jj))*Wetper(i)
     +               *Strlen
                DO j = jj, iset + Nwavst(i) - 2
                  fm = fm + Uzthst(j)*(Uzdpst(j)-Uzdpst(j+1))*Wetper(i)
     +                 *Strlen
                END DO
                fm = fm + Uzthst(iset+Nwavst(i)-1)
     +               *Uzdpst(iset+Nwavst(i)-1)*Wetper(i)*Strlen
              ELSE
                fm = fm + Uzthst(iset+Nwavst(i)-1)*depthsave*Wetper(i)
     +               *Strlen
              END IF
              IF ( i.EQ.1 ) THEN
                Wat1(k) = (fm-totalwc1)/(widthsave*Strlen*depthinc)
                totalwc1 = fm
              END IF
              iset = iset + ntotuzn
            END DO
            Avdpt(k) = depthsave
            Avwat(k) = (fm-totalwc)/(widthsave*Strlen*depthinc)
            totalwc = fm
            depthsave = depthsave + depthinc
            k = k + 1
          END DO
        END IF
      END IF
C17-----STORE UNSATURATED FLOW RATES FOR GAGE PACKAGE.
      STRM(21, L) = Totflwt/Deltinc
      STRM(22, L) = Totdelstor/Deltinc
      STRM(23, L) = Totuzstor
      DEALLOCATE (loop)
C18-----RETURN.
      RETURN
      END SUBROUTINE UZMASSBAL
C
C-------SUBROUTINE ROUTWAVESIT
      SUBROUTINE ROUTWAVESIT(L, Seep, H, Hld, Thr, Thetas, Fks, Epsilon,
     +                       Icalc, Nwavst, Uzwdth, Uzflwt, Uzolsflx, 
     +                       Uzseep, Itrlst, Ltrlst, Uzspst, Uzflst, 
     +                       Uzdpst, Uzthst, Itrlit, Ltrlit, Uzspit, 
     +                       Uzflit, Uzdpit, Uzthit, Deltinc, Sbot)
C     ******************************************************************
C     ROUTE UNSATURATED ZONE WAVES DURING MODEL ITERATIONS
C     CALLED FROM SUBROUTINE GWF2SFR7FM
!--------REVISED FOR MODFLOW-2005 RELEASE 1.9, FEBRUARY 6, 2012
C     ******************************************************************
C
      USE GWFSFRMODULE, ONLY: NSTOTRL, ISUZN, STRM, CLOSEZERO, NEARZERO
!!      USE GLOBAL,       ONLY: IOUT
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL Fks, Seep, Deltinc
      INTEGER L, Icalc
      INTEGER Nwavst(ISUZN), Itrlst(NSTOTRL), Ltrlst(NSTOTRL)
      INTEGER Itrlit(NSTOTRL), Ltrlit(NSTOTRL)
      DOUBLE PRECISION Uzwdth(ISUZN), Uzflwt(ISUZN), Uzolsflx(ISUZN),
     +                 Uzseep(ISUZN), Sbot
      DOUBLE PRECISION Uzspst(NSTOTRL), Uzflst(NSTOTRL), 
     +                 Uzdpst(NSTOTRL), Uzthst(NSTOTRL)
      DOUBLE PRECISION Uzspit(NSTOTRL), Uzflit(NSTOTRL), 
     +                 Uzdpit(NSTOTRL), Uzthit(NSTOTRL)
      DOUBLE PRECISION H, Hld, Thr, Thetas, Epsilon
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      INTEGER i, ick, iset, iwav, numwaves, iuzntemp, ntotuzn
      DOUBLE PRECISION dlength, zoldist, totflux, surflux, oldsflx, 
     +                 htest2
C     -----------------------------------------------------------------
C
C1------ROUTE WAVES THROUGH EACH UNSATURATED ZONE COMPARTMENT BENEATH
C         STREAM.
      htest2 = Hld - Sbot
      iset = 1
      iuzntemp = 0
      IF ( Icalc.EQ.2 ) THEN
        iuzntemp = ISUZN
      ELSE IF ( Icalc.EQ.1 ) THEN
        iuzntemp = 1
      ENDIF
      ntotuzn = NSTOTRL/ISUZN
      DO i = 1, iuzntemp
        ick = 0
        IF ( Uzthst(iset).GT.Thr .OR. Nwavst(i).GT.1 ) ick = 1
        IF ( Uzwdth(i).LE.0.0 ) Seep = 0.0
        IF ( Uzwdth(i).GT.0.0 .OR. ick.EQ.1 ) THEN
          numwaves = Nwavst(i)
          IF ( htest2.GE.-2.0*CLOSEZERO ) THEN
            DO iwav = iset, iset + 5
              Uzthit(iwav) = Thr
              Uzdpit(iwav) = 0.0D0
              Uzspit(iwav) = 0.0D0
              Uzflit(iwav) = 0.0D0
              Itrlit(iwav) = 0
              Ltrlit(iwav) = 0
              Nwavst(i) = 1
              numwaves = 1
            END DO
          ELSE
            DO iwav = iset, iset + numwaves - 1
              Uzthit(iwav) = Uzthst(iwav)
              Uzdpit(iwav) = Uzdpst(iwav)
              Uzspit(iwav) = Uzspst(iwav)
              Uzflit(iwav) = Uzflst(iwav)
              Itrlit(iwav) = Itrlst(iwav)
              Ltrlit(iwav) = Ltrlst(iwav)
            END DO
          END IF
         IF ( DABS(Uzdpst(iset)).GT.NEARZERO ) THEN
            dlength = Uzdpst(iset) + Hld - H
            zoldist = Uzdpst(iset)
          ELSE
            dlength = Sbot - H
            zoldist = 0.0D0
          END IF
          IF ( dlength.LT.0.0 ) dlength = 0.0D0
          IF ( zoldist.LT.0.0 ) zoldist = 0.0D0
          surflux = Uzseep(i)
          oldsflx = Uzolsflx(i)
          CALL UZFLOW(L, surflux, dlength, zoldist, Uzdpit, Uzthit, 
     +                Uzflit, Uzspit, Itrlit, Ltrlit, totflux, 
     +                numwaves, Thr, Thetas, Fks, Epsilon, oldsflx, 
     +                iset, Deltinc) 
          IF ( totflux.LT.0.0 ) totflux = 0.0D0
          Uzflwt(i) = totflux*Uzwdth(i)*STRM(1, L)
          IF ( Uzflwt(i).LT.0.0 ) Uzflwt(i) = 0.0D0
        ELSE
          Uzflwt(i) = 0.0D0
        END IF
        iset = iset + ntotuzn
      END DO
C
C2------RETURN.
      RETURN
      END SUBROUTINE ROUTWAVESIT
C
C-------SUBROUTINE ROUTWAVESST
      SUBROUTINE ROUTWAVESST(L, Seep, H, Hld, Thr, Thetas, Fks, Epsilon,
     +                       Iwidthcheck, Sbot, Icalc, Nwavst, Uzwdth, 
     +                       Uzflwt, Uzolsflx, Uzseep, Itrlst, Ltrlst, 
     +                       Uzspst, Uzflst, Uzdpst, Uzthst, Deltinc)
C     ******************************************************************
C     ROUTE UNSATURATED-ZONE WAVES AFTER FINAL ITERATION
C     CALLED FROM SUBROUTINE GWF2SFR7BD
!--------REVISED FOR MODFLOW-2005 RELEASE 1.9, FEBRUARY 6, 2012
C     ******************************************************************
      USE GWFSFRMODULE, ONLY: NSTOTRL, ISUZN, STRM, NEARZERO
!!      USE GLOBAL,       ONLY: IOUT
      IMPLICIT NONE
      INTRINSIC DABS
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL Fks, Seep, Deltinc
      INTEGER Iwidthcheck, L, Icalc
      INTEGER Nwavst(ISUZN), Itrlst(NSTOTRL), Ltrlst(NSTOTRL)
      DOUBLE PRECISION Uzwdth(ISUZN), Uzflwt(ISUZN), Uzolsflx(ISUZN),
     +                 Uzseep(ISUZN)
      DOUBLE PRECISION Uzspst(NSTOTRL), Uzflst(NSTOTRL), 
     +                 Uzdpst(NSTOTRL), Uzthst(NSTOTRL)
      DOUBLE PRECISION H, Hld, Thr, Thetas, Epsilon, Sbot
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      INTEGER i, ick, iset, numwaves, iuzntemp
      DOUBLE PRECISION dlength, zoldist, totflux, surflux, oldsflx
C     -----------------------------------------------------------------
C
C1------ROUTE WAVES THROUGH EACH UNSATURATED ZONE COMPARTMENT BENEATH
C         STREAM.
      iset = 1
      Sbot = STRM(4, L)
      iuzntemp = 0
      IF ( Icalc.EQ.2 ) THEN
        iuzntemp = ISUZN
      ELSE IF ( Icalc.EQ.1 ) THEN
        iuzntemp = 1
      ENDIF
      DO i = 1, iuzntemp
        ick = 0
        IF ( Uzthst(iset).GT.Thr .OR. Nwavst(i).GT.1 ) ick = 1
        IF ( i.GT.Iwidthcheck ) Seep = 0.0
        IF ( i.LE.Iwidthcheck .OR. ick.EQ.1 ) THEN
          numwaves = Nwavst(i)
          IF ( DABS(Uzdpst(iset)).GT.NEARZERO ) THEN
            dlength = Uzdpst(iset) + Hld - H
            zoldist = Uzdpst(iset)
          ELSE
            dlength = Sbot - H
            zoldist = 0.0D0
          END IF
          IF ( dlength.LT.0.0 ) dlength = 0.0D0
          IF ( zoldist.LT.0.0 ) zoldist = 0.0D0
          surflux = Uzseep(i)
          oldsflx = Uzolsflx(i)
          CALL UZFLOW(L, surflux, dlength, zoldist, Uzdpst, Uzthst, 
     +                Uzflst, Uzspst, Itrlst, Ltrlst, totflux, 
     +                numwaves, Thr, Thetas, Fks, Epsilon, oldsflx, 
     +                iset, Deltinc) 
          Nwavst(i) = numwaves
          Uzflwt(i) = totflux*Uzwdth(i)*STRM(1, L)
          IF ( Uzflwt(i).LT.0.0 ) Uzflwt(i) = 0.0D0
        ELSE
          Uzflwt(i) = 0.0D0
          Uzdpst(iset) = Uzdpst(1)
        END IF
        Uzolsflx(i) = Uzseep(i)
        iset = iset + NSTOTRL/ISUZN
      END DO
C2------RETURN.
      RETURN
      END SUBROUTINE ROUTWAVESST
C
C-------SUBROUTINE UZFLOW
      SUBROUTINE UZFLOW(I, Surflux, Dlength, Zoldist, Depth, Theta, 
     +                  Flux, Speed, Itrwave, Ltrail, Totalflux, 
     +                  Numwaves, Thetar, Thetas, Fksat, Eps, Oldsflx, 
     +                  Jpnt, Deltinc)
C     ******************************************************************
C     WAVE INTERACTION WITHIN AN UNSATURATED FLOW COMPARTMENT
!--------REVISED FOR MODFLOW-2005 RELEASE 1.9, FEBRUARY 6, 2012
C     ******************************************************************
      USE GWFSFRMODULE, ONLY: NSTOTRL, NSFRSETS, NSTRAIL, THETAB, FLUXB
      USE GLOBAL,    ONLY: IOUT
      IMPLICIT NONE
      INTRINSIC ABS, DABS
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER I, Jpnt, Numwaves, Itrwave(NSTOTRL), Ltrail(NSTOTRL)
      REAL Fksat, Deltinc
      DOUBLE PRECISION Depth(NSTOTRL), Theta(NSTOTRL), Flux(NSTOTRL), 
     +                 Speed(NSTOTRL)
      DOUBLE PRECISION Dlength, Zoldist, Totalflux, Surflux, Oldsflx, 
     +                 Thetar, Thetas, Eps
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      DOUBLE PRECISION ffcheck, feps2, feps, time, fm, dlength2
      REAL thetadif
      INTEGER itester, j, jj, jm1, itrailflg
C     ------------------------------------------------------------------
      time = 0.0D0
      Totalflux = 0.0D0
      feps = 1.0D-12/Deltinc
      feps2 = 1.0D-12/Deltinc
      itrailflg = 0
C
C       FEPS IS USED TO SUPPRESS A NEW WAVE WHEN CHANGES IN WATER TABLE 
C       ARE NEGLIGIBLE. FEPS2 IS USED TO SUPPRESS A NEW WAVE WHEN
C       CHANGES IN FLUX ARE NEGLIGIBLE.
      IF ( feps.LT.1.0D-8 ) feps = 1.0D-8
      IF ( feps2.LT.1.0D-8 ) feps2 = 1.0D-8
      fm = 0.0D0
      Oldsflx = Flux(Jpnt+Numwaves-1)
C
C1------DETERMINE IF WATER TABLE IS RISING OR FALLING.
      IF ( (Dlength-Zoldist).LT.-feps ) THEN
        dlength2 = Dlength
        Dlength = Zoldist
      ELSE IF ( (Dlength-Zoldist).GT.feps ) THEN
        dlength2 = Zoldist + 1.0D0
        thetadif = ABS(Theta(Jpnt)-Thetar)
        IF ( thetadif.GT.1.0E-6 ) THEN
          DO j = Jpnt + Numwaves, Jpnt + 1, -1
            jm1 = j - 1
            Theta(j) = Theta(jm1)
            Flux(j) = Flux(jm1)
            Speed(j) = Speed(jm1)
            Depth(j) = Depth(jm1)
            Itrwave(j) = Itrwave(jm1)
            Ltrail(j) = Ltrail(jm1)
          END DO
          IF ( Theta(Jpnt+1).GT.Thetar ) THEN
            Speed(Jpnt+1) = Flux(Jpnt+1)/(Theta(Jpnt+1)-Thetar)
          ELSE
            Speed(Jpnt+1) = 0.0D0
          END IF
          Theta(Jpnt) = Thetar
          Flux(Jpnt) = 0.0D0
          Speed(Jpnt) = 0.0D0
          Depth(Jpnt) = Dlength
          Ltrail(Jpnt) = 0
          Numwaves = Numwaves + 1
          IF ( Numwaves.GT.NSFRSETS*NSTRAIL ) THEN
            WRITE (*, *) 'TOO MANY WAVES IN STREAM CELL', I, Numwaves, 
     +                   '   PROGRAM TERMINATED IN UZFLOW-1'
            WRITE (IOUT, *)'TOO MANY WAVES IN STREAM CELL', I, Numwaves,
     +           '   PROGRAM TERMINATED IN UZFLOW-1; INCREASE NSFRSETS'
            STOP
          END IF
        ELSE
          Depth(Jpnt) = Dlength
        END IF
      ELSE
        dlength2 = Zoldist + 1.0D0
      END IF
      fm = 0.0D0
      THETAB = Theta(Jpnt)
      FLUXB = Flux(Jpnt)
      Totalflux = 0.00D0
      itester = 0
      ffcheck = (Surflux-Flux(Jpnt+Numwaves-1))
C
C2------CREATE A NEW WAVE IF SURFACE FLUX CHANGES.
C         CALL TRAILWAVE IF SURFACE FLUX DECREASES.
C         CALL LEADWAVE IF SURFACE FLUX INCREASES.
      IF ( DABS(ffcheck).GT.feps2 ) THEN
        Numwaves = Numwaves + 1
        IF ( Numwaves.GT.NSFRSETS*NSTRAIL ) THEN
          WRITE (*, *) 'TOO MANY WAVES IN STREAM CELL', I, Numwaves, 
     +                 '   PROGRAM TERMINATED IN UZFLOW-2'
          WRITE (IOUT, *) 'TOO MANY WAVES IN STREAM CELL', I, Numwaves, 
     +       '   PROGRAM TERMINATED IN UZFLOW-2; INCREASE NSFRSETS'
          STOP
        END IF
      ELSE IF ( Numwaves.EQ.1 ) THEN
        itester = 1
      END IF

      IF ( Numwaves.GT.1 ) THEN
        IF ( ffcheck.LT.-feps2 ) THEN
          CALL TRAILWAVE(Numwaves, I, Flux, Theta, Speed, Depth, 
     +                   Itrwave, Ltrail, Fksat, Eps, Thetas, Thetar, 
     +                   Surflux, Jpnt)
          itrailflg = 1
        END IF
        CALL LEADWAVE(Numwaves, time, Totalflux, itester, Flux,
     +                Theta, Speed, Depth, Itrwave, Ltrail, Fksat, 
     +                Eps, Thetas, Thetar, Surflux, Oldsflx, Jpnt, 
     +                feps2, itrailflg, Deltinc)
      END IF
      IF ( itester.EQ.1 ) THEN
        Totalflux = Totalflux + (Deltinc-time)*Flux(Jpnt)
        time = 0.0D0
        itester = 0
      END IF
C
C3------CALCULATE VOLUME OF WATER IN UNSATURATED ZONE LOST WHEN
C         WATER TABLE ROSE AND ADD AS RECHARGE TO GROUND WATER.
      IF ( dlength2.LT.Zoldist ) THEN
        j = 2
        jj = 1
        IF ( Depth(Jpnt+1).GT.dlength2 ) THEN
          DO WHILE ( j.LE.Numwaves )
            IF ( Depth(Jpnt+j-1).GE.dlength2 ) jj = j
            IF ( j.EQ.jj .AND. Depth(Jpnt+j).LT.dlength2 ) j = Numwaves
            j = j + 1
          END DO
        END IF
        IF ( jj.GT.1 .AND. Numwaves.GT.1 ) THEN
          fm = (Depth(Jpnt)-Depth(Jpnt+1))*(Theta(Jpnt)-Thetar)
          DO j = 2, jj - 1
            fm = fm + (Depth(Jpnt+j-1)-Depth(Jpnt+j))*(Theta(Jpnt+j-1)-
     +           Thetar)
          END DO
          fm = fm+(Theta(Jpnt+jj-1)-Thetar)*(Depth(Jpnt+jj-1)-dlength2)
        ELSE
          fm = (Depth(Jpnt)-dlength2)*(Theta(Jpnt)-Thetar)
        END IF
        Dlength = dlength2
        Totalflux = Totalflux + fm
        IF ( Totalflux.LT.1.0D-30 ) Totalflux = 0.0D0
      END IF
C4------RETURN.
      RETURN
      END SUBROUTINE UZFLOW
C
C
C-------SUBROUTINE LEADWAVE
      SUBROUTINE LEADWAVE(Numwaves, Time, Totalflux, Itester, Flux,
     +                    Theta, Speed, Depth, Itrwave, Ltrail, Fksat, 
     +                    Eps, Thetas, Thetar, Surflux, Oldsflx, Jpnt, 
     +                    Feps2, Itrailflg, Deltinc)
C     ******************************************************************
C     CREATE LEAD WAVE WHEN THE SURFACE FLUX INCREASES AND ROUTE WAVES.
!--------REVISED FOR MODFLOW-2005 RELEASE 1.9, FEBRUARY 6, 2012
C     ******************************************************************
C
      USE GWFSFRMODULE, ONLY: NSTOTRL, NEARZERO, CLOSEZERO, THETAB,
     +                        FLUXB, FLUXHLD2
C      USE GLOBAL,       ONLY: IOUT
      IMPLICIT NONE
      INTRINSIC ABS
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER Itester, Jpnt, Numwaves, Itrailflg
      INTEGER Itrwave(NSTOTRL), Ltrail(NSTOTRL)
      REAL Fksat, Deltinc
      DOUBLE PRECISION Depth(NSTOTRL), Theta(NSTOTRL), Flux(NSTOTRL),
     +                 Speed(NSTOTRL)
      DOUBLE PRECISION Feps2, Totalflux, Surflux, Oldsflx, Thetar, Time
      DOUBLE PRECISION Eps, Thetas
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      DOUBLE PRECISION ffcheck, bottomtime, shortest, fcheck, fhold
      DOUBLE PRECISION eps_m1, checktime(NSTOTRL), timenew, feps3
      REAL big, comp1, comp2, diff, f7, f8, ftheta1, ftheta2 
      INTEGER idif, iflag, iflag2, iflx, iremove, itrwaveb, j, jj, k, 
     +        kk, l, jpnwavesm1, jpntpkm1, jpntpkm2, more(NSTOTRL)
C     ------------------------------------------------------------------
C     ADDED FEPS3 TO LIMIT CHANGES IN WATER CONTENT.
      eps_m1 = Eps - 1.0D0
      feps3 = 1.0D-07
      f7 = 0.495
      f8 = 1.0 - f7
      big = 1.0E30
C
C1------INITIALIZE NEWEST WAVE.
      IF ( Itrailflg.EQ.0 ) THEN
        jpnwavesm1 = Jpnt + Numwaves - 1
        ffcheck = Surflux - Oldsflx
        IF ( ffcheck.GT.Feps2 ) THEN
          Flux(jpnwavesm1) = Surflux
          IF ( Flux(jpnwavesm1).LT.NEARZERO ) Flux(jpnwavesm1) = 0.0D0
          Theta(jpnwavesm1) = (((Flux(jpnwavesm1)/Fksat)**(1.0D0/Eps))*
     +                        (Thetas-Thetar)) + Thetar
          IF ( Theta(jpnwavesm1)-Theta(jpnwavesm1-1).GT.feps3 ) THEN
            Speed(jpnwavesm1) = (Flux(jpnwavesm1)-Flux(jpnwavesm1-1))/
     +                          (Theta(jpnwavesm1)-Theta(jpnwavesm1-1))
            Depth(jpnwavesm1) = 0.0D0
            Ltrail(jpnwavesm1) = 0
            Itrwave(jpnwavesm1) = 0
          ELSE
            Speed(jpnwavesm1) = 0.0D0
            Numwaves = Numwaves - 1
          END IF
        END IF
      END IF
C
C2------ROUTE ALL WAVES AND INTERCEPTION OF WAVES OVER TIME STEP.
      diff = 1.0
      iflx = 0
      FLUXHLD2 = Flux(Jpnt)
      IF ( Numwaves.EQ.0 ) Itester = 1
      DO WHILE ( diff.GT.1.0E-7 .AND. Itester.NE.1 )
        DO j = 1, Numwaves
          checktime(j) = 0.0D0
          more(j) = 0
        END DO
        j = 2
C
C3------CALCULATE TIME UNTIL A WAVE WILL OVERTAKE NEXT WAVE BELOW.
! RGN 1/25/08 broke up IF statement to make sure Itrwave(Jpnt+j) does not exceed bounds.
        DO WHILE ( j.LE.Numwaves )
          IF ( j.LT.Numwaves ) THEN
            IF ( Ltrail(Jpnt+j-1).NE.0 .AND. Itrwave(Jpnt+j).GT.0 ) THEN
              DO WHILE ( Ltrail(Jpnt+j-1).NE.0 .AND. 
     +                 Itrwave(Jpnt+j).GT.0)
                kk = j + Itrwave(Jpnt+j)
                IF ( j.GT.2 .AND. ABS(Speed(Jpnt+j-2)-Speed(Jpnt+j-1))
     +              .GT.CLOSEZERO ) THEN
                  checktime(j) = (Depth(Jpnt+j-1)-Depth(Jpnt+j-2))
     +                     /(Speed(Jpnt+j-2)-Speed(Jpnt+j-1))
                ELSE
                  checktime(j) = big
                END IF
                IF ( Numwaves.GT.kk ) THEN
                  jj = j
                  j = j + Itrwave(Jpnt+j) + 1
C
C4------LEAD WAVE INTERSECTING TRAIL WAVE.
                  fhold = 0.0D0
                  IF ( ABS(Theta(Jpnt+jj-1)-Thetar).GT.CLOSEZERO )
     +                 fhold = (f7*Theta(Jpnt+j-2)+f8*Theta(Jpnt+j-3)-
     +                           Thetar)/(Theta(Jpnt+jj-1)-Thetar)
                  IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                  checktime(j) = (Depth(Jpnt+j-1)-Depth(Jpnt+jj-1)
     +                         *(fhold**eps_m1))/(Speed(Jpnt+jj-1)
     +                         *(fhold**eps_m1)-Speed(Jpnt+j-1))
                ELSE
                  j = j + 1
                END IF
              END DO
            ELSE IF ( ABS(Speed(Jpnt+j-2)-Speed(Jpnt+j-1)).GT.
     +              CLOSEZERO .AND. j.NE.1 )THEN
              checktime(j) = (Depth(Jpnt+j-1)-Depth(Jpnt+j-2))
     +                     /(Speed(Jpnt+j-2)-Speed(Jpnt+j-1))
            ELSE
              checktime(j) = big
            END IF
          ELSE IF ( ABS(Speed(Jpnt+j-2)-Speed(Jpnt+j-1)).GT.
     +              CLOSEZERO .AND. j.NE.1 )THEN
            checktime(j) = (Depth(Jpnt+j-1)-Depth(Jpnt+j-2))
     +                     /(Speed(Jpnt+j-2)-Speed(Jpnt+j-1))
          ELSE
            checktime(j) = big
          END IF          
          j = j + 1
        END DO
        DO j = 2, Numwaves
          IF ( checktime(j).LT.NEARZERO ) checktime(j) = big
        END DO
C
C5------CALCULATE HOW LONG IT WILL TAKE BEFORE DEEPEST WAVE REACHES
C         WATER TABLE.
        IF ( Numwaves.GT.1 ) THEN
          bottomtime = (Depth(Jpnt)-Depth(Jpnt+1))/Speed(Jpnt+1)
          IF ( bottomtime.LE.0.0 ) bottomtime = 1.0D-12
        ELSE
          bottomtime = big
        END IF
C6------CALCULATE SHORTEST TIME FOR WAVE INTERCEPTION.
        shortest = Deltinc - Time
        DO j = Numwaves, 3, -1
          IF ( CHECKTIME(j).LE.shortest ) THEN
            MORE(j) = 1
            shortest = CHECKTIME(j)
! Next lines were commented out based on Steve R
!            DO k = j + 1, Numwaves
!              IF ( ABS(CHECKTIME(k)-shortest).GT.CLOSEZERO )
!     +             MORE(k) = 0
!            END DO
          END IF
        END DO
        DO k = 3, Numwaves
          IF ( CHECKTIME(k)>shortest ) MORE(k) = 0
        END DO
        IF ( Numwaves.EQ.2 ) shortest = Deltinc - Time
C
C7------CHECK IF DEEPEST WAVE REACHES WATER TABLE BEFORE WAVES 
C          INTERCEPT EACH OTHER.
        iremove = 0
        timenew = Time
        fcheck = (Time+shortest) - Deltinc
        IF ( shortest.LT.1.0E-7 ) fcheck = -1.0D0
        IF ( bottomtime.LT.shortest .AND.
     +       Time+bottomtime.LE.Deltinc ) THEN
          j = 2
          DO WHILE ( j.LE.Numwaves )
C
C8------ROUTE TRAIL WAVES.
            IF ( Itrwave(Jpnt+j-1).EQ.0 ) THEN
              Depth(Jpnt+j-1) = Depth(Jpnt+j-1) + Speed(Jpnt+j-1)
     +                          *bottomtime
            ELSE
              DO k = j, j + Itrwave(Jpnt+j-1) - 1
Cdep check to see if theta = thetar do not route?
                IF(Theta(Jpnt+j-2)-Thetar.GT.CLOSEZERO) THEN
                  Depth(Jpnt+k-1) = Depth(Jpnt+j-2)*((f7*Theta(Jpnt+k-1)
     +                              +f8*Theta(Jpnt+k-2)-Thetar)
     +                              /(Theta(Jpnt+j-2)-Thetar))**eps_m1
                END IF
              END DO
              j = j + Itrwave(Jpnt+j-1) - 1
            END IF
            j = j + 1
          END DO
          FLUXB = Flux(Jpnt+1)
          THETAB = Theta(Jpnt+1)
          iflx = 1
          itrwaveb = Itrwave(Jpnt+2)
          DO k = 2, Numwaves
            jpntpkm1 = Jpnt + k - 1
            jpntpkm2 = jpntpkm1 - 1
            Flux(jpntpkm2) = Flux(jpntpkm1)
            Theta(jpntpkm2) = Theta(jpntpkm1)
            Speed(jpntpkm2) = Speed(jpntpkm1)
            Depth(jpntpkm2) = Depth(jpntpkm1)
            Itrwave(jpntpkm2) = Itrwave(jpntpkm1)
            Ltrail(jpntpkm2) = Ltrail(jpntpkm1)
          END DO
          IF ( itrwaveb.EQ.1 ) THEN
            Itrwave(Jpnt+1) = 0
            Ltrail(Jpnt+1) = 1
            fhold = (Theta(Jpnt+1)-Thetar)/(Thetas-Thetar)
            IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
            Speed(Jpnt+1) = (Eps*Fksat/(Thetas-Thetar))*fhold**eps_m1
C
C9------MAKE ALL TRAIL WAVES LEAD TRAIL WAVES.
          ELSE IF ( itrwaveb.GT.1 ) THEN
            DO k = Jpnt + 1, Jpnt + itrwaveb
              Itrwave(k) = 0
              Ltrail(k) = 1
              IF ( ABS(Theta(k)-Theta(k-1)).LT.CLOSEZERO ) THEN
                fhold = ((Theta(k)-Thetar)/(Thetas-Thetar))**Eps
                IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                Speed(k) = (Eps*Fksat/(Thetas-Thetar))*fhold**eps_m1
              ELSE
                fhold = ((Theta(k-1)-Thetar)/(Thetas-Thetar))**Eps
                IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                ftheta1 = Fksat*fhold
                fhold = ((Theta(k)-Thetar)/(Thetas-Thetar))**Eps
                IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                ftheta2 = Fksat*fhold
                Speed(k) = (ftheta1-ftheta2)/(Theta(k-1)-Theta(k))
              END IF
            END DO
          END IF
          iremove = 1
          timenew = Time + bottomtime
          Ltrail(Jpnt) = 0
          Speed(Jpnt) = 0.0D0
C
C10-----CHECK IF WAVES INTERCEPT BEFORE TIME STEP ENDS.
        ELSE IF ( fcheck.LT.0.0 .AND. Numwaves.GT.2 ) THEN
          j = 2
          DO WHILE ( j.LE.Numwaves )
            IF ( Itrwave(Jpnt+j-1).EQ.0 ) THEN
              Depth(Jpnt+j-1) = Depth(Jpnt+j-1) + Speed(Jpnt+j-1)
     +                          *shortest
            ELSE
C
C11-----ROUTE TRAIL WAVES.
              DO k = j, j + Itrwave(Jpnt+j-1) - 1
Cdep check to see if theta = thetar do not route?
                IF(Theta(Jpnt+j-2)-Thetar.GT.CLOSEZERO) THEN
                  Depth(Jpnt+k-1) = Depth(Jpnt+j-2)*((f7*Theta(Jpnt+k-1)
     +                              +f8*Theta(Jpnt+k-2)-Thetar)
     +                              /(Theta(Jpnt+j-2)-Thetar))**eps_m1
                END IF
              END DO
              j = j + Itrwave(Jpnt+j-1) - 1
            END IF
            j = j + 1
          END DO
C
C12-----REMOVE WAVES THAT HAVE BEEN INTERCEPTED AND COMPUTE SPEED OF
C         COMBINED WAVE.
          j = 3
          l = j
          iflag = 0
          DO WHILE ( iflag.EQ.0 )
            IF ( more(j).EQ.1 ) THEN
              l = j
              IF ( Ltrail(Jpnt+j-1).NE.1 ) THEN
                iflag2 = 0
                k = j - 1
                idif = 0
                DO WHILE ( iflag2.EQ.0 )
                  IF ( Itrwave(Jpnt+k-1).GT.0 ) THEN
                    iflag2 = 1
                    idif = j - k
                    IF ( idif.EQ.Itrwave(Jpnt+k-1) )
     +                   Itrwave(Jpnt+k-1) = Itrwave(Jpnt+k-1) - 1
                  ELSE
                    k = k - 1
                    IF ( k.EQ.0 ) iflag2 = 1
                  END IF
                END DO
                IF ( j.EQ.3 ) THEN
                  comp1 = ABS(Theta(Jpnt+j-1)-THETAB)
                  comp2 = ABS(Flux(Jpnt+j-1)-FLUXB)
                  IF ( comp1.LE.1.E-9 ) Theta(Jpnt+j-1) = THETAB - 1.D-9
                  IF ( comp2.LE.1.E-15 ) Flux(Jpnt+j-1) = FLUXB - 1.D-15
                  Speed(Jpnt+j-1) = (Flux(Jpnt+j-1)-FLUXB)
     +                              /(Theta(Jpnt+j-1)-THETAB)
                ELSE
                  comp1 = ABS(Theta(Jpnt+j-1)-Theta(Jpnt+j-3))
                  comp2 = ABS(Flux(Jpnt+j-1)-Flux(Jpnt+j-3))
                  IF ( comp1.LT.1.0E-9 ) Theta(Jpnt+j-1)
     +                 = Theta(Jpnt+j-3) - 1.0D-9
                  IF ( comp2.LT.1.0E-15 ) Flux(Jpnt+j-1)
     +                 = Flux(Jpnt+j-3) - 1.0D-15
                  Speed(Jpnt+j-1) = (Flux(Jpnt+j-1)-Flux(Jpnt+j-3))/
     +                              (Theta(Jpnt+j-1)-Theta(Jpnt+j-3))
                END IF
              ELSE IF ( Itrwave(Jpnt+j).GT.0 ) THEN
                IF ( ABS(Speed(Jpnt+j-2)).GT.CLOSEZERO ) THEN
C
C13-----CONVERT TRAIL WAVES TO LEAD TRAIL WAVES WHEN LEAD TRAIL 
C         WAVE INTERSECTS A LEAD WAVE.
                  DO k = Jpnt + j, Jpnt + j + Itrwave(Jpnt+j) - 1
                    Ltrail(k) = 1
                    Itrwave(k) = 0
                    IF ( ABS(Theta(k)-Theta(k-1)).LT.CLOSEZERO ) THEN
                      fhold = ((Theta(k)-Thetar)/(Thetas-Thetar))**Eps
                      IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                      Speed(k) = (Eps*Fksat/(Thetas-Thetar))*fhold
     +                           **eps_m1
                    ELSE
                      fhold = ((Theta(k-1)-Thetar)/(Thetas-Thetar))**Eps
                      IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                      ftheta1 = Fksat*fhold
                      fhold = ((Theta(k)-Thetar)/(Thetas-Thetar))**Eps
                      IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                      ftheta2 = Fksat*fhold
                      Speed(k) = (ftheta1-ftheta2)/(Theta(k-1)-Theta(k))
                    END IF
                  END DO
                  Ltrail(Jpnt+j-1) = 0
                  IF ( j.EQ.3 ) THEN
C
C14-----RECALCULATE FLUX.
                    comp1 = ABS(Theta(Jpnt+j-1)-THETAB)
                    comp2 = ABS(Flux(Jpnt+j-1)-FLUXB)
                    IF (comp1.LE.1.E-9) Theta(Jpnt+j-1) = THETAB - 1.D-9
                    IF (comp2.LE.1.E-15) Flux(Jpnt+j-1) = FLUXB - 1.D-15
                    Speed(Jpnt+j-1) = (Flux(Jpnt+j-1)-FLUXB)
     +                                /(Theta(Jpnt+j-1)-THETAB)
                    IF ( Flux(Jpnt+j-1)-FLUXB.LT.0.0D0 ) THEN
                      fhold = (Theta(Jpnt+j-1)-Thetar)/(Thetas-Thetar)
                      IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                      Speed(Jpnt+j-1) = ((Eps*Fksat)/(Thetas-Thetar))
     +                                  *fhold**eps_m1
                      Ltrail(Jpnt+j-1) = 1
                    ELSE
                      Speed(Jpnt+j-1) = (Flux(Jpnt+j-1)-FLUXB)
     +                                  /(Theta(Jpnt+j-1)-THETAB)
                    END IF

                  ELSE
                    comp1 = ABS(Theta(Jpnt+j-1)-Theta(Jpnt+j-3))
                    comp2 = ABS(Flux(Jpnt+j-1)-Flux(Jpnt+j-3))
                    IF ( comp1.LT.1.0E-9 ) Theta(Jpnt+j-1)
     +                   = Theta(Jpnt+j-3) - 1.0D-9
                    IF ( comp2.LT.1.0E-15 ) Flux(Jpnt+j-1)
     +                   = Flux(Jpnt+j-3) - 1.0D-15
                    IF ( Flux(Jpnt+j-1)-Flux(Jpnt+j-3).LT.0.0D0 ) THEN
                      fhold = (Theta(Jpnt+j-1)-Thetar)/(Thetas-Thetar)
                      IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                      Speed(Jpnt+j-1) = ((Eps*Fksat)/(Thetas-Thetar))
     +                                  *fhold**eps_m1
                      Ltrail(Jpnt+j-1) = 1
                    ELSE
                      Speed(Jpnt+j-1) = (Flux(Jpnt+j-1)-Flux(Jpnt+j-3))
     +                                /(Theta(Jpnt+j-1)-Theta(Jpnt+j-3))
                    END IF

                  END IF
                END IF
                iflag2 = 0
                k = j - 1
                idif = 0
                DO WHILE ( iflag2.EQ.0 )
                  IF ( Itrwave(Jpnt+k-1).GT.0 ) THEN
                    iflag2 = 1
                    idif = j - k
                    IF ( idif.EQ.Itrwave(Jpnt+k-1) )
     +                   Itrwave(Jpnt+k-1) = Itrwave(Jpnt+k-1) - 1
                  ELSE
                    k = k - 1
                    IF ( k.EQ.0 ) iflag2 = 1
                  END IF
                END DO
                j = j + Itrwave(Jpnt+j+1) + 2
              ELSE
                Ltrail(Jpnt+j-1) = 0
                Itrwave(Jpnt+j) = 0
                IF ( j.EQ.3 ) THEN
                  comp1 = ABS(Theta(Jpnt+j-1)-THETAB)
                  comp2 = ABS(Flux(Jpnt+j-1)-FLUXB)
                  IF ( comp1.LE.1.E-9 ) Theta(Jpnt+j-1) = THETAB - 1.D-9
                  IF ( comp2.LE.1.E-15 ) Flux(Jpnt+j-1) = FLUXB - 1.D-15
                  Speed(Jpnt+j-1) = (Flux(Jpnt+j-1)-FLUXB)
     +                              /(Theta(Jpnt+j-1)-THETAB)
                ELSE
                  comp1 = ABS(Theta(Jpnt+j-1)-Theta(Jpnt+j-3))
                  comp2 = ABS(Flux(Jpnt+j-1)-Flux(Jpnt+j-3))
                  IF ( comp1.LT.1.0E-9 ) Theta(Jpnt+j-1)
     +                 = Theta(Jpnt+j-3) - 1.0D-9
                  IF ( comp2.LT.1.0E-15 ) Flux(Jpnt+j-1)
     +                 = Flux(Jpnt+j-3) - 1.0D-15
                  Speed(Jpnt+j-1) = (Flux(Jpnt+j-1)-Flux(Jpnt+j-3))/
     +                              (Theta(Jpnt+j-1)-Theta(Jpnt+j-3))
                END IF
                iflag2 = 0
                k = j - 1
                idif = 0
                DO WHILE ( iflag2.EQ.0 )
                  IF ( Itrwave(Jpnt+k-1).GT.0 ) THEN
                    iflag2 = 1
                    idif = j - k
                    IF ( idif.EQ.Itrwave(Jpnt+k-1) ) THEN
                      Itrwave(Jpnt+k-1) = Itrwave(Jpnt+k-1) - 1
                      IF ( Theta(Jpnt+j-1).LE.Theta(Jpnt+j-3) ) THEN
                        Ltrail(Jpnt+j-1) = 1
                        fhold = (Theta(Jpnt+j-1)-Thetar)/(Thetas-Thetar)
                        IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
                        Speed(Jpnt+j-1) = ((Eps*Fksat)/(Thetas-Thetar))
     +                                    *fhold**eps_m1
                      END IF
                    END IF
                  ELSE
                    k = k - 1
                    IF ( k.EQ.0 ) iflag2 = 1
                  END IF
                END DO
              END IF
              DO k = l, Numwaves
                jpntpkm1 = Jpnt + k - 1
                jpntpkm2 = jpntpkm1 - 1
                Flux(jpntpkm2) = Flux(jpntpkm1)
                Theta(jpntpkm2) = Theta(jpntpkm1)
                Speed(jpntpkm2) = Speed(jpntpkm1)
                Depth(jpntpkm2) = Depth(jpntpkm1)
                Itrwave(jpntpkm2) = Itrwave(jpntpkm1)
                Ltrail(jpntpkm2) = Ltrail(jpntpkm1)
              END DO
              l = Numwaves + 1
              iremove = iremove + 1
            ELSE IF ( Itrwave(Jpnt+j-1).GT.0 ) THEN
              j = j + Itrwave(Jpnt+j-1) - 1
            END IF
            j = j + 1
            IF ( j.GT.Numwaves ) iflag = 1
          END DO
          timenew = timenew + shortest
C
C15-----CALCULATE TOTAL FLUX TO WATER TABLE DURING REMAINING TIME IN
C         STEP.
        ELSE
          j = 2
          DO WHILE ( j.LE.Numwaves )
            IF ( Itrwave(Jpnt+j-1).EQ.0 ) THEN
              Depth(Jpnt+j-1) = Depth(Jpnt+j-1) + Speed(Jpnt+j-1)
     +                          *(Deltinc-Time)
            ELSE
C
C16-----ROUTE TRAIL WAVES.
              DO k = j, j + Itrwave(Jpnt+j-1) - 1
Cdep check to see if theta = thetar do not route?
                IF(Theta(Jpnt+j-2)-Thetar.GT.CLOSEZERO) THEN
                  Depth(Jpnt+k-1) = Depth(Jpnt+j-2)*((f7*Theta(Jpnt+k-1)
     +                              +f8*Theta(Jpnt+k-2)-Thetar)
     +                              /(Theta(Jpnt+j-2)-Thetar))**eps_m1
                END IF
              END DO
              j = j + Itrwave(Jpnt+j-1) - 1
            END IF
            j = j + 1
          END DO
          timenew = Deltinc
        END IF
        Totalflux = Totalflux + FLUXHLD2*(timenew-Time)
        IF ( iflx.EQ.1 ) THEN
          FLUXHLD2 = Flux(Jpnt)
          iflx = 0
        END IF
C17-----REMOVE ARRAY ELEMENTS RESULTING FROM INTERCEPTED WAVES.
        Numwaves = Numwaves - iremove
        Time = timenew
        diff = Deltinc - Time
        IF ( Numwaves.EQ.1 ) Itester = 1
      END DO
C18-----RETURN.
      RETURN
      END SUBROUTINE LEADWAVE
C
C-------SUBROUTINE TRAILWAVE
      SUBROUTINE TRAILWAVE(Numwaves, I, Flux, Theta, Speed, Depth, 
     +                     Itrwave, Ltrail, Fksat, Eps, Thetas, Thetar, 
     +                     Surflux, Jpnt)
C     ******************************************************************
C     INITIALIZE A NEW SET OF TRAIL WAVES WHEN SURFACE FLUX DECREASES.
!--------REVISED FOR MODFLOW-2005 RELEASE 1.9, FEBRUARY 6, 2012
C     ******************************************************************
      USE GWFSFRMODULE, ONLY: NSTOTRL, NSTRAIL, NSFRSETS, NEARZERO,
     +                        FLUXHLD2, FLUXB, THETAB
      USE GLOBAL,       ONLY: IOUT
      IMPLICIT NONE
      INTRINSIC FLOAT
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL Fksat
      INTEGER Numwaves, I, Jpnt, Itrwave(NSTOTRL), Ltrail(NSTOTRL)
      DOUBLE PRECISION Speed(NSTOTRL), Flux(NSTOTRL), Depth(NSTOTRL), 
     +                 Theta(NSTOTRL), Surflux, Thetar, Thetas, Eps
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      DOUBLE PRECISION smoist, smoistinc, ftrail, fhold, eps_m1
      REAL fnuminc
      INTEGER j, jj, jk, kk, numtrail2, jpnwavesm1, jpnwavesm2, jpntpjm1
C     ------------------------------------------------------------------
      eps_m1 = Eps - 1.0D0
      THETAB = Theta(Jpnt)
      FLUXB = Flux(Jpnt)
      numtrail2 = NSTRAIL
      jpnwavesm1 = Jpnt + Numwaves - 1
      jpnwavesm2 = jpnwavesm1 - 1
C1------INITIALIZE TRAIL WAVES WHEN SURFACE FLUX DECREASES.
      kk = 1
      FLUXHLD2 = Flux(Jpnt)
      IF ( Surflux.LT.NEARZERO ) Surflux = 0.0D0
      smoist = (((Surflux/Fksat)**(1.0D0/Eps))*(Thetas-Thetar)) + Thetar
      IF ( Theta(jpnwavesm2)-smoist.GT.1.0D-6 ) THEN
        fnuminc = 0.0
        DO jk = 1, NSTRAIL
          fnuminc = fnuminc + FLOAT(jk)
        END DO
        smoistinc = (Theta(jpnwavesm2)-smoist)/(fnuminc-1.0)
        jj = NSTRAIL
        ftrail = NSTRAIL + 1
        DO j = Numwaves, Numwaves + numtrail2 - 1
          IF ( j.GT.NSTOTRL ) THEN
            WRITE (*, *) 'TOO MANY WAVES IN UNSAT CELL', I, Numwaves, 
     +                   '   PROGRAM TERMINATED IN TRAILWAVE SFR2'
            WRITE (IOUT, *) 'TOO MANY WAVES IN UNSAT CELL', I, Numwaves,
     +      '   PROGRAM TERMINATED IN TRAILWAVE SFR2; INCREASE NSFRSETS'
            STOP
          END IF
          jpntpjm1 = Jpnt + j - 1
          Ltrail(jpntpjm1) = 0
          Itrwave(jpntpjm1) = 0
          IF ( j.GT.Numwaves ) THEN
            Theta(jpntpjm1) = Theta(Jpnt+j-2)
     +                        - ((ftrail-FLOAT(jj))*smoistinc)
          ELSE
            Theta(jpntpjm1) = Theta(Jpnt+j-2) - 1.0D-8
          END IF
          jj = jj - 1
          IF ( Theta(jpntpjm1).LE.Thetar+1.0D-6 ) Theta(jpntpjm1)
     +         = Thetar + 1.0D-6
          Flux(jpntpjm1) = Fksat*((Theta(jpntpjm1)-Thetar)
     +                     /(Thetas-Thetar))**Eps
          IF ( j.EQ.Numwaves ) THEN
            fhold = (Theta(jpntpjm1)-Thetar)/(Thetas-Thetar)
            IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
            Speed(jpntpjm1) = ((Eps*Fksat)/(Thetas-Thetar))*fhold
     +                        **eps_m1
          ELSE
            Speed(jpntpjm1) = 0.0D0
          END IF
          kk = kk + 1
          Depth(jpntpjm1) = 0.0D0
        END DO
        Itrwave(Jpnt+Numwaves) = numtrail2 - 1
        Ltrail(jpnwavesm1) = 1
        Numwaves = Numwaves + numtrail2 - 1
        IF ( Numwaves.GT.NSFRSETS*NSTRAIL ) THEN
            WRITE (*, *) 'TOO MANY WAVES IN STREAM CELL', I, Numwaves, 
     +                   '   PROGRAM TERMINATED IN UZFLOW-4'
            WRITE (IOUT, *)'TOO MANY WAVES IN STREAM CELL', I, Numwaves,
     +            '   PROGRAM TERMINATED IN UZFLOW-4; INCREASE NSFRSETS'
            STOP
        END IF
      ELSE
        Itrwave(jpnwavesm1) = 0
        Ltrail(jpnwavesm1) = 1
        Theta(jpnwavesm1) = Theta(jpnwavesm2)
        fhold = (Theta(jpnwavesm1)-Thetar)/(Thetas-Thetar)
        Depth(jpnwavesm1) = 0.0D0
        IF ( fhold.LT.NEARZERO ) fhold = 0.0D0
        Speed(jpnwavesm1) = ((Eps*Fksat)/(Thetas-Thetar))*fhold**eps_m1
        Flux(jpnwavesm1) = Fksat*((Theta(jpnwavesm1)-Thetar)
     +                     /(Thetas-Thetar))**Eps
      END IF
C
C2------RETURN.
      RETURN
      END SUBROUTINE TRAILWAVE
C
C-------SUBROUTINE CHANNELAREA
      SUBROUTINE CHANNELAREA(Istsg, L)
C     ******************************************************************
C     COMPARTMENTALIZE UNSATURATED ZONE BENEATH STREAMBED ON BASIS OF 
C     EIGHT POINT CROSS SECTION WHEN ICALC IS 2
!--------REVISED FOR MODFLOW-2005 RELEASE 1.9, FEBRUARY 6, 2012
C     ******************************************************************
      USE GWFSFRMODULE, ONLY: NUZST, ISUZN, XSEC, WETPER
      IMPLICIT NONE
      INTRINSIC ABS, SQRT
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER Istsg, L
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      REAL area, area1, b, chap, ffmax, ffmin, finc, fmax, fmin, slope, 
     +     stage, wethold, wetted, xinc, xmid, xx, y1, y2, yy
      INTEGER i, ii, j, k, ll, mark(20)
      DOUBLE PRECISION dpthinc
      ALLOCATABLE dpthinc(:,:)
      ALLOCATE (dpthinc(ISUZN,NUZST))
C     ------------------------------------------------------------------
      area = 0.0
      area1 = 0.0
      wethold = 0.0
      wetted = 0.0
C
C1------CALCULATE THE MAXIMUM AND MINIMUM CHANNEL ELEVATIONS.
C
      fmin = XSEC(9, Istsg)
      fmax = XSEC(9, Istsg)
      DO i = 2, 8
        IF ( XSEC(8+i, Istsg).LT.fmin ) fmin = XSEC(8+i, Istsg)
        IF ( XSEC(8+i, Istsg).GT.fmax ) fmax = XSEC(8+i, Istsg)
      END DO
      finc = (fmax-fmin)/ISUZN
      DO i = 1, ISUZN
        dpthinc(i, L) = i*finc
      END DO
C
C2------CALCULATE WETTED PERIMETERS FOR INCREMENTAL RIVER DEPTHS.
      DO i = 1, ISUZN
        stage = dpthinc(i, L)
        area = 0.0
        wetted = 0.0
C
C3------DETERMINE POINTS THAT ARE BELOW STREAM STAGE.
        k = 0
        DO j = 2, 8
          IF ( XSEC(8+j, Istsg).LT.stage .OR. XSEC(8+j-1, Istsg)
     +         .LT.stage ) THEN
            k = k + 1
            mark(k) = j
          END IF
        END DO
C
C4------BREAK CHANNEL UP INTO A SERIES OF LINES BETWEEN POINTS
C       AND CALCULATE EQUATION OF EACH LINE.
        DO ll = 1, k
          chap = (XSEC(8+mark(ll)-1, Istsg)-XSEC(8+mark(ll), Istsg))
          IF ( ABS(XSEC(8+mark(ll), Istsg)-XSEC(8+mark(ll)-1, Istsg))
     +         .LT.1.0E-30.AND.ABS(XSEC(mark(ll), Istsg)-
     +         XSEC(mark(ll)-1, Istsg)).LT.1.0E-30 )THEN
            WRITE (*, *) 'two cross-section points are identical,', 
     +                   ' check input. Segment number ', Istsg
            slope = 0.0 
          ELSE IF ( ABS(XSEC(8+mark(ll), Istsg)-
     +              XSEC(8+mark(ll)-1, Istsg)).LT.1.0E-30) THEN
            slope = 1.0E-5
          ELSE IF ( ABS(XSEC(mark(ll), Istsg)-XSEC(mark(ll)-1, Istsg))
     +              .LT.1.0E-30 )THEN
            slope = 10.0
          ELSE
            slope = (XSEC(8+mark(ll), Istsg)-XSEC(8+mark(ll)-1, Istsg))
     +              /(XSEC(mark(ll), Istsg)-XSEC(mark(ll)-1, Istsg))
          END IF

          ffmin = XSEC(8+mark(ll), Istsg)
          ffmax = XSEC(8+mark(ll)-1, Istsg)
          IF ( ffmin.GT.ffmax ) THEN
            ffmin = XSEC(8+mark(ll)-1, Istsg)
            ffmax = XSEC(8+mark(ll), Istsg)
          END IF
          b = XSEC(8+mark(ll)-1, Istsg) - slope*XSEC(mark(ll)-1, Istsg)
C
C5------WETTED AREA ASSUMING A FLAT CHANNEL BOTTOM.
          IF ( ABS(chap).LT.1.0E-30 ) THEN
            area1 = (XSEC(mark(ll), Istsg)-XSEC(mark(ll)-1, Istsg))
     +              *(stage-ffmin)
          ELSE
C
C6------DETERMINE IF STREAM STAGE IS BETWEEN POINTS.
            IF ( stage.GT.ffmax ) THEN
              xinc = (XSEC(mark(ll), Istsg)-XSEC(mark(ll)-1, Istsg))/50.
              xmid = XSEC(mark(ll)-1, Istsg)
            ELSE
              ffmax = stage
              xmid = (stage-b)/slope
C
C7------MOVE DOWN THE CHANNEL BANK OR UP OTHER SIDE.
              IF ( XSEC(8+mark(ll)-1, Istsg).LT.XSEC(8+mark(ll), Istsg)
     +             ) THEN
                xinc = (ABS(XSEC(mark(ll)-1,Istsg)-xmid))/50.
                xmid = XSEC(mark(ll)-1, Istsg)
              ELSE
                xinc = (ABS(XSEC(mark(ll),Istsg)-xmid))/50.
              END IF
            END IF

C8------CALCULATE WETTED PERIMETER.
            xx = ABS(xmid-XSEC(mark(ll), Istsg))
            yy = ABS(ffmax-ffmin)
            wetted = wetted + SQRT((xx**2)+(yy**2))
C9------BREAK AREA UP INTO TRAPEZOIDS.
            DO ii = 1, 50
              y1 = slope*xmid + b
              y2 = slope*(xmid+xinc) + b
              area = area + (((stage-y1)+(stage-y2))/2)*xinc
              xmid = xmid + xinc
            END DO
          END IF
        END DO
        area = area + area1
        IF ( i.EQ.1 ) THEN
          WETPER(i, L) = wetted
        ELSE
          WETPER(i, L) = (wetted-wethold)
        END IF
        wethold = wetted
      END DO
      DEALLOCATE (dpthinc)
C
C10-----RETURN.
      RETURN
      END SUBROUTINE CHANNELAREA
C
C-------SUBROUTINE ROUTE_CHAN
      SUBROUTINE ROUTE_CHAN(Qa, Qb, Qc, Qd, Qcnst, Cdpth, Awdth, Fdpth,
     +                      Bwdth, Deltinc, Icalc, Strlen, Slope, Istsg,
     +                      Nreach, Itstr, Qlat, Flobot, Width, L, 
     +                      Chanstor,depth)
C***********************************************************************
C     IMPLICIT FINITE-DIFFERENCE SCHEME TO ROUTE FLOW DOWN CHANNELS 
!--------REVISED FOR MODFLOW-2005 RELEASE 1.9, FEBRUARY 6, 2012
C***********************************************************************
      USE GWFSFRMODULE, ONLY: ISEG, WEIGHT, SEG, FLWTOL, NEARZERO
      USE GLOBAL,       ONLY: IOUT 
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER Icalc, Istsg, Nreach, L, Itstr
      DOUBLE PRECISION Flobot, Slope, Cdpth, Fdpth, Width, Qa, Qb, Qc,
     +                 Qd, Awdth, Bwdth
      REAL Qcnst, Strlen, Deltinc, Qlat, Chanstor
      INTRINSIC ABS, DABS
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      INTEGER i, maxiter, iprndpth
      REAL w_1, stor
      DOUBLE PRECISION qderiv, dq, delq, flobot2, tol, ab, ac, aa, ad1
      DOUBLE PRECISION wetperm, depth, qd2, qd3, f11, f12, f1, f2, ad2
C     ------------------------------------------------------------------
      iprndpth = 0
      maxiter = 200
      w_1 = 1.0 - WEIGHT
      dq = FLWTOL/10.0
      tol = FLWTOL
      ad1=0.0
      ad2=0.0 
      aa=0.0 
      ab=0.0
      ac=0.0
      IF ( dq.LT.NEARZERO ) dq = NEARZERO
      IF ( tol.LT.NEARZERO ) tol = NEARZERO
      i = 1
      IF ( Flobot.GT.Qc+Qlat*Strlen ) THEN
        Flobot  = Qc+Qlat*Strlen
      END IF
C
C1------CHANGE SIGN OF FLOBOT BECAUSE IT IS A DISCHARGE FROM GROUND 
C         WATER AND IS STORED AS A NEGATIVE VALUE.
      flobot2 = -Flobot/Strlen
C
C2------MAKE AN INITIAL GUESS AT Qd.
      Qd = (Qc+Qb)/2.0
C
C3------INITIALIZE CONSTANTS.
      delq = 0.0D0
      IF ( Icalc.EQ.1 ) THEN
        IF ( Qcnst.GT.0 ) THEN
          aa = Width*(Qa/Qcnst)**0.6D0
          ab = Width*(Qb/Qcnst)**0.6D0
          ac = Width*(Qc/Qcnst)**0.6D0
        END IF
      ELSE IF ( Icalc.EQ.2 ) THEN
        CALL GWF2SFR7DPTH(Qa, Slope, Istsg, Nreach, SEG(16,Istsg),
     +                    SEG(17,Istsg), wetperm, depth, Itstr, Width,
     +                    iprndpth)
        aa = depth*Width
        CALL GWF2SFR7DPTH(Qb, Slope, Istsg, Nreach, SEG(16,Istsg),
     +                    SEG(17,Istsg), wetperm, depth, Itstr, Width,
     +                    iprndpth)
        ab = depth*Width
        CALL GWF2SFR7DPTH(Qc, Slope, Istsg, Nreach, SEG(16,Istsg),
     +                        SEG(17,Istsg), wetperm, depth, Itstr, 
     +                        Width, iprndpth)
          ac = depth*Width
      ELSE IF ( Icalc.EQ.3 ) THEN
        depth = Cdpth*(Qa**Fdpth)
        Width = Awdth*(Qa**Bwdth)
        aa = depth*Width
        depth = Cdpth*(Qb**Fdpth)
        Width = Awdth*(Qb**Bwdth)
        ab = depth*Width
        depth = Cdpth*(Qc**Fdpth)
        Width = Awdth*(Qc**Bwdth)
        ac = depth*Width
      ELSE IF ( Icalc.EQ.4 ) THEN
        CALL GWF2SFR7TBD(Qa, depth, Width, ISEG(2,Istsg), Istsg)
        aa = depth*Width
        CALL GWF2SFR7TBD(Qb, depth, Width, ISEG(2,Istsg), Istsg)
        ab = depth*Width
        CALL GWF2SFR7TBD(Qc, depth, Width, ISEG(2,Istsg), Istsg)
        ac = depth*Width
      END IF
C
C4------CALCULATE FLOW IN CHANNELS--MAXIMUM ITERATIONS IS 50.
      CONVERGE: DO WHILE( i.LT.maxiter )
C
C5------LOOP THROUGH UNTIL delq LESS THAN TOLERANCE. 
        IF ( DABS(delq).GT.tol .OR. i.EQ.1 ) THEN
          qd2 = Qd + dq
          qd3 = Qd + 2.0*dq
C
C6------CALCULATE VARIABLES ad1 and ad2 ON BASIS OF Icalc.
          IF ( Icalc.EQ.1 ) THEN
            IF ( Qcnst.GT.0 ) THEN
              depth = (Qd/Qcnst)**0.6D0
              ad1 = depth*Width
              depth = (qd2/Qcnst)**0.6D0
              ad2 = depth*Width
            ELSE
              depth = 0.0
              ad1 = 0.0
              ad2 = 0.0
            END IF
          ELSE IF ( Icalc.EQ.2 ) THEN
            CALL GWF2SFR7DPTH(Qd, Slope, Istsg, Nreach, SEG(16,Istsg),
     +                        SEG(17,Istsg), wetperm, depth, Itstr,
     +                        Width, iprndpth)
            ad1 = depth*Width
            CALL GWF2SFR7DPTH(qd2, Slope, Istsg, Nreach, SEG(16,Istsg),
     +                        SEG(17,Istsg), wetperm, depth, Itstr,
     +                        Width, iprndpth)
            ad2 = depth*Width
          ELSE IF ( Icalc.EQ.3 ) THEN
            depth = Cdpth*(Qd**Fdpth)
            Width = Awdth*(Qd**Bwdth)
            ad1 = depth*Width
            depth = Cdpth*(qd2**Fdpth)
            Width = Awdth*(qd2**Bwdth)
            ad2 = depth*Width
            depth = Cdpth*(qd3**Fdpth)
            Width = Awdth*(qd3**Bwdth)
          ELSE IF ( Icalc.EQ.4 ) THEN
            CALL GWF2SFR7TBD(Qd, depth, Width, ISEG(2,Istsg), Istsg)
            ad1 = depth*Width
            CALL GWF2SFR7TBD(qd2, depth, Width, ISEG(2,Istsg), Istsg)
            ad2 = depth*Width
            CALL GWF2SFR7TBD(qd3, depth, Width, ISEG(2,Istsg), Istsg)
          END IF
C
C7------CALCULATE FLOW.
          IF ( (Qb+Qa).LT.NEARZERO ) THEN
            f11 = (Qd-Qc)/Strlen
          ELSE
            f11 = (WEIGHT*((Qd)-Qc)+(w_1)*(Qb-Qa))/Strlen
          END IF
          f12 = ((ad1-ab)+(ac-aa))/(2.0D0*Deltinc)
          f1 = f11+f12-Qlat-flobot2
          IF ( (Qb+Qa).LT.NEARZERO ) THEN
            f11 = (Qd+dq-Qc)/Strlen
          ELSE
            f11 = (WEIGHT*((Qd+dq)-Qc)+(w_1)*(Qb-Qa))/Strlen
          END IF
          f12 = ((ad2-ab)+(ac-aa))/(2.0D0*Deltinc)
          f2 = f11+f12-Qlat-flobot2
          qderiv = (f2-f1)/dq
          IF ( qderiv.GT.0.0 ) THEN
            delq = -f1/qderiv
          ELSE
            delq = 0.0
          END IF
          IF ( (Qd + delq).LT.NEARZERO ) THEN
            delq = -Qd
            Qd = 0.0D0
          ELSE
            Qd = Qd + delq
          END IF
C
C8------EXIT LOOP IF delq LESS THAN TOLERANCE.
        ELSE
          EXIT CONVERGE
        END IF
          i = i + 1
      END DO CONVERGE
      stor = Strlen*(ad2+ac)/(2.0D0*Deltinc) - 
     +       Strlen*(ab+aa)/(2.0D0*Deltinc)
!      Chanstor = Qc - Qd - stor - FLOBOT
      Chanstor = stor
      IF ( i.GE.maxiter ) WRITE(IOUT,*) 'Non-convergence in ROUTE_CHAN',
     +                                  L, delq
      IF ( Qd.LT.tol ) tol = 0.0D0
C
C9------RETURN.
      RETURN
      END SUBROUTINE ROUTE_CHAN
C
C
      FUNCTION CALC_XSA(Qa,Qcnst,Cdpth,Awdth,Fdpth,Bwdth,Icalc,
     +                  Slope,Istsg,Nreach,Itstr,Width,depth)
C***********************************************************************
C     CALCULATE CHANNEL CROSS-SECTIONAL AREA FOR USE BY LMT
!--------EDM
C***********************************************************************
      USE GWFSFRMODULE, ONLY: ISEG, SEG, NEARZERO
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER Icalc, Istsg, Nreach, Itstr
      DOUBLE PRECISION Slope, Cdpth, Fdpth, Width, Qa,
     +                 Awdth, Bwdth
      REAL Qcnst
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      INTEGER maxiter, iprndpth
      DOUBLE PRECISION aa, wetperm, depth, CALC_XSA
C     ------------------------------------------------------------------
      iprndpth = 0
      maxiter = 200
      aa=0.0
C
C3------INITIALIZE CONSTANTS.
      IF ( Icalc.EQ.1 ) THEN
        IF ( Qcnst.GT.0 ) THEN
          aa = Width*(Qa/Qcnst)**0.6D0
        END IF
      ELSE IF ( Icalc.EQ.2 ) THEN
        CALL GWF2SFR7DPTH(Qa, Slope, Istsg, Nreach, SEG(16,Istsg),
     +                    SEG(17,Istsg), wetperm, depth, Itstr, Width,
     +                    iprndpth)
        aa = depth*Width
      ELSE IF ( Icalc.EQ.3 ) THEN
        depth = Cdpth*(Qa**Fdpth)
        Width = Awdth*(Qa**Bwdth)
        aa = depth*Width
      ELSE IF ( Icalc.EQ.4 ) THEN
        CALL GWF2SFR7TBD(Qa, depth, Width, ISEG(2,Istsg), Istsg)
        aa = depth*Width
      END IF
      CALC_XSA = aa
C
      RETURN
      END FUNCTION CALC_XSA
C
C
!     -----------------------------------------------------------------
      SUBROUTINE GWF2SFR7AD(Iunitlak)
C     ******************************************************************
C     DETERMINE SPECIFIED INFLOWS FOR TIME STEP BASED ON TABULAR VALUES
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFBASMODULE, ONLY: TOTIM
      USE GWFSFRMODULE, ONLY: NSS, TABFLOW, TABTIME, NUMTAB, ISFRLIST,
     +                        SEG, FXLKOT, IDIVAR, CLOSEZERO
!!      USE GWFSFRMODULE, ONLY: NSS, TABFLOW, TABTIME, NUMTAB, ISFRLIST,
!!     +                        SEG, FXLKOT, IDIVAR, CLOSEZERO
      USE GLOBAL, ONLY: IOUT
      IMPLICIT NONE
      EXTERNAL FLOWTERP
      REAL FLOWTERP
      INTEGER i, iseg, Iunitlak, istsg, lk
C     ------------------------------------------------------------------
C
C1------CALL LINEAR INTERPOLATION ROUTINE
      IF ( NUMTAB.GT.0 ) THEN
        DO i = 1, NUMTAB
          iseg = ISFRLIST(1,i)
          SEG(2,iseg) = FLOWTERP(TOTIM,i)  
        END DO
      END IF 
C
C DEP moved the from SFR7FM
C DEP FXLKOT is now adjusted in LAK7FM for limiting to available lake water.
C
C2------COMPUTE INFLOW OF A STREAM SEGMENT EMANATING FROM A LAKE.
      DO istsg = 1, NSS
        IF ( (Iunitlak.GT.0) .AND. (IDIVAR(1,istsg).LT.0) ) THEN
          IF ( SEG(2,istsg).GT.CLOSEZERO ) THEN
            lk = IABS(IDIVAR(1, istsg))
C3------CHECK IF LAKE OUTFLOW IS SPECIFIED AT A FIXED RATE.
             FXLKOT(istsg) = SEG(2, istsg)
           ELSE IF ( SEG(2, istsg).LE.-CLOSEZERO ) THEN
             WRITE (IOUT, 9001) istsg
 9001        FORMAT (/5X, '*** WARNING *** NEGATIVE LAKE OUTFLOW ',
     +                  'NOT ALLOWED; SEG = ', I6, /10X, 
     +                  'CODE WILL ASSUME FLOW = 0.0'/)
             SEG(2, istsg) = 0.0
             FXLKOT(istsg) = 0.0
           END IF
         END IF
       END DO
      RETURN
      END
C
C     ------------------------------------------------------------------
C
      REAL FUNCTION FLOWTERP (TIME,INUM)
C     FUNCTION LINEARLY INTERPOLATES BETWEEN TWO VALUES
C     OF TIME TO CALCULATE SPECIFIED INFLOW TO SEGMENTS.
      USE GWFSFRMODULE, ONLY: TABFLOW, TABTIME, ISFRLIST,
     +                        CLOSEZERO
!!      USE GWFSFRMODULE, ONLY: TABFLOW, TABTIME, NUMTAB, ISFRLIST,
!!     +                        CLOSEZERO
      USE GWFBASMODULE, ONLY: DELT
      IMPLICIT NONE
      REAL TIME, FLOW, TIMEBEG, TIMEND, TIMESTART, SUMFLOW, TOLF2
      INTEGER INUM, IEND, ISTM1, ISTART, iflg, ISEG, NVAL, I
      TOLF2=1.0E-4
      FLOW = 0.0
      NVAL = ISFRLIST(2,INUM)
      ISEG = ISFRLIST(1,INUM)
      IFLG = 0
      SUMFLOW = 0.0
      I = 1
      TIMEBEG = TIME - DELT
      IF ( TIMEBEG-TABTIME(1,ISEG).LT.0.0 ) THEN
        FLOWTERP = TABFLOW(1,ISEG)
      ELSEIF ( TIMEBEG-TABTIME(NVAL,ISEG).GT.0.0 ) THEN
        FLOWTERP = TABFLOW(NVAL,ISEG)
      ELSE
! Find table value before beginning of time step.
        DO WHILE ( I.LE.NVAL-1 )
          IF ( TIMEBEG-TABTIME(I,ISEG).LE.CLOSEZERO ) THEN
            EXIT
          ELSEIF ( TIMEBEG-TABTIME(I+1,ISEG).LE.CLOSEZERO ) THEN
            EXIT
          ELSE
            I = I + 1
          END IF
        END DO
        ISTART = I
        ISTM1 = I
        IF ( I.GT.1 ) ISTM1 = ISTM1 - 1
! Find table value after end of time step
        DO WHILE ( I.LE.NVAL ) 
          IF ( TIME-TABTIME(I,ISEG).LE.0.0 ) THEN
            EXIT
          ELSE
            I = I + 1
          END IF
        END DO
        IEND = I
        IF ( IEND.GT.NVAL ) IEND = NVAL
        DO I = ISTART, IEND - 1
          TIMESTART = TABTIME(I,ISEG)
          TIMEND = TABTIME(I+1,ISEG)
          IF ( TIMEBEG-TIMESTART.GT.0.0 ) TIMESTART = TIMEBEG
          IF ( TIME-TIMEND.LT.0.0 ) TIMEND = TIME
          SUMFLOW = SUMFLOW + (TIMEND-TIMESTART)*TABFLOW(I,ISEG)
        END DO
        FLOWTERP = SUMFLOW/DELT
      END IF
      RETURN
      END FUNCTION FLOWTERP
C
C------FUNCTION FXLKOT_TERP FOR SMOOTHING STREAM WIDTH DURING CHANNEL DRYING.
C
      DOUBLE PRECISION FUNCTION smooth(h,dwdh)
! h is the depth 
! dwdh is the derivative of width with respect to depth
      IMPLICIT NONE
      DOUBLE PRECISION h, s, aa, ad, b, x, y, dwdh
      smooth = 0.0D0
      s = 1.0d-5
      x = h
      IF ( x-s.GT.0.0 ) THEN
        smooth = 1.0
        dwdh = 0.0D0
        RETURN
      END IF
      aa = -1.0d0/(s**2.0d0)
      ad = -2.0D0/(s**2.0d0)
      b = 2.0d0/s
      y = aa*x**2.0d0 + b*x
      dwdh = (ad*x + b)
      IF ( x.LE.0.0 ) THEN
        y = 0.0D0
        dwdh = 0.0D0
      ELSE IF ( x-s.GT.-1.0e-14 ) THEN
        y = 1.0D0
        dwdh = 0.0D0
      END IF
      smooth = y
      END FUNCTION smooth
C
C-------SUBROUTINE GWF2SFR7DA
      SUBROUTINE GWF2SFR7DA(IGRID)
C  Save SFR data for a grid.
      USE GWFSFRMODULE
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER IGRID
C     ------------------------------------------------------------------
      DEALLOCATE (GWFSFRDAT(IGRID)%NSS)
      DEALLOCATE (GWFSFRDAT(IGRID)%NSTRM)
      DEALLOCATE (GWFSFRDAT(IGRID)%NSFRPAR)
      DEALLOCATE (GWFSFRDAT(IGRID)%ISTCB1)
      DEALLOCATE (GWFSFRDAT(IGRID)%ISTCB2)
      DEALLOCATE (GWFSFRDAT(IGRID)%IUZT)
      DEALLOCATE (GWFSFRDAT(IGRID)%MAXPTS)
      DEALLOCATE (GWFSFRDAT(IGRID)%ISFROPT)
      DEALLOCATE (GWFSFRDAT(IGRID)%NSTRAIL)
      DEALLOCATE (GWFSFRDAT(IGRID)%ISUZN)
      DEALLOCATE (GWFSFRDAT(IGRID)%NSFRSETS)
      DEALLOCATE (GWFSFRDAT(IGRID)%NUZST)
      DEALLOCATE (GWFSFRDAT(IGRID)%NSTOTRL)
      DEALLOCATE (GWFSFRDAT(IGRID)%NUMAVE)
      DEALLOCATE (GWFSFRDAT(IGRID)%ITMP)
      DEALLOCATE (GWFSFRDAT(IGRID)%IRDFLG)
      DEALLOCATE (GWFSFRDAT(IGRID)%IPTFLG)
      DEALLOCATE (GWFSFRDAT(IGRID)%NUMTIM)
      DEALLOCATE (GWFSFRDAT(IGRID)%WEIGHT)
      DEALLOCATE (GWFSFRDAT(IGRID)%SFRRATIN)
      DEALLOCATE (GWFSFRDAT(IGRID)%SFRRATOUT)
      DEALLOCATE (GWFSFRDAT(IGRID)%FLWTOL)
      DEALLOCATE (GWFSFRDAT(IGRID)%IRTFLG)
      DEALLOCATE (GWFSFRDAT(IGRID)%NP)
      DEALLOCATE (GWFSFRDAT(IGRID)%CONST)
      DEALLOCATE (GWFSFRDAT(IGRID)%DLEAK)
      DEALLOCATE (GWFSFRDAT(IGRID)%IOTSG)
      !DEALLOCATE (GWFSFRDAT(IGRID)%DVRCH)     !cjm
      !DEALLOCATE (GWFSFRDAT(IGRID)%DVEFF)     !cjm
      !DEALLOCATE (GWFSFRDAT(IGRID)%DVRCELL)   !cjm
       DEALLOCATE (GWFSFRDAT(IGRID)%RECHSAVE)  !cjm
      !DEALLOCATE (GWFSFRDAT(IGRID)%DVRPERC)  !cjm
      !DEALLOCATE (GWFSFRDAT(IGRID)%IDVFLG)  !cjm
      DEALLOCATE (GWFSFRDAT(IGRID)%NSEGCK)
      DEALLOCATE (GWFSFRDAT(IGRID)%ITRLSTH)
      DEALLOCATE (GWFSFRDAT(IGRID)%ISEG)
      DEALLOCATE (GWFSFRDAT(IGRID)%IDIVAR)
      DEALLOCATE (GWFSFRDAT(IGRID)%ISTRM)
      DEALLOCATE (GWFSFRDAT(IGRID)%LTRLIT)
      DEALLOCATE (GWFSFRDAT(IGRID)%LTRLST)
      DEALLOCATE (GWFSFRDAT(IGRID)%ITRLIT)
      DEALLOCATE (GWFSFRDAT(IGRID)%ITRLST)
      DEALLOCATE (GWFSFRDAT(IGRID)%NWAVST)
      DEALLOCATE (GWFSFRDAT(IGRID)%STRIN)
      DEALLOCATE (GWFSFRDAT(IGRID)%STROUT)
      DEALLOCATE (GWFSFRDAT(IGRID)%FXLKOT)
      DEALLOCATE (GWFSFRDAT(IGRID)%UHC)
      DEALLOCATE (GWFSFRDAT(IGRID)%SGOTFLW)
      DEALLOCATE (GWFSFRDAT(IGRID)%DVRSFLW)
      DEALLOCATE (GWFSFRDAT(IGRID)%SFRUZBD)
      DEALLOCATE (GWFSFRDAT(IGRID)%SEG)
      DEALLOCATE (GWFSFRDAT(IGRID)%STRM)
      DEALLOCATE (GWFSFRDAT(IGRID)%HSTRM)
      DEALLOCATE (GWFSFRDAT(IGRID)%QSTRM)
      DEALLOCATE (GWFSFRDAT(IGRID)%SLKOTFLW)
      DEALLOCATE (GWFSFRDAT(IGRID)%DLKOTFLW)
      DEALLOCATE (GWFSFRDAT(IGRID)%DLKSTAGE)
      DEALLOCATE (GWFSFRDAT(IGRID)%HWDTH)
      DEALLOCATE (GWFSFRDAT(IGRID)%HWTPRM)
      DEALLOCATE (GWFSFRDAT(IGRID)%SFRQ)
      DEALLOCATE (GWFSFRDAT(IGRID)%QSTAGE)
      DEALLOCATE (GWFSFRDAT(IGRID)%XSEC)
      DEALLOCATE (GWFSFRDAT(IGRID)%AVDPT)
      DEALLOCATE (GWFSFRDAT(IGRID)%AVWAT)
      DEALLOCATE (GWFSFRDAT(IGRID)%WAT1)
      DEALLOCATE (GWFSFRDAT(IGRID)%CONCQ)
      DEALLOCATE (GWFSFRDAT(IGRID)%CONCRUN)
      DEALLOCATE (GWFSFRDAT(IGRID)%CONCPPT)
      DEALLOCATE (GWFSFRDAT(IGRID)%THTS)
      DEALLOCATE (GWFSFRDAT(IGRID)%THTR)
      DEALLOCATE (GWFSFRDAT(IGRID)%EPS)
      DEALLOCATE (GWFSFRDAT(IGRID)%FOLDFLBT)
      DEALLOCATE (GWFSFRDAT(IGRID)%THTI)
      DEALLOCATE (GWFSFRDAT(IGRID)%UZFLWT)
      DEALLOCATE (GWFSFRDAT(IGRID)%UZSTOR)
      DEALLOCATE (GWFSFRDAT(IGRID)%UZWDTH)
      DEALLOCATE (GWFSFRDAT(IGRID)%UZSEEP)
      DEALLOCATE (GWFSFRDAT(IGRID)%DELSTOR)
      DEALLOCATE (GWFSFRDAT(IGRID)%WETPER)
      DEALLOCATE (GWFSFRDAT(IGRID)%UZDPIT)
      DEALLOCATE (GWFSFRDAT(IGRID)%UZDPST)
      DEALLOCATE (GWFSFRDAT(IGRID)%UZTHIT)
      DEALLOCATE (GWFSFRDAT(IGRID)%UZTHST)
      DEALLOCATE (GWFSFRDAT(IGRID)%UZSPIT)
      DEALLOCATE (GWFSFRDAT(IGRID)%UZSPST)
      DEALLOCATE (GWFSFRDAT(IGRID)%UZFLIT)
      DEALLOCATE (GWFSFRDAT(IGRID)%UZFLST)
      DEALLOCATE (GWFSFRDAT(IGRID)%UZOLSFLX)
      DEALLOCATE (GWFSFRDAT(IGRID)%SUMRCH)
      DEALLOCATE (GWFSFRDAT(IGRID)%SUMLEAK)
      DEALLOCATE (GWFSFRDAT(IGRID)%HLDSFR)
      DEALLOCATE (GWFSFRDAT(IGRID)%STRMDELSTOR_CUM)
      DEALLOCATE (GWFSFRDAT(IGRID)%STRMDELSTOR_RATE)
      DEALLOCATE (GWFSFRDAT(IGRID)%TOTSPFLOW)
      DEALLOCATE (GWFSFRDAT(IGRID)%TABFLOW)
      DEALLOCATE (GWFSFRDAT(IGRID)%TABTIME)
      DEALLOCATE (GWFSFRDAT(IGRID)%ISFRLIST)
      DEALLOCATE (GWFSFRDAT(IGRID)%FNETSEEP)
      DEALLOCATE (GWFSFRDAT(IGRID)%NSEGDIM)
      DEALLOCATE (GWFSFRDAT(IGRID)%factor)
      DEALLOCATE (GWFSFRDAT(IGRID)%NFLOWTYPE)
      DEALLOCATE (GWFSFRDAT(IGRID)%FLOWTYPE)
C
      END SUBROUTINE GWF2SFR7DA
C
C-------SUBROUTINE GWF2SFR7PNT
      SUBROUTINE SGWF2SFR7PNT(IGRID)
C  Change SFR data to a different grid.
      USE GWFSFRMODULE
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER IGRID
C     ------------------------------------------------------------------
      NFLOWTYPE=>GWFSFRDAT(IGRID)%NFLOWTYPE
      FLOWTYPE=>GWFSFRDAT(IGRID)%FLOWTYPE
      NSS=>GWFSFRDAT(IGRID)%NSS
      NSTRM=>GWFSFRDAT(IGRID)%NSTRM
      NSFRPAR=>GWFSFRDAT(IGRID)%NSFRPAR
      ISTCB1=>GWFSFRDAT(IGRID)%ISTCB1
      ISTCB2=>GWFSFRDAT(IGRID)%ISTCB2
      IUZT=>GWFSFRDAT(IGRID)%IUZT
      MAXPTS=>GWFSFRDAT(IGRID)%MAXPTS
      ISFROPT=>GWFSFRDAT(IGRID)%ISFROPT
      NSTRAIL=>GWFSFRDAT(IGRID)%NSTRAIL
      ISUZN=>GWFSFRDAT(IGRID)%ISUZN
      NSFRSETS=>GWFSFRDAT(IGRID)%NSFRSETS
      NUZST=>GWFSFRDAT(IGRID)%NUZST
      NSTOTRL=>GWFSFRDAT(IGRID)%NSTOTRL
      NUMAVE=>GWFSFRDAT(IGRID)%NUMAVE
      ITMP=>GWFSFRDAT(IGRID)%ITMP
      IRDFLG=>GWFSFRDAT(IGRID)%IRDFLG
      IPTFLG=>GWFSFRDAT(IGRID)%IPTFLG
      NP=>GWFSFRDAT(IGRID)%NP
      CONST=>GWFSFRDAT(IGRID)%CONST
      DLEAK=>GWFSFRDAT(IGRID)%DLEAK
      NUMTIM=>GWFSFRDAT(IGRID)%NUMTIM
      WEIGHT=>GWFSFRDAT(IGRID)%WEIGHT
      SFRRATIN=>GWFSFRDAT(IGRID)%SFRRATIN
      SFRRATOUT=>GWFSFRDAT(IGRID)%SFRRATOUT
      FLWTOL=>GWFSFRDAT(IGRID)%FLWTOL
      IRTFLG=>GWFSFRDAT(IGRID)%IRTFLG
      IOTSG=>GWFSFRDAT(IGRID)%IOTSG
      !IDVFLG=>GWFSFRDAT(IGRID)%IDVFLG        !cjm
      !DVRCH=>GWFSFRDAT(IGRID)%DVRCH        !cjm
      !DVEFF=>GWFSFRDAT(IGRID)%DVEFF        !cjm
      !DVRCELL=>GWFSFRDAT(IGRID)%DVRCELL    !cjm
       RECHSAVE=>GWFSFRDAT(IGRID)%RECHSAVE  !cjm
      !DVRPERC=>GWFSFRDAT(IGRID)%DVRPERC  !cjm
      NSEGCK=>GWFSFRDAT(IGRID)%NSEGCK
      ITRLSTH=>GWFSFRDAT(IGRID)%ITRLSTH
      ISEG=>GWFSFRDAT(IGRID)%ISEG
      IDIVAR=>GWFSFRDAT(IGRID)%IDIVAR
      ISTRM=>GWFSFRDAT(IGRID)%ISTRM
      LTRLIT=>GWFSFRDAT(IGRID)%LTRLIT
      LTRLST=>GWFSFRDAT(IGRID)%LTRLST
      ITRLIT=>GWFSFRDAT(IGRID)%ITRLIT
      ITRLST=>GWFSFRDAT(IGRID)%ITRLST
      NWAVST=>GWFSFRDAT(IGRID)%NWAVST
      STRIN=>GWFSFRDAT(IGRID)%STRIN
      STROUT=>GWFSFRDAT(IGRID)%STROUT
      FXLKOT=>GWFSFRDAT(IGRID)%FXLKOT
      UHC=>GWFSFRDAT(IGRID)%UHC
      SGOTFLW=>GWFSFRDAT(IGRID)%SGOTFLW
      DVRSFLW=>GWFSFRDAT(IGRID)%DVRSFLW
      SFRUZBD=>GWFSFRDAT(IGRID)%SFRUZBD
      SEG=>GWFSFRDAT(IGRID)%SEG
      STRM=>GWFSFRDAT(IGRID)%STRM
      HSTRM=>GWFSFRDAT(IGRID)%HSTRM
      QSTRM=>GWFSFRDAT(IGRID)%QSTRM
      HWDTH=>GWFSFRDAT(IGRID)%HWDTH
      HWTPRM=>GWFSFRDAT(IGRID)%HWTPRM
      SFRQ=>GWFSFRDAT(IGRID)%SFRQ
      QSTAGE=>GWFSFRDAT(IGRID)%QSTAGE
      SLKOTFLW=>GWFSFRDAT(IGRID)%SLKOTFLW
      DLKOTFLW=>GWFSFRDAT(IGRID)%DLKOTFLW
      DLKSTAGE=>GWFSFRDAT(IGRID)%DLKSTAGE
      XSEC=>GWFSFRDAT(IGRID)%XSEC
      AVDPT=>GWFSFRDAT(IGRID)%AVDPT
      AVWAT=>GWFSFRDAT(IGRID)%AVWAT
      WAT1=>GWFSFRDAT(IGRID)%WAT1
      CONCQ=>GWFSFRDAT(IGRID)%CONCQ
      CONCRUN=>GWFSFRDAT(IGRID)%CONCRUN
      CONCPPT=>GWFSFRDAT(IGRID)%CONCPPT
      THTS=>GWFSFRDAT(IGRID)%THTS
      THTR=>GWFSFRDAT(IGRID)%THTR
      EPS=>GWFSFRDAT(IGRID)%EPS
      FOLDFLBT=>GWFSFRDAT(IGRID)%FOLDFLBT
      THTI=>GWFSFRDAT(IGRID)%THTI
      UZFLWT=>GWFSFRDAT(IGRID)%UZFLWT
      UZSTOR=>GWFSFRDAT(IGRID)%UZSTOR
      UZWDTH=>GWFSFRDAT(IGRID)%UZWDTH
      UZSEEP=>GWFSFRDAT(IGRID)%UZSEEP
      DELSTOR=>GWFSFRDAT(IGRID)%DELSTOR
      WETPER=>GWFSFRDAT(IGRID)%WETPER
      UZDPIT=>GWFSFRDAT(IGRID)%UZDPIT
      UZDPST=>GWFSFRDAT(IGRID)%UZDPST
      UZTHIT=>GWFSFRDAT(IGRID)%UZTHIT
      UZTHST=>GWFSFRDAT(IGRID)%UZTHST
      UZSPIT=>GWFSFRDAT(IGRID)%UZSPIT
      UZSPST=>GWFSFRDAT(IGRID)%UZSPST
      UZFLIT=>GWFSFRDAT(IGRID)%UZFLIT
      UZFLST=>GWFSFRDAT(IGRID)%UZFLST
      UZOLSFLX=>GWFSFRDAT(IGRID)%UZOLSFLX
      SUMRCH=>GWFSFRDAT(IGRID)%SUMRCH
      SUMLEAK=>GWFSFRDAT(IGRID)%SUMLEAK
      HLDSFR=>GWFSFRDAT(IGRID)%HLDSFR
      STRMDELSTOR_CUM=>GWFSFRDAT(IGRID)%STRMDELSTOR_CUM
      STRMDELSTOR_RATE=>GWFSFRDAT(IGRID)%STRMDELSTOR_RATE
      TOTSPFLOW=>GWFSFRDAT(IGRID)%TOTSPFLOW   
      TABFLOW=>GWFSFRDAT(IGRID)%TABFLOW
      TABTIME=>GWFSFRDAT(IGRID)%TABTIME
      ISFRLIST=>GWFSFRDAT(IGRID)%ISFRLIST
      FNETSEEP=>GWFSFRDAT(IGRID)%FNETSEEP
      NSEGDIM=>GWFSFRDAT(IGRID)%NSEGDIM
      factor=>GWFSFRDAT(IGRID)%factor
C
      END SUBROUTINE SGWF2SFR7PNT
C
C-------SUBROUTINE SGWF2SFR7PSV
      SUBROUTINE SGWF2SFR7PSV(IGRID)
C  Save SFR data for a grid.
      USE GWFSFRMODULE
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER IGRID
C     ------------------------------------------------------------------
      GWFSFRDAT(IGRID)%NFLOWTYPE=>NFLOWTYPE
      GWFSFRDAT(IGRID)%FLOWTYPE=>FLOWTYPE
      GWFSFRDAT(IGRID)%NSS=>NSS
      GWFSFRDAT(IGRID)%NSTRM=>NSTRM
      GWFSFRDAT(IGRID)%NSFRPAR=>NSFRPAR
      GWFSFRDAT(IGRID)%ISTCB1=>ISTCB1
      GWFSFRDAT(IGRID)%ISTCB2=>ISTCB2
      GWFSFRDAT(IGRID)%IUZT=>IUZT
      GWFSFRDAT(IGRID)%MAXPTS=>MAXPTS
      GWFSFRDAT(IGRID)%ISFROPT=>ISFROPT
      GWFSFRDAT(IGRID)%NSTRAIL=>NSTRAIL
      GWFSFRDAT(IGRID)%ISUZN=>ISUZN
      GWFSFRDAT(IGRID)%NSFRSETS=>NSFRSETS
      GWFSFRDAT(IGRID)%NUZST=>NUZST
      GWFSFRDAT(IGRID)%NSTOTRL=>NSTOTRL
      GWFSFRDAT(IGRID)%NUMAVE=>NUMAVE
      GWFSFRDAT(IGRID)%ITMP=>ITMP
      GWFSFRDAT(IGRID)%IRDFLG=>IRDFLG
      GWFSFRDAT(IGRID)%IPTFLG=>IPTFLG
      GWFSFRDAT(IGRID)%NP=>NP
      GWFSFRDAT(IGRID)%CONST=>CONST
      GWFSFRDAT(IGRID)%DLEAK=>DLEAK
      GWFSFRDAT(IGRID)%NUMTIM=>NUMTIM
      GWFSFRDAT(IGRID)%WEIGHT=>WEIGHT
      GWFSFRDAT(IGRID)%SFRRATIN=>SFRRATIN
      GWFSFRDAT(IGRID)%SFRRATOUT=>SFRRATOUT
      GWFSFRDAT(IGRID)%FLWTOL=>FLWTOL
      GWFSFRDAT(IGRID)%IRTFLG=>IRTFLG
      GWFSFRDAT(IGRID)%IOTSG=>IOTSG
      !GWFSFRDAT(IGRID)%IDVFLG=>IDVFLG        !cjm
      !GWFSFRDAT(IGRID)%DVRCH=>DVRCH        !cjm
      !GWFSFRDAT(IGRID)%DVEFF=>DVEFF        !cjm
      !GWFSFRDAT(IGRID)%DVRCELL=>DVRCELL    !cjm
      !GWFSFRDAT(IGRID)%DVRPERC=>DVRPERC  !cjm
      GWFSFRDAT(IGRID)%RECHSAVE=>RECHSAVE  !cjm
      GWFSFRDAT(IGRID)%NSEGCK=>NSEGCK
      GWFSFRDAT(IGRID)%ITRLSTH=>ITRLSTH
      GWFSFRDAT(IGRID)%ISEG=>ISEG
      GWFSFRDAT(IGRID)%IDIVAR=>IDIVAR
      GWFSFRDAT(IGRID)%ISTRM=>ISTRM
      GWFSFRDAT(IGRID)%LTRLIT=>LTRLIT
      GWFSFRDAT(IGRID)%LTRLST=>LTRLST
      GWFSFRDAT(IGRID)%ITRLIT=>ITRLIT
      GWFSFRDAT(IGRID)%ITRLST=>ITRLST
      GWFSFRDAT(IGRID)%NWAVST=>NWAVST
      GWFSFRDAT(IGRID)%STRIN=>STRIN
      GWFSFRDAT(IGRID)%STROUT=>STROUT
      GWFSFRDAT(IGRID)%FXLKOT=>FXLKOT
      GWFSFRDAT(IGRID)%UHC=>UHC
      GWFSFRDAT(IGRID)%SGOTFLW=>SGOTFLW
      GWFSFRDAT(IGRID)%DVRSFLW=>DVRSFLW
      GWFSFRDAT(IGRID)%SFRUZBD=>SFRUZBD
      GWFSFRDAT(IGRID)%SEG=>SEG
      GWFSFRDAT(IGRID)%STRM=>STRM
      GWFSFRDAT(IGRID)%HSTRM=>HSTRM
      GWFSFRDAT(IGRID)%QSTRM=>QSTRM
      GWFSFRDAT(IGRID)%SLKOTFLW=>SLKOTFLW
      GWFSFRDAT(IGRID)%DLKOTFLW=>DLKOTFLW
      GWFSFRDAT(IGRID)%DLKSTAGE=>DLKSTAGE
      GWFSFRDAT(IGRID)%HWDTH=>HWDTH
      GWFSFRDAT(IGRID)%HWTPRM=>HWTPRM
      GWFSFRDAT(IGRID)%SFRQ=>SFRQ
      GWFSFRDAT(IGRID)%QSTAGE=>QSTAGE
      GWFSFRDAT(IGRID)%XSEC=>XSEC
      GWFSFRDAT(IGRID)%AVDPT=>AVDPT
      GWFSFRDAT(IGRID)%AVWAT=>AVWAT
      GWFSFRDAT(IGRID)%WAT1=>WAT1
      GWFSFRDAT(IGRID)%CONCQ=>CONCQ
      GWFSFRDAT(IGRID)%CONCRUN=>CONCRUN
      GWFSFRDAT(IGRID)%CONCPPT=>CONCPPT
      GWFSFRDAT(IGRID)%THTS=>THTS
      GWFSFRDAT(IGRID)%THTR=>THTR
      GWFSFRDAT(IGRID)%EPS=>EPS
      GWFSFRDAT(IGRID)%FOLDFLBT=>FOLDFLBT
      GWFSFRDAT(IGRID)%THTI=>THTI
      GWFSFRDAT(IGRID)%UZFLWT=>UZFLWT
      GWFSFRDAT(IGRID)%UZSTOR=>UZSTOR
      GWFSFRDAT(IGRID)%UZWDTH=>UZWDTH
      GWFSFRDAT(IGRID)%UZSEEP=>UZSEEP
      GWFSFRDAT(IGRID)%DELSTOR=>DELSTOR
      GWFSFRDAT(IGRID)%WETPER=>WETPER
      GWFSFRDAT(IGRID)%UZDPIT=>UZDPIT
      GWFSFRDAT(IGRID)%UZDPST=>UZDPST
      GWFSFRDAT(IGRID)%UZTHIT=>UZTHIT
      GWFSFRDAT(IGRID)%UZTHST=>UZTHST
      GWFSFRDAT(IGRID)%UZSPIT=>UZSPIT
      GWFSFRDAT(IGRID)%UZSPST=>UZSPST
      GWFSFRDAT(IGRID)%UZFLIT=>UZFLIT
      GWFSFRDAT(IGRID)%UZFLST=>UZFLST
      GWFSFRDAT(IGRID)%UZOLSFLX=>UZOLSFLX
      GWFSFRDAT(IGRID)%SUMRCH=>SUMRCH
      GWFSFRDAT(IGRID)%SUMLEAK=>SUMLEAK
      GWFSFRDAT(IGRID)%HLDSFR=>HLDSFR
      GWFSFRDAT(IGRID)%STRMDELSTOR_CUM=>STRMDELSTOR_CUM
      GWFSFRDAT(IGRID)%STRMDELSTOR_RATE=>STRMDELSTOR_RATE
      GWFSFRDAT(IGRID)%TOTSPFLOW=>TOTSPFLOW
      GWFSFRDAT(IGRID)%TABFLOW=>TABFLOW
      GWFSFRDAT(IGRID)%TABTIME=>TABTIME
      GWFSFRDAT(IGRID)%ISFRLIST=>ISFRLIST
      GWFSFRDAT(IGRID)%FNETSEEP=>FNETSEEP
      GWFSFRDAT(IGRID)%NSEGDIM=>NSEGDIM
      GWFSFRDAT(IGRID)%factor=>factor
C
      END SUBROUTINE SGWF2SFR7PSV
