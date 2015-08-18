C Revised May 20, 2006 DEP and RGN;
C The Package was modified to be compatible with modifications made
C  to the Lake (LAK3) Package for computing Lake Outflow
C RGN Two bugs fixed 10/23/06; initialize flowc; IF logic for calls to RDSEG
C-------SUBROUTINE GWF1SFR2DF
      SUBROUTINE GWF1SFR2DF(Nlakes, Nlakesar, Lkacc7, Lcstag, Lslake,
     +                      Lstgld, Lcrnf, Istrin, Idstrt, Istrot,
     +                      Istgnw,Lscout, Lsconq, Lscnrn, Lscppt,
     +                      Lscntb,Lsslin, Lscqin, Lscgw, Lssin, Lssout,
     +                      Lscoto, Isuzn, Nuzst, Nstotrl, Nstrmsqd,
     +                      Numcell, Nuzrow, Nuzcol, Nsslk)
C     ******************************************************************
C     READ STREAM DATA FOR STRESS PERIOD
C     VERSION  2.7: MARCH 16, 2009
C     ******************************************************************
      IMPLICIT NONE

C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER Idstrt, Istgnw, Istrin, Istrot, Isuzn, Lcstag, Lkacc7,
     +        Lscgw, Lscnrn, Lscntb, Lsconq, Lscoto, Lscout, Lscppt,
     +        Lscqin, Lslake, Lssin, Lsslin, Lssout, Lstgld, Lcrnf
      INTEGER Nlakes, Nlakesar, Nstotrl, Nstrmsqd, Numcell, Nuzcol,
     +        Nuzrow, Nuzst, Nsslk

C***********************************************************************
C
C1------CONNECTION TO LAKE PACKAGE.
      Nlakes = 0
      Nlakesar = 1
      Nsslk = 1
      Lkacc7 = 1
      Lcstag = 1
      Lslake = 1
      Lstgld = 1
      Istrin = 1
      Idstrt = 1
      Istrot = 1
      Istgnw = 1
      Lcrnf = 1
C
C2------CONNECTION TO THE GWT PROCESS.
      Lscout = 1
      Lsconq = 1
      Lscnrn = 1
      Lscppt = 1
      Lscntb = 1
      Lsslin = 1
      Lscqin = 1
      Lscgw = 1
      Lssin = 1
      Lssout = 1
      Lscoto = 1
C
C3------UNSATURATED FLOW PROCESS.
      Isuzn = 1
      Nuzst = 1
      Nstotrl = 1
      Nstrmsqd = 1
      Numcell = 1
      Nuzrow = 1
      Nuzcol = 1
C
C4------RETURN.
      RETURN
      END SUBROUTINE GWF1SFR2DF
C
C-------SUBROUTINE GWF1SFR2ALP
      SUBROUTINE GWF1SFR2ALP(Isumrx, Isumir, Isumrz, Lcstrm, Icstrm,
     +                       Nstrm, In, Iout, Istcb1, Istcb2, Nss,
     +                       Const, Maxpts, Dleak, Lcseg, Icseg, Lcotsg,
     +                       Lcxsec, Lcivar, Lcqstg, Iunitlak, Istrin,
     +                       Istrot, Lcotflw, Lcdvflw, I15, Nsol,
     +                       Lscout, Lsconq, Lscnrn, Lscntb, Lsslin,
     +                       Lscqin, Lscgw, Istgld, Idstrt, Lssin,
     +                       Lssout, Lscoto, Lkacc7, Lcstag, Lslake,
     +                       Istgnw, Lscppt, Lcnseg, Nsfrpar, Nsegdim,
     +                       Lcsfrq, Nstrmar, Nssar, Lcsuzdpit,
     +                       Lcsuzdpst, Lcsuzthit, Lcsuzthst, Lcsuzspit,
     +                       Lcsuzspst, Lcsuzflit, Lcsuzflst, Lcsuzflwt,
     +                       Lcsuzstor,Lcsdelstr, Lcsuzwdth, Icsltrlit,
     +                       Icsltrlst,Icsitrlit, Icsitrlst, Icstrlhld,
     +                       Icsnwavst, Isfropt, Iuzt, Isuzn, Nstrail,
     +                       Nstotrl, Nuzst, Lcsolsflx, Icsloop,
     +                       Lcsthts, Lcsthtr, Lcsthti, Lcseps, Ncol,
     +                       Nrow, Icelev, Lcdpth, Lcwetp, Nsfrsets,
     +                       Lcsuzseep, Lcoldflbt, Numcell, Nuzrow,
     +                       Nuzcol, Lcavwat, Lcwat1, Numave, Lcavdpt,
     +                       Lcuhc, Lcsfruzbd, Nsslk, Islkotflw, 
     +                       Idlkotflw, Idlkstage)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR STREAMS
C     VERSION  2.7: MARCH 16, 2009 dep
C      Added locations for three new arrays for computing lake outflow
C     ******************************************************************
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL Const, Dleak
      INTEGER I15, Icelev, Icseg, Icsitrlit, Icsitrlst, Icsloop,
     +        Icsltrlit, Icsltrlst, Icsnwavst, Icstrlhld, Icstrm,
     +        Idstrt, In, Iout, Isfropt
      INTEGER Istcb1, Istcb2, Istgld, Istgnw, Istrin
Cdep  new arrays needed for lake outflow calculations in Lake Package
      INTEGER Islkotflw, Idlkotflw, Idlkstage, Nsslk
      INTEGER Istrot, Isumir, Isumrx, Isumrz, Isuzn, Iunitlak, Iuzt,
     +        Lcavdpt, Lcavwat, Lcdpth, Lcdvflw, Lcivar, Lcnseg,
     +        Lcoldflbt, Lcotflw, Lcotsg, Lcqstg, Lcsdelstr, Lcseg
      INTEGER Lcseps, Lcsfrq, Lcsfruzbd, Lcsolsflx, Lcstag, Lcsthti,
     +        Lcsthtr, Lcsthts, Lcstrm, Lcsuzdpit, Lcsuzdpst, Lcsuzflit,
     +        Lcsuzflst, Lcsuzflwt, Lcsuzseep, Lcsuzspit, Lcsuzspst,
     +        Lcsuzstor, Lcsuzthit, Lcsuzthst, Lcsuzwdth
      INTEGER Lcuhc, Lcwat1, Lcwetp, Lcxsec, Lkacc7, Lscgw, Lscnrn,
     +        Lscntb, Lsconq, Lscoto, Lscout, Lscppt, Lscqin, Lslake,
     +        Lssin, Lsslin, Lssout, Maxpts, Ncol
      INTEGER Nrow, Nsegdim, Nsfrpar, Nsfrsets, Nsol, Nss, Nssar,
     +        Nstotrl, Nstrail, Nstrm, Nstrmar, Numave, Numcell, Nuzcol,
     +        Nuzrow, Nuzst
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      CHARACTER*200 line
      REAL r
      INTEGER i, isfruz, isp, ispa, ispb, ispc, ispd, ispe, ispf, ispg,
     +        isph, ispi, ispir, ispj, ispl, ispm, ispn, isprx, isprz,
     +        istart, istop, lloc, nparseg, nsegsl, nsrchs, nssgsl,
     +        nstrmsqd
C***********************************************************************
C
C1------IDENTIFY PACKAGE AND INITIALIZE NSTRM.
      WRITE (Iout, 9001) In
 9001 FORMAT (1X, /1X, 'SFR2 -- STREAMFLOW ROUTING PACKAGE, VERSION ',
     +        '2.7, 03/16/2009', /, 9X, 'INPUT READ FROM UNIT', I4)
C
C2------READ COMMENT RECORDS, NSTRM, NSS, NSFRPAR, NPARSEG, CONST,
C        DLEAK, ISTCB1, AND ISTCB2 ISFROPT.
      CALL URDCOM(In, Iout, line)
      lloc = 1
      Isfropt = 0
      Iuzt = 0
      ispir = 0
      isprx = 0
      isprz = 0
      CALL URWORD(line, lloc, istart, istop, 2, Nstrm, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, Nss, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, Nsfrpar, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, nparseg, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 3, i, Const, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 3, i, Dleak, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, Istcb1, r, Iout, In)
      CALL URWORD(line, lloc, istart, istop, 2, Istcb2, r, Iout, In)
C
C3------READ UNSATURATED FLOW FLAGS WHEN Nstrm IS LESS THAN ZERO.
      Nsfrsets = 0
      Nstrail = 0
      IF ( Nstrm.LT.0 ) THEN
        IF ( Nsfrpar.GT.0 ) THEN
          WRITE(Iout,9002)
 9002     FORMAT(/1X, ' NSTRM IS NEGATIVE AND NSFRPAR IS GREATER THAN ',
     +          'ZERO ', /1X , 'ALTERNATE SFR2 OPTIONS DO NOT ',
     +          'SUPPORT PARAMETERS--PROGRAM STOPPING ',/)
          CALL USTOP(' ')
        END IF
        Nstrm = ABS(Nstrm)
        CALL URWORD(line, lloc, istart, istop, 2, Isfropt, r, Iout, In)
        IF ( Isfropt.GE.2 ) THEN
          Iuzt = 1
          CALL URWORD(line, lloc, istart, istop, 2, Nstrail, r, Iout,
     +                In)
          CALL URWORD(line, lloc, istart, istop, 2, Isuzn, r, Iout, In)
          CALL URWORD(line, lloc, istart, istop, 2, Nsfrsets, r, Iout,
     +                In)
        END IF
      END IF
      IF ( Nss.LT.0 ) Nss = 0
      IF ( Nsfrpar.LE.0 ) THEN
        Nsfrpar = 0
        nparseg = 0
      END IF
      IF ( nparseg.LT.0 ) nparseg = 0
C
C4------PRINT INFORMATION THAT WAS READ.
      WRITE (Iout, 9003) Nstrm, Nss, Nsfrpar, nparseg, Dleak, Const
 9003 FORMAT (//1X, 'NUMBER OF STREAM NODES IS', I5//1X,
     +        'NUMBER OF STREAM SEGMENTS IS', I5//1X,
     +        'NUMBER OF STREAM PARAMETERS IS', I5//1X,
     +        'NUMBER OF STREAM SEGMENTS DEFINED USING PARAMETERS IS',
     +        I5//1X, 'MAXIMUM ERROR FOR STREAM LEAKAGE RATES IS',
     +        1PE10.2//1X, 'CONSTANT FOR MANNINGS EQUATION IS ',
     +        E11.4,/)

      IF ( Isfropt.LE.1 )WRITE (Iout, 9004)
 9004 FORMAT (/1X, 'UNSATURATED FLOW BENEATH STREAMS IS INACTIVE ', /)
      IF ( Isfropt.EQ.1 ) WRITE (Iout, 9005)
 9005 FORMAT (/1X, 'USING DATA INPUT MODIFIED FROM ORIGINAL SFR ',
     +        'PROGRAM ',//)
      IF ( Isfropt.GE.2 ) WRITE (Iout, 9006)
 9006 FORMAT (//1X, 'UNSATURATED FLOW BENEATH STREAMS IS ACTIVE ', //)
      IF ( Istcb1.GT.0 ) WRITE (Iout, 9007) Istcb1
 9007 FORMAT (' FLOW TO AND FROM GROUND WATER FOR EACH STREAM REACH ',
     +        'WILL BE SAVED ON UNIT', I3)
      IF ( Istcb2.GT.0 ) WRITE (Iout, 9008) Istcb2
 9008 FORMAT (1X, 'STREAM OUTPUT WILL BE WRITTEN TO FILE ON UNIT ', I3)
C
C5------CHECK FOR ERRORS.
      IF ( Nstrm.LE.0 .OR. Nss.LE.0 ) THEN
        WRITE (Iout, 9009)
 9009   FORMAT (//1X,
     +          'NO STREAM REACHES (NSTRM) AND/OR SEGMENTS (NSS)--'//,
     +          ' SFR PACKAGE BEING TURNED OFF'///)
        In = 0
        Nss = 0
        Nstrm = 0
        RETURN
      END IF
      IF ( Nsfrpar.GT.0 .AND. nparseg.LE.0 ) THEN
        WRITE (Iout, 9010)
 9010   FORMAT (//1X, 'NO STREAM SEGMENTS DEFINED BY PARAMETERS--',
     +          'NSFRPAR GT ZERO AND NPARSEG LE ZERO'//,
     +          ' SFR PACKAGE BEING TURNED OFF'///)
        In = 0
        Nss = 0
        Nstrm = 0
        RETURN
      END IF
      IF ( Iuzt.EQ.1 ) THEN
        IF ( Nstrail.LT.0 ) THEN
          WRITE (Iout, 9011)
 9011     FORMAT (//1X, 'NUMBER OF TRAILING WAVES IS LESS THAN ZERO',
     +            '--SETTING VALUE TO A POSITIVE VALUE'///)
          Nstrail = ABS(Nstrail)
        END IF
        IF ( Nstrail.EQ.0 ) THEN
          WRITE (Iout, 9012)
 9012     FORMAT (//1X, 'VERTICAL FLOW THROUGH UNSATURATED ZONE IS ',
     +            'ACTIVE AND NUMBER OF TRAILING WAVES IS ZERO-- ',
     +            ' RESETTING UNSATURATED FLOW TO BE INACTIVE '///)
          Iuzt = 0
        END IF
      END IF
C
C6------CHECK FOR DLEAK.
      IF ( Dleak.LE.0.0 ) THEN
        Dleak = 0.00001
        WRITE (Iout, 9013)
 9013   FORMAT (//1X, '*** WARNING ***   DLEAK IS LESS THAN ',
     +          'OR EQUAL TO ZERO --- ',
     +          'DLEAK ASSIGNED A VALUE OF 0.0001'///)
      END IF
      Nstrmar = Nstrm
      Nssar = Nss
      IF ( Iunitlak.GT.0 ) THEN
        Nsslk = Nss
      ELSE
        Nsslk = 1
      END IF
C
C7------SET LCSTRM EQUAL TO ADDRESS OF FIRST UNUSED SPACE IN RX.
      Lcstrm = Isumrx
C
C8------CALCULATE SPACE NEEDED FOR STRM LIST (X18).
      ispa = 24*Nstrm
      Isumrx = Isumrx + ispa
C
C9------CALCULATE SPACE NEEDED FOR ISTRM LIST (X5).
      Icstrm = Isumir
      ispb = 5*Nstrm
      Isumir = Isumir + ispb
C
C10-----CALCULATE SPACE NEEDED FOR SEG LIST (X26).
      Nsegdim = Nss + nparseg
      Lcseg = Isumrx
      ispc = 26*Nsegdim
      Isumrx = Isumrx + ispc
C
C11-----CALCULATE SPACE NEEDED FOR ISEG, NSEGCK, AND IOTSG LISTS.
      Icseg = Isumir
      ispd = 4*Nsegdim
      Isumir = Isumir + ispd
      Lcnseg = Isumir
      ispe = Nss
      Isumir = Isumir + ispe
      Lcotsg = Isumir
      ispj = Nsegdim
      Isumir = Isumir + ispj
C
C12-----CALCULATE SPACE NEEDED FOR SGOTFLW, AND DVRSFLW LISTS.
      Lcotflw = Isumrx
      ispf = Nss
      Isumrx = Isumrx + ispf
      Lcdvflw = Isumrx
      Isumrx = Isumrx + ispf
C
C13-----CALCULATE SPACE NEEDED FOR XSEC LIST (X16).
      Lcxsec = Isumrx
      ispg = 16*Nsegdim
      Isumrx = Isumrx + ispg
C
C14-----CALCULATE SPACE NEEDED FOR IDIVAR LIST.
      Lcivar = Isumir
      isph = 2*Nsegdim
      Isumir = Isumir + isph
C
C15-----CALCULATE SPACE NEEDED FOR TABULATED DISCHARGE VERSUS FLOW
C         AND WIDTH RELATIONS.
      Lcqstg = Isumrx
      Maxpts = 3*50
      ispi = Maxpts*Nsegdim
      Isumrx = Isumrx + ispi
C
      isprx = ispa + ispc + 2*ispf + ispg + ispi
      ispir = ispb + ispd + ispe + isph + ispj
C
C16-----CALCULATE SPACE FOR CONNECTION TO LAKE PACKAGE.
      IF ( Iunitlak.LE.0 ) THEN
        Lkacc7 = Isumrx
        Lcstag = Isumrx + 1
        Lslake = Isumrx + 2
!        Istgld = Isumrx + 3
        Istgld = Isumrz + 3  ! ERB 2/24/10 Istgld is pointer to RZ array
        Istrin = Isumrx + 4
        Idstrt = Isumrx + 5
        Istrot = Isumrx + 6
!        Istgnw = Isumrx + 7
        Istgnw = Isumrz + 7  ! ERB 2/24/10 Istgnw is pointer to RZ array
        Isumrx = Isumrx + 8
C-------DOUBLE PRECISION VARIABLES
        Islkotflw = Isumrz
          Isumrz = Isumrz + 200*Nsslk
        Idlkotflw = Isumrz
          Isumrz = Isumrz + 200*Nsslk
        Idlkstage = Isumrz
          Isumrz = Isumrz + 200*Nsslk
        isprx = isprx + 8 
        isprz = isprz + 600*Nsslk
      ELSE
C17-----CALCULATE AMOUNT OF SPACE NEEDED FOR STRIN LIST.
        Istrin = Isumrx
        ispl = Nss
        Isumrx = Isumrx + ispl
C
C18-----CALCULATE AMOUNT OF SPACE NEEDED FOR STROUT LIST.
        Istrot = Isumrx
        ispm = Nss
        Isumrx = Isumrx + ispm
C
C19-----CALCULATE AMOUNT OF SPACE NEEDED FOR DSTROT LIST.
Cdep changed DSTROT to FXLKOT
        Idstrt = Isumrx
        ispn = Nss
        Isumrx = Isumrx + ispn
C
Cdep    Added three new double precision arrays for computing 
Cdep       lake outflow into a stream
        Islkotflw = Isumrz 
          Isumrz = Isumrz+ 200*Nsslk
        Idlkotflw = Isumrz
          Isumrz = Isumrz + 200*Nsslk
        Idlkstage = Isumrz
          Isumrz = Isumrz + 200*Nsslk
        isprx = isprx + ispl + ispm + ispn
        isprz = isprz + 600*Nsslk
      END IF
C
C20-----CALCULATE SPACE NEEDED FOR SOLUTE ROUTING.
      IF ( I15.GT.0 ) THEN
        nsrchs = Nstrm*Nsol
        Lscout = Isumrx
        Isumrx = Isumrx + nsrchs
        Lsconq = Isumrx
        nsegsl = Nsegdim*Nsol
        Isumrx = Isumrx + nsegsl
        Lscnrn = Isumrx
        Isumrx = Isumrx + nsegsl
        Lscppt = Isumrx
        Isumrx = Isumrx + nsegsl
        Lscntb = Isumrx
        nssgsl = Nss*Nsol
        Isumrx = Isumrx + nssgsl
        Lsslin = Isumrx
        Isumrx = Isumrx + Nsol
        Lscqin = Isumrx
        Isumrx = Isumrx + Nsol
        Lscgw = Isumrx
        Isumrx = Isumrx + Nsol
        Lssin = Isumrx
        Isumrx = Isumrx + Nsol
        Lssout = Isumrx
        Isumrx = Isumrx + Nsol
        Lscoto = Isumrx
        Isumrx = Isumrx + Nsol
        isprx = isprx + nsrchs + (3*nssgsl) + nsegsl + (6*Nsol)
      ELSE
C
C21-----ELSE, SET FINITE DUMMY VALUES TO ISUM.
        Lscout = Isumrx
        Lsconq = Isumrx + 1
        Lscnrn = Isumrx + 2
        Lscppt = Isumrx + 3
        Lscntb = Isumrx + 4
        Lsslin = Isumrx + 5
        Lscqin = Isumrx + 6
        Lscgw = Isumrx + 7
        Lssin = Isumrx + 8
        Lssout = Isumrx + 9
        Lscoto = Isumrx + 10
        Isumrx = Isumrx + 11
        isprx = isprx + 11
      END IF
      IF ( Iuzt.EQ.1 ) THEN
C
C22-----ALLOCATE SPACE FOR UNSATURATED FLOW.
        Nuzst = Nstrm
        Nstotrl = Isuzn*Nstrail*Nsfrsets
        nstrmsqd = Nuzst*Isuzn
        Numcell = Nrow*Ncol
        Nuzrow = Nrow
        Nuzcol = Ncol
        Numave = 21
        isfruz = 6
      ELSE
C
C23-----ALLOCATE ONLY ONE ARRAY ELEMENT IF UNSATRATED FLOW
C         IS INACTIVE.
        Nuzst = 1
        Nstotrl = 1
        nstrmsqd = 1
        Numcell = 1
        Nuzrow = 1
        Nuzcol = 1
        Numave = 1
        isfruz = 1
      END IF
      isp = Nuzst*Nstotrl
      ispa = isp
C
C24-----CALCULATE SPACE FOR UNSATURATED HYDRAULIC PROPERTIES.
C     SINGLE PRECISION VARIABLE.
      Lcuhc = Isumrx
      Isumrx = Isumrx + Nuzst
C     DOUBLE PRECISION VARIABLES.
      Lcsthts = Isumrz
      Isumrz = Isumrz + Nuzst
      Lcsthtr = Isumrz
      Isumrz = Isumrz + Nuzst
      Lcsthti = Isumrz
      Isumrz = Isumrz + Nuzst
      Lcseps = Isumrz
      Isumrz = Isumrz + Nuzst
      Lcoldflbt = Isumrz
      Isumrz = Isumrz + Nuzst
C
C25-----CALCULATE SPACE FOR UNSATURATED ZONE WAVE ARRAYS.
C     SINGLE PRECISION VARIABLES.
      Lcavwat = Isumrx
      Isumrx = Isumrx + Numave*Nuzst
      Lcwat1 = Isumrx
      Isumrx = Isumrx + Numave*Nuzst
      Lcavdpt = Isumrx
      Isumrx = Isumrx + Numave*Nuzst
C     DOUBLE PRECISION VARIABLES.
      Lcsuzdpit = Isumrz
      Isumrz = Isumrz + ispa
      Lcsuzdpst = Isumrz
      Isumrz = Isumrz + ispa
      Lcsuzthit = Isumrz
      Isumrz = Isumrz + ispa
      Lcsuzthst = Isumrz
      Isumrz = Isumrz + ispa
      Lcsuzflit = Isumrz
      Isumrz = Isumrz + ispa
      Lcsuzflst = Isumrz
      Isumrz = Isumrz + ispa
      Lcsuzspit = Isumrz
      Isumrz = Isumrz + ispa
      Lcsuzspst = Isumrz
      Isumrz = Isumrz + ispa
C
C26-----CALCULATE SPACE NEEDED FOR UZ FLOW AND STORAGE.
C     SINGLE PRECISION VARIABLE.
      Lcdpth = Isumrx
      Isumrx = Isumrx + nstrmsqd
C     DOUBLE PRECISION VARIABLES.
      Lcsuzflwt = Isumrz
      Isumrz = Isumrz + nstrmsqd
      Lcsuzstor = Isumrz
      Isumrz = Isumrz + nstrmsqd
      Lcsdelstr = Isumrz
      Isumrz = Isumrz + nstrmsqd
      Lcsuzwdth = Isumrz
      Isumrz = Isumrz + nstrmsqd
      Lcsuzseep = Isumrz
      Isumrz = Isumrz + nstrmsqd
      Lcsolsflx = Isumrz
      Isumrz = Isumrz + nstrmsqd
      Lcwetp = Isumrz
      Isumrz = Isumrz + nstrmsqd
C
C27-----CALCULATE SPACE NEEDED FOR BUDGET TERMS.
      Lcsfruzbd = Isumrx
      Isumrx = Isumrx + isfruz
C
C28-----SET ICNWAVS EQUAL TO ADDRESS OF FIRST UNUSED SPACE IN IR ARRAY.
      Icsnwavst = Isumir
      Isumir = Isumir + nstrmsqd
      Icelev = Isumir
      Isumir = Isumir + nstrmsqd
C
C29-----CALCULATE SPACE NEEDED FOR ICLTRAIL AND ICITRIAL.
      Icsltrlit = Isumir
      Isumir = Isumir + isp
      Icsitrlit = Isumir
      Isumir = Isumir + isp
      Icsltrlst = Isumir
      Isumir = Isumir + isp
      Icsitrlst = Isumir
      Isumir = Isumir + isp
      Icstrlhld = Isumir
      Isumir = Isumir + isp
      Icsloop = Isumir
      Isumir = Isumir + Isuzn
C
C30-----ALLOCATE SPACE FOR UNSATURATED FLOW.
      isprx = isprx + nstrmsqd + Nuzst + 3*Numave*Nuzst + isfruz
      isprz = isprz + 8*ispa + 7*nstrmsqd + 5*Nuzst
      ispir = ispir + 5*isp + 2*nstrmsqd + Isuzn
C
C31-----ALLOCATE SPACE REQUIRED FOR SENSISTIVITY PROCESS.
      Lcsfrq = Isumrx
      Isumrx = Isumrx + 5*Nstrm
C
C32-----PRINT SPACE USED BY STREAM PACKAGE.
      WRITE (Iout, 9014) isprx
 9014 FORMAT (1X, I10, ' ELEMENTS IN RX ARRAY ARE USED BY SFR2')
      WRITE (Iout, 9015) isprz
 9015 FORMAT (1X, I10, ' ELEMENTS IN RZ ARRAY ARE USED BY SFR2')
      WRITE (Iout, 9016) ispir
 9016 FORMAT (1X, I10, ' ELEMENTS IN IR ARRAY ARE USED BY SFR2')
C
C33-----RETURN.
      RETURN
      END SUBROUTINE GWF1SFR2ALP
C
C-------SUBROUTINE GWF1SFR2RPP
      SUBROUTINE GWF1SFR2RPP(Itrss, Strm, Istrm, Nstrm, In, Iout, Seg, 
     +                       Iseg, Nss, Idivar, Iotsg, Sgotflw, Dvrsflw,
     +                       Maxpts, Xsec, Qstage, I15, Concq, Concrun,
     +                       Concppt, Nsol, Iouts, Nsfrpar, Nsegdim,
     +                       Iterp, Inamloc, Ibound, Ncol, Nrow, Nlay,
     +                       Uzdpit, Uzdpst, Uzthit, Uzthst, Uzspit,
     +                       Uzspst, Uzflit, Uzflst, Ltrlit, Ltrlst,
     +                       Itrlit, Itrlst, Itrlsth, Uzflwt, Uzstor,
     +                       Delstor, Uzwdth, Nwavst, Nstotrl, Isfropt,
     +                       Iuzt, Isuzn, Nuzst, Uzolsflx, Sy, Thts,
     +                       Thtr, Thti, Eps, Delr, Delc, Uzseep,
     +                       Foldflbt, Nuzrow, Nuzcol, Uhc, Iunitbc6,
     +                       Iunithuf, Sc1, Botm, Nbotm, Strt, Sfruzbd, 
     +                       Iss, Itmp, Irdflg, Iptflg, Np, Nsegck, 
     +                       Iunitlak,Nsslk, Slkotflw, Dlkotflw, 
     +                       Dlkstage)
C     ******************************************************************
C     READ STREAM DATA FOR FIRST STRESS PERIOD
C     VERSION  2.7: MARCH 16, 2009
C     Compute three new tables for lake outflow
C     ******************************************************************
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL Botm, Concppt, Concq, Concrun, Delc, Delr, Dvrsflw, Qstage,
     +     Sc1, Seg, Sfruzbd, Sgotflw, Strm, Strt, Sy, Uhc, Xsec
      INTEGER I15, Ibound, Idivar, In, Inamloc, Iotsg, Iout, Iouts
      INTEGER Iseg, Isfropt, Istrm, Isuzn, Iterp, Itrlit, Itrlst,
     +        Itrlsth, Iunitbc6, Iunithuf, Iunitlak, Iuzt, Nsegck, Itrss
      INTEGER Ltrlit, Ltrlst, Maxpts, Nbotm, Ncol, Nlay, Nrow, Nsegdim,
     +        Nsfrpar, Nsol, Iss, Itmp, Irdflg, Iptflg, Np
      INTEGER Nss, Nstotrl, Nstrm, Nuzcol, Nuzrow, Nuzst, Nwavst, Nsslk
      DOUBLE PRECISION Uzspit, Uzspst, Uzflst, Uzflit, Uzthit, Uzthst,
     +                 Uzdpit, Uzdpst, Uzseep, Uzolsflx, Uzflwt, Uzstor,
     +                 Delstor, Uzwdth, Foldflbt, Thts, Thtr, Eps, Thti
Cdep  Assign three new arrays needed for computing Lake Outflow
      DOUBLE PRECISION Slkotflw, Dlkotflw, Dlkstage
      DIMENSION Uzspit(Nuzst, Nstotrl), Uzspst(Nuzst, Nstotrl),
     +          Uzflit(Nuzst, Nstotrl), Uzflst(Nuzst, Nstotrl),
     +          Uzthit(Nuzst, Nstotrl), Uzthst(Nuzst, Nstotrl)
      DIMENSION Uzdpit(Nuzst, Nstotrl), Uzdpst(Nuzst, Nstotrl),
     +          Uzflwt(Nuzst, Isuzn), Uzstor(Nuzst, Isuzn),
     +          Delstor(Nuzst, Isuzn), Uzwdth(Nuzst, Isuzn),
     +          Uzseep(Nuzst, Isuzn), Foldflbt(Nuzst)
      DIMENSION Strm(24, Nstrm), Istrm(5, Nstrm), Seg(26, Nsegdim),
     +          Iseg(4, Nsegdim), Iotsg(Nsegdim), Idivar(2, Nsegdim),
     +          Sgotflw(Nss), Dvrsflw(Nss), Xsec(16, Nsegdim),
     +          Qstage(Maxpts, Nsegdim), Ibound(Ncol, Nrow, Nlay),
     +          Strt(Ncol, Nrow, Nlay)
      DIMENSION Concq(Nsegdim, Nsol), Concrun(Nsegdim, Nsol),
     +          Concppt(Nsegdim, Nsol), Nsegck(Nss)
      DIMENSION Delr(Ncol), Delc(Nrow)
      DIMENSION Ltrlit(Nuzst, Nstotrl), Ltrlst(Nuzst, Nstotrl),
     +          Itrlit(Nuzst, Nstotrl), Itrlst(Nuzst, Nstotrl),
     +          Nwavst(Nuzst, Isuzn), Uzolsflx(Nuzst, Isuzn),
     +          Itrlsth(Nstotrl), Botm(Ncol, Nrow, 0:Nbotm)
      DIMENSION Thts(Nuzst), Thtr(Nuzst), Thti(Nuzst), Eps(Nuzst),
     +          Uhc(Nuzst), Sy(Ncol, Nrow, Nlay), Sc1(Ncol, Nrow, Nlay),
     +          Sfruzbd(6)
Cdep  Dimension three new arrays for computing Lake Outflow
      DIMENSION Slkotflw(200, Nsslk), Dlkotflw(200, Nsslk),
     +          Dlkstage(200, Nsslk)
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      INTEGER i, ib, ichc, ichk, idum, ii, ip, irch, irck, ireach,
     +        ireachck, itr
      INTEGER jj, jrch, jrck, jseg, jsegck, juzst, kk, kkptflg, krch
      INTEGER krck, kss, kuzbud, lb, lstbeg, lstsum, nlst, nreach, nseg,
     +        numinst, ksfropt, icalc
      REAL seglen, sumlen, thsslpe, thislpe, epsslpe, uhcslpe, rchlen,
     +     dist
      DIMENSION idum(1)
C     ------------------------------------------------------------------
C     COMMON VARIABLES
C     ------------------------------------------------------------------
C     COMMON FROM BCF6 PACKAGE.
      INTEGER LAYCON
      COMMON /BCFCOM/ LAYCON(999)
C
C     ------------------------------------------------------------------
C
C
C1------INITIALIZE VARIABLES AND LISTS.
      idum(1) = 0
      DO ichc = 1, Nuzst
        Thts(ichc) = 0.0D0
        Thtr(ichc) = 0.0D0
        Thti(ichc) = 0.0D0
        Eps(ichc) = 0.0D0
        Uhc(ichc) = 0.0
      END DO
      DO irch = 1, Nstrm
        DO ii = 1, 24
          Strm(ii, irch) = 0.0
        END DO
        DO ii = 1, 5
          Istrm(ii, irch) = 0
        END DO
      END DO
      DO kss = 1, Nss
        Sgotflw(kss) = 0.0
        Dvrsflw(kss) = 0.0
        Nsegck(kss) = 0
Cdep  Initialize three new arrays for lake outflow to zero
        IF (Iunitlak.GT.0) THEN
          DO ii = 1, 200
            Slkotflw(ii,kss) = 0.0
            Dlkotflw(ii,kss) = 0.0
            Dlkstage(ii,kss) = 0.0
          END DO
        ELSE IF (kss.EQ.1) THEN
            Slkotflw(ii,1) = 0.0
            Dlkotflw(ii,1) = 0.0
            Dlkstage(ii,1) = 0.0
        END IF
      END DO
Cdep
      DO kss = 1, Nsegdim
        Iseg(1, kss) = 0
        Iseg(2, kss) = 0
        Iseg(3, kss) = 0
        Iseg(4, kss) = 0
        Iotsg(kss) = 0
        Idivar(1, kss) = 0
        Idivar(2, kss) = 0
        DO ii = 1, 26
          Seg(ii, kss) = 0.0
        END DO
        DO ii = 1, 16
          Xsec(ii, kss) = 0.0
        END DO
        DO ii = 1, Maxpts
          Qstage(ii, kss) = 0.0
        END DO
      END DO
      DO kuzbud = 1, 6
        Sfruzbd(kuzbud) = 0.0
      END DO
C
C2------READ AND PRINT DATA FOR EACH STREAM REACH.
      nseg = 0
      nreach = 0
      IF ( Iunithuf.GT.0 ) THEN
        IF ( ISFROPT.GT.1 ) THEN
          WRITE (IOUT, 9034)
 9034     FORMAT (//, ' ***ERROR***  HUF PACKAGE IS ACTIVE ', 
     +          'AND ISFROPT IS GREATER THAN 1 ',/, 
     +          ' PROGRAM IS STOPPING')
          CALL USTOP(' ')
        END IF
      END IF
      IF ( Isfropt.EQ.0 ) THEN
        WRITE (Iout, 9001)
 9001   FORMAT (1X, //3X, 'STREAM NETWORK DESCRIPTION: ', //3X,
     +          'LAYER    ROW    COL   SEGMENT   REACH     LENGTH',
     +          /26X, 'NUMBER   NUMBER    IN CELL', /3X, 50('-'))
      ELSE IF ( Isfropt.EQ.1 ) THEN
        WRITE (Iout, 9002)
 9002   FORMAT (1X, //3X, 'STREAM NETWORK DESCRIPTION: ', //3X,
     +          'LAYER    ROW    COL   SEGMENT   REACH     LENGTH',
     +          '     STREAMBED     STREAMBED   STREAMBED',
     +          '     STREAMBED', /26X,
     +          'NUMBER   NUMBER    IN CELL    TOP ELEV.    ',
     +          '   SLOPE     THICKNESS', '   HYDR. CONDUCT.', /3X,
     +          105('-'))
      ELSE IF ( Isfropt.EQ.2 ) THEN
        WRITE (Iout, 9003)
 9003   FORMAT (1X, //3X, 'STREAM NETWORK DESCRIPTION: ', //3X,
     +          'LAYER  ROW   COL SEGMENT  REACH    LENGTH',
     +          '    STREAMBED   STREAMBED   STREAMBED   ',
     +          'STREAMBED   SATURATED   INITIAL',
     +          '    RESIDUAL    BROOKS/COREY', /20X,
     +          'NUMBER  NUMBER   IN CELL',
     +          '    TOP ELEV.     SLOPE     THICKNESS   ',
     +          'HYD. COND.  WAT.CONT.  WAT.CONT.   ',
     +          'WAT.CONT.     EPSILON', /3X, 150('-'))
      ELSE IF ( Isfropt.EQ.3 ) THEN
        WRITE (Iout, 9004)
 9004   FORMAT (1X, //3X, 'STREAM NETWORK DESCRIPTION: ', //3X,
     +          'LAYER  ROW   COL SEGMENT  REACH   LENGTH     ',
     +          'STREAMBED   STREAMBED   STREAMBED  STREAMBED',
     +          '    SATURATED    INITIAL',
     +          '    RESIDUAL  BROOKS/COREY  SAT. VERT.', /20X,
     +          'NUMBER  NUMBER  IN CELL     ',
     +          'TOP ELEV.     SLOPE     THICKNESS  ',
     +          'HYD. COND.   WAT.CONT.   WAT.CONT.   ',
     +          'WAT.CONT.    EPSILON    HYD. COND.', /3X, 151('-'))
      ELSE IF ( Isfropt.EQ.4 ) THEN
        WRITE (Iout, 9001)
      ELSE IF ( Isfropt.EQ.5 ) THEN
        WRITE (Iout, 9001)
      END IF
      DO ii = 1, Nstrm
        IF ( Isfropt.EQ.0 ) THEN
          READ (In, *) krch, irch, jrch, jseg, ireach, Strm(1, ii)
        ELSE IF ( Isfropt.EQ.1 ) THEN
          READ (In, *) krch, irch, jrch, jseg, ireach, Strm(1, ii),
     +                 Strm(3, ii), Strm(2, ii), Strm(8, ii),
     +                 Strm(6, ii)
          Strm(4, ii) = Strm(3, ii) - Strm(8, ii)
          IF ( Strm(2, ii).LE.0.0 ) THEN
            WRITE (Iout, 9007) jseg, ireach
            CALL USTOP(' ')
          END IF
        ELSE IF ( Isfropt.EQ.2 ) THEN
          READ (In, *) krch, irch, jrch, jseg, ireach, Strm(1, ii),
     +                 Strm(3, ii), Strm(2, ii), Strm(8, ii),
     +                 Strm(6, ii), Thts(ii), Thti(ii), Eps(ii)
          Strm(4, ii) = Strm(3, ii) - Strm(8, ii)
          IF ( Strm(2, ii).LE.0.0 ) THEN
            WRITE (Iout, 9007) jseg, ireach
            CALL USTOP(' ')
          END IF
        ELSE IF ( Isfropt.EQ.3 ) THEN
          READ (In, *) krch, irch, jrch, jseg, ireach, Strm(1, ii),
     +                 Strm(3, ii), Strm(2, ii), Strm(8, ii),
     +                 Strm(6, ii), Thts(ii), Thti(ii), Eps(ii), Uhc(ii)
          Strm(4, ii) = Strm(3, ii) - Strm(8, ii)
          IF ( Strm(2, ii).LE.0.0 ) THEN
            WRITE (Iout, 9007) jseg, ireach
            CALL USTOP(' ')
          END IF
        ELSE IF ( Isfropt.EQ.4 .OR. Isfropt.EQ.5 ) THEN
          READ (In, *) krch, irch, jrch, jseg, ireach, Strm(1, ii)
        END IF
 9007   FORMAT (//1X, '***ERROR***  SLOPE IS SPECIFIED LESS THAN OR ',
     +          'EQUAL TO ZERO FOR SEGMENT ', I7, ' REACH ', I7, /1X,
     +          'PROGRAM IS STOPPING')
        IF ( Iuzt.EQ.1 ) THEN
          IF ( Strt(jrch, irch, krch).LE.Botm(jrch, irch, krch) ) THEN
            WRITE (Iout, 9008) ireach, jseg
 9008       FORMAT (5X, '**WARNING** CELL BENEATH STREAM REACH ',
     +              'IS INACTIVE', /5X, 'CELL BELOW MUST HAVE ',
     +              'THE SAME SPECIFIC YIELD', /5X, 'AND THE ',
     +              'TOP ELEVATION OF ACTIVE CELL MUST ',
     +              'EQUAL BOTTOM OF INACTIVE CELL', /5X,
     +              'INACTIVE CELL IS BELOW STREAM REACH',
     +              'AND SEGMENT:', 2I5)
          END IF
        END IF
C
C3------CALCULATE RESIDUAL WATER CONTENT FROM SATURATED WATER CONTENT
C         AND SPECIFIC YIELD WHEN UNSATURATED FLOW IS ACTIVE.
        IF ( Itrss.EQ.1 ) THEN
          IF ( Isfropt.EQ.2 .OR. Isfropt.EQ.3 ) THEN
            IF ( Iunitbc6.EQ.0 ) THEN
              Thtr(ii) = Thts(ii) - Sy(jrch, irch, krch)
     +                   /(Delr(jrch)*Delc(irch))
            ELSE IF ( LAYCON(krch).EQ.1 ) THEN
              Thtr(ii) = Thts(ii) - Sc1(jrch, irch, krch)
     +                   /(Delr(jrch)*Delc(irch))
            ELSE
              Thtr(ii) = Thts(ii) - Sy(jrch, irch, krch)
     +                   /(Delr(jrch)*Delc(irch))
            END IF
          END IF
        ELSEIF ( Isfropt.EQ.2 .OR. Isfropt.EQ.3 ) THEN
          Thtr(ii) = 0.0
        END IF
        IF ( Isfropt.EQ.0 ) THEN
          WRITE (Iout, 9009) krch, irch, jrch, jseg, ireach, Strm(1, ii)
        ELSE IF ( Isfropt.EQ.1 ) THEN
          WRITE (Iout, 9010) krch, irch, jrch, jseg, ireach, Strm(1, ii)
     +                       , Strm(3, ii), Strm(2, ii), Strm(8, ii),
     +                       Strm(6, ii)
        ELSE IF ( Isfropt.EQ.2 ) THEN
          WRITE (Iout, 9011) krch, irch, jrch, jseg, ireach, Strm(1, ii)
     +                       , Strm(3, ii), Strm(2, ii), Strm(8, ii),
     +                       Strm(6, ii), Thts(ii), Thti(ii), Thtr(ii),
     +                       Eps(ii)
        ELSE IF ( Isfropt.EQ.3 ) THEN
          WRITE (Iout, 9013) krch, irch, jrch, jseg, ireach, Strm(1, ii)
     +                       , Strm(3, ii), Strm(2, ii), Strm(8, ii),
     +                       Strm(6, ii), Thts(ii), Thti(ii), Thtr(ii),
     +                       Eps(ii), Uhc(ii)
        ELSE IF ( Isfropt.EQ.4 .OR. Isfropt.EQ.5 ) THEN
          WRITE (Iout, 9014) krch, irch, jrch, jseg, ireach, Strm(1, ii)
        END IF
 9009   FORMAT (2X, I5, 2I7, I8, I9, 3X, 1PE11.4)
 9010   FORMAT (2X, I6, 2I7, I8, I9, 3X, 1PE11.4, 2X, 1PE11.4, 2X,
     +          1PE11.4, 2X, 1PE11.4, 2X, 1PE11.4)
 9011   FORMAT (3(1X, I5), 1X, I5, 3X, I5, 1X, 5(1X, 1PE11.4),
     +          3(1X, 0PE11.4), 1(1X, 1PE11.4))
 9013   FORMAT (3(1X, I5), 1X, I5, 3X, I5, 1X, 5(1X, 1PE11.4),
     +          3(1X, 0PE11.4), 2(1X, 1PE11.4))
 9014   FORMAT (3(1X, I5), 1X, I5, 3X, I5, 1X, 1PE11.4)
C
C4------CHECK RANGE AND ORDER FOR SEGMENTS AND REACHES.
        IF ( jseg.LE.0 .OR. jseg.GT.Nss ) THEN
          WRITE (Iout, 9015)
 9015     FORMAT (' SEGMENT MUST BE GREATER THAN 0 AND LESS THAN NSS')
          CALL USTOP(' ')
        END IF
        IF ( jseg.NE.nseg ) THEN
          nseg = nseg + 1
          nreach = 0
          IF ( jseg.NE.nseg ) THEN
            WRITE (Iout, 9016)
 9016       FORMAT (' SEGMENTS MUST BE IN ORDER FROM 1 THROUGH NSS')
            CALL USTOP(' ')
          END IF
        END IF
        nreach = nreach + 1
        IF ( ireach.NE.nreach ) THEN
          WRITE (Iout, 9017)
 9017     FORMAT (' EACH SEGMENT MUST START WITH REACH 1, AND', /,
     +            ' REACHES MUST BE NUMBERED CONSECUTIVELY')
          CALL USTOP(' ')
        END IF
        Istrm(1, ii) = krch
        Istrm(2, ii) = irch
        Istrm(3, ii) = jrch
        Istrm(4, ii) = jseg
        Istrm(5, ii) = ireach
        Seg(1, Istrm(4, ii)) = Seg(1, Istrm(4, ii)) + Strm(1, ii)
        Iseg(4, jseg) = ireach
      END DO
C
C5------READ SEGMENT INFORMATION FOR FIRST RECHARGE PERIOD. 7/22/2007 dep
Cdep    Revised to read segment data for first stress period when
Cdep    unsaturated flow beneath streams is active 
      IF ( ISFROPT.GT.1 .AND. NSFRPAR.EQ.0 ) THEN
        READ (In, *) ITMP, IRDFLG, IPTFLG
        NP = 0
        nlst = NSS
        lb = 1
        ichk = 1
        CALL SGWF1SFR2RDSEG(nlst, lb, In, Iout, Seg, Iseg, Idivar,
     +                    Iotsg, Maxpts, Xsec, Qstage, I15, Concq,
     +                    Concrun, Concppt, Nsol, Nsegdim, Nsegck, Nss,
     +                    ichk, Nss, Isfropt, 1)

      ELSEIF ( ISFROPT.GT.0 .AND. NSFRPAR.GT.0 ) THEN
        WRITE (IOUT, 9050)
        CALL USTOP(' ')
      END IF
 9050 FORMAT (//, '  ***  ERROR- CANNOT HAVE ISFROPT ACTIVE (NEGATIVE', 
     +        ' NSTRM), WHEN USING PARAMETERS; CODE STOPPING ***')
C6------COMPUTE UNSATURATED VARIABLE WHEN SPECIFIED BY SEGMENT.
      IF ( Iuzt.EQ.1 ) THEN
        irch = 1
        ksfropt = 0
        DO nseg = 1, Nss
          icalc = Iseg(1, nseg)
          seglen = Seg(1, nseg)
          sumlen = 0.0
          IF ( icalc.EQ.1 .OR. icalc.EQ.2 ) THEN
            IF ( Isfropt.EQ.4 .OR. Isfropt.EQ.5 ) THEN
              ksfropt = 1
              thsslpe = (Seg(18, nseg)-Seg(22, nseg))/seglen
              thislpe = (Seg(19, nseg)-Seg(23, nseg))/seglen
              epsslpe = (Seg(20, nseg)-Seg(24, nseg))/seglen
              IF ( Isfropt.EQ.5 )
     +             uhcslpe = (Seg(21, nseg)-Seg(25, nseg))/seglen
            END IF
          END IF
          DO ii = 1, Iseg(4, nseg)
            IF ( icalc.EQ.1 .OR. icalc.EQ.2 ) THEN
              krck = Istrm(1, irch)
              irck = Istrm(2, irch)
              jrck = Istrm(3, irch)
              rchlen = Strm(1, irch)
              dist = sumlen + (0.5*rchlen)
              IF ( ksfropt.EQ.1 ) THEN
Crevised 4/21/2006
                Thts(irch) = Seg(18, nseg) - (thsslpe*dist)
                Thti(irch) = Seg(19, nseg) - (thislpe*dist)
                Eps(irch) = Seg(20, nseg) - (epsslpe*dist)
                IF ( Isfropt.EQ.5 ) Uhc(irch) = Seg(21, nseg)
     +               - (uhcslpe*dist)
              END IF
C
C7------CALCULATE RESIDUAL WATER CONTENT FROM SATURATED WATER CONTENT
C        AND SPECIFIC YIELD WHEN UNSATURATED FLOW IS ACTIVE.
Crevised 4/21/2006
              IF ( Itrss.EQ.1 ) THEN
                IF ( Isfropt.EQ.4 .OR. Isfropt.EQ.5 ) THEN
                  IF ( Iunitbc6.EQ.0 ) THEN
                    Thtr(irch) = Thts(irch) - Sy(jrck, irck, krck)
     +                         /(Delr(jrck)*Delc(irck))
                  ELSE IF ( LAYCON(krck).EQ.1 ) THEN
                    Thtr(irch) = Thts(irch) - Sc1(jrck, irck, krck)
     +                        /(Delr(jrck)*Delc(irck))
                  ELSE
                    Thtr(irch) = Thts(irch) - Sy(jrck, irck, krck)
     +                         /(Delr(jrck)*Delc(irck))
                  END IF
                END IF
              ELSEIF ( Isfropt.EQ.4 .OR. Isfropt.EQ.5 ) THEN
                Thtr(irch) = 0.0
              END IF
C
C8------CHECK THAT RESIDUAL WATER CONTENT IS LESS THAN SATURATED
C        WATER CONTENT.
Crevised 4/21/2006
              IF ( Iuzt.EQ.1 ) THEN
                IF ( Thtr(irch).GE.Thts(irch) ) THEN
                  WRITE (Iout, 9018)
                  CALL USTOP(' ')
                END IF
                IF ( Thti(irch).GT.Thts(irch) ) THEN
                  WRITE (Iout, 9019)
                  CALL USTOP(' ')
                END IF
Cdep  Added check that THTI is greater than THTR.
                IF ( THTI(irch).LT.THTR(irch) ) THEN
                  WRITE (IOUT, 9020)ISTRM(4,irch), ISTRM(5,irch),
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
 9018 FORMAT (' RESIDUAL WATER CONTENT IS EQUAL OR GREATER THAN ',
     +        'SATURATED WATER CONTENT. CHECK INPUT DATA FOR SPECIFIC',
     +        ' YIELD AND SATURATED WATER CONTENT')
 9019 FORMAT (' INITIAL WATER CONTENT IS GREATER THAN SATURATED ',
     +        'WATER CONTENT. CHECK INPUT DATA')
 9020 FORMAT (' INITIAL WATER CONTENT IS LESS THAN RESIDUAL ',
     +        'WATER CONTENT FOR STREAM SEGMENT: ',I5,' REACH: ',I5,
     +        ' INITIAL WATER CONTENT RESET TO RESIDUAL OF ',E12.5) 
C
C9------CHECK IF STREAM REACH IS IN ACTIVE CELL.
      kkptflg = 0
      ichk = 1
      DO WHILE ( ichk.LE.Nstrm )
        krck = Istrm(1, ichk)
        irck = Istrm(2, ichk)
        jrck = Istrm(3, ichk)
        jsegck = Istrm(4, ichk)
        ireachck = Istrm(5, ichk)
        IF ( Ibound(jrck, irck, krck).EQ.0 ) THEN
          kkptflg = kkptflg + 1
          IF ( kkptflg.EQ.1 ) THEN
            WRITE (Iout, 9021) jsegck, ireachck,
     +                         Ibound(jrck, irck, krck), krck, irck,
     +                         jrck
 9021       FORMAT (/1X, '*** WARNING *** FIRST OCCURRENCE WHERE A ',
     +              'STREAM REACH IS ASSIGNED TO AN INACTIVE CELL ',
     +              'IS SEGMENT ', I4, ' REACH NO.', I4, /1X,
     +              ' IBOUND ARRAY VALUE IS ', I4, ' AT LAYER ', I4,
     +              '; ROW ', I4, '; COLUMN ', I4, '.')
          END IF
        ELSE IF ( Ibound(jrck, irck, krck).LT.0 ) THEN
          WRITE (Iout, 9022) jsegck, ireachck, Ibound(jrck, irck, krck),
     +                       krck, irck, jrck
 9022     FORMAT (/1X, '*** WARNING *** STREAM SEGMENT ', I4,
     +            ' REACH NO. ', I4, ' IS CONNECTED TO A CONSTANT ',
     +            'HEAD CELL.'/1X, 'IBOUND ARRAY VALUE IS ', I4, ' AT ',
     +            'LAYER ', I4, '; ROW ', I4, '; COLUMN ', I4, '.', /1X,
     +            'NO STREAM LEAKAGE WILL BE ALLOWED-- SUGGEST ',
     +            'REMOVING STREAM REACH FROM CELL OR CHANGE CELL ',
     +            'TO VARIABLE HEAD.'/)
        END IF
        ichk = ichk + 1
      END DO
      IF ( kkptflg.EQ.1 ) THEN
        WRITE (Iout, 9023)
 9023   FORMAT (/1X, '*** WARNING *** ONLY 1 STREAM REACH WAS ',
     +          'ASSIGNED TO A CELL WHERE THE IBOUND ARRAY ',
     +          'WAS ZERO.'/1X, 'PROGRAM SEARCHES FOR UPPERMOST ',
     +          'ACTIVE CELL IN VERTICAL COLUMN,IF ALL CELLS ',
     +          'ARE INACTIVE, STREAM LEAKAGE WILL NOT BE ',
     +          'ALLOWED. '/)
      ELSE IF ( kkptflg.GT.1 ) THEN
        WRITE (Iout, 9024) kkptflg
 9024   FORMAT (/1X, '*** WARNING *** A TOTAL OF ', I5,
     +          'STREAM REACHES ',
     +          'WERE ASSIGNED TO CELLS WHERE THE IBOUND ARRAY ',
     +          'WAS ZERO.'/1X, 'PROGRAM SEARCHES FOR UPPERMOST ',
     +          'ACTIVE CELL IN VERTICAL COLUMN FOR ALL ',
     +          'OCCURRENCES.'/1X, 'IF ALL CELLS IN A VERTICAL ',
     +          'COLUMN ARE INACTIVE, STREAM LEAKAGE WILL NOT BE ',
     +          'ALLOWED FOR ASSOCIATED STREAM REACH. '/)
C
      END IF
C
C10-----READ PARAMETER DEFINITIONS.
Cdep  Changed do while loops to do loops 7/22/2007
      IF ( Nsfrpar.GT.0 ) THEN
        lstsum = Nss + 1
        DO ii= 1, Nsfrpar
          lstbeg = lstsum
          CALL UPARLSTRP(lstsum, Nsegdim, In, Iout, ip, 'SFR', 'SFR',
     +                   Iterp, numinst, Inamloc)
          nlst = lstsum - lstbeg
          IF ( numinst.GT.1 ) nlst = nlst/numinst
C11-----ASSIGN STARTING INDEX FOR READING INSTANCES.
          IF ( numinst.EQ.0 ) THEN
            ib = 0
          ELSE
            ib = 1
          END IF
C12-----READ LIST(S) OF CELLS, PRECEDED BY INSTANCE NAME IF NUMINST>0.
Cdep  Revised to change ib loop counter
          lb = lstbeg
          DO i = ib, numinst
            IF ( i.GT.0 ) CALL UINSRP(i, In, Iout, ip, Iterp)
            ichk = 0
            CALL SGWF1SFR2RDSEG(nlst, lb, In, Iout, Seg, Iseg, Idivar,
     +                          Iotsg, Maxpts, Xsec, Qstage, I15, Concq,
     +                          Concrun, Concppt, Nsol, Nsegdim, idum,
     +                          1, ichk, Nss, Isfropt, 1)
            CALL SGWF1SFR2PRSEG(nlst, lb, Iout, Seg, Iseg, Idivar,
     +                          Iotsg, Maxpts, Xsec, Qstage, I15, Concq,
     +                          Concrun, Concppt, Nsol, Nsegdim, Iouts,
     +                          Isfropt, 1)
            lb = lb + nlst
          END DO
        END DO
      END IF
      WRITE (Iout, 9025)
 9025 FORMAT (//)
C
C13-----INITIALIZE VARIABLES AND LISTS FOR UNSATURATED FLOW BENEATH
C        STREAM. NWAVS INITIALLY SET TO 1.
      IF ( Iuzt.EQ.1 ) THEN
        DO kk = 1, Nstrm
          Foldflbt(kk) = 0.0D0
          DO jj = 17, 23
            Strm(jj, kk) = 0.0
          END DO
        END DO
        juzst = 1
        DO WHILE ( juzst.LE.Nuzst )
          DO i = 1, Isuzn
            Uzflwt(juzst, i) = 0.0D0
            Uzstor(juzst, i) = 0.0D0
            Delstor(juzst, i) = 0.0D0
            Uzwdth(juzst, i) = 0.0D0
            Uzseep(juzst, i) = 0.0D0
            Nwavst(juzst, i) = 1
            Uzolsflx(juzst, i) = 0.0D0
          END DO
          itr = 1
          DO WHILE ( itr.LE.Nstotrl )
            Uzdpit(juzst, itr) = 0.0D0
            Uzthit(juzst, itr) = 0.0D0
            Uzspit(juzst, itr) = 0.0D0
            Uzflit(juzst, itr) = 0.0D0
            Uzdpst(juzst, itr) = 0.0D0
            Uzthst(juzst, itr) = 0.0D0
            Uzspst(juzst, itr) = 0.0D0
            Uzflst(juzst, itr) = 0.0D0
            Ltrlit(juzst, itr) = 0
            Itrlit(juzst, itr) = 0
            Ltrlst(juzst, itr) = 0
            Itrlst(juzst, itr) = 0
            Itrlsth(itr) = 0
            itr = itr + 1
          END DO
          juzst = juzst + 1
        END DO
      END IF
C
C14-----RETURN
      RETURN
      END SUBROUTINE GWF1SFR2RPP
C
C-------SUBROUTINE GWF1SFR2UHC
      SUBROUTINE GWF1SFR2UHC(Istrm, Nstrm, Uhc, Hks, Vks, Ibound, Nrow,
     +                       Ncol, Nlay, Nuzst, Iout)
C     ******************************************************************
C     SETS UNSATURATED VERTICAL HYDRAULIC CONDUCTIVITY TO VERTICAL
C     HYDRAULIC CONDUCTIVITY IN THE LAYER-PROPERTY FLOW PACKAGE.
C     VERSION  2.7: MARCH 16, 2009
C     ******************************************************************
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL Hks, Uhc, Vks
      INTEGER Ibound, Iout, Istrm, Ncol, Nlay, Nrow, Nstrm, Nuzst
      DIMENSION Istrm(5, Nstrm), Ibound(Ncol, Nrow, Nlay), Uhc(Nuzst),
     +          Vks(Ncol, Nrow, Nlay), Hks(Ncol, Nrow, Nlay)
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      INTEGER ichk, irck, jrck, krck
C     ------------------------------------------------------------------
C     COMMON VARIABLES
C     ------------------------------------------------------------------
C     COMMON FROM LPF PACKAGE.
      REAL CHANI
      INTEGER LAYAVG, LAYTYP, LAYVKA, LAYWET
      COMMON /LPFCOM/ LAYTYP(999), LAYAVG(999), CHANI(999), LAYVKA(999),
     +                LAYWET(999)
C
C     ******************************************************************
C
C1------CHECK TO SEE IF STREAM IS IN ACTIVE CELL AND SET UHC EQUAL TO
C        VKA FOR CORRESPONDING MODEL CELL.
      ichk = 1
      DO WHILE ( ichk.LE.Nstrm )
        krck = Istrm(1, ichk)
        irck = Istrm(2, ichk)
        jrck = Istrm(3, ichk)
        IF ( Ibound(jrck, irck, krck).GT.0 ) THEN
          IF ( LAYVKA(krck).EQ.0 ) THEN
            Uhc(ichk) = Vks(jrck, irck, krck)
          ELSE
            Uhc(ichk) = Vks(jrck, irck, krck)*Hks(jrck, irck, krck)
          END IF
          IF ( LAYTYP(krck).LE.0 ) THEN
            WRITE (Iout, *) 'PROGRAM TERMINATED-LAYTYP MUST BE GREATER',
     +                      ' THAN ZERO WHEN ISFROPT IS 2 OR 4.'
            CALL USTOP(' ')
          END IF
        END IF
        ichk = ichk + 1
      END DO
C
C2------RETURN
      RETURN
      END SUBROUTINE GWF1SFR2UHC
C
C-------SUBROUTINE GWF1SFR2RPS
      SUBROUTINE GWF1SFR2RPS(Strm, Istrm, Kkper, Nstrm, In, Iout, Seg,
     +                       Iseg, Nsegck, Nss, Idivar, Iotsg, Maxpts,
     +                       Iptflg, Xsec, Qstage, I15, Concq, Concrun,
     +                       Concppt, Nsol, Iouts, Nsfrpar, Nsegdim,
     +                       Uzthst, Uzflst, Uzdpst, Uzspst, Uzolsflx,
     +                       Thti, Thtr, Thts, Eps, Isfropt, Iuzt,
     +                       Isuzn, Ncol, Nrow, Nlay, Nuzst, Nstotrl,
     +                       Uzseep, Uzstor, Uhc, Dpthinc, Wetper,
     +                       Nuzrow, Nuzcol, Nwavst, Hnew, Ibound,
     +                       Issflg, Nper, Itmp, Irdflg, Np, Botm,
     +                       Nbotm,  Const, Iunitlak, Nlakesar, 
     +                       Nsslk, Slkotflw, Dlkotflw, Dlkstage)
C     ******************************************************************
C     READ STREAM DATA FOR STRESS PERIOD
C     VERSION  2.7: MARCH 16, 2009
C     ******************************************************************
C
      IMPLICIT NONE
      INCLUDE 'param.inc'

C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      DOUBLE PRECISION Eps, Hnew, Thti, Thtr, Thts, Uzdpst, Uzflst,
     +                 Uzolsflx, Uzseep, Uzspst, Uzstor, Uzthst, Wetper
Cdep  Assign the three new arrays needed for computing Lake Outflow
      DOUBLE PRECISION Slkotflw, Dlkotflw, Dlkstage
      REAL Botm, Concppt, Concq, Concrun, Dpthinc, Qstage, Seg, Strm,
     +     Uhc, Xsec, Const
      INTEGER I15, Ibound, Idivar, In, Iotsg, Iout, Iouts, Iptflg,
     +        Irdflg, Iseg, Isfropt, Issflg, Istrm, Isuzn, Iuzt, Itmp,
     +        Iunitlak, Kkper 
Cdep  Added Nlakesar to INTEGER list
      INTEGER Maxpts, Nbotm, Ncol, Nlay, Np, Nper, Nrow, Nsegck,
     +        Nsegdim, Nsfrpar, Nsol, Nss, Nstotrl, Nstrm, Nuzcol,
     +        Nuzrow, Nuzst, Nwavst, Nlakesar, Nsslk
      DIMENSION Ibound(Ncol, Nrow, Nlay), Hnew(Ncol, Nrow, Nlay),
     +          Botm(Ncol, Nrow, 0:Nbotm), Issflg(Nper)
      DIMENSION Strm(24, Nstrm), Istrm(5, Nstrm), Seg(26, Nsegdim),
     +          Nsegck(Nss), Iseg(4, Nsegdim), Iotsg(Nsegdim),
     +          Idivar(2, Nsegdim), Xsec(16, Nsegdim),
     +          Qstage(Maxpts, Nsegdim)
      DIMENSION Concq(Nsegdim, Nsol), Concrun(Nsegdim, Nsol),
     +          Concppt(Nsegdim, Nsol)
      DIMENSION Uzspst(Nuzst, Nstotrl), Uzflst(Nuzst, Nstotrl),
     +          Uzthst(Nuzst, Nstotrl), Uzdpst(Nuzst, Nstotrl)
      DIMENSION Eps(Nuzst), Thti(Nuzst), Thts(Nuzst), Thtr(Nuzst),
     +          Uhc(Nuzst)
      DIMENSION Dpthinc(Nuzst, Isuzn), Nwavst(Nuzst, Isuzn),
     +          Wetper(Nuzst, Isuzn), Uzolsflx(Nuzst, Isuzn),
     +          Uzseep(Nuzst, Isuzn), Uzstor(Nuzst, Isuzn)
Cdep  Dimension three new arrays for computing Lake Outflow
      DIMENSION Slkotflw(200, Nsslk), Dlkotflw(200, Nsslk),
     +          Dlkstage(200, Nsslk)
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      DOUBLE PRECISION h
      REAL avdpth, avhc, avthk, bottom, dist, dndiff, dpslpe, dpth1,
     +     dpth2, dpthlw, eldn, elslpe, etsw, flw1, flw2, flwlw, hcslpe,
     +     pptsw, rchlen, rough, roughbnk, roughch, runoff, sbot,
     +     seglen, strlen, sumlen, thkslpe, top, updiff, wdslpe, wdth1,
     +     wdth2, wdthlw, width, CLOSEZERO
      INTEGER i, ic, icalc, ichk, icp, iflginit, ii, ik, il, ilay, ip,
     +        ipt, ir, irch, irp, isoptflg, iss, istep, istsg, iwvcnt, 
     +        jj, jk, k5, k6, k7, kk, ksfropt, kss, ktot, l, lstbeg, 
     +        nseg, nstrpts
C
C     ------------------------------------------------------------------
C
C
C1------READ ITMP FLAG TO REUSE NON-PARAMETER DATA, 2 PRINTING FLAGS,
C         AND NUMBER OF PARAMETERS BEING USED IN CURRENT STRESS PERIOD.
      iss = ISSFLG(Kkper)
      CLOSEZERO = 1.0E-7
Cdep added NSFRPAR to IF statement  (July 22, 2007)
      IF ( ISFROPT.GT.1 .AND. Kkper.GT.1 ) THEN
          READ (In, *) ITMP, IRDFLG, IPTFLG
          NP = 0
      ELSE IF ( ISFROPT.LE.1 ) THEN
        IF ( NSFRPAR.EQ.0 ) THEN
          READ (In, *) ITMP, IRDFLG, IPTFLG
          NP = 0 
        ELSE
          READ (In, *) ITMP, IRDFLG, IPTFLG, NP
        END IF
      END IF
C
C2------CHECK FOR TOO MANY SEGMENTS.
      IF ( Itmp.GT.Nss ) THEN
        WRITE (Iout, 9001)
 9001   FORMAT (/1X, 'CANNOT SPECIFY MORE THAN NSS STREAM SEGMENTS')
        CALL USTOP(' ')
      END IF
C
C3------REUSE NON-PARAMETER DATA FROM LAST STRESS PERIOD IF ITMP<0.
      IF ( Itmp.GE.0 ) THEN
C
C4------NOT REUSING DATA. INITIALIZE NSEGCK LIST TO ZERO FOR ALL
C         SEGMENTS.
        IF ( Kkper.GT.1 ) THEN
          DO kss = 1, Nss
            Nsegck(kss) = 0
          END DO
        END IF
      ELSE IF ( Kkper.EQ.1 ) THEN
        WRITE (Iout, 9002)
 9002   FORMAT (//1X, ' ***  STREAM SEGMENTS MUST BE DEFINED FOR ',
     +          'FIRST STRESS PERIOD; CODE STOPPING ***')
        CALL USTOP(' ')
      ELSE IF ( Nsfrpar.EQ.0 .AND. Iuzt.EQ.0 ) THEN
        WRITE (Iout, 9003)
 9003   FORMAT (/1X,
     +          'REUSING STREAM SEGMENT DATA FROM LAST STRESS PERIOD')
        RETURN
      ELSE IF ( Nsfrpar.NE.0 ) THEN
C
C5------INITIALIZE NSEGCK TO 0 FOR SEGMENTS THAT ARE DEFINED BY
C         CURRENTLY USED PARAMETERS.
        WRITE (Iout, 9003)
        DO ip = 1, MXPAR
          IF ( PARTYP(ip).EQ.'SFR' .AND. IACTIVE(ip).GT.0 ) THEN
            ic = IPLOC(1, ip)
            DO WHILE ( ic.LE.IPLOC(2, ip) )
              Nsegck(Iseg(3, ic)) = 0
              ic = ic + 1
            END DO
          END IF
        END DO
      END IF
C
C6------READ NON-PARAMETER STREAM SEGMENT DATA.
      IF ( Itmp.GT.0 ) THEN
        lstbeg = 1
        ichk = 1
Cdep 7/22/07 changed logic for reading stream segment data
        IF ( ISFROPT.GT.1 .AND. NSFRPAR.EQ.0  ) THEN
           IF ( Kkper.GT.1 ) CALL SGWF1SFR2RDSEG(Itmp, lstbeg, In, Iout,
     +         Seg, Iseg, Idivar, Iotsg, Maxpts, Xsec, Qstage, I15, 
     +         Concq,Concrun, Concppt, Nsol, Nsegdim, Nsegck, Nss, ichk,
     +          Nss,Isfropt, Kkper)
        ELSE
           CALL SGWF1SFR2RDSEG(Itmp, lstbeg, In, Iout,
     +         Seg, Iseg, Idivar, Iotsg, Maxpts, Xsec, Qstage, I15,
     +         Concq,Concrun, Concppt, Nsol, Nsegdim, Nsegck, Nss, ichk,
     +          Nss,Isfropt, Kkper)
        END IF
      END IF
C
C7------DEACTIVATE ANY PREVIOUSLY USED STREAM PARAMETERS, AND ACTIVATE
C         PARAMETERS BEING USED IN CURRENT STRESS PERIOD.
      IF ( Nsfrpar.NE.0 ) THEN
        CALL PRESET('SFR')
        DO jj = 1, Np
          CALL SGWF1SFR2PARMOV(In, Iout, Seg, Iseg, Idivar, Iotsg,
     +                         Maxpts, Xsec, Qstage, I15, Concq,
     +                         Concrun, Concppt, Nsol, Nsegdim, Nsegck,
     +                         Nss)
        END DO
      END IF
C
C8------CHECK FOR ERRORS IN SEGMENT DATA.
      IF ( Itmp.GT.0 .OR. Nsfrpar.NE.0 ) THEN
        DO nseg = 1, Nss
          IF ( Isfropt.EQ.0 ) THEN
            IF ( Nsegck(nseg).LE.0 .AND. Kkper.EQ.1 ) THEN
              WRITE (Iout, 9004) nseg
 9004         FORMAT (/5X, '*** WARNING ***  INPUT DATA FOR ',
     +                'SEGMENT ', I6, ' WERE NOT DEFINED')
            ELSE IF ( Nsegck(nseg).GT.1 ) THEN
              WRITE (Iout, 9005) nseg, Nsegck(nseg)
 9005         FORMAT (/5X, '*** ERROR ***  DATA FOR SEGMENT', I6,
     +                ' WERE DEFINED ', I2, ' TIMES (INSTEAD OF ',
     +                'ONCE)')
              CALL USTOP(' ')
            END IF
          END IF
C
C9------READ DATA ACCORDING TO VARIABLE ISFROPT.
          isoptflg = 0
          IF ( Isfropt.EQ.1 .OR. Isfropt.EQ.2 .OR. Isfropt.EQ.3 )
     +         isoptflg = 1
          IF ( isoptflg.EQ.0 .AND. (Seg(8,nseg).LE.Seg(13,nseg)) ) THEN
            WRITE (Iout, 9006) nseg
 9006       FORMAT (/5X, '*** WARNING *** UPSTREAM ELEVATION IS ',
     +              'EQUAL TO OR LOWER THAN DOWNSTREAM ELEVATION FOR ',
     +              'SEGMENT No. ', I6)
            IF ( Iseg(1, nseg).EQ.1 .OR. Iseg(1, nseg).EQ.2 ) THEN
              WRITE (Iout, 9007) nseg, Iseg(1, nseg)
 9007         FORMAT (/5X, '*** ERROR ***  ',
     +                'SLOPE IS ZERO OR NEGATIVE FOR SEGMENT No.', I5,
     +                '   SLOPE MUST BE POSITIVE WHEN ICALC IS', I3)
              CALL USTOP(' ')
            END IF
          END IF
          IF ( Idivar(2, nseg).GT.0 ) THEN
            WRITE (Iout, 9008) nseg
 9008       FORMAT (/5X, '*** WARNING *** IPRIOR > 0 FOR NSEG = ', I6,
     +              /10X, 'THIS OPTION NOT YET AVAILABLE; CODE WILL ',
     +              'ASSUME IPRIOR = 0'/)
            Idivar(2, nseg) = 0
          ELSE IF ( Idivar(2, nseg).LT.-3 ) THEN
            WRITE (Iout, 9009) nseg
 9009       FORMAT (/5X, '*** WARNING *** IPRIOR < -3 FOR NSEG = ', I6,
     +              /10X, 'THIS VALUE IS OUT OF RANGE; CODE WILL ',
     +              'ASSUME IPRIOR = 0'/)
            Idivar(2, nseg) = 0
          ELSE IF ( Idivar(2, nseg).EQ.-2 ) THEN
            IF ( Seg(2, nseg).LT.0.0 .OR. Seg(2, nseg).GT.1.0 ) THEN
              WRITE (Iout, 9010) nseg
 9010         FORMAT (/5X, '*** WARNING *** IPRIOR = -2 FOR NSEG = ',
     +                I6, ' & FLOW VALUE IS OUT OF RANGE (.0 - 1.);',
     +                /10X, 'ASSUME NO DIVERSION OF FLOW'/)
              Seg(2, nseg) = 0.0
            END IF
          END IF
        END DO
C
C10-----PLACE STREAM SEGMENT IDENTITY NUMBERS IN ISEG ARRAY.
C         5 ASSIGNED TO SEGMENTS NOT RECEIVING TRIBUTARY FLOW.
C         6 ASSINGED TO SEGMENTS THAT DIVERT FLOW.
C         7 ASSIGNED TO SEGMENTS RECEIVING TRIBUTARY FLOW.
        k5 = 0
        k6 = 0
        k7 = 0
        DO nseg = 1, Nss
C
C11-----IDENTIFY SEGMENTS THAT DIVERT FLOW.
          IF ( Idivar(1, nseg).NE.0 ) THEN
            Iseg(3, nseg) = 6
            k6 = k6 + 1
C
C12-----IDENTIFY SEGMENTS THAT DO NOT DIVERT FLOW.
          ELSE
            jj = 0
C
C13-----IDENTIFY SEGMENTS THAT RECEIVE TRIBUTARY FLOW.
            DO ii = 1, Nss
              IF ( Iotsg(ii).EQ.nseg ) jj = 1
            END DO
C
C14-----IDENTIFY SEGMENTS THAT DO NOT RECEIVE TRIBUTARY FLOW.
            IF ( jj.EQ.0 ) THEN
              Iseg(3, nseg) = 5
              k5 = k5 + 1
            ELSE
              Iseg(3, nseg) = 7
              k7 = k7 + 1
              IF ( jj.NE.1 ) WRITE (Iout, 9011) nseg, jj
 9011         FORMAT (//5X, '*** WARNING *** ERROR WHILE ',
     +                'CLASSIFYING SEGMENTS:   NSEG =', I6, 4X, 'JJ =',
     +                I6//)
            END IF
          END IF
        END DO
C
C15-----TALLY DIFFERENT STREAM SEGMENT TYPES.
        ktot = k5 + k6 + k7
        WRITE (Iout, 9012) k5, k6, k7
 9012   FORMAT (///1X, 'CLASSIFICATION & COUNT OF STREAM SEGMENTS ',
     +          'BASED ON SOURCE OF INFLOW:'//16X,
     +          'HEADWATER     DIVERSION     ',
     +          'RECEIVES TRIBUTARY FLOW'/16X,
     +          '---------     ---------    ',
     +          ' -----------------------'/16X, I6, 9X, I6, 10X, I6/)
C
C16-----PRINT WARNING IF TALLIED SEGMENTS LESS THAN NSS.
        IF ( ktot.NE.Nss ) THEN
          WRITE (Iout, 9013) ktot, Nss
 9013     FORMAT (/5X, '*** WARNING ***  INTERNAL ERROR SUMMING ',
     +            'TYPES OF STREAM SEGMENTS:  NSEG =', I6, 5X, 'JJ =',
     +            I6//)
          CALL USTOP(' ')
        END IF
C
C17-----PRINT INPUT DATA IF IRDFLG IS ZERO.
C         SKIP IF INPUT READ BY REACHES (ISFROPT = 1, 3, OR 5).
        IF ( Irdflg.LE.0 ) CALL SGWF1SFR2PRSEG(Nss, 1, Iout, Seg, Iseg,
     +       Idivar, Iotsg, Maxpts, Xsec, Qstage, I15, Concq, Concrun,
     +       Concppt, Nsol, Nsegdim, Iouts, Isfropt, Kkper)
C
C18-----COMPUTE STREAM REACH VARIABLES.
        irch = 1
        ksfropt = 0
        DO nseg = 1, Nss
          icalc = Iseg(1, nseg)
          seglen = Seg(1, nseg)
          runoff = Seg(3, nseg)
          etsw = Seg(4, nseg)
          pptsw = Seg(5, nseg)
          sumlen = 0.0
C
C19-----COMPUTE VARIABLES NEEDED FOR STREAM LEAKAGE.
          IF ( icalc.EQ.0 .OR. icalc.EQ.1 ) THEN
            wdslpe = (Seg(9, nseg)-Seg(14, nseg))/seglen
            IF ( icalc.EQ.0 ) dpslpe = (Seg(10, nseg)-Seg(15, nseg))
     +                                 /seglen
          END IF
          IF ( Isfropt.EQ.0 .OR. Isfropt.EQ.4 .OR. Isfropt.EQ.5 ) THEN
            ksfropt = 1
            elslpe = (Seg(8, nseg)-Seg(13, nseg))/seglen
            hcslpe = (Seg(6, nseg)-Seg(11, nseg))/seglen
            thkslpe = (Seg(7, nseg)-Seg(12, nseg))/seglen
          END IF
          DO ii = 1, Iseg(4, nseg)
            rchlen = Strm(1, irch)
            dist = sumlen + (0.5*rchlen)
            Strm(12, irch) = runoff*(rchlen/seglen)
            IF ( ksfropt.EQ.1 ) THEN
              avhc = Seg(6, nseg) - (hcslpe*dist)
              avthk = Seg(7, nseg) - (thkslpe*dist)
              Strm(2, irch) = elslpe
              Strm(3, irch) = Seg(8, nseg) - (elslpe*dist)
              Strm(4, irch) = Strm(3, irch) - avthk
              Strm(6, irch) = avhc
              Strm(8, irch) = avthk
C20-----COMPUTE STREAMBED ELEVATION AND STREAM WIDTH FOR BEGINNING
C         OF EACH STREAM SEGMENT FOR COMPUTATION OF LAKE OUTFLOW.
cdep 4/26/2006
            ELSE 
              IF ( ii.eq.1) THEN
                SEG(8,nseg) = STRM(3,irch) + ( 0.5 * STRM(1,irch)
     +                       * STRM(2,irch) )
              END IF            
            END IF
!dep 3/16/2009 Added check and warning for streambed thickness
            IF (STRM(8, irch).LT.CLOSEZERO)THEN
              WRITE (IOUT, 9040) nseg, irch, STRM(8, irch)
 9040   FORMAT (/, ' *** WARNING *** STREAMBED THICKNESS', 
     +          'FOR SEGMENT ',I10,' REACH ',I10,  
     +          ' IS ', E10.4,' WHICH IS ZERO OR LESS. '/,
     +          ' VALUE MUST BE GREATER THAN ZERO-- IT HAS BEEN ',
     +          'RESET TO 1.0')
              STRM(8, irch) = 1.0
            END IF
!dep 3/16/2009 end of change
            IF ( icalc.EQ.0 ) THEN
              avdpth = Seg(10, nseg) - (dpslpe*dist)
              Strm(5, irch) = Seg(9, nseg) - (wdslpe*dist)
              Strm(7, irch) = avdpth
              Strm(13, irch) = etsw*rchlen*Strm(5, irch)
              Strm(14, irch) = pptsw*rchlen*Strm(5, irch)
              Strm(15, irch) = avdpth + Strm(3, irch)
              IF ( ksfropt.EQ.1 ) Strm(16, irch)
     +             = (avhc*Strm(5, irch)*rchlen)/avthk
            ELSE IF ( icalc.EQ.1 ) THEN
              Strm(5, irch) = Seg(9, nseg) - (wdslpe*dist)
              Strm(7, irch) = 1.0
              Strm(13, irch) = etsw*rchlen*Strm(5, irch)
              Strm(14, irch) = pptsw*rchlen*Strm(5, irch)
              Strm(15, irch) = Strm(3, irch)
              IF ( ksfropt.EQ.1 ) Strm(16, irch)
     +             = (avhc*Strm(5, irch)*rchlen)/avthk
            ELSE IF ( icalc.GE.2 .AND. icalc.LE.4 ) THEN
              Strm(5, irch) = 1.0
              Strm(7, irch) = 1.0
              Strm(13, irch) = etsw*rchlen
              Strm(14, irch) = pptsw*rchlen
              Strm(15, irch) = Strm(3, irch)
              IF ( ksfropt.EQ.1 ) Strm(16, irch)
     +             = (Strm(5, irch)*Strm(1, irch)*Strm(6, irch))
     +             /Strm(8, irch)
C
C21-----STOP IF ICALC LESS THAN 0 AND GREATER THAN 4.
            ELSE
              CALL USTOP('Error: icalc < 0 or > 4')
            END IF
            sumlen = sumlen + rchlen
            irch = irch + 1
          END DO
        END DO
C
C22-----CHECK VALUES IN STREAM CROSS SECTION LIST (XSEC).
        DO nseg = 1, Nss
          icalc = Iseg(1, nseg)
          IF ( icalc.EQ.2 ) THEN
            IF ( ABS(Xsec(1,nseg)).GT.CLOSEZERO ) THEN
              WRITE (Iout, 9014) nseg
 9014         FORMAT (1X, /1X, '*** ERROR *** EIGHT POINT CROSS ',
     +                'SECTION FOR STREAM SEGMENT ', I6, ' DOES ',
     +                'NOT BEGIN WITH ZERO FOR FIRST VALUE --',
     +                'PROGRAM STOPPING')
              CALL USTOP(' ')
            END IF
            DO jj = 1, 8
              IF ( Xsec(jj, nseg).LT.0.0 ) THEN
                WRITE (Iout, 9015) nseg, jj, Xsec(jj, nseg)
 9015           FORMAT (1X, /1X, '*** ERROR *** STREAM SEGMENT ', I6,
     +                  ' HAS A NEGATIVE X DISTANCE FOR POINT ', I5,
     +                  ' INPUT VALUE IS ', E10.3, /1X,
     +                  'ALL VALUES MUST BE POSITIVE WITH ',
     +                  'FIRST X VALUE STARTING AT EXTREME LEFT ',
     +                  'EDGE OF SECTION LOOKING DOWNSTREAM ',
     +                  'PROGRAM STOPPING')
                CALL USTOP(' ')
              END IF
              kk = jj + 8
              IF ( Xsec(kk, nseg).LT.0.0 ) THEN
                WRITE (Iout, 9016) nseg, kk, Xsec(kk, nseg)
 9016           FORMAT (1X, /1X, '*** ERROR *** STREAM SEGMENT ', I6,
     +                  ' HAS A NEGATIVE Z DISTANCE FOR POINT ', I5,
     +                  ' INPUT VALUE IS ', E10.3, /1X,
     +                  'ALL VALUES MUST BE POSITIVE RELATIVE ',
     +                  'TO STREAMBED ELEVATION ')
                CALL USTOP(' ')
              END IF
            END DO
          END IF
        END DO
C
C23-----CHECK ROUGHNESS COEFFICIENTS WHEN ICALC = 1 OR 2.
        DO nseg = 1, Nss
          icalc = Iseg(1, nseg)
          IF ( icalc.EQ.1 ) THEN
            rough = Seg(16, nseg)
            IF ( rough.LE.0.0 ) THEN
              WRITE (Iout, 9017) rough
 9017         FORMAT ('*** ERROR *** ROUGHNESS COEFFICIENT WHEN ',
     +                'ICALC = 1 IS LESS THAN OR EQUAL TO ZERO', //1X,
     +                'VALUE IS ', 1PE10.3, //1X, 'PROGRAM STOPPING')
              CALL USTOP(' ')
            END IF
          ELSE IF ( icalc.EQ.2 ) THEN
            roughch = Seg(16, nseg)
            roughbnk = Seg(17, nseg)
            IF ( roughch.LE.0.0 ) THEN
              WRITE (Iout, 9018) roughch
 9018         FORMAT ('*** ERROR *** ROUGHNESS COEFFICIENT FOR ',
     +                'CHANNEL WHEN ICALC =2 IS LESS THAN OR EQUAL ',
     +                'TO ZERO', //1X, 'VALUE IS ', 1PE10.3, //1X,
     +                'PROGRAM STOPPING')
              CALL USTOP(' ')
            ELSE IF ( roughbnk.LE.0.0 ) THEN
              WRITE (Iout, 9019) roughbnk
 9019         FORMAT ('*** ERROR *** ROUGHNESS COEFFICIENT FOR BANK ',
     +                'WHEN ICALC =2 IS LESS THAN OR EQUAL TO ZERO', //,
     +                ' VALUE IS ', 1PE11.3, //, 'PROGRAM STOPPING')
     +                
              CALL USTOP(' ')
            END IF
          END IF
        END DO
C
C24-----CHECK VALUES IN TABLE OF FLOW VERSUS DEPTH AND WIDTH
C         WHEN ICALC = 4.
        DO nseg = 1, Nss
          icalc = Iseg(1, nseg)
          IF ( icalc.EQ.4 ) nstrpts = Iseg(2, nseg)
          IF ( icalc.EQ.4 ) THEN
            flwlw = Qstage(1, nseg)
            IF ( flwlw.LE.0.0 ) THEN
              WRITE (Iout, 9020) nseg
 9020         FORMAT (/1X, '*** WARNING *** FIRST FLOW VALUE IN ',
     +                'TABLE OF FLOW VERSUS DEPTH AND WIDTH IS ',
     +                'LESS THAN OR EQUAL TO ZERO FOR SEGMENT ',
     +                'NUMBER ', I6, /1X, 'VALUE SHOULD BE ',
     +                'GREATER THAN ZERO-- IT HAS BEEN RESET ',
     +                'TO 0.1 BUT MAY CAUSE INSTABILITY')
              Qstage(1, nseg) = 0.1
            END IF
            dpthlw = Qstage(1+nstrpts, nseg)
            IF ( dpthlw.LE.0.0 ) THEN
              WRITE (Iout, 9021) nseg
 9021         FORMAT (/1X, '*** WARNING *** FIRST DEPTH VALUE IN TABLE '
     +                , 'OF FLOW VERSUS DEPTH AND WIDTH IS LESS THAN ',
     +                'OR EQUAL TO ZERO FOR SEGMENT NUMBER ', I6, /1X,
     +                'VALUE SHOULD BE GREATER THAN ZERO-- ',
     +                'IT HAS BEEN RESET TO 0.01 BUT MAY CAUSE ',
     +                'INSTABILITY')
              Qstage(1+nstrpts, nseg) = 0.01
            END IF
            wdthlw = Qstage(1+2*nstrpts, nseg)
            IF ( wdthlw.LE.0.0 ) THEN
              WRITE (Iout, 9022) nseg
 9022         FORMAT (/1X,
     +                '*** WARNING *** FIRST WIDTH VALUE IN TABLE OF ',
     +               'FLOW VERSUS DEPTH AND WIDTH IS LESS THAN OR EQUAL'
     +               , ' TO ZERO FOR SEGMENT NUMBER ', I6, /1X,
     +               'VALUE ',
     +               'SHOULD BE GREATER THAN ZERO-- IT HAS BEEN ',
     +               'RESET TO 1.0 BUT MAY CAUSE INSTABILITY')
              Qstage(1+2*nstrpts, nseg) = 1.0
            END IF
            DO ipt = 2, nstrpts
              flw1 = Qstage(ipt-1, nseg)
              flw2 = Qstage(ipt, nseg)
              dpth1 = Qstage((ipt-1)+nstrpts, nseg)
              dpth2 = Qstage(ipt+nstrpts, nseg)
              wdth1 = Qstage((ipt-1)+(2*nstrpts), nseg)
              wdth2 = Qstage(ipt+(2*nstrpts), nseg)
              IF ( flw2.LE.flw1 ) THEN
                WRITE (Iout, 9023) nseg, flw2, ipt
 9023           FORMAT (/1X, '*** ERROR *** SEGMENT NUMBER ', I6,
     +                  'HAS SPECIFIED FLOW VALUE OF ', 1PE10.2, ' IN ',
     +                  'LOCATION ', I5, ' THAT IS LESS THAN OR EQUAL ',
     +                  'TO PRECEDING VALUE', /1X, 'FLOW VALUES MUST ',
     +                  'BE GREATER THAN PRECEDING VALUE IN TABLE--',
     +                  ' PROGRAM STOPPING')
                CALL USTOP(' ')
              END IF
              IF ( dpth2.LE.dpth1 ) THEN
                WRITE (Iout, 9024) nseg, dpth2, ipt
 9024           FORMAT (/1X, '*** ERROR *** SEGMENT NUMBER ', I6,
     +                  'HAS SPECIFIED DEPTH VALUE OF ', 1PE10.2,
     +                  ' IN ', 'LOCATION ', I5,
     +                  ' THAT IS LESS THAN OR EQUAL ',
     +                  'TO PRECEDING VALUE', /1X, 'DEPTH VALUES MUST ',
     +                  'BE GREATER THAN PRECEDING VALUE IN TABLE--',
     +                  ' PROGRAM STOPPING')
                CALL USTOP(' ')
              END IF
              IF ( wdth2.LT.wdth1 ) THEN
                WRITE (Iout, 9025) nseg, wdth2, ipt
 9025           FORMAT (/1X, '*** WARNING *** SEGMENT NUMBER ', I6,
     +                  ' HAS SPECIFIED WIDTH VALUE OF ', 1PE10.2,
     +                  ' IN LOCATION ', I5,
     +                  ' THAT IS LESS THAN PRECEDING VALUE', /1X,
     +                  'FOR MOST CHANNELS, WIDTH NORMALLY',
     +                  ' INCREASES WITH FLOW')
              END IF
            END DO
          END IF
        END DO
        WRITE (Iout, 9026)
 9026   FORMAT (//)
      END IF
C
C25-----COMPUTE STREAMBED ELEVATIONS FOR TOP AND BOTTOM, AND STREAMBED
C        SLOPE FROM LAND SURFACE ELEVATION WHEN SPECIFIED.
C        MODIFIED BY WOLFGANG SCHMID FOR FARM PROCESS.
Cdep----3/16/2009 separated if statement to avoid referencing zero elements in array.
      IF( ABS(Irdflg).EQ.2 ) THEN
        DO irch = 2, Nstrm
          IF( Istrm(4, irch).GT.1 )THEN
            IF( Idivar(1,Istrm(4, irch)-1).GT.0 ) THEN
              icp = Istrm(3, irch-1)
              irp = Istrm(2, irch-1)
              IF( Istrm(5, irch).EQ.1 )  Seg(13, Istrm(4, irch)-1) =
     +           Botm(icp, irp, 0) - Seg(13, Istrm(4, irch)-1 )
            END IF
          END IF
        END DO
        DO Nseg = 1, Nss
          IF( Idivar(1, Nseg).GT.0 ) THEN
C
C26-----COMPUTE STREAMBED TOP ELEVATION FOR CANAL REACHES
C        IN FARM PROCESS.
            seglen = Seg(1, Nseg)
            sumlen = 0.0
            DO irch = 1, Nstrm
              IF( Idivar(1, Istrm(4, irch)).EQ.Idivar(1, Nseg) ) THEN
                rchlen = Strm(1, irch)
                dist = sumlen + (0.5 * rchlen)
                sumlen = sumlen + rchlen
                ic = Istrm(3, irch)
                ir = Istrm(2, irch)
                IF( Istrm(5, irch).EQ.1 ) updiff = Botm(ic, ir, 0) -
     +                                    Seg(8, Istrm(4, irch))
                dndiff = Seg(13, Istrm(4, irch))
                Strm(3, irch) = Botm(ic, ir, 0) - (updiff -
     +                          (((updiff - dndiff) / seglen) * dist))
                avthk = Seg(7, Nseg) - (((Seg(7, Nseg) -
     +                  Seg(12, Nseg)) / seglen) * dist)
                Strm(4, irch) = Strm(3, irch) - avthk
                IF ( Icalc.EQ.0 ) THEN
                  Strm(15, irch) = avdpth + Strm(3, irch)
                ELSE IF ( Icalc.EQ.1 ) THEN
                  Strm(15, irch) = Strm(3, irch)
                ELSE IF ( Icalc.GE.2 .AND. Icalc.LE.4 ) THEN
                  Strm(15, irch) = Strm(3, irch)
                END IF
              END IF
            END DO
C
C27-----COMPUTE STREAMBED SLOPE FOR CANAL REACHES IN FARM PROCESS.
C       NOTE THAT FIRST AND LAST REACH CAN NOT BE CANAL REACHES.
            DO irch = 2, Nstrm-1
              IF( Idivar(1, Istrm(4, irch)).EQ.Idivar(1, Nseg) ) THEN
                Strm(2, irch) = (Strm(3, irch-1) - Strm(3,irch+1) )
     +                           / (0.5 * Strm(1, irch-1) +
     +                          Strm(1, irch) + 0.5 * Strm(1, irch+1))
                IF( Istrm(5, irch).EQ.1 ) THEN
                  Strm(2, irch) = (Seg(8, Istrm(4, irch)) -
     +                             Strm(3, irch+1)) / (Strm(1, irch) +
     +                             0.5 * Strm(1, irch+1))
                END IF
                IF( Istrm(5, irch+1).LT.Istrm(5, irch) ) THEN
                  ic = Istrm(3, irch)
                  ir = Istrm(2, irch)
                  dndiff = Seg(13,Istrm(4, irch))
                  eldn = Botm(ic,ir,0) - dndiff
                  Strm(2, irch) = (Strm(3, irch-1) - eldn) / (0.5 *
     +                             Strm(1, irch-1) + Strm(1, irch))
                END IF
                IF( Strm(2, irch).LT.CLOSEZERO ) THEN
                  IF( Strm(2, irch ).LT.CLOSEZERO ) 
     +                              Strm(2, irch) = 1.0E-06
                  WRITE(Iout,9027)  Istrm(4,irch), Istrm(5,irch),
     +                              Strm(2, irch)
 9027             FORMAT(1X,'SLOPE FOR SEGMENT AND REACH ',2(1x,I5),
     +                   'IS LESS THAN 1.0E-07: SETTING SLOPE TO '
     +                   '1.0E-06 ')
                END IF
              END IF
            END DO
          END IF
        END DO
C
        WRITE (Iout, 9028)
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
      DO l = 1, Nstrm
        il = Istrm(1, l)
        ir = Istrm(2, l)
        ic = Istrm(3, l)
        h = Hnew(ic, ir, il)
        Strm(24, l) = h
        IF ( Iuzt.EQ.1 .AND. iflginit.GE.1 ) THEN
          istsg = Istrm(4, l)
          icalc = Iseg(1, istsg)
          sbot = Strm(4, l)
          strlen = Strm(1, l)
          width = Strm(5, l)
C
C31-----SKIP IF CELL IS OUTSIDE ACTIVE BOUNDARY OR IS NOT WATER TABLE.
Cdep
C31B-----SEARCH FOR UPPER MOST ACTIVE CELL IN STREAM REACH.
            ilay = il   !dep 3/16/2009  moved before IF to be sure ilay has a value
          IF ( Ibound(ic, ir, il).GT.0 ) THEN
!dep        ilay = il
            TOPCELL: DO WHILE ( ilay.LE.Nlay )
              IF ( Hnew(ic, ir, ilay).LE.Botm(ic,ir,ilay) ) THEN
                ilay = ilay + 1
              ELSE
                EXIT TOPCELL
              END IF
            END DO TOPCELL
          END IF
          IF ( ilay.LE.Nlay ) THEN
            il = ilay
            h = Hnew(ic, ir, il)
          ELSE
            h = Botm(ic,ir,Nlay)
          END IF
          IF ( Ibound(ic, ir, il).LE.0 ) THEN
            Uzdpst(l, 1) = 0.0D0
            Uzflst(l, 1) = 0.0D0
            Uzspst(l, 1) = 0.0D0
            Uzthst(l, 1) = Thtr(l)
            Uzstor(l, 1) = 0.0D0
            Uzolsflx(l, 1) = 0.0D0
C
C32-----BREAK CHANNEL INTO ISUZN WIDTHS FOR UNSATURATED FLOW 
C         WHEN ICALC IS 2 AND UNSATURATED FLOW IS ACTIVE.
          ELSE IF ( icalc.EQ.2 ) THEN
            CALL CHANNELAREA(Dpthinc, Wetper, Xsec, istsg, Isuzn, l,
     +                       Nuzst, Nss, Iout)
            istep = Nstotrl/Isuzn
            DO jk = 1, Nstotrl
              Uzthst(l, jk) = Thtr(l)
            END DO
C
C33-----INITIALIZE UNSATURATED ZONE ARRAYS WHEN GROUND WATER HEAD
C         IS LESS THAN BOTTOM OF STREAMBED.
            IF ( sbot.GT.h ) THEN
              iwvcnt = 1
              DO i = 1, Isuzn
                Uzdpst(l, iwvcnt) = sbot - h
                Uzspst(l, iwvcnt) = 0.0D0
                Nwavst(l, i) = 1
C
C34-----INITIALIZE UNSATURATED ZONES ARRAYS FOR SECOND STRESS PERIOD
C         WHEN FIRST STRESS PERIOD IS STEADY STATE.
                IF ( iflginit.EQ.2 ) THEN
                  IF ( Uzseep(l, i).GT.0.0 ) THEN
                    Uzflst(l, iwvcnt) = Uzseep(l, i)
                    Uzthst(l, iwvcnt) = (((Uzflst(l,iwvcnt)/Uhc(l))**(
     +                                  1.0D0/Eps(l)))*(Thts(l)-Thtr(l))
     +                                  ) + Thtr(l)
                    top = (Uzthst(l, iwvcnt)-Thtr(l))
                    Uzstor(l, i) = (Uzdpst(l, iwvcnt))
     +                             *top*Wetper(l, i)*strlen
                    Uzolsflx(l, i) = Uzseep(l, i)
                  ELSE
                    Uzflst(l, iwvcnt) = 0.0D0
                    Uzthst(l, iwvcnt) = Thtr(l)
                    Uzstor(l, i) = 0.0D0
                    Uzolsflx(l, i) = 0.0D0
                  END IF
C
C35-----INITIALIZE UNSATURATED ZONE ARRAYS WHEN FIRST STRESS PERIOD IS
C         TRANSIENT.
                ELSE IF ( iss.EQ.0 ) THEN
                  top = (Thti(l)-Thtr(l))
                  IF ( top.LE.0.0 ) top = 0.0
                  Uzthst(l, 1) = Thti(l)
                  Uzstor(l, 1) = (Uzdpst(l, 1))*top*Wetper(l, 1)*strlen
                  bottom = (Thts(l)-Thtr(l))
                  IF ( bottom.LE.0.0 .OR. top.LE.0.0 ) THEN
                    Uzflst(l, 1) = 0.0D0
                  ELSE
                    Uzflst(l, 1) = Uhc(l)*(top/bottom)**Eps(l)
                  END IF
C
C36-----INITIALIZE UNSATURATED ZONE ARRAYS WHEN FIRST STRESS PERIOD IS
C         STEADY STATE.
                ELSE
                  Uzthst(l, 1) = Thtr(l)
                  Uzstor(l, 1) = 0.0D0
                  Uzflst(l, 1) = 0.0D0
                  Uzolsflx(l, 1) = 0.0D0
                END IF
                iwvcnt = iwvcnt + istep
              END DO
C
C37-----INITIALIZE UNSATURATED ZONE ARRAYS TO ZERO WHEN NO UNSATURATED
C         ZONE.
            ELSE
              iwvcnt = 1
              istep = Nstotrl/Isuzn
              DO i = 1, Isuzn
                Uzdpst(l, iwvcnt) = 0.0D0
                Uzflst(l, iwvcnt) = 0.0D0
                Uzspst(l, iwvcnt) = 0.0D0
                Uzthst(l, 1) = Thtr(l)
                iwvcnt = iwvcnt + istep
              END DO
            END IF
            Uzolsflx(l, 1) = Uzflst(l, 1)
C
C38-----ONLY ONE UNSATURATED ZONE WIDTH WHEN ICALC IS 1.
          ELSE IF ( icalc.EQ.1 ) THEN
            Wetper(l, 1) = width
C
C39-----INITIALIZE UNSATURATED ZONE ARRAYS WHEN GROUND WATER HEAD
C         IS LESS THAN BOTTOM OF STREAMBED.
            IF ( sbot.GT.h ) THEN
              Uzdpst(l, 1) = sbot - h
              Uzspst(l, 1) = 0.0D0
              Nwavst(l, 1) = 1
C
C40-----INITIALIZE UNSATURATED ZONE ARRAYS FOR SECOND STRESS PERIOD
C         WHEN FIRST STRESS PERIOD IS STEADY STATE.
              IF ( iflginit.EQ.2 ) THEN
                IF ( Uzseep(l, 1).GT.0.0 ) THEN
                  Uzflst(l, 1) = Uzseep(l, 1)
                  Uzthst(l, 1) = (((Uzflst(l,1)/Uhc(l))**(1.0D0/Eps(l)))
     +                           *(Thts(l)-Thtr(l))) + Thtr(l)
                  top = (Uzthst(l, 1)-Thtr(l))
                  IF ( top.LE.0.0 ) top = 0.0
                  Uzstor(l, 1) = (Uzdpst(l, 1))*top*Wetper(l, 1)*strlen
                  Uzolsflx(l, 1) = Uzseep(l, 1)
                ELSE
                  Uzflst(l, 1) = 0.0D0
                  Uzthst(l, 1) = Thtr(l)
                  Uzstor(l, 1) = 0.0D0
                  Uzolsflx(l, 1) = 0.0D0
                END IF
C
C41-----INITIALIZE UNSATURATED ZONE ARRAYS WHEN FIRST STRESS PERIOD IS
C         TRANSIENT.
              ELSE IF ( iss.EQ.0 ) THEN
                Uzthst(l, 1) = Thti(l)
                top = (Thti(l)-Thtr(l))
                IF ( top.LE.0.0 ) top = 0.0
                Uzstor(l, 1) = (Uzdpst(l, 1))*top*width*strlen
                bottom = (Thts(l)-Thtr(l))
                IF ( bottom.LE.0.0 .OR. top.LE.0.0 ) THEN
                  Uzflst(l, 1) = 0.0D0
                ELSE
                  Uzflst(l, 1) = Uhc(l)*(top/bottom)**Eps(l)
                END IF
C
C42-----INITIALIZE UNSATURATED ZONE ARRAYS WHEN FIRST STRESS PERIOD IS
C         STEADY STATE.
              ELSE
                Uzthst(l, 1) = Thtr(l)
                Uzstor(l, 1) = 0.0D0
                Uzflst(l, 1) = 0.0D0
              END IF
            ELSE
C
C43-----INITIALIZE UNSATURATED ZONE ARRAYS TO ZERO WHEN NO UNSATURATED
C         ZONE.
              Uzthst(l, 1) = Thtr(l)
              Uzdpst(l, 1) = 0.0D0
              Uzflst(l, 1) = 0.0D0
              Uzspst(l, 1) = 0.0D0
              Uzstor(l, 1) = 0.0D0
            END IF
            DO ik = 2, Nstotrl
              Uzthst(l, ik) = Thtr(l)
            END DO
            Uzolsflx(l, 1) = Uzflst(l, 1)
          END IF
        END IF
      END DO
Cdep    Added new subroutine to compute tables for lake outflow
C44------COMPUTE VAULES FOR ARRAYS DKLOTFLW AND DLKSTAGE WHEN OUTFLOW 
C          FROM LAKES ARE COMPUTED IN THE LAKE PACKAGE.
      IF ( Iunitlak.GT.0 ) THEN
        CALL GWF1SFR2LAKOUTFLW(Nlakesar, Nstrm, Nss, Nsegdim, Const,   
     +                         Istrm, Iseg, Idivar, Strm, Seg, Xsec,    
     +                         Qstage, Maxpts, Nsslk, Slkotflw, 
     +                         Dlkotflw, Dlkstage, Iout)
      END IF
C
C45-----RETURN.
      RETURN
      END SUBROUTINE GWF1SFR2RPS
C
C-------SUBROUTINE GWF1SFR2FM
Cdep   Changed Dstrot in subroutine list to Fxlkot.
      SUBROUTINE GWF1SFR2FM(Strm, Istrm, Hnew, Hold, Hcof, Rhs, Ibound,
     +                      Nstrm, Ncol, Nrow, Nlay, Iout, Nss, Nsegdim,
     +                      Seg, Iseg, Iotsg, Xsec, Idivar, Qstage,
     +                      Const, Maxpts, Dleak, Sgotflw, Dvrsflw,
     +                      Nlakesar, Stgold, Strin, Strout, Stgnew,
     +                      Theta, Vol, Iss, Fxlkot, Sfrq, Iunitlak,
     +                      Kkiter, Uzdpit, Uzthit, Uzspit, Uzflit,
     +                      Uzdpst, Uzthst,Uzspst, Uzflst, Eps, Thts,
     +                      Thtr, Ltrlit, Itrlit, Ltrlst, Itrlst, 
     +                      Uzflwt, Nwavst, Nstrail, Nstotrl, Iuzt,
     +                      Isuzn, Nuzst, Delt, Uzwdth, Uzolsflx, Loop,
     +                      Uhc, Totim, Ndpelev, Dpthinc, Wetper, 
     +                      Nsfrsets, Uzseep, Foldflbt, Kkper, Kkstp, 
     +                      Numcell, Nuzrow, Nuzcol)
C     *****************************************************************
C     ADD STREAM TERMS TO RHS AND HCOF IF FLOW OCCURS IN MODEL CELL
C     VERSION  2.7: MARCH 16, 2009 dep
C     *****************************************************************
      IMPLICIT NONE

C     -----------------------------------------------------------------
C     SPECIFICATIONS:
C     -----------------------------------------------------------------
C     FUNCTIONS
C     -----------------------------------------------------------------
      REAL CALCUNSATFLOBOT
      EXTERNAL CALCUNSATFLOBOT
C     -----------------------------------------------------------------
C     ARGUMENTS
C     -----------------------------------------------------------------
      REAL Hold
      DOUBLE PRECISION Hnew, Foldflbt
      DOUBLE PRECISION Uzspit, Uzspst, Uzflst, Uzflit, Uzdpst, Uzdpit,
     +                 Uzthst, Uzthit, Uzseep, Uzolsflx, Uzflwt, Uzwdth,
     +                 Wetper, Thts, Thtr, Eps
Cdep  Stgnew and Stgold DOUBLE PRECISION
      DOUBLE PRECISION Stgnew(Nlakesar), Stgold(Nlakesar) 
      REAL Const, Delt, Dleak, Dpthinc, Fxlkot, Dvrsflw, Hcof, Qstage,
!     +     Rhs, Seg, Sgotflw, Stgnew, Stgold, Strin, Strm, Strout, Theta
     +     Rhs, Seg, Sgotflw, Strin, Strm, Strout, Theta
      REAL Totim, Uhc, Vol, Xsec
      INTEGER Ibound, Idivar, Iotsg, Iout, Iseg, Iss, Istrm, Isuzn,
     +        Itrlit, Itrlst, Iunitlak, Iuzt, Kkiter, Kkper, Kkstp
      INTEGER Loop, Ltrlit, Ltrlst, Maxpts, Ncol, Ndpelev, Nlakesar,
     +        Nlay, Nrow, Nsegdim, Nsfrsets, Nss, Nstotrl, Nstrail,
     +        Nstrm, Numcell, Nuzcol, Nuzrow, Nuzst, Nwavst
Cdep  Changed Dstrot to Fxlkot in dimension statement
      DIMENSION Strm(24, Nstrm), Istrm(5, Nstrm), Hnew(Ncol, Nrow, Nlay)
     +          , Hcof(Ncol, Nrow, Nlay), Rhs(Ncol, Nrow, Nlay),
     +          Ibound(Ncol, Nrow, Nlay), Seg(26, Nsegdim),
     +          Iseg(4, Nsegdim), Iotsg(Nsegdim), Xsec(16, Nsegdim),
     +          Idivar(2, Nsegdim), Qstage(Maxpts, Nsegdim),
     +          Sgotflw(Nss), Dvrsflw(Nss),  Strin(Nss), Strout(Nss),
     +          Fxlkot(Nss), Vol(Nlakesar)
!     +          Stgold(Nlakesar), Strin(Nss), Strout(Nss),
!     +          Stgnew(Nlakesar), Fxlkot(Nss), Vol(Nlakesar)
      DIMENSION Uzspst(Nuzst, Nstotrl), Uzspit(Nuzst, Nstotrl),
     +          Uzflit(Nuzst, Nstotrl), Uzflst(Nuzst, Nstotrl),
     +          Uzthit(Nuzst, Nstotrl), Uzthst(Nuzst, Nstotrl),
     +          Uzolsflx(Nuzst, Isuzn)
      DIMENSION Uzdpit(Nuzst, Nstotrl), Uzseep(Nuzst, Isuzn),
     +          Uzdpst(Nuzst, Nstotrl), Uzflwt(Nuzst, Isuzn),
     +          Uzwdth(Nuzst, Isuzn), Foldflbt(Nuzst)
      DIMENSION Hold(Ncol, Nrow, Nlay)
      DIMENSION Ndpelev(Nuzst, 8)
      DIMENSION Dpthinc(Nuzst, Isuzn), Wetper(Nuzst, Isuzn)
      DIMENSION Eps(Nuzst), Thts(Nuzst), Thtr(Nuzst), Uhc(Nuzst),
     +          Nwavst(Nuzst, Isuzn), Ltrlit(Nuzst, Nstotrl),
     +          Itrlit(Nuzst, Nstotrl), Ltrlst(Nuzst, Nstotrl),
     +          Itrlst(Nuzst, Nstotrl), Loop(Isuzn)
      REAL Sfrq(5, Nstrm)
C     -----------------------------------------------------------------
C     LOCAL VARIABLES
C     -----------------------------------------------------------------
      DOUBLE PRECISION cstr, cstr1, cstr2, dbleak, dlet1, dlet2, dlfh,
     +                 dlh, dlpp1, dlpp2, dlwp1, dlwp2, depth, depthx,
     +                 depthp, deptha, depthb, depthc, depthd, depth1,
     +                 depth2, dlkstr, dlkstr1, dlkstr2, deps, et1, et2,
     +                 fhstr1, fhstr2, flobot, flobot1, flobot2, flowc,
     +                 flowin, flowot, flwdlk1, flwdlk2, flwmdpta,
     +                 flwmdptb, flwmdptc, flwmdptd, flwmdpt1, flwmdpt2,
     +                 flwmpt, h, hstr, pp1, pp2, sbot, slope, strleak,
     +                 strtop, trbflw, upflw, wdthlk1, wdthlk2, wetperm,
     +                 wetperma, wetpermb, wetpermc, wetpermd, wetperm1,
     +                 wetperm2, wetpermp, wetpermx, width, widtha,
     +                 widthb, widthc, widthd, width1, width2, widthp,
     +                 widthx, bwdth, cdpth, fdpth, awdth, f1, f2, fp,
     +                 enpt1, enpt2, flwen1, flwen2, flwp, flobotp,
     +                 flobotold, flwpetp, flwx, flwmpt2, flwest,
     +                 flwpet1, flwpet2, err, dlhold, precip, etstr,
     +                 runof, thet1
      DOUBLE PRECISION fbcheck, hld, totflwt, sbdthk, thetas, epsilon,
     +                 thr, addrhs, addrise
      REAL areamax, avhc, dif, errold, fherr, fherrold, fks, ha, prcnt,
     +     qcnst, seep, stgon, strlen, thti1
      INTEGER i, ibflg, ic, icalc, idivseg, iflg, iic, iic2, iic3, iic4,
     +        il, ilay, iprior, iprndpth, iprvsg, ir, istsg, itot,
     +        itrib, itstr, iupseg, iwidthcheck, kerp, kss, l, lk, ll,
     +        nreach, maxwav, icalccheck, iskip
Cdep added closezero and nearzero 6/5/2006
      REAL CLOSEZERO
      DOUBLE PRECISION FIVE_THIRDS, NEARZERO, dvrsn
      PARAMETER (CLOSEZERO=1.0E-15)
      PARAMETER (FIVE_THIRDS=5.0D0/3.0D0, NEARZERO=1.0D-30)
C     -----------------------------------------------------------------
C
C1------NO STREAMS WHEN Nstrm LESS THAN OR EQUAL TO ZERO. RETURN.
      IF ( Nstrm.LE.0 ) RETURN
C
C2------THERE ARE STREAMS.  INITIALIZE SEGMENT INFLOWS AND OUTFLOWS
C         TO ZERO FOR LAKE PACKAGE.
      maxwav = Nsfrsets*Nstrail
      itstr = 0
      iprvsg = -1
      IF ( Iunitlak.GT.0 ) THEN
Cdep  No longer need to initialize STRIN array to zero
        DO i = 1, Nss
Cdep          Strin(i) = 0.0
Cdep          Strout(i) = 0.0
Cdep          Dstrot(i) = 0.0
          Fxlkot(i) = 0.0
        END DO
Cdep  Change time weighting factor for lake stage calculation.
Cdep   6/27/2005
        IF (ISS.NE.1) THEN
          THET1 = THETA
        ELSE
          THET1 = 1.0D0
        END IF
      END IF
      flowot = 0.0D0
C
C3------DETERMINE LAYER, ROW, COLUMN OF EACH REACH.
      DO l = 1, Nstrm
        flowin = 0.0D0
        ll = l - 1
        il = Istrm(1, l)
        ir = Istrm(2, l)
        ic = Istrm(3, l)

C
C4------DETERMINE STREAM SEGMENT AND REACH NUMBER.
        istsg = Istrm(4, l)
        nreach = Istrm(5, l)
        icalc = Iseg(1, istsg)
        IF ( icalc.EQ.1 .OR. icalc.EQ.2 ) slope = Strm(2, l)
C
C5------SET FLOWIN EQUAL TO STREAM SEGMENT INFLOW IF FIRST REACH.
        IF ( nreach.EQ.1 ) THEN
          IF ( Iseg(3, istsg).EQ.5 ) flowin = Seg(2, istsg)
C
C6------STORE OUTFLOW FROM PREVIOUS SEGMENT IN SGOTFLW LIST AND IN
C         STRIN FOR LAKE PACKAGE.
          IF ( istsg.GT.1 ) THEN
            iprvsg = Istrm(4, ll)
            Sgotflw(iprvsg) = Strm(9, ll)
            IF ( Iunitlak.GT.0 ) Strin(iprvsg) = Strm(9, ll)
          END IF
C
C7------COMPUTE INFLOW OF A STREAM SEGMENT EMANATING FROM A LAKE.
          IF ( (Iunitlak.GT.0) .AND. (Idivar(1,istsg).LT.0) ) THEN
            lk = IABS(Idivar(1, istsg))
Cdep    Only calculate outflow from lake for initial time step of
Cdep         first stress period
C
C8------CHECK IF LAKE OUTFLOW IS SPECIFIED AT A FIXED RATE.
            IF ( Seg(2, istsg).GT.CLOSEZERO .AND. 
     +           Vol(lk).GT.CLOSEZERO ) THEN
              IF( SEG(2, istsg)*DELT-Vol(lk).LT.-CLOSEZERO )THEN
                FXLKOT(istsg) = SEG(2, istsg)
              ELSE
                FXLKOT(istsg) = Vol(lk)/DELT
                  WRITE(Iout,9000) lk,FXLKOT(istsg)
9000              FORMAT(/5X, '*** WARNING *** SPECIFIED OUTFLOW ',
     +                   'VOLUME FOR TIME STEP IS GREATER THAN ',
     +                   'VOLUME IN LAKE ',I5,//' RATE HAS BEEN ', 
     +                   'DECREASED TO ',1PE15.7)
              END IF
                flowin = FXLKOT(istsg)
            ELSE IF ( Seg(2, istsg).LT.-CLOSEZERO ) THEN
              WRITE (Iout, 9001) istsg
 9001         FORMAT (/5X, '*** WARNING *** NEGATIVE LAKE OUTFLOW ',
     +                'NOT ALLOWED; SEG = ', I6, /10X,
     +                'CODE WILL ASSUME FLOW = 0.0'/)
              Seg(2, istsg) = 0.0
              flowin = Seg(2, istsg)
              Fxlkot(istsg) = flowin
            END IF
C
C9------SPECIFIED FLOW FROM LAKE IS ZERO AND ICALC IS ZERO.
            IF ( icalc.EQ.0 ) THEN
              flowin = Fxlkot(istsg)
            END IF
C
C9B-----ESTIMATE LAKE OUTFLOW FOR FIRST ITERATION OF SIMULATION.
            IF( Kkper.EQ.1 .AND. Kkstp.EQ.1 .AND. Kkiter.EQ.1 ) THEN
              stgon = (1.0-Thet1)*Stgold(lk) + Thet1*Stgnew(lk)
              dlkstr = stgon - SEG(8, istsg)
              IF ( FXLKOT(istsg).LE.CLOSEZERO )THEN
C
C10-----FLOW FROM LAKE COMPUTED USING MANNINGS FORMULA AND ASSUMING A
C         WIDE RECTANGULAR CHANNEL.
                IF ( dlkstr.GT.NEARZERO .AND. icalc.EQ.1 ) THEN
                  flowin = (Const/Seg(16, istsg))*Seg(9, istsg)
     +                     *(dlkstr**FIVE_THIRDS)*(DSQRT(slope))
C
C11-----FLOW FROM LAKE COMPUTED USING MANNINGS FORMULA AND EIGHT POINT
C         CROSS SECTIONAL AREA.
                ELSE IF ( dlkstr.GT.NEARZERO .AND. icalc.EQ.2 ) THEN
                  CALL GWF1SFR2FLW(dlkstr, Xsec, Const, istsg, Nsegdim,
     +                             Seg(16, istsg), Seg(17, istsg), 
     +                             slope, wetperm, flowin, width)
C
C12-----FLOW FROM LAKE COMPUTED USING FORMULA--
C         Q=(DEPTH/CDPTH)**1/FDPTH).
                ELSE IF ( dlkstr.GT.NEARZERO .AND. icalc.EQ.3 ) THEN
                  cdpth = Seg(9, istsg)
                  fdpth = Seg(10, istsg)
                  flowin = (dlkstr/cdpth)**(1.0D0/fdpth)
C
C13-----FLOW FROM LAKE COMPUTED USING TABULATED VALUES.
                ELSE IF ( dlkstr.GT.NEARZERO .AND. icalc.EQ.4 ) THEN
                  CALL GWF1SFR2TBF(flowin, Qstage, dlkstr, width,
     +                         Iseg(2, istsg), Maxpts, Nsegdim, nreach,
     +                         istsg, Kkiter, Iout, 0)
                ELSE IF ( dlkstr.LT.NEARZERO .AND. icalc.GT.0 ) THEN
                  flowin = 0.0D0
                END IF
                Strout(istsg) = flowin
              END IF
            ELSE IF ( Fxlkot(istsg).LE.CLOSEZERO ) THEN
              flowin = Strout(istsg)
            END IF 
          END IF
C
C14-----COMPUTE ONE OR MORE DIVERSIONS FROM UPSTREAM SEGMENT.
Crgn&dep   revised computation of diversions and added subroutine
            IF( istsg.GT.1 )THEN
              DO kss = 2, Nss
                upflw = Sgotflw(istsg-1)
                idivseg = kss
                IF( Idivar(1,kss).EQ.istsg-1 ) THEN
                   dvrsn = Seg(2,idivseg)
                   iprior = Idivar(2,kss)
                  CALL GWF2SFR2DIVERS(iprior, idivseg, upflw, dvrsn)
                  Dvrsflw(kss) = dvrsn
                  Sgotflw(istsg-1) = Sgotflw(istsg-1) - dvrsn
                END IF
              END DO
C
C20-----SET FLOW INTO DIVERSION IF SEGMENT IS DIVERSION.
              IF( Iseg(3,istsg).EQ.6 ) THEN
                IF( Idivar(1,istsg).GT.0 ) flowin = Dvrsflw(istsg)
              END IF
            END IF
C
C21-----SUM TRIBUTARY OUTFLOW AND USE AS INFLOW INTO DOWNSTREAM SEGMENT.
          IF ( istsg.GE.1 .AND. Iseg(3, istsg).EQ.7 ) THEN
            flowin = 0.0D0
            DO itrib = 1, Nss
              IF ( istsg.EQ.Iotsg(itrib) ) THEN
                trbflw = Sgotflw(itrib)
                flowin = flowin + trbflw
              END IF
            END DO
            flowin = flowin + Seg(2, istsg)
            IF ( flowin.LT.0.0D0 ) THEN
              flowin = 0.0D0
              WRITE (Iout, 9002) istsg
 9002         FORMAT (//2X, '*** WARNING *** FLOW INTO TRIBUTARY ',
     +                'STREAM SEGMENT No. ', I6, ' WAS NEGATIVE; ',
     +                'FLOWIN RE-SET = 0.0'/)
            END IF
          END IF
C
C22-----SET INFLOW EQUAL TO OUTFLOW FROM UPSTREAM REACH WHEN REACH
C         IS GREATER THAN 1.
        ELSE IF ( nreach.GT.1 ) THEN
          flowin = Strm(9, ll)
        END IF
C
C23-----SEARCH FOR UPPER MOST ACTIVE CELL IN STREAM REACH.
        ilay = il
        TOPCELL: DO WHILE ( ilay.LE.Nlay )
          IF ( Ibound(ic, ir, ilay).EQ.0 ) THEN
            ilay = ilay + 1
          ELSE
            EXIT TOPCELL
          END IF
        END DO TOPCELL
        IF ( ilay.LE.Nlay ) il = ilay

C
C24-----INITIALIZE VARIABLES.
        iprndpth = 0
        depth = Strm(7, l)
        IF ( depth.LT.NEARZERO ) depth = 0.D0
        strtop = Strm(3, l)
        sbot = Strm(4, l)
        width = Strm(5, l)
        strlen = Strm(1, l)
        h = Hnew(ic, ir, il)
        hld = Strm(24, l)
        avhc = Strm(6, l)
        sbdthk = Strm(8, l)
        hstr = depth + Strm(3, l)
        cstr = Strm(16, l)
        precip = Strm(14, l)
        etstr = Strm(13, l)
        runof = Strm(12, l)
        strleak = strlen*avhc
        depthx = 0.0D0
        dbleak = Dleak
        deps = 0.999*Dleak
        dlh = deps
        dlhold = 1.0D6
        itot = 0
        iskip = 0
        IF ( icalc.EQ.1 ) THEN
          qcnst = Const*width*SQRT(slope)/Seg(16, istsg)
        ELSE IF ( icalc.EQ.3 ) THEN
          cdpth = Seg(9, istsg)
          fdpth = Seg(10, istsg)
          awdth = Seg(14, istsg)
          bwdth = Seg(15, istsg)
        END IF
C
C25-----INITIALIZE UNSATURATED ZONE VARIABLES.
        icalccheck = 0
        flobotold = 0.0D0
        addrhs = 0.0D0
        areamax = 0.0
        addrise = 0.0D0
        IF ( Iuzt.EQ.1 ) THEN
          IF ( icalc.EQ.1 .OR. icalc.EQ.2 ) icalccheck = 1
          IF ( icalccheck.EQ.1 ) THEN
            IF ( icalc.EQ.1 ) THEN
              wetperm = Strm(5, l)
              areamax = Wetper(l, 1)*strlen
            ELSE IF ( icalc.EQ.2 ) THEN
              DO i = 1, Isuzn
                Uzseep(l, i) = 0.0D0
                areamax = areamax + Wetper(l, i)*strlen
              END DO
            END IF
            fherr = h - hld
            thetas = Thts(l)
            fks = Uhc(l)
            thr = Thtr(l)
            epsilon = Eps(l)
            ha = -.15
            addrise = 0.0D0
            totflwt = 0.0D0
            thti1 = Uzthst(l, Nwavst(l, 1))
            fbcheck = 1.0D-12/Delt
            IF ( fbcheck.LT.5.0D-08 ) fbcheck = 5.0D-08
          END IF
        END IF
C
C26-----SET STREAMBED HYDRAULIC CONDUCTIVITY AND STREAM LEAKAGE TO
C         ZERO WHEN NOT AN ACTIVE CELL.
        IF ( Ibound(ic, ir, il).LE.0 ) THEN
          avhc = 0.0
          strleak = 0.0D0
          h = hstr
          IF ( icalc.LE.1 ) iskip = 1
        END IF
C
C27-----BEGIN COMPUTATION OF STREAM DEPTH FOR ACTIVE CELL.
C
C28-----COMPUTE FLOW AT MIDPOINT OF REACH IGNORING STREAMBED LEAKAGE.
        IF ( icalc.EQ.0 ) THEN
          flwmpt = flowin + 0.5D0*(runof-etstr+precip)
Crgn added next line 10/23/06
          flowc = flowin + runof-etstr+precip
          Sfrq(4, l) = width
          IF ( flwmpt.LT.NEARZERO ) flwmpt = 0.0D0
        END IF
        IF ( icalc.EQ.1 ) Sfrq(4, l) = width
        IF ( icalc.EQ.1 ) THEN
          flowc = flowin + (runof-etstr+precip)
          flwmpt = flowin + 0.5D0*(runof-etstr+precip)
          IF ( flwmpt.LT.NEARZERO ) flwmpt = 0.0D0
          IF ( flowc.LT.NEARZERO ) flowc = 0.0D0
          depth = (flwmpt/qcnst)**0.6D0
          IF ( depth.LT.NEARZERO ) THEN
            depth = 0.0D0
            hstr = strtop
          ELSE
            hstr = strtop + depth
          END IF
          cstr = (avhc*width*strlen)/sbdthk
        ELSE IF ( icalc.GE.2 ) THEN
          flwmpt = flowin + 0.5*runof
          flowc = flowin + runof
          IF ( flowc.LT.NEARZERO ) flowc = 0.0D0
C
C29-----CALCULATE AN INITIAL ESTIMATE OF FLOW IN CHANNEL
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
              CALL GWF1SFR2DPTH(flwest, Xsec, slope, Const, istsg,
     +                          nreach, Nsegdim, Seg(16, istsg),
     +                          Seg(17, istsg), wetperm, depth, itstr,
     +                          width, Iout, iprndpth)
            ELSE IF ( icalc.EQ.3 ) THEN
              depth = cdpth*(flwest**fdpth)
              width = awdth*(flwest**bwdth)
              wetperm = width
            ELSE IF ( icalc.EQ.4 ) THEN
              CALL GWF1SFR2TBD(flwest, Qstage, depth, width,
     +                         Iseg(2, istsg), Maxpts, Nsegdim, nreach,
     +                         istsg, Kkiter, Iout)
              wetperm = width
            END IF
            cstr = (avhc*wetperm*strlen)/sbdthk
            flowc = flowc + (precip-etstr)*width
          END IF
        END IF
C
C30-----ESTIMATE DEPTH USING BISECTION METHOD WHEN ICALC IS GREATER
C         THAN 0.
        iflg = 1
C30b----SKIP NEWTON METHOD WHEN ICALC IS 1 AND SURFACE INFLOW IS ZERO.
        IF ( icalc.EQ.1 .AND. hstr.LE.strtop ) iflg = 0
C30c----SKIP NEWTON METHOD WHEN REACH OUTSIDE ACTIVE AREA AND
C         ISKIP IS 1.
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
C32-----ESTIMATE FLOW AT ENDPOINT 1.
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
C         CHANGED AUGUST 10, 2004
            IF ( h.GT.sbot ) THEN
              flobot2 = (cstr*(strtop+enpt2-h))
            ELSE IF ( icalccheck.EQ.1 ) THEN
              flobot2 = CALCUNSATFLOBOT(enpt2, sbot, avhc, fks, thti1,
     +                  ha, thr, width, thetas, epsilon, sbdthk,
     +                  Foldflbt, l, areamax, Numcell, strlen, fbcheck,
     +                  Istrm, Nstrm, Kkiter, 0, fherrold, fherr,
     +                  Nwavst, Nuzst, Isuzn, maxwav, Nstrail)
            ELSE
              flobot2 = (cstr*(strtop+enpt2-sbot))
            END IF
            IF ( flobot2.GT.flowc ) flobot2 = flowc
            depth2 = ((flwmpt-0.5D0*flobot2)/qcnst)**0.6D0
            depth1 = ((flwen1-0.5D0*flobot1)/qcnst)**0.6D0
C
C34-----ESTIMATE DEPTH, WIDTH AND WETTED PERIMETER WHEN
C         ICALC IS GREATER THAN OR EQUAL TO 2.
          ELSE IF ( icalc.GE.2 ) THEN
            IF ( icalc.EQ.2 ) THEN
              CALL GWF1SFR2FLW(enpt2, Xsec, Const, istsg, Nsegdim,
     +                         Seg(16, istsg), Seg(17, istsg), slope,
     +                         wetperm2, flwen2, width2)
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
              CALL GWF1SFR2TBF(flwen2, Qstage, enpt2, width2,
     +                         Iseg(2, istsg), Maxpts, Nsegdim, nreach,
     +                         istsg, Kkiter, Iout, 0)
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
              flobot2 = CALCUNSATFLOBOT(enpt2, sbot, avhc, fks, thti1,
     +                  ha, thr, wetperm2, thetas, epsilon, sbdthk,
     +                  Foldflbt, l, areamax, Numcell, strlen, fbcheck,
     +                  Istrm, Nstrm, Kkiter, 0, fherrold, fherr,
     +                  Nwavst, Nuzst, Isuzn, maxwav, Nstrail)
            ELSE
              flobot2 = ((avhc*wetperm2*strlen/sbdthk)
     +                  *(strtop+enpt2-sbot))
            END IF
            flwmpt2 = flwmpt
            IF ( flobot2.GE.flowc+flwpet2 ) THEN
              flobot2 = flowc + flwpet2
              flwmpt2 = 0.5D0*(flowc+flwpet2)
            END IF
            flwen2 = flwmpt2
            IF ( icalc.EQ.2 ) THEN
              CALL GWF1SFR2DPTH(flwen1, Xsec, slope, Const, istsg,
     +                          nreach, Nsegdim, Seg(16, istsg),
     +                          Seg(17, istsg), wetperm1, depth1, itstr,
     +                          width1, Iout, iprndpth)
            ELSE IF ( icalc.EQ.3 ) THEN
              depth1 = cdpth*(flwen1**fdpth)
              width1 = awdth*(flwen1**bwdth)
              wetperm1 = width1
            ELSE IF ( icalc.EQ.4 ) THEN
              CALL GWF1SFR2TBD(flwen1, Qstage, depth1, width1,
     +                         Iseg(2, istsg), Maxpts, Nsegdim, nreach,
     +                         istsg, Kkiter, Iout)
            END IF
C
C36-----IF FLOW AT ENDPOINT 2 IS LESS THAN OR EQUAL TO ZERO, SET DEPTH2,
C             AND WIDTH2 TO ZERO, AND WETPERM2 TO ONE.
            IF ( flwen2.LE.0.0D0 ) THEN
              depth2 = 0.0D0
              width2 = 0.0D0
              wetperm2 = 1.0D0
C
C37-----OTHERWISE CALCULATE DEPTH2,WIDTH2,AND WETPERM2.
            ELSE IF ( icalc.EQ.2 ) THEN
              CALL GWF1SFR2DPTH(flwen2, Xsec, slope, Const, istsg,
     +                          nreach, Nsegdim, Seg(16, istsg),
     +                          Seg(17, istsg), wetperm2, depth2, itstr,
     +                          width2, Iout, iprndpth)
            ELSE IF ( icalc.EQ.3 ) THEN
              depth2 = cdpth*(flwen2**fdpth)
              width2 = awdth*(flwen2**bwdth)
              wetperm2 = width2
            ELSE IF ( icalc.EQ.4 ) THEN
              CALL GWF1SFR2TBD(flwen2, Qstage, depth2, width2,
     +                         Iseg(2, istsg), Maxpts, Nsegdim, nreach,
     +                         istsg, Kkiter, Iout)
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
C
C41-----CALCULATE FLOBOT1 AND FLOBOT2 FOR ICALC =1.
Cdep  Corrected depth1+dlh and depth2+dlh to be depth1 and depth2.
            IF ( icalc.EQ.1 ) THEN
              flwmdpt1 = qcnst*(depth1**FIVE_THIRDS)
              flwmdpt2 = qcnst*(depth2**FIVE_THIRDS)
              IF ( h.GT.sbot ) THEN
                flobot1 = cstr*((depth1+strtop)-h)
                flobot2 = cstr*((depth2+strtop)-h)
              ELSE IF ( icalccheck.EQ.1 ) THEN
                flobot1 = CALCUNSATFLOBOT(depth1, sbot, avhc, fks,
     +                    thti1, ha, thr, width, thetas, epsilon,
     +                    sbdthk, Foldflbt, l, areamax, Numcell, strlen,
     +                    fbcheck, Istrm, Nstrm, Kkiter, 0, fherrold,
     +                    fherr, Nwavst, Nuzst, Isuzn, maxwav, Nstrail)
                flobot2 = CALCUNSATFLOBOT(depth2, sbot, avhc, fks,
     +                    thti1, ha, thr, width, thetas, epsilon,
     +                    sbdthk, Foldflbt, l, areamax, Numcell, strlen,
     +                    fbcheck, Istrm, Nstrm, Kkiter, 0, fherrold,
     +                    fherr, Nwavst, Nuzst, Isuzn, maxwav, Nstrail)
              ELSE
                flobot1 = cstr*((depth1+strtop)-sbot)
                flobot2 = cstr*((depth2+strtop)-sbot)
              END IF
C
C42-----USE BISECTION WHEN FLOBOT1 IS LIMITED BY FLOW IN CHANNEL.
              IF ( flobot1.GE.flowc ) THEN
                enpt2 = depthp
                depthp = (enpt1+enpt2)*0.5D0
                IF ( h.GT.sbot ) THEN
                  flobotp = cstr*((depthp+strtop)-h)
                ELSE
                  flobotp = cstr*((depthp+strtop)-sbot)
                END IF
                IF ( 0.5D0*flobotp.GT.flwmpt ) flobotp = flowc
                depthx = ((flwmpt-0.5D0*flobotp)/qcnst)**0.6D0
                ibflg = 1
              ELSE
                fhstr1 = (flwmpt-0.5D0*flobot1) - (flwmdpt1)
                fhstr2 = (flwmpt-0.5D0*flobot2) - (flwmdpt2)
              END IF
            ELSE IF ( icalc.GE.2 ) THEN
C
C43-----CALCULATE NEWTON VARIABLES FOR ICALC =2.
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
                CALL GWF1SFR2FLW(deptha, Xsec, Const, istsg, Nsegdim,
     +                           Seg(16, istsg), Seg(17, istsg), slope,
     +                           wetperma, flwmdpta, widtha)
                CALL GWF1SFR2FLW(depthb, Xsec, Const, istsg, Nsegdim,
     +                           Seg(16, istsg), Seg(17, istsg), slope,
     +                           wetpermb, flwmdptb, widthb)
                CALL GWF1SFR2FLW(depthc, Xsec, Const, istsg, Nsegdim,
     +                           Seg(16, istsg), Seg(17, istsg), slope,
     +                           wetpermc, flwmdptc, widthc)
                CALL GWF1SFR2FLW(depthd, Xsec, Const, istsg, Nsegdim,
     +                           Seg(16, istsg), Seg(17, istsg), slope,
     +                           wetpermd, flwmdptd, widthd)
                CALL GWF1SFR2FLW(depth1, Xsec, Const, istsg, Nsegdim,
     +                           Seg(16, istsg), Seg(17, istsg), slope,
     +                           wetperm1, flwmdpt1, width1)
                CALL GWF1SFR2FLW(depth2, Xsec, Const, istsg, Nsegdim,
     +                           Seg(16, istsg), Seg(17, istsg), slope,
     +                           wetperm2, flwmdpt2, width2)
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
                CALL GWF1SFR2TBF(flwmdpta, Qstage, deptha, widtha,
     +                           Iseg(2, istsg), Maxpts, Nsegdim,
     +                           nreach, istsg, Kkiter, Iout, 0)
                CALL GWF1SFR2TBF(flwmdptb, Qstage, depthb, widthb,
     +                           Iseg(2, istsg), Maxpts, Nsegdim,
     +                           nreach, istsg, Kkiter, Iout, 0)
                CALL GWF1SFR2TBF(flwmdptc, Qstage, depthc, widthc,
     +                           Iseg(2, istsg), Maxpts, Nsegdim,
     +                           nreach, istsg, Kkiter, Iout, 0)
                CALL GWF1SFR2TBF(flwmdptd, Qstage, depthd, widthd,
     +                           Iseg(2, istsg), Maxpts, Nsegdim,
     +                           nreach, istsg, Kkiter, Iout, 0)
                CALL GWF1SFR2TBF(flwmdpt1, Qstage, depth1, width1,
     +                           Iseg(2, istsg), Maxpts, Nsegdim,
     +                            nreach, istsg, Kkiter, Iout, 0)
                CALL GWF1SFR2TBF(flwmdpt2, Qstage, depth2, width2,
     +                           Iseg(2, istsg), Maxpts, Nsegdim,
     +                           nreach, istsg, Kkiter, Iout, 0)
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
Cdep revised pp1,pp2,et1,and et2, wrong placment of parenthesis.
              pp1 = precip*(width1)+dlpp1*dlh
              pp2 = precip*(width2)+dlpp2*dlh
              et1 = etstr*(width1)+dlet1*dlh
              et2 = etstr*(width2)+dlet2*dlh
              cstr1 = ((wetperm1+dlwp1*dlh)*strleak)/sbdthk
              cstr2 = ((wetperm2+dlwp2*dlh)*strleak)/sbdthk
C
C47-----CALCULATE FLOBOT1 AND FLOBOT2 WHEN ICALC GREATER THAN 1.
Cdep    removed +dlh from calculation of flobot1 and flobot2
              IF ( h.GE.sbot ) THEN
                flobot1 = cstr1*((depth1+strtop)-h)
                flobot2 = cstr2*((depth2+strtop)-h)
              ELSE IF ( icalccheck.EQ.1 ) THEN
                flobot1 = CALCUNSATFLOBOT(depth1, sbot, avhc, fks,
     +                    thti1, ha, thr, wetperm1, thetas, epsilon,
     +                    sbdthk, Foldflbt, l, areamax, Numcell, strlen,
     +                    fbcheck, Istrm, Nstrm, Kkiter, 0, fherrold,
     +                    fherr, Nwavst, Nuzst, Isuzn, maxwav, Nstrail)
                flobot2 = CALCUNSATFLOBOT(depth2, sbot, avhc, fks,
     +                    thti1, ha, thr, wetperm2, thetas, epsilon,
     +                    sbdthk, Foldflbt, l, areamax, Numcell, strlen,
     +                    fbcheck, Istrm, Nstrm, Kkiter, 0, fherrold,
     +                    fherr, Nwavst, Nuzst, Isuzn, maxwav, Nstrail)
              ELSE
                flobot1 = cstr1*((depth1+strtop)-sbot)
                flobot2 = cstr2*((depth2+strtop)-sbot)
              END IF
C
C48-----DETERMINE IF LEAKAGE LIMITED BY FLOW IN CHANNEL.
              IF ( width1.GT.NEARZERO ) THEN
Cdep revised flwpet1, wrong placment of parenthesis.
                flwpet1 = precip*(width1)+dlpp1*dlh
     +                    - etstr*(width1)+dlet1*dlh
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
                  CALL GWF1SFR2FLW(depthp, Xsec, Const, istsg, Nsegdim,
     +                             Seg(16, istsg), Seg(17, istsg),
     +                             slope, wetpermp, flwx, widtha)
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
                  CALL GWF1SFR2TBF(flwx, Qstage, depthp, widthp,
     +                             Iseg(2, istsg), Maxpts, Nsegdim,
     +                             nreach, istsg, Kkiter, Iout, 0)
                  wetpermp = widthp
                END IF
                cstr1 = wetpermp*strleak/sbdthk
                IF ( h.GT.sbot ) THEN
                  flobotp = cstr1*((depthp+strtop)-h)
                ELSE IF ( icalccheck.EQ.1 ) THEN
                  flobotp = CALCUNSATFLOBOT(depthp, sbot, avhc, fks,
     +                      thti1, ha, thr, wetpermp, thetas, epsilon,
     +                      sbdthk, Foldflbt, l, areamax, Numcell,
     +                      strlen, fbcheck, Istrm, Nstrm, Kkiter, 0,
     +                      fherrold, fherr, Nwavst, Nuzst, Isuzn,
     +                      maxwav, Nstrail)
                ELSE
                  flobotp = cstr1*((depthp+strtop)-sbot)
                END IF
                IF ( flobotp.GT.flowc+flwpet1 ) flobotp = flowc +
     +               flwpet1
                flwmpt = flwmpt + 0.5D0*flwpet1
                flwx = flwmpt - 0.5D0*flobotp
                IF ( flwx.LT.NEARZERO ) THEN
                  depthx = 0.0D0
                  widthx = 0.0D0
                  wetpermx = 1.0D0
                ELSE IF ( icalc.EQ.2 ) THEN
                  CALL GWF1SFR2DPTH(flwx, Xsec, slope, Const, istsg,
     +                              nreach, Nsegdim, Seg(16, istsg),
     +                              Seg(17, istsg), wetpermp, depthx,
     +                              itstr, widthp, Iout, iprndpth)
                ELSE IF ( icalc.EQ.3 ) THEN
                  depthx = cdpth*(flwx**fdpth)
Crgn changed widhtp to widthx 12/5/06
                  widthx = awdth*(flwx**bwdth)
                  wetpermx = widthx
                ELSE IF ( icalc.EQ.4 ) THEN
                  CALL GWF1SFR2TBD(flwx, Qstage, depthx, widthx,
     +                             Iseg(2, istsg), Maxpts, Nsegdim,
     +                             nreach, istsg, Kkiter, Iout)
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
              dlfh = (fhstr1-fhstr2)/(depth1-depth2)
              IF ( DABS(dlfh).LE.dbleak ) dlfh = 0.0D0
              IF ( DABS(dlfh).GT.NEARZERO ) THEN
                dlh = -fhstr1/dlfh
              ELSE
                dlh = 0.0D0
              END IF
              depthp = depth1 + dlh
              IF ( iflg.GT.0 ) THEN
                IF ( (depthp.GE.enpt2) .OR. (depthp.LE.enpt1) ) THEN
                  IF ( DABS(dlh).GT.DABS(dlhold) .OR. 
     +                 depthp.LT.NEARZERO )THEN
                    depthp = (enpt1+enpt2)*0.5D0
                    ibflg = 1
                  END IF
                END IF
C
C51-----SET FLAGS TO DETERMINE IF NEWTON METHOD OSCILLATES OR
C        IF CONVERGENCE IS SLOW.
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
     +               DABS(dlh).GT.DABS(dlhold) ) iic = iic + 1
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
C53-----COMPUTE FLOBOTP ON BASIS OF DEPTHP AND THEN ESTIMATE DEPTHX
C         FROM FLOBOTP.
                IF ( icalc.EQ.1 ) THEN
                  IF ( h.GT.sbot ) THEN
                    flobotp = (cstr*(strtop+depthp-h))
                  ELSE IF ( icalccheck.EQ.1 ) THEN
                    flobotp = CALCUNSATFLOBOT(depthp, sbot, avhc, fks,
     +                        thti1, ha, thr, width, thetas, epsilon,
     +                        sbdthk, Foldflbt, l, areamax, Numcell,
     +                        strlen, fbcheck, Istrm, Nstrm, Kkiter, 0,
     +                        fherrold, fherr, Nwavst, Nuzst, Isuzn,
     +                        maxwav, Nstrail)
                  ELSE
                    flobotp = (cstr*(strtop+depthp-sbot))
                  END IF
                  IF ( flobotp.GE.flowc ) THEN
                    flobotp = flowc
                    IF ( DABS(enpt1-enpt2).LE.dbleak*0.000001D0 )
     +                   depthp = ((flwmpt-0.5D0*flobotp)/qcnst)**0.6D0
                  END IF
                  depthx = ((flwmpt-0.5D0*flobotp)/qcnst)**0.6D0
                ELSE IF ( icalc.GE.2 ) THEN
                  IF ( icalc.EQ.2 ) THEN
                    CALL GWF1SFR2FLW(depthp, Xsec, Const, istsg,
     +                               Nsegdim, Seg(16, istsg),
     +                               Seg(17, istsg), slope, wetpermp,
     +                               flwp, widthp)
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
                    CALL GWF1SFR2TBF(flwp, Qstage, depthp, widthp,
     +                               Iseg(2, istsg), Maxpts, Nsegdim,
     +                               nreach, istsg, Kkiter, Iout, 0)
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
     +                        *(strtop+depthp-h))
                  ELSE IF ( icalccheck.EQ.1 ) THEN
                    flobotp = CALCUNSATFLOBOT(depthp, sbot, avhc, fks,
     +                        thti1, ha, thr, wetpermp, thetas, epsilon,
     +                        sbdthk, Foldflbt, l, areamax, Numcell,
     +                        strlen, fbcheck, Istrm, Nstrm, Kkiter, 0,
     +                        fherrold, fherr, Nwavst, Nuzst, Isuzn,
     +                        maxwav, Nstrail)
                  ELSE
                    flobotp = ((avhc*wetpermp*strlen/sbdthk)
     +                        *(strtop+depthp-sbot))
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
                      CALL GWF1SFR2FLW(depthp, Xsec, Const, istsg,
     +                                 Nsegdim, Seg(16, istsg),
     +                                 Seg(17, istsg), slope, wetpermp,
     +                                 flwp, widthp)
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
                      CALL GWF1SFR2TBF(flwp, Qstage, depthp, widthp,
     +                                 Iseg(2, istsg), Maxpts, Nsegdim,
     +                                 nreach, istsg, Kkiter, Iout, 0)
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
                      flobotp = CALCUNSATFLOBOT(depthp, sbot, avhc, fks,
     +                          thti1, ha, thr, wetpermp, thetas,
     +                          epsilon, sbdthk, Foldflbt, l, areamax,
     +                          Numcell, strlen, fbcheck, Istrm, Nstrm,
     +                          Kkiter, 0, fherrold, fherr, Nwavst,
     +                          Nuzst, Isuzn, maxwav, Nstrail)
                    ELSE
                      flobotp = ((avhc*wetpermp*strlen/sbdthk)
     +                          *(strtop+depthp-sbot))
                    END IF
                    flwmpt = flwmpt + 0.5D0*flwpetp
                    flwx = flwmpt - 0.5D0*flobotp
                    IF ( flobotp.GE.flowc+flwpetp ) flobotp = flowc +
     +                   flwpetp
                  END IF
C
C55-----ESTIMATE DEPTHX WHEN ICALC IS GREATER THAN OR EQUAL TO 2.
                  IF ( flwx.LT.NEARZERO ) THEN
                    depthx = 0.0D0
                    widthx = 0.0D0
                    wetpermx = 1.0D0
                  ELSE IF ( icalc.EQ.2 ) THEN
                    CALL GWF1SFR2DPTH(flwx, Xsec, slope, Const, istsg,
     +                                nreach, Nsegdim, Seg(16, istsg),
     +                                Seg(17, istsg), wetpermx, depthx,
     +                                itstr, widthx, Iout, iprndpth)
                  ELSE IF ( icalc.EQ.3 ) THEN
                    depthx = cdpth*(flwx**fdpth)
                    widthx = awdth*(flwx**bwdth)
                    wetpermx = widthx
                  ELSE IF ( icalc.EQ.4 ) THEN
                    CALL GWF1SFR2TBD(flwx, Qstage, depthx, widthx,
     +                               Iseg(2, istsg), Maxpts, Nsegdim,
     +                               nreach, istsg, Kkiter, Iout)
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
C61-----SET DEPTH TO DEPTHP AND FLOBOT TO FLOBOTP WHEN ERROR IS LESS
C         THAN TOLERANCE.
            IF ( DABS(err).LT.dbleak ) THEN
              iflg = 0
              if(nreach.eq.8) then
                depth =depthp
              end if 
              depth = depthp
              flobot = flobotp
              IF ( icalc.GE.2 ) THEN
                width = widthp
                wetperm = wetpermp
                flowc = flowin + runof - etstr*width +
     +                  precip*width
                flwmpt = flowin +
     +                   0.5D0*(runof+precip*width-etstr*width-
     +                   flobot)
!dep  August 26, 2009 Added ELSE to recompute flwmpt after Newton iterations
              ELSE
                flowc = flowin + runof - etstr +
     +                  precip
                flwmpt = flowin +
     +                   0.5D0*(runof+precip-etstr-flobot) 
              END IF
            END IF
C
C62-----PRINT WARNING THAT REACH FAILED TO CONVERGE AFTER 100 ITERATIONS
C         AND SET DEPTH TO DEPTHP AND FLOBOT TO FLOBOTP.
            IF ( itot.GE.100 ) THEN
              iflg = 0
              WRITE (Iout, 9003) istsg, nreach, Kkiter, err, errold
 9003         FORMAT (//5X, '*** WARNING *** SFR FAILED TO ',
     +                'CONVERGE FOR SEGMENT ', I6, ' REACH NO. ', I6,
     +                ' MODFLOW ITERATION IS ', I6, ' LAST ITERATION ',
     +                G20.10, ' PREVIOUS ITERATION ', G20.10)
              depth = depthp
              flobot = flobotp
              IF ( icalc.GE.2 ) THEN
                width = widthp
                wetperm = wetpermp
                flowc = flowin + runof - etstr*width +
     +                  precip*width
                flwmpt = flowin +
     +                   0.5D0*(runof+precip*width-etstr*width-
     +                   flobot)
!dep  August 26, 2009 Added ELSE to recompute flwmpt after Newton iterations
              ELSE
                flowc = flowin + runof - etstr + precip
                flwmpt = flowin +
     +                   0.5D0*(runof+precip-etstr-flobot) 
              END IF
            END IF
            errold = err
            dlhold = dlh
            flobotold = flobot1
            IF ( ibflg.EQ.1 ) flobotold = flobotp
            Sfrq(4, l) = width
C
C63-----END OF NEWTON LOOP.
          END DO
C
C64-----DEFINE HSTR, CSTR, WIDTH, AND FLOWOT.
          hstr = depth + Strm(3, l)
          IF ( icalc.GE.2 ) cstr = (avhc*wetperm*strlen)/sbdthk
        END IF
C
C65-----ROUTE FLOW WITHOUT ANY STREAM LEAKAGE (FLOBOT IS ZERO) WHEN
C          MODEL CELL IS INACTIVE.
        IF ( iskip.NE.0 .OR. itot.EQ.0 ) THEN
          IF ( icalc.EQ.0 .OR. icalc.EQ.1 ) THEN
            flowc = flowin + runof + precip - etstr
            flwmpt = flowin + 0.5D0*(runof+precip-etstr)
!FLOW AT MIDPOINT CANNOT BE LESS THAN ZERO.
              IF( flwmpt.LT.NEARZERO ) flwmpt = 0.0D0
Crgn--- 10/3/07 added check for flwmpt to avoid NaN values.
              IF ( icalc.EQ.1 .AND. flwmpt.GT.NEARZERO ) THEN
                depth = (flwmpt/qcnst)**0.6D0
              ELSE
                depth = 0.0
              END IF
            IF ( flowc.GT.NEARZERO ) THEN
              IF ( runof.LT.NEARZERO ) THEN
                IF ( flowin+precip.GT.NEARZERO ) THEN
                  IF ( flowin+precip-etstr.LE.-(runof) ) THEN
                    runof = -(flowin+precip-etstr)
                  ELSE IF ( flowin+(precip+runof).LE.etstr ) THEN
                    etstr = flowin + precip + runof
                  END IF
                END IF
              ELSE IF ( flowin+precip+runof.LE.etstr ) THEN
                etstr = flowin + runof + precip - flobot
              ELSE IF ( flowin+precip+runof.LT.NEARZERO ) THEN
                etstr = 0.0D0
              END IF
              flowc = flowin + runof + precip - etstr
              flwmpt = flowin + 0.5D0*(runof+precip-etstr)
Cdep----3/16/2009    added second condition to if statement
            ELSE IF ( runof.LT.NEARZERO. AND. 
     +                 h-strtop.LT.NEARZERO ) THEN
              IF ( flowin+precip.LT.NEARZERO ) THEN
                runof = 0.0D0
                etstr = 0.0D0
              ELSE IF ( flowin+precip-etstr.LE.-(runof) ) THEN
                runof = -(flowin+precip-etstr)
              ELSE IF ( flowin+runof.LE.etstr ) THEN
                etstr = flowin + precip + runof
              END IF
            ELSE IF ( etstr.GE.flowin+runof+precip .AND. 
     +                h-strtop.LT.NEARZERO ) THEN
              etstr = flowin + runof + precip
Cdep----3/16/2009    removed last else statement
C              ELSE
C                etstr = 0.0D0
            END IF
          ELSE
crgn used width in calculations. 9/20/06
            IF ( icalc.EQ.2 ) THEN
              width = XSEC(6, istsg) - XSEC(3, istsg)
            ELSE IF ( icalc.EQ.3 ) THEN
              width = 10.0D0
            ELSE IF ( icalc.EQ.4 ) THEN
              width = Qstage((1+2*Iseg(2,istsg)), istsg)
     +                + Qstage(3*Iseg(2, istsg), istsg)/2.0D0
            END IF
            flowc = flowin + runof + (precip-etstr)*width
!dep August 26, 2009 fixed calculation of flwmpt
            flwmpt = flowin + 0.5D0*(runof+(precip-etstr)*width)
            IF ( flowc.LT.NEARZERO ) THEN
              flowc = 0.0D0
              etstr = (flowin + runof)/width + precip
              flwmpt = 0.0D0
            END IF
          END IF
C
C66----COMPUTE STREAM LEAKAGE IF STREAM DEPTH WAS NOT COMPUTED
C        USING EITHER BISECTION OR NEWTON METHOD.
          IF ( iskip.EQ.0 ) THEN
            cstr = strleak*width/sbdthk
crgn added next 4 lines.
            IF (icalc.GT.1 ) THEN
              etstr = etstr*width
              precip = precip*width
            END IF
            IF ( flowc.LT.NEARZERO ) THEN
              hstr = Strm(3, l)
              IF ( h.LT.hstr ) THEN
                flobot = 0.0D0
              ELSE
                flobot = cstr*(hstr-h)
                IF ( runof.LT.NEARZERO ) THEN
                  IF ( flowin+precip-flobot.LT.NEARZERO ) THEN
                    runof = 0.0D0
                    etstr = 0.0D0
                  ELSE IF ( flowin+precip-flobot-etstr.LE.-
     +                      runof ) THEN
                    runof = -(flowin+precip-flobot-etstr)
                  ELSE IF ( flowin+precip-flobot+runof.LE.etstr )
     +                      THEN
                    etstr = flowin + precip - flobot + runof
                  END IF
                ELSE IF ( flowin+runof+precip-flobot.LE.etstr )
     +                    THEN
                  etstr = flowin + runof + precip - flobot
                ELSE IF ( flowin+runof+precip-flobot.LT.
     +                    NEARZERO ) THEN
                  etstr = 0.0D0
                END IF
              END IF
              flowc = flowin + runof + precip - etstr
            ELSE IF ( h.LT.sbot ) THEN
              flobot = cstr*(hstr-sbot)
            ELSE
              flobot = cstr*(hstr-h)
            END IF
          ELSE
            flobot = 0.0D0
          END IF
        END IF
        IF ( flowc.GT.NEARZERO .AND. icalccheck.EQ.1 ) THEN
          IF ( h.LT.sbot ) THEN
            flobot = CALCUNSATFLOBOT(depth, sbot, avhc, fks, thti1, ha,
     +               thr, wetperm, thetas, epsilon, sbdthk, Foldflbt, l,
     +               areamax, Numcell, strlen, fbcheck, Istrm, Nstrm,
     +               Kkiter, 1, fherrold, fherr, Nwavst, Nuzst, Isuzn,
     +               maxwav, Nstrail)
          ELSE
            DO i = 1, Isuzn
              Uzseep(l, i) = 0.0D0
            END DO
          END IF
        ELSE IF ( icalccheck.EQ.1 ) THEN
          DO i = 1, Isuzn
            Uzseep(l, i) = 0.0D0
          END DO
        END IF
        IF ( flobot.GE.flowc ) flobot = flowc
C
C67-----CALCULATE SEEPAGE THROUGH UNSATURATED ZONE.
        IF ( h.LT.sbot .AND. icalccheck.EQ.1 )
     +       CALL CALC_UNSAT_INFIL(flobot, Isuzn, Uzseep, Uzthst, thr,
     +       ha, thetas, epsilon, fks, avhc, depth, sbdthk, Nstotrl,
     +       Wetper, Uzwdth, Nuzst, flowc, l, Nwavst, strlen,
     +       iwidthcheck, icalc)
C
C68-----STREAMFLOW OUT EQUALS STREAMFLOW IN MINUS LEAKAGE. 
        flowot = flowc - flobot
        IF ( flowot.LT.NEARZERO ) THEN
          flowot = 0.0D0
          flobot = flowc
          flwmpt = 0.5D0*flowc
          depth = 0.0D0
        END IF
C
C69-----STORE STREAM INFLOW, OUTFLOW, LEAKAGE, STAGE, AND STREAMBED
C         CONDUCTANCE FOR EACH REACH.
        Strm(9, l) = flowot
        Strm(10, l) = flowin
        Strm(11, l) = flobot
        Strm(15, l) = hstr
        Strm(16, l) = cstr
        IF ( icalc.GE.1 ) Strm(7, l) = depth
        IF ( icalc.GE.2 ) Strm(5, l) = width
        IF ( icalc.GE.2 ) Strm(20, l) = wetperm
C
C70-----STORE OUTFLOW FOR LAST REACH IN SGOTFLW LIST AND IN
C         STRIN FOR LAKE PACKAGE.
        IF ( l.EQ.Nstrm ) THEN
          Sgotflw(istsg) = Strm(9, l)
          IF ( Iunitlak.GT.0 ) Strin(istsg) = Strm(9, l)
        END IF
C
C71-----ROUTE SEEPAGE THROUGH UNSATURATED ZONE.
        kerp = 0
        IF ( h.LT.sbot .OR. hld.LT.sbot ) kerp = 1
        IF ( kerp.EQ.1 .AND. Iss.EQ.0 .AND. icalccheck.EQ.1 ) THEN
          CALL ROUTWAVESIT(l, Nuzst, Delt, seep, h, hld, Uzdpit, Uzthit,
     +                     Uzflit, Uzspit, Itrlit, Ltrlit, Uzdpst,
     +                     Uzthst, Uzflst, Uzspst, Itrlst, Ltrlst,
     +                     Nwavst, Nstrail, Nstotrl, thr, thetas, fks,
     +                     epsilon, Isuzn, Uzwdth, Uzflwt, ic, ir, il,
     +                     Strm, Istrm, Uzolsflx, Nstrm, Ncol, Nrow,
     +                     Nlay, Iout, iwidthcheck, Totim, Nsfrsets,
     +                     strtop, Uzseep, icalc, Kkstp, Sbot)
C
C72-----SUM SEEPAGE TO WATER TABLE.
          totflwt = 0.0D0
          IF ( icalc.EQ.2 ) THEN
            DO i = 1, Isuzn
              totflwt = totflwt + Uzflwt(l, i)
            END DO
          ELSE IF ( icalc.EQ.1 ) THEN
            totflwt = Uzflwt(l, 1)
          END IF
          addrhs = totflwt/Delt
          IF ( h.GT.sbot ) addrise = totflwt/Delt
        END IF
        IF ( icalccheck.EQ.0 ) THEN
          totflwt = 0.0D0
          addrhs = flobot
        END IF
        IF ( Iuzt.EQ.0 .AND. h.LT.sbot ) addrhs = flobot
        IF ( Iss.NE.0 ) addrhs = flobot
C
C73-----STORE FLOWS NEEDED FOR SENSITIVITIES.
        Sfrq(1, l) = flwmpt
        Sfrq(2, l) = flowc
        Sfrq(3, l) = flobot
        Sfrq(5, l) = flowin
C
C74-----ADD TERMS TO RHS AND HCOF IF FLOBOT IS NOT ZERO.
        IF ( DABS(flobot).GT.0.0D0 ) THEN
C
C75-----ADD TERMS TO RHS AND HCOF WHEN GROUND-WATER HEAD GREATER THAN
C         STREAMBED BOTTOM ELEVATION.
          IF ( Hnew(ic, ir, il).LE.sbot ) THEN
            Rhs(ic, ir, il) = Rhs(ic, ir, il) - addrhs
          ELSE IF ( flobot.LT.flowc ) THEN
C
C76-----STREAM LEAKAGE IS HEAD DEPENDENT.
            Rhs(ic, ir, il) = Rhs(ic, ir, il) - (cstr*hstr) - addrise
            Hcof(ic, ir, il) = Hcof(ic, ir, il) - cstr
          ELSE
C
C77-----CONSTANT STREAMBED LEAKAGE IS LIMITED BY STREAMFLOW OR
C         STREAMBED CONDUCTANCE IN REACH.
            Rhs(ic, ir, il) = Rhs(ic, ir, il) - flobot - addrise
          END IF
C
C78-----ADD TERM ONLY TO RHS WHEN GROUND-WATER HEAD IS LESS THAN
C         STREAMBED BOTTOM ELEVATION.
        ELSE IF ( h.LT.sbot .OR. hld.LT.sbot ) THEN
          Rhs(ic, ir, il) = Rhs(ic, ir, il) - addrhs
        END IF
      END DO
C
C79-----RETURN.
      RETURN
      END SUBROUTINE GWF1SFR2FM
C
C-------SUBROUTINE GWF1SFR2BD
      SUBROUTINE GWF1SFR2BD(Strm, Istrm, Hnew, Ibound, Nstrm, Ncol,
     +                      Nrow, Nlay, Nss, Seg, Iseg, Iotsg, Xsec,
     +                      Idivar, Const, Maxpts, Qstage, Sgotflw,
     +                      Dvrsflw, Nlakesar, Stgold, Stgnew, Vol,
     +                      Strin, Strout, Theta, Delt, Kstp, Kper,
     +                      Vbvl, Vbnm, Msum, Istcb1, Istcb2, Icbcfl,
     +                      Buff, Iout, Iptflg, I15, Iunitgage, Igglst,
     +                      Numgage, Pertim, Totim, Sfrq, Nsegdim,
     +                      Iunitlak, Dleak, Hold, Uzdpst, Uzthst,
     +                      Uzspst, Uzflst, Eps, Thtr, Thts, Ltrlst,
     +                      Itrlst, Itrlsth, Uzflwt, Uzstor, Delstor,
     +                      Nwavst, Nstrail, Nstotrl, Iuzt, Isuzn,
     +                      Nuzst, Uzolsflx, Uzwdth, Loop, Uhc, Ndpelev,
     +                      Dpthinc, Wetper, Nsfrsets, Uzseep, Foldflbt,
     +                      Numcell, Nuzrow, Nuzcol, Avwat, Wat1,
     +                      Numave, Avdpt, Sfruzbd, Iss, Ibudfl, Fxlkot)
C     *****************************************************************
C     CALCULATE VOLUMETRIC GROUND-WATER BUDGET FOR STREAMS AND SUM
C     STREAMFLOWS IN MODELED AREA
C     VERSION  2.7: MARCH 16, 2009
C     *****************************************************************
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     FUNCTIONS
C     ------------------------------------------------------------------
      REAL CALCUNSATFLOBOT
      EXTERNAL CALCUNSATFLOBOT
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL Hold
      DOUBLE PRECISION Hnew, Uzspst, Uzflst, Uzdpst, Uzthst, Uzseep,
     +                 Uzolsflx, Uzflwt, Wetper, Uzwdth, Delstor,
     +                 Uzstor, Foldflbt, Eps, Thts, Thtr
      REAL Avdpt, Avwat, Buff, Const, Delt, Dleak, Dpthinc, Dvrsflw,
!     +     Pertim, Qstage, Seg, Sfruzbd, Sgotflw, Stgnew, Stgold, Strin,
     +     Pertim, Qstage, Seg, Sfruzbd, Sgotflw, Strin,
     +     Strm, Strout, Theta, Totim, Uhc, Vbvl, Vol, Wat1, Xsec
Cdep  Added Fxlkot to subroutine (specified lake outflow)
      REAL Fxlkot
      INTEGER I15, Ibound, Icbcfl, Idivar, Igglst, Iotsg, Iout, Iptflg,
     +        Iseg, Iss, Istcb1, Istcb2, Istrm, Isuzn, Itrlst, Itrlsth,
     +        Iunitgage, Iunitlak, Iuzt, Kper, Kstp, Loop, Ltrlst
      INTEGER Maxpts, Msum, Ncol, Ndpelev, Nlakesar, Nlay, Nrow,
     +        Nsegdim, Nsfrsets, Nss, Nstotrl, Nstrail, Nstrm, Numave,
     +        Numcell, Numgage, Nuzcol, Nuzrow, Nuzst, Nwavst, Ibudfl
      CHARACTER*16 Vbnm(Msum)
      DIMENSION Strm(24, Nstrm), Istrm(5, Nstrm), Hold(Ncol, Nrow, Nlay)
     +          , Hnew(Ncol, Nrow, Nlay), Ibound(Ncol, Nrow, Nlay),
     +          Buff(Ncol, Nrow, Nlay), Seg(26, Nsegdim), Vbvl(4, Msum),
     +          Iseg(4, Nsegdim), Iotsg(Nsegdim), Xsec(16, Nsegdim),
     +          Idivar(2, Nsegdim), Qstage(Maxpts, Nsegdim),
     +          Sgotflw(Nss), Dvrsflw(Nss),Strin(Nss), Strout(Nss)
Cdep   Stgold and Stgnew DOUBLE PRECISION 
      DOUBLE PRECISION   Stgold(Nlakesar), Stgnew(Nlakesar)        
!     +          Sgotflw(Nss), Dvrsflw(Nss), Stgold(Nlakesar),
!     +          Strin(Nss), Strout(Nss), Stgnew(Nlakesar)
Cdep  Dimension Fxlkot to number of stream segments
      DIMENSION Fxlkot(Nss)
      DIMENSION Igglst(4, Numgage), Vol(Nlakesar)
      DIMENSION Uzspst(Nuzst, Nstotrl), Uzflst(Nuzst, Nstotrl),
     +          Uzthst(Nuzst, Nstotrl), Uzolsflx(Nuzst, Isuzn)
      DIMENSION Uzdpst(Nuzst, Nstotrl), Uzflwt(Nuzst, Isuzn),
     +          Uzstor(Nuzst, Isuzn), Delstor(Nuzst, Isuzn),
     +          Uzwdth(Nuzst, Isuzn), Avdpt(Nuzst, Numave)
      DIMENSION Eps(Nuzst), Thts(Nuzst), Thtr(Nuzst), Uhc(Nuzst),
     +          Ltrlst(Nuzst, Nstotrl), Itrlst(Nuzst, Nstotrl),
     +          Itrlsth(Nstotrl), Nwavst(Nuzst, Isuzn), Loop(Isuzn),
     +          Foldflbt(Nuzst)
      DIMENSION Uzseep(Nuzst, Isuzn)
      DIMENSION Ndpelev(Nuzst, Isuzn), Avwat(Nuzst, Numave),
     +          Wat1(Nuzst, Numave), Sfruzbd(6)
      DIMENSION Dpthinc(Nuzst, Isuzn), Wetper(Nuzst, Isuzn)
      REAL Sfrq(5, Nstrm)

C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      REAL areamax, avhc, fherr, fherrold, fks, ha, prcnt, prcntdif,
     +     qcnst, rin, rout, stgon, strlen, thti1, zero
      INTEGER i, ibd, iblst, ibdlbl, ibdst, ibstlb, ic, icalc, idivseg,
     +        iflwhi, iflwlw, il, ilay, Iout1, Iout2, iprior, iprvsg,
     +        ir, istglw, istp, istsg, itrib, iupseg, iwidthcheck, kss,
     +        l, lk, ll, ndum, nreach, nstrpts, maxwav, icalccheck
      DOUBLE PRECISION awdth, bwdth, h, hstr, sbot, cstr, ratin, ratout,
     +                 flowin, flobot, flow, flowot, dif, sbdthk, upflw,
     +                 trbflw, width, wetperm, runof, precip,
     +                 etstr, dlkstr, slope, flwlw, flwhi, stglw, stghi,
     +                 dflwlw, dflwhi, dstglw, dstghi, dlglak, dlgslp,
     +                 dlgflw, cdpth, fdpth, hdiff, grad, depth, hld,
     +                 fbcheck, totflwt, totdelstor, totuzstor, thetas,
     +                 epsilon, thr, gwflow, dvrsn
C     ------------------------------------------------------------------
C     LOCAL STATIC VARIABLES
C     ------------------------------------------------------------------
      REAL CLOSEZERO
      DOUBLE PRECISION FIVE_THIRDS, NEARZERO
      PARAMETER (FIVE_THIRDS=5.0D0/3.0D0, NEARZERO=1.0D-30)
      PARAMETER (CLOSEZERO=1.0E-15)
      CHARACTER*16 text, strtxt, txtlst
      DATA text/'  STREAM LEAKAGE'/
      DATA strtxt/'STREAMFLOW OUT  '/
      DATA txtlst/'STREAM LISTING  '/
      DATA iwidthcheck/1/
C     -----------------------------------------------------------------
C1------INITIALIZE CELL BY CELL FLOW TERM FLAG (IBD) AND
C         ACCUMULATORS (RATIN AND RATOUT).
      ibd = 0
      ibdst = 0
      iblst = 0
      zero = 0.0
      ratin = zero
      ratout = zero
      maxwav = Nsfrsets*Nstrail
      IF ( Iuzt.EQ.1 ) THEN
        Sfruzbd(4) = zero
        Sfruzbd(5) = zero
        Sfruzbd(6) = zero
      END IF
C
C1b-----PRINT STREAM RESULTS WHENEVER BUDGET TERMS ARE PRINTED.
Cdep    REVISED TO ALLOW FOR COMPACT AND NONCOMPACT BUDGETS.
      IF ( Istcb1.EQ.-1 .AND. Icbcfl.NE.0 ) THEN
        ibd = -1
        iblst = -1
      ELSE IF ( Istcb1.EQ.-1 .AND. Ibudfl.GT.0 ) THEN
        iblst = -1
      ELSE IF ( Istcb1.GT.0 .AND. Icbcfl.NE.0 ) THEN
        ibd = Icbcfl
        Iout1 = Istcb1
      END IF
      IF ( Istcb2.GT.0 .AND. Icbcfl.NE.0 ) THEN
        ibdst = -1
        Iout2 = Istcb2
      ELSE IF ( Istcb2.LT.0 .AND. Icbcfl.NE.0 ) THEN
        ibdst = Icbcfl
        Iout2 = ABS(Istcb2)
      END IF
      ibdlbl = 0
      ibstlb = 0
C
C2------WRITE HEADER WHEN CELL-BY-CELL FLOWS WILL BE SAVED AS A LIST.
      IF ( ibd.EQ.2 ) CALL UBDSV2 (Kstp, Kper, text, Iout1, Ncol, Nrow, 
     +                             Nlay, Nstrm, Iout, Delt, Pertim,
     +                             Totim, Ibound)
      IF ( ibdst.EQ.2 ) CALL UBDSV2 (Kstp, Kper, strtxt, Iout2, Ncol, 
     +                               Nrow, Nlay, Nstrm, Iout, Delt, 
     +                               Pertim, Totim, Ibound)      
C
C3------CLEAR BUFFERS.
      DO il = 1, Nlay
        DO ir = 1, Nrow
          DO ic = 1, Ncol
            Buff(ic, ir, il) = zero
          END DO
        END DO
      END DO
C
C4------INITIALIZE SEGMENT INFLOWS AND OUTFLOWS TO ZERO FOR LAKE PACKAGE
C         WHEN THERE ARE STREAMS.
      iprvsg = -1
      IF ( Iunitlak.GT.0 ) THEN
        DO i = 1, Nss
          Strin(i) = zero
Cdep          Strout(i) = zero
          Fxlkot(i) = zero
        END DO
      END IF
C
C5------DETERMINE LAYER, ROW, COLUMN OF EACH REACH.
      DO l = 1, Nstrm
        gwflow = 0.0D0
        ll = l - 1
        il = Istrm(1, l)
        ir = Istrm(2, l)
        ic = Istrm(3, l)
C
C6------DETERMINE STREAM SEGMENT AND REACH NUMBER.
        istsg = Istrm(4, l)
        nreach = Istrm(5, l)
        icalc = Iseg(1, istsg)
        IF ( icalc.EQ.1 .OR. icalc.EQ.2 ) slope = Strm(2, l)
        IF ( icalc.EQ.4 ) nstrpts = Iseg(2, istsg)
C
C7------SET FLOWIN EQUAL TO STREAM SEGMENT INFLOW IF FIRST REACH.
        IF ( nreach.EQ.1 ) THEN
          IF ( Iseg(3, istsg).EQ.5 ) flowin = Seg(2, istsg)
C
C8------STORE OUTFLOW FROM PREVIOUS SEGMENT IN SGOTFLW LIST AND IN
C         STRIN FOR LAKE PACKAGE.
          IF ( istsg.GT.1 ) THEN
            iprvsg = Istrm(4, ll)
            Sgotflw(iprvsg) = Strm(9, ll)
            IF ( Iunitlak.GT.0 ) Strin(iprvsg) = Strm(9, ll)
          END IF
C
C9-------COMPUTE INFLOW OF A STREAM SEGMENT EMANATING FROM A LAKE.
Cdep   Revised this section because outflow computed in Lake Package.
          IF ( (Iunitlak.GT.0) .AND. (Idivar(1,istsg).LT.0) ) THEN
            lk = IABS(Idivar(1, istsg))
            IF ( Seg(2, istsg).GT.CLOSEZERO .AND. 
     +           Vol(lk).GT.CLOSEZERO ) THEN
              IF( Seg(2, istsg)*Delt-Vol(lk).LT.-CLOSEZERO ) THEN
                Fxlkot(istsg) = Seg(2, istsg)
              ELSE
                Fxlkot(istsg) = Vol(lk)/Delt
              END IF
                  flowin = Fxlkot(istsg)
            ELSE IF ( Seg(2, istsg).LT.-CLOSEZERO ) THEN
              WRITE (Iout, 9001) istsg
 9001         FORMAT (/5X, '*** WARNING *** NEGATIVE LAKE DIVERSION ',
     +                'NOT ALLOWED; SEG = ', I6, /10X,
     +                'SFR2BD CODE WILL ASSUME FLOW = 0.0'/)
              Seg(2, istsg) = 0.0
              Fxlkot(istsg) = flowin
            END IF
C
C10-----SPECIFIED FLOW FROM LAKE IS ZERO AND ICALC IS ZERO.
            IF ( icalc.EQ.0 ) THEN
              flowin = Fxlkot(istsg)
            END IF
C10Bdep    OUTFLOW FROM LAKE NOW COMPUTED IN LAKE PACKAGE.
            IF ( Fxlkot(istsg).LE.CLOSEZERO ) THEN
              flowin = STROUT(istsg)
            END IF
          END IF
Cdep  Code between comment lines 11 through 14 removed from SFR2 version
Cdep   because outflow from lake is computed in revised LAKE Package.
C
C15-----COMPUTE ONE OR MORE DIVERSIONS FROM UPSTREAM SEGMENT.
Crgn&dep   revised computation of diversions and added subroutine
            IF( istsg.GT.1 )THEN
              DO kss = 2, Nss
                upflw = Sgotflw(istsg-1)
                idivseg = kss
                IF( Idivar(1,kss).EQ.istsg-1 ) THEN
                   dvrsn = Seg(2,idivseg)
                   iprior = Idivar(2,kss)
                  CALL GWF2SFR2DIVERS(iprior, idivseg, upflw, dvrsn)
                  Dvrsflw(kss) = dvrsn
                  Sgotflw(istsg-1) = Sgotflw(istsg-1) - dvrsn
                END IF
              END DO
C
C20-----SET FLOW INTO DIVERSION IF SEGMENT IS DIVERSION.
              IF( Iseg(3,istsg).EQ.6 ) THEN
                IF( Idivar(1,istsg).GT.0 ) flowin = Dvrsflw(istsg)
              END IF
            END IF
C
C22-----SUM TRIBUTARY OUTFLOW AND USE AS INFLOW INTO DOWNSTREAM SEGMENT.
          IF ( istsg.GE.1 .AND. Iseg(3, istsg).EQ.7 ) THEN
            itrib = 1
            flowin = 0.0D0
            DO WHILE ( itrib.LE.Nss )
              IF ( istsg.EQ.Iotsg(itrib) ) THEN
                trbflw = Sgotflw(itrib)
                flowin = flowin + trbflw
              END IF
              itrib = itrib + 1
            END DO
            flowin = flowin + Seg(2, istsg)
C
C23-----CHECK IF "FLOW" IS WITHDRAWAL, THAT WATER IS AVAILABLE.
            IF ( flowin.LT.0.0D0 ) THEN
              flowin = 0.0D0
              WRITE (Iout, 9003) istsg
 9003         FORMAT (//2X, '*** WARNING *** FLOW INTO DIVERSIONARY ',
     +                'STREAM SEGMENT No. ', I6, ' WAS NEGATIVE; ',
     +                'FLOWIN RE-SET = 0.0'/)
            END IF
          END IF
C
C24-----SET INFLOW EQUAL TO OUTFLOW FROM UPSTREAM REACH, WHEN REACH
C         GREATER THAN 1.
        ELSE IF ( nreach.GT.1 ) THEN
          flowin = Strm(9, ll)
        END IF
C
C25-----SEARCH FOR UPPER MOST ACTIVE CELL IN STREAM REACH.
        ilay = il
        TOPCELL: DO WHILE ( ilay.LE.Nlay )
          IF ( Ibound(ic, ir, ilay).EQ.0 ) THEN
            ilay = ilay + 1
          ELSE
            EXIT TOPCELL
          END IF
        END DO TOPCELL
C
C26----DETERMINE LEAKAGE THROUGH STREAMBED.
        depth = Strm(7, l)
        IF ( icalc.EQ.0 ) depth = Strm(15, l) - Strm(3, l)
        hstr = Strm(15, l)
        cstr = Strm(16, l)
        width = Strm(5, l)
        sbot = Strm(4, l)
        sbdthk = Strm(8, l)
        strlen = Strm(1, l)
        icalccheck = 0
        totflwt = 0.0D0
        totdelstor = 0.0D0
        IF ( icalc.GE.2 ) wetperm = Strm(20, l)
        IF ( Iuzt.EQ.1 ) THEN
          IF ( icalc.EQ.1 .OR. icalc.EQ.2 ) icalccheck = 1
          IF ( icalccheck.EQ.1 ) THEN
            fherr = 0.0
            fherrold = 0.0
            fks = Uhc(l)
            thr = Thtr(l)
            epsilon = Eps(l)
            thetas = Thts(l)
            ha = -0.15
            areamax = 0.0
            fbcheck = 1.0D-12/Delt
            IF ( fbcheck.LT.5.0D-8 ) fbcheck = 5.0D-8
            avhc = Strm(6, l)
            thti1 = Uzthst(l, Nwavst(l, 1))
            IF ( icalc.EQ.2 ) THEN
              DO i = 1, Isuzn
                Uzseep(l, i) = 0.0D0
                areamax = areamax + Wetper(l, i)*strlen
              END DO
            ELSE IF ( icalc.EQ.1 ) THEN
              wetperm = width
              Wetper(l, 1) = width
              Uzseep(l, 1) = 0.0D0
              areamax = Wetper(l, 1)*strlen
            END IF
          END IF
        END IF
          IF ( icalc.EQ.1 ) THEN
            qcnst = CONST*width*SQRT(slope)/SEG(16, istsg)
          ELSE IF ( icalc.EQ.3 ) THEN
            awdth = SEG(14, istsg)
            bwdth = SEG(15, istsg)
          END IF
        IF ( ilay.LE.Nlay ) il = ilay
        IF ( ilay.LE.Nlay .AND. Ibound(ic, ir, il).GT.0 ) THEN
C
C27-----COMPUTE HEAD DIFFERENCE ACROSS STREAMBED.
Cdep  Revised hld.
          h = Hnew(ic, ir, il)
          hld = Strm(24, l)
          Strm(24, l) = h
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
C30----COMPUTE FLOW IN STREAM CHANNEL AND SET LEAKAGE EQUAL TO FLOW
C        IF LEAKAGE MORE THAN FLOW.
        runof = Strm(12, l)
        IF ( icalc.LE.1 ) THEN
          precip = Strm(14, l)
          etstr = Strm(13, l)
        ELSE IF ( icalc.GE.2 .AND. icalc.LE.4 ) THEN
C
C31-----NO PRECIPITATION OR ET FROM CHANNEL WHEN WIDTH IS ZERO.
          precip = Strm(14, l)*width
          etstr = Strm(13, l)*width
        END IF
        flow = flowin + runof + precip - etstr
        IF ( flow.LT.NEARZERO ) THEN
          IF ( icalc.EQ.0 ) depth = 0.0D0
          IF ( flobot.GE.0.0D0 ) THEN
            flobot = 0.0D0
            IF ( runof.LT.NEARZERO ) THEN
              IF ( flowin+precip.LT.NEARZERO ) THEN
                runof = 0.0D0
                etstr = 0.0D0
              ELSE IF ( ABS(runof).GE.flowin+precip-etstr ) THEN
                runof = flowin + precip - etstr
              ELSE IF ( etstr.GE.flowin+precip+runof ) THEN
                etstr = flowin + precip + runof
              END IF
            ELSE IF ( flowin+runof+precip.GT.NEARZERO ) THEN
              etstr = flowin + runof + precip
            ELSE
              etstr = 0.0D0
            END IF
          ELSE IF ( runof.LT.NEARZERO ) THEN
            IF ( flowin+precip-flobot.LT.NEARZERO ) THEN
              runof = 0.0D0
              etstr = 0.0D0
            ELSE IF ( ABS(runof).GE.flowin+precip-flobot-etstr )
     +                THEN
              runof = -(flowin+precip-flobot-etstr)
            ELSE IF ( etstr.GE.flowin+precip-flobot+runof ) THEN
              etstr = flowin + precip - flobot + runof
            END IF
          ELSE IF ( etstr.GT.flowin+runof+precip-flobot ) THEN
            etstr = flowin + runof + precip - flobot
          ELSE IF ( flowin+runof+precip-flobot.LT.NEARZERO ) THEN
            etstr = 0.0D0
          END IF
          flow = flowin + runof + precip - etstr
        END IF
        IF ( flobot.GE.flow ) THEN
          flobot = flow
          IF ( icalc.EQ.0 ) depth = 0.0D0
        END IF
        IF ( icalccheck.EQ.1 ) THEN
          IF ( h.LT.sbot ) THEN
            flobot = CALCUNSATFLOBOT(depth, sbot, avhc, fks, thti1, ha,
     +               thr, wetperm, thetas, epsilon, sbdthk, Foldflbt, l,
     +               areamax, Numcell, strlen, fbcheck, Istrm, Nstrm,
     +               Kper, 0, fherrold, fherr, Nwavst, Nuzst, Isuzn,
     +               maxwav, Nstrail)
            IF ( flobot.GE.flow ) flobot = flow
            CALL CALC_UNSAT_INFIL(flobot, Isuzn, Uzseep, Uzthst, thr,
     +                            ha, thetas, epsilon, fks, avhc, depth,
     +                            sbdthk, Nstotrl, Wetper, Uzwdth,
     +                            Nuzst, flow, l, Nwavst, strlen,
     +                            iwidthcheck, icalc)
          ELSE
            DO i = 1, Isuzn
              Uzseep(l, i) = 0.0D0
            END DO
          END IF
        END IF
        flowot = flow - flobot
C
C32-----STORE STREAM INFLOW, OUTFLOW, AND LEAKAGE FOR EACH REACH.
        Strm(9, l)  = flowot
        Strm(10, l) = flowin
        Strm(11, l) = flobot
        Strm(17, l) = hdiff
        Strm(18, l) = grad
        Strm(19, l) = h
C
C33-----STORE OUTFLOW FROM LAST REACH IN LAST SEGMENT IN STRIN
C         LIST FOR LAKE PACKAGE.
          IF ( Iunitlak.GT.0 ) THEN
            IF ( l.EQ.NSTRM .AND. istsg.EQ.NSS ) STRIN(istsg) = flowot
          END IF
C
C34-----STORE OUTFLOW FOR LAST REACH IN SGOTFLW LIST AND IN STRIN FOR
C         LAKE PACKAGE.
        IF ( l.EQ.Nstrm ) THEN
          Sgotflw(istsg) = Strm(9, l)
          IF ( Iunitlak.GT.0 ) Strin(istsg) = Strm(9, l)
        END IF
C
C35-----ADD RATES TO BUFFERS.
        IF ( icalccheck.EQ.1 .AND. Iss.EQ.0 ) THEN
          CALL UZMASSBAL(Strm, Istrm, h, Nstrm, Buff, Iout, hld, Uzdpst,
     +                   Uzthst, Uzspst, Uzflst, epsilon, thr, thetas,
     +                   Ltrlst, Itrlst, Itrlsth, Uzflwt, Uzstor,
     +                   Delstor, Nwavst, Nstrail, Nstotrl, Iuzt, Isuzn,
     +                   Nuzst, Uzolsflx, Uzwdth, fks, Ndpelev, Dpthinc,
     +                   Wetper, Nsfrsets, Uzseep, Foldflbt, ratin,
     +                   ratout, prcntdif, Loop, Ncol, Nrow, Nlay,
     +                   Nuzcol, Nuzrow, il, ir, ic, flobot, sbot,
     +                   strlen, l, Delt, Totim, totflwt, totuzstor,
     +                   totdelstor, Kper, Kstp, hstr, iwidthcheck,
     +                   Avwat, Wat1, Numave, Avdpt, Sfruzbd, ibd,
     +                   icalc, Iunitgage, gwflow)
           FOLDFLBT(l) = flobot
        ELSE
          gwflow = flobot
          Buff(ic, ir, il) = Buff(ic, ir, il) + flobot
          IF ( Iuzt.GT.0 ) totdelstor = 0.0D0
C
C36-----SUBTRACT FLOBOT FROM RATOUT WHEN GROUND WATER DISCHARGES
C         TO STREAM REACH.
          IF ( flobot.LT.NEARZERO ) ratout = ratout - flobot
C
C37-----ADD FLOBOT TO RATIN WHEN STREAM RECHARGES GROUND WATER.
          IF ( flobot.GT.NEARZERO ) ratin = ratin + flobot
          IF ( icalccheck.EQ.1 .AND. sbot.GT.h ) THEN
            totflwt = flobot*Delt
            Sfruzbd(1) = Sfruzbd(1) + flobot*Delt
            Sfruzbd(2) = 0.0
            Sfruzbd(3) = Sfruzbd(3) + flobot*Delt
            Sfruzbd(4) = Sfruzbd(4) + flobot
            Sfruzbd(5) = 0.0
            Sfruzbd(6) = Sfruzbd(6) + flobot
          ELSE IF ( sbot.LT.h ) THEN
            totflwt = 0.0
          END IF
        END IF
C
C38-----PRINT STREAMFLOWS AND RATES FOR EACH REACH TO MAIN LIST WHEN
C         REQUESTED (ISTCB1<0)AND NO UNSATURATED FLOW.
        IF ( Iuzt.EQ.0 ) THEN
          IF ( ibd.LT.0 .AND. Iptflg.LE.0 ) THEN
            IF ( ibdlbl.EQ.0 ) WRITE (Iout, 9004) txtlst, Kper, Kstp
 9004       FORMAT (1X, ///1X, A, '   PERIOD ', I6, '   STEP ', I8, //,
     +              ' LAYER ROW COL. STREAM  RCH.  FLOW INTO     ',
     +              'FLOW TO   FLOW OUT OF   OVRLND.     DIRECT     ',
     +              'STREAM       STREAM     STREAM     STREAM   ',
     +              'STREAMBED  STREAMBED', /16X,
     +              'SEG.NO.  NO.  STRM. RCH.    AQUIFER    STRM. RCH.',
     +              '    RUNOFF     PRECIP       ET          HEAD    ',
     +              '   DEPTH      WIDTH   CONDCTNC.   GRADIENT'/)
            WRITE (Iout, 9005) il, ir, ic, Istrm(4, l), Istrm(5, l),
     +                         Strm(10, l), Strm(11, l), Strm(9, l),
     +                         SNGL(runof), SNGL(precip),
     +                         SNGL(etstr), Strm(15, l), SNGL(depth),
     +                         Strm(5, l), Strm(16, l), SNGL(grad)
 9005       FORMAT (1X, I3, I5, I5, 2I6, 3X, 1PE11.4, 1X, E11.4, 2X,
     +              E11.4, 3(1X, E10.3), 1X, E12.5, 4(1X, E10.3))
            ibdlbl = 1
          END IF
C
C39-----PRINT STREAMFLOWS AND RATES FOR EACH REACH TO STREAM LIST
C         WHEN REQUESTED (IF ISTCB2>0).
          IF ( ibdst.LT.0 .AND. Iptflg.LE.0 ) THEN
            IF ( ibstlb.EQ.0 ) WRITE (Iout2, 9004) txtlst, Kper, Kstp
            WRITE (Iout2, 9005) il, ir, ic, Istrm(4, l), Istrm(5, l),
     +                          Strm(10, l), Strm(11, l), Strm(9, l),
     +                          SNGL(runof), SNGL(precip),
     +                          SNGL(etstr), Strm(15, l), SNGL(depth),
     +                          Strm(5, l), Strm(16, l), SNGL(grad)
            ibstlb = 1
          END IF
C
C40-----PRINT STREAMLFOWS AND RATES FOR EACH REACH TO MAIN LIST
C         WHEN UNSATURATED FLOW IS ACTIVE.
Cdep      revised print statement
        ELSE
          IF ( ibd.LT.0 .AND. Iptflg.LE.0 ) THEN
            IF ( ibdlbl.EQ.0 ) WRITE (Iout, 9006) txtlst, Kper, Kstp
 9006       FORMAT (1X, ///1X, A, '   PERIOD ', I6, '   STEP ', I8, //,
     +              ' LAYER ROW COL. STREAM  RCH.   FLOW INTO   ',
     +              ' STREAM     FLOW OUT OF   OVRLND.    DIRECT     ',
     +              'STREAM      STREAM      STREAM     STREAM  ',
     +              'STREAMBED   FLOW TO   CHNG. UNSAT. ', /16X,
     +              'SEG.NO.  NO.   STRM. RCH.    LOSS       ',
     +              'STRM. RCH.   RUNOFF     PRECIP       ET         ',
     +              'HEAD        DEPTH      WIDTH  CONDCTNC.   ',
     +              'WAT. TAB.   STORAGE', /)
            WRITE (Iout, 9007) il, ir, ic, Istrm(4, l), Istrm(5, l),
     +                         Strm(10, l), Strm(11, l), Strm(9, l),
     +                         SNGL(runof), SNGL(precip),
     +                         SNGL(etstr), Strm(15, l), SNGL(depth),
     +                         Strm(5, l), Strm(16, l), totflwt/Delt,
     +                         totdelstor/Delt
 9007       FORMAT (1X, I3, I5, I5, 2I6, 3X, 1PE11.4, 1X, E11.4, 2X,
     +              E11.4, 3(1X, E10.3), 1X, E12.5, 5(1X, E10.3))
            ibdlbl = 1
          END IF
C
C41-----PRINT STREAMFLOWS AND RATES FOR EACH REACH TO STREAM LIST
C         WHEN UNSATURATED FLOW IS ACTIVE.
          IF ( ibdst.LT.0 .AND. Iptflg.LE.0 ) THEN
            IF ( ibstlb.EQ.0 ) WRITE (Iout2, 9006) txtlst, Kper, Kstp
            WRITE (Iout2, 9007) il, ir, ic, Istrm(4, l), Istrm(5, l),
     +                          Strm(10, l), Strm(11, l), Strm(9, l),
     +                          SNGL(runof), SNGL(precip),
     +                          SNGL(etstr), Strm(15, l), SNGL(depth),
     +                          Strm(5, l), Strm(16, l), totflwt/Delt,
     +                          totdelstor/Delt
            ibstlb = 1
          END IF
        END IF
C
C42A----SAVE FLOW TO AND FROM GROUND WATER IN LIST FILE WHEN
C         IBD IS EQUAL TO 2. Revised dep 5/10/2006--Fixed 9/15/2006
        IF ( ibd.EQ.2 ) CALL UBDSVA(Iout1, Ncol, Nrow, ic, ir, il,
     +                              SNGL(gwflow), Ibound, Nlay)
      END DO
C
C43-----SAVE FLOW TO AND FROM GROUND WATER AS A 3-D ARRAY WHEN
C         IBD IS EQUAL TO 2. Revised dep 5/10/2006
      IF ( ibd.EQ.1 ) CALL UBUDSV(Kstp, Kper, text, Iout1, Buff, Ncol,
     +                            Nrow, Nlay, Iout)
C
C44-----RECORD STREAM GAGING STATION DATA (IF SOLUTE TRANSPORT NOT ACTIVE).
      IF ( I15.LE.0 ) THEN
        ndum = 1
        IF ( Iunitgage.GT.0 ) CALL SGWF1GAG5SO(Igglst, Numgage, I15,
     +       Strm, Iseg, Nsegdim, Nstrm, Totim, ndum, Buff, Sfrq, Nss,
     +       Seg, Sgotflw, Idivar, Avwat, Wat1, Avdpt, Nuzst, Numave,
     +       ibd)
      END IF
C
C45-----PRINT MASS BALANCE OF UNSATURATED ZONE IF IUZT GREATER THAN 0.
      IF ( Iuzt.GT.0 .AND. ( ibd.NE.0 .OR. iblst.LT.0 ) )
     +     CALL GWF1SFR2UZOT(Delt, Kstp, Kper, Sfruzbd, Iout)
C
C46-----MOVE RATES, VOLUMES AND LABELS INTO ARRAYS FOR PRINTING.
      rin = ratin
      rout = ratout
      Vbvl(3, Msum) = rin
      Vbvl(4, Msum) = rout
      Vbvl(1, Msum) = Vbvl(1, Msum) + rin*Delt
      Vbvl(2, Msum) = Vbvl(2, Msum) + rout*Delt
      Vbnm(Msum) = text
C
C47-----INCREMENT BUDGET TERM COUNTER.
      Msum = Msum + 1
      IF ( ibd.NE.0 .AND. Iptflg.LE.0 ) WRITE (Iout, 9008)
 9008 FORMAT (//)
C
C48-----STREAMFLOW OUT OF EACH REACH IS SAVED TO A LIST FILE
C        WHEN COMPACT BUDGET REQUESTED OR TO A 3-D ARRAY 
C        WHEN STANDARD UNFORMATTED BUDGET. Revised dep 5/10/2006
      IF ( ibdst.GT.0 ) THEN
        DO il = 1, Nlay
          DO ir = 1, Nrow
            DO ic = 1, Ncol
              Buff(ic, ir, il) = zero
            END DO
          END DO
        END DO
        DO l = 1, Nstrm
          il = Istrm(1, l)
          ir = Istrm(2, l)
          ic = Istrm(3, l)
          ilay = il
          Buff(ic, ir, il) = Buff(ic, ir, il) + Strm(9, l)
Cdep   added compact budget option for streamflow out of reach
          IF ( ibdst.EQ.2 )  CALL UBDSVA(Iout2, Ncol, Nrow, ic, ir, 
     +                                   il, Strm(9, l), Ibound, Nlay)
        END DO
        IF ( ibdst.EQ.1 )  CALL UBUDSV(Kstp, Kper, strtxt, Iout2, Buff,
     +                             Ncol, Nrow, Nlay, Iout)
 
      END IF
C
C49-----RETURN.
      RETURN
      END SUBROUTINE GWF1SFR2BD
Cdep  new subroutine for computing relation of lake stage to lake outflow
C-------SUBROUTINE GWF2SFR2LAKOUTFLW
      SUBROUTINE GWF1SFR2LAKOUTFLW(Nlakes, Nstrm, Nss, Nsegdim, 
     +                             Const, Istrm, Iseg, Idivar, Strm, 
     +                             Seg, Xsec, Qstage, Maxpts, Nsslk,
     +                             Slkotflw, Dlkotflw, Dlkstage, Iout)
C     *****************************************************************
C     CALCULATE ARRAYS OF LAKE STAGE, FLOW, AND THE DERIVATIVE OF
C     FLOWS FOR STREAM SEGMENTS THAT HAVE INFLOWS DETERMINED BY
C     LAKE STAGE
C     VERSION  2.7: MARCH 16, 2009
C     *****************************************************************
      IMPLICIT NONE
      INTRINSIC FLOAT, ABS, IABS, DSQRT, DLOG10, SQRT, SNGL
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER Maxpts, Nlakes, Nsegdim, Nstrm, Nss, Nsslk,Istrm, Iseg, 
     +        Idivar, Iout
      REAL Const, Strm, Seg, Qstage, Xsec 
      DOUBLE PRECISION Slkotflw, Dlkotflw, Dlkstage
      DIMENSION Istrm(5, Nstrm), Iseg(4, Nsegdim), Idivar(2, Nsegdim)
      DIMENSION Strm(24, Nstrm), Seg(26, Nsegdim), 
     +          Qstage(Maxpts, Nsegdim), Xsec(16, Nsegdim)
      DIMENSION Slkotflw(200, Nsslk), Dlkotflw(200, Nsslk), 
     +          Dlkstage(200, Nsslk)
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      INTEGER icalc, istsg, l, lk, lake, nreach, nstrpts
      REAL roughch, roughbnk, widthch 
      DOUBLE PRECISION finc, strbdtop, dlkstr1, dlkstr2, slope, cdpth,
     +                 fdpth, flwdlk1, flwdlk2, wdthlk1, wdthlk2,
     +                 wetperm1, wetperm2, width1, width2
C     ------------------------------------------------------------------
C     LOCAL STATIC VARIABLES
C     ------------------------------------------------------------------
      DOUBLE PRECISION FIVE_THIRDS, DPMAXLK
      PARAMETER (FIVE_THIRDS=5.0D0/3.0D0)
      PARAMETER (DPMAXLK=10.0D0)
C
C1------LOOP THROUGH ALL STREAM REACHES.
C
      DO l = 1, NSTRM
        istsg = ISTRM(4, l)
        nreach = ISTRM(5, l)
        icalc = ISEG(1, istsg)
        IF ( icalc.EQ.1 .OR. icalc.EQ.2 ) THEN
          slope = Strm(2, l)
          roughch = Seg(16, istsg)
          IF ( icalc.EQ.1 ) widthch = Seg(9, istsg)
          IF ( icalc.EQ.2 ) roughbnk = Seg(17, istsg)
        END IF
        IF ( icalc.EQ.4 ) nstrpts = Iseg(2, istsg)
C
C2------DETERMINE SEGMENTS THAT GET THEIR INFLOWS FROM A LAKE.
        IF ( nreach.EQ.1 .AND. Idivar(1,istsg).LT.0 ) THEN
          lake = IABS(Idivar(1, istsg))     
          finc = DPMAXLK/200.0D0
          strbdtop = Seg(8,istsg)
C
Cdep    Added tables for computing lake outflow in Lake Package    
C3------CALCUATE TABLES FOR LAKE STAGE AND CHANGE IN LAKE OUTFLOW.
          DO lk = 1, 200
            IF ( lk.EQ.1 )THEN
              Dlkstage(1,istsg) = strbdtop
              Dlkotflw(1, istsg) = 0.0D0
              Slkotflw(1, istsg) = 0.0D0
            ELSE
              Dlkstage(lk,istsg) = Dlkstage(lk-1,istsg) + finc
              dlkstr1 = Dlkstage(lk,istsg)- strbdtop
              dlkstr2 = dlkstr1 + 1.0D-07               
C
C4------ICALC EQUALS 1.
              IF ( icalc.EQ.1 ) THEN
                flwdlk2 = (Const/roughch)*widthch
     +                    *(dlkstr2**FIVE_THIRDS)*(DSQRT(slope)) 
                Dlkotflw(lk, istsg) = FIVE_THIRDS*flwdlk2/dlkstr2
                Slkotflw(lk, istsg) = (Const/roughch)*widthch
     +                    *(dlkstr1**FIVE_THIRDS)*(DSQRT(slope))
C
C5------ICALC EQUALS 2.
              ELSE IF ( icalc.EQ.2 ) THEN
                CALL GWF1SFR2FLW(dlkstr1, Xsec, Const, istsg, Nsegdim, 
     +                         roughch, roughbnk, slope, wetperm1,
     +                         flwdlk1, width1)
                CALL GWF1SFR2FLW(dlkstr2, Xsec, Const, istsg, Nsegdim, 
     +                         roughch, roughbnk, slope, wetperm2,
     +                         flwdlk2, width2)
                Dlkotflw(lk, istsg) = (flwdlk1-flwdlk2)
     +                                /(dlkstr1-dlkstr2)
                Slkotflw(lk, istsg) = flwdlk1
C
C6-----ICALC EQUALS 3 USING FORMULA-- Q=(DEPTH/CDPTH)**1/FDPTH).
              ELSE IF ( icalc.EQ.3 ) THEN
                cdpth = SEG(9, istsg)
                fdpth = SEG(10, istsg)
                Dlkotflw(lk, istsg) = (1.0D0/(cdpth*fdpth)) * (dlkstr2/
     +                                cdpth)**(1.0D0/fdpth-1.0D0)
                Slkotflw(lk, istsg) = (dlkstr1/cdpth)**(1.0/fdpth)
C
C7-----FLOW FROM LAKE COMPUTED USING TABULATED VALUES.
              ELSE IF ( icalc.EQ.4 ) THEN
                CALL GWF1SFR2TBF(flwdlk1, Qstage, dlkstr1, wdthlk1,
     +                         nstrpts, Maxpts, Nsegdim, nreach,
     +                         istsg, 1, Iout, 0)
                CALL GWF1SFR2TBF(flwdlk2, Qstage, dlkstr2, wdthlk2,
     +                         nstrpts, Maxpts, Nsegdim, nreach,
     +                         istsg, 1, Iout, 0)
                Dlkotflw(lk, istsg) = (flwdlk1-flwdlk2)
     +                                 /(dlkstr1-dlkstr2)
                Slkotflw(lk, istsg) = flwdlk1
              END IF
            END IF
          END DO
        END IF
      END DO
      RETURN
      END SUBROUTINE GWF1SFR2LAKOUTFLW
C
C-------SUBROUTINE GWF2SFR2DIVERS
      SUBROUTINE GWF2SFR2DIVERS(Iprior, Idivseg, Upflw, Dvrsn)
C     ******************************************************************
C     COMPUTES DIVERSIONS FROM AN UPSTREAM SEGMENT
C     VERSION  2.7: MARCH 16, 2009   DEP AND RGN
C     ******************************************************************
      IMPLICIT NONE
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER Iprior, Idivseg
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
C4------IF IPRIOR IS -3 THEN FLOW DIVERTED ONLY WHEN STREAMLFOW
C         EXCEEDS SPECIFIED FLOW (FLOOD CONTROL DIVERSION).
      ELSE IF ( Iprior.EQ.-3 ) THEN
        IF ( Upflw.GT.Dvrsn ) THEN
          Dvrsn = Upflw - Dvrsn
        ELSE
          Dvrsn = 0.0D0
        END IF
      END IF
      RETURN
      END SUBROUTINE GWF2SFR2DIVERS
C
C-------SUBROUTINE GWF1SFR2UZOT
      SUBROUTINE GWF1SFR2UZOT(Delt, Kstp, Kper, Sfruzbd, Iout)
C     ******************************************************************
C     PRINTS MASS BALANCE FOR ENTIRE UNSATURATED ZONE
C     VERSION 2.7: MARCH 16, 2009
C     ******************************************************************
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL Delt, Sfruzbd
      INTEGER Iout, Kper, Kstp
      DIMENSION Sfruzbd(6)
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      REAL adiffr, adiffv, bigvl1, bigvl2, diffr, diffv, prcntdifr,
     +     prcntdifv, small, totrin, totrot, totvin, totvot, zero,
     +     CLOSEZERO
      CHARACTER*18 text1, text2, text3, val1, val2

C     ******************************************************************
      text1 = '       STREAM LOSS'
      text2 = ' CHANGE IN STORAGE'
      text3 = '    RECHARGE TO GW'
      bigvl1 = 9.99999E11
      bigvl2 = 9.99999E10
      small = 0.1
      zero = 0.0
      CLOSEZERO = 1.0E-20
      WRITE (Iout, 9001) Kstp, Kper
      WRITE (Iout, 9002)
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
      WRITE (Iout, 9003) text1, val1, text1, val2
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
      WRITE (Iout, 9003) text2, val1, text2, val2
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
      WRITE (Iout, 9003) text3, val1, text3, val2
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
      WRITE (Iout, 9004) val1, val2
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
      WRITE (Iout, 9006) val1, val2
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
      WRITE (Iout, 9007) val1, val2
      WRITE (Iout, 9008) prcntdifv, prcntdifr
C
 9001 FORMAT ('1', /2X,
     +        'VOLUMETRIC BUDGET FOR UNSATURATED ZONE BENEATH ',
     +        'STREAMS AT END OF TIME STEP', I4, ' STRESS PERIOD',
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
      END SUBROUTINE GWF1SFR2UZOT
C
C-------SUBROUTINE GWF1SFR2DPTH
      SUBROUTINE GWF1SFR2DPTH(Flow, Xsec, Slope, Const, Istsg, Nreach,
     +                        Nsegdim, Roughch, Roughbnk, Wetperm,
     +                        Depth, Itstr, Totwdth, Iout, Iprndpth)
C     ******************************************************************
C     COMPUTE STREAM DEPTH GIVEN FLOW USING 8-POINT CROSS SECTION
C     VERSION  2.7: MARCH 16, 2009
C     ******************************************************************
      IMPLICIT NONE

C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL Const, Roughbnk, Roughch, Xsec
      INTEGER Iout, Iprndpth, Istsg, Itstr, Nreach, Nsegdim
      DOUBLE PRECISION Flow, Slope, Wetperm, Depth, Totwdth
      DIMENSION Xsec(16, Nsegdim)
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      INTEGER i, iflg
      REAL CLOSEZERO
      DOUBLE PRECISION flow1, flow2, flow3, y0, ymin, xnum, dnom, stage,
     +                 depth1, depth2, depth3, f1, f2, f3, err1, err2,
     +                 err3

C     ******************************************************************
C
      CLOSEZERO = 1.0E-20
C1------INITIALIZE VARIABLES TO ZERO.
      Totwdth = 0.0D0
      Wetperm = 0.0D0
C
C2------FIND LOWEST POINT IN CHANNEL.
      ymin = Xsec(9, Istsg)
      i = 9
      DO WHILE ( i.LE.16 )
        y0 = Xsec(i, Istsg)
        ymin = DMIN1(ymin, y0)
        i = i + 1
      END DO
C
C3------ESTIMATE INITIAL DEPTH ASSUMING WIDE RECTANGULAR CHANNEL.
      depth1 = 0.0D0
      xnum = Flow*Roughch
      dnom = Const*(Xsec(6, Istsg)-Xsec(3, Istsg))*SQRT(Slope)
      IF ( dnom.GT.0.0 ) depth1 = (xnum/dnom)**0.6D0
      IF ( depth1.GT.0.0D0 ) THEN
        stage = depth1 + ymin
        flow1 = 0.0D0
        CALL GWF1SFR2FLW(stage, Xsec, Const, Istsg, Nsegdim, Roughch,
     +                   Roughbnk, Slope, Wetperm, flow1, Totwdth)
        f1 = Flow - flow1
        depth2 = 1.1D0*depth1
        stage = depth2 + ymin
        flow2 = 0.0D0
        CALL GWF1SFR2FLW(stage, Xsec, Const, Istsg, Nsegdim, Roughch,
     +                   Roughbnk, Slope, Wetperm, flow2, Totwdth)
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
            ELSE IF ( DABS(f2-f1).LT.CLOSEZERO ) THEN
              depth3 = (depth1+depth2)*0.5D0
            END IF
            err1 = DABS(depth3-depth1)
            err2 = DABS(depth3-depth2)
            err3 = DABS(f2-f1)
            IF ( (err1.LT.0.000001 .OR. err2.LT.0.000001) .AND.
     +           Itstr.GT.2 ) iflg = 0
            IF ( err3.LT.0.0001 .AND. Itstr.GT.2 ) iflg = 0
            stage = depth3 + ymin
            CALL GWF1SFR2FLW(stage, Xsec, Const, Istsg, Nsegdim,
     +                       Roughch, Roughbnk, Slope, Wetperm, flow3,
     +                       Totwdth)
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
C13-----DEPTH1, DEPTH2, AND DEPTH3 ARE ON SAME SIDE OF ROOT;
C         REPLACE WITH NEAREST VALUE.
              IF ( DABS(f1).GT.DABS(f2) ) THEN
                depth1 = depth2
                f1 = f2
              END IF
              depth2 = depth3
              f2 = f3
            END IF
            Itstr = Itstr + 1
            IF ( Iprndpth.EQ.1 ) THEN
              WRITE (Iout, 9001) Itstr, iflg, Flow, depth1, depth2,
     +                           depth3, f1, f2, f3
 9001         FORMAT (1X/, 'ITSTR,IFLG,FLOW,DEPTH1,DEPTH2,DEPTH3,F1,F2,'
     +                , ',F3 ', 2I5, 7(2X, D15.6))
            END IF
C
C14-----PRINT WARNING MESSAGE IF SECANT METHOD FAILED TO FIND A DEPTH.
            IF ( Itstr.GT.100 ) THEN
              iflg = 0
              WRITE (Iout, 9002) Istsg, Nreach, depth3, depth1, depth2
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
        CALL GWF1SFR2FLW(stage, Xsec, Const, Istsg, Nsegdim, Roughch,
     +                   Roughbnk, Slope, Wetperm, flow1, Totwdth)
      ELSE
        Depth = 0.0D0
        Wetperm = 0.0D0
        Totwdth = 0.0D0
        Itstr = 0
      END IF
C
C16----RETURN.
      RETURN
      END SUBROUTINE GWF1SFR2DPTH
C
C-------SUBROUTINE GWF1SFR2FLW
      SUBROUTINE GWF1SFR2FLW(Depth, Xsec, Const, Istsg, Nsegdim,
     +                       Roughch, Roughbnk, Slope, Wetperm, Flow,
     +                       Totwdth)
C     *******************************************************************
C     COMPUTE FLOW IN STREAM GIVEN DEPTH USING 8-POINT CROSS SECTION
C     VERSION  2.7: MARCH 16, 2009
C     *******************************************************************
      IMPLICIT NONE

C     -------------------------------------------------------------------
C     SPECIFICATIONS:
C     -------------------------------------------------------------------
C     ARGUMENTS
C     -------------------------------------------------------------------
      REAL Const, Roughbnk, Roughch, Xsec
      INTEGER Istsg, Nsegdim
      DOUBLE PRECISION Flow, Wetperm, Totwdth, Depth, Slope
      DIMENSION Xsec(16, Nsegdim)
C     -------------------------------------------------------------------
C     LOCAL VARIABLES
C     -------------------------------------------------------------------
      REAL fac, r, rough, subarea, wtprm
      INTEGER i, ii, j
      DOUBLE PRECISION xleft, yleft, dpthleft, dpthrght, xright, yright,
     +                 x0, y0, x1, y1, wtprm1, wtprm2, wtprm3, width1,
     +                 width2, width3, width, subarea1, subarea2,
     +                 subarea3, subflow1, subflow2, subflow3

C     *******************************************************************
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
      i = 2
      DO WHILE ( i.LE.8 )
        ii = i + 8
        x0 = Xsec(i-1, Istsg)
        y0 = Xsec(ii-1, Istsg)
        x1 = Xsec(i, Istsg)
        y1 = Xsec(ii, Istsg)
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
            subflow1 = (Const/rough)*subarea1*r*(Slope)**0.5D0
          ELSE IF ( j.EQ.2 .AND. wtprm2.GT.0.0 ) THEN
            r = (subarea2/wtprm2)**fac
            subflow2 = (Const/rough)*subarea2*r*(Slope)**0.5D0
          ELSE IF ( j.EQ.3 .AND. wtprm3.GT.0.0 ) THEN
            r = (subarea3/wtprm3)**fac
            subflow3 = (Const/rough)*subarea3*r*(Slope)**0.5D0
          END IF
          j = j + 1
        END IF
        i = i + 1
      END DO
C
C10-----SUM FLOW, WETTED PERIMETER AND WIDTH FOR SUBSECTIONS.
      Flow = subflow1 + subflow2 + subflow3
      Totwdth = width1 + width2 + width3
      Wetperm = wtprm1 + wtprm2 + wtprm3
C
C11-----RETURN.
      RETURN
      END SUBROUTINE GWF1SFR2FLW
C
C-------SUBROUTINE GWF1SFR2TBD
      SUBROUTINE GWF1SFR2TBD(Flow, Qstage, Depth, Width, Nstrpts,
     +                     Maxpts, Nsegdim, Nreach, Istsg, Kkiter, Iout)
C     *******************************************************************
C     COMPUTE DEPTH AND WIDTH IN STREAM GIVEN FLOW USING RATING TABLES.
C     VERSION  2.7: MARCH 16, 2009
C     *******************************************************************
C
      IMPLICIT NONE

C     -------------------------------------------------------------------
C     SPECIFICATIONS:
C     -------------------------------------------------------------------
C     ARGUMENTS
C     -------------------------------------------------------------------
      INTEGER Iout, Istsg, Kkiter, Maxpts, Nreach, Nsegdim, Nstrpts
      REAL Qstage
      DOUBLE PRECISION Flow, Depth, Width
      DIMENSION Qstage(Maxpts, Nsegdim)
C     -------------------------------------------------------------------
C     LOCAL VARIABLES
C     -------------------------------------------------------------------
      INTEGER iflwlw, istghi, istglw, istp, iwthhi, iwthlw
      DOUBLE PRECISION flwlw, flwhi, stglw, stghi, wthlw, wthhi, dflwlw,
     +                 dflwhi, dstglw, dstghi, dwthlw, dwthhi, dlgflw,
     +                 dlgsls, dlgslw, dlgstg, dlgwth

C     *******************************************************************
C
C1------DEFINE RANGE OF FLOW, DEPTH, AND WIDTH FROM RATING TABLE.
      flwlw = Qstage(1, Istsg)
      stglw = Qstage(1+Nstrpts, Istsg)
      wthlw = Qstage(1+(2*Nstrpts), Istsg)
      flwhi = Qstage(Nstrpts, Istsg)
      stghi = Qstage(2*Nstrpts, Istsg)
      wthhi = Qstage(3*Nstrpts, Istsg)
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
        flwhi = Qstage(istp, Istsg)
        DO WHILE ( Flow.GT.flwhi .AND. istp.LT.Nstrpts )
          istp = istp + 1
          flwhi = Qstage(istp, Istsg)
        END DO
        IF ( Flow.LE.flwhi ) THEN
          istghi = istp + Nstrpts
          istglw = istghi - 1
          iwthhi = istp + (2*Nstrpts)
          iwthlw = iwthhi - 1
          iflwlw = istp - 1
          stghi = Qstage(istghi, Istsg)
          stglw = Qstage(istglw, Istsg)
          wthhi = Qstage(iwthhi, Istsg)
          wthlw = Qstage(iwthlw, Istsg)
          flwlw = Qstage(iflwlw, Istsg)
        ELSE IF ( Flow.GT.flwhi ) THEN
C
C5------COMPUTED FLOW EXCEEDS HIGHEST FLOW IN TABLE.
          flwlw = Qstage(Nstrpts-1, Istsg)
          stglw = Qstage((2*Nstrpts)-1, Istsg)
          stghi = Qstage(2*Nstrpts, Istsg)
          wthlw = Qstage((3*Nstrpts)-1, Istsg)
          wthhi = Qstage((3*Nstrpts), Istsg)
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
C7------RETURN.
      RETURN
      END SUBROUTINE GWF1SFR2TBD
C
C-------SUBROUTINE GWF1SFR2TBF
      SUBROUTINE GWF1SFR2TBF(Flow, Qstage, Depth, Width, Nstrpts,
     +                       Maxpts, Nsegdim, Nreach, Istsg, Kkiter,
     +                       Iout, Itb)
C     *******************************************************************
C     COMPUTE FLOW AND WIDTH IN STREAM GIVEN DEPTH USING RATING TABLES.
C     VERSION  2.7: MARCH 16, 2009
C     *******************************************************************
C
      IMPLICIT NONE

C     -------------------------------------------------------------------
C     SPECIFICATIONS:
C     -------------------------------------------------------------------
C     ARGUMENTS
C     -------------------------------------------------------------------
      INTEGER Iout, Istsg, Itb, Kkiter, Maxpts, Nreach, Nsegdim, Nstrpts
      REAL Qstage
      DOUBLE PRECISION Flow, Depth, Width
      DIMENSION Qstage(Maxpts, Nsegdim)
C     -------------------------------------------------------------------
C     LOCAL VARIABLES
C     -------------------------------------------------------------------
      INTEGER iflwhi, iflwlw, istglw, istp, iwthhi, iwthlw
      DOUBLE PRECISION flwlw, flwhi, stglw, stghi, wthlw, wthhi, dflwlw,
     +                 dflwhi, dstglw, dstghi, dwthlw, dwthhi, dlgflw,
     +                 dlgslf, dlgslw, dlgstg, dlgwth

C     *******************************************************************
C
C1------DEFINE RANGE OF FLOW, DEPTH, AND WIDTH FROM RATING TABLE.
      flwlw = Qstage(1, Istsg)
      stglw = Qstage(1+Nstrpts, Istsg)
      wthlw = Qstage(1+(2*Nstrpts), Istsg)
      flwhi = Qstage(Nstrpts, Istsg)
      stghi = Qstage(2*Nstrpts, Istsg)
      wthhi = Qstage(3*Nstrpts, Istsg)
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
        stghi = Qstage(istp+Nstrpts, Istsg)
C
C4------FIND NEAREST VALUES OF FLOW, DEPTH, AND WIDTH IN TABLE.
        DO WHILE ( Depth.GT.stghi .AND. istp.LT.Nstrpts )
          istp = istp + 1
          stghi = Qstage(istp+Nstrpts, Istsg)
        END DO
        IF ( Depth.LE.stghi ) THEN
          istglw = (istp-1) + Nstrpts
          iflwhi = istp
          iflwlw = istp - 1
          iwthhi = istp + (2*Nstrpts)
          iwthlw = iwthhi - 1
          stglw = Qstage(istglw, Istsg)
          wthhi = Qstage(iwthhi, Istsg)
          wthlw = Qstage(iwthlw, Istsg)
          flwlw = Qstage(iflwlw, Istsg)
          flwhi = Qstage(iflwhi, Istsg)
        ELSE IF ( Depth.GT.stghi .AND. Itb.EQ.1 ) THEN
C
C5------PRINT WARNING IF COMPUTED DEPTH EXCEEDS HIGHEST DEPTH IN TABLE.
          WRITE (Iout, 9001) Kkiter, Istsg, Nreach, Depth, stghi
 9001     FORMAT (1X/, 'FOR MODFLOW ITERATION ', I5,
     +            ' DEPTH IN SEGMENT ', I5, ' REACH ', I5, ' IS ',
     +            1PE10.3, ' AND IS GREATER THAN ',
     +            'HIGHEST DEPTH LISTED IN RATING TABLE OF ', 1PE10.3,
     +            //1X, 'ASSUMING SAME RELATION AS ',
     +            'BETWEEN TWO HIGHEST DEPTHS IN TABLE'//)
          flwlw = Qstage(Nstrpts-1, Istsg)
          stglw = Qstage((2*Nstrpts)-1, Istsg)
          stghi = Qstage(2*Nstrpts, Istsg)
          wthlw = Qstage((3*Nstrpts)-1, Istsg)
          wthhi = Qstage((3*Nstrpts), Istsg)
          flwlw = Qstage(Nstrpts-1, Istsg)
          flwhi = Qstage(Nstrpts, Istsg)
          stglw = Qstage((2*Nstrpts)-1, Istsg)
          stghi = Qstage((2*Nstrpts), Istsg)
          wthlw = Qstage((3*Nstrpts)-1, Istsg)
          wthhi = Qstage((3*Nstrpts), Istsg)
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
      END SUBROUTINE GWF1SFR2TBF
C
C-------SUBROUTINE SGWF2SFR2RDSEG
      SUBROUTINE SGWF1SFR2RDSEG(Nlst, Lstbeg, In, Iout, Seg, Iseg,
     +                          Idivar, Iotsg, Maxpts, Xsec, Qstage,
     +                          I15, Concq, Concrun, Concppt, Nsol,
     +                          Nsegdim, Ischk, Nischk, Ichk, Nss,
     +                          Isfropt, Kper)
C     ******************************************************************
C     READ STREAM SEGMENT DATA -- parameters or non parameters
C     VERSION  2.7: MARCH 16, 2009
C     ******************************************************************
      IMPLICIT NONE

C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL Concppt, Concq, Concrun, Qstage, Seg, Xsec
      INTEGER I15, Ichk, Idivar, In, Iotsg, Iout, Ischk, Iseg, Isfropt,
     +        Lstbeg, Maxpts, Nischk, Nlst, Nsegdim, Nsol, Nss, Kper
      DIMENSION Seg(26, Nsegdim), Iseg(4, Nsegdim), Iotsg(Nsegdim),
     +          Idivar(2, Nsegdim), Xsec(16, Nsegdim),
     +          Qstage(Maxpts, Nsegdim), Ischk(Nischk)
      DIMENSION Concq(Nsegdim, Nsol), Concrun(Nsegdim, Nsol),
     +          Concppt(Nsegdim, Nsol)
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      INTEGER icalc, idum, ii, iqseg, isol, iupseg, jj, jk, lstend, n,
     +        noutseg, nseg, nstrpts

C     ******************************************************************
C
C1------READ STREAM SEGMENT DATA.
      iqseg = Lstbeg
      lstend = Lstbeg + Nlst - 1
      DO WHILE ( iqseg.LE.lstend )
C
C2------ONLY READ FIRST 4 VARIABLES TO DETERMINE VALUE OF IUPSEG.
        READ (In, *) n, icalc, noutseg, iupseg
        IF ( n.GT.Nss .OR. n.LT.1 ) THEN
          WRITE (Iout, 9001) n
 9001     FORMAT (1X, /1X, 'SEGMENT NUMBER (NSEG) OUT OF RANGE: ', I6)
          IF ( Ichk.NE.0 ) THEN
            WRITE (Iout, 9002) iqseg - Lstbeg + 1
 9002       FORMAT (1X, 'READING ENTRY ', I6, ' OF ITEM 6A')
          ELSE
            WRITE (Iout, 9003) iqseg - Lstbeg + 1
 9003       FORMAT (1X, 'READING ENTRY ', I6, ' OF ITEM 4B')
          END IF
          CALL USTOP(' ')
Cdep added error check if unsaturated flow active, icalc is 1 or 2,
Cdep and changes type between stress periods  7/22/2007
        ELSE IF ( Isfropt.GT.1 .AND. Kper.GT.1 ) THEN
          IF ( Iseg(1,n).EQ.1 .OR. Iseg(1,n).EQ.2 ) THEN
            IF ( Iseg(1,n).NE.icalc ) THEN
              WRITE (Iout, 9010) n, Kper, Iseg(1,n), icalc
 9010         FORMAT (/1X, 'ICALC MUST BE CONSTANT WHEN UNSATURATED',
     +                ' FLOW IS ACTIVE BENEATH SEGMENT NUMBER (NSEG): ',
     +                I6, /1X, 'ICALC CHANGED IN STRESS PERIOD: ', I6,
     +                ' FROM ', I6,' TO ', I6)
             WRITE (Iout, 9002) iqseg - Lstbeg + 1
              CALL USTOP(' ')
            END IF
          END IF
        END IF   
C
C3------DETERMINE WHERE DATA ARE STORED.
        IF ( Ichk.NE.0 ) THEN
C3a-----STORE DATA IN ACTIVE SEGMENT AREA.
          nseg = n
          Ischk(n) = Ischk(n) + 1
        ELSE
C3b-----STORE DATA IN PARAMETER AREA.
          nseg = iqseg
          Iseg(3, iqseg) = n
          Seg(1, nseg) = Seg(1, n)
        END IF
        BACKSPACE In
C
C4------READ DATA SET 4B OR 6A FOR SEGMENTS THAT ARE NOT DIVERSIONS.
        IF ( iupseg.LE.0 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) idum, Iseg(1, nseg), Iotsg(nseg),
     +                   Idivar(1, nseg), (Seg(jj, nseg), jj=2, 5)
          ELSE IF ( icalc.EQ.1 ) THEN
            READ (In, *) idum, Iseg(1, nseg), Iotsg(nseg),
     +                   Idivar(1, nseg), (Seg(jj, nseg), jj=2, 5),
     +                   Seg(16, nseg)
          ELSE IF ( icalc.EQ.2 ) THEN
            READ (In, *) idum, Iseg(1, nseg), Iotsg(nseg),
     +                   Idivar(1, nseg), (Seg(jj, nseg), jj=2, 5),
     +                   (Seg(jk, nseg), jk=16, 17)
          ELSE IF ( icalc.EQ.3 ) THEN
            READ (In, *) idum, Iseg(1, nseg), Iotsg(nseg),
     +                   Idivar(1, nseg), (Seg(jj, nseg), jj=2, 5),
     +                   Seg(9, nseg), Seg(10, nseg), Seg(14, nseg),
     +                   Seg(15, nseg)
          ELSE IF ( icalc.EQ.4 ) THEN
            READ (In, *) idum, Iseg(1, nseg), Iotsg(nseg),
     +                   Idivar(1, nseg), Iseg(2, nseg),
     +                   (Seg(jj, nseg), jj=2, 5)
          END IF
C
C5------READ DATA 4B OR 6A FOR SEGMENTS THAT ARE DIVERSIONS FROM STREAMS.
        ELSE IF ( icalc.LE.0 ) THEN
          READ (In, *) idum, Iseg(1, nseg), Iotsg(nseg),
     +                 (Idivar(ii, nseg), ii=1, 2),
     +                 (Seg(jj, nseg), jj=2, 5)
        ELSE IF ( icalc.EQ.1 ) THEN
          READ (In, *) idum, Iseg(1, nseg), Iotsg(nseg),
     +                 (Idivar(ii, nseg), ii=1, 2),
     +                 (Seg(jj, nseg), jj=2, 5), Seg(16, nseg)
        ELSE IF ( icalc.EQ.2 ) THEN
          READ (In, *) idum, Iseg(1, nseg), Iotsg(nseg),
     +                 (Idivar(ii, nseg), ii=1, 2),
     +                 (Seg(jj, nseg), jj=2, 5),
     +                 (Seg(jk, nseg), jk=16, 17)
        ELSE IF ( icalc.EQ.3 ) THEN
          READ (In, *) idum, Iseg(1, nseg), Iotsg(nseg),
     +                 (Idivar(ii, nseg), ii=1, 2),
     +                 (Seg(jj, nseg), jj=2, 5), Seg(9, nseg),
     +                 Seg(10, nseg), Seg(14, nseg), Seg(15, nseg)
        ELSE IF ( icalc.EQ.4 ) THEN
          READ (In, *) idum, Iseg(1, nseg), Iotsg(nseg),
     +                 (Idivar(ii, nseg), ii=1, 2), Iseg(2, nseg),
     +                 (Seg(jj, nseg), jj=2, 5)
        END IF
C
C6------READ DATA SET 4C OR 6B.
        IF ( Isfropt.EQ.0 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) (Seg(jj, nseg), jj=6, 10)
          ELSE IF ( icalc.EQ.1 ) THEN
            READ (In, *) (Seg(jj, nseg), jj=6, 9)
          ELSE IF ( icalc.GE.2 .AND. icalc.LE.4 ) THEN
            READ (In, *) (Seg(jj, nseg), jj=6, 8)
          END IF
        ELSE IF ( Isfropt.EQ.1 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) Seg(9, nseg), Seg(10, nseg)
          ELSE IF ( icalc.EQ.1 ) THEN
            READ (In, *) Seg(9, nseg)
          END IF
        ELSE IF ( Isfropt.EQ.2 .OR. Isfropt.EQ.3 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) Seg(9, nseg), Seg(10, nseg)
          ELSE IF ( icalc.EQ.1 .AND. Kper.EQ.1 ) THEN
            READ (In, *) Seg(9, nseg)
          END IF
        ELSE IF ( Isfropt.EQ.4 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) (Seg(jj, nseg), jj=6, 10)
          ELSE IF ( icalc.EQ.1 ) THEN
            IF ( Kper.EQ.1 ) THEN
              READ (In, *) (Seg(jj, nseg), jj=6, 9),
     +                     (Seg(jj, nseg), jj=18, 20)
            ELSE
              READ (In, *) Seg(6, nseg)
            END IF
          ELSE IF ( icalc.EQ.2 ) THEN
            IF ( Kper.EQ.1 ) THEN
              READ (In, *) (Seg(jj, nseg), jj=6, 8),
     +                     (Seg(jj, nseg), jj=18, 20)
            ELSE
              READ (In, *) Seg(6, nseg)
            END IF
          ELSE IF ( icalc.GE.3 .AND. icalc.LE.4 ) THEN
            READ (In, *) (Seg(jj, nseg), jj=6, 8)
          END IF
        ELSE IF ( Isfropt.EQ.5 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) (Seg(jj, nseg), jj=6, 10)
          ELSE IF ( icalc.EQ.1 ) THEN
            IF ( Kper.EQ.1 ) THEN
              READ (In, *) (Seg(jj, nseg), jj=6, 9),
     +                     (Seg(jj, nseg), jj=18, 21)
            ELSE
              READ (In, *) Seg(6, nseg)
            END IF
          ELSE IF ( icalc.EQ.2 ) THEN
            IF ( Kper.EQ.1 ) THEN
              READ (In, *) (Seg(jj, nseg), jj=6, 8),
     +                     (Seg(jj, nseg), jj=18, 21)
            ELSE
              READ (In, *) Seg(6, nseg)
            END IF
          ELSE IF ( icalc.GE.3 .AND. icalc.LE.4 ) THEN
            READ (In, *) (Seg(jj, nseg), jj=6, 8)
          END IF
        END IF
C
C7------READ DATA SET 4D OR 6C.
        IF ( Isfropt.EQ.0 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) (Seg(jj, nseg), jj=11, 15)
          ELSE IF ( icalc.EQ.1 ) THEN
            READ (In, *) (Seg(jj, nseg), jj=11, 14)
          ELSE IF ( icalc.GE.2 .AND. icalc.LE.4 ) THEN
            READ (In, *) (Seg(jj, nseg), jj=11, 13)
          END IF
        ELSE IF ( Isfropt.EQ.1 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) Seg(14, nseg), Seg(15, nseg)
          ELSE IF ( icalc.EQ.1 ) THEN
            READ (In, *) Seg(14, nseg)
          END IF
        ELSE IF ( Isfropt.EQ.2 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) Seg(14, nseg), Seg(15, nseg)
          ELSE IF ( icalc.EQ.1 .AND. Kper.EQ.1 ) THEN
            READ (In, *) Seg(14, nseg)
          END IF
        ELSE IF ( Isfropt.EQ.3 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) Seg(14, nseg), Seg(15, nseg)
          ELSE IF ( icalc.EQ.1 .AND. Kper.EQ.1 ) THEN
            READ (In, *) Seg(14, nseg)
          END IF
        ELSE IF ( Isfropt.EQ.4 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) (Seg(jj, nseg), jj=11, 15)
          ELSE IF ( icalc.EQ.1 ) THEN
            IF ( Kper.EQ.1 ) THEN
              READ (In, *) (Seg(jj, nseg), jj=11, 14),
     +                     (Seg(jj, nseg), jj=22, 24)
            ELSE
              READ (In, *) Seg(11, nseg)
            END IF
          ELSE IF ( icalc.EQ.2 ) THEN
            IF ( Kper.EQ.1 ) THEN
              READ (In, *) (Seg(jj, nseg), jj=11, 13),
     +                     (Seg(jj, nseg), jj=22, 24)
            ELSE
              READ (In, *) Seg(11, nseg)
            END IF
          ELSE IF ( icalc.GE.3 .AND. icalc.LE.4 ) THEN
            READ (In, *) (Seg(jj, nseg), jj=11, 13)
          END IF
        ELSE IF ( Isfropt.EQ.5 ) THEN
          IF ( icalc.LE.0 ) THEN
            READ (In, *) (Seg(jj, nseg), jj=11, 15)
          ELSE IF ( icalc.EQ.1 ) THEN
            IF ( Kper.EQ.1 ) THEN
              READ (In, *) (Seg(jj, nseg), jj=11, 14),
     +                     (Seg(jj, nseg), jj=22, 25)
            ELSE
              READ (In, *) Seg(11, nseg)
            END IF
          ELSE IF ( icalc.EQ.2 ) THEN
            IF ( Kper.EQ.1 ) THEN
              READ (In, *) (Seg(jj, nseg), jj=11, 13),
     +                     (Seg(jj, nseg), jj=22, 25)
            ELSE
              READ (In, *) Seg(11, nseg)
            END IF
          ELSE IF ( icalc.GE.3 .AND. icalc.LE.4 ) THEN
            READ (In, *) (Seg(jj, nseg), jj=11, 13)
          END IF
        END IF
C
C8------READ DATA SET 4E OR 6D FOR SEGMENT WHEN ICALC IS 2.
        IF ( icalc.EQ.2 ) THEN
C       READ ONLY ONCE FOR UNSATURATED FLOW 4/19/2006.
          IF ( Kper.EQ.1 .OR. ISFROPT.LE.1 ) THEN
            READ (In, *) (Xsec(jj, nseg), jj=1, 8)
            READ (In, *) (Xsec(jj, nseg), jj=9, 16)
          END IF
        END IF
C
C9------READ DATA SET 4F OR 6E FOR SEGMENT WHEN ICALC IS 4.
        IF ( icalc.EQ.4 ) THEN
          nstrpts = Iseg(2, nseg)
          IF ( nstrpts.LT.2 ) THEN
            WRITE (Iout, 9004) n
 9004       FORMAT (/1X, 'NUMBER OF POINTS USED TO RELATE ',
     +              'STREAMFLOW WITH STREAM DEPTH AND WIDTH FOR ',
     +              'SEGMENT ', I6, ' IS LESS THAN TWO'//1X,
     +              'PROGRAM STOPPING')
            CALL USTOP(' ')
          ELSE IF ( nstrpts.GT.Maxpts/3 ) THEN
            WRITE (Iout, 9005) n, nstrpts
 9005       FORMAT (/1X, 'FOR SEGMENT ', I6, ' NUMBER OF POINTS',
     +              'USED TO RELATE STREAMFLOW WITH DEPTH AND ',
     +              'WIDTH IS ', I5//1X, 'WHICH IS MORE THAN ',
     +              'MAXIMUM NUMBER OF 50 POINTS', //1X,
     +              'PROGRAM STOPPING'//)
            CALL USTOP(' ')
          ELSE
            READ (In, *) (Qstage(jj, nseg), jj=1, nstrpts)
            READ (In, *) (Qstage(jj, nseg), jj=nstrpts+1, 2*nstrpts)
            READ (In, *) (Qstage(jj, nseg), jj=2*nstrpts+1, 3*nstrpts)
          END IF
        END IF
C
C10-----READ DATA SET 4G OR 6F FOR SEGMENT IF SOLUTES SPECIFIED.
        IF ( I15.GT.0 ) THEN
          isol = 1
          DO WHILE ( isol.LE.Nsol )
            IF ( Idivar(1, nseg).EQ.0 ) THEN
              READ (In, *) Concq(nseg, isol), Concrun(nseg, isol),
     +                     Concppt(nseg, isol)
            ELSE
              READ (In, *) Concrun(nseg, isol), Concppt(nseg, isol)
            END IF
            isol = isol + 1
          END DO
        END IF
        iqseg = iqseg + 1
      END DO
C11-----RETURN.
      RETURN
      END SUBROUTINE SGWF1SFR2RDSEG
C
C-------SUBROUTINE SGWF2SFR2PARMOV
      SUBROUTINE SGWF1SFR2PARMOV(In, Iout, Seg, Iseg, Idivar, Iotsg,
     +                           Maxpts, Xsec, Qstage, I15, Concq,
     +                           Concrun, Concppt, Nsol, Nsegdim,
     +                           Nsegck, Nss)
C
      IMPLICIT NONE
      INCLUDE 'param.inc'
C     ******************************************************************
C     MOVE STREAM PARAMETER DATA INTO ACTIVE SEGMENTS
C     VERSION  2.7: MARCH 16, 2009
C     ******************************************************************
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL Concppt, Concq, Concrun, Qstage, Seg, Xsec
      INTEGER I15, Idivar, In, Iotsg, Iout, Iseg, Maxpts, Nsegck,
     +        Nsegdim, Nsol, Nss
      DIMENSION Seg(26, Nsegdim), Iseg(4, Nsegdim), Iotsg(Nsegdim),
     +          Idivar(2, Nsegdim), Xsec(16, Nsegdim),
     +          Qstage(Maxpts, Nsegdim), Nsegck(Nss)
      DIMENSION Concq(Nsegdim, Nsol), Concrun(Nsegdim, Nsol),
     +          Concppt(Nsegdim, Nsol)
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      REAL rdum
      INTEGER icalc, idum, iloc, ip, iqseg, isol, istart, istop, iupseg,
     +        jend, jj, ki, lloc, lstend, ni, nlst, nseg, nstrpts,
     +        numinst
      CHARACTER*4 package
      CHARACTER*200 line
      CHARACTER*10 pname, ctmp3, ctmp4
C     ------------------------------------------------------------------
      package = 'SFR '
C
C1------READ PARAMETER NAME AND FIND IT IN THE PARAMETER LIST.
      READ (In, '(A)') line
      lloc = 1
      CALL URWORD(line, lloc, istart, istop, 0, idum, rdum, Iout, In)
      pname = line(istart:istop)
      WRITE (Iout, 9001) pname
 9001 FORMAT (/, ' Parameter:  ', A)
      CALL UPARFIND(pname, 'SFR', 'SFR', ip, Iout)
C
C2------DESIGNATE CELLS CORRESPONDING TO CORRECT PARAMETER INSTANCE.
      nlst = IPLOC(2, ip) - IPLOC(1, ip) + 1
      numinst = IPLOC(3, ip)
      iloc = IPLOC(4, ip)
      ni = 1
      IF ( numinst.GT.0 ) THEN
        nlst = nlst/numinst
        CALL URWORD(line, lloc, istart, istop, 0, idum, rdum, Iout, In)
        ctmp3 = line(istart:istop)
        IF ( ctmp3.EQ.' ' ) THEN
          WRITE (Iout, 9002) package, PARNAM(ip)
 9002     FORMAT (/, 1X, 'Blank instance name in the ', A,
     +            ' file for parameter ', A)
          CALL USTOP(' ')
        END IF
        WRITE (Iout, 9003) ctmp3
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
        WRITE (Iout, 9004) package, ctmp3, PARNAM(ip)
 9004   FORMAT (/, 1X, 'The ', A,
     +          ' file specifies undefined instance "', A,
     +          '" for parameter ', A)
        CALL USTOP(' ')
      END IF
 100  IF ( IACTIVE(ip).GT.0 ) THEN
        WRITE (Iout, 9005) PARNAM(ip)
 9005   FORMAT (/, 1X, '*** ERROR: PARAMETER "', A,
     +          '" HAS ALREADY BEEN ACTIVATED THIS STRESS PERIOD', /,
     +          ' -- STOP EXECUTION (SGWF1SFR2PARMOV)')
        CALL USTOP(' ')
      END IF
C
      IACTIVE(ip) = ni
C
C3------MOVE EACH ENTRY FOR THE PARAMETER
      iqseg = IPLOC(1, ip) + (ni-1)*nlst
      lstend = iqseg + nlst - 1
      DO WHILE ( iqseg.LE.lstend )
C
C4------DETERMINE VALUES OF ICALC, NSEG, AND IUPSEG.
        icalc = Iseg(1, iqseg)
        nseg = Iseg(3, iqseg)
        iupseg = Idivar(1, iqseg)
C
C5------COUNT THE NUMBER OF TIMES A SEGMENT IS DEFINED.
        Nsegck(nseg) = Nsegck(nseg) + 1
C
C6------MOVE DATA SET 4B
        Iseg(1, nseg) = Iseg(1, iqseg)
        Iotsg(nseg) = Iotsg(iqseg)
        Idivar(1, nseg) = Idivar(1, iqseg)
        IF ( iupseg.GT.0 ) Idivar(2, nseg) = Idivar(2, iqseg)
        Seg(2, nseg) = Seg(2, iqseg)
        Seg(3, nseg) = Seg(3, iqseg)
        Seg(4, nseg) = Seg(4, iqseg)
        Seg(5, nseg) = Seg(5, iqseg)
        IF ( icalc.EQ.1 ) THEN
          Seg(16, nseg) = Seg(16, iqseg)
        ELSE IF ( icalc.EQ.2 ) THEN
          Seg(16, nseg) = Seg(16, iqseg)
          Seg(17, nseg) = Seg(17, iqseg)
        ELSE IF ( icalc.EQ.3 ) THEN
          Seg(9, nseg) = Seg(9, iqseg)
          Seg(10, nseg) = Seg(10, iqseg)
          Seg(14, nseg) = Seg(14, iqseg)
          Seg(15, nseg) = Seg(15, iqseg)
        ELSE IF ( icalc.EQ.4 ) THEN
          Iseg(2, nseg) = Iseg(2, iqseg)
        END IF
C
C7------MOVE DATA SET 4C.
        IF ( icalc.LE.0 ) THEN
          jend = 10
        ELSE IF ( icalc.EQ.1 ) THEN
          jend = 9
        ELSE IF ( icalc.GE.2 .AND. icalc.LE.4 ) THEN
          jend = 8
        END IF
        jj = 6
        DO WHILE ( jj.LE.jend )
          Seg(jj, nseg) = Seg(jj, iqseg)
          jj = jj + 1
        END DO
        Seg(6, nseg) = Seg(6, nseg)*B(ip)
C
C8------MOVE DATA SET 4D.
        IF ( icalc.LE.0 ) THEN
          jend = 15
        ELSE IF ( icalc.EQ.1 ) THEN
          jend = 14
        ELSE IF ( icalc.GE.2 .AND. icalc.LE.4 ) THEN
          jend = 13
        END IF
        jj = 11
        DO WHILE ( jj.LE.jend )
          Seg(jj, nseg) = Seg(jj, iqseg)
          jj = jj + 1
        END DO
        Seg(11, nseg) = Seg(11, nseg)*B(ip)
C
C9------MOVE DATA SET 4E FOR SEGMENT WHEN ICALC IS 2.
        IF ( icalc.EQ.2 ) THEN
          jj = 1
          DO WHILE ( jj.LE.16 )
            Xsec(jj, nseg) = Xsec(jj, iqseg)
            jj = jj + 1
          END DO
        END IF
C
C10-----MOVE DATA SET 4F FOR SEGMENT WHEN ICALC IS 4.
        IF ( icalc.EQ.4 ) THEN
          nstrpts = Iseg(2, nseg)
          jj = 1
          DO WHILE ( jj.LE.nstrpts*3 )
            Qstage(jj, nseg) = Qstage(jj, iqseg)
            jj = jj + 1
          END DO
        END IF
C
C11-----MOVE DATA SET 4G FOR SEGMENT IF SOLUTES SPECIFIED.
        IF ( I15.GT.0 ) THEN
          isol = 1
          DO WHILE ( isol.LE.Nsol )
            IF ( Idivar(1, nseg).EQ.0 ) THEN
              Concq(nseg, isol) = Concq(iqseg, isol)
              Concrun(nseg, isol) = Concrun(iqseg, isol)
              Concppt(nseg, isol) = Concppt(iqseg, isol)
            ELSE
              Concrun(nseg, isol) = Concrun(iqseg, isol)
              Concppt(nseg, isol) = Concppt(iqseg, isol)
            END IF
            isol = isol + 1
          END DO
        END IF
        iqseg = iqseg + 1
      END DO
C12-----RETURN.
      RETURN
      END SUBROUTINE SGWF1SFR2PARMOV
C
C-------SUBROUTINE SGWF2SFR2PRSEG
      SUBROUTINE SGWF1SFR2PRSEG(Nlst, Lstbeg, Iout, Seg, Iseg, Idivar,
     +                          Iotsg, Maxpts, Xsec, Qstage, I15, Concq,
     +                          Concrun, Concppt, Nsol, Nsegdim, Iouts,
     +                          Isfropt, Kkper)
C     ******************************************************************
C     PRINT STREAM SEGMENT DATA -- parameters or non parameters
C     VERSION  2.7: MARCH 16, 2009
C     ******************************************************************
      IMPLICIT NONE

C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL Concppt, Concq, Concrun, Qstage, Seg, Xsec
      INTEGER I15, Idivar, Iotsg, Iout, Iouts, Iseg, Isfropt, Kkper,
     +        Lstbeg, Maxpts, Nlst, Nsegdim, Nsol
      DIMENSION Seg(26, Nsegdim), Iseg(4, Nsegdim), Iotsg(Nsegdim),
     +          Idivar(2, Nsegdim), Xsec(16, Nsegdim),
     +          Qstage(Maxpts, Nsegdim)
      DIMENSION Concq(Nsegdim, Nsol), Concrun(Nsegdim, Nsol),
     +          Concppt(Nsegdim, Nsol)
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      INTEGER i, icalc, iflg, ii, ipt, isol, jj, lstend, nn, nseg,
     +        nstrpts
C
C     ------------------------------------------------------------------
C
      lstend = Nlst + Lstbeg - 1
      WRITE (Iout, 9001)
 9001 FORMAT (1X, 20X, 'INPUT DATA FOR EACH STREAM SEGMENT', /1X,
     +        93('-')/)
C
C1------PRINT INPUT FLOW RATES FOR EACH STREAM SEGMENT.
      WRITE (Iout, 9002)
 9002 FORMAT (1X, 'SEGMENT    SEG.     INFLOW   OVERLAND   ',
     +        'STREAM    STREAM   ICALC  OUTFLOW  DIVERSION PRIORITY',
     +        /4X, 'NO.    LENGTH     RATE     RUNOFF      ',
     +        'ET       PPT.    METH.  TO SEG.  FROM SEG.    NO.'/)
      nseg = Lstbeg
      DO WHILE ( nseg.LE.lstend )
        IF ( Lstbeg.EQ.1 ) THEN
          nn = nseg
        ELSE
          nn = Iseg(3, nseg)
        END IF
        WRITE (Iout, 9003) nn, (Seg(ii, nseg), ii=1, 5), Iseg(1, nseg),
     +                     Iotsg(nseg), (Idivar(jj, nseg), jj=1, 2)
 9003   FORMAT (1X, I6, 1X, 1P5E10.3, 2X, I3, 3X, I6, 3X, I6, 4X, I5)
        nseg = nseg + 1
      END DO
C
C2------PRINT STREAMBED PROPERTIES AND STREAM DIMENSIONS.
      IF ( Lstbeg.EQ.1 ) THEN
        IF ( Isfropt.EQ.0 ) THEN
          WRITE (Iout, 9004)
 9004     FORMAT (1X, //9X, 'STREAMBED PROPERTIES AND STREAM ',
     +            'DIMENSIONS', //1X, 'SEGMENT     BED HYD. COND.', 6X,
     +            'BED THICKNESS     ELEV.-TOP OF BED     WIDTH OF ',
     +            'STREAM     DEPTH OF STREAM    STREAM ROUGHNESS', /1X,
     +            '   No.     UPPER     LOWER     UPPER     ',
     +            'LOWER     UPPER     LOWER     UPPER     LOWER     ',
     +            'UPPER     LOWER   CHANNEL      BANK'/)
        ELSE IF ( Isfropt.GT.0 .AND. Isfropt.LT.4 ) THEN
          WRITE (Iout, 9005)
 9005     FORMAT (1X, //9X, 'STREAMBED PROPERTIES AND STREAM ',
     +            'DIMENSIONS', //1X, 'SEGMENT     WIDTH OF STREAM', 5X,
     +            'DEPTH OF STREAM    STREAM ROUGHNESS', /1X,
     +            '   No.     UPPER     LOWER     UPPER     ',
     +            'LOWER     CHANNEL      BANK'/)
        ELSE IF ( Isfropt.EQ.4 ) THEN
          IF ( Kkper.EQ.1 ) THEN
            WRITE (Iout, 9006)
 9006       FORMAT (1X, //9X, 'STREAMBED PROPERTIES AND STREAM ',
     +              'DIMENSIONS', //1X, 'SEGMENT     BED HYD. COND.',
     +              6X,
     +              'BED THICKNESS     ELEV.-TOP OF BED     WIDTH OF ',
     +              'STREAM     DEPTH OF STREAM    STREAM ROUGHNESS  ',
     +              '  SAT. WATER CONT.    INT. WATER CONT.    BROOKS/',
     +              'COREY EPS.'/1X,
     +              '   No.     UPPER     LOWER     UPPER     ',
     +              'LOWER     UPPER     LOWER     UPPER     LOWER     '
     +              ,
     +              'UPPER     LOWER   CHANNEL      BANK     UPPER     '
     +              , 'LOWER     UPPER     LOWER     UPPER     LOWER'/)
          ELSE
            WRITE (Iout, 9007)
 9007       FORMAT (1X, //9X, 'STREAMBED PROPERTIES AND STREAM ',
     +              'DIMENSIONS', //1X, 'SEGMENT     BED HYD. COND.',
     +              6X,
     +              'BED THICKNESS     ELEV.-TOP OF BED     WIDTH OF ',
     +              'STREAM     DEPTH OF STREAM    STREAM ROUGHNESS',
     +              /1X, '   No.     UPPER     LOWER     UPPER     ',
     +              'LOWER     UPPER     LOWER     UPPER     LOWER     '
     +              , 'UPPER     LOWER   CHANNEL      BANK'/)
          END IF
        ELSE IF ( Isfropt.EQ.5 ) THEN
          IF ( Kkper.EQ.1 ) THEN
            WRITE (Iout, 9008)
 9008       FORMAT (1X, //9X, 'STREAMBED PROPERTIES AND STREAM ',
     +              'DIMENSIONS', //1X, 'SEGMENT     BED HYD. COND.',
     +              6X,
     +              'BED THICKNESS     ELEV.-TOP OF BED     WIDTH OF ',
     +              'STREAM     DEPTH OF STREAM    STREAM ROUGHNESS  ',
     +              '  SAT. WATER CONT.    INT. WATER CONT.   BROOKS/',
     +              'COREY EPS.   UNSAT. HYD. COND.'/1X,
     +              '   No.     UPPER     LOWER     UPPER     ',
     +              'LOWER     UPPER     LOWER     UPPER     LOWER     '
     +              ,
     +              'UPPER     LOWER   CHANNEL      BANK     UPPER     '
     +              ,
     +              'LOWER     UPPER     LOWER     UPPER     LOWER     '
     +              , 'UPPER     LOWER'/)
          ELSE
            WRITE (Iout, 9009)
 9009       FORMAT (1X, //9X, 'STREAMBED PROPERTIES AND STREAM ',
     +              'DIMENSIONS', //1X, 'SEGMENT     BED HYD. COND.',
     +              6X,
     +              'BED THICKNESS     ELEV.-TOP OF BED     WIDTH OF ',
     +              'STREAM     DEPTH OF STREAM    STREAM ROUGHNESS',
     +              /1X, '   No.     UPPER     LOWER     UPPER     ',
     +              'LOWER     UPPER     LOWER     UPPER     LOWER     '
     +              , 'UPPER     LOWER   CHANNEL      BANK'/)
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
      nseg = Lstbeg
      DO WHILE ( nseg.LE.lstend )
        IF ( Lstbeg.EQ.1 ) THEN
          nn = nseg
        ELSE
          nn = Iseg(3, nseg)
        END IF
        icalc = Iseg(1, nseg)
        IF ( icalc.EQ.0 ) THEN
          IF ( Isfropt.EQ.0 .OR. Isfropt.GT.3 ) THEN
            WRITE (Iout, 9010) nn, Seg(6, nseg), Seg(11, nseg),
     +                         Seg(7, nseg), Seg(12, nseg), Seg(8, nseg)
     +                         , Seg(13, nseg), Seg(9, nseg),
     +                         Seg(14, nseg), Seg(10, nseg),
     +                         Seg(15, nseg)
 9010       FORMAT (I6, 1X, 1P10E10.3)
          ELSE
            WRITE (Iout, 9011) nn, Seg(9, nseg), Seg(14, nseg),
     +                         Seg(10, nseg), Seg(15, nseg)
 9011       FORMAT (I6, 1X, 1P4E10.3)
          END IF
        ELSE IF ( icalc.EQ.1 ) THEN
          IF ( Isfropt.EQ.0 ) THEN
            WRITE (Iout, 9012) nn, Seg(6, nseg), Seg(11, nseg),
     +                         Seg(7, nseg), Seg(12, nseg), Seg(8, nseg)
     +                         , Seg(13, nseg), Seg(9, nseg),
     +                         Seg(14, nseg), Seg(16, nseg)
 9012       FORMAT (I6, 1X, 1P8E10.3, 20X, 1PE10.3)
Cdep  Revised print output for icalc=1  7/22/2007
          ELSE IF ( ISFROPT.GE.1 .AND. ISFROPT.LT.4 ) THEN
            WRITE (Iout, 9013) nn, Seg(9, nseg), Seg(14, nseg),
     +                         Seg(16, nseg)
 9013       FORMAT (I6, 3X, 1P2E10.3, 21X, 1PE10.3)
          ELSE IF ( Isfropt.EQ.4 ) THEN
            IF ( Kkper.EQ.1 ) THEN
              WRITE (Iout, 9014) nn, Seg(6, nseg), Seg(11, nseg),
     +                           Seg(7, nseg), Seg(12, nseg),
     +                           Seg(8, nseg), Seg(13, nseg),
     +                           Seg(9, nseg), Seg(14, nseg),
     +                           Seg(16, nseg), Seg(18, nseg),
     +                           Seg(22, nseg), Seg(19, nseg),
     +                           Seg(23, nseg), Seg(20, nseg),
     +                           Seg(24, nseg)
 9014         FORMAT (I6, 1X, 1P8E10.3, 20X, 1PE10.3, 10X, 1P6E10.3)
            ELSE
              WRITE (Iout, 9015) nn, Seg(6, nseg), Seg(11, nseg)
 9015         FORMAT (I6, 1X, 1P2E10.3)
            END IF
          ELSE IF ( Isfropt.EQ.5 ) THEN
            IF ( Kkper.EQ.1 ) THEN
              WRITE (Iout, 9016) nn, Seg(6, nseg), Seg(11, nseg),
     +                           Seg(7, nseg), Seg(12, nseg),
     +                           Seg(8, nseg), Seg(13, nseg),
     +                           Seg(9, nseg), Seg(14, nseg),
     +                           Seg(16, nseg), Seg(18, nseg),
     +                           Seg(22, nseg), Seg(19, nseg),
     +                           Seg(23, nseg), Seg(20, nseg),
     +                           Seg(24, nseg), Seg(21, nseg),
     +                           Seg(25, nseg)
 9016         FORMAT (I6, 1X, 1P8E10.3, 20X, 1PE10.3, 10X, 1P8E10.3)
            ELSE
              WRITE (Iout, 9017) nn, Seg(6, nseg), Seg(11, nseg),
     +                           Seg(16, nseg)
 9017         FORMAT (I6, 1X, 1P2E10.3, 60X, 1PE10.3)
            END IF
          END IF
        ELSE IF ( icalc.EQ.2 ) THEN
          IF ( Isfropt.EQ.0 ) THEN
            WRITE (Iout, 9018) nn, Seg(6, nseg), Seg(11, nseg),
     +                         Seg(7, nseg), Seg(12, nseg), Seg(8, nseg)
     +                         , Seg(13, nseg), Seg(16, nseg),
     +                         Seg(17, nseg)
 9018       FORMAT (I6, 1X, 1P6E10.3, 40X, 1P2E10.3)
Cdep  Revised print output for icalc=2  7/22/2007
          ELSE IF ( ISFROPT.GE.1 .AND. ISFROPT.LT.4 ) THEN
             WRITE (IOUT, 9019) nn, SEG(16, nseg), 
     +                         SEG(17, nseg)
 9019       FORMAT (I6, 44X, 1P2E10.3) 
          ELSE IF ( Isfropt.EQ.4 ) THEN
            IF ( Kkper.EQ.1 ) THEN
              WRITE (Iout, 9020) nn, Seg(6, nseg), Seg(11, nseg),
     +                           Seg(7, nseg), Seg(12, nseg),
     +                           Seg(8, nseg), Seg(13, nseg),
     +                           Seg(16, nseg), Seg(17, nseg),
     +                           Seg(18, nseg), Seg(22, nseg),
     +                           Seg(19, nseg), Seg(23, nseg),
     +                           Seg(20, nseg), Seg(24, nseg)
 9020         FORMAT (I6, 1X, 1P6E10.3, 40X, 1P8E10.3)
            ELSE
              WRITE (Iout, 9021) nn, Seg(6, nseg), Seg(11, nseg),
     +                           Seg(16, nseg), Seg(17, nseg)
 9021         FORMAT (I6, 1X, 1P2E10.3, 80X, 1P2E10.3)
            END IF
          ELSE IF ( Isfropt.EQ.5 ) THEN
            IF ( Kkper.EQ.1 ) THEN
              WRITE (Iout, 9022) nn, Seg(6, nseg), Seg(11, nseg),
     +                           Seg(7, nseg), Seg(12, nseg),
     +                           Seg(8, nseg), Seg(13, nseg),
     +                           Seg(16, nseg), Seg(17, nseg),
     +                           Seg(18, nseg), Seg(22, nseg),
     +                           Seg(19, nseg), Seg(23, nseg),
     +                           Seg(20, nseg), Seg(24, nseg),
     +                           Seg(21, nseg), Seg(25, nseg)
 9022         FORMAT (I6, 1X, 1P6E10.3, 40X, 1P10E10.3)
            ELSE
              WRITE (Iout, 9023) nn, Seg(6, nseg), Seg(11, nseg),
     +                           Seg(16, nseg), Seg(17, nseg)
 9023         FORMAT (I6, 1X, 1P2E10.3, 80X, 1P2E10.3)
            END IF
          END IF
Cdep  Revised print output for icalc>2  7/22/2007
        ELSE IF ( icalc.GE.3 ) THEN
          IF ( ISFROPT.EQ.0 .OR. ISFROPT.GE.4 ) THEN
            WRITE (Iout, 9024) nn, Seg(6, nseg), Seg(11, nseg),
     +                         Seg(7, nseg), Seg(12, nseg), Seg(8, nseg)
     +                         , Seg(13, nseg)
 9024       FORMAT (I6, 1X, 1P6E10.3)
          END IF
        END IF
        nseg = nseg + 1
      END DO
C
C3------PRINT CROSS SECTIONAL DATA FOR SEGMENTS WITH ICALC=2.
      nseg = Lstbeg
      iflg = 0
      DO WHILE ( nseg.LE.lstend )
        IF ( Lstbeg.EQ.1 ) THEN
          nn = nseg
        ELSE
          nn = Iseg(3, nseg)
        END IF
        icalc = Iseg(1, nseg)
        IF ( icalc.EQ.2 .AND. iflg.EQ.0 ) THEN
          WRITE (Iout, 9025)
 9025     FORMAT (1X, /1X, ' EIGHT POINT CROSS SECTION DATA ',
     +            'FOR SEGMENTS WITH ICALC = 2', /3X, ' X VALUES',
     +            ' X VALUES START FROM LEFT SIDE LOOKING ',
     +            'DOWNSTREAM', //5X, 'SEGMENT NO.',
     +            '        X1        X2        X3        X4',
     +            '        X5        X6        X7        X8')
          iflg = 1
        END IF
        IF ( icalc.EQ.2 .AND. iflg.EQ.1 ) THEN
          WRITE (Iout, 9026) nn, (Xsec(i, nseg), i=1, 8)
 9026     FORMAT (7X, I6, 5X, 8(1PE10.3))
        END IF
        nseg = nseg + 1
      END DO
      nseg = Lstbeg
      iflg = 0
      DO WHILE ( nseg.LE.lstend )
        IF ( Lstbeg.EQ.1 ) THEN
          nn = nseg
        ELSE
          nn = Iseg(3, nseg)
        END IF
        icalc = Iseg(1, nseg)
        IF ( icalc.EQ.2 .AND. iflg.EQ.0 ) THEN
          WRITE (Iout, 9027)
 9027     FORMAT (1X, /3X, ' Z VALUES ARE RELATIVE TO STREAM',
     +            'BED ELEVATION', //5X, 'SEGMENT NO.          ',
     +            'Z1        Z2        Z3        Z4        Z5',
     +            '        Z6        Z7        Z8')
          iflg = 1
        END IF
        IF ( icalc.EQ.2 .AND. iflg.EQ.1 ) THEN
          WRITE (Iout, 9028) nn, (Xsec(i, nseg), i=9, 16)
 9028     FORMAT (7X, I6, 5X, 8(1PE10.3))
        END IF
        nseg = nseg + 1
      END DO
C
C4------PRINT STREAMFLOW, DEPTH AND WIDTH RELATIONS FOR SEGMENTS
C         WITH ICALC=3.
      nseg = Lstbeg
      iflg = 0
      DO WHILE ( nseg.LE.lstend )
        IF ( Lstbeg.EQ.1 ) THEN
          nn = nseg
        ELSE
          nn = Iseg(3, nseg)
        END IF
        icalc = Iseg(1, nseg)
        IF ( icalc.EQ.3 .AND. iflg.EQ.0 ) THEN
          WRITE (Iout, 9029)
 9029     FORMAT (/1X, 'STREAMFLOW RELATION WITH DEPTH IS ',
     +            'BASED ON EQUATION Q = CDPTH*(DEPTH)**FDPTH', /1X,
     +            'STREAMFLOW RELATION WITH WIDTH IS ',
     +            'BASED ON EQUATION Q = AWDTH*(WIDTH)**BWDTH', //1X,
     +            'SEGMENT NO.      CDPTH     FDPTH    ',
     +            'AWDTH     BWDTH'/)
          iflg = 1
        END IF
        IF ( icalc.EQ.3 .AND. iflg.EQ.1 ) THEN
          WRITE (Iout, 9030) nn, Seg(9, nseg), Seg(10, nseg),
     +                       Seg(14, nseg), Seg(15, nseg)
Cdep  revised format for ICALC 3 output
 9030     FORMAT (5X, I6, 3X, 1P4E10.3)
        END IF
        nseg = nseg + 1
      END DO
C
C5------PRINT TABULATED VALUES FOR COMPUTING STREAM WIDTH AND DEPTH
C         FROM STREAMFLOW FOR SEGMENTS WITH ICALC=4.
      nseg = Lstbeg
      iflg = 0
      DO WHILE ( nseg.LE.lstend )
        IF ( Lstbeg.EQ.1 ) THEN
          nn = nseg
        ELSE
          nn = Iseg(3, nseg)
        END IF
        icalc = Iseg(1, nseg)
        nstrpts = Iseg(2, nseg)
        IF ( icalc.EQ.4 .AND. iflg.EQ.0 ) THEN
          WRITE (Iout, 9031)
 9031     FORMAT (1X, /1X, 'STREAMFLOW RELATION WITH DEPTH ',
     +            'AND WIDTH IS BASED ON TABULATED VALUES', //2X,
     +            'SEGMENT NO.   STREAMFLOW       DEPTH       ',
     +            'WIDTH', /)
          iflg = 1
        END IF
        ipt = 1
        IF ( icalc.EQ.4 .AND. iflg.EQ.1 ) THEN
          DO WHILE ( ipt.LE.nstrpts )
            WRITE (Iout, 9032) nn, Qstage(ipt, nseg),
     +                         Qstage(nstrpts+ipt, nseg),
     +                         Qstage(2*nstrpts+ipt, nseg)
 9032       FORMAT (5X, I6, 2X, 3(3X, 1PE10.4))
            ipt = ipt + 1
          END DO
        END IF
        nseg = nseg + 1
      END DO
C
C6------PRINT SOLUTE DATA FOR EACH STREAM SEGMENT.
      IF ( I15.GT.0 ) THEN
        isol = 1
        DO WHILE ( isol.LE.Nsol )
          WRITE (Iouts, 9033) isol
 9033     FORMAT (1X, //10X, ' DATA FOR EACH STREAM SEGMENT:',
     +            ' SOLUTE No. ', I2//5X, 'SEGMENT          ',
     +            'SOLUTE CONCENTRATION IN:    ', /5X,
     +            'NUMBER       SEGMENT INFLOW   OVERLAND FLOW', 3X,
     +            'PRECIPITATION')
          nseg = Lstbeg
          DO WHILE ( nseg.LE.lstend )
            IF ( Lstbeg.EQ.1 ) THEN
              nn = nseg
            ELSE
              nn = Iseg(3, nseg)
            END IF
            IF ( Idivar(1, nseg).EQ.0 ) THEN
              WRITE (Iouts, 9034) nn, Concq(nseg, isol),
     +                            Concrun(nseg, isol),
     +                            Concppt(nseg, isol)
            ELSE
              WRITE (Iouts, 9035) nn, Concrun(nseg, isol),
     +                            Concppt(nseg, isol)
            END IF
 9034       FORMAT (1X, /4X, I6, 9X, 1PE10.3, 6X, E10.3, 6X, E10.3)
 9035       FORMAT (1X, /4X, I6, 9X, '   N/A    ', 6X, E10.3, 6X, E10.3)
            nseg = nseg + 1
          END DO
          isol = isol + 1
        END DO
        WRITE (Iouts, 9036)
 9036   FORMAT (//)
      END IF
C
C7------RETURN.
      RETURN
      END SUBROUTINE SGWF1SFR2PRSEG
C
C-------SUBROUTINE GWF1SFR2SEN written by erb, february 10, 2004
      SUBROUTINE GWF1SFR2SEN(Ioutg, Nplist, Nsegdim, Iseg, Isens)
C     ******************************************************************
      IMPLICIT NONE
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      INTEGER Ioutg, Nplist, Nsegdim
      INTEGER Iseg(4, Nsegdim), Isens(Nplist)
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      INTEGER i, ierr
C
C     ******************************************************************
 9001 FORMAT (/, 1X, 'ERROR: The current version of the SFR Package is '
     +        , 'incompatible with the', /,
     +     ' Sensitivity Process when ICALC for any segment has a value'
     +     , ' other than 0 or 1', /,
     +     ' -- STOP EXECUTION (GWF1SFR2SEN)')
 9002 FORMAT (/, 1X,
     +        'ERROR: The current version of MODFLOW-2000 does not',
     +        ' support calculation of', /,
     +        ' sensitivities for parameters defined in the SFR Package'
     +        , /, ' -- STOP EXECUTION (GWF1SFR2SEN)')
C
C1------Check for ICALC values > 1.
      ierr = 0
      DO i = 1, Nsegdim
        IF ( Iseg(1, i).GT.1 ) THEN
          WRITE (Ioutg, 9001)
          ierr = ierr + 1
          GOTO 100
        END IF
      END DO
C
C2------Ensure that sensitivities are not to be calculated for any SFR
C         parameter.
 100  DO i = 1, Nplist
        IF ( PARTYP(i).EQ.'SFR' ) THEN
          IF ( Isens(i).GT.0 ) THEN
            WRITE (Ioutg, 9002)
            ierr = ierr + 1
            GOTO 200
          END IF
        END IF
      END DO
C
 200  IF ( ierr.GT.0 ) CALL USTOP(' ')
C
C3------RETURN.
      RETURN
      END SUBROUTINE GWF1SFR2SEN
C
C-------FUNCTION CALCUNSATFLOBOT
      REAL FUNCTION CALCUNSATFLOBOT(Depth, Sbot, Avhc, Fks, Thti1, Ha,
     +                              Thr, Wetperm, Thetas, Epsilon,
     +                              Sbdthk, Foldflbt, L, Areamax,
     +                              Numcell, Strlen, Fbcheck, Istrm,
     +                              Nstrm, Kkiter, Itr, Fherr, Fherrold,
     +                              Nwavst, Nuzst, Isuzn, Maxwav,
     +                              Nstrail)

      IMPLICIT NONE

C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL Areamax, Avhc, Fherr, Fherrold, Fks, Ha, Strlen, Thti1
      INTEGER Istrm, Itr, Kkiter, L, Nstrm, Numcell
      INTEGER Nwavst, Nuzst, Isuzn, Maxwav, Nstrail
      DOUBLE PRECISION Wetperm, Sbot, Depth, Sbdthk, Foldflbt, Fbcheck,
     +                 Thr, Thetas, Epsilon
      DIMENSION Foldflbt(Nuzst), Istrm(5, Nstrm), Nwavst(Nuzst, Isuzn)
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      REAL flobotcheck, seep
      INTEGER nstrailpls1, ii
      DOUBLE PRECISION flobot, closezero
C     ------------------------------------------------------------------
      nstrailpls1 = 2*Nstrail + 1
      closezero = 1.0D-20
C
C1------START OF CODE FOR COMPUTING AIR ENTRY PRESSURE.
C      IF(THTI1.LE.THR)THEN
C        PORPRESS=HA*(((THR+.01)-THR)/
C     1                (THETAS-THR))**(-(1.0/EPSILON))
C      ELSE
C        PORPRESS=HA*((THTI1-THR)/
C     1                (THETAS-THR))**(-(1.0/EPSILON))
C      END IF
C      IF(KKITER.EQ.1)ITTR=0
C      IF(ITR.EQ.1.AND.KKITER.GT.KKITEROLD)THEN
C        IF(FHERR*FHERROLD.LT.0.0)THEN
C          ITTR=ITTR+1
C        END IF
C      END IF
C      IF(ITTR.GT.10)PORPRESS=0.0
C2-----END OF CODE FOR COMPUTING AIR ENTRY PRESSURE.
      seep = Avhc*(1.0D0+(Depth)/(Sbdthk))
      flobot = seep*Wetperm*Strlen
      IF ( flobot.GE.Fks*Areamax ) flobot = Fks*Areamax
      IF ( ABS(flobot).GT.closezero ) THEN
        flobotcheck = ABS(flobot/(Wetperm*Strlen)-Foldflbt(L)
     +                /(Wetperm*Strlen))
        IF ( flobotcheck.LE.Fbcheck ) flobot = Foldflbt(L)
        DO ii = 1, Isuzn
          IF ( Nwavst(L, ii).GT.Maxwav-nstrailpls1 ) flobot = 0.0D0
        END DO
      END IF
      IF ( flobot-Fbcheck.LT.-closezero ) flobot = 0.0D0
      CALCUNSATFLOBOT = flobot
      END FUNCTION CALCUNSATFLOBOT
C
C-------SUBROUTINE CALC_UNSAT_INFIL
      SUBROUTINE CALC_UNSAT_INFIL(Flobot, Isuzn, Uzseep, Uzthst, Thr,
     +                            Ha, Thetas, Epsilon, Fks, Avhc, Depth,
     +                            Sbdthk, Ntotrl, Wetper, Uzwdth, Nuzst,
     +                            Flow, L, Nwavst, Strlen, Iwidthcheck,
     +                            Icalc)
C     ******************************************************************
C     DEFINE UNSATURATED CELLS TO ACCOMMODATE STREAM LOSS.
C     VERSION  2.7: MARCH 16, 2009
C     ******************************************************************
C
      IMPLICIT NONE

C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL Avhc, Fks, Ha, Strlen
      INTEGER Isuzn, Iwidthcheck, L, Ntotrl, Nuzst, Nwavst, Icalc
      DOUBLE PRECISION Flobot, Uzseep, Uzthst, Depth, Uzwdth, Flow,
     +                 Sbdthk, Wetper, uzflobot, Thr, Thetas, Epsilon
C
      DIMENSION Uzseep(Nuzst, Isuzn), Uzthst(Nuzst, Ntotrl),
     +          Wetper(Nuzst, Isuzn), Uzwdth(Nuzst, Isuzn),
     +          Nwavst(Nuzst, Isuzn)
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      REAL ftaken, porpress
      INTEGER i, imoistcheck, iset, k
      DOUBLE PRECISION flobotleft, seepunsat, disconwidth
C     ------------------------------------------------------------------
      flobotleft = Flobot
      IF ( Flobot.LE.0.0D0 .OR. Flow.LE.0.0D0 ) THEN
        IF ( Uzthst(L, 1).GT.Thr .OR. Nwavst(L, 1).GT.1 ) Uzwdth(L, 1)
     +       = Wetper(L, 1)
        DO i = 1, Isuzn
          Uzseep(L, i) = 0.0D0
        END DO
        RETURN
      END IF
      IF ( Icalc.EQ.1 ) THEN
        Uzwdth(L, 1) = Wetper(L, 1)
        Iwidthcheck = 1
        IF ( Fks.LE.Avhc ) THEN
          Uzseep(L, 1) = Fks
        ELSE
          Uzseep(L, 1) = Avhc*(1.0D0+(Depth)/(Sbdthk))
        END IF
        uzflobot = Uzseep(L, 1)*Uzwdth(L, 1)*Strlen
        IF ( uzflobot.GT.Flow ) Uzseep(L, 1)
     +       = Flow/(Uzwdth(L, 1)*Strlen)
      ELSE IF ( Icalc.EQ.2 ) THEN
        IF ( Uzthst(L, Nwavst(L,1)).LE.Thr ) THEN
          porpress = Ha*(((Thr+.01D0)-Thr)/(Thetas-Thr))
     +               **(-(1.0D0/Epsilon))
        ELSE
          porpress = Ha*((Uzthst(L,Nwavst(L,1))-Thr)/(Thetas-Thr))
     +               **(-(1.0D0/Epsilon))
        END IF
        seepunsat = Avhc*(1.0D0+(Depth-porpress)/(Sbdthk))
        IF ( seepunsat.GT.Fks ) seepunsat = Fks
        imoistcheck = 0
        disconwidth = 0.0D0
        IF ( Uzthst(L, 1).GT.Thr .OR. Nwavst(L, 1).GT.1 )
     +       imoistcheck = 1
        IF ( Flow.GT.0.0 .OR. imoistcheck.EQ.1 ) THEN
          Uzwdth(L, 1) = Wetper(L, 1)
          Iwidthcheck = 1
          disconwidth = Uzwdth(L, 1)
          ftaken = seepunsat*disconwidth*Strlen
          IF ( Flobot.GT.ftaken ) THEN
            Uzseep(L, 1) = seepunsat
            flobotleft = flobotleft - ftaken
          ELSE
            Uzseep(L, 1) = Flobot/(disconwidth*Strlen)
            flobotleft = 0.0D0
          END IF
        END IF
        iset = 1 + Ntotrl/Isuzn
        DO k = 2, Isuzn
          imoistcheck = 0
          IF ( Uzthst(L, iset).GT.Thr .OR. Nwavst(L, k).GT.1 )
     +         imoistcheck = 1
          IF ( flobotleft.GT.0.0 .OR. imoistcheck.EQ.1 ) THEN
            Uzwdth(L, k) = Wetper(L, k)
            Iwidthcheck = k
            IF ( flobotleft.GT.0.0 ) THEN
              disconwidth = disconwidth + Uzwdth(L, k)
              ftaken = seepunsat*Uzwdth(L, k)*Strlen
              IF ( flobotleft.GT.ftaken ) THEN
                Uzseep(L, k) = seepunsat
                flobotleft = flobotleft - ftaken
              ELSE
                Uzseep(L, k) = flobotleft/(Uzwdth(L, k)*Strlen)
                flobotleft = 0.0D0
              END IF
            END IF
          END IF
          iset = iset + Ntotrl/Isuzn
        END DO
      END IF
C       RETURN.
      RETURN
      END SUBROUTINE CALC_UNSAT_INFIL
C
C-------SUBROUTINE UZMASSBAL
      SUBROUTINE UZMASSBAL(Strm, Istrm, H, Nstrm, Buff, Iout, Hld,
     +                     Uzdpst, Uzthst, Uzspst, Uzflst, Epsilon, Thr,
     +                     Thetas, Ltrlst, Itrlst, Itrlsth, Uzflwt,
     +                     Uzstor, Delstor, Nwavst, Nstrail, Ntotrl,
     +                     Iuzt, Iuzn, Nuzst, Uzolsflx, Uzwdth, Fks,
     +                     Ndpelev, Dpthinc, Wetper, Nsets, Uzseep,
     +                     Foldflbt, Ratin, Ratout, Prcntdif, Loop,
     +                     Ncol, Nrow, Nlay, Nuzcol, Nuzrow, Il, Ir, Ic,
     +                     Flobot, Sbot, Strlen, L, Delt, Totim,
     +                     Totflwt, Totuzstor, Totdelstor, Kkper, Kkstp,
     +                     Hstr, Iwidthcheck, Avwat, Wat1, Numave,
     +                     Avdpt, Sfruzbd, Ibd, Icalc, Iunitgage, 
     +                     Gwflow)
C     ******************************************************************
C     COMPUTE INFLOW, OUTFLOW, AND CHANGE IN STORAGE IN UNSATURATED
C     ZONE BENEATH STREAMBED.
C     VERSION  2.7: MARCH 16, 2009
C     ******************************************************************
      IMPLICIT NONE

C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL Avdpt, Avwat, Buff, Delt, Dpthinc, Fks, Prcntdif, Sfruzbd,
     +     Strlen, Strm, Totim, Wat1
      INTEGER Ic, Ibd, Il, Iout, Ir, Itrlst, Itrlsth, Iuzn, Iuzt,
     +        Iwidthcheck, Kkper, Kkstp, L, Loop, Ltrlst, Ncol, Ndpelev,
     +        Nlay, Nrow, Nsets, Nstrail, Nstrm, Ntotrl, Numave, Nuzcol
      INTEGER Nuzrow, Nuzst, Nwavst, Istrm, Icalc, Iunitgage
      DOUBLE PRECISION Sbot, Ratin, Ratout, Flobot, Uzspst, Uzflst,
     +                 Uzdpst, Uzthst, H, Hld, Uzseep, Uzolsflx, Uzflwt,
     +                 Totflwt, Wetper, Uzwdth, Delstor, Uzstor,
     +                 Foldflbt, Hstr, Totdelstor, Totuzstor, Thr,
     +                 Thetas, Epsilon, Gwflow
      DIMENSION Uzspst(Nuzst, Ntotrl), Uzflst(Nuzst, Ntotrl),
     +          Uzthst(Nuzst, Ntotrl), Uzolsflx(Nuzst, Iuzn)
      DIMENSION Uzdpst(Nuzst, Ntotrl), Uzflwt(Nuzst, Iuzn),
     +          Uzstor(Nuzst, Iuzn), Delstor(Nuzst, Iuzn),
     +          Uzwdth(Nuzst, Iuzn), Strm(24, Nstrm), Istrm(5, Nstrm),
     +          Buff(Ncol, Nrow, Nlay), Ltrlst(Nuzst, Ntotrl),
     +          Itrlst(Nuzst, Ntotrl), Itrlsth(Ntotrl),
     +          Nwavst(Nuzst, Iuzn), Foldflbt(Nuzst), Loop(Iuzn)
      DIMENSION Uzseep(Nuzst, Iuzn), Avdpt(Nuzst, Numave)
      DIMENSION Ndpelev(Nuzst, Iuzn), Avwat(Nuzst, Numave),
     +          Wat1(Nuzst, Numave), Sfruzbd(6)
      DIMENSION Dpthinc(Nuzst, Iuzn), Wetper(Nuzst, Iuzn)
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      REAL depthinc, depthsave, fhold, hdif, htest1, htest2, seep, time,
     +     totalwc, totalwc1, uzstorhold, widthsave, ftheta1, ftheta2,
     +     eps_m1
      INTEGER i, ick, icounter, iflag, ii, iset, j, jj, jk, k, kk,
     +        numwavhld, nwavecheck, iuznhold
      DOUBLE PRECISION strtop, fm, fluxdif
      REAL CLOSEZERO
      PARAMETER (CLOSEZERO=1.0E-30)
C
C1------INITIALIZE VARIABLES.
      iflag = 0
      Totflwt = 0.0D0
      Totdelstor = 0.0D0
      Totuzstor = 0.0D0
      strtop = Strm(3, L)
      htest1 = H - Sbot
      htest2 = Hld - Sbot
      hdif = ABS(H-Hld)
      eps_m1 = Epsilon - 1.0D0
      fluxdif = 0.0D0
      iflag = 0
      iset = 1
      IF ( Icalc.EQ.2 ) THEN
        iuznhold = Iuzn
      ELSE IF ( Icalc.EQ.1 ) THEN
        iuznhold = 1
      END IF
      nwavecheck = 0
      DO i = 1, iuznhold
        fluxdif = fluxdif + ABS(Uzseep(L, i)-Uzolsflx(L, i))
        nwavecheck = nwavecheck + Nwavst(L, i)
      END DO
      IF ( fluxdif.LT.1.0E-09 ) iflag = 1
C
C2------NO UNSATURATED ZONE.
      IF ( htest1.GE.0.0D0 .AND. htest2.GE.0.0D0 ) THEN
        iset = 1
        DO i = 1, iuznhold
          Delstor(L, i) = 0.0D0
          Uzstor(L, i) = 0.0D0
          Uzdpst(L, iset) = 0.0D0
          Uzthst(L, iset) = Thr
          iset = iset + Ntotrl/Iuzn
        END DO
        IF ( Flobot.LT.0.0D0 ) THEN
          Ratout = Ratout - Flobot
        ELSE
          Ratin = Ratin + Flobot
        END IF
        Gwflow = Flobot
        Buff(Ic, Ir, Il) = Buff(Ic, Ir, Il) + Flobot
C
C3------REMOVE ALL UNSATURATED ZONE WAVES AND CALCULATE CHANGE IN
C         STORAGE WHEN WATER TABLE RISES TO ELEVATION OF STREAMBED.
      ELSE IF ( htest1.GE.0.0D0 .AND. htest2.LT.0.0D0 ) THEN
        DO kk = 1, iuznhold
          Loop(kk) = 0
        END DO
        iset = 1
        DO i = 1, iuznhold
          ick = 0
          IF ( Uzthst(L, iset).GT.Thr .OR. Nwavst(L, i).GT.1 ) ick = 1
          IF ( i.LE.Iwidthcheck .OR. ick.EQ.1 ) Loop(i) = 1
          iset = iset + Ntotrl/Iuzn
        END DO
        iset = 1
        DO i = 1, iuznhold
          IF ( Loop(i).GT.0 ) THEN
            ick = 0
            Delstor(L, i) = Uzstor(L, i)
            Uzstor(L, i) = 0.0D0
          END IF
          iset = iset + Ntotrl/Iuzn
        END DO
        CALL ROUTWAVESST(L, Nuzst, Delt, seep, H, Hld, Uzdpst, Uzthst,
     +                   Uzflst, Uzspst, Itrlst, Ltrlst, Nwavst,
     +                   Nstrail, Ntotrl, Thr, Thetas, Fks, Epsilon,
     +                   Iuzn, Uzwdth, Uzflwt, Strm, Istrm, Ic, Ir, Il,
     +                   Uzolsflx, Nstrm, Ncol, Nrow, Nlay, Iout,
     +                   Iwidthcheck, time, Nsets, strtop, Uzseep,
     +                   Icalc, Kkstp)
        iset = 1
        DO kk = 1, iuznhold
          Loop(kk) = 0
        END DO
        DO i = 1, iuznhold
          ick = 0
          IF ( Uzthst(L, iset).GT.Thr .OR. Nwavst(L, i).GT.1 ) ick = 1
          IF ( i.LE.Iwidthcheck .OR. ick.EQ.1 ) Loop(i) = 1
          iset = iset + Ntotrl/Iuzn
        END DO
        iset = 1
        DO i = 1, iuznhold
          IF ( Loop(i).GT.0 ) THEN
            Uzdpst(L, iset) = 0.0D0
            Uzthst(L, iset) = Thr
            Uzflst(L, iset) = 0.0D0
            Uzspst(L, iset) = 0.0D0
            Itrlst(L, iset) = 0
            Ltrlst(L, iset) = 0
            Nwavst(L, i) = 1
            DO ii = iset + 1, (iset+Ntotrl/Iuzn) - 1
              Uzdpst(L, ii) = 0.0D0
              Uzthst(L, ii) = Thr
              Uzflst(L, ii) = 0.0D0
              Uzspst(L, ii) = 0.0D0
              Itrlst(L, ii) = 0
              Ltrlst(L, ii) = 0
            END DO
          END IF
          iset = iset + Ntotrl/Iuzn
        END DO
        DO i = 1, iuznhold
          IF ( Loop(i).GT.0 ) THEN
            Uzolsflx(L, i) = 0.0D0
            Totflwt = Totflwt + Uzflwt(L, i)
            Totdelstor = Totdelstor + Delstor(L, i)
          END IF
        END DO
        IF ( Flobot.LT.0.0D0 ) THEN
          Ratout = Ratout - Flobot
          Ratin = Ratin + Totflwt/Delt
          Sfruzbd(2) = Sfruzbd(2) - Totflwt
          Sfruzbd(3) = Sfruzbd(3) + Totflwt
          Sfruzbd(5) = Sfruzbd(5) - Totflwt/Delt
          Sfruzbd(6) = Sfruzbd(6) + Totflwt/Delt
        ELSE
          Ratin = Ratin + Flobot + Totflwt/Delt
          Sfruzbd(2) = Sfruzbd(2) - Totflwt
          Sfruzbd(3) = Sfruzbd(3) + Totflwt
          Sfruzbd(5) = Sfruzbd(5) - Totflwt/Delt
          Sfruzbd(6) = Sfruzbd(6) + Totflwt/Delt
        END IF
        Gwflow = Flobot + Totflwt/Delt
        Buff(Ic, Ir, Il) = Buff(Ic, Ir, Il) + Gwflow
C
C4------CALCULATE CHANGE IN STORAGE AND UPDATE UNSATURATED ZONE WAVES
C         WHEN WATER TABLE REMAINS BELOW STREAMBED ELEVATION
C         AND FLUX IS CONSTANT.
      ELSE IF ( hdif.LT.2.0E-4 .AND. nwavecheck.EQ.iuznhold .AND.
     +          iflag.EQ.1 .AND. htest1.LT.2.0E-3 ) THEN
        iset = 1
        DO i = 1, iuznhold
          Delstor(L, i) = 0.0D0
          Uzdpst(L, iset) = Uzdpst(L, iset) - (H-Hld)
          Uzstor(L, i) = Uzdpst(L, iset)*(Uzthst(L, iset)-Thr)
     +                   *Uzwdth(L, i)*Strlen
          Uzflwt(L, i) = Uzseep(L, i)*Uzwdth(L, i)*Strlen*Delt
          Uzolsflx(L, i) = Uzseep(L, i)
          Gwflow = Gwflow + (Uzflwt(L, i)/Delt)
          iset = iset + Ntotrl/Iuzn 
        END DO
        Ratin = Ratin + Gwflow
        Buff(Ic, Ir, Il) = Buff(Ic, Ir, Il) + Gwflow
C
C5------CALCULATE CHANGE IN STORAGE AND UPDATE UNSATURATED ZONE WAVES
C         WHEN WATER TABLE REMAINS BELOW STREAMBED ELEVATION.
      ELSE IF ( htest1.LT.0.0D0 .AND. htest2.LT.0.0D0 ) THEN
        CALL ROUTWAVESST(L, Nuzst, Delt, seep, H, Hld, Uzdpst, Uzthst,
     +                   Uzflst, Uzspst, Itrlst, Ltrlst, Nwavst,
     +                   Nstrail, Ntotrl, Thr, Thetas, Fks, Epsilon,
     +                   Iuzn, Uzwdth, Uzflwt, Strm, Istrm, Ic, Ir, Il,
     +                   Uzolsflx, Nstrm, Ncol, Nrow, Nlay, Iout,
     +                   Iwidthcheck, time, Nsets, strtop, Uzseep,
     +                   Icalc, Kkstp)
        DO kk = 1, iuznhold
          Loop(kk) = 0
        END DO
        iset = 1
        DO i = 1, iuznhold
          ick = 0
          IF ( Uzthst(L, iset).GT.Thr .OR. Nwavst(L, i).GT.1 ) ick = 1
          IF ( i.LE.Iwidthcheck .OR. ick.EQ.1 ) Loop(i) = 1
          iset = iset + Ntotrl/Iuzn
        END DO
        iset = 1
        DO i = 1, iuznhold
          IF ( Loop(i).GT.0 ) THEN
C
C6------CALCULATE CHANGE IN UNSATURATED ZONE STORAGE WHEN
C         WATER TABLE RISES.
            IF ( H.GT.Hld ) THEN
              fm = 0.0D0
              depthsave = Uzdpst(L, iset)
              jj = iset
              jk = iset + 1
              DO WHILE ( jk.LE.iset+Nwavst(L, i)-1 )
                IF ( ((Sbot-Uzdpst(L,jk)).LE.H) ) jj = jk
                jk = jk + 1
              END DO
              jk = iset + 1
C
C7------WATER TABLE RISES THROUGH WAVES.
              IF ( jj.GE.jk ) THEN
                DO j = iset, iset + Nwavst(L, i) - 1
                  Itrlsth(j) = Itrlst(L, j)
                END DO
                numwavhld = Nwavst(L, i)
                Nwavst(L, i) = Nwavst(L, i) - (jj-iset)
                Uzdpst(L, iset) = depthsave - (H-Hld)
                Uzthst(L, iset) = Uzthst(L, jj)
                Uzflst(L, iset) = Uzflst(L, jj)
                Uzspst(L, iset) = 0.0D0
                Itrlst(L, iset) = 0
                Ltrlst(L, iset) = 0
                k = iset + 1
                DO j = jj + 1, iset + numwavhld - 1
                  Uzdpst(L, k) = Uzdpst(L, j)
                  Uzthst(L, k) = Uzthst(L, j)
                  Uzflst(L, k) = Uzflst(L, j)
                  Uzspst(L, k) = Uzspst(L, j)
                  Itrlst(L, k) = Itrlst(L, j)
                  Ltrlst(L, k) = Ltrlst(L, j)
                  k = k + 1
                END DO
C
C8------LOOP THROUGH NUMBER OF TRAIL WAVES INTERSECTED BY WATER TABLE.
                DO j = iset, jj + 1
                  IF ( j.EQ.jj+1 ) THEN
                    IF ( Itrlsth(j).GT.0 ) THEN
C
C9------LEAD TRAIL WAVE BELOW WATER TABLE AND FIRST TRAIL WAVE IS
C         ABOVE WATER TABLE.
                      IF ( Itrlsth(j).EQ.1 ) THEN
                        Ltrlst(L, j-1) = 1
                        Itrlst(L, j-1) = 0
                        fhold = (Uzthst(L, j-1)-Thr)/(Thetas-Thr)
                        IF ( ABS(Uzthst(L,j-1)-Uzthst(L,j-2))
     +                       .LT.CLOSEZERO ) THEN
                          fhold = ((Uzthst(L,j-1)-Thr)/(Thetas-Thr))
     +                            **Epsilon
                          IF ( fhold.LE.0.0 ) fhold = 0.0
                          Uzspst(L, j-1) = (Epsilon*Fks/(Thetas-Thr))
     +                      *fhold**eps_m1
                        ELSE
                          fhold = ((Uzthst(L,j-2)-Thr)/(Thetas-Thr))
     +                            **Epsilon
                          IF ( fhold.LE.0.0 ) fhold = 0.0
                          ftheta1 = Fks*fhold
                          fhold = ((Uzthst(L,j-1)-Thr)/(Thetas-Thr))
     +                            **Epsilon
                          IF ( fhold.LE.0.0 ) fhold = 0.0
                          ftheta2 = Fks*fhold
                          Uzspst(L, j-1) = (ftheta1-ftheta2)
     +                      /(Uzthst(L, j-2)-Uzthst(L, j-1))
                        END IF
                      ELSE
C
C10-----LEAD TRAIL WAVE BELOW WATER TABLE AND MULTIPLE TRAIL WAVES
C         ABOVE WATER TABLE.
                        DO k = iset + 1, iset + Itrlsth(j)
                          Ltrlst(L, k) = 1
                          Itrlst(L, k) = 0
                          fhold = (Uzthst(L, k)-Thr)/(Thetas-Thr)
                          IF ( ABS(Uzthst(L,k)-Uzthst(L,k-1))
     +                         .LT.CLOSEZERO ) THEN
                            fhold = ((Uzthst(L,k)-Thr)/(Thetas-Thr))
     +                              **Epsilon
                            IF ( fhold.LE.0.0 ) fhold = 0.0
                            Uzspst(L, k) = (Epsilon*Fks/(Thetas-Thr))
     +                        *fhold**eps_m1
                          ELSE
                            fhold = ((Uzthst(L,k-1)-Thr)/(Thetas-Thr))
     +                              **Epsilon
                            IF ( fhold.LE.0.0 ) fhold = 0.0
                            ftheta1 = Fks*fhold
                            fhold = ((Uzthst(L,k)-Thr)/(Thetas-Thr))
     +                              **Epsilon
                            IF ( fhold.LE.0.0 ) fhold = 0.0
                            ftheta2 = Fks*fhold
                            Uzspst(L, k) = (ftheta1-ftheta2)
     +                        /(Uzthst(L, k-1)-Uzthst(L, k))
                          END IF
                        END DO
                      END IF
                    END IF
                  ELSE IF ( j.NE.jj ) THEN
C
C11-----MULTIPLE TRAIL WAVES BELOW AND ABOVE WATER TABLE.
                    IF ( Itrlsth(j).GT.jj-j+1 ) THEN
                      DO k = iset + 1, iset + Itrlsth(j) - (jj-j) - 1
                        Ltrlst(L, k) = 1
                        Itrlst(L, k) = 0
                        fhold = (Uzthst(L, k)-Thr)/(Thetas-Thr)
                        IF ( ABS(Uzthst(L,k)-Uzthst(L,k-1))
     +                       .LT.CLOSEZERO ) THEN
                          fhold = ((Uzthst(L,k)-Thr)/(Thetas-Thr))
     +                            **Epsilon
                          IF ( fhold.LE.0.0 ) fhold = 0.0
                          Uzspst(L, k) = (Epsilon*Fks/(Thetas-Thr))
     +                      *fhold**eps_m1
                        ELSE
                          fhold = ((Uzthst(L,k-1)-Thr)/(Thetas-Thr))
     +                            **Epsilon
                          IF ( fhold.LE.0.0 ) fhold = 0.0
                          ftheta1 = Fks*fhold
                          fhold = ((Uzthst(L,k)-Thr)/(Thetas-Thr))
     +                            **Epsilon
                          IF ( fhold.LE.0.0 ) fhold = 0.0
                          ftheta2 = Fks*fhold
                          Uzspst(L, k) = (ftheta1-ftheta2)
     +                      /(Uzthst(L, k-1)-Uzthst(L, k))
                        END IF
                      END DO
                    END IF
C
C12-----ONLY ONE LEAD TRAIL AND ONE TRAIL WAVE BELOW WATER TABLE
C         AND THERE ARE MUTIPLE TRAIL WAVES IN SET ABOVE WATER TABLE.
                  ELSE IF ( Itrlsth(j).GT.1 ) THEN
                    DO k = iset + 1, iset + Itrlsth(j) - 1
                      Ltrlst(L, k) = 1
                      Itrlst(L, k) = 0
                      IF ( ABS(Uzthst(L,k)-Uzthst(L,k-1)).LT.CLOSEZERO )
     +                     THEN
                        fhold = ((Uzthst(L,k)-Thr)/(Thetas-Thr))
     +                          **Epsilon
                        IF ( fhold.LE.0.0 ) fhold = 0.0
                        Uzspst(L, k) = (Epsilon*Fks/(Thetas-Thr))
     +                                 *fhold**eps_m1
                      ELSE
                        fhold = ((Uzthst(L,k-1)-Thr)/(Thetas-Thr))
     +                          **Epsilon
                        IF ( fhold.LE.0.0 ) fhold = 0.0
                        ftheta1 = Fks*fhold
                        fhold = ((Uzthst(L,k)-Thr)/(Thetas-Thr))
     +                          **Epsilon
                        IF ( fhold.LE.0.0 ) fhold = 0.0
                        ftheta2 = Fks*fhold
                        Uzspst(L, k) = (ftheta1-ftheta2)
     +                                 /(Uzthst(L, k-1)-Uzthst(L, k))
                      END IF
                    END DO
                  END IF
                END DO
C
C13-----DETERMINE VOLUME OF WATER IN WAVES BELOW WATER TABLE.
                fm = 0.0D0
                j = iset
                DO WHILE ( j.LE.iset+Nwavst(L, i)-2 )
                  IF ( Ltrlst(L, j).EQ.1 .AND. Itrlst(L, j+1).GT.0 )
     +                 THEN
                    k = j
                    DO WHILE ( k.LE.j+Itrlst(L, j+1)-1 )
                      fm = fm + (Uzthst(L, k)-Thr)
     +                     *(Uzdpst(L, k)-Uzdpst(L, k+1))
                      k = k + 1
                    END DO
                    IF ( k.EQ.iset+Nwavst(L, i)-1 ) THEN
                      fm = fm + (Uzthst(L, k)-Thr)*Uzdpst(L, k)
                    ELSE IF ( iset+Nwavst(L, i)-1.GT.k+1 .AND.
     +                        Itrlst(L, k+2).GT.0 .AND. Ltrlst(L, k+1)
     +                        .EQ.1 ) THEN
                      fm = fm + (Uzthst(L, k)-Thr)
     +                     *(Uzdpst(L, k)-Uzdpst(L, k+1))
                    ELSE
                      fm = fm + (Uzthst(L, k)-Thr)
     +                     *(Uzdpst(L, k)-Uzdpst(L, k+1))
                    END IF
                    j = k
                  ELSE
                    fm = fm + (Uzthst(L, j)-Thr)
     +                   *(Uzdpst(L, j)-Uzdpst(L, j+1))
                  END IF
                  j = j + 1
                END DO
                IF ( j.EQ.iset+Nwavst(L, i)-1 ) fm = fm +
     +               (Uzthst(L, iset+Nwavst(L,i)-1)-Thr)
     +               *Uzdpst(L, iset+Nwavst(L, i)-1)
C
C14-----COMPUTE VOLUME OF WATER BELOW WATER TABLE WHEN
C         WHEN NO WAVES INTERSECTED.
              ELSE
                fm = 0.0D0
                j = iset
                Uzdpst(L, iset) = Uzdpst(L, iset) - (H-Hld)
                DO WHILE ( j.LE.iset+Nwavst(L, i)-2 )
                  IF ( Ltrlst(L, j).EQ.1 .AND. Itrlst(L, j+1).GT.0 )
     +                 THEN
                    k = j
                    DO WHILE ( k.LE.j+Itrlst(L, j+1)-1 )
                      fm = fm + (Uzthst(L, k)-Thr)
     +                     *(Uzdpst(L, k)-Uzdpst(L, k+1))
                      k = k + 1
                    END DO
                    IF ( k.EQ.iset+Nwavst(L, i)-1 ) THEN
                      fm = fm + (Uzthst(L, k)-Thr)*Uzdpst(L, k)
                    ELSE IF ( iset+Nwavst(L, i)-1.GT.k+1 .AND.
     +                        Itrlst(L, k+2).GT.0 .AND. Ltrlst(L, k+1)
     +                        .EQ.1 ) THEN
                      fm = fm + (Uzthst(L, k)-Thr)
     +                     *(Uzdpst(L, k)-Uzdpst(L, k+1))
                    ELSE
                      fm = fm + (Uzthst(L, k)-Thr)
     +                     *(Uzdpst(L, k)-Uzdpst(L, k+1))
                    END IF
                    j = k
                  ELSE
                    fm = fm + (Uzthst(L, j)-Thr)
     +                   *(Uzdpst(L, j)-Uzdpst(L, j+1))
                  END IF
                  j = j + 1
                END DO
                IF ( j.EQ.iset+Nwavst(L, i)-1 ) fm = fm +
     +               (Uzthst(L, iset+Nwavst(L,i)-1)-Thr)
     +               *Uzdpst(L, iset+Nwavst(L, i)-1)
              END IF
              IF ( fm.LT.0.0 ) fm = 0.0D0
              uzstorhold = Uzstor(L, i)
              Uzstor(L, i) = fm*Uzwdth(L, i)*Strlen
              Delstor(L, i) = Uzstor(L, i) - uzstorhold
C
C15------CALCULATE CHANGE IN UNSATURATED ZONE STORAGE WHEN GROUND-
C          WATER LEVEL DROPS.
            ELSE IF ( H.LE.Hld ) THEN
              fm = 0.0D0
              j = iset
              DO WHILE ( j.LE.iset+Nwavst(L, i)-2 )
                IF ( Ltrlst(L, j).EQ.1 .AND. Itrlst(L, j+1).GT.0 ) THEN
                  k = j
                  DO WHILE ( k.LE.j+Itrlst(L, j+1)-1 )
                    fm = fm + (Uzthst(L, k)-Thr)
     +                   *(Uzdpst(L, k)-Uzdpst(L, k+1))
                    k = k + 1
                  END DO
                  IF ( k.EQ.iset+Nwavst(L, i)-1 ) THEN
                    fm = fm + (Uzthst(L, k)-Thr)*Uzdpst(L, k)
                  ELSE IF ( iset+Nwavst(L, i)-1.GT.k+1 .AND.
     +                      Itrlst(L, k+2).GT.0 .AND. Ltrlst(L, k+1)
     +                      .EQ.1 ) THEN
                    fm = fm + (Uzthst(L, k)-Thr)
     +                   *(Uzdpst(L, k)-Uzdpst(L, k+1))
                  ELSE
                    fm = fm + (Uzthst(L, k)-Thr)
     +                   *(Uzdpst(L, k)-Uzdpst(L, k+1))
                  END IF
                  j = k
                ELSE
                  fm = fm + (Uzthst(L, j)-Thr)
     +                 *(Uzdpst(L, j)-Uzdpst(L, j+1))
                END IF
                j = j + 1
              END DO
              IF ( j.EQ.iset+Nwavst(L, i)-1 ) fm = fm +
     +             (Uzthst(L, iset+Nwavst(L,i)-1)-Thr)
     +             *Uzdpst(L, iset+Nwavst(L, i)-1)
              uzstorhold = Uzstor(L, i)
              Uzstor(L, i) = fm*Uzwdth(L, i)*Strlen
              Delstor(L, i) = Uzstor(L, i) - uzstorhold
            END IF
            IF ( Uzflwt(L, i).LE.0.0 ) Uzflwt(L, i) = 0.0D0
          END IF
          iset = iset + Ntotrl/Iuzn
        END DO
        DO i = 1, iuznhold
          IF ( Loop(i).GT.0 ) THEN
            Totflwt = Totflwt + Uzflwt(L, i)
            Totdelstor = Totdelstor + Delstor(L, i)
            Totuzstor = Totuzstor + Uzstor(L, i)
          END IF
        END DO
        Gwflow = Totflwt/Delt
        Ratin = Ratin + Gwflow
        Buff(Ic, Ir, Il) = Buff(Ic, Ir, Il) + Gwflow
        Sfruzbd(1) = Sfruzbd(1) + Flobot*Delt
        Sfruzbd(2) = Sfruzbd(2) + Totdelstor
        Sfruzbd(3) = Sfruzbd(3) + Totflwt
        Sfruzbd(4) = Sfruzbd(4) + Flobot
        Sfruzbd(5) = Sfruzbd(5) + Totdelstor/Delt
        Sfruzbd(6) = Sfruzbd(6) + Totflwt/Delt
C
C16-----UPDATE ALL UNSATURATED ZONE WAVES WHEN WATER TABLE DROPS
C         BELOW STREAMBED.
      ELSE IF ( htest1.LE.0.0D0 .AND. htest2.GE.0.0D0 ) THEN
        iset = 1
        DO i = 1, iuznhold
          Delstor(l,i) = 0.0D0
          Nwavst(l,i) = 1
          DO j = iset, iset + 5
            UZTHST(l,j) = thr
            UZDPST(l,j) = 0.0D0
            UZSPST(l,j) = 0.0D0
            UZFLST(l,j) = 0.0D0
            ITRLST(l,j) = 0
            LTRLST(l,j) = 0
          END DO
          iset = iset + Ntotrl/Iuzn
        END DO
        CALL ROUTWAVESST(L, Nuzst, Delt, seep, H, Hld, Uzdpst, Uzthst,
     +                   Uzflst, Uzspst, Itrlst, Ltrlst, Nwavst,
     +                   Nstrail, Ntotrl, Thr, Thetas, Fks, Epsilon,
     +                   Iuzn, Uzwdth, Uzflwt, Strm, Istrm, Ic, Ir, Il,
     +                   Uzolsflx, Nstrm, Ncol, Nrow, Nlay, Iout,
     +                   Iwidthcheck, time, Nsets, Sbot, Uzseep, Icalc,
     +                   Kkstp)
        DO kk = 1, iuznhold
          Loop(kk) = 0
        END DO
        iset = 1
        DO i = 1, iuznhold
          ick = 0
          IF ( Uzthst(L, iset).GT.Thr .OR. Nwavst(L, i).GT.1 ) ick = 1
          IF ( i.LE.Iwidthcheck .OR. ick.EQ.1 ) Loop(i) = 1
          iset = iset + Ntotrl/Iuzn
        END DO
        iset = 1
        DO i = 1, iuznhold
          IF ( Loop(i).GT.0 ) THEN
            icounter = iset + Nwavst(L, i) - 1
            Delstor(L, i) = (Uzthst(L, icounter)-Thr)
     +                      *(Uzdpst(L, icounter))
            DO j = iset, iset + Nwavst(L, i) - 2
              Delstor(L, i) = Delstor(L, i) + (Uzthst(L, j)-Thr)
     +                        *(Uzdpst(L, j)-Uzdpst(L, j+1))
            END DO
            Delstor(L, i) = Delstor(L, i)*Uzwdth(L, i)*Strlen
            Uzstor(L, i) = Delstor(L, i)
          END IF
          iset = iset + Ntotrl/Iuzn
        END DO
        DO i = 1, iuznhold
          IF ( Loop(i).GT.0 ) THEN
            Totflwt = Totflwt + Uzflwt(L, i)
            Totdelstor = Totdelstor + Delstor(L, i)
            Totuzstor = Totuzstor + Uzstor(L, i)
          END IF
        END DO
        Gwflow = Totflwt/Delt
        Ratin = Ratin + Gwflow
        Buff(Ic, Ir, Il) = Buff(Ic, Ir, Il) + Gwflow
        Sfruzbd(1) = Sfruzbd(1) + Flobot*Delt
        Sfruzbd(2) = Sfruzbd(2) + Totdelstor
        Sfruzbd(3) = Sfruzbd(3) + Totflwt
        Sfruzbd(4) = Sfruzbd(4) + Flobot
        Sfruzbd(5) = Sfruzbd(5) + Totdelstor/Delt
        Sfruzbd(6) = Sfruzbd(6) + Totflwt/Delt
      END IF
C
C17-----TOTAL WATER CONTENT OVER SPECIFIED DEPTH
C         FOR PRINTING WATER CONTENT PROFILES.
      IF ( Ibd.NE.0 .AND. Iunitgage.GT.0 ) THEN
        IF ( H.LT.Sbot ) THEN
          depthinc = Uzdpst(L, 1)/20.001D0
          depthsave = depthinc
          totalwc = 0.0
          totalwc1 = 0.0
          k = 1
          DO WHILE ( depthsave.LE.Uzdpst(L, 1) .AND. depthsave.GT.0.0 )
            widthsave = 0.0
            iset = 1
            fm = 0.0D0
            DO i = 1, iuznhold
              widthsave = widthsave + Wetper(L, i)
              jj = iset
              jk = iset + Nwavst(L, i) - 1
              DO WHILE ( jk.GE.iset )
                IF ( Uzdpst(L, jk).LT.depthsave ) jj = jk
                jk = jk - 1
              END DO
              IF ( jj.GT.iset ) THEN
                fm = fm + Uzthst(L, jj-1)*(depthsave-Uzdpst(L, jj))
     +               *Wetper(L, i)*Strlen
                DO j = jj, iset + Nwavst(L, i) - 2
                  fm = fm + Uzthst(L, j)*(Uzdpst(L, j)-Uzdpst(L, j+1))
     +                 *Wetper(L, i)*Strlen
                END DO
                fm = fm + Uzthst(L, iset+Nwavst(L, i)-1)
     +               *Uzdpst(L, iset+Nwavst(L, i)-1)*Wetper(L, i)*Strlen
              ELSE
                fm = fm + Uzthst(L, iset+Nwavst(L, i)-1)
     +               *depthsave*Wetper(L, i)*Strlen
              END IF
              IF ( i.EQ.1 ) THEN
                Wat1(L, k) = (fm-totalwc1)/(widthsave*Strlen*depthinc)
                totalwc1 = fm
              END IF
              iset = iset + Ntotrl/Iuzn
            END DO
            Avdpt(L, k) = depthsave
            Avwat(L, k) = (fm-totalwc)/(widthsave*Strlen*depthinc)
            totalwc = fm
            depthsave = depthsave + depthinc
            k = k + 1
          END DO
        END IF
      END IF
C18-----STORE UNSATURATED FLOW RATES FOR GAGE PACKAGE.
      Strm(21, L) = Totflwt/Delt
      Strm(22, L) = Totdelstor/Delt
      Strm(23, L) = Totuzstor
C19-----RETURN.
      RETURN
      END SUBROUTINE UZMASSBAL
C
C-------SUBROUTINE ROUTWAVESIT
      SUBROUTINE ROUTWAVESIT(L, Nuzst, Delt, Seep, H, Hld, Uzdpit,
     +                       Uzthit, Uzflit, Uzspit, Itrlit, Ltrlit,
     +                       Uzdpst, Uzthst, Uzflst, Uzspst, Itrlst,
     +                       Ltrlst, Nwavst, Nstrail, Ntotrl, Thr, Ths,
     +                       Fks, Epsilon, Iuzn, Uzwdth, Uzflwt, Ic, Ir,
     +                       Il, Strm, Istrm, Uzolsflx, Nstrm, Ncol,
     +                       Nrow, Nlay, Iout, Iwidthcheck, Totim,
     +                       Nsets, Strtop, Uzseep, Icalc, Kkstp, Sbot)
C     ******************************************************************
C     ROUTE UNSATURATED ZONE WAVES DURING MODEL ITERATIONS
C     CALLED FROM SUBROUTINE GWF2SFR2FM
C     VERSION  2.7: MARCH 16, 2009
C     ******************************************************************
C
      IMPLICIT NONE

C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL Delt, Fks, Seep, Strm, Totim
      INTEGER Ic, Il, Iout, Ir, Itrlit, Itrlst, Iuzn, Iwidthcheck, L,
     +        Ltrlit, Ltrlst, Ncol, Nlay, Nrow, Nsets, Nstrail, Nstrm,
     +        Ntotrl, Nuzst, Nwavst, Istrm, Icalc, Kkstp
      DOUBLE PRECISION Uzspit, Uzspst, Uzflst, Uzflit, Uzthit, Uzthst,
     +                 Uzdpst, Uzdpit, H, Hld, Strtop, Uzseep, Uzolsflx,
     +                 Uzflwt, Uzwdth, Thr, Ths, Epsilon, Sbot
      DIMENSION Uzspit(Nuzst, Ntotrl), Uzspst(Nuzst, Ntotrl),
     +          Uzflit(Nuzst, Ntotrl), Uzflst(Nuzst, Ntotrl),
     +          Uzthit(Nuzst, Ntotrl), Uzthst(Nuzst, Ntotrl),
     +          Uzolsflx(Nuzst, Iuzn), Istrm(5, Nstrm)
      DIMENSION Uzdpit(Nuzst, Ntotrl), Uzdpst(Nuzst, Ntotrl),
     +          Uzflwt(Nuzst, Iuzn), Uzwdth(Nuzst, Iuzn),
     +          Strm(24, Nstrm), Uzseep(Nuzst, Iuzn)
C     ------------------------------------------------------------------
      DIMENSION Nwavst(Nuzst, Iuzn), Ltrlit(Nuzst, Ntotrl),
     +          Itrlit(Nuzst, Ntotrl), Ltrlst(Nuzst, Ntotrl),
     +          Itrlst(Nuzst, Ntotrl)
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      REAL fluxtime
      INTEGER i, ick, iset, iwav, numwaves, iuzntemp
      DOUBLE PRECISION dlength, zoldist, totflux, surflux, oldsflx
      DOUBLE PRECISION htest2
C     -----------------------------------------------------------------
C
C1------ROUTE WAVES THROUGH EACH UNSATURATED ZONE COMPARTMENT BENEATH
C         STREAM.
      iset = 1
      fluxtime = Delt
      htest2 = Hld - Sbot
      IF ( Icalc.EQ.2 ) THEN
        iuzntemp = Iuzn
      ELSE IF ( Icalc.EQ.1 ) THEN
        iuzntemp = 1
      END IF
      DO i = 1, iuzntemp
        ick = 0
        IF ( Uzthst(L, iset).GT.Thr .OR. Nwavst(L, i).GT.1 ) ick = 1
        IF ( Uzwdth(L, i).LE.0.0 ) Seep = 0.0
        IF ( Uzwdth(L, i).GT.0.0 .OR. ick.EQ.1 ) THEN
          numwaves = Nwavst(L, i)
          IF ( htest2.GT.0.0D0 ) THEN
            DO iwav = iset, iset + 5
              UZTHIT(L, iwav) = thr
              UZDPIT(L, iwav) = 0.0D0
              UZSPIT(L, iwav) = 0.0D0
              UZFLIT(L, iwav) = 0.0D0
              ITRLIT(L, iwav) = 0
              LTRLIT(L, iwav) = 0
              numwaves = 1
              Nwavst(L, i) = 1
            END DO
          ELSE
            numwaves = Nwavst(L, i)
            DO iwav = iset, iset + numwaves - 1
              Uzthit(L, iwav) = Uzthst(L, iwav)
              Uzdpit(L, iwav) = Uzdpst(L, iwav)
              Uzspit(L, iwav) = Uzspst(L, iwav)
              Uzflit(L, iwav) = Uzflst(L, iwav)
              Itrlit(L, iwav) = Itrlst(L, iwav)
              Ltrlit(L, iwav) = Ltrlst(L, iwav)
            END DO
          END IF
          dlength = Uzdpst(L, iset) + Hld - H
          zoldist = Uzdpst(L, iset)
          IF ( dlength.LT.0.0 ) dlength = 0.0D0
          IF ( zoldist.LT.0.0 ) zoldist = 0.0D0
          surflux = Uzseep(L, i)
          oldsflx = Uzolsflx(L, i)
          CALL UZFLOW(L, Nuzst, fluxtime, surflux, dlength, zoldist,
     +                Uzdpit, Uzthit, Uzflit, Uzspit, Itrlit, Ltrlit,
     +                totflux, numwaves, Nstrail, Ntotrl, Thr, Ths, Fks,
     +                Epsilon, oldsflx, iset, Totim, Nsets, Iout, 0,
     +                Istrm, Nstrm)
          IF ( totflux.LT.0.0 ) totflux = 0.0D0
          Uzflwt(L, i) = totflux*Uzwdth(L, i)*Strm(1, L)
          IF ( Uzflwt(L, i).LT.0.0 ) Uzflwt(L, i) = 0.0D0
        ELSE
          Uzflwt(L, i) = 0.0D0
        END IF
        iset = iset + Ntotrl/Iuzn
      END DO
C
C2------RETURN.
      RETURN
      END SUBROUTINE ROUTWAVESIT
C
C-------SUBROUTINE ROUTWAVESST
      SUBROUTINE ROUTWAVESST(L, Nuzst, Delt, Seep, H, Hld, Uzdpst,
     +                       Uzthst, Uzflst, Uzspst, Itrlst, Ltrlst,
     +                       Nwavst, Nstrail, Ntotrl, Thr, Ths, Fks,
     +                       Epsilon, Iuzn, Uzwdth, Uzflwt, Strm, Istrm,
     +                       Ic, Ir, Il, Uzolsflx, Nstrm, Ncol, Nrow,
     +                       Nlay, Iout, Iwidthcheck, Time, Nsets, Sbot,
     +                       Uzseep, Icalc, Kkstp)
C     ******************************************************************
C     ROUTE UNSATURATED-ZONE WAVES AFTER FINAL ITERATION
C     CALLED FROM SUBROUTINE GWF2SFR2BD
C     VERSION  2.7: MARCH 16, 2009
C     ******************************************************************
      IMPLICIT NONE

C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL Delt, Fks, Seep, Strm, Time
      INTEGER Ic, Il, Iout, Ir, Itrlst, Iuzn, Iwidthcheck, L, Ltrlst,
     +        Ncol, Nlay, Nrow, Nsets, Nstrail, Nstrm, Ntotrl, Nuzst,
     +        Nwavst, Istrm, Icalc, Kkstp
      DOUBLE PRECISION Uzspst, Uzflst, Uzdpst, Uzthst, Uzwdth, H, Hld,
     +                 Sbot, Uzseep, Uzolsflx, Uzflwt, Thr, Ths, Epsilon
      DIMENSION Uzspst(Nuzst, Ntotrl), Uzflst(Nuzst, Ntotrl),
     +          Uzolsflx(Nuzst, Iuzn), Uzthst(Nuzst, Ntotrl)
      DIMENSION Uzdpst(Nuzst, Ntotrl), Uzflwt(Nuzst, Iuzn),
     +          Strm(24, Nstrm), Istrm(5, Nstrm)
C     -----------------------------------------------------------------
      DIMENSION Nwavst(Nuzst, Iuzn), Ltrlst(Nuzst, Ntotrl),
     +          Itrlst(Nuzst, Ntotrl), Uzwdth(Nuzst, Iuzn),
     +          Uzseep(Nuzst, Iuzn)
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      REAL totim, fluxtime
      INTEGER i, ick, iset, numwaves, iuzntemp
      DOUBLE PRECISION dlength, zoldist, totflux, surflux, oldsflx,
     +                 closezero
C     -----------------------------------------------------------------
C
C1------ROUTE WAVES THROUGH EACH UNSATURATED ZONE COMPARTMENT BENEATH
C         STREAM.
      closezero = 1.0D-20
      iset = 1
      Sbot = Strm(4, L)
      fluxtime = Delt
      IF ( Icalc.EQ.2 ) THEN
        iuzntemp = Iuzn
      ELSE IF ( Icalc.EQ.1 ) THEN
        iuzntemp = 1
      END IF
      DO i = 1, iuzntemp
        ick = 0
        IF ( Uzthst(L, iset).GT.Thr .OR. Nwavst(L, i).GT.1 ) ick = 1
        IF ( i.GT.Iwidthcheck ) Seep = 0.0
        IF ( i.LE.Iwidthcheck .OR. ick.EQ.1 ) THEN
          numwaves = Nwavst(L, i)
          IF ( ABS(Uzdpst(L, iset)).GT.closezero ) THEN
            dlength = Uzdpst(L, iset) + Hld - H
            zoldist = Uzdpst(L, iset)
          ELSE
            dlength = Sbot - H
            zoldist = 0.0D0
          END IF
          IF ( dlength.LT.0.0D0 ) dlength = 0.0D0
          IF ( zoldist.LT.0.0D0 ) zoldist = 0.0D0
          surflux = Uzseep(L, i)
          oldsflx = Uzolsflx(L, i)
          CALL UZFLOW(L, Nuzst, fluxtime, surflux, dlength, zoldist,
     +                Uzdpst, Uzthst, Uzflst, Uzspst, Itrlst, Ltrlst,
     +                totflux, numwaves, Nstrail, Ntotrl, Thr, Ths, Fks,
     +                Epsilon, oldsflx, iset, totim, Nsets, Iout, 1,
     +                Istrm, Nstrm)
          Nwavst(L, i) = numwaves
          Uzflwt(L, i) = totflux*Uzwdth(L, i)*Strm(1, L)
          IF ( Uzflwt(L, i).LT.0.0 ) Uzflwt(L, i) = 0.0D0
        ELSE
          Uzflwt(L, i) = 0.0D0
          Uzdpst(L, iset) = Uzdpst(L, 1)
        END IF
        Uzolsflx(L, i) = Uzseep(L, i)
        iset = iset + Ntotrl/Iuzn
      END DO
C2------RETURN.
      RETURN
      END SUBROUTINE ROUTWAVESST
C
C--------SUBROUTINE UZFLOW
C
      SUBROUTINE UZFLOW(I, Nuzst, Fluxtime, Surflux, Dlength, Zoldist,
     +                  Depth, Theta, Flux, Speed, Itrwave, Ltrail,
     +                  Totalflux, Numwaves, Numtrail, Ntotrl, Thetar,
     +                  Thetas, Fksat, Eps, Oldsflx, Jpnt, Totim, Nsets,
     +                  Iout, Itt, Istrm, Nstrm)
C     ******************************************************************
C     WAVE INTERACTION WITHIN AN UNSATURATED FLOW COMPARTMENT
C     VERSION  2.7: MARCH 16, 2009
C     ******************************************************************
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL Fksat, Totim, Fluxtime
      INTEGER I, Iout, Itrwave, Itt, Jpnt, Ltrail, Nsets, Ntotrl,
     +        Numtrail, Numwaves, Nuzst, Istrm, Nstrm
      DOUBLE PRECISION Speed, Flux, Depth, Theta, Dlength, Zoldist,
     +                 Totalflux, Surflux, Oldsflx, Thetar, Thetas, Eps
      DIMENSION Speed(Nuzst, Ntotrl), Flux(Nuzst, Ntotrl),
     +          Theta(Nuzst, Ntotrl)
      DIMENSION Depth(Nuzst, Ntotrl), Istrm(5, Nstrm)
      DIMENSION Itrwave(Nuzst, Ntotrl), Ltrail(Nuzst, Ntotrl)
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      INTEGER itester, j, jj, lcheck, itrailflg
      DOUBLE PRECISION ffcheck, feps2, feps, time, fm, dlength2
C     ------------------------------------------------------------------
C     COMMON VARIABLES
C     ------------------------------------------------------------------
      DOUBLE PRECISION THETAB, FLUXB, FLUXHLD2, FINITMOIST
      COMMON /FT1   / THETAB, FLUXB, FLUXHLD2, FINITMOIST

C     ------------------------------------------------------------------
      time = 0.0D0
      Totalflux = 0.0D0
C
C       FEPS IS USED TO SUPPRESS A NEW WAVE WHEN CHANGES IN WATER TABLE
C       ARE NEGLIGIBLE. FEPS2 IS USED TO SUPPRESS A NEW WAVE WHEN
C       CHANGES IN FLUX ARE NEGLIGIBLE.
      feps = 1.0D-12/Fluxtime
      feps2 = 1.0D-12/Fluxtime
      IF ( feps.LT.1.0D-9 ) feps = 1.0D-9
      IF ( feps2.LT.1.0D-9 ) feps2 = 1.0D-9
      fm = 0.0D0
      Oldsflx = Flux(I, Jpnt+Numwaves-1)
      itrailflg = 0
C
C1------DETERMINE IF WATER TABLE IS RISING OR FALLING.
      IF ( (Dlength-Zoldist).LT.-feps2 ) THEN
        dlength2 = Dlength
        Dlength = Zoldist
      ELSE IF ( (Dlength-Zoldist).GT.feps2 ) THEN
        dlength2 = Zoldist + 1.0D0
        IF ( Theta(I, Jpnt).GT.Thetar ) THEN
          DO j = Jpnt + Numwaves, Jpnt + 1, -1
            Theta(I, j) = Theta(I, j-1)
            Flux(I, j) = Flux(I, j-1)
            Speed(I, j) = Speed(I, j-1)
            Depth(I, j) = Depth(I, j-1)
            Itrwave(I, j) = Itrwave(I, j-1)
            Ltrail(I, j) = Ltrail(I, j-1)
          END DO
          IF ( Theta(I, Jpnt+1).GT.Thetar ) THEN
            Speed(I, Jpnt+1) = (Flux(I, Jpnt+1))
     +                         /(Theta(I, Jpnt+1)-Thetar)
          ELSE
            Speed(I, Jpnt+1) = 0.0D0
          END IF
          Theta(I, Jpnt) = Thetar
          Flux(I, Jpnt) = 0.0D0
          Speed(I, Jpnt) = 0.0D0
          Depth(I, Jpnt) = Dlength
          Ltrail(I, Jpnt) = 0
          Numwaves = Numwaves + 1
          IF ( Numwaves.GT.Nsets*Numtrail ) THEN
            WRITE (*, *) 'TOO MANY WAVES IN STREAM CELL', I, Numwaves,
     +                   '   PROGRAM TERMINATED IN UZFLOW-1'
            CALL USTOP(' ')
          END IF
        ELSE
          Depth(I, Jpnt) = Dlength
        END IF
      ELSE
        dlength2 = Zoldist + 1.0D0
      END IF
      fm = 0.0D0
      THETAB = Theta(I, Jpnt)
      FLUXB = Flux(I, Jpnt)
      Totalflux = 0.00D0
      itester = 0
      ffcheck = (Surflux-Flux(I, Jpnt+Numwaves-1))
C
C2------CREATE A NEW WAVE IF SURFACE FLUX CHANGES.
C         CALL TRAILWAVE IF SURFACE FLUX DECREASES.
C         CALL LEADWAVE IF SURFACE FLUX INCREASES.
      IF ( ffcheck.GT.feps2 .OR. ffcheck.LT.-feps2 ) THEN
        Numwaves = Numwaves + 1
        IF ( Numwaves.GT.Nsets*Numtrail ) THEN
          WRITE (*, *) 'TOO MANY WAVES IN STREAM CELL', I, Numwaves,
     +                 '   PROGRAM TERMINATED IN UZFLOW-2'
          CALL USTOP(' ')
        END IF
      ELSE IF ( Numwaves.EQ.1 ) THEN
        itester = 1
      END IF
      IF ( Numwaves.GT.1 ) THEN
        IF ( ffcheck.LT.-feps2 ) THEN
          CALL TRAILWAVE(Numwaves, I, Nuzst, Numtrail, Flux, Theta,
     +                   Speed, Depth, Itrwave, Ltrail, Fksat, Eps,
     +                   Thetas, Thetar, Surflux, Ntotrl, Jpnt, Nsets)
          itrailflg = 1
        END IF
        CALL LEADWAVE(Numwaves, I, Nuzst, time, Totalflux, itester,
     +                Numtrail, lcheck, Flux, Theta, Speed, Depth,
     +                Itrwave, Ltrail, Fluxtime, Fksat, Eps, Thetas,
     +                Thetar, Surflux, Dlength, Ntotrl, Oldsflx, Jpnt,
     +                feps2, Iout, itrailflg)
      END IF
      IF ( itester.EQ.1 ) THEN
        Totalflux = Totalflux + (Fluxtime-time)*Flux(I, Jpnt)
        time = 0.0D0
        itester = 0
      END IF
C
C3------CALCULATE VOLUME OF WATER IN UNSATURATED ZONE LOST WHEN
C         WATER TABLE ROSE AND ADD AS RECHARGE TO GROUND WATER.
      IF ( dlength2.LT.Zoldist ) THEN
        j = 2
        jj = 1
        DO WHILE ( j.LE.Numwaves )
          IF ( Depth(I, Jpnt+j-1).GE.dlength2 ) jj = j
          j = j + 1
        END DO
        IF ( jj.GT.1 .AND. Numwaves.GT.1 ) THEN
          fm = (Depth(I, Jpnt)-Depth(I, Jpnt+1))*(Theta(I, Jpnt)-Thetar)
          DO j = 2, jj - 1
            fm = fm + (Depth(I, Jpnt+j-1)-Depth(I, Jpnt+j))
     +           *(Theta(I, Jpnt+j-1)-Thetar)
          END DO
          fm = fm + (Theta(I, Jpnt+jj-1)-Thetar)
     +         *(Depth(I, Jpnt+jj-1)-dlength2)
        ELSE
          fm = (Depth(I, Jpnt)-dlength2)*(Theta(I, Jpnt)-Thetar)
        END IF
        Dlength = dlength2
        Totalflux = Totalflux + fm
      END IF
C4------RETURN.
      RETURN
      END SUBROUTINE UZFLOW
C
C
C-------SUBROUTINE LEADWAVE
      SUBROUTINE LEADWAVE(Numwaves, I, Nuzst, Time, Totalflux, Itester,
     +                    Numtrail, Lcheck, Flux, Theta, Speed, Depth,
     +                    Itrwave, Ltrail, Fluxtime, Fksat, Eps, Thetas,
     +                    Thetar, Surflux, Dlength, Ntotrl, Oldsflx,
     +                    Jpnt, Feps2, Iout, Itrailflg)
C     ******************************************************************
C     CREATE LEAD WAVE WHEN THE SURFACE FLUX INCREASES AND ROUTE WAVES.
C     VERSION  2.7: MARCH 16, 2009
C     ******************************************************************
C
      IMPLICIT NONE

C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL Fksat, Fluxtime
      INTEGER I, Iout, Itester, Itrwave, Jpnt, Lcheck, Ltrail, Ntotrl,
     +        Numtrail, Numwaves, Nuzst, Itrailflg
      DOUBLE PRECISION Feps2, Speed, Flux, Depth, Theta, Totalflux,
     +                 Surflux, Oldsflx, Thetar, Thetas, Eps
      DOUBLE PRECISION Time, Dlength
      DIMENSION Speed(Nuzst, Ntotrl), Flux(Nuzst, Ntotrl),
     +          Theta(Nuzst, Ntotrl)
      DIMENSION Depth(Nuzst, Ntotrl)
      DIMENSION Itrwave(Nuzst, Ntotrl), Ltrail(Nuzst, Ntotrl)
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      REAL big, comp1, comp2, diff, fhold, f7, f8, ftheta1, ftheta2,
     +     closezero
      INTEGER idif, iflag, iflag2, iflx, iremove, itrwaveb, j, jj, k,
     +        kk, l
      INTEGER more(Ntotrl)
      DOUBLE PRECISION fcheck, ffcheck, bottomtime, eps_m1, feps3
      DOUBLE PRECISION checktime(Ntotrl), shortest, timenew
C     ------------------------------------------------------------------
C     COMMON VARIABLES
C     ------------------------------------------------------------------
      DOUBLE PRECISION THETAB, FLUXB, FLUXHLD2, FINITMOIST
      COMMON /FT1   / THETAB, FLUXB, FLUXHLD2, FINITMOIST

C     ******************************************************************
      eps_m1 = Eps - 1.0D0
      f7 = 0.5
      f8 = 1.0 - f7
      feps3 = 1.0D-12
      big = 1.0E30
      closezero = 1.0E-30
C
C1------INITIALIZE NEWEST WAVE.
      IF ( Itrailflg.EQ.0 ) THEN
        ffcheck = Surflux - Oldsflx
        IF ( ffcheck.GT.Feps2 ) THEN
          Flux(I, Jpnt+Numwaves-1) = Surflux
          IF ( Flux(I, Jpnt+Numwaves-1).LE.0.0D0 )
     +         Flux(I, Jpnt+Numwaves-1) = 0.0D0
          Theta(I, Jpnt+Numwaves-1) = (((Flux(I,Jpnt+Numwaves-1)/Fksat)
     +                                **(1.0D0/Eps))*(Thetas-Thetar))
     +                                + Thetar
          IF ( ABS(Theta(I, Jpnt+Numwaves-1)-Theta(I, Jpnt+Numwaves-2))
     +            .GT.feps3 ) THEN
            Speed(I, Jpnt+Numwaves-1) = (Flux(I, Jpnt+Numwaves-1)-
     +                                Flux(I, Jpnt+Numwaves-2))
     +                                /(Theta(I, Jpnt+Numwaves-1)
     +                                -Theta(I, Jpnt+Numwaves-2))
C     Initialization moved to here 4/19/2006.
            Depth(I, Jpnt+Numwaves-1) = 0.0D0
            Ltrail(I, Jpnt+Numwaves-1) = 0
            Itrwave(I, Jpnt+Numwaves-1) = 0
          ELSE
            Speed(I, Jpnt+Numwaves-1) = 0.0D0
C     Subtract 1 from Numwaves added 4/19/2006.
            Numwaves = Numwaves - 1
          END IF
        END IF
      END IF
C
C2------ROUTE ALL WAVES AND INTERCEPTION OF WAVES OVER TIME STEP.
      diff = 1.0
      iflx = 0
      FLUXHLD2 = Flux(I, Jpnt)
      IF ( Numwaves.EQ.0 ) Itester = 1
      DO WHILE ( diff.GT.1.0E-7 .AND. Itester.NE.1 )
        DO j = 1, Numwaves
          checktime(j) = 0.0D0
          more(j) = 0
        END DO
        j = 2
C
C3------CALCULATE TIME UNTIL A WAVE WILL OVERTAKE NEXT WAVE BELOW.
        DO WHILE ( j.LE.Numwaves )
          IF ( Ltrail(I, Jpnt+j-1).NE.0 .AND. Itrwave(I, Jpnt+j).GT.0 )
     +         THEN
            DO WHILE ( Ltrail(I, Jpnt+j-1).NE.0 .AND. Itrwave(I, Jpnt+j)
     +                 .GT.0 )
              kk = j + Itrwave(I, Jpnt+j)
              IF ( j.GT.2 .AND. ABS(Speed(I,Jpnt+j-2)-Speed(I,Jpnt+j-1))
     +             .GT.closezero ) THEN
                checktime(j) = (Depth(I, Jpnt+j-1)-Depth(I, Jpnt+j-2))
     +                         /(Speed(I, Jpnt+j-2)-Speed(I, Jpnt+j-1))
              ELSE
                checktime(j) = big
              END IF
              IF ( Numwaves.GT.kk ) THEN
                jj = j
                j = j + Itrwave(I, Jpnt+j) + 1
C
C4------LEAD WAVE INTERSECTING TRAIL WAVE.
                fhold = 0.0
                IF ( ABS(Theta(I,Jpnt+jj-1)-Thetar).GT.closezero )
     +               fhold = (f7*Theta(I, Jpnt+j-2)
     +                       +f8*Theta(I, Jpnt+j-3)-Thetar)
     +                       /(Theta(I, Jpnt+jj-1)-Thetar)
                IF ( fhold.LE.0.0 ) fhold = 0.0
                checktime(j) = (Depth(I, Jpnt+j-1)-Depth(I, Jpnt+jj-1)
     +                         *(fhold**eps_m1))
     +                         /(Speed(I, Jpnt+jj-1)*(fhold**eps_m1)
     +                         -Speed(I, Jpnt+j-1))
              ELSE
                j = j + 1
              END IF
            END DO
          ELSE IF ( ABS(Speed(I, Jpnt+j-2)-Speed(I, Jpnt+j-1))
     +              .GT.closezero .AND. j.NE.1 )THEN
            checktime(j) = (Depth(I, Jpnt+j-1)-Depth(I, Jpnt+j-2))
     +                     /(Speed(I, Jpnt+j-2)-Speed(I, Jpnt+j-1))
          ELSE
            checktime(j) = big
          END IF
          j = j + 1
        END DO
        DO j = 2, Numwaves
          IF ( checktime(j).LE.0.0D0 ) checktime(j) = big
        END DO
C
C5------CALCULATE HOW LONG IT WILL TAKE BEFORE DEEPEST WAVE REACHES
C         WATER TABLE.
        IF ( Numwaves.GT.1 .AND. Speed(I, Jpnt+1).GT.CLOSEZERO  ) THEN
          bottomtime = (Depth(I, Jpnt)-Depth(I, Jpnt+1))
     +                 /Speed(I, Jpnt+1)
          IF ( bottomtime.LT.0.0 ) bottomtime = 1.0D-12
        ELSE
          bottomtime = big
        END IF
C
C6------CALCULATE SHORTEST TIME FOR WAVE INTERCEPTION.
        shortest = Fluxtime - Time
        DO j = Numwaves, 3, -1
          IF ( checktime(j).LE.shortest ) THEN
            more(j) = 1
            shortest = checktime(j)
            DO k = j + 1, Numwaves
              IF ( ABS(checktime(k)-checktime(j)).GT.closezero ) more(k)
     +             = 0
            END DO
          END IF
        END DO
        IF ( Numwaves.EQ.2 ) shortest = Fluxtime - Time
C
C7------CHECK IF DEEPEST WAVE REACHES WATER TABLE BEFORE WAVES
C          INTERCEPT EACH OTHER.
        iremove = 0
        timenew = Time
        fcheck = (Time+shortest) - Fluxtime
        IF ( shortest.LT.1.0E-7 ) fcheck = -1.0D0
        IF ( bottomtime.LT.shortest .AND. Time+bottomtime.LE.Fluxtime )
     +       THEN
          j = 2
          DO WHILE ( j.LE.Numwaves )
C
C8------ROUTE TRAIL WAVES.
            IF ( Itrwave(I, Jpnt+j-1).EQ.0 ) THEN
              Depth(I, Jpnt+j-1) = Depth(I, Jpnt+j-1)
     +                             + Speed(I, Jpnt+j-1)*bottomtime
            ELSE
              DO k = j, j + Itrwave(I, Jpnt+j-1) - 1
Cdep check to see if theta = thetar do not route? (3/16/2009)
                IF(Theta(I, Jpnt+j-2)-Thetar.GT.CLOSEZERO) THEN
                  Depth(I, Jpnt+k-1) = Depth(I, Jpnt+j-2)*
     +                                 *((f7*Theta(I,Jpnt+k-1)
     +                                 +f8*Theta(I,Jpnt+k-2)-Thetar)
     +                                 /(Theta(I,Jpnt+j-2)-Thetar))
     +                                 **eps_m1
                END IF
              END DO
              j = j + Itrwave(I, Jpnt+j-1) - 1
            END IF
            j = j + 1
          END DO
          FLUXB = Flux(I, Jpnt+1)
          THETAB = Theta(I, Jpnt+1)
          iflx = 1
          itrwaveb = Itrwave(I, Jpnt+2)
          DO k = 2, Numwaves
            Flux(I, Jpnt+k-2) = Flux(I, Jpnt+k-1)
            Theta(I, Jpnt+k-2) = Theta(I, Jpnt+k-1)
            Speed(I, Jpnt+k-2) = Speed(I, Jpnt+k-1)
            Depth(I, Jpnt+k-2) = Depth(I, Jpnt+k-1)
            Itrwave(I, Jpnt+k-2) = Itrwave(I, Jpnt+k-1)
            Ltrail(I, Jpnt+k-2) = Ltrail(I, Jpnt+k-1)
          END DO
          IF ( itrwaveb.EQ.1 ) THEN
            Itrwave(I, Jpnt+1) = 0
            Ltrail(I, Jpnt+1) = 1
            fhold = (Theta(I, Jpnt+1)-Thetar)/(Thetas-Thetar)
            IF ( fhold.LE.0.0 ) fhold = 0.0
            Speed(I, Jpnt+1) = (Eps*Fksat/(Thetas-Thetar))*fhold**eps_m1
C
C9------MAKE ALL TRAIL WAVES LEAD TRAIL WAVES.
          ELSE IF ( itrwaveb.GT.1 ) THEN
            DO k = Jpnt + 1, Jpnt + itrwaveb
              Itrwave(I, k) = 0
              Ltrail(I, k) = 1
              IF ( ABS(Theta(I,k)-Theta(I,k-1)).LT.closezero ) THEN
                fhold = ((Theta(I,k)-Thetar)/(Thetas-Thetar))**Eps
                IF ( fhold.LE.0.0 ) fhold = 0.0
                Speed(I, k) = (Eps*Fksat/(Thetas-Thetar))*fhold**eps_m1
              ELSE
                fhold = ((Theta(I,k-1)-Thetar)/(Thetas-Thetar))**Eps
                IF ( fhold.LE.0.0 ) fhold = 0.0
                ftheta1 = Fksat*fhold
                fhold = ((Theta(I,k)-Thetar)/(Thetas-Thetar))**Eps
                IF ( fhold.LE.0.0 ) fhold = 0.0
                ftheta2 = Fksat*fhold
                Speed(I, k) = (ftheta1-ftheta2)
     +                        /(Theta(I, k-1)-Theta(I, k))
              END IF
            END DO
          END IF
          iremove = 1
          timenew = Time + bottomtime
          Ltrail(I, Jpnt) = 0
          Speed(I, Jpnt) = 0.0D0
C
C10-----CHECK IF WAVES INTERCEPT BEFORE TIME STEP ENDS.
        ELSE IF ( fcheck.LT.0.0 .AND. Numwaves.GT.2 ) THEN
          j = 2
          DO WHILE ( j.LE.Numwaves )
            IF ( Itrwave(I, Jpnt+j-1).EQ.0 ) THEN
              Depth(I, Jpnt+j-1) = Depth(I, Jpnt+j-1)
     +                             + Speed(I, Jpnt+j-1)*shortest
            ELSE
C
C11-----ROUTE TRAIL WAVES.
              DO k = j, j + Itrwave(I, Jpnt+j-1) - 1
                Depth(I, Jpnt+k-1) = Depth(I, Jpnt+j-2)
     +                               *((f7*Theta(I,Jpnt+k-1)
     +                               +f8*Theta(I,Jpnt+k-2)-Thetar)
     +                               /(Theta(I,Jpnt+j-2)-Thetar))
     +                               **eps_m1
              END DO
              j = j + Itrwave(I, Jpnt+j-1) - 1
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
              IF ( Ltrail(I, Jpnt+j-1).NE.1 ) THEN
                iflag2 = 0
                k = j - 1
                idif = 0
                DO WHILE ( iflag2.EQ.0 )
                  IF ( Itrwave(I, Jpnt+k-1).GT.0 ) THEN
                    iflag2 = 1
                    idif = j - k
                    IF ( idif.EQ.Itrwave(I, Jpnt+k-1) )
     +                   Itrwave(I, Jpnt+k-1) = Itrwave(I, Jpnt+k-1) - 1
                  ELSE
                    k = k - 1
                    IF ( k.EQ.0 ) iflag2 = 1
                  END IF
                END DO
                IF ( j.EQ.3 ) THEN
                  comp1 = ABS(Theta(I, Jpnt+j-1)-THETAB)
                  comp2 = ABS(Flux(I, Jpnt+j-1)-FLUXB)
                  IF ( comp1.LE.1.0E-9 ) Theta(I, Jpnt+j-1) = THETAB -
     +                 1.0D-9
                  IF ( comp2.LE.1.0E-15 ) Flux(I, Jpnt+j-1) = FLUXB -
     +                 1.0D-15
                  Speed(I, Jpnt+j-1) = (Flux(I, Jpnt+j-1)-FLUXB)
     +                                 /(Theta(I, Jpnt+j-1)-THETAB)
                ELSE
                  comp1 = ABS(Theta(I, Jpnt+j-1)-Theta(I, Jpnt+j-3))
                  comp2 = ABS(Flux(I, Jpnt+j-1)-Flux(I, Jpnt+j-3))
                  IF ( comp1.LT.1.0E-9 ) Theta(I, Jpnt+j-1)
     +                 = Theta(I, Jpnt+j-3) - 1.0D-9
                  IF ( comp2.LT.1.0E-15 ) Flux(I, Jpnt+j-1)
     +                 = Flux(I, Jpnt+j-3) - 1.0D-15
                  Speed(I, Jpnt+j-1) = (Flux(I, Jpnt+j-1)-Flux(I, Jpnt+j
     +                                 -3))/(Theta(I, Jpnt+j-1)-Theta(I,
     +                                 Jpnt+j-3))
                END IF
              ELSE IF ( Itrwave(I, Jpnt+j).GT.0 ) THEN
                IF ( ABS(Speed(I,Jpnt+j-2)).GT.closezero ) THEN
C
C13-----CONVERT TRAIL WAVES TO LEAD TRAIL WAVES WHEN LEAD TRAIL
C         WAVE INTERSECTS A LEAD WAVE.
                  DO k = Jpnt + j, Jpnt + j + Itrwave(I, Jpnt+j) - 1
                    Ltrail(I, k) = 1
                    Itrwave(I, k) = 0
                    IF ( ABS(Theta(I,k)-Theta(I,k-1)).LT.closezero )
     +                   THEN
                      fhold = ((Theta(I,k)-Thetar)/(Thetas-Thetar))**Eps
                      IF ( fhold.LE.0.0 ) fhold = 0.0
                      Speed(I, k) = (Eps*Fksat/(Thetas-Thetar))
     +                              *fhold**eps_m1
                    ELSE
                      fhold = ((Theta(I,k-1)-Thetar)/(Thetas-Thetar))
     +                        **Eps
                      IF ( fhold.LE.0.0 ) fhold = 0.0
                      ftheta1 = Fksat*fhold
                      fhold = ((Theta(I,k)-Thetar)/(Thetas-Thetar))**Eps
                      IF ( fhold.LE.0.0 ) fhold = 0.0
                      ftheta2 = Fksat*fhold
                      Speed(I, k) = (ftheta1-ftheta2)
     +                              /(Theta(I, k-1)-Theta(I, k))
                    END IF
                  END DO
                  Ltrail(I, Jpnt+j-1) = 0
                  IF ( j.EQ.3 ) THEN
C
C14-----RECALCULATE FLUX.
                    comp1 = ABS(Theta(I, Jpnt+j-1)-THETAB)
                    comp2 = ABS(Flux(I, Jpnt+j-1)-FLUXB)
                    IF ( comp1.LE.1.0E-9 ) Theta(I, Jpnt+j-1) = THETAB -
     +                   1.0D-9
                    IF ( comp2.LE.1.0E-15 ) Flux(I, Jpnt+j-1) = FLUXB -
     +                   1.0D-15
                    Speed(I, Jpnt+j-1) = (Flux(I, Jpnt+j-1)-FLUXB)
     +                /(Theta(I, Jpnt+j-1)-THETAB)
                    IF ( Flux(I, Jpnt+j-1)-FLUXB.LT.0.0D0 ) THEN
                      fhold = (Theta(I, Jpnt+j-1)-Thetar)
     +                        /(Thetas-Thetar)
                      Speed(I, Jpnt+j-1) = ((Eps*Fksat)/(Thetas-Thetar))
     +                  *fhold**eps_m1
                      Ltrail(I, Jpnt+j-1) = 1
                    ELSE
                      Speed(I, Jpnt+j-1) = (Flux(I, Jpnt+j-1)-FLUXB)
     +                  /(Theta(I, Jpnt+j-1)-THETAB)
                    END IF
C
                  ELSE
C
                    comp1 = ABS(Theta(I, Jpnt+j-1)-Theta(I, Jpnt+j-3))
                    comp2 = ABS(Flux(I, Jpnt+j-1)-Flux(I, Jpnt+j-3))
                    IF ( comp1.LT.1.0E-9 ) Theta(I, Jpnt+j-1)
     +                   = Theta(I, Jpnt+j-3) - 1.0D-9
                    IF ( comp2.LT.1.0E-15 ) Flux(I, Jpnt+j-1)
     +                   = Flux(I, Jpnt+j-3) - 1.0D-15
                    IF ( Flux(I, Jpnt+j-1)-Flux(I, Jpnt+j-3).LT.0.0D0 )
     +                   THEN
                      fhold = (Theta(I, Jpnt+j-1)-Thetar)
     +                        /(Thetas-Thetar)
                      Speed(I, Jpnt+j-1) = ((Eps*Fksat)/(Thetas-Thetar))
     +                  *fhold**eps_m1
                      Ltrail(I, Jpnt+j-1) = 1
                    ELSE
                      Speed(I, Jpnt+j-1)
     +                  = (Flux(I, Jpnt+j-1)-Flux(I, Jpnt+j-3))
     +                  /(Theta(I, Jpnt+j-1)-Theta(I, Jpnt+j-3))
                    END IF
C
                  END IF
                END IF
                iflag2 = 0
                k = j - 1
                idif = 0
                DO WHILE ( iflag2.EQ.0 )
                  IF ( Itrwave(I, Jpnt+k-1).GT.0 ) THEN
                    iflag2 = 1
                    idif = j - k
                    IF ( idif.EQ.Itrwave(I, Jpnt+k-1) )
     +                   Itrwave(I, Jpnt+k-1) = Itrwave(I, Jpnt+k-1) - 1
                  ELSE
                    k = k - 1
                    IF ( k.EQ.0 ) iflag2 = 1
                  END IF
                END DO
                j = j + Itrwave(I, Jpnt+j+1) + 2
              ELSE
                Ltrail(I, Jpnt+j-1) = 0
                Itrwave(I, Jpnt+j) = 0
                IF ( j.EQ.3 ) THEN
                  comp1 = ABS(Theta(I, Jpnt+j-1)-THETAB)
                  comp2 = ABS(Flux(I, Jpnt+j-1)-FLUXB)
                  IF ( comp1.LE.1.0E-9 ) Theta(I, Jpnt+j-1) = THETAB -
     +                 1.0D-9
                  IF ( comp2.LE.1.0E-15 ) Flux(I, Jpnt+j-1) = FLUXB -
     +                 1.0D-15
                  Speed(I, Jpnt+j-1) = (Flux(I, Jpnt+j-1)-FLUXB)
     +                                 /(Theta(I, Jpnt+j-1)-THETAB)
                ELSE
                  comp1 = ABS(Theta(I, Jpnt+j-1)-Theta(I, Jpnt+j-3))
                  comp2 = ABS(Flux(I, Jpnt+j-1)-Flux(I, Jpnt+j-3))
                  IF ( comp1.LT.1.0E-9 ) Theta(I, Jpnt+j-1)
     +                 = Theta(I, Jpnt+j-3) - 1.0D-9
                  IF ( comp2.LT.1.0E-15 ) Flux(I, Jpnt+j-1)
     +                 = Flux(I, Jpnt+j-3) - 1.0D-15
                  Speed(I, Jpnt+j-1) = (Flux(I, Jpnt+j-1)-Flux(I, Jpnt+j
     +                                 -3))/(Theta(I, Jpnt+j-1)-Theta(I,
     +                                 Jpnt+j-3))
                END IF
                iflag2 = 0
                k = j - 1
                idif = 0
                DO WHILE ( iflag2.EQ.0 )
                  IF ( Itrwave(I, Jpnt+k-1).GT.0 ) THEN
                    iflag2 = 1
                    idif = j - k
                    IF ( idif.EQ.Itrwave(I, Jpnt+k-1) ) THEN
                      Itrwave(I, Jpnt+k-1) = Itrwave(I, Jpnt+k-1) - 1
                      IF ( Theta(I, Jpnt+j-1).LE.Theta(I, Jpnt+j-3) )
     +                     THEN
                        Ltrail(I, Jpnt+j-1) = 1
                        fhold = (Theta(I, Jpnt+j-1)-Thetar)
     +                          /(Thetas-Thetar)
                        Speed(I, Jpnt+j-1)
     +                    = ((Eps*Fksat)/(Thetas-Thetar))*fhold**eps_m1
                      END IF
                    END IF
                  ELSE
                    k = k - 1
                    IF ( k.EQ.0 ) iflag2 = 1
                  END IF
                END DO
              END IF
              DO k = l, Numwaves
                Flux(I, Jpnt+k-2) = Flux(I, Jpnt+k-1)
                Theta(I, Jpnt+k-2) = Theta(I, Jpnt+k-1)
                Speed(I, Jpnt+k-2) = Speed(I, Jpnt+k-1)
                Depth(I, Jpnt+k-2) = Depth(I, Jpnt+k-1)
                Itrwave(I, Jpnt+k-2) = Itrwave(I, Jpnt+k-1)
                Ltrail(I, Jpnt+k-2) = Ltrail(I, Jpnt+k-1)
              END DO
              l = Numwaves + 1
              iremove = iremove + 1
            ELSE IF ( Itrwave(I, Jpnt+j-1).GT.0 ) THEN
              j = j + Itrwave(I, Jpnt+j-1) - 1
            END IF
            j = j + 1
            IF ( j.GT.Numwaves ) iflag = 1
          END DO
          timenew = timenew + shortest
C
C15-----CALCULATE TOTAL FLUX TO WATER TABLE DURING REMAINING TIME IN
C         STEP.
        ELSE
C
C16-----ROUTE TRAIL WAVES.
          j = 2
          DO WHILE ( j.LE.Numwaves )
            IF ( Itrwave(I, Jpnt+j-1).EQ.0 ) THEN
              Depth(I, Jpnt+j-1) = Depth(I, Jpnt+j-1)
     +                             + Speed(I, Jpnt+j-1)*(Fluxtime-Time)
            ELSE
              DO k = j, j + Itrwave(I, Jpnt+j-1) - 1
                Depth(I, Jpnt+k-1) = Depth(I, Jpnt+j-2)
     +                               *((f7*Theta(I,Jpnt+k-1)
     +                               +f8*Theta(I,Jpnt+k-2)-Thetar)
     +                               /(Theta(I,Jpnt+j-2)-Thetar))
     +                               **eps_m1
              END DO
              j = j + Itrwave(I, Jpnt+j-1) - 1
            END IF
            j = j + 1
          END DO
          timenew = Fluxtime
        END IF
        Totalflux = Totalflux + FLUXHLD2*(timenew-Time)
        IF ( iflx.EQ.1 ) THEN
          FLUXHLD2 = Flux(I, Jpnt)
          iflx = 0
        END IF
C17-----REMOVE ARRAY ELEMENTS RESULTING FROM INTERCEPTED WAVES.
        Numwaves = Numwaves - iremove
        Time = timenew
        diff = Fluxtime - Time
        IF ( Numwaves.EQ.1 ) Itester = 1
      END DO
C18-----RETURN.
      RETURN
      END SUBROUTINE LEADWAVE
C
C-------SUBROUTINE TRAILWAVE
      SUBROUTINE TRAILWAVE(Numwaves, I, Nuzst, Numtrail, Flux, Theta,
     +                     Speed, Depth, Itrwave, Ltrail, Fksat, Eps,
     +                     Thetas, Thetar, Surflux, Ntotrl, Jpnt, Nsets)

C     ******************************************************************
C     INITIALIZE A NEW SET OF TRAIL WAVES WHEN SURFACE FLUX DECREASES.
C     VERSION  2.7: MARCH 16, 2009
C     ******************************************************************
      IMPLICIT NONE
C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL Fksat
      INTEGER I, Itrwave, Jpnt, Ltrail
      INTEGER Nsets, Ntotrl, Numtrail, Numwaves, Nuzst
      DOUBLE PRECISION Speed, Flux, Depth, Theta, Surflux, Thetar,
     +                 Thetas, Eps
      DIMENSION Speed(Nuzst, Ntotrl), Flux(Nuzst, Ntotrl),
     +          Theta(Nuzst, Ntotrl)
      DIMENSION Depth(Nuzst, Ntotrl)
      DIMENSION Itrwave(Nuzst, Ntotrl), Ltrail(Nuzst, Ntotrl)
C     ------------------------------------------------------------------
C     COMMON VARIABLES
C     ------------------------------------------------------------------
      DOUBLE PRECISION THETAB, FLUXB, FLUXHLD2, FINITMOIST
      COMMON /FT1   / THETAB, FLUXB, FLUXHLD2, FINITMOIST
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      DOUBLE PRECISION smoist, smoistinc, ftrail, fhold, eps_m1
      REAL fnuminc
      INTEGER j, jj, jk, kk, numtrail2
C     ------------------------------------------------------------------
      eps_m1 = Eps - 1.0D0
      THETAB = Theta(I, Jpnt)
      FLUXB = Flux(I, Jpnt)
      numtrail2 = Numtrail
C1------INITIALIZE TRAIL WAVES WHEN SURFACE FLUX DECREASES.
      kk = 1
      FLUXHLD2 = Flux(I, Jpnt)
      IF ( Surflux.LE.0.0D0 ) Surflux = 0.0D0
      smoist = (((Surflux/Fksat)**(1.0D0/Eps))*(Thetas-Thetar)) + Thetar
      IF ( Theta(I, Jpnt+Numwaves-2)-smoist.GT.1.0E-9 ) THEN
        fnuminc = 0.0
        DO jk = 1, Numtrail
          fnuminc = fnuminc + FLOAT(jk)
        END DO
        smoistinc = (Theta(I, Jpnt+Numwaves-2)-smoist)/(fnuminc-1)
        jj = Numtrail
        ftrail = Numtrail + 1
        DO j = Numwaves, Numwaves + numtrail2 - 1
          IF ( j.GT.Ntotrl ) THEN
            WRITE (*, *) 'TOO MANY WAVES IN UNSAT CELL', I, Numwaves,
     +                   '   PROGRAM TERMINATED IN TRAILWAVE SFR2'
            CALL USTOP(' ')
          END IF
          Ltrail(I, Jpnt+j-1) = 0
          Itrwave(I, Jpnt+j-1) = 0
          IF ( j.GT.Numwaves ) THEN
            Theta(I, Jpnt+j-1) = Theta(I, Jpnt+j-2)
     +                           - ((ftrail-FLOAT(jj))*smoistinc)
          ELSE
            Theta(I, Jpnt+j-1) = Theta(I, Jpnt+j-2) - 1.0D-8
          END IF
          jj = jj - 1
          IF ( Theta(I, Jpnt+j-1).LE.Thetar+1.0D-6 ) Theta(I, Jpnt+j-1)
     +         = Thetar + 1.0D-6
          Flux(I, Jpnt+j-1) = Fksat*((Theta(I,Jpnt+j-1)-Thetar)/(Thetas-
     +                        Thetar))**Eps
          IF ( j.EQ.Numwaves ) THEN
            fhold = (Theta(I, Jpnt+j-1)-Thetar)/(Thetas-Thetar)
C     set fhold to zero if a small number added 4/19/2006
            IF ( fhold.LT.1.0D-10 ) fhold = 0.0D0
            Speed(I, Jpnt+j-1) = ((Eps*Fksat)/(Thetas-Thetar))
     +                           *fhold**eps_m1
          ELSE
            Speed(I, Jpnt+j-1) = 0.0D0
          END IF
          kk = kk + 1
          Depth(I, Jpnt+j-1) = 0.0D0
        END DO
        Itrwave(I, Jpnt+Numwaves) = numtrail2 - 1
        Ltrail(I, Jpnt+Numwaves-1) = 1
        Numwaves = Numwaves + numtrail2 - 1
      ELSE
        Ltrail(I, Jpnt+Numwaves-1) = 1
C      moved Theta to before fhold and added depth 4/19/2006.
        Theta(I, Jpnt+Numwaves-1) = Theta(I, Jpnt+Numwaves-2)
        fhold = (Theta(I, Jpnt+Numwaves-1)-Thetar)/(Thetas-Thetar)
        Depth(I, Jpnt+Numwaves-1) = 0.0D0        
        Speed(I, Jpnt+Numwaves-1) = ((Eps*Fksat)/(Thetas-Thetar))
     +                              *fhold**eps_m1
        Flux(I, Jpnt+Numwaves-1) = Fksat*((Theta(I,Jpnt+Numwaves-1)-
     +                             Thetar)/(Thetas-Thetar))**Eps

      END IF
C2------RETURN.
      RETURN
      END SUBROUTINE TRAILWAVE
C
C-------SUBROUTINE channelarea
      SUBROUTINE CHANNELAREA(Dpthinc, Wetper, Xsec, Istsg, Iuzn, L,
     +                       Nuzst, Nss, Iout)
C     ******************************************************************
C     COMPARTMENTALIZE UNSATURATED ZONE BENEATH STREAMBED ON BASIS OF
C     EIGHT POINT CROSS-SECTION WHEN ICALC IS 2
C     VERSION  2.7: MARCH 16, 2009
C     ******************************************************************
      IMPLICIT NONE

C     ------------------------------------------------------------------
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL Dpthinc, Xsec
      INTEGER Iout, Istsg, Iuzn, L, Nss, Nuzst
      DOUBLE PRECISION Wetper
      DIMENSION Dpthinc(Nuzst, Iuzn), Wetper(Nuzst, Iuzn), Xsec(16, Nss)
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      REAL area, area1, b, chap, ffmax, ffmin, finc, fmax, fmin, slope,
     +     stage, wethold, wetted, xinc, xmid, xx, y1, y2, yy
      INTEGER i, ii, j, k, ll, mark(20)

C     ******************************************************************
      area = 0.0
      area1 = 0.0
      wethold = 0.0
      wetted = 0.0
C
C1------CALCULATE THE MAXIMUM AND MINIMUM CHANNEL ELEVATIONS.
      fmin = Xsec(9, Istsg)
      fmax = Xsec(9, Istsg)
      DO i = 2, 8
        IF ( Xsec(8+i, Istsg).LT.fmin ) fmin = Xsec(8+i, Istsg)
        IF ( Xsec(8+i, Istsg).GT.fmax ) fmax = Xsec(8+i, Istsg)
      END DO
      finc = (fmax-fmin)/Iuzn
      DO i = 1, Iuzn
        Dpthinc(L, i) = i*finc
      END DO
C
C2------CALCULATE WETTED PERIMETERS FOR INCREMENTAL RIVER DEPTHS.
      DO i = 1, Iuzn
        stage = Dpthinc(L, i)
        area = 0.0
        wetted = 0.0
C
C3------DETERMINE POINTS THAT ARE BELOW STREAM STAGE.
        k = 0
        DO j = 2, 8
          IF ( Xsec(8+j, Istsg).LT.stage .OR. Xsec(8+j-1, Istsg)
     +         .LT.stage ) THEN
            k = k + 1
            mark(k) = j
          END IF
        END DO
C
C4------BREAK CHANNEL UP INTO A SERIES OF LINES BETWEEN POINTS
C   AND CALCULATE EQUATION OF EACH LINE.

        DO ll = 1, k
          chap = (Xsec(8+mark(ll)-1, Istsg)-Xsec(8+mark(ll), Istsg))
          IF ( abs(Xsec(8+mark(ll), Istsg)-Xsec(8+mark(ll)-1, Istsg))
     +         .lt.1.0e-30.AND.abs(Xsec(mark(ll), Istsg)-
     +         Xsec(mark(ll)-1, Istsg)).lt.1.0e-30 )THEN
            WRITE (*, *) 'two crossection points are identical,',
     +                   ' check input. Segment number ', Istsg
          ELSE IF ( abs(Xsec(8+mark(ll), Istsg)-
     +              Xsec(8+mark(ll)-1, Istsg)).lt.1.0e-30) THEN
            slope = 1.0E-5
          ELSE IF ( abs(Xsec(mark(ll), Istsg)-Xsec(mark(ll)-1, Istsg))
     +              .lt.1.0e-30 )THEN
            slope = 10.0
          ELSE
            slope = (Xsec(8+mark(ll), Istsg)-Xsec(8+mark(ll)-1, Istsg))
     +              /(Xsec(mark(ll), Istsg)-Xsec(mark(ll)-1, Istsg))
          END IF

          ffmin = Xsec(8+mark(ll), Istsg)
          ffmax = Xsec(8+mark(ll)-1, Istsg)
          IF ( ffmin.GT.ffmax ) THEN
            ffmin = Xsec(8+mark(ll)-1, Istsg)
            ffmax = Xsec(8+mark(ll), Istsg)
          END IF
          b = Xsec(8+mark(ll)-1, Istsg) - slope*Xsec(mark(ll)-1, Istsg)
C5------COMPUTE WETTED AREA ASSUMING A FLAT CHANNEL BOTTOM.
          IF ( abs(chap).lt.1.0e-30 ) THEN
            area1 = (Xsec(mark(ll), Istsg)-Xsec(mark(ll)-1, Istsg))
     +              *(stage-ffmin)
          ELSE
C
C6------DETERMINE IF STREAM STAGE IS BETWEEN POINTS.
            IF ( stage.GT.ffmax ) THEN
              xinc = (Xsec(mark(ll), Istsg)-Xsec(mark(ll)-1, Istsg))/50.
              xmid = Xsec(mark(ll)-1, Istsg)
            ELSE
              ffmax = stage
              xmid = (stage-b)/slope

C
C7------MOVE DOWN THE CHANNEL BANK OR UP OTHER SIDE.
              IF ( Xsec(8+mark(ll)-1, Istsg).LT.Xsec(8+mark(ll), Istsg)
     +             ) THEN
                xinc = (ABS(Xsec(mark(ll)-1,Istsg)-xmid))/50.
                xmid = Xsec(mark(ll)-1, Istsg)
              ELSE
                xinc = (ABS(Xsec(mark(ll),Istsg)-xmid))/50.
              END IF
            END IF

C8------CALCULATE WETTED PERIMETER.
            xx = ABS(xmid-Xsec(mark(ll), Istsg))
            yy = ABS(ffmax-ffmin)
            wetted = wetted + SQRT((xx**2)+(yy**2))
C9------BREAK AREA UP INTO TRAPAZOIDS.
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
          Wetper(L, i) = wetted
        ELSE
          Wetper(L, i) = (wetted-wethold)
        END IF
        wethold = wetted
      END DO
C
C10-----RETURN.
      RETURN
      END SUBROUTINE CHANNELAREA
