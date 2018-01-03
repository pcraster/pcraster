      MODULE GWFHUFMODULE
        INTEGER, SAVE,   POINTER ::IHUFCB,NHUF,NPHUF,IWETIT,IHDWET,
     1                             IOHUFHDS,IOHUFFLWS
        REAL,    SAVE,   POINTER ::WETFCT
        CHARACTER(LEN=10),SAVE, POINTER, DIMENSION(:)     ::HGUNAM    
        INTEGER, SAVE,   POINTER, DIMENSION(:)     ::LTHUF
        INTEGER, SAVE,   POINTER, DIMENSION(:)     ::LAYWT
        INTEGER, SAVE,   POINTER, DIMENSION(:,:)   ::IHGUFLG
        REAL,    SAVE,   POINTER, DIMENSION(:)     ::HGUHANI
        REAL,    SAVE,   POINTER, DIMENSION(:)     ::HGUVANI
        REAL,    SAVE,   POINTER, DIMENSION(:)     ::HUFHK
        REAL,    SAVE,   POINTER, DIMENSION(:)     ::HUFVK
        REAL,    SAVE,   POINTER, DIMENSION(:)     ::HUFSS
        REAL,    SAVE,   POINTER, DIMENSION(:)     ::HUFSY
        REAL,    SAVE,   POINTER, DIMENSION(:)     ::HUFHANI
        REAL,    SAVE,   POINTER, DIMENSION(:)     ::HUFKDEP
        REAL,    SAVE,   POINTER, DIMENSION(:,:)   ::GS,SC2HUF
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::VKAH
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::SC1
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::WETDRY
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::HK
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::HKCC
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::HUFTMP
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::VDHD
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:,:) ::HUFTHK
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:,:) ::VDHT
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:,:) ::A9
      TYPE GWFHUFTYPE
        INTEGER, POINTER ::IHUFCB,NHUF,NPHUF,IWETIT,IHDWET,
     1                     IOHUFHDS,IOHUFFLWS 
        REAL, POINTER    ::WETFCT
        CHARACTER(LEN=10), POINTER, DIMENSION(:)     ::HGUNAM    
        INTEGER,   POINTER, DIMENSION(:)     ::LTHUF
        INTEGER,   POINTER, DIMENSION(:)     ::LAYWT
        INTEGER,   POINTER, DIMENSION(:,:)   ::IHGUFLG
        REAL,      POINTER, DIMENSION(:)     ::HGUHANI
        REAL,      POINTER, DIMENSION(:)     ::HGUVANI
        REAL,      POINTER, DIMENSION(:)     ::HUFHK
        REAL,      POINTER, DIMENSION(:)     ::HUFVK
        REAL,      POINTER, DIMENSION(:)     ::HUFSS
        REAL,      POINTER, DIMENSION(:)     ::HUFSY
        REAL,      POINTER, DIMENSION(:)     ::HUFHANI
        REAL,      POINTER, DIMENSION(:)     ::HUFKDEP
        REAL,      POINTER, DIMENSION(:,:)   ::GS,SC2HUF
        REAL,      POINTER, DIMENSION(:,:,:) ::VKAH
        REAL,      POINTER, DIMENSION(:,:,:) ::SC1
        REAL,      POINTER, DIMENSION(:,:,:) ::WETDRY
        REAL,      POINTER, DIMENSION(:,:,:) ::HK
        REAL,      POINTER, DIMENSION(:,:,:) ::HKCC
        REAL,      POINTER, DIMENSION(:,:,:) ::HUFTMP
        REAL,      POINTER, DIMENSION(:,:,:) ::VDHD
        REAL,      POINTER, DIMENSION(:,:,:,:) ::HUFTHK
        REAL,      POINTER, DIMENSION(:,:,:,:) ::VDHT
        REAL,      POINTER, DIMENSION(:,:,:,:) ::A9
      END TYPE
      TYPE(GWFHUFTYPE) GWFHUFDAT(10)
      END MODULE GWFHUFMODULE

! Time of File Save by ERB: 7/16/2004 5:20PM
      SUBROUTINE GWF2HUF7AR(IN,ILVDA,IKDEP,IGRID)
C
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR HYDROGEOLOGIC UNIT PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,BOTM,ITRSS,LAYHDT,LAYHDS,
     1                      IOUT,ISSFLG,NPER,IBOUND,LBOTM
      USE GWFBASMODULE,ONLY:HDRY
      USE GWFHUFMODULE,ONLY:IHUFCB,NHUF,NPHUF,IWETIT,IHDWET,IOHUFHDS,
     1                      IOHUFFLWS,WETFCT,HGUNAM,LTHUF,LAYWT,
     2                      IHGUFLG,HGUHANI,HGUVANI,HUFHK,HUFVK,HUFSS,
     3                      HUFSY,HUFHANI,HUFKDEP,GS,VKAH,SC1,WETDRY,HK,
     4                      HKCC,HUFTMP,VDHD,HUFTHK,VDHT,A9,SC2HUF 

      CHARACTER*10 TMPNAM,CTMP1
      CHARACTER*14 LAYPRN(5),TYPNAM(2),VKANAM(2),WETNAM(2),HANNAM,
     &             LAYPRN2 
      DATA TYPNAM/'      CONFINED','   CONVERTIBLE'/
      DATA VKANAM/'    VERTICAL K','    ANISOTROPY'/
      DATA WETNAM/'  NON-WETTABLE','      WETTABLE'/
      DATA HANNAM/'      VARIABLE'/
C
      INTEGER          ::IFLG(5)
C
      CHARACTER*200 LINE
      CHARACTER*24 ANAME(8)
      CHARACTER*4 PTYP
      DOUBLE PRECISION HN
C
      DATA ANAME(1) /'   HYD. COND. ALONG ROWS'/
      DATA ANAME(2) /'  HORIZ. ANI. (COL./ROW)'/
      DATA ANAME(3) /'     VERTICAL HYD. COND.'/
      DATA ANAME(4) /' HORIZ. TO VERTICAL ANI.'/
      DATA ANAME(5) /'QUASI3D VERT. HYD. COND.'/
      DATA ANAME(6) /'        SPECIFIC STORAGE'/
      DATA ANAME(7) /'          SPECIFIC YIELD'/
      DATA ANAME(8) /'        WETDRY PARAMETER'/

C     ------------------------------------------------------------------
      ALLOCATE(IHUFCB,NHUF,NPHUF,IWETIT,IHDWET,IOHUFHDS,
     &         IOHUFFLWS)
      ALLOCATE(WETFCT)
C
C1------IDENTIFY PACKAGE
      WRITE(IOUT,1) IN
    1 FORMAT(1X,/1X,'HUF2 -- HYDROGEOLOGIC-UNIT FLOW PACKAGE,
     & VERSION 7.0, 05/05/2005',/,' INPUT READ FROM UNIT',I3,/)
C
C2------READ FIRST RECORD AND WRITE
      CALL URDCOM(IN,IOUT,LINE)
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHUFCB,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,HDRY,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NHUF,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPHUF,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IOHUFHDS,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IOHUFFLWS,R,-1,IN)
      IF(IHUFCB.LT.0) WRITE(IOUT,8)
    8 FORMAT(1X,'CONSTANT-HEAD CELL-BY-CELL FLOWS WILL BE PRINTED',
     &  ' WHEN ICHUFL IS NOT 0')
      IF(IHUFCB.GT.0) WRITE(IOUT,9) IHUFCB
    9 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT',I3)
      WRITE(IOUT,11) HDRY
   11 FORMAT(1X,'HEAD AT CELLS THAT CONVERT TO DRY=',1PG13.5)
      if(NHUF.GT.0) WRITE(IOUT,12) NHUF,NPHUF
   12 format(1x,'Hydrogeologic-Unit Flow Package Active with ',i3,
     &  ' units and ',i3,' parameters')
      IF(IOHUFHDS.GT.0) WRITE(IOUT,15) IOHUFHDS
   15 FORMAT(1X,'HEADS IN HYDROGEOLOGIC UNITS WILL BE SAVED',
     &  ' ON UNIT',I3)
      LINELEN=LEN(LINE)
      IF(LINE(LINELEN:LINELEN).EQ.'E'.OR.ISTART.EQ.LINELEN) THEN
        IOHUFFLWS = 0
        WRITE(IOUT,17)
   17   FORMAT(/1X,'IOHUFFLWS NOT FOUND AND WILL BE SET TO BE ZERO',/)
      ENDIF
      IF(IOHUFFLWS.GT.0) WRITE(IOUT,18) IOHUFFLWS
   18 FORMAT(1X,'FLOWS IN HYDROGEOLOGIC UNITS WILL BE SAVED',
     &  ' ON UNIT',I3)
C
C  Allocate space in parameter-information arrays
      CALL UPARARRAL(0,IOUT,LINE,NPHUF)
   
C-------CHECK FOR LVDA 
      NPLVDA=0
      IF(ILVDA .NE. 0)THEN
C-------IDENTIFY PACKAGE
        WRITE(IOUT,21) ILVDA 
   21   FORMAT(1X,/1X,'LVDA1 -- MODEL-LAYER VARIABLE-DIRECTION
     &  HORIZONTAL ANISOTROPY CAPABILITY,
     &  VERSION 7.0, 05/05/2005',/,' INPUT READ FROM UNIT',I3,/)
C
C2------READ FIRST RECORD AND WRITE
        CALL URDCOM(ILVDA,IOUT,LINE)
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPLVDA,R,IOUT,ILVDA)
C
C  Allocate space in parameter-information arrays
        CALL UPARARRAL(0,IOUT,LINE,NPLVDA)
      ENDIF
C
C-------CHECK FOR KDEP 
      IFKDEP=0
      NPKDEP=0
      IF(IKDEP .NE. 0)THEN
C-------IDENTIFY PACKAGE
        WRITE(IOUT,31) IKDEP
   31 FORMAT(1X,/1X,'KDEP1 -- HYDRAULIC CONDUCTIVITY DEPTH-DECAY
     &   VERSION 7.0, 05/05/2005',/,' INPUT READ FROM UNIT',I3,/)
C
C-------READ FIRST RECORD AND WRITE
        CALL URDCOM(IKDEP,IOUT,LINE)
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NPKDEP,R,IOUT,IKDEP)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFKDEP,R,IOUT,IKDEP)
C
C  Allocate space in parameter-information arrays
        CALL UPARARRAL(0,IOUT,LINE,NPKDEP)
      ENDIF
C
C4------READ LTHUF, LAYWT.
      ALLOCATE(LTHUF(NLAY))
      ALLOCATE(LAYWT(NLAY))
      ALLOCATE(HGUNAM(NHUF))
      ALLOCATE(HGUHANI(NHUF))
      ALLOCATE(HGUVANI(NHUF))
      ALLOCATE(HUFHK(NHUF))
      ALLOCATE(HUFVK(NHUF))
      ALLOCATE(HUFSS(NHUF))
      ALLOCATE(HUFSY(NHUF))
      ALLOCATE(HUFHANI(NHUF))
      ALLOCATE(HUFKDEP(NHUF))
      READ(IN,*) (LTHUF(K),K=1,NLAY)
      READ(IN,*) (LAYWT(K),K=1,NLAY)
C
C4A-----PRINT TABLES OF VALUES FOR LTHUF, HGUHANI, HGUVANI
C4B-----BASED ON LTHUF, HUFLAYAVG, HGUHANI, LAYWT, COUNT THE NUMBER OF EACH
C4B-----TYPE OF 2-D ARRAY; CHECK VALUES FOR CONSISTENCY; AND SETUP
C4B-----POINTERS IN LTHUF, HGUHANI, AND LAYWT FOR CONVENIENT ACCESS
C4B-----TO SC2, HANI, and WETDRY.  PRINT INTERPRETED VALUES OF FLAGS.
      WRITE(IOUT,47)
   47 FORMAT(1X,/3X,'INTERPRETATION OF LAYER FLAGS:',/1X,
     & 'LAYER     LTHUF    LAYER TYPE     LAYWT WETTABILITY',
     & /1X,75('-'))
      NCNVRT=0
      NWETD=0
      DO 50 K=1,NLAY
        IF(LTHUF(K).NE.0) THEN
          NCNVRT=NCNVRT+1
          LTHUF(K)=NCNVRT
        END IF
        IF(LAYWT(K).NE.0) THEN
          IF(LTHUF(K).EQ.0) THEN
            WRITE(IOUT,*)
     &          ' LAYWT is not 0 and LTHUF is 0 for layer:',K
            WRITE(IOUT,*) ' LAYWT must be 0 if LTHUF is 0'
            CALL USTOP(' ')
          ELSE
            NWETD=NWETD+1
            LAYWT(K)=NWETD
          END IF
        END IF
        LAYPRN(1)=TYPNAM(1)
        IF(LTHUF(K).NE.0) LAYPRN(1)=TYPNAM(2)
        LAYPRN(5)=WETNAM(1)
        IF(LAYWT(K).NE.0) LAYPRN(5)=WETNAM(2)
        WRITE(IOUT,78) K,LTHUF(K),LAYPRN(1),LAYWT(K),LAYPRN(5)
   78   FORMAT(1X,I4,2(I10,A14))
C     SET GLOBAL HEAD-DEPENDENT THICKNESS INDICATOR
      IF (LTHUF(K).NE.0) THEN
        LAYHDT(K)=1
        LAYHDS(K)=1
      ELSE
        LAYHDT(K)=0
        LAYHDS(K)=0
      ENDIF
   50 CONTINUE
C
C
C5------COMPUTE THE NUMBER OF CELLS IN THE ENTIRE GRID AND IN ONE LAYER.
C     NRC=NROW*NCOL
C
C6------ALLOCATE SPACE FOR ARRAYS.
      ALLOCATE(HK(NCOL,NROW,NLAY))
      ALLOCATE(HKCC(NCOL,NROW,NLAY))
      ALLOCATE(VKAH(NCOL,NROW,NLAY))
      ALLOCATE(SC2HUF(NCOL,NROW))
      SC2HUF = 0.0
      IF(ITRSS.NE.0)THEN 
        ALLOCATE(SC1(NCOL,NROW,NLAY))
      ELSE
        ALLOCATE(SC1(1,1,1))
      ENDIF
      IF(NWETD .GT. 0)THEN
        ALLOCATE(WETDRY(NCOL,NROW,NWETD))
      ELSE
        ALLOCATE(WETDRY(1,1,1))
      ENDIF
      IF(NHUF .GT. 0)THEN
        ALLOCATE(IHGUFLG(5,NHUF))
        ALLOCATE(HUFTMP(NCOL,NROW,NHUF))
        ALLOCATE(HUFTHK(NCOL,NROW,NHUF,2))
      ELSE
        ALLOCATE(IHGUFLG(1,1))
        ALLOCATE(HUFTMP(1,1,1))
        ALLOCATE(HUFTHK(1,1,1,1))
      ENDIF

      IF(ILVDA .NE. 0)THEN
        ALLOCATE(VDHD(NCOL,NROW,NLAY))
        ALLOCATE(VDHT(NCOL,NROW,NLAY,3))
        ALLOCATE(A9(NCOL,NROW,NLAY,5))
      ELSE
        ALLOCATE(VDHD(1,1,1))
        ALLOCATE(VDHT(1,1,1,1))
        ALLOCATE(A9(1,1,1,1))
      ENDIF
      ALLOCATE(GS(NCOL,NROW))
C7------READ DATA FOR HYDROGEOLOGIC-UNIT FLOW PACKAGE 

C---Read rewetting information
      IWDFLG=0
      KLAYFLG=0
      DO 10 K=1,NLAY
        IF(LAYWT(K).NE.0) IWDFLG=IWDFLG+1
        IF(LTHUF(K).NE.0) KLAYFLG=1
   10 CONTINUE
      IF(IWDFLG.EQ.0) THEN
         WRITE(IOUT,111)
  111    FORMAT(1X,/,1X,'WETTING CAPABILITY IS NOT ACTIVE IN ANY LAYER')
      ELSE
         WRITE(IOUT,112) IWDFLG
  112    FORMAT(1X,/,1X,'WETTING CAPABILITY IS ACTIVE IN',I4,' LAYERS')
         IWDFLG=1
         READ(IN,*) WETFCT,IWETIT,IHDWET
         IF(IWETIT.LE.0) IWETIT=1
         WRITE(IOUT,*) ' WETTING FACTOR=',WETFCT
         WRITE(IOUT,*) ' WETTING ITERATION INTERVAL=',IWETIT
         WRITE(IOUT,*) ' IHDWET=',IHDWET
      END IF
C
C2H-----READ WETDRY CODES IF WETTING CAPABILITY HAS BEEN INVOKED
C2H-----(LAYWT NOT 0).
      DO 300 K=1,NLAY
      IF(LAYWT(K).NE.0) THEN
         CALL U2DREL(WETDRY(:,:,LAYWT(K)),ANAME(8),NROW,NCOL,K,IN,
     &            IOUT)
      END IF
  300 CONTINUE

      WRITE(IOUT,147)
  147 FORMAT(
     & //1X,'HUF7 -- HYDROGEOLOGIC-UNIT FLOW PACKAGE',
     & /1X,75('-'))
C
C-------READ HYDROGEOLOGIC-UNIT GEOMETRY
      Call SGWF2HUF7GEOMRP(IN)
C
C---Read HANI and VANI values for each named unit
      DO 100 NU=1,NHUF
        READ(IN,'(A)') LINE
C  Get the name of the new unit
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
C  Find the unit name in the list
        TMPNAM=LINE(ISTART:ISTOP)
        IF(TMPNAM.EQ.'ALL') THEN
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,HANITMP,IOUT,IN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,VANITMP,IOUT,IN)
          DO 150 NNU=1,NHUF
            HGUHANI(NNU)=HANITMP
            HGUVANI(NNU)=VANITMP
  150     CONTINUE
          GOTO 101
        ENDIF
        IU=0
        DO 200 NNU=1,NHUF
          CTMP1=HGUNAM(NNU)
          CALL UPCASE(CTMP1)
          IF(TMPNAM.EQ.CTMP1) THEN
            IU=NNU
            WRITE(IOUT,38) TMPNAM,IU
   38       FORMAT('UNIT ',A10,'CORRESPONDS TO UNIT NO. ',I5)
            GO TO 201
          END IF
  200   CONTINUE
  201   CONTINUE
        IF(IU.EQ.0) THEN
          WRITE(IOUT,41) TMPNAM
   41     FORMAT('UNIT ',A10,'NOT FOUND (STOP EXECUTION(GWF2HUF7RPGD))')
          CALL USTOP(' ')
        ENDIF
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,HGUHANI(IU),IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,HGUVANI(IU),IOUT,IN)
  100 CONTINUE
  101 CONTINUE


      WRITE(IOUT,48)
   48 FORMAT(
     & //3X,'INTERPRETATION OF UNIT FLAGS:',/1X,
     & '    UNIT      HANI           VK/VANI',
     & /1X,75('-'))
      DO 210 K=1,NHUF
        LAYPRN2=VKANAM(1)
        IF(HGUVANI(K).NE.0.0) LAYPRN2=VKANAM(2)
        IF(HGUHANI(K).LE.0.0) THEN
          WRITE(IOUT,79) HGUNAM(K),HANNAM,LAYPRN2
        ELSE
          WRITE(IOUT,80) HGUNAM(K),HGUHANI(K),LAYPRN2
        END IF
   79   FORMAT(1X,A10,2A14)
   80   FORMAT(1X,A10,G14.7,A14)
  210 CONTINUE

C
C-------READ NAMED PARAMETERS
      NPSS=0
      NPSY=0
      NPSYTP=0
      IF(NPHUF.GT.0) THEN
        DO 20 K=1,NPHUF
c          CALL UHUF7PARRP(IN,IOUT,NP,PTYP,ITERP,NHUF)
          CALL UHUF7PARRP(IN,IOUT,N,PTYP,1,NHUF)
          IF(PTYP.EQ.'HK') THEN
            CONTINUE
          ELSE IF(PTYP.EQ.'HANI') THEN
            CONTINUE
          ELSE IF(PTYP.EQ.'VK') THEN
            CONTINUE
          ELSE IF(PTYP.EQ.'VANI') THEN
            CONTINUE
          ELSE IF(PTYP.EQ.'SS') THEN
            NPSS=1
          ELSE IF(PTYP.EQ.'SY') THEN
            NPSY=1
          ELSE IF(PTYP.EQ.'SYTP') THEN
            NPSYTP=1
          ELSE
            WRITE(IOUT,*) ' Invalid parameter type for HUF Package'
            CALL USTOP(' ')
          END IF
   20   CONTINUE
      END IF
C
C  Make some basic parameter checks
      IF(ITRSS.NE.0.AND.NPSS.EQ.0.AND.NPSY.EQ.0) THEN
        WRITE(IOUT,500)
  500   FORMAT(//3X,
     &    'Simulation is transient and no storage parameters are ',
     &    'defined in the HUF Package (STOP GWF2HUF7RPGD)')
        CALL USTOP(' ')
      ENDIF
C
      IF(ITRSS.EQ.0.AND.(NPSS.NE.0.OR.NPSY.NE.0)) THEN
        WRITE(IOUT,510)
  510   FORMAT(//3X,
     &    'Simulation is steady state and storage parameters are',
     &    ' defined in the HUF Package (STOP GWF2HUF7RPGD)')
        CALL USTOP(' ')
      ENDIF
C
      IF(ITRSS.NE.0.AND.KLAYFLG.NE.0.AND.
     &       (NPSS.EQ.0.OR.NPSY.EQ.0)) THEN
        WRITE(IOUT,520)
  520   FORMAT(//3X,
     &    'Simulation is transient and has convertible ',
     &    'layers and only one storage parameter is defined in the HUF',
     &    ' Package (STOP GWF2HUF7RPGD)')
        CALL USTOP(' ')
      ENDIF
      IF(KLAYFLG.NE.0.AND.NPSYTP.GT.0) THEN
        WRITE(IOUT,530)
  530   FORMAT(//3X,
     &    'Simulation has SYTP parameter(s) defined but has a ',
     &    'convertible layer.  SYTP will be disregarded (GWF2HUF7RPGD)')
      ENDIF
      IF(ITRSS.EQ.0.AND.NPSYTP.NE.0) THEN
        WRITE(IOUT,540)
  540   FORMAT(//3X,
     &    'Simulation is steady state and SYTP parameter(s) are',
     &    ' defined in the HUF Package (STOP GWF2HUF7RPGD)')
        CALL USTOP(' ')
      ENDIF
C
C
C---Read PRINTCODE
      DO 390 NU=1,NHUF
        DO 390 I=1,5
          IHGUFLG(I,NU)=0
  390 CONTINUE
  399 CONTINUE
      READ(IN,'(A)',END=400) LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
      TMPNAM=LINE(ISTART:ISTOP)
      CALL UPCASE(TMPNAM)
      IF(TMPNAM.NE.'PRINT') GOTO 400
      WRITE(IOUT,'(/,A)') 'Reading PRINTCODE information'
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
C  Find the unit name in the list
      TMPNAM=LINE(ISTART:ISTOP)
      CALL UPCASE(TMPNAM)
      IF(TMPNAM.EQ.'ALL') THEN
        IU=-1
      ELSE
        IU=0
        DO 410 NU=1,NHUF
          CTMP1=HGUNAM(NU)
          CALL UPCASE(CTMP1)
          IF(TMPNAM.EQ.CTMP1) THEN
            IU=NU
            WRITE(IOUT,438) TMPNAM,IU
  438       FORMAT('UNIT ',A10,'CORRESPONDS TO UNIT NO. ',I5)
            GO TO 411
          END IF
  410   CONTINUE
      ENDIF
  411 CONTINUE
      IF(IU.EQ.0) THEN
        WRITE(IOUT,440) TMPNAM
  440   FORMAT('UNIT ',A10,'NOT FOUND (STOP EXECUTION)')
        CALL USTOP(' ')
      ENDIF
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICODE,R,IOUT,IN)
C---Reset flags
      DO 445 I=1,5
        IFLG(I)=0
  445 CONTINUE
  450 CONTINUE
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
      TMPNAM=LINE(ISTART:ISTOP)
      IF(TMPNAM.EQ.' ') GOTO 455
      IF(TMPNAM.EQ.'ALL') THEN
        IFLG(1)=ICODE
        IFLG(2)=ICODE
        IFLG(3)=ICODE
        IF(ITRSS.NE.0) THEN
          IFLG(4)=ICODE
          IFLG(5)=ICODE
        ENDIF
        GOTO 455
      ELSEIF(TMPNAM.EQ.'HK') THEN
        IFLG(1)=ICODE
      ELSEIF(TMPNAM.EQ.'HANI') THEN
        IFLG(2)=ICODE
      ELSEIF(TMPNAM.EQ.'VK') THEN
        IFLG(3)=ICODE
      ELSEIF(TMPNAM.EQ.'SS'.AND.ITRSS.NE.0)THEN
        IFLG(4)=ICODE
      ELSEIF(TMPNAM.EQ.'SY'.AND.ITRSS.NE.0) THEN
        IFLG(5)=ICODE
      ENDIF
      GOTO 450
  455 CONTINUE
      IF(IU.EQ.-1) THEN
        DO 460 NU=1,NHUF
          DO 465 I=1,5
            IHGUFLG(I,NU)=IFLG(I)
  465     CONTINUE
  460   CONTINUE
        GOTO 400
      ELSE
        DO 470 I=1,5
          IHGUFLG(I,IU)=IFLG(I)
  470   CONTINUE
        GOTO 399
      ENDIF
  400 CONTINUE
      WRITE(IOUT,442)
  442 FORMAT(//,'PRINTCODE FLAGS ARE SET AS FOLLOWS',/,
     &  '   UNIT       HK   HANI   VK    SS    SY',/,
     &  '------------------------------------------')
      DO 480 NU=1,NHUF
        WRITE(IOUT,444) HGUNAM(NU),(IHGUFLG(I,NU),I=1,5)
  444 FORMAT(A10,5I6)
  480 CONTINUE

C8------READ DATA FOR LVDA
      IF(ILVDA .NE. 0) CALL SGWF2HUF7LVDA1RPGD(ILVDA,IOUT,1,NHUF,
     &                       NPLVDA,NLAY,0)
C
C9------READ DATA FOR KDEP
      IF(IKDEP .NE. 0) CALL SGWF2HUF7KDEP1RPGD(IKDEP,IOUT,1,NPKDEP,
     &                       IFKDEP,NROW,NCOL,GS,BOTM(:,:,0),NHUF) 
C
C-------SUBSTITUTE AND PREPARE DATA FOR HYDROGEOLOGIC-UNIT FLOW PACKAGE

      CALL SGWF2HUF7SP(0,0,0,ILVDA)    
C
C-------RETURN
      CALL GWF2HUF7PSV(IGRID)
!  RGN DEFINE SECONDARY STORAGE COEFFICIENT FOR SFR2 AND UZF1
!  DETERMINE IF THERE IS A TRANSIENT STRESS PERIOD
      ISS = 1
      DO KPER = 1 , NPER
        IF ( ISSFLG(KPER).EQ.0 ) ISS = 0
      END DO
      IF(ISS.EQ.0) THEN
C   IF CONVERTIBLE LAYER GET PRIMARY STORAGE
        DO  I=1,NROW
          DO  J=1,NCOL
            K = 0
            KK = 1
            UPLAY: DO WHILE ( KK.LE.NLAY )
              IF( IBOUND(J,I,KK).GT.0 ) THEN
                K = KK
                EXIT UPLAY
              ELSE
                KK = KK + 1
              END IF
            END DO UPLAY
C            IF(LTHUF(K).NE.0) THEN
C              IF ( K.GT.0 ) THEN
            IF ( K.GT.0 ) THEN
              IF(LTHUF(K).NE.0) THEN
! TRICK SUBROUTINE TO THINK UPPERMOST LAYER IS UNCONFINED
                TOP=BOTM(J,I,LBOTM(K)-1)
                BOT=BOTM(J,I,LBOTM(K))
                HO=TOP-1.0E-1
                HN=TOP-1.0D-1
                CALL SGWF2HUF7SC2(0,J,I,K,TOP,BOT,HN,HO,1.0,CHCOF,
     &                      CRHS,HUFTHK,NCOL,NROW,NHUF,1.0,IOUT)
                SC2HUF(J,I) = CHCOF
              END IF
            END IF
          END DO
        END DO
      END IF  
      RETURN
      END

c======================================================================
      SUBROUTINE SGWF2HUF7GEOMRP(IN)
C
C     ******************************************************************
C     Read and prepare HYDROGEOLOGIC-UNIT GEOMETRY.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,IOUT
      USE GWFHUFMODULE,ONLY:NHUF,HUFTHK,HGUNAM
      CHARACTER*200 LINE
      CHARACTER*24 ANAME
C     ------------------------------------------------------------------
C
C-----Read the hydrogeologic-unit names and arrays
      DO 100 M=1,NHUF
C  Read a line describing a hydrogeologic unit
        READ (IN,'(A)') LINE
C  Get the name of the new unit
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
C  Add new unit name into list
        HGUNAM(M)=LINE(ISTART:ISTOP)
C  Read top elevation of unit
        ANAME=' TOP ELEVATN: '//HGUNAM(M)
        CALL U2DREL(HUFTHK(:,:,M,1),ANAME,NROW,NCOL,0,IN,IOUT)
C  Read thickness of unit
        ANAME='   THICKNESS: '//HGUNAM(M)
        CALL U2DREL(HUFTHK(:,:,M,2),ANAME,NROW,NCOL,0,IN,IOUT)
  100 CONTINUE
C
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7SP(KITER,KSTP,KPER,ILVDA)
C
C     ******************************************************************
C     SUBSTITUTE AND PREPARE DATA FOR HYDROGEOLOGIC-UNIT FLOW PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IBOUND,HNEW,CR,CC,CV,
     1                      BOTM,NBOTM,DELR,DELC,IOUT,ITRSS
C
c      USE PARAMMODULE, ONLY:RMLT,IZON,NMLTAR,NZONAR
      USE PARAMMODULE
      USE GWFBASMODULE,ONLY:HDRY
C
      USE GWFHUFMODULE,ONLY:NHUF,IWETIT,IHDWET,WETFCT,HK,VKAH,SC1,
     1                      WETDRY,HUFTHK,HKCC,HUFHK,HUFHANI,HUFVK,
     2                      HUFSS,IHGUFLG,HUFSY,VDHD,HUFTMP,VDHT,
     3                      HUFKDEP,GS,A9,HGUHANI,HGUVANI,HGUNAM  
      CHARACTER*4 PTYPE(6)
      CHARACTER*24 ANAME(6)
C
C
      DATA ANAME(1) /'   HYD. COND. ALONG ROWS'/
      DATA ANAME(2) /'  HORIZ. ANI. (COL./ROW)'/
      DATA ANAME(3) /'     VERTICAL HYD. COND.'/
      DATA ANAME(4) /'        SPECIFIC STORAGE'/
      DATA ANAME(5) /'          SPECIFIC YIELD'/
      DATA ANAME(6) /' HORIZ. TO VERTICAL ANI.'/
      DATA PTYPE(1) /'HK'/
      DATA PTYPE(2) /'HANI'/
      DATA PTYPE(3) /'VK'/
      DATA PTYPE(4) /'SS'/
      DATA PTYPE(5) /'SY'/
      DATA PTYPE(6) /'VANI'/

C     ------------------------------------------------------------------
C
C Check for cells that GO DRY/REWET
      DO 5 K=1,NLAY
        CALL SGWF2HUF7WETCHK(HNEW,IBOUND,CC,BOTM,
     &   NBOTM,K,KITER,KSTP,KPER,NCOL,NROW,NLAY,IOUT,WETDRY,
     &   WETFCT,IWETIT,IHDWET,HDRY)
    5 CONTINUE
C
C Zero out arrays
      DO 10 K=1,NLAY
        DO 20 I=1,NROW
          DO 30 J=1,NCOL
            HK(J,I,K)=0.
            HKCC(J,I,K)=0.
            VKAH(J,I,K)=0.
            IF(ITRSS.NE.0 .AND. KITER.EQ.0) SC1(J,I,K)=0.
   30     CONTINUE
   20   CONTINUE
   10 CONTINUE
C
C2------DEFINE DATA FOR NAMED PARAMETERS.
C
C Loop through rows and columns
      DO 100 I=1,NROW
      DO 100 J=1,NCOL
C Zero out arrays
        DO 110 NU=1,NHUF
          HUFHK(NU)=0.
          HUFVK(NU)=0.
          HUFHANI(NU)=0.
          HUFSS(NU)=0.
          HUFSY(NU)=0.
          HUFKDEP(NU)=0.
  110   CONTINUE
        IF(ILVDA .NE. 0)THEN
          DO 115 NK=1,NLAY
            VDHD(J,I,NK) = 0.0
  115     CONTINUE
        ENDIF

C
C---Populate HGU arrays for given i,j depending on parameter type
        CALL UHUF7POP(HUFHK,'HK  ',I,J,0,IOUT)
        CALL UHUF7POP(HUFHANI,'HANI',I,J,0,IOUT)
        CALL UHUF7POP(HUFVK,'VK  ',I,J,0,IOUT)
        CALL UHUF7POP(HUFVK,'VANI',I,J,0,IOUT)
        CALL UHUF7POP(HUFKDEP,'KDEP',I,J,0,IOUT)
        IF(ILVDA.NE.0) CALL UHUF7POPL(VDHD,NCOL,NROW,NLAY,I,J)
        IF(ITRSS.NE.0) THEN
          CALL UHUF7POP(HUFSS,'SS  ',I,J,0,IOUT)
          CALL UHUF7POP(HUFSY,'SY  ',I,J,0,IOUT)
        ENDIF
C
C---Populate HANI and VANI from input file if not already defined by
c     a parameter
        DO 120 NU=1,NHUF
          IF(HGUVANI(NU).GT.0..AND.HUFVK(NU).EQ.0.)
     &          HUFVK(NU)=HGUVANI(NU)
          IF(HGUHANI(NU).GT.0..AND.HUFHANI(NU).EQ.0.)
     &          HUFHANI(NU)=HGUHANI(NU)
  120   CONTINUE
C
C---Populate MODEL arrays
        DO 130 NU=1,NHUF
          TOPU=HUFTHK(J,I,NU,1)
          THCKU=HUFTHK(J,I,NU,2)
          IF(ABS(THCKU).LT.1E-4) GOTO 130
          BOTU=TOPU-THCKU
C-----Determine which layer(s) unit applies to
          IFLG=1
          CALL SGWF2HUF7HSRCH(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,
     &                        HNEW,IBOUND,KT,KB,IFLG)
C-----Skip unit if thickness is zero
          IF(IFLG.EQ.1) GOTO 130
C-----Populate HK and VKA arrays
          CALL SGWF2HUF7HK(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,KT,
     &                     KB,HK,HKCC,HUFHK,HUFHANI,HUFKDEP(NU),NHUF,NU,
     &                     HNEW,GS)
C
          CALL SGWF2HUF7VKA(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,
     &                      VKAH,HNEW,IBOUND,HUFHK,HUFVK,NHUF,NU,
     &                      HUFKDEP(NU),IOUT,GS)
          IF(ITRSS.NE.0 .AND. KITER.EQ.0 .AND. KPER.EQ.0 .AND.
     &       KSTP.EQ.0) THEN
            TOPU = HUFTHK(J,I,NU,1)
            BOTU = TOPU - THCKU
C-----Determine which layer(s) unit applies to, assuming for the sake of
C       computing SC1 that every layer is confined
            IFLG=0
            CALL SGWF2HUF7HSRCH(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,
     &                        HNEW,IBOUND,KT,KB,IFLG)
C-----Note that a check for IFLG=1 is not needed here, as such a
C        condition would have been caught after the earlier call
C        to subroutine SGWF2HUF7HSRCH
C-----Populate SC1 array
            CALL SGWF2HUF7SC1(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,
     &                        BOTU,SC1,HUFSS,KT,KB,NHUF,NU)
          ENDIF
  130   CONTINUE
  100 CONTINUE
C
C
C3------CHECK HUF DATA.
      CALL SGWF2HUF7N(HNEW,IBOUND,HK,VKAH,
     &       NCOL,NROW,NLAY,IOUT,WETDRY,BOTM,NBOTM)
C
C-------CALCULATE CV
      CALL SGWF2HUF7VCND(IBOUND,CV,VKAH,DELR,DELC,
     &       NCOL,NROW,NLAY)
C
      IF(ILVDA.EQ.0) THEN
C
C-------CALCULATE CR AND CC
        CALL SGWF2HUF7HCND(HNEW,IBOUND,CR,CC,HK,DELR,
     &       DELC,NCOL,NROW,NLAY,IOUT,BOTM,NBOTM,HKCC,
     &       HDRY,KITER,KSTP,KPER)

      ELSE
C-------Populate HK, HKCC, VDHD arrays
        CALL SGWF2HUF7VDHHV(
     &    IBOUND,HK,HKCC,VDHD,NCOL,NROW,NLAY,HUFTHK,NHUF,BOTM,NBOTM,
     &    HNEW,GS)
C-------Populate VDHT array
        CALL SGWF2HUF7VDHT(
     &    IBOUND,HK,HKCC,VDHD,VDHT,NCOL,NROW,NLAY,DELR,DELC)
C-------Populate A9 array
        CALL SGWF2HUF7VDA9(VDHT,A9,IBOUND,NLAY,NROW,NCOL)
      ENDIF
C
C-------CALCULATE SC1
      IF(ITRSS.NE.0 .AND. KITER.EQ.0 .AND. KPER.EQ.0 .AND.
     &   KSTP.EQ.0) THEN
C
C-------CHECK FOR SYTP PARAMETERS
        CALL SGWF2HUF7SYTP(SC1,IBOUND,NCOL,NROW,NLAY)
C
C-------MULTIPLY BY DELR*DELC TO GET SC1
        CALL SGWF2HUF7SC(IBOUND,SC1,DELR,DELC,NCOL,
     &                   NROW,NLAY)
      ENDIF
C
C-----Print HUF data
      IF(KITER.EQ.0) THEN
C-----Print unit arrays depending on flags
        DO 200 NU=1,NHUF
          DO 210 IP=1,5
            IF(IHGUFLG(IP,NU).GT.0) THEN
              IF(IP.EQ.3.AND.HGUVANI(NU).GT.0) THEN
                IA=6
              ELSE
                IA=IP
              ENDIF
C-----Populate HUFHK as a temporary holding array
              DO 220 J=1,NCOL
                DO 220 I=1,NROW
                  HUFHK(NU)=0
                  CALL UHUF7POP(HUFHK,PTYPE(IP),I,J,NU,IOUT)
                  IF(IA.EQ.6)
     &            CALL UHUF7POP(HUFHK,PTYPE(IA),I,J,NU,IOUT)
C-----Transfer to HUFTMP
                  HUFTMP(J,I,NU)=HUFHK(NU)
                  IF(IA.EQ.6.AND.HUFHK(NU).EQ.0.)
     &              HUFTMP(J,I,NU)=HGUVANI(NU)
                  IF(IA.EQ.2.AND.HGUHANI(NU).GT.0..AND.HUFHK(NU).EQ.0.)
     &              HUFTMP(J,I,NU)=HGUHANI(NU)
  220         CONTINUE
C-----Print HUFTMP
              WRITE(IOUT,305) ANAME(IA),HGUNAM(NU)
  305         FORMAT(//A24,' FOR UNIT ',A10)
              CALL ULAPRWC(HUFTMP(:,:,NU),NCOL,NROW,0,IOUT,
     &                     IHGUFLG(IP,NU),ANAME(IA))
            ENDIF
  210     CONTINUE
  200   CONTINUE
      ENDIF
C
C
C4------RETURN
      RETURN
      END

c======================================================================
      SUBROUTINE SGWF2HUF7HK(
     &  NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,KT,KB,HK,
     &  HKCC,HUFHK,HUFHANI,LAMBDA,NHUF,NU,HNEW,GS)
C
C     ******************************************************************
C     Substitute for HK parameters.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:LBOTM
      USE GWFHUFMODULE,ONLY:LTHUF

      DOUBLE PRECISION HNEW
      REAL LAMBDA,MULTKDEP
      DIMENSION HK(NCOL,NROW,NLAY),HKCC(NCOL,NROW,NLAY),
     &  HUFHK(NHUF),BOTM(NCOL,NROW,0:NBOTM),HUFHANI(NHUF),
     &  HNEW(NCOL,NROW,NLAY),GS(NCOL,NROW)
C     ------------------------------------------------------------------
C
      MULTKDEP = 1.0
      IF(KT.EQ.KB) THEN
        IF(LAMBDA.NE.0.) CALL SGWF2HUF7KDEP(LAMBDA,TOPU,BOTU,
     &                                      GS(J,I),MULTKDEP)
        HKCR=HUFHK(NU)*MULTKDEP*(TOPU-BOTU)
        HK(J,I,KT)=HK(J,I,KT)+HKCR
        HKCC(J,I,KT)=HKCC(J,I,KT)+HUFHANI(NU)*HKCR
      ELSE
        DO 300 KL=KT,KB
          BOTL = BOTM(J,I,LBOTM(KL))
          TOPL = BOTM(J,I,LBOTM(KL)-1)
C---Adjust top elevation for water-table layers
          IF(LTHUF(KL).NE.0.AND.HNEW(J,I,KL).LT.TOPL)
     &       TOPL=HNEW(J,I,KL)
          IF(KL.EQ.KT) THEN
            TOP1=TOPU
            BOT1=BOTL
C            THCK=TOPU-BOTL
          ELSEIF(KL.EQ.KB) THEN
            TOP1=TOPL
            BOT1=BOTU
C            THCK=TOPL-BOTU
          ELSE
            TOP1=TOPL
            BOT1=BOTL
C            THCK=TOPL-BOTL
          ENDIF
          THCK=TOP1-BOT1
C---Check for small or negative thickness
          IF(THCK.LT.1.E-4) GOTO 300
          IF(LAMBDA.NE.0.) CALL SGWF2HUF7KDEP(LAMBDA,TOP1,BOT1,
     &                                        GS(J,I),MULTKDEP)
          HKCR=HUFHK(NU)*MULTKDEP*THCK
          HK(J,I,KL)=HK(J,I,KL)+HKCR
          HKCC(J,I,KL)=HKCC(J,I,KL)+HUFHANI(NU)*HKCR
  300   CONTINUE
      ENDIF
C
C4------RETURN
      RETURN
      END

c======================================================================
      SUBROUTINE SGWF2HUF7VKA(
     & NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,VKAH,HNEW,IBOUND,HUFHK,
     & HUFVK,NHUF,NU,LAMBDA,IOUT,GS)
C
C     ******************************************************************
C     Substitute for VKA parameters.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:LBOTM
      USE GWFHUFMODULE,ONLY:LTHUF,HGUVANI
      DOUBLE PRECISION HNEW
      REAL TOP1,TOP2,TOP3,RMID1,RMID2,LAMBDA,MULTKDEP
      DIMENSION HNEW(NCOL,NROW,NLAY),BOTM(NCOL,NROW,0:NBOTM),
     &  VKAH(NCOL,NROW,NLAY),HUFHK(NHUF),HUFVK(NHUF),
     &  IBOUND(NCOL,NROW,NLAY),GS(NCOL,NROW)
C     ------------------------------------------------------------------

      CALL SGWF2HUF7VSRCH(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,
     &  HNEW,IBOUND,KT,KB,IFLG)

      IF(IFLG.EQ.1) RETURN

      IF(KB.EQ.NLAY) KB=KB-1
      DO 300 KL=KT,KB
        IF(IBOUND(J,I,KL).EQ.0 .OR. IBOUND(J,I,KL+1).EQ.0) GOTO 300
        TOP1=BOTM(J,I,LBOTM(KL)-1)
        TOP2=BOTM(J,I,LBOTM(KL))
        TOP3=BOTM(J,I,LBOTM(KL)+1)
C---Adjust top elevation for water-table layers
        IF(LTHUF(KL).NE.0.AND.HNEW(J,I,KL).LT.TOP1)
     &     TOP1=HNEW(J,I,KL)
        RMID1=(TOP1+TOP2)/2.
        RMID2=(TOP2+TOP3)/2.
C---If layer below is unconfined, it does not contribute
        IF(LTHUF(KL+1).NE.0.AND.HNEW(J,I,KL+1).LT.TOP2)
     &     RMID2=TOP2
        IF(RMID1.GT.TOPU) RMID1=TOPU
        IF(BOTU.GT.RMID2) RMID2=BOTU
        THCK=RMID1-RMID2
C---Check to see if negative thickness
        IF(THCK.LE.0.0) GOTO 300
        IF(HGUVANI(NU).EQ.0.) THEN
          IF(HUFVK(NU).EQ.0.) THEN
            WRITE(IOUT,500) I,J,NU
            CALL USTOP(' ')
          ENDIF
          VKAH(J,I,KL)=VKAH(J,I,KL)+THCK/HUFVK(NU)
        ELSE
          MULTKDEP = 1.0
          IF(LAMBDA.NE.0.) CALL SGWF2HUF7KDEP(LAMBDA,RMID1,RMID2,
     &                                      GS(J,I),MULTKDEP)
          IF(HUFHK(NU).EQ.0.) THEN
            WRITE(IOUT,510) I,J,NU
            CALL USTOP(' ')
          ELSEIF(MULTKDEP.EQ.0.) THEN
            WRITE(IOUT,520) I,J,NU
            CALL USTOP(' ')
          ENDIF
          VKAH(J,I,KL)=VKAH(J,I,KL)+THCK*HUFVK(NU)/(MULTKDEP*HUFHK(NU))
        ENDIF
  300 CONTINUE
C
  500 FORMAT(//,2X,
     & 'Vertical K is zero in row ',I3,' of column ',I3,' of unit ',
     & I3,' and vertical conductance cannot be calculated',
     & ' (STOP SGWF2HUF7VKA)')
  510 FORMAT(//,2X,
     & 'Horizontal K is zero in row ',I3,' of column ',I3,' of unit ',
     & I3,' and vertical conductance cannot be calculated',
     & ' (STOP SGWF2HUF7VKA)')
  520 FORMAT(//,2X,
     & 'MULTKDEP is zero in row ',I3,' of column ',I3,' of unit ',
     & I3,' and vertical conductance cannot be calculated',
     & ' (STOP SGWF2HUF7VKA)')
C
C4------RETURN
      RETURN
      END

c======================================================================
      SUBROUTINE SGWF2HUF7VKL(
     & VK,KL,NCOL,NROW,NLAY,BOTM,NBOTM,HNEW,IBOUND,IOUT,GS,HUFTHK,NHUF)
C
C     ******************************************************************
C     Calculate VK for a given layer
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:LBOTM
      USE GWFHUFMODULE,ONLY:HGUVANI,HUFHK,HUFVK,HUFKDEP
      DOUBLE PRECISION HNEW
      REAL MULTKDEP
      DIMENSION HNEW(NCOL,NROW,NLAY),BOTM(NCOL,NROW,0:NBOTM),
     &  VK(NCOL,NROW),
     &  IBOUND(NCOL,NROW,NLAY),GS(NCOL,NROW),HUFTHK(NCOL,NROW,NHUF,2)
C     ------------------------------------------------------------------

      DO 100 NU=1,NHUF
        DO 200 I=1,NROW
          DO 210 J=1,NCOL
            IF(IBOUND(J,I,KL).EQ.0) THEN
              VK(J,I)=0.0
              GOTO 210
            ENDIF
C           FIND TOP AND BOTTOM LAYERS THIS UNIT APPLIES TO
            TOPU=HUFTHK(J,I,NU,1)
            THCKU=HUFTHK(J,I,NU,2)
            IF(ABS(THCKU).LT.1E-4) GOTO 210
            BOTU=TOPU-THCKU
C-----------Determine which layer(s) unit applies to
            IFLG=1
            CALL SGWF2HUF7HSRCH(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,
     &                          BOTU,HNEW,IBOUND,KT,KB,IFLG)
            IF(IFLG.EQ.1) GOTO 210
            IF(KT.GT.KL) GOTO 210
            IF(KB.LT.KL) GOTO 210
C           IF IN THIS LAYER, CONTRIBUTE TO VK
C           GET THICKNESS OF UNIT IN THIS LAYER
            CALL UHUF7THK(BOTM(J,I,LBOTM(KL)-1),BOTM(J,I,LBOTM(KL)),
     &                        TOPU,THCKU,THCK,ATPU,ABTU)
            IF(THCK.EQ.0.0) GOTO 210
            HUFHK(NU)=0.0
            HUFVK(NU)=0.0
            HUFKDEP(NU)=0.0
            IF(HGUVANI(NU).EQ.0.) THEN
              CALL UHUF7POP(HUFVK,'VK  ',I,J,NU,IOUT)
              VK(J,I)=VK(J,I)+THCK/HUFVK(NU)
            ELSE
              MULTKDEP = 1.0
              CALL UHUF7POP(HUFHK,'HK  ',I,J,NU,IOUT)
              CALL UHUF7POP(HUFVK,'VANI',I,J,NU,IOUT)
              CALL UHUF7POP(HUFKDEP,'KDEP',I,J,NU,IOUT)
              IF(HUFKDEP(NU).NE.0.) CALL SGWF2HUF7KDEP(HUFKDEP(NU),TOPU,
     &                                                 BOTU,GS(J,I),
     &                                                 MULTKDEP)
              VK(J,I)=VK(J,I)+THCK*HUFVK(NU)/(MULTKDEP*HUFHK(NU))
            ENDIF
  210     CONTINUE
  200   CONTINUE
  100 CONTINUE
C     LOOP THROUGH ROWS
C       LOOP THROUGH COLUMNS
C         VK=THICK(LAYER)/VK
      DO 400 I=1,NROW
        DO 500 J=1,NCOL
          IF(IBOUND(J,I,KL).NE.0.0) VK(J,I)=(BOTM(J,I,LBOTM(KL)-1) -
     &                                     BOTM(J,I,LBOTM(KL)))/VK(J,I)
  500   CONTINUE
  400 CONTINUE
C       END COL LOOP
C     END ROW LOOP
C
C4------RETURN
      RETURN
      END

c======================================================================
      SUBROUTINE SGWF2HUF7VSRCH(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,
     &  HNEW,IBOUND,KT,KB,IFLG)
C
C     ******************************************************************
C     Search for top and bottom layer the unit applies to.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:LBOTM
      USE GWFHUFMODULE,ONLY:LTHUF
      DOUBLE PRECISION HNEW
      DIMENSION HNEW(NCOL,NROW,NLAY),BOTM(NCOL,NROW,0:NBOTM),
     &  IBOUND(NCOL,NROW,NLAY)
C
C Reset IFLG
      IFLG=1
C
C Loop through layers to determine where unit applies
C
C First, search for top
      DO 100 KT=1,NLAY-1
        IF(IBOUND(J,I,KT).EQ.0.OR.IBOUND(J,I,KT+1).EQ.0) GOTO 100
        TOP1=BOTM(J,I,LBOTM(KT)-1)
C---Adjust top of model layer for unconfined layer
        IF(LTHUF(KT).NE.0.AND.HNEW(J,I,KT).LT.TOP1) TOP1=HNEW(J,I,KT)
        TOP2=BOTM(J,I,LBOTM(KT))
        TOP3=BOTM(J,I,LBOTM(KT)+1)
        RMID1=(TOP1+TOP2)/2.
        RMID2=(TOP2+TOP3)/2.
C---If unit bottom is above the middle of the top layer, return
C        IF(KT.EQ.1.AND.TOPU.GT.RMID1.AND.BOTU.GE.RMID1) THEN
        IF(TOPU.GT.RMID1.AND.BOTU.GE.RMID1) THEN
          RETURN
        ENDIF
C---If unit top is above middle of top layer, exit loop
C        IF(KT.EQ.1.AND.TOPU.GT.RMID1.AND.BOTU.LT.RMID1) GOTO 110
        IF(TOPU.GT.RMID1.AND.BOTU.LT.RMID1) GOTO 110
C---If unit top is between the middle of this layer and the middle of the
C     next layer, exit loop
        IF(TOPU.LE.RMID1.AND.TOPU.GE.RMID2) GOTO 110
  100 CONTINUE
      RETURN
  110 CONTINUE
C Now search for bottom
      DO 200 KKB=KT,NLAY-1
        TOP1=BOTM(J,I,LBOTM(KKB)-1)
C---Adjust top of model layer for unconfined layer
        IF(LTHUF(KKB).NE.0.AND.HNEW(J,I,KKB).LT.TOP1) TOP1=HNEW(J,I,KKB)
        TOP2=BOTM(J,I,LBOTM(KKB))
        TOP3=BOTM(J,I,LBOTM(KKB)+1)
        RMID1=(TOP1+TOP2)/2.
        RMID2=(TOP2+TOP3)/2.
C---If unit bottom is between the middle of this layer and the middle of
C     the next layer, return
        IF(BOTU.LE.RMID1.AND.BOTU.GE.RMID2) THEN
          KB=KKB
          IFLG=0
          RETURN
        ENDIF
C---If entire unit is below the middle of the bottom layer, return
        IF(KKB.EQ.NLAY-1.AND.TOPU.LE.RMID2.AND.BOTU.LT.RMID2) THEN
          RETURN
        ENDIF
C---If unit bottom is below the middle of the bottom layer, return
        IF(KKB.EQ.NLAY-1.AND.BOTU.LT.RMID2) THEN
          KB=NLAY
          IFLG=0
          RETURN
        ENDIF
  200 CONTINUE
C
C4------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7HSRCH(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,
     &  HNEW,IBOUND,KT,KB,IFLG)
C
C     ******************************************************************
C     Search for top and bottom layer the unit applies to.
C     Values for IFLG on input:
C       IFLG = 0, Do not adjust top of unconfined layer to HNEW
C       IFLG <> 0, Adjust top of unconfined layer to HNEW
C     Values for IFLG on output:
C       IFLG = 0, Unit successfully found
C       IFLG = 1, Unit not found
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:LBOTM
      USE GWFHUFMODULE,ONLY:LTHUF
      DOUBLE PRECISION HNEW
      DIMENSION HNEW(NCOL,NROW,NLAY),BOTM(NCOL,NROW,0:NBOTM),
     &  IBOUND(NCOL,NROW,NLAY)
C
C Reset IFLG
      IHNEW=IFLG
      IFLG=1
C
C Loop through layers to determine where unit applies
C
C First, search for top
      DO 100 KT=1,NLAY
        IF(IBOUND(J,I,KT).EQ.0) GOTO 100
        TOP=BOTM(J,I,LBOTM(KT)-1)
C---Adjust top of model layer for unconfined layer (if IFLG<>0 on input)
        IF(IHNEW.NE.0.AND.LTHUF(KT).NE.0.AND.HNEW(J,I,KT).LT.TOP)
     &    TOP=HNEW(J,I,KT)
C---If unit top is in this layer, exit loop
        IF(TOPU.LE.TOP.AND.TOPU.GT.BOTM(J,I,LBOTM(KT))) GOTO 110
C---If unit top is above model top, adjust unit top elevation, exit loop
        IF(TOPU.GT.TOP) THEN
          TOPU=TOP
          GOTO 110
        ENDIF
  100 CONTINUE
      RETURN
  110 CONTINUE
C---If unit top has been adjusted to below unit bottom, return
      IF(TOPU.LE.BOTU) THEN
        RETURN
      ENDIF
C
C Now search for bottom
      DO 200 KKB=KT,NLAY
        IF(IBOUND(J,I,KKB).EQ.0) GOTO 200
        TOP=BOTM(J,I,LBOTM(KKB)-1)
C---Adjust top of model layer for unconfined layer (if IFLG<>0 on input)
        IF(IHNEW.NE.0.AND.LTHUF(KKB).NE.0.AND.HNEW(J,I,KKB).LT.TOP)
     &    TOP=HNEW(J,I,KKB)
C---If unit bottom is in this layer, set KB=KKB, return
        IF(BOTU.LE.TOP.AND.BOTU.GE.BOTM(J,I,LBOTM(KKB))) THEN
          KB=KKB
          IFLG=0
          RETURN
        ENDIF
C---If top of model layer has been adjusted for unconfined unit and
C    unit bottom is in the gap, set KB=KKB-1, return
        IF(BOTU.GT.TOP) THEN
          KB=KKB-1
          BOTU=BOTM(J,I,LBOTM(KKB)-1)
          IFLG=0
          RETURN
        ENDIF
C---If unit bottom is below model bottom, set KB=NLAY, return
        IF(KKB.EQ.NLAY.AND.BOTU.LT.BOTM(J,I,LBOTM(KKB))) THEN
          KB=KKB
          BOTU=BOTM(J,I,LBOTM(KKB))
          IFLG=0
          RETURN
        ENDIF
  200 CONTINUE
C---If KB has not been found due to inactive cells, search again from bottom
      DO 300 KKB=NLAY,1,-1
        IF(IBOUND(J,I,KKB).EQ.0) GOTO 300
        KB = KKB
        IFLG = 0
        BOTU = BOTM(J,I,LBOTM(KKB))
        RETURN
  300 CONTINUE
C
C4------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7SC1(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,
     &                        SC1,HUFSS,KT,KB,NHUF,NU)
C
C     ******************************************************************
C     Substitute for SS parameters.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:LBOTM
      DIMENSION BOTM(NCOL,NROW,0:NBOTM),HUFSS(NHUF),
     &  SC1(NCOL,NROW,NLAY)
C     ------------------------------------------------------------------

      IF(KT.EQ.KB) THEN
        SC1(J,I,KT)=SC1(J,I,KT)+HUFSS(NU)*(TOPU-BOTU)
      ELSE
        DO 300 KL=KT,KB
          IF(KL.EQ.KT) THEN
            THCK=TOPU-BOTM(J,I,LBOTM(KL))
          ELSEIF(KL.EQ.KB) THEN
            THCK=BOTM(J,I,LBOTM(KL)-1)-BOTU
          ELSE
            THCK=BOTM(J,I,LBOTM(KL)-1)-BOTM(J,I,LBOTM(KL))
          ENDIF
          IF(ABS(THCK).LT.1.E-4) GOTO 300
          SC1(J,I,KL)=SC1(J,I,KL)+HUFSS(NU)*THCK
  300   CONTINUE
      ENDIF
C
C4------RETURN
      RETURN
      END

c======================================================================
      SUBROUTINE SGWF2HUF7N(HNEW,IBOUND,HK,VKAH,NCOL,
     &  NROW,NLAY,IOUT,WETDRY,BOTM,NBOTM)
C
C     ******************************************************************
C     INITIALIZE AND CHECK HUF DATA
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:LBOTM
      USE GWFBASMODULE,ONLY:HNOFLO
      USE GWFHUFMODULE,ONLY:LAYWT
C
      DOUBLE PRECISION HNEW
C
      DIMENSION HNEW(NCOL,NROW,NLAY),IBOUND(NCOL,NROW,NLAY),
     &    HK(NCOL,NROW,NLAY),VKAH(NCOL,NROW,NLAY),
     &    WETDRY(NCOL,NROW,NLAY),BOTM(NCOL,NROW,0:NBOTM)
C
C     ------------------------------------------------------------------
      ZERO=0.
      HCNV=HNOFLO
      IZFLG=0
C
C-------CONVERT CELL TO NO FLOW IF CELL THICKNESS IS 0.
      DO 30 K=1,NLAY
      KB=LBOTM(K)
      KT=KB-1
      DO 30 I=1,NROW
      DO 30 J=1,NCOL
      IF(IBOUND(J,I,K).GT.0) THEN
         THICK=BOTM(J,I,KT)-BOTM(J,I,KB)
         IF(THICK.LE.ZERO) THEN
            IBOUND(J,I,K)=0
            HNEW(J,I,K)=HCNV
            IF(LAYWT(K).NE.0) WETDRY(J,I,LAYWT(K))=ZERO
            WRITE(IOUT,25) K,I,J
   25       FORMAT(1X,'WARNING: Converting cell to no flow due to 0',
     &      ' thickness (Layer,row,col)',I4,',',I4,',',I4,/,1X,
     &      'Head at this cell set to HNOFLO. To avoid warning, set',
     &      ' IBOUND=0 or thickness>0.')
         END IF
      END IF
   30 CONTINUE
C
C1------INSURE THAT EACH ACTIVE CELL HAS AT LEAST ONE NON-ZERO
C1------TRANSMISSIVE PARAMETER.
      DO 60 K=1,NLAY
      IF(LAYWT(K).NE.0) THEN
C
C2------WETTING IS ACTIVE.
         DO 40 I=1,NROW
         DO 40 J=1,NCOL
         IF(IBOUND(J,I,K).EQ.0) GO TO 40
C
C2A-----CHECK HORIZONTAL HYDRAULIC CONDUCTIVITY (HK).
         IF(HK(J,I,K).NE.ZERO) GO TO 40
C
C2C-----ALL TRANSMISSIVE TERMS ARE ALL 0, SO CONVERT CELL TO NO FLOW.
         IBOUND(J,I,K)=0
         HNEW(J,I,K)=HCNV
         WETDRY(J,I,LAYWT(K))=ZERO
         WRITE(IOUT,43) K,I,J
   40    CONTINUE
C
      ELSE
C
C3------WETTING IS INACTIVE
         DO 50 I=1,NROW
         DO 50 J=1,NCOL
         IF(IBOUND(J,I,K).EQ.0) GO TO 50
C
C3A-----CHECK HORIZONTAL HYDRAULIC CONDUCTIVITY (HK).
         IF(HK(J,I,K).NE.ZERO) GO TO 50
C
C3B-----CHECK VERTICAL HYDRAULIC CONDUCTIVITY AND CONFINING BED
C3B-----VERTICAL HYDRAULIC CONDUCTIVITY.
         IF(NLAY.GT.1) THEN
            IF(VKAH(J,I,K).NE.ZERO) GOTO 50
         END IF
C
C3C-----ALL TRANSMISSIVE TERMS ARE 0, SO CONVERT CELL TO NO FLOW.
         IZFLG=1
         IBOUND(J,I,K)=0
         HNEW(J,I,K)=HCNV
         WRITE(IOUT,43) K,I,J
   43    FORMAT(1X,'NODE (LAYER,ROW,COL)',3I4,
     & ' ELIMINATED BECAUSE ALL HYDRAULIC CONDUCTIVITIES TO NODE ARE 0')
   50    CONTINUE
      END IF
   60 CONTINUE
      IF(IZFLG.EQ.1) WRITE(IOUT,500)
  500 FORMAT(/,1X
     & ,'Nodes are often eliminated for the following reasons:',//
     & ,'1) Hydrogeologic units are not continuous throughout the '
     & ,'model domain and gaps are present, or',/
     & ,'2) The parameters do not define the hydraulic properties '
     & ,'for the entire active model domain.',//
     & ,'Check the HGU and parameter definition to make sure there '
     & ,'are not problems.'
     & ,//)
C
C
C7------RETURN.
      RETURN
      END

c======================================================================
      SUBROUTINE SGWF2HUF7HCND(HNEW,IBOUND,CR,CC,HK,DELR,DELC,
     &  NCOL,NROW,NLAY,IOUT,BOTM,NBOTM,HKCC,HDRY,
     &  KITER,KSTP,KPER)
C
C     ******************************************************************
C     CALCULATE HORIZONTAL CONDUCTANCES
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:LBOTM
      USE GWFHUFMODULE,ONLY:LTHUF
C
      DOUBLE PRECISION HNEW
C
      DIMENSION HNEW(NCOL,NROW,NLAY),IBOUND(NCOL,NROW,NLAY),
     &    CR(NCOL,NROW,NLAY),CC(NCOL,NROW,NLAY),
     &    HK(NCOL,NROW,NLAY), DELR(NCOL),DELC(NROW),
     &    BOTM(NCOL,NROW,0:NBOTM),HKCC(NCOL,NROW,NLAY)
C
C     ------------------------------------------------------------------
C
C4------CALCULATE HOR. CONDUCTANCE(CR AND CC) FOR CONSTANT T LAYERS.
      DO 70 K=1,NLAY
C-------CONVERT HK TO HYDRAULIC CONDUCTIVITY AND SC1 TO SPECIFIC STORAGE
        DO 71 I=1,NROW
        DO 71 J=1,NCOL
          IF(IBOUND(J,I,K).NE.0) THEN
              TOP=BOTM(J,I,LBOTM(K)-1)
              BOT=BOTM(J,I,LBOTM(K))
              IF(LTHUF(K).NE.0.AND.HNEW(J,I,K).LT.TOP)
     &            TOP=HNEW(J,I,K)
              HK(J,I,K)=HK(J,I,K)/(TOP-BOT)
              HKCC(J,I,K)=HKCC(J,I,K)/(TOP-BOT)
          ENDIF
   71   CONTINUE
      KK=K
        CALL SGWF2HUF7HCOND(HNEW,IBOUND,CR,CC,HK,DELR,DELC,BOTM,
     &      NBOTM,KK,KITER,KSTP,KPER,NCOL,NROW,NLAY,IOUT,
     &      HDRY,HKCC)
   70 CONTINUE
C
C7------RETURN.
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7VCND(IBOUND,CV,VKAH,DELR,DELC,
     &  NCOL,NROW,NLAY)
C
C     ******************************************************************
C     CALCULATE VERTICAL CONDUCTANCES
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
C
C
      DIMENSION IBOUND(NCOL,NROW,NLAY),
     &    CV(NCOL,NROW,NLAY),VKAH(NCOL,NROW,NLAY),
     &    DELR(NCOL),DELC(NROW)
C     ------------------------------------------------------------------
C
C5------CALCULATE VERTICAL CONDUCTANCE
      IF(NLAY.GT.1) THEN
        DO 10 K=1,NLAY-1
        DO 11 I=1,NROW
        DO 11 J=1,NCOL
          IF(IBOUND(J,I,K).NE.0.AND.VKAH(J,I,K).GT.0)  THEN
            CV(J,I,K)=DELR(J)*DELC(I)/VKAH(J,I,K)
          ELSE
            CV(J,I,K)=0.
          ENDIF
   11    CONTINUE
   10    CONTINUE
      END IF
C
C7------RETURN.
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7SYTP(SC1,IBOUND,NCOL,NROW,NLAY)
C
C     ******************************************************************
C     REPLACE SC1 WITH SY FOR TOPMOST ACTIVE CELL FOR ANY I,J
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE PARAMMODULE
      USE GWFHUFMODULE,ONLY:LTHUF
C
      DIMENSION IBOUND(NCOL,NROW,NLAY),
     &    SC1(NCOL,NROW,NLAY)
C     ------------------------------------------------------------------
C
C
C Loop through parameters to determine if there are SYTP parameter(s)
      IPFLG=0
      DO 10 NP=1,MXPAR
        IF(PARTYP(NP).EQ.'SYTP') IPFLG=1
   10 CONTINUE
      DO 15 K=1,NLAY
        IF(LTHUF(K).NE.0) IPFLG=0
   15 CONTINUE
      IF(IPFLG.EQ.0) RETURN
C Loop through and zero out SC1 in appropriate spots to prepare for
C   additive parameters
      DO 20 I=1,NROW
      DO 30 J=1,NCOL
        DO 40 K=1,NLAY
          IF(IBOUND(J,I,K).NE.0) THEN
            SC1(J,I,K)=0.0
            GOTO 30
          ENDIF
   40   CONTINUE
   30 CONTINUE
   20 CONTINUE
      DO 100 NP=1,MXPAR
        IF(PARTYP(NP).EQ.'SYTP') THEN
C Loop through layers that apply to this parameter
          DO 200 ND=IPLOC(1,NP),IPLOC(2,NP)
            NM=IPCLST(2,ND)
            NZ=IPCLST(3,ND)
C
            DO 210 I=1,NROW
            DO 220 J=1,NCOL
              CALL UHUF7RMLT(RMLT0,J,I,NZ,NM,ND)
              IF(RMLT0.EQ.0.0) GOTO 220
C         SEARCH DOWN TO FIND HIGHEST ACTIVE CELL
              DO 250 K=1,NLAY
                IF(IBOUND(J,I,K).NE.0) THEN
                  SC1(J,I,K)=SC1(J,I,K)+RMLT0*B(NP)
                  GOTO 220
                ENDIF
  250         CONTINUE
  220       CONTINUE
  210       CONTINUE
  200     CONTINUE
        ENDIF
  100 CONTINUE
C
C7------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7SC(IBOUND,SC1,DELR,DELC,NCOL,NROW,NLAY)
C
C     ******************************************************************
C     CALCULATE SC1
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
C
C
      DIMENSION IBOUND(NCOL,NROW,NLAY),
     &    SC1(NCOL,NROW,NLAY),DELR(NCOL),DELC(NROW)
C     ------------------------------------------------------------------
C
      DO 70 K=1,NLAY
C-------CONVERT SC1 TO SPECIFIC STORAGE
      DO 70 I=1,NROW
      DO 70 J=1,NCOL
        IF(IBOUND(J,I,K).NE.0) SC1(J,I,K)=SC1(J,I,K)*DELR(J)*DELC(I)
   70 CONTINUE
C
C7------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7HCOND(HNEW,IBOUND,CR,CC,HK,DELR,DELC,BOTM,
     & NBOTM,K,KITER,KSTP,KPER,NCOL,NROW,NLAY,IOUT,
     & HDRY,HKCC)
C     ******************************************************************
C     COMPUTE HORIZONTAL BRANCH CONDUCTANCE FOR ONE LAYER.
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:LBOTM
      USE GWFHUFMODULE,ONLY:LTHUF,LAYWT
      DOUBLE PRECISION HNEW,HHD,BBOT,TTOP
C
      DIMENSION HNEW(NCOL,NROW,NLAY),IBOUND(NCOL,NROW,NLAY),
     & CR(NCOL,NROW,NLAY), CC(NCOL,NROW,NLAY), HK(NCOL,NROW,NLAY),
     & DELR(NCOL), DELC(NROW),
     & BOTM(NCOL,NROW,0:NBOTM),
     & HKCC(NCOL,NROW,NLAY)
      CHARACTER*3 ACNVRT
      DIMENSION ICNVRT(5),JCNVRT(5),ACNVRT(5)
C     ------------------------------------------------------------------
C1------INITIALIZE DATA.
      ZERO=0.
      NCNVRT=0
      IHDCNV=0
C
C3------LOOP THROUGH EACH CELL, AND CALCULATE SATURATED THICKNESS.
      DO 200 I=1,NROW
      DO 200 J=1,NCOL
C
C3A-----SET SATURATED THICKNESS=0. FOR DRY CELLS.
      IF(IBOUND(J,I,K).EQ.0) THEN
         CC(J,I,K)=ZERO
      ELSE
C
C3B-----CALCULATE SATURATED THICKNESS FOR A WET CELL.
         BBOT=BOTM(J,I,LBOTM(K))
         TTOP=BOTM(J,I,LBOTM(K)-1)
         IF(LTHUF(K).NE.0) THEN
            HHD=HNEW(J,I,K)
            IF(HHD.LT.TTOP) TTOP=HHD
         END IF
         THCK=TTOP-BBOT
         CC(J,I,K)=THCK
C
C
C3C-----WHEN SATURATED THICKNESS <= 0, PRINT A MESSAGE AND SET
C3C-----HNEW=HDRY, SATURATED THICKNESS=0.0, AND IBOUND=0.
         IF(THCK.LE.ZERO) THEN
            CALL SGWF2HUF7WDMSG(1,NCNVRT,ICNVRT,JCNVRT,ACNVRT,IHDCNV,
     &             IOUT,KITER,J,I,K,KSTP,KPER)
            HNEW(J,I,K)=HDRY
            CC(J,I,K)=ZERO
            IF(IBOUND(J,I,K).LT.0) THEN
               WRITE(IOUT,151)
  151          FORMAT(1X,/1X,'CONSTANT-HEAD CELL WENT DRY',
     &          ' -- SIMULATION ABORTED')
                    WRITE(IOUT,*) TTOP, BBOT, THCK
               WRITE(IOUT,152) K,I,J,KITER,KSTP,KPER
  152          FORMAT(1X,'LAYER=',I2,'   ROW=',I3,'   COLUMN=',I3,
     &    '   ITERATION=',I3,'   TIME STEP=',I3,'   STRESS PERIOD=',I3)
               CALL USTOP(' ')
            END IF
            IBOUND(J,I,K)=0
         END IF
      END IF
  200 CONTINUE
C
C4------PRINT ANY REMAINING CELL CONVERSIONS NOT YET PRINTED.
      CALL SGWF2HUF7WDMSG(0,NCNVRT,ICNVRT,JCNVRT,ACNVRT,IHDCNV,
     &             IOUT,KITER,J,I,K,KSTP,KPER)
C
C5------CHANGE IBOUND VALUE FOR CELLS THAT CONVERTED TO WET THIS
C5------ITERATION FROM 30000 to 1.
      IF(LAYWT(K).NE.0) THEN
         DO 205 I=1,NROW
         DO 205 J=1,NCOL
         IF(IBOUND(J,I,K).EQ.30000) IBOUND(J,I,K)=1
  205    CONTINUE
      END IF
C
C6------COMPUTE HORIZONTAL BRANCH CONDUCTANCES FROM CELL HYDRAULIC
C6------CONDUCTIVITY, SATURATED THICKNESS, AND GRID DIMENSIONS.
         CALL SGWF2HUF7HHARM(CR,CC,HK,IBOUND,DELR,DELC,K,NCOL,NROW,
     &         NLAY,HKCC)
C
C7------RETURN.
      RETURN
      END

c======================================================================
      SUBROUTINE SGWF2HUF7WETCHK(HNEW,IBOUND,CC,BOTM,
     & NBOTM,K,KITER,KSTP,KPER,NCOL,NROW,NLAY,IOUT,WETDRY,
     & WETFCT,IWETIT,IHDWET,HDRY)
C     ******************************************************************
C     CHECK FOR CELLS THAT GO DRY/REWET
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:LBOTM
      USE GWFHUFMODULE,ONLY:LTHUF,LAYWT
      DOUBLE PRECISION HNEW,HHD,BBOT,TTOP
C
      DIMENSION HNEW(NCOL,NROW,NLAY),IBOUND(NCOL,NROW,NLAY),
     & CC(NCOL,NROW,NLAY),
     & BOTM(NCOL,NROW,0:NBOTM),WETDRY(NCOL,NROW,NLAY)
      CHARACTER*3 ACNVRT
      DIMENSION ICNVRT(5),JCNVRT(5),ACNVRT(5)
C     ------------------------------------------------------------------
C1------INITIALIZE DATA.
      ZERO=0.
      NCNVRT=0
      IHDCNV=0
C
C2------IF LAYER IS WETTABLE CONVERT DRY CELLS TO WET WHEN APPROPRIATE.
      ITFLG=1
      IF(LAYWT(K).NE.0) ITFLG=MOD(KITER,IWETIT)
      IF(ITFLG.EQ.0) CALL SGWF2HUF7WET(HNEW,IBOUND,BOTM,NBOTM,K,KITER,
     &      KSTP,KPER,NCOL,NROW,NLAY,IOUT,WETDRY,WETFCT,IHDWET,
     &      IHDCNV,NCNVRT,ICNVRT,JCNVRT,ACNVRT)
C
C3------LOOP THROUGH EACH CELL, AND CALCULATE SATURATED THICKNESS.
      DO 200 I=1,NROW
      DO 200 J=1,NCOL
C
C3A-----SET SATURATED THICKNESS=0. FOR DRY CELLS.
      IF(IBOUND(J,I,K).EQ.0) THEN
         CC(J,I,K)=ZERO
      ELSE
C
C3B-----CALCULATE SATURATED THICKNESS FOR A WET CELL.
         BBOT=BOTM(J,I,LBOTM(K))
         TTOP=BOTM(J,I,LBOTM(K)-1)
         IF(LTHUF(K).NE.0) THEN
            HHD=HNEW(J,I,K)
            IF(HHD.LT.TTOP) TTOP=HHD
         END IF
         THCK=TTOP-BBOT
         CC(J,I,K)=THCK
C
C
C3C-----WHEN SATURATED THICKNESS <= 0, PRINT A MESSAGE AND SET
C3C-----HNEW=HDRY, SATURATED THICKNESS=0.0, AND IBOUND=0.
         IF(THCK.LE.ZERO) THEN
            CALL SGWF2HUF7WDMSG(1,NCNVRT,ICNVRT,JCNVRT,ACNVRT,IHDCNV,
     &             IOUT,KITER,J,I,K,KSTP,KPER)
            HNEW(J,I,K)=HDRY
            CC(J,I,K)=ZERO
            IF(IBOUND(J,I,K).LT.0) THEN
               WRITE(IOUT,151)
  151          FORMAT(1X,/1X,'CONSTANT-HEAD CELL WENT DRY',
     &          ' -- SIMULATION ABORTED')
                    WRITE(IOUT,*) TTOP, BBOT, THCK
               WRITE(IOUT,152) K,I,J,KITER,KSTP,KPER
  152          FORMAT(1X,'LAYER=',I2,'   ROW=',I3,'   COLUMN=',I3,
     &    '   ITERATION=',I3,'   TIME STEP=',I3,'   STRESS PERIOD=',I3)
               CALL USTOP(' ')
            END IF
            IBOUND(J,I,K)=0
         END IF
      END IF
  200 CONTINUE
C
C4------PRINT ANY REMAINING CELL CONVERSIONS NOT YET PRINTED.
      CALL SGWF2HUF7WDMSG(0,NCNVRT,ICNVRT,JCNVRT,ACNVRT,IHDCNV,
     &             IOUT,KITER,J,I,K,KSTP,KPER)
C
C5------CHANGE IBOUND VALUE FOR CELLS THAT CONVERTED TO WET THIS
C5------ITERATION FROM 30000 to 1.
      IF(LAYWT(K).NE.0) THEN
         DO 205 I=1,NROW
         DO 205 J=1,NCOL
         IF(IBOUND(J,I,K).EQ.30000) IBOUND(J,I,K)=1
  205    CONTINUE
      END IF
C
C7------RETURN.
      RETURN
      END

c======================================================================
      SUBROUTINE GWF2HUF7AD(KPER,IGRID)
C
C%%%%%
C  PLEASE NOTE: THIS SUBROUTINE WAS COPIED DIRECTLY FROM THE LPF
C PACKAGE.  THERE IS NOTHING SPECIFIC TO THE HUF PACKAGE.
C%%%%%
C     ******************************************************************
C     SET HOLD TO BOTM WHENEVER A WETTABLE CELL IS DRY
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,ISSFLG,IBOUND,HOLD,BOTM,LBOTM
      USE GWFHUFMODULE,ONLY:LAYWT,WETDRY
C     ------------------------------------------------------------------
C
      CALL SGWF2HUF7PNT(IGRID)
      ISS=ISSFLG(KPER)
C
C1------RETURN IF STEADY STATE.
      IF(ISS.NE.0) RETURN
C
C2------LOOP THROUGH ALL LAYERS TO SET HOLD=BOT IF A WETTABLE CELL IS DRY
      ZERO=0.
      DO 100 K=1,NLAY
C
C2A-----SKIP LAYERS THAT CANNOT CONVERT BETWEEN WET AND DRY
      IF(LAYWT(K).EQ.0) GO TO 100
      DO 90 I=1,NROW
      DO 90 J=1,NCOL
C
C2B-----SKIP CELLS THAT ARE CURRENTLY WET OR ARE NOT WETTABLE
      IF(IBOUND(J,I,K).NE.0) GO TO 90
      IF(WETDRY(J,I,LAYWT(K)).EQ.ZERO) GO TO 90
C
C2C-----SET HOLD=BOT
      HOLD(J,I,K)=BOTM(J,I,LBOTM(K))
   90 CONTINUE
  100 CONTINUE
C
C3-----RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7WET(HNEW,IBOUND,BOTM,NBOTM,K,KITER,KSTP,KPER,
     &      NCOL,NROW,NLAY,IOUT,WETDRY,WETFCT,IHDWET,IHDCNV,
     &      NCNVRT,ICNVRT,JCNVRT,ACNVRT)
C
C%%%%%
C  PLEASE NOTE: THIS SUBROUTINE WAS COPIED DIRECTLY FROM THE LPF
C PACKAGE.  THERE IS NOTHING SPECIFIC TO THE HUF PACKAGE.
C%%%%%
C     ******************************************************************
C     CONVERT DRY CELLS TO WET.
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:LBOTM
      USE GWFHUFMODULE,ONLY:LAYWT
C
      DOUBLE PRECISION HNEW
C
      DIMENSION HNEW(NCOL,NROW,NLAY),IBOUND(NCOL,NROW,NLAY),
     &          BOTM(NCOL,NROW,0:NBOTM),WETDRY(NCOL,NROW,NLAY)
      CHARACTER*3 ACNVRT
      DIMENSION ICNVRT(5),JCNVRT(5),ACNVRT(5)
C     ------------------------------------------------------------------
      ZERO=0.0
C
C1------LOOP THROUGH ALL CELLS.
      DO 100 I=1,NROW
      DO 100 J=1,NCOL
C
C2------IF CELL IS DRY AND IF IT IS WETTABLE, CONTINUE CHECKING TO SEE
C2------IF IT SHOULD BECOME WET.
      IF(IBOUND(J,I,K).EQ.0 .AND. WETDRY(J,I,LAYWT(K)).NE.ZERO) THEN
C
C3------CALCULATE WETTING ELEVATION.
         WD=WETDRY(J,I,LAYWT(K))
         IF(WD.LT.ZERO) WD=-WD
         TURNON=BOTM(J,I,LBOTM(K))+WD
C
C4------CHECK HEAD IN CELL BELOW TO SEE IF WETTING ELEVATION HAS BEEN
C4------REACHED.
         IF(K.NE.NLAY) THEN
            HTMP=HNEW(J,I,K+1)
            IF(IBOUND(J,I,K+1).GT.0 .AND. HTMP.GE.TURNON) GO TO 50
         END IF
C
C5------CHECK HEAD IN ADJACENT HORIZONTAL CELLS TO SEE IF WETTING
C5------ELEVATION HAS BEEN REACHED.
         IF(WETDRY(J,I,LAYWT(K)).GT.ZERO) THEN
            IF(J.NE.1) THEN
               HTMP=HNEW(J-1,I,K)
               IF(IBOUND(J-1,I,K).GT.0 .AND. IBOUND(J-1,I,K).NE.30000.
     &                       .AND. HTMP.GE.TURNON) GO TO 50
            END IF
            IF(J.NE.NCOL) THEN
               HTMP=HNEW(J+1,I,K)
               IF(IBOUND(J+1,I,K).GT.0 .AND. HTMP.GE.TURNON) GO TO 50
            END IF
            IF(I.NE.1) THEN
               HTMP=HNEW(J,I-1,K)
               IF(IBOUND(J,I-1,K).GT.0 .AND. IBOUND(J,I-1,K).NE.30000.
     &                       .AND. HTMP.GE.TURNON) GO TO 50
            END IF
            IF(I.NE.NROW) THEN
               HTMP=HNEW(J,I+1,K)
               IF(IBOUND(J,I+1,K).GT.0 .AND. HTMP.GE.TURNON) GO TO 50
            END IF
         END IF
C
C6------WETTING ELEVATION HAS NOT BEEN REACHED, SO CELL REMAINS DRY.
         GO TO 100
C
C7------CELL BECOMES WET.  PRINT MESSAGE, SET INITIAL HEAD, AND SET
C7------IBOUND.
   50    CALL SGWF2HUF7WDMSG(2,NCNVRT,ICNVRT,JCNVRT,ACNVRT,IHDCNV,
     &             IOUT,KITER,J,I,K,KSTP,KPER)
C
C7A-----USE EQUATION 3A IF IHDWET=0; ISE EQUATION 3B IF IHDWET IS NOT 0.
         IF(IHDWET.EQ.0) THEN
            HNEW(J,I,K)=BOTM(J,I,LBOTM(K))+
     &                        WETFCT*(HTMP-BOTM(J,I,LBOTM(K)))
         ELSE
            HNEW(J,I,K)=BOTM(J,I,LBOTM(K))+WETFCT*WD
         END IF
         IBOUND(J,I,K)=30000
      END IF
  100 CONTINUE
C
      RETURN
      END

c======================================================================
      SUBROUTINE SGWF2HUF7WDMSG(ICODE,NCNVRT,ICNVRT,JCNVRT,ACNVRT,
     &             IHDCNV,IOUT,KITER,J,I,K,KSTP,KPER)
C
C%%%%%
C  PLEASE NOTE: THIS SUBROUTINE WAS COPIED DIRECTLY FROM THE LPF
C PACKAGE.  THERE IS NOTHING SPECIFIC TO THE HUF PACKAGE.
C%%%%%
C     ******************************************************************
C     PRINT MESSAGE WHEN CELLS CONVERT BETWEEN WET AND DRY.
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*3 ACNVRT
      DIMENSION ICNVRT(5),JCNVRT(5),ACNVRT(5)
C     ------------------------------------------------------------------
C1------KEEP TRACK OF CELL CONVERSIONS.
      IF(ICODE.GT.0) THEN
         NCNVRT=NCNVRT+1
         ICNVRT(NCNVRT)=I
         JCNVRT(NCNVRT)=J
         IF(ICODE.EQ.1) THEN
            ACNVRT(NCNVRT)='DRY'
         ELSE
            ACNVRT(NCNVRT)='WET'
         END IF
      END IF
C
C2------PRINT A LINE OF DATA IF 5 CONVERSIONS HAVE OCCURRED OR IF ICODE
C2------INDICATES THAT A PARTIAL LINE SHOULD BE PRINTED.
      IF(NCNVRT.EQ.5 .OR. (ICODE.EQ.0 .AND. NCNVRT.GT.0)) THEN
         IF(IHDCNV.EQ.0) WRITE(IOUT,17) KITER,K,KSTP,KPER
   17    FORMAT(1X,/1X,'CELL CONVERSIONS FOR ITER.=',I3,'  LAYER=',
     &       I3,'  STEP=',I3,'  PERIOD=',I3,'   (ROW,COL)')
         IHDCNV=1
         WRITE(IOUT,18) (ACNVRT(L),ICNVRT(L),JCNVRT(L),L=1,NCNVRT)
   18    FORMAT(1X,3X,5(A,'(',I3,',',I3,')   '))
         NCNVRT=0
      END IF
C
C3------RETURN.
      RETURN
      END

c======================================================================
      SUBROUTINE SGWF2HUF7HHARM(CR,CC,HK,IBOUND,DELR,DELC,K,NCOL,NROW,
     &         NLAY,HKCC)
C
C     ******************************************************************
C     COMPUTE HORIZONTAL BRANCH CONDUCTANCE USING HARMONIC MEAN OF BLOCK
C     CONDUCTANCES (DISTANCE WEIGHTED HARMONIC MEAN OF TRANSMISSIVITY).
C     CELL THICKNESS IS IN CC UPON ENTRY.
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION CR(NCOL,NROW,NLAY),CC(NCOL,NROW,NLAY),
     &          HK(NCOL,NROW,NLAY),
     &          IBOUND(NCOL,NROW,NLAY),DELR(NCOL),DELC(NROW),
     &          HKCC(NCOL,NROW,NLAY)
C     ------------------------------------------------------------------
      ZERO=0.
      TWO=2.
C
C1------FOR EACH CELL CALCULATE BRANCH CONDUCTANCES FROM THAT CELL
C1------TO THE ONE ON THE RIGHT AND THE ONE IN FRONT.
      DO 100 I=1,NROW
      DO 100 J=1,NCOL
C
C2------IF CELL IS DRY OR HK=0., SET CONDUCTANCE EQUAL TO 0 AND GO ON
C2------TO NEXT CELL.
      IF(IBOUND(J,I,K).EQ.0 .OR. HK(J,I,K).EQ.ZERO) THEN
         CR(J,I,K)=ZERO
         CC(J,I,K)=ZERO
      ELSE
C
C3------CELL IS WET -- CALCULATE TRANSMISSIVITY OF CELL.
         T1R=HK(J,I,K)*CC(J,I,K)
         T1C=HKCC(J,I,K)*CC(J,I,K)
C3A-----IF THIS IS NOT THE LAST COLUMN (RIGHTMOST), CALCULATE
C3A-----BRANCH CONDUCTANCE IN THE ROW DIRECTION (CR) TO THE RIGHT.
         IF(J.NE.NCOL) THEN
            IF(IBOUND(J+1,I,K).NE.0) THEN
               T2=HK(J+1,I,K)*CC(J+1,I,K)
               CR(J,I,K)=TWO*T2*T1R*DELC(I)/(T1R*DELR(J+1)+T2*DELR(J))
            ELSE
               CR(J,I,K)=ZERO
            END IF
         ELSE
C3B-----IF THIS IS THE LAST COLUMN, SET BRANCH CONDUCTANCE=0.
            CR(J,I,K)=ZERO
         END IF
C
C3C-----IF THIS IS NOT THE LAST ROW (FRONTMOST) THEN CALCULATE
C3C-----BRANCH CONDUCTANCE IN THE COLUMN DIRECTION (CC) TO THE FRONT.
         IF(I.NE.NROW) THEN
            IF(IBOUND(J,I+1,K).NE.0) THEN
               T2=HKCC(J,I+1,K)*CC(J,I+1,K)
               CC(J,I,K)=TWO*T2*T1C*DELR(J)/(T1C*DELC(I+1)+T2*DELC(I))
            ELSE
               CC(J,I,K)=ZERO
            END IF
         ELSE
C3D-----IF THIS IS THE LAST ROW, SET BRANCH CONDUCTANCE=0.
            CC(J,I,K)=ZERO
         END IF
      END IF
  100 CONTINUE
C
C4------RETURN
      RETURN
      END

c======================================================================
      SUBROUTINE GWF2HUF7FM(KITER,KSTP,KPER,ILVDA,IGRID)
C     ******************************************************************
C     ADD LEAKAGE CORRECTION AND STORAGE TO HCOF AND RHS, AND CALCULATE
C     CONDUCTANCE AS REQUIRED.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IBOUND,BOTM,DELR,DELC,LBOTM,
     1                      CR,CC,CV,HNEW,RHS,HCOF,HOLD,ISSFLG,IOUT
      USE GWFBASMODULE,ONLY:DELT
      USE GWFHUFMODULE,ONLY:LTHUF,SC1,NHUF,HUFTHK,VDHT,A9

      DOUBLE PRECISION HN
C     ------------------------------------------------------------------
      CALL SGWF2HUF7PNT(IGRID)
      ONE=1.
      ISS=ISSFLG(KPER)
C
C1------IF ANY LAYER IS CONVERTIBLE REPOPULATE ARRAYS AND CALCULATE
C          BRANCH CONDUCTANCES
      KLAYFLG=0
      DO 100 K=1,NLAY
        IF(LTHUF(K).NE.0) KLAYFLG=1
  100 CONTINUE
      IF (KLAYFLG.NE.0) THEN
        CALL SGWF2HUF7SP(KITER,KSTP,KPER,ILVDA)
      ENDIF

      IF(ILVDA.GT.0) CALL GWF2HUF7VDFM(HNEW,IBOUND,CR,CC,VDHT,
     &                                 RHS,NCOL,NROW,NLAY,A9)
C
C2------IF THE STRESS PERIOD IS TRANSIENT, ADD STORAGE TO HCOF AND RHS
C
      IF(ISS.EQ.0) THEN
         TLED=ONE/DELT
         DO 200 K=1,NLAY
C
C3------SEE IF THIS LAYER IS CONVERTIBLE OR NON-CONVERTIBLE.
         IF(LTHUF(K).EQ.0) THEN
C4------NON-CONVERTIBLE LAYER, SO USE PRIMARY STORAGE
            DO 140 I=1,NROW
            DO 140 J=1,NCOL
              IF(IBOUND(J,I,K).LE.0) GO TO 140
              RHO=SC1(J,I,K)*TLED
              HCOF(J,I,K)=HCOF(J,I,K)-RHO
              RHS(J,I,K)=RHS(J,I,K)-RHO*HOLD(J,I,K)
  140       CONTINUE
         ELSE
C
C5------A CONVERTIBLE LAYER, SO CHECK OLD AND NEW HEADS TO DETERMINE
C5------WHEN TO USE PRIMARY AND SECONDARY STORAGE
            DO 180 I=1,NROW
            DO 180 J=1,NCOL
C
C5A-----IF THE CELL IS EXTERNAL THEN SKIP IT.
              IF(IBOUND(J,I,K).LE.0) GO TO 180
              TOP=BOTM(J,I,LBOTM(K)-1)
              BOT=BOTM(J,I,LBOTM(K))
              HO=HOLD(J,I,K)
              HN=HNEW(J,I,K)
              CRHS=0.
              CHCOF=0.
              IF(HO.GT.TOP.AND.HN.GT.TOP) THEN
                CHCOF=SC1(J,I,K)*TLED
                CRHS=SC1(J,I,K)*HO*TLED
              ELSE
C---------------Compute SC1 Component
                IF(HO.GT.TOP) THEN
                  CRHS=SC1(J,I,K)*(HO-TOP)*TLED
                ELSEIF(HN.GT.TOP) THEN
                  CHCOF=SC1(J,I,K)*TLED
                  CRHS=SC1(J,I,K)*TOP*TLED
                ENDIF
C---------------Compute SC2 Component
                CALL SGWF2HUF7SC2(0,J,I,K,TOP,BOT,HN,HO,TLED,CHCOF,CRHS,
     &                        HUFTHK,NCOL,NROW,NHUF,DELR(J)*DELC(I),
     &                        IOUT)
              ENDIF
C
C5D-----ADD STORAGE TERMS TO RHS AND HCOF.
              HCOF(J,I,K)=HCOF(J,I,K) - CHCOF
              RHS(J,I,K) = RHS(J,I,K) - CRHS
C
  180       CONTINUE
         END IF
C
  200    CONTINUE
      END IF
C
C6------FOR EACH LAYER DETERMINE IF CORRECTION TERMS ARE NEEDED FOR
C6------FLOW DOWN INTO PARTIALLY SATURATED LAYERS.
      DO 300 K=1,NLAY
C
C7------SEE IF CORRECTION IS NEEDED FOR LEAKAGE FROM ABOVE.
      IF(LTHUF(K).NE.0 .AND. K.NE.1) THEN
C
C7A-----FOR EACH CELL MAKE THE CORRECTION IF NEEDED.
         DO 220 I=1,NROW
         DO 220 J=1,NCOL
C
C7B-----IF THE CELL IS EXTERNAL(IBOUND<=0) THEN SKIP IT.
         IF(IBOUND(J,I,K).LE.0) GO TO 220
         HTMP=HNEW(J,I,K)
C
C7C-----IF HEAD IS ABOVE TOP THEN CORRECTION NOT NEEDED
         TOP=BOTM(J,I,LBOTM(K)-1)
         IF(HTMP.GE.TOP) GO TO 220
C
C7D-----WITH HEAD BELOW TOP ADD CORRECTION TERMS TO RHS.
         RHS(J,I,K)=RHS(J,I,K) + CV(J,I,K-1)*(TOP-HTMP)
  220    CONTINUE
      END IF
C
C8------SEE IF THIS LAYER MAY NEED CORRECTION FOR LEAKAGE TO BELOW.
      IF(K.EQ.NLAY) GO TO 300
      IF(LTHUF(K+1).NE.0) THEN
C
C8A-----FOR EACH CELL MAKE THE CORRECTION IF NEEDED.
         DO 280 I=1,NROW
         DO 280 J=1,NCOL
C
C8B-----IF CELL IS EXTERNAL (IBOUND<=0) THEN SKIP IT.
         IF(IBOUND(J,I,K).LE.0) GO TO 280
C
C8C-----IF HEAD IN THE LOWER CELL IS LESS THAN TOP ADD CORRECTION
C8C-----TERM TO RHS.
         HTMP=HNEW(J,I,K+1)
         TOP=BOTM(J,I,LBOTM(K+1)-1)
         IF(HTMP.LT.TOP) RHS(J,I,K)=RHS(J,I,K)- CV(J,I,K)*(TOP-HTMP)
  280    CONTINUE
      END IF
C
  300 CONTINUE
C
C9------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7SC2(IFLG,J,I,K,TOP,BOT,HN,HO,TLED,CHCOF,CRHS,
     &                    HUFTHK,NCOL,NROW,NHUF,AREA,IOUT)
C
C     ******************************************************************
C     Compute contributions to HCOF and RHS for convertible cell
C     Enter subroutine when HO and/or HN are below TOP
C     Values for IFLG:
C       IFLG = 0, Calculate contributions to HCOF and RHS
C       IFLG = 1, Calculate contributions to flow within cell
C       IFLG = 2, Calculate contributions to sensitivity calculations
C     Subroutine will halt execution if Sy is not defined.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE PARAMMODULE
      DOUBLE PRECISION HN
      REAL TOPU, BOTU, THCKU, TOP, BOT, CHCOF, CRHS, AREA
      INTEGER IFLG
      DIMENSION HUFTHK(NCOL,NROW,NHUF,2)
C     ------------------------------------------------------------------
C
      IFND=0
C-----Loop through parameters
      DO 100 NP=1,MXPAR
        IF(PARTYP(NP).EQ.'SY') THEN
          BNP=B(NP)*AREA*TLED
C---------Loop through units for this parameter to determine if they apply
C         to this layer
          DO 200 ND=IPLOC(1,NP),IPLOC(2,NP)
            NU=IPCLST(1,ND)
            NM=IPCLST(2,ND)
            NZ=IPCLST(3,ND)
            TOPU=HUFTHK(J,I,NU,1)
            THCKU=HUFTHK(J,I,NU,2)
            BOTU=TOPU-THCKU
C-----------Skip this unit if it is not present in this layer
            IF(TOPU.GT.TOP.AND.BOTU.GE.TOP) GOTO 200
            IF(TOPU.LE.BOT.AND.BOTU.LT.BOT) GOTO 200
            IF(TOPU.GT.TOP) TOPU=TOP
            IF(BOTU.LT.BOT) BOTU=BOT
            CALL UHUF7RMLT(RMLT0,J,I,NZ,NM,ND)
C-----------Skip this unit if it does not apply to this cell
            IF(RMLT0.LE.0) GOTO 200
            IFND=1
C-----------Compute contributions for this unit to flow in layer
            IF(IFLG.LT.2) THEN
              IF(HO.GT.TOP) THEN
C-------------Layer converts, water table is coming down
                IF(HN.LT.TOPU.AND.HN.GT.BOTU) THEN
C---------------New head is in this unit
                  CHCOF=CHCOF+RMLT0*BNP
                  CRHS=CRHS+RMLT0*BNP*TOPU
                  IF(IFLG.EQ.1) CRHS=CRHS-RMLT0*BNP*HN
                ELSEIF(HN.LT.BOTU) THEN
C---------------New head is below this unit
                  CRHS=CRHS+RMLT0*BNP*(TOPU-BOTU)
                ENDIF
              ELSEIF(HN.GT.TOP) THEN
C-------------Layer converts, water table is going up
                IF(HO.LT.TOPU.AND.HO.GT.BOTU) THEN
C---------------Old head is in this unit
                  CRHS=CRHS+RMLT0*BNP*(HO-TOPU)
                ELSEIF(HO.LT.BOTU) THEN
C---------------Old head is below this unit
                  CRHS=CRHS+RMLT0*BNP*(BOTU-TOPU)
                ENDIF
              ELSEIF(HO.LT.TOP.AND.HN.LT.TOP) THEN
C-------------Layer does not convert, just use SC2
                IF(HO.GT.HN) THEN
C---------------Water table is coming down
                  IF(HO.LT.TOPU.AND.HO.GT.BOTU .AND.
     &               HN.LT.TOPU.AND.HN.GT.BOTU) THEN
C-----------------Old and new heads are both in this unit
                    CHCOF=CHCOF+RMLT0*BNP
                    CRHS=CRHS+RMLT0*BNP*HO
                    IF(IFLG.EQ.1) CRHS=CRHS-RMLT0*BNP*HN
                  ELSEIF(HO.LT.TOPU.AND.HO.GT.BOTU) THEN
C-----------------Old head is in this unit
                    CRHS=CRHS+RMLT0*BNP*(HO-BOTU)
                  ELSEIF(HN.LT.TOPU.AND.HN.GT.BOTU) THEN
C-----------------New head is in this unit
                    CHCOF=CHCOF+RMLT0*BNP
                    CRHS=CRHS+RMLT0*BNP*TOPU
                    IF(IFLG.EQ.1) CRHS=CRHS-RMLT0*BNP*HN
                  ELSEIF(HO.GT.TOPU.AND.HN.LT.BOTU) THEN
C-----------------Old head is above and new head is below this unit
                    CRHS=CRHS+RMLT0*BNP*(TOPU-BOTU)
                  ENDIF
                ELSE
C---------------Water table is going up
                  IF(HO.LT.TOPU.AND.HO.GT.BOTU .AND.
     &               HN.LT.TOPU.AND.HN.GT.BOTU) THEN
C-----------------Old and new heads are both in this unit
                    CHCOF=CHCOF+RMLT0*BNP
                    CRHS=CRHS+RMLT0*BNP*HO
                    IF(IFLG.EQ.1) CRHS=CRHS-RMLT0*BNP*HN
                  ELSEIF(HO.LT.TOPU.AND.HO.GT.BOTU) THEN
C-----------------Old head is in this unit
                    CRHS=CRHS+RMLT0*BNP*(HO-TOPU)
                  ELSEIF(HN.LT.TOPU.AND.HN.GT.BOTU) THEN
C-----------------New head is in this unit
                    CHCOF=CHCOF+RMLT0*BNP
                    CRHS=CRHS+RMLT0*BNP*BOTU
                    IF(IFLG.EQ.1) CRHS=CRHS-RMLT0*BNP*HN
                  ELSEIF(HO.LT.BOTU.AND.HN.GT.TOPU) THEN
C-----------------Old head is below and new head is abov this unit
                    CRHS=CRHS+RMLT0*BNP*(BOTU-TOPU)
                  ENDIF
                ENDIF
              ENDIF
            ELSEIF(IFLG.EQ.2) THEN
              IF(HO.LE.TOPU.AND.HO.GT.BOTU) THEN
                CRHS=RMLT0*B(NP)*AREA
                RETURN
              ENDIF
            ENDIF
  200     CONTINUE
        ENDIF
  100 CONTINUE
      IF(IFND.EQ.0) THEN
        WRITE(IOUT,500) K,I,J
  500   FORMAT("Sy not defined for cell at (layer,row,column):",
     &         3(I4,","))
        CALL USTOP(' ')
      ENDIF
C
C4------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE GWF2HUF7BDS(KSTP,KPER,IGRID)
C     ******************************************************************
C     COMPUTE STORAGE BUDGET FLOW TERM FOR HUF.
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,ISSFLG,IBOUND,HNEW,HOLD,BOTM,
     1                      LBOTM,DELR,DELC,BUFF,IOUT
      USE GWFBASMODULE,ONLY:MSUM,ICBCFL,VBVL,VBNM,DELT,PERTIM,TOTIM
      USE GWFHUFMODULE,ONLY:LTHUF,SC1,IHUFCB,HUFTHK,NHUF
      
      CHARACTER*16 TEXT
      DOUBLE PRECISION STOIN,STOUT,SSTRG,HN
C
      DATA TEXT /'         STORAGE'/
C     ------------------------------------------------------------------
C
      CALL SGWF2HUF7PNT(IGRID)
C
C2------INITIALIZE BUDGET ACCUMULATORS AND 1/DELT.
      ISS=ISSFLG(KPER)
      ZERO=0.
      STOIN=ZERO
      STOUT=ZERO
C
C1------SKIP STORAGE BUDGET CALCULATIONS IF STEADY STATE.
      IF(ISS.NE.0) GOTO 400
      ONE=1.
      TLED=ONE/DELT
C
C3------IF CELL-BY-CELL FLOWS WILL BE SAVED, SET FLAG IBD.
      IBD=0
      IF(IHUFCB.GT.0) IBD=ICBCFL
C
C4------CLEAR BUFFER.
      DO 210 K=1,NLAY
      DO 210 I=1,NROW
      DO 210 J=1,NCOL
        BUFF(J,I,K)=ZERO
210   CONTINUE
C
C5------LOOP THROUGH EVERY CELL IN THE GRID.
      KT=0
      DO 300 K=1,NLAY
        LC=LTHUF(K)
        IF(LC.NE.0) KT=KT+1
        DO 300 I=1,NROW
        DO 300 J=1,NCOL
C
C6------SKIP NO-FLOW AND CONSTANT-HEAD CELLS.
          IF(IBOUND(J,I,K).LE.0) GO TO 300
          HN=HNEW(J,I,K)
          HO=HOLD(J,I,K)
          STRG=0.
C
C7-----CHECK LAYER TYPE TO SEE IF ONE STORAGE CAPACITY OR TWO.
          IF(LC.EQ.0) GO TO 285
          TOP=BOTM(J,I,LBOTM(K)-1)
          BOT=BOTM(J,I,LBOTM(K))
          IF(HO.GT.TOP.AND.HN.GT.TOP) GOTO 285
C
C7A----TWO STORAGE CAPACITIES.
C---------------Compute SC1 Component
          IF(HO.GT.TOP) THEN
            STRG=SC1(J,I,K)*(HO-TOP)*TLED
          ELSEIF(HN.GT.TOP) THEN
            STRG=SC1(J,I,K)*TLED*(TOP-HN)
          ENDIF
C---------------Compute SC2 Component
          CALL SGWF2HUF7SC2(1,J,I,K,TOP,BOT,HN,HO,TLED,CHCOF,STRG,
     &           HUFTHK,NCOL,NROW,NHUF,DELR(J)*DELC(I),IOUT)
C      STRG=SOLD*(HOLD(J,I,K)-TP) + SNEW*TP - SNEW*HSING
          GO TO 288
C
C7B----ONE STORAGE CAPACITY.
  285     RHO=SC1(J,I,K)*TLED
          STRG=RHO*(HO-HN)

C
C8-----STORE CELL-BY-CELL FLOW IN BUFFER AND ADD TO ACCUMULATORS.
  288     BUFF(J,I,K)=STRG
          SSTRG=STRG
          IF(STRG) 292,300,294
  292     STOUT=STOUT-SSTRG
          GO TO 300
  294     STOIN=STOIN+SSTRG
C
  300 CONTINUE
C
C9-----IF IBD FLAG IS SET RECORD THE CONTENTS OF THE BUFFER.
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,
     &                       IHUFCB,BUFF,NCOL,NROW,NLAY,IOUT)
      IF(IBD.EQ.2) CALL UBDSV1(KSTP,KPER,TEXT,IHUFCB,
     &            BUFF,NCOL,NROW,NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
C
C10-----ADD TOTAL RATES AND VOLUMES TO VBVL & PUT TITLE IN VBNM.
  400 CONTINUE
      SIN=STOIN
      SOUT=STOUT
      VBVL(1,MSUM)=VBVL(1,MSUM)+SIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+SOUT*DELT
      VBVL(3,MSUM)=SIN
      VBVL(4,MSUM)=SOUT
      VBNM(MSUM)=TEXT
      MSUM=MSUM+1
C
C11----RETURN.
      RETURN
      END
c======================================================================
      SUBROUTINE GWF2HUF7BDADJ(KSTP,KPER,IDIR,IBDRET,IC1,IC2,IR1,IR2,
     1                          IL1,IL2,ILVDA,IGRID)
C
C     ******************************************************************
C     COMPUTE FLOW BETWEEN ADJACENT CELLS IN A SUBREGION OF THE GRID
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IBOUND,HNEW,BUFF,CR,CC,CV,
     1                      BOTM,LBOTM,IOUT
      USE GWFBASMODULE,ONLY:DELT,PERTIM,TOTIM,ICBCFL,ICHFLG
      USE GWFHUFMODULE,ONLY:LTHUF,IHUFCB,VDHT

      CHARACTER*16 TEXT(3)
      DOUBLE PRECISION HD,DFL,DFR,DFT,DFB
C
      DATA TEXT(1),TEXT(2),TEXT(3)
     & /'FLOW RIGHT FACE ','FLOW FRONT FACE ','FLOW LOWER FACE '/
C     ------------------------------------------------------------------
      CALL SGWF2HUF7PNT(IGRID)
C
C1------IF CELL-BY-CELL FLOWS WILL BE SAVED IN A FILE, SET FLAG IBD.
C1------RETURN IF FLOWS ARE NOT BEING SAVED OR RETURNED.
      ZERO=0.
      IBD=0
      IF(IHUFCB.GT.0) IBD=ICBCFL
      IF(IBD.EQ.0 .AND. IBDRET.EQ.0) RETURN
C
C2------SET THE SUBREGION EQUAL TO THE ENTIRE GRID IF VALUES ARE BEING
C2------SAVED IN A FILE.
      IF(IBD.NE.0) THEN
         K1=1
         K2=NLAY
         I1=1
         I2=NROW
         J1=1
         J2=NCOL
      END IF
C
C3------TEST FOR DIRECTION OF CALCULATION;  IF NOT ACROSS COLUMNS, GO TO
C3------STEP 4.  IF ONLY 1 COLUMN, RETURN.
      IF(IDIR.NE.1) GO TO 405
      IF(NCOL.EQ.1) RETURN
C
C3A-----CALCULATE FLOW ACROSS COLUMNS (THROUGH RIGHT FACE).  IF NOT
C3A-----SAVING IN A FILE, SET THE SUBREGION.  CLEAR THE BUFFER.
      IF(IBD.EQ.0) THEN
         K1=IL1
         K2=IL2
         I1=IR1
         I2=IR2
         J1=IC1-1
         IF(J1.LT.1) J1=1
         J2=IC2
      END IF
      DO 310 K=K1,K2
      DO 310 I=I1,I2
      DO 310 J=J1,J2
      BUFF(J,I,K)=ZERO
  310 CONTINUE
C
C3B-----FOR EACH CELL CALCULATE FLOW THRU RIGHT FACE & STORE IN BUFFER.
      IF(J2.EQ.NCOL) J2=J2-1
      DO 400 K=K1,K2
      DO 400 I=I1,I2
      DO 400 J=J1,J2
      IF(ICHFLG.EQ.0) THEN
        IF((IBOUND(J,I,K) .EQ. 0) .OR. ((IBOUND(J,I,K).LT.0) .AND. 
     &     (IBOUND(J+1,I,K).LE.0))) GO TO 400
      ELSE
         IF((IBOUND(J,I,K).EQ.0) .OR. (IBOUND(J+1,I,K).EQ.0)) GO TO 400
      END IF
      IF(ILVDA.GT.0) THEN
        CALL SGWF2HUF7VDF9(I,J,K,VDHT,HNEW,IBOUND,NLAY,NROW,NCOL,
     &                 DFL,DFR,DFT,DFB)
        BUFF(J,I,K) = DFR
      ELSE
        HDIFF=HNEW(J,I,K)-HNEW(J+1,I,K)
        BUFF(J,I,K)=HDIFF*CR(J,I,K)
      ENDIF
  400 CONTINUE
C
C3C-----RECORD CONTENTS OF BUFFER AND RETURN.
      IF(IBD.EQ.1)
     &   CALL UBUDSV(KSTP,KPER,TEXT(1),IHUFCB,BUFF,NCOL,NROW,NLAY,IOUT)
      IF(IBD.EQ.2)
     &   CALL UBDSV1(KSTP,KPER,TEXT(1),IHUFCB,BUFF,NCOL,NROW,
     &               NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      RETURN
C
C4------TEST FOR DIRECTION OF CALCULATION;  IF NOT ACROSS ROWS, GO TO
C4------STEP 5.  IF ONLY 1 ROW, RETURN.
  405 IF(IDIR.NE.2) GO TO 505
      IF(NROW.EQ.1) RETURN
C
C4A-----CALCULATE FLOW ACROSS ROWS (THROUGH FRONT FACE).  IF NOT SAVING
C4A-----IN A FILE, SET THE SUBREGION.  CLEAR THE BUFFER.
      IF(IBD.EQ.0) THEN
         K1=IL1
         K2=IL2
         I1=IR1-1
         IF(I1.LT.1) I1=1
         I2=IR2
         J1=IC1
         J2=IC2
      END IF
      DO 410 K=K1,K2
      DO 410 I=I1,I2
      DO 410 J=J1,J2
      BUFF(J,I,K)=ZERO
  410 CONTINUE
C
C4B-----FOR EACH CELL CALCULATE FLOW THRU FRONT FACE & STORE IN BUFFER.
      IF(I2.EQ.NROW) I2=I2-1
      DO 500 K=K1,K2
      DO 500 I=I1,I2
      DO 500 J=J1,J2
      IF(ICHFLG.EQ.0) THEN
         IF((IBOUND(J,I,K) .EQ. 0) .OR. ((IBOUND(J,I,K).LT.0).AND. 
     &      (IBOUND(J,I+1,K).LE.0))) GO TO 500
      ELSE
         IF((IBOUND(J,I,K).EQ.0) .OR. (IBOUND(J,I+1,K).EQ.0)) GO TO 500
      END IF
      IF(ILVDA.GT.0) THEN
        CALL SGWF2HUF7VDF9(I,J,K,VDHT,HNEW,IBOUND,NLAY,NROW,NCOL,
     &                 DFL,DFR,DFT,DFB)
C        BUFF(J,I,K) = DFT
        BUFF(J,I,K) = DFB
      ELSE
        HDIFF=HNEW(J,I,K)-HNEW(J,I+1,K)
        BUFF(J,I,K)=HDIFF*CC(J,I,K)
      ENDIF
  500 CONTINUE
C
C4C-----RECORD CONTENTS OF BUFFER AND RETURN.
      IF(IBD.EQ.1)
     &   CALL UBUDSV(KSTP,KPER,TEXT(2),IHUFCB,BUFF,NCOL,NROW,NLAY,IOUT)
      IF(IBD.EQ.2)
     &   CALL UBDSV1(KSTP,KPER,TEXT(2),IHUFCB,BUFF,NCOL,NROW,
     &     NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      RETURN
C
C5------DIRECTION OF CALCULATION IS ACROSS LAYERS BY ELIMINATION.  IF
C5------ONLY 1 LAYER, RETURN.
  505 IF(NLAY.EQ.1) RETURN
C
C5A-----CALCULATE FLOW ACROSS LAYERS (THROUGH LOWER FACE).  IF NOT
C5A-----SAVING IN A FILE, SET THE SUBREGION.  CLEAR THE BUFFER.
      IF(IBD.EQ.0) THEN
         K1=IL1-1
         IF(K1.LT.1) K1=1
         K2=IL2
         I1=IR1
         I2=IR2
         J1=IC1
         J2=IC2
      END IF
      DO 510 K=K1,K2
      DO 510 I=I1,I2
      DO 510 J=J1,J2
      BUFF(J,I,K)=ZERO
  510 CONTINUE
C
C5B-----FOR EACH CELL CALCULATE FLOW THRU LOWER FACE & STORE IN BUFFER.
      IF(K2.EQ.NLAY) K2=K2-1
      DO 600 K=1,K2
      IF(K.LT.K1) GO TO 600
      DO 590 I=I1,I2
      DO 590 J=J1,J2
      IF(ICHFLG.EQ.0) THEN
         IF((IBOUND(J,I,K).LE.0) .AND. (IBOUND(J,I,K+1).LE.0)) GO TO 590
      ELSE
         IF((IBOUND(J,I,K).EQ.0) .OR. (IBOUND(J,I,K+1).EQ.0)) GO TO 590
      END IF
      HD=HNEW(J,I,K+1)
      IF(LTHUF(K+1).EQ.0) GO TO 580
      TMP=HD
      TOP=BOTM(J,I,LBOTM(K+1)-1)
      IF(TMP.LT.TOP) HD=TOP
  580 HDIFF=HNEW(J,I,K)-HD
      BUFF(J,I,K)=HDIFF*CV(J,I,K)
  590 CONTINUE
  600 CONTINUE
C
C5C-----RECORD CONTENTS OF BUFFER AND RETURN.
      IF(IBD.EQ.1)
     &   CALL UBUDSV(KSTP,KPER,TEXT(3),IHUFCB,BUFF,NCOL,NROW,NLAY,IOUT)
      IF(IBD.EQ.2)
     &   CALL UBDSV1(KSTP,KPER,TEXT(3),IHUFCB,BUFF,NCOL,NROW,
     &               NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      RETURN
      END
c======================================================================
      SUBROUTINE GWF2HUF7BDCH(KSTP,KPER,ILVDA,IGRID)
C     ******************************************************************
C     COMPUTE FLOW FROM CONSTANT-HEAD CELLS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IBOUND,HNEW,BUFF,CR,CC,CV,
     1                      BOTM,LBOTM,IOUT
      USE GWFBASMODULE,ONLY:MSUM,VBVL,VBNM,DELT,PERTIM,TOTIM,ICBCFL,
     1                      ICHFLG
      USE GWFHUFMODULE,ONLY:LTHUF,IHUFCB,VDHT

      CHARACTER*16 TEXT
      DOUBLE PRECISION HD,CHIN,CHOUT,XX1,XX2,XX3,XX4,XX5,XX6
      DOUBLE PRECISION DFL,DFR,DFT,DFB
C
      DATA TEXT /'   CONSTANT HEAD'/
C     ------------------------------------------------------------------
      CALL SGWF2HUF7PNT(IGRID)
C
C1------SET IBD TO INDICATE IF CELL-BY-CELL BUDGET VALUES WILL BE SAVED.
      IBD=0
      IF(IHUFCB.LT.0 .AND. ICBCFL.NE.0) IBD=-1
      IF(IHUFCB.GT.0) IBD=ICBCFL
C
C2------CLEAR BUDGET ACCUMULATORS.
      ZERO=0.
      CHIN=ZERO
      CHOUT=ZERO
      IBDLBL=0
C
C3------CLEAR BUFFER.
      DO 5 K=1,NLAY
      DO 5 I=1,NROW
      DO 5 J=1,NCOL
      BUFF(J,I,K)=ZERO
5     CONTINUE
C
C3A-----IF SAVING CELL-BY-CELL FLOW IN A LIST, COUNT CONSTANT-HEAD
C3A-----CELLS AND WRITE HEADER RECORDS.
      IF(IBD.EQ.2) THEN
         NCH=0
         DO 7 K=1,NLAY
         DO 7 I=1,NROW
         DO 7 J=1,NCOL
         IF(IBOUND(J,I,K).LT.0) NCH=NCH+1
7        CONTINUE
         CALL UBDSV2(KSTP,KPER,TEXT,IHUFCB,NCOL,NROW,NLAY,
     &          NCH,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      END IF
C
C4------LOOP THROUGH EACH CELL AND CALCULATE FLOW INTO MODEL FROM EACH
C4------CONSTANT-HEAD CELL.
      DO 200 K=1,NLAY
      DO 200 I=1,NROW
      DO 200 J=1,NCOL
C
C5------IF CELL IS NOT CONSTANT HEAD SKIP IT & GO ON TO NEXT CELL.
      IF (IBOUND(J,I,K).GE.0)GO TO 200
C
C6------CLEAR VALUES FOR FLOW RATE THROUGH EACH FACE OF CELL.
      X1=ZERO
      X2=ZERO
      X3=ZERO
      X4=ZERO
      X5=ZERO
      X6=ZERO
      CHCH1=ZERO
      CHCH2=ZERO
      CHCH3=ZERO
      CHCH4=ZERO
      CHCH5=ZERO
      CHCH6=ZERO
      IF(ILVDA.GT.0)
     &  CALL SGWF2HUF7VDF9(I,J,K,VDHT,HNEW,IBOUND,NLAY,NROW,NCOL,
     &                     DFL,DFR,DFT,DFB)
C7------CALCULATE FLOW THROUGH THE LEFT FACE.
C7------COMMENTS A-C APPEAR ONLY IN THE SECTION HEADED BY COMMENT 7,
C7------BUT THEY APPLY IN A SIMILAR MANNER TO SECTIONS 8-12.
C
C7A-----IF THERE IS NO FLOW TO CALCULATE THROUGH THIS FACE, THEN GO ON
C7A-----TO NEXT FACE.  NO FLOW OCCURS AT THE EDGE OF THE GRID, TO AN
C7A-----ADJACENT NO-FLOW CELL, OR TO AN ADJACENT CONSTANT-HEAD CELL.
      IF(J.EQ.1) GO TO 30
      IF(IBOUND(J-1,I,K).EQ.0) GO TO 30
      IF(IBOUND(J-1,I,K).LT.0 .AND. ICHFLG.EQ.0) GO TO 30
C
C7B-----CALCULATE FLOW THROUGH THE LEFT FACE.
      IF(ILVDA.GT.0) THEN
        CHCH1 = -DFL
      ELSE
        HDIFF=HNEW(J,I,K)-HNEW(J-1,I,K)
        CHCH1=HDIFF*CR(J-1,I,K)
      ENDIF
      IF(IBOUND(J-1,I,K).LT.0) GO TO 30
      X1=CHCH1
      XX1=X1
C
C7C-----ACCUMULATE POSITIVE AND NEGATIVE FLOW.
      IF (X1) 10,30,20
   10 CHOUT=CHOUT-XX1
      GO TO 30
   20 CHIN=CHIN+XX1
C
C8------CALCULATE FLOW THROUGH THE RIGHT FACE.
   30 IF(J.EQ.NCOL) GO TO 60
      IF(IBOUND(J+1,I,K).EQ.0) GO TO 60
      IF(IBOUND(J+1,I,K).LT.0 .AND. ICHFLG.EQ.0) GO TO 60
      IF(ILVDA.GT.0) THEN
        CHCH2 = DFR
      ELSE
        HDIFF=HNEW(J,I,K)-HNEW(J+1,I,K)
        CHCH2=HDIFF*CR(J,I,K)
      ENDIF
      IF(IBOUND(J+1,I,K).LT.0) GO TO 60
      X2=CHCH2
      XX2=X2
      IF(X2)40,60,50
   40 CHOUT=CHOUT-XX2
      GO TO 60
   50 CHIN=CHIN+XX2
C
C9------CALCULATE FLOW THROUGH THE BACK FACE.
   60 IF(I.EQ.1) GO TO 90
      IF (IBOUND(J,I-1,K).EQ.0) GO TO 90
      IF (IBOUND(J,I-1,K).LT.0 .AND. ICHFLG.EQ.0) GO TO 90
      IF(ILVDA.GT.0) THEN
        CHCH3 = -DFT
      ELSE
        HDIFF=HNEW(J,I,K)-HNEW(J,I-1,K)
        CHCH3=HDIFF*CC(J,I-1,K)
      ENDIF
      IF(IBOUND(J,I-1,K).LT.0) GO TO 90
      X3=CHCH3
      XX3=X3
      IF(X3) 70,90,80
   70 CHOUT=CHOUT-XX3
      GO TO 90
   80 CHIN=CHIN+XX3
C
C10-----CALCULATE FLOW THROUGH THE FRONT FACE.
   90 IF(I.EQ.NROW) GO TO 120
      IF(IBOUND(J,I+1,K).EQ.0) GO TO 120
      IF(IBOUND(J,I+1,K).LT.0 .AND. ICHFLG.EQ.0) GO TO 120
      IF(ILVDA.GT.0) THEN
        CHCH4 = DFB
      ELSE
        HDIFF=HNEW(J,I,K)-HNEW(J,I+1,K)
        CHCH4=HDIFF*CC(J,I,K)
      ENDIF
      IF(IBOUND(J,I+1,K).LT.0) GO TO 120
      X4=CHCH4
      XX4=X4
      IF (X4) 100,120,110
  100 CHOUT=CHOUT-XX4
      GO TO 120
  110 CHIN=CHIN+XX4
C
C11-----CALCULATE FLOW THROUGH THE UPPER FACE.
  120 IF(K.EQ.1) GO TO 150
      IF (IBOUND(J,I,K-1).EQ.0) GO TO 150
      IF (IBOUND(J,I,K-1).LT.0 .AND. ICHFLG.EQ.0) GO TO 150
      HD=HNEW(J,I,K)
      IF(LTHUF(K).EQ.0) GO TO 122
      TMP=HD
      TOP=BOTM(J,I,LBOTM(K)-1)
      IF(TMP.LT.TOP) HD=TOP
  122 HDIFF=HD-HNEW(J,I,K-1)
      CHCH5=HDIFF*CV(J,I,K-1)
      IF(IBOUND(J,I,K-1).LT.0) GO TO 150
      X5=CHCH5
      XX5=X5
      IF(X5) 130,150,140
  130 CHOUT=CHOUT-XX5
      GO TO 150
  140 CHIN=CHIN+XX5
C
C12-----CALCULATE FLOW THROUGH THE LOWER FACE.
  150 IF(K.EQ.NLAY) GO TO 180
      IF(IBOUND(J,I,K+1).EQ.0) GO TO 180
      IF(IBOUND(J,I,K+1).LT.0 .AND. ICHFLG.EQ.0) GO TO 180
      HD=HNEW(J,I,K+1)
      IF(LTHUF(K+1).EQ.0) GO TO 152
      TMP=HD
      TOP=BOTM(J,I,LBOTM(K+1)-1)
      IF(TMP.LT.TOP) HD=TOP
  152 HDIFF=HNEW(J,I,K)-HD
      CHCH6=HDIFF*CV(J,I,K)
      IF(IBOUND(J,I,K+1).LT.0) GO TO 180
      X6=CHCH6
      XX6=X6
      IF(X6) 160,180,170
  160 CHOUT=CHOUT-XX6
      GO TO 180
  170 CHIN=CHIN+XX6
C
C13-----SUM THE FLOWS THROUGH SIX FACES OF CONSTANT HEAD CELL, AND
C13-----STORE SUM IN BUFFER.
 180  RATE=CHCH1+CHCH2+CHCH3+CHCH4+CHCH5+CHCH6
      BUFF(J,I,K)=RATE
C
C14-----PRINT THE FLOW FOR THE CELL IF REQUESTED.
      IF(IBD.LT.0) THEN
         IF(IBDLBL.EQ.0) WRITE(IOUT,899) TEXT,KPER,KSTP
  899    FORMAT(1X,/1X,A,'   PERIOD',I3,'   STEP',I3)
         WRITE(IOUT,900) K,I,J,RATE
  900    FORMAT(1X,'LAYER',I3,'   ROW',I4,'   COL',I4,
     &       '   RATE',1PG15.6)
         IBDLBL=1
      END IF
C
C15-----IF SAVING CELL-BY-CELL FLOW IN LIST, WRITE FLOW FOR CELL.
      IF(IBD.EQ.2)
     &   CALL UBDSVA(IHUFCB,NCOL,NROW,J,I,K,RATE,IBOUND,NLAY)
  200 CONTINUE
C
C16-----IF SAVING CELL-BY-CELL FLOW IN 3-D ARRAY, WRITE THE ARRAY.
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,
     &                   IHUFCB,BUFF,NCOL,NROW,NLAY,IOUT)
C
C17-----SAVE TOTAL CONSTANT HEAD FLOWS AND VOLUMES IN VBVL TABLE
C17-----FOR INCLUSION IN BUDGET. PUT LABELS IN VBNM TABLE.
      CIN=CHIN
      COUT=CHOUT
      VBVL(1,MSUM)=VBVL(1,MSUM)+CIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+COUT*DELT
      VBVL(3,MSUM)=CIN
      VBVL(4,MSUM)=COUT
      VBNM(MSUM)=TEXT
      MSUM=MSUM+1
C
C18-----RETURN.
      RETURN
      END
c======================================================================
      SUBROUTINE GWF2HUF7OT(KSTP,KPER,ICNVG,ISA,IGRID)
C
C     ******************************************************************
C     PRINT AND RECORD HEADS AND FLOWS INTERPOLATED TO HGU'S
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IBOUND,HNEW,BOTM,NBOTM,CV,
     1                      DELR,DELC,IOUT
      USE GWFBASMODULE,ONLY:DELT,PERTIM,TOTIM,IHEDFM,CHEDFM,ICBCFL,
     2                      ICHFLG,HNOFLO

      USE GWFHUFMODULE,ONLY:NHUF,HUFTHK,GS,IOHUFHDS,IOHUFFLWS,HGUNAM,
     1                      HNWHGU=>HUFTMP
      CHARACTER*16 TEXT

c      CHARACTER*20 CHEDFM
c      CHARACTER*10 HGUNAM
C
      DATA TEXT /'     HEAD IN HGU'/
C     ------------------------------------------------------------------
C
      CALL SGWF2HUF7PNT(IGRID)
C
      IF(ISA.EQ.0.OR.ICNVG.EQ.0) THEN
        IF(ISA.EQ.0) THEN
          WRITE(IOUT,9) KSTP,KPER
    9     FORMAT(1X,/11X,'NO FLOW EQUATION TO SOLVE IN TIME STEP',I3,
     &      ' OF STRESS PERIOD',I3,/1X,'ALL HEADS ARE 0.0')
        END IF
C
C2------IF ITERATIVE PROCEDURE FAILED TO CONVERGE PRINT MESSAGE
        IF(ICNVG.EQ.0) THEN
          WRITE(IOUT,17) KSTP,KPER
   17     FORMAT(1X,/11X,'****FAILED TO CONVERGE IN TIME STEP',I3,
     &      ' OF STRESS PERIOD',I3,'****')
        END IF
      RETURN
      ENDIF
C
C-------CALCULATE HEADS WITHIN UNITS
      IF(IOHUFHDS.GT.0) THEN
        CALL SGWF2HUF7HDOT(HNWHGU,HNEW,IBOUND,NHUF,NCOL,NROW,NLAY,
     &               HUFTHK,BOTM,NBOTM,CV,DELR,DELC,HNOFLO,IOUT,GS)

        IFIRST=1
        DO 100 NU=1,NHUF
C
C-------CALL ULAPRS OR ULAPRW TO PRINT HEAD.
          WRITE(IOUT,72) KSTP,KPER,NU,HGUNAM(NU)
   72     FORMAT('1',
     &      /2X,'HEAD AT END OF TIME STEP',I3,' IN STRESS PERIOD',I3,
     &      /2X,'IN HYDROGEOLOGIC UNIT',I3,' WITH NAME ',A10,
     &      /2X,71('-'))
          IF(IHEDFM.LT.0) CALL ULAPRS(HNWHGU(:,:,NU),TEXT,KSTP,KPER,
     &                 NCOL,NROW,0,-IHEDFM,IOUT)
          IF(IHEDFM.GE.0) CALL ULAPRW(HNWHGU(:,:,NU),TEXT,KSTP,KPER,
     &                 NCOL,NROW,0,IHEDFM,IOUT)
C
C5------FOR EACH UNIT: DETERMINE IF HEAD SHOULD BE SAVED ON DISK.
C5------IF SO THEN CALL ULASAV OR ULASV2 TO SAVE HEAD.
          IF(IFIRST.EQ.1) WRITE(IOUT,74) IOHUFHDS,KSTP,KPER
   74       FORMAT(1X,/1X,'HEAD IN HYDROGEOLOGIC UNITS WILL BE SAVED ',
     &        'ON UNIT ',I4,' AT END OF TIME STEP',I3,', STRESS PERIOD',
     &        I3)
          IF(IFIRST.EQ.1) IFIRST=0
          IF(CHEDFM.EQ.' ') THEN
            CALL ULASAV(HNWHGU(:,:,NU),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     &                  NROW,NU,IOHUFHDS)
          ELSE
            CALL ULASV2(HNWHGU(:,:,NU),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     &                  NROW,NU,IOHUFHDS,CHEDFM,1,IBOUND(:,:,1))
          END IF
  100   CONTINUE
      ENDIF
C
C-------CALCULATE FLOWS WITHIN UNITS
      IF(IOHUFFLWS.GT.0) THEN
      CALL SGWF2HUF7CHFLOT(HNWHGU,HNEW,IBOUND,NHUF,NCOL,NROW,NLAY,
     &                   HUFTHK,BOTM,NBOTM,CV,DELR,DELC,IOUT,GS,
     &                   IOHUFFLWS,ICBCFL,KSTP,KPER,ICHFLG)
      CALL SGWF2HUF7FLOT(HNWHGU,HNEW,IBOUND,NHUF,NCOL,NROW,NLAY,
     &                   HUFTHK,BOTM,NBOTM,CV,DELR,DELC,IOUT,GS,
     &                   IOHUFFLWS,ICBCFL,KSTP,KPER,DELT,PERTIM,TOTIM,
     &                   ICHFLG)
      ENDIF
C
C6------RETURN.
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7FLOT(HNWHGU,HNEW,IBOUND,NHUF,NCOL,NROW,NLAY,
     &                   HUFTHK,BOTM,NBOTM,CV,DELR,DELC,IOUT,GS,IHUFCB,
     &                   IBD,KSTP,KPER,DELT,PERTIM,TOTIM,ICHFLG)
C
C     ******************************************************************
C     CALCULATE FLOWS WITHIN A GIVEN HYDROGEOLOGIC UNIT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*16 TEXT(3)
      DOUBLE PRECISION HNEW,H0,HXR,HYB,HZB,DHXR,DHYB,DHZB
      DIMENSION HNEW(NCOL,NROW,NLAY),HNWHGU(NCOL,NROW,NHUF),
     &    IBOUND(NCOL,NROW,NLAY),BOTM(NCOL,NROW,0:NBOTM),
     &    DELR(NCOL),DELC(NROW),CV(NCOL,NROW,NLAY),
     &    HUFTHK(NCOL,NROW,NHUF,2),GS(NCOL,NROW) 
C
      DATA TEXT(1),TEXT(2),TEXT(3)
     & /'FLOW RIGHT FACE ','FLOW FRONT FACE ','FLOW LOWER FACE '/
C     ------------------------------------------------------------------
C
C
      DO 50 ICNT=1,3
        IF(ICNT.EQ.3 .AND. NLAY.EQ.1) GOTO 50
C-----LOOP THROUGH ROWS AND COLUMNS
        DO 100 I=1,NROW
        DO 200 J=1,NCOL
          DO 300 NU=1,NHUF
            TOPU=HUFTHK(J,I,NU,1)
            THCKU=HUFTHK(J,I,NU,2)
            IF(THCKU.EQ.0.0) THEN
              HNWHGU(J,I,NU) = 0.0
              GOTO 300
            ENDIF
            BOTU=TOPU-THCKU
            IF(ICNT.LT.3) THEN
              IFLG=1
              CALL SGWF2HUF7HSRCH(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,
     &                            BOTU,HNEW,IBOUND,KT,KB,IFLG)
C-------------UNIT ABOVE/BELOW MODEL
              IF(IFLG.EQ.1) THEN
                HNWHGU(J,I,NU) = 0.0
                GOTO 300
              ENDIF
              Q = 0.0
              DO 400 KL=KT,KB
C
C-------------GET CONDUCTANCES
                CALL SGWF2HUF7C(I,J,KL,NU,CRL,CRR,CCT,
     &                          CCB,THK0,THKL,THKR,THKT,THKB,
     &                          HNEW,DELR,DELC,BOTM,NCOL,NROW,NLAY,
     &                          IOUT,NHUF,NBOTM,HUFTHK,GS)
C
                H0 = HNEW(J,I,KL)
                IB0 = IBOUND(J,I,KL)
C---------------FLOW TO J+1
                IF(ICNT.EQ.1) THEN
                  IBR = 0
                  IF (J.LT.NCOL) IBR = IBOUND(J+1,I,KL)
                  IF (IBR.NE.0) HXR = HNEW(J+1,I,KL)
                  IF(ICHFLG.EQ.0) THEN
                     IF(IB0.LE.0 .AND. IBR.LE.0) GO TO 400
                  ELSE
                     IF(IB0.EQ.0 .OR. IBR.EQ.0) GO TO 400
                  END IF
                  DHXR = H0 - HXR
                  Q = Q + CRR*DHXR
C---------------FLOW TO I+1
                ELSEIF(ICNT.EQ.2) THEN
                  IBB = 0
                  IF (I.LT.NROW) IBB = IBOUND(J,I+1,KL)
                  IF (IBB.NE.0) HYB = HNEW(J,I+1,KL)
                  IF(ICHFLG.EQ.0) THEN
                     IF(IB0.LE.0 .AND. IBB.LE.0) GO TO 400
                  ELSE
                     IF(IB0.EQ.0 .OR. IBB.EQ.0) GO TO 400
                  END IF
                  DHYB = H0 - HYB
                  Q = Q + CCB*DHYB
                ENDIF
  400         CONTINUE
C-------------FLOW TO K+1
            ELSE
              CALL SGWF2HUF7VSRCH(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,
     &                            TOPU,BOTU,HNEW,IBOUND,KT,KB,IFLG)
C-------------UNIT ABOVE/BELOW MODEL
              IF(IFLG.EQ.1) THEN
                HNWHGU(J,I,NU) = 0.0
                GOTO 300
              ENDIF
              Q = 0.0
              IF(KB.LT.NLAY) THEN
                IF(ICHFLG.EQ.0) THEN
                  IF(IBOUND(J,I,KB).LE.0 .AND. IBOUND(J,I,KB+1).LE.0)
     &              GO TO 410
                ELSE
                  IF(IBOUND(J,I,KB).EQ.0 .OR. IBOUND(J,I,KB+1).EQ.0)
     &              GO TO 410
                END IF
                H0 = HNEW(J,I,KB)
                HZB = HNEW(J,I,KB+1)
                CVB = CV(J,I,KB)
                DHZB = HZB - H0
                Q = CVB*DHZB
              ENDIF
            ENDIF
  410     HNWHGU(J,I,NU) = Q
  300     CONTINUE
  200   CONTINUE
  100   CONTINUE
C
C-----WRITE ARRAYS
      IF(IBD.EQ.1) THEN
C
C1------WRITE AN UNFORMATTED RECORD IDENTIFYING DATA.
        CALL UBUDSV(KSTP,KPER,TEXT(ICNT),
     &                   IHUFCB,HNWHGU,NCOL,NROW,NHUF,IOUT)
C        WRITE(IOUT,1) TEXT(ICNT),IHUFCB,KSTP,KPER
C    1   FORMAT(1X,'SGWF2HUF7FLOT SAVING "',A16,'" ON UNIT',I3,
C     &       ' AT TIME STEP',I3,', STRESS PERIOD',I3)
C        WRITE(IHUFCB) KSTP,KPER,TEXT(ICNT),NCOL,NROW,NHUF
C
C2------WRITE AN UNFORMATTED RECORD CONTAINING VALUES FOR
C2------EACH CELL IN THE GRID.
C        WRITE(IHUFCB) HNWHGU
      ENDIF
      IF(IBD.EQ.2) THEN
C
C1------WRITE TWO UNFORMATTED RECORDS IDENTIFYING DATA.
        CALL UBDSV1(KSTP,KPER,TEXT(ICNT),
     &                   IHUFCB,HNWHGU,NCOL,NROW,NHUF,IOUT,
     &                   DELT,PERTIM,TOTIM,IBOUND)
C        IF(IOUT.GT.0) WRITE(IOUT,2) TEXT(ICNT),IHUFCB,KSTP,KPER
C    2   FORMAT(1X,'SGWF2HUF7FLOT SAVING "',A16,'" ON UNIT',I4,
C     &       ' AT TIME STEP',I3,', STRESS PERIOD',I3)
C        WRITE(IHUFCB) KSTP,KPER,TEXT(ICNT),NCOL,NROW,NHUF
C        WRITE(IHUFCB) 1,DELT,PERTIM,TOTIM
CC
CC
CC2------WRITE AN UNFORMATTED RECORD CONTAINING VALUES FOR
CC2------EACH CELL IN THE GRID.
C        WRITE(IHUFCB) HNWHGU
      ENDIF

   50 CONTINUE
C6------RETURN.
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7HDOT(HNWHGU,HNEW,IBOUND,NHUF,NCOL,NROW,NLAY,
     &                   HUFTHK,BOTM,NBOTM,CV,DELR,DELC,HNOFLO,IOUT,GS)
C
C     ******************************************************************
C     CALCULATE HEADS WITHIN A GIVEN HYDROGEOLOGIC UNIT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:LBOTM
      USE GWFHUFMODULE,ONLY:HGUVANI,HUFHK,HUFVK,HUFKDEP

      REAL LAMBDA,MULTKDEP
      DOUBLE PRECISION HNEW,HDIFF,QN
      DIMENSION HNEW(NCOL,NROW,NLAY),HNWHGU(NCOL,NROW,NHUF),
     &    IBOUND(NCOL,NROW,NLAY),BOTM(NCOL,NROW,0:NBOTM),
     &    DELR(NCOL),DELC(NROW),CV(NCOL,NROW,NLAY),
     &    HUFTHK(NCOL,NROW,NHUF,2),INDX(NHUF),GS(NCOL,NROW)
C     ------------------------------------------------------------------
C
C-----LOOP THROUGH ROWS AND COLUMNS
      DO 100 I=1,NROW
      DO 200 J=1,NCOL
C
C     SORT HYDROGEOLOGIC UNITS
        CALL SGWF2HUF7IND(NCOL,NROW,NHUF,HUFTHK,J,I,INDX)
C
C Zero out arrays
        DO 210 NU=1,NHUF
          HUFHK(NU)=0.
          HUFVK(NU)=0.
          HUFKDEP(NU)=0.
  210   CONTINUE
C
C---Populate HGU arrays depending on parameter type
        CALL UHUF7POP(HUFHK,'HK  ',I,J,0,IOUT)
        CALL UHUF7POP(HUFVK,'VK  ',I,J,0,IOUT)
        CALL UHUF7POP(HUFVK,'VANI',I,J,0,IOUT)
        CALL UHUF7POP(HUFKDEP,'KDEP',I,J,0,IOUT)
C
C---Populate VANI from input file if not already defined by
c     a parameter
        DO 220 NU=1,NHUF
          IF(HGUVANI(NU).GT.0..AND.HUFVK(NU).EQ.0.)
     &          HUFVK(NU)=HGUVANI(NU)
  220   CONTINUE
        DRDC=DELR(J)*DELC(I)
        IFRST=0
        DO 300 NNU=NHUF,1,-1
          NU=INDX(NNU)
          TOPU=HUFTHK(J,I,NU,1)
          THCKU=HUFTHK(J,I,NU,2)
          IF(THCKU.EQ.0.0) THEN
            IF(IFRST.EQ.0) THEN
              HNWHGU(J,I,NU)=HNOFLO
            ELSE
              IF(NNU.GT.1) HNWHGU(J,I,INDX(NNU-1)) = HNWHGU(J,I,NU)
            ENDIF
            GOTO 300
          ENDIF
          BOTU=TOPU-THCKU
          RMIDU=TOPU-0.5*THCKU
          CALL SGWF2HUF7VSRCH(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,
     &                 HNEW,IBOUND,KT,KB,IFLG)
C-------UNIT ABOVE/BELOW MODEL
          IF(IFLG.EQ.1) THEN
            HNWHGU(J,I,NU)=HNOFLO
            GOTO 300
          ENDIF
          IF(HGUVANI(NU).EQ.0.) THEN
            VK=HUFVK(NU)
          ELSE
            MULTKDEP = 1.0
            LAMBDA = HUFKDEP(NU)
            IF(LAMBDA.NE.0.) CALL SGWF2HUF7KDEP(LAMBDA,TOPU,BOTU,
     &                                      GS(J,I),MULTKDEP)
            VK=HUFHK(NU)/HUFVK(NU)
          ENDIF
C
          IF(KT.EQ.KB) THEN
            IF(KT.LT.NLAY) THEN
              HDIFF=HNEW(J,I,KT)-HNEW(J,I,KT+1)
              CVKT=CV(J,I,KT)
            ELSE
              HDIFF=HNEW(J,I,KT-1)-HNEW(J,I,KT)
              CVKT=CV(J,I,KT-1)
            ENDIF
            RMIDKT=(BOTM(J,I,LBOTM(KT)-1)+BOTM(J,I,LBOTM(KT)))/2.
            QN=HDIFF*CVKT/DRDC
            IF(IFRST.EQ.0) THEN
              HNWHGU(J,I,NU) = HNEW(J,I,KT)-QN*(RMIDKT-RMIDU)/VK
              IF(NNU.GT.1) HNWHGU(J,I,INDX(NNU-1)) =
     &                        HNEW(J,I,KT)-QN*(RMIDKT-BOTU)/VK
              IFRST=1
            ELSE
              HNWHGU(J,I,NU) = HNWHGU(J,I,NU)-QN*0.5*THCKU/VK
              IF(NNU.GT.1) HNWHGU(J,I,INDX(NNU-1)) =
     &                        HNWHGU(J,I,NU)-QN*0.5*THCKU/VK
            ENDIF
          ELSE
C---------FIRST, PROPAGATE HEAD FROM MIDDLE OF CELL TO MIDDLE OF UNIT
            DO 400 KL=KT,KB
              TOPKL=BOTM(J,I,LBOTM(KL)-1)
              BOTKL=BOTM(J,I,LBOTM(KL))
              RMIDKL=(TOPKL+BOTKL)/2.
              IF(RMIDU.LT.TOPKL.AND.RMIDU.GE.BOTKL) THEN
                IF(RMIDU.GT.RMIDKL) THEN
                  IF(KL.EQ.1) THEN
                    HDIFF=HNEW(J,I,KL)-HNEW(J,I,KL+1)
                    CVKL=CV(J,I,KL)
                  ELSE
                    HDIFF=HNEW(J,I,KL-1)-HNEW(J,I,KL)
                    CVKL=CV(J,I,KL-1)
                  ENDIF
                ELSE
                  IF(KL.LT.NLAY) THEN
                    HDIFF=HNEW(J,I,KL)-HNEW(J,I,KL+1)
                    CVKL=CV(J,I,KL)
                  ELSE
                    HDIFF=HNEW(J,I,KL-1)-HNEW(J,I,KL)
                    CVKL=CV(J,I,KL-1)
                  ENDIF
                ENDIF
              QN=HDIFF*CVKL/DRDC
              HNWHGU(J,I,NU) = HNEW(J,I,KL)+QN*(RMIDU-RMIDKL)/VK
              GOTO 410
            ENDIF
  400       CONTINUE
C---------NOW, PROPAGATE CELL TO BOTTOM OF UNIT
  410       IF(NNU.GT.1) THEN
              TOPKB=BOTM(J,I,LBOTM(KB)-1)
              BOTKB=BOTM(J,I,LBOTM(KB))
              RMIDKB=(TOPKB+BOTKB)/2.
              IF(KB.LT.NLAY) THEN
                HDIFF=HNEW(J,I,KB)-HNEW(J,I,KB+1)
                CVKB=CV(J,I,KB)
              ELSE
                HDIFF=HNEW(J,I,KB-1)-HNEW(J,I,KB)
                CVKB=CV(J,I,KB-1)
              ENDIF
              QN=HDIFF*CVKB/DRDC
              HNWHGU(J,I,INDX(NNU-1)) = HNEW(J,I,KB)-QN*(RMIDKB-BOTU)/VK
              IF(IFRST.EQ.0) IFRST=1
            ENDIF
          ENDIF
  300   CONTINUE
        DO 350 NU=1,NHUF
          IF(HUFTHK(J,I,NU,2).EQ.0.0) HNWHGU(J,I,NU) = HNOFLO
  350   CONTINUE
  200 CONTINUE
  100 CONTINUE
C
C6------RETURN.
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7CHFLOT(HNWHGU,HNEW,IBOUND,NHUF,NCOL,NROW,NLAY,
     &                   HUFTHK,BOTM,NBOTM,CV,DELR,DELC,IOUT,GS,IHUFCB,
     &                   IBD,KSTP,KPER,ICHFLG)
C     ******************************************************************
C     COMPUTE FLOW FROM CONSTANT-HEAD CELLS IN HGU's
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:LBOTM
      USE GWFHUFMODULE,ONLY:LTHUF

      CHARACTER*16 TEXT
      DOUBLE PRECISION HNEW,HDIFF,HD
C
      DIMENSION HNEW(NCOL,NROW,NLAY),HNWHGU(NCOL,NROW,NHUF),
     &     IBOUND(NCOL,NROW,NLAY),CV(NCOL,NROW,NLAY),
     &     BOTM(NCOL,NROW,0:NBOTM),DELR(NCOL),DELC(NROW),
     &     HUFTHK(NCOL,NROW,NHUF,2),GS(NCOL,NROW) 

C
C
      DATA TEXT /'   CONSTANT HEAD'/
C     ------------------------------------------------------------------
C
C2------CLEAR BUDGET ACCUMULATORS.
      ZERO=0.
C
C4------LOOP THROUGH EACH CELL AND CALCULATE FLOW INTO MODEL FROM EACH
C4------CONSTANT-HEAD CELL.
      DO 200 I=1,NROW
      DO 200 J=1,NCOL
        DO 300 NU=1,NHUF
          TOPU=HUFTHK(J,I,NU,1)
          THCKU=HUFTHK(J,I,NU,2)
          IF(THCKU.EQ.0.0) THEN
            HNWHGU(J,I,NU) = 0.0
            GOTO 300
          ENDIF
          BOTU=TOPU-THCKU
          IFLG=1
          CALL SGWF2HUF7HSRCH(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,
     &                        HNEW,IBOUND,KT,KB,IFLG)
C-------UNIT ABOVE/BELOW MODEL
          IF(IFLG.EQ.1) THEN
            HNWHGU(J,I,NU) = 0.0
            GOTO 300
          ENDIF
C6------CLEAR VALUES FOR FLOW RATE THROUGH EACH FACE OF CELL.
          RATE=ZERO
          DO 400 K=KT,KB
            CHCH1=ZERO
            CHCH2=ZERO
            CHCH3=ZERO
            CHCH4=ZERO
            CHCH5=ZERO
            CHCH6=ZERO
C
C-----------GET CONDUCTANCES
            CALL SGWF2HUF7C(I,J,K,NU,CRL,CRR,CCT,
     &                      CCB,THK0,THKL,THKR,THKT,THKB,
     &                      HNEW,DELR,DELC,BOTM,NCOL,NROW,NLAY,
     &                      IOUT,NHUF,NBOTM,HUFTHK,GS)
C
C5----------IF CELL IS NOT CONSTANT HEAD SKIP IT & GO ON TO NEXT CELL.
            IF (IBOUND(J,I,K).GE.0)GO TO 400
            H0 = HNEW(J,I,K)
C7----------CALCULATE FLOW THROUGH THE LEFT FACE.
C           IF THERE IS NO FLOW TO CALCULATE THROUGH THIS FACE, THEN GO ON
C           TO NEXT FACE.  NO FLOW OCCURS AT THE EDGE OF THE GRID, TO AN
C           ADJACENT NO-FLOW CELL, OR TO AN ADJACENT CONSTANT-HEAD CELL.
            IF(J.EQ.1) GO TO 30
            IF(IBOUND(J-1,I,K).EQ.0) GO TO 30
            IF(IBOUND(J-1,I,K).LT.0 .AND. ICHFLG.EQ.0) GO TO 30
C
C7B---------CALCULATE FLOW THROUGH THE LEFT FACE.
            HDIFF=H0-HNEW(J-1,I,K)
            CHCH1=HDIFF*CRL
C
C8----------CALCULATE FLOW THROUGH THE RIGHT FACE.
   30       IF(J.EQ.NCOL) GO TO 60
            IF(IBOUND(J+1,I,K).EQ.0) GO TO 60
            IF(IBOUND(J+1,I,K).LT.0 .AND. ICHFLG.EQ.0) GO TO 60
            HDIFF=H0-HNEW(J+1,I,K)
            CHCH2=HDIFF*CRR
C
C9----------CALCULATE FLOW THROUGH THE BACK FACE.
   60       IF(I.EQ.1) GO TO 90
            IF (IBOUND(J,I-1,K).EQ.0) GO TO 90
            IF (IBOUND(J,I-1,K).LT.0 .AND. ICHFLG.EQ.0) GO TO 90
            HDIFF=H0-HNEW(J,I-1,K)
            CHCH3=HDIFF*CCT
C
C10---------CALCULATE FLOW THROUGH THE FRONT FACE.
   90       IF(I.EQ.NROW) GO TO 120
            IF(IBOUND(J,I+1,K).EQ.0) GO TO 120
            IF(IBOUND(J,I+1,K).LT.0 .AND. ICHFLG.EQ.0) GO TO 120
            HDIFF=H0-HNEW(J,I+1,K)
            CHCH4=HDIFF*CCB
C
C11---------CALCULATE FLOW THROUGH THE UPPER FACE.
  120       IF(K.EQ.1) GO TO 150
            IF(K.GT.KT) GO TO 150
            IF (IBOUND(J,I,K-1).EQ.0) GO TO 150
            IF (IBOUND(J,I,K-1).LT.0 .AND. ICHFLG.EQ.0) GO TO 150
            HD=H0
            IF(LTHUF(K).EQ.0) GO TO 122
            TMP=HD
            TOP=BOTM(J,I,LBOTM(K)-1)
            IF(TMP.LT.TOP) HD=TOP
  122       HDIFF=HD-HNEW(J,I,K-1)
            CHCH5=HDIFF*CV(J,I,K-1)
C
C12---------CALCULATE FLOW THROUGH THE LOWER FACE.
  150       IF(K.EQ.NLAY) GO TO 180
            IF(K.LT.KB) GO TO 180
            IF(IBOUND(J,I,K+1).EQ.0) GO TO 180
            IF(IBOUND(J,I,K+1).LT.0 .AND. ICHFLG.EQ.0) GO TO 180
            HD=HNEW(J,I,K+1)
            IF(LTHUF(K+1).EQ.0) GO TO 152
            TMP=HD
            TOP=BOTM(J,I,LBOTM(K+1)-1)
            IF(TMP.LT.TOP) HD=TOP
  152       HDIFF=HNEW(J,I,K)-HD
            CHCH6=HDIFF*CV(J,I,K)
C
C13---------SUM THE FLOWS THROUGH SIX FACES OF CONSTANT HEAD CELL
  180       RATE=CHCH1+CHCH2+CHCH3+CHCH4+CHCH5+CHCH6
  400     CONTINUE
C
C13-------STORE SUM IN BUFFER.
          HNWHGU(J,I,NU) = RATE
  300   CONTINUE
  200 CONTINUE
C
C16-----IF SAVING CELL-BY-CELL FLOW IN 3-D ARRAY, WRITE THE ARRAY.
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,
     &                   IHUFCB,HNWHGU,NCOL,NROW,NHUF,IOUT)
C
C18-----RETURN.
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7C(
     1 IPT,JPT,KPT,GPT,CRL,CRR,CCT,CCB,THK0,THKL,
     2 THKR,THKT,THKB,HNEW,DELR,DELC,BOTM,NCOL,NROW,NLAY,
     3 IOUT,NHUF,NBOTM,HUFTHK,GS)
C
C     ******************************************************************
C     CALCULATE HORIZONTAL BRANCH CONDUCTANCES FOR SINGLE HGU OR FOR
C     LAYER WITHIN AN HGU.  FOR UNITS THAT PINCH OUT, THE EQUIVALENT
C     THICKNESS IN THE ADJACENT UNIT IS USED
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:LBOTM
      USE GWFHUFMODULE,ONLY:LTHUF,HGUHANI,HUFKDEP,HUFHK,HUFHANI

      DOUBLE PRECISION HNEW
      REAL LAMBDA,MULTKDEP
      INTEGER GPT
C
      DIMENSION HNEW(NCOL,NROW,NLAY),
     &    DELR(NCOL),DELC(NROW),BOTM(NCOL,NROW,0:NBOTM),
     &    HUFTHK(NCOL,NROW,NHUF,2),
     &    GS(NCOL,NROW)
C
C      INCLUDE 'param.inc'

C     ------------------------------------------------------------------
C
C
      LT = LTHUF(KPT)
      THKL = 0.0
      CRL = 0.0
      THKR = 0.0
      CRR = 0.0
      THKT = 0.0
      CCT = 0.0
      THKB = 0.0
      CCB = 0.0
C
C Loop through 5 times and calculate conductances
      DO 100 ICNT=1,5
          I = IPT
          J = JPT
        IF(ICNT.EQ.2) THEN
          J=JPT-1
        ELSEIF(ICNT.EQ.3) THEN
          J=JPT+1
        ELSEIF(ICNT.EQ.4) THEN
          I=IPT-1
        ELSEIF(ICNT.EQ.5) THEN
          I=IPT+1
        ENDIF
        IF(J.EQ.0 .OR. J.GT.NCOL) GOTO 100
        IF(I.EQ.0 .OR. I.GT.NROW) GOTO 100
C Zero out arrays
        HUFHK(GPT)=0.
        HUFHANI(GPT)=0.
        HUFKDEP(GPT)=0.
C---Get thickness of unit within cell
        TOPU=HUFTHK(J,I,GPT,1)
        THCKU=HUFTHK(J,I,GPT,2)
        TOP0=BOTM(J,I,LBOTM(KPT)-1)
        IF(LT.NE.0.AND.HNEW(J,I,KPT).LT.TOP0) TOP0=HNEW(J,I,KPT)
        BOT0=BOTM(J,I,LBOTM(KPT))
        CALL UHUF7THK(TOP0,BOT0,TOPU,THCKU,THCK,ATPU,ABTU)
        IF(ABS(THCK).LT.1E-4) THEN
C---Calculate transmissivity for cell that pinches out
          TOPG = HUFTHK(JPT,IPT,GPT,1)
          THKG = HUFTHK(JPT,IPT,GPT,2)
          BOTG = TOPG - THKG
          IF(TOPG.GT.TOP0) TOPG = TOP0
          IF(BOTG.LT.BOT0) BOTG = BOT0
          TR1 = 0.0
          TC1 = 0.0
          DO 200 NU =1,NHUF
            TOPUU=HUFTHK(J,I,NU,1)
            THCKUU=HUFTHK(J,I,NU,2)
            BOTUU = TOPUU - THCKUU
            IF(ABS(THCKUU).LT.1E-4) CYCLE
            CALL UHUF7THK(TOPG,BOTG,TOPUU,THCKUU,THK,ATPU,ABTU)
            IF(ABS(THK).GT.1E-4) THEN
              HUFHK(NU) = 0.0
              HUFHANI(NU) = 0.0
              HUFKDEP(NU)=0.
              CALL UHUF7POP(HUFHK,'HK  ',I,J,NU,IOUT)
              CALL UHUF7POP(HUFKDEP,'KDEP',I,J,NU,IOUT)
              CALL UHUF7POP(HUFHANI,'HANI',I,J,NU,IOUT)
              IF(HGUHANI(NU).GT.0..AND.HUFHANI(NU).EQ.0.)
     &              HUFHANI(NU)=HGUHANI(NU)
              MULTKDEP = 1.0
              LAMBDA = HUFKDEP(NU)
              IF(LAMBDA.NE.0.) CALL SGWF2HUF7KDEP(LAMBDA,TOPUU,BOTUU,
     &                                            GS(J,I),MULTKDEP)
              IF(THK.GT.THK0) THK = THK0
              TR1 = TR1 + HUFHK(NU) * THK * MULTKDEP
              TC1 = TC1 + HUFHK(NU) * HUFHANI(NU) * THK * MULTKDEP
            ENDIF
  200     CONTINUE
          THK = THK0
        ELSE
C---Otherwise, calculate transmissivities of cell
          CALL UHUF7POP(HUFHK,'HK  ',I,J,GPT,IOUT)
          CALL UHUF7POP(HUFKDEP,'KDEP',I,J,GPT,IOUT)
          CALL UHUF7POP(HUFHANI,'HANI',I,J,GPT,IOUT)
          IF(HGUHANI(GPT).GT.0..AND.HUFHANI(GPT).EQ.0.)
     &          HUFHANI(GPT)=HGUHANI(GPT)
          MULTKDEP = 1.0
          LAMBDA = HUFKDEP(GPT)
          CALL UHUF7THK(TOP0,BOT0,TOPU,THCKU,THK,ATPU,ABTU)
          IF(LAMBDA.NE.0.) CALL SGWF2HUF7KDEP(LAMBDA,ATPU,ABTU,
     &                                        GS(J,I),MULTKDEP)
          TR1 = HUFHK(GPT) * THK * MULTKDEP
          TC1 = TR1 * HUFHANI(GPT)
        ENDIF
C---Calculate branch conductances
        IF(ICNT.EQ.1) THEN
          THK0 = THK
          TR0 = TR1
          TC0 = TC1
        ELSEIF(ICNT.EQ.2) THEN
          THKL = THK
          IF(TR0.EQ.0.0 .AND. TR1.EQ.0.0) THEN
            CRL = 0.0
          ELSE
            CRL = 2.*TR1*TR0*DELC(I)/(TR1*DELR(J+1)+TR0*DELR(J))
          ENDIF
        ELSEIF(ICNT.EQ.3) THEN
          THKR = THK
          IF(TR0.EQ.0.0 .AND. TR1.EQ.0.0) THEN
            CRR = 0.0
          ELSE
            CRR = 2.*TR1*TR0*DELC(I)/(TR1*DELR(J-1)+TR0*DELR(J))
          ENDIF
        ELSEIF(ICNT.EQ.4) THEN
          THKT = THK
          IF(TC0.EQ.0.0 .AND. TC1.EQ.0.0) THEN
            CCT = 0.0
          ELSE
            CCT = 2.*TC1*TC0*DELR(J)/(TC1*DELC(I+1)+TC0*DELC(I))
          ENDIF
        ELSEIF(ICNT.EQ.5) THEN
          THKB = THK
          IF(TC0.EQ.0.0 .AND. TC1.EQ.0.0) THEN
            CCB = 0.0
          ELSE
            CCB = 2.*TC1*TC0*DELR(J)/(TC1*DELC(I-1)+TC0*DELC(I))
          ENDIF
        ENDIF
  100 CONTINUE
C
C
C4------RETURN
      RETURN
      END

C
c======================================================================
      SUBROUTINE SGWF2HUF7IND(NCOL,NROW,NHUF,HUFTHK,JJ,II,INDX)
C
C     ******************************************************************
C     INDEX HYDROGEOLOGIC UNITS FOR A GIVEN ROW/COLUMN LOCATION
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER NHUF,indx(NHUF),M,NSTACK
      REAL HUFTHK(NCOL,NROW,NHUF,2)
      PARAMETER (M=7,NSTACK=50)
      INTEGER i,indxt,ir,itemp,j,jstack,k,l,istack(NSTACK)
      REAL a
      do 11 j=1,NHUF
        indx(j)=j
11    continue
      jstack=0
      l=1
      ir=NHUF
1     if(ir-l.lt.M)then
        do 13 j=l+1,ir
          indxt=indx(j)
          a=HUFTHK(JJ,II,indxt,1)
          do 12 i=j-1,l,-1
            if(HUFTHK(JJ,II,indx(i),1).le.a)goto 2
            indx(i+1)=indx(i)
12        continue
          i=l-1
2         indx(i+1)=indxt
13      continue
        if(jstack.eq.0)return
        ir=istack(jstack)
        l=istack(jstack-1)

        jstack=jstack-2
      else
        k=(l+ir)/2
        itemp=indx(k)
        indx(k)=indx(l+1)
        indx(l+1)=itemp
        if(HUFTHK(JJ,II,indx(l),1).gt.HUFTHK(JJ,II,indx(ir),1))then
          itemp=indx(l)
          indx(l)=indx(ir)
          indx(ir)=itemp
        endif
        if(HUFTHK(JJ,II,indx(l+1),1).gt.HUFTHK(JJ,II,indx(ir),1))then
          itemp=indx(l+1)
          indx(l+1)=indx(ir)
          indx(ir)=itemp
        endif
        if(HUFTHK(JJ,II,indx(l),1).gt.HUFTHK(JJ,II,indx(l+1),1))then
          itemp=indx(l)
          indx(l)=indx(l+1)
          indx(l+1)=itemp
        endif
        i=l+1
        j=ir
        indxt=indx(l+1)
        a=HUFTHK(JJ,II,indxt,1)

3       continue
          i=i+1
        if(HUFTHK(JJ,II,indx(i),1).lt.a)goto 3
4       continue
          j=j-1
        if(HUFTHK(JJ,II,indx(j),1).gt.a)goto 4
        if(j.lt.i)goto 5
        itemp=indx(i)
        indx(i)=indx(j)
        indx(j)=itemp
        goto 3
5       indx(l+1)=indx(j)
        indx(j)=indxt
        jstack=jstack+2
        if(jstack.gt.NSTACK) call ustop('NSTACK too small in indexx')
        if(ir-i+1.ge.j-l)then
          istack(jstack)=ir
          istack(jstack-1)=i
          ir=j-1
        else
          istack(jstack)=j-1
          istack(jstack-1)=l
          l=i
        endif
      endif
      goto 1
      END
c======================================================================
      SUBROUTINE SGWF2HUF7KDEP1RPGD(
     & IN,IOUT,ITERP,NPKDEP,IFKDEP,NROW,NCOL,GS,TOP,NHUF)
C
C     ******************************************************************
C     READ PARAMETERS FOR KDEP CAPABILITY
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE PARAMMODULE
      CHARACTER*4 PTYP
      DIMENSION GS(NCOL,NROW),TOP(NCOL,NROW)
C     ------------------------------------------------------------------
C
      WRITE(IOUT,47)
   47 FORMAT(
     & //1X,'KDEP1 -- KDEP CAPABILITY',
     & /1X,75('-'))
C
C-------READ GROUND SURFACE OR TRANSFER FROM BOTTOM ARRAY
      IF(IFKDEP.GT.0) THEN
        WRITE(IOUT,*) 'Reading ground surface'
        CALL U2DREL(GS,'GROUND SURFACE          ',NROW,NCOL,0,IN,IOUT)
      ELSE
        WRITE(IOUT,*) 'Transferring ground surface from TOP'
        DO 100 I=1,NROW
          DO 200 J=1,NCOL
            GS(J,I) = TOP(J,I)
  200     CONTINUE
  100   CONTINUE
      ENDIF
C
C-------READ NAMED PARAMETERS
      IF(NPKDEP.GT.0) THEN
        DO 20 K=1,NPKDEP
          CALL UHUF7PARRP(IN,IOUT,NP,PTYP,ITERP,NHUF)
          IF(PTYP.EQ.'KDEP') THEN
            CONTINUE
          ELSE
            WRITE(IOUT,*) ' Invalid parameter type for KDEP capability'
            CALL USTOP(' ')
          END IF
C  Make the parameter global
          IACTIVE(NP)=-1
   20   CONTINUE
      END IF
C
C4------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7KDEP(
     &  LAMBDA,TOP,BOT,GS,MULTKDEP)
C
C     ******************************************************************
C     Calculate KDEP multiplier for interval
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL LAMBDA,MULTKDEP
C     ------------------------------------------------------------------
C
C---First, convert TOP and BOT to depths assuming that ground surface
C   is stored in BOTM(J,I,0)
      D1 = GS - TOP
      D2 = GS - BOT
      IF(ABS(2*(D2-D1)/(D1+D2)).LT.1E-6) THEN
        MULTKDEP = 1.0
        RETURN
      ENDIF
      MULTKDEP = (10.**(-LAMBDA*D2)) - (10.**(-LAMBDA*D1))
      MULTKDEP = MULTKDEP / (-LAMBDA*LOG(10.0)*(D2-D1))
C
C4------RETURN
      RETURN
      END
C=======================================================================
      SUBROUTINE SGWF2HUF7LVDA1RPGD(
     & IN,IOUT,ITERP,NHUF,NPLVDA,NLAY,ISEN)
C
C     ******************************************************************
C     READ PARAMETERS FOR LVDA CAPABILITY
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE PARAMMODULE
      USE GWFHUFMODULE,ONLY:LTHUF
C      
      CHARACTER*4 PTYP
C     ------------------------------------------------------------------
C
      WRITE(IOUT,47)
   47 FORMAT(
     & //1X,'LVDA1 -- LVDA CAPABILITY',
     & /1X,75('-'))
C
C-------READ NAMED PARAMETERS
      IF(NPLVDA.GT.0) THEN
        DO 20 K=1,NPLVDA
          CALL UHUF7PARRP(IN,IOUT,NP,PTYP,ITERP,NHUF)
          IF(PTYP.EQ.'LVDA') THEN
            CONTINUE
          ELSE
            WRITE(IOUT,*) ' Invalid parameter type for LVDA capability'
            CALL USTOP(' ')
          END IF
C  Make the parameter global
          IACTIVE(NP)=-1
   20   CONTINUE
      END IF
C
C-----CHECK TO SEE IF THE SEN PROCESS IS ACTIVE AND THERE ARE CONVERTIBLE
C     LAYERS
      IF(ISEN.GT.0) THEN
        KLAYFLG=0
        DO 30 K=1,NLAY
          IF(LTHUF(K).NE.0) KLAYFLG=1
   30   CONTINUE
        IF(KLAYFLG.NE.0) THEN
          WRITE(IOUT,*) ' LVDA cannot calculate sensitivities for',
     & ' convertible layers!'
          WRITE(IOUT,*) ' STOP EXECUTION - GWF2HUF7LVDA1RPGD'
          CALL USTOP(' ')
        ENDIF
      ENDIF
C
C4------RETURN
      RETURN
      END

c======================================================================
      SUBROUTINE GWF2HUF7VDFM(
     &  HNEW,IBOUND,CR,CC,VDHT,RHS,NCOL,NROW,NLAY,A9)
C
C     ******************************************************************
C     Calculate stiffness matrix and RHS for LVDA
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION HNEW,H1
      DOUBLE PRECISION TF9,TF5,FL5,FR5,FT5,FB5,
     &   CRL,CRR,CCT,CCB
      DIMENSION RHS(NCOL,NROW,NLAY),VDHT(NCOL,NROW,NLAY,3),
     &    IBOUND(NCOL,NROW,NLAY), HNEW(NCOL,NROW,NLAY),
     &    CR(NCOL,NROW,NLAY),CC(NCOL,NROW,NLAY),
     &    A9(NCOL,NROW,NLAY,5)
C-----------------------------------------------------------------------
C
      ZERO = 0.0
      TWO = 2.0
C-------Loop through all cells
      DO 600 K=1,NLAY
        DO 610 I=1,NROW
          DO 620 J=1,NCOL
C-------Skip calculation if cell is inactive
            IF(IBOUND(J,I,K).EQ.0) THEN
              CR(J,I,K)=ZERO
              CC(J,I,K)=ZERO
            ELSE
C
C-------Grab transmissivities
C---------Local cell 1, I,J
              TAA1 = VDHT(J,I,K,1)
              TBB1 = VDHT(J,I,K,3)
C-------Calculate CR equivalent
              IF(J.NE.NCOL) THEN
                IF(IBOUND(J+1,I,K).NE.0) THEN
                  TAA2 = VDHT(J+1,I,K,1)
                  CR(J,I,K)=TWO*TAA2*TAA1/(TAA1+TAA2)
                ELSE
                  CR(J,I,K)=ZERO
                END IF
              ELSE
                CR(J,I,K)=ZERO
              END IF
C
C-------and Calculate CC equivalent
              IF(I.NE.NROW) THEN
                IF(IBOUND(J,I+1,K).NE.0) THEN
                  TBB4 = VDHT(J,I+1,K,3)
                  CC(J,I,K)=TWO*TBB4*TBB1/(TBB1+TBB4)
                ELSE
                  CC(J,I,K)=ZERO
                END IF
              ELSE
                CC(J,I,K)=ZERO
              END IF
C
C-------and subtract elements from RHS
              CALL SGWF2HUF7VDA9F9(I,J,K,A9,HNEW,IBOUND,NLAY,NROW,NCOL,
     &                             TF9)
              H1 = HNEW(J,I,K)
              IF(I.GT.1) THEN
                CCT = CC(J,I-1,K)
                FT5= CCT * (HNEW(J,I-1,K)-H1)
              ELSE
                FT5=ZERO
              ENDIF
              IF(I.LT.NROW) THEN
                CCB = CC(J,I,K)
                FB5= CCB * (HNEW(J,I+1,K)-H1)
              ELSE
                FB5=ZERO
              ENDIF
              IF(J.GT.1) THEN
                CRL = CR(J-1,I,K)
                FL5=CRL * (HNEW(J-1,I,K)-H1)
              ELSE
                FL5=ZERO
              ENDIF
              IF(J.LT.NCOL) THEN
                CRR = CR(J,I,K)
                FR5=CRR * (HNEW(J+1,I,K)-H1)
              ELSE
                FR5=ZERO
              ENDIF
              TF5 = FL5+FR5+FT5+FB5
              RHS(J,I,K) = RHS(J,I,K) - (TF9-TF5)
            END IF
  620     CONTINUE
  610   CONTINUE
  600 CONTINUE
C
C
C-------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7VDHHV(IBOUND,HK,HKCC,VDHD,NCOL,NROW,NLAY,
     &  HUFTHK,NHUF,BOTM,NBOTM,HNEW,GS)
C
C     ******************************************************************
C     Populate the Hydraulic Conductivity Arrays for LVDA.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFHUFMODULE,ONLY:HGUHANI,HUFHK,HUFHANI,HUFKDEP
C
      DOUBLE PRECISION HNEW
      DIMENSION VDHD(NCOL,NROW,NLAY),
     &    HK(NCOL,NROW,NLAY),HKCC(NCOL,NROW,NLAY),
     &    IBOUND(NCOL,NROW,NLAY),
     &    HUFTHK(NCOL,NROW,NHUF,2),BOTM(NCOL,NROW,0:NBOTM),
     &    HNEW(NCOL,NROW,NLAY),GS(NCOL,NROW)
C-----------------------------------------------------------------------
C
C2------DEFINE DATA FOR NAMED PARAMETERS.
C
C Loop through rows and columns
      DO 100 I=1,NROW
      DO 100 J=1,NCOL
C Zero out arrays
        DO 110 NU=1,NHUF
          HUFHK(NU)=0.
          HUFHANI(NU)=0.
          HUFKDEP(NU)=0.
  110   CONTINUE
        DO 115 K=1,NLAY
          VDHD(J,I,K) = 0.0
          HK(J,I,K) = 0.0
          HKCC(J,I,K) = 0.0
  115   CONTINUE

C
C---Populate HGU arrays for given i,j depending on parameter type
        CALL UHUF7POP(HUFHK,'HK  ',I,J,0,IOUT)
        CALL UHUF7POP(HUFHANI,'HANI',I,J,0,IOUT)
        CALL UHUF7POP(HUFKDEP,'KDEP',I,J,0,IOUT)
        CALL UHUF7POPL(VDHD,NCOL,NROW,NLAY,I,J)
C
C---Populate MODEL arrays
        DO 130 NU=1,NHUF
C---Populate HANI and VANI from input file if not already defined by
c     a parameter
          IF(HGUHANI(NU).GT.0..AND.HUFHANI(NU).EQ.0.)
     &          HUFHANI(NU)=HGUHANI(NU)
          TOPU=HUFTHK(J,I,NU,1)
          THCKU=HUFTHK(J,I,NU,2)
          IF(ABS(THCKU).LT.1E-4) GOTO 130
          BOTU=TOPU-THCKU
C-----Determine which layer(s) unit applies to
          IFLG=1
          CALL SGWF2HUF7HSRCH(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,
     &                        HNEW,IBOUND,KT,KB,IFLG)
C-----Skip unit if thickness is zero
          IF(IFLG.EQ.1) GOTO 130
C-----Populate arrays
          CALL SGWF2HUF7HK(NCOL,NROW,NLAY,BOTM,NBOTM,I,J,TOPU,BOTU,KT,
     &                     KB,HK,HKCC,HUFHK,HUFHANI,HUFKDEP(NU),NHUF,NU,
     &                     HNEW,GS)
C
  130   CONTINUE
  100 CONTINUE

C
C
C-------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7VDHT(
     &  IBOUND,HK,HKCC,VDHD,VDHT,NCOL,NROW,NLAY,DELR,DELC)
C
C     ******************************************************************
C     Populate the Hydraulic Conductivity Tensor for LVDA.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION VDHD(NCOL,NROW,NLAY),
     &    VDHT(NCOL,NROW,NLAY,3),DELR(NCOL),DELC(NROW),
     &    HK(NCOL,NROW,NLAY),HKCC(NCOL,NROW,NLAY),
     &    IBOUND(NCOL,NROW,NLAY)
C-----------------------------------------------------------------------
C
      PI=3.14159265
C-------Loop through all cells, calculate Taa, Tab, and Tbb
      DO 500 K=1,NLAY
        DO 510 I=1,NROW
          DC=DELC(I)
          DO 520 J=1,NCOL
            DR=DELR(J)
            TAA=0.0
            TAB=0.0
            TBB=0.0
            IF(IBOUND(J,I,K).NE.0) THEN
              TWOTHETA = VDHD(J,I,K) * 4. * PI / 360.
              HK1=HK(J,I,K)
              HKCC1=HKCC(J,I,K)
              TAA=0.5*(HK1+HKCC1)
              TAA=TAA+0.5*COS(TWOTHETA)*(HK1-HKCC1)
              TAA=TAA*DC/DR
C---Minus sign deleted from beginning of following line 060502, according
c   to Roseanna Neupauer
              TAB=0.5*SIN(TWOTHETA)*(HK1-HKCC1)
              TBB=0.5*(HK1+HKCC1)
              TBB=TBB-0.5*COS(TWOTHETA)*(HK1-HKCC1)
              TBB=TBB*DR/DC
            ENDIF
            VDHT(J,I,K,1) = TAA
            VDHT(J,I,K,2) = TAB
            VDHT(J,I,K,3) = TBB
  520     CONTINUE
  510   CONTINUE
  500 CONTINUE
C
C
C-------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7VDF9(I,J,K,VDHT,HNEW,IBOUND,NLAY,NROW,NCOL,
     &                         FL,FR,FT,FB)
C     ******************************************************************
C     COMPUTE FLOW THROUGH CELL FACES USING THE 9-POINT STENCIL
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION HNEW,H1,H2,H3,H4,H5,H6,H7,H8,H9,FS1A,FS1B,
     &  FW1A,FW1B,FS2A,FS2B,FE2A,FE2B,FE3A,FE3B,FN3A,FN3B,
     &  FW4A,FW4B,FN4A,FN4B
      DOUBLE PRECISION FL,FR,FT,FB
      DOUBLE PRECISION AS11234,AS21234,AS31234,AS41234,
     &                 AW11234,AW21234,AW31234,AW41234,
     &                 AS16145,AS26145,AS36145,AS46145,
     &                 AE16145,AE26145,AE36145,AE46145,
     &                 AN17816,AN27816,AN37816,AN47816,
     &                 AE17816,AE27816,AE37816,AE47816,
     &                 AN18921,AN28921,AN38921,AN48921,
     &                 AW18921,AW28921,AW38921,AW48921,
     & DTAA1,DTAB1,DTBB1
C
      DIMENSION HNEW(NCOL,NROW,NLAY), IBOUND(NCOL,NROW,NLAY),
     &     VDHT(NCOL,NROW,NLAY,3)
C     ------------------------------------------------------------------
C
C-------Set scaling factor for inactive cells around current cell
      TMPCOF = 1E+8
C
C-------Calculate a coefficients
C-------Quadrant I
      CALL SGWF2HUF7VDA1(
     &             IBOUND,VDHT,NCOL,NROW,NLAY,I,J,K,TMPCOF,
     &             AS11234,AS21234,AS31234,AS41234,
     &             AW11234,AW21234,AW31234,AW41234,
     &             TAA1,TAB1,TBB1)
      DTAA1 = TAA1
      DTAB1 = TAB1
      DTBB1 = TBB1
C-------Quadrant II
      CALL SGWF2HUF7VDA2(
     &            IBOUND,VDHT,NCOL,NROW,NLAY,I,J,K,TMPCOF,
     &            AS16145,AS26145,AS36145,AS46145,
     &            AE16145,AE26145,AE36145,AE46145)
C-------Quadrant III
      CALL SGWF2HUF7VDA3(
     &            IBOUND,VDHT,NCOL,NROW,NLAY,I,J,K,TMPCOF,
     &            AN17816,AN27816,AN37816,AN47816,
     &            AE17816,AE27816,AE37816,AE47816)
C-------Quadrant IV
      CALL SGWF2HUF7VDA4(
     &            IBOUND,VDHT,NCOL,NROW,NLAY,I,J,K,TMPCOF,
     &            AN18921,AN28921,AN38921,AN48921,
     &            AW18921,AW28921,AW38921,AW48921)
C-------Get heads for 9 local cells
      CALL SGWF2HUF7VDHGET(
     &  IBOUND,HNEW,J,I,K,NROW,NCOL,NLAY,
     &  H1,H2,H3,H4,H5,H6,H7,H8,H9,
     &  IBD2,IBD3,IBD4,IBD5,IBD6,IBD7,IBD8,IBD9)
C
C
C-------CALCULATE FLOW THROUGH THE LEFT FACE.
      FS2A = AS16145*H6 + AS26145*H1 + AS36145*H4 + AS46145*H5
      FS2A = -DTAA1*(H1 - FS2A)
      FS2B = AE16145*H6 + AE26145*H1 + AE36145*H4 + AE46145*H5
      FS2B = -DTAB1*(FS2B - H1)
      FN3A = AN17816*H7 + AN27816*H8 + AN37816*H1 + AN47816*H6
      FN3A = -DTAA1*(H1 - FN3A)
      FN3B = AE17816*H7 + AE27816*H8 + AE37816*H1 + AE47816*H6
      FN3B = -DTAB1*(H1-FN3B)
      FL = FS2A + FS2B + FN3A + FN3B
C
C-------CALCULATE FLOW THROUGH THE RIGHT FACE.
      FS1A = AS11234*H1 + AS21234*H2 + AS31234*H3 + AS41234*H4
      FS1A = -DTAA1*(FS1A-H1)
      FS1B = AW11234*H1 + AW21234*H2 + AW31234*H3 + AW41234*H4
      FS1B = -DTAB1*(FS1B-H1)
      FN4A = AN18921*H8 + AN28921*H9 + AN38921*H2 + AN48921*H1
      FN4A = -DTAA1*(FN4A-H1)
      FN4B = AW18921*H8 + AW28921*H9 + AW38921*H2 + AW48921*H1
      FN4B = -DTAB1*(H1-FN4B)
      FR = FS1A + FS1B + FN4A + FN4B
C
C-------CALCULATE FLOW THROUGH THE TOP FACE.
      FE3A = AN17816*H7 + AN27816*H8 + AN37816*H1 + AN47816*H6
      FE3A = -DTAB1*(H1 - FE3A)
      FE3B = AE17816*H7 + AE27816*H8 + AE37816*H1 + AE47816*H6
      FE3B = -DTBB1*(H1 - FE3B)
      FW4A = AN18921*H8 + AN28921*H9 + AN38921*H2 + AN48921*H1
      FW4A = -DTAB1*(FW4A - H1)
      FW4B = AW18921*H8 + AW28921*H9 + AW38921*H2 + AW48921*H1
      FW4B = -DTBB1*(H1 - FW4B)
      FT = FE3A + FE3B + FW4A + FW4B
C
C-------CALCULATE FLOW THROUGH THE BOTTOM FACE.
      FE2A = AS16145*H6 + AS26145*H1 + AS36145*H4 + AS46145*H5
      FE2A = -DTAB1*(H1 - FE2A)
      FE2B = AE16145*H6 + AE26145*H1 + AE36145*H4 + AE46145*H5
      FE2B = -DTBB1*(FE2B - H1)
      FW1A = AS11234*H1 + AS21234*H2 + AS31234*H3 + AS41234*H4
      FW1A = -DTAB1*(FW1A-H1)
      FW1B = AW11234*H1 + AW21234*H2 + AW31234*H3 + AW41234*H4
      FW1B = -DTBB1*(FW1B-H1)
      FB = FE2A + FE2B + FW1A + FW1B
C
C-------RETURN.
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7VDA9F9(I,J,K,A9,HNEW,IBOUND,NLAY,NROW,NCOL,F)
C     ******************************************************************
C     COMPUTE FLOW THROUGH CELL FACES USING THE STORED 9-POINT STENCIL
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION HNEW,H1,H2,H3,H4,H5,H6,H7,H8,H9
      DOUBLE PRECISION F
      DIMENSION HNEW(NCOL,NROW,NLAY), IBOUND(NCOL,NROW,NLAY),
     &     A9(NCOL,NROW,NLAY,5)
C     ------------------------------------------------------------------
C-------Get heads for 9 local cells
      CALL SGWF2HUF7VDHGET(
     &  IBOUND,HNEW,J,I,K,NROW,NCOL,NLAY,
     &  H1,H2,H3,H4,H5,H6,H7,H8,H9,
     &  IBD2,IBD3,IBD4,IBD5,IBD6,IBD7,IBD8,IBD9)
C
C-------CALCULATE FLOW IN THE CELL
      F = 0.0
      IF(I.GT.1) THEN
        IF(J.GT.1)    F = F + A9(J-1,I-1,K,5)*H7
        F = F + A9(J,I-1,K,4)*H8
        IF(J.LT.NCOL) F = F + A9(J+1,I-1,K,3)*H9
      ENDIF
      IF(J.GT.1) F = F + A9(J-1,I,K,2)*H6
      F = F + A9(J,I,K,1)*H1
      F = F + A9(J,I,K,2)*H2
      F = F + A9(J,I,K,3)*H5
      F = F + A9(J,I,K,4)*H4
      F = F + A9(J,I,K,5)*H3
C
C-------RETURN.
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7VDA9(VDHT,A9,IBOUND,NLAY,NROW,NCOL)
C     ******************************************************************
C     COMPUTE AND STORE STIFFNESS-COEFFICIENT MATRIX FOR 9-POINT STENCIL
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
C
      DOUBLE PRECISION AS11234,AS21234,AS31234,AS41234,
     &                 AW11234,AW21234,AW31234,AW41234,
     &                 AS16145,AS26145,AS36145,AS46145,
     &                 AE16145,AE26145,AE36145,AE46145,
     &                 AN17816,AN27816,AN37816,AN47816,
     &                 AE17816,AE27816,AE37816,AE47816,
     &                 AN18921,AN28921,AN38921,AN48921,
     &                 AW18921,AW28921,AW38921,AW48921
      DIMENSION A9(NCOL,NROW,NLAY,5), IBOUND(NCOL,NROW,NLAY),
     &     VDHT(NCOL,NROW,NLAY,3)
C     ------------------------------------------------------------------
C
C-------Set scaling factor for inactive cells around current cell
      TMPCOF = 1E+8
      DO 100 K=1,NLAY
        DO 110 I=1,NROW
          DO 120 J=1,NCOL
C            IF(IBOUND(J,I,K).EQ.0) THEN
              DO 130 INDEX=1,5
                A9(J,I,K,INDEX) = 0.0
  130         CONTINUE
            IF(IBOUND(J,I,K).EQ.0) GOTO 120
C            ENDIF
C
C-------Calculate a coefficients
C-------Quadrant I
            CALL SGWF2HUF7VDA1(
     &             IBOUND,VDHT,NCOL,NROW,NLAY,I,J,K,TMPCOF,
     &             AS11234,AS21234,AS31234,AS41234,
     &             AW11234,AW21234,AW31234,AW41234,
     &             TAA1,TAB1,TBB1)
C-------Quadrant II
            CALL SGWF2HUF7VDA2(
     &            IBOUND,VDHT,NCOL,NROW,NLAY,I,J,K,TMPCOF,
     &            AS16145,AS26145,AS36145,AS46145,
     &            AE16145,AE26145,AE36145,AE46145)
C-------Quadrant III
            CALL SGWF2HUF7VDA3(
     &            IBOUND,VDHT,NCOL,NROW,NLAY,I,J,K,TMPCOF,
     &            AN17816,AN27816,AN37816,AN47816,
     &            AE17816,AE27816,AE37816,AE47816)
C-------Quadrant IV
            CALL SGWF2HUF7VDA4(
     &            IBOUND,VDHT,NCOL,NROW,NLAY,I,J,K,TMPCOF,
     &            AN18921,AN28921,AN38921,AN48921,
     &            AW18921,AW28921,AW38921,AW48921)
C
C
C-------CALCULATE COEFFICIENT FOR I,J,K (LOCAL 1)
            C = 0.0
C--FL
            IF(J.GT.1) THEN
              IF(IBOUND(J-1,I,K).NE.0)
     &          C = C + TAA1*(AS26145+AN37816)
     &                + TAB1*(AE37816-AE26145)
     &                - 2*TAA1
            ENDIF
C--FR
            IF(J.LT.NCOL) THEN
              IF(IBOUND(J+1,I,K).NE.0)
     &          C = C + TAA1*(AS11234+AN48921)
     &                + TAB1*(AW11234-AW48921)
     &                - 2*TAA1
            ENDIF
C--FT
            IF(I.GT.1) THEN
              IF(IBOUND(J,I-1,K).NE.0)
     &          C = C + TAB1*(AN37816-AN48921)
     &                + TBB1*(AE37816+AW48921)
     &                - 2*TBB1
            ENDIF
C--FB
            IF(I.LT.NROW) THEN
              IF(IBOUND(J,I+1,K).NE.0)
     &          C = C + TAB1*(AS11234-AS26145)
     &                + TBB1*(AW11234+AE26145)
     &                - 2*TBB1
            ENDIF
C            C = C - (CVB+CVT)
            A9(J,I,K,1) = C
C
C-------CALCULATE COEFFICIENT FOR I,J+1,K (LOCAL 2)
            IF(J.LT.NCOL) THEN
              IF(IBOUND(J+1,I,K).NE.0) THEN
                C =     (TAA1+TAB1)*AS21234
                C = C + (TBB1+TAB1)*AW21234
                C = C + (TAA1-TAB1)*AN38921
                C = C + (TBB1-TAB1)*AW38921
                A9(J,I,K,2) = C
              ELSE
                A9(J,I,K,2) = 0.0
              ENDIF
            ENDIF
C
C-------CALCULATE COEFFICIENT FOR I+1,J-1,K (LOCAL 5)
            IF(I.LT.NROW) THEN
              IF(J.GT.1) THEN
                IF(IBOUND(J-1,I+1,K).NE.0) THEN
                  C =     (TAA1-TAB1)*AS46145
                  C = C + (TBB1-TAB1)*AE46145
                  A9(J,I,K,3) = C
                ELSE
                  A9(J,I,K,3) = 0.0
                ENDIF
              ENDIF
C
C-------CALCULATE COEFFICIENT FOR I+1,J,K (LOCAL 4)
              IF(IBOUND(J,I+1,K).NE.0) THEN
                C =     (TAA1+TAB1)*AS41234
                C = C + (TBB1+TAB1)*AW41234
                C = C + (TAA1-TAB1)*AS36145
                C = C + (TBB1-TAB1)*AE36145
                A9(J,I,K,4) = C
              ELSE
                A9(J,I,K,4) = 0.0
              ENDIF
C
C-------CALCULATE COEFFICIENT FOR I+1,J+1,K (LOCAL 3)
              IF(J.LT.NCOL) THEN
                IF(IBOUND(J+1,I+1,K).NE.0) THEN
                  C =     (TAA1+TAB1)*AS31234
                  C = C + (TBB1+TAB1)*AW31234
                  A9(J,I,K,5) = C
                ELSE
                  A9(J,I,K,5) = 0.0
                ENDIF
              ENDIF
            ENDIF
  120     CONTINUE
  110   CONTINUE
  100 CONTINUE
C
C-------RETURN.
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7VDHGET(
     &  IBOUND,HNEW,J,I,K,NROW,NCOL,NLAY,
     &  H1,H2,H3,H4,H5,H6,H7,H8,H9,
     &  IBD2,IBD3,IBD4,IBD5,IBD6,IBD7,IBD8,IBD9)
C
C     ******************************************************************
C     Grab heads and ibound for all local cells
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION HNEW,H1,H2,H3,H4,H5,H6,H7,H8,H9
      DIMENSION HNEW(NCOL,NROW,NLAY),IBOUND(NCOL,NROW,NLAY)
C-----------------------------------------------------------------------
C
C
      ZERO = 0.0
      H1 = HNEW(J,I,K)
C-------Local cell 2, I,J+1
      IF(J.LT.NCOL .AND. IBOUND(J+1,I,K).NE.0) THEN
        H2 = HNEW(J+1,I,K)
        IBD2 = IABS(IBOUND(J+1,I,K))
      ELSE
        H2 = ZERO
        IBD2 = 0
      ENDIF
C-------Local cell 3, I+1,J+1
      IF(I.LT.NROW.AND.J.LT.NCOL.AND.IBOUND(J+1,I+1,K).NE.0) THEN
        H3 = HNEW(J+1,I+1,K)
        IBD3 = IABS(IBOUND(J+1,I+1,K))
      ELSE
        H3 = ZERO
        IBD3 = 0
      ENDIF
C-------Local cell 4, I+1,J
      IF(I.LT.NROW .AND. IBOUND(J,I+1,K).NE.0) THEN
        H4 = HNEW(J,I+1,K)
        IBD4 = IABS(IBOUND(J,I+1,K))
      ELSE
        H4 = ZERO
        IBD4 = 0
      ENDIF
C-------Local cell 5, I+1,J-1
      IF(J.GT.1.AND.I.LT.NROW .AND. IBOUND(J-1,I+1,K).NE.0) THEN
        H5 = HNEW(J-1,I+1,K)
        IBD5 = IABS(IBOUND(J-1,I+1,K))
      ELSE
        H5 = ZERO
        IBD5 = 0
      ENDIF
C-------Local cell 6, I,J-1
      IF(J.GT.1 .AND. IBOUND(J-1,I,K).NE.0) THEN
        H6 = HNEW(J-1,I,K)
        IBD6 = IABS(IBOUND(J-1,I,K))
      ELSE
        H6 = ZERO
        IBD6 = 0
      ENDIF
C-------Local cell 7, I-1,J-1
      IF(I.GT.1.AND.J.GT.1 .AND. IBOUND(J-1,I-1,K).NE.0) THEN
        H7 = HNEW(J-1,I-1,K)
        IBD7 = IABS(IBOUND(J-1,I-1,K))
      ELSE
        H7 = ZERO
        IBD7 = 0
      ENDIF
C-------Local cell 8, I-1,J
      IF(I.GT.1 .AND. IBOUND(J,I-1,K).NE.0) THEN
        H8 = HNEW(J,I-1,K)
        IBD8 = IABS(IBOUND(J,I-1,K))
      ELSE
        H8 = ZERO
        IBD8 = 0
      ENDIF
C-------Local cell 9, I-1,J+1
      IF(J.LT.NCOL.AND.I.GT.1 .AND. IBOUND(J+1,I-1,K).NE.0) THEN
        H9 = HNEW(J+1,I-1,K)
        IBD9 = IABS(IBOUND(J+1,I-1,K))
      ELSE
        H9 = ZERO
        IBD9 = 0
      ENDIF
C-------RETURN.
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7VDA1(
     &  IBOUND,VDHT,NCOL,NROW,NLAY,I,J,K,TMPCOF,
     2  AS11234,AS21234,AS31234,AS41234,
     3  AW11234,AW21234,AW31234,AW41234,
     4  TAA1,TAB1,TBB1)
C
C     ******************************************************************
C     Calculate the A coefficients for Quadrant I
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION VDHT(NCOL,NROW,NLAY,3),IBOUND(NCOL,NROW,NLAY)
      DOUBLE PRECISION AS11234,AS21234,AS31234,AS41234,
     3  AW11234,AW21234,AW31234,AW41234,D1234
C-----------------------------------------------------------------------
C
C-------Local cell 1, I,J
      TAA1 = VDHT(J,I,K,1)
      TAB1 = VDHT(J,I,K,2)
      TBB1 = VDHT(J,I,K,3)
C-------Local cell 2, I,J+1
      IF(J.LT.NCOL .AND. IBOUND(J+1,I,K).NE.0) THEN
        TAA2 = VDHT(J+1,I,K,1)
        TAB2 = VDHT(J+1,I,K,2)
        TBB2 = VDHT(J+1,I,K,3)
      ELSE
        TAA2 = TAA1/TMPCOF
        TAB2 = TAB1/TMPCOF
        TBB2 = TBB1/TMPCOF
      ENDIF
C-------Local cell 3, I+1,J+1
      IF(I.LT.NROW.AND.J.LT.NCOL.AND.IBOUND(J+1,I+1,K).NE.0) THEN
        TAA3 = VDHT(J+1,I+1,K,1)
        TAB3 = VDHT(J+1,I+1,K,2)
        TBB3 = VDHT(J+1,I+1,K,3)
      ELSE
        TAA3 = TAA1/TMPCOF
        TAB3 = TAB1/TMPCOF
        TBB3 = TBB1/TMPCOF
      ENDIF
C-------Local cell 4, I+1,J
      IF(I.LT.NROW .AND. IBOUND(J,I+1,K).NE.0) THEN
        TAA4 = VDHT(J,I+1,K,1)
        TAB4 = VDHT(J,I+1,K,2)
        TBB4 = VDHT(J,I+1,K,3)
      ELSE
        TAA4 = TAA1/TMPCOF
        TAB4 = TAB1/TMPCOF
        TBB4 = TBB1/TMPCOF
      ENDIF

      CALL SGWF2HUF7VDCOFD(D1234,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
     &                     TAA3,TAB3,TBB3,TAA4,TAB4,TBB4)
      CALL SGWF2HUF7VDCOFAS1(AS11234,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
     &                       TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D1234)
      CALL SGWF2HUF7VDCOFAS2(AS21234,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
     &                       TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D1234)
      CALL SGWF2HUF7VDCOFAS3(AS31234,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
     &                       TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D1234)
      CALL SGWF2HUF7VDCOFAS4(AS41234,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
     &                       TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D1234)
      CALL SGWF2HUF7VDCOFAW1(AW11234,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
     &                       TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D1234)
      CALL SGWF2HUF7VDCOFAW2(AW21234,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
     &                       TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D1234)
      CALL SGWF2HUF7VDCOFAW3(AW31234,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
     &                       TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D1234)
      CALL SGWF2HUF7VDCOFAW4(AW41234,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
     &                       TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D1234)

C
C
C-------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7VDA2(
     1  IBOUND,VDHT,NCOL,NROW,NLAY,I,J,K,TMPCOF,
     2  AS16145,AS26145,AS36145,AS46145,
     3  AE16145,AE26145,AE36145,AE46145)
C
C     ******************************************************************
C     Calculate the A coefficients for Quadrant II
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION VDHT(NCOL,NROW,NLAY,3),IBOUND(NCOL,NROW,NLAY)
      DOUBLE PRECISION AS16145,AS26145,AS36145,AS46145,
     3  AE16145,AE26145,AE36145,AE46145,D6145
C-----------------------------------------------------------------------
C
C-------Local cell 1, I,J
      TAA1 = VDHT(J,I,K,1)
      TAB1 = VDHT(J,I,K,2)
      TBB1 = VDHT(J,I,K,3)
C-------Local cell 4, I+1,J
      IF(I.LT.NROW .AND. IBOUND(J,I+1,K).NE.0) THEN
        TAA4 = VDHT(J,I+1,K,1)
        TAB4 = VDHT(J,I+1,K,2)
        TBB4 = VDHT(J,I+1,K,3)
      ELSE
        TAA4 = TAA1/TMPCOF
        TAB4 = TAB1/TMPCOF
        TBB4 = TBB1/TMPCOF
      ENDIF
C-------Local cell 5, I+1,J-1
      IF(J.GT.1.AND.I.LT.NROW .AND. IBOUND(J-1,I+1,K).NE.0) THEN
        TAA5 = VDHT(J-1,I+1,K,1)
        TAB5 = VDHT(J-1,I+1,K,2)
        TBB5 = VDHT(J-1,I+1,K,3)
      ELSE
        TAA5 = TAA1/TMPCOF
        TAB5 = TAB1/TMPCOF
        TBB5 = TBB1/TMPCOF
      ENDIF
C-------Local cell 6, I,J-1
      IF(J.GT.1 .AND. IBOUND(J-1,I,K).NE.0) THEN
        TAA6 = VDHT(J-1,I,K,1)
        TAB6 = VDHT(J-1,I,K,2)
        TBB6 = VDHT(J-1,I,K,3)
      ELSE
        TAA6 = TAA1/TMPCOF
        TAB6 = TAB1/TMPCOF
        TBB6 = TBB1/TMPCOF
      ENDIF
      CALL SGWF2HUF7VDCOFD(D6145,TAA6,TAB6,TBB6,TAA1,TAB1,TBB1,
     &                     TAA4,TAB4,TBB4,TAA5,TAB5,TBB5)
      CALL SGWF2HUF7VDCOFAS1(AS16145,TAA6,TAB6,TBB6,TAA1,TAB1,TBB1,
     &                       TAA4,TAB4,TBB4,TAA5,TAB5,TBB5,D6145)
      CALL SGWF2HUF7VDCOFAS2(AS26145,TAA6,TAB6,TBB6,TAA1,TAB1,TBB1,
     &                       TAA4,TAB4,TBB4,TAA5,TAB5,TBB5,D6145)
      CALL SGWF2HUF7VDCOFAS3(AS36145,TAA6,TAB6,TBB6,TAA1,TAB1,TBB1,
     &                       TAA4,TAB4,TBB4,TAA5,TAB5,TBB5,D6145)
      CALL SGWF2HUF7VDCOFAS4(AS46145,TAA6,TAB6,TBB6,TAA1,TAB1,TBB1,
     &                       TAA4,TAB4,TBB4,TAA5,TAB5,TBB5,D6145)
      CALL SGWF2HUF7VDCOFAE1(AE16145,TAA6,TAB6,TBB6,TAA1,TAB1,TBB1,
     &                       TAA4,TAB4,TBB4,TAA5,TAB5,TBB5,D6145)
      CALL SGWF2HUF7VDCOFAE2(AE26145,TAA6,TAB6,TBB6,TAA1,TAB1,TBB1,
     &                       TAA4,TAB4,TBB4,TAA5,TAB5,TBB5,D6145)
      CALL SGWF2HUF7VDCOFAE3(AE36145,TAA6,TAB6,TBB6,TAA1,TAB1,TBB1,
     &                       TAA4,TAB4,TBB4,TAA5,TAB5,TBB5,D6145)
      CALL SGWF2HUF7VDCOFAE4(AE46145,TAA6,TAB6,TBB6,TAA1,TAB1,TBB1,
     &                       TAA4,TAB4,TBB4,TAA5,TAB5,TBB5,D6145)
C
C
C-------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7VDA3(
     1  IBOUND,VDHT,NCOL,NROW,NLAY,I,J,K,TMPCOF,
     2  AN17816,AN27816,AN37816,AN47816,
     3  AE17816,AE27816,AE37816,AE47816)
C
C     ******************************************************************
C     Calculate the A coefficients for Quadrant III
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION VDHT(NCOL,NROW,NLAY,3),IBOUND(NCOL,NROW,NLAY)
      DOUBLE PRECISION AN17816,AN27816,AN37816,AN47816,
     3  AE17816,AE27816,AE37816,AE47816,D7816
C-----------------------------------------------------------------------
C
C-------Local cell 1, I,J
      TAA1 = VDHT(J,I,K,1)
      TAB1 = VDHT(J,I,K,2)
      TBB1 = VDHT(J,I,K,3)
C-------Local cell 6, I,J-1
      IF(J.GT.1 .AND. IBOUND(J-1,I,K).NE.0) THEN
        TAA6 = VDHT(J-1,I,K,1)
        TAB6 = VDHT(J-1,I,K,2)
        TBB6 = VDHT(J-1,I,K,3)
      ELSE
        TAA6 = TAA1/TMPCOF
        TAB6 = TAB1/TMPCOF
        TBB6 = TBB1/TMPCOF
      ENDIF
C-------Local cell 7, I-1,J-1
      IF(I.GT.1.AND.J.GT.1 .AND. IBOUND(J-1,I-1,K).NE.0) THEN
        TAA7 = VDHT(J-1,I-1,K,1)
        TAB7 = VDHT(J-1,I-1,K,2)
        TBB7 = VDHT(J-1,I-1,K,3)
      ELSE
        TAA7 = TAA1/TMPCOF
        TAB7 = TAB1/TMPCOF
        TBB7 = TBB1/TMPCOF
      ENDIF
C-------Local cell 8, I-1,J
      IF(I.GT.1 .AND. IBOUND(J,I-1,K).NE.0) THEN
        TAA8 = VDHT(J,I-1,K,1)
        TAB8 = VDHT(J,I-1,K,2)
        TBB8 = VDHT(J,I-1,K,3)
      ELSE
        TAA8 = TAA1/TMPCOF
        TAB8 = TAB1/TMPCOF
        TBB8 = TBB1/TMPCOF
      ENDIF
      CALL SGWF2HUF7VDCOFD(D7816,TAA7,TAB7,TBB7,TAA8,TAB8,TBB8,
     &                     TAA1,TAB1,TBB1,TAA6,TAB6,TBB6)
      CALL SGWF2HUF7VDCOFAN1(AN17816,TAA7,TAB7,TBB7,TAA8,TAB8,TBB8,
     &                       TAA1,TAB1,TBB1,TAA6,TAB6,TBB6,D7816)
      CALL SGWF2HUF7VDCOFAN2(AN27816,TAA7,TAB7,TBB7,TAA8,TAB8,TBB8,
     &                       TAA1,TAB1,TBB1,TAA6,TAB6,TBB6,D7816)
      CALL SGWF2HUF7VDCOFAN3(AN37816,TAA7,TAB7,TBB7,TAA8,TAB8,TBB8,
     &                       TAA1,TAB1,TBB1,TAA6,TAB6,TBB6,D7816)
      CALL SGWF2HUF7VDCOFAN4(AN47816,TAA7,TAB7,TBB7,TAA8,TAB8,TBB8,
     &                       TAA1,TAB1,TBB1,TAA6,TAB6,TBB6,D7816)
      CALL SGWF2HUF7VDCOFAE1(AE17816,TAA7,TAB7,TBB7,TAA8,TAB8,TBB8,
     &                       TAA1,TAB1,TBB1,TAA6,TAB6,TBB6,D7816)
      CALL SGWF2HUF7VDCOFAE2(AE27816,TAA7,TAB7,TBB7,TAA8,TAB8,TBB8,
     &                       TAA1,TAB1,TBB1,TAA6,TAB6,TBB6,D7816)
      CALL SGWF2HUF7VDCOFAE3(AE37816,TAA7,TAB7,TBB7,TAA8,TAB8,TBB8,
     &                       TAA1,TAB1,TBB1,TAA6,TAB6,TBB6,D7816)
      CALL SGWF2HUF7VDCOFAE4(AE47816,TAA7,TAB7,TBB7,TAA8,TAB8,TBB8,
     &                       TAA1,TAB1,TBB1,TAA6,TAB6,TBB6,D7816)
C
C
C-------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7VDA4(
     1  IBOUND,VDHT,NCOL,NROW,NLAY,I,J,K,TMPCOF,
     2  AN18921,AN28921,AN38921,AN48921,
     3  AW18921,AW28921,AW38921,AW48921)
C
C     ******************************************************************
C     Calculate the A coefficients for Quadrant IV
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION VDHT(NCOL,NROW,NLAY,3),IBOUND(NCOL,NROW,NLAY)
      DOUBLE PRECISION AN18921,AN28921,AN38921,AN48921,
     3  AW18921,AW28921,AW38921,AW48921,D8921
C-----------------------------------------------------------------------
C
C-------Local cell 1, I,J
      TAA1 = VDHT(J,I,K,1)
      TAB1 = VDHT(J,I,K,2)
      TBB1 = VDHT(J,I,K,3)
C-------Local cell 2, I,J+1
      IF(J.LT.NCOL .AND. IBOUND(J+1,I,K).NE.0) THEN
        TAA2 = VDHT(J+1,I,K,1)
        TAB2 = VDHT(J+1,I,K,2)
        TBB2 = VDHT(J+1,I,K,3)
      ELSE
        TAA2 = TAA1/TMPCOF
        TAB2 = TAB1/TMPCOF
        TBB2 = TBB1/TMPCOF
      ENDIF
C-------Local cell 8, I-1,J
      IF(I.GT.1 .AND. IBOUND(J,I-1,K).NE.0) THEN
        TAA8 = VDHT(J,I-1,K,1)
        TAB8 = VDHT(J,I-1,K,2)
        TBB8 = VDHT(J,I-1,K,3)
      ELSE
        TAA8 = TAA1/TMPCOF
        TAB8 = TAB1/TMPCOF
        TBB8 = TBB1/TMPCOF
      ENDIF
C-------Local cell 9, I-1,J+1
      IF(J.LT.NCOL.AND.I.GT.1 .AND. IBOUND(J+1,I-1,K).NE.0) THEN
        TAA9 = VDHT(J+1,I-1,K,1)
        TAB9 = VDHT(J+1,I-1,K,2)
        TBB9 = VDHT(J+1,I-1,K,3)
      ELSE
        TAA9 = TAA1/TMPCOF
        TAB9 = TAB1/TMPCOF
        TBB9 = TBB1/TMPCOF
      ENDIF
      CALL SGWF2HUF7VDCOFD(D8921,TAA8,TAB8,TBB8,TAA9,TAB9,TBB9,
     &                     TAA2,TAB2,TBB2,TAA1,TAB1,TBB1)
      CALL SGWF2HUF7VDCOFAN1(AN18921,TAA8,TAB8,TBB8,TAA9,TAB9,TBB9,
     &                       TAA2,TAB2,TBB2,TAA1,TAB1,TBB1,D8921)
      CALL SGWF2HUF7VDCOFAN2(AN28921,TAA8,TAB8,TBB8,TAA9,TAB9,TBB9,
     &                       TAA2,TAB2,TBB2,TAA1,TAB1,TBB1,D8921)
      CALL SGWF2HUF7VDCOFAN3(AN38921,TAA8,TAB8,TBB8,TAA9,TAB9,TBB9,
     &                       TAA2,TAB2,TBB2,TAA1,TAB1,TBB1,D8921)
      CALL SGWF2HUF7VDCOFAN4(AN48921,TAA8,TAB8,TBB8,TAA9,TAB9,TBB9,
     &                       TAA2,TAB2,TBB2,TAA1,TAB1,TBB1,D8921)
      CALL SGWF2HUF7VDCOFAW1(AW18921,TAA8,TAB8,TBB8,TAA9,TAB9,TBB9,
     &                       TAA2,TAB2,TBB2,TAA1,TAB1,TBB1,D8921)
      CALL SGWF2HUF7VDCOFAW2(AW28921,TAA8,TAB8,TBB8,TAA9,TAB9,TBB9,
     &                       TAA2,TAB2,TBB2,TAA1,TAB1,TBB1,D8921)
      CALL SGWF2HUF7VDCOFAW3(AW38921,TAA8,TAB8,TBB8,TAA9,TAB9,TBB9,
     &                       TAA2,TAB2,TBB2,TAA1,TAB1,TBB1,D8921)
      CALL SGWF2HUF7VDCOFAW4(AW48921,TAA8,TAB8,TBB8,TAA9,TAB9,TBB9,
     &                       TAA2,TAB2,TBB2,TAA1,TAB1,TBB1,D8921)
C
C
C-------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7VDCOFD(D,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4)
C
C     ******************************************************************
C     Calculate the D coefficient
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION D
C-----------------------------------------------------------------------
C
      D=(TAB1*TAB1)*(TAB3*TAB3)
      D=D+(TAB2*TAB2)*(TAB4*TAB4)
      D=D-2.0*TAB1*TAB2*TAB3*TAB4
      D=D+(TAA1+TAA2)*(TAA3+TAA4)*(TBB1+TBB4)*(TBB2+TBB3)
      D=D-(TAB1*TAB1)*(TAA3+TAA4)*(TBB2+TBB3)
      D=D-(TAB2*TAB2)*(TAA3+TAA4)*(TBB1+TBB4)
      D=D-(TAB3*TAB3)*(TAA1+TAA2)*(TBB1+TBB4)
      D=D-(TAB4*TAB4)*(TAA1+TAA2)*(TBB2+TBB3)
C
C
C-------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7VDCOFAS1(AS1,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D)
C
C     ******************************************************************
C     Calculate the AS1 coefficient
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION AS1,C1,C2,D
C-----------------------------------------------------------------------
C
      C1=(TAA3+TAA4)*(TBB1+TBB4)*(TBB2+TBB3)
      C1=C1-(TAB3*TAB3)*(TBB1+TBB4)
      C1=C1-(TAB4*TAB4)*(TBB2+TBB3)
      C1=C1*(TAA1+TAB1)
      C2=TAB1*TAB3*TAB3
      C2=C2-TAB1*(TAA3+TAA4)*(TBB2+TBB3)
      C2=C2-TAB2*TAB3*TAB4
      C2=C2*(TBB1+TAB1)
      AS1=(C1+C2)/D
C
C-------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7VDCOFAS2(AS2,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D)
C
C     ******************************************************************
C     Calculate the AS2 coefficient
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION AS2,C1,C2,D
C-----------------------------------------------------------------------
C
      C1=(TAA3+TAA4)*(TBB1+TBB4)*(TBB2+TBB3)
      C1=C1-(TAB3*TAB3)*(TBB1+TBB4)
      C1=C1-TAB4*TAB4*(TBB2+TBB3)
      C1=C1*(TAA2-TAB2)
      C2=TAB1*TAB3*TAB4
      C2=C2+TAB2*(TAA3+TAA4)*(TBB1+TBB4)
      C2=C2-TAB2*TAB4*TAB4
      C2=C2*(TBB2-TAB2)
      AS2=(C1+C2)/D
C
C-------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7VDCOFAS3(AS3,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D)
C
C     ******************************************************************
C     Calculate the AS3 coefficient
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION AS3,C1,C2,D
C-----------------------------------------------------------------------
C
      C1=-TAB1*TAB4*(TBB2+TBB3)
      C1=C1-TAB2*TAB3*(TBB1+TBB4)
      C1=C1*(TAA3+TAB3)
      C2=TAB1*TAB3*TAB4
      C2=C2+TAB2*(TAA3+TAA4)*(TBB1+TBB4)
      C2=C2-TAB2*TAB4*TAB4
      C2=C2*(TBB3+TAB3)
      AS3=(C1+C2)/D
C
C-------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7VDCOFAS4(AS4,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D)
C
C     ******************************************************************
C     Calculate the AS4 coefficient
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION AS4,C1,C2,D
C-----------------------------------------------------------------------
C
      C1=-TAB1*TAB4*(TBB2+TBB3)
      C1=C1-TAB2*TAB3*(TBB1+TBB4)
      C1=C1*(TAA4-TAB4)
      C2=TAB1*TAB3*TAB3
      C2=C2-TAB1*(TAA3+TAA4)*(TBB2+TBB3)
      C2=C2-TAB2*TAB3*TAB4
      C2=C2*(TBB4-TAB4)
      AS4=(C1+C2)/D
C
C-------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7VDCOFAW1(AW1,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D)
C
C     ******************************************************************
C     Calculate the AW1 coefficient
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION AW1,C1,C2,D
C-----------------------------------------------------------------------
C
      C1=TAB1*TAB3*TAB3
      C1=C1-TAB2*TAB3*TAB4
      C1=C1-TAB1*(TAA3+TAA4)*(TBB2+TBB3)
      C1=C1*(TAA1+TAB1)
      C2=(TAA1+TAA2)*(TAA3+TAA4)*(TBB2+TBB3)
      C2=C2-TAB2*TAB2*(TAA3+TAA4)
      C2=C2-TAB3*TAB3*(TAA1+TAA2)
      C2=C2*(TBB1+TAB1)
      AW1=(C1+C2)/D
C
C-------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7VDCOFAW2(AW2,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D)
C
C     ******************************************************************
C     Calculate the AW2 coefficient
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION AW2,C1,C2,D
C-----------------------------------------------------------------------
C
      C1=TAB1*TAB3*TAB3
      C1=C1-TAB2*TAB3*TAB4
      C1=C1-TAB1*(TAA3+TAA4)*(TBB2+TBB3)
      C1=C1*(TAA2-TAB2)
      C2=-TAB1*TAB2*(TAA3+TAA4)
      C2=C2-TAB3*TAB4*(TAA1+TAA2)
      C2=C2*(TBB2-TAB2)
      AW2=(C1+C2)/D
C
C-------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7VDCOFAW3(AW3,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D)
C
C     ******************************************************************
C     Calculate the AW3 coefficient
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION AW3,C1,C2,D
C-----------------------------------------------------------------------
C
      C1=TAB1*TAB2*TAB3
      C1=C1+TAB4*(TAA1+TAA2)*(TBB2+TBB3)
      C1=C1-TAB2*TAB2*TAB4
      C1=C1*(TAA3+TAB3)
      C2=-TAB1*TAB2*(TAA3+TAA4)
      C2=C2-TAB3*TAB4*(TAA1+TAA2)
      C2=C2*(TBB3+TAB3)
      AW3=(C1+C2)/D
C
C-------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7VDCOFAW4(AW4,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D)
C
C     ******************************************************************
C     Calculate the AW4 coefficient
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION AW4,C1,C2,D
C-----------------------------------------------------------------------
C
      C1=TAB1*TAB2*TAB3
      C1=C1+TAB4*(TAA1+TAA2)*(TBB2+TBB3)
      C1=C1-TAB2*TAB2*TAB4
      C1=C1*(TAA4-TAB4)
      C2=(TAA1+TAA2)*(TAA3+TAA4)*(TBB2+TBB3)
      C2=C2-TAB2*TAB2*(TAA3+TAA4)
      C2=C2-TAB3*TAB3*(TAA1+TAA2)
      C2=C2*(TBB4-TAB4)
      AW4=(C1+C2)/D
C
C-------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7VDCOFAE1(AE1,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D)
C
C     ******************************************************************
C     Calculate the AE1 coefficient
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION AE1,C1,C2,D
C-----------------------------------------------------------------------
C     6145
C     1234
      C1=TAB1*TAB3*TAB4
      C1=C1+TAB2*(TAA3+TAA4)*(TBB1+TBB4)
      C1=C1-TAB2*TAB4*TAB4
      C1=C1*(TAA1+TAB1)
      C2=-TAB3*TAB4*(TAA1+TAA2)
      C2=C2-TAB1*TAB2*(TAA3+TAA4)
      C2=C2*(TBB1+TAB1)
      AE1=(C1+C2)/D
C
C-------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7VDCOFAE2(AE2,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D)
C
C     ******************************************************************
C     Calculate the AE2 coefficient
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION AE2,C1,C2,D
C-----------------------------------------------------------------------
C     6145
C     1234
      C1=TAB1*TAB3*TAB4
      C1=C1+TAB2*(TAA3+TAA4)*(TBB1+TBB4)
      C1=C1-TAB2*TAB4*TAB4
      C1=C1*(TAA2-TAB2)
      C2=(TAA1+TAA2)*(TAA3+TAA4)*(TBB1+TBB4)
      C2=C2-TAB4*TAB4*(TAA1+TAA2)
      C2=C2-TAB1*TAB1*(TAA3+TAA4)
      C2=C2*(TBB2-TAB2)
      AE2=(C1+C2)/D
C
C-------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7VDCOFAE3(AE3,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D)
C
C     ******************************************************************
C     Calculate the AE3 coefficient
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION AE3,C1,C2,D
C-----------------------------------------------------------------------
C     6145
C     1234
      C1=TAB1*TAB1*TAB3
      C1=C1-TAB3*(TAA1+TAA2)*(TBB1+TBB4)
      C1=C1-TAB1*TAB2*TAB4
      C1=C1*(TAA3+TAB3)
      C2=(TAA1+TAA2)*(TAA3+TAA4)*(TBB1+TBB4)
      C2=C2-TAB4*TAB4*(TAA1+TAA2)
      C2=C2-TAB1*TAB1*(TAA3+TAA4)
      C2=C2*(TBB3+TAB3)
      AE3=(C1+C2)/D
C
C-------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7VDCOFAE4(AE4,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D)
C
C     ******************************************************************
C     Calculate the AE4 coefficient
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION AE4,C1,C2,D
C-----------------------------------------------------------------------
C     6145
C     1234
      C1=TAB1*TAB1*TAB3
      C1=C1-TAB3*(TAA1+TAA2)*(TBB1+TBB4)
      C1=C1-TAB1*TAB2*TAB4
      C1=C1*(TAA4-TAB4)
      C2=-TAB3*TAB4*(TAA1+TAA2)
      C2=C2-TAB1*TAB2*(TAA3+TAA4)
      C2=C2*(TBB4-TAB4)
      AE4=(C1+C2)/D
C
C-------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7VDCOFAN1(AN1,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D)
C
C     ******************************************************************
C     Calculate the AN1 coefficient
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION AN1,C1,C2,D
C-----------------------------------------------------------------------
C     7816
C     1234
      C1=-TAB2*TAB3*(TBB1+TBB4)
      C1=C1-TAB1*TAB4*(TBB2+TBB3)
      C1=C1*(TAA1+TAB1)
      C2=TAB1*TAB2*TAB3
      C2=C2+TAB4*(TAA1+TAA2)*(TBB2+TBB3)
      C2=C2-TAB2*TAB2*TAB4
      C2=C2*(TBB1+TAB1)
      AN1=(C1+C2)/D
C
C-------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7VDCOFAN2(AN2,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D)
C
C     ******************************************************************
C     Calculate the AN2 coefficient
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION AN2,C1,C2,D
C-----------------------------------------------------------------------
C     7816
C     1234
      C1=-TAB2*TAB3*(TBB1+TBB4)
      C1=C1-TAB1*TAB4*(TBB2+TBB3)
      C1=C1*(TAA2-TAB2)
      C2=TAB1*TAB1*TAB3
      C2=C2-TAB1*TAB2*TAB4
      C2=C2-TAB3*(TAA1+TAA2)*(TBB1+TBB4)
      C2=C2*(TBB2-TAB2)
      AN2=(C1+C2)/D
C
C-------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7VDCOFAN3(AN3,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D)
C
C     ******************************************************************
C     Calculate the AN3 coefficient
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION AN3,C1,C2,D
C-----------------------------------------------------------------------
C     7816
C     1234
      C1=(TAA1+TAA2)*(TBB1+TBB4)*(TBB2+TBB3)
      C1=C1-TAB2*TAB2*(TBB1+TBB4)
      C1=C1-TAB1*TAB1*(TBB2+TBB3)
      C1=C1*(TAA3+TAB3)
      C2=TAB1*TAB1*TAB3
      C2=C2-TAB1*TAB2*TAB4
      C2=C2-TAB3*(TAA1+TAA2)*(TBB1+TBB4)
      C2=C2*(TBB3+TAB3)
      AN3=(C1+C2)/D
C
C-------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7VDCOFAN4(AN4,TAA1,TAB1,TBB1,TAA2,TAB2,TBB2,
     &                          TAA3,TAB3,TBB3,TAA4,TAB4,TBB4,D)
C
C     ******************************************************************
C     Calculate the AN4 coefficient
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION AN4,C1,C2,D
C-----------------------------------------------------------------------
C     7816
C     1234
      C1=(TAA1+TAA2)*(TBB1+TBB4)*(TBB2+TBB3)
      C1=C1-TAB2*TAB2*(TBB1+TBB4)
      C1=C1-TAB1*TAB1*(TBB2+TBB3)
      C1=C1*(TAA4-TAB4)
      C2=TAB1*TAB2*TAB3
      C2=C2+TAB4*(TAA1+TAA2)*(TBB2+TBB3)
      C2=C2-TAB2*TAB2*TAB4
      C2=C2*(TBB4-TAB4)
      AN4=(C1+C2)/D
C
C-------RETURN
      RETURN
      END
c======================================================================
      SUBROUTINE GWF2HUF7DA(IGRID)
C
C     ******************************************************************
C     DEALLOCATE HUF DATA
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFHUFMODULE
C     ------------------------------------------------------------------
      DEALLOCATE(GWFHUFDAT(IGRID)%IHUFCB)
      DEALLOCATE(GWFHUFDAT(IGRID)%NHUF)
      DEALLOCATE(GWFHUFDAT(IGRID)%NPHUF)
      DEALLOCATE(GWFHUFDAT(IGRID)%IWETIT)
      DEALLOCATE(GWFHUFDAT(IGRID)%IHDWET)
      DEALLOCATE(GWFHUFDAT(IGRID)%IOHUFHDS)
      DEALLOCATE(GWFHUFDAT(IGRID)%IOHUFFLWS)
      DEALLOCATE(GWFHUFDAT(IGRID)%WETFCT)
      DEALLOCATE(GWFHUFDAT(IGRID)%HGUNAM)
      DEALLOCATE(GWFHUFDAT(IGRID)%LTHUF)
      DEALLOCATE(GWFHUFDAT(IGRID)%LAYWT)
      DEALLOCATE(GWFHUFDAT(IGRID)%IHGUFLG)
      DEALLOCATE(GWFHUFDAT(IGRID)%HGUHANI)
      DEALLOCATE(GWFHUFDAT(IGRID)%HGUVANI)
      DEALLOCATE(GWFHUFDAT(IGRID)%HUFHK)
      DEALLOCATE(GWFHUFDAT(IGRID)%HUFVK)
      DEALLOCATE(GWFHUFDAT(IGRID)%HUFSS)
      DEALLOCATE(GWFHUFDAT(IGRID)%HUFSY)
      DEALLOCATE(GWFHUFDAT(IGRID)%HUFKDEP)
      DEALLOCATE(GWFHUFDAT(IGRID)%GS)
      DEALLOCATE(GWFHUFDAT(IGRID)%VKAH)
      DEALLOCATE(GWFHUFDAT(IGRID)%SC1)
      DEALLOCATE(GWFHUFDAT(IGRID)%WETDRY)
      DEALLOCATE(GWFHUFDAT(IGRID)%HK)
      DEALLOCATE(GWFHUFDAT(IGRID)%HKCC)
      DEALLOCATE(GWFHUFDAT(IGRID)%HUFTMP)
      DEALLOCATE(GWFHUFDAT(IGRID)%VDHD)
      DEALLOCATE(GWFHUFDAT(IGRID)%HUFTHK)
      DEALLOCATE(GWFHUFDAT(IGRID)%VDHT)
      DEALLOCATE(GWFHUFDAT(IGRID)%A9)
      DEALLOCATE(GWFHUFDAT(IGRID)%SC2HUF)
C
      RETURN
      END
c======================================================================
      SUBROUTINE SGWF2HUF7PNT(IGRID)
C
C     ******************************************************************
C     POINT TO HUF DATA FOR A SPECIFIC GRID
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFHUFMODULE
C     ------------------------------------------------------------------
      IHUFCB=>GWFHUFDAT(IGRID)%IHUFCB
      NHUF=>GWFHUFDAT(IGRID)%NHUF
      NPHUF=>GWFHUFDAT(IGRID)%NPHUF
      IWETIT=>GWFHUFDAT(IGRID)%IWETIT
      IHDWET=>GWFHUFDAT(IGRID)%IHDWET
      IOHUFHDS=>GWFHUFDAT(IGRID)%IOHUFHDS
      IOHUFFLWS=>GWFHUFDAT(IGRID)%IOHUFFLWS
      WETFCT=>GWFHUFDAT(IGRID)%WETFCT
      HGUNAM=>GWFHUFDAT(IGRID)%HGUNAM
      LTHUF=>GWFHUFDAT(IGRID)%LTHUF
      LAYWT=>GWFHUFDAT(IGRID)%LAYWT
      IHGUFLG=>GWFHUFDAT(IGRID)%IHGUFLG
      HGUHANI=>GWFHUFDAT(IGRID)%HGUHANI
      HGUVANI=>GWFHUFDAT(IGRID)%HGUVANI
      HUFHK=>GWFHUFDAT(IGRID)%HUFHK
      HUFVK=>GWFHUFDAT(IGRID)%HUFVK
      HUFSS=>GWFHUFDAT(IGRID)%HUFSS
      HUFSY=>GWFHUFDAT(IGRID)%HUFSY
      HUFKDEP=>GWFHUFDAT(IGRID)%HUFKDEP
      GS=>GWFHUFDAT(IGRID)%GS
      VKAH=>GWFHUFDAT(IGRID)%VKAH
      SC1=>GWFHUFDAT(IGRID)%SC1
      WETDRY=>GWFHUFDAT(IGRID)%WETDRY
      HK=>GWFHUFDAT(IGRID)%HK
      HKCC=>GWFHUFDAT(IGRID)%HKCC
      HUFTMP=>GWFHUFDAT(IGRID)%HUFTMP
      VDHD=>GWFHUFDAT(IGRID)%VDHD
      HUFTHK=>GWFHUFDAT(IGRID)%HUFTHK
      VDHT=>GWFHUFDAT(IGRID)%VDHT
      A9=>GWFHUFDAT(IGRID)%A9
      SC2HUF=>GWFHUFDAT(IGRID)%SC2HUF     
C
      RETURN
      END
c======================================================================
      SUBROUTINE GWF2HUF7PSV(IGRID)
C
C     ******************************************************************
C     SAVE HUF DATA FOR A GRID
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWFHUFMODULE
C     ------------------------------------------------------------------
      GWFHUFDAT(IGRID)%IHUFCB=>IHUFCB
      GWFHUFDAT(IGRID)%NHUF=>NHUF
      GWFHUFDAT(IGRID)%NPHUF=>NPHUF
      GWFHUFDAT(IGRID)%IWETIT=>IWETIT
      GWFHUFDAT(IGRID)%IHDWET=>IHDWET
      GWFHUFDAT(IGRID)%IOHUFHDS=>IOHUFHDS
      GWFHUFDAT(IGRID)%IOHUFFLWS=>IOHUFFLWS
      GWFHUFDAT(IGRID)%WETFCT=>WETFCT
      GWFHUFDAT(IGRID)%HGUNAM=>HGUNAM
      GWFHUFDAT(IGRID)%LTHUF=>LTHUF
      GWFHUFDAT(IGRID)%LAYWT=>LAYWT
      GWFHUFDAT(IGRID)%IHGUFLG=>IHGUFLG
      GWFHUFDAT(IGRID)%HGUHANI=>HGUHANI
      GWFHUFDAT(IGRID)%HGUVANI=>HGUVANI
      GWFHUFDAT(IGRID)%HUFHK=>HUFHK
      GWFHUFDAT(IGRID)%HUFVK=>HUFVK
      GWFHUFDAT(IGRID)%HUFSS=>HUFSS
      GWFHUFDAT(IGRID)%HUFSY=>HUFSY
      GWFHUFDAT(IGRID)%HUFKDEP=>HUFKDEP
      GWFHUFDAT(IGRID)%GS=>GS
      GWFHUFDAT(IGRID)%VKAH=>VKAH
      GWFHUFDAT(IGRID)%SC1=>SC1
      GWFHUFDAT(IGRID)%WETDRY=>WETDRY
      GWFHUFDAT(IGRID)%HK=>HK
      GWFHUFDAT(IGRID)%HKCC=>HKCC
      GWFHUFDAT(IGRID)%HUFTMP=>HUFTMP
      GWFHUFDAT(IGRID)%VDHD=>VDHD
      GWFHUFDAT(IGRID)%HUFTHK=>HUFTHK
      GWFHUFDAT(IGRID)%VDHT=>VDHT
      GWFHUFDAT(IGRID)%A9=>A9
      GWFHUFDAT(IGRID)%SC2HUF=>SC2HUF
C
      RETURN
      END
