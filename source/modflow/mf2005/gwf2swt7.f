      MODULE GWFSWTMODULE
        INTEGER, SAVE,POINTER    ::ISWTCB,ISWTOC,NSYSTM
        INTEGER, SAVE,POINTER    ::ITHK,IVOID,ISTPCS,ICRCC
        INTEGER, SAVE,POINTER    ::IZCFL,IZCFM,IGLFL,IGLFM
        INTEGER, SAVE,POINTER    ::IESTFL,IESTFM,IPCSFL,IPCSFM
        INTEGER, SAVE,POINTER    ::ISTFL,ISTFM
        INTEGER, SAVE,    DIMENSION(:),     POINTER ::ISWOCF
        INTEGER, SAVE,    DIMENSION(:),     POINTER ::ISWOCU
        INTEGER, SAVE,    DIMENSION(:),     POINTER ::IFL2
        LOGICAL, SAVE,    DIMENSION(:),     POINTER ::OCLAY2
        INTEGER, SAVE,    DIMENSION(:),     POINTER ::LNWT
C Number of time steps before current stress time step? Nper
        INTEGER, SAVE,    DIMENSION(:),     POINTER ::NTSSM2
C Output control flags
        LOGICAL, SAVE,    DIMENSION(:,:),   POINTER ::OCFLG2
C specific gravity of saturated sediments
        REAL,    SAVE,    DIMENSION(:,:),   POINTER ::SGS
C specific gravity of moist sediments
        REAL,    SAVE,    DIMENSION(:,:),   POINTER ::SGM
C Preconsolidation stress
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::PCS
C Offset of initial preconsolidation stress from initial effective stress
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::PCSOFF
C Thickness of interbeds in saturated interval
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::THICK
C Recompression index
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::CE
C Compression index
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::CI
C Compaction
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::SUB
C Void ratio
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::VOID
C Effective stress
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::EST
C Effective stress for previous time step
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::ESTOLD
C Geostatic stress
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::GL
C Layer center
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::ZC
C Initial preconsolidation stress
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::PCS0
C Initial geostatic stress
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::GL0
C Initial effective stress
        REAL,    SAVE,    DIMENSION(:,:,:), POINTER ::EST0
C
      TYPE GWFSWTTYPE
        INTEGER,POINTER    ::ISWTCB,ISWTOC,NSYSTM
        INTEGER,POINTER    ::ITHK,IVOID,ISTPCS,ICRCC
        INTEGER,POINTER    ::IZCFL,IZCFM,IGLFL,IGLFM
        INTEGER,POINTER    ::IESTFL,IESTFM,IPCSFL,IPCSFM
        INTEGER,POINTER    ::ISTFL,ISTFM
        INTEGER,    DIMENSION(:),     POINTER ::ISWOCF
        INTEGER,    DIMENSION(:),     POINTER ::ISWOCU
        INTEGER,    DIMENSION(:),     POINTER ::IFL2
        LOGICAL,    DIMENSION(:),     POINTER ::OCLAY2
        INTEGER,    DIMENSION(:),     POINTER ::LNWT
        INTEGER,    DIMENSION(:),     POINTER ::NTSSM2
        LOGICAL,    DIMENSION(:,:),   POINTER ::OCFLG2
        REAL,       DIMENSION(:,:),   POINTER ::SGS
        REAL,       DIMENSION(:,:),   POINTER ::SGM
        REAL,       DIMENSION(:,:,:), POINTER ::PCS
        REAL,       DIMENSION(:,:,:), POINTER ::PCSOFF
        REAL,       DIMENSION(:,:,:), POINTER ::THICK
        REAL,       DIMENSION(:,:,:), POINTER ::CE
        REAL,       DIMENSION(:,:,:), POINTER ::CI
        REAL,       DIMENSION(:,:,:), POINTER ::SUB
        REAL,       DIMENSION(:,:,:), POINTER ::VOID
        REAL,       DIMENSION(:,:,:), POINTER ::EST
        REAL,       DIMENSION(:,:,:), POINTER ::ESTOLD
        REAL,       DIMENSION(:,:,:), POINTER ::GL
        REAL,       DIMENSION(:,:,:), POINTER ::ZC
        REAL,       DIMENSION(:,:,:), POINTER ::PCS0
        REAL,       DIMENSION(:,:,:), POINTER ::GL0
        REAL,       DIMENSION(:,:,:), POINTER ::EST0
      END TYPE
      TYPE(GWFSWTTYPE), SAVE:: GWFSWTDAT(10)
      END MODULE GWFSWTMODULE
C     
      SUBROUTINE GWF2SWT7AR(IN,IGRID)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR SUBSIDENCE-WATER TABLE AND READ AND
C     PREPARE DATA
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY: NCOL,NROW,NLAY,NPER,ISSFLG,NSTP,LAYCBD,
     1                       IBOUND,HNEW,BOTM,BUFF,DELR,DELC,IOUT,NCNFBD
      USE GWFSWTMODULE,ONLY: ISWTCB,ISWTOC,ITHK,IVOID,ICRCC,nsystm,
     1 NTSSM2,ISTPCS,LNWT,THICK,CE,CI,SUB,VOID,PCS,PCS0,PCSOFF,
     2 EST,EST0,ESTOLD,GL,GL0,ZC,SGM,SGS,OCFLG2,OCLAY2,
     3 IZCFL,IZCFM,IGLFL,IGLFM,IESTFL,IESTFM,IPCSFL,IPCSFM,ISTFL,ISTFM,
     4 ISWOCF,ISWOCU,IFL2
      CHARACTER*200 LINE
      CHARACTER*24 ANAME,TMPNAM
      CHARACTER*16 TEXT
      DIMENSION ANAME(13),TEXT(8)
      DATA ANAME(1) /' SILT AND CLAY THICKNESS'/
      DATA ANAME(2) /'ELASTIC SPECIFIC STORAGE'/
      DATA ANAME(3) /'INELAS. SPECIFIC STORAGE'/
      DATA ANAME(4) /'              VOID RATIO'/
      DATA ANAME(5) /'     PRECONSOL.   STRESS'/
      DATA ANAME(6) /'        GEOSTATIC STRESS'/
      DATA ANAME(7) /'   ELEV. OF LAYER CENTER'/
      DATA ANAME(8) /'     STARTING COMPACTION'/
      DATA ANAME(9) /'   ELEV. OF LAND SURFACE'/
      DATA ANAME(10)/'  MOIST SPECIFIC GRAVITY'/
      DATA ANAME(11)/'   SAT. SPECIFIC GRAVITY'/
      DATA ANAME(12)/'     RECOMPRESSION INDEX'/
      DATA ANAME(13)/'       COMPRESSION INDEX'/
      DATA TEXT(1) /'CENTER ELEVATION'/,
     2     TEXT(2) /'GEOSTATIC STRESS'/,
     3     TEXT(3) /'EFFECTIVE STRESS'/,
     4     TEXT(4) /'PRECONSOL STRESS'/,
     5     TEXT(5) /' EQUIVALENT Sske'/,
     6     TEXT(6) /' EQUIVALENT Sskv'/,
     6     TEXT(7) /'   EQUIVALENT Cr'/,
     6     TEXT(8) /'   EQUIVALENT Cc'/
C     ------------------------------------------------------------------
      ALLOCATE(ISWTCB,ISWTOC,NSYSTM,ITHK,IVOID,ISTPCS,ICRCC,IZCFL,IZCFM,
     &         IGLFL,IGLFM,IESTFL,IESTFM,IPCSFL,IPCSFM,ISTFL,ISTFM)
      ALLOCATE(ISWOCF(13),ISWOCU(13),IFL2(26))
C
C1------IDENTIFY PACKAGE.
      WRITE(IOUT,1)IN
    1 FORMAT('SWT -- SUBSIDENCE FOR WATER-TABLE PACKAGE, VERSION 1,',
     1     ' 05/26/06',' INPUT READ FROM UNIT',I3)
C
C2------CHECK TO SEE THAT SUBSIDENCE OPTION IS APPROPRIATE
C2------IF INAPPROPRIATE PRINT A MESSAGE & STOP THE SIMULATION.
C2------ALSO, SUM TO GET THE TOTAL NUMBER OF TIME STEPS IN THE
C2------SIMULATION.
C
      NSTPT=0
      DO NS=1,NPER
       NSTPT=NSTPT+NSTP(NS)
       IF(ISSFLG(NS).NE.0.AND.NS.GT.1) THEN
        WRITE(IOUT,10)
   10   FORMAT(1X,'SUBSIDENCE CANNOT BE USED IN SIMULATIONS',
     1  ' IN WHICH STRESS PERIODS OTHER THAN THE ',/,1X,
     2  ' FIRST ARE STEADY-STATE. SIMULATION ABORTED.')
        CALL USTOP(' ')
       ENDIF
      enddo
C
C3------Check that there are no quasi-3d confining beds
      IF(NCNFBD.GT.0) THEN
        WRITE(IOUT,45)
   45   FORMAT(' STOPPING -- QUASI-3D confining beds cannot ',/,
     &     'be used with SWT')
        CALL USTOP(' ')
      END IF
C
C ------ALLOCATE SPACE FOR ARRAY NTSSM2, WHICH WILL CONTAIN THE TOTAL
C ------NUMBER OF TIME STEPS PRIOR TO THE CURRENT TIME STEP.
      ALLOCATE(NTSSM2(NPER))
C
C4------READ FLAG FOR STORING CELL-BY-CELL STORAGE CHANGES AND
C4------FLAG FOR PRINTING AND STORING COMPACTION, SUBSIDENCE, AND
C4------CRITICAL HEAD ARRAYS.
      CALL URDCOM(IN,IOUT,LINE)
C     READ(IN,'(A)') LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISWTCB,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISWTOC,R,IOUT,IN)              !rw change from isuboc
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NSYSTM,R,IOUT,IN)
C      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IGL,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITHK,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IVOID,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,istpcs,R,IOUT,IN)              !sl added flag
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICRCC,R,IOUT,IN)              !sl added flag
C4------READ FLAG FOR STORING CELL-BY-CELL STORAGE CHANGES AND
C4------FLAG FOR PRINTING AND STORING COMPACTION, SUBSIDENCE, AND
C4------CRITICAL HEAD ARRAYS.
C
C5------IF CELL-BY-CELL TERMS TO BE SAVED THEN PRINT UNIT NUMBER.
      IF(ISWTCB.GT.0) WRITE(IOUT,105) ISWTCB
  105 FORMAT(1X,'CELL-BY-CELL FLOW TERMS WILL BE SAVED ON UNIT',I3)
C
C5A-----IF OUTPUT CONTROL FOR PRINTING ARRAYS IS SELECTED PRINT MESSAGE.
      IF(ISWTOC.GT.0) WRITE(IOUT,106) ISWTOC
  106 FORMAT(1X,I4,' OUTPUT CONTROL RECORDS FOR SUB-WT PACKAGE WILL',
     1 ' BE READ.')
C5A-----IF OUTPUT CONTROL FOR PRINTING ARRAYS IS SELECTED PRINT MESSAGE.
c      IF(ISWTOC.GT.0) WRITE(IOUT,107) ISWTOC
c  107 FORMAT(1X,I4,'OUTPUT CONTROL RECORDS FOR SWT PACKAGE WILL BE ',
c     1 'READ.')
c5b-----print number of interbed systems
      WRITE(IOUT,50) NSYSTM
   50 FORMAT(/,'      NUMBER OF SYSTEMS OF INTERBEDS FOR WT SUBSIDENCE:'
     1 ,I3)
C
C5c-----PRINT MESSAGE ON HOW GEOSTATIC LOAD IS TREATED.              !rw remove igl logic
C      WRITE(IOUT,107)
C 107  FORMAT(1X,'GEOSTATIC LOAD FOR IBS3 PACKAGE WILL BE ',
C    1 'READ WITH U2DREL.')
C
C6B-----PRINT A MESSAGE ON HOW THICKNESS IS TREATED
      IF(ITHK.LE.0) THEN
       WRITE(IOUT,111)
  111  FORMAT(1X,'THICKNESS OF INTERBEDS FOR SUB-WT PACKAGE WILL ',
     1 'BE TREATED AS A CONSTANT.')
C
      ELSE
       WRITE(IOUT,112)
  112  FORMAT(1X,'THICKNESS OF INTERBEDS FOR SUB-WT PACKAGE WILL ',
     1 'BE TREATED AS A FUNCTION OF SATURATED THICKNESS.')
      ENDIF
C
C
C6B-----PRINT A MESSAGE ON HOW VOID RATIO IS TREATED
      IF(IVOID.LE.0) THEN
       WRITE(IOUT,114)
  114  FORMAT(1X,'VOID RATIO FOR SUB-WT PACKAGE WILL BE ',
     1 'TREATED AS A CONSTANT.')
C
      ELSE
       WRITE(IOUT,115)
  115  FORMAT(1X,'VOID RATIO FOR SUB-WT PACKAGE WILL BE ',
     1 'TREATED AS A VARIABLE.')
      ENDIF
C
C
C6B-----PRINT A MESSAGE ON HOW INITIAL PRECONSOLIDATION STRESS IS
C       TREATED
      IF(ISTPCS.NE.0) THEN
       WRITE(IOUT,126)
  126  FORMAT(1X,'ARRAYS OF OFFSET VALUES WILL BE READ AND ADDED',
     1 ' TO INITIAL',/,' EFFECTIVE STRESS TO GET INITIAL ',
     2 'PRECONSOLIDATION STRESS.')
C
      ELSE
       WRITE(IOUT,127)
  127  FORMAT(1X,'ARRAYS OF PRECONSOLIDATION STRESS WILL BE READ.')
      ENDIF
C
C6_-----PRINT A MESSAGE ON HOW RECOMPRESSION AND COMPRESSION 
C       INDICES (Cr AND Cc) WILL BE OBTAINED
      IF(ICRCC.NE.0) THEN
       WRITE(IOUT,136)
  136  FORMAT(1X,'RECOMPRESSION AND COMPRESSION INDICES',
     1 ' WILL BE COMPUTED FROM',/,' INITIAL SPECIFIC ',
     2 'STORAGE, VOID RATIO, AND EFFECTIVE STRESS.')
C
      ELSE
       WRITE(IOUT,137)
  137  FORMAT(1X,'RECOMPRESSION AND COMPRESSION INDICES',
     1 ' WILL BE READ',/,' DIRECTLY INTO ARRAYS.')
      ENDIF
C
C ------ABORT IF NO LAYERS ARE SPECIFIED FOR INTERBED STORAGE
      IF(NSYSTM.LT.1) THEN
       WRITE(IOUT,60)
   60  FORMAT(1X,'NO LAYERS WITH INTERBED STORAGE ',
     1  'WERE SPECIFIED IN INPUT.',/,1X,'SIMULATION ABORTED.')
       CALL USTOP(' ')
      ENDIF
C ------READ IN MODEL LAYER NUMBERS FOR EACH SYSTEM OF INTERBEDS,
C ------FOR LAYERS WITHOUT DELAY.
       ALLOCATE(LNWT(NSYSTM))
       WRITE(IOUT,116) NSYSTM
  116  FORMAT(/,' MODEL LAYER ASSIGNMENTS FOR EACH OF',I3,' SUB-WT',
     1  ' SYSTEMS OF INTERBEDS:')
      CALL URDCOM(IN,IOUT,LINE)
       READ(LINE,*) (LNWT(N),N=1,NSYSTM)
       WRITE(IOUT,117) (LNWT(N),N=1,NSYSTM)
  117  FORMAT(1X,25I4)
       DO N=1,NSYSTM
       IF(LNWT(N).GE.1.AND.LNWT(N).LE.NLAY) CYCLE
       WRITE(IOUT,118)
  118  FORMAT(/,' IMPROPER LAYER ASSIGNMENT FOR SUB-WT SYSTEM OF ',
     1  'INTERBEDS.',/,' ABORTING...')
       CALL USTOP(' ')
       ENDDO
      DO K=1,NLAY
C ------MAKE SURE THERE ARE NO QUASI-3D CONFINING LAYERS
      IF(LAYCBD(K).NE.0) THEN
        WRITE(IOUT,121)
  121   FORMAT(' SUB-WT CANNOT BE USED IN CONJUNCTION WITH QUASI-3D ',
     &   'CONFINING UNITS.',/,' ABORTING...')
       CALL USTOP(' ')
      ENDIF
      ENDDO

C7------Check to see that there are no zero or negative layer
C7------thicknesses
      DO KQ=1,NSYSTM
      K=LNWT(KQ)
      DO I=1,NROW
      DO J=1,NCOL
       IF(IBOUND(J,I,K).LE.0) cycle
        TP=BOTM(J,I,K-1)
        BT=BOTM(J,I,K)
        THICK1=TP-BT
        IF(THICK1.LE.0.0) THEN
          WRITE(IOUT,44) I,J,K
   44     FORMAT(' STOPPING-- zero or negative layer thickness ',/,
     &   ' found at (row, column, layer):',3I5)
          WRITE(IOUT,*) ' Check layer elevation arrays in DIS input.'
         CALL USTOP(' ')
        ENDIF
      ENDDO
      ENDDO
      ENDDO
C
C8------ALLOCATE SPACE FOR THE ARRAYS.
      ALLOCATE(THICK(NCOL,NROW,NSYSTM))
      ALLOCATE(CE(NCOL,NROW,NSYSTM))
      ALLOCATE(CI(NCOL,NROW,NSYSTM))
      ALLOCATE(SUB(NCOL,NROW,NSYSTM))
      ALLOCATE(VOID(NCOL,NROW,NSYSTM))
      ALLOCATE(PCS(NCOL,NROW,NLAY))
      ALLOCATE(PCS0(NCOL,NROW,NLAY))
      if(istpcs.ne.0) then                              !sl added option
       ALLOCATE(PCSOFF(NCOL,NROW,NLAY))
      else
       ALLOCATE(PCSOFF(1,1,1))
      endif
      ALLOCATE(EST(NCOL,NROW,NLAY))
      ALLOCATE(EST0(NCOL,NROW,NLAY))
      ALLOCATE(ESTOld(NCOL,NROW,NLAY))                     !ESTOLD
      ALLOCATE(GL(NCOL,NROW,0:NLAY))                       !modify for gl above layer 1
      ALLOCATE(GL0(NCOL,NROW,0:NLAY))
      ALLOCATE(ZC(NCOL,NROW,NLAY))
      ALLOCATE(SGM(NCOL,NROW))
      ALLOCATE(SGS(NCOL,NROW))
      ALLOCATE(OCFLG2(26,NSTPT))
      ALLOCATE(OCLAY2(NLAY))
C
C     READ INTERBED STORAGE DATA
C
C
C ------INITIALIZE ARRAYS
      NIJ=NROW*NCOL
      DO N=1,NLAY
      DO I=1,NROW
      DO J=1,NCOL
      GL(J,I,N)=0.0
      EST(J,I,N)=0.0
      ESTOLD(J,I,N)=0.0
      ZC(J,I,N)=0.0
      enddo
      enddo
      enddo
C ------READ FLAGS AND FORMATS FOR PRINTING CALCULATED ARRAYS
      CALL URDCOM(IN,IOUT,LINE)
C      READ(IN,'(A)') LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IZCFL,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IZCFM,R,IOUT,IN)              
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IGLFL,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IGLFM,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IESTFL,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IESTFM,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPCSFL,R,IOUT,IN)              !sl new flag
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPCSFM,R,IOUT,IN)              !sl new fmt
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISTFL,R,IOUT,IN)              !sl new flag
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISTFM,R,IOUT,IN)              !sl new fmt
C
C1------READ IN ARRAYS WITH ONE VALUE FOR ALL LAYERS WITH INTERBED STORAGE
      CALL U2DREL(GL(:,:,0),ANAME(6),NROW,NCOL,1,IN,IOUT)    !change zls read to gl read
      CALL U2DREL( SGM,ANAME(10),NROW,NCOL,1,IN,IOUT)
      CALL U2DREL( SGS,ANAME(11),NROW,NCOL,1,IN,IOUT)
C3------READ IN ARRAYS FOR EACH LAYER WITH INTERBED STORAGE
      DO KQ=1,NSYSTM
      K=LNWT(KQ)
      CALL U2DREL(THICK(:,:,KQ),ANAME(1),NROW,NCOL,K,IN,IOUT)
      TMPNAM=ANAME(2)
      IF(ICRCC.EQ.0) TMPNAM=ANAME(12) 
      CALL U2DREL(   CE(:,:,KQ),TMPNAM,NROW,NCOL,K,IN,IOUT)
      TMPNAM=ANAME(3)
      IF(ICRCC.EQ.0) TMPNAM=ANAME(13) 
      CALL U2DREL(   CI(:,:,KQ),TMPNAM,NROW,NCOL,K,IN,IOUT)
      CALL U2DREL( VOID(:,:,KQ),ANAME(4),NROW,NCOL,K,IN,IOUT)
      CALL U2DREL(  SUB(:,:,KQ),ANAME(8),NROW,NCOL,K,IN,IOUT)
      enddo
      DO K=1,NLAY
      if(istpcs.ne.0) then
        CALL U2DREL(  PCSOFF(:,:,K),ANAME(5),NROW,NCOL,K,IN,IOUT)  !remove GL reads
      else
        CALL U2DREL(  PCS(:,:,K),ANAME(5),NROW,NCOL,K,IN,IOUT)   
      endif
      ENDDO
C
c     If the first stress period is steady state, delay
c     calculation of initial layer center, geostatic stress, effective
c     stress, and preconsolidation stress until after first stress
c     period is complete.
      IF(ISSFLG(1).NE.0) THEN
       write(iout,12)
   12  format(' Calculated arrays for SUBWT will be printed after
     & initial steady-state stress period.')    
      else
C ------COMPUTE LAYER CENTERS FOR ALL LAYERS
       CALL SSWT7Z(IBOUND,HNEW,botm,ZC,NROW,NCOL,NLAY)
C
C ------COMPUTE STARTING GEOSTATIC STRESS AND EFFECTIVE STRESS
C       IF(IGL.NE.0) THEN
        CALL SSWT7G(IBOUND,HNEW,BOTM,GL,
     1            SGM,SGS,NROW,NCOL,NLAY)
C       ENDIF
C ------compute effective stress
       CALL SSWT7E(IBOUND,HNEW,BOTM,GL,EST,NROW,NCOL,NLAY,IOUT)
C
C ------LOOP THROUGH ALL CELLS 
       DO K=1,NLAY
       DO IR=1,NROW
       DO JC=1,NCOL
       IF(ISTPCS.NE.0) PCS(JC,IR,K)=0.0                    
       IF(IBOUND(JC,IR,K).LE.0) CYCLE                               !sl changed eq to le
C ------Compute starting PRECONSOLIDATION STRESS from offset
C ------values and STARTING EFFECTIVE STRESS VALUES.
       IF(ISTPCS.NE.0) THEN
         PCS(JC,IR,K)=EST(JC,IR,K)+PCSOFF(JC,IR,K)                     !SL ADDED OPTION
       ELSE
C ------MAKE SURE THAT STARTING PRECONSOLIDATION STRESS VALUES
C ------ARE CONSISTENT WITH STARTING EFFECTIVE STRESS VALUES.
        IF (PCS(JC,IR,K).LT.EST(JC,IR,K)) PCS(JC,IR,K)=EST(JC,IR,K)
       ENDIF
C ------Set effective stress for previous step.   
       ESTOLD(jc,ir,k)=EST(jc,ir,k)
       enddo
       enddo
       enddo
C ------LOOP THROUGH ALL CELLS WITH INTERBED STORAGE.
       DO KQ=1,NSYSTM
       DO IR=1,NROW
       DO JC=1,NCOL
       K=LNWT(KQ)
       IF(IBOUND(JC,IR,K).LE.0) CYCLE                               !SL CHANGED EQ TO LE
C
C ------MULTIPLY SPECIFIC STORAGE BY AREA, 1+VOID RATIO,
C ------ AND EFFECTIVE STRESS
       IF(ICRCC.EQ.0) THEN
         FACT=DELR(JC)*DELC(IR)*0.4342942
       ELSE
        FACT=DELR(JC)*DELC(IR)*(1.+VOID(JC,IR,KQ))*
     1    (EST(JC,IR,K)-(ZC(JC,IR,K)-BOTM(JC,IR,K))*(SGS(JC,IR)-1.))
       ENDIF
       CE(JC,IR,KQ)=CE(JC,IR,KQ)*FACT
       CI(JC,IR,KQ)=CI(JC,IR,KQ)*FACT
       ENDDO
       ENDDO
       ENDDO
C ------SET INITIAL VALUES OF EFFECTIVE STRESS, PRECONSOLIDATION
C ------STRESS AND GEOSTATIC STRESS
       DO K=1,NLAY
       DO IR=1,NROW
       DO JC=1,NCOL
       EST0(JC,IR,K)=EST(JC,IR,K)
       PCS0(JC,IR,K)=PCS(JC,IR,K)
       GL0(JC,IR,K)=GL(JC,IR,K)
       ENDDO
       ENDDO
       ENDDO
C ------PRINT CALCULATED ARRAYS IF FLAGS ARE SET
       DO K=1,NLAY
       KK=K
       IF(IZCFL.GT.0) THEN
        WRITE(IOUT,222)
  222   FORMAT(/,' The following is a calculated (or recalculated) ',
     1    'SUB-WT array at the start of the simulation:')
        IF(IZCFM.LT.0) CALL ULAPRS(ZC(:,:,KK),TEXT(1),1,1,NCOL,
     1          NROW,KK,-IZCFM,IOUT)
        IF(IZCFM.GE.0) CALL ULAPRW(ZC(:,:,KK),TEXT(1),1,1,NCOL,
     1           NROW,KK,IZCFM,IOUT)
       ENDIF
       enddo
       DO K=1,NLAY
       KK=K
       IF(IGLFL.GT.0) THEN
        WRITE(IOUT,222)
        IF(IGLFM.LT.0) CALL ULAPRS(GL(:,:,KK),TEXT(2),1,1,
     1          NCOL,NROW,KK,-IGLFM,IOUT)
        IF(IGLFM.GE.0) CALL ULAPRW(GL(:,:,KK),TEXT(2),1,1,
     1           NCOL,NROW,KK,IGLFM,IOUT)
       ENDIF
       enddo
       DO K=1,NLAY
       KK=K
       IF(IESTFL.GT.0) THEN
        WRITE(IOUT,222)
        IF(IESTFM.LT.0) CALL ULAPRS(EST(:,:,KK),TEXT(3),1,1,
     1           NCOL,NROW,KK,-IESTFM,IOUT)
        IF(IESTFM.GE.0) CALL ULAPRW(EST(:,:,KK),TEXT(3),1,1,
     1           NCOL,NROW,KK,IESTFM,IOUT)
       ENDIF
       enddo
       DO K=1,NLAY
       KK=K
       IF(IPCSFL.GT.0) THEN
        WRITE(IOUT,222)
        IF(IPCSFM.LT.0) CALL ULAPRS(PCS(:,:,KK),TEXT(4),1,1,
     1           NCOL,NROW,KK,-IPCSFM,IOUT)
        IF(IPCSFM.GE.0) CALL ULAPRW(PCS(:,:,KK),TEXT(4),1,1,
     1           NCOL,NROW,KK,IPCSFM,IOUT)
       ENDIF
       enddo
C PRINT EQUIVALENT Sske AND Sskv OR Cr AND Cc
       IF(ISTFL.GT.0) THEN
       DO KQ=1,NSYSTM
       K=LNWT(KQ)
C COMPUTE EQUIVALENT ELASTIC PROPERTY (Cr OR Sske)
        IF(ICRCC.NE.0) THEN
         DO I=1,NROW
         DO J=1,NCOL
          IF(IBOUND(J,I,K).EQ.0) THEN
            BUFF(J,I,1)=0.0
          ELSE
            BUFF(J,I,1)=CE(J,I,KQ)/(DELR(J)*DELC(I)*0.4342942)
          ENDIF
        ENDDO
        ENDDO
        WRITE(IOUT,'(A,I4,A)') ' The following array is for system',
     2  KQ,' of compressible interbeds:'
        IF(ISTFM.LT.0) CALL ULAPRS(BUFF,TEXT(7),1,1,
     1           NCOL,NROW,K,-ISTFM,IOUT)
        IF(ISTFM.GE.0) CALL ULAPRW(BUFF,TEXT(7),1,1,
     1           NCOL,NROW,K,ISTFM,IOUT)
        ELSE
         DO I=1,NROW
         DO J=1,NCOL
          IF(IBOUND(J,I,K).EQ.0) THEN
            BUFF(J,I,1)=0.0
          ELSE
            BUFF(J,I,1)=
     2        CE(J,I,KQ)/(DELR(J)*DELC(I)*(EST(J,I,K)-(ZC(J,I,K)-
     1             botm(J,I,K))*(SGS(J,I)-1.))*(1.+VOID(J,I,kq)))
          ENDIF
         ENDDO
         ENDDO
        WRITE(IOUT,'(A,I4,A)') ' The following array is for system',
     2  KQ,' of compressible interbeds:'
        IF(ISTFM.LT.0) CALL ULAPRS(BUFF,TEXT(5),1,1,
     1           NCOL,NROW,K,-ISTFM,IOUT)
        IF(ISTFM.GE.0) CALL ULAPRW(BUFF,TEXT(5),1,1,
     1           NCOL,NROW,K,ISTFM,IOUT)
      ENDIF
C COMPUTE EQUIVALENT INELASTIC PROPERTY (Cc OR Sskv)
        IF(ICRCC.NE.0) THEN
         DO I=1,NROW
         DO J=1,NCOL
          IF(IBOUND(J,I,K).EQ.0) THEN
            BUFF(J,I,1)=0.0
          ELSE
            BUFF(J,I,1)=CI(J,I,KQ)/(DELR(J)*DELC(I)*0.4342942)
          ENDIF
         ENDDO
         ENDDO
        WRITE(IOUT,'(A,I4,A)') ' The following array is for system',
     2  KQ,' of compressible interbeds:'
        IF(ISTFM.LT.0) CALL ULAPRS(BUFF,TEXT(8),1,1,
     1           NCOL,NROW,K,-ISTFM,IOUT)
        IF(ISTFM.GE.0) CALL ULAPRW(BUFF,TEXT(8),1,1,
     1           NCOL,NROW,K,ISTFM,IOUT)
        ELSE
         DO I=1,NROW
         DO J=1,NCOL
          IF(IBOUND(J,I,K).EQ.0) THEN
            BUFF(J,I,1)=0.0
          ELSE
            BUFF(J,I,1)=
     2        CI(J,I,KQ)/(DELR(J)*DELC(I)*(EST(J,I,K)-(ZC(J,I,K)-
     3        botm(J,I,K))*(SGS(J,I)-1.))*(1.+VOID(J,I,kq)))
          ENDIF
         ENDDO
         ENDDO
        WRITE(IOUT,'(A,I4,A)') ' The following array is for system',
     2  KQ,' of compressible interbeds:'
        IF(ISTFM.LT.0) CALL ULAPRS(BUFF,TEXT(6),1,1,
     1           NCOL,NROW,K,-ISTFM,IOUT)
        IF(ISTFM.GE.0) CALL ULAPRW(BUFF,TEXT(6),1,1,
     1           NCOL,NROW,K,ISTFM,IOUT)
       ENDIF
       ENDDO
      ENDIF
      endif                                                        !sl end actions 1st period not ss
C ------INITIALIZE AND READ OUTPUT FLAGS.
C ------SET ALL FLAGS FOR OUTPUT CONTROL TO "FALSE".
      DO I=1,NSTPT
      DO N=1,26
      OCFLG2(N,I)=.FALSE.
      ENDDO
      ENDDO
C
C5------READ FORMATS AND UNIT NUMBERS OUTPUT FLAGS.
      IF(ISWTOC.GT.0) THEN
      CALL URDCOM(IN,IOUT,LINE)
C      READ(IN,'(A)') LINE
       LLOC=1
       DO N=1,13
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISWOCF(N),R,IOUT,IN)
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISWOCU(N),R,IOUT,IN)
       ENDDO
       WRITE(IOUT,310) (ISWOCF(N),ISWOCU(N),N=1,13)
  310  FORMAT(/,'             SUBSIDENCE PRINT FORMAT IS NUMBER',I4/
     &            '                 UNIT FOR SAVING SUBSIDENCE IS',I4/
     &            '    COMPACTION BY LAYER PRINT FORMAT IS NUMBER',I4/
     &            '        UNIT FOR SAVING COMPACTION BY LAYER IS',I4/
     &            '   COMPACTION BY SYSTEM PRINT FORMAT IS NUMBER',I4/
     &            '       UNIT FOR SAVING COMPACTION BY SYSTEM IS',I4/
     &            '  VERTICAL DISPLACEMENT PRINT FORMAT IS NUMBER',I4/
     &            '      UNIT FOR SAVING VERTICAL DISPLACEMENT IS',I4/
     &            'PRECONSOLIDATION STRESS PRINT FORMAT IS NUMBER',I4/
     &            '    UNIT FOR SAVING PRECONSOLIDATION STRESS IS',I4/
     &            'CHANGE IN PRECON STRESS PRINT FORMAT IS NUMBER',I4/
     &            ' UNIT FOR SAVING CHANGE IN PRECONSOL STRESS IS',I4/
     &            '       GEOSTATIC STRESS PRINT FORMAT IS NUMBER',I4/
     &            '           UNIT FOR SAVING GEOSTATIC STRESS IS',I4/
     &            'CHNGE IN GEOSTATIC STRS PRINT FORMAT IS NUMBER',I4/
     &            ' UNIT FOR SAVING CHANGE IN GEOSTATIC STRESS IS',I4/
     &            '       EFFECTIVE STRESS PRINT FORMAT IS NUMBER',I4/
     &            '           UNIT FOR SAVING EFFECTIVE STRESS IS',I4/
     &            '  CHANGE IN EFF. STRESS PRINT FORMAT IS NUMBER',I4/
     &            ' UNIT FOR SAVING CHANGE IN EFFECTIVE STRESS IS',I4/
     &            '             VOID RATIO PRINT FORMAT IS NUMBER',I4/
     &            '                 UNIT FOR SAVING VOID RATIO IS',I4/
     &            '              THICKNESS PRINT FORMAT IS NUMBER',I4/
     &            '                  UNIT FOR SAVING THICKNESS IS',I4/
     &            '       CENTER ELEVATION PRINT FORMAT IS NUMBER',I4/
     &            '           UNIT FOR SAVING CENTER ELEVATION IS',I4)
C     
       NTSSM2(1)=0
       IF(NPER.GT.1) THEN
        DO N=2,NPER
        NTSSM2(N)=NTSSM2(N-1)+NSTP(N-1)
       ENDDO
       ENDIF
       IOCR=0
       DO NOCLIN=1,ISWTOC
      CALL URDCOM(IN,IOUT,LINE)
C       READ(IN,'(A)',END=500) LINE
       IOCR=IOCR+1
       LLOC=1
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISP1,R,IOUT,IN)
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISP2,R,IOUT,IN)
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,JTS1,R,IOUT,IN)
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,JTS2,R,IOUT,IN)
       DO N=1,26
       CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFL2(N),R,IOUT,IN)
       ENDDO
C
       IF(ISP1.LT.1) ISP1=1
       IF(ISP1.GT.NPER) ISP1=NPER
       IF(ISP2.LT.1) ISP2=1
       IF(ISP2.GT.NPER) ISP2=NPER
       IF(ISP1.GT.ISP2) ISP1=ISP2
       DO I=ISP1,ISP2
       J1=JTS1
       J2=JTS2
       IF(J1.LT.1) J1=1
       IF(J1.GT.NSTP(I)) J1=NSTP(I)
       IF(J2.LT.1) J2=1
       IF(J2.GT.NSTP(I)) J2=NSTP(I)
       IF(J1.GT.J2) J1=J2
       DO J=J1,J2
       ILOC=NTSSM2(I)+J
       DO N=1,26
       IF(IFL2(N).GT.0) OCFLG2(N,ILOC)=.TRUE.
       IF(IFL2(N).EQ.0) OCFLG2(N,ILOC)=.FALSE.
       ENDDO
       ENDDO
       ENDDO
      ENDDO
      ENDIF
      GO TO 200
  500 WRITE(IOUT,502) IOCR,ISWTOC
  502  FORMAT(1X,'ONLY ',I4,' OUT OF ',I4,' OUTPUT CONTROL RECORDS ',
     1  'FOR SUB-WT WERE FOUND.',/,1X,'SIMULATION ABORTED.')
       CALL USTOP(' ')
C
C6------RETURN
  200 CALL SGWF2SWT7PSV(IGRID)
      RETURN
      END
      SUBROUTINE GWF2SWT7ST(KPER,IGRID)
C     ******************************************************************
C        Calculate layer centers, geostatic stress, effective
c     stress, and preconsolidation stress after an initial
c     steady-state stress period
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY: IBOUND,HNEW,BOTM,BUFF,DELR,DELC,ISSFLG,
     1                       NROW,NCOL,NLAY,NPER,IOUT
      USE GWFSWTMODULE,ONLY: ZC,GL,SGM,SGS,EST,ISTPCS,ICRCC,PCS,PCSOFF,
     2 ESTOLD,NSYSTM,LNWT,VOID,CE,CI,EST0,PCS0,GL0,
     3 IZCFL,IZCFM,IGLFL,IGLFM,IESTFL,IESTFM,IPCSFL,IPCSFM,ISTFL,ISTFM
      CHARACTER(16) TEXT
      DIMENSION TEXT(8)
      DATA TEXT(1) /'CENTER ELEVATION'/,
     2     TEXT(2) /'GEOSTATIC STRESS'/,
     3     TEXT(3) /'EFFECTIVE STRESS'/,
     3     TEXT(4) /'PRECONSOL STRESS'/
     5     TEXT(5) /' EQUIVALENT Sske'/,
     6     TEXT(6) /' EQUIVALENT Sskv'/,
     7     TEXT(7) /'   EQUIVALENT Cr'/,
     8     TEXT(8) /'   EQUIVALENT Cc'/
C
C     ------------------------------------------------------------------
      CALL SGWF2SWT7PNT(IGRID)
C
C1------RETURN IF THIS IS NOT THE SECOND STRESS PERIOD OR IF THE FIRST
C1------STRESS PERIOD WAS TRANSIENT.
      IF(KPER.NE.2) RETURN
      IF(ISSFLG(1).EQ.0) RETURN
C ------COMPUTE LAYER CENTERS FOR ALL LAYERS
       CALL SSWT7Z(IBOUND,HNEW,botm,ZC,NROW,NCOL,NLAY)
C
C ------COMPUTE STARTING GEOSTATIC STRESS AND EFFECTIVE STRESS
        CALL SSWT7G(IBOUND,HNEW,BOTM,GL,
     1            SGM,SGS,NROW,NCOL,NLAY)
C ------compute effective stress
       CALL SSWT7E(IBOUND,HNEW,BOTM,GL,EST,NROW,NCOL,NLAY,IOUT)
C
C ------LOOP THROUGH ALL CELLS 
       do k=1,nlay
       do ir=1,nrow
       do jc=1,ncol
       if(istpcs.ne.0) pcs(jc,ir,k)=0.0                    
       IF(IBOUND(jc,ir,k).le.0) cycle                               !sl changed eq to le
C ------Compute starting PRECONSOLIDATION STRESS from offset
C ------values and STARTING EFFECTIVE STRESS VALUES.
       if(istpcs.ne.0) then
         pcs(jc,ir,k)=est(jc,ir,k)+PCSOFF(jc,ir,k)                     !sl added option
       else
C ------MAKE SURE THAT STARTING PRECONSOLIDATION STRESS VALUES
C ------ARE CONSISTENT WITH STARTING EFFECTIVE STRESS VALUES.
        IF (PCS(jc,ir,k).LT.EST(jc,ir,k)) PCS(jc,ir,k)=EST(jc,ir,k)
       endif
C ------Set effective stress for previous step.   
       ESTOLD(jc,ir,k)=EST(jc,ir,k)
       enddo
       enddo
       enddo
C ------LOOP THROUGH ALL CELLS WITH INTERBED STORAGE.
       do kq=1,nsystm
       do ir=1,nrow
       do jc=1,ncol
       k=lnwt(kq)
       IF(IBOUND(jc,ir,k).LE.0) cycle                               !sl changed eq to le
C
C ------MULTIPLY SPECIFIC STORAGE BY AREA, 1+VOID RATIO,
C ------ AND EFFECTIVE STRESS
       IF(ICRCC.EQ.0) THEN
         FACT=DELR(JC)*DELC(IR)*0.4342942
       ELSE
        FACT=DELR(JC)*DELC(IR)*(1.+VOID(jc,ir,kq))*(EST(jc,ir,k)-
     1         (ZC(jc,ir,k)-botm(jc,ir,k))*(SGS(jc,ir)-1.))
       ENDIF
       CE(jc,ir,kq)=CE(jc,ir,kq)*FACT
       CI(jc,ir,kq)=CI(jc,ir,kq)*FACT
       enddo
       enddo
       enddo
C ------SET INITIAL VALUES OF EFFECTIVE STRESS, PRECONSOLIDATION
C ------STRESS AND GEOSTATIC STRESS
       do k=1,nlay
       do ir=1,nrow
       do jc=1,ncol
       EST0(jc,ir,k)=EST(jc,ir,k)
       PCS0(jc,ir,k)=PCS(jc,ir,k)
       GL0(jc,ir,k)=GL(jc,ir,k)
       enddo
       enddo
       enddo
C ------PRINT CALCULATED ARRAYS IF FLAGS ARE SET
       DO K=1,NLAY
       KK=K
       IF(IZCFL.GT.0) THEN
        WRITE(IOUT,222)
  222   FORMAT(/,' The following is a calculated (or recalculated) ',
     1    'SUB-WT array after the initial steady-state stress period:')
        IF(IZCFM.LT.0) CALL ULAPRS(ZC(:,:,KK),TEXT(1),1,1,NCOL,
     1          NROW,KK,-IZCFM,IOUT)
        IF(IZCFM.GE.0) CALL ULAPRW(ZC(:,:,KK),TEXT(1),1,1,NCOL,
     1           NROW,KK,IZCFM,IOUT)
       ENDIF
       ENDDO
       DO K=1,NLAY
       KK=K
       IF(IGLFL.GT.0) THEN
        WRITE(IOUT,222)
        IF(IGLFM.LT.0) CALL ULAPRS(GL(:,:,KK),TEXT(2),1,1,
     1          NCOL,NROW,KK,-IGLFM,IOUT)
        IF(IGLFM.GE.0) CALL ULAPRW(GL(:,:,KK),TEXT(2),1,1,
     1           NCOL,NROW,KK,IGLFM,IOUT)
       ENDIF
       ENDDO
       DO K=1,NLAY
       KK=K
       IF(IESTFL.GT.0) THEN
        WRITE(IOUT,222)
        IF(IESTFM.LT.0) CALL ULAPRS(EST(:,:,KK),TEXT(3),1,1,
     1           NCOL,NROW,KK,-IESTFM,IOUT)
        IF(IESTFM.GE.0) CALL ULAPRW(EST(:,:,KK),TEXT(3),1,1,
     1           NCOL,NROW,KK,IESTFM,IOUT)
       ENDIF
       ENDDO
       DO K=1,NLAY
       KK=K
       IF(IPCSFL.GT.0) THEN
        WRITE(IOUT,222)
        IF(IPCSFM.LT.0) CALL ULAPRS(PCS(:,:,KK),TEXT(4),1,1,
     1           NCOL,NROW,KK,-IPCSFM,IOUT)
        IF(IPCSFM.GE.0) CALL ULAPRW(PCS(:,:,KK),TEXT(4),1,1,
     1           NCOL,NROW,KK,IPCSFM,IOUT)
       ENDIF
       ENDDO
C PRINT EQUIVALENT Sske AND Sskv OR Cr AND Cc
       IF(ISTFL.GT.0) THEN
       DO KQ=1,NSYSTM
       K=LNWT(KQ)
C COMPUTE EQUIVALENT ELASTIC PROPERTY (Cr OR Sske)
        IF(ICRCC.NE.0) THEN
         DO I=1,NROW
         DO J=1,NCOL
          IF(IBOUND(J,I,K).EQ.0) THEN
            BUFF(J,I,1)=0.0
          ELSE
            BUFF(J,I,1)=CE(J,I,KQ)/(DELR(J)*DELC(I)*0.4342942)
          ENDIF
         ENDDO
         ENDDO
        WRITE(IOUT,'(A,I4,A)') ' The following array is for system',
     2  KQ,' of compressible interbeds:'
        IF(ISTFM.LT.0) CALL ULAPRS(BUFF,TEXT(7),1,1,
     1           NCOL,NROW,K,-ISTFM,IOUT)
        IF(ISTFM.GE.0) CALL ULAPRW(BUFF,TEXT(7),1,1,
     1           NCOL,NROW,K,ISTFM,IOUT)
        ELSE
         DO I=1,NROW
         DO J=1,NCOL
          IF(IBOUND(J,I,K).EQ.0) THEN
            BUFF(J,I,1)=0.0
          ELSE
            BUFF(J,I,1)=
     2        CE(J,I,KQ)/(DELR(J)*DELC(I)*(EST(J,I,K)-(ZC(J,I,K)-
     3       botm(J,I,K))*(SGS(J,I)-1.))*(1.+VOID(J,I,kq)))
          ENDIF
         ENDDO
         ENDDO
        WRITE(IOUT,'(A,I4,A)') ' The following array is for system',
     2  KQ,' of compressible interbeds:'
        IF(ISTFM.LT.0) CALL ULAPRS(BUFF,TEXT(5),1,1,
     1           NCOL,NROW,K,-ISTFM,IOUT)
        IF(ISTFM.GE.0) CALL ULAPRW(BUFF,TEXT(5),1,1,
     1           NCOL,NROW,K,ISTFM,IOUT)
        ENDIF
C COMPUTE EQUIVALENT INELASTIC PROPERTY (Cc OR Sskv)
        IF(ICRCC.NE.0) THEN
         DO I=1,NROW
         DO J=1,NCOL
          IF(IBOUND(J,I,K).EQ.0) THEN
            BUFF(J,I,1)=0.0
          ELSE
            BUFF(J,I,1)=CI(J,I,KQ)/(DELR(J)*DELC(I)*0.4342942)
          ENDIF
         ENDDO
         ENDDO
        WRITE(IOUT,'(A,I4,A)') ' The following array is for system',
     2  KQ,' of compressible interbeds:'
        IF(ISTFM.LT.0) CALL ULAPRS(BUFF,TEXT(8),1,1,
     1           NCOL,NROW,K,-ISTFM,IOUT)
        IF(ISTFM.GE.0) CALL ULAPRW(BUFF,TEXT(8),1,1,
     1           NCOL,NROW,K,ISTFM,IOUT)
        ELSE
         DO I=1,NROW
         DO J=1,NCOL
          IF(IBOUND(J,I,K).EQ.0) THEN
            BUFF(J,I,1)=0.0
          ELSE
            BUFF(J,I,1)=
     2        CI(J,I,KQ)/(DELR(J)*DELC(I)*(EST(J,I,K)-(ZC(J,I,K)-
     3       botm(J,I,K))*(SGS(J,I)-1.))*(1.+VOID(J,I,kq)))
          ENDIF
         ENDDO
        ENDDO
        WRITE(IOUT,'(A,I4,A)') ' The following array is for system',
     2  KQ,' of compressible interbeds:'
        IF(ISTFM.LT.0) CALL ULAPRS(BUFF,TEXT(6),1,1,
     1           NCOL,NROW,K,-ISTFM,IOUT)
        IF(ISTFM.GE.0) CALL ULAPRW(BUFF,TEXT(6),1,1,
     1           NCOL,NROW,K,ISTFM,IOUT)
       ENDIF
       ENDDO
       ENDIF
C4-----RETURN.
      RETURN
      END
c
      SUBROUTINE GWF2SWT7FM(KPER,IGRID)
C     ******************************************************************
C        ADD INTERBED STORAGE TO RHS AND HCOF
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY: RHS,HCOF,IBOUND,HNEW,HOLD,BOTM,NCOL,NROW,
     1                       NLAY,ISSFLG,IOUT
      USE GWFBASMODULE,ONLY: DELT
      USE GWFSWTMODULE,ONLY: NSYSTM,THICK,CE,CI,
     1                 VOID,PCS,GL,ZC,EST,ESTOLD,SGM,SGS,ITHK,LNWT
      DOUBLE PRECISION RHO1,RHO2,GLN,ZCN,PCTMP,ESTN1,ESTN
C     ------------------------------------------------------------------
      CALL SGWF2SWT7PNT(IGRID)
C
C0------SKIP CALCULATIONS IF THIS IS A STEADY-STATE STRESS PERIOD.
      IF(ISSFLG(KPER).EQ.1) RETURN
C
C1------INITIALIZE
       TLED=1./DELT
C
C ------UPDATE LAYER CENTERS FOR LAYERS
      CALL SSWT7Z(IBOUND,HNEW,botm,ZC,NROW,NCOL,NLAY)
C
C4------UPDATE GEOSTATIC STRESS
       CALL SSWT7G(IBOUND,HNEW,BOTM,GL,
     1            SGM,SGS,NROW,NCOL,NLAY)
C
C2------ADD CONTRIBUTIONS FROM STORAGE CHANGES TO HCOF AND RHS
C2------FOR EACH SYSTEM OF INTERBEDS
      DO KQ=1,NSYSTM
        K=LNWT(KQ)
      DO I=1,NROW
      DO J=1,NCOL
      IF(IBOUND(J,I,K).LE.0) CYCLE
C
C3------DETERMINE STORAGE CAPACITIES FOR CELL AT START AND END OF STEP
      HHNEW=HNEW(J,I,K)
C
C3A-----FIND THICKNESS OF INTERBEDS IN SATURATED INTERVAL
      TFACT=1.0
      IF(ITHK.GT.0) THEN
        TP=BOTM(J,I,K-1)
        BT=BOTM(J,I,K)
        TOPNEW=HHNEW
        THICK1=TP-BT
C3B-----FIRST FIND TOP OF SATURATED THICKNESS
         IF(TOPNEW.GT.TP) TOPNEW=TP
C3C-----COMPUTE CORRECTION FACTOR AS RATIO OF CURRENT TO PAST SATURATED
C3C-----THICKNESS
        TFACT=(TOPNEW-BT)/THICK1
      ENDIF
      GLN=GL(J,I,K)
      ZCN=BOTM(J,I,K)
      ESTN=GLN-HNEW(J,I,K)+ZCN
      ESTN1=ESTOLD(J,I,K)
      FACT=TFACT*THICK(J,I,KQ)*TLED/((1.+VOID(J,I,KQ))*
     1   (ESTOLD(J,I,K)-(ZC(J,I,K)-botm(J,I,K))*(SGS(J,I)-1.)))
      RHO1=CE(J,I,KQ)*FACT
      RHO2=RHO1
      PCTMP=PCS(J,I,K)
      IF(ESTN.GT.PCTMP) RHO2=CI(J,I,KQ)*FACT
C
C4------ADD APPROPRIATE TERMS TO RHS AND HCOF
      CRHS=-RHO2*(GLN+ZCN)+PCTMP*(RHO2-RHO1)+RHO1*ESTN1
      RHS(J,I,K)=RHS(J,I,K)+CRHS
      CDIAG=-RHO2
      HCOF(J,I,K)=HCOF(J,I,K)+CDIAG
      ENDDO
      ENDDO
      ENDDO
C
C5------RETURN
      RETURN
      END
      SUBROUTINE GWF2SWT7BD(KSTP,KPER,IGRID)
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR INTERBED STORAGE
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY: BUFF,IBOUND,HNEW,HOLD,BOTM,DELR,DELC,
     1                       NCOL,NROW,NLAY,ISSFLG,IOUT
      USE GWFBASMODULE,ONLY: DELT,VBVL,VBNM,MSUM,ICBCFL
      USE GWFSWTMODULE,ONLY: ISWTCB,NSYSTM,THICK,CE,CI,SUB,IVOID,
     1                 VOID,PCS,GL,ZC,EST,ESTOLD,SGM,SGS,ITHK,LNWT
      CHARACTER*16 TEXT
      DOUBLE PRECISION RHO1,RHO2,GLN,ZCN,PCTMP,ESTN1,ESTN
C
      DATA TEXT /'INTERBED STORAGE'/
C     ------------------------------------------------------------------
      CALL SGWF2SWT7PNT(IGRID)
C
C1------INITIALIZE CELL-BY-CELL FLOW TERM FLAG (IBD) AND
C1------ACCUMULATORS (STOIN AND STOUT).
      IBD=0
      STOIN=0.
      STOUT=0.
C
C2------TEST TO SEE IF CELL-BY-CELL FLOW TERMS ARE NEEDED.
      IF(ICBCFL.NE.0  .AND. ISWTCB.GT.0 ) IBD=1
C
C ------IF THIS IS A STEADY-STATE STRESS PERIOD, SKIP CALCULATIONS
      TLED=0.0
      IF(ISSFLG(KPER).EQ.1) GO TO 111
C
C3------CELL-BY-CELL FLOW TERMS ARE NEEDED SET IBD AND CLEAR BUFFER.
      DO IL=1,NLAY
      DO IR=1,NROW
      DO IC=1,NCOL
      BUFF(IC,IR,IL)=0.
      ENDDO
      ENDDO
      ENDDO
      TLED=1./DELT
      DO KQ=1,NSYSTM
        K=LNWT(KQ)
C
C4------RUN THROUGH EVERY CELL IN THE GRID WITH INTERBED STORAGE.
      DO I=1,NROW
      DO J=1,NCOL
C
C5------CALCULATE FLOW FROM STORAGE (VARIABLE HEAD CELLS ONLY)
      IF(IBOUND(J,I,K).LE.0) CYCLE
C
C3------DETERMINE STORAGE CAPACITIES FOR CELL AT START AND END OF STEP
      HHNEW=HNEW(J,I,K)
C
C3A-----FIND CORRECTION FOR THICKNESS OF INTERBEDS FOR END OF TIME STEP
      TFACT=1.0
      IF(ITHK.GT.0) THEN
        TP=BOTM(J,I,K-1)
        BT=BOTM(J,I,K)
        TOPNEW=HHNEW
        THICK1=TP-BT
C3B-----FIRST FIND TOP OF SATURATED THICKNESS
         IF(TOPNEW.GT.TP) TOPNEW=TP
C3C-----COMPUTE CORRECTION FACTOR AS RATIO OF CURRENT TO PAST SATURATED
C3C-----THICKNESS
        TFACT=(TOPNEW-BT)/THICK1
      ENDIF
      GLN=GL(J,I,K)
      ZCN=BOTM(J,I,K)
      ESTN=GLN-HNEW(J,I,K)+ZCN
      ESTN1=ESTOLD(J,I,K)
      IF(ITHK.GT.0) THEN
       TF1=TFACT
      ELSE
       TF1=1.0
      ENDIF
      FACT=TF1*THICK(J,I,KQ)/((1.+VOID(J,I,KQ))*
     1  (ESTOLD(J,I,K)-(ZC(J,I,K)-botm(J,I,K))*(SGS(J,I)-1.)))
      RHO1=CE(J,I,KQ)*FACT
      RHO2=RHO1
      PCTMP=PCS(J,I,K)
      IF(ESTN.GT.PCTMP) RHO2=CI(J,I,KQ)*FACT
C
C7------CALCULATE VOLUME CHANGE IN INTERBED STORAGE FOR TIME STEP.
      STRG=-PCTMP*(RHO2-RHO1)-RHO1*ESTN1+RHO2*ESTN
C
C8------ACCUMULATE SUBSIDENCE ASSOCIATED WITH CHANGE IN STORAGE
      DELB=STRG/(DELR(J)*DELC(I))
      SUB(J,I,KQ)=SUB(J,I,KQ)+DELB
C
C ------UPDATE VOID RATIO AND THICKNESS ARRAYS
      IF(IVOID.GT.0) THEN
       IF(THICK(J,I,KQ).GT.0.0) THEN
        STRAIN=-DELB/THICK(J,I,KQ)
       ELSE
        STRAIN=0.0
       ENDIF
       VOID(J,I,KQ)=STRAIN+VOID(J,I,KQ)*(STRAIN+1.)
       THICK(J,I,KQ)=THICK(J,I,KQ)*(STRAIN+1.)
      ENDIF
C
C9------IF C-B-C FLOW TERMS ARE TO BE SAVED THEN ADD RATE TO BUFFER.
      IF(IBD.EQ.1) BUFF(J,I,K)=BUFF(J,I,K)+STRG*TLED
C
C10-----SEE IF FLOW IS INTO OR OUT OF STORAGE.
      IF(STRG.LT.0.0) THEN
       STOUT=STOUT-STRG
      ELSE
       STOIN=STOIN+STRG
      ENDIF
      ENDDO
      ENDDO
      ENDDO
C
C11-----IF C-B-C FLOW TERMS WILL BE SAVED CALL UBUDSV TO RECORD THEM.
  111 IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,ISWTCB,BUFF,NCOL,NROW,
     1                          NLAY,IOUT)
C
C12-----MOVE RATES,VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
      VBVL(3,MSUM)=STOIN*TLED
      VBVL(4,MSUM)=STOUT*TLED
      VBVL(1,MSUM)=VBVL(1,MSUM)+STOIN
      VBVL(2,MSUM)=VBVL(2,MSUM)+STOUT
      VBNM(MSUM)=TEXT
C
C13-----INCREMENT BUDGET TERM COUNTER
      MSUM=MSUM+1
C
C ------UPDATE LAYER CENTERS FOR LAYERS WITH BOTTOM SPECIFIED
      CALL SSWT7Z(IBOUND,HNEW,botm,ZC,NROW,NCOL,NLAY)
C
C4------UPDATE GEOSTATIC STRESS AND EFFECTIVE STRESS
       CALL SSWT7G(IBOUND,HNEW,BOTM,GL,
     1            SGM,SGS,NROW,NCOL,NLAY)
      CALL SSWT7E(IBOUND,HNEW,BOTM,GL,EST,NROW,NCOL,NLAY,IOUT)
C
C14-----UPDATE PRECONSOLIDATION HEAD AND OLD EFFECTIVE STRESS ARRAYS
      DO K=1,NLAY
      DO I=1,NROW
      DO J=1,NCOL
      IF(IBOUND(J,I,K).LE.0) CYCLE
      IF(EST(J,I,K).GT.PCS(J,I,K)) PCS(J,I,K)=EST(J,I,K)
      ESTOLD(J,I,K)=EST(J,I,K)
      ENDDO
      ENDDO
      ENDDO
C
C15-----RETURN
      RETURN
      END
      SUBROUTINE GWF2SWT7OT(KSTP,KPER,IGRID)
C     ******************************************************************
C     PRINT AND STORE SUBSIDENCE, COMPACTION AND CRITICAL HEAD.
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY: NCOL,NROW,NLAY,NSTP,BUFF,IOUT
      USE GWFBASMODULE,ONLY: PERTIM,TOTIM
      USE GWFSWTMODULE,ONLY: NSYSTM,THICK,SUB,VOID,PCS,GL,ZC,EST,LNWT,
     1                 OCFLG2,ISWOCF,ISWOCU,OCLAY2,NTSSM2,PCS0,GL0,EST0
      CHARACTER*16 TEXT
      DIMENSION TEXT(13)
      DATA TEXT(1)  /'      SUBSIDENCE'/,
     2     TEXT(2)  /'LAYER COMPACTION'/,
     3     TEXT(3)  /'SYSTM COMPACTION'/,
     3     TEXT(4)  /'  Z DISPLACEMENT'/,
     3     TEXT(5)  /'PRECONSOL STRESS'/,
     3     TEXT(6)  /'CHANGE IN PCSTRS'/,
     4     TEXT(7)  /'GEOSTATIC STRESS'/,
     4     TEXT(8)  /'CHANGE IN G-STRS'/,
     5     TEXT(9)  /'EFFECTIVE STRESS'/,
     5     TEXT(10) /'CHANGE IN EFF-ST'/,
     6     TEXT(11) /'      VOID RATIO'/,
     7     TEXT(12) /'       THICKNESS'/,
     8     TEXT(13) /'CENTER ELEVATION'/
C     ------------------------------------------------------------------
      CALL SGWF2SWT7PNT(IGRID)
C
C1------INITIALIZE TIME STEP POINTER TO RETRIEVE FLAGS FOR PRINTING AND
C1------SAVING ARRAYS.
      NNSTP=NTSSM2(KPER)+KSTP
C
C3------PRINT AND STORE SUBSIDENCE, FIRST, CLEAR OUT BUFF.
      IF(OCFLG2(1,NNSTP).OR.OCFLG2(2,NNSTP)) THEN
       DO I=1,NROW
       DO J=1,NCOL
       BUFF(J,I,1)=0.
       enddo
       enddo
C
C4------SUM COMPACTION IN ALL LAYERS TO GET SUBSIDENCE.
      KQ=0
      DO KQ=1,NSYSTM
      DO I=1,NROW
      DO J=1,NCOL
      BUFF(J,I,1)=BUFF(J,I,1)+SUB(J,I,KQ)
      ENDDO
      ENDDO
      ENDDO
C
C5-------PRINT SUBSIDENCE.
       IF(OCFLG2(1,NNSTP)) THEN
        WRITE(IOUT,'(2A)') ' The following subsidence array is the sum',
     1   ' of compaction values for all systems of interbeds:'
        IF(ISWOCF(1).LT.0) CALL ULAPRS(BUFF,TEXT(1),KSTP,KPER,NCOL,
     1            NROW,1,-ISWOCF(1),IOUT)
        IF(ISWOCF(1).GE.0) CALL ULAPRW(BUFF,TEXT(1),KSTP,KPER,NCOL,
     1             NROW,1,ISWOCF(1),IOUT)
       ENDIF
C
C6-------STORE SUBSIDENCE.
       IF(OCFLG2(2,NNSTP)) THEN
        CALL ULASAV(BUFF,TEXT(1),KSTP,KPER,PERTIM,TOTIM,NCOL,NROW,1,
     1              ISWOCU(1))
       ENDIF
C
C7------PRINT AND STORE COMPACTION FOR EACH SYSTEM OF INTERBEDS.
      IF(OCFLG2(5,NNSTP).OR.OCFLG2(6,NNSTP)) THEN
        DO KQ=1,NSYSTM
        K=LNWT(KQ)
        IF(OCFLG2(5,NNSTP)) THEN
       WRITE(IOUT,76) KQ
  76   FORMAT(/,1X,' SYSTEM',I4,' OF SUBWT INTERBEDS:')
        IF(ISWOCF(3).LT.0) CALL ULAPRS(SUB(:,:,KQ),TEXT(3),KSTP,KPER,
     1            NCOL,NROW,K,-ISWOCF(3),IOUT)
        IF(ISWOCF(3).GE.0) CALL ULAPRW(SUB(:,:,KQ),TEXT(3),KSTP,KPER,
     1             NCOL,NROW,K,ISWOCF(3),IOUT)
        ENDIF
        IF(OCFLG2(6,NNSTP)) THEN
         CALL ULASAV(SUB(:,:,KQ),TEXT(3),KSTP,KPER,PERTIM,TOTIM,NCOL,
     1              NROW,KQ,ISWOCU(3))
        ENDIF
        ENDDO
      ENDIF
      ENDIF
C
C ------SUM COMPACTION IN EACH LAYER IN THE BUFF ARRAY FOR SAVING
C ------OR PRINTING COMPACTION OR VERTICAL DISPLACEMENT BY MODEL
C ------LAYER. FIRST, CLEAR OUT BUFF.
      IF(OCFLG2(3,NNSTP).OR.OCFLG2(4,NNSTP).OR.
     &OCFLG2(7,NNSTP).OR.OCFLG2(8,NNSTP)) THEN
       DO NL=1,NLAY
       OCLAY2(NL)=.FALSE.
       enddo
       DO K=1,NLAY
       DO I=1,NROW
       DO J=1,NCOL
       BUFF(J,I,K)=0.
       enddo
       enddo
       enddo
   
C -------SUM COMPACTION IN ALL MODEL LAYERS.
       DO KQ=1,NSYSTM
       K=LNWT(KQ)
       OCLAY2(K)=.TRUE.
       DO I=1,NROW
       DO J=1,NCOL
        BUFF(J,I,K)=BUFF(J,I,K)+SUB(J,I,KQ)
       enddo
       enddo
       enddo
C
C -------PRINT COMPACTION BY LAYER.
       IF(OCFLG2(3,NNSTP)) THEN
        DO KL=1,NLAY
        IF(.NOT.OCLAY2(KL)) cycle
        KKL=KL
        IF(ISWOCF(2).LT.0) CALL ULAPRS(BUFF(:,:,KKL),TEXT(2),KSTP,KPER,
     1           NCOL,NROW,KKL,-ISWOCF(2),IOUT)
        IF(ISWOCF(2).GE.0) CALL ULAPRW(BUFF(:,:,KKL),TEXT(2),KSTP,KPER,
     1            NCOL,NROW,KKL,ISWOCF(2),IOUT)
        enddo
       ENDIF
C
C -------STORE COMPACTION BY LAYER.
       IF(OCFLG2(4,NNSTP)) THEN
        DO KL=1,NLAY
        IF(.NOT.OCLAY2(KL)) cycle                       !sl consider removing this
        KKL=KL
        CALL ULASAV(BUFF(:,:,KKL),TEXT(2),KSTP,KPER,PERTIM,TOTIM,NCOL,
     1             NROW,KKL,ISWOCU(2))
        enddo
       ENDIF
C
C ------CALCULATE VERTICAL DISPLACEMENT.
       IF(OCFLG2(7,NNSTP).OR.OCFLG2(8,NNSTP)) THEN
        NL1=NLAY-1
        IF(NLAY.GT.1) THEN
         DO KL=NL1,1,-1
           KL1=KL+1
         DO I=1,NROW
         DO J=1,NCOL
           BUFF(J,I,KL)=BUFF(J,I,KL)+BUFF(J,I,KL1)
         enddo
         enddo
         enddo
        ENDIF
C ------PRINT VERTICAL DISPLACEMENT FOR ALL MODEL LAYERS.
        IF(OCFLG2(7,NNSTP)) THEN
         DO KL=1,NLAY
         KKL=KL
         IF(ISWOCF(4).LT.0) CALL ULAPRS(BUFF(:,:,KKL),TEXT(4),KSTP,KPER,
     1            NCOL,NROW,KKL,-ISWOCF(4),IOUT)
         IF(ISWOCF(4).GE.0) CALL ULAPRW(BUFF(:,:,KKL),TEXT(4),KSTP,KPER,
     1             NCOL,NROW,KKL,ISWOCF(4),IOUT)
         ENDDO
        ENDIF
C
C ------SAVE VERTICAL DISPLACEMENT FOR ALL MODEL LAYERS.
        IF(OCFLG2(8,NNSTP)) THEN
         DO KL=1,NLAY
         KKL=KL
         CALL ULASAV(BUFF(:,:,KKL),TEXT(4),KSTP,KPER,PERTIM,TOTIM,NCOL,
     1              NROW,KKL,ISWOCU(4))
         ENDDO
        ENDIF
       ENDIF
      ENDIF
C
C ------PRINT AND SAVE PRECONSOLIDATION STRESS
      IF(OCFLG2(9,NNSTP).OR.OCFLG2(10,NNSTP)) THEN
       DO KL=1,NLAY
         KKL=KL
        IF(OCFLG2(9,NNSTP)) THEN
         IF(ISWOCF(5).LT.0) CALL ULAPRS(PCS(:,:,KKL),TEXT(5),KSTP,KPER,
     1          NCOL,NROW,KKL,-ISWOCF(5),IOUT)
         IF(ISWOCF(5).GE.0) CALL ULAPRW(PCS(:,:,KKL),TEXT(5),KSTP,KPER,
     1        NCOL,NROW,KKL,ISWOCF(5),IOUT)
        ENDIF
        IF(OCFLG2(10,NNSTP)) THEN
         CALL ULASAV(PCS(:,:,KKL),TEXT(5),KSTP,KPER,PERTIM,TOTIM,NCOL,
     1          NROW,KKL,ISWOCU(5))
        ENDIF
       enddo
      ENDIF
C
C ------PRINT AND SAVE CHANGE IN PRECONSOLIDATION STRESS
      IF(OCFLG2(11,NNSTP).OR.OCFLG2(12,NNSTP)) THEN
       DO K=1,NLAY
       DO I=1,NROW
       DO J=1,NCOL
         BUFF(J,I,K)=PCS(J,I,K)-PCS0(J,I,K)
       enddo
       enddo
       enddo
       DO KL=1,NLAY
         KKL=KL
       IF(OCFLG2(11,NNSTP)) THEN
        IF(ISWOCF(6).LT.0) CALL ULAPRS(BUFF(:,:,KKL),TEXT(6),KSTP,KPER,
     1          NCOL,NROW,KKL,-ISWOCF(6),IOUT)
        IF(ISWOCF(6).GE.0) CALL ULAPRW(BUFF(:,:,KKL),TEXT(6),KSTP,KPER,
     1      NCOL, NROW,KKL,ISWOCF(6),IOUT)
        ENDIF
       IF(OCFLG2(12,NNSTP)) THEN
        CALL ULASAV(BUFF(:,:,KKL),TEXT(6),KSTP,KPER,PERTIM,TOTIM,NCOL,
     1          NROW,KKL,ISWOCU(6))
       ENDIF
       enddo
      ENDIF
C ------PRINT AND SAVE GEOSTATIC STRESS
      IF(OCFLG2(13,NNSTP).OR.OCFLG2(14,NNSTP)) THEN
       DO KL=1,NLAY
         KKL=KL
       IF(OCFLG2(13,NNSTP)) THEN
        IF(ISWOCF(7).LT.0) CALL ULAPRS(GL(:,:,KKL),TEXT(7),KSTP,KPER,
     1          NCOL,NROW,KKL,-ISWOCF(7),IOUT)
        IF(ISWOCF(7).GE.0) CALL ULAPRW(GL(:,:,KKL),TEXT(7),KSTP,KPER,
     1      NCOL, NROW,KKL,ISWOCF(7),IOUT)
       ENDIF
       IF(OCFLG2(14,NNSTP)) THEN
        CALL ULASAV(GL(:,:,KKL),TEXT(7),KSTP,KPER,PERTIM,TOTIM,NCOL,
     1          NROW,KKL,ISWOCU(7))
       ENDIF
       enddo
       ENDIF
C
C ------PRINT AND SAVE CHANGE IN GEOSTATIC STRESS
      IF(OCFLG2(15,NNSTP).OR.OCFLG2(16,NNSTP)) THEN
       DO K=1,NLAY
       DO I=1,NROW
       DO J=1,NCOL
         BUFF(J,I,K)=GL(J,I,K)-GL0(J,I,K)
       enddo
       enddo
       enddo
       DO KL=1,NLAY
         KKL=KL
       IF(OCFLG2(15,NNSTP)) THEN
        IF(ISWOCF(8).LT.0) CALL ULAPRS(BUFF(:,:,KKL),TEXT(8),KSTP,KPER,
     1          NCOL,NROW,KKL,-ISWOCF(8),IOUT)
        IF(ISWOCF(8).GE.0) CALL ULAPRW(BUFF(:,:,KKL),TEXT(8),KSTP,KPER,
     1      NCOL, NROW,KKL,ISWOCF(8),IOUT)
       ENDIF
       IF(OCFLG2(16,NNSTP)) THEN
        CALL ULASAV(BUFF(:,:,KKL),TEXT(8),KSTP,KPER,PERTIM,TOTIM,NCOL,
     1          NROW,KKL,ISWOCU(8))
       ENDIF
      ENDDO
      ENDIF
C ------PRINT AND SAVE EFFECTIVE STRESS
      IF(OCFLG2(17,NNSTP).OR.OCFLG2(18,NNSTP)) THEN
       DO KL=1,NLAY
         KKL=KL
       IF(OCFLG2(17,NNSTP)) THEN
        IF(ISWOCF(9).LT.0) CALL ULAPRS(EST(:,:,KKL),TEXT(9),KSTP,KPER,
     1          NCOL,NROW,KKL,-ISWOCF(9),IOUT)
        IF(ISWOCF(9).GE.0) CALL ULAPRW(EST(:,:,KKL),TEXT(9),KSTP,KPER,
     1      NCOL, NROW,KKL,ISWOCF(9),IOUT)
       ENDIF
      IF(OCFLG2(18,NNSTP)) THEN
       CALL ULASAV(EST(:,:,KKL),TEXT(9),KSTP,KPER,PERTIM,TOTIM,NCOL,
     1          NROW,KKL,ISWOCU(9))
       ENDIF
       ENDDO
      ENDIF
C
C ------PRINT AND SAVE CHANGE IN EFFECTIVE STRESS
      IF(OCFLG2(19,NNSTP).OR.OCFLG2(20,NNSTP)) THEN
      DO K=1,NLAY
      DO I=1,NROW
      DO J=1,NCOL
        BUFF(J,I,K)=EST(J,I,K)-EST0(J,I,K)
      ENDDO
      ENDDO
      ENDDO
       DO KL=1,NLAY
         KKL=KL
       IF(OCFLG2(19,NNSTP)) THEN
        IF(ISWOCF(10).LT.0) CALL ULAPRS(BUFF(:,:,KKL),TEXT(10),KSTP,
     1          KPER,NCOL,NROW,KKL,-ISWOCF(10),IOUT)
        IF(ISWOCF(10).GE.0) CALL ULAPRW(BUFF(:,:,KKL),TEXT(10),KSTP,
     1      KPER,NCOL, NROW,KKL,ISWOCF(10),IOUT)
        ENDIF
       IF(OCFLG2(20,NNSTP)) THEN
        CALL ULASAV(BUFF(:,:,KKL),TEXT(10),KSTP,KPER,PERTIM,TOTIM,NCOL,
     1          NROW,KKL,ISWOCU(10))
       ENDIF
       ENDDO
      ENDIF
C7------PRINT AND STORE VOID RATIO FOR EACH SYSTEM OF INTERBEDS.
      IF(OCFLG2(21,NNSTP).OR.OCFLG2(22,NNSTP)) THEN
       DO KQ=1,NSYSTM
       K=LNWT(KQ)
       IF(OCFLG2(21,NNSTP)) THEN
        WRITE(IOUT,76) KQ
        IF(ISWOCF(11).LT.0) CALL ULAPRS(VOID(:,:,KQ),TEXT(11),KSTP,KPER,
     1            NCOL,NROW,K,-ISWOCF(11),IOUT)
        IF(ISWOCF(11).GE.0) CALL ULAPRW(VOID(:,:,KQ),TEXT(11),KSTP,KPER,
     1             NCOL,NROW,K,ISWOCF(11),IOUT)
       ENDIF
       IF(OCFLG2(22,NNSTP)) THEN
        CALL ULASAV(VOID(:,:,KQ),TEXT(11),KSTP,KPER,PERTIM,TOTIM,NCOL,
     1              NROW,KQ,ISWOCU(11))
       ENDIF
       ENDDO
      ENDIF
C7------PRINT AND STORE THICKNESS FOR EACH SYSTEM OF INTERBEDS.
      IF(OCFLG2(23,NNSTP).OR.OCFLG2(24,NNSTP)) THEN
       DO KQ=1,NSYSTM
       K=LNWT(KQ)
       IF(OCFLG2(23,NNSTP)) THEN
        WRITE(IOUT,76) KQ
        IF(ISWOCF(12).LT.0) CALL ULAPRS(THICK(:,:,KQ),TEXT(12),KSTP,
     1            KPER,NCOL,NROW,K,-ISWOCF(12),IOUT)
        IF(ISWOCF(12).GE.0) CALL ULAPRW(THICK(:,:,KQ),TEXT(12),KSTP,
     1             KPER,NCOL,NROW,K,ISWOCF(12),IOUT)
       ENDIF
       IF(OCFLG2(24,NNSTP)) THEN
        CALL ULASAV(THICK(:,:,KQ),TEXT(12),KSTP,KPER,PERTIM,TOTIM,NCOL,
     1              NROW,KQ,ISWOCU(12))
       ENDIF
       ENDDO
      ENDIF
C.... ZELEVATION FOLLOWS:
C7------PRINT AND STORE LAYER-CENTER ELEVATION FOR EACH LAYER.
      IF(OCFLG2(25,NNSTP).OR.OCFLG2(26,NNSTP)) THEN
       DO KL=1,NLAY
         KKL=KL
       IF(OCFLG2(25,NNSTP)) THEN
        IF(ISWOCF(13).LT.0) CALL ULAPRS(ZC(:,:,KKL),TEXT(13),KSTP,
     1            KPER,NCOL,NROW,KKL,-ISWOCF(13),IOUT)
        IF(ISWOCF(13).GE.0) CALL ULAPRW(ZC(:,:,KKL),TEXT(13),KSTP,
     1             KPER,NCOL,NROW,KKL,ISWOCF(13),IOUT)
       ENDIF
       IF(OCFLG2(26,NNSTP)) THEN
        CALL ULASAV(ZC(:,:,KKL),TEXT(13),KSTP,KPER,PERTIM,TOTIM,NCOL,
     1              NROW,KKL,ISWOCU(13))
       ENDIF
      ENDDO
      ENDIF
C  -----RETURN
  900 RETURN
      END
      SUBROUTINE SSWT7Z(IBOUND,HNEW,botm,ZC,NROW,NCOL,NLAY)
C     ******************************************************************
C     COMPUTE LAYER CENTER ELEVATION
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION HNEW
      LOGICAL TTOP,TBOT
      DIMENSION IBOUND(NCOL,NROW,NLAY),HNEW(NCOL,NROW,NLAY),
     1          BOTM(NCOL,NROW,0:Nlay),ZC(NCOL,NROW,NLAY)
C
      DO K=1,NLAY
      DO IR=1,NROW
      DO IC=1,NCOL
      HHNEW=HNEW(IC,IR,K)
      ZB=BOTM(IC,IR,K)
      ZT=BOTM(IC,IR,K-1)
      IF(IBOUND(IC,IR,K).EQ.0) THEN
       ZC(IC,IR,K)=(ZT+ZB)*0.5
       cycle
      endif
C
C ------COMPUTE CENTER ELEVATION AS MIDPOINT BETWEEN BOTTOM AND
C ------HEAD ELEVATION
      IF(HHNEW.LT.ZT.and.hhnew.gt.zb) THEN
c ------wt in cell
       ZC(IC,IR,K)=(HHNEW+ZB)*0.5
       cycle
      else
c ------wt is above or below cell
       ZC(IC,IR,K)=(ZT+ZB)*0.5
      endif
      enddo
      enddo
      enddo
C
C ------RETURN
      RETURN
      END
      SUBROUTINE SSWT7G(IBOUND,HNEW,BOTM,GL,SGM,SGS,NROW,NCOL,NLAY)
C     ******************************************************************
C     COMPUTE GEOSTATIC STRESS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION HNEW
      DIMENSION GL(NCOL,NROW,0:NLAY),
     1          SGM(NCOL,NROW),SGS(NCOL,NROW),IBOUND(NCOL,NROW,NLAY),
     3          HNEW(NCOL,NROW,NLAY),BOTM(NCOL,NROW,0:Nlay)
C
C
C
      do ir=1,nrow
      do jc=1,ncol
c If a vertical column of cells contains no active cells (IBOUND=0), it 
c is out of the flow domain and we do not need to worry about geostatic
c stress. However, cells with IBOUND=0 above an active cell must be considered
c as "unsaturated." This could be cells for which IBOUND was originally 
c zero and cells that went dry during the simulation I don't think it matters
c for our calculations here because MODFLOW updates IBOUND when cells saturate
c or desaturate. Cells with IBOUND=0 below active cells likely are out of the
c aquifer. Note that when IBOUND=0, hnew is not a meaningful number for
c calculations. Constant head cells have an IBOUND < 0. I guess we should 
c consider these as saturated with the constant-head value indicating the
c elevation of the water level. This would be the same treatment as
c IBOUND > 0.
      do k=1,nlay
      if(ibound(jc,ir,k).ne.0) then
       h=hnew(jc,ir,k)
       isat=1
      else
        h=0.0
        isat=0
      endif
c     if cell fully unsaturated
c bhl      if (h.lt.botm(jc,ir,k).or.isat.eq.0) then
      if (h.le.botm(jc,ir,k).or.isat.eq.0) then
        gl(jc,ir,k)=gl(jc,ir,k-1)+
     1      (botm(jc,ir,k-1)-botm(jc,ir,k))*sgm(jc,ir)
        cycle
      endif
c     if cell fully saturated
c bhl      if (h.gt.botm(jc,ir,k-1)) then
      if (h.ge.botm(jc,ir,k-1)) then
        gl(jc,ir,k)=gl(jc,ir,k-1)+
     1  (botm(jc,ir,k-1)-botm(jc,ir,k))*sgs(jc,ir)
        cycle
      endif
c     if cell partially saturated
      if (h.lt.botm(jc,ir,k-1).and.h.gt.botm(jc,ir,k)) then
        gl(jc,ir,k)=gl(jc,ir,k-1)+
     1  (botm(jc,ir,k-1)-h)*sgm(jc,ir)+
     2  (h-botm(jc,ir,k))*sgs(jc,ir)
        cycle
      endif
      enddo
      enddo
      enddo
C ------RETURN
      RETURN
      END
      SUBROUTINE SSWT7E(IBOUND,HNEW,BOTM,GL,EST,NROW,NCOL,NLAY,IOUT)
C     ******************************************************************
C     COMPUTE EFFECTIVE STRESS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION HNEW
      DIMENSION EST(NCOL,NROW,NLAY),GL(NCOL,NROW,0:NLAY),
     1          IBOUND(NCOL,NROW,NLAY),
     2          HNEW(NCOL,NROW,NLAY),BOTM(NCOL,NROW,0:Nlay)
C
      DO K=1,NLAY
      DO IR=1,NROW
      DO IC=1,NCOL
      EST(IC,IR,K)=0.0
      IF(IBOUND(IC,IR,K).EQ.0) CYCLE
      HHNEW=HNEW(IC,IR,K)
      EST(IC,IR,K)=GL(IC,IR,K)-HHNEW+BOTM(IC,IR,K)
      IF(EST(IC,IR,K).LT.0.0) THEN
       WRITE(IOUT,5) IR,IC,K
    5  FORMAT(' NEGATIVE EFFECTIVE STRESS VALUE AT (ROW,COL,LAY):',
     $ 3I5,/,'   ABORTING...')
      CALL USTOP('')
      ENDIF
      ENDDO
      ENDDO
      ENDDO
C
C ------RETURN
      RETURN
      END
      SUBROUTINE GWF2SWT7DA(IGRID)
C  Deallocate SWT MEMORY
      USE GWFSWTMODULE
      CALL SGWF2SWT7PNT(IGRID)
      DEALLOCATE(ISWTCB,ISWTOC,NSYSTM,ITHK,IVOID,ISTPCS,ICRCC,IZCFL,
     &     IZCFM,IGLFL,IGLFM,IESTFL,IESTFM,IPCSFL,IPCSFM,ISTFL,ISTFM)
      DEALLOCATE(ISWOCF,ISWOCU,IFL2)
      DEALLOCATE(OCLAY2,LNWT,NTSSM2,OCFLG2,SGS,SGM,PCS,PCSOFF,THICK,
     &     CE,CI,SUB,VOID,EST,ESTOLD,GL,ZC,PCS0,GL0,EST0)
C
      RETURN
      END
      SUBROUTINE SGWF2SWT7PSV(IGRID)
C  Save SWT data for a  grid.
      USE GWFSWTMODULE
C
        GWFSWTDAT(IGRID)%ISWTCB=>ISWTCB
        GWFSWTDAT(IGRID)%ISWTOC=>ISWTOC
        GWFSWTDAT(IGRID)%NSYSTM=>NSYSTM
        GWFSWTDAT(IGRID)%ITHK=>ITHK
        GWFSWTDAT(IGRID)%IVOID=>IVOID
        GWFSWTDAT(IGRID)%ISTPCS=>ISTPCS
        GWFSWTDAT(IGRID)%ICRCC=>ICRCC
        GWFSWTDAT(IGRID)%IZCFL=>IZCFL
        GWFSWTDAT(IGRID)%IZCFM=>IZCFM
        GWFSWTDAT(IGRID)%IGLFL=>IGLFL
        GWFSWTDAT(IGRID)%IGLFM=>IGLFM
        GWFSWTDAT(IGRID)%IESTFL=>IESTFL
        GWFSWTDAT(IGRID)%IESTFM=>IESTFM
        GWFSWTDAT(IGRID)%IPCSFL=>IPCSFL
        GWFSWTDAT(IGRID)%IPCSFM=>IPCSFM
        GWFSWTDAT(IGRID)%ISTFL=>ISTFL
        GWFSWTDAT(IGRID)%ISTFM=>ISTFM
        GWFSWTDAT(IGRID)%ISWOCF=>ISWOCF
        GWFSWTDAT(IGRID)%ISWOCU=>ISWOCU
        GWFSWTDAT(IGRID)%IFL2=>IFL2
        GWFSWTDAT(IGRID)%OCLAY2=>OCLAY2
        GWFSWTDAT(IGRID)%LNWT=>LNWT
        GWFSWTDAT(IGRID)%NTSSM2=>NTSSM2
        GWFSWTDAT(IGRID)%OCFLG2=>OCFLG2
        GWFSWTDAT(IGRID)%SGS=>SGS
        GWFSWTDAT(IGRID)%SGM=>SGM
        GWFSWTDAT(IGRID)%PCS=>PCS
        GWFSWTDAT(IGRID)%PCSOFF=>PCSOFF
        GWFSWTDAT(IGRID)%THICK=>THICK
        GWFSWTDAT(IGRID)%CE=>CE
        GWFSWTDAT(IGRID)%CI=>CI
        GWFSWTDAT(IGRID)%SUB=>SUB
        GWFSWTDAT(IGRID)%VOID=>VOID
        GWFSWTDAT(IGRID)%EST=>EST
        GWFSWTDAT(IGRID)%ESTOLD=>ESTOLD
        GWFSWTDAT(IGRID)%GL=>GL
        GWFSWTDAT(IGRID)%ZC=>ZC
        GWFSWTDAT(IGRID)%PCS0=>PCS0
        GWFSWTDAT(IGRID)%GL0=>GL0
        GWFSWTDAT(IGRID)%EST0=>EST0
C
      RETURN
      END
      SUBROUTINE SGWF2SWT7PNT(IGRID)
C  Change SWT data to a different grid.
      USE GWFSWTMODULE
C
        ISWTCB=>GWFSWTDAT(IGRID)%ISWTCB
        ISWTOC=>GWFSWTDAT(IGRID)%ISWTOC
        NSYSTM=>GWFSWTDAT(IGRID)%NSYSTM
        ITHK=>GWFSWTDAT(IGRID)%ITHK
        IVOID=>GWFSWTDAT(IGRID)%IVOID
        ISTPCS=>GWFSWTDAT(IGRID)%ISTPCS
        ICRCC=>GWFSWTDAT(IGRID)%ICRCC
        IZCFL=>GWFSWTDAT(IGRID)%IZCFL
        IZCFM=>GWFSWTDAT(IGRID)%IZCFM
        IGLFL=>GWFSWTDAT(IGRID)%IGLFL
        IGLFM=>GWFSWTDAT(IGRID)%IGLFM
        IESTFL=>GWFSWTDAT(IGRID)%IESTFL
        IESTFM=>GWFSWTDAT(IGRID)%IESTFM
        IPCSFL=>GWFSWTDAT(IGRID)%IPCSFL
        IPCSFM=>GWFSWTDAT(IGRID)%IPCSFM
        ISTFL=>GWFSWTDAT(IGRID)%ISTFL
        ISTFM=>GWFSWTDAT(IGRID)%ISTFM
        ISWOCF=>GWFSWTDAT(IGRID)%ISWOCF
        ISWOCU=>GWFSWTDAT(IGRID)%ISWOCU
        IFL2=>GWFSWTDAT(IGRID)%IFL2
        OCLAY2=>GWFSWTDAT(IGRID)%OCLAY2
        LNWT=>GWFSWTDAT(IGRID)%LNWT
        NTSSM2=>GWFSWTDAT(IGRID)%NTSSM2
        OCFLG2=>GWFSWTDAT(IGRID)%OCFLG2
        SGS=>GWFSWTDAT(IGRID)%SGS
        SGM=>GWFSWTDAT(IGRID)%SGM
        PCS=>GWFSWTDAT(IGRID)%PCS
        PCSOFF=>GWFSWTDAT(IGRID)%PCSOFF
        THICK=>GWFSWTDAT(IGRID)%THICK
        CE=>GWFSWTDAT(IGRID)%CE
        CI=>GWFSWTDAT(IGRID)%CI
        SUB=>GWFSWTDAT(IGRID)%SUB
        VOID=>GWFSWTDAT(IGRID)%VOID
        EST=>GWFSWTDAT(IGRID)%EST
        ESTOLD=>GWFSWTDAT(IGRID)%ESTOLD
        GL=>GWFSWTDAT(IGRID)%GL
        ZC=>GWFSWTDAT(IGRID)%ZC
        PCS0=>GWFSWTDAT(IGRID)%PCS0
        GL0=>GWFSWTDAT(IGRID)%GL0
        EST0=>GWFSWTDAT(IGRID)%EST0
C
      RETURN
      END
