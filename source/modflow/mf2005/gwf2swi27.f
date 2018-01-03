      MODULE GWFSWIMODULE
        TYPE TSWIDE4
          INTEGER  :: MXITER,NODES,NHALFU,NHALFL,NBWGRD
          INTEGER  :: MXUP,MXLOW,MXEQ,MXBW,ITMX,ID4DIR
          INTEGER  :: NITERDE4,IFREQ,ID4DIM
          INTEGER  :: NBWL,NUPL,NLOWL,NLOW,NEQ,NUP,NBW
          REAL     :: ACCLDE4,HCLOSEDE4,DELTL
          INTEGER,          ALLOCATABLE, DIMENSION(:,:)   :: IUPPNT
          INTEGER,          ALLOCATABLE, DIMENSION(:,:,:) :: IEQPNT
          REAL,             ALLOCATABLE, DIMENSION(:,:)   :: AU
          REAL,             ALLOCATABLE, DIMENSION(:,:)   :: AL
          REAL,             ALLOCATABLE, DIMENSION(:)     :: D4B
          REAL,             ALLOCATABLE, DIMENSION(:)     :: HDCGDE4
          INTEGER,          ALLOCATABLE, DIMENSION(:,:)   :: LRCHDE4
        END TYPE TSWIDE4

        TYPE TSWIPCG
          INTEGER :: MXITER,NODES
          INTEGER :: ITER1,NPCOND,NBPOL,NITER
          REAL    :: ZCLOSEPCG,RCLOSEPCG,RELAXPCG,DAMPPCG
          REAL    :: DAMPPCGT
          INTEGER :: IHCOFADD = 1
          DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:,:) :: VPCG
          DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:,:) :: SS
          DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:,:) :: P
          DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:,:) :: HPCG
          REAL,             ALLOCATABLE, DIMENSION(:,:,:) :: CD
          REAL,             ALLOCATABLE, DIMENSION(:,:,:) :: HCSV
          INTEGER,          ALLOCATABLE, DIMENSION(:,:)   :: LHCH
          REAL,             ALLOCATABLE, DIMENSION(:)     :: HCHG
          INTEGER,          ALLOCATABLE, DIMENSION(:,:)   :: LRCHPCG
          REAL,             ALLOCATABLE, DIMENSION(:)     :: RCHG
          INTEGER,          ALLOCATABLE, DIMENSION(:)     :: IT1
        END TYPE TSWIPCG

        TYPE TSWIOBS
          CHARACTER (LEN=12) :: OBSNAM
          INTEGER :: KLAY
          INTEGER :: IROW
          INTEGER :: JCOL
        END TYPE TSWIOBS
C
C--------------------------------------------------------------
C         COMMENT OUT FOR MODFLOW-NWT        
C         SINGLE PRECISION FOR SELECT ARRAYS WITH MODFLOW-2005
        INTEGER, PARAMETER :: VERSIZE = 4
!C         COMMENT OUT FOR MODFLOW-2005        
!C         SINGLE PRECISION FOR SELECT ARRAYS WITH MODFLOW-NWT
!        INTEGER, PARAMETER :: VERSIZE = 8
C--------------------------------------------------------------
C
C         SWI PARAMETERS
        REAL, PARAMETER :: SWISMALL = 0.001
        REAL, PARAMETER :: SWILOCK = 0.001
C         SWI DIMENSIONS
        INTEGER, SAVE, POINTER :: NSRF,ISTRAT,NSWIOPT,NZONES
        INTEGER, SAVE, POINTER :: IFIXED
C         SWI ADAPTIVE TIME STEP
        INTEGER, SAVE, POINTER :: NADPTFLG
        INTEGER, SAVE, POINTER :: NADPTMX
        INTEGER, SAVE, POINTER :: NADPTMN
        REAL, SAVE, POINTER    :: ADPTFCT
        INTEGER, SAVE, POINTER :: IADPT
        INTEGER, SAVE, POINTER :: IADPTMOD
        REAL, SAVE, POINTER    :: ADPTVAL
        REAL, SAVE, POINTER    :: SWIDELT
C---------STORAGE FOR ADAPTIVE SWI TIME STEP SUMMARY
        INTEGER, SAVE, DIMENSION(:), POINTER :: NADPTSUM
        REAL, SAVE, DIMENSION(:), POINTER :: RADPTSUM
        INTEGER, SAVE, POINTER :: IADPTSUM
C         SWI OUTPUT
        INTEGER, SAVE, POINTER :: NOBS
        INTEGER, SAVE, POINTER :: IOBSHEADER
        INTEGER, SAVE, POINTER :: ISWIZT,ISWICB,ISWIOBS
        INTEGER, SAVE, POINTER :: NLAYSWI
C         SOLVER
        INTEGER, SAVE, POINTER :: NSOLVER
        INTEGER, SAVE, POINTER :: IPRSOL
        INTEGER, SAVE, POINTER :: MUTSOL
C         SWI PARAMETERS
        REAL, SAVE, POINTER    :: TOESLOPE,TIPSLOPE,ALPHA,BETA
        INTEGER, SAVE, DIMENSION(:), POINTER :: ICONV
        INTEGER, SAVE, DIMENSION(:,:), POINTER :: IBO
        REAL(KIND=VERSIZE), SAVE, DIMENSION(:,:), POINTER :: SWIHCOF
        REAL, SAVE, DIMENSION(:,:), POINTER :: SWISOLCR
        REAL, SAVE, DIMENSION(:,:), POINTER :: SWISOLCC
        REAL, SAVE, DIMENSION(:,:), POINTER :: SWISOLCV
        REAL, SAVE, DIMENSION(:,:,:,:), POINTER :: ZETA
        REAL, SAVE, DIMENSION(:,:,:,:), POINTER :: ZETAOLD
        REAL, SAVE, DIMENSION(:,:,:,:), POINTER :: ZETASWITS0
        REAL, SAVE, DIMENSION(:,:,:,:), POINTER :: ZONECHG1
        REAL, SAVE, DIMENSION(:,:,:,:), POINTER :: ZONECHG2
        REAL, SAVE, DIMENSION(:,:,:,:), POINTER :: ZONEIMIX
        REAL, SAVE, DIMENSION(:,:,:), POINTER :: SSZ
        REAL, SAVE, DIMENSION(:), POINTER :: EPS
        REAL, SAVE, DIMENSION(:), POINTER :: NUS
        REAL, SAVE, DIMENSION(:), POINTER :: DELNUS
        REAL, SAVE, DIMENSION(:), POINTER :: NUSRF
        REAL, SAVE, DIMENSION(:,:,:,:), POINTER :: SWICR
        REAL, SAVE, DIMENSION(:,:,:,:), POINTER :: SWICC
        REAL, SAVE, DIMENSION(:,:,:,:), POINTER :: SWICUMCR
        REAL, SAVE, DIMENSION(:,:,:,:), POINTER :: SWICUMCC
        REAL, SAVE, DIMENSION(:,:,:), POINTER :: NUTOP
        REAL, SAVE, DIMENSION(:,:,:), POINTER :: NUBOT
        REAL, SAVE, DIMENSION(:,:,:), POINTER :: QLEXTRA
        REAL, SAVE, DIMENSION(:,:,:), POINTER :: QREXTRA
        REAL, SAVE, DIMENSION(:,:,:), POINTER :: QFEXTRA
        REAL, SAVE, DIMENSION(:,:,:), POINTER :: QLEXTRACUM
        REAL, SAVE, DIMENSION(:,:,:), POINTER :: QREXTRACUM
        REAL, SAVE, DIMENSION(:,:,:), POINTER :: QFEXTRACUM
        REAL(KIND=VERSIZE), SAVE, DIMENSION(:,:,:), POINTER :: BRHS
        DOUBLE PRECISION, SAVE, DIMENSION(:,:), POINTER :: DUM
        REAL(KIND=VERSIZE), SAVE, DIMENSION(:,:,:), POINTER :: RHSPRESWI
        INTEGER, SAVE, DIMENSION(:,:,:,:), POINTER :: IPLPOS
        INTEGER, SAVE, DIMENSION(:,:,:), POINTER :: IZONENR
C---------STORAGE FOR BUDGET DATA
        INTEGER,SAVE,POINTER                :: NBDITEMS
        REAL,SAVE,DIMENSION(:,:,:), POINTER :: CUMBD
        REAL,SAVE,DIMENSION(:,:,:), POINTER :: INCBD
        DOUBLEPRECISION,SAVE,DIMENSION(:), POINTER  :: RRATIN
        DOUBLEPRECISION,SAVE,DIMENSION(:), POINTER  :: RRATOUT
C---------POINTERS WITH STORAGE VALUES FROM BCF, LPF, HUF, OR UPW
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::SC1
        REAL,    SAVE,   POINTER, DIMENSION(:,:,:) ::SC2
C---------STORAGE FOR OBSERVATION DATA
        TYPE (TSWIOBS), SAVE, DIMENSION(:), POINTER :: SWIOBS
C---------STORAGE FOR SOLVERS
        TYPE (TSWIDE4),  SAVE, POINTER :: SWIDE4
        TYPE (TSWIPCG),  SAVE, POINTER :: SWIPCG

        TYPE GWFSWITYPE
C           SWI DIMENSIONS
          INTEGER, POINTER :: NSRF,ISTRAT,NSWIOPT,NZONES
          INTEGER, POINTER :: IFIXED
C           SWI ADAPTIVE TIME STEP
          INTEGER, POINTER :: NADPTFLG
          INTEGER, POINTER :: NADPTMX
          INTEGER, POINTER :: NADPTMN
          REAL, POINTER    :: ADPTFCT
          INTEGER, POINTER :: IADPT
          INTEGER, POINTER :: IADPTMOD
          REAL, POINTER    :: ADPTVAL
          REAL, POINTER    :: SWIDELT
C---------STORAGE FOR ADAPTIVE SWI TIME STEP SUMMARY
          INTEGER, DIMENSION(:), POINTER :: NADPTSUM
          REAL, DIMENSION(:), POINTER :: RADPTSUM
          INTEGER, POINTER :: IADPTSUM
C           SWI OUTPUT
          INTEGER, POINTER :: NOBS
          INTEGER, POINTER :: IOBSHEADER
          INTEGER, POINTER :: ISWIZT,ISWICB,ISWIOBS
          INTEGER, POINTER :: NLAYSWI
C           SOLVER
          INTEGER, POINTER :: NSOLVER
          INTEGER, POINTER :: IPRSOL
          INTEGER, POINTER :: MUTSOL
C           SWI PARAMETERS
          REAL, POINTER    :: TOESLOPE,TIPSLOPE,ALPHA,BETA
          INTEGER, DIMENSION(:), POINTER :: ICONV
          INTEGER, DIMENSION(:,:), POINTER :: IBO
          REAL(KIND=VERSIZE), DIMENSION(:,:), POINTER :: SWIHCOF
          REAL, DIMENSION(:,:), POINTER :: SWISOLCR
          REAL, DIMENSION(:,:), POINTER :: SWISOLCC
          REAL, DIMENSION(:,:), POINTER :: SWISOLCV
          REAL, DIMENSION(:,:,:,:), POINTER :: ZETA
          REAL, DIMENSION(:,:,:,:), POINTER :: ZETAOLD
          REAL, DIMENSION(:,:,:,:), POINTER :: ZETASWITS0
          REAL, DIMENSION(:,:,:,:), POINTER :: ZONECHG1
          REAL, DIMENSION(:,:,:,:), POINTER :: ZONECHG2
          REAL, DIMENSION(:,:,:,:), POINTER :: ZONEIMIX
          REAL, DIMENSION(:,:,:), POINTER :: SSZ
          REAL, DIMENSION(:), POINTER :: EPS
          REAL, DIMENSION(:), POINTER :: NUS
          REAL, DIMENSION(:), POINTER :: DELNUS
          REAL, DIMENSION(:), POINTER :: NUSRF
          REAL, DIMENSION(:,:,:,:), POINTER :: SWICR
          REAL, DIMENSION(:,:,:,:), POINTER :: SWICC
          REAL, DIMENSION(:,:,:,:), POINTER :: SWICUMCR
          REAL, DIMENSION(:,:,:,:), POINTER :: SWICUMCC
          REAL, DIMENSION(:,:,:), POINTER :: NUTOP
          REAL, DIMENSION(:,:,:), POINTER :: NUBOT
          REAL, DIMENSION(:,:,:), POINTER :: QLEXTRA
          REAL, DIMENSION(:,:,:), POINTER :: QREXTRA
          REAL, DIMENSION(:,:,:), POINTER :: QFEXTRA
          REAL, DIMENSION(:,:,:), POINTER :: QLEXTRACUM
          REAL, DIMENSION(:,:,:), POINTER :: QREXTRACUM
          REAL, DIMENSION(:,:,:), POINTER :: QFEXTRACUM
          REAL(KIND=VERSIZE), DIMENSION(:,:,:), POINTER :: BRHS
          DOUBLE PRECISION, DIMENSION(:,:), POINTER :: DUM
          REAL(KIND=VERSIZE), DIMENSION(:,:,:), POINTER :: RHSPRESWI
          INTEGER, DIMENSION(:,:,:,:), POINTER :: IPLPOS
          INTEGER, DIMENSION(:,:,:), POINTER :: IZONENR
C-----------STORAGE FOR BUDGET DATA
          INTEGER,POINTER                :: NBDITEMS
          REAL,DIMENSION(:,:,:), POINTER :: CUMBD
          REAL,DIMENSION(:,:,:), POINTER :: INCBD
          DOUBLEPRECISION,DIMENSION(:), POINTER  :: RRATIN
          DOUBLEPRECISION,DIMENSION(:), POINTER  :: RRATOUT
C---------POINTERS WITH STORAGE VALUES FROM BCF, LPF, HUF, OR UPW
          REAL,      POINTER, DIMENSION(:,:,:) ::SC1
          REAL,      POINTER, DIMENSION(:,:,:) ::SC2
C-----------STORAGE FOR OBSERVATION DATA
          TYPE (TSWIOBS), DIMENSION(:), POINTER :: SWIOBS
C-----------STORAGE FOR SOLVERS
          TYPE (TSWIDE4),  POINTER :: SWIDE4
          TYPE (TSWIPCG),  POINTER :: SWIPCG
        END TYPE
        TYPE(GWFSWITYPE), SAVE:: GWFSWIDAT(10)
      END MODULE GWFSWIMODULE
C
C
      SUBROUTINE GWF2SWI2AR(In,Ibcf,Ilpf,Ihuf,Iupw,Igrid)
C
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR SEA WATER INTRUSION (SWI2) PACKAGE
C     ******************************************************************
C
C     SPECIFICATIONS:
        USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,NPER,IFREFM,
     2                      NSTP,LBOTM,BOTM
        USE GWFBCFMODULE, ONLY:LCB=>LAYCON,SC1B=>SC1,SC2B=>SC2
        USE GWFLPFMODULE, ONLY:LCL=>LAYTYP,SC1L=>SC1,SC2L=>SC2
        USE GWFHUFMODULE, ONLY:LCH=>LTHUF,SC1H=>SC1
        !USE GWFUPWMODULE, ONLY:LCU=>LAYTYPUPW,SC1U=>SC1,SC2U=>SC2UPW
        USE GWFSWIMODULE
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: In
        INTEGER, INTENT(IN) :: Ibcf
        INTEGER, INTENT(IN) :: Ilpf
        INTEGER, INTENT(IN) :: Ihuf
        INTEGER, INTENT(IN) :: Iupw
        INTEGER, INTENT(IN) :: Igrid
C       + + + LOCAL DEFINITIONS + + +
        CHARACTER*200 :: line
        INTEGER :: lloc, istart, istop
        INTEGER :: iadptflg
        INTEGER :: ierr
        INTEGER :: i, j, k, n
        INTEGER :: iz, kk
        INTEGER :: itmem
        INTEGER :: ic
        INTEGER :: iusezone
        REAL :: r
        REAL :: d
        REAL :: bbot, ttop, z
        CHARACTER*40, DIMENSION(2) :: csolver
        CHARACTER*24, DIMENSION(4) :: ANAME
        CHARACTER*24 :: ZETANAME
C       + + + DATA + + +
        DATA csolver /'                     DIRECT SOLVER (DE4)',
     2                ' PRECONDITIONED CONJUGATE GRADIENT (PCG)'/

        DATA ANAME(1) /'                  NUZONE'/
        DATA ANAME(2) /'                   NUSRF'/
        DATA ANAME(3) /'                     SSZ'/
        DATA ANAME(4) /'                 IZONENR'/
C     + + + INPUT FORMATS + + +
C     + + + OUTPUT FORMATS + + +
    1   FORMAT(//1X,'SWI2 -- SWI PACKAGE, VERSION 2.0.0, 07/22/2013',
     2          /1X,50('-'),
     3          /1X,'SWI2 INPUT READ FROM UNIT',I3,//)
    7 FORMAT(//1X,'SWI2 ERROR: ',
     2       'THE TOTAL NUMBER OF SURFACES LESS THAN 1')
02200   FORMAT(//1X,'SWI2 DATASET 1',/,1X,52('-'),
     2    /1X,'NUMBER OF SURFACES (NSRF):                    ',1X,I5,
     3    /1X,'TOTAL NUMBER OF ZONES:                        ',1X,I5,
     4    /1X,'DENSITY DISTRIBUTION OPTION (ISTRAT):         ',1X,I5,
     5    /1X,'  VARIABLE DENSITY FLOW -- ISTRAT = 0',
     6    /1X,'  STRATIFIED FLOW       -- ISTRAT = 1',
     7    /1X,'NUMBER OF SWI OBSERVATIONS (NOBS):            ',1X,I5,
     8    /1X,'ZETA OUTPUT FILE UNIT NUMBER (ISWIZT):        ',1X,I5,
     9    /1X,'BUDGET OUTPUT FILE UNIT NUMBER (ISWICB):      ',1X,I5,
     X    /1X,'OBSERVATION OUTPUT FILE UNIT NUMBER (ISWIOBS):',1X,I5,
     1    /1X,52('-'))
02300   FORMAT(//1X,'SWI2 DATASET 1 KEYWORD OPTIONS',/,1X,52('-'))
02310   FORMAT(1X,A50)
02320   FORMAT(1X,52('-'))
  111   FORMAT('        ZETA SURFACE ',I2)
02100   FORMAT(//1X,'SWI2 PROCESS REQUIRES USE OF THE BCF, LPF,'
     2           1X,'OR HUF FLOW PACKAGES',//)
02110   FORMAT(//1X,16X,'SWI2 OBSERVATION LOCATIONS',
     2          /1X,' OBSERVATION',
     3           1X,'     LAYER',1X,'       ROW',1X,'    COLUMN',
     4           1X,'      OBSNAM',
     5          /1X,58('-'))
02120   FORMAT(1X,I12,3(1X,I10),1X,A12)
02140   FORMAT(//1X,13X,'SWI2 INVALID OBSERVATION LOCATIONS',
     2          /1X,59('-'))
02150   FORMAT(1X,' OBSERVATION',1X,I5,1X,A6,1X,I5,
     2         1X,'NOT BETWEEN',1X,I5,1X,'AND',1X,I5)
02160   FORMAT(1X,'        NONE')
C     ------------------------------------------------------------------
C
C       + + + CODE + + +
C
C---------ALLOCATE VARIABLES - INITIALIZE IF POSSIBLE
        ALLOCATE(NSRF,ISTRAT,NZONES)
        ALLOCATE(NADPTFLG,NADPTMX,NADPTMN,ADPTFCT)
        ALLOCATE(IADPT,IADPTMOD,ADPTVAL,SWIDELT)
        ALLOCATE(NOBS,IOBSHEADER)
        ALLOCATE(ISWIZT,ISWICB,ISWIOBS)
        ALLOCATE(NSWIOPT)
        ALLOCATE(NLAYSWI)
        ALLOCATE(NSOLVER,IPRSOL,MUTSOL)
        ALLOCATE(TOESLOPE,TIPSLOPE,ALPHA,BETA)
        
        ALLOCATE(IFIXED)

        IOBSHEADER   = 0
        iadptflg     = 0
        NSWIOPT      = 0
        IFIXED       = 0
C
C---------IDENTIFY PACKAGE AND INITIALIZE
        WRITE(IOUT,1) In
C
C---------READ DATASET 1
        CALL URDCOM(In, IOUT, line)
        lloc = 1
        CALL URWORD(line, lloc, istart, istop, 2,   NSRF, r, IOUT, In)
        CALL URWORD(line, lloc, istart, istop, 2, ISTRAT, r, IOUT, In)
        CALL URWORD(line, lloc, istart, istop, 2,   NOBS, r, IOUT, In)
        CALL URWORD(line, lloc, istart, istop, 2, ISWIZT, r, IOUT, In)
        CALL URWORD(line, lloc, istart, istop, 2, ISWICB, r, IOUT, In)
        CALL URWORD(line, lloc, istart, istop, 2,ISWIOBS, r, IOUT, In)
C         TEST FOR KEYWORD ARGUMENTS
        DO
          CALL URWORD(line,lloc,istart,istop,1,n,r,IOUT,In)
          SELECT CASE ( line(istart:istop) )
            CASE ( 'ADAPTIVE' )
              iadptflg = 1
            CASE ( 'FSSSOPT' )
              NSWIOPT = 1
            CASE ( 'FIXEDZETA' )
              IFIXED = 1
            CASE ( '0', '' )
              EXIT
            CASE DEFAULT
!              IF ( LEN(line(istart:istop)).EQ.0 ) EXIT
              WRITE (IOUT,'(1X,A,1X,A)') 
     2          'UNRECOGNIZED KEYWORD:', line(istart:istop)
          END SELECT
        END DO

C
C---------CHECK TO SEE THAT NUMBER OF SURFACES IS AT LEAST 1
        IF ( NSRF.LT.1 ) THEN
          WRITE (IOUT,7)
          CALL USTOP('SWI2 ERROR: NUMBER OF SURFACES LESS THAN 1')
        ENDIF
C
C---------CALCULATE THE TOTAL NUMBER OF ZONES
        NZONES = NSRF + 1
        NLAYSWI = 1
C
C---------WRITE DATASET 1
        WRITE (IOUT,2200) NSRF, NZONES, ISTRAT, NOBS,
     2                    ISWIZT, ISWICB, ISWIOBS
C         DATASET 1 OPTIONS
        IF ( NSWIOPT.NE.0 .OR. iadptflg.NE.0 .OR.
     2       IFIXED.NE.0 ) THEN
          WRITE (IOUT,2300)
          IF ( NSWIOPT.NE.0 ) THEN
            WRITE (IOUT,2310) 
     2        'DEBUG OPTION - ADDITION DATA DURING TIP/TOE     '
          END IF
          IF ( iadptflg.NE.0 ) THEN
            WRITE (IOUT,2310) 
     2        'SWI2 ADAPTIVE TIME STEP OPTION (ADAPTIVE)       '
          END IF
          IF ( IFIXED.NE.0 ) THEN
            WRITE (IOUT,2310) 
     2        'FIXED ZETA SURFACES OPTION   '
          END IF
          WRITE (IOUT,2320)
        END IF
C
C---------READ DATASET 2A - SOLVER DATA FOR BOTH SOLVERS
        CALL URDCOM(In, IOUT, line)
        lloc = 1
        CALL URWORD(line, lloc, istart, istop, 2,NSOLVER, r, IOUT, In)
        CALL URWORD(line, lloc, istart, istop, 2, IPRSOL, r, IOUT, In)
        CALL URWORD(line, lloc, istart, istop, 2, MUTSOL, r, IOUT, In)
        IF ( NSOLVER.LT.1 .OR. NSOLVER.GT.2 ) THEN
          WRITE (IOUT,2210) (i,csolver(i),i=1,3)
          CALL USTOP('SWI2 ERROR: INVALID NSOLVER SPECIFIED')
        END IF
        IF ( IPRSOL.LT.1 ) IPRSOL = 999
        IF ( MUTSOL.LT.0 .OR. MUTSOL.GT.3 ) MUTSOL=0
C
C---------WRITE DATASET 2A
        WRITE (IOUT,2220) NSOLVER, csolver(NSOLVER),
     2                    IPRSOL, MUTSOL

02210   FORMAT(//1X,'SWI2 DATASET 2A',/,1X,52('-'),
     2    /1X,'ERROR SPECIFYING NSOLVER - VALID VALUES ARE',
     3    100(:/1X,I2,1X,'=',A40))
02220   FORMAT(//1X,'SWI2 DATASET 2A',/,1X,52('-'),
     2    /1X,'SOLVER (NSOLVER):                             ',1X,I5,
     3    /1X,'  SOLVER =',1X,A40,
     4    /1X,'SOLVER PRINTOUT INTERVAL (IPRSOL):            ',1X,I5,
     5    /1X,'SOLVER OUTPUT FLAG (MUTSOL):                  ',1X,I5,
     6    /1X,'  0 = PRINTING EVERY ITERATION',
     7    /1X,'  1 = LIMITED (TOTAL NUMBER OF ITERATIONS)',
     8    /1X,'  2 = NO PRINTING',
     9    /1X,'  3 = ONLY IF CONVERGENCE FAILS',
     1    /1X,52('-'))
C
C---------ALLOCATE STORAGE FOR SOLVER DATA
        ALLOCATE(SWIDE4,SWIPCG)
C
C---------READ DATASET 2B IF NECESSARY
        SELECT CASE (NSOLVER)
          CASE (2)
            CALL URDCOM(In, IOUT, line)
            lloc = 1
            CALL URWORD(line, lloc, istart, istop, 2, i, r, IOUT, In)
            SWIPCG%MXITER = i
            CALL URWORD(line, lloc, istart, istop, 2, i, r, IOUT, In)
            SWIPCG%ITER1  = i
            CALL URWORD(line, lloc, istart, istop, 2, i, r, IOUT, In)
            SWIPCG%NPCOND = i
            CALL URWORD(line, lloc, istart, istop, 3, i, r, IOUT, In)
            SWIPCG%ZCLOSEPCG = r
            CALL URWORD(line, lloc, istart, istop, 3, i, r, IOUT, In)
            SWIPCG%RCLOSEPCG = r
            CALL URWORD(line, lloc, istart, istop, 3, i, r, IOUT, In)
            SWIPCG%RELAXPCG = r
            CALL URWORD(line, lloc, istart, istop, 2, i, r, IOUT, In)
            SWIPCG%NBPOL = i
            CALL URWORD(line, lloc, istart, istop, 3, i, r, IOUT, In)
            SWIPCG%DAMPPCG = ABS(r)
            IF ( r.LT.0 ) THEN
              CALL URWORD(line, lloc, istart, istop, 3, i, r, IOUT, In)
              SWIPCG%DAMPPCGT = r
            ELSE
              SWIPCG%DAMPPCGT = SWIPCG%DAMPPCG
            END IF
C             WRITE DATASET 2B FOR PCG SOLVER
            WRITE (IOUT,2230) SWIPCG%MXITER, SWIPCG%ITER1,
     2                        SWIPCG%NPCOND,
     3                        SWIPCG%ZCLOSEPCG, SWIPCG%RCLOSEPCG,
     4                        SWIPCG%RELAXPCG, SWIPCG%NBPOL,
     5                        SWIPCG%DAMPPCG, SWIPCG%DAMPPCGT
        END SELECT
02230   FORMAT(//1X,'SWI2 DATASET 2B',
     2    /1X,'PRECONDITIONED CONJUGATE GRADIENT PARAMETERS',
     3    /1X,52('-'),
     4    /1X,'MAXIMUM NUMBER OF CALLS TO PCG (MXITER):      ',1X,I5,
     5    /1X,'MAXIMUM ITERATIONS PER CALL TO PCG (ITER1):   ',1X,I5,
     6    /1X,'MATRIX PRECONDITIONING TYPE (NPCOND):         ',1X,I5,
     7    /1X,'  1 = MODIFIED INCOMPLETE CHOLESKY',
     8    /1X,'  2 = NEUMANN POLYNOMIAL - THE MATRIX WILL BE SCALED',
     9    /1X,'ZETA CLOSURE CRITERION (ZCLOSE):    ',1X,G15.5,
     X    /1X,'RESIDUAL CLOSURE CRITERION (RCLOSE):',1X,G15.5,
     1    /1X,'RELAXATION FACTOR (RELAX):          ',1X,G15.5,
     2    /1X,'  ONLY USED WITH NPCOND = 1',
     3    /1X,'POLYNOMIAL PRECONDITIONER PARAMETER (NBPOL):  ',1X,I5,
     4    /1X,'  INTERNALLY CALCULATED IF NPCOND.NE.2',
     5    /1X,'STEADY-STATE DAMPING (DAMPPCG):     ',1X,G15.5,
     6    /1X,'TRANSIENT DAMPING (DAMPPCGT):       ',1X,G15.5,
     7    /1X,52('-'))
C
C---------ALLOCATE ARRAYS
        IF ( NOBS.GT.0 ) THEN
          ALLOCATE(SWIOBS(NOBS))
        ELSE
          ALLOCATE(SWIOBS(1))
        END IF
        ALLOCATE(ZETA(NCOL,NROW,NLAY,NZONES+1))
        ALLOCATE(SSZ(NCOL,NROW,NLAY))
        ALLOCATE(IZONENR(NCOL,NROW,NLAY))
C---------ALLOCATE AND INITIALIZE SWI ZONE BUDGET DATA
        ALLOCATE(NBDITEMS)
        NBDITEMS = 5
        ALLOCATE(CUMBD(2,NBDITEMS,NZONES),INCBD(2,NBDITEMS,NZONES))
        ALLOCATE(RRATIN(NBDITEMS),RRATOUT(NBDITEMS))
        DO i = 1, NBDITEMS
          RRATIN(i)  = 0.0D0
          RRATOUT(i) = 0.0D0
          DO iz = 1, NZONES
            DO j = 1, 2
              CUMBD(j,i,iz) = 0.0
              INCBD(j,i,iz) = 0.0
            END DO
          END DO
        END DO
C---------ALLOCATE SOLUTION DATA
        ALLOCATE(NUS(NZONES))
        ALLOCATE(NUSRF(NZONES+1))
        ALLOCATE(DELNUS(NZONES))
        ALLOCATE(EPS(NZONES))

        ALLOCATE(IPLPOS(NCOL,NROW,NLAY,NZONES))
        ALLOCATE(RHSPRESWI(NCOL,NROW,NLAY))
        ALLOCATE(SWICR(NCOL,NROW,NLAY,NZONES))
        ALLOCATE(SWICC(NCOL,NROW,NLAY,NZONES))
        ALLOCATE(SWICUMCR(NCOL,NROW,NLAY,NZONES))
        ALLOCATE(SWICUMCC(NCOL,NROW,NLAY,NZONES))
        ALLOCATE(NUTOP(NCOL,NROW,NLAY),NUBOT(NCOL,NROW,NLAY))
        ALLOCATE(QLEXTRA(NCOL,NROW,NLAY))
        ALLOCATE(QREXTRA(NCOL,NROW,NLAY))
        ALLOCATE(QFEXTRA(NCOL,NROW,NLAY))
        ALLOCATE(QLEXTRACUM(NCOL,NROW,NLAY))
        ALLOCATE(QREXTRACUM(NCOL,NROW,NLAY))
        ALLOCATE(QFEXTRACUM(NCOL,NROW,NLAY))

        ALLOCATE(BRHS(NCOL,NROW,NZONES))
        ALLOCATE(ZETAOLD(NCOL,NROW,NLAY,NZONES+1))
        ALLOCATE(ZETASWITS0(NCOL,NROW,NLAY,NZONES+1))
        ALLOCATE(ZONECHG1(NCOL,NROW,NLAY,NZONES+1))
        ALLOCATE(ZONECHG2(NCOL,NROW,NLAY,NZONES+1))
        ALLOCATE(ZONEIMIX(NCOL,NROW,NLAY,NZONES+1))
        ALLOCATE(ICONV(NLAY))
        ALLOCATE(IBO(NCOL,NROW),SWIHCOF(NCOL,NROW))
        ALLOCATE(SWISOLCR(NCOL,NROW))
        ALLOCATE(SWISOLCC(NCOL,NROW))
        ALLOCATE(SWISOLCV(NCOL,NROW))
        ALLOCATE(DUM(NCOL,NROW))
C
C-------READ DATASET 3A PARAMETERS
        CALL URDCOM(In, IOUT, line)
        lloc = 1
        CALL URWORD(line, lloc, istart, istop, 3, i, TOESLOPE, IOUT, In)
        CALL URWORD(line, lloc, istart, istop, 3, i, TIPSLOPE, IOUT, In)
        CALL URWORD(line, lloc, istart, istop, 3, i,    ALPHA,-IOUT, In)
        IF ( ALPHA.NE.0.0 ) THEN
          CALL URWORD(line, lloc, istart, istop, 3, i,  BETA, IOUT, In)
        ELSE
          ALPHA = 0.1
          BETA  = 0.1
        END IF
C
C-------CONFIRM THAT VALID ALPHA AND BETA VALUES ARE SPECIFIED
        IF ( ALPHA.LE.0.0 .OR. ALPHA.GT.1.0 ) THEN
          WRITE (IOUT,2240) 'ALPHA',ALPHA
          CALL USTOP('SWI2 ERROR: ALPHA OUT OF BOUNDS (0.0,1.0]')
        END IF
        IF ( BETA.LE.0.0  .OR. BETA.GT.1.0  ) THEN
          WRITE (IOUT,2240) 'BETA',BETA
          CALL USTOP('SWI2 ERROR: BETA OUT OF BOUNDS (0.0,1.0]')
        END IF
02240   FORMAT(//1X,'SWI2 ERROR:',1X,A,1X,'(',G10.3,')',1X,
     2              'MUST BE GREATER THAN 0.0 AND LESS THAN OR ',
     3              'EQUAL TO 1.0')
C
C-------READ DATASET 3B PARAMETERS
        NADPTMX  = 1
        NADPTMN  = 1
        ADPTFCT  = 1.0
        IF ( iadptflg.NE.0 ) THEN        
          CALL URDCOM(In, IOUT, line)
          lloc = 1
          CALL URWORD(line,lloc,istart,istop,2,  NADPTMX, r,-IOUT, In)
          CALL URWORD(line,lloc,istart,istop,2,NADPTMN, r, IOUT, In)
          CALL URWORD(line,lloc,istart,istop,3,i,ADPTFCT,IOUT,In)
          IF ( NADPTMN.GT.NADPTMX ) THEN
            WRITE (IOUT,2245) NADPTMN, NADPTMX
            CALL USTOP('SWI2 ERROR: NADPTMN EXCEEDS NADPTMX')
          END IF
          IF ( ADPTFCT.LE.0.0 ) THEN
            WRITE (IOUT,2250) ADPTFCT
            CALL USTOP('SWI2 ERROR: ADPTFCT MUST BE > ZERO')
          END IF
        END IF
02245   FORMAT(//1X,'SWI2 ERROR: NADPTMN (',I10,')',1X,
     2              'EXCEEDS NAPTMX (',I10,')')
02250   FORMAT(//1X,'SWI2 ERROR: ADPTFCT (',G10.3,')',1X,
     2              'MUST BE GREATER THAN ZERO')

        IADPT      = NADPTMN
        IADPTMOD   = -1
        ADPTVAL    = 1.0

        NADPTFLG = 0
        IF ( NADPTMX.GT.1 ) NADPTFLG = 1

        WRITE (IOUT,2255) TOESLOPE,TIPSLOPE,ALPHA,BETA
        IF ( iadptflg.NE.0 ) THEN
          WRITE (IOUT,2260) NADPTMN,NADPTMX,ADPTFCT
        END IF

02255   FORMAT(//1X,'SWI2 DATASET 3A',/,1X,52('-'),
     2    /1X,'MAXIMUM TOE SURFACE SLOPE (TOESLOPE):    ',1X,G10.3,
     3    /1X,'MAXIMUM TIP SURFACE SLOPE (TIPSLOPE):    ',1X,G10.3,
     4    /1X,'EXCESS SURFACE SLOPE FRACTION (ALPHA):   ',1X,G10.3,
     5    /1X,'MINIMUM ZONE THICKNESS FRACTION (BETA):  ',1X,G10.3,
     6    /1X,52('-'))
02260   FORMAT(//1X,'SWI2 DATASET 3B',/,1X,52('-'),
     1    /1X,'MINIMUM SWI SUB-TIME STEPS  (NADPTMN):   ',1X,I5,
     2    /1X,'MAXIMUM SWI SUB-TIME STEPS  (NADPTMX):   ',1X,I5,
     3    /1X,'MAXIMUM ZETA SLOPE FRACTION (ADPTFCT):   ',1X,G10.3,
     4    /1X,52('-'))
02270   FORMAT(1X,'Invalid ISOURCE value (',I4,
     2    ') at cell (layer,row,col)',I4,',',I4,',',I4)
C
C---------ALLOCATE AND INITIALIZE STORAGE FOR SWI ADAPTIVE TIME STEP SUMMARY
        IF ( NADPTFLG.NE.0 ) THEN
          i = 0
          DO n = 1, NPER
            i = i + NSTP(n)
          END DO
          ALLOCATE(NADPTSUM(i))
          ALLOCATE(RADPTSUM(i))
          DO n = 1, i
            NADPTSUM(n) = 0
            RADPTSUM(n) = 0.0
          END DO
        ELSE
          ALLOCATE(NADPTSUM(1))
          ALLOCATE(RADPTSUM(1))
          NADPTSUM(1) = 0
          RADPTSUM(1) = 0.0
        END IF
        ALLOCATE(IADPTSUM)
        IADPTSUM = 1
C
C         DATASET 4
C---------READ DENSITY DATA BASED ON USER-SPECIFIED DENSITY MODEL
C         CONSTANT ZONE DENSITY MODEL
        CALL SSWI2_RD_COMM(In)
        DENSITYMODEL: IF (ISTRAT.EQ.1) THEN
C
C-----------READ NU FOR EACH ZONE (NZONES)
          CALL U1DREL(NUS,ANAME(1),NZONES,In,IOUT)
C-----------SET EPS EQUAL TO ZERO
          DO iz = 1, NZONES
            EPS(iz) = 0.0
          END DO
C         LINEAR ZONE DENSITY MODEL
        ELSEIF (ISTRAT.EQ.0) THEN
C
C-----------READ NUSRF FOR EACH SURFACE (NZONES+1)
          CALL U1DREL(NUSRF,ANAME(2),NZONES+1,In,IOUT)
C
C-----------CALCULATE NU AND EPS FROM NUSRF
          DO iz = 1, NZONES
            NUS(iz) = 0.5*(NUSRF(iz)+NUSRF(iz+1))
            EPS(iz) = (NUSRF(iz+1)-NUSRF(iz)) / 6
          END DO
        END IF DENSITYMODEL
C
C---------CALCULATE DELNUS FROM NUS
        DELNUS(1)=NUS(1);
        DO iz = 2, NZONES
          DELNUS(iz)=(NUS(iz)-NUS(iz-1))
        END DO
C
C---------DATASET 5
C---------READ ZETA FOR EACH ACTIVE SURFACE (ZONES 2 TO NZONES)
        CALL SSWI2_RD_COMM(In)
        IZ_ZETA: DO iz=2,NZONES
          K_ZETA: DO k=1,NLAY
            kk = k
            WRITE(ZETANAME,111) iz-1
            CALL U2DREL(ZETA(1:NCOL,1:NROW,k,iz),ZETANAME,
     2                  NROW,NCOL,kk,In,IOUT)
C
C-------------LOOP THROUGH EACH ROW AND COLUMN
C             RESET ZETA IF SPECIFIED ZETA IS GREATER THAN THE TOP
C             OF THE CURRENT CELL OR LESS THAN THE BOTTOM OF THE CURRENT
C             CELL - IF ZETA IS RESET TO THE TOP OR BOTTOM BASED ON IF
C             THE SPECIFIED ZETA VALUE IS CLOSER TO THE TOP OR BOTTOM
            d = SWISMALL !0.001
            I_ZETA: DO i = 1, NROW
              J_ZETA: DO j = 1, NCOL
                ttop = BOTM(j,i,LBOTM(k)-1)
                bbot = BOTM(j,i,LBOTM(k))
                z = ZETA(j,i,k,iz)
                IF (z.NE.bbot) THEN
                  IF (z.LT.(bbot+d)) THEN
                    ZETA(j,i,k,iz) = bbot
                  END IF
                END IF
                IF (z.GT.(ttop-d)) ZETA(j,i,k,iz) = ttop
              END DO J_ZETA
            END DO I_ZETA
          END DO K_ZETA
        END DO IZ_ZETA
C
C---------DATASET 6
C---------READ SSZ FOR EACH LAYER
        CALL SSWI2_RD_COMM(In)
        K_SSZ: DO k = 1, NLAY
          kk=k
          CALL U2DREL(SSZ(1:NCOL,1:NROW,k),ANAME(3),
     2                NROW,NCOL,kk,In,IOUT)
        END DO K_SSZ
C
C---------DATASET 7
C---------READ IZONENR FOR EACH LAYER
        ierr = 0
        CALL SSWI2_RD_COMM(In)
        K_IZONENR: DO k = 1, NLAY
          kk=k
          CALL U2DINT(IZONENR(1:NCOL,1:NROW,kk),ANAME(4),
     2                NROW,NCOL,kk,In,IOUT)
C
C-----------CHECK FOR INVALID ZONE NUMBERS
          DO i = 1, NROW
            DO j = 1, NCOL
              iusezone = ABS(IZONENR(j,i,k))
              if (iusezone > 100) then
                iusezone = 100 - iusezone
              end if
              IF ( iusezone.GT.NZONES ) THEN
                ierr = ierr + 1
                IF ( ierr.EQ.1 ) WRITE(IOUT,'(//)')
                WRITE(IOUT,2270) IZONENR(j,i,k),k,i,j 
              END IF
            END DO
          END DO
      END DO K_IZONENR
      IF ( ierr.GT.0 ) THEN
        WRITE (IOUT,'(A)') 'INVALID ISOURCE VALUES SPECIFIED'
        WRITE (IOUT,'(A,1X,I4)') 
     2   'MAXIMUM ISOURCE VALUE +/-',NZONES
        CALL USTOP('INVALID ISOURCE VALUES SPECIFIED')
      END IF
C
C---------READ OBSERVATION DATA
        IF ( NOBS.GT.0 ) THEN
          DO n = 1, NOBS
C-------------READ OBSERVATION DATA
            CALL URDCOM(In, IOUT, line)
            lloc = 1
            CALL URWORD(line, lloc, istart, istop, 0, i, r, IOUT, In)
            SWIOBS(n)%OBSNAM = line(istart:istop)
            CALL URWORD(line, lloc, istart, istop, 2, k, r, IOUT, In)
            SWIOBS(n)%KLAY = k
            CALL URWORD(line, lloc, istart, istop, 2, i, r, IOUT, In)
            SWIOBS(n)%IROW = i
            CALL URWORD(line, lloc, istart, istop, 2, j, r, IOUT, In)
            SWIOBS(n)%JCOL = j
          END DO
C-------------WRITE OBSERVATION DATA TO IOUT
          WRITE (IOUT,2110)
          DO n = 1, NOBS
            WRITE (IOUT,2120)
     2        n, SWIOBS(n)%KLAY, SWIOBS(n)%IROW, SWIOBS(n)%JCOL,
     3        ADJUSTR(TRIM(SWIOBS(n)%OBSNAM))
          END DO
C-----------CHECK FOR INVALID OBSERVATION LOCATIONS
          ierr = 0
          WRITE(IOUT,2140)
          DO n = 1, NOBS
            k = SWIOBS(n)%KLAY
            i = SWIOBS(n)%IROW
            j = SWIOBS(n)%JCOL
            IF ( k.LT.1 .OR. k.GT.NLAY ) THEN
              ierr = ierr + 1
              WRITE (IOUT,2150) n, 'LAYER ', k, 1, NLAY
            END IF
            IF ( i.LT.1 .OR. i.GT.NROW ) THEN
              ierr = ierr + 1
              WRITE (IOUT,2150) n, 'ROW   ', i, 1, NROW
            END IF
            IF ( j.LT.1 .OR. j.GT.NCOL ) THEN
              ierr = ierr + 1
              WRITE (IOUT,2150) n, 'COLUMN', j, 1, NCOL
            END IF
          END DO
          IF ( ierr.GT.0 ) THEN
            CALL USTOP('SWI2 ERROR: INVALID OBSERVATION LOCATIONS')
          ELSE
            WRITE (IOUT,2160)
          END IF
        END IF
C
C---------ALLOCATE SPACE FOR THE SOLVER ARRAYS
C         DIRECT SOLVER (DE4)
        SOLVERPARAM: IF ( NSOLVER.EQ.1 ) THEN
          SWIDE4%ITMX = 1
C-----------SPECIFY DEFAULT DE4 SOLVER PARAMETERS
          SWIDE4%MXITER = SWIDE4%ITMX
          SWIDE4%NITERDE4 = 1
          SWIDE4%ACCLDE4 = 1.0
          SWIDE4%HCLOSEDE4 = 1.0E-05
          SWIDE4%IFREQ = 3
          SWIDE4%DELTL = 1.
C           INITIALIZE DIMENSION PARAMETERS
          SWIDE4%NODES=NCOL*NROW*NLAYSWI
          SWIDE4%NHALFU=(SWIDE4%NODES-1)/2 + 1
          SWIDE4%NHALFL=SWIDE4%NODES-SWIDE4%NHALFU
          SWIDE4%NBWL = 0
          SWIDE4%NUPL = 0
          SWIDE4%NLOWL = 0
          SWIDE4%ID4DIM = 7
C           CALCULATE SOLVER DIMENSIONS
          IF(NLAYSWI.LE.NCOL .AND. NLAYSWI.LE.NROW) THEN
             IF(NLAYSWI.EQ.1) SWIDE4%ID4DIM=5
             IF(NCOL.GE.NROW) THEN
                SWIDE4%ID4DIR=1
                SWIDE4%NBWGRD=NROW*NLAYSWI+1
             ELSE
                SWIDE4%ID4DIR=2
                SWIDE4%NBWGRD=NCOL*NLAYSWI+1
             END IF
          ELSE IF(NROW.LE.NCOL .AND. NROW.LE.NLAYSWI) THEN
             IF(NROW.EQ.1) SWIDE4%ID4DIM=5
             IF(NCOL.GE.NLAYSWI) THEN
                SWIDE4%ID4DIR=3
                SWIDE4%NBWGRD=NROW*NLAYSWI+1
             ELSE
                SWIDE4%ID4DIR=4
                SWIDE4%NBWGRD=NROW*NCOL+1
             END IF
          ELSE
             IF(NCOL.EQ.1) SWIDE4%ID4DIM=5
             IF(NROW.GE.NLAYSWI) THEN
                SWIDE4%ID4DIR=5
                SWIDE4%NBWGRD=NCOL*NLAYSWI+1
             ELSE
                SWIDE4%ID4DIR=6
                SWIDE4%NBWGRD=NCOL*NROW+1
             END IF
          END IF
          SWIDE4%MXUP=SWIDE4%NHALFU
          SWIDE4%MXLOW=SWIDE4%NHALFL
          SWIDE4%MXBW=SWIDE4%NBWGRD + 4
          SWIDE4%MXEQ=SWIDE4%MXUP+SWIDE4%MXLOW
C         ALLOCATE DE4 ARRAYS
          ALLOCATE (SWIDE4%AU(SWIDE4%ID4DIM,SWIDE4%MXUP))
          ALLOCATE (SWIDE4%IUPPNT(SWIDE4%ID4DIM,SWIDE4%MXUP))
          ALLOCATE (SWIDE4%AL(SWIDE4%MXBW,SWIDE4%MXLOW))
          ALLOCATE (SWIDE4%IEQPNT(NCOL,NROW,NLAYSWI))
          ALLOCATE (SWIDE4%D4B(SWIDE4%MXEQ))
          ALLOCATE (SWIDE4%LRCHDE4(3,SWIDE4%ITMX))
          ALLOCATE (SWIDE4%HDCGDE4(SWIDE4%ITMX))
C           INITIALIZE FLOAT ARRAYS
          DO i = 1, SWIDE4%MXUP
            DO j = 1, SWIDE4%ID4DIM
              SWIDE4%AU(j,i) = 0.0
            END DO
          END DO
          DO i = 1, SWIDE4%MXLOW
            DO j = 1, SWIDE4%ID4DIM
              SWIDE4%AL(j,i) = 0.0
            END DO
          END DO
          DO i = 1, SWIDE4%MXEQ
            SWIDE4%D4B(i) = 0.0
          END DO
C         PRECONDITIONED CONJUGATE GRADIENT SOLVER (PCG4)
        ELSE IF ( NSOLVER.EQ.2 ) THEN
C           INITIALIZE DIMENSION PARAMETERS
          SWIPCG%NODES=NCOL*NROW*NLAYSWI
C---------ALLOCATE SPACE FOR THE PCG ARRAYS
          ALLOCATE (SWIPCG%VPCG(NCOL,NROW,NLAYSWI))
          ALLOCATE (SWIPCG%SS(NCOL,NROW,NLAYSWI))
          ALLOCATE (SWIPCG%P(NCOL,NROW,NLAYSWI))
          ALLOCATE (SWIPCG%HPCG(NCOL,NROW,NLAYSWI))
          ALLOCATE (SWIPCG%CD(NCOL,NROW,NLAYSWI))
          IF(SWIPCG%NPCOND.EQ.2) THEN
             ALLOCATE (SWIPCG%HCSV(NCOL,NROW,NLAYSWI))
          ELSE
             ALLOCATE (SWIPCG%HCSV(1,1,1))
          END IF
          itmem=SWIPCG%MXITER*SWIPCG%ITER1
          ALLOCATE (SWIPCG%HCHG(itmem))
          ALLOCATE (SWIPCG%LHCH(3,itmem))
          ALLOCATE (SWIPCG%RCHG(itmem))
          ALLOCATE (SWIPCG%LRCHPCG(3,itmem))
          ALLOCATE (SWIPCG%IT1(itmem))
C           INITIALIZE FLOAT ARRAYS
          DO k = 1, NLAYSWI
            DO i = 1, NROW
              DO j = 1, NCOL
                SWIPCG%VPCG(j,i,k) = 0.0
                SWIPCG%SS(j,i,k)   = 0.0
                SWIPCG%P(j,i,k)    = 0.0
                SWIPCG%HPCG(j,i,k) = 0.0
                SWIPCG%CD(j,i,k)   = 0.0
                IF ( SWIPCG%NPCOND.EQ.2 ) THEN
                  SWIPCG%HCSV(j,i,k) = 0.0
                END IF
              END DO
            END DO
          END DO
      END IF SOLVERPARAM
C
C-------SET ICONV FOR EACH LAYER
      DO k = 1, NLAY
        ic = 1
        IF ( Ibcf.GT.0 ) THEN
          IF ( LCB(k).EQ.0 .OR. LCB(k).EQ.2 ) ic = 0
        ELSE IF ( Ilpf.GT.0 ) THEN
          IF ( LCL(k).EQ.0 ) ic = 0
        ELSE IF ( Ihuf.GT.0 ) THEN
          IF ( LCH(k).EQ.0 ) THEN
            ic = 0
          ELSE
            WRITE (IOUT,'(A)') 
     2        'CONVERTIBLE HUF LAYER INCONSISTENT WITH SWI2'
            CALL USTOP('CONVERTIBLE HUF LAYER INCONSISTENT WITH SWI2') 
          END IF
        !ELSE IF ( Iupw.GT.0 ) THEN
        !  IF ( LCU(k).EQ.0 ) ic = 0
        ELSE
          WRITE (IOUT,'(A)') 
     2      'FLOW PACKAGE SPECIFIED INCONSISTENT WITH SWI2'
          CALL USTOP('FLOW PACKAGE SPECIFIED INCONSISTENT WITH SWI2') 
        END IF
        ICONV(k) = ic
      END DO
C-------SET SC1 AND SC2 USED BY SWI2 TO CALCULATE MODFLOW STORAGE CHANGES
      IF ( Ibcf.GT.0 ) THEN
        SC1 => SC1B
        SC2 => SC2B
      ELSE IF ( Ilpf.GT.0 ) THEN
        SC1 => SC1L
        SC2 => SC2L
      ELSE IF ( Ihuf.GT.0 ) THEN
        SC1 => SC1H
        SC2 => SC1H
      !ELSE IF ( Iupw.GT.0 ) THEN
      !  SC1 => SC1U
      !  SC2 => SC2U
      END IF
C
C---------SET POINTERS FOR GRID
        CALL SGWF2SWI2PSV(Igrid)
C
C---------RETURN
        RETURN
      END SUBROUTINE GWF2SWI2AR
C
C      
      SUBROUTINE GWF2SWI2AD(Kkstp,Kkper,Igrid)
C
C     ******************************************************************
C     SET FIRST AND LAST SURFACE TO TOP AND BOT OF LAYER ON FIRST TIME
C     STEP OF THE FIRST STRESS PERIOD AND ADVANCE ZETAOLD AND ZETASWITS0 
C     TO ZETA EVERY TIME STEP.
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
        USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,BOTM,LBOTM,IBOUND,HNEW
        USE GWFSWIMODULE
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Kkstp
        INTEGER, INTENT(IN) :: Kkper
        INTEGER, INTENT(IN) :: Igrid
C       + + + LOCAL DEFINITIONS + + +
        INTEGER :: i, j, k, ierr
        INTEGER :: iz
        INTEGER :: iz0
        REAL :: ttop, bbot
        DOUBLEPRECISION :: h
        DOUBLEPRECISION :: dtop, dbot
C     ------------------------------------------------------------------
C       + + + CODE + + +
C
C---------SET POINTERS FOR GRID
        CALL SGWF2SWI2PNT(Igrid)
C

C--------SET FIRST AND LAST SURFACE EQUAL TO TOP AND BOTTOM OF THE AQUIFER
        ierr = 0
        IF ( Kkstp.EQ.1 .AND. Kkper.EQ.1 ) THEN
          DO k = 1, NLAY
            DO i = 1, NROW
              DO j = 1, NCOL
C                 SET ZETA SURFACE FOR FIRST AND LAST SURFACE
                ttop=BOTM(j,i,LBOTM(k)-1)
                bbot=BOTM(j,i,LBOTM(k))
                dtop=BOTM(j,i,LBOTM(k)-1)
                dbot=BOTM(j,i,LBOTM(k))
                h = HNEW(j,i,k)
                IF (bbot.GT.ttop) THEN
                  WRITE(IOUT,35) k,i,j
                  WRITE(IOUT,36) ttop,bbot
                  ierr = ierr + 1
                END IF
                ZETA(j,i,k,1)              = ttop
                ZETA(j,i,k,NZONES+1)       = bbot
                ZETAOLD(j,i,k,1)           = ttop
                ZETAOLD(j,i,k,NZONES+1)    = bbot
                ZETASWITS0(j,i,k,1)        = ttop
                ZETASWITS0(j,i,k,NZONES+1) = bbot
              END DO
            END DO
          END DO
C
C-----------SET FLAG FOR LOCATION OF THE ZETA SURFACE RELATIVE
C           TO THE TOP AND BOTTOM OF A CELL
C           IPLPOS=1 AT TOP, IPLPOS=2 AT BOTTOM, IPLPOS=0 IN BETWEEN, AND 
C           IPLPOS=3 IN INACTIVE CELLS
          CALL SSWI2_SET_IPLPOS()
            
        END IF
   35   FORMAT(1X,/1X,'Negative cell thickness at (layer,row,col)',
     1         I4,',',I4,',',I4)
   36   FORMAT(1X,'Top elevation, bottom elevation:',1P,2G13.5)
C---------TERMINATE IF ANY CELL THICKNESSES ARE NEGATIVE
        IF ( ierr.NE.0 ) THEN
          CALL USTOP('ERROR SWI: NEGATIVE CELL THICKNESSES')
        END IF
C
C---------UPDATE ZETA FOR UPPER SURFACE FOR CONVERTIBLE LAYERS
        DO k = 1, NLAY
          IF ( ICONV(k).EQ.0 ) CYCLE
          CALL SSWI2_UPZ1(k,1)
        END DO
C
C----------COPY ZETA TO ZETAOLD AND ZETASWITS0
        DO k = 1, NLAY
          iz0 = 2
          IF (ICONV(k).EQ.1) iz0 = 1
          DO i = 1, NROW
            DO j = 1, NCOL
C               SKIP INACTIVE CELLS                
              IF ( IBOUND(j,i,k).EQ.0 ) CYCLE
C               UPDATE ZETAOLD AND ZETASWITS0              
              DO iz = iz0, NZONES
                ZETAOLD(j,i,k,iz)    = ZETA(j,i,k,iz)
                ZETASWITS0(j,i,k,iz) = ZETA(j,i,k,iz)
              END DO
            END DO
          END DO
        END DO
C
C---------RETURN
        RETURN
      END SUBROUTINE GWF2SWI2AD
C
C
      SUBROUTINE GWF2SWI2FM(Kkstp,Kkper,Kkiter,Igrid)
C
C     ******************************************************************
C     ADD SWI2 PACKAGE TERMS TO RHS AND HCOF
C
C     SWI2 FORMULATE (GWF2SWI2FM) NEEDS TO BE THE LAST PACKAGE ENTRY
C     SINCE SWI2 SAVES THE RHS (RHSPRESWI) PRIOR TO ADDING SWI TERMS
C     RHSPRESWI IS USED TO CALCULATE BOUNDARY CONDITION FLUXES 
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
        USE GLOBAL,      ONLY:IOUT,ISSFLG,NCOL,NROW,NLAY,IFREFM,
     2                        LBOTM,BOTM,
     3                        CR,CC,CV,HCOF,RHS,BUFF,
     4                        DELR,DELC,IBOUND,HNEW
        USE GWFBASMODULE, ONLY: DELT,HDRY,TOTIM
        USE GWFSWIMODULE
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Kkstp
        INTEGER, INTENT(IN) :: Kkper
        INTEGER, INTENT(IN) :: Kkiter
        INTEGER, INTENT(IN) :: Igrid
C       + + + LOCAL DEFINITIONS + + +
        INTEGER :: i, j, k
        INTEGER :: iz
        INTEGER :: iswi
        INTEGER :: izrev
        REAL :: sumthickrf, sumthickff
        REAL :: dzeta1, dzeta2
        REAL :: thickrf, thickff
        REAL :: sumswicr, sumswicc
        REAL :: headdiff
        REAL :: switfact
        REAL :: r
C     ------------------------------------------------------------------
C       + + + CODE + + +
C
C---------SET POINTERS FOR GRID
        CALL SGWF2SWI2PNT(Igrid)
C
C---------COPY RHS TO RHSPRESWI FOR CALCULATING BOUNDARY FLUXES WHEN UPDATING ZETA
        KRHS: DO k=1,NLAY
          IRHS: DO i=1,NROW
            JRHS: DO j=1,NCOL
              RHSPRESWI(j,i,k) = RHS(j,i,k)
            END DO JRHS
          END DO IRHS
        END DO KRHS
C
C-------SET ZETA TO ZETA VALUE AT THE END OF THE LAST SWI TIME STEP IN THE
C       LAST MODFLOW TIME STEP (ZETAOLD). ZETAOLD IS ASSIGNED IN GWF2SWI2AD
        DO k = 1, NLAY
          DO i = 1, NROW
            DO j = 1, NCOL
              QLEXTRACUM(j,i,k) = 0.0
              QREXTRACUM(j,i,k) = 0.0
              QFEXTRACUM(j,i,k) = 0.0
              DO iz = 1, NZONES
                ZETA(j,i,k,iz)       = ZETAOLD(j,i,k,iz)
                ZETASWITS0(j,i,k,iz) = ZETAOLD(j,i,k,iz)
              END DO
              DO iz = 1, NZONES+1
                ZONECHG1(j,i,k,iz)    = 0.0
                ZONECHG2(j,i,k,iz)    = 0.0
                ZONEIMIX(j,i,k,iz)    = 0.0
              END DO
            END DO
          END DO
        END DO
C
C-------EVALUATE IF THE NUMBER OF SWI TIME STEPS PER MODFLOW
C       TIME STEP SHOULD BE REDUCED. REDUCTION IN THE NUMBER
C       OF SWI TIME STEPS PER MODFLOW TIME STEP CAN ONLY
C       OCCUR ON THE FIRST CALL TO GWF2SWI2FM IN EACH MODFLOW
C       TIME STEP
        switfact = 1.0 / REAL( IADPT, 4 )
        SWIDELT  = DELT * switfact
        IF ( NADPTFLG.NE.0 .AND. IFIXED.NE.1 ) THEN
          IF ( IADPT.GT.1 .AND. Kkiter.EQ.1 ) THEN
            IF ( IADPT.GT.NADPTMN ) THEN
              IF ( IADPTMOD.LT.0 ) THEN
                WRITE (IOUT,2000)
     2           'PREVIOUS SWI DELT:  ', IADPT, DELT, SWIDELT
                  r = REAL( IADPT, 4 ) / 2.0
                  IF ( r.LT.REAL( NADPTMN, 4 ) ) THEN
                    r = NADPTMN
                  END IF
                  IADPT    = INT( CEILING( r ) )
                  switfact = 1.0 / REAL( IADPT, 4 )
                  SWIDELT  = DELT * switfact
                  WRITE (IOUT,2000)
     2             'INCREASING SWI DELT:', IADPT, DELT, SWIDELT
              END IF
            END IF
          END IF
        END IF
2000    FORMAT(1X,A,
     2         1X,'IADPT      :',1X,I10,
     3         1X,'DELT       :',1X,G10.3,
     4         1X,'SWI DELT   :',1X,G10.3)
C
C-------ADAPTIVE TIME STEP LOOP
        iswi = 1
        ADAPTIVE: DO
C
C-----------COMPUTE HORIZONTAL (CUMULATIVE) CONDUCTANCE FOR EACH ZONE
          KCALC_CCCR: DO k = 1, NLAY
            ICALC_CCCR: DO i = 1, NROW
              JCALC_CCCR: DO j = 1, NCOL
                IF ( ABS( IBOUND(j,i,k) ).EQ.0 ) CYCLE JCALC_CCCR
                sumthickrf = 0.
                sumthickff = 0.
                IZCALC_CCCR: DO iz = 1, NZONES
                  IF( j.NE.NCOL ) THEN
                    dzeta1 = ZETA(j,i,k,iz)-ZETA(j,i,k,iz+1)
                    dzeta2 = ZETA(j+1,i,k,iz)-ZETA(j+1,i,k,iz+1)
                    IF (dzeta1.LE.0 .OR. dzeta2.LE.0) THEN
                      thickrf = 0.
                    ELSE
                      thickrf = (dzeta1*DELR(j+1)+dzeta2*DELR(j))/
     2                          (DELR(j)+DELR(j+1))
                    END IF
                    SWICR(j,i,k,iz) = CR(j,i,k)*thickrf
                    sumthickrf = sumthickrf+thickrf
                  ELSE
                    SWICR(j,i,k,iz) = 0.
                    sumthickrf = 1.
                  END IF
                  IF( i.LT.NROW ) THEN
                    dzeta1 = ZETA(j,i,k,iz)-ZETA(j,i,k,iz+1)
                    dzeta2 = ZETA(j,i+1,k,iz)-ZETA(j,i+1,k,iz+1)
                      IF (dzeta1.LE.0 .OR. dzeta2.LE.0) THEN
                        thickff = 0.
                      ELSE
                        thickff = (dzeta1*DELC(i+1)+dzeta2*DELC(i))/
     2                            (DELC(i)+DELC(i+1))
                      END IF
                      SWICC(j,i,k,iz) = CC(j,i,k)*thickff
                      sumthickff = sumthickff + thickff
                  ELSE
                    SWICC(j,i,k,iz) = 0.
                    sumthickff = 1.
                  END IF
                END DO IZCALC_CCCR
C
                DO iz=1,NZONES
                  IF (sumthickrf.NE.0) THEN
                    SWICR(j,i,k,iz) = SWICR(j,i,k,iz)/sumthickrf
                  END IF
                  IF (sumthickff.NE.0) THEN
                    SWICC(j,i,k,iz) = SWICC(j,i,k,iz)/sumthickff
                  END IF
                END DO
C
                sumswicr = 0.
                sumswicc = 0.
                DO iz=1,NZONES
                  sumswicr = sumswicr + SWICR(j,i,k,NZONES+1-iz)
                  sumswicc = sumswicc + SWICC(j,i,k,NZONES+1-iz)
                  SWICUMCR(j,i,k,NZONES+1-iz) = sumswicr
                  SWICUMCC(j,i,k,NZONES+1-iz) = sumswicc
                END DO
              END DO JCALC_CCCR
            END DO ICALC_CCCR
          END DO KCALC_CCCR
C
C-----------SET FLAG FOR LOCATION OF THE ZETA SURFACE RELATIVE
C           TO THE TOP AND BOTTOM OF A CELL
C           IPLPOS=1 AT TOP, IPLPOS=2 AT BOTTOM, IPLPOS=0 IN BETWEEN, AND 
C           IPLPOS=3 IN INACTIVE CELLS
          CALL SSWI2_SET_IPLPOS()
C
C-----------SET ZONE CONDUCTANCES TO ZERO IF NOT CELL ON EITHER SIDE HAS ACTIVE ZONE
          KACTIVE: DO k=1,NLAY
            IACTIVE: DO i=1,NROW
              JACTIVE: DO j=1,NCOL
                IF ( ABS( IBOUND(j,i,k) ).EQ.0 ) CYCLE JACTIVE
                IZACTIVE: DO iz = 2, NZONES-1
                  IF (j.LT.NCOL) THEN
                    IF ((IPLPOS(j,i,k,iz)+IPLPOS(j+1,i,k,iz)+
     2                   IPLPOS(j,i,k,iz+1)+
     3                   IPLPOS(j+1,i,k,iz+1)).GT.0) THEN
                      SWICR(j,i,k,iz) = 0.
                    END IF
                  END IF
                  IF (i.LT.NROW) THEN
                    IF ((IPLPOS(j,i,k,iz)+IPLPOS(j,i+1,k,iz)+
     2                   IPLPOS(j,i,k,iz+1)+
     3                   IPLPOS(j,i+1,k,iz+1)).GT.0) THEN
                      SWICC(j,i,k,iz) = 0.
                    END IF
                  END IF
                END DO IZACTIVE
              END DO JACTIVE
            END DO IACTIVE
          END DO KACTIVE
C
C-----------COMPUTE NUTOP AND NUBOT
          KCALCNU: DO k=1,NLAY
            ICALCNU: DO i=1,NROW
              JCALCNU: DO j=1,NCOL
                NUTOP(j,i,k) = NUS(1)
                NUBOT(j,i,k) = NUS(NZONES)
                IF ( ABS( IBOUND(j,i,k) ).EQ.0 ) CYCLE
                IZCALCNU: DO iz=2,NZONES
                  IF (IPLPOS(j,i,k,iz).EQ.1) THEN
                    NUTOP(j,i,k) = NUTOP(j,i,k)+DELNUS(iz)
                  END IF
                  izrev = NZONES - iz+2
                  IF (IPLPOS(j,i,k,izrev).EQ.2) THEN
                    NUBOT(j,i,k)=NUBOT(j,i,k)-DELNUS(izrev)
                  END IF
                END DO IZCALCNU
              END DO JCALCNU
            END DO ICALCNU
          END DO KCALCNU
C
C-----------CALCULATE QREXTRA and QFEXTRA and QLEXTRA
          KQEX: DO k=1,NLAY
            IQEX: DO i=1,NROW
              JQEX: DO j=1,NCOL
                QREXTRA(j,i,k)=0
                QFEXTRA(j,i,k)=0
                QLEXTRA(j,i,k)=0
                IF ( ABS( IBOUND(j,i,k) ).EQ.0 ) CYCLE JQEX
                headdiff = 0.
                IZDDIFF: DO iz=1,NZONES
                 IF ( j.LT.NCOL ) THEN
                   IF ( IBOUND(j+1,i,k).NE.0 ) THEN
                     QREXTRA(j,i,k) = QREXTRA(j,i,k)-DELNUS(iz) *
     &                SWICUMCR(j,i,k,iz) *
     &                (ZETA(j+1,i,k,iz)-ZETA(j,i,k,iz)) +
     &                EPS(iz)*SWICR(j,i,k,iz) *
     &                ((ZETA(j+1,i,k,iz)-ZETA(j+1,i,k,iz+1)) -
     &                (ZETA(j,i,k,iz)-ZETA(j,i,k,iz+1)))
                   END IF
                 END IF
                 IF ( i.LT.NROW ) THEN
                   IF ( IBOUND(j,i+1,k).NE.0 ) THEN
                     QFEXTRA(j,i,k) = QFEXTRA(j,i,k)-DELNUS(iz) *
     &                 SWICUMCC(j,i,k,iz) *
     &                 (ZETA(j,i+1,k,iz)-ZETA(j,i,k,iz)) +
     &                 EPS(iz)*SWICC(j,i,k,iz) *
     &                 ((ZETA(j,i+1,k,iz)-ZETA(j,i+1,k,iz+1))-
     &                 (ZETA(j,i,k,iz)-ZETA(j,i,k,iz+1)))
                   END IF
                 END IF
                 IF ( k.GT.1 ) THEN
                    headdiff = headdiff-
     &                NUS(iz)*(ZETA(j,i,k-1,iz+1)-ZETA(j,i,k-1,iz))
                 END IF
                END DO IZDDIFF
                IF (k.GT.1) THEN
                  IF ( IBOUND(j,i,k-1).NE.0  ) THEN
                    QLEXTRA(j,i,k-1) = 
     &                CV(j,i,k-1)*(headdiff+
     &                0.5*(ZETA(j,i,k-1,NZONES+1)-ZETA(j,i,k,1))*
     &                (NUBOT(j,i,k-1)+NUTOP(j,i,k)))
                  END IF
                END IF
C           ADD QREXTRA, QFEXTRA and QLEXTRA TO QREXTRACUM, QFEXTRA and QLEXTRA
                IF (k.GT.1) THEN
                  QLEXTRACUM(j,i,k-1) = QLEXTRACUM(j,i,k-1) +
     2                                  QLEXTRA(j,i,k-1) * switfact
                END IF
                QFEXTRACUM(j,i,k) = QFEXTRACUM(j,i,k) +
     2                              QFEXTRA(j,i,k) * switfact
                QREXTRACUM(j,i,k) = QREXTRACUM(j,i,k) +
     2                              QREXTRA(j,i,k) * switfact
          END DO JQEX
         END DO IQEX
        END DO KQEX

C
C-------ADD DENSITY CORRECTIONS TO RHS
        KDDIFF: DO k=1,NLAY
          IDDIFF: DO i=1,NROW
            JDDIFF: DO j=1,NCOL
              IF ( ABS( IBOUND(j,i,k) ).EQ.0 ) CYCLE JDDIFF
C---------------LEFT FACE
              IF (j.GT.1) THEN
                RHS(j,i,k) = RHS(j,i,k) - QREXTRA(j-1,i,k) * switfact
              END IF
C---------------RIGHT FACE
              IF (j.LT.NCOL) THEN
                RHS(j,i,k) = RHS(j,i,k) + QREXTRA(j,i,k) * switfact
              END IF
C---------------BACK FACE--------------------
              IF (i.GT.1) THEN
                RHS(j,i,k) = RHS(j,i,k) - QFEXTRA(j,i-1,k) * switfact
              END IF
C---------------FRONT FACE
              IF (i.LT.NROW) THEN
                RHS(j,i,k) = RHS(j,i,k) + QFEXTRA(j,i,k) * switfact
              END IF
C---------------UPPER FACE--------------------
              IF (k.GT.1) THEN
                RHS(j,i,k) = RHS(j,i,k) - QLEXTRA(j,i,k-1) * switfact
              END IF
C---------------LOWER FACE
              IF (k.LT.NLAY) THEN
                RHS(j,i,k) = RHS(j,i,k) + QLEXTRA(j,i,k) * switfact
              END IF
            END DO JDDIFF
          END DO IDDIFF
        END DO KDDIFF
C
C---------ADAPTIVE SWI TIME STEP UPDATE OF ZETA SURFACE
C         OR SIMULATING STEADY STATE ZETA SURFACE FOR A TIME STEP
C         IN A STEADY-STATE STRESS PERIOD
        ADPUPZTST: IF ( NADPTFLG.NE.0 .AND. IFIXED.NE.1 ) THEN
C
C-----------STORE TOTAL CONSTANT HEAD FLUXES in BUFF
          CALL SSWI2_BDCH(1)
C
C-----------UPDATE ZETA SURFACE BASED ON CURRENT HEAD SOLUTION
C           PRIOR TO SOLVING FOR NEW HEAD SOLUTION IN SELECTED
C           GWF PROCESS SOLVER
          CALL SSWI2_UPDZ(Kkstp,Kkper)
C
C-----------EVALUATE IF THE NUMBER OF SWI TIME STEPS PER MODFLOW
C           TIME STEP SHOULD BE INCREASED AND THE SWI SOLUTION
C           RESTARTED AT THE FIRST SWI TIME STEP IN THE CURRENT
C           MODFLOW TIME STEP.
          IF ( IADPTMOD.GT.0 .AND. IADPT.NE.NADPTMX ) THEN
            IF ( ADPTVAL.GT.1.0E0 ) THEN
              WRITE (IOUT,2010)
     2         'PREVIOUS SWI DELT:  ', IADPT, DELT, SWIDELT, ADPTVAL
              r = REAL( IADPT, 4 )
              r = r * ADPTVAL
              IF ( r.GT.REAL( NADPTMX, 4 ) ) THEN
                r = REAL( NADPTMX, 4 )
              END IF
C---------------RESET RHS AND ZETA IF CHANGE IN ZETA EXCEEDS
              DO k = 1, NLAY
                DO i = 1, NROW
                  DO j = 1, NCOL
                    IF ( ABS( IBOUND(j,i,k) ).EQ.0 ) CYCLE
                    RHS(j,i,k)        = RHSPRESWI(j,i,k)
                    QLEXTRACUM(j,i,k) = 0.0
                    QREXTRACUM(j,i,k) = 0.0
                    QFEXTRACUM(j,i,k) = 0.0
                    DO iz = 1, NZONES
                      ZETA(j,i,k,iz)       = ZETAOLD(j,i,k,iz)
                      ZETASWITS0(j,i,k,iz) = ZETAOLD(j,i,k,iz)
                    END DO
                    DO iz = 1, NZONES + 1
                      ZONECHG1(j,i,k,iz)   = 0.0
                      ZONECHG2(j,i,k,iz)   = 0.0
                      ZONEIMIX(j,i,k,iz)   = 0.0
                    END DO
                  END DO
                END DO
              END DO
C---------------UPDATE IADPT, IADPTMOD, AND SWIDELT
              IADPT    = INT( CEILING( r ) )
              IADPTMOD = 0
              switfact = 1.0 / REAL( IADPT, 4 )
              SWIDELT  = DELT * switfact
              WRITE (IOUT,2000)
     2         'DECREASING SWI DELT:', IADPT, DELT, SWIDELT
C---------------RESTART SWI CALCULATIONS AT THE BEGINNING
              iswi = 1
              ADPTVAL = 1.0
              CYCLE ADAPTIVE
            END IF
          END IF
C           RESET IADPTMOD AND ADPTVAL
          IADPTMOD = -1
          ADPTVAL  = 1.0
C
C-----------MOVE TIPS AND TOES OF ZETA SURFACES
          CALL SSWI2_ZETAADJ(Kkstp,Kkper)
        END IF ADPUPZTST
2010    FORMAT(1X,A,
     2         1X,'IADPT      :',1X,I10,
     3         1X,'DELT       :',1X,G10.3,
     4         1X,'SWI DELT   :',1X,G10.3,
     5         1X,'ADJ. FACTOR:',1X,G10.3)
C
C-------END OF ADAPTIVE
        iswi = iswi + 1
        IF ( iswi.GT.IADPT ) EXIT ADAPTIVE
      END DO ADAPTIVE
C---------RETURN
        RETURN
      END SUBROUTINE GWF2SWI2FM
C
C
      SUBROUTINE GWF2SWI2BD(Kkstp,Kkper,Igrid)
C
C     ******************************************************************
C     CALCULATE GLOBAL SWI2 BUDGET TERMS FOR MODFLOW, CALCULATE ZONE
C     BUDGETS, SAVE ZETA, SAVE SWI2 CBC DATA, AND SAVE SWI2 OBSERVATIONS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
        USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,NPER,IFREFM,
     2                        LBOTM,BOTM,
     3                        CR,CC,CV,HCOF,RHS,
     4                        DELR,DELC,IBOUND,HNEW,HOLD,
     5                        BUFF,ISSFLG,NSTP
        USE GWFBASMODULE, ONLY: DELT,PERTIM,TOTIM,HDRY,
     2                          IHDDFL,IBUDFL,ICBCFL,IOFLG,
     3                          MSUM,VBVL,VBNM
        USE GWFSWIMODULE
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Kkstp
        INTEGER, INTENT(IN) :: Kkper
        INTEGER, INTENT(IN) :: Igrid
C       + + + LOCAL DEFINITIONS + + +
        CHARACTER (LEN=16), DIMENSION(5) :: bdtext
        CHARACTER (LEN=17)   :: val1, val2
        INTEGER :: iozeta
        INTEGER :: iu
        INTEGER :: ibd, ibdlbl
        INTEGER :: i, j, k, n
        INTEGER :: kk
        INTEGER :: ib
        INTEGER :: iz, iz2
        INTEGER :: izrev
        INTEGER :: iusezone
        REAL :: t
        REAL :: q
        DOUBLEPRECISION :: qbnd, qch, qstor, qcstor, qint, qtt, qmix
        REAL :: t0, b0
        REAL :: bt, ht
        doubleprecision :: thick, thickb
        DOUBLEPRECISION :: db, dh
        REAL :: z
        REAL :: zero, rate, rin, rout
        DOUBLE PRECISION :: dzero, ratin, ratout, rrate
        REAL :: incin,incout
        REAL :: cumin,cumout
        REAL :: incavg,incp
        REAL :: cumavg,cump
        CHARACTER*16               :: ZETANAME
        CHARACTER*16, DIMENSION(1) :: textflf, textfrf, textfff, textch
        CHARACTER*15               :: cobs
C       + + + FUNCTIONS + + +
        CHARACTER (LEN=17) :: SSWI2_BDCHAR
C
        DATA textflf /'     SWIADDTOFLF'/
        DATA textfrf /'     SWIADDTOFRF'/
        DATA textfff /'     SWIADDTOFFF'/
        DATA textch  /'      SWIADDTOCH'/
        DATA bdtext  /'      BOUNDARIES',
     2                '   CONSTANT HEAD',
     3                '     ZONE CHANGE',
     4                'ZONE CHG TIP/TOE',
     5                '     ZONE MIXING'/
C
C-------OUTPUT FORMAT STATEMENTS
 2000  FORMAT(1X,/1X,A,'   PERIOD ',I4,'   STEP ',I3)
 2010  FORMAT(1X,'   LAYER ',I3,'   ROW ',I5,'   COL ',
     1        I5,'   RATE ',1PG15.6)
 2020  FORMAT(1X,'SOLVING FOR ZETA FOR LAYER',1X,I5,1X,'ZONE',1X,I5)
 2030  FORMAT('      ZETASRF ',I2)
 2031  FORMAT('   SLVZETASRF ',I2)
 2032  FORMAT('  TPTOZETASRF ',I2)
 2040  FORMAT(A15,1X)
 2050  FORMAT(A12,I3.3,1X)
 2060  FORMAT(1X)
 2070  FORMAT(G15.7)
 3000  FORMAT(//22X,'VOLUMETRIC SWI ZONE BUDGET FOR ENTIRE MODEL',/,
     2        21X,'AT END OF TIME STEP',I5,' IN STRESS PERIOD',I4,/,
     3        38X,'ZONE',1X,I5,/,2X,83('-'))
 3010  FORMAT(1X,/8X,'CUMULATIVE VOLUMES',6X,'L**3',7X,
     2        'RATES FOR THIS TIME STEP',6X,'L**3/T',/,
     3        8X,18('-'),17X,24('-'),//,
     4        14X,'IN:',38X,'IN:'/14X,'---',38X,'---')
 3020  FORMAT(1X,6X,A16,' =',A17,6X,A16,' =',A17)
 3030  FORMAT(1X,/15X,'TOTAL IN =',A,14X,'TOTAL IN =',A)
 3040  FORMAT(1X,/13X,'OUT:',37X,'OUT:'/13X,4('-'),37X,4('-'))
 3050  FORMAT(1X,/14X,'TOTAL OUT =',A,13X,'TOTAL OUT =',A)
 3060  FORMAT(1X,/15X,'IN - OUT =',A,14X,'IN - OUT =',A)
 3070  FORMAT(1X,/4X,'PERCENT DISCREPANCY =',F15.2,
     2        5X,'PERCENT DISCREPANCY =',F15.2,/)
 3080  FORMAT(//24X,'SUMMARY OF SWI ADAPTIVE TIME STEPS',
     2          /1X,'  STRESS PERIOD',
     3           1X,'      TIME STEP',
     4           3X,'           DELT',
     5           1X,'          NADPT',
     6           3X,'        SWIDELT',
     8          /1X,83('-'))
 3090  FORMAT(1X,2(I15,1X),A17,1X,I15,1X,A17)
 3100  FORMAT(1X,83('-'),//)
C
C-------SET POINTERS FOR GRID
      CALL SGWF2SWI2PNT(Igrid)
C
C-------INITIALIZE ZERO VARIABLES
      zero   = 0.0
      dzero  = 0.0D0
C
C---------FILL STORAGE FOR SWI ADAPTIVE TIME STEP SUMMARY
        IF ( NADPTFLG.NE.0 .AND. IFIXED.NE.1 ) THEN
          NADPTSUM(IADPTSUM) = IADPT
          RADPTSUM(IADPTSUM) = SWIDELT
          IADPTSUM  = IADPTSUM + 1
C           WRITE SUMMARY OF SWI ADAPTIVE TIME STEPS
          IF ( Kkper.EQ.NPER .AND.
     2         Kkstp.EQ.NSTP(Kkper) ) THEN
            WRITE (IOUT,3080)
            n = 0
            DO i = 1, NPER
              DO j = 1, NSTP(i)
                n = n + 1
                t = RADPTSUM(n) * REAL( NADPTSUM(n), 4 )
                val1 = SSWI2_BDCHAR(t)
                val2 = SSWI2_BDCHAR(RADPTSUM(n))
                WRITE (IOUT,3090) i, j, val1, NADPTSUM(n), val2
              END DO
            END DO
            WRITE (IOUT,3100)
          END IF
        END IF
C
C-------DETERMINE IF ZETA VALUES SHOULD BE OUTPUT
      iozeta = 0
      IF ( IHDDFL.GT.0 ) THEN
        DO k = 1, NLAY
          IF ( IOFLG(k,3).GT.0 ) THEN
            iozeta = 1
            EXIT
          END IF
        END DO
      END IF
C-------INITIALIZE CELL-BY-CELL FLOW TERM FLAG (IBD) AND
      ibd    = 0
      !IF( ISWICB.LT.0 .AND. ICBCFL.NE.0 ) ibd = -1
      IF( ISWICB.GT.0 ) ibd = ICBCFL
      ibdlbl = 0
C
C-------IF CELL-BY-CELL TERMS WILL BE SAVED AS A 3-D ARRAY, THEN CALL
C       UTILITY MODULE UBUDSV TO SAVE THEM.
C
C---------STORE CONSTANT HEAD CORRECTION FLUXES in BUFF
      CALL SSWI2_BDCH(0)
      IF ( ibd.EQ.1 ) CALL UBUDSV(Kkstp,Kkper,textch(1),ISWICB,BUFF,
     2                            NCOL,NROW,NLAY,IOUT)
      IF ( ibd.EQ.2 ) CALL UBDSV1(Kkstp,Kkper,textch(1),ISWICB,
     2                            BUFF,NCOL,NROW,NLAY,IOUT,
     3                            DELT,PERTIM,TOTIM,IBOUND)
C
C---------STORE RIGHT, FRONT AND LOWER FACE CORRECTION FLUXES
      IF ( ibd.EQ.1 ) CALL UBUDSV(Kkstp,Kkper,textfrf(1),ISWICB,
     2                            QREXTRACUM,NCOL,NROW,NLAY,IOUT)
      IF ( ibd.EQ.2 ) CALL UBDSV1(Kkstp,Kkper,textfrf(1),ISWICB,
     2                            QREXTRACUM,NCOL,NROW,NLAY,IOUT,
     3                            DELT,PERTIM,TOTIM,IBOUND)

      IF ( ibd.EQ.1 ) CALL UBUDSV(Kkstp,Kkper,textfff(1),ISWICB,
     2                            QFEXTRACUM,NCOL,NROW,NLAY,IOUT)
      IF ( ibd.EQ.2 ) CALL UBDSV1(Kkstp,Kkper,textfff(1),ISWICB,
     2                            QFEXTRACUM,NCOL,NROW,NLAY,IOUT,
     3                            DELT,PERTIM,TOTIM,IBOUND)

      IF ( ibd.EQ.1 ) CALL UBUDSV(Kkstp,Kkper,textflf(1),ISWICB,
     2                            QLEXTRACUM,NCOL,NROW,NLAY,IOUT)
      IF ( ibd.EQ.2 ) CALL UBDSV1(Kkstp,Kkper,textflf(1),ISWICB,
     2                            QLEXTRACUM,NCOL,NROW,NLAY,IOUT,
     3                            DELT,PERTIM,TOTIM,IBOUND)

C
C-------SWIADDTOCH FLOW TERMS
      ibdlbl = 0
      ratout = dzero
      ratin  = dzero
      DO k=1,NLAY
        DO i=1,NROW
          DO j=1,NCOL
              rrate = BUFF(j,i,k)
              rate  = rrate

C-------------PRINT THE INDIVIDUAL RATES IF REQUESTED(IGHBCB<0).
            IF( ibd.LT.0 ) THEN
               IF( ibdlbl.EQ.0 ) WRITE(IOUT,2000) textch(1),Kkper,Kkstp
               WRITE(IOUT,2010) k, i, j, rate
               ibdlbl=1
            END IF
C-------------SEE IF FLOW IS INTO AQUIFER OR OUT OF AQUIFER.
            IF( rate.LT.zero ) THEN
C---------------FLOW IS OUT OF AQUIFER SUBTRACT RATE FROM RATOUT.
              ratout = ratout - rrate
            ELSE
C---------------FLOW IS INTO AQUIFER; ADD RATE TO RATIN.swiaddtofrf
              ratin = ratin + rrate
            END IF
          END DO
        END DO
      END DO
C
C-------MOVE RATES, VOLUMES AND LABELS INTO ARRAYS FOR PRINTING.
      rin          = ratin
      rout         = ratout
      VBVL(3,MSUM) = rin
      VBVL(1,MSUM) = VBVL(1,MSUM) + rin  * DELT
      VBVL(4,MSUM) = rout
      VBVL(2,MSUM) = VBVL(2,MSUM) + rout * DELT
      VBNM(MSUM)   = textch(1)
C
C-------INCREMENT THE BUDGET TERM COUNTER.
      MSUM = MSUM + 1
C
C---------REINITIALIZE BUFF
        DO k = 1, NLAY
          DO i = 1, NROW
            DO j = 1, NCOL
              BUFF(j,i,k) = 0.0
            END DO
          END DO
        END DO

C---------STORE TOTAL CONSTANT HEAD FLUXES in BUFF
        CALL SSWI2_BDCH(1)
C
C---------UPDATE ZETA SURFACE IF NON-ADAPTIVE
C         SWI TIME STEPPING USED
        IF ( NADPTFLG.EQ.0 .AND. IFIXED.NE.1 ) THEN
          CALL SSWI2_UPDZ(Kkstp,Kkper)
        END IF
C
C---------MOVE TIP AND TOES IF ORIGINAL NON-ADAPTIVE SWI TIME STEP
C         APPROACH USED AND NOT SOLVING FOR A STEADY STATE ZETA
C         SURFACE
        IF ( NADPTFLG.EQ.0 .AND. IFIXED.NE.1 ) THEN
C
C-----------WRITE ZETA TO UNFORMATTED FILE PRIOR TO TIP AND TOE TRACKING
          IF ( IHDDFL.GT.0 .AND. ISWIZT.LT.0 ) THEN
            DO iz = 2, NZONES
              WRITE(ZETANAME,2031) iz-1
              CALL UBUDSV(kkstp,Kkper,ZETANAME,
     &                  ABS(ISWIZT),ZETA(1:NCOL,1:NROW,1:NLAY,iz),
     &                  NCOL,NROW,NLAY,IOUT)
            END DO
          END IF
C-----------MOVE TIPS AND TOES
          CALL SSWI2_ZETAADJ(Kkstp,Kkper)
        END IF
C
C---------WRITE FINAL ZETA TO UNFORMATTED FILE
        IF ( iozeta.GT.0 .AND. ISWIZT.NE.0 ) THEN
          DO iz = 2, NZONES
            WRITE(ZETANAME,2030) iz-1
            CALL UBUDSV(kkstp,Kkper,ZETANAME,
     &                  ABS(ISWIZT),ZETA(1:NCOL,1:NROW,1:NLAY,iz),
     &                  NCOL,NROW,NLAY,IOUT)
          END DO
        END IF
C
C---------REINITIALIZE BUFF
        DO k = 1, NLAY
          DO i = 1, NROW
            DO j = 1, NCOL
              BUFF(j,i,k) = 0.0
            END DO
          END DO
        END DO
C
C---------STORE TOTAL CONSTANT HEAD FLUXES in BUFF
        CALL SSWI2_BDCH(1)
C
C---------ZONE BUDGETS
        CZBUDGET: DO iz = 1, NZONES
          DO ib = 1, NBDITEMS
            RRATIN(ib)  = dzero
            RRATOUT(ib) = dzero
          END DO
          DO k = 1, NLAY
            DO i = 1, NROW
              DO j = 1, NCOL
                IF ( IBOUND(J,I,K).EQ.0 ) CYCLE
                kk = LBOTM(k)
                bt = BOTM(j,i,kk)
                db = BOTM(j,i,kk)
                dh = HNEW(j,i,k)
                ht = HOLD(j,i,k)
                IF ( (dh.LT.db) .AND. (ht.LT.bt) ) CYCLE 
                q      = dzero
                qbnd   = dzero
                qch    = dzero
                qstor  = dzero
                qcstor = dzero
                qint   = dzero
                qtt    = dzero
                qmix   = dzero
C                   BOUNDARIES AND STORAGE
                IF ( IBOUND(J,I,K).GT.0 ) THEN
                  IF ( db.LT.dh ) THEN
                    qbnd = -RHSPRESWI(j,i,k) +
     2                      HNEW(j,i,k) * HCOF(j,i,k)
                  END IF
                  q   = -1.0 * qbnd
C                   CONSTANT HEAD
                ELSE
                  qch = BUFF(j,i,k)
                  q   = -1.0 * qch
                END IF
C
C                  TIP-TOE ADJUSTMENT CHANGE IN ZONE VOLUME
                qtt  = REAL((ZONECHG2(j,i,k,iz)-ZONECHG1(j,i,k,iz)),8)
C
C                 FLOW-BASED CHANGE IN THE ZONE VOLUME
                qint = REAL( ZONECHG1(j,i,k,iz), 8 )
C
C                  INSTANTANEOUS ZONE MIXING
                qmix = REAL( ZONEIMIX(j,i,k,iz), 8 )
C
C                 DETERMINE ZONE NUMBER FOR BOUNDARY CONDITION
                iusezone = IZONENR(j,i,k)
                IF ( iusezone.EQ.0 ) then
                 iusezone = 1
                END IF
                IF ( (iusezone.LT.0) .AND. (q.GT.zero) ) THEN
                  iusezone = 1
                ENDIF
                IF ( (IZONENR(j,i,k) > 100) ) then
                  iusezone = iusezone - 100
                end if
C-----------------FIND HIGHEST ACTIVE ZONE IF THICKNESS IS ZERO
                t0 = ZETAOLD(j,i,k,ABS(iusezone))
                b0 = ZETAOLD(j,i,k,ABS(iusezone)+1)
                IF (t0.LE.b0) THEN
                  DO iz2 = 1, NZONES
                    izrev = NZONES - iz2 + 1
                    t0 = ZETAOLD(j,i,k,izrev)
                    b0 = ZETAOLD(j,i,k,izrev+1)
                    IF (t0.GT.b0) THEN
                      iusezone = izrev
                    END IF
                  END DO
                END IF
                IF ( IZONENR(j,i,k) > 100 ) then
                  if (qbnd < 0. .or. qch < 0.) then
                    thick = ZETAOLD(j,i,k,1) - ZETAOLD(j,i,k,NZONES+1)
                    thickb = ZETAOLD(j,i,k,iz) - ZETAOLD(j,i,k,iz+1)
                    if (thickb <  SWISMALL) then
                      thickb = 0.d0
                    end if
                    qbnd = qbnd * thickb / thick
                    qch = qch * thickb / thick
                    iusezone = iz
                  end if
                end if

                IF ( iz.EQ.ABS(iusezone) ) THEN
C-----------------ADJUST qbnd USING qint TO REMOVE TOTAL ZONE CHANGE FROM
C                 COMBINED BOUNDARY AND STORAGE TERM IF TRANSIENT SOLUTION
                  IF ( ISSFLG(Kkper).EQ.0 ) THEN
                    CALL SSWI2_MSTO(j,i,k,qstor,qcstor)
                    qbnd = qbnd - qstor
                    qint = qint + qcstor
                  END IF
C-----------------SEE IF FLOW IS INTO AQUIFER OR OUT OF AQUIFER.
                  IF( qbnd.LT.dzero ) THEN
C-----------------FLOW IS OUT OF AQUIFER SUBTRACT RATE FROM RATOUT.
                    RRATOUT(1) = RRATOUT(1) - qbnd
                  ELSE
C-----------------FLOW IS INTO AQUIFER; ADD RATE TO RATIN
                    RRATIN(1)  = RRATIN(1) +  qbnd
                  END IF
                  IF( qch.LT.dzero ) THEN
C-----------------FLOW IS OUT OF AQUIFER SUBTRACT RATE FROM RATOUT.
                    RRATOUT(2) = RRATOUT(2) - qch
                  ELSE
C-----------------FLOW IS INTO AQUIFER; ADD RATE TO RATIN
                    RRATIN(2)  = RRATIN(2) + qch
                  END IF
                END IF
                IF( qint.LT.dzero ) THEN
C----------------FLOW IS OUT OF AQUIFER SUBTRACT RATE FROM RATOUT.
                   RRATOUT(3) = RRATOUT(3) - qint
                ELSE
C----------------FLOW IS INTO AQUIFER; ADD RATE TO RATIN
                   RRATIN(3)  = RRATIN(3)  + qint
                END IF
                IF( qtt.LT.dzero ) THEN
C----------------FLOW IS OUT OF AQUIFER SUBTRACT RATE FROM RATOUT.
                   RRATOUT(4) = RRATOUT(4) - qtt
                ELSE
C----------------FLOW IS INTO AQUIFER; ADD RATE TO RATIN
                   RRATIN(4)  = RRATIN(4)  + qtt
                END IF
                IF( qmix.LT.dzero ) THEN
C----------------FLOW IS OUT OF AQUIFER SUBTRACT RATE FROM RATOUT.
                   RRATOUT(5) = RRATOUT(5) - qmix
                ELSE
C----------------FLOW IS INTO AQUIFER; ADD RATE TO RATIN
                   RRATIN(5)  = RRATIN(5)  + qmix
                END IF
              END DO
            END DO
          END DO
C
C-----------MOVE RATES, VOLUMES AND LABELS INTO ARRAYS FOR PRINTING.
          ZBUDGET: DO ib = 1, NBDITEMS
            rin            = RRATIN(ib)
            rout           = RRATOUT(ib)
            INCBD(1,ib,iz) = rin
            CUMBD(1,ib,iz) = CUMBD(1,ib,iz) + rin  * DELT
            INCBD(2,ib,iz) = rout
            CUMBD(2,ib,iz) = CUMBD(2,ib,iz) + rout * DELT
          END DO ZBUDGET
C
C-----------WRITE BUDGET FOR EACH ZONE TO LIST FILE IF NECESSARY
          WZBUDGET: IF (IBUDFL.NE.0) THEN
            incin  = SUM(INCBD(1,:,iz))
            incout = SUM(INCBD(2,:,iz))
            cumin  = SUM(CUMBD(1,:,iz))
            cumout = SUM(CUMBD(2,:,iz))
            WRITE (IOUT,3000) Kkstp, Kkper, iz
C-------------INFLOW ITEMS
            WRITE (IOUT,3010)
            DO i = 1, NBDITEMS
              val1 = SSWI2_BDCHAR(CUMBD(1,i,iz))
              val2 = SSWI2_BDCHAR(INCBD(1,i,iz))
              WRITE (IOUT,3020) bdtext(i),val1,bdtext(i),val2
            END DO
            val1 = SSWI2_BDCHAR(cumin)
            val2 = SSWI2_BDCHAR(incin)
            WRITE (IOUT,3030) val1, val2
C-------------OUTFLOW ITEMS
            WRITE (IOUT,3040)
            DO i = 1, NBDITEMS
              val1 = SSWI2_BDCHAR(CUMBD(2,i,iz))
              val2 = SSWI2_BDCHAR(INCBD(2,i,iz))
              WRITE (IOUT,3020) bdtext(i),val1,bdtext(i),val2
            END DO
            val1 = SSWI2_BDCHAR(cumout)
            val2 = SSWI2_BDCHAR(incout)
            WRITE (IOUT,3050) val1, val2
C-------------SUMMARY ITEMS
C             IN - OUT
            val1 = SSWI2_BDCHAR(cumin-cumout)
            val2 = SSWI2_BDCHAR(incin-incout)
            WRITE (IOUT,3060) val1, val2
C             PERCENT DIFFERENCE
            incavg = (incin + incout) / 2.0
            cumavg = (cumin + cumout) / 2.0
            cump = zero
            IF (cumavg.NE.zero) cump = 100.0 * (cumin-cumout) / cumavg
            incp = zero
            IF (incavg.NE.zero) incp = 100.0 * (incin-incout) / incavg
            WRITE (IOUT,3070) cump, incp
          END IF WZBUDGET
        END DO CZBUDGET
C
C---------WRITE OBSERVATION DATA TO OBSERVATION FILE
        IF ( NOBS.GT.0 .AND. ISWIOBS.NE.0 ) THEN
          iu = ABS( ISWIOBS )
C           WRITE HEADER FOR ASCII FILE
          IF ( ISWIOBS.GT.0 ) THEN
            IF ( IOBSHEADER.LT.1 ) THEN
              WRITE (iu,2040,ADVANCE='NO') '          TOTIM'
              DO n = 1, NOBS
                DO iz = 2, NZONES
                  WRITE (iu,2050,ADVANCE='NO')
     2              ADJUSTR(TRIM(SWIOBS(n)%OBSNAM)), iz-1
                END DO
              END DO
              WRITE (iu,2060,ADVANCE='YES')
              IOBSHEADER = 1
            END IF
          END IF
C           WRITE DATA TO OBSERVATION OUTPUT FILE
          BINOBS: DO n = 1, NOBS
            k = SWIOBS(n)%KLAY
            i = SWIOBS(n)%IROW
            j = SWIOBS(n)%JCOL
            IZBINOBS: DO iz = 2, NZONES
              z = ZETA(j,i,k,iz)
C               ASCII OUTPUT
              IF ( ISWIOBS.GT.0 ) THEN
                IF ( n.EQ.1 .AND. iz.EQ.2 ) THEN
                  WRITE (cobs,2070) TOTIM
                  WRITE (iu,2040,ADVANCE='NO') ADJUSTR(TRIM(cobs))
                END IF
                WRITE (cobs,2070) z
                WRITE (iu,2040,ADVANCE='NO') ADJUSTR(TRIM(cobs))
                IF ( n.EQ.NOBS .AND. iz.EQ.NZONES) THEN
                  WRITE (iu,2060,ADVANCE='YES')
                END IF
C               BINARY OUTPUT
              ELSE
                IF ( n.EQ.1 .AND. iz.EQ.2 ) WRITE (iu) TOTIM
                WRITE (iu) z
              END IF
            END DO IZBINOBS
          END DO BINOBS
        END IF
C
C---------RETURN
        RETURN
      END SUBROUTINE GWF2SWI2BD
C
C
      SUBROUTINE SSWI2_QR(J,I,K,IZ,NCOL,NROW,NLAY,NZONES,
     &                   Q,HNEW,ZETA,DELNUS,EPS,SWICUMCR,SWICR)

C     ******************************************************************
C     SWI2 PACKAGE - CALCULATE FLUX IN THE ROW DIRECTION
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
        USE GLOBAL,      ONLY:BOTM,LBOTM
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: J,I,K,IZ
        INTEGER, INTENT(IN) :: NCOL,NROW,NLAY,NZONES
        REAL, INTENT(INOUT) :: Q
        DOUBLEPRECISION, DIMENSION(NCOL,NROW,NLAY), INTENT(IN) :: HNEW
        REAL, DIMENSION(NCOL,NROW,NLAY,NZONES+1), INTENT(IN) :: ZETA
        REAL, DIMENSION(NZONES), INTENT(IN) :: DELNUS
        REAL, DIMENSION(NZONES), INTENT(IN) :: EPS
        REAL, DIMENSION(NCOL,NROW,NLAY,NZONES), INTENT(IN)   :: SWICR
        REAL, DIMENSION(NCOL,NROW,NLAY,NZONES), INTENT(IN)   :: SWICUMCR
C       + + + LOCAL DEFINITIONS + + +
        INTEGER :: iz2
        INTEGER :: kk
        DOUBLEPRECISION :: h1, h2
        DOUBLEPRECISION :: b1, b2
C
C       + + + CODE + + +
C
C---------ENSURE HEAD IN CELL J-1 AND J IS GREATER THAN CELL BOTTOM - FOR MODFLOW-NWT
        kk = LBOTM(K)
        b1 = BOTM(J-1,I,kk)
        b2 = BOTM(J,I,kk)
        h1 = HNEW(J-1,I,k)
        h2 = HNEW(J,I,k)
        IF ( h1.LT.b1 .OR. h2.LT.b2 ) THEN
          Q = 0.0
          RETURN
        END IF
C
C---------CALCULATE Q IN ROW DIRECTION
        Q = SWICUMCR(J-1,I,K,IZ) * (HNEW(J-1,I,K)-HNEW(J,I,K))

        DO IZ2 = 1, IZ
          Q = Q + SWICUMCR(J-1,I,K,IZ) * DELNUS(IZ2) *
     &            (ZETA(J-1,I,K,IZ2)-ZETA(J,I,K,IZ2))
        END DO

        Q = Q - SWICR(J-1,I,K,IZ) * EPS(IZ) *
     &          (ZETA(J-1,I,K,IZ)-ZETA(J,I,K,IZ))

        Q = Q + SWICR(J-1,I,K,IZ) * EPS(IZ) *
     &          (ZETA(J-1,I,K,IZ+1)-ZETA(J,I,K,IZ+1))


        DO IZ2 = IZ+1, NZONES
          Q = Q + SWICUMCR(J-1,I,K,IZ2) * DELNUS(IZ2) *
     &            (ZETA(J-1,I,K,IZ2)-ZETA(J,I,K,IZ2))
          Q = Q - SWICR(J-1,I,K,IZ2) * EPS(IZ2) *
     &            (ZETA(J-1,I,K,IZ2)-ZETA(J,I,K,IZ2))
          Q = Q + SWICR(J-1,I,K,IZ2) * EPS(IZ2) *
     &            (ZETA(J-1,I,K,IZ2+1)-ZETA(J,I,K,IZ2+1))
        END DO
C
C---------RETURN
        RETURN
      END SUBROUTINE SSWI2_QR
C
C
      SUBROUTINE SSWI2_QC(J,I,K,IZ,NCOL,NROW,NLAY,NZONES,
     &                   Q,HNEW,ZETA,DELNUS,EPS,SWICUMCC,SWICC)
C
C     ******************************************************************
C     SWI2 PACKAGE - CALCULATE FLUX IN THE COLUMN DIRECTION
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
        USE GLOBAL,      ONLY:BOTM,LBOTM
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: J,I,K,IZ
        INTEGER, INTENT(IN) :: NCOL,NROW,NLAY,NZONES
        REAL, INTENT(INOUT) :: Q
        DOUBLEPRECISION, DIMENSION(NCOL,NROW,NLAY), INTENT(IN) :: HNEW
        REAL, DIMENSION(NCOL,NROW,NLAY,NZONES+1), INTENT(IN) :: ZETA
        REAL, DIMENSION(NZONES), INTENT(IN) :: DELNUS
        REAL, DIMENSION(NZONES), INTENT(IN) :: EPS
        REAL, DIMENSION(NCOL,NROW,NLAY,NZONES), INTENT(IN)   :: SWICC
        REAL, DIMENSION(NCOL,NROW,NLAY,NZONES), INTENT(IN)   :: SWICUMCC
C       + + + LOCAL DEFINITIONS + + +
        INTEGER :: iz2
        INTEGER :: kk
        DOUBLEPRECISION :: h1, h2
        DOUBLEPRECISION :: b1, b2
C
C       + + + CODE + + +
C
C---------ENSURE HEAD IN CELL I-1 AND I IS GREATER THAN CELL BOTTOM - FOR MODFLOW-NWT
        kk = LBOTM(K)
        b1 = BOTM(J,I-1,kk)
        b2 = BOTM(J,I,kk)
        h1 = HNEW(J,I-1,k)
        h2 = HNEW(J,I,k)
        IF ( h1.LT.b1 .OR. h2.LT.b2 ) THEN
          Q = 0.0
          RETURN
        END IF
C
C---------CALCULATE Q IN COLUMN DIRECTION
        Q = SWICUMCC(J,I-1,K,IZ) * (HNEW(J,I-1,K)-HNEW(J,I,K))
C
        DO IZ2 = 1, IZ
          Q = Q + SWICUMCC(J,I-1,K,IZ) * DELNUS(IZ2) *
     &            (ZETA(J,I-1,K,IZ2)-ZETA(J,I,K,IZ2))
        END DO
C
        Q = Q - SWICC(J,I-1,K,IZ) * EPS(IZ) *
     &          (ZETA(J,I-1,K,IZ)-ZETA(J,I,K,IZ))
        Q = Q + SWICC(J,I-1,K,IZ) * EPS(IZ) *
     &          (ZETA(J,I-1,K,IZ+1)-ZETA(J,I,K,IZ+1))

C
        DO IZ2 = IZ + 1, NZONES
          Q = Q + SWICUMCC(J,I-1,K,IZ2) * DELNUS(IZ2) *
     &            (ZETA(J,I-1,K,IZ2)-ZETA(J,I,K,IZ2))
          Q = Q - SWICC(J,I-1,K,IZ2) * EPS(IZ2) *
     &            (ZETA(J,I-1,K,IZ2)-ZETA(J,I,K,IZ2))
          Q = Q + SWICC(J,I-1,K,IZ2) * EPS(IZ2) *
     &            (ZETA(J,I-1,K,IZ2+1)-ZETA(J,I,K,IZ2+1))
        END DO
C
C---------RETURN
        RETURN
      END SUBROUTINE SSWI2_QC
C
C
      SUBROUTINE SSWI2_SD(J,I,NCOL,NROW,IPOS,FAC,B,CRLAY,CCLAY,VAR)
C
C     ******************************************************************
C     SWI2 PACKAGE - CALCULATE FLUX IN THE COLUMN AND ROW DIRECTIONS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
        USE GWFSWIMODULE, ONLY: VERSIZE
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: J,I
        INTEGER, INTENT(IN) :: NCOL,NROW
        INTEGER, DIMENSION(NCOL,NROW), INTENT(IN) :: IPOS
        REAL, INTENT(IN) :: FAC
        REAL(KIND=VERSIZE), DIMENSION(NCOL,NROW), INTENT(INOUT) :: B
        REAL, DIMENSION(NCOL,NROW), INTENT(IN)    :: CRLAY
        REAL, DIMENSION(NCOL,NROW), INTENT(IN)    :: CCLAY
        DOUBLEPRECISION, DIMENSION(NCOL,NROW), INTENT(IN) :: VAR
C       + + + LOCAL DEFINITIONS + + +
C       + + + CODE + + +
C
C---------CALCULATE FLUX IN COLUMN DIRECTION
C         (LEFT FACE)
        IF (J.NE.1) THEN
          IF (IPOS(J-1,I).EQ.0) THEN
            B(J,I) = B(J,I)- FAC*CRLAY(J-1,I)*(VAR(J-1,I)-VAR(J,I))
          END IF
        END IF
C         (RIGHT FACE)
        IF (J.NE.NCOL) THEN
          IF (IPOS(J+1,I).EQ.0) THEN
            B(J,I) = B(J,I)-FAC*CRLAY(J,I)*(VAR(J+1,I)-VAR(J,I))
          END IF
        END IF
C
C---------CALCULATE FLUX IN ROW DIRECTION
C         (BACK FACE)
        IF (I.NE.1) THEN
          IF (IPOS(J,I-1).EQ.0) THEN
            B(J,I) = B(J,I)-FAC*CCLAY(J,I-1)*(VAR(J,I-1)-VAR(J,I))
          END IF
        END IF
C         (FRONT FACE)
        IF (I.NE.NROW) THEN
          IF (IPOS(J,I+1).EQ.0) THEN
            B(J,I) = B(J,I)-FAC*CCLAY(J,I)*(VAR(J,I+1)-VAR(J,I))
          END IF
        END IF
C
C---------RETURN
        RETURN
      END SUBROUTINE SSWI2_SD
C
C
      SUBROUTINE SSWI2_SR(J,I,NCOL,NROW,IPOS,FAC,B,CRLAY,CCLAY,VAR)
C
C     ******************************************************************
C     SWI2 PACKAGE - CALCULATE FLUX IN THE COLUMN AND ROW DIRECTIONS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
        USE GWFSWIMODULE, ONLY: VERSIZE
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: J,I
        INTEGER, INTENT(IN) :: NCOL,NROW
        INTEGER, DIMENSION(NCOL,NROW), INTENT(IN) :: IPOS
        REAL, INTENT(IN) :: FAC
        REAL(KIND=VERSIZE), DIMENSION(NCOL,NROW), INTENT(INOUT) :: B
        REAL, DIMENSION(NCOL,NROW), INTENT(IN)    :: CRLAY
        REAL, DIMENSION(NCOL,NROW), INTENT(IN)    :: CCLAY
        REAL, DIMENSION(NCOL,NROW), INTENT(IN)    :: VAR
C       + + + LOCAL DEFINITIONS + + +
C       + + + CODE + + +
C
C----------CALCULATE FLUX IN COLUMN DIRECTION
C         (LEFT FACE)
        IF (J.NE.1) THEN
          IF (IPOS(J-1,I).EQ.0) THEN
            B(J,I) = B(J,I)- FAC*CRLAY(J-1,I)*(VAR(J-1,I)-VAR(J,I))
          END IF
        END IF
C         (RIGHT FACE)
        IF (J.NE.NCOL) THEN
          IF (IPOS(J+1,I).EQ.0) THEN
            B(J,I) = B(J,I)-FAC*CRLAY(J,I)*(VAR(J+1,I)-VAR(J,I))
          END IF
        END IF

C
C--------CALCULATE FLUX IN ROW DIRECTION
C        (BACK FACE)
        IF (I.NE.1) THEN
          IF (IPOS(J,I-1).EQ.0) THEN
            B(J,I) = B(J,I)-FAC*CCLAY(J,I-1)*(VAR(J,I-1)-VAR(J,I))
          END IF
        END IF
C        (FRONT FACE)
        IF (I.NE.NROW) THEN
          IF (IPOS(J,I+1).EQ.0) THEN
            B(J,I) = B(J,I)-FAC*CCLAY(J,I)*(VAR(J,I+1)-VAR(J,I))
          END IF
        END IF
C
C---------RETURN
        RETURN
      END SUBROUTINE SSWI2_SR
C
C
      SUBROUTINE SSWI2_ZETAADJ(Kkstp,Kkper)
C
C     ******************************************************************
C     POST FLOW SOLUTION AND ZETA SOLUTION ADJUSTMENT OF ZETA SURFACES
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
        USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IFREFM,
     2                        LBOTM,BOTM,
     3                        CR,CC,CV,HCOF,RHS,
     4                        DELR,DELC,IBOUND,HNEW,HOLD,
     5                        BUFF,ISSFLG,NSTP
        USE GWFBASMODULE, ONLY: DELT,HDRY,TOTIM,IHDDFL,IBUDFL
        USE GWFSWIMODULE
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Kkstp
        INTEGER, INTENT(IN) :: Kkper
C       + + + LOCAL DEFINITIONS + + +
        CHARACTER*24 :: ZETANAME
        INTEGER :: i, j, k
        INTEGER :: iz, iz2, iz3
        INTEGER :: izrev
        INTEGER :: icount
        INTEGER :: iplz
        REAL :: zt, zb, zetac, zdiff, zetaavg
        DOUBLEPRECISION :: qztop, sszxa, dz
        REAL :: t2
        REAL :: s0, s1, s2, d0, d1, d2, dzeta1, dzeta2, b2, dzetamax
        INTEGER :: kk1, kk2
        DOUBLEPRECISION :: ht1, ht2
        DOUBLEPRECISION :: bt1, bt2
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C
C---------CALCULATE INSTANTANEOUS ZONE MIXING
        CALL SSWI2_IMIX(ZONEIMIX)
C
C---------CALCULATE PRE TIP TOE TRACKING CHANGE IN ZONE THICKNESS (ZONECHG1)
        CALL SSWI2_ZCHG(ZONECHG1)
C
        MOVETIPTOE: IF ( ALPHA.GT.0.0 ) THEN
C
C-----------VERTICAL MOVEMENT OF SURFACES WHEN A POSITIVE ALPHA IS SPECIFIED
          CALL SSWI2_VERTMOVE(Kkstp,Kkper)
C
C-----------HORIZONTAL MOVEMENT OF SURFACES WHEN A POSITIVE ALPHA IS SPECIFIED
          CALL SSWI2_HORZMOVE(Kkstp,Kkper)
C
C-----------CHECK WHETHER ANYWHERE THE THICKNESS GETS TOO THIN
          CALL SSWI2_ZETACLIP()
C
C-----------MODIFY ZETA ANYWHERE THE SURFACES ARE CROSSING
          CALL SSWI2_ZETACROSS()
C
C-----------ADJUST TIP AND TOE FOR CELLS WERE CURRENT CELL IS AT THE TOP OR BOTTOM
C           AND THE ADJACENT CELL IS AT THE BOTTOM OR TOP, RESPECTIVELY.
          CALL SSWI2_ANTILOCKMIN(Kkstp,Kkper)
C
        END IF MOVETIPTOE ! this loop done only when ALPHA > 0
C
C---------CALCULATE POST TIP TOE TRACKING CHANGE IN ZONE THICKNESS (ZONECHG2)
        CALL SSWI2_ZCHG(ZONECHG2)
C
C---------RESET ZETASWITS0 TO ZETA
        RSTZETASWITS0: DO iz = 1, NZONES
          DO k = 1, NLAY
            DO i = 1, NROW
              DO j = 1, NCOL
                ZETASWITS0(j,i,k,iz) = ZETA(j,i,k,iz)
              END DO
            END DO
          END DO
        END DO RSTZETASWITS0
C
C---------RETURN
        RETURN
      END SUBROUTINE SSWI2_ZETAADJ
C
C
      SUBROUTINE SSWI2_VERTMOVE(Kkstp,Kkper)
C
C     ******************************************************************
C     VERTICAL MOVEMENT OF SURFACES
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
        USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IFREFM,
     2                        LBOTM,BOTM,
     3                        CR,CC,CV,HCOF,RHS,
     4                        DELR,DELC,IBOUND,HNEW,HOLD,
     5                        BUFF,ISSFLG,NSTP
        USE GWFBASMODULE, ONLY: DELT,HDRY,TOTIM,IHDDFL,IBUDFL
        USE GWFSWIMODULE
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Kkstp
        INTEGER, INTENT(IN) :: Kkper
C       + + + LOCAL DEFINITIONS + + +
        CHARACTER*24 :: ZETANAME
        INTEGER :: i, j, k
        INTEGER :: iz, iz2, iz3
        INTEGER :: izrev
        INTEGER :: icount
        INTEGER :: iplz
        REAL :: zt, zb, zetac, zdiff, zetaavg
        DOUBLEPRECISION :: qztop, sszxa, dz
        REAL :: t2
        REAL :: s0, s1, s2, d0, d1, d2, dzeta1, dzeta2, b2, dzetamax
        INTEGER :: kk1, kk2
        DOUBLEPRECISION :: ht1, ht2
        DOUBLEPRECISION :: bt1, bt2
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C
C.........VERTICAL MOVEMENT OF SURFACES
          VZTIPTOE: DO k = 1, NLAY
            VIZTIPTOE: DO iz = 2, NZONES
              VITIPTOE: DO i = 1, NROW
                VJTIPTOE: DO j = 1, NCOL
C-------------------SKIP INACTIVE CELLS
                  IF ( IBOUND(J,I,K).EQ.0 ) CYCLE VJTIPTOE
C-------------------SET TEMPORARY ZETA VARIABLES
                  zt    = ZETA(j,i,k,1)
                  zb    = ZETA(j,i,k,NZONES+1)
                  zetac = ZETA(j,i,k,iz)
C-------------------SKIP CELLS WHERE HEAD IS BELOW THE BOTTOM  OF THE LAYER
                  ht1 = HNEW(J,I,K)
                  bt1 = REAL( BOTM(J,I,LBOTM(K)), 8 )
                  IF ( ht1.LT.bt1 ) CYCLE VJTIPTOE
C-------------------CALCULATE qztop
                  qztop = 0.0D0
                  IF ( k.GT.1 ) THEN
                    IF ( IBOUND(J,I,K-1).NE.0 ) THEN
C-----------------------CALCULATE qztop IF HEAD IN OVERLYING LAYER (k-1) ABOVE THE CELL BOTTOM
C                       AND HEAD IN LAYER k ABOVE THE CELL BOTTOM - FOR MODFLOW-NWT
                      kk1 = LBOTM(k-1)
                      kk2 = LBOTM(k)
                      bt1 = BOTM(J,I,kk1)
                      bt2 = BOTM(J,I,kk2)
                      ht1 = HNEW(J,I,k-1)
                      ht2 = HNEW(J,I,k)
                      IF ( ht1.GE.bt1 .AND. ht2.GE.bt2 ) THEN
                        qztop = CV(j,i,k-1) * 
     2                          (HNEW(j,i,k)-HNEW(j,i,k-1)) - 
     3                          QLEXTRA(j,i,k-1)
                      END IF
                    END IF
                  END IF
C-------------------MOVE TO UPPER OR LOWER LAYER
                  IF( NSWIOPT.GE.0 )THEN
                    IF ( k.NE.1 ) THEN  !JDH - 01/02/2012
                      IF ( IPLPOS(j,i,k,iz).EQ.1 ) THEN
                        IF (IPLPOS(j,i,k-1,iz).EQ.2) THEN
                          IF ( qztop.GT.0. ) THEN
                            sszxa = DELR(j)*DELC(i)*SSZ(j,i,k-1)
                            sszxa = 1.0 / sszxa
                            dz = REAL( SWIDELT, 8) * qztop * sszxa
                            ZETA(j,i,k-1,iz) =
     2                        ZETA(j,i,k-1,NZONES+1) + dz
                          END IF
                        END IF
                      END IF
                      IF ( IPLPOS(j,i,k,iz).EQ.1 ) THEN
                        IF (IPLPOS(j,i,k-1,iz).EQ.2) THEN
                          IF ( qztop.LT.0. ) THEN
                            sszxa = DELR(j)*DELC(i)*SSZ(j,i,k)
                            sszxa = 1.0 / sszxa
                            dz = REAL( SWIDELT, 8) * qztop * sszxa
                            ZETA(j,i,k,iz) =
     2                        ZETA(j,i,k,1) + dz
                          END IF
                        END IF
                      END IF
                    END IF
                  END IF
               END DO VJTIPTOE
              END DO VITIPTOE
            END DO VIZTIPTOE
          END DO VZTIPTOE
C-----------WRITE ZETA TO UNFORMATTED FILE
          IF ( IHDDFL.GT.0 .AND. ISWIZT.LT.0 ) THEN
            DO iz = 2, NZONES
              WRITE(ZETANAME,2221) iz-1
              CALL UBUDSV(kkstp,Kkper,ZETANAME,
     &                    ABS(ISWIZT),ZETA(1:NCOL,1:NROW,1:NLAY,iz),
     &                    NCOL,NROW,NLAY,IOUT)
            END DO
          END IF
2221  FORMAT('  TLAYZETASRF ',I2)
C
C
C---------RETURN
        RETURN
      END SUBROUTINE SSWI2_VERTMOVE
C
C
      SUBROUTINE SSWI2_HORZMOVE(Kkstp,Kkper)
C
C     ******************************************************************
C     HORIZONTAL MOVEMENT OF SURFACES
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
        USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IFREFM,
     2                        LBOTM,BOTM,
     3                        CR,CC,CV,HCOF,RHS,
     4                        DELR,DELC,IBOUND,HNEW,HOLD,
     5                        BUFF,ISSFLG,NSTP
        USE GWFBASMODULE, ONLY: DELT,HDRY,TOTIM,IHDDFL,IBUDFL
        USE GWFSWIMODULE
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Kkstp
        INTEGER, INTENT(IN) :: Kkper
C       + + + LOCAL DEFINITIONS + + +
        CHARACTER*24 :: ZETANAME
        INTEGER :: i, j, k
        INTEGER :: iz, iz2, iz3
        INTEGER :: izrev
        INTEGER :: icount
        INTEGER :: iplz
        REAL :: zt, zb, zetac, zdiff, zetaavg
        DOUBLEPRECISION :: qztop, sszxa, dz
        REAL :: t2
        REAL :: s0, s1, s2, d0, d1, d2, dzeta1, dzeta2, b2, dzetamax
        INTEGER :: kk1, kk2
        DOUBLEPRECISION :: ht1, ht2
        DOUBLEPRECISION :: bt1, bt2
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C
          ZTIPTOEFS: DO k = 1, NLAY
            IZTIPTOEFS: DO iz = 2, NZONES
              ITIPTOEFS: DO i = 1, NROW
                JTIPTOEFS: DO j = 1, NCOL
C-------------------SKIP INACTIVE CELLS
                  IF ( IBOUND(J,I,K).EQ.0 ) CYCLE JTIPTOEFS
C-------------------SET TEMPORARY ZETA VARIABLES
                  zt    = ZETA(j,i,k,1)
                  zb    = ZETA(j,i,k,NZONES+1)
                  zetac = ZETA(j,i,k,iz)
C-------------------SKIP CELLS WHERE HEAD IS BELOW THE BOTTOM  OF THE LAYER
                  ht1 = HNEW(J,I,K)
                  bt1 = REAL( BOTM(J,I,LBOTM(K)), 8 )
                  IF ( ht1.LT.bt1 ) CYCLE JTIPTOEFS
C
C-------------------ONLY EVALUATE TIP AND TOE MOVEMENT TO ADJACENT CELLS FOR CELLS WHERE
C                   ZETA SURFACE IS NOT AT THE TOP (ILPOS=1) OR BOTTOM (ILPOS=2) OF THE LAYER
                  LACTIVE: IF (IPLPOS(j,i,k,iz).EQ.0) THEN
C--------------------ADJUST TIPS AND TOES FOR EACH COLUMN BUT THE FIRST AND LAST
C                    d1, s1: for the current (active) tip- or toe-cell
C                    d2, s2: for the adjacent inactive cell (left, right, back, front)
C                    d0, s0: for the (possibly active) cell on the other side (right, left, front, back)
                    LCOL: IF ((j.NE.1).AND.(j.NE.NCOL)) THEN
C-----------------------LEFT FACE
                      zetac = ZETA(j,i,k,iz)
                      d0  = DELR(j+1)
                      s0  = SSZ(j+1,i,k)
                      d1  = DELR(j)
                      s1  = SSZ(j,i,k)
                      d2  = DELR(j-1)
                      s2  = SSZ(j-1,i,k)
                      b2  = ZETA(j-1,i,k,NZONES+1)
                      t2  = ZETA(j-1,i,k,1)

                      IF ( IPLPOS(j-1,i,k,iz).EQ.1 ) THEN
                        dzetamax = TIPSLOPE * 0.5*(d1+d2)
                        dzeta1 =  ALPHA * dzetamax * (s2*d2) /
     2                                      (s1*d1 + s2*d2)
                        dzeta2 =  ALPHA * dzetamax * (s1*d1) /
     2                                      (s1*d1 + s2*d2)
                        IF ((t2-zetac).GT.dzetamax) THEN
                          ZETA(j,i,k,iz)   = zetac + dzeta1
                          ZETA(j-1,i,k,iz) = t2 - dzeta2
                        ELSEIF ((zetac-zb).LT.(BETA*dzeta2)) THEN
                          IF (IPLPOS(j+1,i,k,iz).EQ.0) THEN
                            ZETA(j+1,i,k,iz)=ZETA(j+1,i,k,iz)+
     2                            (zetac-zb) * (d1*s1) / (d0*s0)
                            ZETA(j,i,k,iz)=zb
                          END IF
                        END IF

                      ELSEIF ( IPLPOS(j-1,i,k,iz).EQ.2 ) THEN
                        dzetamax = TOESLOPE * 0.5*(d1+d2)
                        dzeta1 =  ALPHA * dzetamax * (s2*d2) /
     2                                      (s1*d1 + s2*d2)
                        dzeta2 =  ALPHA * dzetamax * (s1*d1) /
     2                                      (s1*d1 + s2*d2)
                        IF ((zetac-b2).GT.dzetamax) THEN
                          ZETA(j,i,k,iz)   = zetac - dzeta1
                          ZETA(j-1,i,k,iz)   = b2 + dzeta2
                        ELSEIF ((zetac-zb).LT.(BETA*dzeta2)) THEN
                          IF (IPLPOS(j+1,i,k,iz).EQ.0) THEN
                            ZETA(j+1,i,k,iz)=ZETA(j+1,i,k,iz)+
     2                            (zetac-zb) * (d1*s1) / (d0*s0)
                            ZETA(j,i,k,iz)=zb
                          END IF
                        END IF

                      END IF

C-----------------------RIGHT FACE
                      zetac = ZETA(j,i,k,iz)
                      d0  = DELR(j-1)
                      s0  = SSZ(j-1,i,k)
                      d1  = DELR(j)
                      s1  = SSZ(j,i,k)
                      d2  = DELR(j+1)
                      s2  = SSZ(j+1,i,k)
                      b2  = ZETA(j+1,i,k,NZONES+1)
                      t2  = ZETA(j+1,i,k,1)

                      IF ( IPLPOS(j+1,i,k,iz).EQ.1 ) THEN
                        dzetamax = TIPSLOPE * 0.5*(d1+d2)
                        dzeta1 =  ALPHA * dzetamax * (s2*d2) /
     2                                      (s1*d1 + s2*d2)
                        dzeta2 =  ALPHA * dzetamax * (s1*d1) /
     2                                      (s1*d1 + s2*d2)
                        IF ((t2-zetac).GT.dzetamax) THEN
                          ZETA(j,i,k,iz)   = zetac + dzeta1
                          ZETA(j+1,i,k,iz)   = t2 - dzeta2
                        ELSEIF ((zetac-zb).LT.(BETA*dzeta2)) THEN
                          IF (IPLPOS(j-1,i,k,iz).EQ.0) THEN
                            ZETA(j-1,i,k,iz)=ZETA(j-1,i,k,iz)+
     2                            (zetac-zb) * (d1*s1) / (d0*s0)
                            ZETA(j,i,k,iz)=zb
                          END IF
                        END IF

                      ELSEIF ( IPLPOS(j+1,i,k,iz).EQ.2 ) THEN
                        dzetamax = TOESLOPE * 0.5*(d1+d2)
                        dzeta1 =  ALPHA * dzetamax * (s2*d2) /
     2                                      (s1*d1 + s2*d2)
                        dzeta2 =  ALPHA * dzetamax * (s1*d1) /
     2                                      (s1*d1 + s2*d2)
                        IF ((zetac-b2).GT.dzetamax) THEN
                          ZETA(j,i,k,iz)   = zetac - dzeta1
                          ZETA(j+1,i,k,iz)   = b2 + dzeta2
                        ELSEIF ((zetac-zb).LT.(BETA*dzeta2)) THEN
                          IF (IPLPOS(j-1,i,k,iz).EQ.0) THEN
                            ZETA(j-1,i,k,iz)=ZETA(j-1,i,k,iz)+
     2                            (zetac-zb) * (d1*s1) / (d0*s0)
                            ZETA(j,i,k,iz)=zb
                          END IF
                        END IF

                      END IF
                    END IF LCOL
C
C---------------------ADJUST TIPS AND TOES FOR EACH ROW BUT THE FIRST AND LAST
                    LROW: IF ((i.GT.1).AND.(i.LT.NROW)) THEN

C-----------------------BACK FACE
                      zetac = ZETA(j,i,k,iz)
                      d0 = DELC(i+1)
                      s0 = SSZ(j,i+1,k)
                      d1 = DELC(i)
                      s1 = SSZ(j,i,k)
                      d2 = DELC(i-1)
                      s2 = SSZ(j,i-1,k)
                      b2 = ZETA(j,i-1,k,NZONES+1)
                      t2 = ZETA(j,i-1,k,1)

                      IF ( IPLPOS(j,i-1,k,iz).EQ.1 ) THEN
                        dzetamax = TIPSLOPE * 0.5*(d1+d2)
                        dzeta1 =  ALPHA * dzetamax * (s2*d2) /
     2                                      (s1*d1 + s2*d2)
                        dzeta2 =  ALPHA * dzetamax * (s1*d1) /
     2                                      (s1*d1 + s2*d2)
                        IF ((t2-zetac).GT.dzetamax) THEN
                         ZETA(j,i,k,iz)   = zetac + dzeta1
                         ZETA(j,i-1,k,iz)   = t2 - dzeta2
                        ELSEIF ((zetac-zb).LT.(BETA*dzeta2)) THEN
                          IF (IPLPOS(j,i+1,k,iz).EQ.0) THEN
                            ZETA(j,i+1,k,iz)=ZETA(j,i+1,k,iz)+
     2                          (zetac-zb) * (d1*s1) / (d0*s0)
                            ZETA(j,i,k,iz)=zb
                          END IF
                        END IF

                      ELSEIF ( IPLPOS(j,i-1,k,iz).EQ.2 ) THEN
                        dzetamax = TOESLOPE * 0.5*(d1+d2)
                        dzeta1 =  ALPHA * dzetamax * (s2*d2) /
     2                                      (s1*d1 + s2*d2)
                        dzeta2 =  ALPHA * dzetamax * (s1*d1) /
     2                                      (s1*d1 + s2*d2)
                        IF ((zetac-b2).GT.dzetamax) THEN
                         ZETA(j,i,k,iz)   = zetac - dzeta1
                         ZETA(j,i-1,k,iz)   = b2 + dzeta2
                        ELSEIF ((zetac-zb).LT.(BETA*dzeta2)) THEN
                          IF (IPLPOS(j,i+1,k,iz).EQ.0) THEN
                            ZETA(j,i+1,k,iz)=ZETA(j,i+1,k,iz)+
     2                          (zetac-zb) * (d1*s1) / (d0*s0)
                            ZETA(j,i,k,iz)=zb
                          END IF
                        END IF
                      END IF
C-----------------------FRONT FACE
                      zetac = ZETA(j,i,k,iz)
                      d0 = DELC(i-1)
                      s0 = SSZ(j,i-1,k)
                      d1 = DELC(i)
                      s1 = SSZ(j,i,k)
                      d2 = DELC(i+1)
                      s2 = SSZ(j,i+1,k)
                      b2 = ZETA(j,i+1,k,NZONES+1)
                      t2 = ZETA(j,i+1,k,1)

                      IF ( IPLPOS(j,i+1,k,iz).EQ.1 ) THEN
                        dzetamax = TIPSLOPE * 0.5*(d1+d2)
                        dzeta1 =  ALPHA * dzetamax * (s2*d2) /
     2                                      (s1*d1 + s2*d2)
                        dzeta2 =  ALPHA * dzetamax * (s1*d1) /
     2                                      (s1*d1 + s2*d2)
                        IF ((t2-zetac).GT.dzetamax) THEN
                         ZETA(j,i,k,iz)   = zetac + dzeta1
                         ZETA(j,i+1,k,iz)   = t2 - dzeta2
                        ELSEIF ((zetac-zb).LT.(BETA*dzeta2)) THEN
                          IF (IPLPOS(j,i-1,k,iz).EQ.0) THEN
                            ZETA(j,i-1,k,iz)=ZETA(j,i-1,k,iz)+
     2                          (zetac-zb) * (d1*s1) / (d0*s0)
                            ZETA(j,i,k,iz)=zb
                          END IF
                        END IF


                      ELSEIF ( IPLPOS(j,i+1,k,iz).EQ.2 ) THEN
                        dzetamax = TOESLOPE * 0.5*(d1+d2)
                        dzeta1 =  ALPHA * dzetamax * (s2*d2) /
     2                                      (s1*d1 + s2*d2)
                        dzeta2 =  ALPHA * dzetamax * (s1*d1) /
     2                                      (s1*d1 + s2*d2)
                        IF ((zetac-b2).GT.dzetamax) THEN
                         ZETA(j,i,k,iz)   = zetac - dzeta1
                         ZETA(j,i+1,k,iz)   = b2 + dzeta2
                        ELSEIF ((zetac-zb).LT.(BETA*dzeta2)) THEN
                          IF (IPLPOS(j,i-1,k,iz).EQ.0) THEN
                            ZETA(j,i-1,k,iz)=ZETA(j,i-1,k,iz)+
     2                          (zetac-zb) * (d1*s1) / (d0*s0)
                            ZETA(j,i,k,iz)=zb
                          END IF
                        END IF


                      END IF
                    END IF LROW
                  END IF LACTIVE
                END DO JTIPTOEFS
              END DO ITIPTOEFS
            END DO IZTIPTOEFS
          END DO ZTIPTOEFS

C-----------WRITE ZETA TO UNFORMATTED FILE
          IF ( IHDDFL.GT.0 .AND. ISWIZT.LT.0 ) THEN
            DO iz = 2, NZONES
              WRITE(ZETANAME,2222) iz-1
              CALL UBUDSV(kkstp,Kkper,ZETANAME,
     &                    ABS(ISWIZT),ZETA(1:NCOL,1:NROW,1:NLAY,iz),
     &                    NCOL,NROW,NLAY,IOUT)
            END DO
          END IF
2222  FORMAT('  TPTOZETASRF ',I2)
C
C---------RETURN
        RETURN
      END SUBROUTINE SSWI2_HORZMOVE
C
C
      SUBROUTINE SSWI2_ZETACLIP()
C
C     ******************************************************************
C     IF ZETA IS ABOVE TOP OR BELOW BOTTOM THEN CLIP THIS WATER VOLUME
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
        USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IFREFM,
     2                        LBOTM,BOTM,
     3                        CR,CC,CV,HCOF,RHS,
     4                        DELR,DELC,IBOUND,HNEW,HOLD,
     5                        BUFF,ISSFLG,NSTP
        USE GWFBASMODULE, ONLY: DELT,HDRY,TOTIM,IHDDFL,IBUDFL
        USE GWFSWIMODULE
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
C       + + + LOCAL DEFINITIONS + + +
        CHARACTER*24 :: ZETANAME
        INTEGER :: i, j, k
        INTEGER :: iz, iz2, iz3
        INTEGER :: izrev
        INTEGER :: icount
        INTEGER :: iplz
        REAL :: zt, zb, zetac, zdiff, zetaavg
        DOUBLEPRECISION :: qztop, sszxa, dz
        REAL :: t2
        REAL :: s0, s1, s2, d0, d1, d2, dzeta1, dzeta2, b2, dzetamax
        INTEGER :: kk1, kk2
        DOUBLEPRECISION :: ht1, ht2
        DOUBLEPRECISION :: bt1, bt2
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C
C-----------CHECK WHETHER ANYWHERE THE THICKNESS GETS TOO THIN
          KZTT: DO k=1,NLAY
            IZTT: DO i=1,NROW
              JZTT: DO j=1,NCOL
                IZZTT: DO iz=2,NZONES
                  IF (IPLPOS(j,i,k,iz).EQ.0) THEN
                    IF ( ZETA(j,i,k,iz).LT.ZETA(j,i,k,NZONES+1)) THEN
                      ZETA(j,i,k,iz)=ZETA(j,i,k,NZONES+1)
                    END IF
                    IF ( ZETA(j,i,k,iz).GT.ZETA(j,i,k,1) ) THEN
                      ZETA(j,i,k,iz) = ZETA(j,i,k,1)
                    END IF
                  END IF
                END DO IZZTT
              END DO JZTT
            END DO IZTT
          END DO KZTT
C
C---------RETURN
        RETURN
      END SUBROUTINE SSWI2_ZETACLIP
C
C
      SUBROUTINE SSWI2_ZETACROSS()
C
C     ******************************************************************
C     CORRECT IF SURFACES HAVE CROSSED
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
        USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IFREFM,
     2                        LBOTM,BOTM,
     3                        CR,CC,CV,HCOF,RHS,
     4                        DELR,DELC,IBOUND,HNEW,HOLD,
     5                        BUFF,ISSFLG,NSTP
        USE GWFBASMODULE, ONLY: DELT,HDRY,TOTIM,IHDDFL,IBUDFL
        USE GWFSWIMODULE
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
C       + + + LOCAL DEFINITIONS + + +
        CHARACTER*24 :: ZETANAME
        INTEGER :: i, j, k
        INTEGER :: iz, iz2, iz3
        INTEGER :: izrev
        INTEGER :: icount
        INTEGER :: iplz
        REAL :: zt, zb, zetac, zdiff, zetaavg
        DOUBLEPRECISION :: qztop, sszxa, dz
        REAL :: t2
        REAL :: s0, s1, s2, d0, d1, d2, dzeta1, dzeta2, b2, dzetamax
        INTEGER :: kk1, kk2
        DOUBLEPRECISION :: ht1, ht2
        DOUBLEPRECISION :: bt1, bt2
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C
C-----------MODIFY ZETA ANYWHERE THE SURFACES ARE CROSSING        
          KPX: DO k=1,NLAY
            IPX: DO i=1,NROW
              JPX: DO j=1,NCOL
                IZPX: DO iz=2,NZONES-1
                  IF ((ZETA(j,i,k,iz)-ZETA(j,i,k,iz+1)).LT.
     &                SWISMALL) THEN
                    zetaavg = 0.5 * (ZETA(j,i,k,iz)+ZETA(j,i,k,iz+1))
                    ZETA(j,i,k,iz)=zetaavg
                    ZETA(j,i,k,iz+1)=zetaavg
                    IZ2PX: DO iz2=2,iz-1
                      izrev = iz+1 - iz2
                      zdiff = ZETA(j,i,k,izrev)-ZETA(j,i,k,iz+1)
                      IF (zdiff.LT.SWISMALL) THEN
                        zetaavg = 0.
                        icount = 0
                        DO iz3 = izrev, iz+1
                          icount = icount + 1
                          zetaavg = zetaavg + ZETA(j,i,k,iz3)
                        END DO
                        IF (icount.NE.0) THEN
                          zetaavg = zetaavg / icount
                          DO iz3 = izrev, iz+1
                            ZETA(j,i,k,iz3) = zetaavg
                          END DO
                        END IF
                      END IF
                    END DO IZ2PX
                  END IF
                END DO IZPX
              END DO JPX
            END DO IPX
          END DO KPX

C
C---------RETURN
        RETURN
      END SUBROUTINE SSWI2_ZETACROSS
C
C
      SUBROUTINE SSWI2_ANTILOCKMIN(Kkstp,Kkper)
C
C     ******************************************************************
C     PREVENT SURFACES FROM LOCKING
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
        USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IFREFM,
     2                        LBOTM,BOTM,
     3                        CR,CC,CV,HCOF,RHS,
     4                        DELR,DELC,IBOUND,HNEW,HOLD,
     5                        BUFF,ISSFLG,NSTP
        USE GWFBASMODULE, ONLY: DELT,HDRY,TOTIM,IHDDFL,IBUDFL
        USE GWFSWIMODULE
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Kkstp
        INTEGER, INTENT(IN) :: Kkper
C       + + + LOCAL DEFINITIONS + + +
        CHARACTER*24 :: ZETANAME
        INTEGER :: i, j, k
        INTEGER :: iz, iz2, iz3
        INTEGER :: izrev
        INTEGER :: icount
        INTEGER :: iplz
        REAL :: zt, zb, zetac, zdiff, zetaavg
        DOUBLEPRECISION :: qztop, sszxa, dz
        REAL :: t1, t2
        REAL :: s0, s1, s2, d0, d1, d2, dzeta1, dzeta2, b2, dzetamax
        INTEGER :: kk1, kk2
        DOUBLEPRECISION :: ht1, ht2
        DOUBLEPRECISION :: bt1, bt2
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C
C----------ADJUST TIP AND TOE FOR CELLS WERE CURRENT CELL IS AT THE TOP OR BOTTOM
C          AND THE ADJACENT CELL IS AT THE BOTTOM OR TOP, RESPECTIVELY.
         ZTIPTOEAL: DO k = 1, NLAY
            IZTIPTOEAL: DO iz = 2, NZONES
              ITIPTOEAL: DO i = 1, NROW
                JTIPTOEAL: DO j = 1, NCOL
C-------------------SKIP INACTIVE CELLS
                  IF ( IBOUND(J,I,K).EQ.0 ) CYCLE JTIPTOEAL
C-------------------SET TEMPORARY ZETA VARIABLES
                  zt    = ZETA(j,i,k,1)
                  zb    = ZETA(j,i,k,NZONES+1)
                  zetac = ZETA(j,i,k,iz)
                  iplz  = IPLPOS(j,i,k,iz)
C
C-------------------ONLY EVALUATE TIP AND TOE MOVEMENT TO ADJACENT CELLS FOR CELLS WHERE
C                   ZETA SURFACE IS AT THE TOP (ILPOS=1) OR BOTTOM (ILPOS=2) OF THE LAYER
                  LEDGE: IF (iplz.NE.0 .AND. iplz.NE.3 ) THEN
C--------------------ADJUST TIPS AND TOES FOR EACH COLUMN BUT THE FIRST AND LAST
                    LCOLAL: IF ((j.NE.1).AND.(j.NE.NCOL)) THEN
C----------------------LEFT FACE
                      IF ( IBOUND(j-1,i,k).NE.0 ) THEN
                        d1  = DELR(j)
                        s1  = SSZ(j,i,k)
                        kk1 = LBOTM(k)
                        t1  = (BOTM(j,i,kk1-1) - BOTM(j,i,kk1)) *
     2                        ALPHA
                        d2  = DELR(j-1)
                        s2  = SSZ(j-1,i,k)
                        kk2 = LBOTM(k)
                        t2  = (BOTM(j-1,i,kk2-1) - BOTM(j-1,i,kk2)) *
     2                        ALPHA

                        dzetamax = SWILOCK
                        if (dzetamax.GT.t1 .OR. dzetamax.GT.t2) then
                          dzetamax = MIN(t1, t2)
                        end if
                        dzeta1 = dzetamax * (s2*d2) / (s1*d1 + s2*d2)
                        dzeta2 = dzetamax * (s1*d1) / (s1*d1 + s2*d2)

                        IF (IPLPOS(j-1,i,k,iz).EQ.1 .AND. 
     2                      iplz.EQ.2) THEN
                          ZETA(j,i,k,iz) = zetac + dzeta1
                          ZETA(j-1,i,k,iz) = ZETA(j-1,i,k,iz) - dzeta2

                        ELSEIF (IPLPOS(j-1,i,k,iz).EQ.2. AND.
     2                          iplz.EQ.1) THEN
                          ZETA(j,i,k,iz) = zetac - dzeta1
                          ZETA(j-1,i,k,iz) = ZETA(j-1,i,k,iz) + dzeta2
                        END IF
                      END IF
C----------------------RIGHT FACE
                      IF ( IBOUND(j+1,i,k).NE.0 ) THEN
                        d1  = DELR(j)
                        s1  = SSZ(j,i,k)
                        kk1 = LBOTM(k)
                        t1  = (BOTM(j,i,kk1-1) - BOTM(j,i,kk1)) *
     2                        ALPHA
                        d2  = DELR(j+1)
                        s2  = SSZ(j+1,i,k)
                        kk2 = LBOTM(k)
                        t2  = (BOTM(j+1,i,kk2-1) - BOTM(j+1,i,kk2)) *
     2                        ALPHA
                        
                        dzetamax = SWILOCK
                        if (dzetamax.GT.t1 .OR. dzetamax.GT.t2) then
                          dzetamax = MIN(t1, t2)
                        end if
                        dzeta1 = dzetamax * (s2*d2) / (s1*d1 + s2*d2)
                        dzeta2 = dzetamax * (s1*d1) / (s1*d1 + s2*d2)

                        IF (IPLPOS(j+1,i,k,iz).EQ.1 .AND. 
     2                      iplz.EQ.2) THEN
                          ZETA(j,i,k,iz) = zetac + dzeta1
                          ZETA(j+1,i,k,iz) = ZETA(j+1,i,k,iz) - dzeta2
                        ELSEIF (IPLPOS(j+1,i,k,iz).EQ.2 .AND.
     2                          iplz.EQ.1) THEN
                          ZETA(j,i,k,iz) = zetac - dzeta1
                          ZETA(j+1,i,k,iz) = ZETA(j+1,i,k,iz) + dzeta2
                        END IF
                      END IF
                    END IF LCOLAL
C
C---------------------ADJUST TIPS AND TOES FOR EACH ROW BUT THE FIRST AND LAST
                    LROWAL: IF ((i.GT.1).AND.(i.LT.NROW)) THEN
C-----------------------BACK FACE
                      IF ( IBOUND(j,i-1,k).NE.0 ) THEN
                        d1  = DELC(i)
                        s1  = SSZ(j,i,k)
                        kk1 = LBOTM(k)
                        t1  = (BOTM(j,i,kk1-1) - BOTM(j,i,kk1)) *
     2                        ALPHA
                        d2  = DELC(i-1)
                        s2  = SSZ(j,i-1,k)
                        kk2 = LBOTM(k)
                        t2  = (BOTM(j,i-1,kk2-1) - BOTM(j,i-1,kk2)) *
     2                        ALPHA
                        
                        dzetamax = SWILOCK
                        if (dzetamax.GT.t1 .OR. dzetamax.GT.t2) then
                          dzetamax = MIN(t1, t2)
                        end if
                        dzeta1 = dzetamax * (s2*d2) / (s1*d1 + s2*d2)
                        dzeta2 = dzetamax * (s1*d1) / (s1*d1 + s2*d2)
                        
                        IF (IPLPOS(j,i-1,k,iz).EQ.1 .AND. 
     2                      iplz.EQ.2) THEN
                          ZETA(j,i,k,iz)=zetac + dzeta1
                          ZETA(j,i-1,k,iz)= ZETA(j,i-1,k,iz) - dzeta2
                        ELSEIF (IPLPOS(j,i-1,k,iz).EQ.2 .AND.
     2                          iplz.EQ.1) THEN
                          ZETA(j,i,k,iz)=zetac - dzeta1
                          ZETA(j,i-1,k,iz)= ZETA(j,i-1,k,iz) + dzeta2
                        END IF
                      END IF
C-----------------------FRONT FACE
                      IF ( IBOUND(j,i+1,k).NE.0 ) THEN
                        d1  = DELC(i)
                        s1  = SSZ(j,i,k)
                        kk1 = LBOTM(k)
                        t1  = (BOTM(j,i,kk1-1) - BOTM(j,i,kk1)) *
     2                        ALPHA
                        d2  = DELC(i+1)
                        s2  = SSZ(j,i+1,k)
                        kk2 = LBOTM(k)
                        t2  = (BOTM(j,i+1,kk2-1) - BOTM(j,i+1,kk2)) *
     2                        ALPHA
                        
                        dzetamax = SWILOCK
                        if (dzetamax.GT.t1 .OR. dzetamax.GT.t2) then
                          dzetamax = MIN(t1, t2)
                        end if
                        dzeta1 = dzetamax * (s2*d2) / (s1*d1 + s2*d2)
                        dzeta2 = dzetamax * (s1*d1) / (s1*d1 + s2*d2)

                        IF (IPLPOS(j,i+1,k,iz).EQ.1 .AND. 
     2                      iplz.EQ.2) THEN
                          ZETA(j,i,k,iz)=zetac + dzeta1
                          ZETA(j,i+1,k,iz)= ZETA(j,i+1,k,iz) - dzeta2
                        ELSEIF (IPLPOS(j,i+1,k,iz).EQ.2 .AND.
     2                          iplz.EQ.1) THEN
                          ZETA(j,i,k,iz)=zetac - dzeta1
                          ZETA(j,i+1,k,iz)= ZETA(j,i+1,k,iz) + dzeta2
                        END IF
                      END IF
                    END IF LROWAL
                  END IF LEDGE
                END DO JTIPTOEAL
              END DO ITIPTOEAL
            END DO IZTIPTOEAL
          END DO ZTIPTOEAL

C-----------WRITE ZETA TO UNFORMATTED FILE
          IF ( IHDDFL.GT.0 .AND. ISWIZT.LT.0 ) THEN
            DO iz = 2, NZONES
              WRITE(ZETANAME,2223) iz-1
              CALL UBUDSV(kkstp,Kkper,ZETANAME,
     &                    ABS(ISWIZT),ZETA(1:NCOL,1:NROW,1:NLAY,iz),
     &                    NCOL,NROW,NLAY,IOUT)
            END DO
          END IF
2223  FORMAT('TPTOANTILOCKZ ',I2)
C
C---------RETURN
        RETURN
      END SUBROUTINE SSWI2_ANTILOCKMIN
C
C
      SUBROUTINE GWF2SWI2DA(Igrid)
C
C     ******************************************************************
C     DEALLOCATE SWI2 PACKAGE DATA FOR A GRID
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
        USE GWFSWIMODULE
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Igrid
C       + + + CODE + + +
        DEALLOCATE(GWFSWIDAT(Igrid)%NSRF)
        DEALLOCATE(GWFSWIDAT(Igrid)%ISTRAT)
        DEALLOCATE(GWFSWIDAT(Igrid)%NZONES)

        DEALLOCATE(GWFSWIDAT(Igrid)%IFIXED)
        
        DEALLOCATE(GWFSWIDAT(Igrid)%NADPTFLG)
        DEALLOCATE(GWFSWIDAT(Igrid)%NADPTMX)
        DEALLOCATE(GWFSWIDAT(Igrid)%NADPTMN)
        DEALLOCATE(GWFSWIDAT(Igrid)%ADPTFCT)
        DEALLOCATE(GWFSWIDAT(Igrid)%IADPT)
        DEALLOCATE(GWFSWIDAT(Igrid)%IADPTMOD)
        DEALLOCATE(GWFSWIDAT(Igrid)%ADPTVAL)
        DEALLOCATE(GWFSWIDAT(Igrid)%SWIDELT)
        DEALLOCATE(GWFSWIDAT(Igrid)%NADPTSUM)
        DEALLOCATE(GWFSWIDAT(Igrid)%RADPTSUM)
        DEALLOCATE(GWFSWIDAT(Igrid)%IADPTSUM)

        DEALLOCATE(GWFSWIDAT(Igrid)%NOBS)
        DEALLOCATE(GWFSWIDAT(Igrid)%IOBSHEADER)

        DEALLOCATE(GWFSWIDAT(Igrid)%ISWIZT)
        DEALLOCATE(GWFSWIDAT(Igrid)%ISWICB)
        DEALLOCATE(GWFSWIDAT(Igrid)%ISWIOBS)

        DEALLOCATE(GWFSWIDAT(Igrid)%NSWIOPT)

        DEALLOCATE(GWFSWIDAT(Igrid)%NLAYSWI)
C         SOLVER
        DEALLOCATE(GWFSWIDAT(Igrid)%NSOLVER)
        DEALLOCATE(GWFSWIDAT(Igrid)%IPRSOL)
        DEALLOCATE(GWFSWIDAT(Igrid)%MUTSOL)
C         SWI PARAMETERS
        DEALLOCATE(GWFSWIDAT(Igrid)%TOESLOPE)
        DEALLOCATE(GWFSWIDAT(Igrid)%TIPSLOPE)
        DEALLOCATE(GWFSWIDAT(Igrid)%ALPHA)
        DEALLOCATE(GWFSWIDAT(Igrid)%BETA)
        DEALLOCATE(GWFSWIDAT(Igrid)%ICONV)
        DEALLOCATE(GWFSWIDAT(Igrid)%IBO)
        DEALLOCATE(GWFSWIDAT(Igrid)%SWIHCOF)
        DEALLOCATE(GWFSWIDAT(Igrid)%SWISOLCR)
        DEALLOCATE(GWFSWIDAT(Igrid)%SWISOLCC)
        DEALLOCATE(GWFSWIDAT(Igrid)%SWISOLCV)
        DEALLOCATE(GWFSWIDAT(Igrid)%ZETA)
        DEALLOCATE(GWFSWIDAT(Igrid)%ZETAOLD)
        DEALLOCATE(GWFSWIDAT(Igrid)%ZETASWITS0)
        DEALLOCATE(GWFSWIDAT(Igrid)%ZONECHG1)
        DEALLOCATE(GWFSWIDAT(Igrid)%ZONECHG2)
        DEALLOCATE(GWFSWIDAT(Igrid)%ZONEIMIX)
        DEALLOCATE(GWFSWIDAT(Igrid)%SSZ)
        DEALLOCATE(GWFSWIDAT(Igrid)%EPS)
        DEALLOCATE(GWFSWIDAT(Igrid)%NUS)
        DEALLOCATE(GWFSWIDAT(Igrid)%DELNUS)
        DEALLOCATE(GWFSWIDAT(Igrid)%NUSRF)
        DEALLOCATE(GWFSWIDAT(Igrid)%SWICR)
        DEALLOCATE(GWFSWIDAT(Igrid)%SWICC)
        DEALLOCATE(GWFSWIDAT(Igrid)%SWICUMCR)
        DEALLOCATE(GWFSWIDAT(Igrid)%SWICUMCC)
        DEALLOCATE(GWFSWIDAT(Igrid)%NUTOP)
        DEALLOCATE(GWFSWIDAT(Igrid)%NUBOT)
        DEALLOCATE(GWFSWIDAT(Igrid)%QLEXTRA)
        DEALLOCATE(GWFSWIDAT(Igrid)%QREXTRA)
        DEALLOCATE(GWFSWIDAT(Igrid)%QFEXTRA)
        DEALLOCATE(GWFSWIDAT(Igrid)%QLEXTRACUM)
        DEALLOCATE(GWFSWIDAT(Igrid)%QREXTRACUM)
        DEALLOCATE(GWFSWIDAT(Igrid)%QFEXTRACUM)
        DEALLOCATE(GWFSWIDAT(Igrid)%BRHS)
        DEALLOCATE(GWFSWIDAT(Igrid)%DUM)
        DEALLOCATE(GWFSWIDAT(Igrid)%RHSPRESWI)
        DEALLOCATE(GWFSWIDAT(Igrid)%IPLPOS)
        DEALLOCATE(GWFSWIDAT(Igrid)%IZONENR)
        DEALLOCATE(GWFSWIDAT(Igrid)%NBDITEMS)
        DEALLOCATE(GWFSWIDAT(Igrid)%CUMBD)
        DEALLOCATE(GWFSWIDAT(Igrid)%INCBD)
        DEALLOCATE(GWFSWIDAT(Igrid)%RRATIN)
        DEALLOCATE(GWFSWIDAT(Igrid)%RRATOUT)
        DEALLOCATE(GWFSWIDAT(Igrid)%SWIOBS)
        DEALLOCATE(GWFSWIDAT(Igrid)%SWIDE4)
        DEALLOCATE(GWFSWIDAT(Igrid)%SWIPCG)
C
C-------RETURN
        RETURN
      END SUBROUTINE GWF2SWI2DA
C
C
      SUBROUTINE SGWF2SWI2PNT(Igrid)
C
C     ******************************************************************
C     SET POINTERS TO SWI2 PACKAGE DATA FOR A GRID
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
        USE GWFSWIMODULE
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Igrid
C       + + + CODE + + +
        NSRF=>GWFSWIDAT(Igrid)%NSRF
        ISTRAT=>GWFSWIDAT(Igrid)%ISTRAT
        NZONES=>GWFSWIDAT(Igrid)%NZONES

        IFIXED=>GWFSWIDAT(Igrid)%IFIXED
        
        NADPTFLG=>GWFSWIDAT(Igrid)%NADPTFLG
        NADPTMX=>GWFSWIDAT(Igrid)%NADPTMX
        NADPTMN=>GWFSWIDAT(Igrid)%NADPTMN
        ADPTFCT=>GWFSWIDAT(Igrid)%ADPTFCT
        IADPT=>GWFSWIDAT(Igrid)%IADPT
        IADPTMOD=>GWFSWIDAT(Igrid)%IADPTMOD
        ADPTVAL=>GWFSWIDAT(Igrid)%ADPTVAL
        SWIDELT=>GWFSWIDAT(Igrid)%SWIDELT
        NADPTSUM=>GWFSWIDAT(Igrid)%NADPTSUM
        RADPTSUM=>GWFSWIDAT(Igrid)%RADPTSUM
        IADPTSUM=>GWFSWIDAT(Igrid)%IADPTSUM

        NOBS=>GWFSWIDAT(Igrid)%NOBS
        IOBSHEADER=>GWFSWIDAT(Igrid)%IOBSHEADER

        ISWIZT=>GWFSWIDAT(Igrid)%ISWIZT
        ISWICB=>GWFSWIDAT(Igrid)%ISWICB
        ISWIOBS=>GWFSWIDAT(Igrid)%ISWIOBS

        NSWIOPT=>GWFSWIDAT(Igrid)%NSWIOPT

        NLAYSWI=>GWFSWIDAT(Igrid)%NLAYSWI
C         SOLVER
        NSOLVER=>GWFSWIDAT(Igrid)%NSOLVER
        IPRSOL=>GWFSWIDAT(Igrid)%IPRSOL
        MUTSOL=>GWFSWIDAT(Igrid)%MUTSOL
C         SWI PARAMETERS
        TOESLOPE=>GWFSWIDAT(Igrid)%TOESLOPE
        TIPSLOPE=>GWFSWIDAT(Igrid)%TIPSLOPE
        ALPHA=>GWFSWIDAT(Igrid)%ALPHA
        BETA=>GWFSWIDAT(Igrid)%BETA
        ICONV=>GWFSWIDAT(Igrid)%ICONV
        IBO=>GWFSWIDAT(Igrid)%IBO
        SWIHCOF=>GWFSWIDAT(Igrid)%SWIHCOF
        SWISOLCR=>GWFSWIDAT(Igrid)%SWISOLCR
        SWISOLCC=>GWFSWIDAT(Igrid)%SWISOLCC
        SWISOLCV=>GWFSWIDAT(Igrid)%SWISOLCV
        ZETA=>GWFSWIDAT(Igrid)%ZETA
        ZETAOLD=>GWFSWIDAT(Igrid)%ZETAOLD
        ZETASWITS0=>GWFSWIDAT(Igrid)%ZETASWITS0
        ZONECHG1=>GWFSWIDAT(Igrid)%ZONECHG1
        ZONECHG2=>GWFSWIDAT(Igrid)%ZONECHG2
        ZONEIMIX=>GWFSWIDAT(Igrid)%ZONEIMIX
        SSZ=>GWFSWIDAT(Igrid)%SSZ
        EPS=>GWFSWIDAT(Igrid)%EPS
        NUS=>GWFSWIDAT(Igrid)%NUS
        DELNUS=>GWFSWIDAT(Igrid)%DELNUS
        NUSRF=>GWFSWIDAT(Igrid)%NUSRF
        SWICR=>GWFSWIDAT(Igrid)%SWICR
        SWICC=>GWFSWIDAT(Igrid)%SWICC
        SWICUMCR=>GWFSWIDAT(Igrid)%SWICUMCR
        SWICUMCC=>GWFSWIDAT(Igrid)%SWICUMCC
        NUTOP=>GWFSWIDAT(Igrid)%NUTOP
        NUBOT=>GWFSWIDAT(Igrid)%NUBOT
        QLEXTRA=>GWFSWIDAT(Igrid)%QLEXTRA
        QREXTRA=>GWFSWIDAT(Igrid)%QREXTRA
        QFEXTRA=>GWFSWIDAT(Igrid)%QFEXTRA
        QLEXTRACUM=>GWFSWIDAT(Igrid)%QLEXTRACUM
        QREXTRACUM=>GWFSWIDAT(Igrid)%QREXTRACUM
        QFEXTRACUM=>GWFSWIDAT(Igrid)%QFEXTRACUM
        BRHS=>GWFSWIDAT(Igrid)%BRHS
        DUM=>GWFSWIDAT(Igrid)%DUM
        RHSPRESWI=>GWFSWIDAT(Igrid)%RHSPRESWI
        IPLPOS=>GWFSWIDAT(Igrid)%IPLPOS
        IZONENR=>GWFSWIDAT(Igrid)%IZONENR
        NBDITEMS=>GWFSWIDAT(Igrid)%NBDITEMS
        CUMBD=>GWFSWIDAT(Igrid)%CUMBD
        INCBD=>GWFSWIDAT(Igrid)%INCBD
        RRATIN=>GWFSWIDAT(Igrid)%RRATIN
        RRATOUT=>GWFSWIDAT(Igrid)%RRATOUT
        SC1=>GWFSWIDAT(Igrid)%SC1
        SC2=>GWFSWIDAT(Igrid)%SC2
        SWIOBS=>GWFSWIDAT(Igrid)%SWIOBS
        SWIDE4=>GWFSWIDAT(Igrid)%SWIDE4
        SWIPCG=>GWFSWIDAT(Igrid)%SWIPCG
C
C---------RETURN
        RETURN
      END SUBROUTINE SGWF2SWI2PNT
C
C
      SUBROUTINE SGWF2SWI2PSV(Igrid)
C
C     ******************************************************************
C     SAVE POINTERS TO SWI2 PACKAGE DATA FOR A GRID
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
        USE GWFSWIMODULE
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Igrid
C       + + + CODE + + +
        GWFSWIDAT(Igrid)%NSRF=>NSRF
        GWFSWIDAT(Igrid)%ISTRAT=>ISTRAT
        GWFSWIDAT(Igrid)%NZONES=>NZONES

        GWFSWIDAT(Igrid)%IFIXED=>IFIXED
        
        GWFSWIDAT(Igrid)%NADPTFLG=>NADPTFLG
        GWFSWIDAT(Igrid)%NADPTMX=>NADPTMX
        GWFSWIDAT(Igrid)%NADPTMN=>NADPTMN
        GWFSWIDAT(Igrid)%ADPTFCT=>ADPTFCT
        GWFSWIDAT(Igrid)%IADPT=>IADPT
        GWFSWIDAT(Igrid)%IADPTMOD=>IADPTMOD
        GWFSWIDAT(Igrid)%ADPTVAL=>ADPTVAL
        GWFSWIDAT(Igrid)%SWIDELT=>SWIDELT
        GWFSWIDAT(Igrid)%NADPTSUM=>NADPTSUM
        GWFSWIDAT(Igrid)%RADPTSUM=>RADPTSUM
        GWFSWIDAT(Igrid)%IADPTSUM=>IADPTSUM

        GWFSWIDAT(Igrid)%NOBS=>NOBS
        GWFSWIDAT(Igrid)%IOBSHEADER=>IOBSHEADER

        GWFSWIDAT(Igrid)%ISWIZT=>ISWIZT
        GWFSWIDAT(Igrid)%ISWICB=>ISWICB
        GWFSWIDAT(Igrid)%ISWIOBS=>ISWIOBS

        GWFSWIDAT(Igrid)%NSWIOPT=>NSWIOPT

        GWFSWIDAT(Igrid)%NLAYSWI=>NLAYSWI
C         SOLVER
        GWFSWIDAT(Igrid)%NSOLVER=>NSOLVER
        GWFSWIDAT(Igrid)%IPRSOL=>IPRSOL
        GWFSWIDAT(Igrid)%MUTSOL=>MUTSOL
C         SWI PARAMETERS
        GWFSWIDAT(Igrid)%TOESLOPE=>TOESLOPE
        GWFSWIDAT(Igrid)%TIPSLOPE=>TIPSLOPE
        GWFSWIDAT(Igrid)%ALPHA=>ALPHA
        GWFSWIDAT(Igrid)%BETA=>BETA
        GWFSWIDAT(Igrid)%ICONV=>ICONV
        GWFSWIDAT(Igrid)%IBO=>IBO
        GWFSWIDAT(Igrid)%SWIHCOF=>SWIHCOF
        GWFSWIDAT(Igrid)%SWISOLCR=>SWISOLCR
        GWFSWIDAT(Igrid)%SWISOLCC=>SWISOLCC
        GWFSWIDAT(Igrid)%SWISOLCV=>SWISOLCV
        GWFSWIDAT(Igrid)%ZETA=>ZETA
        GWFSWIDAT(Igrid)%ZETAOLD=>ZETAOLD
        GWFSWIDAT(Igrid)%ZETASWITS0=>ZETASWITS0
        GWFSWIDAT(Igrid)%ZONECHG1=>ZONECHG1
        GWFSWIDAT(Igrid)%ZONECHG2=>ZONECHG2
        GWFSWIDAT(Igrid)%ZONEIMIX=>ZONEIMIX
        GWFSWIDAT(Igrid)%SSZ=>SSZ
        GWFSWIDAT(Igrid)%EPS=>EPS
        GWFSWIDAT(Igrid)%NUS=>NUS
        GWFSWIDAT(Igrid)%DELNUS=>DELNUS
        GWFSWIDAT(Igrid)%NUSRF=>NUSRF
        GWFSWIDAT(Igrid)%SWICR=>SWICR
        GWFSWIDAT(Igrid)%SWICC=>SWICC
        GWFSWIDAT(Igrid)%SWICUMCR=>SWICUMCR
        GWFSWIDAT(Igrid)%SWICUMCC=>SWICUMCC
        GWFSWIDAT(Igrid)%NUTOP=>NUTOP
        GWFSWIDAT(Igrid)%NUBOT=>NUBOT
        GWFSWIDAT(Igrid)%QLEXTRA=>QLEXTRA
        GWFSWIDAT(Igrid)%QREXTRA=>QREXTRA
        GWFSWIDAT(Igrid)%QFEXTRA=>QFEXTRA
        GWFSWIDAT(Igrid)%QLEXTRACUM=>QLEXTRACUM
        GWFSWIDAT(Igrid)%QREXTRACUM=>QREXTRACUM
        GWFSWIDAT(Igrid)%QFEXTRACUM=>QFEXTRACUM
        GWFSWIDAT(Igrid)%BRHS=>BRHS
        GWFSWIDAT(Igrid)%DUM=>DUM
        GWFSWIDAT(Igrid)%RHSPRESWI=>RHSPRESWI
        GWFSWIDAT(Igrid)%IPLPOS=>IPLPOS
        GWFSWIDAT(Igrid)%IZONENR=>IZONENR
        GWFSWIDAT(Igrid)%NBDITEMS=>NBDITEMS
        GWFSWIDAT(Igrid)%CUMBD=>CUMBD
        GWFSWIDAT(Igrid)%INCBD=>INCBD
        GWFSWIDAT(Igrid)%RRATIN=>RRATIN
        GWFSWIDAT(Igrid)%RRATOUT=>RRATOUT
        GWFSWIDAT(Igrid)%SC1=>SC1
        GWFSWIDAT(Igrid)%SC2=>SC2
        GWFSWIDAT(Igrid)%SWIOBS=>SWIOBS
        GWFSWIDAT(Igrid)%SWIDE4=>SWIDE4
        GWFSWIDAT(Igrid)%SWIPCG=>SWIPCG
C
C---------RETURN
        RETURN
      END SUBROUTINE SGWF2SWI2PSV

      SUBROUTINE SSWI2_BDCH(ITOTAL)
C     ******************************************************************
C     COMPUTE SWI2 FLOW CORRECTION FOR CONSTANT-HEAD CELLS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IBOUND,HNEW,BUFF,CR,CC,CV,
     1                      BOTM,LBOTM,IOUT
      USE GWFBASMODULE,ONLY:MSUM,VBVL,VBNM,DELT,PERTIM,TOTIM,ICBCFL,
     1                      ICHFLG
C      USE GWFBCFMODULE,ONLY:LAYCON
      USE GWFSWIMODULE
      IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
      INTEGER, INTENT(IN) :: ITOTAL
C     + + + LOCAL DEFINITIONS + + +
      INTEGER :: i, j, k
      INTEGER :: kk
      REAL            :: zero
      DOUBLEPRECISION :: dzero
      REAL            :: rtotal
      REAL            :: hdiff
      REAL            :: chch1, chch2, chch3, chch4, chch5, chch6
      REAL            :: rate
      DOUBLEPRECISION :: hd
      DOUBLEPRECISION :: b1,h1
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C     ------------------------------------------------------------------
C
C-------INITIALIZE VARIABLES
      zero   = 0.0
      dzero  = 0.0D0
      rtotal = REAL( ITOTAL, 4 )
C
C-------CLEAR BUFFER.
      DO k = 1, NLAY
        DO i = 1, NROW
          DO j = 1, NCOL
            BUFF(j,i,k) = zero
          END DO
        END DO
      END DO
C
C
C-------LOOP THROUGH EACH CELL AND CALCULATE FLOW INTO MODEL FROM EACH
C-------CONSTANT-HEAD CELL.
      KCH: DO k = 1, NLAY
        ICH: DO i = 1, NROW
          JCH: DO j = 1,NCOL
C
C-------------IF CELL IS NOT CONSTANT HEAD SKIP IT & GO ON TO NEXT CELL.
            IF (IBOUND(J,I,K).GE.0) CYCLE JCH
C
C-------------CLEAR VALUES FOR FLOW RATE THROUGH EACH FACE OF CELL.
            chch1 = zero
            chch2 = zero
            chch3 = zero
            chch4 = zero
            chch5 = zero
            chch6 = zero
C
C-------------CALCULATE FLOW THROUGH THE LEFT FACE.
C-------------IF THERE IS NO FLOW TO CALCULATE THROUGH THIS FACE, THEN GO ON
C-------------TO NEXT FACE.  NO FLOW OCCURS AT THE EDGE OF THE GRID, TO AN
C-------------ADJACENT NO-FLOW CELL, OR TO AN ADJACENT CONSTANT-HEAD CELL.
            IF ( j.EQ.1 ) GO TO 30
            IF ( IBOUND(j-1,i,k).EQ.0 ) GO TO 30
            IF ( IBOUND(j-1,i,k).LT.0 .AND. ICHFLG.EQ.0 ) GO TO 30
C-------------DO NOT CALCULATE FLOW THROUGH THE LEFT FACE IF HEAD IN CELL J-1 
C             IS LESS THAN THE CELL BOTTOM IN CELL J-1 - FOR MODFLOW-NWT
            kk = LBOTM(k)
            b1 = BOTM(J-1,I,kk)
            h1 = HNEW(J-1,I,k)
            IF ( h1.LT.b1 ) GOTO 30
C
C-------------CALCULATE FLOW THROUGH THE LEFT FACE INTO THE ADJACENT CELL.
            hdiff = REAL( ( HNEW(j,i,k) - HNEW(j-1,i,k) ), 4 ) * rtotal
            CHCH1 = ( hdiff * CR(j-1,i,k) ) - QREXTRACUM(J-1,I,K)
C
C-------------CALCULATE FLOW THROUGH THE RIGHT FACE.
C-------------IF THERE IS NO FLOW TO CALCULATE THROUGH THIS FACE, THEN GO ON
C-------------TO NEXT FACE.  NO FLOW OCCURS AT THE EDGE OF THE GRID, TO AN
C-------------ADJACENT NO-FLOW CELL, OR TO AN ADJACENT CONSTANT-HEAD CELL.
   30       IF ( j.EQ.NCOL ) GO TO 60
            IF ( IBOUND(j+1,i,k).EQ.0 ) GO TO 60
            IF ( IBOUND(j+1,i,k).LT.0 .AND. ICHFLG.EQ.0 ) GO TO 60
C-------------DO NOT CALCULATE FLOW THROUGH THE RIGHT FACE IF HEAD IN CELL J+1 
C             IS LESS THAN THE CELL BOTTOM IN CELL J+1 - FOR MODFLOW-NWT
            kk = LBOTM(k)
            b1 = BOTM(J+1,I,kk)
            h1 = HNEW(J+1,I,k)
            IF ( h1.LT.b1 ) GOTO 60
C
C-------------CALCULATE FLOW THROUGH THE RIGHT FACE INTO THE ADJACENT CELL.
            hdiff = REAL( ( HNEW(j,i,k) - HNEW(j+1,i,k) ), 4 ) * rtotal
            chch2 = ( hdiff * CR(j,i,k) ) + QREXTRACUM(j,i,k)
C
C-------------CALCULATE FLOW THROUGH THE BACK FACE.
C-------------IF THERE IS NO FLOW TO CALCULATE THROUGH THIS FACE, THEN GO ON
C-------------TO NEXT FACE.  NO FLOW OCCURS AT THE EDGE OF THE GRID, TO AN
C-------------ADJACENT NO-FLOW CELL, OR TO AN ADJACENT CONSTANT-HEAD CELL.
   60       IF ( i.EQ.1 ) GO TO 90
            IF ( IBOUND(j,i-1,k).EQ.0 ) GO TO 90
            IF ( IBOUND(j,i-1,k).LT.0 .AND. ICHFLG.EQ.0 ) GO TO 90
C-------------DO NOT CALCULATE FLOW THROUGH THE BACK FACE IF HEAD IN CELL I-1 
C             IS LESS THAN THE CELL BOTTOM IN CELL I-1 - FOR MODFLOW-NWT
            kk = LBOTM(k)
            b1 = BOTM(J,I-1,kk)
            h1 = HNEW(J,I-1,k)
            IF ( h1.LT.b1 ) GOTO 90
C
C-------------CALCULATE FLOW THROUGH THE BACK FACE INTO THE ADJACENT CELL.
            hdiff = REAL( ( HNEW(j,i,k) - HNEW(j,i-1,k) ), 4 ) * rtotal
            chch3 = ( hdiff * CC(j,i-1,k) ) - QFEXTRACUM(j,i-1,k)
C
C-------------CALCULATE FLOW THROUGH THE FRONT FACE.
C-------------IF THERE IS NO FLOW TO CALCULATE THROUGH THIS FACE, THEN GO ON
C-------------TO NEXT FACE.  NO FLOW OCCURS AT THE EDGE OF THE GRID, TO AN
C-------------ADJACENT NO-FLOW CELL, OR TO AN ADJACENT CONSTANT-HEAD CELL.
   90       IF ( i.EQ.NROW ) GO TO 120
            IF ( IBOUND(j,i+1,k).EQ.0 ) GO TO 120
            IF ( IBOUND(j,i+1,k).LT.0 .AND. ICHFLG.EQ.0 ) GO TO 120
C-------------DO NOT CALCULATE FLOW THROUGH THE FRONT FACE IF HEAD IN CELL I+1 
C             IS LESS THAN THE CELL BOTTOM IN CELL I+1 - FOR MODFLOW-NWT
            kk = LBOTM(k)
            b1 = BOTM(J,I+1,kk)
            h1 = HNEW(J,I+1,k)
            IF ( h1.LT.b1 ) GOTO 120
C
C-------------CALCULATE FLOW THROUGH THE FRONT FACE INTO THE ADJACENT CELL.
            hdiff = REAL( ( HNEW(j,i,k) - HNEW(j,i+1,k) ), 4 ) * rtotal
            chch4 = ( hdiff * CC(j,i,k) ) + QFEXTRACUM(j,i,k)
C
C-------------CALCULATE FLOW THROUGH THE UPPER FACE.
C-------------IF THERE IS NO FLOW TO CALCULATE THROUGH THIS FACE, THEN GO ON
C-------------TO NEXT FACE.  NO FLOW OCCURS AT THE EDGE OF THE GRID, TO AN
C-------------ADJACENT NO-FLOW CELL, OR TO AN ADJACENT CONSTANT-HEAD CELL.
  120       IF ( k.EQ.1 ) GO TO 150
            IF ( IBOUND(j,i,k-1).EQ.0 ) GO TO 150
            IF ( IBOUND(j,i,k-1).LT.0 .AND. ICHFLG.EQ.0 ) GO TO 150
C-------------DO NOT CALCULATE FLOW THROUGH THE UPPER FACE IF HEAD IN CELL K-1 
C             IS LESS THAN THE CELL BOTTOM IN CELL K-1 - FOR MODFLOW-NWT
            kk = LBOTM(k-1)
            b1 = BOTM(J,I,kk)
            h1 = HNEW(J,I,k-1)
            IF ( h1.LT.b1 ) GOTO 150
C
C-------------CALCULATE FLOW THROUGH THE UPPER FACE INTO THE ADJACENT CELL.
            hd = HNEW(j,i,k)
            IF( hd.LT.BOTM(j,i,LBOTM(k)-1) ) hd = BOTM(j,i,LBOTM(k)-1)
  122       hdiff = REAL( ( hd - HNEW(j,i,k-1) ), 4 ) * rtotal
            chch5 = ( hdiff * CV(j,i,k-1) ) - QLEXTRACUM(j,i,k-1)
C
C-------------CALCULATE FLOW THROUGH THE LOWER FACE.
C-------------IF THERE IS NO FLOW TO CALCULATE THROUGH THIS FACE, THEN GO ON
C-------------TO NEXT FACE.  NO FLOW OCCURS AT THE EDGE OF THE GRID, TO AN
C-------------ADJACENT NO-FLOW CELL, OR TO AN ADJACENT CONSTANT-HEAD CELL.
  150       IF ( k.EQ.NLAY ) GO TO 180
            IF ( IBOUND(j,i,k+1).EQ.0 ) GO TO 180
            IF ( IBOUND(j,i,k+1).LT.0 .AND. ICHFLG.EQ.0 ) GO TO 180
C-------------DO NOT CALCULATE FLOW THROUGH THE LOWER FACE IF HEAD IN CELL K+1 
C             IS LESS THAN THE CELL BOTTOM IN CELL K+1 - FOR MODFLOW-NWT
            kk = LBOTM(k+1)
            b1 = BOTM(J,I,kk)
            h1 = HNEW(J,I,k+1)
            IF ( h1.LT.b1 ) GOTO 180
C
C-------------CALCULATE FLOW THROUGH THE LOWER FACE INTO THE ADJACENT CELL.
            hd = HNEW(j,i,k+1)
            IF( hd.LT.BOTM(j,i,LBOTM(k+1)-1) ) hd=BOTM(j,i,LBOTM(k+1)-1)
  152       hdiff = REAL( ( HNEW(j,i,k) - hd ), 4 ) * rtotal
            chch6 = ( hdiff * CV(j,i,k) ) + QLEXTRACUM(j,i,k)
C
C-------------SUM THE FLOWS THROUGH SIX FACES OF CONSTANT HEAD CELL, AND
C-------------STORE SUM IN BUFFER.
 180        rate = chch1 + chch2 + chch3 + chch4 + chch5 + chch6
            BUFF(j,i,k) = rate
C
          END DO JCH
        END DO ICH
      END DO KCH
C
C-------RETURN.
      RETURN
      END SUBROUTINE SSWI2_BDCH
C
C
      SUBROUTINE SSWI2_UPDZ(Kkstp,Kkper)
C
C     ******************************************************************
C     UPDATE ACTIVE ZETA SURFACES FOR SWI2 PACKAGE
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
        USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IFREFM,
     2                        LBOTM,BOTM,
     3                        CR,CC,CV,HCOF,RHS,
     4                        DELR,DELC,IBOUND,HNEW,HOLD,
     5                        BUFF,ISSFLG,NSTP
        USE GWFBASMODULE, ONLY: DELT,HDRY,TOTIM
        USE GWFSWIMODULE
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Kkstp
        INTEGER, INTENT(IN) :: Kkper
C       + + + LOCAL DEFINITIONS + + +
        INTEGER :: i, j, k
        INTEGER :: kk
        INTEGER :: iz, iz2
        INTEGER :: iusezone, iactive
        INTEGER :: ksoliter, kksoliter
        INTEGER :: ierr, icnvg
        REAL :: q, qztop, qzbot
        REAL :: nuontop, nubelbot
        REAL :: rclose
        DOUBLEPRECISION :: dt, ht
        real :: thick, thickb, fact
C
C-------OUTPUT FORMAT STATEMENTS
 2000  FORMAT(1X,/1X,A,'   PERIOD ',I4,'   STEP ',I3)
 2010  FORMAT(1X,'   LAYER ',I3,'   ROW ',I5,'   COL ',
     1        I5,'   RATE ',1PG15.6)
 2020  FORMAT(1X,'SOLVING FOR ZETA FOR LAYER',1X,I5,1X,'ZONE',1X,I5)
 2030  FORMAT('      ZETASRF ',I2)
C
C---------UPDATE THE UPPER ZETA SURFACE TO THE TOP OF THE WATER
C         TABLE FOR UNCONFINED CONDITIONS PRIOR TO SOLVING FOR 
C         ZETA SURFACES       
        DO k = 1, NLAY
          IF ( ICONV(k).EQ.0 ) CYCLE
          CALL SSWI2_UPZ1(k,0)
        END DO
C
C---------CALCULATE BRHS
        KBRHS: DO k=1,NLAY
          IBRHS: DO i=1,NROW
            JBRHS: DO j=1,NCOL
C---------------CALCULATE BOUNDARY FLUX
              q = 0.0
C               ACTIVE CELL
              IF ( IBOUND(J,I,K).GT.0 ) THEN
C                 DETERMINE IF THE HEAD IN CELL IS LESS THAN THE 
C                 BOTTOM OF THE CELL - FOR MODFLOW-NWT
                kk = LBOTM(k)
                dt = BOTM(J,I,kk)
                ht = HNEW(J,I,K)
                IF ( ht.LT.dt ) THEN
                  q = 0.0
                ELSE
                  q = RHSPRESWI(j,i,k) -
     2                HNEW(j,i,k) * HCOF(j,i,k)
                END IF
C               CONSTANT HEAD
              ELSE IF ( IBOUND(J,I,K).LT.0 ) THEN
                q = -BUFF(j,i,k)
              END IF
C               SET ZONE NUMBER FOR BOUNDARY CONDITIONS
              iusezone = IZONENR(j,i,k)
              IF ( (IZONENR(j,i,k).LT.0) .AND. (q.GT.0) ) THEN
                iusezone = 1
              ENDIF
              thick = 0.0
              IF ( (IZONENR(j,i,k) > 100) ) then
                if (q.GT.0) THEN 
                  thick = ZETA(j,i,k,1) - ZETA(j,i,k,NZONES+1)
                end if
                iusezone = iusezone - 100
              end if
              IZBRHS: DO iz=1,NZONES
C-----------------INITIALIZE BRHS
                BRHS(j,i,iz)=0.
                IF (IPLPOS(j,i,k,iz).EQ.0) THEN
                  IF ((iz.LE.ABS(iusezone)).AND.(q.NE.0)) THEN
                    fact = 1.
                    if (thick > 0.) then
                      thickb = ZETA(j,i,k,iz) - ZETA(j,i,k,NZONES+1)
                      fact = thickb / thick
                    end if
                    BRHS(j,i,iz)=BRHS(j,i,iz) + q * fact
                  ENDIF
                ELSE
                  BRHS(j,i,iz) = 0.0
                ENDIF
C
                IF (iz.NE.1) THEN
                  IF (IPLPOS(j,i,k,iz).EQ.0) THEN
                    BRHS(j,i,iz)= BRHS(j,i,iz) -
     &                SSZ(j,i,k)*DELR(j)*DELC(i)*ZETA(j,i,k,iz)/SWIDELT
                    CALL SSWI2_SD(j,i,NCOL,NROW,
     &                IPLPOS(1:NCOL,1:NROW,k,iz),1.,
     &                BRHS(1:NCOL,1:NROW,iz),
     &                SWICUMCR(1:NCOL,1:NROW,k,iz),
     &                SWICUMCC(1:NCOL,1:NROW,k,iz),
     &                HNEW(1:NCOL,1:NROW,k))
                    DO iz2 = 1, iz-1
                      CALL SSWI2_SR(j,i,NCOL,NROW,
     &                  IPLPOS(1:NCOL,1:NROW,k,iz),
     &                  DELNUS(iz2),BRHS(1:NCOL,1:NROW,iz),
     &                  SWICUMCR(1:NCOL,1:NROW,k,iz),
     &                  SWICUMCC(1:NCOL,1:NROW,k,iz),
     &                  ZETA(1:NCOL,1:NROW,k,iz2))
                    END DO
                  END IF
                  CALL SSWI2_SR(j,i,NCOL,NROW,
     &              IPLPOS(1:NCOL,1:NROW,k,iz),
     &              EPS(iz),BRHS(1:NCOL,1:NROW,iz),
     &              SWICR(1:NCOL,1:NROW,k,iz),
     &              SWICC(1:NCOL,1:NROW,k,iz),
     &              ZETA(1:NCOL,1:NROW,k,iz+1))
C
C-----------Zones iz+1 through bottom
                  DO iz2 = iz+1, NZONES
                    IF (IPLPOS(j,i,k,iz).EQ.0) THEN
                      CALL SSWI2_SR(j,i,NCOL,NROW,
     &                  IPLPOS(1:NCOL,1:NROW,k,iz),
     &                  DELNUS(iz2),
     &                  BRHS(1:NCOL,1:NROW,iz),
     &                  SWICUMCR(1:NCOL,1:NROW,k,iz2),
     &                  SWICUMCC(1:NCOL,1:NROW,k,iz2),
     &                  ZETA(1:NCOL,1:NROW,k,iz2))
                    END IF
C---------------------LEFT FACE
                    IF (j.NE.1) THEN
                      BRHS(j,i,iz) = BRHS(j,i,iz)+EPS(iz2)*
     &                  SWICR(j-1,i,k,iz2)*
     &                  ((ZETA(j-1,i,k,iz2)-ZETA(j,i,k,iz2))-
     &                  (ZETA(j-1,i,k,iz2+1)-ZETA(j,i,k,iz2+1)))
                    END IF
C---------------------RIGHT FACE
                    IF (j.NE.NCOL) THEN
                      BRHS(j,i,iz) = BRHS(j,i,iz)+EPS(iz2)*
     &                  SWICR(j,i,k,iz2)*
     &                 ((ZETA(j+1,i,k,iz2)-ZETA(j,i,k,iz2))-
     &                 (ZETA(j+1,i,k,iz2+1)-ZETA(j,i,k,iz2+1)))
                    END IF
C---------------------BACK FACE
                    IF (i.NE.1) THEN
                      BRHS(j,i,iz) = BRHS(j,i,iz)+EPS(iz2)*
     &                  SWICC(j,i-1,k,iz2)*
     &                  ((ZETA(j,i-1,k,iz2)-ZETA(j,i,k,iz2))-
     &                  (ZETA(j,i-1,k,iz2+1)-ZETA(j,i,k,iz2+1)))
                    END IF
C---------------------FRONT FACE
                    IF (i.NE.NROW) THEN
                      BRHS(j,i,iz) = BRHS(j,i,iz)+EPS(iz2)*
     &                  SWICC(j,i,k,iz2)*
     &                  ((ZETA(j,i+1,k,iz2)-ZETA(j,i,k,iz2))-
     &                  (ZETA(j,i+1,k,iz2+1)-ZETA(j,i,k,iz2+1)))
                    END IF
                  END DO
C
C-------------------SPECIFY BOUNDARY CONDITION OF FLOW AT TIPS AND TOES
                  IF (IPLPOS(j,i,k,iz).EQ.0) THEN
C---------------------LEFT FACE
                    IF (j.NE.1) THEN
                      IF (IPLPOS(j-1,i,k,iz).NE.0 .AND.
     &                    IPLPOS(j-1,i,k,iz).NE.3) THEN
                        CALL SSWI2_QR(j,i,k,iz,NCOL,NROW,NLAY,NZONES,
     &                         q,HNEW,ZETA,DELNUS,EPS,
     &                         SWICUMCR,SWICR)
                        BRHS(j,i,iz) = BRHS(j,i,iz) - q
                      END IF
                    END IF
C---------------------RIGHT FACE
                    IF (j.NE.NCOL) THEN
                      IF (IPLPOS(j+1,i,k,iz).NE.0 .AND.
     &                    IPLPOS(j+1,i,k,iz).NE.3) THEN
                        CALL SSWI2_QR(j+1,i,k,iz,NCOL,NROW,NLAY,NZONES,
     &                         q,HNEW,ZETA,DELNUS,EPS,
     &                         SWICUMCR,SWICR)
                        BRHS(j,i,iz) = BRHS(j,i,iz) + q
                      END IF
                    END IF
C---------------------BACK FACE
                    IF (i.NE.1) THEN
                      IF (IPLPOS(j,i-1,k,iz).NE.0 .AND.
     &                    IPLPOS(j,i-1,k,iz).NE.3) THEN
                        CALL SSWI2_QC(j,i,k,iz,NCOL,NROW,NLAY,NZONES,
     &                         q,HNEW,ZETA,DELNUS,EPS,
     &                         SWICUMCC,SWICC)
                        BRHS(j,i,iz) = BRHS(j,i,iz) - q
                      END IF
                    END IF
C---------------------FRONT FACE
                    IF (i.NE.NROW) THEN
                      IF (IPLPOS(j,i+1,k,iz).NE.0 .AND.
     &                    IPLPOS(j,i+1,k,iz).NE.3) THEN
                        CALL SSWI2_QC(j,i+1,k,iz,NCOL,NROW,NLAY,NZONES,
     &                         q,HNEW,ZETA,DELNUS,EPS,
     &                         SWICUMCC,SWICC)
                        BRHS(j,i,iz) = BRHS(j,i,iz) + q
                      END IF
                    END IF
C---------------------UPPER FACE
                    IF (k.NE.1) THEN
                      IF ( IBOUND(j,i,k-1).NE.0 ) THEN
                        qztop = CV(j,i,k-1)*(HNEW(j,i,k)-HNEW(j,i,k-1))-
     &                          QLEXTRA(j,i,k-1)
                        nuontop = NUBOT(j,i,k-1)
                        IF ((qztop.LT.0).AND.(nuontop.GE.NUS(iz)).AND.
     &                      (NUBOT(j,i,k).GE.nuontop)) THEN
                          BRHS(j,i,iz) = BRHS(j,i,iz) + qztop
                        ENDIF
                      END IF
                    END IF
C---------------------LOWER FACE
                    IF (k.NE.NLAY) THEN
                      IF ( IBOUND(j,i,k+1).NE.0 ) THEN
                        qzbot = CV(j,i,k)*(HNEW(j,i,k)-HNEW(j,i,k+1))+
     &                          QLEXTRA(j,i,k)
                        nubelbot = NUTOP(j,i,k+1)
                        IF ((qzbot.LT.0).AND.(nubelbot.LT.NUS(iz)).AND.
     &                      (NUTOP(j,i,k).LE.nubelbot)) THEN
                          BRHS(j,i,iz) = BRHS(j,i,iz) + 0
                        ELSE
                          BRHS(j,i,iz) = BRHS(j,i,iz) + qzbot
                        END IF
                      END IF
                    END IF
                  END IF
                END IF
              END DO IZBRHS
            END DO JBRHS
          END DO IBRHS
C
C-----------SOLVE EQUATIONS FOR 1 LAYER
          IZSOLVZ4LAY: DO iz = 2, NZONES
            iactive = 0
C             CALCULATE SWI CONDUCTANCES FOR CURRENT ZETA SURFACE
C             IN CURRENT LAYER
            ISOLVZ4LAY: DO i=1,NROW
              JSOLVZ4LAY: DO j=1,NCOL
C
C                 CALCULATE SWISOLCR
                IF (j.LT.NCOL) THEN
                  IF ((IPLPOS(j,i,k,iz).EQ.0).AND.
     &                (IPLPOS(j+1,i,k,iz).EQ.0)) THEN
                    SWISOLCR(j,i) = DELNUS(iz)*SWICUMCR(j,i,k,iz)
                  ELSE
                    SWISOLCR(j,i) = 0.
                  END IF
                END IF
                SWISOLCR(j,i) = SWISOLCR(j,i)-EPS(iz)*SWICR(j,i,k,iz)
C
C                 CALCULATE SWISOLCC
                IF (i.LT.NROW) THEN
                  IF ((IPLPOS(j,i,k,iz).EQ.0).AND.
     &                (IPLPOS(j,i+1,k,iz).EQ.0)) THEN
                    SWISOLCC(j,i) = DELNUS(iz)*SWICUMCC(j,i,k,iz)
                  ELSE
                    SWISOLCC(j,i)=0.
                  END IF
                END IF
                SWISOLCC(j,i) = SWISOLCC(j,i)-EPS(iz)*SWICC(j,i,k,iz)
C
C                 CALCULATE SWISOLCV AND SWIHCOF (STORAGE)
                SWISOLCV(j,i) = 0.
                SWIHCOF(j,i)   = -SSZ(j,i,k)*DELR(j)*DELC(i)/SWIDELT

                DUM(j,i)       = ZETA(j,i,k,iz)
C
C                 DETERMINE IF CURRENT CELL HAS ZETA VALUE GREATER THAN
C                 THE BOTTOM AND LESS THAN THE TOP (IPLPOS=0)
                IF (IPLPOS(j,i,k,iz).EQ.0 .AND. 
     2              IBOUND(j,i,k).NE.0 ) THEN
                  IBO(j,i) = 1
                  iactive = iactive + 1
                ELSE
                  IBO(j,i) = 0
                END IF
              END DO JSOLVZ4LAY
            END DO ISOLVZ4LAY
C
C-------------SOLVE FOR ZETA SURFACE
            IF ( iactive.GT.0 .AND. MUTSOL.NE.2 ) THEN
              WRITE (IOUT,2020) k, iz
            END IF
C             DIRECT SOLVER
            SOLVEZETA: IF (NSOLVER.EQ.1) THEN
C-------------INITIALIZE PARAMETERS FOR DIRECT SOLVING
              kksoliter = 1
              ierr=0
C---------------CALL DE45AP TO SOLVE FOR ZETA
              ACTIVEDE4: IF ( iactive.GT.0 ) THEN
                CALL DE47AP(DUM,IBO,SWIDE4%AU,SWIDE4%AL,SWIDE4%IUPPNT,
     2            SWIDE4%IEQPNT,SWIDE4%D4B,SWIDE4%MXUP,SWIDE4%MXLOW,
     3            SWIDE4%MXEQ,SWIDE4%MXBW,
     4            SWISOLCR,SWISOLCC,SWISOLCV,
     5            SWIHCOF,BRHS(1:NCOL,1:NROW,iz),SWIDE4%ACCLDE4,
     6            kksoliter,SWIDE4%ITMX,SWIDE4%MXITER,SWIDE4%NITERDE4,
     7            SWIDE4%HCLOSEDE4,IPRSOL,icnvg,NCOL,NROW,NLAYSWI,
     8            IOUT,SWIDE4%LRCHDE4,SWIDE4%HDCGDE4,SWIDE4%IFREQ,
     9            Kkstp,Kkper,SWIDE4%DELTL,NSTP(Kkper),SWIDE4%ID4DIR,
     Z            SWIDE4%ID4DIM,MUTSOL,SWIDE4%DELTL,SWIDE4%NBWL,
     1            SWIDE4%NUPL,SWIDE4%NLOWL,SWIDE4%NLOW,SWIDE4%NEQ,
     2            SWIDE4%NUP,SWIDE4%NBW,ierr)

                IF (ierr.EQ.0) THEN
                  WRITE(IOUT,'(A,I3,A,I3,A,I3,A,I3,A,I5,A,I8)')
     &             ' STRESS PERIOD=',Kkper,
     &             ' TIMESTEP=',kkstp,
     &             ' LAYER=',k,
     &             ' ZONE=',iz,
     &             ' ACTIVE CELLS=',iactive,
     &             ' NUMBER OF EQUATIONS=',SWIDE4%NEQ  !ierr
                END IF
              END IF ACTIVEDE4
C             PRECONDITIONED CONJUGATE GRADIENT
            ELSE IF (NSOLVER.EQ.2) THEN
              kksoliter=1
              ierr = 0
C---------------CALL PCG7AP TO SOLVE FOR ZETA
              ACTIVEPCG: IF ( iactive.GT.0 ) THEN
C                 LINEAR EQUATION -- OUTER ITERATIONS ONLY NEEDED IF
C                 HCLOSE AND/OR RCLOSE NOT ACHIEVED AFTER FIRST
C                 INNER ITERATION
                rclose = ( 1.0 + DELNUS(iz) ) * SWIPCG%RCLOSEPCG
                OUTERPCG: DO ksoliter = 1, SWIPCG%MXITER
                  kksoliter = ksoliter
                  CALL PCG7AP(DUM,IBO,SWISOLCR,SWISOLCC,SWISOLCV,
     2              SWIHCOF,BRHS(1:NCOL,1:NROW,iz),SWIPCG%VPCG,
     3              SWIPCG%SS,SWIPCG%P,SWIPCG%CD,SWIPCG%HCHG,
     4              SWIPCG%LHCH,SWIPCG%RCHG,SWIPCG%LRCHPCG,
     5              kksoliter,SWIPCG%NITER,
     6              SWIPCG%ZCLOSEPCG,rclose,icnvg,Kkstp,Kkper,IPRSOL,
     7              SWIPCG%MXITER,SWIPCG%ITER1,SWIPCG%NPCOND,
     8              SWIPCG%NBPOL,NSTP(Kkper),NCOL,NROW,NLAYSWI,
     9              SWIPCG%NODES,SWIPCG%RELAXPCG,IOUT,MUTSOL,
     X              SWIPCG%IT1,SWIPCG%DAMPPCG,BUFF,SWIPCG%HCSV,ierr,
     1              SWIPCG%HPCG,SWIPCG%DAMPPCGT,ISSFLG(Kkper),HDRY,
     2              SWIPCG%IHCOFADD)
                  IF ( icnvg.EQ.1 ) EXIT OUTERPCG
                END DO OUTERPCG
	        END IF ACTIVEPCG
            END IF SOLVEZETA
C
C-------------COPY REAL ARRAY DUM INTO DOUBLE ARRAY ZETA
            IF ( iactive.GT.0 ) THEN
              DO i=1,NROW
                DO j=1,NCOL
                  IF ( IBO(j,i).LT.1 ) CYCLE
                  ZETA(j,i,k,iz) = DUM(j,i)
                END DO
              END DO
            END IF
          END DO IZSOLVZ4LAY
      END DO KBRHS
C
C---------CHECK SLOPE AT TIPS AND TOES OF ZETA SURFACES
        IF ( NADPTFLG.NE.0 ) CALL SSWI2_CHKSLOPE()
C
C---------RETURN
        RETURN
      END SUBROUTINE SSWI2_UPDZ
C
C
      SUBROUTINE SSWI2_CHKSLOPE()
C
C     ******************************************************************
C     CHECK TIP AND TOE SLOPE FOR SWI2 PACKAGE
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
        USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IFREFM,
     2                        LBOTM,BOTM,
     3                        CR,CC,CV,HCOF,RHS,
     4                        DELR,DELC,IBOUND,HNEW,HOLD,
     5                        BUFF,ISSFLG,NSTP
        USE GWFBASMODULE, ONLY: DELT,HDRY,TOTIM,IHDDFL,IBUDFL
        USE GWFSWIMODULE
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
C       + + + LOCAL DEFINITIONS + + +
        INTEGER :: i, j, k
        INTEGER :: iz
        INTEGER :: kk
        REAL :: top, bot, h, aqb, zoldc
        REAL :: zt, zb, zetac, zcalc
        REAL :: rvl, dist, b
        REAL :: tv
          IADPTMOD = 0
          rvl      = 1.0 / ADPTFCT
          ADPTVAL  = 1.0
          ZTIPTOE: DO k = 1, NLAY
            kk    = LBOTM(k)
            IZTIPTOE: DO iz = 2, NZONES
              ITIPTOE: DO i = 1, NROW
                JTIPTOE: DO j = 1, NCOL
C
C-------------------SET TEMPORARY AQUIFER THICKNESS VARIABLES
                  IF ( IBOUND(j,i,k).EQ.0 ) CYCLE JTIPTOE
                  top   = BOTM(j,i,kk-1)
                  bot   = BOTM(j,i,kk)
                  h     = HNEW(j,i,k)
                  IF ( h.LT.top ) top = h
                  aqb   = ( top - bot )
C
C-------------------SET TEMPORARY ZETA VARIABLES
                  zt    = ZETA(j,i,k,1)
                  zb    = ZETA(j,i,k,NZONES+1)
                  zetac = ZETA(j,i,k,iz)
                  zoldc = ZETASWITS0(j,i,k,iz)
C
C-------------------ONLY EVALUATE TIP AND TOE MOVEMENT TO ADJACENT CELLS FOR CELLS WHERE
C                   ZETA SURFACE IS NOT AT THE TOP (ILPOS=1) OR BOTTOM (ILPOS=2) OF THE LAYER
                  LACTIVE: IF (IPLPOS(j,i,k,iz).EQ.0) THEN
C
C--------------------EVALUATE CHANGE IN ZETA FOR ALL ACTIVE ZONES FOR CURRENT CELL
                    zcalc = rvl * aqb
                    b     = ABS( zetac - zoldc )  
                    IF ( b.GT.zcalc ) THEN
                      IADPTMOD = 1
                      tv       = b / zcalc
                      IF ( tv.GT.ADPTVAL ) ADPTVAL  = tv
                    END IF
C--------------------CHECK TIPS AND TOES FOR EACH COLUMN BUT THE FIRST AND LAST
                    LCOL: IF ( (j.NE.1) .AND. (j.NE.NCOL) ) THEN
C-----------------------LEFT FACE
                      IF ( IPLPOS(j-1,i,k,iz).EQ.1 ) THEN
                        dist  = 0.5 * ( DELR(j) + DELR(j-1) )
                        zcalc = rvl * TIPSLOPE * dist
                        b     = zt - zetac
                        IF ( b.GT.zcalc ) THEN
                          IADPTMOD = 1
                          tv       = b / zcalc
                          IF ( tv.GT.ADPTVAL ) ADPTVAL  = tv
                        END IF
                      ELSEIF ( IPLPOS(j-1,i,k,iz).EQ.2 ) THEN
                        dist  = 0.5 * ( DELR(j) + DELR(j-1) )
                        zcalc = rvl * TOESLOPE * dist
                        b     = zetac - zb
                        IF ( b.GT.zcalc ) THEN
                          IADPTMOD = 1
                          tv       = b / zcalc
                          IF ( tv.GT.ADPTVAL ) ADPTVAL  = tv
                        END IF
                      END IF
C-----------------------RIGHT FACE
                      IF ( IPLPOS(j+1,i,k,iz).EQ.1 ) THEN
                        dist  = 0.5 * ( DELR(j) + DELR(j+1) )
                        zcalc = rvl * TIPSLOPE * dist
                        b     = zt - zetac
                        IF ( b.GT.zcalc ) THEN
                          IADPTMOD = 1
                          tv       = b / zcalc
                          IF ( tv.GT.ADPTVAL ) ADPTVAL  = tv
                        END IF
                      ELSEIF ( IPLPOS(j+1,i,k,iz).EQ.2 ) THEN
                        dist  = 0.5 * ( DELR(j) + DELR(j+1) )
                        zcalc = rvl * TOESLOPE * dist
                        b     = zetac - zb
                        IF ( b.GT.zcalc ) THEN
                          IADPTMOD = 1
                          tv       = b / zcalc
                          IF ( tv.GT.ADPTVAL ) ADPTVAL  = tv
                        END IF
                      END IF
                    END IF LCOL
C
C---------------------CHECK TIPS AND TOES FOR EACH ROW BUT THE FIRST AND LAST
                    LROW: IF ( (i.NE.1) .AND. (i.NE.NROW) ) THEN
C-----------------------BACK FACE
                      IF ( IPLPOS(j,i-1,k,iz).EQ.1 ) THEN
                        dist  = 0.5 * ( DELC(i) + DELC(i-1) )
                        zcalc = rvl * TIPSLOPE * dist
                        b     = zt - zetac
                        IF ( b.GT.zcalc ) THEN
                          IADPTMOD = 1
                          tv       = b / zcalc
                          IF ( tv.GT.ADPTVAL ) ADPTVAL  = tv
                        END IF
                      ELSEIF ( IPLPOS(j,i-1,k,iz).EQ.2 ) THEN
                        dist  = 0.5 * ( DELC(i) + DELC(i-1) )
                        zcalc = rvl * TOESLOPE * dist
                        b     = zetac - zb
                        IF ( b.GT.zcalc ) THEN
                          IADPTMOD = 1
                          tv       = b / zcalc
                          IF ( tv.GT.ADPTVAL ) ADPTVAL  = tv
                        END IF
                      END IF
C-----------------------FRONT FACE
                      IF ( IPLPOS(j,i+1,k,iz).EQ.1 ) THEN
                        dist  = 0.5 * ( DELC(i) + DELC(i+1) )
                        zcalc = rvl * TIPSLOPE * dist
                        b     = zt - zetac
                        IF ( b.GT.zcalc ) THEN
                          IADPTMOD = 1
                          tv       = b / zcalc
                          IF ( tv.GT.ADPTVAL ) ADPTVAL  = tv
                        END IF
                      ELSE IF ( IPLPOS(j,i+1,k,iz).EQ.2 ) THEN
                        dist  = 0.5 * ( DELC(i) + DELC(i+1) )
                        zcalc = rvl * TOESLOPE * dist
                        b     = zetac - zb
                        IF ( b.GT.zcalc ) THEN
                          IADPTMOD = 1
                          tv       = b / zcalc
                          IF ( tv.GT.ADPTVAL ) ADPTVAL  = tv
                        END IF
                      END IF
                    END IF LROW
                  END IF LACTIVE
                END DO JTIPTOE
              END DO ITIPTOE
            END DO IZTIPTOE
          END DO ZTIPTOE
C
C---------RETURN
        RETURN
      END SUBROUTINE SSWI2_CHKSLOPE
C
C
      SUBROUTINE SSWI2_ZCHG(A)
C
C     ******************************************************************
C     CALCULATE SWI2 ZONE CHANGE TERM FOR SWI2 PACKAGE ZONE BUDGETS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
        USE GLOBAL,       ONLY: NCOL,NROW,NLAY,DELR,DELC,IBOUND,
     2                          LBOTM,BOTM,HNEW,HOLD
        USE GWFBASMODULE, ONLY: DELT,HNOFLO,HDRY
        USE GWFSWIMODULE, ONLY: NZONES,IADPT,SWIDELT,ZETA,ZETASWITS0,SSZ
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        REAL, INTENT(INOUT), DIMENSION(NCOL,NROW,NLAY,NZONES+1) :: A
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: i, j, k, iz
        DOUBLEPRECISION :: switfact
        DOUBLEPRECISION :: t1, t0
        DOUBLEPRECISION :: b1, b0
        DOUBLEPRECISION :: sszxa
        REAL :: small
        REAL :: h0, h0dry
        REAL :: rb
        DOUBLEPRECISION :: dsmall
        DOUBLEPRECISION :: dhnoflo, dhdry
        DOUBLEPRECISION :: h1, h1noflo, h1dry
        DOUBLEPRECISION :: db
        DOUBLEPRECISION :: qint
        DOUBLEPRECISION :: dt
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        small    = 1.0E-6
        dsmall   = 1.0D-6
        dhnoflo  = REAL( HNOFLO, 8 )
        dhdry    = REAL( HDRY, 8 )
        switfact = 1.0 / REAL( IADPT, 8 )
        dt       = switfact / REAL( SWIDELT, 8 )
        CZONECHG: DO iz = 1, NZONES
          DO k = 1, NLAY
            DO i = 1, NROW
              DO j = 1, NCOL
C                 CURRENT AND ALCULATE CURRENT ZONE CHANGE
                h1    = HNEW(j,i,k)
                h0    = HOLD(j,i,k)
C                 SKIP INACTIVE CELLS                
                h1noflo = ABS( h1 - dhnoflo )
                IF ( h1noflo.LT.dsmall ) CYCLE
C                 REAL AND DOUBLE PRECISION CELL BOTTOM
                rb = BOTM(j,i,LBOTM(k))
                db = BOTM(j,i,LBOTM(k))
C                 CALCULATE CURRENT ZONE VOLUME
                t1    = ZETA(j,i,k,iz)
                b1    = ZETA(j,i,k,iz+1)
clangevin
clangevin -- potential problem here?  IF ( t1.LT.b1 ) t1 = b1
clangevin
                IF ( t1.LT.b1 ) t1 = b1
C                 RESET THE CURRENT ZONE THICKNESS TO ZERO IF THE CELL IS DRY                
                h1dry = ABS( h1 - dhdry )
                IF ( h1dry.LT.dsmall ) THEN
                  t1 = db
                  b1 = db
                END IF
C                 RESET THE CURRENT ZONE THICKNESS TO ZERO
C                 IF HNEW IS LESS THAN THE BOTTOM OF THE LAYER - FOR MODFLOW-NWT                
                IF ( h1.LT.db ) THEN
                  t1 = db
                  b1 = db
                END IF
C                 CALCULATE PREVIOUS ZONE VOLUME
                t0    = ZETASWITS0(j,i,k,iz)
                b0    = ZETASWITS0(j,i,k,iz+1)
clangevin
clangevin -- potential problem here?  IF ( t0.LT.b0 ) t0 = b0
clangevin
                IF ( t0.LT.b0 ) t0 = b0
C                 RESET THE PREVIOUS ZONE THICKNESS TO ZERO IF THE CELL IS DRY                
                h0dry = ABS( h0 - HDRY )
                IF ( h0dry.LT.small ) THEN
                  t0 = db
                  b0 = db
                END IF
C                 RESET THE PREVIOUS ZONE THICKNESS TO ZERO
C                 IF HOLD IS LESS THAN THE BOTTOM OF THE LAYER - FOR MODFLOW-NWT                
                IF ( h0.LT.rb ) THEN
                  t0 = db
                  b0 = db
                END IF
C                 CALCULATE INCREMENTAL ZONE CHANGE FOR CURRENT SWI TIMESTEP AND
C                 CUMULATIVE ZONE CHANGE FOR CURRENT MODFLOW TIMESTEP                
                sszxa = SSZ(j,i,k)*DELR(j)*DELC(i)
                qint =  ( ( t0 - b0 ) - ( t1 - b1 ) ) * sszxa
                A(j,i,k,iz) = A(j,i,k,iz) + qint * dt
              END DO
            END DO
          END DO
        END DO CZONECHG
C
C---------RETURN
        RETURN
      END SUBROUTINE SSWI2_ZCHG
C
      SUBROUTINE SSWI2_IMIX(A)
C
C     ******************************************************************
C     CALCULATE SWI2 PACKAGE INSTANTANEOUS MIXING TERMS FOR ZONE BUDGETS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
        USE GLOBAL,       ONLY: NCOL,NROW,NLAY,DELR,DELC,IBOUND,CV,
     2                          LBOTM,BOTM,HNEW
        USE GWFBASMODULE, ONLY: DELT
        USE GWFSWIMODULE, ONLY: NZONES,IADPT,SWIDELT,
     2                          NUS,NUBOT,NUTOP,QLEXTRA
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        REAL, INTENT(INOUT), DIMENSION(NCOL,NROW,NLAY,NZONES+1) :: A
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: i, j, k, iz
        INTEGER :: izu, izl
        REAL :: nuuppertop, nuupperbot
        REAL :: nulowertop, nulowerbot
        DOUBLEPRECISION :: switfact
        DOUBLEPRECISION :: ht, ht2
        DOUBLEPRECISION :: db, db2
        DOUBLEPRECISION :: qzbot
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        switfact = 1.0 / REAL( IADPT, 8 )
        KZONEMIX: DO k = 1, NLAY
          IZONEMIX: DO i = 1, NROW
            JZONEMIX: DO j = 1, NCOL
C               SKIP INACTIVE CELLS                
              IF ( IBOUND(j,i,k).EQ.0 ) CYCLE JZONEMIX
C               DETERMINE IF THE HEAD IN CELL IS LESS THAN THE 
C               BOTTOM OF THE CELL - FOR MODFLOW-NWT
              db = BOTM(J,I,LBOTM(k))
              ht = HNEW(J,I,K)
              IF ( ht.LT.db ) CYCLE JZONEMIX
C---------------LOWER FACE
              IF (k.NE.NLAY) THEN
                ht2  = HNEW(j,i,k+1)
                db2  = BOTM(J,I,LBOTM(k+1))
                IF ( IBOUND(j,i,k+1).NE.0 .AND. ht2.GT.db2 ) THEN
                  nuuppertop = NUTOP(j,i,k)
                  nuupperbot = NUBOT(j,i,k)
                  nulowertop = NUTOP(j,i,k+1)
                  nulowerbot = NUBOT(j,i,k+1)
                  IF ( nuupperbot.GT.nulowertop ) THEN
                    qzbot = CV(j,i,k)*(HNEW(j,i,k)-HNEW(j,i,k+1))+
     &                      QLEXTRA(j,i,k)
C                     FIND ZONE NUMBER AT THE BOTTOM OF THE CURRENT LAYER
C                     AND AT THE TOP OF THE UNDERLYING LAYER    
                    IZUPPER: DO iz = 1,NZONES
                      IF ( NUS(iz).EQ.nuupperbot ) THEN
                        izu = iz
                        EXIT IZUPPER
                      END IF
                    END DO IZUPPER
                    IZLOWER: DO iz = 1,NZONES
                      IF ( NUS(iz).EQ.nulowertop ) THEN
                        izl = iz
                        EXIT IZLOWER
                      END IF
                    END DO IZLOWER
C                     DOWNWARD FLOW
                    IF ( qzbot.gt.0.0D0 ) THEN
                      IF ( nulowerbot.GE.nuupperbot ) izl = 0
C                     UPWARD FLOW
                    ELSE
                      IF ( nuuppertop.LE.nulowertop ) izu = 0
                    END IF
                    IF ( izu.GT.0 .AND. izl.GT.0 ) THEN
                      A(j,i,k,izu)   = A(j,i,k,izu)   - qzbot * switfact
                      A(j,i,k+1,izl) = A(j,i,k+1,izl) + qzbot * switfact
                    END IF
                  END IF
                END IF
              END IF
            END DO JZONEMIX
          END DO IZONEMIX
        END DO KZONEMIX
C
C---------RETURN
        RETURN
      END SUBROUTINE SSWI2_IMIX
C
C
      SUBROUTINE SSWI2_MSTO(J,I,K,V,Vc)
C
C     ******************************************************************
C     COMPUTE MODFLOW STORAGE FLOW TERM FROM SC1 AND SC2 TO ADJUST
C     SWI2 PACKAGE BOUNDARY TERM FOR ZONE BUDGETS.
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
        USE GLOBAL,      ONLY:IBOUND,HNEW,HOLD,BOTM,LBOTM
        USE GWFBASMODULE,ONLY:DELT
        USE GWFSWIMODULE
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: J
        INTEGER, INTENT(IN) :: I
        INTEGER, INTENT(IN) :: K
        DOUBLEPRECISION, INTENT(INOUT) :: V
        DOUBLEPRECISION, INTENT(INOUT) :: Vc
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: kk, kt
        INTEGER :: ic
        REAL :: zero
        REAL :: one
        REAL :: tled
        REAL :: rh, rh0, rt, rb
        REAL :: sv, sv1, sv2
        REAL :: sold, snew
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        zero  = 0.
        one   = 1.
        V     = zero
        Vc    = zero
        tled  = one / DELT
C---------DETERMINE SC2 POSITION
        kt = 0
        DO kk = 1, K
          IF( ICONV(kk).NE.0 ) kt = kt + 1
        END DO
C
C---------SET FLAG FOR CONVERTIBLE LAYER 
        ic = ICONV(K)
C---------SKIP NO-FLOW AND CONSTANT-HEAD CELLS.
        IF ( IBOUND(J,I,K).GT.0 ) THEN
          rb  = BOTM(J,I,LBOTM(K))
          rh  = HNEW(J,I,K)
          rh0 = HOLD(J,I,K)
          IF ( rh.LT.rb )  rh  = rb
          IF ( rh0.LT.rb ) rh0 = rb
C           CONVERTIBLE LAYER        
          IF ( ic.NE.0 ) THEN
            rt   = BOTM(J,I,LBOTM(K)-1)
            sv2 = SC2(J,I,kt) * tled
            sv1 = SC1(J,I,K) * tled
            sold = sv2
            IF ( rh0.GT.rt ) sold = sv1
            snew = sv2
            IF ( rh.GT.rt ) snew = sv1
C             TOTAL STORAGE CHANGE
            V = sold * ( rh0 - rt ) + snew * rt - snew * rh
C             CONFINED PORTION OF THE STORAGE CHANGE
            IF ( rh0.LT.rt ) rh0 = rt
            IF ( rh.LT.rt )  rh = rt
            Vc = sv1 * ( rh0 - rt ) + sv1 * ( rt - rh )
C           CONFINED LAYER
          ELSE
            sv = SC1(j,i,k) * tled
            V  = sv * rh0 - sv * rh
            Vc = V
          END IF
        END IF
C---------RETURN
        RETURN
      END SUBROUTINE SSWI2_MSTO
C
C
      SUBROUTINE SSWI2_UPZ1(Klay,Init)
C
C     ******************************************************************
C     UPDATE THE UPPER ZETA SURFACE TO THE TOP OF THE WATER TABLE 
C     FOR UNCONFINED CONDITIONS      
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
        USE GLOBAL,       ONLY: NCOL,NROW,NLAY,IBOUND,LBOTM,BOTM,HNEW
        USE GWFSWIMODULE
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Klay
        INTEGER, INTENT(IN) :: Init
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: i, j
        INTEGER :: iz
        DOUBLE PRECISION :: dtop, dbot, h
        REAL :: rval
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        DO i = 1, NROW
          DO j = 1, NCOL
            IF ( IBOUND(j,i,Klay).EQ.0 ) CYCLE
            dtop=BOTM(j,i,LBOTM(Klay)-1)
            dbot=BOTM(j,i,LBOTM(Klay))
            h = HNEW(j,i,Klay)
C             TOP OF LAYER LESS THAN HEAD
            IF ( dtop.GT.h ) THEN
              IF ( h.GT.dbot ) THEN
                rval = REAL( h, 4 )
              ELSE
                rval = BOTM(j,i,LBOTM(Klay))
              END IF
              ZETA(j,i,Klay,1) = rval
              IF ( Init.EQ.1 ) THEN
                ZETAOLD(j,i,Klay,1)    = rval
                ZETASWITS0(j,i,Klay,1) = rval
              END IF
C             ADJUST ZETA SURFACE 2 TO NZONES IF NECESSARY
C             ASSUME THAT THE UPPER LAYERS ARE ADJUSTED IF THEY
C             EXCEED THE MODIFIED UPPER SURFACE
              DO iz = 2, NZONES
                IF ( ZETA(j,i,Klay,iz).GT.rval ) THEN
                  ZETA(j,i,Klay,iz) = rval
                END IF
                IF ( Init.EQ.1 ) THEN
                  ZETAOLD(j,i,Klay,iz)    = rval
                  ZETASWITS0(j,i,Klay,iz) = rval
                END IF
              END DO
C             TOP OF LAYER LESS THAN OR EQUAL TO HEAD
            ELSE
              ZETA(j,i,Klay,1) = BOTM(j,i,LBOTM(Klay)-1)
              IF ( Init.EQ.1 ) THEN
                ZETAOLD(j,i,Klay,1)    = BOTM(j,i,LBOTM(Klay)-1)
                ZETASWITS0(j,i,Klay,1) = BOTM(j,i,LBOTM(Klay)-1)
              END IF
            END IF
          END DO
        END DO
C
C---------RETURN
        RETURN
      END SUBROUTINE SSWI2_UPZ1
C
C
       SUBROUTINE SSWI2_SET_IPLPOS()
C
C     ******************************************************************
C     SET FLAG FOR LOCATION OF THE ZETA SURFACE RELATIVE TO THE TOP AND
C     BOTTOM OF A CELL
C       IPLPOS=1 AT TOP
C       IPLPOS=2 AT BOTTOM
C       IPLPOS=0 IN BETWEEN, AND
C       IPLPOS=3 IN INACTIVE CELLS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
        USE GLOBAL,       ONLY: NCOL,NROW,NLAY,IBOUND,LBOTM,BOTM,HNEW
        USE GWFSWIMODULE
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: i, j, k, iz
        INTEGER :: kk
        DOUBLEPRECISION :: db,ht
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        K_IPLPOS: DO k=1,NLAY
          kk = LBOTM(k)
          I_IPLPOS: DO i=1,NROW
            J_IPLPOS: DO j=1,NCOL
              IPLPOS(j,i,k,1) = 0
              db = BOTM(j,i,kk)
              ht = HNEW(j,i,k)
              IZ_IPLPOS: DO iz=2,NZONES
                IF ( IBOUND(j,i,k).EQ.0 ) THEN
                  IPLPOS(j,i,k,iz) = 3
                ELSE IF (ZETA(j,i,k,iz).GE.ZETA(j,i,k,1)) THEN
                  IPLPOS(j,i,k,iz) = 1
                ELSEIF (ZETA(j,i,k,iz).LE.ZETA(j,i,k,NZONES+1)) THEN
                  IPLPOS(j,i,k,iz) = 2
C-----------------SET ILPOS TO THE BOTTOM OF THE LAYER (IPLPOS=2) IF THE
C                 THE HEAD IN THE CELL IS LESS THAN THE BOTTOM OF THE 
C                 CELL - FOR MODFLOW-NWT
                ELSE IF (ht.LT.db) THEN
                  IPLPOS(j,i,k,iz) = 2
                ELSE
                  IPLPOS(j,i,k,iz) = 0
                END IF
              END DO IZ_IPLPOS
            END DO J_IPLPOS
          END DO I_IPLPOS
        END DO K_IPLPOS
C
C---------RETURN
        RETURN
      END SUBROUTINE SSWI2_SET_IPLPOS
C
C
      CHARACTER (LEN=17) FUNCTION SSWI2_BDCHAR(R) RESULT(value)
C
C     ******************************************************************
C     CREATE SWI2 ZONE BUDGET TERMS CHARACTER STRINGS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        REAL, INTENT(IN) :: R
C     + + + LOCAL DEFINITIONS + + +
        REAL :: t
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        t = ABS(R)
        IF(t.NE.0.0 .AND.
     1    (t.GE.9.99999E11 .OR. t.LT.0.1) ) THEN
          WRITE(value,'(1PE17.4)') R
        ELSE
          WRITE(value,'(F17.4)') R
        END IF
C---------RETURN
        RETURN
      END FUNCTION SSWI2_BDCHAR
C
C
      SUBROUTINE SSWI2_RD_COMM(Iu)
C
C     ******************************************************************
C     READ NON COMMENT AND NON BLANK LINE FROM SWI2 INPUT FILE
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
        IMPLICIT NONE
C       + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: Iu
C       + + + LOCAL DEFINITIONS + + +
        CHARACTER (LEN=2), PARAMETER :: comment = '//'
        CHARACTER (LEN=200) :: line
        LOGICAL :: iscomment
        INTEGER :: ios
        line = comment
        DO
          READ (Iu,'(A)',IOSTAT=ios) line
          IF (ios /= 0) CALL USTOP('COULD NOT READ FROM UNIT Iu')
          IF (LEN_TRIM(line).LT.1) THEN
            line = comment
            CYCLE
          END IF
          line = TRIM(ADJUSTL(line))
          iscomment = .FALSE.
          SELECT CASE (line(1:1))
            CASE ('#')
              iscomment = .TRUE.
            CASE ('!')
              iscomment = .TRUE.
            CASE DEFAULT
              IF (line(1:2).EQ.comment) iscomment = .TRUE.
          END SELECT
          IF (.NOT.iscomment) THEN
            BACKSPACE(Iu)
            RETURN
          END IF
        END DO
        RETURN
      END SUBROUTINE SSWI2_RD_COMM
