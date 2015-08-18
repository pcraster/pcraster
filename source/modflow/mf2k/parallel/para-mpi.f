C     Last change:  ERB  24 Jul 2001    4:28 pm
C
C     Note that the statements that INCLUDE 'dlimport.inc' are required
C     for compilation using a Lahey compiler under the NT operating
C     system, but not for unix - ERB
C
CGR PARALLEL CODE FOR MODFLOW-2000
C=======================================================================
      SUBROUTINE PLL1IN
C
C     ******************************************************************
C     INITIALIZE MPI PROCESS INFORMATION
C     ******************************************************************
C
      INCLUDE 'mpif.h'
      INCLUDE 'parallel.inc'
      INCLUDE 'dlimport.inc'
      COMMON /ILOCAL/ILPASS
C
      INTEGER IFLEN
C
C     INITIALIZE PARALLEL VARIABLES
      MYID = 0
      MPROC = 0
      NUMPROCS = 1
      ILPASS = 0
C
      CALL MPI_Init( IERRPLL )
      CALL MPI_Comm_rank( MPI_COMM_WORLD, MYID, IERRPLL )
      CALL MPI_Comm_size( MPI_COMM_WORLD, NUMPROCS, IERRPLL )
      WRITE(*,*) "Process ", MYID, " of ", NUMPROCS, " is alive"
C
CGR SET MASTER/OVERSEER MPI PROCESS NUMBER
      MPROC=0
C
      RETURN
      END
C=======================================================================
      SUBROUTINE PLL1FN(FNAME)
C     ******************************************************************
C     BROADCAST FNAME TO ALL MPI PROCESSES
C     ******************************************************************
      CHARACTER*200 FNAME
      INTEGER IFLEN
      INCLUDE 'mpif.h'
      INCLUDE 'parallel.inc'
      INCLUDE 'dlimport.inc'
C
      IFLEN = LEN(FNAME)
      IDSIZE = 1
      CALL MPI_Bcast(IFLEN,IDSIZE,MPI_INTEGER,MPROC,
     &     MPI_COMM_WORLD, IERRPLL)

      CALL MPI_Bcast(FNAME,IFLEN,MPI_CHARACTER,MPROC,
     &     MPI_COMM_WORLD, IERRPLL)
C
C CALL BARRIER TO ENSURE AT THE SAME POINT
      CALL MPI_Barrier(MPI_COMM_WORLD, IERRPLL)
C
      RETURN
      END
C=======================================================================
      SUBROUTINE PLL1OP(IERRU,IERR)
C
C     ******************************************************************
C     OPEN A FILE FOR THIS MPI PROCESS TO HOLD WARNINGS AND ERROR
C     MESSAGES
C     ******************************************************************
C
      CHARACTER*1 DIGIT(0:9)
      CHARACTER*2 PSUF
      CHARACTER*11 FN
      INCLUDE 'parallel.inc'
C
      DATA (DIGIT(I),I=0,9)/'0','1','2','3','4','5','6','7','8','9'/
C     ------------------------------------------------------------------
      IERR = 0
      PSUF = '00'
      IF (MYID.GT.0) THEN
        ITEN=MYID/10
        IONE=MOD(MYID,10)
        PSUF(1:1)=DIGIT(ITEN)
        PSUF(2:2)=DIGIT(IONE)
      ENDIF
C
      FN = 'mf2kerr.p'//PSUF
      OPEN(IERRU,FILE=FN,ACTION='WRITE')
C
      RETURN
      END
C=======================================================================
      SUBROUTINE PLL1AS(NPE)
C
C     ******************************************************************
C     ASSIGN PARAMETERS TO MPI PROCESSES
C     ******************************************************************
C
cc      INCLUDE 'mpif.h'    
      INCLUDE 'parallel.inc'
      INTEGER NPE
      INTEGER NPDO, NPSET, IPFIRST
C
      IPDO(0)=-1
C     SET IN CYCLIC DISTRIBUTION
      NPDO=0
      DO NPSET=1,NPE
        IPDO(NPSET)=NPDO
        NPDO=NPDO+1
        IF(NPDO.GE.NUMPROCS) NPDO=0
      ENDDO
C
C FIND FIRST PARAMETER MPI PROCESS DOES
      IPFIRST=-1
      DO NPSET=1,NPE
        IF(IPDO(NPSET).EQ.MYID) THEN
          IF(IPFIRST.EQ.-1) IPFIRST=NPSET
        ENDIF
      ENDDO
C
      RETURN
      END
C=======================================================================
      SUBROUTINE PLL1MX(X,XND,IL,JL)
C     ******************************************************************
C     MERGE X, A REPLICATED ARRAY, USING BLOCKING SENDS AND RECEIVES.
C     ******************************************************************
C
      INCLUDE 'mpif.h'
      INCLUDE 'parallel.inc'
      INCLUDE 'dlimport.inc'
C
      DIMENSION X(IL,JL), XND(JL), ISTATUS(MPI_STATUS_SIZE)
      COMMON /ILOCAL/ ILPASS
C
      ILPASS=ILPASS+1
C
      IF (NUMPROCS.GT.1) THEN
C
C       SEND DATA FROM HOST MPI PROCESS
C
        IF (MYID .NE. MPROC) THEN  ! Processes other than master do this
          DO I=1,IL
            IF(IPDO(I).EQ.MYID) THEN
              ITAG=I+1000*ILPASS
              DO J=1,JL
                XND(J) = X(I,J)
              ENDDO
              CALL MPI_Send(XND,JL,MPI_REAL,MPROC,ITAG,
     &                      MPI_COMM_WORLD,IERRPLL)
            ENDIF
          ENDDO
        ELSE             ! Only the master process does this part
C       RECEIVE ALL REMOTE ROWS
          DO I=1,IL
            IF(IPDO(I).NE.MYID) THEN
              ITAG=I+1000*ILPASS
              CALL MPI_Recv(XND,JL,MPI_REAL,IPDO(I),ITAG,
     &                      MPI_COMM_WORLD,ISTATUS,IERRPLL)
              DO J=1,JL
                X(I,J) = XND(J)
              ENDDO
            ENDIF
          ENDDO
        ENDIF
C
C       NOW BROADCAST X
C
        NBDATA=IL*JL
        CALL MPI_Bcast(X,NBDATA,MPI_REAL,MPROC,MPI_COMM_WORLD,IERRPLL)
C
      ENDIF
C
      RETURN
      END
C=======================================================================
      SUBROUTINE PLL1CV(I)
C
C     ******************************************************************
C     BROADCAST AN INTEGER.  MUST BE CALLED ON ALL MPI PROCESSES.
C     ******************************************************************
C
      INTEGER I
C
      INCLUDE 'mpif.h'
      INCLUDE 'parallel.inc'
      INCLUDE 'dlimport.inc'
C
      CALL MPI_Bcast(I,1,MPI_INTEGER,MPROC,MPI_COMM_WORLD,IERRPLL)
C
      RETURN
      END
C=======================================================================
      SUBROUTINE PLL1BA(AB,NDATA)
C
C     ******************************************************************
C     BROADCAST ARRAY AB, NDATA ITEMS.  MUST BE CALLED ON ALL MPI
C     PROCESSES.
C     ******************************************************************
C
      INTEGER NDATA 
      REAL AB(*) 
C
      INCLUDE 'mpif.h'
      INCLUDE 'parallel.inc'
      INCLUDE 'dlimport.inc'
C
      CALL MPI_Bcast (AB,NDATA,MPI_REAL,MPROC,MPI_COMM_WORLD,IERRPLL)
C
      RETURN
      END
C=======================================================================
      SUBROUTINE PLL1CL()
C     ******************************************************************
C     CLOSE MPI SERVICES
C     ******************************************************************
C
      INCLUDE 'mpif.h'
      INCLUDE 'parallel.inc'
      INCLUDE 'dlimport.inc'
C
      CALL MPI_Finalize(IERRPLL)
C
      RETURN
      END
C=======================================================================
      SUBROUTINE PLL1BR()
C     ******************************************************************
C     USED ONLY TO CALL MPI_Barrier ROUTINE
C     ******************************************************************
      INCLUDE 'mpif.h'
      INCLUDE 'parallel.inc'
      INCLUDE 'dlimport.inc'
C
      CALL MPI_Barrier(MPI_COMM_WORLD,IERRPLL)
C
      RETURN
      END
C=======================================================================
      SUBROUTINE PLL1EH(IERR,IERRU,IOUT,IOUTG,MINERR)
C     VERSION 20000214 ERB
C     ******************************************************************
C     ERROR-HANDLING ROUTINE:  CHECK FOR ERROR CONDITION RAISED BY ANY
C     MPI PROCESS.  IF ANY MPI PROCESS REPORTS AN ERROR, CLOSE FILES ON
C     ALL MPI PROCESSES AND STOP ALL MPI PROCESSES
C     ******************************************************************
C
      INCLUDE 'mpif.h'
      INCLUDE 'dlimport.inc'
C
      MAXERR = 0
      MINERR = 0
C
C     DETERMINE THE LARGEST VALUE OF IERR AND COMMUNICATE IT TO ALL
C     MPI PROCESSES AS MAXERR
      CALL MPI_Allreduce(IERR,MAXERR,1,MPI_INTEGER,MPI_MAX,
     &                   MPI_COMM_WORLD,IERRPLL)
      IF (MAXERR.GT.0) CALL PLL1SD(IERR,IERRU,IOUT,IOUTG)
      CALL MPI_Allreduce(IERR,MINERR,1,MPI_INTEGER,MPI_MIN,
     &                   MPI_COMM_WORLD,IERRPLL)
C
      END
C=======================================================================
      SUBROUTINE PLL1SD(IERR,IERRU,IOUT,IOUTG)
C     VERSION 20000208 ERB
C     ******************************************************************
C     SHUTDOWN PROGRAM IN CASE OF ERROR CONDITION.  METHODICALLY CLOSE
C     ALL FILES TO ENSURE THAT THE FILES ASSOCIATED WITH THE MASTER
C     MPI PROCESS ARE CLOSED LAST, AND ATTEMPT TO END ALL MPI PROCESSES.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      LOGICAL LOP, LEX
      CHARACTER*1 DIGIT(0:9)
      CHARACTER*2 PSUF
      CHARACTER*12 FN
      CHARACTER*100 LINE
      INCLUDE 'mpif.h'
      INCLUDE 'parallel.inc'
      INCLUDE 'dlimport.inc'
C
      DATA (DIGIT(I),I=0,9)/'0','1','2','3','4','5','6','7','8','9'/
C     ---------------------------------------------------------------
C
      ITEN = MYID/10
      IONE = MOD(MYID,10)
      PSUF(1:1) = DIGIT(ITEN)
      PSUF(2:2) = DIGIT(IONE)
C
C-----CLOSE ERROR FILES, DELETE IF EMPTY, KEEP IF NOT.
      IF (IERR.EQ.0) THEN
        CLOSE(IERRU)
        FN = 'mf2kerr.p'//PSUF
        OPEN(UNIT=IERRU,FILE=FN)
        CLOSE(UNIT=IERRU,STATUS='DELETE',IOSTAT=ISTAT)
      ELSE
          WRITE(*,500) MYID
        CLOSE(UNIT=IERRU)
      ENDIF
  500 FORMAT(/,' Error condition or warning reported by process ',i3)
C
      CALL MPI_Barrier(MPI_COMM_WORLD,IERRPLL)

C-----IF MASTER MPI PROCESS, LOOK FOR ERROR FILES.  IF FOUND, APPEND
C     CONTENTS TO IOUTG
      IF (MYID.EQ.MPROC) THEN
        IF (NUMPROCS.GT.1) THEN
          DO 300 I=0,NUMPROCS-1
            ITEN = I/10
            IONE = MOD(I,10)
            PSUF(1:1) = DIGIT(ITEN)
            PSUF(2:2) = DIGIT(IONE)
            FN = 'mf2kerr.p'//PSUF
            INQUIRE(FILE=FN,EXIST=LEX)
            IF (LEX) THEN
              IF (MYID.NE.I) THEN
                OPEN(IERRU,FILE=FN)
                DO 280 J=1,100
                  READ(IERRU,'(A)',END=290) LINE
                  WRITE(IOUTG,'(A)') LINE
  280           CONTINUE
  290           CONTINUE
                CLOSE(IERRU)
              ENDIF
              WRITE(*,520)FN
            ENDIF
  300     CONTINUE
        ENDIF
      ENDIF
  520 FORMAT(/,' File "',a11,
     &'" contains a description of the error or warning')
C
      CALL MPI_Barrier(MPI_COMM_WORLD,IERRPLL)
C
C-----CLOSE MAIN OUTPUT FILES, DELETE IF NOT MASTER MPI PROCESS
C     AND NO ERROR IN THIS MPI PROCESS
      IF (MYID.NE.MPROC .AND. IERR.EQ.0) THEN
        CLOSE(IOUTG)
        FN = 'mf2kglob.p'//PSUF
        OPEN(UNIT=IOUTG,FILE=FN)
        CLOSE(IOUTG,STATUS='DELETE')
        IF (IOUT.NE.IOUTG) THEN
          CLOSE(IOUT)
          FN = 'mf2klist.p'//PSUF
          OPEN(UNIT=IOUT,FILE=FN)
          CLOSE(IOUT,STATUS='DELETE')
        ENDIF
      ELSE
        CLOSE(IOUTG)
        IF (IOUT.NE.IOUTG) CLOSE(IOUT)
        IF (IERR.NE.0) THEN
          IF (MYID.NE.MPROC) WRITE(*,510)'mf2kglob.p'//PSUF,
     &                                   'mf2klist.p'//PSUF
        ENDIF
      ENDIF
  510 FORMAT(/,' File "',a,'" or "',a,'" may contain additional',/,
     &' information related to the error or warning.')
C
C     CLOSE ALL OTHER FILES
C
      DO 350 J=NUMPROCS,1,-1
        CALL MPI_Barrier(MPI_COMM_WORLD,IERRPLL)
        IF (MYID.EQ.J-1) THEN
          DO 340 I=1,MAXUNIT
            INQUIRE (UNIT=I,OPENED=LOP)
            IF (LOP .AND. I.NE.5 .AND. I.NE.6) CLOSE (I)
  340     CONTINUE
        ENDIF
  350 CONTINUE
C
C     END MPI PROCESSES
C
      CALL MPI_Abort(MPI_COMM_WORLD,IERRCODE,IERRPLL)
      END
C=======================================================================
      SUBROUTINE PLL1DE(IERRU,IOUT,IOUTG)
C     VERSION 20000215 ERB
C     ******************************************************************
C     CLOSE AND DELETE FILES THAT WERE OPENED TO HOLD WARNINGS AND ERROR
C     MESSAGES.  THIS ROUTINE SHOULD BE CALLED ONLY IF IERR = 0 FOR ALL
C     MPI PROCESSES
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      LOGICAL LOP
      CHARACTER*1 DIGIT(0:9)
      CHARACTER*2 PSUF
      CHARACTER*12 FN
      INCLUDE 'parallel.inc'
C
      DATA (DIGIT(I),I=0,9)/'0','1','2','3','4','5','6','7','8','9'/
C     ---------------------------------------------------------------
C
      ITEN = MYID/10
      IONE = MOD(MYID,10)
      PSUF(1:1) = DIGIT(ITEN)
      PSUF(2:2) = DIGIT(IONE)
C
C-----CLOSE AND DELETE ERROR FILE FOR THIS MPI PROCESS.
      FN = 'mf2kerr.p'//PSUF
      CLOSE(IERRU)
      OPEN(UNIT=IERRU,FILE=FN)
      CLOSE(UNIT=IERRU,STATUS='DELETE')
C
C-----CLOSE AND DELETE MAIN OUTPUT FILES IF THIS IS NOT THE MASTER
C     MPI PROCESS
C
      IF (MYID.NE.MPROC) THEN
C-------CLOSE AND DELETE GLOBAL FILE.
        FN = 'mf2kglob.p'//PSUF
        CLOSE(IOUTG)
        OPEN(UNIT=IOUTG,FILE=FN)
        CLOSE(UNIT=IOUTG,STATUS='DELETE')
C
C-------CLOSE AND DELETE LIST FILE.
        IF (IOUT.NE.IOUTG) THEN
          FN = 'mf2klist.p'//PSUF
          CLOSE(IOUT)
          OPEN(UNIT=IOUT,FILE=FN)
          CLOSE(UNIT=IOUT,STATUS='DELETE')
        ENDIF
      ENDIF
C
      RETURN
      END

