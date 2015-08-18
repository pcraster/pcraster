C     Last change:  ERB  26 Jul 2002    1:30 pm
CGR PARALLEL CODE FOR MODFLOW-2000

C ----------------------------------------------------------------------
      SUBROUTINE PLL1IN
C
C     INITIALIZE PROCESSOR INFORMATION
C
      INCLUDE 'parallel.inc'
C
      MYID = 0
      MPROC = 0
      NUMPROCS = 1
C
      RETURN
      END

C ----------------------------------------------------------------------
      SUBROUTINE PLL1OP(IERRU,IERR)
      CHARACTER*(11) FN
C
      IERR = 0
      FN = 'mf2kerr.p00'
      OPEN(UNIT=IERRU,FILE=FN)
      RETURN
      END

C ----------------------------------------------------------------------
      SUBROUTINE PLL1FN(FNAME)
      CHARACTER*(*) FNAME
C
      RETURN
      END

C ----------------------------------------------------------------------
      SUBROUTINE PLL1AS (NPE)
C
C     ASSIGN PARAMETERS TO PROCESSORS
C
      INCLUDE 'parallel.inc'
C
      IPDO(0)=-1
C     BECAUSE PARALLEL PROCESSING IS NOT ENABLED, ALL PARAMETERS ARE
C     ASSIGN TO MPI PROCESS 0
      DO NPSET=1,NPE
        IPDO(NPSET)=0
      ENDDO
      RETURN
      END

C ----------------------------------------------------------------------
      SUBROUTINE PLL1MX (X,XND,IL,JL)
C
C     MERGE X, A REPLICATED ARRAY USING A SUM ACTIVITY.
C
      DIMENSION X(IL,JL), XND(JL)
      RETURN
      END

C ----------------------------------------------------------------------
      SUBROUTINE PLL1CV(IFO)
C      SIMPLE ROUTINE TO BROADCAST IFO
C MUST BE CALLED ON ALL PROCESSORS.
      RETURN
      END

C ----------------------------------------------------------------------
      SUBROUTINE PLL1BA(AB,NDATA)
C      SIMPLE ROUTINE TO BROADCAST ARRAY AB, NDATA ITEMS
C MUST BE CALLED ON ALL PROCESSORS.
      RETURN
      END

C ----------------------------------------------------------------------
      SUBROUTINE PLL1CL
      RETURN
      END

C ----------------------------------------------------------------------
      SUBROUTINE PLL1BR()
C
      RETURN
      END

C=======================================================================
      SUBROUTINE PLL1EH(IERR,IERRU,IOUT,IOUTG,MINERR)
C     VERSION 20000215 ERB
C     ******************************************************************
C     ERROR-HANDLING ROUTINE:  CHECK FOR ERROR CONDITION RAISED BY ANY
C     MPI PROCESS.  IF ANY MPI PROCESS REPORTS AN ERROR, CLOSE FILES ON
C     ALL PROCESSES AND STOP ALL MPI PROCESSES
C     ******************************************************************
C
      IF (IERR.GT.0) CALL PLL1SD(IERR,IERRU,IOUT,IOUTG)
      MINERR = IERR
C
      RETURN
      END
C ----------------------------------------------------------------------
      SUBROUTINE PLL1SD(IERR,IERRU,IOUT,IOUTG)
C     VERSION 20000215 ERB
C     ******************************************************************
C     FOR SERIAL-PROCESSING ENVIRONMENT:  CLOSE ALL FILES AND STOP
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      LOGICAL LOP
      CHARACTER*12 FN
      INCLUDE 'parallel.inc'
C     ---------------------------------------------------------------
  500 FORMAT(/,' Error condition or warning reported')
  520 FORMAT(/,' File "',a11,
     &'" contains a description of the error or warning')
C     ---------------------------------------------------------------
C
C-----CLOSE ERROR FILE, DELETE IF EMPTY, KEEP IF NOT.
      FN = 'mf2kerr.p00'
      IF (IERR.EQ.0) THEN
        CLOSE(IERRU)
        OPEN(UNIT=IERRU,FILE=FN)
        CLOSE(UNIT=IERRU,STATUS='DELETE')
      ELSE
        WRITE(*,500)
        WRITE(*,520) FN
        CLOSE(UNIT=IERRU)
      ENDIF
C
C-----CLOSE ALL OTHER FILES
      DO 340 I=1,MAXUNIT
        INQUIRE (UNIT=I,OPENED=LOP)
        IF (LOP) CLOSE (I)
  340 CONTINUE
C
      STOP
      END
C=======================================================================
      SUBROUTINE PLL1DE(IERRU,IOUT,IOUTG)
C     VERSION 20000215 ERB
C     ******************************************************************
C     CLOSE AND DELETE FILE THAT WAS OPENED TO HOLD WARNINGS AND ERROR
C     MESSAGES.  THIS ROUTINE SHOULD BE CALLED ONLY IF IERR = 0
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*12 FN
C     ---------------------------------------------------------------
C
C-----CLOSE AND DELETE ERROR FILE.
      FN = 'mf2kerr.p00'
      CLOSE(IERRU)
      OPEN(UNIT=IERRU,FILE=FN)
      CLOSE(UNIT=IERRU,STATUS='DELETE')
C
      RETURN
      END

