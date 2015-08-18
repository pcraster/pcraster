      SUBROUTINE LMG1ALG(ISUM,ISUMI,LCA,LCIA,LCJA,LCU1,LCFRHS,LCIG,
     1                   ISIZ1,ISIZ2,ISIZ3,ISIZ4,ICG,NCOL,NROW,NLAY,
     2                   IN,IOUT,IFREFM)
C
C     ******************************************************************
C     LMG / AMG can no longer be distributed.  Write message to this
C     effect and stop execution.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
C
C1------PRINT A MESSAGE IDENTIFYING AMG PACKAGE
      WRITE(IOUT,500)
  500 FORMAT(1X,/,1X,'LMG -- ALGEBRAIC MULTI-GRID SOLUTION PACKAGE:',/,
     &    1X,'BECAUSE OF LICENSING RESTRICTIONS, THE ALGEBRAIC',
     &    ' MULTI-GRID SOLVER CAN NO',/,
     &    1X,'LONGER BE DISTRIBUTED BY THE USGS.  PLEASE CHOOSE',
     &    ' ANOTHER SOLVER PACKAGE.')
      CALL USTOP(' ')
C
      RETURN
      END
C
C***********************************************************************
      SUBROUTINE LMG1RPG(IN,MXITER,MXCYC,BCLOSE,DAMP,IOUTAMG,IOUT,
     &                  IFREFM,ICG,IADAMP,DUP,DLOW,HCLOSE)
C
      END
C
C***********************************************************************
      SUBROUTINE LMG1AP(HNEW,IBOUND,CR,CC,CV,HCOF,RHS,A,IA,JA,U,FRHS,IG,
     &                  ISIZ1,ISIZ2,ISIZ3,ISIZ4,KITER,BCLOSE,DAMP,ICNVG,
     &                  KSTP,KPER,MXITER,MXCYC,NCOL,NROW,NLAY,NODES,
     &                  HNOFLO,IOUT,IOUTAMG,ICG,IADAMP,DUP,DLOW)
C
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLEPRECISION HNEW
      DOUBLEPRECISION A(ISIZ1),U(ISIZ4),FRHS(ISIZ4)
      DIMENSION  IBOUND(NODES), HNEW(NODES),CR(NODES), CC(NODES),
     1   CV(NODES), HCOF(NODES), RHS(NODES), IA(ISIZ2),
     2   JA(ISIZ1),IG(ISIZ3)
C
      END
