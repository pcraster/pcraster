C     Last change:  ERB  21 Nov 2001    9:58 am
      SUBROUTINE SEN1ETS1FM(NCOL,NROW,NLAY,DELR,DELC,RMLT,NETSOP,IETS,
     &                      IBOUND,RHS,ETSS,ETSX,HNEW,IZON,NMLTAR,
     &                      NZONAR,IP,NETSEG,PXDP,PETM,NSEGAR)
C-----VERSION 20011121 ERB
C     ******************************************************************
C     CALCULATE FORCING FUNCTION DERIVATIVE FOR ETS AND ADD TO RHS.
C     ******************************************************************
C     Modified 11/21/2001 to support parameter instances - ERB
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL DDD, DELC, DELR, ETSX, H, RHS, S, SM, RMLT, ETSS, XXX, ZERO
      INTEGER IBOUND, IC, IFL, NETSOP, IETS, IR, IZ, K, KK, IZON, NCOL,
     &        NLAY, NROW, NZ
      DOUBLE PRECISION RO, HNEW(NCOL,NROW,NLAY), PXDP1, PXDP2
      DIMENSION DELR(NCOL), DELC(NROW), IETS(NCOL,NROW),
     &          RMLT(NCOL,NROW,NMLTAR), IBOUND(NCOL,NROW,NLAY),
     &          RHS(NCOL,NROW,NLAY), ETSS(NCOL,NROW), 
     &          ETSX(NCOL,NROW), IZON(NCOL,NROW,NZONAR),
     &          PXDP(NCOL,NROW,NSEGAR), PETM(NCOL,NROW,NSEGAR)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
      NI = IACTIVE(IP)
      IF (NI.EQ.0) RETURN
      ZERO = 0.0
      ICL1 = IPLOC(1,IP)
      ICL2 = IPLOC(2,IP)
      NUMINST = IPLOC(3,IP)
      IF (NUMINST.GT.1) THEN
C       SELECT CORRECT INSTANCE
        NCLU = (ICL2-ICL1+1)/NUMINST
        ICL1 = ICL1+(NI-1)*NCLU
        ICL2 = ICL1+NCLU-1
      ENDIF
C-----LOOP THROUGH CLUSTERS
      DO 70 K = ICL1, ICL2
        M = IPCLST(2,K)
        LZ1 = IPCLST(3,K)
        LASTZ = IPCLST(4,K)
C-------LOOP THROUGH CELLS
        DO 60 IR = 1, NROW
          DO 50 IC = 1, NCOL
            SM = 1.
            IF (M.GT.0) SM = RMLT(IC,IR,M)
            IF (LZ1.GT.0) THEN
              IFL = 0
              DO 10 IZ = 5, LASTZ
                NZ = IPCLST(IZ,K)
                IF (NZ.EQ.0 .OR. IFL.EQ.1) GOTO 20
                IF (NZ.EQ.IZON(IC,IR,LZ1)) IFL = 1
   10         CONTINUE
   20         IF (IFL.EQ.0) SM = ZERO
            ENDIF
            IF (SM.EQ.ZERO) GOTO 50
C--------NETSOP=1
            IF (NETSOP.EQ.1 .AND. IBOUND(IC,IR,1).LT.1) GOTO 50
            IF (NETSOP.EQ.1 .AND. IBOUND(IC,IR,1).GT.0) THEN
              RO = SM*DELR(IC)*DELC(IR)
              KK = 1
            ENDIF
C--------NETSOP=2
            IF (NETSOP.EQ.2) THEN
              IF (IETS(IC,IR).EQ.0) GOTO 50  ! ERB 1/11/07
              IF (IBOUND(IC,IR,(IETS(IC,IR))).LT.1) GOTO 50
              IF (IBOUND(IC,IR,(IETS(IC,IR))).GT.0) THEN
                RO = SM*DELR(IC)*DELC(IR)
                KK = IETS(IC,IR)
              ENDIF
            ENDIF
C--------ADJUST
            RO = -RO
            S = ETSS(IC,IR)
            H = HNEW(IC,IR,KK)
            IF (H.LT.S) THEN
              DDD = S - H
              XXX = ETSX(IC,IR)
              IF (DDD.GE.XXX) GOTO 50
C--------VARIABLE RANGE. CALCULATE DERIVATIVE AND ADD TO RHS
C
              IF (NETSEG.GT.1) THEN
C               DETERMINE WHICH SEGMENT APPLIES BASED ON HEAD
C
C               SET PROPORTIONS CORRESPONDING TO ETSS ELEVATION
                PXDP1 = 0.0
                PETM1 = 1.0
                DO 30 ISEG = 1,NETSEG
C                 SET PROPORTIONS CORRESPONDING TO LOWER END OF
C                 SEGMENT
                  IF (ISEG.LT.NETSEG) THEN
                    PXDP2 = PXDP(IC,IR,ISEG)
                    PETM2 = PETM(IC,IR,ISEG)
                  ELSE
                    PXDP2 = 1.0
                    PETM2 = 0.0
                  ENDIF
                  IF (DDD.LE.PXDP2*XXX) THEN
C                   HEAD IS IN DOMAIN OF THIS SEGMENT
                    GOTO 40
                  ENDIF
C                 PROPORTIONS AT LOWER END OF SEGMENT WILL BE FOR
C                 UPPER END OF SEGMENT NEXT TIME THROUGH LOOP
                  PXDP1 = PXDP2
                  PETM1 = PETM2
   30           CONTINUE
   40           CONTINUE
C               CALCULATE FORCING FUNCTION DERIVATIVE BASED ON
C               SEGMENT THAT APPLIES AT HEAD ELEVATION
                PSLOPE = (PETM1-PETM2)/(PXDP2-PXDP1)
                RO = RO*(PETM1+PSLOPE*(PXDP1-DDD/XXX))
              ELSE
                RO = RO*(1.-(DDD/XXX))
              ENDIF
            ENDIF
C--------CONTRIBUTIONS TO RHS.
            RHS(IC,IR,KK) = RHS(IC,IR,KK) - RO
   50     CONTINUE
   60   CONTINUE
   70 CONTINUE
C
      RETURN
      END
