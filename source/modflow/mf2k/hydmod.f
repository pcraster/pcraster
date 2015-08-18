C     Last change:  ERB  12 Sep 2002    5:04 pm
C*******************************************************************************
      SUBROUTINE GWF1HYD1ALP(ISUM,LCHYDM,NHYDM,IHYDMUN,HYDNOH,
     1IN,IOUT)
C
C-----VERSION 14SEPT2000 GWF1HYD1ALP
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR HYDROGRAPH PROGRAM
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'hydmod.inc'
      CHARACTER*80 LINE
C     ------------------------------------------------------------------
C
C1------IDENTIFY PROGRAM.
      WRITE(IOUT,1) IN
    1 FORMAT(1H0,'HYDM -- HYDROGRAPH PROGRAM, VERSION 1.0,',
     1     ' 09/14/2000',' INPUT READ FROM UNIT',I3)
C
C4------READ NUMBER OF HYDROGRAPHS AND UNIT FOR SAVING UNFORMATTED
C4------HYDROGRAPH FILE AND NUMERIC FLAG FOR DRY/INACTIVE CELLS
      READ(IN,'(A)') LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NHYDM,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IHYDMUN,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,HYDNOH,IOUT,IN)
      WRITE(IOUT,5) NHYDM,IHYDMUN,HYDNOH
    5 FORMAT(1X,'      NUMBER OF HYDROGRAPH POINTS:',I4,
     1     /,1X,'HYDROGRAPHS WILL BE SAVED ON UNIT:',I4,
     2     /,1X,'PERIODS FOR DRY/INACTIVE CELLS WILL USE',
     3         ' THE NUMERIC FLAG:',F10.2)
      IF(NHYDM.GT.NPTSLBL) THEN
         WRITE(IOUT,*)
     1   ' PTSLBL is not dimensioned large enough in HYDROGRAPH Option'
         CALL USTOP(' ')
      END IF
C
C3------SET LCHYDM EQUAL TO ADDRESS OF FIRST UNUSED SPACE IN X.
      LCHYDM=ISUM
C
C4------CALCULATE AMOUNT OF SPACE USED BY THE HYDROGRAPH LIST.
      ISUM=ISUM+18*NHYDM
      ISP=18*NHYDM
      ISUM=ISUM+ISP
C
C5------PRINT AMOUNT OF SPACE USED BY THE HYD PROGRAM
      WRITE(IOUT,6) ISP
    6 FORMAT(1X,I8,' ELEMENTS IN RX ARRAY ARE USED FOR HYDROGRAPHS')
C
C6------RETURN
      RETURN
      END
C========================================================================
      SUBROUTINE GWF1HYD1RPP(HYDM,STRT,NHYDM,NUMH,DELR,DELC,
     1NCOL,NROW,NLAY,LCHNEW,LCIBOU,IN,IOUT)
C
C
C-----VERSION SEPT142000 GWF1HYD1RPP
C     ******************************************************************
C     READ BASIC PACKAGE DATA FOR HYDROGRAPHS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'hydmod.inc'
      CHARACTER HYDLBL*20,LINE*80
      CHARACTER PCKG*3,ARR*2,INTYP*1
      DIMENSION HYDM(18,NHYDM),DELR(NCOL),DELC(NROW)
      DIMENSION STRT(NCOL,NROW,NLAY)
      LOGICAL IBCHK
C     ------------------------------------------------------------------
C
      NIJ=NROW*NCOL
      NCOL1=NCOL-1
      NUMHP=0
      NUMH=0
      REWIND(IN)
      READ(IN,'(A)',END=99) LINE
 10   READ(IN,'(A)',END=99) LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).NE.'BAS') GO TO 10
      PCKG=LINE(ISTART:ISTOP)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      ARR=LINE(ISTART:ISTOP)
      WRITE(HYDLBL(1:2),FMT='(A2)')ARR
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      INTYP=LINE(ISTART:ISTOP)
      WRITE(HYDLBL(3:3),FMT='(A1)')LINE(ISTART:ISTOP)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,KLAY,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,XL,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,YL,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
      WRITE(HYDLBL(4:6),FMT='(I3.3)')KLAY
      HYDLBL(7:20)=LINE(ISTART:ISTART+14)
cc    TIME SERIES FROM THE HEAD ARRAY
      IF (ARR.EQ.'HD') THEN
         LOC=LCHNEW
         NW=2
         ITYP=1
         IBCHK=.TRUE.
cc    TIME SERIES FOR THE DRAWDOWN ARRAY
      ELSE IF (ARR.EQ.'DD') THEN
         LOC=LCHNEW
         NW=2
         ITYP=2
         IBCHK=.TRUE.
      ELSE
         WRITE(IOUT,25) LINE
 25      FORMAT(' Invalid array type was found on the following',
     &   ' record:',/,A80)
         WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
         GO TO 10
      ENDIF
      CALL SGWF1HYD1GRDLOC(XL,YL,DELR,DELC,NROW,NCOL,NR1,NC1,NR2,NC2,
     & X1,X2,Y1,Y2)
      IF(INTYP.EQ.'C') THEN
         NWT=1
         IF(NR1.LT.1.OR.NR1.GT.NROW.OR.NC1.LT.1.OR.NC1.GT.NCOL) THEN
            WRITE(IOUT,26) LINE
 26         FORMAT(' Coordinates of the following record are ',
     &           'outside of the model grid:',/,A80)
            WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
            GO TO 10
         ENDIF
         LOC1=LOC+((KLAY-1)*NIJ+(NR1-1)*NCOL+NC1-1)
         LOC2=LOC1
         LOC3=LOC1
         LOC4=LOC1
         W1=1.
         W2=0.
         W3=0.
         W4=0.
         IF(IBCHK) THEN
            IBLOC1=LCIBOU+(KLAY-1)*NIJ+(NR1-1)*NCOL+NC1-1
            IBLOC2=IBLOC1
            IBLOC3=IBLOC1
            IBLOC4=IBLOC1
         ELSE
            IBLOC1=0
            IBLOC2=0
            IBLOC3=0
            IBLOC4=0
         ENDIF
      ELSE IF(INTYP.EQ.'I') THEN
         NWT=4
         IF(NR2.LT.2.OR.NR2.GT.NROW.OR.NC2.LT.1.OR.NC2.GT.NCOL1) THEN
            WRITE(IOUT,26) LINE
            GO TO 10
         ENDIF
         LOC1=LOC+((KLAY-1)*NIJ+(NR2-1)*NCOL+NC2-1)
         LOC2=LOC1+1
         LOC3=LOC2-NCOL
         LOC4=LOC3-1
         CALL SGWF1HYD1MW(XL,YL,X1,X2,Y1,Y2,W1,W2,W3,W4)
         IF(IBCHK) THEN
            IBLOC1=LCIBOU+(KLAY-1)*NIJ+(NR2-1)*NCOL+NC2-1
            IBLOC2=IBLOC1+1
            IBLOC3=IBLOC2-NCOL
            IBLOC4=IBLOC1-NCOL
         ELSE
            IBLOC1=0
            IBLOC2=0
            IBLOC3=0
            IBLOC4=0
         ENDIF
      ELSE
         WRITE(IOUT,27) LINE
 27      FORMAT(' Invalid interpolation type was found on the ',
     &   'following record:',/,A80)
         WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
         GO TO 10
      ENDIF
      NUMH=NUMH+1
      IF(NUMH.GT.NHYDM) THEN
         WRITE(IOUT,*)
     1   ' NHYDM is not large enough in HYDROGRAPH Option'
         CALL USTOP(' ')
      END IF
      HYDM(1,NUMH)=LOC1
      HYDM(2,NUMH)=LOC2
      HYDM(3,NUMH)=LOC3
      HYDM(4,NUMH)=LOC4
      HYDM(5,NUMH)=W1
      HYDM(6,NUMH)=W2
      HYDM(7,NUMH)=W3
      HYDM(8,NUMH)=W4
      HYDM(10,NUMH)=NW
      HYDM(12,NUMH)=IBLOC1
      HYDM(13,NUMH)=IBLOC2
      HYDM(14,NUMH)=IBLOC3
      HYDM(15,NUMH)=IBLOC4
      HYDM(16,NUMH)=KLAY
      HYDM(17,NUMH)=ITYP
      HYDM(18,NUMH)=NWT
      PTSLBL(NUMH)=HYDLBL
      IF(ARR.EQ.'DD') THEN
         IF(INTYP.EQ.'I')THEN
            H1=STRT(NC2,NR2,KLAY)
            H2=STRT(NC2+1,NR2,KLAY)
            H3=STRT(NC2+1,NR2-1,KLAY)
            H4=STRT(NC2,NR2-1,KLAY)
            HYDM(11,NUMH)=H1*W1+H2*W2+H3*W3+H4*W4
         ELSEIF(INTYP.EQ.'C')THEN
            HYDM(11,NUMH)=STRT(NC1,NR1,KLAY)
         ENDIF
      ENDIF
      GO TO 10
 99   NNEW=NUMH-NUMHP
      IF(NNEW.GT.0)WRITE(IOUT,28) NNEW
 28   FORMAT(' A total of ',I3,' points have been added ',
     & 'for the hydrographs of BAS arrays.')
CC
      RETURN
      END
C========================================================================
      SUBROUTINE GWF1HYD1IBS2RPP(HYDM,NHYDM,NUMH,DELR,DELC,
     1NCOL,NROW,NLAY,LCIBOU,LCSUB,LCHC,IN,IOUT)
C
C
C-----VERSION 14SEPT2000 GWF1HYD1IBS2RPP
C     ******************************************************************
C     READ SUBSIDENCE PACKAGE DATA FOR HYDROGRAPHS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'hydmod.inc'
      CHARACTER HYDLBL*20,LINE*80
      CHARACTER PCKG*3,ARR*2,INTYP*1
      DIMENSION HYDM(18,NHYDM),DELR(NCOL),DELC(NROW)
      LOGICAL IBCHK
      COMMON /IBSCOM/ IBQ(999)
C     ------------------------------------------------------------------
      NIJ=NROW*NCOL
      NCOL1=NCOL-1
      NUMHP=NUMH
      REWIND(IN)
      READ(IN,'(A)',END=99) LINE
 10   READ(IN,'(A)',END=99) LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).NE.'IBS') GO TO 10
      PCKG=LINE(ISTART:ISTOP)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      ARR=LINE(ISTART:ISTOP)
      WRITE(HYDLBL(1:2),FMT='(A2)')ARR
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      INTYP=LINE(ISTART:ISTOP)
      WRITE(HYDLBL(3:3),FMT='(A1)')INTYP
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,KLAY,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,XL,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,YL,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
      WRITE(HYDLBL(4:6),FMT='(I3.3)')KLAY
      HYDLBL(7:20)=LINE(ISTART:ISTART+14)
cc    TIME SERIES FROM THE CRITICAL HEAD ARRAY
      IF (ARR.EQ.'HC') THEN
         LOC=LCHC
         NW=1
         ITYP=1
         IBCHK=.TRUE.
cc    TIME SERIES FROM THE COMPACTION ARRAY
      ELSE IF (ARR.EQ.'CP') THEN
         LOC=LCSUB
         NW=1
         ITYP=1
         IBCHK=.TRUE.
cc    TIME SERIES FROM THE SUBSIDENCE ARRAY
      ELSE IF (ARR.EQ.'SB') THEN
         LOC=LCSUB
         NW=1
         ITYP=3
         NQ=0
         IBCHK=.FALSE.
C ----- SET THE CORRECT LAYER COUNTER FOR SUBSIDENCE ARRAYS
        DO 20 K=1,KLAY
        IF(IBQ(K).GT.0) NQ=NQ+1
 20     CONTINUE
        KLAY=NQ
      ELSE
         WRITE(IOUT,25) LINE
 25      FORMAT(' Invalid array type was found on the following',
     &   ' record:',/,A80)
         WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
         GO TO 10
      ENDIF
      CALL SGWF1HYD1GRDLOC(XL,YL,DELR,DELC,NROW,NCOL,NR1,NC1,NR2,NC2,
     & X1,X2,Y1,Y2)
      IF(INTYP.EQ.'C') THEN
         NWT=1
         IF(NR1.LT.1.OR.NR1.GT.NROW.OR.NC1.LT.1.OR.NC1.GT.NCOL) THEN
            WRITE(IOUT,26) LINE
 26         FORMAT(' Coordinates of the following record are ',
     &           'outside of the model grid:',/,A80)
            WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
            GO TO 10
         ENDIF
         LOC1=LOC+((KLAY-1)*NIJ+(NR1-1)*NCOL+NC1-1)
         LOC2=LOC1
         LOC3=LOC1
         LOC4=LOC1
         W1=1.
         W2=0.
         W3=0.
         W4=0.
         IF(IBCHK) THEN
            IBLOC1=LCIBOU+(KLAY-1)*NIJ+(NR1-1)*NCOL+NC1-1
            IBLOC2=IBLOC1
            IBLOC3=IBLOC1
            IBLOC4=IBLOC1
         ELSE
            IBLOC1=0
            IBLOC2=0
            IBLOC3=0
            IBLOC4=0
         ENDIF
      ELSE IF(INTYP.EQ.'I') THEN
         NWT=4
         IF(NR2.LT.2.OR.NR2.GT.NROW.OR.NC2.LT.1.OR.NC2.GT.NCOL1) THEN
            WRITE(IOUT,26) LINE
            GO TO 10
         ENDIF
         LOC1=LOC+((KLAY-1)*NIJ+(NR2-1)*NCOL+NC2-1)
         LOC2=LOC1+1
         LOC3=LOC2-NCOL
         LOC4=LOC3-1
         CALL SGWF1HYD1MW(XL,YL,X1,X2,Y1,Y2,W1,W2,W3,W4)
         IF(IBCHK) THEN
            IBLOC1=LCIBOU+(KLAY-1)*NIJ+(NR1-1)*NCOL+NC1-1
            IBLOC2=IBLOC1+1
            IBLOC3=IBLOC2-NCOL
            IBLOC4=IBLOC1-NCOL
         ELSE
            IBLOC1=0
            IBLOC2=0
            IBLOC3=0
            IBLOC4=0
         ENDIF
      ELSE
         WRITE(IOUT,27) LINE
 27      FORMAT(' Invalid interpolation type was found on the ',
     &   'following record:',/,A80)
         WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
         GO TO 10
      ENDIF
      NUMH=NUMH+1
      IF(NUMH.GT.NHYDM) THEN
         WRITE(IOUT,*)
     1   ' NHYDM is not large enough in HYDROGRAPH Option'
         CALL USTOP(' ')
      END IF
      HYDM(1,NUMH)=LOC1
      HYDM(2,NUMH)=LOC2
      HYDM(3,NUMH)=LOC3
      HYDM(4,NUMH)=LOC4
      HYDM(5,NUMH)=W1
      HYDM(6,NUMH)=W2
      HYDM(7,NUMH)=W3
      HYDM(8,NUMH)=W4
      HYDM(10,NUMH)=NW
      HYDM(12,NUMH)=IBLOC1
      HYDM(13,NUMH)=IBLOC2
      HYDM(14,NUMH)=IBLOC3
      HYDM(15,NUMH)=IBLOC4
      HYDM(16,NUMH)=KLAY
      HYDM(17,NUMH)=ITYP
      HYDM(18,NUMH)=NWT
      PTSLBL(NUMH)=HYDLBL
      GO TO 10
 99   NNEW=NUMH-NUMHP
      IF(NNEW.GT.0)WRITE(IOUT,28) NNEW
 28   FORMAT(' A total of ',I3,' points have been added ',
     & 'for the hydrographs of IBS arrays.')
      RETURN
      END
CC

C========================================================================
      SUBROUTINE GWF1HYD1STR6RPS(ISTRM,HYDM,NHYDM,NUMH,DELR,DELC,
     1NCOL,NROW,NLAY,LCIBOU,LCSTRM,NSTREM,IN,IOUT,MXSTRM)
C
C
C-----VERSION 14SEPT2000 GWF1HYD1STR6RPS
C     ******************************************************************
C     READ STREAM PACKAGE DATA FOR HYDROGRAPHS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'hydmod.inc'
      CHARACTER HYDLBL*20,LINE*80
      CHARACTER PCKG*3,ARR*2,INTYP*1
      DIMENSION ISTRM(5,MXSTRM),HYDM(18,NHYDM),DELR(NCOL),DELC(NROW)
      LOGICAL IBCHK
C     ------------------------------------------------------------------
      NIJ=NROW*NCOL
      NUMHP=NUMH
      REWIND(IN)
      READ(IN,'(A)',END=99) LINE
 10   READ(IN,'(A)',END=99) LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).NE.'STR') GO TO 10
      PCKG=LINE(ISTART:ISTOP)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      ARR=LINE(ISTART:ISTOP)
      WRITE(HYDLBL(1:2),FMT='(A2)')ARR
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      INTYP=LINE(ISTART:ISTOP)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,KLAY,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,XL,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,YL,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
      WRITE(HYDLBL(3:5),FMT='(I3.3)')INT(XL)
      WRITE(HYDLBL(6:8),FMT='(I3.3)')INT(YL)
      HYDLBL(9:20)=LINE(ISTART:ISTART+12)
cc    XL contains the SEGMENT number and YL contains the REACH number. 
cc    ARR/IRR designate the streamflow option with 
cc    ST==>IRR=6 (stream stage), SO==>IRR=7 (out of reach),
cc    SI==>IRR=8 (into reach), and SA==>IRR=9(into aquifer)
cc    TIME SERIES FROM THE STREAM STAGE OF RIVER IN CELL
      IF (ARR.EQ.'ST') THEN
         LOC=LCSTRM
         NW=1
         ITYP=1
         IRR=6
         IBCHK=.FALSE.
cc    TIME SERIES FROM THE STREAMFLOW OUT OF CELL
      ELSE IF (ARR.EQ.'SO') THEN
         LOC=LCSTRM
         NW=1
         ITYP=1
         IRR=7
         IBCHK=.FALSE.
cc    TIME SERIES FROM THE STREAMFLOW INTO CELL
      ELSE IF (ARR.EQ.'SI') THEN
         LOC=LCSTRM
         NW=1
         ITYP=1
         IRR=8
         IBCHK=.FALSE.
cc    TIME SERIES FROM THE INTO OR OUT OF AQUIFER
      ELSE IF (ARR.EQ.'SA') THEN
         LOC=LCSTRM
         NW=1
         ITYP=1
         IRR=9
         IBCHK=.TRUE.
      ELSE IF(NSTREM.LT.1)THEN
      WRITE(IOUT,24) LINE
 24      FORMAT(' No Active Streams in this Model:',
     &   /,A80)
         WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
         GO TO 10
      ELSE
         WRITE(IOUT,25) LINE
 25      FORMAT(' Invalid streamflow feature was found on the following'
     &   ,' record:',/,A80)
         WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
         GO TO 10
      ENDIF
C
       K=1
 26    ISEG=ISTRM(4,K)
       IRCH=ISTRM(5,K)
       IF(ISEG.EQ.INT(XL).AND.IRCH.EQ.INT(YL))THEN
        NL1=ISTRM(1,K)
        NR1=ISTRM(2,K)
        NC1=ISTRM(3,K)
        NSTR=K
       ELSE IF(K.LT.NSTREM)THEN
        K=K+1
        GOTO 26
       ELSE
        WRITE(IOUT,*)
     1      ' Hydrograph specified for non-existent strem reach'
        WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
        GO TO 10
       ENDIF
      IF(INTYP.EQ.'C') THEN
         NWT=1
         IF(NR1.LT.1.OR.NR1.GT.NROW.OR.NC1.LT.1.OR.NC1.GT.NCOL .OR.
     1             NL1.LT.1.OR.NL1.GT.NLAY ) THEN
            WRITE(IOUT,27) LINE
 27         FORMAT(' Coordinates of the following record are ',
     &           'outside of the model grid:',/,A80)
            WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
            GO TO 10
         ENDIF
         IF(NL1.NE.KLAY) THEN
            WRITE(IOUT,28) LINE
 28         FORMAT(' Layer for the following record aredoes not ',
     &           'match the layer for the stream reach:',/,A80)
            WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
            GO TO 10
         ENDIF
         LOC1=LOC+((NSTR-1)*11)+IRR+1
         LOC2=LOC1
         LOC3=LOC1
         LOC4=LOC1
         W1=1.
         W2=0.
         W3=0.
         W4=0.
         IF(IBCHK) THEN
            IBLOC1=LCIBOU+(NL1-1)*NIJ+(NR1-1)*NCOL+NC1-1
            IBLOC2=IBLOC1
            IBLOC3=IBLOC1
            IBLOC4=IBLOC1
         ELSE
            IBLOC1=0
            IBLOC2=0
            IBLOC3=0
            IBLOC4=0
         ENDIF
      ELSE
         WRITE(IOUT,29) LINE
 29      FORMAT(' Invalid interpolation type was found on the ',
     &   'following record:',/,A80)
         WRITE(IOUT,*) 'Hydrograph Record will be ignored.'
         GO TO 10
      ENDIF
      NUMH=NUMH+1
      IF(NUMH.GT.NHYDM) THEN
         WRITE(IOUT,*)
     1   ' NHYDM is not large enough in HYDROGRAPH Option'
         CALL USTOP(' ')
      END IF
      HYDM(1,NUMH)=LOC1
      HYDM(2,NUMH)=LOC2
      HYDM(3,NUMH)=LOC3
      HYDM(4,NUMH)=LOC4
      HYDM(5,NUMH)=W1
      HYDM(6,NUMH)=W2
      HYDM(7,NUMH)=W3
      HYDM(8,NUMH)=W4
      HYDM(10,NUMH)=NW
      HYDM(12,NUMH)=IBLOC1
      HYDM(13,NUMH)=IBLOC2
      HYDM(14,NUMH)=IBLOC3
      HYDM(15,NUMH)=IBLOC4
      HYDM(16,NUMH)=NL1
      HYDM(17,NUMH)=ITYP
      HYDM(18,NUMH)=NWT
      PTSLBL(NUMH)=HYDLBL
      GO TO 10
 99   NNEW=NUMH-NUMHP
      IF(NNEW.GT.0)WRITE(IOUT,30) NNEW
 30   FORMAT(' A total of ',I3,' points have been added ',
     & 'for the hydrographs of STR arrays.')
CC
      RETURN
      END

C========================================================================
      SUBROUTINE GWF1HYD1OT(GZ,LENGZ,RX,LENRX,IG,LENIG,HYDM,NUMH,
     1            IHYDMUN,TOTIM,HYDNOH,NROW,NCOL,ITMUNI,IOUT)
C
C
C-----VERSION 14SEPT2000 GWF1HYD1OT
C     ******************************************************************
C     WRITE HYDROGRAPH RECORDS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'hydmod.inc'
      CHARACTER TIMLBL*4
      LOGICAL FIRST
      SAVE FIRST
      DOUBLE PRECISION GZ
      DIMENSION GZ(LENGZ),RX(LENRX),IG(LENIG),HYDM(18,NUMH)
      DATA FIRST/.TRUE./
C     ------------------------------------------------------------------
C
C -----If this is the first time into the output routine, write header
C -----records
C ------WRITE HYDROGRAPH HEADER RECORD
      IF(FIRST) THEN
      TIMLBL='TIME'
      FIRST=.FALSE.
C
         IF(NUMH.LE.0) THEN
            WRITE(IOUT,120)
 120        FORMAT(1X,'HYDROGRAPH OPTION CANCELLED BECAUSE NO VALID ',
     1     'HYDROGRAPH POINTS EXIST.')
            RETURN
         ENDIF
C
         WRITE(IHYDMUN) NUMH,ITMUNI
         WRITE(IOUT,130) NUMH
 130     FORMAT(1X,'A TOTAL OF ',I3,' HYDROGRAPH POINTS HAVE BEEN ',
     1        'PREPARED.')
C ------WRITE HYDROGRAPH LABEL HEADER RECORD
         WRITE(IHYDMUN) TIMLBL,(PTSLBL(N),N=1,NUMH)
      ENDIF
C
C -----Calculate the number of cells in one layer of the grid.
      NIJ=NROW*NCOL
      IF(NUMH.LE.0)RETURN
C
C -----Calculate value for each hydrograph point.
      DO 50 N=1,NUMH
      NN=N
C -----Determine interpolation type, word length, number of weights,
C -----and locations of IBOUND codes at or around hydrograph point.
      ITYP=HYDM(17,N)
      NW = HYDM(10,N)
      NWT= HYDM(18,N)
      IBLOC1=HYDM(12,N)
      IBLOC2=HYDM(13,N)
      IBLOC3=HYDM(14,N)
      IBLOC4=HYDM(15,N)
C -----If IBOUND is to be checked for inactive cells, retrieve values
C -----at or around hydrograph point.
      IF(IBLOC1.GT.0) THEN
         IB1=IG(IBLOC1)
         IBFACT=IB1
         IF(NWT.EQ.4) THEN
            IB2=IG(IBLOC2)
            IB3=IG(IBLOC3)
            IB4=IG(IBLOC4)
            IBFACT=IB1*IB2*IB3*IB4
         ENDIF
      ENDIF
C -----Compute hydrograph value by interpolation of one or more
C -----values in the X array.
      IF(ITYP.EQ.1) THEN
         IF(IBLOC1.NE.0.AND.IBFACT.EQ.0) THEN
            HYDM(9,N)=HYDNOH
         ELSE
            HYDM(9,N)=SHYD1WTAVG(GZ,RX,HYDM,LENGZ,LENRX,NUMH,NN,NW,
     1               NWT,0)
         ENDIF
C -----Compute hydrograph value as the difference between a constant and
C -----a value interpolated from one or more values in the X array.
      ELSE IF(ITYP.EQ.2) THEN
         IF(IBLOC1.NE.0.AND.IBFACT.EQ.0) THEN
            HYDM(9,N)=HYDNOH
         ELSE
            HYDM(9,N)=HYDM(11,N)-
     1            SHYD1WTAVG(GZ,RX,HYDM,LENGZ,LENRX,NUMH,NN,NW,NWT,0)
         ENDIF
C -----Compute hydrograph value as the sum of interpolated values in
C -----arrays from one or more contiguous model layers.
      ELSE IF(ITYP.EQ.3) THEN
         KLAY=HYDM(16,N)
         TOTL=0.0
         DO 9 K=1,KLAY
         NOFF=-NIJ*(KLAY-K)
         TOTL=TOTL+SHYD1WTAVG(GZ,RX,HYDM,LENGZ,LENRX,NUMH,NN,NW,NWT,
     1                NOFF)
   9    CONTINUE
        HYDM(9,N)=TOTL
      ENDIF
   50 CONTINUE
C
C ------WRITE HYDROGRAPH RECORD
      WRITE(IHYDMUN) TOTIM,(HYDM(9,N),N=1,NUMH)
C
C ------ RETURN
      RETURN
      END
C========================================================================
      SUBROUTINE SGWF1HYD1GRDLOC(XL,YL,DELR,DELC,NROW,NCOL,NR1,NC1,NR2,
     & NC2,XX1,XX2,YY1,YY2)
C
C
C-----VERSION 14SEPT2000 SGWF1HYD1GRDLOC
C     ******************************************************************
C     LOCATE CELLS FOR HYDROGRAPH POINTS
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      DIMENSION DELR(NCOL),DELC(NROW)
C     ------------------------------------------------------------------
      XX1=0.
      XX2=0.
      YY1=0.
      YY2=0.
      X1=0.
      IF(XL.LT.X1) THEN
         NC1=0
         NC2=0
         GO TO 100
      ENDIF
      XCB=0.
      XCF=DELR(1)*0.5
      DO 10 N=1,NCOL
      X2=X1+DELR(N)
      XC=XCF
      DXF=0.
      IF(N.LT.NCOL) DXF=DELR(N+1)*0.5
      XCF=X2+DXF
      IF(XL.LE.X2) THEN
         NC1=N
         IF(XL.LT.XC) THEN
            NC2=N-1
            XX1=XCB
            XX2=XC
         ELSE
            NC2=N
            XX1=XC
            XX2=XCF
         ENDIF
         GO TO 100
      ENDIF
      X1=X2
      XCB=XC
 10   CONTINUE
      NC1=NCOL+1
      NC2=NCOL+1
 100  Y1=0.
      YCB=0.
      YCF=DELC(NROW)*0.5
      IF(YL.LT.Y1) THEN
         NR1=NROW+1
         NR2=NROW+1
         RETURN
      ENDIF
      DO 110 N=NROW,1,-1
      Y2=Y1+DELC(N)
      YC=YCF
      DYF=0.
      IF(N.GT.1) DYF=DELC(N-1)*0.5
      YCF=Y2+DYF
      IF(YL.LE.Y2) THEN
         NR1=N
         IF(YL.LT.YC) THEN
            NR2=N+1
            YY1=YCB
            YY2=YC
         ELSE
            NR2=N
            YY1=YC
            YY2=YCF
         ENDIF
         RETURN
      ENDIF
      Y1=Y2
      YCB=YC
 110  CONTINUE
      NC1=0
      NC2=0
      RETURN
      END
C========================================================================
      FUNCTION SHYD1WTAVG(GZ,RX,HYDM,LENGZ,LENRX,NUMH,N,NW,NWT,NOFF)
C
C
C-----VERSION 14SEPT2000 SHYD1WTAVG
C     ******************************************************************
C     COMPUTE WEIGHTED AVERAGE
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      DOUBLE PRECISION GZ
      DIMENSION GZ(LENGZ),RX(LENRX),HYDM(18,NUMH)
C     ------------------------------------------------------------------

      SHYD1WTAVG=0.0
      DO 10 M=1,NWT
      LOC=HYDM(M,N)+NOFF
      IF(NW.EQ.2) THEN
         ZZ=GZ(LOC)
      ELSE
         ZZ=RX(LOC)
      ENDIF
      SHYD1WTAVG=SHYD1WTAVG+ZZ*HYDM(4+M,N)
 10   CONTINUE
      RETURN
      END
C========================================================================
      SUBROUTINE SGWF1HYD1MW(X0,Y0,X1,X2,Y1,Y2,W1,W2,W3,W4)
C
C-----VERSION 14SEPT2000 SGWF1HYD1MW
C     ******************************************************************
C     COMPUTE WEIGHTS FOR BILINEAR INTERPOLATION
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
C
      DX=(X0-X1)/(X2-X1)
      DY=(Y0-Y1)/(Y2-Y1)
      DXY=DX*DY
      W1=1-DX-DY+DXY
      W2=DX-DXY
      W3=DXY
      W4=DY-DXY
      RETURN
      END
C*******************************************************************************
