C     Last change:  ERB  30 Sep 2002    2:18 pm
      SUBROUTINE UPARARRAL(IN,IOUT,LINE,NP)
C
C-----VERSION 15SEPT1998 UPARARRAL
C     ******************************************************************
C     Setup array parameter definition for a package.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
      CHARACTER*(*) LINE
C     ------------------------------------------------------------------
C
C  If NP has not already been defined, decode PARAMETER definitions if
C  they exist
      IF(IN.GT.0) THEN
         NP=0
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
         IF(LINE(ISTART:ISTOP).EQ.'PARAMETER') THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NP,R,IOUT,IN)
            READ(IN,'(A)') LINE
         END IF
      END IF
C
C  Process the parameter information
      IF(NP.GT.0) THEN
         WRITE(IOUT,31) NP
   31    FORMAT(1X,I5,' Named Parameters     ')
      ELSE
         NP=0
         WRITE(IOUT,'(A)') ' No named parameters'
      END IF
C
      RETURN
      END
      SUBROUTINE UPARARRRP(IN,IOUT,NP,ILFLG,PTYP,ITERP,ITVP,INAMLOC)
C
C-----VERSION 20NOV2001 UPARARRRP
C     ******************************************************************
C     Read and store array parameter definition information
C     ******************************************************************
C     Modified 11/20/2001 to support parameter instances - ERB
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
      CHARACTER*(*) PTYP
      CHARACTER*200 LINE
      CHARACTER*10 PN,CTMP1,CTMP2
C     ------------------------------------------------------------------
C
C     Read a parameter definition line and decode the parameter name,
C     type, and value
      READ(IN,'(A)') LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
      PN=LINE(ISTART:ISTOP)
      CTMP1=PN
      CALL UPCASE(CTMP1)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      PTYP=LINE(ISTART:ISTOP)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,PV,IOUT,IN)
C
C     Look for the parameter name in the parameter list
      DO 10 NP=1,MXPAR
        CTMP2=PARNAM(NP)
        CALL UPCASE(CTMP2)
        IF(CTMP1.EQ.CTMP2) THEN
C
C         If found, determine if it is an illegal duplicate or if it was
C         predefined.
          IF(PARTYP(NP).NE.' ' .AND. IDEFPAR.EQ.0) THEN
C           Illegal duplicate
            WRITE(IOUT,110) CTMP1
  110       FORMAT(' Duplicate parameter name: ',A)
            CALL USTOP(' ')
          END IF
C         Parameter was predefined -- leave its value alone
C         (i.e. ignore PV).
          GO TO 100
        ELSE IF(PARNAM(NP).EQ.' ') THEN
C         Parameter was not found in the list, so it is a new
C         definition. Put values in the list.
          PARNAM(NP)=PN
          B(NP)=PV
          IPSUM=IPSUM+1
          GO TO 100
        END IF
10    CONTINUE
C     Too many parameters
      WRITE(IOUT,11)
   11 FORMAT(1X,'The number of parameters has exceeded the maximum')
      CALL USTOP(' ')
C
C     Parse the rest of the parameter definition.
  100 PARTYP(NP)=PTYP
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NCLU,R,IOUT,IN)
C
      IF(ITERP.EQ.1) THEN
        NUMINST=0
        IF (ITVP.GT.0) THEN
C         CHECK FOR MULTIPLE INSTANCES
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
          IF (LINE(ISTART:ISTOP).EQ.'INSTANCES') THEN
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NUMINST,R,IOUT,IN)
            IF (NUMINST.LT.1) THEN
              WRITE(IOUT,12) PARNAM(NP),PTYP
   12         FORMAT(/,1X,'*** ERROR: NUMINST SPECIFIED LESS THAN 1',
     &            ' FOR PARAMETER "',A,'"',/,12X,'OF TYPE "',A,
     &            '" -- STOP EXECUTION (UPARARRRP)')
              CALL USTOP(' ')
            ENDIF
          ENDIF
        ENDIF
C
        IPLOC(1,NP)=ICLSUM+1
        NI=MAX(1,NUMINST)
        ICLSUM=ICLSUM+NCLU*NI
        IPLOC(2,NP)=ICLSUM
        IPLOC(3,NP)=NUMINST
        IPLOC(4,NP)=INAMLOC
        INAMLOC=INAMLOC+NUMINST
C
        IF(IPLOC(2,NP).GT.MXCLST) THEN
          WRITE(IOUT,117) IPLOC(2,NP),MXCLST
  117     FORMAT(1X,I5,
     1   ' CLUSTERS WERE SPECIFIED, BUT THERE IS SPACE FOR ONLY',I5)
          WRITE(IOUT,*) NP,NCLU
          WRITE(IOUT,'(A)') PARNAM(NP)
          WRITE(IOUT,'(4I10)') IPLOC
          CALL USTOP(' ')
        END IF
        WRITE(IOUT,121) PARNAM(NP),PARTYP(NP),NCLU
  121   FORMAT(1X/,1X,'PARAMETER NAME:',A,'   TYPE:',A,'   CLUSTERS:',
     &         I4)
        WRITE(IOUT,122) PV
  122   FORMAT(1X,'Parameter value from package file is: ',1PG13.5)
        IF(B(NP).NE.PV) THEN
          WRITE(IOUT,123) B(NP)
  123     FORMAT(1X,'This value has been changed to:',7X,1PG13.5,
     &        ', as read from',/,' the Sensitivity Process file')
        END IF
        IF(NUMINST.GT.0) THEN
          WRITE(IOUT,124)NUMINST
  124      FORMAT(3X,'NUMBER OF INSTANCES: ',I4)
          IF((INAMLOC-1).GT.MXINST) THEN
            WRITE(IOUT,125)INAMLOC-1,MXINST
  125       FORMAT(1X,'EXCEEDED THE MAXIMUM NUMBER OF INSTANCES:'/
     &       1X,I5,' instances have been specified'/
     &       1X,'The maximum number of instances is',I5)
            CALL USTOP(' ')
          ENDIF
        ENDIF
      ELSE
        NUMINST=IPLOC(3,NP)
      ENDIF
      IACTIVE(NP)=0
C
      IF(NUMINST.EQ.0) THEN
        IB=0
      ELSE
        IB=1
      ENDIF
      I=IPLOC(1,NP)-1
      DO 210 INST=IB,NUMINST
        IF(NUMINST.GT.0) CALL UINSRP(INST,IN,IOUT,NP,ITERP)
C       Read clusters
        DO 200 KK=1,NCLU
          I=I+1
          READ(IN,'(A)') LINE
          IF(ITERP.EQ.1) THEN
            LLOC=1
            IF(ILFLG.NE.0) THEN
C             Read layer number for cluster
              CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPCLST(1,I),R,IOUT,
     &                    IN)
            ELSE
              IPCLST(1,I)=0
            END IF
C           Read multiplier and zone array names
            CALL URWORD(LINE,LLOC,IM1,IM2,0,N,R,IOUT,IN)
            CALL URWORD(LINE,LLOC,IZ1,IZ2,0,N,R,IOUT,IN)
C           Read zone numbers
            DO 30 J=5,14
              CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPCLST(J,I),R,-1,IN)
              IF(IPCLST(J,I).EQ.0) THEN
                IPCLST(4,I)=J-1
                GO TO 32
              END IF
   30       CONTINUE
            IPCLST(4,I)=14
   32       CONTINUE
            IF(ILFLG.NE.0) THEN
              WRITE(IOUT,36) IPCLST(1,I),LINE(IM1:IM2),LINE(IZ1:IZ2)
   36         FORMAT(16X,'LAYER: ',I3,'    MULTIPLIER ARRAY: ',A,
     2        '    ZONE ARRAY: ',A)
            ELSE
              WRITE(IOUT,37) LINE(IM1:IM2),LINE(IZ1:IZ2)
   37         FORMAT(16X,'MULTIPLIER ARRAY: ',A,'    ZONE ARRAY: ',A)
            END IF
C
C           Find the multiplier array number
            CTMP1=LINE(IM1:IM2)
            CALL UPCASE(CTMP1)
            IF(CTMP1.EQ.'NONE') THEN
              IPCLST(2,I)=0
            ELSE
              DO 40 J=1,MXMLT
                CTMP2=MLTNAM(J)
                CALL UPCASE(CTMP2)
                IF(CTMP1.EQ.CTMP2) GO TO 45
   40         CONTINUE
              WRITE(IOUT,'(A)') ' Multiplier array has not been defined'
              CALL USTOP(' ')
   45         IPCLST(2,I)=J
            END IF
C
C           Find the zone array number
            CTMP1=LINE(IZ1:IZ2)
            CALL UPCASE(CTMP1)
            IF(CTMP1.EQ.'ALL') THEN
              IPCLST(3,I)=0
            ELSE
              IF(IPCLST(4,I).EQ.4) THEN
                WRITE(IOUT,47)
   47           FORMAT(1X,
     1          'There were no zone values specified in the cluster',/
     2          1X,'At least one zone must be specified')
                CALL USTOP(' ')
              END IF
              WRITE(IOUT,48) (IPCLST(J,I),J=5,IPCLST(4,I))
   48         FORMAT(1X,'               ZONE VALUES:',10I5)
              DO 50 J=1,MXZON
                CTMP2=ZONNAM(J)
                CALL UPCASE(CTMP2)
                IF(CTMP1.EQ.CTMP2) GO TO 55
   50         CONTINUE
              WRITE(IOUT,'(A)') ' Zone array has not been defined'
              CALL USTOP(' ')
   55         IPCLST(3,I)=J
            END IF
          ENDIF
C
  200   CONTINUE
  210 CONTINUE
C
      RETURN
      END
      SUBROUTINE UPARARRSUB1(ZZ,NCOL,NROW,ILAY,PTYP,IOUT,ANAME,IPF,
     1           RMLT,IZON,NMLTAR,NZONAR)
C
C-----VERSION 26FEB1998 UPARARRSUB1
C     ******************************************************************
C     Substitute parameter-based values into a 2-D array based on a
C     parameter type.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
      DIMENSION ZZ(NCOL,NROW),RMLT(NCOL,NROW,NMLTAR),
     1          IZON(NCOL,NROW,NZONAR)
      CHARACTER*(*) PTYP
      CHARACTER*24 ANAME
C     ------------------------------------------------------------------
      INIT=1
      WRITE(IOUT,11) ANAME
   11 FORMAT(1X,/,1X,A,' is defined by the following parameters:')
C
C  Loop through each parameter.
      DO 100 IP=1,MXPAR
C  Stop looping if the end of the parameter list is found.
      IF(PARNAM(IP).EQ.' ') GO TO 200
C  Check for the specified parameter type.
      IF(PARTYP(IP).EQ.PTYP) THEN
C  Loop through each cluster definition for layers that match the
C  specified layer.
         II=IP
         CALL USUB2D(ZZ,NCOL,NROW,II,ILAY,INIT,RMLT,IZON,NMLTAR,
     1           NZONAR,NSUB)
         INIT=0
         IF(NSUB.GT.0) WRITE(IOUT,47) PARNAM(IP)
   47    FORMAT(1X,A)
      END IF
  100 CONTINUE
C
C  PRINT THE ARRAY.
  200 CALL ULAPRWC(ZZ,NCOL,NROW,ILAY,IOUT,IPF,ANAME)
C
      RETURN
      END
      SUBROUTINE UPARARRSUB2(ZZ,NCOL,NROW,ILAY,NP,IN,IOUT,PTYP,ANAME,
     1      PACK,IPF,RMLT,IZON,NMLTAR,NZONAR)
C
C-----VERSION 21NOV2001 UPARARRSUB2
C     ******************************************************************
C     Read a series of parameter names and substitute their values into
C     a 2-D array.
C     ******************************************************************
C     Modified 11/21/2001 to support parameter instances - ERB
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
      DIMENSION ZZ(NCOL,NROW),RMLT(NCOL,NROW,NMLTAR),
     1          IZON(NCOL,NROW,NZONAR)
      CHARACTER*(*) PTYP,PACK
      CHARACTER*24 ANAME
      CHARACTER*200 LINE
      CHARACTER*10 CTMP1,CTMP2,CTMP3,CTMP4
C     ------------------------------------------------------------------
      INIT=1
C
C     Read each parameter name.
      DO 100 N=1,NP
        READ(IN,'(A)') LINE
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,IDUM,RDUM,IOUT,IN)
        WRITE(IOUT,5) LINE(ISTART:ISTOP)
    5   FORMAT(' Parameter:  ',A)
        IF(LINE(ISTART:ISTOP).EQ.' ') THEN
          WRITE(IOUT,*) ' Blank parameter name in the ',PACK,' file.'
          CALL USTOP(' ')
        END IF
C
C       Loop through each parameter looking for the specified name.
        CTMP1=LINE(ISTART:ISTOP)
        CALL UPCASE(CTMP1)
        DO 10 IP=1,MXPAR
          CTMP2=PARNAM(IP)
          CALL UPCASE(CTMP2)
          IF(CTMP1.EQ.CTMP2) GO TO 20
C         Stop looping if the end of the parameter list is found.
          IF(PARNAM(IP).EQ.' ') GO TO 15
   10   CONTINUE
   15   WRITE(IOUT,16) PACK
   16   FORMAT(1X,'Error in ',A,' file:',/
     1      1X,'The above parameter must be defined prior to its use')
        CALL USTOP(' ')
C
   20   CONTINUE
C
        IF(PARTYP(IP).NE.PTYP) THEN
C         Print an error message if the parameter type does not match.
          WRITE(IOUT,83) PARNAM(IP),PARTYP(IP),PACK,PTYP
   83     FORMAT(1X,'Parameter type conflict:',/
     1           1X,'Named parameter:',A,' was defined as type:',A,/
     2           1X,'However, this parameter is used in the ',A,
     3             ' file, so it should be type:',A)
          CALL USTOP(' ')
        ENDIF
C
        NUMINST=IPLOC(3,IP)
        ILOC=IPLOC(4,IP)
        NI=1
C       If parameter is time-varying, read instance name
        IF(NUMINST.GT.0) THEN
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,IDUM,RDUM,IOUT,IN)
          CTMP3=LINE(ISTART:ISTOP)
          IF(CTMP3.EQ.' ') THEN
            WRITE(IOUT,1000) PACK,PARNAM(IP)
 1000       FORMAT(/,1X,'Blank instance name in the ',A,
     &             ' file for parameter ',A)
            CALL USTOP(' ')
          ENDIF
          WRITE(IOUT,1010) CTMP3
 1010     FORMAT(3X,'Instance:  ',A)
          CALL UPCASE(CTMP3)
          DO 50 KI=1,NUMINST
            CTMP4=INAME(ILOC+KI-1)
            CALL UPCASE(CTMP4)
            IF(CTMP3.EQ.CTMP4) THEN
              NI=KI
              GOTO 55
            ENDIF
   50     CONTINUE
          WRITE(IOUT,1020) PACK,CTMP3,PARNAM(IP)
 1020     FORMAT(/,1X,'The ',A,' file specifies undefined instance "',
     &           A,'" for parameter ',A)
          CALL USTOP(' ')
   55     CONTINUE
        ENDIF
C
        IF (IACTIVE(IP).GT.0) THEN
          WRITE(IOUT,1030) PARNAM(IP)
 1030     FORMAT(/,1X,'*** ERROR: PARAMETER "',A,
     &        '" HAS ALREADY BEEN ACTIVATED THIS STRESS PERIOD',/,
     &        ' -- STOP EXECUTION (UPARARRSUB2)')
          CALL USTOP(' ')
        ENDIF
C
C       Activate the parameter and substitute.
        IACTIVE(IP)=NI
        II=IP
        CALL USUB2D(ZZ,NCOL,NROW,II,ILAY,INIT,RMLT,IZON,NMLTAR,
     1              NZONAR,NSUB)
        INIT=0
C
C       Get new value of print flag if it is there.
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,I,RDUM,-1,IN)
        IF(LINE(ISTART:ISTOP) .NE.'E' .AND.
     1     LINE(ISTART:ISTOP) .NE.' ') IPF=I
C
  100 CONTINUE
C
C     Check to see if entire array is a constant.
      TMP=ZZ(1,1)
      DO 300 I=1,NROW
        DO 290 J=1,NCOL
          IF(ZZ(J,I).NE.TMP) GO TO 400
  290   CONTINUE
  300 CONTINUE
      IF(ILAY.GT.0) WRITE(IOUT,302) ANAME,TMP,ILAY
  302 FORMAT(1X,A,' =',1P,G14.6,' FOR LAYER',I4)
      IF(ILAY.LE.0) WRITE(IOUT,303) ANAME,TMP
  303 FORMAT(1X,A,' =',1P,G14.6)
      RETURN
C
C     Print the array.
  400 IF(ILAY.GT.0) THEN
        WRITE(IOUT,494) ANAME,ILAY
  494   FORMAT(1X,//11X,A,' FOR LAYER',I4)
      ELSE IF(ILAY.EQ.0) THEN
        WRITE(IOUT,495) ANAME
  495   FORMAT(1X,//11X,A)
      ELSE
        WRITE(IOUT,496) ANAME
  496   FORMAT(1X,//11X,A,' FOR CROSS SECTION')
      END IF
      IF(IPF.GE.0) CALL ULAPRW(ZZ,ANAME,0,0,NCOL,NROW,0,IPF,IOUT)
      RETURN
      END
      SUBROUTINE UPARLSTAL(IN,IOUT,LINE,NP,MXL)
C
C-----VERSION 26FEB1998 UPARLSTAL
C     ******************************************************************
C     Setup list parameter definition for a package
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
      CHARACTER*(*) LINE
C     ------------------------------------------------------------------
C
C  Decode PARAMETER definitions if they exist
      NP=0
      MXL=0
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'PARAMETER') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NP,R,IOUT,IN)
         IF(NP.LT.0) NP=0
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXL,R,IOUT,IN)
         IF(MXL.LT.0) MXL=0
C
C
         WRITE(IOUT,31) NP,MXL
   31    FORMAT(1X,I5,' Named Parameters     ',I5,' List entries')
         READ(IN,'(A)') LINE
      ELSE
         WRITE(IOUT,'(A)') ' No named parameters'
      END IF
C
      RETURN
      END
      SUBROUTINE UPARLSTRP(LSTSUM,MXLST,IN,IOUT,NP,PACK,PTYPX,ITERP,
     &                     NUMINST,INAMLOC)
C
C-----VERSION 20011105 UPARLSTRP
C     ******************************************************************
C     Read and store list parameter definition information
C     ******************************************************************
C     Modified 11/5/2001 to support parameter instances - ERB
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
      CHARACTER*(*) PACK,PTYPX
      CHARACTER*4 PTYP
      CHARACTER*10 PN,CTMP1,CTMP2
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
C
C  Read the parameter name and definition
      READ(IN,'(A)') LINE
C
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
      PN=LINE(ISTART:ISTOP)
      CTMP1=PN
      CALL UPCASE(CTMP1)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      PTYP=LINE(ISTART:ISTOP)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,N,PV,IOUT,IN)
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NLST,R,IOUT,IN)
C     Check for multiple instances
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF (LINE(ISTART:ISTOP).EQ.'INSTANCES') THEN
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NUMINST,R,IOUT,IN)
        IF (NUMINST.LT.1) THEN
          WRITE(IOUT,12) PN,PTYP
   12     FORMAT(/,1X,'*** ERROR: NUMINST SPECIFIED LESS THAN 1',
     &        ' FOR PARAMETER "',A,'"',/,12X,'OF TYPE "',A,
     &        '" -- STOP EXECUTION (UPARLSTRP)')
          CALL USTOP(' ')
        ENDIF
      ELSE
        NUMINST = 0
      ENDIF
C
      DO 10 NP=1,MXPAR
        CTMP2=PARNAM(NP)
        CALL UPCASE(CTMP2)
        IF(CTMP1.EQ.CTMP2) THEN
C         If found, determine if it is an illegal duplicate or if it was
C         predefined in the SEN file.
          IF(PARTYP(NP).NE.' ' .AND. IDEFPAR.EQ.0) THEN
C           Illegal duplicate
            WRITE(IOUT,110) CTMP1
  110       FORMAT (' Duplicate parameter name: ',A)
            CALL USTOP(' ')
          END IF
C  Parameter was predefined -- leave its value alone (i.e. ignore PV).
          GO TO 100
        ELSE IF(PARNAM(NP).EQ.' ') THEN
C         Parameter was not found in the list, so it is a new definition.
          PARNAM(NP)=PN
          B(NP)=PV
          IPSUM=IPSUM+1
          GO TO 100
        ENDIF
10    CONTINUE
C  Too many parameters
      WRITE(IOUT,99)
  99  FORMAT(' Number of parameters exceeds MXPAR -- STOP EXECUTION')
      CALL USTOP(' ')
 100  CONTINUE
C
      IF(ITERP.EQ.1) THEN
C       PUT PARAMETER TYPE IN THE LIST
        PARTYP(NP)=PTYP
C
        IPLOC(1,NP)=LSTSUM
        NI=MAX(1,NUMINST)
        LSTSUM=LSTSUM+(NLST*NI)
        IPLOC(2,NP)=LSTSUM-1
        IPLOC(3,NP)=NUMINST
        IPLOC(4,NP)=INAMLOC
        INAMLOC=INAMLOC+NUMINST
C
C-----WRITE PARAMETER INFORMATION
        WRITE(IOUT,121) PARNAM(NP),PARTYP(NP)
  121   FORMAT(1X/,1X,'PARAMETER NAME:',A,'   TYPE:',A)
        WRITE(IOUT,122) PV
  122   FORMAT(1X,'Parameter value from package file is: ',1PG13.5)
        IF(B(NP).NE.PV) THEN
          WRITE(IOUT,123) B(NP)
  123     FORMAT(1X,'This value has been changed to:',7X,1PG13.5,
     &        ', as read from',/,' the Sensitivity Process file')
        END IF
        WRITE(IOUT,130) NLST
  130   FORMAT(  '   NUMBER OF ENTRIES: ',I6)
        IF(NUMINST.GT.0) THEN
          WRITE(IOUT,131)NUMINST
  131     FORMAT('   NUMBER OF INSTANCES: ',I4)
        ENDIF
C
C  Check for list too large
        IF((LSTSUM-1) .GT. MXLST) THEN
          WRITE(IOUT,134) LSTSUM-1,MXLST
  134     FORMAT(1X,'EXCEEDED THE MAXIMUM NUMBER OF LIST ENTRIES:'/
     1          1X,I5,' list entries have been specified'/
     2          1X,'The maximum number of list entries is',I5)
          CALL USTOP(' ')
        END IF
        IF((INAMLOC-1).GT.MXINST) THEN
          WRITE(IOUT,135)INAMLOC-1,MXINST
  135     FORMAT(1X,'EXCEEDED THE MAXIMUM NUMBER OF INSTANCES:'/
     1          1X,I5,' instances have been specified'/
     2          1X,'The maximum number of instances is',I5)
          CALL USTOP(' ')
        END IF
C
C  Check for correct parameter type
        IF(PARTYP(NP).NE.PTYPX) THEN
          WRITE(IOUT,137) PTYPX,PACK
  137     FORMAT(1X,'Parameter type must be:',A,' in the ',A,' Package')
          CALL USTOP(' ')
        END IF
C
C       Parameter definition must include at least one cell
        IF (NLST.LE.0) THEN
          WRITE(IOUT,140) PN
          CALL USTOP(' ')
        ENDIF
  140   FORMAT(' ERROR:  DEFINITION FOR PARAMETER "',A,'"',
     &      ' INCLUDES NO CELLS',/,'   -- STOP EXECUTION (UPARLSTRP)')
      ELSE
        LSTSUM=LSTSUM+IPLOC(2,NP)-IPLOC(1,NP)+1
        NUMINST=IPLOC(3,NP)
      ENDIF
C
      IACTIVE(NP)=0
C
      RETURN
      END
      SUBROUTINE UINSRP(I,IN,IOUT,IP,ITERP)
C-----VERSION 20011206 UINSRP
C     ******************************************************************
C     Read and store one instance name
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
      CHARACTER*10 CTMP1,CTMP2
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
 1000 FORMAT(A)
 1010 FORMAT(/,3X,'INSTANCE:  ',A)
 1020 FORMAT(/,1X,'*** ERROR: "',A,
     &'" IS A DUPLICATE INSTANCE NAME FOR THIS PARAMETER',/,
     &' -- STOP EXECUTION (UINSRP)')
C
      LLOC = 1
      IPL4 = IPLOC(4,IP)
      ILOC = IPL4+I-1
      READ(IN,1000)LINE
C     READ INSTANCE NAME
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,N,R,IOUT,IN)
      INAME(ILOC) = LINE(ISTART:ISTOP)
      CTMP1 = LINE(ISTART:ISTOP)
      CALL UPCASE(CTMP1)
      IF (ITERP.EQ.1) WRITE(IOUT,1010)INAME(ILOC)
      IF (I.GT.1) THEN
        DO 10 J=IPL4,IPL4+I-2
          CTMP2 = INAME(J)
          CALL UPCASE(CTMP2)
          IF (CTMP1.EQ.CTMP2) THEN
            WRITE(IOUT,1020)INAME(J)
            CALL USTOP(' ')
          ENDIF
   10   CONTINUE
      ENDIF
      RETURN
      END
      SUBROUTINE UPARLSTSUB(IN,PACK,IOUTU,PTYP,RLIST,LSTVL,LSTDIM,NREAD,
     1                MXLST,NTOT,IPVL1,IPVL2,LABEL,CAUX,NCAUX,NAUX)
C
C-----VERSION 20020412 UPARLSTSUB
C     ******************************************************************
C     Read a list parameter name, look it up in the list of parameters,
C     and substitute values into active part of package array.
C     ******************************************************************
C     Modified 11/6/2001 to support parameter instances - ERB
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
      CHARACTER*(*) PACK,PTYP
      DIMENSION RLIST(LSTVL,LSTDIM)
      CHARACTER*(*) LABEL
      CHARACTER*16 CAUX(NCAUX)
      CHARACTER*200 LINE
      CHARACTER*10 CTMP1,CTMP2,CTMP3,CTMP4
C     ------------------------------------------------------------------
C
      IOUT = ABS(IOUTU)
      READ(IN,'(A)') LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,IDUM,RDUM,IOUT,IN)
      WRITE(IOUT,1) LINE(ISTART:ISTOP)
    1 FORMAT(/,' Parameter:  ',A)
      IF(LINE(ISTART:ISTOP).EQ.' ') THEN
        WRITE(IOUT,*) ' Blank parameter name in the ',PACK,' file.'
        CALL USTOP(' ')
      END IF
C
      CTMP1=LINE(ISTART:ISTOP)
      CALL UPCASE(CTMP1)
      DO 100 IP=1,MXPAR
        CTMP2=PARNAM(IP)
        CALL UPCASE(CTMP2)
        IF(CTMP1.EQ.CTMP2) THEN
          IF(PARTYP(IP).NE.PTYP) THEN
            WRITE(IOUT,81) PARNAM(IP),PARTYP(IP),PACK,PTYP
   81       FORMAT(1X,'Parameter type conflict:',/
     1        1X,'Named parameter:',A,' was defined as type:',A,/
     2        1X,'However, this parameter is used in the ',A,
     3          ' file, so it should be type:',A)
            CALL USTOP(' ')
          END IF
C
C         DESIGNATE CELLS CORRESPONDING TO CORRECT PARAMETER INSTANCE
          NLST=IPLOC(2,IP)-IPLOC(1,IP)+1
          NUMINST=IPLOC(3,IP)
          ILOC=IPLOC(4,IP)
          NI=1
          IF(NUMINST.GT.0) THEN
            NLST=NLST/NUMINST
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,IDUM,RDUM,IOUT,IN)
            CTMP3=LINE(ISTART:ISTOP)
            IF(CTMP3.EQ.' ') THEN
              WRITE(IOUT,1000)PACK,PARNAM(IP)
 1000         FORMAT(/,1X,'Blank instance name in the ',A,
     &               ' file for parameter ',A)
              CALL USTOP(' ')
            ENDIF
            WRITE(IOUT,1010) CTMP3
 1010       FORMAT(3X,'Instance:  ',A)
            CALL UPCASE(CTMP3)
            DO 50 KI=1,NUMINST
              CTMP4=INAME(ILOC+KI-1)
              CALL UPCASE(CTMP4)
              IF(CTMP3.EQ.CTMP4) THEN
                NI=KI
                GOTO 55
              ENDIF
   50       CONTINUE
            WRITE(IOUT,1020) PACK,CTMP3,PARNAM(IP)
 1020       FORMAT(/,1X,'The ',A,' file specifies undefined instance "',
     &             A,'" for parameter ',A)
            CALL USTOP(' ')
   55       CONTINUE
          ENDIF
C
          IF (IACTIVE(IP).GT.0) THEN
            WRITE(IOUT,1030) PARNAM(IP)
 1030       FORMAT(/,1X,'*** ERROR: PARAMETER "',A,
     &          '" HAS ALREADY BEEN ACTIVATED THIS STRESS PERIOD',/,
     &          ' -- STOP EXECUTION (UPARLSTSUB)')
            CALL USTOP(' ')
          ENDIF
C
          IACTIVE(IP)=NI
C
          NTOT=NTOT+NLST
          IF(NTOT.GT.MXLST) THEN
            WRITE(IOUT,83) NTOT,MXLST
   83       FORMAT(1X,/1X,'THE NUMBER OF ACTIVE LIST ENTRIES (',I6,
     1       ')',/1X,'IS GREATER THAN THE MAXIMUM ALLOWED (',I6,')')
            CALL USTOP(' ')
          END IF
C
C  Write label for list values
          IF (IOUTU.GT.0) CALL ULSTLB(IOUT,LABEL,CAUX,NCAUX,NAUX)
C
C  Substitute values
          DO 90 I=1,NLST
            II=NTOT-NLST+I
            III=I-1+IPLOC(1,IP)+(NI-1)*NLST
            DO 85 J=1,NREAD
              RLIST(J,II)=RLIST(J,III)
   85       CONTINUE
            DO 86 IPVL=IPVL1,IPVL2
              RLIST(IPVL,II)=RLIST(IPVL,II)*B(IP)
   86       CONTINUE
            IL=RLIST(1,II)
            IR=RLIST(2,II)
            IC=RLIST(3,II)
            IF (IOUTU.GT.0) WRITE(IOUT,89) II,IL,IR,IC,
     &          (RLIST(JJ,II),JJ=4,NREAD)
   89       FORMAT(1X,I6,I7,I7,I7,14G16.4)
   90     CONTINUE
          RETURN
        END IF
  100 CONTINUE
C
      WRITE(IOUT,*) ' The ',PACK,
     1   ' file specifies an undefined parameter:',LINE(ISTART:ISTOP)
      CALL USTOP(' ')
C
C
      END
      SUBROUTINE PRESET(PTYP)
C
C-----VERSION 30MARCH1998
C     ******************************************************************
C     Clear time step flag for all parameters of the specified type
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
      CHARACTER*(*) PTYP
C     ------------------------------------------------------------------
C
      DO 10 I=1,IPSUM
      IF(PARTYP(I).EQ.PTYP) IACTIVE(I)=0
   10 CONTINUE
C
      RETURN
      END

      SUBROUTINE USUB2D(ZZ,NCOL,NROW,IP,ILAY,INIT,RMLT,IZON,NMLTAR,
     1             NZONAR,NSUB)
C
C-----VERSION 21NOV2001 USUB2D
C     ******************************************************************
C     Substitute values for a single parameter into a 2-D array.
C     ******************************************************************
C     Modified 11/21/2001 to support parameter instances - ERB
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
      DIMENSION ZZ(NCOL,NROW),RMLT(NCOL,NROW,NMLTAR),
     1          IZON(NCOL,NROW,NZONAR)
C     ------------------------------------------------------------------
      ZERO=0.0
C
C     Initialize the array.
      IF(INIT.NE.0) THEN
        DO 10 I=1,NROW
          DO 5 J=1,NCOL
            ZZ(J,I)=ZERO
    5     CONTINUE
   10   CONTINUE
      END IF
C
      ICSTART=IPLOC(1,IP)
      ICSTOP=IPLOC(2,IP)
      NUMINST=IPLOC(3,IP)
      IF(NUMINST.GT.1) THEN
C       Select correct instance
        NCLU=(ICSTOP-ICSTART+1)/NUMINST
        NI=IACTIVE(IP)
        ICSTART=ICSTART+(NI-1)*NCLU
        ICSTOP=ICSTART+NCLU-1
      ENDIF
C
C     Loop through each cluster definition for layers that match the
C     specified layer.
      NSUB=0
      DO 80 IC=ICSTART,ICSTOP
C       Check if the cluster layer matches the specified layer
        IF(IPCLST(1,IC).EQ.ILAY) THEN
C         The parameter layer matches the specified layer, so loop
C         through each value of the array.
          MLT=IPCLST(2,IC)
          AA=1.
          IZ=IPCLST(3,IC)
          IF(IZ.GT.0) THEN
            DO 50 I=1,NROW
              DO 40 J=1,NCOL
                DO 30 JJ=5,IPCLST(4,IC)
C                 If the value in the zone array is equal to the cluster
C                 zone value, modify the array value.
                  IF(IZON(J,I,IZ).EQ.IPCLST(JJ,IC)) THEN
                    IF(MLT.GT.0) AA=RMLT(J,I,MLT)
                    ZZ(J,I)=ZZ(J,I)+AA*B(IP)
                    NSUB=NSUB+1
                  END IF
   30           CONTINUE
   40         CONTINUE
   50       CONTINUE
          ELSE
            DO 70 I=1,NROW
              DO 60 J=1,NCOL
                IF(MLT.GT.0) AA=RMLT(J,I,MLT)
                ZZ(J,I)=ZZ(J,I)+AA*B(IP)
   60         CONTINUE
   70       CONTINUE
            NSUB=NSUB+NCOL*NROW
          END IF
        END IF
   80 CONTINUE
C
C
      RETURN
      END
      SUBROUTINE UPARLSTLOC(IN,PACK,IOUT,PTYP,IBEG,IEND,PV)
C
C-----VERSION 20011107 UPARLSTLOC
C     ******************************************************************
C     Read a list parameter name, look it up in the list of
C     parameters, and return its location.
C     ******************************************************************
C     Modified 11/7/2001 to support parameter instances - ERB
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
      CHARACTER*(*) PACK,PTYP
      CHARACTER*200 LINE
      CHARACTER*10 CTMP1,CTMP2,CTMP3,CTMP4
C     ------------------------------------------------------------------
  500 FORMAT(' Parameter:  ',A)
  510 FORMAT(1X,'Parameter type conflict:',/
     &       1X,'Named parameter:',A,' was defined as type:',A,/
     &       1X,'However, this parameter is used in the ',A,
     &          ' file, so it should be type:',A)
  520 FORMAT(/,1X,'Blank instance name in the ',A,
     &       ' file for parameter ',A)
  530 FORMAT(3X,'Instance:  ',A)
  540 FORMAT(/,1X,'The ',A,' file specifies undefined instance "',
     &       A,'" for parameter ',A)
  550 FORMAT(/,1X,'*** ERROR: PARAMETER "',A,
     &'" HAS ALREADY BEEN ACTIVATED THIS STRESS PERIOD',/,
     &' -- STOP EXECUTION (UPARLSTLOC)')
C
      READ(IN,'(A)') LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,IDUM,RDUM,IOUT,IN)
      WRITE(IOUT,500) LINE(ISTART:ISTOP)
      IF(LINE(ISTART:ISTOP).EQ.' ') THEN
        WRITE(IOUT,*) ' Blank parameter name in the ',PACK,' file.'
        CALL USTOP(' ')
      END IF
C
      CTMP1=LINE(ISTART:ISTOP)
      CALL UPCASE(CTMP1)
      DO 100 IP=1,MXPAR
        CTMP2=PARNAM(IP)
        CALL UPCASE(CTMP2)
        IF(CTMP1.EQ.CTMP2) THEN
          IF(PARTYP(IP).NE.PTYP) THEN
            WRITE(IOUT,510) PARNAM(IP),PARTYP(IP),PACK,PTYP
            CALL USTOP(' ')
          END IF
C
C         DESIGNATE CELLS CORRESPONDING TO CORRECT PARAMETER INSTANCE
          NLST=IPLOC(2,IP)-IPLOC(1,IP)+1
          NUMINST=IPLOC(3,IP)
          ILOC=IPLOC(4,IP)
          NI=1
          IF(NUMINST.GT.0) THEN
            NLST=NLST/NUMINST
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,IDUM,RDUM,IOUT,IN)
            CTMP3=LINE(ISTART:ISTOP)
            IF(CTMP3.EQ.' ') THEN
              WRITE(IOUT,520)PACK,PARNAM(IP)
              CALL USTOP(' ')
            ENDIF
            WRITE(IOUT,530) CTMP3
            CALL UPCASE(CTMP3)
            DO 50 KI=1,NUMINST
              CTMP4=INAME(ILOC+KI-1)
              CALL UPCASE(CTMP4)
              IF(CTMP3.EQ.CTMP4) THEN
                NI=KI
                GOTO 60
              ENDIF
   50       CONTINUE
            WRITE(IOUT,540) PACK,CTMP3,PARNAM(IP)
            CALL USTOP(' ')
   60       CONTINUE
          ENDIF
C
          IF (IACTIVE(IP).GT.0) THEN
            WRITE(IOUT,550) PARNAM(IP)
            CALL USTOP(' ')
          ENDIF
C
          IACTIVE(IP)=NI
C
          IBEG=IPLOC(1,IP)+(NI-1)*NLST
          IEND=IBEG+NLST-1
          PV=B(IP)
          IACTIVE(IP)=NI
          RETURN
        END IF
  100 CONTINUE
C
      WRITE(IOUT,*) ' The ',PACK,
     &   ' file specifies an undefined parameter:',LINE(ISTART:ISTOP)
      CALL USTOP(' ')
C
      END
      SUBROUTINE UPARARRCK(BUFF,IBOUND,IOUT,IZON,LAY,NCOL,NLAY,NROW,
     &                     NZONAR,PTYP)
C
C-----VERSION 19990805 ERB
C     ******************************************************************
C     CHECK FOR COMPLETE DEFINITION OF ONE LAYER OF CELLS BY PARAMETERS
C     OF A GIVEN TYPE
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
      INTEGER IBOUND(NCOL,NROW,NLAY), IZON(NCOL,NROW,NZONAR)
      REAL BUFF(NCOL,NROW)
      CHARACTER*4 PTYP
C     ------------------------------------------------------------------
  500 FORMAT(1X,'ERROR: BLANK PARAMETER TYPE -- STOP EXECUTION',
     &       ' (UPARARRCK)')
  510 FORMAT(1X,'ROW: ',I5,', COLUMN: ',I5,' IN LAYER ',I3,
     &       ' NOT DEFINED FOR PARAMETER TYPE ',A)
  520 FORMAT(/,1X,'PARAMETER DEFINITIONS INCOMPLETE -- STOP',
     &       ' EXECUTION (UPARARRCK)')
C
      IF (PTYP.EQ.' ') THEN
        WRITE (IOUT,500)
        CALL USTOP(' ')
      ENDIF
C
C-----INITIALIZE BUFF TO INDICATE DEFINITION BY PARAMETERS
      DO 20 I = 1, NROW
        DO 10 J = 1, NCOL
          BUFF(J,I) = 0.0
   10   CONTINUE
   20 CONTINUE
C
C-----INCREMENT BUFF FOR EACH CELL WHERE A PARAMETER OF THE CORRECT TYPE
C     APPLIES -- LOOP THROUGH PARAMETERS TO FIND MATCHING PARAMETER
C     TYPES
      DO 100 IP = 1, MXPAR
        IF (PARTYP(IP).EQ.PTYP) THEN
C---------LOOP THROUGH CLUSTERS ASSOCIATED WITH THIS PARAMETER
          DO 80 IC = IPLOC(1,IP), IPLOC(2,IP)
            IF (IPCLST(1,IC).EQ.LAY) THEN
              IZA = IPCLST(3,IC)
              DO 60 I = 1, NROW
                DO 50 J = 1,NCOL
                  IF (IZA.GT.0) THEN
C-------------------LOOP THROUGH ZONES LISTED FOR THIS CLUSTER
                    DO 40 IZI = 5, IPCLST(4,IC)
                      IZ = IPCLST(IZI,IC)
                      IF (IZ.EQ.IZON(J,I,IZA)) THEN
                        BUFF(J,I) = BUFF(J,I) + 1.0
                      ENDIF
   40               CONTINUE
                  ELSE
C-------------------ZONES DO NOT APPLY TO THIS CLUSTER
                    BUFF(J,I) = BUFF(J,I) + 1.0
                  ENDIF
   50           CONTINUE
   60         CONTINUE
            ENDIF
   80     CONTINUE
        ENDIF
  100 CONTINUE
C
C-----IDENTIFY ANY ACTIVE CELLS THAT ARE STILL EQUAL TO ZERO, THEREFORE
C     NOT CONTROLLED BY ANY PARAMETER OF THE CORRECT TYPE
      IERR = 0
      DO 140 I = 1, NROW
        DO 120 J = 1, NCOL
          IF (IBOUND(J,I,LAY).NE.0) THEN
            IF (BUFF(J,I).EQ.0.0)THEN
              WRITE (IOUT,510) I,J,LAY,PTYP
              IERR = IERR + 1
            ENDIF
          ENDIF
  120   CONTINUE
  140 CONTINUE
C
C-----IF ANY UNCONTROLLED ACTIVE CELLS ARE FOUND, WRITE ERROR MESSAGE
C     AND STOP
      IF (IERR.GT.0) THEN
        WRITE (IOUT,520)
        CALL USTOP(' ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE UPARFIND(PNAME,PTYP,PACK,IFOUND,IOUT)
C
C-----VERSION 31AUG2000 UPARFIND
C     ******************************************************************
C     Find the parameter number for a parameter name
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
      CHARACTER*(*) PNAME,PTYP,PACK
      CHARACTER*10 CTMP1,CTMP2
C     ------------------------------------------------------------------
C
      IF(PNAME.EQ.' ') THEN
         WRITE(IOUT,*) ' Blank parameter name in the ',PACK,' file.'
         CALL USTOP(' ')
      END IF
C
      CTMP1=PNAME
      CALL UPCASE(CTMP1)
      DO 100 IP=1,MXPAR
        CTMP2=PARNAM(IP)
        CALL UPCASE(CTMP2)
        IF(CTMP1.EQ.CTMP2) THEN
           IF(PARTYP(IP).NE.PTYP) THEN
              WRITE(IOUT,81) PARNAM(IP),PARTYP(IP),PACK,PTYP
   81         FORMAT(1X,'Parameter type conflict:',/
     1          1X,'Named parameter:',A,' was defined as type:',A,/
     2          1X,'However, this parameter is used in the ',A,
     3            ' file, so it should be type:',A)
              CALL USTOP(' ')
           END IF
           IFOUND=IP
           RETURN
        END IF
  100 CONTINUE
C
      WRITE(IOUT,101) PACK
  101 FORMAT(1X,'Parameter for ',A,' Package has not been defined')
      CALL USTOP(' ')
C
      END
      INTEGER FUNCTION INDXPTR(IP,NPE)
C
C-----VERSION 20010301 ERB INDXPTR
C     ******************************************************************
C     Return the position in the IPPTR pointer array of a specified
C     parameter number.  This provides the position for a parameter in
C     arrays with a dimension of NPE.  Function returns 0 if the
C     parameter number IP is not in IPPTR.
C     ******************************************************************
C     IP -  A PARAMETER NUMBER IN THE LIST OF NPLIST PARAMETERS IN THE
C           SEN FILE
C     NPE - NUMBER OF PARAMETERS FOR WHICH ISENS > 0 IN SEN FILE
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
      INDXPTR = 0
      DO 10 I=1,NPE
        IF (IPPTR(I).EQ.IP) THEN
          INDXPTR = I
          RETURN
        ENDIF
   10 CONTINUE
      RETURN
      END

