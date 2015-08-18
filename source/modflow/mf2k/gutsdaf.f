C  Modified 11-30-2002 by Harvey Jobson
C
      SUBROUTINE STARTDAF (IERR,LUFLW,LUIN,LUOT)
C
C     + + + + + + + + + + + + + PURPOSE  + + + + + + + + + + + + + + + +
C     ************  This subroutine starts DAFLOW **********************
C     DAFLOW uses a variable DF=Q^(1-W2)/(2 W1 S) and assumes 
C     A=AO+A1(Q**A2), computes celerity from dQ/dA, conserves mass to
C     compute the average flow at the nodes. All boundry conditions
C     represent the average during the time step. The first BC represents
C     the flow from time 0 to Dt, for example. Dispersion is modeled by
C     mixing at shocks over a dispersion distance.
C     The Q is at the node point with QT occuring just upstream of node.
C
C     + + +  + + + + + + + + + + + PARAMETERS  + + + + + + + + + + + + +
      INCLUDE 'params.inc'
C     NOBR   - Maximum number of branches allowed in model
C     NOSC   - Maximum number of cross sections (nodes) allowed in branch
C     NOSH   - Maximum number of shocks allowed in branch
C              (NOSH should be at least 4 times NOSC)
C
C     + + + + + + + + + + + COMMONS  + + + + + + + + + + + + + + + + + +
      INCLUDE 'startdaf.com'
C
C     + + + + + + + + + +  COMMON VARIBLES (startdaf.com)  + + + + + + +
C     AO(I,N) A1(I,N) A2(I,N) F(K,N) FI(K,N) DT IENG IOUT(I,N)
C     JNCD(N) JNCU(N) JGO JTS NBRCH NHR NHRR NJNCT NS(N) NSI(N) NXSEC(N)
C     PF(N) PX(K,N) PXI(K,N) QI SL(I,N) TF(I,N) TFI(I,N) TIME VIN(I,N)
C     VI W1(I,N) W2(I,N) X(I,N) XFACT 
C
C     + + + + + + + + + + + + LOCAL VARIABLES  + + + + + + + + + + + + +
      INTEGER     I,IERR,J,K,LUFLW,LUIN,LUOT,N
      REAL        A,AA
C     CHARACTER*64 VERSN
      CHARACTER*80 TITLE
C
C     + + + + + + + + + + + LOCAL DEFINITIONS  + + + + + + + + + + + + +
C     IERR     Error code (0=ok, 20<stop as gracefully as you can)
C     TITLE    Title of program (80 characters max)
C
C     + + + + + + + + + + + + INTRINSICS + + + + + + + + + + + + + + + +
      INTRINSIC  FLOAT
C     + + + + + + + + + + + EXTERNALS  + + + + + + + + + + + + + + + + +
C
C    + + + + + + + + + + + + INPUT FORMATS + + + + + + + + + + + + + + +
 1000 FORMAT (A)
 1010 FORMAT (20X,I10)
 1020 FORMAT (20X,F10.3)
 1030 FORMAT (13X,I3,16X,F5.2,16X,I3,8X,I3)
C
C     + + + + + + + + + + + + + + OUTPUT FORMATS + + + + + + + + + + + +
 2000 FORMAT (1X,A,/)
 2010 FORMAT (' The',I3,' Branch Model with',I3,' Internal Junctions',
     #        ' is run',I5,' Time Steps each',F5.2,' hours long.')
 2020 FORMAT ('The Model starts at',F6.2,' hours past midnight.')
 2030 FORMAT ('The node output is given in "bltm.out" every',I4,
     #        ' time steps.')
 2040 FORMAT ('Input units are Metric (Meters & river kilometers')
 2050 FORMAT ('Input units are English (feet and river miles)')
 2060 FORMAT (' Width=W1(Q)**W2')
 2070 FORMAT (' Cross sectional area = AO+A1(Q**A2)')
 2080 FORMAT (' AO = Cross sectional area at zero flow.')
 2090 FORMAT (/,28X,'* * *  INITIAL CONDITIONS  * * *')
 2100 FORMAT (/,' node   Mi/km  Disch       Area      Width  ',
     $          ' Slope           A1         A2         W1        ',
     $          ' W2         AO')
 2110 FORMAT (/,'Branch',I4,' Extends from JNCT',I3,' to JNCT',I3,
     $        ' and receives',F5.2,' of flow at JNCT',I3,/)
 2120 FORMAT (I5,F8.2,9G11.4)
 2130 FORMAT (/,25X,'* * *  OUTPUT  * * *',/)
 3000 FORMAT (3I5,4E18.5)
C
C      + + + + + + + + + + + END SPECIFICATIONS  + + + + + + + + + + + +
C
C     unix what information
C     INCLUDE 'versn.inc'
C     VERSN = '@(#)DAFLOW - last modified June 27, 1997 hej '
C     VERSN = Written by HEJ on March 11, 1998
C
C     **************** Zero arrays and preliminaries *******************
      NHRR=1
      DO 40 N=1,NOBR
        JNCU(N)=0
        JNCD(N)=0
        NS(N)=0
        NSI(N)=0
        NXSEC(N)=0
        PF(N)=0.0
        DO 20 I=1,NOSC
          AO(I,N)=0.0
          A1(I,N)=0.0
          A2(I,N)=0.0
          SL(I,N)=0.0
          IOUT(I,N)=0
          X(I,N)=0.0
          TF(I,N)=0.0
          TFI(I,N)=0.0
          W1(I,N)=0.0
          W2(I,N)=0.0
          VIN(I,N)=0.0
C  8-28-2003   AWH
          V(I,N)=0.0
          AQ(I,N)=0.0
   20   CONTINUE
        DO 30 K=1,NOSH
          F(K,N)=0.0
          FI(K,N)=0.0
          PX(K,N)=0.0
          PXI(K,N)=0.0
   30   CONTINUE
   40 CONTINUE
C
C     *********************** Read common input ************************
      READ(LUIN,1000)TITLE
      WRITE(LUOT,2000)TITLE
      READ(LUIN,1010,ERR=900)NBRCH
      READ(LUIN,1010,ERR=900)NJNCT
      READ(LUIN,1010,ERR=900)NHR
      READ(LUIN,1010,ERR=900)JTS
      READ(LUIN,1010,ERR=900)JGO
      READ(LUIN,1010,ERR=900)IENG
      READ(LUIN,1020,ERR=900)DT
      READ(LUIN,1020,ERR=900)AA
      QI=AA/100000.0
      VI=QI*DT*3600.0
      TIME=(FLOAT(JTS)-0.5)*DT
      WRITE(LUOT,2010)NBRCH,NJNCT,NHR,DT
      AA=DT*FLOAT(JTS)
      WRITE(LUOT,2020)AA
      WRITE(LUOT,2030)JGO
C
C     ************************* Read data for each branch **************
      DO 60 N=1,NBRCH
        READ(LUIN,1030,ERR=900)NXSEC(N),PF(N),JNCU(N),JNCD(N)
        READ(LUIN,1000)TITLE
        DO 50 I=1,NXSEC(N)
          IF(I.LT.NXSEC(N))THEN
            READ(LUIN,*,END=900,ERR=900)K,X(K,N),IOUT(K,N),F(K,N),
     #             A1(K,N),A2(K,N),AO(K,N),SL(K,N),W1(K,N),W2(K,N)
          ELSE
            READ(LUIN,*,END=900,ERR=900)K,X(K,N),IOUT(K,N)
          END IF
   50   CONTINUE
   60 CONTINUE
C
C     ***** Make preliminary computation and write initial conditions **
      IF(IENG.EQ.0)THEN
C       Metric units
        WRITE(LUOT,2040)
C       XFACT=1609.34
        XFACT=1000.0
      ELSE
C       English units
        WRITE(LUOT,2050)
        XFACT=5280.00
      END IF
      WRITE(LUOT,2070)
      WRITE(LUOT,2080)
      WRITE(LUOT,2060)
C
      WRITE(LUOT,2090)
      WRITE(LUOT,2100)
      J=0
      DO 80 N=1,NBRCH
        NS(N)=NXSEC(N)-1
        NSI(N)=NS(N)
        WRITE(LUOT,2110)N,JNCU(N),JNCD(N),PF(N),JNCU(N)
        DO 70 I=1,NXSEC(N)
          IF(I.LT.NXSEC(N))THEN
            PX(I,N)=X(I,N)*XFACT
            PXI(I,N)=PX(I,N)
            FI(I,N)=F(I,N)
            IF(F(I,N).GT.0.0)THEN
              A=AO(I,N)+A1(I,N)*(F(I,N)**A2(I,N))
              AA=W1(I,N)*(F(I,N)**W2(I,N))
            ELSE
              A=AO(I,N)
              AA=0.0
            END IF
            VIN(I,N)=A*XFACT*(X(I+1,N)-X(I,N))
            IF(I.GT.1)THEN
              TF(I,N)=F(I,N)-F(I-1,N)
              TFI(I,N)=TF(I,N)
            END IF
            WRITE(LUOT,2120)I,X(I,N),F(I,N),A,AA,SL(I,N),
     $                      A1(I,N),A2(I,N),W1(I,N),W2(I,N),AO(I,N)
            WRITE(LUFLW,3000)J,N,I,F(I,N),A,AA,TF(I,N)
          ELSE
            WRITE(LUOT,2120)I,X(I,N),F(I-1,N)
            WRITE(LUFLW,3000)J,N,NXSEC(N),F(I-1,N)
          END IF
          X(I,N)=X(I,N)*XFACT
   70   CONTINUE
   80 CONTINUE
C
      WRITE(LUOT,2130)
      GO TO 999
  900   IERR=22
  999 RETURN
      END
C
      SUBROUTINE GETBC (IERR,J,LUIN,LUOT)
C
C     + + + + + + + + + + + + + PURPOSE  + + + + + + + + + + + + + + + +
C     ******This subroutine reads the boundary conditions for DAFLOW ***
C
C     + + +  + + + + + + + + + + + PARAMETERS  + + + + + + + + + + + + +
      INCLUDE 'params.inc'
C
C     + + + + + + + + + + + COMMONS  + + + + + + + + + + + + + + + + + +
      INCLUDE 'startdaf.com'
C
C     + + + + + + + + + + + COMMON VARIABLES (startdaf.com) + + + + + +
C     DT IDBG JTS TIME TRB(I,N)
C
C     + + + + + + + +  + + + LOCAL VARIABLES  + + + + + + + + + + + + +
      INTEGER I,IERR,J,JJ,K,LUIN,LUOT,N,NBC
C     CHARACTER*64 VERSN
C
C     + + + + + + + + + + + LOCAL DEFINITIONS  + + + + + + + + + + + + +
C     IERR     Error code (0=ok, 20<stop as gracefully as you can)
C     NBC      Number of boundary conditions to be read
C
C     + + + + + + + + + + + INPUT FORMATS  + + + + + + + + + + + + + + +
C
 1000 FORMAT (18X,I3)
 1010 FORMAT (10X,I3,5X,I3,3X,G14.5)
C
C     + + + + + + + + + + + + + + OUTPUT FORMATS + + + + + + + + + + + +
 2000 FORMAT('Format error on number of boundary condition for time',I5)
 2010 FORMAT('Format error on boundary condition',I5,' Time step',I5)
C      + + + + + + + + + + + END SPECIFICATIONS  + + + + + + + + + + + +
C
C     unix what information
C     INCLUDE 'versn.inc'
C     VERSN = '@(#)DAFLOW - written by HEJ March 16, 1998'
C
C     ************************ read boundary conditions ****************
      READ(LUIN,1000,ERR=900)NBC
      IF(NBC.GT.0)THEN
C       ***********  boundary conditons for this time are to be read ***
        DO 40 K=1,NBC
          READ(LUIN,1010,ERR=910)N,I,TRB(I,N)
          JJ=IFIX(TIME/DT+0.501)-JTS+1
          IF(IDBG.EQ.1) WRITE(LUOT,*)'J,N,I,TRB',JJ,N,I,TRB(I,N)
   40   CONTINUE
      END IF
C
      GO TO 999
  900   IERR=22
        WRITE(LUOT,2000)J
        GO TO 999
  910   IERR=22
        WRITE(LUOT,2010)K,J
  999 RETURN
      END
C
      SUBROUTINE PRERTE
C
C     + + + + + + + + + + + + + PURPOSE  + + + + + + + + + + + + + + + +
C     Prepare for routing NHRR time steps by setting current flow arrays
C
C     + + +  + + + + + + + + + + + PARAMETERS  + + + + + + + + + + + + +
      INCLUDE 'params.inc'
C     + + + + + + + + + + + COMMONS  + + + + + + + + + + + + + + + + + +
      INCLUDE 'startdaf.com'
C
C     + + + + + + + + + +  COMMON VARIBLES (startdaf.com)  + + + + + + +
C     F(K,N) FI(K,N) NBRCH NS(N) NSI(N) NXSEC(N) PX(K,N) PXI(K,N)
C     TF(I,N) TFI(I,N)
C     + + + + + + + + + + + + LOCAL VARIABLES  + + + + + + + + + + + + +
      INTEGER  I,N
C     CHARACTER*64 VERSN
C
C     + + + + + + + + + + + LOCAL DEFINITIONS  + + + + + + + + + + + + +
C     + + + + + + + + + + + + INTRINSICS + + + + + + + + + + + + + + + +
C     + + + + + + + + + + + EXTERNALS  + + + + + + + + + + + + + + + + +
C     + + + + + + + + + + + + INPUT FORMATS  + + + + + + + + + + + + + +
C     + + + + + + + + + + + + + + OUTPUT FORMATS + + + + + + + + + + + +
C     + + + + + + + + + + + END SPECIFICATIONS + + + + + + + + + + + + +
C
C     unix what information
C     INCLUDE 'versn.inc'
C     VERSN = 'Written by HEJ on March 17, 1998'
C     **************** Zero arrays and preliminaries *******************
      DO 20 N=1,NBRCH
        NS(N)=NSI(N)
        DO 10 I=1,NXSEC(N)
          TF(I,N)=TFI(I,N)
   10   CONTINUE
        DO 20 I=1,NS(N)
          F(I,N)=FI(I,N)
          PX(I,N)=PXI(I,N)
   20 CONTINUE
  999 RETURN
      END
C
      SUBROUTINE SETJNVL (JCD,NCD)
C
C     + + + + + + + + + + + + + PURPOSE  + + + + + + + + + + + + + + + +
C     Set junction values and mixing codes
C
C     + + +  + + + + + + + + + + + PARAMETERS  + + + + + + + + + + + + +
      INCLUDE 'params.inc'
C     NOBR   - Maximum number of branches allowed in model
C
C     + + + + + + + + + + + COMMONS  + + + + + + + + + + + + + + + + + +
      INCLUDE 'startdaf.com'
C     + + + + + + + + + +  COMMON VARIBLES (startdaf.com)  + + + + + + +
C     NBRCH NJNCT 
C     + + + + + + + + + + + + LOCAL VARIABLES  + + + + + + + + + + + + +
      INTEGER JCD(NOBR),N,NCD(NOBR)
C
C     + + + + + + + + + + + LOCAL DEFINITIONS  + + + + + + + + + + + + +
C     JCD(M)   Code for junction mixing (0=all inflows known, 1=not known)
C     NCD(N)   Branch code (0=routed, 1=not routed)
C     + + + + + + + + + + + + INTRINSICS + + + + + + + + + + + + + + + +
C     + + + + + + + + + + + EXTERNALS  + + + + + + + + + + + + + + + + +
C     + + + + + + + + + + + + INPUT FORMATS  + + + + + + + + + + + + + +
C     + + + + + + + + + + + + + + OUTPUT FORMATS + + + + + + + + + + + +
C     + + + + + + + + + + + END SPECIFICATIONS + + + + + + + + + + + + +
C     unix what information
C     INCLUDE 'versn.inc'
C     VERSN = 'Written by HEJ on March 17, 1998'
C       ** set branch code to not routed and junction code to not mixed*
        DO 10 N=1,NOBR
          NCD(N)=1
          IF(N.LE.NJNCT)THEN
            JCD(N)=1
          ELSE
            JCD(N)=0
          END IF
   10   CONTINUE
  999 RETURN
      END
C
C
      SUBROUTINE RTBR (IERR,LUOT,J,JCD,JN,N,NCD)
C
C     + + + + + + + + + + + + + PURPOSE  + + + + + + + + + + + + + + + +
C     Find branch to route and route it
C
C     + + +  + + + + + + + + + + + PARAMETERS  + + + + + + + + + + + + +
      INCLUDE 'params.inc'
C     NOBR   - Maximum number of branches allowed in model
C     NOSC   - Maximum number of cross sections (nodes) allowed in branch
C     NOSH   - Maximum number of shocks allowed in branch
C              (NOSH should be at least 4 times NOSC)
C
C     + + + + + + + + + + + COMMONS  + + + + + + + + + + + + + + + + + +
      INCLUDE 'startdaf.com'
C
C     + + + + + + + + + +  COMMON VARIBLES (startdaf.com)  + + + + + + +
C     AO(I,N) AQ(I,N) A1(I,N) A2(I,N) DT F(K,N) IDBG JNCD(N) JNCU(N) 
C     NS(N) NXSEC(N) PX(K,N) QI SL(I,N) TF(I,N) TRB(I,N) VI W1(I,N) 
C     W2(I,N) X(I,N)
C
C     + + + + + + + + + + + + LOCAL VARIABLES  + + + + + + + + + + + + +
      INTEGER  IERR,JCD(NOBR),J,JN,K,LUOT,N,NCD(NOBR),NSS
      REAL     DTS,FS(NOSH),PXS(NOSH)
C
C     + + + + + + + + + + + LOCAL DEFINITIONS  + + + + + + + + + + + + +
C     DTS      Time step in seconds
C     FS(K)    Flow in shock k of local branch
C     J        Time step  
C     JCD(M)   Code for junction mixing (0=all inflows known, 1=not known)
C     JN       Junction being updated
C     N        Branch being routed
C     NCD(N)   Branch code (0=routed, 1=not routed)
C     PXS(K)   Location of shock K for local branch
C
C     + + + + + + + + + + + END SPECIFICATIONS + + + + + + + + + + + + +
C     VERSN = Written by HEJ on March 17, 1998
C     **************** Zero arrays and preliminaries *******************
      DTS=DT*3600.0
      N=0
      IERR=0
   10 CONTINUE
C       ***************** Looking for something to route? **************
        N=N+1
        IF(NCD(N).EQ.0)GO TO 10
        IF(JCD(JNCU(N)).NE.0)GO TO 10
      NSS=NS(N)
      DO 20 K=1,NSS
        FS(K)=F(K,N)
        PXS(K)=PX(K,N)
   20 CONTINUE
      CALL ROUTE
     I      (AO(1,N),A1(1,N),A2(1,N),SL(1,N),DTS,IDBG,IERR,J,LUOT,
     M      N,NXSEC(N),NSS,FS,PXS,QI,TRB(1,N),TF(1,N),VI,W1(1,N),
     N      W2(1,N),X(1,N),TIME,JTS)
C     *********** An error here causes a quick and nasty exit **********
      IF(IERR.GT.20)GO TO 999
        NS(N)=NSS
        DO 30 K=1,NSS
          F(K,N)=FS(K)
          PX(K,N)=PXS(K)
   30   CONTINUE
C       ****************** update junction flows and codes *************
        NCD(N)=0
        JN=JNCD(N)
        JCD(JN)=0
        AQ(1,N)=TRB(1,N)
  999 RETURN
      END
C
C
      SUBROUTINE FGQ (I,J,LUOT,N,VO)
C
C     + + + + + + + + + + + + + PURPOSE  + + + + + + + + + + + + + + + +
C     Find volumes in subreaches and compute node flows
C
C     + + +  + + + + + + + + + + + PARAMETERS  + + + + + + + + + + + + +
      INCLUDE 'params.inc'
C     NOBR   - Maximum number of branches allowed in model
C     NOSC   - Maximum number of cross sections (nodes) allowed in branch
C     + + + + + + + + + + + COMMONS  + + + + + + + + + + + + + + + + + +
      INCLUDE 'startdaf.com'
C     + + + + + + + + + + + + COMMON VARIBLES (startdaf.com) + + + + + +
C     AO(I,N) AQ(I,N) A1(I,N) A2(I,N) DT F(I,N) IDBG NS(N) NXSEC(N)
C     PX(I,N) QI TF(I,N) TRB(I,N) V(I,N) X(I,N)
C
C     + + + + + + + + + + + + LOCAL VARIABLES  + + + + + + + + + + + + +
      INTEGER  I,J,K,LUOT,N
      REAL     AA,BB,VO,XL,XR
C     + + + + + + + + + + + LOCAL DEFINITIONS  + + + + + + + + + + + + +
C     I     - node of flow or subreach of volume
C     J     - Time step number
C     N     - Branch number
C     + + + + + + + + + + + + INTRINSICS + + + + + + + + + + + + + + + +
      INTRINSIC  ABS
C     + + + + + + + + + + + EXTERNALS  + + + + + + + + + + + + + + + + +
C     + + + + + + + + + + + + INPUT FORMATS  + + + + + + + + + + + + + +
C     + + + + + + + + + + + + + + OUTPUT FORMATS + + + + + + + + + + + +
 2000 FORMAT ('Computed negative flow of',G14.3,' at',
     $         I4,' branch',I3, ' node',I3)
C     + + + + + + + + + + + END SPECIFICATIONS + + + + + + + + + + + + +
C     unix what information
C     INCLUDE 'versn.inc'
C     VERSN = 'Written by HEJ on March 18, 1998'
C     
            XL=X(I,N)
            XR=X(I+1,N)
            CALL FKAI(K,NS(N),PX(1,N),X(I,N))
            CALL FVOL(AO(1,N),A1(1,N),A2(1,N),K,NS(N),NXSEC(N),F(1,N),
     #                PX(1,N),TF(1,N),V(I,N),X(1,N),XL,XR,AA,BB)
            AQ(I+1,N)=AQ(I,N)+TRB(I+1,N)+(VO-V(I,N))/(DT*3600.0)
            IF (AQ(I+1,N).LT.0.0) THEN
C             ************* Can't have negative flow *******************
              K=I+1
              WRITE(LUOT,2000) AQ(K,N),J,N,K
            END IF
            IF(AQ(I+1,N).LT.QI)AQ(I+1,N)=0.0
            IF(IDBG.EQ.1)THEN
C             ************ Debug output ********************************
              WRITE(LUOT,*)'VO,V(I,N,J),AQ',VO,V(I,N),AQ(I+1,N)
            END IF
  999 RETURN
      END
C
      SUBROUTINE SETJV2 (JCD,JN,NCD)
C
C     + + + + + + + + + + + + + PURPOSE  + + + + + + + + + + + + + + + +
C     Set junction values after route
C
C     + + +  + + + + + + + + + + + PARAMETERS  + + + + + + + + + + + + +
      INCLUDE 'params.inc'
C     NOBR   - Maximum number of branches allowed in model
C     NOSC   - Maximum number of cross sections (nodes) allowed in branch
C     + + + + + + + + + + + COMMONS  + + + + + + + + + + + + + + + + + +
      INCLUDE 'startdaf.com'
C     + + + + + + + + + +  COMMON VARIBLES (startdaf.com)  + + + + + + +
C     AQ(I,N) JNCD(N) JNCU(N) NBRCH NXSEC(N) PF(N) TRB(I,N)
C
C     + + + + + + + + + + + + LOCAL VARIABLES  + + + + + + + + + + + + +
      INTEGER  JCD(NOBR),JN,N,NCD(NOBR)
      REAL     QJ
C
C     + + + + + + + + + + + LOCAL DEFINITIONS  + + + + + + + + + + + + +
C     JCD(M)   Code for junction mixing (0=all inflows known, 1=not known)
C     JN       Junction in question
C     NCD(N)   Branch code (0=routed, 1=not routed)
C     QJ       Flow at junction
C
C     + + + + + + + + + + + + INTRINSICS + + + + + + + + + + + + + + + +
C     + + + + + + + + + + + EXTERNALS  + + + + + + + + + + + + + + + + +
C     + + + + + + + + + + + + INPUT FORMATS  + + + + + + + + + + + + + +
C     + + + + + + + + + + + + + + OUTPUT FORMATS + + + + + + + + + + + +
C     + + + + + + + + + + + END SPECIFICATIONS + + + + + + + + + + + + +
C
C     unix what information
C     INCLUDE 'versn.inc'
C     VERSN = 'Written by HEJ on March 11, 1998'
C     **************** Set junction codes and flows ********************
      QJ=0.0
      DO 10 N=1,NBRCH
        IF(JNCD(N).EQ.JN)THEN
          IF(NCD(N).NE.0)JCD(JN)=1
          QJ=QJ+AQ(NXSEC(N),N)
        END IF
   10 CONTINUE
      IF(JCD(JN).EQ.0) THEN
        DO 20 N=1,NBRCH
          IF(JNCU(N).EQ.JN) THEN
            TRB(1,N)=QJ*PF(N)
          END IF
   20   CONTINUE
      END IF
  999 RETURN
      END
C
      SUBROUTINE PRTFLW (LUFLW,LUOT)
C
C     + + + + + + + + + + + + + PURPOSE  + + + + + + + + + + + + + + + +
C     *** This subroutine prints the results ***************************
C     + + + + + + + + + + + + PARAMETERS + + + + + + + + + + + + + + + +
      INCLUDE 'params.inc'
C     NOBR   - Maximum number of branches allowed in model
C     NOSC   - Maximum number of cross sections (nodes) allowed in branch
C
C     + + + + + + + + + + + + + COMMONS  + + + + + + + + + + + + + + + +
      INCLUDE 'startdaf.com'
C
C     + + + + + +  COMMON DEFINTIONS  (startdaf.com) + + + + + + + + + +
C     AO(I,N) AQ(I,N) A1(I,N) A2(I,N) DT IOUT(I,N) JGO JTS NBRCH 
C     NXSEC(N) TIME TRB(I,N) V(I,N) W1(I,N) W2(I,N) X(I,N) 
C
C     + + + + + + + + + + LOCAL VARIABLES  + + + + + + + + + + + + + + +
      INTEGER I,LUFLW,LUOT,N,NTS
      REAL    A,AA,Q,W
C
C     + + + + + + + + + + + LOCAL DEFINITIONS  + + + + + + + + + + + + +
C     A        Area of flow
C     NTS      Number of time steps since start of model
C     Q        Discharge
C     W        Top width of channel in subreach
C
C     + + + + + + + + + + + + + INTRINSICS + + + + + + + + + + + + + + +
      INTRINSIC  FLOAT,IFIX,MOD
C
C     + + + + + + + + + + + + + OUTPUT FORMATS + + + + + + + + + + + + +
 2000 FORMAT (' Day',I4,'  Hour',F6.2,' Branch  node  Discharge')
 2010 FORMAT (20X,I7,I6,G14.4)
 3000 FORMAT (3I5,4E18.5)
C     + + + + + + + + + + + + END SPECIFICATIONS + + + + + + + + + + + +
C
C     ************************ Write results  **************************
        TIME=TIME+DT
        I=IFIX(TIME/24.0)+1
        AA=TIME-FLOAT(I-1)*24.0
        NTS=IFIX((TIME+DT/2.0+0.0001)/DT)-JTS
        IF(MOD(NTS,JGO).EQ.0) THEN
          WRITE(LUOT,2000)I,AA
        END IF
        DO 20 N=1,NBRCH
          DO 10 I=1,NXSEC(N)
            IF(MOD(NTS,JGO).EQ.0.AND.IOUT(I,N).EQ.1)THEN
              WRITE(LUOT,2010)N,I,AQ(I,N)
            END IF
            IF(I.LT.NXSEC(N))THEN
              AA=0.0
              IF(I.GT.1)AA=TRB(I,N)
              A=V(I,N)/(X(I+1,N)-X(I,N))
              IF(A.GT.AO(I,N))THEN
                Q=((A-AO(I,N))/A1(I,N))**(1.0/A2(I,N))
              ELSE
                Q=0.0
              END IF
              IF(Q.GT.0.0)THEN
                W=W1(I,N)*(Q**W2(I,N))
              ELSE
                W=0.0
              END IF
              WRITE(LUFLW,3000)NTS,N,I,AQ(I,N),A,W,AA
            END IF
   10     CONTINUE
          WRITE(LUFLW,3000)NTS,N,NXSEC(N),AQ(NXSEC(N),N)
   20   CONTINUE
  999 RETURN
      END
C
