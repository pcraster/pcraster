C     + + + + + + + + + + + + + + + COMMONS  + + + + + + + + + + + + + +
C     INCLUDE 'startdaf.com'
      INTEGER  IDBG,IENG,IOUT(NOSC,NOBR),JNCD(NOBR),JNCU(NOBR),JGO,JTS,
     #         NBRCH,NHR,NHRR,NJNCT,NS(NOBR),NSI(NOBR),NXSEC(NOBR)
      REAL     AO(NOSC,NOBR),AQ(NOSC,NOBR),A1(NOSC,NOBR),
     #         A2(NOSC,NOBR),DT,F(NOSH,NOBR),
     #         FI(NOSH,NOBR),PF(NOBR),PX(NOSH,NOBR),PXI(NOSH,NOBR),QI,
     #         SL(NOSC,NOBR),TF(NOSC,NOBR),TFI(NOSC,NOBR),TIME,
     #         TRB(NOSC,NOBR),V(NOSC,NOBR),VIN(NOSC,NOBR),VI,
     #         W1(NOSC,NOBR),W2(NOSC,NOBR),X(NOSC,NOBR),XFACT
C
      COMMON /STARTI/ IDBG,IENG,IOUT,JNCD,JNCU,JGO,JTS,NBRCH,NHR,NHRR,
     #                NJNCT,NS,NSI,NXSEC
      COMMON /STARTR/ AO,AQ,A1,A2,DT,F,FI,PF,PX,PXI,QI,SL,TF,TFI,
     #                TIME,TRB,V,VIN,VI,W1,W2,X,XFACT
      SAVE /STARTI/,/STARTR/
C
C     + + + + + + + + + + + COMMON DEFINTIONS (startdaf.com) + + + + + +
C     AO(I,N)  Cross sectional area at zero flow for subreach I, branch N
C     AQ(I,N)  Discharge at node I, branch N (time step averaged)
C     A1(I,N)  Cross sectional area at a flow of 1.0.
C     A2(I,N)  Exponent of area equation A=AO+A1(Q**A2).
C     F(K,N)   Steady flow in shock K of branch N
C     FI(K,N)  Initial steady flow in shock K of branch N
C     DT       Time step size (hours)
C     IDBG     Debugger code (0 or N = no, 1 = write debug code)
C     IENG     Input units: 0=metric , 1=English
C     IOUT(I,N) Flag (1 = output) for node I in branch N
C     JNCD(N)  d/s junction no. for branch N (number interior first)
C     JNCU(N)  u/s junction no. for branch N (number interior first)
C     JGO      Number of time steps between output in "flow.out"
C     JTS      Number of time steps from midnight to start of model
C     NBRCH    Number of branchs
C     NHR      Number of time steps to be modeled
C     NHRR     Number of time steps to be repeated
C     NJNCT    Number of interior junctions
C     NS(N)    Number of shocks in branch N
C     NSI(N)   Initial number of shocks in branch N
C     NXSEC(N) Number of nodes in branch N
C     PF(N)    Portion of flow at junction to enter branch N
C     PX(K,N)  Location of u/s boundary of shock K in branch N
C     PXI(K,N) Initial location of u/s boundary of shock K in branch N
C     QI       Insignificant discharge (QP/100000.0)
C     SL(I,N)  Slope, wave dispersion coefficient=Q/(2*S*W)
C     TF(I,N)  Flow  in tributary at node I of branch N
C     TFI(I,N) Initial flow  in tributary at node I of branch N
C     TIME     Time in hours since midnight on model start day
C     TRB(I,N) New flow in tributary at node I of branch N
C     V(I,N)   Volume of water in subreach I, of branch N
C     VIN(I,N) Initial volume of water in subreach I of branch N
C     VI       Insignificant volume (QI*DT, in seconds)
C     W1(I,N)  Coefficient in W=W1(Q)**W2 for subreach I branch N
C     W2(I,N)  Coefficient in W=W1(Q)**W2 for subreach I branch N
C     X(I,N)   Distance of node I in branch N downstream of reference point
C     XFACT    Conversion from miles to feet or meters, depending on IENG

