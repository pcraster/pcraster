C     + + + + + + + + + + + COMMONS  + + + + + + + + + + + + + + + + + +
C     INCLUDE 'ground.com'
C
      INTEGER  NCL(NOSC,NOBR),NLY(NOSC,NOBR),NRW(NOSC,NOBR)
      REAL     AQGW(NOSC,NOBR,NOTS),BC(NOSC,NOBR,NOTS),BEL(NOSC,NOBR),
     #         BTH(NOSC,NOBR),CND(NOSC,NOBR),SEP(NOSC,NOBR),
     #         SSEP(NOSC,NOBR),VGW(NOSC,NOBR,NOTS)
      REAL CSTR(NOSC,NOBR),STAGE(NOSC,NOBR),QSTR(NOSC,NOBR)
      REAL CCSTR(NOSC,NOBR),RHSSTR(NOSC,NOBR)
C
      COMMON /GROUNDI/ NCL,NLY,NRW
      COMMON /GROUNDR/ AQGW,BC,BEL,BTH,CND,SEP,SSEP,
     1               CSTR,STAGE,QSTR,CCSTR,RHSSTR,VGW
      SAVE /GROUNDI/,/GROUNDR/
C
C     +  + + + + + + + + + + COMMON DEFINTIONS  (ground.com) + + + + + +
C     AQGW(I,N,J) Discharge at node I, brch N, averaged during time step J
C     BC(I,N,J) Boundary condition for node I, branch N, & time step J.
C     BEL(I,N)  Elevation of stream bed for subreach I, branch N
C     BTH(I,N)  Thickness of the stream bed for subreach I, branch N
C     CND(I,N)  Hydraulic conductivity of streambed for subreach I, branch N
C     CSTR(I,N) Zero if water table is below river bottom
C                 Seepage per unit head otherwise
C     NCL(I,N)  Column number of seepage connection to subreach I, branch N
C     NLY(I,N)  Layer number of seepage connection to subreach I, branch N
C     NRW(I,N)  Row number of seepage connection to subreach I, branch N
C     QSTR(I,N) Zero if water table is abov river bottom, seepage otherwise
C     SEP(I,N)  Seepage to river subreach I, branch N during daflow time step
C     SSEP(I,N) Seepage to river subreach I, branch N during MODFLOW time step
C     STAGE(I,N) Elev of river surface if water table above river bottom
C                 Zero if water table is below river bottom.
C     VGW(I,N)   Volume of water in subreach I of branch N, time J

