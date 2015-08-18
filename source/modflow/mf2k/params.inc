      INTEGER NOBR,NOSC,NOSH,NOCO,NOPR,NOCP,NOTS 
C
      PARAMETER (NOBR=5, NOSC=300, NOSH=1200, NOCO=2)
      PARAMETER (NOPR=620, NOCP=150, NOTS=20)
C
C     + + + PARAMETER DEFINITIONS + + +
C     NOBR   - Maximum number of branches allowed in model
C     NOSC   - Maximum number of cross sections (nodes) allowed in branch
C     NOSH   - Maximum number of shocks allowed in branch
C              (NOSH should be at least 4 times NOSC)
C     NOCO   - Maximum number of constituents allowed
C     NOPR   - Maximum number of parcels allowed in branch
C              (NOPR should be at least 20 + 2 times NOSC)
C     NOCP   - Maximum number of computed or observed points
C     NOTS   - Maximum of time steps per ground water step

