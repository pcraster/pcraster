#include "stddefx.h"


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "misc.h"
#include "calc.h"

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */

/***************/
/* EXTERNALS   */
/***************/

/**********************/
/* LOCAL DECLARATIONS */
/**********************/


#define NR_LDD_DIR 9  /* number of directions as defined in ldd */
#define LDD_PIT 5  /* ldd direction 5 is pit */
#define INVALID_LDD_DIR 10

/* see note RIVM-36 */

/* local drain direction maps have values for outflow directions as
 * follows:
 *  7   8   9
 *   \  |  /
 *  4 - 5 - 6
 *    / |  \
 *  1   2   3
 *  e.g. if a cell has a value 7, it flows TO its upper left neigbour
 */

static struct { 
     int deltaCol;
     int deltaRow;
       } LddValue[10] = {{ 0,   0},
               {-1,   1},    /* 1 */
               { 0,   1},    /* 2 */
               { 1,   1},    /* 3 */
               {-1,   0},    /* 4 */
               { 0,   0},    /* LDD_PIT */
               { 1,   0},    /* 6 */
               {-1,  -1},    /* 7 */
               { 0,  -1},    /* 8 */
               { 1,  -1} };  /* 9 = NR_LDD_DIR */


/******************/
/* IMPLEMENTATION */
/******************/

/* Checks whether a cell (rFrom, cFrom) streams into cell (rToDS, cToDS).
 * Needs the row and column numbers of both cells and the ldd value
 * of the cell (rFrom, cFrom).
 * Determines whether the cell(rFrom, cFrom) is input for the cell
 * (rTo, cTo) according to the ldd value (lddFrom).
 * Returns 1 if first cell is input of second cell, 0 otherwise.
 *
 * EXAMPLE
 * .so examples/pitups.tr
 */
BOOL FlowsTo(int lddFrom,  /* ldd value of (rFrom,cFrom)  */
       int rFrom,          /* row    */
       int cFrom,          /* column */
       int rToDS,          /* row of possible downstream cell */
       int cToDS)          /* column possible downstream cell */
{
  PRECOND( 0 < lddFrom && lddFrom < 10 ); /* valid ldd value? */

  /* cells are adjacent: */
  PRECOND( ABS(rFrom-rToDS) <= 1 && ABS(cFrom-cToDS) <= 1 );

  return ((rFrom+ LddValue[lddFrom].deltaRow == rToDS) &&
          (cFrom+ LddValue[lddFrom].deltaCol == cToDS));
}

/* Calculates the row number of the downstream cell.
 * An index and the row number of the current cell are input.
 * Returns the row number of the downstream cell.
 *
 * EXAMPLE
 * .so examples/pitneigh.tr
 */
 int DownStrR( int rowNr,  /* rowNr from current cell */
          int d)          /* ldd code of current cell */
 {
   int DSr;
   PRECOND(0 <= d && d <= NR_LDD_DIR);
   DSr = rowNr + LddValue[d].deltaRow;
   return DSr;
 }

/* Calculates the col number of the downstream cell.
 * An index and the col number of the current cell are input.
 * Returns the col number of the downstream cell.
 *
 * EXAMPLE
 * .so examples/pitneigh.tr
 */
 int DownStrC( int colNr,  /* colNr from current cell */
               int d)      /* ldd code of current cell */
 {
   int DSc;
   PRECOND(0 <= d && d <= NR_LDD_DIR);
   DSc = colNr + LddValue[d].deltaCol;
   return DSc;
 }

#ifdef DEBUG
  /* Calculates the col number of a neighbor cell.
   * An index and the col number of the current cell are input.
   * This a macro that calls DownStrC().
   * Returns the col number of the neighbor cell.
   *
   * EXAMPLE
   * .so examples/pitneigh.tr
   */
   int CNeighbor(int colNr,  /* colNr from current cell */
                 int index)  /* determines which neighbor */
   { return DownStrC(colNr,index);
   }

  /* Calculates the row number of a neighbor cell.
   * An index and the row number of the current cell are input.
   * This a macro that calls DownStrR().
   * Returns the row number of the neighbor cell.
   *
   * EXAMPLE
   * .so examples/pitneigh.tr
   */
   int RNeighbor(int rowNr,  /* rowNr from current cell */
           int index)  /* determines which neighbor */
   { return DownStrR(rowNr,index);
   }
#endif

/* Checks whether a cell has no input.
 * If a cell has no input none of its neighbors flows into it.
 * So for every neighbor is checked whether it flows into the
 * current cell.
 * Returns TRUE if current cell has no input, else FALSE.
 *
 * EXAMPLE
 * .so examples/pitend.tr
 */
BOOL NoInput(      const MAP_UINT1 *ldd,    /* ldd.map */
        int rowNr,       /* row of current cell*/
        int colNr)       /* column current cell*/
{
  int   j;
  UINT1   lddVal;

  PRECOND(ldd->GetGetTest(ldd) == GET_MV_TEST);
  FOR_ALL_LDD_NBS(j)
  {
    int r = DownStrR(rowNr, j);
    int c = DownStrC(colNr, j);
    if (ldd -> Get(&lddVal, r, c, ldd)&&
      FlowsTo(lddVal, r, c, rowNr, colNr))
        return FALSE;
  }
  return TRUE;
}

/* Calculates the ldd value of a cell given its downstream cell.
 * Returns the ldd value for the cell.
 *
 * EXAMPLE
 * .so examples/exldddir.tr
 */
UINT1 Ldddir(
        int rFrom,  /* row number of cell */
        int cFrom,   /* column number of cell */
        int rDS,  /* row number of downstream cell */
        int cDS)  /* column number of downstream cell */
{
  UINT1   ldddir;
  int   lddtab[3][3] = {{3, 2, 1}, {6, 5, 4},{9, 8, 7}};
  int   rDelta = rFrom - rDS + 1;
  int   cDelta = cFrom - cDS + 1;

  PRECOND(0 <= cDelta && cDelta <= 2 &&
          0 <= rDelta && rDelta <= 2);

  ldddir = lddtab[rDelta][cDelta];

  POSTCOND(0 < ldddir && ldddir <= NR_LDD_DIR);
  return ldddir;
}

/* Calculates the sum of fluxes of upstream neighbors.
 * When one of the upstream neighbors has an undefined flux,
 * the sum is undefined and the state map will get a missing
 * value for the current cell.
 * Returns 1 when MV is found and sum is undefined, 0 otherwise.
 *
 * EXAMPLE
 * .so examples/exsumflux.tr
 */
int SumFluxUps(
    REAL8 *newState,  /* write-only new state */
    const MAP_REAL8 *flux,  /* map with fluxes */
    const MAP_UINT1 *ldd,  /* ldd map */
    int r,      /* row of current cell */
    int c)      /* column of current cell */
{
  int i;
  *newState = 0;

  PRECOND(ldd->GetGetTest(ldd) == GET_MV_TEST);
  PRECOND(flux->GetGetTest(flux) == GET_MV_TEST);

  /* sum of fluxes from upstream neighbors */
  for(i = 1; i <= NR_LDD_DIR; i++)
  {  /* all neighbors */
    if(i != LDD_PIT)    /* not cell itself */
    {
      int   rNext = DownStrR(r, i);
      int  cNext = DownStrC(c, i);
      UINT1   lddVal;
      REAL8   fluxVal;

      if(ldd->Get(&lddVal, rNext, cNext, ldd) &&
      flux->Get(&fluxVal, rNext, cNext, flux))
      /* neighbor is upstream element */
      {
        if(FlowsTo(lddVal, rNext, cNext, r, c))
          *newState += fluxVal;
      }
      else
      {
        if(ldd->Get(&lddVal, rNext, cNext, ldd) &&
        (FlowsTo(lddVal, rNext, cNext, r, c)))
         /* upstream neighbor with MV flux */
          return 1;
      }
    }
  }
  return 0;
}

/* Determines whether neighbor i is a corner neighbor.
 * Does not work for the pit.
 * Should become a macro in Non-DEBUG mode.
 * Returns TRUE if this is the case, FALSE otherwise.
 *
 * EXAMPLE
 * .so examples/pitcorne.tr
 */
BOOL Corner(int i)    /* direction of neighbor */
{
  PRECOND(IS_VALID_LDD_CODE(i));
  PRECOND(i != LDD_PIT);
  return i % 2;
}
