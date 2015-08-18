#include "stddefx.h" 


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "misc.h"
#include "calc.h"
#include "app.h" /* appUnitTrue, appOutput */
#include "mathx.h" /* exp,sqr */

/* global header (opt.) and test's prototypes "" */

#ifndef INCLUDED_GEO_CELLLOCVISITOR
#include "geo_celllocvisitor.h"
#define INCLUDED_GEO_CELLLOCVISITOR
#endif

#ifndef INCLUDED_FIELDAPI_INTERFACE
#include "fieldapi_interface.h"
#define INCLUDED_FIELDAPI_INTERFACE
#endif

/***************/
/* EXTERNALS   */
/***************/

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 

/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 

/******************/
/* IMPLEMENTATION */
/******************/

/* TODO add range check error
 error epsVal not garantueed to be > 0
 but guard inputVal > epsVal depends on it
 Get rid of MAX with ugly castings
*/

/* NOTE RIVM-32
 * i = input
 * d = distance
 * r = range
 * e = eps
 * we want
 * i*exp(-d/r) < e
 * rewritten in d, others are constant in loop
 * exp(-d/r) < e/i
 * log(exp(1)**(-d/r)) < log(e/i)
 *              -d/r   < log(e/i)
 *              -d     < r*log(e/i)
 *               d     <-r*log(e/i)
 * OTHER TRICK
 * we compare the square of distances
 * since sqrt is monotome incr function
 * only when we need the distance we take the sqrt
 */


extern "C" int InfluenceSimpleGauss(
     MAP_REAL8 *m_out,
     const MAP_REAL8 *m_input,
     const MAP_REAL8 *m_range,
     const MAP_REAL8 *m_eps)
{

 ReadWriteReal8_ref(out,m_out);
 ReadOnlyReal8_ref(input,m_input);
 ReadOnlyReal8_ref(range,m_range);
 ReadOnlyReal8_ref(eps,m_eps);

 // MV is the initial value
 out.putAllMV();

 size_t nrRows = out.nrRows();
 size_t nrCols = out.nrCols();

 /* set all to 0 or MV
  * check range of rangeVal
  */
 for(geo::CellLocVisitor c(out); c.valid(); ++c) {
   REAL8 inputVal,rangeVal,epsVal;
   if(input.get(inputVal, *c) &&
      range.get(rangeVal, *c) &&
      eps.get(epsVal,  *c) ) {
     /* init to 0 */
     out.put(0, *c);
     if (rangeVal <= 0)
      return RetError(1,
        "influencesimplegauss: Domain error on parameters");
   }
   else /* some of the input maps are MV */
     out.putMV(*c);
 }

 for(geo::CellLocVisitor c(out); c.valid(); ++c) {
   REAL8 inputVal,rangeVal,epsVal;
   if(input.get(inputVal, *c) &&
      range.get(rangeVal, *c) &&
      eps.get(epsVal,  *c)    &&
      inputVal > epsVal
     )
   {
    /* range in cell units!
     * then we have all in cell units
     * thus gauss-function is scaled to cellunits
     */
    rangeVal /= Side();
    /* see RIVM-32 doc */
    double maxDist = -rangeVal*log(epsVal/inputVal);
    int maxDistCells = (int)ceil(maxDist);
    maxDist *= maxDist; /* square again, can do > cmp
                            before taking sqrt */
     size_t rStart = static_cast<size_t>(MAX(((int)c.row())-maxDistCells,0));
     size_t cStart = static_cast<size_t>(MAX(((int)c.col())-maxDistCells,0));
     size_t rStop  = MIN(nrRows,c.row()+maxDistCells);
     size_t cStop  = MIN(nrCols,c.col()+maxDistCells);
     for(size_t rDest = rStart; rDest < rStop; rDest++)
       for(size_t cDest = cStart; cDest < cStop; cDest++)
      {
        REAL8 outVal;
        if(out.get(outVal, rDest, cDest)) {
         /* inside defined area */

         /* compute distSqr */
         REAL8 add,dist = sqr((double)c.row()-rDest)+sqr((double)c.col()-cDest);
         if (dist > maxDist)
          continue; /* do not compute */
         /* else compute distSqr */
         add = inputVal*exp(-sqrt(dist)/rangeVal);
         outVal += add;
         out.put(outVal, rDest, cDest);
       }
      }
   }
 }
 return 0;  /* successful terminated */
}
