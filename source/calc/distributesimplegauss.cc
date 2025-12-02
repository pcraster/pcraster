#include "stddefx.h"

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "misc.h"
#include "calc.h"
#include "app.h" /* appUnitTrue, appOutput */
#include "mathx.h"

/* global header (opt.) and test's prototypes "" */

#include "geo_celllocvisitor.h"
#include "fieldapi_interface.h"

#include <cmath>

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

extern "C" int DistributeSimpleGauss(MAP_REAL8 *m_out, MAP_REAL8 *m_tmp, const MAP_REAL8 *m_input,
                                     const MAP_REAL8 *m_range, const MAP_REAL8 *m_eps)
{

  ReadWriteReal8_ref(out, m_out);
  ReadWriteReal8_ref(tmp, m_tmp);
  ReadOnlyReal8_ref(input, m_input);
  ReadOnlyReal8_ref(range, m_range);
  ReadOnlyReal8_ref(eps, m_eps);

  // MV is the initial value
  out.putAllMV();

  size_t const nrRows = out.nrRows();
  size_t const nrCols = out.nrCols();

  /* set all to 0 or MV
  * check range of rangeVal
  */
  for (geo::CellLocVisitor c(out); c.valid(); ++c) {
    REAL8 inputVal = NAN;
    REAL8 rangeVal = NAN;
    REAL8 epsVal = NAN;
    if (input.get(inputVal, *c) && range.get(rangeVal, *c) && eps.get(epsVal, *c)) {
      /* init to 0 */
      out.put(0, *c);
      if (rangeVal <= 0) {
        return RetError(1, "distributesimplegauss: Domain error on parameters");
      }
    } else { /* some of the input maps are MV */
      out.putMV(*c);
    }
  }

  for (geo::CellLocVisitor c(out); c.valid(); ++c) {
    REAL8 inputVal = NAN;
    REAL8 rangeVal = NAN;
    REAL8 epsVal = NAN;
    if (input.get(inputVal, *c) && range.get(rangeVal, *c) && eps.get(epsVal, *c) && inputVal > epsVal) {
      double maxDist = NAN;
      int maxDistCells = 0;
      /* range in cell units!
     * then we have all in cell units
     * thus gauss-function is scaled to cellunits
     */
      rangeVal /= Side();
      /* see RIVM-32 doc */
      maxDist = -rangeVal * log(epsVal / inputVal);
      maxDistCells = (int)ceil(maxDist);
      maxDist *= maxDist; /* square again, can do > cmp
                            before taking sqrt */
      double sum = 0;

      auto rStart = static_cast<size_t>(MAX(((int)c.row()) - maxDistCells, 0));
      auto cStart = static_cast<size_t>(MAX(((int)c.col()) - maxDistCells, 0));
      size_t const rStop = std::min(nrRows, c.row() + maxDistCells);
      size_t const cStop = std::min(nrCols, c.col() + maxDistCells);
      for (size_t rDest = rStart; rDest < rStop; rDest++) {
        for (size_t cDest = cStart; cDest < cStop; cDest++) {
          tmp.putMV(rDest, cDest);
          REAL8 outVal = NAN;  // TODO implement fieldapi::Common::isMV(int,int)
          if (out.get(outVal, rDest, cDest)) {
            /* inside defined area */

            /* compute distSqr */
            REAL8 const dist = sqr((double)c.row() - rDest) + sqr((double)c.col() - cDest);
            if (dist > maxDist) {
              continue; /* do not compute */
            }
            /* else compute distSqr */
            outVal = exp(-sqrt(dist) / rangeVal);
            tmp.put(outVal, rDest, cDest);
            sum += outVal;
          }
        }
      }

      for (size_t rDest = rStart; rDest < rStop; rDest++) {
        for (size_t cDest = cStart; cDest < cStop; cDest++) {
          REAL8 tmpVal = NAN;
          REAL8 outVal = NAN;
          if (tmp.get(tmpVal, rDest, cDest)) {
            tmpVal = (tmpVal / sum) * inputVal;
            /* inside defined area */
            out.get(outVal, rDest, cDest);
            outVal += tmpVal;
            out.put(outVal, rDest, cDest);
          }
        }
      }
    }
  }
  return 0; /* successful terminated */
}
