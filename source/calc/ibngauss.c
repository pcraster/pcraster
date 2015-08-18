#include "stddefx.h"


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "misc.h"
#include "calc.h"
#include "app.h"  /* appUnitTrue, appOutput */
#include "mathx.h"

/* global header (opt.) and test's prototypes "" */

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

/*!
 * Result = ibngauss(Units, Range, Packages)

  Result   scalar spatial
  Units    scalar spatial
  Range    scalar spatial or nonspatial
  Packages scalar spatial or nonspatial


  This function distributes a number of Units over a certain area.

  The Unit argument describes an number of units that can be distributed.
  Each distributable unit has a value of 1. The possible fractional part of
  Unit is left on the same position in Result. Example, if Unit has a
  value of 3.4, then 3 distinct units are distributed and 0.4 is kept on the same location on Result.

   The destination cell, the location where a unit is put in Result, is computed using
   a 3 dimensional gaussian distribution with it's mean value on the source cell,
   the location on Unit where the unit originates, and a standard deviation equal to
   the Range argument value on the source cell.

   <ol>
   <li> The range parameter must be equal or larger than zero.
   <li> The destination cell can equal the source cell.
   <li> If the destination cell is a MV on Units or Range or is outside the map, the unit  is "lost". In other words, maptotal(Result) eq maptotal(Units) is not true, but
     maptotal(Result) le maptotal(Units) is true.
   <li> Since this function deploys an gaussian distribution using a random number generator, subsequent calls to the function will yield different results, except if the -s option of pcrcalc is used.
   </ol>

The package argument allows you to package large amounts of units into a limited number of packages. ibngauss treads each package as a seperate entity which is distributed according to the described mechanism, and after being distributed unpackages the package. This is especially useful if you have a large number of units to be distributed.
The size of each package distributed as seperate entity contains Units/Packages units with some packages having 1 unit more, to distribute the possible remainder after the integer division of Units by packages.  Thus the total sum of all package sizes equals the Units parameter.
A Package parameter larger than Units or a Package parameter of 0 acts like a Package parameter equal to Units; each Unit is distributed on its own.
Non-integer values for the Package parameter are rounded to the nearest integer neighbour

*/

int IBNGauss(
     MAP_REAL8 *out,
     const MAP_REAL8 *units,
     const MAP_REAL8 *range,
     const MAP_REAL8 *nrPackages)
{
  int   rSrc,nrRows= units->NrRows(units);
  int   cSrc,nrCols= units->NrCols(units);
  REAL8 unitsValSrc,rangeValSrc,nrPackValSrc;

  /* algorithm wants points->Get() and all others to
   * return FALSE if a value is a missing value
   */
  out->SetGetTest(GET_MV_TEST, out);
  units->SetGetTest(GET_MV_TEST, units);
  nrPackages->SetGetTest(GET_MV_TEST, nrPackages);
  range->SetGetTest(GET_MV_TEST, range);

  /* set all to 0 or MV
   * check range of rangeValSrc
   */
  for(rSrc = 0; rSrc < nrRows; rSrc++)
   for(cSrc = 0; cSrc < nrCols; cSrc++)
   {
    if(units->Get(&unitsValSrc, rSrc, cSrc, units) &&
       nrPackages->Get(&nrPackValSrc, rSrc, cSrc, nrPackages) &&
       range->Get(&rangeValSrc, rSrc, cSrc, range))
    { /* init with remainder */
      REAL8 remainder = unitsValSrc - floor(unitsValSrc);
      out->Put(remainder, rSrc, cSrc, out);
      if (rangeValSrc < 0)
       return  RetError(1,"ibngauss: Domain error on parameters");
    } else /* some of the units maps are MV */
      out->PutMV(rSrc, cSrc, out);
   }


  for(rSrc = 0; rSrc < nrRows; rSrc++)
   for(cSrc = 0; cSrc < nrCols; cSrc++)
    if(units->Get(&unitsValSrc, rSrc, cSrc, units) &&
       nrPackages->Get(&nrPackValSrc, rSrc, cSrc, nrPackages) &&
       range->Get(&rangeValSrc, rSrc, cSrc, range)
      )
    {
      int i,nrInputUnits = (int)floor(unitsValSrc);
      int nrPackages = (int)nrPackValSrc; /* round to integer */
      int packageSize;
      int nrPackagesWithOneMore;
      if (nrPackages <= 0 || nrPackages > nrInputUnits) {
        nrPackages=nrInputUnits;
        packageSize=1;
        nrPackagesWithOneMore=0;
      }
      else {
        packageSize=nrInputUnits/nrPackages;          /* integer division */
        nrPackagesWithOneMore= nrInputUnits%nrPackages; /* integer modulo */
      }
      rangeValSrc /= Side(); /* transform to pixel lengths */
       for(i=0; i < nrPackages; i++) {
         /* GasDev returns value from normal distribution mean=0,sd=1 */
          REAL8 length = GasDev()*rangeValSrc;
         /* Ran() return an uniform number
          * we create an angle between 0 and 180 degrees
          */
          REAL8 angle =Ran()*M_PI;
         /* now since length can be negative or positive we
          * generate a receiving cell address having an angle
          * with the src cell between 0 and 360 degrees
          */
          int xColDest = cSrc+(int)(length*cos(angle));
          int yRowDest = rSrc+(int)(length*sin(angle));
          REAL8 valDest;
          if (out->Get(&valDest, yRowDest, xColDest, out)) {
            /* destination is defined area */
            valDest+=packageSize;
           if (i < nrPackagesWithOneMore) {
             /* the first nrPackagesWithOneMore package do 1 more */
             valDest+=1;
           }
           out->Put(valDest, yRowDest, xColDest, out);
          }
       } /* for all packages */
    } /* eo for all cells */
  return 0;    /* successful terminated */
}
