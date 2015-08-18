#include "stddefx.h"

#include "calc.h"  // for it's own interface

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif
#ifndef INCLUDED_CMATH
#include <cmath>
#define INCLUDED_CMATH
#endif

#ifndef INCLUDED_GEO_CELLLOCVISITOR
#include "geo_celllocvisitor.h"
#define INCLUDED_GEO_CELLLOCVISITOR
#endif

#ifndef INCLUDED_GEO_IDI
#include "geo_idi.h"
#define INCLUDED_GEO_IDI
#endif

#ifndef INCLUDED_COM_AUTOARRAYPTR
#include "com_autoarrayptr.h"
#define INCLUDED_COM_AUTOARRAYPTR
#endif

#ifndef INCLUDED_FIELDAPI_INTERFACE
#include "fieldapi_interface.h"
#define INCLUDED_FIELDAPI_INTERFACE
#endif



template<class Point>
static double interpolateNoSort(
       const std::vector<geo::IdiPoint<Point> >& points,
       double idp,
       const Point& c)
{
  // see PAB
  size_t nrPoints=points.size();
  geo::ComputeValue<Point> cv(c,idp);
  for(size_t i=0; i < nrPoints; i++) {
   if (points[i].isThisLocation(c)) // on exact location
     return points[i].value();
   cv.add(points[i]);
  }
  return cv.value();
}

// [map scalar ] = interpolate(
//                  [mask boolean],
//                  [inputvalues scalar],
//                  [ idp scalar spatial])
//                  [ max nr points nonspatial])
//                  [ radius nonspatial])
// idp may any value since we do distance**idp where is always distance > 0
extern "C" int Idi(
     MAP_REAL8 *m_resultMap,    /* write-only output map  */
     const MAP_UINT1 *m_mask,   /* points to be spread */
     const MAP_REAL8 *m_input,  /* initial costs */
     const MAP_REAL8 *m_idp, /* idi pow */
     const MAP_REAL8 *m_radius, /* max search radius (unit cell) */
     const MAP_REAL8 *m_maxNr)  /* max. nr points */
{
  ReadWriteReal8_ref(result,m_resultMap);
  ReadOnlyUint1_ref(mask,m_mask);
  ReadOnlyReal8_ref(input,m_input);
  ReadOnlyReal8_ref(idp,m_idp);
  ReadOnlyReal8_ref(radius,m_radius);
  ReadOnlyReal8_ref(maxNr,m_maxNr);

  std::vector<geo::IdiPoint<geo::CellLoc> > points;

   for(geo::CellLocVisitor c(mask); c.valid(); ++c) {
     REAL8 inpVal;
     if ( input.get(inpVal, *c))
      points.push_back(geo::IdiPoint<geo::CellLoc>(*c,inpVal));
  }
  size_t nrPoints = points.size();

  // if no points at all then all MV
  if (!nrPoints) {
     // no points; all mv
     result.putAllMV();
     return 0;
  }

  // maxNrP == 0 and radius == 0
  // try interpolation with all points, no sorting
  // required; do not use a radius or maxNr of points
  if ( (!maxNr.spatial()) && (!radius.spatial())) {
      double radVal=radius.value(0,0);
      double v=maxNr.value(0,0);
      size_t maxNrP = static_cast<size_t>(v <= 0 ? 0 : v);

     if ( (!maxNrP) && (radVal <= 0)) {
       com::auto_array_ptr<double>dist(new double[nrPoints]);
       // both 0 means interpolate on all
       for(geo::CellLocVisitor c(mask); c.valid(); ++c) {
         UINT1 maskVal;
         REAL8 idpVal;
         if ( mask.get(maskVal, *c) && idp.get(idpVal, *c)
              && maskVal /* == 1 garantueed by boolean type */)
            result.put(interpolateNoSort(points,idpVal,*c),*c);
         else
            result.putMV(*c);
       }
       return 0;
     }
   }

   // generic case
   for(geo::CellLocVisitor c(mask); c.valid(); ++c) {
      UINT1 maskVal;
      REAL8 idpVal, radVal, f_maxNr;
      if ( mask.get(maskVal, *c) && idp.get(idpVal, *c)
          && maxNr.get(f_maxNr, *c) && radius.get(radVal, *c)
          && maskVal /* == 1 garantueed by boolean type */
          && f_maxNr >=  0 ) {
            size_t maxNr = static_cast<size_t>(f_maxNr);
            if (maxNr)
              maxNr = MIN(maxNr, nrPoints);
            else
              maxNr = nrPoints;
            double v;
            if (geo::idi(v,points,idpVal,maxNr,radVal / Side(), *c))
                   result.put(v, *c);
            else
                   result.putMV(*c);
         } else
            result.putMV(*c);
   }
   return 0;
}
