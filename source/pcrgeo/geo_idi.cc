#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_IDI
#include "geo_idi.h"
#define INCLUDED_GEO_IDI
#endif

// Library headers.
#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif
// PCRaster library headers.
#ifndef INCLUDED_COM_AUTOARRAYPTR
#include "com_autoarrayptr.h"
#define INCLUDED_COM_AUTOARRAYPTR
#endif

// Module headers.
#ifndef INCLUDED_GEO_POINT
#include "geo_point.h"
#define INCLUDED_GEO_POINT
#endif


namespace geo {

class SortKey {
  public:
   size_t pId;
   double distSqr;
};

class CmpSortKey {
 public:
  bool operator()(const SortKey& e1, const SortKey& e2) {
   return e1.distSqr < e2.distSqr;
  };
};

} // namespace geo



template<class Point>
bool geo::idi(
       double& value,
       const std::vector<IdiPoint<Point> >& points,
       double idp,
       size_t maxNr,
       double radius,
       const Point& c)
{
  double cellRadiusSqr = radius * radius;

  bool   allCandidate = cellRadiusSqr == 0;
  size_t nrPoints=points.size();
  com::auto_array_ptr<SortKey>keys(new SortKey[nrPoints]);
  size_t nrKeys=0;
  for(size_t i=0; i < nrPoints; i++) {
   if (points[i].isThisLocation(c)) {
     // on exact location
     value = points[i].value();
     return true;
   }
   keys[nrKeys].distSqr=points[i].distSqr(c);
   if (allCandidate || (keys[nrKeys].distSqr <= cellRadiusSqr)) {
    keys[nrKeys].pId = i;
    nrKeys++; // keep this one; within radius
   }
  }

  if(!nrKeys)
    return false;

  if (nrKeys > maxNr) {
    // more keys candidate than allowed
    // select the maxNr nearest
    SortKey *keyPtr = keys.ptr();
    std::partial_sort(keyPtr,keyPtr+maxNr,keyPtr+nrKeys,CmpSortKey());
#   ifdef DEBUG_DEVELOP
    // check if partial_sort does what it promised
    for(size_t i=1; i < maxNr; i++)
      POSTCOND(keys[i-1].distSqr <= keys[i].distSqr);
    for(size_t i=maxNr; i < nrKeys; i++)
      POSTCOND(keys[maxNr-1].distSqr <= keys[i].distSqr);
#   endif
  }

  ComputeValue<Point> cv(c,idp);
  maxNr = MIN(nrKeys,maxNr);
  for(size_t i=0; i < maxNr; i++)
    cv.add(points[keys[i].pId]);
  value=cv.value();

  return true;
}


// Instantiate the template functions using explicit instantiation declarations.
namespace geo {
template bool idi<CellLoc>(double& value, const std::vector<IdiPoint<CellLoc> >& points, double idp, size_t maxNr, double radius, const CellLoc& c);
template bool idi<Point<double, 2> >(double& value, const std::vector<IdiPoint<Point<double, 2> > >& points, double idp, size_t maxNr, double radius, const Point<double, 2>& c);
}
