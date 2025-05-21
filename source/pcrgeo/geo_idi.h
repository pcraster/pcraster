#ifndef INCLUDED_GEO_IDI
#define INCLUDED_GEO_IDI

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_GEO_CELLLOC
#include "geo_cellloc.h"
#define INCLUDED_GEO_CELLLOC
#endif

#include <cmath>
#include <vector>


namespace geo {
  // Idi declarations.
}



namespace geo {

template<class Point>
class IdiPoint {
private:
  Point d_c;
  float d_val;
public:
      IdiPoint(const Point& c, double val):
                 d_c(c), d_val(POSSIBLE_DATA_LOSS(float,val)) {}
      //! squared distance
      double distSqr(const Point& c) const {
           double x = static_cast<double>(d_c.x())-c.x();
           double y = static_cast<double>(d_c.y())-c.y();
           return (x*x+y*y);
       }
      double value() const {
         return d_val;
      }
      bool   isThisLocation(const Point& c) const {
         return d_c == c;
      }
};



template<class Point>
class ComputeValue
{
private:
   // point where value is for computed
   const Point d_loc;
   double d_idp;
   long double d_sumDist, d_sumDV;
public:
   ComputeValue(const Point& loc,double idp) :
    d_loc(loc),d_idp(idp),d_sumDist(0),d_sumDV(0) {
   }
   ComputeValue(const ComputeValue&) = delete;

   ComputeValue& operator=(const ComputeValue&) = delete;

   void add(const IdiPoint<Point>& p) {
     double dist = NAN;
     if (d_idp == 2) {
      // 1/f = pow(f,-1) = -1 = -2/2
      dist= 1/(p.distSqr(d_loc));
     } else {
      // -idp/2 since distSqr = d^2 = (d^2)^0.5^-idp = distSqr-idp/2
      dist= pow(p.distSqr(d_loc),-d_idp/2);
    }
     d_sumDist += dist;
     d_sumDV += dist*p.value();
  }

  double value() const {
     return (double)(d_sumDV/d_sumDist);
      // if not stable enough then do:
      //  sumDV=0;
      //  for(size_t i=0; i < n; i++)
      //   sumDV += (dist[i]/sumDist)*points[i].value();
      //  return sumDV;
      // requires an array of dist of course
  }
};



template<class Point>
bool     idi                           (
                                  double& value,
                                  const std::vector<IdiPoint<Point> >& points,
                                  double idp,
                                  size_t maxNr,
                                  double radius,
                                  const Point& c);

} // namespace geo

#endif
