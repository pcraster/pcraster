#include "stddefx.h"
#include "calc.h"  // for it's own interface
#include "geo_celllocvisitor.h"
#include "geo_idi.h"
#include "com_autoarrayptr.h"
#include "fieldapi_interface.h"

#include <vector>
#include <algorithm>
#include <cmath>

template <class Point>
static double interpolateNoSort(const std::vector<geo::IdiPoint<Point>> &points, double idp,
                                const Point &c)
{
  // see PAB
  size_t const nrPoints = points.size();
  geo::ComputeValue<Point> cv(c, idp);
  for (size_t i = 0; i < nrPoints; i++) {
    if (points[i].isThisLocation(c))  // on exact location
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
extern "C" int Idi(MAP_REAL8 *m_resultMap,    /* write-only output map  */
                   const MAP_UINT1 *m_mask,   /* points to be spread */
                   const MAP_REAL8 *m_input,  /* initial costs */
                   const MAP_REAL8 *m_idp,    /* idi pow */
                   const MAP_REAL8 *m_radius, /* max search radius (unit cell) */
                   const MAP_REAL8 *m_maxNr)  /* max. nr points */
{
  ReadWriteReal8_ref(result, m_resultMap);
  ReadOnlyUint1_ref(mask, m_mask);
  ReadOnlyReal8_ref(input, m_input);
  ReadOnlyReal8_ref(idp, m_idp);
  ReadOnlyReal8_ref(radius, m_radius);
  ReadOnlyReal8_ref(maxNr, m_maxNr);

  std::vector<geo::IdiPoint<geo::CellLoc>> points;

  for (geo::CellLocVisitor c(mask); c.valid(); ++c) {
    REAL8 inpVal = NAN;
    if (input.get(inpVal, *c))
      points.push_back(geo::IdiPoint<geo::CellLoc>(*c, inpVal));
  }
  size_t const nrPoints = points.size();

  // if no points at all then all MV
  if (!nrPoints) {
    // no points; all mv
    result.putAllMV();
    return 0;
  }

  // maxNrP == 0 and radius == 0
  // try interpolation with all points, no sorting
  // required; do not use a radius or maxNr of points
  if ((!maxNr.spatial()) && (!radius.spatial())) {
    double const radVal = radius.value(0, 0);
    double const v = maxNr.value(0, 0);
    auto maxNrP = static_cast<size_t>(v <= 0 ? 0 : v);

    if ((!maxNrP) && (radVal <= 0)) {
      com::auto_array_ptr<double> const dist(new double[nrPoints]);
      // both 0 means interpolate on all
      for (geo::CellLocVisitor c(mask); c.valid(); ++c) {
        UINT1 maskVal = 0;
        REAL8 idpVal = NAN;
        if (mask.get(maskVal, *c) && idp.get(idpVal, *c) &&
            maskVal /* == 1 garantueed by boolean type */)
          result.put(interpolateNoSort(points, idpVal, *c), *c);
        else
          result.putMV(*c);
      }
      return 0;
    }
  }

  // generic case
  for (geo::CellLocVisitor c(mask); c.valid(); ++c) {
    UINT1 maskVal = 0;
    REAL8 idpVal = NAN;
    REAL8 radVal = NAN;
    REAL8 f_maxNr = NAN;
    if (mask.get(maskVal, *c) && idp.get(idpVal, *c) && maxNr.get(f_maxNr, *c) &&
        radius.get(radVal, *c) && maskVal /* == 1 garantueed by boolean type */
        && f_maxNr >= 0) {
      auto maxNr = static_cast<size_t>(f_maxNr);
      if (maxNr)
        maxNr = std::min(maxNr, nrPoints);
      else
        maxNr = nrPoints;
      double v = NAN;
      if (geo::idi(v, points, idpVal, maxNr, radVal / Side(), *c))
        result.put(v, *c);
      else
        result.putMV(*c);
    } else
      result.putMV(*c);
  }
  return 0;
}
