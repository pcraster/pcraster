#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC
#include "calc.h"
#define INCLUDED_CALC
#endif

// Library headers.
#include <utility>
#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif
#ifndef INCLUDED_BOOST_MATH_SPECIAL_FUNCTIONS_ROUND
#include <boost/math/special_functions/round.hpp>
#define INCLUDED_BOOST_MATH_SPECIAL_FUNCTIONS_ROUND
#endif
#ifndef INCLUDED_BOOST_MATH_TR1
#include <boost/math/tr1.hpp>
#define INCLUDED_BOOST_MATH_TR1
#endif


// PCRaster library headers.

#ifndef INCLUDED_MISC
#include "misc.h"
#define INCLUDED_MISC
#endif

#ifndef INCLUDED_COM_MATH
#include "com_math.h"
#define INCLUDED_COM_MATH
#endif
#ifndef INCLUDED_COM_INTERVALTYPES
#include "com_intervaltypes.h"
#define INCLUDED_COM_INTERVALTYPES
#endif

#ifndef INCLUDED_GEO_CELLLOCVISITOR
#include "geo_celllocvisitor.h"
#define INCLUDED_GEO_CELLLOCVISITOR
#endif

#ifndef INCLUDED_GEO_SCANCONVERSION
#include "geo_scanconversion.h"
#define INCLUDED_GEO_SCANCONVERSION
#endif

#ifndef INCLUDED_FIELDAPI_INTERFACE
#include "fieldapi_interface.h"
#define INCLUDED_FIELDAPI_INTERFACE
#endif

#ifndef INCLUDED_FIELDAPI_SCALARDOMAINCHECK
#include "fieldapi_scalardomaincheck.h"
#define INCLUDED_FIELDAPI_SCALARDOMAINCHECK
#endif

// Module headers.



/*!
  \file
  This file contains the implementation of the ExtentOfView class.
*/



template<class T>
class RememberPoints: public boost::noncopyable
{

private:

  const fieldapi::ReadOnly<INT4>& d_classes;
  INT4 d_class;
  std::vector<std::pair<T, T> > d_points;

public:

  RememberPoints(const fieldapi::ReadOnly<INT4>& classes)
    : d_classes(classes)
  {
  }

  RememberPoints(const fieldapi::ReadOnly<INT4>& classes, INT4 currentClass)
    : d_classes(classes), d_class(currentClass)
  {
  }

  void setClass(INT4 currentClass) {
    d_class = currentClass;
  }

  bool operator()(T x, T y) {
    static INT4 value;
    if(d_classes.get(value, y, x) && value == d_class) {
      d_points.push_back(std::make_pair(y, x));
      return true;
    }

    return false;
  }

  std::pair<T, T>& operator[](size_t i) {
    return d_points[i];
  }

  void clear() {
    d_points.clear();
  }

  size_t size() {
    return d_points.size();
  }

  // Distance in cells.
  double distance() {
    double distance = 0.0;

    if(size()) {
      const std::pair<T, T>& begin = d_points.front();
      const std::pair<T, T>& end = d_points.back();
      double dx = static_cast<double>(end.first - begin.first);
      double dy = static_cast<double>(end.second - begin.second);
      // Add one for current cell.
      distance = boost::math::tr1::hypot(dx,dy) + 1.0;
    }

    return distance;
  }

};



extern "C" int ExtentOfView(
  MAP_REAL8 *m_result,                  // scalar, average extent of view
  const MAP_INT4* m_classes,            // nominal, classes
  const MAP_REAL8* m_nrDirections)      // scalar, number of directions
{
  ReadWriteReal8_ref(result, m_result);

  std::vector<const fieldapi::Common*> inputs;
  ReadOnlyInt4_ref(classes, m_classes);
  inputs.push_back(&classes);
  ReadOnlyReal8_ref(nrDirectionsInterface, m_nrDirections);
  inputs.push_back(&nrDirectionsInterface);

  PRECOND(classes.spatial());
  PRECOND(!nrDirectionsInterface.spatial());
  size_t nrDirections = static_cast<size_t>(nrDirectionsInterface.value(0,0));

  std::vector<fieldapi::ScalarDomainCheck> nsDomains;
  nsDomains.push_back(fieldapi::ScalarDomainCheck(nrDirectionsInterface,
        "Number of directions", com::GreaterThan<double>(0)));
  int nsCheck = fieldapi::checkScalarDomains(nsDomains,geo::CellLoc(0,0));
  if(nsCheck != -1) {
    return RetError(1, nsDomains[nsCheck].msg().c_str());
  }

  size_t nrRows = classes.nrRows();
  size_t nrCols = classes.nrCols();

  // Result is missing value if any of the inputs is.
  for(geo::CellLocVisitor visitor(classes); visitor.valid(); ++visitor) {

    if(fieldapi::nonMV(inputs, *visitor)) {
       result.put(0.0, *visitor);
    }
    else {
      result.putMV(*visitor);
    }
  }

  // Determine all direction angles.
  // Determine max distance in cells in the raster.
  double maxExtent = MAX(nrRows,nrCols);
  int offsetX = com::ceil<int, double>(boost::math::tr1::hypot(maxExtent,maxExtent));
  int offsetY;
  double subAngle = 360.0 / nrDirections;
  double angle;
  typedef std::pair<int, int> Offset;
  typedef std::vector<std::pair<double, Offset> > Offsets;
  Offsets offsets;

  for(size_t direction = 0; direction < nrDirections; ++direction) {

    angle = direction * subAngle;
    PRECOND(angle >= 0 && angle < 360);

    if(angle == 90.0) {
      offsetX = 0;
      offsetY = 100;
    }
    else if(angle == 270.0) {
      offsetX = 0;
      offsetY = -100;
    }
    else {
      if(angle < 90 || angle > 270) {
        offsetX = 100;
      }
      else {
        offsetX = -100;
      }

      angle *= M_PI / 180;
      offsetY = boost::math::iround(std::tan(angle) * offsetX);
    }
    offsets.push_back(std::make_pair(angle,
         std::make_pair(offsetX, offsetY)));
  }

  int x, y;
  double sum;
  RememberPoints<int> points(classes);

  // Loop over each cell.
  for(geo::CellLocVisitor visitor(classes); visitor.valid(); ++visitor) {
    x = (*visitor).col();
    y = (*visitor).row();
    sum = 0.0;

    // Determine class.
    if(result.isMV(*visitor)) {
      continue;
    }

    points.setClass(classes[*visitor]);

    // Loop over each direction.
    for(Offsets::const_iterator it = offsets.begin(); it != offsets.end();
         ++it) {

      points.clear();
      POSTCOND(!points.size());

      // Determine number of cells with same class.
      PRECOND(!classes.isMV(*visitor));
      geo::midpointLine(x, y, x + (*it).second.first, y + (*it).second.second,
         points);
      POSTCOND(points.size());

      sum += points.distance();
    }

    // Write sum to cell.
    // Check whether --unitcell or --unittrue is set.
    // unittrue: area is computed in true area represented by cells (default)
    // unitcell: area is computed in number of cells
    result.put(Side() * sum, *visitor);
  }

  return 0;
}
