#ifndef INCLUDED_GEO_ALGORITHM
#define INCLUDED_GEO_ALGORITHM



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif

#ifndef INCLUDED_ITERATOR
#include <iterator>
#define INCLUDED_ITERATOR
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// PCRaster library headers.

// Module headers.



namespace geo {
  // Algorithm declarations.
}



namespace geo {

//! Determines which points are within \a radius around \a point.
/*!
  \param     point Location to find a value for.
  \param     radius Radius of area to search in.
  \param     sourceBegin Iterator to first point value.
  \param     sourceEnd Iterator to end point value.
  \param     sink Output iterator to write point values to.

  If \a radius is 0 all points will be searched.
*/
template<class Point, class Radius, class InputIterator, class OutputIterator>
void pointsInArea(Point const& point, Radius radius,
         InputIterator sourceBegin, InputIterator sourceEnd,
         OutputIterator sink)
{
  if(radius == 0) {
    std::copy(sourceBegin, sourceEnd, sink);
  }
  else {
    Radius squaredRadius = radius * radius;

    while(sourceBegin != sourceEnd) {
      if(point.squaredDistance((*sourceBegin).point()) <= squaredRadius) {
        *sink = *sourceBegin;
        ++sink;
      }

      ++sourceBegin;
    }
  }
}

//! Returns the maximum \a value of point values within an area with radius \a radius around \a point.
/*!
  \param     value Maximum value.
  \param     point Location to find a value for.
  \param     radius Radius of area to search in.
  \param     sourceBegin Iterator to first point value.
  \param     sourceEnd Iterator to end point value.
  \return    Whether the search was successful.

  If \a radius is 0 all points will be searched.
*/
template<class Value, class Point, class Radius, class InputIterator>
bool maximum(Value& value, Point const& point, Radius radius,
         InputIterator sourceBegin, InputIterator sourceEnd)
{
  typedef typename InputIterator::value_type PointValue;

  // Determine points within the search area.
  std::vector<PointValue> subset;
  pointsInArea(point, radius, sourceBegin, sourceEnd,
         std::back_inserter(subset));

  if(subset.empty()) {
    return false;
  }

  // Determine maximum value.
  typename std::vector<PointValue>::const_iterator it = subset.begin();

  value = (*it).value();

  while(it != subset.end()) {
    value = std::max(value, (*it).value());
    ++it;
  }

  return true;
}



} // namespace geo

#endif
