#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_RIKSNEIGHBOURHOOD
#include "geo_riksneighbourhood.h"
#define INCLUDED_GEO_RIKSNEIGHBOURHOOD
#endif

// Library headers.

#ifndef INCLUDED_BOOST_MATH_TR1
#include <boost/math/tr1.hpp>
#define INCLUDED_BOOST_MATH_TR1
#endif
using namespace boost::math; // use then tr1

// PCRaster library headers.
#ifndef INCLUDED_COM_MATH
#include "com_math.h"
#define INCLUDED_COM_MATH
#endif

// Module headers.



/*!
  \file
  This file contains the implementation of the RiksNeighbourhood class.
*/



//------------------------------------------------------------------------------

/*
namespace geo {

class RiksNeighbourhoodPrivate
{
public:

  RiksNeighbourhoodPrivate()
  {
  }

  ~RiksNeighbourhoodPrivate()
  {
  }

};

} // namespace geo
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC RIKSNEIGHBOURHOOD MEMBERS
//------------------------------------------------------------------------------

boost::tuple<size_t, size_t> geo::RiksNeighbourhood::circleCell(double radius)
{
  PRECOND(radius > 0.0);

  boost::tuple<size_t, size_t> cell;
  double currentDifference(0), difference(0);

  // Determine max radius of raster which can contain a circle with given
  // radius.
  size_t maxRadius = static_cast<size_t>(std::ceil(radius));

  // Only handle one quarter of the circle.
  for(size_t row = 0; row <= maxRadius; ++row) {
    for(size_t col = 0; col <= maxRadius; ++col) {

      if(row == 0 && col == 0) {
        // First cell handled.
        difference = std::abs(radius - tr1::hypot<double>(row, col));
        cell = boost::make_tuple(row, col);
      }
      else {
        currentDifference = std::abs(radius - tr1::hypot<double>(row, col));
        if(currentDifference < difference) {
          difference = currentDifference;
          cell = boost::make_tuple(row, col);
        }
      }
    }
  }

  return cell;
}



//------------------------------------------------------------------------------
// DEFINITION OF RIKSNEIGHBOURHOOD MEMBERS
//------------------------------------------------------------------------------

geo::RiksNeighbourhood::RiksNeighbourhood(double toRadius)

  : Neighbourhood(toRadius)

{
  init();
}



geo::RiksNeighbourhood::RiksNeighbourhood(double fromRadius, double toRadius)

  : Neighbourhood(fromRadius, toRadius)

{
  init();
}



geo::RiksNeighbourhood::~RiksNeighbourhood()
{
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
void geo::RiksNeighbourhood::init()
{
  // Determine which cells match the given radiusses best. These are the cells
  // who's centers are closest to the circle defined by the radiusses.
  boost::tuple<size_t, size_t> fromCircleCell = boost::make_tuple(0, 0);
  boost::tuple<size_t, size_t> toCircleCell(fromCircleCell);

  if(fromRadius() > 0.0) {
    fromCircleCell = circleCell(fromRadius());
  }

  if(toRadius() > 0.0) {
    toCircleCell = circleCell(toRadius());
  }

  // Determine the radiusses of the Riks neighbourhoods of which the selected
  // cells are part.
  double fromRadius = tr1::hypot<double>(
         fromCircleCell.get<0>(), fromCircleCell.get<1>());
  double toRadius = tr1::hypot<double>(
         toCircleCell.get<0>(), toCircleCell.get<1>());

  // Make sure the current set radius is equal of larger than the selected one.
  POSTCOND(static_cast<double>(radius()) >= toRadius);

  // Determine which cells are part of the Riks neighbourhood with the
  // selected radius.
  size_t offset = radius();
  for(size_t row = 0; row <= radius(); ++row) {
    for(size_t col = 0; col <= radius(); ++col) {
      double radius = tr1::hypot<double>(row, col);
      if((radius > fromRadius && radius < toRadius) ||
          com::equal_epsilon(radius, fromRadius) ||
          com::equal_epsilon(radius, toRadius)) {
        this->cell(offset + row, offset + col) = 1.0;
        this->cell(offset + row, offset - col) = 1.0;
        this->cell(offset - row, offset + col) = 1.0;
        this->cell(offset - row, offset - col) = 1.0;
      }
    }
  }
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



