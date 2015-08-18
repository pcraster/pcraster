#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_MOORENEIGHBOURHOOD
#include "geo_mooreneighbourhood.h"
#define INCLUDED_GEO_MOORENEIGHBOURHOOD
#endif

// Library headers.
#ifndef INCLUDED_BOOST_MATH_SPECIAL_FUNCTIONS_ROUND
#include <boost/math/special_functions/round.hpp>
#define INCLUDED_BOOST_MATH_SPECIAL_FUNCTIONS_ROUND
#endif

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the MooreNeighbourhood class.
*/



//------------------------------------------------------------------------------

/*
namespace geo {

class MooreNeighbourhoodPrivate
{
public:

  MooreNeighbourhoodPrivate()
  {
  }

  ~MooreNeighbourhoodPrivate()
  {
  }

};

} // namespace geo
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC MOORENEIGHBOURHOOD MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MOORENEIGHBOURHOOD MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     radius Radius of the neighbourhood.

  Concentric square rings of cells are added to the neighbourhood if the
  following holds for the cell to the west/north/east or south of the center
  cell:

  The center is at a distance <= toRadius of the center of the center cell.

  If you want to exclude the center cell use MooreNeighbourhood(size_t, size_t)
  with fromRadius 1.
*/
geo::MooreNeighbourhood::MooreNeighbourhood(double toRadius)

  : Neighbourhood(toRadius)

{
  init();
}



//! Constructor.
/*!
  \param     fromRadius Radius of inner border of donut.
  \param     toRadius Radius of outer border of donut.

  This constructor constructs a donut shaped neighbourhood in which the inner
  cells are not part of the neighbourhood. With this constructor it is possible
  to create square zones around a center cell.

  Concentric square rings of cells are added to the neighbourhood if the
  following holds for the cell to the west/north/east or south of the center
  cell:

  The center is at a distance >= \a fromRadius and <= toRadius of the center
  of the center cell.
*/
geo::MooreNeighbourhood::MooreNeighbourhood(double fromRadius, double toRadius)

  : Neighbourhood(fromRadius, toRadius)

{
  init();
}



//! Destructor.
/*!
*/
geo::MooreNeighbourhood::~MooreNeighbourhood()
{
}



//! Initialises the object.
/*!
*/
void geo::MooreNeighbourhood::init()
{
  for(size_t radius = static_cast<size_t>(boost::math::round(fromRadius()));
         radius <= static_cast<size_t>(boost::math::round(toRadius())); ++radius) {
    addRing(radius);
  }
}



//! Adds a ring with radius \a radius to the neighbourhood.
/*!
  \param     radius Radius of ring around center cell to add to neighbourhood.
*/
void geo::MooreNeighbourhood::addRing(size_t radius)
{
  PRECOND(radius <= this->radius());

  size_t upperLeft = this->radius() - radius;

  for(size_t row = upperLeft; row < upperLeft + 2 * radius + 1; ++row) {
    cell(row, upperLeft) = 1.0;
    cell(row, upperLeft + 2 * radius) = 1.0;
  }

  for(size_t col = upperLeft + 1; col < upperLeft + 2 * radius; ++col) {
    cell(upperLeft, col) = 1.0;
    cell(upperLeft + 2 * radius, col) = 1.0;
  }
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
