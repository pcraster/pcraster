#include "stddefx.h"
#include "geo_riksneighbourhood.h"
#include "com_math.h"



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

std::tuple<size_t, size_t> geo::RiksNeighbourhood::circleCell(double radius)
{
  PRECOND(radius > 0.0);

  std::tuple<size_t, size_t> cell;
  double currentDifference(0);
  double difference(0);

  // Determine max radius of raster which can contain a circle with given
  // radius.
  auto maxRadius = static_cast<size_t>(std::ceil(radius));

  // Only handle one quarter of the circle.
  for(size_t row = 0; row <= maxRadius; ++row) {
    for(size_t col = 0; col <= maxRadius; ++col) {

      if(row == 0 && col == 0) {
        // First cell handled.
        difference = std::abs(radius - std::hypot<double>(row, col));
        cell = std::make_tuple(row, col);
      }
      else {
        currentDifference = std::abs(radius - std::hypot<double>(row, col));
        if(currentDifference < difference) {
          difference = currentDifference;
          cell = std::make_tuple(row, col);
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
  std::tuple<size_t, size_t> fromCircleCell = std::make_tuple(0, 0);
  std::tuple<size_t, size_t> toCircleCell(fromCircleCell);

  if(fromRadius() > 0.0) {
    fromCircleCell = circleCell(fromRadius());
  }

  if(toRadius() > 0.0) {
    toCircleCell = circleCell(toRadius());
  }

  // Determine the radiusses of the Riks neighbourhoods of which the selected
  // cells are part.
  double fromRadius = std::hypot<double>(
         std::get<0>(fromCircleCell), std::get<1>(fromCircleCell));
  double toRadius = std::hypot<double>(
         std::get<0>(toCircleCell), std::get<1>(toCircleCell));

  // Make sure the current set radius is equal of larger than the selected one.
  POSTCOND(static_cast<double>(radius()) >= toRadius);

  // Determine which cells are part of the Riks neighbourhood with the
  // selected radius.
  size_t offset = radius();
  for(size_t row = 0; row <= radius(); ++row) {
    for(size_t col = 0; col <= radius(); ++col) {
      double radius = std::hypot<double>(row, col);
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



