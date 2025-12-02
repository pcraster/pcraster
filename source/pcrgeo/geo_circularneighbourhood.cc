#include "stddefx.h"
#include "geo_circularneighbourhood.h"
#include "geo_scanconversion.h"

#include <boost/math/special_functions/round.hpp>

/*!
  \file
  This file contains the implementation of the CircularNeighbourhood class.
*/


//------------------------------------------------------------------------------

/*
namespace geo {

class CircularNeighbourhoodPrivate
{
public:

  CircularNeighbourhoodPrivate()
  {
  }

  ~CircularNeighbourhoodPrivate()
  {
  }

};

} // namespace geo
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC CIRCULARNEIGHBOURHOOD MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF CIRCULARNEIGHBOURHOOD MEMBERS
//------------------------------------------------------------------------------

geo::CircularNeighbourhood::CircularNeighbourhood(double radius)

    : Neighbourhood(radius)

{
  init();
}

geo::CircularNeighbourhood::CircularNeighbourhood(double fromRadius, double toRadius)

    : Neighbourhood(fromRadius, toRadius)

{
  init();
}

geo::CircularNeighbourhood::~CircularNeighbourhood()
{
}

namespace geo
{

class SetRaster
{

private:
  SimpleRaster<double> &d_raster;

public:
  SetRaster(SimpleRaster<double> &raster) : d_raster(raster)
  {
  }

  SetRaster(const SetRaster &other) = delete;

  SetRaster &operator=(const SetRaster &other) = delete;

  bool operator()(size_t col, size_t row)
  {
    d_raster.cell(row, col) = 1.0;
    return true;
  }
};

}  // namespace geo

void geo::CircularNeighbourhood::init()
{
  SetRaster setRaster(*this);
  using namespace boost::math;

  if (isOutline()) {
    midpointCircle<int, SetRaster>(static_cast<size_t>(round(toRadius())),
                                   static_cast<size_t>(round(toRadius())),
                                   static_cast<size_t>(round(toRadius())), setRaster);
  } else {
    midpointCircle<int, SetRaster>(
        static_cast<size_t>(round(toRadius())), static_cast<size_t>(round(toRadius())),
        static_cast<size_t>(round(fromRadius())), static_cast<size_t>(round(toRadius())), setRaster);
  }
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
