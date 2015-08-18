#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_CIRCULARNEIGHBOURHOOD
#include "geo_circularneighbourhood.h"
#define INCLUDED_GEO_CIRCULARNEIGHBOURHOOD
#endif

// Library headers.
#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif
#ifndef INCLUDED_BOOST_MATH_SPECIAL_FUNCTIONS_ROUND
#include <boost/math/special_functions/round.hpp>
#define INCLUDED_BOOST_MATH_SPECIAL_FUNCTIONS_ROUND
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_GEO_SCANCONVERSION
#include "geo_scanconversion.h"
#define INCLUDED_GEO_SCANCONVERSION
#endif



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



geo::CircularNeighbourhood::CircularNeighbourhood(double fromRadius,
         double toRadius)

  : Neighbourhood(fromRadius, toRadius)

{
  init();
}



geo::CircularNeighbourhood::~CircularNeighbourhood()
{
}



namespace geo {

class SetRaster: public boost::noncopyable {

private:

  SimpleRaster<double>& d_raster;

public:

  SetRaster(SimpleRaster<double>& raster)
    : d_raster(raster)
  {
  }

  bool operator()(size_t col, size_t row) {
    d_raster.cell(row, col) = 1.0;
    return true;
  }
};

}



void geo::CircularNeighbourhood::init()
{
  SetRaster setRaster(*this);
  using namespace boost::math;

  if(isOutline()) {
    midpointCircle<int, SetRaster>(
         static_cast<size_t>(round(toRadius())),
         static_cast<size_t>(round(toRadius())),
         static_cast<size_t>(round(toRadius())),
         setRaster);
  }
  else {
    midpointCircle<int, SetRaster>(
         static_cast<size_t>(round(toRadius())),
         static_cast<size_t>(round(toRadius())),
         static_cast<size_t>(round(fromRadius())),
         static_cast<size_t>(round(toRadius())),
         setRaster);
  }
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



