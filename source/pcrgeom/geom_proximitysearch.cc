#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEOM_PROXIMITYSEARCH
#include "geom_proximitysearch.h"
#define INCLUDED_GEOM_PROXIMITYSEARCH
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_LIMITS
#include <limits>
#define INCLUDED_LIMITS
#endif



/*!
  \file
  This file contains the implementation of the ProximitySearch class.
*/



//------------------------------------------------------------------------------

/*
namespace geom {

class ProximitySearchPrivate
{
public:

  ProximitySearchPrivate()
  {
  }

  ~ProximitySearchPrivate()
  {
  }

};

} // namespace geom
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC PROXIMITYSEARCH MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF PROXIMITYSEARCH MEMBERS
//------------------------------------------------------------------------------

void geom::ProximitySearch::init()
{
  d_minNr =0;
  d_maxNr =std::numeric_limits<size_t>::max();
  d_radius=-1;
  d_square=false;
}

geom::ProximitySearch::ProximitySearch(double radius)
{
  init();
  PRECOND(radius >= 0);
  d_radius=radius;
}



geom::ProximitySearch::~ProximitySearch()
{
}

/* NOT IMPLEMENTED
//! Assignment operator.
geom::ProximitySearch& geom::ProximitySearch::operator=(const ProximitySearch& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
geom::ProximitySearch::ProximitySearch(const ProximitySearch& rhs)
{
}
*/

void geom::ProximitySearch::setSquared(bool isSquare)
{
  d_square=isSquare;
}

void geom::ProximitySearch::setRadius(double radius)
{
  d_radius=radius;
}

void geom::ProximitySearch::setMinNr(size_t minNr)
{
  d_minNr=minNr;
}
void geom::ProximitySearch::setMaxNr(size_t maxNr)
{
  d_maxNr=maxNr;
}

size_t geom::ProximitySearch::minNr()          const
{
  return d_minNr;
}
size_t geom::ProximitySearch::maxNr()          const
{
  return d_maxNr;
}

//! the maximum nr of point is set
bool   geom::ProximitySearch::maxNrIsBounded() const
{
  return d_maxNr!=std::numeric_limits<size_t>::max();
}
bool   geom::ProximitySearch::circularRadius() const
{
  return !d_square;
}
bool   geom::ProximitySearch::squaredRadius()  const
{
  return  d_square;
}
double geom::ProximitySearch::radius()         const
{
  return d_radius;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



