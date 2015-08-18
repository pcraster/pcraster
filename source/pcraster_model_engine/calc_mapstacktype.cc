#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_MAPSTACKTYPE
#include "calc_mapstacktype.h"
#define INCLUDED_CALC_MAPSTACKTYPE
#endif

// Library headers.
#ifndef INCLUDED_BOOST_FORMAT
#include <boost/format.hpp>
#define INCLUDED_BOOST_FORMAT
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_DATATYPECLASH
#include "calc_datatypeclash.h"
#define INCLUDED_CALC_DATATYPECLASH
#endif


#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

/*!
  \file
  This file contains the implementation of the MapStackType class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class MapStackTypePrivate
{
public:

  MapStackTypePrivate()
  {
  }

  ~MapStackTypePrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC MAPSTACKTYPE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MAPSTACKTYPE MEMBERS
//------------------------------------------------------------------------------

namespace calc {

MapStackType::MapStackType():
  d_use(Unknown),
  d_highestTimestepAvailable(0)
{
}



/* DEFAULT
//! Copy constructor.
MapStackType::MapStackType(
         MapStackType const& rhs)

  : Base(rhs)

{
}
*/



MapStackType::~MapStackType()
{
}



/*  DEFAULT
//! Assignment operator.
MapStackType& MapStackType::operator=(
         MapStackType const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

//! set value of d_highestTimestepAvailable
void MapStackType::setHighestTimestepAvailable(size_t highestTimestepAvailable)
{
  d_highestTimestepAvailable=highestTimestepAvailable;
}

//! update for use
void MapStackType::update(const MapStackType& u)
{
  switch(u.use()) {
   case Unknown: return;
   case Modulo:
               setHighestTimestepAvailable(u.highestTimestepAvailable());
               // fall trough;
   default:    setUse(u.use());
 }
}

//! get value of d_highestTimestepAvailable
size_t MapStackType::highestTimestepAvailable() const
{
  return d_highestTimestepAvailable;
}

//! set value of d_use
/*! \throws MapStackClash if already in other use
 */
void MapStackType::setUse(Use use)
{
  if (d_use == Unknown || d_use == use)
    d_use=use;
  else {
    throw MapStackClash((boost::format(
     "can not apply both %1% and %2% to same mapstack")
                % operationName(d_use) % operationName(use)).str());
  }
}

//! get value of d_use
MapStackType::Use MapStackType::use() const
{
  return d_use;
}

std::string MapStackType::operationName(Use u)
{
  switch(u) {
    case Unknown: break;
    case Full:
      return "timeinput";
    case Sparse:
      return "timeinputsparse";
    case Modulo:
      return "timeinputmodulo";
    case Lookup:
      return "lookupmapstack";
  }
  DEVELOP_PRECOND(false);
  return "timeinput?";
}


} // namespace calc

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



