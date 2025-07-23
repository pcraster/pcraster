#include "stddefx.h"
#include "calc_mapstacktype.h"
#include "calc_datatypeclash.h"

#include <iostream>
#include <format>

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

MapStackType::MapStackType()

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
               [[fallthrough]];
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
    auto const f1 = operationName(d_use);
    auto const f2 = operationName(use);
    throw MapStackClash(std::vformat(
     "can not apply both {0} and {1} to same mapstack",
          std::make_format_args(f1, f2)));
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



