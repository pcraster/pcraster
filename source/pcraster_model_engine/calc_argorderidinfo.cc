#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_ARGORDERIDINFO
#include "calc_argorderidinfo.h"
#define INCLUDED_CALC_ARGORDERIDINFO
#endif

// Library headers.
#ifndef INCLUDED_CMATH
#include <cmath>
#define INCLUDED_CMATH
#endif
#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif
// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the ArgOrderIdInfo class.
*/



namespace calc {

//------------------------------------------------------------------------------

/*
class ArgOrderIdInfoPrivate
{
public:

  ArgOrderIdInfoPrivate()
  {
  }

  ~ArgOrderIdInfoPrivate()
  {
  }

};
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC ARGORDERIDINFO MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF ARGORDERIDINFO MEMBERS
//------------------------------------------------------------------------------

ArgOrderIdInfo::ArgOrderIdInfo(const REAL4* chance,
                               UINT4        id):
  d_chance(chance),
  d_id(id),
  d_areaLimit(0),
  d_orgAreaLimit(0),
  d_areaAssigned(0),
  d_areaTaken(0)
{
}

ArgOrderIdInfo::ArgOrderIdInfo(const REAL4* chance,
                               UINT4        id,
                               double       areaLimit):
  d_chance(chance),
  d_id(id),
  d_areaLimit(0),
  d_orgAreaLimit(0),
  d_areaAssigned(0),
  d_areaTaken(0)
{
  d_areaLimit=static_cast<size_t>(std::floor(std::max<double>(0,areaLimit)));
  d_orgAreaLimit=d_areaLimit;
}



/* DEFAULT
//! Copy constructor.
ArgOrderIdInfo::ArgOrderIdInfo(
         ArgOrderIdInfo const& rhs)

  : Base(rhs)

{
}
*/



ArgOrderIdInfo::~ArgOrderIdInfo()
{
}



/* DEFAULT
//! Assignment operator.
ArgOrderIdInfo& ArgOrderIdInfo::operator=(
         ArgOrderIdInfo const& rhs)
{
  if(this != &rhs) {
  }

  return *this;
}
*/

//! set value of d_id
void ArgOrderIdInfo::setId(UINT4 id)
{
  d_id=id;
}

//! get value of d_id
UINT4 ArgOrderIdInfo::id() const
{
  return d_id;
}

//! increment area assigned
void ArgOrderIdInfo::incrementAreaAssigned()
{
  d_areaAssigned+=1;
}
//! increment area assigned
void ArgOrderIdInfo::incrementAreaTaken()
{
  d_areaTaken+=1;
}

//! get value of d_areaAssigned
size_t ArgOrderIdInfo::areaAssigned() const
{
  return d_areaAssigned;
}
//! get value of d_areaAssigned
size_t ArgOrderIdInfo::areaTaken() const
{
  return d_areaTaken;
}

void ArgOrderIdInfo::resetForSweep()
{
  d_areaLimit=d_orgAreaLimit+d_areaTaken;
  d_areaAssigned=0;
  d_areaTaken=0;
}


//! get value of d_areaLimit
size_t ArgOrderIdInfo::areaLimit() const
{
  return d_areaLimit;
}

REAL4 const* ArgOrderIdInfo::chance() const
{
  return d_chance;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace calc

