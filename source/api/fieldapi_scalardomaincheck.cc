#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_FIELDAPI_SCALARDOMAINCHECK
#include "fieldapi_scalardomaincheck.h"
#define INCLUDED_FIELDAPI_SCALARDOMAINCHECK
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_GEO_CELLLOC
#include "geo_cellloc.h"
#define INCLUDED_GEO_CELLLOC
#endif

// Module headers.
#ifndef INCLUDED_FIELDAPI_INTERFACE
#include "fieldapi_interface.h"
#define INCLUDED_FIELDAPI_INTERFACE
#endif



/*!
  \file
  This file contains the implementation of the ScalarDomainCheck class.
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC SCALARDOMAINCHECK MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF SCALARDOMAINCHECK MEMBERS
//------------------------------------------------------------------------------

//! ctor
fieldapi::ScalarDomainCheck:: ScalarDomainCheck(const ReadOnlyReal8& map,
      const char *parName, const com::IntervalD& v):
      d_map(map),d_parName(parName),d_v(v.createClone())
{
}

//! Assignment constructor.
fieldapi::ScalarDomainCheck& fieldapi::ScalarDomainCheck::operator=(
    const ScalarDomainCheck& s)
{
     if (this != &s) {
        *this = s;
        this->d_v = s.d_v->createClone();
     }
     return *this;
}

//! Copy constructor.
fieldapi::ScalarDomainCheck::ScalarDomainCheck(
   const ScalarDomainCheck& s):
      d_map(s.d_map),d_parName(s.d_parName),d_v(s.d_v->createClone())
{
}

//! dtor
fieldapi::ScalarDomainCheck::~ScalarDomainCheck()
{
  delete d_v;
}

//! check domain range for location \a l
bool fieldapi::ScalarDomainCheck::valid(const geo::CellLoc& l) const
{
  return d_v->valid(d_map[l]);
}

//! return "argument "parName" must be "DomainTest
std::string fieldapi::ScalarDomainCheck::msg() const
{
  std::string s("argument '");
   s+=d_parName;
   s+="' must be ";
   s+=d_v->msg();
  return s;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------






//! check a vector of scalar domains for \a loc
/*!
   \returns -1 if all valid, the index of the first
     one found to be invalid
 */
int fieldapi::checkScalarDomains(
    const std::vector<ScalarDomainCheck>& v,
    const geo::CellLoc& loc)
{
  for(size_t i=0; i < v.size(); i++)
    if (! v[i].valid(loc))
      return i;
  return -1;
}
