#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_ZEROMAP
#include "calc_zeromap.h"
#define INCLUDED_CALC_ZEROMAP
#endif

// Library headers.

#ifndef INCLUDED_CSTRING
#include <cstring> // memset
#define INCLUDED_CSTRING
#endif
// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_MAP2CSF
#include "calc_map2csf.h"
#define INCLUDED_CALC_MAP2CSF
#endif



/*!
  \file
  This file contains the implementation of the ZeroMap class.
*/

//------------------------------------------------------------------------------
// DEFINITION OF STATIC ZEROMAP MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF ZEROMAP MEMBERS
//------------------------------------------------------------------------------

calc::ZeroMap::ZeroMap(const Spatial *f):
 Spatial(f->vs(),f->nrValues(), false)
{
}

calc::ZeroMap::~ZeroMap()
{
}

void calc::ZeroMap::loadExternal() const
{
  if (valuePtr())
    return;
  allocate();
  std::memset(valuePtr(),0,valLen());
}

calc::Spatial *calc::ZeroMap::copy() const
{
 if (valuePtr())
   return Spatial::copy();

 // call ZeroMap(const Spatial *f) ctor
 return new ZeroMap(this);
}

void calc::ZeroMap::analyzeBoolean(bool& noneAreTrue,bool& noneAreFalse) const
{
 if (valuePtr()) {
  Spatial::analyzeBoolean(noneAreTrue,noneAreFalse);
 } else {
  noneAreTrue = true;
  noneAreFalse = false;
 }
}

bool calc::ZeroMap::getCell(double& value, size_t i) const
{
 if (valuePtr())
   return Spatial::getCell(value,i);
 value = 0;
 return true;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
