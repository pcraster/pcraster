#include "stddefx.h"
#include "calc_zeromap.h"
#include "calc_map2csf.h"

#include <cstring>  // memset

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

calc::ZeroMap::ZeroMap(const Spatial *f) : Spatial(f->vs(), f->nrValues(), false)
{
}

calc::ZeroMap::~ZeroMap()
{
}

void calc::ZeroMap::loadExternal() const
{
  if (valuePtr() != nullptr) {
    return;
  }
  allocate();
  std::memset(valuePtr(), 0, valLen());
}

calc::Spatial *calc::ZeroMap::copy() const
{
  if (valuePtr() != nullptr) {
    return Spatial::copy();
  }

  // call ZeroMap(const Spatial *f) ctor
  return new ZeroMap(this);
}

void calc::ZeroMap::analyzeBoolean(bool &noneAreTrue, bool &noneAreFalse) const
{
  if (valuePtr() != nullptr) {
    Spatial::analyzeBoolean(noneAreTrue, noneAreFalse);
  } else {
    noneAreTrue = true;
    noneAreFalse = false;
  }
}

bool calc::ZeroMap::getCell(double &value, size_t i) const
{
  if (valuePtr() != nullptr) {
    return Spatial::getCell(value, i);
  }
  value = 0;
  return true;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
