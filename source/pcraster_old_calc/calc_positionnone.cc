#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_POSITIONNONE
#include "calc_positionnone.h"
#define INCLUDED_CALC_POSITIONNONE
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_POSEXCEPTION
#include "calc_posexception.h"
#define INCLUDED_CALC_POSEXCEPTION
#endif



/*!
  \file
  This file contains the implementation of the PositionNone class.
*/

//------------------------------------------------------------------------------
// DEFINITION OF STATIC POSITIONNONE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF POSITIONNONE MEMBERS
//------------------------------------------------------------------------------

calc::PositionNone::PositionNone()
{
}

calc::PositionNone::PositionNone(const std::string& context):
  d_context(context)
{
}

calc::PositionNone::~PositionNone()
{
}

calc::PositionNone* calc::PositionNone::createClone() const
{
 return new PositionNone(*this);
}

void calc::PositionNone::throwError(const std::string& msg) const
{
  throw PosException(d_context+": "+msg);
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



