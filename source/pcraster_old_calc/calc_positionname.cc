#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_POSITIONNAME
#include "calc_positionname.h"
#define INCLUDED_CALC_POSITIONNAME
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif

// Module headers.
#ifndef INCLUDED_CALC_POSEXCEPTION
#include "calc_posexception.h"
#define INCLUDED_CALC_POSEXCEPTION
#endif



/*!
  \file
  This file contains the implementation of the PositionName class.
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC POSITIONNAME MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF POSITIONNAME MEMBERS
//------------------------------------------------------------------------------

calc::PositionName::PositionName(
    StringSharedPtr name):
  d_name(name)
{
}

calc::PositionName::PositionName(const PositionName& pt):
 Position(pt),
 d_name(pt.d_name)
{
}

calc::PositionName::~PositionName()
{
}

//! generate error message that is prefixed with the position
/*!
   \throws calc::PosException
 */
void calc::PositionName::throwError(const std::string& inMsg) const
{
  // clean up and make sure it ends with a new line
  std::string msg(inMsg);
  com::removeFrontEndSpace(msg);
  msg+='\n';

  throw PosException(*d_name+": "+msg);
}

calc::PositionName*  calc::PositionName::createClone() const
{
  return new PositionName(*this);
}

//! return format: "d_name"
std::string calc::PositionName::text() const
{
  return *d_name;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



