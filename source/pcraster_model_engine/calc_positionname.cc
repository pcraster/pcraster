#include "stddefx.h"
#include "calc_positionname.h"
#include "calc_posexception.h"

#include <utility>

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

calc::PositionName::PositionName() : d_name(new std::string(""))
{
}

calc::PositionName::PositionName(StringSharedPtr name) : d_name(std::move(name))
{
}

calc::PositionName::PositionName(const std::string &name) : d_name(new std::string(name))
{
}

calc::PositionName::PositionName(const PositionName &pt) : Position(pt), d_name(pt.d_name)
{
}

calc::PositionName::~PositionName()
{
}

//! generate error message that is prefixed with the position
/*!
   \throws calc::PosException
 */
void calc::PositionName::throwError(const std::string &msg) const
{
  throw PosException(*d_name, msg, false);
}

calc::PositionName *calc::PositionName::createClone() const
{
  return new PositionName(*this);
}

//! return format: "d_name"
std::string calc::PositionName::fullText() const
{
  return *d_name;
}

//! return format: "d_name"
std::string calc::PositionName::shortText() const
{
  return fullText();
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
