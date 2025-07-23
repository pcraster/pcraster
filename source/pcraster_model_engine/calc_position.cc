#include "stddefx.h"
#include "calc_position.h"



/*!
  \file
  This file contains the implementation of the Position class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class PositionPrivate
{
public:

  PositionPrivate()
  {
  }

  ~PositionPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC POSITION MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF POSITION MEMBERS
//------------------------------------------------------------------------------

calc::Position::Position()
  
{
}

calc::Position::~Position()
{
}

void calc::Position::throwError(const std::ostringstream& msg) const
{
  throwError(msg.str());
}

/*
//! only used for error/descriptive  messages
std::string  calc::Position::shortText() const
{
  return "";
}
*/

int calc::Position::priority() const
{
  return d_priority;
}

void calc::Position::setPriority(int priority)
{
  d_priority=priority;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



