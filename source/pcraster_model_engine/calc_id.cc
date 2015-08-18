#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_ID
#include "calc_id.h"
#define INCLUDED_CALC_ID
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_SYMEXCEPTION
#include "calc_symexception.h"
#define INCLUDED_CALC_SYMEXCEPTION
#endif
#ifndef INCLUDED_CALC_POSITIONNAME
#include "calc_positionname.h" // TmpId
#define INCLUDED_CALC_POSITIONNAME
#endif
#ifndef INCLUDED_CALC_POSITION
#include "calc_position.h"
#define INCLUDED_CALC_POSITION
#endif
#ifndef INCLUDED_CALC_QUOTE
#include "calc_quote.h"
#define INCLUDED_CALC_QUOTE
#endif


/*!
  \file
  This file contains the implementation of the Id class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class IdPrivate
{
public:

  IdPrivate()
  {
  }

  ~IdPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC ID MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF ID MEMBERS
//------------------------------------------------------------------------------

calc::TmpId::TmpId(const std::string& name):
  Id()
{
  PositionName pn("<noPosition>");
  setPosition(&pn);
  setName(name);
}

calc::Id::Id():
  d_position(0)
{
}


calc::Id::Id(const std::string &name, const Position *position):
  d_name(name)
{
    d_position=position->createClone();
}

calc::Id::~Id()
{
  delete d_position;
}

//! Assignment operator.
calc::Id& calc::Id::operator=(const Id& rhs)
{
  if (this != &rhs) {
    d_name=rhs.d_name;
    setPosition(rhs.d_position);
  }
  return *this;
}

//! Copy constructor.
calc::Id::Id(const Id& rhs):
  d_name(rhs.d_name),
  d_position(0)
{
  setPosition(rhs.d_position);
}


void  calc::Id::symError(const std::string& msg) const
{
  throw SymException(*position(),name(),msg);
}


//! (re)set value of name
/*!
 * \pre d_position != 0; this is not default constructed
 */
void calc::Id::setName(const std::string& name)
{
  PRECOND(d_position);
  d_name=name;
}

//! set value of position
void calc::Id::setPosition(const Position* position)
{
  delete d_position;
  d_position=0;
  if (position)
    d_position=position->createClone();
}

//! get value of name
const std::string& calc::Id::name() const
{
  return d_name;
}

std::string calc::Id::qName() const
{
  return quote(name());
}

//! get value of position
calc::Position* calc::Id::position() const
{
  return d_position;
}

//! are there no action done after the default constructor?
bool calc::Id::empty() const
{
  PRECOND( (d_position == 0) == d_name.empty());
  return d_position == 0;
}

void calc::Id::posError(const std::string& msg) const
{
  d_position->throwError(msg);
}

void calc::Id::posError(const std::ostringstream& msg) const
{
  posError(msg.str());
}

std::string calc::Id::shortPosText     () const
{
  return d_position->shortText();
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

bool calc::operator<(const Id& lhs, const Id& rhs)
{
  return lhs.name() < rhs.name();
}
bool calc::operator==(const Id& lhs, const Id& rhs)
{
  return lhs.name() == rhs.name();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



