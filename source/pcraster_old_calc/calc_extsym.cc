#include "stddefx.h"
#include "calc_extsym.h"
#include "calc_position.h"
#include "calc_positionnone.h"
#include "calc_symbol.h"


/*!
  \file
  This file contains the implementation of the ExtSym class.
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC EXTSYM MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF EXTSYM MEMBERS
//------------------------------------------------------------------------------

//! ctor with positional information
/*!
 * \param name symbol 'value'
 * \param pos  position where symbol is found
 * \pre   pos != 0
 */
calc::ExtSym::ExtSym(const std::string& name, const Position* pos):
  d_name(name),
  d_pos(pos->createClone())
{
  PRECOND(pos);
}

//! ctor with no positional information
/*!
 * a \class PositionNone is created for the position
 * \param name symbol 'value'
 */
calc::ExtSym::ExtSym(const std::string& name):
  d_name(name),
  d_pos(new PositionNone())
{
}

//! ctor with script Symbol
calc::ExtSym::ExtSym(const Symbol& s):
 d_name(s.name()),
 d_pos(s.position()->createClone())
{
  d_pos->setPriority(1);
}

//! Assignment operator.
calc::ExtSym&   calc::ExtSym::operator=(const ExtSym& rhs)
{
  if (this != &rhs) {
    d_name=rhs.name();
    if (d_pos)
      delete d_pos;
    d_pos =rhs.position()->createClone();
  }
  return *this;
}


//! Copy constructor.
calc::ExtSym::ExtSym(const ExtSym& e):
 d_name(e.d_name),
  d_pos(e.d_pos->createClone())
{
}

calc::ExtSym::~ExtSym()
{
  delete d_pos;
}

const calc::Position*  calc::ExtSym::position() const
{
  DEVELOP_PRECOND(d_pos);
  return d_pos;
}

int calc::ExtSym::positionPriority() const
{
  return position()->priority();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

bool calc::operator<(const ExtSym& lhs, const ExtSym& rhs)
{
  return lhs.name() < rhs.name();
}
bool calc::operator==(const ExtSym& lhs, const ExtSym& rhs)
{
  return lhs.name() == rhs.name();
}

