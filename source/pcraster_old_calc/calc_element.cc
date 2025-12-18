#include "stddefx.h"
#include "calc_element.h"
#include "calc_iscript.h"
#include "lextoken.h"
#include "calc_position.h"
#include "calc_positionnone.h"

/*!
  \file
  This file contains the implementation of the Element class.
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC ELEMENT MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF ELEMENT MEMBERS
//------------------------------------------------------------------------------

calc::Element::Element()
    :

      d_pos(new PositionNone())
{
}

calc::Element::Element(IScript *script, const Position *pos) : d_script(script)
{
  if (pos != nullptr) {
    d_pos = pos->createClone();
  } else {
    d_pos = new PositionNone();
  }
}

calc::Element::Element(const Element &e) : d_script(e.d_script), d_pos(e.d_pos->createClone())
{
}

calc::Element &calc::Element::operator=(const Element &e)
{
  if (&e != this) {
    d_script = e.d_script;
    delete d_pos;
    d_pos = e.d_pos->createClone();
  }
  return *this;
}

calc::Element::~Element()
{
  delete d_pos;
}

calc::IScript &calc::Element::script()
{
  if (d_script == nullptr) {
    throw SyntaxErrorBug();
  }
  return *d_script;
}

const calc::IScript &calc::Element::scriptConst() const
{
  if (d_script == nullptr) {
    throw SyntaxErrorBug();
  }
  return *d_script;
}

//! as Pos::posError() with timestep info added
void calc::Element::runtimeError(const std::string &inMsg) const
{
  size_t const t = scriptConst().currentTimeStep();
  std::ostringstream msg;
  msg << "\nRUNTIME";
  if (t >= 1) {
    msg << " (at timestep " << t << ")";
  }
  msg << " " << inMsg;
  posError(msg);
}

//! textual description of definition point
std::string calc::Element::definitionPoint() const
{
  return d_pos->text();
}

const calc::Position *calc::Element::position() const
{
  DEVELOP_PRECOND(d_pos);
  return d_pos;
}

int calc::Element::positionPriority() const
{
  return position()->priority();
}

void calc::Element::posError(const std::string &msg) const
{
  d_pos->throwError(msg);
}

void calc::Element::posError(const std::ostringstream &msg) const
{
  posError(msg.str());
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
