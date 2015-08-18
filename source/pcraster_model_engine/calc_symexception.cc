#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_SYMEXCEPTION
#include "calc_symexception.h"
#define INCLUDED_CALC_SYMEXCEPTION
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_ASTPAR
#include "calc_astpar.h"
#define INCLUDED_CALC_ASTPAR
#endif
#ifndef INCLUDED_CALC_POSITION
#include "calc_position.h"
#define INCLUDED_CALC_POSITION
#endif



/*!
  \file
  This file contains the implementation of the SymException class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class SymExceptionPrivate
{
public:

  SymExceptionPrivate()
  {
  }

  ~SymExceptionPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC SYMEXCEPTION MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF SYMEXCEPTION MEMBERS
//------------------------------------------------------------------------------

calc::SymException::SymException(
                       const Position&    pos,
                       const std::string& symbolName,
                       const std::string& message):
   PosException(pos.fullText(), symbolName+": "+message,true),
   d_position(pos.fullText()),
   d_symbolName(symbolName),
   d_message(message)
{
}

calc::SymException::SymException(
                       const ASTPar*      par,
                       const std::string& message):
   PosException(par->position()->fullText(),
                par->name()+": "+message,true),
   d_position(par->position()->fullText()),
   d_symbolName(par->name()),
   d_message(message)
{
}

calc::SymException::~SymException()
{
}

const std::string& calc::SymException::symbolName() const
{
  return d_symbolName;
}

void calc::SymException::throwPos(const std::string& symbolInfo) const
{
  throw PosException(d_position, symbolInfo+": "+d_message,true);
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::SymException& calc::SymException::operator=(const SymException& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
calc::SymException::SymException(const SymException& rhs):
  Base(rhs)
{
}
*/

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



