#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_POSEXCEPTION
#include "calc_posexception.h"
#define INCLUDED_CALC_POSEXCEPTION
#endif

// Library headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif

// Module headers.



/*!
  \file
  This file contains the implementation of the PosException class.
*/

//------------------------------------------------------------------------------
// DEFINITION OF STATIC POSEXCEPTION MEMBERS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF POSEXCEPTION MEMBERS
//------------------------------------------------------------------------------

calc::PosException::PosException(
     const std::string& pos,
     const std::string& message,
     bool  positionFirst):
  d_message(message),
  d_position(pos)
{
  std::ostringstream str;
  if (positionFirst)
    str << pos << ":ERROR: ";
  else
    str << "ERROR: " << pos << ": ";
  finish(str);
}

//! a script exception with no specific position
calc::PosException::PosException(
     const std::string& message):
  d_message(message)
{
  std::ostringstream str;
  str << "ERROR: ";
  finish(str);
}

//! dtor
calc::PosException::~PosException()
{
}

void calc::PosException::finish(
     std::ostringstream& s)
{
  // clean up and make sure it ends with a new line
  com::removeFrontEndSpace(d_message);
  d_message+='\n';
  s << d_message;
  append(s.str());
}

//! get value of message
const std::string& calc::PosException::message() const
{
  return d_message;
}

//! get value of position
const std::string& calc::PosException::position() const
{
  return d_position;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



