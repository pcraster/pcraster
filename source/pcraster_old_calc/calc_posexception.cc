#include "stddefx.h"
#include "calc_posexception.h"

#include <sstream>


/*!
  \file
  This file contains the implementation of the PosException class.
*/

//------------------------------------------------------------------------------
// DEFINITION OF STATIC POSEXCEPTION MEMBERS
//------------------------------------------------------------------------------

std::string hackMsg;

//------------------------------------------------------------------------------
// DEFINITION OF POSEXCEPTION MEMBERS
//------------------------------------------------------------------------------

calc::PosException::PosException(const std::string &fileName, size_t lineNr, size_t charNr,
                                 const std::string &message)
{
  std::ostringstream str;
  str << fileName << ":" << lineNr << ":" << charNr << ":ERROR: " << message;
  append(str.str());
  hackMsg = messages();
}

calc::PosException::PosException(const std::string &message)
{
  append("ERROR: " + message);
  hackMsg = messages();
}

//! dtor
calc::PosException::~PosException()
{
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
