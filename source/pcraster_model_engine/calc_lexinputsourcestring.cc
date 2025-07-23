#include "stddefx.h"
#include "calc_lexinputsourcestring.h"


/*!
  \file
  This file contains the implementation of the LexInputSourceString class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC LEXINPUTSOURCESTRING MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF LEXINPUTSOURCESTRING MEMBERS
//------------------------------------------------------------------------------

//! ctor, get input from string with contents
calc::LexInputSourceString::LexInputSourceString(
    const std::string& contents):
   LexInputSource("?"),
   d_ptr(0),
   d_contents(contents)
{
}


calc::LexInputSourceString::~LexInputSourceString()
{
}

int calc::LexInputSourceString::getChar()
{
  if (d_ptr >= d_contents.size())
    return(EOF);
  return d_contents[d_ptr++];
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



