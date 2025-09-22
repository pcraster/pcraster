#include "stddefx.h"
#include "calc_lexinputsourcestring.h"
#include "com_stringfo.h"
#include "com_strlib.h"

#include <algorithm>


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

//! ctor, get input from argument vector
/*!
 * \param nrArgv size of \a argv
 * \param argv   arguments making up the input, if concatenated with space as delimeter
 *               last element may contain ;;
 */
calc::LexInputSourceString::LexInputSourceString(int nrArgv, const char**argv):
   LexInputSource("?")
{
  d_contents=std::for_each(argv,argv+nrArgv,com::Concatenate(" "));
  d_contents=com::replaceStrByStr(d_contents,";;",";");
  d_contents+="\n";
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



