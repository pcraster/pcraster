#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_LEXINPUTSOURCESTRING
#include "calc_lexinputsourcestring.h"
#define INCLUDED_CALC_LEXINPUTSOURCESTRING
#endif

// Library headers.
#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_STRINGFO
#include "com_stringfo.h"
#define INCLUDED_COM_STRINGFO
#endif
#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif

// Module headers.



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
   LexInputSource("?"),
   d_ptr(0)
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



