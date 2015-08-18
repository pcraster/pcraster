#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_UNIQUESTRINGGENERATOR
#include "com_uniquestringgenerator.h"
#define INCLUDED_COM_UNIQUESTRINGGENERATOR
#endif

// Library headers.

// PCRaster library headers.


// Module headers.
#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif


/*!
  \file
  This file contains the implementation of the UniqueStringGenerator class.
*/



//------------------------------------------------------------------------------

/*
namespace com {

class UniqueStringGeneratorPrivate
{
public:

  UniqueStringGeneratorPrivate()
  {
  }

  ~UniqueStringGeneratorPrivate()
  {
  }

};

} // namespace com
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC UNIQUESTRINGGENERATOR MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF UNIQUESTRINGGENERATOR MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::UniqueStringGenerator::UniqueStringGenerator()
{
}

//! dtor
com::UniqueStringGenerator::~UniqueStringGenerator()
{
}

//! set the prefix
/*! generate() create a unique string 
 */
void com::UniqueStringGenerator::setPrefix(const std::string& prefix)
{
   d_prefix=prefix;
}

/*! create an unique string of format prefixNumber
 *  the format create is prefix with an integer larger
 *  then 0 attached, e.g. if prefix is "New-", New-1, New-2
 *  etc.
 *  The generated string is garantueed not to be in the current set
 *  and appended to the set
 */
std::string com::UniqueStringGenerator::generate()
{
   int i=0;
   std::string n;
   do {
         n=d_prefix+com::intToStr(++i);
   } while (count(n));
   insert(n);
   return n;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



