#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_CALCLIB
#include "calc_calclib.h"
#define INCLUDED_CALC_CALCLIB
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_COM_DYNAMICLIBRARY
#include "com_dynamiclibrary.h"
#define INCLUDED_COM_DYNAMICLIBRARY
#endif

// Module headers.



/*!
  \file
  This file contains the implementation of the CalcLib class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class CalcLibPrivate
{
public:

  CalcLibPrivate()
  {
  }

  ~CalcLibPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CALCLIB MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CALCLIB MEMBERS
//------------------------------------------------------------------------------

/*! open library
 *  \throws com::DynamicLibraryException if not found
 */
calc::CalcLib::CalcLib(const std::string& libName):
  d_dl(new com::DynamicLibrary(libName))
{
}



/* NOT IMPLEMENTED
//! Copy constructor.
calc::CalcLib::CalcLib(CalcLib const& rhs)

  : Base(rhs)

{
}
*/



calc::CalcLib::~CalcLib()
{
  delete d_dl;
}



/* NOT IMPLEMENTED
//! Assignment operator.
calc::CalcLib& calc::CalcLib::operator=(CalcLib const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

/*!
 *  \brief get ptr to function
 *  \throws com::DynamicLibraryException if not found
 */
calc::CalcLib::GetMeta
 calc::CalcLib::getMeta() const
{
  return (GetMeta)d_dl->loadSymbol("getMeta");
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



