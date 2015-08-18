#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_CLIBERROR
#include "com_cliberror.h"
#define INCLUDED_COM_CLIBERROR
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif


/*!
  \file
  This file contains the implementation of to translate error handling
  done in the old PCRaster Ansi C libraries done in old code by calling
  Error() from the misc library. The code here enables one to translate
  that mechanism in throwing com::Exception's
*/



//------------------------------------------------------------------------------

/*
namespace com {

class ClibErrorPrivate
{
public:

  ClibErrorPrivate()
  {
  }

  ~ClibErrorPrivate()
  {
  }

};

} // namespace com
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLIBERROR MEMBERS
//------------------------------------------------------------------------------

std::string   com::ClibError::d_libErrorStr;
bool          com::ClibError::d_errorHandlerCalled;


//------------------------------------------------------------------------------
// DEFINITION OF CLIBERROR MEMBERS
//------------------------------------------------------------------------------

com::ClibError::ClibError()
{
}



/* NOT IMPLEMENTED
//! Copy constructor.
com::ClibError::ClibError(ClibError const& rhs)

  : Base(rhs)

{
}
*/

com::ClibError::~ClibError()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
com::ClibError& com::ClibError::operator=(ClibError const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

//! this is called from libmisc::Error, registered in reset()
extern "C" void com::ClibError::HandleLibError(const char *msg)
{
  d_libErrorStr = msg;
  d_errorHandlerCalled=true;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
