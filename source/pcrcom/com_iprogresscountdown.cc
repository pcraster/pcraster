#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_IPROGRESSCOUNTDOWN
#include "com_iprogresscountdown.h"
#define INCLUDED_COM_IPROGRESSCOUNTDOWN
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the IProgressCountdown class.
*/



//------------------------------------------------------------------------------

/*
namespace com {

class IProgressCountdownPrivate
{
public:

  IProgressCountdownPrivate()
  {
  }

  ~IProgressCountdownPrivate()
  {
  }

};

} // namespace com
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC IPROGRESSCOUNTDOWN MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF IPROGRESSCOUNTDOWN MEMBERS
//------------------------------------------------------------------------------

com::IProgressCountdown::IProgressCountdown()

  : ProgressTracker()

{
}



com::IProgressCountdown::IProgressCountdown(size_t nrSteps)

  : ProgressTracker(nrSteps)

{
}



/* NOT IMPLEMENTED
//! Copy constructor.
com::IProgressCountdown::IProgressCountdown(IProgressCountdown const& rhs)

  : Base(rhs)

{
}
*/



com::IProgressCountdown::~IProgressCountdown()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
com::IProgressCountdown& com::IProgressCountdown::operator=(IProgressCountdown const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



