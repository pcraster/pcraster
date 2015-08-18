#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_IPROGRESSBAR
#include "com_iprogressbar.h"
#define INCLUDED_COM_IPROGRESSBAR
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the IProgressBar class.
*/



//------------------------------------------------------------------------------

/*
namespace com {

class IProgressBarPrivate
{
public:

  IProgressBarPrivate()
  {
  }

  ~IProgressBarPrivate()
  {
  }

};

} // namespace com
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC IPROGRESSBAR MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF IPROGRESSBAR MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     nrSteps Number of steps the task consists of.
  \param     width Width of the progress bar.
*/
com::IProgressBar::IProgressBar(size_t nrSteps, size_t width)

  : ProgressTracker(nrSteps),
    d_width(width)

{
}



//! Destructor.
/*!
*/
com::IProgressBar::~IProgressBar()
{
}



//! Returns the width of the progress bar.
/*!
  \return    Width
*/
size_t com::IProgressBar::width() const
{
  return d_width;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



