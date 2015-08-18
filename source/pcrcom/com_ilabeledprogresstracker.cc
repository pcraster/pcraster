#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_ILABELEDPROGRESSTRACKER
#include "com_ilabeledprogresstracker.h"
#define INCLUDED_COM_ILABELEDPROGRESSTRACKER
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the ILabeledProgressTracker class.
*/



//------------------------------------------------------------------------------

/*
namespace com {

class ILabeledProgressTrackerPrivate
{
public:

  ILabeledProgressTrackerPrivate()
  {
  }

  ~ILabeledProgressTrackerPrivate()
  {
  }

};

} // namespace com
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC ILABELEDPROGRESSTRACKER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF ILABELEDPROGRESSTRACKER MEMBERS
//------------------------------------------------------------------------------

com::ILabeledProgressTracker::ILabeledProgressTracker(size_t nrSteps)

  : ProgressTracker(nrSteps),
    d_label()

{
}



com::ILabeledProgressTracker::ILabeledProgressTracker(size_t nrSteps,
         std::string const& label)

  : ProgressTracker(nrSteps),
    d_label(label)

{
}



com::ILabeledProgressTracker::~ILabeledProgressTracker()
{
}



std::string const& com::ILabeledProgressTracker::label() const
{
  return d_label;
}



void com::ILabeledProgressTracker::setLabel(std::string const& label)
{
  d_label = label;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



