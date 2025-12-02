#include "stddefx.h"
#include "calc_progresscallback.h"

/*!
  \file
  This file contains the implementation of the ProgressCallBack class.
*/


//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF STATIC PROGRESSCALLBACK MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF PROGRESSCALLBACK MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::ProgressCallBack::ProgressCallBack()
{
}

//! dtor
calc::ProgressCallBack::~ProgressCallBack()
{
}

//! when to call, every loop,statement etc..
calc::ProgressPulse calc::ProgressCallBack::callAtPulse() const
{
  return LoopPulse;
}

//! default implementation, do nothing
int calc::ProgressCallBack::update(const struct ProgressInfo &)
{
  return 0;
}

//! status of this callback
calc::ProgressInfo::Status calc::ProgressInfo::status() const
{
  if (inTimeStep > nrTimeSteps)
    return Finished;
  if (!inTimeStep) {
    if (nrTimeSteps)
      return InitialSection;
    else
      return Static;
  }
  return DynamicSection;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
