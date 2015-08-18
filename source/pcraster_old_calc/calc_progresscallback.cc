#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_PROGRESSCALLBACK
#include "calc_progresscallback.h"
#define INCLUDED_CALC_PROGRESSCALLBACK
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



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
int calc::ProgressCallBack::update(const struct ProgressInfo& )
{
  return 0;
}

//! status of this callback
calc::ProgressInfo::Status
 calc::ProgressInfo::status() const
{
    if (inTimeStep > nrTimeSteps )
      return Finished;
    if (!inTimeStep)
    {
       if(nrTimeSteps)
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



