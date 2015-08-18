#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_PROGRESSTRACKER
#include "com_progresstracker.h"
#define INCLUDED_COM_PROGRESSTRACKER
#endif

// Library headers.
#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif
// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the ProgressTracker class.
*/



//------------------------------------------------------------------------------

/*
namespace com {

class ProgressTrackerPrivate
{
public:

  ProgressTrackerPrivate()
  {
  }

  ~ProgressTrackerPrivate()
  {
  }

};

} // namespace com
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC IPROGRESSTRACKER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF IPROGRESSTRACKER MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  Initializes the number of steps to 0.
*/
com::ProgressTracker::ProgressTracker()

  : d_nrSteps(0), d_nrFinishedSteps(0)

{
}



//! Constructor.
/*!
  \param     nrSteps Number of steps to be performed.
*/
com::ProgressTracker::ProgressTracker(size_t nrSteps)

  : d_nrSteps(nrSteps), d_nrFinishedSteps(0)

{
}



//! Destructor.
/*!
*/
com::ProgressTracker::~ProgressTracker()
{
}



//! Returns the number of steps to be performed.
/*!
  \return    Number of steps to be performed.
  \sa        nrFinishedSteps(), setNrSteps(size_t)

  The number returned is garanteed to be larger or equal to nrFinishedSteps().
*/
size_t com::ProgressTracker::nrSteps() const
{
  DEVELOP_PRECOND(d_nrFinishedSteps <= d_nrSteps);

  return d_nrSteps;
}



//! Returns the number of finished steps.
/*!
  \return    Number of finished steps.
  \sa        nrSteps(), setNrFinishedSteps(size_t), finishedStep(),
             finishedSteps(size_t), finishedSteps()

  The number returned is garanteed to be smaller or equal to nrSteps().
*/
size_t com::ProgressTracker::nrFinishedSteps() const
{
  DEVELOP_PRECOND(d_nrFinishedSteps <= d_nrSteps);

  return d_nrFinishedSteps;
}



//! Sets the number of steps to be performed to \a nrSteps.
/*!
  \param     nrSteps New number steps to be performed.

  If the number of finished steps is larger than \a nrSteps it is clipped to
  \a nrSteps.
*/
void com::ProgressTracker::setNrSteps(size_t nrSteps)
{
  d_nrSteps = nrSteps;
  d_nrFinishedSteps = std::min(d_nrFinishedSteps, d_nrSteps);
}



//! Sets the number of finished steps to \a nrSteps.
/*!
  \param     nrSteps New number of finished steps.

  This function calls update().
*/
void com::ProgressTracker::setNrFinishedSteps(size_t nrSteps)
{
  DEVELOP_PRECOND(nrSteps <= d_nrSteps);
  DEVELOP_PRECOND(nrSteps >= d_nrFinishedSteps);

  if(nrSteps != d_nrFinishedSteps) {
    d_nrFinishedSteps = nrSteps;
    update();
  }
}



//! Returns if the number of steps finished >= number of steps to process.
/*!
  \return    true or false.
*/
bool com::ProgressTracker::finished() const
{
  return d_nrFinishedSteps >= d_nrSteps;
}



//! Notifies the object that a step has finished.
/*!
  \sa        update()

  This function calls update().

  If nrFinishedSteps() equals nrSteps() than this function has no effect
  (you can't do more steps than the total number of steps).
*/
void com::ProgressTracker::finishedStep()
{
  finishedSteps(1);
}



//! Notifies the object that nrSteps steps have finished.
/*!
  \sa        update()

  This function calls update().

  If nrFinishedSteps() equals nrSteps() than this function has no effect
  (you can't do more steps than the total number of steps).
*/
void com::ProgressTracker::finishedSteps(size_t nrSteps)
{
  DEVELOP_PRECOND(d_nrFinishedSteps + nrSteps <= d_nrSteps);

  if(nrSteps && (d_nrFinishedSteps + nrSteps) <= d_nrSteps) {
    d_nrFinishedSteps += nrSteps;
    update();
  }
}



//! Notifies the object that all steps have finished.
/*!
  \sa        update()

  This function calls update().

  If nrFinishedSteps() equals nrSteps() than this function has no effect
  (you can't do more steps than the total number of steps).
*/
void com::ProgressTracker::finishedSteps()
{
  DEVELOP_PRECOND(d_nrFinishedSteps <= d_nrSteps);

  finishedSteps(d_nrSteps - d_nrFinishedSteps);
}



//! Clears the progress indicator.
/*!
  The default does nothing.
*/
void com::ProgressTracker::clear()
{
}



//! Initializes the progress object.
/*!
  \warning   init() must be called after the object is (re)configured and ready
             to process finishedSteps() calls (again).

  You can override this function and do some specialized (visual)
  initialization stuff, but most probably you also want to call this default.
  It resets the number of finished steps to 0. The number of steps to perform
  stays the same.

  This function is typically called just before processing your steps.
*/
void com::ProgressTracker::init()
{
  d_nrFinishedSteps = 0;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

