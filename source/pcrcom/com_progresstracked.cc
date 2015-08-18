#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_PROGRESSTRACKED
#include "com_progresstracked.h"
#define INCLUDED_COM_PROGRESSTRACKED
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_COM_PROGRESSTRACKER
#include "com_progresstracker.h"
#define INCLUDED_COM_PROGRESSTRACKER
#endif



/*!
  \file
  This file contains the implementation of the ProgressTracked class.
*/



//------------------------------------------------------------------------------

/*
namespace com {

class ProgressTrackedPrivate
{
public:

  ProgressTrackedPrivate()
  {
  }

  ~ProgressTrackedPrivate()
  {
  }

};

} // namespace com
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC PROGRESSTRACKED MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF PROGRESSTRACKED MEMBERS
//------------------------------------------------------------------------------

template<typename ProgressTracker>
com::ProgressTracked<ProgressTracker>::ProgressTracked()

  : d_tracker(0)

{
}



template<typename ProgressTracker>
com::ProgressTracked<ProgressTracker>::~ProgressTracked()
{
}



/*
template<typename ProgressTracker>
void com::ProgressTracked<ProgressTracker>::clear()
{
  if(d_tracker) {
    d_tracker->clear();
  }
}



template<typename ProgressTracker>
void com::ProgressTracked<ProgressTracker>::init()
{
  if(d_tracker) {
    d_tracker->clear();
    d_tracker->init();
  }
}
*/



template<typename ProgressTracker>
void com::ProgressTracked<ProgressTracker>::init(size_t nrSteps)
{
  if(d_tracker) {
    d_tracker->setNrSteps(nrSteps);
    d_tracker->clear();
    d_tracker->init();
  }
}



//! Calls ProgressTracker::setNrSteps(size_t).
/*!
  \param     nrSteps Number of actions to be performed.
  \warning   Nothing happends if a tracker is not set.
  \sa        setProgress(ProgressTracker*)
*/
template<typename ProgressTracker>
void com::ProgressTracked<ProgressTracker>::setNrSteps(size_t nrSteps)
{
  if(d_tracker) {
    d_tracker->setNrSteps(nrSteps);
  }
}



//! Calls ProgressTracker::finishedStep().
/*!
  \warning   Nothing happends if a tracker is not set.
  \sa        setProgress(ProgressTracker*)
*/
template<typename ProgressTracker>
void com::ProgressTracked<ProgressTracker>::finishedStep()
{
  if(d_tracker) {
    d_tracker->finishedStep();
  }
}



//! Calls ProgressTracker::finishedSteps().
/*!
  \warning   Nothing happends if a tracker is not set.
  \sa        setProgress(ProgressTracker*)
*/
template<typename ProgressTracker>
void com::ProgressTracked<ProgressTracker>::finishedSteps()
{
  if(d_tracker) {
    d_tracker->finishedSteps();
  }
}



//! Calls ProgressTracker::finishedSteps(size_t).
/*!
  \warning   Nothing happends if a tracker is not set.
  \sa        setProgress(ProgressTracker*)
*/
template<typename ProgressTracker>
void com::ProgressTracked<ProgressTracker>::finishedSteps(size_t nrSteps)
{
  if(d_tracker) {
    d_tracker->finishedSteps(nrSteps);
  }
}



//! Sets the ProgressTracker member to \a tracker.
/*!
  \param     tracker tracker object to set.

  \a tracker can be 0.
*/
template<typename ProgressTracker>
void com::ProgressTracked<ProgressTracker>::setTracker(ProgressTracker* tracker)
{
  d_tracker = tracker;
}



template<typename ProgressTracker>
ProgressTracker* com::ProgressTracked<ProgressTracker>::tracker()
{
  return d_tracker;
}



template<typename ProgressTracker>
size_t com::ProgressTracked<ProgressTracker>::nrFinishedSteps() const
{
  return d_tracker->nrFinishedSteps();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
