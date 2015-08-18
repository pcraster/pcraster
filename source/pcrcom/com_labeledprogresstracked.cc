#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_LABELEDPROGRESSTRACKED
#include "com_labeledprogresstracked.h"
#define INCLUDED_COM_LABELEDPROGRESSTRACKED
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the LabeledProgressTracked class.
*/



//------------------------------------------------------------------------------

/*
namespace com {

class LabeledProgressTrackedPrivate
{
public:

  LabeledProgressTrackedPrivate()
  {
  }

  ~LabeledProgressTrackedPrivate()
  {
  }

};

} // namespace com
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC LABELEDPROGRESSTRACKED MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF LABELEDPROGRESSTRACKED MEMBERS
//------------------------------------------------------------------------------

template<typename StreamProgressTracker>
com::LabeledProgressTracked<StreamProgressTracker>::LabeledProgressTracked()

  : ProgressTracked<LabeledProgressTracker<StreamProgressTracker> >()

{
}



template<typename StreamProgressTracker>
com::LabeledProgressTracked<StreamProgressTracker>::~LabeledProgressTracked()
{
}



template<typename StreamProgressTracker>
void com::LabeledProgressTracked<StreamProgressTracker>::setLabel(
         const std::string& label)
{
  if(this->tracker()) {
    this->tracker()->setLabel(label);
  }
}



template<typename StreamProgressTracker>
void com::LabeledProgressTracked<StreamProgressTracker>::init(
         const std::string& label,
         size_t nrSteps)
{
  setLabel(label);
  ProgressTracked<LabeledProgressTracker<StreamProgressTracker> >::init(
         nrSteps);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------


