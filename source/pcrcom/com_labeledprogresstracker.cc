#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_LABELEDPROGRESSTRACKER
#include "com_labeledprogresstracker.h"
#define INCLUDED_COM_LABELEDPROGRESSTRACKER
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the LabeledProgressTracker class.
*/



//------------------------------------------------------------------------------

/*
namespace com {

class LabeledProgressTrackerPrivate
{
public:

  LabeledProgressTrackerPrivate()
  {
  }

  ~LabeledProgressTrackerPrivate()
  {
  }

};

} // namespace com
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC LABELEDPROGRESSTRACKER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF LABELEDPROGRESSTRACKER MEMBERS
//------------------------------------------------------------------------------

template<typename StreamProgressTracker>
com::LabeledProgressTracker<StreamProgressTracker>::LabeledProgressTracker(
         StreamProgressTracker& tracker, size_t labelWidth)

  : ILabeledProgressTracker(tracker.nrSteps()),
    StreamWriter(tracker.stream()),
    d_tracker(tracker), d_labelWidth(labelWidth)

{
}



template<typename StreamProgressTracker>
com::LabeledProgressTracker<StreamProgressTracker>::LabeledProgressTracker(
         StreamProgressTracker& tracker, std::string const& label,
         size_t labelWidth)

  : ILabeledProgressTracker(tracker.nrSteps(), label),
    StreamWriter(tracker.stream()),
    d_tracker(tracker), d_labelWidth(labelWidth)

{
}



template<typename StreamProgressTracker>
com::LabeledProgressTracker<StreamProgressTracker>::~LabeledProgressTracker()
{
  /*
  if(nrCharactersWritten()) {
    *this << std::endl;
  }
  */
}



template<typename StreamProgressTracker>
void com::LabeledProgressTracker<StreamProgressTracker>::update()
{
  DEVELOP_PRECOND(nrCharactersWritten() == width());

  d_tracker.setNrFinishedSteps(nrFinishedSteps());
  d_tracker.update();
}



template<typename StreamProgressTracker>
size_t com::LabeledProgressTracker<StreamProgressTracker>::width() const
{
  return d_labelWidth + 1;
}



template<typename StreamProgressTracker>
size_t com::LabeledProgressTracker<StreamProgressTracker>::labelWidth() const
{
  return d_labelWidth;
}



template<typename StreamProgressTracker>
void com::LabeledProgressTracker<StreamProgressTracker>::init()
{
  DEVELOP_PRECOND(nrCharactersWritten() == 0);

  com::ProgressTracker::init();
  d_tracker.setNrSteps(nrSteps());

  if(label().size() >= labelWidth()) {
    *this << label().substr(0, labelWidth());
  }
  else {
    *this << label();
    for(size_t i = label().size(); i < labelWidth(); ++i) {
      *this << ' ';
    }
  }

  *this << " ";
  flush();

  d_tracker.init();

  DEVELOP_POSTCOND(nrCharactersWritten() == width());
}



template<typename StreamProgressTracker>
void com::LabeledProgressTracker<StreamProgressTracker>::clear()
{
  d_tracker.clear();
  StreamWriter::clear();

  DEVELOP_POSTCOND(nrCharactersWritten() == 0);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------


