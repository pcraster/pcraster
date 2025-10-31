#ifndef INCLUDED_COM_LABELEDPROGRESSTRACKER
#define INCLUDED_COM_LABELEDPROGRESSTRACKER

#include "stddefx.h"
#include "com_ilabeledprogresstracker.h"
#include "com_streamwriter.h"



namespace com {
  // LabeledProgressTracker declarations.
}



namespace com {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  The width in characters of a labeled progress tracker is labelWidth + 1 +
  width-of-layered-tracker.
*/
template<typename StreamProgressTracker>
class LabeledProgressTracker: public ILabeledProgressTracker,
                              public StreamWriter
{

  friend class LabeledProgressTrackerTest;

private:

  //! Tracker to label.
  StreamProgressTracker& d_tracker;

  //! Width in number of characters of the area reserved for the label.
  size_t           d_labelWidth;

  //! Assignment operator. NOT IMPLEMENTED.
  LabeledProgressTracker& operator=    (LabeledProgressTracker const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   LabeledProgressTracker(LabeledProgressTracker const& rhs);

  void             update              () override;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   LabeledProgressTracker(StreamProgressTracker& tracker,
                                        size_t labelWidth);

                   LabeledProgressTracker(StreamProgressTracker& tracker,
                                        std::string const& label,
                                        size_t labelWidth);

           ~LabeledProgressTracker() override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             init                () override;

  void             clear               () override;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  size_t           width               () const;

  size_t           labelWidth          () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace com

#endif

