#ifndef INCLUDED_COM_ILABELEDPROGRESSTRACKER
#define INCLUDED_COM_ILABELEDPROGRESSTRACKER



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_COM_PROGRESSTRACKER
#include "com_progresstracker.h"
#define INCLUDED_COM_PROGRESSTRACKER
#endif



namespace com {
  // ILabeledProgressTracker declarations.
}



namespace com {



//! Base class for labeled progress trackers.
/*!
  A labeled progress tracker is a progress tracker with a label. Most trackers
  will want to have a label to show the user which task is currently being
  processed.
*/
class ILabeledProgressTracker: public ProgressTracker
{

  friend class ILabeledProgressTrackerTest;

private:

  //! Label contents.
  std::string      d_label;

  //! Assignment operator. NOT IMPLEMENTED.
  ILabeledProgressTracker& operator=   (ILabeledProgressTracker const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   ILabeledProgressTracker(ILabeledProgressTracker const& rhs);

protected:

                   ILabeledProgressTracker(size_t nrSteps);

                   ILabeledProgressTracker(size_t nrSteps,
                                        std::string const& label);

  std::string const& label             () const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~ILabeledProgressTracker();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setLabel            (std::string const& label);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

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
