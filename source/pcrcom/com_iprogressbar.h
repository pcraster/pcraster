#ifndef INCLUDED_COM_IPROGRESSBAR
#define INCLUDED_COM_IPROGRESSBAR



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_COM_PROGRESSTRACKER
#include "com_progresstracker.h"
#define INCLUDED_COM_PROGRESSTRACKER
#endif



namespace com {
  // IProgressBar declarations.
}



namespace com {



//! Base class for all progress bar classes.
/*!
  This class defines the interface for progress bar implementations. These can
  be either graphical or textual.
*/
class IProgressBar: public ProgressTracker
{

  friend class IProgressBarTest;

private:

  //! Width of the progress bar.
  size_t           d_width;

  //! Assignment operator. NOT IMPLEMENTED.
  IProgressBar&    operator=           (IProgressBar const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   IProgressBar        (IProgressBar const& rhs);

protected:

                   IProgressBar        (size_t width,
                                        size_t nrSteps);

  size_t           width               () const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~IProgressBar       ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

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

