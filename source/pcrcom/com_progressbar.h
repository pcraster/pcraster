#ifndef INCLUDED_COM_PROGRESSBAR
#define INCLUDED_COM_PROGRESSBAR



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_COM_IPROGRESSBAR
#include "com_iprogressbar.h"
#define INCLUDED_COM_IPROGRESSBAR
#endif

#ifndef INCLUDED_COM_STREAMWRITER
#include "com_streamwriter.h"
#define INCLUDED_COM_STREAMWRITER
#endif



namespace com {
  // ProgressBar declarations.
}



namespace com {



//! Concrete class for textual progress bars.
/*!
  This class implements a textual progress bar.
*/
class ProgressBar: public IProgressBar,
                   public StreamWriter
{

  friend class ProgressBarTest;

private:

  size_t           d_nrHashesWritten;

  //! Assignment operator. NOT IMPLEMENTED.
  ProgressBar&     operator=           (ProgressBar const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   ProgressBar         (ProgressBar const& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ProgressBar         (size_t width,
                                        std::ostream& stream);

                   ProgressBar         (size_t nrSteps,
                                        size_t width,
                                        std::ostream& stream);

  /* virtual */    ~ProgressBar        ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             clear               ();

  void             init                ();

  void             update              ();

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
