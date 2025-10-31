#ifndef INCLUDED_COM_PROGRESSBAR
#define INCLUDED_COM_PROGRESSBAR

#include "stddefx.h"
#include "com_iprogressbar.h"
#include "com_streamwriter.h"



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

  /* virtual */    ~ProgressBar        () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             clear               () override;

  void             init                () override;

  void             update              () override;

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
