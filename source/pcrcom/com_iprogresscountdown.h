#ifndef INCLUDED_COM_IPROGRESSCOUNTDOWN
#define INCLUDED_COM_IPROGRESSCOUNTDOWN

#include "stddefx.h"
#include "com_progresstracker.h"



namespace com {
  // IProgressCountdown declarations.
}



namespace com {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class IProgressCountdown: public ProgressTracker
{

  friend class IProgressCountdownTest;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  IProgressCountdown& operator=        (IProgressCountdown const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   IProgressCountdown  (IProgressCountdown const& rhs);

protected:

                   IProgressCountdown  ();

                   IProgressCountdown  (size_t nrSteps);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

           ~IProgressCountdown () override;

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
