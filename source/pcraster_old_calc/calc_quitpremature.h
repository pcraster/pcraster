#ifndef INCLUDED_OLDCALC_QUITPREMATURE
#define INCLUDED_OLDCALC_QUITPREMATURE

#include "stddefx.h"



namespace calc {
  // QuitPremature declarations.
}



namespace calc {



//! exception class to throw a premature end of running the model.
class QuitPremature
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  QuitPremature&           operator=           (const QuitPremature&);

  // Copy constructor. DEFAULT
  // QuitPremature               (const QuitPremature&);

protected:
                   QuitPremature               ();
public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------


   virtual     ~QuitPremature              ();

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



} // namespace calc

#endif
