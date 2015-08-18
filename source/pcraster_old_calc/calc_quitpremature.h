#ifndef INCLUDED_CALC_QUITPREMATURE
#define INCLUDED_CALC_QUITPREMATURE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



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
