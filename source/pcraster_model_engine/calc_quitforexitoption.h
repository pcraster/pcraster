#ifndef INCLUDED_CALC_QUITFOREXITOPTION
#define INCLUDED_CALC_QUITFOREXITOPTION



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_QUITPREMATURE
#include "calc_quitpremature.h"
#define INCLUDED_CALC_QUITPREMATURE
#endif



namespace calc {
  // QuitForExitOption declarations.
}



namespace calc {



//! thrown when quit because -e is used on fileoutput statement
class QuitForExitOption : public QuitPremature {

private:

  //! Assignment operator. NOT IMPLEMENTED.
  QuitForExitOption&           operator=           (const QuitForExitOption&);

  // Copy constructor. DEFAULT
  //               QuitForExitOption               (const QuitForExitOption&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   QuitForExitOption               ();

  /* virtual */    ~QuitForExitOption              ();

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
