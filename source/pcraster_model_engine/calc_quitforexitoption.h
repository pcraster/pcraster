#ifndef INCLUDED_CALC_QUITFOREXITOPTION
#define INCLUDED_CALC_QUITFOREXITOPTION

#include "stddefx.h"
#include "calc_quitpremature.h"



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

  /* virtual */    ~QuitForExitOption              () override;

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
