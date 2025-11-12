#ifndef INCLUDED_CALC_QUITFORPROGRESSCALLBACK
#define INCLUDED_CALC_QUITFORPROGRESSCALLBACK

#include "stddefx.h"
#include "calc_quitpremature.h"



namespace calc {
  // QuitForProgressCallBack declarations.
}



namespace calc {



//! thrown when quit because ProgressCallBack instructed to quit. 
class QuitForProgressCallBack : public QuitPremature
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  QuitForProgressCallBack&           operator=           (const QuitForProgressCallBack&);

  // Copy constructor. DEFAULT
  // QuitForProgressCallBack               (const QuitForProgressCallBack&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   QuitForProgressCallBack               ();

  /* virtual */    ~QuitForProgressCallBack              () override;

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
