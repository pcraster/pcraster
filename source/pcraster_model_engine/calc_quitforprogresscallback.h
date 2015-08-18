#ifndef INCLUDED_CALC_QUITFORPROGRESSCALLBACK
#define INCLUDED_CALC_QUITFORPROGRESSCALLBACK



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

  /* virtual */    ~QuitForProgressCallBack              ();

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
