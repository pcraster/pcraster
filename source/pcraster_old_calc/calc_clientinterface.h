#ifndef INCLUDED_CALC_CLIENTINTERFACE
#define INCLUDED_CALC_CLIENTINTERFACE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

#ifndef INCLUDED_CALC_DLLCALC
#include "calc_dllcalc.h"
#define INCLUDED_CALC_DLLCALC
#endif

namespace calc {

//! concret class for calc::IClientInterface
class ClientInterface: public DllCalc
{

private:
  //! Assignment operator. NOT IMPLEMENTED.
  ClientInterface&           operator=           (const ClientInterface&);

  //! Copy constructor. NOT IMPLEMENTED.
                   ClientInterface               (const ClientInterface&);

  int execute();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ClientInterface              ();

  /* virtual */   ~ClientInterface              ();

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
