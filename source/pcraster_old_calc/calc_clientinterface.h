#ifndef INCLUDED_CALC_CLIENTINTERFACE
#define INCLUDED_CALC_CLIENTINTERFACE

#include "stddefx.h"
#include "calc_dllcalc.h"


namespace calc {

//! concret class for calc::IClientInterface
class ClientInterface: public DllCalc
{

private:
  //! Assignment operator. NOT IMPLEMENTED.
  ClientInterface&           operator=           (const ClientInterface&);

  //! Copy constructor. NOT IMPLEMENTED.
                   ClientInterface               (const ClientInterface&);

  int execute() override;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ClientInterface              ();

  /* virtual */   ~ClientInterface              () override;

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
