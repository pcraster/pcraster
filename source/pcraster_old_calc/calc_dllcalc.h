#ifndef INCLUDED_CALC_DLLCALC
#define INCLUDED_CALC_DLLCALC

#include "stddefx.h"
#include "calc_calc.h"

#include <sstream>


namespace calc {

class ProgressCallBack;



//! a calc object dll 'callable
/*!
 *  Features:
 *  <ul>
 *   <li>allow for customized ProgressCallBack</li>
 *   <li>catch all errors of execute() and store in memory buffer</li>
 *   <li>ignore everything on stdout</li>
 *  </ul>
 */
class DllCalc : public Calc
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  DllCalc&           operator=           (const DllCalc&);

  //! Copy constructor. NOT IMPLEMENTED.
                   DllCalc               (const DllCalc&);

  //! error stream
  std::ostringstream d_error;
  //! std out stream, not of interest
  std::ostringstream d_devNull;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DllCalc              ();

  /* virtual */   ~DllCalc              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  std::string errorMsg() const;
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
