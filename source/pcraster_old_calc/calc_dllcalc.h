#ifndef INCLUDED_CALC_DLLCALC
#define INCLUDED_CALC_DLLCALC

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_CALC
#include "calc_calc.h"
#define INCLUDED_CALC_CALC
#endif

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

  /* virtual */   ~DllCalc              ();

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
