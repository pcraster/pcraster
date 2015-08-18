#ifndef INCLUDED_CALC_LOOKUPLINEAR
#define INCLUDED_CALC_LOOKUPLINEAR



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_LOOKUPTABLE
#include "calc_lookuptable.h"
#define INCLUDED_CALC_LOOKUPTABLE
#endif



namespace calc {
  // LookupLinear declarations.
}



namespace calc {


/*!
   table must be:
    - like any lookup table with the following additional restricitions
       - only one key column
       - keys are sorted from low to high
 */

class LookupLinear : public LookupTable
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  LookupLinear&           operator=           (const LookupLinear&);

  //! Copy constructor. NOT IMPLEMENTED.
                   LookupLinear               (const LookupLinear&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   LookupLinear(VS outType);

  /* virtual */    ~LookupLinear              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  bool find(double& result, const std::vector<double>& key) const;

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
