#ifndef INCLUDED_CALC_LOOKUPLINEAR
#define INCLUDED_CALC_LOOKUPLINEAR

#include "stddefx.h"
#include "calc_lookuptable.h"



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

  /* virtual */    ~LookupLinear              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  bool find(double& result, const std::vector<double>& key) const override;

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
