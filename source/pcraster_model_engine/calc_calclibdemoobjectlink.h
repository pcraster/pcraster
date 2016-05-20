#ifndef INCLUDED_CALC_CALCLIBDEMOOBJECTLINK
#define INCLUDED_CALC_CALCLIBDEMOOBJECTLINK



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace calc {
  // CalcLibDemoObjectLink declarations.
  class RunTimeEnv;
}



namespace calc {



//! demo used in calc_objectlinktest.cc
class CalcLibDemoObjectLink
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  CalcLibDemoObjectLink&           operator=           (CalcLibDemoObjectLink const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   CalcLibDemoObjectLink               (CalcLibDemoObjectLink const& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

/*
                   CalcLibDemoObjectLink               (const std::string&  stringArg,
                                          calc::RunTimeEnv*   rte,
                                          size_t              nrFieldArgs);
 */
                   CalcLibDemoObjectLink               (const geo::RasterSpace& rs);

  /* virtual */    ~CalcLibDemoObjectLink              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             noArguments         ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void setDem (const REAL4 *dem);
  void testOrder(
      REAL4*                           result,
      const std::vector<const REAL4 *>dems,UINT1 singleBool);
  void getDem (REAL4 *dem);
  void testOrder2(
    std::vector<REAL4*>&             result,
    const std::vector<const REAL4 *> dems,
    UINT1                            singleLdd) const;

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
