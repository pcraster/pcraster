#ifndef INCLUDED_CALC_ICACHEDOBJECT
#define INCLUDED_CALC_ICACHEDOBJECT

#include "stddefx.h"



namespace calc {
  // ICachedObject declarations.
}



namespace calc {



//! Interface for objects stored in RunTimeEnv::d_cache
class ICachedObject
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  ICachedObject&           operator=           (ICachedObject const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   ICachedObject               (ICachedObject const& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ICachedObject               ();

     virtual       ~ICachedObject              ();

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
