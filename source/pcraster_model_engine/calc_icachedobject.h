#ifndef INCLUDED_CALC_ICACHEDOBJECT
#define INCLUDED_CALC_ICACHEDOBJECT



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



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
