#ifndef INCLUDED_CALC_CALCLIB
#define INCLUDED_CALC_CALCLIB

#include "stddefx.h"

#include <string>


namespace com {
  class DynamicLibrary;
}
namespace calc {
  // CalcLib declarations.
  class ObjectLinkMeta;
}



namespace calc {



//! A (dynamically) loaded library of calc code, classes and functions
class CalcLib
{

  com::DynamicLibrary *d_dl;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  CalcLib&           operator=           (CalcLib const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   CalcLib               (CalcLib const& rhs);

public:
  //! FTTB return an ObjectLinkMeta later, this is again a list of ptrs to funcs
  typedef ObjectLinkMeta (*GetMeta)();

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   CalcLib               (const std::string& libName);

  /* virtual */    ~CalcLib              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  GetMeta           getMeta              () const;

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
