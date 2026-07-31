#ifndef INCLUDED_CALC_ARGORDERAL
#define INCLUDED_CALC_ARGORDERAL

#include "stddefx.h"
#include "csftypes.h"
#include "calc_argorderidinfo.h"

#include <vector>


namespace calc {
  // ArgOrderAL declarations.
}



namespace calc {


//! wrapper for ArgOrder and AddArea family of functions
class ArgOrderAndAddArea {

public:
  using Vector = std::vector<ArgOrderIdInfo>;
  using ArgIter = Vector::iterator;
  using ArgConstIter = Vector::const_iterator;
  using CellIndex = size_t;

  static void       argOrder      (std::vector<ArgOrderIdInfo> const& args,
                                   INT4 * result,
                                   size_t len);
  static void       argOrderAreaLimited
                                  (Vector& args,
                                   INT4 * result,
                                   size_t len);
  static void       argOrderAddAreaLimited(Vector const& args,
                                   const INT4 * currentId,
                                   INT4 * result,
                                   size_t len);
private:
  static Vector     initArgs      (Vector const& args,
                                   INT4 * result,
                                   size_t len);
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
