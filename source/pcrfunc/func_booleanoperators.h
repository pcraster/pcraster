#ifndef INCLUDED_FUNC_BOOLEANOPERATORS
#define INCLUDED_FUNC_BOOLEANOPERATORS

#include "stddefx.h"



namespace func {
  // BooleanOperators declarations.
}



namespace func {

template<typename T>
void greaterThan(
         UINT1* result,
         T const* lhs,
         T const* rhs,
         size_t size)
{
  for(size_t i = 0; i < size; ++i) {
    if(pcr::isMV(lhs[i]) || pcr::isMV(rhs[i])) {
      pcr::setMV(result[i]);
    }
    else {
      result[i] = lhs > rhs ? UINT1(1) : UINT1(0);
    }
  }
}

} // namespace func

#endif
