#ifndef INCLUDED_FUNC_SETMV
#define INCLUDED_FUNC_SETMV



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_PCRTYPES
#include "pcrtypes.h"
#define INCLUDED_PCRTYPES
#endif

// PCRaster library headers.

// Module headers.



namespace func {
  // SetMV declarations.
}



namespace func {

//! Sets the first \a size cells in \a destination to a missing value.
/*!
  \param     destination Array to set the missing value in.
  \param     size Number of cells to set to missing value.
*/
template<typename T>
inline void setAllMV(
         T* destination,
         size_t size)
{
  for(size_t i = 0; i < size; ++i) {
    pcr::setMV(destination[i]);
  }
}

} // namespace func

#endif
