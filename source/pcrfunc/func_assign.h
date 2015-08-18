#ifndef INCLUDED_FUNC_ASSIGN
#define INCLUDED_FUNC_ASSIGN



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_PCRTYPES
#include "pcrtypes.h"
#define INCLUDED_PCRTYPES
#endif

// Module headers.
#ifndef INCLUDED_FUNC_SETMV
#include "func_setmv.h"
#define INCLUDED_FUNC_SETMV
#endif



namespace func {
  // Assign declarations.
}



namespace func {

template<typename T>
inline void assign(
         T const& source,
         T& destination)
{
  if(pcr::isMV(source)) {
    pcr::setMV(destination);
  }
  else {
    destination = source;
  }
}



//! Assigns values from \a source to \a destination.
/*!
  \param     source Array with source values.
  \param     destination Array for destination values.
  \param     size Number of values to assign from source to destination.
  \warning   T should be a simple type. See prerequisites in memcpy manual.

  Missing values in source are handled.
*/
template<typename T>
inline void assign(
         T const* source,
         T* destination,
         size_t size)
{
  std::memcpy(static_cast<void*>(destination),
         static_cast<void const*>(source), size * sizeof(T));
}



//! Assign value from \a source to \a destination.
/*!
  \param     source Value to assign to \a destination.
  \param     destination Array for destination values.
  \warning   T should be a simple type. See prerequisites in memcpy manual.

  If \a source is a missing value, then all cells in \a destination will become
  missing values.
*/
template<typename T>
inline void assign(
         T const& source,
         T* destination,
         size_t size)
{
  if(pcr::isMV(source)) {
    setAllMV(destination, size);
  }
  else {
    for(size_t i = 0; i < size; ++i) {
      if(!pcr::isMV(destination[i])) {
        destination[i] = source;
      }
    }
  }
}

} // namespace func

#endif

