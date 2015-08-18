#ifndef INCLUDED_FUNC_SUMMARYSTATISTICS
#define INCLUDED_FUNC_SUMMARYSTATISTICS



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



namespace func {

template<typename SourceType, typename ResultType>
inline void mean(
         SourceType const* source,
         size_t size,
         ResultType& result)
{
  result = 0;
  size_t nr = 0;

  for(size_t i = 0; i < size; ++i) {
    if(!pcr::isMV(source[i])) {
      result += source[i];
      ++nr;
    }
  }

  if(nr == 0) {
    pcr::setMV(result);
  }
  else {
    result /= nr;
  }
}



template <class InputIterator, typename ResultType>
inline void mean(
         InputIterator begin,
         InputIterator end,
         ResultType& result)
{
  result = 0;
  size_t nr = 0;

  for(; begin != end; ++begin) {
    if(!pcr::isMV(*begin)) {
      result += *begin;
      ++nr;
    }
  }

  if(nr == 0) {
    pcr::setMV(result);
  }
  else {
    result /= nr;
  }
}

}

#endif
