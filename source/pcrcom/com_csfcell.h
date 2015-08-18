#ifndef INCLUDED_COM_CSFCELL
#define INCLUDED_COM_CSFCELL

#ifdef _MSC_VER
#undef min
#undef max
#endif

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif

#ifndef INCLUDED_LIMITS
#include <limits>
#define INCLUDED_LIMITS
#endif

#ifndef INCLUDED_PCRTYPES
#include "pcrtypes.h"
#define INCLUDED_PCRTYPES
#endif

namespace com {


/*!
 * \todo
 *   maak alles hier FO's en check of er werkelijk gelinlined wordt
 *   analyseer nm en valgrid uitvoer!
 *
 * template<typename T>
 * class CsfCell {
 *   public:
 *     typedef T      value_type;
 *     static  CSF_CR csfCR();
 *     static  bool   isMV(T *v);
 * };
 * 
 * template<>
 * class CsfCell<UINT1> {
 *   public:
 *     static bool isMV(UINT1 *v) {
 *       return IS_MV_UINT1(v);
 *     }
 *     static CSF_CR csfCR() {
 *       return CR_UINT1;
 *     }
 * };
 * etc
 */

//! check if one of the arguments is a MV
/*!
 * bitwise or | (not ||) to minize jumps:
 * full evaluation instead of short circuit ||
 */
template<typename T1,
         typename T2>
inline bool oneIsMV(T1 v1,T2 v2) {
  return pcr::isMV(v1)|pcr::isMV(v2);
}

//! check if one of the arguments is a MV
template<typename T1,
         typename T3,
         typename T2>
inline bool oneIsMV(T1 v1,T2 v2,T3 v3) {
  return pcr::isMV(v1)|pcr::isMV(v2)|pcr::isMV(v3);
}

bool isUINT1(const double& v);
bool isINT2 (const double& v);
bool isINT4 (const double& v);

//! get min and max of a value range
/*!
 * currently only needed for geo::BandMap, with the MV being a valid
 * number in all circumstances
 */
template<typename T>
  struct GetMinMax {
    T d_MV;
    T d_min;
    T d_max;
    GetMinMax(T MV):
      d_MV(MV),d_min(MV),d_max(MV) {};

    void add(const T*v, size_t n) {
      for (size_t i=0; i< n; ++i)
        if (v[i] != d_MV) {
          if (d_min==d_MV)
            d_min=d_max=v[i];
          d_min = std::min(d_min,v[i]);
          d_max = std::max(d_max,v[i]);
      }
    }
    T min() const {
      return d_min;
    }
    T max() const {
      return d_max;
    }
};

//! return (ptr to) minimum element or end if all MV
template<typename T>
const T* csfCellMin(T const* begin, T const* end) {
  T const* min = end;
  for(T const* i = begin; i != end; ++i) {
    if(!pcr::isMV(*i)) {
      if(min == end) {
        min = i; // init
      }
      if(*i < *min) {
        min = i;
      }
    }
  }

  return min;
}

//! return (ptr to) maximum element or end if all MV
template<typename T>
const T* csfCellMax(const T* begin, const T* end) {
  const T* max=end;
  for(const T* i=begin; i!=end; ++i) {
    if (!pcr::isMV(*i)) {
      if (max==end)
        max=i; // init
      if (*i > *max)
        max=i;
    }
  }
  return max;
}

/*!
 *  \warning  release mode on linux has an issue (Bugzilla 147)
 */
template<typename DestType, typename SrcType>
 struct CastCell {
    void operator()(DestType& dest, const SrcType& src) {
      if (pcr::isMV(src))
        pcr::setMV(dest);
      else
        dest=static_cast<DestType>(src);
    }
 };

//! endian swap for 4 byte values, see csf/swapio.c for full story
template<typename T>
  struct EndianSwap4 {
    void operator()(T& v) {
       DEVELOP_PRECOND(sizeof(T)==4);
       // 0123 => 3210 */
       char tmp,*b=(char *)&v;
       tmp = b[0]; b[0] = b[3]; b[3] = tmp;
       tmp = b[1]; b[1] = b[2]; b[2] = tmp;
    }
  };

//! endian swap for 2 byte values, see csf/swapio.c for full story
template<typename T>
  struct EndianSwap2 {
    void operator()(T& v) {
       DEVELOP_PRECOND(sizeof(T)==2);
       // 01 => 10 */
       char tmp,*b=(char *)&v;
       tmp = b[0]; b[0] = b[1]; b[1] = tmp;
    }
  };


typedef  EndianSwap4<REAL4> EndianSwapREAL4;
typedef  EndianSwap4<INT4>  EndianSwapINT4;
typedef  EndianSwap2<INT2>  EndianSwapINT2;

//! implementation of template copyCells
void copyCells( INT4 *dest, const UINT1 *src, size_t n);
void copyCells( INT4 *dest, const  INT2 *src, size_t n);
void copyCells(REAL4 *dest, const UINT1 *src, size_t n);
void copyCells(REAL4 *dest, const  INT2 *src, size_t n);
void copyCells(UINT1 *dest, const INT4  *src, size_t n);
void copyCells( INT2 *dest, const INT4  *src, size_t n);
void copyCells2Boolean(UINT1 *dest, const INT4  *src, size_t n);

//! less with MV being greater than a value: 1,2,MAX_INT4, MV
/*! e1 &lt; e2
 */
template<typename T>
 bool lessMV(T const& e1, T const& e2)
{
  if (pcr::isMV(e1))
    return false;
  if (pcr::isMV(e2))
    return true;
  return e1 < e2;
}

} // namespace com

#endif
