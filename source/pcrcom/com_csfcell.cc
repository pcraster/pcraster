#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif


#ifndef INCLUDED_COM_MATH
#include "com_math.h"
#define INCLUDED_COM_MATH
#endif

/*!
  \file
  Utility functions to be used with the PCRaster csf library.

  The pcr::isMV() and pcr::setMV() functions are defined inline so there's no overhead
  implied in calling these functions. They're convenient because you don't
  have to specify the type of the argument:

  \todo see todo notes in com_mvgeneric.h

  \code
  REAL4 value = calculate();

  if(pcr::isMV(value))
    doIt();
  else
    not();

  \endcode
*/

namespace com {

//! copy \a nrCells from \a src to \dest array with proper conversion
/*! \destType and srcType are one of the csf types (e.g.
 *  UINT1,REAL4,INT4. When copying MV's are also correctly converted
 *  copy works correct with overlapping memory regions
 *  <br> PRECOND(sizeof(DestType) <= sizeof(SrcType));
 */

template<class DestType, class SrcType> void copyCellsBig2Small(
  DestType *dest,
  const SrcType  *src,
  size_t nrCells)
{
  PRECOND(sizeof(DestType) <= sizeof(SrcType));
  for(size_t i=0; i < nrCells; i++)
    CastCell<DestType,SrcType>()(dest[i],src[i]);
}

//! copy \a nrCells from \a src to \a dest array with proper conversion
/*! \a destType and srcType are one of the csf types (e.g.
 *  UINT1,REAL4,INT4. When copying MV's are also correctly converted
 *  copy works correct with overlapping memory regions
 *  <br> PRECOND(sizeof(SrcType) <= sizeof(DestType));
 */
template<class DestType, class SrcType> void copyCellsSmall2Big(
  DestType *dest,
  const SrcType  *src,
  size_t nrCells)
{
  PRECOND(sizeof(SrcType) <= sizeof(DestType));
  size_t i=nrCells;
  do {
    i--;
    CastCell<DestType,SrcType>()(dest[i],src[i]);
  } while(i);
}

}

void com::copyCells(INT4 *dest, const UINT1 *src, size_t n) {
  com::copyCellsSmall2Big<INT4,UINT1>(dest,src,n);
}
void com::copyCells(INT4 *dest, const  INT2 *src, size_t n) {
  com::copyCellsSmall2Big<INT4,INT2>(dest,src,n);
}
void com::copyCells(REAL4 *dest, const  INT2 *src, size_t n) {
  com::copyCellsSmall2Big<REAL4,INT2>(dest,src,n);
}
void com::copyCells(REAL4 *dest, const UINT1 *src, size_t n) {
  com::copyCellsSmall2Big<REAL4,UINT1>(dest,src,n);
}

void com::copyCells(UINT1 *dest, const INT4 *src, size_t n) {
  com::copyCellsBig2Small<UINT1,INT4>(dest,src,n);
}
void com::copyCells(INT2 *dest, const INT4 *src, size_t n) {
  com::copyCellsBig2Small<INT2,INT4>(dest,src,n);
}

void com::copyCells2Boolean(UINT1 *dest, const INT4  *src, size_t n) {
  PRECOND(sizeof(UINT1) <= sizeof(INT4));
  for(size_t i=0; i < n; i++)
    if (pcr::isMV(src[i]))
      pcr::setMV(dest[i]);
    else {
      dest[i] = (src[i] != 0);
    }
}

//! test if v can be represented exactly as an UINT1
bool com::isUINT1(const double& v)
{
  return v >= UINT1_MIN && v <= UINT1_MAX && isInteger(v);
}

//! test if v can be represented exactly as an INT2
bool com::isINT2(const  double& v)
{
  return v >= INT2_MIN && v <= INT2_MAX && isInteger(v);
}

//! test if v can be represented exactly as an INT4
bool com::isINT4(const  double& v)
{
  return v >= INT4_MIN && v <= INT4_MAX && isInteger(v);
}
