#include "stddefx.h"

#ifndef INCLUDED_CALC_CR
#include "calc_cr.h"
#define INCLUDED_CALC_CR
#endif

#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif

CSF_CR calc::cr(const CRIndex& cri)
{
  CSF_CR crV[4]= { CR_UINT1,CR_INT4,CR_REAL4,CR_UNDEFINED };
  POSTCOND(cri < 4);
  return crV[cri];
}
size_t calc::size(const CRIndex& cri)
{
  size_t s[3]= { 1,4,4 };
  POSTCOND(cri < 3);
  return s[cri];
}

/*! pick one value scale from \a vsSet that can best
 * fit all of a set of value scales assuming:
 * VS_SD &gt; VS_NO &gt; VS_BL and
 * CR_REAL4 &gt; CR_INT4 &gt; CR_UINT1 and that the smallest
 * is preferred
 */
calc::CRIndex calc::allFitCRIndex(
  VS vsSet) /* set of value scales */
{
  if(isIn(VS_SD, vsSet))
    return CRI_f;
  if(isIn(VS_NO, vsSet))
    return CRI_4;
  if(isIn(VS_BL, vsSet))
    return CRI_1;
  POSTCOND(FALSE); // NEVER
  return CRI_X;
}
