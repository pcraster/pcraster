#include "stddefx.h"

#ifndef INCLUDED_CALC_STACKCR
#include "calc_stackcr.h"
#define INCLUDED_CALC_STACKCR
#endif

#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif

STACK_CR stackCellRepr(VS vs)
{
  if(isIn(VS_SD, vs)) 
    return STACK_CR_S;
  if(isIn(VS_NO, vs))
    return STACK_CR_4; 
  if(isIn(VS_BL, vs)) 
    return STACK_CR_1;
  POSTCOND(FALSE); // NEVER
  return STACK_CR_S;
}
