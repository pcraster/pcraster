#include "stddefx.h"
#include "calc_stackcr.h"
#include "calc_vs.h"

STACK_CR stackCellRepr(VS vs)
{
  if (isIn(VS_SD, vs))
    return STACK_CR_S;
  if (isIn(VS_NO, vs))
    return STACK_CR_4;
  if (isIn(VS_BL, vs))
    return STACK_CR_1;
  POSTCOND(false);  // NEVER
  return STACK_CR_S;
}
