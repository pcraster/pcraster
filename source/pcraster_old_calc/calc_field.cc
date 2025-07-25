#include "stddefx.h"
#include "calc_field.h"
#include "calc_map2csf.h"
#include <algorithm>

namespace calc {
 template<> int ObjCount<Field>::numObjects(0);
 static ObjCounter<Field> fieldCounter("calc::Field");
}

calc::Field::Field(VS vs):
  d_vs(vs)
{
}

calc::Field::~Field()
{
}

VS calc::Field::vs()const
{
  return d_vs;
}

/*!
 * \bug
 *   pcrcalc/test83, probably pcrcalc must generate a 'type needed' error message
 *   prior to execution.
 */
void calc::Field::resetVs(VS newVs)
{
  PRECOND(biggestCellRepr(newVs) == biggestCellRepr(d_vs));
  d_vs=newVs;
}
