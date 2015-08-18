#include "stddefx.h"

#ifndef INCLUDED_CALC_MIXEDEXPR
# include "calc_mixedexpr.h"
#define INCLUDED_CALC_MIXEDEXPR
#endif

#ifndef INCLUDED_CALC_INFOSCRIPT
#include "calc_infoscript.h"
#define INCLUDED_CALC_INFOSCRIPT
#endif

calc::MixedExpr::MixedExpr(
  const calc::Element&     pos,
  const calc::Operator& op,
        calc::FieldExprArgs& fa):
  calc::FieldExpr(pos), calc::FieldArgs(pos,op,fa),
  d_type(op)
{
}

void calc::MixedExpr::buildTypesRecursive(VS )
{
  Args& fa = fieldArgs();
  for(size_t i=0; i < nrFieldArgs(); i++)
    fa[i]->buildTypesRecursive(vs());
  buildTypes();
}

void calc::MixedExpr::prepareExecution()
{
  calc::FieldArgs::prepareExecution();
}

void calc::MixedExpr::skipExecution()
{
  calc::FieldArgs::skipExecution();
}

void calc::MixedExpr::buildTypes()
{
  bool isSpatial = false;
  Args& fa = fieldArgs();
  for(size_t i=0; i < nrFieldArgs(); i++)
    if (fa[i]->spatial())
      isSpatial = true;
  restrictFieldArgs(1);

  d_type.restrictSystem(d_type.vs(),isSpatial);
}

calc::FieldType& calc::MixedExpr::restrictType()
{
  return d_type;
}

const calc::FieldType& calc::MixedExpr::fieldType() const
{
  return d_type;
}

void calc::MixedExpr::printFieldArgs(calc::InfoScript& si)const
{
  const Args& fa = fieldArgs();
  for (size_t i = 0; i < nrFieldArgs(); i++) {
     si.stream() << ",";
     fa[i]->print(si);
  }
}
