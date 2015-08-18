#include "stddefx.h"

#ifndef INCLUDED_CALC_INDEXPARAMETERVARIABLE
#include "calc_indexparametervariable.h"
#define INCLUDED_CALC_INDEXPARAMETERVARIABLE
#endif

#ifndef INCLUDED_CALC_FOREACH
#include "calc_foreach.h"
#define INCLUDED_CALC_FOREACH
#endif

#ifndef INCLUDED_CALC_INFOSCRIPT
#include "calc_infoscript.h"
#define INCLUDED_CALC_INFOSCRIPT
#endif

#ifndef INCLUDED_CALC_ARRAYDEFINITION
#include "calc_arraydefinition.h"
#define INCLUDED_CALC_ARRAYDEFINITION
#endif

calc::IndexParameterVariable::IndexParameterVariable(
  const calc::BindedSymbol& name,
      const calc::ArrayDefinition *def,
  calc::ForEach* foreach):
  calc::IndexParameter(name,true,def),d_foreach(*foreach)
{
}

const calc::ForEach& calc::IndexParameterVariable::forEach() const
{
  return d_foreach;
}

size_t calc::IndexParameterVariable::index() const
{
  return d_foreach.currentIndex()->index();
}

const calc::IndexParameterConstant* 
  calc::IndexParameterVariable::indexParameterConstant() const
{
  return d_foreach.currentIndex()->indexParameterConstant();
}
