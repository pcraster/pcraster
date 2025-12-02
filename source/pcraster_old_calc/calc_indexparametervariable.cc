#include "stddefx.h"
#include "calc_indexparametervariable.h"
#include "calc_foreach.h"
#include "calc_infoscript.h"
#include "calc_arraydefinition.h"

calc::IndexParameterVariable::IndexParameterVariable(const calc::BindedSymbol &name,
                                                     const calc::ArrayDefinition *def,
                                                     calc::ForEach *foreach)
    : calc::IndexParameter(name, true, def), d_foreach(*foreach)
{
}

const calc::ForEach &calc::IndexParameterVariable::forEach() const
{
  return d_foreach;
}

size_t calc::IndexParameterVariable::index() const
{
  return d_foreach.currentIndex()->index();
}

const calc::IndexParameterConstant *calc::IndexParameterVariable::indexParameterConstant() const
{
  return d_foreach.currentIndex()->indexParameterConstant();
}
