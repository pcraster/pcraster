#include "stddefx.h"
#include "calc_indexparameterconstant.h"
#include "calc_arraydefinition.h"
#include "calc_infoscript.h"
#include "calc_iscript.h"

calc::IndexParameterConstant::IndexParameterConstant(
            const BindedSymbol& name,
            bool on,
            const ArrayDefinition *def,
            size_t ind
            ):
  IndexParameter(name,true,def),
  d_indexInArray(ind),d_on(on)
{
}

size_t calc::IndexParameterConstant::index() const
{
  return d_indexInArray;
}

const calc::IndexParameterConstant*
  calc::IndexParameterConstant::indexParameterConstant() const
{
  return this;
}
