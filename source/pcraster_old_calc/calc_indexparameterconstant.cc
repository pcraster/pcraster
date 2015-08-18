#include "stddefx.h"

#ifndef INCLUDED_CALC_INDEXPARAMETERCONSTANT
#include "calc_indexparameterconstant.h"
#define INCLUDED_CALC_INDEXPARAMETERCONSTANT
#endif

#ifndef INCLUDED_CALC_ARRAYDEFINITION
#include "calc_arraydefinition.h"
#define INCLUDED_CALC_ARRAYDEFINITION
#endif

#ifndef INCLUDED_CALC_INFOSCRIPT
#include "calc_infoscript.h"
#define INCLUDED_CALC_INFOSCRIPT
#endif

#ifndef INCLUDED_CALC_ISCRIPT
#include "calc_iscript.h"
#define INCLUDED_CALC_ISCRIPT
#endif

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
