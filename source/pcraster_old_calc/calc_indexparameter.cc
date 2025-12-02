#include "stddefx.h"
#include "calc_indexparameter.h"

calc::IndexParameter::IndexParameter(const BindedSymbol &name, bool constant, const ArrayDefinition *def)
    : Parameter(name, constant), IndexContainer(def)
{
}

VS calc::IndexParameter::symbolType() const
{
  return VS_INDEX;
}

void calc::IndexParameter::addToSet(std::set<const calc::IndexParameter *> &listToBeAddedTo) const
{
  listToBeAddedTo.insert(this);
}
