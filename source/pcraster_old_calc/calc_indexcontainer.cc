#include "stddefx.h"
#include "calc_indexcontainer.h"

calc::IndexContainer::IndexContainer(const calc::ArrayDefinition *partOf) : d_partOf(partOf)
{
}

const calc::ArrayDefinition *calc::IndexContainer::partOf() const
{
  return d_partOf;
}

void calc::IndexContainer::addActiveToSet(Set &setToBeAddedTo) const
{
  if (isOn())
    addToSet(setToBeAddedTo);
}
