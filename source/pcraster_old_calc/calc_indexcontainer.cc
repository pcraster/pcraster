#include "stddefx.h" 

#ifndef INCLUDED_CALC_INDEXCONTAINER
# include "calc_indexcontainer.h" 
# define INCLUDED_CALC_INDEXCONTAINER
#endif

calc::IndexContainer::IndexContainer(const calc::ArrayDefinition* partOf):
  d_partOf(partOf)
{}

const calc::ArrayDefinition* calc::IndexContainer::partOf() const 
{ return d_partOf; }

void calc::IndexContainer::addActiveToSet(Set& setToBeAddedTo)const
{
  if (isOn())
    addToSet(setToBeAddedTo);
}
