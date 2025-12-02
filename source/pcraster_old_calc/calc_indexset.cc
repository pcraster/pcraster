#include "stddefx.h"
#include "calc_indexset.h"
#include "calc_infoscript.h"
#include "calc_indexparameter.h"

#include <utility>

calc::IndexSet::IndexSet(const calc::Symbol &name, Set set, bool on, calc::ArrayDefinition *a)
    : UserSymbol(name), IndexContainer(a), d_on(on), d_set(std::move(set))
{
}

VS calc::IndexSet::symbolType() const
{
  return VS_INDEXSET;
}

bool calc::IndexSet::isOn() const
{
  return d_on;
}

void calc::IndexSet::addToSet(Set &listToBeAddedTo) const
{
  listToBeAddedTo.insert(d_set.begin(), d_set.end());
}

void calc::IndexSet::printSpecific(InfoScript &is) const
{
  is.stream() << "Contents: ";
  for (auto i : d_set) {
    is.parTag(i->name());
    is.stream() << " ";
  }
  is.stream() << "<BR>";
}
