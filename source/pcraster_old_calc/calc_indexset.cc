
#include "stddefx.h"

#ifndef INCLUDED_CALC_INDEXSET
# include "calc_indexset.h"
# define INCLUDED_CALC_INDEXSET
#endif

#ifndef INCLUDED_CALC_INFOSCRIPT
# include "calc_infoscript.h"
# define INCLUDED_CALC_INFOSCRIPT
#endif

#ifndef INCLUDED_CALC_INDEXPARAMETER
# include "calc_indexparameter.h"
# define INCLUDED_CALC_INDEXPARAMETER
#endif

calc::IndexSet::IndexSet(const calc::Symbol& name,
    Set set,
    bool on,
    calc::ArrayDefinition *a):
  UserSymbol(name),IndexContainer(a),
  d_on(on),d_set(set)
{}

VS calc::IndexSet::symbolType()const
{
 return VS_INDEXSET;
}

bool calc::IndexSet::isOn()const
{
 return d_on;
}

void calc::IndexSet::addToSet(Set& listToBeAddedTo) const
{
   listToBeAddedTo.insert(d_set.begin(),d_set.end());
}

void calc::IndexSet::printSpecific(InfoScript& is)const
{
  is.stream() << "Contents: ";
  for (Set::const_iterator i = d_set.begin(); i != d_set.end(); i++) {
    is.parTag((*i)->name());
    is.stream() << " ";
  }
  is.stream() << "<BR>";
}
