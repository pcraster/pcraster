#include "stddefx.h"

#ifndef INCLUDED_CALC_ARRAYDEFINITION
# include "calc_arraydefinition.h"
# define INCLUDED_CALC_ARRAYDEFINITION
#endif

#ifndef INCLUDED_CALC_INDEXPARAMETERCONSTANT
# include "calc_indexparameterconstant.h"
# define INCLUDED_CALC_INDEXPARAMETERCONSTANT
#endif

#ifndef INCLUDED_CALC_PARSINDEX
# include "calc_parsindex.h"
# define INCLUDED_CALC_PARSINDEX
#endif

#ifndef INCLUDED_CALC_ISCRIPT
# include "calc_iscript.h"
# define INCLUDED_CALC_ISCRIPT
#endif

#ifndef INCLUDED_CALC_INFOSCRIPT
#include "calc_infoscript.h"
#define INCLUDED_CALC_INFOSCRIPT
#endif


static void cleanUpArray(
  const std::vector<calc::ParsIndex *>& index)
{
  for (size_t i=0; i < index.size(); i++)
    delete index[i];
}

//! ind is deleted on return
calc::ArrayDefinition::ArrayDefinition(
  const calc::Symbol& name,
  const std::vector<calc::ParsIndex *>& index) :
  calc::UserSymbol(name),calc::IndexContainer(this)
{
  try {
  for (size_t i=0; i < index.size(); i++)
    script().addSymbol(index[i]->addMe(this));
  } catch (...) {
    cleanUpArray(index);
    throw;
  }
  cleanUpArray(index);
}

size_t calc::ArrayDefinition::activeIndexSize()const
{
  return d_activeIndex.size();
}

const class calc::IndexParameterConstant* calc::ArrayDefinition::item(
    size_t i) const
{
  PRECOND(i < activeIndexSize());
  return d_activeIndex[i];
}

VS calc::ArrayDefinition::symbolType()const
{
  return VS_ARRAY;
}

void calc::ArrayDefinition::addToSet(std::set<const calc::IndexParameter *>& listToBeAddedTo)const
{
   listToBeAddedTo.insert(d_activeIndex.begin(),d_activeIndex.end());
}

void calc::ArrayDefinition::printSpecific(calc::InfoScript& is)const
{
  is.stream() << "Active Indeces: ";
  for(size_t i=0; i < d_activeIndex.size(); i++) {
    is.parTag(d_activeIndex[i]->name());
    is.stream() << " ";
  }
  is.stream() << "Off Indeces: ";
  for(size_t i=0; i < d_offIndex.size(); i++) {
    is.parTag(d_offIndex[i]->name());
    is.stream() << " ";
  }
  is.stream() << "<BR>";
}
