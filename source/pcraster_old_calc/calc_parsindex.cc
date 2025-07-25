#include "stddefx.h"
#include "calc_parsindex.h"
#include "calc_arraydefinition.h"
#include "calc_indexparameterconstant.h"
#include "calc_indexset.h"
#include "calc_iscript.h"

calc::ParsIndex::ParsIndex(bool on):
  d_on(on)
{}
calc::ParsIndex::~ParsIndex()
{}

calc::ParsIndexName::ParsIndexName(
  bool on,const Symbol& name, const Symbol& extName):
    calc::ParsIndex(on),d_name(name),
    d_extName(new calc::Symbol(extName))
{
}

calc::ParsIndexName::ParsIndexName( bool on,const Symbol& name):
  ParsIndex(on),d_name(name), d_extName(nullptr)
{}

calc::ParsIndexName::~ParsIndexName()
{
  delete d_extName;
}

calc::ParsIndexSet::ParsIndexSet(bool on, const Symbol& name,
    const IdList&  setList):
  ParsIndex(on),d_name(name),d_setList(setList)
{}


//! returned new allocated copy of IndexSet
calc::UserSymbol *calc::ParsIndexSet::addMe(ArrayDefinition *a) const
{
  calc::IndexContainer::Set list;
  for (size_t i=0; i < d_setList.size(); i++) {
    const IndexContainer *n = dynamic_cast<IndexContainer *>
      (a->scriptConst().findSymbol(&(d_setList[i]),VS_INDEX_SUBSET,true));
    POSTCOND(n);
    if (a != n->partOf())
    d_setList[i].posError("Element expected to be part of array "
      +a->qName()); // pcrcalc/test302
    n->addActiveToSet(list);
  }
  return new IndexSet(d_name,list,d_on,a);
}

//! returned new allocated copy of calc::IndexParameterConstant
calc::UserSymbol *calc::ParsIndexName::addMe(calc::ArrayDefinition *a) const
{
   IndexParameterConstant *n = nullptr;
   // arrayIndex is current size
   // a->d_activeIndex grows while adding indices
   // result is that active indices has an uniq number
   // while off indices does not, but that doesn' matter
   size_t arrayIndex =  a->d_activeIndex.size();
   if (d_extName)
    n = new IndexParameterConstant(
      BindedSymbol(d_name,*d_extName),
      d_on, a, arrayIndex);
   else
    n = new IndexParameterConstant(
      BindedSymbol(d_name), d_on, a,arrayIndex);
   if (n->isOn())
     a->d_activeIndex.push_back(n);
   else
     a->d_offIndex.push_back(n);
   return n;
}
