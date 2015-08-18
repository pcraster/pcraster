#include "stddefx.h"

#ifndef INCLUDED_CALC_FOREACH
#include "calc_foreach.h"
#define INCLUDED_CALC_FOREACH
#endif

#ifndef INCLUDED_CALC_ARRAYDEFINITION
#include "calc_arraydefinition.h"
#define INCLUDED_CALC_ARRAYDEFINITION
#endif

#ifndef INCLUDED_CALC_INDEXPARAMETERVARIABLE
#include "calc_indexparametervariable.h"
#define INCLUDED_CALC_INDEXPARAMETERVARIABLE
#endif

#ifndef INCLUDED_CALC_INDEXPARAMETERCONSTANT
#include "calc_indexparameterconstant.h"
#define INCLUDED_CALC_INDEXPARAMETERCONSTANT
#endif

#ifndef INCLUDED_CALC_INFOSCRIPT
#include "calc_infoscript.h"
#define INCLUDED_CALC_INFOSCRIPT
#endif

#ifndef INCLUDED_CALC_ISCRIPT
#include "calc_iscript.h"
#define INCLUDED_CALC_ISCRIPT
#endif

void calc::ForEach::print(calc::InfoScript& i)const
{
  i.stream() << "<UL><LI><B>" << "FOREACH" << "</B> "; 
  i.stream() << " <BR>\n";
  calc::InnerStatementBlock::print(i);
  i.stream() << "</LI></UL>\n";
}

calc::ForEach::ForEach(
    const calc::Element& pos,
    calc::StatementBlock *parentBlock,
    const calc::Symbol& iter,
    const calc::IdList& in,
    const calc::IdList& excl,
    const calc::IdList& order,
    bool  orderIsAscending) : 
    calc::InnerStatementBlock(pos,parentBlock),
    d_symTab(parentBlock),
    d_iterSymbol(iter),
    d_loopedArray(0),
    d_orderIsAscending(orderIsAscending)
{
  PRECOND(in.size() > 0);

  //! find the array that is controlled by this foreach
  const calc::IndexContainer *ic =
    dynamic_cast<const calc::IndexContainer *>(findSymbol(&(in[0]),VS_INDEX_CONTAINER,true));
  d_loopedArray = ic->partOf();

  indexSet(d_in,in);
  indexSet(d_excl,excl);
  indexSet(d_order,order);

  d_iter = new calc::IndexParameterVariable(
    calc::BindedSymbol(d_iterSymbol),d_loopedArray,this);
  try {
    addLocal(d_iter);
  } catch (...) {
    delete d_iter;
    throw;
  }
}

class calc::UserSymbol *calc::ForEach::findSymbol(const class calc::Symbol* sym,
    VS typesExpected, bool mustExist) const
{
  calc::UserSymbol *p = d_symTab.find(sym,typesExpected,mustExist);
  if (!p)
    p = parentBlock()->findSymbol(sym,typesExpected,mustExist);   
  return p;
}

void calc::ForEach::addLocal(calc::UserSymbol *par) 
{
  calc::Symbol *firstDef = findSymbol(par,VS_ANYTHING,false);
  if (firstDef) { // pcrcalc/test273[a]
   std::ostringstream msg;
   msg << par->qName()
       << " defined twice, first definition at "
       << firstDef->definitionPoint();
   par->posError(msg);
  }
  d_symTab.add(par);
}

void calc::ForEach::indexSet(
  Set& set,
  const calc::IdList& list) 
{
  for (size_t i=0; i < list.size(); i++) {
   const calc::IndexContainer *ic = 
     dynamic_cast<const calc::IndexContainer *>(findSymbol(&(list[i]),VS_INDEX_CONTAINER,true));
   POSTCOND(ic);
   if (d_loopedArray != ic->partOf())
    list[i].posError("Element expected to be part of array "
      +d_loopedArray->qName()); // pcrcalc/test270
   ic->addActiveToSet(set);
  }
}

void calc::ForEach::executeBlock()
{
  /* first compute the current set of indices
   * this can only be done now, since an except clause
   * can point to an iterator of an enclosing foreach
   */
     std::set<const calc::IndexParameterConstant *>loopSet;
  std::vector<const calc::IndexParameterConstant *>loop;
  for(Set::const_iterator i = d_in.begin(); i != d_in.end(); i++) {
    const calc::IndexParameterConstant *ipc = (*i)->indexParameterConstant();
    loopSet.insert(ipc);
  }
  for(Set::const_iterator i = d_excl.begin(); i != d_excl.end(); i++) {
    const calc::IndexParameterConstant *ipc = (*i)->indexParameterConstant();
    loopSet.erase(ipc);
  }
  // put in array in correct order
  for(size_t i=0; i < d_loopedArray->activeIndexSize(); i++) {
    const calc::IndexParameterConstant *ipc = d_loopedArray->item(i);
    if (loopSet.count(ipc))
      loop.push_back(ipc);
  }
 
  for(d_current = loop.begin(); d_current != loop.end(); d_current++)
    executeStatements();
}

//! return the current index, controlled by the loop
const calc::IndexParameter* calc::ForEach::currentIndex() const
{
  return *d_current;
}

bool calc::ForEach::isForEachBlock() const
{
  return true;
}
