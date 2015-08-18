#include "stddefx.h"

#ifndef INCLUDED_COM_ALGORITHM
#include "com_algorithm.h"
#define INCLUDED_COM_ALGORITHM
#endif
#ifndef INCLUDED_FUNCTIONAL
#include <functional>  // mem_fun
#define INCLUDED_FUNCTIONAL
#endif

#ifndef INCLUDED_CALC_STATEMENTBLOCK
# include "calc_statementblock.h"
#define INCLUDED_CALC_STATEMENTBLOCK
#endif

#ifndef INCLUDED_CALC_USERSYMBOL
# include "calc_usersymbol.h"
#define INCLUDED_CALC_USERSYMBOL
#endif

#ifndef INCLUDED_CALC_INFOSCRIPT
# include "calc_infoscript.h"
#define INCLUDED_CALC_INFOSCRIPT
#endif

#ifndef INCLUDED_CALC_PARSPAR
# include "calc_parspar.h"
#define INCLUDED_CALC_PARSPAR
#endif

#ifndef INCLUDED_CALC_FIELDPARAMETER
# include "calc_fieldparameter.h"
#define INCLUDED_CALC_FIELDPARAMETER
#endif

#ifndef INCLUDED_CALC_ISCRIPT
# include "calc_iscript.h"
#define INCLUDED_CALC_ISCRIPT
#endif

calc::StatementBlock::StatementBlock(
  const Element&         p,
       StatementBlock   *parentBlock):
  Statement(p),
  d_parentBlock(parentBlock)
{
}

void calc::StatementBlock::printBlock(InfoScript& i)const
{
  for (StatList::const_iterator it = d_stats.begin();
         it != d_stats.end(); it++) {
    (*it)->print(i);
    i.stream() << "<BR>";
  }
  bool first=true;
  for (FieldParList::const_iterator it=d_valueDelete.begin();
       it != d_valueDelete.end(); it++) {
    if (first)
      i.stream() << "<U>CLEAN UP</U><BR>";
    first=false;
    (*it)->print(i);
    i.stream() << "<BR>";
  }
  i.stream() << "<BR>End of block: ";
}

void calc::StatementBlock::deleteAtExit(FieldParameter *par)
{
#ifdef DEBUG_DEVELOP
  for (FieldParList::const_iterator
     it=d_valueDelete.begin(); it != d_valueDelete.end(); it++)
    PRECOND((*it) != par);

#endif
  d_valueDelete.push_front(par);
}


calc::StatementBlock::~StatementBlock()
{
  for (StatList::iterator it = d_stats.begin(); it != d_stats.end(); it++)
    delete *it;
}

calc::StatementBlock* calc::StatementBlock::parentBlock()
{
  return d_parentBlock;
}

const calc::StatementBlock* calc::StatementBlock::parentBlock() const
{
  return d_parentBlock;
}

//! calc::Script and calc::Foreach implement others redirect
void calc::StatementBlock::addSymbol(
  class calc::UserSymbol *sym)
{
  PRECOND(parentBlock());
  parentBlock()->addSymbol(sym);
}

class calc::UserSymbol *
  calc::StatementBlock::findSymbol(
    const Symbol *sym,
    VS typeExpected,
    bool mustExist) const
{
  PRECOND(parentBlock());
  return parentBlock()->findSymbol(sym,typeExpected,mustExist);
}

/*!
 * \todo
 *   hebben we deze methode echt nodig, kan een statement niet
 *   zichtzelf toevoegen?
 */
void calc::StatementBlock::addStatement(Statement *s)
{
    d_stats.push_back(s);
}


namespace calc {
  struct PromotionOccured {
    bool onceTrue;
    PromotionOccured():onceTrue(false) {};
    void operator()(Statement *s) {
      if (s->buildTypes())
       onceTrue=true;
    }
    operator bool() {
      return onceTrue;
    }
  };
}

bool calc::StatementBlock::buildTypes()
{
  return com::forWhole(d_stats,PromotionOccured());
}

void calc::StatementBlock::executeStatements()
{
  com::forWhole(d_stats,std::mem_fun(&Statement::start));
}

void calc::StatementBlock::prepareExecution()
{
  com::forWhole(d_stats, std::mem_fun(&Statement::prepareExecution));
}

void calc::StatementBlock::run()
{
  executeBlock();
  com::forWhole(d_valueDelete, std::mem_fun(&FieldParameter::deleteValues));
}

bool calc::operator==(
  const StatementBlock &b1,
  const StatementBlock &b2)
{
  return &b1 == &b2;
}

bool calc::StatementBlock::isForEachBlock() const
{
  return false;
}

bool calc::StatementBlock::inDynamic() const
{
  return parentBlock()->inDynamic();
}
