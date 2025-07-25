#include "stddefx.h"
#include "com_algorithm.h"
#include "calc_statementblock.h"
#include "calc_usersymbol.h"
#include "calc_infoscript.h"
#include "calc_parspar.h"
#include "calc_fieldparameter.h"
#include "calc_iscript.h"
#include <functional>

calc::StatementBlock::StatementBlock(
  const Element&         p,
       StatementBlock   *parentBlock):
  Statement(p),
  d_parentBlock(parentBlock)
{
}

void calc::StatementBlock::printBlock(InfoScript& i)const
{
  for (auto d_stat : d_stats) {
    d_stat->print(i);
    i.stream() << "<BR>";
  }
  bool first=true;
  for (auto it : d_valueDelete) {
    if (first)
      i.stream() << "<U>CLEAN UP</U><BR>";
    first=false;
    it->print(i);
    i.stream() << "<BR>";
  }
  i.stream() << "<BR>End of block: ";
}

void calc::StatementBlock::deleteAtExit(FieldParameter *par)
{
#ifdef DEBUG_DEVELOP
  for (auto
     it=d_valueDelete.begin(); it != d_valueDelete.end(); it++)
    PRECOND((*it) != par);

#endif
  d_valueDelete.push_front(par);
}


calc::StatementBlock::~StatementBlock()
{
  for (auto & d_stat : d_stats)
    delete d_stat;
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
    bool onceTrue{false};
    PromotionOccured() {}
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
  com::forWhole(d_stats,std::mem_fn(&Statement::start));
}

void calc::StatementBlock::prepareExecution()
{
  com::forWhole(d_stats, std::mem_fn(&Statement::prepareExecution));
}

void calc::StatementBlock::run()
{
  executeBlock();
  com::forWhole(d_valueDelete, std::mem_fn(&FieldParameter::deleteValues));
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
