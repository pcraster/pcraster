#include "stddefx.h"
#include "calc_repeatuntil.h"
#include "calc_fieldexpr.h"
#include "calc_fieldstack.h"
#include "calc_infoscript.h"


calc::RepeatUntil::RepeatUntil(
    const Element& pos,
    StatementBlock *parentBlock):
    InnerStatementBlock(pos,parentBlock)
{
}

calc::RepeatUntil::~RepeatUntil()
{
    delete d_condition;
}

// extern int repeatCondition;
/*!
 * \todo
 *   remove this repeatCondition hack
 *   Does not work anyway
 */
void calc::RepeatUntil::executeBlock()
{
  bool condition = false;
  do {
    executeStatements();
 //   repeatCondition++;
     condition = executeCondition();
  //  repeatCondition--;
  }  while (!condition);
  // UNTIL condition is not false anywhere
}
bool calc::RepeatUntil::executeCondition()
{
  FieldStack stack;

  d_condition->execute(stack);
  FieldHandle fh = stack.popReadOnly();
  bool noneAreTrue = false;
  bool noneAreFalse = false;
  fh->analyzeBoolean(noneAreTrue,noneAreFalse);
  return noneAreFalse;
}

/*
void calc::RepeatUntil::prepareExecution()
{
  StatementBlock::prepareExecution();
//  repeatCondition++;
   d_condition->prepareExecution();
 // repeatCondition--;
}
*/

bool calc::RepeatUntil::buildTypes()
{
  bool promotionOccured=StatementBlock::buildTypes();
  d_condition->buildTypesRecursive(VS_B);
  return promotionOccured;
}

void calc::RepeatUntil::print(InfoScript& i)const
{
  i.stream() << "<UL><LI><B>" << "REPEATUNTIL" << "</B> ";
  i.stream() << " <BR>\n";
  InnerStatementBlock::print(i);
  i.stream() << "</LI></UL>\n";
}

//! this will delete condition
void calc::RepeatUntil::addCondition(FieldExpr* condition)
{
  PRECOND(!d_condition); // only calle once
  d_condition=condition;
}
