#include "stddefx.h"

#ifndef INCLUDED_CALC_REPEATUNTIL
#include "calc_repeatuntil.h"
#define INCLUDED_CALC_REPEATUNTIL
#endif

#ifndef INCLUDED_CALC_FIELDEXPR
#include "calc_fieldexpr.h"
#define INCLUDED_CALC_FIELDEXPR
#endif

#ifndef INCLUDED_CALC_FIELDSTACK
#include "calc_fieldstack.h"
#define INCLUDED_CALC_FIELDSTACK
#endif


#ifndef INCLUDED_CALC_INFOSCRIPT
#include "calc_infoscript.h"
#define INCLUDED_CALC_INFOSCRIPT
#endif


calc::RepeatUntil::RepeatUntil(
    const Element& pos,
    StatementBlock *parentBlock):
    InnerStatementBlock(pos,parentBlock),
    d_condition(0)
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
  bool condition;
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
  bool noneAreTrue,noneAreFalse;
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
