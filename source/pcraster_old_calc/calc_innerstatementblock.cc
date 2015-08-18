#include "stddefx.h"

#ifndef INCLUDED_CALC_INNERSTATEMENTBLOCK
#include "calc_innerstatementblock.h"
#define INCLUDED_CALC_INNERSTATEMENTBLOCK
#endif

#ifndef INCLUDED_CALC_USERSYMBOL
#include "calc_usersymbol.h"
#define INCLUDED_CALC_USERSYMBOL
#endif

calc::InnerStatementBlock::InnerStatementBlock(
  const calc::Element& p,
  calc::StatementBlock *parentBlock):
  calc::StatementBlock(p,parentBlock)
{
}

void calc::InnerStatementBlock::executeBlock()
{
  executeStatements();
}

calc::InnerStatementBlock::~InnerStatementBlock()
{
}

void calc::InnerStatementBlock::print(calc::InfoScript& i) const
{
  printBlock(i);
}
