#include "stddefx.h"
#include "calc_innerstatementblock.h"
#include "calc_usersymbol.h"

calc::InnerStatementBlock::InnerStatementBlock(const calc::Element &p, calc::StatementBlock *parentBlock)
    : calc::StatementBlock(p, parentBlock)
{
}

void calc::InnerStatementBlock::executeBlock()
{
  executeStatements();
}

calc::InnerStatementBlock::~InnerStatementBlock()
{
}

void calc::InnerStatementBlock::print(calc::InfoScript &i) const
{
  printBlock(i);
}
