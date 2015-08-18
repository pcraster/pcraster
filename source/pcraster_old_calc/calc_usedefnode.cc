#include "stddefx.h"

#ifndef INCLUDED_CALC_USEDEFNODE
#include "calc_usedefnode.h"
#define INCLUDED_CALC_USEDEFNODE
#endif

#ifndef INCLUDED_CALC_STATEMENTBLOCK
#include "calc_statementblock.h"      // parentBlock()
#define INCLUDED_CALC_STATEMENTBLOCK
#endif

calc::UseDefNode::UseDefNode(
  calc::StatementBlock &inBlock):
  d_next(0),d_inBlock(inBlock)
{
}

calc::UseDefNode::~UseDefNode()
{
}

//! l may be 0 in case of last node add in calc::FieldParameter::finalCheck()
void calc::UseDefNode::addNextUseDef(const calc::UseDefNode *l)
{
  d_next = l;
  analyseUseDef();
}

bool calc::UseDefNode::inDynamic() const
{
  return d_inBlock.inDynamic();
}

/*! The next is a definition <b>or</b> there is no next use or def at all, this
    is the last node.
 */
bool calc::UseDefNode::nextIsNotUse() const
{
  return !d_next || d_next->IsDef();
}

/*! There is a next and it is in the same block
 */
bool calc::UseDefNode::nextInSameBlock() const
{
  return d_next && (d_inBlock == d_next->d_inBlock);
}

bool calc::UseDefNode::deleteValueAtEndOfBlock(
    calc::FieldParameter *par,
    bool forceBlockClean) 
{
  calc::StatementBlock *p,*delAfterBlock = &d_inBlock;
  p = delAfterBlock;
  while (p) {
    if (p->isForEachBlock())
      delAfterBlock = p;
    p = p->parentBlock();
  }
  if (delAfterBlock != &d_inBlock || forceBlockClean) {
    delAfterBlock->deleteAtExit(par);
    return true;
  }
  return false;
}
