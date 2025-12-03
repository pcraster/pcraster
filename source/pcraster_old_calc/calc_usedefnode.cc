#include "stddefx.h"
#include "calc_usedefnode.h"
#include "calc_statementblock.h"  // parentBlock()

calc::UseDefNode::UseDefNode(calc::StatementBlock &inBlock) : d_inBlock(inBlock)
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

bool calc::UseDefNode::deleteValueAtEndOfBlock(calc::FieldParameter *par, bool forceBlockClean)
{
  calc::StatementBlock *p = nullptr;
  calc::StatementBlock *delAfterBlock = &d_inBlock;
  p = delAfterBlock;
  while (p) {
    if (p->isForEachBlock()) {
      delAfterBlock = p;
    }
    p = p->parentBlock();
  }
  if (delAfterBlock != &d_inBlock || forceBlockClean) {
    delAfterBlock->deleteAtExit(par);
    return true;
  }
  return false;
}
