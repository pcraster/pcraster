#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_BASICBLOCK
#include "calc_basicblock.h"
#define INCLUDED_CALC_BASICBLOCK
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_BLOCKENTRANCE
#include "calc_blockentrance.h"
#define INCLUDED_CALC_BLOCKENTRANCE
#endif
#ifndef INCLUDED_CALC_JUMPNODE
#include "calc_jumpnode.h"
#define INCLUDED_CALC_JUMPNODE
#endif
#ifndef INCLUDED_CALC_ASTNODELIST
#include "calc_astnodelist.h"
#define INCLUDED_CALC_ASTNODELIST
#endif


/*!
  \file
  This file contains the implementation of the BasicBlock class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class BasicBlockPrivate
{
public:

  BasicBlockPrivate()
  {
  }

  ~BasicBlockPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC BASICBLOCK MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF BASICBLOCK MEMBERS
//------------------------------------------------------------------------------

/*!
 * \param beginOfBlock  if 0, the Position is taken from begin() in d_statements
 *                      used to record position of possible keyword starting the block
 * \todo  make beginOfBlock a default (0) parameter at end of argument list
 */
calc::BasicBlock::BasicBlock(
      const Position*   beginOfBlock,
      BlockEntrance*    transferredBlockEntrance,
      ASTNode*          transferredStatements,
      JumpNode*         transferredJumpNode):
   d_blockEntrance(transferredBlockEntrance),
   d_statements(0),
   d_jumpNode(transferredJumpNode)
{
 // transferredStatements must be a ASTNodeList because we allow
 // code to be added by BasicBlock::transferPushBack()
 ASTNodeList *nl = (dynamic_cast<ASTNodeList *>(transferredStatements));
 if (nl)
   d_statements = nl;
 else {
   d_statements = new ASTNodeList();
   d_statements->transferPushBack(transferredStatements);
 }

 if (beginOfBlock)
   setPosition(beginOfBlock);
 else
   setPosition(transferredStatements->position());
}

calc::BasicBlock::~BasicBlock()
{
  delete d_blockEntrance;
  delete d_statements;
  delete d_jumpNode;
}

//! Copy constructor.
calc::BasicBlock::BasicBlock(const BasicBlock& rhs):
  ASTNode(rhs)
{
  d_statements    = rhs.d_statements->createClone();
  // createClone on BlockEntrance and JumpNode is wrong!
  d_blockEntrance = new BlockEntrance(this);
  d_jumpNode      = new JumpNode(this);
}

//! add a node to the end of the statements
void calc::BasicBlock::transferPushBack(ASTNode *n)
{
 d_statements->transferPushBack(n);
}

//! as ASTNode::accept but only called from BasicBlock subclasses (e.g. DynamicSection)
void calc::BasicBlock::accept(ASTVisitor& v)
{
  d_blockEntrance->accept(v);
  d_statements->accept(v);
  d_jumpNode->accept(v);
}


/* NOT IMPLEMENTED
//! Assignment operator.
calc::BasicBlock& calc::BasicBlock::operator=(const BasicBlock& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

//! get value of d_blockEntrance
calc::BlockEntrance* calc::BasicBlock::blockEntrance() const
{
  return d_blockEntrance;
}

//! get value of d_statements
calc::ASTNode* calc::BasicBlock::statements() const
{
  return d_statements;
}

//! get value of d_jumpNode
calc::JumpNode* calc::BasicBlock::jumpNode() const
{
  return d_jumpNode;
}

//! redirect to JumpNode::addDeleteOnForward()
void calc::BasicBlock::addDeleteOnForward(const std::string& parName) const
{
  d_jumpNode->addDeleteOnForward(parName);
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



