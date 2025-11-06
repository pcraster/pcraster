#include "stddefx.h"
#include "calc_jumpnode.h"
#include "calc_astvisitor.h"
#include "calc_basicblock.h"
#include "calc_runtimeenv.h"


/*!
  \file
  This file contains the implementation of the JumpNode class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class JumpNodePrivate
{
public:

  JumpNodePrivate()
  {
  }

  ~JumpNodePrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC JUMPNODE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF JUMPNODE MEMBERS
//------------------------------------------------------------------------------

calc::JumpNode::JumpNode(BasicBlock* block):
  d_block(block)
{
  PRECOND(d_block);
}

calc::JumpNode::~JumpNode()
{
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::JumpNode& calc::JumpNode::operator=(const JumpNode& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

//! Copy constructor for createClone
calc::JumpNode::JumpNode(const JumpNode& rhs):
  ASTNode(rhs),
  d_block(rhs.d_block)
{
}

//! first call the specific jump (e.g. jumpOutDynamicSection) then the visitJumpNode
void calc::JumpNode::accept(ASTVisitor& v)
{
  d_block->callJump(v);
  v.visitJumpNode(this);
}

//! get value of d_block
calc::BasicBlock* calc::JumpNode::block() const
{
  return d_block;
}

//! add a parameter to d_deletesOnForward
void calc::JumpNode::addDeleteOnForward(const std::string& parName)
{
  d_deletesOnForward.insert(parName);
}


//! SHOULD NOT BE CALLED, since this still holds the old d_block, fixed in copy ctor of BasicBlock
calc::JumpNode *calc::JumpNode::createClone() const
{
  PRECOND(false);
  return new JumpNode(nullptr);
}

//! get value of d_deletesOnForward
const std::set<std::string>& calc::JumpNode::deletesOnForward() const
{
  return d_deletesOnForward;
}

void calc::JumpNode::deleteForwards(RunTimeEnv& rte) const
{
    typedef std::set<std::string> S;
    S s(d_deletesOnForward);
    for(const auto & i : s)
      rte.deleteValue(i);
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



