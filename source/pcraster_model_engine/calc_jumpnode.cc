#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_JUMPNODE
#include "calc_jumpnode.h"
#define INCLUDED_CALC_JUMPNODE
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_ASTVISITOR
#include "calc_astvisitor.h"
#define INCLUDED_CALC_ASTVISITOR
#endif
#ifndef INCLUDED_CALC_BASICBLOCK
#include "calc_basicblock.h"
#define INCLUDED_CALC_BASICBLOCK
#endif
#ifndef INCLUDED_CALC_RUNTIMEENV
#include "calc_runtimeenv.h"
#define INCLUDED_CALC_RUNTIMEENV
#endif

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
  PRECOND(FALSE);
  return new JumpNode(0);
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
    for(S::const_iterator i= s.begin(); i!=s.end(); ++i)
      rte.deleteValue(*i);
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



