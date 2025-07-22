#include "stddefx.h"
#include "calc_repeatuntil.h"
#include "com_clone.h"
#include "calc_astnodelist.h"
#include "calc_astvisitor.h"
#include "calc_blockentrance.h"
#include "calc_jumpnode.h"



/*!
  \file
  This file contains the implementation of the RepeatUntil class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class RepeatUntilPrivate
{
public:

  RepeatUntilPrivate()
  {
  }

  ~RepeatUntilPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC REPEATUNTIL MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF REPEATUNTIL MEMBERS
//------------------------------------------------------------------------------


void calc::RepeatUntil::init()
{
  d_condition=nullptr;
}

calc::RepeatUntil::RepeatUntil(
      const Position*    posOfRepeatKeyword,
      ASTNode*           transferredStatements):
  BasicBlock(posOfRepeatKeyword,
             new BlockEntrance(this),
             transferredStatements,
             new JumpNode(this))
{
  init();
  setPosition(posOfRepeatKeyword);
}


calc::RepeatUntil::~RepeatUntil()
{
  delete d_condition;
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::RepeatUntil& calc::RepeatUntil::operator=(const RepeatUntil& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
calc::RepeatUntil::RepeatUntil(const RepeatUntil& rhs):
  Base(rhs)
{
}
*/


void calc::RepeatUntil::transferCondition(ASTNode* condition)
{
  delete d_condition;
  d_condition=condition;
}

//! get value of condition
calc::ASTNode* calc::RepeatUntil::condition() const
{
  return d_condition;
}

//! Copy constructor.
calc::RepeatUntil::RepeatUntil(const RepeatUntil& rhs):
  BasicBlock(rhs)
{
  d_condition=com::non0Clone(rhs.d_condition);
}

calc::RepeatUntil* calc::RepeatUntil::createClone() const
{
  return new RepeatUntil(*this);
}

//! as ASTNode::accept but only called from BasicBlock subclasses (e.g. DynamicSection)
void calc::RepeatUntil::accept(ASTVisitor& v)
{
  PRECOND(d_condition);

  blockEntrance()->accept(v);
     statements()->accept(v);
      d_condition->accept(v);
       jumpNode()->accept(v);
}

void calc::RepeatUntil::callEnter(ASTVisitor& v)
{
  v.enterRepeatUntil(this);
}

void calc::RepeatUntil::callJump(ASTVisitor& v)
{
  v.jumpOutRepeatUntil(this);
}

bool calc::RepeatUntil::hasBackBranch() const
{
  return true;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
