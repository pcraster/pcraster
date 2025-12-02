#include "stddefx.h"
#include "calc_code.h"
#include "calc_astvisitor.h"
#include "calc_blockentrance.h"
#include "calc_jumpnode.h"

/*!
  \file
  This file contains the implementation of the Code class.
*/


//------------------------------------------------------------------------------

/*
namespace calc {

class CodePrivate
{
public:

  CodePrivate()
  {
  }

  ~CodePrivate()
  {
  }

};

} // namespace calc
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC CODE MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF CODE MEMBERS
//------------------------------------------------------------------------------

calc::Code::Code(ASTNode *transferredStatements)
    : BasicBlock(nullptr, new BlockEntrance(this), transferredStatements, new JumpNode(this))
{
}

calc::Code::~Code()
{
}

/*
//! Assignment operator.
calc::Code& calc::Code::operator=(const Code& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

//! Copy constructor.
calc::Code::Code(const Code &rhs) : BasicBlock(rhs)
{
}

calc::Code *calc::Code::createClone() const
{
  return new Code(*this);
}

void calc::Code::callEnter(ASTVisitor &v)
{
  v.enterCode(this);
}

void calc::Code::callJump(ASTVisitor &v)
{
  v.jumpOutCode(this);
}

bool calc::Code::hasBackBranch() const
{
  return false;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
