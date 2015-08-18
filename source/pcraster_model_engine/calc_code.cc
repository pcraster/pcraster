#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_CODE
#include "calc_code.h"
#define INCLUDED_CALC_CODE
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_ASTVISITOR
#include "calc_astvisitor.h"
#define INCLUDED_CALC_ASTVISITOR
#endif
#ifndef INCLUDED_CALC_BLOCKENTRANCE
#include "calc_blockentrance.h"
#define INCLUDED_CALC_BLOCKENTRANCE
#endif
#ifndef INCLUDED_CALC_JUMPNODE
#include "calc_jumpnode.h"
#define INCLUDED_CALC_JUMPNODE
#endif



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

calc::Code::Code(ASTNode* transferredStatements):
  BasicBlock(0,
             new BlockEntrance(this),
             transferredStatements,
             new JumpNode(this))
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
calc::Code::Code(const Code& rhs):
  BasicBlock(rhs)
{
}

calc::Code* calc::Code::createClone() const
{
  return new Code(*this);
}

void calc::Code::callEnter(ASTVisitor& v)
{
  v.enterCode(this);
}

void calc::Code::callJump(ASTVisitor& v)
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
