#include "stddefx.h"
#include "calc_dynamicsection.h"
#include "calc_astvisitor.h"
#include "calc_blockentrance.h"
#include "calc_jumpnode.h"

/*!
  \file
  This file contains the implementation of the DynamicSection class.
*/


//------------------------------------------------------------------------------

/*
namespace calc {

class DynamicSectionPrivate
{
public:

  DynamicSectionPrivate()
  {
  }

  ~DynamicSectionPrivate()
  {
  }

};

} // namespace calc
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC DYNAMICSECTION MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF DYNAMICSECTION MEMBERS
//------------------------------------------------------------------------------

calc::DynamicSection::DynamicSection(const Position *posOfDynamicKeyword, ASTNode *transferredStatements)
    : BasicBlock(posOfDynamicKeyword, new BlockEntrance(this), transferredStatements, new JumpNode(this))
{
}

calc::DynamicSection::~DynamicSection()
{
}

/*
//! Assignment operator.
calc::DynamicSection& calc::DynamicSection::operator=(const DynamicSection& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

//! Copy constructor.
calc::DynamicSection::DynamicSection(const DynamicSection &rhs) : BasicBlock(rhs)
{
}

calc::DynamicSection *calc::DynamicSection::createClone() const
{
  return new DynamicSection(*this);
}

void calc::DynamicSection::callEnter(ASTVisitor &v)
{
  v.enterDynamicSection(this);
}

void calc::DynamicSection::callJump(ASTVisitor &v)
{
  v.jumpOutDynamicSection(this);
}

bool calc::DynamicSection::hasBackBranch() const
{
  return true;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
