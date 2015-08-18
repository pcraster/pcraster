#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_DYNAMICSECTION
#include "calc_dynamicsection.h"
#define INCLUDED_CALC_DYNAMICSECTION
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

calc::DynamicSection::DynamicSection(
      const Position*    posOfDynamicKeyword,
      ASTNode*           transferredStatements):
  BasicBlock(posOfDynamicKeyword,
             new BlockEntrance(this),
             transferredStatements,
             new JumpNode(this))
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
calc::DynamicSection::DynamicSection(const DynamicSection& rhs):
  BasicBlock(rhs)
{
}

calc::DynamicSection* calc::DynamicSection::createClone() const
{
  return new DynamicSection(*this);
}

void calc::DynamicSection::callEnter(ASTVisitor& v)
{
  v.enterDynamicSection(this);
}

void calc::DynamicSection::callJump(ASTVisitor& v)
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
