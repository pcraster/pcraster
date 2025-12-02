#include "stddefx.h"
#include "calc_blockentrance.h"
#include "calc_astvisitor.h"
#include "calc_basicblock.h"

/*!
  \file
  This file contains the implementation of the BlockEntrance class.
*/


//------------------------------------------------------------------------------

/*
namespace calc {

class BlockEntrancePrivate
{
public:

  BlockEntrancePrivate()
  {
  }

  ~BlockEntrancePrivate()
  {
  }

};

} // namespace calc
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC BLOCKENTRANCE MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF BLOCKENTRANCE MEMBERS
//------------------------------------------------------------------------------

calc::BlockEntrance::BlockEntrance(BasicBlock *block) : d_block(block)
{
}

calc::BlockEntrance::~BlockEntrance()
{
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::BlockEntrance& calc::BlockEntrance::operator=(const BlockEntrance& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

*/
//! Copy constructor. NOT IMPLEMENTED.
calc::BlockEntrance::BlockEntrance(const BlockEntrance &rhs) : ASTNode(rhs)
{
}

void calc::BlockEntrance::accept(ASTVisitor &v)
{
  v.visitBlockEntrance(this);
  d_block->callEnter(v);
}

//! get value of d_block
calc::BasicBlock *calc::BlockEntrance::block() const
{
  return d_block;
}

//! SHOULD NOT BE CALLED, since this still holds the old d_block, fixed in copy ctor of BasicBlock
calc::BlockEntrance *calc::BlockEntrance::createClone() const
{
  PRECOND(false);
  return new BlockEntrance(nullptr);
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
