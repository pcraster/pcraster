#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_BLOCKENTRANCE
#include "calc_blockentrance.h"
#define INCLUDED_CALC_BLOCKENTRANCE
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

calc::BlockEntrance::BlockEntrance(BasicBlock* block):
  d_block(block)
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
calc::BlockEntrance::BlockEntrance(const BlockEntrance& rhs):
  ASTNode(rhs)
{
}

void calc::BlockEntrance::accept(ASTVisitor& v)
{
  v.visitBlockEntrance(this);
  d_block->callEnter(v);
}

//! get value of d_block
calc::BasicBlock* calc::BlockEntrance::block() const
{
  return d_block;
}


//! SHOULD NOT BE CALLED, since this still holds the old d_block, fixed in copy ctor of BasicBlock
calc::BlockEntrance* calc::BlockEntrance::createClone() const
{
  PRECOND(FALSE);
  return new BlockEntrance(0);
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



