#include "stddefx.h"
#include "calc_cfgvisitor.h"
#include "calc_cfgnode.h"
#include "calc_astnode.h"

/*!
  \file
  This file contains the implementation of the CFGVisitor class.
*/




//------------------------------------------------------------------------------

/*
namespace calc {

class CFGVisitorPrivate
{
public:

  CFGVisitorPrivate()
  {
  }

  ~CFGVisitorPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CFGVISITOR MEMBERS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF CFGVISITOR MEMBERS
//------------------------------------------------------------------------------

//! ctor
/*!
 * sets current to first node (reset())
 */
calc::CFGVisitor::CFGVisitor(CFGNode *cfg):
  d_cfg(cfg)
{
  reset();
}


calc::CFGVisitor::~CFGVisitor()
{
}

//! visit the whole cfg
void calc::CFGVisitor::visit()
{
  reset();
  while(d_current) {
    visitCurrent();
    advance();
  }
}

//! sets current to first node
void calc::CFGVisitor::reset()
{
  d_current=d_cfg;
  setTakeBackBranch(false);
}

void calc::CFGVisitor::advance()
{
    size_t  i=takeBackBranch() ? 1 : 0;
    d_current=d_current->succ(i);
}

void calc::CFGVisitor::visitCurrent()
{
    setTakeBackBranch(false);
    d_current->node()->accept(*this);
}

//! a no-op, called before ASTStat::stat()
void calc::CFGVisitor::visitStat(ASTStat* /* a */)
{
}

//! BaseExpr is a no-op, a place holder in a CFG
void calc::CFGVisitor::visitExpr(BaseExpr* /* e */)
{
}

void calc::CFGVisitor::visitNonAssExpr(NonAssExpr   *)
{
}

//! set value of d_takeBackBranch
void calc::CFGVisitor::setTakeBackBranch(bool takeBackBranch)
{
  d_takeBackBranch=takeBackBranch;
}

//! get value of d_takeBackBranch
bool calc::CFGVisitor::takeBackBranch() const
{
  return d_takeBackBranch;
}

//! get value of d_current, 0 outside of visit
/*!
 * only has a non-0 value is visit() throws.
 */
calc::ASTNode* calc::CFGVisitor::current() const
{
  if (d_current)
    return d_current->node();
  return nullptr;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
