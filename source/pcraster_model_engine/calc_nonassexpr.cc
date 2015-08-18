#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_NONASSEXPR
#include "calc_nonassexpr.h"
#define INCLUDED_CALC_NONASSEXPR
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_ASTVISITOR
#include "calc_astvisitor.h"
#define INCLUDED_CALC_ASTVISITOR
#endif



/*!
  \file
  This file contains the implementation of the NonAssExpr class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class NonAssExprPrivate
{
public:

  NonAssExprPrivate()
  {
  }

  ~NonAssExprPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC NONASSEXPR MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF NONASSEXPR MEMBERS
//------------------------------------------------------------------------------

calc::NonAssExpr::NonAssExpr(ASTNode *transferedExpr):
  d_expr(transferedExpr)
{
  setNrReturns(1);
}



//! Copy constructor.
calc::NonAssExpr::NonAssExpr(NonAssExpr const& rhs)
  : ASTNode(rhs),
    d_expr(rhs.d_expr->createClone())
{
  setNrReturns(1);
}

calc::NonAssExpr::~NonAssExpr()
{
  delete d_expr;
}



/* NOT IMPLEMENTED
//! Assignment operator.
calc::NonAssExpr& calc::NonAssExpr::operator=(NonAssExpr const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

calc::ASTNode *calc::NonAssExpr::createClone() const {
  return new NonAssExpr(*this);
}

void calc::NonAssExpr::accept(ASTVisitor& v)
{
  v.visitNonAssExpr(this);
}

//! get value of d_expr
calc::ASTNode* calc::NonAssExpr::expr() const
{
  return d_expr;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



