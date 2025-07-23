#include "stddefx.h"
#include "calc_astvisitor.h"
#include "calc_nonassexpr.h"
#include "calc_astass.h"
#include "calc_astpar.h"
#include "calc_baseexpr.h"
#include "calc_aststat.h"
#include "calc_astnodelist.h"
#include "calc_astnodevector.h"


/*!
  \file
  This file contains the implementation of the ASTVisitor class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class ASTVisitorPrivate
{
public:

  ASTVisitorPrivate()
  {
  }

  ~ASTVisitorPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC ASTVISITOR MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF ASTVISITOR MEMBERS
//------------------------------------------------------------------------------

calc::ASTVisitor::ASTVisitor()
{
}



calc::ASTVisitor::~ASTVisitor()
{
}

void calc::ASTVisitor::visitPar (ASTPar *)
{
}

void calc::ASTVisitor::visitExpr(BaseExpr *e)
{
  PRECOND(e);
  PRECOND(e->args());
  e->args()->accept(*this);
}

//! visit its arguments
void calc::ASTVisitor::visitNonAssExpr(NonAssExpr *e)
{
   e->expr()->accept(*this);
}

void calc::ASTVisitor::visitNumber(ASTNumber *)
{
}

void calc::ASTVisitor::visitAss(ASTAss *a)
{
  for(size_t i=0; i < a->nrPars(); ++i)
    a->par(i)->accept(*this);
  a->rhs()->accept(*this);
}

void calc::ASTVisitor::visitStat(ASTStat *s)
{
  s->stat()->accept(*this);
}

void calc::ASTVisitor::visitNodeList(ASTNodeList *l)
{
  l->visitAll(*this);
}

//! a default no-op
void calc::ASTVisitor::visitPointCodeBlock(PointCodeBlock *)
{
}

void calc::ASTVisitor::enterDynamicSection(DynamicSection *)
{
}

void calc::ASTVisitor::jumpOutDynamicSection(DynamicSection *)
{
}

void calc::ASTVisitor::enterRepeatUntil(RepeatUntil *)
{
}

void calc::ASTVisitor::jumpOutRepeatUntil(RepeatUntil *)
{
}

void calc::ASTVisitor::enterCode(Code *)
{
}

void calc::ASTVisitor::jumpOutCode(Code *)
{
}

void calc::ASTVisitor::visitJumpNode(JumpNode *)
{
}

void calc::ASTVisitor::visitBlockEntrance(BlockEntrance *)
{
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



