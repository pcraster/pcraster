#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_POINTCODEBLOCKREPLACER
#include "calc_pointcodeblockreplacer.h"
#define INCLUDED_CALC_POINTCODEBLOCKREPLACER
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_ASTNODELIST
#include "calc_astnodelist.h"
#define INCLUDED_CALC_ASTNODELIST
#endif
#ifndef INCLUDED_CALC_OPERATOR
#include "calc_operator.h"
#define INCLUDED_CALC_OPERATOR
#endif
#ifndef INCLUDED_CALC_ASTASS
#include "calc_astass.h"
#define INCLUDED_CALC_ASTASS
#endif
#ifndef INCLUDED_CALC_ASTPAR
#include "calc_astpar.h"
#define INCLUDED_CALC_ASTPAR
#endif
#ifndef INCLUDED_CALC_BASEEXPR
#include "calc_baseexpr.h"
#define INCLUDED_CALC_BASEEXPR
#endif
#ifndef INCLUDED_CALC_ASTSYMBOLTABLE
#include "calc_astsymboltable.h"
#define INCLUDED_CALC_ASTSYMBOLTABLE
#endif
/*!
  \file
  This file contains the implementation of the PointCodeBlockReplacer class.
*/



//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF STATIC POINTCODEBLOCKREPLACER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF POINTCODEBLOCKREPLACER MEMBERS
//------------------------------------------------------------------------------

calc::PointCodeBlockReplacer::PointCodeBlockReplacer(
  ASTSymbolTable const& symbols):
   d_symbols(symbols)
{
}



/* NOT IMPLEMENTED
//! Copy constructor.
calc::PointCodeBlockReplacer::PointCodeBlockReplacer(PointCodeBlockReplacer const& rhs)

  : Base(rhs)

{
}
*/



calc::PointCodeBlockReplacer::~PointCodeBlockReplacer()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
calc::PointCodeBlockReplacer& calc::PointCodeBlockReplacer::operator=(PointCodeBlockReplacer const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/


void calc::PointCodeBlockReplacer::visitNonAssExpr(NonAssExpr *)
{
  // do not know how to handle this
  info().d_allPoint=false;
}

void calc::PointCodeBlockReplacer::visitExpr(BaseExpr *o)
{
  ASTVisitor::visitExpr(o);

  if (!o->op().pointOperator())
    info().d_allPoint=false;
  else
    d_nrPointsOpsInExpr++;
}


void calc::PointCodeBlockReplacer::visitPar(ASTPar *p)
{
  d_parsInExpr.insert(p);
}

void calc::PointCodeBlockReplacer::visitAss(ASTAss *a)
{
  DEVELOP_PRECOND(d_info.size());
  info().d_allPoint=true;
  for(size_t i=0; i < a->nrPars(); ++i) {
    ASTPar *p=a->par(i);
    // FTTB skip reported stuff
    if(d_symbols.count(p->name()) && d_symbols[p->name()].reportPar() == p)
       info().d_allPoint=false;
  }

  // collect these two in expr traversal
  d_parsInExpr.clear();
  d_nrPointsOpsInExpr=0;

  // do expr
  a->rhs()->accept(*this);

  if (info().d_allPoint) {
    // update with current expr traversal info
   info().d_nrPointsOps += d_nrPointsOpsInExpr;
   info().d_pars.insert(d_parsInExpr.begin(),d_parsInExpr.end());
   info().d_pars.insert(a->pars().begin(),a->pars().end());
  }
}

void calc::PointCodeBlockReplacer::addBlock(ASTNodeList *l,I begin, I end)
{
 // if statements and some operations (not only a=0;b=a;)
 if (begin!=end && info().d_nrPointsOps) {
    // created object is inserted into input AST by its ctor
    d_list.push_back(
     new PointCodeBlock(l,begin,end, info().d_pars,info().d_nrPointsOps));
  }
 // reinit to false and empty sets
 info() = BlockInfo();
}

void calc::PointCodeBlockReplacer::visitNodeList(ASTNodeList *l)
{
  // begin and end of point block
  I pbBegin,pbEnd;

  d_info.push(BlockInfo());
  for(I i=pbBegin=pbEnd=l->begin(); i!=l->end(); ++i) {
    // non ASTAss will keep top false
    // ASTAss will set top true and then
    // non PointCode BaseExpr's may set rhsAllPoint false
    info().d_allPoint=false;

    (*i)->accept(*this);

    if (info().d_allPoint)
      ++pbEnd;
    else {
      // possible end of PointBlock
      addBlock(l,pbBegin,pbEnd);
      // possible next block:
      pbBegin=i;
      pbBegin++;
      pbEnd=pbBegin;
    }

  }
  addBlock(l,pbBegin,pbEnd);
  d_info.pop();
}

//! get value of d_list that is set when accept() is called
const std::vector<calc::PointCodeBlock *>& calc::PointCodeBlockReplacer::list() const
{
  return d_list;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

