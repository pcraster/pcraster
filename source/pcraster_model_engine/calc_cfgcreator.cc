#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_CFGCREATOR
#include "calc_cfgcreator.h"
#define INCLUDED_CALC_CFGCREATOR
#endif

// Library headers.
#ifndef INCLUDED_STACK
#include <stack>
#define INCLUDED_STACK
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_ASTSTAT
#include "calc_aststat.h"
#define INCLUDED_CALC_ASTSTAT
#endif
#ifndef INCLUDED_CALC_POINTCODEBLOCK
#include "calc_pointcodeblock.h"
#define INCLUDED_CALC_POINTCODEBLOCK
#endif
#ifndef INCLUDED_CALC_ASTVISITOR
#include "calc_astvisitor.h"
#define INCLUDED_CALC_ASTVISITOR
#endif
#ifndef INCLUDED_CALC_CFGNODE
#include "calc_cfgnode.h"
#define INCLUDED_CALC_CFGNODE
#endif
#ifndef INCLUDED_CALC_BASEEXPR
#include "calc_baseexpr.h"
#define INCLUDED_CALC_BASEEXPR
#endif
#ifndef INCLUDED_CALC_NONASSEXPR
#include "calc_nonassexpr.h"
#define INCLUDED_CALC_NONASSEXPR
#endif
#ifndef INCLUDED_CALC_ASTNUMBER
#include "calc_astnumber.h"
#define INCLUDED_CALC_ASTNUMBER
#endif
#ifndef INCLUDED_CALC_ASTPAR
#include "calc_astpar.h"
#define INCLUDED_CALC_ASTPAR
#endif
#ifndef INCLUDED_CALC_ASTASS
#include "calc_astass.h"
#define INCLUDED_CALC_ASTASS
#endif
#ifndef INCLUDED_CALC_JUMPNODE
#include "calc_jumpnode.h"
#define INCLUDED_CALC_JUMPNODE
#endif
#ifndef INCLUDED_CALC_BLOCKENTRANCE
#include "calc_blockentrance.h"
#define INCLUDED_CALC_BLOCKENTRANCE
#endif

/*!
  \file
  This file contains the implementation of the CFGCreator class.
*/



//------------------------------------------------------------------------------



//------------------------------------------------------------------------------

namespace calc {

class CFGCreatorPrivate : public ASTVisitor
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  CFGCreatorPrivate&           operator=           (const CFGCreatorPrivate&);

  //! Copy constructor. NOT IMPLEMENTED.
                   CFGCreatorPrivate               (const CFGCreatorPrivate&);

  CFGNode          *d_last;
  CFGNode          *d_first;
  ASTAss           *d_currentAss;

  std::stack<CFGNode *> d_blockEntrances;

  void              add(ASTNode *an);
  void              setBack(CFGNode *an);


  void visitPar          (ASTPar    *p);
  void visitNumber       (ASTNumber *n);
  void visitStat         (ASTStat   *s);
  void visitPointCodeBlock(PointCodeBlock *b);
  void visitExpr         (BaseExpr   *e);
  void visitAss          (ASTAss    *a);

  void visitNonAssExpr(NonAssExpr   *e);

  void visitJumpNode     (JumpNode  *j);
  void visitBlockEntrance(BlockEntrance  *e);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   CFGCreatorPrivate               ();

  /* virtual */    ~CFGCreatorPrivate              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  CFGNode* releaseFirst() ;
  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------


};


} // namespace calc




//------------------------------------------------------------------------------
// DEFINITION OF STATIC CFGCREATOR MEMBERS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF CFGCREATOR MEMBERS
//------------------------------------------------------------------------------

calc::CFGNode *calc::createCFG(ASTNode *n) {
  std::vector<ASTNode *> nodeVector(1,n);
  return createCFG(nodeVector);
}

calc::CFGNode *calc::createCFG(const std::vector<ASTNode *>& nodeVector)
{
  CFGCreatorPrivate c;
  for(size_t i=0; i < nodeVector.size(); ++i)
    if (nodeVector[i])
      nodeVector[i]->accept(c);
  return c.releaseFirst();
}

calc::ScopedCFG::ScopedCFG(CFGNode *n):
 cfg(n)
{
}

calc::ScopedCFG::ScopedCFG(ASTNode *n):
 cfg(createCFG(n))
{}

calc::ScopedCFG::~ScopedCFG()
{
  delete cfg;
  cfg=0;
}

//! ctor
calc::CFGCreatorPrivate::CFGCreatorPrivate():
  d_last(0),d_first(0),
  d_currentAss(0)
{
}


calc::CFGCreatorPrivate::~CFGCreatorPrivate()
{
  delete d_first;
}

//! release the start of CFG
calc::CFGNode* calc::CFGCreatorPrivate::releaseFirst() {
  CFGNode* first=d_first;
  d_first=0;
  return first;
}

void calc::CFGCreatorPrivate::add(ASTNode *an)
{
  CFGNode *n= new CFGNode(an);
  if (d_last) {
    d_last->setForward(n);
    n->setPred(d_last);
  } else {
    d_first=n;
  }
  d_last=n;
}

void calc::CFGCreatorPrivate::setBack(CFGNode *b)
{
  PRECOND(d_last); // something to point back to
  d_last->setBack(b);
  // FTTB pred not set
}

void calc::CFGCreatorPrivate::visitPar(ASTPar *p)
{
  add(p);
}

void calc::CFGCreatorPrivate::visitNumber(ASTNumber *n)
{
  add(n);
}

void calc::CFGCreatorPrivate::visitPointCodeBlock(PointCodeBlock *b)
{
  add(b);
}
void calc::CFGCreatorPrivate::visitStat(ASTStat   *s)
{
  add(s);
  PRECOND(s->stat());
  s->stat()->accept(*this);
}

void calc::CFGCreatorPrivate::visitNonAssExpr(NonAssExpr   *e)
{
  e->expr()->accept(*this);
  add(e);
}

void calc::CFGCreatorPrivate::visitExpr(BaseExpr *e)
{
   ASTVisitor::visitExpr(e);
   add(e);
}

void calc::CFGCreatorPrivate::visitAss (ASTAss    *a)
{
  // d_currentAss=a;
  a->rhs()->accept(*this);
  add(a);
}

void calc::CFGCreatorPrivate::visitJumpNode(JumpNode  *j)
{
  add(j);
  setBack(d_blockEntrances.top());
  d_blockEntrances.pop();
}

void calc::CFGCreatorPrivate::visitBlockEntrance(BlockEntrance  *e)
{
  add(e);
  d_blockEntrances.push(d_last);
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF STATIC CFGCREATOR MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CFGCREATOR MEMBERS
//------------------------------------------------------------------------------

//   NOT USED
//  calc::CFGCreator::CFGCreator()
//  {
//    d_data=new CFGCreatorPrivate();
//  }
//  
//  
//  
//  /* NOT IMPLEMENTED
//  //! Copy constructor.
//  calc::CFGCreator::CFGCreator(CFGCreator const& rhs)
//  
//    : Base(rhs)
//  
//  {
//  }
//  */
//  
//  
//  
//  calc::CFGCreator::~CFGCreator()
//  {
//    delete d_data;
//  }
//  
//  
//  
//  /* NOT IMPLEMENTED
//  //! Assignment operator.
//  calc::CFGCreator& calc::CFGCreator::operator=(CFGCreator const& rhs)
//  {
//    if (this != &rhs) {
//    }
//    return *this;
//  }
//  */
//  
//  //! add AST fragment to CFG in creation
//  void calc::CFGCreator::add(ASTNode *fragment)
//  {
//    fragment->accept(*d_data);
//  }
//  
//  
//  //! create the CFG with the add()'ed fragments
//  /*!
//   * This always the last call to this object, since
//   * it is destructive.
//   */
//  calc::CFGNode* calc::CFGCreator::create()
//  {
//    return d_data->releaseFirst();
//  }
//  
//  

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



