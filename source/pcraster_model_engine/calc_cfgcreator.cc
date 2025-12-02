#include "stddefx.h"
#include "calc_cfgcreator.h"
#include "calc_aststat.h"
#include "calc_pointcodeblock.h"
#include "calc_astvisitor.h"
#include "calc_cfgnode.h"
#include "calc_baseexpr.h"
#include "calc_nonassexpr.h"
#include "calc_astnumber.h"
#include "calc_astpar.h"
#include "calc_astass.h"
#include "calc_jumpnode.h"
#include "calc_blockentrance.h"

#include <stack>

/*!
  \file
  This file contains the implementation of the CFGCreator class.
*/


//------------------------------------------------------------------------------


//------------------------------------------------------------------------------

namespace calc
{

class CFGCreatorPrivate : public ASTVisitor
{

private:
  //! Assignment operator. NOT IMPLEMENTED.
  CFGCreatorPrivate &operator=(const CFGCreatorPrivate &);

  //! Copy constructor. NOT IMPLEMENTED.
  CFGCreatorPrivate(const CFGCreatorPrivate &);

  CFGNode *d_last{nullptr};
  CFGNode *d_first{nullptr};
  ASTAss *d_currentAss{nullptr};

  std::stack<CFGNode *> d_blockEntrances;

  void add(ASTNode *an);
  void setBack(CFGNode *an);


  void visitPar(ASTPar *p) override;
  void visitNumber(ASTNumber *n) override;
  void visitStat(ASTStat *s) override;
  void visitPointCodeBlock(PointCodeBlock *b) override;
  void visitExpr(BaseExpr *e) override;
  void visitAss(ASTAss *a) override;

  void visitNonAssExpr(NonAssExpr *e) override;

  void visitJumpNode(JumpNode *j) override;
  void visitBlockEntrance(BlockEntrance *e) override;

public:
  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  CFGCreatorPrivate();

  /* virtual */ ~CFGCreatorPrivate() override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  CFGNode *releaseFirst();
  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
};


}  // namespace calc

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CFGCREATOR MEMBERS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF CFGCREATOR MEMBERS
//------------------------------------------------------------------------------

calc::CFGNode *calc::createCFG(ASTNode *n)
{
  std::vector<ASTNode *> const nodeVector(1, n);
  return createCFG(nodeVector);
}

calc::CFGNode *calc::createCFG(const std::vector<ASTNode *> &nodeVector)
{
  CFGCreatorPrivate c;
  for (auto i : nodeVector)
    if (i)
      i->accept(c);
  return c.releaseFirst();
}

calc::ScopedCFG::ScopedCFG(CFGNode *n) : cfg(n)
{
}

calc::ScopedCFG::ScopedCFG(ASTNode *n) : cfg(createCFG(n))
{
}

calc::ScopedCFG::~ScopedCFG()
{
  delete cfg;
  cfg = nullptr;
}

//! ctor
calc::CFGCreatorPrivate::CFGCreatorPrivate()

{
}

calc::CFGCreatorPrivate::~CFGCreatorPrivate()
{
  delete d_first;
}

//! release the start of CFG
calc::CFGNode *calc::CFGCreatorPrivate::releaseFirst()
{
  CFGNode *first = d_first;
  d_first = nullptr;
  return first;
}

void calc::CFGCreatorPrivate::add(ASTNode *an)
{
  auto *n = new CFGNode(an);
  if (d_last) {
    d_last->setForward(n);
    n->setPred(d_last);
  } else {
    d_first = n;
  }
  d_last = n;
}

void calc::CFGCreatorPrivate::setBack(CFGNode *b)
{
  PRECOND(d_last);  // something to point back to
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

void calc::CFGCreatorPrivate::visitStat(ASTStat *s)
{
  add(s);
  PRECOND(s->stat());
  s->stat()->accept(*this);
}

void calc::CFGCreatorPrivate::visitNonAssExpr(NonAssExpr *e)
{
  e->expr()->accept(*this);
  add(e);
}

void calc::CFGCreatorPrivate::visitExpr(BaseExpr *e)
{
  ASTVisitor::visitExpr(e);
  add(e);
}

void calc::CFGCreatorPrivate::visitAss(ASTAss *a)
{
  // d_currentAss=a;
  a->rhs()->accept(*this);
  add(a);
}

void calc::CFGCreatorPrivate::visitJumpNode(JumpNode *j)
{
  add(j);
  setBack(d_blockEntrances.top());
  d_blockEntrances.pop();
}

void calc::CFGCreatorPrivate::visitBlockEntrance(BlockEntrance *e)
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
