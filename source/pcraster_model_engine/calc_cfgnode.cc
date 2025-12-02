#include "stddefx.h"
#include "calc_cfgnode.h"

/*!
  \file
  This file contains the implementation of the CFGNode class.
*/


//------------------------------------------------------------------------------

/*
namespace calc {

class CFGNodePrivate
{
public:

  CFGNodePrivate()
  {
  }

  ~CFGNodePrivate()
  {
  }

};

} // namespace calc
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC CFGNODE MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF CFGNODE MEMBERS
//------------------------------------------------------------------------------

void calc::CFGNode::init()
{
  d_node = nullptr;
  d_pred[0] = d_pred[1] = nullptr;
  d_succ[0] = d_succ[1] = nullptr;
}

calc::CFGNode::CFGNode()
{
  init();
}

calc::CFGNode::CFGNode(ASTNode *node)
{
  init();
  d_node = node;
}

calc::CFGNode::~CFGNode()
{
  delete d_succ[Forward];
  d_succ[Forward] = nullptr;
  // Back is ptr to an already deleted node
  d_succ[Back] = nullptr;
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::CFGNode& calc::CFGNode::operator=(const CFGNode& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
calc::CFGNode::CFGNode(const CFGNode& rhs):
  Base(rhs)
{
}
*/

void calc::CFGNode::setForward(CFGNode *succ)
{
  d_succ[Forward] = succ;
}

void calc::CFGNode::setBack(CFGNode *succ)
{
  d_succ[Back] = succ;
}

void calc::CFGNode::setPred(CFGNode *pred)
{
  d_pred[0] = pred;
}

calc::CFGNode *calc::CFGNode::pred() const
{
  return d_pred[0];
}

calc::CFGNode *calc::CFGNode::succ(size_t i) const
{
  DEVELOP_PRECOND(i < NrSuccs);
  return d_succ[i];
}

calc::ASTNode *calc::CFGNode::node() const
{
  return d_node;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
