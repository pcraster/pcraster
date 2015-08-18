#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_ASTCFGTESTER
#include "calc_astcfgtester.h"
#define INCLUDED_CALC_ASTCFGTESTER
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_CFGCREATOR
#include "calc_cfgcreator.h"
#define INCLUDED_CALC_CFGCREATOR
#endif

/*!
  \file
  This file contains the implementation of the ASTCFGTester class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class ASTCFGTesterPrivate
{
public:

  ASTCFGTesterPrivate()
  {
  }

  ~ASTCFGTesterPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC ASTCFGTESTER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF ASTCFGTESTER MEMBERS
//------------------------------------------------------------------------------

calc::ASTCFGTester::ASTCFGTester(ASTNode *a)
{
  setAST(a);
}



calc::ASTCFGTester::~ASTCFGTester()
{
}

calc::ASTNode *calc::ASTCFGTester::ast() const
{
    return d_ast.get();
}

//! set new AST and build its CFG
void calc::ASTCFGTester::setAST(ASTNode *a)
{
  if (a != d_ast.get())
    d_ast.reset(a);
  rebuildCFG();
}

//! set new CFG, assuming its the CFG of the current AST
void calc::ASTCFGTester::setCFG(CFGNode *c)
{
  d_cfg.reset(c);
}

//! rebuild the CFG from the AST
void calc::ASTCFGTester::rebuildCFG()
{
  d_cfg.reset(createCFG(ast()));
}


/* NOT IMPLEMENTED
//! Assignment operator.
calc::ASTCFGTester& calc::ASTCFGTester::operator=(const ASTCFGTester& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
calc::ASTCFGTester::ASTCFGTester(const ASTCFGTester& rhs):
  Base(rhs)
{
}
*/

calc::CFGNode *calc::ASTCFGTester::cfg() const
{
    return d_cfg.get();
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



