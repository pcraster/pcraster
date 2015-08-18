#include "stddefx.h"

#ifndef INCLUDED_CALC_ASSIGNMENT
# include "calc_assignment.h"
#define INCLUDED_CALC_ASSIGNMENT
#endif

#ifndef INCLUDED_CALC_FIELDEXPR
# include "calc_fieldexpr.h"
#define INCLUDED_CALC_FIELDEXPR
#endif

#ifndef INCLUDED_CALC_BRANCHEXPRIMPL
# include "calc_branchexprimpl.h"
#define INCLUDED_CALC_BRANCHEXPRIMPL
#endif

#ifndef INCLUDED_CALC_FIELDLEFT
# include "calc_fieldleft.h"
#define INCLUDED_CALC_FIELDLEFT
#endif

#ifndef INCLUDED_CALC_FIELDPARAMETER
# include "calc_fieldparameter.h"
#define INCLUDED_CALC_FIELDPARAMETER
#endif

#ifndef INCLUDED_CALC_FIELDSTACK
# include "calc_fieldstack.h"
#define INCLUDED_CALC_FIELDSTACK
#endif

#ifndef INCLUDED_CALC_FINDSYMBOL
#include "calc_findsymbol.h"
#define INCLUDED_CALC_FINDSYMBOL
#endif

#ifndef INCLUDED_CALC_INFOSCRIPT
# include "calc_infoscript.h"
#define INCLUDED_CALC_INFOSCRIPT
#endif

#ifndef INCLUDED_CALC_USEPAR
#include "calc_usepar.h"
#define INCLUDED_CALC_USEPAR
#endif

//! add \a par = \a right to end of block \a b
calc::Assignment::Assignment(
     StatementBlock  *b,
     const WriteInfo& w,
     const UsePar&  par,
     class FieldExpr *right): 
   Statement(par),d_left(0),d_right(right)
{
  try {
    d_left =  new FieldLeft(b,w,par,right->vs());
    d_left->restrictUser(d_right);
 } catch (...) {
   cleanUp();
   throw;
 }
}

calc::Assignment::~Assignment()
{
  cleanUp();
}

void calc::Assignment::cleanUp()
{
  delete d_left;
  delete d_right;
}

bool calc::Assignment::buildTypes()
{
  d_right->buildTypesRecursive(d_left->vs());
  return d_left->restrictUser(d_right);
}

void calc::Assignment::prepareExecution()
{
  d_right->prepareExecution();
  d_left->prepareExecution();
  // we can do this when building types (?)
  // I think so, spatial promotion is not yet
  // known then.
  if ( (!d_right->spatial()) && d_left->spatial()) {
    calc::FieldExprArgs arg(1);
    arg[0] = d_right;
    d_right = new calc::BranchExprImpl(*d_right,major2op(OP_SPATIAL),arg);
    d_right->buildTypesRecursive(d_left->vs());
  }
  POSTCOND(d_right->spatial() == d_left->spatial());
}

void calc::Assignment::run()
{
  PRECOND(d_right->spatial() == d_left->spatial());

  FieldStack stack;
  d_right->execute(stack);
  d_left->assign(stack.popDest(d_left->vs()));
}

void calc::Assignment::print(calc::InfoScript& i)const
{
  i.stream() << definitionPoint() << " ";
  d_left->print(i);
  i.stream() << " = ";
  d_right->print(i);
}
