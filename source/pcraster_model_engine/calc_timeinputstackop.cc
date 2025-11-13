#include "stddefx.h"
#include "calc_timeinputstackop.h"
#include "calc_execarguments.h"
#include "calc_runtimeenv.h"
#include "calc_stackinput.h"
#include "calc_nonspatial.h"



void calc::TimeinputStackOp::exec(RunTimeEnv* rte,const Operator& op,size_t nrArgs) const
{
  ExecArguments const a(op,rte,nrArgs);

  auto *stack= dynamic_cast<StackInput *>(a.firstNonFieldInput());
  POSTCOND(stack);

  rte->pushField(stack->read(rte->timer().currentInt()));
}

void calc::LookupMapStack::exec(RunTimeEnv* rte,const Operator& op,size_t nrArgs) const
{
  ExecArguments const a(op,rte,nrArgs);

  auto *stack= dynamic_cast<StackInput *>(a.firstNonFieldInput());
  POSTCOND(stack);

  PRECOND(a.size()==1);
  const auto *ns = dynamic_cast<const NonSpatial *>(&(a[0]));
  POSTCOND(ns);
  // TODO make it VS_BNOL, then switch to get value
  POSTCOND(ns->vs()==VS_O);
  // RUNTIMER ERROR ON MV
  POSTCOND(!ns->isMV());

  rte->pushField(stack->readLookup((int)ns->getValue()));
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
