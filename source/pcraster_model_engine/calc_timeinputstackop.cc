#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_TIMEINPUTSTACKOP
#include "calc_timeinputstackop.h"
#define INCLUDED_CALC_TIMEINPUTSTACKOP
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_EXECARGUMENTS
#include "calc_execarguments.h"
#define INCLUDED_CALC_EXECARGUMENTS
#endif
#ifndef INCLUDED_CALC_RUNTIMEENV
#include "calc_runtimeenv.h"
#define INCLUDED_CALC_RUNTIMEENV
#endif
#ifndef INCLUDED_CALC_STACKINPUT
#include "calc_stackinput.h"
#define INCLUDED_CALC_STACKINPUT
#endif

#ifndef INCLUDED_CALC_NONSPATIAL
#include "calc_nonspatial.h"
#define INCLUDED_CALC_NONSPATIAL
#endif



void calc::TimeinputStackOp::exec(RunTimeEnv* rte,const Operator& op,size_t nrArgs) const
{
  ExecArguments a(op,rte,nrArgs);

  StackInput *stack= dynamic_cast<StackInput *>(a.firstNonFieldInput());
  POSTCOND(stack);

  rte->pushField(stack->read(rte->timer().currentInt()));
}

void calc::LookupMapStack::exec(RunTimeEnv* rte,const Operator& op,size_t nrArgs) const
{
  ExecArguments a(op,rte,nrArgs);

  StackInput *stack= dynamic_cast<StackInput *>(a.firstNonFieldInput());
  POSTCOND(stack);

  PRECOND(a.size()==1);
  const NonSpatial *ns = dynamic_cast<const NonSpatial *>(&(a[0]));
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
