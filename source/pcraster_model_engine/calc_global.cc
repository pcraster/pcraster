#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_GLOBAL
#include "calc_global.h"
#define INCLUDED_CALC_GLOBAL
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_GLOBARGS
#include "calc_globargs.h"
#define INCLUDED_CALC_GLOBARGS
#endif
#ifndef INCLUDED_CALC_DOMAINERROR
#include "calc_domainerror.h"
#define INCLUDED_CALC_DOMAINERROR
#endif

/*!
  \file
  This file contains the implementation of the Global class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class GlobalPrivate
{
public:

  GlobalPrivate()
  {
  }

  ~GlobalPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC GLOBAL MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF GLOBAL MEMBERS
//------------------------------------------------------------------------------

calc::Global::Global(F f):
  OpImplRedirect(),
  d_f(f)
{
}

calc::Global::Global(const IOpImpl* redirect):
  OpImplRedirect(redirect),
  d_f(0)
{
}


calc::Global::~Global()
{
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::Global& calc::Global::operator=(const Global& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
calc::Global::Global(const Global& rhs):
  Base(rhs)
{
}
*/
void calc::Global::exec(RunTimeEnv* rte,const Operator& op,size_t nrArgs) const
{
  if (redirect()) {
    redirect()->exec(rte,op,nrArgs);
    return;
  }

  GlobArgs a(op,rte,nrArgs);
  int error=d_f(a.dest(),a.src());
  if (error)
    throwDomainErrorFromCalcLib();
  a.pushResults();
}

calc::MRF::MRF(F f):
  OpImplRedirect(),
  d_f(f)
{
}

calc::MRF::MRF(const IOpImpl* redirect):
  OpImplRedirect(redirect),
  d_f(0)
{
}

calc::MRF::~MRF()
{
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::MRF& calc::MRF::operator=(const MRF& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
calc::MRF::MRF(const MRF& rhs):
  Base(rhs)
{
}
*/
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif
#ifndef INCLUDED_CALC_RUNTIMEENV
#include "calc_runtimeenv.h"
#define INCLUDED_CALC_RUNTIMEENV
#endif
#ifndef INCLUDED_CALC_OPERATOR
#include "calc_operator.h"
#define INCLUDED_CALC_OPERATOR
#endif
void calc::MRF::exec(RunTimeEnv* rte,const Operator& op,size_t nrArgs) const
{
  if (redirect())
    redirect()->exec(rte,op,nrArgs);
  else {
    GlobArgs a(op,rte,nrArgs);
    int error=d_f(a.dest(0),a.dest(1),a.src());
    if (error)
      throwDomainErrorFromCalcLib();
    a.pushResults();
  }

  Field *r1 = rte->popField();
  Field *r0 = rte->popField();
  rte->pushField(r1);
  rte->pushField(r0);
}


calc::OneOfMRF::OneOfMRF(const MRF *mrf):
  d_mrf(mrf)
{
}


calc::OneOfMRF::~OneOfMRF()
{
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::OneOfMRF& calc::OneOfMRF::operator=(const OneOfMRF& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
calc::OneOfMRF::OneOfMRF(const OneOfMRF& rhs):
  Base(rhs)
{
}
*/
#ifndef INCLUDED_CALC_OPERATOR
#include "calc_operator.h"
#define INCLUDED_CALC_OPERATOR
#endif
void calc::OneOfMRF::exec(RunTimeEnv* rte,const Operator& op,size_t nrArgs) const
{
  PRECOND(d_mrf);
  // get the Operator of the mrf from op
  d_mrf->exec(rte, oneOf2Mrf(op.opCode()),nrArgs);

  setMRFResult(rte,oneOfMrfIsStackTop(op.opCode()) ? 0: 1);
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



