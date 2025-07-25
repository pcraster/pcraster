#include "stddefx.h"
#include "calc_globargs.h"
#include "calc_globarg.h"
#include "calc_runtimeenv.h"
#include "calc_operator.h"  // argType
#include "calc_globresult.h"
#include "calc_field.h"

/*!
  \file
  This file contains the implementation of the GlobArgs class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class GlobArgsPrivate
{
public:

  GlobArgsPrivate()
  {
  }

  ~GlobArgsPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC GLOBARGS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF GLOBARGS MEMBERS
//------------------------------------------------------------------------------


//! each arg in an calc::ApiMap
calc::GlobArgs::GlobArgs(const Operator& op,RunTimeEnv *rte, size_t nrArgs):
  ExecArguments(op,rte,nrArgs),
  d_globArgs(nrArgs)
{
 // TODO?  object if nrActualArgs > 0 use that as the actual nr of arguments
 // test/pcrcalc224c
 // if (!nrArgs)
 //   POSTCOND(op.nrArgs() >= 0); // not a var arg def
 init(rte);
}

void calc::GlobArgs::init(RunTimeEnv *rte)
{
  d_voidArgs = new void *[d_globArgs.size()];
  for (size_t i=0; i < d_globArgs.size(); ++i) {
   d_globArgs[i] = new GlobArg(d_op.argType(i).vs(),*d_fields[i], rte->spatialPacking());
   d_voidArgs[i] = d_globArgs[i]->MAPinterface();
 }
}

calc::GlobArgs::~GlobArgs()
{
  for (auto & d_globArg : d_globArgs)
    delete d_globArg;

  delete[] d_voidArgs;
  for(auto & d_globResult : d_globResults)
    delete d_globResult;
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::GlobArgs& calc::GlobArgs::operator=(const GlobArgs& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
calc::GlobArgs::GlobArgs(const GlobArgs& rhs):
  Base(rhs)
{
}
*/

/*!
 * \param o result of this operator is created, differs from d_op in case of MRF
 */
calc::GlobResult *calc::GlobArgs::createGlobResult(size_t n)
{
  DataType r=resultType(n);
  return new GlobResult(d_op.resultType(n).vs(),r.vs(),d_rte->spatialPacking());
}

//! result MAPinterface as void ptr
void* calc::GlobArgs::dest(size_t r)
{
  if (d_globResults.empty())
    for(size_t i=0; i < d_op.nrResults(); ++i)
     d_globResults.push_back(createGlobResult(i));

  return d_globResults[r]->MAPinterface();
}

void calc::GlobArgs::pushResults()
{
 for(auto & d_globResult : d_globResults)
  ExecArguments::pushResult(d_globResult->createField());
}


//! return array of opaque MAP_* ptr's
const void ** calc::GlobArgs::src()
{
  return (const void **)d_voidArgs;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

//! adjust rte.stack in need of OneOfMRF operation
/*!
 * keeps one result on stack, deleting the other one
 * zit er nu erg onhandig uit, maar later handig bij selectie
 * van nog meer resultaten, b.v een selectie van de ObjectLink results
 */
void calc::setMRFResult(RunTimeEnv* rte,size_t resultPos)
{
  PRECOND(resultPos<2);
  Field *results[2];
  //! pop in reverse
  for(size_t r=0; r<2; ++r)
    results[2-r-1]=rte->popField();
  rte->pushField(results[resultPos]);
  delete results[!resultPos];
}
