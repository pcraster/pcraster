#include "stddefx.h"

#ifndef INCLUDED_CALC_GLOBARGS
#include "calc_globargs.h"
#define INCLUDED_CALC_GLOBARGS
#endif

#ifndef INCLUDED_CALC_GLOBARG
#include "calc_globarg.h"
#define INCLUDED_CALC_GLOBARG
#endif

#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif

#ifndef INCLUDED_CALC_OPERATOR
#include "calc_operator.h"
#define INCLUDED_CALC_OPERATOR
#endif

/*! pop op.nrArgs() fields from stack and wrap each arg in an calc::ApiMap
 *  object if nrActualArgs > 0 use that as the actual nr of arguments
 */
calc::GlobArgs::GlobArgs(
    const Operator& op,
    const Compressor& compressor,
    FieldStack& stack,
    size_t nrActualArgs):
  d_nrArgs( nrActualArgs ? nrActualArgs : op.nrArgs()),
  d_fields(stack,d_nrArgs),
  d_args(d_nrArgs)
{
#ifdef DEBUG_DEVELOP
  // test/pcrcalc224c
  if (!nrActualArgs)
    POSTCOND(op.nrArgs() >= 0); // not a var arg def
#endif
  d_vals = new void *[d_nrArgs];
  for (size_t i=0; i < d_nrArgs; i++) {
   d_args[i] = new GlobArg(op.argVs(i),d_fields[i], compressor);
   d_vals[i] = d_args[i]->MAPinterface();
 }
}

//! return array of opaque MAP_* ptr's
const void ** calc::GlobArgs::mapVals()
{ return (const void **)d_vals; }

calc::GlobArgs::~GlobArgs() {
  for (size_t i=0; i < d_nrArgs; i++)
    delete d_args[i];
  delete[] d_vals;
}
