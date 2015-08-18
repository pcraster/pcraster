#include "stddefx.h"

#ifndef INCLUDED_CALC_TSSOUTPUTPARAMETER
#include "calc_tssoutputparameter.h"
#define INCLUDED_CALC_TSSOUTPUTPARAMETER
#endif

#ifndef INCLUDED_CALC_FILEWRITER
#include "calc_filewriter.h"
#define INCLUDED_CALC_FILEWRITER
#endif

#ifndef INCLUDED_CALC_TSSOUTPUTVALUE
#include "calc_tssoutputvalue.h"
#define INCLUDED_CALC_TSSOUTPUTVALUE
#endif

#ifndef INCLUDED_CALC_ISCRIPT
#include "calc_iscript.h"
#define INCLUDED_CALC_ISCRIPT
#endif

#ifndef INCLUDED_CALC_PARSPAR
#include "calc_parspar.h"
#define INCLUDED_CALC_PARSPAR
#endif

#ifndef INCLUDED_CALC
#include "calc.h"      // TssRow stuff
#define INCLUDED_CALC
#endif

#ifndef INCLUDED_API
#include "api.h"      // MAP_INT4 stuff
#define INCLUDED_API
#endif

#ifndef INCLUDED_STDEXCEPT
#include <stdexcept>
#define INCLUDED_STDEXCEPT
#endif

calc::TssOutputParameter::TssOutputParameter(
    const calc::ParsPar& par,
    const calc::WriteInfo& w,
    bool constant):
  calc::TssParameter(par,constant,false),
  d_vs(VS_FIELD),
  d_value(nrElements(),0)
{
  //! we must set this to get it written
  setReportPoint(par.position(),w);
}

calc::TssOutputParameter::~TssOutputParameter()
{
  for(size_t i=0; i < d_value.size(); i++)
    delete d_value[i];
}

//! returns the field value scale of tss result
VS  calc::TssOutputParameter::vs() const
{
  return d_vs;
}


void calc::TssOutputParameter::setVs(VS vsOfOutput)
{
  d_vs= vsOfOutput;
}

void calc::TssOutputParameter::goInScope()
{
}

static int GetMaxId(
  const MAP_INT4 *id)
{
  int r,nrRows = id->NrRows(id);
  int c,nrCols = id->NrCols(id);
  INT4 val, max = -1; /* -1 is save, we need a positive id */

  id->SetGetTest(GET_MV_TEST, id);
  for(r=0;r <nrRows;r++)
   for(c=0;c <nrCols;c++)
    if (id->Get(&val,r,c, id))
     max = MAX(max,val);
  return max;
}

//! add a line of values to the tss
void calc::TssOutputParameter::AddTotss(
  size_t index, const void **args, bool isClassTss)
{
  const MAP_INT4 *idMap = (const MAP_INT4 *)args[0];
  if (scriptConst().currentTimeStep() == 1) { // initializing, first timestep
   PRECOND(nrInSet(d_vs) == 1);
   int max = GetMaxId(idMap);
   if (max > 0 &&
      ( !d_value[index] /* prevent multiple alloc: see pcrcalc/test317 */))
     d_value[index] =
       new calc::TssOutputValue(calc::FileWriter(*this,index,d_vs),
                               max, d_vs);
   /* CW if max <= 0
    * maybe message/warning: idMap contained only MV's at first timestep
    */
  }

  if (!d_value[index])
    return; // no tss was created, during the first timestep

  size_t nrVals;
  double *val = d_value[index]->getValueBuffer(nrVals);
  if (!val) // do not write this time step
    return;
  int result;
  if (isClassTss) // VS_BNO
   result = AddToTssRowINT4(val,nrVals, idMap, (const MAP_INT4 *)args[1]);
  else
   result = AddToTssRowREAL8(val,nrVals,idMap, (const MAP_REAL8 *)args[1]);
  if (result)
    throw std::runtime_error("Failed to add data to timeseries");
}
