#include "stddefx.h"
#include "calc_tssoutputparameter.h"
#include "calc_filewriter.h"
#include "calc_tssoutputvalue.h"
#include "calc_iscript.h"
#include "calc_parspar.h"
#include "calc.h"      // TssRow stuff
#include "api.h"      // MAP_INT4 stuff

#include <stdexcept>

calc::TssOutputParameter::TssOutputParameter(
    const calc::ParsPar& par,
    const calc::WriteInfo& w,
    bool constant):
  calc::TssParameter(par,constant,false),
  d_value(nrElements(),nullptr)
{
  //! we must set this to get it written
  setReportPoint(par.position(),w);
}

calc::TssOutputParameter::~TssOutputParameter()
{
  for(auto & i : d_value)
    delete i;
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
  int r = 0;
  int const nrRows = id->NrRows(id);
  int c = 0;
  int const nrCols = id->NrCols(id);
  INT4 val = 0;
  INT4 max = -1; /* -1 is save, we need a positive id */

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
  const auto *idMap = (const MAP_INT4 *)args[0];
  if (scriptConst().currentTimeStep() == 1) { // initializing, first timestep
   PRECOND(nrInSet(d_vs) == 1);
   int const max = GetMaxId(idMap);
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

  size_t nrVals = 0;
  double *val = d_value[index]->getValueBuffer(nrVals);
  if (!val) // do not write this time step
    return;
  int result = 0;
  if (isClassTss) // VS_BNO
   result = AddToTssRowINT4(val,nrVals, idMap, (const MAP_INT4 *)args[1]);
  else
   result = AddToTssRowREAL8(val,nrVals,idMap, (const MAP_REAL8 *)args[1]);
  if (result)
    throw std::runtime_error("Failed to add data to timeseries");
}
