#include "stddefx.h"
#include "calc_timeoutput.h"
#include "calc_fieldexpr.h"
#include "calc_infoscript.h"
#include "calc_iscript.h"
#include "calc_fieldexprargs.h"
#include "calc_findsymbol.h"
#include "calc_operationtimer.h"
#include "calc_usepar.h"
#include "calc_tssoutputparameter.h"
#include "calc_indexselected.h"
#include "calc_fieldstack.h"
#include "calc_globargs.h"

calc::Timeoutput::Timeoutput(
  const calc::WriteInfo& w,
  const calc::UsePar&  par,
        calc::FieldExprArgs& args):
   calc::FieldArgs(par,major2op(OP_TIMEOUTPUT),args),
   calc::Statement(par),
   d_par(new calc::TssOutputParameter(par,w, true)),
   d_index(par.createSelector())
{
  // to check if not already defined as a different usersymbol
  script().findLeftParameter(par,VS_TSS);
  // if it does exist, we do not care, b->Add will
  // generate a redefinition error
  script().addSymbol(d_par);
  buildTypes();
}

calc::Timeoutput::~Timeoutput()
{
  delete d_index;
}

bool calc::Timeoutput::buildTypes()
{
  const char *msg=
 "Use a conversion function to pick a data type for second argument of timeoutput \npossible data type is ";
  restrictFieldArgs(0);
  const Args& args = fieldArgs();
  // timeoutput(arg0=idmap,arg1=value)
  args[1]->buildTypesRecursive(VS_FIELD);
  VS const vs = args[1]->vs();
  if (nrInSet(vs) > 1) {
     // but we may catch it later
    if (!d_buildTypesVisited) {
      // pcrcalc test68a
      d_buildTypesVisited=true;
      return true;
    }
    // pcrcalc test68
    args[1]->posError(msg+toString(vs));
   }
  d_par->setVs(vs);

  return false;
}

void calc::Timeoutput::prepareExecution()
{
  calc::FieldArgs::prepareExecution();
}

//! perform the time output
void calc::Timeoutput::run()
{
  OPERATION_TIMER("timeoutput",true);
  VS const resVS = d_par->vs();
  MAJOR_CODE const opCode = isIn(resVS,VS_SD) ? OP_TIMEOUTPUT_S : OP_TIMEOUTPUT_4;

  FieldStack stack;
  executeArgs(stack);

  GlobArgs args(major2op(opCode), scriptConst().compressor(),stack);

  d_par->AddTotss(d_index->select(),args.mapVals(),opCode == OP_TIMEOUTPUT_4);
}

void calc::Timeoutput::print(calc::InfoScript& i)const
{
  d_par->print(i);
  i.stream() << " = timeoutput (";
  calc::FieldArgs::print(i);
  i.stream() << ");<BR>";
}
