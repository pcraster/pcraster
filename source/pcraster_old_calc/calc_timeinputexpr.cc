#include "stddefx.h"
#ifndef INCLUDED_CALC_OPERATIONTIMER
#include "calc_operationtimer.h"
#define INCLUDED_CALC_OPERATIONTIMER
#endif

#ifndef INCLUDED_CALC_TIMEINPUTEXPR
# include "calc_timeinputexpr.h"
#define INCLUDED_CALC_TIMEINPUTEXPR
#endif

#ifndef INCLUDED_CALC_TSSINPUTLEAF
#include "calc_tssinputleaf.h"
#define INCLUDED_CALC_TSSINPUTLEAF
#endif

#ifndef INCLUDED_CALC_TIMETABLE
#include "calc_timetable.h"
#define INCLUDED_CALC_TIMETABLE
#endif

#ifndef INCLUDED_CALC_GLOBRESULT
#include "calc_globresult.h"
#define INCLUDED_CALC_GLOBRESULT
#endif
#ifndef INCLUDED_CALC_GLOBARGS
#include "calc_globargs.h"
#define INCLUDED_CALC_GLOBARGS
#endif

#ifndef INCLUDED_CALC_FIELDSTACK
#include "calc_fieldstack.h"
#define INCLUDED_CALC_FIELDSTACK
#endif

#ifndef INCLUDED_CALC_INFOSCRIPT
#include "calc_infoscript.h"
#define INCLUDED_CALC_INFOSCRIPT
#endif

#ifndef INCLUDED_CALC_ISCRIPT
#include "calc_iscript.h"
#define INCLUDED_CALC_ISCRIPT
#endif

#ifndef INCLUDED_CALC_NONSPATIAL
#include "calc_nonspatial.h"
#define INCLUDED_CALC_NONSPATIAL
#endif

#ifndef INCLUDED_STDEXCEPT
#include <stdexcept>
#define INCLUDED_STDEXCEPT
#endif

#include "calc.h"

calc::TimeinputExpr::TimeinputExpr(
  const Element& pos,
  const Operator& op,
        UsePar &tss,
        FieldExprArgs& keyArgs):
  MixedExpr(pos,op, keyArgs),
  d_tss(0)
{
  try {
    d_tss = new TssInputLeaf(tss,fieldType().vs());
    buildTypes();
  } catch (...) {
    cleanUp();
    throw;
  }
}

void calc::TimeinputExpr::cleanUp()
{
 delete d_tss;
}

calc::TimeinputExpr::~TimeinputExpr()
{
  cleanUp();
}

void calc::TimeinputExpr::execute(FieldStack& stack)
{
  OPERATION_TIMER("timeinput....",spatial());
  executeArgs(stack);


  const TimeTable *tss = d_tss->execute();
  const TIME_TABLE *tssTable = tss->tss();
  size_t rowIndex = scriptConst().currentTimeStep()-1;

  try {

   FieldHandle id = stack.popReadOnly();
   if (!id->isSpatial()) {
     const calc::NonSpatial *ns =
       dynamic_cast<const calc::NonSpatial *>(id.get_rep());
    int colNr = static_cast<int>(ns->getValue());
    // no such column pcrcalc/test232
    if (colNr <= 0 || colNr >= tssTable->nrCols)
      throw std::runtime_error("No match in timeinput...");

    REAL8 *vPtr=tssTable->vals[rowIndex]+colNr;
    if (IS_MV_REAL8(vPtr)) // pcrcalc/test37e
      throw std::runtime_error("Read mv for non-spatial in timeinput...");
    stack.push(calc::FieldHandle(new calc::NonSpatial(vs(),*vPtr)));

   } else {
    GlobResult result(VS_S,vs(),scriptConst().compressor());
    stack.push(id); // GlobArgs must read from stack
    GlobArgs   args(op(), scriptConst().compressor(), stack);
    (void)TimeInputSeries(
      (struct MAP_REAL8 *)result.MAPinterface(),
      (const struct MAP_INT4 *)args.mapVals()[0],
      tssTable,
      rowIndex);
    stack.push(result.createField());

   }
  } catch(std::exception& v) {
    calc::Element::runtimeError(v.what());
  }
}

void calc::TimeinputExpr::print(calc::InfoScript& si)const
{
  fieldType().print(si,op());
  si.stream() << "(";
  d_tss->print(si);
  printFieldArgs(si);
  si.stream() << ")";
}
