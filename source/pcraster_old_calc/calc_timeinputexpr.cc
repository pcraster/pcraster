#include "stddefx.h"
#include "calc_operationtimer.h"
#include "calc_timeinputexpr.h"
#include "calc_tssinputleaf.h"
#include "calc_timetable.h"
#include "calc_globresult.h"
#include "calc_globargs.h"
#include "calc_fieldstack.h"
#include "calc_infoscript.h"
#include "calc_iscript.h"
#include "calc_nonspatial.h"
#include "calc.h"
#include <stdexcept>

calc::TimeinputExpr::TimeinputExpr(
  const Element& pos,
  const Operator& op,
        UsePar &tss,
        FieldExprArgs& keyArgs):
  MixedExpr(pos,op, keyArgs)
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
  size_t const rowIndex = scriptConst().currentTimeStep()-1;

  try {

   FieldHandle id = stack.popReadOnly();
   if (!id->isSpatial()) {
     const auto *ns =
       dynamic_cast<const calc::NonSpatial *>(id.get_rep());
    int const colNr = static_cast<int>(ns->getValue());
    // no such column pcrcalc/test232
    if (colNr <= 0 || colNr >= tssTable->nrCols)
      throw std::runtime_error("No match in timeinput...");

    REAL8 *vPtr=tssTable->vals[rowIndex]+colNr;
    if (IS_MV_REAL8(vPtr)) // pcrcalc/test37e
      throw std::runtime_error("Read mv for non-spatial in timeinput...");
    stack.push(calc::FieldHandle(new calc::NonSpatial(vs(),*vPtr)));

   } else {
    GlobResult const result(VS_S,vs(),scriptConst().compressor());
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
