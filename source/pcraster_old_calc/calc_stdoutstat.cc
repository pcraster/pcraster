#include "stddefx.h"
#include "calc_stdoutstat.h"
#include "calc_fieldexpr.h"
#include "calc_fieldstack.h"
#include "calc_nonspatial.h"
#include "calc_iscript.h"
#include "appargs.h"

#define FORGOT_ASS  "Can not write spatial output using outputfile"

calc::StdoutStatement::StdoutStatement(
  calc::FieldExpr *right)
  : calc::Statement(*right),d_expr(right)
{
// #define FORGOT_ASS  "Assignment forgotten (expected <id> = <expr>, read <expr>)"
// NO LONGER  if (right->IsTimeoutput()) // pcrcalc/test37b
// NO LONGER    posError(FORGOT_ASS) generates syntax error now-> fileoutput
  buildTypes();
}

bool calc::StdoutStatement::buildTypes()
{
  if (d_expr->spatial()) // pcrcalc/test20
    posError(FORGOT_ASS);
  return false;
}

void calc::StdoutStatement::prepareExecution()
{
  d_expr->prepareExecution();
}

void calc::StdoutStatement::run()
{
  calc::FieldStack stack;

  d_expr->execute(stack);
  calc::FieldHandle fh = stack.popReadOnly();
  const auto *f = dynamic_cast<const calc::NonSpatial *>(fh.get_rep());
  POSTCOND(f);

  double val = f->getValue();
  if (d_expr->vs() == VS_D) // pcrcalc/test7[67]
        val = AppOutputDirection(val);
  script().processFileOutputValue(val);
}

void calc::StdoutStatement::print(calc::InfoScript& i)const
{
  d_expr->print(i);
}
