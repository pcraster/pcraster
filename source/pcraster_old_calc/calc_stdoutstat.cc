#include "stddefx.h"

#ifndef INCLUDED_CALC_STDOUTSTAT
# include "calc_stdoutstat.h"
#define INCLUDED_CALC_STDOUTSTAT
#endif

#ifndef INCLUDED_CALC_FIELDEXPR
# include "calc_fieldexpr.h"
#define INCLUDED_CALC_FIELDEXPR
#endif

#ifndef INCLUDED_CALC_FIELDSTACK
# include "calc_fieldstack.h"
#define INCLUDED_CALC_FIELDSTACK
#endif

#ifndef INCLUDED_CALC_NONSPATIAL
# include "calc_nonspatial.h"
#define INCLUDED_CALC_NONSPATIAL
#endif

#ifndef INCLUDED_CALC_ISCRIPT
# include "calc_iscript.h"
#define INCLUDED_CALC_ISCRIPT
#endif

#ifndef INCLUDED_APPARGS
# include "appargs.h"
#define INCLUDED_APPARGS
#endif

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
  const calc::NonSpatial *f = dynamic_cast<const calc::NonSpatial *>(fh.get_rep());
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
