#include "stddefx.h"
# include "calc_methodoperator.h"
# include "calc_operator.h"
# include "calc_modellink.h"

calc::MethodOperator::MethodOperator(
  const std::string& name,
  const ModelLinkMethodSignature& s)
{
  d_op = new Operator(name,
    OP_NOP, // DONT_CARE
    SYNTAX_FUNC,
    VS_FIELD,// DONT_CARE
    ST_DERIVED,// DONT_CARE
    s.d_input.size(),
    EXEC_EXTERN,// DONT_CARE
    0,// DONT_CARE
    CG_PLAIN);// DONT_CARE
  for (const auto & i : s.d_input)
    d_op->pushBackArg(i.vs, i.st);
}

calc::MethodOperator::~MethodOperator()
{
  delete d_op;
}
