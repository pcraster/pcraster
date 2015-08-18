#include "stddefx.h"

#ifndef INCLUDED_CALC_METHODOPERATOR
# include "calc_methodoperator.h"
#define INCLUDED_CALC_METHODOPERATOR
#endif

#ifndef INCLUDED_CALC_OPERATOR
# include "calc_operator.h"
#define INCLUDED_CALC_OPERATOR
#endif

#ifndef INCLUDED_CALC_MODELLINK
# include "calc_modellink.h"
#define INCLUDED_CALC_MODELLINK
#endif

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
  for (size_t i=0; i < s.d_input.size(); i++)
    d_op->pushBackArg(s.d_input[i].vs, s.d_input[i].st);
}

calc::MethodOperator::~MethodOperator()
{
  delete d_op;
}
