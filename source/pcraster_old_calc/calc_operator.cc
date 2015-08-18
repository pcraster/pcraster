#include "stddefx.h"

#ifndef INCLUDED_CALC_OPERATOR
#include "calc_operator.h"
#define INCLUDED_CALC_OPERATOR
#endif

#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"    // nrInSet
#define INCLUDED_CALC_VS
#endif



calc::Operator::Operator(const std::string &name,
    MAJOR_CODE major,SYNTAX syntax,
    VS vs,ST st, int nrArgs,EXEC exec,int execId, CG cg):
    d_name(name),
    d_major(major),d_syntax(syntax),
    d_vs(vs),d_st(st),d_nrArgs(nrArgs),d_exec(exec),
    d_execId(execId),d_cg(cg)
{
#ifdef DEBUG
  /* all CG_VARARG's are EXEC_POLY's */
  if (d_cg == CG_VARARG)
    POSTCOND(d_exec == EXEC_POLY);
  if(exec == EXEC_SAME_UN) {
    ;
    // this category only works on fields
    // CW make that assertion in Operator creation!
  }
#endif
}

void calc::Operator::pushBackArg(VS vs, ST st)
{
  OP_ARGS a = { vs, st };
  d_argPars.push_back(a);
}



std::string calc::Operator::syntax() const
{
  if (d_syntax == SYNTAX_FUNC)
    return "function";
  POSTCOND(d_syntax == SYNTAX_OP);
  return "operator";
}

VS calc::Operator::argVs(int argNr) const {
  if (d_nrArgs < 0 && (nrArgsDef() <= argNr))
    /* last one describes all remaining arguments */
    argNr = nrArgsDef()-1;
  return d_argPars[argNr].vs;
}

ST calc::Operator::argSt(int argNr) const {
  if (d_nrArgs < 0 && (nrArgsDef() <= argNr))
    /* last one describes all remaining arguments */
    argNr = nrArgsDef()-1;
  return d_argPars[argNr].st;
}

//! returns first argument that can have multiple types
int calc::Operator::firstPolyArg() const
{
  for(int i=0; i < nrArgsDef(); i++)
    if (nrInSet(d_argPars[i].vs) > 1)
      return i;
  POSTCOND(FALSE); // CW NEVER
  return -1;
}

std::string calc::Operator::strArg(int nr) const // return an argument description
{
  std::ostringstream msg;
  if (syntax() == "function") {
    msg << "argument nr. "<<(nr+1);
    msg << " of function '";
  } else {
    switch(nrArgs()) {
     case 1: if (d_major == OP_TEST_UNTIL)
               return "until condition";
             msg << "operand";
             break;
     case 2: PRECOND(nr==0 || nr ==1);
        if (nr == 0)
           msg << "left operand";
        else
           msg << "right operand";
        break;
     default: POSTCOND(FALSE); // CW NEVER
    }
     msg << " of operator '";
  }
  msg << name() <<"'";
  return msg.str();
}

