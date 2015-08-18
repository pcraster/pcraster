#include "stddefx.h"

#ifndef INCLUDED_CALC_BRANCHEXPR
#include "calc_branchexpr.h"
#define INCLUDED_CALC_BRANCHEXPR
#endif

#ifndef INCLUDED_CALC_OPERATOR
#include "calc_operator.h"
#define INCLUDED_CALC_OPERATOR
#endif

#ifndef INCLUDED_CALC_FINDSYMBOL
#include "calc_findsymbol.h"
#define INCLUDED_CALC_FINDSYMBOL
#endif

#ifndef INCLUDED_CALC_INFOSCRIPT
#include "calc_infoscript.h"
#define INCLUDED_CALC_INFOSCRIPT
#endif

#ifndef INCLUDED_COM_AUTOARRAYPTR
#include "com_autoarrayptr.h"
#define INCLUDED_COM_AUTOARRAYPTR
#endif

#ifndef INCLUDED_STDEXCEPT
#include <stdexcept>
#define INCLUDED_STDEXCEPT
#endif

calc::BranchExpr::BranchExpr(
  const Element& pos,
  const Operator&  op,
        FieldExprArgs& fieldArgs):
  FieldArgs(pos,op,fieldArgs),
  FieldExpr(pos),
  d_type(op)
{
  buildTypes();
}

size_t calc::BranchExpr::nrArgs() const {
  return nrFieldArgs();
}

void calc::BranchExpr::buildTypesRecursive(VS resultVsSet)
{
  if (isSubset(resultVsSet,vs())) // expr is polymorphic
    d_type.restrictSystem(resultVsSet,spatial());
  Args& args = fieldArgs();
  for(size_t i = 0; i < nrArgs(); i++)
    args[i]->buildTypesRecursive(vs());
  buildTypes();
}

void calc::BranchExpr::defaultBuildType(
  VS& newVs, bool& isSpatial)
{
  Args& args = fieldArgs();
  if (nrInSet(newVs) > 1) {
     // type derived from first argwith
     // multiple types in its argdefinition
     // e.g. areaminimum
     int i = op().firstPolyArg();
     newVs = args[i]->vs();
  }
  if (d_type.spatialDerived()) {
    // spatial type is spatial if one of the operands
    // is spatial
    for(size_t i=0; i < nrArgs(); i++)
      if ( args[i]->spatial())
       isSpatial = true;
  }
}

void calc::BranchExpr::prepareExecution()
{
  FieldArgs::prepareExecution();
}

void calc::BranchExpr::skipExecution()
{
  FieldArgs::skipExecution();
}

void calc::BranchExpr::buildTypes()
{
  // xml if op has attr special set to true
  if((int)(op().opCode()) < OP_SIMPLE_RULE)
    special();

  restrictFieldArgs(0);

  Args& args = fieldArgs();

  VS newVs = d_type.vs();
  bool isSpatial = d_type.spatial();

  if (op().exec() == EXEC_EXTERN) {
    com::auto_array_ptr<OP_ARGS> argsExt(new OP_ARGS[nrArgs()]);
    for(size_t i=0; i < nrArgs(); i++) {
      argsExt[i].vs = args[i]->vs();
      argsExt[i].st = args[i]->spatial()
          ? ST_SPATIAL : ST_NONSPATIAL;
    }
    externalBuildType( newVs,isSpatial,
        op().opCode(), argsExt.get(), nrArgs());
  }
  else
    defaultBuildType(newVs,isSpatial);

  d_type.restrictSystem(newVs,isSpatial);

  if (op().cg() == CG_COMM && nrArgs() == 2 && args[1]->isEndNode()) {
    // flip to compute +,* most efficiently
    Args newArgs(2);
    newArgs[1] = args[0];
    newArgs[0] = args[1];
    args = newArgs;
  }

   /* if I don't want to implement (max SN) and (min SN)
    *  and other communative operators then
    *  I have to sort the arguments here
    *  putting spatials before non-spatials
    */
    if (op().cg() == CG_COMM || op().opCode() == OP_MAX || op().opCode() == OP_MIN) {
      Args newArgs(nrArgs());
      size_t pos=0;
      bool order[2] = { false, true }; // first non-spatial then spatials
      for(size_t o=0; o<2; o++)
       for(size_t i=0; i < nrArgs(); i++)
        if (args[i]->spatial() == order[o])
         newArgs[pos++] = args[i];
      POSTCOND(pos == nrArgs());
      args = newArgs;
    }
}

calc::FieldType& calc::BranchExpr::restrictType()
{
 return d_type;
}

const calc::FieldType& calc::BranchExpr::fieldType() const
{
  return d_type;
}

void calc::BranchExpr::argCombError(
  int nr,      /* offending argument nr        */
  VS  prevVs) const
{
  const calc::FieldExpr  *offendingArg = fieldArgs()[nr];
  std::ostringstream msg;
  msg << op().strArg(nr) << ": type is " << toString(offendingArg->vs()) <<", while ";
  if (op().syntax() == "function") {
     int prev = (nr > 1) ? -1 : 0;
     /* prev forcing a type or
      * -1 if the comb of arg 0 to nr-1
      * forced an illegal type for arg nr
      */
     if (prev == -1) {
      /* pcrcalc/test259a */
      msg << "previous argument types are ";
     }
     else {
      /* pcrcalc/test259 */
      msg << "type of argument nr. "<< (prev+1) <<" is ";
     }
  }  else {
    /* pcrcalc/test260 */
     PRECOND(nr == 1);
     msg << "type of left argument is ";
  }
  msg << toString(prevVs);
  offendingArg->posError(msg.str());
}

void calc::BranchExpr::special()
{
  Args& args = fieldArgs();
  VS newVs = VS_FIELD; // depends on arguments
  int startArg = (op().opCode() == OP_IF_ELSE) ? 1 : 0;
  switch(op().opCode()) {
    case OP_MIN: case OP_MAX: case OP_COVER:
    // xml: if (lastArg has repeat ???
      newVs = vs();
      // fallthrough
    case OP_NE: case OP_EQ:
    case OP_GT: case OP_GE:
    case OP_LT: case OP_LE:
    case OP_IF_ELSE:
      /*  derived rule:
       *  all polymorphic arguments must be of the
       *  same type, e.g. more types allowed as long as
       *  all arguments have the same type
       */
        for(size_t i=startArg; i <nrArgs(); i++) {
         VS oldVs = newVs;
         newVs = intersect(newVs,args[i]->vs());
         if (newVs == VS_UNKNOWN)
            argCombError(i,oldVs);
        }
        for(size_t i=startArg; i <nrArgs(); i++)
         args[i]->restrictType().restrictSystem(newVs,args[i]->spatial());
      break;
    default: POSTCOND(FALSE);
  }
}

void calc::BranchExpr::print(calc::InfoScript &si) const
{
  const Args& args = fieldArgs();
  if (op().syntax() == "function") {
    d_type.print(si,op());
    si.stream() << "(";
    for(size_t i = 0; i < nrArgs(); i++) {
      args[i]->print(si);
      if (i < nrArgs()-1)
        si.stream() << ",";
    }
    si.stream() << ")";
  } else {
    if (nrArgs() == 1) {
     d_type.print(si,op().name());
     args[0]->print(si);
    } else {
     d_type.print(si,"<SMALL>[[</SMALL>");
     args[0]->print(si);
     si.stream() << " " << op().name() << " ";
     args[1]->print(si);
     d_type.print(si,"<SMALL>]]</SMALL>");
    }
  }
}

