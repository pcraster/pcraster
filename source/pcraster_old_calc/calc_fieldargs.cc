#include "stddefx.h"
#include "calc_fieldargs.h"
#include "calc_operator.h"
#include "calc_fieldexprargs.h"
#include "calc_fieldexpr.h"
#include "calc_fieldtype.h"
#include "calc_infoscript.h"

#include <cmath>


/*!
 * \todo
 *   vervang door std::shared_ptrs?
 */
void calc::cleanUp(const FieldExprArgs&  argsVect)
{
  for(auto i : argsVect)
    delete i;
}

/*!
 * \todo
 *   vervang door std::shared_ptrs?
 *   waarom deze 2 versies
 */
void calc::cleanUp(FieldExprArgs&  argsVect)
{
  for(auto & i : argsVect)
    delete i;
  argsVect.resize(0);
}

void calc::FieldArgs::checkArgs()
{
/* Check number of arguments
 */
  int argTest = std::abs(op().nrArgs()) - (int)nrFieldArgs();
  argTest = (argTest == 0) ? 0 : argTest/std::abs(argTest);

  std::string msg("");
  switch (argTest) {
   //   case 0:  if (op().nrArgs() != -1) break;
   //   /* if it is -1 then fall through */
   case 1:  msg = "not enough arguments specified"; break;
   case -1: if (op().nrArgs() >= 0) {
         /* if < 0 then the last arg can be repeated many times */
         msg = "too many arguments specified";
       }
       break; // pcrcalc/test14
  }
  if (msg != "") {
    /* pcrcalc/test25[23] */
     std::string const str(op().syntax()+" "+quote(op().name())+" "+msg);
     if (nrFieldArgs())
      d_args[0]->posError(str);
     else // pcrcalc/test252a
      d_pos.posError(str);
  }
}

/*! Verify correct nr of argument
 */
calc::FieldArgs::FieldArgs(
  const Element&       p,
  const Operator&      o,
        FieldExprArgs& inArgs):
  d_pos(p),d_op(o),d_args(inArgs),d_ownArgs(true)
{
 inArgs.resize(0); // resize now passed to calc::FieldArgs object
 try {
     checkArgs();
 } catch (...) {
    cleanUp(d_args);
    throw;
 }
}

/*! Verify correct nr of argument
 * hack for modellink*
 */
calc::FieldArgs::FieldArgs(
  const Element&       p,
  const Operator&      o,
  const FieldExprArgs& inArgs):
  d_pos(p),d_op(o),d_args(inArgs),d_ownArgs(false)
{
 checkArgs();
}

calc::FieldArgs::~FieldArgs()
{
    if (d_ownArgs)
      cleanUp(d_args);
}

const calc::Operator& calc::FieldArgs::op() const
{ return d_op;}

//! restrict the field arguments
/*! check if arguments match with the types of the operation
 *  <BR>
 *  the fieldArgOffset denote the incr for the arg nr. reported to the user
 */
void calc::FieldArgs::restrictFieldArgs(
  size_t fieldArgOffset)
{
  // check if arguments match with the types of the operation
  try {
    for (size_t i = 0; i < d_args.size(); i++) {
      d_args[i]->restrictType().restrictArg(op(),i,fieldArgOffset);
    }
   }catch(SyntaxArgumentError& msg) {
     d_pos.posError(msg.d_s);
   }
}

void calc::FieldArgs::executeArgs(calc::FieldStack& stack)
{
  // execute the arguments
  /* same order (=stack arrangement )
   * as needed by expanding COVER
   * into COVERS
   * => leftmost arg on top
   */
  for (int i = (int)d_args.size()-1; i >= 0; i--)
      d_args[i]->execute(stack);
}

void calc::FieldArgs::prepareExecution()
{
  int downTo = 0;
  switch(op().opCode()) {
    case OP_IF_ELSE:
    case OP_IF:
          d_args[0]->prepareExecution();
      downTo = 1;
      break;
    default:
      downTo = 0;
  }
  for (int i = (int)d_args.size()-1; i >= downTo; i--)
      d_args[i]->prepareExecution();
}

void calc::FieldArgs::skipExecution()
{
  int downTo = 0;
  switch(op().opCode()) {
    case OP_IF_ELSE:
    case OP_IF:
          d_args[0]->skipExecution();
      downTo = 1;
      break;
    default:
      downTo = 0;
  }
  for (int i = (int)d_args.size()-1; i >= downTo; i--)
      d_args[i]->skipExecution();
}

void calc::FieldArgs::print(calc::InfoScript &si) const
{
  for(size_t i = 0; i < nrFieldArgs(); i++) {
    d_args[i]->print(si);
    if (i < nrFieldArgs()-1)
      si.stream() << ",";

  }
}
