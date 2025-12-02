#include "stddefx.h"
#include "calc_calc.h"
#include "com_exception.h"
#include "com_pathname.h"
#include "calc_globallibdefs.h"
#include "calc_quitforexitoption.h"
#include "calc_quitforprogresscallback.h"
#include "calc_progresscallback.h"
#include "calc_catchallexceptions.h"
#include "calc_parserinput.h"

/*!
  \file
  This file contains the implementation of the Calc class.
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC CALC MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF CALC MEMBERS
//------------------------------------------------------------------------------

//! ctor
/*! sets default for doing something
    with a script
 */
calc::Calc::Calc(std::ostream &stdOut, std::ostream &stdErr) : d_stdOut(stdOut), d_stdErr(stdErr)
{
  globalInit();
}

//! dtor
calc::Calc::~Calc()
{
  globalEnd();
}

//! parse the input script, building symbol table and code tree
/*!
 *  \todo
 *    can do better on syntax error, by remembering the last correct
 *    create (createPosition) in lexer, the msg: "Past this point"
 */
void calc::Calc::parse()
{
}

//! execute the script parsed in parse()
/*!
   sets executeScriptStatus()
   \returns the exit value, Script::exitVal(), of the script
 */
int calc::Calc::executeScript()
{
  try {
    d_executeScriptStatus = ErrorExecScript;
    d_executeScriptStatus = FinishedExecScript;
  } catch (const QuitForExitOption &) {
    // thrown by
    // calc::Script::processFileOutputValue
    d_executeScriptStatus = FileOutputValueExecScript;
  } catch (const QuitForProgressCallBack &) {
    // Script::updateProgress is already updated
    d_executeScriptStatus = CanceledExecScript;
  }
  PRECOND(false);
  return 0;  // script().exitVal();
}

//! returns the  ExecuteScriptStatus set in last call to executeScript()
calc::ExecuteScriptStatus calc::Calc::executeScriptStatus() const
{
  return d_executeScriptStatus;
}

//! runs execute(), catching all errors and putting it on Calc::d_stdErr
/*!
 *   No exception will leave this method, unknown exceptions are reported
 *   as programming error on the error stream
 *
 *    \returns return value of execute()
 */
int calc::Calc::run(int returnValueOnException)
{
  int status = returnValueOnException;
  extern void dumpZero();

  TRY_ALL
  {
    status = execute();
  }
  CATCH_ALL_EXCEPTIONS(d_stdErr);
  return status;
}

void calc::Calc::setProgressCallBack(ProgressCallBack *)
{
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
