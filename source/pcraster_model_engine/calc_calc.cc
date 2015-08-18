#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_CALC
#include "calc_calc.h"
#define INCLUDED_CALC_CALC
#endif
// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif

// Module headers.
#ifndef INCLUDED_CALC_GLOBALLIBDEFS
#include "calc_globallibdefs.h"
#define INCLUDED_CALC_GLOBALLIBDEFS
#endif

#ifndef INCLUDED_CALC_QUITFOREXITOPTION
#include "calc_quitforexitoption.h"
#define INCLUDED_CALC_QUITFOREXITOPTION
#endif
#ifndef INCLUDED_CALC_QUITFORPROGRESSCALLBACK
#include "calc_quitforprogresscallback.h"
#define INCLUDED_CALC_QUITFORPROGRESSCALLBACK
#endif
#ifndef INCLUDED_CALC_PROGRESSCALLBACK
#include "calc_progresscallback.h"
#define INCLUDED_CALC_PROGRESSCALLBACK
#endif

#ifndef INCLUDED_CALC_CATCHALLEXCEPTIONS
#include "calc_catchallexceptions.h"
#define INCLUDED_CALC_CATCHALLEXCEPTIONS
#endif

#ifndef INCLUDED_CALC_PARSERINPUT
#include "calc_parserinput.h"
#define INCLUDED_CALC_PARSERINPUT
#endif

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
calc::Calc::Calc(
    std::ostream& stdOut,
    std::ostream& stdErr):
  d_executeScriptStatus(IsRunningExecScript),
  d_stdOut(stdOut),
  d_stdErr(stdErr)
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
  }
  catch (const QuitForExitOption&) {
   // thrown by
   // calc::Script::processFileOutputValue
   d_executeScriptStatus = FileOutputValueExecScript;
   }
  catch (const QuitForProgressCallBack& ){
     // Script::updateProgress is already updated
    d_executeScriptStatus = CanceledExecScript;
   }
 PRECOND(false);
 return 0; // script().exitVal();
}

//! returns the  ExecuteScriptStatus set in last call to executeScript()
calc::ExecuteScriptStatus
   calc::Calc::executeScriptStatus() const
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
 int status=returnValueOnException;
 extern void dumpZero();

 TRY_ALL {
   status = execute();
 } CATCH_ALL_EXCEPTIONS(d_stdErr);
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

