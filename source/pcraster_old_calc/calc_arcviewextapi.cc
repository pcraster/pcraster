#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_CSTRING
#include <cstring>
#define INCLUDED_CSTRING
#endif
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_APPARGS
#include "com_appargs.h"
#define INCLUDED_COM_APPARGS
#endif
#ifndef INCLUDED_APPARGS
#include "appargs.h"
#define INCLUDED_APPARGS
#endif
#ifndef  INCLUDED_PCRDLL
#include "pcrdll.h"
#define  INCLUDED_PCRDLL
#endif

#ifdef WIN32
# ifndef INCLUDED_COM_WIN32
# include "com_win32.h"
# define INCLUDED_COM_WIN32
# endif
#endif

// Module headers.
#ifndef INCLUDED_CALC_DLLCALC
#include "calc_dllcalc.h"
#define INCLUDED_CALC_DLLCALC
#endif
#ifndef INCLUDED_CALC_CALC
#include "calc_calc.h"
#define INCLUDED_CALC_CALC
#endif
#ifndef INCLUDED_CALC_ARCVIEWEXTCHECKDATA
#include "calc_arcviewextcheckdata.h"
#define INCLUDED_CALC_ARCVIEWEXTCHECKDATA
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of all the API functions, with
  Ansi C bindings, used in the Arview extension.
*/


//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF STATIC
//------------------------------------------------------------------------------

//!  hold last error message, 0 if no error on last dll call
static std::string errStr;
extern bool esriArcView3Only;

//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF STATIC DLLCALC MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

//! 0 if last call OF ANY CALLS LISTED BELOW was no error
extern "C" PCR_DLL_FUNC(const char*) pcrCalcErrorResult()
{
 esriArcView3Only=true;
 if (errStr.empty())
   return 0;
 return errStr.c_str();
}

static void initErrorResult()
{
 // clean up possible previous one
 errStr.clear();
}

static void evalErrorResult(const std::string& errorMsg)
{
  errStr=errorMsg;
}



namespace calc {

//! holds info after pcrArcViewScriptVerify() is called
std::vector<ArcViewExtCheckData> arcViewExtCheckData;

//! setup check list (only used in avpcr/src/dll/PCRStep1.pas)
class ArcViewExtCheckDataClass: public DllCalc
{
  public:
     ArcViewExtCheckDataClass(const char *scriptName)
     {
       appIOstrategy = APP_IO_ESRIGRID;
       setScriptFile(scriptName);
       arcViewExtCheckData.clear();
     }
  protected:
     int execute()
     {
       parse();
       arcViewExtCheckData.clear();
       script().setArcViewExtCheckData(arcViewExtCheckData);
       return arcViewExtCheckData.size();
     }
};

}

//! return a name that must be checked on open in ArcView
/*!
   \param name must point to buffer with minimum size of 2048
   \return the absolute path to a map or stack directory in name
 */
extern "C" PCR_DLL_FUNC(void) pcrGetArcViewCheckDataItem(
    int   itemNr,
    int  *isStack,
    char *name)
{
 esriArcView3Only=true;
  if (itemNr < 0 ||
      static_cast<size_t>(itemNr) >= calc::arcViewExtCheckData.size())
    return;
  *isStack = calc::arcViewExtCheckData[itemNr].d_isStack;
  if (calc::arcViewExtCheckData[itemNr].d_name.size() < 2048)
   strcpy(name,calc::arcViewExtCheckData[itemNr].d_name.c_str());
}


//! verify a script and set up Esri output grids
/*!
    in case of error, pcrCalcErrorResult will return the message
   \returns -1 in case of error, nr of esri out grid names otherwise
 */
extern "C" PCR_DLL_FUNC(int) pcrArcViewScriptVerify(
  const char *scriptName)
{
 esriArcView3Only=true;
 initErrorResult();
 calc::ArcViewExtCheckDataClass sv(scriptName);
 int r = sv.run(-1);
 evalErrorResult(sv.errorMsg());
 return r;
}

namespace calc {
//! run command from Esri's ArcView PCRaster extension
/*! call pcrcalc in a command line fashion, without argv0
 */
class ArcViewExtRun : public DllCalc
{
  private:
    std::string d_cmdString;
  public:
     ArcViewExtRun(const char *cmdString):
       d_cmdString(cmdString)
     {
     }
  protected:
     int execute()
     {
       com::AppArgs args("ArcView",d_cmdString);
       if (!processArgs(args.argc(), args.argv()))
         return 0;
       // if reset by arguments, put back to this:
       // appOutput=APP_NOOUT; does not happen in ArcViewExtRun
       parse();
       return executeScript();
     }
};
}


static int runCmd(
  const char *cmdString)
{
 calc::ArcViewExtRun dc(cmdString);
 initErrorResult();
 int r = dc.run();
 evalErrorResult(dc.errorMsg());
 return r;
}

//! run a calc command line (only used in avpcr/src/dll/PCRStep1.pas)
/*!
 *  in PCRStep1 only called with
 *  <ol>
 *   <li> --esrigrid -f scriptFile</li>
 *   <li> --esrigrid -K esriGrid</li>
 *  </ol>
 *
    \param cmdString is a string with all arguments, except argv0
      space is used to seperate args, quoting to preserve space is
      not yet supported --nothing is prepended, nothing printed
      and errors message are catched and stored local, and can
      be retrieved by calling pcrCalcErrorResult()

   \returns exit code as defined by arguments

 */
extern "C" PCR_DLL_FUNC(int) pcrCalcCmd(
  const char *cmdString)
{
 esriArcView3Only=true;
 return runCmd(cmdString);
}

extern "C" PCR_DLL_FUNC(int) pcrCalcMBCmd(
  const char *cmdString)
{
 esriArcView3Only=false;
 return runCmd(cmdString);
}
