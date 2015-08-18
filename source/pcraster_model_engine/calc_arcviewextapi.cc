#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
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
static char *errStr(0);

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
extern "C" PCR_DLL_EXP const char  *pcrCalcErrorResult()
{
 return errStr;
}

static void initErrorResult()
{
 // clean up possible previous one
 if (!errStr)
  delete [] errStr;
 errStr=0;
}

static void evalErrorResult(const std::string& errorMsg)
{
  size_t l=errorMsg.size();
  if (l) {
    // there is error
    errStr = new char[l+1];
    strcpy(errStr,errorMsg.c_str());
 }
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
extern "C" PCR_DLL_EXP void pcrGetArcViewCheckDataItem(
    int   itemNr,
    int  *isStack,
    char *name)
{
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
extern "C" PCR_DLL_EXP int pcrArcViewScriptVerify(
  const char *scriptName)
{
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
     const char *d_cmdString;
  public:
     ArcViewExtRun(const char *cmdString):
       d_cmdString(cmdString)
     {
     }
  protected:
     int execute()
     {
       com::AppArgs args("",d_cmdString);
       if (!processArgs(args.argc(), args.argv()))
         return 0;
       // if reset by arguments, put back to this:
       // appOutput=APP_NOOUT; does not happen in ArcViewExtRun
       parse();
       return executeScript();
     }
};
};

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
extern "C" PCR_DLL_EXP int pcrCalcCmd(
  const char *cmdString)
{
 calc::ArcViewExtRun dc(cmdString);
 initErrorResult();
 int r = dc.run();
 evalErrorResult(dc.errorMsg());
 return r;
}
