#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_CALC
#include "calc_calc.h"
#define INCLUDED_CALC_CALC
#endif
#ifndef INCLUDED_CALC_RUNSCRIPT
#include "calc_runscript.h"
#define INCLUDED_CALC_RUNSCRIPT
#endif

// Library headers.
#ifndef INCLUDED_MATHX
#include "mathx.h"
#define INCLUDED_MATHX
#endif
#ifndef INCLUDED_MISC
#include "misc.h"
#define INCLUDED_MISC
#endif
#ifndef INCLUDED_APPARGS
#include "appargs.h"
#define INCLUDED_APPARGS
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_COM_TUNE
#include "com_tune.h"
#define INCLUDED_COM_TUNE
#endif
#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif

// Module headers.
#ifndef INCLUDED_CALC_LIBERROR
#include "calc_liberror.h" // HandleLibError
#define INCLUDED_CALC_LIBERROR
#endif
#ifndef INCLUDED_CALC_FILE
#include "calc_file.h"
#define INCLUDED_CALC_FILE
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
#ifndef INCLUDED_CALC_MODELPARSER
#include "calc_modelparser.h"
#define INCLUDED_CALC_MODELPARSER
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

//! all things we want to get rid off
/*!
 * \todo get rid off it
 */
void calc::globalInit()
{
  com::tune();
  // the mathx library
  SetRan(0); /* time seed */

  // the misc library
  exitOnError =0;
  errorPrefixMsg = "";
  errorHandler = HandleLibError;

  // the app library
  AppSetGlobalArgsDefaults();
}

//! ctor
/*! sets default for doing something
    with a script
 */
calc::Calc::Calc(
    std::ostream& stdOut,
    std::ostream& stdErr):
  d_executeScriptStatus(IsRunningExecScript),
  d_stdOut(stdOut),
  d_stdErr(stdErr),
  d_esriGridKillHack(false),
  d_printShellExpansionOnly(false),
  d_testScriptRunableOnly(false),
  d_printProfileInfo(false)
{
  globalInit();
}

//! dtor
calc::Calc::~Calc()
{
  AppEnd();
}

//! install scriptFileName as the file containing the script
void calc::Calc::setScriptFile(
    const com::PathName& scriptName)
{
    d_lexInput.installFileScript(scriptName.toString());
    d_script.setScriptFileName(scriptName);
}

void calc::Calc::setRunDirectory(const com::PathName&  rd)
{
  com::PathName externalBindingFile;
  script().setRunDirectory(rd,externalBindingFile);
}


//! process arguments
/*!
 * Process arguments in comand line style and perfrom relevant
 * actions associated with the arguments
 *  \return false if no other action is needed, true otherwise
 */
bool calc::Calc::processArgs(
   int   argc,
   char**argv)
{

    /*  WE CAN USE THE APP_ARG PARSER AS LONG AS WE
     *  KEEP THE CONVENTION:
     *  calc options { (-f script.calc) | statement) } [; arguments]
     *  all options must be in front, if calc uses a script then there
     *   are no arguments, if there are arguments but also a script then
     *    there is a problem
     */

    appAllOptionsMostLeft = true;
    if (InstallArgs(argc, (char **)argv, "cd*1m0eEr*s#tTf*F*X*K*b*p",
        "pcrcalc", __DATE__))
             throwLibError();

    typedef enum SCRIPT_TYPE {
         SCRIPT_CMD_LINE, SCRIPT_SCRIPT_FILE, SCRIPT_SHELL_FILE
    } SCRIPT_TYPE;
    SCRIPT_TYPE scriptType = SCRIPT_CMD_LINE;


    const char *scriptName;
    int  shellArgsStart = 1; /* pos in argv where shellArgs start */
    // both empty if not set
    com::PathName runDirectory,externalBindingFile;

    int c;
    while ( (c = GetOpt()) != 0 )
     switch(c) {
      case 'c' : File::d_testCaseTypeOnExistingName=true;
           break;
      case 'T' : d_testScriptRunableOnly = true;
           break;
      case 'm' :
            script().setMVCompression(true);
            break;
      case '0' :
            // -0 implies -m due to way MaskChecker()
            // is implemented
            script().setMVCompression(true);
            script().set0Compression(true);
            break;
      case '1' : script().setWriteEachTimeStep(true);
            break;
      case 'e' : script().setExitValueType(calc::Script::LAST_VAL);
            break;
      case 'E' : script().setExitValueType(calc::Script::EXIT_ON_0);
            break;
      case 'r' : runDirectory=(const char *)OptArg;
            break;
      case 'b' : externalBindingFile=(const char *)OptArg;
            break;
      case 's' : {
            int seed = *(const int *)OptArg;
            if (seed <= 0) // args/test26
              throw com::Exception("-s seed must be > 0 (not "+quote(seed)+")");
            SetRan((unsigned int)seed);
           } break;
      case 't' : d_printShellExpansionOnly = true;
            break;
      case 'd' : script().setDebugMap((const char *)OptArg);
            break;
      // case 'H' : script().setHtmlFile((const char *)OptArg);
      //      break;
      case 'X' : script().setXmlFile((const char *)OptArg);
            break;
      case 'f' :
            setScriptFile((const char *)OptArg);
            scriptType = SCRIPT_SCRIPT_FILE;
            shellArgsStart = 1;
            break;
      case 'F' : /* this is in #! mode
                  * after -F the global options are given which
                  * are passed by pcrcalc itself in InstallScript
                  * OptArg now contains everything after -F
                  * they are discarded (parsed by InstallScript)
                  * the shell wil add the name of the executable script
                  * as the next argument
                  * other arguments at the command line are put after that
                  * argument
                  */
             scriptType = SCRIPT_SHELL_FILE;
             shellArgsStart = 2;
             break;
      case 'K'  : // hack to kill EsriGrids
             scriptName = (const char *)OptArg;
             d_esriGridKillHack = true;
             break;
      case 'p'    : d_printProfileInfo =true;
                    break;
    }

    if (d_esriGridKillHack) {
      script().removeOutputObject(std::string(scriptName));
      return false;
    }

    script().setRunDirectory(runDirectory,externalBindingFile);

    argv = ArgArguments(&argc);
    switch(scriptType) {
     case SCRIPT_SHELL_FILE:
              if (argc == 1) /* no arguments specified after -F in #! line */
               throw com::Exception(
                 "-F found in '#!' line but no succeeding options\n"
                 "use -f instead in '#!' line");  // args/test23
              scriptName = argv[1]; /* this is the script */
              d_lexInput.installFileScript(scriptName);
              /* FALLTHROUGH */
      case SCRIPT_SCRIPT_FILE:
        d_lexInput.installShellArgs(argc-shellArgsStart,
                                  (const char **)argv+shellArgsStart);
              break;
     case SCRIPT_CMD_LINE:
      if ( argc <= 1)
      throw com::Exception("No expression or script specified"); // args/test27
      else
       d_lexInput.installArgvScript(argc-1, (const char **)(argv+1));
    }
    return true;
}

//! parse the input script, building symbol table and code tree
/*!
 *  \todo
 *    redo htmlPrint
 *  \todo
 *    can do better on syntax error, by remembering the last correct
 *    create (createPosition) in lexer, the msg: "Past this point"
 */
void calc::Calc::parse()
{
  try {
   ParserInput pi(d_lexInput);
   ModelParser mp(pi,script());
   script().htmlPrint();
  } catch(const Element::SyntaxErrorBug& seb) {
    // pcrcalc/test345(ab)
    throw com::Exception("Syntax Error");
  }
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
   script().run();
   d_executeScriptStatus = FinishedExecScript;
  }
  catch (const QuitForExitOption& e) {
   // thrown by
   // calc::Script::processFileOutputValue
   d_executeScriptStatus = FileOutputValueExecScript;
   }
  catch (const QuitForProgressCallBack& e){
     // Script::updateProgress is already updated
    d_executeScriptStatus = CanceledExecScript;
   }
 return  script().exitVal();
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
 // dumpZero();
 return status;
}

void calc::Calc::setProgressCallBack(ProgressCallBack *pcb)
{
  script().setProgressCallBack(pcb);
}

bool calc::Calc::printProfileInfo() const
{
  return d_printProfileInfo;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

//! run script \a scriptName with global setting --nothing
/*! currently only used in test modules
   \returns as Calc::run() does
   \throws exceptions that are normally catched in Calc::run()
 */
int calc::runScriptFile(const char *scriptName)
{
  appOutput=APP_NOOUT;
  Script   s;
  com::PathName sn(scriptName);
  ParserInput pi(sn);

  ModelParser mp(pi,s);

  s.run();
  return s.exitVal();
}
//! run script \a scriptName with global setting --nothing
/*! currently only used in test modules
   \returns as Calc::run() does
   \throws exceptions that are normally catched in Calc::run()
 */
int calc::runScriptString(const char *script)
{
  appOutput=APP_NOOUT;
  Script   s;
  std::string si(script);
  ParserInput pi(si);

  ModelParser mp(pi,s);

  s.run();
  return s.exitVal();

  return 0;
}


