#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_OPTIONS
#include "calc_options.h"
#define INCLUDED_CALC_OPTIONS
#endif

// Library headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_COM_APPARGS
#include "com_appargs.h"
#define INCLUDED_COM_APPARGS
#endif
#ifndef INCLUDED_APPARGS
#include "appargs.h"
#define INCLUDED_APPARGS
#endif
// Module headers.
#ifndef INCLUDED_CALC_LEXINPUT
#include "calc_lexinput.h"
#define INCLUDED_CALC_LEXINPUT
#endif
#ifndef INCLUDED_CALC_GLOBALLIBDEFS
#include "calc_globallibdefs.h"
#define INCLUDED_CALC_GLOBALLIBDEFS
#endif



/*!
  \file
  This file contains the implementation of the Options class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class OptionsPrivate
{
public:

  OptionsPrivate()
  {
  }

  ~OptionsPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC OPTIONS MEMBERS
//------------------------------------------------------------------------------

#define VERSION __DATE__
void calc::Options::printUsage()
{

#ifdef DEBUG_DEVELOP
 if (appOutput != APP_NOOUT)
    fprintf(stderr,"PCRTEAM VERSION, INTERNAL USE ONLY! (%s)\n", PLATFORM_TXT);
#endif

  fprintf(stderr, "pcrcalc %s (%s)\n", VERSION, PLATFORM_TXT);
  fprintf(stderr,
   " USAGE: pcrcalc [options] \"expression\"\n"
   " or     pcrcalc [options] -f scriptFile\n"
   "  ( or #!: pcrcalc -F [options]+)\n"
   " other flags:\n"
   "  s #  : set seed (integer > 0) for random generator\n"
   "         default is based on current time\n"
   "  b f  : overrule script bindings\n"
   "  1    : update timeseries files at end of each timestep\n"
   "  r f  : set run directory\n"
   "  d f  : debug mode, check MV creation on assignment\n"
   "          comparing against clone or areamap boolean mask\n"
   "  c    : strict Case significant filename check (Unix portability)\n"
   "  p    : print profile information\n"
   "  m    : optimize with areamap MV compression\n"
   "  l    : use less memory but more temporary disk storage\n"
   "  t    : test argument substitution\n"
// NON WORKING FEATURES
// "  H f  : generate HTML file for model inspection\n"
// "         (Experimental and incomplete)\n"
// "  e    : return as exit code the last fileoutput expression\n"
// "  E    : quit if a fileoutput expression evaluates to 0 \n"
// "          exit code is timestep when quitted\n"
//X"  C    : optimize by using C compiler\n"
   );
}



//------------------------------------------------------------------------------
// DEFINITION OF OPTIONS MEMBERS
//------------------------------------------------------------------------------

calc::Options::Options():
  d_argc(0),
  d_argv(0),
  d_scriptType(SCRIPT_CMD_LINE)
{
  AppSetGlobalArgsDefaults();
}



calc::Options::~Options()
{
  EndGetOpt();
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::Options& calc::Options::operator=(const Options& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
calc::Options::Options(const Options& rhs):
  Base(rhs)
{
}
*/


//! process arguments
/*!
 * Process arguments in command line style and
 * remember remaining args for a later createLexInput
 * call.
 *
 */
void calc::Options::processArgs(
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
    if (InstallArgs(argc, (char **)argv, "Ccd*1mleEr*s#tTf*F*X*K*b*p",
        "pcrcalc", __DATE__))
             throwLibError();

    int c;
    while ( (c = GetOpt()) != 0 )
     switch(c) {
      // RunTimeEnvSettings:
      case 'm' : setMVCompression(true);                      break;
      case 'l' : setUseDiskStorage(true);                     break;
      case 'p' : setProfile(true);                            break;
      case 'C' : setCompile(true);                            break;
      case 'd' : setDebugMVAssignments((const char *)OptArg); break;
      case '1' : setWriteEachTimeStep(true);                  break;
      case 'e' : setExitValueType(calc::RunTimeEnvSettings::LAST_VAL);
                 break;
      case 'E' : setExitValueType(calc::RunTimeEnvSettings::EXIT_ON_0);
                 break;
      case 'r' : setRunDirectory((const char *)OptArg); break;
      case 's' : {
                  int seed = *(const int *)OptArg;
                  if (seed <= 0) { // args/test26
                    std::ostringstream s;
                    s << "-s seed must be > 0 (not '" << seed << "')";
                     throw com::Exception(s.str());
                  }
                  setSeed((unsigned int)seed);
                 } break;

    // Function modifiers
      case 'c' : setTestCaseTypeOnExistingName(true);
                 break;
      case 'T' : setTestScriptRunableOnly(true);
                 break;
      case 't' : setPrintShellExpansionOnly(true);
                 break;
      /*
      case 'H' : script().setHtmlFile((const char *)OptArg);
                 break;
      case 'X' : script().setXMLFile((const char *)OptArg);
            break;
      */
      // hack to kill EsriGrids
      case 'K' : setScriptFile((const char *)OptArg);
                 d_scriptType=SCRIPT_ESRI_GRID_KILL;
                break;
      // Parse options
      case 'b' : setExternalBindingFile((const char *)OptArg);
                 break;
      case 'f' : setScriptFile((const char *)OptArg);
                 d_scriptType=SCRIPT_SCRIPT_FILE;
                 break;
      case 'F' : // this is in #! mode
                 // after -F the global options are given which
                 // are passed by pcrcalc itself in InstallScript
                 // OptArg now contains everything after -F
                 // they are discarded (parsed by InstallScript)
                 // the shell wil add the name of the executable script
                 // as the next argument
                 // other arguments at the command line are put after that
                 // argument

                 // can not yet determine scriptFile
                 d_scriptType=SCRIPT_SHELL_FILE;
                 break;

    }
}

void calc::Options::processCmdLineOptions(int   argc, char**argv)
{
  processArgs(argc,argv);
  d_argv = ArgArguments(&d_argc);

  if (d_scriptType == SCRIPT_SHELL_FILE) {
   // determine scriptFile
   if (d_argc == 1) // no arguments specified after -F in #! line
    throw com::Exception(
      "-F found in '#!' line but no succeeding options\n"
      "use -f instead in '#!' line");  // args/test23
   setScriptFile(d_argv[1]); // this is the script
  }
}

void calc::Options::processOptionString(const std::string& argsExclArg0)
{
  com::AppArgs args("PCRasterModelEngine",argsExclArg0);
  processArgs(args.argc(), args.argv());
}

//! \pre processArgs called
calc::LexInput* calc::Options::createLexInput() const {

  std::auto_ptr<LexInput> l(new LexInput());

  int  shellArgsStart=1; // pos in argv where shellArgs start
  switch(d_scriptType) {
   case SCRIPT_SHELL_FILE:
     shellArgsStart = 2;
     // FALLTHROUGH
   case SCRIPT_SCRIPT_FILE:
     l->installFileScript(scriptFile());
     l->installShellArgs(d_argc-shellArgsStart,
                                (const char **)d_argv+shellArgsStart);
     break;
   case SCRIPT_CMD_LINE:
     if (d_argc <= 1)
      throw com::Exception("No expression or script specified"); // args/test27
     else
       l->installArgvScript(d_argc-1, (const char **)(d_argv+1));
     break;
   case SCRIPT_ESRI_GRID_KILL:
      throw com::Exception("-K disabled");
  }
  return l.release();
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



