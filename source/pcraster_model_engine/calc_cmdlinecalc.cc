#include "stddefx.h"

/********/
/* USES */
/********/
#ifndef INCLUDED_MEMORY
#include <memory>
#define INCLUDED_MEMORY
#endif

#ifndef INCLUDED_COM_STRCONV
#include "com_strconv.h"  // toString
#define INCLUDED_COM_STRCONV
#endif

#ifndef INCLUDED_CALC_CMDLINECALC
#include "calc_cmdlinecalc.h"
#define INCLUDED_CALC_CMDLINECALC
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream> // cerr
#define INCLUDED_IOSTREAM
#endif
#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
#endif

/* libs ext. <>, our ""  */
#ifndef INCLUDED_APPARGS
#include "appargs.h"
#define INCLUDED_APPARGS
#endif

#ifndef INCLUDED_CALC_CALC
#include "calc_calc.h"
#define INCLUDED_CALC_CALC
#endif



/* global header (opt.) and prototypes "" */

/* headers of this app. modules called */
#ifndef INCLUDED_CALC_EXECUTOR
#include "calc_executor.h"
#define INCLUDED_CALC_EXECUTOR
#endif
#ifndef INCLUDED_CALC_ASTSCRIPT
#include "calc_astscript.h"
#define INCLUDED_CALC_ASTSCRIPT
#endif
#ifndef INCLUDED_CALC_PROGRESSCALLBACK
#include "calc_progresscallback.h"
#define INCLUDED_CALC_PROGRESSCALLBACK
#endif
#ifndef INCLUDED_CALC_LEXINPUT
#include "calc_lexinput.h"
#define INCLUDED_CALC_LEXINPUT
#endif
#ifndef INCLUDED_CALC_COMPLETEPARSER
#include "calc_completeparser.h"
#define INCLUDED_CALC_COMPLETEPARSER
#endif
#ifndef INCLUDED_CALC_OPTIONS
#include "calc_options.h"
#define INCLUDED_CALC_OPTIONS
#endif

/***************/
/* EXTERNALS   */
/***************/

/**********************/
/* LOCAL DECLARATIONS */
/**********************/

namespace calc {

//! pcrcalc command line application interface
/*!
 * only used in calc::commandLineCalc() to
 * support calcapp/main.cc
 */
class CmdLineCalc : public Calc
{
private:

  //! Assignment operator. NOT IMPLEMENTED.
  CmdLineCalc&        operator=           (const CmdLineCalc&);

  //! Copy constructor. NOT IMPLEMENTED.
                   CmdLineCalc            (const CmdLineCalc&);

  Options d_options;

  int     d_argc;
  char**  d_argv;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   CmdLineCalc         (int   argc,
                                        char**argv);

  /* virtual */    ~CmdLineCalc        ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  int              execute             ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

};


//! command line mode progress call back
/*
 * \bug   test349 win32/release segfaults very strange here
 */
class  CmdLineProgressCallBack : public ProgressCallBack {
public:
    int update(const ProgressInfo& pi)
     {
        // no progress when --nothing or static model
        if  (appOutput == APP_NOOUT || !pi.nrTimeSteps)
          return 0;

        if (pi.inTimeStep == 1)
          std::cerr << "\n"; // first clear line for progress

        if (pi.inTimeStep > pi.nrTimeSteps) {
          // finished
          std::cerr << "\n"; // after skip line used in timestep printing
          return 0;
        }

        if (pi.inTimeStep > 0) {
          // overwrite this one each time
          // seems to do test349 better from the commandline :-(
           std::string s(com::toString(pi.inTimeStep));
           std::cerr << "Executing timestep " << s << "\r" ; 
           std::cerr.flush();
        }
        return 0;
     }
     ProgressPulse callAtPulse() const {
       return LoopPulse;
     }
};

static CmdLineProgressCallBack cmdLineProgressCallBack;

}


/*********************/
/* LOCAL DEFINITIONS */
/*********************/

/******************/
/* IMPLEMENTATION */
/******************/

//! ctor
calc::CmdLineCalc::CmdLineCalc(
   int   argc,
   char**argv):
  Calc(std::cout,std::cerr),
  d_argc(argc),
  d_argv(argv)
{
}

//! dtor
calc::CmdLineCalc::~CmdLineCalc() {
}


int calc::CmdLineCalc::execute()
{
  setProgressCallBack(&cmdLineProgressCallBack);

  if (d_argc == 1) {
      d_options.printUsage();
      return 1;
  }
  d_options.processCmdLineOptions(d_argc,d_argv);

  if (d_options.printShellExpansionOnly()) {
    std::auto_ptr<LexInput> l(d_options.createLexInput());
    l->printExpandedCode(d_stdOut);
    return 0;
  }

  CompleteParser<ASTScript,LexInputCreator> parser(d_options);

  std::auto_ptr<ASTScript> script(parser.parseScript());
  script->setRteSettings(d_options);

  script->analyzeAndResolve();

  Executor executor(
      script->cfgCode(),
      script->rteSettings(),
      script->symbols());

  if (d_options.testScriptRunableOnly())
    return 0;

  executor.setProgressCallBack(&cmdLineProgressCallBack);
  executor.execAll();

  if (d_options.profile()) {
    // NOT maxBPC depends on static method of Spatial
    //  and BPC is not reset
    std::cout << std::endl;
    std::cout << "maximum bpc: " << Spatial::maxBPC() << std::endl;
  }

  // return  script().exitVal();
  return 0;
}

extern "C" PCR_DLL_FUNC(int) calc::executeCommandLine(int argc, char**argv)
{
  CmdLineCalc c(argc,argv);
  return c.run();
}
