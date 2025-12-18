#include "stddefx.h"
#include "com_strconv.h"  // toString
#include "calc_cmdlinecalc.h"
#include "calc_spatial.h"
#include "appargs.h"
#include "calc_calc.h"
#include "calc_executor.h"
#include "calc_astscript.h"
#include "calc_progresscallback.h"
#include "calc_lexinput.h"
#include "calc_completeparser.h"
#include "calc_options.h"

#include <iostream>  // cerr
#include <memory>

/***************/
/* EXTERNALS   */
/***************/

/**********************/
/* LOCAL DECLARATIONS */
/**********************/

namespace calc
{

//! pcrcalc command line application interface
/*!
 * only used in calc::commandLineCalc() to
 * support calcapp/main.cc
 */
class CmdLineCalc : public Calc
{
private:
  //! Assignment operator. NOT IMPLEMENTED.
  CmdLineCalc &operator=(const CmdLineCalc &);

  //! Copy constructor. NOT IMPLEMENTED.
  CmdLineCalc(const CmdLineCalc &);

  Options d_options;

  int d_argc;
  char **d_argv;

public:
  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  CmdLineCalc(int argc, char **argv);

  /* virtual */ ~CmdLineCalc() override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  int execute() override;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
};

//! command line mode progress call back
/*
 * \bug   test349 win32/release segfaults very strange here
 */
class CmdLineProgressCallBack : public ProgressCallBack
{
public:
  int update(const ProgressInfo &pi) override
  {
    // no progress when --nothing or static model
    if (appOutput == APP_NOOUT || (pi.nrTimeSteps == 0)) {
      return 0;
    }

    if (pi.inTimeStep == 1) {
      std::cerr << "\n";  // first clear line for progress
    }

    if (pi.inTimeStep > pi.nrTimeSteps) {
      // finished
      std::cerr << "\n";  // after skip line used in timestep printing
      return 0;
    }

    if (pi.inTimeStep > 0) {
      // overwrite this one each time
      // seems to do test349 better from the commandline :-(
      std::string const s(com::toString(pi.inTimeStep));
      std::cerr << "Executing timestep " << s << "\r";
      std::cerr.flush();
    }
    return 0;
  }

  ProgressPulse callAtPulse() const override
  {
    return LoopPulse;
  }
};

static CmdLineProgressCallBack cmdLineProgressCallBack;

}  // namespace calc

/*********************/
/* LOCAL DEFINITIONS */
/*********************/

/******************/
/* IMPLEMENTATION */
/******************/

//! ctor
calc::CmdLineCalc::CmdLineCalc(int argc, char **argv)
    : Calc(std::cout, std::cerr), d_argc(argc), d_argv(argv)
{
}

//! dtor
calc::CmdLineCalc::~CmdLineCalc()
{
}

int calc::CmdLineCalc::execute()
{
  setProgressCallBack(&cmdLineProgressCallBack);

  if (d_argc == 1) {
    d_options.printUsage();
    exit(0);
  }
  d_options.processCmdLineOptions(d_argc, d_argv);

  if (d_options.printShellExpansionOnly()) {
    std::unique_ptr<LexInput> l(d_options.createLexInput());
    l->printExpandedCode(d_stdOut);
    return 0;
  }

  CompleteParser<ASTScript, LexInputCreator> parser(d_options);

  std::unique_ptr<ASTScript> script(parser.parseScript());
  script->setRteSettings(d_options);

  script->analyzeAndResolve();

  Executor executor(script->cfgCode(), script->rteSettings(), script->symbols());

  if (d_options.testScriptRunableOnly()) {
    return 0;
  }

  executor.setProgressCallBack(&cmdLineProgressCallBack);
  executor.execAll();

  if (d_options.profile()) {
    // NOT maxBPC depends on static method of Spatial
    //  and BPC is not reset
    std::cout << '\n';
    std::cout << "maximum bpc: " << Spatial::maxBPC() << '\n';
  }

  // return  script().exitVal();
  return 0;
}

extern "C" PCR_ME_EXPORT int calc::executeCommandLine(int argc, char **argv)
{
  CmdLineCalc c(argc, argv);
  return c.run();
}
