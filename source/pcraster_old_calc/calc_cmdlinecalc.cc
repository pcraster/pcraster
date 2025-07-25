#include "stddefx.h"
#include "com_strconv.h"  // toString
#include "calc_cmdlinecalc.h"
#include "appargs.h"
#include "calc_calc.h"
#include "calc_spatial.h"
#include "calc_progresscallback.h"

#include "pcraster_version.h"

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

   //! args argc
   int   d_argc;
   //! args argv
   char  **d_argv;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   CmdLineCalc         (int   argc,
                                        char**argv);

  /* virtual */    ~CmdLineCalc           () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  int              execute             () override;

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
    int update(const ProgressInfo& pi) override
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
          // std::cerr << "Executing timestep " << pi.inTimeStep << "\r" ;
          // seems to do test349 better from the commandline :-(
           std::string s(com::toString(pi.inTimeStep));
           std::cerr << "Executing timestep " << s << "\r" ;
           std::cerr.flush();
        }
        return 0;
     }
     ProgressPulse callAtPulse() const override {
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

static void printUsage()
{

#ifdef DEBUG_DEVELOP
 if (appOutput != APP_NOOUT)
    fprintf(stderr,"PCRTEAM VERSION, INTERNAL USE ONLY! (%s)\n", PLATFORM_TXT);
#endif
  // this is the old edition supporting arrayed variables and so on
  fprintf(stderr,"pcrcalc (oldcalc/2003 edition) %s (%s)\n", PCRASTER_VERSION,
    PLATFORM_TXT);
  fprintf(stderr,
   " USAGE: pcrcalc [options] \"expression\"\n"
   " or     pcrcalc [options] -f scriptFile\n"
   "  ( or #!: pcrcalc -F [options]+)\n"
   " other flags:\n"
   "  T    : only test model for syntax errors and input data available\n"
   "  d f  : debug mode, check MV creation on assignment\n"
   "          comparing against clone or areamap boolean mask\n"
   "  m    : optimize with MV compression\n"
   "  0    : optimize with 0 map detection, implies -m\n"
   "  1    : update timeseries files at end of each timestep\n"
   "  e    : return as exit code the last fileoutput expression\n"
   "  E    : quit if a fileoutput expression evaluates to 0 \n"
   "          exit code is timestep when quitted\n"
   "  s #  : set seed (integer > 0) for random generator\n"
   "         default is based on current time\n"
   "  c    : strict Case significant filename check (Unix portability)\n"
   "  t    : print substituted model to stdout\n"
   "  r f  : set run directory\n"
   "  b f  : overrule script bindings\n"
   "  p    : print profile information\n");
// "  H f  : generate HTML file for model inspection\n"
// "         (Experimental and incomplete)\n"
}


int calc::CmdLineCalc::execute()
{

    setProgressCallBack(&cmdLineProgressCallBack);
    script().setFileOutputStream(&std::cout);

    if (d_argc == 1) {
      printUsage();
      exit(0);
    }

    if (!processArgs(d_argc,d_argv))
      return 0;

    // appIOstrategy = APP_IO_BANDMAP;
    // PRINT_VAR(appIOstrategy);

    if (d_printShellExpansionOnly) {
      d_lexInput.printExpandedCode(d_stdOut);
      return 0;
    }
    parse();
    if (!d_testScriptRunableOnly)
      executeScript();
   return  script().exitVal();
}

extern "C" PCR_DLL_FUNC(int)  executeCommandLine(int argc, char**argv)
{
  int r = 0;
  {
    calc::CmdLineCalc c(argc,argv);
    r=c.run();
    if (c.printProfileInfo()) {
      std::cout << '\n';
      std::cout << "maxBPC: "  <<  calc::Spatial::maxBPC()  << '\n';
    }
  }
  return r;
}
