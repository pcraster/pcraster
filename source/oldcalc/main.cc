#include "stddefx.h"

#ifndef INCLUDED_CALC_CMDLINECALC
#include "calc_cmdlinecalc.h"
#define INCLUDED_CALC_CMDLINECALC
#endif

//#ifndef INCLUDED_COM_CPUCYCLECOUNTER
//#include "com_cpucyclecounter.h"
//#define INCLUDED_COM_CPUCYCLECOUNTER
//#endif

#include <stdlib.h>

#include <iostream>


int main(
  int   argc,
  char**argv)
{
  int r = EXIT_FAILURE;
  try {
  // startCpuCycleCounter(0);
  r = executeCommandLine(argc,argv);
  // stopCpuCycleCounter(0);
  // writeCpuCycleCounterStats();
  } catch(...) {
    std::cerr << "ERROR: programming error uncaught exception (in main())\n";
  }
  return r;
}
