#include "stddefx.h"
#include "calc_cmdlinecalc.h"
//#include "com_cpucyclecounter.h"
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
