#include "calc_cmdlinecalc.h"

#include <cstdlib>
#include <iostream>


int main(
  int   argc,
  char**argv)
{
  int r = EXIT_FAILURE;
  try {
    r = executeCommandLine(argc,argv);
  } catch(...) {
    std::cerr << "ERROR: programming error uncaught exception (in main())\n";
  }
  return r;
}
