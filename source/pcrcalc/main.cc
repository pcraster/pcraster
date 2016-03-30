#include "stddefx.h" 

#ifndef INCLUDED_CALC_CMDLINECALC
#include "calc_cmdlinecalc.h"
#define INCLUDED_CALC_CMDLINECALC
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif
#ifndef INCLUDED_MEMORY
#include <memory>
#define INCLUDED_MEMORY
#endif

#ifndef INCLUDED_CALC_LIBRARYCLASS
#include "calc_LibraryClass.h"
#define INCLUDED_CALC_LIBRARYCLASS
#endif

//! deriving from com::App not yet possible
//  due to command line options mixed with the statements
int main(
  int   argc,
  char**argv)
{
  struct Main 
   // public calc::LibraryClass
   {
   int result;
   Main(int   argc,
        char**argv):
        //LibraryClass(argc,argv),
        result(1)
   {
      struct ClientHolder : public calc::LibraryClass
      {
        ClientHolder(
           int   argc,
           char**argv):
             calc::LibraryClass(argc,argv)
                  {}
      };
      // std::auto_ptr<ClientHolder> holder(new ClientHolder(argc,argv));
      ClientHolder* holder=new ClientHolder(argc,argv);
      (void)holder; // shut up compiler
    try {
      result = calc::executeCommandLine(argc,argv);
    } catch(...) {
      std::cerr << "ERROR: programming error uncaught exception (in main())\n";
    }
   }
  };

  Main m(argc,argv);
  return m.result;
}
