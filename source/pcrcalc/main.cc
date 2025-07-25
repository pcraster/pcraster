#include "stddefx.h"
#include "calc_cmdlinecalc.h"
#include "calc_LibraryClass.h"
#include <iostream>
#include <memory>

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
      auto* holder=new ClientHolder(argc,argv);
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
