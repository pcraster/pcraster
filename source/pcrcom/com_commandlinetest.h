#ifndef INCLUDED_COM_COMMANDLINETEST
#define INCLUDED_COM_COMMANDLINETEST



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace com {
  // CommandLine declarations.
  class CommandLine;
}



namespace com {



//! This class implements the unit tests for the CommandLine class.
class CommandLineTest
{

private:

  CommandLine*     d_commandLine;

public:

                   CommandLineTest     ();

  void             setUp               ();

  void             tearDown            ();

  void             testConstructor     ();

  void             testPositionalValue ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
