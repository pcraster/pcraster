#ifndef INCLUDED_COM_COMMANDLINEARGUMENTSTEST
#define INCLUDED_COM_COMMANDLINEARGUMENTSTEST



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
  // CommandLineArguments declarations.
}



namespace com {



//! This class implements the unit tests for the CommandLineArguments class.
class CommandLineArgumentsTest
{

public:

                   CommandLineArgumentsTest();

  void             setUp               ();

  void             tearDown            ();

  void             testOptions         ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
