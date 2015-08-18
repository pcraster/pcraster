#ifndef INCLUDED_COM_COMMANDLINEARGUMENTTEST
#define INCLUDED_COM_COMMANDLINEARGUMENTTEST



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
  // CommandLineArgument declarations.
}



namespace com {



//! This class implements the unit tests for the CommandLineArgument class.
class CommandLineArgumentTest
{

public:

                   CommandLineArgumentTest();

  void             setUp               ();

  void             tearDown            ();

  void             testPositionalValue ();

  void             testPositionalList  ();

  void             testOption          ();

  void             testOptionValue     ();

  void             testOptionList      ();

  void             testArgumentSort    ();

  void             testRepeatableArgument();

  void             testExclusiveArgument();

  void             testRepeatableExclusiveArgument();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
