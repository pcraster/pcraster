#ifndef INCLUDED_COM_FUNCTIONSTEST
#define INCLUDED_COM_FUNCTIONSTEST



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
  // Functions declarations.
}



namespace com {



//! This class implements the unit tests for the Functions class.
class FunctionsTest
{

public:

                   FunctionsTest       ();

  void             setUp               ();

  void             tearDown            ();

  void             testMinimum         ();

  void             testMaximum         ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
