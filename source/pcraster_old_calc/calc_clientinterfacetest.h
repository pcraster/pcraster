#ifndef INCLUDED_CALC_CLIENTINTERFACETEST
#define INCLUDED_CALC_CLIENTINTERFACETEST



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

namespace calc {
  // ClientInterface declarations.
}



namespace calc {



//! This class implements the unit tests for the ClientInterface class.
class ClientInterfaceTest
{

public:

                   ClientInterfaceTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testCapi            ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
