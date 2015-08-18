#ifndef INCLUDED_FIELDAPI_INTERFACETEST
#define INCLUDED_FIELDAPI_INTERFACETEST



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

namespace fieldapi {
  // Interface declarations.
}



namespace fieldapi {



//! This class implements the unit tests for the Interface class.
class InterfaceTest
{

public:

                   InterfaceTest       ();

  void             testNonMV           ();

  static boost::unit_test::test_suite* suite();

};

} // namespace fieldapi

#endif
