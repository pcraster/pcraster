#ifndef INCLUDED_COM_ANYPOINTERSTEST
#define INCLUDED_COM_ANYPOINTERSTEST



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
  // AnyPointers declarations.
}



namespace com {



//! This class implements the unit tests for the AnyPointers class.
class AnyPointersTest
{

public:

                   AnyPointersTest     ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
