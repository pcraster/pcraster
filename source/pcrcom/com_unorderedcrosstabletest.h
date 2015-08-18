#ifndef INCLUDED_COM_UNORDEREDCROSSTABLETEST
#define INCLUDED_COM_UNORDEREDCROSSTABLETEST



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
  // UnOrderedCrossTable declarations.
}



namespace com {



//! This class implements the unit tests for the UnOrderedCrossTable class.
class UnOrderedCrossTableTest
{

public:

                   UnOrderedCrossTableTest();

  void             setUp               ();

  void             tearDown            ();

  void             testSimpleUsage     ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
