#ifndef INCLUDED_GEO_UTILTEST
#define INCLUDED_GEO_UTILTEST



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

namespace geo {
  // Util declarations.
}



namespace geo {



//! This class implements the unit tests for the Util class.
class UtilTest
{

public:

                   UtilTest            ();

  void             setUp               ();

  void             tearDown            ();

  void             testRaster2Boundaries();

  void             testMagnitude       ();

  static boost::unit_test::test_suite* suite();

};

} // namespace geo

#endif
