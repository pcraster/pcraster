#ifndef INCLUDED_CALC_NONSPATIALTEST
#define INCLUDED_CALC_NONSPATIALTEST



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
  // NonSpatial declarations.
}



namespace calc {



//! This class implements the unit tests for the NonSpatial class.
class NonSpatialTest
{

public:

                   NonSpatialTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testSetAndGetCell   ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
