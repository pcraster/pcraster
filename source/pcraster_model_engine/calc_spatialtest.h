#ifndef INCLUDED_CALC_SPATIALTEST
#define INCLUDED_CALC_SPATIALTEST



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
  // Spatial declarations.
}



namespace calc {



//! This class implements the unit tests for the Spatial class.
class SpatialTest
{

public:

                   SpatialTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testSetAndGetCell   ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
