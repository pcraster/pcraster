#ifndef INCLUDED_CALC_MASKPACKINGTEST
#define INCLUDED_CALC_MASKPACKINGTEST



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
  // MaskPacking declarations.
}



namespace calc {



//! This class implements the unit tests for the MaskPacking class.
class MaskPackingTest
{

public:

                   MaskPackingTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testSpatialPacking      ();

  void             testScript          ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
