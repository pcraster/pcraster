#ifndef INCLUDED_CALC_FIELDTEST
#define INCLUDED_CALC_FIELDTEST



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
  // Field declarations.
}



namespace calc {



//! This class implements the unit tests for the Field class.
class FieldTest
{

public:

                   FieldTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testCtorNonSpatial  ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
