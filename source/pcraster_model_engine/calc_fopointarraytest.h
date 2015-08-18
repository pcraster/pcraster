#ifndef INCLUDED_CALC_FOPOINTARRAYTEST
#define INCLUDED_CALC_FOPOINTARRAYTEST



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
  // FoPointArray declarations.
}



namespace calc {



//! This class implements the unit tests for the FoPointArray class.
class FoPointArrayTest
{

public:

                   FoPointArrayTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test1               ();

  void             testAggregate       ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
