#ifndef INCLUDED_CALC_TRANSIENTTEST
#define INCLUDED_CALC_TRANSIENTTEST

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace calc {
  // Transient declarations.
}

namespace boost {
  namespace unit_test {
    class test_suite;
  }
}


namespace calc {



//! This class implements the unit tests for the Transient class.
class TransientTest
{

public:

                   TransientTest      ();

  void             testFixedHead       ();

  void             testBudget          ();

  static boost::unit_test::test_suite *    suite               ();

};

} // namespace calc

#endif
