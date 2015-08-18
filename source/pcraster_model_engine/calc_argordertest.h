#ifndef INCLUDED_CALC_ARGORDERTEST
#define INCLUDED_CALC_ARGORDERTEST



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
  // ArgOrderTest declarations.
}



namespace calc {

//! This class implements the unit tests for the ArgOrderAL class.
class ArgOrderTest
{

private:

public:

                   ArgOrderTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testArgOrderAL      ();
  void             testArgOrder        ();
  void             testAddArea         ();

  static boost::unit_test::test_suite* suite();

};

} // namespace calc

#endif
