#ifndef INCLUDED_CALC_AREAMAPTEST
#define INCLUDED_CALC_AREAMAPTEST



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
  // AreaMap declarations.
}



namespace calc {



//! This class implements the unit tests for the AreaMap class.
class AreaMapTest
{

private:

public:

                   AreaMapTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testInit            ();

  static boost::unit_test::test_suite* suite();

};

} // namespace calc

#endif
